#!/usr/bin/env python3
"""Check developer IDs, scan Git blobs as data, and publish PR admission."""

import base64
import json
import os
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
import tempfile
import time
import urllib.request

ADMISSION = "CFAST / PR admission"
SCAN = "CFAST / ClamAV scan"
POLICY = Path(__file__).with_name("trusted-developers.json")
MAX_FILE = 512 * 1024**2
MAX_TOTAL = 4 * 1024**3
MAX_FILES = 100000


def api(path, data=None):
    request = urllib.request.Request("https://api.github.com" + path,
        data=None if data is None else json.dumps(data).encode(),
        headers={"Authorization": "Bearer " + os.environ["GH_TOKEN"],
                 "Accept": "application/vnd.github+json", "User-Agent": "cfast-pr-security"})
    with urllib.request.urlopen(request, timeout=60) as response:
        return json.load(response)


def status(context, state):
    repository = os.environ["GITHUB_REPOSITORY"]
    api(f"/repos/{repository}/statuses/{os.environ['HEAD_SHA']}", {
        "context": context, "state": state,
        "target_url": f"https://github.com/{repository}/actions/runs/{os.environ['GITHUB_RUN_ID']}",
    })


def trusted(author, sender):
    entries = json.loads(POLICY.read_text())["developers"]
    ids = {entry["id"] for entry in entries if type(entry["id"]) is int and entry["id"] > 0}
    return all(user.get("type") == "User" and type(user.get("id")) is int
               and user["id"] in ids for user in (author, sender))


def prepare():
    event = json.loads(Path(os.environ["GITHUB_EVENT_PATH"]).read_text())
    repository, head = os.environ["GITHUB_REPOSITORY"], os.environ["HEAD_SHA"]
    path = f"/repos/{repository}/pulls/{int(os.environ['PR_NUMBER'])}"
    pull = api(path)
    if pull["head"]["sha"] != head or (pull["state"] != "open" and not pull.get("merged")):
        raise RuntimeError("Stale PR event")
    member = trusted(pull["user"], event["sender"])
    base, merge = pull["base"]["sha"], ""
    if pull["state"] == "open":
        status(ADMISSION, "pending")
    status(SCAN, "pending")
    for attempt in range(10):
        candidate = pull.get("merge_commit_sha")
        if candidate:
            commit = api(f"/repos/{repository}/git/commits/{candidate}")
            if pull.get("merged") or [p["sha"] for p in commit["parents"]] == [base, head]:
                merge = candidate
                break
        if pull.get("mergeable") is False:
            break
        time.sleep(3)
        pull = api(path)
        if pull["head"]["sha"] != head or (not pull.get("merged") and
                (pull["state"] != "open" or pull["base"]["sha"] != base)):
            raise RuntimeError("PR changed while preparing scan")
    with open(os.environ["GITHUB_OUTPUT"], "a") as output:
        output.write(f"trusted={str(member).lower()}\nbase_sha={base}\nmerge_sha={merge}\n")
    if pull["state"] == "open" and member and merge:
        status(ADMISSION, "success")
    print(f"Trusted developer: {member}; head={head}; merge={merge or 'unavailable'}")


def materialize(repository, sha, destination):
    """Fetch objects without checkout/hooks/filters; write regular blobs as data."""
    if not re.fullmatch(r"[A-Za-z0-9-]+/[A-Za-z0-9_.-]+", repository) or not re.fullmatch(r"[0-9a-f]{40}", sha):
        raise ValueError("Invalid repository or commit")
    with tempfile.TemporaryDirectory() as objects:
        git = ["git", "-C", objects, "-c", "core.hooksPath=/dev/null", "-c", "transfer.fsckObjects=true"]
        subprocess.run(git + ["init", "--bare", "--quiet"], check=True)
        subprocess.run(git + ["fetch", "--quiet", "--depth=1", "--no-tags",
                             f"https://github.com/{repository}.git", sha], check=True, timeout=300)
        tree = subprocess.check_output(git + ["ls-tree", "-rlz", sha])
        total = count = 0
        for entry in filter(None, tree.split(b"\0")):
            metadata, name = entry.split(b"\t", 1)
            mode, kind, blob, size = metadata.split()
            path = PurePosixPath(os.fsdecode(name))
            if mode not in (b"100644", b"100755") or kind != b"blob" or path.is_absolute() or ".." in path.parts:
                raise ValueError("Links, submodules, or unsafe paths cannot be scanned completely")
            size = int(size)
            total += size
            count += 1
            if size > MAX_FILE or total > MAX_TOTAL or count > MAX_FILES:
                raise ValueError("Repository exceeds scan limits")
            target = destination / path
            target.parent.mkdir(parents=True, exist_ok=True)
            with target.open("xb") as output:
                subprocess.run(git + ["cat-file", "blob", blob.decode()], stdout=output, check=True)
            with target.open("rb") as source:
                if source.read(128).startswith(b"version https://git-lfs.github.com/spec/v1"):
                    raise ValueError("Unresolved Git LFS object")
        if not count:
            raise ValueError("Empty source tree")
        print(f"Prepared {count} Git blobs for {sha}")


def clamav(directory):
    result = subprocess.run([
        "clamscan", "--recursive=yes", "--infected", "--alert-exceeds-max=yes",
        "--alert-encrypted=yes", "--max-filesize=512M", "--max-scansize=2048M",
        "--max-recursion=40", "--max-files=100000", "--max-scantime=120000",
        "--follow-dir-symlinks=0", "--follow-file-symlinks=0", str(directory),
    ], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=1200)
    for line in result.stdout.splitlines():
        print("ClamAV | " + line)  # Do not interpret filenames as Actions commands.
    count = re.search(r"^Scanned files:\s+(\d+)\s*$", result.stdout, re.MULTILINE)
    expected = sum(p.stat().st_size > 0 for p in directory.rglob("*") if p.is_file())
    if (result.returncode or re.search(r"\b(ERROR|WARNING)\b", result.stdout)
            or not count or int(count[1]) < max(1, expected)):
        raise RuntimeError("ClamAV failed or did not scan all files")


def scan():
    with tempfile.TemporaryDirectory() as folder:
        work = Path(folder)
        # Generate the harmless EICAR test only on the disposable scan runner.
        probe = work / "probe"
        probe.write_bytes(base64.b64decode(
            "WDVPIVAlQEFQWzRcUFpYNTQoUF4pN0NDKTd9JEVJQ0FSLVNUQU5EQVJELUFOVElWSVJVUy1URVNULUZJTEUhJEgrSCo="))
        result = subprocess.run(["clamscan", "--infected", str(probe)], capture_output=True, text=True, timeout=120)
        probe.unlink()
        if result.returncode != 1 or "eicar" not in result.stdout.lower():
            raise RuntimeError("ClamAV detection self-test failed")
        for index, sha in enumerate((os.environ["HEAD_SHA"], os.environ["MERGE_SHA"])):
            if not sha:
                raise RuntimeError("No merge snapshot available; scan incomplete")
            destination = work / str(index)
            destination.mkdir()
            materialize(os.environ["GITHUB_REPOSITORY"], sha, destination)
            clamav(destination)


def report():
    state = "success" if os.environ["SCAN_RESULT"] == "success" else "failure"
    status(SCAN, state)
    if os.environ["TRUSTED"] != "true":
        pull = api(f"/repos/{os.environ['GITHUB_REPOSITORY']}/pulls/{int(os.environ['PR_NUMBER'])}")
        if (pull["state"] == "open" and pull["head"]["sha"] == os.environ["HEAD_SHA"]
                and pull["base"]["sha"] == os.environ["BASE_SHA"]):
            status(ADMISSION, state)
    if state != "success":
        raise RuntimeError("Scan failed; inspect scan job logs")


if __name__ == "__main__":
    {"prepare": prepare, "scan": scan, "report": report}[sys.argv[1]]()

"""Offline security regressions: no network, ClamAV, or EICAR files."""
import copy
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch
import pr_security as security

HEAD, BASE, MERGE = (letter * 40 for letter in "abc")
ENV = {"GITHUB_REPOSITORY": "developer/cfast", "HEAD_SHA": HEAD, "PR_NUMBER": "7",
       "BASE_SHA": BASE, "TRUSTED": "false", "SCAN_RESULT": "success"}


def pull():
    return {"state": "open", "head": {"sha": HEAD}, "base": {"sha": BASE},
            "user": {"id": 13065776, "type": "User"}, "merge_commit_sha": MERGE}


class GateTests(unittest.TestCase):
    def prepare(self, current=None, sender=None, later=None, parents=None):
        current = copy.deepcopy(current or pull())
        responses = [current, later] if later else [current]
        def api(path):
            if "/git/commits/" in path:
                return {"parents": [{"sha": sha} for sha in (parents or [BASE, HEAD])]}
            return responses.pop(0) if len(responses) > 1 else responses[0]
        with tempfile.TemporaryDirectory() as folder:
            event, output = Path(folder) / "event", Path(folder) / "output"
            event.write_text(json.dumps({"sender": sender or current["user"]}))
            with patch.dict(os.environ, {**ENV, "GITHUB_EVENT_PATH": str(event), "GITHUB_OUTPUT": str(output)}), \
                    patch.object(security, "api", side_effect=api), patch.object(security, "status") as status, \
                    patch.object(security.time, "sleep"):
                security.prepare()
            return status.call_args_list, output.read_text()

    def test_trusted_author_and_sender_admit_before_scan(self):
        calls, output = self.prepare()
        self.assertEqual(calls[-1].args, (security.ADMISSION, "success"))
        self.assertIn("trusted=true", output)

    def test_outside_author_or_sender_must_wait_even_if_fork_owner(self):
        for field in ("author", "sender"):
            current, sender = pull(), pull()["user"]
            if field == "author":
                current["user"] = {"id": 999, "type": "User"}
                current["author_association"] = "OWNER"
            else:
                sender = {"id": 999, "type": "User"}
            calls, output = self.prepare(current, sender)
            self.assertIn("trusted=false", output)
            self.assertFalse(any(call.args[1] == "success" for call in calls))

    def test_merged_pr_still_scans_without_resetting_admission(self):
        current = pull()
        current.update(state="closed", merged=True)
        calls, output = self.prepare(current, parents=[BASE])  # Squash/rebase merge.
        self.assertEqual([call.args[0] for call in calls], [security.SCAN])
        self.assertIn(f"merge_sha={MERGE}", output)

    def test_merge_during_preparation_keeps_scan(self):
        current, later = pull(), pull()
        current["merge_commit_sha"] = None
        later.update(state="closed", merged=True)
        later["base"]["sha"] = MERGE
        calls, output = self.prepare(current, later=later, parents=[BASE])
        self.assertIn(f"merge_sha={MERGE}", output)
        self.assertFalse(any(call.args == (security.ADMISSION, "success") for call in calls))

    def test_stale_closed_and_conflicted_prs_do_not_admit(self):
        for change in ({"head": {"sha": "d" * 40}}, {"state": "closed"}):
            with self.assertRaises(RuntimeError):
                self.prepare({**pull(), **change})
        calls, output = self.prepare(parents=[BASE])
        self.assertIn("merge_sha=\n", output)
        self.assertFalse(any(call.args[1] == "success" for call in calls))

    def test_reporting_requires_scan_success_only_for_outsiders(self):
        for member in ("true", "false"):
            for result in ("success", "failure", "cancelled", "skipped", ""):
                with self.subTest(member=member, result=result), \
                        patch.dict(os.environ, {**ENV, "TRUSTED": member, "SCAN_RESULT": result}), \
                        patch.object(security, "api", return_value=pull()), patch.object(security, "status") as status:
                    if result == "success":
                        security.report()
                    else:
                        with self.assertRaises(RuntimeError):
                            security.report()
                    admission = [call.args for call in status.call_args_list if call.args[0] == security.ADMISSION]
                    self.assertEqual(admission, [] if member == "true" else
                                     [(security.ADMISSION, "success" if result == "success" else "failure")])

    def test_merged_or_changed_pr_retains_admission_when_scan_finishes(self):
        for change in ({"state": "closed", "merged": True}, {"head": {"sha": "d" * 40}},
                       {"base": {"sha": "d" * 40}}):
            with patch.dict(os.environ, ENV), patch.object(security, "api", return_value={**pull(), **change}), \
                    patch.object(security, "status") as status:
                security.report()
                status.assert_called_once_with(security.SCAN, "success")

    def test_ten_ids_include_cfast_developers_and_ignore_login_spoofing(self):
        entries = json.loads(security.POLICY.read_text())["developers"]
        self.assertEqual(len(entries), 10)
        for account_id in (13065776, 12814856):
            user = {"id": account_id, "type": "User", "login": "renamed"}
            self.assertTrue(security.trusted(user, user))
        for invalid in ({"id": 999, "login": "preneke", "type": "User"},
                        {"id": "13065776", "type": "User"}, {"id": 13065776, "type": "Bot"}, {}):
            self.assertFalse(security.trusted(invalid, invalid))


class ScannerTests(unittest.TestCase):
    def test_scan_checks_detection_and_both_commits(self):
        # Suppress the probe write: never create EICAR on a workstation.
        with patch.dict(os.environ, {**ENV, "MERGE_SHA": MERGE}), \
                patch.object(Path, "write_bytes"), patch.object(Path, "unlink"), \
                patch.object(security.subprocess, "run", return_value=subprocess.CompletedProcess([], 1, "Eicar FOUND")), \
                patch.object(security, "materialize") as files, patch.object(security, "clamav") as scan:
            security.scan()
            self.assertEqual([call.args[1] for call in files.call_args_list], [HEAD, MERGE])
            self.assertEqual(scan.call_count, 2)
            files.reset_mock()
            with patch.dict(os.environ, {"MERGE_SHA": ""}), self.assertRaises(RuntimeError):
                security.scan()
            self.assertEqual(files.call_count, 1)

    def test_failed_detection_self_test_stops_scan(self):
        with patch.object(Path, "write_bytes"), patch.object(Path, "unlink"), \
                patch.object(security.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "clean")), \
                patch.object(security, "materialize") as files, self.assertRaises(RuntimeError):
            security.scan()
        files.assert_not_called()

    def test_git_objects_are_materialized_as_data_and_unsafe_trees_fail(self):
        def run(command, **kwargs):
            if "cat-file" in command:
                kwargs["stdout"].write(b"data")
        entries = [b"100644 blob " + HEAD.encode() + b" 4\tSource/test.f90\0",
                   b"120000 blob " + HEAD.encode() + b" 4\tlink\0",
                   b"160000 commit " + HEAD.encode() + b" -\tmodule\0",
                   b"100644 blob " + HEAD.encode() + b" 4\t../escape\0", b""]
        for index, tree in enumerate(entries):
            with tempfile.TemporaryDirectory() as folder, patch.object(security.subprocess, "run", side_effect=run) as commands, \
                    patch.object(security.subprocess, "check_output", return_value=tree):
                if index == 0:
                    security.materialize("dev/cfast", HEAD, Path(folder))
                    self.assertEqual((Path(folder) / "Source/test.f90").read_bytes(), b"data")
                    self.assertFalse(any("checkout" in call.args[0] for call in commands.call_args_list))
                else:
                    with self.assertRaises(ValueError):
                        security.materialize("dev/cfast", HEAD, Path(folder))

    def test_limits_and_lfs_pointers_fail(self):
        tree = b"100644 blob " + HEAD.encode() + b" 4\tfile\0"
        for limit in ("MAX_FILE", "MAX_TOTAL", "MAX_FILES"):
            with tempfile.TemporaryDirectory() as folder, patch.object(security, limit, 0), \
                    patch.object(security.subprocess, "run"), patch.object(security.subprocess, "check_output", return_value=tree):
                with self.assertRaises(ValueError):
                    security.materialize("dev/cfast", HEAD, Path(folder))
        def run(command, **kwargs):
            if "cat-file" in command:
                kwargs["stdout"].write(b"version https://git-lfs.github.com/spec/v1\n")
        with tempfile.TemporaryDirectory() as folder, patch.object(security.subprocess, "run", side_effect=run), \
                patch.object(security.subprocess, "check_output", return_value=tree):
            with self.assertRaises(ValueError):
                security.materialize("dev/cfast", HEAD, Path(folder))

    def test_errors_warnings_and_incomplete_clamav_coverage_fail(self):
        for code, output in ((0, "Scanned files: 1\n"), (1, "Scanned files: 1\n"),
                             (2, "ERROR"), (0, "WARNING\nScanned files: 1\n"), (0, "Scanned files: 0\n")):
            with tempfile.TemporaryDirectory() as folder, patch.object(security.subprocess, "run",
                    return_value=subprocess.CompletedProcess([], code, output)):
                path = Path(folder)
                (path / "file").write_text("data")
                if code == 0 and output == "Scanned files: 1\n":
                    security.clamav(path)
                else:
                    with self.assertRaises(RuntimeError):
                        security.clamav(path)

    def test_fetch_failure_and_timeout_do_not_pass(self):
        with tempfile.TemporaryDirectory() as folder, patch.object(security.subprocess, "run",
                side_effect=subprocess.TimeoutExpired("git", 300)), self.assertRaises(subprocess.TimeoutExpired):
            security.materialize("dev/cfast", HEAD, Path(folder))


class WorkflowTests(unittest.TestCase):
    def test_job_dependencies_permissions_and_trusted_checkouts(self):
        root = Path(__file__).resolve().parents[1]
        script = 'require "yaml"; require "json"; puts JSON.generate(YAML.safe_load(File.read(ARGV[0])))'
        workflow = json.loads(subprocess.check_output(["ruby", "-e", script, str(root / "workflows/pr-security.yml")]))
        self.assertEqual(set(workflow.get("on", workflow.get("true"))), {"pull_request_target"})
        jobs = workflow["jobs"]
        self.assertEqual(set(jobs), {"developer", "scan", "report"})
        self.assertNotIn("needs", jobs["developer"])
        self.assertEqual(jobs["scan"]["needs"], "developer")
        self.assertEqual(jobs["report"]["needs"], ["developer", "scan"])
        self.assertEqual(jobs["scan"]["permissions"], {"contents": "read"})
        self.assertNotIn("GH_TOKEN", json.dumps(jobs["scan"]))
        for job in jobs.values():
            self.assertEqual(job["runs-on"], "ubuntu-24.04")
            self.assertNotIn("secrets", job)
            checkout = job["steps"][0]
            self.assertRegex(checkout["uses"], r"@[0-9a-f]{40}$")
            self.assertEqual(checkout["with"], {"ref": "${{ github.workflow_sha }}", "persist-credentials": False})
        self.assertIn("sudo freshclam", jobs["scan"]["steps"][1]["run"])


if __name__ == "__main__":
    unittest.main()

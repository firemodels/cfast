#!/usr/bin/env python3
"""Stage a CFAST Windows bundle and build a self-extracting installer EXE."""

import argparse
import os
import re
import shutil
import stat
import subprocess
import sys
import textwrap
import zipfile
from pathlib import Path


APP_NAME = "cedit"
WINDOWS_RUNTIME_DLLS = (
    "concrt140.dll",
    "libgcc_s_dw2-1.dll",
    "libgcc_s_seh-1.dll",
    "libgfortran-3.dll",
    "libgfortran-5.dll",
    "libgomp-1.dll",
    "libiomp5md.dll",
    "libifcoremd.dll",
    "libifcoremdd.dll",
    "libifportmd.dll",
    "libifportmdd.dll",
    "libimalloc.dll",
    "libintlc.dll",
    "libirc.dll",
    "libirng.dll",
    "libmmd.dll",
    "libquadmath-0.dll",
    "libstdc++-6.dll",
    "libwinpthread-1.dll",
    "msvcp140.dll",
    "svml_dispmd.dll",
    "ucrtbase.dll",
    "vcruntime140.dll",
    "vcruntime140_1.dll",
)
MANUAL_FILES = (
    ("CFAST_Configuration_Guide", "CFAST_Configuration_Guide.pdf"),
    ("CFAST_Tech_Ref", "CFAST_Tech_Ref.pdf"),
    ("CFAST_Users_Guide", "CFAST_Users_Guide.pdf"),
    ("CFAST_Validation_Guide", "CFAST_Validation_Guide.pdf"),
)
EXTRA_EXAMPLE_FILES = (
    "Large_Building.in",
)
RELEASE_MANUAL_ASSETS = tuple(filename for _, filename in MANUAL_FILES)
RELEASE_INFO_ASSET = "CFAST_INFO.txt"
CFAST_WINDOWS_BUILD_TARGETS = ("intel_win", "gnu_win")
SMV_WINDOWS_BUILD_TARGETS = ("intel_win", "clang_win")
DEFAULT_UPLOAD_RELEASE_TAG = os.environ.get("GH_CFAST_TAG", "CFAST_TEST")
DEFAULT_CFAST_REPO_URL = os.environ.get("CFAST_REPO_URL", "git@github.com:firemodels/cfast.git")
DEFAULT_SMV_REPO_URL = os.environ.get("SMV_REPO_URL", "git@github.com:firemodels/smv.git")
DEFAULT_FDS_REPO_URL = os.environ.get("FDS_REPO_URL", "git@github.com:firemodels/fds.git")


def default_repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def default_upload_release_repo() -> str:
    owner = os.environ.get("GH_OWNER")
    repo = os.environ.get("GH_REPO")
    if owner and repo:
        return f"{owner}/{repo}"
    return "firemodels/test_bundles"


def first_existing(paths):
    for path in paths:
        if path.exists():
            return path
    return paths[0]


def cfast_exe_for_build_target(repo_root: Path, target: str) -> Path:
    return repo_root / "Build/CFAST" / target / "cfast8_win.exe"


def smokeview_exe_for_build_target(firemodels_root: Path, target: str) -> Path:
    return firemodels_root / "smv/Build/smokeview" / target / "smokeview_win.exe"


def git_version(repo_root: Path) -> str:
    try:
        result = subprocess.run(
            ["git", "-C", str(repo_root), "describe", "--tags", "--dirty", "--always"],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except Exception:
        return "CFAST-windows"

    version = result.stdout.strip() or "CFAST"
    if version.upper().startswith("CFAST"):
        return f"{version}-windows"
    return f"CFAST-{version}-windows"


def sanitize_name(name: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]+", "-", name).strip("-") or "CFAST-windows"


def parse_bool(value) -> bool:
    if isinstance(value, bool):
        return value
    lowered = value.lower()
    if lowered in {"1", "true", "yes", "on"}:
        return True
    if lowered in {"0", "false", "no", "off"}:
        return False
    raise argparse.ArgumentTypeError(f"expected true or false, got {value!r}")


def require_file(path: Path, description: str) -> None:
    if not path.is_file():
        raise SystemExit(f"***error: {description} not found: {path}")


def require_command(command: str) -> None:
    if shutil.which(command) is None:
        raise SystemExit(f"***error: required command not found: {command}")


def require_dir(path: Path, description: str) -> None:
    if not path.is_dir():
        raise SystemExit(f"***error: {description} not found: {path}")


def run_checked(command, cwd: Path, description: str, shell: bool = False) -> None:
    try:
        subprocess.run(command, check=True, cwd=cwd, shell=shell)
    except subprocess.CalledProcessError as exc:
        raise SystemExit(f"***error: {description} failed with exit code {exc.returncode}")


def remove_tree(path: Path) -> None:
    def make_writable_and_retry(function, failing_path, _exc_info):
        os.chmod(failing_path, stat.S_IWRITE)
        function(failing_path)

    if path.exists():
        shutil.rmtree(path, onerror=make_writable_and_retry)


def command_processor() -> str:
    return os.environ.get("ComSpec") or os.environ.get("COMSPEC") or "cmd.exe"


def git_output(repo_dir: Path, args: list[str]) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo_dir), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout.strip()


def tracked_local_changes(repo_name: str, repo_dir: Path) -> str:
    try:
        subprocess.run(
            ["git", "-C", str(repo_dir), "update-index", "--refresh"],
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        status = git_output(repo_dir, ["status", "--short", "--untracked-files=no"])
    except subprocess.CalledProcessError as exc:
        raise SystemExit(f"***error: unable to inspect {repo_name} repo status: {repo_dir}\n{exc.stderr}")

    return status


def remote_branch_exists(repo_dir: Path, remote_name: str, branch_name: str) -> bool:
    return subprocess.run(
        [
            "git",
            "-C",
            str(repo_dir),
            "show-ref",
            "--verify",
            "--quiet",
            f"refs/remotes/{remote_name}/{branch_name}",
        ],
        check=False,
    ).returncode == 0


def update_git_repo(repo_name: str, repo_dir: Path, branch_name: str, strict_local_changes: bool = False) -> bool:
    require_dir(repo_dir / ".git", f"{repo_name} git repository")
    status = tracked_local_changes(repo_name, repo_dir)
    if status:
        if strict_local_changes:
            raise SystemExit(
                f"***error: {repo_name} repo has tracked local changes; refusing to update before strict bundle build.\n"
                f"         repo: {repo_dir}\n"
                f"{status}"
            )
        print(f"*** Warning: {repo_name} repo has tracked local changes; skipping update for this repo.")
        print(f"         repo: {repo_dir}")
        print(status)
        return False

    print(f"*** Updating {repo_name} repo")
    print(f"    branch: {branch_name}")
    print(f"    repo:   {repo_dir}")
    run_checked(["git", "checkout", branch_name], repo_dir, f"{repo_name} checkout {branch_name}")
    status = tracked_local_changes(repo_name, repo_dir)
    if status:
        if strict_local_changes:
            raise SystemExit(
                f"***error: {repo_name} repo has tracked local changes; refusing to update before strict bundle build.\n"
                f"         repo: {repo_dir}\n"
                f"{status}"
            )
        print(f"*** Warning: {repo_name} repo has tracked local changes; skipping update for this repo.")
        print(f"         repo: {repo_dir}")
        print(status)
        return False
    run_checked(["git", "remote", "update"], repo_dir, f"{repo_name} remote update")

    if remote_branch_exists(repo_dir, "origin", branch_name):
        run_checked(["git", "merge", "--ff-only", f"origin/{branch_name}"], repo_dir, f"{repo_name} merge origin/{branch_name}")
    if remote_branch_exists(repo_dir, "firemodels", branch_name):
        run_checked(
            ["git", "merge", "--ff-only", f"firemodels/{branch_name}"],
            repo_dir,
            f"{repo_name} merge firemodels/{branch_name}",
        )
    return True


def sync_cfast_repo(args) -> None:
    require_dir(args.repo_root / ".git", "cfast git repository")

    print("*** Synchronizing cfast repo")
    print(f"    repo:   {args.repo_root}")
    print(f"    remote: {args.cfast_repo_url}")
    if args.cfast_tag:
        print(f"    tag:    {args.cfast_tag}")
    else:
        print(f"    branch: {args.update_branch}")

    run_checked(["git", "reset", "--hard"], args.repo_root, "cfast tracked file reset")
    run_checked(["git", "clean", "-fd"], args.repo_root, "cfast untracked file cleanup")

    if args.cfast_tag:
        run_checked(["git", "fetch", "--tags", args.cfast_repo_url], args.repo_root, "cfast central tag fetch")
        run_checked(["git", "checkout", "--detach", args.cfast_tag], args.repo_root, f"cfast checkout tag {args.cfast_tag}")
    else:
        run_checked(["git", "fetch", args.cfast_repo_url, args.update_branch], args.repo_root, "cfast central branch fetch")
        run_checked(
            ["git", "checkout", "-B", args.update_branch, "FETCH_HEAD"],
            args.repo_root,
            f"cfast checkout {args.update_branch}",
        )


def clone_fresh_repo(repo_name: str, repo_url: str, repo_dir: Path, branch_name: str) -> None:
    require_command("git")

    print(f"*** Cloning fresh {repo_name} repo")
    print(f"    repo:   {repo_dir}")
    print(f"    remote: {repo_url}")
    print(f"    branch: {branch_name}")

    remove_tree(repo_dir)
    repo_dir.parent.mkdir(parents=True, exist_ok=True)
    run_checked(["git", "clone", "--depth", "1", "--branch", branch_name, repo_url, str(repo_dir)], repo_dir.parent, f"{repo_name} central clone")


def needs_fds_python_env(args) -> bool:
    return not args.python_was_set


def update_bundle_repos(args) -> None:
    if not args.update_repos:
        return
    if os.environ.get("CFAST_WINDOWS_BUNDLE_REEXECUTED") == "1":
        return

    firemodels_root = args.repo_root.parent
    updated_repo = False
    sync_cfast_repo(args)
    updated_repo = True
    clone_fresh_repo("smv", args.smv_repo_url, firemodels_root / "smv", args.update_branch)
    if needs_fds_python_env(args):
        clone_fresh_repo("fds", args.fds_repo_url, firemodels_root / "fds", args.update_branch)

    if not updated_repo:
        return

    print("*** Re-starting Windows bundle script after repo updates", flush=True)
    os.environ["CFAST_WINDOWS_BUNDLE_REEXECUTED"] = "1"
    os.execv(sys.executable, [sys.executable, str(Path(__file__).resolve()), *sys.argv[1:]])


def fds_venv_python(venv_dir: Path) -> Path:
    if os.name == "nt":
        return venv_dir / "Scripts/python.exe"
    return venv_dir / "bin/python"


def setup_fds_python_env(args) -> None:
    if not needs_fds_python_env(args):
        return

    fds_repo = args.repo_root.parent / "fds"
    venv_dir = fds_repo / ".github/fds_python_env"

    print("*** Preparing FDS Python environment")
    print(f"    repo:   {fds_repo}")

    require_dir(fds_repo / ".git", "FDS support git repository")

    requirements = fds_repo / ".github/requirements.txt"
    require_file(requirements, "FDS Python requirements")

    remove_tree(venv_dir)
    run_checked([args.python, "-m", "venv", str(venv_dir)], fds_repo, "FDS Python virtual environment creation")
    python_exe = fds_venv_python(venv_dir)
    require_file(python_exe, "FDS Python virtual environment executable")

    run_checked([str(python_exe), "-m", "pip", "install", "--upgrade", "pip"], fds_repo, "FDS Python pip upgrade")
    run_checked([str(python_exe), "-m", "pip", "install", "-r", "requirements.txt"], fds_repo / ".github", "FDS Python requirements install")

    args.python = str(python_exe)
    python_path = str(fds_repo / "Utilities/Python")
    if os.environ.get("PYTHONPATH"):
        os.environ["PYTHONPATH"] = python_path + os.pathsep + os.environ["PYTHONPATH"]
    else:
        os.environ["PYTHONPATH"] = python_path

    print("*** FDS Python environment ready:")
    print(f"    python: {args.python}")


def copy_file(from_path: Path, to_path: Path) -> None:
    to_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(from_path, to_path)


def copy_dir(from_path: Path, to_path: Path) -> None:
    if to_path.exists():
        shutil.rmtree(to_path)
    to_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copytree(from_path, to_path)


def copy_optional_file(from_path: Path, to_path: Path) -> None:
    if from_path.is_file():
        copy_file(from_path, to_path)


def copy_optional_dir(from_path: Path, to_path: Path) -> None:
    if from_path.is_dir():
        copy_dir(from_path, to_path)


def local_manual_sources(repo_root: Path) -> dict[str, Path]:
    return {
        filename: repo_root / "Manuals" / guide_dir / filename
        for guide_dir, filename in MANUAL_FILES
    }


def parse_cfast_info(info_file: Path) -> dict[str, str]:
    info = {}
    for line in info_file.read_text(encoding="utf-8").splitlines():
        parts = line.split(maxsplit=1)
        if len(parts) == 2:
            info[parts[0]] = parts[1]
    return info


def current_git_hash(repo_root: Path) -> str:
    try:
        result = subprocess.run(
            ["git", "-C", str(repo_root), "rev-parse", "HEAD"],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except Exception:
        return ""
    return result.stdout.strip()


def check_release_revision(repo_root: Path, info_file: Path) -> None:
    info = parse_cfast_info(info_file)
    release_hash = info.get("CFAST_HASH", "")
    local_hash = current_git_hash(repo_root)

    if not release_hash:
        raise SystemExit(f"***error: CFAST_INFO.txt does not contain CFAST_HASH: {info_file}")
    if not local_hash:
        raise SystemExit("***error: unable to determine local CFAST git hash")
    if not local_hash.startswith(release_hash):
        raise SystemExit(
            "***error: release manuals were generated from a different CFAST revision.\n"
            f"         release CFAST_HASH: {release_hash}\n"
            f"         local CFAST_HASH:   {local_hash[:len(release_hash)]}\n"
            "         Rerun after the Linux Cfastbot -U job has uploaded matching manuals."
        )


def download_release_manuals(args) -> dict[str, Path]:
    require_command("gh")

    print("*** Checking manual release assets")
    view_command = [
        "gh",
        "release",
        "view",
        args.manuals_release_tag,
        "-R",
        args.manuals_release_repo,
        "--json",
        "assets",
        "--jq",
        ".assets[].name",
    ]
    try:
        result = subprocess.run(
            view_command,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        raise SystemExit(
            "***error: unable to read GitHub release manual assets.\n"
            f"         release: {args.manuals_release_repo} {args.manuals_release_tag}\n"
            f"{exc.stderr}"
        )

    assets = {line.strip() for line in result.stdout.splitlines() if line.strip()}
    missing = [asset for asset in RELEASE_MANUAL_ASSETS if asset not in assets]
    if missing:
        raise SystemExit(
            "***error: release is missing required CFAST manual assets:\n"
            + "\n".join(f"         {asset}" for asset in missing)
        )
    if args.strict_revision and RELEASE_INFO_ASSET not in assets:
        raise SystemExit(f"***error: release is missing required revision asset: {RELEASE_INFO_ASSET}")

    download_dir = args.manuals_download_dir
    if download_dir.exists():
        shutil.rmtree(download_dir)
    download_dir.mkdir(parents=True)

    print("*** Downloading manual release assets")
    download_command = [
        "gh",
        "release",
        "download",
        args.manuals_release_tag,
        "-R",
        args.manuals_release_repo,
        "--dir",
        str(download_dir),
        "--clobber",
    ]
    download_assets = list(RELEASE_MANUAL_ASSETS)
    if RELEASE_INFO_ASSET in assets:
        download_assets.append(RELEASE_INFO_ASSET)
    for asset in download_assets:
        download_command.extend(["-p", asset])

    subprocess.run(download_command, check=True)

    for asset in RELEASE_MANUAL_ASSETS:
        require_file(download_dir / asset, f"release asset {asset}")

    if args.strict_revision:
        check_release_revision(args.repo_root, download_dir / RELEASE_INFO_ASSET)

    return {filename: download_dir / filename for _, filename in MANUAL_FILES}


def resolve_manual_sources(args) -> dict[str, Path]:
    if args.manuals_from_release:
        return download_release_manuals(args)
    return local_manual_sources(args.repo_root)


def build_cfast_executable(args) -> None:
    if not args.build_cfast:
        return
    if os.name != "nt":
        raise SystemExit("***error: CFAST Windows executable builds must run on Windows.")

    build_dir = args.repo_root / "Build/CFAST" / args.cfast_build_target
    require_dir(build_dir, f"CFAST build directory for {args.cfast_build_target}")

    print(f"*** Building CFAST Windows executable ({args.cfast_build_target})")
    if args.cfast_build_target == "intel_win":
        make_script = build_dir / "make_cfast.bat"
        require_file(make_script, "CFAST Intel Windows make script")
        run_checked([command_processor(), "/d", "/c", make_script.name], build_dir, "CFAST intel_win build")
    elif args.cfast_build_target == "gnu_win":
        require_command("make")
        run_checked(["make", "-f", "..\\makefile", "gnu_win"], build_dir, "CFAST gnu_win build")
    else:
        raise SystemExit(f"***error: unsupported CFAST Windows build target: {args.cfast_build_target}")


def build_cedit_app(args) -> None:
    if not args.include_cedit or not args.build_cedit:
        return
    if os.name != "nt":
        raise SystemExit("***error: CEditQt Windows app builds must run on Windows.")

    build_script = args.repo_root / "Build/CeditQt/build_windows_app.py"
    require_file(build_script, "CEditQt Windows build script")
    print("*** Building CEditQt Windows app")
    run_checked(
        [
            args.python,
            str(build_script),
            "--python",
            args.python,
            "--output-dir",
            str(args.cedit_app.parent),
            "--name",
            args.cedit_app.name,
        ],
        args.repo_root,
        "CEditQt Windows app build",
    )


def build_smokeview_executable(args) -> None:
    if not args.include_smokeview or not args.build_smokeview:
        return
    if os.name != "nt":
        raise SystemExit("***error: Smokeview Windows executable builds must run on Windows.")

    smv_root = args.repo_root.parent / "smv"
    libs_dir = smv_root / "Build/LIBS" / args.smokeview_build_target
    smokeview_dir = smv_root / "Build/smokeview" / args.smokeview_build_target
    libs_script = libs_dir / "make_LIBS.bat"
    smokeview_script = smokeview_dir / "make_smokeview.bat"

    require_file(libs_script, "Smokeview Windows library build script")
    require_file(smokeview_script, "Smokeview Windows build script")

    print(f"*** Building Smokeview Windows libraries ({args.smokeview_build_target})")
    run_checked([command_processor(), "/d", "/c", libs_script.name], libs_dir, "Smokeview Windows library build")

    print(f"*** Building Smokeview Windows executable ({args.smokeview_build_target})")
    run_checked([command_processor(), "/d", "/c", smokeview_script.name], smokeview_dir, "Smokeview Windows build")


def path_entries():
    seen = set()
    for entry in os.environ.get("PATH", "").split(os.pathsep):
        if not entry:
            continue
        path = Path(entry)
        key = str(path).lower()
        if key in seen:
            continue
        seen.add(key)
        yield path


def copy_windows_runtime_libraries(binary_path: Path, target_dir: Path) -> None:
    target_dir.mkdir(parents=True, exist_ok=True)
    copied = set()

    def copy_dll(candidate: Path) -> None:
        if not candidate.is_file():
            return
        key = candidate.name.lower()
        if key in copied:
            return
        copied.add(key)
        destination = target_dir / candidate.name
        if not destination.exists():
            print(f"*** Copying Windows runtime library: {candidate.name}")
            copy_file(candidate, destination)

    if binary_path.is_file():
        for candidate in binary_path.parent.glob("*.dll"):
            copy_dll(candidate)

    for dll_name in WINDOWS_RUNTIME_DLLS:
        for search_dir in path_entries():
            copy_dll(search_dir / dll_name)
            if dll_name.lower() in copied:
                break


def make_icon_from_png(python_exe: str, png_path: Path, ico_path: Path):
    if not png_path.is_file():
        return None

    script = (
        "from pathlib import Path\n"
        "import sys\n"
        "from PIL import Image\n"
        "png = Path(sys.argv[1])\n"
        "ico = Path(sys.argv[2])\n"
        "ico.parent.mkdir(parents=True, exist_ok=True)\n"
        "source = Image.open(png).convert('RGBA')\n"
        "transparent = source.getchannel('A').point(lambda value: 255 if value == 0 else 0)\n"
        "source.paste(Image.new('RGBA', source.size, (0, 0, 0, 0)), mask=transparent)\n"
        "sizes = [(16, 16), (24, 24), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)]\n"
        "source.save(ico, sizes=sizes)\n"
        "probe = Image.open(ico).convert('RGBA')\n"
        "if source.getpixel((0, 0))[3] == 0 and probe.getpixel((0, 0))[3] != 0:\n"
        "    raise SystemExit(f'generated icon does not preserve transparent corners: {ico}')\n"
    )
    try:
        subprocess.run([python_exe, "-c", script, str(png_path), str(ico_path)], check=True)
    except Exception:
        raise SystemExit(
            "***error: Pillow is required to create the Windows .ico from Source/CeditQt/assets/CeditQt.png.\n"
            f"         Try: {python_exe} -m pip install pillow"
        )
    return ico_path


def resolve_installer_icon(args):
    if args.icon:
        icon_path = args.icon.resolve()
        if not icon_path.is_file():
            raise SystemExit(f"***error: icon file not found: {icon_path}")
        return icon_path

    png_path = args.repo_root / "Source/CeditQt/assets/CeditQt.png"
    generated_icon = args.repo_root / "Build/bundle/icons/CeditQt.ico"
    icon_path = make_icon_from_png(args.python, png_path, generated_icon)
    if icon_path is not None:
        return icon_path

    icon_candidates = [
        args.repo_root / "Build/CeditQt/icons/CeditQt.ico",
        args.repo_root / "Source/CeditQt/assets/CeditQt.ico",
    ]
    return first_existing(icon_candidates)


def write_cfast_vars_bat(out_file: Path) -> None:
    out_file.write_text(
        textwrap.dedent(
            """\
            @echo off
            rem Add this CFAST bundle to PATH for the current command prompt.

            set "CFAST_HOME=%~dp0.."
            set "PATH=%CFAST_HOME%\\bin;%PATH%"
            if exist "%CFAST_HOME%\\SMV6" set "PATH=%CFAST_HOME%\\SMV6;%PATH%"

            echo CFAST_HOME=%CFAST_HOME%
            echo CFAST and Smokeview have been added to PATH for this command prompt.
            """
        ),
        encoding="utf-8",
        newline="\r\n",
    )


def write_cedit_launcher(out_file: Path) -> None:
    out_file.write_text(
        textwrap.dedent(
            """\
            @echo off
            setlocal

            set "CFAST_HOME=%~dp0.."
            set "PATH=%CFAST_HOME%\\bin;%PATH%"
            if exist "%CFAST_HOME%\\SMV6" set "PATH=%CFAST_HOME%\\SMV6;%PATH%"

            set "CEDIT_EXE=%CFAST_HOME%\\CEditQt\\cedit\\cedit.exe"
            if not exist "%CEDIT_EXE%" (
              echo ***error: CEditQt executable not found:
              echo          "%CEDIT_EXE%"
              exit /b 1
            )

            "%CEDIT_EXE%" %*
            exit /b %ERRORLEVEL%
            """
        ),
        encoding="utf-8",
        newline="\r\n",
    )


def write_readme(out_file: Path) -> None:
    out_file.write_text(
        textwrap.dedent(
            """\
            CFAST Windows Bundle
            ====================

            This bundle contains:

            - bin\\cfast.exe and bin\\cfast8_win.exe
            - bin\\CFASTVARS.bat
            - bin\\cedit.bat, if CEditQt was available when the bundle was made
            - CEditQt\\cedit, if CEditQt was available when the bundle was made
            - Documentation\\*.pdf
            - Examples\\*.in
            - SMV6\\smokeview.exe and SMV6\\smokeview_win.exe, if Smokeview was available

            To install from the self-extracting EXE, double-click it or run it from
            a command prompt. The default installation directory is:

                %ProgramFiles%\\firemodels\\CFAST8

            To install somewhere else, enter a different installation directory
            when prompted, or run the installer with --install-dir PATH.

            During interactive installation, the installer can create Desktop
            shortcuts for cedit and CMDcfast. cedit launches CFAST Editor
            (CEdit), and CMDcfast opens a command prompt and calls
            bin\\CFASTVARS.bat.

            To use CFAST from an existing command prompt:

                call "C:\\Program Files\\firemodels\\CFAST8\\bin\\CFASTVARS.bat"
                cfast "C:\\Program Files\\firemodels\\CFAST8\\Examples\\Users_Guide_Example.in"

            To launch CEditQt from a command prompt:

                cedit
            """
        ),
        encoding="utf-8",
        newline="\r\n",
    )


def stage_bundle(args) -> Path:
    repo_root = args.repo_root
    firemodels_root = repo_root.parent
    dist_name = args.name or git_version(repo_root)
    dist_dir = args.stage_dir / dist_name / "CFAST8"

    print("*** Staging CFAST Windows bundle")
    print(f"*** Distribution: {dist_name}")
    print(f"*** Stage: {dist_dir}")

    if (args.stage_dir / dist_name).exists():
        shutil.rmtree(args.stage_dir / dist_name)
    dist_dir.mkdir(parents=True)

    bin_dir = dist_dir / "bin"
    docs_dir = dist_dir / "Documentation"
    examples_dir = dist_dir / "Examples"
    bin_dir.mkdir()
    docs_dir.mkdir()
    examples_dir.mkdir()

    copy_file(args.cfast_exe, bin_dir / "cfast8_win.exe")
    copy_file(args.cfast_exe, bin_dir / "cfast.exe")
    copy_windows_runtime_libraries(args.cfast_exe, bin_dir)

    copy_examples(args, examples_dir)

    manual_sources = args.manual_sources or resolve_manual_sources(args)
    for _, filename in MANUAL_FILES:
        source = manual_sources[filename]
        require_file(source, f"CFAST manual {filename}")
        copy_file(source, docs_dir / filename)

    write_cfast_vars_bat(bin_dir / "CFASTVARS.bat")
    write_readme(dist_dir / "README.txt")

    if args.include_cedit:
        if args.cedit_app.is_dir():
            print("*** Adding CEditQt")
            copy_dir(args.cedit_app, dist_dir / "CEditQt" / APP_NAME)
            write_cedit_launcher(bin_dir / "cedit.bat")
        else:
            print("*** Warning: CEditQt app not found; continuing without CEditQt.")
            print(f"             cedit: {args.cedit_app}")

    if args.include_smokeview:
        if args.smokeview_exe.is_file() and args.smokeview_data.is_dir():
            print("*** Adding Smokeview")
            smv_dir = dist_dir / "SMV6"
            smv_dir.mkdir()
            copy_file(args.smokeview_exe, smv_dir / "smokeview_win.exe")
            copy_file(args.smokeview_exe, smv_dir / "smokeview.exe")
            copy_windows_runtime_libraries(args.smokeview_exe, smv_dir)
            copy_optional_file(args.smokeview_data / "objects.svo", smv_dir / "objects.svo")
            copy_optional_file(args.smokeview_data / "volrender.ssf", smv_dir / "volrender.ssf")
            copy_optional_file(args.smokeview_data / "smokeview.ini", smv_dir / "smokeview.ini")
            copy_optional_dir(args.smokeview_data / "colorbars", smv_dir / "colorbars")
            copy_optional_dir(args.smokeview_data / "textures", smv_dir / "textures")
        else:
            print("*** Warning: Smokeview artifacts not found; continuing without Smokeview.")
            print(f"             smokeview: {args.smokeview_exe}")
            print(f"             data:      {args.smokeview_data}")

    return dist_dir


def copy_examples(args, examples_dir: Path) -> None:
    copy_file(args.example_file, examples_dir / "Users_Guide_Example.in")
    for filename in EXTRA_EXAMPLE_FILES:
        source = args.extra_examples_dir / filename
        require_file(source, f"CFAST example {filename}")
        copy_file(source, examples_dir / filename)


def make_payload_zip(payload_root: Path, zip_path: Path) -> None:
    if zip_path.exists():
        zip_path.unlink()
    zip_path.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(payload_root.rglob("*")):
            archive.write(path, path.relative_to(payload_root))


def write_installer_script(script_path: Path) -> None:
    script_text = r"""#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path


def resource_path(name):
    base = Path(getattr(sys, "_MEIPASS", Path(__file__).resolve().parent))
    return base / name


def default_install_parent():
    program_files = os.environ.get("ProgramFiles")
    if program_files:
        return Path(program_files) / "firemodels"
    return Path.home() / "firemodels"


def default_install_dir():
    return default_install_parent() / "CFAST8"


def expanded_path(value):
    return Path(os.path.expandvars(os.path.expanduser(value.strip().strip('"'))))


def read_custom_install_dir():
    while True:
        value = input("Install directory: ").strip()
        if value.lower() in {"q", "quit", "exit"}:
            raise SystemExit(1)
        if not value:
            print("***error: no install directory was entered.")
            continue

        target = expanded_path(value)
        if target.parent.exists() and not target.parent.is_dir():
            print(f"***error: parent path is not a directory: {target.parent}")
            continue
        return target


def read_interactive_target(default_dir):
    print("")
    print("CFAST 8 Windows installer")
    print("")
    print(f"1) Install CFAST8 to {default_dir}")
    print("2) Install CFAST8 to another directory")
    print("q) Quit")
    print("")
    choice = input("Select an option [1]: ").strip()

    if choice in {"", "1"}:
        return default_dir
    if choice == "2":
        return read_custom_install_dir()
    if choice.lower() in {"q", "quit", "exit"}:
        print("Cancelled.")
        raise SystemExit(1)

    raise SystemExit(f"***error: unknown option: {choice}")


def extract_payload(payload_zip, target, overwrite):
    target = Path(target)

    if target.exists():
        if not overwrite:
            answer = input(f"{target} already exists. Replace it? [y/N]: ").strip().lower()
            if answer not in {"y", "yes"}:
                raise SystemExit("Install cancelled.")
        shutil.rmtree(target)

    target.parent.mkdir(parents=True, exist_ok=True)
    target.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(payload_zip, "r") as archive:
        archive.extractall(target)
    return target


def report_install_permission_error(error):
    locked_file = getattr(error, "filename", None)
    if getattr(error, "winerror", None) in {32, 33}:
        print("***error: the existing CFAST installation cannot be replaced because a file is in use.")
        print("         Close CEdit, Smokeview, and any CMDcfast or Command Prompt windows using CFAST,")
        print("         then run this installer again.")
        if locked_file:
            print(f"         File in use: {locked_file}")
        return

    print("***error: permission denied while installing CFAST.")
    if locked_file:
        print(f"         Unable to update: {locked_file}")
    print("         Close any CFAST programs that are running. If the problem continues,")
    print("         right-click this installer and choose Run as administrator, or choose a writable folder.")


def wait_to_close(silent):
    if not silent:
        input("Press Enter to close.")


def cedit_executable(install_root):
    return install_root / "CEditQt" / "cedit" / "cedit.exe"


def cfast_vars_bat(install_root):
    return install_root / "bin" / "CFASTVARS.bat"


def command_processor():
    return os.environ.get("ComSpec") or os.environ.get("COMSPEC") or "cmd.exe"


def should_create_desktop_shortcut(args, install_root):
    if not cedit_executable(install_root).is_file():
        return False
    if args.desktop_shortcut:
        return True
    if args.no_desktop_shortcut or args.silent:
        return False

    answer = input("Create a Desktop shortcut to cedit? [Y/n]: ").strip().lower()
    return answer in {"", "y", "yes"}


def should_create_cmdcfast_shortcut(args, install_root):
    if not cfast_vars_bat(install_root).is_file():
        return False
    if args.cmdcfast_shortcut:
        return True
    if args.no_cmdcfast_shortcut or args.silent:
        return False

    answer = input("Create a Desktop shortcut to CMDcfast? [Y/n]: ").strip().lower()
    return answer in {"", "y", "yes"}


def create_windows_shortcut(shortcut_name, target_path, working_directory, arguments="", icon_location=""):
    powershell = shutil.which("powershell") or shutil.which("pwsh")
    if powershell is None:
        print("*** Warning: PowerShell was not found; Desktop shortcut was not created.")
        return False

    script_text = r'''
param(
    [string]$TargetPath,
    [string]$ShortcutName,
    [string]$WorkingDirectory,
    [string]$Arguments,
    [string]$IconLocation
)

$Desktop = [Environment]::GetFolderPath("DesktopDirectory")
$ShortcutPath = Join-Path $Desktop $ShortcutName
$WshShell = New-Object -ComObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut($ShortcutPath)
$Shortcut.TargetPath = $TargetPath
$Shortcut.WorkingDirectory = $WorkingDirectory
if ($Arguments) {
    $Shortcut.Arguments = $Arguments
}
if ($IconLocation) {
    $Shortcut.IconLocation = $IconLocation
}
$Shortcut.Save()
'''

    script_file = None
    try:
        with tempfile.NamedTemporaryFile("w", suffix=".ps1", delete=False, encoding="utf-8-sig") as handle:
            script_file = Path(handle.name)
            handle.write(script_text)
        subprocess.run(
            [
                powershell,
                "-NoProfile",
                "-ExecutionPolicy",
                "Bypass",
                "-File",
                str(script_file),
                str(target_path),
                shortcut_name,
                str(working_directory),
                arguments,
                icon_location,
            ],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except Exception:
        print("*** Warning: Desktop shortcut creation failed.")
        return False
    finally:
        if script_file is not None:
            try:
                script_file.unlink()
            except OSError:
                pass

    return True


def create_desktop_shortcut(install_root):
    target_path = cedit_executable(install_root)
    if not target_path.is_file():
        return False

    return create_windows_shortcut(
        "cedit.lnk",
        target_path,
        target_path.parent,
        icon_location=f"{target_path},0",
    )


def create_cmdcfast_shortcut(install_root):
    vars_path = cfast_vars_bat(install_root)
    if not vars_path.is_file():
        return False

    return create_windows_shortcut(
        "CMDcfast.lnk",
        command_processor(),
        os.environ.get("USERPROFILE", str(Path.home())),
        arguments=f'/k call "{vars_path}"',
    )


def main():
    parser = argparse.ArgumentParser(description="Install the CFAST Windows bundle.")
    parser.add_argument("--install-dir", metavar="PATH", help="installation directory")
    parser.add_argument("--extract-to", metavar="PATH", help=argparse.SUPPRESS)
    parser.add_argument("--overwrite", action="store_true", help="replace an existing installation directory")
    parser.add_argument("--silent", action="store_true", help="use defaults without prompting")
    shortcut_group = parser.add_mutually_exclusive_group()
    shortcut_group.add_argument("--desktop-shortcut", action="store_true", help="create a Desktop shortcut to cedit")
    shortcut_group.add_argument("--no-desktop-shortcut", action="store_true", help="do not create a Desktop shortcut")
    cmdcfast_shortcut_group = parser.add_mutually_exclusive_group()
    cmdcfast_shortcut_group.add_argument("--cmdcfast-shortcut", action="store_true", help="create a Desktop shortcut to CMDcfast")
    cmdcfast_shortcut_group.add_argument("--no-cmdcfast-shortcut", action="store_true", help="do not create a CMDcfast Desktop shortcut")
    args = parser.parse_args()

    payload_zip = resource_path("payload.zip")
    if not payload_zip.is_file():
        raise SystemExit(f"***error: payload not found: {payload_zip}")

    if args.install_dir and args.extract_to:
        parser.error("--install-dir and --extract-to cannot be used together")
    if args.install_dir:
        target = Path(args.install_dir)
    elif args.extract_to:
        target = Path(args.extract_to) / "CFAST8"
    else:
        target = default_install_dir()
        if not args.silent:
            target = read_interactive_target(target)

    try:
        target = extract_payload(payload_zip, target, args.overwrite)
    except PermissionError as error:
        print("")
        report_install_permission_error(error)
        wait_to_close(args.silent)
        return 1

    print("")
    print(f"CFAST installed to: {target}")

    if should_create_desktop_shortcut(args, target):
        if create_desktop_shortcut(target):
            print("Desktop shortcut created: cedit")
    if should_create_cmdcfast_shortcut(args, target):
        if create_cmdcfast_shortcut(target):
            print("Desktop shortcut created: CMDcfast")
    print("")
    print("To use CFAST from a command prompt:")
    print(f'    call "{target}\\bin\\CFASTVARS.bat"')
    print(f'    cfast "{target}\\Examples\\Users_Guide_Example.in"')
    print("")
    wait_to_close(args.silent)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
"""
    script_path.write_text(
        script_text,
        encoding="utf-8",
    )


def build_self_extracting_exe(args, payload_root: Path) -> Path:
    if os.name != "nt":
        raise SystemExit("***error: Windows self-extracting EXE creation must run on Windows.")

    try:
        subprocess.run([args.python, "-c", "import PyInstaller"], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except Exception:
        raise SystemExit(f"***error: PyInstaller is required. Try: {args.python} -m pip install pyinstaller")

    dist_name = args.name or git_version(args.repo_root)
    exe_name = sanitize_name(dist_name)
    build_dir = args.stage_dir / "windows-self-extractor"
    payload_zip = build_dir / "payload.zip"
    installer_script = build_dir / "self_extract_installer.py"

    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir(parents=True)

    make_payload_zip(payload_root, payload_zip)
    write_installer_script(installer_script)
    icon_path = resolve_installer_icon(args)

    command = [
        args.python,
        "-m",
        "PyInstaller",
        "--noconfirm",
        "--onefile",
        "--clean",
        "--name",
        exe_name,
        "--distpath",
        str(args.output_dir),
        "--workpath",
        str(build_dir / "work"),
        "--specpath",
        str(build_dir / "spec"),
        "--add-data",
        f"{payload_zip};.",
    ]

    if not args.no_uac_admin:
        command.append("--uac-admin")

    if icon_path and icon_path.is_file():
        command.extend(["--icon", str(icon_path)])

    command.append(str(installer_script))

    print("*** Creating self-extracting EXE")
    subprocess.run(command, check=True)

    exe_path = args.output_dir / f"{exe_name}.exe"
    if not exe_path.is_file():
        raise SystemExit(f"***error: expected installer was not created: {exe_path}")
    return exe_path


def release_asset_names(release_repo: str, release_tag: str) -> list[str]:
    command = [
        "gh",
        "release",
        "view",
        release_tag,
        "-R",
        release_repo,
        "--json",
        "assets",
        "--jq",
        ".assets[].name",
    ]
    try:
        result = subprocess.run(
            command,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        raise SystemExit(
            "***error: unable to read GitHub release assets before upload.\n"
            f"         release: {release_repo} {release_tag}\n"
            f"{exc.stderr}"
        )
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def is_cfast_windows_bundle_asset(asset_name: str) -> bool:
    lower_name = asset_name.lower()
    return lower_name.startswith("cfast") and lower_name.endswith(".exe") and (
        "win" in lower_name or "windows" in lower_name
    )


def sign_windows_bundle(exe_path: Path) -> None:
    cert_sha1 = os.environ.get("CFAST_SIGNING_CERT_SHA1")
    signtool = os.environ.get("SIGNTOOL_EXE")

    if not cert_sha1:
        raise RuntimeError("CFAST_SIGNING_CERT_SHA1 is not set")
    if not signtool:
        raise RuntimeError("SIGNTOOL_EXE is not set")

    require_file(Path(signtool), "SignTool executable")

    print("*** Signing Windows bundle")
    run_checked(
        [
            signtool,
            "sign",
            "/sha1",
            cert_sha1,
            "/fd",
            "SHA256",
            str(exe_path),
        ]
    )

    print("*** Verifying Windows bundle signature")
    run_checked([signtool, "verify", "/pa", "/v", str(exe_path)])


def upload_windows_bundle(args, exe_path: Path) -> None:
    if not args.upload:
        return

    require_command("gh")
    release_repo = args.upload_release_repo
    release_tag = args.upload_release_tag

    print(f"*** Uploading Windows bundle to {release_repo} release {release_tag}")
    for asset_name in release_asset_names(release_repo, release_tag):
        if is_cfast_windows_bundle_asset(asset_name):
            print(f"*** Removing previous CFAST Windows bundle: {asset_name}")
            run_checked(
                [
                    "gh",
                    "release",
                    "delete-asset",
                    release_tag,
                    asset_name,
                    "-R",
                    release_repo,
                    "-y",
                ],
                args.repo_root,
                f"GitHub release asset removal for {asset_name}",
            )

    run_checked(
        [
            "gh",
            "release",
            "upload",
            release_tag,
            str(exe_path),
            "--clobber",
            "-R",
            release_repo,
        ],
        args.repo_root,
        "GitHub release upload",
    )


def parse_args():
    repo_root = default_repo_root()
    firemodels_root = repo_root.parent
    python_was_set = any(arg == "--python" or arg.startswith("--python=") for arg in sys.argv[1:])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--name", help="distribution folder and installer base name")
    parser.add_argument("--output-dir", type=Path, default=repo_root / "Build/bundle/windows", help="output directory")
    parser.add_argument("--stage-dir", type=Path, default=repo_root / "Build/bundle/stage", help="temporary staging directory")
    parser.add_argument("--update-branch", default="master", help="branch to update before building")
    parser.add_argument("--no-update-repos", dest="update_repos", action="store_false", help="do not sync cfast or fresh-clone smv/fds before bundling")
    parser.add_argument("--cfast-build-target", choices=CFAST_WINDOWS_BUILD_TARGETS, default="intel_win", help="CFAST Windows build target")
    parser.add_argument("--cfast-exe", type=Path, help="CFAST executable to bundle")
    parser.add_argument("--cfast-repo-url", default=DEFAULT_CFAST_REPO_URL, help="central CFAST repo URL used for updates")
    parser.add_argument("--cfast-tag", default=os.environ.get("CFAST_TAG", ""), help="checkout this CFAST tag after updating")
    parser.add_argument("--cedit-app", type=Path, default=repo_root / "Build/CeditQt/windows" / APP_NAME, help="CEditQt PyInstaller directory")
    parser.add_argument("--example", dest="example_file", type=Path, default=repo_root / "Utilities/for_bundle/Bin/Data/Users_Guide_Example.in", help="Users Guide example input file")
    parser.add_argument("--smokeview-build-target", choices=SMV_WINDOWS_BUILD_TARGETS, default="intel_win", help="Smokeview Windows build target")
    parser.add_argument("--smokeview-exe", type=Path, help="Smokeview executable to bundle")
    parser.add_argument("--smokeview-repo-url", dest="smv_repo_url", default=DEFAULT_SMV_REPO_URL, help="central Smokeview repo URL used for fresh clones")
    parser.add_argument("--smokeview-data", type=Path, default=firemodels_root / "smv/Build/for_bundle", help="Smokeview for_bundle directory")
    parser.add_argument("--fds-repo-url", default=DEFAULT_FDS_REPO_URL, help="central FDS repo URL used for fresh clones")
    parser.add_argument("--python", default=sys.executable, help="Python executable used to build the self-extracting EXE; disables fresh FDS Python env setup")
    parser.add_argument("--icon", type=Path, help="optional installer .ico file")
    parser.add_argument("--no-uac-admin", action="store_true", help="build installer without requesting administrator privileges")
    parser.add_argument("--no-build-cfast", dest="build_cfast", action="store_false", help="do not build CFAST before bundling")
    parser.add_argument("--no-build-cedit", dest="build_cedit", action="store_false", help="do not build CEditQt before bundling")
    parser.add_argument("--no-build-smokeview", dest="build_smokeview", action="store_false", help="do not build Smokeview before bundling")
    parser.add_argument("--no-cedit", dest="include_cedit", action="store_false", help="do not bundle CEditQt")
    parser.add_argument("--no-smokeview", dest="include_smokeview", action="store_false", help="do not bundle Smokeview")
    parser.add_argument("--manuals-from-release", action="store_true", help="download manuals from a GitHub release before bundling")
    parser.add_argument("--manuals-release-repo", default="firemodels/test_bundles", help="GitHub owner/repo containing released manual assets")
    parser.add_argument("--manuals-release-tag", default="CFAST_TEST", help="GitHub release tag containing released manual assets")
    parser.add_argument("--manuals-download-dir", type=Path, default=repo_root / "Build/bundle/stage/release-manuals", help="temporary directory for downloaded release manuals")
    parser.add_argument(
        "--upload",
        action="store_true",
        help="upload the Windows bundle to a GitHub release after it is created",
    )
    parser.add_argument(
        "--upload-release-repo",
        default=default_upload_release_repo(),
        help="GitHub owner/repo receiving the Windows bundle",
    )
    parser.add_argument(
        "--upload-release-tag",
        default=DEFAULT_UPLOAD_RELEASE_TAG,
        help="GitHub release tag receiving the Windows bundle",
    )
    parser.add_argument(
        "--strict-revision",
        dest="strict_revision",
        nargs="?",
        const=True,
        default=False,
        type=parse_bool,
        help="require CFAST_INFO.txt CFAST_HASH to match the local checkout",
    )
    parser.set_defaults(build_cfast=True, build_cedit=True, build_smokeview=True, include_cedit=True, include_smokeview=True, update_repos=True)
    args = parser.parse_args()
    args.python_was_set = python_was_set
    args.repo_root = repo_root
    args.output_dir.mkdir(parents=True, exist_ok=True)
    if args.cfast_exe is None:
        args.cfast_exe = cfast_exe_for_build_target(repo_root, args.cfast_build_target)
    if args.smokeview_exe is None:
        args.smokeview_exe = smokeview_exe_for_build_target(firemodels_root, args.smokeview_build_target)
    args.cfast_exe = args.cfast_exe.resolve()
    args.example_file = args.example_file.resolve()
    args.extra_examples_dir = (repo_root / "Utilities/for_bundle/Bin/Data").resolve()
    args.cedit_app = args.cedit_app.resolve()
    args.smokeview_exe = args.smokeview_exe.resolve()
    args.smokeview_data = args.smokeview_data.resolve()
    args.manuals_download_dir = args.manuals_download_dir.resolve()
    args.manual_sources = None
    return args


def main() -> int:
    args = parse_args()
    update_bundle_repos(args)
    setup_fds_python_env(args)
    args.manual_sources = resolve_manual_sources(args)
    build_cfast_executable(args)
    build_cedit_app(args)
    build_smokeview_executable(args)
    require_file(args.cfast_exe, "CFAST executable")
    require_file(args.example_file, "CFAST example file")

    dist_dir = stage_bundle(args)
    exe_path = build_self_extracting_exe(args, dist_dir)
    print("*** Self-extracting EXE created:")
    print(f"    {exe_path}")
    if args.upload:
        sign_windows_bundle(exe_path)
    upload_windows_bundle(args, exe_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Stage a CFAST Windows bundle and build a self-extracting installer EXE."""

import argparse
import os
import re
import shutil
import subprocess
import sys
import textwrap
import zipfile
from pathlib import Path


APP_NAME = "CFAST Editor (CEdit)"
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


def default_repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def first_existing(paths):
    for path in paths:
        if path.exists():
            return path
    return paths[0]


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


def require_file(path: Path, description: str) -> None:
    if not path.is_file():
        raise SystemExit(f"***error: {description} not found: {path}")


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

            set "CEDIT_EXE=%CFAST_HOME%\\CEditQt\\CFAST Editor (CEdit)\\CFAST Editor (CEdit).exe"
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

            - bin\\cfast.exe and bin\\cfast7_win.exe
            - bin\\CFASTVARS.bat
            - bin\\cedit.bat, if CEditQt was available when the bundle was made
            - CEditQt\\CFAST Editor (CEdit), if CEditQt was available when the bundle was made
            - Documentation\\*.pdf
            - Examples\\Users_Guide_Example.in
            - SMV6\\smokeview.exe and SMV6\\smokeview_win.exe, if Smokeview was available

            To install from the self-extracting EXE, double-click it or run it from
            a command prompt. The default install parent is:

                %ProgramFiles%\\firemodels

            The installed folder is:

                %ProgramFiles%\\firemodels\\CFAST

            To use CFAST from an existing command prompt:

                call "C:\\Program Files\\firemodels\\CFAST\\bin\\CFASTVARS.bat"
                cfast "C:\\Program Files\\firemodels\\CFAST\\Examples\\Users_Guide_Example.in"

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
    dist_dir = args.stage_dir / dist_name / "CFAST"

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

    copy_file(args.cfast_exe, bin_dir / "cfast7_win.exe")
    copy_file(args.cfast_exe, bin_dir / "cfast.exe")
    copy_windows_runtime_libraries(args.cfast_exe, bin_dir)

    copy_file(args.example_file, examples_dir / "Users_Guide_Example.in")

    copy_file(repo_root / "Manuals/CFAST_Configuration_Guide/CFAST_Configuration_Guide.pdf", docs_dir / "CFAST_Configuration_Guide.pdf")
    copy_file(repo_root / "Manuals/CFAST_Tech_Ref/CFAST_Tech_Ref.pdf", docs_dir / "CFAST_Tech_Ref.pdf")
    copy_file(repo_root / "Manuals/CFAST_Users_Guide/CFAST_Users_Guide.pdf", docs_dir / "CFAST_Users_Guide.pdf")
    copy_file(repo_root / "Manuals/CFAST_Validation_Guide/CFAST_Validation_Guide.pdf", docs_dir / "CFAST_Validation_Guide.pdf")

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


def make_payload_zip(payload_root: Path, zip_path: Path) -> None:
    if zip_path.exists():
        zip_path.unlink()
    zip_path.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(payload_root.rglob("*")):
            archive.write(path, path.relative_to(payload_root.parent))


def write_installer_script(script_path: Path) -> None:
    script_path.write_text(
        r"""#!/usr/bin/env python3
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


def read_destination(default_parent):
    print("")
    print("CFAST 8 self-extracting installer")
    print("")
    print(f"Default install parent: {default_parent}")
    print("The installer will create or replace a CFAST folder inside that parent.")
    print("")
    value = input("Install parent [press Enter for default, or q to quit]: ").strip()
    if value.lower() in {"q", "quit", "exit"}:
        raise SystemExit(1)
    if not value:
        return default_parent
    return Path(value)


def extract_payload(payload_zip, destination_parent, overwrite):
    target = destination_parent / "CFAST"

    if target.exists():
        if not overwrite:
            answer = input(f"{target} already exists. Replace it? [y/N]: ").strip().lower()
            if answer not in {"y", "yes"}:
                raise SystemExit("Install cancelled.")
        shutil.rmtree(target)

    destination_parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(payload_zip, "r") as archive:
        archive.extractall(destination_parent)
    return target


def cedit_executable(install_root):
    return install_root / "CEditQt" / "CFAST Editor (CEdit)" / "CFAST Editor (CEdit).exe"


def should_create_desktop_shortcut(args, install_root):
    if not cedit_executable(install_root).is_file():
        return False
    if args.desktop_shortcut:
        return True
    if args.no_desktop_shortcut or args.silent:
        return False

    answer = input("Create a Desktop shortcut to CFAST Editor (CEdit)? [Y/n]: ").strip().lower()
    return answer in {"", "y", "yes"}


def create_desktop_shortcut(install_root):
    target_path = cedit_executable(install_root)
    if not target_path.is_file():
        return False

    powershell = shutil.which("powershell") or shutil.which("pwsh")
    if powershell is None:
        print("*** Warning: PowerShell was not found; Desktop shortcut was not created.")
        return False

    script_text = r'''
param(
    [string]$TargetPath,
    [string]$ShortcutName
)

$Desktop = [Environment]::GetFolderPath("DesktopDirectory")
$ShortcutPath = Join-Path $Desktop $ShortcutName
$WshShell = New-Object -ComObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut($ShortcutPath)
$Shortcut.TargetPath = $TargetPath
$Shortcut.WorkingDirectory = Split-Path -Parent $TargetPath
$Shortcut.IconLocation = "$TargetPath,0"
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
                "CFAST Editor (CEdit).lnk",
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


def main():
    parser = argparse.ArgumentParser(description="Extract the CFAST Windows bundle.")
    parser.add_argument("--extract-to", metavar="PATH", help="install parent folder")
    parser.add_argument("--overwrite", action="store_true", help="replace an existing CFAST folder")
    parser.add_argument("--silent", action="store_true", help="use defaults without prompting")
    shortcut_group = parser.add_mutually_exclusive_group()
    shortcut_group.add_argument("--desktop-shortcut", action="store_true", help="create a Desktop shortcut to CFAST Editor (CEdit)")
    shortcut_group.add_argument("--no-desktop-shortcut", action="store_true", help="do not create a Desktop shortcut")
    args = parser.parse_args()

    payload_zip = resource_path("payload.zip")
    if not payload_zip.is_file():
        raise SystemExit(f"***error: payload not found: {payload_zip}")

    destination_parent = Path(args.extract_to) if args.extract_to else default_install_parent()
    if not args.silent and not args.extract_to:
        destination_parent = read_destination(destination_parent)

    try:
        target = extract_payload(payload_zip, destination_parent, args.overwrite)
    except PermissionError:
        print("")
        print("***error: permission denied while installing CFAST.")
        print("         Run this installer as administrator or choose a writable folder.")
        input("Press Enter to close.")
        return 1

    print("")
    print(f"CFAST installed to: {target}")
    if should_create_desktop_shortcut(args, target):
        if create_desktop_shortcut(target):
            print("Desktop shortcut created: CFAST Editor (CEdit)")
    print("")
    print("To use CFAST from a command prompt:")
    print(f'    call "{target}\\bin\\CFASTVARS.bat"')
    print(f'    cfast "{target}\\Examples\\Users_Guide_Example.in"')
    print("")
    if not args.silent:
        input("Press Enter to close.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
""",
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

    if args.icon and args.icon.is_file():
        command.extend(["--icon", str(args.icon)])

    command.append(str(installer_script))

    print("*** Creating self-extracting EXE")
    subprocess.run(command, check=True)

    exe_path = args.output_dir / f"{exe_name}.exe"
    if not exe_path.is_file():
        raise SystemExit(f"***error: expected installer was not created: {exe_path}")
    return exe_path


def parse_args():
    repo_root = default_repo_root()
    firemodels_root = repo_root.parent
    cfast_candidates = [
        repo_root / "Build/CFAST/intel_win/cfast7_win.exe",
        repo_root / "Build/CFAST/gnu_win/cfast7_win.exe",
        repo_root / "Utilities/for_bundle/Bin/cfast.exe",
    ]
    smv_candidates = [
        firemodels_root / "smv/Build/smokeview/intel_win/smokeview_win.exe",
        firemodels_root / "smv/Build/smokeview/gnu_win/smokeview_win.exe",
        repo_root / "Utilities/for_bundle/SMV6/smokeview.exe",
    ]
    icon_candidates = [
        repo_root / "Build/CeditQt/icons/CeditQt.ico",
        repo_root / "Source/CeditQt/assets/CeditQt.ico",
    ]
    default_icon = first_existing(icon_candidates)

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--name", help="distribution folder and installer base name")
    parser.add_argument("--output-dir", type=Path, default=repo_root / "Build/bundle/windows", help="output directory")
    parser.add_argument("--stage-dir", type=Path, default=repo_root / "Build/bundle/stage", help="temporary staging directory")
    parser.add_argument("--cfast-exe", type=Path, default=first_existing(cfast_candidates), help="CFAST executable to bundle")
    parser.add_argument("--cedit-app", type=Path, default=repo_root / "Build/CeditQt/windows" / APP_NAME, help="CEditQt PyInstaller directory")
    parser.add_argument("--example", dest="example_file", type=Path, default=repo_root / "Utilities/for_bundle/Bin/Data/Users_Guide_Example.in", help="example input file")
    parser.add_argument("--smokeview-exe", type=Path, default=first_existing(smv_candidates), help="Smokeview executable to bundle")
    parser.add_argument("--smokeview-data", type=Path, default=firemodels_root / "smv/Build/for_bundle", help="Smokeview for_bundle directory")
    parser.add_argument("--python", default=sys.executable, help="Python executable used to build the self-extracting EXE")
    parser.add_argument("--icon", type=Path, default=default_icon if default_icon.is_file() else None, help="optional installer .ico file")
    parser.add_argument("--no-cedit", dest="include_cedit", action="store_false", help="do not bundle CEditQt")
    parser.add_argument("--no-smokeview", dest="include_smokeview", action="store_false", help="do not bundle Smokeview")
    parser.set_defaults(include_cedit=True, include_smokeview=True)
    args = parser.parse_args()
    args.repo_root = repo_root
    args.output_dir.mkdir(parents=True, exist_ok=True)
    args.cfast_exe = args.cfast_exe.resolve()
    args.example_file = args.example_file.resolve()
    args.cedit_app = args.cedit_app.resolve()
    args.smokeview_exe = args.smokeview_exe.resolve()
    args.smokeview_data = args.smokeview_data.resolve()
    if args.icon is not None:
        args.icon = args.icon.resolve()
    return args


def main() -> int:
    args = parse_args()
    require_file(args.cfast_exe, "CFAST executable")
    require_file(args.example_file, "CFAST example file")

    dist_dir = stage_bundle(args)
    exe_path = build_self_extracting_exe(args, dist_dir)
    print("*** Self-extracting EXE created:")
    print(f"    {exe_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

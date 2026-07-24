#!/usr/bin/env python3
"""Build CEditQt as a Windows PyInstaller application."""

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path


APP_NAME = "CFAST Editor (CEdit)"


def repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def check_python_environment(python_exe: str) -> None:
    script = "import PyInstaller, PySide6, matplotlib"
    try:
        subprocess.run([python_exe, "-c", script], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except Exception:
        raise SystemExit(
            "***error: the selected Python environment must provide PyInstaller, PySide6, and matplotlib.\n"
            f"         Try: {python_exe} -m pip install pyinstaller"
        )


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
        "image = Image.open(png).convert('RGBA')\n"
        "image.save(ico, sizes=[(16, 16), (24, 24), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)])\n"
    )
    try:
        subprocess.run([python_exe, "-c", script, str(png_path), str(ico_path)], check=True)
    except Exception:
        raise SystemExit(
            "***error: Pillow is required to create the Windows .ico from assets/CeditQt.png.\n"
            f"         Try: {python_exe} -m pip install pillow"
        )
    return ico_path


def resolve_icon(args, root: Path):
    if args.icon:
        icon_path = args.icon.resolve()
        if not icon_path.is_file():
            raise SystemExit(f"***error: icon file not found: {icon_path}")
        return icon_path

    asset_dir = Path(__file__).resolve().parent / "assets"
    ico_path = asset_dir / "CeditQt.ico"
    if ico_path.is_file():
        return ico_path

    return make_icon_from_png(args.python, asset_dir / "CeditQt.png", root / "Build/CeditQt/icons/CeditQt.ico")


def parse_args():
    root = repo_root()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--python", default=os.environ.get("PYTHON", sys.executable), help="Python executable to use")
    parser.add_argument("--output-dir", type=Path, default=root / "Build/CeditQt/windows", help="application output directory")
    parser.add_argument("--name", default=APP_NAME, help="application name")
    parser.add_argument("--icon", type=Path, help="optional .ico file")
    parser.add_argument("--no-clean", dest="clean", action="store_false", help="reuse the previous PyInstaller work directory")
    parser.set_defaults(clean=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    root = repo_root()

    if os.name != "nt":
        raise SystemExit("***error: CEditQt Windows builds must be run on Windows.")

    check_python_environment(args.python)

    out_dir = args.output_dir.resolve()
    work_dir = root / "Build/CeditQt/pyinstaller-work"
    spec_dir = root / "Build/CeditQt/spec"
    cache_dir = root / "Build/CeditQt/pyinstaller-cache"
    app_path = out_dir / args.name

    if app_path.exists():
        shutil.rmtree(app_path)
    out_dir.mkdir(parents=True, exist_ok=True)
    work_dir.mkdir(parents=True, exist_ok=True)
    spec_dir.mkdir(parents=True, exist_ok=True)
    cache_dir.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env["PYINSTALLER_CONFIG_DIR"] = str(cache_dir)
    env["MPLCONFIGDIR"] = str(Path(os.environ.get("TEMP", str(root / "Build/CeditQt"))) / "cedit-qt-matplotlib")
    env["MPLBACKEND"] = "QtAgg"
    env["QT_API"] = "PySide6"

    icon_path = resolve_icon(args, root)

    command = [
        args.python,
        "-m",
        "PyInstaller",
        "--noconfirm",
        "--windowed",
        "--name",
        args.name,
        "--distpath",
        str(out_dir),
        "--workpath",
        str(work_dir),
        "--specpath",
        str(spec_dir),
        "--hidden-import",
        "matplotlib.backends.backend_qtagg",
        "--hidden-import",
        "matplotlib.backends.backend_qt",
        "--hidden-import",
        "matplotlib.backends.qt_compat",
        "--exclude-module",
        "PyQt5",
        "--exclude-module",
        "PyQt6",
        "--exclude-module",
        "PySide2",
        "--add-data",
        f"{Path(__file__).resolve().parent / 'assets'};assets",
    ]

    if icon_path is not None:
        command.extend(["--icon", str(icon_path)])
    if args.clean:
        command.append("--clean")
    command.append("cedit_qt.py")

    print(f"*** Building {args.name}")
    print(f"*** Python: {args.python}")
    print(f"*** Output: {out_dir}")
    if icon_path is not None:
        print(f"*** Icon: {icon_path}")

    subprocess.run(command, check=True, cwd=Path(__file__).resolve().parent, env=env)

    app_exe = app_path / f"{args.name}.exe"
    if not app_exe.is_file():
        raise SystemExit(f"***error: expected app executable was not created: {app_exe}")

    print("*** CEditQt Windows app built:")
    print(f"    {app_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

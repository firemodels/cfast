#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Tuple


os.environ.setdefault("MPLBACKEND", "Agg")
os.environ.setdefault("QT_API", "PySide6")
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")


@dataclass(frozen=True)
class TabFigure:
    tab_name: str
    filename: str


TAB_FIGURES = (
    TabFigure("Simulation", "Environment_Tab.png"),
    TabFigure("Thermal Properties", "Thermal_Properties_Tab.png"),
    TabFigure("Compartments", "Compartment_Geometry_Tab.png"),
    TabFigure("Wall Vents", "Natural_Flow_Tab.png"),
    TabFigure("Ceiling/Floor Vents", "Vertical_Flow_Tab.png"),
    TabFigure("Mechanical Ventilation", "Mechanical_Vent_Tab.png"),
    TabFigure("Fires", "Fire_Tab.png"),
    TabFigure("Targets", "Target_Tab.png"),
    TabFigure("Detection / Suppression", "Detector_Tab.png"),
    TabFigure("Surface Connections", "Surface_Connection_Tab.png"),
    TabFigure("Output", "Visualizations_Tab.png"),
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Generate CFAST Users Guide screenshots from the current CEdit Qt UI."
        )
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=None,
        help="CFAST repository root. Defaults to the first parent containing Source/CeditQt.",
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=None,
        help=(
            "Input file to load before capturing screenshots. Defaults to "
            "Utilities/for_bundle/Bin/Data/Users_Guide_Example.in."
        ),
    )
    parser.add_argument(
        "--cfast-exe",
        type=Path,
        default=None,
        help=(
            "CFAST executable used to generate the standard output dialog when "
            "the .status and .log files are not already present. Relative paths "
            "are interpreted from the CFAST repository root."
        ),
    )
    parser.add_argument(
        "--status",
        type=Path,
        default=None,
        help=(
            "CFAST status file to use for the standard output dialog. Defaults to "
            "the selected input file with a .status suffix."
        ),
    )
    parser.add_argument(
        "--log",
        type=Path,
        default=None,
        help=(
            "CFAST log file to use for the standard output dialog. Defaults to "
            "the selected input file with a .log suffix."
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=None,
        help=(
            "Directory for generated PNG files. Defaults to "
            "Manuals/CFAST_Users_Guide/SCRIPT_FIGURES."
        ),
    )
    parser.add_argument(
        "--width",
        type=int,
        default=1400,
        help="CEdit Qt window width in pixels. Defaults to 1400.",
    )
    parser.add_argument(
        "--height",
        type=int,
        default=900,
        help="CEdit Qt window height in pixels. Defaults to 900.",
    )
    return parser.parse_args()


def find_repo_root(start: Path) -> Path:
    for parent in [start.resolve(), *start.resolve().parents]:
        if (parent / "Source" / "CeditQt").is_dir():
            return parent
    raise RuntimeError("Could not find a CFAST repo root containing Source/CeditQt.")


def patch_message_boxes(QMessageBox) -> None:
    def quiet_message_box(*_args, **_kwargs):
        return QMessageBox.StandardButton.Ok

    QMessageBox.critical = quiet_message_box
    QMessageBox.information = quiet_message_box


def is_executable(path: Path) -> bool:
    return path.is_file() and os.access(str(path), os.X_OK)


def candidate_cfast_executables(repo_root: Path) -> list[Path]:
    # Cfastbot generates these screenshots during verification plotting on Linux.
    # Keep the GNU Linux build first for that workflow. For manual runs on macOS,
    # Windows, or another build tree, pass --cfast-exe with the desired executable.
    candidates = [
        repo_root / "Build" / "CFAST" / "gnu_linux" / "cfast7_linux",
        repo_root / "Build" / "CFAST" / "intel_linux" / "cfast7_linux",
        repo_root / "Build" / "CFAST" / "gnu_linux_db" / "cfast7_linux_db",
        repo_root / "Build" / "CFAST" / "intel_linux_db" / "cfast7_linux_db",
        repo_root / "Build" / "CFAST" / "gnu_osx" / "cfast7_osx",
        repo_root / "Build" / "CFAST" / "gnu_osx_db" / "cfast7_osx_db",
        repo_root / "Build" / "CFAST" / "intel_osx" / "cfast7_osx",
        repo_root / "Build" / "CFAST" / "intel_osx_db" / "cfast7_osx_db",
        repo_root / "Build" / "CFAST" / "gnu_win" / "cfast7_win.exe",
        repo_root / "Build" / "CFAST" / "intel_win" / "cfast7_win.exe",
        repo_root / "Build" / "CFAST" / "gnu_win_db" / "cfast7_win_db.exe",
        repo_root / "Build" / "CFAST" / "intel_win_db" / "cfast7_win_db.exe",
    ]
    path_exe = shutil.which("cfast")
    if path_exe:
        candidates.append(Path(path_exe))
    return candidates


def find_cfast_executable(repo_root: Path, requested: Optional[Path]) -> Path:
    if requested is not None:
        requested_path = requested.expanduser()
        if requested_path.is_absolute():
            path = requested_path.resolve()
        else:
            path = (repo_root / requested_path).resolve()
        if not is_executable(path):
            raise FileNotFoundError(path)
        return path

    for candidate in candidate_cfast_executables(repo_root):
        if is_executable(candidate):
            return candidate

    raise FileNotFoundError(
        "Could not find a CFAST executable. Use --cfast-exe to generate "
        "Standard_Output.png when the .status and .log files are absent."
    )


def run_cfast_for_output_files(
    repo_root: Path,
    cfast_exe: Optional[Path],
    input_path: Path,
) -> Tuple[Path, Path]:
    executable = find_cfast_executable(repo_root, cfast_exe)
    run_dir = Path(tempfile.mkdtemp(prefix="cedit-qt-guide-run-"))
    run_input = run_dir / input_path.name
    shutil.copy2(input_path, run_input)

    subprocess.run(
        [str(executable), run_input.name],
        cwd=str(run_dir),
        check=True,
        capture_output=True,
        text=True,
        timeout=120,
    )

    status_path = run_input.with_suffix(".status")
    log_path = run_input.with_suffix(".log")
    if not status_path.is_file():
        raise FileNotFoundError(status_path)
    if not log_path.is_file():
        raise FileNotFoundError(log_path)
    return status_path, log_path


def output_source_files(
    repo_root: Path,
    cfast_exe: Optional[Path],
    input_path: Path,
    status_path: Path,
    log_path: Path,
) -> Tuple[Path, Path]:
    if status_path.is_file() and log_path.is_file():
        return status_path, log_path
    return run_cfast_for_output_files(repo_root, cfast_exe, input_path)


def process_events(app, count: int = 4) -> None:
    for _index in range(count):
        app.processEvents()


def tab_index(window, tab_name: str) -> int:
    for index in range(window.tabs.count()):
        if window.tabs.tabText(index) == tab_name:
            return index
    raise RuntimeError(f"Could not find CEdit Qt tab named {tab_name!r}.")


def save_widget_figure(app, widget, output_path: Path) -> Path:
    process_events(app)
    if not widget.grab().save(str(output_path)):
        raise RuntimeError(f"Could not save screenshot: {output_path}")
    return output_path


def save_tab_figure(app, window, figure: TabFigure, output_dir: Path) -> Path:
    window.tabs.setCurrentIndex(tab_index(window, figure.tab_name))
    process_events(app)
    return save_widget_figure(app, window, output_dir / figure.filename)


def display_path(path: Path, repo_root: Path) -> str:
    try:
        return str(path.relative_to(repo_root))
    except ValueError:
        return str(path)


def save_standard_output_figure(
    app,
    window,
    RunMonitorDialog,
    input_path: Path,
    status_path: Path,
    log_path: Path,
    output_dir: Path,
) -> Path:
    if not status_path.is_file():
        raise FileNotFoundError(status_path)
    if not log_path.is_file():
        raise FileNotFoundError(log_path)

    case = window.build_cfast_case(require_compartments=False)
    dialog = RunMonitorDialog(window, input_path.name, case.simulation_time)
    dialog.update_status(status_path.read_text(encoding="utf-8", errors="replace"))
    dialog.set_finished(True, log_path.read_text(encoding="utf-8", errors="replace"))
    dialog.show()
    process_events(app)

    try:
        return save_widget_figure(app, dialog, output_dir / "Standard_Output.png")
    finally:
        dialog.close()
        process_events(app)


def main() -> int:
    args = parse_args()
    repo_root = find_repo_root(args.repo_root or Path(__file__))
    input_path = args.input or (
        repo_root / "Utilities" / "for_bundle" / "Bin" / "Data" / "Users_Guide_Example.in"
    )
    output_dir = args.output_dir or (
        repo_root / "Manuals" / "CFAST_Users_Guide" / "SCRIPT_FIGURES"
    )

    input_path = input_path.resolve()
    status_path = (args.status or input_path.with_suffix(".status")).resolve()
    log_path = (args.log or input_path.with_suffix(".log")).resolve()
    output_dir = output_dir.resolve()

    if not input_path.is_file():
        raise FileNotFoundError(input_path)

    output_dir.mkdir(parents=True, exist_ok=True)

    qt_settings_dir = Path(tempfile.mkdtemp(prefix="cedit-qt-guide-settings-"))
    mpl_config_dir = Path(tempfile.gettempdir()) / "cedit-qt-guide-matplotlib"
    mpl_config_dir.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("MPLCONFIGDIR", str(mpl_config_dir))

    sys.path.insert(0, str((repo_root / "Source" / "CeditQt").resolve()))

    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import QApplication, QMessageBox

    QSettings.setDefaultFormat(QSettings.Format.IniFormat)
    QSettings.setPath(
        QSettings.Format.IniFormat,
        QSettings.Scope.UserScope,
        str(qt_settings_dir),
    )

    from main_window import CeditMainWindow, RunMonitorDialog

    patch_message_boxes(QMessageBox)

    app = QApplication.instance() or QApplication([])
    window = CeditMainWindow()
    window.resize(args.width, args.height)
    window.load_cfast_input(input_path)
    window.simulation_tab.set_message("")
    window.statusBar().showMessage("No Errors")
    window.show()
    process_events(app)

    try:
        for figure in TAB_FIGURES:
            path = save_tab_figure(app, window, figure, output_dir)
            print(display_path(path, repo_root))

        status_path, log_path = output_source_files(
            repo_root,
            args.cfast_exe,
            input_path,
            status_path,
            log_path,
        )

        path = save_standard_output_figure(
            app,
            window,
            RunMonitorDialog,
            input_path,
            status_path,
            log_path,
            output_dir,
        )
        print(display_path(path, repo_root))
    finally:
        window.close()
        process_events(app)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

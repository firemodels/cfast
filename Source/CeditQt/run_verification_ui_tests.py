from __future__ import annotations

import argparse
import csv
import math
import os
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

_MPLCONFIGDIR = Path(tempfile.gettempdir()) / "cedit-qt-matplotlib"
_MPLCONFIGDIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(_MPLCONFIGDIR))
os.environ.setdefault("MPLBACKEND", "Agg")
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QProcess
from PySide6.QtWidgets import QApplication, QMessageBox

from main_window import CeditMainWindow


HELPER_SCRIPTS = {
    "fire_ignition.py": [
        "Fires/Ignition_Test_compartments.csv",
        "Fires/Ignition_Test_devices.csv",
    ],
    "radiativefluxes.py": [
        "Radiation/radiation_3_devices.csv",
        "Radiation/radiation_4_devices.csv",
        "Radiation/radiation_5_devices.csv",
    ],
    "speciesmass.py": [
        "Mass_Balance/species_mass_1_masses.csv",
        "Mass_Balance/species_mass_2_masses.csv",
        "Mass_Balance/species_mass_3_masses.csv",
        "Mass_Balance/species_mass_4_masses.csv",
    ],
    "sprinkler_1.py": [
        "Sprinkler/sprinkler_1_compartments.csv",
        "Sprinkler/sprinkler_1_devices.csv",
    ],
    "e_coefficient.py": [
        "Sprinkler/e_coefficient_compartments.csv",
        "Sprinkler/e_coefficient_devices.csv",
        "Sprinkler/e_coefficient.smv",
    ],
    "target_2.py": [
        "Target/target_2_devices.csv",
    ],
}


@dataclass
class CaseResult:
    path: Path
    ok: bool
    elapsed: float
    message: str


@dataclass
class HelperResult:
    script: str
    status: str
    message: str


@dataclass
class ComparisonResult:
    row_number: int
    dataname: str
    status: str
    quantity: str
    metric: str
    error: Optional[float]
    tolerance: Optional[float]
    message: str


@dataclass
class Series:
    x: list[float]
    y: list[float]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Load CFAST cases through the CEdit Qt UI. By default the script runs "
            "verification cases through CFAST; in rewrite mode it only imports and "
            "writes input files for UI regression testing."
        )
    )
    parser.add_argument(
        "--mode",
        choices=["run", "rewrite"],
        default="run",
        help="Test mode. 'run' executes CFAST; 'rewrite' only loads and writes inputs.",
    )
    parser.add_argument(
        "--cfast-exe",
        default="cfast",
        help="CFAST executable to run. Defaults to cfast from PATH.",
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=None,
        help="CFAST repository root. Defaults to the first parent containing Verification/.",
    )
    parser.add_argument(
        "--case",
        action="append",
        default=[],
        help=(
            "Case stem or filename to run. May be repeated. "
            "Defaults to all .in cases recursively under Verification/."
        ),
    )
    parser.add_argument(
        "--input",
        action="append",
        type=Path,
        default=[],
        help=(
            "Specific input file to load. May be absolute or relative to the repo root. "
            "May be repeated. Intended for rewrite mode."
        ),
    )
    parser.add_argument(
        "--work-dir",
        type=Path,
        default=None,
        help=(
            "Directory for the copied verification tree. "
            "Defaults to a new directory under the system temp directory."
        ),
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=300.0,
        help="Per-case timeout in seconds. Defaults to 300.",
    )
    parser.add_argument(
        "--skip-helpers",
        action="store_true",
        help="Do not run derived-CSV helper scripts after CFAST completes.",
    )
    parser.add_argument(
        "--compare-targets",
        action="store_true",
        help=(
            "Run the experimental built-in CSV target comparison. "
            "The official pass/fail check remains Utilities/Python/CFAST_verification_script.py."
        ),
    )
    parser.add_argument(
        "--keep-work-dir",
        action="store_true",
        help="Keep the temporary work directory after the run.",
    )
    return parser.parse_args()


def find_repo_root(start: Path) -> Path:
    for parent in [start.resolve(), *start.resolve().parents]:
        if (parent / "Verification").is_dir() and (parent / "Utilities" / "Python").is_dir():
            return parent
    raise RuntimeError("Could not find a CFAST repo root containing Verification/.")


def resolve_executable(executable: str) -> str:
    if executable == "cfast":
        return executable

    path = Path(executable).expanduser()
    if path.is_absolute() or any(separator in executable for separator in ("/", os.sep)):
        return str(path.resolve())

    return executable


def prepare_work_tree(repo_root: Path, work_dir: Optional[Path]) -> tuple[Path, bool]:
    if work_dir is None:
        temp_dir = Path(tempfile.mkdtemp(prefix="cedit-qt-verification-"))
        remove_when_done = True
    else:
        temp_dir = work_dir.resolve()
        temp_dir.mkdir(parents=True, exist_ok=True)
        remove_when_done = False

    verification_copy = temp_dir / "Verification"
    utilities_copy = temp_dir / "Utilities" / "Python"

    if verification_copy.exists():
        raise RuntimeError(f"Work tree already exists: {verification_copy}")

    shutil.copytree(repo_root / "Verification", verification_copy)
    shutil.copytree(repo_root / "Utilities" / "Python" / "scripts", utilities_copy / "scripts")
    shutil.copy2(
        repo_root / "Utilities" / "Python" / "CFAST_verification_dataplot_inputs.csv",
        utilities_copy / "CFAST_verification_dataplot_inputs.csv",
    )

    return temp_dir, remove_when_done


def select_cases(verification_root: Path, requested: list[str]) -> list[Path]:
    cases = sorted(verification_root.rglob("*.in"))
    if not requested:
        return cases

    selected: list[Path] = []
    requested_normalized = [value.lower() for value in requested]

    for path in cases:
        names = {path.name.lower(), path.stem.lower(), str(path.relative_to(verification_root)).lower()}
        if any(value in names for value in requested_normalized):
            selected.append(path)

    missing = sorted(set(requested_normalized) - {item for path in selected for item in {path.name.lower(), path.stem.lower(), str(path.relative_to(verification_root)).lower()}})
    if missing:
        raise RuntimeError("No Verification .in case matched: " + ", ".join(missing))

    return selected


def resolve_input_cases(repo_root: Path, requested: list[Path]) -> list[Path]:
    cases: list[Path] = []
    for value in requested:
        path = value.expanduser()
        if not path.is_absolute():
            path = repo_root / path
        path = path.resolve()
        if not path.is_file():
            raise RuntimeError(f"Input file not found: {path}")
        cases.append(path)
    return cases


def patch_message_boxes() -> None:
    def quiet_message_box(*args, **kwargs):
        return QMessageBox.StandardButton.Ok

    QMessageBox.critical = quiet_message_box
    QMessageBox.information = quiet_message_box


def run_case(app: QApplication, cfast_exe: str, case_path: Path, timeout: float) -> CaseResult:
    window = CeditMainWindow()
    window.cfast_executable = cfast_exe

    start_time = time.monotonic()
    try:
        window.load_cfast_input(case_path)
        app.processEvents()

        if window.statusBar().currentMessage() == "Errors":
            return CaseResult(
                path=case_path,
                ok=False,
                elapsed=time.monotonic() - start_time,
                message=window.simulation_tab.message_panel.toPlainText().strip(),
            )

        window.run_cfast()
        app.processEvents()

        if window.cfast_process is None:
            return CaseResult(
                path=case_path,
                ok=False,
                elapsed=time.monotonic() - start_time,
                message=window.simulation_tab.message_panel.toPlainText().strip(),
            )

        deadline = time.monotonic() + timeout
        while window.cfast_process is not None and time.monotonic() < deadline:
            app.processEvents()
            if (
                window.cfast_process is not None
                and window.cfast_process.state() == QProcess.ProcessState.NotRunning
            ):
                break
            time.sleep(0.02)

        if window.cfast_process is not None:
            if window.cfast_process.state() == QProcess.ProcessState.NotRunning:
                message = window.simulation_tab.message_panel.toPlainText().strip()
                return CaseResult(
                    path=case_path,
                    ok=False,
                    elapsed=time.monotonic() - start_time,
                    message=message,
                )
            window.cfast_process.kill()
            window.cfast_process.waitForFinished(5000)
            return CaseResult(
                path=case_path,
                ok=False,
                elapsed=time.monotonic() - start_time,
                message=f"Timed out after {timeout:g} s.",
            )

        message = window.simulation_tab.message_panel.toPlainText().strip()
        ok = (
            window.statusBar().currentMessage() == "No Errors"
            and "CFAST completed successfully" in message
        )
        return CaseResult(
            path=case_path,
            ok=ok,
            elapsed=time.monotonic() - start_time,
            message=message,
        )
    finally:
        window.close()
        app.processEvents()


def rewrite_case(app: QApplication, repo_root: Path, output_root: Path, case_path: Path) -> CaseResult:
    window = CeditMainWindow()
    start_time = time.monotonic()

    try:
        window.load_cfast_input(case_path)
        app.processEvents()

        if window.statusBar().currentMessage() == "Errors":
            return CaseResult(
                path=case_path,
                ok=False,
                elapsed=time.monotonic() - start_time,
                message=window.simulation_tab.message_panel.toPlainText().strip(),
            )

        try:
            relative_path = case_path.relative_to(repo_root)
        except ValueError:
            relative_path = Path(case_path.name)

        output_path = output_root / relative_path
        output_path.parent.mkdir(parents=True, exist_ok=True)
        written_path = window.write_case_to_path(output_path)
        app.processEvents()

        if written_path is None:
            return CaseResult(
                path=case_path,
                ok=False,
                elapsed=time.monotonic() - start_time,
                message=window.simulation_tab.message_panel.toPlainText().strip(),
            )

        ok = (
            window.statusBar().currentMessage() == "No Errors"
            and written_path.is_file()
            and written_path.stat().st_size > 0
        )
        return CaseResult(
            path=case_path,
            ok=ok,
            elapsed=time.monotonic() - start_time,
            message=window.simulation_tab.message_panel.toPlainText().strip(),
        )
    except Exception as exc:
        return CaseResult(
            path=case_path,
            ok=False,
            elapsed=time.monotonic() - start_time,
            message=str(exc),
        )
    finally:
        window.close()
        app.processEvents()


def run_helper_scripts(work_root: Path) -> list[HelperResult]:
    results: list[HelperResult] = []
    verification_root = work_root / "Verification"
    utilities_root = work_root / "Utilities" / "Python"

    for script, required_files in HELPER_SCRIPTS.items():
        missing = [path for path in required_files if not (verification_root / path).exists()]
        if missing:
            results.append(
                HelperResult(
                    script=script,
                    status="SKIP",
                    message="Missing input files: " + ", ".join(missing),
                )
            )
            continue

        proc = subprocess.run(
            [sys.executable, f"scripts/{script}"],
            cwd=utilities_root,
            text=True,
            capture_output=True,
        )
        if proc.returncode == 0:
            results.append(HelperResult(script=script, status="PASS", message=""))
        else:
            message = (proc.stderr or proc.stdout).strip()
            results.append(
                HelperResult(
                    script=script,
                    status="FAIL",
                    message=message.splitlines()[-1] if message else "Helper failed.",
                )
            )

    return results


def load_dataplot_rows(config_path: Path) -> list[tuple[int, dict[str, str]]]:
    rows: list[tuple[int, dict[str, str]]] = []
    with config_path.open(newline="", encoding="utf-8-sig") as infile:
        reader = csv.DictReader(infile)
        for row_number, row in enumerate(reader, start=2):
            if row.get("switch_id", "").strip().lower() == "d":
                rows.append((row_number, row))
    return rows


def split_columns(value: str) -> list[str]:
    return [item.strip() for item in value.split("|") if item.strip()]


def float_or_none(value: Optional[str]) -> Optional[float]:
    if value is None:
        return None
    text = value.strip()
    if not text:
        return None
    try:
        return float(text.replace("D", "E").replace("d", "e"))
    except ValueError:
        return None


def int_field(row: dict[str, str], key: str, default: int) -> int:
    value = float_or_none(row.get(key))
    return default if value is None else int(value)


def read_series(
    path: Path,
    col_name_row: int,
    data_row: int,
    x_column: str,
    y_column: str,
) -> Series:
    with path.open(newline="", encoding="utf-8-sig") as infile:
        rows = list(csv.reader(infile))

    if col_name_row < 1 or col_name_row > len(rows):
        raise ValueError(f"Header row {col_name_row} is outside {path}.")

    headers = [header.strip() for header in rows[col_name_row - 1]]
    try:
        x_index = headers.index(x_column)
    except ValueError as exc:
        raise ValueError(f"Column {x_column!r} not found in {path}.") from exc
    try:
        y_index = headers.index(y_column)
    except ValueError as exc:
        raise ValueError(f"Column {y_column!r} not found in {path}.") from exc

    x_values: list[float] = []
    y_values: list[float] = []
    for raw_row in rows[data_row - 1 :]:
        if max(x_index, y_index) >= len(raw_row):
            continue

        x_value = float_or_none(raw_row[x_index])
        y_value = float_or_none(raw_row[y_index])
        if x_value is None or y_value is None:
            continue

        x_values.append(x_value)
        y_values.append(y_value)

    if not x_values:
        raise ValueError(f"No numeric data found for {x_column!r}/{y_column!r} in {path}.")

    return Series(x=x_values, y=y_values)


def filter_series(series: Series, start: Optional[float], end: Optional[float]) -> Series:
    x_values: list[float] = []
    y_values: list[float] = []

    for x_value, y_value in zip(series.x, series.y):
        if start is not None and x_value < start:
            continue
        if end is not None and x_value > end:
            continue
        x_values.append(x_value)
        y_values.append(y_value)

    return Series(x=x_values, y=y_values)


def interpolate(series: Series, x_value: float) -> Optional[float]:
    if x_value < min(series.x) or x_value > max(series.x):
        return None

    for left in range(len(series.x) - 1):
        x0 = series.x[left]
        x1 = series.x[left + 1]
        y0 = series.y[left]
        y1 = series.y[left + 1]

        if x0 == x_value:
            return y0
        if x1 == x_value:
            return y1
        if (x0 <= x_value <= x1) or (x1 <= x_value <= x0):
            if abs(x1 - x0) < 1.0e-30:
                return y0
            fraction = (x_value - x0) / (x1 - x0)
            return y0 + fraction * (y1 - y0)

    return series.y[-1] if x_value == series.x[-1] else None


def pair_columns(first: list[str], second: list[str]) -> list[tuple[str, str]]:
    if len(first) == len(second):
        return list(zip(first, second))
    if len(first) == 1:
        return [(first[0], value) for value in second]
    if len(second) == 1:
        return [(value, second[0]) for value in first]
    raise ValueError(f"Column count mismatch: {first!r} vs {second!r}.")


def point_error(expected: float, predicted: float, quantity: str) -> Optional[float]:
    delta = abs(predicted - expected)
    quantity_upper = quantity.upper()

    if quantity_upper == "ABSOLUTE":
        return delta
    if quantity_upper == "RELATIVE":
        denominator = abs(expected)
        if denominator < 1.0e-30:
            return 0.0 if delta < 1.0e-30 else None
        return delta / denominator
    return None


def compare_series(expected: Series, predicted: Series, quantity: str, metric: str) -> Optional[float]:
    if not expected.x or not predicted.x:
        return None

    overlap_start = max(min(expected.x), min(predicted.x))
    overlap_end = min(max(expected.x), max(predicted.x))
    comparison_points = [
        (x_value, y_value)
        for x_value, y_value in zip(expected.x, expected.y)
        if overlap_start <= x_value <= overlap_end
    ]

    if not comparison_points:
        return None

    if metric == "end":
        comparison_points = [comparison_points[-1]]

    errors: list[float] = []
    for x_value, expected_value in comparison_points:
        predicted_value = interpolate(predicted, x_value)
        if predicted_value is None:
            continue

        error = point_error(expected_value, predicted_value, quantity)
        if error is not None and math.isfinite(error):
            errors.append(error)

    if not errors:
        return None
    if metric == "mean":
        return sum(errors) / len(errors)
    return max(errors)


def compare_row(verification_root: Path, row_number: int, row: dict[str, str]) -> ComparisonResult:
    dataname = row.get("Dataname", "")
    quantity = row.get("Quantity", "").strip()
    metric = row.get("Metric", "").strip().lower()
    tolerance = float_or_none(row.get("Error_Tolerance"))

    if quantity.upper() == "N/A":
        return ComparisonResult(
            row_number=row_number,
            dataname=dataname,
            status="INFO",
            quantity=quantity,
            metric=metric,
            error=None,
            tolerance=tolerance,
            message="Dataplot row is not scored.",
        )

    d1_file = verification_root / row["d1_Filename"]
    d2_file = verification_root / row["d2_Filename"]
    if not d1_file.exists() or not d2_file.exists():
        missing = [str(path.relative_to(verification_root)) for path in [d1_file, d2_file] if not path.exists()]
        return ComparisonResult(
            row_number=row_number,
            dataname=dataname,
            status="SKIP",
            quantity=quantity,
            metric=metric,
            error=None,
            tolerance=tolerance,
            message="Missing CSV: " + ", ".join(missing),
        )

    try:
        d1_columns = split_columns(row["d1_Dep_Col_Name"])
        d2_columns = split_columns(row["d2_Dep_Col_Name"])
        column_pairs = pair_columns(d1_columns, d2_columns)
        errors: list[float] = []

        for d1_column, d2_column in column_pairs:
            expected = read_series(
                d1_file,
                int_field(row, "d1_Col_Name_Row", 1),
                int_field(row, "d1_Data_Row", 2),
                row["d1_Ind_Col_Name"].strip(),
                d1_column,
            )
            predicted = read_series(
                d2_file,
                int_field(row, "d2_Col_Name_Row", 1),
                int_field(row, "d2_Data_Row", 2),
                row["d2_Ind_Col_Name"].strip(),
                d2_column,
            )
            expected = filter_series(
                expected,
                float_or_none(row.get("d1_Comp_Start")),
                float_or_none(row.get("d1_Comp_End")),
            )
            predicted = filter_series(
                predicted,
                float_or_none(row.get("d2_Comp_Start")),
                float_or_none(row.get("d2_Comp_End")),
            )
            error = compare_series(expected, predicted, quantity, metric)
            if error is not None:
                errors.append(error)

        if not errors:
            return ComparisonResult(
                row_number=row_number,
                dataname=dataname,
                status="SKIP",
                quantity=quantity,
                metric=metric,
                error=None,
                tolerance=tolerance,
                message="No comparable data points.",
            )

        row_error = max(errors)
        status = "PASS" if tolerance is None or row_error <= tolerance else "FAIL"
        return ComparisonResult(
            row_number=row_number,
            dataname=dataname,
            status=status,
            quantity=quantity,
            metric=metric,
            error=row_error,
            tolerance=tolerance,
            message="",
        )
    except Exception as exc:
        return ComparisonResult(
            row_number=row_number,
            dataname=dataname,
            status="FAIL",
            quantity=quantity,
            metric=metric,
            error=None,
            tolerance=tolerance,
            message=str(exc),
        )


def compare_targets(work_root: Path) -> list[ComparisonResult]:
    verification_root = work_root / "Verification"
    config_path = work_root / "Utilities" / "Python" / "CFAST_verification_dataplot_inputs.csv"
    return [
        compare_row(verification_root, row_number, row)
        for row_number, row in load_dataplot_rows(config_path)
    ]


def status_counts(items) -> dict[str, int]:
    counts: dict[str, int] = {}
    for item in items:
        counts[item.status] = counts.get(item.status, 0) + 1
    return counts


def print_summary(
    repo_root: Path,
    work_root: Path,
    case_results: list[CaseResult],
    helper_results: list[HelperResult],
    comparison_results: list[ComparisonResult],
) -> None:
    passed_cases = sum(1 for result in case_results if result.ok)
    print(f"Repository: {repo_root}")
    print(f"Work tree:  {work_root}")
    print(f"Cases:      {passed_cases}/{len(case_results)} passed")

    for result in case_results:
        try:
            rel = result.path.relative_to(repo_root)
        except ValueError:
            try:
                rel = result.path.relative_to(work_root / "Verification")
            except ValueError:
                rel = result.path
        status = "PASS" if result.ok else "FAIL"
        print(f"  {status:4} {rel} ({result.elapsed:.1f} s)")
        if not result.ok and result.message:
            print(f"       {result.message.splitlines()[-1]}")

    if helper_results:
        print("Helpers:    " + ", ".join(f"{key}={value}" for key, value in sorted(status_counts(helper_results).items())))
        for result in helper_results:
            if result.status != "PASS":
                print(f"  {result.status:4} {result.script}: {result.message}")

    if comparison_results:
        print("Targets:    " + ", ".join(f"{key}={value}" for key, value in sorted(status_counts(comparison_results).items())))
        for result in comparison_results:
            if result.status == "FAIL":
                tolerance = "" if result.tolerance is None else f" tol={result.tolerance:g}"
                error = "" if result.error is None else f" error={result.error:g}"
                print(
                    f"  FAIL row {result.row_number} {result.dataname} "
                    f"{result.quantity}/{result.metric}:{error}{tolerance} {result.message}"
                )


def check_verification_case_discovery():
    """Include nested cases such as the NRC Users Guide examples."""
    with tempfile.TemporaryDirectory(prefix="cedit-qt-discovery-") as tmp:
        root = Path(tmp)
        paths = [root / "Fires" / "fire.in", root / "NRC_Users_Guide" / "Example" / "nrc.in"]
        for path in paths:
            path.parent.mkdir(parents=True)
            path.touch()
        assert select_cases(root, []) == paths
        assert select_cases(root, ["NRC_Users_Guide/Example/nrc.in"]) == [paths[1]]
        assert select_cases(root, ["nrc"]) == [paths[1]]


def check_compartment_header_palettes():
    """Grouped headers must remain readable when the inherited palette changes."""
    from PySide6.QtCore import QRect, Qt
    from PySide6.QtGui import QColor, QImage, QPainter, QPalette
    from PySide6.QtWidgets import QStyleFactory, QTableWidget

    from tabs.compartments_tab import CompartmentSummaryHeader, summary_headers

    def luminance(color):
        channels = [color.redF(), color.greenF(), color.blueF()]
        linear = [c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4 for c in channels]
        return sum(c * weight for c, weight in zip(linear, (0.2126, 0.7152, 0.0722)))

    table = QTableWidget(0, len(summary_headers()))
    header = CompartmentSummaryHeader(Qt.Orientation.Horizontal)
    table.setHorizontalHeader(header)
    table.setHorizontalHeaderLabels(summary_headers())
    # Fusion is available on Linux and renders directly from the Qt palette.
    style = QStyleFactory.create("Fusion")
    header.setStyle(style)
    header.setDefaultSectionSize(120)
    try:
        for background, foreground in (("#efefef", "#202020"), ("#353535", "#eeeeee"),
                                       ("#efefef", "#202020")):
            palette = QPalette(table.palette())
            palette.setColor(QPalette.ColorRole.Button, QColor(background))
            palette.setColor(QPalette.ColorRole.ButtonText, QColor(foreground))
            table.setPalette(palette)
            image = QImage(header.length(), 60, QImage.Format.Format_ARGB32)
            image.fill(QColor(background))
            painter = QPainter(image)
            try:
                for column in range(table.columnCount()):
                    header.paintSection(painter, QRect(header.sectionPosition(column), 0,
                                                      header.sectionSize(column), 60), column)
            finally:
                painter.end()
            text_luminance = luminance(QColor(foreground))
            for column in range(header.surface_first_column, table.columnCount()):
                # Sample away from labels and borders in both header rows.
                for y in (3, header.group_height + 3):
                    color = image.pixelColor(header.sectionPosition(column) + 5, y)
                    background_luminance = luminance(color)
                    contrast = ((max(text_luminance, background_luminance) + 0.05)
                                / (min(text_luminance, background_luminance) + 0.05))
                    assert contrast >= 4.5, (background, column, y, contrast)
    finally:
        header.setStyle(None)
        table.deleteLater()


def check_target_material_initialization():
    """Regression for #2458: no demo or implicit default target material."""
    from cfast_case import Target
    from main_window import default_concrete_material, opening_case

    window = CeditMainWindow()
    tab = window.targets_tab
    try:
        assert not tab.targets
        assert tab.material_combo.currentText() == "OFF"
        assert tab.temperature_depth_label.text() == "Internal Temperature at"
        assert tab.depth_units_combo.currentText() == "Fraction"
        assert tab.temperature_depth_edit.text() == ""
        assert tab.material_ids == ["OFF"]
        assert tab.thickness_edit.text() == ""
        assert tab.conductivity_label.text().strip() == "Conductivity:"

        from dataclasses import replace
        from units import CONDUCTIVITY, DENSITY, LENGTH, SPECIFIC_HEAT, format_value

        material = replace(default_concrete_material(), id="NM 1", conductivity=0.16,
                           specific_heat=0.9, density=790.0, thickness=0.016)
        tab.set_material_ids([material.id])
        tab.set_material_properties([material])
        tab.material_combo.setCurrentText("NM 1")
        assert not tab.targets
        assert format_value(CONDUCTIVITY, material.conductivity, include_unit=False) in tab.conductivity_label.text()
        assert format_value(SPECIFIC_HEAT, material.specific_heat, include_unit=False) in tab.specific_heat_label.text()
        assert format_value(DENSITY, material.density, include_unit=False) in tab.density_label.text()
        assert tab.thickness_edit.text() == format_value(LENGTH, material.thickness)
        tab.set_material_properties([replace(material, thickness=0.02)])
        assert tab.thickness_edit.text() == format_value(LENGTH, 0.02)
        tab.add_target()
        assert tab.targets[0].matl_id == "NM 1"
        assert tab.effective_thickness(tab.targets[0]) == 0.02
        window.load_case(opening_case())

        tab.add_target()
        assert tab.targets[0].matl_id == "OFF"
        assert tab.summary_table.item(0, 9).text() == "OFF"
        assert tab.material_combo.currentText() == "OFF"

        case = opening_case()
        case.materials = [default_concrete_material()]
        case.targets = [Target(id="Target 1", comp_id="Comp 1", matl_id="CONCRETE")]
        window.load_case(case)
        assert tab.material_ids == ["OFF", "CONCRETE"]
        assert tab.material_combo.currentText() == "CONCRETE"
        assert tab.effective_thickness(tab.targets[0]) == 0.15
        tab.add_target()
        assert tab.targets[-1].matl_id == "CONCRETE"

        window.load_case(case)
        tab.remove_target()
        assert tab.material_combo.currentText() == "OFF"
        assert tab.thickness_edit.text() == ""
        assert tab.density_label.text().strip() == "Density:"
        assert tab.temperature_depth_label.text() == "Internal Temperature at"
        assert tab.depth_units_combo.currentText() == "Fraction"
        assert tab.temperature_depth_edit.text() == ""

        window.load_case(opening_case())
        assert tab.material_ids == ["OFF"]
        assert tab.material_combo.currentText() == "OFF"
        tab.add_target()
        case = opening_case()
        tab.add_to_case(case)
        assert case.targets[0].matl_id == "OFF"
    finally:
        window.deleteLater()


def check_target_temperature_depth():
    from dataclasses import replace

    from cfast_case import Target
    from cfast_reader import parse_namelists, read_cfast_input
    from cfast_writer import validate_case
    from main_window import default_concrete_material, opening_case
    from units import LENGTH, format_value, unit_system

    original_units = dict(unit_system.selected)
    window = CeditMainWindow()
    tab = window.targets_tab
    try:
        unit_system.set_indices({key: 0 for key in original_units})
        case = opening_case()
        material = replace(default_concrete_material(), thickness=0.016)
        case.materials = [material]
        case.targets = [Target(id="Target 1", comp_id=case.compartments[0].id, matl_id=material.id)]
        window.load_case(case)
        assert tab.depth_units_combo.currentText() == "Fraction"
        assert tab.temperature_depth_edit.text() == "0.5"
        tab.depth_units_combo.setCurrentText("Depth")
        assert tab.temperature_depth_edit.text() == "0.008 m"
        assert tab.targets[0].depth_units == "METERS"
        tab.depth_units_combo.setCurrentText("Fraction")
        assert tab.temperature_depth_edit.text() == "0.5"

        with tempfile.TemporaryDirectory(prefix="cedit-target-depth-") as tmp:
            path = Path(tmp) / "target.in"
            for mode, values, units in (
                ("Fraction", ("0", "0.5", "1"), "FRACTION"),
                ("Depth", ("0 m", "8 mm", "0.016 m"), "METERS"),
            ):
                tab.depth_units_combo.setCurrentText(mode)
                for value in values:
                    tab.temperature_depth_edit.setText(value)
                    window.update_live_validation()
                    assert window.statusBar().currentMessage() == "No Errors"
                    expected = tab.temperature_depth_from_editor()
                    assert window.write_case_to_path(path) == path
                    fields = next(record.fields for record in parse_namelists(path.read_text())
                                  if record.name == "DEVC")
                    assert fields["DEPTH_UNITS"] == [units]
                    assert math.isclose(fields["TEMPERATURE_DEPTH"][0], expected, abs_tol=1e-12)
                    window.load_cfast_input(path)
                    assert tab.depth_units_combo.currentText() == mode
                    assert math.isclose(read_cfast_input(path).targets[0].temperature_depth, expected, abs_tol=1e-12)

            for mode, invalid in (
                ("Fraction", ("-0.1", "1.1", "0.5 m", "nan", "inf", "bad")),
                ("Depth", ("-0.001 m", "0.017 m", "bad")),
            ):
                for value in invalid:
                    window.load_case(case)
                    tab.depth_units_combo.setCurrentText(mode)
                    tab.temperature_depth_edit.setText(value)
                    window.update_live_validation()
                    assert window.statusBar().currentMessage().startswith("Error:")
                    previous = path.read_text()
                    assert window.write_case_to_path(path) is None
                    assert path.read_text() == previous

            window.load_case(case)
            tab.depth_units_combo.setCurrentText("Depth")
            tab.thickness_edit.setText("0.004 m")
            window.update_live_validation()
            assert "exceeds target thickness" in window.statusBar().currentMessage()
            tab.temperature_depth_edit.setText("0.004 m")
            window.update_live_validation()
            assert window.statusBar().currentMessage() == "No Errors"

        # The model uses zero thickness to inherit the material's thickness.
        case.targets[0] = replace(case.targets[0], depth_units="METERS", temperature_depth=0.017)
        try:
            validate_case(case)
        except ValueError as exc:
            assert "exceeds target thickness" in str(exc)
        else:
            raise AssertionError("Inherited material thickness must constrain depth")

        case.targets[0] = replace(case.targets[0], temperature_depth=0.008)
        unit_system.set_indices({**original_units, "length": 3})
        window.load_case(case)
        assert tab.temperature_depth_edit.text() == format_value(LENGTH, 0.008)
        tab.depth_units_combo.setCurrentText("Fraction")
        assert math.isclose(float(tab.temperature_depth_edit.text()), 0.5, rel_tol=1e-8)
        tab.depth_units_combo.setCurrentText("Depth")
        assert math.isclose(window.build_cfast_case().targets[0].temperature_depth, 0.008, rel_tol=1e-8)
        tab.temperature_depth_edit.setText("0.016 m")
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"
        tab.depth_units_combo.setCurrentText("Fraction")
        assert tab.temperature_depth_edit.text() == "1"
        tab.depth_units_combo.setCurrentText("Depth")
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_target_material_validation():
    """Regression for #2458: OFF targets report an error before saving/running."""
    from PySide6.QtTest import QTest

    from main_window import default_concrete_material, opening_case

    window = CeditMainWindow()
    tab = window.targets_tab
    try:
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"
        tab.add_target()
        assert window.live_validation_timer.isActive()
        QTest.qWait(250)
        assert "OFF is not a valid target material" in window.statusBar().currentMessage()
        assert "#b00020" in window.statusBar().styleSheet()

        with tempfile.TemporaryDirectory(prefix="cedit-target-material-") as directory:
            path = Path(directory) / "target.in"
            assert window.write_case_to_path(path) is None
            assert not path.exists()

        case = opening_case()
        material = default_concrete_material()
        case.materials = [material]
        case.targets = list(tab.targets)
        window.load_case(case)
        tab.material_combo.setCurrentText(material.id)
        QTest.qWait(250)
        assert window.statusBar().currentMessage() == "No Errors"
        assert window.statusBar().styleSheet() == ""

        tab.material_combo.setCurrentText("OFF")
        QTest.qWait(250)
        assert "OFF is not a valid target material" in window.statusBar().currentMessage()
        tab.remove_target()
        QTest.qWait(250)
        assert window.statusBar().currentMessage() == "No Errors"
    finally:
        window.deleteLater()


def check_material_id_length():
    """Material IDs follow the CFAST namelist length of 64 characters."""
    from dataclasses import replace

    from cfast_reader import read_cfast_input
    from cfast_writer import validate_case, write_cfast_input
    from main_window import default_concrete_material, opening_case

    case = opening_case()
    case.materials = [replace(default_concrete_material(), id="M" * 64)]
    with tempfile.TemporaryDirectory(prefix="cedit-material-id-") as directory:
        path = Path(directory) / "material_id.in"
        write_cfast_input(case, path)
        assert read_cfast_input(path).materials[0].id == "M" * 64

    case.materials[0].id = "M" * 65
    try:
        validate_case(case)
    except ValueError as exc:
        assert "1 to 64 characters" in str(exc)
    else:
        raise AssertionError("A material ID longer than 64 characters was accepted")


def check_target_orientation():
    """Issue #2504: select one orientation and write only the chosen normal form."""
    from dataclasses import replace
    from PySide6.QtCore import Qt
    from PySide6.QtTest import QTest
    from cfast_case import FireDefinition, FireProperty, FireRampPoint, Target
    from cfast_reader import parse_namelists, read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import default_concrete_material, opening_case

    case = opening_case()
    room = case.compartments[0]
    case.compartments.append(replace(room, id="Other room"))
    case.materials = [default_concrete_material()]
    case.targets = [Target(id="Target", comp_id=room.id, matl_id=case.materials[0].id)]
    case.fire_properties = [FireProperty(id="Fuel", ramp=[FireRampPoint(0, 0), FireRampPoint(60, 100)])]
    case.fires = [FireDefinition("Local fire", room.id, "Fuel"), FireDefinition("Other fire", "Other room", "Fuel")]
    for fire in case.fires:
        fire.x_position, fire.y_position = room.width / 2, room.depth / 2
    window = CeditMainWindow()
    tab = window.targets_tab
    try:
        window.load_case(case)
        window.tabs.setCurrentWidget(tab)
        window.show()
        combo = tab.surface_orientation_combo
        assert tab.normal_mode_combo.currentText() == "Specify Normal"
        assert tab.nx_edit.isEnabled() and not combo.isEnabled()
        assert combo.findText("Local fire") >= 0 and combo.findText("Other fire") == -1
        tab.normal_mode_combo.setCurrentText("Specify Direction")
        assert combo.isEnabled() and not tab.nx_edit.isEnabled()
        assert not combo.isHidden() and tab.nx_edit.isHidden()
        assert not tab.surface_orientation_label.isHidden() and tab.nx_label.isHidden()
        with tempfile.TemporaryDirectory(prefix="cedit-target-orientation-") as directory:
            path = Path(directory) / "target.in"
            for direction in ("CEILING", "FLOOR", "LEFT WALL", "RIGHT WALL", "FRONT WALL", "BACK WALL", "Local fire"):
                combo.showPopup()
                QTest.qWait(10)
                index = combo.model().index(combo.findText(direction), 0)
                combo.view().setCurrentIndex(index)
                window.schedule_live_validation()
                QTest.qWait(250)
                assert combo.view().currentIndex() == index
                QTest.mouseClick(combo.view().viewport(), Qt.LeftButton, pos=combo.view().visualRect(index).center())
                write_cfast_input(window.build_cfast_case(), path)
                fields = next(record.fields for record in parse_namelists(path.read_text()) if record.name == "DEVC")
                assert fields["SURFACE_ORIENTATION"] == [direction] and "NORMAL" not in fields
                window.load_case(read_cfast_input(path))
                assert combo.currentText() == direction and combo.isEnabled()
            tab.compartment_combo.setCurrentText("Other room")
            assert combo.findText("Local fire") == -1 and combo.findText("Other fire") >= 0
            assert combo.currentIndex() == -1
            try:
                window.build_cfast_case()
            except ValueError as exc:
                assert "select a surface orientation" in str(exc)
            else:
                raise AssertionError("Changing compartments must not silently retain an unavailable fire")
            combo.setCurrentText("Other fire")
            assert window.build_cfast_case().targets[0].surface_orientation == "Other fire"
            tab.normal_mode_combo.setCurrentText("Specify Normal")
            tab.nx_edit.setText("1")
            tab.nz_edit.setText("0")
            write_cfast_input(window.build_cfast_case(), path)
            fields = next(record.fields for record in parse_namelists(path.read_text()) if record.name == "DEVC")
            assert fields["NORMAL"] == [1, 0, 0] and "SURFACE_ORIENTATION" not in fields
            window.load_case(read_cfast_input(path))
            assert tab.nx_edit.isEnabled() and not combo.isEnabled()
    finally:
        window.deleteLater()


def check_adiabatic_target():
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import default_concrete_material, opening_case

    window = CeditMainWindow()
    tab = window.targets_tab
    try:
        tab.adiabatic_checkbox.setChecked(True)
        assert window.live_validation_timer.isActive()
        window.update_live_validation()
        assert "Adiabatic target" in window.statusBar().currentMessage()
        assert "#b00020" in window.statusBar().styleSheet()
        tab.add_target()
        assert tab.targets[0].adiabatic
        window.update_live_validation()
        assert "Adiabatic target" in window.statusBar().currentMessage()

        material = default_concrete_material()
        tab.set_material_ids([material.id])
        tab.set_material_properties([material])
        case = opening_case()
        case.materials = [material]
        case.targets = tab.targets
        window.load_case(case)
        tab.material_combo.setCurrentText(material.id)
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"
        assert window.statusBar().styleSheet() == ""
        case = window.build_cfast_case()
        assert case.targets[0].adiabatic
        assert case.targets[0].matl_id == material.id
        with tempfile.TemporaryDirectory(prefix="cedit-adiabatic-") as directory:
            path = Path(directory) / "target.in"
            write_cfast_input(case, path)
            loaded = read_cfast_input(path)
            assert loaded.targets[0].adiabatic
            assert loaded.targets[0].matl_id == material.id
            assert loaded.materials[0].emissivity == material.emissivity
            window.load_case(loaded)
            assert tab.adiabatic_checkbox.isChecked()

        for missing in ("OFF", "DEFAULT", "Missing", ""):
            tab.material_combo.setCurrentText(missing)
            window.update_live_validation()
            assert "Adiabatic target" in window.statusBar().currentMessage()
            assert "#b00020" in window.statusBar().styleSheet()
        tab.material_combo.setCurrentText("OFF")
        tab.adiabatic_checkbox.setChecked(False)
        window.update_live_validation()
        assert "OFF is not a valid target material" in window.statusBar().currentMessage()
        assert not tab.targets[0].adiabatic
        tab.material_combo.setCurrentText(material.id)
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"
    finally:
        window.deleteLater()


def check_adiabatic_convection_coefficients():
    """Regression for #2503: inline fields follow engineering units and preserve input units."""
    from cfast_case import Target
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import default_concrete_material, opening_case
    from units import ENERGY, LENGTH, TEMPERATURE, TIME, unit_system

    window = CeditMainWindow()
    original_units = dict(unit_system.selected)
    tab = window.targets_tab
    case = opening_case()
    material = default_concrete_material()
    case.materials = [material]
    case.targets = [Target(id="Target 1", comp_id="Comp 1", matl_id=material.id)]
    scenarios = [
        ({ENERGY: 0, TIME: 0, LENGTH: 0, TEMPERATURE: 0}, 10.0, "W/(m² °C)"),
        ({ENERGY: 1, TIME: 0, LENGTH: 0, TEMPERATURE: 0}, 10.0, "W/(m² °C)"),
        ({ENERGY: 3, TIME: 2, LENGTH: 3, TEMPERATURE: 2},
         10.0 * 3600.0 * 0.3048 ** 2 * (5.0 / 9.0) / 1054.35, "BTU/(h ft² °F)"),
    ]
    try:
        for indices, expected, label in scenarios:
            unit_system.set_indices(indices)
            window.load_case(case)
            assert tab.convection_group.isHidden()
            tab.adiabatic_checkbox.click()
            assert not tab.convection_group.isHidden()
            assert QApplication.activeModalWidget() is None
            assert label in tab.convection_front_label.text()
            assert label in tab.convection_back_label.text()
            assert math.isclose(float(tab.convection_front_edit.text()), expected, rel_tol=1e-9)
            assert math.isclose(float(tab.convection_back_edit.text()), expected, rel_tol=1e-9)
            # Bare numbers use the selected units; explicit units also work.
            tab.convection_front_edit.setText(str(2.0 * expected))
            assert math.isclose(tab.targets[0].convection_coefficient_front, 0.02, rel_tol=1e-9)
            tab.convection_front_edit.setText("12.5 W/(m2 C)")
            tab.convection_back_edit.setText("7 W/(m2 C)")
            with tempfile.TemporaryDirectory(prefix="cedit-convection-") as directory:
                path = Path(directory) / "target.in"
                write_cfast_input(window.build_cfast_case(), path)
                assert "CONVECTION_COEFFICIENTS = 0.0125, 0.007" in path.read_text()
                window.load_case(read_cfast_input(path))
                assert math.isclose(float(tab.convection_front_edit.text()), expected * 1.25, rel_tol=1e-9)
                assert math.isclose(float(tab.convection_back_edit.text()), expected * 0.7, rel_tol=1e-9)
                # Switching unit selections must retain the physical coefficients.
                saved = window.build_cfast_case()
                unit_system.set_indices(scenarios[1][0])
                window.load_case(saved)
                assert math.isclose(float(tab.convection_front_edit.text()), 12.5, rel_tol=1e-9)
                assert math.isclose(float(tab.convection_back_edit.text()), 7.0, rel_tol=1e-9)
                tab.adiabatic_checkbox.click()
                assert tab.convection_group.isHidden()
                write_cfast_input(window.build_cfast_case(), path)
                assert "ADIABATIC_TARGET" not in path.read_text()
                assert "CONVECTION_COEFFICIENTS" not in path.read_text()
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_mechanical_vent_defaults():
    """Regression for #2467: newly added diffusers fit their compartment."""
    from PySide6.QtCore import QSignalBlocker

    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case
    from units import LENGTH, format_value, unit_system

    window = CeditMainWindow()
    tab = window.mechanical_vents_tab
    original_units = dict(unit_system.selected)
    try:
        for length_unit in (0, 3):  # metres and feet
            unit_system.set_index(LENGTH, length_unit)
            for height, width, depth in ((2.4, 5.0, 2.4), (1.0, 3.0, 1.8), (4.0, 7.0, 6.0)):
                case = opening_case()
                case.compartments[0].height = height
                case.compartments[0].width = width
                case.compartments[0].depth = depth
                window.load_case(case)
                tab.add_vent()
                vent = tab.vents[-1]
                assert vent.from_comp_id == "OUTSIDE"
                assert vent.to_comp_id == case.compartments[0].id
                assert math.isclose(vent.from_height, height / 2.0)
                assert math.isclose(vent.to_height, height / 2.0)
                assert tab.to_height_edit.text() == format_value(LENGTH, height / 2.0)
                assert math.isclose(vent.offset_x, 0.0)
                assert math.isclose(vent.offset_y, depth / 2.0)
                assert tab.offset_y_edit.text() == format_value(LENGTH, depth / 2.0)
                tab.from_orientation_combo.setCurrentText("Horizontal")
                tab.to_orientation_combo.setCurrentText("Horizontal")
                assert tab.offset_x_edit.text() == format_value(LENGTH, width / 2.0)
                assert tab.offset_y_edit.text() == format_value(LENGTH, depth / 2.0)
                # Editing the summary must apply the same defaults as the detail controls.
                with QSignalBlocker(tab.summary_table):
                    item = tab.summary_table.item(0, 5)
                    item.setText("Vertical")
                tab.summary_item_changed(item)
                assert tab.offset_x_edit.text() == format_value(LENGTH, 0.0)
                assert tab.offset_y_edit.text() == format_value(LENGTH, depth / 2.0)
                window.update_live_validation()
                assert window.statusBar().currentMessage() == "No Errors"

        unit_system.set_index(LENGTH, 0)
        window.load_case(opening_case())
        window.tabs.setCurrentWidget(window.compartments_tab)
        window.compartments_tab.height_edit.setText("1.6 m")
        window.compartments_tab.width_edit.setText("6 m")
        window.compartments_tab.depth_edit.setText("2.4 m")
        window.compartments_tab.save_detail_to_selected()
        window.tabs.setCurrentWidget(tab)
        tab.add_vent()
        assert math.isclose(tab.vents[-1].to_height, 0.8)
        assert math.isclose(tab.vents[-1].offset_y, 1.2)
        tab.from_orientation_combo.setCurrentText("Horizontal")
        tab.to_orientation_combo.setCurrentText("Horizontal")
        with tempfile.TemporaryDirectory(prefix="cedit-centered-vent-") as directory:
            path = Path(directory) / "mechanical.in"
            write_cfast_input(window.build_cfast_case(), path)
            vent = read_cfast_input(path).mechanical_vents[0]
            assert math.isclose(vent.offset_x, 3.0)
            assert math.isclose(vent.offset_y, 1.2)
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"

        # Orientation changes, loading, duplicating, and writing preserve custom offsets.
        tab.from_height_edit.setText("0.6 m")
        tab.to_height_edit.setText("0.7 m")
        tab.offset_x_edit.setText("0.3 m")
        tab.offset_y_edit.setText("0.4 m")
        tab.store_current_vent()
        tab.from_orientation_combo.setCurrentText("Vertical")
        tab.to_orientation_combo.setCurrentText("Vertical")
        case = window.build_cfast_case()
        with tempfile.TemporaryDirectory(prefix="cedit-mechanical-defaults-") as directory:
            path = Path(directory) / "mechanical.in"
            write_cfast_input(case, path)
            window.load_case(read_cfast_input(path))
            window.refresh_reference_lists()
            tab.duplicate_vent()
            for vent in tab.vents:
                assert math.isclose(vent.from_height, 0.6)
                assert math.isclose(vent.to_height, 0.7)
                assert math.isclose(vent.offset_x, 0.3)
                assert math.isclose(vent.offset_y, 0.4)
        window.update_live_validation()
        assert window.statusBar().currentMessage() == "No Errors"

        # Explicitly invalid heights must still be rejected.
        tab.to_height_edit.setText("2.75 m")
        tab.store_current_vent()
        window.update_live_validation()
        assert "diffuser extends" in window.statusBar().currentMessage()
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_detection_device_parameters():
    from PySide6.QtCore import Qt

    from cfast_case import DetectionDevice
    from cfast_reader import detection_device_from_fields, parse_namelists, read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case
    from units import SMOKE, TEMPERATURE, format_value, unit_system

    window = CeditMainWindow()
    tab = window.detection_suppression_tab
    original_units = dict(unit_system.selected)
    try:
        for temperature_unit in (0, 1):
            unit_system.set_index(TEMPERATURE, temperature_unit)
            case = opening_case()
            window.load_case(case)
            for display_type in ("Sprinkler", "Heat", "Smoke"):
                tab.add_device()
                tab.type_combo.setCurrentText(display_type)
                device = tab.selected_device()
                assert device.activation_temperature == 57
                assert device.activation_obscuration == 24
                assert device.rti == 50
                assert tab.activation_temperature_edit.isEnabled() == (display_type != "Smoke")
                assert tab.activation_obscuration_edit.isEnabled() == (display_type == "Smoke")
                assert tab.rti_edit.isEnabled() == (display_type != "Smoke")
                assert tab.spray_density_edit.isEnabled() == (display_type == "Sprinkler")
                for name, visible in (("activation_temperature", display_type != "Smoke"),
                                      ("activation_obscuration", display_type == "Smoke"),
                                      ("rti", display_type != "Smoke"),
                                      ("spray_density", display_type == "Sprinkler")):
                    assert getattr(tab, f"{name}_edit").isHidden() == (not visible)
                    assert getattr(tab, f"{name}_label").isHidden() == (not visible)

            # Rebuilding and updating rows must both leave irrelevant cells blank and locked.
            tab.rebuild_summary_table()
            for row in range(3):
                tab.update_summary_row(row)
                for col, relevant in ((tab.COL_RTI, row != 2), (tab.COL_SPRAY_DENSITY, row == 0)):
                    item = tab.summary_table.item(row, col)
                    assert bool(item.text()) == relevant
                    assert bool(item.flags() & Qt.ItemFlag.ItemIsEditable) == relevant
                if row == 2:
                    assert tab.table_text(row, tab.COL_ACTIVATION) == format_value(SMOKE, 24)

            # Type edits in the table must not reinterpret temperature as obscuration.
            tab.select_device(0)
            tab.activation_temperature_edit.setText("80 C")
            tab.summary_table.item(0, tab.COL_TYPE).setText("Smoke")
            assert tab.devices[0].activation_obscuration == 24
            assert not tab.rti_edit.isEnabled()
            tab.summary_table.item(0, tab.COL_ACTIVATION).setText("31 %/m")
            tab.summary_table.item(0, tab.COL_TYPE).setText("Heat")
            assert tab.devices[0].activation_temperature == 80
            assert tab.devices[0].activation_obscuration == 31
            assert tab.table_text(0, tab.COL_SPRAY_DENSITY) == ""
            tab.summary_table.item(0, tab.COL_RTI).setText("65")
            assert tab.devices[0].rti == 65
            tab.type_combo.setCurrentText("Sprinkler")
            tab.spray_density_edit.setText("0.0002 m/s")
            tab.select_device(1)
            tab.activation_temperature_edit.setText("90 C")
            tab.select_device(2)
            tab.activation_obscuration_edit.setText("30 %/m")

            # Hidden parameters must not be parsed, validated, or written for smoke devices.
            tab.rti_edit.setText("invalid")
            tab.spray_density_edit.setText("invalid")
            tab.devices[2].rti = -1
            tab.devices[2].spray_density = -1
            saved_case = window.build_cfast_case()
            with tempfile.TemporaryDirectory(prefix="cedit-detectors-") as directory:
                path = Path(directory) / "devices.in"
                write_cfast_input(saved_case, path)
                records = [record.fields for record in parse_namelists(path.read_text())
                           if record.name == "DEVC"]
                assert len(records) == 3
                assert [record["SETPOINT"] for record in records] == [[80], [90], [30]]
                assert records[0]["RTI"] == [65]
                assert records[0]["SPRAY_DENSITY"] == [0.0002]
                assert records[1]["RTI"] == [50]
                assert "SPRAY_DENSITY" not in records[1]
                assert "RTI" not in records[2] and "SPRAY_DENSITY" not in records[2]
                reopened = read_cfast_input(path).detection_devices
                assert reopened[0].activation_temperature == 80
                assert reopened[1].activation_temperature == 90
                assert reopened[2].activation_obscuration == 30

        # Omitted namelist values use the same defaults as newly added devices.
        for device_type in ("SPRINKLER", "HEAT_DETECTOR", "SMOKE_DETECTOR"):
            fields = {"ID": ["Device"], "COMP_ID": ["Comp 1"]}
            device = detection_device_from_fields(fields, device_type)
            assert device.activation_temperature == 57
            assert device.activation_obscuration == 24
            assert device.rti == 50
            fields.update({"SETPOINT": [42], "RTI": [75]})
            device = detection_device_from_fields(fields, device_type)
            assert device.rti == 75
            assert (device.activation_obscuration if device_type == "SMOKE_DETECTOR"
                    else device.activation_temperature) == 42
        assert DetectionDevice("Default", "Comp 1").rti == 50
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_engineering_units_editor():
    from unittest.mock import patch
    from PySide6.QtCore import QSettings, Qt
    from PySide6.QtTest import QTest
    from PySide6.QtWidgets import QDialogButtonBox, QStyleOptionViewItem, QTableWidget
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case, RunMonitorDialog
    from units import LENGTH, PRESSURE, PRESSURE_DIFFERENCE, format_value, parse_value, unit_system

    window = CeditMainWindow()
    original_units = dict(unit_system.selected)
    tab = window.detection_suppression_tab
    try:
        unit_system.set_indices({key: 0 for key in original_units})
        window.load_case(opening_case())
        tab.add_device()
        window.show()
        window.tabs.setCurrentWidget(tab)
        QApplication.processEvents()
        # Enter units one keystroke at a time, then commit by moving focus.
        for edit, attr in ((tab.x_edit, "x_position"), (tab.y_edit, "y_position"),
                           (tab.z_edit, "z_position")):
            edit.setFocus()
            edit.selectAll()
            QTest.keyClicks(edit, "5 ft")
            QTest.keyClick(edit, Qt.Key.Key_Tab)
            assert edit.text() == "1.524 m", edit.text()
            assert math.isclose(getattr(tab.selected_device(), attr), 1.524)

        def displayed(table, row, col):
            option = QStyleOptionViewItem()
            table.itemDelegate().initStyleOption(option, table.model().index(row, col))
            return option.text

        assert displayed(tab.summary_table, 0, tab.COL_X) == "1.524"
        assert displayed(tab.summary_table, 0, tab.COL_RTI) == "50"
        assert displayed(tab.summary_table, 0, tab.COL_ACTIVATION) == "57.00 °C"
        tab.type_combo.setCurrentText("Smoke")
        assert displayed(tab.summary_table, 0, tab.COL_ACTIVATION) == "24 %/m"
        assert tab.response_group.isHidden()
        assert not tab.fyi_edit.isHidden()
        # Table edits in other units still convert and normalize correctly.
        tab.summary_table.item(0, tab.COL_X).setText("10 ft")
        assert math.isclose(tab.selected_device().x_position, 3.048)
        assert displayed(tab.summary_table, 0, tab.COL_X) == "3.048"

        with tempfile.TemporaryDirectory(prefix="cedit-units-editor-") as directory:
            window.settings = QSettings(str(Path(directory) / "settings.ini"), QSettings.Format.IniFormat)
            window.select_engineering_units()
            dialog = window.engineering_units_dialog
            assert not dialog.isModal() and QApplication.activeModalWidget() is None
            window.select_engineering_units()
            assert window.engineering_units_dialog is dialog
            apply = dialog.findChild(QDialogButtonBox).button(QDialogButtonBox.StandardButton.Apply)
            # Edits made after opening the dialog must survive Apply.
            tab.x_edit.setText("7 ft")
            dialog.combos[LENGTH].setCurrentText("ft")
            apply.click()
            assert dialog.isVisible()
            assert tab.x_edit.text() == "7 ft"
            assert displayed(tab.summary_table, 0, tab.COL_X) == "7"
            assert math.isclose(window.build_cfast_case().detection_devices[0].x_position, 2.1336)
            # Presets replace every selection and apply immediately without closing.
            for combo in dialog.combos.values():
                combo.setCurrentIndex(combo.count() - 1)
            for button, labels, position in (
                (dialog.english_button,
                 {"length": "ft", "mass": "lbm", "time": "s", "temperature": "°F",
                  "pressure": "psia", "energy": "BTU", "smoke": "%/ft"}, "7 ft"),
                (dialog.si_button,
                 {"length": "m", "mass": "kg", "time": "s", "temperature": "°C",
                  "pressure": "Pa", "energy": "kJ", "smoke": "%/m"}, "2.1336 m"),
            ):
                button.click()
                assert dialog.isVisible() and QApplication.activeModalWidget() is None
                assert {key: combo.currentText() for key, combo in dialog.combos.items()} == labels
                assert {key: unit_system.choices(key)[unit_system.selected[key]] for key in labels} == labels
                assert tab.x_edit.text() == position
                fraction_unit = f"{labels['mass']}/{labels['mass']}"
                assert window.simulation_tab.interior_o2_mass_fraction_edit.text() == f"0.23 {fraction_unit}"
                assert window.simulation_tab.exterior_o2_mass_fraction_edit.text() == f"0.23 {fraction_unit}"
                saved = window.build_cfast_case()
                assert math.isclose(saved.detection_devices[0].x_position, 2.1336)
                assert math.isclose(saved.detection_devices[0].activation_obscuration, 24.0)
                assert math.isclose(saved.pressure, 101325.0, abs_tol=1e-4)
                assert math.isclose(saved.interior_temperature, 20.0)
                for key, index in unit_system.selected.items():
                    assert window.settings.value(f"units/{key}", type=int) == index
            # Loading a new case while the dialog is open must not restore the old case.
            case = opening_case()
            case.title = "Loaded while units are open"
            case.compartments[0].width = 8.0
            window.load_case(case)
            for label, expected in (("psi", 14.7), ("psia", 14.7), ("psig", 0.0)):
                dialog.combos[PRESSURE].setCurrentText(label)
                apply.click()
                assert math.isclose(float(window.simulation_tab.pressure_edit.text().split()[0]),
                                    expected, abs_tol=1e-8)
                assert window.build_cfast_case().title == case.title
                assert math.isclose(window.build_cfast_case().pressure, 101325.0, abs_tol=1e-4)
                path = Path(directory) / "pressure.in"
                write_cfast_input(window.build_cfast_case(), path)
                assert math.isclose(read_cfast_input(path).pressure, 101325.0, abs_tol=1e-3)
                assert format_value(PRESSURE_DIFFERENCE, 0.0) == "0 psi"
            monitor = RunMonitorDialog(window, "Units", 60)
            assert "(psi)" in monitor.summary_headers()[-1]
            assert monitor.format_status_value(PRESSURE_DIFFERENCE, "0") == "0"
            monitor.deleteLater()
            assert math.isclose(parse_value(PRESSURE, "1 psia", "Pressure"), 6894.757293168)
            assert math.isclose(parse_value(PRESSURE, "0 psig", "Pressure"), 101325.0)
            assert math.isclose(parse_value(PRESSURE, "-1 psig", "Pressure"), 94430.242706832)
            assert math.isclose(parse_value(PRESSURE_DIFFERENCE, "1 psig", "Pressure difference"),
                                6894.757293168)
            window.mechanical_vents_tab.add_vent()
            vent_tab = window.mechanical_vents_tab
            vent_tab.begin_dropoff_edit.setText("1 psig")
            vent_tab.zero_flow_edit.setText("2 psi")
            saved = window.build_cfast_case().mechanical_vents[0]
            assert math.isclose(saved.begin_dropoff, 6894.757293168)
            assert math.isclose(saved.zero_flow, 13789.514586336)
            # Invalid edits must leave both the case and selected units available for repair.
            window.simulation_tab.pressure_edit.setText("1 invalid")
            previous = dict(unit_system.selected)
            dialog.combos[PRESSURE].setCurrentText("Pa")
            with patch.object(QMessageBox, "critical") as critical:
                apply.click()
                critical.assert_called_once()
            assert unit_system.selected == previous and dialog.isVisible()
            with patch.object(QMessageBox, "critical") as critical:
                dialog.si_button.click()
                critical.assert_called_once()
            assert unit_system.selected == previous and dialog.isVisible()
            window.simulation_tab.pressure_edit.setText("0 psig")
            dialog.close()
            window.select_engineering_units()
            assert dialog.combos[PRESSURE].currentText() == "psig"
            dialog.close()
        # Check both summary and detail tables, including compound units.
        units_dir = Path(__file__).resolve().parents[2] / "Verification" / "Units"
        checked = 0
        for length_index in (0, 3):
            unit_system.set_indices({**{key: 0 for key in original_units}, LENGTH: length_index})
            for name in ("units_basic", "units_vents_devices", "units_fire"):
                window.load_case(read_cfast_input(units_dir / f"{name}.in"))
                for table in window.findChildren(QTableWidget):
                    for col in range(table.columnCount()):
                        header = table.horizontalHeaderItem(col)
                        if header is None:
                            continue
                        for row in range(table.rowCount()):
                            item = table.item(row, col)
                            if item is None:
                                continue
                            number, _, suffix = item.text().partition(" ")
                            if suffix and header.text().endswith(f"({suffix})"):
                                assert math.isclose(float(displayed(table, row, col)), float(number), rel_tol=5e-5)
                                checked += 1
        assert checked > 100, checked
    finally:
        unit_system.set_indices(original_units)
        window.hide()
        window.deleteLater()


def check_quantity_display_precision():
    from dataclasses import asdict
    from PySide6.QtCore import Qt
    from PySide6.QtTest import QTest
    from PySide6.QtWidgets import QStyleOptionViewItem
    from cfast_reader import read_cfast_input
    from main_window import EngineeringUnitsDialog
    from table_widgets import UnitLineEdit
    from units import unit_system

    def same_values(expected, actual, path="case"):
        if isinstance(expected, dict):
            assert expected.keys() == actual.keys(), path
            for key in expected:
                same_values(expected[key], actual[key], f"{path}.{key}")
        elif isinstance(expected, (list, tuple)):
            assert len(expected) == len(actual), path
            for index, (left, right) in enumerate(zip(expected, actual)):
                same_values(left, right, f"{path}[{index}]")
        elif isinstance(expected, float):
            assert math.isclose(expected, actual, rel_tol=1e-8, abs_tol=1e-9), (path, expected, actual)
        else:
            assert expected == actual, (path, expected, actual)

    window = CeditMainWindow()
    original_units = dict(unit_system.selected)
    try:
        units_dir = Path(__file__).resolve().parents[2] / "Verification/Units"
        compact_fields = 0
        for name in ("units_basic", "units_vents_devices", "units_fire"):
            unit_system.set_indices({key: 0 for key in original_units})
            window.load_case(read_cfast_input(units_dir / f"{name}.in"))
            baseline = asdict(window.build_cfast_case())
            for preset in ("English", "SI", "English"):
                case = window.build_cfast_case()
                unit_system.set_indices({key: unit_system.choices(key).index(label)
                                         for key, label in EngineeringUnitsDialog.PRESETS[preset].items()})
                window.load_case(case)
                for edit in window.findChildren(UnitLineEdit):
                    if edit.displayText() != edit.text():
                        compact_fields += 1
                        number = edit.displayText().split()[0]
                        digits = number.lower().split("e")[0].lstrip("+-").replace(".", "").lstrip("0")
                        assert len(digits) <= 5, edit.displayText()
                same_values(baseline, asdict(window.build_cfast_case()))
        assert compact_fields > 20, compact_fields

        tab = window.mechanical_vents_tab
        tab.add_vent()
        tab.from_area_edit.setText("2.690977604 ft²")
        tab.from_height_edit.setText("3.937007874 ft")
        tab.flow_edit.setText("0.7062933344 ft³/s")
        assert tab.from_area_edit.displayText() == "2.691 ft²"
        assert tab.from_height_edit.displayText() == "3.937 ft"
        assert tab.flow_edit.displayText() == "0.70629 ft³/s"
        window.build_cfast_case()
        option = QStyleOptionViewItem()
        tab.summary_table.itemDelegate().initStyleOption(option, tab.summary_table.model().index(0, 3))
        assert option.text == "2.691"
        assert tab.summary_table.horizontalHeaderItem(2).text() == "From\nCompartment"
        assert tab.summary_table.horizontalHeaderItem(6).text() == "To\nCompartment"
        window.show()
        window.tabs.setCurrentWidget(tab)
        QApplication.processEvents()
        tab.from_area_edit.setFocus()
        tab.from_area_edit.selectAll()
        QTest.keyClicks(tab.from_area_edit, "3.123456789 ft2")
        QTest.keyClick(tab.from_area_edit, Qt.Key.Key_Tab)
        assert tab.from_area_edit.displayText() == "3.1235 ft2"
        assert tab.from_area_edit.text() == "3.123456789 ft2"
        assert math.isclose(window.build_cfast_case().mechanical_vents[0].from_area,
                            3.123456789 * 0.3048 ** 2)
    finally:
        unit_system.set_indices(original_units)
        window.hide()
        window.deleteLater()


def check_pound_mass_units():
    from units import (
        DENSITY, E_COEFFICIENT, HOC, MASS, MASS_FRACTION, MASS_LOSS,
        SPECIFIC_EXTINCTION, SPECIFIC_HEAT, format_value, parse_value, unit_system,
    )

    original_units = dict(unit_system.selected)
    try:
        unit_system.set_index(MASS, unit_system.choices(MASS).index("lbm"))
        assert format_value(MASS, 0.4535924) == "1 lbm"
        assert format_value(MASS_FRACTION, 0.23) == "0.23 lbm/lbm"
        for kind, label, expected in (
            (MASS, "lbm", 0.4535924),
            (MASS_FRACTION, "lbm/lbm", 1.0),
            (MASS_LOSS, "lbm/s", 0.4535924),
            (DENSITY, "lbm/ft3", 0.4535924 / 0.3048 ** 3),
            (HOC, "BTU/lbm", 1.05435 / 0.4535924),
            (SPECIFIC_HEAT, "BTU/(lbm F)", 1.05435 / (0.4535924 * 5 / 9)),
            (SPECIFIC_EXTINCTION, "ft2/lbm", 0.3048 ** 2 / 0.4535924),
            (E_COEFFICIENT, "ft2/(lbm s)", 0.3048 ** 2 / 0.4535924),
        ):
            for spelling in (label, label.replace("lbm", "lb")):
                assert math.isclose(parse_value(kind, f"1 {spelling}", "Mass quantity"), expected)
            try:
                parse_value(kind, f"1 {label.replace('lbm', 'lbf')}", "Mass quantity")
            except ValueError:
                pass
            else:
                raise AssertionError("Pound force must not be accepted as pound mass")
    finally:
        unit_system.set_indices(original_units)


def check_units_start_in_si():
    from unittest.mock import patch
    from PySide6.QtCore import QSettings
    from units import unit_system

    original_units = dict(unit_system.selected)
    window = None
    try:
        with tempfile.TemporaryDirectory(prefix="cedit-startup-units-") as directory:
            settings = QSettings(str(Path(directory) / "settings.ini"), QSettings.Format.IniFormat)
            for key in unit_system.selected:
                settings.setValue(f"units/{key}", len(unit_system.choices(key)) - 1)
                unit_system.set_index(key, len(unit_system.choices(key)) - 1)
            with patch("main_window.QSettings", return_value=settings):
                window = CeditMainWindow()
            assert all(index == 0 for index in unit_system.selected.values())
            assert window.simulation_tab.interior_temperature_edit.text() == "20 °C"
            assert window.simulation_tab.pressure_edit.text() == "101325 Pa"
            assert window.simulation_tab.interior_o2_mass_fraction_edit.text() == "0.23 kg/kg"
            assert window.simulation_tab.flaming_extinction_edit.text() == "8700 m²/kg"
    finally:
        unit_system.set_indices(original_units)
        if window is not None:
            window.deleteLater()


def check_simulation_unit_display():
    from PySide6.QtTest import QTest
    from cfast_case import CfastCase
    from tabs.simulation_tab import SimulationTab
    from units import LENGTH, MASS, PRESSURE, SPECIFIC_EXTINCTION, display_value, parse_value, unit_label, unit_system

    original_units = dict(unit_system.selected)
    tab = SimulationTab()
    try:
        # All available mass units label both fractions without changing their values.
        for index, mass in enumerate(unit_system.choices(MASS)):
            unit_system.set_index(MASS, index)
            case = CfastCase()
            tab.load_case(case)
            for edit in (tab.interior_o2_mass_fraction_edit, tab.exterior_o2_mass_fraction_edit):
                assert edit.text() == f"0.23 {mass}/{mass}"
                edit.setText("0.21 kg/kg")
                edit.editingFinished.emit()
                assert edit.text() == f"0.21 {mass}/{mass}"
            tab.add_to_case(case)
            assert case.interior_o2_mass_fraction == case.exterior_o2_mass_fraction == 0.21

        # Rounded displays retain the exact pressure through normalization and unit changes.
        for pressure in (101325.0, 100987.654321):
            case = CfastCase(pressure=pressure)
            for label in ("psia", "Pa", "psig", "psi", "psia"):
                unit_system.set_index(PRESSURE, unit_system.choices(PRESSURE).index(label))
                tab.load_case(case)
                for _ in range(3):
                    tab.pressure_edit.editingFinished.emit()
                    tab.add_to_case(case)
                    assert case.pressure == pressure
            # User-entered precision also survives the rounded display.
            tab.pressure_edit.setText("14.123456789 psia")
            tab.pressure_edit.editingFinished.emit()
            assert tab.pressure_edit.text() == "14.12 psia"
            tab.add_to_case(case)
            assert case.pressure == parse_value(PRESSURE, "14.123456789 psia", "Pressure")

        # Five significant figures affect only display, even across repeated unit changes.
        for coefficients in ((8700.0, 4400.0), (1234.567891, 0.0000123456789)):
            case = CfastCase(specific_extinction=coefficients)
            for length_index, mass_index in ((3, 2), (0, 0), (3, 2), (0, 0)):
                unit_system.set_indices({LENGTH: length_index, MASS: mass_index})
                tab.load_case(case)
                for edit, value in zip((tab.flaming_extinction_edit, tab.smoldering_extinction_edit), coefficients):
                    expected = f"{display_value(SPECIFIC_EXTINCTION, value):.5g} {unit_label(SPECIFIC_EXTINCTION)}"
                    assert edit.text() == expected
                    edit.editingFinished.emit()
                    edit.editingFinished.emit()
                tab.add_to_case(case)
                assert case.specific_extinction == coefficients
        tab.flaming_extinction_edit.setText("1234.567891 m2/kg")
        tab.flaming_extinction_edit.editingFinished.emit()
        assert tab.flaming_extinction_edit.text() == "1234.6 m²/kg"
        tab.add_to_case(case)
        assert case.specific_extinction[0] == 1234.567891

        # Explicitly retyping the displayed number counts as a new pressure value.
        tab.load_case(CfastCase(pressure=101325.0))
        tab.pressure_edit.selectAll()
        QTest.keyClicks(tab.pressure_edit, "14.7 psia")
        tab.pressure_edit.editingFinished.emit()
        case = CfastCase()
        tab.add_to_case(case)
        assert case.pressure == parse_value(PRESSURE, "14.7 psia", "Pressure")
    finally:
        unit_system.set_indices(original_units)
        tab.deleteLater()


def check_derived_unit_input_files():
    from cfast_reader import read_cfast_input
    from unittest.mock import patch
    from main_window import EngineeringUnitsDialog
    from units import HEAT_FLUX, SPECIFIC_EXTINCTION, TEMPERATURE, unit_label, unit_system

    window = CeditMainWindow()
    original_units = dict(unit_system.selected)

    def save_input(path):
        with patch.object(QMessageBox, "information"):
            assert window.write_case_to_path(path) == path
        return read_cfast_input(path)

    units_file = Path(__file__).resolve().parents[2] / "Verification/Units/units_vents_devices.in"
    try:
        with tempfile.TemporaryDirectory(prefix="cedit-derived-units-") as directory:
            for profile, labels in EngineeringUnitsDialog.PRESETS.items():
                unit_system.set_indices({key: unit_system.choices(key).index(label)
                                         for key, label in labels.items()})
                for criterion, kind, setpoint in (("TEMPERATURE", TEMPERATURE, 100.0),
                                                  ("FLUX", HEAT_FLUX, 5.0)):
                    case = read_cfast_input(units_file)
                    for vents in (case.wall_vents, case.ceiling_floor_vents, case.mechanical_vents):
                        vents[0].criterion = criterion
                        vents[0].setpoint = setpoint
                        vents[0].target = "Thermocouple"
                    window.load_case(case)
                    sim = window.simulation_tab
                    if profile == "English":
                        assert sim.flaming_extinction_edit.text().endswith(" ft²/lbm")
                        assert sim.smoldering_extinction_edit.text().endswith(" ft²/lbm")
                        expected_extinction = 8700 * 0.4535924 / 0.3048 ** 2
                        assert sim.flaming_extinction_edit.text() == f"{expected_extinction:.5g} ft²/lbm"
                        coefficient = window.compartments_tab.flow_coefficient_edit.text()
                        assert coefficient.endswith(" ft³/(s √(psi))")
                        assert math.isclose(float(coefficient.split()[0]),
                                            0.07 * math.sqrt(6894.757293168) / 0.3048 ** 3)
                    tabs = (window.wall_vents_tab, window.ceiling_floor_vents_tab, window.mechanical_vents_tab)
                    for tab in tabs:
                        assert tab.setpoint_label.text() == f"Setpoint ({unit_label(kind)}):"
                        if profile == "English":
                            expected = 212.0 if criterion == "TEMPERATURE" else 5000 * 0.3048 ** 2 / 1054.35
                            assert math.isclose(float(tab.setpoint_edit.text()), expected)

                    # Write exactly what Save writes, then read the actual input file.
                    path = Path(directory) / f"{profile}_{criterion}.in"
                    saved = save_input(path)
                    assert math.isclose(saved.pressure, 101325.0)
                    assert math.isclose(saved.interior_temperature, 20.0)
                    assert math.isclose(saved.interior_o2_mass_fraction, 0.23)
                    assert all(math.isclose(a, b) for a, b in zip(saved.specific_extinction, (8700, 4400)))
                    assert math.isclose(saved.compartments[0].flow_coefficient, 0.07)
                    assert math.isclose(saved.compartments[0].width, 5.0)
                    for vents in (saved.wall_vents, saved.ceiling_floor_vents, saved.mechanical_vents):
                        assert math.isclose(vents[0].setpoint, setpoint)

                    # Explicit SI entries also work while English is selected.
                    sim.flaming_extinction_edit.setText("9000 m2/kg")
                    sim.smoldering_extinction_edit.setText("4500 m²/kg")
                    for tab in tabs:
                        tab.setpoint_edit.setText("80 C" if criterion == "TEMPERATURE" else "2 kW/m2")
                        tab.setpoint_edit.editingFinished.emit()
                    saved = save_input(path)
                    assert saved.specific_extinction == (9000.0, 4500.0)
                    for vents in (saved.wall_vents, saved.ceiling_floor_vents, saved.mechanical_vents):
                        assert math.isclose(vents[0].setpoint, 80.0 if criterion == "TEMPERATURE" else 2.0)
                    if profile == "English":
                        sim.pressure_edit.setText("15 psia")
                        sim.flaming_extinction_edit.setText("100 ft2/lbm")
                        sim.smoldering_extinction_edit.setText("50 ft2/lbm")
                        saved = save_input(path)
                        # The existing input writer retains six significant figures.
                        assert math.isclose(saved.pressure, 15 * 6894.757293168, rel_tol=5e-6)
                        assert math.isclose(saved.specific_extinction[0], 100 * 0.3048 ** 2 / 0.4535924, rel_tol=5e-6)
                        assert math.isclose(saved.specific_extinction[1], 50 * 0.3048 ** 2 / 0.4535924, rel_tol=5e-6)
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_surface_connections_removed():
    """Legacy surface connections must not be exposed or saved by the UI."""
    from dataclasses import replace

    from cfast_case import CeilingFloorSurfaceConnection, WallSurfaceConnection
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case

    window = CeditMainWindow()
    try:
        tab_names = [window.tabs.tabText(i) for i in range(window.tabs.count())]
        assert "Surface Connections" not in tab_names
        assert tab_names[-1] == "Output"

        case = opening_case()
        first_id = case.compartments[0].id
        second_id = "Comp 2"
        case.compartments.append(replace(case.compartments[0], id=second_id))
        case.wall_surface_connections = [WallSurfaceConnection(first_id, second_id)]
        case.ceiling_floor_surface_connections = [
            CeilingFloorSurfaceConnection(first_id, second_id, connection_type=kind)
            for kind in ("FLOOR", "CEILING")
        ]

        with tempfile.TemporaryDirectory(prefix="cedit-qt-connections-") as tmp:
            legacy_path = Path(tmp) / "legacy.in"
            write_cfast_input(case, legacy_path)
            assert legacy_path.read_text().count("&CONN") == 3
            window.load_cfast_input(legacy_path)
            window.update_live_validation()
            assert window.statusBar().currentMessage() == "No Errors"

            output_path = Path(tmp) / "saved.in"
            assert window.write_case_to_path(output_path) == output_path
            assert "&CONN" not in output_path.read_text()
            saved = read_cfast_input(output_path)
            assert [comp.id for comp in saved.compartments] == [first_id, second_id]
            assert not saved.wall_surface_connections
            assert not saved.ceiling_floor_surface_connections
    finally:
        window.deleteLater()


def check_unrecognized_namelists():
    from cfast_reader import parse_namelists

    for name in ("RAMP", "ramp", "UNKNOWN"):
        try:
            parse_namelists(f"&{name} ID='Test' /\n")
        except ValueError as exc:
            assert str(exc) == (
                f"Line 1: &{name.upper()} is not a valid CFAST or CData namelist "
                "for the current version."
            )
        else:
            raise AssertionError(f"&{name} should be rejected")


def check_debug_output():
    """The checkbox must control the logical flag, including after reloading."""
    from cfast_reader import parse_namelists, read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case

    window = CeditMainWindow()
    try:
        window.load_case(opening_case())
        with tempfile.TemporaryDirectory(prefix="cedit-qt-debug-") as tmp:
            path = Path(tmp) / "debug.in"
            # Old input files must not restore the removed output-mode switch.
            for value in ("T", "F"):
                write_cfast_input(opening_case(), path)
                path.write_text(path.read_text().replace(
                    "&TAIL /", f"&OUTP VALIDATION_OUTPUT = {value}, FYI = 'Output' /\n&TAIL /",
                ))
                window.load_cfast_input(path)
                assert not hasattr(window.output_tab, "validation_checkbox")
                assert window.write_case_to_path(path) == path
                assert "VALIDATION_OUTPUT" not in path.read_text()
                assert not hasattr(read_cfast_input(path), "validation_output")

            for checked in (False, True, False):
                window.output_tab.debug_checkbox.setChecked(checked)
                assert window.write_case_to_path(path) == path
                records = parse_namelists(path.read_text())
                diag = [record.fields for record in records if record.name == "DIAG"]
                assert diag == ([{"RESIDUAL_DEBUG_PRINT": [True]}] if checked else [])
                window.load_cfast_input(path)
                assert window.output_tab.debug_checkbox.isChecked() == checked

            for value, checked in (("T", True), (".TRUE.", True), ("F", False), (".FALSE.", False)):
                case = opening_case()
                write_cfast_input(case, path)
                text = path.read_text().replace(
                    "&TAIL /",
                    f"&DIAG RESIDUAL_DEBUG_PRINT = {value}, "
                    "RADIATION_SUB_MODEL = 'OFF', T = 0, 60, F = 20, 100 /\n&TAIL /",
                )
                path.write_text(text)
                window.load_cfast_input(path)
                assert window.output_tab.debug_checkbox.isChecked() == checked
                for enabled in (checked, not checked):
                    window.output_tab.debug_checkbox.setChecked(enabled)
                    assert window.write_case_to_path(path) == path
                    diag = [record.fields for record in parse_namelists(path.read_text())
                            if record.name == "DIAG"]
                    expected = {"RADIATION_SUB_MODEL": ["OFF"], "T": [0, 60], "F": [20, 100]}
                    if enabled:
                        expected["RESIDUAL_DEBUG_PRINT"] = [True]
                    assert diag == [expected]
                    assert read_cfast_input(path).debug_output == enabled
                    window.load_cfast_input(path)

            # Invalid legacy settings must reach CFAST unchanged, not be repaired.
            legacy = "&DIAG DEBUG_PRINT = 'ON', DASSL_DEBUG_PRINT = 'OFF', RESIDUAL_DEBUG_PRINT = 'ON' /"
            case = opening_case()
            case.extra_namelists = [legacy]
            write_cfast_input(case, path)
            window.load_cfast_input(path)
            assert not window.output_tab.debug_checkbox.isChecked()
            assert window.write_case_to_path(path) == path
            diag = [record.fields for record in parse_namelists(path.read_text())
                    if record.name == "DIAG"]
            assert diag == [parse_namelists(legacy)[0].fields]
    finally:
        window.deleteLater()


def check_output_visualizations():
    from PySide6.QtCore import QSignalBlocker
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import opening_case
    from units import LENGTH, TEMPERATURE, format_value, unit_system

    window = CeditMainWindow()
    tab = window.output_tab
    original_units = dict(unit_system.selected)
    try:
        for length_unit, temperature_unit in ((0, 0), (3, 2)):
            unit_system.set_index(LENGTH, length_unit)
            unit_system.set_index(TEMPERATURE, temperature_unit)
            case = opening_case()
            case.compartments[0].width = 1.0
            case.compartments[0].depth = 1.0
            case.compartments[0].height = 1.0
            case.output_visualizations = []
            window.load_case(case)
            tab.add_visualization()
            assert tab.visual_position_edit.text() == format_value(LENGTH, case.compartments[0].width / 2.0)
            assert tab.visual_compartment_edit.currentText() == case.compartments[0].id
            assert not tab.visual_temperature_edit.isEnabled()
            for axis in range(3):
                tab.axis_combo.setCurrentIndex(axis)
                window.update_live_validation()
                assert window.statusBar().currentMessage() == "No Errors"

            tab.visualization_type_combo.setCurrentText("ISOF")
            assert tab.visual_temperature_edit.isEnabled()
            assert not tab.visual_position_edit.isEnabled()
            assert not tab.axis_combo.isEnabled()
            assert tab.axis_combo.isHidden() and tab.axis_label.isHidden()
            assert tab.visual_position_edit.isHidden() and tab.visual_position_label.isHidden()
            assert not tab.visual_temperature_edit.isHidden()
            tab.visual_temperature_edit.setText("212 F")
            tab.visual_temperature_edit.editingFinished.emit()
            tab.duplicate_visualization()
            tab.visual_compartment_edit.setCurrentText(case.compartments[0].id)
            with tempfile.TemporaryDirectory(prefix="cedit-isof-") as directory:
                path = Path(directory) / "output.in"
                write_cfast_input(window.build_cfast_case(), path)
                assert path.read_text().count("&ISOF") == 2
                loaded = read_cfast_input(path)
                assert not loaded.extra_namelists
                assert all(vis.visualization_type == "ISOF" and math.isclose(vis.value, 100.0)
                           for vis in loaded.output_visualizations)
                window.load_case(loaded)
                tab.visual_table.setCurrentCell(0, 1)
                tab.visual_row_to_editor(0)
                assert tab.visual_temperature_edit.text() == format_value(TEMPERATURE, 100.0)

            # A table type edit must reset temperature to a valid slice position.
            with QSignalBlocker(tab.visual_table):
                tab.visual_table.item(0, 1).setText("2-D")
            tab.visual_cell_changed(0, 1)
            assert tab.visual_position_edit.text() == format_value(LENGTH, 0.0)
            assert not tab.visual_temperature_edit.isEnabled()
            tab.visualization_type_combo.setCurrentText("3-D")
            assert not tab.visual_position_edit.isEnabled()
            window.update_live_validation()
            assert window.statusBar().currentMessage() == "No Errors"
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_fire_import():
    import copy
    from unittest.mock import patch
    from PySide6.QtCore import Qt
    from PySide6.QtWidgets import QDialog, QPushButton
    from cfast_case import FireProperty
    from cfast_reader import read_cfast_input, read_fire_properties
    from cfast_writer import write_cfast_input
    from tabs.fires_tab import FireImportDialog
    from units import ENERGY, HRR, HOC, format_value, unit_system

    assert FireProperty(id="Methane").fuel_formula() == "CH₄"
    assert FireProperty(id="Single atoms", hydrogen=1, oxygen=1, nitrogen=1, chlorine=1).fuel_formula() == "CHONCl"
    assert FireProperty(id="Decane", carbon=10, hydrogen=22).fuel_formula() == "C₁₀H₂₂"
    assert FireProperty(id="Hydrogen", carbon=0, hydrogen=2).fuel_formula() == "H₂"

    window = CeditMainWindow()
    tab = window.fires_tab
    original_units = dict(unit_system.selected)
    try:
        with tempfile.TemporaryDirectory(prefix="cedit-fire-import-") as directory:
            directory = Path(directory)
            source = directory / "source.in"
            source.write_text(
                "&HEAD VERSION=7600, TITLE='Source scenario' /\n"
                "&COMP ID='Source room', WIDTH=12, DEPTH=8, HEIGHT=3 /\n"
                "&FIRE ID='Source instance', COMP_ID='Source room', FIRE_ID='Fuel', "
                "IGNITION_CRITERION='TEMPERATURE', DEVC_ID='Source target', SETPOINT=200 /\n"
                "&TABL ID='Fuel', DATA=0.02, 250, 60, 0.04, 2, 0.03, 0.5, 0.01, 0 /\n"
                "&TABL ID='Fuel', DATA=0.02, 0, 0, 0.04, 2, 0.03, 0.5, 0.01, 0 /\n"
                "&CHEM ID='Fuel', CARBON=2, HYDROGEN=6, HEAT_OF_COMBUSTION=42000, "
                "RADIATIVE_FRACTION=0.25 /\n"
                "&TABL ID='Fuel', LABELS='TRACE_YIELD','HRR','TIME','SOOT_YIELD',"
                "'AREA','HCN_YIELD','HEIGHT','CO_YIELD','HCL_YIELD' /\n",
                encoding="utf-8",
            )
            props = read_fire_properties(source)
            prop = props[0]
            assert prop.carbon == 2 and prop.heat_of_combustion == 42000
            point = prop.ramp[0]
            assert (point.time, point.hrr, point.height, point.area) == (60, 250, 0.5, 2)
            assert (point.co_yield, point.soot_yield, point.hcn_yield, point.trace_yield) == (0.01, 0.04, 0.03, 0.02)
            assert read_cfast_input(source).fire_properties == props

            constants = directory / "constants.in"
            constants.write_text(
                "&CHEM ID='Constant yields', AREA=2, HRR=50, CO_YIELD=0.1, SOOT_YIELD=0.2 /\n"
                "&TABL ID='Constant yields', LABELS='TIME', 'HEIGHT' /\n"
                "&TABL ID='Constant yields', DATA=0, 0.5 /\n"
            )
            constant_point = read_fire_properties(constants)[0].ramp[0]
            assert (constant_point.hrr, constant_point.area, constant_point.co_yield,
                    constant_point.soot_yield) == (50, 2, 0.1, 0.2)
            for content in (
                "&CHEM ID='Incomplete' /",
                "&CHEM ID='Invalid' / &TABL ID='Invalid', LABELS='TIME','HRR' / "
                "&TABL ID='Invalid', DATA=0 /",
                "FIRE,1,0,0,0,1,TIME,0,0,0,0,Incomplete\nCHEMI,1,4,0,0,0,0.35,50000000\n",
            ):
                constants.write_text(content)
                try:
                    read_fire_properties(constants)
                except ValueError:
                    pass
                else:
                    raise AssertionError("Incomplete fire definitions must report an error")

            baseline = copy.deepcopy(window.build_cfast_case())
            unit_system.set_index(ENERGY, 1)
            window.load_case(baseline)
            dialog = FireImportDialog([(str(source), prop)])
            assert dialog.table.item(0, 3).text() == format_value(HRR, 250)
            assert dialog.table.item(0, 4).text() == format_value(HOC, 42000)
            dialog.set_all_checked(Qt.CheckState.Unchecked)
            assert not dialog.selected_properties()
            dialog.set_all_checked(Qt.CheckState.Checked)
            assert dialog.selected_properties() == props
            dialog.deleteLater()

            button = next(button for button in tab.findChildren(QPushButton)
                          if button.text() == "From File")
            file_picker = "tabs.fires_tab.QFileDialog.getOpenFileNames"
            dialog_exec = "tabs.fires_tab.FireImportDialog.exec"
            with patch(file_picker, return_value=([], "")):
                button.click()
            with patch(file_picker, return_value=([str(source)], "")), \
                    patch(dialog_exec, return_value=QDialog.DialogCode.Rejected):
                button.click()
            def select_none(dialog):
                dialog.set_all_checked(Qt.CheckState.Unchecked)
                return QDialog.DialogCode.Accepted

            with patch(file_picker, return_value=([str(source)], "")), patch(dialog_exec, select_none):
                button.click()
            assert window.build_cfast_case() == baseline

            empty = directory / "empty.in"
            empty.write_text("&HEAD VERSION=7600 /\n")
            invalid = directory / "invalid.in"
            invalid.write_text("&CHEM ID='broken'")
            messages = []

            def select_second(dialog):
                assert len(dialog.candidates) == 2
                dialog.set_all_checked(Qt.CheckState.Unchecked)
                dialog.table.item(1, 0).setCheckState(Qt.CheckState.Checked)
                return QDialog.DialogCode.Accepted

            with patch(file_picker, return_value=(
                    [str(source), str(source), str(empty), str(invalid), str(directory / "missing.in")], "")), \
                    patch(dialog_exec, select_second), \
                    patch.object(QMessageBox, "warning", side_effect=lambda *args: messages.append(args[2])):
                button.click()
            assert len(messages) == 1
            assert all(name in messages[0] for name in ("empty.in", "invalid.in", "missing.in"))
            assert len(tab.fires) == len(baseline.fires) + 1

            with patch(file_picker, return_value=([str(source)], "")), \
                    patch(dialog_exec, return_value=QDialog.DialogCode.Accepted):
                button.click()
            result = window.build_cfast_case()
            assert len(result.fires) == len(baseline.fires) + 2
            assert result.fires[:len(baseline.fires)] == baseline.fires
            assert result.fire_properties[:len(baseline.fire_properties)] == baseline.fire_properties
            assert result.compartments == baseline.compartments
            assert result.targets == baseline.targets and result.title == baseline.title
            imported = result.fires[-2:]
            assert [fire.fire_property_id for fire in imported] == ["Fuel", "Fuel_2"]
            room = baseline.compartments[0]
            assert all(fire.comp_id == room.id and not fire.target and fire.setpoint == 0
                       and fire.ignition_criterion == "TIME"
                       and fire.x_position == room.width / 2 and fire.y_position == room.depth / 2
                       for fire in imported)
            assert result.fire_properties[-1].sorted_ramp() == prop.sorted_ramp()
            saved = directory / "saved.in"
            write_cfast_input(result, saved)
            reopened = read_cfast_input(saved)
            assert reopened.fires == result.fires
            for actual, expected in zip(reopened.fire_properties, result.fire_properties):
                assert actual.sorted_ramp() == expected.sorted_ramp()
                assert actual.heat_of_combustion == expected.heat_of_combustion
            window.load_case(reopened)
            assert len(tab.fires) == len(result.fires)
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def check_unused_fire_definitions():
    import copy
    from cfast_case import FireDefinition, FireProperty, FireRampPoint
    from cfast_reader import parse_namelists, read_cfast_input
    from main_window import opening_case

    case = opening_case()
    room = case.compartments[0]
    case.fire_properties = [
        FireProperty(id=f"Fire_{i}_Fire", ramp=[
            FireRampPoint(0, 0), FireRampPoint(300, 1000),
            FireRampPoint(600, 200 if i == 1 else 0),
        ]) for i in range(1, 4)
    ]
    case.fires = [FireDefinition(
        id=f"Fire_{i}", comp_id=room.id, fire_property_id=f"Fire_{i}_Fire",
        x_position=room.width / 2, y_position=room.depth / 2,
    ) for i in range(1, 4)]
    window = CeditMainWindow()
    try:
        window.load_case(case)
        tab = window.fires_tab
        tab.select_fire(2)
        tab.fire_property_combo.setCurrentText("Fire_1_Fire")
        expected = copy.deepcopy(window.build_cfast_case())
        assert expected.fires[2].fire_property_id == "Fire_1_Fire"
        with tempfile.TemporaryDirectory(prefix="cedit-unused-fires-") as directory:
            path = Path(directory) / "shared.in"
            assert window.write_case_to_path(path) == path
            records = parse_namelists(path.read_text())
            assert [record.fields["ID"][0] for record in records if record.name == "CHEM"] == [
                "Fire_1_Fire", "Fire_2_Fire"]
            tables = [record for record in records if record.name == "TABL"]
            assert {record.fields["ID"][0] for record in tables} == {"Fire_1_Fire", "Fire_2_Fire"}
            assert len(tables) == 8  # One header and three points per used definition.
            reopened = read_cfast_input(path)
            assert reopened.fires == expected.fires
            assert reopened.fire_properties == expected.fire_properties[:2]
            assert window.build_cfast_case() == expected  # Saving does not prune the editor.

            # Reusing the old definition must include it again on the next save.
            tab.select_fire(2)
            tab.fire_property_combo.setCurrentText("Fire_3_Fire")
            assert window.write_case_to_path(path) == path
            assert len(read_cfast_input(path).fire_properties) == 3

            # Removing every instance leaves no CHEM or TABL records to write.
            while tab.fires:
                tab.select_fire(0)
                tab.remove_fire()
            assert window.write_case_to_path(path) == path
            assert not any(record.name in {"FIRE", "CHEM", "TABL"}
                           for record in parse_namelists(path.read_text()))
            assert len(tab.fire_properties) == 3
    finally:
        window.deleteLater()


def check_fire_e_coefficient():
    from cfast_reader import fire_from_fields, parse_namelists, read_cfast_input
    from cfast_writer import validate_case, write_cfast_input
    from units import E_COEFFICIENT, LENGTH, MASS, TIME, format_value, parse_value, unit_system

    root = find_repo_root(Path(__file__))
    case = read_cfast_input(root / "Verification/Sprinkler/e_coefficient.in")
    assert [fire.e_coefficient for fire in case.fires] == [1, 2, 0, None, 1, 0.5, 0.1, 1]
    for value in (-1, float("inf"), float("nan"), "invalid", True):
        try:
            fire_from_fields({"E_COEFFICIENT": [value]})
        except ValueError as exc:
            assert "E_COEFFICIENT" in str(exc)
        else:
            raise AssertionError("Invalid E_COEFFICIENT must not silently select the legacy model")
    original_units = dict(unit_system.selected)
    window = CeditMainWindow()
    try:
        with tempfile.TemporaryDirectory(prefix="cedit-e-coefficient-") as directory:
            path = Path(directory) / "saved.in"
            for indices in ((0, 0, 0), (3, 2, 1)):
                unit_system.set_index(LENGTH, indices[0])
                unit_system.set_index(MASS, indices[1])
                unit_system.set_index(TIME, indices[2])
                window.load_case(case)
                tab = window.fires_tab
                tab.select_fire(0)
                assert tab.e_coefficient_edit.text() == format_value(E_COEFFICIENT, 1)
                assert math.isclose(parse_value(E_COEFFICIENT, "1 m2/(kg s)", "E"), 1)
                tab.e_coefficient_edit.setText("0 m2/(kg s)")
                tab.select_fire(1)
                assert tab.e_coefficient_edit.text() == format_value(E_COEFFICIENT, 2)
                assert tab.fires[0].e_coefficient == 0
                tab.e_coefficient_edit.clear()
                # Changing the shared definition must not change the instance's coefficient.
                tab.fire_property_combo.setCurrentText("Growing burner")
                assert tab.fires[1].e_coefficient is None
                assert window.write_case_to_path(path) == path
                records = [record for record in parse_namelists(path.read_text()) if record.name == "FIRE"]
                assert records[0].fields["E_COEFFICIENT"] == [0]
                assert "E_COEFFICIENT" not in records[1].fields
                assert "E_COEFFICIENT" not in records[3].fields
                reopened = read_cfast_input(path)
                assert [fire.e_coefficient for fire in reopened.fires] == [0, None, 0, None, 1, 0.5, 0.1, 1]
                assert case.fires[0].e_coefficient == 1  # Editor owns a copy.

            for invalid in (-1, float("inf"), float("nan")):
                case.fires[0].e_coefficient = invalid
                try:
                    validate_case(case)
                except ValueError as exc:
                    assert "E_COEFFICIENT" in str(exc)
                else:
                    raise AssertionError("Invalid E_COEFFICIENT was accepted")
            case.fires[0].e_coefficient = 1
            write_cfast_input(case, path)
            assert read_cfast_input(path).fires == case.fires
    finally:
        unit_system.set_indices(original_units)
        window.deleteLater()


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root.resolve() if args.repo_root else find_repo_root(Path(__file__))
    cfast_exe = resolve_executable(args.cfast_exe)

    patch_message_boxes()
    app = QApplication.instance() or QApplication([])
    check_verification_case_discovery()
    check_compartment_header_palettes()
    check_target_material_initialization()
    check_target_temperature_depth()
    check_target_material_validation()
    check_material_id_length()
    check_target_orientation()
    check_adiabatic_target()
    check_adiabatic_convection_coefficients()
    check_mechanical_vent_defaults()
    check_detection_device_parameters()
    check_engineering_units_editor()
    check_quantity_display_precision()
    check_pound_mass_units()
    check_units_start_in_si()
    check_simulation_unit_display()
    check_derived_unit_input_files()
    check_surface_connections_removed()
    check_unrecognized_namelists()
    check_debug_output()
    check_output_visualizations()
    check_fire_import()
    check_unused_fire_definitions()
    check_fire_e_coefficient()

    if args.mode == "rewrite":
        if args.work_dir is None:
            work_root = Path(tempfile.mkdtemp(prefix="cedit-qt-rewrite-"))
            remove_when_done = True
        else:
            work_root = args.work_dir.resolve()
            work_root.mkdir(parents=True, exist_ok=True)
            remove_when_done = False
    else:
        work_root, remove_when_done = prepare_work_tree(repo_root, args.work_dir)

    try:
        if args.mode == "rewrite":
            cases = (
                resolve_input_cases(repo_root, args.input)
                if args.input
                else select_cases(repo_root / "Verification", args.case)
            )
            case_results = [
                rewrite_case(app, repo_root, work_root, case_path)
                for case_path in cases
            ]
        else:
            verification_root = work_root / "Verification"
            cases = select_cases(verification_root, args.case)
            case_results = [
                run_case(app, cfast_exe, case_path, args.timeout)
                for case_path in cases
            ]

        helper_results: list[HelperResult] = []
        if args.mode == "run" and not args.skip_helpers:
            helper_results = run_helper_scripts(work_root)

        comparison_results: list[ComparisonResult] = []
        if args.mode == "run" and args.compare_targets:
            comparison_results = compare_targets(work_root)

        print_summary(repo_root, work_root, case_results, helper_results, comparison_results)

        failed_cases = any(not result.ok for result in case_results)
        failed_helpers = any(result.status == "FAIL" for result in helper_results)
        failed_targets = any(result.status == "FAIL" for result in comparison_results)
        return 1 if failed_cases or failed_helpers or failed_targets else 0
    finally:
        if remove_when_done and not args.keep_work_dir:
            shutil.rmtree(work_root)


if __name__ == "__main__":
    raise SystemExit(main())

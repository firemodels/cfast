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
    error: float | None
    tolerance: float | None
    message: str


@dataclass
class Series:
    x: list[float]
    y: list[float]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Load CFAST verification cases through the CEdit Qt UI, run CFAST, "
            "and leave generated CSV outputs ready for the official verification script."
        )
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
            "Defaults to all Verification/*/*.in cases."
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


def prepare_work_tree(repo_root: Path, work_dir: Path | None) -> tuple[Path, bool]:
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
    cases = sorted(verification_root.glob("*/*.in"))
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
        raise RuntimeError("No Verification/*/*.in case matched: " + ", ".join(missing))

    return selected


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


def float_or_none(value: str | None) -> float | None:
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


def filter_series(series: Series, start: float | None, end: float | None) -> Series:
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


def interpolate(series: Series, x_value: float) -> float | None:
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


def point_error(expected: float, predicted: float, quantity: str) -> float | None:
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


def compare_series(expected: Series, predicted: Series, quantity: str, metric: str) -> float | None:
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
        rel = result.path.relative_to(work_root / "Verification")
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


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root.resolve() if args.repo_root else find_repo_root(Path(__file__))
    cfast_exe = resolve_executable(args.cfast_exe)

    patch_message_boxes()
    app = QApplication.instance() or QApplication([])

    work_root, remove_when_done = prepare_work_tree(repo_root, args.work_dir)

    try:
        verification_root = work_root / "Verification"
        cases = select_cases(verification_root, args.case)
        case_results = [
            run_case(app, cfast_exe, case_path, args.timeout)
            for case_path in cases
        ]

        helper_results: list[HelperResult] = []
        if not args.skip_helpers:
            helper_results = run_helper_scripts(work_root)

        comparison_results: list[ComparisonResult] = []
        if args.compare_targets:
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

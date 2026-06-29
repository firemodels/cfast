#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


@dataclass
class CaseResult:
    case_path: Path
    returncode: int
    elapsed: float
    stdout: str
    stderr: str

    @property
    def passed(self) -> bool:
        return self.returncode == 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run CFAST verification cases so the generated CSV files can be "
            "processed by Utilities/Python/CFAST_verification_script.py."
        )
    )
    parser.add_argument(
        "--cfast-exe",
        default="cfast",
        help=(
            "CFAST executable to run. Defaults to 'cfast' from PATH. "
            "Example from Verification/: ../Build/CFAST/gnu_osx/cfast7_osx"
        ),
    )
    parser.add_argument(
        "--verification-root",
        type=Path,
        default=Path(__file__).resolve().parent,
        help="Verification directory to run. Defaults to this script's directory.",
    )
    parser.add_argument(
        "--case",
        action="append",
        default=[],
        help=(
            "Case stem, filename, or path relative to Verification/ to run. "
            "May be repeated. Defaults to all guide verification cases."
        ),
    )
    parser.add_argument(
        "--recursive",
        action="store_true",
        help=(
            "Run all .in files recursively, including NRC_Users_Guide cases. "
            "By default only Verification/*/*.in cases are run."
        ),
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the cases that would run without launching CFAST.",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop after the first CFAST failure.",
    )
    return parser.parse_args()


def resolve_executable(executable: str) -> str:
    path = Path(executable).expanduser()
    if path.is_absolute() or "/" in executable or "\\" in executable:
        return str(path.resolve())
    return executable


def discover_cases(verification_root: Path, recursive: bool) -> list[Path]:
    pattern = "**/*.in" if recursive else "*/*.in"
    return sorted(path for path in verification_root.glob(pattern) if path.is_file())


def select_cases(cases: list[Path], verification_root: Path, requested: list[str]) -> list[Path]:
    if not requested:
        return cases

    selected: list[Path] = []
    requested_values = {value.lower() for value in requested}

    for case_path in cases:
        relative = case_path.relative_to(verification_root)
        aliases = {
            case_path.name.lower(),
            case_path.stem.lower(),
            str(relative).lower(),
            str(relative.with_suffix("")).lower(),
        }
        if aliases & requested_values:
            selected.append(case_path)

    matched_aliases = set()
    for case_path in selected:
        relative = case_path.relative_to(verification_root)
        matched_aliases.update(
            {
                case_path.name.lower(),
                case_path.stem.lower(),
                str(relative).lower(),
                str(relative.with_suffix("")).lower(),
            }
        )

    missing = sorted(value for value in requested_values if value not in matched_aliases)
    if missing:
        raise ValueError("No verification case matched: " + ", ".join(missing))

    return selected


def run_case(cfast_exe: str, case_path: Path) -> CaseResult:
    start_time = time.monotonic()
    proc = subprocess.run(
        [cfast_exe, case_path.name],
        cwd=case_path.parent,
        text=True,
        capture_output=True,
    )
    return CaseResult(
        case_path=case_path,
        returncode=proc.returncode,
        elapsed=time.monotonic() - start_time,
        stdout=proc.stdout,
        stderr=proc.stderr,
    )


def last_nonblank_line(text: str) -> str:
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    return lines[-1] if lines else ""


def print_case_list(cases: list[Path], verification_root: Path) -> None:
    for case_path in cases:
        print(case_path.relative_to(verification_root))


def print_failure_detail(result: CaseResult) -> None:
    output = last_nonblank_line(result.stderr) or last_nonblank_line(result.stdout)
    if output:
        print(f"       {output}")


def main() -> int:
    args = parse_args()
    verification_root = args.verification_root.resolve()

    if not verification_root.is_dir():
        print(f"Verification directory not found: {verification_root}", file=sys.stderr)
        return 2

    try:
        cases = select_cases(
            discover_cases(verification_root, args.recursive),
            verification_root,
            args.case,
        )
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    if not cases:
        print(f"No CFAST input files found under {verification_root}", file=sys.stderr)
        return 2

    cfast_exe = resolve_executable(args.cfast_exe)

    print(f"Verification root: {verification_root}")
    print(f"CFAST executable:  {cfast_exe}")
    print(f"Cases:             {len(cases)}")

    if args.dry_run:
        print_case_list(cases, verification_root)
        return 0

    results: list[CaseResult] = []
    for index, case_path in enumerate(cases, start=1):
        relative = case_path.relative_to(verification_root)
        print(f"[{index:>3}/{len(cases)}] {relative}", flush=True)

        result = run_case(cfast_exe, case_path)
        results.append(result)

        status = "PASS" if result.passed else "FAIL"
        print(f"       {status} returncode={result.returncode} elapsed={result.elapsed:.1f} s")
        if not result.passed:
            print_failure_detail(result)
            if args.fail_fast:
                break

    passed = sum(1 for result in results if result.passed)
    failed = len(results) - passed

    print("")
    print(f"Completed: {passed}/{len(results)} passed, {failed} failed.")
    if failed:
        print("Failed cases:")
        for result in results:
            if not result.passed:
                print(f"  {result.case_path.relative_to(verification_root)}")

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())

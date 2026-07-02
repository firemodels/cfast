from __future__ import annotations

import argparse
import csv
import sys
from dataclasses import dataclass
from pathlib import Path


PROFILES = {
    "default": {
        "length": 0,
        "mass": 0,
        "time": 0,
        "temperature": 0,
        "pressure": 0,
        "energy": 0,
        "smoke": 0,
    },
    "english": {
        "length": 3,  # ft
        "mass": 2,  # lb
        "time": 1,  # min
        "temperature": 2,  # F
        "pressure": 3,  # atm
        "energy": 3,  # BTU
        "smoke": 1,  # %/ft
    },
}


@dataclass(frozen=True)
class Quantity:
    value: float
    kind: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Check CEditQt engineering-unit display values for Verification/Units "
            "cases against a sidecar expected-value CSV."
        )
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=None,
        help="CFAST repository root. Defaults to the first parent containing Source/CeditQt.",
    )
    parser.add_argument(
        "--profile",
        choices=sorted(PROFILES),
        default="english",
        help="Engineering-unit profile to check. Defaults to english.",
    )
    parser.add_argument(
        "--expected",
        type=Path,
        default=None,
        help="Expected-value CSV. Defaults to units_ui_expected_<profile>.csv.",
    )
    parser.add_argument(
        "--write-actual",
        type=Path,
        default=None,
        help="Optional path for a CSV of actual displayed values and errors.",
    )
    return parser.parse_args()


def find_repo_root(start: Path) -> Path:
    for path in [start.resolve(), *start.resolve().parents]:
        if (path / "Source" / "CeditQt").is_dir() and (path / "Verification").is_dir():
            return path
    raise RuntimeError("Could not find CFAST repo root containing Source/CeditQt.")


def configure_imports(repo_root: Path) -> None:
    cedit_qt = repo_root / "Source" / "CeditQt"
    sys.path.insert(0, str(cedit_qt))


def find_by_id(items, item_id: str, label: str):
    for item in items:
        if getattr(item, "id", "") == item_id:
            return item
    raise KeyError(f"{label} {item_id!r} was not found.")


def indexed_value(values: list[float], token: str) -> float:
    index = int(token.rsplit("_", 1)[1]) - 1
    return values[index]


def resolve_field(case, field: str) -> Quantity:
    from units import (
        AREA,
        CONDUCTIVITY,
        DENSITY,
        FLOWRATE,
        HEAT_FLUX,
        HOC,
        HRR,
        LENGTH,
        PRESSURE,
        RTI,
        SMOKE,
        SPECIFIC_HEAT,
        TEMPERATURE,
        TIME,
        VELOCITY,
    )

    parts = field.split(".")
    section = parts[0]

    if section == "time":
        attr = {
            "simulation": "simulation_time",
            "print": "print_interval",
            "smokeview": "smokeview_interval",
            "spreadsheet": "spreadsheet_interval",
        }[parts[1]]
        return Quantity(getattr(case, attr), TIME)

    if section == "init":
        attr, kind = {
            "interior_temperature": ("interior_temperature", TEMPERATURE),
            "exterior_temperature": ("exterior_temperature", TEMPERATURE),
            "pressure": ("pressure", PRESSURE),
        }[parts[1]]
        return Quantity(getattr(case, attr), kind)

    if section == "matl":
        material = find_by_id(case.materials, parts[1], "material")
        attr, kind = {
            "conductivity": ("conductivity", CONDUCTIVITY),
            "specific_heat": ("specific_heat", SPECIFIC_HEAT),
            "density": ("density", DENSITY),
            "thickness": ("thickness", LENGTH),
        }[parts[2]]
        return Quantity(getattr(material, attr), kind)

    if section == "comp":
        compartment = find_by_id(case.compartments, parts[1], "compartment")
        attr, kind = {
            "width": ("width", LENGTH),
            "depth": ("depth", LENGTH),
            "height": ("height", LENGTH),
            "origin_x": ("origin_x", LENGTH),
            "origin_y": ("origin_y", LENGTH),
            "origin_z": ("origin_z", LENGTH),
            "wall_leak_area": ("wall_leak_area", AREA),
            "floor_leak_area": ("floor_leak_area", AREA),
        }[parts[2]]
        return Quantity(getattr(compartment, attr), kind)

    if section == "wall_vent":
        vent = find_by_id(case.wall_vents, parts[1], "wall vent")
        token = parts[2]
        if token.startswith("t_"):
            return Quantity(indexed_value(vent.t_values, token), TIME)
        attr, kind = {
            "bottom": ("bottom", LENGTH),
            "height": ("height", LENGTH),
            "width": ("width", LENGTH),
            "offset": ("offset", LENGTH),
        }[token]
        return Quantity(getattr(vent, attr), kind)

    if section == "ceiling_floor_vent":
        vent = find_by_id(case.ceiling_floor_vents, parts[1], "ceiling/floor vent")
        token = parts[2]
        if token.startswith("t_"):
            return Quantity(indexed_value(vent.t_values, token), TIME)
        attr, kind = {
            "area": ("area", AREA),
            "offset_x": ("offset_x", LENGTH),
            "offset_y": ("offset_y", LENGTH),
        }[token]
        return Quantity(getattr(vent, attr), kind)

    if section == "mechanical_vent":
        vent = find_by_id(case.mechanical_vents, parts[1], "mechanical vent")
        token = parts[2]
        if token.startswith("t_"):
            return Quantity(indexed_value(vent.t_values, token), TIME)
        attr, kind = {
            "flow": ("flow", FLOWRATE),
            "from_area": ("from_area", AREA),
            "to_area": ("to_area", AREA),
            "from_height": ("from_height", LENGTH),
            "to_height": ("to_height", LENGTH),
            "begin_dropoff": ("begin_dropoff", PRESSURE),
            "zero_flow": ("zero_flow", PRESSURE),
            "filter_time": ("filter_time", TIME),
        }[token]
        return Quantity(getattr(vent, attr), kind)

    if section == "target":
        target = find_by_id(case.targets, parts[1], "target")
        attr, kind = {
            "x_position": ("x_position", LENGTH),
            "y_position": ("y_position", LENGTH),
            "z_position": ("z_position", LENGTH),
            "temperature_depth": ("temperature_depth", LENGTH),
        }[parts[2]]
        return Quantity(getattr(target, attr), kind)

    if section == "device":
        device = find_by_id(case.detection_devices, parts[1], "device")
        attr, kind = {
            "activation_temperature": ("activation_temperature", TEMPERATURE),
            "activation_obscuration": ("activation_obscuration", SMOKE),
            "rti": ("rti", RTI),
            "spray_density": ("spray_density", VELOCITY),
        }[parts[2]]
        return Quantity(getattr(device, attr), kind)

    if section == "fire":
        fire = find_by_id(case.fires, parts[1], "fire")
        token = parts[2]
        if token == "setpoint":
            kind = {
                "TEMPERATURE": TEMPERATURE,
                "FLUX": HEAT_FLUX,
            }.get(fire.ignition_criterion.upper(), TIME)
            return Quantity(fire.setpoint, kind)
        attr, kind = {
            "x_position": ("x_position", LENGTH),
            "y_position": ("y_position", LENGTH),
        }[token]
        return Quantity(getattr(fire, attr), kind)

    if section == "fire_property":
        prop = find_by_id(case.fire_properties, parts[1], "fire property")
        token = parts[2]
        if token == "heat_of_combustion":
            return Quantity(prop.heat_of_combustion, HOC)
        if token.startswith("ramp_"):
            _, row_text, attr = token.split("_", 2)
            row = int(row_text) - 1
            point = prop.sorted_ramp()[row]
            kind = {
                "time": TIME,
                "hrr": HRR,
                "height": LENGTH,
                "area": AREA,
            }[attr]
            return Quantity(getattr(point, attr), kind)

    if section == "output_visualization":
        index = int(parts[1]) - 1
        return Quantity(case.output_visualizations[index].value, LENGTH)

    raise KeyError(f"Unknown field path {field!r}.")


def load_expected(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root or find_repo_root(Path(__file__))
    configure_imports(repo_root)

    from cfast_reader import read_cfast_input
    from units import display_value, unit_label, unit_system

    units_dir = repo_root / "Verification" / "Units"
    expected = args.expected or units_dir / f"units_ui_expected_{args.profile}.csv"
    unit_system.set_indices(PROFILES[args.profile])

    rows = load_expected(expected)
    cases = {}
    actual_rows = []
    failures = []

    for row in rows:
        case_name = row["case"]
        field = row["field"]
        if case_name not in cases:
            cases[case_name] = read_cfast_input(units_dir / f"{case_name}.in")

        quantity = resolve_field(cases[case_name], field)
        actual = display_value(quantity.kind, quantity.value)
        actual_unit = unit_label(quantity.kind)
        expected_value = float(row["expected_value"])
        expected_unit = row["unit"]
        tolerance = float(row.get("tolerance") or 1.0e-9)
        error = abs(actual - expected_value)
        status = "PASS" if error <= tolerance and actual_unit == expected_unit else "FAIL"

        actual_row = {
            "case": case_name,
            "field": field,
            "actual_value": f"{actual:.12g}",
            "unit": actual_unit,
            "expected_value": f"{expected_value:.12g}",
            "expected_unit": expected_unit,
            "error": f"{error:.12g}",
            "tolerance": f"{tolerance:.12g}",
            "status": status,
        }
        actual_rows.append(actual_row)

        if status != "PASS":
            failures.append(actual_row)

    if args.write_actual is not None:
        args.write_actual.parent.mkdir(parents=True, exist_ok=True)
        with args.write_actual.open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=list(actual_rows[0]))
            writer.writeheader()
            writer.writerows(actual_rows)

    if failures:
        print(f"{len(failures)} CEditQt unit checks failed:")
        for failure in failures[:20]:
            print(
                f"  {failure['case']} {failure['field']}: "
                f"actual {failure['actual_value']} {failure['unit']}, "
                f"expected {failure['expected_value']} {failure['expected_unit']}"
            )
        return 1

    print(f"{len(actual_rows)} CEditQt unit checks passed for profile {args.profile}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

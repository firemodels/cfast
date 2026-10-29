from __future__ import annotations

import argparse
import csv
import itertools
import math
import sys
import tempfile
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
        "spray_density": 0,
    },
    "english": {
        "length": 3,  # ft
        "mass": 2,  # lbm
        "time": 1,  # min
        "temperature": 2,  # F
        "pressure": 3,  # atm
        "energy": 3,  # BTU
        "smoke": 1,  # %/ft
        "spray_density": 2,  # mm/min (independent of length/time)
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
    parser.add_argument("--ui", action="store_true", help="Also check all conversions, editor tabs, and custom dialogs.")
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
        PRESSURE_DIFFERENCE,
        SPRAY_DENSITY,
        RTI,
        SMOKE,
        SPECIFIC_HEAT,
        TEMPERATURE,
        TIME,
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
            "begin_dropoff": ("begin_dropoff", PRESSURE_DIFFERENCE),
            "zero_flow": ("zero_flow", PRESSURE_DIFFERENCE),
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
            "spray_density": ("spray_density", SPRAY_DENSITY),
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
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


# Independent reference data: SI definitions, plus CFAST's established BTU/lbm
# constants. Do not derive expected answers from units.BASE_UNITS or Conversion.
REFERENCE_UNITS = {
    "length": [("m", 1), ("cm", .01), ("mm", .001), ("ft", .3048), ("in", .0254)],
    "mass": [("kg", 1), ("g", .001), ("lbm", .4535924), ("oz", .02834952)],
    "time": [("s", 1), ("min", 60), ("h", 3600)],
    "temperature": [("°C", 1), ("K", 1), ("°F", 5 / 9), ("R", 5 / 9)],
    "pressure": [("Pa", 1), ("mm Hg", 133.3224), ("in H₂O", 249.0889),
                 ("atm", 101325), ("psi", 6894.757293168), ("psia", 6894.757293168),
                 ("psig", 6894.757293168)],
    "energy": [("kJ", 1000), ("J", 1), ("MJ", 1e6), ("BTU", 1054.35), ("cal", 4.184)],
    "smoke": [("%/m", 1), ("%/ft", .3048)],
    "spray_density": [("m/s", 1), ("mm/s", .001), ("mm/min", 1 / 60000),
                      ("L/min/m²", 1 / 60000), ("L/s/m²", .001),
                      ("US gal/min/ft²", .003785411784 / (60 * .3048 ** 2)), ("ft/s", .3048)],
}

# Dimension exponents independently specify each derived quantity. The inventory
# assertion below makes new quantities/choices fail until their coverage is added.
DIMENSIONS = {
    "length": {"length": 1}, "mass": {"mass": 1}, "time": {"time": 1},
    "temperature": {"temperature": 1}, "temperature_rise": {"temperature": 1},
    "pressure": {"pressure": 1}, "pressure_difference": {"pressure": 1},
    "smoke": {"smoke": 1}, "spray_density": {"spray_density": 1},
    "area": {"length": 2}, "velocity": {"length": 1, "time": -1},
    "flowrate": {"length": 3, "time": -1},
    "flow_coefficient": {"length": 3, "time": -1, "pressure": -.5},
    "rti": {"length": .5, "time": .5}, "mass_fraction": {"mass": 0},
    "mass_loss": {"mass": 1, "time": -1}, "density": {"mass": 1, "length": -3},
    "hrr": {"energy": 1, "time": -1}, "hoc": {"energy": 1, "mass": -1},
    "hog": {"energy": 1, "mass": -1}, "specific_extinction": {"length": 2, "mass": -1},
    "heat_flux": {"energy": 1, "time": -1, "length": -2},
    "conductivity": {"energy": 1, "time": -1, "length": -1, "temperature": -1},
    "convection_coefficient": {"energy": 1, "time": -1, "length": -2, "temperature": -1},
    "specific_heat": {"energy": 1, "mass": -1, "temperature": -1},
}
MODEL_KILO = {"hrr", "hoc", "hog", "heat_flux", "convection_coefficient", "specific_heat"}


def reference_value(kind, value, profile):
    units = {key: REFERENCE_UNITS[key][profile.get(key, 0)] for key in REFERENCE_UNITS}
    if kind == "temperature":
        return {"°C": value, "K": value + 273.15, "°F": value * 1.8 + 32,
                "R": (value + 273.15) * 1.8}[units["temperature"][0]]
    if kind == "smoke":
        return 100 * (1 - (1 - value / 100) ** units["smoke"][1])
    if kind == "pressure" and units["pressure"][0] == "psig":
        return (value - 101325) / 6894.757293168
    factor = math.prod(units[key][1] ** power for key, power in DIMENSIONS[kind].items())
    # The convection selector uses W when the power choice is kJ/s.
    if kind == "convection_coefficient" and units["energy"][0] == "kJ" and units["time"][0] == "s":
        factor /= 1000
    return value * (1000 if kind in MODEL_KILO else 1) / factor


def reference_label(kind, profile):
    u = {key: REFERENCE_UNITS[key][profile.get(key, 0)][0] for key in REFERENCE_UNITS}
    length, mass, time, temp, pressure, energy = [u[key] for key in
                                                ("length", "mass", "time", "temperature", "pressure", "energy")]
    power = {"J/s": "W", "kJ/s": "kW", "MJ/s": "MW"}.get(f"{energy}/{time}", f"{energy}/{time}")
    difference = "psi" if pressure in ("psig", "psia") else pressure
    labels = {
        **u, "temperature_rise": temp, "pressure_difference": difference,
        "area": f"{length}²", "velocity": f"{length}/{time}", "flowrate": f"{length}³/{time}",
        "flow_coefficient": f"{length}³/({time} √({difference}))", "rti": f"√({length} {time})",
        "mass_fraction": f"{mass}/{mass}", "mass_loss": f"{mass}/{time}", "density": f"{mass}/{length}³",
        "hrr": power, "hoc": f"{energy}/{mass}", "hog": f"{energy}/{mass}",
        "specific_extinction": f"{length}²/{mass}",
        "heat_flux": f"{energy}/({time} {length}²)",
        "conductivity": f"{energy}/({time} {length} {temp})",
        "convection_coefficient": f"{energy}/({time} {length}² {temp})",
        "specific_heat": f"{energy}/({mass} {temp})",
    }
    if "/" not in power:
        labels["conductivity"] = f"{power}/({length} {temp})"
        labels["convection_coefficient"] = f"{'W' if power == 'kW' else power}/({length}² {temp})"
        if length == "m":
            labels["heat_flux"] = f"{power}/m²"
    return labels[kind]


def assert_close(actual, expected, context, *, rel_tol=5e-9, abs_tol=1e-9):
    assert math.isclose(actual, expected, rel_tol=rel_tol, abs_tol=abs_tol), (
        f"{context}: actual {actual!r}, expected {expected!r}")


def check_conversion_matrix():
    from units import UnitSystem, display_value, format_value, model_value, parse_value, unit_system

    assert set(unit_system.conversions) == set(DIMENSIONS), "Update the quantity coverage inventory"
    assert set(unit_system.selected) == set(REFERENCE_UNITS), "Update the base-unit coverage inventory"
    for key, choices in REFERENCE_UNITS.items():
        assert UnitSystem().choices(key) == [label for label, _ in choices], key
    original = dict(unit_system.selected)
    count = 0
    try:
        for kind, dimensions in DIMENSIONS.items():
            keys = list(dimensions)
            for indices in itertools.product(*(range(len(REFERENCE_UNITS[key])) for key in keys)):
                profile = {key: 0 for key in REFERENCE_UNITS}
                profile.update(zip(keys, indices))
                unit_system.set_indices(profile)
                label = reference_label(kind, profile)
                assert unit_system.label(kind) == label, (kind, profile, unit_system.label(kind), label)
                samples = (0, 17.25, 83.5) if kind == "smoke" else (-2.5, 0, 37.125)
                for value in samples:
                    expected = reference_value(kind, value, profile)
                    context = f"{kind}, {label}, model value {value}"
                    assert_close(display_value(kind, value), expected, context)
                    assert_close(model_value(kind, expected), value, context)
                    assert_close(parse_value(kind, str(expected), context), value, context)
                    assert_close(parse_value(kind, f"{expected:.15g} {label}", context), value, context)
                    # Explicit units must work even when the selected profile differs.
                    unit_system.set_indices({key: 0 for key in REFERENCE_UNITS})
                    assert_close(parse_value(kind, f"{expected:.15g} {label}", context), value, context)
                    unit_system.set_indices(profile)
                    assert_close(parse_value(kind, format_value(kind, value), context), value, context,
                                 abs_tol=1e-4 if kind == "pressure" else 1e-7)
                    count += 1
                for invalid in ("", "not a number", "1 incompatible"):
                    try:
                        parse_value(kind, invalid, kind)
                    except ValueError:
                        pass
                    else:
                        raise AssertionError(f"{kind} accepted {invalid!r}")
        print(f"Conversions: {len(DIMENSIONS)} quantities, {count} numerical cases passed "
              "(all dependent unit combinations, explicit and implicit input).", flush=True)
    finally:
        unit_system.set_indices(original)


def ui_profiles():
    defaults = {key: 0 for key in REFERENCE_UNITS}
    profiles = {
        "SI": defaults,
        "English": dict(zip(REFERENCE_UNITS, (3, 2, 0, 2, 5, 3, 1, 5))),
        "Custom": PROFILES["english"],
    }
    for key, choices in REFERENCE_UNITS.items():
        for index, (label, _) in enumerate(choices[1:], 1):
            profiles[f"{key}={label}"] = {**defaults, key: index}
    # Also exercise interactions across simultaneous non-default choices in Qt.
    for index in range(7):
        profiles[f"mixed-{index}"] = {key: index % len(choices) for key, choices in REFERENCE_UNITS.items()}
    return profiles


def assert_same_case(expected, actual, path="case", *, rel_tol=2e-8):
    if isinstance(expected, dict):
        assert expected.keys() == actual.keys(), path
        for key in expected:
            # These are UI bookkeeping, not input quantities.
            if key not in {"fire_count", "hvent_count", "vent_count", "mechanical_count", "detector_count", "target_count"}:
                assert_same_case(expected[key], actual[key], f"{path}.{key}", rel_tol=rel_tol)
    elif isinstance(expected, (list, tuple)):
        assert len(expected) == len(actual), path
        for index, (left, right) in enumerate(zip(expected, actual)):
            assert_same_case(left, right, f"{path}[{index}]", rel_tol=rel_tol)
    elif isinstance(expected, float):
        assert_close(actual, expected, path, rel_tol=rel_tol, abs_tol=1e-7)
    else:
        assert expected == actual, (path, expected, actual)


def check_ui_matrix(repo_root):
    import ast
    import copy
    import os
    from dataclasses import asdict, replace
    from unittest.mock import patch

    os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
    os.environ.setdefault("MPLBACKEND", "Agg")
    os.environ.setdefault("MPLCONFIGDIR", str(Path(tempfile.gettempdir()) / "cedit-qt-matplotlib"))
    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import (
        QApplication, QDialogButtonBox, QMessageBox, QPushButton, QStyleOptionViewItem, QTableWidget,
    )
    from cfast_case import OutputVisualization
    from cfast_reader import read_cfast_input
    from cfast_writer import write_cfast_input
    from main_window import CeditMainWindow, EngineeringUnitsDialog, RunMonitorDialog
    from table_widgets import UnitLineEdit
    from tabs.fires_tab import FireImportDialog
    from tabs.thermal_properties_tab import ThermalPropertyImportDialog
    from units import unit_system

    app = QApplication.instance() or QApplication([])
    original = dict(unit_system.selected)
    source = repo_root / "Source/CeditQt"
    # A new custom dialog must be added here and exercised below.
    dialogs = {node.name for path in source.rglob("*.py") for node in ast.walk(ast.parse(path.read_text(encoding="utf-8")))
               if isinstance(node, ast.ClassDef) and any(isinstance(base, ast.Name) and base.id == "QDialog"
                                                        for base in node.bases)}
    assert dialogs == {"EngineeringUnitsDialog", "RunMonitorDialog", "FireImportDialog", "ThermalPropertyImportDialog"}, dialogs
    units_dir = repo_root / "Verification/Units"
    case = read_cfast_input(units_dir / "units_vents_devices.in")
    fire_case = read_cfast_input(units_dir / "units_fire.in")
    case.fires = fire_case.fires
    case.fire_properties = fire_case.fire_properties
    case.output_visualizations = fire_case.output_visualizations + [OutputVisualization("ISOF", "All", "X", 100)]
    case.max_time_step = 1.25
    case.compartments[0].cross_section_heights = [0, 1.5, 3]
    case.compartments[0].cross_section_areas = [20, 18, 16]
    case.targets[0].convection_coefficient_front = .01
    case.targets[0].convection_coefficient_back = .02
    case.targets[0].thickness = .12
    case.detection_devices.append(replace(case.detection_devices[0], id="Heat", device_type="HEAT_DETECTOR"))
    covered_edits, covered_tabs, table_checks = set(), set(), 0
    covered_columns = {}
    profiles = ui_profiles()

    def unexpected_message(*args, **kwargs):
        raise AssertionError(f"Unexpected UI error: {args[1:]}")

    with tempfile.TemporaryDirectory(prefix="cedit-units-matrix-") as directory, \
            patch.object(QMessageBox, "critical", side_effect=unexpected_message), \
            patch.object(QMessageBox, "warning", side_effect=unexpected_message):
        # Avoid writing real application preferences, including during construction.
        settings = QSettings(str(Path(directory) / "settings.ini"), QSettings.Format.IniFormat)
        with patch("main_window.QSettings", return_value=settings):
            window = CeditMainWindow()
        try:
            unit_system.set_indices({key: 0 for key in REFERENCE_UNITS})
            window.load_case(copy.deepcopy(case))
            case = window.build_cfast_case()  # Normalize reader/editor representation once.
            baseline = asdict(case)
            window.engineering_units_dialog = EngineeringUnitsDialog(window)
            reset = window.engineering_units_dialog
            reset.apply_requested.connect(window.apply_engineering_units)
            monitor = RunMonitorDialog(window, "units", 300)
            window.run_monitor_dialog = monitor
            monitor.update_status("Status at T = 120 DT = 0.5\n1 100 20 1.5 0.02 250000 100")
            # A partial update must not discard the last complete summary on reset.
            monitor.update_status("Status at T = 150 DT = 0.25")
            apply = reset.findChild(QDialogButtonBox).button(QDialogButtonBox.StandardButton.Apply)
            assert reset.si_button.text() == "Reset to SI"
            assert reset.english_button.text() == "Reset to English"

            def check_text(text, kind, value, context, *, suffix=True, rel_tol=5e-9, abs_tol=1e-9):
                expected = reference_value(kind, value, profile)
                number, _, unit = text.partition(" ")
                assert_close(float(number), expected, f"{profile_name}: {context}", rel_tol=rel_tol, abs_tol=abs_tol)
                if suffix:
                    assert unit == reference_label(kind, profile), (profile_name, context, text, kind)

            def field(tab, name, kind, value, *, suffix=True, rel_tol=5e-9, abs_tol=1e-9):
                edit = getattr(tab, name)
                context = f"{type(tab).__name__}.{name}"
                covered_edits.add(edit)
                covered_tabs.add(tab)
                check_text(edit.text(), kind, value, context, suffix=suffix, rel_tol=rel_tol, abs_tol=abs_tol)

            def attributes(tab, obj, specs):
                for edit, attr, kind in specs:
                    field(tab, edit + "_edit", kind, getattr(obj, attr))

            def cell(table, row, col, kind, value, *, suffix=True, header=True, abs_tol=1e-9):
                nonlocal table_checks
                item = table.item(row, col)
                assert item is not None, (type(table).__name__, row, col)
                context = f"{type(table.parent()).__name__} table[{row},{col}]"
                check_text(item.text(), kind, value, context, suffix=suffix, abs_tol=abs_tol)
                if header:
                    assert reference_label(kind, profile) in table.horizontalHeaderItem(col).text(), context
                option = QStyleOptionViewItem()
                table.itemDelegate().initStyleOption(option, table.model().index(row, col))
                check_text(option.text, kind, value, context + " rendered", suffix=False, rel_tol=5e-5, abs_tol=max(abs_tol, 1e-8))
                table_checks += 1
                covered_columns.setdefault(table, set()).add(col)

            for profile_name, profile in profiles.items():
                print(f"UI units: {profile_name}", flush=True)
                # Exercise Apply itself, then every tab's displayed values and save path.
                for key, combo in reset.combos.items():
                    combo.setCurrentIndex(profile.get(key, 0))
                if profile_name == "SI":
                    reset.si_button.click()
                elif profile_name == "English":
                    reset.english_button.click()
                else:
                    apply.click()
                assert unit_system.selected == {key: profile.get(key, 0) for key in REFERENCE_UNITS}
                # The open monitor must update immediately, before another status poll.
                check_text(monitor.time_edit.text(), "time", 150, "open monitor time after Apply")
                check_text(monitor.dt_edit.text(), "time", .25, "open monitor step after Apply")
                cell(monitor.summary_table, 0, 5, "hrr", 250, suffix=False)
                cell(monitor.summary_table, 0, 6, "pressure_difference", 100, suffix=False)
                sim = window.simulation_tab
                for attr in ("simulation_time", "print_interval", "spreadsheet_interval", "smokeview_interval", "max_time_step"):
                    field(sim, attr + "_edit", "time", getattr(case, attr))
                for attr in ("interior_temperature", "exterior_temperature"):
                    field(sim, attr + "_edit", "temperature", getattr(case, attr))
                field(sim, "pressure_edit", "pressure", case.pressure, rel_tol=5e-5, abs_tol=.0051)
                check_text(sim.pressure_edit.toolTip(), "pressure", case.pressure, "pressure tooltip")
                for attr in ("interior_o2_mass_fraction", "exterior_o2_mass_fraction", "lower_oxygen_limit"):
                    field(sim, attr + "_edit", "mass_fraction", getattr(case, attr))
                for name, value in zip(("flaming", "smoldering"), case.specific_extinction):
                    field(sim, name + "_extinction_edit", "specific_extinction", value, rel_tol=5e-5)
                covered_tabs.add(window.thermal_properties_tab)
                for row, material in enumerate(case.materials):
                    for col, (attr, kind) in enumerate((("conductivity", "conductivity"), ("specific_heat", "specific_heat"),
                                                       ("density", "density"), ("thickness", "length")), 2):
                        cell(window.thermal_properties_tab.table, row, col, kind, getattr(material, attr))
                tab = window.compartments_tab
                for row, comp in enumerate(case.compartments):
                    tab.summary_table.selectRow(row)
                    attributes(tab, comp, [(a, a, "length") for a in ("width", "depth", "height")] +
                               [(a, "origin_" + a, "length") for a in ("x", "y", "z")] +
                               [("flow_coefficient", "flow_coefficient", "flow_coefficient")])
                    for col, attr in enumerate(("width", "depth", "height", "origin_x", "origin_y", "origin_z"), 2):
                        cell(tab.summary_table, row, col, "length", getattr(comp, attr))
                    for col, attr in ((1, "ceiling_thickness"), (3, "wall_thickness"), (5, "floor_thickness")):
                        for layer, value in enumerate(getattr(comp, attr)):
                            cell(tab.materials_table, layer, col, "length", value)
                    for index, (height, area) in enumerate(zip(comp.cross_section_heights, comp.cross_section_areas)):
                        cell(tab.area_table, index, 0, "length", height)
                        cell(tab.area_table, index, 1, "area", area)
                for tab, vents, specs, columns, schedule in (
                    (window.wall_vents_tab, case.wall_vents,
                     [(a, a, "length") for a in ("bottom", "height", "width", "offset")],
                     [(4, "bottom", "length"), (5, "height", "length"), (6, "width", "length"), (8, "offset", "length")], "schedule_table"),
                    (window.ceiling_floor_vents_tab, case.ceiling_floor_vents,
                     [("area", "area", "area"), ("offset_x", "offset_x", "length"), ("offset_y", "offset_y", "length")],
                     [(5, "area", "area"), (6, "offset_x", "length"), (7, "offset_y", "length")], "fraction_table"),
                    (window.mechanical_vents_tab, case.mechanical_vents,
                     [(a, a, k) for a, k in (("from_area", "area"), ("to_area", "area"), ("from_height", "length"),
                      ("to_height", "length"), ("flow", "flowrate"), ("begin_dropoff", "pressure_difference"),
                      ("zero_flow", "pressure_difference"), ("from_offset_x", "length"), ("from_offset_y", "length"),
                      ("to_offset_x", "length"), ("to_offset_y", "length"), ("filter_time", "time"))],
                     [(3, "from_area", "area"), (4, "from_height", "length"), (7, "to_area", "area"),
                      (8, "to_height", "length"), (10, "flow", "flowrate")], "schedule_table"),
                ):
                    for row, vent in enumerate(vents):
                        tab.summary_table.setCurrentCell(row, 0)
                        tab.summary_table.selectRow(row)
                        attributes(tab, vent, specs)
                        for col, attr, kind in columns:
                            cell(tab.summary_table, row, col, kind, getattr(vent, attr))
                        for index, value in enumerate(vent.t_values):
                            cell(getattr(tab, schedule), index, 0, "time", value)
                tab = window.targets_tab
                for row, target in enumerate(case.targets):
                    tab.summary_table.selectRow(row)
                    attributes(tab, target, [(a, a + "_position", "length") for a in ("x", "y", "z")])
                    for col, attr in enumerate(("x_position", "y_position", "z_position"), 3):
                        cell(tab.summary_table, row, col, "length", getattr(target, attr))
                    field(tab, "thickness_edit", "length", target.thickness or case.materials[0].thickness)
                    field(tab, "temperature_depth_edit", "length", target.temperature_depth)
                    for side in ("front", "back"):
                        field(tab, f"convection_{side}_edit", "convection_coefficient",
                              getattr(target, f"convection_coefficient_{side}"), suffix=False)
                        assert reference_label("convection_coefficient", profile) in getattr(tab, f"convection_{side}_label").text()
                tab = window.detection_suppression_tab
                for row, device in enumerate(case.detection_devices):
                    tab.select_device(row)
                    attributes(tab, device, [(a, a + "_position", "length") for a in ("x", "y", "z")])
                    for col, attr in enumerate(("x_position", "y_position", "z_position"), 4):
                        cell(tab.summary_table, row, col, "length", getattr(device, attr))
                    smoke = device.device_type == "SMOKE_DETECTOR"
                    attr, kind = ("activation_obscuration", "smoke") if smoke else ("activation_temperature", "temperature")
                    field(tab, attr + "_edit", kind, getattr(device, attr), abs_tol=.0051)
                    cell(tab.summary_table, row, 7, kind, getattr(device, attr), header=False, abs_tol=.0051)
                    if not smoke:
                        field(tab, "rti_edit", "rti", device.rti)
                        cell(tab.summary_table, row, 8, "rti", device.rti)
                    if device.device_type == "SPRINKLER":
                        field(tab, "spray_density_edit", "spray_density", device.spray_density)
                        cell(tab.summary_table, row, 9, "spray_density", device.spray_density)
                tab = window.fires_tab
                for row, fire in enumerate(case.fires):
                    tab.select_fire(row)
                    attributes(tab, fire, [(a, a, kind) for a, kind in (("x_position", "length"), ("y_position", "length"),
                                           ("decay_time", "time"), ("spray_density", "spray_density"), ("setpoint", "time"))])
                    prop = next(p for p in case.fire_properties if p.id == fire.fire_property_id)
                    field(tab, "heat_of_combustion_edit", "hoc", prop.heat_of_combustion)
                    for col, kind, value in ((4, "time", fire.setpoint), (6, "length", fire.x_position),
                                              (7, "length", fire.y_position), (10, "hrr", prop.peak_hrr())):
                        cell(tab.summary_table, row, col, kind, value, header=False)
                    for index, point in enumerate(prop.sorted_ramp()):
                        for col, (attr, kind) in enumerate((("time", "time"), ("hrr", "hrr"), ("height", "length"),
                            ("area", "area"), ("co_yield", "mass_fraction"), ("soot_yield", "mass_fraction"),
                            ("hcn_yield", "mass_fraction"), ("trace_yield", "mass_fraction"))):
                            cell(tab.ramp_table, index, col, kind, getattr(point, attr), suffix=False)
                    axes = tab.plot_canvas.ax
                    assert axes.get_xlabel() == f"Time ({reference_label('time', profile)})"
                    assert axes.get_ylabel() == f"HRR ({reference_label('hrr', profile)})"
                    for point, x, y in zip(prop.sorted_ramp(), axes.lines[0].get_xdata(), axes.lines[0].get_ydata()):
                        assert_close(x, reference_value("time", point.time, profile), "fire plot time")
                        assert_close(y, reference_value("hrr", point.hrr, profile), "fire plot HRR")
                tab = window.output_tab
                for row, vis in enumerate(case.output_visualizations):
                    tab.visual_table.setCurrentCell(row, 1)
                    tab.visual_row_to_editor(row)
                    kind = "temperature" if vis.visualization_type == "ISOF" else "length"
                    cell(tab.visual_table, row, 4, kind, vis.value, header=False)
                    field(tab, "visual_temperature_edit" if kind == "temperature" else "visual_position_edit", kind, vis.value)
                assert_same_case(baseline, asdict(window.build_cfast_case()), profile_name)
                path = Path(directory) / "roundtrip.in"
                write_cfast_input(case, path)
                expected_file = asdict(read_cfast_input(path))
                write_cfast_input(window.build_cfast_case(), path)
                assert_same_case(expected_file, asdict(read_cfast_input(path)), profile_name + " saved input", rel_tol=1e-5)

                material_dialog = ThermalPropertyImportDialog(case.materials, window)
                fire_dialog = FireImportDialog([("units_fire.in", prop) for prop in case.fire_properties], window)
                try:
                    for row, material in enumerate(case.materials):
                        for col, kind, value in ((3, "conductivity", material.conductivity), (4, "specific_heat", material.specific_heat),
                                                (5, "density", material.density), (6, "length", material.thickness)):
                            cell(material_dialog.table, row, col, kind, value)
                    for row, prop in enumerate(case.fire_properties):
                        cell(fire_dialog.table, row, 3, "hrr", prop.peak_hrr())
                        cell(fire_dialog.table, row, 4, "hoc", prop.heat_of_combustion)
                    for dialog, selected, expected in ((material_dialog, material_dialog.selected_materials, case.materials),
                                                      (fire_dialog, fire_dialog.selected_properties, case.fire_properties)):
                        for title, result in (("Deselect All", []), ("Select All", expected)):
                            next(b for b in dialog.findChildren(QPushButton) if b.text() == title).click()
                            assert selected() == result
                        next(b for b in dialog.findChildren(QPushButton) if b.text() == "OK").click()
                        assert dialog.result() == dialog.DialogCode.Accepted
                        next(b for b in dialog.findChildren(QPushButton) if b.text() == "Cancel").click()
                        assert dialog.result() == dialog.DialogCode.Rejected
                    monitor.update_status("Status at T = 120 DT = 0.5\n1 100 20 1.5 0.02 250000 100")
                    check_text(monitor.time_edit.text(), "time", 120, "monitor time")
                    check_text(monitor.dt_edit.text(), "time", .5, "monitor step")
                    assert monitor.progress_bar.value() == 40
                    for col, kind, value in ((1, "temperature", 100), (2, "temperature", 20), (3, "length", 1.5),
                                            (4, "mass_loss", .02), (5, "hrr", 250), (6, "pressure_difference", 100)):
                        cell(monitor.summary_table, 0, col, kind, value, suffix=False)
                finally:
                    for dialog in (material_dialog, fire_dialog):
                        dialog.deleteLater()
                monitor.update_status("Status at T = 150 DT = 0.25")
                # Conditional setpoints use temperature/heat flux; time-controlled
                # vent schedules were checked above. Exercise both detail and save.
                for criterion, kind, value in (("TEMPERATURE", "temperature", 80), ("FLUX", "heat_flux", 2.5)):
                    variant = copy.deepcopy(case)
                    for objects in (variant.wall_vents, variant.ceiling_floor_vents, variant.mechanical_vents, variant.fires):
                        for obj in objects:
                            if hasattr(obj, "ignition_criterion"):
                                obj.ignition_criterion = criterion
                            else:
                                obj.criterion = criterion
                            obj.setpoint = value
                            obj.target = variant.targets[0].id
                    window.load_case(variant)
                    for tab in (window.wall_vents_tab, window.ceiling_floor_vents_tab, window.mechanical_vents_tab, window.fires_tab):
                        field(tab, "setpoint_edit", kind, value, suffix=tab is window.fires_tab)
                        if tab is not window.fires_tab:
                            assert reference_label(kind, profile) in tab.setpoint_label.text()
                        # Bare input must use the current selection, not SI by accident.
                        tab.setpoint_edit.setText(f"{reference_value(kind, value + 1, profile):.15g}")
                        tab.setpoint_edit.editingFinished.emit()
                    saved = window.build_cfast_case()
                    for objects in (saved.wall_vents, saved.ceiling_floor_vents, saved.mechanical_vents, saved.fires):
                        assert_close(objects[0].setpoint, value + 1, f"{profile_name}: {criterion} edited setpoint")
                window.load_case(copy.deepcopy(case))
                app.processEvents()

            # Completed monitors retain their completion message when units change.
            monitor.set_finished(True, "No Errors")
            reset.si_button.click()
            assert monitor.dt_edit.text() == "CFAST finished"
            assert monitor.time_edit.text() == "150 s"
            assert monitor.summary_table.horizontalHeaderItem(5).text().endswith("(kW)")
            assert monitor.summary_table.item(0, 5).text() == "250"
            # Explicit table inventories make added tables/columns require a coverage decision.
            table_inventory = {
                window.thermal_properties_tab: {"table": (8, {2, 3, 4, 5})},
                window.compartments_tab: {"summary_table": (17, set(range(2, 8))),
                                          "materials_table": (6, {1, 3, 5}), "area_table": (2, {0, 1})},
                window.wall_vents_tab: {"summary_table": (9, {4, 5, 6, 8}), "schedule_table": (2, {0})},
                window.ceiling_floor_vents_tab: {"summary_table": (8, {5, 6, 7}), "fraction_table": (2, {0})},
                window.mechanical_vents_tab: {"summary_table": (11, {3, 4, 7, 8, 10}), "schedule_table": (2, {0})},
                window.targets_tab: {"summary_table": (11, {3, 4, 5})},
                window.detection_suppression_tab: {"summary_table": (10, set(range(4, 10)))},
                window.fires_tab: {"summary_table": (11, {4, 6, 7, 10}), "ramp_table": (8, set(range(8)))},
                window.output_tab: {"visual_table": (5, {4}), "resolution_table": (4, set())},
                window.simulation_tab: {},
            }
            for tab, tables in table_inventory.items():
                actual = {name for name, widget in vars(tab).items() if isinstance(widget, QTableWidget)}
                assert actual == tables.keys(), f"Update table coverage: {type(tab).__name__}: {actual}"
                for name, (count, columns) in tables.items():
                    table = getattr(tab, name)
                    assert table.columnCount() == count, f"Update column coverage: {type(tab).__name__}.{name}"
                    assert covered_columns.get(table, set()) == columns, (type(tab).__name__, name)
            # Inventory guards keep future tabs/quantity editors from silently escaping the suite.
            dimensionless = {
                window.simulation_tab: ("relative_humidity_edit",),
                window.compartments_tab: ("wall_leak_area_ratio_edit", "floor_leak_area_ratio_edit", "ceiling_leak_area_ratio_edit"),
                window.wall_vents_tab: ("pre_fraction_edit", "post_fraction_edit"),
                window.ceiling_floor_vents_tab: ("pre_fraction_edit", "post_fraction_edit"),
                window.mechanical_vents_tab: ("pre_fraction_edit", "post_fraction_edit", "filter_efficiency_edit"),
                window.fires_tab: ("radiative_fraction_edit",),
            }
            exempt = {getattr(tab, name) for tab, names in dimensionless.items() for name in names}
            for index in range(window.tabs.count()):
                tab = window.tabs.widget(index)
                assert tab in covered_tabs, f"Missing tab coverage: {window.tabs.tabText(index)}"
                for name, widget in vars(tab).items():
                    if isinstance(widget, UnitLineEdit):
                        assert widget in covered_edits | exempt, f"Missing unit field coverage: {type(tab).__name__}.{name}"
            print(f"UI units: {len(profiles)} profiles, {len(covered_tabs)} tabs, {len(dialogs)} custom dialogs, "
                  f"{len(covered_edits)} unit fields, {table_checks} table cells passed.", flush=True)
        finally:
            window.hide()
            window.deleteLater()
            unit_system.set_indices(original)


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
        with args.write_actual.open("w", newline="", encoding="utf-8") as stream:
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

    print(f"{len(actual_rows)} CEditQt unit checks passed for profile {args.profile}.", flush=True)
    if args.ui:
        check_conversion_matrix()
        check_ui_matrix(repo_root)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

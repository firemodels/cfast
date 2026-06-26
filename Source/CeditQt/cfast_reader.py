from __future__ import annotations

from pathlib import Path
from typing import Any

import f90nml

from cfast_case import (
    CeilingFloorSurfaceConnection,
    CeilingFloorVent,
    CfastCase,
    Compartment,
    DetectionDevice,
    FireDefinition,
    FireProperty,
    FireRampPoint,
    MaterialProperty,
    MechanicalVent,
    OutputVisualization,
    Target,
    WallSurfaceConnection,
    WallVent,
)


class CfastNamelistReadError(ValueError):
    """Raised when a CFAST namelist record cannot be read."""


def read_cfast_input(path: str | Path) -> CfastCase:
    """Read a CFAST namelist input file into the CEdit Qt case model.

    The Fortran namelist grammar is delegated to f90nml.  This module only does
    the CFAST-specific work that f90nml cannot do by itself: preserving repeated
    records in file order and mapping CFAST records into the CEdit Qt dataclasses.
    """
    text = Path(path).read_text(encoding="utf-8", errors="replace")
    case = CfastCase()
    fire_properties: dict[str, FireProperty] = {}
    material_thickness: dict[str, float] = {}

    for name, fields in parse_namelists(text):
        name = name.upper()

        if name == "HEAD":
            case.version = as_int(first(fields, "VERSION", case.version), case.version)
            case.title = as_str(first(fields, "TITLE", case.title), case.title)

        elif name == "TIME":
            case.simulation_time = as_float(first(fields, "SIMULATION", case.simulation_time), case.simulation_time)
            case.print_interval = as_float(first(fields, "PRINT", case.print_interval), case.print_interval)
            case.smokeview_interval = as_float(first(fields, "SMOKEVIEW", case.smokeview_interval), case.smokeview_interval)
            case.spreadsheet_interval = as_float(first(fields, "SPREADSHEET", case.spreadsheet_interval), case.spreadsheet_interval)

        elif name == "INIT":
            case.pressure = as_float(first(fields, "PRESSURE", case.pressure), case.pressure)
            case.relative_humidity = as_float(first(fields, "RELATIVE_HUMIDITY", case.relative_humidity), case.relative_humidity)
            case.interior_temperature = as_float(first(fields, "INTERIOR_TEMPERATURE", case.interior_temperature), case.interior_temperature)
            case.exterior_temperature = as_float(first(fields, "EXTERIOR_TEMPERATURE", case.exterior_temperature), case.exterior_temperature)

        elif name == "MISC":
            case.adiabatic_surfaces = as_bool(first(fields, "ADIABATIC", case.adiabatic_surfaces), case.adiabatic_surfaces)
            case.lower_oxygen_limit = as_float(first(fields, "LOWER_OXYGEN_LIMIT", case.lower_oxygen_limit), case.lower_oxygen_limit)
            if "MAX_TIME_STEP" in fields:
                case.max_time_step = as_float(first(fields, "MAX_TIME_STEP", 0.0), 0.0)

        elif name == "MATL":
            material = MaterialProperty(
                id=as_str(first(fields, "ID", "")),
                material=as_str(first(fields, "MATERIAL", first(fields, "ID", ""))),
                conductivity=as_float(first(fields, "CONDUCTIVITY", 0.0), 0.0),
                specific_heat=as_float(first(fields, "SPECIFIC_HEAT", 0.0), 0.0),
                density=as_float(first(fields, "DENSITY", 0.0), 0.0),
                thickness=as_float(first(fields, "THICKNESS", 0.0), 0.0),
                emissivity=as_float(first(fields, "EMISSIVITY", 0.9), 0.9),
                fyi=as_str(first(fields, "FYI", "")),
            )
            if material.id:
                case.materials.append(material)
                material_thickness[material.id.upper()] = material.thickness

        elif name == "COMP":
            origin = as_float_list(fields.get("ORIGIN", []), [0.0, 0.0, 0.0], 3)
            grid = [as_int(value, 50) for value in expand_values(fields.get("GRID", []), 3, 50)]
            leak_area_ratio = as_float_list(fields.get("LEAK_AREA_RATIO", []), [0.00017, 5.2e-5], 2)
            leak_area = as_float_list(fields.get("LEAK_AREA", []), [0.0, 0.0], 2)
            compartment = Compartment(
                id=as_str(first(fields, "ID", f"Comp {len(case.compartments) + 1}")),
                width=as_float(first(fields, "WIDTH", 1.0), 1.0),
                depth=as_float(first(fields, "DEPTH", 1.0), 1.0),
                height=as_float(first(fields, "HEIGHT", 1.0), 1.0),
                origin_x=origin[0],
                origin_y=origin[1],
                origin_z=origin[2],
                ceiling_matl_id=as_str_tuple(fields.get("CEILING_MATL_ID", ["OFF"]), "OFF"),
                ceiling_thickness=as_float_tuple(fields.get("CEILING_THICKNESS", [0.0]), 0.0),
                wall_matl_id=as_str_tuple(fields.get("WALL_MATL_ID", ["OFF"]), "OFF"),
                wall_thickness=as_float_tuple(fields.get("WALL_THICKNESS", [0.0]), 0.0),
                floor_matl_id=as_str_tuple(fields.get("FLOOR_MATL_ID", ["OFF"]), "OFF"),
                floor_thickness=as_float_tuple(fields.get("FLOOR_THICKNESS", [0.0]), 0.0),
                grid=(grid[0], grid[1], grid[2]),
                hall=as_bool(first(fields, "HALL", False), False),
                shaft=as_bool(first(fields, "SHAFT", False), False),
                flow_coefficient=as_float(first(fields, "FLOW_COEFFICIENT", 0.07), 0.07),
                wall_leak_area_ratio=leak_area_ratio[0],
                floor_leak_area_ratio=leak_area_ratio[1],
                wall_leak_area=leak_area[0],
                floor_leak_area=leak_area[1],
                fyi=as_str(first(fields, "FYI", "")),
            )
            case.compartments.append(compartment)

        elif name == "VENT":
            vent_type = as_str(first(fields, "TYPE", "WALL"), "WALL").upper()
            comp_ids = [as_str(value) for value in fields.get("COMP_IDS", [])]
            if len(comp_ids) < 2:
                comp_ids = ["", "OUTSIDE"]
            t_values = [as_float(value, 0.0) for value in fields.get("T", [])]
            f_values = [as_float(value, 0.0) for value in fields.get("F", [])]

            if vent_type == "WALL":
                initial_open = f_values[0] if len(t_values) == 1 and abs(t_values[0]) < 1.0e-12 else 1.0
                case.wall_vents.append(
                    WallVent(
                        id=as_str(first(fields, "ID", f"WallVent_{len(case.wall_vents) + 1}")),
                        first_comp_id=comp_ids[0],
                        second_comp_id=normalize_outside(comp_ids[1]),
                        bottom=as_float(first(fields, "BOTTOM", 0.0), 0.0),
                        height=as_float(first(fields, "HEIGHT", 2.0), 2.0),
                        width=as_float(first(fields, "WIDTH", 1.0), 1.0),
                        initial_open=initial_open,
                        face=as_str(first(fields, "FACE", "FRONT"), "FRONT").upper(),
                        offset=as_float(first(fields, "OFFSET", 0.0), 0.0),
                        criterion=as_str(first(fields, "CRITERION", "TIME"), "TIME").upper(),
                        t_values=t_values,
                        f_values=f_values,
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )
            elif vent_type in {"CEILING", "FLOOR"}:
                offsets = as_float_list(fields.get("OFFSETS", []), [0.0, 0.0], 2)
                case.ceiling_floor_vents.append(
                    CeilingFloorVent(
                        id=as_str(first(fields, "ID", f"CFVent_{len(case.ceiling_floor_vents) + 1}")),
                        first_comp_id=comp_ids[0],
                        second_comp_id=comp_ids[1],
                        vent_type=vent_type,
                        area=as_float(first(fields, "AREA", 1.0), 1.0),
                        shape=as_str(first(fields, "SHAPE", "ROUND"), "ROUND").upper(),
                        initial_open=f_values[0] if len(t_values) == 1 and abs(t_values[0]) < 1.0e-12 else 1.0,
                        offset_x=offsets[0],
                        offset_y=offsets[1],
                        criterion=as_str(first(fields, "CRITERION", "TIME"), "TIME").upper(),
                        t_values=t_values,
                        f_values=f_values,
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )
            elif vent_type == "MECHANICAL":
                areas = as_float_list(fields.get("AREAS", []), [0.25, 0.25], 2)
                heights = as_float_list(fields.get("HEIGHTS", []), [2.75, 2.75], 2)
                orientations = [as_str(value) for value in expand_values(fields.get("ORIENTATIONS", []), 2, "VERTICAL")]
                cutoffs = as_float_list(fields.get("CUTOFFS", []), [200.0, 300.0], 2)
                offsets = as_float_list(fields.get("OFFSETS", []), [0.0, 0.0], 2)
                case.mechanical_vents.append(
                    MechanicalVent(
                        id=as_str(first(fields, "ID", f"MechanicalVent_{len(case.mechanical_vents) + 1}")),
                        from_comp_id=normalize_outside(comp_ids[0]),
                        to_comp_id=normalize_outside(comp_ids[1]),
                        from_area=areas[0],
                        to_area=areas[1],
                        from_height=heights[0],
                        to_height=heights[1],
                        from_orientation=orientations[0].upper(),
                        to_orientation=orientations[1].upper(),
                        flow=as_float(first(fields, "FLOW", 0.02), 0.02),
                        begin_dropoff=cutoffs[0],
                        zero_flow=cutoffs[1],
                        offset_x=offsets[0],
                        offset_y=offsets[1],
                        filter_efficiency=as_float(first(fields, "FILTER_EFFICIENCY", 0.0), 0.0),
                        filter_time=as_float(first(fields, "FILTER_TIME", 0.0), 0.0),
                        criterion=as_str(first(fields, "CRITERION", "TIME"), "TIME").upper(),
                        t_values=t_values,
                        f_values=f_values,
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )

        elif name == "FIRE":
            location = as_float_list(fields.get("LOCATION", []), [2.5, 2.5], 2)
            case.fires.append(
                FireDefinition(
                    id=as_str(first(fields, "ID", f"Fire_{len(case.fires) + 1}")),
                    comp_id=as_str(first(fields, "COMP_ID", "")),
                    fire_property_id=as_str(first(fields, "FIRE_ID", "")),
                    ignition_criterion=as_str(first(fields, "IGNITION_CRITERION", "TIME"), "TIME").upper(),
                    setpoint=as_float(first(fields, "SETPOINT", 0.0), 0.0),
                    target=as_str(first(fields, "DEVC_ID", "")),
                    x_position=location[0],
                    y_position=location[1],
                    fyi=as_str(first(fields, "FYI", "")),
                )
            )

        elif name == "CHEM":
            prop = FireProperty(
                id=as_str(first(fields, "ID", f"FireProp_{len(fire_properties) + 1}")),
                carbon=as_int(first(fields, "CARBON", 1), 1),
                hydrogen=as_int(first(fields, "HYDROGEN", 4), 4),
                oxygen=as_int(first(fields, "OXYGEN", 0), 0),
                nitrogen=as_int(first(fields, "NITROGEN", 0), 0),
                chlorine=as_int(first(fields, "CHLORINE", 0), 0),
                heat_of_combustion=as_float(first(fields, "HEAT_OF_COMBUSTION", 50000.0), 50000.0),
                radiative_fraction=as_float(first(fields, "RADIATIVE_FRACTION", 0.35), 0.35),
                fyi=as_str(first(fields, "FYI", "")),
            )
            fire_properties[prop.id] = prop

        elif name == "TABL":
            table_id = as_str(first(fields, "ID", ""))
            if not table_id or "DATA" not in fields:
                continue
            prop = fire_properties.setdefault(table_id, FireProperty(id=table_id))
            data = [as_float(value, 0.0) for value in fields["DATA"]]
            if len(data) >= 2:
                prop.ramp.append(
                    FireRampPoint(
                        time=data[0],
                        hrr=data[1],
                        height=data[2] if len(data) > 2 else 0.0,
                        area=data[3] if len(data) > 3 else 0.1,
                        co_yield=data[4] if len(data) > 4 else 0.0,
                        soot_yield=data[5] if len(data) > 5 else 0.01,
                        hcn_yield=data[6] if len(data) > 6 else 0.0,
                        trace_yield=data[8] if len(data) > 8 else (data[7] if len(data) > 7 else 0.0),
                    )
                )

        elif name == "DEVC":
            devc_type = as_str(first(fields, "TYPE", ""), "").upper()
            location = as_float_list(fields.get("LOCATION", []), [0.0, 0.0, 0.0], 3)
            if devc_type in {"PLATE", "CYLINDER"}:
                orientation = as_str(first(fields, "SURFACE_ORIENTATION", ""))
                normal = as_float_list(fields.get("NORMAL", []), normal_from_orientation(orientation), 3)
                matl_id = as_str(first(fields, "MATL_ID", "OFF"), "OFF")
                default_thickness = material_thickness.get(matl_id.upper(), 0.0)
                thickness = as_float(first(fields, "THICKNESS", default_thickness), default_thickness)
                case.targets.append(
                    Target(
                        id=as_str(first(fields, "ID", f"Target_{len(case.targets) + 1}")),
                        comp_id=as_str(first(fields, "COMP_ID", "")),
                        x_position=location[0],
                        y_position=location[1],
                        z_position=location[2],
                        x_normal=normal[0],
                        y_normal=normal[1],
                        z_normal=normal[2],
                        matl_id=matl_id,
                        target_type=devc_type,
                        thickness=thickness,
                        temperature_depth=as_float(first(fields, "TEMPERATURE_DEPTH", 0.0), 0.0),
                        depth_units=as_str(first(fields, "DEPTH_UNITS", "DISTANCE"), "DISTANCE").upper(),
                        surface_orientation=as_str(first(fields, "SURFACE_ORIENTATION", "USER SPECIFIED"), "USER SPECIFIED"),
                        surface_temperature=as_optional_float(first(fields, "SURFACE_TEMPERATURE", None)),
                        adiabatic=as_bool(first(fields, "ADIABATIC_TARGET", False), False),
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )
            elif devc_type:
                case.detection_devices.append(
                    DetectionDevice(
                        id=as_str(first(fields, "ID", f"Device_{len(case.detection_devices) + 1}")),
                        comp_id=as_str(first(fields, "COMP_ID", "")),
                        device_type=devc_type,
                        x_position=location[0],
                        y_position=location[1],
                        z_position=location[2],
                        activation_temperature=as_float(first(fields, "SETPOINT", 73.88998), 73.88998),
                        rti=as_float(first(fields, "RTI", 100.0), 100.0),
                        spray_density=as_float(first(fields, "SPRAY_DENSITY", 0.0), 0.0),
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )

        elif name == "CONN":
            conn_type = as_str(first(fields, "TYPE", "WALL"), "WALL").upper()
            first_comp = as_str(first(fields, "COMP_ID", ""))
            second_values = fields.get("COMP_IDS", [])
            second_comp = as_str(second_values[0]) if second_values else ""
            if conn_type == "WALL":
                case.wall_surface_connections.append(
                    WallSurfaceConnection(
                        first_comp_id=first_comp,
                        second_comp_id=second_comp,
                        fraction=as_float(first(fields, "F", 1.0), 1.0),
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )
            else:
                case.ceiling_floor_surface_connections.append(
                    CeilingFloorSurfaceConnection(
                        top_comp_id=first_comp,
                        bottom_comp_id=second_comp,
                        fyi=as_str(first(fields, "FYI", "")),
                    )
                )

        elif name == "SLCF":
            domain = as_str(first(fields, "DOMAIN", "2-D"), "2-D").upper()
            case.output_visualizations.append(
                OutputVisualization(
                    visualization_type=domain,
                    comp_id=as_str(first(fields, "COMP_ID", "NULL")),
                    axis=as_str(first(fields, "PLANE", "X"), "X").upper(),
                    value=as_float(first(fields, "POSITION", 0.0), 0.0),
                    fyi=as_str(first(fields, "FYI", "")),
                )
            )

    case.fire_properties = list(fire_properties.values())
    return case


def parse_namelists(text: str) -> list[tuple[str, dict[str, list[Any]]]]:
    """Split a CFAST file into records and parse each record with f90nml."""
    records: list[tuple[str, dict[str, list[Any]]]] = []
    current: list[str] = []
    in_record = False

    for raw_line in text.splitlines():
        line = strip_comment(raw_line).strip()
        if not line:
            continue
        if not in_record:
            if not line.lstrip().startswith("&"):
                continue
            in_record = True
            current = [line]
        else:
            current.append(line)

        if record_is_complete("\n".join(current)):
            record = "\n".join(current)
            name, fields = parse_record_with_f90nml(record)
            if name.upper() != "TAIL":
                records.append((name, fields))
            in_record = False
            current = []

    if in_record and current:
        raise CfastNamelistReadError("Unterminated CFAST namelist record near: " + current[0][:80])

    return records


def parse_record_with_f90nml(record: str) -> tuple[str, dict[str, list[Any]]]:
    """Parse one &GROUP ... / record using f90nml and normalize keys/values."""
    parser = f90nml.Parser()
    parser.default_start_index = 1
    try:
        nml = parser.reads(record)
    except Exception as exc:
        raise CfastNamelistReadError(
            "Could not parse CFAST namelist record with f90nml:\n"
            f"{record[:500]}"
        ) from exc

    if len(nml) != 1:
        raise CfastNamelistReadError(
            f"Expected one namelist group per CFAST record, got {list(nml.keys())!r}."
        )

    group_name = next(iter(nml.keys())).upper()
    group_values = nml[next(iter(nml.keys()))]
    fields: dict[str, list[Any]] = {}

    for key, value in group_values.items():
        fields[str(key).upper()] = ensure_list(value)

    return group_name, fields


def ensure_list(value: Any) -> list[Any]:
    """Return f90nml scalar/array values as a simple Python list."""
    if isinstance(value, list):
        return flatten_f90nml_list(value)
    if isinstance(value, tuple):
        return flatten_f90nml_list(list(value))
    return [value]


def flatten_f90nml_list(values: list[Any]) -> list[Any]:
    result: list[Any] = []
    for value in values:
        if isinstance(value, list):
            result.extend(flatten_f90nml_list(value))
        elif isinstance(value, tuple):
            result.extend(flatten_f90nml_list(list(value)))
        else:
            result.append(value)
    return result


def strip_comment(line: str) -> str:
    result: list[str] = []
    quote: str | None = None
    i = 0
    while i < len(line):
        ch = line[i]
        if quote is not None:
            result.append(ch)
            if ch == quote:
                if i + 1 < len(line) and line[i + 1] == quote:
                    result.append(line[i + 1])
                    i += 2
                    continue
                quote = None
            i += 1
            continue
        if ch in {"'", '"'}:
            quote = ch
            result.append(ch)
        elif ch == "!":
            break
        else:
            result.append(ch)
        i += 1
    return "".join(result)


def record_is_complete(record: str) -> bool:
    quote: str | None = None
    i = 0
    while i < len(record):
        ch = record[i]
        if quote is not None:
            if ch == quote:
                if i + 1 < len(record) and record[i + 1] == quote:
                    i += 2
                    continue
                quote = None
        elif ch in {"'", '"'}:
            quote = ch
        elif ch == "/":
            return True
        i += 1
    return False


def first(fields: dict[str, list[Any]], key: str, default: Any = None) -> Any:
    values = fields.get(key.upper())
    if not values:
        return default
    return values[0]


def as_str(value: Any, default: str = "") -> str:
    if value is None:
        return default
    if isinstance(value, bool):
        return ".TRUE." if value else ".FALSE."
    if isinstance(value, float) and abs(value - round(value)) < 1.0e-12:
        return str(int(round(value)))
    return str(value)


def as_float(value: Any, default: float = 0.0) -> float:
    if value is None or value == "":
        return default
    if isinstance(value, bool):
        return 1.0 if value else 0.0
    try:
        return float(str(value).replace("D", "E").replace("d", "e"))
    except ValueError:
        return default


def as_optional_float(value: Any) -> float | None:
    if value is None or value == "":
        return None
    return as_float(value, 0.0)


def as_int(value: Any, default: int = 0) -> int:
    return int(round(as_float(value, float(default))))


def as_bool(value: Any, default: bool = False) -> bool:
    if isinstance(value, bool):
        return value
    if value is None:
        return default
    return str(value).strip().upper() in {".TRUE.", "TRUE", "T", "1", "YES"}


def expand_values(values: list[Any], n: int, default: Any) -> list[Any]:
    if not values:
        values = [default]
    if len(values) == 1:
        values = values * n
    if len(values) < n:
        values = values + [default] * (n - len(values))
    return values[:n]


def as_float_list(values: list[Any], defaults: list[float], n: int) -> list[float]:
    if not values:
        return (defaults + [0.0] * n)[:n]
    expanded = expand_values(values, n, defaults[0] if defaults else 0.0)
    result = []
    for i, value in enumerate(expanded):
        default = defaults[i] if i < len(defaults) else 0.0
        result.append(as_float(value, default))
    return result


def as_str_tuple(values: list[Any], default: str) -> tuple[str, str, str]:
    expanded = expand_values(values, 3, default)
    return (as_str(expanded[0], default), as_str(expanded[1], default), as_str(expanded[2], default))


def as_float_tuple(values: list[Any], default: float) -> tuple[float, float, float]:
    expanded = expand_values(values, 3, default)
    return (as_float(expanded[0], default), as_float(expanded[1], default), as_float(expanded[2], default))


def normalize_outside(value: str) -> str:
    return "OUTSIDE" if value.strip().upper() == "OUTSIDE" else value


def normal_from_orientation(orientation: str) -> list[float]:
    value = orientation.strip().upper()
    if value == "LEFT WALL":
        return [-1.0, 0.0, 0.0]
    if value == "RIGHT WALL":
        return [1.0, 0.0, 0.0]
    if value == "FRONT WALL":
        return [0.0, -1.0, 0.0]
    if value == "REAR WALL":
        return [0.0, 1.0, 0.0]
    if value == "CEILING":
        return [0.0, 0.0, -1.0]
    if value == "FLOOR":
        return [0.0, 0.0, 1.0]
    return [0.0, 0.0, 1.0]

from __future__ import annotations

from pathlib import Path

from cfast_case import CfastCase


def cfast_string(value: str) -> str:
    escaped = value.replace("'", "''")
    return f"'{escaped}'"


def cfast_logical(value: bool) -> str:
    return ".TRUE." if value else ".FALSE."


def cfast_number(value: float | int) -> str:
    if isinstance(value, int):
        return str(value)

    value = float(value)

    if abs(value - round(value)) < 1.0e-12:
        return str(int(round(value)))

    return f"{value:.6g}"


def cfast_vector(values) -> str:
    return ", ".join(cfast_number(value) for value in values)


def cfast_string_vector(values) -> str:
    return ", ".join(cfast_string(value) for value in values)


def spreadsheet_output_code(case: CfastCase) -> str:
    codes = []
    if getattr(case, "spreadsheet_output_compartments", True):
        codes.append("C")
    if getattr(case, "spreadsheet_output_devices", True):
        codes.append("D")
    if getattr(case, "spreadsheet_output_masses", True):
        codes.append("M")
    if getattr(case, "spreadsheet_output_vents", True):
        codes.append("V")
    if getattr(case, "spreadsheet_output_walls", True):
        codes.append("W")

    return "".join(codes) if codes else "NONE"


def add_wrapped_namelist(lines: list[str], name: str, fields: list[str]) -> None:
    max_len = 120
    indent = "      "
    current = f"&{name} "

    for index, field in enumerate(fields):
        suffix = "," if index < len(fields) - 1 else " /"
        entry = f"{field}{suffix}"

        if len(current) + len(entry) + 1 > max_len:
            lines.append(current.rstrip())
            current = indent + entry + " "
        else:
            current = current + entry + " "

    lines.append(current.rstrip())


def scheduled_values(vent) -> tuple[list[float], list[float]]:
    t_values = list(vent.t_values)
    f_values = list(vent.f_values)

    if len(t_values) != len(f_values):
        raise ValueError(f"Vent {vent.id!r}: T and F schedules must match.")

    return t_values, f_values


def wall_vent_schedule(vent) -> tuple[list[float], list[float]]:
    t_values, f_values = scheduled_values(vent)

    if not t_values:
        if abs(vent.initial_open - 1.0) > 1.0e-12:
            return [0.0], [vent.initial_open]
        return [], []

    if abs(t_values[0]) > 1.0e-12:
        t_values.insert(0, 0.0)
        f_values.insert(0, vent.initial_open)

    return t_values, f_values


def validate_fire_property(prop) -> None:
    if prop.heat_of_combustion <= 0.0:
        raise ValueError(
            f"Fire properties {prop.id!r}: heat of combustion must be positive."
        )

    if not 0.0 <= prop.radiative_fraction <= 1.0:
        raise ValueError(
            f"Fire properties {prop.id!r}: radiative fraction must be 0 to 1."
        )

    if not prop.ramp:
        raise ValueError(f"Fire properties {prop.id!r}: HRR table is empty.")

    for point in prop.ramp:
        if point.time < 0.0:
            raise ValueError(f"Fire properties {prop.id!r}: time must be non-negative.")
        if point.hrr < 0.0:
            raise ValueError(f"Fire properties {prop.id!r}: HRR must be non-negative.")
        if point.area < 0.0:
            raise ValueError(f"Fire properties {prop.id!r}: area must be non-negative.")
        if point.height < 0.0:
            raise ValueError(f"Fire properties {prop.id!r}: height must be non-negative.")


def validate_case(case: CfastCase) -> None:
    if not case.compartments:
        raise ValueError("At least one compartment is required.")

    compartment_ids = {compartment.id for compartment in case.compartments}

    for vent in case.wall_vents:
        if vent.first_comp_id not in compartment_ids:
            raise ValueError(
                f"Wall vent {vent.id!r}: first compartment "
                f"{vent.first_comp_id!r} does not exist."
            )
        if vent.second_comp_id != "OUTSIDE" and vent.second_comp_id not in compartment_ids:
            raise ValueError(
                f"Wall vent {vent.id!r}: second compartment "
                f"{vent.second_comp_id!r} does not exist."
            )
        scheduled_values(vent)

    for vent in getattr(case, "ceiling_floor_vents", []):
        if vent.top_comp_id != "OUTSIDE" and vent.top_comp_id not in compartment_ids:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: top compartment "
                f"{vent.top_comp_id!r} does not exist."
            )
        if vent.bottom_comp_id != "OUTSIDE" and vent.bottom_comp_id not in compartment_ids:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: bottom compartment "
                f"{vent.bottom_comp_id!r} does not exist."
            )
        scheduled_values(vent)

    for vent in getattr(case, "mechanical_vents", []):
        if vent.from_comp_id != "OUTSIDE" and vent.from_comp_id not in compartment_ids:
            raise ValueError(
                f"Mechanical vent {vent.id!r}: from compartment "
                f"{vent.from_comp_id!r} does not exist."
            )
        if vent.to_comp_id != "OUTSIDE" and vent.to_comp_id not in compartment_ids:
            raise ValueError(
                f"Mechanical vent {vent.id!r}: to compartment "
                f"{vent.to_comp_id!r} does not exist."
            )
        scheduled_values(vent)

    for target in getattr(case, "targets", []):
        if target.comp_id not in compartment_ids:
            raise ValueError(
                f"Target {target.id!r}: compartment {target.comp_id!r} does not exist."
            )
        if target.target_type.upper() not in {"PLATE", "CYLINDER"}:
            raise ValueError(
                f"Target {target.id!r}: target type must be PLATE or CYLINDER."
            )
        if target.thickness < 0.0:
            raise ValueError(f"Target {target.id!r}: thickness must be non-negative.")
        if target.temperature_depth < 0.0:
            raise ValueError(
                f"Target {target.id!r}: internal temperature depth must be non-negative."
            )

    for device in getattr(case, "detection_devices", []):
        if device.comp_id not in compartment_ids:
            raise ValueError(
                f"Detection device {device.id!r}: compartment "
                f"{device.comp_id!r} does not exist."
            )
        if device.device_type.upper() not in {
            "SPRINKLER",
            "SMOKE_DETECTOR",
            "HEAT_DETECTOR",
        }:
            raise ValueError(
                f"Detection device {device.id!r}: invalid type {device.device_type!r}."
            )

    for conn in getattr(case, "wall_surface_connections", []):
        if conn.first_comp_id not in compartment_ids:
            raise ValueError(
                f"Wall surface connection: first compartment "
                f"{conn.first_comp_id!r} does not exist."
            )
        if conn.second_comp_id not in compartment_ids:
            raise ValueError(
                f"Wall surface connection: second compartment "
                f"{conn.second_comp_id!r} does not exist."
            )
        if not 0.0 <= conn.fraction <= 1.0:
            raise ValueError("Wall surface connection fraction must be 0 to 1.")

    for conn in getattr(case, "ceiling_floor_surface_connections", []):
        if conn.top_comp_id not in compartment_ids:
            raise ValueError(
                f"Ceiling/floor surface connection: top compartment "
                f"{conn.top_comp_id!r} does not exist."
            )
        if conn.bottom_comp_id not in compartment_ids:
            raise ValueError(
                f"Ceiling/floor surface connection: bottom compartment "
                f"{conn.bottom_comp_id!r} does not exist."
            )

    for vis in getattr(case, "output_visualizations", []):
        comp_id = vis.comp_id.strip()
        if comp_id.upper() not in {"ALL", "NULL", ""} and comp_id not in compartment_ids:
            raise ValueError(
                f"Visualization output: compartment {comp_id!r} does not exist."
            )

        vis_type = vis.visualization_type.upper()
        if vis_type not in {"2-D", "3-D"}:
            raise ValueError(
                f"Visualization output: type must be 2-D or 3-D, got "
                f"{vis.visualization_type!r}."
            )

        axis = vis.axis.upper()[0:1]
        if vis_type == "2-D" and axis not in {"X", "Y", "Z"}:
            raise ValueError(
                f"Visualization output: 2-D axis must be X, Y, or Z, got "
                f"{vis.axis!r}."
            )

    property_ids = {prop.id for prop in case.fire_properties}

    for prop in case.fire_properties:
        validate_fire_property(prop)

    if case.fires and not property_ids:
        raise ValueError("At least one fire property definition is required.")

    for fire in case.fires:
        if fire.comp_id not in compartment_ids:
            raise ValueError(
                f"Fire {fire.id!r}: compartment {fire.comp_id!r} does not exist."
            )
        if fire.fire_property_id not in property_ids:
            raise ValueError(
                f"Fire {fire.id!r}: fire properties ID "
                f"{fire.fire_property_id!r} does not exist."
            )

        ignition = fire.ignition_criterion.upper()
        if ignition not in {"TIME", "TEMPERATURE", "FLUX"}:
            raise ValueError(
                f"Fire {fire.id!r}: ignition criterion must be TIME, TEMPERATURE, "
                "or FLUX."
            )


def write_cfast_input(case: CfastCase, path: str | Path) -> None:
    validate_case(case)

    path = Path(path)
    lines: list[str] = []

    add_wrapped_namelist(
        lines,
        "HEAD",
        [
            f"VERSION = {case.version}",
            f"TITLE = {cfast_string(case.title)}",
        ],
    )

    lines.append("!! CFAST input generated by CEdit Qt prototype")
    lines.append("")

    add_wrapped_namelist(
        lines,
        "TIME",
        [
            f"SIMULATION = {cfast_number(case.simulation_time)}",
            f"PRINT = {cfast_number(case.print_interval)}",
            f"SMOKEVIEW = {cfast_number(case.smokeview_interval)}",
            f"SPREADSHEET = {cfast_number(case.spreadsheet_interval)}",
        ],
    )

    add_wrapped_namelist(
        lines,
        "INIT",
        [
            f"PRESSURE = {cfast_number(case.pressure)}",
            f"RELATIVE_HUMIDITY = {cfast_number(case.relative_humidity)}",
            f"INTERIOR_TEMPERATURE = {cfast_number(case.interior_temperature)}",
            f"EXTERIOR_TEMPERATURE = {cfast_number(case.exterior_temperature)}",
        ],
    )

    misc_fields = [
        f"ADIABATIC = {cfast_logical(case.adiabatic_surfaces)}",
        f"LOWER_OXYGEN_LIMIT = {cfast_number(case.lower_oxygen_limit)}",
        "OVERWRITE = .TRUE.",
    ]

    if case.max_time_step is not None:
        misc_fields.insert(1, f"MAX_TIME_STEP = {cfast_number(case.max_time_step)}")

    add_wrapped_namelist(lines, "MISC", misc_fields)
    lines.append("")

    if getattr(case, "debug_output", False):
        add_wrapped_namelist(
            lines,
            "DIAG",
            [
                "DEBUG_PRINT = 'ON'",
                "RESIDUAL_DEBUG_PRINT = 'ON'",
            ],
        )
        lines.append("")

    add_wrapped_namelist(
        lines,
        "OUTP",
        [
            f"NET_HEAT_FLUX_OUTPUT = {cfast_logical(getattr(case, 'net_heat_flux_output', True))}",
            f"VALIDATION_OUTPUT = {cfast_logical(getattr(case, 'validation_output', False))}",
            f"SPREADSHEET_OUTPUT = {cfast_string(spreadsheet_output_code(case))}",
        ],
    )
    lines.append("")

    for extra_namelist in getattr(case, "extra_namelists", []):
        text = extra_namelist.strip()
        if text:
            lines.append(text)
            lines.append("")

    if case.materials:
        lines.append("!! Thermal Properties")
        for material in case.materials:
            fields = [
                f"ID = {cfast_string(material.id)}",
                f"MATERIAL = {cfast_string(material.material)}",
                f"CONDUCTIVITY = {cfast_number(material.conductivity)}",
                f"SPECIFIC_HEAT = {cfast_number(material.specific_heat)}",
                f"DENSITY = {cfast_number(material.density)}",
                f"THICKNESS = {cfast_number(material.thickness)}",
                f"EMISSIVITY = {cfast_number(material.emissivity)}",
            ]

            if material.fyi:
                fields.append(f"FYI = {cfast_string(material.fyi)}")

            add_wrapped_namelist(lines, "MATL", fields)

        lines.append("")

    lines.append("!! Compartments")
    for compartment in case.compartments:
        fields = [
            f"ID = {cfast_string(compartment.id)}",
            f"DEPTH = {cfast_number(compartment.depth)}",
            f"HEIGHT = {cfast_number(compartment.height)}",
            f"WIDTH = {cfast_number(compartment.width)}",
            f"ORIGIN = {cfast_vector((compartment.origin_x, compartment.origin_y, compartment.origin_z))}",
            f"GRID = {cfast_vector(compartment.grid)}",
            f"CEILING_MATL_ID = {cfast_string_vector(compartment.ceiling_matl_id)}",
            f"CEILING_THICKNESS = {cfast_vector(compartment.ceiling_thickness)}",
            f"WALL_MATL_ID = {cfast_string_vector(compartment.wall_matl_id)}",
            f"WALL_THICKNESS = {cfast_vector(compartment.wall_thickness)}",
            f"FLOOR_MATL_ID = {cfast_string_vector(compartment.floor_matl_id)}",
            f"FLOOR_THICKNESS = {cfast_vector(compartment.floor_thickness)}",
            f"HALL = {cfast_logical(compartment.hall)}",
            f"SHAFT = {cfast_logical(compartment.shaft)}",
            f"LEAK_AREA_RATIO = {cfast_vector((compartment.wall_leak_area_ratio, compartment.floor_leak_area_ratio))}",
            f"LEAK_AREA = {cfast_vector((compartment.wall_leak_area, compartment.floor_leak_area))}",
            f"FLOW_COEFFICIENT = {cfast_number(compartment.flow_coefficient)}",
        ]

        if compartment.cross_section_heights and compartment.cross_section_areas:
            fields.append(
                f"CROSS_SECT_HEIGHTS = {cfast_vector(compartment.cross_section_heights)}"
            )
            fields.append(
                f"CROSS_SECT_AREAS = {cfast_vector(compartment.cross_section_areas)}"
            )

        if compartment.fyi:
            fields.append(f"FYI = {cfast_string(compartment.fyi)}")

        add_wrapped_namelist(lines, "COMP", fields)

    lines.append("")

    if case.wall_vents:
        lines.append("!! Wall Vents")
        for vent in case.wall_vents:
            fields = [
                "TYPE = 'WALL'",
                f"ID = {cfast_string(vent.id)}",
                f"COMP_IDS = {cfast_string(vent.first_comp_id)} {cfast_string(vent.second_comp_id)}",
                f"BOTTOM = {cfast_number(vent.bottom)}",
                f"HEIGHT = {cfast_number(vent.height)}",
                f"WIDTH = {cfast_number(vent.width)}",
            ]

            t_values, f_values = wall_vent_schedule(vent)
            if t_values and f_values:
                fields.extend(
                    [
                        f"CRITERION = {cfast_string(vent.criterion)}",
                        f"T = {cfast_vector(t_values)}",
                        f"F = {cfast_vector(f_values)}",
                    ]
                )

            fields.extend(
                [
                    f"FACE = {cfast_string(vent.face)}",
                    f"OFFSET = {cfast_number(vent.offset)}",
                ]
            )

            if vent.fyi:
                fields.append(f"FYI = {cfast_string(vent.fyi)}")

            add_wrapped_namelist(lines, "VENT", fields)

        lines.append("")

    if getattr(case, "ceiling_floor_vents", []):
        lines.append("!! Ceiling/Floor Vents")
        for vent in case.ceiling_floor_vents:
            t_values, f_values = scheduled_values(vent)
            initial_open = getattr(vent, "initial_open", 1.0)
            vent_type = getattr(vent, "vent_type", "CEILING").upper()

            if not t_values and abs(initial_open - 1.0) > 1.0e-12:
                t_values = [0.0]
                f_values = [initial_open]

            fields = [
                f"TYPE = {cfast_string(vent_type)}",
                f"ID = {cfast_string(vent.id)}",
                f"COMP_IDS = {cfast_string(vent.top_comp_id)} {cfast_string(vent.bottom_comp_id)}",
                f"AREA = {cfast_number(vent.area)}",
                f"SHAPE = {cfast_string(vent.shape)}",
                f"OFFSETS = {cfast_vector((vent.offset_x, vent.offset_y))}",
            ]

            if t_values and f_values:
                fields.extend(
                    [
                        f"CRITERION = {cfast_string(vent.criterion)}",
                        f"T = {cfast_vector(t_values)}",
                        f"F = {cfast_vector(f_values)}",
                    ]
                )

            if vent.fyi:
                fields.append(f"FYI = {cfast_string(vent.fyi)}")

            add_wrapped_namelist(lines, "VENT", fields)

        lines.append("")

    if getattr(case, "mechanical_vents", []):
        lines.append("!! Mechanical Ventilation")
        for vent in case.mechanical_vents:
            t_values, f_values = scheduled_values(vent)
            fields = [
                "TYPE = 'MECHANICAL'",
                f"ID = {cfast_string(vent.id)}",
                f"COMP_IDS = {cfast_string(vent.from_comp_id)} {cfast_string(vent.to_comp_id)}",
                f"AREAS = {cfast_vector((vent.from_area, vent.to_area))}",
                f"HEIGHTS = {cfast_vector((vent.from_height, vent.to_height))}",
                f"ORIENTATIONS = {cfast_string_vector((vent.from_orientation, vent.to_orientation))}",
                f"FLOW = {cfast_number(vent.flow)}",
                f"CUTOFFS = {cfast_vector((vent.begin_dropoff, vent.zero_flow))}",
                f"OFFSETS = {cfast_vector((vent.offset_x, vent.offset_y))}",
                f"FILTER_EFFICIENCY = {cfast_number(vent.filter_efficiency)}",
                f"FILTER_TIME = {cfast_number(vent.filter_time)}",
            ]

            if t_values and f_values:
                fields.extend(
                    [
                        f"CRITERION = {cfast_string(vent.criterion)}",
                        f"T = {cfast_vector(t_values)}",
                        f"F = {cfast_vector(f_values)}",
                    ]
                )

            if vent.fyi:
                fields.append(f"FYI = {cfast_string(vent.fyi)}")

            add_wrapped_namelist(lines, "VENT", fields)

        lines.append("")

    if getattr(case, "targets", []):
        lines.append("!! Targets")
        for target in case.targets:
            fields = [
                f"TYPE = {cfast_string(target.target_type.upper())}",
                f"ID = {cfast_string(target.id)}",
                f"COMP_ID = {cfast_string(target.comp_id)}",
                f"LOCATION = {cfast_vector((target.x_position, target.y_position, target.z_position))}",
                f"NORMAL = {cfast_vector((target.x_normal, target.y_normal, target.z_normal))}",
                f"MATL_ID = {cfast_string(target.matl_id)}",
                f"THICKNESS = {cfast_number(target.thickness)}",
                f"TEMPERATURE_DEPTH = {cfast_number(target.temperature_depth)}",
                f"DEPTH_UNITS = {cfast_string(target.depth_units.upper())}",
                f"ADIABATIC_TARGET = {cfast_logical(target.adiabatic)}",
                f"CONVECTION_COEFFICIENTS = {cfast_vector((target.convection_coefficient_front, target.convection_coefficient_back))}",
            ]

            if target.surface_orientation != "USER SPECIFIED":
                fields.append(
                    f"SURFACE_ORIENTATION = {cfast_string(target.surface_orientation)}"
                )

            if target.surface_temperature is not None:
                fields.append(
                    f"SURFACE_TEMPERATURE = {cfast_number(target.surface_temperature)}"
                )

            if target.fyi:
                fields.append(f"FYI = {cfast_string(target.fyi)}")

            add_wrapped_namelist(lines, "DEVC", fields)

        lines.append("")

    if getattr(case, "detection_devices", []):
        lines.append("!! Detection / Suppression")
        for device in case.detection_devices:
            fields = [
                f"TYPE = {cfast_string(device.device_type.upper())}",
                f"ID = {cfast_string(device.id)}",
                f"COMP_ID = {cfast_string(device.comp_id)}",
                f"LOCATION = {cfast_vector((device.x_position, device.y_position, device.z_position))}",
            ]

            if device.device_type.upper() == "SMOKE_DETECTOR":
                fields.append(f"SETPOINT = {cfast_number(device.activation_obscuration)}")
            else:
                fields.append(f"SETPOINT = {cfast_number(device.activation_temperature)}")

            fields.extend(
                [
                    f"RTI = {cfast_number(device.rti)}",
                    f"SPRAY_DENSITY = {cfast_number(device.spray_density)}",
                ]
            )

            if device.fyi:
                fields.append(f"FYI = {cfast_string(device.fyi)}")

            add_wrapped_namelist(lines, "DEVC", fields)

        lines.append("")

    if getattr(case, "wall_surface_connections", []) or getattr(
        case,
        "ceiling_floor_surface_connections",
        [],
    ):
        lines.append("!! Surface Connections")

        for conn in case.wall_surface_connections:
            fields = [
                "TYPE = 'WALL'",
                f"COMP_ID = {cfast_string(conn.first_comp_id)}",
                f"COMP_IDS = {cfast_string(conn.second_comp_id)}",
                f"F = {cfast_number(conn.fraction)}",
            ]

            if conn.fyi:
                fields.append(f"FYI = {cfast_string(conn.fyi)}")

            add_wrapped_namelist(lines, "CONN", fields)

        for conn in case.ceiling_floor_surface_connections:
            fields = [
                "TYPE = 'FLOOR'",
                f"COMP_ID = {cfast_string(conn.top_comp_id)}",
                f"COMP_IDS = {cfast_string(conn.bottom_comp_id)}",
            ]

            if conn.fyi:
                fields.append(f"FYI = {cfast_string(conn.fyi)}")

            add_wrapped_namelist(lines, "CONN", fields)

        lines.append("")

    lines.append("!! Fires")
    for fire in case.fires:
        fields = [
            f"ID = {cfast_string(fire.id)}",
            f"COMP_ID = {cfast_string(fire.comp_id)}",
            f"FIRE_ID = {cfast_string(fire.fire_property_id)}",
            f"IGNITION_CRITERION = {cfast_string(fire.ignition_criterion.upper())}",
            f"SETPOINT = {cfast_number(fire.setpoint)}",
            f"LOCATION = {cfast_vector((fire.x_position, fire.y_position))}",
        ]

        if fire.target:
            fields.append(f"DEVC_ID = {cfast_string(fire.target)}")

        if fire.fyi:
            fields.append(f"FYI = {cfast_string(fire.fyi)}")

        add_wrapped_namelist(lines, "FIRE", fields)

    lines.append("")
    lines.append("!! Fire Properties")
    for prop in case.fire_properties:
        add_wrapped_namelist(
            lines,
            "CHEM",
            [
                f"ID = {cfast_string(prop.id)}",
                f"CARBON = {prop.carbon}",
                f"CHLORINE = {prop.chlorine}",
                f"HYDROGEN = {prop.hydrogen}",
                f"NITROGEN = {prop.nitrogen}",
                f"OXYGEN = {prop.oxygen}",
                f"HEAT_OF_COMBUSTION = {cfast_number(prop.heat_of_combustion)}",
                f"RADIATIVE_FRACTION = {cfast_number(prop.radiative_fraction)}",
            ],
        )

        lines.append(
            f"&TABL ID = {cfast_string(prop.id)}, "
            "LABELS = 'TIME', 'HRR', 'HEIGHT', 'AREA', 'CO_YIELD',"
        )
        lines.append("      'SOOT_YIELD', 'HCN_YIELD', 'TRACE_YIELD' /")

        for point in prop.sorted_ramp():
            data = (
                point.time,
                point.hrr,
                point.height,
                point.area,
                point.co_yield,
                point.soot_yield,
                point.hcn_yield,
                point.trace_yield,
            )

            add_wrapped_namelist(
                lines,
                "TABL",
                [
                    f"ID = {cfast_string(prop.id)}",
                    f"DATA = {cfast_vector(data)}",
                ],
            )

    lines.append("")

    if getattr(case, "output_visualizations", []):
        lines.append("!! Visualizations")

        for vis in case.output_visualizations:
            vis_type = vis.visualization_type.upper()
            comp_id = vis.comp_id.strip()
            comp_value = "NULL" if comp_id.upper() in {"ALL", "NULL", ""} else comp_id

            if vis_type == "2-D":
                axis = vis.axis.upper()[0:1]
                add_wrapped_namelist(
                    lines,
                    "SLCF",
                    [
                        "DOMAIN = '2-D'",
                        f"COMP_ID = {cfast_string(comp_value)}",
                        f"PLANE = {cfast_string(axis)}",
                        f"POSITION = {cfast_number(vis.value)}",
                    ],
                )
            else:
                add_wrapped_namelist(
                    lines,
                    "SLCF",
                    [
                        "DOMAIN = '3-D'",
                        f"COMP_ID = {cfast_string(comp_value)}",
                    ],
                )

        lines.append("")

    lines.append("&TAIL /")

    path.write_text("\n".join(lines) + "\n", encoding="utf-8")

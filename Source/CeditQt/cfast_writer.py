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
    compartments = {compartment.id: compartment for compartment in case.compartments}
    material_ids = {material.id for material in case.materials}

    def require_unique_ids(object_type: str, objects) -> None:
        ids = [object_.id for object_ in objects]
        duplicates = sorted({object_id for object_id in ids if ids.count(object_id) > 1})
        if duplicates:
            raise ValueError(
                f"Duplicate {object_type} ID: {duplicates[0]!r}. IDs must be unique."
            )

    def get_compartment(vent_type: str, vent_id: str, location: str, comp_id: str):
        if comp_id not in compartments:
            raise ValueError(
                f"{vent_type} {vent_id!r}: {location} compartment "
                f"{comp_id!r} does not exist."
            )
        return compartments[comp_id]

    def validate_vent_schedule(vent_type: str, vent) -> None:
        initial_open = getattr(vent, "initial_open", 1.0)
        if not 0.0 <= initial_open <= 1.0:
            raise ValueError(
                f"{vent_type} {vent.id!r}: initial opening fraction must be 0 to 1."
            )

        t_values, f_values = scheduled_values(vent)
        for index, (time_value, fraction_value) in enumerate(zip(t_values, f_values)):
            if time_value < 0.0:
                raise ValueError(
                    f"{vent_type} {vent.id!r}: opening times must be non-negative."
                )
            if index and time_value < t_values[index - 1]:
                raise ValueError(
                    f"{vent_type} {vent.id!r}: opening times must increase."
                )
            if not 0.0 <= fraction_value <= 1.0:
                raise ValueError(
                    f"{vent_type} {vent.id!r}: opening fractions must be 0 to 1."
                )

    def material_is_defined(material_id: str) -> bool:
        material_id = material_id.strip()
        if material_id.upper() in {"", "OFF", "NULL", "DEFAULT"}:
            return True

        return material_id in material_ids

    require_unique_ids("compartment", case.compartments)
    require_unique_ids("thermal property", case.materials)
    require_unique_ids("wall vent", case.wall_vents)
    require_unique_ids("ceiling/floor vent", getattr(case, "ceiling_floor_vents", []))
    require_unique_ids("mechanical vent", getattr(case, "mechanical_vents", []))
    require_unique_ids("target", getattr(case, "targets", []))
    require_unique_ids("detection device", getattr(case, "detection_devices", []))
    require_unique_ids("fire property", case.fire_properties)
    require_unique_ids("fire", case.fires)

    for material in case.materials:
        if not material.id.strip() or len(material.id) > 16:
            raise ValueError(
                f"Thermal property {material.id!r}: ID must contain 1 to 16 characters."
            )
        if not 0.0 <= material.emissivity <= 1.0:
            raise ValueError(
                f"Thermal property {material.id!r}: emissivity must be 0 to 1."
            )
        if material.conductivity < 0.0:
            raise ValueError(
                f"Thermal property {material.id!r}: conductivity must be non-negative."
            )
        if material.specific_heat < 0.0:
            raise ValueError(
                f"Thermal property {material.id!r}: specific heat must be non-negative."
            )
        if material.density < 0.0:
            raise ValueError(
                f"Thermal property {material.id!r}: density must be non-negative."
            )
        if material.thickness < 0.0:
            raise ValueError(
                f"Thermal property {material.id!r}: thickness must be non-negative."
            )

    for compartment in case.compartments:
        if compartment.width <= 0.0:
            raise ValueError(f"Compartment {compartment.id!r}: width must be positive.")
        if compartment.depth <= 0.0:
            raise ValueError(f"Compartment {compartment.id!r}: depth must be positive.")
        if compartment.height <= 0.0:
            raise ValueError(f"Compartment {compartment.id!r}: height must be positive.")
        if len(compartment.cross_section_heights) != len(compartment.cross_section_areas):
            raise ValueError(
                f"Compartment {compartment.id!r}: cross-section heights and areas must match."
            )
        for height, area in zip(
            compartment.cross_section_heights, compartment.cross_section_areas
        ):
            if not 0.0 <= height <= compartment.height:
                raise ValueError(
                    f"Compartment {compartment.id!r}: cross-section height must be "
                    "between the floor and ceiling."
                )
            if area < 0.0:
                raise ValueError(
                    f"Compartment {compartment.id!r}: cross-section area must be non-negative."
                )

        for surface_name, material_values in (
            ("ceiling", compartment.ceiling_matl_id),
            ("wall", compartment.wall_matl_id),
            ("floor", compartment.floor_matl_id),
        ):
            for material_id in material_values:
                if not material_is_defined(material_id):
                    raise ValueError(
                        f"Compartment {compartment.id!r}: {surface_name} material "
                        f"{material_id!r} is not defined in Thermal Properties."
                    )

    for vent in case.wall_vents:
        first_compartment = get_compartment(
            "Wall vent", vent.id, "first", vent.first_comp_id
        )
        if vent.second_comp_id != "OUTSIDE" and vent.second_comp_id not in compartment_ids:
            raise ValueError(
                f"Wall vent {vent.id!r}: second compartment "
                f"{vent.second_comp_id!r} does not exist."
            )
        if vent.first_comp_id == vent.second_comp_id:
            raise ValueError(
                f"Wall vent {vent.id!r}: the two compartments must be different."
            )
        if vent.bottom < 0.0 or vent.bottom > first_compartment.height:
            raise ValueError(
                f"Wall vent {vent.id!r}: bottom must be between the floor and ceiling "
                f"of compartment {vent.first_comp_id!r}."
            )
        if vent.height <= 0.0 or vent.bottom + vent.height > first_compartment.height:
            raise ValueError(
                f"Wall vent {vent.id!r}: top is above the ceiling of compartment "
                f"{vent.first_comp_id!r}. Reduce the height or bottom elevation."
            )
        if vent.width <= 0.0 or vent.width > max(first_compartment.width, first_compartment.depth):
            raise ValueError(
                f"Wall vent {vent.id!r}: width must be positive and no greater than "
                f"the dimensions of compartment {vent.first_comp_id!r}."
            )
        if vent.second_comp_id != "OUTSIDE":
            second_compartment = compartments[vent.second_comp_id]
            vent_bottom = vent.bottom + first_compartment.origin_z
            vent_top = vent_bottom + vent.height
            if (
                vent_bottom < second_compartment.origin_z
                or vent_top > second_compartment.origin_z + second_compartment.height
            ):
                raise ValueError(
                    f"Wall vent {vent.id!r}: it extends below the floor or above the "
                    f"ceiling of compartment {vent.second_comp_id!r}."
                )
        validate_vent_schedule("Wall vent", vent)

    for vent in getattr(case, "ceiling_floor_vents", []):
        top_compartment = None
        bottom_compartment = None
        if vent.top_comp_id != "OUTSIDE":
            top_compartment = get_compartment(
                "Ceiling/floor vent", vent.id, "top", vent.top_comp_id
            )
        if vent.bottom_comp_id != "OUTSIDE":
            bottom_compartment = get_compartment(
                "Ceiling/floor vent", vent.id, "bottom", vent.bottom_comp_id
            )
        if vent.top_comp_id == vent.bottom_comp_id:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: the two compartments must be different."
            )
        if vent.area <= 0.0:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: area must be positive."
            )
        if top_compartment is not None and vent.area > top_compartment.width * top_compartment.depth:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: area must be no greater than the floor "
                f"area of compartment {vent.top_comp_id!r}."
            )
        if top_compartment is not None and not 0.0 <= vent.offset_x <= top_compartment.width:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: X offset must be within compartment "
                f"{vent.top_comp_id!r}."
            )
        if top_compartment is not None and not 0.0 <= vent.offset_y <= top_compartment.depth:
            raise ValueError(
                f"Ceiling/floor vent {vent.id!r}: Y offset must be within compartment "
                f"{vent.top_comp_id!r}."
            )
        if top_compartment is not None and bottom_compartment is not None:
            if abs(top_compartment.origin_z - (bottom_compartment.origin_z + bottom_compartment.height)) > 0.1:
                raise ValueError(
                    f"Ceiling/floor vent {vent.id!r}: the floor of top compartment "
                    f"{vent.top_comp_id!r} must align with the ceiling of bottom compartment "
                    f"{vent.bottom_comp_id!r}."
                )
        validate_vent_schedule("Ceiling/floor vent", vent)

    for vent in getattr(case, "mechanical_vents", []):
        from_compartment = None
        to_compartment = None
        if vent.from_comp_id != "OUTSIDE":
            from_compartment = get_compartment(
                "Mechanical vent", vent.id, "from", vent.from_comp_id
            )
        if vent.to_comp_id != "OUTSIDE":
            to_compartment = get_compartment(
                "Mechanical vent", vent.id, "to", vent.to_comp_id
            )
        if vent.from_comp_id == vent.to_comp_id:
            raise ValueError(
                f"Mechanical vent {vent.id!r}: the two compartments must be different."
            )
        if vent.from_area <= 0.0 or vent.to_area <= 0.0:
            raise ValueError(f"Mechanical vent {vent.id!r}: diffuser areas must be positive.")
        if vent.begin_dropoff < 0.0 or vent.zero_flow < 0.0:
            raise ValueError(f"Mechanical vent {vent.id!r}: fan pressure cutoffs must be non-negative.")
        if vent.begin_dropoff >= vent.zero_flow:
            raise ValueError(
                f"Mechanical vent {vent.id!r}: the flow-dropoff pressure must be less "
                "than the zero-flow pressure."
            )
        if vent.filter_time < 0.0:
            raise ValueError(f"Mechanical vent {vent.id!r}: filter time must be non-negative.")
        for location, comp, area, height, orientation in (
            ("from", from_compartment, vent.from_area, vent.from_height, vent.from_orientation),
            ("to", to_compartment, vent.to_area, vent.to_height, vent.to_orientation),
        ):
            if comp is not None and orientation.upper() == "VERTICAL":
                half_size = area ** 0.5 / 2.0
                if height - half_size < 0.0 or height + half_size > comp.height:
                    raise ValueError(
                        f"Mechanical vent {vent.id!r}: the {location} diffuser extends "
                        f"below the floor or above the ceiling of compartment {comp.id!r}."
                    )
        validate_vent_schedule("Mechanical vent", vent)

    for target in getattr(case, "targets", []):
        if target.comp_id not in compartment_ids:
            raise ValueError(
                f"Target {target.id!r}: compartment {target.comp_id!r} does not exist."
            )
        if not material_is_defined(target.matl_id):
            raise ValueError(
                f"Target {target.id!r}: material {target.matl_id!r} is not "
                "defined in Thermal Properties."
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
        target_compartment = compartments[target.comp_id]
        if not 0.0 <= target.x_position <= target_compartment.width:
            raise ValueError(f"Target {target.id!r}: X position is outside its compartment.")
        if not 0.0 <= target.y_position <= target_compartment.depth:
            raise ValueError(f"Target {target.id!r}: Y position is outside its compartment.")
        if not 0.0 <= target.z_position <= target_compartment.height:
            raise ValueError(f"Target {target.id!r}: Z position is outside its compartment.")
        if any(abs(value) > 1.0 for value in (target.x_normal, target.y_normal, target.z_normal)):
            raise ValueError(f"Target {target.id!r}: normal-vector values must be between -1 and 1.")
        if target.thickness > 0.0 and target.temperature_depth > target.thickness:
            raise ValueError(
                f"Target {target.id!r}: internal temperature depth exceeds target thickness."
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
        device_compartment = compartments[device.comp_id]
        if not 0.0 <= device.x_position <= device_compartment.width:
            raise ValueError(
                f"Detection device {device.id!r}: X position is outside its compartment."
            )
        if not 0.0 <= device.y_position <= device_compartment.depth:
            raise ValueError(
                f"Detection device {device.id!r}: Y position is outside its compartment."
            )
        if not 0.0 <= device.z_position <= device_compartment.height:
            raise ValueError(
                f"Detection device {device.id!r}: Z position is outside its compartment."
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
        if vis_type == "2-D":
            visualization_compartments = (
                case.compartments
                if comp_id.upper() in {"ALL", "NULL", ""}
                else [compartments[comp_id]]
            )
            dimension_name = {"X": "width", "Y": "depth", "Z": "height"}[axis]
            for visualization_compartment in visualization_compartments:
                maximum = getattr(visualization_compartment, dimension_name)
                if not 0.0 <= vis.value <= maximum:
                    raise ValueError(
                        f"Visualization output: {axis} position is outside compartment "
                        f"{visualization_compartment.id!r}."
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

        fire_compartment = compartments[fire.comp_id]
        if not 0.0 <= fire.x_position <= fire_compartment.width:
            raise ValueError(f"Fire {fire.id!r}: X position is outside its compartment.")
        if not 0.0 <= fire.y_position <= fire_compartment.depth:
            raise ValueError(f"Fire {fire.id!r}: Y position is outside its compartment.")

        ignition = fire.ignition_criterion.upper()
        if ignition not in {"TIME", "TEMPERATURE", "FLUX"}:
            raise ValueError(
                f"Fire {fire.id!r}: ignition criterion must be TIME, TEMPERATURE, "
                "or FLUX."
            )
        if ignition != "TIME":
            target_ids = {target.id for target in getattr(case, "targets", [])}
            if fire.target not in target_ids:
                raise ValueError(
                    f"Fire {fire.id!r}: ignition by {ignition.lower()} requires an "
                    "existing target."
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

    lines.append("!! CFAST input generated by CEdit 8")
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

    outp_fields = []
    if getattr(case, "net_heat_flux_output", False):
        outp_fields.append("NET_HEAT_FLUX_OUTPUT = .TRUE.")
    if getattr(case, "validation_output", False):
        outp_fields.append("VALIDATION_OUTPUT = .TRUE.")

    spreadsheet_output = spreadsheet_output_code(case)
    if spreadsheet_output != "CDMVW":
        outp_fields.append(f"SPREADSHEET_OUTPUT = {cfast_string(spreadsheet_output)}")

    if outp_fields:
        add_wrapped_namelist(lines, "OUTP", outp_fields)
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

    for extra_namelist in getattr(case, "extra_namelists", []):
        text = extra_namelist.strip()
        if text:
            lines.append(text)
            lines.append("")

    lines.append("&TAIL /")

    text = "\n".join(lines) + "\n"
    if path.exists():
        try:
            if path.read_text(encoding="utf-8") == text:
                return
        except UnicodeDecodeError:
            pass

    path.write_text(text, encoding="utf-8")

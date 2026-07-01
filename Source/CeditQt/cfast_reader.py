from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import re
from typing import Any

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


_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_NUMBER_RE = re.compile(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?")


# Current executable namelists from Source/CFAST/input_namelist.f90.
CFAST_NAMELIST_PARAMETERS: dict[str, set[str]] = {
    "HEAD": {"VERSION", "TITLE"},
    "TIME": {"PRINT", "SIMULATION", "SPREADSHEET", "SMOKEVIEW"},
    "INIT": {
        "PRESSURE",
        "RELATIVE_HUMIDITY",
        "INTERIOR_TEMPERATURE",
        "EXTERIOR_TEMPERATURE",
        "INTERIOR_O2_MASS_FRACTION",
        "EXTERIOR_O2_MASS_FRACTION",
    },
    "MISC": {
        "ADIABATIC",
        "MAX_TIME_STEP",
        "MAX_ITERATION",
        "LOWER_OXYGEN_LIMIT",
        "SPECIFIC_EXTINCTION",
        "OVERWRITE",
    },
    "MATL": {
        "CONDUCTIVITY",
        "DENSITY",
        "EMISSIVITY",
        "ID",
        "MATERIAL",
        "SPECIFIC_HEAT",
        "THICKNESS",
        "FYI",
    },
    "COMP": {
        "CROSS_SECT_AREAS",
        "CROSS_SECT_HEIGHTS",
        "DEPTH",
        "GRID",
        "HALL",
        "HEIGHT",
        "ID",
        "FYI",
        "CEILING_MATL_ID",
        "FLOOR_MATL_ID",
        "WALL_MATL_ID",
        "CEILING_THICKNESS",
        "FLOOR_THICKNESS",
        "WALL_THICKNESS",
        "ORIGIN",
        "SHAFT",
        "WIDTH",
        "LEAK_AREA_RATIO",
        "LEAK_AREA",
        "FLOW_COEFFICIENT",
    },
    "DEVC": {
        "COMP_ID",
        "TYPE",
        "ID",
        "TEMPERATURE_DEPTH",
        "DEPTH_UNITS",
        "LOCATION",
        "MATL_ID",
        "NORMAL",
        "SURFACE_ORIENTATION",
        "SURFACE_TEMPERATURE",
        "THICKNESS",
        "RTI",
        "SETPOINT",
        "SPRAY_DENSITY",
        "SETPOINTS",
        "ADIABATIC_TARGET",
        "CONVECTION_COEFFICIENTS",
        "FYI",
    },
    "RAMP": {"F", "ID", "T", "Z", "TYPE", "COMP_IDS"},
    "TABL": {"ID", "LABELS", "DATA"},
    "FIRE": {
        "COMP_ID",
        "DEVC_ID",
        "FIRE_ID",
        "ID",
        "IGNITION_CRITERION",
        "LOCATION",
        "SETPOINT",
        "FYI",
    },
    "CHEM": {
        "AREA",
        "CARBON",
        "CHLORINE",
        "COMP_ID",
        "CO_YIELD",
        "HEAT_OF_COMBUSTION",
        "HCN_YIELD",
        "HRR",
        "HYDROGEN",
        "ID",
        "NITROGEN",
        "OXYGEN",
        "RADIATIVE_FRACTION",
        "SOOT_YIELD",
        "TABLE_ID",
        "TRACE_YIELD",
        "FLAMING_TRANSITION_TIME",
    },
    "VENT": {
        "AREA",
        "AREAS",
        "BOTTOM",
        "COMP_IDS",
        "CRITERION",
        "CUTOFFS",
        "DEVC_ID",
        "F",
        "FACE",
        "FILTER_EFFICIENCY",
        "FILTER_TIME",
        "FLOW",
        "HEIGHT",
        "HEIGHTS",
        "ID",
        "OFFSET",
        "OFFSETS",
        "ORIENTATIONS",
        "PRE_FRACTION",
        "POST_FRACTION",
        "SETPOINT",
        "SHAPE",
        "T",
        "TOP",
        "TYPE",
        "WIDTH",
        "FYI",
        "FLOW_COEFFICIENT",
    },
    "CONN": {"COMP_ID", "COMP_IDS", "F", "TYPE"},
    "ISOF": {"COMP_ID", "VALUE"},
    "SLCF": {"DOMAIN", "PLANE", "POSITION", "COMP_ID"},
    "DIAG": {
        "MODE",
        "RAD_SOLVER",
        "PARTIAL_PRESSURE_H2O",
        "PARTIAL_PRESSURE_CO2",
        "GAS_TEMPERATURE",
        "T",
        "F",
        "HORIZONTAL_FLOW_SUB_MODEL",
        "FIRE_SUB_MODEL",
        "ENTRAINMENT_SUB_MODEL",
        "VERTICAL_FLOW_SUB_MODEL",
        "CEILING_JET_SUB_MODEL",
        "DOOR_JET_FIRE_SUB_MODEL",
        "CONVECTION_SUB_MODEL",
        "RADIATION_SUB_MODEL",
        "CONDUCTION_SUB_MODEL",
        "DEBUG_PRINT",
        "MECHANICAL_FLOW_SUB_MODEL",
        "KEYBOARD_INPUT",
        "STEADY_STATE_INITIAL_CONDITIONS",
        "DASSL_DEBUG_PRINT",
        "OXYGEN_TRACKING",
        "GAS_ABSORBTION_SUB_MODEL",
        "RESIDUAL_DEBUG_PRINT",
        "LAYER_MIXING_SUB_MODEL",
        "ADIABATIC_TARGET_VERIFICATION",
        "RADIATIVE_INCIDENT_FLUX",
        "UPPER_LAYER_THICKNESS",
        "VERIFICATION_TIME_STEP",
        "VERIFICATION_FIRE_HEAT_FLUX",
    },
    "DUMP": {
        "ID",
        "FILE",
        "FIRST_DEVICE",
        "FIRST_MEASUREMENT",
        "SECOND_DEVICE",
        "SECOND_MEASUREMENT",
        "FIRST_FIELD",
        "SECOND_FIELD",
        "CRITERION",
        "TYPE",
        "FYI",
    },
    "OUTP": {
        "ID",
        "FILE",
        "FIRST_DEVICE",
        "FIRST_MEASUREMENT",
        "SECOND_DEVICE",
        "SECOND_MEASUREMENT",
        "FIRST_FIELD",
        "SECOND_FIELD",
        "CRITERION",
        "TYPE",
        "FYI",
        "VALIDATION_OUTPUT",
        "NET_HEAT_FLUX_OUTPUT",
        "SPREADSHEET_OUTPUT",
    },
    "TAIL": set(),
}


# Current CData/preprocessor namelists from Source/Cdata/input_pp_namelist.f90.
CDATA_NAMELIST_PARAMETERS: dict[str, set[str]] = {
    "MHDR": {
        "NUMBER_OF_CASES",
        "SEEDS",
        "WRITE_SEEDS",
        "WORK_FOLDER",
        "OUTPUT_FOLDER",
        "PARAMETER_FILE",
        "WRITE_VALIDATION_OUTPUT",
    },
    "MRND": {
        "ID",
        "FYI",
        "DISTRIBUTION_TYPE",
        "MINIMUM",
        "MAXIMUM",
        "MEAN",
        "STDEV",
        "ALPHA",
        "BETA",
        "PEAK",
        "RANDOM_SEEDS",
        "VALUES",
        "PROBABILITIES",
        "CONSTANT",
        "MINIMUM_FIELD",
        "MAXIMUM_FIELD",
        "MINIMUM_OFFSET",
        "MAXIMUM_OFFSET",
        "ADD_FIELD",
    },
    "MFLD": {
        "ID",
        "FYI",
        "FIELD_TYPE",
        "FIELD",
        "RAND_ID",
        "PARAMETER_COLUMN_LABEL",
        "ADD_TO_PARAMETERS",
        "REAL_VALUES",
        "INTEGER_VALUES",
        "STRING_VALUES",
        "LOGICAL_VALUES",
        "SCENARIO_TITLES",
        "BASE_SCALING_VALUE",
        "POSITION",
    },
    "MSTT": {
        "ID",
        "FYI",
        "ANALYSIS_TYPE",
        "INPUT_FILENAME",
        "OUTPUT_FILENAME",
        "ERROR_FILENAME",
        "LOG_FILENAME",
        "COLUMN_LABEL",
    },
    "MFIR": {
        "ID",
        "FYI",
        "FIRE_ID",
        "BASE_FIRE_ID",
        "SCALING_FIRE_HRR_RANDOM_GENERATOR_ID",
        "SCALING_FIRE_TIME_RANDOM_GENERATOR_ID",
        "MODIFY_FIRE_AREA_TO_MATCH_HRR",
        "ADD_HRR_SCALE_TO_PARAMETERS",
        "ADD_TIME_SCALE_TO_PARAMETERS",
        "HRR_SCALE_COLUMN_LABEL",
        "TIME_SCALE_COLUMN_LABEL",
        "FIRE_COMPARTMENT_RANDOM_GENERATOR_ID",
        "FIRE_COMPARTMENT_IDS",
        "ADD_FIRE_COMPARTMENT_ID_TO_PARAMETERS",
        "FIRE_COMPARTMENT_ID_COLUMN_LABEL",
        "FLAMING_SMOLDERING_INCIPIENT_RANDOM_GENERATOR_ID",
        "FLAMING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID",
        "FIRE_LABEL_PARAMETER_COLUMN_LABEL",
        "FLAMING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID",
        "SMOLDERING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID",
        "SMOLDERING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID",
        "FIRE_HRR_GENERATOR_IDS",
        "FIRE_TIME_GENERATOR_IDS",
        "TYPE_OF_INCIPIENT_GROWTH",
        "INCIPIENT_FIRE_TYPES",
        "INCIPIENT_TYPE_COLUMN_LABEL",
        "SMOLDERING_TIME_COLUMN_LABEL",
        "SMOLDERING_HRR_PEAK_COLUMN_LABEL",
        "FLAMING_TIME_COLUMN_LABEL",
        "FLAMING_HRR_PEAK_COLUMN_LABEL",
        "ADD_INCIPIENT_TYPE_TO_PARAMETERS",
        "ADD_INCIPIENT_PEAK_HRR_TO_PARAMETERS",
        "ADD_INCIPIENT_TIME_TO_PARAMETERS",
        "NUMBER_OF_GROWTH_POINTS",
        "NUMBER_OF_DECAY_POINTS",
        "GROWTH_EXPONENT",
        "DECAY_EXPONENT",
        "ADD_HRR_TO_PARAMETERS",
        "ADD_TIME_TO_PARAMETERS",
        "HRR_LABELS",
        "TIME_LABELS",
        "ADD_FIRE_TO_PARAMETERS",
        "GENERATOR_IS_TIME_TO_1054_KW",
    },
    "MDIA": {
        "ID",
        "TYPE",
        "FIRST_FIELD",
        "SECOND_FIELD",
        "ZERO_EXCEPTIONS",
        "TEST",
        "CRITERION",
        "COLUMN_OUTPUT",
        "CUTOFFS",
        "TEST_COLUMN",
        "COLUMN_HEADERS",
    },
}


VALID_NAMELIST_PARAMETERS: dict[str, set[str]] = {
    **CFAST_NAMELIST_PARAMETERS,
    **CDATA_NAMELIST_PARAMETERS,
}


@dataclass
class NamelistRecord:
    name: str
    fields: dict[str, list[Any]]
    line: int
    raw: str


@dataclass
class ImportResult:
    case: CfastCase
    warnings: list[str]
    records: list[NamelistRecord]


@dataclass
class Token:
    kind: str
    value: Any


def read_cfast_input(path: str | Path) -> CfastCase:
    return read_cfast_input_with_warnings(path).case


def read_cfast_input_with_warnings(path: str | Path) -> ImportResult:
    path = Path(path)
    text = path.read_text(encoding="utf-8")
    try:
        records = parse_namelists(text)
    except ValueError as exc:
        raise ValueError(f"{path}: {exc}") from exc

    warnings: list[str] = []
    case = CfastCase()
    fire_properties_by_id: dict[str, FireProperty] = {}

    for record in records:
        try:
            apply_record(case, fire_properties_by_id, record, warnings)
        except Exception as exc:
            raise ValueError(
                f"{path}:{record.line}: could not import &{record.name}: {exc}"
            ) from exc

    case.fire_properties = list(fire_properties_by_id.values())

    if case.fires:
        first_fire = case.fires[0]
        case.comp_id = first_fire.comp_id
        case.fire_id = first_fire.id
        case.fire_chem_id = first_fire.fire_property_id
        case.fire_location_x = first_fire.x_position
        case.fire_location_y = first_fire.y_position

        first_property = fire_properties_by_id.get(first_fire.fire_property_id)
        if first_property is not None:
            case.carbon = first_property.carbon
            case.hydrogen = first_property.hydrogen
            case.oxygen = first_property.oxygen
            case.nitrogen = first_property.nitrogen
            case.chlorine = first_property.chlorine
            case.heat_of_combustion = first_property.heat_of_combustion
            case.radiative_fraction = first_property.radiative_fraction
            case.fire_ramp = list(first_property.ramp)

    case.import_warnings = warnings
    return ImportResult(case=case, warnings=warnings, records=records)


def parse_namelists(text: str) -> list[NamelistRecord]:
    stripped = strip_comments(text)
    validate_namelist_structure(stripped)
    records: list[NamelistRecord] = []
    index = 0

    while index < len(stripped):
        if stripped[index] != "&":
            index += 1
            continue

        match = _IDENT_RE.match(stripped, index + 1)
        if match is None:
            index += 1
            continue

        name = match.group(0).upper()
        body_start = match.end()
        end = find_record_end(stripped, body_start)
        if end is None:
            raise ValueError(f"Unterminated &{name} namelist.")

        raw = stripped[index : end + 1]
        body = stripped[body_start:end]
        line = stripped.count("\n", 0, index) + 1
        try:
            fields = parse_fields(body)
        except ValueError as exc:
            raise ValueError(
                f"Line {line}: could not parse &{name} namelist: {exc}"
            ) from exc
        records.append(
            NamelistRecord(
                name=name,
                fields=fields,
                line=line,
                raw=raw,
            )
        )
        index = end + 1

    validate_records_against_input_schema(records)
    return records


def validate_namelist_structure(text: str) -> None:
    current_name: str | None = None
    current_line: int | None = None

    for line_number, line in enumerate(text.splitlines(), start=1):
        namelist_name = line_start_namelist_name(line)
        if namelist_name is not None:
            validate_namelist_name(namelist_name, line_number)
            if current_name is not None and current_line is not None:
                raise ValueError(
                    f"Line {line_number}: &{namelist_name} starts before "
                    f"&{current_name} from line {current_line} is closed with '/'."
                )
            current_name = namelist_name
            current_line = line_number

        if current_name is not None and find_record_end(line, 0) is not None:
            current_name = None
            current_line = None

    if current_name is not None and current_line is not None:
        raise ValueError(
            f"Line {current_line}: &{current_name} namelist is missing closing '/'."
        )


def line_start_namelist_name(line: str) -> str | None:
    stripped = line.lstrip()
    if not stripped.startswith("&"):
        return None

    match = _IDENT_RE.match(stripped, 1)
    if match is None:
        return None

    return match.group(0).upper()


def validate_records_against_input_schema(records: list[NamelistRecord]) -> None:
    for record in records:
        valid_parameters = VALID_NAMELIST_PARAMETERS[record.name]
        unknown_parameters = sorted(
            parameter for parameter in record.fields if parameter not in valid_parameters
        )
        if not unknown_parameters:
            continue

        if len(unknown_parameters) == 1:
            raise ValueError(
                f"Line {record.line}: {unknown_parameters[0]} is not a valid "
                f"&{record.name} parameter for current CFAST or CData inputs."
            )

        raise ValueError(
            f"Line {record.line}: {', '.join(unknown_parameters)} are not valid "
            f"&{record.name} parameters for current CFAST or CData inputs."
        )


def validate_namelist_name(name: str, line: int) -> None:
    if name in VALID_NAMELIST_PARAMETERS:
        return

    raise ValueError(
        f"Line {line}: &{name} is not a valid CFAST or CData namelist "
        "for the current version."
    )


def strip_comments(text: str) -> str:
    lines = []

    for line in text.splitlines():
        in_string = False
        result = []
        index = 0

        while index < len(line):
            char = line[index]

            if char == "'":
                result.append(char)
                if in_string and index + 1 < len(line) and line[index + 1] == "'":
                    result.append(line[index + 1])
                    index += 2
                    continue
                in_string = not in_string
                index += 1
                continue

            if char == "!" and not in_string:
                break

            result.append(char)
            index += 1

        lines.append("".join(result))

    return "\n".join(lines)


def find_record_end(text: str, start: int) -> int | None:
    in_string = False
    index = start

    while index < len(text):
        char = text[index]

        if char == "'":
            if in_string and index + 1 < len(text) and text[index + 1] == "'":
                index += 2
                continue
            in_string = not in_string
        elif char == "/" and not in_string:
            return index

        index += 1

    return None


def parse_fields(body: str) -> dict[str, list[Any]]:
    tokens = tokenize(body)
    fields: dict[str, list[Any]] = {}
    index = 0

    while index < len(tokens):
        if is_assignment_start(tokens, index):
            key = str(tokens[index].value).upper()
            index += 2
            values: list[Any] = []

            while index < len(tokens) and not is_assignment_start(tokens, index):
                token = tokens[index]
                index += 1

                if token.kind == "COMMA":
                    continue

                values.append(token.value)

            fields.setdefault(key, []).extend(values)
        else:
            index += 1

    return fields


def tokenize(body: str) -> list[Token]:
    tokens: list[Token] = []
    index = 0

    while index < len(body):
        char = body[index]

        if char.isspace():
            index += 1
            continue

        if char == ",":
            tokens.append(Token("COMMA", ","))
            index += 1
            continue

        if char == "=":
            tokens.append(Token("EQUALS", "="))
            index += 1
            continue

        if char == "'":
            value, index = read_string_token(body, index)
            tokens.append(Token("STRING", value))
            continue

        logical = read_logical_token(body, index)
        if logical is not None:
            value, index = logical
            tokens.append(Token("LOGICAL", value))
            continue

        number = _NUMBER_RE.match(body, index)
        if number is not None:
            text = number.group(0)
            tokens.append(Token("NUMBER", parse_number(text)))
            index = number.end()
            continue

        ident = _IDENT_RE.match(body, index)
        if ident is not None:
            tokens.append(Token("IDENT", ident.group(0)))
            index = ident.end()
            continue

        index += 1

    return tokens


def read_string_token(text: str, start: int) -> tuple[str, int]:
    result = []
    index = start + 1

    while index < len(text):
        char = text[index]

        if char == "'":
            if index + 1 < len(text) and text[index + 1] == "'":
                result.append("'")
                index += 2
                continue
            return "".join(result), index + 1

        result.append(char)
        index += 1

    raise ValueError("Unterminated string literal.")


def read_logical_token(text: str, start: int) -> tuple[bool, int] | None:
    upper = text[start:].upper()

    if upper.startswith(".TRUE."):
        return True, start + len(".TRUE.")

    if upper.startswith(".FALSE."):
        return False, start + len(".FALSE.")

    return None


def parse_number(text: str) -> int | float:
    if "." not in text and "e" not in text.lower() and "d" not in text.lower():
        return int(text)

    return float(text.replace("D", "E").replace("d", "e"))


def is_assignment_start(tokens: list[Token], index: int) -> bool:
    return (
        index + 1 < len(tokens)
        and tokens[index].kind == "IDENT"
        and tokens[index + 1].kind == "EQUALS"
    )


def apply_record(
    case: CfastCase,
    fire_properties_by_id: dict[str, FireProperty],
    record: NamelistRecord,
    warnings: list[str],
) -> None:
    fields = record.fields
    name = record.name

    if name == "HEAD":
        case.version = int(number_field(fields, "VERSION", case.version))
        case.title = string_field(fields, "TITLE", case.title)
    elif name == "TIME":
        case.simulation_time = number_field(fields, "SIMULATION", case.simulation_time)
        case.print_interval = number_field(fields, "PRINT", case.print_interval)
        case.smokeview_interval = number_field(
            fields,
            "SMOKEVIEW",
            case.smokeview_interval,
        )
        case.spreadsheet_interval = number_field(
            fields,
            "SPREADSHEET",
            case.spreadsheet_interval,
        )
    elif name == "INIT":
        case.pressure = number_field(fields, "PRESSURE", case.pressure)
        case.relative_humidity = number_field(
            fields,
            "RELATIVE_HUMIDITY",
            case.relative_humidity,
        )
        case.interior_temperature = number_field(
            fields,
            "INTERIOR_TEMPERATURE",
            case.interior_temperature,
        )
        case.exterior_temperature = number_field(
            fields,
            "EXTERIOR_TEMPERATURE",
            case.exterior_temperature,
        )
    elif name == "MISC":
        case.adiabatic_surfaces = bool_field(
            fields,
            "ADIABATIC",
            case.adiabatic_surfaces,
        )
        case.lower_oxygen_limit = number_field(
            fields,
            "LOWER_OXYGEN_LIMIT",
            case.lower_oxygen_limit,
        )
        if "MAX_TIME_STEP" in fields:
            case.max_time_step = number_field(fields, "MAX_TIME_STEP", 0.0)
    elif name == "DIAG":
        case.extra_namelists.append(record.raw.strip())
        warnings.append(
            f"Line {record.line}: &DIAG was preserved and will be written on save "
            "but is not editable."
        )
    elif name == "OUTP":
        if "VALIDATION_OUTPUT" in fields:
            case.validation_output = bool_field(
                fields,
                "VALIDATION_OUTPUT",
                case.validation_output,
            )
        if "NET_HEAT_FLUX_OUTPUT" in fields:
            case.net_heat_flux_output = bool_field(
                fields,
                "NET_HEAT_FLUX_OUTPUT",
                case.net_heat_flux_output,
            )
        if "SPREADSHEET_OUTPUT" in fields:
            apply_spreadsheet_output(
                case,
                string_field(fields, "SPREADSHEET_OUTPUT", ""),
            )
        known_outp_fields = {
            "VALIDATION_OUTPUT",
            "NET_HEAT_FLUX_OUTPUT",
            "SPREADSHEET_OUTPUT",
        }
        if any(key not in known_outp_fields for key in fields):
            case.extra_namelists.append(record.raw.strip())
            warnings.append(
                f"Line {record.line}: &OUTP was preserved and will be written on save "
                "but is not editable."
            )
    elif name == "MATL":
        case.materials.append(material_from_fields(fields))
    elif name == "COMP":
        case.compartments.append(compartment_from_fields(fields))
    elif name == "VENT":
        add_vent(case, fields, warnings)
    elif name == "FIRE":
        case.fires.append(fire_from_fields(fields))
    elif name == "CHEM":
        prop = fire_property_from_fields(fields)
        fire_properties_by_id[prop.id] = prop
    elif name == "TABL":
        add_table_data(fire_properties_by_id, fields)
    elif name == "DEVC":
        add_device(case, fields, warnings)
    elif name == "CONN":
        add_connection(case, fields)
    elif name == "SLCF":
        case.output_visualizations.append(output_visualization_from_fields(fields))
    elif name == "TAIL":
        return
    else:
        case.extra_namelists.append(record.raw.strip())
        warnings.append(
            f"Line {record.line}: &{record.name} was preserved and will be written "
            "on save but is not editable."
        )


def apply_spreadsheet_output(case: CfastCase, value: str) -> None:
    selected = value.strip().upper()
    if not selected:
        return

    if selected == "ALL":
        selected = "CDMVW"
    elif selected in {"NONE", "OFF"}:
        selected = ""

    case.spreadsheet_output_compartments = "C" in selected
    case.spreadsheet_output_devices = "D" in selected
    case.spreadsheet_output_masses = "M" in selected
    case.spreadsheet_output_vents = "V" in selected
    case.spreadsheet_output_walls = "W" in selected


def material_from_fields(fields: dict[str, list[Any]]) -> MaterialProperty:
    matl_id = required_string(fields, "ID", "MATL ID")

    return MaterialProperty(
        id=matl_id,
        material=string_field(fields, "MATERIAL", matl_id),
        conductivity=number_field(fields, "CONDUCTIVITY", 0.0),
        specific_heat=number_field(fields, "SPECIFIC_HEAT", 0.0),
        density=number_field(fields, "DENSITY", 0.0),
        thickness=number_field(fields, "THICKNESS", 0.0),
        emissivity=number_field(fields, "EMISSIVITY", 0.9),
        fyi=string_field(fields, "FYI", ""),
    )


def compartment_from_fields(fields: dict[str, list[Any]]) -> Compartment:
    comp_id = required_string(fields, "ID", "COMP ID")
    origin = number_vector(fields, "ORIGIN", [0.0, 0.0, 0.0], length=3)
    grid = number_vector(fields, "GRID", [50, 50, 50], length=3)
    leak_area_ratio = number_vector(
        fields,
        "LEAK_AREA_RATIO",
        [0.0, 0.0],
        length=2,
    )
    leak_area = number_vector(fields, "LEAK_AREA", [0.0, 0.0], length=2)

    return Compartment(
        id=comp_id,
        width=number_field(fields, "WIDTH", 1.0),
        depth=number_field(fields, "DEPTH", 1.0),
        height=number_field(fields, "HEIGHT", 1.0),
        origin_x=origin[0],
        origin_y=origin[1],
        origin_z=origin[2],
        ceiling_matl_id=string_triple(fields, "CEILING_MATL_ID", "OFF"),
        ceiling_thickness=number_triple(fields, "CEILING_THICKNESS", 0.0),
        wall_matl_id=string_triple(fields, "WALL_MATL_ID", "OFF"),
        wall_thickness=number_triple(fields, "WALL_THICKNESS", 0.0),
        floor_matl_id=string_triple(fields, "FLOOR_MATL_ID", "OFF"),
        floor_thickness=number_triple(fields, "FLOOR_THICKNESS", 0.0),
        grid=(int(grid[0]), int(grid[1]), int(grid[2])),
        hall=bool_field(fields, "HALL", False),
        shaft=bool_field(fields, "SHAFT", False),
        flow_coefficient=number_field(fields, "FLOW_COEFFICIENT", 0.07),
        wall_leak_area_ratio=leak_area_ratio[0],
        floor_leak_area_ratio=leak_area_ratio[1],
        wall_leak_area=leak_area[0],
        floor_leak_area=leak_area[1],
        cross_section_heights=number_vector(fields, "CROSS_SECT_HEIGHTS", []),
        cross_section_areas=number_vector(fields, "CROSS_SECT_AREAS", []),
        fyi=string_field(fields, "FYI", ""),
    )


def add_vent(
    case: CfastCase,
    fields: dict[str, list[Any]],
    warnings: list[str],
) -> None:
    vent_type = string_field(fields, "TYPE", "WALL").upper()

    if vent_type == "WALL":
        case.wall_vents.append(wall_vent_from_fields(fields))
    elif vent_type in {"CEILING", "FLOOR"}:
        case.ceiling_floor_vents.append(ceiling_floor_vent_from_fields(fields, vent_type))
    elif vent_type == "MECHANICAL":
        case.mechanical_vents.append(mechanical_vent_from_fields(fields))
    else:
        warnings.append(f"Unsupported vent type {vent_type!r}.")


def wall_vent_from_fields(fields: dict[str, list[Any]]) -> WallVent:
    comp_ids = string_vector(fields, "COMP_IDS", ["", "OUTSIDE"])
    t_values = number_vector(fields, "T", [])
    f_values = number_vector(fields, "F", [])

    initial_open = number_field(fields, "INITIAL_OPEN", 1.0)
    if t_values and f_values and abs(t_values[0]) < 1.0e-12:
        initial_open = f_values[0]

    return WallVent(
        id=required_string(fields, "ID", "wall vent ID"),
        first_comp_id=comp_ids[0] if comp_ids else "",
        second_comp_id=comp_ids[1] if len(comp_ids) > 1 else "OUTSIDE",
        bottom=number_field(fields, "BOTTOM", 0.0),
        height=number_field(fields, "HEIGHT", 2.0),
        width=number_field(fields, "WIDTH", 1.0),
        initial_open=initial_open,
        face=string_field(fields, "FACE", "FRONT").upper(),
        offset=number_field(fields, "OFFSET", 0.0),
        criterion=string_field(fields, "CRITERION", "TIME").upper(),
        t_values=t_values,
        f_values=f_values,
        fyi=string_field(fields, "FYI", ""),
    )


def ceiling_floor_vent_from_fields(
    fields: dict[str, list[Any]],
    vent_type: str,
) -> CeilingFloorVent:
    comp_ids = string_vector(fields, "COMP_IDS", ["", ""])
    offsets = number_vector(fields, "OFFSETS", [0.0, 0.0], length=2)
    t_values = number_vector(fields, "T", [])
    f_values = number_vector(fields, "F", [])

    initial_open = number_field(fields, "INITIAL_OPEN", 1.0)
    if t_values and f_values and abs(t_values[0]) < 1.0e-12:
        initial_open = f_values[0]

    return CeilingFloorVent(
        id=required_string(fields, "ID", "ceiling/floor vent ID"),
        first_comp_id=comp_ids[0] if comp_ids else "",
        second_comp_id=comp_ids[1] if len(comp_ids) > 1 else "",
        vent_type=vent_type,
        area=number_field(fields, "AREA", 1.0),
        shape=string_field(fields, "SHAPE", "ROUND").upper(),
        initial_open=initial_open,
        offset_x=offsets[0],
        offset_y=offsets[1],
        criterion=string_field(fields, "CRITERION", "TIME").upper(),
        t_values=t_values,
        f_values=f_values,
        fyi=string_field(fields, "FYI", ""),
    )


def mechanical_vent_from_fields(fields: dict[str, list[Any]]) -> MechanicalVent:
    comp_ids = string_vector(fields, "COMP_IDS", ["OUTSIDE", ""])
    areas = number_vector(fields, "AREAS", [0.25, 0.25], length=2)
    heights = number_vector(fields, "HEIGHTS", [2.75, 2.75], length=2)
    cutoffs = number_vector(fields, "CUTOFFS", [200.0, 300.0], length=2)
    offsets = number_vector(fields, "OFFSETS", [0.0, 0.0], length=2)
    orientations = string_vector(fields, "ORIENTATIONS", ["VERTICAL", "VERTICAL"])
    orientations = pad_strings(orientations, "VERTICAL", 2)

    return MechanicalVent(
        id=required_string(fields, "ID", "mechanical vent ID"),
        from_comp_id=comp_ids[0] if comp_ids else "OUTSIDE",
        to_comp_id=comp_ids[1] if len(comp_ids) > 1 else "OUTSIDE",
        from_area=areas[0],
        from_height=heights[0],
        from_orientation=orientations[0].upper(),
        to_area=areas[1],
        to_height=heights[1],
        to_orientation=orientations[1].upper(),
        flow=number_field(fields, "FLOW", 0.02),
        begin_dropoff=cutoffs[0],
        zero_flow=cutoffs[1],
        offset_x=offsets[0],
        offset_y=offsets[1],
        filter_efficiency=number_field(fields, "FILTER_EFFICIENCY", 0.0),
        filter_time=number_field(fields, "FILTER_TIME", 0.0),
        criterion=string_field(fields, "CRITERION", "TIME").upper(),
        t_values=number_vector(fields, "T", []),
        f_values=number_vector(fields, "F", []),
        fyi=string_field(fields, "FYI", ""),
    )


def fire_from_fields(fields: dict[str, list[Any]]) -> FireDefinition:
    location = number_vector(fields, "LOCATION", [2.5, 2.5], length=2)

    return FireDefinition(
        id=required_string(fields, "ID", "fire ID"),
        comp_id=required_string(fields, "COMP_ID", "fire compartment"),
        fire_property_id=required_string(fields, "FIRE_ID", "fire property ID"),
        ignition_criterion=string_field(fields, "IGNITION_CRITERION", "TIME").upper(),
        setpoint=number_field(fields, "SETPOINT", 0.0),
        target=string_field(fields, "DEVC_ID", ""),
        x_position=location[0],
        y_position=location[1],
        fyi=string_field(fields, "FYI", ""),
    )


def fire_property_from_fields(fields: dict[str, list[Any]]) -> FireProperty:
    return FireProperty(
        id=required_string(fields, "ID", "CHEM ID"),
        carbon=int(number_field(fields, "CARBON", 1)),
        hydrogen=int(number_field(fields, "HYDROGEN", 4)),
        oxygen=int(number_field(fields, "OXYGEN", 0)),
        nitrogen=int(number_field(fields, "NITROGEN", 0)),
        chlorine=int(number_field(fields, "CHLORINE", 0)),
        heat_of_combustion=number_field(fields, "HEAT_OF_COMBUSTION", 50000.0),
        radiative_fraction=number_field(fields, "RADIATIVE_FRACTION", 0.35),
        fyi=string_field(fields, "FYI", ""),
    )


def add_table_data(
    fire_properties_by_id: dict[str, FireProperty],
    fields: dict[str, list[Any]],
) -> None:
    if "DATA" not in fields:
        return

    prop_id = required_string(fields, "ID", "TABL ID")
    prop = fire_properties_by_id.setdefault(prop_id, FireProperty(id=prop_id))
    data = number_vector(fields, "DATA", [])

    if len(data) < 2:
        return

    point = FireRampPoint(
        time=data[0],
        hrr=data[1],
        height=data[2] if len(data) > 2 else 0.0,
        area=data[3] if len(data) > 3 else 0.1,
        co_yield=data[4] if len(data) > 4 else 0.0,
        soot_yield=data[5] if len(data) > 5 else 0.01,
        hcn_yield=data[6] if len(data) > 6 else 0.0,
        trace_yield=data[8] if len(data) > 8 else (data[7] if len(data) > 7 else 0.0),
    )
    prop.ramp.append(point)


def add_device(
    case: CfastCase,
    fields: dict[str, list[Any]],
    warnings: list[str],
) -> None:
    device_type = string_field(fields, "TYPE", "PLATE").upper()

    if device_type in {"PLATE", "CYLINDER"}:
        case.targets.append(target_from_fields(fields, device_type))
    elif device_type in {"SPRINKLER", "SMOKE_DETECTOR", "HEAT_DETECTOR"}:
        case.detection_devices.append(detection_device_from_fields(fields, device_type))
    else:
        warnings.append(f"Unsupported DEVC type {device_type!r}.")


def target_from_fields(fields: dict[str, list[Any]], target_type: str) -> Target:
    location = number_vector(fields, "LOCATION", [0.0, 0.0, 0.0], length=3)
    normal = number_vector(fields, "NORMAL", [0.0, 0.0, 1.0], length=3)
    convection = number_vector(fields, "CONVECTION_COEFFICIENTS", [0.0, 0.0], length=2)

    return Target(
        id=required_string(fields, "ID", "target ID"),
        comp_id=required_string(fields, "COMP_ID", "target compartment"),
        x_position=location[0],
        y_position=location[1],
        z_position=location[2],
        x_normal=normal[0],
        y_normal=normal[1],
        z_normal=normal[2],
        matl_id=string_field(fields, "MATL_ID", "DEFAULT"),
        target_type=target_type,
        thickness=number_field(fields, "THICKNESS", 0.0),
        temperature_depth=number_field(fields, "TEMPERATURE_DEPTH", 0.5),
        depth_units=string_field(fields, "DEPTH_UNITS", "FRACTION").upper(),
        surface_orientation=string_field(
            fields,
            "SURFACE_ORIENTATION",
            "USER SPECIFIED",
        ),
        surface_temperature=optional_number_field(fields, "SURFACE_TEMPERATURE"),
        adiabatic=bool_field(fields, "ADIABATIC_TARGET", False),
        convection_coefficient_front=convection[0],
        convection_coefficient_back=convection[1],
        fyi=string_field(fields, "FYI", ""),
    )


def detection_device_from_fields(
    fields: dict[str, list[Any]],
    device_type: str,
) -> DetectionDevice:
    location = number_vector(fields, "LOCATION", [0.0, 0.0, 0.0], length=3)
    setpoint = number_field(fields, "SETPOINT", 73.88998)
    is_smoke = device_type == "SMOKE_DETECTOR"

    return DetectionDevice(
        id=required_string(fields, "ID", "device ID"),
        comp_id=required_string(fields, "COMP_ID", "device compartment"),
        device_type=device_type,
        x_position=location[0],
        y_position=location[1],
        z_position=location[2],
        activation_temperature=73.88998 if is_smoke else setpoint,
        activation_obscuration=setpoint if is_smoke else 23.93346,
        rti=number_field(fields, "RTI", 100.0),
        spray_density=number_field(fields, "SPRAY_DENSITY", 0.0),
        fyi=string_field(fields, "FYI", ""),
    )


def add_connection(case: CfastCase, fields: dict[str, list[Any]]) -> None:
    conn_type = string_field(fields, "TYPE", "WALL").upper()
    comp_ids = string_vector(fields, "COMP_IDS", [])
    comp_id = string_field(fields, "COMP_ID", "")

    if conn_type == "WALL":
        second = comp_ids[0] if comp_ids else ""
        case.wall_surface_connections.append(
            WallSurfaceConnection(
                first_comp_id=comp_id,
                second_comp_id=second,
                fraction=number_field(fields, "F", 1.0),
                connection_type="WALL",
                fyi=string_field(fields, "FYI", ""),
            )
        )
    elif conn_type in {"FLOOR", "CEILING"}:
        bottom = comp_ids[0] if comp_ids else ""
        case.ceiling_floor_surface_connections.append(
            CeilingFloorSurfaceConnection(
                top_comp_id=comp_id,
                bottom_comp_id=bottom,
                connection_type=conn_type,
                fyi=string_field(fields, "FYI", ""),
            )
        )


def output_visualization_from_fields(fields: dict[str, list[Any]]) -> OutputVisualization:
    domain = string_field(fields, "DOMAIN", "2-D").upper()
    plane = string_field(fields, "PLANE", "X").upper()

    return OutputVisualization(
        visualization_type=domain,
        comp_id=string_field(fields, "COMP_ID", "All"),
        axis=plane[0:1] or "X",
        value=number_field(fields, "POSITION", 0.0),
        fyi=string_field(fields, "FYI", ""),
    )


def required_string(fields: dict[str, list[Any]], key: str, label: str) -> str:
    value = string_field(fields, key, "")
    if not value:
        raise ValueError(f"{label} is required.")
    return value


def string_field(fields: dict[str, list[Any]], key: str, default: str) -> str:
    values = fields.get(key.upper())
    if not values:
        return default
    return str(values[0])


def number_field(fields: dict[str, list[Any]], key: str, default: float) -> float:
    value = optional_number_field(fields, key)
    return default if value is None else value


def optional_number_field(fields: dict[str, list[Any]], key: str) -> float | None:
    values = fields.get(key.upper())
    if not values:
        return None

    value = values[0]
    if isinstance(value, bool):
        return 1.0 if value else 0.0
    if isinstance(value, (int, float)):
        return float(value)

    match = _NUMBER_RE.search(str(value))
    if match is None:
        return None
    return float(match.group(0).replace("D", "E").replace("d", "e"))


def bool_field(fields: dict[str, list[Any]], key: str, default: bool) -> bool:
    values = fields.get(key.upper())
    if not values:
        return default

    value = values[0]
    if isinstance(value, bool):
        return value

    return str(value).strip().upper() in {".TRUE.", "TRUE", "T", "YES", "ON"}


def number_vector(
    fields: dict[str, list[Any]],
    key: str,
    default: list[float],
    length: int | None = None,
) -> list[float]:
    values = fields.get(key.upper())
    if not values:
        result = list(default)
    else:
        result = []
        for value in values:
            if isinstance(value, bool):
                result.append(1.0 if value else 0.0)
            elif isinstance(value, (int, float)):
                result.append(float(value))
            else:
                match = _NUMBER_RE.search(str(value))
                if match is not None:
                    result.append(float(match.group(0).replace("D", "E").replace("d", "e")))

    if length is not None:
        result = pad_numbers(result, default[-1] if default else 0.0, length)

    return result


def string_vector(
    fields: dict[str, list[Any]],
    key: str,
    default: list[str],
) -> list[str]:
    values = fields.get(key.upper())
    if not values:
        return list(default)
    return [str(value) for value in values]


def string_triple(
    fields: dict[str, list[Any]],
    key: str,
    default: str,
) -> tuple[str, str, str]:
    values = string_vector(fields, key, [default])
    values = pad_strings(values, values[-1] if values else default, 3)
    return (values[0], values[1], values[2])


def number_triple(
    fields: dict[str, list[Any]],
    key: str,
    default: float,
) -> tuple[float, float, float]:
    values = number_vector(fields, key, [default])
    values = pad_numbers(values, values[-1] if values else default, 3)
    return (values[0], values[1], values[2])


def pad_strings(values: list[str], default: str, length: int) -> list[str]:
    result = list(values)
    while len(result) < length:
        result.append(default)
    return result[:length]


def pad_numbers(values: list[float], default: float, length: int) -> list[float]:
    result = list(values)
    while len(result) < length:
        result.append(default)
    return result[:length]

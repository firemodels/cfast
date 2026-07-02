from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class MaterialProperty:
    id: str
    material: str
    conductivity: float
    specific_heat: float
    density: float
    thickness: float
    emissivity: float = 0.9
    fyi: str = ""


@dataclass
class Compartment:
    id: str
    width: float
    depth: float
    height: float
    origin_x: float = 0.0
    origin_y: float = 0.0
    origin_z: float = 0.0
    ceiling_matl_id: tuple[str, str, str] = ("OFF", "OFF", "OFF")
    ceiling_thickness: tuple[float, float, float] = (0.0, 0.0, 0.0)
    wall_matl_id: tuple[str, str, str] = ("OFF", "OFF", "OFF")
    wall_thickness: tuple[float, float, float] = (0.0, 0.0, 0.0)
    floor_matl_id: tuple[str, str, str] = ("OFF", "OFF", "OFF")
    floor_thickness: tuple[float, float, float] = (0.0, 0.0, 0.0)
    grid: tuple[int, int, int] = (50, 50, 50)
    hall: bool = False
    shaft: bool = False
    flow_coefficient: float = 0.07
    wall_leak_area_ratio: float = 0.0
    floor_leak_area_ratio: float = 0.0
    wall_leak_area: float = 0.0
    floor_leak_area: float = 0.0
    cross_section_heights: list[float] = field(default_factory=list)
    cross_section_areas: list[float] = field(default_factory=list)
    fyi: str = ""
    fire_count: int = 0
    hvent_count: int = 0
    vent_count: int = 0
    mechanical_count: int = 0
    detector_count: int = 0
    target_count: int = 0


@dataclass
class WallVent:
    id: str
    first_comp_id: str
    second_comp_id: str = "OUTSIDE"
    bottom: float = 0.0
    height: float = 2.0
    width: float = 1.0
    initial_open: float = 1.0
    face: str = "FRONT"
    offset: float = 2.0
    criterion: str = "TIME"
    t_values: list[float] = field(default_factory=list)
    f_values: list[float] = field(default_factory=list)
    fyi: str = ""


@dataclass(init=False)
class CeilingFloorVent:
    id: str
    first_comp_id: str
    second_comp_id: str
    vent_type: str = "CEILING"
    area: float = 1.0
    shape: str = "ROUND"
    initial_open: float = 1.0
    offset_x: float = 2.5
    offset_y: float = 1.0
    criterion: str = "TIME"
    t_values: list[float] = field(default_factory=list)
    f_values: list[float] = field(default_factory=list)
    fyi: str = ""

    def __init__(
        self,
        id: str,
        first_comp_id: str | None = None,
        second_comp_id: str | None = None,
        vent_type: str = "CEILING",
        area: float = 1.0,
        shape: str = "ROUND",
        initial_open: float = 1.0,
        offset_x: float = 2.5,
        offset_y: float = 1.0,
        criterion: str = "TIME",
        t_values: list[float] | None = None,
        f_values: list[float] | None = None,
        fyi: str = "",
        top_comp_id: str | None = None,
        bottom_comp_id: str | None = None,
    ):
        self.id = id
        self.first_comp_id = first_comp_id or top_comp_id or ""
        self.second_comp_id = second_comp_id or bottom_comp_id or ""
        self.vent_type = vent_type
        self.area = area
        self.shape = shape
        self.initial_open = initial_open
        self.offset_x = offset_x
        self.offset_y = offset_y
        self.criterion = criterion
        self.t_values = list(t_values) if t_values is not None else []
        self.f_values = list(f_values) if f_values is not None else []
        self.fyi = fyi

    @property
    def top_comp_id(self) -> str:
        return self.first_comp_id

    @top_comp_id.setter
    def top_comp_id(self, value: str) -> None:
        self.first_comp_id = value

    @property
    def bottom_comp_id(self) -> str:
        return self.second_comp_id

    @bottom_comp_id.setter
    def bottom_comp_id(self, value: str) -> None:
        self.second_comp_id = value


@dataclass
class MechanicalVent:
    id: str
    from_comp_id: str
    to_comp_id: str
    from_area: float = 0.25
    from_height: float = 2.75
    from_orientation: str = "VERTICAL"
    to_area: float = 0.25
    to_height: float = 2.75
    to_orientation: str = "VERTICAL"
    flow: float = 0.02
    begin_dropoff: float = 200.0
    zero_flow: float = 300.0
    offset_x: float = 0.0
    offset_y: float = 4.0
    filter_efficiency: float = 0.0
    filter_time: float = 0.0
    criterion: str = "TIME"
    t_values: list[float] = field(default_factory=list)
    f_values: list[float] = field(default_factory=list)
    fyi: str = ""


@dataclass
class Target:
    id: str
    comp_id: str
    x_position: float = 0.0
    y_position: float = 0.0
    z_position: float = 0.0
    x_normal: float = 0.0
    y_normal: float = 0.0
    z_normal: float = 1.0
    matl_id: str = "DEFAULT"
    target_type: str = "PLATE"
    thickness: float = 0.0
    temperature_depth: float = 0.5
    depth_units: str = "FRACTION"
    surface_orientation: str = "USER SPECIFIED"
    surface_temperature: float | None = None
    adiabatic: bool = False
    convection_coefficient_front: float = 0.0
    convection_coefficient_back: float = 0.0
    fyi: str = ""


@dataclass
class DetectionDevice:
    id: str
    comp_id: str
    device_type: str = "SPRINKLER"
    x_position: float = 3.0
    y_position: float = 3.0
    z_position: float = 2.97
    activation_temperature: float = 73.88998
    activation_obscuration: float = 23.93346
    rti: float = 100.0
    spray_density: float = 7.0e-5
    fyi: str = ""

    def display_type(self) -> str:
        value = self.device_type.upper()
        if value == "SPRINKLER":
            return "Sprinkler"
        if value == "SMOKE_DETECTOR":
            return "Smoke"
        if value == "HEAT_DETECTOR":
            return "Heat"
        return self.device_type


@dataclass
class WallSurfaceConnection:
    first_comp_id: str
    second_comp_id: str
    fraction: float = 1.0
    connection_type: str = "WALL"
    fyi: str = ""


@dataclass
class CeilingFloorSurfaceConnection:
    top_comp_id: str
    bottom_comp_id: str
    connection_type: str = "FLOOR"
    fyi: str = ""


@dataclass
class OutputVisualization:
    visualization_type: str = "2-D"
    comp_id: str = "All"
    axis: str = "X"
    value: float = 2.5
    fyi: str = ""


@dataclass
class FireRampPoint:
    time: float
    hrr: float
    height: float = 0.0
    area: float = 0.1
    co_yield: float = 0.0
    soot_yield: float = 0.01
    hcn_yield: float = 0.0
    trace_yield: float = 0.0


@dataclass
class FireProperty:
    id: str
    carbon: int = 1
    hydrogen: int = 4
    oxygen: int = 0
    nitrogen: int = 0
    chlorine: int = 0
    heat_of_combustion: float = 50000.0
    radiative_fraction: float = 0.35
    ramp: list[FireRampPoint] = field(default_factory=list)
    fyi: str = ""

    def sorted_ramp(self) -> list[FireRampPoint]:
        return sorted(self.ramp, key=lambda point: point.time)

    def peak_hrr(self) -> float:
        if not self.ramp:
            return 0.0
        return max(point.hrr for point in self.ramp)

    def fuel_formula(self) -> str:
        parts = []
        if self.carbon:
            parts.append(f"C{self.carbon}")
        if self.hydrogen:
            parts.append(f"H{self.hydrogen}")
        if self.oxygen:
            parts.append(f"O{self.oxygen}")
        if self.nitrogen:
            parts.append(f"N{self.nitrogen}")
        if self.chlorine:
            parts.append(f"Cl{self.chlorine}")
        return "".join(parts) if parts else "Unknown"


@dataclass
class FireDefinition:
    id: str
    comp_id: str
    fire_property_id: str
    ignition_criterion: str = "TIME"
    setpoint: float = 0.0
    target: str = ""
    x_position: float = 2.5
    y_position: float = 2.5
    fyi: str = ""


@dataclass
class CfastCase:
    title: str = "CFAST Simulation"
    version: int = 7600

    simulation_time: float = 3600.0
    print_interval: float = 60.0
    smokeview_interval: float = 60.0
    spreadsheet_interval: float = 60.0
    max_time_step: float | None = None

    pressure: float = 101325.0
    relative_humidity: float = 50.0
    interior_temperature: float = 20.0
    exterior_temperature: float = 20.0

    adiabatic_surfaces: bool = False
    lower_oxygen_limit: float = 0.1

    materials: list[MaterialProperty] = field(default_factory=list)
    compartments: list[Compartment] = field(default_factory=list)
    wall_vents: list[WallVent] = field(default_factory=list)
    ceiling_floor_vents: list[CeilingFloorVent] = field(default_factory=list)
    mechanical_vents: list[MechanicalVent] = field(default_factory=list)
    targets: list[Target] = field(default_factory=list)
    detection_devices: list[DetectionDevice] = field(default_factory=list)
    wall_surface_connections: list[WallSurfaceConnection] = field(default_factory=list)
    ceiling_floor_surface_connections: list[CeilingFloorSurfaceConnection] = field(default_factory=list)
    output_visualizations: list[OutputVisualization] = field(default_factory=list)
    net_heat_flux_output: bool = False
    validation_output: bool = False
    debug_output: bool = False
    show_cfast_window: bool = False
    spreadsheet_output_compartments: bool = True
    spreadsheet_output_devices: bool = True
    spreadsheet_output_masses: bool = True
    spreadsheet_output_vents: bool = True
    spreadsheet_output_walls: bool = True
    extra_namelists: list[str] = field(default_factory=list)
    fires: list[FireDefinition] = field(default_factory=list)
    fire_properties: list[FireProperty] = field(default_factory=list)

    # Kept for compatibility with early prototype code paths.
    comp_id: str = "Comp 1"
    fire_id: str = "Initial Fire"
    fire_chem_id: str = "Initial Fire_Fire"
    fire_location_x: float = 2.5
    fire_location_y: float = 2.5
    carbon: int = 1
    chlorine: int = 0
    hydrogen: int = 4
    nitrogen: int = 0
    oxygen: int = 0
    heat_of_combustion: float = 50000.0
    radiative_fraction: float = 0.35
    fire_ramp: list[FireRampPoint] = field(default_factory=list)

    def sorted_fire_ramp(self) -> list[FireRampPoint]:
        return sorted(self.fire_ramp, key=lambda point: point.time)

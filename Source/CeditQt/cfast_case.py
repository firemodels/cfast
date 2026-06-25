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
    wall_leak_area_ratio: float = 0.00017
    floor_leak_area_ratio: float = 5.2e-5
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


@dataclass
class CeilingFloorVent:
    id: str
    top_comp_id: str
    bottom_comp_id: str
    area: float = 1.0
    shape: str = "ROUND"
    offset_x: float = 2.5
    offset_y: float = 1.0
    criterion: str = "TIME"
    t_values: list[float] = field(default_factory=list)
    f_values: list[float] = field(default_factory=list)
    fyi: str = ""


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
    title: str = "CEdit Qt Prototype Case"
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

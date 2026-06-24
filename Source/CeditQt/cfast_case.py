from dataclasses import dataclass, field


@dataclass
class FireRampPoint:
    time: float
    hrr: float
    height: float = 0.0
    area: float = 0.3
    co_yield: float = 0.0
    soot_yield: float = 0.01
    hcn_yield: float = 0.0
    hcl_yield: float = 0.0
    trace_yield: float = 0.0


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

    comp_id: str = "Comp 1"
    comp_width: float = 5.0
    comp_depth: float = 5.0
    comp_height: float = 5.0
    comp_origin_x: float = 0.0
    comp_origin_y: float = 0.0
    comp_origin_z: float = 0.0
    comp_grid: tuple[int, int, int] = (50, 50, 50)

    wall_vent_id: str = "WallVent_1"
    wall_vent_bottom: float = 0.0
    wall_vent_height: float = 2.0
    wall_vent_width: float = 1.0
    wall_vent_face: str = "FRONT"
    wall_vent_offset: float = 2.0

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

from __future__ import annotations

import math
import re
from dataclasses import dataclass


LENGTH = "length"
MASS = "mass"
TIME = "time"
TEMPERATURE = "temperature"
TEMPERATURE_RISE = "temperature_rise"
PRESSURE = "pressure"
ENERGY = "energy"
SMOKE = "smoke"

AREA = "area"
VELOCITY = "velocity"
FLOWRATE = "flowrate"
RTI = "rti"
MASS_LOSS = "mass_loss"
DENSITY = "density"
HRR = "hrr"
HOC = "hoc"
HOG = "hog"
HEAT_FLUX = "heat_flux"
CONDUCTIVITY = "conductivity"
SPECIFIC_HEAT = "specific_heat"


BASE_UNIT_KEYS = (LENGTH, MASS, TIME, TEMPERATURE, PRESSURE, ENERGY, SMOKE)

_NUMBER_RE = re.compile(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?")


@dataclass(frozen=True)
class BaseUnit:
    label: str
    multiplier: float
    offset: float = 0.0


@dataclass(frozen=True)
class Conversion:
    multiplier: float
    offset: float
    label: str
    smoke: bool = False

    def to_si(self, value: float) -> float:
        if self.smoke and self.multiplier != 1.0:
            return 100.0 * (1.0 - (1.0 - value / 100.0) ** (1.0 / self.multiplier))
        return (value + self.offset) * self.multiplier

    def from_si(self, value: float) -> float:
        if self.smoke and self.multiplier != 1.0:
            return 100.0 * (1.0 - (1.0 - value / 100.0) ** self.multiplier)
        return value / self.multiplier - self.offset


BASE_UNITS: dict[str, list[BaseUnit]] = {
    LENGTH: [
        BaseUnit("m", 1.0),
        BaseUnit("cm", 0.01),
        BaseUnit("mm", 0.001),
        BaseUnit("ft", 0.3048),
        BaseUnit("in", 0.0254),
    ],
    MASS: [
        BaseUnit("kg", 1.0),
        BaseUnit("g", 0.001),
        BaseUnit("lb", 0.4535924),
        BaseUnit("oz", 0.02834952),
    ],
    TIME: [
        BaseUnit("s", 1.0),
        BaseUnit("min", 60.0),
        BaseUnit("h", 3600.0),
    ],
    TEMPERATURE: [
        BaseUnit("C", 1.0, 273.15),
        BaseUnit("K", 1.0, 0.0),
        BaseUnit("F", 5.0 / 9.0, 459.67),
        BaseUnit("R", 5.0 / 9.0, 0.0),
    ],
    PRESSURE: [
        BaseUnit("Pa", 1.0),
        BaseUnit("mm Hg", 133.3224),
        BaseUnit("in H2O", 249.0889),
        BaseUnit("atm", 101325.0),
    ],
    ENERGY: [
        BaseUnit("kJ", 1000.0),
        BaseUnit("J", 1.0),
        BaseUnit("MJ", 1000000.0),
        BaseUnit("BTU", 1054.35),
        BaseUnit("cal", 4.184),
    ],
    SMOKE: [
        BaseUnit("%/m", 1.0),
        BaseUnit("%/ft", 0.3048),
    ],
}


def format_number(value: float | int) -> str:
    if isinstance(value, int):
        return str(value)

    value = float(value)
    if abs(value - round(value)) < 1.0e-12:
        return str(int(round(value)))
    return f"{value:.10g}"


def parse_number(
    text: str,
    field_name: str,
    default: float | None = None,
    allow_default: bool = False,
) -> float | None:
    text = text.strip()

    if allow_default and text.lower() == "default":
        return None
    if not text and default is not None:
        return default

    match = _NUMBER_RE.search(text)
    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


class UnitSystem:
    """Engineering-unit conversions matching the legacy CEdit unit table."""

    def __init__(self):
        self.selected: dict[str, int] = {key: 0 for key in BASE_UNIT_KEYS}
        self.conversions: dict[str, Conversion] = {}
        self.refresh()

    def choices(self, key: str) -> list[str]:
        return [unit.label for unit in BASE_UNITS[key]]

    def label(self, kind: str) -> str:
        return self.conversions[kind].label

    def set_index(self, key: str, index: int) -> None:
        index = max(0, min(index, len(BASE_UNITS[key]) - 1))
        self.selected[key] = index
        self.refresh()

    def set_indices(self, indices: dict[str, int]) -> None:
        for key, index in indices.items():
            if key in self.selected:
                self.selected[key] = max(0, min(index, len(BASE_UNITS[key]) - 1))
        self.refresh()

    def load_settings(self, settings) -> None:
        values = {}
        for key in BASE_UNIT_KEYS:
            values[key] = settings.value(f"units/{key}", 0, type=int)
        self.set_indices(values)

    def save_settings(self, settings) -> None:
        for key in BASE_UNIT_KEYS:
            settings.setValue(f"units/{key}", self.selected[key])

    def to_si(self, kind: str, value: float) -> float:
        return self.conversions[kind].to_si(value)

    def from_si(self, kind: str, value: float) -> float:
        return self.conversions[kind].from_si(value)

    def refresh(self) -> None:
        length = BASE_UNITS[LENGTH][self.selected[LENGTH]]
        mass = BASE_UNITS[MASS][self.selected[MASS]]
        time = BASE_UNITS[TIME][self.selected[TIME]]
        temperature = BASE_UNITS[TEMPERATURE][self.selected[TEMPERATURE]]
        pressure = BASE_UNITS[PRESSURE][self.selected[PRESSURE]]
        energy = BASE_UNITS[ENERGY][self.selected[ENERGY]]
        smoke = BASE_UNITS[SMOKE][self.selected[SMOKE]]

        l_length = length.label
        l_mass = mass.label
        l_time = time.label
        l_temperature = temperature.label
        l_pressure = pressure.label
        l_energy = energy.label
        l_smoke = smoke.label

        hrr_label = f"{l_energy}/{l_time}"
        if hrr_label == "J/s":
            hrr_label = "W"
        elif hrr_label == "kJ/s":
            hrr_label = "kW"
        elif hrr_label == "MJ/s":
            hrr_label = "MW"

        heat_flux_label = f"{l_energy}/({l_time} {l_length}2)"
        if heat_flux_label == "J/(s m2)":
            heat_flux_label = "W/m2"
        elif heat_flux_label == "kJ/(s m2)":
            heat_flux_label = "kW/m2"
        elif heat_flux_label == "MJ/(s m2)":
            heat_flux_label = "MW/m2"

        conductivity_prefix = f"{l_energy}/({l_time} "
        if conductivity_prefix == "J/(s ":
            conductivity_prefix = "W/("
        elif conductivity_prefix == "kJ/(s ":
            conductivity_prefix = "kW/("
        elif conductivity_prefix == "MJ/(s ":
            conductivity_prefix = "MW/("

        self.conversions = {
            TIME: Conversion(time.multiplier, 0.0, l_time),
            TEMPERATURE: Conversion(
                temperature.multiplier,
                temperature.offset,
                l_temperature,
            ),
            TEMPERATURE_RISE: Conversion(temperature.multiplier, 0.0, l_temperature),
            PRESSURE: Conversion(pressure.multiplier, 0.0, l_pressure),
            LENGTH: Conversion(length.multiplier, 0.0, l_length),
            SMOKE: Conversion(smoke.multiplier, 0.0, l_smoke, smoke=True),
            AREA: Conversion(length.multiplier * length.multiplier, 0.0, f"{l_length}2"),
            VELOCITY: Conversion(
                length.multiplier / time.multiplier,
                0.0,
                f"{l_length}/{l_time}",
            ),
            FLOWRATE: Conversion(
                length.multiplier ** 3 / time.multiplier,
                0.0,
                f"{l_length}^3/{l_time}",
            ),
            RTI: Conversion(
                math.sqrt(length.multiplier * time.multiplier),
                0.0,
                f"({l_length} {l_time})^0.5",
            ),
            MASS: Conversion(mass.multiplier, 0.0, l_mass),
            MASS_LOSS: Conversion(
                mass.multiplier / time.multiplier,
                0.0,
                f"{l_mass}/{l_time}",
            ),
            DENSITY: Conversion(
                mass.multiplier / length.multiplier ** 3,
                0.0,
                f"{l_mass}/{l_length}^3",
            ),
            HRR: Conversion(energy.multiplier / time.multiplier, 0.0, hrr_label),
            HOC: Conversion(
                energy.multiplier / mass.multiplier,
                0.0,
                f"{l_energy}/{l_mass}",
            ),
            HOG: Conversion(
                energy.multiplier / mass.multiplier,
                0.0,
                f"{l_energy}/{l_mass}",
            ),
            HEAT_FLUX: Conversion(
                energy.multiplier / (time.multiplier * length.multiplier ** 2),
                0.0,
                heat_flux_label,
            ),
            CONDUCTIVITY: Conversion(
                energy.multiplier
                / (time.multiplier * length.multiplier * temperature.multiplier),
                0.0,
                f"{conductivity_prefix}{l_length} {l_temperature})",
            ),
            SPECIFIC_HEAT: Conversion(
                energy.multiplier / (mass.multiplier * temperature.multiplier),
                0.0,
                f"{l_energy}/({l_mass} {l_temperature})",
            ),
        }


unit_system = UnitSystem()


_MODEL_TO_SI_FACTOR = {
    HRR: 1000.0,  # CFAST input uses kW.
    HOC: 1000.0,  # CFAST input uses kJ/kg.
    HOG: 1000.0,
    HEAT_FLUX: 1000.0,  # CFAST input uses kW/m2.
    SPECIFIC_HEAT: 1000.0,  # CFAST input uses kJ/(kg K).
}


def model_to_si(kind: str, value: float) -> float:
    if kind == TEMPERATURE:
        return value + 273.15
    return value * _MODEL_TO_SI_FACTOR.get(kind, 1.0)


def si_to_model(kind: str, value: float) -> float:
    if kind == TEMPERATURE:
        return value - 273.15
    return value / _MODEL_TO_SI_FACTOR.get(kind, 1.0)


def display_value(kind: str, model_value: float) -> float:
    return unit_system.from_si(kind, model_to_si(kind, model_value))


def model_value(kind: str, display: float) -> float:
    return si_to_model(kind, unit_system.to_si(kind, display))


def format_value(kind: str, model_value: float | int, include_unit: bool = True) -> str:
    value = display_value(kind, float(model_value))
    text = format_number(value)
    if include_unit:
        return f"{text} {unit_system.label(kind)}"
    return text


def parse_value(
    kind: str,
    text: str,
    field_name: str,
    default: float | None = None,
    allow_default: bool = False,
) -> float | None:
    stripped = text.strip()
    if allow_default and stripped.lower() == "default":
        return None
    if not stripped and default is not None:
        return default

    value = parse_number(stripped, field_name)
    if value is None:
        return None
    return model_value(kind, value)


def unit_label(kind: str) -> str:
    return unit_system.label(kind)

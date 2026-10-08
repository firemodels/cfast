"""Check the multi-room E_COEFFICIENT case against the analytical solution."""
from __future__ import annotations

import argparse
import csv
import math
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[3]


def read_results(path):
    with path.open(newline="") as stream:
        reader = csv.reader(stream)
        headers = next(reader)
        for _ in range(3):
            next(reader)
        return {name: list(values) for name, values in zip(
            headers, zip(*([float(value) for value in row] for row in reader if row)))}


def verify(case_path: Path):
    case_path = case_path.with_suffix("")
    data = read_results(case_path.with_name(case_path.name + "_compartments.csv"))
    devices = read_results(case_path.with_name(case_path.name + "_devices.csv"))
    lines = case_path.with_suffix(".smv").read_text().splitlines()
    activation = {}
    for index, line in enumerate(lines[:-1]):
        if line.strip() == "DEVICE_ACT":
            device, time, state = lines[index + 1].split()
            if int(state) == 1:
                activation[int(device)] = float(time)
    if set(activation) != {1, 2} or not all(1 < time < 15 for time in activation.values()):
        raise AssertionError(f"Expected two sprinklers to activate before the delayed fire: {activation}")
    # Smokeview reports times to 0.01 s. Independently check the sensor transition.
    for device, time in activation.items():
        first = next(i for i, value in enumerate(devices[f"SENSACT_{device}"]) if value > 0.5)
        if not devices["Time"][first - 1] - 0.005 <= time <= devices["Time"][first] + 0.005:
            raise AssertionError("Sprinkler activation does not match device output")

    # Fire order and inputs are deliberately fixed to the verification case.
    names = ["Reference", "Stronger", "No attenuation", "Legacy", "Second room", "Growing", "Delayed", "Dry room"]
    coefficients = [1.0, 2.0, 0.0, None, 1.0, 0.5, 0.1, 1.0]
    mass_fluxes = [0.1] * 4 + [0.2] * 3 + [0.0]  # kg/(m2 s)
    expected = {}
    errors = {}
    output_step = data["Time"][1] - data["Time"][0]
    for fire, (name, coefficient, flux) in enumerate(zip(names, coefficients, mass_fluxes), 1):
        values = []
        for time in data["Time"]:
            baseline = 18000 + 600 * time if fire == 6 else 36000.0
            if fire == 7 and time < 15:
                baseline = 0.0
            wet_time = max(0.0, time - activation[1 if fire <= 4 else 2]) if flux else 0.0
            if coefficient is None:
                tau = 3.0 / flux ** 1.8  # Legacy correlation uses the numerical mm/s value.
                factor = math.exp(-wet_time / tau)
            else:
                factor = math.exp(-0.5 * coefficient * flux * wet_time ** 2)
            values.append(baseline * factor * min(time, 1.0))  # Existing adiabatic startup filter.
        expected[f"Expected_{fire}"] = values
        peak = 54000 if fire == 6 else 36000
        # 0.2% of unsuppressed peak allows for 0.01 s activation-time output precision.
        # Check prescribed HRR, actual HRR, and fuel mass loss (50 MJ/kg).
        # CFAST updates ignition after the flow calculation. Exclude the two
        # output intervals straddling that discontinuity, not the ensuing decay.
        compare = [not (fire == 7 and 15 <= time <= 15 + 2 * output_step + 1e-8)
                   for time in data["Time"]]
        errors[name] = max(
            max(abs(actual * scale - target) for actual, target, include in zip(data[column], values, compare)
                if include) / peak
            for column, scale in [(f"HRR_E{fire}", 1), (f"HRR_{fire}", 1), (f"PYROL_{fire}", 5e7)]
        )
        if errors[name] > 0.002:
            raise AssertionError(f"{name}: normalized HRR/mass-loss error {errors[name]:.6g} exceeds 0.002")
    output = case_path.with_name(case_path.name + "_expected.csv")
    with output.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["Time", *expected])
        # One-second samples keep the dataplot analytical markers legible.
        stride = max(1, round(1.0 / output_step))
        writer.writerows(zip(data["Time"][::stride], *(values[::stride] for values in expected.values())))

    print(f"E_COEFFICIENT: all 8 fires passed; maximum normalized error {max(errors.values()):.6g}")
    return errors


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", type=Path, default=REPO_ROOT / "Verification/Sprinkler/e_coefficient.in")
    args = parser.parse_args()
    verify(args.case)

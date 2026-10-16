from __future__ import annotations

import math
import os
import sys
from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtGui import QFont, QFontDatabase, QTextCursor
from PySide6.QtWidgets import (
    QCheckBox,
    QFrame,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPlainTextEdit,
    QVBoxLayout,
    QWidget,
)

from table_widgets import UnitLineEdit

from cfast_case import CfastCase
from cfast_syntax_highlighter import CfastSyntaxHighlighter
from units import (
    MASS_FRACTION, PRESSURE, SPECIFIC_EXTINCTION, TEMPERATURE, TIME,
    display_value, format_number, format_value, parse_number, parse_value, unit_label,
)


def format_percent(value: float | int) -> str:
    return f"{format_number(value)} %"


def format_mol_fraction(value: float | int) -> str:
    return f"{format_number(value)} mol/mol"


def format_mass_fraction(value: float | int) -> str:
    return format_value(MASS_FRACTION, value)


def fixed_width_font() -> QFont:
    if sys.platform == "win32":
        # Preserve Windows' Courier appearance without the offscreen bitmap fallback.
        font_dir = Path(os.environ.get("WINDIR", r"C:\Windows")) / "Fonts"
        font_path = font_dir / "cour.ttf"
        font_id = QFontDatabase.addApplicationFont(str(font_path))
        families = QFontDatabase.applicationFontFamilies(font_id)
        if not families:
            raise RuntimeError(f"Could not load Courier New TrueType font {font_path}")
        if (font_dir / "courbd.ttf").is_file():
            QFontDatabase.addApplicationFont(str(font_dir / "courbd.ttf"))
        font = QFont(families[0])
    elif "Menlo" in QFontDatabase.families():
        font = QFont("Menlo")
    else:
        font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
    font.setPointSize(12)
    font.setStyleHint(QFont.StyleHint.Monospace)
    font.setFixedPitch(True)
    font.setStyleStrategy(QFont.StyleStrategy.PreferAntialias | QFont.StyleStrategy.PreferOutline)
    return font


class SimulationTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.title_edit = QLineEdit("CFAST Simulation")

        self.simulation_time_edit = UnitLineEdit(format_value(TIME, 3600.0))
        self.print_interval_edit = UnitLineEdit(format_value(TIME, 60.0))
        self.spreadsheet_interval_edit = UnitLineEdit(format_value(TIME, 15.0))
        self.smokeview_interval_edit = UnitLineEdit(format_value(TIME, 15.0))
        self.max_time_step_edit = UnitLineEdit("Default")

        self.interior_temperature_edit = UnitLineEdit(format_value(TEMPERATURE, 20.0))
        self.relative_humidity_edit = UnitLineEdit(format_percent(50.0))
        self.exterior_temperature_edit = UnitLineEdit(format_value(TEMPERATURE, 20.0))
        self.pressure_edit = UnitLineEdit()
        self.set_pressure_value(101325.0)
        self.interior_o2_mass_fraction_edit = UnitLineEdit(format_mass_fraction(0.23))
        self.exterior_o2_mass_fraction_edit = UnitLineEdit(format_mass_fraction(0.23))

        self.adiabatic_checkbox = QCheckBox("Adiabatic Compartment Surfaces")
        self.lower_oxygen_limit_edit = UnitLineEdit(format_mol_fraction(0.15))
        self.max_iteration_edit = QLineEdit("-1")
        self.max_iteration_edit.setToolTip("Use -1 for unlimited solver iterations.")
        self.overwrite_checkbox = QCheckBox("Overwrite Existing Output Files")
        self.overwrite_checkbox.setChecked(True)
        self.flaming_extinction_edit = UnitLineEdit()
        self.smoldering_extinction_edit = UnitLineEdit()
        self._extinction_values: dict[QLineEdit, tuple[float, str]] = {}
        self.set_extinction_value(self.flaming_extinction_edit, 8700.0)
        self.set_extinction_value(self.smoldering_extinction_edit, 4400.0)

        self.message_panel = QPlainTextEdit()
        self.message_panel.setReadOnly(True)
        self.message_panel.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)

        self.message_panel.setFont(fixed_width_font())

        self.message_panel.setPlainText("")
        self.message_panel.setMinimumHeight(140)
        self.syntax_highlighter = CfastSyntaxHighlighter(self.message_panel.document())

        self.build_layout()
        self.connect_unit_normalizers()

    def build_layout(self):
        main_layout = QVBoxLayout()

        title_layout = QHBoxLayout()
        title_layout.addStretch(1)
        title_layout.addWidget(QLabel("Title:"))
        title_layout.addWidget(self.title_edit, 3)
        title_layout.addStretch(1)

        main_layout.addLayout(title_layout)

        middle_layout = QHBoxLayout()
        middle_layout.addStretch(1)
        middle_layout.addWidget(self.build_times_group())
        middle_layout.addSpacing(40)
        middle_layout.addWidget(self.build_conditions_group())
        middle_layout.addStretch(1)

        main_layout.addSpacing(15)
        main_layout.addLayout(middle_layout)
        main_layout.addWidget(self.build_misc_group())
        main_layout.addSpacing(15)

        self.message_panel.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        main_layout.addWidget(self.message_panel, 1)

        self.setLayout(main_layout)

    def load_case(self, case: CfastCase):
        self.title_edit.setText(case.title)
        self.simulation_time_edit.setText(format_value(TIME, case.simulation_time))
        self.print_interval_edit.setText(format_value(TIME, case.print_interval))
        self.spreadsheet_interval_edit.setText(format_value(TIME, case.spreadsheet_interval))
        self.smokeview_interval_edit.setText(format_value(TIME, case.smokeview_interval))
        self.max_time_step_edit.setText(
            "Default" if case.max_time_step is None else format_value(TIME, case.max_time_step)
        )
        self.interior_temperature_edit.setText(format_value(TEMPERATURE, case.interior_temperature))
        self.relative_humidity_edit.setText(format_percent(case.relative_humidity))
        self.exterior_temperature_edit.setText(format_value(TEMPERATURE, case.exterior_temperature))
        self.set_pressure_value(case.pressure)
        self.interior_o2_mass_fraction_edit.setText(
            format_mass_fraction(case.interior_o2_mass_fraction)
        )
        self.exterior_o2_mass_fraction_edit.setText(
            format_mass_fraction(case.exterior_o2_mass_fraction)
        )
        self.adiabatic_checkbox.setChecked(case.adiabatic_surfaces)
        self.lower_oxygen_limit_edit.setText(format_mol_fraction(case.lower_oxygen_limit))
        self.max_iteration_edit.setText(str(case.max_iteration))
        self.overwrite_checkbox.setChecked(case.overwrite)
        self.set_extinction_value(self.flaming_extinction_edit, case.specific_extinction[0])
        self.set_extinction_value(self.smoldering_extinction_edit, case.specific_extinction[1])

    def build_times_group(self):
        group = QGroupBox("Simulation Times")
        layout = QGridLayout()

        labels_and_widgets = [
            ("Simulation Time:", self.simulation_time_edit),
            ("Text Output Interval:", self.print_interval_edit),
            ("Spreadsheet Output Interval:", self.spreadsheet_interval_edit),
            ("Smokeview Output Interval:", self.smokeview_interval_edit),
        ]

        for row, (label, widget) in enumerate(labels_and_widgets):
            layout.addWidget(QLabel(label), row, 0, alignment=Qt.AlignmentFlag.AlignRight)
            layout.addWidget(widget, row, 1)

        layout.setColumnStretch(0, 1)
        layout.setColumnStretch(1, 1)
        group.setLayout(layout)
        group.setMinimumWidth(300)

        return group

    def build_conditions_group(self):
        group = QGroupBox("Ambient Conditions")
        layout = QGridLayout()
        fields = [
            ("Interior Temperature:", self.interior_temperature_edit),
            ("Exterior Temperature:", self.exterior_temperature_edit),
            ("Humidity:", self.relative_humidity_edit),
            ("Pressure:", self.pressure_edit),
            ("Interior O₂ Mass Fraction:", self.interior_o2_mass_fraction_edit),
            ("Exterior O₂ Mass Fraction:", self.exterior_o2_mass_fraction_edit),
        ]
        for row, (label, widget) in enumerate(fields):
            layout.addWidget(QLabel(label), row, 0, alignment=Qt.AlignmentFlag.AlignRight)
            layout.addWidget(widget, row, 1)

        group.setLayout(layout)
        group.setMinimumWidth(380)

        return group

    def build_misc_group(self):
        group = QGroupBox("Miscellaneous")
        layout = QGridLayout()
        layout.addWidget(self.adiabatic_checkbox, 0, 0, 1, 2)
        layout.addWidget(self.overwrite_checkbox, 0, 2, 1, 2)
        fields = [
            ("Lower Oxygen Limit:", self.lower_oxygen_limit_edit, 1, 0),
            ("Maximum Solver Iterations:", self.max_iteration_edit, 2, 0),
            ("Maximum Time Step:", self.max_time_step_edit, 3, 0),
            ("Flaming Extinction Coefficient:", self.flaming_extinction_edit, 1, 2),
            ("Smoldering Extinction Coefficient:", self.smoldering_extinction_edit, 2, 2),
        ]
        for label, widget, row, col in fields:
            layout.addWidget(QLabel(label), row, col, alignment=Qt.AlignmentFlag.AlignRight)
            layout.addWidget(widget, row, col + 1)
        layout.setColumnStretch(1, 1)
        layout.setColumnStretch(3, 1)
        layout.setHorizontalSpacing(20)
        group.setLayout(layout)
        return group

    def connect_unit_normalizers(self):
        for edit, kind, field_name, allow_default in (
            (self.simulation_time_edit, TIME, "Simulation Time", False),
            (self.print_interval_edit, TIME, "Text Output Interval", False),
            (self.spreadsheet_interval_edit, TIME, "Spreadsheet Output Interval", False),
            (self.smokeview_interval_edit, TIME, "Smokeview Output Interval", False),
            (self.max_time_step_edit, TIME, "Maximum Time Step", True),
            (self.interior_temperature_edit, TEMPERATURE, "Interior Temperature", False),
            (self.exterior_temperature_edit, TEMPERATURE, "Exterior Temperature", False),
        ):
            edit.editingFinished.connect(
                lambda edit=edit, kind=kind, field_name=field_name, allow_default=allow_default: (
                    self.normalize_value_edit(edit, kind, field_name, allow_default)
                )
            )
        for edit in (self.flaming_extinction_edit, self.smoldering_extinction_edit):
            edit.editingFinished.connect(lambda edit=edit: self.normalize_extinction(edit))
        self.pressure_edit.editingFinished.connect(self.normalize_pressure)
        self.relative_humidity_edit.editingFinished.connect(self.normalize_relative_humidity)
        self.lower_oxygen_limit_edit.editingFinished.connect(self.normalize_lower_oxygen_limit)
        self.interior_o2_mass_fraction_edit.editingFinished.connect(
            self.normalize_o2_mass_fractions
        )
        self.exterior_o2_mass_fraction_edit.editingFinished.connect(
            self.normalize_o2_mass_fractions
        )

    def normalize_value_edit(
        self,
        edit: QLineEdit,
        kind: str,
        field_name: str,
        allow_default: bool = False,
    ):
        try:
            value = parse_value(
                kind,
                edit.text(),
                field_name,
                allow_default=allow_default,
            )
        except ValueError:
            return

        if value is None:
            edit.setText("Default")
        else:
            edit.setText(format_value(kind, value))

    def set_pressure_value(self, value: float):
        self._pressure_value = value
        label = unit_label(PRESSURE)
        if label in {"psi", "psia", "psig"}:
            number = format_number(round(display_value(PRESSURE, value), 2))
            self._pressure_text = f"{number} {label}"
        else:
            self._pressure_text = format_value(PRESSURE, value)
        self.pressure_edit.setText(self._pressure_text)
        self.pressure_edit.setModified(False)
        self.pressure_edit.setToolTip(format_value(PRESSURE, value))

    def pressure_value(self) -> float:
        # Reading or normalizing an unchanged, rounded display must not change
        # the stored pressure when saving or switching engineering units.
        if (self.pressure_edit.text().strip() == self._pressure_text
                and not self.pressure_edit.isModified()):
            return self._pressure_value
        return parse_value(PRESSURE, self.pressure_edit.text(), "Pressure")

    def normalize_pressure(self):
        try:
            value = self.pressure_value()
        except ValueError:
            return
        self.set_pressure_value(value)

    def set_extinction_value(self, edit: QLineEdit, value: float):
        text = f"{display_value(SPECIFIC_EXTINCTION, value):.5g} {unit_label(SPECIFIC_EXTINCTION)}"
        self._extinction_values[edit] = (value, text)
        edit.setText(text)
        edit.setModified(False)
        edit.setToolTip(format_value(SPECIFIC_EXTINCTION, value))

    def extinction_value(self, edit: QLineEdit, field_name: str) -> float:
        value, text = self._extinction_values[edit]
        # Display rounding must not change the SI value exported to the input file.
        if edit.text().strip() == text and not edit.isModified():
            return value
        return parse_value(SPECIFIC_EXTINCTION, edit.text(), field_name)

    def normalize_extinction(self, edit: QLineEdit):
        try:
            value = self.extinction_value(edit, "Extinction Coefficient")
        except ValueError:
            return
        self.set_extinction_value(edit, value)

    def normalize_relative_humidity(self):
        try:
            value = parse_number(self.relative_humidity_edit.text(), "Humidity")
        except ValueError:
            return

        self.relative_humidity_edit.setText(format_percent(value))

    def normalize_lower_oxygen_limit(self):
        try:
            value = parse_number(self.lower_oxygen_limit_edit.text(), "Lower Oxygen Limit")
        except ValueError:
            return

        self.lower_oxygen_limit_edit.setText(format_mol_fraction(value))

    def normalize_o2_mass_fractions(self):
        for edit, field_name in (
            (self.interior_o2_mass_fraction_edit, "Interior O₂ Mass Fraction"),
            (self.exterior_o2_mass_fraction_edit, "Exterior O₂ Mass Fraction"),
        ):
            try:
                value = parse_value(MASS_FRACTION, edit.text(), field_name)
            except ValueError:
                continue
            edit.setText(format_mass_fraction(value))

    def add_to_case(self, case: CfastCase):
        case.title = self.title_edit.text().strip() or "CFAST Simulation"

        case.simulation_time = parse_value(
            TIME,
            self.simulation_time_edit.text(),
            "Simulation Time",
        )
        case.print_interval = parse_value(
            TIME,
            self.print_interval_edit.text(),
            "Text Output Interval",
        )
        case.spreadsheet_interval = parse_value(
            TIME,
            self.spreadsheet_interval_edit.text(),
            "Spreadsheet Output Interval",
        )
        case.smokeview_interval = parse_value(
            TIME,
            self.smokeview_interval_edit.text(),
            "Smokeview Output Interval",
        )
        case.max_time_step = parse_value(
            TIME,
            self.max_time_step_edit.text(),
            "Maximum Time Step",
            allow_default=True,
        )

        case.interior_temperature = parse_value(
            TEMPERATURE,
            self.interior_temperature_edit.text(),
            "Interior Temperature",
        )
        case.relative_humidity = parse_number(
            self.relative_humidity_edit.text(),
            "Humidity",
        )
        case.exterior_temperature = parse_value(
            TEMPERATURE,
            self.exterior_temperature_edit.text(),
            "Exterior Temperature",
        )
        case.pressure = self.pressure_value()
        case.interior_o2_mass_fraction = self.o2_mass_fraction_value(
            self.interior_o2_mass_fraction_edit,
            "Interior O₂ Mass Fraction",
        )
        case.exterior_o2_mass_fraction = self.o2_mass_fraction_value(
            self.exterior_o2_mass_fraction_edit,
            "Exterior O₂ Mass Fraction",
        )

        case.adiabatic_surfaces = self.adiabatic_checkbox.isChecked()
        case.lower_oxygen_limit = parse_number(
            self.lower_oxygen_limit_edit.text(),
            "Lower Oxygen Limit",
        )

        iterations = parse_number(self.max_iteration_edit.text(), "Maximum Solver Iterations")
        if not math.isfinite(iterations) or not iterations.is_integer() or iterations < -1:
            raise ValueError("Maximum Solver Iterations must be -1 (unlimited) or a non-negative integer.")
        case.max_iteration = int(iterations)
        case.overwrite = self.overwrite_checkbox.isChecked()
        extinction = []
        for edit, name in (
            (self.flaming_extinction_edit, "Flaming Extinction Coefficient"),
            (self.smoldering_extinction_edit, "Smoldering Extinction Coefficient"),
        ):
            value = self.extinction_value(edit, name)
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{name} must be a finite, non-negative number.")
            extinction.append(value)
        case.specific_extinction = tuple(extinction)

    @staticmethod
    def o2_mass_fraction_value(edit: QLineEdit, field_name: str) -> float:
        value = parse_value(MASS_FRACTION, edit.text(), field_name)
        if not 0.0 <= value <= 1.0:
            raise ValueError(f"{field_name} must be between 0 and 1 {unit_label(MASS_FRACTION)}.")
        return value

    def set_message(self, text: str, syntax_highlight: bool = False):
        self.syntax_highlighter.set_enabled(syntax_highlight)
        self.message_panel.setPlainText(text)

    def append_message(self, text: str):
        self.syntax_highlighter.set_enabled(False)
        self.message_panel.moveCursor(QTextCursor.MoveOperation.End)
        self.message_panel.insertPlainText(text)
        self.message_panel.ensureCursorVisible()

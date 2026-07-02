from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtGui import QTextCursor
from PySide6.QtWidgets import (
    QCheckBox,
    QFrame,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase
from units import PRESSURE, TEMPERATURE, TIME, format_number, format_value, parse_number, parse_value


class SimulationTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.title_edit = QLineEdit("CFAST Simulation")

        self.simulation_time_edit = QLineEdit(format_value(TIME, 3600.0))
        self.print_interval_edit = QLineEdit(format_value(TIME, 60.0))
        self.spreadsheet_interval_edit = QLineEdit(format_value(TIME, 60.0))
        self.smokeview_interval_edit = QLineEdit(format_value(TIME, 60.0))
        self.max_time_step_edit = QLineEdit("Default")

        self.interior_temperature_edit = QLineEdit(format_value(TEMPERATURE, 20.0))
        self.relative_humidity_edit = QLineEdit("50 %")
        self.exterior_temperature_edit = QLineEdit(format_value(TEMPERATURE, 20.0))
        self.pressure_edit = QLineEdit(format_value(PRESSURE, 101325.0))

        self.adiabatic_checkbox = QCheckBox("Adiabatic Compartment Surfaces")
        self.lower_oxygen_limit_edit = QLineEdit("0.1")

        self.message_panel = QTextEdit()
        self.message_panel.setReadOnly(True)
        self.message_panel.setPlainText("")
        self.message_panel.setMinimumHeight(140)

        self.build_layout()

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
        self.relative_humidity_edit.setText(format_number(case.relative_humidity))
        self.exterior_temperature_edit.setText(format_value(TEMPERATURE, case.exterior_temperature))
        self.pressure_edit.setText(format_value(PRESSURE, case.pressure))
        self.adiabatic_checkbox.setChecked(case.adiabatic_surfaces)
        self.lower_oxygen_limit_edit.setText(format_number(case.lower_oxygen_limit))

    def build_times_group(self):
        group = QGroupBox("Simulation Times")
        layout = QGridLayout()

        labels_and_widgets = [
            ("Simulation Time:", self.simulation_time_edit),
            ("Text Output Interval:", self.print_interval_edit),
            ("Spreadsheet Output Interval:", self.spreadsheet_interval_edit),
            ("Smokeview Output Interval:", self.smokeview_interval_edit),
            ("Maximum Time Step:", self.max_time_step_edit),
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
        group = QGroupBox("Simulation Conditions")
        outer_layout = QVBoxLayout()

        interior_group = QGroupBox("Interior")
        interior_layout = QGridLayout()
        interior_layout.addWidget(QLabel("Temperature:"), 0, 0)
        interior_layout.addWidget(self.interior_temperature_edit, 0, 1)
        interior_layout.addWidget(QLabel("Humidity:"), 0, 2)
        interior_layout.addWidget(self.relative_humidity_edit, 0, 3)
        interior_group.setLayout(interior_layout)

        exterior_group = QGroupBox("Exterior")
        exterior_layout = QGridLayout()
        exterior_layout.addWidget(QLabel("Temperature:"), 0, 0)
        exterior_layout.addWidget(self.exterior_temperature_edit, 0, 1)
        exterior_layout.addWidget(QLabel("Pressure:"), 0, 2)
        exterior_layout.addWidget(self.pressure_edit, 0, 3)
        exterior_group.setLayout(exterior_layout)

        misc_layout = QGridLayout()
        misc_layout.addWidget(self.adiabatic_checkbox, 0, 0, 1, 2)
        misc_layout.addWidget(QLabel("Lower Oxygen Limit:"), 1, 0)
        misc_layout.addWidget(self.lower_oxygen_limit_edit, 1, 1)

        outer_layout.addWidget(interior_group)
        outer_layout.addWidget(exterior_group)
        outer_layout.addLayout(misc_layout)

        group.setLayout(outer_layout)
        group.setMinimumWidth(380)

        return group

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
        case.pressure = parse_value(
            PRESSURE,
            self.pressure_edit.text(),
            "Pressure",
        )

        case.adiabatic_surfaces = self.adiabatic_checkbox.isChecked()
        case.lower_oxygen_limit = parse_number(
            self.lower_oxygen_limit_edit.text(),
            "Lower Oxygen Limit",
        )

    def set_message(self, text: str):
        self.message_panel.setPlainText(text)

    def append_message(self, text: str):
        self.message_panel.moveCursor(QTextCursor.MoveOperation.End)
        self.message_panel.insertPlainText(text)
        self.message_panel.ensureCursorVisible()

import re

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QComboBox,
    QFrame,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

from cfast_case import CfastCase, FireDefinition, FireProperty, FireRampPoint


_NUMBER_RE = re.compile(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?")


def parse_float(text: str, field_name: str, default: float | None = None) -> float:
    text = text.strip()

    if not text and default is not None:
        return default

    match = _NUMBER_RE.search(text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


def parse_int(text: str, field_name: str, default: int | None = None) -> int:
    if not text.strip() and default is not None:
        return default

    value = parse_float(text, field_name)

    if abs(value - round(value)) > 1.0e-12:
        raise ValueError(f"{field_name} must be an integer: {text!r}")

    return int(round(value))


def set_combo_text(combo: QComboBox, text: str) -> None:
    index = combo.findText(text)

    if index < 0:
        combo.addItem(text)
        index = combo.findText(text)

    combo.setCurrentIndex(index)


def read_table_item(table: QTableWidget, row: int, col: int) -> str:
    item = table.item(row, col)

    if item is None:
        return ""

    return item.text().strip()


def make_read_only_item(text: str) -> QTableWidgetItem:
    item = QTableWidgetItem(text)
    item.setFlags(Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled)
    return item


class FirePlotCanvas(FigureCanvas):
    def __init__(self, parent=None):
        self.figure = Figure(figsize=(6, 3), tight_layout=True)
        self.ax = self.figure.add_subplot(111)
        super().__init__(self.figure)
        self.setParent(parent)

    def plot_fire(self, fire_property: FireProperty | None):
        self.ax.clear()

        if fire_property is None or not fire_property.ramp:
            self.ax.text(
                0.5,
                0.5,
                "Enter HRR table data",
                ha="center",
                va="center",
                transform=self.ax.transAxes,
            )
            self.ax.set_xlim(0.0, 1.0)
            self.ax.set_ylim(0.0, 1.0)
            self.ax.set_title("HRR (kW)")
        else:
            ramp = fire_property.sorted_ramp()
            times = [point.time for point in ramp]
            hrrs = [point.hrr for point in ramp]

            self.ax.plot(times, hrrs)
            self.ax.set_xlim(left=0.0)
            self.ax.set_ylim(bottom=0.0)
            self.ax.set_title(f"{fire_property.id}: HRR (kW)")

        self.ax.set_xlabel("Time (s)")
        self.ax.set_ylabel("HRR (kW)")
        self.ax.grid(True)
        self.draw()


class FiresTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.loading = False
        self.current_index = -1
        self.fires: list[FireDefinition] = []
        self.fire_properties: list[FireProperty] = []

        self.summary_table = QTableWidget(0, 11)
        self.summary_table.setHorizontalHeaderLabels(
            [
                "Num",
                "Compartment",
                "Fire ID",
                "Ignition by",
                "Set Point",
                "Target",
                "X Position",
                "Y Position",
                "Fire Properties ID",
                "Fuel",
                "Peak HRR",
            ]
        )
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.summary_table.itemSelectionChanged.connect(self.selection_changed)

        self.fire_id_edit = QLineEdit()
        self.compartment_combo = QComboBox()
        self.compartment_combo.setEditable(True)
        self.x_position_edit = QLineEdit()
        self.y_position_edit = QLineEdit()
        self.ignition_combo = QComboBox()
        self.ignition_combo.addItems(["TIME", "TEMPERATURE", "FLUX"])
        self.setpoint_edit = QLineEdit()
        self.target_combo = QComboBox()
        self.target_combo.setEditable(True)
        self.fire_property_combo = QComboBox()
        self.fire_property_combo.setEditable(True)

        self.property_id_edit = QLineEdit()
        self.carbon_edit = QLineEdit()
        self.hydrogen_edit = QLineEdit()
        self.oxygen_edit = QLineEdit()
        self.nitrogen_edit = QLineEdit()
        self.chlorine_edit = QLineEdit()
        self.heat_of_combustion_edit = QLineEdit()
        self.radiative_fraction_edit = QLineEdit()

        self.ramp_table = QTableWidget(8, 8)
        self.ramp_table.setHorizontalHeaderLabels(
            [
                "Time\n(s)",
                "HRR\n(kW)",
                "Height\n(m)",
                "Area\n(m2)",
                "CO Yield",
                "Soot Yield",
                "HCN Yield",
                "TS Yield",
            ]
        )
        self.ramp_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.ramp_table.verticalHeader().setVisible(True)
        self.ramp_table.cellChanged.connect(self.editor_changed)

        self.plot_canvas = FirePlotCanvas()

        self.build_layout()
        self.connect_editor_signals()
        self.load_demo_data()
        self.rebuild_summary_table()
        self.select_fire(0)

    def build_layout(self):
        main_layout = QVBoxLayout()

        main_layout.addWidget(self.summary_table, 2)
        main_layout.addLayout(self.build_button_row())
        main_layout.addLayout(self.build_editor_layout(), 5)

        self.setLayout(main_layout)

    def build_button_row(self):
        layout = QHBoxLayout()
        layout.addStretch(1)

        add_new_button = QPushButton("Add New")
        add_t2_button = QPushButton("Add t²")
        from_file_button = QPushButton("From File")
        remove_button = QPushButton("Remove")

        add_new_button.clicked.connect(self.add_new_fire)
        add_t2_button.clicked.connect(self.add_t_squared_fire)
        from_file_button.clicked.connect(self.from_file_placeholder)
        remove_button.clicked.connect(self.remove_fire)

        layout.addWidget(add_new_button)
        layout.addWidget(add_t2_button)
        layout.addWidget(from_file_button)
        layout.addWidget(remove_button)
        layout.addStretch(1)

        return layout

    def build_editor_layout(self):
        layout = QHBoxLayout()
        layout.addWidget(self.build_left_editor(), 2)
        layout.addWidget(self.build_right_editor(), 3)
        return layout

    def build_left_editor(self):
        widget = QWidget()
        layout = QVBoxLayout()

        fire_group = QGroupBox("Fire")
        fire_layout = QGridLayout()
        fire_layout.addWidget(QLabel("Fire ID:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        fire_layout.addWidget(self.fire_id_edit, 0, 1, 1, 3)
        fire_layout.addWidget(QLabel("Compartment:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        fire_layout.addWidget(self.compartment_combo, 1, 1, 1, 3)
        fire_layout.addWidget(QLabel("Position, X:"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        fire_layout.addWidget(self.x_position_edit, 2, 1)
        fire_layout.addWidget(QLabel("Position Y:"), 2, 2, alignment=Qt.AlignmentFlag.AlignRight)
        fire_layout.addWidget(self.y_position_edit, 2, 3)
        fire_layout.addWidget(
            QLabel("Ignition Criterion:"),
            3,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        fire_layout.addWidget(self.ignition_combo, 3, 1)
        fire_layout.addWidget(QLabel("Set Point:"), 4, 0, alignment=Qt.AlignmentFlag.AlignRight)
        fire_layout.addWidget(self.setpoint_edit, 4, 1)
        fire_layout.addWidget(
            QLabel("Ignition Target:"),
            4,
            2,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        fire_layout.addWidget(self.target_combo, 4, 3)
        fire_layout.addWidget(
            QLabel("Referenced Fire\nProperties ID:"),
            5,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        fire_layout.addWidget(self.fire_property_combo, 5, 1, 1, 3)
        fire_group.setLayout(fire_layout)

        property_group = QGroupBox("Fire Properties")
        property_layout = QGridLayout()
        property_layout.addWidget(
            QLabel("Fire Properties ID:"),
            0,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        property_layout.addWidget(self.property_id_edit, 0, 1, 1, 3)
        property_layout.addWidget(QLabel("C:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        property_layout.addWidget(self.carbon_edit, 1, 1)
        property_layout.addWidget(QLabel("N:"), 1, 2, alignment=Qt.AlignmentFlag.AlignRight)
        property_layout.addWidget(self.nitrogen_edit, 1, 3)
        property_layout.addWidget(QLabel("H:"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        property_layout.addWidget(self.hydrogen_edit, 2, 1)
        property_layout.addWidget(QLabel("Cl:"), 2, 2, alignment=Qt.AlignmentFlag.AlignRight)
        property_layout.addWidget(self.chlorine_edit, 2, 3)
        property_layout.addWidget(QLabel("O:"), 3, 0, alignment=Qt.AlignmentFlag.AlignRight)
        property_layout.addWidget(self.oxygen_edit, 3, 1)
        property_layout.addWidget(
            QLabel("Heat of Combustion:"),
            4,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        property_layout.addWidget(self.heat_of_combustion_edit, 4, 1, 1, 3)
        property_layout.addWidget(
            QLabel("Radiative Fraction:"),
            5,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        property_layout.addWidget(self.radiative_fraction_edit, 5, 1, 1, 3)
        property_group.setLayout(property_layout)

        layout.addWidget(fire_group)
        layout.addWidget(property_group)
        layout.addStretch(1)
        widget.setLayout(layout)
        return widget

    def build_right_editor(self):
        widget = QWidget()
        layout = QVBoxLayout()
        layout.addWidget(self.ramp_table, 2)
        layout.addWidget(self.plot_canvas, 3)
        widget.setLayout(layout)
        return widget

    def connect_editor_signals(self):
        line_edits = [
            self.fire_id_edit,
            self.x_position_edit,
            self.y_position_edit,
            self.setpoint_edit,
            self.property_id_edit,
            self.carbon_edit,
            self.hydrogen_edit,
            self.oxygen_edit,
            self.nitrogen_edit,
            self.chlorine_edit,
            self.heat_of_combustion_edit,
            self.radiative_fraction_edit,
        ]

        for line_edit in line_edits:
            line_edit.textChanged.connect(self.editor_changed)

        combos = [
            self.compartment_combo,
            self.ignition_combo,
            self.target_combo,
            self.fire_property_combo,
        ]

        for combo in combos:
            combo.currentTextChanged.connect(self.editor_changed)

        self.fire_property_combo.currentTextChanged.connect(
            self.referenced_property_changed
        )

    def load_demo_data(self):
        self.fire_properties = [
            FireProperty(
                id="Cushion_Fire",
                carbon=9,
                hydrogen=6,
                oxygen=2,
                nitrogen=2,
                chlorine=0,
                heat_of_combustion=50000.0,
                radiative_fraction=0.33,
                ramp=[
                    FireRampPoint(0.0, 0.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(60.0, 100.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(120.0, 150.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(180.0, 200.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(240.0, 150.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(360.0, 100.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(540.0, 75.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                    FireRampPoint(1800.0, 75.0, 0.0, 0.1, 0.0846, 0.2270, 0.3104, 0.0),
                ],
            ),
            FireProperty(
                id="Wood_Wall_Fire",
                carbon=6,
                hydrogen=10,
                oxygen=5,
                nitrogen=0,
                chlorine=0,
                heat_of_combustion=17500.0,
                radiative_fraction=0.35,
                ramp=[
                    FireRampPoint(0.0, 0.0, 0.0, 0.1, 0.0, 0.015, 0.0, 0.0),
                    FireRampPoint(300.0, 200.0, 0.0, 0.1, 0.0, 0.015, 0.0, 0.0),
                    FireRampPoint(900.0, 200.0, 0.0, 0.1, 0.0, 0.015, 0.0, 0.0),
                ],
            ),
        ]

        self.fires = [
            FireDefinition(
                id="Cushion",
                comp_id="Comp 1",
                fire_property_id="Cushion_Fire",
                ignition_criterion="TIME",
                setpoint=0.0,
                x_position=2.5,
                y_position=2.5,
            ),
            FireDefinition(
                id="Wood_Wall",
                comp_id="Comp 2",
                fire_property_id="Wood_Wall_Fire",
                ignition_criterion="TIME",
                setpoint=0.0,
                x_position=2.5,
                y_position=5.0,
            ),
        ]

        self.update_compartment_choices()
        self.update_property_choices()

    def update_compartment_choices(self):
        current = self.compartment_combo.currentText()
        self.compartment_combo.blockSignals(True)
        self.compartment_combo.clear()
        self.compartment_combo.addItems(["Comp 1", "Comp 2", "Comp 3"])
        if current:
            set_combo_text(self.compartment_combo, current)
        self.compartment_combo.blockSignals(False)

    def update_property_choices(self):
        current = self.fire_property_combo.currentText()
        self.fire_property_combo.blockSignals(True)
        self.fire_property_combo.clear()
        for prop in self.fire_properties:
            self.fire_property_combo.addItem(prop.id)
        if current:
            set_combo_text(self.fire_property_combo, current)
        self.fire_property_combo.blockSignals(False)

    def selected_fire(self) -> FireDefinition | None:
        if 0 <= self.current_index < len(self.fires):
            return self.fires[self.current_index]
        return None

    def selected_property(self) -> FireProperty | None:
        fire = self.selected_fire()
        if fire is None:
            return None
        return self.find_property(fire.fire_property_id)

    def find_property(self, prop_id: str) -> FireProperty | None:
        for prop in self.fire_properties:
            if prop.id == prop_id:
                return prop
        return None

    def rebuild_summary_table(self):
        self.summary_table.blockSignals(True)
        self.summary_table.setRowCount(len(self.fires))

        for row, fire in enumerate(self.fires):
            prop = self.find_property(fire.fire_property_id)
            fuel = prop.fuel_formula() if prop is not None else ""
            peak_hrr = prop.peak_hrr() if prop is not None else 0.0
            values = [
                str(row + 1),
                fire.comp_id,
                fire.id,
                fire.ignition_criterion.title(),
                f"{fire.setpoint:g}",
                fire.target,
                f"{fire.x_position:g}",
                f"{fire.y_position:g}",
                fire.fire_property_id,
                fuel,
                f"{peak_hrr:g}",
            ]

            for col, value in enumerate(values):
                self.summary_table.setItem(row, col, make_read_only_item(value))

        self.summary_table.blockSignals(False)

    def select_fire(self, index: int):
        if not self.fires:
            self.current_index = -1
            self.clear_editor()
            return

        index = max(0, min(index, len(self.fires) - 1))
        self.summary_table.blockSignals(True)
        self.summary_table.selectRow(index)
        self.summary_table.blockSignals(False)
        self.current_index = index
        self.load_editor(index)

    def selection_changed(self):
        if self.loading:
            return

        indexes = self.summary_table.selectionModel().selectedRows()

        if not indexes:
            return

        new_index = indexes[0].row()

        if new_index == self.current_index:
            return

        self.save_current_editor()
        self.current_index = new_index
        self.load_editor(new_index)

    def clear_editor(self):
        self.loading = True
        for widget in [
            self.fire_id_edit,
            self.x_position_edit,
            self.y_position_edit,
            self.setpoint_edit,
            self.property_id_edit,
            self.carbon_edit,
            self.hydrogen_edit,
            self.oxygen_edit,
            self.nitrogen_edit,
            self.chlorine_edit,
            self.heat_of_combustion_edit,
            self.radiative_fraction_edit,
        ]:
            widget.clear()
        self.ramp_table.clearContents()
        self.loading = False

    def load_editor(self, index: int):
        if not 0 <= index < len(self.fires):
            self.clear_editor()
            return

        self.loading = True
        fire = self.fires[index]
        prop = self.find_property(fire.fire_property_id)

        self.fire_id_edit.setText(fire.id)
        set_combo_text(self.compartment_combo, fire.comp_id)
        self.x_position_edit.setText(f"{fire.x_position:g}")
        self.y_position_edit.setText(f"{fire.y_position:g}")
        set_combo_text(self.ignition_combo, fire.ignition_criterion.upper())
        self.setpoint_edit.setText(f"{fire.setpoint:g}")
        set_combo_text(self.target_combo, fire.target)
        self.update_property_choices()
        set_combo_text(self.fire_property_combo, fire.fire_property_id)

        if prop is None:
            prop = FireProperty(id=fire.fire_property_id)
            self.fire_properties.append(prop)
            self.update_property_choices()

        self.load_property_editor(prop)
        self.loading = False
        self.update_plot()

    def load_property_editor(self, prop: FireProperty):
        self.property_id_edit.setText(prop.id)
        self.carbon_edit.setText(str(prop.carbon))
        self.hydrogen_edit.setText(str(prop.hydrogen))
        self.oxygen_edit.setText(str(prop.oxygen))
        self.nitrogen_edit.setText(str(prop.nitrogen))
        self.chlorine_edit.setText(str(prop.chlorine))
        self.heat_of_combustion_edit.setText(f"{prop.heat_of_combustion:g} kJ/kg")
        self.radiative_fraction_edit.setText(f"{prop.radiative_fraction:g}")

        self.ramp_table.blockSignals(True)
        self.ramp_table.clearContents()
        self.ramp_table.setRowCount(max(8, len(prop.ramp)))

        for row, point in enumerate(prop.sorted_ramp()):
            values = [
                point.time,
                point.hrr,
                point.height,
                point.area,
                point.co_yield,
                point.soot_yield,
                point.hcn_yield,
                point.trace_yield,
            ]

            for col, value in enumerate(values):
                self.ramp_table.setItem(row, col, QTableWidgetItem(f"{value:g}"))

        self.ramp_table.blockSignals(False)

    def editor_changed(self):
        if self.loading:
            return

        try:
            self.save_current_editor()
            self.rebuild_summary_table()
            self.select_summary_row_without_loading(self.current_index)
            self.update_plot()
        except ValueError:
            pass

    def referenced_property_changed(self):
        if self.loading:
            return

        fire = self.selected_fire()
        if fire is None:
            return

        prop_id = self.fire_property_combo.currentText().strip()
        if not prop_id:
            return

        self.save_current_fire_only()
        prop = self.find_property(prop_id)

        if prop is None:
            prop = FireProperty(id=prop_id)
            self.fire_properties.append(prop)
            self.update_property_choices()

        self.loading = True
        self.load_property_editor(prop)
        self.loading = False
        self.update_plot()
        self.rebuild_summary_table()
        self.select_summary_row_without_loading(self.current_index)

    def select_summary_row_without_loading(self, row: int):
        if not 0 <= row < self.summary_table.rowCount():
            return

        self.summary_table.blockSignals(True)
        self.summary_table.selectRow(row)
        self.summary_table.blockSignals(False)

    def save_current_fire_only(self):
        fire = self.selected_fire()
        if fire is None:
            return

        fire.id = self.fire_id_edit.text().strip() or fire.id
        fire.comp_id = self.compartment_combo.currentText().strip() or "Comp 1"
        fire.fire_property_id = self.fire_property_combo.currentText().strip()
        fire.ignition_criterion = self.ignition_combo.currentText().strip().upper()
        fire.setpoint = parse_float(self.setpoint_edit.text(), "Set Point", 0.0)
        fire.target = self.target_combo.currentText().strip()
        fire.x_position = parse_float(self.x_position_edit.text(), "X Position", 0.0)
        fire.y_position = parse_float(self.y_position_edit.text(), "Y Position", 0.0)

    def save_current_editor(self):
        fire = self.selected_fire()
        if fire is None:
            return

        old_prop_id = fire.fire_property_id
        self.save_current_fire_only()
        new_prop_id = self.property_id_edit.text().strip() or fire.fire_property_id
        old_prop = self.find_property(old_prop_id)
        prop = self.find_property(new_prop_id)

        if prop is None:
            prop = old_prop if old_prop is not None else FireProperty(id=new_prop_id)
            if prop not in self.fire_properties:
                self.fire_properties.append(prop)

        prop.id = new_prop_id
        prop.carbon = parse_int(self.carbon_edit.text(), "C", 0)
        prop.hydrogen = parse_int(self.hydrogen_edit.text(), "H", 0)
        prop.oxygen = parse_int(self.oxygen_edit.text(), "O", 0)
        prop.nitrogen = parse_int(self.nitrogen_edit.text(), "N", 0)
        prop.chlorine = parse_int(self.chlorine_edit.text(), "Cl", 0)
        prop.heat_of_combustion = parse_float(
            self.heat_of_combustion_edit.text(),
            "Heat of Combustion",
            50000.0,
        )
        prop.radiative_fraction = parse_float(
            self.radiative_fraction_edit.text(),
            "Radiative Fraction",
            0.35,
        )
        prop.ramp = self.extract_ramp()

        fire.fire_property_id = prop.id
        self.update_property_choices()
        set_combo_text(self.fire_property_combo, prop.id)

    def extract_ramp(self) -> list[FireRampPoint]:
        points: list[FireRampPoint] = []

        for row in range(self.ramp_table.rowCount()):
            values = [read_table_item(self.ramp_table, row, col) for col in range(8)]

            if not any(values):
                continue

            point = FireRampPoint(
                time=parse_float(values[0], "Time", 0.0),
                hrr=parse_float(values[1], "HRR", 0.0),
                height=parse_float(values[2], "Height", 0.0),
                area=parse_float(values[3], "Area", 0.1),
                co_yield=parse_float(values[4], "CO Yield", 0.0),
                soot_yield=parse_float(values[5], "Soot Yield", 0.0),
                hcn_yield=parse_float(values[6], "HCN Yield", 0.0),
                trace_yield=parse_float(values[7], "TS Yield", 0.0),
            )
            points.append(point)

        points.sort(key=lambda point: point.time)
        return points

    def update_plot(self):
        self.plot_canvas.plot_fire(self.selected_property())

    def add_new_fire(self):
        self.save_current_editor()
        number = len(self.fires) + 1
        fire_id = f"Fire_{number}"
        prop_id = f"{fire_id}_Fire"
        prop = FireProperty(
            id=prop_id,
            carbon=1,
            hydrogen=4,
            heat_of_combustion=50000.0,
            radiative_fraction=0.35,
            ramp=[
                FireRampPoint(0.0, 0.0, 0.0, 0.1, 0.0, 0.01, 0.0, 0.0),
                FireRampPoint(300.0, 1000.0, 0.0, 0.1, 0.0, 0.01, 0.0, 0.0),
                FireRampPoint(600.0, 0.0, 0.0, 0.1, 0.0, 0.01, 0.0, 0.0),
            ],
        )
        fire = FireDefinition(
            id=fire_id,
            comp_id="Comp 1",
            fire_property_id=prop_id,
            ignition_criterion="TIME",
            setpoint=0.0,
            x_position=2.5,
            y_position=2.5,
        )
        self.fire_properties.append(prop)
        self.fires.append(fire)
        self.rebuild_summary_table()
        self.select_fire(len(self.fires) - 1)

    def add_t_squared_fire(self):
        self.save_current_editor()
        number = len(self.fires) + 1
        fire_id = f"T2_Fire_{number}"
        prop_id = f"{fire_id}_Fire"
        t_peak = 300.0
        peak = 1000.0
        ramp = []

        for time in [0.0, 60.0, 120.0, 180.0, 240.0, 300.0, 600.0]:
            hrr = peak * min((time / t_peak) ** 2, 1.0)
            ramp.append(FireRampPoint(time, hrr, 0.0, 0.1, 0.0, 0.01, 0.0, 0.0))

        prop = FireProperty(
            id=prop_id,
            carbon=1,
            hydrogen=4,
            heat_of_combustion=50000.0,
            radiative_fraction=0.35,
            ramp=ramp,
        )
        fire = FireDefinition(
            id=fire_id,
            comp_id="Comp 1",
            fire_property_id=prop_id,
            ignition_criterion="TIME",
            setpoint=0.0,
            x_position=2.5,
            y_position=2.5,
        )
        self.fire_properties.append(prop)
        self.fires.append(fire)
        self.rebuild_summary_table()
        self.select_fire(len(self.fires) - 1)

    def remove_fire(self):
        if not 0 <= self.current_index < len(self.fires):
            return

        del self.fires[self.current_index]
        self.rebuild_summary_table()
        self.select_fire(min(self.current_index, len(self.fires) - 1))

    def from_file_placeholder(self):
        QMessageBox.information(
            self,
            "From File",
            "Loading a fire definition from file is not implemented yet.",
        )

    def add_to_case(self, case: CfastCase):
        self.save_current_editor()
        case.fires = list(self.fires)
        case.fire_properties = list(self.fire_properties)

        if case.fires:
            first_fire = case.fires[0]
            first_property = self.find_property(first_fire.fire_property_id)
            case.comp_id = first_fire.comp_id
            case.fire_id = first_fire.id
            case.fire_chem_id = first_fire.fire_property_id
            case.fire_location_x = first_fire.x_position
            case.fire_location_y = first_fire.y_position

            if first_property is not None:
                case.carbon = first_property.carbon
                case.hydrogen = first_property.hydrogen
                case.oxygen = first_property.oxygen
                case.nitrogen = first_property.nitrogen
                case.chlorine = first_property.chlorine
                case.heat_of_combustion = first_property.heat_of_combustion
                case.radiative_fraction = first_property.radiative_fraction
                case.fire_ramp = list(first_property.ramp)

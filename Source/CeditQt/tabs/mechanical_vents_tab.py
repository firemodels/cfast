import copy

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QComboBox,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, MechanicalVent
from units import (
    AREA,
    FLOWRATE,
    LENGTH,
    PRESSURE,
    TIME,
    format_number,
    format_value,
    parse_number,
    parse_value,
    unit_label,
)


def summary_headers() -> list[str]:
    return [
        "Num",
        "ID",
        "From Compartment",
        f"From Area\n({unit_label(AREA)})",
        f"From Height\n({unit_label(LENGTH)})",
        "From Type",
        "To Compartment",
        f"To Area\n({unit_label(AREA)})",
        f"To Height\n({unit_label(LENGTH)})",
        "To Type",
        f"Flow\n({unit_label(FLOWRATE)})",
    ]


def table_item(text: str) -> QTableWidgetItem:
    item = QTableWidgetItem(text)
    item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
    return item


class MechanicalVentsTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.vents: list[MechanicalVent] = []
        self.current_index = -1
        self.updating = False

        self.summary_table = QTableWidget(0, 11)
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.summary_table.itemSelectionChanged.connect(self.summary_selection_changed)

        self.id_edit = QLineEdit()

        self.from_compartment_edit = QLineEdit()
        self.from_area_edit = QLineEdit()
        self.from_height_edit = QLineEdit()
        self.from_orientation_combo = QComboBox()
        self.from_orientation_combo.addItems(["Vertical", "Horizontal"])

        self.to_compartment_edit = QLineEdit()
        self.to_area_edit = QLineEdit()
        self.to_height_edit = QLineEdit()
        self.to_orientation_combo = QComboBox()
        self.to_orientation_combo.addItems(["Vertical", "Horizontal"])

        self.flow_edit = QLineEdit()
        self.begin_dropoff_edit = QLineEdit()
        self.zero_flow_edit = QLineEdit()
        self.offset_x_edit = QLineEdit()
        self.offset_y_edit = QLineEdit()
        self.filter_efficiency_edit = QLineEdit()
        self.filter_time_edit = QLineEdit()

        self.criterion_combo = QComboBox()
        self.criterion_combo.addItems(["Time", "Temperature", "Flux"])

        self.schedule_table = QTableWidget(8, 2)
        self.schedule_table.setHorizontalHeaderLabels(
            [f"Time\n({unit_label(TIME)})", "Fraction"]
        )
        self.schedule_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.schedule_table.verticalHeader().setVisible(True)

        self.build_layout()
        self.connect_editor_signals()
        self.load_demo_data()

    def load_case(self, case: CfastCase):
        self.refresh_unit_labels()
        self.vents = copy.deepcopy(case.mechanical_vents)
        self.refresh_summary_table()

        if self.vents:
            self.select_row(0)
        else:
            self.current_index = -1
            self.clear_editor()

    def build_layout(self):
        layout = QVBoxLayout()
        layout.addWidget(self.summary_table, 2)
        layout.addLayout(self.build_button_row())
        layout.addWidget(self.build_editor_group(), 3)
        self.setLayout(layout)

    def build_button_row(self):
        row = QHBoxLayout()
        row.addStretch(1)

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_vent)
        duplicate_button.clicked.connect(self.duplicate_vent)
        remove_button.clicked.connect(self.remove_vent)

        row.addWidget(add_button)
        row.addSpacing(20)
        row.addWidget(duplicate_button)
        row.addStretch(2)
        row.addWidget(remove_button)
        row.addStretch(1)

        return row

    def build_editor_group(self):
        self.editor_group = QGroupBox("Vent 0 (of 0) Geometry")
        outer = QVBoxLayout()

        id_row = QHBoxLayout()
        id_row.addStretch(1)
        id_row.addWidget(QLabel("ID:"))
        id_row.addWidget(self.id_edit, 2)
        id_row.addStretch(1)
        outer.addLayout(id_row)

        middle = QHBoxLayout()
        middle.addWidget(self.build_left_geometry_group(), 2)
        middle.addSpacing(40)
        middle.addWidget(self.build_schedule_group(), 1)
        outer.addLayout(middle)

        bottom = QGridLayout()
        bottom.addWidget(QLabel("Flow Rate:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.flow_edit, 0, 1)
        bottom.addWidget(QLabel("Vent Offset X:"), 0, 2, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.offset_x_edit, 0, 3)
        bottom.addWidget(QLabel("Filter Efficiency:"), 0, 4, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.filter_efficiency_edit, 0, 5)

        bottom.addWidget(QLabel("Begin Dropoff At:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.begin_dropoff_edit, 1, 1)
        bottom.addWidget(QLabel("Y:"), 1, 2, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.offset_y_edit, 1, 3)
        bottom.addWidget(QLabel("Begin Filter At:"), 1, 4, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.filter_time_edit, 1, 5)

        bottom.addWidget(QLabel("Zero Flow At:"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        bottom.addWidget(self.zero_flow_edit, 2, 1)
        bottom.setColumnStretch(6, 1)
        outer.addLayout(bottom)

        self.editor_group.setLayout(outer)
        return self.editor_group

    def build_left_geometry_group(self):
        group = QWidget()
        layout = QVBoxLayout()
        layout.addWidget(self.build_compartment_group(
            "From Compartment",
            self.from_compartment_edit,
            self.from_area_edit,
            self.from_height_edit,
            self.from_orientation_combo,
        ))
        layout.addWidget(self.build_compartment_group(
            "To Compartment",
            self.to_compartment_edit,
            self.to_area_edit,
            self.to_height_edit,
            self.to_orientation_combo,
        ))
        group.setLayout(layout)
        return group

    def build_compartment_group(self, title, compartment, area, height, orientation):
        group = QGroupBox(title)
        layout = QGridLayout()

        layout.addWidget(compartment, 0, 0, 1, 4)
        layout.addWidget(QLabel("Area:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(area, 1, 1)
        layout.addWidget(QLabel("Center Height:"), 1, 2, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(height, 1, 3)
        layout.addWidget(QLabel("Orientation:"), 2, 2, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(orientation, 2, 3)

        group.setLayout(layout)
        return group

    def build_schedule_group(self):
        group = QGroupBox("Opening Fraction")
        layout = QGridLayout()

        layout.addWidget(QLabel("Open/Close Criterion:"), 0, 0)
        layout.addWidget(self.criterion_combo, 0, 1)
        layout.addWidget(self.schedule_table, 1, 0, 1, 2)

        group.setLayout(layout)
        return group

    def connect_editor_signals(self):
        line_edits = [
            self.id_edit,
            self.from_compartment_edit,
            self.from_area_edit,
            self.from_height_edit,
            self.to_compartment_edit,
            self.to_area_edit,
            self.to_height_edit,
            self.flow_edit,
            self.begin_dropoff_edit,
            self.zero_flow_edit,
            self.offset_x_edit,
            self.offset_y_edit,
            self.filter_efficiency_edit,
            self.filter_time_edit,
        ]

        for edit in line_edits:
            edit.editingFinished.connect(self.store_current_vent)

        self.from_orientation_combo.currentIndexChanged.connect(self.store_current_vent)
        self.to_orientation_combo.currentIndexChanged.connect(self.store_current_vent)
        self.criterion_combo.currentIndexChanged.connect(self.store_current_vent)
        self.schedule_table.cellChanged.connect(self.store_current_vent)

    def load_demo_data(self):
        self.vents = [
            MechanicalVent(
                id="MechanicalVent_1",
                from_comp_id="OUTSIDE",
                to_comp_id="Comp 1",
                from_area=0.25,
                from_height=2.75,
                to_area=0.25,
                to_height=2.75,
                flow=0.02,
                begin_dropoff=200.0,
                zero_flow=300.0,
                offset_x=0.0,
                offset_y=4.0,
                filter_efficiency=0.0,
                filter_time=0.0,
                t_values=[0.0, 100.0, 500.0],
                f_values=[0.0, 0.5, 1.0],
            )
        ]
        self.refresh_summary_table()
        self.select_row(0)

    def refresh_summary_table(self):
        self.updating = True
        self.summary_table.setRowCount(len(self.vents))

        for row, vent in enumerate(self.vents):
            values = [
                str(row + 1),
                vent.id,
                vent.from_comp_id,
                format_value(AREA, vent.from_area),
                format_value(LENGTH, vent.from_height),
                self.display_orientation(vent.from_orientation),
                vent.to_comp_id,
                format_value(AREA, vent.to_area),
                format_value(LENGTH, vent.to_height),
                self.display_orientation(vent.to_orientation),
                format_value(FLOWRATE, vent.flow),
            ]

            for col, value in enumerate(values):
                self.summary_table.setItem(row, col, table_item(value))

        self.updating = False

    def summary_selection_changed(self):
        if self.updating:
            return

        selected = self.summary_table.selectionModel().selectedRows()

        if not selected:
            return

        self.load_vent_into_editor(selected[0].row())

    def select_row(self, row: int):
        if not self.vents:
            self.current_index = -1
            return

        row = max(0, min(row, len(self.vents) - 1))
        self.summary_table.selectRow(row)
        self.load_vent_into_editor(row)

    def load_vent_into_editor(self, index: int):
        if index < 0 or index >= len(self.vents):
            return

        self.updating = True
        self.current_index = index
        vent = self.vents[index]

        self.editor_group.setTitle(f"Vent {index + 1} (of {len(self.vents)}) Geometry")
        self.id_edit.setText(vent.id)
        self.from_compartment_edit.setText(vent.from_comp_id)
        self.from_area_edit.setText(format_value(AREA, vent.from_area))
        self.from_height_edit.setText(format_value(LENGTH, vent.from_height))
        self.set_combo_text(self.from_orientation_combo, self.display_orientation(vent.from_orientation))

        self.to_compartment_edit.setText(vent.to_comp_id)
        self.to_area_edit.setText(format_value(AREA, vent.to_area))
        self.to_height_edit.setText(format_value(LENGTH, vent.to_height))
        self.set_combo_text(self.to_orientation_combo, self.display_orientation(vent.to_orientation))

        self.flow_edit.setText(format_value(FLOWRATE, vent.flow))
        self.begin_dropoff_edit.setText(format_value(PRESSURE, vent.begin_dropoff))
        self.zero_flow_edit.setText(format_value(PRESSURE, vent.zero_flow))
        self.offset_x_edit.setText(format_value(LENGTH, vent.offset_x))
        self.offset_y_edit.setText(format_value(LENGTH, vent.offset_y))
        self.filter_efficiency_edit.setText(f"{format_number(vent.filter_efficiency)} %")
        self.filter_time_edit.setText(format_value(TIME, vent.filter_time))
        self.set_combo_text(self.criterion_combo, self.display_criterion(vent.criterion))

        self.schedule_table.blockSignals(True)
        self.schedule_table.clearContents()
        row_count = max(8, len(vent.t_values), len(vent.f_values))
        self.schedule_table.setRowCount(row_count)

        for row, time_value in enumerate(vent.t_values):
            self.schedule_table.setItem(row, 0, table_item(format_value(TIME, time_value)))

        for row, fraction_value in enumerate(vent.f_values):
            self.schedule_table.setItem(row, 1, table_item(format_number(fraction_value)))

        self.schedule_table.blockSignals(False)
        self.updating = False

    def clear_editor(self):
        self.updating = True
        for widget in [
            self.id_edit,
            self.from_compartment_edit,
            self.from_area_edit,
            self.from_height_edit,
            self.to_compartment_edit,
            self.to_area_edit,
            self.to_height_edit,
            self.flow_edit,
            self.begin_dropoff_edit,
            self.zero_flow_edit,
            self.offset_x_edit,
            self.offset_y_edit,
            self.filter_efficiency_edit,
            self.filter_time_edit,
        ]:
            widget.clear()
        self.schedule_table.clearContents()
        self.editor_group.setTitle("Vent 0 (of 0) Geometry")
        self.updating = False

    def store_current_vent(self):
        if self.updating:
            return

        if self.current_index < 0 or self.current_index >= len(self.vents):
            return

        vent = self.vents[self.current_index]
        vent.id = self.id_edit.text().strip() or vent.id
        vent.from_comp_id = self.from_compartment_edit.text().strip() or "OUTSIDE"
        vent.to_comp_id = self.to_compartment_edit.text().strip() or "OUTSIDE"

        try:
            vent.from_area = parse_value(AREA, self.from_area_edit.text(), "From Area")
            vent.from_height = parse_value(LENGTH, self.from_height_edit.text(), "From Height")
            vent.to_area = parse_value(AREA, self.to_area_edit.text(), "To Area")
            vent.to_height = parse_value(LENGTH, self.to_height_edit.text(), "To Height")
            vent.flow = parse_value(FLOWRATE, self.flow_edit.text(), "Flow Rate")
            vent.begin_dropoff = parse_value(PRESSURE, self.begin_dropoff_edit.text(), "Begin Dropoff")
            vent.zero_flow = parse_value(PRESSURE, self.zero_flow_edit.text(), "Zero Flow")
            vent.offset_x = parse_value(LENGTH, self.offset_x_edit.text(), "Vent Offset X")
            vent.offset_y = parse_value(LENGTH, self.offset_y_edit.text(), "Vent Offset Y")
            vent.filter_efficiency = parse_number(
                self.filter_efficiency_edit.text(),
                "Filter Efficiency",
            )
            vent.filter_time = parse_value(TIME, self.filter_time_edit.text(), "Begin Filter")
        except ValueError:
            return

        vent.from_orientation = self.internal_orientation(self.from_orientation_combo.currentText())
        vent.to_orientation = self.internal_orientation(self.to_orientation_combo.currentText())
        vent.criterion = self.internal_criterion(self.criterion_combo.currentText())
        vent.t_values, vent.f_values = self.extract_schedule()

        self.refresh_summary_table()
        self.summary_table.selectRow(self.current_index)

    def extract_schedule(self) -> tuple[list[float], list[float]]:
        t_values: list[float] = []
        f_values: list[float] = []

        for row in range(self.schedule_table.rowCount()):
            time_item = self.schedule_table.item(row, 0)
            fraction_item = self.schedule_table.item(row, 1)

            time_text = "" if time_item is None else time_item.text().strip()
            fraction_text = "" if fraction_item is None else fraction_item.text().strip()

            if not time_text and not fraction_text:
                continue

            t_values.append(parse_value(TIME, time_text, "Schedule Time"))
            f_values.append(parse_number(fraction_text, "Schedule Fraction"))

        return t_values, f_values

    def add_vent(self):
        new_index = len(self.vents) + 1
        self.store_current_vent()
        self.vents.append(
            MechanicalVent(
                id=f"MechanicalVent_{new_index}",
                from_comp_id="OUTSIDE",
                to_comp_id="Comp 1",
            )
        )
        self.refresh_summary_table()
        self.select_row(len(self.vents) - 1)

    def duplicate_vent(self):
        if self.current_index < 0 or self.current_index >= len(self.vents):
            return

        self.store_current_vent()
        source = self.vents[self.current_index]
        new_vent = MechanicalVent(
            id=f"{source.id}_Copy",
            from_comp_id=source.from_comp_id,
            to_comp_id=source.to_comp_id,
            from_area=source.from_area,
            from_height=source.from_height,
            from_orientation=source.from_orientation,
            to_area=source.to_area,
            to_height=source.to_height,
            to_orientation=source.to_orientation,
            flow=source.flow,
            begin_dropoff=source.begin_dropoff,
            zero_flow=source.zero_flow,
            offset_x=source.offset_x,
            offset_y=source.offset_y,
            filter_efficiency=source.filter_efficiency,
            filter_time=source.filter_time,
            criterion=source.criterion,
            t_values=list(source.t_values),
            f_values=list(source.f_values),
            fyi=source.fyi,
        )
        self.vents.insert(self.current_index + 1, new_vent)
        self.refresh_summary_table()
        self.select_row(self.current_index + 1)

    def remove_vent(self):
        if self.current_index < 0 or self.current_index >= len(self.vents):
            return

        row = self.current_index
        del self.vents[row]
        self.refresh_summary_table()

        if self.vents:
            self.select_row(min(row, len(self.vents) - 1))
        else:
            self.current_index = -1
            self.editor_group.setTitle("Vent 0 (of 0) Geometry")

    def add_to_case(self, case: CfastCase):
        self.store_current_vent()
        ids_seen: set[str] = set()

        for index, vent in enumerate(self.vents):
            if not vent.id:
                raise ValueError(f"Mechanical Vent row {index + 1}: ID is required.")

            if vent.id in ids_seen:
                raise ValueError(
                    f"Mechanical Vent row {index + 1}: duplicate ID {vent.id!r}."
                )

            ids_seen.add(vent.id)

            if vent.from_area < 0.0 or vent.to_area < 0.0:
                raise ValueError(
                    f"Mechanical Vent {vent.id!r}: diffuser areas must be non-negative."
                )

            if vent.flow < 0.0:
                raise ValueError(
                    f"Mechanical Vent {vent.id!r}: flow rate must be non-negative."
                )

            if len(vent.t_values) != len(vent.f_values):
                raise ValueError(
                    f"Mechanical Vent {vent.id!r}: schedule T/F lengths differ."
                )

        case.mechanical_vents = list(self.vents)

    @staticmethod
    def set_combo_text(combo: QComboBox, text: str):
        index = combo.findText(text)
        if index >= 0:
            combo.setCurrentIndex(index)

    @staticmethod
    def display_orientation(value: str) -> str:
        return value.strip().capitalize()

    @staticmethod
    def internal_orientation(value: str) -> str:
        return value.strip().upper()

    @staticmethod
    def display_criterion(value: str) -> str:
        return value.strip().capitalize()

    @staticmethod
    def internal_criterion(value: str) -> str:
        return value.strip().upper()

    def refresh_unit_labels(self):
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.schedule_table.setHorizontalHeaderLabels(
            [f"Time\n({unit_label(TIME)})", "Fraction"]
        )

from __future__ import annotations

import copy

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QAbstractItemView,
    QButtonGroup,
    QComboBox,
    QFormLayout,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QPushButton,
    QRadioButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, Compartment
from units import AREA, LENGTH, format_number, format_value, parse_number, parse_value, unit_label


def summary_headers() -> list[str]:
    length = unit_label(LENGTH)
    return [
        "ID",
        "Num",
        f"Width\n({length})",
        f"Depth\n({length})",
        f"Height\n({length})",
        f"X Position\n({length})",
        f"Y Position\n({length})",
        f"Z Position\n({length})",
        "Ceiling",
        "Walls",
        "Floor",
        "F",
        "H",
        "V",
        "M",
        "D",
        "T",
    ]


def material_headers() -> list[str]:
    length = unit_label(LENGTH)
    return [
        "Ceiling Material",
        f"Ceiling Thickness\n({length})",
        "Wall Material",
        f"Wall Thickness\n({length})",
        "Floor Material",
        f"Floor Thickness\n({length})",
    ]


def area_headers() -> list[str]:
    return [f"Height\n({unit_label(LENGTH)})", f"Area\n({unit_label(AREA)})"]


def parse_int(text: str, field_name: str) -> int:
    value = parse_number(text, field_name)

    if abs(value - round(value)) > 1.0e-12:
        raise ValueError(f"{field_name} must be an integer: {text!r}")

    return int(round(value))


def first_or_off(values: tuple[str, str, str]) -> str:
    return values[0] if values and values[0] else "OFF"


class CompartmentsTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.loading = False
        self.selected_index = 0
        self.compartments = self.default_compartments()
        self.material_choices = ["OFF"]

        self.summary_table = QTableWidget(0, len(summary_headers()))
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows
        )
        self.summary_table.setSelectionMode(
            QAbstractItemView.SelectionMode.SingleSelection
        )
        self.summary_table.setMinimumHeight(170)
        self.summary_table.setMaximumHeight(230)

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        move_up_button = QPushButton("Move Up")
        move_down_button = QPushButton("Move Down")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_compartment)
        duplicate_button.clicked.connect(self.duplicate_compartment)
        move_up_button.clicked.connect(self.move_compartment_up)
        move_down_button.clicked.connect(self.move_compartment_down)
        remove_button.clicked.connect(self.remove_compartment)

        self.detail_group = QGroupBox("Compartment 1 (of 1)")
        self.id_edit = QLineEdit()

        self.width_edit = QLineEdit()
        self.depth_edit = QLineEdit()
        self.height_edit = QLineEdit()
        self.x_edit = QLineEdit()
        self.y_edit = QLineEdit()
        self.z_edit = QLineEdit()

        self.normal_radio = QRadioButton("Normal (Two-zone model)")
        self.shaft_radio = QRadioButton("Shaft (Single-zone model)")
        self.hall_radio = QRadioButton("Corridor (Revised ceiling jet)")
        self.flow_group = QButtonGroup(self)
        self.flow_group.addButton(self.normal_radio)
        self.flow_group.addButton(self.shaft_radio)
        self.flow_group.addButton(self.hall_radio)

        self.flow_coefficient_edit = QLineEdit()
        self.wall_leak_area_ratio_edit = QLineEdit()
        self.floor_leak_area_ratio_edit = QLineEdit()
        self.wall_leak_area_edit = QLineEdit()
        self.floor_leak_area_edit = QLineEdit()

        self.area_table = QTableWidget(6, 2)
        self.area_table.setHorizontalHeaderLabels(area_headers())
        self.area_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.area_table.verticalHeader().setVisible(False)

        self.materials_table = QTableWidget(3, 6)
        self.materials_table.setHorizontalHeaderLabels(material_headers())
        self.materials_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.materials_table.verticalHeader().setVisible(True)
        self.materials_table.setVerticalHeaderLabels(["1", "2", "3"])
        self.materials_table.setMinimumHeight(120)
        self.materials_table.setMaximumHeight(150)

        self.fyi_edit = QLineEdit()

        button_row = QHBoxLayout()
        button_row.addStretch(1)
        button_row.addWidget(add_button)
        button_row.addWidget(duplicate_button)
        button_row.addWidget(move_up_button)
        button_row.addWidget(move_down_button)
        button_row.addStretch(1)
        button_row.addWidget(remove_button)
        button_row.addStretch(1)

        main_layout = QVBoxLayout()
        main_layout.addWidget(self.summary_table)
        main_layout.addLayout(button_row)
        main_layout.addWidget(self.detail_group, 1)
        self.setLayout(main_layout)

        self.build_detail_layout()
        self.connect_detail_signals()

        self.summary_table.itemChanged.connect(self.summary_item_changed)
        self.summary_table.selectionModel().currentRowChanged.connect(
            self.summary_row_changed
        )

        self.refresh_summary_table(select_row=0)
        self.load_detail_from_selected()

    def load_case(self, case: CfastCase):
        self.refresh_unit_labels()
        self.compartments = copy.deepcopy(case.compartments)
        self.selected_index = 0 if self.compartments else -1
        self.refresh_summary_table(select_row=0 if self.compartments else None)
        if self.compartments:
            self.load_detail_from_selected()
        else:
            self.clear_detail()

    def refresh_unit_labels(self):
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.materials_table.setHorizontalHeaderLabels(material_headers())
        self.area_table.setHorizontalHeaderLabels(area_headers())

    def set_material_ids(self, material_ids: list[str]):
        choices = ["OFF"]
        for material_id in material_ids:
            if material_id and material_id not in choices:
                choices.append(material_id)
        self.material_choices = choices
        self.refresh_material_combo_choices()

    def default_compartments(self) -> list[Compartment]:
        concrete_surface = {
            "ceiling_matl_id": ("CONCRETE", "OFF", "OFF"),
            "ceiling_thickness": (0.15, 0.0, 0.0),
            "wall_matl_id": ("CONCRETE", "OFF", "OFF"),
            "wall_thickness": (0.15, 0.0, 0.0),
            "floor_matl_id": ("CONCRETE", "OFF", "OFF"),
            "floor_thickness": (0.15, 0.0, 0.0),
        }

        return [
            Compartment(
                id="Comp 1",
                width=5.0,
                depth=5.0,
                height=3.0,
                origin_x=0.0,
                origin_y=0.0,
                origin_z=0.0,
                **concrete_surface,
            ),
            Compartment(
                id="Comp 2",
                width=5.0,
                depth=5.0,
                height=3.0,
                origin_x=5.0,
                origin_y=0.0,
                origin_z=0.0,
                **concrete_surface,
            ),
            Compartment(
                id="Comp 3",
                width=5.0,
                depth=5.0,
                height=3.0,
                origin_x=5.0,
                origin_y=0.0,
                origin_z=3.0,
                **concrete_surface,
            ),
        ]

    def default_compartment(self) -> Compartment:
        return self.default_compartments()[0]

    def build_detail_layout(self):
        group_layout = QVBoxLayout()

        id_row = QHBoxLayout()
        id_row.addStretch(1)
        id_row.addWidget(QLabel("ID:"))
        id_row.addWidget(self.id_edit, 2)
        id_row.addStretch(1)
        group_layout.addLayout(id_row)

        middle_row = QHBoxLayout()
        middle_row.addWidget(self.build_geometry_group(), 1)
        middle_row.addWidget(self.build_advanced_group(), 1)
        group_layout.addLayout(middle_row)

        materials_group = QGroupBox("Materials")
        materials_layout = QVBoxLayout()
        materials_layout.addWidget(self.materials_table)
        materials_group.setLayout(materials_layout)
        group_layout.addWidget(materials_group)

        fyi_layout = QHBoxLayout()
        fyi_layout.addWidget(QLabel("FYI:"))
        fyi_layout.addWidget(self.fyi_edit)
        group_layout.addLayout(fyi_layout)

        self.detail_group.setLayout(group_layout)

    def build_geometry_group(self) -> QGroupBox:
        group = QGroupBox("Geometry")
        layout = QGridLayout()

        layout.addWidget(QLabel("Width (X):"), 0, 0)
        layout.addWidget(self.width_edit, 0, 1)
        layout.addWidget(QLabel("Position, X:"), 0, 2)
        layout.addWidget(self.x_edit, 0, 3)

        layout.addWidget(QLabel("Depth (Y):"), 1, 0)
        layout.addWidget(self.depth_edit, 1, 1)
        layout.addWidget(QLabel("Y:"), 1, 2)
        layout.addWidget(self.y_edit, 1, 3)

        layout.addWidget(QLabel("Height (Z):"), 2, 0)
        layout.addWidget(self.height_edit, 2, 1)
        layout.addWidget(QLabel("Z:"), 2, 2)
        layout.addWidget(self.z_edit, 2, 3)

        group.setLayout(layout)
        return group

    def build_advanced_group(self) -> QGroupBox:
        group = QGroupBox("Advanced")
        layout = QHBoxLayout()

        flow_box = QGroupBox("Flow Characteristics")
        flow_layout = QVBoxLayout()
        flow_layout.addWidget(self.normal_radio)
        flow_layout.addWidget(self.shaft_radio)
        flow_layout.addWidget(self.hall_radio)

        form_layout = QFormLayout()
        form_layout.addRow("Flow Coefficient:", self.flow_coefficient_edit)
        form_layout.addRow("Wall Leak Area Ratio:", self.wall_leak_area_ratio_edit)
        form_layout.addRow("Floor Leak Area Ratio:", self.floor_leak_area_ratio_edit)
        form_layout.addRow("Wall Leak Area:", self.wall_leak_area_edit)
        form_layout.addRow("Floor Leak Area:", self.floor_leak_area_edit)
        flow_layout.addLayout(form_layout)
        flow_box.setLayout(flow_layout)

        area_box = QGroupBox("Variable Cross-sectional Area")
        area_layout = QVBoxLayout()
        area_layout.addWidget(self.area_table)
        area_box.setLayout(area_layout)

        layout.addWidget(flow_box, 1)
        layout.addWidget(area_box, 1)
        group.setLayout(layout)
        return group

    def connect_detail_signals(self):
        edits = [
            self.id_edit,
            self.width_edit,
            self.depth_edit,
            self.height_edit,
            self.x_edit,
            self.y_edit,
            self.z_edit,
            self.flow_coefficient_edit,
            self.wall_leak_area_ratio_edit,
            self.floor_leak_area_ratio_edit,
            self.wall_leak_area_edit,
            self.floor_leak_area_edit,
            self.fyi_edit,
        ]

        for edit in edits:
            edit.editingFinished.connect(self.save_detail_to_selected)

        self.flow_group.buttonClicked.connect(self.save_detail_to_selected)
        self.area_table.cellChanged.connect(self.save_detail_to_selected)
        self.materials_table.cellChanged.connect(self.save_detail_to_selected)

    def add_compartment(self):
        self.save_detail_to_selected()
        new_index = len(self.compartments) + 1
        if self.compartments:
            compartment = copy.deepcopy(self.compartments[self.selected_index])
        else:
            compartment = self.default_compartment()
        compartment.id = f"Comp {new_index}"
        if len(self.compartments) > 0:
            compartment.origin_x += compartment.width
        self.compartments.append(compartment)
        self.refresh_summary_table(select_row=len(self.compartments) - 1)
        self.load_detail_from_selected()

    def duplicate_compartment(self):
        self.save_detail_to_selected()
        compartment = copy.deepcopy(self.compartments[self.selected_index])
        compartment.id = f"{compartment.id} Copy"
        compartment.origin_x += compartment.width
        insert_row = self.selected_index + 1
        self.compartments.insert(insert_row, compartment)
        self.refresh_summary_table(select_row=insert_row)
        self.load_detail_from_selected()

    def move_compartment_up(self):
        self.save_detail_to_selected()

        if self.selected_index <= 0:
            return

        i = self.selected_index
        self.compartments[i - 1], self.compartments[i] = (
            self.compartments[i],
            self.compartments[i - 1],
        )
        self.refresh_summary_table(select_row=i - 1)
        self.load_detail_from_selected()

    def move_compartment_down(self):
        self.save_detail_to_selected()

        if self.selected_index >= len(self.compartments) - 1:
            return

        i = self.selected_index
        self.compartments[i + 1], self.compartments[i] = (
            self.compartments[i],
            self.compartments[i + 1],
        )
        self.refresh_summary_table(select_row=i + 1)
        self.load_detail_from_selected()

    def remove_compartment(self):
        self.save_detail_to_selected()

        if not self.compartments:
            return

        if len(self.compartments) == 1:
            self.compartments.clear()
            self.selected_index = -1
            self.refresh_summary_table()
            self.clear_detail()
            return

        del self.compartments[self.selected_index]
        new_row = min(self.selected_index, len(self.compartments) - 1)
        self.refresh_summary_table(select_row=new_row)
        self.load_detail_from_selected()

    def refresh_summary_table(self, select_row: int | None = None):
        self.loading = True
        self.summary_table.setRowCount(len(self.compartments))

        for row, compartment in enumerate(self.compartments):
            values = self.summary_values(row, compartment)

            for col, value in enumerate(values):
                item = QTableWidgetItem(value)

                if col in {1, 11, 12, 13, 14, 15, 16}:
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)

                self.summary_table.setItem(row, col, item)

        self.loading = False

        if select_row is not None:
            select_row = max(0, min(select_row, len(self.compartments) - 1))
            self.selected_index = select_row
            self.summary_table.selectRow(select_row)

    def summary_values(self, row: int, compartment: Compartment) -> list[str]:
        return [
            compartment.id,
            str(row + 1),
            format_value(LENGTH, compartment.width),
            format_value(LENGTH, compartment.depth),
            format_value(LENGTH, compartment.height),
            format_value(LENGTH, compartment.origin_x),
            format_value(LENGTH, compartment.origin_y),
            format_value(LENGTH, compartment.origin_z),
            first_or_off(compartment.ceiling_matl_id),
            first_or_off(compartment.wall_matl_id),
            first_or_off(compartment.floor_matl_id),
            str(compartment.fire_count),
            str(compartment.hvent_count),
            str(compartment.vent_count),
            str(compartment.mechanical_count),
            str(compartment.detector_count),
            str(compartment.target_count),
        ]

    def summary_row_changed(self, current, previous):
        if self.loading:
            return

        if previous.isValid():
            self.save_detail_to_selected(previous.row())

        if current.isValid():
            self.selected_index = current.row()
            self.load_detail_from_selected()

    def summary_item_changed(self, item: QTableWidgetItem):
        if self.loading:
            return

        row = item.row()
        self.update_model_from_summary(row)

        if row == self.selected_index:
            self.load_detail_from_selected()

    def update_model_from_summary(self, row: int):
        if row < 0 or row >= len(self.compartments):
            return

        c = self.compartments[row]

        try:
            c.id = self.summary_cell(row, 0) or c.id
            c.width = parse_value(LENGTH, self.summary_cell(row, 2), "Width")
            c.depth = parse_value(LENGTH, self.summary_cell(row, 3), "Depth")
            c.height = parse_value(LENGTH, self.summary_cell(row, 4), "Height")
            c.origin_x = parse_value(LENGTH, self.summary_cell(row, 5), "X Position")
            c.origin_y = parse_value(LENGTH, self.summary_cell(row, 6), "Y Position")
            c.origin_z = parse_value(LENGTH, self.summary_cell(row, 7), "Z Position")
            c.ceiling_matl_id = self.replace_first_string(
                c.ceiling_matl_id,
                self.summary_cell(row, 8) or "OFF",
            )
            c.wall_matl_id = self.replace_first_string(
                c.wall_matl_id,
                self.summary_cell(row, 9) or "OFF",
            )
            c.floor_matl_id = self.replace_first_string(
                c.floor_matl_id,
                self.summary_cell(row, 10) or "OFF",
            )
        except ValueError:
            pass

    def summary_cell(self, row: int, col: int) -> str:
        item = self.summary_table.item(row, col)
        return "" if item is None else item.text().strip()

    def load_detail_from_selected(self):
        if not self.compartments:
            return

        c = self.compartments[self.selected_index]
        self.loading = True

        self.detail_group.setTitle(
            f"Compartment {self.selected_index + 1} (of {len(self.compartments)})"
        )

        self.id_edit.setText(c.id)
        self.width_edit.setText(format_value(LENGTH, c.width))
        self.depth_edit.setText(format_value(LENGTH, c.depth))
        self.height_edit.setText(format_value(LENGTH, c.height))
        self.x_edit.setText(format_value(LENGTH, c.origin_x))
        self.y_edit.setText(format_value(LENGTH, c.origin_y))
        self.z_edit.setText(format_value(LENGTH, c.origin_z))

        if c.shaft:
            self.shaft_radio.setChecked(True)
        elif c.hall:
            self.hall_radio.setChecked(True)
        else:
            self.normal_radio.setChecked(True)

        self.flow_coefficient_edit.setText(format_number(c.flow_coefficient))
        self.wall_leak_area_ratio_edit.setText(format_number(c.wall_leak_area_ratio))
        self.floor_leak_area_ratio_edit.setText(format_number(c.floor_leak_area_ratio))
        self.wall_leak_area_edit.setText(format_value(AREA, c.wall_leak_area))
        self.floor_leak_area_edit.setText(format_value(AREA, c.floor_leak_area))

        self.load_materials_table(c)
        self.load_area_table(c)
        self.fyi_edit.setText(c.fyi)

        self.loading = False

    def clear_detail(self):
        self.loading = True
        self.detail_group.setTitle("Compartment")

        for edit in (
            self.id_edit,
            self.width_edit,
            self.depth_edit,
            self.height_edit,
            self.x_edit,
            self.y_edit,
            self.z_edit,
            self.flow_coefficient_edit,
            self.wall_leak_area_ratio_edit,
            self.floor_leak_area_ratio_edit,
            self.wall_leak_area_edit,
            self.floor_leak_area_edit,
            self.fyi_edit,
        ):
            edit.clear()

        self.normal_radio.setChecked(True)
        self.clear_materials_table()
        self.area_table.clearContents()
        self.area_table.setRowCount(6)
        self.loading = False

    def clear_materials_table(self):
        for row in range(self.materials_table.rowCount()):
            for col in (0, 2, 4):
                self.materials_table.removeCellWidget(row, col)
        self.materials_table.clearContents()

    def load_materials_table(self, compartment: Compartment):
        self.materials_table.blockSignals(True)
        self.clear_materials_table()

        for row in range(3):
            values = [
                compartment.ceiling_matl_id[row],
                format_value(LENGTH, compartment.ceiling_thickness[row]),
                compartment.wall_matl_id[row],
                format_value(LENGTH, compartment.wall_thickness[row]),
                compartment.floor_matl_id[row],
                format_value(LENGTH, compartment.floor_thickness[row]),
            ]

            for col, value in enumerate(values):
                if col in (0, 2, 4):
                    self.set_material_combo(row, col, value)
                else:
                    self.materials_table.setItem(row, col, QTableWidgetItem(value))

        self.materials_table.blockSignals(False)

    def set_material_combo(self, row: int, col: int, value: str):
        combo = QComboBox(self.materials_table)
        combo.setEditable(True)
        self.populate_material_combo(combo, value)
        combo.activated.connect(lambda _index: self.save_detail_to_selected())
        if combo.lineEdit() is not None:
            combo.lineEdit().editingFinished.connect(self.save_detail_to_selected)
        self.materials_table.setCellWidget(row, col, combo)

    def populate_material_combo(self, combo: QComboBox, value: str):
        choices = list(self.material_choices)
        value = value.strip()
        if value and value not in choices:
            choices.append(value)

        combo.blockSignals(True)
        combo.clear()
        combo.addItems(choices)
        match = combo.findText(value, Qt.MatchFlag.MatchFixedString)
        if match >= 0:
            combo.setCurrentIndex(match)
        else:
            combo.setEditText(value)
        combo.blockSignals(False)

    def refresh_material_combo_choices(self):
        for row in range(self.materials_table.rowCount()):
            for col in (0, 2, 4):
                widget = self.materials_table.cellWidget(row, col)
                if isinstance(widget, QComboBox):
                    self.populate_material_combo(widget, widget.currentText())

    def load_area_table(self, compartment: Compartment):
        self.area_table.blockSignals(True)
        self.area_table.clearContents()
        row_count = max(6, len(compartment.cross_section_heights))
        self.area_table.setRowCount(row_count)

        for row, (height, area) in enumerate(
            zip(compartment.cross_section_heights, compartment.cross_section_areas)
        ):
            self.area_table.setItem(row, 0, QTableWidgetItem(format_value(LENGTH, height)))
            self.area_table.setItem(row, 1, QTableWidgetItem(format_value(AREA, area)))

        self.area_table.blockSignals(False)

    def save_detail_to_selected(self, row: int | None = None):
        if self.loading or not self.compartments:
            return

        if row is None:
            row = self.selected_index

        if row < 0 or row >= len(self.compartments):
            return

        c = self.compartments[row]

        try:
            c.id = self.id_edit.text().strip() or c.id
            c.width = parse_value(LENGTH, self.width_edit.text(), "Width")
            c.depth = parse_value(LENGTH, self.depth_edit.text(), "Depth")
            c.height = parse_value(LENGTH, self.height_edit.text(), "Height")
            c.origin_x = parse_value(LENGTH, self.x_edit.text() or "0", "X Position")
            c.origin_y = parse_value(LENGTH, self.y_edit.text() or "0", "Y Position")
            c.origin_z = parse_value(LENGTH, self.z_edit.text() or "0", "Z Position")
            c.shaft = self.shaft_radio.isChecked()
            c.hall = self.hall_radio.isChecked()
            c.flow_coefficient = parse_number(
                self.flow_coefficient_edit.text() or "0.07",
                "Flow Coefficient",
            )
            c.wall_leak_area_ratio = parse_number(
                self.wall_leak_area_ratio_edit.text() or "0",
                "Wall Leak Area Ratio",
            )
            c.floor_leak_area_ratio = parse_number(
                self.floor_leak_area_ratio_edit.text() or "0",
                "Floor Leak Area Ratio",
            )
            c.wall_leak_area = parse_value(
                AREA,
                self.wall_leak_area_edit.text() or "0",
                "Wall Leak Area",
            )
            c.floor_leak_area = parse_value(
                AREA,
                self.floor_leak_area_edit.text() or "0",
                "Floor Leak Area",
            )
            c.fyi = self.fyi_edit.text().strip()
            self.save_materials_from_table(c)
            self.save_area_from_table(c)
        except ValueError:
            return

        self.refresh_summary_table(select_row=row)

    def save_materials_from_table(self, compartment: Compartment):
        ceiling_ids: list[str] = []
        ceiling_thickness: list[float] = []
        wall_ids: list[str] = []
        wall_thickness: list[float] = []
        floor_ids: list[str] = []
        floor_thickness: list[float] = []

        for row in range(3):
            ceiling_ids.append(self.material_cell(row, 0) or "OFF")
            ceiling_thickness.append(parse_value(LENGTH, self.material_cell(row, 1) or "0", "Ceiling Thickness"))
            wall_ids.append(self.material_cell(row, 2) or "OFF")
            wall_thickness.append(parse_value(LENGTH, self.material_cell(row, 3) or "0", "Wall Thickness"))
            floor_ids.append(self.material_cell(row, 4) or "OFF")
            floor_thickness.append(parse_value(LENGTH, self.material_cell(row, 5) or "0", "Floor Thickness"))

        compartment.ceiling_matl_id = tuple(ceiling_ids)  # type: ignore[assignment]
        compartment.ceiling_thickness = tuple(ceiling_thickness)  # type: ignore[assignment]
        compartment.wall_matl_id = tuple(wall_ids)  # type: ignore[assignment]
        compartment.wall_thickness = tuple(wall_thickness)  # type: ignore[assignment]
        compartment.floor_matl_id = tuple(floor_ids)  # type: ignore[assignment]
        compartment.floor_thickness = tuple(floor_thickness)  # type: ignore[assignment]

    def save_area_from_table(self, compartment: Compartment):
        heights: list[float] = []
        areas: list[float] = []

        for row in range(self.area_table.rowCount()):
            height_text = self.area_cell(row, 0)
            area_text = self.area_cell(row, 1)

            if not height_text and not area_text:
                continue

            if not height_text or not area_text:
                raise ValueError("Variable area rows need both height and area.")

            heights.append(parse_value(LENGTH, height_text, "Cross-section Height"))
            areas.append(parse_value(AREA, area_text, "Cross-section Area"))

        compartment.cross_section_heights = heights
        compartment.cross_section_areas = areas

    def material_cell(self, row: int, col: int) -> str:
        widget = self.materials_table.cellWidget(row, col)
        if isinstance(widget, QComboBox):
            return widget.currentText().strip()

        item = self.materials_table.item(row, col)
        return "" if item is None else item.text().strip()

    def area_cell(self, row: int, col: int) -> str:
        item = self.area_table.item(row, col)
        return "" if item is None else item.text().strip()

    def add_to_case(self, case: CfastCase):
        self.save_detail_to_selected()

        if not self.compartments:
            raise ValueError("At least one compartment is required.")

        ids_seen: set[str] = set()

        for row, compartment in enumerate(self.compartments, start=1):
            if not compartment.id:
                raise ValueError(f"Compartments row {row}: ID is required.")

            if compartment.id in ids_seen:
                raise ValueError(
                    f"Compartments row {row}: duplicate ID {compartment.id!r}."
                )

            ids_seen.add(compartment.id)

            if compartment.width <= 0.0:
                raise ValueError(f"Compartments row {row}: width must be positive.")

            if compartment.depth <= 0.0:
                raise ValueError(f"Compartments row {row}: depth must be positive.")

            if compartment.height <= 0.0:
                raise ValueError(f"Compartments row {row}: height must be positive.")

            if any(grid_value <= 0 for grid_value in compartment.grid):
                raise ValueError(f"Compartments row {row}: grid values must be positive.")

            if len(compartment.cross_section_heights) != len(compartment.cross_section_areas):
                raise ValueError(
                    f"Compartments row {row}: variable area heights and areas differ."
                )

        case.compartments = copy.deepcopy(self.compartments)
        case.comp_id = case.compartments[0].id

    @staticmethod
    def replace_first_string(values: tuple[str, str, str], replacement: str):
        return (replacement, values[1], values[2])

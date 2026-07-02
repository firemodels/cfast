from __future__ import annotations

import copy
from dataclasses import replace

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
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, Target
from units import (
    CONDUCTIVITY,
    DENSITY,
    LENGTH,
    SPECIFIC_HEAT,
    format_number,
    format_value,
    parse_number,
    parse_value,
    unit_label,
)


MATERIAL_LIBRARY = {
    "DEFAULT": {
        "conductivity": "Default",
        "specific_heat": "Default",
        "density": "Default",
        "thickness": 0.0,
    },
    "CONCRETE": {
        "conductivity": 1.75,
        "specific_heat": 1.0,
        "density": 2200.0,
        "thickness": 0.15,
    },
    "GYPSUM": {
        "conductivity": 0.16,
        "specific_heat": 1.09,
        "density": 800.0,
        "thickness": 0.0127,
    },
}


def table_columns() -> list[str]:
    length = unit_label(LENGTH)
    return [
        "Num",
        "ID",
        "Compartment",
        f"X Position\n({length})",
        f"Y Position\n({length})",
        f"Z Position\n({length})",
        "X Normal",
        "Y Normal",
        "Z Normal",
        "Material",
        "Type",
    ]


class TargetsTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.targets: list[Target] = []
        self.current_index = -1
        self.updating = False
        self.editor_connections_ready = False

        self.summary_table = QTableWidget(0, len(table_columns()))
        self.summary_table.setHorizontalHeaderLabels(table_columns())
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.summary_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.summary_table.itemSelectionChanged.connect(self.summary_selection_changed)

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        move_up_button = QPushButton("Move Up")
        move_down_button = QPushButton("Move Down")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_target)
        duplicate_button.clicked.connect(self.duplicate_target)
        move_up_button.clicked.connect(self.move_target_up)
        move_down_button.clicked.connect(self.move_target_down)
        remove_button.clicked.connect(self.remove_target)

        button_row = QHBoxLayout()
        button_row.addStretch(1)
        button_row.addWidget(add_button)
        button_row.addWidget(duplicate_button)
        button_row.addWidget(move_up_button)
        button_row.addWidget(move_down_button)
        button_row.addStretch(1)
        button_row.addWidget(remove_button)
        button_row.addStretch(1)

        self.editor_group = QGroupBox("Target Geometry")
        self.editor_group.setLayout(self.build_editor_layout())

        layout = QVBoxLayout()
        layout.addWidget(self.summary_table, 2)
        layout.addLayout(button_row)
        layout.addWidget(self.editor_group, 3)
        self.setLayout(layout)

        self.load_demo_data()

    def load_case(self, case: CfastCase):
        self.refresh_unit_labels()
        self.targets = copy.deepcopy(case.targets)
        self.current_index = 0 if self.targets else -1
        self.refresh_summary_table(select_row=0 if self.targets else None)
        if self.targets:
            self.load_target_into_editor(self.targets[0])

    def build_editor_layout(self):
        layout = QVBoxLayout()

        header = QGridLayout()
        self.id_edit = QLineEdit()
        self.compartment_combo = QComboBox()
        self.compartment_combo.setEditable(True)
        self.target_type_combo = QComboBox()
        self.target_type_combo.addItems(["Plate", "Cylinder"])

        header.addWidget(QLabel("ID:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        header.addWidget(self.id_edit, 0, 1)
        header.addWidget(
            QLabel("Compartment:"),
            0,
            2,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        header.addWidget(self.compartment_combo, 0, 3)
        header.addWidget(
            QLabel("Target Type:"),
            0,
            4,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        header.addWidget(self.target_type_combo, 0, 5)
        header.setColumnStretch(1, 1)
        header.setColumnStretch(3, 2)
        header.setColumnStretch(5, 1)

        body = QHBoxLayout()
        body.addWidget(self.build_geometry_group(), 3)
        body.addWidget(self.build_construction_group(), 2)

        layout.addLayout(header)
        layout.addLayout(body)
        return layout

    def build_geometry_group(self):
        group = QGroupBox("Target Geometry")
        layout = QGridLayout()

        self.x_edit = QLineEdit()
        self.y_edit = QLineEdit()
        self.z_edit = QLineEdit()
        self.normal_mode_combo = QComboBox()
        self.normal_mode_combo.addItems(["User Specified"])
        self.nx_edit = QLineEdit()
        self.ny_edit = QLineEdit()
        self.nz_edit = QLineEdit()

        layout.addWidget(QLabel("Target Position"), 0, 1)
        layout.addWidget(QLabel("Normal Vector Points to"), 0, 3)

        layout.addWidget(QLabel("Width (X):"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.x_edit, 1, 1)
        layout.addWidget(self.normal_mode_combo, 1, 3)

        layout.addWidget(QLabel("Depth (Y):"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.y_edit, 2, 1)
        layout.addWidget(QLabel("Normal (X):"), 2, 2, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.nx_edit, 2, 3)

        layout.addWidget(QLabel("Height (Z):"), 3, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.z_edit, 3, 1)
        layout.addWidget(QLabel("Normal (Y):"), 3, 2, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.ny_edit, 3, 3)

        layout.addWidget(QLabel("Normal (Z):"), 4, 2, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.nz_edit, 4, 3)

        group.setLayout(layout)
        return group

    def build_construction_group(self):
        group = QGroupBox("Target Construction")
        layout = QGridLayout()

        self.material_combo = QComboBox()
        self.material_combo.setEditable(True)
        self.material_combo.addItems(["CONCRETE", "GYPSUM", "DEFAULT"])

        self.conductivity_label = QLabel("Conductivity: ")
        self.specific_heat_label = QLabel("Specific Heat: ")
        self.density_label = QLabel("Density: ")
        self.thickness_label = QLabel("Thickness: ")
        self.temperature_depth_edit = QLineEdit()

        layout.addWidget(QLabel("Material:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.material_combo, 0, 1)
        layout.addWidget(self.conductivity_label, 1, 0, 1, 2)
        layout.addWidget(self.specific_heat_label, 2, 0, 1, 2)
        layout.addWidget(self.density_label, 3, 0, 1, 2)
        layout.addWidget(self.thickness_label, 4, 0, 1, 2)
        layout.addWidget(
            QLabel("Internal Temperature at:"),
            5,
            0,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        layout.addWidget(self.temperature_depth_edit, 5, 1)

        group.setLayout(layout)
        return group

    def load_demo_data(self):
        self.targets = [
            Target(
                id="Targ 1",
                comp_id="Comp 1",
                x_position=2.2,
                y_position=1.88,
                z_position=2.34,
                x_normal=0.0,
                y_normal=0.0,
                z_normal=1.0,
                matl_id="CONCRETE",
                target_type="PLATE",
                thickness=0.15,
                temperature_depth=0.075,
                depth_units="DISTANCE",
            )
        ]

        self.refresh_summary_table(select_row=0)

    def refresh_summary_table(self, select_row: int | None = None):
        self.updating = True
        self.summary_table.setRowCount(len(self.targets))

        for row, target in enumerate(self.targets):
            values = [
                str(row + 1),
                target.id,
                target.comp_id,
                format_value(LENGTH, target.x_position),
                format_value(LENGTH, target.y_position),
                format_value(LENGTH, target.z_position),
                format_number(target.x_normal),
                format_number(target.y_normal),
                format_number(target.z_normal),
                target.matl_id,
                display_target_type(target.target_type),
            ]

            for col, value in enumerate(values):
                item = QTableWidgetItem(value)
                if col == 0:
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.summary_table.setItem(row, col, item)

        self.updating = False

        if self.targets:
            if select_row is None:
                select_row = max(0, min(self.current_index, len(self.targets) - 1))
            self.summary_table.selectRow(select_row)
        else:
            self.current_index = -1
            self.load_target_into_editor(None)

    def summary_selection_changed(self):
        if self.updating:
            return

        indexes = self.summary_table.selectionModel().selectedRows()

        if not indexes:
            return

        self.current_index = indexes[0].row()
        self.load_target_into_editor(self.targets[self.current_index])

    def load_target_into_editor(self, target: Target | None):
        self.updating = True

        if target is None:
            self.editor_group.setTitle("Target Geometry")
            for widget in self.editor_widgets():
                widget.clear()
            self.updating = False
            return

        self.editor_group.setTitle(
            f"Target {self.current_index + 1} (of {len(self.targets)}) Geometry"
        )
        self.id_edit.setText(target.id)
        set_combo_text(self.compartment_combo, target.comp_id)
        set_combo_text(self.target_type_combo, display_target_type(target.target_type))
        self.x_edit.setText(format_value(LENGTH, target.x_position))
        self.y_edit.setText(format_value(LENGTH, target.y_position))
        self.z_edit.setText(format_value(LENGTH, target.z_position))
        self.nx_edit.setText(format_number(target.x_normal))
        self.ny_edit.setText(format_number(target.y_normal))
        self.nz_edit.setText(format_number(target.z_normal))
        set_combo_text(self.material_combo, target.matl_id)
        self.temperature_depth_edit.setText(format_value(LENGTH, target.temperature_depth))
        self.update_material_labels(target)

        self.updating = False

        self.connect_editor_signals_once()

    def connect_editor_signals_once(self):
        if self.editor_connections_ready:
            return

        for widget in self.editor_widgets():
            widget.textChanged.connect(self.editor_changed)

        for combo in self.editor_combos():
            combo.currentTextChanged.connect(self.editor_changed)

        self.editor_connections_ready = True

    def editor_widgets(self):
        return [
            self.id_edit,
            self.x_edit,
            self.y_edit,
            self.z_edit,
            self.nx_edit,
            self.ny_edit,
            self.nz_edit,
            self.temperature_depth_edit,
        ]

    def editor_combos(self):
        return [
            self.compartment_combo,
            self.target_type_combo,
            self.material_combo,
        ]

    def editor_changed(self):
        if self.updating or self.current_index < 0:
            return

        try:
            target = self.target_from_editor()
        except ValueError:
            return

        self.targets[self.current_index] = target
        self.update_material_labels(target)
        self.refresh_summary_table(select_row=self.current_index)

    def target_from_editor(self) -> Target:
        matl_id = self.material_combo.currentText().strip() or "DEFAULT"
        material = material_properties(matl_id)
        existing = self.targets[self.current_index] if 0 <= self.current_index < len(self.targets) else None
        if matl_id.strip().upper() in MATERIAL_LIBRARY:
            thickness = material.get("thickness", 0.0)
        else:
            thickness = existing.thickness if existing is not None else 0.0

        return Target(
            id=self.id_edit.text().strip() or f"Targ {self.current_index + 1}",
            comp_id=self.compartment_combo.currentText().strip() or "Comp 1",
            x_position=parse_value(LENGTH, self.x_edit.text(), "X Position"),
            y_position=parse_value(LENGTH, self.y_edit.text(), "Y Position"),
            z_position=parse_value(LENGTH, self.z_edit.text(), "Z Position"),
            x_normal=parse_number(self.nx_edit.text(), "X Normal"),
            y_normal=parse_number(self.ny_edit.text(), "Y Normal"),
            z_normal=parse_number(self.nz_edit.text(), "Z Normal"),
            matl_id=matl_id,
            target_type=self.target_type_combo.currentText().strip().upper(),
            thickness=float(thickness) if isinstance(thickness, (float, int)) else 0.0,
            temperature_depth=parse_value(
                LENGTH,
                self.temperature_depth_edit.text(),
                "Internal Temperature Depth",
            ),
            depth_units=existing.depth_units if existing is not None else "DISTANCE",
            surface_orientation=(
                existing.surface_orientation if existing is not None else "USER SPECIFIED"
            ),
            surface_temperature=existing.surface_temperature if existing is not None else None,
            adiabatic=existing.adiabatic if existing is not None else False,
            convection_coefficient_front=(
                existing.convection_coefficient_front if existing is not None else 0.0
            ),
            convection_coefficient_back=(
                existing.convection_coefficient_back if existing is not None else 0.0
            ),
            fyi=existing.fyi if existing is not None else "",
        )

    def update_material_labels(self, target: Target):
        material = material_properties(target.matl_id)
        self.conductivity_label.setText(
            f"Conductivity: {format_property(material.get('conductivity'), CONDUCTIVITY)} {unit_label(CONDUCTIVITY)}"
        )
        self.specific_heat_label.setText(
            f"Specific Heat: {format_property(material.get('specific_heat'), SPECIFIC_HEAT)} {unit_label(SPECIFIC_HEAT)}"
        )
        self.density_label.setText(
            f"Density: {format_property(material.get('density'), DENSITY)} {unit_label(DENSITY)}"
        )
        self.thickness_label.setText(
            f"Thickness: {format_property(material.get('thickness'), LENGTH)} {unit_label(LENGTH)}"
        )

    def add_target(self):
        next_number = len(self.targets) + 1
        base = self.targets[-1] if self.targets else None

        if base is None:
            target = Target(
                id=f"Targ {next_number}",
                comp_id="Comp 1",
                x_position=0.0,
                y_position=0.0,
                z_position=0.0,
                matl_id="DEFAULT",
            )
        else:
            target = replace(base, id=f"Targ {next_number}")

        self.targets.append(target)
        self.refresh_summary_table(select_row=len(self.targets) - 1)

    def duplicate_target(self):
        if self.current_index < 0:
            return

        target = replace(
            self.targets[self.current_index],
            id=unique_id(
                self.targets[self.current_index].id,
                {target.id for target in self.targets},
            ),
        )
        insert_at = self.current_index + 1
        self.targets.insert(insert_at, target)
        self.refresh_summary_table(select_row=insert_at)

    def move_target_up(self):
        if self.current_index <= 0:
            return

        row = self.current_index
        self.targets[row - 1], self.targets[row] = self.targets[row], self.targets[row - 1]
        self.refresh_summary_table(select_row=row - 1)

    def move_target_down(self):
        if self.current_index < 0 or self.current_index >= len(self.targets) - 1:
            return

        row = self.current_index
        self.targets[row + 1], self.targets[row] = self.targets[row], self.targets[row + 1]
        self.refresh_summary_table(select_row=row + 1)

    def remove_target(self):
        if self.current_index < 0:
            return

        del self.targets[self.current_index]
        if not self.targets:
            self.current_index = -1
            self.refresh_summary_table()
            return

        self.refresh_summary_table(select_row=min(self.current_index, len(self.targets) - 1))

    def add_to_case(self, case: CfastCase):
        if self.current_index >= 0:
            try:
                self.targets[self.current_index] = self.target_from_editor()
            except ValueError:
                pass

        ids_seen: set[str] = set()

        for row, target in enumerate(self.targets):
            if not target.id:
                raise ValueError(f"Targets row {row + 1}: ID is required.")

            if target.id in ids_seen:
                raise ValueError(f"Targets row {row + 1}: duplicate ID {target.id!r}.")

            ids_seen.add(target.id)

            if not target.comp_id:
                raise ValueError(f"Targets row {row + 1}: compartment is required.")

            if target.target_type.upper() not in {"PLATE", "CYLINDER"}:
                raise ValueError(
                    f"Targets row {row + 1}: target type must be Plate or Cylinder."
                )

        case.targets = list(self.targets)

    def refresh_unit_labels(self):
        self.summary_table.setHorizontalHeaderLabels(table_columns())


def set_combo_text(combo: QComboBox, text: str):
    index = combo.findText(text)
    if index >= 0:
        combo.setCurrentIndex(index)
    else:
        combo.setEditText(text)


def display_target_type(value: str) -> str:
    value = value.strip().upper()
    if value == "CYLINDER":
        return "Cylinder"
    return "Plate"


def material_properties(matl_id: str) -> dict:
    return MATERIAL_LIBRARY.get(matl_id.strip().upper(), MATERIAL_LIBRARY["DEFAULT"])


def format_property(value, kind: str) -> str:
    if isinstance(value, str):
        return value
    return format_value(kind, float(value), include_unit=False)


def unique_id(base: str, existing: set[str]) -> str:
    root = base.strip() or "Targ"
    index = 2

    while True:
        candidate = f"{root}_{index}"
        if candidate not in existing:
            return candidate
        index += 1

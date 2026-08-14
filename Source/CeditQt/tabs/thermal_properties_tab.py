from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QAbstractItemView,
    QDialog,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QFileDialog,
    QMessageBox,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, MaterialProperty
from cfast_reader import read_cfast_input_with_warnings
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


def read_only_item(text: str) -> QTableWidgetItem:
    item = QTableWidgetItem(text)
    item.setFlags(Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled)
    return item


class ThermalPropertyImportDialog(QDialog):
    def __init__(self, materials: list[MaterialProperty], parent=None):
        super().__init__(parent)

        self.materials = materials
        self.setWindowTitle("Insert Thermal Properties")
        self.resize(800, 360)

        self.table = QTableWidget(len(materials), 7)
        self.table.setHorizontalHeaderLabels(
            [
                "",
                "ID",
                "Material",
                f"Conductivity\n({unit_label(CONDUCTIVITY)})",
                f"Specific Heat\n({unit_label(SPECIFIC_HEAT)})",
                f"Density\n({unit_label(DENSITY)})",
                f"Thickness\n({unit_label(LENGTH)})",
            ]
        )
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents
        )
        self.table.horizontalHeader().setStretchLastSection(True)

        for row, material in enumerate(materials):
            check_item = QTableWidgetItem()
            check_item.setFlags(
                Qt.ItemFlag.ItemIsUserCheckable | Qt.ItemFlag.ItemIsEnabled
            )
            check_item.setCheckState(Qt.CheckState.Checked)
            self.table.setItem(row, 0, check_item)
            self.table.setItem(row, 1, read_only_item(material.id))
            self.table.setItem(row, 2, read_only_item(material.material))
            self.table.setItem(
                row, 3, read_only_item(format_value(CONDUCTIVITY, material.conductivity))
            )
            self.table.setItem(
                row, 4, read_only_item(format_value(SPECIFIC_HEAT, material.specific_heat))
            )
            self.table.setItem(
                row, 5, read_only_item(format_value(DENSITY, material.density))
            )
            self.table.setItem(
                row, 6, read_only_item(format_value(LENGTH, material.thickness))
            )

        select_all_button = QPushButton("Select All")
        deselect_all_button = QPushButton("Deselect All")
        ok_button = QPushButton("OK")
        cancel_button = QPushButton("Cancel")

        select_all_button.clicked.connect(
            lambda: self.set_all_checked(Qt.CheckState.Checked)
        )
        deselect_all_button.clicked.connect(
            lambda: self.set_all_checked(Qt.CheckState.Unchecked)
        )
        ok_button.clicked.connect(self.accept)
        cancel_button.clicked.connect(self.reject)

        button_layout = QHBoxLayout()
        button_layout.addStretch(1)
        button_layout.addWidget(select_all_button)
        button_layout.addWidget(deselect_all_button)
        button_layout.addStretch(1)
        button_layout.addWidget(ok_button)
        button_layout.addWidget(cancel_button)

        layout = QVBoxLayout()
        layout.addWidget(self.table)
        layout.addLayout(button_layout)
        self.setLayout(layout)

    def set_all_checked(self, state: Qt.CheckState) -> None:
        for row in range(self.table.rowCount()):
            item = self.table.item(row, 0)
            if item is not None:
                item.setCheckState(state)

    def selected_materials(self) -> list[MaterialProperty]:
        selected: list[MaterialProperty] = []
        for row, material in enumerate(self.materials):
            item = self.table.item(row, 0)
            if item is not None and item.checkState() == Qt.CheckState.Checked:
                selected.append(material)
        return selected


class ThermalPropertiesTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.table = QTableWidget(8, 8)
        self.refresh_unit_labels()

        self.table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.table.verticalHeader().setVisible(True)
        self.table.itemChanged.connect(self.cell_changed)

        add_row_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        from_file_button = QPushButton("From File")
        remove_button = QPushButton("Remove")

        add_row_button.clicked.connect(self.add_row)
        duplicate_button.clicked.connect(self.duplicate_row)
        from_file_button.clicked.connect(self.import_from_file)
        remove_button.clicked.connect(self.delete_selected_rows)

        layout = QVBoxLayout()
        layout.addWidget(QLabel("<b>Thermal Properties</b>"))
        layout.addWidget(self.table)

        button_layout = QHBoxLayout()
        button_layout.addWidget(add_row_button)
        button_layout.addWidget(duplicate_button)
        button_layout.addWidget(from_file_button)
        button_layout.addWidget(remove_button)
        button_layout.addStretch(1)

        layout.addLayout(button_layout)
        self.setLayout(layout)

        self.load_demo_data()

    def load_case(self, case: CfastCase):
        self.refresh_unit_labels()
        rows = max(1, len(case.materials))
        self.table.blockSignals(True)
        self.table.clearContents()
        self.table.setRowCount(rows)

        for row, material in enumerate(case.materials):
            values = [
                material.id,
                material.material,
                format_value(CONDUCTIVITY, material.conductivity),
                format_value(SPECIFIC_HEAT, material.specific_heat),
                format_value(DENSITY, material.density),
                format_value(LENGTH, material.thickness),
                format_number(material.emissivity),
                material.fyi,
            ]

            for col, value in enumerate(values):
                self.table.setItem(row, col, QTableWidgetItem(value))

        self.table.blockSignals(False)

    def load_demo_data(self):
        demo_rows = [
            [
                "CONCRETE",
                "Concrete Normal Weight (6 in)",
                format_value(CONDUCTIVITY, 1.75),
                format_value(SPECIFIC_HEAT, 1.0),
                format_value(DENSITY, 2200.0),
                format_value(LENGTH, 0.15),
                "0.94",
                "",
            ],
            [
                "GYPSUM",
                "Gypsum Wallboard",
                format_value(CONDUCTIVITY, 0.16),
                format_value(SPECIFIC_HEAT, 1.09),
                format_value(DENSITY, 800.0),
                format_value(LENGTH, 0.0127),
                "0.9",
                "",
            ],
        ]

        self.table.blockSignals(True)

        for row, values in enumerate(demo_rows):
            for col, value in enumerate(values):
                self.table.setItem(row, col, QTableWidgetItem(value))

        self.table.blockSignals(False)

    def add_row(self):
        material_id, material_name = self.next_new_material_names()
        values = [
            material_id,
            material_name,
            format_value(CONDUCTIVITY, 0.16),
            format_value(SPECIFIC_HEAT, 900.0),
            format_value(DENSITY, 790.0),
            format_value(LENGTH, 0.016),
            "0.9",
            "",
        ]

        row = self.first_blank_row()
        if row is None:
            row = self.table.rowCount()
            self.table.insertRow(row)

        self.table.blockSignals(True)
        for col, value in enumerate(values):
            self.table.setItem(row, col, QTableWidgetItem(value))
        self.table.blockSignals(False)

        self.table.setCurrentCell(row, 0)

    def duplicate_row(self):
        source_row = self.selected_row()

        if source_row is None:
            return

        values = [
            self.cell_text(source_row, col) for col in range(self.table.columnCount())
        ]

        if not any(values):
            return

        values[0], values[1] = self.next_new_material_names()

        row = self.first_blank_row()
        if row is None:
            row = self.table.rowCount()
            self.table.insertRow(row)

        self.table.blockSignals(True)
        for col, value in enumerate(values):
            self.table.setItem(row, col, QTableWidgetItem(value))
        self.table.blockSignals(False)

        self.table.selectRow(row)

    def import_from_file(self):
        paths, _ = QFileDialog.getOpenFileNames(
            self,
            "Insert Thermal Properties",
            "",
            "CFAST files (*.in *.cfast);;All files (*.*)",
        )

        if not paths:
            return

        materials: list[MaterialProperty] = []
        errors: list[str] = []

        for path in paths:
            try:
                result = read_cfast_input_with_warnings(path)
            except ValueError as exc:
                errors.append(str(exc))
                continue

            materials.extend(result.case.materials)

        if not materials:
            message = "No thermal properties were found."
            if errors:
                message += "\n\nFiles with errors:\n" + "\n".join(errors)
                QMessageBox.warning(self, "Insert Thermal Properties", message)
            else:
                QMessageBox.information(self, "Insert Thermal Properties", message)
            return

        dialog = ThermalPropertyImportDialog(materials, self)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return

        added = 0
        skipped = 0

        for material in dialog.selected_materials():
            if self.has_material_id(material.id):
                skipped += 1
                continue

            self.add_material_property(material)
            added += 1

        message = f"Added {added} thermal propert{'y' if added == 1 else 'ies'}."
        if skipped:
            message += f"\nSkipped {skipped} duplicate ID{'s' if skipped != 1 else ''}."
        if errors:
            message += "\n\nFiles with errors:\n" + "\n".join(errors)
            QMessageBox.warning(self, "Insert Thermal Properties", message)
        else:
            QMessageBox.information(self, "Insert Thermal Properties", message)

    def selected_row(self) -> int | None:
        selected_rows = sorted(
            {index.row() for index in self.table.selectionModel().selectedIndexes()}
        )

        if selected_rows:
            return selected_rows[0]

        row = self.table.currentRow()
        if row >= 0:
            return row

        return None

    def next_new_material_names(self) -> tuple[str, str]:
        existing_ids = set(self.material_ids())
        index = max(1, len(existing_ids) + 1)

        while True:
            material_id = f"NM {index}"
            if material_id not in existing_ids:
                return material_id, f"New Material {index}"
            index += 1

    def delete_selected_rows(self):
        selected_rows = sorted(
            {index.row() for index in self.table.selectionModel().selectedIndexes()},
            reverse=True,
        )

        for row in selected_rows:
            self.table.removeRow(row)

        if self.table.rowCount() == 0:
            self.table.setRowCount(1)

    def cell_changed(self, item: QTableWidgetItem):
        kind_by_column = {
            2: CONDUCTIVITY,
            3: SPECIFIC_HEAT,
            4: DENSITY,
            5: LENGTH,
        }
        kind = kind_by_column.get(item.column())
        if kind is None:
            return

        text = item.text().strip()
        if not text:
            return

        try:
            value = parse_value(kind, text, self.table.horizontalHeaderItem(item.column()).text())
        except ValueError:
            return

        self.table.blockSignals(True)
        item.setText(format_value(kind, value))
        self.table.blockSignals(False)

    def cell_text(self, row: int, col: int) -> str:
        item = self.table.item(row, col)

        if item is None:
            return ""

        return item.text().strip()

    def material_ids(self) -> list[str]:
        material_ids: list[str] = []
        for row in range(self.table.rowCount()):
            material_id = self.cell_text(row, 0)
            if material_id:
                material_ids.append(material_id)
        return material_ids

    def has_material_id(self, material_id: str) -> bool:
        return material_id in self.material_ids()

    def add_material_property(self, material: MaterialProperty):
        if self.has_material_id(material.id):
            return

        row = self.first_blank_row()
        if row is None:
            row = self.table.rowCount()
            self.table.insertRow(row)

        values = [
            material.id,
            material.material,
            format_value(CONDUCTIVITY, material.conductivity),
            format_value(SPECIFIC_HEAT, material.specific_heat),
            format_value(DENSITY, material.density),
            format_value(LENGTH, material.thickness),
            format_number(material.emissivity),
            material.fyi,
        ]

        self.table.blockSignals(True)
        for col, value in enumerate(values):
            self.table.setItem(row, col, QTableWidgetItem(value))
        self.table.blockSignals(False)

    def first_blank_row(self) -> int | None:
        for row in range(self.table.rowCount()):
            values = [self.cell_text(row, col) for col in range(self.table.columnCount())]
            if not any(values):
                return row
        return None

    def add_to_case(self, case: CfastCase):
        case.materials = self.materials_from_table()

    def materials_from_table(self) -> list[MaterialProperty]:
        materials: list[MaterialProperty] = []
        ids_seen: set[str] = set()

        for row in range(self.table.rowCount()):
            values = [self.cell_text(row, col) for col in range(self.table.columnCount())]

            if not any(values):
                continue

            matl_id = values[0]
            material_name = values[1]

            if not matl_id:
                raise ValueError(f"Thermal Properties row {row + 1}: ID is required.")

            if matl_id in ids_seen:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: duplicate ID {matl_id!r}."
                )

            ids_seen.add(matl_id)

            material = MaterialProperty(
                id=matl_id,
                material=material_name or matl_id,
                conductivity=parse_value(CONDUCTIVITY, values[2], "Conductivity"),
                specific_heat=parse_value(SPECIFIC_HEAT, values[3], "Specific Heat"),
                density=parse_value(DENSITY, values[4], "Density"),
                thickness=parse_value(LENGTH, values[5], "Thickness"),
                emissivity=parse_number(values[6], "Emissivity"),
                fyi=values[7],
            )

            if material.conductivity < 0.0:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: conductivity must be non-negative."
                )

            if material.specific_heat < 0.0:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: specific heat must be non-negative."
                )

            if material.density < 0.0:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: density must be non-negative."
                )

            if material.thickness < 0.0:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: thickness must be non-negative."
                )

            if not 0.0 <= material.emissivity <= 1.0:
                raise ValueError(
                    f"Thermal Properties row {row + 1}: emissivity must be 0 to 1."
                )

            materials.append(material)

        return materials

    def refresh_unit_labels(self):
        self.table.setHorizontalHeaderLabels(
            [
                "ID",
                "Material",
                f"Conductivity\n({unit_label(CONDUCTIVITY)})",
                f"Specific Heat\n({unit_label(SPECIFIC_HEAT)})",
                f"Density\n({unit_label(DENSITY)})",
                f"Thickness\n({unit_label(LENGTH)})",
                "Emissivity",
                "FYI",
            ]
        )

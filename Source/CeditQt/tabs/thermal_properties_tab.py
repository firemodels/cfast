import re

from PySide6.QtWidgets import (
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, MaterialProperty


def parse_float(text: str, field_name: str) -> float:
    text = text.strip()

    match = re.search(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?", text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


class ThermalPropertiesTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.table = QTableWidget(8, 8)
        self.table.setHorizontalHeaderLabels(
            [
                "ID",
                "Material",
                "Conductivity\n(W/m-K)",
                "Specific Heat\n(kJ/kg-K)",
                "Density\n(kg/m3)",
                "Thickness\n(m)",
                "Emissivity",
                "FYI",
            ]
        )

        self.table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.table.verticalHeader().setVisible(True)

        add_row_button = QPushButton("Add Row")
        delete_row_button = QPushButton("Delete Selected")
        clear_button = QPushButton("Clear")

        add_row_button.clicked.connect(self.add_row)
        delete_row_button.clicked.connect(self.delete_selected_rows)
        clear_button.clicked.connect(self.clear_table)

        layout = QVBoxLayout()
        layout.addWidget(QLabel("<b>Thermal Properties</b>"))
        layout.addWidget(self.table)

        button_layout = QHBoxLayout()
        button_layout.addWidget(add_row_button)
        button_layout.addWidget(delete_row_button)
        button_layout.addWidget(clear_button)
        button_layout.addStretch(1)

        layout.addLayout(button_layout)
        self.setLayout(layout)

        self.load_demo_data()

    def load_demo_data(self):
        demo_rows = [
            [
                "GYPSUM",
                "Gypsum Wallboard",
                "0.16",
                "1.09",
                "800",
                "0.0127",
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
        self.table.insertRow(self.table.rowCount())

    def delete_selected_rows(self):
        selected_rows = sorted(
            {index.row() for index in self.table.selectionModel().selectedIndexes()},
            reverse=True,
        )

        for row in selected_rows:
            self.table.removeRow(row)

        if self.table.rowCount() == 0:
            self.table.setRowCount(1)

    def clear_table(self):
        self.table.clearContents()

    def cell_text(self, row: int, col: int) -> str:
        item = self.table.item(row, col)

        if item is None:
            return ""

        return item.text().strip()

    def add_to_case(self, case: CfastCase):
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
                conductivity=parse_float(values[2], "Conductivity"),
                specific_heat=parse_float(values[3], "Specific Heat"),
                density=parse_float(values[4], "Density"),
                thickness=parse_float(values[5], "Thickness"),
                emissivity=parse_float(values[6], "Emissivity"),
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

        case.materials = materials

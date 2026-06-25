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

from cfast_case import CfastCase, Compartment


def parse_float(text: str, field_name: str) -> float:
    text = text.strip()
    match = re.search(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?", text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


def parse_int(text: str, field_name: str) -> int:
    value = parse_float(text, field_name)

    if abs(value - round(value)) > 1.0e-12:
        raise ValueError(f"{field_name} must be an integer: {text!r}")

    return int(round(value))


def parse_bool(text: str) -> bool:
    value = text.strip().lower()

    if value in {"true", ".true.", "t", "yes", "y", "1"}:
        return True

    if value in {"false", ".false.", "f", "no", "n", "0", ""}:
        return False

    raise ValueError(f"Could not parse logical value: {text!r}")


class CompartmentsTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.table = QTableWidget(8, 19)
        self.table.setHorizontalHeaderLabels(
            [
                "ID",
                "Width\n(m)",
                "Depth\n(m)",
                "Height\n(m)",
                "X0\n(m)",
                "Y0\n(m)",
                "Z0\n(m)",
                "Ceiling\nMatl ID",
                "Ceiling\nThick (m)",
                "Wall\nMatl ID",
                "Wall\nThick (m)",
                "Floor\nMatl ID",
                "Floor\nThick (m)",
                "Grid I",
                "Grid J",
                "Grid K",
                "Hall",
                "Shaft",
                "FYI",
            ]
        )

        self.table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents
        )
        self.table.verticalHeader().setVisible(True)

        add_row_button = QPushButton("Add Row")
        delete_row_button = QPushButton("Delete Selected")
        clear_button = QPushButton("Clear")

        add_row_button.clicked.connect(self.add_row)
        delete_row_button.clicked.connect(self.delete_selected_rows)
        clear_button.clicked.connect(self.clear_table)

        layout = QVBoxLayout()
        layout.addWidget(QLabel("<b>Compartments</b>"))
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
                "Comp 1",
                "5",
                "5",
                "5",
                "0",
                "0",
                "0",
                "OFF",
                "0",
                "OFF",
                "0",
                "OFF",
                "0",
                "50",
                "50",
                "50",
                ".FALSE.",
                ".FALSE.",
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
        compartments: list[Compartment] = []
        ids_seen: set[str] = set()

        for row in range(self.table.rowCount()):
            values = [self.cell_text(row, col) for col in range(self.table.columnCount())]

            if not any(values):
                continue

            comp_id = values[0]

            if not comp_id:
                raise ValueError(f"Compartments row {row + 1}: ID is required.")

            if comp_id in ids_seen:
                raise ValueError(
                    f"Compartments row {row + 1}: duplicate ID {comp_id!r}."
                )

            ids_seen.add(comp_id)

            compartment = Compartment(
                id=comp_id,
                width=parse_float(values[1], "Width"),
                depth=parse_float(values[2], "Depth"),
                height=parse_float(values[3], "Height"),
                origin_x=parse_float(values[4] or "0", "X0"),
                origin_y=parse_float(values[5] or "0", "Y0"),
                origin_z=parse_float(values[6] or "0", "Z0"),
                ceiling_matl_id=values[7] or "OFF",
                ceiling_thickness=parse_float(values[8] or "0", "Ceiling Thickness"),
                wall_matl_id=values[9] or "OFF",
                wall_thickness=parse_float(values[10] or "0", "Wall Thickness"),
                floor_matl_id=values[11] or "OFF",
                floor_thickness=parse_float(values[12] or "0", "Floor Thickness"),
                grid=(
                    parse_int(values[13] or "50", "Grid I"),
                    parse_int(values[14] or "50", "Grid J"),
                    parse_int(values[15] or "50", "Grid K"),
                ),
                hall=parse_bool(values[16]),
                shaft=parse_bool(values[17]),
                fyi=values[18],
            )

            if compartment.width <= 0.0:
                raise ValueError(f"Compartments row {row + 1}: width must be positive.")

            if compartment.depth <= 0.0:
                raise ValueError(f"Compartments row {row + 1}: depth must be positive.")

            if compartment.height <= 0.0:
                raise ValueError(f"Compartments row {row + 1}: height must be positive.")

            if any(grid_value <= 0 for grid_value in compartment.grid):
                raise ValueError(
                    f"Compartments row {row + 1}: grid values must be positive."
                )

            compartments.append(compartment)

        if not compartments:
            raise ValueError("At least one compartment is required.")

        case.compartments = compartments
        case.comp_id = compartments[0].id

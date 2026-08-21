from __future__ import annotations

import copy
import re

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

from cfast_case import (
    CeilingFloorSurfaceConnection,
    CfastCase,
    WallSurfaceConnection,
)
from table_widgets import HoverEditTableWidget

_NUMBER_RE = re.compile(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?")


def parse_float(text: str, field_name: str, default: float | None = None) -> float:
    text = text.strip()

    if not text and default is not None:
        return default

    match = _NUMBER_RE.search(text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


def make_item(text: str, editable: bool = True) -> QTableWidgetItem:
    item = QTableWidgetItem(text)
    flags = Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled
    if editable:
        flags |= Qt.ItemFlag.ItemIsEditable
    item.setFlags(flags)
    return item


def set_combo_text(combo: QComboBox, text: str) -> None:
    index = combo.findText(text)
    if index < 0:
        combo.addItem(text)
        index = combo.findText(text)
    combo.setCurrentIndex(index)


class SurfaceConnectionsTab(QWidget):
    WALL_COL_NUM = 0
    WALL_COL_TYPE = 1
    WALL_COL_FIRST = 2
    WALL_COL_SECOND = 3
    WALL_COL_FRACTION = 4

    FLOOR_COL_NUM = 0
    FLOOR_COL_TYPE = 1
    FLOOR_COL_TOP = 2
    FLOOR_COL_BOTTOM = 3

    def __init__(self, parent=None):
        super().__init__(parent)

        self.updating_wall_table = False
        self.loading_wall_editor = False
        self.current_wall_index = -1
        self.wall_connections: list[WallSurfaceConnection] = []

        self.updating_floor_table = False
        self.loading_floor_editor = False
        self.current_floor_index = -1
        self.ceiling_floor_connections: list[CeilingFloorSurfaceConnection] = []

        self.wall_table = HoverEditTableWidget(0, 5)
        self.wall_table.setHorizontalHeaderLabels(
            ["Num", "Type", "First", "Second", "Fraction"]
        )
        self.wall_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.wall_table.verticalHeader().setVisible(False)
        self.wall_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.wall_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.wall_table.itemSelectionChanged.connect(self.wall_selection_changed)
        self.wall_table.itemChanged.connect(self.wall_table_changed)

        self.floor_table = HoverEditTableWidget(0, 4)
        self.floor_table.setHorizontalHeaderLabels(["Num", "Type", "Top", "Bottom"])
        self.floor_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.floor_table.verticalHeader().setVisible(False)
        self.floor_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.floor_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.floor_table.itemSelectionChanged.connect(self.floor_selection_changed)
        self.floor_table.itemChanged.connect(self.floor_table_changed)

        self.wall_first_combo = QComboBox()
        self.wall_first_combo.setEditable(True)
        self.wall_second_combo = QComboBox()
        self.wall_second_combo.setEditable(True)
        self.wall_fraction_edit = QLineEdit("1")
        self.wall_editor_group = self.build_wall_editor_group()

        self.floor_top_combo = QComboBox()
        self.floor_top_combo.setEditable(True)
        self.floor_bottom_combo = QComboBox()
        self.floor_bottom_combo.setEditable(True)
        self.floor_editor_group = self.build_floor_editor_group()

        self.build_layout()
        self.connect_editor_signals()
        self.load_demo_data()
        self.rebuild_wall_table()
        self.rebuild_floor_table()
        self.select_wall_connection(0)
        self.select_floor_connection(0)

    def load_case(self, case: CfastCase):
        self.wall_connections = copy.deepcopy(case.wall_surface_connections)
        self.ceiling_floor_connections = copy.deepcopy(
            case.ceiling_floor_surface_connections
        )
        self.update_compartment_choices([compartment.id for compartment in case.compartments])
        self.rebuild_wall_table()
        self.rebuild_floor_table()
        self.select_wall_connection(0 if self.wall_connections else -1)
        self.select_floor_connection(0 if self.ceiling_floor_connections else -1)

    def build_layout(self):
        layout = QHBoxLayout()
        layout.addWidget(self.build_wall_panel(), 1)
        layout.addWidget(self.build_floor_panel(), 1)
        self.setLayout(layout)

    def build_wall_panel(self):
        panel = QGroupBox("Wall Connections")
        layout = QVBoxLayout()
        layout.addWidget(self.wall_table, 3)

        button_layout = QHBoxLayout()
        button_layout.addStretch(1)
        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        remove_button = QPushButton("Remove")
        add_button.clicked.connect(self.add_wall_connection)
        duplicate_button.clicked.connect(self.duplicate_wall_connection)
        remove_button.clicked.connect(self.remove_wall_connection)
        button_layout.addWidget(add_button)
        button_layout.addWidget(duplicate_button)
        button_layout.addStretch(1)
        button_layout.addWidget(remove_button)
        button_layout.addStretch(1)

        layout.addLayout(button_layout)
        layout.addWidget(self.wall_editor_group, 2)
        panel.setLayout(layout)
        return panel

    def build_floor_panel(self):
        panel = QGroupBox("Ceiling/Floor Connections")
        layout = QVBoxLayout()
        layout.addWidget(self.floor_table, 3)

        button_layout = QHBoxLayout()
        button_layout.addStretch(1)
        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        remove_button = QPushButton("Remove")
        add_button.clicked.connect(self.add_floor_connection)
        duplicate_button.clicked.connect(self.duplicate_floor_connection)
        remove_button.clicked.connect(self.remove_floor_connection)
        button_layout.addWidget(add_button)
        button_layout.addWidget(duplicate_button)
        button_layout.addStretch(1)
        button_layout.addWidget(remove_button)
        button_layout.addStretch(1)

        layout.addLayout(button_layout)
        layout.addWidget(self.floor_editor_group, 2)
        panel.setLayout(layout)
        return panel

    def build_wall_editor_group(self):
        group = QGroupBox("Heat Transfer Connection")
        layout = QGridLayout()
        layout.addWidget(QLabel("First Compartment:"), 0, 0)
        layout.addWidget(self.wall_first_combo, 0, 1)
        layout.addWidget(QLabel("Second Compartment:"), 1, 0)
        layout.addWidget(self.wall_second_combo, 1, 1)
        layout.addWidget(QLabel("Fraction:"), 2, 0)
        layout.addWidget(self.wall_fraction_edit, 2, 1)
        group.setLayout(layout)
        return group

    def build_floor_editor_group(self):
        group = QGroupBox("Connection")
        layout = QGridLayout()
        layout.addWidget(QLabel("Top Compartment:"), 0, 0)
        layout.addWidget(self.floor_top_combo, 0, 1)
        layout.addWidget(QLabel("Bottom Compartment:"), 1, 0)
        layout.addWidget(self.floor_bottom_combo, 1, 1)
        group.setLayout(layout)
        return group

    def connect_editor_signals(self):
        self.wall_first_combo.currentTextChanged.connect(self.wall_editor_changed)
        self.wall_second_combo.currentTextChanged.connect(self.wall_editor_changed)
        self.wall_fraction_edit.textChanged.connect(self.wall_editor_changed)

        self.floor_top_combo.currentTextChanged.connect(self.floor_editor_changed)
        self.floor_bottom_combo.currentTextChanged.connect(self.floor_editor_changed)

    def load_demo_data(self):
        self.update_compartment_choices()
        self.ceiling_floor_connections = [
            CeilingFloorSurfaceConnection(
                top_comp_id="Comp 3",
                bottom_comp_id="Comp 2",
            )
        ]

    def update_compartment_choices(self, choices: list[str] | None = None):
        choices = choices or ["Comp 1", "Comp 2", "Comp 3"]

        for combo in [
            self.wall_first_combo,
            self.wall_second_combo,
            self.floor_top_combo,
            self.floor_bottom_combo,
        ]:
            current = combo.currentText()
            combo.blockSignals(True)
            combo.clear()
            combo.addItems(choices)
            if current:
                set_combo_text(combo, current)
            combo.blockSignals(False)

    # Wall connections
    def wall_values(self, index: int, conn: WallSurfaceConnection) -> list[str]:
        return [
            str(index + 1),
            "Wall",
            conn.first_comp_id,
            conn.second_comp_id,
            f"{conn.fraction:g}",
        ]

    def rebuild_wall_table(self):
        self.updating_wall_table = True
        self.wall_table.setRowCount(max(8, len(self.wall_connections)))
        self.wall_table.clearContents()

        for row in range(self.wall_table.rowCount()):
            if row < len(self.wall_connections):
                values = self.wall_values(row, self.wall_connections[row])
                editable = True
            else:
                values = ["", "", "", "", ""]
                editable = False

            for col, value in enumerate(values):
                self.wall_table.setItem(
                    row,
                    col,
                    make_item(value, editable=editable and col != self.WALL_COL_NUM),
                )

        self.updating_wall_table = False
        self.update_wall_title()

    def update_wall_row(self, row: int):
        if not 0 <= row < len(self.wall_connections):
            return

        self.updating_wall_table = True
        values = self.wall_values(row, self.wall_connections[row])
        for col, value in enumerate(values):
            self.wall_table.setItem(
                row,
                col,
                make_item(value, editable=col != self.WALL_COL_NUM),
            )
        self.updating_wall_table = False

    def update_wall_title(self):
        if not self.wall_connections or self.current_wall_index < 0:
            title = "Heat Transfer Connection 0"
        else:
            title = f"Heat Transfer Connection {self.current_wall_index + 1}"
        self.wall_editor_group.setTitle(title)

    def select_wall_connection(self, index: int):
        if not self.wall_connections:
            self.current_wall_index = -1
            self.load_wall_editor(-1)
            self.rebuild_wall_table()
            return

        index = max(0, min(index, len(self.wall_connections) - 1))
        self.current_wall_index = index
        self.wall_table.blockSignals(True)
        self.wall_table.selectRow(index)
        self.wall_table.blockSignals(False)
        self.load_wall_editor(index)
        self.update_wall_title()

    def load_wall_editor(self, index: int):
        self.loading_wall_editor = True
        if 0 <= index < len(self.wall_connections):
            conn = self.wall_connections[index]
            set_combo_text(self.wall_first_combo, conn.first_comp_id)
            set_combo_text(self.wall_second_combo, conn.second_comp_id)
            self.wall_fraction_edit.setText(f"{conn.fraction:g}")
        else:
            self.wall_first_combo.setCurrentIndex(-1)
            self.wall_second_combo.setCurrentIndex(-1)
            self.wall_fraction_edit.setText("1")
        self.loading_wall_editor = False

    def save_wall_editor(self):
        if not 0 <= self.current_wall_index < len(self.wall_connections):
            return

        self.wall_connections[self.current_wall_index] = WallSurfaceConnection(
            first_comp_id=self.wall_first_combo.currentText().strip(),
            second_comp_id=self.wall_second_combo.currentText().strip(),
            fraction=parse_float(
                self.wall_fraction_edit.text(),
                "Wall Connection Fraction",
                default=1.0,
            ),
        )

    def wall_selection_changed(self):
        if self.loading_wall_editor or self.updating_wall_table:
            return

        indexes = self.wall_table.selectionModel().selectedRows()
        if not indexes:
            return

        row = indexes[0].row()
        if not 0 <= row < len(self.wall_connections):
            return

        try:
            self.save_wall_editor()
            self.update_wall_row(self.current_wall_index)
        except ValueError:
            pass

        self.current_wall_index = row
        self.load_wall_editor(row)
        self.update_wall_title()

    def wall_table_changed(self, item: QTableWidgetItem):
        if self.updating_wall_table or self.loading_wall_editor:
            return

        row = item.row()
        if not 0 <= row < len(self.wall_connections):
            return

        try:
            conn = self.wall_connections[row]
            values = [
                self.wall_table.item(row, col).text().strip()
                if self.wall_table.item(row, col) is not None else ""
                for col in range(self.wall_table.columnCount())
            ]
            conn.connection_type = "WALL"
            conn.first_comp_id = values[self.WALL_COL_FIRST] or conn.first_comp_id
            conn.second_comp_id = values[self.WALL_COL_SECOND] or conn.second_comp_id
            conn.fraction = parse_float(
                values[self.WALL_COL_FRACTION],
                "Wall Connection Fraction",
                default=conn.fraction,
            )
        except ValueError:
            return

        if row == self.current_wall_index:
            self.load_wall_editor(row)

    def wall_editor_changed(self):
        if self.loading_wall_editor:
            return

        try:
            self.save_wall_editor()
            self.update_wall_row(self.current_wall_index)
        except ValueError:
            pass

    def add_wall_connection(self):
        index = len(self.wall_connections) + 1
        self.wall_connections.append(
            WallSurfaceConnection(
                first_comp_id="Comp 1",
                second_comp_id="Comp 2",
                fraction=1.0,
            )
        )
        self.rebuild_wall_table()
        self.select_wall_connection(index - 1)

    def duplicate_wall_connection(self):
        if not 0 <= self.current_wall_index < len(self.wall_connections):
            return

        self.wall_connections.insert(
            self.current_wall_index + 1,
            copy.deepcopy(self.wall_connections[self.current_wall_index]),
        )
        self.rebuild_wall_table()
        self.select_wall_connection(self.current_wall_index + 1)

    def remove_wall_connection(self):
        if not 0 <= self.current_wall_index < len(self.wall_connections):
            return

        index = self.current_wall_index
        self.wall_connections.pop(index)
        self.rebuild_wall_table()
        if self.wall_connections:
            self.select_wall_connection(min(index, len(self.wall_connections) - 1))
        else:
            self.select_wall_connection(-1)

    # Ceiling/floor surface connections
    def floor_values(
        self,
        index: int,
        conn: CeilingFloorSurfaceConnection,
    ) -> list[str]:
        return [
            str(index + 1),
            "Vertical",
            conn.top_comp_id,
            conn.bottom_comp_id,
        ]

    def rebuild_floor_table(self):
        self.updating_floor_table = True
        self.floor_table.setRowCount(max(8, len(self.ceiling_floor_connections)))
        self.floor_table.clearContents()

        for row in range(self.floor_table.rowCount()):
            if row < len(self.ceiling_floor_connections):
                values = self.floor_values(row, self.ceiling_floor_connections[row])
                editable = True
            else:
                values = ["", "", "", ""]
                editable = False

            for col, value in enumerate(values):
                self.floor_table.setItem(
                    row,
                    col,
                    make_item(value, editable=editable and col != self.FLOOR_COL_NUM),
                )

        self.updating_floor_table = False
        self.update_floor_title()

    def update_floor_row(self, row: int):
        if not 0 <= row < len(self.ceiling_floor_connections):
            return

        self.updating_floor_table = True
        values = self.floor_values(row, self.ceiling_floor_connections[row])
        for col, value in enumerate(values):
            self.floor_table.setItem(
                row,
                col,
                make_item(value, editable=col != self.FLOOR_COL_NUM),
            )
        self.updating_floor_table = False

    def update_floor_title(self):
        if not self.ceiling_floor_connections or self.current_floor_index < 0:
            title = "Connection 0 (of 0)"
        else:
            title = (
                f"Connection {self.current_floor_index + 1} "
                f"(of {len(self.ceiling_floor_connections)})"
            )
        self.floor_editor_group.setTitle(title)

    def select_floor_connection(self, index: int):
        if not self.ceiling_floor_connections:
            self.current_floor_index = -1
            self.load_floor_editor(-1)
            self.rebuild_floor_table()
            return

        index = max(0, min(index, len(self.ceiling_floor_connections) - 1))
        self.current_floor_index = index
        self.floor_table.blockSignals(True)
        self.floor_table.selectRow(index)
        self.floor_table.blockSignals(False)
        self.load_floor_editor(index)
        self.update_floor_title()

    def load_floor_editor(self, index: int):
        self.loading_floor_editor = True
        if 0 <= index < len(self.ceiling_floor_connections):
            conn = self.ceiling_floor_connections[index]
            set_combo_text(self.floor_top_combo, conn.top_comp_id)
            set_combo_text(self.floor_bottom_combo, conn.bottom_comp_id)
        else:
            self.floor_top_combo.setCurrentIndex(-1)
            self.floor_bottom_combo.setCurrentIndex(-1)
        self.loading_floor_editor = False

    def save_floor_editor(self):
        if not 0 <= self.current_floor_index < len(self.ceiling_floor_connections):
            return

        self.ceiling_floor_connections[
            self.current_floor_index
        ] = CeilingFloorSurfaceConnection(
            top_comp_id=self.floor_top_combo.currentText().strip(),
            bottom_comp_id=self.floor_bottom_combo.currentText().strip(),
        )

    def floor_selection_changed(self):
        if self.loading_floor_editor or self.updating_floor_table:
            return

        indexes = self.floor_table.selectionModel().selectedRows()
        if not indexes:
            return

        row = indexes[0].row()
        if not 0 <= row < len(self.ceiling_floor_connections):
            return

        try:
            self.save_floor_editor()
            self.update_floor_row(self.current_floor_index)
        except ValueError:
            pass

        self.current_floor_index = row
        self.load_floor_editor(row)
        self.update_floor_title()

    def floor_table_changed(self, item: QTableWidgetItem):
        if self.updating_floor_table or self.loading_floor_editor:
            return

        row = item.row()
        if not 0 <= row < len(self.ceiling_floor_connections):
            return

        conn = self.ceiling_floor_connections[row]
        values = [
            self.floor_table.item(row, col).text().strip()
            if self.floor_table.item(row, col) is not None else ""
            for col in range(self.floor_table.columnCount())
        ]
        conn.connection_type = "FLOOR"
        conn.top_comp_id = values[self.FLOOR_COL_TOP] or conn.top_comp_id
        conn.bottom_comp_id = values[self.FLOOR_COL_BOTTOM] or conn.bottom_comp_id

        if row == self.current_floor_index:
            self.load_floor_editor(row)

    def floor_editor_changed(self):
        if self.loading_floor_editor:
            return

        try:
            self.save_floor_editor()
            self.update_floor_row(self.current_floor_index)
        except ValueError:
            pass

    def add_floor_connection(self):
        index = len(self.ceiling_floor_connections) + 1
        self.ceiling_floor_connections.append(
            CeilingFloorSurfaceConnection(
                top_comp_id="Comp 3",
                bottom_comp_id="Comp 2",
            )
        )
        self.rebuild_floor_table()
        self.select_floor_connection(index - 1)

    def duplicate_floor_connection(self):
        if not 0 <= self.current_floor_index < len(self.ceiling_floor_connections):
            return

        self.ceiling_floor_connections.insert(
            self.current_floor_index + 1,
            copy.deepcopy(self.ceiling_floor_connections[self.current_floor_index]),
        )
        self.rebuild_floor_table()
        self.select_floor_connection(self.current_floor_index + 1)

    def remove_floor_connection(self):
        if not 0 <= self.current_floor_index < len(self.ceiling_floor_connections):
            return

        index = self.current_floor_index
        self.ceiling_floor_connections.pop(index)
        self.rebuild_floor_table()
        if self.ceiling_floor_connections:
            self.select_floor_connection(
                min(index, len(self.ceiling_floor_connections) - 1)
            )
        else:
            self.select_floor_connection(-1)

    def add_to_case(self, case: CfastCase):
        self.save_wall_editor()
        self.save_floor_editor()
        case.wall_surface_connections = list(self.wall_connections)
        case.ceiling_floor_surface_connections = list(self.ceiling_floor_connections)

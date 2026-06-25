import re

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFrame,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, OutputVisualization


AXIS_LABELS = {
    "X": "X-Axis",
    "Y": "Y-Axis",
    "Z": "Z-Axis",
}


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


def make_item(text: str, editable: bool = True) -> QTableWidgetItem:
    item = QTableWidgetItem(text)

    if not editable:
        item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)

    return item


def axis_code(text: str) -> str:
    value = text.strip().upper()

    if value.startswith("X"):
        return "X"
    if value.startswith("Y"):
        return "Y"
    if value.startswith("Z"):
        return "Z"

    raise ValueError(f"Axis must be X-Axis, Y-Axis, or Z-Axis: {text!r}")


class OutputTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.updating = False

        self.visual_table = QTableWidget(10, 5)
        self.visual_table.setHorizontalHeaderLabels(
            ["Num", "Type", "Compartment", "Axis", "Value"]
        )
        self.visual_table.verticalHeader().setVisible(False)

        self.resolution_table = QTableWidget(10, 5)
        self.resolution_table.setHorizontalHeaderLabels(
            ["Compartment", "Num", "Width", "Depth", "Height"]
        )
        self.resolution_table.verticalHeader().setVisible(False)

        self.visualization_type_combo = QComboBox()
        self.visualization_type_combo.addItems(["2-D", "3-D"])

        self.visual_compartment_edit = QLineEdit("All")
        self.visual_position_edit = QLineEdit("2.5 m")

        self.axis_combo = QComboBox()
        self.axis_combo.addItems(["X-axis (Width)", "Y-axis (Depth)", "Z-axis (Height)"])

        self.width_grid_edit = QLineEdit("50")
        self.depth_grid_edit = QLineEdit("50")
        self.height_grid_edit = QLineEdit("50")

        self.validation_checkbox = QCheckBox("Validation Output")
        self.debug_checkbox = QCheckBox("Debug Output")
        self.show_cfast_window_checkbox = QCheckBox("Show CFAST Window")

        self.build_layout()
        self.load_demo_data()
        self.connect_signals()
        self.refresh_visual_numbers()
        self.refresh_resolution_numbers()
        self.select_first_rows()

    def build_layout(self):
        main_layout = QHBoxLayout()
        left_layout = QVBoxLayout()
        right_layout = QVBoxLayout()

        left_layout.addWidget(self.build_visualizations_group(), 1)
        left_layout.addWidget(self.build_resolution_group(), 1)

        right_layout.addSpacing(110)
        right_layout.addWidget(self.validation_checkbox)
        right_layout.addSpacing(25)
        right_layout.addWidget(self.debug_checkbox)
        right_layout.addSpacing(25)
        right_layout.addWidget(self.show_cfast_window_checkbox)
        right_layout.addStretch(1)

        main_layout.addLayout(left_layout, 1)
        main_layout.addSpacing(35)
        main_layout.addLayout(right_layout)
        self.setLayout(main_layout)

    def build_visualizations_group(self):
        group = QGroupBox("Visualizations")
        group_layout = QGridLayout()

        self.visual_table.setMinimumHeight(230)
        self.visual_table.setColumnWidth(0, 55)
        self.visual_table.setColumnWidth(1, 100)
        self.visual_table.setColumnWidth(2, 160)
        self.visual_table.setColumnWidth(3, 100)
        self.visual_table.setColumnWidth(4, 100)

        group_layout.addWidget(self.visual_table, 0, 0, 1, 2)

        button_layout = QHBoxLayout()
        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        remove_button = QPushButton("Remove")
        defaults_button = QPushButton("Add Defaults")

        add_button.clicked.connect(self.add_visualization)
        duplicate_button.clicked.connect(self.duplicate_visualization)
        remove_button.clicked.connect(self.remove_visualization)
        defaults_button.clicked.connect(self.add_default_visualizations)

        button_layout.addWidget(add_button)
        button_layout.addWidget(duplicate_button)
        button_layout.addWidget(remove_button)
        button_layout.addWidget(defaults_button)
        button_layout.addStretch(1)

        group_layout.addLayout(button_layout, 1, 0, 1, 1)
        group_layout.addLayout(self.build_visual_editor(), 0, 2, 2, 1)

        group.setLayout(group_layout)
        return group

    def build_visual_editor(self):
        layout = QGridLayout()
        layout.addWidget(QLabel("Visualization Type:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.visualization_type_combo, 0, 1)
        layout.addWidget(QLabel("Compartment:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.visual_compartment_edit, 1, 1)
        layout.addWidget(QLabel("Position:"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.visual_position_edit, 2, 1)
        layout.addWidget(QLabel("Axis:"), 3, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.axis_combo, 3, 1)
        return layout

    def build_resolution_group(self):
        group = QGroupBox("Resolution")
        group_layout = QGridLayout()

        self.resolution_table.setMinimumHeight(220)
        self.resolution_table.setColumnWidth(0, 160)
        self.resolution_table.setColumnWidth(1, 55)
        self.resolution_table.setColumnWidth(2, 100)
        self.resolution_table.setColumnWidth(3, 100)
        self.resolution_table.setColumnWidth(4, 100)

        group_layout.addWidget(self.resolution_table, 0, 0)
        group_layout.addLayout(self.build_resolution_editor(), 0, 1)
        group.setLayout(group_layout)
        return group

    def build_resolution_editor(self):
        layout = QGridLayout()
        layout.addWidget(QLabel("Width (X) Grid:"), 0, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.width_grid_edit, 0, 1)
        layout.addWidget(QLabel("Depth (Y) Grid:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.depth_grid_edit, 1, 1)
        layout.addWidget(QLabel("Height (Z) Grid:"), 2, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.height_grid_edit, 2, 1)
        return layout

    def connect_signals(self):
        self.visual_table.currentCellChanged.connect(self.visual_row_changed)
        self.visual_table.cellChanged.connect(self.visual_cell_changed)
        self.resolution_table.currentCellChanged.connect(self.resolution_row_changed)
        self.resolution_table.cellChanged.connect(self.resolution_cell_changed)

        self.visualization_type_combo.currentTextChanged.connect(self.editor_to_visual_row)
        self.visual_compartment_edit.editingFinished.connect(self.editor_to_visual_row)
        self.visual_position_edit.editingFinished.connect(self.editor_to_visual_row)
        self.axis_combo.currentTextChanged.connect(self.editor_to_visual_row)

        self.width_grid_edit.editingFinished.connect(self.editor_to_resolution_row)
        self.depth_grid_edit.editingFinished.connect(self.editor_to_resolution_row)
        self.height_grid_edit.editingFinished.connect(self.editor_to_resolution_row)

    def load_demo_data(self):
        self.clear_table(self.visual_table)
        self.clear_table(self.resolution_table)

        visuals = [
            ["", "2-D", "All", "X-Axis", "2.5 m"],
            ["", "2-D", "All", "Y-Axis", "2.5 m"],
            ["", "2-D", "All", "Z-Axis", "2.95 m"],
        ]
        resolutions = [
            ["Comp 1", "", "50", "50", "50"],
            ["Comp 2", "", "50", "50", "50"],
            ["Comp 3", "", "50", "50", "50"],
        ]

        self.updating = True
        for row, values in enumerate(visuals):
            for col, value in enumerate(values):
                self.visual_table.setItem(row, col, make_item(value, editable=(col != 0)))

        for row, values in enumerate(resolutions):
            for col, value in enumerate(values):
                self.resolution_table.setItem(row, col, make_item(value, editable=(col != 1)))

        self.updating = False

    def select_first_rows(self):
        if self.visual_table.rowCount() > 0:
            self.visual_table.setCurrentCell(0, 1)
        if self.resolution_table.rowCount() > 0:
            self.resolution_table.setCurrentCell(0, 2)

    def clear_table(self, table: QTableWidget):
        table.blockSignals(True)
        table.clearContents()
        table.blockSignals(False)

    def cell_text(self, table: QTableWidget, row: int, col: int) -> str:
        item = table.item(row, col)
        return "" if item is None else item.text().strip()

    def set_cell_text(self, table: QTableWidget, row: int, col: int, text: str, editable: bool = True):
        item = table.item(row, col)
        if item is None:
            table.setItem(row, col, make_item(text, editable=editable))
        else:
            item.setText(text)
            if not editable:
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)

    def first_empty_row(self, table: QTableWidget) -> int:
        for row in range(table.rowCount()):
            values = [self.cell_text(table, row, col) for col in range(table.columnCount())]
            if not any(values):
                return row

        table.insertRow(table.rowCount())
        return table.rowCount() - 1

    def add_visualization(self):
        row = self.first_empty_row(self.visual_table)
        self.updating = True
        values = ["", "2-D", "All", "X-Axis", "2.5 m"]
        for col, value in enumerate(values):
            self.set_cell_text(self.visual_table, row, col, value, editable=(col != 0))
        self.updating = False
        self.refresh_visual_numbers()
        self.visual_table.setCurrentCell(row, 1)

    def duplicate_visualization(self):
        source_row = self.visual_table.currentRow()
        if source_row < 0:
            return

        values = [self.cell_text(self.visual_table, source_row, col) for col in range(1, 5)]
        if not any(values):
            return

        row = self.first_empty_row(self.visual_table)
        self.updating = True
        self.set_cell_text(self.visual_table, row, 0, "", editable=False)
        for col, value in enumerate(values, start=1):
            self.set_cell_text(self.visual_table, row, col, value)
        self.updating = False
        self.refresh_visual_numbers()
        self.visual_table.setCurrentCell(row, 1)

    def remove_visualization(self):
        row = self.visual_table.currentRow()
        if row < 0:
            return

        self.visual_table.removeRow(row)
        if self.visual_table.rowCount() < 10:
            self.visual_table.insertRow(self.visual_table.rowCount())
        self.refresh_visual_numbers()
        self.visual_table.setCurrentCell(min(row, self.visual_table.rowCount() - 1), 1)

    def add_default_visualizations(self):
        self.load_demo_data()
        self.refresh_visual_numbers()
        self.refresh_resolution_numbers()
        self.select_first_rows()

    def refresh_visual_numbers(self):
        self.updating = True
        number = 1
        for row in range(self.visual_table.rowCount()):
            values = [self.cell_text(self.visual_table, row, col) for col in range(1, 5)]
            if any(values):
                self.set_cell_text(self.visual_table, row, 0, str(number), editable=False)
                number += 1
            else:
                self.set_cell_text(self.visual_table, row, 0, "", editable=False)
        self.updating = False

    def refresh_resolution_numbers(self):
        self.updating = True
        number = 1
        for row in range(self.resolution_table.rowCount()):
            comp_id = self.cell_text(self.resolution_table, row, 0)
            if comp_id:
                self.set_cell_text(self.resolution_table, row, 1, str(number), editable=False)
                number += 1
            else:
                self.set_cell_text(self.resolution_table, row, 1, "", editable=False)
        self.updating = False

    def visual_row_changed(self, current_row, current_col, previous_row, previous_col):
        self.visual_row_to_editor(current_row)

    def visual_cell_changed(self, row, col):
        if self.updating:
            return
        self.refresh_visual_numbers()
        if row == self.visual_table.currentRow():
            self.visual_row_to_editor(row)

    def visual_row_to_editor(self, row: int):
        if row < 0:
            return

        values = [self.cell_text(self.visual_table, row, col) for col in range(1, 5)]
        if not any(values):
            return

        self.updating = True
        self.visualization_type_combo.setCurrentText(values[0] or "2-D")
        self.visual_compartment_edit.setText(values[1] or "All")
        axis = axis_code(values[2] or "X")
        if axis == "X":
            self.axis_combo.setCurrentText("X-axis (Width)")
        elif axis == "Y":
            self.axis_combo.setCurrentText("Y-axis (Depth)")
        else:
            self.axis_combo.setCurrentText("Z-axis (Height)")
        self.visual_position_edit.setText(values[3] or "0")
        self.updating = False

    def editor_to_visual_row(self):
        if self.updating:
            return

        row = self.visual_table.currentRow()
        if row < 0:
            return

        self.updating = True
        self.set_cell_text(self.visual_table, row, 1, self.visualization_type_combo.currentText())
        self.set_cell_text(self.visual_table, row, 2, self.visual_compartment_edit.text())
        self.set_cell_text(self.visual_table, row, 3, AXIS_LABELS[axis_code(self.axis_combo.currentText())])
        self.set_cell_text(self.visual_table, row, 4, self.visual_position_edit.text())
        self.updating = False
        self.refresh_visual_numbers()

    def resolution_row_changed(self, current_row, current_col, previous_row, previous_col):
        self.resolution_row_to_editor(current_row)

    def resolution_cell_changed(self, row, col):
        if self.updating:
            return
        self.refresh_resolution_numbers()
        if row == self.resolution_table.currentRow():
            self.resolution_row_to_editor(row)

    def resolution_row_to_editor(self, row: int):
        if row < 0:
            return

        comp_id = self.cell_text(self.resolution_table, row, 0)
        if not comp_id:
            return

        self.updating = True
        self.width_grid_edit.setText(self.cell_text(self.resolution_table, row, 2) or "50")
        self.depth_grid_edit.setText(self.cell_text(self.resolution_table, row, 3) or "50")
        self.height_grid_edit.setText(self.cell_text(self.resolution_table, row, 4) or "50")
        self.updating = False

    def editor_to_resolution_row(self):
        if self.updating:
            return

        row = self.resolution_table.currentRow()
        if row < 0:
            return

        self.updating = True
        self.set_cell_text(self.resolution_table, row, 2, self.width_grid_edit.text())
        self.set_cell_text(self.resolution_table, row, 3, self.depth_grid_edit.text())
        self.set_cell_text(self.resolution_table, row, 4, self.height_grid_edit.text())
        self.updating = False

    def add_to_case(self, case: CfastCase):
        visualizations: list[OutputVisualization] = []

        for row in range(self.visual_table.rowCount()):
            values = [self.cell_text(self.visual_table, row, col) for col in range(1, 5)]
            if not any(values):
                continue

            vis_type = values[0] or "2-D"
            comp_id = values[1] or "All"
            axis = axis_code(values[2] or "X")
            value = parse_float(values[3] or "0", "Visualization Value")

            visualizations.append(
                OutputVisualization(
                    visualization_type=vis_type,
                    comp_id=comp_id,
                    axis=axis,
                    value=value,
                )
            )

        case.output_visualizations = visualizations
        case.validation_output = self.validation_checkbox.isChecked()
        case.debug_output = self.debug_checkbox.isChecked()
        case.show_cfast_window = self.show_cfast_window_checkbox.isChecked()

        grid_by_compartment: dict[str, tuple[int, int, int]] = {}
        for row in range(self.resolution_table.rowCount()):
            comp_id = self.cell_text(self.resolution_table, row, 0)
            if not comp_id:
                continue

            grid_by_compartment[comp_id] = (
                parse_int(self.cell_text(self.resolution_table, row, 2) or "50", "Width Grid"),
                parse_int(self.cell_text(self.resolution_table, row, 3) or "50", "Depth Grid"),
                parse_int(self.cell_text(self.resolution_table, row, 4) or "50", "Height Grid"),
            )

        for compartment in case.compartments:
            if compartment.id in grid_by_compartment:
                compartment.grid = grid_by_compartment[compartment.id]

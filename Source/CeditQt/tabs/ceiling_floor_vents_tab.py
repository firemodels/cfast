import re

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QCheckBox,
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

from cfast_case import CeilingFloorVent, CfastCase


def parse_float(text: str, field_name: str, default: float | None = None) -> float:
    text = text.strip()

    if not text and default is not None:
        return default

    match = re.search(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?", text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


def table_item_text(table: QTableWidget, row: int, col: int) -> str:
    item = table.item(row, col)

    if item is None:
        return ""

    return item.text().strip()


class CeilingFloorVentsTab(QWidget):
    SUMMARY_HEADERS = [
        "Num",
        "ID",
        "First Compartment",
        "Second Compartment",
        "Type",
        "Shape",
        "Area",
        "Initial Open",
        "X Offset",
        "Y Offset",
    ]

    def __init__(self, parent=None):
        super().__init__(parent)

        self.current_row = -1
        self.loading_editor = False
        self.schedules: dict[int, tuple[list[float], list[float]]] = {}

        self.summary_table = QTableWidget(8, len(self.SUMMARY_HEADERS))
        self.summary_table.setHorizontalHeaderLabels(self.SUMMARY_HEADERS)
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        move_up_button = QPushButton("Move Up")
        move_down_button = QPushButton("Move Down")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_vent)
        duplicate_button.clicked.connect(self.duplicate_vent)
        move_up_button.clicked.connect(self.move_selected_up)
        move_down_button.clicked.connect(self.move_selected_down)
        remove_button.clicked.connect(self.remove_selected)

        self.id_edit = QLineEdit()
        self.first_comp_edit = QLineEdit()
        self.second_comp_edit = QLineEdit()
        self.type_combo = QComboBox()
        self.type_combo.addItems(["CEILING", "FLOOR"])
        self.shape_combo = QComboBox()
        self.shape_combo.addItems(["ROUND", "SQUARE"])
        self.area_edit = QLineEdit()
        self.initial_open_edit = QLineEdit()
        self.offset_x_edit = QLineEdit()
        self.offset_y_edit = QLineEdit()
        self.fyi_edit = QLineEdit()

        self.criterion_combo = QComboBox()
        self.criterion_combo.addItems(["TIME"])

        self.use_time_fraction_checkbox = QCheckBox("Use Time Opening Fraction")
        self.fraction_table = QTableWidget(5, 2)
        self.fraction_table.setHorizontalHeaderLabels(["Time", "Fraction"])
        self.fraction_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )

        self.build_layout(
            add_button,
            duplicate_button,
            move_up_button,
            move_down_button,
            remove_button,
        )
        self.connect_editor_signals()
        self.load_demo_data()

        self.summary_table.currentCellChanged.connect(self.summary_selection_changed)
        self.summary_table.itemChanged.connect(self.summary_item_changed)
        self.summary_table.setCurrentCell(0, 0)

    def load_case(self, case: CfastCase):
        vents = list(case.ceiling_floor_vents)

        self.summary_table.blockSignals(True)
        self.summary_table.clearContents()
        self.summary_table.setRowCount(max(8, len(vents)))
        self.schedules = {}

        for row, vent in enumerate(vents):
            self.set_summary_row(
                row,
                [
                    str(row + 1),
                    vent.id,
                    vent.first_comp_id,
                    vent.second_comp_id,
                    vent.vent_type,
                    vent.shape,
                    format_number(vent.area),
                    format_number(vent.initial_open),
                    format_number(vent.offset_x),
                    format_number(vent.offset_y),
                ],
            )
            self.schedules[row] = (list(vent.t_values), list(vent.f_values))

        self.summary_table.blockSignals(False)
        self.renumber_rows()

        if vents:
            self.summary_table.blockSignals(True)
            self.summary_table.setCurrentCell(0, 0)
            self.summary_table.blockSignals(False)
            self.current_row = 0
            self.load_editor_from_row(0)
        else:
            self.current_row = -1
            self.clear_editor()

    def build_layout(self, add_button, duplicate_button, move_up_button, move_down_button, remove_button):
        main_layout = QVBoxLayout()
        main_layout.addWidget(self.summary_table, 2)

        button_row = QHBoxLayout()
        button_row.addStretch(1)
        button_row.addWidget(add_button)
        button_row.addWidget(duplicate_button)
        button_row.addWidget(move_up_button)
        button_row.addWidget(move_down_button)
        button_row.addStretch(1)
        button_row.addWidget(remove_button)
        button_row.addStretch(1)
        main_layout.addLayout(button_row)

        self.detail_group = QGroupBox("Vent 1 (of 1) Geometry")
        detail_layout = QVBoxLayout()

        id_layout = QHBoxLayout()
        id_layout.addStretch(1)
        id_layout.addWidget(QLabel("ID:"))
        id_layout.addWidget(self.id_edit, 2)
        id_layout.addStretch(1)
        detail_layout.addLayout(id_layout)

        lower_layout = QHBoxLayout()
        lower_layout.addWidget(self.build_geometry_group(), 2)
        lower_layout.addWidget(self.build_opening_group(), 1)
        detail_layout.addLayout(lower_layout)

        self.detail_group.setLayout(detail_layout)
        main_layout.addWidget(self.detail_group, 3)
        self.setLayout(main_layout)

    def build_geometry_group(self):
        group = QGroupBox("Geometry")
        layout = QGridLayout()

        rows = [
            ("First Compartment:", self.first_comp_edit),
            ("Second Compartment:", self.second_comp_edit),
            ("Type:", self.type_combo),
            ("Shape:", self.shape_combo),
            ("Area:", self.area_edit),
            ("Initial Open:", self.initial_open_edit),
            ("X Offset:", self.offset_x_edit),
            ("Y Offset:", self.offset_y_edit),
            ("FYI:", self.fyi_edit),
        ]

        for row, (label, widget) in enumerate(rows):
            layout.addWidget(QLabel(label), row, 0, alignment=Qt.AlignmentFlag.AlignRight)
            layout.addWidget(widget, row, 1)

        group.setLayout(layout)
        return group

    def build_opening_group(self):
        group = QGroupBox("Opening Fraction")
        layout = QVBoxLayout()

        criterion_layout = QHBoxLayout()
        criterion_layout.addWidget(QLabel("Open/Close Criterion:"))
        criterion_layout.addWidget(self.criterion_combo)
        layout.addLayout(criterion_layout)

        layout.addWidget(self.use_time_fraction_checkbox)
        layout.addWidget(self.fraction_table)

        group.setLayout(layout)
        return group

    def connect_editor_signals(self):
        for widget in [
            self.id_edit,
            self.first_comp_edit,
            self.second_comp_edit,
            self.area_edit,
            self.initial_open_edit,
            self.offset_x_edit,
            self.offset_y_edit,
            self.fyi_edit,
        ]:
            widget.editingFinished.connect(self.save_current_editor)

        self.type_combo.currentTextChanged.connect(self.save_current_editor)
        self.shape_combo.currentTextChanged.connect(self.save_current_editor)
        self.use_time_fraction_checkbox.stateChanged.connect(self.save_current_editor)
        self.fraction_table.itemChanged.connect(self.save_current_editor)

    def load_demo_data(self):
        self.summary_table.blockSignals(True)
        self.set_summary_row(
            0,
            [
                "1",
                "CFVent_1",
                "Comp 2",
                "Comp 1",
                "CEILING",
                "ROUND",
                "1",
                "1",
                "0",
                "0",
            ],
        )
        self.summary_table.blockSignals(False)

    def set_summary_row(self, row: int, values: list[str]):
        for col, value in enumerate(values):
            self.summary_table.setItem(row, col, QTableWidgetItem(value))

    def summary_selection_changed(self, current_row, current_col, previous_row, previous_col):
        if previous_row >= 0:
            self.save_editor_for_row(previous_row)

        self.current_row = current_row
        self.load_editor_from_row(current_row)

    def summary_item_changed(self, item):
        if item.column() == 1 and item.row() == self.current_row:
            self.id_edit.setText(item.text())

        self.renumber_rows()

    def renumber_rows(self):
        self.summary_table.blockSignals(True)

        count = 0
        for row in range(self.summary_table.rowCount()):
            if self.row_has_data(row):
                count += 1
                self.summary_table.setItem(row, 0, QTableWidgetItem(str(count)))

        self.summary_table.blockSignals(False)

    def row_has_data(self, row: int) -> bool:
        return any(
            table_item_text(self.summary_table, row, col)
            for col in range(1, self.summary_table.columnCount())
        )

    def load_editor_from_row(self, row: int):
        if row < 0 or row >= self.summary_table.rowCount():
            return

        self.loading_editor = True
        self.id_edit.setText(table_item_text(self.summary_table, row, 1))
        self.first_comp_edit.setText(table_item_text(self.summary_table, row, 2))
        self.second_comp_edit.setText(table_item_text(self.summary_table, row, 3))
        self.type_combo.setCurrentText(table_item_text(self.summary_table, row, 4) or "CEILING")
        self.shape_combo.setCurrentText(table_item_text(self.summary_table, row, 5) or "ROUND")
        self.area_edit.setText(table_item_text(self.summary_table, row, 6))
        self.initial_open_edit.setText(table_item_text(self.summary_table, row, 7))
        self.offset_x_edit.setText(table_item_text(self.summary_table, row, 8))
        self.offset_y_edit.setText(table_item_text(self.summary_table, row, 9))

        self.load_schedule_for_row(row)
        self.update_detail_title(row)
        self.loading_editor = False

    def clear_editor(self):
        self.loading_editor = True
        for widget in [
            self.id_edit,
            self.first_comp_edit,
            self.second_comp_edit,
            self.area_edit,
            self.initial_open_edit,
            self.offset_x_edit,
            self.offset_y_edit,
            self.fyi_edit,
        ]:
            widget.clear()
        self.fraction_table.clearContents()
        self.use_time_fraction_checkbox.setChecked(False)
        self.detail_group.setTitle("Vent 0 (of 0) Geometry")
        self.loading_editor = False

    def update_detail_title(self, row: int):
        total = sum(self.row_has_data(r) for r in range(self.summary_table.rowCount()))
        index = 0

        for r in range(row + 1):
            if self.row_has_data(r):
                index += 1

        index = max(index, 1)
        total = max(total, 1)
        self.detail_group.setTitle(f"Vent {index} (of {total}) Geometry")

    def save_current_editor(self):
        if self.loading_editor:
            return

        if self.current_row >= 0:
            self.save_editor_for_row(self.current_row)

    def save_editor_for_row(self, row: int):
        if row < 0 or row >= self.summary_table.rowCount():
            return

        self.summary_table.blockSignals(True)
        values = [
            table_item_text(self.summary_table, row, 0) or str(row + 1),
            self.id_edit.text().strip(),
            self.first_comp_edit.text().strip(),
            self.second_comp_edit.text().strip(),
            self.type_combo.currentText().strip(),
            self.shape_combo.currentText().strip(),
            self.area_edit.text().strip(),
            self.initial_open_edit.text().strip(),
            self.offset_x_edit.text().strip(),
            self.offset_y_edit.text().strip(),
        ]
        self.set_summary_row(row, values)
        self.summary_table.blockSignals(False)
        self.save_schedule_for_row(row)
        self.renumber_rows()

    def load_schedule_for_row(self, row: int):
        t_values, f_values = self.schedules.get(row, ([], []))

        self.fraction_table.blockSignals(True)
        self.fraction_table.clearContents()

        for idx, (time_value, fraction_value) in enumerate(zip(t_values, f_values)):
            if idx >= self.fraction_table.rowCount():
                self.fraction_table.insertRow(self.fraction_table.rowCount())

            self.fraction_table.setItem(idx, 0, QTableWidgetItem(str(time_value)))
            self.fraction_table.setItem(idx, 1, QTableWidgetItem(str(fraction_value)))

        self.use_time_fraction_checkbox.blockSignals(True)
        self.use_time_fraction_checkbox.setChecked(bool(t_values))
        self.use_time_fraction_checkbox.blockSignals(False)
        self.fraction_table.blockSignals(False)

    def save_schedule_for_row(self, row: int):
        if not self.use_time_fraction_checkbox.isChecked():
            self.schedules[row] = ([], [])
            return

        t_values: list[float] = []
        f_values: list[float] = []

        for table_row in range(self.fraction_table.rowCount()):
            t_text = table_item_text(self.fraction_table, table_row, 0)
            f_text = table_item_text(self.fraction_table, table_row, 1)

            if not t_text and not f_text:
                continue

            if not t_text or not f_text:
                continue

            t_values.append(parse_float(t_text, "Time"))
            f_values.append(parse_float(f_text, "Fraction"))

        self.schedules[row] = (t_values, f_values)

    def add_vent(self):
        self.save_current_editor()

        row = self.first_empty_row()
        if row is None:
            row = self.summary_table.rowCount()
            self.summary_table.insertRow(row)

        num = self.next_vent_number()
        self.set_summary_row(
            row,
            [
                str(num),
                f"CFVent_{num}",
                "Comp 2",
                "Comp 1",
                "CEILING",
                "ROUND",
                "1",
                "1",
                "0",
                "0",
            ],
        )
        self.summary_table.setCurrentCell(row, 0)
        self.renumber_rows()

    def duplicate_vent(self):
        row = self.current_row
        if row < 0 or not self.row_has_data(row):
            return

        self.save_current_editor()
        values = [table_item_text(self.summary_table, row, col) for col in range(self.summary_table.columnCount())]
        values[1] = f"{values[1]}_copy"

        new_row = self.first_empty_row()
        if new_row is None:
            new_row = self.summary_table.rowCount()
            self.summary_table.insertRow(new_row)

        self.set_summary_row(new_row, values)
        self.schedules[new_row] = self.schedules.get(row, ([], []))
        self.summary_table.setCurrentCell(new_row, 0)
        self.renumber_rows()

    def move_selected_up(self):
        self.move_selected(-1)

    def move_selected_down(self):
        self.move_selected(1)

    def move_selected(self, direction: int):
        row = self.current_row
        new_row = row + direction

        if row < 0 or new_row < 0 or new_row >= self.summary_table.rowCount():
            return

        self.save_current_editor()

        for col in range(self.summary_table.columnCount()):
            current = table_item_text(self.summary_table, row, col)
            other = table_item_text(self.summary_table, new_row, col)
            self.summary_table.setItem(row, col, QTableWidgetItem(other))
            self.summary_table.setItem(new_row, col, QTableWidgetItem(current))

        self.schedules[row], self.schedules[new_row] = (
            self.schedules.get(new_row, ([], [])),
            self.schedules.get(row, ([], [])),
        )

        self.summary_table.setCurrentCell(new_row, 0)
        self.renumber_rows()

    def remove_selected(self):
        row = self.current_row

        if row < 0:
            return

        self.summary_table.removeRow(row)
        self.schedules = {
            (schedule_row if schedule_row < row else schedule_row - 1): schedule
            for schedule_row, schedule in self.schedules.items()
            if schedule_row != row
        }

        if self.summary_table.rowCount() == 0:
            self.summary_table.setRowCount(1)

        self.renumber_rows()
        self.summary_table.setCurrentCell(min(row, self.summary_table.rowCount() - 1), 0)

    def first_empty_row(self):
        for row in range(self.summary_table.rowCount()):
            if not self.row_has_data(row):
                return row

        return None

    def next_vent_number(self) -> int:
        count = sum(self.row_has_data(row) for row in range(self.summary_table.rowCount()))
        return count + 1

    def add_to_case(self, case: CfastCase):
        self.save_current_editor()

        vents: list[CeilingFloorVent] = []
        ids_seen: set[str] = set()

        for row in range(self.summary_table.rowCount()):
            if not self.row_has_data(row):
                continue

            vent_id = table_item_text(self.summary_table, row, 1)
            first_comp = table_item_text(self.summary_table, row, 2)
            second_comp = table_item_text(self.summary_table, row, 3)

            if not vent_id:
                raise ValueError(f"Ceiling/Floor Vents row {row + 1}: ID is required.")

            if vent_id in ids_seen:
                raise ValueError(
                    f"Ceiling/Floor Vents row {row + 1}: duplicate ID {vent_id!r}."
                )

            ids_seen.add(vent_id)

            t_values, f_values = self.schedules.get(row, ([], []))

            vent = CeilingFloorVent(
                id=vent_id,
                first_comp_id=first_comp,
                second_comp_id=second_comp,
                vent_type=(table_item_text(self.summary_table, row, 4) or "CEILING").upper(),
                shape=(table_item_text(self.summary_table, row, 5) or "ROUND").upper(),
                area=parse_float(table_item_text(self.summary_table, row, 6), "Area"),
                initial_open=parse_float(
                    table_item_text(self.summary_table, row, 7) or "1",
                    "Initial Open",
                ),
                offset_x=parse_float(
                    table_item_text(self.summary_table, row, 8) or "0",
                    "X Offset",
                ),
                offset_y=parse_float(
                    table_item_text(self.summary_table, row, 9) or "0",
                    "Y Offset",
                ),
                t_values=t_values,
                f_values=f_values,
            )

            if vent.vent_type not in {"CEILING", "FLOOR"}:
                raise ValueError(
                    f"Ceiling/Floor Vents row {row + 1}: type must be CEILING or FLOOR."
                )

            if vent.shape not in {"ROUND", "SQUARE"}:
                raise ValueError(
                    f"Ceiling/Floor Vents row {row + 1}: shape must be ROUND or SQUARE."
                )

            if vent.area <= 0.0:
                raise ValueError(f"Ceiling/Floor Vents row {row + 1}: area must be positive.")

            if not 0.0 <= vent.initial_open <= 1.0:
                raise ValueError(
                    f"Ceiling/Floor Vents row {row + 1}: initial open must be between 0 and 1."
                )

            vents.append(vent)

        case.ceiling_floor_vents = vents


def format_number(value: float | int) -> str:
    if isinstance(value, int):
        return str(value)

    value = float(value)
    if abs(value - round(value)) < 1.0e-12:
        return str(int(round(value)))
    return f"{value:.6g}"

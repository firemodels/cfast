from __future__ import annotations

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

from cfast_case import CfastCase, WallVent
from units import LENGTH, TIME, format_number, format_value, parse_number, parse_value, unit_label


def summary_headers() -> list[str]:
    length = unit_label(LENGTH)
    return [
        "Num",
        "ID",
        "First Compartment",
        "Second Compartment",
        f"Bottom\n({length})",
        f"Height\n({length})",
        f"Width\n({length})",
        "Initial Open",
        "Face",
        f"Offset\n({length})",
    ]


class WallVentsTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.records: list[dict] = []
        self.loading = False

        self.summary_table = QTableWidget(0, 10)
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.summary_table.currentCellChanged.connect(self.current_row_changed)

        self.editor_group = QGroupBox("Vent Geometry")

        self.id_edit = QLineEdit()
        self.first_comp_combo = QComboBox()
        self.second_comp_combo = QComboBox()
        self.face_combo = QComboBox()
        self.criterion_combo = QComboBox()

        for combo in [self.first_comp_combo, self.second_comp_combo]:
            combo.setEditable(True)
            combo.addItems(["Comp 1", "Comp 2", "Comp 3", "Outside"])

        self.face_combo.addItems(["Front", "Rear", "Right", "Left"])
        self.criterion_combo.addItems(["Time"])

        self.bottom_edit = QLineEdit()
        self.height_edit = QLineEdit()
        self.width_edit = QLineEdit()
        self.initial_open_edit = QLineEdit()
        self.offset_edit = QLineEdit()

        self.schedule_table = QTableWidget(8, 2)
        self.schedule_table.setHorizontalHeaderLabels(
            [f"Time\n({unit_label(TIME)})", "Fraction"]
        )
        self.schedule_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.schedule_table.verticalHeader().setVisible(False)

        self.build_layout()
        self.connect_editor_signals()
        self.load_demo_data()

    def load_case(self, case: CfastCase):
        self.refresh_unit_labels()
        self.records = [self.record_from_vent(vent) for vent in case.wall_vents]
        self.rebuild_summary_table()

        if self.records:
            self.summary_table.setCurrentCell(0, 0)
            self.load_record_into_editor(0)
        else:
            self.clear_editor()

    def record_from_vent(self, vent: WallVent) -> dict:
        second_compartment = vent.second_comp_id
        if second_compartment.upper() == "OUTSIDE":
            second_compartment = "Outside"

        return {
            "id": vent.id,
            "first_compartment": vent.first_comp_id,
            "second_compartment": second_compartment,
            "bottom": format_value(LENGTH, vent.bottom),
            "height": format_value(LENGTH, vent.height),
            "width": format_value(LENGTH, vent.width),
            "initial_open": format_number(vent.initial_open),
            "face": vent.face.strip().capitalize(),
            "offset": format_value(LENGTH, vent.offset),
            "criterion": vent.criterion.strip().capitalize(),
            "schedule": [
                (format_value(TIME, time_value), format_number(fraction_value))
                for time_value, fraction_value in zip(vent.t_values, vent.f_values)
            ],
        }

    def build_layout(self):
        main_layout = QVBoxLayout()
        main_layout.addWidget(self.summary_table, 2)
        main_layout.addLayout(self.build_button_row())
        main_layout.addWidget(self.build_editor(), 3)
        self.setLayout(main_layout)

    def build_button_row(self):
        row = QHBoxLayout()

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        move_up_button = QPushButton("Move Up")
        move_down_button = QPushButton("Move Down")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_record)
        duplicate_button.clicked.connect(self.duplicate_record)
        move_up_button.clicked.connect(self.move_record_up)
        move_down_button.clicked.connect(self.move_record_down)
        remove_button.clicked.connect(self.remove_record)

        row.addStretch(2)
        row.addWidget(add_button)
        row.addWidget(duplicate_button)
        row.addWidget(move_up_button)
        row.addWidget(move_down_button)
        row.addStretch(1)
        row.addWidget(remove_button)
        row.addStretch(2)

        return row

    def build_editor(self):
        layout = QVBoxLayout()

        id_layout = QHBoxLayout()
        id_layout.addStretch(1)
        id_layout.addWidget(QLabel("ID:"))
        id_layout.addWidget(self.id_edit, 2)
        id_layout.addStretch(1)
        layout.addLayout(id_layout)

        lower_layout = QHBoxLayout()
        lower_layout.addWidget(self.build_geometry_group(), 2)
        lower_layout.addWidget(self.build_open_close_group(), 1)
        layout.addLayout(lower_layout)

        self.editor_group.setLayout(layout)
        return self.editor_group

    def build_geometry_group(self):
        group = QGroupBox("Geometry")
        layout = QGridLayout()

        layout.addWidget(QLabel("First Compartment"), 0, 0, 1, 2)
        layout.addWidget(self.first_comp_combo, 1, 0, 1, 2)
        layout.addWidget(QLabel("Second Compartment"), 2, 0, 1, 2)
        layout.addWidget(self.second_comp_combo, 3, 0, 1, 2)

        entries = [
            ("Bottom:", self.bottom_edit),
            ("Height:", self.height_edit),
            ("Width:", self.width_edit),
            ("Initial Open:", self.initial_open_edit),
            ("Vent Offset:", self.offset_edit),
            ("Face:", self.face_combo),
        ]

        start_row = 4
        for index, (label, widget) in enumerate(entries):
            row = start_row + index
            layout.addWidget(QLabel(label), row, 0, alignment=Qt.AlignmentFlag.AlignRight)
            layout.addWidget(widget, row, 1)

        group.setLayout(layout)
        return group

    def build_open_close_group(self):
        group = QGroupBox("Open/Close")
        layout = QVBoxLayout()

        criterion_layout = QHBoxLayout()
        criterion_layout.addStretch(1)
        criterion_layout.addWidget(QLabel("Open/Close Criterion:"))
        criterion_layout.addWidget(self.criterion_combo)
        criterion_layout.addStretch(1)

        layout.addLayout(criterion_layout)
        layout.addWidget(self.schedule_table)
        group.setLayout(layout)
        return group

    def connect_editor_signals(self):
        for widget in [
            self.id_edit,
            self.bottom_edit,
            self.height_edit,
            self.width_edit,
            self.initial_open_edit,
            self.offset_edit,
        ]:
            widget.textChanged.connect(self.update_current_record)

        for combo in [
            self.first_comp_combo,
            self.second_comp_combo,
            self.face_combo,
            self.criterion_combo,
        ]:
            combo.currentTextChanged.connect(self.update_current_record)

        self.schedule_table.cellChanged.connect(self.update_current_record)

    def make_default_record(self, index: int) -> dict:
        return {
            "id": f"WallVent_{index}",
            "first_compartment": "Comp 1",
            "second_compartment": "Outside",
            "bottom": format_value(LENGTH, 0.0),
            "height": format_value(LENGTH, 2.0),
            "width": format_value(LENGTH, 1.0),
            "initial_open": "1",
            "face": "Front",
            "offset": format_value(LENGTH, 2.0),
            "criterion": "Time",
            "schedule": [],
        }

    def load_demo_data(self):
        self.records = [self.make_default_record(1)]
        self.rebuild_summary_table()
        self.summary_table.setCurrentCell(0, 0)
        self.load_record_into_editor(0)

    def rebuild_summary_table(self):
        self.loading = True
        self.summary_table.setRowCount(len(self.records))

        for row, record in enumerate(self.records):
            self.update_summary_row(row, record)

        self.loading = False

    def update_summary_row(self, row: int, record: dict):
        values = [
            str(row + 1),
            record["id"],
            record["first_compartment"],
            record["second_compartment"],
            record["bottom"],
            record["height"],
            record["width"],
            record["initial_open"],
            record["face"],
            record["offset"],
        ]

        for col, value in enumerate(values):
            item = self.summary_table.item(row, col)

            if item is None:
                item = QTableWidgetItem()
                self.summary_table.setItem(row, col, item)

            item.setText(value)

    def current_row(self) -> int:
        return self.summary_table.currentRow()

    def current_row_changed(self, current_row, current_col, previous_row, previous_col):
        if self.loading:
            return

        if 0 <= previous_row < len(self.records):
            self.save_editor_to_record(previous_row)

        if 0 <= current_row < len(self.records):
            self.load_record_into_editor(current_row)
        else:
            self.clear_editor()

    def clear_editor(self):
        self.loading = True

        self.id_edit.clear()
        self.set_combo_text(self.first_comp_combo, "")
        self.set_combo_text(self.second_comp_combo, "")
        self.bottom_edit.clear()
        self.height_edit.clear()
        self.width_edit.clear()
        self.initial_open_edit.clear()
        self.offset_edit.clear()
        self.set_combo_text(self.face_combo, "Front")
        self.set_combo_text(self.criterion_combo, "Time")
        self.schedule_table.clearContents()
        self.set_editor_enabled(False)

        self.editor_group.setTitle("Vent Geometry")
        self.loading = False

    def set_editor_enabled(self, enabled: bool):
        for widget in [
            self.id_edit,
            self.first_comp_combo,
            self.second_comp_combo,
            self.bottom_edit,
            self.height_edit,
            self.width_edit,
            self.initial_open_edit,
            self.offset_edit,
            self.face_combo,
            self.criterion_combo,
            self.schedule_table,
        ]:
            widget.setEnabled(enabled)

    def set_combo_text(self, combo: QComboBox, text: str):
        index = combo.findText(text, Qt.MatchFlag.MatchFixedString)

        if index >= 0:
            combo.setCurrentIndex(index)
        else:
            combo.setEditText(text)

    def load_record_into_editor(self, row: int):
        record = self.records[row]
        self.loading = True
        self.set_editor_enabled(True)

        self.editor_group.setTitle(f"Vent {row + 1} (of {len(self.records)}) Geometry")
        self.id_edit.setText(record["id"])
        self.set_combo_text(self.first_comp_combo, record["first_compartment"])
        self.set_combo_text(self.second_comp_combo, record["second_compartment"])
        self.bottom_edit.setText(record["bottom"])
        self.height_edit.setText(record["height"])
        self.width_edit.setText(record["width"])
        self.initial_open_edit.setText(record["initial_open"])
        self.offset_edit.setText(record["offset"])
        self.set_combo_text(self.face_combo, record["face"])
        self.set_combo_text(self.criterion_combo, record["criterion"])

        self.schedule_table.clearContents()
        self.schedule_table.setRowCount(max(8, len(record["schedule"])))

        for schedule_row, (time_value, fraction_value) in enumerate(record["schedule"]):
            self.schedule_table.setItem(schedule_row, 0, QTableWidgetItem(time_value))
            self.schedule_table.setItem(schedule_row, 1, QTableWidgetItem(fraction_value))

        self.loading = False

    def save_editor_to_record(self, row: int):
        if not 0 <= row < len(self.records):
            return

        record = self.records[row]
        record["id"] = self.id_edit.text().strip()
        record["first_compartment"] = self.first_comp_combo.currentText().strip()
        record["second_compartment"] = self.second_comp_combo.currentText().strip()
        record["bottom"] = self.bottom_edit.text().strip()
        record["height"] = self.height_edit.text().strip()
        record["width"] = self.width_edit.text().strip()
        record["initial_open"] = self.initial_open_edit.text().strip()
        record["offset"] = self.offset_edit.text().strip()
        record["face"] = self.face_combo.currentText().strip()
        record["criterion"] = self.criterion_combo.currentText().strip()
        record["schedule"] = self.extract_schedule_strings()
        self.update_summary_row(row, record)

    def extract_schedule_strings(self) -> list[tuple[str, str]]:
        schedule = []

        for row in range(self.schedule_table.rowCount()):
            time_item = self.schedule_table.item(row, 0)
            fraction_item = self.schedule_table.item(row, 1)

            time_text = "" if time_item is None else time_item.text().strip()
            fraction_text = "" if fraction_item is None else fraction_item.text().strip()

            if not time_text and not fraction_text:
                continue

            schedule.append((time_text, fraction_text))

        return schedule

    def update_current_record(self):
        if self.loading:
            return

        row = self.current_row()

        if 0 <= row < len(self.records):
            self.save_editor_to_record(row)

    def add_record(self):
        self.update_current_record()
        row = self.current_row()

        if row < 0:
            insert_row = len(self.records)
        else:
            insert_row = row + 1

        self.records.insert(insert_row, self.make_default_record(len(self.records) + 1))
        self.rebuild_summary_table()
        self.summary_table.setCurrentCell(insert_row, 0)

    def duplicate_record(self):
        self.update_current_record()
        row = self.current_row()

        if not 0 <= row < len(self.records):
            return

        record = copy.deepcopy(self.records[row])
        record["id"] = f"{record['id']}_copy"
        self.records.insert(row + 1, record)
        self.rebuild_summary_table()
        self.summary_table.setCurrentCell(row + 1, 0)

    def move_record_up(self):
        self.update_current_record()
        row = self.current_row()

        if row <= 0:
            return

        self.records[row - 1], self.records[row] = self.records[row], self.records[row - 1]
        self.rebuild_summary_table()
        self.summary_table.setCurrentCell(row - 1, 0)

    def move_record_down(self):
        self.update_current_record()
        row = self.current_row()

        if row < 0 or row >= len(self.records) - 1:
            return

        self.records[row + 1], self.records[row] = self.records[row], self.records[row + 1]
        self.rebuild_summary_table()
        self.summary_table.setCurrentCell(row + 1, 0)

    def remove_record(self):
        row = self.current_row()

        if not 0 <= row < len(self.records):
            return

        del self.records[row]
        self.rebuild_summary_table()

        if self.records:
            self.summary_table.setCurrentCell(min(row, len(self.records) - 1), 0)
        else:
            self.clear_editor()

    def add_to_case(self, case: CfastCase):
        self.update_current_record()
        vents: list[WallVent] = []
        ids_seen: set[str] = set()
        valid_faces = {"FRONT", "REAR", "LEFT", "RIGHT"}

        for row, record in enumerate(self.records):
            if not any(str(value).strip() for value in record.values() if not isinstance(value, list)):
                continue

            vent_id = record["id"].strip()
            first_comp_id = record["first_compartment"].strip()
            second_comp_id = record["second_compartment"].strip() or "Outside"
            face = record["face"].strip().upper()
            criterion = record["criterion"].strip().upper() or "TIME"

            if second_comp_id.upper() == "OUTSIDE":
                second_comp_id = "OUTSIDE"

            if not vent_id:
                raise ValueError(f"Wall Vents row {row + 1}: ID is required.")

            if vent_id in ids_seen:
                raise ValueError(f"Wall Vents row {row + 1}: duplicate ID {vent_id!r}.")

            if not first_comp_id:
                raise ValueError(
                    f"Wall Vents row {row + 1}: first compartment is required."
                )

            if face not in valid_faces:
                raise ValueError(
                    f"Wall Vents row {row + 1}: Face must be Front, Rear, Right, or Left."
                )

            ids_seen.add(vent_id)

            bottom = parse_value(LENGTH, record["bottom"], "Wall vent bottom")
            height = parse_value(LENGTH, record["height"], "Wall vent height")
            width = parse_value(LENGTH, record["width"], "Wall vent width")
            initial_open = parse_number(record["initial_open"], "Initial Open")
            offset = parse_value(LENGTH, record["offset"], "Wall vent offset")

            if bottom < 0.0:
                raise ValueError(f"Wall Vents row {row + 1}: bottom must be non-negative.")

            if height <= 0.0:
                raise ValueError(f"Wall Vents row {row + 1}: height must be positive.")

            if width <= 0.0:
                raise ValueError(f"Wall Vents row {row + 1}: width must be positive.")

            if not 0.0 <= initial_open <= 1.0:
                raise ValueError(
                    f"Wall Vents row {row + 1}: Initial Open must be 0 to 1."
                )

            t_values: list[float] = []
            f_values: list[float] = []

            for schedule_row, (time_text, fraction_text) in enumerate(record["schedule"]):
                if not time_text or not fraction_text:
                    raise ValueError(
                        f"Wall Vents row {row + 1}, schedule row {schedule_row + 1}: "
                        "time and fraction are both required."
                    )

                time_value = parse_value(TIME, time_text, "Opening time")
                fraction_value = parse_number(fraction_text, "Opening fraction")

                if time_value < 0.0:
                    raise ValueError(
                        f"Wall Vents row {row + 1}: opening times must be non-negative."
                    )

                if not 0.0 <= fraction_value <= 1.0:
                    raise ValueError(
                        f"Wall Vents row {row + 1}: opening fractions must be 0 to 1."
                    )

                t_values.append(time_value)
                f_values.append(fraction_value)

            vents.append(
                WallVent(
                    id=vent_id,
                    first_comp_id=first_comp_id,
                    second_comp_id=second_comp_id,
                    bottom=bottom,
                    height=height,
                    width=width,
                    initial_open=initial_open,
                    face=face,
                    offset=offset,
                    criterion=criterion,
                    t_values=t_values,
                    f_values=f_values,
                )
            )

        case.wall_vents = vents

    def refresh_unit_labels(self):
        self.summary_table.setHorizontalHeaderLabels(summary_headers())
        self.schedule_table.setHorizontalHeaderLabels(
            [f"Time\n({unit_label(TIME)})", "Fraction"]
        )

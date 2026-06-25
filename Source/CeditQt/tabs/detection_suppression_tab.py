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

from cfast_case import CfastCase, DetectionDevice


_NUMBER_RE = re.compile(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?")


def parse_float(text: str, field_name: str, default: float | None = None) -> float:
    text = text.strip()

    if not text and default is not None:
        return default

    match = _NUMBER_RE.search(text)

    if match is None:
        raise ValueError(f"Could not parse numeric value for {field_name}: {text!r}")

    return float(match.group(0).replace("D", "E").replace("d", "e"))


def set_combo_text(combo: QComboBox, text: str) -> None:
    index = combo.findText(text)

    if index < 0:
        combo.addItem(text)
        index = combo.findText(text)

    combo.setCurrentIndex(index)


def display_type_to_devc_type(text: str) -> str:
    normalized = text.strip().upper()

    if normalized == "SPRINKLER":
        return "SPRINKLER"
    if normalized == "SMOKE":
        return "SMOKE_DETECTOR"
    if normalized == "HEAT":
        return "HEAT_DETECTOR"
    if normalized in {"SMOKE_DETECTOR", "HEAT_DETECTOR"}:
        return normalized

    return normalized


def make_table_item(text: str, editable: bool = True) -> QTableWidgetItem:
    item = QTableWidgetItem(text)

    flags = Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled
    if editable:
        flags |= Qt.ItemFlag.ItemIsEditable

    item.setFlags(flags)
    return item


class DetectionSuppressionTab(QWidget):
    """CEdit-style detector/sprinkler editor.

    The summary table is intentionally editable. The lower editor and the
    summary table are two views of the same in-memory DetectionDevice list.
    Signal blocking is used whenever one view updates the other, so editing a
    table cell does not rebuild or clear the table.
    """

    COL_NUM = 0
    COL_ID = 1
    COL_COMPARTMENT = 2
    COL_TYPE = 3
    COL_X = 4
    COL_Y = 5
    COL_Z = 6
    COL_ACTIVATION = 7
    COL_RTI = 8
    COL_SPRAY_DENSITY = 9

    def __init__(self, parent=None):
        super().__init__(parent)

        self.loading_editor = False
        self.updating_table = False
        self.current_index = -1
        self.devices: list[DetectionDevice] = []

        self.summary_table = QTableWidget(0, 10)
        self.summary_table.setHorizontalHeaderLabels(
            [
                "Num",
                "ID",
                "Compartment",
                "Type",
                "X Position",
                "Y Position",
                "Z Position",
                "Activation",
                "RTI",
                "Spray Density",
            ]
        )
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.verticalHeader().setVisible(False)
        self.summary_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.summary_table.setSelectionMode(QTableWidget.SelectionMode.SingleSelection)
        self.summary_table.itemSelectionChanged.connect(self.selection_changed)
        self.summary_table.itemChanged.connect(self.summary_item_changed)

        self.id_edit = QLineEdit()
        self.type_combo = QComboBox()
        self.type_combo.addItems(["Sprinkler", "Smoke", "Heat"])
        self.compartment_combo = QComboBox()
        self.compartment_combo.setEditable(True)

        self.x_edit = QLineEdit()
        self.y_edit = QLineEdit()
        self.z_edit = QLineEdit()

        self.activation_temperature_edit = QLineEdit()
        self.activation_obscuration_edit = QLineEdit()
        self.rti_edit = QLineEdit()
        self.spray_density_edit = QLineEdit()
        self.fyi_edit = QLineEdit()

        self.editor_group = self.build_editor_group()

        self.build_layout()
        self.connect_editor_signals()
        self.load_demo_data()
        self.rebuild_summary_table()
        self.select_device(0)

    def build_layout(self):
        main_layout = QVBoxLayout()
        main_layout.addWidget(self.summary_table, 3)
        main_layout.addLayout(self.build_button_row())
        main_layout.addWidget(self.editor_group, 2)
        self.setLayout(main_layout)

    def build_button_row(self):
        layout = QHBoxLayout()
        layout.addStretch(1)

        add_button = QPushButton("Add")
        duplicate_button = QPushButton("Duplicate")
        move_up_button = QPushButton("Move Up")
        move_down_button = QPushButton("Move Down")
        remove_button = QPushButton("Remove")

        add_button.clicked.connect(self.add_device)
        duplicate_button.clicked.connect(self.duplicate_device)
        move_up_button.clicked.connect(self.move_device_up)
        move_down_button.clicked.connect(self.move_device_down)
        remove_button.clicked.connect(self.remove_device)

        layout.addWidget(add_button)
        layout.addWidget(duplicate_button)
        layout.addWidget(move_up_button)
        layout.addWidget(move_down_button)
        layout.addStretch(1)
        layout.addWidget(remove_button)
        layout.addStretch(1)

        return layout

    def build_editor_group(self):
        group = QGroupBox("Alarm")
        layout = QGridLayout()

        layout.addWidget(QLabel("ID:"), 0, 1, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.id_edit, 0, 2, 1, 2)

        layout.addWidget(QLabel("Type:"), 1, 0, alignment=Qt.AlignmentFlag.AlignRight)
        layout.addWidget(self.type_combo, 1, 1)

        layout.addWidget(
            QLabel("Compartment:"), 1, 2, alignment=Qt.AlignmentFlag.AlignRight
        )
        layout.addWidget(self.compartment_combo, 1, 3)

        layout.addWidget(
            QLabel("Activation Temperature:"),
            1,
            4,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        layout.addWidget(self.activation_temperature_edit, 1, 5)

        layout.addWidget(
            QLabel("Activation Obscuration:"),
            2,
            4,
            alignment=Qt.AlignmentFlag.AlignRight,
        )
        layout.addWidget(self.activation_obscuration_edit, 2, 5)

        position_group = QGroupBox("Position")
        position_layout = QGridLayout()
        position_layout.addWidget(QLabel("Width (X):"), 0, 0)
        position_layout.addWidget(self.x_edit, 0, 1)
        position_layout.addWidget(QLabel("Depth (Y):"), 1, 0)
        position_layout.addWidget(self.y_edit, 1, 1)
        position_layout.addWidget(QLabel("Height (Z):"), 2, 0)
        position_layout.addWidget(self.z_edit, 2, 1)
        position_group.setLayout(position_layout)

        response_group = QGroupBox("Response / Suppression")
        response_layout = QGridLayout()
        response_layout.addWidget(QLabel("RTI:"), 0, 0)
        response_layout.addWidget(self.rti_edit, 0, 1)
        response_layout.addWidget(QLabel("Spray Density:"), 1, 0)
        response_layout.addWidget(self.spray_density_edit, 1, 1)
        response_layout.addWidget(QLabel("FYI:"), 2, 0)
        response_layout.addWidget(self.fyi_edit, 2, 1)
        response_group.setLayout(response_layout)

        layout.addWidget(position_group, 3, 1, 1, 2)
        layout.addWidget(response_group, 3, 4, 1, 2)
        layout.setColumnStretch(0, 1)
        layout.setColumnStretch(6, 1)

        group.setLayout(layout)
        return group

    def connect_editor_signals(self):
        for line_edit in [
            self.id_edit,
            self.x_edit,
            self.y_edit,
            self.z_edit,
            self.activation_temperature_edit,
            self.activation_obscuration_edit,
            self.rti_edit,
            self.spray_density_edit,
            self.fyi_edit,
        ]:
            line_edit.textChanged.connect(self.editor_changed)

        self.type_combo.currentTextChanged.connect(self.type_changed)
        self.compartment_combo.currentTextChanged.connect(self.editor_changed)

    def load_demo_data(self):
        self.devices = [
            DetectionDevice(
                id="Sprinkler_1",
                comp_id="Comp 1",
                device_type="SPRINKLER",
                x_position=3.0,
                y_position=3.0,
                z_position=2.97,
                activation_temperature=73.88998,
                activation_obscuration=23.93346,
                rti=100.0,
                spray_density=7.0e-5,
            ),
            DetectionDevice(
                id="SmokeDetector_2",
                comp_id="Comp 1",
                device_type="SMOKE_DETECTOR",
                x_position=2.0,
                y_position=2.0,
                z_position=2.97,
                activation_temperature=73.88998,
                activation_obscuration=23.93346,
                rti=404.0,
                spray_density=7.0e-5,
            ),
            DetectionDevice(
                id="HeatDetector_3",
                comp_id="Comp 1",
                device_type="HEAT_DETECTOR",
                x_position=2.0,
                y_position=2.0,
                z_position=2.97,
                activation_temperature=30.0,
                activation_obscuration=23.93346,
                rti=5.0,
                spray_density=0.0,
            ),
        ]
        self.update_compartment_choices()

    def update_compartment_choices(self):
        current = self.compartment_combo.currentText()
        self.compartment_combo.blockSignals(True)
        self.compartment_combo.clear()
        self.compartment_combo.addItems(["Comp 1", "Comp 2", "Comp 3"])
        if current:
            set_combo_text(self.compartment_combo, current)
        self.compartment_combo.blockSignals(False)

    def selected_device(self) -> DetectionDevice | None:
        if 0 <= self.current_index < len(self.devices):
            return self.devices[self.current_index]
        return None

    def device_activation_value(self, device: DetectionDevice) -> float:
        if device.device_type.upper() == "SMOKE_DETECTOR":
            return device.activation_obscuration
        return device.activation_temperature

    def device_summary_values(self, row: int, device: DetectionDevice) -> list[str]:
        return [
            str(row + 1),
            device.id,
            device.comp_id,
            device.display_type(),
            f"{device.x_position:g}",
            f"{device.y_position:g}",
            f"{device.z_position:g}",
            f"{self.device_activation_value(device):g}",
            f"{device.rti:g}",
            f"{device.spray_density:g}",
        ]

    def rebuild_summary_table(self):
        self.updating_table = True
        self.summary_table.setRowCount(max(8, len(self.devices)))
        self.summary_table.clearContents()

        for row in range(self.summary_table.rowCount()):
            if row < len(self.devices):
                values = self.device_summary_values(row, self.devices[row])
                editable = True
            else:
                values = ["", "", "", "", "", "", "", "", "", ""]
                editable = False

            for col, value in enumerate(values):
                self.summary_table.setItem(
                    row,
                    col,
                    make_table_item(value, editable=(editable and col != self.COL_NUM)),
                )

        self.updating_table = False
        self.update_editor_title()

    def update_summary_row(self, row: int):
        if not 0 <= row < len(self.devices):
            return

        self.updating_table = True
        values = self.device_summary_values(row, self.devices[row])

        for col, value in enumerate(values):
            self.summary_table.setItem(
                row,
                col,
                make_table_item(value, editable=(col != self.COL_NUM)),
            )

        self.updating_table = False

    def update_editor_title(self):
        if not self.devices or self.current_index < 0:
            title = "Alarm 0 (of 0)"
        else:
            title = f"Alarm {self.current_index + 1} (of {len(self.devices)})"

        self.editor_group.setTitle(title)

    def select_device(self, index: int):
        if not self.devices:
            self.current_index = -1
            self.clear_editor()
            self.rebuild_summary_table()
            return

        index = max(0, min(index, len(self.devices) - 1))
        self.current_index = index

        self.summary_table.blockSignals(True)
        self.summary_table.selectRow(index)
        self.summary_table.blockSignals(False)

        self.load_editor(index)
        self.update_editor_title()

    def selection_changed(self):
        if self.loading_editor or self.updating_table:
            return

        indexes = self.summary_table.selectionModel().selectedRows()
        if not indexes:
            return

        new_index = indexes[0].row()
        if not 0 <= new_index < len(self.devices):
            return

        if new_index == self.current_index:
            return

        try:
            self.save_current_editor()
            self.update_summary_row(self.current_index)
        except ValueError:
            pass

        self.current_index = new_index
        self.load_editor(new_index)
        self.update_editor_title()

    def clear_editor(self):
        self.loading_editor = True
        for widget in [
            self.id_edit,
            self.x_edit,
            self.y_edit,
            self.z_edit,
            self.activation_temperature_edit,
            self.activation_obscuration_edit,
            self.rti_edit,
            self.spray_density_edit,
            self.fyi_edit,
        ]:
            widget.clear()
        self.loading_editor = False

    def load_editor(self, index: int):
        if not 0 <= index < len(self.devices):
            self.clear_editor()
            return

        self.loading_editor = True
        device = self.devices[index]

        self.id_edit.setText(device.id)
        set_combo_text(self.type_combo, device.display_type())
        set_combo_text(self.compartment_combo, device.comp_id)
        self.x_edit.setText(f"{device.x_position:g} m")
        self.y_edit.setText(f"{device.y_position:g} m")
        self.z_edit.setText(f"{device.z_position:g} m")
        self.activation_temperature_edit.setText(f"{device.activation_temperature:g} C")
        self.activation_obscuration_edit.setText(
            f"{device.activation_obscuration:g} %/m"
        )
        self.rti_edit.setText(f"{device.rti:g}")
        self.spray_density_edit.setText(f"{device.spray_density:g}")
        self.fyi_edit.setText(device.fyi)

        self.loading_editor = False
        self.update_editor_enabled_state()

    def update_editor_enabled_state(self):
        device_type = self.type_combo.currentText().strip().upper()
        is_smoke = device_type == "SMOKE"
        is_sprinkler = device_type == "SPRINKLER"

        self.activation_temperature_edit.setEnabled(not is_smoke)
        self.activation_obscuration_edit.setEnabled(is_smoke)
        self.spray_density_edit.setEnabled(is_sprinkler)

    def type_changed(self):
        self.update_editor_enabled_state()
        self.editor_changed()

    def editor_changed(self):
        if self.loading_editor:
            return

        try:
            self.save_current_editor()
            self.update_summary_row(self.current_index)
        except ValueError:
            # Let partially typed text remain in the editor. Export/run will
            # validate the final value.
            pass

    def summary_item_changed(self, item: QTableWidgetItem):
        if self.updating_table or self.loading_editor:
            return

        row = item.row()
        if not 0 <= row < len(self.devices):
            return

        try:
            self.apply_summary_row_to_device(row)
        except ValueError:
            # Do not rebuild or clear the row during partial edits. The invalid
            # value remains visible and will be reported on export/run.
            return

        if row == self.current_index:
            self.load_editor(row)

        self.update_summary_row(row)

    def table_text(self, row: int, col: int) -> str:
        item = self.summary_table.item(row, col)
        if item is None:
            return ""
        return item.text().strip()

    def apply_summary_row_to_device(self, row: int):
        device = self.devices[row]

        device.id = self.table_text(row, self.COL_ID) or device.id
        device.comp_id = self.table_text(row, self.COL_COMPARTMENT) or "Comp 1"
        device.device_type = display_type_to_devc_type(
            self.table_text(row, self.COL_TYPE) or device.display_type()
        )
        device.x_position = parse_float(
            self.table_text(row, self.COL_X),
            "X Position",
            device.x_position,
        )
        device.y_position = parse_float(
            self.table_text(row, self.COL_Y),
            "Y Position",
            device.y_position,
        )
        device.z_position = parse_float(
            self.table_text(row, self.COL_Z),
            "Z Position",
            device.z_position,
        )

        activation = parse_float(
            self.table_text(row, self.COL_ACTIVATION),
            "Activation",
            self.device_activation_value(device),
        )
        if device.device_type.upper() == "SMOKE_DETECTOR":
            device.activation_obscuration = activation
        else:
            device.activation_temperature = activation

        device.rti = parse_float(
            self.table_text(row, self.COL_RTI),
            "RTI",
            device.rti,
        )
        device.spray_density = parse_float(
            self.table_text(row, self.COL_SPRAY_DENSITY),
            "Spray Density",
            device.spray_density,
        )

    def save_current_editor(self):
        device = self.selected_device()
        if device is None:
            return

        device.id = self.id_edit.text().strip() or device.id
        device.device_type = display_type_to_devc_type(self.type_combo.currentText())
        device.comp_id = self.compartment_combo.currentText().strip() or "Comp 1"
        device.x_position = parse_float(self.x_edit.text(), "X Position", 0.0)
        device.y_position = parse_float(self.y_edit.text(), "Y Position", 0.0)
        device.z_position = parse_float(self.z_edit.text(), "Z Position", 0.0)
        device.activation_temperature = parse_float(
            self.activation_temperature_edit.text(),
            "Activation Temperature",
            73.88998,
        )
        device.activation_obscuration = parse_float(
            self.activation_obscuration_edit.text(),
            "Activation Obscuration",
            23.93346,
        )
        device.rti = parse_float(self.rti_edit.text(), "RTI", 100.0)
        device.spray_density = parse_float(
            self.spray_density_edit.text(),
            "Spray Density",
            0.0,
        )
        device.fyi = self.fyi_edit.text().strip()

    def add_device(self):
        try:
            self.save_current_editor()
            self.update_summary_row(self.current_index)
        except ValueError:
            pass

        number = len(self.devices) + 1
        device = DetectionDevice(
            id=f"Sprinkler_{number}",
            comp_id="Comp 1",
            device_type="SPRINKLER",
        )
        self.devices.append(device)
        self.rebuild_summary_table()
        self.select_device(len(self.devices) - 1)

    def duplicate_device(self):
        if not 0 <= self.current_index < len(self.devices):
            return

        self.save_current_editor()
        device = copy.deepcopy(self.devices[self.current_index])
        device.id = f"{device.id}_copy"
        self.devices.insert(self.current_index + 1, device)
        self.rebuild_summary_table()
        self.select_device(self.current_index + 1)

    def move_device_up(self):
        if self.current_index <= 0:
            return

        self.save_current_editor()
        index = self.current_index
        self.devices[index - 1], self.devices[index] = (
            self.devices[index],
            self.devices[index - 1],
        )
        self.rebuild_summary_table()
        self.select_device(index - 1)

    def move_device_down(self):
        if not 0 <= self.current_index < len(self.devices) - 1:
            return

        self.save_current_editor()
        index = self.current_index
        self.devices[index + 1], self.devices[index] = (
            self.devices[index],
            self.devices[index + 1],
        )
        self.rebuild_summary_table()
        self.select_device(index + 1)

    def remove_device(self):
        if not 0 <= self.current_index < len(self.devices):
            return

        del self.devices[self.current_index]
        self.rebuild_summary_table()

        if self.devices:
            self.select_device(min(self.current_index, len(self.devices) - 1))
        else:
            self.current_index = -1
            self.clear_editor()
            self.update_editor_title()

    def add_to_case(self, case: CfastCase):
        self.save_current_editor()
        self.update_summary_row(self.current_index)

        ids_seen: set[str] = set()
        for index, device in enumerate(self.devices, start=1):
            if not device.id:
                raise ValueError(f"Detection / Suppression row {index}: ID is required.")
            if device.id in ids_seen:
                raise ValueError(
                    f"Detection / Suppression row {index}: duplicate ID {device.id!r}."
                )
            ids_seen.add(device.id)

            if device.device_type.upper() not in {
                "SPRINKLER",
                "SMOKE_DETECTOR",
                "HEAT_DETECTOR",
            }:
                raise ValueError(
                    f"Detection / Suppression row {index}: unknown type "
                    f"{device.device_type!r}."
                )
            if device.rti < 0.0:
                raise ValueError(f"Detection / Suppression row {index}: RTI < 0.")
            if device.spray_density < 0.0:
                raise ValueError(
                    f"Detection / Suppression row {index}: spray density < 0."
                )

        case.detection_devices = list(self.devices)

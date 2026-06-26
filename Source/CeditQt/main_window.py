from __future__ import annotations

import copy
from pathlib import Path

from PySide6.QtCore import QProcess, QSettings
from PySide6.QtGui import QAction
from PySide6.QtWidgets import (
    QFileDialog,
    QHBoxLayout,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QSpacerItem,
    QStatusBar,
    QTableWidgetItem,
    QTabWidget,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase
from cfast_reader import read_cfast_input
from cfast_writer import write_cfast_input
from tabs.ceiling_floor_vents_tab import CeilingFloorVentsTab
from tabs.compartments_tab import CompartmentsTab
from tabs.detection_suppression_tab import DetectionSuppressionTab
from tabs.fires_tab import FiresTab
from tabs.mechanical_vents_tab import MechanicalVentsTab
from tabs.output_tab import OutputTab
from tabs.placeholder_tab import PlaceholderTab
from tabs.simulation_tab import SimulationTab
from tabs.surface_connections_tab import SurfaceConnectionsTab
from tabs.targets_tab import TargetsTab
from tabs.thermal_properties_tab import ThermalPropertiesTab
from tabs.wall_vents_tab import WallVentsTab
import tabs.targets_tab as targets_tab_module


class CeditMainWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("CEdit Qt Prototype")
        self.resize(1200, 800)

        self.current_path: Path | None = None
        self.loaded_case: CfastCase | None = None
        self.cfast_process: QProcess | None = None
        self.settings = QSettings("FireModels", "CEditQt")
        self.cfast_executable = self.settings.value("cfast_executable", "", type=str)

        self.simulation_tab = SimulationTab()
        self.thermal_properties_tab = ThermalPropertiesTab()
        self.compartments_tab = CompartmentsTab()
        self.wall_vents_tab = WallVentsTab()
        self.ceiling_floor_vents_tab = CeilingFloorVentsTab()
        self.mechanical_vents_tab = MechanicalVentsTab()
        self.fires_tab = FiresTab()
        self.targets_tab = TargetsTab()
        self.detection_suppression_tab = DetectionSuppressionTab()
        self.surface_connections_tab = SurfaceConnectionsTab()
        self.output_tab = OutputTab()
        self.tabs = None

        self.build_menu()
        self.build_toolbar()
        self.build_central_widget()

        self.setStatusBar(QStatusBar(self))
        self.statusBar().showMessage("No Errors")

    def build_menu(self):
        self.menuBar().setNativeMenuBar(False)

        file_menu = self.menuBar().addMenu("&File")

        open_action = QAction("&Open...", self)
        open_action.triggered.connect(self.open_cfast_input)
        file_menu.addAction(open_action)

        save_action = QAction("&Save", self)
        save_action.triggered.connect(self.save_cfast_input)
        file_menu.addAction(save_action)

        export_action = QAction("&Export CFAST Input...", self)
        export_action.triggered.connect(self.export_cfast_input)
        file_menu.addAction(export_action)

        file_menu.addSeparator()

        set_cfast_action = QAction("Set &CFAST Executable...", self)
        set_cfast_action.triggered.connect(self.set_cfast_executable)
        file_menu.addAction(set_cfast_action)

        clear_cfast_action = QAction("Use CFAST from &PATH", self)
        clear_cfast_action.triggered.connect(self.clear_cfast_executable)
        file_menu.addAction(clear_cfast_action)

        file_menu.addSeparator()

        exit_action = QAction("E&xit", self)
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)

        view_menu = self.menuBar().addMenu("&View")

        geometry_action = QAction("&Geometry", self)
        geometry_action.triggered.connect(self.geometry_placeholder)
        view_menu.addAction(geometry_action)

        results_action = QAction("&Results", self)
        results_action.triggered.connect(self.view_placeholder)
        view_menu.addAction(results_action)

        help_menu = self.menuBar().addMenu("&Help")
        about_action = QAction("&About CEdit Qt Prototype", self)
        about_action.triggered.connect(self.about)
        help_menu.addAction(about_action)

        self.open_action = open_action
        self.save_action = save_action
        self.export_action = export_action
        self.set_cfast_action = set_cfast_action
        self.clear_cfast_action = clear_cfast_action
        self.exit_action = exit_action
        self.geometry_action = geometry_action
        self.results_action = results_action

    def build_toolbar(self):
        toolbar = QToolBar("Main Toolbar")
        self.addToolBar(toolbar)

        toolbar.addAction(self.open_action)
        toolbar.addAction(self.save_action)
        toolbar.addAction(self.export_action)
        toolbar.addSeparator()
        toolbar.addAction(self.geometry_action)
        toolbar.addAction(self.results_action)
        toolbar.addSeparator()
        toolbar.addAction(self.exit_action)

    def build_central_widget(self):
        container = QWidget()
        layout = QVBoxLayout()

        self.tabs = QTabWidget()
        self.tabs.addTab(self.simulation_tab, "Simulation")
        self.tabs.addTab(self.thermal_properties_tab, "Thermal Properties")
        self.tabs.addTab(self.compartments_tab, "Compartments")
        self.tabs.addTab(self.wall_vents_tab, "Wall Vents")
        self.tabs.addTab(self.ceiling_floor_vents_tab, "Ceiling/Floor Vents")
        self.tabs.addTab(self.mechanical_vents_tab, "Mechanical Ventilation")
        self.tabs.addTab(self.fires_tab, "Fires")
        self.tabs.addTab(self.targets_tab, "Targets")
        self.tabs.addTab(self.detection_suppression_tab, "Detection / Suppression")
        self.tabs.addTab(self.surface_connections_tab, "Surface Connections")
        self.tabs.addTab(self.output_tab, "Output")

        layout.addWidget(self.tabs, 1)
        layout.addLayout(self.build_button_row())
        container.setLayout(layout)
        self.setCentralWidget(container)

    def build_button_row(self):
        row = QHBoxLayout()
        row.addStretch(1)

        open_button = QPushButton("Open")
        save_button = QPushButton("Save")
        geometry_button = QPushButton("Geometry")
        run_button = QPushButton("Run")
        view_button = QPushButton("View")

        open_button.clicked.connect(self.open_cfast_input)
        save_button.clicked.connect(self.save_cfast_input)
        geometry_button.clicked.connect(self.geometry_placeholder)
        run_button.clicked.connect(self.run_cfast)
        view_button.clicked.connect(self.view_placeholder)

        row.addWidget(open_button)
        row.addWidget(save_button)
        row.addItem(QSpacerItem(40, 1))
        row.addWidget(geometry_button)
        row.addWidget(run_button)
        row.addItem(QSpacerItem(40, 1))
        row.addWidget(view_button)
        row.addStretch(1)
        return row

    def build_cfast_case(self):
        case = CfastCase()
        self.simulation_tab.add_to_case(case)
        self.thermal_properties_tab.add_to_case(case)
        self.compartments_tab.add_to_case(case)
        self.wall_vents_tab.add_to_case(case)
        self.ceiling_floor_vents_tab.add_to_case(case)
        self.mechanical_vents_tab.add_to_case(case)
        self.targets_tab.add_to_case(case)
        self.detection_suppression_tab.add_to_case(case)
        self.surface_connections_tab.add_to_case(case)
        self.output_tab.add_to_case(case)
        self.fires_tab.add_to_case(case)

        # Until every tab has a mature load-from-case implementation, prevent old
        # screenshot-derived demo rows in less-used tabs from contaminating an opened case.
        if self.loaded_case is not None:
            case.ceiling_floor_vents = copy.deepcopy(self.loaded_case.ceiling_floor_vents)
            case.mechanical_vents = copy.deepcopy(self.loaded_case.mechanical_vents)
            case.detection_devices = copy.deepcopy(self.loaded_case.detection_devices)
            case.wall_surface_connections = copy.deepcopy(
                self.loaded_case.wall_surface_connections
            )
            case.ceiling_floor_surface_connections = copy.deepcopy(
                self.loaded_case.ceiling_floor_surface_connections
            )
            case.output_visualizations = copy.deepcopy(
                self.loaded_case.output_visualizations
            )

        return case

    def open_cfast_input(self):
        path_text, _ = QFileDialog.getOpenFileName(
            self,
            "Open CFAST Input",
            str(Path.cwd()),
            "CFAST input files (*.in);;All files (*)",
        )
        if not path_text:
            return

        path = Path(path_text)
        try:
            case = read_cfast_input(path)
            self.load_case_into_editor(case)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Open failed", str(exc))
            return

        self.current_path = path
        self.loaded_case = copy.deepcopy(case)
        message = f"Opened CFAST input file:\n{path}"
        self.simulation_tab.set_message(message)
        self.statusBar().showMessage("No Errors")

    def load_case_into_editor(self, case: CfastCase):
        self.load_simulation_tab(case)
        self.load_thermal_properties_tab(case)
        self.load_compartments_tab(case)
        self.load_wall_vents_tab(case)
        self.load_ceiling_floor_vents_tab(case)
        self.load_targets_tab(case)
        self.load_fires_tab(case)
        self.clear_unsupported_demo_tabs()

    def load_simulation_tab(self, case: CfastCase):
        tab = self.simulation_tab
        tab.title_edit.setText(case.title)
        tab.simulation_time_edit.setText(f"{format_number(case.simulation_time)} s")
        tab.print_interval_edit.setText(f"{format_number(case.print_interval)} s")
        tab.spreadsheet_interval_edit.setText(
            f"{format_number(case.spreadsheet_interval)} s"
        )
        tab.smokeview_interval_edit.setText(f"{format_number(case.smokeview_interval)} s")
        if case.max_time_step is None:
            tab.max_time_step_edit.setText("Default")
        else:
            tab.max_time_step_edit.setText(f"{format_number(case.max_time_step)} s")
        tab.interior_temperature_edit.setText(
            f"{format_number(case.interior_temperature)} C"
        )
        tab.relative_humidity_edit.setText(f"{format_number(case.relative_humidity)} %")
        tab.exterior_temperature_edit.setText(
            f"{format_number(case.exterior_temperature)} C"
        )
        tab.pressure_edit.setText(f"{format_number(case.pressure)} Pa")
        tab.adiabatic_checkbox.setChecked(case.adiabatic_surfaces)
        tab.lower_oxygen_limit_edit.setText(format_number(case.lower_oxygen_limit))

    def load_thermal_properties_tab(self, case: CfastCase):
        tab = self.thermal_properties_tab
        tab.table.blockSignals(True)
        tab.table.clearContents()
        tab.table.setRowCount(max(8, len(case.materials)))
        for row, material in enumerate(case.materials):
            values = [
                material.id,
                material.material,
                format_number(material.conductivity),
                format_number(material.specific_heat),
                format_number(material.density),
                format_number(material.thickness),
                format_number(material.emissivity),
                material.fyi,
            ]
            for col, value in enumerate(values):
                tab.table.setItem(row, col, QTableWidgetItem(value))
        tab.table.blockSignals(False)

    def load_compartments_tab(self, case: CfastCase):
        tab = self.compartments_tab
        tab.compartments = copy.deepcopy(case.compartments)
        if not tab.compartments:
            tab.compartments = tab.default_compartments()
        tab.refresh_summary_table(select_row=0)
        tab.load_detail_from_selected()

    def load_wall_vents_tab(self, case: CfastCase):
        tab = self.wall_vents_tab
        records = []
        for vent in case.wall_vents:
            records.append(
                {
                    "id": vent.id,
                    "first_compartment": vent.first_comp_id,
                    "second_compartment": display_outside(vent.second_comp_id),
                    "bottom": format_number(vent.bottom),
                    "height": format_number(vent.height),
                    "width": format_number(vent.width),
                    "initial_open": format_number(vent.initial_open),
                    "face": vent.face.title(),
                    "offset": format_number(vent.offset),
                    "criterion": vent.criterion.title(),
                    "schedule": [
                        (format_number(t_value), format_number(f_value))
                        for t_value, f_value in zip(vent.t_values, vent.f_values)
                    ],
                }
            )
        tab.records = records
        tab.rebuild_summary_table()
        if tab.records:
            tab.summary_table.setCurrentCell(0, 0)
            tab.load_record_into_editor(0)
        else:
            tab.clear_editor()

    def load_ceiling_floor_vents_tab(self, case: CfastCase):
        tab = self.ceiling_floor_vents_tab
        if not hasattr(tab, "summary_table"):
            return
        tab.summary_table.blockSignals(True)
        tab.summary_table.clearContents()
        tab.summary_table.setRowCount(max(8, len(case.ceiling_floor_vents)))
        if hasattr(tab, "schedules"):
            tab.schedules.clear()
        for row, vent in enumerate(case.ceiling_floor_vents):
            values = [
                str(row + 1),
                vent.id,
                vent.first_comp_id,
                vent.second_comp_id,
                vent.vent_type.upper(),
                vent.shape.upper(),
                format_number(vent.area),
                format_number(vent.initial_open),
                format_number(vent.offset_x),
                format_number(vent.offset_y),
            ]
            for col, value in enumerate(values):
                tab.summary_table.setItem(row, col, QTableWidgetItem(value))
            if hasattr(tab, "schedules"):
                tab.schedules[row] = (list(vent.t_values), list(vent.f_values))
        tab.summary_table.blockSignals(False)
        if hasattr(tab, "renumber_rows"):
            tab.renumber_rows()
        if case.ceiling_floor_vents:
            tab.summary_table.setCurrentCell(0, 0)
            if hasattr(tab, "load_editor_from_row"):
                tab.load_editor_from_row(0)
        elif hasattr(tab, "load_editor_from_row"):
            tab.current_row = 0
            tab.load_editor_from_row(0)

    def load_targets_tab(self, case: CfastCase):
        for material in case.materials:
            targets_tab_module.MATERIAL_LIBRARY[material.id.upper()] = {
                "conductivity": material.conductivity,
                "specific_heat": material.specific_heat,
                "density": material.density,
                "thickness": material.thickness,
            }

        tab = self.targets_tab
        tab.material_combo.blockSignals(True)
        for material in case.materials:
            if tab.material_combo.findText(material.id) < 0:
                tab.material_combo.addItem(material.id)
        tab.material_combo.blockSignals(False)
        tab.targets = copy.deepcopy(case.targets)
        tab.current_index = 0 if tab.targets else -1
        tab.refresh_summary_table(select_row=0 if tab.targets else None)

    def load_fires_tab(self, case: CfastCase):
        tab = self.fires_tab
        tab.fires = copy.deepcopy(case.fires)
        tab.fire_properties = copy.deepcopy(case.fire_properties)

        compartment_ids = [compartment.id for compartment in case.compartments]
        set_combo_items(tab.compartment_combo, compartment_ids)
        set_combo_items(tab.target_combo, [target.id for target in case.targets])

        tab.update_property_choices()
        tab.rebuild_summary_table()
        if tab.fires:
            tab.select_fire(0)
        else:
            tab.current_index = -1
            tab.clear_editor()

    def clear_unsupported_demo_tabs(self):
        # These tabs either are not present in the Ignition_Test verification case or
        # do not yet have a stable public load-from-case API. Clear visible demo data
        # where possible; build_cfast_case also preserves the loaded case values.
        for tab in [
            self.mechanical_vents_tab,
            self.detection_suppression_tab,
            self.surface_connections_tab,
            self.output_tab,
        ]:
            for attr in [
                "records",
                "mechanical_vents",
                "devices",
                "detection_devices",
                "wall_connections",
                "ceiling_floor_connections",
                "visualizations",
                "outputs",
            ]:
                if hasattr(tab, attr):
                    try:
                        setattr(tab, attr, [])
                    except Exception:
                        pass
            for method_name in [
                "rebuild_summary_table",
                "refresh_summary_table",
                "refresh_table",
                "clear_table",
            ]:
                method = getattr(tab, method_name, None)
                if callable(method):
                    try:
                        method()
                        break
                    except Exception:
                        pass

    def save_cfast_input(self):
        if self.current_path is None:
            self.export_cfast_input()
            return

        self.write_case_to_path(self.current_path)

    def export_cfast_input(self):
        path_text, _ = QFileDialog.getSaveFileName(
            self,
            "Export CFAST Input",
            "cedit_qt_test.in",
            "CFAST input files (*.in);;All files (*)",
        )
        if not path_text:
            return

        path = Path(path_text)
        self.write_case_to_path(path)
        self.current_path = path

    def write_case_to_path(self, path: Path):
        try:
            case = self.build_cfast_case()
            write_cfast_input(case, path)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Export failed", str(exc))
            return

        self.loaded_case = copy.deepcopy(case)
        message = f"Wrote CFAST input file:\n{path}"
        self.simulation_tab.set_message(message)
        self.statusBar().showMessage("No Errors")
        QMessageBox.information(self, "Export complete", message)

    def get_cfast_executable(self) -> str:
        if self.cfast_executable:
            return self.cfast_executable
        return "cfast"

    def set_cfast_executable(self):
        path_text, _ = QFileDialog.getOpenFileName(
            self,
            "Select CFAST Executable",
            str(Path.home()),
            "All files (*)",
        )
        if not path_text:
            return

        path = Path(path_text).expanduser()
        if not path.is_file():
            QMessageBox.critical(
                self,
                "CFAST Executable",
                f"Not a file:\n{path}",
            )
            return

        self.cfast_executable = str(path)
        self.settings.setValue("cfast_executable", self.cfast_executable)
        self.simulation_tab.set_message(
            f"CFAST executable set to:\n{self.cfast_executable}"
        )
        self.statusBar().showMessage("No Errors")

    def clear_cfast_executable(self):
        self.cfast_executable = ""
        self.settings.remove("cfast_executable")
        self.simulation_tab.set_message(
            "CFAST executable override cleared.\n"
            "CEdit Qt will run 'cfast' from PATH."
        )
        self.statusBar().showMessage("No Errors")

    def run_cfast(self):
        if self.cfast_process is not None:
            if self.cfast_process.state() != QProcess.ProcessState.NotRunning:
                QMessageBox.information(
                    self,
                    "Run",
                    "CFAST is already running.",
                )
                return

        if self.current_path is None:
            path_text, _ = QFileDialog.getSaveFileName(
                self,
                "Save CFAST Input Before Running",
                "cedit_qt_test.in",
                "CFAST input files (*.in);;All files (*)",
            )
            if not path_text:
                return
            self.current_path = Path(path_text)

        path = self.current_path
        try:
            case = self.build_cfast_case()
            write_cfast_input(case, path)
            self.loaded_case = copy.deepcopy(case)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Run failed", str(exc))
            return

        self.simulation_tab.set_message(
            f"Wrote CFAST input file:\n{path}\n\n"
            f"Running CFAST...\n\n"
        )
        self.statusBar().showMessage("Running CFAST")

        self.cfast_process = QProcess(self)
        self.cfast_process.setWorkingDirectory(str(path.parent))
        self.cfast_process.readyReadStandardOutput.connect(
            self.cfast_ready_read_stdout
        )
        self.cfast_process.readyReadStandardError.connect(
            self.cfast_ready_read_stderr
        )
        self.cfast_process.finished.connect(self.cfast_finished)
        self.cfast_process.errorOccurred.connect(self.cfast_error)

        cfast_exe = self.get_cfast_executable()
        self.simulation_tab.append_message(f"Executable: {cfast_exe}\n\n")
        self.cfast_process.start(cfast_exe, [path.name])

    def cfast_ready_read_stdout(self):
        if self.cfast_process is None:
            return
        text = bytes(self.cfast_process.readAllStandardOutput()).decode(
            "utf-8",
            errors="replace",
        )
        self.simulation_tab.append_message(text)

    def cfast_ready_read_stderr(self):
        if self.cfast_process is None:
            return
        text = bytes(self.cfast_process.readAllStandardError()).decode(
            "utf-8",
            errors="replace",
        )
        self.simulation_tab.append_message(text)

    def cfast_finished(self, exit_code, exit_status):
        if exit_status == QProcess.ExitStatus.NormalExit and exit_code == 0:
            self.simulation_tab.append_message(
                f"\nCFAST completed successfully with exit code {exit_code}.\n"
            )
            self.statusBar().showMessage("No Errors")
        else:
            self.simulation_tab.append_message(
                f"\nCFAST finished with exit code {exit_code}.\n"
            )
            self.statusBar().showMessage("Errors")
        self.cfast_process = None

    def cfast_error(self, error):
        executable = self.get_cfast_executable()
        self.simulation_tab.append_message(
            f"\nCFAST process error: {error}\n\n"
            f"Attempted executable:\n{executable}\n\n"
            "If this is a development build, use:\n"
            "File > Set CFAST Executable...\n"
            "and select your built CFAST executable.\n"
        )
        self.statusBar().showMessage("Errors")

    def geometry_placeholder(self):
        QMessageBox.information(
            self,
            "Geometry",
            "Geometry preview is not implemented yet.",
        )

    def view_placeholder(self):
        QMessageBox.information(
            self,
            "View",
            "Viewing results is not implemented yet.",
        )

    def about(self):
        QMessageBox.information(
            self,
            "About CEdit Qt Prototype",
            "Experimental Python/PySide6 prototype for a new CEdit front end.",
        )


def format_number(value: float | int | None) -> str:
    if value is None:
        return ""
    value = float(value)
    if abs(value - round(value)) < 1.0e-12:
        return str(int(round(value)))
    return f"{value:.6g}"


def display_outside(value: str) -> str:
    return "Outside" if value.strip().upper() == "OUTSIDE" else value


def set_combo_items(combo, values: list[str]) -> None:
    current = combo.currentText()
    combo.blockSignals(True)
    combo.clear()
    for value in values:
        combo.addItem(value)
    if current and combo.findText(current) >= 0:
        combo.setCurrentText(current)
    combo.blockSignals(False)

from __future__ import annotations

import re
import subprocess
from pathlib import Path

from PySide6.QtCore import QProcess, QSettings, QTimer, Qt, Signal
from PySide6.QtGui import QAction
from PySide6.QtWidgets import (
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QFormLayout,
    QGridLayout,
    QHeaderView,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QMainWindow,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QSpacerItem,
    QStatusBar,
    QTableWidget,
    QTableWidgetItem,
    QTabWidget,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from cfast_case import (
    CfastCase,
    Compartment,
    FireDefinition,
    FireProperty,
    FireRampPoint,
    MaterialProperty,
    WallVent,
)
from cfast_reader import read_cfast_input_with_warnings
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
from units import (
    BASE_UNIT_KEYS,
    HRR,
    LENGTH,
    MASS_LOSS,
    PRESSURE,
    TEMPERATURE,
    TIME,
    format_value,
    unit_label,
    unit_system,
)


FILENAME_WHITESPACE = re.compile(r"\s+")
CFAST_VERSION_TAG = re.compile(r"^CFAST-?(\d+(?:\.\d+)+)$", re.IGNORECASE)
BASE_UNIT_NAMES = {
    "length": "Length",
    "mass": "Mass",
    "time": "Time",
    "temperature": "Temperature",
    "pressure": "Pressure",
    "energy": "Energy",
    "smoke": "Smoke",
}


def latest_cfast_version_from_tag() -> tuple[str, str] | None:
    repo_root = Path(__file__).resolve().parents[2]
    try:
        result = subprocess.run(
            ["git", "tag", "--list"],
            cwd=repo_root,
            check=True,
            capture_output=True,
            text=True,
            timeout=2,
        )
    except (OSError, subprocess.SubprocessError):
        return None

    latest: tuple[tuple[int, ...], str, str] | None = None
    for tag in result.stdout.splitlines():
        match = CFAST_VERSION_TAG.match(tag.strip())
        if not match:
            continue

        version = match.group(1)
        version_key = tuple(int(part) for part in version.split("."))
        if latest is None or version_key > latest[0]:
            latest = (version_key, version, tag)

    if latest is None:
        return None

    return latest[1], latest[2]


def single_compartment_example() -> CfastCase:
    case = CfastCase(
        title="Single Compartment Example",
        simulation_time=600.0,
        print_interval=10.0,
        smokeview_interval=10.0,
        spreadsheet_interval=10.0,
    )
    case.compartments = [
        Compartment(
            id="Room",
            width=5.0,
            depth=4.0,
            height=3.0,
            grid=(50, 50, 50),
        )
    ]
    case.wall_vents = [
        WallVent(
            id="Door",
            first_comp_id="Room",
            second_comp_id="OUTSIDE",
            bottom=0.0,
            height=2.0,
            width=0.9,
            face="FRONT",
            offset=2.05,
        )
    ]
    case.fire_properties = [
        FireProperty(
            id="Example Fire",
            carbon=1,
            hydrogen=4,
            heat_of_combustion=50000.0,
            radiative_fraction=0.35,
            ramp=[
                FireRampPoint(time=0.0, hrr=0.0, height=0.0, area=0.25),
                FireRampPoint(time=60.0, hrr=250.0, height=0.0, area=0.25),
                FireRampPoint(time=300.0, hrr=250.0, height=0.0, area=0.25),
                FireRampPoint(time=600.0, hrr=0.0, height=0.0, area=0.25),
            ],
        )
    ]
    case.fires = [
        FireDefinition(
            id="Fire",
            comp_id="Room",
            fire_property_id="Example Fire",
            ignition_criterion="TIME",
            setpoint=0.0,
            x_position=2.5,
            y_position=2.0,
        )
    ]
    return case


def default_concrete_material() -> MaterialProperty:
    return MaterialProperty(
        id="CONCRETE",
        material="Concrete Normal Weight (6 in)",
        conductivity=1.75,
        specific_heat=1.0,
        density=2200.0,
        thickness=0.15,
        emissivity=0.94,
    )


def sanitize_cfast_input_path(path: Path) -> Path:
    """Return a CFAST input path whose file name has no whitespace."""
    name = path.name.strip()
    sanitized_name = FILENAME_WHITESPACE.sub("_", name)
    if not sanitized_name:
        sanitized_name = "cedit_qt_test.in"

    return path.with_name(sanitized_name)


class CeditMainWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("CFAST Editor (CEdit)")
        self.resize(1200, 800)

        self.current_path: Path | None = None
        self.cfast_process: QProcess | None = None
        self.cfast_process_context = ""
        self.cfast_output_text = ""
        self.cfast_status_path: Path | None = None
        self.cfast_stop_path: Path | None = None
        self.cfast_query_path: Path | None = None
        self.cfast_last_status_text = ""
        self.cfast_last_status_line = ""
        self.launch_smokeview_after_cfast = False
        self.extra_namelists: list[str] = []
        self.main_toolbar: QToolBar | None = None
        self.run_button: QPushButton | None = None
        self.stop_button: QPushButton | None = None
        self.run_monitor_dialog: RunMonitorDialog | None = None
        self.cfast_status_timer = QTimer(self)
        self.cfast_status_timer.setInterval(250)
        self.cfast_status_timer.timeout.connect(self.poll_cfast_status)
        self.cfast_query_timer = QTimer(self)
        self.cfast_query_timer.setInterval(1000)
        self.cfast_query_timer.timeout.connect(self.request_cfast_status_update)

        self.settings = QSettings("FireModels", "CEditQt")
        unit_system.load_settings(self.settings)
        self.cfast_executable = self.settings.value("cfast_executable", "", type=str)
        self.smokeview_executable = self.settings.value(
            "smokeview_executable",
            "",
            type=str,
        )

        self.simulation_tab = SimulationTab()
        self.thermal_properties_tab = ThermalPropertiesTab()
        self.compartments_tab = CompartmentsTab()
        self.compartments_tab.materials_referenced.connect(
            self.add_missing_default_materials
        )
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
        self.load_case(CfastCase())

    def build_menu(self):
        self.menuBar().setNativeMenuBar(False)

        file_menu = self.menuBar().addMenu("&File")

        open_action = QAction("&Open...", self)
        open_action.triggered.connect(self.open_cfast_input)
        file_menu.addAction(open_action)

        save_action = QAction("&Save", self)
        save_action.triggered.connect(self.save_cfast_input)
        file_menu.addAction(save_action)

        save_as_action = QAction("Save &As...", self)
        save_as_action.triggered.connect(self.save_cfast_input_as)
        file_menu.addAction(save_as_action)

        file_menu.addSeparator()

        load_example_action = QAction("Load &Example...", self)
        load_example_action.triggered.connect(self.load_single_compartment_example)
        file_menu.addAction(load_example_action)

        file_menu.addSeparator()

        set_cfast_action = QAction("Set &CFAST Executable...", self)
        set_cfast_action.triggered.connect(self.set_cfast_executable)
        file_menu.addAction(set_cfast_action)

        clear_cfast_action = QAction("Use CFAST from &PATH", self)
        clear_cfast_action.triggered.connect(self.clear_cfast_executable)
        file_menu.addAction(clear_cfast_action)

        set_smokeview_action = QAction("Set S&mokeview Executable...", self)
        set_smokeview_action.triggered.connect(self.set_smokeview_executable)
        file_menu.addAction(set_smokeview_action)

        clear_smokeview_action = QAction("Use Smokeview from PA&TH", self)
        clear_smokeview_action.triggered.connect(self.clear_smokeview_executable)
        file_menu.addAction(clear_smokeview_action)

        file_menu.addSeparator()

        exit_action = QAction("E&xit", self)
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)

        view_menu = self.menuBar().addMenu("&View")

        units_action = QAction("Select Engineering &Units...", self)
        units_action.triggered.connect(self.select_engineering_units)
        view_menu.addAction(units_action)

        view_menu.addSeparator()

        show_toolbar_action = QAction("Show &Toolbar", self)
        show_toolbar_action.setCheckable(True)
        show_toolbar_action.setChecked(
            self.settings.value("show_toolbar", False, type=bool)
        )
        show_toolbar_action.toggled.connect(self.set_toolbar_visible)
        view_menu.addAction(show_toolbar_action)

        view_menu.addSeparator()

        geometry_action = QAction("&Geometry", self)
        geometry_action.triggered.connect(self.generate_smokeview_geometry)
        view_menu.addAction(geometry_action)

        results_action = QAction("&Results", self)
        results_action.triggered.connect(self.view_results)
        view_menu.addAction(results_action)

        help_menu = self.menuBar().addMenu("&Help")

        about_action = QAction("&About CFAST Editor (CEdit)", self)
        about_action.triggered.connect(self.about)
        help_menu.addAction(about_action)

        self.open_action = open_action
        self.save_action = save_action
        self.save_as_action = save_as_action
        self.load_example_action = load_example_action
        self.set_cfast_action = set_cfast_action
        self.clear_cfast_action = clear_cfast_action
        self.set_smokeview_action = set_smokeview_action
        self.clear_smokeview_action = clear_smokeview_action
        self.exit_action = exit_action
        self.units_action = units_action
        self.show_toolbar_action = show_toolbar_action
        self.geometry_action = geometry_action
        self.results_action = results_action

    def build_toolbar(self):
        toolbar = QToolBar("Main Toolbar")
        self.main_toolbar = toolbar
        self.addToolBar(toolbar)
        toolbar.addAction(self.open_action)
        toolbar.addAction(self.save_action)
        toolbar.addAction(self.save_as_action)
        toolbar.addSeparator()
        toolbar.addAction(self.geometry_action)
        toolbar.addAction(self.results_action)
        toolbar.addSeparator()
        toolbar.addAction(self.exit_action)
        toolbar.visibilityChanged.connect(self.on_toolbar_visibility_changed)
        toolbar.setVisible(self.show_toolbar_action.isChecked())

    def set_toolbar_visible(self, visible: bool):
        if self.main_toolbar is not None:
            self.main_toolbar.setVisible(visible)
        self.settings.setValue("show_toolbar", visible)

    def on_toolbar_visibility_changed(self, visible: bool):
        if self.show_toolbar_action.isChecked() != visible:
            self.show_toolbar_action.setChecked(visible)
        self.settings.setValue("show_toolbar", visible)

    def select_engineering_units(self):
        try:
            case = self.build_cfast_case(require_compartments=False)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Engineering Units", str(exc))
            return

        dialog = EngineeringUnitsDialog(self)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return

        unit_system.set_indices(dialog.selected_indices())
        unit_system.save_settings(self.settings)
        self.load_case(case)
        self.simulation_tab.set_message("Engineering units updated.")
        self.statusBar().showMessage("No Errors")

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
        self.tabs.currentChanged.connect(self.refresh_reference_lists)

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
        stop_button = QPushButton("Stop")
        stop_button.setEnabled(False)
        view_button = QPushButton("View")

        open_button.clicked.connect(self.open_cfast_input)
        save_button.clicked.connect(self.save_cfast_input)
        geometry_button.clicked.connect(self.generate_smokeview_geometry)
        run_button.clicked.connect(self.run_cfast)
        stop_button.clicked.connect(self.stop_cfast)
        view_button.clicked.connect(self.view_results)

        self.run_button = run_button
        self.stop_button = stop_button

        row.addWidget(open_button)
        row.addWidget(save_button)
        row.addItem(QSpacerItem(40, 1))
        row.addWidget(geometry_button)
        row.addWidget(run_button)
        row.addWidget(stop_button)
        row.addItem(QSpacerItem(40, 1))
        row.addWidget(view_button)
        row.addStretch(1)

        return row

    def build_cfast_case(self, require_compartments: bool = True):
        case = CfastCase()
        case.extra_namelists = list(self.extra_namelists)
        self.simulation_tab.add_to_case(case)
        self.thermal_properties_tab.add_to_case(case)
        self.compartments_tab.add_to_case(
            case,
            require_compartments=require_compartments,
        )
        self.wall_vents_tab.add_to_case(case)
        self.ceiling_floor_vents_tab.add_to_case(case)
        self.mechanical_vents_tab.add_to_case(case)
        self.targets_tab.add_to_case(case)
        self.detection_suppression_tab.add_to_case(case)
        self.surface_connections_tab.add_to_case(case)
        self.output_tab.add_to_case(case)
        self.fires_tab.add_to_case(case)
        return case

    def current_compartment_ids(self) -> list[str]:
        return [
            compartment.id
            for compartment in self.compartments_tab.compartments
            if compartment.id
        ]

    def current_target_ids(self) -> list[str]:
        return [target.id for target in self.targets_tab.targets if target.id]

    def current_material_ids(self) -> list[str]:
        return self.thermal_properties_tab.material_ids()

    def refresh_reference_lists(self, *_args):
        compartment_ids = self.current_compartment_ids()
        material_ids = self.current_material_ids()
        target_ids = self.current_target_ids()

        self.compartments_tab.set_material_ids(material_ids)
        self.wall_vents_tab.set_compartment_ids(compartment_ids)
        self.ceiling_floor_vents_tab.set_compartment_ids(compartment_ids)
        self.mechanical_vents_tab.set_compartment_ids(compartment_ids)
        self.fires_tab.set_compartment_ids(compartment_ids)
        self.fires_tab.set_target_ids(target_ids)
        self.targets_tab.set_compartment_ids(compartment_ids)
        self.targets_tab.set_material_ids(material_ids)
        self.detection_suppression_tab.set_compartment_ids(compartment_ids)
        self.output_tab.set_compartment_ids(compartment_ids)

    def add_missing_default_materials(self, material_ids: list[str]):
        added_material = False
        for material_id in material_ids:
            if material_id.upper() != "CONCRETE":
                continue
            if self.thermal_properties_tab.has_material_id(material_id):
                continue

            self.thermal_properties_tab.add_material_property(default_concrete_material())
            added_material = True

        if added_material:
            self.refresh_reference_lists()

    def save_cfast_input(self):
        if self.current_path is None:
            self.save_cfast_input_as()
            return

        written_path = self.write_case_to_path(self.current_path)
        if written_path is not None:
            self.current_path = written_path

    def save_cfast_input_as(self):
        path_text, _ = QFileDialog.getSaveFileName(
            self,
            "Save As",
            "cedit_qt_test.in",
            "CFAST input files (*.in);;All files (*)",
        )

        if not path_text:
            return

        written_path = self.write_case_to_path(Path(path_text))
        if written_path is not None:
            self.current_path = written_path

    def sanitize_path_for_write(self, path: Path) -> Path | None:
        sanitized_path = sanitize_cfast_input_path(path)
        if sanitized_path == path:
            return path

        if sanitized_path.exists():
            response = QMessageBox.question(
                self,
                "CFAST Input File Name",
                "CFAST input file names cannot contain blanks.\n\n"
                f"CEdit Qt will use:\n{sanitized_path}\n\n"
                "This file already exists. Overwrite it?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No,
            )
            if response != QMessageBox.StandardButton.Yes:
                return None

        return sanitized_path

    def write_case_to_path(self, path: Path) -> Path | None:
        original_path = path
        path = self.sanitize_path_for_write(path)
        if path is None:
            return None

        try:
            case = self.build_cfast_case()
            write_cfast_input(case, path)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Save failed", str(exc))
            return None

        message = f"Wrote CFAST input file:\n{path}"
        if path != original_path:
            message = (
                "CFAST input file names cannot contain blanks.\n"
                f"Using:\n{path}\n\n"
                f"{message}"
            )
        self.simulation_tab.set_message(message)
        self.statusBar().showMessage("No Errors")
        QMessageBox.information(self, "Save complete", message)
        return path

    def open_cfast_input(self):
        path_text, _ = QFileDialog.getOpenFileName(
            self,
            "Open CFAST Input",
            str(Path.cwd()),
            "CFAST input files (*.in);;All files (*)",
        )

        if not path_text:
            return

        self.load_cfast_input(Path(path_text))

    def load_single_compartment_example(self):
        self.current_path = None
        self.load_case(single_compartment_example())
        self.simulation_tab.set_message(
            "Loaded example: Single Compartment Example.\n"
            "Use File > Save As... to write a CFAST input file."
        )
        self.statusBar().showMessage("No Errors")

    def load_cfast_input(self, path: Path):
        try:
            result = read_cfast_input_with_warnings(path)
            self.load_case(result.case)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, "Open failed", str(exc))
            return

        self.current_path = path
        message = f"Loaded CFAST input file:\n{path}"
        if result.warnings:
            message += "\n\nImport warnings:\n" + "\n".join(result.warnings)

        self.simulation_tab.set_message(message)
        self.statusBar().showMessage("No Errors")

    def load_case(self, case: CfastCase):
        self.extra_namelists = list(getattr(case, "extra_namelists", []))
        self.simulation_tab.load_case(case)
        self.thermal_properties_tab.load_case(case)
        self.compartments_tab.load_case(case)
        self.wall_vents_tab.load_case(case)
        self.ceiling_floor_vents_tab.load_case(case)
        self.mechanical_vents_tab.load_case(case)
        self.targets_tab.load_case(case)
        self.detection_suppression_tab.load_case(case)
        self.surface_connections_tab.load_case(case)
        self.output_tab.load_case(case)
        self.fires_tab.load_case(case)
        self.refresh_reference_lists()

    def get_cfast_executable(self) -> str:
        if self.cfast_executable:
            return self.cfast_executable

        return "cfast"

    def get_smokeview_executable(self) -> str:
        if self.smokeview_executable:
            return self.smokeview_executable

        return "smokeview"

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

    def set_smokeview_executable(self):
        path_text, _ = QFileDialog.getOpenFileName(
            self,
            "Select Smokeview Executable",
            str(Path.home()),
            "All files (*)",
        )

        if not path_text:
            return

        path = Path(path_text).expanduser()

        if not path.is_file():
            QMessageBox.critical(
                self,
                "Smokeview Executable",
                f"Not a file:\n{path}",
            )
            return

        self.smokeview_executable = str(path)
        self.settings.setValue("smokeview_executable", self.smokeview_executable)

        self.simulation_tab.set_message(
            f"Smokeview executable set to:\n{self.smokeview_executable}"
        )
        self.statusBar().showMessage("No Errors")

    def clear_smokeview_executable(self):
        self.smokeview_executable = ""
        self.settings.remove("smokeview_executable")

        self.simulation_tab.set_message(
            "Smokeview executable override cleared.\n"
            "CEdit Qt will run 'smokeview' from PATH."
        )
        self.statusBar().showMessage("No Errors")

    def cfast_is_running(self) -> bool:
        return (
            self.cfast_process is not None
            and self.cfast_process.state() != QProcess.ProcessState.NotRunning
        )

    def prepare_case_for_execution(
        self,
        save_dialog_title: str,
        error_title: str,
    ) -> tuple[Path, CfastCase] | None:
        path = self.current_path

        if path is None:
            path_text, _ = QFileDialog.getSaveFileName(
                self,
                save_dialog_title,
                "cedit_qt_test.in",
                "CFAST input files (*.in);;All files (*)",
            )

            if not path_text:
                return None

            path = Path(path_text)

        original_path = path
        path = self.sanitize_path_for_write(path)
        if path is None:
            return None

        try:
            case = self.build_cfast_case()
            write_cfast_input(case, path)
        except Exception as exc:
            self.simulation_tab.set_message(str(exc))
            self.statusBar().showMessage("Errors")
            QMessageBox.critical(self, error_title, str(exc))
            return None

        self.current_path = path
        if path != original_path:
            self.simulation_tab.set_message(
                "CFAST input file names cannot contain blanks.\n"
                f"Using:\n{path}\n"
            )
        return path, case

    def run_cfast(self):
        if self.cfast_is_running():
            QMessageBox.information(
                self,
                "Run",
                "CFAST is already running.",
            )
            return

        prepared = self.prepare_case_for_execution(
            "Save CFAST Input Before Running",
            "Run failed",
        )
        if prepared is None:
            return

        path, case = prepared

        self.simulation_tab.set_message(
            f"Wrote CFAST input file:\n{path}\n\n"
            f"Running CFAST...\n\n"
        )
        self.statusBar().showMessage("Running CFAST")

        cfast_args = [path.name]
        if abs(case.simulation_time) < 1.0e-12:
            cfast_args.append("-I")

        self.start_cfast_process(
            path,
            cfast_args,
            "run",
            simulation_time=case.simulation_time,
        )

    def start_cfast_process(
        self,
        path: Path,
        cfast_args: list[str],
        context: str,
        launch_smokeview_after_cfast: bool = False,
        simulation_time: float | None = None,
    ):
        self.cfast_process = QProcess(self)
        self.cfast_process.setWorkingDirectory(str(path.parent))
        self.cfast_process_context = context
        self.cfast_output_text = ""
        self.cfast_status_path = path.with_suffix(".status")
        self.cfast_stop_path = path.with_suffix(".stop")
        self.cfast_query_path = path.with_suffix(".query")
        self.cfast_last_status_text = ""
        self.cfast_last_status_line = ""
        self.launch_smokeview_after_cfast = launch_smokeview_after_cfast
        self.remove_stale_run_control_files()

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
        self.simulation_tab.append_message(
            f"Arguments: {' '.join(cfast_args)}\n\n"
        )
        if context == "run" and "-I" in cfast_args:
            self.simulation_tab.append_message(
                "Simulation Time is 0; running initialization only (-I).\n\n"
            )
        elif context == "geometry":
            self.simulation_tab.append_message(
                "Running initialization only (-I) to generate Smokeview geometry.\n\n"
            )
        if context == "run":
            self.run_monitor_dialog = RunMonitorDialog(
                self,
                path.name,
                simulation_time or 0.0,
            )
            self.run_monitor_dialog.stop_requested.connect(self.stop_cfast)
            self.run_monitor_dialog.update_requested.connect(
                self.request_cfast_status_update
            )
            self.run_monitor_dialog.show()
            self.cfast_query_timer.start()
        self.set_cfast_running_ui(True)
        self.cfast_status_timer.start()
        self.cfast_process.start(cfast_exe, cfast_args)

    def remove_stale_run_control_files(self):
        for path in (
            self.cfast_status_path,
            self.cfast_stop_path,
            self.cfast_query_path,
        ):
            if path is None:
                continue
            try:
                path.unlink()
            except FileNotFoundError:
                pass
            except OSError:
                pass

    def set_cfast_running_ui(self, running: bool):
        if self.run_button is not None:
            self.run_button.setEnabled(not running)
        if self.stop_button is not None:
            self.stop_button.setEnabled(running)

    def stop_cfast(self):
        if not self.cfast_is_running():
            return

        if self.cfast_stop_path is not None:
            try:
                self.cfast_stop_path.write_text("", encoding="utf-8")
                self.simulation_tab.append_message(
                    f"\nStop requested. Wrote:\n{self.cfast_stop_path}\n"
                )
                if self.run_monitor_dialog is not None:
                    self.run_monitor_dialog.set_stopping()
                self.statusBar().showMessage("Stopping CFAST")
                return
            except OSError as exc:
                self.simulation_tab.append_message(
                    f"\nCould not write CFAST stop file: {exc}\n"
                )

        if self.cfast_process is not None:
            self.cfast_process.terminate()
            self.simulation_tab.append_message("\nStop requested. Terminating CFAST.\n")
            if self.run_monitor_dialog is not None:
                self.run_monitor_dialog.set_stopping()
            self.statusBar().showMessage("Stopping CFAST")

    def request_cfast_status_update(self):
        if not self.cfast_is_running() or self.cfast_query_path is None:
            return

        try:
            self.cfast_query_path.write_text("", encoding="utf-8")
        except OSError as exc:
            self.simulation_tab.append_message(
                f"\nCould not write CFAST query file: {exc}\n"
            )

    def poll_cfast_status(self):
        if self.cfast_status_path is None or not self.cfast_status_path.exists():
            return

        try:
            text = self.cfast_status_path.read_text(
                encoding="utf-8",
                errors="replace",
            ).strip()
        except OSError:
            return

        if not text:
            return

        first_line = text.splitlines()[0] if text.splitlines() else "CFAST running"
        self.statusBar().showMessage(first_line)
        self.append_cfast_status_line(first_line)

        if self.status_text_is_partial(text):
            if self.run_monitor_dialog is not None:
                self.run_monitor_dialog.update_status(text)
            return

        if text == self.cfast_last_status_text:
            return

        self.cfast_last_status_text = text
        if self.run_monitor_dialog is not None:
            self.run_monitor_dialog.update_status(text)

    def append_cfast_status_line(self, line: str):
        if not line or line == self.cfast_last_status_line:
            return

        self.cfast_last_status_line = line
        if line.startswith("Status"):
            self.simulation_tab.append_message(f"{line}\n")

    def format_cfast_status(self, text: str) -> str:
        return text.rstrip()

    def status_text_is_partial(self, text: str) -> bool:
        lines = text.splitlines()
        return (
            self.cfast_is_running()
            and len(lines) < 7
            and bool(lines)
            and lines[0].startswith("Status")
        )

    def cfast_ready_read_stdout(self):
        if self.cfast_process is None:
            return

        text = bytes(self.cfast_process.readAllStandardOutput()).decode(
            "utf-8",
            errors="replace",
        )

        self.simulation_tab.append_message(text)
        self.cfast_output_text += text

    def cfast_ready_read_stderr(self):
        if self.cfast_process is None:
            return

        text = bytes(self.cfast_process.readAllStandardError()).decode(
            "utf-8",
            errors="replace",
        )

        self.simulation_tab.append_message(text)
        self.cfast_output_text += text

    def cfast_finished(self, exit_code, exit_status):
        context = self.cfast_process_context
        launch_smokeview = self.launch_smokeview_after_cfast
        path = self.current_path
        self.poll_cfast_status()

        if exit_status == QProcess.ExitStatus.NormalExit and exit_code == 0:
            if context == "geometry":
                self.simulation_tab.append_message(
                    f"\nSmokeview geometry generated with exit code {exit_code}.\n"
                )
            else:
                self.simulation_tab.append_message(
                    f"\nCFAST completed successfully with exit code {exit_code}.\n"
                )
            self.statusBar().showMessage("No Errors")
        else:
            self.simulation_tab.append_message(
                f"\nCFAST finished with exit code {exit_code}.\n"
            )
            hint = self.cfast_output_compatibility_hint()
            if hint:
                self.simulation_tab.append_message(hint)
            self.statusBar().showMessage("Errors")
            launch_smokeview = False

        if self.run_monitor_dialog is not None:
            self.run_monitor_dialog.set_finished(
                exit_status == QProcess.ExitStatus.NormalExit and exit_code == 0,
                self.run_error_text(path, exit_code),
            )

        self.cfast_process = None
        self.cfast_process_context = ""
        self.cfast_status_path = None
        self.cfast_stop_path = None
        self.cfast_query_path = None
        self.cfast_status_timer.stop()
        self.cfast_query_timer.stop()
        self.set_cfast_running_ui(False)
        self.launch_smokeview_after_cfast = False

        if launch_smokeview and path is not None:
            self.launch_smokeview(path)

    def run_error_text(self, path: Path | None, exit_code: int) -> str:
        messages: list[str] = []
        if path is not None:
            log_path = path.with_suffix(".log")
            try:
                log_text = log_path.read_text(encoding="utf-8", errors="replace").strip()
            except OSError:
                log_text = ""
            if log_text:
                messages.append(log_text)

        if self.cfast_output_text.strip():
            messages.append(self.cfast_output_text.strip())

        if exit_code != 0:
            messages.append(f"CFAST exited with a non-zero exit code: {exit_code}")

        return "\n\n".join(messages) if messages else "No Errors"

    def cfast_output_compatibility_hint(self) -> str:
        text = self.cfast_output_text.upper()
        if "INVALID SPECIFICATION IN &OUTP" not in text:
            return ""

        if not any(
            field in text
            for field in (
                "NET_HEAT_FLUX_OUTPUT",
                "VALIDATION_OUTPUT",
                "SPREADSHEET_OUTPUT",
            )
        ):
            return ""

        return (
            "\nCEdit Qt wrote an &OUTP option that this CFAST executable "
            "does not recognize. Rebuild CFAST from the current source, or "
            "choose the current executable with File > Set CFAST Executable...\n"
        )

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
        if error == QProcess.ProcessError.FailedToStart:
            if self.run_monitor_dialog is not None:
                self.run_monitor_dialog.set_finished(
                    False,
                    f"CFAST process error: {error}",
                )
            self.cfast_process = None
            self.cfast_process_context = ""
            self.cfast_status_path = None
            self.cfast_stop_path = None
            self.cfast_query_path = None
            self.cfast_status_timer.stop()
            self.cfast_query_timer.stop()
            self.set_cfast_running_ui(False)
            self.launch_smokeview_after_cfast = False

    def generate_smokeview_geometry(self):
        if self.cfast_is_running():
            QMessageBox.information(
                self,
                "Geometry",
                "CFAST is already running.",
            )
            return

        prepared = self.prepare_case_for_execution(
            "Save CFAST Input Before Generating Geometry",
            "Geometry failed",
        )
        if prepared is None:
            return

        path, _case = prepared

        self.simulation_tab.set_message(
            f"Wrote CFAST input file:\n{path}\n\n"
            "Generating Smokeview geometry...\n\n"
        )
        self.statusBar().showMessage("Generating Smokeview geometry")
        self.start_cfast_process(path, [path.name, "-I"], "geometry")

    def view_results(self):
        if self.cfast_is_running():
            QMessageBox.information(
                self,
                "View",
                "CFAST is already running.",
            )
            return

        prepared = self.prepare_case_for_execution(
            "Save CFAST Input Before Viewing",
            "View failed",
        )
        if prepared is None:
            return

        path, _case = prepared

        if self.smokeview_geometry_needs_update(path):
            self.simulation_tab.set_message(
                f"Wrote CFAST input file:\n{path}\n\n"
                "Smokeview geometry is missing or older than the input file.\n\n"
                "Generating Smokeview geometry...\n\n"
            )
            self.statusBar().showMessage("Generating Smokeview geometry")
            self.start_cfast_process(
                path,
                [path.name, "-I"],
                "geometry",
                launch_smokeview_after_cfast=True,
            )
            return

        self.launch_smokeview(path)

    def smokeview_geometry_needs_update(self, path: Path) -> bool:
        smv_path = path.with_suffix(".smv")

        if not smv_path.exists():
            return True

        try:
            return smv_path.stat().st_mtime < path.stat().st_mtime
        except OSError:
            return True

    def launch_smokeview(self, path: Path):
        smokeview_exe = self.get_smokeview_executable()
        smokeview_case = path.with_suffix("").name
        result = QProcess.startDetached(
            smokeview_exe,
            [smokeview_case],
            str(path.parent),
        )
        started = result[0] if isinstance(result, tuple) else result

        if started:
            self.simulation_tab.append_message(
                f"\nStarted Smokeview:\n{smokeview_exe}\n\n"
                f"Case: {smokeview_case}\n"
            )
            self.statusBar().showMessage("No Errors")
            return

        message = (
            "Smokeview failed to start.\n\n"
            f"Attempted executable:\n{smokeview_exe}\n\n"
            "If this is a development build, use:\n"
            "File > Set Smokeview Executable...\n"
            "and select your Smokeview executable."
        )
        self.simulation_tab.append_message(f"\n{message}\n")
        self.statusBar().showMessage("Errors")
        QMessageBox.critical(self, "Smokeview", message)

    def about(self):
        cfast_version = latest_cfast_version_from_tag()
        if cfast_version is None:
            version_text = "CFAST version: Unknown"
        else:
            version, tag = cfast_version
            version_text = f"CFAST version: {version} ({tag})"

        QMessageBox.information(
            self,
            "About CFAST Editor (CEdit)",
            "Python/PySide6 front end for editing and running CFAST input files.\n\n"
            f"{version_text}",
        )


class EngineeringUnitsDialog(QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.setWindowTitle("Select Engineering Units")
        self.combos: dict[str, QComboBox] = {}

        layout = QFormLayout()
        for key in BASE_UNIT_KEYS:
            combo = QComboBox()
            combo.addItems(unit_system.choices(key))
            combo.setCurrentIndex(unit_system.selected[key])
            self.combos[key] = combo
            layout.addRow(f"{BASE_UNIT_NAMES[key]}:", combo)

        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok
            | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        self.setLayout(layout)

    def selected_indices(self) -> dict[str, int]:
        return {key: combo.currentIndex() for key, combo in self.combos.items()}


class RunMonitorDialog(QDialog):
    stop_requested = Signal()
    update_requested = Signal()

    STATUS_RE = re.compile(
        r"Status\s+at\s+T\s*=\s*"
        r"(?P<time>[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][-+]?\d+)?)"
        r"\s+DT\s*=\s*"
        r"(?P<dt>[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][-+]?\d+)?)"
    )

    def __init__(self, parent, case_name: str, simulation_time: float):
        super().__init__(parent)

        self.simulation_time = simulation_time
        self.running = True
        self.setWindowTitle(f"Run Model ({case_name})")
        self.setWindowModality(Qt.WindowModality.NonModal)
        self.resize(980, 560)

        self.dt_edit = QLineEdit("0")
        self.dt_edit.setReadOnly(True)
        self.time_edit = QLineEdit("0")
        self.time_edit.setReadOnly(True)
        self.progress_bar = QProgressBar()
        self.progress_bar.setRange(0, 100)

        top_layout = QGridLayout()
        top_layout.addWidget(QLabel("Current Time Step:"), 0, 0)
        top_layout.addWidget(self.dt_edit, 0, 1)
        top_layout.addWidget(QLabel("Simulation Time:"), 0, 2)
        top_layout.addWidget(self.time_edit, 0, 3)
        top_layout.addWidget(QLabel("Progress:"), 0, 4)
        top_layout.addWidget(self.progress_bar, 0, 5)
        top_layout.setColumnStretch(5, 1)

        self.summary_table = QTableWidget(0, 7)
        self.summary_table.setHorizontalHeaderLabels(self.summary_headers())
        self.summary_table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
        self.summary_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.summary_table.verticalHeader().setVisible(False)

        self.errors_edit = QPlainTextEdit("No Errors")
        self.errors_edit.setReadOnly(True)
        self.errors_edit.setMinimumHeight(120)

        self.close_button = QPushButton("Close")
        self.stop_button = QPushButton("Stop")
        self.update_button = QPushButton("Update")

        self.close_button.setEnabled(False)
        self.close_button.clicked.connect(self.accept)
        self.stop_button.clicked.connect(self.stop_requested.emit)
        self.update_button.clicked.connect(self.update_requested.emit)

        button_layout = QHBoxLayout()
        button_layout.addStretch(1)
        button_layout.addWidget(self.close_button)
        button_layout.addWidget(self.stop_button)
        button_layout.addWidget(self.update_button)
        button_layout.addStretch(1)

        layout = QVBoxLayout()
        layout.addLayout(top_layout)
        layout.addWidget(self.summary_table, 1)
        layout.addWidget(self.errors_edit)
        layout.addLayout(button_layout)
        self.setLayout(layout)

    def summary_headers(self) -> list[str]:
        return [
            "Compartment",
            f"Upper Layer\nTemperature\n({unit_label(TEMPERATURE)})",
            f"Lower Layer\nTemperature\n({unit_label(TEMPERATURE)})",
            f"Interface\nHeight\n({unit_label(LENGTH)})",
            f"Pyrolysis\nRate\n({unit_label(MASS_LOSS)})",
            f"Fire\nSize\n({unit_label(HRR)})",
            f"Pressure\n({unit_label(PRESSURE)})",
        ]

    def update_status(self, text: str):
        lines = text.splitlines()
        if not lines:
            return

        self.update_time_fields(lines[0])
        self.update_summary_rows(lines[1:])

    def update_time_fields(self, line: str):
        match = self.STATUS_RE.search(line)
        if match is None:
            return

        time = self.parse_status_number(match.group("time"))
        dt = self.parse_status_number(match.group("dt"))
        if self.simulation_time > 0:
            progress = max(0, min(round(time / self.simulation_time * 100), 100))
            self.progress_bar.setValue(progress)

        self.time_edit.setText(format_value(TIME, time))
        if self.running:
            self.dt_edit.setText(format_value(TIME, dt))

    def update_summary_rows(self, lines: list[str]):
        rows = [self.parse_compact_row(line) for line in lines]
        rows = [row for row in rows if row is not None]
        if not rows:
            return

        self.summary_table.setRowCount(len(rows))
        for row_index, values in enumerate(rows):
            for col, value in enumerate(values):
                item = QTableWidgetItem(value)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.summary_table.setItem(row_index, col, item)

    def parse_compact_row(self, line: str) -> list[str] | None:
        if not line.strip() or set(line.strip()) == {"-"}:
            return None

        values = line.split()
        if not values:
            return None

        if values[0].lower() == "outside":
            fire_size = self.format_status_value(HRR, values[-1]) if len(values) > 1 else ""
            return ["Outside", "", "", "", "", fire_size, ""]

        if not values[0].lstrip("+-").isdigit():
            return None

        if len(values) >= 7:
            return [
                values[0],
                self.format_status_value(TEMPERATURE, values[1]),
                self.format_status_value(TEMPERATURE, values[2]),
                self.format_status_value(LENGTH, values[3]),
                self.format_status_value(MASS_LOSS, values[4]),
                self.format_status_value(HRR, values[5]),
                self.format_status_value(PRESSURE, values[6]),
            ]

        if len(values) >= 5:
            return [
                values[0],
                self.format_status_value(TEMPERATURE, values[1]),
                "",
                "",
                self.format_status_value(MASS_LOSS, values[2]),
                self.format_status_value(HRR, values[3]),
                self.format_status_value(PRESSURE, values[4]),
            ]

        return None

    def format_status_value(self, kind: str, text: str) -> str:
        try:
            return format_value(kind, self.parse_status_number(text), include_unit=False)
        except ValueError:
            return ""

    @staticmethod
    def parse_status_number(text: str) -> float:
        return float(text.replace("D", "E").replace("d", "e"))

    def set_stopping(self):
        self.dt_edit.setText("Stopping")
        self.stop_button.setEnabled(False)
        self.update_button.setEnabled(False)

    def set_finished(self, success: bool, errors: str):
        self.running = False
        self.dt_edit.setText("CFAST finished" if success else "CFAST stopped")
        self.close_button.setEnabled(True)
        self.stop_button.setEnabled(False)
        self.update_button.setEnabled(False)
        self.errors_edit.setPlainText(errors or "No Errors")

    def closeEvent(self, event):
        if self.running:
            event.ignore()
            return
        super().closeEvent(event)

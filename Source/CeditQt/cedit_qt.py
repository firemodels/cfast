import sys

from PySide6.QtCore import Qt
from PySide6.QtGui import QAction
from PySide6.QtWidgets import (
   QApplication,
   QDockWidget,
   QFrame,
   QHBoxLayout,
   QHeaderView,
   QLabel,
   QListWidget,
   QMainWindow,
   QPushButton,
   QSizePolicy,
   QSplitter,
   QStatusBar,
   QTabWidget,
   QTableWidget,
   QTableWidgetItem,
   QToolBar,
   QVBoxLayout,
   QWidget,
)

from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure


class RampPlotCanvas(FigureCanvas):
   def __init__(self, parent=None):
      self.figure = Figure(figsize=(5, 4), tight_layout=True)
      self.ax = self.figure.add_subplot(111)
      super().__init__(self.figure)
      self.setParent(parent)
      self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
      self.updateGeometry()

   def plot_ramp(self, points):
      self.ax.clear()

      if points:
         times = [p[0] for p in points]
         hrrs = [p[1] for p in points]
         self.ax.plot(times, hrrs, marker='o')
         self.ax.set_xlim(left=0.0)
         self.ax.set_ylim(bottom=0.0)
      else:
         self.ax.text(
            0.5, 0.5,
            "Enter time / HRR points",
            ha='center', va='center',
            transform=self.ax.transAxes
         )
         self.ax.set_xlim(0.0, 1.0)
         self.ax.set_ylim(0.0, 1.0)

      self.ax.set_title("HRR Ramp")
      self.ax.set_xlabel("Time (s)")
      self.ax.set_ylabel("HRR (kW)")
      self.ax.grid(True)
      self.draw()


class FiresTab(QWidget):
   def __init__(self, parent=None):
      super().__init__(parent)

      self.table = QTableWidget(8, 2)
      self.table.setHorizontalHeaderLabels(["Time (s)", "HRR (kW)"])
      self.table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
      self.table.verticalHeader().setVisible(True)

      self.plot_canvas = RampPlotCanvas()

      self.info_label = QLabel(
         "Enter a fire ramp as time / HRR pairs.\n"
         "The plot updates whenever the table changes."
      )
      self.info_label.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)

      add_row_button = QPushButton("Add Row")
      delete_row_button = QPushButton("Delete Selected")
      clear_button = QPushButton("Clear")

      add_row_button.clicked.connect(self.add_row)
      delete_row_button.clicked.connect(self.delete_selected_rows)
      clear_button.clicked.connect(self.clear_table)
      self.table.cellChanged.connect(self.update_plot)

      left_layout = QVBoxLayout()
      left_layout.addWidget(QLabel("<b>Fire Ramp Input</b>"))
      left_layout.addWidget(self.info_label)
      left_layout.addWidget(self.table)

      button_layout = QHBoxLayout()
      button_layout.addWidget(add_row_button)
      button_layout.addWidget(delete_row_button)
      button_layout.addWidget(clear_button)
      left_layout.addLayout(button_layout)

      left_widget = QWidget()
      left_widget.setLayout(left_layout)

      right_layout = QVBoxLayout()
      right_layout.addWidget(QLabel("<b>Ramp Preview</b>"))
      right_layout.addWidget(self.plot_canvas)

      right_widget = QWidget()
      right_widget.setLayout(right_layout)

      splitter = QSplitter(Qt.Orientation.Horizontal)
      splitter.addWidget(left_widget)
      splitter.addWidget(right_widget)
      splitter.setSizes([350, 650])

      main_layout = QVBoxLayout()
      main_layout.addWidget(splitter)
      self.setLayout(main_layout)

      self.load_demo_data()
      self.update_plot()

   def load_demo_data(self):
      demo_points = [
         (0, 0),
         (60, 250),
         (120, 750),
         (180, 1200),
         (240, 1200),
         (300, 600),
         (360, 0),
      ]

      self.table.blockSignals(True)
      self.table.setRowCount(max(8, len(demo_points)))

      for row, (time_val, hrr_val) in enumerate(demo_points):
         self.table.setItem(row, 0, QTableWidgetItem(str(time_val)))
         self.table.setItem(row, 1, QTableWidgetItem(str(hrr_val)))

      self.table.blockSignals(False)

   def add_row(self):
      self.table.insertRow(self.table.rowCount())

   def delete_selected_rows(self):
      selected_rows = sorted(
         {index.row() for index in self.table.selectionModel().selectedIndexes()},
         reverse=True
      )

      for row in selected_rows:
         self.table.removeRow(row)

      if self.table.rowCount() == 0:
         self.table.setRowCount(1)

      self.update_plot()

   def clear_table(self):
      self.table.blockSignals(True)
      self.table.clearContents()
      self.table.blockSignals(False)
      self.update_plot()

   def extract_points(self):
      points = []

      for row in range(self.table.rowCount()):
         time_item = self.table.item(row, 0)
         hrr_item = self.table.item(row, 1)

         if time_item is None or hrr_item is None:
            continue

         time_text = time_item.text().strip()
         hrr_text = hrr_item.text().strip()

         if not time_text or not hrr_text:
            continue

         try:
            time_val = float(time_text)
            hrr_val = float(hrr_text)
         except ValueError:
            continue

         points.append((time_val, hrr_val))

      points.sort(key=lambda p: p[0])
      return points

   def update_plot(self):
      points = self.extract_points()
      self.plot_canvas.plot_ramp(points)


class PlaceholderTab(QWidget):
   def __init__(self, text, parent=None):
      super().__init__(parent)

      label = QLabel(text)
      label.setAlignment(Qt.AlignmentFlag.AlignCenter)

      layout = QVBoxLayout()
      layout.addWidget(label)
      self.setLayout(layout)


class CeditMainWindow(QMainWindow):
   def __init__(self):
      super().__init__()

      self.setWindowTitle("Cedit Qt Prototype")
      self.resize(1200, 800)

      self.build_menu()
      self.build_toolbar()
      self.build_dock()
      self.build_tabs()

      self.setStatusBar(QStatusBar(self))
      self.statusBar().showMessage("Ready")

   def build_menu(self):
      file_menu = self.menuBar().addMenu("&File")

      exit_action = QAction("E&xit", self)
      exit_action.triggered.connect(self.close)
      file_menu.addAction(exit_action)

      self.exit_action = exit_action

   def build_toolbar(self):
      toolbar = QToolBar("Main Toolbar")
      self.addToolBar(toolbar)
      toolbar.addAction(self.exit_action)

   def build_dock(self):
      project_dock = QDockWidget("Model Objects", self)
      project_dock.setAllowedAreas(
         Qt.DockWidgetArea.LeftDockWidgetArea |
         Qt.DockWidgetArea.RightDockWidgetArea
      )

      object_list = QListWidget()
      object_list.addItems([
         "Model",
         "Compartments",
         "Vents",
         "Fires",
         "Targets",
         "Thermal Properties",
      ])

      project_dock.setWidget(object_list)
      self.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, project_dock)

   def build_tabs(self):
      tabs = QTabWidget()
      tabs.addTab(PlaceholderTab("Model page coming soon"), "Model")
      tabs.addTab(PlaceholderTab("Compartments page coming soon"), "Compartments")
      tabs.addTab(PlaceholderTab("Vents page coming soon"), "Vents")
      tabs.addTab(FiresTab(), "Fires")
      tabs.addTab(PlaceholderTab("Targets page coming soon"), "Targets")
      tabs.addTab(PlaceholderTab("Thermal Properties page coming soon"), "Thermal Properties")
      self.setCentralWidget(tabs)


def main():
   app = QApplication(sys.argv)
   app.setApplicationName("Cedit Qt Prototype")

   window = CeditMainWindow()
   window.show()

   sys.exit(app.exec())


if __name__ == "__main__":
   main()
from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QFrame,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QPushButton,
    QSplitter,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from cfast_case import CfastCase, FireRampPoint
from widgets.ramp_plot_canvas import RampPlotCanvas


class FiresTab(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.table = QTableWidget(8, 2)
        self.table.setHorizontalHeaderLabels(["Time (s)", "HRR (kW)"])
        self.table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Stretch
        )
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
            reverse=True,
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

    def add_to_case(self, case: CfastCase):
        points = self.extract_points()

        if not points:
            raise ValueError("Enter at least one valid time / HRR pair before exporting.")

        case.fire_ramp = [
            FireRampPoint(time=time_val, hrr=hrr_val)
            for time_val, hrr_val in points
        ]

    def update_plot(self):
        points = self.extract_points()
        self.plot_canvas.plot_ramp(points)

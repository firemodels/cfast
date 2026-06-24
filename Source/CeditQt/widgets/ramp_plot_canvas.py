from PySide6.QtWidgets import QSizePolicy

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
            self.ax.plot(times, hrrs, marker="o")
            self.ax.set_xlim(left=0.0)
            self.ax.set_ylim(bottom=0.0)
        else:
            self.ax.text(
                0.5,
                0.5,
                "Enter time / HRR points",
                ha="center",
                va="center",
                transform=self.ax.transAxes,
            )
            self.ax.set_xlim(0.0, 1.0)
            self.ax.set_ylim(0.0, 1.0)

        self.ax.set_title("HRR Ramp")
        self.ax.set_xlabel("Time (s)")
        self.ax.set_ylabel("HRR (kW)")
        self.ax.grid(True)
        self.draw()

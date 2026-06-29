import os
import sys
import tempfile
from pathlib import Path

_MPLCONFIGDIR = Path(tempfile.gettempdir()) / "cedit-qt-matplotlib"
_MPLCONFIGDIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(_MPLCONFIGDIR))
os.environ.setdefault("MPLBACKEND", "QtAgg")
os.environ.setdefault("QT_API", "PySide6")

from PySide6.QtWidgets import QApplication

from main_window import CeditMainWindow


def main():
    app = QApplication(sys.argv)
    app.setApplicationName("CEdit Qt Prototype")

    window = CeditMainWindow()
    window.show()

    sys.exit(app.exec())


if __name__ == "__main__":
    main()

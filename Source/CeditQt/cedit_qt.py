import os
import sys
import tempfile
from pathlib import Path

_MPLCONFIGDIR = Path(tempfile.gettempdir()) / "cedit-qt-matplotlib"
_MPLCONFIGDIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(_MPLCONFIGDIR))
os.environ.setdefault("MPLBACKEND", "QtAgg")
os.environ.setdefault("QT_API", "PySide6")

from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QApplication

from main_window import CeditMainWindow


def set_windows_app_id():
    if not sys.platform.startswith("win"):
        return
    try:
        import ctypes

        ctypes.windll.shell32.SetCurrentProcessExplicitAppUserModelID("gov.nist.firemodels.cedit")
    except Exception:
        pass


def resource_path(relative_path):
    base_path = Path(getattr(sys, "_MEIPASS", Path(__file__).resolve().parent))
    return base_path / relative_path


def application_icon():
    for icon_path in (
        resource_path("assets/CeditQt.ico"),
        resource_path("assets/CeditQt.png"),
    ):
        if icon_path.is_file():
            icon = QIcon(str(icon_path))
            if not icon.isNull():
                return icon
    return QIcon()


def main():
    set_windows_app_id()
    app = QApplication(sys.argv)
    app.setApplicationName("CFAST Editor (CEdit)")
    icon = application_icon()
    if not icon.isNull():
        app.setWindowIcon(icon)

    window = CeditMainWindow()
    if not icon.isNull():
        window.setWindowIcon(icon)
    window.show()

    sys.exit(app.exec())


if __name__ == "__main__":
    main()

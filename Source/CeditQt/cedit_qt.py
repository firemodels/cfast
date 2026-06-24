import sys

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

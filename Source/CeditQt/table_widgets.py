from __future__ import annotations

import math
import re

from PySide6.QtCore import QEvent, QTimer, Qt
from PySide6.QtGui import QColor, QPen
from PySide6.QtWidgets import QLineEdit, QStyledItemDelegate, QStyle, QTableWidget


_QUANTITY_TEXT = re.compile(r"([-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?)(\s+.*)?")


def rounded_display_text(text: str) -> str:
    """Limit fractional displays to five significant figures without changing units."""
    match = _QUANTITY_TEXT.fullmatch(text)
    if not match:
        return text
    number = match[1]
    mantissa = number.lower().split("e")[0]
    digits = mantissa.lstrip("+-").replace(".", "").lstrip("0")
    if len(digits) <= 5 or ("." not in number and "e" not in number.lower()):
        return text
    value = float(number)
    if not math.isfinite(value):
        return text
    return f"{value:.5g}{match[2] or ''}"


class UnitLineEdit(QLineEdit):
    """Show a compact quantity while retaining its full text until the user edits it.

    displayText() is the visible value; text() supplies the unrounded value to
    existing model parsers. Merely focusing, saving, or changing units is lossless.
    """

    def __init__(self, text: str = "", parent=None):
        super().__init__(parent)
        self.setText(text)

    def setText(self, text: str):
        self._value_text = text
        self._display_text = rounded_display_text(text)
        super().setText(self._display_text)
        self.setToolTip(text if self._display_text != text else "")

    def text(self) -> str:
        visible = super().text()
        if not self.isModified() and visible == self._display_text:
            return self._value_text
        return visible

    def compact_display(self):
        visible = super().text()
        if rounded_display_text(visible) != visible:
            self.setText(self.text())

    def focusOutEvent(self, event):
        # Let the existing editor slots commit the user's text before rounding it.
        super().focusOutEvent(event)
        self.compact_display()

    def keyPressEvent(self, event):
        super().keyPressEvent(event)
        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            self.compact_display()


class UnitCellDelegate(QStyledItemDelegate):
    """Round quantity displays and omit repeated header units, retaining model data."""

    def initStyleOption(self, option, index):
        super().initStyleOption(option, index)
        header = str(index.model().headerData(index.column(), Qt.Orientation.Horizontal))
        match = _QUANTITY_TEXT.fullmatch(option.text)
        if not match:
            return
        suffix = (match[2] or "").strip()
        if suffix or header.endswith(")"):
            option.text = rounded_display_text(option.text)
        if suffix and header.endswith(f"({suffix})"):
            option.text = option.text[:-(len(suffix) + 1)]


class UnitTableWidget(QTableWidget):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.setItemDelegate(UnitCellDelegate(self))


class HoverCellDelegate(UnitCellDelegate):
    def paint(self, painter, option, index):
        super().paint(painter, option, index)

        table = self.parent()
        if not isinstance(table, HoverEditTableWidget):
            return

        if table.hovered_cell() != (index.row(), index.column()):
            return

        selected = bool(option.state & QStyle.StateFlag.State_Selected)
        outer_color = QColor("#ffffff") if selected else QColor("#0078d7")
        inner_color = QColor("#004a99") if selected else QColor("#ffffff")

        painter.save()
        painter.setPen(QPen(outer_color, 2))
        painter.drawRect(option.rect.adjusted(1, 1, -2, -2))
        painter.setPen(QPen(inner_color, 1))
        painter.drawRect(option.rect.adjusted(3, 3, -4, -4))
        painter.restore()


class HoverEditTableWidget(UnitTableWidget):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._hovered_cell: tuple[int, int] | None = None
        self.setMouseTracking(True)
        self.viewport().setMouseTracking(True)
        self.setItemDelegate(HoverCellDelegate(self))

    def hovered_cell(self) -> tuple[int, int] | None:
        return self._hovered_cell

    def viewportEvent(self, event):
        if event.type() == QEvent.Type.MouseMove:
            self.set_hovered_cell_from_position(event.position().toPoint())
        elif event.type() == QEvent.Type.Leave:
            self.set_hovered_cell(None)

        return super().viewportEvent(event)

    def mousePressEvent(self, event):
        index = self.indexAt(event.position().toPoint())
        super().mousePressEvent(event)

        if event.button() != Qt.MouseButton.LeftButton:
            return

        if not self.index_is_editable(index):
            return

        row = index.row()
        col = index.column()
        QTimer.singleShot(0, lambda: self.edit_cell(row, col))

    def set_hovered_cell_from_position(self, position):
        index = self.indexAt(position)
        if index.isValid():
            self.set_hovered_cell((index.row(), index.column()))
        else:
            self.set_hovered_cell(None)

    def set_hovered_cell(self, cell: tuple[int, int] | None):
        if cell == self._hovered_cell:
            return

        old_cell = self._hovered_cell
        self._hovered_cell = cell

        for update_cell in (old_cell, cell):
            if update_cell is None:
                continue
            index = self.model().index(update_cell[0], update_cell[1])
            if index.isValid():
                self.viewport().update(self.visualRect(index))

    def edit_cell(self, row: int, col: int):
        index = self.model().index(row, col)
        if not self.index_is_editable(index):
            return

        self.setCurrentIndex(index)
        self.edit(index)

    def index_is_editable(self, index) -> bool:
        if not index.isValid():
            return False

        flags = self.model().flags(index)
        return bool(
            flags & Qt.ItemFlag.ItemIsEnabled
            and flags & Qt.ItemFlag.ItemIsEditable
        )

from __future__ import annotations

from PySide6.QtCore import QEvent, QTimer, Qt
from PySide6.QtGui import QColor, QPen
from PySide6.QtWidgets import QStyledItemDelegate, QStyle, QTableWidget


class HoverCellDelegate(QStyledItemDelegate):
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


class HoverEditTableWidget(QTableWidget):
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

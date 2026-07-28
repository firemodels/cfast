from __future__ import annotations

import re

from PySide6.QtGui import QColor, QFont, QSyntaxHighlighter, QTextCharFormat


def text_format(
    color: str,
    *,
    bold: bool = False,
    italic: bool = False,
) -> QTextCharFormat:
    fmt = QTextCharFormat()
    fmt.setForeground(QColor(color))
    if bold:
        fmt.setFontWeight(QFont.Weight.Bold)
    if italic:
        fmt.setFontItalic(True)
    return fmt


class CfastSyntaxHighlighter(QSyntaxHighlighter):
    parameter_pattern = re.compile(
        r"\b([A-Za-z_][A-Za-z0-9_]*)(?=\s*(?:\([^)]*\))?\s*=)"
    )
    namelist_pattern = re.compile(r"^\s*&[A-Za-z][A-Za-z0-9_]*\b")
    logical_pattern = re.compile(r"\.(?:TRUE|FALSE)\.", re.IGNORECASE)
    number_pattern = re.compile(
        r"(?<![A-Za-z0-9_.])"
        r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?"
        r"(?![A-Za-z0-9_.])"
    )
    delimiter_pattern = re.compile(r"[=,()]")
    namelist_end_pattern = re.compile(r"(?<!\S)/(?=\s*$)")

    def __init__(self, document):
        super().__init__(document)
        self.enabled = False

        self.comment_format = text_format("#6A737D", italic=True)
        self.namelist_format = text_format("#003C8F", bold=True)
        self.parameter_format = text_format("#7A3E9D", bold=True)
        self.string_format = text_format("#007A3D")
        self.number_format = text_format("#9A4B00")
        self.logical_format = text_format("#A0005A", bold=True)
        self.delimiter_format = text_format("#555555")

    def set_enabled(self, enabled: bool):
        if self.enabled == enabled:
            return

        self.enabled = enabled
        self.rehighlight()

    def highlightBlock(self, text: str):
        if not self.enabled:
            return

        string_spans, comment_start = self.literal_and_comment_spans(text)
        code_end = len(text) if comment_start is None else comment_start

        for start, end in string_spans:
            self.setFormat(start, end - start, self.string_format)

        if comment_start is not None:
            self.setFormat(comment_start, len(text) - comment_start, self.comment_format)

        self.apply_pattern(
            self.namelist_pattern,
            self.namelist_format,
            text,
            code_end,
            string_spans,
        )

        self.apply_parameter_pattern(text, code_end, string_spans)

        for pattern, fmt in (
            (self.logical_pattern, self.logical_format),
            (self.number_pattern, self.number_format),
            (self.delimiter_pattern, self.delimiter_format),
            (self.namelist_end_pattern, self.namelist_format),
        ):
            self.apply_pattern(pattern, fmt, text, code_end, string_spans)

    def literal_and_comment_spans(self, text: str) -> tuple[list[tuple[int, int]], int | None]:
        spans: list[tuple[int, int]] = []
        index = 0

        while index < len(text):
            char = text[index]

            if char == "!":
                return spans, index

            if char in ("'", '"'):
                quote = char
                start = index
                index += 1

                while index < len(text):
                    if text[index] == quote:
                        if index + 1 < len(text) and text[index + 1] == quote:
                            index += 2
                            continue
                        index += 1
                        break
                    index += 1

                spans.append((start, index))
                continue

            index += 1

        return spans, None

    def apply_parameter_pattern(
        self,
        text: str,
        code_end: int,
        protected_spans: list[tuple[int, int]],
    ):
        for match in self.parameter_pattern.finditer(text, 0, code_end):
            start, end = match.span(1)
            if self.overlaps_protected_span(start, end, protected_spans):
                continue
            self.setFormat(start, end - start, self.parameter_format)

    def apply_pattern(
        self,
        pattern: re.Pattern[str],
        fmt: QTextCharFormat,
        text: str,
        code_end: int,
        protected_spans: list[tuple[int, int]],
    ):
        for match in pattern.finditer(text, 0, code_end):
            start, end = match.span()
            if self.overlaps_protected_span(start, end, protected_spans):
                continue
            self.setFormat(start, end - start, fmt)

    @staticmethod
    def overlaps_protected_span(
        start: int,
        end: int,
        protected_spans: list[tuple[int, int]],
    ) -> bool:
        return any(
            start < span_end and end > span_start
            for span_start, span_end in protected_spans
        )

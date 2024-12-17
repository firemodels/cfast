Imports System
Imports System.IO

Public Class CSVcell
    Private cellNum As Double
    Private cellStr As String

    Property Num() As Double
        Get
            Return cellNum
        End Get
        Set(ByVal Value As Double)
            If Value = Nothing Then
                cellNum = 0.0
            Else
                cellNum = Value
            End If
            cellStr = cellNum.ToString
        End Set
    End Property
    Property Str() As String
        Get
            Return cellStr
        End Get
        Set(ByVal Value As String)
            If Value = Nothing Then
                cellStr = ""
            Else
                cellStr = Value
            End If
            cellNum = Val(cellStr)
        End Set
    End Property
    Property CSVcell() As String
        Get
            Dim dummy As String = ""
            ConvertStrforCSVOutput(cellStr, dummy)
            Return dummy
        End Get
        Set(ByVal Value As String)
            Dim dummy As String = ""
            If Value = Nothing Then
                dummy = ""
            Else
                ConvertStrfromCSVInput(Value, dummy)
            End If
            cellStr = dummy
            cellNum = Val(cellStr)
        End Set
    End Property
    Private Sub ConvertStrforCSVOutput(ByVal str As String, ByRef dummy As String)
        If str.IndexOf(",") > 0 Then
            dummy = """" + str + """"
        Else
            dummy = str
        End If
    End Sub
    Private Sub ConvertStrfromCSVInput(ByVal str As String, ByRef dummy As String)
        If str = "" Then
            dummy = str
        Else
            If str.Substring(0, 1) = """" Then str = str.Remove(0, 1)
            If str.Substring(str.Length - 1, 1) = """" Then str = str.Remove(str.Length - 1, 1)
            dummy = str
        End If
    End Sub
    Public Sub New()
        cellNum = 0.0
        cellStr = ""
    End Sub
End Class

Public Class CSVrow
    Private cCol() As CSVcell

    Property Num(ByVal col As Integer) As Double
        Get
            If col > cCol(0).Num Then
                Return 0.0
            Else
                Return cCol(col).Num
            End If
        End Get
        Set(ByVal Value As Double)
            If col > cCol(0).Num Then
                ReDim Preserve cCol(col)
                Dim i As Integer
                For i = cCol(0).Num + 1 To col
                    cCol(i) = New CSVcell
                Next
                cCol(0).Num = col
            End If
            If Value = Nothing Then
                cCol(col).Num = 0.0
            Else
                cCol(col).Num = Value
            End If
        End Set
    End Property
    Property Str(ByVal col As Integer) As String
        Get
            If col > cCol(0).Num Then
                Return ""
            Else
                Return cCol(col).Str
            End If
        End Get
        Set(ByVal Value As String)
            If col > cCol(0).Num Then
                ReDim Preserve cCol(col)
                Dim i As Integer
                For i = cCol(0).Num + 1 To col
                    cCol(i) = New CSVcell
                Next
                cCol(0).Num = col
            End If
            If Value = Nothing Then
                cCol(col).Str = ""
            Else
                cCol(col).Str = Value
            End If
        End Set
    End Property
    Property CSVcell(ByVal col As Integer) As String
        Get
            If col > cCol(0).Num Then
                Return ""
            Else
                Return cCol(col).CSVcell
            End If
        End Get
        Set(ByVal Value As String)
            If col > cCol(0).Num Then
                ReDim Preserve cCol(col)
                Dim i As Integer
                For i = cCol(0).Num + 1 To col
                    cCol(i) = New CSVcell
                Next
                cCol(0).Num = col
            End If
            If Value = Nothing Then
                cCol(col).CSVcell = ""
            Else
                cCol(col).CSVcell = Value
            End If
        End Set
    End Property
    Property strrow() As String
        Get
            Dim col As Integer
            Dim dummy As String = ""
            If cCol(0).Num > 0 Then
                dummy = cCol(1).Str
            End If
            For col = 2 To cCol(0).Num
                dummy = dummy + "," + cCol(col).Str
            Next
            Return dummy
        End Get
        Set(ByVal Value As String)
            Dim col As Integer = 0
            Dim dummy As String = ""
            If Value = Nothing Then Return
            Do While Value.Length > 0
                GetCell(dummy, Value)
                col += 1
                If col > cCol(0).Num Then
                    ReDim Preserve cCol(col)
                    cCol(col) = New CSVcell
                    cCol(0).Num = col
                End If
                cCol(col).Str = dummy
            Loop
        End Set
    End Property
    Property CSVrow() As String
        Get
            Dim col As Integer
            Dim dummy As String = ""
            If cCol(0).Num > 0 Then
                dummy = cCol(1).CSVcell
            End If
            For col = 2 To cCol(0).Num
                dummy = dummy + "," + cCol(col).CSVcell
            Next
            Return dummy
        End Get
        Set(ByVal Value As String)
            Dim col As Integer = 0
            Dim dummy As String = ""
            If Value = Nothing Then Return
            Do While Value.Length > 0
                GetCell(dummy, Value)
                col += 1
                If col > cCol(0).Num Then
                    ReDim Preserve cCol(col)
                    cCol(col) = New CSVcell
                    cCol(0).Num = col
                End If
                cCol(col).CSVcell = dummy
            Loop
        End Set
    End Property
    Property CSVrow(ByVal col As Integer) As String
        Get
            Dim cl As Integer
            Dim dummy As String = ""
            For cl = 1 To col
                If cl > cCol(0).Num Then
                    dummy = dummy + ","
                Else
                    dummy = dummy + "," + cCol(cl).CSVcell
                End If
            Next
            Return dummy
        End Get
        Set(ByVal Value As String)
            Dim cl As Integer = 0
            Dim dummy As String = ""
            For cl = 1 To col
                GetCell(dummy, Value)
                If cl > cCol(0).Num Then
                    ReDim Preserve cCol(cl)
                    cCol(cl) = New CSVcell
                    cCol(0).Num = cl
                End If
                cCol(cl).CSVcell = dummy
            Next
        End Set
    End Property
    Private Sub GetCell(ByRef cell As String, ByRef str As String)

        Dim dblq As String = """"
        Dim commachar As String = ","
        Dim tabchar As String = Chr(9)

        If str = Nothing Then
            cell = ""
            Return
        End If
        Dim len As Integer = str.Length()
        If len = 0 Then
            cell = ""
        ElseIf str.Substring(0, 1) = dblq Then
            Dim qcnt As Integer = 1
            Dim idx As Integer = 1
            Do
                If str.Substring(idx, 1) = dblq Then
                    qcnt = qcnt + 1
                End If
                idx = idx + 1
            Loop Until ((qcnt Mod 2 = 0) And (str.Substring(idx, 1) = commachar) Or idx >= len - 1)
            cell = str.Substring(0, idx)
            str = str.Remove(0, idx + 1)
        Else
            If str.IndexOf(commachar) >= 0 Then
                cell = str.Substring(0, str.IndexOf(commachar))
                str = str.Remove(0, str.IndexOf(commachar) + 1)
            ElseIf str.IndexOf(tabchar) >= 0 Then
                cell = str.Substring(0, str.IndexOf(tabchar))
                str = str.Remove(0, str.IndexOf(tabchar) + 1)
            Else
                cell = str
                str = ""
            End If
        End If
    End Sub
    Public Sub New()
        ReDim cCol(0)
        cCol(0) = New CSVcell
        cCol(0).Num = 0
    End Sub
    Public Sub New(ByVal col As Integer)
        ReDim cCol(col)
        Dim i As Integer
        For i = 0 To col
            cCol(i) = New CSVcell
        Next
        cCol(0).Num = col
    End Sub
    Public Sub New(ByVal col As Integer, ByVal str As String)
        ReDim cCol(col)
        Dim cl As Integer
        Dim dummy As String = ""
        cCol(0) = New CSVcell
        cCol(0).Num = 0
        For cl = 1 To col
            GetCell(dummy, str)
            If cl > cCol(0).Num Then
                ReDim Preserve cCol(cl)
                cCol(cl) = New CSVcell
                cCol(0).Num = cl
            End If
            cCol(cl).CSVcell = dummy
        Next
    End Sub
    Public Sub New(ByVal str As String)
        Dim cl As Integer = 0
        Dim dummy As String = ""
        ReDim cCol(0)
        cCol(0).Num = 0
        If str = Nothing Then
            Return
        End If
        Do While str.Length > 0
            GetCell(dummy, str)
            cl += 1
            If cl > cCol(0).Num Then
                ReDim cCol(cl)
                cCol(cl) = New CSVcell
                cCol(0).Num = cl
            End If
            cCol(cl).CSVcell = dummy
        Loop
    End Sub
End Class

Public Class CSVsheet
    Private cRow() As CSVrow
    Private mxRow As Integer
    Private mxCol As Integer

    Property num(ByVal row As Integer, ByVal col As Integer) As Double
        Get
            If row > mxRow Then
                Return 0.0
            Else
                Return cRow(row).Num(col)
            End If
        End Get
        Set(ByVal Value As Double)
            If Value = Nothing Then Value = 0.0
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            cRow(row).Num(col) = Value
            If cRow(row).Num(0) > mxCol Then
                mxCol = cRow(row).Num(0)
            End If
        End Set
    End Property
    Property str(ByVal row As Integer, ByVal col As Integer) As String
        Get
            If row > mxRow Then
                Return ""
            Else
                Return cRow(row).Str(col)
            End If
        End Get
        Set(ByVal Value As String)
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            If Value = Nothing Then
                cRow(row).Str(col) = ""
            Else
                cRow(row).Str(col) = Value
            End If
            If cRow(row).Num(0) > mxCol Then
                mxCol = cRow(row).Num(0)
            End If
        End Set
    End Property
    Property CSVcell(ByVal row As Integer, ByVal col As Integer) As String
        Get
            If row > mxRow Then
                Return ""
            Else
                Return cRow(row).CSVcell(col)
            End If
        End Get
        Set(ByVal Value As String)
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            If Value = Nothing Then
                cRow(row).CSVcell(col) = ""
            Else
                cRow(row).CSVcell(col) = Value
            End If
            If cRow(row).Num(0) > mxCol Then
                mxCol = cRow(row).Num(0)
            End If
        End Set
    End Property
    Property strrow(ByVal row As Integer) As String
        Get
            If row > mxRow Then
                Return ""
            Else
                Return cRow(row).strrow
            End If
        End Get
        Set(ByVal Value As String)
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            If Value = Nothing Then
                cRow(row).strrow = ""
            Else
                cRow(row).strrow = Value
            End If
            If cRow(row).Num(0) > mxCol Then
                mxCol = cRow(row).Num(0)
            End If
        End Set
    End Property
    Property CSVrow(ByVal row As Integer) As String
        Get
            If row > mxRow Then
                Return ""
            Else
                Return cRow(row).CSVrow
            End If
        End Get
        Set(ByVal Value As String)
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            If Value = Nothing Then
                cRow(row).CSVrow = ""
            Else
                cRow(row).CSVrow = Value
            End If
            If cRow(row).Num(0) > mxCol Then
                mxCol = cRow(row).Num(0)
            End If
        End Set
    End Property
    Property CSVrow(ByVal row As Integer, ByVal col As Integer) As String
        Get
            If row > mxRow Then
                Dim dummy As String = ""
                Dim j As Integer
                For j = 1 To col
                    dummy = dummy + ","
                Next
                Return dummy
            Else
                Return cRow(row).CSVrow(col)
            End If
        End Get
        Set(ByVal Value As String)
            If row > mxRow Then
                ReDim Preserve cRow(row)
                Dim i As Integer
                For i = mxRow + 1 To row
                    cRow(i) = New CSVrow
                Next
                mxRow = row
            End If
            If Value = Nothing Then
                cRow(row).CSVrow(col) = ""
            Else
                cRow(row).CSVrow(col) = Value
            End If
            If col > mxCol Then
                mxCol = col
            End If
        End Set
    End Property
    WriteOnly Property CSVrow() As String
        Set(ByVal Value As String)
            mxRow += 1
            ReDim Preserve cRow(mxRow)
            cRow(mxRow) = New CSVrow
            cRow(mxRow).CSVrow = Value
            If cRow(mxRow).Num(0) > mxCol Then
                mxCol = cRow(mxRow).Num(0)
            End If
        End Set
    End Property
    ReadOnly Property MaxRow() As Integer
        Get
            Return mxRow
        End Get
    End Property
    ReadOnly Property MaxCol() As Integer
        Get
            Return mxCol
        End Get
    End Property
    Public Sub New()
        ReDim cRow(0)
        cRow(0) = New CSVrow
        mxRow = 0
        mxCol = 0
    End Sub

    Public Sub New(ByVal row As Integer, ByVal col As Integer)
        ReDim cRow(row)
        Dim i As Integer
        For i = 0 To row
            cRow(i) = New CSVrow
        Next
        mxRow = row
        mxCol = col
    End Sub

    Public Sub New(ByVal filenm As String)
        Dim ln As String
        ReDim cRow(0)
        cRow(0) = New CSVrow
        mxRow = 0
        mxCol = 0
        Dim io As Integer = 1
        If file.exists(filenm) Then
            FileOpen(io, filenm, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
            Do Until EOF(io)
                ln = LineInput(io)
                CSVrow = ln
            Loop
            FileClose(io)
        End If
    End Sub

    Public Sub WrtCSVfile(ByVal filenm As String)
        Dim i As Integer
        Dim ln As String
        Dim io As Integer = 1
        FileOpen(io, filenm, OpenMode.Output)
        For i = 1 To mxRow
            ln = CSVrow(i)
            PrintLine(io, ln)
        Next
        FileClose(io)
    End Sub
End Class
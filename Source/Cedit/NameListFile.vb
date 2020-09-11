Imports System
Imports System.IO

Public Class NameListFile
    Private str As String
    Private nmlst As String
    Private ssht As CSVsheet
    Private fsht As CSVsheet
    Private filename As String
    Private mxnmlist As Integer
    Private curnmlist As Integer
    Private irow As Integer
    Private icol As Integer
    Private io As Integer = 1

    Public ReadOnly Property TotNMList() As Integer
        Get
            Return mxnmlist
        End Get
    End Property
    Public ReadOnly Property GetNMListID(ByVal idx As Integer) As String
        Get
            Return ssht.CSVcell(fsht.num(idx, 1), 1)
        End Get
    End Property
    Public ReadOnly Property ForNMListNumVar(ByVal idx As Integer) As Integer
        Get
            Return fsht.num(idx, 2)
        End Get
    End Property
    Public ReadOnly Property ForNMListGetVar(ByVal idx As Integer, ByVal jdx As Integer) As String
        Get
            Return ssht.CSVcell(fsht.CSVcell(idx, 1) + jdx, 2)
        End Get
    End Property
    Public ReadOnly Property ForNMListVarNumVal(ByVal idx As Integer, ByVal jdx As Integer) As Integer
        Get
            Return ssht.num(fsht.CSVcell(idx, 1) + jdx, 3)
        End Get
    End Property
    Public ReadOnly Property ForNMListVarGetStr(ByVal idx As Integer, ByVal jdx As Integer, ByVal kdx As Integer) As String
        Get
            Return ssht.CSVcell(fsht.num(idx, 1) + jdx, 3 + kdx)
        End Get
    End Property
    Public ReadOnly Property ForNMListVarGetNum(ByVal idx As Integer, ByVal jdx As Integer, ByVal kdx As Integer) As Double
        Get
            Return ssht.num(fsht.num(idx, 1) + jdx, 3 + kdx)
        End Get
    End Property
    Private ReadOnly Property NextNMLInFile() As Boolean
        Get
            Dim flag As Boolean = False
            Do Until (EOF(io))
                str = LineInput(io)
                If (str.IndexOf("&", 0) >= 0) Then
                    nmlst = str + " "
                    flag = True
                    'Do While (str.IndexOf("/", 0) < 0)
                    Do While (FindEndOfNameList(nmlst) < 0)
                        str = LineInput(io)
                        nmlst = nmlst + str + " "
                    Loop
                    Exit Do
                End If
            Loop
            Return flag
        End Get
    End Property
    Private ReadOnly Property NextVarInFile() As Boolean
        Get
            Dim flag As Boolean = False
            If (nmlst.IndexOf("=", 0) >= 0) Then
                flag = True
            End If
            Return flag
        End Get
    End Property
    Private ReadOnly Property GetVarIDInFile() As String
        Get
            Dim hdr As String = ""
            If (nmlst.IndexOf("=", 0) >= 0) Then
                hdr = nmlst.Substring(0, nmlst.IndexOf("=", 0))
                nmlst = nmlst.Remove(0, nmlst.IndexOf("=", 0) + 1)
                Dim idx As Integer = hdr.Length - 1
                Do While (delimiter(hdr.Substring(idx, 1)))
                    idx -= 1
                Loop
                hdr = hdr.Substring(0, idx + 1)
            End If
            Return hdr
        End Get
    End Property
    Private ReadOnly Property GetValuesInFile() As String
        Get
            Dim val As String = ""
            Dim idx As Integer
            If (nmlst.IndexOf("=", 0) >= 0) Then
                idx = nmlst.IndexOf("=", 0) - 1
                Do While (delimiter(nmlst.Substring(idx, 1)))
                    idx -= 1
                Loop
                Do While (Not delimiter(nmlst.Substring(idx, 1)))
                    idx -= 1
                Loop
                val = nmlst.Substring(0, idx + 1) + "  "
                nmlst = nmlst.Remove(0, idx + 1)
                'ElseIf (nmlst.IndexOf("/", 0) >= 0) Then
                '    idx = nmlst.IndexOf("/", 0)
            ElseIf (FindEndOfNameList(nmlst) >= 0) Then
                idx = FindEndOfNameList(nmlst)
                val = nmlst.Substring(0, idx)
                nmlst = nmlst.Remove(0, idx)
            End If
            Return val
        End Get
    End Property
    Private ReadOnly Property delimiter(ByVal Dummy As String) As Boolean
        Get
            Dim sp As String = " "
            Dim comma As String = ","
            Dim tab As String = Chr(9)
            Dim flag As Boolean = False
            If ((Dummy = sp) Or (Dummy = comma) Or (Dummy = tab)) Then
                flag = True
            End If
            Return flag
        End Get
    End Property
    Private ReadOnly Property expdelimiter(ByVal Dummy As String) As Boolean
        Get
            Dim sp As String = " "
            Dim comma As String = ","
            Dim tab As String = Chr(9)
            Dim dquote As String = """"


            Dim flag As Boolean = False
            If ((Dummy = sp) Or (Dummy = comma) Or (Dummy = tab) Or (Dummy = dquote)) Then
                flag = True
            End If
            Return flag
        End Get
    End Property
    Private ReadOnly Property GetNMLIDInFile() As String
        Get
            Dim hdr As String = ""
            If (nmlst.IndexOf("&", 0) >= 0) Then
                hdr = nmlst.Substring(nmlst.IndexOf("&", 0) + 1, nmlst.IndexOf(" ", nmlst.IndexOf("&", 0) + 1) - 1)
                nmlst = nmlst.Remove(0, nmlst.IndexOf(" ", nmlst.IndexOf("&", 0) + 1) + 1)
                Do While (delimiter(nmlst.Substring(0, 1)))
                    nmlst = nmlst.Remove(0, 1)
                Loop
            End If
            Return hdr
        End Get
    End Property
    Private ReadOnly Property FindEndOfNameList(ByVal str As String) As Integer
        Get
            Dim idx As Integer = 0
            Do Until str.Substring(idx, 1) = "/" Or idx >= str.Length - 1
                If str.Substring(idx, 1) = "'" Then
                    idx += 1
                    Do Until str.Substring(idx, 1) = "'" Or idx >= str.Length - 1
                        idx += 1
                    Loop
                End If
                idx += 1
            Loop
            If idx >= str.Length - 1 Then
                Return -1
            Else
                Return idx
            End If
        End Get
    End Property

    Public Sub New()
        ssht = New CSVsheet
        fsht = New CSVsheet
        str = ""
        nmlst = ""
        filename = ""
        irow = 0
        icol = 0
        mxnmlist = 0
        curnmlist = 0
    End Sub
    Public Sub New(ByVal filenm As String)
        ssht = New CSVsheet
        fsht = New CSVsheet
        str = ""
        nmlst = ""
        filename = filenm
        irow = 0
        icol = 0
        mxnmlist = 0
        curnmlist = 0
        If file.exists(filename) Then
            FileOpen(io, filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
            Do While (NextNMLInFile)
                irow += 1
                str = GetNMLIDInFile
                ssht.CSVcell(irow, 1) = str
                mxnmlist += 1
                curnmlist += 1
                fsht.CSVcell(curnmlist, 1) = irow
                fsht.CSVcell(curnmlist, 2) = 0
                Do While (NextVarInFile)
                    irow += 1
                    fsht.CSVcell(curnmlist, 2) = fsht.CSVcell(curnmlist, 2) + 1
                    ssht.CSVcell(irow, 2) = GetVarIDInFile
                    ssht.CSVcell(irow, 4) = GetValuesInFile
                Loop
            Loop
            FileClose(io)
            Dim mxrow As Integer = ssht.MaxRow
            For irow = 1 To mxrow
                nmlst = ssht.CSVcell(irow, 1)
                If (nmlst = "") Then
                    icol = 4
                    nmlst = ssht.CSVcell(irow, icol) + " "
                    Dim ilen As Integer = nmlst.Length
                    Dim idx As Integer = 0
                    Dim idx2 As Integer
                    ssht.CSVcell(irow, 3) = 0
                    Do While (idx <= ilen - 1)
                        If (expdelimiter(nmlst.Substring(idx, 1))) Then
                            idx += 1
                        ElseIf (nmlst.Substring(idx, 2) = "''") Then
                            idx = idx + 2
                            ssht.CSVcell(irow, icol) = ""
                            icol += 1
                        ElseIf (nmlst.Substring(idx, 1) = "'") Then
                            idx2 = idx + 1
                            idx += 1
                            Do While (nmlst.Substring(idx2, 1) <> "'" Or (nmlst.Substring(idx2, 2) = "''"))
                                If nmlst.Substring(idx2, 2) = "''" Then
                                    nmlst = nmlst.Remove(idx2, 1)
                                    ilen = nmlst.Length
                                    idx2 += 1
                                Else
                                    idx2 += 1
                                End If
                            Loop
                            ssht.CSVcell(irow, icol) = nmlst.Substring(idx, idx2 - idx)
                            ssht.CSVcell(irow, 3) = ssht.CSVcell(irow, 3) + 1
                            icol += 1
                            idx = idx2 + 1
                        Else
                            idx2 = idx
                            Do While (Not expdelimiter(nmlst.Substring(idx2, 1)) And idx2 < ilen - 1)
                                idx2 += 1
                            Loop
                            Dim str1 As String = nmlst.Substring(idx, idx2 - idx)
                            If (str1.IndexOf("*", 0) >= 0) Then
                                Dim ix As Integer = Val(str1.Substring(0, str1.IndexOf("*", 0)))
                                Dim xx As Double = Val(str1.Remove(0, str1.IndexOf("*", 0) + 1))
                                Dim jdx As Integer
                                For jdx = 1 To ix
                                    ssht.CSVcell(irow, icol) = xx
                                    icol += 1
                                    ssht.CSVcell(irow, 3) = ssht.CSVcell(irow, 3) + 1
                                Next
                            Else
                                ssht.CSVcell(irow, icol) = str1
                                ssht.CSVcell(irow, 3) = ssht.CSVcell(irow, 3) + 1
                                icol += 1
                            End If
                            idx = idx2 + 1
                        End If
                    Loop
                End If
            Next
        End If
    End Sub
    Public Sub DebugPrint(ByVal filenm As String, ByVal filenm2 As String)
        ssht.WrtCSVfile(filenm)
        fsht.WrtCSVfile(filenm2)
    End Sub

End Class

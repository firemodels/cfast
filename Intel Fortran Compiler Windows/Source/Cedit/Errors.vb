Public Class ErrorMessages
    Public Queue As New Queue                       ' Holds current list of errors in the current input file
    Private ErrorScans As Integer = 0               ' Counts number of times we've looked for errors
    Private iTop As Integer

    Public Const TypeWarning As Integer = 0         ' Error types for warning and error messages in increasing severity
    Public Const TypeError As Integer = 1
    Public Const TypeFatal As Integer = 2
    Public Const TypeCFastLog As Integer = 3
    Public Const TypeNothing As Integer = 4
    Public Const TypeCFASTError As Integer = 5
    Public Sub New()
        Queue.Clear()
        Queue.Enqueue("No Errors or Warnings")
    End Sub
    Friend Sub Add(ByVal ErrorText As String, ByVal ErrorType As Integer)
        Dim Text As String
        If ErrorType <> TypeNothing Then
            Text = ErrorNames.Substring(7 * (ErrorType), 7).Trim + ": " + ErrorText
        Else
            Text = ErrorText
        End If
        Queue.Enqueue(Text)
        While Queue.Count > 200
            Queue.Dequeue()
        End While
    End Sub
    Friend Sub Break(ByVal FileName As String)
        If FileName = Nothing Or FileName = "" Then FileName = "New File"
        If Queue.Count > 0 Then Add("-------------------- Input File Syntax Check " + ErrorScans.ToString + " (" + FileName + ")", TypeNothing)
        ErrorScans += 1
    End Sub
    Friend ReadOnly Property TopError() As String
        Get
            Dim ErrorText(200) As String
            Queue.CopyTo(ErrorText, 0)
            iTop = Count - 1
            While (ErrorText(iTop) + " ").Substring(0, 1) = "-" And iTop > 0
                iTop -= 1
            End While
            Return ErrorText(iTop)
        End Get
    End Property
    Friend ReadOnly Property Count() As Integer
        Get
            Return Queue.Count
        End Get
    End Property
End Class
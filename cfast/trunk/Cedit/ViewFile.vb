Public Class ViewFile
    Private _fileName As String
    Public Property FileName() As String
        Get
            Return _fileName
        End Get
        Set(ByVal value As String)
            Dim io As Integer = 1
            Dim ln As String
            If System.IO.File.Exists(value) Then
                _fileName = value
                Me.Text = _fileName
                Me.C1PrintDocument1.Body.Children.Clear()
                FileOpen(io, _fileName, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
                Do Until EOF(io)
                    ln = LineInput(io)
                    If ln = "" Then
                        C1PrintDocument1.Body.Children.Add(New C1.C1Preview.RenderText(vbCr))
                    Else
                        C1PrintDocument1.Body.Children.Add(New C1.C1Preview.RenderText(ln, New Font("Courier New", 12)))
                    End If
                Loop
                FileClose(io)
                C1PrintDocument1.Generate()
            End If
        End Set
    End Property
End Class
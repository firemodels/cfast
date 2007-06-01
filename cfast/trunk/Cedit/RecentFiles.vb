Public Class RecentFiles
    Public Filenames() As String = {" ", " ", " ", " "}, temp(3) As String
    Public Count As Integer
    Private RegistryFiles(,) As String
    Private aAppTitle As String
    Private i As Integer, j As Integer
    Public Sub New(ByVal AppTitle As String)
        aAppTitle = AppTitle
        RegistryFiles = GetAllSettings(AppTitle, "RecentFiles")
        Try
            Me.Count = RegistryFiles.GetUpperBound(0) + 1
            For i = 0 To Me.Count - 1
                Filenames(i) = RegistryFiles(i, 1)
            Next
        Catch ex As Exception
            Me.Count = 0
        End Try
    End Sub
    Public Sub Save()
        If Me.Count > 0 Then
            For i = 0 To Me.Count - 1
                SaveSetting(aAppTitle, "RecentFiles", "File" + i.ToString, Filenames(i))
            Next
        End If
    End Sub
    Public Sub Add(ByVal Item As String)
        ' add the item at the beginning of the list and remove any duplicate lower down the list
        j = 0
        For i = 0 To 3
            temp(i) = Filenames(i)
        Next
        Filenames(0) = Item
        If Count > 0 Then
            For i = 0 To Count - 1
                If temp(i) <> Item And j < 3 Then
                    j = j + 1
                    Filenames(j) = temp(i)
                End If
            Next
        End If
        Count = Math.Min(j + 1, 4)
    End Sub
End Class

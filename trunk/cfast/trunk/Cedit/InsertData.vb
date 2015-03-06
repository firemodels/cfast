Public Class InsertData
    Private MainWin As CeditMain
    Private SelectedThermalProperties As New ThermalPropertiesCollection
    Private SelectedFires As New FireCollection
    Private NumAdded, numSelected As Integer
    Public Sub New(ByVal ParentWindow As Object)

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        MainWin = ParentWindow
    End Sub
    Public Sub SetupData(ByVal Type As Integer)
        Select Case Type
            Case InsertDataType.Fire
                Me.InsertDataSummary(0, InsertFireNum.Fire) = "Fire"
                Me.InsertDataSummary(0, InsertFireNum.Formula) = "Formula"
                Me.InsertDataSummary(0, InsertFireNum.Height) = "Height"
                Me.InsertDataSummary(0, InsertFireNum.Area) = "Area"
                Me.InsertDataSummary(0, InsertFireNum.QDot) = "QDot"
                Me.InsertDataSummary(0, InsertFireNum.Soot) = "Soot"
                Me.InsertDataSummary(0, InsertFireNum.CO) = "CO"
                Me.InsertDataSummary(0, InsertFireNum.HoC) = "HoC"
                Me.InsertDataSummary(0, InsertFireNum.Material) = "Material"

                Me.InsertDataSummary.Location = New System.Drawing.Point(12, 12)
                Me.InsertDataSummary.Size = New System.Drawing.Size(984, 156)

                Me.SelectedFires.Clear()
                MainWin.UpdateGUI.InitEditGrid(InsertDataSummary)
                MainWin.UpdateGUI.ClearGrid(InsertDataSummary)
            Case InsertDataType.ThermalProperty
                Me.InsertDataSummary(0, InsertThermalNum.Material) = "Material"
                Me.InsertDataSummary(0, InsertThermalNum.Conductivity) = "Conductivity"
                Me.InsertDataSummary(0, InsertThermalNum.SpecificHeat) = "Specific Heat"
                Me.InsertDataSummary(0, InsertThermalNum.Density) = "Density"
                Me.InsertDataSummary(0, InsertThermalNum.Thickness) = "Thickness"
                Me.InsertDataSummary(0, InsertThermalNum.Emissivity) = "Emissivity"

                Me.InsertDataSummary.Location = New System.Drawing.Point(154, 12)
                Me.InsertDataSummary.Size = New System.Drawing.Size(700, 156)

                Me.SelectedThermalProperties.Clear()
                MainWin.UpdateGUI.InitEditGrid(InsertDataSummary)
                MainWin.UpdateGUI.ClearGrid(InsertDataSummary)
        End Select
        NumAdded = 0
        numSelected = 0
    End Sub
    Public Sub AddSelectedData(ByVal Type As Integer, ByVal Filename As String)
        Dim i As Integer
        Select Case Type
            Case InsertDataType.Fire
            Case InsertDataType.ThermalProperty
                Dim csv As New CSVsheet(Filename), aThermalProperty As New ThermalProperty
                TempThermalProperties.Clear()
                NumAdded = 0
                IO.FindThermalProperties(csv, TempThermalProperties)
                If TempThermalProperties.Count > Me.InsertDataSummary.Rows.Count Then Me.InsertDataSummary.Rows.Count = TempThermalProperties.Count + 1
                If TempThermalProperties.Count > 0 Then
                    For i = 1 To TempThermalProperties.Count
                        aThermalProperty = TempThermalProperties.Item(i - 1)
                        Me.InsertDataSummary(i, InsertThermalNum.Material) = aThermalProperty.Name
                        Me.InsertDataSummary(i, InsertThermalNum.Conductivity) = aThermalProperty.Conductivity.ToString
                        Me.InsertDataSummary(i, InsertThermalNum.SpecificHeat) = aThermalProperty.SpecificHeat.ToString
                        Me.InsertDataSummary(i, InsertThermalNum.Density) = aThermalProperty.Density.ToString
                        Me.InsertDataSummary(i, InsertThermalNum.Thickness) = aThermalProperty.Thickness.ToString
                        Me.InsertDataSummary(i, InsertThermalNum.Emissivity) = aThermalProperty.Emissivity.ToString
                    Next
                    NumAdded = TempThermalProperties.Count
                End If
        End Select
    End Sub
    Private Sub InsertSelect_Click(sender As Object, e As EventArgs) Handles InsertSelect.Click, InsertDeselect.Click
        Dim i As Integer
        If NumAdded > 0 Then
            For i = 1 To NumAdded
                If sender Is InsertSelect And Me.InsertDataSummary(i, 1) <> "" Then
                    Me.InsertDataSummary(i, 0) = True
                Else
                    Me.InsertDataSummary(i, 0) = False
                End If
            Next
        End If
    End Sub
    Public ReadOnly Property Selected(ByVal item As Integer) As Boolean
        Get
            If item <= NumAdded And item > 0 Then
                Selected = Me.InsertDataSummary(item, 0)
            Else
                Selected = False
            End If
        End Get
    End Property
    Public ReadOnly Property Count() As Integer
        Get
            Count = NumAdded
        End Get
    End Property
End Class
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
                Text = "Insert Fires"
                InsertDataSummary(0, InsertFireNum.Fire) = "Fire"
                InsertDataSummary(0, InsertFireNum.Formula) = "Formula"
                InsertDataSummary(0, InsertFireNum.Height) = "Height"
                InsertDataSummary(0, InsertFireNum.Area) = "Area"
                InsertDataSummary(0, InsertFireNum.QDot) = "HRR"
                InsertDataSummary(0, InsertFireNum.Soot) = "Soot"
                InsertDataSummary(0, InsertFireNum.CO) = "CO"
                InsertDataSummary(0, InsertFireNum.HoC) = "HoC"

                InsertDataSummary.Location = New System.Drawing.Point(12, 12)
                InsertDataSummary.Size = New System.Drawing.Size(890, 156)

                SelectedFires.Clear()
                MainWin.UpdateGUI.InitEditGrid(InsertDataSummary)
                MainWin.UpdateGUI.ClearGrid(InsertDataSummary)
            Case InsertDataType.ThermalProperty
                Text = "Insert Thermal Properties"
                InsertDataSummary(0, InsertThermalNum.Material) = "Material"
                InsertDataSummary(0, InsertThermalNum.Conductivity) = "Conductivity"
                InsertDataSummary(0, InsertThermalNum.SpecificHeat) = "Specific Heat"
                InsertDataSummary(0, InsertThermalNum.Density) = "Density"
                InsertDataSummary(0, InsertThermalNum.Thickness) = "Thickness"
                InsertDataSummary(0, InsertThermalNum.Emissivity) = "Emissivity"

                InsertDataSummary.Location = New System.Drawing.Point(115, 12)
                InsertDataSummary.Size = New System.Drawing.Size(700, 156)

                SelectedThermalProperties.Clear()
                MainWin.UpdateGUI.InitEditGrid(InsertDataSummary)
                MainWin.UpdateGUI.ClearGrid(InsertDataSummary)
        End Select
        NumAdded = 0
    End Sub
    Public Sub AddSelectedData(ByVal Type As Integer, ByVal Filename As String)
        Dim i As Integer
        Dim csv As New CSVsheet(Filename)
        NumAdded = 0
        Select Case Type
            Case InsertDataType.Fire
                Dim aFire As New Fire
                Dim FileExt As String = System.IO.Path.GetExtension(Filename)
                TempFires.Clear()
                myUnits.SI = True
                If FileExt = ".o" Then
                    IO.FindFires(InsertDataType.ObjectFile, Filename)
                Else
                    IO.FindFires(InsertDataType.EmbeddedFire, Filename)
                End If
                myUnits.SI = False
                If TempFires.Count > InsertDataSummary.Rows.Count Then InsertDataSummary.Rows.Count = TempFires.Count + 1
                If TempFires.Count > 0 Then
                    For i = 1 To TempFires.Count
                        aFire = TempFires.Item(i - 1)
                        InsertDataSummary(i, InsertFireNum.Fire) = aFire.Name
                        InsertDataSummary(i, InsertFireNum.Formula) = aFire.ChemicalFormula
                        InsertDataSummary(i, InsertFireNum.Height) = aFire.Peak(Fire.FireHeight).ToString
                        InsertDataSummary(i, InsertFireNum.Area) = aFire.Peak(Fire.FireArea).ToString
                        InsertDataSummary(i, InsertFireNum.QDot) = aFire.Peak(Fire.FireHRR).ToString
                        InsertDataSummary(i, InsertFireNum.Soot) = aFire.Peak(Fire.FireSoot)
                        InsertDataSummary(i, InsertFireNum.CO) = aFire.Peak(Fire.FireCO).ToString
                        InsertDataSummary(i, InsertFireNum.HoC) = aFire.HeatofCombustion.ToString
                    Next
                    NumAdded = TempFires.Count
                End If
            Case InsertDataType.ThermalProperty
                Dim aThermalProperty As New ThermalProperty
                TempThermalProperties.Clear()
                myUnits.SI = True
                IO.FindThermalProperties(Filename, TempThermalProperties)
                myUnits.SI = False
                If TempThermalProperties.Count > InsertDataSummary.Rows.Count Then InsertDataSummary.Rows.Count = TempThermalProperties.Count + 1
                If TempThermalProperties.Count > 0 Then
                    For i = 1 To TempThermalProperties.Count
                        aThermalProperty = TempThermalProperties.Item(i - 1)
                        InsertDataSummary(i, InsertThermalNum.Material) = aThermalProperty.Name
                        InsertDataSummary(i, InsertThermalNum.Conductivity) = aThermalProperty.Conductivity.ToString
                        InsertDataSummary(i, InsertThermalNum.SpecificHeat) = aThermalProperty.SpecificHeat.ToString
                        InsertDataSummary(i, InsertThermalNum.Density) = aThermalProperty.Density.ToString
                        InsertDataSummary(i, InsertThermalNum.Thickness) = aThermalProperty.Thickness.ToString
                        InsertDataSummary(i, InsertThermalNum.Emissivity) = aThermalProperty.Emissivity.ToString
                    Next
                    NumAdded = TempThermalProperties.Count
                End If
        End Select
    End Sub
    Private Sub InsertSelect_Click(sender As Object, e As EventArgs) Handles InsertSelect.Click, InsertDeselect.Click
        Dim i As Integer
        If NumAdded > 0 Then
            For i = 1 To NumAdded
                If sender Is InsertSelect And InsertDataSummary(i, 1) <> "" Then
                    InsertDataSummary(i, 0) = True
                Else
                    InsertDataSummary(i, 0) = False
                End If
            Next
        End If
    End Sub
    Public ReadOnly Property Selected(ByVal item As Integer) As Boolean
        Get
            If item <= NumAdded And item > 0 Then
                Selected = InsertDataSummary(item, 0)
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
Public Class InsertData
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

                Dim Data As New FireCollection
            Case InsertDataType.ThermalProperty
                Me.InsertDataSummary(0, InsertThermalNum.Material) = "Material"
                Me.InsertDataSummary(0, InsertThermalNum.Conductivity) = "Conductivity"
                Me.InsertDataSummary(0, InsertThermalNum.SpecificHeat) = "Specific Heat"
                Me.InsertDataSummary(0, InsertThermalNum.Density) = "Density"
                Me.InsertDataSummary(0, InsertThermalNum.Thickness) = "Thickness"
                Me.InsertDataSummary(0, InsertThermalNum.Emissivity) = "Emissivity"

                Me.InsertDataSummary.Location = New System.Drawing.Point(154, 12)
                Me.InsertDataSummary.Size = New System.Drawing.Size(700, 156)

                Dim Data As New ThermalPropertiesCollection
        End Select

    End Sub
    Public Sub ReadSelectedData(ByVal Type As Integer, ByVal Filename As String)
        Select Case Type
            Case InsertDataType.Fire
            Case InsertDataType.ThermalProperty
        End Select

    End Sub
End Class
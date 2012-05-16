Public Class EditFireObject
    Friend CurrentFireObject As Integer
    Friend aFireObject As New Fire
    Private Sub Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireMaterial.SelectedIndexChanged, FireC.Leave, FireH.Leave, FireO.Leave, FireN.Leave, FireCl.Leave, FireHoC.Leave, FireSootYield.Leave, FireCOYield.Leave, FireRadiativeFraction.Leave, FireName.Leave
        Dim numPoints As Integer, ir As Integer

        If sender Is Me.FireName Then aFireObject.Name = Me.FireName.Text
        If sender Is Me.FireMaterial Then aFireObject.Material = myThermalProperties.GetShortName(sender.text)
        If sender Is Me.FireC Then
            If Val(Me.FireC.Text) > 0 Then
                aFireObject.ChemicalFormula(formula.C) = Val(Me.FireC.Text)
            Else
                aFireObject.ChemicalFormula(formula.C) = 1
            End If
        End If
        If sender Is Me.FireH Then aFireObject.ChemicalFormula(formula.H) = Val(Me.FireH.Text)
        If sender Is Me.FireO Then aFireObject.ChemicalFormula(formula.O) = Val(Me.FireO.Text)
        If sender Is Me.FireN Then aFireObject.ChemicalFormula(formula.N) = Val(Me.FireN.Text)
        If sender Is Me.FireCl Then aFireObject.ChemicalFormula(formula.Cl) = Val(Me.FireCl.Text)
        If sender Is Me.FireC Or sender Is Me.FireH Or sender Is Me.FireO Or sender Is Me.FireN Or sender Is Me.FireCl Then
            numPoints = CountGridPoints(Me.FireDataSS)
            For ir = 1 To numPoints
                Me.FireDataSS(ir, Fire.FireHC) = aFireObject.ChemicalFormula(formula.H) * 1.00794 / (aFireObject.ChemicalFormula(formula.C) * 12.0107)
                Me.FireDataSS(ir, Fire.FireHCN) = (1.00794 + 12.0107 + 14.01) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.N)
                Me.FireDataSS(ir, Fire.FireHCl) = (1.00794 + 35.453) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.Cl)
            Next
            CopyFireData(aFireObject)
        End If
        If sender Is Me.FireHoC Then
            If Val(Me.FireHoC.Text) <> aFireObject.HeatofCombustion Then
                aFireObject.HeatofCombustion = Val(Me.FireHoC.Text)
                numPoints = CountGridPoints(Me.FireDataSS)
                For ir = 1 To numPoints
                    Me.FireDataSS(ir, Fire.FireMdot) = Me.FireDataSS(ir, Fire.FireHRR) / aFireObject.HeatofCombustion
                Next
            End If
            CopyFireData(aFireObject)
        End If
        If sender Is Me.FireSootYield Then
            numPoints = CountGridPoints(Me.FireDataSS)
            For ir = 1 To numPoints
                Me.FireDataSS(ir, Fire.FireSoot) = Val(Me.FireSootYield.Text)
            Next
            CopyFireData(aFireObject)
        End If
        If sender Is Me.FireCOYield Then
            numPoints = CountGridPoints(Me.FireDataSS)
            For ir = 1 To numPoints
                If Me.FireCOYield.Text <> "" Then
                    Me.FireDataSS(ir, Fire.FireCO) = Val(Me.FireCOYield.Text)
                Else
                    Me.FireDataSS(ir, Fire.FireCO) = (0.012 * Val(Me.FireC.Text) / aFireObject.MolarMass * 0.0014 + 0.37 * Me.FireDataSS(ir, Fire.FireSoot))
                End If
            Next
            CopyFireData(aFireObject)
        End If
        If sender Is Me.FireRadiativeFraction Then aFireObject.RadiativeFraction = Val(Me.FireRadiativeFraction.Text)
        UpdateFireObject()
    End Sub
    Private Sub UpdateFireObject()
        Dim aFireTimeSeries(12, 0) As Single, numPoints As Integer
        Dim ir As Integer, ic As Integer

        ' Update details for currently selected fire object
        Me.FireName.Text = aFireObject.Name
        Me.FireMaterial.Text = myThermalProperties.GetLongName(aFireObject.Material)
        Me.FireC.Text = aFireObject.ChemicalFormula(formula.C).ToString
        Me.FireH.Text = aFireObject.ChemicalFormula(formula.H).ToString
        Me.FireO.Text = aFireObject.ChemicalFormula(formula.O).ToString
        Me.FireN.Text = aFireObject.ChemicalFormula(formula.N).ToString
        Me.FireCl.Text = aFireObject.ChemicalFormula(formula.Cl).ToString
        Me.FireHoC.Text = aFireObject.HeatofCombustion.ToString + myUnits.Convert(UnitsNum.HoC).Units
        Me.FireRadiativeFraction.Text = aFireObject.RadiativeFraction.ToString
        aFireObject.GetFireData(aFireTimeSeries, numPoints)
        ClearGrid(Me.FireDataSS)
        Me.FireDataSS(0, 0) = "Time" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireTime).Units.Substring(1) + ")"
        Me.FireDataSS(0, 1) = "Mdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireMdot).Units.Substring(1) + ")"
        Me.FireDataSS(0, 2) = "Qdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireQdot).Units.Substring(1) + ")"
        Me.FireDataSS(0, 3) = "Height" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireHeight).Units.Substring(1) + ")"
        Me.FireDataSS(0, 4) = "Area" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireArea).Units.Substring(1) + ")"
        Me.FireDataSS.AutoSizeRow(0)

        If numPoints >= 0 Then
            For ir = 0 To numPoints
                For ic = 0 To 12
                    Me.FireDataSS(ir + 1, ic) = aFireTimeSeries(ic, ir)
                Next
            Next
        End If

        UpdateFirePlot()
        If aFireObject.Changed Then
            Me.Text = "Add Fire Object *"
        Else
            Me.Text = "Add Fire Object"
        End If
    End Sub
    Private Sub UpdateFirePlot()
        Dim aFireData(12, 0) As Single, numPoints As Integer
        Dim x() As Single, y() As Single, j As Integer, iSelectedColumn As Integer
        Me.FireObjectPlot.Clear()
        aFireObject.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        iSelectedColumn = Me.FireDataSS.ColSel
        If iSelectedColumn < 1 Then iSelectedColumn = Fire.FireHRR
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(iSelectedColumn, j)
        Next
        Dim lp As New NPlot.LinePlot(y, x)
        Me.FireObjectPlot.Add(lp)
        Me.FireObjectPlot.Title = aFireObject.Name
        Me.FireObjectPlot.Refresh()
    End Sub
    Private Sub CopyFireData(ByVal aFireObject As Fire)
        ' Copies time dependent data from the display spreadsheet to the appropriate fire object data array
        Dim numPoints As Integer, ir As Integer, ic As Integer
        numPoints = CountGridPoints(Me.FireDataSS)
        If numPoints > 0 Then
            Dim aFireTimeSeries(12, numPoints - 1) As Single
            For ir = 0 To numPoints - 1
                For ic = 0 To 12
                    aFireTimeSeries(ic, ir) = Val(Me.FireDataSS(ir + 1, ic))
                Next
            Next
            aFireObject.SetFireData(aFireTimeSeries)
        End If
    End Sub
    Private Sub ClearGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        Dim i As Integer
        ' Erase the contents of a grid, leaving only the header row
        For i = 2 To obj.Rows.Count
            obj.Clear(C1.Win.C1FlexGrid.ClearFlags.Content, i - 1, 0, i - 1, obj.Cols.Count - 1)
        Next
    End Sub
    Public ReadOnly Property CountGridPoints(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid) As Integer
        ' Find the last non-blank row of a grid on the GUI
        Get
            Dim LastRow As Integer, ir As Integer
            Dim s As String
            LastRow = -1
            ir = obj.Rows.Count
            Do
                ir = ir - 1
                s = CType(obj(ir, 0), String) + " "
            Loop Until s <> " " Or ir < 0
            If LastRow < ir Then LastRow = ir
            Return LastRow
        End Get
    End Property
    Public Sub InitThermalPropertyList(ByVal obj As ComboBox)
        Dim i As Integer
        obj.Items.Clear()
        obj.Items.Add("Off")
        If myThermalProperties.Count > 0 Then
            For i = 0 To myThermalProperties.Count - 1
                obj.Items.Add(myThermalProperties.Item(i).Name)
            Next
        End If
    End Sub
    Private Sub EditFireObject_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        If myFireObjects.Count > 0 Then
            FireCopy(myFireObjects(CurrentFireObject), aFireObject)
            InitThermalPropertyList(Me.FireMaterial)
            Me.FireDataSS.Cols.Fixed = 0
            UpdateFireObject()
        End If
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.BeforeRowColChange
        Dim numPoints As Integer
        numPoints = CountGridPoints(Me.FireDataSS)
        ' Copy the values from the spreadsheet to the array for fire data, then put them in the FireObject data structure
        If Me.FireDataSS.ColSel = Fire.FireHRR Then
            Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireMdot) = Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireHRR) / aFireObject.HeatofCombustion
        End If
        CopyFireData(aFireObject)
        UpdateFireObject()
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.AfterRowColChange
        UpdateFirePlot()
    End Sub
    Private Sub FireData_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FireDataSS.KeyDown
        Dim aChar As String
        aChar = e.KeyCode.ToString
        If e.Control Then
            If aChar = "Delete" Then
                If FireDataSS.Row > 0 And FireDataSS.Col >= 0 Then
                    FireDataSS(FireDataSS.Row, FireDataSS.Col) = " "
                End If
            End If
        End If
        If e.Alt Then
            If aChar = "Delete" Then
                If FireDataSS.Row > 0 And FireDataSS.Col >= 0 Then
                    If CurrentFireObject >= 0 And CurrentFireObject < TempFireObjects.Count Then
                        Dim numPoints As Integer, i As Integer, j As Integer
                        numPoints = CountGridPoints(Me.FireDataSS)
                        If numPoints > FireDataSS.Row Then
                            For i = FireDataSS.Row To numPoints
                                For j = 0 To FireDataSS.Cols.Count - 1
                                    FireDataSS(i, j) = FireDataSS(i + 1, j)
                                Next
                            Next
                        End If
                        For j = 0 To FireDataSS.Cols.Count - 1
                            FireDataSS(numPoints, j) = Nothing
                        Next
                        Dim aFireObject As New Fire
                        aFireObject = TempFireObjects(CurrentFireObject)
                        CopyFireData(aFireObject)
                        TempFireObjects(CurrentFireObject) = aFireObject
                        UpdateFireObject()
                    End If
                End If
            End If
            If aChar = "Insert" Then
                If FireDataSS.Row > 0 Then
                    Dim numPoints As Integer, i As Integer, j As Integer
                    numPoints = Me.CountGridPoints(Me.FireDataSS)
                    If numPoints > FireDataSS.Row Then
                        For i = numPoints To FireDataSS.Row Step -1
                            For j = 0 To FireDataSS.Cols.Count - 1
                                FireDataSS(i + 1, j) = FireDataSS(i, j)
                            Next
                        Next
                        For j = 0 To FireDataSS.Cols.Count - 1
                            FireDataSS(FireDataSS.Row, j) = 0
                        Next
                    End If

                End If
            End If
        End If
    End Sub
End Class
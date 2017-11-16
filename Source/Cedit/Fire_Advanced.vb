Public Class Fire_Advanced
    Private CurrentFire As Integer = -1
    Private MainWin As CeditMain
    Private aFire As New Fire
    Public Sub New(ByVal ParentWindow As Object)

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        MainWin = ParentWindow
    End Sub
    WriteOnly Property Index() As Integer
        Set(Value As Integer)
            CurrentFire = Value
            Dim FromFire As New Fire
            FromFire = myFires(CurrentFire)
            FireCopy(FromFire, aFire)
            UpdateFire()
        End Set
    End Property
    ReadOnly Property ReturnedFire() As Fire
        Get
            ReturnedFire = aFire
        End Get
    End Property
    Private Sub FireChanged(sender As Object, e As EventArgs) Handles FireHRR.TextChanged, FireHoC.TextChanged, FireRadiativeFraction.TextChanged, FireArea.TextChanged, FireHeight.TextChanged, FireCOYield.TextChanged, FireHClYield.TextChanged, FireHCNYield.TextChanged, FireSootYield.TextChanged, FireTSYield.TextChanged
        If sender Is FireHRR Then aFire.HRR = Val(FireHRR.Text)
        If sender Is FireHoC Then aFire.HeatofCombustion = Val(FireHoC.Text)
        If sender Is FireRadiativeFraction Then aFire.RadiativeFraction = Val(FireRadiativeFraction.Text)
        If sender Is FireArea Then aFire.Area = Val(FireArea.Text)
        If sender Is FireHeight Then aFire.Height = Val(FireHeight.Text)
        If sender Is FireCOYield Then aFire.COYield = Val(FireCOYield.Text)
        If sender Is FireHClYield Then aFire.HClYield = Val(FireHClYield.Text)
        If sender Is FireHCNYield Then aFire.HCNYield = Val(FireHCNYield.Text)
        If sender Is FireSootYield Then aFire.SootYield = Val(FireSootYield.Text)
        If sender Is FireTSYield Then aFire.TSYield = Val(FireTSYield.Text)
    End Sub
    Private Sub UpdateFire()
        Dim afireTimeSeries(12, 0) As Single, NumPoints As Integer, ir As Integer, ic As Integer

        InitRampList(FireHRRName, Ramp.TypeHRR)
        InitRampList(FireAreaName, Ramp.TypeArea)
        InitRampList(FireHeightName, Ramp.TypeLength)
        InitRampList(FireCOName, Ramp.TypeFrac)
        InitRampList(FireHClName, Ramp.TypeFrac)
        InitRampList(FireHCNName, Ramp.TypeFrac)
        InitRampList(FireSootName, Ramp.TypeFrac)
        InitRampList(FireTSName, Ramp.TypeFrac)

        FireHRR.Text = aFire.HRR.ToString + myUnits.Convert(UnitsNum.HRR).Units
        FireHoC.Text = aFire.HeatofCombustion.ToString + myUnits.Convert(UnitsNum.HoC).Units
        FireRadiativeFraction.Text = aFire.RadiativeFraction.ToString
        FireArea.Text = aFire.Area.ToString + myUnits.Convert(UnitsNum.Area).Units
        FireHeight.Text = aFire.Height.ToString + myUnits.Convert(UnitsNum.Length).Units
        FireCOYield.Text = aFire.COYield.ToString
        FireHClYield.Text = aFire.HClYield.ToString
        FireHCNYield.Text = aFire.HCNYield.ToString
        FireSootYield.Text = aFire.SootYield.ToString
        FireTSYield.Text = aFire.TSYield.ToString

        aFire.GetFireData(afireTimeSeries, NumPoints)
        MainWin.UpdateGUI.ClearGrid(FireDataSS)
        FireDataSS(0, 0) = "Time" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireTime).Units.Substring(1) + ")"
        FireDataSS(0, 1) = "Mdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireMdot).Units.Substring(1) + ")"
        FireDataSS(0, 2) = "HRR" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireQdot).Units.Substring(1) + ")"
        FireDataSS(0, 3) = "Height" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireHeight).Units.Substring(1) + ")"
        FireDataSS(0, 4) = "Area" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireArea).Units.Substring(1) + ")"
        FireDataSS.AutoSizeRow(0)

        If NumPoints >= 0 Then
            For ir = 0 To NumPoints
                For ic = 0 To 12
                    FireDataSS(ir + 1, ic) = afireTimeSeries(ic, ir)
                Next
            Next
        End If

        UpdateFirePlot()
    End Sub
    Private Sub UpdateFireData()
        Dim numPoints As Integer, ir As Integer
        If Val(FireHoC.Text) <> aFire.HeatofCombustion Then
            aFire.HeatofCombustion = Val(FireHoC.Text)
            numPoints = MainWin.CountGridPoints(FireDataSS)
            For ir = 1 To numPoints
                FireDataSS(ir, Fire.FireMdot) = FireDataSS(ir, Fire.FireHRR) / aFire.HeatofCombustion
            Next
        End If
        CopyFireData(aFire)

    End Sub

    Friend Sub CopyFireData(ByVal aFire As Fire)
        ' Copies time dependent data from the display spreadsheet to the appropriate fire object data array
        Dim numPoints As Integer, ir As Integer, ic As Integer
        numPoints = MainWin.CountGridPoints(FireDataSS)
        If numPoints > 0 Then
            Dim aFireTimeSeries(12, numPoints - 1) As Single
            For ir = 0 To numPoints - 1
                For ic = 0 To 12
                    aFireTimeSeries(ic, ir) = Val(FireDataSS(ir + 1, ic))
                Next
            Next
            aFire.SetFireData(aFireTimeSeries)
        End If
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs)
        Dim aFire As New Fire
        Dim numPoints As Integer
        If CurrentFire >= 0 And myFires.Count > 0 Then
            aFire = myFires(CurrentFire)
            numPoints = MainWin.CountGridPoints(FireDataSS)
            ' Copy the values from the spreadsheet to the array for fire data, then put them in the FireObject data structure
            If FireDataSS.ColSel = Fire.FireHRR Then
                FireDataSS(FireDataSS.RowSel, Fire.FireMdot) = FireDataSS(FireDataSS.RowSel, Fire.FireHRR) / aFire.HeatofCombustion
            End If
            CopyFireData(aFire)
            myFires(CurrentFire) = aFire
            UpdateFire()
        End If
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs)
        UpdateFire()
    End Sub
    Private Sub Data_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FireDataSS.KeyDown
        Dim aChar As String
        aChar = e.KeyCode.ToString
        If e.Control Then
            If aChar = "Delete" Then
                If sender.Row > 0 And sender.Col >= 0 Then
                    sender(sender.Row, sender.Col) = " "
                End If
            End If
        End If
        If e.Alt Then
            If aChar = "Delete" Then
                Dim Current As Integer, Count As Integer
                Current = CurrentFire
                Count = myFires.Count
                If sender.Row > 0 And sender.Col >= 0 Then
                    If Current >= 0 And Current < Count Then
                        Dim numPoints As Integer, i As Integer, j As Integer
                        numPoints = MainWin.CountGridPoints(sender)
                        If numPoints > sender.Row Then
                            For i = sender.Row To numPoints
                                For j = 0 To sender.Cols.Count - 1
                                    sender(i, j) = sender(i + 1, j)
                                Next
                            Next
                        End If
                        For j = 0 To sender.Cols.Count - 1
                            sender(numPoints, j) = Nothing
                        Next
                        Dim aFire As New Fire
                        aFire = myFires(CurrentFire)
                        CopyFireData(aFire)
                        myFires(CurrentFire) = aFire
                        UpdateFire()
                    End If
                End If
            End If
            If aChar = "Insert" Then
                If sender.Row > 0 Then
                    Dim numPoints As Integer, i As Integer, j As Integer
                    numPoints = MainWin.CountGridPoints(sender)
                    If numPoints > sender.Row Then
                        For i = numPoints To sender.Row Step -1
                            For j = 0 To sender.Cols.Count - 1
                                sender(i + 1, j) = sender(i, j)
                            Next
                        Next
                        For j = 0 To sender.Cols.Count - 1
                            sender(sender.Row, j) = 0
                        Next
                    End If

                End If
            End If
        End If
    End Sub
    Private Sub UpdateFirePlot()
        Dim aFireData(12, 0) As Single, numPoints As Integer, iSelectedColumn As Integer
        Dim x() As Single, y() As Single, j As Integer
        MainWin.HRRPlot.Clear()
        aFire.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        iSelectedColumn = Fire.FireHRR
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(iSelectedColumn, j)
        Next
        Dim lp As New NPlot.LinePlot(y, x)
        MainWin.HRRPlot.Add(lp)
        MainWin.HRRPlot.Title = aFire.Name + ": " + FireDataSS(0, iSelectedColumn).ToString.Replace(Chr(10), " ")
        MainWin.HRRPlot.Refresh()


        FirePlot.Clear()
        iSelectedColumn = FireDataSS.ColSel
        If iSelectedColumn < 1 Then iSelectedColumn = Fire.FireHRR
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(iSelectedColumn, j)
        Next
        lp = New NPlot.LinePlot(y, x)
        FirePlot.Add(lp)
        FirePlot.Title = aFire.Name + ": " + FireDataSS(0, iSelectedColumn).ToString.Replace(Chr(10), " ")
        FirePlot.Refresh()
    End Sub
    Public Sub InitRampList(ByVal obj As ComboBox, RampType As String)
        Dim i As Integer, current As String
        current = obj.Text
        obj.Items.Clear()
        If myRamps.Count > 0 Then
            For i = 0 To myRamps.Count - 1
                If myRamps.Item(i).Type = RampType Then
                    obj.Items.Add(myRamps.Item(i).Name)
                    If myRamps.Item(i).Name = current Then obj.SelectedIndex = i
                End If
            Next
        End If
    End Sub
End Class
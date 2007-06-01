Public Class t2Fire
    Private aGrowthTime As Single = 300.0
    Private aSteadyTime As Single = 300.0
    Private aPeakHRR As Single = 1054000.0
    Private aDecayTime As Single = 300.0
    Private t2Constants(,) As Single = {{0.0, 0.0, 0.0, 0.0}, {600.0, 300.0, 1054000.0, 600.0}, {300.0, 300.0, 1054000.0, 300.0}, {150.0, 300.0, 1054000.0, 150.0}, {75.0, 300.0, 1054000.0, 75.0}}
    Private Enum t2FireType
        Slow = 1
        Medium
        Fast
        Ultrafast
    End Enum
    Public Property GrowthTime() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aGrowthTime)
        End Get
        Set(ByVal value As Single)
            aGrowthTime = myUnits.Convert(UnitsNum.Time).ToSI(value)
        End Set
    End Property
    Public Property SteadyTime() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSteadyTime)
        End Get
        Set(ByVal value As Single)
            aSteadyTime = myUnits.Convert(UnitsNum.Time).ToSI(value)
        End Set
    End Property
    Public Property PeakHRR() As Single
        Get
            Return myUnits.Convert(UnitsNum.HRR).FromSI(aPeakHRR)
        End Get
        Set(ByVal value As Single)
            aPeakHRR = myUnits.Convert(UnitsNum.HRR).ToSI(value)
        End Set
    End Property
    Public Property DecayTime() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aDecayTime)
        End Get
        Set(ByVal value As Single)
            aDecayTime = myUnits.Convert(UnitsNum.Time).ToSI(value)
        End Set
    End Property
    Private Sub t2Fire_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Updatet2Fire()
    End Sub
    Private Sub t2_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles t2GrowthTime.Leave, t2SteadyTime.Leave, _
                        t2PeakHRR.Leave, t2DecayTime.Leave, t2GrowthType.SelectedIndexChanged
        Dim iType As Integer
        If sender Is Me.t2GrowthTime Then
            GrowthTime = Val(Me.t2GrowthTime.Text)
            Me.t2GrowthType.SelectedIndex = 0
        End If
        If sender Is Me.t2SteadyTime Then
            SteadyTime = Val(Me.t2SteadyTime.Text)
            Me.t2GrowthType.SelectedIndex = 0
        End If
        If sender Is Me.t2PeakHRR Then
            PeakHRR = Val(Me.t2PeakHRR.Text)
            Me.t2GrowthType.SelectedIndex = 0
        End If
        If sender Is Me.t2DecayTime Then
            DecayTime = Val(Me.t2DecayTime.Text)
            Me.t2GrowthType.SelectedIndex = 0
        End If
        If sender Is Me.t2GrowthType Then
            iType = Me.t2GrowthType.SelectedIndex
            If iType >= 1 And iType <= 4 Then
                myUnits.SI = True
                GrowthTime = t2Constants(iType, 0)
                SteadyTime = t2Constants(iType, 1)
                PeakHRR = t2Constants(iType, 2)
                DecayTime = t2Constants(iType, 3)
                myUnits.SI = False
            End If
        End If
        Updatet2Fire()
    End Sub
    Private Sub Updatet2Fire()
        Dim i As Integer
        Me.t2GrowthTime.Text = GrowthTime.ToString + myUnits.ConvertFireData(UnitsNum.FireTime).Units
        Me.t2SteadyTime.Text = SteadyTime.ToString + myUnits.ConvertFireData(UnitsNum.FireTime).Units
        Me.t2PeakHRR.Text = PeakHRR.ToString + myUnits.ConvertFireData(UnitsNum.FireQdot).Units
        Me.t2DecayTime.Text = DecayTime.ToString + myUnits.ConvertFireData(UnitsNum.FireTime).Units
        Updatet2Plot()
    End Sub
    Private Sub Updatet2Plot()
        Dim aFireObject As New Fire
        Dim aFireData(11, 0) As Single, numPoints As Integer
        Dim x() As Single, y() As Single, j As Integer, iSelectedColumn As Integer
        aFireObject = New Fire(GrowthTime, PeakHRR, SteadyTime, DecayTime)
        Me.FireObjectPlot.Clear()
        aFireObject.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        iSelectedColumn = Fire.FireHRR
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(iSelectedColumn, j)
        Next
        Dim lp As New NPlot.LinePlot(y, x)
        Me.FireObjectPlot.Add(lp)
        Me.FireObjectPlot.Title = aFireObject.Name
        Me.FireObjectPlot.Refresh()
        If GrowthTime > 0 And PeakHRR > 0 And SteadyTime > 0 And DecayTime > 0 Then
            Me.t2OK.Enabled = True
            Me.t2StripStatusLabel.Text = ""
        Else
            Me.t2OK.Enabled = False
            Me.t2StripStatusLabel.Text = "Constants must all be greater than zero"
        End If
    End Sub
End Class

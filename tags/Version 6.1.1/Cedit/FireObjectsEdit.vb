Public Class FireObjectsEdit
    Inherits System.Windows.Forms.Form
    Friend CurrentFireObject As Integer


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents GroupFireObject As System.Windows.Forms.GroupBox
    Friend WithEvents FireNewObject As System.Windows.Forms.Button
    Friend WithEvents FireMaterial As System.Windows.Forms.ComboBox
    Friend WithEvents Label84 As System.Windows.Forms.Label
    Friend WithEvents FireDupObject As System.Windows.Forms.Button
    Friend WithEvents FireHoG As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents FireHoC As System.Windows.Forms.TextBox
    Friend WithEvents Label56 As System.Windows.Forms.Label
    Friend WithEvents FireTotalMass As System.Windows.Forms.TextBox
    Friend WithEvents FireWidth As System.Windows.Forms.TextBox
    Friend WithEvents Label64 As System.Windows.Forms.Label
    Friend WithEvents FireLength As System.Windows.Forms.TextBox
    Friend WithEvents FireThickness As System.Windows.Forms.TextBox
    Friend WithEvents Label65 As System.Windows.Forms.Label
    Friend WithEvents Label39 As System.Windows.Forms.Label
    Friend WithEvents Label38 As System.Windows.Forms.Label
    Friend WithEvents FireVolitilizationTemp As System.Windows.Forms.TextBox
    Friend WithEvents Label59 As System.Windows.Forms.Label
    Friend WithEvents Label57 As System.Windows.Forms.Label
    Friend WithEvents FireMolarMass As System.Windows.Forms.TextBox
    Friend WithEvents Label94 As System.Windows.Forms.Label
    Friend WithEvents FireRadiativeFraction As System.Windows.Forms.TextBox
    Friend WithEvents FireData As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents FireRemoveObject As System.Windows.Forms.Button
    Friend WithEvents FireName As System.Windows.Forms.TextBox
    Friend WithEvents FireObjectSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents FireObjectPlot As NPlot.Windows.PlotSurface2D
    Friend WithEvents FireObjectsOK As System.Windows.Forms.Button
    Friend WithEvents FireObjectsCancel As System.Windows.Forms.Button
    Friend WithEvents FireNewt2 As System.Windows.Forms.Button
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FireObjectsEdit))
        Me.GroupFireObject = New System.Windows.Forms.GroupBox
        Me.FireMaterial = New System.Windows.Forms.ComboBox
        Me.Label84 = New System.Windows.Forms.Label
        Me.FireHoG = New System.Windows.Forms.TextBox
        Me.Label17 = New System.Windows.Forms.Label
        Me.FireHoC = New System.Windows.Forms.TextBox
        Me.Label56 = New System.Windows.Forms.Label
        Me.FireTotalMass = New System.Windows.Forms.TextBox
        Me.FireWidth = New System.Windows.Forms.TextBox
        Me.Label64 = New System.Windows.Forms.Label
        Me.FireLength = New System.Windows.Forms.TextBox
        Me.FireThickness = New System.Windows.Forms.TextBox
        Me.Label65 = New System.Windows.Forms.Label
        Me.Label39 = New System.Windows.Forms.Label
        Me.Label38 = New System.Windows.Forms.Label
        Me.FireVolitilizationTemp = New System.Windows.Forms.TextBox
        Me.Label59 = New System.Windows.Forms.Label
        Me.Label57 = New System.Windows.Forms.Label
        Me.FireMolarMass = New System.Windows.Forms.TextBox
        Me.Label94 = New System.Windows.Forms.Label
        Me.FireRadiativeFraction = New System.Windows.Forms.TextBox
        Me.FireNewObject = New System.Windows.Forms.Button
        Me.FireDupObject = New System.Windows.Forms.Button
        Me.FireData = New C1.Win.C1FlexGrid.C1FlexGrid
        Me.FireObjectSummary = New C1.Win.C1FlexGrid.C1FlexGrid
        Me.FireName = New System.Windows.Forms.TextBox
        Me.Label9 = New System.Windows.Forms.Label
        Me.FireRemoveObject = New System.Windows.Forms.Button
        Me.FireObjectPlot = New NPlot.Windows.PlotSurface2D
        Me.FireObjectsOK = New System.Windows.Forms.Button
        Me.FireObjectsCancel = New System.Windows.Forms.Button
        Me.C1SizerLight1 = New C1.Win.C1Sizer.C1SizerLight(Me.components)
        Me.FireNewt2 = New System.Windows.Forms.Button
        Me.GroupFireObject.SuspendLayout()
        CType(Me.FireData, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FireObjectSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupFireObject
        '
        Me.GroupFireObject.Controls.Add(Me.FireMaterial)
        Me.GroupFireObject.Controls.Add(Me.Label84)
        Me.GroupFireObject.Controls.Add(Me.FireHoG)
        Me.GroupFireObject.Controls.Add(Me.Label17)
        Me.GroupFireObject.Controls.Add(Me.FireHoC)
        Me.GroupFireObject.Controls.Add(Me.Label56)
        Me.GroupFireObject.Controls.Add(Me.FireTotalMass)
        Me.GroupFireObject.Controls.Add(Me.FireWidth)
        Me.GroupFireObject.Controls.Add(Me.Label64)
        Me.GroupFireObject.Controls.Add(Me.FireLength)
        Me.GroupFireObject.Controls.Add(Me.FireThickness)
        Me.GroupFireObject.Controls.Add(Me.Label65)
        Me.GroupFireObject.Controls.Add(Me.Label39)
        Me.GroupFireObject.Controls.Add(Me.Label38)
        Me.GroupFireObject.Controls.Add(Me.FireVolitilizationTemp)
        Me.GroupFireObject.Controls.Add(Me.Label59)
        Me.GroupFireObject.Controls.Add(Me.Label57)
        Me.GroupFireObject.Controls.Add(Me.FireMolarMass)
        Me.GroupFireObject.Controls.Add(Me.Label94)
        Me.GroupFireObject.Controls.Add(Me.FireRadiativeFraction)
        Me.GroupFireObject.Location = New System.Drawing.Point(20, 256)
        Me.GroupFireObject.Name = "GroupFireObject"
        Me.GroupFireObject.Size = New System.Drawing.Size(376, 256)
        Me.GroupFireObject.TabIndex = 6
        Me.GroupFireObject.TabStop = False
        Me.GroupFireObject.Text = "Details"
        '
        'FireMaterial
        '
        Me.FireMaterial.ItemHeight = 13
        Me.FireMaterial.Location = New System.Drawing.Point(120, 24)
        Me.FireMaterial.Name = "FireMaterial"
        Me.FireMaterial.Size = New System.Drawing.Size(192, 21)
        Me.FireMaterial.TabIndex = 7
        Me.FireMaterial.Text = "GYPSUM"
        '
        'Label84
        '
        Me.Label84.AutoSize = True
        Me.Label84.Location = New System.Drawing.Point(64, 24)
        Me.Label84.Name = "Label84"
        Me.Label84.Size = New System.Drawing.Size(47, 13)
        Me.Label84.TabIndex = 134
        Me.Label84.Text = "Material:"
        Me.Label84.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireHoG
        '
        Me.FireHoG.Location = New System.Drawing.Point(276, 112)
        Me.FireHoG.Name = "FireHoG"
        Me.FireHoG.Size = New System.Drawing.Size(80, 20)
        Me.FireHoG.TabIndex = 14
        Me.FireHoG.Text = "Default"
        '
        'Label17
        '
        Me.Label17.Location = New System.Drawing.Point(196, 112)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(72, 24)
        Me.Label17.TabIndex = 130
        Me.Label17.Text = "Heat of Gasification:"
        Me.Label17.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireHoC
        '
        Me.FireHoC.Location = New System.Drawing.Point(276, 80)
        Me.FireHoC.Name = "FireHoC"
        Me.FireHoC.Size = New System.Drawing.Size(80, 20)
        Me.FireHoC.TabIndex = 13
        Me.FireHoC.Text = "50000000 J/kg"
        '
        'Label56
        '
        Me.Label56.Location = New System.Drawing.Point(196, 80)
        Me.Label56.Name = "Label56"
        Me.Label56.Size = New System.Drawing.Size(72, 24)
        Me.Label56.TabIndex = 116
        Me.Label56.Text = "Heat of Combustion:"
        Me.Label56.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireTotalMass
        '
        Me.FireTotalMass.Location = New System.Drawing.Point(92, 208)
        Me.FireTotalMass.Name = "FireTotalMass"
        Me.FireTotalMass.Size = New System.Drawing.Size(80, 20)
        Me.FireTotalMass.TabIndex = 12
        '
        'FireWidth
        '
        Me.FireWidth.Location = New System.Drawing.Point(92, 112)
        Me.FireWidth.Name = "FireWidth"
        Me.FireWidth.Size = New System.Drawing.Size(80, 20)
        Me.FireWidth.TabIndex = 9
        '
        'Label64
        '
        Me.Label64.AutoSize = True
        Me.Label64.Location = New System.Drawing.Point(42, 80)
        Me.Label64.Name = "Label64"
        Me.Label64.Size = New System.Drawing.Size(43, 13)
        Me.Label64.TabIndex = 118
        Me.Label64.Text = "Length:"
        Me.Label64.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireLength
        '
        Me.FireLength.Location = New System.Drawing.Point(94, 80)
        Me.FireLength.Name = "FireLength"
        Me.FireLength.Size = New System.Drawing.Size(80, 20)
        Me.FireLength.TabIndex = 8
        '
        'FireThickness
        '
        Me.FireThickness.Location = New System.Drawing.Point(92, 144)
        Me.FireThickness.Name = "FireThickness"
        Me.FireThickness.Size = New System.Drawing.Size(80, 20)
        Me.FireThickness.TabIndex = 10
        '
        'Label65
        '
        Me.Label65.AutoSize = True
        Me.Label65.Location = New System.Drawing.Point(28, 144)
        Me.Label65.Name = "Label65"
        Me.Label65.Size = New System.Drawing.Size(59, 13)
        Me.Label65.TabIndex = 122
        Me.Label65.Text = "Thickness:"
        Me.Label65.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label39
        '
        Me.Label39.AutoSize = True
        Me.Label39.Location = New System.Drawing.Point(52, 112)
        Me.Label39.Name = "Label39"
        Me.Label39.Size = New System.Drawing.Size(38, 13)
        Me.Label39.TabIndex = 119
        Me.Label39.Text = "Width:"
        Me.Label39.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label38
        '
        Me.Label38.AutoSize = True
        Me.Label38.Location = New System.Drawing.Point(20, 208)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(62, 13)
        Me.Label38.TabIndex = 116
        Me.Label38.Text = "Total Mass:"
        Me.Label38.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireVolitilizationTemp
        '
        Me.FireVolitilizationTemp.Location = New System.Drawing.Point(276, 144)
        Me.FireVolitilizationTemp.Name = "FireVolitilizationTemp"
        Me.FireVolitilizationTemp.Size = New System.Drawing.Size(80, 20)
        Me.FireVolitilizationTemp.TabIndex = 15
        Me.FireVolitilizationTemp.Text = "20 °C"
        '
        'Label59
        '
        Me.Label59.Location = New System.Drawing.Point(212, 176)
        Me.Label59.Name = "Label59"
        Me.Label59.Size = New System.Drawing.Size(56, 24)
        Me.Label59.TabIndex = 124
        Me.Label59.Text = "Radiative Fraction:"
        Me.Label59.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label57
        '
        Me.Label57.Location = New System.Drawing.Point(196, 144)
        Me.Label57.Name = "Label57"
        Me.Label57.Size = New System.Drawing.Size(72, 24)
        Me.Label57.TabIndex = 114
        Me.Label57.Text = "Volitilization Temperature:"
        Me.Label57.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireMolarMass
        '
        Me.FireMolarMass.Location = New System.Drawing.Point(92, 176)
        Me.FireMolarMass.Name = "FireMolarMass"
        Me.FireMolarMass.Size = New System.Drawing.Size(80, 20)
        Me.FireMolarMass.TabIndex = 11
        '
        'Label94
        '
        Me.Label94.AutoSize = True
        Me.Label94.Location = New System.Drawing.Point(20, 176)
        Me.Label94.Name = "Label94"
        Me.Label94.Size = New System.Drawing.Size(64, 13)
        Me.Label94.TabIndex = 128
        Me.Label94.Text = "Molar Mass:"
        Me.Label94.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireRadiativeFraction
        '
        Me.FireRadiativeFraction.Location = New System.Drawing.Point(276, 176)
        Me.FireRadiativeFraction.Name = "FireRadiativeFraction"
        Me.FireRadiativeFraction.Size = New System.Drawing.Size(80, 20)
        Me.FireRadiativeFraction.TabIndex = 17
        Me.FireRadiativeFraction.Text = "0.3"
        '
        'FireNewObject
        '
        Me.FireNewObject.Location = New System.Drawing.Point(411, 184)
        Me.FireNewObject.Name = "FireNewObject"
        Me.FireNewObject.Size = New System.Drawing.Size(75, 23)
        Me.FireNewObject.TabIndex = 2
        Me.FireNewObject.Text = "Add"
        '
        'FireDupObject
        '
        Me.FireDupObject.Location = New System.Drawing.Point(507, 184)
        Me.FireDupObject.Name = "FireDupObject"
        Me.FireDupObject.Size = New System.Drawing.Size(75, 23)
        Me.FireDupObject.TabIndex = 3
        Me.FireDupObject.Text = "Duplicate"
        '
        'FireData
        '
        Me.FireData.AllowDelete = True
        Me.FireData.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.FireData.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.FireData.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.FireData.AutoClipboard = True
        Me.FireData.ColumnInfo = resources.GetString("FireData.ColumnInfo")
        Me.FireData.ExtendLastCol = True
        Me.FireData.Location = New System.Drawing.Point(16, 536)
        Me.FireData.Name = "FireData"
        Me.FireData.Rows.Count = 101
        Me.FireData.Rows.DefaultSize = 17
        Me.FireData.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireData.Size = New System.Drawing.Size(945, 112)
        Me.FireData.TabIndex = 18
        '
        'FireObjectSummary
        '
        Me.FireObjectSummary.ColumnInfo = resources.GetString("FireObjectSummary.ColumnInfo")
        Me.FireObjectSummary.ExtendLastCol = True
        Me.FireObjectSummary.Location = New System.Drawing.Point(40, 16)
        Me.FireObjectSummary.Name = "FireObjectSummary"
        Me.FireObjectSummary.Rows.Count = 101
        Me.FireObjectSummary.Rows.DefaultSize = 17
        Me.FireObjectSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireObjectSummary.Size = New System.Drawing.Size(896, 144)
        Me.FireObjectSummary.TabIndex = 1
        '
        'FireName
        '
        Me.FireName.Location = New System.Drawing.Point(452, 224)
        Me.FireName.Name = "FireName"
        Me.FireName.Size = New System.Drawing.Size(208, 20)
        Me.FireName.TabIndex = 5
        Me.FireName.Text = "New Fire Object"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(332, 224)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(92, 13)
        Me.Label9.TabIndex = 139
        Me.Label9.Text = "Fire Object Name:"
        Me.Label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireRemoveObject
        '
        Me.FireRemoveObject.Location = New System.Drawing.Point(603, 184)
        Me.FireRemoveObject.Name = "FireRemoveObject"
        Me.FireRemoveObject.Size = New System.Drawing.Size(75, 23)
        Me.FireRemoveObject.TabIndex = 4
        Me.FireRemoveObject.Text = "Remove"
        '
        'FireObjectPlot
        '
        Me.FireObjectPlot.AutoScaleAutoGeneratedAxes = True
        Me.FireObjectPlot.AutoScaleTitle = False
        Me.FireObjectPlot.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.FireObjectPlot.DateTimeToolTip = False
        Me.FireObjectPlot.Legend = Nothing
        Me.FireObjectPlot.LegendZOrder = -1
        Me.FireObjectPlot.Location = New System.Drawing.Point(416, 264)
        Me.FireObjectPlot.Name = "FireObjectPlot"
        Me.FireObjectPlot.RightMenu = Nothing
        Me.FireObjectPlot.ShowCoordinates = True
        Me.FireObjectPlot.Size = New System.Drawing.Size(560, 248)
        Me.FireObjectPlot.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None
        Me.FireObjectPlot.TabIndex = 144
        Me.FireObjectPlot.TabStop = False
        Me.FireObjectPlot.Title = ""
        Me.FireObjectPlot.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.FireObjectPlot.XAxis1 = Nothing
        Me.FireObjectPlot.XAxis2 = Nothing
        Me.FireObjectPlot.YAxis1 = Nothing
        Me.FireObjectPlot.YAxis2 = Nothing
        '
        'FireObjectsOK
        '
        Me.FireObjectsOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.FireObjectsOK.Location = New System.Drawing.Point(395, 672)
        Me.FireObjectsOK.Name = "FireObjectsOK"
        Me.FireObjectsOK.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsOK.TabIndex = 19
        Me.FireObjectsOK.Text = "OK"
        '
        'FireObjectsCancel
        '
        Me.FireObjectsCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FireObjectsCancel.Location = New System.Drawing.Point(523, 672)
        Me.FireObjectsCancel.Name = "FireObjectsCancel"
        Me.FireObjectsCancel.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsCancel.TabIndex = 20
        Me.FireObjectsCancel.Text = "Cancel"
        '
        'FireNewt2
        '
        Me.FireNewt2.Location = New System.Drawing.Point(314, 184)
        Me.FireNewt2.Name = "FireNewt2"
        Me.FireNewt2.Size = New System.Drawing.Size(75, 23)
        Me.FireNewt2.TabIndex = 145
        Me.FireNewt2.Text = "Add t²"
        '
        'FireObjectsEdit
        '
        Me.C1SizerLight1.SetAutoResize(Me, True)
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(992, 710)
        Me.Controls.Add(Me.FireNewt2)
        Me.Controls.Add(Me.FireObjectsCancel)
        Me.Controls.Add(Me.FireObjectsOK)
        Me.Controls.Add(Me.FireObjectPlot)
        Me.Controls.Add(Me.FireRemoveObject)
        Me.Controls.Add(Me.FireName)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.FireObjectSummary)
        Me.Controls.Add(Me.FireData)
        Me.Controls.Add(Me.GroupFireObject)
        Me.Controls.Add(Me.FireNewObject)
        Me.Controls.Add(Me.FireDupObject)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MinimumSize = New System.Drawing.Size(800, 490)
        Me.Name = "FireObjectsEdit"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Fire Objects"
        Me.GroupFireObject.ResumeLayout(False)
        Me.GroupFireObject.PerformLayout()
        CType(Me.FireData, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.FireObjectSummary, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region
    Private Sub FireObjectsEdit_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        If myFireObjects.Count > 0 Then
            Dim FromFire As New Fire
            Dim i As Integer
            If TempFireObjects.Count > 0 Then TempFireObjects.Clear()
            For i = 0 To myFireObjects.Count - 1
                TempFireObjects.Add(New Fire)
                FireCopy(myFireObjects(i), TempFireObjects(TempFireObjects.Count - 1))
            Next
            InitThermalPropertyList(Me.FireMaterial)
            Me.FireData.Cols.Fixed = 0
            UpdateFireObjects(CurrentFireObject)
        End If
    End Sub
    Private Sub FireSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireObjectSummary.Click
        ' The currently selected Fire has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.FireObjectSummary.RowSel - 1
        If index >= 0 And index <= TempFireObjects.Count - 1 Then
            CurrentFireObject = index
            UpdateFireObjects(CurrentFireObject)
        End If
    End Sub
    Private Sub FireSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireObjectSummary.AfterSelChange
        ' The currently selected Fire has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.FireObjectSummary.RowSel - 1
        If index >= 0 And index <= TempFireObjects.Count - 1 Then
            CurrentFireObject = index
            UpdateFireObjects(CurrentFireObject)
        End If
    End Sub
    Private Sub UpdateFirePlot(ByVal index As Integer)
        Dim aFireObject As New Fire
        Dim aFireData(12, 0) As Single, numPoints As Integer
        Dim x() As Single, y() As Single, j As Integer, iSelectedColumn As Integer
        aFireObject = TempFireObjects(index)
        Me.FireObjectPlot.Clear()
        aFireObject.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        iSelectedColumn = Me.FireData.ColSel
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
    Private Sub UpdateFireObjects(ByVal index As Integer)
        Dim aFireObject As New Fire
        Dim aFireTimeSeries(12, 0) As Single, numPoints As Integer
        Dim i As Integer, j As Integer, ir As Integer, ic As Integer
        Dim PeakHRR As Single, PeakCO As Single, PeakC As Single, PeakHCN As Single, PeakHCl As Single

        aFireObject = TempFireObjects(index)
        Me.FireName.Text = aFireObject.Name
        Me.FireMaterial.Text = myThermalProperties.GetLongName(aFireObject.Material)
        Me.FireLength.Text = aFireObject.Length.ToString + myUnits.Convert(UnitsNum.Length).Units
        Me.FireWidth.Text = aFireObject.Width.ToString + myUnits.Convert(UnitsNum.Length).Units
        Me.FireThickness.Text = aFireObject.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
        Me.FireMolarMass.Text = aFireObject.MolarMass.ToString + myUnits.Convert(UnitsNum.Mass).Units + "/mol"
        Me.FireTotalMass.Text = aFireObject.TotalMass.ToString + myUnits.Convert(UnitsNum.Mass).Units
        Me.FireHoC.Text = aFireObject.HeatofCombustion.ToString + myUnits.Convert(UnitsNum.HoC).Units
        Me.FireHoG.Text = aFireObject.HeatofGasification.ToString + myUnits.Convert(UnitsNum.HoG).Units
        Me.FireVolitilizationTemp.Text = aFireObject.VolitilTemp.ToString + myUnits.Convert(UnitsNum.Temperature).Units
        Me.FireRadiativeFraction.Text = aFireObject.RadiativeFraction.ToString
        aFireObject.GetFireData(aFireTimeSeries, numPoints)
        ClearGrid(Me.FireData)
        Me.FireData(0, 0) = "Time" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireTime).Units.Substring(1) + ")"
        Me.FireData(0, 1) = "Mdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireMdot).Units.Substring(1) + ")"
        Me.FireData(0, 2) = "Qdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireQdot).Units.Substring(1) + ")"
        Me.FireData(0, 3) = "Height" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireHeight).Units.Substring(1) + ")"
        Me.FireData(0, 4) = "Area" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireArea).Units.Substring(1) + ")"
        Me.FireData.AutoSizeRow(0)

        If numPoints >= 0 Then
            For ir = 0 To numPoints
                For ic = 0 To 12
                    Me.FireData(ir + 1, ic) = aFireTimeSeries(ic, ir)
                Next
            Next
        End If

        For i = 1 To TempFireObjects.Count
            aFireObject = TempFireObjects(i - 1)
            Me.FireObjectSummary(i, 0) = i
            Me.FireObjectSummary(i, 1) = aFireObject.Name
            Me.FireObjectSummary(i, 2) = aFireObject.Length.ToString
            Me.FireObjectSummary(i, 3) = aFireObject.Width.ToString
            Me.FireObjectSummary(i, 4) = aFireObject.Thickness.ToString
            Me.FireObjectSummary(i, 10) = aFireObject.HeatofCombustion.ToString
            Me.FireObjectSummary(i, 11) = aFireObject.Material
            PeakHRR = 0.0
            PeakCO = 0.0
            PeakC = 0.0
            PeakHCN = 0.0
            PeakHCl = 0.0
            aFireObject.GetFireData(aFireTimeSeries, numPoints)
            For j = 0 To numPoints
                If aFireTimeSeries(Fire.FireHRR, j) > PeakHRR Then PeakHRR = aFireTimeSeries(Fire.FireHRR, j)
                If aFireTimeSeries(Fire.FireCO, j) > PeakCO Then PeakCO = aFireTimeSeries(Fire.FireCO, j)
                If aFireTimeSeries(Fire.FireSoot, j) > PeakC Then PeakC = aFireTimeSeries(Fire.FireSoot, j)
                If aFireTimeSeries(Fire.FireHCN, j) > PeakHCN Then PeakHCN = aFireTimeSeries(Fire.FireHCN, j)
                If aFireTimeSeries(Fire.FireHCl, j) > PeakHCl Then PeakHCN = aFireTimeSeries(Fire.FireHCl, j)
            Next
            Me.FireObjectSummary(i, 5) = PeakHRR.ToString
            Me.FireObjectSummary(i, 6) = PeakCO.ToString
            Me.FireObjectSummary(i, 7) = PeakC.ToString
            Me.FireObjectSummary(i, 8) = PeakHCN.ToString
            Me.FireObjectSummary(i, 9) = PeakHCl.ToString
        Next
        Me.FireObjectSummary.Select(index + 1, 0, index + 1, Me.FireObjectSummary.Cols.Count - 1, True)

        UpdateFirePlot(CurrentFireObject)
        If TempFireObjects.Changed Then
            Me.Text = "Fire Objects *"
        Else
            Me.Text = "Fire Objects"
        End If
    End Sub
    Private Sub ClearGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        Dim i As Integer
        ' Erase the contents of a grid, leaving only the header row
        For i = 2 To obj.Rows.Count
            obj.Clear(C1.Win.C1FlexGrid.ClearFlags.Content, i - 1, 0, i - 1, obj.Cols.Count - 1)
        Next
    End Sub
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
    Private Sub Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireMaterial.SelectedIndexChanged, FireLength.Leave, FireWidth.Leave, FireThickness.Leave, FireMolarMass.Leave, FireTotalMass.Leave, FireHoC.Leave, FireHoG.Leave, FireVolitilizationTemp.Leave, FireRadiativeFraction.Leave, FireName.Leave
        Dim aFireObject As New Fire
        Dim numPoints As Integer, ir As Integer
        aFireObject = TempFireObjects(CurrentFireObject)
        If sender Is Me.FireName Then aFireObject.Name = Me.FireName.Text
        If sender Is Me.FireMaterial Then aFireObject.Material = myThermalProperties.GetShortName(sender.text)
        If sender Is Me.FireLength Then aFireObject.Length = Val(Me.FireLength.Text)
        If sender Is Me.FireWidth Then aFireObject.Width = Val(Me.FireWidth.Text)
        If sender Is Me.FireThickness Then aFireObject.Thickness = Val(Me.FireThickness.Text)
        If sender Is Me.FireMolarMass Then aFireObject.MolarMass = Val(Me.FireMolarMass.Text)
        If sender Is Me.FireTotalMass Then aFireObject.TotalMass = Val(Me.FireTotalMass.Text)
        If sender Is Me.FireHoC Then
            If Val(Me.FireHoC.Text) <> aFireObject.HeatofCombustion Then
                aFireObject.HeatofCombustion = Val(Me.FireHoC.Text)
                numPoints = CountGridPoints(Me.FireData)
                For ir = 1 To numPoints
                    Me.FireData(ir, Fire.FireMdot) = Me.FireData(ir, Fire.FireHRR) / aFireObject.HeatofCombustion
                Next
                CopyFireData(aFireObject)
            End If
        End If
        If sender Is Me.FireHoG Then aFireObject.HeatofGasification = Val(Me.FireHoG.Text)
        If sender Is Me.FireVolitilizationTemp Then aFireObject.VolitilTemp = Val(Me.FireVolitilizationTemp.Text)
        If sender Is Me.FireRadiativeFraction Then aFireObject.RadiativeFraction = Val(Me.FireRadiativeFraction.Text)
        If CurrentFireObject >= 0 Then TempFireObjects(CurrentFireObject) = aFireObject
        UpdateFireObjects(CurrentFireObject)
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireData.BeforeRowColChange
        Dim numPoints As Integer
        numPoints = CountGridPoints(Me.FireData)
        ' Copy the values from the spreadsheet to the array for fire data, then put them in the FireObject data structure
        If CurrentFireObject >= 0 And CurrentFireObject < TempFireObjects.Count Then
            Dim aFireObject As New Fire
            aFireObject = TempFireObjects(CurrentFireObject)
            If Me.FireData.ColSel = Fire.FireHRR Then
                Me.FireData(Me.FireData.RowSel, Fire.FireMdot) = Me.FireData(Me.FireData.RowSel, Fire.FireHRR) / aFireObject.HeatofCombustion
            End If
            If Me.FireData.ColSel = Fire.FireMdot Then
                Me.FireData(Me.FireData.RowSel, Fire.FireHRR) = Me.FireData(Me.FireData.RowSel, Fire.FireMdot) * aFireObject.HeatofCombustion
            End If
            CopyFireData(aFireObject)
            TempFireObjects(CurrentFireObject) = aFireObject
            UpdateFireObjects(CurrentFireObject)
        End If
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireData.AfterRowColChange
        UpdateFirePlot(CurrentFireObject)
    End Sub
    Private Sub CopyFireData(ByVal aFireObject As Fire)
        Dim numPoints As Integer, ir As Integer, ic As Integer
        numPoints = CountGridPoints(Me.FireData)
        If numPoints > 0 Then
            Dim aFireTimeSeries(12, numPoints - 1) As Single
            For ir = 0 To numPoints - 1
                For ic = 0 To 12
                    aFireTimeSeries(ic, ir) = Val(Me.FireData(ir + 1, ic))
                Next
            Next
            aFireObject.SetFireData(aFireTimeSeries)
        End If
    End Sub
    Private Sub FireObject_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireNewObject.Click, FireNewt2.Click, FireDupObject.Click
        If sender Is FireNewt2 And TempFireObjects.Count + 1 <= Fire.MaximumFireObjects Then
            Dim t2FireDialog As New t2Fire
            Dim iReturn As Integer
            iReturn = t2FireDialog.ShowDialog(Me)
            If iReturn = Windows.Forms.DialogResult.OK Then
                TempFireObjects.Add(New Fire(t2FireDialog.GrowthTime, t2FireDialog.PeakHRR, t2FireDialog.SteadyTime, t2FireDialog.DecayTime))
                CurrentFireObject = TempFireObjects.Count - 1
            End If
        ElseIf sender Is FireNewObject And TempFireObjects.Count + 1 <= Fire.MaximumFireObjects Then
            TempFireObjects.Add(New Fire(Fire.TypeFireObject))
            CurrentFireObject = TempFireObjects.Count - 1
        ElseIf CurrentFireObject >= 0 And CurrentFireObject < TempFireObjects.Count And TempFireObjects.Count + 1 <= Fire.MaximumFireObjects Then
            TempFireObjects.Add(New Fire(Fire.TypeFireObject))
            TempFireObjects.Copy(CurrentFireObject, TempFireObjects.Count - 1)
            Dim fireComments As New Collection
            CurrentFireObject = TempFireObjects.Count - 1
            TempFireObjects(CurrentFireObject).Name = TempFireObjects(CurrentFireObject).Name + "(1)"
            fireFilesComments.Add(fireComments)
            TempFireObjects(CurrentFireObject).CommentsIndex = fireFilesComments.Count
        Else
            MessageBox.Show("A maximum of " + TempFireObjects.Maximum.ToString + " Fire objects are allowed. New object not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
        UpdateFireObjects(CurrentFireObject)
    End Sub
    Private Sub FireData_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FireData.KeyDown
        Dim aChar As String
        aChar = e.KeyCode.ToString
        If e.Control Then
            If aChar = "Delete" Then
                If FireData.Row > 0 And FireData.Col >= 0 Then
                    FireData(FireData.Row, FireData.Col) = " "
                End If
            End If
        End If
        If e.Alt Then
            If aChar = "Delete" Then
                If FireData.Row > 0 And FireData.Col >= 0 Then
                    If CurrentFireObject >= 0 And CurrentFireObject < TempFireObjects.Count Then
                        Dim numPoints As Integer, i As Integer, j As Integer
                        numPoints = CountGridPoints(Me.FireData)
                        If numPoints > FireData.Row Then
                            For i = FireData.Row To numPoints
                                For j = 0 To FireData.Cols.Count - 1
                                    FireData(i, j) = FireData(i + 1, j)
                                Next
                            Next
                        End If
                        For j = 0 To FireData.Cols.Count - 1
                            FireData(numPoints, j) = Nothing
                        Next
                        Dim aFireObject As New Fire
                        aFireObject = TempFireObjects(CurrentFireObject)
                        CopyFireData(aFireObject)
                        TempFireObjects(CurrentFireObject) = aFireObject
                        UpdateFireObjects(CurrentFireObject)
                    End If
                End If
            End If
            If aChar = "Insert" Then
                If FireData.Row > 0 Then
                    Dim numPoints As Integer, i As Integer, j As Integer
                    numPoints = Me.CountGridPoints(Me.FireData)
                    If numPoints > FireData.Row Then
                        For i = numPoints To FireData.Row Step -1
                            For j = 0 To FireData.Cols.Count - 1
                                FireData(i + 1, j) = FireData(i, j)
                            Next
                        Next
                        For j = 0 To FireData.Cols.Count - 1
                            FireData(FireData.Row, j) = 0
                        Next
                    End If

                End If
            End If
        End If
    End Sub
End Class
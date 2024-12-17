Public Class AllFireObjects
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
    Friend WithEvents FireHoC As System.Windows.Forms.TextBox
    Friend WithEvents Label56 As System.Windows.Forms.Label
    Friend WithEvents FireN As System.Windows.Forms.TextBox
    Friend WithEvents FireH As System.Windows.Forms.TextBox
    Friend WithEvents Label64 As System.Windows.Forms.Label
    Friend WithEvents FireC As System.Windows.Forms.TextBox
    Friend WithEvents FireO As System.Windows.Forms.TextBox
    Friend WithEvents Label65 As System.Windows.Forms.Label
    Friend WithEvents Label39 As System.Windows.Forms.Label
    Friend WithEvents Label38 As System.Windows.Forms.Label
    Friend WithEvents Label59 As System.Windows.Forms.Label
    Friend WithEvents FireRadiativeFraction As System.Windows.Forms.TextBox
    Friend WithEvents FireDataSS As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents FireRemoveObject As System.Windows.Forms.Button
    Friend WithEvents FireName As System.Windows.Forms.TextBox
    Friend WithEvents FireObjectSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents FireObjectPlot As NPlot.Windows.PlotSurface2D
    Friend WithEvents FireObjectsOK As System.Windows.Forms.Button
    Friend WithEvents FireObjectsCancel As System.Windows.Forms.Button
    Friend WithEvents FireNewt2 As System.Windows.Forms.Button
    Friend WithEvents FireCl As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents FireCOYield As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents FireSootYield As System.Windows.Forms.TextBox
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight
    Friend WithEvents Label2 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(AllFireObjects))
        Me.GroupFireObject = New System.Windows.Forms.GroupBox()
        Me.FireCOYield = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.FireSootYield = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.FireCl = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.FireMaterial = New System.Windows.Forms.ComboBox()
        Me.Label84 = New System.Windows.Forms.Label()
        Me.FireHoC = New System.Windows.Forms.TextBox()
        Me.Label56 = New System.Windows.Forms.Label()
        Me.FireN = New System.Windows.Forms.TextBox()
        Me.FireH = New System.Windows.Forms.TextBox()
        Me.Label64 = New System.Windows.Forms.Label()
        Me.FireC = New System.Windows.Forms.TextBox()
        Me.FireO = New System.Windows.Forms.TextBox()
        Me.Label65 = New System.Windows.Forms.Label()
        Me.Label39 = New System.Windows.Forms.Label()
        Me.Label38 = New System.Windows.Forms.Label()
        Me.Label59 = New System.Windows.Forms.Label()
        Me.FireRadiativeFraction = New System.Windows.Forms.TextBox()
        Me.FireNewObject = New System.Windows.Forms.Button()
        Me.FireDupObject = New System.Windows.Forms.Button()
        Me.FireDataSS = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.FireObjectSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.FireName = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.FireRemoveObject = New System.Windows.Forms.Button()
        Me.FireObjectPlot = New NPlot.Windows.PlotSurface2D()
        Me.FireObjectsOK = New System.Windows.Forms.Button()
        Me.FireObjectsCancel = New System.Windows.Forms.Button()
        Me.FireNewt2 = New System.Windows.Forms.Button()
        Me.C1SizerLight1 = New C1.Win.C1Sizer.C1SizerLight(Me.components)
        Me.GroupFireObject.SuspendLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FireObjectSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupFireObject
        '
        Me.GroupFireObject.Controls.Add(Me.FireCOYield)
        Me.GroupFireObject.Controls.Add(Me.Label3)
        Me.GroupFireObject.Controls.Add(Me.FireSootYield)
        Me.GroupFireObject.Controls.Add(Me.Label2)
        Me.GroupFireObject.Controls.Add(Me.FireCl)
        Me.GroupFireObject.Controls.Add(Me.Label1)
        Me.GroupFireObject.Controls.Add(Me.FireMaterial)
        Me.GroupFireObject.Controls.Add(Me.Label84)
        Me.GroupFireObject.Controls.Add(Me.FireHoC)
        Me.GroupFireObject.Controls.Add(Me.Label56)
        Me.GroupFireObject.Controls.Add(Me.FireN)
        Me.GroupFireObject.Controls.Add(Me.FireH)
        Me.GroupFireObject.Controls.Add(Me.Label64)
        Me.GroupFireObject.Controls.Add(Me.FireC)
        Me.GroupFireObject.Controls.Add(Me.FireO)
        Me.GroupFireObject.Controls.Add(Me.Label65)
        Me.GroupFireObject.Controls.Add(Me.Label39)
        Me.GroupFireObject.Controls.Add(Me.Label38)
        Me.GroupFireObject.Controls.Add(Me.Label59)
        Me.GroupFireObject.Controls.Add(Me.FireRadiativeFraction)
        Me.GroupFireObject.Location = New System.Drawing.Point(20, 256)
        Me.GroupFireObject.Name = "GroupFireObject"
        Me.GroupFireObject.Size = New System.Drawing.Size(376, 256)
        Me.GroupFireObject.TabIndex = 6
        Me.GroupFireObject.TabStop = False
        Me.GroupFireObject.Text = "Details"
        '
        'FireCOYield
        '
        Me.FireCOYield.Location = New System.Drawing.Point(279, 136)
        Me.FireCOYield.Name = "FireCOYield"
        Me.FireCOYield.Size = New System.Drawing.Size(80, 20)
        Me.FireCOYield.TabIndex = 139
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(220, 140)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(51, 13)
        Me.Label3.TabIndex = 140
        Me.Label3.Text = "CO Yield:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireSootYield
        '
        Me.FireSootYield.Location = New System.Drawing.Point(279, 105)
        Me.FireSootYield.Name = "FireSootYield"
        Me.FireSootYield.Size = New System.Drawing.Size(80, 20)
        Me.FireSootYield.TabIndex = 137
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(213, 109)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(58, 13)
        Me.Label2.TabIndex = 138
        Me.Label2.Text = "Soot Yield:"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireCl
        '
        Me.FireCl.Location = New System.Drawing.Point(47, 198)
        Me.FireCl.Name = "FireCl"
        Me.FireCl.Size = New System.Drawing.Size(80, 20)
        Me.FireCl.TabIndex = 135
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(17, 202)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(19, 13)
        Me.Label1.TabIndex = 136
        Me.Label1.Text = "Cl:"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireMaterial
        '
        Me.FireMaterial.ItemHeight = 13
        Me.FireMaterial.Location = New System.Drawing.Point(122, 38)
        Me.FireMaterial.Name = "FireMaterial"
        Me.FireMaterial.Size = New System.Drawing.Size(192, 21)
        Me.FireMaterial.TabIndex = 7
        Me.FireMaterial.Text = "GYPSUM"
        '
        'Label84
        '
        Me.Label84.AutoSize = True
        Me.Label84.Location = New System.Drawing.Point(66, 38)
        Me.Label84.Name = "Label84"
        Me.Label84.Size = New System.Drawing.Size(47, 13)
        Me.Label84.TabIndex = 134
        Me.Label84.Text = "Material:"
        Me.Label84.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireHoC
        '
        Me.FireHoC.Location = New System.Drawing.Point(279, 73)
        Me.FireHoC.Name = "FireHoC"
        Me.FireHoC.Size = New System.Drawing.Size(80, 20)
        Me.FireHoC.TabIndex = 13
        Me.FireHoC.Text = "50000000 J/kg"
        '
        'Label56
        '
        Me.Label56.Location = New System.Drawing.Point(199, 71)
        Me.Label56.Name = "Label56"
        Me.Label56.Size = New System.Drawing.Size(72, 24)
        Me.Label56.TabIndex = 116
        Me.Label56.Text = "Heat of Combustion:"
        Me.Label56.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireN
        '
        Me.FireN.Location = New System.Drawing.Point(49, 167)
        Me.FireN.Name = "FireN"
        Me.FireN.Size = New System.Drawing.Size(80, 20)
        Me.FireN.TabIndex = 12
        '
        'FireH
        '
        Me.FireH.Location = New System.Drawing.Point(47, 105)
        Me.FireH.Name = "FireH"
        Me.FireH.Size = New System.Drawing.Size(80, 20)
        Me.FireH.TabIndex = 9
        '
        'Label64
        '
        Me.Label64.AutoSize = True
        Me.Label64.Location = New System.Drawing.Point(19, 78)
        Me.Label64.Name = "Label64"
        Me.Label64.Size = New System.Drawing.Size(17, 13)
        Me.Label64.TabIndex = 118
        Me.Label64.Text = "C:"
        Me.Label64.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireC
        '
        Me.FireC.Location = New System.Drawing.Point(49, 74)
        Me.FireC.Name = "FireC"
        Me.FireC.Size = New System.Drawing.Size(80, 20)
        Me.FireC.TabIndex = 8
        '
        'FireO
        '
        Me.FireO.Location = New System.Drawing.Point(47, 136)
        Me.FireO.Name = "FireO"
        Me.FireO.Size = New System.Drawing.Size(80, 20)
        Me.FireO.TabIndex = 10
        '
        'Label65
        '
        Me.Label65.AutoSize = True
        Me.Label65.Location = New System.Drawing.Point(18, 140)
        Me.Label65.Name = "Label65"
        Me.Label65.Size = New System.Drawing.Size(18, 13)
        Me.Label65.TabIndex = 122
        Me.Label65.Text = "O:"
        Me.Label65.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label39
        '
        Me.Label39.AutoSize = True
        Me.Label39.Location = New System.Drawing.Point(18, 109)
        Me.Label39.Name = "Label39"
        Me.Label39.Size = New System.Drawing.Size(18, 13)
        Me.Label39.TabIndex = 119
        Me.Label39.Text = "H:"
        Me.Label39.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label38
        '
        Me.Label38.AutoSize = True
        Me.Label38.Location = New System.Drawing.Point(18, 171)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(18, 13)
        Me.Label38.TabIndex = 116
        Me.Label38.Text = "N:"
        Me.Label38.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label59
        '
        Me.Label59.Location = New System.Drawing.Point(215, 167)
        Me.Label59.Name = "Label59"
        Me.Label59.Size = New System.Drawing.Size(56, 24)
        Me.Label59.TabIndex = 124
        Me.Label59.Text = "Radiative Fraction:"
        Me.Label59.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireRadiativeFraction
        '
        Me.FireRadiativeFraction.Location = New System.Drawing.Point(279, 169)
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
        Me.FireNewObject.Text = "Create New"
        '
        'FireDupObject
        '
        Me.FireDupObject.Location = New System.Drawing.Point(507, 184)
        Me.FireDupObject.Name = "FireDupObject"
        Me.FireDupObject.Size = New System.Drawing.Size(75, 23)
        Me.FireDupObject.TabIndex = 3
        Me.FireDupObject.Text = "Duplicate"
        '
        'FireDataSS
        '
        Me.FireDataSS.AllowDelete = True
        Me.FireDataSS.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.FireDataSS.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.FireDataSS.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.FireDataSS.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom), System.Windows.Forms.AnchorStyles)
        Me.FireDataSS.AutoClipboard = True
        Me.FireDataSS.ColumnInfo = resources.GetString("FireDataSS.ColumnInfo")
        Me.FireDataSS.ExtendLastCol = True
        Me.FireDataSS.Location = New System.Drawing.Point(198, 536)
        Me.FireDataSS.Name = "FireDataSS"
        Me.FireDataSS.Rows.Count = 101
        Me.FireDataSS.Rows.DefaultSize = 17
        Me.FireDataSS.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireDataSS.Size = New System.Drawing.Size(597, 112)
        Me.FireDataSS.TabIndex = 18
        '
        'FireObjectSummary
        '
        Me.FireObjectSummary.ColumnInfo = resources.GetString("FireObjectSummary.ColumnInfo")
        Me.FireObjectSummary.ExtendLastCol = True
        Me.FireObjectSummary.Location = New System.Drawing.Point(36, 12)
        Me.FireObjectSummary.Name = "FireObjectSummary"
        Me.FireObjectSummary.Rows.Count = 101
        Me.FireObjectSummary.Rows.DefaultSize = 17
        Me.FireObjectSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireObjectSummary.Size = New System.Drawing.Size(921, 144)
        Me.FireObjectSummary.TabIndex = 1
        '
        'FireName
        '
        Me.FireName.Location = New System.Drawing.Point(424, 224)
        Me.FireName.Name = "FireName"
        Me.FireName.Size = New System.Drawing.Size(208, 20)
        Me.FireName.TabIndex = 5
        Me.FireName.Text = "New Fire Object"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(360, 227)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(58, 13)
        Me.Label9.TabIndex = 139
        Me.Label9.Text = "Fire Name:"
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
        Me.FireNewt2.Text = "Create t²"
        '
        'AllFireObjects
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
        Me.Controls.Add(Me.FireDataSS)
        Me.Controls.Add(Me.GroupFireObject)
        Me.Controls.Add(Me.FireNewObject)
        Me.Controls.Add(Me.FireDupObject)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MinimumSize = New System.Drawing.Size(800, 490)
        Me.Name = "AllFireObjects"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Fires"
        Me.GroupFireObject.ResumeLayout(False)
        Me.GroupFireObject.PerformLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).EndInit()
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
            Me.FireDataSS.Cols.Fixed = 0
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
    Private Sub UpdateFireObjects(ByVal index As Integer)
        Dim aFireObject As New Fire
        Dim aFireTimeSeries(12, 0) As Single, numPoints As Integer
        Dim i As Integer, ir As Integer, ic As Integer

        ' Update details for currently selected fire object
        aFireObject = TempFireObjects(index)
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

        ' Update Summary table of all fire objects
        For i = 1 To TempFireObjects.Count
            aFireObject = TempFireObjects(i - 1)
            Me.FireObjectSummary(i, 0) = i
            Me.FireObjectSummary(i, 1) = aFireObject.Name
            Me.FireObjectSummary(i, 2) = aFireObject.ChemicalFormula()
            Me.FireObjectSummary(i, 10) = aFireObject.HeatofCombustion.ToString
            Me.FireObjectSummary(i, 11) = aFireObject.Material
            Me.FireObjectSummary(i, 3) = aFireObject.Peak(Fire.FireHeight)
            Me.FireObjectSummary(i, 4) = aFireObject.Peak(Fire.FireArea)
            Me.FireObjectSummary(i, 5) = aFireObject.Peak(Fire.FireHRR)
            Me.FireObjectSummary(i, 6) = aFireObject.Peak(Fire.FireCO)
            Me.FireObjectSummary(i, 7) = aFireObject.Peak(Fire.FireSoot)
            Me.FireObjectSummary(i, 8) = aFireObject.Peak(Fire.FireHCN)
            Me.FireObjectSummary(i, 9) = aFireObject.Peak(Fire.FireHCl)
        Next
        Me.FireObjectSummary.Select(index + 1, 0, index + 1, Me.FireObjectSummary.Cols.Count - 1, True)

        UpdateFirePlot(CurrentFireObject)
        If TempFireObjects.Changed Then
            Me.Text = "Fires *"
        Else
            Me.Text = "Fires"
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
    Private Sub Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireMaterial.SelectedIndexChanged, FireC.Leave, FireH.Leave, FireO.Leave, FireN.Leave, FireCl.Leave, FireHoC.Leave, FireSootYield.Leave, FireCOYield.Leave, FireRadiativeFraction.Leave, FireName.Leave
        Dim aFireObject As New Fire
        Dim numPoints As Integer, ir As Integer
        aFireObject = TempFireObjects(CurrentFireObject)
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
        If CurrentFireObject >= 0 Then TempFireObjects(CurrentFireObject) = aFireObject
        UpdateFireObjects(CurrentFireObject)
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.BeforeRowColChange
        Dim numPoints As Integer
        numPoints = CountGridPoints(Me.FireDataSS)
        ' Copy the values from the spreadsheet to the array for fire data, then put them in the FireObject data structure
        If CurrentFireObject >= 0 And CurrentFireObject < TempFireObjects.Count Then
            Dim aFireObject As New Fire
            aFireObject = TempFireObjects(CurrentFireObject)
            If Me.FireDataSS.ColSel = Fire.FireHRR Then
                Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireMdot) = Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireHRR) / aFireObject.HeatofCombustion
            End If
            CopyFireData(aFireObject)
            TempFireObjects(CurrentFireObject) = aFireObject
            UpdateFireObjects(CurrentFireObject)
        End If
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.AfterRowColChange
        UpdateFirePlot(CurrentFireObject)
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
                        UpdateFireObjects(CurrentFireObject)
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
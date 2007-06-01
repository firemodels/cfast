Imports C1.Win.C1FlexGrid
Public Class Thermal_Properties_Edit
    Inherits System.Windows.Forms.Form
    Private CurrentThermalProperty As Integer
    Private Const OK As Integer = 1, Cancel As Integer = 2

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
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ThermalOK As System.Windows.Forms.Button
    Friend WithEvents ThermalCancel As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents ThermalSpecificHeat As System.Windows.Forms.TextBox
    Friend WithEvents ThermalLongName As System.Windows.Forms.TextBox
    Friend WithEvents ThermalDensity As System.Windows.Forms.TextBox
    Friend WithEvents ThermalThickness As System.Windows.Forms.TextBox
    Friend WithEvents ThermalEmissivity As System.Windows.Forms.TextBox
    Friend WithEvents ThermalShortName As System.Windows.Forms.TextBox
    Friend WithEvents ThermalHclb1 As System.Windows.Forms.TextBox
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb2 As System.Windows.Forms.TextBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb3 As System.Windows.Forms.TextBox
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb6 As System.Windows.Forms.TextBox
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb5 As System.Windows.Forms.TextBox
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb4 As System.Windows.Forms.TextBox
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents ThermalHclb7 As System.Windows.Forms.TextBox
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents ThermalSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents ThermalConductivity As System.Windows.Forms.TextBox
    Friend WithEvents GroupThermal As System.Windows.Forms.GroupBox
    Friend WithEvents ThermalRemove As System.Windows.Forms.Button
    Friend WithEvents ThermalAdd As System.Windows.Forms.Button
    Friend WithEvents ThermalDup As System.Windows.Forms.Button
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Thermal_Properties_Edit))
        Me.ThermalSummary = New C1.Win.C1FlexGrid.C1FlexGrid
        Me.Label1 = New System.Windows.Forms.Label
        Me.ThermalOK = New System.Windows.Forms.Button
        Me.ThermalCancel = New System.Windows.Forms.Button
        Me.ThermalConductivity = New System.Windows.Forms.TextBox
        Me.ThermalSpecificHeat = New System.Windows.Forms.TextBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.ThermalLongName = New System.Windows.Forms.TextBox
        Me.Label3 = New System.Windows.Forms.Label
        Me.ThermalDensity = New System.Windows.Forms.TextBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.ThermalThickness = New System.Windows.Forms.TextBox
        Me.Label5 = New System.Windows.Forms.Label
        Me.ThermalEmissivity = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.ThermalShortName = New System.Windows.Forms.TextBox
        Me.Label7 = New System.Windows.Forms.Label
        Me.ThermalHclb1 = New System.Windows.Forms.TextBox
        Me.Label8 = New System.Windows.Forms.Label
        Me.ThermalHclb2 = New System.Windows.Forms.TextBox
        Me.Label9 = New System.Windows.Forms.Label
        Me.ThermalHclb3 = New System.Windows.Forms.TextBox
        Me.Label10 = New System.Windows.Forms.Label
        Me.ThermalHclb6 = New System.Windows.Forms.TextBox
        Me.Label11 = New System.Windows.Forms.Label
        Me.ThermalHclb5 = New System.Windows.Forms.TextBox
        Me.Label12 = New System.Windows.Forms.Label
        Me.ThermalHclb4 = New System.Windows.Forms.TextBox
        Me.Label13 = New System.Windows.Forms.Label
        Me.ThermalHclb7 = New System.Windows.Forms.TextBox
        Me.Label14 = New System.Windows.Forms.Label
        Me.C1SizerLight1 = New C1.Win.C1Sizer.C1SizerLight(Me.components)
        Me.GroupThermal = New System.Windows.Forms.GroupBox
        Me.ThermalRemove = New System.Windows.Forms.Button
        Me.ThermalAdd = New System.Windows.Forms.Button
        Me.ThermalDup = New System.Windows.Forms.Button
        CType(Me.ThermalSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupThermal.SuspendLayout()
        Me.SuspendLayout()
        '
        'ThermalSummary
        '
        Me.ThermalSummary.ColumnInfo = resources.GetString("ThermalSummary.ColumnInfo")
        Me.ThermalSummary.ExtendLastCol = True
        Me.ThermalSummary.Location = New System.Drawing.Point(20, 16)
        Me.ThermalSummary.Name = "ThermalSummary"
        Me.ThermalSummary.Rows.Count = 126
        Me.ThermalSummary.Rows.DefaultSize = 17
        Me.ThermalSummary.Size = New System.Drawing.Size(752, 176)
        Me.ThermalSummary.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(272, 47)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(109, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Thermal Conductivity:"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalOK
        '
        Me.ThermalOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.ThermalOK.Location = New System.Drawing.Point(279, 465)
        Me.ThermalOK.Name = "ThermalOK"
        Me.ThermalOK.Size = New System.Drawing.Size(75, 23)
        Me.ThermalOK.TabIndex = 16
        Me.ThermalOK.Text = "OK"
        '
        'ThermalCancel
        '
        Me.ThermalCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.ThermalCancel.Location = New System.Drawing.Point(439, 465)
        Me.ThermalCancel.Name = "ThermalCancel"
        Me.ThermalCancel.Size = New System.Drawing.Size(75, 23)
        Me.ThermalCancel.TabIndex = 17
        Me.ThermalCancel.Text = "Cancel"
        '
        'ThermalConductivity
        '
        Me.ThermalConductivity.Location = New System.Drawing.Point(392, 47)
        Me.ThermalConductivity.Name = "ThermalConductivity"
        Me.ThermalConductivity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalConductivity.TabIndex = 4
        '
        'ThermalSpecificHeat
        '
        Me.ThermalSpecificHeat.Location = New System.Drawing.Point(616, 47)
        Me.ThermalSpecificHeat.Name = "ThermalSpecificHeat"
        Me.ThermalSpecificHeat.Size = New System.Drawing.Size(80, 20)
        Me.ThermalSpecificHeat.TabIndex = 5
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(536, 47)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(74, 13)
        Me.Label2.TabIndex = 5
        Me.Label2.Text = "Specific Heat:"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalLongName
        '
        Me.ThermalLongName.Location = New System.Drawing.Point(300, 15)
        Me.ThermalLongName.Name = "ThermalLongName"
        Me.ThermalLongName.Size = New System.Drawing.Size(208, 20)
        Me.ThermalLongName.TabIndex = 2
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(244, 15)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(47, 13)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Material:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalDensity
        '
        Me.ThermalDensity.Location = New System.Drawing.Point(128, 79)
        Me.ThermalDensity.Name = "ThermalDensity"
        Me.ThermalDensity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalDensity.TabIndex = 6
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(79, 79)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(45, 13)
        Me.Label4.TabIndex = 9
        Me.Label4.Text = "Density:"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalThickness
        '
        Me.ThermalThickness.Location = New System.Drawing.Point(392, 79)
        Me.ThermalThickness.Name = "ThermalThickness"
        Me.ThermalThickness.Size = New System.Drawing.Size(80, 20)
        Me.ThermalThickness.TabIndex = 7
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(320, 79)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(59, 13)
        Me.Label5.TabIndex = 11
        Me.Label5.Text = "Thickness:"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalEmissivity
        '
        Me.ThermalEmissivity.Location = New System.Drawing.Point(616, 79)
        Me.ThermalEmissivity.Name = "ThermalEmissivity"
        Me.ThermalEmissivity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalEmissivity.TabIndex = 8
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(552, 79)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(55, 13)
        Me.Label6.TabIndex = 13
        Me.Label6.Text = "Emissivity:"
        Me.Label6.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalShortName
        '
        Me.ThermalShortName.Location = New System.Drawing.Point(128, 47)
        Me.ThermalShortName.Name = "ThermalShortName"
        Me.ThermalShortName.Size = New System.Drawing.Size(80, 20)
        Me.ThermalShortName.TabIndex = 3
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(56, 47)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(66, 13)
        Me.Label7.TabIndex = 15
        Me.Label7.Text = "Short Name:"
        Me.Label7.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb1
        '
        Me.ThermalHclb1.Location = New System.Drawing.Point(128, 111)
        Me.ThermalHclb1.Name = "ThermalHclb1"
        Me.ThermalHclb1.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb1.TabIndex = 9
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(80, 111)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(48, 13)
        Me.Label8.TabIndex = 17
        Me.Label8.Text = "HCl (b1):"
        Me.Label8.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb2
        '
        Me.ThermalHclb2.Location = New System.Drawing.Point(392, 111)
        Me.ThermalHclb2.Name = "ThermalHclb2"
        Me.ThermalHclb2.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb2.TabIndex = 10
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(344, 111)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(48, 13)
        Me.Label9.TabIndex = 19
        Me.Label9.Text = "HCl (b2):"
        Me.Label9.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb3
        '
        Me.ThermalHclb3.Location = New System.Drawing.Point(616, 111)
        Me.ThermalHclb3.Name = "ThermalHclb3"
        Me.ThermalHclb3.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb3.TabIndex = 11
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(568, 111)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(48, 13)
        Me.Label10.TabIndex = 21
        Me.Label10.Text = "HCl (b3):"
        Me.Label10.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb6
        '
        Me.ThermalHclb6.Location = New System.Drawing.Point(616, 143)
        Me.ThermalHclb6.Name = "ThermalHclb6"
        Me.ThermalHclb6.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb6.TabIndex = 14
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(568, 143)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(48, 13)
        Me.Label11.TabIndex = 27
        Me.Label11.Text = "HCl (b6):"
        Me.Label11.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb5
        '
        Me.ThermalHclb5.Location = New System.Drawing.Point(392, 143)
        Me.ThermalHclb5.Name = "ThermalHclb5"
        Me.ThermalHclb5.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb5.TabIndex = 13
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(344, 143)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(48, 13)
        Me.Label12.TabIndex = 25
        Me.Label12.Text = "HCl (b5):"
        Me.Label12.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb4
        '
        Me.ThermalHclb4.Location = New System.Drawing.Point(128, 143)
        Me.ThermalHclb4.Name = "ThermalHclb4"
        Me.ThermalHclb4.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb4.TabIndex = 12
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(80, 143)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(48, 13)
        Me.Label13.TabIndex = 23
        Me.Label13.Text = "HCl (b4):"
        Me.Label13.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalHclb7
        '
        Me.ThermalHclb7.Location = New System.Drawing.Point(392, 175)
        Me.ThermalHclb7.Name = "ThermalHclb7"
        Me.ThermalHclb7.Size = New System.Drawing.Size(80, 20)
        Me.ThermalHclb7.TabIndex = 15
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(344, 175)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(48, 13)
        Me.Label14.TabIndex = 29
        Me.Label14.Text = "HCl (b7):"
        Me.Label14.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'GroupThermal
        '
        Me.GroupThermal.Controls.Add(Me.ThermalHclb7)
        Me.GroupThermal.Controls.Add(Me.ThermalThickness)
        Me.GroupThermal.Controls.Add(Me.Label14)
        Me.GroupThermal.Controls.Add(Me.Label1)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb6)
        Me.GroupThermal.Controls.Add(Me.ThermalConductivity)
        Me.GroupThermal.Controls.Add(Me.Label11)
        Me.GroupThermal.Controls.Add(Me.Label2)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb5)
        Me.GroupThermal.Controls.Add(Me.ThermalSpecificHeat)
        Me.GroupThermal.Controls.Add(Me.Label12)
        Me.GroupThermal.Controls.Add(Me.Label3)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb4)
        Me.GroupThermal.Controls.Add(Me.ThermalLongName)
        Me.GroupThermal.Controls.Add(Me.Label13)
        Me.GroupThermal.Controls.Add(Me.Label4)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb3)
        Me.GroupThermal.Controls.Add(Me.ThermalDensity)
        Me.GroupThermal.Controls.Add(Me.Label10)
        Me.GroupThermal.Controls.Add(Me.Label5)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb2)
        Me.GroupThermal.Controls.Add(Me.Label6)
        Me.GroupThermal.Controls.Add(Me.Label9)
        Me.GroupThermal.Controls.Add(Me.ThermalEmissivity)
        Me.GroupThermal.Controls.Add(Me.ThermalHclb1)
        Me.GroupThermal.Controls.Add(Me.Label7)
        Me.GroupThermal.Controls.Add(Me.Label8)
        Me.GroupThermal.Controls.Add(Me.ThermalShortName)
        Me.GroupThermal.Location = New System.Drawing.Point(20, 249)
        Me.GroupThermal.Name = "GroupThermal"
        Me.GroupThermal.Size = New System.Drawing.Size(752, 210)
        Me.GroupThermal.TabIndex = 30
        Me.GroupThermal.TabStop = False
        Me.GroupThermal.Text = "Property 1 (of 1)"
        '
        'ThermalRemove
        '
        Me.ThermalRemove.Location = New System.Drawing.Point(483, 207)
        Me.ThermalRemove.Name = "ThermalRemove"
        Me.ThermalRemove.Size = New System.Drawing.Size(75, 23)
        Me.ThermalRemove.TabIndex = 33
        Me.ThermalRemove.Text = "Remove"
        '
        'ThermalAdd
        '
        Me.ThermalAdd.Location = New System.Drawing.Point(235, 207)
        Me.ThermalAdd.Name = "ThermalAdd"
        Me.ThermalAdd.Size = New System.Drawing.Size(75, 23)
        Me.ThermalAdd.TabIndex = 31
        Me.ThermalAdd.Text = "Add"
        '
        'ThermalDup
        '
        Me.ThermalDup.Location = New System.Drawing.Point(331, 207)
        Me.ThermalDup.Name = "ThermalDup"
        Me.ThermalDup.Size = New System.Drawing.Size(75, 23)
        Me.ThermalDup.TabIndex = 32
        Me.ThermalDup.Text = "Duplicate"
        '
        'Thermal_Properties_Edit
        '
        Me.AcceptButton = Me.ThermalOK
        Me.C1SizerLight1.SetAutoResize(Me, True)
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.ThermalCancel
        Me.ClientSize = New System.Drawing.Size(859, 502)
        Me.Controls.Add(Me.ThermalRemove)
        Me.Controls.Add(Me.ThermalAdd)
        Me.Controls.Add(Me.ThermalDup)
        Me.Controls.Add(Me.ThermalSummary)
        Me.Controls.Add(Me.ThermalCancel)
        Me.Controls.Add(Me.ThermalOK)
        Me.Controls.Add(Me.GroupThermal)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MinimumSize = New System.Drawing.Size(800, 490)
        Me.Name = "Thermal_Properties_Edit"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Thermal Properties"
        CType(Me.ThermalSummary, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupThermal.ResumeLayout(False)
        Me.GroupThermal.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub Thermal_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalLongName.Leave, ThermalShortName.Leave, _
    ThermalConductivity.Leave, ThermalSpecificHeat.Leave, ThermalDensity.Leave, ThermalThickness.Leave, ThermalEmissivity.Leave, ThermalHclb1.Leave, _
    ThermalHclb2.Leave, ThermalHclb3.Leave, ThermalHclb4.Leave, ThermalHclb5.Leave, ThermalHclb6.Leave, ThermalHclb7.Leave
        If CurrentThermalProperty >= 0 And CurrentThermalProperty < TempThermalProperties.Count Then
            Dim aProperty As New ThermalProperty
            aProperty = TempThermalProperties(CurrentThermalProperty)
            If sender Is Me.ThermalLongName Then aProperty.Name = Me.ThermalLongName.Text
            If sender Is Me.ThermalShortName Then aProperty.ShortName = Me.ThermalShortName.Text
            If sender Is Me.ThermalConductivity Then aProperty.Conductivity = Val(Me.ThermalConductivity.Text)
            If sender Is Me.ThermalSpecificHeat Then aProperty.SpecificHeat = Val(Me.ThermalSpecificHeat.Text)
            If sender Is Me.ThermalDensity Then aProperty.Density = Val(Me.ThermalDensity.Text)
            If sender Is Me.ThermalThickness Then aProperty.Thickness = Val(Me.ThermalThickness.Text)
            If sender Is Me.ThermalEmissivity Then aProperty.Emissivity = Val(Me.ThermalEmissivity.Text)
            If sender Is Me.ThermalHclb1 Then aProperty.HClDeposition(0) = Val(Me.ThermalHclb1.Text)
            If sender Is Me.ThermalHclb2 Then aProperty.HClDeposition(1) = Val(Me.ThermalHclb2.Text)
            If sender Is Me.ThermalHclb3 Then aProperty.HClDeposition(2) = Val(Me.ThermalHclb3.Text)
            If sender Is Me.ThermalHclb4 Then aProperty.HClDeposition(3) = Val(Me.ThermalHclb4.Text)
            If sender Is Me.ThermalHclb5 Then aProperty.HClDeposition(4) = Val(Me.ThermalHclb5.Text)
            If sender Is Me.ThermalHclb6 Then aProperty.HClDeposition(5) = Val(Me.ThermalHclb6.Text)
            If sender Is Me.ThermalHclb7 Then aProperty.HClDeposition(6) = Val(Me.ThermalHclb7.Text)
            UpdateThermalProperties(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalSummary.Click
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.ThermalSummary.RowSel - 1
        If index >= 0 And index <= TempThermalProperties.Count - 1 Then
            CurrentThermalProperty = index
            UpdateThermalProperties(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles ThermalSummary.AfterSelChange
        ' The currently selected ThermalProperties has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.ThermalSummary.RowSel - 1
        If index >= 0 And index <= TempThermalProperties.Count - 1 Then
            CurrentThermalProperty = index
            UpdateThermalProperties(CurrentThermalProperty)
        End If
    End Sub

    Private Sub Thermal_Properties_Edit_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim FromMaterial As New ThermalProperty
        Dim i As Integer
        InitThermalGrid(Me.ThermalSummary)
        If TempThermalProperties.Count > 0 Then TempThermalProperties.Clear()
        For i = 0 To myThermalProperties.Count - 1
            TempThermalProperties.Add(New ThermalProperty)
            PropertyCopy(myThermalProperties(i), TempThermalProperties(TempThermalProperties.Count - 1))
        Next
        CurrentThermalProperty = 0
        UpdateThermalProperties(CurrentThermalProperty)
    End Sub
    Private Sub UpdateThermalProperties(ByVal index As Integer)
        If CurrentThermalProperty >= 0 And CurrentThermalProperty < TempThermalProperties.Count Then
            Me.GroupThermal.Text = "Thermal Property " + CurrentThermalProperty.ToString + " of(" + TempThermalProperties.Count.ToString + ")"
            Dim aThermalProperty As New ThermalProperty
            Dim i As Integer
            aThermalProperty = TempThermalProperties(CurrentThermalProperty)
            Me.ThermalShortName.Text = aThermalProperty.ShortName
            Me.ThermalLongName.Text = aThermalProperty.Name
            Me.ThermalConductivity.Text = aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
            Me.ThermalSpecificHeat.Text = aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
            Me.ThermalDensity.Text = aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
            Me.ThermalThickness.Text = aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
            Me.ThermalEmissivity.Text = aThermalProperty.Emissivity.ToString
            Dim b(6) As Single
            aThermalProperty.GetHCl(b)
            Me.ThermalHclb1.Text = b(0).ToString
            Me.ThermalHclb2.Text = b(1).ToString
            Me.ThermalHclb3.Text = b(2).ToString
            Me.ThermalHclb4.Text = b(3).ToString
            Me.ThermalHclb5.Text = b(4).ToString
            Me.ThermalHclb6.Text = b(5).ToString
            Me.ThermalHclb7.Text = b(6).ToString
            ClearGrid(Me.ThermalSummary)
            For i = 1 To TempThermalProperties.Count
                aThermalProperty = TempThermalProperties(i - 1)
                Me.ThermalSummary(i, 0) = aThermalProperty.Name
                Me.ThermalSummary(i, 1) = aThermalProperty.ShortName
                Me.ThermalSummary(i, 2) = aThermalProperty.Conductivity.ToString
                Me.ThermalSummary(i, 3) = aThermalProperty.SpecificHeat.ToString
                Me.ThermalSummary(i, 4) = aThermalProperty.Density.ToString
                Me.ThermalSummary(i, 5) = aThermalProperty.Thickness.ToString
                Me.ThermalSummary(i, 6) = aThermalProperty.Emissivity.ToString
            Next
            Me.ThermalSummary.Select(index + 1, 0, index + 1, Me.ThermalSummary.Cols.Count - 1, True)
        End If
    End Sub
    Public Sub InitThermalGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        obj.Cols.Fixed = 0
        obj.Rows.Fixed = 1
        obj.AllowEditing = False
        obj.SelectionMode = SelectionModeEnum.Row
        obj.FocusRect = FocusRectEnum.None
    End Sub
    Private Sub ClearGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        Dim i As Integer
        ' Erase the contents of a grid, leaving only the header row
        For i = 2 To obj.Rows.Count
            obj.Clear(C1.Win.C1FlexGrid.ClearFlags.Content, i - 1, 0, i - 1, obj.Cols.Count - 1)
        Next
    End Sub
    Private Sub ThermalAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalAdd.Click
        ' Add a blank material to the end of the thermal property list
        If TempThermalProperties.Count + 1 < TempThermalProperties.Maximum Then
            Dim aProperty As New ThermalProperty
            aProperty.Name = "New Material " + (TempThermalProperties.Count + 1).ToString
            aProperty.ShortName = "NM " + (TempThermalProperties.Count + 1).ToString
            TempThermalProperties.Add(aProperty)
            CurrentThermalProperty = TempThermalProperties.Count - 1
            UpdateThermalProperties(CurrentThermalProperty)
        Else
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub ThermalDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalDup.Click
        ' Copy the current thermal property, adding it to the end of the property list
        If CurrentThermalProperty >= 0 And TempThermalProperties.Count > 0 And CurrentThermalProperty + 1 < TempThermalProperties.Maximum Then
            TempThermalProperties.Add(New ThermalProperty)
            TempThermalProperties.Copy(CurrentThermalProperty, TempThermalProperties.Count - 1)
            CurrentThermalProperty = TempThermalProperties.Count - 1
            TempThermalProperties(TempThermalProperties.Count - 1).Name = "New Material " + TempThermalProperties.Count.ToString
            TempThermalProperties(TempThermalProperties.Count - 1).ShortName = "NM " + (TempThermalProperties.Count).ToString
            UpdateThermalProperties(CurrentThermalProperty)
        ElseIf CurrentThermalProperty + 1 >= TempThermalProperties.Maximum Then
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub ThermalRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalRemove.Click
        Dim ReturnedButton As Integer, TotalConnections As Integer
        If CurrentThermalProperty > 0 And TempThermalProperties.Count > 0 Then
            Dim aProperty As New ThermalProperty
            aProperty = TempThermalProperties.Item(CurrentThermalProperty)
            TotalConnections = TempThermalProperties.NumberofConnections(aProperty.ShortName)
            If TotalConnections > 0 Then
                ReturnedButton = MessageBox.Show(aProperty.Name + " is used " + TotalConnections.ToString + _
                " times to define materials in the current simulation. These will be changed to Off.", Me.Text, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button2, MessageBoxOptions.DefaultDesktopOnly)
                If ReturnedButton = OK Then
                    TempThermalProperties.Remove(CurrentThermalProperty)
                    If CurrentThermalProperty > 0 Then CurrentThermalProperty -= 1
                    myEnvironment.Changed = True
                End If
            Else
                TempThermalProperties.Remove(CurrentThermalProperty)
                If CurrentThermalProperty > 0 Then CurrentThermalProperty -= 1
                myEnvironment.Changed = True
            End If
            UpdateThermalProperties(CurrentThermalProperty)
        End If
    End Sub
End Class
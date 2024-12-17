Imports C1.Win.C1FlexGrid
Public Class Thermal_Properties_Edit
    Inherits System.Windows.Forms.Form
    Private CurrentThermalProperty As Integer
    Private Const OK As Integer = 1, Cancel As Integer = 2
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight

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
    Friend WithEvents ThermalOK As System.Windows.Forms.Button
    Friend WithEvents ThermalCancel As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Thermal_Properties_Edit))
        Me.ThermalOK = New System.Windows.Forms.Button()
        Me.ThermalCancel = New System.Windows.Forms.Button()
        Me.C1SizerLight1 = New C1.Win.C1Sizer.C1SizerLight(Me.components)
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'ThermalOK
        '
        Me.ThermalOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.ThermalOK.Location = New System.Drawing.Point(279, 385)
        Me.ThermalOK.Name = "ThermalOK"
        Me.ThermalOK.Size = New System.Drawing.Size(75, 23)
        Me.ThermalOK.TabIndex = 16
        Me.ThermalOK.Text = "OK"
        '
        'ThermalCancel
        '
        Me.ThermalCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.ThermalCancel.Location = New System.Drawing.Point(439, 385)
        Me.ThermalCancel.Name = "ThermalCancel"
        Me.ThermalCancel.Size = New System.Drawing.Size(75, 23)
        Me.ThermalCancel.TabIndex = 17
        Me.ThermalCancel.Text = "Cancel"
        '
        'Thermal_Properties_Edit
        '
        Me.AcceptButton = Me.ThermalOK
        Me.C1SizerLight1.SetAutoResize(Me, True)
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.ThermalCancel
        Me.ClientSize = New System.Drawing.Size(793, 423)
        Me.Controls.Add(Me.ThermalCancel)
        Me.Controls.Add(Me.ThermalOK)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "Thermal_Properties_Edit"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Thermal Properties"
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

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

    Private Sub Thermal_TextChanged(sender As Object, e As EventArgs)

    End Sub
End Class
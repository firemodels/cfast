Public Class TimeStep
    Inherits System.Windows.Forms.Form

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
    Friend WithEvents Label64 As System.Windows.Forms.Label
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents TimeStepCancel As System.Windows.Forms.Button
    Friend WithEvents TimeStepOK As System.Windows.Forms.Button
    Friend WithEvents TimeStepValue As System.Windows.Forms.TextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(TimeStep))
        Me.TimeStepCancel = New System.Windows.Forms.Button
        Me.TimeStepOK = New System.Windows.Forms.Button
        Me.Label64 = New System.Windows.Forms.Label
        Me.TimeStepValue = New System.Windows.Forms.TextBox
        Me.TextBox1 = New System.Windows.Forms.TextBox
        Me.SuspendLayout()
        '
        'TimeStepCancel
        '
        Me.TimeStepCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.TimeStepCancel.Location = New System.Drawing.Point(171, 148)
        Me.TimeStepCancel.Name = "TimeStepCancel"
        Me.TimeStepCancel.TabIndex = 3
        Me.TimeStepCancel.Text = "Cancel"
        '
        'TimeStepOK
        '
        Me.TimeStepOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.TimeStepOK.Location = New System.Drawing.Point(43, 148)
        Me.TimeStepOK.Name = "TimeStepOK"
        Me.TimeStepOK.TabIndex = 2
        Me.TimeStepOK.Text = "OK"
        '
        'Label64
        '
        Me.Label64.AutoSize = True
        Me.Label64.Location = New System.Drawing.Point(20, 108)
        Me.Label64.Name = "Label64"
        Me.Label64.Size = New System.Drawing.Size(165, 16)
        Me.Label64.TabIndex = 120
        Me.Label64.Text = "Maximum Simulation Timestep::"
        Me.Label64.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TimeStepValue
        '
        Me.TimeStepValue.Location = New System.Drawing.Point(188, 108)
        Me.TimeStepValue.Name = "TimeStepValue"
        Me.TimeStepValue.Size = New System.Drawing.Size(80, 20)
        Me.TimeStepValue.TabIndex = 1
        Me.TimeStepValue.Text = ""
        '
        'TextBox1
        '
        Me.TextBox1.BackColor = System.Drawing.SystemColors.Control
        Me.TextBox1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TextBox1.Location = New System.Drawing.Point(8, 12)
        Me.TextBox1.Multiline = True
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ReadOnly = True
        Me.TextBox1.Size = New System.Drawing.Size(272, 80)
        Me.TextBox1.TabIndex = 121
        Me.TextBox1.TabStop = False
        Me.TextBox1.Text = "The timestep should be changed with care. Usually the default value is appropriat" & _
        "e. Values smaller than 0.2 s will increase simulation time, sometimes dramatical" & _
        "ly.  Values larger than 2 s will reduce the accuracy of the calculation, sometim" & _
        "es dramatically."
        '
        'TimeStep
        '
        Me.AcceptButton = Me.TimeStepOK
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.TimeStepCancel
        Me.ClientSize = New System.Drawing.Size(288, 182)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Label64)
        Me.Controls.Add(Me.TimeStepValue)
        Me.Controls.Add(Me.TimeStepCancel)
        Me.Controls.Add(Me.TimeStepOK)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "TimeStep"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "TimeStep"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub TimeStepValue_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TimeStepValue.Leave
        Dim value As Single
        value = Val(TimeStepValue.Text)
        If value > 0 Then
            TimeStepValue.Text = value.ToString + myUnits.Convert(UnitsNum.Time).Units
        Else
            TimeStepValue.Text = "Default"
        End If
    End Sub
End Class
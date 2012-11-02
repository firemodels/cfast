<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class t2Fire
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(t2Fire))
        Me.t2OK = New System.Windows.Forms.Button()
        Me.t2Cancel = New System.Windows.Forms.Button()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.t2GrowthType = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.t2GrowthTime = New System.Windows.Forms.TextBox()
        Me.t2DecayTime = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.t2SteadyTime = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.t2PeakHRR = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.FireObjectPlot = New NPlot.Windows.PlotSurface2D()
        Me.t2StatusStrip = New System.Windows.Forms.StatusStrip()
        Me.t2StripStatusLabel = New System.Windows.Forms.ToolStripStatusLabel()
        Me.t2StatusStrip.SuspendLayout()
        Me.SuspendLayout()
        '
        't2OK
        '
        Me.t2OK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.t2OK.Location = New System.Drawing.Point(200, 437)
        Me.t2OK.Name = "t2OK"
        Me.t2OK.Size = New System.Drawing.Size(75, 23)
        Me.t2OK.TabIndex = 0
        Me.t2OK.Text = "OK"
        Me.t2OK.UseVisualStyleBackColor = True
        '
        't2Cancel
        '
        Me.t2Cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.t2Cancel.Location = New System.Drawing.Point(303, 437)
        Me.t2Cancel.Name = "t2Cancel"
        Me.t2Cancel.Size = New System.Drawing.Size(75, 23)
        Me.t2Cancel.TabIndex = 1
        Me.t2Cancel.Text = "Cancel"
        Me.t2Cancel.UseVisualStyleBackColor = True
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(173, 28)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(90, 13)
        Me.Label4.TabIndex = 6
        Me.Label4.Text = "Fire Growth Rate:"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        't2GrowthType
        '
        Me.t2GrowthType.Items.AddRange(New Object() {"Custom", "Slow", "Medium", "Fast", "Ultra fast"})
        Me.t2GrowthType.Location = New System.Drawing.Point(269, 25)
        Me.t2GrowthType.Name = "t2GrowthType"
        Me.t2GrowthType.Size = New System.Drawing.Size(161, 21)
        Me.t2GrowthType.TabIndex = 5
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(190, 55)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(73, 13)
        Me.Label1.TabIndex = 8
        Me.Label1.Text = "Time to Peak:"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        't2GrowthTime
        '
        Me.t2GrowthTime.Location = New System.Drawing.Point(269, 52)
        Me.t2GrowthTime.Name = "t2GrowthTime"
        Me.t2GrowthTime.Size = New System.Drawing.Size(100, 20)
        Me.t2GrowthTime.TabIndex = 9
        '
        't2DecayTime
        '
        Me.t2DecayTime.Location = New System.Drawing.Point(269, 130)
        Me.t2DecayTime.Name = "t2DecayTime"
        Me.t2DecayTime.Size = New System.Drawing.Size(100, 20)
        Me.t2DecayTime.TabIndex = 11
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(196, 133)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(67, 13)
        Me.Label2.TabIndex = 10
        Me.Label2.Text = "Decay Time:"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        't2SteadyTime
        '
        Me.t2SteadyTime.Location = New System.Drawing.Point(269, 104)
        Me.t2SteadyTime.Name = "t2SteadyTime"
        Me.t2SteadyTime.Size = New System.Drawing.Size(100, 20)
        Me.t2SteadyTime.TabIndex = 13
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(148, 107)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(115, 13)
        Me.Label3.TabIndex = 12
        Me.Label3.Text = "Steady Burning Period:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        't2PeakHRR
        '
        Me.t2PeakHRR.Location = New System.Drawing.Point(269, 78)
        Me.t2PeakHRR.Name = "t2PeakHRR"
        Me.t2PeakHRR.Size = New System.Drawing.Size(100, 20)
        Me.t2PeakHRR.TabIndex = 15
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(201, 81)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(62, 13)
        Me.Label5.TabIndex = 14
        Me.Label5.Text = "Peak HRR:"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireObjectPlot
        '
        Me.FireObjectPlot.AutoScaleAutoGeneratedAxes = True
        Me.FireObjectPlot.AutoScaleTitle = False
        Me.FireObjectPlot.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.FireObjectPlot.DateTimeToolTip = False
        Me.FireObjectPlot.Legend = Nothing
        Me.FireObjectPlot.LegendZOrder = -1
        Me.FireObjectPlot.Location = New System.Drawing.Point(9, 166)
        Me.FireObjectPlot.Name = "FireObjectPlot"
        Me.FireObjectPlot.RightMenu = Nothing
        Me.FireObjectPlot.ShowCoordinates = True
        Me.FireObjectPlot.Size = New System.Drawing.Size(560, 248)
        Me.FireObjectPlot.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None
        Me.FireObjectPlot.TabIndex = 145
        Me.FireObjectPlot.TabStop = False
        Me.FireObjectPlot.Title = ""
        Me.FireObjectPlot.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.FireObjectPlot.XAxis1 = Nothing
        Me.FireObjectPlot.XAxis2 = Nothing
        Me.FireObjectPlot.YAxis1 = Nothing
        Me.FireObjectPlot.YAxis2 = Nothing
        '
        't2StatusStrip
        '
        Me.t2StatusStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.t2StripStatusLabel})
        Me.t2StatusStrip.Location = New System.Drawing.Point(0, 472)
        Me.t2StatusStrip.Name = "t2StatusStrip"
        Me.t2StatusStrip.Size = New System.Drawing.Size(579, 22)
        Me.t2StatusStrip.TabIndex = 146
        '
        't2StripStatusLabel
        '
        Me.t2StripStatusLabel.Name = "t2StripStatusLabel"
        Me.t2StripStatusLabel.Size = New System.Drawing.Size(0, 17)
        '
        't2Fire
        '
        Me.AcceptButton = Me.t2OK
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.t2Cancel
        Me.ClientSize = New System.Drawing.Size(579, 494)
        Me.Controls.Add(Me.t2StatusStrip)
        Me.Controls.Add(Me.FireObjectPlot)
        Me.Controls.Add(Me.t2PeakHRR)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.t2SteadyTime)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.t2DecayTime)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.t2GrowthTime)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.t2GrowthType)
        Me.Controls.Add(Me.t2Cancel)
        Me.Controls.Add(Me.t2OK)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "t2Fire"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Define New Fire"
        Me.t2StatusStrip.ResumeLayout(False)
        Me.t2StatusStrip.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents t2OK As System.Windows.Forms.Button
    Friend WithEvents t2Cancel As System.Windows.Forms.Button
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents t2GrowthType As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents t2GrowthTime As System.Windows.Forms.TextBox
    Friend WithEvents t2DecayTime As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents t2SteadyTime As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents t2PeakHRR As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents FireObjectPlot As NPlot.Windows.PlotSurface2D
    Friend WithEvents t2StatusStrip As System.Windows.Forms.StatusStrip
    Friend WithEvents t2StripStatusLabel As System.Windows.Forms.ToolStripStatusLabel
End Class

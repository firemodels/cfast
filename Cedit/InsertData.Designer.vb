<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class InsertData
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(InsertData))
        Me.InsertDataSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.InsertDeselect = New System.Windows.Forms.Button()
        Me.InsertSelect = New System.Windows.Forms.Button()
        Me.FireObjectsCancel = New System.Windows.Forms.Button()
        Me.FireObjectsOK = New System.Windows.Forms.Button()
        CType(Me.InsertDataSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'InsertDataSummary
        '
        Me.InsertDataSummary.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.InsertDataSummary.ColumnInfo = resources.GetString("InsertDataSummary.ColumnInfo")
        Me.InsertDataSummary.Location = New System.Drawing.Point(12, 12)
        Me.InsertDataSummary.Name = "InsertDataSummary"
        Me.InsertDataSummary.Rows.DefaultSize = 19
        Me.InsertDataSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.InsertDataSummary.Size = New System.Drawing.Size(890, 156)
        Me.InsertDataSummary.TabIndex = 0
        '
        'InsertDeselect
        '
        Me.InsertDeselect.Location = New System.Drawing.Point(366, 189)
        Me.InsertDeselect.Name = "InsertDeselect"
        Me.InsertDeselect.Size = New System.Drawing.Size(75, 23)
        Me.InsertDeselect.TabIndex = 7
        Me.InsertDeselect.Text = "Deselect All"
        '
        'InsertSelect
        '
        Me.InsertSelect.Location = New System.Drawing.Point(266, 189)
        Me.InsertSelect.Name = "InsertSelect"
        Me.InsertSelect.Size = New System.Drawing.Size(75, 23)
        Me.InsertSelect.TabIndex = 6
        Me.InsertSelect.Text = "Select All"
        '
        'FireObjectsCancel
        '
        Me.FireObjectsCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FireObjectsCancel.Location = New System.Drawing.Point(616, 189)
        Me.FireObjectsCancel.Name = "FireObjectsCancel"
        Me.FireObjectsCancel.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsCancel.TabIndex = 155
        Me.FireObjectsCancel.Text = "Cancel"
        '
        'FireObjectsOK
        '
        Me.FireObjectsOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.FireObjectsOK.Location = New System.Drawing.Point(516, 189)
        Me.FireObjectsOK.Name = "FireObjectsOK"
        Me.FireObjectsOK.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsOK.TabIndex = 154
        Me.FireObjectsOK.Text = "OK"
        '
        'InsertData
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(914, 241)
        Me.Controls.Add(Me.FireObjectsCancel)
        Me.Controls.Add(Me.FireObjectsOK)
        Me.Controls.Add(Me.InsertDeselect)
        Me.Controls.Add(Me.InsertSelect)
        Me.Controls.Add(Me.InsertDataSummary)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "InsertData"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "InsertData"
        CType(Me.InsertDataSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents InsertDataSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents InsertDeselect As System.Windows.Forms.Button
    Friend WithEvents InsertSelect As System.Windows.Forms.Button
    Friend WithEvents FireObjectsCancel As System.Windows.Forms.Button
    Friend WithEvents FireObjectsOK As System.Windows.Forms.Button
End Class

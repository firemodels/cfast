<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditFireObject
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditFireObject))
        Me.FireObjectsCancel = New System.Windows.Forms.Button()
        Me.FireObjectsOK = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'FireObjectsCancel
        '
        Me.FireObjectsCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FireObjectsCancel.Location = New System.Drawing.Point(523, 477)
        Me.FireObjectsCancel.Name = "FireObjectsCancel"
        Me.FireObjectsCancel.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsCancel.TabIndex = 153
        Me.FireObjectsCancel.Text = "Cancel"
        '
        'FireObjectsOK
        '
        Me.FireObjectsOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.FireObjectsOK.Location = New System.Drawing.Point(395, 477)
        Me.FireObjectsOK.Name = "FireObjectsOK"
        Me.FireObjectsOK.Size = New System.Drawing.Size(75, 23)
        Me.FireObjectsOK.TabIndex = 152
        Me.FireObjectsOK.Text = "OK"
        '
        'EditFireObject
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(992, 531)
        Me.Controls.Add(Me.FireObjectsCancel)
        Me.Controls.Add(Me.FireObjectsOK)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "EditFireObject"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Define New Fire"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents FireObjectsCancel As System.Windows.Forms.Button
    Friend WithEvents FireObjectsOK As System.Windows.Forms.Button
End Class

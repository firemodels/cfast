<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class UpdateInputFiles
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(UpdateInputFiles))
        Me.UpdateErrors = New System.Windows.Forms.TextBox()
        Me.OK = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'UpdateErrors
        '
        Me.UpdateErrors.Location = New System.Drawing.Point(12, 12)
        Me.UpdateErrors.Multiline = True
        Me.UpdateErrors.Name = "UpdateErrors"
        Me.UpdateErrors.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.UpdateErrors.Size = New System.Drawing.Size(960, 254)
        Me.UpdateErrors.TabIndex = 19
        Me.UpdateErrors.TabStop = False
        Me.UpdateErrors.Text = "No Errors"
        '
        'OK
        '
        Me.OK.Location = New System.Drawing.Point(455, 294)
        Me.OK.Name = "OK"
        Me.OK.Size = New System.Drawing.Size(75, 23)
        Me.OK.TabIndex = 20
        Me.OK.Text = "OK"
        Me.OK.UseVisualStyleBackColor = True
        '
        'UpdateInputFiles
        '
        Me.AcceptButton = Me.OK
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(984, 325)
        Me.Controls.Add(Me.OK)
        Me.Controls.Add(Me.UpdateErrors)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "UpdateInputFiles"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Update Data Files"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents UpdateErrors As System.Windows.Forms.TextBox
    Friend WithEvents OK As System.Windows.Forms.Button
End Class

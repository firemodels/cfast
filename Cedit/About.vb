Public Class About
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
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents AboutVersion As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents LinkLabel1 As System.Windows.Forms.LinkLabel
    Friend WithEvents AboutRevision As System.Windows.Forms.Label
    Friend WithEvents AboutRevisionDate As System.Windows.Forms.Label
    Friend WithEvents AboutCompileDate As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents TextBox2 As System.Windows.Forms.TextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(About))
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.AboutVersion = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.AboutRevision = New System.Windows.Forms.Label()
        Me.AboutRevisionDate = New System.Windows.Forms.Label()
        Me.AboutCompileDate = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TextBox1
        '
        Me.TextBox1.BackColor = System.Drawing.SystemColors.Control
        Me.TextBox1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TextBox1.Location = New System.Drawing.Point(20, 185)
        Me.TextBox1.Multiline = True
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ReadOnly = True
        Me.TextBox1.Size = New System.Drawing.Size(504, 152)
        Me.TextBox1.TabIndex = 0
        Me.TextBox1.TabStop = False
        Me.TextBox1.Text = resources.GetString("TextBox1.Text")
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(16, 24)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(46, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "CFAST"
        '
        'AboutVersion
        '
        Me.AboutVersion.AutoSize = True
        Me.AboutVersion.Location = New System.Drawing.Point(68, 24)
        Me.AboutVersion.Name = "AboutVersion"
        Me.AboutVersion.Size = New System.Drawing.Size(31, 13)
        Me.AboutVersion.TabIndex = 2
        Me.AboutVersion.Text = "7.0.0"
        Me.AboutVersion.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 99)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(116, 13)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = "Engineering Laboratory"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(12, 115)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(229, 13)
        Me.Label3.TabIndex = 4
        Me.Label3.Text = "National Institute of Standards and Technology"
        '
        'Label4
        '
        Me.Label4.Location = New System.Drawing.Point(12, 131)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(224, 16)
        Me.Label4.TabIndex = 5
        Me.Label4.Text = "Gaithersburg, MD 20899"
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(376, 16)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(136, 96)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 6
        Me.PictureBox1.TabStop = False
        '
        'LinkLabel1
        '
        Me.LinkLabel1.Location = New System.Drawing.Point(12, 155)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(224, 16)
        Me.LinkLabel1.TabIndex = 7
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "http://cfast.nist.gov"
        '
        'TextBox2
        '
        Me.TextBox2.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.TextBox2.Location = New System.Drawing.Point(20, 353)
        Me.TextBox2.Multiline = True
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.ReadOnly = True
        Me.TextBox2.Size = New System.Drawing.Size(504, 32)
        Me.TextBox2.TabIndex = 8
        Me.TextBox2.TabStop = False
        Me.TextBox2.Text = "CEdit includes software developed as part of the NPlot library project available " & _
    "from: http://netcontrols.org/nplot/wiki/"
        '
        'AboutRevision
        '
        Me.AboutRevision.AutoSize = True
        Me.AboutRevision.Location = New System.Drawing.Point(215, 24)
        Me.AboutRevision.Name = "AboutRevision"
        Me.AboutRevision.Size = New System.Drawing.Size(53, 13)
        Me.AboutRevision.TabIndex = 9
        Me.AboutRevision.Text = "Unknown"
        Me.AboutRevision.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'AboutRevisionDate
        '
        Me.AboutRevisionDate.AutoSize = True
        Me.AboutRevisionDate.Location = New System.Drawing.Point(215, 44)
        Me.AboutRevisionDate.Name = "AboutRevisionDate"
        Me.AboutRevisionDate.Size = New System.Drawing.Size(53, 13)
        Me.AboutRevisionDate.TabIndex = 10
        Me.AboutRevisionDate.Text = "Unknown"
        '
        'AboutCompileDate
        '
        Me.AboutCompileDate.AutoSize = True
        Me.AboutCompileDate.Location = New System.Drawing.Point(215, 64)
        Me.AboutCompileDate.Name = "AboutCompileDate"
        Me.AboutCompileDate.Size = New System.Drawing.Size(53, 13)
        Me.AboutCompileDate.TabIndex = 11
        Me.AboutCompileDate.Text = "Unknown"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(129, 24)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(51, 13)
        Me.Label5.TabIndex = 12
        Me.Label5.Text = "Revision:"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(129, 44)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(80, 13)
        Me.Label6.TabIndex = 13
        Me.Label6.Text = "Revision Date: "
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(129, 64)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(62, 13)
        Me.Label7.TabIndex = 14
        Me.Label7.Text = "Created on:"
        '
        'About
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(552, 414)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.AboutCompileDate)
        Me.Controls.Add(Me.AboutRevisionDate)
        Me.Controls.Add(Me.AboutRevision)
        Me.Controls.Add(Me.TextBox2)
        Me.Controls.Add(Me.LinkLabel1)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.AboutVersion)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.TextBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "About"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "About"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Private Sub About_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim Revision As String = "$Revision: unknown $", aRev As String, aVersion As String
        Dim RevisionDate As String = "$RevisionDate: unknown $"
        Dim CompileDate As String = "$CompileDate: unknown $"
        Dim sLen As Integer
        sLen = Len(Application.ProductVersion)
        aVersion = Application.ProductVersion.Substring(0, sLen - 1)
        sLen = Len(Revision)
        aRev = Revision.Substring(11, sLen - 13)
        Me.AboutRevision.Text = aRev
        sLen = Len(RevisionDate)
        aRev = RevisionDate.Substring(15, sLen - 17)
        Me.AboutRevisionDate.Text = aRev
        sLen = Len(CompileDate)
        aRev = CompileDate.Substring(14, sLen - 15)
        Me.AboutCompileDate.Text = aRev
    End Sub

    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Process.Start(LinkLabel1.Text)
    End Sub

    Private Sub Label5_Click(sender As Object, e As EventArgs)

    End Sub
End Class

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
        TextBox1 = New System.Windows.Forms.TextBox()
        Label1 = New System.Windows.Forms.Label()
        AboutVersion = New System.Windows.Forms.Label()
        Label2 = New System.Windows.Forms.Label()
        Label3 = New System.Windows.Forms.Label()
        Label4 = New System.Windows.Forms.Label()
        PictureBox1 = New System.Windows.Forms.PictureBox()
        LinkLabel1 = New System.Windows.Forms.LinkLabel()
        TextBox2 = New System.Windows.Forms.TextBox()
        AboutRevision = New System.Windows.Forms.Label()
        AboutRevisionDate = New System.Windows.Forms.Label()
        AboutCompileDate = New System.Windows.Forms.Label()
        Label5 = New System.Windows.Forms.Label()
        Label6 = New System.Windows.Forms.Label()
        Label7 = New System.Windows.Forms.Label()
        CType(PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        '
        'TextBox1
        '
        TextBox1.BackColor = System.Drawing.SystemColors.Control
        TextBox1.BorderStyle = System.Windows.Forms.BorderStyle.None
        TextBox1.Location = New System.Drawing.Point(20, 185)
        TextBox1.Multiline = True
        TextBox1.Name = "TextBox1"
        TextBox1.ReadOnly = True
        TextBox1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        TextBox1.Size = New System.Drawing.Size(504, 152)
        TextBox1.TabIndex = 0
        TextBox1.TabStop = False
        TextBox1.Text = resources.GetString("TextBox1.Text")
        '
        'Label1
        '
        Label1.AutoSize = True
        Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Label1.Location = New System.Drawing.Point(16, 24)
        Label1.Name = "Label1"
        Label1.Size = New System.Drawing.Size(46, 13)
        Label1.TabIndex = 1
        Label1.Text = "CFAST"
        '
        'AboutVersion
        '
        AboutVersion.AutoSize = True
        AboutVersion.Location = New System.Drawing.Point(68, 24)
        AboutVersion.Name = "AboutVersion"
        AboutVersion.Size = New System.Drawing.Size(31, 13)
        AboutVersion.TabIndex = 2
        AboutVersion.Text = "7.1.0"
        AboutVersion.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label2
        '
        Label2.AutoSize = True
        Label2.Location = New System.Drawing.Point(12, 99)
        Label2.Name = "Label2"
        Label2.Size = New System.Drawing.Size(116, 13)
        Label2.TabIndex = 3
        Label2.Text = "Engineering Laboratory"
        '
        'Label3
        '
        Label3.AutoSize = True
        Label3.Location = New System.Drawing.Point(12, 115)
        Label3.Name = "Label3"
        Label3.Size = New System.Drawing.Size(229, 13)
        Label3.TabIndex = 4
        Label3.Text = "National Institute of Standards and Technology"
        '
        'Label4
        '
        Label4.Location = New System.Drawing.Point(12, 131)
        Label4.Name = "Label4"
        Label4.Size = New System.Drawing.Size(224, 16)
        Label4.TabIndex = 5
        Label4.Text = "Gaithersburg, MD 20899"
        '
        'PictureBox1
        '
        PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        PictureBox1.Location = New System.Drawing.Point(376, 16)
        PictureBox1.Name = "PictureBox1"
        PictureBox1.Size = New System.Drawing.Size(136, 96)
        PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        PictureBox1.TabIndex = 6
        PictureBox1.TabStop = False
        '
        'LinkLabel1
        '
        LinkLabel1.Location = New System.Drawing.Point(12, 155)
        LinkLabel1.Name = "LinkLabel1"
        LinkLabel1.Size = New System.Drawing.Size(224, 16)
        LinkLabel1.TabIndex = 7
        LinkLabel1.TabStop = True
        LinkLabel1.Text = "http://cfast.nist.gov"
        '
        'TextBox2
        '
        TextBox2.BorderStyle = System.Windows.Forms.BorderStyle.None
        TextBox2.Location = New System.Drawing.Point(20, 353)
        TextBox2.Multiline = True
        TextBox2.Name = "TextBox2"
        TextBox2.ReadOnly = True
        TextBox2.Size = New System.Drawing.Size(504, 32)
        TextBox2.TabIndex = 8
        TextBox2.TabStop = False
        TextBox2.Text = "CEdit includes software developed as part of the NPlot library project available " & _
    "from: http://netcontrols.org/nplot/wiki/"
        '
        'AboutRevision
        '
        AboutRevision.AutoSize = True
        AboutRevision.Location = New System.Drawing.Point(215, 24)
        AboutRevision.Name = "AboutRevision"
        AboutRevision.Size = New System.Drawing.Size(53, 13)
        AboutRevision.TabIndex = 9
        AboutRevision.Text = "Unknown"
        AboutRevision.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'AboutRevisionDate
        '
        AboutRevisionDate.AutoSize = True
        AboutRevisionDate.Location = New System.Drawing.Point(215, 44)
        AboutRevisionDate.Name = "AboutRevisionDate"
        AboutRevisionDate.Size = New System.Drawing.Size(53, 13)
        AboutRevisionDate.TabIndex = 10
        AboutRevisionDate.Text = "Unknown"
        '
        'AboutCompileDate
        '
        AboutCompileDate.AutoSize = True
        AboutCompileDate.Location = New System.Drawing.Point(215, 64)
        AboutCompileDate.Name = "AboutCompileDate"
        AboutCompileDate.Size = New System.Drawing.Size(53, 13)
        AboutCompileDate.TabIndex = 11
        AboutCompileDate.Text = "Unknown"
        '
        'Label5
        '
        Label5.AutoSize = True
        Label5.Location = New System.Drawing.Point(129, 24)
        Label5.Name = "Label5"
        Label5.Size = New System.Drawing.Size(51, 13)
        Label5.TabIndex = 12
        Label5.Text = "Revision:"
        '
        'Label6
        '
        Label6.AutoSize = True
        Label6.Location = New System.Drawing.Point(129, 44)
        Label6.Name = "Label6"
        Label6.Size = New System.Drawing.Size(80, 13)
        Label6.TabIndex = 13
        Label6.Text = "Revision Date: "
        '
        'Label7
        '
        Label7.AutoSize = True
        Label7.Location = New System.Drawing.Point(129, 64)
        Label7.Name = "Label7"
        Label7.Size = New System.Drawing.Size(62, 13)
        Label7.TabIndex = 14
        Label7.Text = "Created on:"
        '
        'About
        '
        AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        ClientSize = New System.Drawing.Size(552, 414)
        Controls.Add(Label7)
        Controls.Add(Label6)
        Controls.Add(Label5)
        Controls.Add(AboutCompileDate)
        Controls.Add(AboutRevisionDate)
        Controls.Add(AboutRevision)
        Controls.Add(TextBox2)
        Controls.Add(LinkLabel1)
        Controls.Add(PictureBox1)
        Controls.Add(Label4)
        Controls.Add(Label3)
        Controls.Add(Label2)
        Controls.Add(AboutVersion)
        Controls.Add(Label1)
        Controls.Add(TextBox1)
        FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Name = "About"
        ShowInTaskbar = False
        StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Text = "About"
        CType(PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
        PerformLayout()

    End Sub

#End Region

    Private Sub About_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim Revision As String = "$Revision: unknown $", aRev As String, aVersion As String
        Dim RevisionDate As String = "$RevisionDate: unknown $"
        Dim CompileDate As String = "$CompileDate: unknown $"
        Dim sLen As Integer
        sLen = Len(Application.ProductVersion)
        aVersion = Application.ProductVersion.Substring(0, sLen - 2)
        AboutVersion.Text = aVersion
        sLen = Len(Revision)
        aRev = Revision.Substring(11, sLen - 13)
        AboutRevision.Text = aRev
        sLen = Len(RevisionDate)
        aRev = RevisionDate.Substring(15, sLen - 17)
        AboutRevisionDate.Text = aRev
        sLen = Len(CompileDate)
        aRev = CompileDate.Substring(14, sLen - 15)
        AboutCompileDate.Text = aRev
    End Sub

    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Process.Start(LinkLabel1.Text)
    End Sub
End Class

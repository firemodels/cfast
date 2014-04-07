Public Class RunModel
    Inherits System.Windows.Forms.Form
    'Public CFastInputFile As String
    'Public CFASTSimulationTime As Single
    'Public CommandWindowVisible As Boolean = False
    'Public ExitCode As Integer = 0
    Private ProcessID As Integer
    Private localById As Process
    Private FileName As String, IO As Integer = 1
    Friend WithEvents RunOptions As System.Windows.Forms.Label
    Friend WithEvents RunJac As System.Windows.Forms.Button
    Private CurrentTime As Single
    Private DebugOn As Boolean = False
    Private JacobianOn As Boolean = False

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
    Friend WithEvents RunSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents RunOK As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents RunTime As System.Windows.Forms.TextBox
    Friend WithEvents RunDT As System.Windows.Forms.TextBox
    Friend WithEvents RunTimer As System.Windows.Forms.Timer
    Friend WithEvents RunStop As System.Windows.Forms.Button
    Friend WithEvents RunProgress As System.Windows.Forms.ProgressBar
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents RunUpdate As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(RunModel))
        Me.RunSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.RunOK = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.RunTime = New System.Windows.Forms.TextBox()
        Me.RunDT = New System.Windows.Forms.TextBox()
        Me.RunTimer = New System.Windows.Forms.Timer(Me.components)
        Me.RunStop = New System.Windows.Forms.Button()
        Me.RunProgress = New System.Windows.Forms.ProgressBar()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.RunUpdate = New System.Windows.Forms.Button()
        Me.RunOptions = New System.Windows.Forms.Label()
        Me.RunJac = New System.Windows.Forms.Button()
        CType(Me.RunSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'RunSummary
        '
        Me.RunSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.RunSummary.ColumnInfo = resources.GetString("RunSummary.ColumnInfo")
        Me.RunSummary.ExtendLastCol = True
        Me.RunSummary.Location = New System.Drawing.Point(24, 80)
        Me.RunSummary.Name = "RunSummary"
        Me.RunSummary.Rows.DefaultSize = 17
        Me.RunSummary.Size = New System.Drawing.Size(942, 344)
        Me.RunSummary.TabIndex = 3
        '
        'RunOK
        '
        Me.RunOK.Location = New System.Drawing.Point(212, 445)
        Me.RunOK.Name = "RunOK"
        Me.RunOK.Size = New System.Drawing.Size(100, 23)
        Me.RunOK.TabIndex = 4
        Me.RunOK.Text = "Close"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(296, 34)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(84, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "Simulation Time:"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(32, 34)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(95, 13)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Current Time Step:"
        '
        'RunTime
        '
        Me.RunTime.Location = New System.Drawing.Point(400, 32)
        Me.RunTime.Name = "RunTime"
        Me.RunTime.Size = New System.Drawing.Size(100, 20)
        Me.RunTime.TabIndex = 2
        Me.RunTime.Text = "0 s"
        '
        'RunDT
        '
        Me.RunDT.Location = New System.Drawing.Point(144, 32)
        Me.RunDT.Name = "RunDT"
        Me.RunDT.Size = New System.Drawing.Size(100, 20)
        Me.RunDT.TabIndex = 1
        Me.RunDT.Text = "0.1 s"
        '
        'RunTimer
        '
        Me.RunTimer.Interval = 250
        '
        'RunStop
        '
        Me.RunStop.Location = New System.Drawing.Point(364, 445)
        Me.RunStop.Name = "RunStop"
        Me.RunStop.Size = New System.Drawing.Size(100, 23)
        Me.RunStop.TabIndex = 5
        Me.RunStop.Text = "Stop"
        '
        'RunProgress
        '
        Me.RunProgress.Location = New System.Drawing.Point(616, 34)
        Me.RunProgress.Name = "RunProgress"
        Me.RunProgress.Size = New System.Drawing.Size(344, 16)
        Me.RunProgress.Step = 5
        Me.RunProgress.TabIndex = 8
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(536, 34)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(51, 13)
        Me.Label3.TabIndex = 9
        Me.Label3.Text = "Progress:"
        '
        'RunUpdate
        '
        Me.RunUpdate.Location = New System.Drawing.Point(516, 445)
        Me.RunUpdate.Name = "RunUpdate"
        Me.RunUpdate.Size = New System.Drawing.Size(100, 23)
        Me.RunUpdate.TabIndex = 6
        Me.RunUpdate.Text = "Update"
        '
        'RunOptions
        '
        Me.RunOptions.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.RunOptions.AutoSize = True
        Me.RunOptions.Location = New System.Drawing.Point(12, 502)
        Me.RunOptions.Name = "RunOptions"
        Me.RunOptions.Size = New System.Drawing.Size(0, 13)
        Me.RunOptions.TabIndex = 10
        '
        'RunJacobian
        '
        Me.RunJac.Location = New System.Drawing.Point(672, 445)
        Me.RunJac.Name = "RunJac"
        Me.RunJac.Size = New System.Drawing.Size(100, 23)
        Me.RunJac.TabIndex = 11
        Me.RunJac.Text = "Jacobian Off"
        Me.RunJac.Visible = False
        '
        'RunJac
        '
        Me.RunJac.Location = New System.Drawing.Point(672, 445)
        Me.RunJac.Name = "RunJac"
        Me.RunJac.Size = New System.Drawing.Size(100, 23)
        Me.RunJac.TabIndex = 11
        Me.RunJac.Text = "Jacobian Off"
        Me.RunJac.Visible = False
        '
        'RunModel
        '
        Me.AcceptButton = Me.RunOK
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 534)
        Me.Controls.Add(Me.RunJac)
        Me.Controls.Add(Me.RunOptions)
        Me.Controls.Add(Me.RunUpdate)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.RunProgress)
        Me.Controls.Add(Me.RunStop)
        Me.Controls.Add(Me.RunDT)
        Me.Controls.Add(Me.RunTime)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.RunSummary)
        Me.Controls.Add(Me.RunOK)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximumSize = New System.Drawing.Size(1000, 600)
        Me.Name = "RunModel"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "RunModel"
        CType(Me.RunSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region
    Private Sub RunModel_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim CommandString As String, found As Integer
        Me.RunSummary(0, 1) = "Upper Layer" + Chr(10) + "Temperature" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Temperature).Units.Substring(1) + ")"
        Me.RunSummary(0, 2) = "Lower Layer" + Chr(10) + "Temperature" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Temperature).Units.Substring(1) + ")"
        Me.RunSummary(0, 3) = "Interface Height" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Length).Units.Substring(1) + ")"
        Me.RunSummary(0, 4) = "Pyrolysis Rate" + Chr(10) + "(" + myUnits.Convert(UnitsNum.MassLoss).Units.Substring(1) + ")"
        Me.RunSummary(0, 5) = "Fire Size" + Chr(10) + "(" + myUnits.Convert(UnitsNum.HRR).Units.Substring(1) + ")"
        Me.RunSummary(0, 6) = "Pressure" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Pressure).Units.Substring(1) + ")"
        Me.RunSummary(0, 7) = "Ambient Target" + Chr(10) + "Flux" + Chr(10) + "(" + myUnits.Convert(UnitsNum.HeatFlux).Units.Substring(1) + ")"
        Me.RunSummary.AutoSizeRow(0)
        Me.DebugOn = False
        Me.JacobianOn = False
        If DebugOutput Then
            Me.RunOK.Location = New Point(212, 445)
            Me.RunStop.Location = New Point(364, 445)
            Me.RunUpdate.Location = New Point(516, 445)
            Me.RunJac.Location = New Point(672, 445)
            Me.RunUpdate.Text = "Debug is Off"
            Me.RunJac.Text = "Jacobian is Off"
            Me.RunJac.Visible = True
            Me.RunJac.Enabled = False
        Else
            Me.RunOK.Location = New Point(290, 445)
            Me.RunStop.Location = New Point(442, 445)
            Me.RunUpdate.Location = New Point(594, 445)
            Me.RunJac.Location = New Point(672, 445)
            Me.RunUpdate.Text = "Update"
            Me.RunJac.Text = "Jacobian is Off"
            Me.RunJac.Visible = False
        End If
        Me.JacobianOn = False
        ' Start the model run and then just look for the status file every so often
        found = CFastInputFile.IndexOf(" ", 0)
        If found <= 0 Then
            CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + System.IO.Path.GetFileNameWithoutExtension(CFastInputFile)
        Else
            CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + """" + System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + """"
        End If
        RunOptions.Text = "RunOptions: "
        If TotalMassCFASTOutput Then
            CommandString += " /T"
            RunOptions.Text += "Total Mass Output"
        End If
        If NetHeatFluxCFASTOutput Then
            CommandString += " /N"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Net Heat Flux Output"
        End If
        If ValidationOutput Then
            CommandString += " /V"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Validation Output"
        End If
        Me.RunOK.Enabled = False
        Me.RunStop.Enabled = True
        Me.RunUpdate.Enabled = True
        ExitCode = 0
        Me.RunProgress.Value = 0
        Me.RunTimer.Enabled = True
        If CommandWindowVisible Then
            ProcessID = Shell(CommandString, AppWinStyle.NormalNoFocus)
        Else
            ProcessID = Shell(CommandString, AppWinStyle.Hide)
        End If

        localById = System.Diagnostics.Process.GetProcessById(ProcessID)
    End Sub
    Private Sub RunTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunTimer.Tick
        Me.RunTimer.Enabled = False
        Dim StatusFileExists As Boolean
        Dim iLine As Integer
        Dim ir As Integer
        Dim Progress As Integer
        If localById.HasExited Then
            Me.RunTimer.Enabled = False
            Me.RunOK.Enabled = True
            Me.RunStop.Enabled = False
            Me.RunUpdate.Enabled = False
            Me.RunDT.Text = "CFAST finished"
        End If
        FileName = System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + ".status"
        StatusFileExists = System.IO.File.Exists(FileName)
        If StatusFileExists Then
            Dim ln As String
            FileOpen(IO, FileName, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
            iLine = 0
            ir = 1
            Do Until EOF(IO)
                ln = LineInput(IO)
                iLine = iLine + 1
                Select Case iLine
                    Case 1
                        If ln.Substring(0, 6) = "Status" Then
                            Progress = Math.Max(Math.Min(Val(ln.Substring(16, 10)) / CFASTSimulationTime * 100, 100), 0)
                            Me.RunProgress.Value = Progress
                            CurrentTime = myUnits.Convert(UnitsNum.Time).FromSI(ln.Substring(16, 10))
                            Me.RunTime.Text = CurrentTime.ToString + myUnits.Convert(UnitsNum.Time).Units
                            If Me.RunOK.Enabled Then
                                Me.RunDT.Text = "CFAST finished"
                            Else
                                Me.RunDT.Text = myUnits.Convert(UnitsNum.Time).FromSI(ln.Substring(32, 10)).ToString + myUnits.Convert(UnitsNum.Time).Units
                            End If
                        End If
                    Case Is > 6
                        If ln.Substring(2, 7) <> "Outside" Then
                            Me.RunSummary(ir, 0) = Val(ln.Substring(0, 5))
                            Me.RunSummary(ir, 1) = Math.Round(myUnits.Convert(UnitsNum.Temperature).FromSI(Val(ln.Substring(12, 8)) + 273.15), 1).ToString
                            Me.RunSummary(ir, 2) = Math.Round(myUnits.Convert(UnitsNum.Temperature).FromSI(Val(ln.Substring(20, 8)) + 273.15), 1).ToString
                            Me.RunSummary(ir, 3) = Math.Round(myUnits.Convert(UnitsNum.Length).FromSI(Val(ln.Substring(30, 9))), 2).ToString
                            Me.RunSummary(ir, 4) = Math.Round(myUnits.Convert(UnitsNum.MassLoss).FromSI(Val(ln.Substring(39, 10))), 5).ToString
                            Me.RunSummary(ir, 5) = Math.Round(myUnits.Convert(UnitsNum.HRR).FromSI(Val(ln.Substring(49, 10))), 2).ToString
                            Me.RunSummary(ir, 6) = Math.Round(myUnits.Convert(UnitsNum.Pressure).FromSI(Val(ln.Substring(59, 10))), 4).ToString
                            Me.RunSummary(ir, 7) = Math.Round(myUnits.Convert(UnitsNum.HeatFlux).FromSI(Val(ln.Substring(69, 10))), 3).ToString
                        Else
                            Me.RunSummary(ir, 0) = "Outside"
                            Me.RunSummary(ir, 5) = Math.Round(myUnits.Convert(UnitsNum.HRR).FromSI(Val(ln.Substring(48, 10))), 2).ToString
                        End If
                        ir += 1
                End Select
            Loop
            FileClose(IO)
        End If
        Me.RunTimer.Enabled = True
    End Sub

    Private Sub RunStop_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunStop.Click
        FileName = System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + ".stop"
        FileOpen(IO, FileName, OpenMode.Output)
        FileClose(IO)
        Me.RunOK.Enabled = True
        Me.RunStop.Enabled = False
        Me.RunUpdate.Enabled = False
        Me.RunDT.Text = "Stopped"
        Me.RunTimer.Enabled = False
    End Sub

    Private Sub RunOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunOK.Click
        Me.RunTimer.Enabled = False
        If System.IO.File.Exists(FileName + ".debug") Then System.IO.File.Delete(FileName + ".debug")
        If System.IO.File.Exists(FileName + ".jacobian") Then System.IO.File.Delete(FileName + ".jacobian")
        Me.Close()
    End Sub

    Private Sub RunUpdate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunUpdate.Click
        RunTimer.Enabled = False
        FileName = System.IO.Path.GetFileNameWithoutExtension(CFastInputFile)
        If DebugOutput Then
            If Me.DebugOn Then
                Me.DebugOn = False
                Me.RunUpdate.Text = "Debug is Off"
                Me.RunJac.Text = "Jacobian is Off"
                Me.RunJac.Enabled = False
                If System.IO.File.Exists(FileName + ".debug") Then System.IO.File.Delete(FileName + ".debug")
                If System.IO.File.Exists(FileName + ".jacobian") Then System.IO.File.Delete(FileName + ".jacobian")
            Else
                Me.DebugOn = True
                Me.RunUpdate.Text = "Debug is On"
                Me.RunJac.Enabled = True
                FileOpen(IO, FileName + ".debug", OpenMode.Output)
                FileClose(IO)
            End If
        Else
            FileOpen(IO, FileName + ".query", OpenMode.Output)
            FileClose(IO)
        End If
        Me.RunTimer.Enabled = True
    End Sub

    Private Sub Jacobian_Click(sender As System.Object, e As System.EventArgs) Handles RunJac.Click
        RunTimer.Enabled = False
        If DebugOutput Then
            FileName = System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + ".jacobian"
            If Me.JacobianOn Then
                Me.JacobianOn = False
                Me.RunJac.Text = "Jacobian is Off"
                If System.IO.File.Exists(FileName) Then System.IO.File.Delete(FileName)
            Else
                Me.JacobianOn = True
                Me.RunJac.Text = "Jacobian is On"
                FileOpen(IO, FileName, OpenMode.Output)
                FileClose(IO)
            End If
        End If
        Me.RunTimer.Enabled = True
    End Sub
End Class
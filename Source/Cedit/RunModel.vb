Public Class RunModel
    Inherits System.Windows.Forms.Form
    'Public CFastInputFile As String
    'Public CFASTSimulationTime As Single
    'Public CommandWindowVisible As Boolean = False
    'Public ExitCode As Integer = 0
    Private ProcessID As Integer
    Private localById As Process
    Private FileName As String, IO As Integer = 1, RunhasFinished As Boolean = False
    Friend WithEvents RunOptions As System.Windows.Forms.Label
    Friend WithEvents RunJac As System.Windows.Forms.Button
    Private CurrentTime As Single
    Private DebugOn As Boolean = False
    Friend WithEvents RunErrors As System.Windows.Forms.TextBox
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
        components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(RunModel))
        RunSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        RunOK = New System.Windows.Forms.Button()
        Label1 = New System.Windows.Forms.Label()
        Label2 = New System.Windows.Forms.Label()
        RunTime = New System.Windows.Forms.TextBox()
        RunDT = New System.Windows.Forms.TextBox()
        RunTimer = New System.Windows.Forms.Timer(components)
        RunStop = New System.Windows.Forms.Button()
        RunProgress = New System.Windows.Forms.ProgressBar()
        Label3 = New System.Windows.Forms.Label()
        RunUpdate = New System.Windows.Forms.Button()
        RunOptions = New System.Windows.Forms.Label()
        RunJac = New System.Windows.Forms.Button()
        RunErrors = New System.Windows.Forms.TextBox()
        CType(RunSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        '
        'RunSummary
        '
        RunSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        RunSummary.ColumnInfo = resources.GetString("RunSummary.ColumnInfo")
        RunSummary.ExtendLastCol = True
        RunSummary.Location = New System.Drawing.Point(82, 80)
        RunSummary.Name = "RunSummary"
        RunSummary.Rows.DefaultSize = 17
        RunSummary.Size = New System.Drawing.Size(821, 265)
        RunSummary.TabIndex = 3
        RunSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'RunOK
        '
        RunOK.Location = New System.Drawing.Point(212, 520)
        RunOK.Name = "RunOK"
        RunOK.Size = New System.Drawing.Size(100, 23)
        RunOK.TabIndex = 4
        RunOK.Text = "Close"
        '
        'Label1
        '
        Label1.AutoSize = True
        Label1.Location = New System.Drawing.Point(296, 34)
        Label1.Name = "Label1"
        Label1.Size = New System.Drawing.Size(84, 13)
        Label1.TabIndex = 3
        Label1.Text = "Simulation Time:"
        '
        'Label2
        '
        Label2.AutoSize = True
        Label2.Location = New System.Drawing.Point(32, 34)
        Label2.Name = "Label2"
        Label2.Size = New System.Drawing.Size(95, 13)
        Label2.TabIndex = 4
        Label2.Text = "Current Time Step:"
        '
        'RunTime
        '
        RunTime.Location = New System.Drawing.Point(400, 32)
        RunTime.Name = "RunTime"
        RunTime.Size = New System.Drawing.Size(100, 20)
        RunTime.TabIndex = 2
        RunTime.Text = "0 s"
        '
        'RunDT
        '
        RunDT.Location = New System.Drawing.Point(144, 32)
        RunDT.Name = "RunDT"
        RunDT.Size = New System.Drawing.Size(100, 20)
        RunDT.TabIndex = 1
        RunDT.Text = "0.1 s"
        '
        'RunTimer
        '
        RunTimer.Interval = 250
        '
        'RunStop
        '
        RunStop.Location = New System.Drawing.Point(364, 520)
        RunStop.Name = "RunStop"
        RunStop.Size = New System.Drawing.Size(100, 23)
        RunStop.TabIndex = 5
        RunStop.Text = "Stop"
        '
        'RunProgress
        '
        RunProgress.Location = New System.Drawing.Point(616, 34)
        RunProgress.Name = "RunProgress"
        RunProgress.Size = New System.Drawing.Size(344, 16)
        RunProgress.Step = 5
        RunProgress.TabIndex = 8
        '
        'Label3
        '
        Label3.AutoSize = True
        Label3.Location = New System.Drawing.Point(536, 34)
        Label3.Name = "Label3"
        Label3.Size = New System.Drawing.Size(51, 13)
        Label3.TabIndex = 9
        Label3.Text = "Progress:"
        '
        'RunUpdate
        '
        RunUpdate.Location = New System.Drawing.Point(516, 520)
        RunUpdate.Name = "RunUpdate"
        RunUpdate.Size = New System.Drawing.Size(100, 23)
        RunUpdate.TabIndex = 6
        RunUpdate.Text = "Update"
        '
        'RunOptions
        '
        RunOptions.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        RunOptions.AutoSize = True
        RunOptions.Location = New System.Drawing.Point(12, 530)
        RunOptions.Name = "RunOptions"
        RunOptions.Size = New System.Drawing.Size(0, 13)
        RunOptions.TabIndex = 10
        '
        'RunJac
        '
        RunJac.Location = New System.Drawing.Point(672, 520)
        RunJac.Name = "RunJac"
        RunJac.Size = New System.Drawing.Size(100, 23)
        RunJac.TabIndex = 11
        RunJac.Text = "Jacobian Off"
        RunJac.Visible = False
        '
        'RunErrors
        '
        RunErrors.Location = New System.Drawing.Point(82, 351)
        RunErrors.Multiline = True
        RunErrors.Name = "RunErrors"
        RunErrors.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        RunErrors.Size = New System.Drawing.Size(821, 149)
        RunErrors.TabIndex = 20
        RunErrors.TabStop = False
        RunErrors.Text = "No Errors"
        '
        'RunModel
        '
        AcceptButton = RunOK
        AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        ClientSize = New System.Drawing.Size(984, 562)
        Controls.Add(RunErrors)
        Controls.Add(RunJac)
        Controls.Add(RunOptions)
        Controls.Add(RunUpdate)
        Controls.Add(Label3)
        Controls.Add(RunProgress)
        Controls.Add(RunStop)
        Controls.Add(RunDT)
        Controls.Add(RunTime)
        Controls.Add(Label2)
        Controls.Add(Label1)
        Controls.Add(RunSummary)
        Controls.Add(RunOK)
        Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        MaximumSize = New System.Drawing.Size(1000, 800)
        Name = "RunModel"
        ShowInTaskbar = False
        StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Text = "RunModel"
        CType(RunSummary, System.ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
        PerformLayout()

    End Sub

#End Region
    Private Sub RunModel_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim CommandString As String, found As Integer
        RunSummary(0, 1) = "Upper Layer" + Chr(10) + "Temperature" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Temperature).Units.Substring(1) + ")"
        RunSummary(0, 2) = "Lower Layer" + Chr(10) + "Temperature" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Temperature).Units.Substring(1) + ")"
        RunSummary(0, 3) = "Interface Height" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Length).Units.Substring(1) + ")"
        RunSummary(0, 4) = "Pyrolysis Rate" + Chr(10) + "(" + myUnits.Convert(UnitsNum.MassLoss).Units.Substring(1) + ")"
        RunSummary(0, 5) = "Fire Size" + Chr(10) + "(" + myUnits.Convert(UnitsNum.HRR).Units.Substring(1) + ")"
        RunSummary(0, 6) = "Pressure" + Chr(10) + "(" + myUnits.Convert(UnitsNum.Pressure).Units.Substring(1) + ")"
        RunSummary.AutoSizeRow(0)
        DebugOn = False
        JacobianOn = False
        RunhasFinished = False
        RunErrors.Enabled = False
        If DebugOutput Then
            RunOK.Location = New Point(212, 520)
            RunStop.Location = New Point(364, 520)
            RunUpdate.Location = New Point(516, 520)
            RunJac.Location = New Point(672, 520)
            RunUpdate.Text = "Debug is Off"
            RunJac.Text = "Jacobian is Off"
            RunJac.Visible = True
            RunJac.Enabled = False
        Else
            RunOK.Location = New Point(290, 520)
            RunStop.Location = New Point(442, 520)
            RunUpdate.Location = New Point(594, 520)
            RunJac.Location = New Point(672, 520)
            RunUpdate.Text = "Update"
            RunJac.Text = "Jacobian is Off"
            RunJac.Visible = False
        End If
        JacobianOn = False
        ' Start the model run and then just look for the status file every so often
        found = CFastInputFile.IndexOf(" ", 0)
        If found <= 0 Then
            CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + CFastInputFile + ".in"
        Else
            CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + """" + CFastInputFile + ".in" + """"
        End If
        RunOptions.Text = "RunOptions: "
        If NetHeatFluxCFASTOutput Then
            CommandString += " -N"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Net Heat Flux Output"
        End If
        If ValidationOutput Then
            CommandString += " -V"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Validation Output"
        End If
        RunOK.Enabled = False
        RunStop.Enabled = True
        RunUpdate.Enabled = True
        ExitCode = 0
        RunProgress.Value = 0
        RunTimer.Enabled = True
        If CommandWindowVisible Then
            ProcessID = Shell(CommandString, AppWinStyle.NormalNoFocus)
        Else
            ProcessID = Shell(CommandString, AppWinStyle.Hide)
        End If

        localById = System.Diagnostics.Process.GetProcessById(ProcessID)
    End Sub
    Private Sub RunTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunTimer.Tick
        RunTimer.Enabled = False
        Dim StatusFileExists As Boolean
        Dim iLine As Integer
        Dim ir As Integer
        Dim Progress As Integer
        Dim aLine As String
        Dim LogFileExists As Boolean
        If localById.HasExited Then
            RunTimer.Enabled = False
            RunOK.Enabled = True
            RunStop.Enabled = False
            RunUpdate.Enabled = False
            RunDT.Text = "CFAST finished"
            If Not RunhasFinished Then
                RunhasFinished = True
                RunErrors.Enabled = True
                myErrors.Queue.Clear()
                FileName = myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".log"
                LogFileExists = System.IO.File.Exists(FileName)
                If LogFileExists Then
                    RunErrors.Text = ""
                    Dim ln As String
                    FileOpen(IO, FileName, OpenMode.Input)
                    Do Until EOF(IO)
                        ln = LineInput(IO)
                        If Not ln.StartsWith("Write to the history") Then myErrors.Add(ln, ErrorMessages.TypeCFastLog)
                    Loop
                    FileClose(IO)
                    If myErrors.Count > 0 Then
                        Dim myEnumerator As System.Collections.IEnumerator = myErrors.Queue.GetEnumerator()
                        While myEnumerator.MoveNext()
                            RunErrors.Text = myEnumerator.Current + ControlChars.CrLf + RunErrors.Text
                        End While
                    End If
                End If
            End If
        End If
        FileName = CFastInputFile + ".status"
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
                            RunProgress.Value = Progress
                            CurrentTime = myUnits.Convert(UnitsNum.Time).FromSI(ln.Substring(16, 10))
                            RunTime.Text = CurrentTime.ToString + myUnits.Convert(UnitsNum.Time).Units
                            If RunOK.Enabled Then
                                RunDT.Text = "CFAST finished"
                            Else
                                RunDT.Text = myUnits.Convert(UnitsNum.Time).FromSI(ln.Substring(32, 10)).ToString + myUnits.Convert(UnitsNum.Time).Units
                            End If
                        End If
                    Case Is > 6
                        If ln.Substring(2, 7) <> "Outside" Then
                            aLine = ln + "           0.0" ' add some blanks at the end to prevent line from being too short
                            RunSummary(ir, 0) = Val(ln.Substring(0, 5))
                            RunSummary(ir, 1) = Math.Round(myUnits.Convert(UnitsNum.Temperature).FromSI(Val(aLine.Substring(12, 8)) + 273.15), 1).ToString
                            RunSummary(ir, 2) = Math.Round(myUnits.Convert(UnitsNum.Temperature).FromSI(Val(aLine.Substring(20, 8)) + 273.15), 1).ToString
                            RunSummary(ir, 3) = Math.Round(myUnits.Convert(UnitsNum.Length).FromSI(Val(aLine.Substring(30, 9))), 2).ToString
                            RunSummary(ir, 4) = Math.Round(myUnits.Convert(UnitsNum.MassLoss).FromSI(Val(aLine.Substring(39, 10))), 5).ToString
                            RunSummary(ir, 5) = Math.Round(myUnits.Convert(UnitsNum.HRR).FromSI(Val(aLine.Substring(49, 10))), 2).ToString
                            RunSummary(ir, 6) = Math.Round(myUnits.Convert(UnitsNum.Pressure).FromSI(Val(aLine.Substring(59, 10))), 4).ToString
                        Else
                            RunSummary(ir, 0) = "Outside"
                            RunSummary(ir, 5) = Math.Round(myUnits.Convert(UnitsNum.HRR).FromSI(Val(ln.Substring(48, 10))), 2).ToString
                        End If
                        ir += 1
                End Select
            Loop
            FileClose(IO)
        End If
        RunTimer.Enabled = True
    End Sub

    Private Sub RunStop_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunStop.Click
        FileName = CFastInputFile + ".stop"
        FileOpen(IO, FileName, OpenMode.Output)
        FileClose(IO)
        RunOK.Enabled = True
        RunStop.Enabled = False
        RunUpdate.Enabled = False
        RunDT.Text = "Stopped"
        RunTimer.Enabled = False
    End Sub

    Private Sub RunOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunOK.Click
        RunTimer.Enabled = False
        If System.IO.File.Exists(FileName + ".debug") Then System.IO.File.Delete(FileName + ".debug")
        If System.IO.File.Exists(FileName + ".jacobian") Then System.IO.File.Delete(FileName + ".jacobian")
        Close()
    End Sub

    Private Sub RunUpdate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunUpdate.Click
        RunTimer.Enabled = False
        FileName = CFastInputFile
        If DebugOutput Then
            If DebugOn Then
                DebugOn = False
                RunUpdate.Text = "Debug is Off"
                RunJac.Text = "Jacobian is Off"
                RunJac.Enabled = False
                If System.IO.File.Exists(FileName + ".debug") Then System.IO.File.Delete(FileName + ".debug")
                If System.IO.File.Exists(FileName + ".jacobian") Then System.IO.File.Delete(FileName + ".jacobian")
            Else
                DebugOn = True
                RunUpdate.Text = "Debug is On"
                RunJac.Enabled = True
                FileOpen(IO, FileName + ".debug", OpenMode.Output)
                FileClose(IO)
            End If
        Else
            FileOpen(IO, FileName + ".query", OpenMode.Output)
            FileClose(IO)
        End If
        RunTimer.Enabled = True
    End Sub

    Private Sub Jacobian_Click(sender As System.Object, e As System.EventArgs) Handles RunJac.Click
        RunTimer.Enabled = False
        If DebugOutput Then
            FileName = CFastInputFile + ".jacobian"
            If JacobianOn Then
                JacobianOn = False
                RunJac.Text = "Jacobian is Off"
                If System.IO.File.Exists(FileName) Then System.IO.File.Delete(FileName)
            Else
                JacobianOn = True
                RunJac.Text = "Jacobian is On"
                FileOpen(IO, FileName, OpenMode.Output)
                FileClose(IO)
            End If
        End If
        RunTimer.Enabled = True
    End Sub
End Class
Public Class RunModel
    Inherits System.Windows.Forms.Form
    'Public CFastInputFile As String
    'Public CFASTSimulationTime As Double
    'Public CommandWindowVisible As Boolean = False
    Private ExitCode As Integer = 0
    Private ProcessID As Integer
    Private localById As Process
    Private FileName As String, IO As Integer = 1, RunhasFinished As Boolean = False
    Friend WithEvents RunOptions As System.Windows.Forms.Label
    Friend WithEvents RunJac As System.Windows.Forms.Button
    Private CurrentTime As Double
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
        Me.RunErrors = New System.Windows.Forms.TextBox()
        CType(Me.RunSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'RunSummary
        '
        Me.RunSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.RunSummary.ColumnInfo = resources.GetString("RunSummary.ColumnInfo")
        Me.RunSummary.ExtendLastCol = True
        Me.RunSummary.Location = New System.Drawing.Point(82, 80)
        Me.RunSummary.Name = "RunSummary"
        Me.RunSummary.Rows.Count = 150
        Me.RunSummary.Rows.DefaultSize = 17
        Me.RunSummary.Size = New System.Drawing.Size(821, 265)
        Me.RunSummary.TabIndex = 3
        Me.RunSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'RunOK
        '
        Me.RunOK.Location = New System.Drawing.Point(212, 520)
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
        Me.RunStop.Location = New System.Drawing.Point(364, 520)
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
        Me.RunUpdate.Location = New System.Drawing.Point(516, 520)
        Me.RunUpdate.Name = "RunUpdate"
        Me.RunUpdate.Size = New System.Drawing.Size(100, 23)
        Me.RunUpdate.TabIndex = 6
        Me.RunUpdate.Text = "Update"
        '
        'RunOptions
        '
        Me.RunOptions.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.RunOptions.AutoSize = True
        Me.RunOptions.Location = New System.Drawing.Point(12, 530)
        Me.RunOptions.Name = "RunOptions"
        Me.RunOptions.Size = New System.Drawing.Size(0, 13)
        Me.RunOptions.TabIndex = 10
        '
        'RunJac
        '
        Me.RunJac.Location = New System.Drawing.Point(672, 520)
        Me.RunJac.Name = "RunJac"
        Me.RunJac.Size = New System.Drawing.Size(100, 23)
        Me.RunJac.TabIndex = 11
        Me.RunJac.Text = "Jacobian Off"
        Me.RunJac.Visible = False
        '
        'RunErrors
        '
        Me.RunErrors.Location = New System.Drawing.Point(82, 351)
        Me.RunErrors.Multiline = True
        Me.RunErrors.Name = "RunErrors"
        Me.RunErrors.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.RunErrors.Size = New System.Drawing.Size(821, 149)
        Me.RunErrors.TabIndex = 20
        Me.RunErrors.TabStop = False
        Me.RunErrors.Text = "No Errors"
        '
        'RunModel
        '
        Me.AcceptButton = Me.RunOK
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(984, 562)
        Me.Controls.Add(Me.RunErrors)
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
        Me.MaximumSize = New System.Drawing.Size(1000, 800)
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
        Dim Arguments As String, found As Integer, Outputs As String
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
            RunUpdate.Text = "Debug is On"
            RunJac.Text = "Jacobian is On"
            RunJac.Visible = True
            DebugOn = True
            RunJac.Enabled = True
            FileName = CFastInputFile
            FileOpen(IO, FileName + ".debug", OpenMode.Output)
            FileClose(IO)
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

        ' Set up the command line and options for the run
        found = CFastInputFilewithExtension.IndexOf(" ", 0)
        If found <= 0 Then
            Arguments = CFastInputFilewithExtension
        Else
            Arguments = """" + CFastInputFilewithExtension + """"
        End If
        RunOptions.Text = "RunOptions: "
        If NetHeatFluxCFASTOutput Then
            Arguments += " -N"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Net Heat Flux Output"
        End If
        If ValidationOutput Then
            Arguments += " -V"
            If RunOptions.Text.Length > 12 Then RunOptions.Text += ", "
            RunOptions.Text += "Validation Output"
        End If
        Outputs = " -O:"
        If SSOutputCompartments Then Outputs += "C"
        If SSOutputDevices Then Outputs += "D"
        If SSOutputMasses Then Outputs += "M"
        If SSOutputVents Then Outputs += "V"
        If SSOutputWalls Then Outputs += "W"
        If Outputs <> " -O:" And Outputs <> " -O:CDMVW" Then Arguments += Outputs

        RunOK.Enabled = False
        RunStop.Enabled = True
        RunUpdate.Enabled = True
        ExitCode = 0
        RunProgress.Value = 0

        ' Start the model run and then just look for the status file every so often
        Try
            localById = New Process
            localById.StartInfo.UseShellExecute = False
            localById.StartInfo.FileName = """" + Application.StartupPath + "\CFAST.exe"" "
            localById.StartInfo.Arguments = Arguments
            If CommandWindowVisible Then
                localById.StartInfo.CreateNoWindow = False
            Else
                localById.StartInfo.CreateNoWindow = True
            End If
            localById.StartInfo.RedirectStandardOutput = True
            localById.StartInfo.RedirectStandardError = True
            localById.Start()
            RunTimer.Enabled = True
        Catch oops As Exception
            MsgBox((oops.Message))
        End Try

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
            Dim exitcode As Integer = localById.ExitCode
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

                    ln = localById.StandardOutput.ReadToEnd
                    If ln <> "" Then
                        If Not ln.StartsWith("Total execution") Then myErrors.Add(ln, ErrorMessages.TypeCFASTError)
                    End If

                    ln = localById.StandardError.ReadToEnd
                    If ln <> "" Then myErrors.Add(ln, ErrorMessages.TypeCFASTError)

                    If localById.ExitCode <> 0 Then myErrors.Add("CFAST exited with a non-zero exit code: " + localById.ExitCode.ToString, ErrorMessages.TypeCFASTError)

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
        FileName = CFastInputFile
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
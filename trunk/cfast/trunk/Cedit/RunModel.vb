Public Class RunModel
    Inherits System.Windows.Forms.Form
    'Public CFastInputFile As String
    'Public CFASTSimulationTime As Single
    'Public CommandWindowVisible As Boolean = False
    'Public ExitCode As Integer = 0
    Private ProcessID As Integer
    Private localById As Process
    Private FileName As String, IO As Integer = 1
    Private CurrentTime As Single

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
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(RunModel))
        Me.RunSummary = New C1.Win.C1FlexGrid.C1FlexGrid
        Me.RunOK = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.RunTime = New System.Windows.Forms.TextBox
        Me.RunDT = New System.Windows.Forms.TextBox
        Me.RunTimer = New System.Windows.Forms.Timer(Me.components)
        Me.RunStop = New System.Windows.Forms.Button
        Me.RunProgress = New System.Windows.Forms.ProgressBar
        Me.Label3 = New System.Windows.Forms.Label
        Me.RunUpdate = New System.Windows.Forms.Button
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
        Me.RunOK.Location = New System.Drawing.Point(311, 464)
        Me.RunOK.Name = "RunOK"
        Me.RunOK.Size = New System.Drawing.Size(75, 23)
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
        Me.RunStop.Location = New System.Drawing.Point(458, 464)
        Me.RunStop.Name = "RunStop"
        Me.RunStop.Size = New System.Drawing.Size(75, 23)
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
        Me.RunUpdate.Location = New System.Drawing.Point(607, 464)
        Me.RunUpdate.Name = "RunUpdate"
        Me.RunUpdate.Size = New System.Drawing.Size(75, 23)
        Me.RunUpdate.TabIndex = 6
        Me.RunUpdate.Text = "Update"
        '
        'RunModel
        '
        Me.AcceptButton = Me.RunOK
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(992, 534)
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
        ' Start the model run and then just look for the status file every so often
        found = CFastInputFile.IndexOf(" ", 0)
        If found = 0 Then
            CommandString = Application.StartupPath + "\CFAST.exe " + System.IO.Path.GetFileNameWithoutExtension(CFastInputFile)
        Else
            CommandString = Application.StartupPath + "\CFAST.exe " + """" + System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + """"
        End If
        If TotalMassCFASTOutput Then CommandString += " /T"
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
                            Me.RunSummary(ir, 3) = Math.Round(myUnits.Convert(UnitsNum.Length).FromSI(Val(ln.Substring(30, 8))), 2).ToString
                            Me.RunSummary(ir, 4) = Math.Round(myUnits.Convert(UnitsNum.MassLoss).FromSI(Val(ln.Substring(38, 10))), 5).ToString
                            Me.RunSummary(ir, 5) = Math.Round(myUnits.Convert(UnitsNum.HRR).FromSI(Val(ln.Substring(48, 10))), 2).ToString
                            Me.RunSummary(ir, 6) = Math.Round(myUnits.Convert(UnitsNum.Pressure).FromSI(Val(ln.Substring(58, 10))), 4).ToString
                            Me.RunSummary(ir, 7) = Math.Round(myUnits.Convert(UnitsNum.HeatFlux).FromSI(Val(ln.Substring(68, 10))), 3).ToString
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
        Me.Close()
    End Sub

    Private Sub RunUpdate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunUpdate.Click
        Me.RunTimer.Enabled = False
        FileName = System.IO.Path.GetFileNameWithoutExtension(CFastInputFile) + ".query"
        FileOpen(IO, FileName, OpenMode.Output)
        FileClose(IO)
        Me.RunTimer.Enabled = True
    End Sub
End Class
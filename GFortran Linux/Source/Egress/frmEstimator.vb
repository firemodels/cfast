Imports System.IO
Public Class frmEstimator

    Dim FileName As String
    Dim FileBase As String
    Dim FileExtension As String
    Dim BuildDataFile As String
    Dim fileIdx As Integer
    Dim Batchrun As Integer
    Dim NumBatchruns As Integer
    Dim OriginalFileName As String
    Dim estimatingFlg, CompleteFlg As Boolean
    Dim estimator As EgressCalculation
    Dim dt As Double = 0.2
    Dim cycles As Integer = 5
    Dim numStairs As Integer
    ' Set DoDebug = True to show btnTestCase and btnDebug buttons
    Dim DoDebug As Boolean = False
    'Debug is for debug printout its value is changed in btnDebug_Click
    'Debug = True means debug information is printed in the output file
    'Debug = False is normal printout
    'Set Debug intially in Form1_Load in the if DoDebug block of code
    'Currently intialized to false
    'NEVER CHANGE If Not DoDebug then Debug = False part of Form1_Load
    Dim Debug As Boolean
    Dim icnt As Integer
    Dim pdecnt As Integer = 0
    Dim s, tmp As String
    Dim ElevatorCapacitiesinPeople() As Integer = {12, 16, 19, 21, 24, 0}
    ' Variables for values in fields
    Dim ee As EgressEstimatorInput
    'Dim aNumFloors, aNumOccupants, aNumStairs As Integer
    'Dim aElevatorFrac As Double
    'Dim aHallLength, aHallWidth, aHallEntryRate As Double
    'Dim aFirstFloorUseStairwells As Boolean
    'Dim aMergingFlow, aFlightsPerFloor, aStairsPerFlight As Integer
    'Dim aStairWidth, aStairRiserHeight, aStairTreadDepth, aStairEntryRate As Double
    'Dim aStairHasHandrails As Boolean
    'Dim aExitHallLength, aExitHallWidth, aExitHallEntryRate, aExitHallExitRate As Double
    'Dim aElExitHallLength, aElExitHallWidth, aElExitHallExitRate As Double
    'Dim aEndEstimate As Integer
    'Dim aNumElevators, aMaxElevatorCarCap As Integer
    'Dim aElevatorVel, aElevatorAcc, aElevatorRecallDelay As Double
    'Dim aElevatorLoadRate, aElevatorUnloadRate As Double
    'Dim aElevatorDoorType As Integer
    'Dim aBuildingfile As Boolean
    Dim IOParameters As Integer = 27
    'lbl is defined in SetDefaultValues
    Dim lbl(IOParameters) As String
    Dim ElevatorCapFactor As Double = 0.01333

    Private Sub GreyOut(ByVal grey As Boolean)
        Dim flag As Boolean = Not grey
        NumFloors.Enabled = flag
        NumOccupants.Enabled = flag
        Me.NumElevators.Enabled = flag
        Me.ElevatorFraction.Enabled = flag
        Me.ElevatorAcceleration.Enabled = flag
        Me.ElevatorCapacity.Enabled = flag
        Me.ElevatorStartup.Enabled = flag
        Me.ElevatorVelocity.Enabled = flag
        Me.NumStairwells.Enabled = flag
        Me.DoorType.Enabled = flag
        HallLength.Enabled = flag
        HallWidth.Enabled = flag
        HallEntryRate.Enabled = flag
        FirstFloorUseStairwells.Enabled = flag
        StairWidth.Enabled = flag
        FlightsPerFloor.Enabled = flag
        StairsPerFlight.Enabled = flag
        StairRiserHeight.Enabled = flag
        StairTreadDepth.Enabled = flag
        StairEntryRate.Enabled = flag
        StairHallLength.Enabled = flag
        StairHallWidth.Enabled = flag
        ExitHallEntryRate.Enabled = flag
        StairHallExitRate.Enabled = flag
        Me.EndEstimate.Enabled = flag
        Me.EndEstimateTime.Enabled = flag
    End Sub
    Private Sub ResetEstimator()
        If (Not CompleteFlg) And estimatingFlg Then
            estimatingFlg = False
            Estimate.Text = "Complete"
        ElseIf (Not estimatingFlg) And CompleteFlg Then
            Estimate.Text = "Estimate"
            timerEstimate.Enabled = False
            icnt = 0
            Progress.Hide()
            EgressTime.Text = ""
            EgressTime.Hide()
            BatchRunCounter.Hide()
            NumberEvacuated.Text = ""
            NumberEvacuated.Hide()
            GreyOut(False)
        End If
    End Sub
    Private Sub Set_btnDebugText()
        If DoDebug Then
            If Debug Then
                btnDebug.Text = "Turn Debug Off"
            Else
                btnDebug.Text = "Turn Debug On"
            End If
        End If
    End Sub
    Private Sub RunEstimator()
        Progress.Maximum = estimator.MaxCriteriaValue
        Progress.Value = estimator.CurrentCalcValue
        Progress.Show()
        timerEstimate.Interval = 5
        timerEstimate.Enabled = True
        estimatingFlg = True
        CompleteFlg = False
        Estimate.Text = "Stop"
        EgressTime.Show()
        NumberEvacuated.Show()
        GreyOut(True)

    End Sub

    Private Sub ReadFields()

        'Sub for taking data from the fields on the interface

        ee.aNumFloors = Val(NumFloors.Text)
        ee.aNumOccupants = Val(NumOccupants.Text)
        ee.aElevatorFrac = Val(Me.ElevatorFraction.Text)
        ee.aHallLength = Val(HallLength.Text)
        ee.aHallWidth = Val(HallWidth.Text)
        ee.aHallEntryRate = Val(HallEntryRate.Text)
        ee.aFirstFloorUseStairwells = FirstFloorUseStairwells.Checked
        ee.aBuildingFile = BuildingFile.Checked

        ' Properties of the stairwells

        ee.aNumStairs = Val(Me.NumStairwells.Text)
        ee.aStairWidth = Val(StairWidth.Text)
        ee.aFlightsPerFloor = Val(FlightsPerFloor.Text)
        ee.aStairsPerFlight = Val(StairsPerFlight.Text)
        ee.aStairRiserHeight = Val(StairRiserHeight.Text) / 1000.0
        ee.aStairTreadDepth = Val(StairTreadDepth.Text) / 1000.0
        ee.aStairEntryRate = Val(StairEntryRate.Text)

        'Properties of the exit hallways

        ee.aExitHallLength = Val(StairHallLength.Text)
        ee.aExitHallWidth = Val(StairHallWidth.Text)
        ee.aExitHallEntryRate = Val(ExitHallEntryRate.Text)
        ee.aExitHallExitRate = Val(StairHallExitRate.Text)

        'When to end estimate

        If EndEstimate.SelectedIndex = 1 Then
            ee.aEndEstimate = Val(EndEstimateTime.Text)
        Else
            ee.aEndEstimate = -0
        End If

        ee.aNumElevators = Val(Me.NumElevators.Text)
        If Me.ElevatorCapacity.SelectedIndex >= 0 And Me.ElevatorCapacity.SelectedIndex <= 4 Then
            ee.aMaxElevatorCarCap = ElevatorCapacitiesinPeople(Me.ElevatorCapacity.SelectedIndex)
        Else
            ee.aMaxElevatorCarCap = Math.Round(Val(ElevatorCustomCapacity.Text) * Me.ElevatorCapFactor)
        End If
        ee.aElevatorVel = Val(Me.ElevatorVelocity.Text)
        ee.aElevatorAcc = Val(Me.ElevatorAcceleration.Text)
        ee.aElevatorRecallDelay = Val(Me.ElevatorStartup.Text)
        ee.aElevatorDoorType = Me.DoorType.SelectedIndex
        ee.aElevatorLoadRate = EgressElevator.defaultLoadRate
        ee.aElevatorUnloadRate = EgressElevator.defaultUnloadRate

    End Sub

    Private Function LoadFields(ByRef errMsg As String) As Boolean
        ' Tbis function currently assumes that all the values have been validated

        errMsg = ""

        Me.NumFloors.Text = ee.aNumFloors.ToString
        Me.NumOccupants.Text = ee.aNumOccupants.ToString
        Me.ElevatorFraction.Text = ee.aElevatorFrac.ToString
        Me.HallLength.Text = ee.aHallLength.ToString
        Me.HallWidth.Text = ee.aHallWidth.ToString
        Me.HallEntryRate.Text = ee.aHallEntryRate.ToString
        Me.FirstFloorUseStairwells.Checked = ee.aFirstFloorUseStairwells
        Me.BuildingFile.Checked = ee.aBuildingFile
        If ee.aBuildingFile Then
            Me.SetBuildFileText()
        End If

        ' Properties of the stairwells

        Me.NumStairwells.Text = ee.aNumStairs.ToString
        Me.StairWidth.Text = ee.aStairWidth.ToString
        Me.FlightsPerFloor.Text = ee.aFlightsPerFloor.ToString
        Me.StairsPerFlight.Text = ee.aStairsPerFlight.ToString
        Me.StairRiserHeight.Text = (ee.aStairRiserHeight * 1000.0).ToString
        Me.StairTreadDepth.Text = (ee.aStairTreadDepth * 1000.0).ToString
        Me.StairEntryRate.Text = ee.aStairEntryRate.ToString

        'Properties of the exit hallways

        Me.StairHallLength.Text = ee.aExitHallLength.ToString
        Me.StairHallWidth.Text = ee.aExitHallWidth.ToString
        Me.ExitHallEntryRate.Text = ee.aExitHallEntryRate.ToString
        Me.StairHallExitRate.Text = ee.aExitHallExitRate.ToString

        'When to end estimate

        If ee.aEndEstimate > 0 Then
            Me.EndEstimate.SelectedIndex = 1
        Else
            Me.EndEstimate.SelectedIndex = 0
        End If

        Me.NumElevators.Text = ee.aNumElevators.ToString
        Me.ElevatorCapacity.SelectedIndex = IndexFromElevatorCapacity(ee.aMaxElevatorCarCap)
        If Me.ElevatorCapacity.SelectedIndex = 5 Then
            Me.ElevatorCustomCapacity.Text = Math.Round(ee.aMaxElevatorCarCap / Me.ElevatorCapFactor).ToString
        End If

        Me.ElevatorVelocity.Text = ee.aElevatorVel.ToString
        Me.ElevatorAcceleration.Text = ee.aElevatorAcc.ToString
        Me.ElevatorStartup.Text = ee.aElevatorRecallDelay.ToString
        Me.DoorType.SelectedIndex = ee.aElevatorDoorType

        Return True
    End Function
    Private Sub SetBuildFileText()
        Dim dFile As New System.IO.FileInfo(ee.buildingFile)
        BuildingFile.Text = "Data file: " + dFile.Name
        Me.Text = "Egress Estimator: " + dFile.DirectoryName
    End Sub

    Private Function IndexFromElevatorCapacity(ByVal cap As Integer) As Integer
        For i As Integer = 0 To 4
            If ElevatorCapacitiesinPeople(i) = cap Then
                Return i
            End If
        Next
        Return 5
    End Function

    Private Function SaveInput(ByVal FileName As String, ByVal ReadFieldsFlg As Boolean, ByRef err As String) As Boolean

        If ReadFieldsFlg Then
            ReadFields()
        End If
        Return ee.WriteInput(FileName, err)

    End Function

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        EndEstimate.SelectedIndex = 0
        DoorType.SelectedIndex = 7
        FileBase = "EgressEstimate"
        fileIdx = 1
        FileExtension = ".csv"
        ee = New EgressEstimatorInput(1)
        Dim path As String = My.Computer.FileSystem.CurrentDirectory
        Me.SaveFileDialog.InitialDirectory = path
        CompleteFlg = True
        ResetEstimator()
        SetDefaultValues()
        'set DoDebug = True in general declariation to show btnDebug and btnTestCase
        If DoDebug Then
            'Debug = False assumes normal printout unless told otherwise
            'Debug = True assumes debug printout unless told otherwise
            Debug = True
            Set_btnDebugText()
            btnDebug.Show()
            btnDebug.Enabled = True
        Else
            'NEVER CHANGE following line Debug = False
            Debug = False
            'NEVER CHANGE preceeding line Debug = False
            btnDebug.Hide()
            btnDebug.Enabled = False
        End If
        Batchmode.Hide()
        GreyOut(False)
    End Sub
    Private Sub Help_Clicked(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.HelpButtonClicked
        Help.ShowHelp(Me, Application.StartupPath + "\" + "EgressEstimator.chm")
    End Sub

    Private Sub Estimate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Estimate.Click
        Dim errMsg As String = ""

        ' Properties of the building and it's occupants

        If Me.Estimate.Text = "OK" Then
            Me.Estimate.Text = "Estimate"
            CompleteFlg = True
            estimatingFlg = False
            ResetEstimator()
        ElseIf (Not estimatingFlg) And CompleteFlg Then

            'Sub puts information from interface into global variables
            ReadFields()

            'Ask user for filename to save output
            Dim path As String = Me.SaveFileDialog.InitialDirectory
            My.Computer.FileSystem.CurrentDirectory = path
            Dim file As String = FileBase + fileIdx.ToString("00") + FileExtension
            Do Until Not My.Computer.FileSystem.FileExists(file)
                fileIdx = fileIdx + 1
                file = FileBase + fileIdx.ToString("00") + FileExtension
            Loop
            SaveFileDialog.FileName = file
            SaveFileDialog.OverwritePrompt = True
            If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                FileName = SaveFileDialog.FileName
                If ee.aBuildingFile = True Then

                    Dim PopulationPF(ee.aNumFloors) As Integer
                    Dim LobbyFractionPF(ee.aNumFloors) As Double
                    Dim LobbyDelay(ee.aNumFloors) As Double
                    Dim StairDelay(ee.aNumFloors) As Double
                    If Not ee.ReadBuildFile(ee.buildingFile, PopulationPF, LobbyFractionPF, StairDelay, LobbyDelay, errMsg) Then
                        MsgBox("Error with Building file: " + errMsg)
                        Return
                    End If

                    estimator = New EgressCalculation(ee.aNumFloors, PopulationPF, LobbyFractionPF, ee.aNumStairs, _
                                    EgressElement.MergeType.Interleve, ee.aFlightsPerFloor, ee.aStairRiserHeight, ee.aStairTreadDepth, _
                                    ee.aStairsPerFlight, ee.aStairWidth, EgressElement.Bndry.StairWalls, ee.aStairWidth, _
                                    1.5 * ee.aStairWidth, EgressElement.Bndry.StairHandrails, ee.aStairEntryRate, _
                                    ee.aHallLength, ee.aHallWidth, EgressElement.Bndry.Corridor, ee.aHallEntryRate, _
                                    ee.aExitHallLength, ee.aExitHallWidth, EgressElement.Bndry.Corridor, _
                                    ee.aExitHallEntryRate, ee.aExitHallExitRate, ee.aFirstFloorUseStairwells, _
                                    ee.aNumElevators, ee.aElevatorDoorType, ee.aElevatorVel, ee.aNumElevators, _
                                    ee.aMaxElevatorCarCap, ee.aElevatorAcc, 1.0, 1.0, _
                                    EgressElement.Bndry.Corridor, 1.0, ee.aElevatorRecallDelay, StairDelay, LobbyDelay)
                Else

                    estimator = New EgressCalculation(ee.aNumFloors, ee.aNumOccupants, ee.aElevatorFrac, ee.aNumStairs, _
                                    EgressElement.MergeType.Interleve, ee.aFlightsPerFloor, ee.aStairRiserHeight, ee.aStairTreadDepth, _
                                    ee.aStairsPerFlight, ee.aStairWidth, EgressElement.Bndry.StairWalls, ee.aStairWidth, _
                                    1.5 * ee.aStairWidth, EgressElement.Bndry.StairHandrails, ee.aStairEntryRate, _
                                    ee.aHallLength, ee.aHallWidth, EgressElement.Bndry.Corridor, ee.aHallEntryRate, _
                                    ee.aExitHallLength, ee.aExitHallWidth, EgressElement.Bndry.Corridor, _
                                    ee.aExitHallEntryRate, ee.aExitHallExitRate, ee.aFirstFloorUseStairwells, _
                                    ee.aNumElevators, ee.aElevatorDoorType, ee.aElevatorVel, ee.aNumElevators, _
                                    ee.aMaxElevatorCarCap, ee.aElevatorAcc, 1.0, 1.0, _
                                    EgressElement.Bndry.Corridor, 1.0, ee.aElevatorRecallDelay)
                End If

                If Not estimator.IsValid Then
                    MsgBox("Error not a validly defined simulation")
                    Return
                End If

                If ee.aEndEstimate > 0 Then
                    If Not estimator.InitializeCalc(ee.aEndEstimate, 1) Then
                        MsgBox("Error in Initializing on time criteria t = " + ee.aEndEstimate.ToString)
                        Return
                    End If
                Else
                    If Not estimator.InitializeCalc(1) Then
                        MsgBox("Error in Initializing for population criteria")
                        Return
                    End If
                End If

                If Not SaveInput(FileName, False, errMsg) Then
                    MsgBox("Error in SaveInput in Estimate_Click" + errMsg)
                    Return
                End If

                If Not Debug Then
                    s = "Time(s)," + estimator.ElementTotPopNamesCSV + EgressCalculation.EndLine
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    s = estimator.DoElementTotPopsCSV + EgressCalculation.EndLine()
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)
                Else
                    estimator.DoDebug = Debug
                    s = "Time(s)," + estimator.PDEElementTotPopNamesCSV + "Total Egress, Total In," + EgressCalculation.EndLine
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    's = "," + estimator.EgressElementNameListCSV + estimator.EndLine
                    'My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    's = estimator.EquationNameListCSV + estimator.EndLine
                    'My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    's = estimator.DoOutputCSV + estimator.EndLine
                    'My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    'If True Then
                    '    Dim Extension() As Char = {".", "c", "v", "s"}
                    '    Dim dbFileName As String = FileName.TrimEnd(Extension) + "DB" + FileExtension
                    '    Dim h() As String = {estimator.EgressElementNameListCSV, _
                    '                            estimator.EquationNameListCSV}
                    '    estimator.SetDiagnosticPrint(dbFileName, h, 2, estimator.EndLine, -224.0)
                    'End If
                End If
                RunEstimator()
            End If
        Else
            CompleteFlg = True
            estimatingFlg = False
            ResetEstimator()
        End If
    End Sub

    Private Sub timerEstimate_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles timerEstimate.Tick

        Dim s As String

        If estimatingFlg Then
            If Not estimator.StepForwardPDE(dt) Then
                s = "Error in taking step foward at time = " + estimator.CurrentTime.ToString
                s = s + EgressCalculation.EndLine + estimator.ErrMsg()
                estimatingFlg = False
                MsgBox(s)
            End If
            pdecnt = pdecnt + 1
            icnt = icnt + 1
            Math.DivRem(icnt, cycles, icnt)
            If icnt = 0 Then
                Dim tot, otot, itot, time As Double
                tot = estimator.TotalPDEEgress
                itot = estimator.TotalPDEIn
                otot = estimator.TotalPDEOut + estimator.PDEElevatorOut()
                time = estimator.CurrentTime
                's = time.ToString + "," + estimator.PDEElementTotPopsCSV + otot.ToString + "," + tot.ToString + "," + itot.ToString + "," + estimator.EndLine
                s = time.ToString + "," + estimator.ElementTotPopsCSV + EgressCalculation.EndLine '+ otot.ToString + "," + tot.ToString + "," + itot.ToString + "," + estimator.EndLine
                My.Computer.FileSystem.WriteAllText(FileName, s, True)
                EgressTime.Text = "Total Egress Time(s) = " + time.ToString("    .")
                NumberEvacuated.Text = "Evacuated (persons) = " + Math.Max(0.0, otot).ToString("    .")
                Progress.Value = estimator.CurrentCalcValue
            End If

            If estimator.IsCalcComplete Then


                Dim tot, otot, itot As Double
                tot = estimator.TotalPDEEgress
                itot = estimator.TotalPDEIn
                otot = estimator.TotalPDEOut + estimator.PDEElevatorOut

                Dim maxout As Double = estimator.CurrentEvacPop
                estimatingFlg = False
                CompleteFlg = True
                Me.Estimate.Text = "OK"
                Progress.Value = Progress.Maximum
                'MsgBox("Time " + estimator.CurrentTime.ToString("          .")) ' + " s" + "Total in system = " + tot.ToString("    .000") + " Total out = " + otot.ToString("    .000") + " Total in = " + itot.ToString("    .000"))
                'ResetEstimator()
                pdecnt = 0

                If Batchrun < NumBatchruns Then
                    Batchrun = Batchrun + 1
                    DoBatchRun(Batchrun)
                End If


            End If
            'If Not estimator.StepForward Then
            '    s = "Error in taking step foward at time = " + estimator.CurrentTime.ToString
            '    s = s + estimator.EndLine + estimator.ErrMsg()
            '    estimatingFlg = False
            '    MsgBox(s)
            'End If
            'icnt = icnt + 1
            'Math.DivRem(icnt, 20, icnt)
            'If icnt = 0 Then
            '    EgressTime.Text = "Time(s) = " + estimator.CurrentTime.ToString("    .000")
            '    NumberEvacuated.Text = "Evacuated (persons) = " + estimator.CurrentEvacPop.ToString
            'End If
            'If estimator.IsCalcComplete Then
            '    If estimatingFlg Then
            '        EgressTime.Text = "Time(s) = " + estimator.CurrentTime.ToString("    .000")
            '        NumberEvacuated.Text = "Evacuated (persons) = " + estimator.CurrentEvacPop.ToString
            '        ResetEstimator()
            '    End If
            'End If
            'Progress.Value = estimator.CurrentCalcValue
            'If estimator.DoOutput Then
            '    If Not Debug Then
            '        s = estimator.DoElementTotPopsCSV + estimator.EndLine
            '    Else
            '        s = estimator.DoOutputCSV + estimator.EndLine
            '    End If
            '    My.Computer.FileSystem.WriteAllText(FileName, s, True)
            'End If
            'If Not estimatingFlg Then
            '    ResetEstimator()
            'End If
        End If

    End Sub

    Private Sub SetDefaultValues()

        Me.NumFloors.Text = "15"
        Me.NumOccupants.Text = "24"
        Me.ElevatorFraction.Text = "0.5"
        Me.HallLength.Text = "41.1"
        Me.HallWidth.Text = "2.3"
        Me.HallEntryRate.Text = "1.0"
        Me.EndEstimateTime.Text = "0.0"

        ' Properties of the stairwells

        Me.NumStairwells.Text = "2"
        Me.StairWidth.Text = "1.27"
        Me.FlightsPerFloor.Text = "2"
        Me.StairsPerFlight.Text = "9"
        Me.StairRiserHeight.Text = "178"
        Me.StairTreadDepth.Text = "279"
        Me.StairEntryRate.Text = "1.0"

        'Properties of the exit hallways

        Me.StairHallLength.Text = "2.0"
        Me.StairHallWidth.Text = "2.0"
        Me.ExitHallEntryRate.Text = "1.46965"
        Me.StairHallExitRate.Text = "1.25"

        'Properties of Elevators

        Me.NumElevators.Text = "2"
        Me.ElevatorCapacity.SelectedIndex = 0
        Me.ElevatorCustomCapacity.Text = "900"
        Me.ElevatorCustomCapacity.Visible = False
        Me.ElevatorVelocity.Text = "3.0"
        Me.ElevatorAcceleration.Text = "3.0"
        Me.ElevatorStartup.Text = "42.5"

        Me.BuildingFile.Checked = False

        Me.ReadFields()

    End Sub

    Private Sub btnDebug_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDebug.Click
        Debug = Not Debug
        Set_btnDebugText()

    End Sub

    Public Sub New()

        ' This call is required by the Windows Form Designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Private Sub ElevatorCapacity_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ElevatorCapacity.SelectedIndexChanged
        If Me.ElevatorCapacity.SelectedIndex < 0 Then
            Me.ElevatorCapacity.SelectedIndex = 0
        ElseIf Me.ElevatorCapacity.SelectedIndex = 5 Then
            Me.ElevatorCustomCapacity.Visible = True
        Else
            Me.ElevatorCustomCapacity.Visible = False
        End If
    End Sub

    Private Sub EndEstimate_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EndEstimate.SelectedIndexChanged
        If Me.EndEstimate.SelectedIndex = 1 Then
            Me.EndEstimateTime.Visible = True
        Else
            Me.EndEstimateTime.Visible = False
        End If
    End Sub

    Private Sub Batchmode_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Batchmode.Click


        Dim aNumFloors, aNumOccupants, aNumStairs As Integer
        Dim aElevatorFrac As Double
        Dim aHallLength, aHallWidth, aHallEntryRate As Double
        Dim aFirstFloorUseStairwells As Boolean
        Dim aMergingFlow, aFlightsPerFloor, aStairsPerFlight As Integer
        Dim aStairWidth, aStairRiserHeight, aStairTreadDepth, aStairEntryRate As Double
        Dim aStairHasHandrails As Boolean
        Dim aExitHallLength, aExitHallWidth, aExitHallEntryRate, aExitHallExitRate As Double
        Dim aElExitHallLength, aElExitHallWidth, aElExitHallExitRate As Double
        Dim aEndEstimate As Integer
        Dim aNumElevators, aMaxElevatorCarCap As Integer
        Dim aElevatorVel, aElevatorAcc, aElevatorRecallDelay As Double
        Dim aElevatorLoadRate, aElevatorUnloadRate As Double
        Dim aElevatorDoorType As Integer
        Dim aBuildingfile As Boolean

        Return
        ' Properties of the building and it's occupants

        If Me.Estimate.Text = "OK" Then
            Me.Estimate.Text = "Estimate"
            CompleteFlg = True
            estimatingFlg = False
            ResetEstimator()
        ElseIf (Not estimatingFlg) And CompleteFlg Then

            '''''''''''''''''''''''''''''''''''''''''''''''
            '  READING THE BATCHLIST FILE AND CREATING ARRAY FOR BATCH PARAMETERS
            '''''''''''''''''''''''''''''''''''''''''''''''

            Dim strfilename As String
            Dim num_rows As Long
            Dim num_cols As Long
            Dim x As Integer
            Dim y As Integer
            Dim strarray(1, 1) As String

            strfilename = "batchlist.csv"

            Dim tmpstream As StreamReader = System.IO.File.OpenText(strfilename)
            Dim strlines() As String
            Dim strline() As String

            strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)

            num_rows = UBound(strlines)
            strline = strlines(0).Split(",")
            num_cols = UBound(strline)
            ReDim strarray(num_rows, num_cols)

            For x = 0 To num_rows
                strline = strlines(x).Split(",")

                For y = 0 To num_cols
                    strarray(x, y) = strline(y)
                Next
            Next

            tmpstream.Close()

            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

            NumBatchruns = num_cols

            aElevatorLoadRate = EgressElevator.defaultLoadRate
            aElevatorUnloadRate = EgressElevator.defaultUnloadRate

            'Ask user for filename to save output
            FileBase = "EgressBatchRun"
            Dim path As String = Me.SaveFileDialog.InitialDirectory

            My.Computer.FileSystem.CurrentDirectory = path
            Dim file As String = FileBase + fileIdx.ToString("00") + FileExtension
            Do Until Not My.Computer.FileSystem.FileExists(file)
                fileIdx = fileIdx + 1
                file = FileBase + fileIdx.ToString("00") + FileExtension
            Loop
            SaveFileDialog.FileName = file
            SaveFileDialog.OverwritePrompt = True

            If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then

                Batchrun = 1
                'fileIdx = fileIdx + 1
                'file = FileBase + Batchrun.ToString("00") + FileExtension
                FileName = SaveFileDialog.FileName
                OriginalFileName = FileName
                Dim DotCSVLocation As Integer
                DotCSVLocation = OriginalFileName.IndexOf(".csv")

                'fileIdx = 0

                Dim LittleString As String
                Dim Score As String

                Score = String.Concat(Batchrun.ToString, "/", NumBatchruns.ToString)
                BatchRunCounter.Text = String.Concat("Run: ", Score)
                BatchRunCounter.Show()
                LittleString = String.Concat("-", Batchrun.ToString("00"))
                FileName = OriginalFileName.Insert(DotCSVLocation, LittleString)


                aNumFloors = Val(strarray(0, Batchrun))
                aNumOccupants = Val(strarray(1, Batchrun))
                aElevatorFrac = Val(strarray(2, Batchrun))
                aHallLength = Val(strarray(4, Batchrun))
                aHallWidth = Val(strarray(5, Batchrun))
                aHallEntryRate = Val(strarray(6, Batchrun))
                aFirstFloorUseStairwells = Val(strarray(8, Batchrun))
                aBuildingfile = Val(strarray(3, Batchrun))
                aNumStairs = Val(strarray(7, Batchrun))
                aStairWidth = Val(strarray(9, Batchrun))
                aFlightsPerFloor = Val(strarray(10, Batchrun))
                aStairsPerFlight = Val(strarray(11, Batchrun))
                aStairRiserHeight = Val(strarray(12, Batchrun)) / 1000
                aStairTreadDepth = Val(strarray(13, Batchrun)) / 1000
                aStairEntryRate = Val(strarray(14, Batchrun))
                aExitHallLength = Val(strarray(16, Batchrun))
                aExitHallWidth = Val(strarray(17, Batchrun))
                aExitHallEntryRate = Val(strarray(15, Batchrun))
                aExitHallExitRate = Val(strarray(18, Batchrun))
                aNumElevators = Val(strarray(19, Batchrun))
                aMaxElevatorCarCap = Val(strarray(21, Batchrun))
                aElevatorVel = Val(strarray(22, Batchrun))
                aElevatorAcc = Val(strarray(23, Batchrun))
                aElevatorRecallDelay = Val(strarray(24, Batchrun))
                aElevatorDoorType = Val(strarray(21, Batchrun))

                estimator = New EgressCalculation(aNumFloors, aNumOccupants, aElevatorFrac, aNumStairs, _
                                aMergingFlow, aFlightsPerFloor, aStairRiserHeight, aStairTreadDepth, _
                                aStairsPerFlight, aStairWidth, aStairHasHandrails, aStairWidth, _
                                1.5 * aStairWidth, EgressElement.Bndry.StairHandrails, aStairEntryRate, _
                                aHallLength, aHallWidth, EgressElement.Bndry.Corridor, aHallEntryRate, _
                                aExitHallLength, aExitHallWidth, EgressElement.Bndry.Corridor, _
                                aExitHallEntryRate, aExitHallExitRate, aFirstFloorUseStairwells, _
                                aNumElevators, aElevatorDoorType, aElevatorVel, aNumElevators, _
                                aMaxElevatorCarCap, aElevatorAcc, aElExitHallLength, aElExitHallWidth, _
                                EgressElement.Bndry.Corridor, aElExitHallExitRate, aElevatorRecallDelay)

                If Not estimator.IsValid Then
                    MsgBox("Error not a validly defined simulation")
                    Return
                End If

                If aEndEstimate > 0 Then
                    If Not estimator.InitializeCalc(aEndEstimate, 1) Then
                        MsgBox("Error in Initializing on time criteria t = " + aEndEstimate.ToString)
                        Return
                    End If
                Else
                    If Not estimator.InitializeCalc(1) Then
                        MsgBox("Error in Initializing for population criteria")
                        Return
                    End If
                End If

                If Not Debug Then
                    s = "Time(s)," + estimator.ElementTotPopNamesCSV + estimator.EndLine
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)
                    s = estimator.DoElementTotPopsCSV + estimator.EndLine
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)
                Else
                    estimator.DoDebug = Debug
                    s = "Time(s)," + estimator.PDEElementTotPopNamesCSV + "Total Egress, Total In," + estimator.EndLine
                    My.Computer.FileSystem.WriteAllText(FileName, s, True)

                End If

                CompleteFlg = True
                estimatingFlg = False



                'BatchRunCounter.Hide()
                RunEstimator()
            End If
        Else
            'CompleteFlg = True
            'estimatingFlg = False
            ResetEstimator()
        End If



    End Sub

    Private Sub DoBatchRun(ByVal Batchrun As Integer) '(ByVal sender As System.Object, ByVal e As System.EventArgs)


        Dim aNumFloors, aNumOccupants, aNumStairs As Integer
        Dim aElevatorFrac As Double
        Dim aHallLength, aHallWidth, aHallEntryRate As Double
        Dim aFirstFloorUseStairwells As Boolean
        Dim aMergingFlow, aFlightsPerFloor, aStairsPerFlight As Integer
        Dim aStairWidth, aStairRiserHeight, aStairTreadDepth, aStairEntryRate As Double
        Dim aStairHasHandrails As Boolean
        Dim aExitHallLength, aExitHallWidth, aExitHallEntryRate, aExitHallExitRate As Double
        Dim aElExitHallLength, aElExitHallWidth, aElExitHallExitRate As Double
        Dim aEndEstimate As Integer
        Dim aNumElevators, aMaxElevatorCarCap As Integer
        Dim aElevatorVel, aElevatorAcc, aElevatorRecallDelay As Double
        Dim aElevatorLoadRate, aElevatorUnloadRate As Double
        Dim aElevatorDoorType As Integer
        Dim aBuildingfile As Boolean

        ' Properties of the building and it's occupants

        'If Me.Estimate.Text = "OK" Then
        'Me.Estimate.Text = "Estimate"
        'CompleteFlg = True
        'estimatingFlg = False
        'ResetEstimator()
        'ElseIf (Not estimatingFlg) And CompleteFlg Then

        '''''''''''''''''''''''''''''''''''''''''''''''
        '  READING THE BATCHLIST FILE AND CREATING ARRAY FOR BATCH PARAMETERS
        '''''''''''''''''''''''''''''''''''''''''''''''

        Dim strfilename As String
        Dim num_rows As Long
        Dim num_cols As Long
        Dim x As Integer
        Dim y As Integer
        Dim strarray(1, 1) As String

        strfilename = "batchlist.csv"

        Dim tmpstream As StreamReader = System.IO.File.OpenText(strfilename)
        Dim strlines() As String
        Dim strline() As String

        strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)

        num_rows = UBound(strlines)
        strline = strlines(0).Split(",")
        num_cols = UBound(strline)
        ReDim strarray(num_rows, num_cols)

        For x = 0 To num_rows
            strline = strlines(x).Split(",")

            For y = 0 To num_cols
                strarray(x, y) = strline(y)
            Next
        Next

        tmpstream.Close()

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

        aElevatorLoadRate = EgressElevator.defaultLoadRate
        aElevatorUnloadRate = EgressElevator.defaultUnloadRate

        'Ask user for filename to save output


        Dim DotCSVLocation As Integer
        DotCSVLocation = OriginalFileName.IndexOf(".csv")

        Dim LittleString As String
        Dim Score As String



        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        '  BATCH LOOP STARTS HERE
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

        'Progress.Maximum = estimator.MaxCriteriaValue
        'Progress.Value = estimator.CurrentCalcValue

        Score = String.Concat(Batchrun.ToString, "/", NumBatchruns.ToString)
        BatchRunCounter.Text = String.Concat("Run: ", Score)
        BatchRunCounter.Show()
        LittleString = String.Concat("-", Batchrun.ToString("00"))
        FileName = OriginalFileName.Insert(DotCSVLocation, LittleString)


        aNumFloors = Val(strarray(0, Batchrun))
        aNumOccupants = Val(strarray(1, Batchrun))
        aElevatorFrac = Val(strarray(2, Batchrun))
        aHallLength = Val(strarray(4, Batchrun))
        aHallWidth = Val(strarray(5, Batchrun))
        aHallEntryRate = Val(strarray(6, Batchrun))
        aFirstFloorUseStairwells = Val(strarray(8, Batchrun))
        aBuildingfile = Val(strarray(3, Batchrun))
        aNumStairs = Val(strarray(7, Batchrun))
        aStairWidth = Val(strarray(9, Batchrun))
        aFlightsPerFloor = Val(strarray(10, Batchrun))
        aStairsPerFlight = Val(strarray(11, Batchrun))
        aStairRiserHeight = Val(strarray(12, Batchrun)) / 1000
        aStairTreadDepth = Val(strarray(13, Batchrun)) / 1000
        aStairEntryRate = Val(strarray(14, Batchrun))
        aExitHallLength = Val(strarray(16, Batchrun))
        aExitHallWidth = Val(strarray(17, Batchrun))
        aExitHallEntryRate = Val(strarray(15, Batchrun))
        aExitHallExitRate = Val(strarray(18, Batchrun))
        aNumElevators = Val(strarray(19, Batchrun))
        aMaxElevatorCarCap = Val(strarray(21, Batchrun))
        aElevatorVel = Val(strarray(22, Batchrun))
        aElevatorAcc = Val(strarray(23, Batchrun))
        aElevatorRecallDelay = Val(strarray(24, Batchrun))
        aElevatorDoorType = Val(strarray(21, Batchrun))

        estimator = New EgressCalculation(aNumFloors, aNumOccupants, aElevatorFrac, aNumStairs, _
                        aMergingFlow, aFlightsPerFloor, aStairRiserHeight, aStairTreadDepth, _
                        aStairsPerFlight, aStairWidth, aStairHasHandrails, aStairWidth, _
                        1.5 * aStairWidth, EgressElement.Bndry.StairHandrails, aStairEntryRate, _
                        aHallLength, aHallWidth, EgressElement.Bndry.Corridor, aHallEntryRate, _
                        aExitHallLength, aExitHallWidth, EgressElement.Bndry.Corridor, _
                        aExitHallEntryRate, aExitHallExitRate, aFirstFloorUseStairwells, _
                        aNumElevators, aElevatorDoorType, aElevatorVel, aNumElevators, _
                        aMaxElevatorCarCap, aElevatorAcc, aElExitHallLength, aElExitHallWidth, _
                        EgressElement.Bndry.Corridor, aElExitHallExitRate, aElevatorRecallDelay)

        If Not estimator.IsValid Then
            MsgBox("Error, not a validly defined simulation")
            Return
        End If

        If aEndEstimate > 0 Then
            If Not estimator.InitializeCalc(aEndEstimate, 1) Then
                MsgBox("Error in Initializing on time criteria t = " + aEndEstimate.ToString)
                Return
            End If
        Else
            If Not estimator.InitializeCalc(1) Then
                MsgBox("Error in Initializing for population criteria")
                Return
            End If
        End If

        If Not Debug Then
            s = "Time(s)," + estimator.ElementTotPopNamesCSV + estimator.EndLine
            My.Computer.FileSystem.WriteAllText(FileName, s, True)
            s = estimator.DoElementTotPopsCSV + estimator.EndLine
            My.Computer.FileSystem.WriteAllText(FileName, s, True)
        Else
            estimator.DoDebug = Debug
            s = "Time(s)," + estimator.PDEElementTotPopNamesCSV + "Total Egress, Total In," + estimator.EndLine
            My.Computer.FileSystem.WriteAllText(FileName, s, True)

        End If

        'CompleteFlg = True
        'estimatingFlg = False



        'BatchRunCounter.Hide()
        RunEstimator()
        'End If




    End Sub

    Private Sub BuildingFile_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BuildingFile.CheckedChanged

        If ee.aBuildingFile Then Return
        If BuildingFile.Checked = True Then
            Dim path As String = Me.SaveFileDialog.InitialDirectory
            My.Computer.FileSystem.CurrentDirectory = path
            ee.aBuildingFile = True
            ee.buildingFile = "buildfile.csv"
            SaveFileDialog.FileName = ee.buildingFile
            BuildingFile.Checked = True
            SaveFileDialog.OverwritePrompt = False
            If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                ee.buildingFile = SaveFileDialog.FileName
                If My.Computer.FileSystem.FileExists(ee.buildingFile) Then
                    Me.SetBuildFileText() 'routine is after LoadFields
                Else
                    NoBuildingDataFile()
                End If
            Else
                NoBuildingDataFile()
            End If
        Else
            NoBuildingDataFile()
        End If
    End Sub
    Private Sub NoBuildingDataFile()
        BuildingFile.Checked = False
        ee.aBuildingFile = False
        ee.buildingFile = "No Building File"
        BuildingFile.Text = "Use building data file"
        Me.Text = "Egress Estimator"
    End Sub

    Private Sub FileMenuOpenItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuOpenItem.Click
        Dim err As String = ""
        SaveFileDialog.FileName = ""
        SaveFileDialog.OverwritePrompt = False
        If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            FileName = SaveFileDialog.FileName
            If ee.ReadInput(FileName, 1, 2, err) Then
                If Not Me.LoadFields(err) Then
                    MsgBox(err)
                End If
            Else
                MsgBox(err)
            End If
        End If
    End Sub

    Private Sub FileMenuSaveItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FileMenuSaveItem.Click
        Dim err As String = ""
        SaveFileDialog.FileName = ""
        SaveFileDialog.OverwritePrompt = True
        If SaveFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            FileName = SaveFileDialog.FileName
            If Not SaveInput(FileName, True, err) Then
                MsgBox("Error Saving input file" + err)
            End If
        End If
    End Sub

    Private Sub NewToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewToolStripMenuItem.Click
        Me.SetDefaultValues()
    End Sub
End Class


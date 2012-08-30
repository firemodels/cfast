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

    Private Function ReadBuildFile(ByVal strfilename As String, ByVal aNumFloors As Integer, ByVal PopPerFlr As Integer, ByVal ElFracPerFlr As Double, _
                                   ByRef strPop() As Integer, ByRef lbyFrac() As Double, ByRef strDelay() As Double, ByRef lbyDelay() As Double, ByRef errMsg As String) As Boolean

        Dim num_rows As Long
        Dim num_cols As Long
        Dim sflr, eflr As Integer
        Dim x As Integer
        Dim y As Integer
        Dim anum_cols As Integer
        Dim strarray(1, 1) As String

        ' Load the file.

        Dim tmpstream As StreamReader = System.IO.File.OpenText(strfilename)
        Dim strlines() As String
        Dim strline() As String

        'Load content of file to strLines array
        strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)

        ' Redimension the array.
        num_rows = UBound(strlines) + 1
        strline = strlines(0).Split(",")
        'num_cols = UBound(strline) + 1
        num_cols = 6
        ReDim strarray(num_rows, num_cols)

        ' Copy the data into the array.
        For x = 0 To num_rows - 1
                strline = strlines(x).Split(",")
            anum_cols = UBound(strline) + 1
            If anum_cols = 6 Then
                For y = 0 To num_cols - 1
                    strarray(x, y) = strline(y)
                Next
            ElseIf anum_cols <> 6 Then
                errMsg = "Building file must have 6 columns current file has " + anum_cols.ToString + " in row " + (x + 1).ToString
                Return False
            End If
        Next
        ReDim strPop(aNumFloors - 1)
        ReDim lbyFrac(aNumFloors - 1)
        ReDim strDelay(aNumFloors - 1)
        ReDim lbyDelay(aNumFloors - 1)
        For x = 0 To aNumFloors - 1
            strPop(x) = PopPerFlr
            strDelay(x) = 0.0
            lbyFrac(x) = ElFracPerFlr
            lbyDelay(x) = 0.0
        Next

        For x = 1 To num_rows - 1
            sflr = Val(strarray(x, 0))
            eflr = Val(strarray(x, 1))
            If sflr >= 2 And eflr >= sflr And eflr <= aNumFloors Then
                For y = sflr - 1 To eflr - 1
                    strPop(y) = Val(strarray(x, 2))
                    lbyFrac(y) = Val(strarray(x, 3))
                    strDelay(y) = Val(strarray(x, 4))
                    lbyDelay(y) = Val(strarray(x, 5))
                Next
            Else
                errMsg = "Start floor, " + sflr.ToString + ", must be >= 2 and end floor. " + eflr.ToString + ", must be > start floor and <= top floor, " + aNumFloors.ToString
                Return False
            End If
        Next
        errMsg = "No errors"
        Return True

    End Function

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
            Dim dFile As New System.IO.FileInfo(ee.buildingFile)
            BuildingFile.Text = "Data file: " + dFile.Name
            Me.Text = "Egress Estimator: " + dFile.DirectoryName
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

    Private Function IndexFromElevatorCapacity(ByVal cap As Integer) As Integer
        For i As Integer = 0 To 4
            If ElevatorCapacitiesinPeople(i) = cap Then
                Return i
            End If
        Next
        Return 5
    End Function

    Private Function ReadInput(ByVal EgressIOFile As String, ByVal skipRows As Integer, ByVal col As Integer, ByVal errMsg As String) As Boolean
        Dim num_rows As Long
        Dim num_cols As Long
        Dim x As Integer
        Dim y As Integer
        Dim anum_cols As Integer
        Dim strarray(,) As String

        ' Load the file.

        Dim tmpstream As StreamReader = System.IO.File.OpenText(EgressIOFile)
        Dim strlines() As String
        Dim strline() As String

        'Load content of file to strLines array
        strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)
        tmpstream.Close()

        ' Redimension the array.
        num_rows = UBound(strlines) + 1
        If num_rows < Me.IOParameters + skipRows Then
            errMsg = "must have " + Me.IOParameters + " rows in io file of input data and " + skipRows.ToString + " rows of before input data starts"
            Return False
        End If
        strline = strlines(0).Split(",")
        num_cols = UBound(strline) + 1
        ReDim strarray(Me.IOParameters, num_cols)

        ' Copy the data into the array.
        For x = skipRows To Me.IOParameters + skipRows - 1
            strline = strlines(x).Split(",")
            anum_cols = UBound(strline) + 1
            If anum_cols >= col Then
                For y = 0 To num_cols - 1
                    strarray(x - skipRows, y) = strline(y)
                Next
            Else
                errMsg = "Must have at least " + col.ToString + " columns in each row"
                Return False
            End If
        Next

        'Sub for taking data from the fields on the interface

        Dim icol As Integer = col - 1
        ee.aNumFloors = Val(strarray(0, icol))
        ee.aNumOccupants = Val(strarray(1, icol))
        ee.aElevatorFrac = Val(strarray(2, icol))
        ee.aHallLength = Val(strarray(3, icol))
        ee.aHallWidth = Val(strarray(4, icol))
        ee.aHallEntryRate = Val(strarray(5, icol))
        If strarray(6, icol) = "True" Then
            ee.aFirstFloorUseStairwells = True
        Else
            ee.aFirstFloorUseStairwells = False
        End If
        If strarray(7, icol) = "True" Then
            ee.aBuildingFile = True
        Else
            ee.aBuildingFile = False
        End If
        ee.buildingFile = strarray(8, icol)

        ' Properties of the stairwells

        ee.aNumStairs = Val(strarray(9, icol))
        ee.aStairWidth = Val(strarray(10, icol))
        ee.aFlightsPerFloor = Val(strarray(11, icol))
        ee.aStairsPerFlight = Val(strarray(12, icol))
        ee.aStairRiserHeight = Val(strarray(13, icol))
        ee.aStairTreadDepth = Val(strarray(14, icol))
        ee.aStairEntryRate = Val(strarray(15, icol))

        'Properties of the exit hallways

        ee.aExitHallLength = Val(strarray(16, icol))
        ee.aExitHallWidth = Val(strarray(17, icol))
        ee.aExitHallEntryRate = Val(strarray(18, icol))
        ee.aExitHallExitRate = Val(strarray(19, icol))

        'When to end estimate

        ee.aEndEstimate = Val(strarray(20, icol))

        ' Properties of Elevators

        ee.aNumElevators = Val(strarray(21, icol))
        ee.aMaxElevatorCarCap = Val(strarray(22, icol))
        ee.aElevatorVel = Val(strarray(23, icol))
        ee.aElevatorAcc = Val(strarray(24, icol))
        ee.aElevatorRecallDelay = Val(strarray(25, icol))
        ee.aElevatorDoorType = Val(strarray(26, icol))

        Return True
    End Function

    Private Function SaveInput(ByVal FileName As String, ByVal ReadFieldsFlg As Boolean)
        Dim s As String = ",EgressEstimator Output"
        Dim el As String = EgressCalculation.EndLine

        If ReadFieldsFlg Then
            ReadFields()
        End If
        My.Computer.FileSystem.WriteAllText(FileName, s + el, False)
        s = Me.lbl(0) + ee.aNumFloors.ToString + el
        s = s + Me.lbl(1) + ee.aNumOccupants.ToString + el + Me.lbl(2) + ee.aElevatorFrac.ToString + el + Me.lbl(3) + ee.aHallLength.ToString + el + Me.lbl(4) + ee.aHallWidth.ToString + el
        s = s + Me.lbl(5) + ee.aHallEntryRate.ToString + el + Me.lbl(6) + ee.aFirstFloorUseStairwells.ToString + el + Me.lbl(7) + ee.aBuildingFile.ToString + el
        If ee.aBuildingFile Then
            s = s + Me.lbl(8) + ee.buildingFile + el
        Else
            s = s + Me.lbl(8) + "No Building File" + el
        End If
        s = s + Me.lbl(9) + ee.aNumStairs.ToString + el + Me.lbl(10) + ee.aStairWidth.ToString + el + Me.lbl(11) + ee.aFlightsPerFloor.ToString + el + Me.lbl(12) + ee.aStairsPerFlight.ToString + el
        s = s + Me.lbl(13) + ee.aStairRiserHeight.ToString + el + Me.lbl(14) + ee.aStairTreadDepth.ToString + el + Me.lbl(15) + ee.aStairEntryRate.ToString + el + Me.lbl(16) + ee.aExitHallLength.ToString + el
        s = s + Me.lbl(17) + ee.aExitHallWidth.ToString + el + Me.lbl(18) + ee.aExitHallEntryRate.ToString + el + Me.lbl(19) + ee.aExitHallExitRate.ToString + el + Me.lbl(20) + ee.aEndEstimate.ToString + el
        s = s + Me.lbl(21) + ee.aNumElevators.ToString + el + Me.lbl(22) + ee.aMaxElevatorCarCap.ToString + el
        s = s + Me.lbl(23) + ee.aElevatorVel.ToString + el + Me.lbl(24) + ee.aElevatorAcc.ToString + el + Me.lbl(25) + ee.aElevatorRecallDelay.ToString + el + Me.lbl(26) + ee.aElevatorDoorType.ToString + el
        My.Computer.FileSystem.WriteAllText(FileName, s, True)
        s = "" + el
        My.Computer.FileSystem.WriteAllText(FileName, s, True)
        Return True
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
                    Dim errMsg As String = " "
                    If Not ReadBuildFile(ee.buildingFile, ee.aNumFloors, ee.aNumOccupants, ee.aElevatorFrac, PopulationPF, LobbyFractionPF, StairDelay, LobbyDelay, errMsg) Then
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

                If Not SaveInput(FileName, False) Then
                    MsgBox("Error in SaveInput in Estimate_Click")
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

        Me.lbl(0) = "Number of floors,"
        Me.lbl(1) = "Occupants per floor,"
        Me.lbl(2) = "Fraction using elevator,"
        Me.lbl(3) = "Hall length,"
        Me.lbl(4) = "Hall width,"
        Me.lbl(5) = "Hall entry rate,"
        Me.lbl(6) = "First floor exits in stairs,"
        Me.lbl(7) = "Use building file,"
        Me.lbl(8) = "Building file,"
        Me.lbl(9) = "Number of stairs,"
        Me.lbl(10) = "Stair width,"
        Me.lbl(11) = "Flights per floor,"
        Me.lbl(12) = "Stairs per flight,"
        Me.lbl(13) = "Riser height,"
        Me.lbl(14) = "Tread depth,"
        Me.lbl(15) = "Stair entry rate,"
        Me.lbl(16) = "Exit hall length,"
        Me.lbl(17) = "Exit hall width,"
        Me.lbl(18) = "Exit hall entry rate,"
        Me.lbl(19) = "Exit hall exit rate,"
        Me.lbl(20) = "End criteria,"
        Me.lbl(21) = "Number of elevator cars,"
        Me.lbl(22) = "Max capacity of car,"
        Me.lbl(23) = "Elevator velocity,"
        Me.lbl(24) = "Elevator acceleration,"
        Me.lbl(25) = "Elevator recall delay,"
        Me.lbl(26) = "Elevator door type,"

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
                    Dim dFile As New System.IO.FileInfo(ee.buildingFile)
                    BuildingFile.Text = "Data file: " + dFile.Name
                    Me.Text = "Egress Estimator: " + dFile.DirectoryName
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
            If ReadInput(FileName, 1, 2, err) Then
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
            If Not SaveInput(FileName, True) Then
                MsgBox("Error Saving input file")
            End If
        End If
    End Sub
End Class


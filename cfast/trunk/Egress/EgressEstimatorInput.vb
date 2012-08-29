Imports System.IO
Public Class EgressEstimatorInput
    ' Variables for values in fields
    Protected numFloors(), numOccupants(), numStairs() As Integer
    Protected elevatorFrac() As Double
    Protected hallLength(), hallWidth(), hallEntryRate() As Double
    Protected firstFloorUseStairwells() As Boolean
    'Protected mergingFlow() as Integer ***** old data structure no longer used
    Protected flightsPerFloor(), stairsPerFlight() As Integer
    Protected stairWidth(), stairRiserHeight(), stairTreadDepth(), stairEntryRate() As Double
    'Protected StairHasHandrails() As Boolean ***** old data structure no longer used
    Protected exitHallLength(), exitHallWidth(), exitHallEntryRate(), exitHallExitRate() As Double
    'Protected elExitHallLength(), elExitHallWidth(), elExitHallExitRate() As Double
    Protected endEstimate() As Integer
    Protected numElevators(), maxElevatorCarCap() As Integer
    Protected elevatorVel(), elevatorAcc(), elevatorRecallDelay() As Double
    Protected elevatorLoadRate(), elevatorUnloadRate() As Double
    Protected elevatorDoorType() As Integer
    Protected useBuildingfile() As Boolean
    Protected buildingF() As String
    Protected iOParameters As Integer = 27
    'lbl is defined in SetDefaultValues
    Protected lbl(IOParameters) As String
    Protected elevatorCapFactor As Double = 0.01333
    Protected idx As Integer
    Protected maxIdx As Integer
    Protected rdFileFlg As Boolean
    Protected errMsg As String
    Protected strarray(,) As String
    Protected straRows, straCols As Integer

    Public Overloads Property aNumFloors() As Integer
        Get
            Return Me.aNumFloors(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aNumFloors(Me.idx) = value
            Return
        End Set
    End Property
    Public Overloads Property aNumFloors(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.numFloors(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 2 Then
                    Me.numFloors(i) = value
                Else
                    Me.numFloors(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aNumOccupants(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.numOccupants(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 0 Then
                    Me.numOccupants(i) = value
                Else
                    Me.numOccupants(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aNumOccupants() As Integer
        Get
            Return Me.aNumOccupants(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aNumOccupants(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aNumStairs(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.numStairs(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 1 Then
                    Me.numStairs(i) = value
                Else
                    Me.numStairs(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aNumStairs() As Integer
        Get
            Return Me.aNumStairs(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aNumStairs(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorFrac(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorFrac(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 0.0 And value <= 1.0 Then
                    Me.elevatorFrac(i) = value
                Else
                    Me.elevatorFrac(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorFrac() As Double
        Get
            Return Me.aElevatorFrac(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorFrac(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aHallLength(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.hallLength(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.hallLength(i) = value
                Else
                    Me.hallLength(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aHallLength() As Double
        Get
            Return Me.aHallLength(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aHallLength(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aHallWidth(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.hallWidth(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.hallWidth(i) = value
                Else
                    Me.hallWidth(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aHallWidth() As Double
        Get
            Return Me.aHallWidth(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aHallWidth(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aHallEntryRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.hallEntryRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.hallEntryRate(i) = value
                Else
                    Me.hallEntryRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aHallEntryRate() As Double
        Get
            Return Me.aHallEntryRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aHallEntryRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aFirstFloorUseStairwells(ByVal i As Integer) As Boolean
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.firstFloorUseStairwells(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Boolean)
            If i >= 0 And i < Me.maxIdx Then
                Me.firstFloorUseStairwells(i) = value
            End If
        End Set
    End Property
    Public Overloads Property aFirstFloorUseStairwells() As Boolean
        Get
            Return Me.aFirstFloorUseStairwells(Me.idx)
        End Get
        Set(ByVal value As Boolean)
            Me.aFirstFloorUseStairwells(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aFlightsPerFloor(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.flightsPerFloor(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.flightsPerFloor(i) = value
                Else
                    Me.flightsPerFloor(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aFlightsPerFloor() As Integer
        Get
            Return Me.aFlightsPerFloor(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aFlightsPerFloor(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aStairsPerFlight(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.stairsPerFlight(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.stairsPerFlight(i) = value
                Else
                    Me.stairsPerFlight(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aStairsPerFlight() As Integer
        Get
            Return Me.aStairsPerFlight(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aStairsPerFlight(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aStairWidth(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.stairWidth(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.stairWidth(i) = value
                Else
                    Me.stairWidth(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aStairWidth() As Integer
        Get
            Return Me.aStairWidth(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aStairWidth(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aStairRiserHeight(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.stairRiserHeight(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.stairRiserHeight(i) = value
                Else
                    Me.stairRiserHeight(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aStairRiserHeight() As Double
        Get
            Return Me.aStairRiserHeight(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aStairRiserHeight(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aStairTreadDepth(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.stairTreadDepth(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.stairTreadDepth(i) = value
                Else
                    Me.stairTreadDepth(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aStairTreadDepth() As Double
        Get
            Return Me.aStairTreadDepth(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aStairTreadDepth(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aStairEntryRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.stairEntryRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.stairEntryRate(i) = value
                Else
                    Me.stairEntryRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aStairEntryRate() As Double
        Get
            Return Me.aStairEntryRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aStairEntryRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aExitHallLength(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.exitHallLength(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.exitHallLength(i) = value
                Else
                    Me.exitHallLength(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aExitHallLength() As Double
        Get
            Return Me.aExitHallLength(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aExitHallLength(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aExitHallWidth(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.exitHallWidth(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.exitHallWidth(i) = value
                Else
                    Me.exitHallWidth(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aExitHallWidth() As Double
        Get
            Return Me.aExitHallWidth(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aExitHallWidth(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aExitHallEntryRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.exitHallEntryRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.exitHallEntryRate(i) = value
                Else
                    Me.exitHallEntryRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aExitHallEntryRate() As Double
        Get
            Return Me.aExitHallEntryRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aExitHallEntryRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aExitHallExitRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.exitHallExitRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.exitHallExitRate(i) = value
                Else
                    Me.exitHallExitRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aExitHallExitRate() As Double
        Get
            Return Me.aExitHallExitRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aExitHallExitRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aEndEstimate(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.endEstimate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 0 Then
                    Me.endEstimate(i) = value
                Else
                    Me.endEstimate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aEndEstimate() As Integer
        Get
            Return Me.aEndEstimate(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aEndEstimate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aNumElevators(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.numElevators(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.numElevators(i) = value
                Else
                    Me.numElevators(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aNumElevators() As Integer
        Get
            Return Me.aNumElevators(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aNumElevators(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aMaxElevatorCarCap(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.maxElevatorCarCap(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0 Then
                    Me.maxElevatorCarCap(i) = value
                Else
                    Me.maxElevatorCarCap(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aMaxElevatorCarCap() As Integer
        Get
            Return Me.aMaxElevatorCarCap(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aMaxElevatorCarCap(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorVel(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorVel(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.elevatorVel(i) = value
                Else
                    Me.elevatorVel(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorVel() As Double
        Get
            Return Me.aElevatorVel(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorVel(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorAcc(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorAcc(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.elevatorAcc(i) = value
                Else
                    Me.elevatorAcc(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorAcc() As Double
        Get
            Return Me.aElevatorAcc(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorAcc(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorRecallDelay(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorRecallDelay(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 0.0 Then
                    Me.elevatorRecallDelay(i) = value
                Else
                    Me.elevatorRecallDelay(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorRecallDelay() As Double
        Get
            Return Me.aElevatorRecallDelay(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorRecallDelay(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorLoadRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorLoadRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.elevatorLoadRate(i) = value
                Else
                    Me.elevatorLoadRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorLoadRate() As Double
        Get
            Return Me.aElevatorLoadRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorLoadRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorUnloadRate(ByVal i As Integer) As Double
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorUnloadRate(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If i >= 0 And i < Me.maxIdx Then
                If value > 0.0 Then
                    Me.elevatorUnloadRate(i) = value
                Else
                    Me.elevatorUnloadRate(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorUnloadRate() As Double
        Get
            Return Me.aElevatorUnloadRate(Me.idx)
        End Get
        Set(ByVal value As Double)
            Me.aElevatorUnloadRate(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aElevatorDoorType(ByVal i As Integer) As Integer
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.elevatorDoorType(i)
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Integer)
            If i >= 0 And i < Me.maxIdx Then
                If value >= 0 Then
                    Me.elevatorDoorType(i) = value
                Else
                    Me.elevatorDoorType(i) = -1
                End If
            End If
        End Set
    End Property
    Public Overloads Property aElevatorDoorType() As Integer
        Get
            Return Me.aElevatorDoorType(Me.idx)
        End Get
        Set(ByVal value As Integer)
            Me.aElevatorDoorType(Me.idx) = value
        End Set
    End Property
    Public Overloads Property aBuildingFile(ByVal i As Integer) As Boolean
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.useBuildingfile(i)
            Else
                Return False
            End If
        End Get
        Set(ByVal value As Boolean)
            If i >= 0 And i > Me.maxIdx Then
                Me.useBuildingfile(i) = value
            End If
        End Set
    End Property
    Public Overloads Property aBuildingFile() As Boolean
        Get
            Return Me.aBuildingFile(Me.idx)
        End Get
        Set(ByVal value As Boolean)
            Me.aBuildingFile(Me.idx) = value
        End Set
    End Property
    Public Overloads Property buildingFile(ByVal i As Integer) As String
        Get
            If i >= 0 And i < Me.maxIdx Then
                Return Me.buildingF(i)
            Else
                Return False
            End If
        End Get
        Set(ByVal value As String)
            If i >= 0 And i > Me.maxIdx Then
                Me.buildingF(i) = value
            End If
        End Set
    End Property
    Public Overloads Property buildingFile() As String
        Get
            Return Me.BuildingFile(Me.idx)
        End Get
        Set(ByVal value As String)
            Me.BuildingFile(Me.idx) = value
        End Set
    End Property
    Public ReadOnly Property numInputs() As Integer
        Get
            Return Me.iOParameters
        End Get
    End Property
    Public Property CurrentCol() As Integer
        Get
            Return Me.idx
        End Get
        Set(ByVal value As Integer)
            If value >= 0 And value < Me.maxIdx Then
                Me.idx = value
            End If
        End Set
    End Property
    Public ReadOnly Property MaxCol() As Integer
        Get
            Return Me.maxIdx
        End Get
    End Property
    Public ReadOnly Property aElevatorCapFactor() As Double
        Get
            Return Me.elevatorCapFactor
        End Get
    End Property
    Public ReadOnly Property errors() As Boolean
        Get
            Return Me.rdFileFlg
        End Get
    End Property
    Public ReadOnly Property aErrMsg() As String
        Get
            Return Me.errMsg
        End Get
    End Property

    Public Sub New(ByVal imax As Integer)

        Me.MakeSpace(imax)
        Me.InitSpace()
        Me.idx = 0
        Me.rdFileFlg = False
        Me.errMsg = "No Errors"
        Me.SetLabels()

    End Sub

    Public Sub New(ByVal filenm As String, ByVal skipRows As Integer)
        Dim emsg As String = ""

        Me.rdFileFlg = Me.ReadInput(filenm, 1, emsg)
        Me.errMsg = emsg
        Me.SetLabels()

    End Sub

    Private Sub MakeSpace(ByVal imax As Integer)
        ReDim numFloors(imax)
        ReDim numOccupants(imax)
        ReDim numStairs(imax)
        ReDim elevatorFrac(imax)
        ReDim hallLength(imax)
        ReDim hallWidth(imax)
        ReDim hallEntryRate(imax)
        ReDim firstFloorUseStairwells(imax)
        ReDim flightsPerFloor(imax)
        ReDim stairsPerFlight(imax)
        ReDim stairWidth(imax)
        ReDim stairRiserHeight(imax)
        ReDim stairTreadDepth(imax)
        ReDim stairEntryRate(imax)
        ReDim exitHallLength(imax)
        ReDim exitHallWidth(imax)
        ReDim exitHallEntryRate(imax)
        ReDim exitHallExitRate(imax)
        ReDim endEstimate(imax)
        ReDim numElevators(imax)
        ReDim maxElevatorCarCap(imax)
        ReDim elevatorVel(imax)
        ReDim elevatorAcc(imax)
        ReDim elevatorRecallDelay(imax)
        ReDim elevatorLoadRate(imax)
        ReDim elevatorUnloadRate(imax)
        ReDim elevatorDoorType(imax)
        ReDim useBuildingfile(imax)
        ReDim buildingF(imax)
        Me.maxIdx = imax
    End Sub

    Private Sub InitSpace()
        For i As Integer = 0 To Me.maxIdx - 1
            Me.numFloors(i) = -1
            Me.numOccupants(i) = -1
            Me.numStairs(i) = -1
            Me.elevatorFrac(i) = -1
            Me.hallLength(i) = -1
            Me.hallWidth(i) = -1
            Me.hallEntryRate(i) = -1
            Me.firstFloorUseStairwells(i) = False
            Me.flightsPerFloor(i) = -1
            Me.stairsPerFlight(i) = -1
            Me.stairWidth(i) = -1
            Me.stairRiserHeight(i) = -1
            Me.stairTreadDepth(i) = -1
            Me.stairEntryRate(i) = -1
            Me.exitHallLength(i) = -1
            Me.exitHallWidth(i) = -1
            Me.exitHallEntryRate(i) = -1
            Me.exitHallExitRate(i) = -1
            Me.endEstimate(i) = -1
            Me.numElevators(i) = -1
            Me.maxElevatorCarCap(i) = -1
            Me.elevatorVel(i) = -1
            Me.elevatorAcc(i) = -1
            Me.elevatorRecallDelay(i) = -1
            Me.elevatorLoadRate(i) = -1
            Me.elevatorUnloadRate(i) = -1
            Me.elevatorDoorType(i) = -1
            Me.useBuildingfile(i) = False
            Me.buildingF(i) = ""
        Next
    End Sub

    Private Sub SetLabels()

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

    Private Function LoadFile(ByVal fileName As String, ByVal skipRows As Integer, ByVal mxRows As Integer, ByVal mxCols As Integer, ByRef errMsg As String) As Boolean
        Dim num_rows As Long
        Dim num_cols As Long
        Dim x As Integer
        Dim y As Integer
        Dim anum_cols As Integer

        ' Load the file.

        Dim tmpstream As StreamReader = System.IO.File.OpenText(fileName)
        Dim strlines() As String
        Dim strline() As String

        'Load content of file to strLines array
        strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)

        ' Redimension the array.
        num_rows = UBound(strlines) + 1
        If mxRows > 0 And num_rows < mxRows + skipRows Then
            errMsg = "In LoadFile: Must have " + mxRows.ToString + " rows in io file of input data and " + skipRows.ToString + " rows of before input data starts"
            Return False
        End If
        Me.straRows = num_rows
        strline = strlines(0).Split(",")
        num_cols = UBound(strline) + 1
        If mxCols > 0 And num_cols < mxCols + skipRows Then
            errMsg = "In LoadFile: Must have " + mxCols.ToString + " columns in io file of input data and " + skipRows.ToString + " rows of before input data starts"
            Return False
        End If
        Me.straCols = num_cols
        ReDim Me.strarray(Me.straRows, Me.straCols)

        ' Copy the data into the array.
        Me.idx = 0
        For x = skipRows To mxRows + skipRows - 1
            strline = strlines(x).Split(",")
            anum_cols = UBound(strline) + 1
            For y = 0 To Me.straCols - 1
                Me.strarray(x - skipRows, y) = strline(y)
            Next
        Next
        errMsg = "In LoadFile: No Errors"
        Return True
    End Function

    Public Overloads Function ReadInput(ByVal fileName As String, ByVal skipRows As Integer, ByRef errMsg As String) As Boolean

        If Not Me.LoadFile(fileName, skipRows, Me.iOParameters, -1, Me.errMsg) Then
            Me.errMsg = "In ReadInput: " + Me.errMsg
            errMsg = Me.errMsg
            Return False
        End If

        'Sub for taking data from the fields on the interface

        For icol As Integer = 1 To Me.straCols - 1
            Me.aNumFloors(icol) = Val(Me.strarray(0, icol))
            Me.aNumOccupants(icol) = Val(Me.strarray(1, icol))
            Me.aElevatorFrac(icol) = Val(Me.strarray(2, icol))
            Me.aHallLength(icol) = Val(Me.strarray(3, icol))
            Me.aHallWidth(icol) = Val(Me.strarray(4, icol))
            Me.aHallEntryRate(icol) = Val(Me.strarray(5, icol))
            If Me.strarray(6, icol) = "TRUE" Then
                Me.aFirstFloorUseStairwells(icol) = True
            Else
                Me.aFirstFloorUseStairwells(icol) = False
            End If
            If Me.strarray(7, icol) = "TRUE" Then
                Me.aBuildingFile(icol) = True
                Me.buildingFile(icol) = Me.strarray(8, icol)
            Else
                Me.aBuildingFile(icol) = False
                Me.buildingFile(icol) = ""
            End If

            ' Properties of the stairwells

            Me.aNumStairs(icol) = Val(Me.strarray(9, icol))
            Me.aStairWidth(icol) = Val(Me.strarray(10, icol))
            Me.aFlightsPerFloor(icol) = Val(Me.strarray(11, icol))
            Me.aStairsPerFlight(icol) = Val(Me.strarray(12, icol))
            Me.aStairRiserHeight(icol) = Val(Me.strarray(13, icol))
            Me.aStairTreadDepth(icol) = Val(Me.strarray(14, icol))
            Me.aStairEntryRate(icol) = Val(Me.strarray(15, icol))

            'Properties of the exit hallways

            Me.aExitHallLength(icol) = Val(strarray(16, icol))
            Me.aExitHallWidth(icol) = Val(strarray(17, icol))
            Me.aExitHallEntryRate(icol) = Val(strarray(18, icol))
            Me.aExitHallExitRate(icol) = Val(strarray(19, icol))

            'When to end estimate

            Me.aEndEstimate(icol) = Val(strarray(20, icol))

            ' Properties of Elevators

            Me.aNumElevators(icol) = Val(strarray(21, icol))
            Me.aMaxElevatorCarCap(icol) = Val(strarray(22, icol))
            Me.aElevatorVel(icol) = Val(strarray(23, icol))
            Me.aElevatorAcc(icol) = Val(strarray(24, icol))
            Me.aElevatorRecallDelay(icol) = Val(strarray(25, icol))
            Me.aElevatorDoorType(icol) = Val(strarray(26, icol))
        Next
        Me.errMsg = "In ReadInput: " + Me.errMsg
        errMsg = Me.errMsg
        Return True
    End Function

    Public Overloads Function ReadInput(ByVal fileName As String, ByRef errMsg As String) As Boolean
        Return Me.ReadInput(fileName, 1, errMsg)
    End Function

    Public Overloads Function ReadBuildFile(ByVal fileName As String, ByVal skipRows As Integer, ByVal icol As Integer, _
                                   ByRef strPop() As Integer, ByRef lbyFrac() As Double, ByRef strDelay() As Double, ByRef lbyDelay() As Double, ByRef errMsg As String) As Boolean

        Dim x, y, sflr, eflr As Integer

        If Not Me.LoadFile(fileName, skipRows, -1, 6, Me.errMsg) Then
            Me.errMsg = "In ReadBuildFile: " + Me.errMsg
            errMsg = Me.errMsg
            Return False
        End If

        'Sub for taking data from the fields on the interface
        ReDim strPop(Me.aNumFloors - 1)
        ReDim lbyFrac(aNumFloors - 1)
        ReDim strDelay(aNumFloors - 1)
        ReDim lbyDelay(aNumFloors - 1)
        For x = 0 To aNumFloors - 1
            strPop(x) = Me.aNumOccupants(icol)
            strDelay(x) = 0.0
            lbyFrac(x) = Me.aElevatorFrac(icol)
            lbyDelay(x) = 0.0
        Next

        For x = 1 To Me.straRows - 1
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
                Me.errMsg = "In ReadBuildFile: Start floor, " + sflr.ToString + ", must be >= 2 and end floor. " + eflr.ToString + ", must be > start floor and <= top floor, " + aNumFloors.ToString
                errMsg = Me.errMsg
                Return False
            End If
        Next
        Me.errMsg = "In ReadBuildFile: " + Me.errMsg
        Return True
        Return True
    End Function

    Public Overloads Function ReadBuildFile(ByVal fileName As String, _
                                   ByRef strPop() As Integer, ByRef lbyFrac() As Double, ByRef strDelay() As Double, ByRef lbyDelay() As Double, ByRef errMsg As String) As Boolean
        Return Me.ReadBuildFile(fileName, 1, Me.idx, strPop, lbyFrac, strDelay, lbyDelay, errMsg)
    End Function

    Public Overloads Function WriteInput(ByVal fileName As String, ByVal icol As Integer, ByRef errMsg As String)
        Dim s As String = ",EgressEstimator Output"
        Dim el As String = Chr(13) + Chr(10)

        My.Computer.FileSystem.WriteAllText(fileName, s + el, False)
        s = Me.lbl(0) + Me.aNumFloors(icol).ToString + el
        s = s + Me.lbl(1) + Me.aNumOccupants(icol).ToString + el + Me.lbl(2) + Me.aElevatorFrac(icol).ToString + el + Me.lbl(3) + Me.aHallLength(icol).ToString + el + Me.lbl(4) + Me.aHallWidth(icol).ToString + el
        s = s + Me.lbl(5) + Me.aHallEntryRate(icol).ToString + el + Me.lbl(6) + Me.aFirstFloorUseStairwells(icol).ToString + el + Me.lbl(7) + Me.aBuildingFile(icol).ToString + el
        If Me.aBuildingFile(icol) Then
            s = s + Me.lbl(8) + Me.buildingFile(icol) + el
        Else
            s = s + Me.lbl(8) + "No Building File" + el
        End If
        s = s + Me.lbl(9) + Me.aNumStairs(icol).ToString + el + Me.lbl(10) + Me.aStairWidth(icol).ToString + el + Me.lbl(11) + Me.aFlightsPerFloor(icol).ToString + el + Me.lbl(12) + Me.aStairsPerFlight(icol).ToString + el
        s = s + Me.lbl(13) + Me.aStairRiserHeight(icol).ToString + el + Me.lbl(14) + Me.aStairTreadDepth(icol).ToString + el + Me.lbl(15) + Me.aStairEntryRate(icol).ToString + el + Me.lbl(16) + Me.aExitHallLength(icol).ToString + el
        s = s + Me.lbl(17) + Me.aExitHallWidth(icol).ToString + el + Me.lbl(18) + Me.aExitHallEntryRate(icol).ToString + el + Me.lbl(19) + Me.aExitHallExitRate(icol).ToString + el + Me.lbl(20) + Me.aEndEstimate(icol).ToString + el
        s = s + Me.lbl(21) + Me.aNumElevators(icol).ToString + el + Me.lbl(22) + Me.aMaxElevatorCarCap(icol).ToString + el
        s = s + Me.lbl(23) + Me.aElevatorVel(icol).ToString + el + Me.lbl(24) + Me.aElevatorAcc(icol).ToString + el + Me.lbl(25) + Me.aElevatorRecallDelay(icol).ToString + el + Me.lbl(26) + Me.aElevatorDoorType(icol).ToString + el
        My.Computer.FileSystem.WriteAllText(fileName, s, True)
        s = "" + el
        My.Computer.FileSystem.WriteAllText(fileName, s, True)
        Me.errMsg = "In WriteInput: No Errors"
        errMsg = Me.errMsg
        Return True
    End Function

    Public Overloads Function WriteInput(ByVal fileName As String, ByRef errMsg As String)
        Return Me.WriteInput(fileName, Me.idx, errMsg)
    End Function
End Class

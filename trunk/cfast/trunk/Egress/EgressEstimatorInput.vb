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
    Protected iOP As Integer = 27 'iOP for IOParameter
    Protected lbl(iOP) As String
    Protected elevatorCapFactor As Double = 0.01333
    Protected idx As Integer
    Protected maxIdx As Integer
    Protected rdFileFlg As Boolean
    Protected errMsg As String
    Protected strarray(,) As String
    Protected straRows, straCols As Integer

    Public ReadOnly Property IOParameter As Integer
        Get
            Return iOP
        End Get
    End Property

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
            If i >= 0 And i < Me.maxIdx Then
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
            If i >= 0 And i < Me.maxIdx Then
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
            Return Me.IOParameter
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

        Me.rdFileFlg = Me.ReadInput(filenm, 1, 2, emsg)
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

        Me.lbl(0) = "Number of floors"
        Me.lbl(1) = "Occupants per floor"
        Me.lbl(2) = "Fraction using elevator"
        Me.lbl(3) = "Hall length"
        Me.lbl(4) = "Hall width"
        Me.lbl(5) = "Hall entry rate"
        Me.lbl(6) = "First floor exits in stairs"
        Me.lbl(7) = "Use building file"
        Me.lbl(8) = "Building file"
        Me.lbl(9) = "Number of stairs"
        Me.lbl(10) = "Stair width"
        Me.lbl(11) = "Flights per floor"
        Me.lbl(12) = "Stairs per flight"
        Me.lbl(13) = "Riser height"
        Me.lbl(14) = "Tread depth"
        Me.lbl(15) = "Stair entry rate"
        Me.lbl(16) = "Exit hall length"
        Me.lbl(17) = "Exit hall width"
        Me.lbl(18) = "Exit hall entry rate"
        Me.lbl(19) = "Exit hall exit rate"
        Me.lbl(20) = "End criteria"
        Me.lbl(21) = "Number of elevator cars"
        Me.lbl(22) = "Max capacity of car"
        Me.lbl(23) = "Elevator velocity"
        Me.lbl(24) = "Elevator acceleration"
        Me.lbl(25) = "Elevator recall delay"
        Me.lbl(26) = "Elevator door type"

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
        tmpstream.Close()

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

    Public Overloads Function ReadInput(ByVal fileName As String, ByVal skipRows As Integer, ByVal col As Integer, ByRef errMsg As String) As Boolean
        Dim idx As Integer = -1

        If Not Me.LoadFile(fileName, skipRows, Me.iOP, -1, Me.errMsg) Then
            Me.errMsg = "In ReadInput: " + Me.errMsg
            errMsg = Me.errMsg
            Return False
        End If

        'Sub for taking data from the fields on the interface

        Dim icol As Integer = col - 1
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aNumFloors = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aNumOccupants = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aElevatorFrac = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aHallLength = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aHallWidth = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aHallEntryRate = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        If strarray(idx, icol) = "True" Then
            Me.aFirstFloorUseStairwells = True
        Else
            Me.aFirstFloorUseStairwells = False
        End If
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        If strarray(idx, icol) = "True" Then
            Me.aBuildingFile = True
        Else
            Me.aBuildingFile = False
        End If
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.buildingFile = strarray(idx, icol)

        ' Properties of the stairwells

        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aNumStairs = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aStairWidth = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aFlightsPerFloor = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aStairsPerFlight = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aStairRiserHeight = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aStairTreadDepth = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aStairEntryRate = Val(strarray(idx, icol))

        'Properties of the exit hallways

        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aExitHallLength = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aExitHallWidth = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aExitHallEntryRate = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aExitHallExitRate = Val(strarray(idx, icol))

        'When to end estimate

        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aEndEstimate = Val(strarray(idx, icol))

        ' Properties of Elevators

        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aNumElevators = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aMaxElevatorCarCap = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aElevatorVel = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aElevatorAcc = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aElevatorRecallDelay = Val(strarray(idx, icol))
        If Not Me.TokenCk(idx, errMsg) Then
            Return False
        End If
        Me.aElevatorDoorType = Val(strarray(idx, icol))

        Return True
    End Function

    Private Function TokenCk(ByRef idx As Integer, ByRef err As String) As Boolean
        idx = idx + 1
        If String.Compare(Me.strarray(idx, 0), Me.lbl(idx)) Then
            Return True
        Else
            err = "EgressEstimatorInput:ReadInput: error in row " + (idx + 1).ToString + " label doesn't match"
            Return False
        End If
    End Function

    Public Overloads Function ReadInput(ByVal fileName As String, ByRef errMsg As String) As Boolean
        Return Me.ReadInput(fileName, 1, 2, errMsg)
    End Function

    Public Overloads Function ReadBuildFile(ByVal fileName As String, ByVal skipRows As Integer, _
                                   ByRef strPop() As Integer, ByRef lbyFrac() As Double, ByRef strDelay() As Double, ByRef lbyDelay() As Double, ByRef errMsg As String) As Boolean

        Dim num_rows As Long
        Dim num_cols As Long
        Dim sflr, eflr As Integer
        Dim x As Integer
        Dim y As Integer
        Dim anum_cols As Integer
        Dim strarray(1, 1) As String

        ' Load the file.

        Dim tmpstream As StreamReader = System.IO.File.OpenText(fileName)
        Dim strlines() As String
        Dim strline() As String

        'Load content of file to strLines array
        strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)
        tmpstream.Close()

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
            ElseIf String.Compare(strline(0), "End") Then
                Exit For
            ElseIf anum_cols <> 6 Then
                errMsg = "Building file must have 6 columns current file has " + anum_cols.ToString + " in row " + (x + 1).ToString
                Return False
            End If
        Next
        num_rows = x
        ReDim strPop(Me.aNumFloors - 1)
        ReDim lbyFrac(Me.aNumFloors - 1)
        ReDim strDelay(Me.aNumFloors - 1)
        ReDim lbyDelay(Me.aNumFloors - 1)
        For x = 0 To Me.aNumFloors - 1
            strPop(x) = Me.aNumOccupants
            strDelay(x) = 0.0
            lbyFrac(x) = Me.aElevatorFrac
            lbyDelay(x) = 0.0
        Next

        For x = skipRows To num_rows - 1
            sflr = Val(strarray(x, 0))
            eflr = Val(strarray(x, 1))
            If sflr >= 2 And eflr >= sflr And eflr <= Me.aNumFloors Then
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

    Public Overloads Function ReadBuildFile(ByVal fileName As String, _
                                   ByRef strPop() As Integer, ByRef lbyFrac() As Double, ByRef strDelay() As Double, ByRef lbyDelay() As Double, ByRef errMsg As String) As Boolean
        Return Me.ReadBuildFile(fileName, 1, strPop, lbyFrac, strDelay, lbyDelay, errMsg)
    End Function

    Public Overloads Function WriteInput(ByVal fileName As String, ByVal icol As Integer, ByRef errMsg As String) As Boolean
        Dim s As String = ",EgressEstimator Output"
        Dim el As String = Chr(13) + Chr(10)
        Dim c As String = ","

        My.Computer.FileSystem.WriteAllText(fileName, s + el, False)
        s = Me.lbl(0) + c + Me.aNumFloors(icol).ToString + el
        s = s + Me.lbl(1) + c + Me.aNumOccupants(icol).ToString + el + Me.lbl(2) + c + Me.aElevatorFrac(icol).ToString + el + Me.lbl(3) + c + Me.aHallLength(icol).ToString + el + Me.lbl(4) + c + Me.aHallWidth(icol).ToString + el
        s = s + Me.lbl(5) + c + Me.aHallEntryRate(icol).ToString + el + Me.lbl(6) + c + Me.aFirstFloorUseStairwells(icol).ToString + el + Me.lbl(7) + c + Me.aBuildingFile(icol).ToString + el
        If Me.aBuildingFile(icol) Then
            s = s + Me.lbl(8) + c + System.IO.Path.GetFileName(Me.buildingFile(icol)) + el
        Else
            s = s + Me.lbl(8) + c + "No Building File" + el
        End If
        s = s + Me.lbl(9) + c + Me.aNumStairs(icol).ToString + el + Me.lbl(10) + c + Me.aStairWidth(icol).ToString + el + Me.lbl(11) + c + Me.aFlightsPerFloor(icol).ToString + el + Me.lbl(12) + c + Me.aStairsPerFlight(icol).ToString + el
        s = s + Me.lbl(13) + c + Me.aStairRiserHeight(icol).ToString + el + Me.lbl(14) + c + Me.aStairTreadDepth(icol).ToString + el + Me.lbl(15) + c + Me.aStairEntryRate(icol).ToString + el + Me.lbl(16) + c + Me.aExitHallLength(icol).ToString + el
        s = s + Me.lbl(17) + c + Me.aExitHallWidth(icol).ToString + el + Me.lbl(18) + c + Me.aExitHallEntryRate(icol).ToString + el + Me.lbl(19) + c + Me.aExitHallExitRate(icol).ToString + el + Me.lbl(20) + c + Me.aEndEstimate(icol).ToString + el
        s = s + Me.lbl(21) + c + Me.aNumElevators(icol).ToString + el + Me.lbl(22) + c + Me.aMaxElevatorCarCap(icol).ToString + el
        s = s + Me.lbl(23) + c + Me.aElevatorVel(icol).ToString + el + Me.lbl(24) + c + Me.aElevatorAcc(icol).ToString + el + Me.lbl(25) + c + Me.aElevatorRecallDelay(icol).ToString + el + Me.lbl(26) + c + Me.aElevatorDoorType(icol).ToString + el
        My.Computer.FileSystem.WriteAllText(fileName, s, True)
        s = "" + el
        My.Computer.FileSystem.WriteAllText(fileName, s, True)
        Me.errMsg = "In WriteInput: No Errors"
        errMsg = Me.errMsg
        Return True
    End Function

    Public Overloads Function WriteInput(ByVal fileName As String, ByRef errMsg As String) As Boolean
        Return Me.WriteInput(fileName, Me.idx, errMsg)
    End Function
End Class

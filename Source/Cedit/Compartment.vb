Public Class Compartment
    Friend Const TypeNormal As Integer = 0
    Friend Const TypeShaft As Integer = 1
    Friend Const TypeCorridor As Integer = 2
    Friend Const MaximumCompartments As Integer = 100
    Friend Const OutsideCompartment As Integer = -1
    Friend Const UndefinedCompartment As Integer = -2
    Friend Const TempCompartment As Integer = -3

    Public Const MinSize As Double = 0.5
    Public Const MaxSize As Double = 100.0
    Public Const MinPosition As Double = 0.0
    Public Const MaxPosition As Double = 10000.0

    ' All units within the class are assumed to be consistent and typically SI
    Private aName As String                 ' Name for compartment
    Private aRoomHeight As Double           ' Height (highest point) of compartment ceiling relative to compartment floor
    Private aRoomWidth As Double            ' Width of the compartment
    Private aRoomDepth As Double            ' Depth of the compartment
    Private aRoomOriginX As Double          ' Absolute X position of lower left corner of room
    Private aRoomOriginY As Double          ' Absolute Y position of lower left corner of room
    Private aRoomOriginZ As Double          ' Absolute Z position (height) of the floor of the compartment
    Private aCeilingMaterial(3) As String   ' Named material for ceiling from Thermal.df
    Private aWallMaterial(3) As String      ' Named material for walls from Thermal.df
    Private aFloorMaterial(3) As String     ' Named material for floor from Thermal.df
    Private aCeilingThickness(3) As Double  ' Ceiling layer thickness
    Private aWallThickness(3) As Double     ' Wall layer thickness
    Private aFloorThickness(3) As Double    ' Floor layer thickness
    Private aShaft As Boolean               ' True if compartment is a aShaft
    Private aHall As Boolean                ' True if compartment is a aHallway
    Private aAreaPoints(0) As Double        ' Vector of room areas as a function of height
    Private aHeightPoints(0) As Double      ' Vector of room heights corresponding to room areas
    Private aGridCells(3) As Integer        ' Number of grid cells for visualization in x, y, z directions
    Private aAreaRampID As String           ' Name of Ramp for area as a function of height
    Private aWallLeak As Double             ' Wall leakage per unit area of wall
    Private aFloorLeak As Double            ' Floor leakage per unit area of floor
    Private aFYI As String                  ' Descriptor for additional user supplied information
    Private aChanged As Boolean = False     ' True once compartment information has changed
    Private HasErrors As Integer = 0        ' Temporary variable to indicate whether there are errors in the specification
    Private i As Integer
    Public Sub New()
        aName = ""
        aRoomHeight = 2.4
        aRoomDepth = 2.4
        aRoomWidth = 3.6
        aRoomOriginX = 0.0
        aRoomOriginY = 0.0
        aRoomOriginZ = 0.0
        For i = 1 To 3
            aCeilingMaterial(i) = ""
            aWallMaterial(i) = ""
            aFloorMaterial(i) = ""
            aCeilingThickness(i) = 0.0
            aWallThickness(i) = 0.0
            aFloorThickness(i) = 0.0
        Next
        aAreaPoints(0) = aRoomDepth * aRoomWidth
        aHeightPoints(0) = aRoomHeight
        aWallLeak = 0
        aFloorLeak = 0
        aFYI = ""
        aShaft = False
        aHall = False
        aGridCells = {50, 50, 50, 50}
        aAreaRampID = ""
    End Sub
    Public Property Name() As String
        Get
            Return aName
        End Get
        Set(ByVal Value As String)
            If Value <> aName Then
                aChanged = True
                aName = Value
            End If
        End Set
    End Property
    Public Property RoomHeight() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomHeight)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomHeight And Value > 0.0 Then
                aChanged = True
                aRoomHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property RoomWidth() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomWidth)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomWidth And Value > 0.0 Then
                aChanged = True
                aRoomWidth = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property RoomDepth() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomDepth)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomDepth And Value > 0.0 Then
                aChanged = True
                aRoomDepth = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property RoomOriginX() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomOriginX)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomOriginX Then
                aChanged = True
                aRoomOriginX = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property RoomOriginY() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomOriginY)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomOriginY Then
                aChanged = True
                aRoomOriginY = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property RoomOriginZ() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aRoomOriginZ)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aRoomOriginZ Then
                aChanged = True
                aRoomOriginZ = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Public Property CeilingMaterial(index As Integer) As String
        Get
            If index >= 1 And index <= 3 Then
                Return aCeilingMaterial(index)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return "Off"
            End If
        End Get
        Set(ByVal Value As String)
            If index >= 1 And index <= 3 Then
                If Value <> aCeilingMaterial(index) And Value <> "" Then
                    If myCompartments.DoChange Then aChanged = True
                    aCeilingMaterial(index) = myThermalProperties.ValidThermalProperty(Value, "Ceiling Material")
                    If Value = "Off" Then
                        aCeilingMaterial(index) = ""
                        If index = 1 Then
                            CeilingMaterial(2) = ""
                            CeilingMaterial(3) = ""
                        ElseIf index = 2 Then
                            CeilingMaterial(3) = ""
                        End If
                    End If
                ElseIf Value = "" Then
                    aCeilingMaterial(index) = ""
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If
        End Set
    End Property
    Public Property CeilingThickness(index As Integer) As Double
        Get
            If index >= 1 And index <= 3 Then
                Return myUnits.Convert(UnitsNum.Length).FromSI(aCeilingThickness(index))
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return 0.0
            End If
        End Get
        Set(value As Double)
            If index >= 1 And index <= 3 Then
                If value <> aCeilingThickness(index) And value >= 0 Then
                    If myCompartments.DoChange Then aChanged = True
                    aCeilingThickness(index) = myUnits.Convert(UnitsNum.Length).ToSI(value)
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If

        End Set
    End Property
    Public Property WallMaterial(index As Integer) As String
        Get
            If index >= 1 And index <= 3 Then
                Return aWallMaterial(index)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return "Off"
            End If
        End Get
        Set(ByVal Value As String)
            If index >= 1 And index <= 3 Then
                If Value <> aWallMaterial(index) And Value <> "" Then
                    If myCompartments.DoChange Then aChanged = True
                    aWallMaterial(index) = myThermalProperties.ValidThermalProperty(Value, "Wall Material")
                    If Value = "Off" Then
                        aWallMaterial(index) = ""
                        If index = 1 Then
                            WallMaterial(2) = ""
                            WallMaterial(3) = ""
                        ElseIf index = 2 Then
                            WallMaterial(3) = ""
                        End If
                    End If
                ElseIf Value = "" Then
                    aWallMaterial(index) = ""
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If
        End Set
    End Property
    Public Property WallThickness(index As Integer) As Double
        Get
            If index >= 1 And index <= 3 Then
                Return myUnits.Convert(UnitsNum.Length).FromSI(aWallThickness(index))
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return 0.0
            End If
        End Get
        Set(value As Double)
            If index >= 1 And index <= 3 Then
                If value <> aWallThickness(index) And value >= 0 Then
                    If myCompartments.DoChange Then aChanged = True
                    aWallThickness(index) = myUnits.Convert(UnitsNum.Length).ToSI(value)
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If

        End Set
    End Property
    Public Property FloorMaterial(index As Integer) As String
        Get
            If index >= 1 And index <= 3 Then
                Return aFloorMaterial(index)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return "Off"
            End If
        End Get
        Set(ByVal Value As String)
            If index >= 1 And index <= 3 Then
                If Value <> aFloorMaterial(index) And Value <> "" Then
                    If myCompartments.DoChange Then aChanged = True
                    aFloorMaterial(index) = myThermalProperties.ValidThermalProperty(Value, "Floor Material")
                    If Value = "Off" Then
                        aFloorMaterial(index) = ""
                        If index = 1 Then
                            FloorMaterial(2) = ""
                            FloorMaterial(3) = ""
                        ElseIf index = 2 Then
                            FloorMaterial(3) = ""
                        End If
                    End If
                ElseIf Value = "" Then
                    aFloorMaterial(index) = ""
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If
        End Set
    End Property
    Public Property FloorThickness(index As Integer) As Double
        Get
            If index >= 1 And index <= 3 Then
                Return myUnits.Convert(UnitsNum.Length).FromSI(aFloorThickness(index))
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
                Return 0.0
            End If
        End Get
        Set(value As Double)
            If index >= 1 And index <= 3 Then
                If value <> aFloorThickness(index) And value >= 0 Then
                    If myCompartments.DoChange Then aChanged = True
                    aFloorThickness(index) = myUnits.Convert(UnitsNum.Length).ToSI(value)
                End If
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Surface material number not found.")
            End If

        End Set
    End Property
    Public Property Shaft() As Boolean
        Get
            Return aShaft
        End Get
        Set(ByVal Value As Boolean)
            If Value <> aShaft Then
                aChanged = True
                aShaft = Value
                If aHall Then aHall = False
            End If
        End Set
    End Property
    Public Property Hall() As Boolean
        Get
            Return aHall
        End Get
        Set(ByVal Value As Boolean)
            If Value <> aHall Then
                aChanged = True
                aHall = Value
                If aShaft Then aShaft = False
            End If
        End Set
    End Property
    Public Property xGrid() As Integer
        Get
            Return aGridCells(1)
        End Get
        Set(value As Integer)
            If value <> aGridCells(1) Then
                aChanged = True
                aGridCells(1) = value
            End If
        End Set
    End Property
    Public Property yGrid() As Integer
        Get
            Return aGridCells(2)
        End Get
        Set(value As Integer)
            If value <> aGridCells(2) Then
                aChanged = True
                aGridCells(2) = value
            End If
        End Set
    End Property
    Public Property zGrid() As Integer
        Get
            Return aGridCells(3)
        End Get
        Set(value As Integer)
            If value <> aGridCells(3) Then
                aChanged = True
                aGridCells(3) = value
            End If
        End Set
    End Property
    Public Property AreaRampID() As String
        Get
            Return aAreaRampID
        End Get
        Set(value As String)
            If value <> aAreaRampID Then
                aChanged = True
                aAreaRampID = value
            End If
        End Set
    End Property
    Public Property WallLeak() As Double
        Get
            Return aWallLeak
        End Get
        Set(value As Double)
            If value <> aWallLeak Then
                aChanged = True
                aWallLeak = value
            End If
        End Set
    End Property
    Public Property FloorLeak() As Double
        Get
            Return aFloorLeak
        End Get
        Set(value As Double)
            If value <> aFloorLeak Then
                aChanged = True
                aFloorLeak = value
            End If
        End Set
    End Property
    Public Property FYI() As String
        Get
            Return aFYI
        End Get
        Set(ByVal Value As String)
            If Value <> aFYI Then
                aChanged = True
                aFYI = Value
            End If
        End Set
    End Property
    Public Sub GetGrid(ByRef axGrid As Integer, ByRef ayGrid As Integer, ByRef azGrid As Integer)
        axGrid = xGrid
        ayGrid = yGrid
        azGrid = zGrid
    End Sub
    Public Sub SetGrid(ByVal axGrid As Integer, ByVal ayGrid As Integer, ByVal azGrid As Integer)
        xGrid = axGrid
        yGrid = ayGrid
        zGrid = azGrid
    End Sub
    Public Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Public Sub GetSize(ByRef aRoomWidth As Double, ByRef aRoomDepth As Double, ByRef aRoomHeight As Double)
        aRoomWidth = RoomWidth
        aRoomDepth = RoomDepth
        aRoomHeight = RoomHeight
    End Sub
    Public Sub SetSize(ByVal aRoomWidth As Double, ByVal aRoomDepth As Double, ByVal aRoomHeight As Double)
        RoomWidth = aRoomWidth
        RoomDepth = aRoomDepth
        RoomHeight = aRoomHeight
        aChanged = True
    End Sub
    Public Sub GetPosition(ByRef aRoomOriginX As Double, ByRef aRoomOriginY As Double, ByRef aRoomOriginZ As Double)
        aRoomOriginX = RoomOriginX
        aRoomOriginY = RoomOriginY
        aRoomOriginZ = RoomOriginZ
    End Sub
    Public Sub SetPosition(ByVal aRoomOriginX As Double, ByVal aRoomOriginY As Double, ByVal aRoomOriginZ As Double)
        RoomOriginX = aRoomOriginX
        RoomOriginY = aRoomOriginY
        RoomOriginZ = aRoomOriginZ
        aChanged = True
    End Sub
    Public Sub SetMaterial(Surface As String, ByVal aMaterial1 As String, ByVal aMaterial2 As String, ByVal aMaterial3 As String)
        If Surface = "Ceiling" Then
            CeilingMaterial(1) = aMaterial1
            CeilingMaterial(2) = aMaterial2
            CeilingMaterial(3) = aMaterial3
        ElseIf Surface = "Walls" Then
            WallMaterial(1) = aMaterial1
            WallMaterial(2) = aMaterial2
            WallMaterial(3) = aMaterial3
        ElseIf Surface = "Floor" Then
            FloorMaterial(1) = aMaterial1
            FloorMaterial(2) = aMaterial2
            FloorMaterial(3) = aMaterial3
        End If
        aChanged = True
    End Sub
    Public Sub SetThickness(Surface As String, ByVal aThickness1 As String, ByVal aThickness2 As String, ByVal aThickness3 As String)
        If Surface = "Ceiling" Then
            CeilingThickness(1) = aThickness1
            CeilingThickness(2) = aThickness2
            CeilingThickness(3) = aThickness3
        ElseIf Surface = "Walls" Then
            WallThickness(1) = aThickness1
            WallThickness(2) = aThickness2
            WallThickness(3) = aThickness3
        ElseIf Surface = "Floor" Then
            FloorThickness(1) = aThickness1
            FloorThickness(2) = aThickness2
            FloorThickness(3) = aThickness3
        End If
        aChanged = True
    End Sub
    Public Sub GetVariableArea(ByRef AreaPoints() As Double, ByRef HeightPoints() As Double, ByRef NumAreaPoints As Integer)
        Dim i As Integer
        If aAreaPoints.GetLength(0) = aHeightPoints.GetLength(0) Then
            ReDim AreaPoints(aAreaPoints.GetUpperBound(0)), HeightPoints(aHeightPoints.GetUpperBound(0))
            For i = 0 To aAreaPoints.GetUpperBound(0)
                AreaPoints(i) = myUnits.Convert(UnitsNum.Area).FromSI(aAreaPoints(i))
                HeightPoints(i) = myUnits.Convert(UnitsNum.Length).FromSI(aHeightPoints(i))
                NumAreaPoints = aAreaPoints.GetUpperBound(0)
            Next
        End If
    End Sub
    Public Sub SetVariableArea(ByVal AreaPoints() As Double, ByVal HeightPoints() As Double)
        Dim i As Integer
        If AreaPoints.GetLength(0) = HeightPoints.GetLength(0) Then
            ReDim aAreaPoints(AreaPoints.GetUpperBound(0)), aHeightPoints(HeightPoints.GetUpperBound(0))
            For i = 0 To AreaPoints.GetUpperBound(0)
                aAreaPoints(i) = myUnits.Convert(UnitsNum.Area).ToSI(AreaPoints(i))
                aHeightPoints(i) = myUnits.Convert(UnitsNum.Length).ToSI(HeightPoints(i))
            Next
            aChanged = True
        End If
    End Sub
    Public Sub GetVariableAreasHeight(ByRef HeightPoints() As Double)
        Dim i As Integer
        ReDim HeightPoints(aHeightPoints.GetUpperBound(0))
        For i = 0 To HeightPoints.GetUpperBound(0)
            HeightPoints(i) = myUnits.Convert(UnitsNum.Area).FromSI(aHeightPoints(i))
        Next
    End Sub
    Public Sub GetVariableAreaPoints(ByRef AreaPoints() As Double)
        Dim i As Integer
        ReDim AreaPoints(aAreaPoints.GetUpperBound(0))
        For i = 0 To AreaPoints.GetUpperBound(0)
            AreaPoints(i) = myUnits.Convert(UnitsNum.Area).FromSI(aAreaPoints(i))
        Next
    End Sub
    Public Sub SetVariableAreasHeight(ByVal HeightPoints() As Double)
        Dim i As Integer
        ReDim aHeightPoints(HeightPoints.GetUpperBound(0))
        For i = 0 To HeightPoints.GetUpperBound(0)
            aHeightPoints(i) = myUnits.Convert(UnitsNum.Area).ToSI(HeightPoints(i))
        Next
        aChanged = True
    End Sub
    Public Sub SetVariableAreaPoints(ByVal AreaPoints() As Double)
        Dim i As Integer
        ReDim aAreaPoints(AreaPoints.GetUpperBound(0))
        For i = 0 To AreaPoints.GetUpperBound(0)
            aAreaPoints(i) = myUnits.Convert(UnitsNum.Area).ToSI(AreaPoints(i))
        Next
        aChanged = True
    End Sub
    Public ReadOnly Property IsValid() As Integer
        ' Checks the overall validity of the current compartment specification
        Get
            HasErrors = 0
            ' Compartment size and position are limited to a minimum and maximum size
            If aRoomWidth <= MinSize Or aRoomWidth > MaxSize Then
                myErrors.Add(aName + " has a width that is outside typical bounds.  Width should be greater than " + MinSize.ToString + " m and less than " + MaxSize.ToString + " m", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            If aRoomDepth <= MinSize Or aRoomDepth > MaxSize Then
                myErrors.Add(aName + " has a depth that is outside typical bounds.  Depth should be greater than " + MinSize.ToString + " m and less than " + MaxSize.ToString + " m", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            If aRoomHeight <= MinSize Or aRoomHeight > MaxSize Then
                myErrors.Add(aName + " has a height that is outside typical bounds.  Height should be greater than " + MinSize.ToString + " m and less than " + MaxSize.ToString + " m", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            If aRoomOriginX < MinPosition Or aRoomOriginX > MaxPosition Or aRoomOriginY < MinPosition Or aRoomOriginY > MaxPosition Or aRoomOriginZ < MinPosition Or aRoomOriginZ > MaxPosition Then
                myErrors.Add(aName + " has a position that is outside typical bounds.  Position should be greater than " + MinPosition.ToString + " m and less than " + MaxPosition.ToString + " m", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            ' Compartment area is limited to the square of the minimum and maximum size. Height is limited to the compartment size
            If aAreaPoints.GetUpperBound(0) > 0 Then
                For i = 0 To aAreaPoints.GetUpperBound(0)
                    If aAreaPoints(i) < 0.0 Or aAreaPoints(i) > MaxSize ^ 2 Then
                        myErrors.Add(aName + " has an area point that is outside typical bounds. Cross-sectional area should be greater than " + (MinSize ^ 2).ToString + " m and less than " + (MaxSize ^ 2).ToString + " m?", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aHeightPoints(i) < 0.0 Or aHeightPoints(i) > aRoomHeight Then
                        myErrors.Add(aName + " has an area point that is outside typical bounds. Height values should be greater than 0 m and less than the compartment height", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                Next
            End If
            ' Thermal properties have to exist
            For i = 1 To 3
                If aCeilingMaterial(i) <> "" Then
                    If myThermalProperties.GetIndex(aCeilingMaterial(i)) < -1 Then
                        myErrors.Add(aName + " has an unknown ceiling material.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                End If
                If aWallMaterial(i) <> "" Then
                    If myThermalProperties.GetIndex(WallMaterial(i)) < -1 Then
                        myErrors.Add(aName + " has an unknown wall material.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                End If
                If aFloorMaterial(i) <> "" Then
                    If myThermalProperties.GetIndex(aFloorMaterial(i)) < -1 Then
                        myErrors.Add(aName + " has an unknown floor material.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                End If
            Next
            ' If surfaces have thermal properties, we cannot also have adiabatic surfaces on
            If myThermalProperties.GetIndex(aCeilingMaterial(1)) >= 0 Or myThermalProperties.GetIndex(aWallMaterial(1)) >= 0 Or myThermalProperties.GetIndex(aFloorMaterial(1)) >= 0 Then
                If myEnvironment.AdiabaticWalls = True Then
                    myErrors.Add(aName + " has surfaces defined but adiabatic surfaces is also turned on. The inputs are incompatible.", ErrorMessages.TypeError)
                    HasErrors += 1
                End If
            End If
            ' Leakage areas must be between zero and 1
            If aWallLeak < 0 Or aWallLeak > 1 Or aFloorLeak < 0 Or aFloorLeak > 1 Then
                myErrors.Add(aName + " has leakage areas less then zero or greater than the total wall area.", ErrorMessages.TypeError)
                HasErrors += 1
            End If
            Return HasErrors
        End Get
    End Property
End Class
Public Class CompartmentCollection
    Inherits System.Collections.CollectionBase
    Public ReadOnly Maximum As Integer = Compartment.MaximumCompartments
    Private i As Integer, j As Integer
    Private HasErrors As Integer
    Public DoChange As Boolean = True
    Public Sub Add(ByVal aCompartment As Compartment)
        List.Add(aCompartment)
    End Sub
    Public Sub Remove(ByVal index As Integer)
        If index >= 0 Or index <= Count - 1 Then
            If index < Count - 1 Then
                For i = index To Count - 2
                    myHVents.Renumber(i + 1, i)
                    myVVents.Renumber(i + 1, i)
                    myMVents.Renumber(i + 1, i)
                    myTargets.Renumber(i + 1, i)
                    myDetectors.Renumber(i + 1, i)
                    myHHeats.Renumber(i + 1, i)
                    myVHeats.Renumber(i + 1, i)
                    myFireProperties.Renumber(i + 1, i)
                    myVisuals.Renumber(i + 1, i)
                Next
            End If
            List.RemoveAt(index)
        End If
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromCompartment As New Compartment, ToCompartment As New Compartment
        FromCompartment = CType(List.Item(indexFrom), Compartment)
        ToCompartment.Name = FromCompartment.Name
        ToCompartment.RoomHeight = FromCompartment.RoomHeight
        ToCompartment.RoomWidth = FromCompartment.RoomWidth
        ToCompartment.RoomDepth = FromCompartment.RoomDepth
        ToCompartment.RoomOriginX = FromCompartment.RoomOriginX
        ToCompartment.RoomOriginY = FromCompartment.RoomOriginY
        ToCompartment.RoomOriginZ = FromCompartment.RoomOriginZ
        For i = 1 To 3
            ToCompartment.CeilingMaterial(i) = FromCompartment.CeilingMaterial(i)
            ToCompartment.WallMaterial(i) = FromCompartment.WallMaterial(i)
            ToCompartment.FloorMaterial(i) = FromCompartment.FloorMaterial(i)
        Next
        Dim Vector1() As Double = {0}, Vector2() As Double = {0}, aNum As Integer
        FromCompartment.GetVariableArea(Vector1, Vector2, aNum)
        ToCompartment.SetVariableArea(Vector1, Vector2)
        ToCompartment.Shaft = FromCompartment.Shaft
        ToCompartment.Hall = FromCompartment.Hall

        List.Item(indexTo) = ToCompartment
    End Sub
    Public Sub Swap(ByVal index1 As Integer, ByVal index2 As Integer)
        If index1 >= 0 And index1 < Count And index2 >= 0 And index2 < Count Then
            myHVents.Renumber(index1, Compartment.TempCompartment) : myHVents.Renumber(index2, index1) : myHVents.Renumber(Compartment.TempCompartment, index2)
            myVVents.Renumber(index1, Compartment.TempCompartment) : myVVents.Renumber(index2, index1) : myVVents.Renumber(Compartment.TempCompartment, index2)
            myMVents.Renumber(index1, Compartment.TempCompartment) : myMVents.Renumber(index2, index1) : myMVents.Renumber(Compartment.TempCompartment, index2)
            myTargets.Renumber(index1, Compartment.TempCompartment) : myTargets.Renumber(index2, index1) : myTargets.Renumber(Compartment.TempCompartment, index2)
            myDetectors.Renumber(index1, Compartment.TempCompartment) : myDetectors.Renumber(index2, index1) : myDetectors.Renumber(Compartment.TempCompartment, index2)
            myHHeats.Renumber(index1, Compartment.TempCompartment) : myHHeats.Renumber(index2, index1) : myHHeats.Renumber(Compartment.TempCompartment, index2)
            myVHeats.Renumber(index1, Compartment.TempCompartment) : myVHeats.Renumber(index2, index1) : myVHeats.Renumber(Compartment.TempCompartment, index2)
            myFireProperties.Renumber(index1, Compartment.TempCompartment) : myFireProperties.Renumber(index2, index1) : myFireProperties.Renumber(Compartment.TempCompartment, index2)
            myVisuals.Renumber(index1, Compartment.TempCompartment) : myVisuals.Renumber(index2, index1) : myVisuals.Renumber(Compartment.TempCompartment, index2)
            Dim temp As New Compartment
            temp = CType(List.Item(index2), Compartment)
            List.Item(index2) = List.Item(index1)
            List.Item(index1) = temp
        End If
    End Sub
    Default Public Property Item(ByVal index As Integer) As Compartment
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Compartment number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aComp As New Compartment
                Return aComp
            Else
                Return CType(List.Item(index), Compartment)
            End If
        End Get
        Set(ByVal Value As Compartment)
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Compartment number not found.")
            Else
                List.Item(index) = Value
            End If
        End Set
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aCompartment As Compartment
                For i = 0 To Count - 1
                    aCompartment = CType(List(i), Compartment)
                    If aCompartment.Changed Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            HasErrors = 0
            If Count > 0 Then
                ' Check each compartment for errors
                Dim aComp1 As Compartment, aComp2 As Compartment
                For i = 0 To Count - 1
                    aComp1 = CType(List(i), Compartment)
                    HasErrors += aComp1.IsValid
                Next
                ' Cannot have duplicate compartment names
                If Count > 1 Then
                    For i = 0 To myCompartments.Count - 2
                        aComp1 = myCompartments(i)
                        For j = i + 1 To myCompartments.Count - 1
                            aComp2 = myCompartments(j)
                            If aComp1.Name = aComp2.Name Then
                                myErrors.Add(aComp1.Name + " is used more than once as a compartment name. Duplicate names are not allowed.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        Next
                    Next
                End If
            End If
            Return HasErrors
        End Get
    End Property
    Public ReadOnly Property GetCompIndex(ByVal Shortname As String) As Integer
        Get
            If Count > 0 Then
                For i = 0 To Count - 1
                    If Item(i).Name = Shortname Then
                        Return i
                        Exit Property
                    End If
                Next
            End If
            Return -1
        End Get
    End Property
End Class
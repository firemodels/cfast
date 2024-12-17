Public Class Vent

    Friend Const TypeHVent As Integer = 0
    Friend Const TypeVVent As Integer = 1
    Friend Const TypeMVent As Integer = 2
    Friend Const TypeVHeat As Integer = 3
    Friend Const TypeHHeat As Integer = 4
    Friend Const ventFront As Integer = 1
    Friend Const ventRight As Integer = 2
    Friend Const ventBack As Integer = 3
    Friend Const ventLeft As Integer = 4
    Friend Const MaximumHVents As Integer = 10 * Compartment.MaximumCompartments
    Friend Const MaximumVVents As Integer = 10 * Compartment.MaximumCompartments
    Friend Const MaximumMVents As Integer = 10 * Compartment.MaximumCompartments
    Friend Const MaximumVHeats As Integer = 10 * Compartment.MaximumCompartments
    Friend Const MaximumHHeats As Integer = 10 * Compartment.MaximumCompartments
    Friend Const OpenbyTime As Integer = 0
    Friend Const OpenbyTemperature As Integer = 1
    Friend Const OpenbyFlux As Integer = 2
    Private Const minValueOpenType As Integer = 0
    Private Const maxValueOpenType As Integer = 2

    Public Const MaxPressure As Double = 8000.0
    Public Const default_min_cutoff_relp As Double = 200.0
    Public Const default_max_cutoff_relp As Double = 300.0

    ' All units within the class are assumed to be consistent and typically SI
    Private aVentType As Integer                ' Type of vent; 0 = wall vent flow, 1 = ceiling/floor vent flow, 2 = mechanical flow
    Private aFirstCompartment As Integer        ' First of compartments connected by vent
    Private aSecondCompartment As Integer       ' Second of compartments connected by vent
    Private aOffset As Double                   ' vent position along wall from end of first compartment for wall vents
    Private aOffsetX As Double                  ' Placement of vent for Smokeview visualization in X (width) direction for vertical and mechanical vents
    Private aOffsetY As Double                  ' Placement of vent for Smokeview visualization in Y (depth) direction for vertical and mechanical vents
    Private aWidth As Double                    ' Width of the wall vent
    Private aHeight As Double                   ' Height of the wall vent relative to the bottom of the vent
    Private aBottom As Double                   ' Bottom (bottom of vent) height from floor of first comparment for wall vents
    Private aCoeff As Double                    ' Optional vent flow coefficient for wall vents
    Private aFace As Integer                    ' Defines which wall on which to display vent in Smokeview, 1 for front, 2 for right, 3 for back, 4 for left
    Private aOpenType As Integer                ' Vent opening by time, temperature, or incident heat flux
    Private aOpenValue As Double                ' Vent opening criterion if by temperature or flux
    Private aOpenTarget As String               ' Associated target for vent opening by temperature or flux
    Private aInitialOpening As Double           ' Fraction vent is open at t=0 for flow vents
    Private aInitialOpeningTime As Double       ' Last time vent is open at initial opening fraction
    Private aFinalOpening As Double             ' EVENT vent opening fraction or HHEAT connected fraction
    Private aFinalOpeningTime As Double         ' EVENT vent opening times
    Private aRampTimePoints(0) As Double        ' Vent opening times
    Private aRampFractionPoints(0) As Double    ' Vent open fractions
    Private aArea As Double                     ' Cross-sectional area of vent for ceiling/floor vents
    Private aShape As Integer                   ' Ceiling/Floor vent shape, 1 for circular and 2 for square
    Private aFirstArea As Double                ' Mechanical vent opening size in first compartment
    Private aFirstCenterHeight As Double        ' Height of center of Mechanical vent opening
    Private aFirstOrientation As Integer        ' Orientation of Mechanical vent opening, 1 for vertical (on wall) and 2 for horizontal (ceiling or floor)
    Private aSecondArea As Double               ' Mechanical vent opening size in second compartment
    Private aSecondCenterHeight As Double       ' Height of center of Mechanical vent opening
    Private aSecondOrientation As Integer       ' Orientation of Mechanical vent opening, 1 for vertical (on wall) and 2 for horizontal (ceiling or floor)
    Private aFlowRate As Double                 ' Fan flow rate for Mechanical vents
    Private aBeginFlowDropoff As Double         ' Beginning backward pressure for flow dropoff in mechanical vents
    Private aZeroFlow As Double                 ' Backward pressure for zero flow in mechanical vents
    Private aFilterEfficiency As Double         ' EVENT Fraction of smoke and user-specified species that gets through filter
    Private aFilterTime As Double               ' EVENT begin filter operation time
    Private aChanged As Boolean = False         ' True when vent information has changed
    Private HasErrors As Integer = 0            ' Temporary variable to indicate whether there are errors in the specification
    Private aName As String                     ' User supplied vent name 
    Private aFYI As String                      ' Descriptor for additional user supplied information

    Public Sub New()
        aFirstCompartment = -2
        aSecondCompartment = -2
        aInitialOpening = 1.0
        aFinalOpening = 1.0
        aFinalOpeningTime = 0.0
        aShape = 1
        aFirstOrientation = 1
        aSecondOrientation = 1
        aBeginFlowDropoff = 200.0
        aZeroFlow = 300.0
        aFace = 1
        aInitialOpening = 1.0
        aFilterEfficiency = 0.0
        aFYI = ""
        aCoeff = 0.0
    End Sub
    Public Property VentType() As Integer
        Get
            Return aVentType
        End Get
        Set(ByVal Value As Integer)
            If Value <> aVentType Then
                aVentType = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Sub SetDefaultLocation()
        If aVentType = TypeHVent Then
            If aFirstCompartment >= 0 And aFirstCompartment <= myCompartments.Count Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                If aFace = 1 Or aFace = 3 Then
                    aOffset = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomWidth) / 2 - aWidth / 2
                Else
                    aOffset = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomDepth) / 2 - aWidth / 2
                End If
            End If
        ElseIf aVentType = TypeVVent Then
            If aFirstCompartment >= 0 And aFirstCompartment <= myCompartments.Count Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                aOffsetX = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomWidth) / 2
                aOffsetY = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomDepth) / 2
                aChanged = True
            ElseIf aFirstCompartment = -1 And aSecondCompartment >= 0 And aSecondCompartment <= myCompartments.Count Then
                Dim aCompartment As Compartment = myCompartments.Item(aSecondCompartment)
                aOffsetX = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomWidth) / 2
                aOffsetY = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomDepth) / 2
                aChanged = True
            End If
        ElseIf aVentType = TypeMVent Then
            If aFirstCompartment >= 0 And aFirstCompartment <= myCompartments.Count Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                aOffsetX = 0
                aOffsetY = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomDepth) / 2
                aChanged = True
            ElseIf aFirstCompartment = -1 And aSecondCompartment >= 0 And aSecondCompartment <= myCompartments.Count Then
                Dim aCompartment As Compartment = myCompartments.Item(aSecondCompartment)
                aOffsetX = 0
                aOffsetY = myUnits.Convert(UnitsNum.Length).ToSI(aCompartment.RoomDepth) / 2
                aChanged = True
            End If
        End If
    End Sub
    Public Property FirstCompartment() As Integer
        Get
            Return aFirstCompartment
        End Get
        Set(ByVal Value As Integer)
            If Value <> aFirstCompartment Then
                aFirstCompartment = Value
                If Value > -1 Then SetDefaultLocation()
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondCompartment() As Integer
        Get
            Return aSecondCompartment
        End Get
        Set(ByVal Value As Integer)
            If Value <> aSecondCompartment Then
                aSecondCompartment = Value
                If Value > -1 Then SetDefaultLocation()
                aChanged = True
            End If
        End Set
    End Property
    Public Property Offset() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aOffset)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aOffset Then
                aOffset = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property OffsetX() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aOffsetX)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aOffsetX Then
                aOffsetX = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property OffsetY() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aOffsetY)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aOffsetY Then
                aOffsetY = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Width() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aWidth)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aWidth Then
                aWidth = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                SetDefaultLocation()
                aChanged = True
            End If
        End Set
    End Property
    Public Property Bottom() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aBottom)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aBottom Then
                aBottom = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Height() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aHeight)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aHeight Then
                aHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Coeff() As Double
        Get
            Return aCoeff
        End Get
        Set(ByVal Value As Double)
            If Value <> aCoeff Then
                aCoeff = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property OpenType() As Integer
        Get
            Return aOpenType
        End Get
        Set(ByVal Value As Integer)
            If Value >= Vent.minValueOpenType And Value <= Vent.maxValueOpenType Then
                If aOpenType <> Value Then
                    aOpenType = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property OpenValue() As Double
        Get
            If aOpenType = OpenbyTime Then
                Return myUnits.Convert(UnitsNum.Time).FromSI(aOpenValue)
            ElseIf aOpenType = OpenbyTemperature Then
                Return myUnits.Convert(UnitsNum.Temperature).FromSI(aOpenValue)
            ElseIf aOpenType = OpenbyFlux Then
                Return myUnits.Convert(UnitsNum.HeatFlux).FromSI(aOpenValue)
            Else
                Return 0.0
            End If
        End Get
        Set(ByVal Value As Double)
            If aOpenType = OpenbyTime Then
                If aOpenValue <> myUnits.Convert(UnitsNum.Time).ToSI(Value) And Value >= 0.0 Then
                    aOpenValue = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                    aChanged = True
                End If
            ElseIf aOpenType = OpenbyTemperature Then
                If aOpenValue <> myUnits.Convert(UnitsNum.Temperature).ToSI(Value) Then
                    aOpenValue = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                    aChanged = True
                End If
            ElseIf aOpenType = OpenbyFlux Then
                If aOpenValue <> myUnits.Convert(UnitsNum.HeatFlux).ToSI(Value) And Value >= 0.0 Then
                    aOpenValue = myUnits.Convert(UnitsNum.HeatFlux).ToSI(Value)
                    aChanged = True
                End If
            End If

        End Set
    End Property
    Public Property Target() As String
        Get
            Return aOpenTarget
        End Get
        Set(ByVal Value As String)
            If Value <> aOpenTarget Then
                aOpenTarget = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property InitialOpeningTime() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aInitialOpeningTime)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aInitialOpeningTime Then
                aInitialOpeningTime = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property InitialOpening() As Double
        Get
            Return aInitialOpening
        End Get
        Set(ByVal Value As Double)
            If Value <> aInitialOpening Then
                aInitialOpening = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FinalOpening() As Double
        Get
            Return aFinalOpening
        End Get
        Set(ByVal Value As Double)
            If Value <> aFinalOpening Then
                aFinalOpening = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FinalOpeningTime() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aFinalOpeningTime)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aFinalOpeningTime Then
                aFinalOpeningTime = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Face() As Integer
        Get
            Return aFace
        End Get
        Set(ByVal Value As Integer)
            If Value <> aFace Then
                aFace = Value
                SetDefaultLocation()
                aChanged = True
            End If
        End Set
    End Property
    Public Property Area() As Double
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aArea)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Area).ToSI(Value) <> aArea Then
                aArea = myUnits.Convert(UnitsNum.Area).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Shape() As Integer
        Get
            Return aShape
        End Get
        Set(ByVal Value As Integer)
            If Value <> aShape Then
                aShape = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FirstArea() As Double
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aFirstArea)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Area).ToSI(Value) <> aFirstArea Then
                aFirstArea = myUnits.Convert(UnitsNum.Area).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property FirstCenterHeight() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aFirstCenterHeight)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aFirstCenterHeight Then
                aFirstCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property FirstOrientation() As Integer
        Get
            Return aFirstOrientation
        End Get
        Set(ByVal Value As Integer)
            If Value <> aFirstOrientation Then
                aFirstOrientation = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondArea() As Double
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aSecondArea)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Area).ToSI(Value) <> aSecondArea Then
                aSecondArea = myUnits.Convert(UnitsNum.Area).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondCenterHeight() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aSecondCenterHeight)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aSecondCenterHeight Then
                aSecondCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondOrientation() As Integer
        Get
            Return aSecondOrientation
        End Get
        Set(ByVal Value As Integer)
            If Value <> aSecondOrientation Then
                aSecondOrientation = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FlowRate() As Double
        Get
            Return myUnits.Convert(UnitsNum.Flowrate).FromSI(aFlowRate)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Flowrate).ToSI(Value) <> aFlowRate Then
                aFlowRate = myUnits.Convert(UnitsNum.Flowrate).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property BeginFlowDropoff() As Double
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aBeginFlowDropoff)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aBeginFlowDropoff Then
                If Value >= 0 Then
                    aBeginFlowDropoff = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
                    aChanged = True
                Else
                    aBeginFlowDropoff = default_min_cutoff_relp
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property ZeroFlow() As Double
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aZeroFlow)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aZeroFlow Then
                If Value >= 0 Then
                    aZeroFlow = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
                    aChanged = True
                Else
                    aZeroFlow = default_max_cutoff_relp
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property FilterEfficiency() As Double
        Get
            Return aFilterEfficiency * 100.0
        End Get
        Set(ByVal Value As Double)
            If Value <> aFilterEfficiency Then
                aFilterEfficiency = Value / 100.0
                aChanged = True
            End If
        End Set
    End Property
    Public Property FilterTime() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aFilterTime)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aFilterTime Then
                aFilterTime = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
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
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer, ByVal Fraction As Double)
        ' Wall heat transfer connection of two compartments by a specified fraction
        aVentType = TypeHHeat
        aFirstCompartment = FirstCompartment
        aSecondCompartment = SecondCompartment
        aInitialOpening = Fraction
    End Sub
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer)
        ' Ceiling/Floor heat transfer connection of two compartments
        aVentType = TypeVHeat
        aFirstCompartment = FirstCompartment
        aSecondCompartment = SecondCompartment
        aInitialOpening = 1.0
    End Sub
    Public Sub GetVent(ByRef FirstCompartment As Integer, ByRef SecondCompartment As Integer, ByRef Width As Double, ByRef Height As Double, ByRef Bottom As Double)
        ' Wall vent connection
        FirstCompartment = Me.FirstCompartment
        SecondCompartment = Me.SecondCompartment
        Width = Me.Width
        Height = Me.Height
        Bottom = Me.Bottom
    End Sub
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer, ByVal Width As Double, ByVal Height As Double, ByVal Bottom As Double, Top As Double)
        ' Wall vent connection
        aVentType = TypeHVent
        aFirstCompartment = FirstCompartment
        aSecondCompartment = SecondCompartment
        aWidth = myUnits.Convert(UnitsNum.Length).ToSI(Width)
        ' you can specify any two of bottom, top, and height but not all three. If all three, we ignore one
        If Height > 0 Then
            If Bottom < 0 Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                Bottom = aCompartment.RoomHeight - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Bottom))
            ElseIf Bottom = 0 And Top < 0 Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                Bottom = aCompartment.RoomHeight - Height - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Top))
            ElseIf Bottom = 0 And Top > 0 Then
                Bottom = Top - Height
            End If
        ElseIf Height = 0 Then
            If Bottom < 0 Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                Bottom = aCompartment.RoomHeight - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Bottom))
            End If
            If Top < 0 Then
                Dim aCompartment As Compartment = myCompartments.Item(aFirstCompartment)
                Top = aCompartment.RoomHeight - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Top))
            End If
            Height = Top - Bottom
        End If
        aHeight = myUnits.Convert(UnitsNum.Length).ToSI(Height)
        aBottom = myUnits.Convert(UnitsNum.Length).ToSI(Bottom)
        aFace = 1
        SetDefaultLocation()
    End Sub
    Public Sub GetVent(ByVal TopCompartment As Integer, ByVal BottomCompartment As Integer, ByVal Area As Double, ByVal Shape As Integer)
        ' Ceiling/Floor vent connection
        TopCompartment = FirstCompartment
        BottomCompartment = SecondCompartment
        Area = Me.Area
        Shape = Me.Shape
    End Sub
    Public Sub SetVent(ByVal TopCompartment As Integer, ByVal BottomCompartment As Integer, ByVal Area As Double, ByVal Shape As Integer)
        ' Ceiling/Floor vent connection
        VentType = TypeVVent
        FirstCompartment = TopCompartment
        SecondCompartment = BottomCompartment
        Me.Area = Area
        Me.Shape = Shape
        SetDefaultLocation()
    End Sub
    Public Sub GetVent(ByRef FromCompartment As Integer, ByRef FromArea As Double, ByRef FromCenterHeight As Double, ByRef FromOrientation As String, ByRef ToCompartment As Integer, ByRef ToArea As Double, ByRef ToCenterHeight As Double, ByRef ToOrientation As String, ByRef FlowRate As Double, ByRef BeginFlowDropoff As Double, ByRef ZeroFlow As Double)
        ' Mechanical vent connection
        aVentType = TypeMVent
        FromCompartment = FirstCompartment
        FromArea = FirstArea
        FromCenterHeight = FirstCenterHeight
        If aFirstOrientation = 2 Then
            FromOrientation = "H"
        Else
            FromOrientation = "V"
        End If
        ToCompartment = aSecondCompartment
        ToArea = SecondArea
        ToCenterHeight = SecondCenterHeight
        If aSecondOrientation = 2 Then
            ToOrientation = "H"
        Else
            ToOrientation = "V"
        End If
        FlowRate = Me.FlowRate
        BeginFlowDropoff = Me.BeginFlowDropoff
        ZeroFlow = Me.ZeroFlow
    End Sub
    Public Sub SetVent(ByVal FromCompartment As Integer, ByVal FromArea As Double, ByVal FromCenterHeight As Double, ByVal FromOrientation As String, ByVal ToCompartment As Integer, ByVal ToArea As Double, ByVal ToCenterHeight As Double, ByVal ToOrientation As String, ByVal FlowRate As Double, ByVal BeginFlowDropoff As Double, ByVal ZeroFlow As Double)
        ' Mechanical vent connection
        aVentType = TypeMVent
        aFirstCompartment = FromCompartment
        aFirstArea = myUnits.Convert(UnitsNum.Area).ToSI(FromArea)
        aFirstCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(FromCenterHeight)
        If FromOrientation = "H" Then
            aFirstOrientation = 2
        Else
            aFirstOrientation = 1
        End If
        aSecondCompartment = ToCompartment
        aSecondArea = myUnits.Convert(UnitsNum.Area).ToSI(ToArea)
        aSecondCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(ToCenterHeight)
        If ToOrientation = "H" Then
            aSecondOrientation = 2
        Else
            aSecondOrientation = 1
        End If
        aFlowRate = FlowRate
        aBeginFlowDropoff = myUnits.Convert(UnitsNum.Pressure).ToSI(BeginFlowDropoff)
        aZeroFlow = myUnits.Convert(UnitsNum.Pressure).ToSI(ZeroFlow)
        SetDefaultLocation()
    End Sub
    Public Sub GetRamp(ByRef TimePoints() As Double, ByRef FractionPoints() As Double, ByRef NumPoints As Integer)
        Dim i As Integer
        NumPoints = 0
        If aRampTimePoints.GetLength(0) = aRampFractionPoints.GetLength(0) Then
            ReDim TimePoints(aRampTimePoints.GetUpperBound(0)), FractionPoints(aRampFractionPoints.GetUpperBound(0))
            For i = 0 To aRampTimePoints.GetUpperBound(0)
                TimePoints(i) = myUnits.Convert(UnitsNum.Time).FromSI(aRampTimePoints(i))
                FractionPoints(i) = aRampFractionPoints(i)
            Next
            NumPoints = aRampTimePoints.GetUpperBound(0)
        End If
    End Sub
    Public Sub SetRamp(ByVal TimePoints() As Double, ByVal FractionPoints() As Double)
        Dim i As Integer
        If TimePoints.GetLength(0) = FractionPoints.GetLength(0) Then
            ReDim aRampTimePoints(TimePoints.GetUpperBound(0)), aRampFractionPoints(FractionPoints.GetUpperBound(0))
            For i = 0 To TimePoints.GetUpperBound(0)
                aRampTimePoints(i) = myUnits.Convert(UnitsNum.Time).ToSI(TimePoints(i))
                aRampFractionPoints(i) = FractionPoints(i)
            Next
            aChanged = True
        End If
    End Sub
    Public Sub GetRampFractions(ByRef FractionPoints() As Double)
        Dim i As Integer
        ReDim FractionPoints(aRampFractionPoints.GetUpperBound(0))
        For i = 0 To FractionPoints.GetUpperBound(0)
            FractionPoints(i) = aRampFractionPoints(i)
        Next
    End Sub
    Public Sub GetRampTimes(ByRef TimePoints() As Double)
        Dim i As Integer
        ReDim TimePoints(aRampTimePoints.GetUpperBound(0))
        For i = 0 To TimePoints.GetUpperBound(0)
            TimePoints(i) = myUnits.Convert(UnitsNum.Time).FromSI(aRampTimePoints(i))
        Next
    End Sub
    Public Sub SetRampFractions(ByVal FractionPoints() As Double)
        Dim i As Integer
        ReDim aRampFractionPoints(FractionPoints.GetUpperBound(0))
        For i = 0 To FractionPoints.GetUpperBound(0)
            aRampFractionPoints(i) = FractionPoints(i)
        Next
        aChanged = True
    End Sub
    Public Sub SetRampTimes(ByVal TimePoints() As Double)
        Dim i As Integer
        ReDim aRampTimePoints(TimePoints.GetUpperBound(0))
        For i = 0 To TimePoints.GetUpperBound(0)
            aRampTimePoints(i) = myUnits.Convert(UnitsNum.Time).ToSI(TimePoints(i))
        Next
        aChanged = True
    End Sub
    Public ReadOnly Property IsValid(ByVal VentNumber As Integer) As Integer
        Get
            myUnits.SI = True
            HasErrors = 0

            Dim cVentType As String = ""
            If aVentType = TypeHVent Then cVentType = "Horizontal"
            If aVentType = TypeVVent Then cVentType = "Vertical"
            If aVentType = TypeMVent Then cVentType = "Mechanical"
            ' check opening and closing specifications
            If aVentType = TypeHVent Or aVentType = TypeVVent Or aVentType = TypeMVent Then
                Select Case aOpenType
                    Case OpenbyTime
                        If aInitialOpeningTime < 0 Or aInitialOpeningTime > myEnvironment.SimulationTime Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " initial opening time is less than zero or greater than simulation time.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aFinalOpeningTime < 0 Or aFinalOpeningTime > myEnvironment.SimulationTime Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " final opening time is less than zero or greater than simulation time.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aFinalOpeningTime < aInitialOpeningTime Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " final opening time is less than initial opening time.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                    Case OpenbyTemperature
                        If aOpenValue < myEnvironment.IntAmbTemperature Or aOpenValue > 1273.15 Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " opening temperature is less than ambient temperature or greater than 1000 C", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                    Case OpenbyFlux
                        If aOpenValue < 0 Or aOpenValue > 150000 Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " opening incident flux is less than 0 or greater than 150 kW/m^2", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                    Case Else
                        If aVentType < 0 Or aVentType > 2 Then
                            myErrors.Add(cVentType + " flow vent " + VentNumber.ToString + " opening type must be Time, Temp, or Flux.", ErrorMessages.TypeError)
                            HasErrors += 1
                        End If
                End Select
                If aInitialOpening < 0 Or aInitialOpening > 1 Or aFinalOpening < 0 Or aFinalOpening > 1 Then
                    myErrors.Add(cVentType + " flow vent" + VentNumber.ToString + " opening fraction is less than 0 or greater than 1", ErrorMessages.TypeFatal)
                    HasErrors += 1
                End If
                If aRampTimePoints.GetUpperBound(0) > 0 Then
                    If aRampFractionPoints(0) < 0 Or aRampFractionPoints(0) > 1 Then
                        myErrors.Add(cVentType + " flow vent" + VentNumber.ToString + " opening fraction is less than 0 or greater than 1", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    Dim i As Integer
                    For i = 1 To aRampTimePoints.GetUpperBound(0)
                        If aRampTimePoints(i) < aRampTimePoints(i - 1) Then
                            myErrors.Add(cVentType + " flow vent" + VentNumber.ToString + " time points must increase with time", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aRampFractionPoints(i) < 0 Or aRampFractionPoints(i) > 1 Then
                            myErrors.Add(cVentType + " flow vent" + VentNumber.ToString + " opening fraction is less than 0 or greater than 1", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    Next
                End If
            End If
            ' check vent size, position, and parameters specific to each vent type
            Select Case aVentType
                Case TypeHVent
                    If aCoeff < 0.0 Or aCoeff > 1.0 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + " has an invalid vent flow coefficient. Value must be greater than zero and less than 1", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment > aSecondCompartment And aFirstCompartment >= 0 And aSecondCompartment >= 0 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Compartment order is incorrect. Wall vents must always be connected with lower numbered vent first.", ErrorMessages.TypeError)
                        HasErrors += 1
                    ElseIf aFirstCompartment = -1 And aSecondCompartment >= 0 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Compartment order is incorrect. Outside must always be second compartment.", ErrorMessages.TypeError)
                        HasErrors += 1
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aBottom < 0.0 Or aBottom > aComp1.RoomHeight Then
                            myErrors.Add("Wall vent " + VentNumber.ToString + ". Vent Bottom is below floor level or above ceiling level of first compartment.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aBottom < 0 Or aHeight < 0 Or aBottom + aHeight < 0.0 Or aBottom + aHeight > aComp1.RoomHeight Then
                            myErrors.Add("Wall vent " + VentNumber.ToString + ". Vent top is below  floor level or above ceiling level of first compartment.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aWidth <= 0.0 Or aWidth > Math.Max(aComp1.RoomWidth, aComp1.RoomDepth) Then
                            myErrors.Add("Wall vent " + VentNumber.ToString + ". Width is less than 0 or greater than compartment dimensions.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                            Dim aComp2 As New Compartment
                            aComp2 = myCompartments(aSecondCompartment)
                            If aBottom + aComp1.RoomOriginZ < aComp2.RoomOriginZ Or aBottom + aHeight + aComp1.RoomOriginZ > aComp2.RoomOriginZ + aComp2.RoomHeight Then
                                myErrors.Add("Wall vent " + VentNumber.ToString + ". Vent top is below floor level or above ceiling level of second compartment.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                    End If
                    If aInitialOpening < 0.0 Or aInitialOpening > 1.0 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Initial opening fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFinalOpening < 0.0 Or aFinalOpening > 1.0 Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Final opening fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFinalOpeningTime < 0.0 Or aFinalOpeningTime > myEnvironment.SimulationTime Then
                        myErrors.Add("Wall vent " + VentNumber.ToString + ". Final opening time is less than 0 or greater than simulation time.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                Case TypeVVent
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aOffsetX < 0 Or aOffsetX > aComp1.RoomWidth Then
                            myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + ". Width (X) offset is less than 0 or greater than compartment width.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aOffsetY < 0 Or aOffsetY > aComp1.RoomDepth Then
                            myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + ". Depth (Y) offset is less than 0 or greater than compartment depth.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aArea <= 0 Or aArea > aComp1.RoomWidth * aComp1.RoomDepth Then
                            myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + ". Cross-sectional area is less than 0 or greater than compartment floor area.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                            Dim aComp2 As New Compartment
                            aComp2 = myCompartments(aSecondCompartment)
                            If Math.Abs(aComp1.RoomOriginZ - (aComp2.RoomOriginZ + aComp2.RoomHeight)) > 0.1 Then
                                myErrors.Add("Ceiling/Floor vent " + VentNumber.ToString + ". Floor of the top compartment is above ceiling of the bottom compartment.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                    End If
                Case TypeMVent
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aBeginFlowDropoff < 0.0 Or aBeginFlowDropoff >= MaxPressure Or aZeroFlow < 0 Or aZeroFlow >= MaxPressure Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Fan pressure constants are less than 0 Pa or greater than " + MaxPressure.ToString + " Pa.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aBeginFlowDropoff >= aZeroFlow Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Fan pressure constant where flow drop off begins is greater than fan pressure constant at zero flow.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFirstArea <= 0.0 Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser area is less than 0 m� in size in from compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstArea > 1.0 Then
                        If Data.Update = False Then
                            myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser area is greater than 1 m� in size in from compartment.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                    End If
                    If aSecondArea <= 0.0 Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser area is less than 0 m� in size in to compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aSecondArea > 1.0 Then
                        If Data.Update = False Then
                            myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser area is greater than 1 m� in size in to compartment.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        myUnits.SI = True
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aOffsetX < 0 Or aOffsetX > aComp1.RoomWidth Then
                            myErrors.Add("Mechnical flow vent " + VentNumber.ToString + ". Width (X) offset is less than 0 or greater than compartment width.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aOffsetY < 0 Or aOffsetY > aComp1.RoomDepth Then
                            myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Depth (Y) offset is less than 0 or greater than compartment depth.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        If aFirstOrientation = 1 Then
                            If aFirstCenterHeight - Math.Sqrt(aFirstArea) / 2 < 0.0 Or Math.Sqrt(aFirstArea) / 2 + aFirstCenterHeight > aComp1.RoomHeight Then
                                myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser area is below floor level or above ceiling level in from compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        If aFlowRate > 10 * aComp1.RoomWidth * aComp1.RoomDepth * aComp1.RoomHeight / 3600.0 Then
                            If Data.Update = False Then
                                myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Flowrate is more than 10 air changes per hour out of compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        myUnits.SI = False
                    End If
                    If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                        myUnits.SI = True
                        Dim aComp2 As New Compartment
                        aComp2 = myCompartments(aSecondCompartment)
                        If aSecondOrientation = 1 Then
                            If aSecondCenterHeight - Math.Sqrt(aSecondArea) / 2 < 0.0 Or Math.Sqrt(aSecondArea) / 2 + aSecondCenterHeight > aComp2.RoomHeight Then
                                myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Vent diffuser is below floor level or above ceiling level in to compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        If aFlowRate > 10 * aComp2.RoomWidth * aComp2.RoomDepth * aComp2.RoomHeight / 3600.0 Then
                            If Data.Update = False Then
                                myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Flowrate is more than 10 air changes per hour into compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        myUnits.SI = False
                    End If
                    If aFilterEfficiency < 0.0 Or aFilterEfficiency > 1.0 Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Filter transmission fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFilterTime < 0.0 Or aFilterTime > myEnvironment.SimulationTime Then
                        myErrors.Add("Mechanical vent " + VentNumber.ToString + ". Filter operation time is less than 0 or greater than simulation time.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                Case TypeHHeat
                    Dim cFirst, cSecond As New Compartment
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    Else
                        cFirst = myCompartments(aFirstCompartment)
                        cSecond = myCompartments(aSecondCompartment)
                        If cFirst.WallMaterial(1) = "Off" Or cSecond.WallMaterial(1) = "Off" Then
                            myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Thermal properties for wall surfaces cannot be set to Off.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    End If
                    If aInitialOpening < 0.0 Or aInitialOpening > 1.0 Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Fraction of connected surface area is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                Case TypeVHeat
                    Dim cTop, cBottom As New Compartment
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    Else
                        cTop = myCompartments(aFirstCompartment)
                        cBottom = myCompartments(aSecondCompartment)
                        If cTop.FloorMaterial(1) = "Off" Or cBottom.CeilingMaterial(1) = "Off" Then
                            myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Thermal properties for floor and ceiling surfaces cannot be set to Off.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    End If
                    If aInitialOpening < 0.0 Or aInitialOpening > 1.0 Then
                        myErrors.Add("Heat transfer connection " + VentNumber.ToString + ". Fraction of connected surface area is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
            End Select
            myUnits.SI = False
            Return HasErrors
        End Get
    End Property
End Class
Public Class VentCollection
    Inherits System.Collections.CollectionBase
    Private i As Integer, VentType As Integer
    Private HasError As Integer
    Public Sub Add(ByVal aVent As Vent)
        List.Add(aVent)
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromVent As New Vent, ToVent As New Vent
        FromVent = CType(List.Item(indexFrom), Vent)
        ToVent.VentType = FromVent.VentType
        ToVent.FirstCompartment = FromVent.FirstCompartment
        ToVent.Offset = FromVent.Offset
        ToVent.SecondCompartment = FromVent.SecondCompartment
        ToVent.Width = FromVent.Width
        ToVent.Height = FromVent.Height
        ToVent.Bottom = FromVent.Bottom
        ToVent.InitialOpening = FromVent.InitialOpening
        ToVent.Face = FromVent.Face
        ToVent.FinalOpening = FromVent.FinalOpening
        ToVent.FinalOpeningTime = FromVent.FinalOpeningTime
        ToVent.Area = FromVent.Area
        ToVent.Shape = FromVent.Shape
        ToVent.FirstArea = FromVent.FirstArea
        ToVent.FirstCenterHeight = FromVent.FirstCenterHeight
        ToVent.FirstOrientation = FromVent.FirstOrientation
        ToVent.SecondArea = FromVent.SecondArea
        ToVent.SecondCenterHeight = FromVent.SecondCenterHeight
        ToVent.SecondOrientation = FromVent.SecondOrientation
        ToVent.FlowRate = FromVent.FlowRate
        ToVent.BeginFlowDropoff = FromVent.BeginFlowDropoff
        ToVent.ZeroFlow = FromVent.ZeroFlow
        ToVent.FilterEfficiency = FromVent.FilterEfficiency
        ToVent.FilterTime = FromVent.FilterTime
        ToVent.FinalOpening = FromVent.FinalOpening
        ToVent.FinalOpeningTime = FromVent.FinalOpeningTime
        ToVent.InitialOpeningTime = FromVent.InitialOpeningTime
        ToVent.OffsetX = FromVent.OffsetX
        ToVent.OffsetY = FromVent.OffsetY
        ToVent.OpenType = FromVent.OpenType
        ToVent.OpenValue = FromVent.OpenValue
        ToVent.Target = FromVent.Target
        ToVent.VentType = FromVent.VentType
        Dim Vector1() As Double = {0}, Vector2() As Double = {0}, aNum As Integer
        FromVent.GetRamp(Vector1, Vector2, aNum)
        ToVent.SetRamp(Vector1, Vector2)

        List.Item(indexTo) = ToVent
    End Sub
    Public Sub Swap(ByVal index1 As Integer, ByVal index2 As Integer)
        If index1 >= 0 And index1 < Count And index2 >= 0 And index2 < Count Then
            Dim temp As New Vent
            temp = CType(List.Item(index2), Vent)
            List.Item(index2) = List.Item(index1)
            List.Item(index1) = temp
        End If
    End Sub
    Public Sub Remove(ByVal index As Integer)
        ' make sure that the vent number is valid
        If index >= 0 And index < Count Then
            List.RemoveAt(index)
        End If
    End Sub
    Public Sub RemoveAll(ByVal index As Integer)
        ' Removes all vents connected to the compartment specified by index
        If index >= 0 And index < myCompartments.Count Then
            Dim aVent As New Vent
            i = 0
            While i < Count
                aVent = CType(List.Item(i), Vent)
                If aVent.FirstCompartment = index Or aVent.SecondCompartment = index Then
                    List.RemoveAt(i)
                    i = 0
                Else
                    i += 1
                End If
            End While

        End If
    End Sub
    Public Sub Renumber(ByVal Oldindex As Integer, ByVal Newindex As Integer)
        Dim aVent As New Vent
        For i = 0 To Count - 1
            aVent = CType(List.Item(i), Vent)
            If aVent.FirstCompartment = Oldindex Then aVent.FirstCompartment = Newindex
            If aVent.SecondCompartment = Oldindex Then aVent.SecondCompartment = Newindex
            List.Item(i) = aVent
        Next
    End Sub
    Public ReadOnly Property Maximum() As Integer
        Get
            If Count > 0 Then
                VentType = CType(List.Item(0), Vent).VentType
                If VentType = Vent.TypeHVent Then Return Vent.MaximumHVents
                If VentType = Vent.TypeVVent Then Return Vent.MaximumVVents
                If VentType = Vent.TypeMVent Then Return Vent.MaximumMVents
                If VentType = Vent.TypeHHeat Then Return Vent.MaximumHHeats
                If VentType = Vent.TypeVHeat Then Return Vent.MaximumVHeats
            Else
                Return 1
            End If
        End Get
    End Property
    Default Public Property Item(ByVal index As Integer) As Vent
        Get
            If index >= 0 And index < Count Then
                Return CType(List.Item(index), Vent)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Vent number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aVent As New Vent
                Return aVent
            End If
        End Get
        Set(ByVal Value As Vent)
            List.Item(index) = Value
        End Set
    End Property
    Public ReadOnly Property NumberofConnections(ByVal index As Integer) As Integer
        Get
            Dim aVent As Vent
            Dim numVents As Integer = 0
            numVents = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aVent = CType(List.Item(i), Vent)
                    If aVent.FirstCompartment = index Or aVent.SecondCompartment = index Then numVents += 1
                Next
            End If
            Return numVents
        End Get
    End Property
    Public ReadOnly Property FromConnections(ByVal index As Integer) As Integer
        Get
            Dim aVent As Vent
            Dim numVents As Integer = 0
            numVents = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aVent = CType(List.Item(i), Vent)
                    If aVent.FirstCompartment = index Then numVents += 1
                Next
            End If
            Return numVents
        End Get
    End Property
    Public ReadOnly Property ToConnections(ByVal index As Integer) As Integer
        Get
            Dim aVent As Vent
            Dim numVents As Integer = 0
            numVents = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aVent = CType(List.Item(i), Vent)
                    If aVent.SecondCompartment = index Then numVents += 1
                Next
            End If
            Return numVents
        End Get
    End Property
    Public ReadOnly Property ConnectedFraction(ByVal index As Integer) As Double
        Get
            Dim aVent As Vent
            Dim TotalFraction As Double = 0
            If Count > 0 And FromConnections(index) <> 0 Then
                For i = 0 To Count - 1
                    aVent = CType(List.Item(i), Vent)
                    If aVent.FirstCompartment = index Then TotalFraction += aVent.InitialOpening
                Next
            End If
            Return TotalFraction
        End Get
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aVent As Vent
                For i = 0 To Count - 1
                    aVent = CType(List.Item(i), Vent)
                    If aVent.Changed Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property VentNumber(ByVal index As Integer) As Integer
        Get
            Dim aVent, tmpVent As Vent
            Dim i As Integer
            Dim numVents As Integer = 0
            aVent = List.Item(index)
            For i = 0 To index - 1
                tmpVent = List.Item(i)
                If tmpVent.FirstCompartment = aVent.FirstCompartment Or tmpVent.SecondCompartment = aVent.FirstCompartment Then
                    If tmpVent.FirstCompartment = aVent.SecondCompartment Or tmpVent.SecondCompartment = aVent.SecondCompartment Then
                        numVents += 1
                    End If
                End If
            Next
            numVents += 1
            Return numVents
        End Get
    End Property
    Public ReadOnly Property GetIndex(ByVal compNum1 As Integer, ByVal compNum2 As Integer, ByVal ventNum As Integer) As Integer
        Get
            Dim aVent As Vent
            Dim i As Integer
            Dim numVents As Integer = 0
            For i = 0 To List.Count - 1
                aVent = List.Item(i)
                If aVent.VentType = Vent.TypeMVent Then
                    If compNum1 = aVent.FirstCompartment And compNum2 = aVent.SecondCompartment And ventNum = i + 1 Then
                        Return i
                    End If
                Else
                    If compNum1 = aVent.FirstCompartment Or compNum2 = aVent.FirstCompartment Then
                        If compNum1 = aVent.SecondCompartment Or compNum2 = aVent.SecondCompartment Then
                            numVents += 1
                            If numVents = ventNum Then Return i
                        End If
                    End If
                End If
            Next
            Return -1
        End Get
    End Property
    Public ReadOnly Property GetVentIndex(ByVal name As String) As Integer
        Get
            Dim i, idx As Integer
            Dim aVent As Vent
            idx = -1
            For i = 0 To List.Count - 1
                aVent = List.Item(i)
                If aVent.Name = name Then
                    idx = i
                    Exit For
                End If
            Next
            Return idx
        End Get
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            Dim FractionTotal As Double, i As Integer
            HasError = 0
            If Count > 0 Then
                Dim aVent As Vent
                For i = 0 To Count - 1
                    aVent = CType(List(i), Vent)
                    HasError += aVent.IsValid(i + 1)

                    ' We have to check total connected area here since we only know compartment number outside of IsValid for the vent
                    If aVent.VentType = Vent.TypeHHeat And FromConnections(aVent.FirstCompartment) <> 0 Then
                        FractionTotal = ConnectedFraction(aVent.FirstCompartment)
                        If FractionTotal > 1.0 Then
                            myErrors.Add("Horizontal heat transfer connections to compartment " + (aVent.FirstCompartment + 1).ToString + ". Fraction of connected surface area is greater than 1.", ErrorMessages.TypeError)
                            HasError += 1
                        ElseIf FractionTotal > 0.0 And FractionTotal < 1.0 Then
                            myErrors.Add("Horizontal heat transfer connections to compartment " + (aVent.FirstCompartment + 1).ToString + ". Fraction of connected surface area is less than 1. Remaining fraction will be allocated to the outside.", ErrorMessages.TypeWarning)
                            HasError += 1
                        End If
                    End If

                Next
            End If
            Return HasError
        End Get
    End Property
End Class
Public Class Vent

    Friend Const TypeHVent As Integer = 0
    Friend Const TypeVVent As Integer = 1
    Friend Const TypeMVent As Integer = 2
    Friend Const TypeVHeat As Integer = 3
    Friend Const TypeHHeat As Integer = 4
    Friend Const MaximumHVents As Integer = 100
    Friend Const MaximumVVents As Integer = 100
    Friend Const MaximumMVents As Integer = 100
    Friend Const MaximumVHeats As Integer = 60
    Friend Const MaximumHHeats As Integer = 120

    Public Const MaxPressure As Single = 8000.0

    ' All units within the class are assumed to be consistent and typically SI
    Private aVentType As Integer                ' Type of vent; 0 = horizontal flow, 1 = vertical flow, 2 = mechanical flow
    Private aFirstCompartment As Integer        ' First of compartments connected by vent
    Private aFirstOffset As Single              ' vent position along wall from end of first compartment for horizontal flow vents
    Private aSecondCompartment As Integer       ' Second of compartments connected by vent
    Private aSecondOffset As Single             ' Vent position along wall from end of second compartment for horizontal flow vents
    Private aWidth As Single                    ' Width of the horizontal flow vent
    Private aSoffit As Single                   ' Soffit (top of vent) height from floor of first compartment for horizontal flow vents
    Private aSill As Single                     ' Sill (bottom of vent) height from floor of first comparment for horizontal flow vents
    Private aWindCosine As Single                ' Wind coefficient (cosine of angle of wind vector and vent opening) for horizontal flow vents
    Private aInitialOpening As Single           ' Fraction vent is open at t=0 for horizontal flow vents
    Private aFace As Integer                    ' Defines which wall on which to display vent in Smokeview, 1 for front, 2 for left, 3 for back, 4 for right
    Private aFinalOpening As Single             ' EVENT vent opening fraction or HHEAT connected fraction
    Private aFinalOpeningTime As Single         ' EVENT vent opening times
    Private aArea As Single                     ' Cross-sectional area of vent for vertical flow vents
    Private aShape As Integer                   ' Vertical flow vent shape, 1 for circular and 2 for square
    Private aFirstArea As Single                ' Mechanical vent opening size in first compartment
    Private aFirstCenterHeight As Single        ' Height of center of mechanical flow vent opening
    Private aFirstOrientation As Integer        ' Orientation of mechanical flow vent opening, 1 for vertical (on wall) and 2 for horizontal (ceiling or floor)
    Private aSecondArea As Single               ' Mechanical vent opening size in second compartment
    Private aSecondCenterHeight As Single       ' Height of center of mechanical flow vent opening
    Private aSecondOrientation As Integer       ' Orientation of mechanical flow vent opening, 1 for vertical (on wall) and 2 for horizontal (ceiling or floor)
    Private aFlowRate As Single                 ' Fan flow rate for mechanical flow vents
    Private aBeginFlowDropoff As Single         ' Beginning backward pressure for flow dropoff in mechanical vents
    Private aZeroFlow As Single                 ' Backward pressure for zero flow in mechanical vents
    Private aFilterEfficiency As Single         ' EVENT Fraction of smoke and user-specified species that gets through filter
    Private aFilterTime As Single               ' EVENT begin filter operation time
    Private aChanged As Boolean = False         ' True once compartment information has changed
    Private HasErrors As Integer = 0            ' Temporary variable to indicate whether there are errors in the specification

    Public Sub New()
        aFirstCompartment = -2
        aSecondCompartment = -2
        aWindCosine = 1.0
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
    Public Property FirstCompartment() As Integer
        Get
            Return aFirstCompartment
        End Get
        Set(ByVal Value As Integer)
            If Value <> aFirstCompartment Then
                aFirstCompartment = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FirstOffset() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aFirstOffset)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aFirstOffset Then
                aFirstOffset = myUnits.Convert(UnitsNum.Length).ToSI(Value)
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
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondOffset() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aSecondOffset)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aSecondOffset Then
                aSecondOffset = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Width() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aWidth)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aWidth Then
                aWidth = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Sill() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aSill)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aSill Then
                aSill = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Soffit() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aSoffit)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aSoffit Then
                aSoffit = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property WindAngle() As Single
        Get
            Return Math.Acos(aWindCosine) * 180.0 / Math.PI
        End Get
        Set(ByVal Value As Single)
            If Math.Cos(Value * Math.PI / 180.0) <> aWindCosine Then
                aWindCosine = Math.Cos(Value * Math.PI / 180.0)
                aChanged = True
            End If
        End Set
    End Property
    Public Property WindCosine() As Single
        Get
            Return aWindCosine
        End Get
        Set(ByVal Value As Single)
            If Value <> aWindCosine Then
                aWindCosine = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property InitialOpening() As Single
        Get
            Return aInitialOpening
        End Get
        Set(ByVal Value As Single)
            If Value <> aInitialOpening Then
                aInitialOpening = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FinalOpening() As Single
        Get
            Return aFinalOpening
        End Get
        Set(ByVal Value As Single)
            If Value <> aFinalOpening Then
                aFinalOpening = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property FinalOpeningTime() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aFinalOpeningTime)
        End Get
        Set(ByVal Value As Single)
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
                aChanged = True
            End If
        End Set
    End Property
    Public Property Area() As Single
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aArea)
        End Get
        Set(ByVal Value As Single)
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
    Public Property FirstArea() As Single
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aFirstArea)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Area).ToSI(Value) <> aFirstArea Then
                aFirstArea = myUnits.Convert(UnitsNum.Area).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property FirstCenterHeight() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aFirstCenterHeight)
        End Get
        Set(ByVal Value As Single)
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
    Public Property SecondArea() As Single
        Get
            Return myUnits.Convert(UnitsNum.Area).FromSI(aSecondArea)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Area).ToSI(Value) <> aSecondArea Then
                aSecondArea = myUnits.Convert(UnitsNum.Area).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property SecondCenterHeight() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aSecondCenterHeight)
        End Get
        Set(ByVal Value As Single)
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
    Public Property FlowRate() As Single
        Get
            Return myUnits.Convert(UnitsNum.Flowrate).FromSI(aFlowRate)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Flowrate).ToSI(Value) <> aFlowRate Then
                aFlowRate = myUnits.Convert(UnitsNum.Flowrate).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property BeginFlowDropoff() As Single
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aBeginFlowDropoff)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aBeginFlowDropoff Then
                aBeginFlowDropoff = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property ZeroFlow() As Single
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aZeroFlow)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aZeroFlow Then
                aZeroFlow = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property FilterEfficiency() As Single
        Get
            Return aFilterEfficiency * 100.0
        End Get
        Set(ByVal Value As Single)
            If Value <> aFilterEfficiency Then
                aFilterEfficiency = Value / 100.0
                aChanged = True
            End If
        End Set
    End Property
    Public Property FilterTime() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aFilterTime)
        End Get
        Set(ByVal Value As Single)
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
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer, ByVal Fraction As Single)
        ' HHEAT connection of two compartments by a specified fraction
        aVentType = TypeHHeat
        aFirstCompartment = FirstCompartment
        aSecondCompartment = SecondCompartment
        aInitialOpening = Fraction
    End Sub
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer)
        ' VHeat connection of two compartments
        aVentType = TypeVHeat
        aFirstCompartment = FirstCompartment
        aSecondCompartment = SecondCompartment
        aInitialOpening = 1.0
    End Sub
    Public Sub GetVent(ByRef FirstCompartment As Integer, ByRef SecondCompartment As Integer, ByRef Width As Single, ByRef Soffit As Single, ByRef Sill As Single)
        ' Horizontal flow vent connection
        FirstCompartment = Me.FirstCompartment
        SecondCompartment = Me.SecondCompartment
        Width = Me.Width
        Soffit = Me.Soffit
        Sill = Me.Sill
    End Sub
    Public Sub SetVent(ByVal FirstCompartment As Integer, ByVal SecondCompartment As Integer, ByVal Width As Single, ByVal Soffit As Single, ByVal Sill As Single)
        ' Horizontal flow vent connection
        aVentType = TypeHVent
        aFirstCompartment = FirstCompartment
        aFirstOffset = 0.0
        aSecondCompartment = SecondCompartment
        aSecondOffset = 0.0
        aWidth = myUnits.Convert(UnitsNum.Length).ToSI(Width)
        aSoffit = myUnits.Convert(UnitsNum.Length).ToSI(Soffit)
        aSill = myUnits.Convert(UnitsNum.Length).ToSI(Sill)
        aWindCosine = 0.0
        aFace = 1
    End Sub
    Public Sub GetVent(ByVal TopCompartment As Integer, ByVal BottomCompartment As Integer, ByVal Area As Single, ByVal Shape As Integer)
        ' Vertical flow vent connection
        TopCompartment = Me.FirstCompartment
        BottomCompartment = Me.SecondCompartment
        Area = Me.Area
        Shape = Me.Shape
    End Sub
    Public Sub SetVent(ByVal TopCompartment As Integer, ByVal BottomCompartment As Integer, ByVal Area As Single, ByVal Shape As Integer)
        ' Vertical flow vent connection
        Me.VentType = TypeVVent
        Me.FirstCompartment = TopCompartment
        Me.SecondCompartment = BottomCompartment
        Me.Area = Area
        Me.Shape = Shape
    End Sub
    Public Sub GetVent(ByRef FromCompartment As Integer, ByRef FromArea As Single, ByRef FromCenterHeight As Single, ByRef FromOrientation As String, ByRef ToCompartment As Integer, ByRef ToArea As Single, ByRef ToCenterHeight As Single, ByRef ToOrientation As String, ByRef FlowRate As Single, ByRef BeginFlowDropoff As Single, ByRef ZeroFlow As Single)
        ' Mechanical flow vent connection
        Me.aVentType = TypeMVent
        FromCompartment = Me.FirstCompartment
        FromArea = Me.FirstArea
        FromCenterHeight = Me.FirstCenterHeight
        If Me.aFirstOrientation = 2 Then
            FromOrientation = "H"
        Else
            FromOrientation = "V"
        End If
        ToCompartment = Me.aSecondCompartment
        ToArea = Me.SecondArea
        ToCenterHeight = Me.SecondCenterHeight
        If Me.aSecondOrientation = 2 Then
            ToOrientation = "H"
        Else
            ToOrientation = "V"
        End If
        FlowRate = Me.FlowRate
        BeginFlowDropoff = Me.BeginFlowDropoff
        ZeroFlow = Me.ZeroFlow
    End Sub
    Public Sub SetVent(ByVal FromCompartment As Integer, ByVal FromArea As Single, ByVal FromCenterHeight As Single, ByVal FromOrientation As String, ByVal ToCompartment As Integer, ByVal ToArea As Single, ByVal ToCenterHeight As Single, ByVal ToOrientation As String, ByVal FlowRate As Single, ByVal BeginFlowDropoff As Single, ByVal ZeroFlow As Single)
        ' Mechanical flow vent connection
        Me.aVentType = TypeMVent
        Me.aFirstCompartment = FromCompartment
        Me.aFirstArea = myUnits.Convert(UnitsNum.Area).ToSI(FromArea)
        Me.aFirstCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(FromCenterHeight)
        If FromOrientation = "H" Then
            Me.aFirstOrientation = 2
        Else
            Me.aFirstOrientation = 1
        End If
        Me.aSecondCompartment = ToCompartment
        Me.aSecondArea = myUnits.Convert(UnitsNum.Area).ToSI(ToArea)
        Me.aSecondCenterHeight = myUnits.Convert(UnitsNum.Length).ToSI(ToCenterHeight)
        If ToOrientation = "H" Then
            Me.aSecondOrientation = 2
        Else
            Me.aSecondOrientation = 1
        End If
        Me.aFlowRate = FlowRate
        Me.aBeginFlowDropoff = myUnits.Convert(UnitsNum.Pressure).ToSI(BeginFlowDropoff)
        Me.aZeroFlow = myUnits.Convert(UnitsNum.Pressure).ToSI(ZeroFlow)
    End Sub
    Public ReadOnly Property IsValid(ByVal VentNumber As Integer) As Integer
        Get
            myUnits.SI = True
            HasErrors = 0
            Select Case aVentType
                Case TypeHVent
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aSill < 0.0 Or aSill > aComp1.RoomHeight Then
                            myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Sill is below floor level or above ceiling level.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aSoffit < 0.0 Or aSoffit <= aSill Or aSoffit > aComp1.RoomHeight Then
                            myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Soffit is below sill level or above ceiling level of first compartment.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aWidth <= 0.0 Or aWidth > Math.Max(aComp1.RoomWidth, aComp1.RoomDepth) Then
                            myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Width is less than 0 or greater than compartment dimensions.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                            Dim aComp2 As New Compartment
                            aComp2 = myCompartments(aSecondCompartment)
                            If aSill + aComp1.RoomOriginZ < aComp2.RoomOriginZ Or aSoffit + aComp1.RoomOriginZ > aComp2.RoomOriginZ + aComp2.RoomHeight Then
                                myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Soffit is below sill level or above ceiling level of second compartment.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                    End If
                    If Me.WindAngle < 0.0 Or Me.WindAngle > 360.0 Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Angle of wind to vent opening is less than 0� or greater than 360�.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aInitialOpening < 0.0 Or aInitialOpening > 1.0 Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Initial opening fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFinalOpening < 0.0 Or aFinalOpening > 1.0 Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Final opening fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFinalOpeningTime < 0.0 Or aFinalOpeningTime > myEnvironment.SimulationTime Then
                        myErrors.Add("Horizontal flow vent " + VentNumber.ToString + ". Final opening time is less than 0 or greater than simulation time.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                Case TypeVVent
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Vertical flow vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Vertical flow vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aArea <= 0 Or aArea > aComp1.RoomWidth * aComp1.RoomDepth Then
                            myErrors.Add("Vertical flow vent " + VentNumber.ToString + ". Cross-sectional area is less than 0 or greater than compartment floor area.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                            Dim aComp2 As New Compartment
                            aComp2 = myCompartments(aSecondCompartment)
                            If Math.Abs(aComp1.RoomOriginZ - (aComp2.RoomOriginZ + aComp2.RoomHeight)) > 0.1 Then
                                myErrors.Add("Vertical flow vent " + VentNumber.ToString + ". Floor of the top compartment is above ceiling of the bottom compartment.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                    End If
                Case TypeMVent
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aBeginFlowDropoff < 0.0 Or aBeginFlowDropoff >= MaxPressure Or aZeroFlow < 0 Or aZeroFlow >= MaxPressure Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Fan pressure constants are less than 0 Pa or greater than " + MaxPressure.ToString + " Pa.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aBeginFlowDropoff >= aZeroFlow Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Fan pressure constant where flow drop off begins is greater than fan pressure constant at zero flow.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFirstArea <= 0.0 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is less than 0 m� in size in from compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstArea > 1.0 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is greater than 1 m� in size in from compartment.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aSecondArea <= 0.0 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is less than 0 m� in size in to compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aSecondArea > 1.0 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is greater than 1 m� in size in to compartment.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aFirstCompartment >= 0 And aFirstCompartment < myCompartments.Count Then
                        myUnits.SI = True
                        Dim aComp1 As New Compartment
                        aComp1 = myCompartments(aFirstCompartment)
                        If aFirstOrientation = 1 Then
                            If aFirstCenterHeight - Math.Sqrt(aFirstArea) / 2 < 0.0 Or Math.Sqrt(aFirstArea) + aFirstCenterHeight > aComp1.RoomHeight Then
                                myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is below floor level or above ceiling level in from compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        If aFlowRate > 10 * aComp1.RoomWidth * aComp1.RoomDepth * aComp1.RoomHeight / 3600.0 Then
                            myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Flowrate is more than 10 air changes per hour out of compartment.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        myUnits.SI = False
                    End If
                    If aSecondCompartment >= 0 And aSecondCompartment < myCompartments.Count Then
                        myUnits.SI = True
                        Dim aComp2 As New Compartment
                        aComp2 = myCompartments(aSecondCompartment)
                        If aSecondOrientation = 1 Then
                            If aSecondCenterHeight - Math.Sqrt(aSecondArea) / 2 < 0.0 Or Math.Sqrt(aSecondArea) + aSecondCenterHeight > aComp2.RoomHeight Then
                                myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Vent diffuser area is below floor level or above ceiling level in to compartment.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                        If aFlowRate > 10 * aComp2.RoomWidth * aComp2.RoomDepth * aComp2.RoomHeight / 3600.0 Then
                            myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Flowrate is more than 10 air changes per hour into compartment.", ErrorMessages.TypeWarning)
                            HasErrors += 1
                        End If
                        myUnits.SI = False
                    End If
                    If aFilterEfficiency < 0.0 Or aFilterEfficiency > 1.0 Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Filter transmission fraction is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aFilterTime < 0.0 Or aFilterTime > myEnvironment.SimulationTime Then
                        myErrors.Add("Mechanical flow vent " + VentNumber.ToString + ". Filter operation time is less than 0 or greater than simulation time.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                Case TypeVHeat, TypeHHeat
                    If aFirstCompartment < -1 Or aSecondCompartment < -1 Then
                        myErrors.Add("Heat flow vent " + VentNumber.ToString + " is not connected between two existing compartments. Select compartment connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    ElseIf aFirstCompartment = aSecondCompartment Then
                        myErrors.Add("Heat flow vent " + VentNumber.ToString + ". Compartment is connected to itself. Select two different compartment as connections.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    End If
                    If aInitialOpening < 0.0 Or aInitialOpening > 1.0 Then
                        myErrors.Add("Heat flow vent " + VentNumber.ToString + ". Fraction of connected surface area is less than 0 or greater than 1.", ErrorMessages.TypeFatal)
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
        ToVent.FirstOffset = FromVent.FirstOffset
        ToVent.SecondCompartment = FromVent.SecondCompartment
        ToVent.SecondOffset = FromVent.SecondOffset
        ToVent.Width = FromVent.Width
        ToVent.Soffit = FromVent.Soffit
        ToVent.Sill = FromVent.Sill
        ToVent.WindAngle = FromVent.WindAngle
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
    Public ReadOnly Property IsValid() As Integer
        Get
            HasError = 0
            If Count > 0 Then
                Dim aVent As Vent
                For i = 0 To Count - 1
                    aVent = CType(List(i), Vent)
                    HasError += aVent.IsValid(i + 1)
                Next
            End If
            Return HasError
        End Get
    End Property
End Class
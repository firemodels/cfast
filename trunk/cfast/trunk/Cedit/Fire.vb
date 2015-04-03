Public Class Fire
    ' All units within the class are assumed to be consistent and typically SI

    Friend Const FireIgnitionbyTime As Integer = 0                  ' Ignition Criteria for additional fires, 0 for time, 1 for temperature, 2 for heat flux
    Friend Const FireIgnitionbyTemperature As Integer = 1
    Friend Const FireIgnitionbyFlux As Integer = 2
    Friend Const FireTime As Integer = 0                            ' Column numbers for fire time series data array
    Friend Const FireMdot As Integer = 1
    Friend Const FireHRR As Integer = 2
    Friend Const FireHeight As Integer = 3
    Friend Const FireArea As Integer = 4
    Friend Const FireCO As Integer = 5
    Friend Const FireSoot As Integer = 6
    Friend Const FireHC As Integer = 7
    Friend Const FireO2 As Integer = 8
    Friend Const FireHCN As Integer = 9
    Friend Const FireHCl As Integer = 10
    Friend Const FireCt As Integer = 11
    Friend Const FireLPF As Integer = 12
    Friend Const MaximumFires As Integer = 100
    Friend Const MaximumFireObjects As Integer = 100

    Private Const MinTemperature As Single = 173.15
    Private Const MaxTemperature As Single = 873.15
    Private Const MaxFlux As Single = 50000.0
    Private Const MaxMolarMass As Single = 292.0
    Private Const MaxTotalMass As Single = 10000.0
    Private Const MinHeatofCombustion As Single = 10000000.0
    Private Const MaxHeatofCombustion As Single = 1000000000.0
    Private Const MaxHRR As Single = 10000000000.0
    Private Const MaxMdot As Single = 1500.0
    Private Const MaxCO As Single = 0.4
    Private Const MaxSoot As Single = 99.0
    Private Const MaxHC As Single = 4.03176 / 12.0107
    Private Const MaxO2 As Single = 99.0
    Private Const MaxHCN As Single = 1.0
    Private Const MaxHCl As Single = 1.0
    Private Const MaxCt As Single = 100.0
    Private Const maxLPF As Single = 1.0

    Public Const TypeUnconstrained As Integer = 1
    Public Const TypeConstrained As Integer = 0
    Public Const TypeFireObject As Integer = -1

    Private aFireType As Integer                    ' Fire type, -1 for object definition, 0 for unConstrained and 1 for Constrained
    Private aChanged As Boolean = False
    Private HasErrors As Integer                    ' Temp variable that holds error COunt during error check
    Private ir As Integer, ic As Integer
    Private aValue, aPeak As Single
    Private aString As String

    ' Variables for current instance of a fire
    Private aCompartment As Integer                 ' COmpartment where the fire is located
    Private aXPosition As Single                    ' X (width) position of the fire in the COmpartment
    Private aYPosition As Single                    ' Y (depth) position of the fire in the COmpartment
    Private aZPosition As Single                    ' Z (height) position of the fire in the COmpartment
    Private aXNormal As Single                      ' X Component of normal vector from chosen surface of target
    Private aYNormal As Single                      ' Y Component of normal vector from chosen surface of target
    Private aZNormal As Single                      ' Z Component of normal vector from chosen surface of target
    Private aIgnitionType As Integer                ' Igntion criterion, 0 if by time, 1 if by temperature and 2 if by heat flux
    Private Const minValueIgnType As Integer = 0
    Private Const maxValueIgnType As Integer = 2
    Private aIgnitionValue As Single                ' Value at ignition for chosen igntion criterion
    Private aPlumeType As Integer                   ' Plume for this fire, 0 for Heskestad, 1 for McCaffrey
    Private Const minValuePlumes As Integer = 0
    Private Const maxValuePlumes As Integer = 1
    Private aFireObject As Integer                  ' index pointer to the selected fire object for this instance

    ' Object Definition for FireType = -1
    Private aName As String                         ' Single word name for the fire ... used as a filename for the fire as an object
    Private aChemicalFormula(5) As Single           ' Chemical formula, C atoms, H atoms, O atoms, N atoms, Cl atoms
    Private aLength As Single                       ' Length of the burning object
    Private aWidth As Single                        ' Width of the burning object
    Private aThickness As Single                    ' Thickness of the burning object
    Private aMolarMass As Single                    ' Molecular weight of the fuel
    Private aTotalMass As Single                    ' Total mass of the fuel
    Private aMaterial As String                     ' Material from material database
    Private aHeatofCombustion As Single             ' Heat of Combustion (note overspecification with HRR and Mdot)
    Private aHeatofGasification As Single           ' Heat of gasification
    Private aVolitilTemp As Single                  ' Volitilization temperature, default is ambient
    Private aRadiativeFraction As Single            ' Radiative fraction
    Private aFireTimeSeries(12, 0) As Single        ' Time series values for time, Mdot, HRR, and species
    Private aCommentsIndex As Integer               ' pointer into collection of comments for fire objects
    Public Sub New()
        Me.New(TypeConstrained)
    End Sub
    Public Sub New(ByVal Type As Integer)
        If Type = TypeUnconstrained Or Type = TypeConstrained Or Type = TypeFireObject Then aFireType = Type
        ' New definitions for an instance of a fire (that can then be associated with a fire object) or a default object
        aCompartment = -2
        aXPosition = -1.0
        aYPosition = -1.0
        aZPosition = -1.0
        aXNormal = 0.0
        aYNormal = 0.0
        aZNormal = 1.0
        aPlumeType = 0
        aIgnitionType = FireIgnitionbyTime
        aIgnitionValue = 0.0
        aFireObject = -1
        aCommentsIndex = -1
        ' New definitions for a fire object
        aName = "New Fire"
        aChemicalFormula(1) = 1.0 : aChemicalFormula(2) = 4.0 : aChemicalFormula(3) = 0.0 : aChemicalFormula(4) = 0.0 : aChemicalFormula(5) = 0.0
        aHeatofCombustion = 50000000.0
        aRadiativeFraction = 0.35
        aCommentsIndex = -1
        aMaterial = "METHANE"
    End Sub
    Public Sub New(ByVal Name As String, ByVal Chemical_Formula() As Single, ByVal Hoc As Single, ByVal RadiativeFraction As Single)
        ' New to define a fire object with all the details
        Me.New(TypeFireObject)
        aName = Name
        If UBound(Chemical_Formula) = 5 Then
            aChemicalFormula(1) = Chemical_Formula(1)
            aChemicalFormula(2) = Chemical_Formula(2)
            aChemicalFormula(3) = Chemical_Formula(3)
            aChemicalFormula(4) = Chemical_Formula(4)
            aChemicalFormula(5) = Chemical_Formula(5)
        End If
        aHeatofCombustion = Hoc
        aRadiativeFraction = RadiativeFraction
        aCommentsIndex = -1
    End Sub
    Public Sub New(ByVal TimetoPeak As Single, ByVal PeakHRR As Single, ByVal SteadyBurningTime As Single, ByVal DecayTime As Single)
        ' New to define a t^2 fire object
        Me.New(TypeFireObject)
        Dim ir As Integer
        Dim FireTimeSeries(12, 22) As Single, AlphaGrowth As Single, AlphaDecay As Single
        Dim aTimetoPeak As Single, aPeakHRR As Single, aSteadyBurningTime As Single, aDecayTime As Single
        aTimetoPeak = myUnits.ConvertFireData(UnitsNum.FireTime).ToSI(TimetoPeak)
        aPeakHRR = myUnits.ConvertFireData(UnitsNum.FireQdot).ToSI(PeakHRR)
        aSteadyBurningTime = myUnits.ConvertFireData(UnitsNum.FireTime).ToSI(SteadyBurningTime)
        aDecayTime = myUnits.ConvertFireData(UnitsNum.FireTime).ToSI(DecayTime)
        For ir = 0 To 22
            FireTimeSeries(FireHeight, ir) = 0.0
            FireTimeSeries(FireArea, ir) = 1.0
            FireTimeSeries(FireCO, ir) = 0.01
            FireTimeSeries(FireSoot, ir) = 0.01
            FireTimeSeries(FireHC, ir) = 1.0 / 3.0
            FireTimeSeries(FireO2, ir) = 0.0
            FireTimeSeries(FireHCN, ir) = 0.0
            FireTimeSeries(FireHCl, ir) = 0.0
            FireTimeSeries(FireCt, ir) = 1.0
            FireTimeSeries(FireLPF, ir) = 0.0
        Next
        FireTimeSeries(FireTime, 0) = 0.0
        FireTimeSeries(FireMdot, 0) = 0.0
        FireTimeSeries(FireHRR, 0) = 0
        AlphaGrowth = aPeakHRR / aTimetoPeak ^ 2
        For ir = 1 To 10
            FireTimeSeries(FireTime, ir) = ir / 10.0 * aTimetoPeak
            FireTimeSeries(FireHRR, ir) = AlphaGrowth * FireTimeSeries(FireTime, ir) ^ 2
            FireTimeSeries(FireMdot, ir) = FireTimeSeries(FireHRR, ir) / aHeatofCombustion
        Next
        AlphaDecay = aPeakHRR / aDecayTime ^ 2
        For ir = 11 To 21
            FireTimeSeries(FireTime, ir) = aTimetoPeak + aSteadyBurningTime + (10 - (21 - ir)) / 10.0 * aDecayTime
            FireTimeSeries(FireHRR, ir) = AlphaDecay * (aTimetoPeak + aSteadyBurningTime + aDecayTime - FireTimeSeries(FireTime, ir)) ^ 2
            FireTimeSeries(FireMdot, ir) = FireTimeSeries(FireHRR, ir) / aHeatofCombustion
        Next
        FireTimeSeries(FireTime, 22) = aTimetoPeak + aSteadyBurningTime + aDecayTime + 10.0
        FireTimeSeries(FireMdot, 22) = 0.0
        FireTimeSeries(FireHRR, 22) = 0
        myUnits.SI = True
        Me.SetFireData(FireTimeSeries)
        myUnits.SI = False
    End Sub
    Public Sub PeakFireValues(ByRef PeakHeight As Single, ByRef PeakArea As Single, ByRef PeakHRR As Single, ByRef PeakCO As Single, ByRef PeakC As Single, ByRef PeakHCN As Single, ByRef PeakHCl As Single)
        Dim NumPoints, j As Integer
        NumPoints = aFireTimeSeries.GetUpperBound(1)
        PeakHeight = 0.0
        PeakArea = 0.0
        PeakHRR = 0.0
        PeakCO = 0.0
        PeakC = 0.0
        PeakHCN = 0.0
        PeakHCl = 0.0
        For j = 0 To NumPoints
            If aFireTimeSeries(Fire.FireHeight, j) > PeakHeight Then PeakHeight = aFireTimeSeries(Fire.FireHeight, j)
            If aFireTimeSeries(Fire.FireArea, j) > PeakArea Then PeakArea = aFireTimeSeries(Fire.FireArea, j)
            If aFireTimeSeries(Fire.FireHRR, j) > PeakHRR Then PeakHRR = aFireTimeSeries(Fire.FireHRR, j)
            If aFireTimeSeries(Fire.FireCO, j) > PeakCO Then PeakCO = aFireTimeSeries(Fire.FireCO, j)
            If aFireTimeSeries(Fire.FireSoot, j) > PeakC Then PeakC = aFireTimeSeries(Fire.FireSoot, j)
            If aFireTimeSeries(Fire.FireHCN, j) > PeakHCN Then PeakHCN = aFireTimeSeries(Fire.FireHCN, j)
            If aFireTimeSeries(Fire.FireHCl, j) > PeakHCl Then PeakHCl = aFireTimeSeries(Fire.FireHCl, j)
        Next
    End Sub
    ReadOnly Property Peak(ByVal whichItem As Integer) As Single
        Get
            aPeak = -10 ^ 99
            If whichItem >= 0 And whichItem <= 12 Then
                For ic = 0 To aFireTimeSeries.GetUpperBound(1)
                    aValue = myUnits.ConvertFireData(whichItem).FromSI(aFireTimeSeries(whichItem, ic))
                    If aValue > aPeak Then aPeak = aValue
                Next
            End If
            Return aPeak
        End Get
    End Property
    Property FireType() As Integer
        Get
            Return aFireType
        End Get
        Set(ByVal Value As Integer)
            If aFireType <> Value Then
                aFireType = Value
                aChanged = True
            End If
        End Set
    End Property
    Property Compartment() As Integer
        Get
            Return aCompartment
        End Get
        Set(ByVal Value As Integer)
            If aCompartment <> Value Then
                aCompartment = Value
                aChanged = True
            End If
        End Set
    End Property
    Property XPosition() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aXPosition)
        End Get
        Set(ByVal Value As Single)
            If Value >= 0 Then
                If aXPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                    aXPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                    aChanged = True
                End If
            Else
                If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                    Dim tmpCompartment As New Compartment
                    tmpCompartment = myCompartments.Item(aCompartment)
                    aXPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomWidth / 2)
                    aChanged = True
                Else
                    aXPosition = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property YPosition() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aYPosition)
        End Get
        Set(ByVal Value As Single)
            If Value >= 0 Then
                If aYPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                    aYPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                    aChanged = True
                End If
            Else
                If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                    Dim tmpCompartment As New Compartment
                    tmpCompartment = myCompartments.Item(aCompartment)
                    aYPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomDepth / 2)
                    aChanged = True
                Else
                    aYPosition = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property ZPosition() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aZPosition)
        End Get
        Set(ByVal Value As Single)
            If Value >= 0 Then
                If aZPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                    aZPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                    aChanged = True
                End If
            Else
                If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                    aZPosition = 0.0
                    aChanged = True
                Else
                    aZPosition = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property XNormal() As Single
        Get
            Return aXNormal
        End Get
        Set(ByVal Value As Single)
            If aXNormal <> Value Then
                aXNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Property YNormal() As Single
        Get
            Return aYNormal
        End Get
        Set(ByVal Value As Single)
            If aYNormal <> Value Then
                aYNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Property ZNormal() As Single
        Get
            Return aZNormal
        End Get
        Set(ByVal Value As Single)
            If aZNormal <> Value Then
                aZNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Property IgnitionType() As Integer
        Get
            Return aIgnitionType
        End Get
        Set(ByVal Value As Integer)
            If Value >= Fire.minValueIgnType And Value <= Fire.maxValueIgnType Then
                If aIgnitionType <> Value Then
                    aIgnitionType = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property IgnitionValue() As Single
        Get
            If aIgnitionType = FireIgnitionbyTime Then
                Return myUnits.Convert(UnitsNum.Time).FromSI(aIgnitionValue)
            ElseIf aIgnitionType = FireIgnitionbyTemperature Then
                Return myUnits.Convert(UnitsNum.Temperature).FromSI(aIgnitionValue)
            ElseIf aIgnitionType = FireIgnitionbyFlux Then
                Return myUnits.Convert(UnitsNum.HeatFlux).FromSI(aIgnitionValue)
            Else
                Return 0.0
            End If
        End Get
        Set(ByVal Value As Single)
            If aIgnitionType = FireIgnitionbyTime Then
                If aIgnitionValue <> myUnits.Convert(UnitsNum.Time).ToSI(Value) And Value >= 0.0 Then
                    aIgnitionValue = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                    aChanged = True
                End If
            ElseIf aIgnitionType = FireIgnitionbyTemperature Then
                If aIgnitionValue <> myUnits.Convert(UnitsNum.Temperature).ToSI(Value) Then
                    aIgnitionValue = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                    aChanged = True
                End If
            ElseIf aIgnitionType = FireIgnitionbyFlux Then
                If aIgnitionValue <> myUnits.Convert(UnitsNum.HeatFlux).ToSI(Value) And Value >= 0.0 Then
                    aIgnitionValue = myUnits.Convert(UnitsNum.HeatFlux).ToSI(Value)
                    aChanged = True
                End If
            End If

        End Set
    End Property
    Property PlumeType() As Integer
        Get
            Return aPlumeType
        End Get
        Set(ByVal Value As Integer)
            If Value >= Fire.minValuePlumes And Value <= Fire.maxValuePlumes Then
                If aPlumeType <> Value Then
                    aPlumeType = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property FireObject() As Integer
        Get
            Return aFireObject
        End Get
        Set(ByVal Value As Integer)
            If aFireObject <> Value Then
                aFireObject = Value
                aChanged = True
            End If
        End Set
    End Property
    Property Name() As String
        Get
            Return aName
        End Get
        Set(ByVal Value As String)
            If aName <> Value Then
                aName = Value
                aChanged = True
            End If
        End Set
    End Property
    ReadOnly Property ChemicalFormula() As String
        Get
            aString = ""
            For ir = 1 To 5
                If aChemicalFormula(ir) <> 0 Then
                    aString = aString + Trim("C H O N Cl".Substring(2 * (ir - 1), 2))
                    aString = aString + Trim(aChemicalFormula(ir).ToString)
                End If
            Next
            Return aString
        End Get
    End Property
    Property ChemicalFormula(whichAtom As Integer) As Single
        Get
            If whichAtom >= 1 And whichAtom <= 5 Then
                Return aChemicalFormula(whichAtom)
            Else
                Return -1
            End If
        End Get
        Set(value As Single)
            If whichAtom >= 1 And whichAtom <= 5 Then
                aChemicalFormula(whichAtom) = value
            End If
        End Set
    End Property
    ReadOnly Property MolarMass() As Single
        Get
            Return myUnits.Convert(UnitsNum.Mass).FromSI((12.0107 * aChemicalFormula(1) + 1.00794 * aChemicalFormula(2) + 15.9994 * aChemicalFormula(3) + 14.0067 * aChemicalFormula(4) + 35.453 * aChemicalFormula(5)) / 1000.0)
        End Get
    End Property
    Public Property Material() As String
        Get
            Return aMaterial
        End Get
        Set(ByVal Value As String)
            If Value <> aMaterial Then
                Me.aMaterial = Value
                aChanged = True
            End If
        End Set
    End Property
    Property HeatofCombustion() As Single
        Get
            Return myUnits.Convert(UnitsNum.HoC).FromSI(aHeatofCombustion)
        End Get
        Set(ByVal Value As Single)
            If aHeatofCombustion <> myUnits.Convert(UnitsNum.HoC).ToSI(Value) And Value > 0.0 Then
                aHeatofCombustion = myUnits.Convert(UnitsNum.HoC).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Property RadiativeFraction() As Single
        Get
            Return aRadiativeFraction
        End Get
        Set(ByVal Value As Single)
            If aRadiativeFraction <> Value And Value >= 0.0 Then
                aRadiativeFraction = Value
                aChanged = True
            End If
        End Set
    End Property
    Property FireTimeSeries(ByVal i As Integer, ByVal j As Integer) As Single
        Get
            If i <= 12 And i > 0 And j > 0 And j <= aFireTimeSeries.GetUpperBound(1) Then
                Return myUnits.ConvertFireData(i).FromSI(aFireTimeSeries(i, j))
            Else
                Return -1.0
            End If
        End Get
        Set(ByVal Value As Single)
            If i <= 12 And i > 0 And j > 0 And j < aFireTimeSeries.GetUpperBound(1) Then
                If aFireTimeSeries(i, j) <> myUnits.ConvertFireData(i).ToSI(Value) Then
                    aFireTimeSeries(i, j) = myUnits.ConvertFireData(i).ToSI(Value)
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Property CommentsIndex() As Integer
        Get
            Return aCommentsIndex
        End Get
        Set(ByVal Value As Integer)
            If Value <> aCommentsIndex Then
                aCommentsIndex = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Sub SetPosition(ByVal index As Integer)
        Dim tmpCompartment As New Compartment
        If index <= myCompartments.Count - 1 Then
            tmpCompartment = myCompartments.Item(index)
            aCompartment = index
            aXNormal = 0.0
            aYNormal = 0.0
            aZNormal = 1.0
        End If
    End Sub
    Public Sub SetPosition(ByVal index As Integer, ByVal XPosition As Single, ByVal YPosition As Single, ByVal ZPosition As Single)
        If index <= myCompartments.Count - 1 Then
            Me.Compartment = index
            Me.XPosition = XPosition
            Me.YPosition = YPosition
            Me.ZPosition = ZPosition
            Me.XNormal = 0.0
            Me.YNormal = 0.0
            Me.ZNormal = 1.0
        End If
    End Sub
    Public Sub SetPosition(ByVal index As Integer, ByVal XPosition As Single, ByVal YPosition As Single, ByVal ZPosition As Single, ByVal XNormal As Single, ByVal YNormal As Single, ByVal ZNormal As Single)
        If index <= myCompartments.Count - 1 Then
            Me.Compartment = index
            Me.XPosition = XPosition
            Me.YPosition = YPosition
            Me.ZPosition = ZPosition
            Me.XNormal = XNormal
            Me.YNormal = YNormal
            Me.ZNormal = ZNormal
        End If
    End Sub
    Public Sub GetFireData(ByRef FireTimeSeries(,) As Single, ByRef NumDataPoints As Integer)
        Dim i As Integer, j As Integer
        If aFireTimeSeries.GetUpperBound(0) = aFireTimeSeries.GetUpperBound(0) Then
            ReDim FireTimeSeries(aFireTimeSeries.GetUpperBound(0), aFireTimeSeries.GetUpperBound(1))
            For i = 0 To aFireTimeSeries.GetUpperBound(0)
                For j = 0 To aFireTimeSeries.GetUpperBound(1)
                    FireTimeSeries(i, j) = myUnits.ConvertFireData(i).FromSI(aFireTimeSeries(i, j))
                Next
            Next
            NumDataPoints = aFireTimeSeries.GetUpperBound(1)
        End If
    End Sub
    Public Sub SetFireData(ByVal FireTimeSeries(,) As Single)
        Dim i As Integer, j As Integer
        If FireTimeSeries.GetUpperBound(0) = aFireTimeSeries.GetUpperBound(0) Then
            If aFireTimeSeries.GetUpperBound(1) <> FireTimeSeries.GetUpperBound(1) Then
                ReDim aFireTimeSeries(aFireTimeSeries.GetUpperBound(0), FireTimeSeries.GetUpperBound(1))
                aChanged = True
            End If
            For i = 0 To aFireTimeSeries.GetUpperBound(0)
                For j = 0 To aFireTimeSeries.GetUpperBound(1)
                    If aFireTimeSeries(i, j) <> myUnits.ConvertFireData(i).ToSI(FireTimeSeries(i, j)) Then
                        aChanged = True
                        aFireTimeSeries(i, j) = myUnits.ConvertFireData(i).ToSI(FireTimeSeries(i, j))
                    End If
                Next
            Next
        End If
    End Sub
    Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Public ReadOnly Property IsValid(ByVal FireNumber As Integer) As Integer
        Get
            myUnits.SI = True
            HasErrors = 0
            Select Case aFireType
                Case TypeFireObject
                    If aMolarMass < 0.0 Or aMolarMass > MaxMolarMass Then
                        myErrors.Add("Fire Object " + aName + " has a molar mass less than 0 kg/mol or greater than " + MaxMolarMass.ToString + " kg/mol.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aHeatofCombustion < MinHeatofCombustion Or aHeatofCombustion > MaxHeatofCombustion Then
                        myErrors.Add("Fire Object " + aName + " has a heat of combustion less than " + MinHeatofCombustion.ToString + _
                            " J/kg or greater than " + MaxHeatofCombustion.ToString + " J/kg.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aRadiativeFraction < 0.0 Or aRadiativeFraction > 1.0 Then
                        myErrors.Add("Fire object " + aName + ". Radiative fraction is less than 0 or greater than 1", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aFireTimeSeries.GetUpperBound(1) > 0 Then
                        Dim FireCurveErrors() As Boolean = {False, False, False, False, False, False, False, False, False, False, False, False, False}
                        For ir = 0 To aFireTimeSeries.GetUpperBound(1)
                            For ic = 0 To 12
                                Select Case ic
                                    Case FireTime
                                        If aFireTimeSeries(FireTime, ir) < 0.0 Then
                                            FireCurveErrors(FireTime) = True
                                            HasErrors += 1
                                        End If
                                    Case FireHRR
                                        If aFireTimeSeries(FireHRR, ir) < 0.0 Or aFireTimeSeries(FireHRR, ir) > MaxHRR Then
                                            FireCurveErrors(FireHRR) = True
                                            HasErrors += 1
                                        End If
                                        'Dim aD As Single, qstar As Single, FlameLength As Single, HRRpm3 As Single
                                        'aD = Math.Max(0.3, aFireTimeSeries(FireArea, ir))
                                        'qstar = aFireTimeSeries(FireHRR, ir) / (1.29 * 1012.0 * 288.0) * Math.Sqrt(9.8 * aD) * aD * aD
                                        'FlameLength = Math.Max(0.0, aD * (3.7 * qstar ^ 0.4 - 1.02))
                                        'HRRpm3 = aFireTimeSeries(FireHRR, ir) / (aFireTimeSeries(FireArea, ir) * FlameLength)
                                        'If HRRpm3 > 4 * 10 ^ 6 Then
                                        'FireCurveErrors(FireHRR) = True
                                        'HasErrors += 1
                                        'End If
                                    Case FireMdot
                                        If aFireTimeSeries(FireMdot, ir) < 0.0 Or aFireTimeSeries(FireMdot, ir) > MaxMdot Then
                                            FireCurveErrors(FireMdot) = True
                                            HasErrors += 1
                                        End If
                                    Case FireHeight
                                        If aFireTimeSeries(FireHeight, ir) < 0.0 Or aFireTimeSeries(FireHeight, ir) > CEdit.Compartment.MaxSize Then
                                            FireCurveErrors(FireHeight) = True
                                            HasErrors += 1
                                        End If
                                    Case FireArea
                                        If (aFireTimeSeries(FireArea, ir) < 0.0 And aFireTimeSeries(FireHRR, ir) <> 0.0) And aFireTimeSeries(FireArea, ir) < 0.0 Or aFireTimeSeries(FireArea, ir) > CEdit.Compartment.MaxSize ^ 2 Then
                                            FireCurveErrors(FireArea) = True
                                            HasErrors += 1
                                        End If
                                    Case FireCO
                                        If aFireTimeSeries(FireCO, ir) < 0.0 Or aFireTimeSeries(FireCO, ir) > MaxCO Then
                                            FireCurveErrors(FireCO) = True
                                            HasErrors += 1
                                        End If
                                    Case FireSoot
                                        If aFireTimeSeries(FireSoot, ir) < 0.0 Or aFireTimeSeries(FireSoot, ir) > MaxSoot Then
                                            FireCurveErrors(FireSoot) = True
                                            HasErrors += 1
                                        End If
                                    Case FireHC
                                        If aFireTimeSeries(FireHC, ir) < 0.0 Or aFireTimeSeries(FireHC, ir) > MaxHC Then
                                            FireCurveErrors(FireHC) = True
                                            HasErrors += 1
                                        End If
                                    Case FireO2
                                        If aFireTimeSeries(FireO2, ir) < 0.0 Or aFireTimeSeries(FireO2, ir) > MaxO2 Then
                                            FireCurveErrors(FireO2) = True
                                            HasErrors += 1
                                        End If
                                    Case FireHCN
                                        If aFireTimeSeries(FireHCN, ir) < 0.0 Or aFireTimeSeries(FireHCN, ir) > MaxHCN Then
                                            FireCurveErrors(FireHCN) = True
                                            HasErrors += 1
                                        End If
                                    Case FireHCl
                                        If aFireTimeSeries(FireHCl, ir) < 0.0 Or aFireTimeSeries(FireHCl, ir) > MaxHCl Then
                                            FireCurveErrors(FireHCl) = True
                                            HasErrors += 1
                                        End If
                                    Case FireCt
                                        If aFireTimeSeries(FireCt, ir) < 0.0 Or aFireTimeSeries(FireCt, ir) > MaxCt Then
                                            FireCurveErrors(FireCt) = True
                                            HasErrors += 1
                                        End If
                                    Case FireLPF
                                        If aFireTimeSeries(FireLPF, ir) < 0.0 Or aFireTimeSeries(FireLPF, ir) > maxLPF Then
                                            FireCurveErrors(FireLPF) = True
                                            HasErrors += 1
                                        End If
                                End Select
                            Next
                        Next
                        If FireCurveErrors(FireTime) Then myErrors.Add("Fire object " + aName + ". One or more time values are less than 0 s.", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireHRR) Then myErrors.Add("Fire object " + aName + ". One or more heat release rate (Qdot) values are less than 0 W, more than" + (MaxHRR / 1000000).ToString + " MW, or more than 2 MW/m^3.", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireMdot) Then myErrors.Add("Fire object " + aName + ". One or more pyrolysis rate (Mdot) values are less than 0 kg/s or greater than" + MaxMdot.ToString + " kg/s.", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireHeight) Then myErrors.Add("Fire object " + aName + ". One or more fire height values are less than 0 m or greater than" + CEdit.Compartment.MaxSize.ToString + " m.", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireArea) Then myErrors.Add("Fire object " + aName + ". One or more fire area values are less than or equal to 0 m² or greater than" + (CEdit.Compartment.MaxSize ^ 2).ToString + " m².", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireCO) Then myErrors.Add("Fire object " + aName + ". One or more CO/CO2 ratios are less than 0 or greater than" + MaxCO.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireSoot) Then myErrors.Add("Fire object " + aName + ". One or more C/CO2 ratios are less than 0 or greater than" + MaxSoot.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireHC) Then myErrors.Add("Fire object " + aName + ". One or more H/C ratios are less than 0 or greater than" + MaxHC.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireO2) Then myErrors.Add("Fire object " + aName + ". One or more O/C ratios are less than 0 or greater than" + MaxO2.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireHCN) Then myErrors.Add("Fire object " + aName + ". One or more HCN yields are less than 0 or greater than" + MaxHCN.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireHCl) Then myErrors.Add("Fire object " + aName + ". One or more HCl yields are less than 0 or greater than" + MaxHCl.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireCt) Then myErrors.Add("Fire object " + aName + ". One or more Ct values are less than 0 or greater than" + MaxCt.ToString + ".", ErrorMessages.TypeWarning)
                        If FireCurveErrors(FireLPF) Then myErrors.Add("Fire object " + aName + ". One or more LPF values are less than 0 or greater than" + maxLPF.ToString + ".", ErrorMessages.TypeWarning)
                    End If
                Case TypeUnconstrained, TypeConstrained
                    If myEnvironment.LowerOxygenLimit < 0.0 Or myEnvironment.LowerOxygenLimit > 20.95 Then
                        myErrors.Add("Lower oxygen limit is less than 0 or greater than 21 %.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aFireObject < 0 Or aFireObject > myFireObjects.Count Then
                        myErrors.Add("Fire " + FireNumber.ToString + " does not have a fire object selected. Select a fire object for this fire.", ErrorMessages.TypeFatal)
                    End If
                    If aCompartment < 0 Or aCompartment > myCompartments.Count - 1 Then
                        myErrors.Add("Fire " + FireNumber.ToString + " is not assigned to an existing Compartment. Select Compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    Else
                        Dim aComp As New Compartment
                        aComp = myCompartments(aCompartment)
                        If aXPosition <> -1.0 Then
                            If aXPosition < 0.0 Or aXPosition > aComp.RoomWidth Then
                                myErrors.Add("Target " + FireNumber.ToString + " width position is less than 0 m or greater than Compartment width.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                        If aYPosition <> -1.0 Then
                            If aYPosition < 0.0 Or aYPosition > aComp.RoomDepth Then
                                myErrors.Add("Fire " + FireNumber.ToString + " depth position is less than 0 m or greater than Compartment depth.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                        If aZPosition <> -1 Then
                            If aZPosition < 0.0 Or aZPosition > aComp.RoomHeight Then
                                myErrors.Add("Fire " + FireNumber.ToString + " initial height is less than 0 m or greater than Compartment height.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        End If
                        If XNormal ^ 2 + YNormal ^ 2 + ZNormal ^ 2 = 0 Then
                            myErrors.Add("Fire " + FireNumber.ToString + ". Normal vector cannot be of zero length.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aIgnitionType = FireIgnitionbyTime Then
                            If aIgnitionValue < 0.0 Or aIgnitionValue > myEnvironment.SimulationTime Then
                                myErrors.Add("Fire " + FireNumber.ToString + ". Ignition value is less than 0 s or greater than simulation time.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        ElseIf aIgnitionType = FireIgnitionbyTemperature Then
                            If aIgnitionValue < MinTemperature Or aIgnitionValue > MaxTemperature Then
                                myErrors.Add("Fire " + FireNumber.ToString + ". Ignition value is less than " + (MinTemperature - 273.15).ToString + " °C or greater than " + (MaxTemperature - 273.15).ToString + " °C.", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        ElseIf aIgnitionType = FireIgnitionbyFlux Then
                            If aIgnitionValue < 0.0 Or aIgnitionValue > MaxFlux Then
                                myErrors.Add("Fire " + FireNumber.ToString + ". Ignition value is less than 0 kW/m² or greater than " + MaxFlux + " kW/m².", ErrorMessages.TypeWarning)
                                HasErrors += 1
                            End If
                        End If
                    End If
            End Select
            myUnits.SI = False
            Return HasErrors
        End Get
    End Property
End Class
Public Class FireCollection
    Inherits System.Collections.CollectionBase
    Private i As Integer, j As Integer, aFireType As Integer
    Private HasErrors As Integer
    Public Sub Add(ByVal aFire As Fire)
        List.Add(aFire)
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromFire As New Fire, ToFire As New Fire
        FromFire = CType(List.Item(indexFrom), Fire)
        FireCopy(FromFire, ToFire)
        List.Item(indexTo) = ToFire
    End Sub
    Public Sub Swap(ByVal index1 As Integer, ByVal index2 As Integer)
        If index1 >= 0 And index1 < Count And index2 >= 0 And index2 < Count Then
            Dim temp As New Fire
            temp = CType(List.Item(index2), Fire)
            List.Item(index2) = List.Item(index1)
            List.Item(index1) = temp
        End If
    End Sub
    Public Sub Remove(ByVal index As Integer)
        ' make sure that the COmpartment number is valid
        If index > Count - 1 Or index < 0 Then
            System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Fire number not found.")
        Else
            List.RemoveAt(index)
        End If
    End Sub
    Public Sub RemoveAll(ByVal index As Integer)
        ' Removes all fires COnnected to the COmpartment specified by index
        If index >= 0 And index < myCompartments.Count Then
            Dim aFire As New Fire
            i = 0
            While i < Count
                aFire = CType(List.Item(i), Fire)
                If aFire.Compartment = index Then
                    List.RemoveAt(i)
                    i = 0
                Else
                    i += 1
                End If
            End While
        End If
    End Sub
    Public Sub Renumber(ByVal Oldindex As Integer, ByVal Newindex As Integer)
        Dim aFire As New Fire
        For i = 0 To Count - 1
            aFire = CType(List.Item(i), Fire)
            If aFire.Compartment = Oldindex Then
                aFire.Compartment = Newindex
            End If
            List.Item(i) = aFire
        Next
    End Sub
    Public ReadOnly Property GetFireIndex(ByVal Shortname As String) As Integer
        Get
            If Me.Count > 0 Then
                For i = 0 To Me.Count - 1
                    If Me.Item(i).Name = Shortname Then
                        Return i
                        Exit Property
                    End If
                Next
            End If
            Return -1
        End Get
    End Property
    Public ReadOnly Property Maximum() As Integer
        Get
            If Count > 0 Then
                aFireType = CType(List.Item(0), Fire).FireType
                If aFireType = Fire.TypeFireObject Then
                    Return Fire.MaximumFireObjects
                Else
                    Return Fire.MaximumFires
                End If
            Else
                Return 1
            End If
        End Get
    End Property
    Default Public Property Item(ByVal index As Integer) As Fire
        Get
            If index >= 0 And index < Count Then
                Return CType(List.Item(index), Fire)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Fire number not found.")
            End If
        End Get
        Set(ByVal Value As Fire)
            List.Item(index) = Value
        End Set
    End Property
    Public ReadOnly Property NumberofConnections(ByVal index As Integer) As Integer
        Get
            Dim aFire As Fire
            Dim numFires As Integer = 0
            numFires = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aFire = CType(List.Item(i), Fire)
                    If aFire.Compartment = index Then numFires += 1
                Next
            End If
            Return numFires
        End Get
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aFire As Fire
                For i = 0 To Count - 1
                    aFire = CType(List.Item(i), Fire)
                    If aFire.Changed Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            HasErrors = 0
            ' Check individual fire objects or instances for errors
            If Count > 0 Then
                Dim aFire1 As Fire, aFire2 As Fire, isFireObject As Boolean
                For i = 0 To Count - 1
                    aFire1 = CType(List(i), Fire)
                    HasErrors += aFire1.IsValid(i + 1)
                    If aFire1.FireType = Fire.TypeFireObject Then isFireObject = True
                Next
                ' Cannot have duplicate fire object names
                If Count > 1 And isFireObject Then
                    For i = 0 To myFireObjects.Count - 2
                        aFire1 = myFireObjects(i)
                        For j = i + 1 To myFireObjects.Count - 1
                            aFire2 = myFireObjects(j)
                            If aFire1.Name = aFire2.Name Then
                                myErrors.Add(aFire1.Name + " is used more than once as a fire object name. Duplicate names are not allowed.", ErrorMessages.TypeFatal)
                                HasErrors += 1
                            End If
                        Next
                    Next
                End If
            End If
            Return HasErrors
        End Get
    End Property
End Class
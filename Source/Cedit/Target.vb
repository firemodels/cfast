Public Class Target

    Friend Const TypeTarget As Integer = 0
    Friend Const TypeDetector As Integer = 1
    Friend Const SmokeDetectorActivationTemperature As Double = 30.0    ' Default Smoke detector simulated as a heat detector with 10 °C rise trigger with RTI = 5 or obscuration of 8 %/ft
    Friend Const SmokeDetectorRTI As Double = 5.0
    Friend Const SmokeDetectorActivationObscuration As Double = 100 * (1 - (1 - 8 / 100) ^ (1 / 0.3048))
    Friend Const HeatDetectorActiviationTemperature As Double = 57.22 + 273.15   ' Default heat detector is a 135 °F detector with 10 ft spacing from NRC NUREG 1805, table 12-2
    Friend Const HeatDetectorRTI As Double = 404.0
    Friend Const SprinklerActivationTemperature As Double = 73.89 + 273.15       ' Default sprinkler is standard response link, ordinary sprinkler, RTI = 130 and 165 °F
    Friend Const SprinklerRTI As Double = 130
    Friend Const Implicit As Integer = 0
    Friend Const Explicit As Integer = 1
    Friend Const Steady As Integer = 2
    Friend Const ThermallyThick As Integer = 0
    Friend Const Cylindrical As Integer = 1
    Friend Const TypeSmokeDetector As Integer = 0                       ' Type 0 is a smoke detector, 1 is a heat detector, 2 is a sprinkler
    Friend Const TypeHeatDetector As Integer = 1
    Friend Const TypeSprinkler As Integer = 2
    Friend Const MaximumTargets As Integer = 1000
    Friend Const ActivationbyTemperature As Integer = 0
    Friend Const ActivationbyObscuration As Integer = 1

    ' All units within the class are assumed to be consistent and typically SI
    Private aAdiabaticTarget As Boolean = False ' True for output of adiabatic surface temperature of target
    Private aConvectionCoefficients(2) As Double ' Front(1) and back(2) convection coefficients for output of adiabatic target temperature 
    Private aType As Integer                    ' 0 for target and 1 for detector
    Private aName As String                     ' Target name (at the moment, only used for targets, not detectors)
    Private aCompartment As Integer             ' Compartment number where target or detector is located
    Private aXPosition As Double                ' X Position of target or detector
    Private aYPosition As Double                ' Y Position of target or detector
    Private aZPosition As Double                ' Z Position of target or detector
    Private aTargetFacing As String             ' Front face orientation of target: UP, DOWN, FRONT, BACK, LEFT, RIGHT, or a fire ID
    Private aFixedTemperature As Double         ' Fixed front surfae temperature for calculation of gauge heat flux
    Private aXNormal As Double                  ' X component of normal vector from chosen surface of target
    Private aYNormal As Double                  ' Y component of normal vector from chosen surface of target
    Private aZNormal As Double                  ' Z component of normal vector from chosen surface of target
    Private aInternalLocation As Double         ' Location for reporting of internal temperature as a fraction of thickness. Default is 0.5 (center)
    Private aMaterial As String                 ' Target material from material database
    Private aThickness As Double                ' Target thickness. If zero, we use the material thickness from thermal properties tab
    Private aSolutionType As Integer            ' 0 for thermally thick plate which is the PDE option or 1 for cylindrical coordinates.  ODE is no longer supported
    Private aDetectorType As Integer            ' 0 for heat detector, 1 for smoke detector, 2 for sprinkler 
    Private aActivationType As Integer          ' Used to maintain compatibility with older smoke dtector inputs. 0 for temperature, 1 for obscuration. 
    Private aActivationTemperature As Double    ' Activation temperature for all detector types.  Default depends on type
    Private aActivationObscurationFlaming As Double ' Activation obscuration for smoke detectors from flaming smoke
    Private aActivationObscurationSmoldering As Double ' Activation obscuration for smoke detectors from smoldering smoke
    Private aRTI As Double                      ' Detector RTI value
    Private aSprayDensity As Double             ' Sprinkler spray density
    Private aFYI As String                      ' Descriptor for additional user supplied information
    Private aChanged As Boolean = False         ' True once compartment information has changed
    Private HasErrors As Integer                ' Temp variable that holds error count during error check

    Public Sub New()
        aName = ""
        aFYI = ""
        aCompartment = -2
        aXPosition = -1.0
        aYPosition = -1.0
        aZPosition = -1.0
        aType = TypeTarget
        aTargetFacing = "-"
        aFixedTemperature = -1001
        aXNormal = 0.0
        aYNormal = 0.0
        aZNormal = 1.0
        aInternalLocation = 0.5
        aMaterial = "Off"
        aThickness = 0
        aSolutionType = 0
        aDetectorType = 1
        aActivationTemperature = HeatDetectorActiviationTemperature
        aActivationObscurationSmoldering = SmokeDetectorActivationObscuration
        aActivationObscurationFlaming = SmokeDetectorActivationObscuration
        aActivationType = ActivationbyTemperature
        aRTI = HeatDetectorRTI
        aSprayDensity = 0.00007
        aAdiabaticTarget = False
        aConvectionCoefficients = {0, 0, 0}
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
    Public Property Type() As Integer
        Get
            Return aType
        End Get
        Set(ByVal Value As Integer)
            If Value <> aType Then
                aType = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Sub SetDefaultLocation()
        If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
            Dim tmpCompartment As Compartment = myCompartments.Item(aCompartment)
            XPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomWidth) / 2
            YPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomDepth) / 2
            If aType = TypeTarget Then
                ZPosition = 0
            Else
                ZPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomHeight) * 0.99
            End If
        End If
    End Sub
    Public Property Compartment() As Integer
        Get
            Return aCompartment
        End Get
        Set(ByVal Value As Integer)
            If Value <> aCompartment Then
                aCompartment = Value
                If Value > -1 Then SetDefaultLocation()
                aChanged = True
            End If
        End Set
    End Property
    Public Property XPosition() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aXPosition)
        End Get
        Set(ByVal Value As Double)
            If aXPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                If Value >= 0 Then
                    aXPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                Else
                    If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                        Dim tmpCompartment As Compartment = myCompartments.Item(aCompartment)
                        aXPosition = tmpCompartment.RoomWidth - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Value))
                    End If
                End If
                aChanged = True
            End If
        End Set
    End Property
    Public Property YPosition() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aYPosition)
        End Get
        Set(ByVal Value As Double)
            If aYPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                If Value >= 0 Then
                    aYPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                Else
                    If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                        Dim tmpCompartment As Compartment = myCompartments.Item(aCompartment)
                        aYPosition = tmpCompartment.RoomDepth - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Value))
                    End If
                End If
                aChanged = True
            End If
        End Set
    End Property
    Public Property ZPosition() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aZPosition)
        End Get
        Set(ByVal Value As Double)
            If aZPosition <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                If Value >= 0 Then
                    aZPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                Else
                    If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                        Dim tmpCompartment As Compartment = myCompartments.Item(aCompartment)
                        aZPosition = tmpCompartment.RoomHeight - Math.Abs(myUnits.Convert(UnitsNum.Length).ToSI(Value))
                    End If
                End If
                aChanged = True
            End If
        End Set
    End Property
    Public Property TargetFacing() As String
        Get
            Return aTargetFacing
        End Get
        Set(value As String)
            If CheckTargetFacing(value) <> aTargetFacing Then
                aTargetFacing = value
                aChanged = True
            End If
        End Set
    End Property
    Public Property TargetFacingNoCheck() As String
        Get
            Return aTargetFacing
        End Get
        Set(value As String)
            If value <> aTargetFacing Then
                aTargetFacing = value
                aChanged = True
            End If
        End Set
    End Property
    Public ReadOnly Property CheckTargetFacing(aValue As String) As String
        Get
            If InStr(Data.NormalPointsTo, aValue, CompareMethod.Text) > 0 Then
                Return aValue
            Else
                Dim numFires As Integer, i As Integer
                numFires = myFires.Count
                If numFires > 0 Then
                    Dim aFire As Fire
                    For i = 1 To numFires
                        aFire = myFires(i - 1)
                        If aCompartment = aFire.Compartment Then
                            If aValue = "Fire " + i.ToString + ", " + aFire.Name Then
                                Return aFire.Name
                            ElseIf aValue = aFire.Name Then
                                Return aFire.Name
                                ' Dim Hypotenuse As Double, FHeight As Double
                                'FHeight = Me.FireDataSS(1, 3) ' this should be fire height @ t=0
                                'Hypotenuse = Math.Sqrt((aFire.XPosition - aTarget.XPosition) ^ 2 + (aFire.YPosition - aTarget.YPosition) ^ 2 + (FHeight - aTarget.ZPosition) ^ 2)
                                'If Hypotenuse <> 0 Then
                                ' aTarget.XNormal = (aFire.XPosition - aTarget.XPosition) / Hypotenuse
                                ' aTarget.YNormal = (aFire.YPosition - aTarget.YPosition) / Hypotenuse
                                ' aTarget.ZNormal = (FHeight - aTarget.ZPosition) / Hypotenuse
                                ' End If
                            End If
                        End If
                    Next
                End If
            End If
            Return "-"
        End Get
    End Property
    Public Property XNormal() As Double
        Get
            Return aXNormal
        End Get
        Set(ByVal Value As Double)
            If Value <> aXNormal Then
                aXNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property YNormal() As Double
        Get
            Return aYNormal
        End Get
        Set(ByVal Value As Double)
            If Value <> aYNormal Then
                aYNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property ZNormal() As Double
        Get
            Return aZNormal
        End Get
        Set(ByVal Value As Double)
            If Value <> aZNormal Then
                aZNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property InternalLocation() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aInternalLocation)
        End Get
        Set(ByVal Value As Double)
            If aInternalLocation <> myUnits.Convert(UnitsNum.Length).ToSI(Value) Then
                aInternalLocation = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property FixedTemperature() As Double
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aFixedTemperature)
        End Get
        Set(value As Double)
            If aFixedTemperature <> myUnits.Convert(UnitsNum.Temperature).ToSI(value) Then
                aFixedTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property Material() As String
        Get
            Return aMaterial
        End Get
        Set(ByVal Value As String)
            If Value <> aMaterial Then
                aMaterial = myThermalProperties.ValidThermalProperty(Value, "Target")
                If myTargets.DoChange Then aChanged = True
            End If
        End Set
    End Property
    Public Property Thickness() As Double
        Get
            Return aThickness
        End Get
        Set(ByVal Value As Double)
            If Value <> aThickness Then
                aThickness = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property SolutionType() As Integer
        Get
            Return aSolutionType
        End Get
        Set(ByVal Value As Integer)
            If Value <> aSolutionType Then
                aSolutionType = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property DetectorType() As Integer
        Get
            Return aDetectorType
        End Get
        Set(ByVal Value As Integer)
            If Value <> aDetectorType Then
                aDetectorType = Value
                If aDetectorType = TypeSmokeDetector Then
                    aActivationType = ActivationbyObscuration
                Else
                    aActivationType = ActivationbyTemperature
                End If
                aChanged = True
            End If
        End Set
    End Property
    Public ReadOnly Property ActivationType() As Integer
        Get
            ActivationType = aActivationType
        End Get
    End Property
    Public Property ActivationObscurationSmoldering() As Double
        Get
            If aActivationObscurationSmoldering <> 0 Then
                Return myUnits.Convert(UnitsNum.Smoke).FromSI(aActivationObscurationSmoldering)
            Else
                Return 0
            End If
        End Get
        Set(ByVal Value As Double)
            If Value <> 0 Then
                If myUnits.Convert(UnitsNum.Smoke).ToSI(Value) <> aActivationObscurationSmoldering Then
                    aActivationObscurationSmoldering = myUnits.Convert(UnitsNum.Smoke).ToSI(Value)
                    aChanged = True
                End If
                If aActivationType <> aActivationObscurationSmoldering Then
                    aActivationType = ActivationbyObscuration
                End If
            Else
                If aActivationObscurationSmoldering <> 0 Then
                    aActivationObscurationSmoldering = 0
                    aActivationType = ActivationbyObscuration
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property ActivationObscurationFlaming() As Double
        Get
            If aActivationObscurationFlaming <> 0 Then
                Return myUnits.Convert(UnitsNum.Smoke).FromSI(aActivationObscurationFlaming)
            Else
                Return 0
            End If
        End Get
        Set(ByVal Value As Double)
            If Value <> 0 Then
                If myUnits.Convert(UnitsNum.Smoke).ToSI(Value) <> aActivationObscurationFlaming Then
                    aActivationObscurationFlaming = myUnits.Convert(UnitsNum.Smoke).ToSI(Value)
                    aChanged = True
                End If
                If aActivationType <> aActivationObscurationFlaming Then
                    aActivationType = ActivationbyObscuration
                End If
            Else
                If aActivationObscurationFlaming <> 0 Then
                    aActivationObscurationFlaming = 0
                    aActivationType = ActivationbyObscuration
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property ActivationTemperature() As Double
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aActivationTemperature)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aActivationTemperature Then
                aActivationTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                aChanged = True
            End If
            If aActivationType <> ActivationbyTemperature Then
                aActivationType = ActivationbyTemperature
            End If
        End Set
    End Property
    Public Property RTI() As Double
        Get
            Return myUnits.Convert(UnitsNum.RTI).FromSI(aRTI)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.RTI).ToSI(Value) <> aRTI Then
                aRTI = myUnits.Convert(UnitsNum.RTI).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property SprayDensity() As Double
        Get
            Return myUnits.Convert(UnitsNum.Velocity).FromSI(aSprayDensity)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Velocity).ToSI(Value) <> aSprayDensity Then
                aSprayDensity = myUnits.Convert(UnitsNum.Velocity).ToSI(Value)
                aChanged = True
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
    Public Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Public Sub SetPosition(ByVal XPosition As Double, ByVal YPosition As Double, ByVal ZPosition As Double)
        Me.XPosition = XPosition
        Me.YPosition = YPosition
        Me.ZPosition = ZPosition
    End Sub
    Public Sub SetPosition(ByVal XPosition As Double, ByVal YPosition As Double, ByVal ZPosition As Double, ByVal XNormal As Double, ByVal YNormal As Double, ByVal ZNormal As Double)
        Me.XPosition = XPosition
        Me.YPosition = YPosition
        Me.ZPosition = ZPosition
        Me.XNormal = XNormal
        Me.YNormal = YNormal
        Me.ZNormal = ZNormal
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal Material As String, ByVal SolutionType As Integer)
        ' This one defines a target
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(index)
            Compartment = index
            DetectorType = TypeTarget
            Me.Material = Material
            If SolutionType = Cylindrical Then
                Me.SolutionType = Cylindrical
            Else
                Me.SolutionType = ThermallyThick
            End If
            SetDefaultLocation()
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer)
        ' This defines a simple smoke detector
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Compartment = index
            DetectorType = TypeSmokeDetector
            ActivationTemperature = SmokeDetectorActivationTemperature
            RTI = SmokeDetectorRTI
            SetDefaultLocation()
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal ActivationTemperature As Double)
        ' This defines a simple heat detector (no water spray)
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Compartment = index
            DetectorType = TypeHeatDetector
            Me.ActivationTemperature = ActivationTemperature
            SetDefaultLocation()
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal DetectorType As Integer, ByVal ActivationTemperature As Double, ByVal RTI As Double, ByVal SprayDensity As Double)
        ' This is a detector with water spray
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Compartment = index
            Me.DetectorType = TypeSprinkler
            Me.ActivationTemperature = ActivationTemperature
            Me.RTI = RTI
            Me.SprayDensity = SprayDensity
            SetDefaultLocation()
        End If
    End Sub
    Public Sub SetConvectionCoefficients(ByVal isAdiabatic As Boolean, ByVal Coeff1 As Double, ByVal Coeff2 As Double)
        Me.Adiabatic = isAdiabatic
        Me.aConvectionCoefficients(1) = Coeff1
        Me.aConvectionCoefficients(2) = Coeff2
    End Sub
    Public Property Adiabatic() As Boolean
        Get
            Return aAdiabaticTarget
        End Get
        Set(value As Boolean)
            If value <> aAdiabaticTarget Then
                aAdiabaticTarget = value
                aChanged = True
            End If
        End Set
    End Property
    Public Property Convection_Coefficient(index As Integer) As Double
        Get
            If index >= 1 And index <= 2 Then
                Convection_Coefficient = aConvectionCoefficients(index)
            Else
                myErrors.Add("Target Convection coefficients specification out of bounds.", ErrorMessages.TypeFatal)
                HasErrors += 1
            End If
        End Get
        Set(value As Double)
            If value <> aConvectionCoefficients(index) Then
                aConvectionCoefficients(index) = value
                aChanged = True
            End If
        End Set
    End Property
    Public ReadOnly Property IsValid(ByVal TargetNumber As Integer) As Integer
        Get
            myUnits.SI = True
            HasErrors = 0
            Select Case aType
                Case TypeTarget
                    If aCompartment < 0 Or aCompartment > myCompartments.Count - 1 Then
                        myErrors.Add("Target " + TargetNumber.ToString + " is not assigned to an existing compartment. Select compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    Else
                        Dim aComp As New Compartment
                        aComp = myCompartments(aCompartment)
                        If aXPosition < 0.0 Or aXPosition > aComp.RoomWidth Then
                            myErrors.Add("Target " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment width.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aYPosition < 0.0 Or aYPosition > aComp.RoomDepth Then
                            myErrors.Add("Target " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment depth.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aZPosition < 0.0 Or aZPosition > aComp.RoomHeight Then
                            myErrors.Add("Target " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment height.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aXNormal < -1.0 Or aXNormal > 1.0 Or aYNormal < -1.0 Or aYNormal > 1.0 Or aZNormal < -1.0 Or aZNormal > 1.0 Then
                            myErrors.Add("Target " + TargetNumber.ToString + ". Value(s) for the normal vector must be between -1 and +1.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    End If
                    If myThermalProperties.GetIndex(aMaterial) < 0 Then
                        myErrors.Add("Target " + TargetNumber.ToString + " has an unknown surface material.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If myThermalProperties.GetIndex(aMaterial) >= 0 Then
                        Dim thickness As Double = myThermalProperties(myThermalProperties.GetIndex(aMaterial)).Thickness
                        If aThickness > 0 Then thickness = aThickness
                        If aInternalLocation > thickness Then
                            myErrors.Add("Target " + TargetNumber.ToString + ". Location for internal temperature is greater than target thickness.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        ElseIf aInternalLocation < 0 Then
                            myErrors.Add("Target " + TargetNumber.ToString + ". Location for internal temperature is less than zero.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    End If
                    If aXNormal <> 0 Or aYNormal <> 0 Or aZNormal <> 0 Then
                        If CheckTargetFacing(aTargetFacing) <> "-" Then
                            If aXNormal <> 0 Or aYNormal <> 0 Or aZNormal <> 0 Then
                                myErrors.Add("Target " + TargetNumber.ToString + ". Cannot specify both front face orientation and normal vector.", ErrorMessages.TypeFatal)
                            End If
                        End If
                    Else
                        If CheckTargetFacing(aTargetFacing) = "-" Then
                            myErrors.Add("Target " + TargetNumber.ToString + ". Front face orientation is not valid: " + aTargetFacing, ErrorMessages.TypeFatal)
                        End If
                    End If
                Case TypeDetector
                    If aCompartment < 0 Or aCompartment > myCompartments.Count - 1 Then
                        myErrors.Add("Detector " + TargetNumber.ToString + " is not assigned to an existing compartment. Select compartment.", ErrorMessages.TypeFatal)
                        HasErrors += 1
                    Else
                        Dim aComp As New Compartment
                        aComp = myCompartments(aCompartment)
                        If aXPosition < 0.0 Or aXPosition > aComp.RoomWidth Then
                            myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment width.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aYPosition < 0.0 Or aYPosition > aComp.RoomDepth Then
                            myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment depth.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                        If aZPosition < 0.0 Or aZPosition > aComp.RoomHeight Then
                            myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " width position is less than 0 m or greater than compartment height.", ErrorMessages.TypeFatal)
                            HasErrors += 1
                        End If
                    End If
                    If aActivationTemperature < 273.15 Or aActivationTemperature > 473.15 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " activation temperature is less than 20 °C or greater than 200 °C.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aActivationObscurationFlaming < 0 Or aActivationObscurationFlaming > 100 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " activation obscuration is less than 0 % or greater than 100 %/m.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aRTI <= 0.0 Or aRTI >= 500.0 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " RTI is less than 0 or greater than 500 (m s)^1/2.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aSprayDensity < 0.0 Or aSprayDensity > 0.003 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " Spray density is less than 0 or greater than 0.003 m/s.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
            End Select
            myUnits.SI = False
            Return HasErrors
        End Get
    End Property
End Class
Public Class TargetCollection
    Inherits System.Collections.CollectionBase
    Friend ReadOnly Maximum As Integer = Target.MaximumTargets
    Private i As Integer
    Private Haserror As Integer

    Public DoChange As Boolean = True
    Public Sub Add(ByVal aTarget As Target)
        List.Add(aTarget)
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromTarget As New Target, ToTarget As New Target
        FromTarget = CType(List.Item(indexFrom), Target)
        ToTarget.Type = FromTarget.Type
        ToTarget.Compartment = FromTarget.Compartment
        ToTarget.XPosition = FromTarget.XPosition
        ToTarget.YPosition = FromTarget.YPosition
        ToTarget.ZPosition = FromTarget.ZPosition
        ToTarget.XNormal = FromTarget.XNormal
        ToTarget.YNormal = FromTarget.YNormal
        ToTarget.ZNormal = FromTarget.ZNormal
        ToTarget.Material = FromTarget.Material
        ToTarget.Thickness = FromTarget.Thickness
        ToTarget.SolutionType = FromTarget.SolutionType
        ToTarget.DetectorType = FromTarget.DetectorType
        ToTarget.ActivationTemperature = FromTarget.ActivationTemperature
        ToTarget.RTI = FromTarget.RTI
        ToTarget.SprayDensity = FromTarget.SprayDensity
        ToTarget.InternalLocation = FromTarget.InternalLocation
        List.Item(indexTo) = ToTarget
    End Sub
    Public Sub Swap(ByVal index1 As Integer, ByVal index2 As Integer)
        If index1 >= 0 And index1 < Count And index2 >= 0 And index2 < Count Then
            Dim temp As New Target
            temp = CType(List.Item(index2), Target)
            List.Item(index2) = List.Item(index1)
            List.Item(index1) = temp
        End If
    End Sub
    Public Sub Remove(ByVal index As Integer)
        ' make sure that the Target number is valid
        If index > Count - 1 Or index < 0 Then
            System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Target number not found.")
        Else
            List.RemoveAt(index)
        End If
    End Sub

    Public Sub RemoveAll(ByVal index As Integer)
        ' Removes all targets associated with the compartment specified by index
        If index >= 0 And index < myCompartments.Count Then
            Dim aTarget As New Target
            i = 0
            While i < Count
                aTarget = CType(List.Item(i), Target)
                If aTarget.Compartment = index Then
                    List.RemoveAt(i)
                    i = 0
                Else
                    i += 1
                End If
            End While

        End If
    End Sub
    Public Sub Renumber(ByVal Oldindex As Integer, ByVal Newindex As Integer)
        Dim aTarget As New Target
        For i = 0 To Count - 1
            aTarget = CType(List.Item(i), Target)
            If aTarget.Compartment = Oldindex Then aTarget.Compartment = Newindex
            List.Item(i) = aTarget
        Next
    End Sub
    Default Public Property Item(ByVal index As Integer) As Target
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Target number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aTarget As New Target
                Return aTarget
            Else
                Return CType(List.Item(index), Target)
            End If
        End Get
        Set(ByVal Value As Target)
            List.Item(index) = Value
        End Set
    End Property
    Public ReadOnly Property GetIndex(ByVal Name As String) As Integer
        Get
            If Count > 0 Then
                For i = 0 To Count - 1
                    If Item(i).Name = Name Then
                        Return i
                        Exit Property
                    End If
                Next
            End If
            Return -1
        End Get
    End Property
    Public ReadOnly Property NumberofConnections(ByVal index As Integer) As Integer
        Get
            Dim aTarget As Target
            Dim numTargets As Integer = 0
            numTargets = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aTarget = CType(List.Item(i), Target)
                    If aTarget.Compartment = index Then numTargets += 1
                Next
            End If
            Return numTargets
        End Get
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aTarget As Target
                For i = 0 To Count - 1
                    aTarget = CType(List.Item(i), Target)
                    If aTarget.Changed Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property ValidTarget(ByVal Name As String) As Boolean
        Get
            If Count > 0 Then
                Dim aTarget As Target
                For i = 0 To Count - 1
                    aTarget = CType(List(i), Target)
                    If aTarget.Name = Name Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            Haserror = 0
            If Count > 0 Then
                Dim aTarget As Target
                For i = 0 To Count - 1
                    aTarget = CType(List(i), Target)
                    Haserror += aTarget.IsValid(i + 1)
                Next
            End If
            Return Haserror
        End Get
    End Property
End Class
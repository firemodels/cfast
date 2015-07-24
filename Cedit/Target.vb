Public Class Target

    Friend Const TypeTarget As Integer = 0
    Friend Const TypeDetector As Integer = 1
    Friend Const SmokeDetectorActivationTemperature As Single = 30.0    ' Smoke detector simulated as a heat detector with 10 °C rise trigger with RTI = 5
    Friend Const SmokeDetectorRTI As Single = 5.0
    Friend Const HeatDetectorActiviationTemperature As Single = 57.22   ' Default heat detector is a 135 °F detector with 10 ft spacing from NRC NUREG 1085, table 12-2
    Friend Const HeatDetectorRTI As Single = 404.0
    Friend Const SprinklerActivationTemperature As Single = 73.89       ' Default sprinkler is standard response link, ordinary sprinkler, RTI = 130 and 165 °F
    Friend Const SprinklerRTI As Single = 130
    Friend Const Implicit As Integer = 0
    Friend Const Explicit As Integer = 1
    Friend Const Steady As Integer = 2
    Friend Const ThermallyThick As Integer = 0
    Friend Const Cylindrical As Integer = 1
    Friend Const TypeHeatDetector As Integer = 1                    ' Type 0 is a normal target (type from above), 1 is a heat detector, 2 is a smoke detector and 3 is a sprinkler
    Friend Const TypeSmokeDetector As Integer = 0
    Friend Const TypeSprinkler As Integer = 2
    Friend Const MaximumTargets As Integer = 100

    ' All units within the class are assumed to be consistent and typically SI
    Private aType As Integer                    ' 0 for target and 1 for detector
    Private aName As String                     ' Target name (at the moment, only used for targets, not detectors)
    Private aCompartment As Integer             ' Compartment number where target or detector is located
    Private aXPosition As Single                ' X Position of target or detector
    Private aYPosition As Single                ' Y Position of target or detector
    Private aZPosition As Single                ' Z Position of target or detector
    Private aXNormal As Single                  ' X component of normal vector from chosen surface of target
    Private aYNormal As Single                  ' Y component of normal vector from chosen surface of target
    Private aZNormal As Single                  ' Z component of normal vector from chosen surface of target
    Private aInternalLocation As Single         ' Location for reporting of internal temperature as a fraction of thickness. Default is 0.5 (center)
    Private aMaterial As String                 ' Target material from material database
    Private aSolutionType As Integer            ' 0 for thermally thick plate which is the PDE option or 1 for cylindrical coordinates.  ODE is no loger supported
    Private aSolutionMethod As Integer          ' 0 for implicit solution or 1 for explicit solution or 2 for steady solution
    Private aDetectorType As Integer            ' 0 for heat detector, 1 for smoke detector, 2 for sprinkler 
    Private aActivationTemperature As Single    ' Activation temperature for all detector types.  Default depends on type
    Private aRTI As Single                      ' Detector RTI value
    Private aSprayDensity As Single             ' Sprinkler spray density
    Private aChanged As Boolean = False         ' True once compartment information has changed
    Private HasErrors As Integer                ' Temp variable that holds error count during error check

    Public Sub New()
        aName = ""
        aCompartment = -2
        aXPosition = -1.0
        aYPosition = -1.0
        aZPosition = -1.0
        aType = TypeTarget
        aXNormal = 0.0
        aYNormal = 0.0
        aZNormal = 1.0
        aInternalLocation = 0.5
        aMaterial = "Off"
        aSolutionType = 0
        aSolutionMethod = 0
        aDetectorType = 1
        aActivationTemperature = 10.0 + 273.15
        aRTI = 100
        aSprayDensity = 0.00007
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
    Public Property Compartment() As Integer
        Get
            Return aCompartment
        End Get
        Set(ByVal Value As Integer)
            If Value <> aCompartment Then
                aCompartment = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property XPosition() As Single
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
    Public Property YPosition() As Single
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
    Public Property ZPosition() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aZPosition)
        End Get
        Set(ByVal Value As Single)
            If Value >= 0 Then
                If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aZPosition Then
                    aZPosition = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                    aChanged = True
                End If
            Else
                If aCompartment > -1 And aCompartment <= myCompartments.Count - 1 Then
                    aZPosition = 0.0
                    If aType = TypeDetector Then
                        Dim tmpCompartment As New Compartment
                        tmpCompartment = myCompartments.Item(aCompartment)
                        aZPosition = myUnits.Convert(UnitsNum.Length).ToSI(tmpCompartment.RoomHeight) * 0.99
                    Else
                        aZPosition = 0.0
                    End If
                    aChanged = True
                Else
                    aZPosition = Value
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Public Property XNormal() As Single
        Get
            Return aXNormal
        End Get
        Set(ByVal Value As Single)
            If Value <> aXNormal Then
                aXNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property YNormal() As Single
        Get
            Return aYNormal
        End Get
        Set(ByVal Value As Single)
            If Value <> aYNormal Then
                aYNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property ZNormal() As Single
        Get
            Return aZNormal
        End Get
        Set(ByVal Value As Single)
            If Value <> aZNormal Then
                aZNormal = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property InternalLocation() As Single
        Get
            Return aInternalLocation
        End Get
        Set(ByVal Value As Single)
            If Value <> aInternalLocation Then
                aInternalLocation = Value
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
                Me.aMaterial = myThermalProperties.ValidThermalProperty(Value, "Target")
                If myTargets.DoChange Then aChanged = True
            End If
        End Set
    End Property
    Public Property SolutionMethod() As Integer
        Get
            Return aSolutionMethod
        End Get
        Set(ByVal Value As Integer)
            If Value <> aSolutionMethod Then
                Me.aSolutionMethod = Value
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
                Me.aSolutionType = Value
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
                aChanged = True
            End If
        End Set
    End Property
    Public Property ActivationTemperature() As Single
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aActivationTemperature)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aActivationTemperature Then
                aActivationTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property RTI() As Single
        Get
            Return myUnits.Convert(UnitsNum.RTI).FromSI(aRTI)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.RTI).ToSI(Value) <> aRTI Then
                aRTI = myUnits.Convert(UnitsNum.RTI).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Public Property SprayDensity() As Single
        Get
            Return myUnits.Convert(UnitsNum.Velocity).FromSI(aSprayDensity)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Velocity).ToSI(Value) <> aSprayDensity Then
                aSprayDensity = myUnits.Convert(UnitsNum.Velocity).ToSI(Value)
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
    Public Sub SetPosition(ByVal XPosition As Single, ByVal YPosition As Single, ByVal ZPosition As Single)
        Me.XPosition = XPosition
        Me.YPosition = YPosition
        Me.ZPosition = ZPosition
    End Sub
    Public Sub SetPosition(ByVal XPosition As Single, ByVal YPosition As Single, ByVal ZPosition As Single, ByVal XNormal As Single, ByVal YNormal As Single, ByVal ZNormal As Single)
        Me.XPosition = XPosition
        Me.YPosition = YPosition
        Me.ZPosition = ZPosition
        Me.XNormal = XNormal
        Me.YNormal = YNormal
        Me.ZNormal = ZNormal
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal Material As String, ByVal SolutionType As Integer, ByVal SolutionMethod As Integer)
        ' This one defines a target
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(index)
            Me.Compartment = index
            Me.DetectorType = TypeTarget
            Me.Material = Material
            If SolutionType = Cylindrical Then
                Me.SolutionType = Cylindrical
            Else
                Me.SolutionType = ThermallyThick
            End If
            If SolutionMethod = Explicit Then
                Me.SolutionMethod = Explicit
            ElseIf SolutionMethod = Steady Then
                Me.SolutionMethod = Steady
            Else
                Me.SolutionMethod = Implicit
            End If
            If Me.XPosition = -1 Then Me.XPosition = aCompartment.RoomWidth / 2
            If Me.YPosition = -1 Then Me.YPosition = aCompartment.RoomDepth / 2
            If Me.ZPosition = -1 Then Me.ZPosition = 0.0
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer)
        ' This defines a simple smoke detector
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Me.Compartment = index
            Me.DetectorType = TypeSmokeDetector
            Me.ActivationTemperature = SmokeDetectorActivationTemperature
            Me.RTI = SmokeDetectorRTI
            If Me.XPosition = -1 Then Me.XPosition = aCompartment.RoomWidth / 2
            If Me.YPosition = -1 Then Me.YPosition = aCompartment.RoomDepth / 2
            If Me.ZPosition = -1 Then Me.ZPosition = aCompartment.RoomHeight
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal ActivationTemperature As Single)
        ' This defines a simple heat detector (no water spray)
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Me.Compartment = index
            Me.DetectorType = TypeHeatDetector
            Me.ActivationTemperature = ActivationTemperature
            If Me.XPosition = -1 Then Me.XPosition = aCompartment.RoomWidth / 2
            If Me.YPosition = -1 Then Me.YPosition = aCompartment.RoomDepth / 2
            If Me.ZPosition = -1 Then Me.ZPosition = aCompartment.RoomHeight
        End If
    End Sub
    Public Sub SetTarget(ByVal index As Integer, ByVal DetectorType As Integer, ByVal ActivationTemperature As Single, ByVal RTI As Single, ByVal SprayDensity As Single)
        ' This is a detector with water spray
        Dim aCompartment As Compartment
        If index <= myCompartments.Count - 1 Then
            aCompartment = myCompartments.Item(Compartment)
            Me.Compartment = index
            Me.DetectorType = TypeSprinkler
            Me.ActivationTemperature = ActivationTemperature
            Me.RTI = RTI
            Me.SprayDensity = SprayDensity
            If Me.XPosition = -1 Then Me.XPosition = aCompartment.RoomWidth / 2
            If Me.YPosition = -1 Then Me.YPosition = aCompartment.RoomDepth / 2
            If Me.ZPosition = -1 Then Me.ZPosition = aCompartment.RoomHeight
        End If
    End Sub
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
                    If aRTI <= 0.0 Or aRTI >= 400.0 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " RTI is less than 0 or greater than 400 (m s)^1/2.", ErrorMessages.TypeWarning)
                        HasErrors += 1
                    End If
                    If aSprayDensity < 0.0 Or aSprayDensity > 0.003 Then
                        myErrors.Add("Detector/Sprinkler " + TargetNumber.ToString + " spray density is less than 0 or greater than 0.003 m/s.", ErrorMessages.TypeWarning)
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
        ToTarget.SolutionType = FromTarget.SolutionType
        ToTarget.SolutionMethod = FromTarget.SolutionMethod
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
                For i = 0 To Me.Count - 1
                    If Me.Item(i).Name = Name Then
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
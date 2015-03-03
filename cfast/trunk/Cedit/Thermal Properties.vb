Public Class ThermalProperty
    Friend Const MaximumProperties As Integer = 125
    ' All units within the class are assumed to be consistent and typically SI
    Private aShortName As String          ' One word name for material
    Private aName As String               ' Material Name
    Private aConductivity As Single       ' Thermal conductivity
    Private aSpecificHeat As Single       ' Specific Heat
    Private aDensity As Single            ' Density in kg/m^3
    Private aThickness As Single          ' Material Thickness in m
    Private aEmissivity As Single         ' Emissivity
    Private Const HClDim As Integer = 6              '
    Private aHClDeposition(HClDim) As Single   ' HCl Deposition Coefficients
    Private aChanged As Boolean           ' Indicates if record has been changed
    Public Sub New()
        aShortName = ""
        aName = ""
        aConductivity = 0.16
        aSpecificHeat = 900.0
        aDensity = 790.0
        aThickness = 0.016
        aEmissivity = 0.9
        Dim i As Integer
        For i = 0 To HClDim
            aHClDeposition(i) = 0.0
        Next
        aChanged = True
    End Sub
    Public Sub New(ByVal ShortName As String, ByVal Name As String, ByVal Conductivity As Single, ByVal SpecificHeat As Single, _
    ByVal Density As Single, ByVal Thickness As Single, ByVal Emissivity As Single)
        Me.ShortName = ShortName
        Me.Name = Name
        Me.Conductivity = Conductivity
        Me.SpecificHeat = SpecificHeat
        Me.Density = Density
        Me.Thickness = Thickness
        Me.Emissivity = Emissivity
        Dim i As Integer
        For i = 0 To HClDim
            Me.HClDeposition(i) = 0.0
        Next
        aChanged = True
    End Sub
    Public Sub GetThermalProperties(ByRef ShortName As String, ByRef Name As String, ByRef Conductivity As Single, ByRef SpecificHeat As Single, _
    ByRef Density As Single, ByRef Thickness As Single, ByRef Emissivity As Single)
        ShortName = Me.ShortName
        Name = Me.Name
        Conductivity = Me.Conductivity
        SpecificHeat = Me.SpecificHeat
        Density = Me.Density
        Thickness = Me.Thickness
        Emissivity = Me.Emissivity
    End Sub
    Friend Property ShortName() As String
        Get
            Return aShortName
        End Get
        Set(ByVal Value As String)
            If Value <> aShortName Then
                aShortName = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Name() As String
        Get
            Return aName
        End Get
        Set(ByVal Value As String)
            If Value <> aName Then
                aName = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property HClDeposition(ByVal i As Integer) As Single
        Get
            Return aHClDeposition(i)
        End Get
        Set(ByVal Value As Single)
            If Value <> aHClDeposition(i) Then
                aHClDeposition(i) = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Emissivity() As Single
        Get
            Return aEmissivity
        End Get
        Set(ByVal Value As Single)
            If Value <> aEmissivity Then
                aEmissivity = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Thickness() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aThickness)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aThickness Then
                aThickness = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Density() As Single
        Get
            Return myUnits.Convert(UnitsNum.Density).FromSI(aDensity)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Density).ToSI(Value) <> aDensity Then
                aDensity = myUnits.Convert(UnitsNum.Density).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property SpecificHeat() As Single
        Get
            Return myUnits.Convert(UnitsNum.SpecificHeat).FromSI(aSpecificHeat)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.SpecificHeat).ToSI(Value) <> aSpecificHeat Then
                aSpecificHeat = myUnits.Convert(UnitsNum.SpecificHeat).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Conductivity() As Single
        Get
            Return myUnits.Convert(UnitsNum.Conductivity).FromSI(aConductivity)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Conductivity).ToSI(Value) <> aConductivity Then
                aConductivity = myUnits.Convert(UnitsNum.Conductivity).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Public Sub GetHCl(ByRef HClCoefficients() As Single)
        Dim i As Integer
        ReDim HClCoefficients(HClDim)
        For i = 0 To HClDim
            HClCoefficients(i) = Me.HClDeposition(i)
        Next
    End Sub
    Public Sub SetHCl(ByVal HClCoefficients() As Single)
        Dim i As Integer
        If HClCoefficients.GetUpperBound(0) = HClDim Then
            For i = 0 To HClDim
                Me.HClDeposition(i) = HClCoefficients(i)
            Next
        End If
        aChanged = True
    End Sub
    Public ReadOnly Property IsValid() As String
        ' Checks the overall validity of the current compartment specification
        Get
            Return ""
        End Get
    End Property
End Class
Public Class ThermalPropertiesCollection
    Inherits System.Collections.CollectionBase
    Public ReadOnly Maximum As Integer = ThermalProperty.MaximumProperties
    Private i As Integer, FoundIndex As Integer
    Private aFileName As String = "thermal"
    Private aFileChanged As Boolean = False
    Property FileName() As String
        Get
            Return System.IO.Path.GetFileNameWithoutExtension(Me.aFileName)
        End Get
        Set(ByVal Value As String)
            If Value <> Me.aFileName Then
                Me.aFileName = System.IO.Path.GetFileNameWithoutExtension(Value) + ".csv"
                Me.aFileChanged = True
            End If
        End Set
    End Property
    Property FileChanged() As Boolean
        Get
            Return Me.aFileChanged
        End Get
        Set(ByVal Value As Boolean)
            Me.aFileChanged = Value
        End Set
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aThermal As ThermalProperty
                For i = 0 To Count - 1
                    aThermal = CType(List.Item(i), ThermalProperty)
                    If aThermal.Changed Then Return True
                Next
            End If
            If Me.FileChanged = True Then Return True
            Return False
        End Get
    End Property
    Public Sub Add(ByVal aThermal As ThermalProperty)
        Dim i As Integer
        If Count > 0 Then
            For i = 0 To Count - 1
                If aThermal.ShortName = Me.Item(i).ShortName Then
                    Exit Sub
                End If
            Next
        End If
        List.Add(aThermal)
    End Sub
    Public Sub Remove(ByVal index As Integer)
        ' make sure that the compartment number is valid
        If index > Count - 1 Or index < 0 Then
            System.Windows.Forms.MessageBox.Show("Internal Error: Thermal property number not found.")
        Else
            List.RemoveAt(index)
        End If
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromMaterial As New ThermalProperty, ToMaterial As New ThermalProperty
        FromMaterial = CType(List.Item(indexFrom), ThermalProperty)
        ToMaterial.ShortName = FromMaterial.ShortName
        ToMaterial.Name = FromMaterial.Name
        ToMaterial.Conductivity = FromMaterial.Conductivity
        ToMaterial.SpecificHeat = FromMaterial.SpecificHeat
        ToMaterial.Density = FromMaterial.Density
        ToMaterial.Thickness = FromMaterial.Thickness
        ToMaterial.Emissivity = FromMaterial.Emissivity
        Dim Vector1() As Single = {0}
        FromMaterial.GetHCl(Vector1)
        ToMaterial.SetHCl(Vector1)
        List.Item(indexTo) = ToMaterial
    End Sub
    Default Public Property Item(ByVal index As Integer) As ThermalProperty
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error ThermalPropertiesColletion.Item.get. Item number not found.")
            Else
                Return CType(List.Item(index), ThermalProperty)
            End If
        End Get
        Set(ByVal Value As ThermalProperty)
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error ThermalPropertiesColletion.Item.get. Item number not found.")
            Else
                List.Item(index) = Value
            End If
        End Set
    End Property
    Public ReadOnly Property GetLongName(ByVal Shortname As String) As String
        Get
            If Count > 0 Then
                For i = 0 To Me.Count - 1
                    If Me.Item(i).ShortName = Shortname Then
                        Return Me.Item(i).Name
                        Exit Property
                    End If
                Next
                Return "Off"
            Else
                Return ""
            End If
        End Get
    End Property
    Public ReadOnly Property GetShortName(ByVal aName As String) As String
        Get
            FoundIndex = -1
            If Count > 0 Then
                For i = 0 To Count - 1
                    If Me(i).Name = aName Then
                        Return Me(i).ShortName
                        Exit Property
                    End If
                Next
            End If
            Return "Off"
        End Get
    End Property
    Public ReadOnly Property GetIndex(ByVal Shortname As String) As Integer
        Get
            If Count > 0 Then
                For i = 0 To Me.Count - 1
                    If Me.Item(i).ShortName = Shortname Then
                        Return i
                        Exit Property
                    End If
                Next
                If Shortname.ToUpper = "OFF" Then
                    Return -1
                Else
                    Return -2
                End If
            End If
        End Get
    End Property
    Public ReadOnly Property NumberofConnections(ByVal aShortName As String) As Integer
        Get
            Dim numUses As Integer = 0
            If myCompartments.Count > 0 Then
                Dim aCompartment As Compartment
                For i = 0 To myCompartments.Count - 1
                    aCompartment = myCompartments.Item(i)
                    If aCompartment.CeilingMaterial = aShortName Then numUses = numUses + 1
                    If aCompartment.FloorMaterial = aShortName Then numUses = numUses + 1
                    If aCompartment.WallMaterial = aShortName Then numUses = numUses + 1
                Next
            End If
            If myTargets.Count > 0 Then
                Dim aTarget As New Target
                For i = 0 To myTargets.Count - 1
                    aTarget = myTargets.Item(i)
                    If aTarget.Material = aShortName Then numUses = numUses + 1
                Next
            End If
            Return numUses
        End Get
    End Property
End Class
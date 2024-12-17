Public Class User_Units
    Inherits System.Windows.Forms.Form
    Private BaseUnitsDimension As Integer = 6
    Private cArray(BaseUnitsDimension) As ComboBox, CurrentUnits(BaseUnitsDimension) As Integer, EnteringUnits(BaseUnitsDimension) As Integer
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents UnitsSmoke As System.Windows.Forms.ComboBox

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        InitUser_Units()

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents Time As System.Windows.Forms.Label
    Friend WithEvents UnitsTime As System.Windows.Forms.ComboBox
    Friend WithEvents UnitsTemperature As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents UnitsOK As System.Windows.Forms.Button
    Friend WithEvents UnitsDefault As System.Windows.Forms.Button
    Friend WithEvents UnitsPressure As System.Windows.Forms.ComboBox
    Friend WithEvents UnitsMass As System.Windows.Forms.ComboBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents UnitsLength As System.Windows.Forms.ComboBox
    Friend WithEvents UnitsEnergy As System.Windows.Forms.ComboBox
    Friend WithEvents UnitsCancel As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(User_Units))
        Time = New System.Windows.Forms.Label()
        UnitsTime = New System.Windows.Forms.ComboBox()
        UnitsTemperature = New System.Windows.Forms.ComboBox()
        Label1 = New System.Windows.Forms.Label()
        UnitsPressure = New System.Windows.Forms.ComboBox()
        Label9 = New System.Windows.Forms.Label()
        UnitsEnergy = New System.Windows.Forms.ComboBox()
        Label18 = New System.Windows.Forms.Label()
        UnitsMass = New System.Windows.Forms.ComboBox()
        Label3 = New System.Windows.Forms.Label()
        UnitsOK = New System.Windows.Forms.Button()
        UnitsDefault = New System.Windows.Forms.Button()
        UnitsCancel = New System.Windows.Forms.Button()
        UnitsLength = New System.Windows.Forms.ComboBox()
        Label4 = New System.Windows.Forms.Label()
        Label2 = New System.Windows.Forms.Label()
        UnitsSmoke = New System.Windows.Forms.ComboBox()
        SuspendLayout()
        '
        'Time
        '
        Time.AutoSize = True
        Time.Location = New System.Drawing.Point(128, 89)
        Time.Name = "Time"
        Time.Size = New System.Drawing.Size(33, 13)
        Time.TabIndex = 0
        Time.Text = "Time:"
        Time.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsTime
        '
        UnitsTime.Location = New System.Drawing.Point(171, 85)
        UnitsTime.Name = "UnitsTime"
        UnitsTime.Size = New System.Drawing.Size(96, 21)
        UnitsTime.TabIndex = 2
        '
        'UnitsTemperature
        '
        UnitsTemperature.Location = New System.Drawing.Point(171, 117)
        UnitsTemperature.Name = "UnitsTemperature"
        UnitsTemperature.Size = New System.Drawing.Size(96, 21)
        UnitsTemperature.TabIndex = 3
        '
        'Label1
        '
        Label1.AutoSize = True
        Label1.Location = New System.Drawing.Point(91, 121)
        Label1.Name = "Label1"
        Label1.Size = New System.Drawing.Size(70, 13)
        Label1.TabIndex = 2
        Label1.Text = "Temperature:"
        Label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsPressure
        '
        UnitsPressure.Location = New System.Drawing.Point(171, 149)
        UnitsPressure.Name = "UnitsPressure"
        UnitsPressure.Size = New System.Drawing.Size(96, 21)
        UnitsPressure.TabIndex = 4
        '
        'Label9
        '
        Label9.AutoSize = True
        Label9.Location = New System.Drawing.Point(110, 153)
        Label9.Name = "Label9"
        Label9.Size = New System.Drawing.Size(51, 13)
        Label9.TabIndex = 18
        Label9.Text = "Pressure:"
        Label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsEnergy
        '
        UnitsEnergy.Location = New System.Drawing.Point(171, 181)
        UnitsEnergy.Name = "UnitsEnergy"
        UnitsEnergy.Size = New System.Drawing.Size(96, 21)
        UnitsEnergy.TabIndex = 6
        '
        'Label18
        '
        Label18.AutoSize = True
        Label18.Location = New System.Drawing.Point(35, 185)
        Label18.Name = "Label18"
        Label18.Size = New System.Drawing.Size(126, 13)
        Label18.TabIndex = 36
        Label18.Text = "Energy, Quantity of Heat:"
        Label18.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsMass
        '
        UnitsMass.Location = New System.Drawing.Point(171, 53)
        UnitsMass.Name = "UnitsMass"
        UnitsMass.Size = New System.Drawing.Size(96, 21)
        UnitsMass.TabIndex = 1
        '
        'Label3
        '
        Label3.AutoSize = True
        Label3.Location = New System.Drawing.Point(126, 57)
        Label3.Name = "Label3"
        Label3.Size = New System.Drawing.Size(35, 13)
        Label3.TabIndex = 42
        Label3.Text = "Mass:"
        Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsOK
        '
        UnitsOK.DialogResult = System.Windows.Forms.DialogResult.OK
        UnitsOK.Location = New System.Drawing.Point(18, 258)
        UnitsOK.Name = "UnitsOK"
        UnitsOK.Size = New System.Drawing.Size(75, 23)
        UnitsOK.TabIndex = 7
        UnitsOK.Text = "OK"
        '
        'UnitsDefault
        '
        UnitsDefault.Location = New System.Drawing.Point(210, 258)
        UnitsDefault.Name = "UnitsDefault"
        UnitsDefault.Size = New System.Drawing.Size(75, 23)
        UnitsDefault.TabIndex = 9
        UnitsDefault.Text = "Reset"
        '
        'UnitsCancel
        '
        UnitsCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        UnitsCancel.Location = New System.Drawing.Point(114, 258)
        UnitsCancel.Name = "UnitsCancel"
        UnitsCancel.Size = New System.Drawing.Size(75, 23)
        UnitsCancel.TabIndex = 8
        UnitsCancel.Text = "Cancel"
        '
        'UnitsLength
        '
        UnitsLength.Location = New System.Drawing.Point(171, 21)
        UnitsLength.Name = "UnitsLength"
        UnitsLength.Size = New System.Drawing.Size(96, 21)
        UnitsLength.TabIndex = 0
        '
        'Label4
        '
        Label4.AutoSize = True
        Label4.Location = New System.Drawing.Point(118, 25)
        Label4.Name = "Label4"
        Label4.Size = New System.Drawing.Size(43, 13)
        Label4.TabIndex = 4
        Label4.Text = "Length:"
        Label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label2
        '
        Label2.AutoSize = True
        Label2.Location = New System.Drawing.Point(58, 217)
        Label2.Name = "Label2"
        Label2.Size = New System.Drawing.Size(103, 13)
        Label2.TabIndex = 44
        Label2.Text = "Smoke Obscuration:"
        Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsSmoke
        '
        UnitsSmoke.Location = New System.Drawing.Point(171, 213)
        UnitsSmoke.Name = "UnitsSmoke"
        UnitsSmoke.Size = New System.Drawing.Size(96, 21)
        UnitsSmoke.TabIndex = 43
        '
        'User_Units
        '
        AcceptButton = UnitsOK
        AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        CancelButton = UnitsCancel
        ClientSize = New System.Drawing.Size(302, 302)
        Controls.Add(Label2)
        Controls.Add(UnitsSmoke)
        Controls.Add(UnitsCancel)
        Controls.Add(UnitsDefault)
        Controls.Add(UnitsOK)
        Controls.Add(UnitsMass)
        Controls.Add(Label3)
        Controls.Add(Label18)
        Controls.Add(Label9)
        Controls.Add(Label1)
        Controls.Add(Time)
        Controls.Add(Label4)
        Controls.Add(UnitsEnergy)
        Controls.Add(UnitsPressure)
        Controls.Add(UnitsTemperature)
        Controls.Add(UnitsTime)
        Controls.Add(UnitsLength)
        FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Name = "User_Units"
        ShowInTaskbar = False
        StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Text = "User Units"
        ResumeLayout(False)
        PerformLayout()

    End Sub

#End Region

    Private Sub InitUser_Units()
        Dim i As Integer, j As Integer, Labels() As String = {" "}, aLabel As String
        ' This is called by the New method for the form, usually hidden in the design-generated code

        ' Define an array of the base units
        cArray(0) = UnitsLength
        cArray(1) = UnitsMass
        cArray(2) = UnitsTime
        cArray(3) = UnitsTemperature
        cArray(4) = UnitsPressure
        cArray(5) = UnitsEnergy
        cArray(6) = UnitsSmoke
        ' Fill the combo boxes with base unit labels
        For i = 0 To BaseUnitsDimension
            myUnits.BaseUnits(i).GetAllUnits(Labels)
            For j = 0 To Labels.GetUpperBound(0)
                aLabel = Labels(j)
                cArray(i).Items.AddRange(New Object() {aLabel})
            Next
            cArray(i).SelectedIndex = 0
            CurrentUnits(i) = 0
        Next
        Dim RegistryUnits(,) As String
        RegistryUnits = GetAllSettings("CFAST", "Units")
        Try
            If RegistryUnits.GetUpperBound(0) = BaseUnitsDimension Then
                For i = 0 To BaseUnitsDimension
                    CurrentUnits(i) = Val(RegistryUnits(i, 1))
                    cArray(i).SelectedIndex = CurrentUnits(i)
                Next
            End If
        Catch ex As Exception
        End Try

        myUnits.InitConversionFactors(CurrentUnits)
    End Sub
    Private Sub User_Units_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim i As Integer
        For i = 1 To BaseUnitsDimension
            EnteringUnits(i) = CurrentUnits(i)
        Next
    End Sub
    Private Sub UnitsDefault_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsDefault.Click
        Dim i As Integer, Zero() As Integer = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        For i = 0 To BaseUnitsDimension
            cArray(i).SelectedIndex = 0
        Next
        myUnits.InitConversionFactors(Zero)
    End Sub
    Private Sub Units_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsLength.SelectedIndexChanged, UnitsMass.SelectedIndexChanged, _
        UnitsTime.SelectedIndexChanged, UnitsTemperature.SelectedIndexChanged, UnitsPressure.SelectedIndexChanged, UnitsEnergy.SelectedIndexChanged, UnitsSmoke.SelectedIndexChanged
        Dim i As Integer
        For i = 0 To BaseUnitsDimension
            CurrentUnits(i) = cArray(i).SelectedIndex
        Next
        myUnits.InitConversionFactors(CurrentUnits)
    End Sub
    Private Sub UnitsCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsCancel.Click
        Dim i As Integer, Zero() As Integer = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        For i = 0 To BaseUnitsDimension
            cArray(i).SelectedIndex = EnteringUnits(i)
        Next
        myUnits.InitConversionFactors(Zero)
    End Sub
    Private Sub UnitsOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsOK.Click
        Dim i As Integer
        Dim UnitNames() As String = {"Length", "Mass", "Time", "Temperature", "Pressure", "Energy", "Smoke"}
        For i = 0 To BaseUnitsDimension
            SaveSetting("CFAST", "Units", UnitNames(i), CurrentUnits(i))
        Next
    End Sub
End Class
Public Class EngineeringUnits
    Public BaseUnits(6) As Conversions
    Public Convert(18) As Conversion
    Public ConvertFireData(12) As Conversion
    Private aSI As Boolean = False, aSITemp As Boolean, SIStack As New Stack
    Private aValue As Double
    Private i As Integer
    Public Property SI() As Boolean
        Get
            If SIStack.Count > 0 Then
                Return SIStack.Peek()
            Else
                Return False
            End If
        End Get
        Set(ByVal Value As Boolean)
            If Value = False Then
                If SIStack.Count > 0 Then aSITemp = SIStack.Pop()
            Else
                SIStack.Push(Value)
            End If
        End Set
    End Property
    Public Sub New()
        Dim Zero() As Double = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        Dim DefaultUnits() As Integer = {0, 0, 0, 0, 0, 0, 0, 0}
        ' These conversion factors are all from the given units to SI according to NIST SP 811
        ' All the conversions except smoke are of the form y=(x+b)*m, though usually b is 0.0
        ' Smoke is a special case to properly convert % obscuration from %/m to/from %/m

        ' Length conversions
        Dim mLength() As Double = {1.0, 0.01, 0.001, 0.3048, 0.0254}
        Dim LLength() As String = {"m", "cm", "mm", "ft", "in"}
        Dim aLengthConversion As New Conversions(mLength, Zero, LLength)
        BaseUnits(BaseUnitsNum.Length) = aLengthConversion
        ' Mass conversions
        Dim MMass() As Double = {1.0, 0.001, 0.4535924, 0.02834952}
        Dim LMass() As String = {"kg", "g", "lb", "oz"}
        Dim aMassConversion As New Conversions(MMass, Zero, LMass)
        BaseUnits(BaseUnitsNum.Mass) = aMassConversion
        ' Time conversions
        Dim MTime() As Double = {1.0, 60.0, 3600.0}
        Dim LTime() As String = {"s", "min", "h"}
        Dim aTimeConversion As New Conversions(MTime, Zero, LTime)
        BaseUnits(BaseUnitsNum.Time) = aTimeConversion
        ' Temperature conversions
        Dim MTemperature() As Double = {1.0, 1.0, 0.5555556, 0.5555556}, BTemperature() As Double = {273.15, 0.0, 459.67, 0.0}
        MTemperature(2) = 5.0 / 9.0 ' to correct a round-off error in display of �C, we'll use the full precision for this constant
        MTemperature(3) = 5.0 / 9.0
        Dim LTemperature() As String = {"�C", "K", "�F", "�R"}
        Dim aTemperatureConversion As New Conversions(MTemperature, BTemperature, LTemperature)
        BaseUnits(BaseUnitsNum.Temperature) = aTemperatureConversion
        ' Pressure conversions
        Dim MPressure() As Double = {1.0, 133.3224, 249.0889, 101325.0}
        Dim Lpressure() As String = {"Pa", "mm Hg", "in H2O", "atm"}
        Dim aPressureConversion As New Conversions(MPressure, Zero, Lpressure)
        BaseUnits(BaseUnitsNum.Pressure) = aPressureConversion
        ' Energy conversions
        Dim MEnergy() As Double = {1000.0, 1.0, 1000000.0, 1054.35, 4.184}
        Dim LEnergy() As String = {"kJ", "J", "MJ", "BTU", "cal"}
        Dim aEnergyConversion As New Conversions(MEnergy, Zero, LEnergy)
        BaseUnits(BaseUnitsNum.Energy) = aEnergyConversion
        ' Smoke conversions (We treat this as a base unit to limit it to %/ft and %/m)
        Dim MSmoke() As Double = {1, 0.3048}
        Dim LSmoke() As String = {"%/m", "%/ft"}
        Dim aSmokeConversion As New Conversions(MSmoke, Zero, LSmoke)
        BaseUnits(BaseUnitsNum.Smoke) = aSmokeConversion

        InitConversionFactors(DefaultUnits)
    End Sub
    Public Sub InitConversionFactors(ByVal DesiredUnits() As Integer)
        Dim i As Integer
        Dim m As Double, b As Double, Label As String

        Dim mLength As Double, mMass As Double, mTime As Double, mTemperature As Double, mPressure As Double, mEnergy As Double, mSmoke As Double
        Dim lLength As String, lMass As String, lTime As String, lTemperature As String, lPressure As String, lEnergy As String, lSmoke As String
        mLength = BaseUnits(BaseUnitsNum.Length).m(DesiredUnits(BaseUnitsNum.Length))
        mMass = BaseUnits(BaseUnitsNum.Mass).m(DesiredUnits(BaseUnitsNum.Mass))
        mTime = BaseUnits(BaseUnitsNum.Time).m(DesiredUnits(BaseUnitsNum.Time))
        mTemperature = BaseUnits(BaseUnitsNum.Temperature).m(DesiredUnits(BaseUnitsNum.Temperature))
        mPressure = BaseUnits(BaseUnitsNum.Pressure).m(DesiredUnits(BaseUnitsNum.Pressure))
        mEnergy = BaseUnits(BaseUnitsNum.Energy).m(DesiredUnits(BaseUnitsNum.Energy))
        mSmoke = BaseUnits(BaseUnitsNum.Smoke).m(DesiredUnits(BaseUnitsNum.Smoke))

        lLength = BaseUnits(BaseUnitsNum.Length).Units(DesiredUnits(BaseUnitsNum.Length))
        lMass = BaseUnits(BaseUnitsNum.Mass).Units(DesiredUnits(BaseUnitsNum.Mass))
        lTime = BaseUnits(BaseUnitsNum.Time).Units(DesiredUnits(BaseUnitsNum.Time))
        lTemperature = BaseUnits(BaseUnitsNum.Temperature).Units(DesiredUnits(BaseUnitsNum.Temperature))
        lPressure = BaseUnits(BaseUnitsNum.Pressure).Units(DesiredUnits(BaseUnitsNum.Pressure))
        lEnergy = BaseUnits(BaseUnitsNum.Energy).Units(DesiredUnits(BaseUnitsNum.Energy))
        lSmoke = BaseUnits(BaseUnitsNum.Smoke).Units(DesiredUnits(BaseUnitsNum.Smoke))

        ' Time conversions
        m = mTime
        b = 0.0
        Label = lTime
        Dim aTimeConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Time) = aTimeConversion
        ' Temperature conversions
        m = mTemperature
        b = BaseUnits(BaseUnitsNum.Temperature).b(DesiredUnits(BaseUnitsNum.Temperature))
        Label = lTemperature
        Dim aTemperatureConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Temperature) = aTemperatureConversion
        ' Temperature rise conversions
        Dim aTemperatureRiseConversion As New Conversion(m, 0.0, Label)
        Convert(UnitsNum.TemperatureRise) = aTemperatureRiseConversion
        ' Pressure conversion
        m = mPressure
        b = 0.0
        Label = lPressure
        Dim aPressureConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Pressure) = aPressureConversion
        ' Length conversion
        m = mLength
        b = 0.0
        Label = lLength
        Dim aLengthConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Length) = aLengthConversion
        ' % smoke obscuration conversion
        m = mSmoke
        b = 0.0
        Label = lSmoke
        Dim aSmokeConversion As New Conversion(m, b, Label, 1)
        Convert(UnitsNum.Smoke) = aSmokeConversion

        ' Derived unit conversions
        ' Area conversion
        m = mLength * mLength
        b = 0.0
        Label = lLength + "�"
        Dim aAreaConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Area) = aAreaConversion
        ' Velocity conversion
        m = mLength / mTime
        b = 0.0
        Label = lLength + "/" + lTime
        Dim aVelocityConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Velocity) = aVelocityConversion
        m = mLength * mLength * mLength / mTime
        b = 0.0
        Label = lLength + "^3/" + lTime
        Dim aFlowrateCOnversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Flowrate) = aFlowrateCOnversion
        ' RTI conversion
        m = Math.Sqrt(mLength * mTime)
        b = 0
        Label = "(" + lLength + " " + lTime + ")^0.5"
        Dim aRTIConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.RTI) = aRTIConversion
        ' Mass conversion
        m = mMass
        b = 0.0
        Label = lMass
        Dim aMassConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Mass) = aMassConversion
        'Mass loss conversion
        m = mMass / mTime
        b = 0.0
        Label = lMass + "/" + lTime
        Dim aMassLossConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.MassLoss) = aMassLossConversion
        ' Density conversion
        m = mMass / (mLength * mLength * mLength)
        b = 0.0
        Label = lMass + "/" + lLength + "^3"
        Dim aDensityConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Density) = aDensityConversion
        ' Heat release rate conversion
        m = mEnergy / mTime
        b = 0.0
        Label = lEnergy + "/" + lTime
        If Label = "J/s" Then Label = "W"
        If Label = "kJ/s" Then Label = "kW"
        If Label = "MJ/s" Then Label = "MW"
        Dim aHRRConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.HRR) = aHRRConversion
        ' Heat of combustion conversion
        m = mEnergy / mMass
        b = 0.0
        Label = lEnergy + "/" + lMass
        Dim aHoCConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.HoC) = aHoCConversion
        ' Heat of Gasification conversion
        m = mEnergy / mMass
        b = 0.0
        Label = lEnergy + "/" + lMass
        Dim aHoGConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.HoG) = aHoGConversion
        ' Heat flux conversion
        m = mEnergy / (mTime * mLength * mLength)
        b = 0.0
        Label = lEnergy + "/(" + lTime + " " + lLength + "�)"
        If Label = "J/(s m�)" Then Label = "W/m�"
        If Label = "kJ/(s m�)" Then Label = "kW/m�"
        If Label = "MJ/(s m�)" Then Label = "MW/m�"
        Dim aFluxConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.HeatFlux) = aFluxConversion
        ' Thermal conductivity conversion
        m = mEnergy / (mTime * mLength * mTemperature)
        b = 0.0
        Label = lEnergy + "/(" + lTime + " "
        If Label = "J/(s " Then Label = "W/("
        If Label = "kJ/(s " Then Label = "kW/("
        If Label = "MJ/(s " Then Label = "MW/("
        Label = Label + lLength + " " + lTemperature + ")"
        Dim aConductivityConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.Conductivity) = aConductivityConversion
        ' Specific heat conversion
        m = mEnergy / (mMass * mTemperature)
        b = 0.0
        Label = lEnergy + "/(" + lMass + " " + lTemperature + ")"
        Dim aSpecificHeatConversion As New Conversion(m, b, Label)
        Convert(UnitsNum.SpecificHeat) = aSpecificHeatConversion

        ' Conversions for fire time series data
        ' Fire time conversion
        Dim One As New Conversion(1.0, 0.0, " ")
        For i = 0 To 12
            ConvertFireData(i) = One
        Next
        ConvertFireData(UnitsNum.FireTime) = aTimeConversion
        ConvertFireData(UnitsNum.FireMdot) = aMassLossConversion
        ConvertFireData(UnitsNum.FireQdot) = aHRRConversion
        ConvertFireData(UnitsNum.FireHeight) = aLengthConversion
        ConvertFireData(UnitsNum.FireArea) = aAreaConversion
    End Sub
End Class
Public Class Conversion
    Private aM As Double, aB As Double          ' Conversion constants to SI for current units
    Private aLabel As String                    ' Units label for current units
    Private aValue As Double
    Private aType As Integer                    ' 0 for linear conversion, 1 for % obscuration conversion
    Friend Sub New(ByVal m As Double, ByVal b As Double, ByVal Label As String)
        aType = 0
        aM = m
        aB = b
        aLabel = Label
    End Sub
    Friend Sub New(ByVal m As Double, ByVal b As Double, ByVal Label As String, Type As Integer)
        aType = Type
        aM = m
        aB = b
        aLabel = Label
    End Sub
    Friend ReadOnly Property ToSI(ByVal EngineeringUnits As Double) As Double
        ' Conversion to SI units is of the form y=(x+b)*m
        Get
            If myUnits.SI Then
                Return EngineeringUnits
            Else
                If aType = 0 Then
                    Return (Val(EngineeringUnits) + aB) * aM
                Else
                    If aM = 1 Then
                        Return EngineeringUnits
                    Else
                        Return 100 * (1 - (1 - EngineeringUnits / 100) ^ (1 / aM))
                    End If
                End If
            End If
        End Get
    End Property
    Friend ReadOnly Property FromSI(ByVal SIUnits As Double) As Double
        ' Conversion from SI units is inverse of conversion to SI ... x=y/m-b
        Get
            If myUnits.SI Then
                Return SIUnits
            Else
                If aType = 0 Then
                    Return Val(SIUnits) / aM - aB
                Else
                    If aM = 1 Then
                        Return SIUnits
                    Else
                        Return 100 * (1 - (1 - SIUnits / 100) ^ aM)
                    End If
                End If
            End If
        End Get
    End Property
    Friend ReadOnly Property Units() As String
        Get
            Return " " + aLabel
        End Get
    End Property
End Class
Public Class Conversions
    Private aM() As Double, aB() As Double   ' Holds values for conversion constants to SI Units
    Private aLabels() As String              ' Labels cooresponding to the constants
    Private aValue As Double
    Public Sub New(ByVal m() As Double, ByVal b() As Double, ByVal Labels() As String)
        Dim i As Integer
        ReDim aM(m.GetUpperBound(0)), aB(b.GetUpperBound(0)), aLabels(Labels.GetUpperBound(0))
        For i = 0 To m.GetUpperBound(0)
            aM(i) = m(i)
            aB(i) = b(i)
            aLabels(i) = Labels(i)
        Next
    End Sub
    Public ReadOnly Property m(ByVal CurrentUnits As Integer) As Double
        Get
            If CurrentUnits >= 0 And CurrentUnits <= 6 Then
                Return aM(CurrentUnits)
            End If
        End Get
    End Property
    Public ReadOnly Property b(ByVal CurrentUnits As Integer) As Double
        Get
            If CurrentUnits >= 0 And CurrentUnits <= 6 Then
                Return aB(CurrentUnits)
            End If
        End Get
    End Property
    Public ReadOnly Property Units(ByVal CurrentUnits As Integer) As String
        Get
            If CurrentUnits >= 0 And CurrentUnits <= aLabels.GetUpperBound(0) Then
                Return aLabels(CurrentUnits)
            Else
                Return ""
            End If
        End Get
    End Property
    Public Sub GetAllUnits(ByRef Labels() As String)
        Dim i As Integer
        ReDim Labels(aLabels.GetUpperBound(0))
        For i = 0 To aLabels.GetUpperBound(0)
            Labels(i) = aLabels(i)
        Next
    End Sub
End Class
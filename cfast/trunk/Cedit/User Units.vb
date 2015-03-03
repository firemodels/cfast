Public Class User_Units
    Inherits System.Windows.Forms.Form
    Private cArray(5) As ComboBox, CurrentUnits(5) As Integer, EnteringUnits(5) As Integer

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
        Me.Time = New System.Windows.Forms.Label()
        Me.UnitsTime = New System.Windows.Forms.ComboBox()
        Me.UnitsTemperature = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.UnitsPressure = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.UnitsEnergy = New System.Windows.Forms.ComboBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.UnitsMass = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.UnitsOK = New System.Windows.Forms.Button()
        Me.UnitsDefault = New System.Windows.Forms.Button()
        Me.UnitsCancel = New System.Windows.Forms.Button()
        Me.UnitsLength = New System.Windows.Forms.ComboBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'Time
        '
        Me.Time.AutoSize = True
        Me.Time.Location = New System.Drawing.Point(103, 90)
        Me.Time.Name = "Time"
        Me.Time.Size = New System.Drawing.Size(33, 13)
        Me.Time.TabIndex = 0
        Me.Time.Text = "Time:"
        Me.Time.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsTime
        '
        Me.UnitsTime.Location = New System.Drawing.Point(143, 88)
        Me.UnitsTime.Name = "UnitsTime"
        Me.UnitsTime.Size = New System.Drawing.Size(96, 21)
        Me.UnitsTime.TabIndex = 2
        '
        'UnitsTemperature
        '
        Me.UnitsTemperature.Location = New System.Drawing.Point(143, 120)
        Me.UnitsTemperature.Name = "UnitsTemperature"
        Me.UnitsTemperature.Size = New System.Drawing.Size(96, 21)
        Me.UnitsTemperature.TabIndex = 3
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(63, 122)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(70, 13)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Temperature:"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsPressure
        '
        Me.UnitsPressure.Location = New System.Drawing.Point(143, 152)
        Me.UnitsPressure.Name = "UnitsPressure"
        Me.UnitsPressure.Size = New System.Drawing.Size(96, 21)
        Me.UnitsPressure.TabIndex = 4
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(87, 154)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(51, 13)
        Me.Label9.TabIndex = 18
        Me.Label9.Text = "Pressure:"
        Me.Label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsEnergy
        '
        Me.UnitsEnergy.Location = New System.Drawing.Point(143, 184)
        Me.UnitsEnergy.Name = "UnitsEnergy"
        Me.UnitsEnergy.Size = New System.Drawing.Size(96, 21)
        Me.UnitsEnergy.TabIndex = 6
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(8, 184)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(126, 13)
        Me.Label18.TabIndex = 36
        Me.Label18.Text = "Energy, Quantity of Heat:"
        Me.Label18.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsMass
        '
        Me.UnitsMass.Location = New System.Drawing.Point(143, 56)
        Me.UnitsMass.Name = "UnitsMass"
        Me.UnitsMass.Size = New System.Drawing.Size(96, 21)
        Me.UnitsMass.TabIndex = 1
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(103, 58)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(35, 13)
        Me.Label3.TabIndex = 42
        Me.Label3.Text = "Mass:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'UnitsOK
        '
        Me.UnitsOK.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.UnitsOK.Location = New System.Drawing.Point(16, 224)
        Me.UnitsOK.Name = "UnitsOK"
        Me.UnitsOK.Size = New System.Drawing.Size(75, 23)
        Me.UnitsOK.TabIndex = 7
        Me.UnitsOK.Text = "OK"
        '
        'UnitsDefault
        '
        Me.UnitsDefault.Location = New System.Drawing.Point(208, 224)
        Me.UnitsDefault.Name = "UnitsDefault"
        Me.UnitsDefault.Size = New System.Drawing.Size(75, 23)
        Me.UnitsDefault.TabIndex = 9
        Me.UnitsDefault.Text = "Reset"
        '
        'UnitsCancel
        '
        Me.UnitsCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.UnitsCancel.Location = New System.Drawing.Point(112, 224)
        Me.UnitsCancel.Name = "UnitsCancel"
        Me.UnitsCancel.Size = New System.Drawing.Size(75, 23)
        Me.UnitsCancel.TabIndex = 8
        Me.UnitsCancel.Text = "Cancel"
        '
        'UnitsLength
        '
        Me.UnitsLength.Location = New System.Drawing.Point(143, 24)
        Me.UnitsLength.Name = "UnitsLength"
        Me.UnitsLength.Size = New System.Drawing.Size(96, 21)
        Me.UnitsLength.TabIndex = 0
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(95, 26)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(43, 13)
        Me.Label4.TabIndex = 4
        Me.Label4.Text = "Length:"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'User_Units
        '
        Me.AcceptButton = Me.UnitsOK
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.CancelButton = Me.UnitsCancel
        Me.ClientSize = New System.Drawing.Size(302, 268)
        Me.Controls.Add(Me.UnitsCancel)
        Me.Controls.Add(Me.UnitsDefault)
        Me.Controls.Add(Me.UnitsOK)
        Me.Controls.Add(Me.UnitsMass)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label18)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Time)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.UnitsEnergy)
        Me.Controls.Add(Me.UnitsPressure)
        Me.Controls.Add(Me.UnitsTemperature)
        Me.Controls.Add(Me.UnitsTime)
        Me.Controls.Add(Me.UnitsLength)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "User_Units"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "User Units"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Private Sub InitUser_Units()
        Dim i As Integer, j As Integer, Labels() As String = {" "}, aLabel As String
        ' This is called by the New method for the form, usually hidden in the design-generated code

        ' Define an array of the base units
        cArray(0) = Me.UnitsLength
        cArray(1) = Me.UnitsMass
        cArray(2) = Me.UnitsTime
        cArray(3) = Me.UnitsTemperature
        cArray(4) = Me.UnitsPressure
        cArray(5) = Me.UnitsEnergy
        ' Fill the combo boxes with base unit labels
        For i = 0 To 5
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
            If RegistryUnits.GetUpperBound(0) = 5 Then
                For i = 0 To 5
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
        EnteringUnits(i) = CurrentUnits(i)
    End Sub
    Private Sub UnitsDefault_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsDefault.Click
        Dim i As Integer, Zero() As Integer = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        For i = 0 To 5
            cArray(i).SelectedIndex = 0
        Next
        myUnits.InitConversionFactors(Zero)
    End Sub
    Private Sub Units_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsLength.SelectedIndexChanged, UnitsMass.SelectedIndexChanged, UnitsTime.SelectedIndexChanged, UnitsTemperature.SelectedIndexChanged, UnitsPressure.SelectedIndexChanged, UnitsEnergy.SelectedIndexChanged
        Dim i As Integer
        For i = 0 To 5
            CurrentUnits(i) = cArray(i).SelectedIndex
        Next
        myUnits.InitConversionFactors(CurrentUnits)
    End Sub
    Private Sub UnitsCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsCancel.Click
        Dim i As Integer, Zero() As Integer = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        For i = 0 To 5
            cArray(i).SelectedIndex = EnteringUnits(i)
        Next
        myUnits.InitConversionFactors(Zero)
    End Sub
    Private Sub UnitsOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnitsOK.Click
        Dim i As Integer
        Dim UnitNames() As String = {"Length", "Mass", "Time", "Temperature", "Pressure", "Energy"}
        For i = 0 To 5
            SaveSetting("CFAST", "Units", UnitNames(i), CurrentUnits(i))
        Next
    End Sub
End Class
Public Class EngineeringUnits
    Public BaseUnits(5) As Conversions
    Public Convert(17) As Conversion
    Public ConvertFireData(12) As Conversion
    Private aSI As Boolean = False, aSITemp As Boolean, SIStack As New Stack
    Private aValue As Single
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
        Dim Zero() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        Dim DefaultUnits() As Integer = {0, 0, 0, 0, 0, 0, 0}
        ' These conversion factors are all from the given units to SI according to NIST SP 811
        ' All the conversions are of the form y=(x+b)*m, though usually b is 0.0

        ' Length conversions
        Dim mLength() As Single = {1.0, 0.01, 0.001, 0.3048, 0.0254}
        Dim LLength() As String = {"m", "cm", "mm", "ft", "in"}
        Dim aLengthConversion As New Conversions(mLength, Zero, LLength)
        BaseUnits(BaseUnitsNum.Length) = aLengthConversion
        ' Mass conversions
        Dim MMass() As Single = {1.0, 0.001, 0.4535924, 0.02834952}
        Dim LMass() As String = {"kg", "g", "lb", "oz"}
        Dim aMassConversion As New Conversions(MMass, Zero, LMass)
        BaseUnits(BaseUnitsNum.Mass) = aMassConversion
        ' Time conversions
        Dim MTime() As Single = {1.0, 60.0, 3600.0}
        Dim LTime() As String = {"s", "min", "h"}
        Dim aTimeConversion As New Conversions(MTime, Zero, LTime)
        BaseUnits(BaseUnitsNum.Time) = aTimeConversion
        ' Temperature conversions
        Dim MTemperature() As Single = {1.0, 1.0, 0.5555556, 0.5555556}, BTemperature() As Single = {273.15, 0.0, 459.67, 0.0}
        MTemperature(2) = 5.0 / 9.0 ' to correct a round-off error in display of °C, we'll use the full precision for this constant
        MTemperature(3) = 5.0 / 9.0
        Dim LTemperature() As String = {"°C", "K", "°F", "°R"}
        Dim aTemperatureConversion As New Conversions(MTemperature, BTemperature, LTemperature)
        BaseUnits(BaseUnitsNum.Temperature) = aTemperatureConversion
        ' Pressure conversions
        Dim MPressure() As Single = {1.0, 133.3224, 249.0889, 101325.0}
        Dim Lpressure() As String = {"Pa", "mm Hg", "in H2O", "atm"}
        Dim aPressureConversion As New Conversions(MPressure, Zero, Lpressure)
        BaseUnits(BaseUnitsNum.Pressure) = aPressureConversion
        ' Energy conversions
        Dim MEnergy() As Single = {1000.0, 1.0, 1000000.0, 1054.35, 4.184}
        Dim LEnergy() As String = {"kJ", "J", "MJ", "BTU", "cal"}
        Dim aEnergyConversion As New Conversions(MEnergy, Zero, LEnergy)
        BaseUnits(BaseUnitsNum.Energy) = aEnergyConversion
        InitConversionFactors(DefaultUnits)
    End Sub
    Public Sub InitConversionFactors(ByVal DesiredUnits() As Integer)
        Dim i As Integer
        Dim m As Single, b As Single, Label As String

        Dim mLength As Single, mMass As Single, mTime As Single, mTemperature As Single, mPressure As Single, mEnergy As Single
        Dim lLength As String, lMass As String, lTime As String, lTemperature As String, lPressure As String, lEnergy As String
        mLength = BaseUnits(BaseUnitsNum.Length).m(DesiredUnits(BaseUnitsNum.Length))
        mMass = BaseUnits(BaseUnitsNum.Mass).m(DesiredUnits(BaseUnitsNum.Mass))
        mTime = BaseUnits(BaseUnitsNum.Time).m(DesiredUnits(BaseUnitsNum.Time))
        mTemperature = BaseUnits(BaseUnitsNum.Temperature).m(DesiredUnits(BaseUnitsNum.Temperature))
        mPressure = BaseUnits(BaseUnitsNum.Pressure).m(DesiredUnits(BaseUnitsNum.Pressure))
        mEnergy = BaseUnits(BaseUnitsNum.Energy).m(DesiredUnits(BaseUnitsNum.Energy))

        lLength = BaseUnits(BaseUnitsNum.Length).Units(DesiredUnits(BaseUnitsNum.Length))
        lMass = BaseUnits(BaseUnitsNum.Mass).Units(DesiredUnits(BaseUnitsNum.Mass))
        lTime = BaseUnits(BaseUnitsNum.Time).Units(DesiredUnits(BaseUnitsNum.Time))
        lTemperature = BaseUnits(BaseUnitsNum.Temperature).Units(DesiredUnits(BaseUnitsNum.Temperature))
        lPressure = BaseUnits(BaseUnitsNum.Pressure).Units(DesiredUnits(BaseUnitsNum.Pressure))
        lEnergy = BaseUnits(BaseUnitsNum.Energy).Units(DesiredUnits(BaseUnitsNum.Energy))

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
        ' Area conversion
        m = mLength * mLength
        b = 0.0
        Label = lLength + "²"
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
        Label = lEnergy + "/(" + lTime + " " + lLength + "²)"
        If Label = "J/(s m²)" Then Label = "W/m²"
        If Label = "kJ/(s m²)" Then Label = "kW/m²"
        If Label = "MJ/(s m²)" Then Label = "MW/m²"
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
    Private aM As Single, aB As Single          ' Conversion constants to SI for current units
    Private aLabel As String                    ' Units label for current units
    Private aValue As Single
    Friend Sub New(ByVal m As Single, ByVal b As Single, ByVal Label As String)
        aM = m
        aB = b
        aLabel = Label
    End Sub
    Friend ReadOnly Property ToSI(ByVal EngineeringUnits As Single) As Single
        ' Conversion to SI units is of the form y=(x+b)*m
        Get
            If myUnits.SI Then
                Return EngineeringUnits
            Else
                Return (Val(EngineeringUnits) + aB) * aM
            End If
        End Get
    End Property
    Friend ReadOnly Property FromSI(ByVal SIUnits As Single) As Single
        ' Conversion from SI units is inverse of conversion to SI ... x=y/m-b
        Get
            If myUnits.SI Then
                Return SIUnits
            Else
                Return Val(SIUnits) / aM - aB
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
    Private aM() As Single, aB() As Single   ' Holds values for conversion constants to SI Units
    Private aLabels() As String              ' Labels cooresponding to the constants
    Private aValue As Single
    Public Sub New(ByVal m() As Single, ByVal b() As Single, ByVal Labels() As String)
        Dim i As Integer
        ReDim aM(m.GetUpperBound(0)), aB(b.GetUpperBound(0)), aLabels(Labels.GetUpperBound(0))
        For i = 0 To m.GetUpperBound(0)
            aM(i) = m(i)
            aB(i) = b(i)
            aLabels(i) = Labels(i)
        Next
    End Sub
    Public ReadOnly Property m(ByVal CurrentUnits As Integer) As Single
        Get
            If CurrentUnits >= 0 And CurrentUnits <= 6 Then
                Return aM(CurrentUnits)
            End If
        End Get
    End Property
    Public ReadOnly Property b(ByVal CurrentUnits As Integer) As Single
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
        For i = 0 To Me.aLabels.GetUpperBound(0)
            Labels(i) = aLabels(i)
        Next
    End Sub
End Class
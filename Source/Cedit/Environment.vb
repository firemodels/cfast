Public Class Environment

    Public Const MinTemp As Single = 223.15          ' Minimum ambient temperature limit
    Public Const MaxTemp As Single = 373.15          ' Maximum ambient temperature limit
    Public Const DefaultLOI As Single = 0.15         ' Default limiting oxygen index. This needs to be consistent with CFAST
    Public Const DefaultNonValue As Single = -1001.0 ' Default non value

    ' All units within the class are assumed to be consistent and typically SI
    Private aTitle As String                        ' Title for the simulation
    Private aVersion As Integer                     ' CFAST version number
    Private aSimulationTime As Integer              ' Total simulation time
    Private aOutputInterval As Integer              ' Time interval for printed output, + is compact output and - is full output
    Private aSpreadsheetInterval As Integer         ' Time interval for comma-separated output
    Private aSmokeviewInterval As Integer           ' Time interval for smokeview output
    Private aIntAmbTemperature As Single            ' Ambient temperature inside the structure at t=0
    Private aIntAmbPressure As Single               ' Ambient pressure inside the structure at t=0
    Private aIntAmbElevation As Single              ' Reference elevation for measurement of ambients inside the structure
    Private aIntAmbRH As Single                     ' Ambient relative humidity inside the structure at t=0
    Private aExtAmbTemperature As Single            ' Ambient temperature outside the structure at t=0
    Private aExtAmbPressure As Single               ' Ambient pressure outside the structure at t=0
    Private aExtAmbElevation As Single              ' Reference elevation for measurement of ambients outside the structure
    Private aExtWindSpeed As Single                 ' Ambient wind speed outside the structure
    Private aExtScaleHeight As Single               ' Height at which wind speed is measured
    Private aExtPowerLawCoefficient As Single       ' Power law coefficient for calculation of wind speed at height, normally 0.16
    Private aLowerOxygenLimit As Single             ' Oxygen concentration below which burning will not take place.  Default is 15 % by volume
    Private aIgnitionTemp As Single                 ' Gaseous ignition temperature of the fuel, default is ambient + 100 °C
    Private aMaximumTimeStep As Single              ' Maximum time step for model calculations
    Private aInputFileName As String                ' Current input data file name
    Private aInputFilePath As String                ' Path to current input data file
    Private aAdiabaticWalls As Boolean               ' True if all walls are adiabatic
    Private HasErrors As Integer = 0                ' Temporary variable to indicate whether there are errors in the specification
    Private aChanged As Boolean = False             ' True if any values have changed
    Private aF(0) As Single
    Private aT(0) As Single
    Private aGasTemp As Single
    Private aPartPressH2O As Single
    Private aPartPressCO As Single
    Private aRadSolver As String

    Public Sub New()
        aTitle = "CFAST Simulation"
        aVersion = 7300

        aSimulationTime = 3600
        aOutputInterval = 60
        aSpreadsheetInterval = 15
        aSmokeviewInterval = 15
        aIntAmbTemperature = 293.15
        aIntAmbPressure = 101325.0
        aIntAmbElevation = 0.0
        aIntAmbRH = 50.0
        aExtAmbTemperature = 293.15
        aExtAmbPressure = 101325.0
        aExtAmbElevation = 0.0
        aExtWindSpeed = 0.0
        aExtScaleHeight = 10.0
        aExtPowerLawCoefficient = 0.16
        aLowerOxygenLimit = 0.15
        aIgnitionTemp = aIntAmbTemperature + 100.0
        aMaximumTimeStep = -1.0
        aAdiabaticWalls = False
        aF(0) = DefaultNonValue
        aT(0) = DefaultNonValue
        aGasTemp = DefaultNonValue
        aPartPressCO = DefaultNonValue
        aPartPressH2O = DefaultNonValue
        aRadSolver = ""
    End Sub
    Friend Property Title() As String
        Get
            Return aTitle
        End Get
        Set(ByVal Value As String)
            If Value <> aTitle Then
                aChanged = True
                aTitle = Value
            End If
        End Set
    End Property
    Friend Property Version() As Integer
        Get
            Return aVersion
        End Get
        Set(value As Integer)
            If value <> aVersion Then
                aChanged = True
                aVersion = value
            End If
        End Set
    End Property
    Friend Property SimulationTime() As Integer
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSimulationTime)
        End Get
        Set(ByVal Value As Integer)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSimulationTime Then
                aChanged = True
                aSimulationTime = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property OutputInterval() As Integer
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aOutputInterval)
        End Get
        Set(ByVal Value As Integer)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aOutputInterval Then
                aChanged = True
                aOutputInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property SpreadsheetInterval() As Integer
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSpreadsheetInterval)
        End Get
        Set(ByVal Value As Integer)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSpreadsheetInterval Then
                aChanged = True
                aSpreadsheetInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property SmokeviewInterval() As Integer
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSmokeviewInterval)
        End Get
        Set(ByVal Value As Integer)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSmokeviewInterval Then
                aChanged = True
                aSmokeviewInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property IntAmbTemperature() As Single
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aIntAmbTemperature)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aIntAmbTemperature Then
                aChanged = True
                aIntAmbTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
            End If
        End Set
    End Property
    Friend ReadOnly Property IntAmbPressure() As Single
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aExtAmbPressure * aIntAmbTemperature / aExtAmbTemperature)
        End Get
    End Property
    Friend Property IntAmbRH() As Single
        Get
            Return aIntAmbRH
        End Get
        Set(ByVal Value As Single)
            If Value <> IntAmbRH Then
                aChanged = True
                aIntAmbRH = Value
            End If
        End Set
    End Property
    Friend Property ExtAmbTemperature() As Single
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aExtAmbTemperature)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aExtAmbTemperature Then
                aChanged = True
                aExtAmbTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtAmbPressure() As Single
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aExtAmbPressure)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aExtAmbPressure Then
                aChanged = True
                aExtAmbPressure = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtWindSpeed() As Single
        Get
            Return myUnits.Convert(UnitsNum.Velocity).FromSI(aExtWindSpeed)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Velocity).ToSI(Value) <> aExtWindSpeed Then
                aChanged = True
                aExtWindSpeed = myUnits.Convert(UnitsNum.Velocity).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtScaleHeight() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aExtScaleHeight)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aExtScaleHeight Then
                aChanged = True
                aExtScaleHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtPowerLawCoefficient() As Single
        Get
            Return aExtPowerLawCoefficient
        End Get
        Set(ByVal Value As Single)
            If Value <> aExtPowerLawCoefficient Then
                aChanged = True
                aExtPowerLawCoefficient = Value
            End If
        End Set
    End Property
    Friend Property LowerOxygenLimit() As Single
        Get
            Return aLowerOxygenLimit
        End Get
        Set(ByVal Value As Single)
            If Value <> aLowerOxygenLimit Then
                aChanged = True
                aLowerOxygenLimit = Value
            End If
        End Set
    End Property
    Property IgnitionTemp() As Single
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aIgnitionTemp)
        End Get
        Set(ByVal Value As Single)
            If aIgnitionTemp <> myUnits.Convert(UnitsNum.Temperature).ToSI(Value) Then
                aIgnitionTemp = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property MaximumTimeStep() As Single
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aMaximumTimeStep)
        End Get
        Set(ByVal Value As Single)
            If Value <> 0 Then
                If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aMaximumTimeStep Then
                    aMaximumTimeStep = myUnits.Convert(UnitsNum.Time).ToSI(Value)
                    aChanged = True
                End If
            End If
        End Set
    End Property
    Friend Property AdiabaticWalls() As Boolean
        Get
            Return aAdiabaticWalls
        End Get
        Set(ByVal Value As Boolean)
            If aAdiabaticWalls <> Value Then
                aAdiabaticWalls = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property InputFileName() As String
        Get
            Return aInputFileName
        End Get
        Set(ByVal Value As String)
            aInputFileName = System.IO.Path.GetFileNameWithoutExtension(Value)
        End Set
    End Property
    Friend Property InputFilePath() As String
        Get
            Return AInputFilePath
        End Get
        Set(ByVal Value As String)
            AInputFilePath = System.IO.Path.GetDirectoryName(Value)
        End Set
    End Property
    Friend ReadOnly Property FileChanged() As Boolean
        Get
            Return myEnvironment.Changed Or myCompartments.Changed Or myHVents.Changed Or myVVents.Changed Or
                    myMVents.Changed Or myHHeats.Changed Or myVHeats.Changed Or myTargets.Changed Or myDetectors.Changed Or
                    myFires.Changed Or myFireInstances.Changed Or myThermalProperties.Changed Or myVisuals.Changed
        End Get
    End Property
    Friend ReadOnly Property IsValid() As Integer
        ' Checks the overall validity of the current environment specification
        Get
            HasErrors = 0
            ' Ambient temperatures
            If aIntAmbTemperature <= MinTemp Or aIntAmbTemperature > MaxTemp Then
                myErrors.Add("Internal ambient temperature is should be greater than " + (MinTemp - 273.15).ToString + " °C and less than " + (MaxTemp - 273.15).ToString + " °C", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            If aExtAmbTemperature <= MinTemp Or aExtAmbTemperature > MaxTemp Then
                myErrors.Add("External ambient temperature is should be greater than " + (MinTemp - 273.15).ToString + " °C and less than " + (MaxTemp - 273.15).ToString + " °C", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
            If myEnvironment.LowerOxygenLimit < 0.0 Or myEnvironment.LowerOxygenLimit > 0.23 Then
                myErrors.Add("Lower oxygen limit is less than 0 or greater than ambient.", ErrorMessages.TypeWarning)
                HasErrors += 1
            End If
        End Get
    End Property
    Friend Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Friend Property F(ByVal idx As Integer) As Single
        Get
            If idx >= 0 And idx <= aF.GetUpperBound(0) Then
                Return aF(idx)
            Else
                Return DefaultNonValue
            End If
        End Get
        Set(value As Single)
            Dim i As Integer = aF.GetUpperBound(0)
            If idx >= 0 And idx <= i Then
                aF(idx) = value
            ElseIf idx > i Then
                Dim j As Integer
                ReDim Preserve aF(idx)
                For j = i + 1 To idx
                    aF(j) = DefaultNonValue
                Next
                aF(idx) = value
            End If
        End Set
    End Property
    Friend Property T(ByVal idx As Integer) As Single
        Get
            If idx >= 0 And idx <= aT.GetUpperBound(0) Then
                Return aT(idx)
            Else
                Return DefaultNonValue
            End If
        End Get
        Set(value As Single)
            Dim i As Integer = aT.GetUpperBound(0)
            If idx >= 0 And idx <= i Then
                aT(idx) = value
            ElseIf idx > i Then
                Dim j As Integer
                ReDim Preserve aT(idx)
                For j = i + 1 To idx
                    aT(j) = DefaultNonValue
                Next
                aT(idx) = value
            End If
        End Set
    End Property
    Friend Property GasTemp As Single
        Get
            Return aGasTemp
        End Get
        Set(value As Single)
            aGasTemp = value
        End Set
    End Property
    Friend Property PartPressH2O As Single
        Get
            Return aPartPressH2O
        End Get
        Set(value As Single)
            aPartPressH2O = value
        End Set
    End Property
    Friend Property PartPressCO As Single
        Get
            Return aPartPressCO
        End Get
        Set(value As Single)
            aPartPressCO = value
        End Set
    End Property
    Friend Property RadSolver As String
        Get
            Return aRadSolver
        End Get
        Set(value As String)
            aRadSolver = value
        End Set
    End Property
    Friend Sub SetDiagF(ByVal F() As Single)
        Dim i As Integer
        If F.GetUpperBound(0) <> aF.GetUpperBound(0) Then
            ReDim aF(F.GetUpperBound(0))
        End If
        For i = 0 To aF.GetUpperBound(0)
            aF(i) = F(i)
        Next
    End Sub
    Friend Sub SetDiagT(ByVal T() As Single)
        Dim i As Integer
        If T.GetUpperBound(0) <> aT.GetUpperBound(0) Then
            ReDim aT(T.GetUpperBound(0))
        End If
        For i = 0 To aT.GetUpperBound(0)
            aT(i) = T(i)
        Next
    End Sub
    Friend Sub GetDiagF(ByRef F() As Single)
        Dim i As Integer
        If F.GetUpperBound(0) <> aF.GetUpperBound(0) Then
            ReDim F(aF.GetUpperBound(0))
        End If
        For i = 0 To F.GetUpperBound(0)
            F(i) = aF(i)
        Next
    End Sub
    Friend Sub GetDiagT(ByRef T() As Single)
        Dim i As Integer
        If T.GetUpperBound(0) <> aT.GetUpperBound(0) Then
            ReDim T(aT.GetUpperBound(0))
        End If
        For i = 0 To T.GetUpperBound(0)
            T(i) = aT(i)
        Next
    End Sub
End Class
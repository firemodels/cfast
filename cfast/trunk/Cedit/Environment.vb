Public Class Environment

    Public Const MinTemp As Single = 223.15                   ' Minimum ambient temperature limit
    Public Const MaxTemp As Single = 373.15                   ' Maximum ambient temperature limit

    ' All units within the class are assumed to be consistent and typically SI
    Private aTitle As String                        ' Title for the simulation
    Private aSimulationTime As Integer              ' Total simulation time
    Private aOutputInterval As Integer              ' Time interval for printed output, + is compact output and - is full output
    Private aBinaryInterval As Integer              ' Time interval for binary output
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
    Private aCeilingJet As Integer                  ' Specifies calculation of ceiling jet heat transfer. 0 for off, 1 for ceiling, 2 for ceiling and walls, 3 for all surfaces
    Private aLowerOxygenLimit As Single             ' Oxygen concentration below which burning will not take place.  Default is 10 % by volume
    Private aIgnitionTemp As Single                 ' Gaseous ignition temperature of the fuel, default is ambient + 100 °C
    Private aMaximumTimeStep As Single              ' Maximum time step for model calculations
    Private aInputFileName As String                ' Current input data file name
    Private AInputFilePath As String                ' Path to current input data file
    Private HasErrors As Integer = 0                ' Temporary variable to indicate whether there are errors in the specification
    Private aChanged As Boolean = False             ' True if any values have changed

    Public Sub New()
        aTitle = "CFAST Simulation"
        aSimulationTime = 900
        aOutputInterval = 50
        aBinaryInterval = 0
        aSpreadsheetInterval = 10
        aSmokeviewInterval = 10
        aIntAmbTemperature = 293.15
        aIntAmbPressure = 101300.0
        aIntAmbElevation = 0.0
        aIntAmbRH = 50.0
        aExtAmbTemperature = 293.15
        aExtAmbPressure = 101300.0
        aExtAmbElevation = 0.0
        aExtWindSpeed = 0.0
        aExtScaleHeight = 10.0
        aExtPowerLawCoefficient = 0.16
        aCeilingJet = 2
        aLowerOxygenLimit = 10.0
        aIgnitionTemp = aIntAmbTemperature + 100.0
        aMaximumTimeStep = -1.0
    End Sub
    Friend Property Title() As String
        Get
            Return aTitle
        End Get
        Set(ByVal Value As String)
            aChanged = True
            aTitle = Value
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
    Friend Property BinaryOutputInterval() As Integer
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aBinaryInterval)
        End Get
        Set(ByVal Value As Integer)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aBinaryInterval Then
                aChanged = True
                aBinaryInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
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
    Friend Property IntAmbPressure() As Single
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aIntAmbPressure)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aIntAmbPressure Then
                aChanged = True
                aIntAmbPressure = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property IntAmbElevation() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aIntAmbElevation)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aIntAmbElevation Then
                aChanged = True
                aIntAmbElevation = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
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
    Friend Property ExtAmbElevation() As Single
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aExtAmbElevation)
        End Get
        Set(ByVal Value As Single)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aExtAmbElevation Then
                aChanged = True
                aExtAmbElevation = myUnits.Convert(UnitsNum.Length).ToSI(Value)
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
    Friend Property CeilingJet() As Integer
        Get
            Return aCeilingJet
        End Get
        Set(ByVal Value As Integer)
            If Value <> aCeilingJet And Value >= 0 And Value <= 2 Then
                aChanged = True
                aCeilingJet = Value
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
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aMaximumTimeStep Then
                aChanged = True
                aMaximumTimeStep = myUnits.Convert(UnitsNum.Time).ToSI(Value)
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
            Return myEnvironment.Changed Or myCompartments.Changed Or myHVents.Changed Or myVVents.Changed Or _
                    myMVents.Changed Or myHHeats.Changed Or myVHeats.Changed Or myTargets.Changed Or myDetectors.Changed Or _
                    myFires.Changed Or myFireObjects.Changed Or myThermalProperties.Changed
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
End Class
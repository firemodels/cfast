Public Class Environment

    Public Const MinTemp As Double = 223.15           ' Minimum ambient temperature limit
    Public Const MaxTemp As Double = 373.15           ' Maximum ambient temperature limit
    Public Const DefaultMaximumTimeStep As Double = 2 ' Default maximum solver time step
    Public Const DefaultLOI As Double = 0.15          ' Default limiting oxygen index. This needs to be consistent with CFAST
    Public Const DefaultNonValue As Double = -1001.0  ' Default non value
    Public Const DIAGon As Integer = 1                ' For DIAG namelist, is the integer value for something being on
    Public Const DIAGoff As Integer = 0               ' For DIAG namelist, is the integer value for something being off

    ' All units within the class are assumed to be consistent and typically SI
    Private aTitle As String                        ' Title for the simulation
    Private aVersion As Integer                     ' CFAST version number
    Private aSimulationTime As Double               ' Total simulation time
    Private aOutputInterval As Double               ' Time interval for printed output, + is compact output and - is full output
    Private aSpreadsheetInterval As Double          ' Time interval for comma-separated output
    Private aSmokeviewInterval As Double            ' Time interval for smokeview output
    Private aIntAmbTemperature As Double            ' Ambient temperature inside the structure at t=0
    Private aIntAmbPressure As Double               ' Ambient pressure inside the structure at t=0
    Private aIntAmbElevation As Double              ' Reference elevation for measurement of ambients inside the structure
    Private aIntAmbRH As Double                     ' Ambient relative humidity inside the structure at t=0
    Private aExtAmbTemperature As Double            ' Ambient temperature outside the structure at t=0
    Private aExtAmbPressure As Double               ' Ambient pressure outside the structure at t=0
    Private aExtAmbElevation As Double              ' Reference elevation for measurement of ambients outside the structure
    Private aExtWindSpeed As Double                 ' Ambient wind speed outside the structure
    Private aExtScaleHeight As Double               ' Height at which wind speed is measured
    Private aExtPowerLawCoefficient As Double       ' Power law coefficient for Dump of wind speed at height, normally 0.16
    Private aLowerOxygenLimit As Double             ' Oxygen concentration below which burning will not take place.  Default is 15 % by volume
    Private aIgnitionTemp As Double                 ' Gaseous ignition temperature of the fuel, default is ambient + 100 °C
    Private aMaximumTimeStep As Double              ' Maximum time step for model Dumps
    Private aInputFileName As String                ' Current input data file name
    Private aInputFilePath As String                ' Path to current input data file
    Private aAdiabaticWalls As Boolean              ' True if all walls are adiabatic
    Private aExtinctionFlaming As Double = 8700     ' Soot concentration to OD for flaming soot
    Private aExtinctionSmoldering As Double = 4400  ' Soot concentration to OD for flaming soot
    Private aOverwrite As Boolean = True
    Private HasErrors As Integer = 0                ' Temporary variable to indicate whether there are errors in the specification
    Private aChanged As Boolean = False             ' True if any values have changed
    ' all variables below are for the &DIAG namelist. 
    Private aDIAGf(0) As Double
    Private aDIAGt(0) As Double
    Private aDIAGGasTemp As Double
    Private aDIAGPartPressH2O As Double
    Private aDIAGPartPressCO2 As Double
    Private aDIAGUpperLayerThickness As Double
    Private aDIAGFireHeatFlux As Double
    Private aDIAGVerificationTimeStep As Double
    Private aDIAGRadSolver As String
    Private aDiagAdiabaticTargetVerification As Boolean
    Private aDiagAdiabaticTargetFlux As Double
    Private aDIAGfire As Integer
    Private aDIAGhflow As Integer
    Private aDIAGentrain As Integer
    Private aDIAGvflow As Integer
    Private aDIAGcjet As Integer
    Private aDIAGdfire As Integer
    Private aDIAGconvec As Integer
    Private aDIAGrad As Integer
    Private aDIAGgasabsorp As Integer
    Private aDIAGconduc As Integer
    Private aDIAGdebugprn As Integer
    Private aDIAGmflow As Integer
    Private aDIAGkeyin As Integer
    Private aDIAGsteadyint As Integer
    Private aDIAGdasslprn As Integer
    Private aDIAGoxygen As Integer
    Private aDIAGresiddbprn As Integer
    Private aDIAGlayermixing As Integer

    Public Sub New()
        aTitle = "CFAST Simulation"
        aVersion = Convert.ToInt32(My.Application.Info.Version.Major.ToString) * 1000 + Convert.ToInt32(My.Application.Info.Version.Minor.ToString) * 100
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
        aMaximumTimeStep = DefaultMaximumTimeStep
        aAdiabaticWalls = False
        aDIAGf(0) = DefaultNonValue
        aDIAGt(0) = DefaultNonValue
        aDIAGGasTemp = DefaultNonValue
        aDIAGPartPressCO2 = DefaultNonValue
        aDIAGPartPressH2O = DefaultNonValue
        aDIAGUpperLayerThickness = DefaultNonValue
        aDIAGFireHeatFlux = DefaultNonValue
        aDIAGVerificationTimeStep = DefaultNonValue
        aDIAGRadSolver = "DEFAULT"
        aDiagAdiabaticTargetVerification = False
        aDiagAdiabaticTargetFlux = 0
        aOverwrite = True
        aDIAGfire = DIAGon
        aDIAGhflow = DIAGon
        aDIAGentrain = DIAGon
        aDIAGvflow = DIAGon
        aDIAGcjet = DIAGon
        aDIAGdfire = DIAGon
        aDIAGconvec = DIAGon
        aDIAGrad = DIAGon
        aDIAGgasabsorp = DIAGon
        aDIAGconduc = DIAGon
        aDIAGdebugprn = DIAGoff
        aDIAGmflow = DIAGon
        aDIAGkeyin = DIAGon
        aDIAGsteadyint = DIAGoff
        aDIAGdasslprn = DIAGoff
        aDIAGoxygen = DIAGoff
        aDIAGresiddbprn = DIAGoff
        aDIAGlayermixing = DIAGon

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
        Set(Value As Integer)
            If Value <> Convert.ToInt32(My.Application.Info.Version.Major.ToString) * 1000 + Convert.ToInt32(My.Application.Info.Version.Minor.ToString) * 100 Then
                aChanged = True
                aVersion = Convert.ToInt32(My.Application.Info.Version.Major.ToString) * 1000 + Convert.ToInt32(My.Application.Info.Version.Minor.ToString) * 100
            End If
        End Set
    End Property
    Friend Property SimulationTime() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSimulationTime)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSimulationTime Then
                aChanged = True
                aSimulationTime = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property OutputInterval() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aOutputInterval)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aOutputInterval Then
                aChanged = True
                aOutputInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property SpreadsheetInterval() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSpreadsheetInterval)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSpreadsheetInterval Then
                aChanged = True
                aSpreadsheetInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property SmokeviewInterval() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aSmokeviewInterval)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Time).ToSI(Value) <> aSmokeviewInterval Then
                aChanged = True
                aSmokeviewInterval = myUnits.Convert(UnitsNum.Time).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property IntAmbTemperature() As Double
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aIntAmbTemperature)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aIntAmbTemperature Then
                aChanged = True
                aIntAmbTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
            End If
        End Set
    End Property
    Friend ReadOnly Property IntAmbPressure() As Double
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aExtAmbPressure * aIntAmbTemperature / aExtAmbTemperature)
        End Get
    End Property
    Friend Property IntAmbRH() As Double
        Get
            Return aIntAmbRH
        End Get
        Set(ByVal Value As Double)
            If Value <> IntAmbRH Then
                aChanged = True
                aIntAmbRH = Value
            End If
        End Set
    End Property
    Friend Property ExtAmbTemperature() As Double
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aExtAmbTemperature)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aExtAmbTemperature Then
                aChanged = True
                aExtAmbTemperature = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtAmbPressure() As Double
        Get
            Return myUnits.Convert(UnitsNum.Pressure).FromSI(aExtAmbPressure)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Pressure).ToSI(Value) <> aExtAmbPressure Then
                aChanged = True
                aExtAmbPressure = myUnits.Convert(UnitsNum.Pressure).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtWindSpeed() As Double
        Get
            Return myUnits.Convert(UnitsNum.Velocity).FromSI(aExtWindSpeed)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Velocity).ToSI(Value) <> aExtWindSpeed Then
                aChanged = True
                aExtWindSpeed = myUnits.Convert(UnitsNum.Velocity).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtScaleHeight() As Double
        Get
            Return myUnits.Convert(UnitsNum.Length).FromSI(aExtScaleHeight)
        End Get
        Set(ByVal Value As Double)
            If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aExtScaleHeight Then
                aChanged = True
                aExtScaleHeight = myUnits.Convert(UnitsNum.Length).ToSI(Value)
            End If
        End Set
    End Property
    Friend Property ExtPowerLawCoefficient() As Double
        Get
            Return aExtPowerLawCoefficient
        End Get
        Set(ByVal Value As Double)
            If Value <> aExtPowerLawCoefficient Then
                aChanged = True
                aExtPowerLawCoefficient = Value
            End If
        End Set
    End Property
    Friend Property LowerOxygenLimit() As Double
        Get
            Return aLowerOxygenLimit
        End Get
        Set(ByVal Value As Double)
            If Value <> aLowerOxygenLimit Then
                aChanged = True
                aLowerOxygenLimit = Value
            End If
        End Set
    End Property
    Property IgnitionTemp() As Double
        Get
            Return myUnits.Convert(UnitsNum.Temperature).FromSI(aIgnitionTemp)
        End Get
        Set(ByVal Value As Double)
            If aIgnitionTemp <> myUnits.Convert(UnitsNum.Temperature).ToSI(Value) Then
                aIgnitionTemp = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                aChanged = True
            End If
        End Set
    End Property
    Friend Property MaximumTimeStep() As Double
        Get
            Return myUnits.Convert(UnitsNum.Time).FromSI(aMaximumTimeStep)
        End Get
        Set(ByVal Value As Double)
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
    Friend Property FlamingExtinctionCoefficient() As Double
        Get
            Return aExtinctionFlaming
        End Get
        Set(Value As Double)
            If aExtinctionFlaming <> Value Then
                aExtinctionFlaming = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property SmolderingExtinctionCoefficient() As Double
        Get
            Return aExtinctionSmoldering
        End Get
        Set(Value As Double)
            If aExtinctionSmoldering <> Value Then
                aExtinctionSmoldering = Value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property Overwrite As Boolean
        Get
            Return aOverwrite
        End Get
        Set(value As Boolean)
            If Overwrite <> value Then
                aOverwrite = value
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
                    myFireProperties.Changed Or myFires.Changed Or myThermalProperties.Changed Or myVisuals.Changed
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
    Friend Property DIAGf(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx <= aDIAGf.GetUpperBound(0) Then
                Return aDIAGf(idx)
            Else
                Return DefaultNonValue
            End If
        End Get
        Set(value As Double)
            Dim i As Integer = aDIAGf.GetUpperBound(0)
            If idx >= 0 And idx <= i Then
                If aDIAGf(idx) <> value Then
                    aChanged = True
                    aDIAGf(idx) = value
                End If
            ElseIf idx > i Then
                aChanged = True
                Dim j As Integer
                ReDim Preserve aDIAGf(idx)
                For j = i + 1 To idx
                    aDIAGf(j) = DefaultNonValue
                Next
                aDIAGf(idx) = value
            End If
        End Set
    End Property
    Friend Property DIAGt(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx <= aDIAGt.GetUpperBound(0) Then
                Return aDIAGt(idx)
            Else
                Return DefaultNonValue
            End If
        End Get
        Set(value As Double)
            Dim i As Integer = aDIAGt.GetUpperBound(0)
            If idx >= 0 And idx <= i Then
                If aDIAGt(idx) <> value Then
                    aChanged = True
                    aDIAGt(idx) = value
                End If
            ElseIf idx > i Then
                aChanged = True
                Dim j As Integer
                ReDim Preserve aDIAGt(idx)
                For j = i + 1 To idx
                    aDIAGt(j) = DefaultNonValue
                Next
                aDIAGt(idx) = value
            End If
        End Set
    End Property
    Friend Property DIAGGasTemp As Double
        Get
            Return aDIAGGasTemp
        End Get
        Set(value As Double)
            If aDIAGGasTemp <> value Then
                aDIAGGasTemp = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGPartPressH2O As Double
        Get
            Return aDIAGPartPressH2O
        End Get
        Set(value As Double)
            aDIAGPartPressH2O = value
        End Set
    End Property
    Friend Property DIAGPartPressCO2 As Double
        Get
            Return aDIAGPartPressCO2
        End Get
        Set(value As Double)
            If aDIAGPartPressCO2 <> value Then
                aDIAGPartPressCO2 = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGUpperLayerThickness As Double
        Get
            Return aDIAGUpperLayerThickness
        End Get
        Set(value As Double)
            If aDIAGUpperLayerThickness <> value Then
                aDIAGUpperLayerThickness = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGFireHeatFlux As Double
        Get
            Return aDIAGFireHeatFlux
        End Get
        Set(value As Double)
            If aDIAGFireHeatFlux <> value Then
                aDIAGFireHeatFlux = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGVerificationTimeStep As Double
        Get
            Return aDIAGVerificationTimeStep
        End Get
        Set(value As Double)
            If aDIAGVerificationTimeStep <> value Then
                aDIAGVerificationTimeStep = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGRadSolver As String
        Get
            Return aDIAGRadSolver
        End Get
        Set(value As String)
            If aDIAGRadSolver <> value Then
                aDIAGRadSolver = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Sub SetDIAGf(ByVal F() As Double)
        Dim i As Integer
        If F.GetUpperBound(0) <> aDIAGf.GetUpperBound(0) Then
            ReDim aDIAGf(F.GetUpperBound(0))
        End If
        For i = 0 To aDIAGf.GetUpperBound(0)
            aDIAGf(i) = F(i)
        Next
        aChanged = True
    End Sub
    Friend Sub SetDIAGt(ByVal T() As Double)
        Dim i As Integer
        If T.GetUpperBound(0) <> aDIAGt.GetUpperBound(0) Then
            ReDim aDIAGt(T.GetUpperBound(0))
        End If
        For i = 0 To aDIAGt.GetUpperBound(0)
            aDIAGt(i) = T(i)
        Next
        aChanged = True
    End Sub
    Friend Sub GetDIAGf(ByRef F() As Double)
        Dim i As Integer
        If F.GetUpperBound(0) <> aDIAGf.GetUpperBound(0) Then
            ReDim F(aDIAGf.GetUpperBound(0))
        End If
        For i = 0 To F.GetUpperBound(0)
            F(i) = aDIAGf(i)
        Next
    End Sub
    Friend Sub GetDIAGt(ByRef T() As Double)
        Dim i As Integer
        If T.GetUpperBound(0) <> aDIAGt.GetUpperBound(0) Then
            ReDim T(aDIAGt.GetUpperBound(0))
        End If
        For i = 0 To T.GetUpperBound(0)
            T(i) = aDIAGt(i)
        Next
    End Sub
    Friend Property DIAGAdiabaticTargetVerification As Boolean
        Get
            Return aDiagAdiabaticTargetVerification
        End Get
        Set(value As Boolean)
            If value <> aDiagAdiabaticTargetVerification Then
                aDiagAdiabaticTargetVerification = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGAdiabaticTargetFlux As Double
        Get
            Return aDiagAdiabaticTargetFlux
        End Get
        Set(value As Double)
            If value <> aDiagAdiabaticTargetFlux Then
                aDiagAdiabaticTargetFlux = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGfire As Integer
        Get
            Return aDIAGfire
        End Get
        Set(value As Integer)
            If aDIAGfire <> value Then
                aDIAGfire = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGhflow As Integer
        Get
            Return aDIAGhflow
        End Get
        Set(value As Integer)
            If aDIAGhflow <> value Then
                aDIAGhflow = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGentrain As Integer
        Get
            Return aDIAGentrain
        End Get
        Set(value As Integer)
            If aDIAGentrain <> value Then
                aDIAGentrain = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGvflow As Integer
        Get
            Return aDIAGvflow
        End Get
        Set(value As Integer)
            If aDIAGvflow <> value Then
                aDIAGvflow = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGcjet As Integer
        Get
            Return aDIAGcjet
        End Get
        Set(value As Integer)
            If aDIAGcjet <> value Then
                aDIAGcjet = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGdfire As Integer
        Get
            Return aDIAGdfire
        End Get
        Set(value As Integer)
            If aDIAGdfire <> value Then
                aDIAGdfire = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGconvec As Integer
        Get
            Return aDIAGconvec
        End Get
        Set(value As Integer)
            If aDIAGconvec <> value Then
                aDIAGconvec = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGrad As Integer
        Get
            Return aDIAGrad
        End Get
        Set(value As Integer)
            If aDIAGrad <> value Then
                aDIAGrad = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGgasabsorp As Integer
        Get
            Return aDIAGgasabsorp
        End Get
        Set(value As Integer)
            If aDIAGgasabsorp <> value Then
                aDIAGgasabsorp = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGconduc As Integer
        Get
            Return aDIAGconduc
        End Get
        Set(value As Integer)
            If aDIAGconduc <> value Then
                aDIAGconduc = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGdebugprn As Integer
        Get
            Return aDIAGdebugprn
        End Get
        Set(value As Integer)
            If aDIAGdebugprn <> value Then
                aDIAGdebugprn = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGmflow As Integer
        Get
            Return aDIAGmflow
        End Get
        Set(value As Integer)
            If aDIAGmflow <> value Then
                aDIAGmflow = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGkeyin As Integer
        Get
            Return aDIAGkeyin
        End Get
        Set(value As Integer)
            If aDIAGkeyin <> value Then
                aDIAGkeyin = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGsteadyint As Integer
        Get
            Return aDIAGsteadyint
        End Get
        Set(value As Integer)
            If aDIAGsteadyint <> value Then
                aDIAGsteadyint = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGdasslprn As Integer
        Get
            Return aDIAGdasslprn
        End Get
        Set(value As Integer)
            If aDIAGdasslprn <> value Then
                aDIAGdasslprn = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGoxygen As Integer
        Get
            Return aDIAGoxygen
        End Get
        Set(value As Integer)
            If aDIAGoxygen <> value Then
                aDIAGoxygen = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGresiddbprn As Integer
        Get
            Return aDIAGresiddbprn
        End Get
        Set(value As Integer)
            If aDIAGresiddbprn <> value Then
                aDIAGresiddbprn = value
                aChanged = True
            End If
        End Set
    End Property
    Friend Property DIAGlayermixing As Integer
        Get
            Return aDIAGlayermixing
        End Get
        Set(value As Integer)
            If aDIAGlayermixing <> value Then
                aDIAGlayermixing = value
                aChanged = True
            End If
        End Set
    End Property
End Class
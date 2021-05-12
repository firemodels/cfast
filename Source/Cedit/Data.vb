Friend Module Data

    Friend CFastInputFile As String
    Friend CFastInputFilewithExtension As String
    Friend CFASTSimulationTime As Double
    Friend ExitCode As Integer = 0

    Friend myRecentFiles As RecentFiles
    Friend myEnvironment As New Environment
    Friend Const ErrorNames As String = "WarningError  Fatal  Log           CFAST  "
    Friend myErrors As New ErrorMessages
    Friend Update As Boolean = False

    Friend myVisuals As New VisualCollection
    Friend Const VisualTypeNames As String = "2-D       3-D       Isosurface"
    Friend Const VisualAxisNames As String = "X-AxisY-AxisZ-Axis"

    Friend myUnits As New EngineeringUnits

    Friend myCompartments As New CompartmentCollection

    Friend myHVents As New VentCollection
    Friend myVVents As New VentCollection
    Friend myMVents As New VentCollection
    Friend myHHeats As New VentCollection
    Friend myVHeats As New VentCollection
    Friend Const FaceNames As String = "FrontRightRear Left "
    Friend Const ShapeNames As String = "Round Square"
    Friend Const OrientationNames As String = "Vertical  Horizontal"
    Friend Const OpenTypes As String = "TIMETEMPFLUX"

    Friend myTargets As New TargetCollection
    Friend myDetectors As New TargetCollection
    Friend Const SolutionMethodNames As String = "ImplicitExplicitSteady  "
    Friend Const SolutionTypeNames As String = "Plate      Cylindrical"
    Friend Const DetectorTypes As String = "Smoke    Heat     Sprinkler"
    Friend Const NormalPointsTo As String = "Ceiling    Floor      Front Wall Back Wall  Right Wall Left Wall  "
    Friend myFireProperties As New FireCollection                   ' fire properties (HRR, etc) defined for this test case
    Friend myFires As New FireCollection                            ' fires defined for this test case (location, ignition criterion and link to fire properties for each
    Friend TempFires As New FireCollection
    Friend Const IgnitionNames As String = "Time       TemperatureHeat Flux  "
    Friend Const IgnitionTypes As String = "TIMETEMPFLUX"
    Friend Const FireTypeNames As String = "ConstrainedHeat Source"
    Friend Const CJetNames As String = "OFF    CEILINGWALLS  ALL    "
    Friend firefile() As Integer = {2, 4, 3, 5, 6, 9, 10, 8, 7, 11, 12, 13, 14}
    Friend Const NumFireCurves As Integer = 7
    Friend Const FireCurveTypes As String = "TIME HRR  SOOT CO   TRACEAREA HEIGH"
    Friend FireCurveColumns() As Integer = {0, Fire.FireTime, Fire.FireHRR, Fire.FireSoot, Fire.FireCO, Fire.FireTS, Fire.FireArea, Fire.FireHeight}

    Friend myThermalProperties As New ThermalPropertiesCollection
    Friend TempThermalProperties As New ThermalPropertiesCollection
    Friend Const MaximumThermalProperties As Integer = 150

    Friend myMHeaders As New MonteCarloCollection
    Friend myMRandoms As New MonteCarloCollection
    Friend myMFields As New MonteCarloCollection
    Friend myMFires As New MonteCarloCollection
    Friend myOutputs As New MonteCarloCollection

    Friend dataFileHeader As New Collection                         'comments for the header of a datafile (indicated as !*)
    Friend dataFileComments As New Collection                       'dead keywords and other comments
    Friend thermalFileComments As New Collection                    'comments in the thermal file
    Friend fireFilesComments As New Collection                      'comments in the fire file

    Friend CommandWindowVisible As Boolean = False
    Friend NetHeatFluxCFASTOutput As Boolean = True                 ' True if heat flux output is to be net heat flux rather than incident heat flux (adds -N option to CFAST execution command)
    Friend ValidationOutput As Boolean = False                      ' True if heat flux output is to be net heat flux (adds -V option to CFAST execution command)
    Friend DebugOutput As Boolean = False                           ' True if CFAST is to output debug file from successful RESID solutions (creates casename.resid file when checked)

    Friend SSOutputCompartments As Boolean = True                   ' True if CFAST run should generate a compartments spreadsheet
    Friend SSOutputDevices As Boolean = True                        ' True if CFAST run should generate a devices spreadsheet
    Friend SSOutputMasses As Boolean = True                         ' True if CFAST run should generate a masses spreadsheet
    Friend SSOutputVents As Boolean = True                          ' True if CFAST run should generate a vents spreadsheet
    Friend SSOutputWalls As Boolean = True                          ' True if CFAST run should generate a walls spreadsheet
    Friend Enum BaseUnitsNum    ' Provides an index into the array of base units conversion by type of conversion
        Length = 0
        Mass
        Time
        Temperature
        Pressure
        Energy
        Smoke
    End Enum

    Friend Enum UnitsNum    ' Provides an index into the array of units conversion by type of conversion
        Time = 0
        Temperature
        TemperatureRise
        Pressure
        Length
        Area
        Velocity
        Flowrate
        RTI
        Mass
        MassLoss
        Density
        HRR
        HoC
        HoG
        HeatFlux
        Conductivity
        SpecificHeat
        Smoke
        FireTime = 0
        FireMdot
        FireQdot
        FireHeight
        FireArea
        FireHCN = 9
        FireHCl
        FireSpeciesYields = 5
    End Enum

    Friend Enum fireNum
        compartment = 2
        xPosition
        yPosition
        zposition
        plumeType
        ignType
        ignCriterion
        xNormal
        yNormal
        zNormal
        name
        ignTarget = 9
        limo2 = 2
    End Enum

    Friend Enum formula
        C = 1
        H
        O
        N
        Cl
    End Enum

    Friend Enum chemieNum
        limo2 = 2
        igntemp
        cjetType
        C = 2
        H
        O
        N
        Cl
        chiR
        HoC
        Material
    End Enum

    Friend Enum CFASTlnNum
        keyWord = 1
        version
        title
    End Enum

    Friend Enum thermalNum
        shortName = 1
        Conductivity
        specificHeat
        density
        thickness
        emissivity
        HClCoefficients
        longName = 14
    End Enum

    Friend Enum MaterialNum
        shortName = 2
        Conductivity
        specificHeat
        density
        thickness
        emissivity
        longName
    End Enum

    Friend Enum InsertFireNum
        Fire = 1
        Formula
        Height
        Area
        QDot
        Soot
        CO
        HoC
        Material
    End Enum

    Friend Enum FireSummaryNum
        Fire = 0
        Compartment
        FireID
        IgnitionType
        SetPoint
        Target
        X
        Y
        FirePropertyID
        Fuel
        HRR
    End Enum

    Friend Enum InsertThermalNum
        Material = 1
        Conductivity
        SpecificHeat
        Density
        Thickness
        Emissivity
    End Enum

    Friend Enum InsertDataType
        Fire = 1
        ThermalProperty
        EmbeddedFire = 1
        ObjectFile
    End Enum

    Friend Enum LocationNum
        x = 0
        y
        z
    End Enum

    Friend Enum CompMaterialsColNum
        Layer = 0
        CeilingMaterial
        CeilingThickness
        WallMaterial
        WallThickness
        FloorMaterial
        FloorThickness
    End Enum

    Friend Function IsArrayEmpty(Arr As Object) As Boolean
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' IsArrayEmpty
        ' This function tests whether the array is empty (unallocated). Returns TRUE or FALSE.
        '
        ' The IsArray function indicates whether a variable is an array, but it does not
        ' distinguish between allocated and unallocated arrays. It will return TRUE for both
        ' allocated and unallocated arrays. This function tests whether the array has actually
        ' been allocated.
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

        Dim LB As Long
        Dim UB As Long

        Err.Clear()
        On Error Resume Next
        If IsArray(Arr) = False Then
            ' we weren't passed an array, return True
            IsArrayEmpty = True
        End If

        ' Attempt to get the UBound of the array. If the array is
        ' unallocated, an error will occur.
        UB = UBound(Arr, 1)
        If (Err.Number <> 0) Then
            IsArrayEmpty = True
        Else
            ''''''''''''''''''''''''''''''''''''''''''
            ' On rare occassion, under circumstances I
            ' cannot reliably replictate, Err.Number
            ' will be 0 for an unallocated, empty array.
            ' On these occassions, LBound is 0 and
            ' UBoung is -1.
            ' To accomodate the weird behavior, test to
            ' see if LB > UB. If so, the array is not
            ' allocated.
            ''''''''''''''''''''''''''''''''''''''''''
            Err.Clear()
            LB = LBound(Arr)
            If LB > UB Then
                IsArrayEmpty = True
            Else
                IsArrayEmpty = False
            End If
        End If

    End Function
End Module

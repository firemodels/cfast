Public Class MonteCarlo


    Friend Const MaximumMonteCarlos As Integer = 1000

    ' All units within the class are assumed to be consistent and typically SI

    ' Variables shared by different Montecarlo commands
    Private aID As String                   ' id used a heading for output column
    Private aFYI As String                  ' Descriptor for additional user supplied information
    Private aRealValues(0), aRealConstantValue As Double
    Private aIntegerValues(0), aIntegerConstantValue As Integer
    Private aStringValues(0), aStringConstantValue As String
    Private aLogicalValues(0), aLogicialConstantValue As Boolean

    ' Inputs for &OUTP
    Private aFileType As String             ' 'COMPARTMENTS', 'DEVICES', 'MASSES', 'VENTS', or 'WALLS'
    Private aType As String                 ' 'trigger_greater', 'trigger_lesser', 'minimum', 'maximum', 'integrate', 'check_total_HRR'
    Private aCriterion As Double            ' Value used in 'trigger_...' analysis
    Private aFirstMeasurement As String     ' Name of measurement, second row in spreadsheet
    Private aFirstDevice As String          ' Name of instrument within the measurement, third row in spreadsheet
    Private aSecondMeasurement As String    ' Name of measure for second instrument, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'
    Private aSecondDevice As String         ' Name of second instrument within measurement, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'

    ' Inputs for &MHDR
    Private aNumberofCases As Integer       ' Number of CFAST files to generate
    Private aSeeds(2) As Double             ' an integer pair used to determine random number seeds for distributions
    Private aWriteSeeds As Boolean          ' If ture, all random number seeds are saved to a file
    Private aParameterFile As String        ' file name for parameters files
    Private aWorkFolder As String           ' folder name for work files. Defaults to current folder
    Private aOutputFolder As String         ' folder name for output files. Defaults to current folder

    ' Inputs for &MRND
    Private aDistributionType As String
    Private aMinimum As Double
    Private aMaximum As Double
    Private aMean As Double
    Private aStdev As Double
    Private aAlpha As Double
    Private aBeta As Double
    Private aPeak As Double
    Private aRandomSeeds(0) As Double
    Private aProbabilities(0) As Double
    Private aMinimumOffset As Double
    Private aMaximumOffset As Double
    Private aMinimumField As String
    Private aMaximumField As String
    Private aAddField As String

    ' Inputs for &MFLD
    Private aFieldType As String
    Private aField(2) As String
    Private aRandId As String
    Private aParameterColumnLabel As String
    Private aAddToParameters As Boolean
    Private aScenarioTitles(0) As String
    Private aBaseScalingValue As Double
    Private aPosition As Integer

    ' Inputs for &MFIR
    Private aFireID As String
    Private aBaseFireID As String
    Private aModifyFireAreatoMatchHRR As Boolean
    Private aFireCompartmentRandomGeneratorID As String
    Private aFireCompartmentIDs(0) As String
    Private aAddFireCompartmentIDtoParameters As Boolean
    Private aFireCompartmentIDColumnLabel As String

    Private aFlamingSmolderingIncipientRandomGeneratorID As String
    Private aIncipientFireTypes(0) As String
    Private aTypeofIncipientFireGrowth As String
    Private aFlamingIncipientDelayRandomGeneratorID As String
    Private aFlamingIncipientPeakRandomGeneratorID As String
    Private aSmolderingIncipientDelayRandomGeneratorID As String
    Private aSmolderingIncipientPeakRandomGeneratorID As String
    Private aAddIncipientTypetoParameters As Boolean
    Private aAddIncipientTimetoParameters As Boolean
    Private aAddIncipientPeaktoParameters As Boolean
    Private aIncipientTypeColumnLabel As String
    Private aIncipientTimeColumnLabel As String
    Private aIncipientPeakColumnLabel As String

    Private aScalingFireHRRRandomGeneratorID As String
    Private aScalingFireTimeRandomGeneratorID As String
    Private aAddHRRScaletoParameters As Boolean
    Private aHRRScaleColumnLabel As String
    Private aAddTimeScaletoParameters As Boolean
    Private aTimeScaleColumnLabel As String

    Private aFireHRRGeneratorIDs(0) As String
    Private aFireTimeGeneratorIDs(0) As String
    Private aNumberofGrowthPoints As Integer
    Private aNumberofDecayPoints As Integer
    Private aGrowthExponent As Double
    Private aDecayExponent As Double
    Private aGeneratorIsTimeto1054kW As Boolean
    Private aAddFiretoParameters As Boolean
    Private aAddHRRtoParameters As Boolean
    Private aAddTimetoParameters As Boolean
    Private aHRRLabels(0) As String
    Private aTimeLabels(0) As String

    'Inputs for &MSTT
    Private aAnalysisType As String
    Private aInputFileName As String
    Private aOutputFileName As String
    Private aErrorFileName As String
    Private aLogFileName As String
    Private aColumnLabel As String

    Public Sub New()
        ' Generic New that initializes everything
        ' &OUTP
        aID = ""
        aFileType = ""
        aType = ""
        aCriterion = 0
        aFirstMeasurement = ""
        aFirstDevice = ""
        aSecondMeasurement = ""
        aSecondDevice = ""
        aFYI = ""

        ' &MHDR
        aNumberofCases = 0
        aSeeds(1) = -1001.0
        aSeeds(2) = -1001.0
        aWriteSeeds = False
        aParameterFile = ""
        aWorkFolder = ""
        aOutputFolder = ""

        ' &MRND
        aDistributionType = ""
        aMinimum = -Double.MaxValue
        aMaximum = Double.MaxValue
        aMean = 0.0
        aStdev = -1001.0
        aAlpha = -1001.0
        aBeta = -1001.0
        aRealConstantValue = -1001.0
        aIntegerConstantValue = -1001
        aStringConstantValue = ""
        aLogicialConstantValue = True
        aMinimumOffset = -1001.0
        aMaximumOffset = -1001.0
        aMinimumField = ""
        aMaximumField = ""
        aAddField = ""

        ' &MFLD
        aFieldType = ""
        aField(1) = ""
        aField(2) = ""
        aRandId = ""
        aParameterColumnLabel = ""
        aAddToParameters = True
        aBaseScalingValue = 1.0
        aPosition = 1

        ' &MFIR
        aFireID = ""
        aBaseFireID = ""
        aModifyFireAreatoMatchHRR = False
        aFireCompartmentRandomGeneratorID = ""
        aFireCompartmentIDs(0) = ""
        aAddFireCompartmentIDtoParameters = True
        aFireCompartmentIDColumnLabel = ""

        aFlamingSmolderingIncipientRandomGeneratorID = ""
        aIncipientFireTypes(0) = ""
        aTypeofIncipientFireGrowth = ""
        aFlamingIncipientDelayRandomGeneratorID = ""
        aFlamingIncipientPeakRandomGeneratorID = ""
        aSmolderingIncipientDelayRandomGeneratorID = ""
        aSmolderingIncipientPeakRandomGeneratorID = ""
        aAddIncipientTypetoParameters = True
        aAddIncipientTimetoParameters = True
        aAddIncipientPeaktoParameters = True
        aIncipientTypeColumnLabel = ""
        aIncipientTimeColumnLabel = ""
        aIncipientPeakColumnLabel = ""
        aScalingFireHRRRandomGeneratorID = ""
        aScalingFireTimeRandomGeneratorID = ""
        aAddHRRScaletoParameters = True
        aHRRScaleColumnLabel = ""
        aAddTimeScaletoParameters = True
        aTimeScaleColumnLabel = ""

        aFireHRRGeneratorIDs(0) = ""
        aFireTimeGeneratorIDs(0) = ""
        aNumberofGrowthPoints = 0
        aNumberofDecayPoints = 0
        aGrowthExponent = 0.0
        aDecayExponent = 0.0
        aGeneratorIsTimeto1054kW = True
        aAddFiretoParameters = True
        aAddHRRtoParameters = True
        aAddTimetoParameters = True

        ' &MSTT
        aAnalysisType = ""
        aInputFileName = ""
        aOutputFileName = ""
        aErrorFileName = ""
        aLogFileName = ""
        aColumnLabel = ""
    End Sub
    Public Sub SetStat(ByRef ID As String, ByVal FYI As String, ByVal AnalysisType As String, ByVal InputFileName As String, ByVal OutputFileName As String, ByVal ErrorFileName As String, ByVal LogFileName As String, ByVal ColumnLabel As String)
        aID = ID
        aFYI = FYI
        aAnalysisType = AnalysisType
        aInputFileName = InputFileName
        aOutputFileName = OutputFileName
        aErrorFileName = ErrorFileName
        aLogFileName = LogFileName
        aColumnLabel = ColumnLabel
    End Sub
    Public Sub GetStat(ByRef ID As String, ByRef FYI As String, ByRef AnalysisType As String, ByRef InputFileName As String, ByRef OutputFileName As String, ByRef ErrorFileName As String, ByRef LogFileName As String, ByRef ColumnLabel As String)
        ID = aID
        aFYI = FYI
        AnalysisType = aAnalysisType
        InputFileName = aInputFileName
        OutputFileName = aOutputFileName
        ErrorFileName = aErrorFileName
        LogFileName = aLogFileName
        ColumnLabel = aColumnLabel
    End Sub
    Public Sub SetFire(ByVal ID As String, ByVal FYI As String, ByVal FireID As String, ByVal BaseFireID As String, ByVal ModifyFireAreatoMatchHRR As Boolean, ByVal FireCompartmentRandomGeneratorID As String, ByVal FireCompartmentIDs() As String, ByVal AddFireCompartmentIDtoParameters As Boolean, ByVal FireCompartmentIDColumnLabel As String, ByVal FlamingSmolderingIncipientRandomGeneratorID As String, ByVal IncipientFireTypes() As String, ByVal TypeofIncipientFireGrowth As String, ByVal FlamingIncipientDelayRandomGeneratorID As String, ByVal FlamingIncipientPeakRandomGeneratorID As String, ByVal SmolderingIncipientDelayRandomGeneratorID As String, ByVal SmolderingIncipientPeakRandomGeneratorID As String, ByVal AddIncipientTypetoParameters As Boolean, ByVal AddIncipientTimetoParameters As Boolean, ByVal AddIncipientPeaktoParameters As Boolean, ByVal IncipientTypeColumnLabel As String, ByVal IncipientTimeColumnLabel As String, ByVal IncipientPeakColumnLabel As String, ByVal ScalingFireHRRRandomGeneratorID As String, ByVal ScalingFireTimeRandomGeneratorID As String, ByVal AddHRRScaletoParameters As Boolean, ByVal HRRScaleColumnLabel As String, ByVal AddTimeScaletoParameters As Boolean, ByVal TimeScaleColumnLabel As String, ByVal FireHRRGeneratorIDs() As String, ByVal FireTimeGeneratorIDs() As String, ByVal NumberofGrowthPoints As Integer, ByVal NumberofDecayPoints As Integer, ByVal GrowthExponent As Double, ByVal DecayExponent As Double, ByVal GeneratorIsTimeto1054kW As Boolean, ByVal AddFiretoParameters As Boolean, ByVal AddHRRtoParameters As Boolean, ByVal AddTimetoParameters As Boolean, ByVal HRRLabels() As String, ByVal TimeLabels() As String)
        Dim i, max As Integer
        aID = ID
        aFYI = FYI
        aFireID = FireID
        aBaseFireID = BaseFireID
        aModifyFireAreatoMatchHRR = ModifyFireAreatoMatchHRR
        aFireCompartmentRandomGeneratorID = FireCompartmentRandomGeneratorID
        If Not IsArrayEmpty(FireCompartmentIDs) Then
            max = FireCompartmentIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim aFireCompartmentIDs(max)
                For i = 1 To max
                    aFireCompartmentIDs(i) = FireCompartmentIDs(i)
                Next
            End If
        End If
        aAddFireCompartmentIDtoParameters = AddFireCompartmentIDtoParameters
        aFireCompartmentIDColumnLabel = FireCompartmentIDColumnLabel
        aAddFireCompartmentIDtoParameters = AddFireCompartmentIDtoParameters
        aFireCompartmentIDColumnLabel = FireCompartmentIDColumnLabel
        aFlamingSmolderingIncipientRandomGeneratorID = FlamingSmolderingIncipientRandomGeneratorID
        If Not IsArrayEmpty(IncipientFireTypes) Then
            max = IncipientFireTypes.GetUpperBound(0)
            If max > 0 Then
                ReDim aIncipientFireTypes(max)
                For i = 1 To max
                    aIncipientFireTypes(i) = IncipientFireTypes(i)
                Next
            End If
        End If
        aFlamingIncipientDelayRandomGeneratorID = FlamingIncipientDelayRandomGeneratorID
        aFlamingIncipientPeakRandomGeneratorID = FlamingIncipientPeakRandomGeneratorID
        aSmolderingIncipientDelayRandomGeneratorID = SmolderingIncipientDelayRandomGeneratorID
        aSmolderingIncipientPeakRandomGeneratorID = SmolderingIncipientPeakRandomGeneratorID
        aAddIncipientTypetoParameters = AddIncipientTypetoParameters
        aAddIncipientTimetoParameters = AddIncipientTimetoParameters
        aAddIncipientPeaktoParameters = AddIncipientPeaktoParameters
        aIncipientTypeColumnLabel = IncipientTypeColumnLabel
        aIncipientTimeColumnLabel = IncipientTimeColumnLabel
        aIncipientPeakColumnLabel = IncipientPeakColumnLabel
        aTypeofIncipientFireGrowth = TypeofIncipientFireGrowth
        If aTypeofIncipientFireGrowth = "" Then
            If aFlamingIncipientDelayRandomGeneratorID <> "" And aSmolderingIncipientDelayRandomGeneratorID <> "" Then
                aTypeofIncipientFireGrowth = "RANDOM"
            ElseIf aFlamingIncipientDelayRandomGeneratorID <> "" Then
                aTypeofIncipientFireGrowth = "FLAMING"
            ElseIf aSmolderingIncipientDelayRandomGeneratorID <> "" Then
                aTypeofIncipientFireGrowth = "SMOLDERING"
            End If
        End If

        aScalingFireHRRRandomGeneratorID = ScalingFireHRRRandomGeneratorID
        aScalingFireTimeRandomGeneratorID = ScalingFireTimeRandomGeneratorID
        aAddHRRScaletoParameters = AddHRRScaletoParameters
        aHRRScaleColumnLabel = HRRScaleColumnLabel
        aAddTimeScaletoParameters = AddTimeScaletoParameters
        aTimeScaleColumnLabel = TimeScaleColumnLabel

        If Not IsArrayEmpty(FireHRRGeneratorIDs) Then
            max = FireHRRGeneratorIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim aFireHRRGeneratorIDs(max)
                For i = 1 To max
                    aFireHRRGeneratorIDs(i) = FireHRRGeneratorIDs(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(FireTimeGeneratorIDs) Then
            max = FireTimeGeneratorIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim aFireTimeGeneratorIDs(max)
                For i = 1 To max
                    aFireTimeGeneratorIDs(i) = FireTimeGeneratorIDs(i)
                Next
            End If
        End If
        aNumberofGrowthPoints = NumberofGrowthPoints
        aNumberofDecayPoints = NumberofDecayPoints
        aGrowthExponent = GrowthExponent
        aDecayExponent = DecayExponent
        aGeneratorIsTimeto1054kW = GeneratorIsTimeto1054kW
        aAddFiretoParameters = AddFiretoParameters
        aAddHRRtoParameters = AddHRRtoParameters
        aAddTimetoParameters = AddTimetoParameters
        If Not IsArrayEmpty(HRRLabels) Then
            max = HRRLabels.GetUpperBound(0)
            If max > 0 Then
                ReDim aHRRLabels(max)
                For i = 1 To max
                    aHRRLabels(i) = HRRLabels(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(TimeLabels) Then
            max = TimeLabels.GetUpperBound(0)
            If max > 0 Then
                ReDim aTimeLabels(max)
                For i = 1 To max
                    aTimeLabels(i) = TimeLabels(i)
                Next
            End If
        End If
    End Sub
    Public Sub GetFire(ByRef ID As String, ByRef FYI As String, ByRef FireID As String, ByRef BaseFireID As String, ByRef ModifyFireAreatoMatchHRR As Boolean, ByRef FireCompartmentRandomGeneratorID As String, ByRef FireCompartmentIDs() As String, ByRef AddFireCompartmentIDtoParameters As Boolean, ByRef FireCompartmentIDColumnLabel As String, ByRef FlamingSmolderingIncipientRandomGeneratorID As String, ByRef IncipientFireTypes() As String, ByRef TypeofIncipientFireGrowth As String, ByRef FlamingIncipientDelayRandomGeneratorID As String, ByRef FlamingIncipientPeakRandomGeneratorID As String, ByRef SmolderingIncipientDelayRandomGeneratorID As String, ByRef SmolderingIncipientPeakRandomGeneratorID As String, ByRef AddIncipientTypetoParameters As Boolean, ByRef AddIncipientTimetoParameters As Boolean, ByRef AddIncipientPeaktoParameters As Boolean, ByRef IncipientTypeColumnLabel As String, ByRef IncipientTimeColumnLabel As String, ByRef IncipientPeakColumnLabel As String, ByRef ScalingFireHRRRandomGeneratorID As String, ByRef ScalingFireTimeRandomGeneratorID As String, ByRef AddHRRScaletoParameters As Boolean, ByRef HRRScaleColumnLabel As String, ByRef AddTimeScaletoParameters As Boolean, ByRef TimeScaleColumnLabel As String, ByRef FireHRRGeneratorIDs() As String, ByRef FireTimeGeneratorIDs() As String, ByRef NumberofGrowthPoints As Integer, ByRef NumberofDecayPoints As Integer, ByRef GrowthExponent As Double, ByRef DecayExponent As Double, ByRef GeneratorIsTimeto1054kW As Boolean, ByRef AddFiretoParameters As Boolean, ByRef AddHRRtoParameters As Boolean, ByRef AddTimetoParameters As Boolean, ByRef HRRLabels() As String, ByRef TimeLabels() As String)
        Dim i, max As Integer
        ID = aID
        FYI = aFYI
        FireID = aFireID
        BaseFireID = aBaseFireID
        ModifyFireAreatoMatchHRR = aModifyFireAreatoMatchHRR
        FireCompartmentRandomGeneratorID = aFireCompartmentRandomGeneratorID
        If Not IsArrayEmpty(aFireCompartmentIDs) Then
            max = aFireCompartmentIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim FireCompartmentIDs(max)
                For i = 1 To max
                    FireCompartmentIDs(i) = aFireCompartmentIDs(i)
                Next
            End If
        End If
        AddFireCompartmentIDtoParameters = aAddFireCompartmentIDtoParameters
        FireCompartmentIDColumnLabel = aFireCompartmentIDColumnLabel
        AddFireCompartmentIDtoParameters = aAddFireCompartmentIDtoParameters
        FireCompartmentIDColumnLabel = aFireCompartmentIDColumnLabel
        FlamingSmolderingIncipientRandomGeneratorID = aFlamingSmolderingIncipientRandomGeneratorID
        If Not IsArrayEmpty(aIncipientFireTypes) Then
            max = aIncipientFireTypes.GetUpperBound(0)
            If max > 0 Then
                ReDim IncipientFireTypes(max)
                For i = 1 To max
                    IncipientFireTypes(i) = aIncipientFireTypes(i)
                Next
            End If
        End If
        TypeofIncipientFireGrowth = aTypeofIncipientFireGrowth
        FlamingIncipientDelayRandomGeneratorID = aFlamingIncipientDelayRandomGeneratorID
        FlamingIncipientPeakRandomGeneratorID = aFlamingIncipientPeakRandomGeneratorID
        SmolderingIncipientDelayRandomGeneratorID = aSmolderingIncipientDelayRandomGeneratorID
        SmolderingIncipientPeakRandomGeneratorID = aSmolderingIncipientPeakRandomGeneratorID
        AddIncipientTypetoParameters = aAddIncipientTypetoParameters
        AddIncipientTimetoParameters = aAddIncipientTimetoParameters
        AddIncipientPeaktoParameters = aAddIncipientPeaktoParameters
        IncipientTypeColumnLabel = aIncipientTypeColumnLabel
        IncipientTimeColumnLabel = aIncipientTimeColumnLabel
        IncipientPeakColumnLabel = aIncipientPeakColumnLabel
        ScalingFireHRRRandomGeneratorID = aScalingFireHRRRandomGeneratorID
        ScalingFireTimeRandomGeneratorID = aScalingFireTimeRandomGeneratorID
        AddHRRScaletoParameters = aAddHRRScaletoParameters
        HRRScaleColumnLabel = aHRRScaleColumnLabel
        AddTimeScaletoParameters = aAddTimeScaletoParameters
        TimeScaleColumnLabel = aTimeScaleColumnLabel
        If Not IsArrayEmpty(aFireHRRGeneratorIDs) Then
            max = aFireHRRGeneratorIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim FireHRRGeneratorIDs(max)
                For i = 1 To max
                    FireHRRGeneratorIDs(i) = aFireHRRGeneratorIDs(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(aFireTimeGeneratorIDs) Then
            max = aFireTimeGeneratorIDs.GetUpperBound(0)
            If max > 0 Then
                ReDim FireTimeGeneratorIDs(max)
                For i = 1 To max
                    FireTimeGeneratorIDs(i) = aFireTimeGeneratorIDs(i)
                Next
            End If
        End If
        NumberofGrowthPoints = aNumberofGrowthPoints
        NumberofDecayPoints = aNumberofDecayPoints
        GrowthExponent = aGrowthExponent
        DecayExponent = aDecayExponent
        GeneratorIsTimeto1054kW = aGeneratorIsTimeto1054kW
        AddFiretoParameters = aAddFiretoParameters
        AddHRRtoParameters = aAddHRRtoParameters
        AddTimetoParameters = aAddTimetoParameters
        If Not IsArrayEmpty(aHRRLabels) Then
            max = aHRRLabels.GetUpperBound(0)
            If max > 0 Then
                ReDim HRRLabels(max)
                For i = 1 To max
                    HRRLabels(i) = aHRRLabels(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(aTimeLabels) Then
            max = aTimeLabels.GetUpperBound(0)
            If max > 0 Then
                ReDim TimeLabels(max)
                For i = 1 To max
                    TimeLabels(i) = aTimeLabels(i)
                Next
            End If
        End If
    End Sub
    Public Sub SetField(ByVal Id As String, ByVal FieldType As String, ByVal Field() As String, ByVal RandId As String, ByVal ParameterColumnLabel As String, ByVal AddToParameters As Boolean, RealValues() As Double, ByVal IntegerValues() As Integer, ByVal StringValues() As String, ByVal LogicalValues() As Boolean, ByVal BaseScalingValue As Double, ByVal Position As Integer, ByVal FYI As String)
        Dim i, max As Integer
        aID = Id
        aFieldType = FieldType
        If Not IsArrayEmpty(Field) Then
            max = Field.GetUpperBound(0)
            If max >= 2 Then
                ReDim aField(max)
                For i = 1 To 2
                    aField(i) = Field(i)
                Next
            End If
        End If
        aRandId = RandId
        aParameterColumnLabel = ParameterColumnLabel
        aAddToParameters = AddToParameters
        If Not IsArrayEmpty(RealValues) Then
            max = RealValues.GetUpperBound(0)
            If max > 0 Then
                ReDim aRealValues(max)
                For i = 1 To max
                    aRealValues(i) = RealValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(IntegerValues) Then
            max = IntegerValues.GetUpperBound(0)
            If max > 0 Then
                ReDim aIntegerValues(max)
                For i = 1 To max
                    aIntegerValues(i) = IntegerValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(StringValues) Then
            max = StringValues.GetUpperBound(0)
            If max > 0 Then
                ReDim aStringValues(max)
                For i = 1 To max
                    aStringValues(i) = StringValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(LogicalValues) Then
            max = LogicalValues.GetUpperBound(0)
            If max > 0 Then
                ReDim aLogicalValues(max)
                For i = 1 To max
                    aLogicalValues(i) = LogicalValues(i)
                Next
            End If
        End If
        aBaseScalingValue = BaseScalingValue
        aPosition = Position
        aFYI = FYI
    End Sub
    Public Sub GetField(ByRef Id As String, ByRef FieldType As String, ByRef Field() As String, ByRef RandId As String, ByRef ParameterColumnLabel As String, ByRef AddToParameters As Boolean, ByRef RealValues() As Double, ByRef IntegerValues() As Integer, ByRef StringValues() As String, ByRef LogicalValues() As Boolean, ByRef BaseScalingValue As Double, ByRef Position As Integer, ByRef FYI As String)
        Dim i, max As Integer
        Id = aID
        FieldType = aFieldType
        If Not IsArrayEmpty(aField) Then
            max = aField.GetUpperBound(0)
            If max >= 2 Then
                ReDim Field(max)
                For i = 1 To 2
                    Field(i) = aField(i)
                Next
            End If
        End If
        RandId = aRandId
        ParameterColumnLabel = aParameterColumnLabel
        AddToParameters = aAddToParameters
        If Not IsArrayEmpty(aRealValues) Then
            max = aRealValues.GetUpperBound(0)
            If max > 0 Then
                ReDim RealValues(max)
                For i = 1 To max
                    RealValues(i) = aRealValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(aIntegerValues) Then
            max = aIntegerValues.GetUpperBound(0)
            If max > 0 Then
                ReDim IntegerValues(max)
                For i = 1 To max
                    IntegerValues(i) = aIntegerValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(aStringValues) Then
            max = aStringValues.GetUpperBound(0)
            If max > 0 Then
                ReDim StringValues(max)
                For i = 1 To max
                    StringValues(i) = aStringValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(aLogicalValues) Then
            max = aLogicalValues.GetUpperBound(0)
            If max > 0 Then
                ReDim LogicalValues(max)
                For i = 1 To max
                    LogicalValues(i) = aLogicalValues(i)
                Next
            End If
        End If
        BaseScalingValue = aBaseScalingValue
        Position = aPosition
        FYI = aFYI
    End Sub
    Public Sub SetRandom(ByVal Id As String, ByVal DistributionType As String, ByVal Minimum As Double, ByVal Maximum As Double, ByVal Mean As Double, ByVal Stdev As Double, ByVal Alpha As Double, ByVal Beta As Double, ByVal Peak As Double, ByVal RandomSeeds() As Double, ByVal RealValues() As Double, ByVal RealConstantValue As Double, ByVal Probabilities() As Double, ByVal MinimumOffset As Double, ByVal MaximumOffset As Double, ByVal MinimumField As String, ByVal MaximumField As String, ByVal AddField As String, ByVal FYI As String)
        ' Define values from an &MRND input
        Dim i, max As Integer
        aID = Id
        aDistributionType = DistributionType
        aMinimum = Minimum
        aMaximum = Maximum
        aMean = Mean
        aStdev = Stdev
        aAlpha = Alpha
        aBeta = Beta
        max = RandomSeeds.GetUpperBound(0)
        If max > 0 Then
            ReDim aRandomSeeds(max)
            For i = 1 To max
                aRandomSeeds(i) = RandomSeeds(i)
            Next
        End If
        If Not IsArrayEmpty(RealValues) Then
            max = RealValues.GetUpperBound(0)
            If max > 0 Then
                ReDim aRealValues(max)
                For i = 1 To max
                    aRealValues(i) = RealValues(i)
                Next
            End If
        End If
        If Not IsArrayEmpty(Probabilities) Then
            max = Probabilities.GetUpperBound(0)
            If max > 0 Then
                ReDim aProbabilities(max)
                For i = 1 To max
                    aProbabilities(i) = Probabilities(i)
                Next
            End If
        End If
        aRealConstantValue = RealConstantValue
        aMinimumOffset = MinimumOffset
        aMaximumOffset = MaximumOffset
        aMinimumField = MinimumField
        aMaximumField = MaximumField
        aAddField = AddField
        aFYI = FYI
    End Sub
    Public Sub GetRandom(ByRef id As String, ByRef DistributionType As String, ByRef Minimum As Double, ByRef Maximum As Double, ByRef Mean As Double, ByRef Stdev As Double, ByRef Alpha As Double, ByRef Beta As Double, ByRef Peak As Double, ByRef RandomSeeds() As Double, ByRef RealValues() As Double, ByRef RealConstantValue As Double, ByRef IntegerValues() As Integer, ByRef IntegerConstantValue As Integer, ByRef StringValues() As String, ByRef StringConstantValue As String, ByRef LogicalValues() As Boolean, ByRef LogicalConstantValue As Boolean, ByRef Probabilities() As Double, ByRef MinimumOffset As Double, ByRef MaximumOffset As Double, ByRef MinimumField As String, ByRef MaximumField As String, ByRef AddField As String, ByRef FYI As String)
        ' Define values from an &MRND input
        Dim i, max As Integer
        id = aID
        DistributionType = aDistributionType
        Minimum = aMinimum
        Maximum = aMaximum
        Mean = aMean
        Stdev = aStdev
        Alpha = aAlpha
        Beta = aBeta
        ReDim RandomSeeds(0)
        If Not Data.IsArrayEmpty(aRandomSeeds) Then
            max = aRandomSeeds.GetUpperBound(0)
            If max > 0 Then
                ReDim RandomSeeds(max)
                For i = 1 To max
                    RandomSeeds(i) = aRandomSeeds(i)
                Next
            End If
        End If
        ReDim RealValues(0)
        If Not Data.IsArrayEmpty(aRealValues) Then
            max = aRealValues.GetUpperBound(0)
            If max > 0 Then
                ReDim RealValues(max)
                For i = 1 To max
                    RealValues(i) = aRealValues(i)
                Next
            End If
        End If
        ReDim IntegerValues(0)
        If Not Data.IsArrayEmpty(aIntegerValues) Then
            max = aIntegerValues.GetUpperBound(0)
            If max > 0 Then
                ReDim IntegerValues(max)
                For i = 1 To max
                    IntegerValues(i) = aIntegerValues(i)
                Next
            End If
        End If
        ReDim aStringValues(0)
        If Not Data.IsArrayEmpty(aStringValues) Then
            max = aStringValues.GetUpperBound(0)
            If max > 0 Then
                ReDim StringValues(max)
                For i = 1 To max
                    StringValues(i) = aStringValues(i)
                Next
            End If
        End If
        ReDim LogicalValues(0)
        If Not Data.IsArrayEmpty(aLogicalValues) Then
            max = aLogicalValues.GetUpperBound(0)
            If max > 0 Then
                ReDim LogicalValues(max)
                For i = 1 To max
                    LogicalValues(i) = aLogicalValues(i)
                Next
            End If
        End If
        ReDim Probabilities(0)
        If Not Data.IsArrayEmpty(aProbabilities) Then
            max = aProbabilities.GetUpperBound(0)
            If max > 0 Then
                ReDim Probabilities(max)
                For i = 1 To max
                    Probabilities(i) = aProbabilities(i)
                Next
            End If
        End If
        RealConstantValue = aRealConstantValue
        IntegerConstantValue = aIntegerConstantValue
        StringConstantValue = aStringConstantValue
        LogicalConstantValue = aLogicialConstantValue
        MinimumOffset = aMinimumOffset
        MaximumOffset = aMaximumOffset
        MinimumField = aMinimumField
        MaximumField = aMaximumField
        AddField = aAddField
        FYI = aFYI
    End Sub
    Public Sub SetHeader(ByVal NumberofCases As Integer, ByVal Seeds() As Double, ByVal WriteSeeds As Boolean, ByVal ParameterFile As String, WorkFolder As String, OutputFolder As String)
        aNumberofCases = NumberofCases
        If Seeds.GetUpperBound(0) >= 2 Then
            aSeeds(1) = Seeds(1)
            aSeeds(2) = Seeds(2)
        End If
        aWriteSeeds = WriteSeeds
        aParameterFile = ParameterFile
        aWorkFolder = WorkFolder
        aOutputFolder = OutputFolder
    End Sub
    Public Sub GetHeader(ByRef NumberofCases As Integer, Seeds() As Double, WriteSeeds As Boolean, ByRef ParameterFile As String, WorkFolder As String, OutputFolder As String)
        NumberofCases = aNumberofCases
        If Seeds.GetUpperBound(0) >= 2 Then
            Seeds(1) = aSeeds(1)
            Seeds(2) = aSeeds(2)
        End If
        WriteSeeds = aWriteSeeds
        ParameterFile = aParameterFile
        WorkFolder = aWorkFolder
        OutputFolder = aOutputFolder
    End Sub
    Public Sub SetOutput(ByVal ID As String, ByVal FileType As String, ByVal Type As String, ByVal Criterion As Double, ByVal FirstMeasurement As String, ByVal FirstDevice As String, ByVal SecondMeasurement As String, ByVal SecondDevice As String, ByVal fyi As String)
        aID = ID
        aFileType = FileType
        aType = Type
        aCriterion = Criterion
        aFirstMeasurement = FirstMeasurement
        aFirstDevice = FirstDevice
        aSecondMeasurement = SecondMeasurement
        aSecondDevice = SecondDevice
    End Sub
    Public Sub GetOutput(ByRef ID As String, ByRef FileType As String, ByRef Type As String, ByRef Criterion As Double, ByRef FirstMeasurement As String, ByRef FirstDevice As String, ByRef SecondMeasurement As String, ByRef SecondDevice As String, ByRef FYI As String)
        ID = aID
        FileType = aFileType
        Type = aType
        Criterion = aCriterion
        FirstMeasurement = aFirstMeasurement
        FirstDevice = aFirstDevice
        SecondMeasurement = aSecondMeasurement
        SecondDevice = aSecondDevice
        FYI = aFYI
    End Sub
End Class
Public Class MonteCarloCollection
    Inherits System.Collections.CollectionBase
    Friend ReadOnly Maximum As Integer = MonteCarlo.MaximumMonteCarlos

    Public Sub Add(ByVal aMonteCarlo As MonteCarlo)
        List.Add(aMonteCarlo)
    End Sub
    Default Public Property Item(ByVal index As Integer) As MonteCarlo
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). MonteCarlo number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aMonteCarlo As New MonteCarlo
                Return aMonteCarlo
            Else
                Return CType(List.Item(index), MonteCarlo)
            End If
        End Get
        Set(ByVal Value As MonteCarlo)
            List.Item(index) = Value
        End Set
    End Property
End Class
Public Class MonteCarlo


    Friend Const MaximumMonteCarlos As Integer = 1000

    ' All units within the class are assumed to be consistent and typically SI

    ' Variables shared by different Montecarlo commands
    Private aID As String                   ' id used a heading for output column
    Private aFYI As String                  ' Descriptor for additional user supplied information

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
    Private aValueType As String
    Private aMinimum As Double
    Private aMaximum As Double
    Private aMean As Double
    Private aStdev As Double
    Private aAlpha As Double
    Private aBeta As Double
    Private aPeak As Double
    Private aRandomSeeds(0) As Double
    Private aRealValues(0), aRealConstantValue As Double
    Private aIntegerValues(0), aIntegerConstantValue As Integer
    Private aStringValues(0), aStringConstantValue As String
    Private aLogicalValues(0), aLogicialConstantValue As Boolean
    Private aProbabilities(0) As Double
    Private aMinimumOffset As Double
    Private aMaximumOffset As Double
    Private aMinimumField As String
    Private aMaximumField As String
    Private aAddField As String

    ' Inputs for &MFIR
    Private aFireID As String
    Private aBaseFireID As String
    Private aModifyFireAreatoMatchHRR As Boolean
    Private aFireCompartmentRandomGeneratorID As String
    Private aFireCompartmentIDs(0) As String
    Private aAddFireCompartmentIDtoParameters As Boolean
    Private aFireCompartmentIDColumnLabel As String

    Private aFlamingSmolderingIgnitionRandomGeneratorID As String
    Private aIncipientFireTypes(0) As String
    Private aTypeofIncipientFireGrowth As String
    Private aFlamingIgnitionDelayRandomGeneratorID As String
    Private aPeakFlamingIgnitionRandomGeneratorID As String
    Private aSmolderingIgnitionDelayRandomGeneratorID As String
    Private aPeakSmolderingIgnitionRandomGeneratorID As String
    Private aAddIgnitionTypetoParameters As Boolean
    Private aAddSmolderingIgnitionTimetoParameters As Boolean
    Private aAddSmolderingIgnitionPeaktoParameters As Boolean
    Private aAddFlamingIgnitionTimetoParameters As Boolean
    Private aAddFlamingIgnitionPeaktoParameters As Boolean
    Private aIngitionTypeColumnLabel As String
    Private aFlamingIgnitionTimeColumnLabel As String
    Private aFlamingIgnitionPeakColumnLabel As String
    Private aSmolderingIgnitionTimeColumnLabel As String
    Private aSmolderingIgnitionPeakColumnLabel As String

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
    Private aTimeto1054kW As Double
    Private aTimeto0kW As Double
    Private aAddFiretoParameters As Boolean
    Private aAddHRRtoParameters As Boolean
    Private aAddTimetoParameters As Boolean
    Private aHRRLabels(0) As String
    Private aTimeLabeels As String

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
        aValueType = ""
        aMinimum = -Double.MaxValue
        aMaximum = Double.MaxValue
        aMean = 0.0
        aStdev = 0.0
        aAlpha = 0.0
        aBeta = 0.0
        ReDim aRandomSeeds(0)
        ReDim aRealValues(0)
        ReDim aIntegerValues(0)
        ReDim aStringValues(0)
        ReDim aLogicalValues(0)
        ReDim aProbabilities(0)
        aRealConstantValue = 0.0
        aIntegerConstantValue = 0
        aStringConstantValue = ""
        aLogicialConstantValue = True
        aMinimumOffset = 0
        aMaximumOffset = 0
        aMinimumField = ""
        aMaximumField = ""
        aAddField = ""
    End Sub
    Public Sub SetRandom(ByVal id As String, ByVal DistributionType As String, ByVal ValueType As String, ByVal Minimum As Double, ByVal Maximum As Double, ByVal Mean As Double, ByVal Stdev As Double, ByVal Alpha As Double, ByVal Beta As Double, ByVal Peak As Double, ByVal RandomSeeds() As Double, ByVal RealValues() As Double, ByVal RealConstantValue As Double, ByVal IntegerValues() As Integer, ByVal IntegerConstantValue As Integer, ByVal StringValues() As String, ByVal StringConstantValue As String, ByVal LogicalValues() As Boolean, ByVal LogicalConstantValue As Boolean, ByVal Probabilities() As Double, ByVal MinimumOffset As Double, ByVal MaximumOffset As Double, ByVal MinimumField As String, ByVal MaximumField As String, ByVal AddField As String, ByVal FYI As String)
        ' Define values from an &MRND input
        Dim i, max As Integer
        aID = id
        aDistributionType = DistributionType
        aValueType = ValueType
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
        aIntegerConstantValue = IntegerConstantValue
        aStringConstantValue = StringConstantValue
        aLogicialConstantValue = LogicalConstantValue
        aMinimumOffset = MinimumOffset
        aMaximumOffset = MaximumOffset
        aMinimumField = MinimumField
        aMaximumField = MaximumField
        aAddField = AddField
        aFYI = FYI
    End Sub

    Public Sub GetRandom(ByRef DistributionType As String, ByRef ValueType As String, ByRef Minimum As Double, ByRef Maximum As Double, ByRef Mean As Double, ByRef Stdev As Double, ByRef Alpha As Double, ByRef Beta As Double, ByRef Peak As Double, ByRef RandomSeeds() As Double, ByRef RealValues() As Double, ByRef RealConstantValue As Double, ByRef IntegerValues() As Integer, ByRef IntegerConstantValue As Integer, ByRef StringValues() As String, ByRef StringConstantValue As String, ByRef LogicalValues() As Boolean, ByRef LogicalConstantValue As Boolean, ByRef Probabilities() As Double, ByRef MinimumOffset As Double, ByRef MaximumOffset As Double, ByRef MinimumField As String, ByRef MaximumField As String, ByRef AddField As String)
        ' Get values for an &MRND output
        Dim i, max As Integer
        aDistributionType = DistributionType
        aValueType = ValueType
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
        max = RealValues.GetUpperBound(0)
        If max > 0 Then
            ReDim aRealValues(max)
            For i = 1 To max
                aRealValues(i) = RealValues(i)
            Next
        End If
        max = IntegerValues.GetUpperBound(0)
        If max > 0 Then
            ReDim aIntegerValues(max)
            For i = 1 To max
                aIntegerValues(i) = IntegerValues(i)
            Next
        End If
        max = StringValues.GetUpperBound(0)
        If max > 0 Then
            ReDim aStringValues(max)
            For i = 1 To max
                aStringValues(i) = StringValues(i)
            Next
        End If
        max = LogicalValues.GetUpperBound(0)
        If max > 0 Then
            ReDim aLogicalValues(max)
            For i = 1 To max
                aLogicalValues(i) = LogicalValues(i)
            Next
        End If
        max = Probabilities.GetUpperBound(0)
        If max > 0 Then
            ReDim aProbabilities(max)
            For i = 1 To max
                aProbabilities(i) = Probabilities(i)
            Next
        End If
        aRealConstantValue = RealConstantValue
        aIntegerConstantValue = IntegerConstantValue
        aStringConstantValue = StringConstantValue
        aLogicialConstantValue = LogicalConstantValue
        aMinimumOffset = MinimumOffset
        aMaximumOffset = MaximumOffset
        aMinimumField = MinimumField
        aMaximumField = MaximumField
        aAddField = AddField
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
        ID = aID
        aFileType = FileType
        aType = Type
        aCriterion = Criterion
        aFirstMeasurement = FirstMeasurement
        aFirstDevice = FirstDevice
        aSecondMeasurement = SecondMeasurement
        aSecondDevice = SecondDevice
    End Sub
    Public Sub GetOutput(ByRef ID As String, ByRef FileType As String, ByRef Type As String, ByRef Criterion As Double, ByRef FirstMeasurement As String, ByRef FirstDevice As String, ByRef SecondMeasurement As String, ByRef SecondDevice As String, ByRef FYI As String)
        aID = ID
        FileType = aFileType
        Type = aType
        Criterion = aCriterion
        FirstMeasurement = aFirstMeasurement
        FirstDevice = aFirstDevice
        SecondMeasurement = aSecondMeasurement
        SecondDevice = aSecondDevice
        FYI = aFYI
    End Sub
    Private Function IsArrayEmpty(Arr As Object) As Boolean
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
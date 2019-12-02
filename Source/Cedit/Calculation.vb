Public Class Calculation


    Friend Const MaximumCalculations As Integer = 1000

    ' All units within the class are assumed to be consistent and typically SI
    Private aID As String                   ' id used a heading for output column
    Private aFileType As String             ' 'wall' for '_w', 'normal' for '_n', 'flow' for '_f', 'mass' for '_m', 'species' for '_s'
    Private aType As String                 ' 'trigger_greater', 'trigger_lesser', 'minimum', 'maximum', 'integrate', 'check_total_HRR'
    Private aCriteria As Single             ' Value used in 'trigger_...' analysis
    Private aFirstMeasurement As String     ' Name of measurement, second row in spreadsheet
    Private aFirstName As String            ' Name of instrument within the measurement, third row in spreadsheet
    Private aSecondMeasurement As String    ' Name of measure for second instrument, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'
    Private aSecondName As String           ' Name of second instrument within measurement, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'

    Public Sub New()
        aID = ""
        aFileType = ""
        aType = ""
        aCriteria = 0
        aFirstMeasurement = ""
        aFirstName = ""
        aSecondMeasurement = ""
        aSecondName = ""
    End Sub
    Public Sub New(ByVal ID As String, ByVal FileType As String, ByVal Type As String, ByVal Criteria As Single, ByVal FirstMeasurement As String, ByVal FirstName As String, ByVal SecondMeasurement As String, ByVal SecondName As String)
        aID = ID
        aFileType = FileType
        aType = Type
        aCriteria = Criteria
        aFirstMeasurement = FirstMeasurement
        aFirstName = FirstName
        aSecondMeasurement = SecondMeasurement
        aSecondName = SecondName
    End Sub
    Public ReadOnly Property ID As String
        Get
            Return aID
        End Get
    End Property
    Public ReadOnly Property FileType As String
        Get
            Return aFileType
        End Get
    End Property
    Public ReadOnly Property Type As String
        Get
            Return aType
        End Get
    End Property
    Public ReadOnly Property Criteria As Single
        Get
            Return aCriteria
        End Get
    End Property
    Public ReadOnly Property FirstMeasurement As String
        Get
            Return aFirstMeasurement
        End Get
    End Property
    Public ReadOnly Property FirstName As String
        Get
            Return aFirstName
        End Get
    End Property
    Public ReadOnly Property SecondMeasurement As String
        Get
            Return aSecondMeasurement
        End Get
    End Property
    Public ReadOnly Property SecondName As String
        Get
            Return aSecondName
        End Get
    End Property
    Public Sub GetCalc(ByRef ID As String, ByRef FileType As String, ByRef Type As String, ByRef Criteria As Single, ByRef FirstMeasurement As String, ByRef FirstName As String, ByRef SecondMeasurement As String, ByRef SecondName As String)
        aID = ID
        FileType = aFileType
        Type = aType
        Criteria = aCriteria
        FirstMeasurement = aFirstMeasurement
        FirstName = aFirstName
        SecondMeasurement = aSecondMeasurement
        SecondName = aSecondName
    End Sub
End Class
Public Class CalculationCollection
    Inherits System.Collections.CollectionBase
    Friend ReadOnly Maximum As Integer = Calculation.MaximumCalculations

    Public Sub Add(ByVal aCalculation As Calculation)
        List.Add(aCalculation)
    End Sub
    Default Public Property Item(ByVal index As Integer) As Calculation
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Calculation number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aCalculation As New Calculation
                Return aCalculation
            Else
                Return CType(List.Item(index), Calculation)
            End If
        End Get
        Set(ByVal Value As Calculation)
            List.Item(index) = Value
        End Set
    End Property
End Class
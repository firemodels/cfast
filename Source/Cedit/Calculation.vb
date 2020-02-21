Public Class Dump


    Friend Const MaximumDumps As Integer = 1000

    ' All units within the class are assumed to be consistent and typically SI
    Private aID As String                   ' id used a heading for output column
    Private aFileType As String             ' 'COMPARTMENTS', 'DEVICES', 'MASSES', 'VENTS', or 'WALLS'
    Private aType As String                 ' 'trigger_greater', 'trigger_lesser', 'minimum', 'maximum', 'integrate', 'check_total_HRR'
    Private aCriteria As Single             ' Value used in 'trigger_...' analysis
    Private aFirstMeasurement As String     ' Name of measurement, second row in spreadsheet
    Private aFirstDevice As String            ' Name of instrument within the measurement, third row in spreadsheet
    Private aSecondMeasurement As String    ' Name of measure for second instrument, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'
    Private aSecondDevice As String           ' Name of second instrument within measurement, needed for 'trigger_...' and 'integrate'; ignored for 'maximum', 'minimum', 'check_total_hrr'
    Private aFYI As String                  ' Descriptor for additional user supplied information

    Public Sub New()
        aID = ""
        aFileType = ""
        aType = ""
        aCriteria = 0
        aFirstMeasurement = ""
        aFirstDevice = ""
        aSecondMeasurement = ""
        aSecondDevice = ""
        aFYI = ""
    End Sub
    Public Sub New(ByVal ID As String, ByVal FileType As String, ByVal Type As String, ByVal Criteria As Single, ByVal FirstMeasurement As String, ByVal FirstDevice As String, ByVal SecondMeasurement As String, ByVal SecondDevice As String, fyi As String)
        aID = ID
        aFileType = FileType
        aType = Type
        aCriteria = Criteria
        aFirstMeasurement = FirstMeasurement
        aFirstDevice = FirstDevice
        aSecondMeasurement = SecondMeasurement
        aSecondDevice = SecondDevice
        aFYI = fyi
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
    Public ReadOnly Property FirstDevice As String
        Get
            Return aFirstDevice
        End Get
    End Property
    Public ReadOnly Property SecondMeasurement As String
        Get
            Return aSecondMeasurement
        End Get
    End Property
    Public ReadOnly Property SecondDevice As String
        Get
            Return aSecondDevice
        End Get
    End Property
    Public Property FYI() As String
        Get
            Return aFYI
        End Get
        Set(ByVal Value As String)
            If Value <> aFYI Then
                aFYI = Value
            End If
        End Set
    End Property
    Public Sub GetDump(ByRef ID As String, ByRef FileType As String, ByRef Type As String, ByRef Criteria As Single, ByRef FirstMeasurement As String, ByRef FirstDevice As String, ByRef SecondMeasurement As String, ByRef SecondDevice As String)
        aID = ID
        FileType = aFileType
        Type = aType
        Criteria = aCriteria
        FirstMeasurement = aFirstMeasurement
        FirstDevice = aFirstDevice
        SecondMeasurement = aSecondMeasurement
        SecondDevice = aSecondDevice
    End Sub
End Class
Public Class DumpCollection
    Inherits System.Collections.CollectionBase
    Friend ReadOnly Maximum As Integer = Dump.MaximumDumps

    Public Sub Add(ByVal aDump As Dump)
        List.Add(aDump)
    End Sub
    Default Public Property Item(ByVal index As Integer) As Dump
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Dump number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aDump As New Dump
                Return aDump
            Else
                Return CType(List.Item(index), Dump)
            End If
        End Get
        Set(ByVal Value As Dump)
            List.Item(index) = Value
        End Set
    End Property
End Class
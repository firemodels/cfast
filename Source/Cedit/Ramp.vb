Public Class Ramp

    Friend Const MaximumRamps As Integer = 5050     'Constant for Maximum number of ramps allowed in CFAST
    Friend Const TypeArea As String = "AREA"
    Friend Const TypeFrac As String = "FRACTION"
    Friend Const TypeHRR As String = "HRR"
    Friend Const TypeLength As String = "LENGTH"
    Friend Const IdxArea As Integer = 0
    Friend Const IdxFrac As Integer = 1
    Friend Const IdxHRR As Integer = 2
    Friend Const IdxLength As Integer = 3
    Friend Const xdx As Integer = 0
    Friend Const fdx As Integer = 1
    Friend Const ErrorVal As Integer = -1

    Private aName As String                         'Single word name used as ID of the ramp
    Private aRampData(2, 0) As Single               'Array for the series data the 0 row is the independent data and 1 is the dependent of f data
    Private aX(0) As Single                         'Array for the independent variable for the ramp data
    Private aF(0) As Single                         'Array for the dependent variable for the ramp data
    Private aIsT As Boolean                         'Logical flag is true if the independant variable is time t 
    Private aMaxNumRamp As Integer                  'Number of data points in arrays af and ax
    Private aType(4) As String                      'Selection list that determines units of the function
    Private aIdxType As Integer
    Private aChange As Boolean                      'Logical flag for when some vaule in object has changed
    Private HasErrors As Integer                    ' Temp variable that holds error count during error check

    Public Property Name() As String
        Get
            Return aName
        End Get
        Set(value As String)
            If aName <> value Then
                aName = value
                aChange = True
            End If
        End Set
    End Property
    Public Property IsZ() As Boolean
        Get
            Return Not aIsT
        End Get
        Set(value As Boolean)
            If aIsT <> Not value Then
                aIsT = Not value
                aChange = True
            End If
        End Set
    End Property
    Public Property IsT() As Boolean
        Get
            Return aIsT
        End Get
        Set(value As Boolean)
            If aIsT <> value Then
                aIsT = value
                aChange = True
            End If
        End Set
    End Property
    Public ReadOnly Property MaxNumRamp() As Single
        Get
            Return aX.GetUpperBound(0)
        End Get
    End Property
    Public Property DimX() As Integer
        Get
            Return aX.GetUpperBound(0)
        End Get
        Set(value As Integer)
            If value <> Me.MaxNumRamp Then
                Dim aDim As Integer = aX.GetUpperBound(0)
                ReDim Preserve aX(value)
                ReDim Preserve aF(value)
                If aDim < value Then
                    Dim i As Integer
                    For i = aDim To value
                        aX(i) = ErrorVal
                        aF(i) = ErrorVal
                    Next
                End If
                aChange = True
            End If
        End Set
    End Property
    Public Property DimF() As Integer
        Get
            Return aF.GetUpperBound(0)
        End Get
        Set(value As Integer)
            If value <> Me.MaxNumRamp Then
                Dim aDim As Integer = aF.GetUpperBound(0)
                ReDim Preserve aX(value)
                ReDim Preserve aF(value)
                If aDim < value Then
                    Dim i As Integer
                    For i = aDim To value
                        aX(i) = ErrorVal
                        aF(i) = ErrorVal
                    Next
                End If
                aChange = True
            End If
        End Set
    End Property
    Public Property X(ByVal i As Integer) As Single
        Get
            If i >= 0 And i <= aX.GetUpperBound(0) Then
                Return aX(i)
            Else
                Return ErrorVal
            End If
        End Get
        Set(value As Single)
            If i < 0 Then Return
            If i <= aX.GetUpperBound(0) Then
                aX(i) = value
            Else
                Dim aDim As Integer = aX.GetUpperBound(0)
                ReDim Preserve aX(i)
                ReDim Preserve aF(i)
                Dim j As Integer
                For j = aDim + 1 To i
                    aX(j) = ErrorVal
                    aF(j) = ErrorVal
                Next
                aX(i) = value
            End If
            aChange = True
        End Set
    End Property
    Public Property F(ByVal i As Integer) As Single
        Get
            If i >= 0 And i <= aX.GetUpperBound(0) Then
                Return aF(i)
            Else
                Return ErrorVal
            End If
        End Get
        Set(value As Single)
            If i < 0 Then Return
            If i <= aF.GetUpperBound(0) Then
                aF(i) = value
            Else
                Dim aDim As Integer = aF.GetUpperBound(0)
                ReDim Preserve aX(i)
                ReDim Preserve aF(i)
                Dim j As Integer
                For j = aDim + 1 To i
                    aX(j) = ErrorVal
                    aF(j) = ErrorVal
                Next
                aF(i) = value
            End If
            aChange = True
        End Set
    End Property
    Public Property RampData(ByVal i As Integer, ByVal j As Integer) As Single
        Get
            If i = xdx Then
                Return X(j)
            ElseIf i = fdx Then
                Return F(j)
            Else
                Return ErrorVal
            End If
        End Get
        Set(value As Single)
            If i = xdx Then
                X(j) = value
            ElseIf i = fdx Then
                F(j) = value
            Else

            End If
        End Set
    End Property
    Public Property Type() As String
        Get
            Return aType(aIdxType)
        End Get
        Set(value As String)
            If aType(aIdxType) <> value Then
                Dim i As Integer
                For i = 0 To 2
                    If aType(i) = value Then
                        aIdxType = i
                        Exit For
                    End If
                Next
                aChange = True
            End If
        End Set
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            myUnits.SI = True
            HasErrors = 0

            If aName = "" Then
                myErrors.Add("Ramp defined with no name", ErrorMessages.TypeFatal)
                HasErrors += 1
            End If
            If aMaxNumRamp = 0 Then
                myErrors.Add("Ramp " + aName + " does not have a ramp defined", ErrorMessages.TypeFatal)
                HasErrors += 1
            End If
            If aIdxType < 0 Then
                myErrors.Add("Ramp " + aName + "does not have a type defined", ErrorMessages.TypeFatal)
                HasErrors += 1
            End If

            myUnits.SI = False
            Return HasErrors
        End Get
    End Property
    Public Sub New()
        aName = ""
        aIsT = False
        aMaxNumRamp = 0
        aIdxType = 1
        aType(IdxArea) = TypeArea
        aType(IdxFrac) = TypeFrac
        aType(IdxHRR) = TypeHRR
        aType(IdxLength) = TypeLength
        aChange = False
    End Sub
    Public Sub New(ByVal Name As String, ByVal Type As String, ByVal x() As Single, ByVal f() As Single, ByVal IsT As Boolean)
        Me.New
        Me.Name = Name
        Me.IsT = IsT
        Me.SetRampData(x, f)
        Me.Type = Type
        aChange = True
    End Sub
    Public Sub SetRampData(ByVal aX() As Single, ByVal aF() As Single)
        Dim i As Integer = 0

        DimX = Math.Min(aX.GetUpperBound(0), aF.GetUpperBound(0))
        For i = 0 To MaxNumRamp
            X(i) = aX(i)
            F(i) = aF(i)
        Next
        aChange = True
    End Sub
    Public Sub SetRampData(ByVal aRampData(,) As Single)
        Dim i As Integer
        If aRampData.GetUpperBound(0) = 2 Then
            aMaxNumRamp = aRampData.GetUpperBound(1)
            ReDim aRampData(2, aMaxNumRamp)
            For i = 0 To aMaxNumRamp
                X(i) = aRampData(Ramp.xdx, i)
                F(i) = aRampData(Ramp.fdx, i)
            Next
        End If
    End Sub
    Public Sub GetRampData(ByRef aRampData(,) As Single)
        Dim i As Integer
        ReDim aRampData(2, MaxNumRamp)
        For i = 0 To aMaxNumRamp
            aRampData(Ramp.xdx, i) = X(i)
            aRampData(Ramp.fdx, i) = F(i)
        Next
    End Sub
    Public Shared Function ValidRamp(ByVal name As String, ByVal type As String, ByVal x() As Single, ByVal f() As Single) As Boolean
        Dim valid As Boolean
        Dim i As Integer

        valid = False
        If type.Trim = TypeArea Then
            valid = True
        ElseIf type.Trim = TypeFrac Then
            valid = True
        ElseIf type.Trim = TypeHRR Then
            valid = True
        ElseIf type.Trim = TypeLength Then
            valid = True
        Else
            valid = False
        End If
        If name = "" Then
            valid = False
        ElseIf x.GetUpperBound(0) <> f.GetUpperBound(0) Then
            valid = False
        Else
            If f(0) < 0 Then valid = False
            If x(0) < 0 Then valid = False
            For i = 1 To x.GetUpperBound(0) - 1
                If x(i - 1) >= x(i) Then valid = False
                If f(i) < 0 Then valid = False
            Next
        End If
        Return valid

    End Function
    Public Function IsEqual(ByVal ramp As Ramp) As Boolean
        Dim equal As Boolean
        Dim i, j As Integer

        equal = True
        If Me.Name <> ramp.Name Then
            equal = False
        ElseIf Me.Type Is ramp.Type Then
            equal = False
        ElseIf Me.MaxNumRamp <> ramp.MaxNumRamp Then
            equal = False
        Else
            For i = 0 To 1
                For j = 0 To Me.MaxNumRamp - 1
                    If Me.RampData(i, j) <> ramp.RampData(i, j) Then equal = False
                Next
            Next
        End If
        Return equal
    End Function
    Public Function IsEqual(ByVal name As String, ByVal type As String, ByVal x() As Single, ByVal f() As Single, ByVal isT As Boolean) As Boolean
        Dim equal As Boolean
        Dim i As Integer

        equal = True
        If aName <> name Then
            equal = False
        ElseIf aType Is type Then
            equal = False
        ElseIf x.GetUpperBound(0) <> f.GetUpperBound(0) Then
            equal = False
        ElseIf aMaxNumRamp <> x.GetUpperBound(0) Then
            equal = False
        Else
            For i = 0 To aMaxNumRamp
                If aRampData(Ramp.xdx, i) <> x(i) Then equal = False
                If aRampData(Ramp.fdx, i) <> f(i) Then equal = False
            Next
        End If
        Return equal
    End Function
End Class
Public Class RampCollection
    Inherits System.Collections.CollectionBase
    Private i As Integer, j As Integer
    Private HasErrors As Integer
    Public DoChange As Boolean = True
    Public Sub Add(ByVal aRamp As Ramp)
        List.Add(aRamp)
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromRamp As New Ramp, ToRamp As New Ramp
        FromRamp = CType(List.Item(indexFrom), Ramp)
        RampCopy(FromRamp, ToRamp)
        List.Item(indexTo) = ToRamp
    End Sub
    Public Sub Swap(ByVal index1 As Integer, ByVal index2 As Integer)
        If index1 >= 0 And index1 < Count And index2 >= 0 And index2 < Count Then
            Dim temp As New Ramp
            temp = CType(List.Item(index2), Ramp)
            List.Item(index2) = List.Item(index1)
            List.Item(index1) = temp
        Else
            System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). In RampCollection.Swap either " + index1.ToString + " or " + index2.ToString + " or both are not valid ramp numbers")
        End If
    End Sub
    Public Sub Remove(ByVal index As Integer)
        ' make sure that the Compartment number is valid
        If index > Count - 1 Or index < 0 Then
            System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). In RampCollection.Remove Ramp number not found.")
        Else
            List.RemoveAt(index)
        End If
    End Sub
    Public ReadOnly Property GetRampIndex(ByVal Shortname As String) As Integer
        Get
            If Count > 0 Then
                For i = 0 To Count - 1
                    If Item(i).Name = Shortname Then
                        Return i
                        Exit Property
                    End If
                Next
            End If
            Return -1
        End Get
    End Property
    Public ReadOnly Property Maximum() As Integer
        Get
            If Count > 0 Then
                Return Ramp.MaximumRamps
            Else
                Return 1
            End If
        End Get
    End Property
    Default Public Property Item(ByVal index As Integer) As Ramp
        Get
            If index >= 0 And index < Count Then
                Return CType(List.Item(index), Ramp)
            Else
                System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). Ramp number not found.")
                ' These are just to eliminate a compile warning.  If we get here, we're in trouble anyway
                Dim aRamp As New Ramp
                Return aRamp
            End If
        End Get
        Set(ByVal Value As Ramp)
            List.Item(index) = Value
        End Set
    End Property

End Class
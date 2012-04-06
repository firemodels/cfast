Public Class Matrix
    Protected matVal(,) As Double
    Protected matTrans As Boolean = False
    Protected maxRow As Integer = 0
    Protected maxCol As Integer = 0

    Default Public Property Val(ByVal i As Integer, ByVal j As Integer) As Double
        Get
            If Me.isTransposed Then
                Return Me.matVal(j - 1, i - 1)
            Else
                Return Me.matVal(i - 1, j - 1)
            End If
        End Get
        Set(ByVal value As Double)
            If Me.isTransposed Then
                Me.matVal(j - 1, i - 1) = value
            Else
                Me.matVal(i - 1, j - 1) = value
            End If
            Return
        End Set
    End Property

    Public ReadOnly Property numCol() As Integer
        Get
            If Me.isTransposed Then
                Return Me.maxRow + 1
            Else
                Return Me.maxCol + 1
            End If
        End Get
    End Property

    Public ReadOnly Property numRow() As Integer
        Get
            If Me.isTransposed Then
                Return Me.maxCol + 1
            Else
                Return Me.maxRow + 1
            End If
        End Get
    End Property

    Sub New(ByVal i As Integer, ByVal j As Integer)
        Me.setDim(i, j)
    End Sub

    Sub New()
        Me.New(1, 1)
    End Sub

    Sub setDim(ByVal i As Integer, ByVal j As Integer)
        Me.maxRow = i - 1
        Me.maxCol = j - 1
        ReDim Me.matVal(Me.maxRow, Me.maxCol)
        Me.matTrans = False
    End Sub

    Protected Function sumOf(ByVal m1 As Matrix) As Matrix
        Dim i, j As Integer
        Dim m2 As Matrix

        If Me.canSum(m1) Then
            m2 = New Matrix(Me.numRow, Me.numCol)
            For i = 1 To Me.numRow
                For j = 1 To Me.numCol
                    m2(i, j) = Me(i, j) + m1(i, j)
                Next
            Next
        Else
            m2 = New Matrix
            m2(1, 1) = -1
        End If
        Return m2
    End Function

    Protected Function diffOf(ByVal m1 As Matrix) As Matrix
        Dim i, j As Integer
        Dim m2 As Matrix

        If Me.canSum(m1) Then
            m2 = New Matrix(Me.numRow, Me.numCol)
            For i = 1 To Me.numRow
                For j = 1 To Me.numCol
                    m2(i, j) = Me(i, j) - m1(i, j)
                Next
            Next
        Else
            m2 = New Matrix
            m2(1, 1) = -1
        End If
        Return m2
    End Function

    Public Sub transpose()
        Me.matTrans = Not Me.matTrans
    End Sub

    Public Function isTransposed() As Boolean
        Return Me.matTrans
    End Function

    Protected Function timesScalor(ByVal value As Double) As Matrix
        Dim i, j As Integer
        Dim m1 As Matrix

        m1 = New Matrix(Me.numRow, Me.numCol)
        For i = 1 To Me.numRow
            For j = 1 To Me.numCol
                m1(i, j) = value * Me(i, j)
            Next
        Next
        Return m1
    End Function

    Protected Function prodOf(ByRef m1 As Matrix) As Matrix
        Dim i, j, k As Integer
        Dim m2 As Matrix

        If Me.canTimes(m1) Then
            m2 = New Matrix(Me.numRow, m1.numCol)
            For i = 1 To Me.numRow
                For j = 1 To m1.numCol
                    m2(i, j) = 0
                    For k = 1 To Me.numCol
                        m2(i, j) = m2(i, j) + Me(i, k) * m1(k, j)
                    Next
                Next
            Next
        Else
            m2 = New Matrix
            m2(1, 1) = -1
        End If
        Return m2

    End Function

    Public Overridable Function clone() As Matrix
        Dim m1 As Matrix
        Dim i, j As Integer

        m1 = New Matrix(Me.numRow, Me.numCol)
        For i = 1 To Me.numRow
            For j = 1 To Me.numCol
                m1(i, j) = Me(i, j)
            Next
        Next
        Return m1
    End Function

    Public Function canSum(ByVal m1 As Matrix) As Boolean
        If Me.numCol = m1.numCol And Me.numRow = m1.numRow Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function canTimes(ByVal m1 As Matrix) As Boolean
        If Me.numCol = m1.numRow Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Operator +(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        Return m1.sumOf(m2)
    End Operator

    Public Shared Operator -(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        Return m1.diffOf(m2)
    End Operator

    Public Shared Operator *(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        Return m1.prodOf(m2)
    End Operator

    Public Shared Operator *(ByVal a As Double, ByVal m1 As Matrix) As Matrix
        Return m1.timesScalor(a)
    End Operator

    Public Shared Operator *(ByVal m1 As Matrix, ByVal a As Double) As Matrix
        Return m1.timesScalor(a)
    End Operator

End Class

Public Class Vector
    Inherits Matrix

    Public ReadOnly Property dimension() As Integer
        Get
            If Not Me.isTransposed Then
                Return Me.numRow
            Else
                Return Me.numCol
            End If
        End Get
    End Property

    Sub New(ByVal i As Integer, ByVal x As Double)
        Me.New(i)
        Dim j As Integer
        For j = 1 To i
            Me(j) = x
        Next
    End Sub

    Sub New(ByVal i As Integer)
        MyBase.New(i, 1)
    End Sub

    Sub New()
        MyBase.New(1, 1)
    End Sub

    Default Public Shadows Property Val(ByVal ipos As Integer) As Double
        Get
            If Me.dimension >= ipos And ipos > 0 Then
                If Me.isTransposed Then
                    Return MyBase.Val(1, ipos)
                Else
                    Return MyBase.Val(ipos, 1)
                End If
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If Me.dimension >= ipos Then
                If Me.isTransposed Then
                    MyBase.Val(1, ipos) = value
                Else
                    MyBase.Val(ipos, 1) = value
                End If
            End If
        End Set
    End Property

    Shadows Sub setDim(ByVal i As Integer)
        MyBase.setDim(i, 1)
    End Sub

    Public Shadows Function clone() As Vector
        Dim m1 As Vector
        Dim i As Integer

        m1 = New Vector(Me.numRow)
        For i = 1 To Me.numRow
            m1(i) = Me(i)
        Next
        Return m1
    End Function

    Public Overrides Function tostring() As String
        Dim i As Integer
        Dim s As String = ""

        For i = 1 To Me.dimension
            s = s & Me(i) & ","
        Next
        Return s
    End Function


    Public Function canDot(ByVal v1 As Vector) As Boolean
        If v1.dimension = Me.dimension Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function dot(ByVal v1 As Vector, ByVal v2 As Vector) As Double
        Dim i As Integer
        Dim r As Double

        r = 0
        If v1.dimension = v2.dimension Then
            For i = 1 To v1.dimension
                r = r + v1(i) * v2(i)
            Next
        End If
        Return r
    End Function

    Public Shared Function elProd(ByVal v1 As Vector, ByVal v2 As Vector) As Vector
        Dim v3 As Vector
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            v3 = New Vector(v1.dimension)
            For i = 1 To v1.dimension
                v3(i) = v1(i) * v2(i)
            Next
        Else
            v3 = New Vector(1)
            v3(1) = -1
        End If
        Return v3.clone
    End Function

    Public Shared Function ElDiv(ByVal v1 As Vector, ByVal v2 As Vector) As Vector
        Dim v3 As Vector
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            v3 = New Vector(v1.dimension)
            For i = 1 To v1.dimension
                v3(i) = v1(i) / v2(i)
            Next
        Else
            v3 = New Vector(1)
            v3(1) = -1
        End If
        Return v3.clone
    End Function

    Public Shared Function NormInf(ByVal v1 As Vector) As Double
        Dim norm As Double = 0
        Dim i As Integer

        For i = 1 To v1.dimension
            norm = Math.Max(norm, Math.Abs(v1(i)))
        Next
        Return norm
    End Function

    Public Shared Function abs(ByVal v1 As Vector) As Vector
        Dim i As Integer
        Dim v2 As Vector

        v2 = New Vector(v1.dimension)
        For i = 1 To v1.dimension
            v2(i) = Math.Abs(v1(i))
        Next
        Return v2
    End Function

    Protected Shadows Function sumOf(ByVal v1 As Vector) As Vector
        Dim i As Integer
        Dim v2 As Vector

        If Me.canSum(v1) Then
            v2 = New Vector(Me.numRow)
            For i = 1 To Me.numRow
                v2(i) = Me(i) + v1(i)
            Next
        Else
            v2 = New Vector
            v2(1) = -1
        End If
        Return v2
    End Function

    Protected Sub setEqualTo(ByVal v1 As Vector)
        Dim i As Integer

        If Me.dimension = v1.dimension Then
            For i = 1 To v1.dimension
                Me(i) = v1(i)
            Next
        End If

    End Sub

    Protected Shadows Function diffOf(ByVal v1 As Vector) As Vector
        Dim i As Integer
        Dim v2 As Vector

        If Me.canSum(v1) Then
            v2 = New Vector(Me.numRow)
            For i = 1 To Me.numRow
                v2(i) = Me(i) - v1(i)
            Next
        Else
            v2 = New Vector
            v2(1) = -1
        End If
        Return v2
    End Function

    Protected Shadows Function timesScalor(ByVal value As Double) As Vector
        Dim i As Integer
        Dim v1 As Vector

        v1 = New Vector(Me.numRow)
        For i = 1 To Me.numRow
            v1(i) = value * Me(i)
        Next
        Return v1
    End Function

    Public Shared Function LessThen(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            For i = 1 To v1.dimension
                If v1(i) > v2(i) Then
                    Return False
                End If
            Next
            Return True
        End If
        Return False
    End Function

    Public Shared Function GreaterThen(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            For i = 1 To v1.dimension
                If v1(i) < v2(i) Then
                    Return False
                End If
            Next
            Return True
        End If
        Return False
    End Function

    Public Shared Function EqualTo(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            For i = 1 To v1.dimension
                If v1(i) <> v2(i) Then
                    Return False
                End If
            Next
            Return True
        End If
        Return False
    End Function

    Public Shared Function NotEqualTo(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Dim i As Integer

        If v1.dimension = v2.dimension Then
            For i = 1 To v1.dimension
                If v1(i) <> v2(i) Then
                    Return True
                End If
            Next
            Return False
        End If
        Return True
    End Function

    Public Overloads Shared Operator +(ByVal v1 As Vector, ByVal v2 As Vector) As Vector
        Return v1.sumOf(v2)
    End Operator

    Public Overloads Shared Operator -(ByVal v1 As Vector, ByVal v2 As Vector) As Vector
        Return v1.diffOf(v2)
    End Operator

    Public Overloads Shared Operator *(ByVal a As Double, ByVal v1 As Vector) As Vector
        Return v1.timesScalor(a)
    End Operator

    Public Overloads Shared Operator *(ByVal v1 As Vector, ByVal a As Double) As Vector
        Return v1.timesScalor(a)
    End Operator

    Public Shared Operator <(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Return Vector.LessThen(v1, v2)
    End Operator

    Public Shared Operator >(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Return Vector.GreaterThen(v1, v2)
    End Operator

    Public Shared Operator =(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Return Vector.EqualTo(v1, v2)
    End Operator

    Public Shared Operator <>(ByVal v1 As Vector, ByVal v2 As Vector) As Boolean
        Return Vector.NotEqualTo(v1, v2)
    End Operator
End Class

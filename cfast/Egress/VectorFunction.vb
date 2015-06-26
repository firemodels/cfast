Public Interface ScalorFunction
    Function Evaluate(ByRef Inputs As Vector) As Boolean
    Function GetEqVal(ByVal iNum As Integer) As Double
    Sub MapEqNums(ByVal iNum As Integer, ByVal xNum As Integer)
    Sub MapInputNums(ByVal iNum As Integer, ByVal xNum As Integer)
    Function IToXEqNum(ByVal iNum As Integer) As Integer
    Function IToXInputNum(ByVal iNum As Integer) As Integer
    Overloads Function ErrMsg() As String
    Overloads Function ErrMsg(ByVal errCode As Integer) As String
    ReadOnly Property NumEqs() As Integer
    ReadOnly Property NumInputs() As Integer
    ReadOnly Property ErrorCode() As Integer
    Property NameExEqNo(ByVal idx As Integer) As String
    Property NameInEqNo(ByVal idx As Integer) As String
End Interface

'Public Class SimpleVectorFunction(Of itemType As ScalorFunction)
Public Class SimpleVectorFunction
    'Protected items() As itemType
    Protected items() As ScalorFunction
    Protected funcVals As Vector
    Protected funcInputs As Vector
    Protected nmbrItem As Integer
    Protected nmbrEq As Integer
    Protected nmbrInput As Integer
    Protected nmbrOutput As Integer
    Protected funSet As Boolean
    Protected errCode As Integer
    Protected errItem As Integer

    Public ReadOnly Property numEqs() As Integer
        Get
            Return Me.nmbrEq
        End Get
    End Property

    Public ReadOnly Property numItems() As Integer
        Get
            Return Me.nmbrItem + 1
        End Get
    End Property

    Public ReadOnly Property numInputs() As Integer
        Get
            Return Me.nmbrInput
        End Get
    End Property

    Public ReadOnly Property numOutputs() As Integer
        Get
            Return Me.nmbrOutput
        End Get
    End Property

    Public ReadOnly Property isFuncSet() As Boolean
        Get
            Return Me.funSet
        End Get
    End Property

    Public ReadOnly Property getLastEval() As Vector
        Get
            Return Me.funcVals.clone
        End Get
    End Property

    Public ReadOnly Property NameListCSV() As String
        Get
            Dim s As String = ""
            Dim i As Integer
            Dim j As Integer
            Dim k As Integer

            For i = 1 To Me.numEqs
                For j = 0 To Me.numItems - 1
                    For k = 1 To Me.items(j).NumEqs
                        If i = Me.items(j).iToXEqNum(k) Then
                            s = s + Me.items(j).nameInEqNo(k) + ","
                        End If
                    Next
                Next
            Next
            Return s
        End Get
    End Property

    Public ReadOnly Property getVal(ByVal ieq As Integer) As Double
        Get
            If ieq <= Me.funcVals.dimension And ieq >= 1 Then
                Return Me.funcVals.Val(ieq)
            Else
                Return 0
            End If
        End Get
    End Property

    Public ReadOnly Property ResultVector() As Vector
        Get
            Return Me.funcVals.clone
        End Get
    End Property

    Public ReadOnly Property InputMap() As Vector
        Get
            Dim v As Vector
            Dim i, j, k As Integer

            v = New Vector(Me.nmbrInput)
            k = 1
            For i = 0 To Me.nmbrItem - 1
                For j = 1 To Me.items(i).NumInputs
                    v(k) = Me.items(i).IToXInputNum(j)
                    k = k + 1
                Next
            Next
            Return v.clone
        End Get
    End Property

    Public Sub New()
        MyBase.New()
        Me.funSet = False
        Me.funcVals = New Vector(1)
    End Sub

    Public Sub setFunction(ByRef itm() As ScalorFunction)
        Dim i As Integer

        Me.nmbrItem = itm.GetUpperBound(0)
        Me.items = itm
        Me.nmbrEq = 0
        Me.nmbrInput = 0
        For i = 0 To Me.nmbrItem
            Me.nmbrEq = Me.nmbrEq + Me.items(i).NumEqs
            Me.nmbrInput = Me.nmbrInput + Me.items(i).NumInputs
        Next
        Me.nmbrOutput = Me.nmbrEq
        Me.funSet = True
    End Sub

    Public Function Evaluate(ByRef inputs As Vector) As Boolean
        Dim i, j As Integer

        If Me.isFuncSet Then
            Me.funcVals = New Vector(Me.nmbrOutput)
            For i = 0 To Me.numItems - 1
                If Not Me.items(i).Evaluate(inputs) Then
                    Me.errCode = Me.items(i).ErrorCode
                    Me.errItem = i
                    Return False
                End If
                For j = 1 To Me.items(i).NumEqs
                    Me.funcVals(Me.items(i).IToXEqNum(j)) = Me.items(i).GetEqVal(j)
                Next
            Next
        End If
        Return True
    End Function

    Public Overloads Function ErrMsg() As String
        Return "Error in scalor function " + Me.items(Me.errItem).errmsg
    End Function

    Public Overloads Function ErrMsg(ByVal ierr As Integer) As String
        Return "Unknown error"
    End Function

End Class

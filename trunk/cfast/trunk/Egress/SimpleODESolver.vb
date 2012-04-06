Public Class SimpleODESolver
    Protected tolSet As Boolean
    Protected maskSet As Boolean
    Protected dfdtSet As Boolean
    Protected signsSet As Boolean
    Protected rangeSet As Boolean
    Protected probSet As Boolean
    Protected time As Boolean
    Protected tindex As Integer
    Protected cont As Boolean
    Protected useDt As Boolean
    Protected dtidx As Integer
    Protected dfdt As SimpleVectorFunction
    Protected tOld As Double
    Protected dt As Double
    Protected dtLast As Double
    Protected errCode As Integer
    Protected mask As Vector
    Protected negMask As Vector
    Protected faTol As Vector
    Protected dfdtaTol As Vector
    Protected frTol As Vector
    Protected dfdtrTol As Vector
    Protected state As Vector
    Protected signs As Vector
    Protected range As Matrix
    Protected eqmaprange As Vector
    Protected maxCount As Integer
    Protected maxResize As Integer
    Protected setIncrease As Double = 4.0 / 3.0
    Protected setDecrease As Double = 2.0 / 3.0
    Protected fIncrease As Double
    ' for diagnositic print from solver
    Protected dbFilename As String
    Protected dbEndLine As String
    Protected dbStartTime As Double = -1.0
    Protected dbPrint As Boolean = False
    Protected Enum MaskVals
        NotODE
        ODE
    End Enum
    Protected Enum SignsVals
        Neg = -1
        Any
        Pos
    End Enum
    Protected Enum RangeVals
        NoEq = 0
        MinEq = 1
        MaxEq
        MinVal
        MaxVal
        MinCon
        MaxCon
        MinCoef
        MaxCoef
        RangeCols = MaxCoef
    End Enum
    Protected Enum RangeFlgVals
        NoEq
        NoConEq = NoEq
        MinEqVal
        MaxEqVal
    End Enum

    Public ReadOnly Property currentState() As Vector
        Get
            Return state.clone
        End Get
    End Property
    Public ReadOnly Property currentT() As Double
        Get
            Return tOld
        End Get
    End Property
    Public ReadOnly Property currentDT() As Double
        Get
            Return dt
        End Get
    End Property
    Public ReadOnly Property LastDT() As Double
        Get
            Return Me.dtlast
        End Get
    End Property
    Public ReadOnly Property isMaskSet() As Boolean
        Get
            Return maskSet
        End Get
    End Property
    Public ReadOnly Property isTolSet() As Boolean
        Get
            Return tolSet
        End Get
    End Property
    Public ReadOnly Property isDfDTset() As Boolean
        Get
            Return dfdtSet
        End Get
    End Property
    Public ReadOnly Property isProblemSet() As Boolean
        Get
            Return probSet
        End Get
    End Property
    Public ReadOnly Property errorCode() As Integer
        Get
            Return errCode
        End Get
    End Property
    Public Shared ReadOnly Property ODEMask() As Integer
        Get
            Return MaskVals.ODE
        End Get
    End Property
    Public Shared ReadOnly Property NonODEMask() As Integer
        Get
            Return MaskVals.NotODE
        End Get
    End Property
    Public Shared ReadOnly Property NonPos() As Integer
        Get
            Return SignsVals.Neg
        End Get
    End Property
    Public Shared ReadOnly Property AnySign() As Integer
        Get
            Return SignsVals.Any
        End Get
    End Property
    Public Shared ReadOnly Property NonNeg() As Integer
        Get
            Return SignsVals.Pos
        End Get
    End Property
    Public Shared ReadOnly Property NoConnectionEq() As Integer
        Get
            Return RangeFlgVals.NoConEq
        End Get
    End Property
    Public Shared ReadOnly Property NoMinBndry() As Integer
        Get
            Return RangeFlgVals.NoEq
        End Get
    End Property
    Public Shared ReadOnly Property MinBndry() As Integer
        Get
            Return RangeFlgVals.MinEqVal
        End Get
    End Property
    Public Shared ReadOnly Property NoMaxBndry() As Integer
        Get
            Return RangeFlgVals.NoEq
        End Get
    End Property
    Public Shared ReadOnly Property MaxBndry() As Integer
        Get
            Return RangeFlgVals.MaxEqVal
        End Get
    End Property
    Public Overridable Property EqMinValue(ByVal ieqnum As Integer) As Double
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MinVal)
            End If
        End Get
        Set(ByVal value As Double)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MinEq) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MinVal) = value
            End If
        End Set
    End Property
    Public Overridable Property EqMaxValue(ByVal ieqnum As Integer) As Double
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxVal)
            End If
        End Get
        Set(ByVal value As Double)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxEq) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxVal) = value
            End If
        End Set
    End Property
    Public Overridable Property EqMinConnection(ByVal ieqnum As Integer) As Integer
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MinCon)
            End If
        End Get
        Set(ByVal value As Integer)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MinCon) = value
            End If
        End Set
    End Property
    Public Overridable Property EqMaxConnection(ByVal ieqnum As Integer) As Integer
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon)
            End If
        End Get
        Set(ByVal value As Integer)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon) = value
            End If
        End Set
    End Property
    Public Overridable Property EqMinConnCoef(ByVal ieqnum As Integer) As Integer
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MinCoef)
            End If
        End Get
        Set(ByVal value As Integer)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MinCon) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MinCoef) = value
            End If
        End Set
    End Property
    Public Overridable Property EqMaxConnCoef(ByVal ieqnum As Integer) As Integer
        Get
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return RangeFlgVals.NoEq
            If Me.eqmaprange(ieqnum) = RangeFlgVals.NoEq Then
                Return RangeFlgVals.NoEq
            Else
                Return Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon)
            End If
        End Get
        Set(ByVal value As Integer)
            If ieqnum <= 0 Or ieqnum > Me.eqmaprange.dimension Then Return
            If Me.eqmaprange(ieqnum) <> RangeFlgVals.NoEq Then
                If Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCon) = RangeFlgVals.NoEq Then Return
                Me.range(Me.eqmaprange(ieqnum), RangeVals.MaxCoef) = value
            End If
        End Set
    End Property


    Sub New()
        tolSet = False
        maskSet = False
        dfdtSet = False
        Me.signsSet = False
        probSet = False
        cont = False
        errCode = 0
        maxResize = 10
        maxCount = 10 * maxResize
        Me.fIncrease = Me.setIncrease
        Me.dbPrint = False
        Me.dbStartTime = -1.0
    End Sub

    Sub reset()
        tolSet = False
        maskSet = False
        dfdtSet = False
        Me.signsSet = False
        probSet = False
        cont = False
        errCode = 0
        maxResize = 10
        maxCount = 4 * maxResize
        Me.dbPrint = False
        Me.dbStartTime = -1.0
    End Sub

    Sub restart()
        Me.cont = False
        Me.errCode = 0
    End Sub

    Public Sub PassDt(ByVal idx As Integer)
        Me.useDt = True
        Me.dtidx = idx
    End Sub

    Public Sub DoNotPassDt()
        Me.useDt = False
    End Sub
    Public Overridable Function SetDiagnosticPrint(ByRef filename As String, ByRef headers() As String, ByRef n As Integer, _
                                        ByRef endln As String) As Boolean

        Return SetDiagnosticPrint(filename, headers, n, endln, -1.0)

    End Function

    Public Overridable Function SetDiagnosticPrint(ByRef filename As String, ByRef headers() As String, ByRef n As Integer, _
                                        ByRef endln As String, ByRef start As Double) As Boolean
        Dim s As String

        Me.dbFilename = filename
        Me.dbEndLine = endln
        Me.dbStartTime = start
        For i As Integer = 0 To n - 2
            s = " , , ," + headers(i) + Me.dbEndLine
            My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        Next
        s = "Time, dt," + headers(n - 1) + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        Me.dbPrint = True
        Return True

    End Function

    Sub StopDiagnosticPrint()

        Me.dbPrint = False
        Me.dbStartTime = -1.0

    End Sub

    Private Sub Solver1stDBPrint(ByRef DFRWD As Vector)
        Dim s As String

        s = Me.tOld.ToString + ",,," + Me.state.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + DFRWD.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)

    End Sub

    Private Sub SolverDBPrint(ByRef DT As Double, ByRef CNT As Integer, ByRef S1 As Vector, ByRef S2 As Vector, ByRef DS1 As Vector, ByRef DS2 As Vector)
        Dim s As String

        s = " ," + DT.ToString + "," + CNT.ToString + "," + S1.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + S2.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + (S1 - S2).tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + (Vector.elProd(Me.frTol, S2) + Me.faTol).tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + DS1.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)
        s = " ,,," + DS2.tostring + Me.dbEndLine
        My.Computer.FileSystem.WriteAllText(Me.dbFilename, s, True)

    End Sub

    Public Function settol(ByRef fatoltmp As Vector, ByRef dfatoltmp As Vector, _
              ByRef frtoltmp As Vector, ByRef dfrtoltmp As Vector) As Boolean
        If errCode = 0 And Not tolSet Then
            faTol = fatoltmp.clone
            dfdtaTol = dfatoltmp.clone
            frTol = frtoltmp.clone
            dfdtrTol = dfrtoltmp.clone
            If (faTol.dimension = 1 And faTol(1) = -1) Or (dfdtaTol.dimension = 1 And dfdtaTol(1) = -1) Then
                errCode = 1
                Return False
            ElseIf (frTol.dimension = 1 And frTol(1) = -1) Or (dfdtrTol.dimension = 1 And dfdtrTol(1) = -1) Then
                errCode = 1
                Return False
            ElseIf frTol.dimension = dfdtrTol.dimension And faTol.dimension = dfdtaTol.dimension Then
                tolSet = True
                Return True
            Else
                errCode = 13
                Return False
            End If
        Else
            If tolSet Then
                If errCode = 0 Then errCode = 2
            End If
            Return False
        End If
    End Function

    Public Function setmask(ByRef tmpmask As Vector) As Boolean
        If Me.errCode = 0 And Not Me.maskSet Then
            Me.mask = tmpmask.clone
            Me.negMask = New Vector(Me.mask.dimension, 1.0)
            Me.negMask = Me.negMask - Me.mask
            If Me.mask.dimension = 1 And Me.mask(1) = -1 Then
                Me.errCode = 3
                Return False
            Else
                Me.maskSet = True
                Return Me.maskSet
            End If
        Else
            If Me.maskSet Then
                If Me.errCode = 0 Then Me.errCode = 4
            End If
            Return False
        End If
    End Function

    Public Function setsigns(ByRef tmpsign As Vector) As Boolean
        If Me.errCode = 0 And Not Me.signsSet Then
            Me.signs = tmpsign.clone
            If Me.signs.dimension = 1 And Me.signs(1) = -1 Then
                Me.errCode = 17
                Return False
            Else
                Me.signsSet = True
                Dim min As New Vector(Me.signs.dimension)
                Dim max As New Vector(Me.signs.dimension)
                Dim minval As New Vector(Me.signs.dimension)
                Dim maxval As New Vector(Me.signs.dimension)
                Dim minconn As New Vector(Me.signs.dimension)
                Dim maxconn As New Vector(Me.signs.dimension)
                Dim mincoef As New Vector(Me.signs.dimension)
                Dim maxcoef As New Vector(Me.signs.dimension)
                For idx As Integer = 1 To Me.signs.dimension
                    If Me.signs(idx) = SignsVals.Pos Then
                        min(idx) = RangeFlgVals.MinEqVal
                        minval(idx) = 0.0
                        max(idx) = RangeFlgVals.NoEq
                        minconn(idx) = RangeFlgVals.NoEq
                    ElseIf Me.signs(idx) = SignsVals.Neg Then
                        min(idx) = RangeFlgVals.NoEq
                        max(idx) = RangeFlgVals.MaxEqVal
                        maxval(idx) = 0.0
                        maxconn(idx) = RangeFlgVals.NoEq
                    Else
                        min(idx) = RangeFlgVals.NoEq
                        max(idx) = RangeFlgVals.NoEq
                    End If
                Next
                Return Me.setrange(min, max, minval, maxval, minconn, maxconn, mincoef, maxcoef)
                'Return Me.signsSet
            End If
        Else
            If Me.signsSet Then
                If Me.errCode = 0 Then Me.errCode = 18
            End If
            Return False
        End If
    End Function

    Public Function setrange(ByVal min As Vector, ByVal max As Vector, ByVal minval As Vector, ByVal maxval As Vector, _
            ByVal mincon As Vector, ByVal maxcon As Vector, ByVal mincoef As Vector, ByVal maxcoef As Vector)

        Dim icnt, idx As Integer

        If Me.errCode = 0 And Not Me.rangeSet Then
            icnt = 0
            For idx = 1 To min.dimension
                If min(idx) > RangeVals.NoEq Or max(idx) > RangeVals.NoEq Then
                    icnt = icnt + 1
                End If
            Next
            If icnt > 0 Then
                Me.range = New Matrix(icnt, RangeVals.RangeCols)
                Me.eqmaprange = New Vector(min.dimension, RangeVals.NoEq)
            Else
                Me.rangeSet = False
                Return True
            End If
            icnt = 1
            For idx = 1 To min.numRow
                If min(idx) <> RangeVals.NoEq Or max(idx) <> RangeVals.NoEq Then
                    If min(idx) > RangeVals.NoEq Then
                        Me.range(icnt, RangeVals.MinEq) = idx
                        Me.range(icnt, RangeVals.MinVal) = minval(idx)
                        If mincon(idx) > RangeVals.NoEq Then
                            Me.range(icnt, RangeVals.MinCon) = mincon(idx)
                            Me.range(icnt, RangeVals.MinCoef) = mincoef(idx)
                        Else
                            Me.range(icnt, RangeVals.MinCon) = RangeVals.NoEq
                            Me.range(icnt, RangeVals.MinCoef) = 0
                        End If
                    Else
                        Me.range(icnt, RangeVals.MinEq) = RangeVals.NoEq
                    End If
                    If max(idx) > RangeVals.NoEq Then
                        Me.range(icnt, RangeVals.MaxEq) = idx
                        Me.range(icnt, RangeVals.MaxVal) = maxval(idx)
                        If maxcon(idx) > RangeVals.NoEq Then
                            Me.range(icnt, RangeVals.MaxCon) = maxcon(idx)
                            Me.range(icnt, RangeVals.MaxCoef) = maxcoef(idx)
                        Else
                            Me.range(icnt, RangeVals.MaxCon) = RangeVals.NoEq
                            Me.range(icnt, RangeVals.MaxCoef) = 0
                        End If
                    Else
                        Me.range(icnt, RangeVals.MaxEq) = RangeVals.NoEq
                    End If
                    Me.eqmaprange(idx) = icnt
                    icnt = icnt + 1
                End If
            Next
            Me.rangeSet = True
            Return True
        Else
            If Me.rangeSet Then
                Me.errCode = 18
            End If
            Return False
        End If
        Return False

    End Function

    Public Sub ResetMax(ByVal ieq As Integer, ByVal newmax As Double)

        Me.resetRangeVal(ieq, RangeVals.MaxEq, RangeVals.MaxVal, newmax)

    End Sub

    Public Sub ResetMin(ByVal ieq As Integer, ByVal newmin As Double)

        Me.resetRangeVal(ieq, RangeVals.MinEq, RangeVals.MinVal, newmin)

    End Sub

    Private Sub resetRangeVal(ByVal ieq As Integer, ByVal eqcol As Integer, ByVal eqval As Integer, ByVal value As Double)

        If Me.rangeSet Then
            If Me.eqmaprange(ieq) > 0 And Me.eqmaprange(ieq) <= Me.range.numRow Then
                Me.range(Me.eqmaprange(ieq), eqval) = value
            End If
        End If

    End Sub

    Public Function setdfdt(ByRef tmpdfdt As SimpleVectorFunction) As Boolean
        If Me.errCode = 0 And Not Me.dfdtSet Then
            Me.dfdt = tmpdfdt
            Me.dfdtSet = True
            Return Me.dfdtSet
        Else
            If dfdtSet Then
                If Me.errCode = 0 Then Me.errCode = 5
            End If
            Return False
        End If
    End Function

    Public Function dotime(ByVal timeIndex As Integer) As Boolean
        If Me.errCode = 0 Then
            Me.time = True
            Me.tindex = timeIndex
            Return True
        Else
            Return False
        End If
    End Function

    Public Function setprob(ByVal t As Double, ByRef stateTInit As Vector) As Boolean
        If Me.errCode = 0 And Not Me.probSet Then
            Me.state = stateTInit.clone
            If Me.state.dimension = 1 And Me.state(1) = -1 Then
                Me.errCode = 6
                Return False
            Else
                If Not Me.tolSet Then
                    Me.errCode = 8
                    Return False
                End If
                If Not Me.maskSet Then
                    Me.errCode = 9
                    Return False
                End If
                If Not Me.dfdtSet Then
                    Me.errCode = 10
                    Return False
                End If
                If Me.state.dimension <> Me.dfdt.numEqs Then
                    Me.errCode = 11
                    Return False
                End If
                If Me.mask.dimension <> Me.dfdt.numEqs Then
                    Me.errCode = 12
                    Return False
                End If
                If Me.faTol.dimension <> Me.dfdt.numEqs Or Me.frTol.dimension <> Me.dfdt.numEqs Then
                    Me.errCode = 14
                    Return False
                End If
                Me.probSet = True
                Me.frTol = Vector.elProd(Me.mask, Me.frTol)
                Me.faTol = Vector.elProd(Me.mask, Me.faTol)
                Me.dfdtrTol = Vector.elProd(Me.mask, Me.dfdtrTol)
                Me.dfdtaTol = Vector.elProd(Me.mask, Me.dfdtaTol)
                Me.tOld = t
                Me.dt = 0.0
                Me.dtLast = 0.0
                Return True
            End If
        Else
            If Me.probSet Then
                If Me.errCode = 0 Then Me.errCode = 7
            End If
            Return False
        End If
    End Function

    Protected Function DoSigns(ByRef s As Vector) As Boolean
        'returns true if there is a problem with the signs of at least one term.
        Dim i As Integer

        If Me.signsSet Then
            For i = 1 To Me.signs.dimension
                If Me.signs(i) * s(i) < 0.0 Then
                    If Math.Abs(s(i)) < Me.faTol(i) * 0.9 Then
                        s(i) = 0.0
                    Else
                        Return True
                    End If
                End If
            Next
        End If
        Return False
    End Function

    Protected Function DoRange(ByRef s As Vector) As Boolean

        If Me.rangeSet Then
            For idx As Integer = 1 To Me.range.numRow
                If Me.range(idx, RangeVals.MinEq) <> RangeVals.NoEq Then
                    If s(Me.range(idx, RangeVals.MinEq)) < Me.range(idx, RangeVals.MinVal) Then
                        Dim x As Double = Me.range(idx, RangeVals.MinVal)
                        Dim y As Double = s(Me.range(idx, RangeVals.MinEq))
                        If y > x Then
                            Return False
                        End If
                        If s(Me.range(idx, RangeVals.MinEq)) > Me.range(idx, RangeVals.MinVal) - Me.faTol(Me.range(idx, RangeVals.MinEq)) Then
                            s(Me.range(idx, RangeVals.MinEq)) = Me.range(idx, RangeVals.MinVal)
                        Else
                            Return True
                        End If
                    End If
                End If
                If Me.range(idx, RangeVals.MaxEq) <> RangeVals.NoEq Then
                    If s(Me.range(idx, RangeVals.MaxEq)) > Me.range(idx, RangeVals.MaxVal) Then
                        Dim icol As Integer = Me.range(idx, RangeVals.MaxEq)
                        Dim tmpmax As Double = Me.range(idx, RangeVals.MaxVal)
                        Dim tmpfatol As Double = Me.faTol(icol)
                        Dim tmptotal As Double = tmpfatol + tmpmax
                        Dim tmps As Double = s(icol)
                        If tmps < tmptotal Then
                            s(icol) = tmpmax
                            'End If
                            'If s(Me.range(idx, RangeVals.MaxVal)) < Me.range(idx, RangeVals.MaxVal) + Me.faTol(Me.range(idx, RangeVals.MaxEq)) Then
                            's(Me.range(idx, RangeVals.MaxEq)) = Me.range(idx, RangeVals.MaxVal)
                        Else
                            Return True
                        End If
                    End If
                End If
            Next
        End If

        Return False

    End Function

    Public Function ForwardStep(ByVal tmax As Double, ByRef tnxt As Double) As Boolean
        Dim frwd As Vector
        Dim bckwd As Vector
        Dim bckold As Vector
        Dim s1, s2 As Vector
        Dim cnt1 As Integer
        Dim cnt2 As Integer
        Dim cnt3 As Integer
        Dim ldt As Double
        Dim fine As Boolean = False
        Dim negsign As Boolean = False

        If Me.errCode = 0 And Me.probSet Then
            If Me.tOld + Me.dt > tmax Then
                ldt = tmax - Me.tOld
            ElseIf Me.dt = 0 Then
                ldt = (tmax - Me.tOld) * Me.faTol(1)
            Else
                ldt = Me.dt
            End If
            If Me.time Then
                Me.state(Me.tindex) = Me.tOld + ldt
            End If
            If Me.useDt Then
                Me.state(Me.dtidx) = ldt
            End If
            frwd = New Vector(Me.dfdt.numOutputs)
            bckwd = New Vector(Me.dfdt.numOutputs)
            If Not Me.dfdt.Evaluate(Me.state) Then
                Return False
            End If
            frwd = Me.dfdt.ResultVector.clone
            If Me.dbPrint Then
                Me.Solver1stDBPrint(frwd)
            End If
            bckold = frwd.clone
            s1 = Me.state + ldt * frwd
            s1 = Vector.elProd(Me.mask, s1) + Vector.elProd(Me.negMask, frwd)
            'added 20080108
            If Not Me.dfdt.Evaluate(s1) Then
                Return False
            End If
            bckwd = Me.dfdt.ResultVector
            s1 = Me.state + ldt * 0.5 * (frwd + bckwd)
            's1 = Me.state + ldt * bckwd
            s1 = Vector.elProd(Me.mask, s1) + Vector.elProd(Me.negMask, bckwd)
            'end add
            cnt1 = 0
            cnt2 = 0
            Do While (cnt1 < Me.maxCount)
                cnt1 = cnt1 + 1
                cnt2 = cnt2 + 1
                fine = False
                Do While (Not fine)
                    cnt3 = 0
                    'Do While (Me.DoSigns(s1) Or Me.DoRange(s1))
                    Do While Me.DoRange(s1)
                        negsign = True
                        ldt = ldt * Me.setDecrease
                        Me.fIncrease = Me.setIncrease
                        If Me.time Then
                            Me.state(Me.tindex) = Me.tOld + ldt
                        End If
                        If Me.useDt Then
                            Me.state(Me.dtidx) = ldt
                        End If
                        If Me.time Or Me.useDt Then
                            If Not Me.dfdt.Evaluate(Me.state) Then
                                Me.errCode = 1
                                Return False
                            End If
                            frwd = Me.dfdt.ResultVector
                            bckold = frwd
                        End If
                        s1 = Me.state + ldt * frwd
                        s1 = Vector.elProd(Me.mask, s1) + Vector.elProd(Me.negMask, frwd)
                        'added 20090108
                        If Not Me.dfdt.Evaluate(s1) Then
                            Return False
                        End If
                        bckwd = Me.dfdt.ResultVector.clone
                        s1 = Me.state + ldt * 0.5 * (frwd + bckwd)
                        's1 = Me.state + ldt * bckwd
                        s1 = Me.state + ldt * Vector.elProd(Me.mask, bckwd)
                        'end add
                    Loop
                    If Not Me.dfdt.Evaluate(s1) Then
                        Return False
                    End If
                    bckwd = Me.dfdt.ResultVector.clone
                    'commented out 20080108
                    s2 = Me.state + ldt * 0.5 * (frwd + bckwd)
                    'end commment
                    'added 20080108
                    's2 = Me.state + ldt * bckwd
                    'end add
                    s2 = Vector.elProd(Me.mask, s2) + Vector.elProd(Me.negMask, bckwd)
                    'If Not (Me.DoSigns(s2) Or Me.DoRange(s2)) Then
                    If Not Me.DoRange(s2) Then
                        fine = True
                    ElseIf cnt3 > 10 Then
                        Return False
                    Else
                        negsign = True
                        ldt = ldt * Me.setDecrease
                        Me.fIncrease = Me.setIncrease
                        If Me.time Then
                            Me.state(Me.tindex) = Me.tOld + ldt
                        End If
                        If Me.useDt Then
                            Me.state(Me.dtidx) = ldt
                        End If
                        If Me.time Or Me.useDt Then
                            If Not Me.dfdt.Evaluate(Me.state) Then
                                Return False
                            End If
                            frwd = Me.dfdt.ResultVector.clone
                            bckold = frwd.clone
                        End If
                        s1 = Me.state + ldt * frwd
                        s1 = Vector.elProd(Me.mask, s1) + Vector.elProd(Me.negMask, frwd)
                        'added 20090108
                        If Not Me.dfdt.Evaluate(s1) Then
                            Return False
                        End If
                        bckwd = Me.dfdt.ResultVector.clone
                        s1 = Me.state + ldt * 0.5 * (frwd + bckwd)
                        's1 = Me.state + ldt * bckwd
                        s1 = Vector.elProd(Me.mask, s1) + Vector.elProd(Me.negMask, bckwd)
                        'end add
                        cnt3 = cnt3 + 1
                    End If
                Loop
                If Vector.abs(Vector.elProd(Me.mask, (s1 - s2))) < (Vector.elProd(Me.frTol, Vector.abs(s2)) + Me.faTol) Then
                    'If (ldt * Vector.abs(bckold - bckwd)) < (Vector.elProd(Me.dfdtrTol, ldt * Vector.abs(bckwd)) + Me.dfdtaTol) Then
                    Me.dtLast = ldt
                    If Me.dbPrint Then
                        Me.SolverDBPrint(ldt, cnt1, s1, s2, bckold, bckwd)
                    End If
                    If Me.time Then
                        s2(Me.tindex) = ldt
                    End If
                    Me.state = s2
                    If ldt < tmax - Me.tOld Then
                        If cnt1 = 1 And Not negsign Then
                            Me.fIncrease = Me.setIncrease * Me.fIncrease
                        End If
                        If cnt1 <= Me.maxResize / 4 And Not negsign Then
                            Me.dt = Me.fIncrease * ldt
                        Else
                            Me.dt = ldt
                        End If
                    ElseIf cnt1 > Me.maxResize Then
                        Me.dt = ldt
                    End If
                    Me.tOld = Me.tOld + ldt
                    tnxt = Me.tOld
                    Return True
                    'End If
                End If
                If Me.dbPrint And Me.dbStartTime > 0.0 And Me.tOld > Me.dbStartTime Then
                    Me.SolverDBPrint(ldt, cnt1, s1, s2, bckold, bckwd)
                End If
                If cnt1 < Me.maxCount Then
                    bckold = bckwd.clone
                    If cnt2 < Me.maxResize Then
                        s1 = s2.clone
                    Else
                        ldt = ldt * Me.setDecrease
                        Me.fIncrease = Me.setIncrease
                        If Me.time Then
                            Me.state(Me.tindex) = Me.tOld + ldt
                        End If
                        If Me.useDt Then
                            Me.state(Me.dtidx) = ldt
                        End If
                        If Me.time Or Me.useDt Then
                            If Not Me.dfdt.Evaluate(Me.state) Then
                                Return False
                            End If
                            frwd = Me.dfdt.ResultVector.clone
                        End If
                        s1 = Me.state + ldt * frwd
                        s1 = Vector.elProd(mask, s1) + Vector.elProd(negMask, frwd)
                        'added 20090108
                        If Not Me.dfdt.Evaluate(s1) Then
                            Return False
                        End If
                        bckwd = Me.dfdt.ResultVector.clone
                        s1 = Me.state + ldt * 0.5 * (frwd + bckwd)
                        's1 = Me.state + ldt * bckwd
                        s1 = Vector.elProd(mask, s1) + Vector.elProd(negMask, frwd)
                        'end add
                        cnt2 = 0
                    End If
                End If
            Loop
            Me.errCode = 15
            If Me.dbPrint Then
                Me.SolverDBPrint(ldt, cnt1, s1, s2, bckold, bckwd)
            End If
            Return False
        Else
            If Not Me.probSet Then
                If Me.errCode = 0 Then Me.errCode = 16
            End If
            Return False
        End If
    End Function

    Public ReadOnly Property NameListCSV() As String
        Get
            Return Me.dfdt.NameListCSV
        End Get
    End Property

    Public Overloads Function ErrMsg(ByVal err As Integer) As String

        Select Case err
            Case 1
                Return "Function SetTol: error in saving tolerence vectors"
            Case 2
                Return "Function SetTol: called SetTol after tolerences were already set"
            Case 3
                Return "Function SetMask: error in saving mask vector"
            Case 4
                Return "Function SetMask: called SetMask after mask was already set"
            Case 5
                Return "Function SetDfDt: called SetDfDt after derivative function was already set"
            Case 6
                Return "Function SetProb: error saving inital state vector"
            Case 7
                Return "Function SetProb: called SetProb after problem already set up"
            Case 8
                Return "Function SetProb: called SetProb before tolerences were set"
            Case 9
                Return "Function SetProb: called SetProb befor Mask was set"
            Case 10
                Return "Function SetProb: called SetProb before DfDt was set"
            Case 11
                Return "Function SetProb: dimension of State vector does not match dimension of input vector for DfDT"
            Case 12
                Return "Function SetProb: dimension of Mask vector does not match number of equations from DfDt"
            Case 13
                Return "Function SetTol: unknown error"
            Case 14
                Return "Function SetProb: dimension of tolerences doesn't match number of equations in DfDt"
            Case 15
                Return "Function ForwardStep: failure to converge max number of steps = " & maxCount & "time = " & Me.tOld & " dt = " & Me.dt
            Case 16
                Return "Function ForwardStep: Problem not set up"
            Case 17
                Return "Function ForwardStep: Problem calculating derivative " + Me.dfdt.ErrMsg
            Case Else
                Return "Error Code " & err & " not found"
        End Select
    End Function

    Public Overloads Function ErrMsg() As String
        Return ErrMsg(Me.errCode)
    End Function

End Class

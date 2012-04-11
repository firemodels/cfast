Imports System.IO
' Types of single Egress elements
Public Class EgressElement
    'Implements Matricies.ScalorFunction
    Implements ScalorFunction

    Protected boundry() As Double = {0.02, 0.15, 0.09, 0.15} 'corridors, stair walls, stairs handrails, doors
    Protected spdK As Double
    Protected MaxSpdCoef() As Double = {1.19, 0.215, 1.15}
    Protected Angle As Double
    Protected a As Double = 0.263158
    Protected numConnections As Integer
    Protected conns() As EgressElement
    Protected merge As Integer
    Protected wtype As Integer 'wtype is for effective width and goes with boundry()
    Protected w, l As Double
    Protected effw As Double
    Protected fin As Double
    Protected finmax() As Double
    Protected fout, foutmax As Double
    Protected fa As Double
    Protected pin, pw, pa, pmax As Double
    Protected dmax As Double = 3.8
    Protected dmin As Double = 0.0
    Protected da As Double
    Protected lw, la, lt, ll As Double
    Protected s As Double
    Protected eqnum(), inputnum() As Integer
    Protected eqResults() As Double
    Protected eqno, inno As Integer
    Protected elName As String
    Protected names() As String
    Protected dtIdx As Integer
    Protected tIdx As Integer
    Protected dt As Double
    Protected t As Double
    Protected useDt As Boolean = False
    Protected useT As Boolean = False
    Protected warnCode As Integer
    Protected warnMsgStrs() As String
    Protected errCode As Integer
    Protected errMsgStrs() As String
    Protected err As ErrorHandler
    Protected ready As Boolean
    Protected valid As Boolean = True
    Protected stime As Double
    ' varibles for pde solution
    Protected K(,,) As Double
    Protected wenter() As Double
    Protected strfact As Double
    Protected exitConnNum As Integer
    Protected D() As Double
    Protected sd() As Double
    Protected Dtmp(,) As Double
    Protected sdtmp(,) As Double
    Protected ncells As Integer
    Protected dx As Double
    Protected sdmax As Double
    Protected endElement As EgressElement
    Protected RKstep As Boolean = False
    Protected s_a As Double = 4.0
    Protected s_b As Double = 0.0
    Protected ptmp(2) As Double
    Protected kp(3) As Double
    'Names eout, hout, iout from Sage program
    Protected eout As Double = 0.0
    Protected hout As Double = 0.0
    Protected iout As Double = 0.0
    Protected cout As Double = 0.0

    Public Enum MergeType
        Interleve
        Ascending
        Decending
        MaxMergeTypes
    End Enum
    Public Enum Bndry
        Corridor
        StairWalls
        StairHandrails
        Doors
        Custom
        MaxBndryTypes
    End Enum
    Protected Enum EqTypes
        TranPop
        WaitPop
        FlowOut
        LeadDist
        TailDist
        MaxFlowIn
        MaxEqs
    End Enum
    Protected Enum InTypes
        TranPop
        WaitPop
        LeadDist
        TailDist
        MaxFlowOut
        FlowIn
        MaxInputs
    End Enum
    Protected Enum ErrCodeNums
        NoErrors
        InitODEErr
        StairWaySeteqninRoomSeteqnin
        StairWaySeteqninHallSeteqnin
        StairWaySeteqninStairSeteqnin
        ElementEvaluateCalculateEq
        UnspecifiedErr
        MaxErrCodeNums
    End Enum
    Protected Enum WarnCodeNums
        NoWarning
        UnknownWarning
        InValidWidth
        InValidEffWidth
        UndefinedBoundryLayer
        InValidLength
        MaxWarnCodeNums
    End Enum

    Public Overridable ReadOnly Property ErrorCode() As Integer Implements ScalorFunction.ErrorCode
        Get
            Return Me.errCode
        End Get
    End Property
    Public Overridable ReadOnly Property WarningCode() As Integer
        Get
            Return Me.warnCode
        End Get
    End Property
    Public Overridable ReadOnly Property IsReady() As Boolean
        Get
            Return Me.ready
        End Get
    End Property
    Public Overridable ReadOnly Property IsValid() As Boolean
        Get
            Return Me.valid
        End Get
    End Property
    Public Overridable ReadOnly Property NumberOfConnections() As Integer
        Get
            Return Me.numConnections
        End Get
    End Property
    Public Overridable Property width() As Double
        Get
            Return Me.w
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.w = value
            Else
                Me.warnCode = EgressElement.WarnCodeNums.UnknownWarning
                Me.WarnMsg()
            End If
        End Set
    End Property
    Public Overridable Property effWidth() As Double
        Get
            Return Me.effw
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.effw = value
                Me.wtype = EgressElement.Bndry.Custom
            Else
                Me.warnCode = EgressElement.WarnCodeNums.InValidEffWidth
                Me.WarnMsg()
            End If
        End Set
    End Property
    Public Overridable Property length() As Double
        Get
            Return Me.l
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.l = length
            Else
                Me.warnCode = EgressElement.WarnCodeNums.InValidLength
                Me.WarnMsg()
            End If
        End Set
    End Property
    Public Overridable Property totalPop() As Double
        Get
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            If value >= 0 Then
                Me.pin = value
            Else
                Me.pin = 0.0
            End If
        End Set
    End Property
    Public Overridable Property WaitPop() As Double
        Get
            Return Me.pw
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.pw = value
            End If
        End Set
    End Property
    Public Overridable ReadOnly Property EntryDensity(ByVal istep As Integer) As Double
        Get
            If istep > 0 Then
                Return Me.Dtmp(istep - 1, 0)
            Else
                Return Me.D(0)
            End If
        End Get
    End Property
    Public Overridable Property transitPop() As Double
        Get
            Return Me.pa
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.pa = value
            End If
        End Set
    End Property
    Public Overridable Property BoundryLayer() As Double
        Get
            Return (Me.width - Me.effWidth) / 2.0
        End Get
        Set(ByVal value As Double)
            If 2 * value > 0 And 2 * value < Me.width Then
                Me.effWidth = Me.width - 2 * value
                Me.wtype = Bndry.Custom
            End If
        End Set
    End Property
    Public Overridable Property BoundryType() As Integer
        Get
            Return Me.wtype
        End Get
        Set(ByVal value As Integer)
            If value >= 0 And value <= Bndry.Custom Then
                Me.wtype = value
                Me.effw = Me.w - Me.boundry(Me.wtype)
            End If
        End Set
    End Property
    Public Overridable Property incline() As Double
        Get
            Return Me.Angle
        End Get
        Set(ByVal value As Double)
            Me.Angle = value
        End Set
    End Property
    Public Overloads Property maxFlowIn() As Double
        Get
            Return Me.finmax(0)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.finmax(0) = value
            End If
        End Set
    End Property
    Public Overloads Property maxFlowIn(ByVal idx As Integer) As Double
        Get
            If idx <= 0 Or idx > Me.numConnections Then Return -1.0
            Return Me.finmax(idx - 1)
        End Get
        Set(ByVal value As Double)
            If idx > 0 And idx <= Me.numConnections Then
                If value > 0.0 Then
                    Me.finmax(idx - 1) = value
                End If
            End If
        End Set
    End Property
    Public Overridable Property dtIndex() As Integer
        Get
            Return Me.dtIdx
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then
                Me.dtIdx = value
            End If
        End Set
    End Property
    Public Overridable ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overridable ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPop)
        End Get
    End Property
    Public Overridable ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overridable ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overridable ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    Public Overridable ReadOnly Property NumEqs() As Integer Implements ScalorFunction.NumEqs
        Get
            Return Me.eqno
        End Get
    End Property
    Public Overridable ReadOnly Property NumInputs() As Integer Implements ScalorFunction.NumInputs
        Get
            Return Me.inno
        End Get
    End Property
    Public Overridable Property ElementName() As String
        Get
            Return Me.elName
        End Get
        Set(ByVal value As String)
            Me.elName = value
        End Set
    End Property
    Public Overridable Property PDEElementName() As String
        Get
            Return Me.ElementName()
        End Get
        Set(ByVal value As String)
            Me.ElementName() = value
        End Set
    End Property
    Public Overridable Property nameExEqNo(ByVal idx As Integer) As String Implements ScalorFunction.NameExEqNo
        Get
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Return Me.names(i)
                End If
            Next
            Return ""
        End Get
        Set(ByVal value As String)
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Me.names(i) = value
                End If
            Next
        End Set
    End Property
    Public Overridable Property nameInEqNo(ByVal idx As Integer) As String Implements ScalorFunction.NameInEqNo
        Get
            If idx <= Me.eqno And idx >= 1 Then
                Return Me.names(idx - 1)
            End If
            Return ""
        End Get
        Set(ByVal value As String)
            If idx <= Me.eqno And idx >= 1 Then
                Me.names(idx - 1) = value
            End If
        End Set
    End Property
    Public Overridable ReadOnly Property EgressElementNameListCSV() As String
        Get
            Dim i As Integer
            Dim s As String

            s = Me.elName
            For i = 0 To Me.NumEqs - 1
                s = s + ","
            Next
            Return s
        End Get
    End Property

    Public Overridable ReadOnly Property EgressEntranceWidth(ByVal idx As Integer) As Double
        Get
            If Me.wenter(idx) > 0.0 Then
                Return Me.wenter(idx)
            Else
                Return 0.0
            End If
        End Get
    End Property

    Public Overridable Property EgressExitNum() As Integer
        Get
            Return Me.exitConnNum
        End Get
        Set(ByVal value As Integer)
            If value <= 0 Then
                Dim idx As Integer = 0
            End If
            Me.exitConnNum = value
        End Set
    End Property

    Public Overridable Property Connection(ByVal idx As Integer) As EgressElement
        Get
            Return Me.conns(idx)
        End Get
        Set(ByVal value As EgressElement)
            Me.conns(idx) = value
        End Set
    End Property

    Protected Sub SetErrMsgStr()

        Me.errMsgStrs = New String(ErrCodeNums.MaxErrCodeNums - 1) {"No Errors Reported", _
                  "Element.InitializeODEVectors: Not Ready to assign initial values", _
                  "StairWay.seteqnin: error in Room.Seteqnin", _
                  "StairWay.Seteqnin: error in Hall.seteqnin", _
                  "StairWay.seteqnin: error in stair.seteqnin", _
                  "Element.evaluate: error returned from CalculateEq", _
                  "UnSpecified Error detected"}

    End Sub

    Sub New(ByVal numConnect As Integer, ByVal mrgType As Integer, ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin As Double, ByVal tmpangle As Double, ByVal stime As Double)

        Me.ready = False
        Me.errCode = ErrCodeNums.NoErrors
        Me.stime = stime
        Me.validPos(Me.numConnections, numConnect)
        Me.validPos(Me.l, length)
        If mrgType >= 0 And mrgType < MergeType.MaxMergeTypes Then
            Me.merge = mrgType
        Else
            Me.valid = False
            Me.merge = -1
        End If
        If Me.validPos(Me.w, wide) Then
            If widthtype >= 0 And widthtype < Bndry.MaxBndryTypes Then
                Me.wtype = widthtype
                Me.effw = Me.w - Me.boundry(Me.wtype)
                Me.effw = Me.w - 0.3
            Else
                Me.wtype = -1
                Me.effw = 0.0
                Me.valid = False
            End If
        Else
            Me.wtype = 0
            Me.effw = 0.0
        End If
        If Me.valid Then
            ReDim Me.finmax(Me.numConnections - 1)
            ' for pde
            ReDim Me.wenter(Me.numConnections - 1)
            For i As Integer = 0 To Me.numConnections - 1
                Me.validPos(Me.finmax(i), maxfin)
                Me.wenter(i) = -1.0
            Next
        End If
        If Not Me.valid Then
            Me.l = 0.0
            Me.w = 0.0
            Me.effw = 0.0
            Me.wtype = -1
            Me.eqno = 0
            Me.inno = 0
            ReDim Me.eqnum(0)
            ReDim Me.eqResults(0)
            ReDim Me.inputnum(0)
            ReDim Me.names(0)
            Me.elName = "Invalide EgressElement"
            Return
        End If
        Me.eqno = EqTypes.MaxEqs + Me.numConnections - 1
        Me.inno = InTypes.MaxInputs + Me.numConnections - 1
        ReDim Me.conns(Me.numConnections - 1)
        ReDim Me.eqnum(Me.eqno - 1)
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.names(Me.eqno - 1)
        Me.names(EqTypes.TranPop) = "Pop. in Transit"
        Me.names(EqTypes.WaitPop) = "Pop. Waiting"
        Me.names(EqTypes.FlowOut) = "Exit Flow (people/s)"
        Me.names(EqTypes.LeadDist) = "Dist from exit(m)"
        Me.names(EqTypes.TailDist) = "Dist of tail from exit(m)"
        For i As Integer = 0 To Me.numConnections - 1
            Me.names(EqTypes.MaxFlowIn + i) = "Max Flow In (people/s)"
        Next
        Me.elName = "EgressElement"
        Me.pin = 0.0
        For i As Integer = 0 To 2
            Me.ptmp(i) = Me.pin
            Me.kp(i) = 0.0
        Next
        Me.pw = 0.0
        Me.pa = 0.0
        Me.pmax = Me.l * Me.w * Me.dmax
        Me.CalSpeedK(Me.Angle)
        Me.strfact = 1.0
        'Code for pde solution

        Me.ncells = Math.Round(Me.l, 0)
        Me.sdmax = 1.19
        Me.dx = Me.l / Me.ncells
        ReDim Me.D(Me.ncells - 1)
        ReDim Me.sd(Me.ncells - 1)
        ReDim Me.Dtmp(2, Me.ncells - 1)
        ReDim Me.sdtmp(2, Me.ncells - 1)
        ReDim Me.K(2, 4, Me.ncells - 1)
        For i As Integer = 0 To Me.ncells - 1
            Me.D(i) = 0.0
            'Me.sd(i) = Me.speed(0.0)
            Me.sd(i) = Me.sdmax
            Me.Dtmp(0, i) = Me.D(i)
            Me.Dtmp(1, i) = Me.D(i)
            Me.Dtmp(2, i) = Me.D(i)
            Me.sdtmp(0, i) = Me.sd(i)
            Me.sdtmp(1, i) = Me.sd(i)
            Me.sdtmp(2, i) = Me.sd(i)
        Next
    End Sub

    Sub New(ByVal numConnect As Integer, ByVal mrgType As Integer, ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin As Double, ByVal tmpangle As Double)

        Me.New(numConnect, mrgType, length, wide, widthtype, maxfin, tmpangle, 0.0)

    End Sub

    Sub New(ByVal numConnect As Integer, ByVal mrgType As Integer, ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin() As Double, ByVal tmpangle As Double)

        Me.New(numConnect, mrgType, length, wide, widthtype, maxfin, tmpangle, 0.0)

    End Sub

    Sub New(ByVal numConnect As Integer, ByVal mrgType As Integer, ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin() As Double, ByVal tmpangle As Double, ByVal stime As Double)
        Me.new(numConnect, mrgType, length, wide, widthtype, maxfin(0), tmpangle, stime)
        If Me.valid Then
            Dim iend As Integer = maxfin.GetUpperBound(0)
            If Me.numConnections = maxfin.GetUpperBound(0) + 1 Then
                For i As Integer = 0 To Me.numConnections - 1
                    Me.validPos(Me.finmax(i), maxfin(i))
                Next
            Else
                Me.valid = False
                Me.InvalidateElement()
            End If
        End If
    End Sub

    Sub New(ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin As Double)
        Me.New(1, MergeType.Interleve, length, wide, widthtype, maxfin, 0.0, 0.0)
    End Sub

    Sub New()
        Me.New(1.0, 1.0, Bndry.Corridor, 1.0)
    End Sub
    Protected Overloads Function validPos(ByRef len As Double, ByVal value As Double) As Boolean
        If value > 0.0 And Me.valid Then
            len = value
            Return True
        Else
            len = 0.0
            Me.valid = False
            Return False
        End If
    End Function
    Protected Overloads Function validPos(ByRef idx As Integer, ByVal value As Integer) As Boolean
        If value > 0.0 And Me.valid Then
            idx = value
        Else
            idx = 0.0
            Me.valid = False
        End If

    End Function
    Protected Overridable Sub InvalidateElement()
        Me.l = 0.0
        Me.w = 0.0
        Me.effw = 0.0
        Me.wtype = -1
        Me.eqno = 0
        Me.inno = 0
        ReDim Me.eqnum(0)
        ReDim Me.eqResults(0)
        ReDim Me.inputnum(0)
        ReDim Me.names(0)
        Me.elName = "Invalide EgressElement"
        Return
    End Sub

    Public Overloads Sub Redefine(ByVal length As Double, ByVal wide As Double, ByVal widthtype As Integer, ByVal maxfin As Double, ByVal tmpangle As Double)
        Me.length = length
        Me.width = wide
        Me.BoundryType = widthtype
        For i As Integer = 0 To Me.numConnections - 1
            Me.finmax(i) = maxfin
        Next
        Me.Angle = tmpangle
    End Sub

    Public Overloads Sub Redefine(ByVal element As EgressElement)
        Me.l = element.length
        Me.w = element.width
        Me.effw = element.effWidth
        Me.wtype = element.BoundryType
        Me.Angle = element.incline
        For i As Integer = 0 To element.numConnections - 1
            Me.finmax(i) = element.maxFlowIn(i)
        Next
    End Sub

    Public Overridable Function copy() As EgressElement
        Dim tmp As New EgressElement(Me.numConnections, Me.merge, Me.l, Me.w, Me.wtype, Me.finmax, Me.Angle)
        If Me.wtype = Bndry.Custom Then
            tmp.effWidth = Me.effw
        End If
        Return tmp
    End Function

    Public Overridable Sub seteqin(ByRef eqidx As Integer)

        If Not Me.valid Then Return
        Me.mapEqNums(EqTypes.TranPop + 1, EqTypes.TranPop + eqidx + 1)
        Me.mapEqNums(EqTypes.WaitPop + 1, EqTypes.WaitPop + eqidx + 1)
        Me.mapEqNums(EqTypes.FlowOut + 1, EqTypes.FlowOut + eqidx + 1)
        Me.mapEqNums(EqTypes.LeadDist + 1, EqTypes.LeadDist + eqidx + 1)
        Me.mapEqNums(EqTypes.TailDist + 1, EqTypes.TailDist + eqidx + 1)
        For i As Integer = 1 To Me.numConnections
            Me.mapEqNums(EqTypes.MaxFlowIn + i, EqTypes.MaxFlowIn + eqidx + i)
        Next
        'using EqTypes so that an object inhereting EgressElement, for example
        'EgressMCElement can call this sub and and not have to worry about how
        'the child object changes me.eqno
        eqidx = eqidx + EqTypes.MaxEqs + Me.numConnections - 1

        Me.mapInputNums(Me.InEqNoTranPop, Me.ExEqNoTranPop)
        Me.mapInputNums(Me.InEqNoWaitPop, Me.ExEqNoWaitPop)
        Me.mapInputNums(Me.InEqNoLeadDist, Me.ExEqNoLeadDist)
        Me.mapInputNums(Me.InEqNoTailDist, Me.ExEqNoTailDist)
        Me.ready = True

    End Sub

    Public Overridable Function InitilizeODEVecters(ByRef state As Vector, _
                            ByRef mask As Vector, ByRef min As Vector, ByRef max As Vector, _
                            ByRef minval As Vector, ByRef maxval As Vector, ByRef mincon As Vector, ByRef maxcon As Vector, _
                            ByRef mincoef As Vector, ByRef maxcoef As Vector)
        'ByRef mask As Vector, ByRef signs As Vector) As Boolean

        If Not Me.ready Or Not Me.valid Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        state(Me.eqnum(EqTypes.TranPop)) = Me.transitPop
        mask(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.TotPop)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.TranPop)) = 0.0
        mincon(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.MaxBndry
        maxval(Me.eqnum(EqTypes.TranPop)) = Me.pmax
        maxcon(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.NoConnectionEq

        state(Me.eqnum(EqTypes.WaitPop)) = Me.WaitPop
        mask(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.TranPop)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.WaitPop)) = 0.0
        mincon(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.NoMaxBndry

        state(Me.eqnum(EqTypes.FlowOut)) = Me.fout
        mask(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NonODEMask
        'signs(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.AnySign
        min(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMinBndry
        max(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMaxBndry

        state(Me.eqnum(EqTypes.LeadDist)) = Me.l
        mask(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.LeadDist)) = 0.0
        mincon(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.MaxBndry
        maxval(Me.eqnum(EqTypes.LeadDist)) = Me.l
        maxcon(Me.eqnum(EqTypes.LeadDist)) = SimpleODESolver.NoConnectionEq

        state(Me.eqnum(EqTypes.TailDist)) = Me.l
        mask(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.TailDist)) = 0.0
        mincon(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.MaxBndry
        maxval(Me.eqnum(EqTypes.TailDist)) = Me.l
        maxcon(Me.eqnum(EqTypes.TailDist)) = SimpleODESolver.NoConnectionEq

        For i As Integer = 0 To Me.numConnections - 1
            state(Me.eqnum(EqTypes.MaxFlowIn + i)) = 0.0
            mask(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NonODEMask
            'signs(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.AnySign
            min(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NoMinBndry
            max(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NoMaxBndry
        Next

        Return True

    End Function

    Public Overridable Function ConnectInFlow(ByVal icon As Integer, ByRef element As EgressElement) As Boolean

        If icon <= 0 Or icon > Me.numConnections Then Return False
        If Me.InEqNoFlowIn(icon) > 0 And element.ExEqNoFlowOut > 0 Then
            Me.mapInputNums(Me.InEqNoFlowIn(icon), element.ExEqNoFlowOut)
            If element.InEqNoMaxFlowOut > 0 And Me.ExEqNoMaxFlowIn(icon) > 0 Then
                element.mapInputNums(element.InEqNoMaxFlowOut, Me.ExEqNoMaxFlowIn(icon))
                Me.conns(icon - 1) = element
                'Me.wenter(icon - 1) = (Me.finmax(icon - 1) * 4.0) / (Me.dmax * Me.sdmax)
                Me.wenter(icon - 1) = (Me.finmax(icon - 1)) / (0.37 * Me.dmax * Me.sdmax)
                element.NextElement = Me
                element.EgressExitNum = icon
                Return True
            End If
        End If
        Return False

    End Function

    Protected Overridable Function CalculateEq() As Boolean
        Me.pin = Me.pa + Me.pw
        Me.lw = Math.Min(Me.l, Me.pw / (Me.dmax * Me.effw))
        Me.la = Math.Max(0.0, Math.Min(Me.l, Me.lt) - Math.Max(Me.lw, Me.ll))
        If Me.la > 0.0 Then
            Me.da = Math.Min(Me.dmax, Me.pa / (Me.la * Me.effw))
        ElseIf Me.pa > 0 Then
            Me.da = Me.dmax
        Else
            Me.da = 0.0
        End If
        If Me.da < Me.dmax Then
            Me.s = Me.speed(Me.da)
        Else
            Me.s = Me.speed(0.0)
        End If

        If Me.ll <= 0.0 And Me.pa > 0.0 Then
            Me.fa = Me.s * Me.effw * Me.da
            'If Me.pmax - Me.pin < 1.0 And Me.fin > 0.0 Then
            'Me.fa = Math.Min(Me.fin, Me.fa)
            'ElseIf (Me.dFadDa(Me.da) < 0.0) And Me.fa <= Me.fin Then
            'Me.fa = Me.fin + 0.1
            'End If
        Else
            Me.fa = 0.0
        End If

        'Do EqTypes.TranPop and EqTypes.WaitPop
        If Me.pw = 0.0 And Me.fa < Me.fout Then
            Me.eqResults(EqTypes.TranPop) = Me.fin - Me.fout
            Me.eqResults(EqTypes.WaitPop) = 0.0
        Else
            Me.eqResults(EqTypes.TranPop) = Me.fin - Me.fa
            Me.eqResults(EqTypes.WaitPop) = Me.fa - Me.fout
        End If

        'Do EqTypes.TotPop equation
        'Me.eqResults(EqTypes.TotPop) = Me.fin - Me.fout

        'Do Eqtypes.TranPop Equation
        'Me.eqResults(EqTypes.TranPop) = Me.fin - Me.fa
        'If Me.ll <= 0.0 And Me.pa > 0.0 Then
        'If ((Me.fin - Me.fa) > Me.eqResults(EqTypes.TotPop)) And Me.pw <= 0.0 Then
        'Me.eqResults(EqTypes.TranPop) = Me.eqResults(EqTypes.TotPop)
        'Me.fa = Me.fout
        'End If
        'If Me.pin - Me.pa < -0.001 Then
        'If Me.eqResults(EqTypes.TotPop) < Me.eqResults(EqTypes.TranPop) Then
        'Me.eqResults(EqTypes.TranPop) = Me.eqResults(EqTypes.TotPop) - 0.1
        'Me.fa = Me.fin - Me.eqResults(EqTypes.TranPop)
        'End If
        'End If
        'End If

        'Do EqTypes.LeadDist Equation
        Me.eqResults(EqTypes.LeadDist) = 0.0
        If (Me.pa >= 1.0 Or (Me.pa > 0.0 And Me.fin <= 0.0)) And Me.ll > 0.0 Then
            Me.eqResults(EqTypes.LeadDist) = -Me.speed(0.0)
        ElseIf Me.pin <= 0.0 And Me.pa <= 0.0 And Me.ll < Me.l Then
            Me.eqResults(EqTypes.LeadDist) = Me.l
        End If

        'Do EqTypes.TailDist Equation
        Me.eqResults(EqTypes.TailDist) = 0.0
        If Me.pa > 0.0 And Me.fin <= 0.0 And Me.lt > Me.ll Then
            Me.eqResults(EqTypes.TailDist) = -Me.speed(Me.da)
        ElseIf Me.pa <= 0.0 And Me.lt < Me.l Then
            Me.eqResults(EqTypes.TailDist) = Me.l
        End If

        'Do EqTypes.FlowOut Equation
        'If Me.ll <= 0.0 And Me.pa > 0.0 Then
        'Me.fout = Math.Min(Me.foutmax, Math.Max(Me.s * Me.effw * Me.da, 0.0) + Me.pw)
        'If Me.pw <= 0.0 Then
        'Me.fout = Math.Min(Me.foutmax, Me.fa)
        'ElseIf Me.fin > 0.0 Then
        '    Me.fout = Math.Min(Me.foutmax, Me.fa + Me.pw)
        'Else
        'Me.fout = Me.foutmax
        'End If
        'ElseIf Me.pw > 0.0 Then
        'Me.fout = Me.foutmax
        'Else
        'Me.fout = 0.0
        'End If
        If Me.ll <= 0.0 And Me.pa > 0.0 Then
            If Me.pa < 1.0 And Me.fin > 0.0 Then
                Me.eqResults(EqTypes.FlowOut) = Math.Min(Me.fa + Me.pw, Me.foutmax)
            Else
                Me.eqResults(EqTypes.FlowOut) = Math.Min(Math.Max(Me.fa + Me.pw, 0.1), Me.foutmax)
            End If
        ElseIf Me.pw > 0.0 Then
            Me.eqResults(EqTypes.FlowOut) = Me.foutmax
        Else
            Me.eqResults(EqTypes.FlowOut) = 0.0
        End If

        'Me.eqResults(EqTypes.FlowOut) = Me.fout

        'Do EqTypes.MaxFlowIn Equation
        'If Me.pmax - Me.pin < 1.0 Then
        'If Me.pmax - Me.pin <= 0.0 Then
        'Me.eqResults(EqTypes.MaxFlowIn) = 0.0
        'ElseIf Me.pmax - Me.pin <= 0.5 Then
        'Me.eqResults(EqTypes.MaxFlowIn) = Me.fout
        'Else
        'Me.eqResults(EqTypes.MaxFlowIn) = (((Me.pmax - Me.pin) - 0.5) / 0.5) * Me.pmax + (1.0 - (Me.pmax - Me.pin)) / 0.5 * Me.fout
        'End If
        'Else
        'Me.eqResults(EqTypes.MaxFlowIn) = Me.pmax
        'End If
        If Me.pmax - Me.pin < 1.0 And Me.eqResults(EqTypes.MaxFlowIn) = 0.0 Then
            Me.eqResults(EqTypes.MaxFlowIn) = 0.0
        ElseIf Me.pmax - Me.pin < 0.5 Then
            Me.eqResults(EqTypes.MaxFlowIn) = 0.0
        ElseIf Me.pmax - Me.pin < 1.0 Then
            Me.eqResults(EqTypes.MaxFlowIn) = Me.fout
        Else
            Me.eqResults(EqTypes.MaxFlowIn) = Me.pmax
        End If

        Return True

    End Function

    Public Overridable Function Evaluate(ByRef Inputs As Vector) As Boolean Implements ScalorFunction.Evaluate
        ' equation 1 eqnum(EqTypes.TotPop) dP_in/dt         input 1 inputnum(InTypes.TotPop) Population in element P_in or pin
        ' equation 2 eqnum(EqTypes.TranPop) dP_transit/dt   input 2 inputnum(InTypes.TranPop) Population waiting    P_w or pw
        ' equation 3 eqnum(EqTypes.FlowOut) flow out        input 3 inputnum(InTypes.LeadDist) distance lead from exit
        ' equation 4 eqnum(EqTypes.LeadSpd) lead travel spd input 4 inputnum(InTypes.TailDist) distance tail from exit
        ' equation 5 eqnum(EqTypes.TailSpd) tail travel spd input 5 inputnum(InTypes.MaxFlowOut) max flow out
        ' equation 6 eqnum(EqTypes.MaxFlowIn) max flow in   input 6 inputnum(InTypes.FlowIn) flow into element     fin

        If Me.useDt Then Me.dt = Inputs(Me.dtIdx)
        Me.pa = Math.Max(0.0, Inputs(inputnum(InTypes.TranPop)))
        Me.pw = Math.Max(0.0, Inputs(inputnum(InTypes.WaitPop)))
        Me.fin = 0.0
        For i As Integer = 0 To Me.numConnections - 1
            Me.fin = Me.fin + Math.Max(0.0, Inputs(inputnum(InTypes.FlowIn + i)))
        Next
        Me.foutmax = Math.Max(0.0, Inputs(inputnum(InTypes.MaxFlowOut)))
        Me.fout = Inputs(Me.ExEqNoFlowOut)
        Me.ll = Math.Max(0.0, Inputs(inputnum(InTypes.LeadDist)))
        Me.lt = Math.Max(0.0, Inputs(inputnum(InTypes.TailDist)))
        If Not Me.CalculateEq Then
            Me.errCode = ErrCodeNums.ElementEvaluateCalculateEq
            Return False
        End If

        Dim totin As Double = Me.eqResults(EqTypes.MaxFlowIn)
        If Me.numConnections = 1 Then
            Me.eqResults(EqTypes.MaxFlowIn) = Math.Min(totin, Me.finmax(0))
        ElseIf Me.merge = MergeType.Interleve Then
            Dim totden As Double
            Dim flw(Me.numConnections - 1) As Boolean

            totden = 0.0
            For i As Integer = 0 To Me.numConnections - 1
                If Inputs(Me.conns(i).ExEqNoLeadDist) <= 0.0 Or Inputs(Me.conns(i).ExEqNoWaitPop) > 0.0 Then
                    flw(i) = True
                    totden = totden + 1.0
                    totin = Math.Min(totin, Me.finmax(i))
                Else
                    flw(i) = False
                End If
            Next
            For i As Integer = 0 To Me.numConnections - 1
                If flw(i) Then
                    Me.eqResults(EqTypes.MaxFlowIn + i) = totin / totden
                Else
                    Me.eqResults(EqTypes.MaxFlowIn + i) = 0.0
                End If
            Next
        ElseIf Me.merge = MergeType.Ascending Then
            Dim found As Boolean = False
            For i As Integer = 0 To Me.numConnections - 1
                If Not found And (Inputs(Me.conns(i).ExEqNoLeadDist) <= 0.0 Or _
                   Inputs(Me.conns(i).ExEqNoWaitPop) > 0.0) Then
                    Me.eqResults(EqTypes.MaxFlowIn + i) = Math.Min(totin, Me.finmax(i))
                    found = True
                Else
                    Me.eqResults(EqTypes.MaxFlowIn + i) = 0.0
                End If
            Next
        Else
            Dim found As Boolean = False
            For i As Integer = Me.numConnections - 1 To 0 Step -1
                If Not found And (Inputs(Me.conns(i).ExEqNoLeadDist) <= 0.0 Or _
                   Inputs(Me.conns(i).ExEqNoWaitPop) > 0.0) Then
                    Me.eqResults(EqTypes.MaxFlowIn + i) = Math.Min(totin, Me.finmax(i))
                    found = True
                Else
                    Me.eqResults(EqTypes.MaxFlowIn + i) = 0.0
                End If
            Next
        End If

        Return True

    End Function

    Public Overridable Function GetEqVal(ByVal iNum As Integer) As Double Implements ScalorFunction.GetEqVal
        If iNum > 0 And iNum <= Me.eqno Then
            Return Me.eqResults(iNum - 1)
        Else
            Return -1.0
        End If
    End Function

    Public Overridable Function iToXEqNum(ByVal iNum As Integer) As Integer Implements ScalorFunction.IToXEqNum
        Return Me.eqnum(iNum - 1)
    End Function

    Public Overridable Function iToXInputNum(ByVal iNum As Integer) As Integer Implements ScalorFunction.IToXInputNum
        Return Me.inputnum(iNum - 1)
    End Function

    Public Overridable Sub mapEqNums(ByVal iNum As Integer, ByVal xNum As Integer) Implements ScalorFunction.MapEqNums
        If iNum > 0 And iNum <= Me.eqno Then
            Me.eqnum(iNum - 1) = xNum
        End If
    End Sub

    Public Overridable Sub mapInputNums(ByVal iNum As Integer, ByVal xNum As Integer) Implements ScalorFunction.MapInputNums
        If iNum > 0 And iNum <= Me.inno Then
            Me.inputnum(iNum - 1) = xNum
        End If
    End Sub

    Protected Function CalSpeedK(ByVal angle) As Double

        Me.spdK = Me.MaxSpdCoef(0) + Me.MaxSpdCoef(1) * angle + Me.MaxSpdCoef(2) * angle ^ 2
    End Function

    Protected Function speed(ByVal density As Double) As Double

        If density < 0.54 Then
            Return Me.sdmax
        ElseIf density >= Me.dmax Then
            Return 0.0
        End If
        Return Math.Max((Me.dmax - density) / (Me.dmax - 0.54) * Me.sdmax, 0.0)
    End Function

    Protected Function dFadDa(ByVal density As Double) As Double
        If density < Me.dmin Then
            density = Me.dmin
        End If
        Return Me.spdK * (1.0 - 2 * Me.a * density)
    End Function

    Public Overridable Function MakePopInteger(ByVal state As Matricies.Vector) As Matricies.Vector
        Dim idx As Integer

        idx = Me.ExEqNoTranPop
        If idx > 0 And idx <= state.dimension Then
            state(idx) = Math.Round(state(idx))
        End If
        idx = Me.ExEqNoWaitPop
        If idx > 0 And idx <= state.dimension Then
            state(idx) = Math.Round(state(idx))
        End If
        Return state.clone
    End Function

    Public Shared Function FixPop(ByRef elements() As EgressElement, ByVal state As Matricies.Vector) As Matricies.Vector
        Dim i As Integer

        For i = 0 To elements.GetUpperBound(0)
            state = elements(i).MakePopInteger(state)
        Next
        Return state.clone
    End Function

    Public Overloads Function ErrMsg() As String Implements ScalorFunction.ErrMsg
        Return Me.ErrMsg(Me.errCode)
    End Function

    Public Overloads Function ErrMsg(ByVal ierr As Integer) As String Implements ScalorFunction.ErrMsg
        Me.SetErrMsgStr()
        Return "For Object " + Me.elName + " " + Me.errMsgStrs(ierr)
    End Function

    Public Overridable Overloads Function WarnMsg() As String
        Return Me.WarnMsg(Me.warnCode)
    End Function

    Public Overridable Overloads Function WarnMsg(ByVal iwarn As Integer) As String
        Return "unknown warning"
    End Function
    ' Functions added to implement PDE solution
    Public Overridable ReadOnly Property HeadD(ByVal istep As Integer) As Double
        Get
            If istep = 0 Then
                Return Me.D(0)
            Else
                Return Me.Dtmp(istep - 1, 0)
            End If
        End Get
    End Property

    Public Overridable ReadOnly Property TailD(ByVal istep As Integer) As Double
        Get
            If istep = 0 Then
                Return Me.D(Me.ncells - 1)
            Else
                Return Me.Dtmp(istep - 1, Me.ncells - 1)
            End If
        End Get
    End Property

    Public Overridable ReadOnly Property HeadSpd(ByVal istep As Integer) As Double
        Get
            If istep = 0 Then
                Return Me.sd(0)
            Else
                Return Me.sdtmp(istep - 1, 0)
            End If
        End Get
    End Property

    Public Overridable ReadOnly Property TailSpd(ByVal istep As Integer) As Double
        Get
            If istep = 0 Then
                Return Me.sd(Me.ncells - 1)
            Else
                Return Me.sdtmp(istep - 1, Me.ncells - 1)
            End If
        End Get
    End Property

    Public Overridable ReadOnly Property TotalEgress() As Double
        Get
            Me.iout = 0.0
            For idx As Integer = 0 To Me.numConnections - 1
                If Me.wenter(idx) > 0.0 Then
                    Me.iout = Me.iout + Me.conns(idx).TotalEgress
                End If
            Next
            Return Me.iout
        End Get
    End Property

    Public Overridable ReadOnly Property TotalIn() As Double
        Get
            Me.hout = 0.0
            For idx As Integer = 0 To Me.numConnections - 1
                If Me.wenter(idx) > 0.0 Then
                    Me.hout = Me.hout + Me.conns(idx).TotalIn()
                End If
            Next
            Me.hout = Me.hout + Me.pin
            Return Me.hout
            Dim tt As Double = Me.TotalEgress
            If (tt - Me.hout) < Me.pin Then
                Dim ratio As Double
                If (tt - Me.hout) > 0.0 Then
                    ratio = (tt - Me.hout) / Me.pin
                    Me.pin = tt - Me.hout
                Else
                    ratio = 0.0
                    Me.pin = 0.0
                End If
                For idx As Integer = 0 To Me.ncells - 1
                    Me.D(idx) = ratio * Me.D(idx)
                Next
                Me.hout = tt
            Else
                Me.hout = Me.hout + Me.pin
            End If
            Return Me.hout
        End Get
    End Property

    Public Overridable ReadOnly Property TotalOut() As Double
        Get
            Me.eout = Me.TotalEgress - Me.TotalIn
            Return Me.eout
        End Get
    End Property

    Public Overridable Property NextElement() As EgressElement
        Get
            Return Me.endElement
        End Get
        Set(ByVal value As EgressElement)
            Me.endElement = value
        End Set
    End Property

    Public Overridable Sub CorrectDensity()

        Me.hout = 0.0
        For idx As Integer = 0 To Me.numConnections - 1
            If Me.wenter(idx) > 0.0 Then
                Me.hout = Me.hout + Me.conns(idx).TotalIn()
            End If
        Next
        Dim tt As Double = Me.TotalEgress
        If (tt - Me.hout) < Me.pin Then
            Dim ratio As Double
            If (tt - Me.hout) > 0.0 Then
                ratio = (tt - Me.hout) / Me.pin
                Me.pin = tt - Me.hout
            Else
                ratio = 0.0
                Me.pin = 0.0
            End If
            For idx As Integer = 0 To Me.ncells - 1
                Me.D(idx) = ratio * Me.D(idx)
            Next
            Me.hout = tt
        Else
            Me.hout = Me.hout + Me.pin
        End If

    End Sub

    Protected Overridable Function Ddot(ByVal Din As Double, ByVal sin As Double, ByVal wenter As Double, ByVal D As Double, _
                    ByVal s As Double, ByVal wexit As Double, ByVal w As Double, ByVal dx As Double) As Double

        Return (Din * sin * wenter - D * s * wexit) / (w * dx)

    End Function

    Protected Overridable Function sdot(ByVal D As Double, ByVal s As Double, ByVal wexit As Double, ByVal Dnxt As Double, _
                    ByVal snxt As Double, ByVal dx As Double) As Double
        Dim dddx As Double = (Dnxt - D) / dx
        Dim dsdx As Double = (snxt - s) / dx
        Return Me.s_a * (Me.speed(Me.DenSpdAve(D, Dnxt)) - s)
        'Return Me.s_a * ((Me.dmax - Dnxt) / Me.dmax * Me.sdmax - s) + Me.s_b * (wexit / w) * (Me.sdmax / Me.dmax) * (D * dsdx + (s - Me.sdmax) * dddx) _
        '                    - 0.1 * (D / Me.dmax) * dsdx ^ 2
    End Function

    Public Overridable Function FlowIn(ByVal istep As Integer) As Double

        Dim flw As Double = 0.0
        For icnt As Integer = 0 To Me.numConnections - 1
            If Me.wenter(icnt) > 0.0 Then
                flw = flw + Me.conns(icnt).FlowOut(istep)
            End If
        Next
        Return flw
    End Function

    Public Overridable Function FlowOut(ByVal istep As Integer) As Double
        Dim w0 As Double = Me.NextElement.EgressEntranceWidth(Me.EgressExitNum - 1)
        Dim d1 As Double = Me.NextElement.HeadD(istep)
        Dim d0, dtmp, dflg As Double
        Dim s As Double
        If istep > 0 Then
            d0 = Me.Dtmp(istep - 1, Me.ncells - 1)
            s = Me.sdtmp(istep - 1, Me.ncells - 1)
            If Me.ncells = 1 Then
                dflg = Me.FlowIn(istep)
            Else
                dflg = Me.Dtmp(istep - 1, Me.ncells - 2)
            End If
        Else
            d0 = Me.D(Me.ncells - 1)
            s = Me.sd(Me.ncells - 1)
            If Me.ncells = 1 Then
                dflg = Me.FlowIn(istep)
            Else
                dflg = Me.D(Me.ncells - 2)
            End If
        End If
        If dflg = 0.0 And d0 > 0.0 Then
            dtmp = Math.Max(0.54, Me.DenMassAve(d0, d1))
        Else
            dtmp = Me.DenMassAve(d0, d1)
        End If
        Me.fout = w0 * dtmp * s * Me.strfact
        Return Me.fout
    End Function

    Public Overridable Function DenMassAve(ByVal d0 As Double, ByVal d1 As Double) As Double
        d0 = Math.Max(0.0, Math.Min(d0, Me.dmax))
        d1 = Math.Max(0.0, Math.Min(d1, Me.dmax))
        Return ((2.0 - d0 / Me.dmax) * d0 + d0 / Me.dmax * d1) / 2.0
    End Function

    Public Overridable Function DenSpdAve(ByVal d0 As Double, ByVal d1 As Double) As Double
        d0 = Math.Max(0.0, Math.Min(d0, Me.dmax))
        d1 = Math.Max(0.0, Math.Min(d1, Me.dmax))
        Return ((1 - d1 / Me.dmax) * d0 + (1 + d1 / Me.dmax) * d1) / 2.0
    End Function

    Public Overridable Function RK4step0(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim istep As Integer = 0
        If Not Me.RKstep Then
            Dim flw As Double = Me.FlowIn(0)
            Me.kp(istep) = flw
            For icnt As Integer = 0 To Me.ncells - 2
                Me.K(0, 0, icnt) = flw
                'If flw = 0.0 And Me.D(icnt) > 0.0 Then
                'flw = Math.Max(0.54, Me.DenMassAve(Me.D(icnt), Me.D(icnt + 1))) * Me.sd(icnt) * Me.effw
                'Else
                'flw = Me.DenMassAve(Me.D(icnt), Me.D(icnt + 1)) * Me.sd(icnt) * Me.effw
                'End If
                flw = Me.DenMassAve(Me.D(icnt), Me.D(icnt + 1)) * Me.sd(icnt) * Me.effw
                Me.K(0, istep, icnt) = (Me.K(istep, 0, icnt) - flw) / (Me.effw * Me.dx)
                Me.K(1, istep, icnt) = Me.sdot(Me.D(icnt), Me.sd(icnt), Me.w, Me.D(icnt + 1), Me.sd(icnt + 1), Me.dx)
                Me.Dtmp(istep, icnt) = Math.Max(0.0, Me.D(icnt) + dt * Me.K(0, 0, icnt))
                Me.sdtmp(istep, icnt) = Me.sd(icnt) + dt * Me.K(1, 0, icnt)
            Next
            Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.exitConnNum - 1)
            Me.kp(istep) = Me.kp(istep) - Me.FlowOut(istep)
            Me.ptmp(istep) = Math.Max(0.0, Me.pin + dt * Me.kp(istep))
            Me.K(0, istep, (Me.ncells - 1)) = (flw - Me.FlowOut(istep)) / (Me.effw * Me.dx)
            Me.K(1, istep, Me.ncells - 1) = Me.sdot(Me.D(Me.ncells - 1), Me.sd(Me.ncells - 1), w0, Me.endElement.HeadD(istep), Me.endElement.HeadSpd(istep), dx)
            Me.Dtmp(istep, Me.ncells - 1) = Math.Max(0.0, Me.D(Me.ncells - 1) + dt * Me.K(0, 0, Me.ncells - 1))
            Me.sdtmp(istep, Me.ncells - 1) = Me.sd(Me.ncells - 1) + dt * Me.K(1, 0, Me.ncells - 1)
            Me.RKstep = True
        Else
            Me.RKstep = False
        End If
        Return Me.RKstep

    End Function


    Public Overridable Function RK4step1or2(ByVal istep As Integer, ByVal curtime As Double, ByVal dt As Double) As Boolean
        If Me.RKstep Then
            Dim flw As Double = Me.FlowIn(istep)
            Me.kp(istep) = flw
            For icnt As Integer = 0 To Me.ncells - 2
                Me.K(0, istep, icnt) = flw
                'If flw = 0.0 And Me.Dtmp(icnt) > 0.0 Then
                'flw = Math.Max(0.54, Me.DenMassAve(Me.Dtmp(icnt), Me.Dtmp(icnt + 1))) * Me.sdtmp(icnt) * Me.effw
                'Else
                'flw = Me.DenMassAve(Me.Dtmp(icnt), Me.Dtmp(icnt + 1)) * Me.sdtmp(icnt) * Me.effw
                'End If
                flw = Me.DenMassAve(Me.Dtmp(istep - 1, icnt), Me.Dtmp(istep - 1, icnt + 1)) * Me.sdtmp(istep - 1, icnt) * Me.effw
                Me.K(0, istep, icnt) = (Me.K(0, istep, icnt) - flw) / (Me.effw * Me.dx)
                Me.K(1, istep, icnt) = Me.sdot(Me.Dtmp(istep - 1, icnt), Me.sdtmp(istep - 1, icnt), Me.w, Me.Dtmp(istep - 1, icnt + 1), Me.sdtmp(istep - 1, icnt + 1), Me.dx)
                Me.Dtmp(istep, icnt) = Math.Max(0.0, Me.D(icnt) + dt * Me.K(0, istep, icnt))
                Me.sdtmp(istep, icnt) = Me.sd(icnt) + dt * Me.K(1, istep, icnt)
            Next
            Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.exitConnNum - 1)
            Me.kp(istep) = Me.kp(istep) - Me.FlowOut(istep)
            Me.ptmp(istep) = Math.Max(0.0, Me.pin + dt * Me.kp(istep))
            Me.K(0, istep, (Me.ncells - 1)) = (flw - Me.FlowOut(istep)) / (Me.effw * Me.dx)
            'Me.K(0, istep, (Me.ncells - 1)) = (flw - Me.Dtmp(istep - 1, Me.ncells - 1) * Me.sdtmp(istep - 1, Me.ncells - 1) * w0) / (Me.effw * Me.dx)
            Me.K(1, istep, Me.ncells - 1) = Me.sdot(Me.D(Me.ncells - 1), Me.sd(Me.ncells - 1), w0, Me.endElement.HeadD(istep), Me.endElement.HeadSpd(istep), dx)
            Me.Dtmp(istep, Me.ncells - 1) = Math.Max(0.0, Me.D(Me.ncells - 1) + dt * Me.K(0, istep, Me.ncells - 1))
            Me.sdtmp(istep, Me.ncells - 1) = Me.sd(Me.ncells - 1) + dt * Me.K(1, istep, Me.ncells - 1)
        End If
        Return Me.RKstep

    End Function


    Public Overridable Function RK4step3(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Dim istep As Integer = 3
        If Me.RKstep Then
            Dim flw As Double = Me.FlowIn(istep)
            Me.kp(istep) = flw
            For icnt As Integer = 0 To Me.ncells - 2
                Me.K(0, istep, icnt) = flw
                'If flw = 0.0 And Me.Dtmp(icnt) > 0.0 Then
                'flw = Math.Max(0.54, Me.DenMassAve(Me.Dtmp(icnt), Me.Dtmp(icnt + 1))) * Me.sdtmp(icnt) * Me.effw
                'Else
                'flw = Me.DenMassAve(Me.Dtmp(icnt), Me.Dtmp(icnt + 1)) * Me.sdtmp(icnt) * Me.effw
                'End If
                flw = Me.DenMassAve(Me.Dtmp(istep - 1, icnt), Me.Dtmp(istep - 1, icnt + 1)) * Me.sdtmp(istep - 1, icnt) * Me.effw
                Me.K(0, istep, icnt) = (Me.K(0, istep, icnt) - flw) / (Me.effw * Me.dx)
                Me.K(1, istep, icnt) = Me.sdot(Me.Dtmp(istep - 1, icnt), Me.sdtmp(istep - 1, icnt), Me.w, Me.Dtmp(istep - 1, icnt + 1), Me.sdtmp(istep - 1, icnt + 1), Me.dx)
            Next
            Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.exitConnNum - 1)
            Me.kp(istep) = Me.kp(istep) - Me.FlowOut(istep)
            Me.K(0, istep, (Me.ncells - 1)) = flw - Me.FlowOut(istep)
            'Me.K(0, istep, (Me.ncells - 1)) = flw - Me.Dtmp(istep - 1, Me.ncells - 1) * Me.sdtmp(istep - 1, Me.ncells - 1) * w0
            Me.K(1, istep, Me.ncells - 1) = Me.sdot(Me.Dtmp(istep - 1, Me.ncells - 1), Me.sdtmp(istep - 1, Me.ncells - 1), w0, Me.endElement.HeadD(istep), Me.endElement.HeadSpd(istep), dx)
        End If
        Return True
    End Function


    Public Overridable Function RK4Complete(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Dim flg As Boolean = False
        Dim pintmp As Double = 0.0
        If Me.RKstep Then
            Me.pin = Math.Max(0.0, Me.pin + dt / 6.0 * (Me.kp(0) + 2 * (Me.kp(1) + Me.kp(2)) + Me.kp(3)))
            For icnt As Integer = 0 To Me.ncells - 1
                Dim dddt As Double = (1.0 / 6.0) * (Me.K(0, 0, icnt) + 2.0 * (Me.K(0, 1, icnt) + Me.K(0, 2, icnt)) + Me.K(0, 3, icnt))
                Me.D(icnt) = Math.Max(0.0, Me.D(icnt) + dt * dddt)
                pintmp = pintmp + Me.D(icnt) * Me.dx * Me.effw
                Me.sd(icnt) = Me.sd(icnt) + dt * (1.0 / 6.0) * (Me.K(1, 0, icnt) + 2.0 * (Me.K(1, 1, icnt) + Me.K(1, 2, icnt)) + Me.K(1, 3, icnt))
            Next
            If pintmp > 0.0 Then
                Dim factor As Double = Me.pin / pintmp
                For icnt As Integer = 0 To Me.ncells - 1
                    Me.D(icnt) = factor * Me.D(icnt)
                Next
            End If
            Me.RKstep = False
            flg = Not Me.RKstep
        End If
        Return flg
    End Function
End Class

Public Class EgressFlight
    Inherits EgressElement

    Protected rsr As Double
    Protected trd As Double
    Protected numSteps As Integer

    Public Overridable Property Tread() As Double
        Get
            Return Me.trd
        End Get
        Set(ByVal value As Double)
            If value > 0.0 Then
                Me.trd = value
                Me.l = Me.calcLen()
                Me.Angle = Me.calcAngle()
            End If
        End Set
    End Property
    Public Overridable Property Riser() As Double
        Get
            Return Me.rsr
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.rsr = value
                Me.l = Me.calcLen()
                Me.Angle = Me.calcAngle()
            End If
        End Set
    End Property
    Public Overridable Property NumberSteps() As Integer
        Get
            Return Me.numSteps
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then
                Me.numSteps = value
                Me.calcLen()
            End If
        End Set
    End Property

    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errors

    Public Sub New(ByVal rsr As Double, ByVal trd As Double, ByVal numSteps As Integer, ByVal width As Double, ByVal widthtype As Integer, _
                    ByVal maxfin As Double)
        Me.New(1, rsr, trd, numSteps, width, widthtype, maxfin, 0.0)
    End Sub

    Public Sub New(ByVal numConnect As Integer, ByVal rsr As Double, ByVal trd As Double, ByVal numSteps As Integer, ByVal width As Double, _
                    ByVal widthtype As Double, ByVal maxfin() As Double)
        Me.New(numConnect, rsr, trd, numSteps, width, widthtype, maxfin(0), 0.0)
        For i As Integer = 0 To Me.numConnections - 1
            Me.finmax(i) = maxfin(i)
        Next
    End Sub

    Public Sub New(ByVal numConnect As Integer, ByVal mrgType As Integer, ByVal rsr As Double, ByVal trd As Double, ByVal numStps As Integer, ByVal width As Double, _
                    ByVal widthtype As Double, ByVal maxfin As Double)

        MyBase.New(numConnect, mrgType, 1.0, width, widthtype, maxfin, 0.0)

        Me.validPos(Me.trd, trd)
        Me.validPos(Me.rsr, rsr)
        Me.validPos(Me.numSteps, numStps)
        If Not Me.valid Then
            Me.l = 0.0
            Me.w = 0.0
            Me.trd = 0.0
            Me.rsr = 0.0
            Me.numSteps = 0
            Me.effw = 0.0
            Me.wtype = -1
            Me.eqno = 0
            Me.inno = 0
            ReDim Me.eqnum(0)
            ReDim Me.eqResults(0)
            ReDim Me.inputnum(0)
            ReDim Me.names(0)
            Me.elName = "InValid EgressFlight"
            Return
        End If
        Me.Angle = Me.calcAngle()
        Me.strfact = Math.Sqrt(Me.Tread ^ 2 / (Me.Tread ^ 2 + Me.Riser ^ 2))
        Me.effw = Me.effw * Me.strfact
        Me.sdmax = 1.19 + 0.215 * Me.Angle - 1.1511 * Me.Angle ^ 2
        Me.l = Me.calcLen()
        Me.pmax = Me.l * Me.w * Me.dmax
        Me.elName = "EgressFlight"
        'Code for pde solution

        Me.ncells = Math.Round(Me.l, 0)
        'Me.sdmax = Me.speed(0.0)
        Me.dx = Me.l / Me.ncells
        ReDim Me.D(Me.ncells - 1)
        ReDim Me.sd(Me.ncells - 1)
        ReDim Me.Dtmp(2, Me.ncells - 1)
        ReDim Me.sdtmp(2, Me.ncells - 1)
        ReDim Me.K(2, 4, Me.ncells - 1)
        Dim istep As Integer
        For i As Integer = 0 To Me.ncells - 1
            Me.D(i) = 0.0
            Me.sd(i) = Me.speed(0.0)
            For istep = 0 To 2
                Me.Dtmp(istep, i) = Me.D(i)
                Me.sdtmp(istep, i) = Me.sd(i)
            Next
        Next

    End Sub

    Public Shadows Sub redefine(ByVal rsr As Double, ByVal trd As Double, ByVal width As Double, ByVal widthtype As Integer, ByVal maxfin As Double)

        MyBase.Redefine(1.0, width, widthtype, maxfin, 0.0)
        Me.trd = trd
        Me.rsr = rsr
        Me.Angle = Me.calcAngle()
        Me.l = Me.calcLen()

    End Sub

    Protected Overridable Overloads Function calcLen() As Double
        Return Me.calcLen(Me.trd, Me.rsr, Me.numSteps)
    End Function

    Protected Overridable Overloads Function calcLen(ByVal tread As Double, ByVal riser As Double, ByVal numSteps As Integer) As Double
        Return numSteps * Math.Sqrt(tread ^ 2 + riser ^ 2)
    End Function

    Protected Overloads Function calcAngle() As Double
        Return Me.calcAngle(Me.trd, Me.rsr)
    End Function

    Protected Overloads Function calcAngle(ByVal tread As Double, ByVal riser As Double) As Double
        Return Math.Acos(Me.trd / Math.Sqrt(Me.trd ^ 2 + Me.rsr ^ 2))
    End Function
End Class

Public Class EgressSink
    Inherits EgressElement
    Protected Shadows Enum InTypes
        TranPop = -4
        LeadDist
        TailDist
        MaxFlowOut
        WaitPop
        FlowIn
        MaxInputs
    End Enum
    Protected Shadows Enum EqTypes
        TranPop = -4
        FlowOut
        LeadDist
        TailDist
        WaitPoP
        MaxFlowIn
        MaxEqs
    End Enum
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using.
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPoP < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPoP)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errors

    Public Sub New(ByVal maxfin As Double)
        MyBase.New()
        Me.setConstants(EqTypes.MaxEqs, InTypes.MaxInputs)
        Me.finmax(0) = maxfin
        Me.names(EqTypes.WaitPoP) = "Total Pop."
        Me.names(EqTypes.MaxFlowIn) = "Max Flow In (people/s)"
        Me.D(0) = 0.0
        Me.sd(0) = 0.0
        For istep As Integer = 0 To 2
            Me.Dtmp(istep, 0) = Me.D(0)
            Me.sdtmp(istep, 0) = Me.sd(0)
        Next
    End Sub

    Public Sub New()
        Me.New(1.0)
    End Sub

    Public Sub setConstants(ByVal numeq As Integer, ByVal numin As Integer)

        Me.eqno = numeq
        Me.inno = numin
        ReDim Preserve Me.eqnum(Me.eqno - 1)
        ReDim Preserve Me.inputnum(Me.inno - 1)
        ReDim Preserve Me.names(Me.eqno - 1)

    End Sub

    Public Overrides Sub seteqin(ByRef eqidx As Integer)

        Me.mapEqNums(EqTypes.WaitPoP + 1, EqTypes.WaitPoP + eqidx + 1)
        Me.mapEqNums(EqTypes.MaxFlowIn + 1, EqTypes.MaxFlowIn + eqidx + 1)

        'using EqTypes so that an object inhereting EgressElement, for example
        'EgressMCElement can call this sub and and not have to worry about how
        'the child object changes me.eqno
        Me.mapInputNums(InTypes.WaitPop + 1, EqTypes.WaitPoP + 1 + eqidx)
        eqidx = eqidx + EqTypes.MaxEqs
        Me.ready = True

    End Sub

    Public Overrides Function InitilizeODEVecters(ByRef state As Vector, ByRef mask As Vector, _
                            ByRef min As Vector, ByRef max As Vector, ByRef minval As Vector, ByRef maxval As Vector, _
                            ByRef mincon As Vector, ByRef maxcon As Vector, ByRef mincoef As Vector, ByRef maxcoef As Vector)
        'ByRef signs As Vector) As Boolean

        If Not Me.ready Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        state(Me.eqnum(EqTypes.WaitPoP)) = Me.totalPop
        mask(Me.eqnum(EqTypes.WaitPoP)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.TotPoP)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.WaitPoP)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.WaitPoP)) = 0.0
        mincon(Me.eqnum(EqTypes.WaitPoP)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.WaitPoP)) = SimpleODESolver.NoMaxBndry

        state(Me.eqnum(EqTypes.MaxFlowIn)) = 0.0
        mask(Me.eqnum(EqTypes.MaxFlowIn)) = SimpleODESolver.NonODEMask
        'signs(Me.eqnum(EqTypes.MaxFlowIn)) = SimpleODESolver.AnySign
        min(Me.eqnum(EqTypes.MaxFlowIn)) = SimpleODESolver.NoMinBndry
        max(Me.eqnum(EqTypes.MaxFlowIn)) = SimpleODESolver.NoMaxBndry

        Return True

    End Function

    Public Overrides Function Evaluate(ByRef Inputs As Vector) As Boolean
        Me.pin = Inputs(Me.inputnum(InTypes.WaitPop))
        Me.fin = Inputs(Me.inputnum(InTypes.FlowIn))
        Me.eqResults(EqTypes.WaitPoP) = Me.fin
        Me.eqResults(EqTypes.MaxFlowIn) = Me.finmax(0)
        Return True
    End Function

    ' routines for pde egress
    Public Overrides ReadOnly Property HeadD(ByVal istep As Integer) As Double
        Get
            Return 0.0
        End Get
    End Property

    Public Overrides ReadOnly Property HeadSpd(ByVal istep As Integer) As Double
        Get
            Return Me.sdmax
        End Get
    End Property

    Public Overrides ReadOnly Property TailD(ByVal istep As Integer) As Double
        Get
            Return 0.0
        End Get
    End Property

    Public Overrides ReadOnly Property TailSpd(ByVal istep As Integer) As Double
        Get
            Return Me.sdmax
        End Get
    End Property

    Public Overrides ReadOnly Property TotalEgress() As Double
        Get
            Return Me.conns(0).TotalEgress
        End Get
    End Property

    Public Overrides ReadOnly Property TotalIn() As Double
        Get
            Return Me.conns(0).TotalIn
        End Get
    End Property

    Public Overrides ReadOnly Property TotalOut() As Double
        Get
            Me.pin = Me.TotalEgress - Me.TotalIn
            Return Me.pin
        End Get
    End Property

    Public Overrides Function RK4step0(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Return True
    End Function

    Public Overrides Function RK4step1or2(ByVal istep As Integer, ByVal curtime As Double, ByVal dt As Double) As Boolean
        Return True
    End Function

    Public Overrides Function RK4step3(ByVal cutime As Double, ByVal dt As Double) As Boolean
        Return True
    End Function

    Public Overrides Function RK4Complete(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Me.pin = Me.TotalEgress - Me.TotalIn
        Return True
    End Function

    Public Overrides Property totalPop As Double
        Get
            Return Me.TotalOut
        End Get
        Set(ByVal value As Double)
            MyBase.totalPop = value
        End Set
    End Property
End Class

Public Class EgressElSink
    Inherits EgressSink
    Protected Shadows Enum InTypes
        TranPop = -4
        LeadDist
        TailDist
        MaxFlowOut
        WaitPop
        FlowIn
        MaxInputs
    End Enum
    Protected Shadows Enum EqTypes
        TranPop = -4
        FlowOut
        LeadDist
        TailDist
        WaitPoP
        MaxFlowIn
        MaxEqs
    End Enum
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using.
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPoP < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPoP)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errors

    Public Overrides Property totalPop As Double
        Get
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            MyBase.pin = value
        End Set
    End Property
End Class

Public Class EgressSource
    Inherits EgressElement
    Protected pinit As Double

    Protected Shadows Enum EqTypes
        TranPop = -4
        LeadDist
        TailDist
        MaxFlowIn
        WaitPop
        FlowOut
        MaxEqs
    End Enum
    Protected Shadows Enum InTypes
        TranPop = -4
        LeadDist
        TailDist
        FlowIn
        WaitPop
        MaxFlowOut
        MaxInputs
    End Enum
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errors
    Public Overrides Property totalPop() As Double
        Get
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            If value >= 0 Then
                Me.pin = value
                Me.pinit = value
                Me.D(0) = Me.pinit
            Else
                Me.pin = 0.0
                Me.pinit = 0.0
                Me.D(0) = 0.0
            End If
        End Set
    End Property
    Public Property Delay() As Double
        Get
            Return Me.stime
        End Get
        Set(ByVal value As Double)
            If value >= 0.0 Then
                Me.stime = value
            Else
                Me.stime = 0.0
            End If
        End Set
    End Property

    Public Sub New()
        Me.New(0.0)
    End Sub
    Public Sub New(ByVal delay As Double)
        MyBase.New()
        Me.Delay = delay
        Me.numConnections = 0
        Me.eqno = 2
        Me.inno = 2
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.names(Me.eqno - 1)
        Me.names(EqTypes.WaitPop) = "Total Pop."
        Me.names(EqTypes.FlowOut) = "Exit Flow (people/s)"
        Me.elName = "EgressCompartment"
    End Sub

    Public Overrides Sub seteqin(ByRef eqidx As Integer)

        If Not Me.valid Then Return
        Me.mapEqNums(EqTypes.WaitPop + 1, EqTypes.WaitPop + eqidx + 1)
        Me.mapEqNums(EqTypes.FlowOut + 1, EqTypes.FlowOut + eqidx + 1)
        Me.mapInputNums(InTypes.WaitPop + 1, EqTypes.WaitPop + eqidx + 1)

        'using EqTypes so that an object inhereting EgressElement, for example
        'EgressMCElement can call this sub and and not have to worry about how
        'the child object changes me.eqno
        eqidx = eqidx + EqTypes.MaxEqs
        Me.ready = True

    End Sub

    Public Overrides Function InitilizeODEVecters(ByRef state As Vector, ByRef mask As Vector, _
                    ByRef min As Vector, ByRef max As Vector, ByRef minval As Vector, ByRef maxval As Vector, _
                    ByRef mincon As Vector, ByRef maxcon As Vector, ByRef mincoef As Vector, ByRef maxcoef As Vector)
        'ByRef signs As Vector) As Boolean

        If Not Me.ready Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        state(Me.eqnum(EqTypes.WaitPop)) = Me.totalPop
        mask(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.ODEMask
        'signs(Me.eqnum(EqTypes.TotPop)) = SimpleODESolver.NonNeg
        min(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.WaitPop)) = 0.0
        mincon(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.NoMaxBndry

        state(Me.eqnum(EqTypes.FlowOut)) = 0.0
        mask(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NonODEMask
        'signs(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.AnySign
        min(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMinBndry
        max(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMaxBndry

        Return True

    End Function

    Public Overrides Function Evaluate(ByRef Inputs As Vector) As Boolean
        Me.pin = Math.Max(0.0, Inputs(Me.inputnum(InTypes.WaitPop)))
        If Me.pin > 0.0 Then
            Me.fout = Math.Max(0.0, Inputs(Me.inputnum(InTypes.MaxFlowOut)))
        Else
            Me.fout = 0.0
        End If
        'commented out 20090108
        'Me.eqResults(EqTypes.TotPop) = -Me.fout
        'end comment
        'added 20090108
        Me.eqResults(EqTypes.WaitPop) = -Inputs(Me.ExEqNoFlowOut)
        'end comment
        Me.eqResults(EqTypes.FlowOut) = Me.fout
        Return True
    End Function

    ' Additions for pde egress

    Public Overrides ReadOnly Property HeadD(ByVal istep As Integer) As Double
        Get
            Return -1.0
        End Get
    End Property

    Public Overrides ReadOnly Property HeadSpd(ByVal istep As Integer) As Double
        Get
            Return -1.0
        End Get
    End Property

    Public Overrides ReadOnly Property TailD(ByVal istep As Integer) As Double
        Get
            Dim tmp As Double = 0.0
            If Me.pin > 0.0 Then
                tmp = Me.dmax / 2.0
            End If
            Return tmp
        End Get
    End Property

    Public Overrides ReadOnly Property TailSpd(ByVal istep As Integer) As Double
        Get
            Dim tmp As Double = 0.0
            If Me.pin > 0.0 Then
                tmp = Me.sdmax / 2.0
            End If
            Return tmp
        End Get
    End Property

    Public Overrides ReadOnly Property TotalEgress() As Double
        Get
            Return Me.pinit - Me.pin
        End Get
    End Property

    Public Overrides ReadOnly Property TotalIn() As Double
        Get
            Return 0.0
        End Get
    End Property

    Public Overrides Function FlowOut(ByVal istep As Integer) As Double
        Dim w0 As Double = Me.NextElement.EgressEntranceWidth(Me.EgressExitNum - 1)
        Me.fout = Me.dmax / 2.0 * Me.sdmax / 2.0
        If istep = 0 Then
            If Me.pin <= 0.0 Then
                Me.fout = 0.0
            End If
        Else
            If Me.Dtmp(istep - 1, 0) <= 0.0 Then
                Me.fout = 0.0
            End If
        End If
        Return Me.fout * w0
    End Function

    Public Overrides Function RK4step0(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Dim istep As Integer = 0
        If Not Me.RKstep Then
            Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.EgressExitNum - 1)
            If curtime >= Me.stime Then
                If Me.D(0) > 0.0 Then
                    Me.K(0, istep, 0) = -Me.dmax / 2.0 * Me.sdmax / 2.0 * w0
                    Me.K(1, istep, 0) = 0.0
                Else
                    Me.K(0, istep, 0) = 0.0
                    Me.K(1, istep, 0) = 0.0
                End If
            Else
                Me.K(0, istep, 0) = 0.0
                Me.K(1, istep, 0) = 0.0
            End If
            Me.Dtmp(istep, 0) = Me.D(0) - Me.K(0, istep, 0) * dt
            Me.sdtmp(istep, 0) = Me.sdmax / 2.0
            If Me.Dtmp(istep, 0) < 0.0 Then
                Me.Dtmp(istep, 0) = 0.0
                Me.sdtmp(istep, 0) = Me.sdmax
            End If
            Me.RKstep = True
        Else
            Me.RKstep = False
        End If
        Return Me.RKstep
    End Function

    Public Overrides Function RK4step1or2(ByVal istep As Integer, ByVal curtime As Double, ByVal dt As Double) As Boolean
        If Me.RKstep Then
            If curtime >= Me.stime Then
                Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.EgressExitNum - 1)
                If Me.Dtmp(istep - 1, 0) > 0.0 Then
                    Me.K(0, istep, 0) = -Me.dmax / 2.0 * Me.sdmax / 2.0 * w0
                    Me.K(1, istep, 0) = 0.0
                Else
                    Me.K(0, istep, 0) = 0.0
                    Me.K(1, istep, 0) = 0.0
                End If
            Else
                Me.K(0, istep, 0) = 0.0
                Me.K(1, istep, 0) = 0.0
            End If
            Me.Dtmp(istep, 0) = Me.D(0) - Me.K(0, istep, 0) * dt
            Me.sdtmp(istep, 0) = Me.sdmax / 2.0
            If Me.Dtmp(istep, 0) < 0.0 Then
                Me.Dtmp(istep, 0) = 0.0
                Me.sdtmp(istep, 0) = Me.sdmax
            End If
        Else
            Me.RKstep = False
        End If
        Return Me.RKstep
    End Function

    Public Overrides Function RK4step3(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Dim istep As Integer = 3
        If Me.RKstep Then
            If curtime >= Me.stime Then
                Dim w0 As Double = Me.endElement.EgressEntranceWidth(Me.EgressExitNum - 1)
                If Me.Dtmp(istep - 1, 0) > 0.0 Then
                    Me.K(0, istep, 0) = -Me.dmax / 2.0 * Me.sdmax / 2.0 * w0
                    Me.K(1, istep, 0) = 0.0
                Else
                    Me.K(0, istep, 0) = 0.0
                    Me.K(1, istep, 0) = 0.0
                End If
            Else
                Me.K(0, istep, 0) = 0.0
                Me.K(1, istep, 0) = 0.0
            End If
        Else
            Me.RKstep = False
        End If
        Return Me.RKstep
    End Function

    Public Overrides Function RK4Complete(ByVal curtime As Double, ByVal dt As Double) As Boolean
        Dim flg As Boolean = True
        If Me.RKstep Then
            Me.D(0) = Me.D(0) + dt / 6.0 * (Me.K(0, 0, 0) + 2.0 * (Me.K(0, 1, 0) + Me.K(0, 2, 0)) + Me.K(0, 3, 0))
            If Me.D(0) <= 0.0 Then
                Me.D(0) = 0.0
                Me.sd(0) = Me.sdmax
            Else
                Me.sd(0) = Me.sdmax / 2.0
            End If
            Me.RKstep = False
            Me.pin = Me.D(0)
        Else
            flg = False
        End If
        Return flg
    End Function
End Class

Public Class EgressElLobby
    Inherits EgressSource
    Protected cll As Boolean
    Protected flr As Integer
    Shadows conns() As EgressElLobby

    Protected Shadows Enum EqTypes
        TranPop = -4
        LeadDist
        TailDist
        MaxFlowIn
        WaitPop
        FlowOut
        MaxEqs
    End Enum
    Protected Shadows Enum InTypes
        TranPop = -4
        LeadDist
        TailDist
        FlowIn
        WaitPop
        MaxFlowOut
        MaxInputs
    End Enum
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errorsz
    Public Property CallButton(ByVal curtime As Double) As Boolean
        Get
            If curtime >= Me.stime Then
                If Me.totalPop > 0 Then
                    Me.cll = True
                Else
                    Me.cll = False
                End If
                Return Me.cll
            Else
                Return False
            End If
        End Get
        Set(ByVal value As Boolean)
            If Me.totalPop > 0 Then
                Me.cll = value
            Else
                Me.cll = False
            End If
        End Set
    End Property
    Public Property Floor As Integer
        Get
            Return Me.flr
        End Get
        Set(ByVal value As Integer)
            Me.flr = value
        End Set
    End Property

    Public Sub New()
        Me.New(0.0)
    End Sub

    Public Sub New(ByVal lobbyDelay As Double)
        MyBase.New()
        Me.cll = False
        Me.Delay = lobbyDelay
    End Sub

    Public Overrides Property totalPop As Double
        Get
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            If value >= 0.0 Then
                Me.pin = value
            Else
                Me.pin = 0.0
            End If
        End Set
    End Property

End Class

Public Class EgressElevator
    Inherits EgressElement

    Protected Shadows conns() As EgressElLobby
    Protected numFlr As Integer
    Protected evacFlr As Integer
    Protected maxFlwOut As Double
    Protected flrHieght As Double
    Protected maxCarCap As Integer
    Protected numCars As Integer
    Protected vmax As Double
    Protected acc As Double
    Protected tran As Double = 1.0
    Protected ineff As Double = 0.0
    Protected v1 As Double
    Protected s1 As Double
    Protected s2 As Double
    Protected t1 As Double
    Protected t2 As Double
    Protected lvlTime As Double = 0.5
    Protected Shared defltLoadRate As Double = 1.0
    Protected Shared defltUnLoadRate As Double = 1.667
    Protected defTrvlTime As Boolean = True
    Protected doorIdx As Integer
    Protected fxdTime As Double
    Protected tToFlr() As Double
    Protected trvlTime As Double
    Protected noValue As Double = 0.0
    Protected setDest As Boolean
    Protected doorTimes(,) As Double
    Protected doorNames() As String
    Protected ElState As Integer
    '
    Protected timeFlag As Double
    Protected outside As EgressElSink
    Protected fracEl As Double = 0.8

    Protected Shadows Enum EqTypes
        TranPop = -3
        LeadDist
        TailDist
        WaitPop
        TrvlTime
        FlowOut
        MaxFlowIn
        MaxEqs
    End Enum
    Protected Shadows Enum InTypes
        TranPop = -3
        LeadDist
        TailDist
        WaitPop
        TrvlTime
        MaxFlowOut
        FlowIn
        MaxInputs
    End Enum
    Protected Enum ElDoorTypes
        SingleSide09
        TwoSpeed09
        CenterOpen09
        SingleSide11
        TwoSpeed11
        CenterOpen11
        TwoSpeed12
        CenterOpen12
        TwoSpeed14
        CenterOpen14
        TwoSpeed16
        CenterOpen16
        MaxElDoorTypes
    End Enum
    Protected Enum ElStateTypes
        Recall
        Loading
        Unloading
        TransitUp
        TransitDwn
        ElStandby
        MaxElStateTypes
    End Enum

    Protected Sub DefineDoorTypes()

        ReDim Me.doorNames(ElDoorTypes.MaxElDoorTypes - 1)
        ReDim Me.doorTimes(ElDoorTypes.MaxElDoorTypes - 1, 1)

        Me.doorTimes(ElDoorTypes.SingleSide09, 0) = 6.6
        Me.doorTimes(ElDoorTypes.SingleSide09, 1) = 0.1
        Me.doorNames(ElDoorTypes.SingleSide09) = "Single-Side 900 mm (36 in)"
        Me.doorTimes(ElDoorTypes.TwoSpeed09, 0) = 5.9
        Me.doorTimes(ElDoorTypes.TwoSpeed09, 1) = 0.1
        Me.doorNames(ElDoorTypes.TwoSpeed09) = "Two-Speed 900 mm (36 in)"
        Me.doorTimes(ElDoorTypes.CenterOpen09, 0) = 4.1
        Me.doorTimes(ElDoorTypes.CenterOpen09, 1) = 0.08
        Me.doorNames(ElDoorTypes.CenterOpen09) = "Center-Opening 900 mm (36 in)"
        Me.doorTimes(ElDoorTypes.SingleSide11, 0) = 7.0
        Me.doorTimes(ElDoorTypes.SingleSide11, 1) = 0.07
        Me.doorNames(ElDoorTypes.SingleSide11) = "Single-Side 1100 mm (42 in)"
        Me.doorTimes(ElDoorTypes.TwoSpeed11, 0) = 6.6
        Me.doorTimes(ElDoorTypes.TwoSpeed11, 1) = 0.07
        Me.doorNames(ElDoorTypes.TwoSpeed11) = "Two-Speed 1100 mm (42 in)"
        Me.doorTimes(ElDoorTypes.CenterOpen11, 0) = 4.6
        Me.doorTimes(ElDoorTypes.CenterOpen11, 1) = 0.07
        Me.doorNames(ElDoorTypes.CenterOpen11) = "Center-Open 1100 mm (42 in)"
        Me.doorTimes(ElDoorTypes.TwoSpeed12, 0) = 7.7
        Me.doorTimes(ElDoorTypes.TwoSpeed12, 1) = 0.02
        Me.doorNames(ElDoorTypes.TwoSpeed12) = "Two-Speed 1200 mm (48 in)"
        Me.doorTimes(ElDoorTypes.CenterOpen12, 0) = 5.3
        Me.doorTimes(ElDoorTypes.CenterOpen12, 1) = 0.0
        Me.doorNames(ElDoorTypes.CenterOpen12) = "Center-Open 1200 mm (48 in)"
        Me.doorTimes(ElDoorTypes.TwoSpeed14, 0) = 8.8
        Me.doorTimes(ElDoorTypes.TwoSpeed14, 1) = 0.02
        Me.doorNames(ElDoorTypes.TwoSpeed14) = "Two-Speed 1400 mm (54 in)"
        Me.doorTimes(ElDoorTypes.CenterOpen14, 0) = 6.0
        Me.doorTimes(ElDoorTypes.CenterOpen14, 1) = 0.0
        Me.doorNames(ElDoorTypes.CenterOpen14) = "Center-Open 1400 mm (54 in)"
        Me.doorTimes(ElDoorTypes.TwoSpeed16, 0) = 9.9
        Me.doorTimes(ElDoorTypes.TwoSpeed16, 1) = 0.02
        Me.doorNames(ElDoorTypes.TwoSpeed16) = "Two-Speed 1600 mm (60 in)"
        Me.doorTimes(ElDoorTypes.CenterOpen16, 0) = 6.5
        Me.doorTimes(ElDoorTypes.CenterOpen16, 1) = 0.0
        Me.doorNames(ElDoorTypes.CenterOpen16) = "Center-Opening 1600 mm (60 in)"

    End Sub

    Public Shared ReadOnly Property defaultLoadRate() As Double
        Get
            Return EgressElevator.defltLoadRate
        End Get
    End Property
    Public Shared ReadOnly Property defaultUnloadRate() As Double
        Get
            Return EgressElevator.defltUnLoadRate
        End Get
    End Property
    Public Overridable ReadOnly Property timeToFloor() As Double
        Get
            Return Me.tToFlr(Me.evacFlr)
        End Get
    End Property
    Public Overrides Property width() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property effWidth() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property length() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property WaitPop() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property transitPop() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property BoundryLayer() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overrides Property BoundryType() As Integer
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Integer)
            Return
        End Set
    End Property
    Public Overrides Property incline() As Double
        Get
            Return Me.noValue
        End Get
        Set(ByVal value As Double)
            Return
        End Set
    End Property
    Public Overloads Property maxFlowIn() As Double
        Get
            Return Me.finmax(0)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then
                Me.finmax(0) = value
            End If
        End Set
    End Property
    Public Overloads Property maxFlowIn(ByVal idx As Integer) As Double
        Get
            If idx <= 0 Or idx > Me.numConnections Then Return Me.noValue
            Return Me.finmax(0)
        End Get
        Set(ByVal value As Double)
            If idx > 0 And idx <= Me.numConnections Then
                If value > 0.0 Then
                    Me.finmax(0) = value
                End If
            End If
        End Set
    End Property
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            If EqTypes.WaitPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.WaitPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            If EqTypes.TranPop < 0 Then Return -1
            Return Me.eqnum(EqTypes.TranPop)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            If EqTypes.FlowOut < 0 Then Return -1
            Return Me.eqnum(EqTypes.FlowOut)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            If EqTypes.LeadDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.LeadDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            If EqTypes.TailDist < 0 Then Return -1
            Return Me.eqnum(EqTypes.TailDist)
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn)
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If EqTypes.MaxFlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return Me.eqnum(EqTypes.MaxFlowIn + idx - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            If InTypes.WaitPop < 0 Then Return -1
            Return InTypes.WaitPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            If InTypes.TranPop < 0 Then Return -1
            Return InTypes.TranPop + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            Return InTypes.FlowIn + 1
        End Get
    End Property
    Public Overloads Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            If InTypes.FlowIn < 0 Then Return -1
            If idx <= 0 Or idx > Me.numConnections Then Return -1
            Return InTypes.FlowIn + idx
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            If InTypes.LeadDist < 0 Then Return -1
            Return InTypes.LeadDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            If InTypes.TailDist < 0 Then Return -1
            Return InTypes.TailDist + 1
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            If InTypes.MaxFlowOut < 0 Then Return -1
            Return InTypes.MaxFlowOut + 1
        End Get
    End Property
    'End of properites included because Visual Basic errors
    Public Overridable ReadOnly Property ExEqNoTrvlTime() As Integer
        Get
            If EqTypes.TrvlTime < 0 Then Return -1
            Return Me.eqnum(EqTypes.TrvlTime)
        End Get
    End Property
    Public Overridable ReadOnly Property InEqNoTrvlTime() As Integer
        Get
            If InTypes.TrvlTime < 0 Then Return -1
            Return InTypes.TrvlTime + 1
        End Get
    End Property
    Public Overrides Property nameExEqNo(ByVal idx As Integer) As String
        Get
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Return Me.names(i)
                End If
            Next
            Return ""
        End Get
        Set(ByVal value As String)
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Me.names(i) = value
                End If
            Next
        End Set
    End Property
    Public Overrides Property nameInEqNo(ByVal idx As Integer) As String
        Get
            If idx <= Me.eqno And idx >= 1 Then
                Return Me.names(idx - 1)
            End If
            Return ""
        End Get
        Set(ByVal value As String)
            If idx <= Me.eqno And idx >= 1 Then
                Me.names(idx - 1) = value
            End If
        End Set
    End Property
    Public Overridable ReadOnly Property Destination() As Integer
        Get
            If Me.evacFlr < 0 Then
                Return 1
            Else
                Return Me.conns(Me.evacFlr).Floor
            End If
        End Get
    End Property

    Sub New(ByVal numFloors As Integer, ByVal doorIdx As Integer, ByVal speed As Double, _
            ByVal accel As Double, ByVal flrhigh As Double, ByVal nmCars As Integer, _
            ByVal maxCap As Integer, ByVal maxfin As Double, ByVal recallDelay As Double)

        Me.DefineDoorTypes()
        Me.ElState = ElStateTypes.Recall
        Me.ready = False
        Me.l = Me.noValue
        Me.w = Me.noValue
        Me.merge = Me.noValue
        Me.effw = Me.noValue
        Me.wtype = Me.noValue
        Me.errCode = ErrCodeNums.NoErrors
        Me.validPos(Me.pmax, nmCars * maxCap)
        Me.numCars = nmCars
        Me.maxCarCap = maxCap
        Me.validPos(Me.numFlr, numFloors)
        Me.numConnections = Me.numFlr - 1
        Me.fxdTime = Me.CalcDoorTime(doorIdx)
        Me.validPos(Me.vmax, speed)
        Me.validPos(Me.acc, accel)
        Me.validPos(Me.flrHieght, flrhigh)
        Me.validPos(Me.trvlTime, recallDelay)
        Me.timeFlag = Me.trvlTime
        Me.evacFlr = -1
        If Me.valid Then
            ReDim Me.finmax(Me.numConnections - 1)
            ReDim Me.tToFlr(Me.numConnections - 1)
            Me.validPos(Me.maxFlwOut, nmCars * maxfin)
            For i As Integer = 0 To Me.numConnections - 1
                Me.finmax(i) = 0.0
                Me.tToFlr(i) = (1 + Me.ineff) * (Me.CalcFloorTime(i + 1) + Me.fxdTime)
            Next
        End If
        If Not Me.valid Then
            Me.InvalidateElement()
            Return
        End If
        Me.eqno = EqTypes.MaxEqs + Me.numConnections - 1
        Me.inno = InTypes.MaxInputs + Me.numConnections - 1
        ReDim Me.conns(Me.numConnections - 1)
        ReDim Me.eqnum(Me.eqno - 1)
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.names(Me.eqno - 1)
        Me.names(EqTypes.WaitPop) = "Total Pop."
        Me.names(EqTypes.TrvlTime) = "Travel Time (s)"
        Me.names(EqTypes.FlowOut) = "Exit Flow (people/s)"
        For i As Integer = 0 To Me.numConnections - 1
            Me.names(EqTypes.MaxFlowIn + i) = "Max Flow In (people/s)"
        Next
        Me.elName = "EgressElevtor"
        Me.pin = 0.0
        Me.pw = Me.noValue
        Me.pa = Me.noValue

    End Sub
    Protected Overrides Sub InvalidateElement()
        Me.pmax = 0.0
        Me.numConnections = 0
        Me.vmax = 0.0
        Me.acc = 0.0
        Me.fxdTime = 0.0
        Me.flrHieght = 0.0
        ReDim Me.eqnum(0)
        ReDim Me.eqResults(0)
        ReDim Me.inputnum(0)
        ReDim Me.names(0)
        Me.elName = "Invalide EgressElevator"
        Return
    End Sub

    Protected Function CalcDoorTime(ByVal tmpDoorType As Integer) As Double

        If tmpDoorType < 0 Or tmpDoorType >= ElDoorTypes.MaxElDoorTypes Then
            Me.valid = False
            Me.doorIdx = -1
            Return -1
        End If
        Me.doorIdx = tmpDoorType
        'Return (1 + Me.doorTimes(tmpDoorType, 1)) * 2 * Me.doorTimes(tmpDoorType, 0)
        Return (1 + Me.doorTimes(tmpDoorType, 1)) * Me.doorTimes(tmpDoorType, 0)
    End Function

    Protected Function CalcFloorTime(ByVal flrIdx As Integer) As Double

        Dim hght As Double = flrIdx * Me.flrHieght

        If Me.defTrvlTime Then
            Me.v1 = Me.tran * Me.vmax
            Me.s1 = Me.v1 ^ 2 / (2.0 * Me.acc)
            Me.s2 = (1.0 / (3.0 * Me.acc)) * (Me.vmax ^ 3 / Me.v1 - Me.v1 ^ 2) + Me.s1
            Me.t1 = Me.v1 / Me.acc
            Me.t2 = (Me.vmax ^ 2 - Me.v1 ^ 2) / (2.0 * Me.acc * Me.v1) + Me.t1
            Me.defTrvlTime = False
        End If

        Dim tmp As Double
        If hght > 0.0 And hght < Me.s1 Then
            tmp = 2 * Math.Sqrt(hght / Me.acc) + Me.lvlTime
        ElseIf hght < 2 * Me.s2 Then
            Dim x As Double = Me.v1 ^ 3 + 3 * Me.acc * Me.v1 * (hght / 2 - Me.s1)
            x = x ^ (1.0 / 3.0)
            x = (x ^ 2 - Me.v1 ^ 2) / (2 * Me.acc * Me.v1) + t1
            tmp = 2 * x + Me.lvlTime
        Else
            tmp = 2 * Me.t2 + (hght - 2 * Me.s2) / Me.vmax + Me.lvlTime
        End If
        Return tmp

    End Function

    Public Overridable Function ResetEvacFloor(ByVal s As Vector) As Boolean
        If Me.ElState = ElStateTypes.Recall And Me.trvlTime <= 0 Then
            Me.ElState = ElStateTypes.Unloading
        End If
        If Me.ElState = ElStateTypes.Unloading Then
            If s(eqnum(EqTypes.TrvlTime)) <= 0.0 And s(eqnum(EqTypes.WaitPop)) <= 0.0 Then
                For i As Integer = Me.numConnections - 1 To 0 Step -1
                    If Me.conns(i).totalPop > 0.0 Then
                        Me.evacFlr = i
                        Me.ElState = ElStateTypes.TransitUp
                        Return True
                    End If
                Next
                Me.ElState = ElStateTypes.ElStandby
            End If
        ElseIf Me.ElState = ElStateTypes.TransitUp Then
            If s(eqnum(EqTypes.TrvlTime)) = Me.tToFlr(Me.evacFlr) Then
                Me.ElState = ElStateTypes.Loading
            End If
        ElseIf Me.ElState = ElStateTypes.Loading Then
            If s(eqnum(EqTypes.WaitPop)) = Me.pmax Or s(Me.conns(Me.evacFlr).ExEqNoWaitPop) <= 0.0 Then
                Me.ElState = ElStateTypes.TransitDwn
            End If
        ElseIf Me.ElState = ElStateTypes.TransitDwn Then
            If s(eqnum(EqTypes.TrvlTime)) = 0.0 Then
                Me.ElState = ElStateTypes.Unloading
            End If
        End If
        Return False
    End Function

    Public Overridable Function ElevatorAction() As String
        Dim s As String = ""

        If Me.ElState = ElStateTypes.Recall Then
            s = "In Recall"
        ElseIf Me.ElState = ElStateTypes.Unloading Then
            s = "Unloading"
        ElseIf Me.ElState = ElStateTypes.Loading Then
            s = "Loading"
        ElseIf Me.ElState = ElStateTypes.TransitUp Then
            s = "In transit to evac floor"
        ElseIf Me.ElState = ElStateTypes.TransitDwn Then
            s = "In Transit to base floor"
        ElseIf Me.ElState = ElStateTypes.ElStandby Then
            s = "Elevator in Standby"
        Else
            s = "There is an error"
        End If

        Return s
    End Function

    Public Overrides Sub seteqin(ByRef eqidx As Integer)

        If Not Me.valid Then Return
        Me.mapEqNums(EqTypes.WaitPop + 1, EqTypes.WaitPop + eqidx + 1)
        Me.mapEqNums(EqTypes.TrvlTime + 1, EqTypes.TrvlTime + eqidx + 1)
        Me.mapEqNums(EqTypes.FlowOut + 1, EqTypes.FlowOut + eqidx + 1)
        For i As Integer = 1 To Me.numConnections
            Me.mapEqNums(EqTypes.MaxFlowIn + i, EqTypes.MaxFlowIn + eqidx + i)
        Next
        'using EqTypes so that an object inhereting EgressElement, for example
        'EgressMCElement can call this sub and and not have to worry about how
        'the child object changes me.eqno
        eqidx = eqidx + EqTypes.MaxEqs + Me.numConnections - 1

        Me.mapInputNums(Me.InEqNoWaitPop, Me.ExEqNoWaitPop)
        Me.mapInputNums(Me.InEqNoTrvlTime, Me.ExEqNoTrvlTime)
        Me.ready = True

    End Sub

    Public Sub ConnectOutside(ByRef oside As EgressElSink)

        Me.outside = oside

    End Sub

    Public Overrides Function ConnectInFlow(ByVal icon As Integer, ByRef element As EgressElement) As Boolean

        If icon <= 0 Or icon > Me.numConnections Then Return False
        If Me.InEqNoFlowIn(icon) > 0 And element.ExEqNoFlowOut > 0 Then
            Me.mapInputNums(Me.InEqNoFlowIn(icon), element.ExEqNoFlowOut)
            If element.InEqNoMaxFlowOut > 0 And Me.ExEqNoMaxFlowIn(icon) > 0 Then
                element.mapInputNums(element.InEqNoMaxFlowOut, Me.ExEqNoMaxFlowIn(icon))
                Me.conns(icon - 1) = element
                element.NextElement = Me
                element.EgressExitNum = icon
                Return True
            End If
        End If
        Return False

    End Function

    Public Overrides Function InitilizeODEVecters(ByRef state As Vector, _
                            ByRef mask As Vector, ByRef min As Vector, ByRef max As Vector, _
                            ByRef minval As Vector, ByRef maxval As Vector, ByRef mincon As Vector, ByRef maxcon As Vector, _
                            ByRef mincoef As Vector, ByRef maxcoef As Vector)

        If Not Me.ready Or Not Me.valid Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        state(Me.eqnum(EqTypes.WaitPop)) = Me.totalPop
        mask(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.ODEMask
        min(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.WaitPop)) = 0.0
        mincon(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.WaitPop)) = SimpleODESolver.MaxBndry
        maxval(Me.eqnum(EqTypes.WaitPop)) = Me.pmax
        maxcon(Me.eqnum(EqTypes.WaitPop)) = Me.noValue
        maxcoef(Me.eqnum(EqTypes.WaitPop)) = 1.0

        Me.evacFlr = 0
        state(Me.eqnum(EqTypes.TrvlTime)) = Me.trvlTime
        mask(Me.eqnum(EqTypes.TrvlTime)) = SimpleODESolver.ODEMask
        min(Me.eqnum(EqTypes.TrvlTime)) = SimpleODESolver.MinBndry
        minval(Me.eqnum(EqTypes.TrvlTime)) = 0.0
        mincon(Me.eqnum(EqTypes.TrvlTime)) = SimpleODESolver.NoConnectionEq
        max(Me.eqnum(EqTypes.TrvlTime)) = SimpleODESolver.MaxBndry
        maxval(Me.eqnum(EqTypes.TrvlTime)) = Math.Max(1.0, Me.trvlTime)
        maxcon(Me.eqnum(EqTypes.TrvlTime)) = SimpleODESolver.NoConnectionEq

        state(Me.eqnum(EqTypes.FlowOut)) = Me.fout
        mask(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NonODEMask
        min(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMinBndry
        max(Me.eqnum(EqTypes.FlowOut)) = SimpleODESolver.NoMaxBndry

        For i As Integer = 0 To Me.numConnections - 1
            state(Me.eqnum(EqTypes.MaxFlowIn + i)) = 0.0
            mask(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NonODEMask
            min(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NoMinBndry
            max(Me.eqnum(EqTypes.MaxFlowIn + i)) = SimpleODESolver.NoMaxBndry
        Next

        Return True

    End Function

    Protected Overrides Function CalculateEq() As Boolean

        'Do EqTypes.TotPop equation
        Me.pin = Me.pw
        If Me.ElState = ElStateTypes.Unloading Then
            Me.fin = 0.0
        ElseIf Me.ElState = ElStateTypes.Loading Then
            Me.fout = 0.0
        Else
            Me.fin = 0.0
            Me.fout = 0.0
        End If
        Me.eqResults(EqTypes.WaitPop) = Me.fin - Me.fout

        'Do EqTypes.FlowOut Equation
        If Me.ElState = ElStateTypes.Unloading Then
            If Me.pin > 0.0 Then
                Me.fout = Math.Max(0.0, Me.foutmax)
            Else
                Me.fout = 0.0
            End If
        Else
            Me.fout = 0.0
        End If
        Me.eqResults(EqTypes.FlowOut) = Me.fout

        'Do Eqtypes.TrvlTime Equation
        If Me.ElState = ElStateTypes.Recall Then
            Me.eqResults(EqTypes.TrvlTime) = -1.0
        ElseIf Me.ElState = ElStateTypes.TransitDwn Then
            Me.eqResults(EqTypes.TrvlTime) = -1.0
        ElseIf Me.ElState = ElStateTypes.TransitUp Then
            Me.eqResults(EqTypes.TrvlTime) = 1.0
        Else
            Me.eqResults(EqTypes.TrvlTime) = 0.0
        End If

            'Do EqTypes.MaxFlowIn Equation
        For i As Integer = 0 To Me.numConnections - 1
            Me.finmax(i) = 0.0
            Me.eqResults(EqTypes.MaxFlowIn + i) = 0.0
        Next
        If Me.ElState = ElStateTypes.Loading Then
            Me.finmax(Me.evacFlr) = Me.maxFlwOut
            Me.eqResults(EqTypes.MaxFlowIn + Me.evacFlr) = Me.maxFlwOut
        End If

        Return True

    End Function

    Public Overrides Function Evaluate(ByRef Inputs As Vector) As Boolean
        ' equation 1 eqnum(EqTypes.TotPop) dP_in/dt         input 1 inputnum(InTypes.TotPop) Population in element P_in or pin
        ' equation 2 eqnum(EqTypes.TrvlTime) dTrvlTime/dt   input 2 inputnum(InTypes.TrvlTime) Travel Time in seconds
        ' equation 3 eqnum(EqTypes.FlowOut) flow out        input 3 inputnum(InTypes.MaxFlowOut) max flow into next element
        ' equation 4 eqnum(EqTypes.MaxFlowIn) max flow in   input 4 inputnum(InTypes.FlowIn) flow into element     fin

        If Me.useDt Then Me.dt = Inputs(Me.dtIdx)
        Me.pw = Math.Max(0.0, Inputs(inputnum(InTypes.WaitPop)))
        Me.pa = 0.0
        Me.trvlTime = Math.Max(0.0, Inputs(inputnum(InTypes.TrvlTime)))
        Me.fin = 0.0
        For i As Integer = 0 To Me.numConnections - 1
            Me.fin = Me.fin + Math.Max(0.0, Inputs(inputnum(InTypes.FlowIn + i)))
        Next
        Me.foutmax = Math.Max(0.0, Inputs(inputnum(InTypes.MaxFlowOut)))
        Me.fout = Inputs(Me.ExEqNoFlowOut)

        If Not Me.CalculateEq Then
            Me.errCode = ErrCodeNums.ElementEvaluateCalculateEq
            Return False
        End If

        Return True

    End Function

    Public Overrides Function MakePopInteger(ByVal state As Matricies.Vector) As Matricies.Vector
        Dim idx As Integer

        idx = Me.ExEqNoWaitPop
        If idx > 0 And idx <= state.dimension Then
            state(idx) = Math.Round(state(idx))
        End If
        Return state.clone
    End Function

    Public Overridable Function PDEResetEvacFlr(ByVal curTime As Double) As Boolean
        If Me.timeFlag < 0.0 Then
            Return False
        ElseIf Me.timeFlag <= curTime Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Overridable Function PDEDoElevator(ByVal curTime As Double) As Boolean
        If Me.ElState = ElStateTypes.Recall And Me.trvlTime <= curTime Then
            Me.timeFlag = Me.trvlTime
            Me.ElState = ElStateTypes.Unloading
        End If
        If Me.timeFlag + 0.0001 < curTime Then
            If Me.ElState = ElStateTypes.Unloading Then
                Me.outside.totalPop = Me.outside.totalPop + Me.totalPop
                Me.totalPop = 0.0
                For i As Integer = Me.numConnections - 1 To 0 Step -1
                    If Me.conns(i).CallButton(curTime) Then
                        Me.evacFlr = i
                        Me.conns(i).CallButton(curTime) = False
                        Me.timeFlag = Me.timeFlag + Me.tToFlr(Me.conns(i).Floor - 2)
                        Me.ElState = ElStateTypes.TransitUp
                        Return True
                    End If
                Next
                Me.timeFlag = Me.timeFlag + 1.0
                Me.ElState = ElStateTypes.ElStandby
            ElseIf Me.ElState = ElStateTypes.TransitUp Then
                Me.timeFlag = Me.timeFlag + Me.KloteLoadTime(Math.Min(Me.maxCarCap - Me.totalPop, Me.conns(Me.evacFlr).totalPop))
                Me.ElState = ElStateTypes.Loading
            ElseIf Me.ElState = ElStateTypes.Loading Then
                Dim tmp As Double = Math.Min(Me.maxCarCap - Me.totalPop, Me.conns(Me.evacFlr).totalPop)
                Me.totalPop = Me.totalPop + tmp
                Me.conns(Me.evacFlr).totalPop = Me.conns(Me.evacFlr).totalPop - tmp
                Me.conns(Me.evacFlr).CallButton(curTime) = True
                If Me.totalPop / Me.maxCarCap < Me.fracEl Then
                    For i As Integer = Me.evacFlr - 1 To 0 Step -1
                        If Me.conns(i).CallButton(curTime) Then
                            Me.conns(i).CallButton(curTime) = False
                            Me.timeFlag = Me.timeFlag + Me.tToFlr(Me.conns(Me.evacFlr).Floor - Me.conns(i).Floor - 1)
                            Me.evacFlr = i
                            Me.ElState = ElStateTypes.TransitUp
                            Return True
                        End If
                    Next
                End If
                Me.timeFlag = Me.timeFlag + Me.tToFlr(Me.conns(Me.evacFlr).Floor - 2)
                Me.evacFlr = -1
                Me.ElState = ElStateTypes.TransitDwn
            ElseIf Me.ElState = ElStateTypes.TransitDwn Then
                Me.timeFlag = Me.timeFlag + Me.KloteUnloadTime(Me.totalPop / Me.numCars)
                Me.ElState = ElStateTypes.Unloading
            ElseIf Me.ElState = ElStateTypes.ElStandby Then
                For i As Integer = Me.numConnections - 1 To 0 Step -1
                    If Me.conns(i).CallButton(curTime) Then
                        Me.conns(i).CallButton(curTime) = False
                        Me.timeFlag = Me.timeFlag + Me.tToFlr(Me.conns(i).Floor - 2)
                        Me.evacFlr = i
                        Me.ElState = ElStateTypes.TransitUp
                        Return True
                    End If
                Next
                Me.timeFlag = Me.timeFlag + 1.0
                Return True
            End If
        End If
        Return True
    End Function

    Public Overridable Function PDEElevatorOut() As Double
        Return Me.outside.totalPop
    End Function

    Private Function KloteLoadUnloadTime(ByVal capCar As Double, ByVal tload As Double) As Double
        If capCar < 3 Then
            Return 4.0
        Else
            Return 4.0 + tload * (capCar - 2)
        End If
    End Function

    Public Function KloteLoadTime(ByVal capCar As Double) As Double
        Return Me.KloteLoadUnloadTime(capCar, 1.0)
    End Function

    Public Function KloteUnloadTime(ByVal capCar As Double) As Double
        Return Me.KloteLoadUnloadTime(capCar, 0.6)
    End Function
End Class


' Composite Egress Elements

Public Class EgressComposite
    Inherits EgressElement

    Protected elements() As EgressElement
    Protected element0, elementLast As EgressElement
    Protected eqnums(,) As Integer
    Protected inputnums(,) As Integer
    Protected numElements As Integer
    Protected errElement As Integer = -1
    Protected errComposit As Boolean = True
    Protected setErrMsg As Boolean = False

    Public Overrides Property width() As Double
        Get
            Return Me.w
        End Get
        Set(ByVal value As Double)
            For i As Integer = 0 To Me.numElements - 1
                Me.width(i) = value
            Next
        End Set
    End Property
    Public Overridable Overloads Property width(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).width
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0.0 Then Return
            If idx >= 0 And idx < Me.numElements Then
                Me.elements(idx).width = value
                If value < Me.elements(idx).effWidth Then
                    Me.elements(idx).effWidth = value
                    If value < Me.effw Then Me.effw = value
                End If
                If value < Me.w Then Me.w = value
            End If
        End Set
    End Property
    Public Overrides Property effWidth() As Double
        Get
            Return Me.effw
        End Get
        Set(ByVal value As Double)
            For i As Integer = 0 To Me.numElements - 1
                Me.effWidth(i) = value
            Next
        End Set
    End Property
    Public Overridable Overloads Property effWidth(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).effWidth
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0 Then Return
            If idx < 0 And idx >= Me.numElements Then Return
            Me.elements(idx).effWidth = value
            If value < Me.effw Then Me.effw = value
            If value > Me.elements(idx).width Then
                If Me.w = Me.elements(idx).width Then
                    Me.elements(idx).width = value
                    Me.w = value
                    For j As Integer = 0 To Me.numElements - 1
                        If Me.elements(j).width < Me.w _
                                Then Me.w = Me.elements(j).width
                    Next
                Else
                    Me.elements(idx).width = value
                End If
            End If
        End Set
    End Property
    Public Overrides Property length() As Double
        Get
            Return Me.l
        End Get
        Set(ByVal value As Double)
            Me.l = value
            Dim x As Double = Me.l / Me.numElements
            For i As Integer = 0 To Me.numElements - 1
                Me.length(i) = x
            Next
        End Set
    End Property
    Public Overridable Overloads Property length(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).length
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0 Then Return
            If idx >= 0 And idx < Me.numElements Then
                Me.elements(idx).length = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property
    Public Overrides Property totalpop() As Double
        Get
            Me.pin = 0.0
            For i As Integer = 0 To Me.numElements - 1
                Me.pin = Me.pin + Me.elements(i).totalPop
            Next
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            If value < 0.0 Then Return
            Me.pin = value
            Dim x As Double = Me.pin / Me.numElements
            For i As Integer = 0 To Me.numElements - 1
                Me.totalpop(i) = x
            Next
        End Set
    End Property
    Public Overridable Overloads Property totalPop(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).totalPop
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0 Then Return
            If idx >= 0 And idx < Me.numElements Then
                Me.elements(idx).totalPop = value
            End If
        End Set
    End Property
    Public Overrides Property WaitPop() As Double
        Get
            Return Me.pw
        End Get
        Set(ByVal value As Double)
            If value < 0.0 Then Return
            Me.pw = value
            Me.WaitPop(Me.numElements - 1) = Me.pw
        End Set
    End Property
    Public Overridable Overloads Property WaitPop(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).WaitPop
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0 Then Return
            If idx >= 0 And idx < Me.numElements Then
                Me.elements(idx).WaitPop = value
            End If
        End Set
    End Property
    Public Overrides Property transitPop() As Double
        Get
            Return Me.pa
        End Get
        Set(ByVal value As Double)
            If value < 0.0 Then Return
            Me.pa = value
            Dim x As Double = Me.pa / Me.numElements
            For i As Integer = 0 To Me.numElements - 1
                Me.transitPop(i) = value
            Next
        End Set
    End Property
    Public Overridable Overloads Property transitPop(ByVal idx As Integer) As Double
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).transitPop
            Else
                Return -1
            End If
        End Get
        Set(ByVal value As Double)
            If value < 0 Then Return
            If idx >= 0 And idx < Me.numElements Then
                Me.elements(idx).transitPop = value
            End If
        End Set
    End Property
    Public Overridable ReadOnly Property NoElements()
        Get
            Return Me.numElements
        End Get
    End Property
    Public Overrides Property dtIndex() As Integer
        Get
            Return Me.dtIdx
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then
                Me.dtIdx = value
                Dim i As Integer
                For i = 0 To Me.numElements - 1
                    Me.elements(i).dtIndex = value
                Next
            End If
        End Set
    End Property
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoWaitPop
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoWaitPop
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTranPop
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoFlowOut
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoFlowOut(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoFlowOut
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTailDist
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoLeadDist
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            Return Me.elements(0).ExEqNoMaxFlowIn
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoMaxFlowIn
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property ExEqNoMaxFlowIn(ByVal elidx As Integer, ByVal conidx As Integer) As Integer
        Get
            If elidx >= 0 And elidx < Me.numElements Then
                Return Me.elements(elidx).ExEqNoMaxFlowIn(conidx)
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            Return Me.InEqNoWaitPop(Me.numElements - 1)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoWaitPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            Return Me.InEqNoTranPop(Me.numElements - 1)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            Return Me.InEqNoFlowIn(0)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoFlowIn
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            Return Me.InEqNoLeadDist(Me.numElements - 1)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).InEqNoTailDist
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            Return Me.InEqNoMaxFlowOut(Me.numElements - 1)
        End Get
    End Property
    Public Overridable Overloads ReadOnly Property InEqNoMaxFlowout(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            For i = 0 To idx - 1
                itot = itot + Me.elements(i).NumInputs
            Next
            Return itot + Me.elements(idx - 1).InEqNoMaxFlowOut
        End Get
    End Property
    Public Overrides Property nameExEqNo(ByVal idx As Integer) As String
        Get
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Return Me.elements(Me.eqnums(i, 0)).nameInEqNo(Me.eqnums(i, 1))
                End If
            Next
            Return ""
        End Get
        Set(ByVal value As String)
            Dim i As Integer
            For i = 0 To Me.eqno - 1
                If Me.eqnum(i) = idx Then
                    Me.elements(Me.eqnums(i, 0)).nameInEqNo(Me.eqnums(i, 1)) = value
                End If
            Next
        End Set
    End Property
    Public Overrides Property nameInEqNo(ByVal idx As Integer) As String
        Get
            If idx <= Me.eqno And idx >= 1 Then
                Return Me.elements(Me.eqnums(idx - 1, 0)).nameInEqNo(Me.eqnums(idx - 1, 1))
            End If
            Return ""
        End Get
        Set(ByVal value As String)
            If idx <= Me.eqno And idx >= 1 Then
                Me.elements(Me.eqnums(idx - 1, 0)).nameInEqNo(Me.eqnums(idx - 1, 1)) = value
            End If
        End Set
    End Property
    Public Overrides Property ElementName() As String
        Get
            Return Me.elName
        End Get
        Set(ByVal value As String)
            Me.elName = value
            Dim i, j As Integer
            For i = 0 To Me.numElements - 1
                j = i + 1
                Me.elements(i).ElementName = Me.elName + " " + j.ToString
            Next
        End Set
    End Property
    Public Overrides Property PDEElementName As String
        Get
            Dim s As String = Me.elements(0).PDEElementName
            For i As Integer = 1 To Me.numElements - 1
                s = s + "," + Me.elements(i).PDEElementName
            Next
            Return s
        End Get
        Set(ByVal value As String)
            MyBase.PDEElementName = value
        End Set
    End Property
    Public Overrides ReadOnly Property EgressElementNameListCSV() As String
        Get
            Dim i, j As Integer
            Dim s As String

            s = ""
            For i = 0 To Me.numElements - 1
                s = s + Me.elements(i).ElementName
                For j = 0 To Me.elements(i).NumEqs - 1
                    s = s + ","
                Next
            Next
            Return s
        End Get
    End Property
    Public Overrides Property EgressExitNum As Integer
        Get
            Return Me.elements(Me.numElements - 1).EgressExitNum
        End Get
        Set(ByVal value As Integer)
            Me.elements(Me.numElements - 1).EgressExitNum = value
        End Set
    End Property
    Public Overrides Property NextElement As EgressElement
        Get
            Return Me.elements(Me.numElements - 1).NextElement
        End Get
        Set(ByVal value As EgressElement)
            Me.elements(Me.numElements - 1).NextElement = value
        End Set
    End Property

    Public Overrides ReadOnly Property TotalEgress() As Double
        Get
            Me.iout = 0.0
            Me.iout = Me.elements(Me.numElements - 1).TotalEgress
            Return Me.iout
        End Get
    End Property

    Public Overrides ReadOnly Property TotalIn() As Double
        Get
            Me.hout = 0.0
            Me.hout = Me.elements(Me.numElements - 1).TotalIn
            Return Me.hout
        End Get
    End Property

    Public Overrides ReadOnly Property TotalOut() As Double
        Get
            Me.eout = Me.TotalEgress - Me.TotalIn
            Return Me.eout
        End Get
    End Property


    Public Sub New()
        Me.numElements = 0
        Me.numConnections = 0
        Me.eqno = 0
        Me.inno = 0
        Me.Angle = 0.0
        Me.l = 0.0
        Me.w = 0.0
        Me.effw = 0.0
        Me.pin = 0.0
        Me.pw = 0.0
        Me.pa = 0.0
    End Sub

    Public Sub New(ByVal numConnect As Integer, ByVal mergeType As Integer, ByVal numElements As Integer, _
                 ByVal length As Double, ByVal width As Double, ByVal widthtype As Integer, ByVal maxfin As Double, ByVal tmpangle As Double)
        Me.New(numConnect, 1, mergeType, numElements, length, width, widthtype, maxfin, tmpangle)
    End Sub

    Public Sub New(ByVal numConnect0 As Integer, ByVal numConnectLast As Integer, ByVal mergeType As Integer, ByVal numElements As Integer, _
                 ByVal length As Double, ByVal width As Double, ByVal widthtype As Integer, ByVal maxfin As Double, ByVal tmpangle As Double)
        Dim i As Integer

        Me.ready = False
        Me.errCode = ErrCodeNums.NoErrors
        Me.numElements = numElements
        Me.wtype = widthtype
        ReDim Me.elements(Me.numElements - 1)
        Me.eqno = 0
        Me.inno = 0
        If Me.numElements >= 1 Then
            Me.elements(0) = New EgressElement(numConnect0, mergeType, length, width, widthtype, maxfin, tmpangle)
            Me.element0 = Me.elements(0)
            Me.valid = Me.elements(0).IsValid
            If Me.numElements >= 2 And Me.valid Then
                If Me.numElements > 2 Then
                    For i = 1 To Me.numElements - 2
                        If Me.valid Then
                            Me.elements(i) = New EgressElement(length, width, widthtype, maxfin)
                            Me.valid = Me.elements(i).IsValid
                        End If
                    Next
                End If
                Me.elements(Me.numElements - 1) = New EgressElement(numConnectLast, mergeType, length, width, widthtype, maxfin, tmpangle)
                Me.elementLast = Me.elements(Me.numElements - 1)
                Me.valid = Me.elements(Me.numElements - 1).IsValid
            End If
        End If
        Me.valid = Me.SetVars()
        If Not Me.valid Then
            Me.errCode = ErrCodeNums.UnspecifiedErr
            Me.ready = False
            Me.numElements = 0
            ReDim Me.elements(0)
            Me.eqno = 0
            Me.inno = 0
            Me.elName = "Invalid EgressComposite"
            Return
        End If
        Me.elName = "EgressComposit"
        Me.ready = False

    End Sub
    Protected Function SetVars() As Boolean
        Dim i, j, k, l As Integer

        If Not Me.valid Then Return False
        Me.eqno = 0
        Me.inno = 0
        Me.l = 0.0
        Me.w = 1000.0
        Me.effw = Me.w
        For i = 0 To Me.numElements - 1
            Me.eqno = Me.eqno + Me.elements(i).NumEqs
            Me.inno = Me.inno + Me.elements(i).NumInputs
            Me.l = Me.l + Me.elements(i).length
            Me.w = Math.Min(Me.w, Me.elements(i).width)
            Me.effw = Math.Min(Me.effw, Me.elements(i).effWidth)
        Next

        ReDim Me.eqnum(Me.eqno - 1)
        ReDim Me.eqnums(Me.eqno - 1, 1)
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.inputnums(Me.inno - 1, 1)
        l = 0
        k = 0
        For i = 0 To Me.numElements - 1
            For j = 0 To Me.elements(i).NumEqs - 1
                Me.eqnums(l, 0) = i
                Me.eqnums(l, 1) = j + 1
                l = l + 1
            Next
            For j = 0 To Me.elements(i).NumInputs - 1
                Me.inputnums(k, 0) = i
                Me.inputnums(k, 1) = j + 1
                k = k + 1
            Next
        Next
        l = 1
        Me.seteqin(l)
        ReDim Me.names(Me.eqno - 1)
        Me.pin = 0.0
        Me.pw = 0.0
        Me.pa = 0.0
        Return True

    End Function

    Public Overridable Function PointerToElements(ByVal idx As Integer) As EgressElement
        If idx >= 0 And idx < Me.numElements Then
            Return Me.elements(idx)
        Else
            Return New EgressElement()
        End If
    End Function

    Public Overrides Sub seteqin(ByRef eqidx As Integer)
        Dim i As Integer

        If Not Me.valid Then
            Me.ready = False
            Return
        End If
        For i = 0 To Me.numElements - 1
            Me.elements(i).seteqin(eqidx)
            Me.ready = Me.elements(i).IsReady
            If Not Me.ready Then
                Me.errCode = Me.elements(i).ErrorCode
                Return
            End If
        Next
        For i = 1 To Me.numElements - 1
            If Not Me.elements(i).ConnectInFlow(1, Me.elements(i - 1)) Then
                Me.ready = False
                Return
            End If
        Next
        For i = 0 To Me.NumEqs - 1
            Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
        Next
        For i = 0 To Me.NumInputs - 1
            Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
        Next
        Me.ready = True

    End Sub

    Public Overrides Function InitilizeODEVecters(ByRef state As Vector, ByRef mask As Vector, _
                    ByRef min As Vector, ByRef max As Vector, ByRef minval As Vector, ByRef maxval As Vector, _
                    ByRef mincon As Vector, ByRef maxcon As Vector, ByRef mincoef As Vector, ByRef maxcoef As Vector)
        'ByRef signs As Vector) As Boolean

        If Not Me.ready Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        For i As Integer = 0 To Me.numElements - 1
            If Not Me.elements(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then Return False
        Next

        Return True

    End Function

    Public Overrides Function ConnectInFlow(ByVal conNumber As Integer, ByRef element As EgressElement) As Boolean
        Return Me.ConnectInFlow(0, conNumber, element)
    End Function

    Public Overridable Overloads Function ConnectInFlow(ByVal first As Integer, ByVal conNumber As Integer, ByRef element As EgressElement) As Boolean
        Dim i As Integer
        Dim flag As Boolean

        If first = 0 Then
            flag = Me.element0.ConnectInFlow(conNumber, element)
        Else
            flag = Me.elementLast.ConnectInFlow(conNumber, element)
        End If
        If flag Then
            For i = 0 To Me.NumEqs - 1
                Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
            Next
            For i = 0 To Me.NumInputs - 1
                Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
            Next
            Return True
        Else
            Return False
        End If
    End Function

    Public Overrides Sub mapEqNums(ByVal iNum As Integer, ByVal xNum As Integer)
        If iNum > 0 And iNum <= Me.eqno Then
            Me.eqnum(iNum - 1) = xNum
            Me.elements(Me.eqnums(iNum - 1, 0)).mapEqNums(Me.eqnums(iNum - 1, 1), xNum)
        End If
    End Sub

    Public Overrides Sub mapInputNums(ByVal iNum As Integer, ByVal xNum As Integer)
        If iNum > 0 And iNum <= Me.inno Then
            Me.inputnum(iNum - 1) = xNum
            Me.elements(Me.inputnums(iNum - 1, 0)).mapInputNums(Me.inputnums(iNum - 1, 1), xNum)
        End If
    End Sub

    Public Overrides Function Evaluate(ByRef Inputs As Vector) As Boolean
        Dim tmpin As Double = 0.0
        Dim tmpa As Double = 0.0

        For i As Integer = 0 To Me.numElements - 1
            If Not Me.elements(i).Evaluate(Inputs) Then
                Return False
            End If
            tmpin = tmpin + Me.totalpop(i)
            tmpa = tmpa + Me.transitPop(i)
        Next
        Me.pw = Me.WaitPop(Me.numElements - 1)
        Me.pin = tmpin
        Me.pa = tmpa
        Return True
    End Function

    Public Overrides Function GetEqVal(ByVal iNum As Integer) As Double
        If iNum > 0 And iNum <= Me.eqno Then
            Return Me.elements(Me.eqnums(iNum - 1, 0)).GetEqVal(Me.eqnums(iNum - 1, 1))
        Else
            Return -1.0
        End If
    End Function

    Public Overrides Function iToXEqNum(ByVal iNum As Integer) As Integer
        'Return Me.eqnum(iNum - 1)
        Return Me.elements(Me.eqnums(iNum - 1, 0)).iToXEqNum(Me.eqnums(iNum - 1, 1))
    End Function

    Public Overrides Function iToXInputNum(ByVal iNum As Integer) As Integer
        'Return Me.inputnum(iNum - 1)
        Return Me.elements(Me.inputnums(iNum - 1, 0)).iToXInputNum(Me.inputnums(iNum - 1, 1))
    End Function

    Public Overrides Function MakePopInteger(ByVal state As Matricies.Vector) As Matricies.Vector
        Dim i As Integer

        For i = 0 To Me.numElements - 1
            state = elements(i).MakePopInteger(state)
        Next
        Return state.clone
    End Function

    Public Overridable Function ElementTotPopNamesCSV() As String
        Dim s As String = ""
        For i As Integer = 1 To Me.numElements - 1
            s = s + Me.elements(i).ElementName + ","
        Next
        Return s
    End Function

    Public Overridable Function PDEElementTotPopNamesCSV() As String
        Dim s As String = ""
        For i As Integer = 1 To Me.numElements - 1
            s = s + Me.elements(i).PDEElementName + ","
        Next
        Return s
    End Function

    Public Overridable Function ElementTotPopsCSV() As String
        'The idea is to return a string without a comma before the first cell
        'or after the last. 
        Dim xtot As Double
        Dim iel, itot As Integer
        Dim s As String = ""

        For i As Integer = 0 To Me.numElements - 1
            xtot = xtot + Me.elements(i).totalPop
            iel = Math.Round(Me.elements(i).totalPop)
            itot = itot + iel
            If xtot - itot > 0.5 Then
                iel = iel + 1
                itot = itot + 1
            End If
            s = s + iel.ToString + ","
        Next
        Return s
    End Function

    Public Overridable Function PDEElementTotPopsCSV() As String
        'The idea is to return a string without a comma before the first cell
        Dim s As String = ""

        For i As Integer = 0 To Me.numElements - 1
            s = s + Me.elements(i).totalPop.ToString + ","
        Next
        Return s
    End Function

    Public Overrides Function FlowOut(ByVal istep As Integer) As Double
        Me.fout = Me.elements(Me.numElements - 1).FlowOut(istep)
        Return Me.fout
    End Function

    Public Shadows Function ErrMsg() As String
        Return Me.ErrMsg(Me.errCode)
    End Function

    Public Shadows Function ErrMsg(ByVal idx As Integer) As String
        If Me.setErrMsg Then
            Me.setErrMsg = False
            Me.SetErrMsgStr()
        End If
        If Me.errComposit Then
            Return "In " + Me.elName + " element: " + Me.errMsgStrs(idx)
        Else
            Return "In " + Me.elName + " element: " + Me.elements(Me.errElement).ErrMsg
        End If
    End Function

    Protected Function CalcLength() As Double
        Dim len As Double = 0.0
        For i As Integer = 0 To Me.numElements - 1
            len = len + Me.elements(i).length
        Next
    End Function
End Class

Public Class EgressStair
    Inherits EgressComposite

    Protected flights() As EgressFlight
    Protected landings() As EgressElement
    Protected numFlights As Integer
    Protected Shadows element0 As EgressFlight

    Public Overridable Property NumberStepsFlight(ByVal iflight As Integer) As Integer
        Get
            If iflight <= 0 Or iflight > Me.numFlights Then Return -1
            Return Me.flights(iflight - 1).NumberSteps
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then Return
            If iflight > 0 And iflight <= Me.numFlights Then
                Me.flights(iflight - 1).NumberSteps = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property
    Public Overridable Property RiserFlight(ByVal iflight As Integer) As Double
        Get
            If iflight <= 0 Or iflight > Me.numFlights Then Return -1
            Return Me.flights(iflight - 1).Riser
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            If iflight > 0 And iflight <= Me.numFlights Then
                Me.flights(iflight - 1).Riser = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property
    Public Overridable Property TreadFlight(ByVal iflight As Integer) As Double
        Get
            If iflight <= 0 Or iflight > Me.numFlights Then Return -1
            Return Me.flights(iflight - 1).Tread
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            If iflight > 0 And iflight <= Me.numFlights Then
                Me.flights(iflight - 1).Tread = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property
    Public Overridable Property LengthLanding(ByVal iflight As Integer) As Double
        Get
            If iflight <= 0 Or iflight > Me.numFlights Then Return -1
            Return Me.landings(iflight - 1).length
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            If iflight > 0 And iflight <= Me.numFlights Then
                Me.landings(iflight - 1).length = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property
    Public Overridable Property WidthLanding(ByVal iflight As Integer) As Double
        Get
            If iflight <= 0 Or iflight > Me.numFlights Then Return -1
            Return Me.landings(iflight - 1).width
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            If iflight > 0 And iflight <= Me.numFlights Then
                Me.landings(iflight - 1).width = value
                Me.l = Me.CalcLength()
            End If
        End Set
    End Property

    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoWaitPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoWaitPop
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTranPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoFlowOut
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoFlowOut
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoLeadDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            Return Me.elements(0).ExEqNoMaxFlowIn
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoMaxFlowIn
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal elidx As Integer, ByVal conidx As Integer) As Integer
        Get
            If elidx >= 0 And elidx < Me.numElements Then
                Return Me.elements(elidx).ExEqNoMaxFlowIn(conidx)
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            Return Me.InEqNoWaitPop(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoWaitPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            Return Me.InEqNoTranPop(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            Return Me.InEqNoFlowIn(0)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoFlowIn
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            Return Me.InEqNoLeadDist(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).InEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            Return Me.InEqNoMaxFlowOut(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowout(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            For i = 0 To idx - 1
                itot = itot + Me.elements(i).NumInputs
            Next
            Return itot + Me.elements(idx - 1).InEqNoMaxFlowOut
        End Get
    End Property
    'End of properites included because Visual Basic errors

    Public Overrides Property ElementName() As String
        Get
            Return Me.elName
        End Get
        Set(ByVal value As String)
            Me.elName = value
            For i As Integer = 1 To Me.numFlights
                Me.flights(i - 1).ElementName = Me.elName + " flight " + i.ToString
                Me.landings(i - 1).ElementName = Me.elName + " landing " + i.ToString
            Next
        End Set
    End Property

    Public Sub New()
        MyBase.New()
    End Sub

    Public Sub New(ByVal numConnect As Integer, ByVal mergeType As Integer, ByVal numFlights As Integer, ByVal rsr() As Double, ByVal trd() As Double, ByVal numsteps() As Integer, _
                 ByVal stairwidth() As Double, ByVal landlength() As Double, ByVal landwidth() As Double, ByVal widthtype() As Integer, _
                 ByVal maxfin As Double)

        Me.New(numConnect, mergeType, numFlights, rsr(0), trd(0), numsteps(0), stairwidth(0), landlength(0), landwidth(0), widthtype(0), maxfin)

        Dim i As Integer

        For i = 0 To Me.numFlights - 1
            Me.flights(i).redefine(rsr(i), trd(i), stairwidth(i), widthtype(i), maxfin)
            Me.landings(i).Redefine(landlength(i), landwidth(i), widthtype(i), maxfin, 0.0)
        Next

    End Sub

    Public Sub New(ByVal numConnect As Integer, ByVal mergeType As Integer, ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, ByVal numSteps As Integer, _
                 ByVal stairwidth As Double, ByVal landlength As Double, ByVal landwidth As Double, ByVal widthtype As Integer, _
                 ByVal maxfin As Double)

        Dim iele As Integer = 0

        If numFlights < 1 Then
            Me.valid = False
            Return
        End If
        Me.numConnections = numConnect
        ReDim Me.conns(Me.numConnections - 1)
        Me.numFlights = numFlights
        Me.numElements = 2 * Me.numFlights
        ReDim Me.flights(numFlights - 1)
        ReDim Me.landings(numFlights - 1)
        ReDim Me.elements(Me.numElements - 1)
        Dim strmaxfin As Double = stairwidth * Me.spdK / 0.5588
        For i As Integer = Me.numFlights - 1 To 0 Step -1
            If Me.valid Then
                Me.flights(i) = New EgressFlight(1, mergeType, rsr, trd, numSteps, stairwidth, widthtype, strmaxfin)
                If Me.flights(i).IsValid Then
                    Me.elements(iele) = Me.flights(i)
                    iele = iele + 1
                Else
                    Me.valid = False
                End If
                landlength = (stairwidth * Math.PI) / 2.0 + 0.1
                'landwidth = Math.Max(0.3, stairwidth - 0.3)
                landwidth = (Math.PI * ((stairwidth - 0.15) ^ 2 - 0.15 ^ 2) / 2 + 0.1 * (stairwidth - 0.3)) / landlength
                Me.landings(i) = New EgressElement(1, EgressElement.MergeType.Interleve, landlength, landwidth, widthtype, strmaxfin, 0.0)
                If Me.flights(i).IsValid Then
                    Me.elements(iele) = Me.landings(i)
                    iele = iele + 1
                Else
                    Me.valid = False
                End If
            End If
        Next
        If Me.valid Then
            Me.element0 = Me.flights(Me.numFlights - 1)
            Dim maxfin_a(numConnect - 1) As Double
            maxfin_a(numConnect - 1) = maxfin
            For i As Integer = 0 To numConnect - 2
                maxfin_a(i) = strmaxfin
            Next
            Me.elementLast = New EgressElement(numConnect, mergeType, landlength, landwidth, widthtype, maxfin_a, 0.0)
            Me.landings(0) = Me.elementLast
            Me.elements(Me.numElements - 1) = Me.elementLast
            If Me.elementLast.IsValid Then
                Me.elements(Me.numElements - 1) = Me.elementLast
            Else
                Me.valid = False
            End If
        Else
            Return
        End If
        Me.valid = Me.SetVars
        If Not Me.valid Then
            Me.numFlights = 0
            Me.numElements = 0
            Me.l = 0
            Me.w = 0
            Me.effw = 0
            Me.eqno = 0
            Me.inno = 0
            Me.elName = "Invalide Stair"
            Return
        End If
        Me.elName = "EgressStair"
        Me.ready = False

    End Sub

    Public Overridable Function ConnectStairs(ByRef element As EgressStair) As Boolean
        Return Me.ConnectInFlow(0, 1, element.elementLast)
    End Function

    Public Overridable Function ConnectFloor(ByRef element As EgressElement) As Boolean
        Return Me.ConnectInFlow(1, 2, element)
    End Function

    Public Overloads Function ConnectInFlow(ByVal first As Integer, ByVal conNumber As Integer, ByRef element As EgressElement) As Boolean
        Dim i As Integer
        Dim flag As Boolean

        If first = 0 Then
            flag = Me.element0.ConnectInFlow(conNumber, element)
        Else
            flag = Me.elementLast.ConnectInFlow(conNumber, element)
        End If
        If flag Then
            For i = 0 To Me.NumEqs - 1
                Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
            Next
            For i = 0 To Me.NumInputs - 1
                Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
            Next
            Return True
        Else
            Return False
        End If
    End Function
    ' routines for the PDE solution
    Public Overrides ReadOnly Property HeadD(ByVal istep As Integer) As Double
        Get
            Return Me.elements(0).HeadD(istep)
        End Get
    End Property

    Public Overrides ReadOnly Property HeadSpd(ByVal istep As Integer) As Double
        Get
            Return Me.elements(0).HeadSpd(istep)
        End Get
    End Property
    Public Overrides ReadOnly Property TailD(ByVal istep As Integer) As Double
        Get
            Return Me.elements(Me.numElements - 1).TailD(istep)
        End Get
    End Property

    Public Overrides ReadOnly Property TailSpd(ByVal istep As Integer) As Double
        Get
            Return Me.elements(Me.numElements - 1).TailSpd(istep)
        End Get
    End Property

    Public Overrides Property NextElement As EgressElement
        Get
            Return Me.elements(Me.numElements - 1).NextElement
        End Get
        Set(ByVal value As EgressElement)
            Me.elements(Me.numElements - 1).NextElement = value
        End Set
    End Property


    Public Overrides Function RK4step0(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step0(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4step1or2(ByVal istep As Integer, ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step1or2(istep, curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4step3(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step3(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4Complete(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4Complete(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function
End Class

Public Class EgressStairWay
    Inherits EgressComposite

    Protected Stairs() As EgressStair
    Protected StairElements() As Integer
    Protected Halls() As EgressElement
    Protected Rooms() As EgressSource
    Protected numFloors, numHalls, numRooms, numStairs, numStairElements As Integer
    Protected baseFloor As Integer
    Protected hName As String = "hall"
    Protected rName As String = "room"
    Protected sName As String = "stair"
    Protected fName As String = "floor"
    Protected Enum tPtr 'pointer to type of element
        s  'Stair
        h  'Hall
        r  'Room
        maxTypes 'maximum number of types
    End Enum

    Public Overrides Property length() As Double
        Get
            Return Me.l
        End Get
        Set(ByVal value As Double)
            Me.l = value
            Dim x As Double = Me.l / Me.numStairs
            For i As Integer = 0 To Me.numStairs - 1
                Me.Stairs(i).length = x
            Next
        End Set
    End Property
    Public Overridable Property LengthHallFloor(ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Rooms(iflr).length
        End Get
        Set(ByVal value As Double)
            If value <= 0.0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Rooms(iflr).length = value
        End Set
    End Property
    Public Overridable Property WidthHallFloor(ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Rooms(iflr).width
        End Get
        Set(ByVal value As Double)
            If value <= 0.0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Rooms(iflr).width = value
        End Set
    End Property
    Public Overridable Property NumberStepsFlightFloor(ByVal iflight As Integer, ByVal ifloor As Integer) As Integer
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Stairs(iflr).NumberStepsFlight(iflight)
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Stairs(iflr).NumberStepsFlight(iflight) = value
        End Set
    End Property
    Public Overridable Property RiserFlightFloor(ByVal iflight As Integer, ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Stairs(iflr - 1).RiserFlight(iflight)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Stairs(iflr).RiserFlight(iflr) = value
        End Set
    End Property
    Public Overridable Property TreadFlightFloor(ByVal iflight As Integer, ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Stairs(iflr - 1).TreadFlight(iflight)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Stairs(iflr).TreadFlight(iflr) = value
        End Set
    End Property
    Public Overridable Property LengthLandingFloor(ByVal iflight As Integer, ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Stairs(iflr - 1).LengthLanding(iflight)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Stairs(iflr).LengthLanding(iflr) = value
        End Set
    End Property
    Public Overridable Property WidthLandingFloor(ByVal iflight As Integer, ByVal ifloor As Integer) As Double
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Stairs(iflr - 1).WidthLanding(iflight)
        End Get
        Set(ByVal value As Double)
            If value > 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Stairs(iflr).WidthLanding(iflr) = value
        End Set
    End Property
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoWaitPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoWaitPop
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTranPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoFlowOut
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoFlowOut
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).ExEqNoLeadDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            Return Me.elements(0).ExEqNoMaxFlowIn
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoMaxFlowIn
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal elidx As Integer, ByVal conidx As Integer) As Integer
        Get
            If elidx >= 0 And elidx < Me.numElements Then
                Return Me.elements(elidx).ExEqNoMaxFlowIn(conidx)
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            Return Me.InEqNoWaitPop(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoWaitPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            Return Me.InEqNoTranPop(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            Return Me.InEqNoFlowIn(0)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoFlowIn
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            Return Me.InEqNoLeadDist(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            Return Me.elements(Me.numElements - 1).InEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            Return Me.InEqNoMaxFlowOut(Me.numElements - 1)
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowout(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            For i = 0 To idx - 1
                itot = itot + Me.elements(i).NumInputs
            Next
            Return itot + Me.elements(idx - 1).InEqNoMaxFlowOut
        End Get
    End Property
    'End of properites included because Visual Basic errors
    Public Overridable Property RoomName() As String
        Get
            Return Me.rName
        End Get
        Set(ByVal value As String)
            Me.rName = value
        End Set
    End Property
    Public Overridable Property HallName() As String
        Get
            Return Me.sName
        End Get
        Set(ByVal value As String)
            Me.sName = value
        End Set
    End Property
    Public Overridable Property StairName() As String
        Get
            Return Me.sName
        End Get
        Set(ByVal value As String)
            Me.sName = value
        End Set
    End Property
    Public Overridable Property FloorName() As String
        Get
            Return Me.fName
        End Get
        Set(ByVal value As String)
            Me.fName = value
        End Set
    End Property
    Public Overrides Property ElementName() As String
        Get
            Return MyBase.ElementName
        End Get
        Set(ByVal value As String)
            Dim iflr, tmp As Integer
            Dim s As String

            Me.elName = value
            For i As Integer = 0 To Me.numFloors - 1
                iflr = i + Me.baseFloor
                Math.DivRem(iflr, 10, tmp)
                If tmp = 1 Then
                    s = " " + iflr.ToString + "st " + Me.fName + " "
                ElseIf tmp = 2 Then
                    s = " " + iflr.ToString + "nd " + Me.fName + " "
                ElseIf tmp = 3 Then
                    s = " " + iflr.ToString + "rd " + Me.fName + " "
                Else
                    s = " " + iflr.ToString + "th " + Me.fName + " "
                End If
                Me.Rooms(i).ElementName = Me.elName + s + Me.rName
                Me.Halls(i).ElementName = Me.elName + s + Me.hName
                Me.Stairs(i).ElementName = Me.elName + s + Me.sName
            Next
        End Set
    End Property
    Public Overridable Property RoomPop(ByVal ifloor As Integer) As Integer
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Rooms(iflr).totalPop
        End Get
        Set(ByVal value As Integer)
            If value < 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Rooms(iflr).totalPop = value
        End Set
    End Property
    Public Overridable Property RoomDelay(ByVal ifloor As Integer) As Integer
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Rooms(iflr).Delay
        End Get
        Set(ByVal value As Integer)
            If value < 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Rooms(iflr).Delay = value
        End Set
    End Property

    Public Sub New(ByVal numFloors As Integer, ByVal numConnect As Integer, ByVal mergeType As Integer, ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, ByVal numsteps As Integer, _
                     ByVal stairwidth As Double, ByVal landlength As Double, ByVal landwidth As Double, ByVal widthtype As Integer, _
                     ByVal maxfin As Double, ByVal hallLength As Double, ByVal hallWidth As Double, ByVal hallwidthtype As Integer, ByVal hallmaxfin As Double, ByVal tmpangle As Double, ByVal delay As Double)

        Me.ready = False
        Me.errCode = ErrCodeNums.NoErrors
        Me.numFloors = numFloors
        Me.numStairs = Me.numFloors
        ReDim Me.Stairs(Me.numStairs - 1)
        Me.baseFloor = 1
        Me.numStairElements = Me.numStairs * 2 * numFlights
        Me.numHalls = Me.numFloors
        ReDim Me.Halls(Me.numHalls - 1)
        Me.numRooms = Me.numFloors
        ReDim Me.Rooms(Me.numRooms - 1)
        Me.numElements = Me.numStairElements + Me.numHalls + Me.numRooms
        ReDim Me.elements(Me.numElements - 1)

        Dim i, l, j, k, elidx, sidx, hidx, ridx As Integer
        elidx = 0
        hidx = 0
        ridx = 0
        sidx = 0

        Me.l = 0.0
        Me.w = 10000.0
        Me.effw = 10000.0
        Me.eqno = 0
        Me.inno = 0
        For i = Me.numFloors - 1 To 0 Step -1
            Me.Rooms(i) = New EgressSource(delay)
            Me.eqno = Me.eqno + Me.Rooms(i).NumEqs
            Me.inno = Me.inno + Me.Rooms(i).NumInputs
            Me.elements(elidx) = Me.Rooms(i)
            elidx = elidx + 1
            Me.Halls(i) = New EgressElement(1, EgressElement.MergeType.Interleve, hallLength, hallWidth, hallwidthtype, hallmaxfin, tmpangle)
            If Not Me.Halls(i).IsValid Then Me.valid = False
            Me.eqno = Me.eqno + Me.Halls(i).NumEqs
            Me.inno = Me.inno + Me.Halls(i).NumInputs
            Me.elements(elidx) = Me.Halls(i)
            elidx = elidx + 1
            Me.Stairs(i) = New EgressStair(2, mergeType, numFlights, rsr, trd, numsteps, stairwidth, landlength, landwidth, widthtype, maxfin)
            If Not Me.Stairs(i).IsValid Then Me.valid = False
            Me.eqno = Me.eqno + Me.Stairs(i).NumEqs
            Me.inno = Me.inno + Me.Stairs(i).NumInputs
            For j = 0 To Me.Stairs(i).NoElements - 1
                Me.elements(elidx) = Me.Stairs(i).PointerToElements(j)
                elidx = elidx + 1
            Next
            Me.l = Me.l + Me.Stairs(i).length
            Me.w = Math.Min(Me.w, Me.Stairs(i).width)
            Me.effw = Math.Min(Me.effw, Me.Stairs(i).effWidth)
        Next
        If Not Me.valid Then
            Me.InvalidateElement()
            Return
        End If
        ReDim Me.eqnum(Me.eqno - 1)
        ReDim Me.eqnums(Me.eqno - 1, 1)
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.inputnums(Me.inno - 1, 1)
        l = 0
        k = 0
        For i = 0 To Me.numElements - 1
            For j = 0 To Me.elements(i).NumEqs - 1
                Me.eqnums(l, 0) = i
                Me.eqnums(l, 1) = j + 1
                l = l + 1
            Next
            For j = 0 To Me.elements(i).NumInputs - 1
                Me.inputnums(k, 0) = i
                Me.inputnums(k, 1) = j + 1
                k = k + 1
            Next
        Next
        l = 0
        Me.seteqin(l)
        If Not Me.ready Then
            Me.InvalidateElement()
            Return
        End If
        ReDim Me.names(Me.eqno - 1)
        Me.ElementName = "EgressStairWay"
        Me.wtype = widthtype
        Me.finmax(0) = maxfin
        Me.pin = 0.0
        Me.pw = 0.0
        Me.pa = 0.0

    End Sub
    Protected Overrides Sub InvalidateElement()
        Me.ready = False
        Me.valid = False
        Me.errCode = ErrCodeNums.UnspecifiedErr
        Me.numFloors = 0
        Me.numStairs = 0
        ReDim Me.Stairs(0)
        Me.baseFloor = 0
        Me.numStairElements = 0
        Me.numHalls = 0
        ReDim Me.Halls(0)
        Me.numRooms = 0
        ReDim Me.Rooms(0)
        Me.numElements = 0
        ReDim Me.elements(0)
        Me.l = 0.0
        Me.w = 0.0
        Me.effw = 0.0
        Me.finmax(0) = 0.0
        Me.eqno = 0
        Me.inno = 0
        Me.elName = "Invalide EgressStairWay"
        Return
    End Sub

    Public Overrides Sub seteqin(ByRef eqidx As Integer)
        Dim i As Integer

        If Not Me.valid Then Return
        For i = Me.numFloors - 1 To 0 Step -1
            Me.Rooms(i).seteqin(eqidx)
            If Not Me.Rooms(i).IsReady Then
                Me.errCode = ErrCodeNums.StairWaySeteqninRoomSeteqnin
                MsgBox("error EgressStairWay Room seteqin " + i.ToString)
                Return
            End If
            Me.Halls(i).seteqin(eqidx)
            If Not Me.Halls(i).IsReady Then
                Me.errCode = ErrCodeNums.StairWaySeteqninHallSeteqnin
                MsgBox("error EgressStairWay Hall seteqin " + i.ToString)
                Return
            End If
            Me.Stairs(i).seteqin(eqidx)
            If Not Me.Stairs(i).IsReady Then
                Me.errCode = ErrCodeNums.StairWaySeteqninStairSeteqnin
                MsgBox("error EgressStairWay Stair seteqin " + i.ToString)
                Return
            End If
        Next
        If Not Me.Halls(Me.numFloors - 1).ConnectInFlow(1, Me.Rooms(Me.numFloors - 1)) Then
            Me.errCode = ErrCodeNums.UnspecifiedErr
            MsgBox("error EgressStairWay Halls.ConnectInFlow ")
            Return
        End If
        If Not Me.Stairs(Me.numFloors - 1).ConnectFloor(Me.Halls(Me.numFloors - 1)) Then
            Me.errCode = ErrCodeNums.UnspecifiedErr
            MsgBox("erro EgressStairWay Stairs.ConnectFloor ")
            Return
        End If
        For i = Me.numFloors - 2 To 0 Step -1
            If Not Me.Stairs(i).ConnectStairs(Me.Stairs(i + 1)) Then
                Me.errCode = ErrCodeNums.UnspecifiedErr
                MsgBox("erro EgressStairWay Stairs.ConnectStairs ")
                Return
            End If
            If Not Me.Halls(i).ConnectInFlow(1, Me.Rooms(i)) Then
                Me.errCode = ErrCodeNums.UnspecifiedErr
                MsgBox("erro EgressStairWay Halls.ConnectInFlow ")
                Return
            End If
            If Not Me.Stairs(i).ConnectFloor(Me.Halls(i)) Then
                Me.errCode = ErrCodeNums.UnspecifiedErr
                MsgBox("erro EgressStairWay Stairs.ConnectFloor ")
                Return
            End If
        Next
        For i = 0 To Me.NumEqs - 1
            Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
        Next
        For i = 0 To Me.NumInputs - 1
            Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
        Next
        Me.ready = True

    End Sub

    Public Overrides Function ConnectInFlow(ByVal con As Integer, ByRef element As EgressElement) As Boolean
        Dim i As Integer

        If Me.Stairs(Me.numFloors - 1).ConnectInFlow(con, element) Then
            For i = 0 To Me.NumEqs - 1
                Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
            Next
            For i = 0 To Me.NumInputs - 1
                Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
            Next
            Return True
        Else
            Return False
        End If
    End Function

    Public Overrides Function ElementTotPopNamesCSV() As String
        Dim s As String = ""

        For i As Integer = Me.numFloors - 1 To 0 Step -1
            s = s + Me.Rooms(i).ElementName + "," _
                  + Me.Halls(i).ElementName + "," _
                  + Me.Stairs(i).ElementName + ","
        Next
        Return s
    End Function

    Public Overrides Function ElementTotPopsCSV() As String
        Return Me.elementtotpopscsv(1)
    End Function

    Public Overloads Function ElementTotPopsCSV(ByVal numStairs As Integer) As String
        Dim s As String
        Dim xtot As Double
        Dim iel, itot As Integer

        xtot = 0.0
        s = ""
        For i As Integer = Me.numFloors - 1 To 0 Step -1
            xtot = xtot + numStairs * Me.Rooms(i).totalPop
            iel = numStairs * Math.Round(Me.Rooms(i).totalPop)
            itot = itot + iel
            s = s + iel.ToString + ","
            xtot = xtot + numStairs * Me.Halls(i).totalPop
            iel = numStairs * Math.Round(Me.Halls(i).totalPop)
            itot = itot + iel
            s = s + iel.ToString + ","
            xtot = xtot + numStairs * Me.Stairs(i).totalpop
            iel = iel + numStairs * Math.Round(Me.Stairs(i).totalpop)
            itot = itot + iel
            If Math.Round(xtot) - itot <> 0 Then
                iel = Math.Round(xtot) - (itot - iel)
                itot = Math.Round(xtot)
            End If
            s = s + iel.ToString + ","
        Next
        Return s
    End Function

    ' routines for the PDE solution
    Public Overrides ReadOnly Property HeadD(ByVal istep As Integer) As Double
        Get
            Return Me.elements(0).HeadD(istep)
        End Get
    End Property

    Public Overrides ReadOnly Property HeadSpd(ByVal istep As Integer) As Double
        Get
            Return Me.elements(0).HeadSpd(istep)
        End Get
    End Property
    Public Overrides ReadOnly Property TailD(ByVal istep As Integer) As Double
        Get
            Return Me.elements(Me.numElements - 1).TailD(istep)
        End Get
    End Property

    Public Overrides ReadOnly Property TailSpd(ByVal istep As Integer) As Double
        Get
            Return Me.elements(Me.numElements - 1).TailSpd(istep)
        End Get
    End Property

    Public Overrides Property NextElement As EgressElement
        Get
            Return Me.elements(Me.numElements - 1).NextElement
        End Get
        Set(ByVal value As EgressElement)
            Me.elements(Me.numElements - 1).NextElement = value
        End Set
    End Property

    Public Overrides Function RK4step0(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step0(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4step1or2(ByVal istep As Integer, ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step1or2(istep, curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4step3(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4step3(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function

    Public Overrides Function RK4Complete(ByVal curtime As Double, ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        For idx As Integer = 0 To Me.numElements - 1
            If Not Me.elements(idx).RK4Complete(curtime, dt) Then
                Return False
            End If
        Next
        Return flg
    End Function
End Class

Public Class EgressKloteElevatorBank
    Inherits EgressComposite

    Protected El() As EgressElevator
    Protected numCars As Integer
    Protected Lobbies() As EgressElLobby
    Protected numFloors, numLobbies As Integer
    Protected baseFloor As Integer
    Protected lName As String = "lobby"
    Protected eName As String = "elevator"
    Protected fName As String = "floor"
    ' for elevator calculation for PDE
    Protected Out As EgressElement
    Protected timeFlag As Double

    Protected Enum tPtr 'pointer to type of element
        l  'Lobby
        e  'Elevator
        maxTypes 'maximum number of types
    End Enum

    Public Overridable ReadOnly Property timeToFloor() As Double
        Get
            Return Me.El(0).timeToFloor
        End Get
    End Property
    Public Overridable ReadOnly Property ExEqNoTrvlTime() As Integer
        Get
            Return Me.El(0).ExEqNoTrvlTime
        End Get
    End Property
    Public Overridable ReadOnly Property NumberCars() As Integer
        Get
            Return Me.numCars
        End Get
    End Property
    'The following properties should not have to be included but 
    'Visual Basic in debug mode seems to lose track of the enumerated 
    'properties it should be using. 
    Public Overrides ReadOnly Property ExEqNoWaitPop() As Integer
        Get
            Return Me.El(0).ExEqNoWaitPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoWaitPop
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop() As Integer
        Get
            Return Me.El(0).ExEqNoTranPop
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut() As Integer
        Get
            Return Me.El(0).ExEqNoFlowOut
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoFlowOut(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoFlowOut
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist() As Integer
        Get
            Return Me.El(0).ExEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist() As Integer
        Get
            Return Me.El(0).ExEqNoLeadDist
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn() As Integer
        Get
            Return -1
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal idx As Integer) As Integer
        Get
            If idx >= 0 And idx < Me.numElements Then
                Return Me.elements(idx).ExEqNoMaxFlowIn
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property ExEqNoMaxFlowIn(ByVal elidx As Integer, ByVal conidx As Integer) As Integer
        Get
            If elidx >= 0 And elidx < Me.numElements Then
                Return Me.elements(elidx).ExEqNoMaxFlowIn(conidx)
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop() As Integer
        Get
            Return Me.El(0).InEqNoFlowIn
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoWaitPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoWaitPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop() As Integer
        Get
            Return Me.El(0).InEqNoTranPop
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTranPop(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoTranPop
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn() As Integer
        Get
            Return Me.El(0).InEqNoFlowIn
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoFlowIn(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumEqs
                Next
                Return itot + Me.elements(idx).InEqNoFlowIn
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist() As Integer
        Get
            Return Me.El(0).InEqNoLeadDist
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoLeadDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoLeadDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist() As Integer
        Get
            Return Me.El(0).InEqNoTailDist
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoTailDist(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            If idx >= 0 And idx < Me.numElements Then
                For i = 0 To idx - 1
                    itot = itot + Me.elements(i).NumInputs
                Next
                Return itot + Me.elements(idx).InEqNoTailDist
            Else
                Return -1
            End If
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowOut() As Integer
        Get
            Return Me.El(0).InEqNoMaxFlowOut
        End Get
    End Property
    Public Overrides ReadOnly Property InEqNoMaxFlowout(ByVal idx As Integer) As Integer
        Get
            Dim i, itot As Integer

            itot = 0
            For i = 0 To idx - 1
                itot = itot + Me.elements(i).NumInputs
            Next
            Return itot + Me.elements(idx - 1).InEqNoMaxFlowOut
        End Get
    End Property
    'End of properites included because Visual Basic errors
    Public Overrides Property totalpop() As Double
        Get
            Me.pin = 0.0
            For i As Integer = 0 To Me.numElements - 1
                Me.pin = Me.pin + Me.elements(i).totalPop
            Next
            Return Me.pin
        End Get
        Set(ByVal value As Double)
            If value < 0.0 Then Return
            Me.pin = value
            Dim x As Double = Me.pin / Me.numElements
            For i As Integer = 0 To Me.numElements - 1
                Me.totalpop(i) = x
            Next
        End Set
    End Property

    Public Overridable Property ElevatorName() As String
        Get
            Return Me.eName
        End Get
        Set(ByVal value As String)
            Me.eName = value
        End Set
    End Property
    Public Overridable Property FloorName() As String
        Get
            Return Me.fName
        End Get
        Set(ByVal value As String)
            Me.fName = value
        End Set
    End Property
    Public Overrides Property ElementName() As String
        Get
            Return MyBase.ElementName
        End Get
        Set(ByVal value As String)
            Dim iflr, tmp As Integer
            Dim s As String

            Me.elName = value
            For iflr = 0 To Me.numCars - 1
                Me.El(iflr).ElementName = Me.ElementName + " " + Me.eName + " Car " + (iflr + 1).ToString
            Next
            For i As Integer = 0 To Me.numFloors - 2
                iflr = i + Me.baseFloor
                Math.DivRem(iflr, 10, tmp)
                If tmp = 1 Then
                    s = " " + iflr.ToString + "st " + Me.fName + " "
                ElseIf tmp = 2 Then
                    s = " " + iflr.ToString + "nd " + Me.fName + " "
                ElseIf tmp = 3 Then
                    s = " " + iflr.ToString + "rd " + Me.fName + " "
                Else
                    s = " " + iflr.ToString + "th " + Me.fName + " "
                End If
                Me.Lobbies(i).ElementName = Me.elName + s + Me.lName
            Next
        End Set
    End Property
    Public Overridable Property LobbyPop(ByVal ifloor As Integer) As Integer
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Lobbies(iflr).totalPop
        End Get
        Set(ByVal value As Integer)
            If value < 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Lobbies(iflr).totalPop = value
        End Set
    End Property
    Public Overridable Property LobbyDelay(ByVal ifloor As Integer) As Integer
        Get
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return -1
            Return Me.Lobbies(iflr).Delay
        End Get
        Set(ByVal value As Integer)
            If value < 0 Then Return
            Dim iflr As Integer = ifloor - Me.baseFloor
            If iflr < 0 Or iflr >= Me.numFloors Then Return
            Me.Lobbies(iflr).Delay = value
        End Set
    End Property
    Public Overrides ReadOnly Property EgressElementNameListCSV() As String
        Get
            Dim i, j As Integer
            Dim s As String

            s = ""
            For i = 0 To Me.numElements - 1
                s = s + Me.elements(i).ElementName
                For j = 0 To Me.elements(i).NumEqs - 1
                    s = s + ","
                Next
            Next
            Return s
        End Get
    End Property

    Public Overrides ReadOnly Property TotalEgress() As Double
        Get
            Me.iout = 0.0
            For i As Integer = 0 To Me.numLobbies - 1
                Me.iout = Me.iout + Me.Lobbies(i).TotalEgress
            Next
            Return Me.iout
        End Get
    End Property

    Public Overrides ReadOnly Property TotalIn() As Double
        Get
            Me.hout = 0.0
            For i As Integer = 0 To Me.numLobbies - 1
                Me.hout = Me.hout + Me.Lobbies(i).totalPop
            Next
            Return Me.hout
            For i As Integer = 0 To Me.numCars - 1
                Me.hout = Me.hout + Me.El(i).totalPop
            Next
        End Get
    End Property

    Public Overrides ReadOnly Property TotalOut() As Double
        Get
            Me.eout = Me.Out.totalPop
            Return Me.eout
        End Get
    End Property

    Public Sub New(ByVal numFloors As Integer, ByVal DoorType As Integer, ByVal speed As Double, _
                    ByVal accel As Double, ByVal flrhigh As Double, ByVal numCars As Integer, _
                    ByVal maxcarcap As Double, ByVal elmaxfin As Double, ByVal pop As Integer, _
                    ByVal recallDelay As Double, ByVal lobbyDelay As Double)

        Me.ready = False
        Me.errCode = ErrCodeNums.NoErrors
        Me.numFloors = numFloors
        Me.baseFloor = 2
        Me.numLobbies = Me.numFloors - 1
        ReDim Me.Lobbies(Me.numLobbies - 1)
        Me.numCars = numCars
        ReDim Me.El(Me.numCars - 1)
        'Me.numElements = Me.numLobbies + Me.numCars + 1
        Me.numElements = Me.numLobbies + Me.numCars
        ReDim Me.elements(Me.numElements - 1)

        Dim i, l, j, k, elidx, lidx, hidx, ridx As Integer
        elidx = 0
        hidx = 0
        ridx = 0
        lidx = 0

        Me.l = 0.0
        Me.w = 10000.0
        Me.effw = 10000.0
        Me.eqno = 0
        Me.inno = 0
        For i = 0 To Me.numCars - 1
            Me.El(i) = New EgressElevator(numFloors, DoorType, speed, accel, flrhigh, 1, maxcarcap, elmaxfin, recallDelay)
            If Not Me.El(i).IsValid Then Me.valid = False
            Me.eqno = Me.eqno + Me.El(i).NumEqs
            Me.inno = Me.inno + Me.El(i).NumInputs
            Me.elements(elidx) = Me.El(i)
            elidx = elidx + 1
        Next
        For i = Me.numFloors - 2 To 0 Step -1
            Me.Lobbies(i) = New EgressElLobby(lobbyDelay)
            Me.Lobbies(i).totalPop = pop
            Me.Lobbies(i).Floor = i + 2
            If Not Me.Lobbies(i).IsValid Then Me.valid = False
            Me.eqno = Me.eqno + Me.Lobbies(i).NumEqs
            Me.inno = Me.inno + Me.Lobbies(i).NumInputs
            Me.elements(elidx) = Me.Lobbies(i)
            Me.Lobbies(i).CallButton(lobbyDelay) = True
            elidx = elidx + 1
        Next
        Me.Out = New EgressElSink()
        'Me.eqno = Me.eqno + Me.Out.NumEqs
        'Me.inno = Me.inno + Me.Out.NumInputs
        'Me.elements(elidx) = Me.Out
        'elidx = elidx + 1
        Me.timeFlag = 0.0
        If Not Me.valid Then
            Me.InvalidateElement()
            Return
        End If
        ReDim Me.eqnum(Me.eqno - 1)
        ReDim Me.eqnums(Me.eqno - 1, 1)
        ReDim Me.eqResults(Me.eqno - 1)
        ReDim Me.inputnum(Me.inno - 1)
        ReDim Me.inputnums(Me.inno - 1, 1)
        l = 0
        k = 0
        For i = 0 To Me.numElements - 1
            For j = 0 To Me.elements(i).NumEqs - 1
                Me.eqnums(l, 0) = i
                Me.eqnums(l, 1) = j + 1
                l = l + 1
            Next
            For j = 0 To Me.elements(i).NumInputs - 1
                Me.inputnums(k, 0) = i
                Me.inputnums(k, 1) = j + 1
                k = k + 1
            Next
        Next
        l = 0
        Me.seteqin(l)
        If Not Me.ready Then
            Me.InvalidateElement()
            Return
        End If
        ReDim Me.names(Me.eqno - 1)
        Me.ElementName = "EgressElevatorPath"
        Me.finmax(0) = elmaxfin
        Me.pin = 0.0
        Me.pw = 0.0
        Me.pa = 0.0

    End Sub
    Protected Overrides Sub InvalidateElement()
        Me.ready = False
        Me.valid = False
        Me.errCode = ErrCodeNums.UnspecifiedErr
        Me.numFloors = 0
        Me.numLobbies = 0
        ReDim Me.Lobbies(0)
        Me.baseFloor = 0
        Me.numElements = 0
        ReDim Me.elements(0)
        Me.l = 0.0
        Me.w = 0.0
        Me.effw = 0.0
        Me.finmax(0) = 0.0
        Me.eqno = 0
        Me.inno = 0
        Me.elName = "Invalide EgressElevatorPath"
        Return
    End Sub

    Public Overridable Function ResetEvacFloor(ByVal s As Vector) As Boolean
        Return Me.El(0).ResetEvacFloor(s)
    End Function

    Public Overrides Sub seteqin(ByRef eqidx As Integer)
        Dim i, iidx As Integer

        If Not Me.valid Then Return
        For i = Me.numFloors - 2 To 0 Step -1
            Me.Lobbies(i).seteqin(eqidx)
            If Not Me.Lobbies(i).IsReady Then
                Me.errCode = ErrCodeNums.StairWaySeteqninStairSeteqnin
                MsgBox("error EgressElevatorPath Lobbies seteqin " + i.ToString)
                Return
            End If
        Next
        For iidx = 0 To Me.numCars - 1
            Me.El(iidx).seteqin(eqidx)
            If Not Me.El(iidx).IsReady Then
                Me.errCode = ErrCodeNums.StairWaySeteqninHallSeteqnin
                MsgBox("error EgressElevatorPath Elevator seteqin ")
                Return
            End If
            If Not Me.El(iidx).ConnectInFlow(Me.numFloors - 1, Me.Lobbies(Me.numFloors - 2)) Then
                Me.errCode = ErrCodeNums.UnspecifiedErr
                MsgBox("error EgressElevatorPath El.ConnectInFlow unspecified error 1")
            End If
            For i = Me.numFloors - 3 To 0 Step -1
                If Not Me.El(iidx).ConnectInFlow(i + 1, Me.Lobbies(i)) Then
                    Me.errCode = ErrCodeNums.UnspecifiedErr
                    MsgBox("error EgressElevatorPath El.ConnectInFlow unspecifed error 2")
                    Return
                End If
            Next
            Me.El(iidx).ConnectOutside(Me.Out)
        Next

        For i = 0 To Me.NumEqs - 1
            Me.eqnum(i) = Me.elements(Me.eqnums(i, 0)).iToXEqNum(Me.eqnums(i, 1))
        Next
        For i = 0 To Me.NumInputs - 1
            Me.inputnum(i) = Me.elements(Me.inputnums(i, 0)).iToXInputNum(Me.inputnums(i, 1))
        Next
        Me.ready = True

    End Sub

    Public Overrides Function InitilizeODEVecters(ByRef state As Vector, ByRef mask As Vector, _
                    ByRef min As Vector, ByRef max As Vector, ByRef minval As Vector, ByRef maxval As Vector, _
                    ByRef mincon As Vector, ByRef maxcon As Vector, ByRef mincoef As Vector, ByRef maxcoef As Vector)
        'ByRef signs As Vector) As Boolean

        If Not Me.ready Then
            Me.errCode = ErrCodeNums.InitODEErr
            Return False
        End If
        For i As Integer = 0 To Me.numElements - 1
            If Not Me.elements(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then Return False
        Next

        Return True

    End Function

    Public Overrides Function ConnectInFlow(ByVal con As Integer, ByRef element As EgressElement) As Boolean

        Return False

    End Function

    Public Overrides Function PDEElementTotPopNamesCSV() As String
        Dim s As String

        s = Me.ElementTotPopNamesCSV + "Outside,"
        Return s
    End Function

    Public Overrides Function PDEElementTotPopsCSV() As String
        Dim s As String = ""
        For i As Integer = 0 To Me.numCars - 1
            s = s + Me.El(i).totalPop.ToString + ","
            s = s + Me.El(i).ElevatorAction + ","
        Next
        For i As Integer = Me.numFloors - 2 To 0 Step -1
            s = s + Me.Lobbies(i).totalPop.ToString + ","
        Next
        s = s + Me.PDEElevatorOut.ToString + ","
        Return s
    End Function

    Public Overrides Function ElementTotPopNamesCSV() As String
        Dim s As String = ""

        For i As Integer = 0 To Me.numCars - 1
            s = s + Me.El(i).ElementName + ",Elevator Status," + "Destination,"
        Next
        For i As Integer = Me.numFloors - 2 To 0 Step -1
            s = s + Me.Lobbies(i).ElementName + ","
        Next
        Return s
    End Function

    Public Overrides Function ElementTotPopsCSV() As String
        Dim s As String
        Dim iel As Integer

        s = ""
        For i As Integer = 0 To Me.numCars - 1
            iel = Math.Round(Me.El(i).totalPop)
            s = s + iel.ToString + ","
            s = s + Me.El(i).ElevatorAction + "," + Me.El(i).Destination.ToString + ","
        Next
        For i As Integer = Me.numFloors - 2 To 0 Step -1
            iel = Math.Round(Me.Lobbies(i).totalPop)
            s = s + iel.ToString + ","
        Next
        Return s
    End Function

    Public Overridable Function ElevatorAction() As String
        Dim s As String = ""

        Return s
    End Function

    Public Shared Function KloteLoadRate(ByVal capCar As Integer) As Double

        If capCar = 0 Then
            Return 0.0
        ElseIf capCar = 1 Then
            Return 0.5
        Else
            Return 1.0
        End If

    End Function

    Public Shared Function KloteUnLoadRate(ByVal capCar As Integer) As Double

        If capCar <= 0 Then
            Return 0.0
        ElseIf capCar <= 6 Then
            Return capCar / 4.0
        Else
            Return capCar / (4.0 + 0.6 * (capCar - 6))
        End If

    End Function

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub

    Public Overridable Function PDEResetEvacFlr(ByVal curTime As Double) As Boolean
        Dim flg As Boolean = True
        For i As Integer = 0 To Me.numCars - 1
            If Not Me.El(i).PDEResetEvacFlr(curTime) Then
                flg = False
            End If
        Next
        Return flg
    End Function

    Public Overridable Function PDEDoElevator(ByVal curTime As Double) As Boolean
        Dim flg As Boolean = True
        For i As Integer = 0 To Me.numCars - 1
            If Not Me.El(i).PDEDoElevator(curTime) Then
                flg = False
            End If
        Next
        Return flg
    End Function

    Public Overridable Function PDEElevatorOut() As Double
        Dim tmp As Double = Me.Out.totalPop
        Return tmp
    End Function
End Class



'Object to do simple tall building calculation

Public Class EgressCalculation

    'structures for building
    Protected numStairCase, numFloors, numElPath, numStairs As Integer
    Protected stairPopPerFlr, elPopPerFlr As Integer
    Protected firstFloorExit As Boolean
    Protected StairCase() As EgressStairWay
    Protected ElBank() As EgressKloteElevatorBank
    Protected ExitHalls() As EgressElement
    Protected Outside() As EgressSink
    Protected ElExitHalls() As EgressElement
    Protected OutsideEl() As EgressSink
    Protected totpop() As Integer
    Protected totEvacPop As Integer
    Protected maxPop As Double
    Protected valid As Boolean = True
    Protected IncludeStair, IncludeEl As Boolean
    'PT changes'''''''''''''''''''''
    Protected PopulationPF() As Integer
    Protected FractionPF() As Double
    Protected LobbyPopulationPF() As Integer
    '''''''''''''''''''''''''''''''''''''''''
    'structures for solution
    Protected simTime, outTime, curTime, outInterval, doOut As Double
    Protected Ready, Calculating, CompleteCalc, CalculationError, CompleteCritPop As Boolean
    Protected Debug As Boolean = False
    Protected eqno, inno As Integer
    Protected currentCalcVal As Double
    Protected funcs() As EgressElement
    Protected df As SimpleVectorFunction
    Protected state, mask, signs, fatol, frtol, dfatol, dfrtol As Vector
    Protected min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef As Vector
    Protected f As SimpleODESolver
    Protected errCode As Integer
    Protected errMsgStrs() As String
    Protected doorTimes(,) As Double

    Protected Enum ErrCodeNums
        NoError
        StepForwardODEreturn
        UnspecifiedError
        MaxNumErrorCodes
    End Enum

    Public Property DoDebug() As Boolean
        Get
            Return Me.Debug
        End Get
        Set(ByVal value As Boolean)
            Me.Debug = value
        End Set
    End Property
    Public ReadOnly Property CurrentTime() As Double
        Get
            Return Me.curTime
        End Get
    End Property

    Public ReadOnly Property CurrentEvacPop() As Integer
        Get
            Me.totEvacPop = 0
            For i As Integer = 0 To Me.numStairCase - 1
                Me.totEvacPop = Me.totEvacPop + Me.numStairs * Me.Outside(i).totalPop
            Next
            For i As Integer = 0 To Me.numElPath - 1
                'Me.totEvacPop = Me.totEvacPop + Me.OutsideEl(i).totalPop
                Me.totEvacPop = Me.totEvacPop + Me.ElBank(i).TotalOut
            Next
            Return Me.totEvacPop
        End Get
    End Property
    Public ReadOnly Property SimulationTime() As Double
        Get
            Return Me.simTime
        End Get
    End Property
    Public ReadOnly Property OutputTimeInterval() As Double
        Get
            Return Me.outTime
        End Get
    End Property
    Public ReadOnly Property NextOutputTime() As Boolean
        Get
            Return Me.outTime
        End Get
    End Property
    Public ReadOnly Property IsValid() As Boolean
        Get
            Return Me.valid
        End Get
    End Property
    Public ReadOnly Property IsCalcComplete() As Boolean
        Get
            Return Me.CompleteCalc
        End Get
    End Property
    Public ReadOnly Property IsCalculating() As Boolean
        Get
            Return False
        End Get
    End Property
    Public Property DoStairCase() As Boolean
        Get
            Return Me.IncludeStair
        End Get
        Set(ByVal value As Boolean)
            Me.IncludeStair = value
        End Set
    End Property
    Public Property DoElevator() As Boolean
        Get
            Return Me.IncludeEl
        End Get
        Set(ByVal value As Boolean)
            Me.IncludeEl = value
        End Set
    End Property
    Public ReadOnly Property IsInitialied() As Boolean
        Get
            Return Me.Ready
        End Get
    End Property
    Public ReadOnly Property IsThereAnError() As Boolean
        Get
            Return Me.CalculationError
        End Get
    End Property
    Public ReadOnly Property DoOutput() As Boolean
        Get
            If Me.Debug Then Return True
            Return Me.doOut
        End Get
    End Property
    Public ReadOnly Property OutputCSV() As String
        Get
            Return Me.CurrentTimeCSV + f.currentState.tostring + ","
        End Get
    End Property
    Public ReadOnly Property DoOutputCSV() As String
        Get
            Me.doOut = False
            Return Me.OutputCSV
        End Get
    End Property
    Public ReadOnly Property CurrentState() As Vector
        Get
            Return f.currentState
        End Get
    End Property
    Public ReadOnly Property CurrentCalcValue() As Double
        Get
            Return Me.currentCalcVal
        End Get
    End Property
    Public ReadOnly Property MaxCriteriaValue() As Double
        Get
            If Me.CompleteCritPop Then
                Return Me.maxPop
            Else
                Return Me.simTime
            End If
        End Get
    End Property

    Public ReadOnly Property NumberStairCase() As Double
        Get
            Return Me.numStairCase
        End Get
    End Property

    Public Overridable ReadOnly Property EgressElementNameListCSV() As String
        Get
            Dim i As Integer
            Dim s As String

            s = " "
            For i = 0 To Me.numElPath - 1
                s = s + Me.ElBank(i).EgressElementNameListCSV
                s = s + Me.ElExitHalls(i).EgressElementNameListCSV
                s = s + Me.OutsideEl(i).EgressElementNameListCSV
            Next

            For i = 0 To Me.numStairCase - 1
                s = s + Me.StairCase(i).EgressElementNameListCSV
                s = s + Me.ExitHalls(i).EgressElementNameListCSV
                s = s + Me.Outside(i).EgressElementNameListCSV
            Next

            Return s
        End Get
    End Property
    Public Overridable ReadOnly Property EquationNameListCSV() As String
        Get
            Return "Time(s)," + f.NameListCSV
        End Get
    End Property
    Public ReadOnly Property EndLine() As String
        Get
            Return Chr(13) + Chr(10)
        End Get
    End Property
    Public Overridable ReadOnly Property TotalPDEEgress() As Double
        Get
            Dim tot As Double = 0.0
            For idx As Integer = 0 To Me.numStairCase - 1
                tot = tot + Me.numStairs * Me.Outside(idx).TotalEgress
            Next
            Return tot
        End Get
    End Property
    Public Overridable ReadOnly Property TotalPDEIn() As Double
        Get
            Dim tot As Double = 0.0
            For idx As Integer = 0 To Me.numStairCase - 1
                tot = tot + Me.numStairs * Me.Outside(idx).TotalIn
            Next
            Return tot
        End Get
    End Property
    Public Overridable ReadOnly Property TotalPDEOut() As Double
        Get
            Dim tot As Double = 0.0
            For idx As Integer = 0 To Me.numStairCase - 1
                tot = tot + Me.numStairs * Me.Outside(idx).TotalOut
            Next
            Return tot
        End Get
    End Property

    Public Sub New()
        Me.numStairCase = 0
        Me.numFloors = 0
        Me.firstFloorExit = False
        Me.eqno = 0
        Me.inno = 0
        Me.Ready = False
        Me.Calculating = False
        Me.CompleteCalc = False
        Me.CalculationError = False
        Me.IncludeEl = False
        Me.IncludeStair = False
        ReDim Me.StairCase(-1)
        ReDim Me.ExitHalls(-1)
        ReDim Me.Outside(-1)
        ReDim Me.ElBank(-1)
    End Sub

    Public Sub New(ByVal numFloors As Integer, ByVal popPerFlr As Integer, ByVal fracInElevators As Double, _
                   ByVal numStrs As Integer, ByVal mergeType As Integer, _
                   ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, _
                   ByVal numSteps As Integer, ByVal strWide As Double, ByVal strWType As Integer, _
                   ByVal landLen As Double, ByVal landWide As Double, ByVal landWType As Integer, _
                   ByVal strMxFin As Double, ByVal hLen As Double, ByVal hWide As Double, _
                   ByVal hWType As Integer, ByVal hMxFin As Double, ByVal xLen As Double, _
                   ByVal xWide As Double, ByVal xWType As Integer, ByVal xMxFin As Double, _
                   ByVal oMxFin As Double, ByVal frstFlr As Boolean, _
                   ByVal numEls As Integer, ByVal doorType As Double, ByVal speed As Double, _
                   ByVal numCars As Integer, ByVal maxCarCap As Integer, ByVal accel As Double, _
                   ByVal exLen As Double, ByVal exWide As Double, ByVal exWType As Double, _
                   ByVal eoMxFin As Double, ByVal recallDelay As Double)

        Me.New(numFloors, popPerFlr, fracInElevators, numStrs, mergeType, numFlights, rsr, trd, _
                   numSteps, strWide, strWType, landLen, landWide, landWType, strMxFin, hLen, hWide, _
                   hWType, hMxFin, xLen, xWide, xWType, xMxFin, oMxFin, frstFlr, numEls, doorType, speed, _
                   EgressKloteElevatorBank.KloteLoadRate(maxCarCap), EgressKloteElevatorBank.KloteUnLoadRate(maxCarCap), _
                   numCars, maxCarCap, accel, exLen, exWide, exWType, eoMxFin, recallDelay)

    End Sub

    Public Sub New(ByVal numFloors As Integer, ByVal flrPop() As Integer, ByVal elFrac() As Double, _
                   ByVal numStrs As Integer, ByVal mergeType As Integer, _
                   ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, _
                   ByVal numSteps As Integer, ByVal strWide As Double, ByVal strWType As Integer, _
                   ByVal landLen As Double, ByVal landWide As Double, ByVal landWType As Integer, _
                   ByVal strMxFin As Double, ByVal hLen As Double, ByVal hWide As Double, _
                   ByVal hWType As Integer, ByVal hMxFin As Double, ByVal xLen As Double, _
                   ByVal xWide As Double, ByVal xWType As Integer, ByVal xMxFin As Double, _
                   ByVal oMxFin As Double, ByVal frstFlr As Boolean, _
                   ByVal numEls As Integer, ByVal doorType As Double, ByVal speed As Double, _
                   ByVal numCars As Integer, ByVal maxCarCap As Integer, ByVal accel As Double, _
                   ByVal exLen As Double, ByVal exWide As Double, ByVal exWType As Double, _
                   ByVal eoMxFin As Double, ByVal recallDelay As Double, ByVal strDlay() As Double, ByVal elDlay() As Double)

        Me.DoNew(numFloors, flrPop, elFrac, numStrs, mergeType, numFlights, rsr, trd, _
                   numSteps, strWide, strWType, landLen, landWide, landWType, strMxFin, hLen, hWide, _
                   hWType, hMxFin, xLen, xWide, xWType, xMxFin, oMxFin, frstFlr, numEls, doorType, speed, _
                   EgressKloteElevatorBank.KloteLoadRate(maxCarCap), EgressKloteElevatorBank.KloteUnLoadRate(maxCarCap), _
                   numCars, maxCarCap, accel, exLen, exWide, exWType, eoMxFin, recallDelay, strDlay, elDlay)

    End Sub

    Public Sub New(ByVal numFloors As Integer, ByVal popPerFlr As Integer, ByVal fracInElevators As Double, _
                   ByVal numStrs As Integer, ByVal mergeType As Integer, _
                   ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, _
                   ByVal numSteps As Integer, ByVal strWide As Double, ByVal strWType As Integer, _
                   ByVal landLen As Double, ByVal landWide As Double, ByVal landWType As Integer, _
                   ByVal strMxFin As Double, ByVal hLen As Double, ByVal hWide As Double, _
                   ByVal hWType As Integer, ByVal hMxFin As Double, ByVal xLen As Double, _
                   ByVal xWide As Double, ByVal xWType As Integer, ByVal xMxFin As Double, _
                   ByVal oMxFin As Double, ByVal frstFlr As Boolean, _
                   ByVal numEls As Integer, ByVal doorType As Double, ByVal speed As Double, _
                   ByVal eMxFin As Double, ByVal eMxFout As Double, ByVal numCars As Integer, _
                   ByVal maxCarCap As Integer, ByVal accel As Double, ByVal exLen As Double, _
                   ByVal exWide As Double, ByVal exWType As Double, ByVal eoMxFin As Double, _
                   ByVal recallDelay As Double)

        Dim flrPop(numFloors - 1) As Integer
        Dim elFrac(numFloors - 1) As Double
        Dim delay(numFloors - 1) As Double
        If frstFlr Then
            flrPop(0) = popPerFlr
        Else
            flrPop(0) = 0.0
        End If
        elFrac(0) = 0.0
        delay(0) = 0.0
        For i As Integer = 1 To numFloors - 1
            flrPop(i) = popPerFlr
            elFrac(i) = fracInElevators
            delay(i) = 0.0
        Next

        Me.DoNew(numFloors, flrPop, elFrac, numStrs, mergeType, numFlights, rsr, trd, numSteps, strWide, strWType, landLen, landWide, landWType, _
                strMxFin, hLen, hWide, hWType, hMxFin, xLen, xWide, xWType, xMxFin, oMxFin, frstFlr, numEls, doorType, speed, eMxFin, eMxFout, _
                numCars, maxCarCap, accel, exLen, exWide, exWType, eoMxFin, recallDelay, delay, delay)

    End Sub

    Public Sub New(ByVal numFloors As Integer, ByVal flrPop() As Integer, ByVal elFrac() As Double, _
                   ByVal numStrs As Integer, ByVal mergeType As Integer, _
                   ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, _
                   ByVal numSteps As Integer, ByVal strWide As Double, ByVal strWType As Integer, _
                   ByVal landLen As Double, ByVal landWide As Double, ByVal landWType As Integer, _
                   ByVal strMxFin As Double, ByVal hLen As Double, ByVal hWide As Double, _
                   ByVal hWType As Integer, ByVal hMxFin As Double, ByVal xLen As Double, _
                   ByVal xWide As Double, ByVal xWType As Integer, ByVal xMxFin As Double, _
                   ByVal oMxFin As Double, ByVal frstFlr As Boolean, _
                   ByVal numEls As Integer, ByVal doorType As Double, ByVal speed As Double, _
                   ByVal eMxFin As Double, ByVal eMxFout As Double, ByVal numCars As Integer, _
                   ByVal maxCarCap As Integer, ByVal accel As Double, ByVal exLen As Double, _
                   ByVal exWide As Double, ByVal exWType As Double, ByVal eoMxFin As Double, _
                   ByVal recallDelay As Double, ByVal strDlay() As Double, ByVal elDlay() As Double)

        Me.DoNew(numFloors, flrPop, elFrac, numStrs, mergeType, numFlights, rsr, trd, numSteps, strWide, strWType, landLen, landWide, _
                 landWType, strMxFin, hLen, hWide, hWType, hMxFin, xLen, xWide, xWType, xMxFin, oMxFin, frstFlr, numEls, doorType, speed, eMxFin, _
                 eMxFout, numCars, maxCarCap, accel, exLen, exWide, exWType, eoMxFin, recallDelay, strDlay, elDlay)

    End Sub

    Private Sub DoNew(ByVal numFloors As Integer, ByVal flrPop() As Integer, ByVal elFrac() As Double, _
                   ByVal numStrs As Integer, ByVal mergeType As Integer, _
                   ByVal numFlights As Integer, ByVal rsr As Double, ByVal trd As Double, _
                   ByVal numSteps As Integer, ByVal strWide As Double, ByVal strWType As Integer, _
                   ByVal landLen As Double, ByVal landWide As Double, ByVal landWType As Integer, _
                   ByVal strMxFin As Double, ByVal hLen As Double, ByVal hWide As Double, _
                   ByVal hWType As Integer, ByVal hMxFin As Double, ByVal xLen As Double, _
                   ByVal xWide As Double, ByVal xWType As Integer, ByVal xMxFin As Double, _
                   ByVal oMxFin As Double, ByVal frstFlr As Boolean, _
                   ByVal numEls As Integer, ByVal doorType As Double, ByVal speed As Double, _
                   ByVal eMxFin As Double, ByVal eMxFout As Double, ByVal numCars As Integer, _
                   ByVal maxCarCap As Integer, ByVal accel As Double, ByVal exLen As Double, _
                   ByVal exWide As Double, ByVal exWType As Double, ByVal eoMxFin As Double, _
                   ByVal recallDelay As Double, ByVal strDlay() As Double, ByVal elDlay() As Double)

        If Not Me.ValidPos(Me.numStairs, numStrs) Then Return
        If Not Me.ValidPos(Me.numFloors, numFloors) Then Return
        If Not Me.ValidPos(Me.numStairCase, 1) Then Return
        If Not Me.ValidPos(Me.numElPath, 1) Then Return
        Me.firstFloorExit = frstFlr
        If Me.numStairCase > 0 Then
            Me.IncludeStair = True
            ReDim Me.StairCase(Me.numStairCase - 1)
            ReDim Me.ExitHalls(Me.numStairCase - 1)
            ReDim Me.Outside(Me.numStairCase - 1)
        Else
            Me.IncludeStair = False
        End If
        If Me.numElPath > 0 Then
            Me.IncludeEl = True
            ReDim Me.ElBank(Me.numElPath - 1)
            ReDim Me.ElExitHalls(Me.numElPath - 1)
            ReDim Me.OutsideEl(Me.numElPath - 1)
        Else
            Me.IncludeEl = False
        End If
        ReDim Me.totpop(Me.numElPath + Me.numStairCase - 1)
        Me.eqno = 0
        Me.inno = 0
        Me.Ready = False
        Me.Calculating = False
        Me.CompleteCalc = False
        Me.CalculationError = False
        Me.doOut = False
        Me.CompleteCritPop = True
        Me.maxPop = 0.0

        ' PT changes - read csv into array
        '''''''''''''''''''''''''''''''''''''''''''''''
        'If aBuildingfile = True Then

        'Dim strfilename As String
        'Dim num_rows As Long
        'Dim num_cols As Long
        'Dim x As Integer
        'Dim y As Integer
        'Dim strarray(1, 1) As String

        ' Load the file.
        'strfilename = "buildingfile.csv"

        'Dim tmpstream As StreamReader = File.OpenText(strfilename)
        'Dim strlines() As String
        'Dim strline() As String

        'Load content of file to strLines array
        'strlines = tmpstream.ReadToEnd().Split(Environment.NewLine)

        ' Redimension the array.
        'num_rows = UBound(strlines)
        'strline = strlines(0).Split(",")
        'num_cols = UBound(strline)
        'ReDim strarray(num_rows, num_cols)

        ' Copy the data into the array.
        'For x = 0 To num_rows
        'strline = strlines(x).Split(",")

        'For y = 0 To num_cols
        'strarray(x, y) = strline(y)
        'Next
        'Next

        'Me.numFloors = num_rows + 1
        'ReDim PopulationPF(Me.numFloors)
        'ReDim LobbyPopulationPF(Me.numFloors)

        'For x = 1 To Me.numFloors
        'Me.LobbyPopulationPF(x) = Val(strarray(x - 1, 1))
        'Me.PopulationPF(x) = Val(strarray(x - 1, 0))
        'Next

        'End If


        '''''''''''''''''''''''''''''''''''''''''''''''

        For i As Integer = 0 To Me.numElPath - 1
            If Not Me.valid Then Return
            Dim flrhigh As Double = numSteps * numFlights * rsr
            Me.ElBank(i) = New EgressKloteElevatorBank(Me.numFloors, doorType, speed, accel, flrhigh, _
                                numCars, maxCarCap, eMxFin, Me.elPopPerFlr, recallDelay, 0.0)
            If Not Me.ElBank(i).IsValid Then
                Me.valid = False
            End If
            Me.ElBank(i).ElementName = "Bank " + i.ToString

            'Me.ElExitHalls(i) = New EgressElement(1, EgressElement.MergeType.Interleve, exLen, exWide, exWType, numCars * eMxFout, 0.0)
            'If Not Me.ElExitHalls(i).IsValid Then Me.valid = False
            'Me.ElExitHalls(i).ElementName = "Elevator Exit " + i.ToString

            'Me.OutsideEl(i) = New EgressSink(eoMxFin)
            'If Not Me.OutsideEl(i).IsValid Then Me.valid = False
            'Me.OutsideEl(i).ElementName = "Outside Elevator " + i.ToString

            'PT changed: If structure to include the option with building file
            If Me.valid Then
                For j As Integer = 2 To Me.numFloors
                    'If aBuildingfile = True Then
                    'Me.ElBank(i).LobbyPop(j) = Me.LobbyPopulationPF(j)
                    Me.DividePop(flrPop(j - 1), elFrac(j - 1), Me.numStairs, stairPopPerFlr, elPopPerFlr, valid)
                    Me.ElBank(i).LobbyPop(j) = elPopPerFlr
                    Me.ElBank(i).LobbyDelay(j) = elDlay(j - 1)
                    'Else
                    'Me.ElBank(i).LobbyPop(j) = Me.elPopPerFlr
                    'End If
                Next

                'If aBuildingfile = True Then
                For x As Integer = 2 To Me.numFloors
                    Me.totpop(i) = Me.totpop(i) + Me.ElBank(i).LobbyPop(x)
                Next
                Me.maxPop = Me.maxPop + Me.totpop(i)
                'Else
                'Me.totpop(i) = (Me.numFloors - 1) * Me.elPopPerFlr
                'Me.maxPop = Me.maxPop + Me.totpop(i)
                'End If
            End If
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Next

        For i As Integer = 0 To Me.numStairCase - 1
            If Not Me.valid Then Return
            Me.StairCase(i) = New EgressStairWay(Me.numFloors, 1, mergeType, numFlights, _
                rsr, trd, numSteps, strWide, landLen, landWide, strWide, strMxFin, hLen, _
                hWide, hWType, hMxFin, 0.0, 0.0)
            If Not Me.StairCase(i).IsValid Then
                Me.valid = False
            End If
            Me.StairCase(i).ElementName = "Tower " + i.ToString

            Me.ExitHalls(i) = New EgressElement(1, EgressElement.MergeType.Interleve, xLen, xWide, xWType, xMxFin, 0.0)
            If Not Me.ExitHalls(i).IsValid Then Me.valid = False
            Me.ExitHalls(i).ElementName = "Exit " + i.ToString

            Me.Outside(i) = New EgressSink(oMxFin)
            If Not Me.Outside(i).IsValid Then Me.valid = False
            Me.Outside(i).ElementName = "Outside Exit " + i.ToString

            'PT changed: IF structure to include the option with building file
            If Me.valid Then
                If Me.firstFloorExit Then Me.StairCase(i).RoomPop(0) = Me.stairPopPerFlr
                For j As Integer = 1 To Me.numFloors - 1
                    'If aBuildingfile = True Then
                    'Me.StairCase(i).RoomPop(j) = Me.PopulationPF(j)
                    Me.DividePop(flrPop(j), elFrac(j), Me.numStairs, stairPopPerFlr, elPopPerFlr, valid)
                    Me.StairCase(i).RoomPop(j) = stairPopPerFlr
                    Me.StairCase(i).RoomDelay(j) = strDlay(j)
                    'Else
                    'Me.StairCase(i).RoomPop(j) = Me.stairPopPerFlr
                    'End If
                Next
                'If aBuildingfile = True Then
                For x As Integer = 1 To Me.numFloors
                    Me.totpop(Me.numElPath + i) = Me.totpop(Me.numElPath + i) + Me.numStairs * Me.StairCase(i).RoomPop(x)
                Next
                If Me.firstFloorExit Then Me.totpop(Me.numElPath + i) = Me.totpop(Me.numElPath + i) + Me.numStairs * Me.StairCase(i).RoomPop(1)
                Me.maxPop = Me.maxPop + Me.totpop(Me.numElPath + i)
                MsgBox(Me.maxPop)
                'Else
                'Me.totpop(Me.numElPath + i) = Me.numStairs * (Me.numFloors - 1) * Me.stairPopPerFlr
                'If Me.firstFloorExit Then Me.totpop(Me.numElPath + i) = Me.totpop(Me.numElPath + i) + Me.numStairs * Me.stairPopPerFlr
                'Me.maxPop = Me.maxPop + Me.totpop(Me.numElPath + i)
                'End If

            End If
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Next

    End Sub
    Protected Function ValidPos(ByRef var As Integer, ByVal value As Integer) As Boolean
        If Me.valid And value > 0 Then
            var = value
        Else
            var = 0
            Me.valid = False
        End If
        Return Me.valid
    End Function
    Protected Sub DividePop(ByVal totPop As Integer, ByVal fracEl As Double, ByVal numStrs As Integer, _
                            ByRef stairPop As Integer, ByRef elePop As Integer, ByRef valid As Boolean)

        If fracEl < 0.0 Then
            fracEl = 0.0
        ElseIf fracEl > 1.0 Then
            fracEl = 1.0
        End If
        If totPop < 0.0 Then
            totPop = 0.0
        End If


        stairPop = Math.Floor(((1.0 - fracEl) * totPop) / numStrs)
        If fracEl > 0 Then
            elePop = totPop - numStrs * stairPop
        Else
            stairPop = stairPop + totPop - numStrs * stairPop
        End If

    End Sub

    Protected Sub SetErrMsgStr()

        ReDim Me.errMsgStrs(ErrCodeNums.MaxNumErrorCodes - 1)
        Me.errMsgStrs(ErrCodeNums.NoError) = "No Error Detected"
        Me.errMsgStrs(ErrCodeNums.StepForwardODEreturn) = "StepForward: Error return from f.ForwardStep"
        Me.errMsgStrs(ErrCodeNums.UnspecifiedError) = "Unspecfied Error has occured"


    End Sub

    Public Overridable Function SetDiagnosticPrint(ByRef filename As String, ByRef h() As String, ByRef n As Integer, ByRef endln As String) As Boolean

        Return Me.f.SetDiagnosticPrint(filename, h, n, endln)
    End Function

    Public Overridable Function SetDiagnosticPrint(ByRef filename As String, ByRef h() As String, ByRef n As Integer, ByRef endln As String, ByRef start As Double) As Boolean
        Return Me.f.SetDiagnosticPrint(filename, h, n, endln, start)
    End Function

    Public Sub StopDiagnosticPrint()
        Me.f.StopDiagnosticPrint()
    End Sub

    Public Overloads Function ErrMsg() As String
        Return Me.ErrMsg(Me.errCode)
    End Function

    Public Overloads Function ErrMsg(ByVal ierr As Integer) As String
        Me.SetErrMsgStr()
        Return "For Object " + "ErgessCalculation" + " " + Me.errMsgStrs(ierr)
    End Function

    Public Overridable Overloads Function InitializeCalc(ByVal simTime As Double, ByVal outInterval As Double) As Boolean

        Me.CompleteCritPop = False
        Me.simTime = simTime
        Return Me.InitializeCalc(outInterval)

    End Function

    Public Overridable Overloads Function InitializeCalc(ByVal outInterval As Double) As Boolean

        Dim idx As Integer

        If Not Me.valid Then Return False
        Me.outInterval = outInterval
        Me.currentCalcVal = 0.0
        idx = 0
        For i As Integer = 0 To Me.numElPath - 1
            Me.ElBank(i).seteqin(idx)
            If Not Me.ElBank(i).IsReady Then Return False
            'Me.ElExitHalls(i).seteqin(idx)
            'If Not Me.ElExitHalls(i).IsReady Then Return False
            'Me.OutsideEl(i).seteqin(idx)
            'If Not Me.OutsideEl(i).IsReady Then Return False
            'If Not Me.ElExitHalls(i).ConnectInFlow(1, Me.ElBank(i)) Then
            'Me.Ready = False
            'Me.CalculationError = True
            'Return Me.Ready
            'End If
            'If Not Me.OutsideEl(i).ConnectInFlow(1, Me.ElExitHalls(i)) Then
            'Me.Ready = False
            'Me.CalculationError = True
            'Return Me.Ready
            'End If
        Next
        For i As Integer = 0 To Me.numStairCase - 1
            Me.StairCase(i).seteqin(idx)
            If Not Me.StairCase(i).IsReady Then Return False
            Me.ExitHalls(i).seteqin(idx)
            If Not Me.ExitHalls(i).IsReady Then Return False
            Me.Outside(i).seteqin(idx)
            If Not Me.Outside(i).IsReady Then Return False
            If Not Me.ExitHalls(i).ConnectInFlow(1, Me.StairCase(i)) Then
                Me.Ready = False
                Me.CalculationError = True
                Return Me.Ready
            End If
            If Not Me.Outside(i).ConnectInFlow(1, Me.ExitHalls(i)) Then
                Me.Ready = False
                Me.CalculationError = True
                Return Me.Ready
            End If
        Next
        Me.Ready = True
        Return Me.Ready

        ReDim Me.funcs(3 * Me.numStairCase + 3 * Me.numElPath - 1)
        For i As Integer = 0 To Me.numElPath - 1
            Me.funcs(3 * i) = Me.ElBank(i)
            Me.funcs(3 * i + 1) = Me.ElExitHalls(i)
            Me.funcs(3 * i + 2) = Me.OutsideEl(i)
        Next
        For i As Integer = 0 To Me.numStairCase - 1
            Me.funcs(3 * Me.numElPath + 3 * i) = Me.StairCase(i)
            Me.funcs(3 * Me.numElPath + 3 * i + 1) = Me.ExitHalls(i)
            Me.funcs(3 * Me.numElPath + 3 * i + 2) = Me.Outside(i)
        Next

        Me.df = New SimpleVectorFunction
        Me.df.setFunction(Me.funcs)
        Me.eqno = df.numEqs
        Me.inno = df.numInputs

        state = New Vector(Me.eqno)
        mask = New Vector(Me.eqno)
        signs = New Vector(Me.eqno)
        min = New Vector(Me.eqno)
        max = New Vector(Me.eqno)
        minval = New Vector(Me.eqno)
        maxval = New Vector(Me.eqno)
        mincon = New Vector(Me.eqno)
        maxcon = New Vector(Me.eqno)
        mincoef = New Vector(Me.eqno)
        maxcoef = New Vector(Me.eqno)

        For i As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
            If Not Me.ExitHalls(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
            If Not Me.Outside(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
        Next

        For i As Integer = 0 To Me.numElPath - 1
            If Not Me.ElBank(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
            If Not Me.ElExitHalls(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
            If Not Me.OutsideEl(i).InitilizeODEVecters(state, mask, min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
                Me.CalculationError = True
                Me.Ready = False
                Return Me.Ready
            End If
        Next

        fatol = New Vector(Me.eqno, 0.00001)
        frtol = New Vector(Me.eqno, 0.00001)
        dfatol = New Vector(Me.eqno, 0.001)
        dfrtol = New Vector(Me.eqno, 0.00001)

        f = New SimpleODESolver
        If Not f.setdfdt(df) Then
            'MsgBox(f.ErrMsg())
            Me.CalculationError = True
            Me.Ready = False
            Return Me.Ready
        End If
        If Not f.setmask(mask) Then
            'MsgBox(f.ErrMsg())
            Me.CalculationError = True
            Me.Ready = False
            Return Me.Ready
        End If
        If Not f.settol(fatol, dfatol, frtol, dfrtol) Then
            'MsgBox(f.ErrMsg())
            Me.CalculationError = True
            Me.Ready = False
            Return Me.Ready
        End If
        'If Not f.setsigns(signs) Then
        If Not f.setrange(min, max, minval, maxval, mincon, maxcon, mincoef, maxcoef) Then
            'MsgBox(f.ErrMsg())
            Me.CalculationError = True
            Me.Ready = False
            Return Me.Ready
        End If
        Me.curTime = 0.0
        If Not f.setprob(Me.curTime, state) Then
            'MsgBox(f.ErrMsg())
            Me.CalculationError = True
            Me.Ready = False
            Return Me.Ready
        End If

        Me.doOut = True
        Me.outTime = Me.curTime + Me.outInterval
        Me.Ready = True
        Return Me.Ready

    End Function

    Public Function StepForward() As Boolean

        If Not Me.Ready Or Me.CompleteCalc Or Me.CalculationError Then
            Return False
        End If

        'Taking forward step

        Me.Calculating = True
        If Me.curTime > 503.0 Then
            Me.Calculating = Me.Calculating
        End If
        If Not Me.f.ForwardStep(Me.outTime, Me.curTime) Then
            Me.Calculating = False
            Me.CompleteCalc = True
            Me.Ready = False
            Me.CalculationError = True
            Me.errCode = ErrCodeNums.StepForwardODEreturn
            Return False
        Else
            For i As Integer = 0 To Me.numElPath - 1
                If Me.ElBank(i).ResetEvacFloor(Me.f.currentState) Then
                    Me.f.ResetMax(Me.ElBank(i).ExEqNoTrvlTime, Me.ElBank(i).timeToFloor)
                End If
            Next
        End If

        'Checking if calculation is complete

        If Me.CompleteCritPop Then
            If Me.curTime + 0.000001 > Me.outTime Then
                Me.CompleteCalc = True
                Me.currentCalcVal = Me.maxPop
                Me.simTime = Me.curTime + 10 * Me.outInterval
                For i As Integer = 0 To Me.numStairCase - 1
                    Me.currentCalcVal = Me.currentCalcVal - Me.numStairs * (Me.StairCase(i).totalpop _
                                                + Me.ExitHalls(i).totalPop)
                    Me.currentCalcVal = Math.Max(0.0, Me.currentCalcVal)
                    If Me.StairCase(i).totalpop > 0.0 Then
                        Me.CompleteCalc = False
                    ElseIf Me.ExitHalls(i).totalPop > 0.0 Then
                        Me.CompleteCalc = False
                    End If
                Next
                For i As Integer = 0 To Me.numElPath - 1
                    Me.currentCalcVal = Me.currentCalcVal - Me.ElBank(i).totalpop _
                                                            - Me.ElExitHalls(i).totalPop
                    Me.currentCalcVal = Math.Max(0.0, Me.currentCalcVal)
                    If Me.ElBank(i).totalpop > 0.0 Then
                        Me.CompleteCalc = False
                    ElseIf Me.ElExitHalls(i).totalPop > 0.0 Then
                        Me.CompleteCalc = False
                    End If
                Next
                If Me.CompleteCalc Then Me.Calculating = True
            End If
        Else
            Me.currentCalcVal = Me.curTime
            If Me.curTime + 0.000001 >= Me.simTime Then
                Me.CompleteCalc = True
                Me.Calculating = False
            End If
        End If

        'determining if an output is needed

        If Me.CompleteCalc Then
            Me.doOut = True
            Me.Calculating = False
        End If
        If Me.curTime + 0.000001 >= Me.outTime Then
            Me.doOut = True
            Me.outTime = Math.Min(Me.simTime, Me.curTime + Me.outInterval)
        End If

        Return True

    End Function

    Public Overridable Overloads Function CurrentTimeCSV() As String
        Return Me.CurrentTime.ToString + ","
    End Function

    Public Overridable Overloads Function CurrentTimeCSV(ByVal f As String) As String
        Return Me.CurrentTime.ToString(f) + ","
    End Function


    Public Overridable Function DoTotPopOutput() As String
        If Me.doOut = True Then
            Me.doOut = False
            Return Me.CurrentTimeCSV + Me.ElementTotPopsCSV
        Else
            Return ""
        End If
    End Function

    Public Overridable Function DoFullOutput() As String
        If Me.doOut = True Then
            Me.doOut = False
            Return Me.CurrentTimeCSV + Me.OutputCSV
        Else
            Return ""
        End If
    End Function

    Public Overridable Function ElementTotPopNamesCSV() As String
        Dim s As String

        If Not Me.valid Then Return ""
        s = ""

        For i As Integer = 0 To Me.numElPath - 1
            's = s + Me.ElBank(i).ElementTotPopNamesCSV _
            '     + Me.ElExitHalls(i).ElementName + "," _
            '    + Me.OutsideEl(i).ElementName + ","
            s = s + Me.ElBank(i).ElementTotPopNamesCSV
        Next

        For i As Integer = 0 To Me.numStairCase - 1
            s = s + Me.StairCase(i).ElementTotPopNamesCSV _
                  + Me.ExitHalls(i).ElementName + "," _
                  + Me.Outside(i).ElementName + ","
        Next
        Return s
    End Function

    Public Overridable Function PDEElementTotPopNamesCSV() As String
        Dim s As String

        If Not Me.valid Then Return ""
        s = ""
        For i As Integer = Me.numElPath - 1 To 0 Step -1
            s = s + Me.ElBank(i).PDEElementTotPopNamesCSV
        Next
        For i As Integer = Me.numStairCase - 1 To 0 Step -1
            s = s + Me.StairCase(i).PDEElementTotPopNamesCSV _
                  + Me.ExitHalls(i).ElementName + "," _
                  + Me.Outside(i).ElementName + ","
        Next
        Return s
    End Function

    Public Overridable Function DoElementTotPopsCSV() As String
        Me.doOut = False
        Return Me.CurrentTimeCSV + Me.ElementTotPopsCSV
    End Function
    Public Overridable Function ElementTotPopsCSV() As String
        Dim s As String = ""
        Dim xtot As Double
        Dim iel, itot, ieltot As Integer

        xtot = 0.0
        itot = 0
        ieltot = 0
        For i As Integer = Me.numElPath - 1 To 0 Step -1
            ieltot = ieltot + Me.ElBank(i).TotalOut
            s = s + Me.ElBank(i).ElementTotPopsCSV
        Next

        For i As Integer = Me.numStairCase - 1 To 0 Step -1
            xtot = Me.numStairs * Me.StairCase(i).totalpop
            iel = Math.Round(xtot)
            itot = iel
            s = s + Me.StairCase(i).ElementTotPopsCSV
            xtot = xtot + Me.numStairs * Me.ExitHalls(i).totalPop
            iel = Me.numStairs * Math.Round(Me.ExitHalls(i).totalPop)
            itot = itot + iel
            s = s + iel.ToString + ","
            iel = Math.Max(0.0, Math.Round(Me.totpop(Me.numElPath + i) - xtot)) + ieltot
            s = s + iel.ToString + ","
        Next
        Return s

    End Function

    Public Overridable Function PDEElementTotPopsCSV() As String
        Dim s As String = ""

        For i As Integer = Me.numElPath - 1 To 0 Step -1
            s = s + Me.ElBank(i).PDEElementTotPopsCSV
        Next
        For i As Integer = Me.numStairCase - 1 To 0 Step -1
            s = s + Me.StairCase(i).PDEElementTotPopsCSV
            s = s + Me.ExitHalls(i).totalPop.ToString + ","
        Next
        Return s

    End Function

    Public Function StepForwardPDE(ByVal dt As Double) As Boolean

        If Not Me.Ready Or Me.CompleteCalc Or Me.CalculationError Then
            Return False
        End If

        'Taking forward step

        If Not Me.StepPDEEgress(dt) Then
            Me.Calculating = False
            Me.CompleteCalc = True
            Me.Ready = False
            Me.CalculationError = True
            Me.errCode = ErrCodeNums.StepForwardODEreturn
            Return False
        End If
        For i As Integer = 0 To Me.numElPath - 1
            'If Me.ElBank(i).PDEResetEvacFlr(Me.curTime) Then
            If Not Me.ElBank(i).PDEDoElevator(Me.curTime) Then
                Me.Calculating = False
                Me.CompleteCalc = True
                Me.Ready = False
                Me.CalculationError = True
                Me.errCode = ErrCodeNums.StepForwardODEreturn
                Return False
            End If
            'End If
        Next

        'Checking if calculation is complete

        If Me.CompleteCritPop Then
            If Me.curTime + 0.000001 > Me.outTime Then
                Me.CompleteCalc = True
                'Me.currentCalcVal = Me.maxPop
                Me.currentCalcVal = 0.0
                Me.simTime = Me.curTime + 10 * Me.outInterval
                For i As Integer = 0 To Me.numStairCase - 1
                    'Me.currentCalcVal = Me.currentCalcVal - Me.numStairs * (Me.StairCase(i).totalpop _
                    '                            + Me.ExitHalls(i).totalPop)
                    Me.currentCalcVal = Me.currentCalcVal + Me.numStairs * Me.StairCase(i).TotalOut
                    Me.currentCalcVal = Math.Max(0.0, Me.currentCalcVal)
                    If Me.StairCase(i).totalpop > 0.01 Then
                        Me.CompleteCalc = False
                    ElseIf Me.ExitHalls(i).totalPop > 0.01 Then
                        Me.CompleteCalc = False
                    End If
                Next
                For i As Integer = 0 To Me.numElPath - 1
                    Me.currentCalcVal = Me.currentCalcVal + Me.ElBank(i).TotalOut
                    Me.currentCalcVal = Math.Max(0.0, Me.currentCalcVal)
                    If Me.ElBank(i).totalpop > 0.01 Then
                        Me.CompleteCalc = False
                        'ElseIf Me.ElExitHalls(i).totalPop > 0.01 Then
                        'Me.CompleteCalc = False
                    End If
                Next
                If Me.CompleteCalc Then Me.Calculating = True
            End If
        Else
            Me.currentCalcVal = Me.curTime
            If Me.curTime + 0.000001 >= Me.simTime Then
                Me.CompleteCalc = True
                Me.Calculating = False
            End If
        End If

        'determining if an output is needed

        If Me.CompleteCalc Then
            Me.doOut = True
            Me.Calculating = False
        End If
        If Me.curTime + 0.000001 >= Me.outTime Then
            Me.doOut = True
            Me.outTime = Math.Min(Me.simTime, Me.curTime + Me.outInterval)
        End If
        Me.curTime = Me.curTime + dt
        Return True

    End Function

    Public Overridable Function StepPDEEgress(ByVal dt As Double) As Boolean

        Dim flg As Boolean = True
        Dim dthalf As Double = dt / 2.0
        For idx As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(idx).RK4step0(Me.curTime, dthalf) Then
                flg = False
            End If
            If Not Me.ExitHalls(idx).RK4step0(Me.curTime, dthalf) Then
                flg = False
            End If
            If Not Me.Outside(idx).RK4step0(Me.curTime, dthalf) Then
                flg = False
            End If
        Next
        For idx As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(idx).RK4step1or2(1, Me.curTime, dthalf) Then
                flg = False
            End If
            If Not Me.ExitHalls(idx).RK4step1or2(1, Me.curTime, dthalf) Then
                flg = False
            End If
            If Not Me.Outside(idx).RK4step1or2(1, Me.curTime, dthalf) Then
                flg = False
            End If
        Next
        For idx As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(idx).RK4step1or2(2, Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.ExitHalls(idx).RK4step1or2(2, Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.Outside(idx).RK4step1or2(2, Me.curTime, dt) Then
                flg = False
            End If
        Next
        For idx As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(idx).RK4step3(Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.ExitHalls(idx).RK4step3(Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.Outside(idx).RK4step3(Me.curTime, dt) Then
                flg = False
            End If
        Next
        For idx As Integer = 0 To Me.numStairCase - 1
            If Not Me.StairCase(idx).RK4Complete(Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.ExitHalls(idx).RK4Complete(Me.curTime, dt) Then
                flg = False
            End If
            If Not Me.Outside(idx).RK4Complete(Me.curTime, dt) Then
                flg = False
            End If
            Me.ExitHalls(idx).CorrectDensity()
        Next
        'Me.curTime = Me.curTime + dt
        Return flg
    End Function

    Public Overridable Function PDEElevatorOut() As Double
        Return Me.ElBank(0).PDEElevatorOut
    End Function
End Class
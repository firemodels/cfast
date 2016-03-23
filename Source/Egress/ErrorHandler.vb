Public Interface ErrorWriteHandler
    Sub ErrorOut(ByVal ObjType As String, ByVal ObjName As String, ByVal MethName As String, _
                  ByVal ierr As Integer, ByVal errmsg As String)
End Interface
Public Class ErrorHandler

    Protected eWrt As ErrorWriteHandler
    Protected eWrtFlg As Boolean
    Protected DefType As String
    Protected DefName As String
    Protected DefTypeSet As Boolean
    Protected WarnFlg As Boolean
    Protected ErrFlg As Boolean
    Protected LastNum As Integer
    Protected LastMsg As String
    Protected LastMeth As String
    Protected LastName As String
    Protected LastType As String
    Protected valid As Boolean
    Protected TraceBack As Boolean
    Protected First As Boolean
    Protected eHnd As ErrorHandler
    Protected eHndDef As Boolean
    Protected Daughters() As ErrorHandler
    Protected DaughtersDef As Boolean
    Protected MethodNames() As String
    Protected numMethodNames As Integer
    Protected WarnCodeToMethMap() As Integer
    Protected WarningMsgs() As String
    Protected numWarnCodes As Integer
    Protected ErrCodeToMethMap() As Integer
    Protected ErrMsgs() As String
    Protected numErrCodes As Integer

    Public ReadOnly Property IsValid() As Boolean
        Get
            Return Me.valid
        End Get
    End Property
    Public ReadOnly Property ErrorWriterSet() As Boolean
        Get
            Return Me.eWrtFlg
        End Get
    End Property
    Public Property ErrorWriter() As ErrorWriteHandler
        Get
            Return Me.eWrt
        End Get
        Set(ByVal value As ErrorWriteHandler)
            If value Is Nothing Then
                Me.eWrtFlg = False
                Me.eWrt = Nothing
            Else
                Me.eWrtFlg = True
                Me.eWrt = value
            End If
        End Set
    End Property
    Public ReadOnly Property ErrorHandlerSet() As Boolean
        Get
            Return Me.eHndDef
        End Get
    End Property
    Public Property ErrorHandler() As ErrorHandler
        Get
            Return Me.eHnd
        End Get
        Set(ByVal value As ErrorHandler)
            If value Is Nothing Then
                Me.eHndDef = False
                Me.eHnd = Nothing
            Else
                Me.eHndDef = True
                Me.eHnd = value
            End If
        End Set
    End Property
    Public Property DefaultName() As String
        Get
            Return Me.DefName
        End Get
        Set(ByVal value As String)
            Me.DefName = value
        End Set
    End Property
    Public Property DefaultType() As String
        Get
            Return Me.DefType
        End Get
        Set(ByVal value As String)
            If Not Me.DefTypeSet Then Return
            Me.DefType = value
            Me.DefTypeSet = True
        End Set
    End Property
    Public ReadOnly Property Errors() As Boolean
        Get
            Return Me.ErrFlg
        End Get
    End Property
    Public ReadOnly Property Warnings() As Boolean
        Get
            Return Me.WarnFlg
        End Get
    End Property
    Public Property ListMethodNames() As String()
        Get
            Return Me.MethodNames
        End Get
        Set(ByVal value As String())
            Me.MethodNames = value
            Me.numMethodNames = Me.MethodNames.GetLength(0)
        End Set
    End Property
    Public Property MapWarningCodesToMethodNames() As Integer()
        Get
            Return Me.WarnCodeToMethMap
        End Get
        Set(ByVal value As Integer())
            Me.WarnCodeToMethMap = value
            Me.numWarnCodes = Me.WarnCodeToMethMap.GetLength(0)
        End Set
    End Property
    Public Property ListWarningMsgs() As String()
        Get
            Return Me.WarningMsgs
        End Get
        Set(ByVal value As String())
            Me.WarningMsgs = value
            Me.numWarnCodes = Me.WarningMsgs.GetLength(0)
        End Set
    End Property
    Public Property MapErrorCodesToMethodNames() As Integer()
        Get
            Return Me.ErrCodeToMethMap
        End Get
        Set(ByVal value As Integer())
            Me.ErrCodeToMethMap = value
            Me.numErrCodes = Me.ErrCodeToMethMap.GetLength(0)
        End Set
    End Property
    Public Property ListErrorMsgs() As String()
        Get
            Return Me.ErrMsgs
        End Get
        Set(ByVal value As String())
            Me.ErrMsgs = value
            Me.numErrCodes = Me.ErrMsgs.GetLength(0)
        End Set
    End Property

    Public Sub New(ByRef errhndr As ErrorHandler, ByRef errwrt As ErrorWriteHandler, _
                    ByVal ObjType As String, ByVal ObjName As String)

        Me.eHnd = Nothing
        Me.eWrt = Nothing
        Me.eHndDef = False
        Me.eWrtFlg = False
        Me.DefType = ""
        Me.DefTypeSet = False
        Me.DefName = ""

        If errhndr Is ErrorHandler Then
            Me.eHndDef = True
            Me.valid = True
            Me.eHnd = errhndr
        End If
        If Not (errwrt Is Nothing) Then
            Me.eWrtFlg = True
            Me.valid = True
            Me.eWrt = errwrt
        End If
        If ObjType <> "" Then
            Me.DefTypeSet = True
            Me.DefType = ObjType
        End If
        Me.DefName = ObjName
        Me.WarnFlg = False
        Me.ErrFlg = False
        Me.LastNum = 0
        Me.LastMsg = "No Errors Reported"
        Me.LastMeth = "New"
        Me.LastName = Me.DefName
        Me.LastType = Me.DefType
    End Sub

    Public Sub New()
        Me.New(Nothing, Nothing, "", "")
    End Sub

    Public Sub New(ByRef errhndlr As ErrorHandler)
        Me.New(errhndlr, Nothing, "", "")
    End Sub

    Public Sub New(ByRef errhndlr As ErrorHandler, ByVal ObjType As String)
        Me.New(errhndlr, Nothing, ObjType, "")
    End Sub

    Public Sub New(ByRef errhndlr As ErrorHandler, ByVal ObjType As String, ByVal ObjName As String)
        Me.New(errhndlr, Nothing, ObjType, ObjName)
    End Sub

    Public Sub New(ByRef errwrt As ErrorWriteHandler)
        Me.New(Nothing, errwrt, "", "")
    End Sub

    Public Sub New(ByRef errwrt As ErrorWriteHandler, ByVal ObjType As String)
        Me.New(Nothing, errwrt, ObjType, "")
    End Sub

    Public Sub New(ByRef errwrt As ErrorWriteHandler, ByVal ObjType As String, ByVal ObjName As String)
        Me.New(Nothing, errwrt, ObjType, ObjName)
    End Sub

    Public Sub New(ByRef errhndlr As ErrorHandler, ByRef errwrt As ErrorWriteHandler)
        Me.New(errhndlr, errwrt, "", "")
    End Sub

    Public Sub New(ByRef errhndlr As ErrorHandler, ByRef errwrt As ErrorWriteHandler, ByVal ObjType As String)
        Me.New(errhndlr, errwrt, ObjType, "")
    End Sub

    Public Sub New(ByVal ObjType As String)
        Me.New(Nothing, Nothing, ObjType, "")
    End Sub

    Public Sub New(ByVal ObjType As String, ByVal ObjName As String)
        Me.New(Nothing, Nothing, ObjType, ObjName)
    End Sub

    Protected Sub DoOut(ByVal type As String, ByVal name As String, ByVal meth As String, ByVal num As Integer, ByVal msg As String, ByVal trace As Boolean, ByVal first As Boolean)
        If Me.eWrtFlg And first Then
            Me.eWrt.ErrorOut(type, name, meth, num, msg)
            first = False
        End If
        If Me.eHndDef Then
            Me.eHnd.ErrorOut(type, name, meth, num, msg, False, False)
        End If
        If trace Then
            Dim tmsg As String = "Called method " + Me.LastMeth + " of " + Me.LastName + " Object class " + Me.LastType
            If Me.eWrtFlg Then
                Me.eWrt.ErrorOut(Me.DefType, Me.DefName, "", Me.LastNum, tmsg)
            End If
            If Me.eHndDef Then
                Me.eHnd.ErrorOut(Me.DefType, Me.DefName, "TraceBack", num, tmsg, trace, False)
            End If
        End If
    End Sub

    Public Sub ErrorOut(ByVal type As String, ByVal name As String, ByVal meth As String, ByVal icode As Integer, ByVal msg As String, ByVal trace As Boolean, ByVal first As Boolean)
        Me.DoOut(type, name, meth, icode, msg, trace, first)
    End Sub

    Public Overridable Overloads Sub comment()
        Me.comment(Me.DefType, Me.DefName, "", "Unspecified comment made")
    End Sub

    Public Overridable Overloads Sub comment(ByVal c As String)
        Me.comment(Me.DefType, Me.DefName, "", c)
    End Sub

    Public Overridable Overloads Sub comment(ByVal method As String, ByVal c As String)
        Me.comment(Me.DefType, Me.DefName, method, c)
    End Sub

    Public Overridable Overloads Sub comment(ByVal name As String, ByVal method As String, ByVal c As String)
        Me.comment(Me.DefType, name, method, c)
    End Sub

    Public Overridable Overloads Sub comment(ByVal type As String, ByVal name As String, ByVal method As String, ByVal c As String)
        Me.DoOut(type, name, method, 0, c, False, True)
    End Sub

    Public Overridable Overloads Sub Warning()
        Me.Warning(Me.DefType, Me.DefName, "", 1, "Unspecified warning")
    End Sub

    Public Overridable Overloads Sub Warning(ByVal c As String)
        Me.Warning(Me.DefType, Me.DefName, "", 1, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal method As String, ByVal c As String)
        Me.Warning(Me.DefType, Me.DefName, method, 1, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal name As String, ByVal method As String, ByVal c As String)
        Me.Warning(Me.DefType, name, method, 1, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal type As String, ByVal name As String, ByVal method As String, ByVal c As String)
        Me.Warning(type, name, method, 1, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal icode As Integer)
        Me.Warning(Me.DefType, Me.DefName, "", icode)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal icode As Integer, ByVal c As String)
        Me.Warning(Me.DefType, Me.DefName, "", icode, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal method As String, ByVal icode As Integer)
        Me.Warning(Me.DefType, Me.DefName, method, icode)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.Warning(Me.DefType, Me.DefName, method, icode, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal name As String, ByVal method As String, ByVal icode As Integer)
        Me.Warning(Me.DefType, name, method, icode)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal name As String, ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.Warning(Me.DefType, name, method, icode, c)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal type As String, ByVal name As String, ByVal method As String, ByVal icode As Integer)
        Dim msg As String
        icode = Math.Abs(icode)
        If icode > 0 And icode <= Me.numWarnCodes Then
            msg = Me.WarningMsgs(icode - 1)
        Else
            msg = "Unknown warning code"
        End If
        Me.Warning(type, name, method, icode, msg)
    End Sub

    Public Overridable Overloads Sub Warning(ByVal type As String, ByVal name As String, ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.WarnFlg = True
        icode = Math.Abs(icode)
        If icode = 0 Then
            Me.comment(type, name, method, c)
        ElseIf method = "" Then
            If icode > 0 And icode <= Me.numWarnCodes Then
                method = Me.MethodNames(Me.WarnCodeToMethMap(icode - 1))
            End If
        End If
        Me.WarnFlg = True
        Me.LastNum = icode
        Me.LastMeth = method
        Me.LastName = name
        Me.LastType = type
        Me.LastMsg = c
        Me.eWrt.ErrorOut(Me.LastType, Me.LastName, Me.LastMeth, Me.LastNum, Me.LastMsg)
    End Sub

    Public Overridable Overloads Sub Err()
        Me.Err(Me.DefType, Me.DefName, "", -1, "Unspecified Error")
    End Sub

    Public Overridable Overloads Sub Err(ByVal c As String)
        Me.Err(Me.DefType, Me.DefName, "", -1, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal method As String, ByVal c As String)
        Me.Err(Me.DefType, Me.DefName, method, -1, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal name As String, ByVal method As String, ByVal c As String)
        Me.Err(Me.DefType, name, method, -1, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal type As String, ByVal name As String, ByVal method As String, ByVal c As String)
        Me.Err(type, name, method, -1, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal icode As Integer)
        Me.Err(Me.DefType, Me.DefName, "", icode)
    End Sub

    Public Overridable Overloads Sub Err(ByVal icode As Integer, ByVal c As String)
        Me.Err(Me.DefType, Me.DefName, "", icode, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal method As String, ByVal icode As Integer)
        Me.Err(Me.DefType, Me.DefName, method, icode)
    End Sub

    Public Overridable Overloads Sub Err(ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.Err(Me.DefType, Me.DefName, method, icode, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal name As String, ByVal method As String, ByVal icode As Integer)
        Me.Err(Me.DefType, name, method, icode)
    End Sub

    Public Overridable Overloads Sub Err(ByVal name As String, ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.Err(Me.DefType, name, method, icode, c)
    End Sub

    Public Overridable Overloads Sub Err(ByVal type As String, ByVal name As String, ByVal method As String, ByVal icode As Integer)
        Dim msg As String
        icode = Math.Abs(icode)
        If icode > 0 And icode < Me.numWarnCodes Then
            msg = Me.ErrMsgs(icode - 1)
        Else
            msg = "Unknown error code"
        End If
        Me.Err(type, name, method, icode, msg)
    End Sub

    Public Overridable Overloads Sub Err(ByVal type As String, ByVal name As String, ByVal method As String, ByVal icode As Integer, ByVal c As String)
        Me.ErrFlg = True
        icode = Math.Abs(icode)
        If icode = 0 Then
            Me.comment(type, name, method, c)
        ElseIf method = "" Then
            icode = Math.Abs(icode)
            If icode > 0 And icode <= Me.numErrCodes Then
                method = Me.MethodNames(Me.ErrCodeToMethMap(icode - 1))
            End If
        End If
        Me.ErrFlg = True
        icode = Math.Abs(icode)
        Me.LastNum = -icode
        Me.LastMsg = c
        Me.LastName = name
        Me.LastType = type
        Me.DoOut(Me.LastType, Me.LastName, Me.LastMeth, Me.LastNum, Me.LastMsg, Me.TraceBack, True)
    End Sub

End Class

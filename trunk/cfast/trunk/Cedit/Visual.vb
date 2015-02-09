Public Class Visual

    Friend Const MaximumVisuals As Integer = 30
    Public Const TwoD As Integer = 0
    Public Const ThreeD As Integer = 1
    Public Const IsoSurface As Integer = 2

    Private aType As Integer
    Private aCompartment As Integer
    Private aValue As Single
    Private aAxis As Single
    Private aChanged As Boolean = False     ' True once compartment information has changed
    Private HasErrors As Integer = 0        ' Temporary variable to indicate whether there are errors in the specification
    Public Sub New()
        Me.aType = 0
        Me.aCompartment = -1
        Me.aValue = 0.0
        Me.Axis = 0
    End Sub
    Public Sub New(ByVal Type As Integer, ByVal Axis As Integer, ByVal Value As Single, ByVal Compartment As Integer)
        Me.aType = Type
        Me.aAxis = Axis
        Me.aValue = Value
        Me.aCompartment = Compartment
    End Sub
    Property Compartment() As Integer
        Get
            Return aCompartment
        End Get
        Set(ByVal Value As Integer)
            If aCompartment <> Value Then
                aCompartment = Value
                aChanged = True
            End If
        End Set
    End Property
    Property Type() As Integer
        Get
            Return aType
        End Get
        Set(ByVal Value As Integer)
            If aType <> Value Then
                aType = Value
                aChanged = True
            End If
        End Set
    End Property
    Property Value() As Single
        Get
            If Me.aType = IsoSurface Then
                Return myUnits.Convert(UnitsNum.Temperature).FromSI(aValue)
            Else
                Return myUnits.Convert(UnitsNum.Length).FromSI(aValue)
            End If
        End Get
        Set(ByVal Value As Single)
            If Me.aType = IsoSurface Then
                If myUnits.Convert(UnitsNum.Temperature).ToSI(Value) <> aValue And Value > 0.0 Then
                    aChanged = True
                    aValue = myUnits.Convert(UnitsNum.Temperature).ToSI(Value)
                End If
            Else
                If myUnits.Convert(UnitsNum.Length).ToSI(Value) <> aValue And Value >= 0.0 Then
                    aChanged = True
                    aValue = myUnits.Convert(UnitsNum.Length).ToSI(Value)
                End If
            End If
        End Set
    End Property
    Property Axis() As Integer
        Get
            Return aAxis
        End Get
        Set(ByVal Value As Integer)
            If aAxis <> Value Then
                aAxis = Value
                aChanged = True
            End If
        End Set
    End Property
    Public Property Changed() As Boolean
        Get
            Return aChanged
        End Get
        Set(ByVal Value As Boolean)
            aChanged = Value
        End Set
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get

        End Get
    End Property

End Class
Public Class VisualCollection
    Inherits System.Collections.CollectionBase
    Public ReadOnly Maximum As Integer = Visual.MaximumVisuals
    Private i As Integer, j As Integer
    Private HasErrors As Integer
    Public Sub Add(ByVal aVisual As Visual)
        List.Add(aVisual)
    End Sub
    Public Sub Copy(ByVal indexFrom As Integer, ByVal indexTo As Integer)
        Dim FromVisual As New Visual, ToVisual As New Visual
        FromVisual = CType(List.Item(indexFrom), Visual)
        ToVisual.Type = FromVisual.Type
        ToVisual.Axis = FromVisual.Axis
        ToVisual.Value = FromVisual.Value
        ToVisual.Compartment = FromVisual.Compartment
        List.Item(indexTo) = ToVisual
    End Sub
    Public Sub Remove(ByVal index As Integer)
        If index >= 0 Or index <= Count - 1 Then
            List.RemoveAt(index)
        End If
    End Sub
    Public Sub RemoveAll(ByVal index As Integer)
        ' Removes all visuals associated with the compartment specified by index
        If index >= 0 And index < myCompartments.Count Then
            Dim aVisual As New Visual
            i = 0
            While i < Count
                aVisual = CType(List.Item(i), Visual)
                If aVisual.Compartment = index Then
                    List.RemoveAt(i)
                    i = 0
                Else
                    i += 1
                End If
            End While
        End If
    End Sub
    Public Sub Renumber(ByVal Oldindex As Integer, ByVal Newindex As Integer)
        Dim aVisual As New Visual
        For i = 0 To Count - 1
            aVisual = CType(List.Item(i), Visual)
            If aVisual.Compartment = Oldindex Then
                aVisual.Compartment = Newindex
            End If
            List.Item(i) = aVisual
        Next
    End Sub
    Default Public Property Item(ByVal index As Integer) As Visual
        Get
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error. Visualization number not found.")
            Else
                Return CType(List.Item(index), Visual)
            End If
        End Get
        Set(ByVal Value As Visual)
            If index > Count - 1 Or index < 0 Then
                System.Windows.Forms.MessageBox.Show("Internal Error. Visualization number not found.")
            Else
                List.Item(index) = Value
            End If
        End Set
    End Property
    Public ReadOnly Property NumberofConnections(ByVal index As Integer) As Integer
        Get
            Dim aVisual As Visual
            Dim numVisuals As Integer = 0
            numVisuals = 0
            If Count > 0 Then
                For i = 0 To Count - 1
                    aVisual = CType(List.Item(i), Visual)
                    If aVisual.Compartment = index Then numVisuals += 1
                Next
            End If
            Return numVisuals
        End Get
    End Property
    Public ReadOnly Property Changed() As Boolean
        Get
            If Count > 0 Then
                Dim aVisual As Visual
                For i = 0 To Count - 1
                    aVisual = CType(List(i), Visual)
                    If aVisual.Changed Then Return True
                Next
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsValid() As Integer
        Get
            HasErrors = 0
            If Count > 0 Then
                ' Check each Visual for errors
                Dim aVisual As Visual
                For i = 0 To Count - 1
                    aVisual = CType(List(i), Visual)
                    HasErrors += aVisual.IsValid
                Next
            End If
            Return HasErrors
        End Get
    End Property
End Class
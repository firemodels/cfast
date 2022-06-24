Imports System
Imports System.IO
Imports System.Xml

Module IO
#Region "Read Routines"
    Public Sub ReadInputFile(Filename As String)
        Dim IO As Integer = 1
        Dim str As String

        FileOpen(IO, Filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
        str = LineInput(IO)
        FileClose(IO)
        If str.Substring(0, 1) = "&" Then
            ReadInputFileNML(Filename)
        ElseIf str.Substring(0, 5) = "VERSN" Then
            System.Windows.Forms.MessageBox.Show("CFAST no longer supports the older spreadsheet input file format.")
        End If
    End Sub
    Private Sub ReadInputFileNML(Filename As String)
        'Filename is assumed to be the complete path plus name and extenstion
        Dim NMList As NameListFile

        myErrors.Break(Filename)

        NMList = New NameListFile(Filename)
        ReadInputFileNMLHead(NMList, myEnvironment)
        ReadInputFileNMLTime(NMList, myEnvironment)
        ReadInputFileNMLInit(NMList, myEnvironment)
        ReadInputFileNMLMisc(NMList, myEnvironment)
        ReadInputFileNMLMatl(NMList, myThermalProperties)
        ReadInputFileNMLComp(NMList, myCompartments)
        ReadInputFileNMLDevc(NMList, myDetectors)
        ReadInputFileNMLChem(NMList, myFireProperties)
        ReadInputFileNMLFire(NMList, myFires)
        ReadInputFileNMLVent(NMList, myHVents, myMVents, myVVents)
        ReadInputFileNMLConn(NMList, myHHeats, myVHeats)
        ReadInputFileNMLISOF(NMList, myVisuals)
        ReadInputFileNMLSLCF(NMList, myVisuals)
        ReadInputFileNMLDiag(NMList, myEnvironment)

        ReadInputFileNMLMHDR(NMList, myMHeaders)
        ReadInputFileNMLMRND(NMList, myMRandoms)
        ReadInputFileNMLMFLD(NMList, myMFields)
        ReadInputFileNMLMFIR(NMList, myMFires)
        ReadInputFileNMLMSTT(NMList, myMStats)
        ReadInputFileNMLOutp(NMList, myOutputs)
    End Sub
    Private Sub ReadInputFileNMLHead(NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j As Integer
        Dim ver As Integer
        Dim title As String

        someEnvironment.Version = 0
        title = ""
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "HEAD") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "VERSION") Then
                        ver = someEnvironment.Version
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TITLE") Then
                        title = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In HEAD namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        someEnvironment.Title = title
        someEnvironment.Version = ver
        someEnvironment.Changed = False
    End Sub
    Private Sub ReadInputFileNMLTime(NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j As Integer
        Dim print, sim, smoke, ss As Double

        print = 60
        sim = 900
        smoke = 15
        ss = 15
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "TIME") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "PRINT") Then
                        print = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SIMULATION") Then
                        sim = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SMOKEVIEW") Then
                        smoke = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SPREADSHEET") Then
                        ss = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In TIME namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        someEnvironment.OutputInterval = print
        someEnvironment.SimulationTime = sim
        someEnvironment.SmokeviewInterval = smoke
        someEnvironment.SpreadsheetInterval = ss
        someEnvironment.Changed = False
    End Sub
    Private Sub ReadInputFileNMLInit(NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j As Integer
        Dim pressure, rh, intemp, extemp As Double

        pressure = 101325
        rh = 50
        intemp = 20
        extemp = 20
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "INIT") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "PRESSURE") Then
                        pressure = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "RELATIVE_HUMIDITY") Then
                        rh = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INTERIOR_TEMPERATURE") Then
                        intemp = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "EXTERIOR_TEMPERATURE") Then
                        extemp = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In INIT namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        someEnvironment.IntAmbTemperature = intemp + 273.15
        someEnvironment.ExtAmbTemperature = extemp + 273.15
        someEnvironment.ExtAmbPressure = pressure
        someEnvironment.IntAmbRH = rh
        someEnvironment.Changed = False
    End Sub
    Private Sub ReadInputFileNMLMisc(NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j, max As Integer
        Dim adiabatic, overwrite As Boolean
        Dim maxts, loxyl, extinctionFlaming, extinctionSmoldering As Double

        adiabatic = False
        overwrite = True
        maxts = Environment.DefaultMaximumTimeStep
        loxyl = 0.15
        extinctionFlaming = 8700
        extinctionSmoldering = 4400
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "MISC") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ADIABATIC") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            adiabatic = False
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            adiabatic = True
                        Else
                            myErrors.Add("In MISC namelist for ADIABATIC " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MAX_TIME_STEP") Then
                        maxts = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "LOWER_OXYGEN_LIMIT") Then
                        loxyl = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SPECIFIC_EXTINCTION") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            extinctionFlaming = NMList.ForNMListVarGetNum(i, j, 1)
                            extinctionSmoldering = NMList.ForNMListVarGetNum(i, j, 2)
                        Else
                            myErrors.Add("In MISC namelist for SPECIFIC_EXTINCTION input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "OVERWRITE") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            overwrite = False
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            overwrite = True
                        Else
                            myErrors.Add("In MISC namelist for OVERWRITE " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    Else
                        myErrors.Add("In MISC namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next

        someEnvironment.AdiabaticWalls = adiabatic
        someEnvironment.MaximumTimeStep = maxts
        someEnvironment.LowerOxygenLimit = loxyl
        someEnvironment.FlamingExtinctionCoefficient = extinctionFlaming
        someEnvironment.SmolderingExtinctionCoefficient = extinctionSmoldering
        someEnvironment.Overwrite = overwrite
        someEnvironment.Changed = False
    End Sub
    Private Sub ReadInputFileNMLMatl(NMList As NameListFile, ByRef someThermalProperties As ThermalPropertiesCollection)
        Dim i, j As Integer
        Dim conduct, dens, emiss, spech, thick As Double
        Dim id, matl, fyi As String
        Dim valid As Boolean
        Dim hcl() As Double = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        Dim iProp As Integer

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "MATL") Then
                conduct = -1
                dens = -1
                emiss = 0.9
                spech = -1
                thick = -1
                id = ""
                matl = ""
                fyi = ""
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "CONDUCTIVITY") Then
                        conduct = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "DENSITY") Then
                        dens = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "EMISSIVITY") Then
                        emiss = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SPECIFIC_HEAT") Then
                        spech = NMList.ForNMListVarGetNum(i, j, 1) * 1000
                    ElseIf (NMList.ForNMListGetVar(i, j) = "THICKNESS") Then
                        thick = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MATERIAL") Then
                        matl = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MATL namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If conduct <= 0 Then valid = False
                If dens <= 0 Then valid = False
                If emiss <= 0 Then valid = False
                If spech <= 0 Then valid = False
                If thick <= 0 Then valid = False
                If id = "" Then valid = False
                If valid Then
                    If matl = "" Then matl = id
                    If someThermalProperties.Count > 0 Then
                        iProp = someThermalProperties.GetIndex(id)
                        If iProp >= 0 Then
                            ' We already have a thermal property with this name.  If it's totally identical, then it's already been added.  If not, they are trying to add a second one with the same name.  We'll allow it but error checking with flag it as an issue.
                            Dim aProperty As New ThermalProperty
                            aProperty = someThermalProperties.Item(iProp)
                            If aProperty.Name = matl And aProperty.Conductivity = conduct And aProperty.SpecificHeat = spech And aProperty.Density = dens _
                                And aProperty.Thickness = thick And aProperty.Emissivity = emiss Then
                                'logic needs to be reworked
                            Else
                                someThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                                someThermalProperties.Item(someThermalProperties.Count - 1).SetHCl(hcl)
                                someThermalProperties.Item(someThermalProperties.Count - 1).FYI = fyi
                                someThermalProperties.Item(someThermalProperties.Count - 1).Changed = False
                            End If
                        Else
                            someThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                            someThermalProperties.Item(someThermalProperties.Count - 1).SetHCl(hcl)
                            someThermalProperties.Item(someThermalProperties.Count - 1).FYI = fyi
                            someThermalProperties.Item(someThermalProperties.Count - 1).Changed = False
                        End If
                    Else
                        someThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                        someThermalProperties.Item(someThermalProperties.Count - 1).SetHCl(hcl)
                        someThermalProperties.Item(someThermalProperties.Count - 1).FYI = fyi
                        someThermalProperties.Item(someThermalProperties.Count - 1).Changed = False
                    End If
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLComp(NMList As NameListFile, ByRef someCompartments As CompartmentCollection)
        Dim i, j, k, max As Integer
        Dim id, fyi As String
        Dim depth, height, width As Double
        Dim shaft, hall, valid, leakasarea, leakasratio As Boolean
        Dim grid(3) As Integer
        Dim origin(3), leakareas(2), leakratios(2), comparea(0), compheight(0) As Double

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "COMP") Then
                hall = False
                shaft = False
                leakasarea = False
                leakasratio = False
                depth = -1
                width = -1
                height = -1
                id = ""
                fyi = ""
                Dim ceilid As String() = {"", "", "", ""}
                Dim floorid As String() = {"", "", "", ""}
                Dim wallid As String() = {"", "", "", ""}
                Dim ceilthick As Double() = {0.0, 0.0, 0.0, 0.0}
                Dim floorthick As Double() = {0.0, 0.0, 0.0, 0.0}
                Dim wallthick As Double() = {0.0, 0.0, 0.0, 0.0}
                ReDim comparea(0), compheight(0)
                For k = 0 To 2
                    grid(k) = 50
                    origin(k) = 0.0
                Next
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CEILING_MATL_ID") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            ceilid(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLOOR_MATL_ID") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            floorid(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "WALL_MATL_ID") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            wallid(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CEILING_THICKNESS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            ceilthick(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLOOR_THICKNESS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            floorthick(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "WALL_THICKNESS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        For k = 1 To max
                            wallthick(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "HALL") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            hall = True
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            hall = False
                        Else
                            myErrors.Add("In COMP namelist for HALL " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SHAFT") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            shaft = True
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            shaft = False
                        Else
                            myErrors.Add("In COMP namelist for SHAFT " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEPTH" Then
                        depth = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEIGHT" Then
                        height = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "WIDTH" Then
                        width = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CROSS_SECT_AREAS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim comparea(max)
                        For k = 1 To max
                            comparea(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf NMList.ForNMListGetVar(i, j) = "CROSS_SECT_HEIGHTS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim compheight(max)
                        For k = 1 To max
                            compheight(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf NMList.ForNMListGetVar(i, j) = "GRID" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= 3 Then
                            For k = 1 To max
                                grid(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In COMP namelist for GRID input must be 1 to 3 positive integers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "ORIGIN" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 3 And max <= 3 Then
                            For k = 1 To max
                                origin(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In COMP namelist for ORIGIN input must be 3 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "LEAK_AREA" Then
                        leakasarea = True
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            leakareas(1) = NMList.ForNMListVarGetNum(i, j, 1)
                            leakareas(2) = NMList.ForNMListVarGetNum(i, j, 2)
                        Else
                            myErrors.Add("In COMP namelist for LEAK_AREA input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "LEAK_AREA_RATIO" Then
                        leakasratio = True
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            leakratios(1) = NMList.ForNMListVarGetNum(i, j, 1)
                            leakratios(2) = NMList.ForNMListVarGetNum(i, j, 2)
                        Else
                            myErrors.Add("In COMP namelist for LEAK_AREA_RATIO input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In COMP namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then valid = False
                If depth <= 0 Then valid = False
                If width <= 0 Then valid = False
                If height <= 0 Then valid = False
                If valid Then
                    Dim aComp As New Compartment
                    someCompartments.Add(aComp)
                    aComp.Name = id
                    aComp.SetSize(width, depth, height)
                    For k = 1 To 3
                        If ceilid(k) <> "" Then
                            If myThermalProperties.GetIndex(ceilid(k)) < 0 And ceilid(k) <> "OFF" Then
                                ceilid(k) = "OFF"
                                myErrors.Add("In COMP namelist " + id + " CEILING_MATL_ID " + ceilid(k) + " is not valid; switching ceiling to OFF", ErrorMessages.TypeWarning)
                            Else
                                If ceilid(k) <> "OFF" And ceilthick(k) = 0 Then ceilthick(k) = myThermalProperties(myThermalProperties.GetIndex(ceilid(k))).Thickness
                            End If
                        End If
                        If wallid(k) <> "" Then
                            If myThermalProperties.GetIndex(wallid(k)) < 0 And wallid(k) <> "OFF" Then
                                wallid(k) = "OFF"
                                myErrors.Add("In COMP namelist " + id + " WALL_MATL_ID " + wallid(k) + " is not valid; switching wall to OFF", ErrorMessages.TypeWarning)
                            Else
                                If wallid(k) <> "OFF" And wallthick(k) = 0 Then wallthick(k) = myThermalProperties(myThermalProperties.GetIndex(wallid(k))).Thickness
                            End If
                        End If
                        If floorid(k) <> "" Then
                            If myThermalProperties.GetIndex(floorid(k)) < 0 And floorid(k) <> "OFF" Then
                                floorid(k) = "OFF"
                                myErrors.Add("In COMP namelist " + id + "FLOOR_MATL_ID " + floorid(k) + " is not valid switching floor to OFF", ErrorMessages.TypeWarning)
                            Else
                                If floorid(k) <> "OFF" And floorthick(k) = 0 Then floorthick(k) = myThermalProperties(myThermalProperties.GetIndex(floorid(k))).Thickness
                            End If
                        End If
                    Next
                    aComp.SetMaterial("Ceiling", ceilid(1), ceilid(2), ceilid(3))
                    aComp.SetMaterial("Walls", wallid(1), wallid(2), wallid(3))
                    aComp.SetMaterial("Floor", floorid(1), floorid(2), floorid(3))
                    aComp.SetThickness("Ceiling", ceilthick(1), ceilthick(2), ceilthick(3))
                    aComp.SetThickness("Walls", wallthick(1), wallthick(2), wallthick(3))
                    aComp.SetThickness("Floor", floorthick(1), floorthick(2), floorthick(3))
                    aComp.SetPosition(origin(LocationNum.x), origin(LocationNum.y), origin(LocationNum.z))
                    If comparea.GetUpperBound(0) > 0 Then
                        aComp.SetVariableArea(comparea, compheight)
                    End If
                    If hall Then
                        aComp.Hall = True
                    End If
                    If shaft Then
                        aComp.Shaft = True
                    End If
                    If leakasarea = True And leakareas(1) >= 0 And leakareas(2) >= 0 Then
                        aComp.WallLeak = leakareas(1) / (2 * height * (width + depth))
                        aComp.FloorLeak = leakareas(2) / (width * depth)
                    ElseIf leakasarea = True Then
                        myErrors.Add("In COMP namelist for LEAK_AREA input must be 2 positive numbers", ErrorMessages.TypeFatal)
                    End If
                    If leakasratio = True And leakratios(1) >= 0 And leakratios(2) >= 0 Then
                        aComp.WallLeak = leakratios(1)
                        aComp.FloorLeak = leakratios(2)
                    ElseIf leakasratio = True Then
                        myErrors.Add("In COMP namelist for LEAK_AREA_RATIO input must be 2 positive numbers", ErrorMessages.TypeFatal)
                    End If
                    aComp.FYI = fyi
                    aComp.Changed = False
                Else
                    myErrors.Add("In COMP namelist " + id + " is not fully defined", ErrorMessages.TypeWarning)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLDevc(NMList As NameListFile, ByRef someDetectors As TargetCollection)
        Dim i, j, k, max As Integer
        Dim compid, matlid, id, type, fyi, targetfacing As String
        Dim tempdepthunits As String = "FRACTION"
        Dim thickness, fixedtemperature, tempdepth, rti, setp, setps(2), sprayd As Double
        Dim loc(3), norm(3), coeffs(2) As Double
        Dim valid, adiabatic As Boolean
        Dim aTempOffset As Double = 273.15

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "DEVC") Then
                adiabatic = False
                coeffs(1) = 0
                coeffs(2) = 0
                tempdepth = 0.5
                rti = 130
                setp = -1
                id = ""
                type = ""
                compid = ""
                matlid = ""
                fyi = ""
                targetfacing = ""
                fixedtemperature = -1001
                thickness = 0

                For k = 0 To 2
                    loc(k) = -1
                    norm(k) = 0
                Next
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "COMP_ID") Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MATL_ID") Then
                        matlid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TYPE") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = "PLATE" Or NMList.ForNMListVarGetStr(i, j, 1) = "CYLINDER" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "SPRINKLER" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                            If setp <= 0 Then setp = Target.SprinklerActivationTemperature
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "HEAT_DETECTOR" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                            If setp <= 0 Then setp = Target.HeatDetectorActiviationTemperature
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "SMOKE_DETECTOR" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                            If setp <= 0 Then setp = Target.SmokeDetectorActivationObscuration
                        Else
                            myErrors.Add("In DEVC namelist for TYPE " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "THICKNESS" Then
                        thickness = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEPTH_UNITS" Then
                        tempdepthunits = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TEMPERATURE_DEPTH" Then
                        tempdepth = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FRONT_SURFACE_TEMPERATURE" Then
                        fixedtemperature = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SURFACE_TEMPERATURE" Then
                        fixedtemperature = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "RTI" Then
                        rti = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINT" Then
                        setp = NMList.ForNMListVarGetNum(i, j, 1)
                        setps = {0, setp}
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINTS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                setps(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC namelist for SETPOINTS input must be 2 numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "SPRAY_DENSITY" Then
                        sprayd = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "LOCATION" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 3 And max <= 3 Then
                            For k = 1 To max
                                loc(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC namelist for LOCATION input must be 3 numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "FRONT_SURFACE_ORIENTATION" Then
                        targetfacing = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SURFACE_ORIENTATION" Then
                        targetfacing = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "NORMAL" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= 3 Then
                            For k = 1 To max
                                norm(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC namelist for NORMAL input must be 3 numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "ADIABATIC_TARGET" Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            adiabatic = False
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            adiabatic = True
                        Else
                            myErrors.Add("In DEVC namelist for ADIABATIC_TARGET " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "CONVECTION_COEFFICIENTS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                coeffs(k) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC namelist for CONVECTION_COEFFICIENTS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In DEVC namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next

                valid = True
                If id = "" Then
                    valid = False
                    myErrors.Add("DEVC namelist is not a valid DEVC because it has no ID", ErrorMessages.TypeFatal)
                End If
                If type = "" Then
                    valid = False
                    myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because it has no type", ErrorMessages.TypeFatal)
                End If
                If compid = "" Then
                    valid = False
                    myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because it has no COMP_ID", ErrorMessages.TypeFatal)
                ElseIf myCompartments.GetCompIndex(compid) < 0 Then
                    valid = False
                    myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because COMP_ID " + compid + " does not refernce a valid compartment", ErrorMessages.TypeFatal)
                End If
                If valid Then
                    Dim aDetect As New Target
                    If type = "PLATE" Or type = "CYLINDER" Then
                        aDetect.Type = Target.TypeTarget
                        Dim atype As Integer
                        If type = "CYLINDER" Then
                            atype = Target.Cylindrical
                        Else ' PDE
                            atype = Target.ThermallyThick
                        End If
                        aDetect.SetTarget(myCompartments.GetCompIndex(compid), matlid, atype)
                        aDetect.SetPosition(loc(LocationNum.x), loc(LocationNum.y), loc(LocationNum.z), norm(LocationNum.x), norm(LocationNum.y), norm(LocationNum.z))
                        If targetfacing.ToUpper = "CEILING" Then aDetect.TargetFacing = "Ceiling"
                        If targetfacing.ToUpper = "FLOOR" Then aDetect.TargetFacing = "Floor"
                        If targetfacing.ToUpper = "FRONT WALL" Then aDetect.TargetFacing = "Front Wall"
                        If targetfacing.ToUpper = "BACK WALL" Then aDetect.TargetFacing = "Back Wall"
                        If targetfacing.ToUpper = "LEFT WALL" Then aDetect.TargetFacing = "Left Wall"
                        If targetfacing.ToUpper = "RIGHT WALL" Then aDetect.TargetFacing = "Right Wall"
                        If InStr(targetfacing, "Fire", CompareMethod.Text) > 0 Then aDetect.TargetFacingNoCheck = targetfacing ' for now, assume it's a valid fire name.  Check comes later
                        If thickness < 0 Then
                            myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because thickness is less than zero", ErrorMessages.TypeFatal)
                        End If
                        If thickness > 0 Then
                            aDetect.Thickness = thickness
                        End If
                        If tempdepthunits = "FRACTION" Then
                            Dim MaterialIndex As Integer = myThermalProperties.GetIndex(matlid)
                            If MaterialIndex >= 0 Then
                                If thickness = 0 Then thickness = myThermalProperties(MaterialIndex).Thickness
                                aDetect.InternalLocation = Math.Max(0, Math.Min(tempdepth * thickness, thickness))
                            Else
                                aDetect.InternalLocation = tempdepth
                                myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because material ID has not been set", ErrorMessages.TypeFatal)
                            End If
                        Else
                            aDetect.InternalLocation = tempdepth
                        End If
                        If (fixedtemperature <= -273.15) Or (fixedtemperature > 1000) Then
                            If fixedtemperature <> -1001 Then myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because front surface temperature is out of range", ErrorMessages.TypeFatal)
                        Else
                            aDetect.FixedTemperature = fixedtemperature
                        End If
                        aDetect.Name = id
                        aDetect.FYI = fyi
                        If adiabatic = True Then
                            aDetect.Adiabatic = adiabatic
                            aDetect.Convection_Coefficient(1) = coeffs(1)
                            aDetect.Convection_Coefficient(2) = coeffs(2)
                        End If

                        aDetect.Changed = False
                        myTargets.Add(aDetect)
                    Else
                        aDetect.Type = Target.TypeDetector
                        aDetect.Name = id
                        aDetect.FYI = fyi
                        aDetect.Compartment = myCompartments.GetCompIndex(compid)
                        If type = "HEAT_DETECTOR" Then
                            aDetect.DetectorType = Target.TypeHeatDetector
                            aDetect.RTI = rti
                            aDetect.ActivationTemperature = setp + aTempOffset
                            aDetect.SprayDensity = 0.0
                        ElseIf type = "SMOKE_DETECTOR" Then
                            aDetect.DetectorType = Target.TypeSmokeDetector
                            aDetect.ActivationObscurationSmoldering = setps(0)
                            aDetect.ActivationObscurationFlaming = setps(1)
                        Else
                            aDetect.DetectorType = Target.TypeSprinkler
                            aDetect.RTI = rti
                            aDetect.ActivationTemperature = setp + aTempOffset
                            aDetect.SprayDensity = sprayd
                        End If
                        aDetect.SetPosition(loc(LocationNum.x), loc(LocationNum.y), loc(LocationNum.z), norm(LocationNum.x), norm(LocationNum.y), norm(LocationNum.z))
                        aDetect.Changed = False
                        myDetectors.Add(aDetect)
                    End If
                Else
                    myErrors.Add("In DEVC namelist " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLFire(NMList As NameListFile, ByRef someFireInstances As FireCollection)
        Dim i, j, k, max As Integer
        Dim compid, id, devcid, ignitcrit, fireid, fyi As String
        Dim setp As Double
        Dim loc(1) As Double
        Dim aDummy As Double = 273.15

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "FIRE") Then
                id = ""
                compid = ""
                devcid = ""
                fireid = ""
                ignitcrit = ""
                setp = 0
                loc(0) = -1.0
                loc(1) = -1.0
                fyi = ""
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "COMP_ID" Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEVC_ID" Then
                        devcid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FIRE_ID" Then
                        fireid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "IGNITION_CRITERION" Then
                        ignitcrit = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINT" Then
                        setp = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "LOCATION" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                loc(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In FIRE namelist for LOCATION input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In FIRE namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aFire As New Fire()
                aFire.ObjectType = Fire.TypeInstance
                someFireInstances.Add(aFire)
                aFire.Name = id
                aFire.FYI = fyi
                aFire.Compartment = myCompartments.GetCompIndex(compid)
                If ignitcrit = "TIME" Then
                    aFire.IgnitionType = Fire.FireIgnitionbyTime
                    aFire.IgnitionValue = setp
                ElseIf ignitcrit = "TEMPERATURE" Then
                    aFire.Target = devcid
                    aFire.IgnitionType = Fire.FireIgnitionbyTemperature
                    aFire.IgnitionValue = setp + aDummy
                ElseIf ignitcrit = "FLUX" Then
                    aFire.Target = devcid
                    aFire.IgnitionType = Fire.FireIgnitionbyFlux
                    aFire.IgnitionValue = setp * 1000.0
                End If
                aFire.XPosition = loc(0)
                aFire.YPosition = loc(1)
                aFire.ReferencedFireDefinition = fireid
                aFire.Changed = False
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLChem(NMList As NameListFile, ByRef someFires As FireCollection)
        Dim i, j As Integer
        Dim id As String
        Dim carbon, chlorine, flametime, hoc, hydrogen, nitrogen, oxygen, radfrac As Double
        Dim aFireCurves(12, 0) As Double
        Dim valid As Boolean

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "CHEM") Then
                carbon = 1
                chlorine = 0
                flametime = 0
                hoc = 50000
                hydrogen = 4
                nitrogen = 0
                oxygen = 0
                radfrac = 0.35
                id = ""
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CARBON" Then
                        carbon = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CHLORINE" Then
                        chlorine = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FLAMING_TRANSITION_TIME" Then
                        flametime = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEAT_OF_COMBUSTION" Then
                        hoc = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HYDROGEN" Then
                        hydrogen = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "NITROGEN" Then
                        nitrogen = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "OXYGEN" Then
                        oxygen = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "RADIATIVE_FRACTION" Then
                        radfrac = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In CHEM namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then
                    myErrors.Add("In CHEM namelist ID parameter must be set", ErrorMessages.TypeFatal)
                    valid = False
                Else
                    Dim aFireObject As New Fire()
                    aFireObject.ObjectType = Fire.TypeDefinition
                    aFireObject.Name = id
                    aFireObject.ChemicalFormula(formula.C) = carbon
                    aFireObject.ChemicalFormula(formula.H) = hydrogen
                    aFireObject.ChemicalFormula(formula.O) = oxygen
                    aFireObject.ChemicalFormula(formula.N) = nitrogen
                    aFireObject.ChemicalFormula(formula.Cl) = chlorine
                    aFireObject.HeatofCombustion = hoc * 1000.0
                    aFireObject.RadiativeFraction = radfrac
                    aFireObject.FlamingTransitionTime = flametime
                    ReadInputFileNMLTabl(NMList, id, aFireCurves, valid)
                    aFireObject.SetFireData(aFireCurves)
                    aFireObject.Changed = False
                    someFires.Add(aFireObject)
                End If
                If valid = False Then
                    myErrors.Add("In FIRE namelist " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLTabl(NMList As NameListFile, id As String, ByRef aFireCurves(,) As Double, ByRef Valid As Boolean)
        Dim i, j, k, m, n, max As Integer
        Dim aMap(8) As Integer
        Dim labels(8) As String
        Dim LabelFlag, IDFlag As Boolean

        Valid = True
        Dim aFire As New Fire()
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "TABL") Then
                LabelFlag = False
                IDFlag = False
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        If id = NMList.ForNMListVarGetStr(i, j, 1) Then
                            IDFlag = True
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "LABELS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 9 Then
                            LabelFlag = True
                            For k = 1 To max
                                labels(k - 1) = NMList.ForNMListVarGetStr(i, j, k)
                            Next
                        Else
                            myErrors.Add("In TABL namelist for LABELS input must include between 2 and 8 labels ", ErrorMessages.TypeFatal)
                            Valid = False
                        End If
                    End If
                Next
            End If
            If IDFlag And LabelFlag Then
                Exit For
            End If
        Next
        If LabelFlag Then
            For i = 0 To max - 1
                For j = 0 To aFire.ColMapUpperBound
                    If labels(i) = aFire.ColNames(aFire.ColMap(j)) Then
                        aMap(i) = aFire.ColMap(j)
                    End If
                Next
            Next
        Else
            myErrors.Add("In TABL namelist LABELS keyword not found for FIRE " + id, ErrorMessages.TypeFatal)
            Valid = False
            Exit Sub
        End If

        m = 0
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "TABL") Then
                n = -1
                IDFlag = False
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        If id = NMList.ForNMListVarGetStr(i, j, 1) Then
                            IDFlag = True
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "DATA" Then
                        n = j
                    End If
                Next
                If IDFlag Then
                    If n > 0 Then
                        max = NMList.ForNMListVarNumVal(i, n)
                        ReDim Preserve aFireCurves(12, m)
                        For k = 1 To 12
                            aFireCurves(k, m) = 0.0
                        Next
                        For k = 1 To max
                            aFireCurves(aMap(k - 1), m) = NMList.ForNMListVarGetNum(i, n, k)
                            If aMap(k - 1) = 2 Then aFireCurves(aMap(k - 1), m) *= 1000
                        Next
                        m += 1
                    End If
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLVent(NMList As NameListFile, ByRef someHVents As VentCollection, ByRef someMVents As VentCollection, ByRef someVVents As VentCollection)
        Dim i, j, k, max As Integer
        Dim area, areas(2), bot, top, height, width, cutoffs(2), flow, heights(2), offset, offsets(2), setp, prefrac, postfrac, filttime, filteff, coeff As Double
        Dim tt(0), ff(0) As Double
        Dim compids(2), filtramp, face, orien(2), shape, type, id, crit, devcid, fyi As String
        Dim valid As Boolean
        Dim comp0dx, comp1dx As Integer

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "VENT") Then
                area = -1
                bot = 0
                top = 0
                height = 0
                coeff = 0
                crit = "TIME"
                devcid = ""
                setp = 0.0
                prefrac = 1
                postfrac = 1
                For k = 0 To 1
                    compids(k) = ""
                    orien(k) = "VERTICAL"
                Next
                face = ""
                filtramp = ""
                filttime = 0
                filteff = 0.0
                flow = -1
                id = ""
                fyi = ""
                For k = 0 To 1
                    areas(k) = -1
                    cutoffs(k) = -1
                    heights(k) = -1
                    offsets(k) = -1
                Next
                shape = ""
                type = ""
                width = -1
                ReDim ff(0), tt(0)
                For j = 1 To NMList.ForNMListNumVar(i)
                    max = 0
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CRITERION" Then
                        crit = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEVC_ID" Then
                        devcid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FACE" Then
                        face = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FILTERING_RAMP_ID" Then
                        filtramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "ORIENTATIONS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                orien(k - 1) = NMList.ForNMListVarGetStr(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for COMP_IDS there must be 2 compartment IDs", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "SHAPE" Then
                        shape = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TYPE" Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "COMP_IDS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                compids(k - 1) = NMList.ForNMListVarGetStr(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for COMP_IDS there must be 2 compartment IDs", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREA" Then
                        area = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "BOTTOM" Then
                        bot = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TOP" Then
                        top = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEIGHT" Then
                        height = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "VENT_FLOW_COEFFICIENT" Then
                        coeff = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FILTER_TIME" Then
                        filttime = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FILTER_EFFICIENCY" Then
                        filteff = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FLOW" Then
                        flow = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "OFFSET" Then
                        offset = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "PRE_FRACTION" Then
                        prefrac = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "POST_FRACTION" Then
                        postfrac = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINT" Then
                        setp = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "WIDTH" Then
                        width = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREAS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                areas(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for AREAS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "CUTOFFS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                cutoffs(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for CUTOFFS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEIGHTS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                heights(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for HEIGHTS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "OFFSETS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                offsets(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for OFFSETS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "T" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim tt(max)
                            For k = 1 To max
                                tt(k) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for T input must be at least 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "F" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim ff(max)
                            For k = 1 To max
                                ff(k) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for F input must be at least 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In VENT namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then
                    valid = False
                    myErrors.Add("In VENT namelist ID must be defined", ErrorMessages.TypeFatal)
                End If
                For k = 0 To 1
                    If myCompartments.GetCompIndex(compids(k)) < 0 And compids(k) <> "OUTSIDE" Then
                        valid = False
                        myErrors.Add("In VENT namelist " + id + " does Not have two valid compartments", ErrorMessages.TypeFatal)
                    End If
                Next
                If type <> "CEILING" And type <> "FLOOR" And type <> "MECHANICAL" And type <> "WALL" Then
                    valid = False
                    myErrors.Add("In VENT namelist " + id + " TYPE Is Not set to a valid value", ErrorMessages.TypeFatal)
                End If
                If valid Then
                    Dim aVent As New Vent
                    If type = "WALL" Then
                        aVent.VentType = Vent.TypeHVent
                        aVent.Name = id
                        aVent.FYI = fyi
                        If compids(0) = "OUTSIDE" Then
                            comp0dx = -1
                        Else
                            comp0dx = myCompartments.GetCompIndex(compids(0))
                        End If
                        If compids(1) = "OUTSIDE" Then
                            comp1dx = -1
                        Else
                            comp1dx = myCompartments.GetCompIndex(compids(1))
                        End If
                        If height < 0 Then
                            myErrors.Add("In &VENT namelist " + NMList.ForNMListGetVar(i, j) + "  HEIGHT must be greater than zero.", ErrorMessages.TypeFatal)
                        End If
                        aVent.SetVent(comp0dx, comp1dx, width, height, bot, top)
                        If coeff > 0 Then
                            aVent.Coeff = coeff
                        End If
                        ' This is the new format that includes trigger by flux or temperature
                        If face = "FRONT" Then
                            aVent.Face = 1
                        ElseIf face = "RIGHT" Then
                            aVent.Face = 2
                        ElseIf face = "REAR" Then
                            aVent.Face = 3
                        ElseIf face = "LEFT" Then
                            aVent.Face = 4
                        End If
                        aVent.Offset = offset
                        If crit = "TIME" Then
                            aVent.OpenType = Vent.OpenbyTime
                            aVent.SetRamp(tt, ff)
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp * 1000
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp + 273.15
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        someHVents.Add(aVent)
                    ElseIf type = "MECHANICAL" Then
                        aVent.VentType = Vent.TypeMVent
                        aVent.Name = id
                        aVent.FYI = fyi
                        If compids(0) = "OUTSIDE" Then
                            aVent.FirstCompartment = -1
                        Else
                            aVent.FirstCompartment = myCompartments.GetCompIndex(compids(0))
                        End If
                        If compids(1) = "OUTSIDE" Then
                            aVent.SecondCompartment = -1
                        Else
                            aVent.SecondCompartment = myCompartments.GetCompIndex(compids(1))
                        End If
                        aVent.SetDefaultLocation()
                        aVent.FirstArea = areas(0)
                        aVent.SecondArea = areas(1)
                        aVent.FirstCenterHeight = heights(0)
                        aVent.SecondCenterHeight = heights(1)
                        If orien(0) = "HORIZONTAL" Then
                            aVent.FirstOrientation = 2
                        Else
                            aVent.FirstOrientation = 1
                        End If
                        If orien(0) = "HORIZONTAL" Then
                            aVent.SecondOrientation = 2
                        Else
                            aVent.SecondOrientation = 1
                        End If
                        aVent.OffsetX = offsets(0)
                        aVent.OffsetY = offsets(1)
                        aVent.BeginFlowDropoff = cutoffs(0)
                        aVent.ZeroFlow = cutoffs(1)
                        aVent.FilterTime = filttime
                        aVent.FilterEfficiency = filteff
                        aVent.FlowRate = flow
                        If crit = "TIME" Then
                            aVent.OpenType = Vent.OpenbyTime
                            aVent.SetRamp(tt, ff)
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp * 1000
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp + 273.15
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        someMVents.Add(aVent)
                    ElseIf type = "CEILING" Or type = "FLOOR" Then
                        aVent.VentType = Vent.TypeVVent
                        aVent.Name = id
                        aVent.FYI = fyi
                        aVent.Area = area
                        If compids(0) = "OUTSIDE" Then
                            aVent.FirstCompartment = -1
                        Else
                            aVent.FirstCompartment = myCompartments.GetCompIndex(compids(0))
                        End If
                        If compids(1) = "OUTSIDE" Then
                            aVent.SecondCompartment = -1
                        Else
                            aVent.SecondCompartment = myCompartments.GetCompIndex(compids(1))
                        End If
                        If shape = "ROUND" Then
                            aVent.Shape = 1
                        Else
                            aVent.Shape = 2
                        End If
                        aVent.OffsetX = offsets(0)
                        aVent.OffsetY = offsets(1)
                        If crit = "TIME" Then
                            aVent.OpenType = Vent.OpenbyTime
                            aVent.SetRamp(tt, ff)
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp * 1000
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp + 273.15
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        someVVents.Add(aVent)
                    End If
                Else
                    myErrors.Add("In VENT namelist " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLConn(NMList As NameListFile, ByRef someHHeats As VentCollection, ByRef someVHeats As VentCollection)
        Dim i, j, k, max, cFirst, cSecond As Integer
        Dim compid, compids(1), type As String
        Dim f(1) As Double

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "CONN") Then
                compid = ""
                type = ""
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "COMP_ID" Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "COMP_IDS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim compids(max - 1)
                            For k = 1 To max
                                compids(k - 1) = NMList.ForNMListVarGetStr(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for COMP_IDS there must be at least 1 compartment ID", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "F" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim f(max - 1)
                            For k = 1 To max
                                f(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT namelist for F input must be 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "TYPE" Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In CONN namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                If type = "WALL" Then
                    max = compids.GetUpperBound(0)
                    cFirst = myCompartments.GetCompIndex(compid)
                    For k = 0 To max
                        If compids(k) = "OUTSIDE" Then
                            cSecond = -1
                        Else
                            cSecond = myCompartments.GetCompIndex(compids(k))
                        End If
                        Dim aHeat As New Vent
                        aHeat.SetVent(cFirst, cSecond, f(k))
                        aHeat.Changed = False
                        someHHeats.Add(aHeat)
                    Next
                Else
                    If compid = "OUTSIDE" Then
                        cFirst = -1
                    Else
                        cFirst = myCompartments.GetCompIndex(compid)
                    End If
                    If compids(0) = "OUTSIDE" Then
                        cSecond = -1
                    Else
                        cSecond = myCompartments.GetCompIndex(compids(0))
                    End If
                    Dim aHeat As New Vent
                    aHeat.SetVent(cFirst, cSecond)
                    aHeat.Changed = False
                    someVHeats.Add(aHeat)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLISOF(NMList As NameListFile, ByRef someVisuals As VisualCollection)
        Dim i, j As Integer
        Dim compid As String
        Dim value As Double
        Dim aTempOffset As Double = 273.15

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "ISOF") Then
                compid = ""
                value = -1
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "COMP_ID" Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "VALUE" Then
                        value = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In ISOF namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aVisual As New Visual
                aVisual.Type = Visual.IsoSurface
                If compid <> "" Then
                    aVisual.Compartment = myCompartments.GetCompIndex(compid)
                Else
                    aVisual.Compartment = -1
                End If
                aVisual.Value = value + aTempOffset
                aVisual.Changed = False
                someVisuals.Add(aVisual)
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLSLCF(NMList As NameListFile, ByRef someVisuals As VisualCollection)
        Dim i, j As Integer
        Dim compid, domain, plane As String
        Dim pos As Double

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "SLCF") Then
                compid = ""
                domain = ""
                plane = ""
                pos = -1
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "COMP_ID" Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "DOMAIN" Then
                        domain = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "PLANE" Then
                        plane = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "POSITION" Then
                        pos = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In SLCF namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aVisual As New Visual
                If compid = "" Then
                    aVisual.Compartment = -1
                Else
                    aVisual.Compartment = myCompartments.GetCompIndex(compid)
                End If
                If domain = "2-D" Then
                    aVisual.Type = Visual.TwoD
                    If plane = "X" Then
                        aVisual.Axis = 0
                    ElseIf plane = "Y" Then
                        aVisual.Axis = 1
                    ElseIf plane = "Z" Then
                        aVisual.Axis = 2
                    End If
                    aVisual.Value = pos
                Else
                    aVisual.Type = Visual.ThreeD
                End If
                aVisual.Changed = False
                someVisuals.Add(aVisual)
            End If
        Next

    End Sub
    Public Sub ReadInputFileNMLDiag(NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j, k, max As Integer
        Dim f(0), t(0) As Double
        Dim ppco2, pph2o, gastemp, ULThickness, verificationstep, fireheatflux As Double
        Dim radsolv As String
        Dim fire, hflow, entrain, vflow, cjet, dfire, convec, rad, gasabsorp, conduc, debugprn, mflow, keyin, steadyint, dasslprn, oxygen As Integer
        Dim residdbprn, layermixing As Integer
        Dim dummy As String
        Dim adiabatic As Boolean, fluxAST As Double

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "DIAG") Then
                ReDim f(0), t(0)
                f(0) = Environment.DefaultNonValue
                t(0) = Environment.DefaultNonValue
                ppco2 = Environment.DefaultNonValue
                pph2o = Environment.DefaultNonValue
                gastemp = Environment.DefaultNonValue
                ULThickness = Environment.DefaultNonValue
                verificationstep = Environment.DefaultNonValue
                fireheatflux = Environment.DefaultNonValue
                radsolv = "DEFAULT"
                adiabatic = False
                fluxAST = 0
                fire = Environment.DIAGon
                hflow = Environment.DIAGon
                entrain = Environment.DIAGon
                vflow = Environment.DIAGon
                cjet = Environment.DIAGon
                dfire = Environment.DIAGon
                convec = Environment.DIAGon
                rad = Environment.DIAGon
                gasabsorp = Environment.DIAGon
                conduc = Environment.DIAGon
                debugprn = Environment.DIAGoff
                mflow = Environment.DIAGon
                keyin = Environment.DIAGon
                steadyint = Environment.DIAGoff
                dasslprn = Environment.DIAGoff
                oxygen = Environment.DIAGoff
                residdbprn = Environment.DIAGoff
                layermixing = Environment.DIAGon
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "RADSOLVER" Then
                        dummy = NMList.ForNMListVarGetStr(i, j, 1)
                        If dummy = "DEFAULT" Then
                            radsolv = "DEFAULT"
                        ElseIf dummy = "RADNET" Then
                            radsolv = "RADNET"
                        Else
                            myErrors.Add("In DIAG namelist for " + "RADSOLVER" + " " + dummy + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "GAS_TEMPERATURE" Then
                        gastemp = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "PARTIAL_PRESSURE_H2O" Then
                        pph2o = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "PARTIAL_PRESSURE_CO2" Then
                        ppco2 = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "T" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim t(max - 1)
                            For k = 1 To max
                                t(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DIAG namelist for T input must be at least 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "F" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim f(max - 1)
                            For k = 1 To max
                                f(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DIAG namelist for F input must be at least 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "ADIABATIC_TARGET_VERIFICATION" Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = "ON" Then
                            adiabatic = True
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "OFF" Then
                            adiabatic = False
                        Else
                            myErrors.Add("In DIAG namelist for ADIABATIC_TARGET_VERIFICATION " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either ON or OFF", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "RADIATIVE_INCIDENT_FLUX" Then
                        fluxAST = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "UPPER_LAYER_THICKNESS" Then
                        ULThickness = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "VERIFICATION_FIRE_HEAT_FLUX" Then
                        fireheatflux = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "VERIFICATION_TIME_STEP" Then
                        verificationstep = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FIRE_SUB_MODEL" Then
                        ReadINIInput(fire, "FIRE_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "HORIZONTAL_FLOW_SUB_MODEL" Then
                        ReadINIInput(hflow, "HORIZONTAL_FLOW_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "ENTRAINMENT_SUB_MODEL" Then
                        ReadINIInput(entrain, "ENTRAINMENT_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "VERTICAL_FLOW_SUB_MODEL" Then
                        ReadINIInput(vflow, "VERTICAL_FLOW_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "CEILING_JET_SUB_MODEL" Then
                        ReadINIInput(cjet, "CEILING_JET_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "DOOR_JET_FIRE_SUB_MODEL" Then
                        ReadINIInput(dfire, "DOOR_JET_FIRE_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "CONVECTION_SUB_MODEL" Then
                        ReadINIInput(convec, "CONVECTION_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "RADIATION_SUB_MODEL" Then
                        ReadINIInput(rad, "RADIATION_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "GAS_ABSORBTION_SUB_MODEL" Then
                        dummy = NMList.ForNMListVarGetStr(i, j, 1)
                        If dummy = "CALCULATED" Then
                            gasabsorp = Environment.DIAGon
                        ElseIf dummy = "CONSTANT" Then
                            gasabsorp = Environment.DIAGoff
                        Else
                            myErrors.Add("In DIAG namelist for " + "GAS_ABSORBTION_SUB_MODEL" + " " + dummy + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "CONDUCTION_SUB_MODEL" Then
                        ReadINIInput(conduc, "CONDUCTION_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEBUG_PRINT" Then
                        ReadINIInput(debugprn, "DEBUG_PRINT", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "MECHANICAL_FLOW_SUB_MODEL" Then
                        ReadINIInput(mflow, "MECHANICAL_FLOW_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "KEYBOARD_INPUT" Then
                        ReadINIInput(keyin, "KEYBOARD_INPUT", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "STEADY_STATE_INITIAl_CONDITIONS" Then
                        ReadINIInput(steadyint, "STEADY_STATE_INITIAL_CONDITIONS", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "DASSL_DEBUG_PRINT" Then
                        ReadINIInput(dasslprn, "DASSL_DEBUG_PRINT", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "OXYGEN_TRACKING" Then
                        ReadINIInput(oxygen, "OXYGEN_TRACKING", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "RESIDUAL_DEBUG_PRINT" Then
                        ReadINIInput(residdbprn, "RESIDUAL_DEBUG_PRINT", NMList.ForNMListVarGetStr(i, j, 1))
                    ElseIf NMList.ForNMListGetVar(i, j) = "LAYER_MIXING_SUB_MODEL" Then
                        ReadINIInput(layermixing, "LAYER_MIXING_SUB_MODEL", NMList.ForNMListVarGetStr(i, j, 1))
                    Else
                        myErrors.Add("In DIAG namelist " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                someEnvironment.SetDIAGf(f)
                someEnvironment.SetDIAGt(t)
                someEnvironment.DIAGGasTemp = gastemp
                someEnvironment.DIAGPartPressCO2 = ppco2
                someEnvironment.DIAGPartPressH2O = pph2o
                someEnvironment.DIAGUpperLayerThickness = ULThickness
                someEnvironment.DIAGFireHeatFlux = fireheatflux
                someEnvironment.DIAGVerificationTimeStep = verificationstep
                someEnvironment.DIAGRadSolver = radsolv
                someEnvironment.DIAGAdiabaticTargetVerification = adiabatic
                someEnvironment.DIAGAdiabaticTargetFlux = fluxAST
                someEnvironment.DIAGfire = fire
                someEnvironment.DIAGhflow = hflow
                someEnvironment.DIAGentrain = entrain
                someEnvironment.DIAGvflow = vflow
                someEnvironment.DIAGcjet = cjet
                someEnvironment.DIAGdfire = dfire
                someEnvironment.DIAGconvec = convec
                someEnvironment.DIAGrad = rad
                someEnvironment.DIAGconduc = conduc
                someEnvironment.DIAGdebugprn = debugprn
                someEnvironment.DIAGmflow = mflow
                someEnvironment.DIAGkeyin = keyin
                someEnvironment.DIAGsteadyint = steadyint
                someEnvironment.DIAGdasslprn = dasslprn
                someEnvironment.DIAGoxygen = oxygen
                someEnvironment.DIAGresiddbprn = residdbprn
                someEnvironment.DIAGlayermixing = layermixing
            End If
        Next
        someEnvironment.Changed = False
    End Sub
    Private Sub ReadInputFileNMLOutp(NMList As NameListFile, ByRef someOutputs As MonteCarloCollection)
        Dim i, j As Integer
        Dim id, filetype, type, firstmeasurement, secondmeasurement, firstdevice, seconddevice, fyi As String
        Dim criterion As Double

        For i = 1 To NMList.TotNMList
            id = ""
            filetype = ""
            type = ""
            criterion = 0
            firstmeasurement = ""
            firstdevice = ""
            secondmeasurement = ""
            seconddevice = ""
            fyi = ""
            If (NMList.GetNMListID(i) = "DUMP" Or NMList.GetNMListID(i) = "OUTP") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FILE") Then
                        filetype = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TYPE") Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CRITERION") Then
                        criterion = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRST_DEVICE") Then
                        firstdevice = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRST_MEASUREMENT") Then
                        firstmeasurement = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SECOND_DEVICE") Then
                        seconddevice = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SECOND_MEASUREMENT") Then
                        secondmeasurement = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRST_FIELD") Then
                        firstdevice = NMList.ForNMListVarGetStr(i, j, 1)
                        firstmeasurement = NMList.ForNMListVarGetStr(i, j, 2)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SECOND_FIELD") Then
                        seconddevice = NMList.ForNMListVarGetStr(i, j, 1)
                        secondmeasurement = NMList.ForNMListVarGetStr(i, j, 2)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In OUTP namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aOutput As New MonteCarlo
                someOutputs.Add(aOutput)
                aOutput.SetOutput(id, filetype, type, criterion, firstmeasurement, firstdevice, secondmeasurement, seconddevice, fyi)
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLMHDR(NMList As NameListFile, ByRef someMHeaders As MonteCarloCollection)
        Dim i, j, k, max As Integer
        Dim NumberofCases As Integer
        Dim Seeds() As Double
        Dim WriteSeeds As Boolean
        Dim ParameterFile, WorkFolder, OutputFolder As String

        For i = 1 To NMList.TotNMList
            NumberofCases = 0
            ReDim Seeds(2)
            Seeds(1) = -1001
            Seeds(2) = -1001
            WriteSeeds = False
            ParameterFile = ""
            WorkFolder = ""
            OutputFolder = ""
            If (NMList.GetNMListID(i) = "MHDR") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "NUMBER_OF_CASES") Then
                        NumberofCases = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SEEDS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                Seeds(k) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In MHDR namelist for SEEDS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "WRITE_SEEDS" Then
                        WriteSeeds = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "PARAMETER_FILE" Then
                        ParameterFile = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "WORK_FOLDER" Then
                        WorkFolder = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "OUTPUT_FOLDER" Then
                        OutputFolder = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MHDR namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aHeader As New MonteCarlo
                aHeader.SetHeader(NumberofCases, Seeds, WriteSeeds, ParameterFile, WorkFolder, OutputFolder)
                someMHeaders.Add(aHeader)
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLMRND(NMList As NameListFile, ByRef someRandoms As MonteCarloCollection)
        Dim i, j, k As Integer
        Dim id As String
        Dim max As Integer
        Dim DistributionType As String, MinimumField As String, MaximumField As String, AddField As String, FYI As String
        Dim Minimum As Double, Maximum As Double, Mean As Double, Stdev As Double, Alpha As Double, Beta As Double, Peak As Double, RandomSeeds(2) As Double, RealValues() As Double, RealConstantValue As Double, Probabilities() As Double, MinimumOffset As Double, MaximumOffset As Double

        For i = 1 To NMList.TotNMList
            id = ""
            DistributionType = ""
            MinimumField = ""
            MaximumField = ""
            AddField = ""
            Minimum = 0.0
            Maximum = 0.0
            Mean = 0.0
            Stdev = 0.0
            Alpha = 0.0
            Beta = 0.0
            Peak = 0.0
            RandomSeeds(1) = -1001.0
            RandomSeeds(2) = -1001.0
            RealConstantValue = 0.0
            FYI = ""
            ReDim RealValues(0), Probabilities(0)

            If (NMList.GetNMListID(i) = "MRND") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "DISTRIBUTION_TYPE") Then
                        DistributionType = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MINIMUM") Then
                        Minimum = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MAXIMUM") Then
                        Maximum = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MEAN") Then
                        Mean = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "STDEV") Then
                        Stdev = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ALPHA") Then
                        Alpha = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "BETA") Then
                        Beta = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "PEAK") Then
                        Peak = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "VALUES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim RealValues(max)
                        For k = 1 To max
                            RealValues(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "PROBABILITIES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim Probabilities(max)
                        For k = 1 To max
                            Probabilities(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CONSTANT") Then
                        RealConstantValue = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MINIMUM_OFFSET") Then
                        MinimumOffset = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MAXIMUM_OFFSET") Then
                        MaximumOffset = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MINIMUM_FIELD") Then
                        MinimumField = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MAXIMUM_FIELD") Then
                        MaximumField = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_FIELD") Then
                        AddField = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FYI") Then
                        FYI = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MRND namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aRandom As New MonteCarlo
                aRandom.SetRandom(id, DistributionType, Minimum, Maximum, Mean, Stdev, Alpha, Beta, Peak, RandomSeeds, RealValues, RealConstantValue, Probabilities, MinimumOffset, MaximumOffset, MinimumField, MaximumField, AddField, FYI)
                someRandoms.Add(aRandom)
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLMFLD(NMList As NameListFile, ByRef someFields As MonteCarloCollection)
        Dim i, j, k As Integer
        Dim id As String, FYI As String
        Dim max As Integer
        Dim FieldType As String, Field(2) As String, RandId As String, ParameterColumnLabel As String, StringValues() As String
        Dim RealValues() As Double, BaseScalingValue As Double
        Dim IntegerValues() As Integer, Position As Integer
        Dim LogicalValues() As Boolean, AddToParameters As Boolean

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "MFLD") Then
                id = ""
                FieldType = ""
                Field(1) = ""
                Field(2) = ""
                RandId = ""
                ParameterColumnLabel = ""
                AddToParameters = False
                BaseScalingValue = 1.0
                Position = -1001
                FYI = ""
                ReDim RealValues(0), IntegerValues(0), StringValues(0), LogicalValues(0)

                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "RAND_ID") Then
                        RandId = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIELD_TYPE") Then
                        FieldType = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "REAL_VALUES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim RealValues(max)
                        For k = 1 To max
                            RealValues(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INTEGER_VALUES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim IntegerValues(max)
                        For k = 1 To max
                            IntegerValues(k) = NMList.ForNMListVarGetNum(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "STRING_VALUES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim StringValues(max)
                        For k = 1 To max
                            StringValues(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "LOGICAL_VALUES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim LogicalValues(max)
                        For k = 1 To max
                            LogicalValues(k) = NMList.ForNMListVarGetBool(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIELD") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 Then
                            Field(1) = NMList.ForNMListVarGetStr(i, j, 1)
                            Field(2) = NMList.ForNMListVarGetStr(i, j, 2)
                        Else
                            Field(1) = ""
                            Field(2) = ""
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "PARAMETER_COLUMN_LABEL") Then
                        ParameterColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_TO_PARAMETERS") Then
                        AddToParameters = False
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then AddToParameters = True
                    ElseIf (NMList.ForNMListGetVar(i, j) = "BASE_SCALING_VALUE") Then
                        BaseScalingValue = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "POSITION") Then
                        Position = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FYI") Then
                        FYI = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MFLD namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aField As New MonteCarlo
                aField.SetField(id, FieldType, Field, RandId, ParameterColumnLabel, AddToParameters, RealValues, IntegerValues, StringValues, LogicalValues, BaseScalingValue, Position, FYI)
                someFields.Add(aField)
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLMFIR(NMList As NameListFile, ByRef someFires As MonteCarloCollection)
        Dim i, j, k As Integer
        Dim max As Integer

        Dim id As String, FYI As String, FireID As String, BaseFireID As String, FireCompartmentRandomGeneratorID As String, FireCompartmentIDs() As String, FireCompartmentIDColumnLabel As String, FlamingSmolderingIncipientRandomGeneratorID As String
        Dim IncipientFireTypes() As String, TypeofIncipientFireGrowth As String, FlamingIncipientDelayRandomGeneratorID As String, FlamingIncipientPeakRandomGeneratorID As String, SmolderingIncipientDelayRandomGeneratorID As String, SmolderingIncipientPeakRandomGeneratorID As String
        Dim IncipientTypeColumnLabel As String, IncipientTimeColumnLabel As String, IncipientPeakColumnLabel As String, ScalingFireHRRRandomGeneratorID As String, ScalingFireTimeRandomGeneratorID As String
        Dim HRRScaleColumnLabel As String, TimeScaleColumnLabel As String, FireHRRGeneratorIDs() As String, FireTimeGeneratorIDs() As String, HRRLabels() As String, TimeLabels() As String
        Dim ModifyFireAreatoMatchHRR As Boolean, AddFireCompartmentIDtoParameters As Boolean, AddIncipientTypetoParameters As Boolean, AddIncipientTimetoParameters As Boolean, AddIncipientPeaktoParameters As Boolean
        Dim AddHRRScaletoParameters As Boolean, AddTimeScaletoParameters As Boolean, AddFiretoParameters As Boolean, AddHRRtoParameters As Boolean, AddTimetoParameters As Boolean
        Dim NumberofGrowthPoints As Integer, NumberofDecayPoints As Integer
        Dim GrowthExponent As Double, DecayExponent As Double, GeneratorIsTimeto1054kW As Boolean

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "MFIR") Then
                id = ""
                FYI = ""
                FireID = ""
                BaseFireID = ""
                FireCompartmentRandomGeneratorID = ""
                FireCompartmentIDColumnLabel = ""
                FlamingSmolderingIncipientRandomGeneratorID = ""
                TypeofIncipientFireGrowth = ""
                FlamingIncipientDelayRandomGeneratorID = ""
                FlamingIncipientPeakRandomGeneratorID = ""
                SmolderingIncipientDelayRandomGeneratorID = ""
                SmolderingIncipientPeakRandomGeneratorID = ""
                IncipientTypeColumnLabel = ""
                IncipientTimeColumnLabel = ""
                IncipientPeakColumnLabel = ""
                ScalingFireHRRRandomGeneratorID = ""
                ScalingFireTimeRandomGeneratorID = ""
                HRRScaleColumnLabel = ""
                TimeScaleColumnLabel = ""


                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_ID") Then
                        FireID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "BASE_FIRE_ID") Then
                        BaseFireID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FYI") Then
                        FYI = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MODIFY_FIRE_AREA_TO_MATCH_HRR") Then
                        ModifyFireAreatoMatchHRR = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FYI") Then
                        FYI = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_COMPARTMENT_RANDOM_GENERATOR_ID") Then
                        FireCompartmentRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_COMPARTMENT_IDS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim FireCompartmentIDs(max)
                        For k = 1 To max
                            FireCompartmentIDs(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_FIRE_COMPARTMENT_ID_TO_PARAMETERS") Then
                        AddFireCompartmentIDtoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_COMPARTMENT_ID_COLUMN_LABEL") Then
                        FireCompartmentIDColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)

                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLAMING_SMOLDERING_INCIPIENT_RANDOM_GENERATOR_ID") Then
                        FlamingSmolderingIncipientRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "NUMBER_OF_INCIPIENT_FIRE_TYPES") Then
                        ' This keyword is no longer used and here just to prevent flagging an error
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INCIPIENT_FIRE_TYPES") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim IncipientFireTypes(max)
                        For k = 1 To max
                            IncipientFireTypes(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TYPE_OF_INCIPIENT_GROWTH") Then
                        TypeofIncipientFireGrowth = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLAMING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID") Then
                        FlamingIncipientDelayRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLAMING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID") Then
                        FlamingIncipientPeakRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SMOLDERING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID") Then
                        SmolderingIncipientDelayRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SMOLDERING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID") Then
                        SmolderingIncipientPeakRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_INCIPIENT_TYPE_TO_PARAMETERS") Then
                        AddIncipientTypetoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_INCIPIENT_TIME_TO_PARAMETERS") Then
                        AddIncipientTimetoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_INCIPIENT_PEAK_TO_PARAMETERS") Then
                        AddIncipientPeaktoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INCIPIENT_TYPE_COLUMN_LABEL") Then
                        IncipientTypeColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INCIPIENT_TIME_COLUMN_LABEL") Then
                        IncipientTimeColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INCIPIENT_PEAK_COLUMN_LABEL") Then
                        IncipientPeakColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)

                    ElseIf (NMList.ForNMListGetVar(i, j) = "SCALING_FIRE_HRR_RANDOM_GENERATOR_ID") Then
                        ScalingFireHRRRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SCALING_FIRE_TIME_RANDOM_GENERATOR_ID") Then
                        ScalingFireTimeRandomGeneratorID = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_HRR_SCALE_TO_PARAMETERS") Then
                        AddHRRScaletoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_TIME_SCALE_TO_PARAMETERS") Then
                        AddTimeScaletoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "HRR_SCALE_COLUMN_LABEL") Then
                        HRRScaleColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TIME_SCALE_COLUMN_LABEL") Then
                        TimeScaleColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)

                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_HRR_GENERATOR_IDS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim FireHRRGeneratorIDs(max)
                        For k = 1 To max
                            FireHRRGeneratorIDs(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRE_TIME_GENERATOR_IDS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim FireTimeGeneratorIDs(max)
                        For k = 1 To max
                            FireTimeGeneratorIDs(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "NUMBER_OF_GROWTH_POINTS") Then
                        NumberofGrowthPoints = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "NUMBER_OF_DECAY_POINTS") Then
                        NumberofDecayPoints = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "GROWTH_EXPONENT") Then
                        GrowthExponent = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "DECAY_EXPONENT") Then
                        DecayExponent = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "GENERATOR_IS_TIME_TO_1054_KW") Then
                        GeneratorIsTimeto1054kW = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_FIRE_TO_PARAMETERS") Then
                        AddFiretoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_HRR_TO_PARAMETERS") Then
                        AddHRRtoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ADD_TIME_TO_PARAMETERS") Then
                        AddTimetoParameters = NMList.ForNMListVarGetBool(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "HRR_LABELS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim HRRLabels(max)
                        For k = 1 To max
                            HRRLabels(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TIME_LABELS") Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        ReDim TimeLabels(max)
                        For k = 1 To max
                            TimeLabels(k) = NMList.ForNMListVarGetStr(i, j, k)
                        Next
                    Else
                        myErrors.Add("In MFIR namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                Dim aFire As New MonteCarlo
                aFire.SetFire(id, FYI, FireID, BaseFireID, ModifyFireAreatoMatchHRR, FireCompartmentRandomGeneratorID, FireCompartmentIDs, AddFireCompartmentIDtoParameters, FireCompartmentIDColumnLabel, FlamingSmolderingIncipientRandomGeneratorID, IncipientFireTypes, TypeofIncipientFireGrowth, FlamingIncipientDelayRandomGeneratorID, FlamingIncipientPeakRandomGeneratorID, SmolderingIncipientDelayRandomGeneratorID, SmolderingIncipientPeakRandomGeneratorID, AddIncipientTypetoParameters, AddIncipientTimetoParameters, AddIncipientPeaktoParameters, IncipientTypeColumnLabel, IncipientTimeColumnLabel, IncipientPeakColumnLabel, ScalingFireHRRRandomGeneratorID, ScalingFireTimeRandomGeneratorID, AddHRRScaletoParameters, HRRScaleColumnLabel, AddTimeScaletoParameters, TimeScaleColumnLabel, FireHRRGeneratorIDs, FireTimeGeneratorIDs, NumberofGrowthPoints, NumberofDecayPoints, GrowthExponent, DecayExponent, GeneratorIsTimeto1054kW, AddFiretoParameters, AddHRRtoParameters, AddTimetoParameters, HRRLabels, TimeLabels)
                someFires.Add(aFire)
            End If
        Next
    End Sub
    Private Sub ReadInputFileNMLMSTT(NMList As NameListFile, ByRef someStats As MonteCarloCollection)
        Dim i As Integer, j As Integer

        Dim id As String, AnalysisType As String, InputFileName As String, OutputFileName As String, ErrorFileName As String, LogFIleName As String, ColumnLabel As String, FYI As String
        For i = 1 To NMList.TotNMList
            If NMList.GetNMListID(i) = "MSTT" Then
                id = ""
                AnalysisType = ""
                InputFileName = ""
                OutputFileName = ""
                ErrorFileName = ""
                LogFIleName = ""
                ColumnLabel = ""
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FYI") Then
                        FYI = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ANALYSIS_TYPE") Then
                        AnalysisType = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "INPUT_FILENAME") Then
                        InputFileName = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "OUTPUT_FILENAME") Then
                        OutputFileName = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ERROR_FILENAME") Then
                        ErrorFileName = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "LOG_FILENAME") Then
                        LogFIleName = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "COLUMN_LABEL") Then
                        ColumnLabel = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MSTT namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next

                Dim aStat As New MonteCarlo
                aStat.SetStat(id, FYI, AnalysisType, InputFileName, OutputFileName, ErrorFileName, LogFIleName, ColumnLabel)
                someStats.Add(aStat)
            End If
        Next
    End Sub
    Public Sub ReadINIInput(ByRef x As Integer, label As String, value As String)
        If value = "ON" Then
            x = Environment.DIAGon
        ElseIf value = "OFF" Then
            x = Environment.DIAGoff
        Else
            myErrors.Add("In DIAG namelist for " + label + " " + value + " Is Not a valid parameter", ErrorMessages.TypeFatal)
        End If
    End Sub
    Public Sub ReadThermalProperties(FileName As String, SomeThermalProperties As ThermalPropertiesCollection)
        'Simple read of only thermal properties from a file. 
        FindThermalProperties(FileName, SomeThermalProperties)
        SomeThermalProperties.FileName = FileName
        SomeThermalProperties.FileChanged = False
    End Sub
    Public Sub FindThermalProperties(Filename As String, ByRef SomeThermalProperties As ThermalPropertiesCollection)
        Dim IO As Integer = 1
        Dim str As String

        If File.Exists(Filename) Then
            FileOpen(IO, Filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
            str = LineInput(IO)
            FileClose(IO)
            If str.Substring(0, 1) = "&" Then
                FindThermalPropertiesNML(Filename, SomeThermalProperties)
            Else
                Dim csv As New CSVsheet(Filename)
                FindThermalPropertiesCSV(csv, SomeThermalProperties)
            End If
        End If
    End Sub
    Public Sub FindThermalPropertiesNML(Filename As String, ByRef SomeThermalProperties As ThermalPropertiesCollection)
        Dim NMList As New NameListFile(Filename)
        ReadInputFileNMLMatl(NMList, SomeThermalProperties)
    End Sub
    Public Sub FindThermalPropertiesCSV(csv As CSVsheet, ByRef SomeThermalProperties As ThermalPropertiesCollection)
        Dim i As Integer
        ' do material properties so they are defined for compartments, fires, and targets
        Dim hcl() As Double = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
        Dim iProp As Integer
        i = 1
        myUnits.SI = True
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                    Case "MATL"
                        If SomeThermalProperties.Count > 0 Then
                            iProp = SomeThermalProperties.GetIndex(csv.str(i, MaterialNum.shortName))
                            If iProp > -1 Then
                                ' We already have a thermal property with this name.  If it's totally identical, then it's already been added.  If not, they are trying to add a second one with the same name.  We'll allow it but error checking with flag it as an issue.
                                Dim aProperty As New ThermalProperty
                                aProperty = SomeThermalProperties.Item(iProp)
                                If aProperty.Name = csv.str(i, MaterialNum.longName) And aProperty.Conductivity = csv.num(i, MaterialNum.Conductivity) And aProperty.SpecificHeat = csv.num(i, MaterialNum.specificHeat) And aProperty.Density = csv.num(i, MaterialNum.density) _
                                    And aProperty.Thickness = csv.num(i, MaterialNum.thickness) And aProperty.Emissivity = csv.num(i, MaterialNum.emissivity) Then
                                    Exit Select
                                End If
                            End If
                        End If
                        SomeThermalProperties.Add(New ThermalProperty(csv.str(i, MaterialNum.shortName),
                            csv.str(i, MaterialNum.longName), csv.num(i, MaterialNum.Conductivity),
                            csv.num(i, MaterialNum.specificHeat), csv.num(i, MaterialNum.density), csv.num(i, MaterialNum.thickness),
                            csv.num(i, MaterialNum.emissivity)))
                        SomeThermalProperties.Item(SomeThermalProperties.Count - 1).SetHCl(hcl)
                        SomeThermalProperties.Item(SomeThermalProperties.Count - 1).Changed = False
                End Select
            End If
            i += 1
        Loop
        myUnits.SI = False
    End Sub
    Public Sub FindaThermalProperty(Filename As String, Material As String, ByRef aThermalPropery As ThermalProperty)
        ' look for a specific material in the current spreadsheet
        Dim SomeThermalProperties As New ThermalPropertiesCollection, aMaterial As New ThermalProperty, index As Integer
        FindThermalProperties(Filename, SomeThermalProperties)
        If SomeThermalProperties.Count > 0 Then
            index = SomeThermalProperties.GetIndex(Material)
            If index >= 0 Then
                aThermalPropery = SomeThermalProperties.Item(index)
                Return
            End If
        End If
        aThermalPropery.ShortName = " "
    End Sub
    Private Sub readFires(Filename As String, FileType As Integer)
        Dim csv As New CSVsheet(Filename), i As Integer
        If csv.MaxRow > 0 Then
            FindFires(FileType, Filename)
            If TempFires.Count > 0 Then
                For i = 1 To TempFires.Count
                    Dim aFire As New Fire
                    aFire = TempFires.Item(i - 1)
                    myFireProperties.Add(aFire)
                Next
            End If
        End If
    End Sub
    Public Sub FindFires(FileType As Integer, Filename As String)
        If FileType = InsertDataType.ObjectFile Then
            FindFiresCSV(FileType, Filename)
        Else
            Dim IO As Integer = 1
            Dim str As String
            If File.Exists(Filename) Then
                FileOpen(IO, Filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
                str = LineInput(IO)
                FileClose(IO)
                If str.Substring(0, 1) = "&" Then
                    FindFiresNML(FileType, Filename)
                ElseIf str.Substring(0, 5) = "VERSN" Then
                    FindFiresCSV(FileType, Filename)
                End If
            End If
        End If
    End Sub
    Public Sub FindFiresNML(FileType As Integer, Filename As String)
        Dim NMList As New NameListFile(Filename)

        ReadInputFileNMLChem(NMList, TempFires)
    End Sub
    Public Sub FindFiresCSV(FileType As Integer, Filename As String)
        'simple read of a fire object file
        Dim csv As New CSVsheet(Filename)
        Dim fireComments As New Collection
        Dim i, j, k, iStart, index As Integer
        Dim rowidx(csv.MaxRow) As Integer
        Dim rdx As Integer = 0
        Dim ChemicalCompound() As Double = {0.0, 1.0, 4.0, 0.0, 0.0, 0.0}
        Dim NewFileformat, NewFireFormat As Boolean

        myUnits.SI = True
        i = 1
        NewFileformat = False
        NewFireFormat = False
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                    Case "MATL"
                        NewFileformat = True
                    Case "FIRE"
                        NewFileformat = True
                        NewFireFormat = True
                    Case "CHEMI"
                        If csv.num(i, 0) > 3 Then NewFileformat = True
                End Select
            End If
            i += 1
        Loop
        If NewFileformat Then
            i = 1
            Do Until i > csv.MaxRow
                If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                    Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                        Case "FIRE"
                            If csv.str(i, 2) = "OBJECT" Or csv.str(i + 1, 1) = "CHEMI" Then
                                ' New format fire
                                For j = 0 To TempFires.Count - 1
                                    If csv.str(iStart, fireNum.name) = TempFires.Item(j).Name Then
                                        Exit Sub
                                    End If
                                Next

                                Dim hcl() As Double = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
                                Dim iFire As Integer = 0, iChemie As Integer = 0, iTime As Integer = 0
                                Dim fireComplete As Integer = 0
                                Do Until fireComplete >= NumFireCurves + 1
                                    If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                                        Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                                            Case "FIRE"
                                                iFire = i
                                                fireComplete = 1
                                            Case "CHEMI"
                                                iChemie = i
                                                fireComplete += 1
                                            Case "TIME"
                                                iTime = i
                                                fireComplete += 1
                                            Case "HRR", "SOOT", "CO", "TRACE", "AREA", "HEIGH"
                                                fireComplete += 1
                                        End Select
                                    End If
                                    i += 1
                                Loop
                                Dim aFireObject As New Fire()
                                Dim aThermalProperty As New ThermalProperty()
                                aFireObject.Name = csv.str(iFire, fireNum.name)
                                aFireObject.ChemicalFormula(formula.C) = csv.num(iChemie, chemieNum.C)
                                aFireObject.ChemicalFormula(formula.H) = csv.num(iChemie, chemieNum.H)
                                aFireObject.ChemicalFormula(formula.O) = csv.num(iChemie, chemieNum.O)
                                aFireObject.ChemicalFormula(formula.N) = csv.num(iChemie, chemieNum.N)
                                aFireObject.ChemicalFormula(formula.Cl) = csv.num(iChemie, chemieNum.Cl)
                                aFireObject.HeatofCombustion = csv.num(iChemie, chemieNum.HoC)
                                index = myThermalProperties.GetIndex(csv.str(iChemie, chemieNum.Material))

                                aFireObject.RadiativeFraction = csv.num(iChemie, chemieNum.chiR)
                                aFireObject.Changed = False
                                TempFires.Add(aFireObject)

                                Dim firedata(12, CInt(csv.num(iTime, 0) - 2)) As Double

                                For j = 0 To csv.num(iTime, 0) - 2
                                    For k = 1 To NumFireCurves
                                        firedata(FireCurveColumns(k), j) = csv.num(iTime + k - 1, j + 2)
                                    Next
                                    firedata(Fire.FireMdot, j) = firedata(Fire.FireHRR, j) / aFireObject.HeatofCombustion
                                    firedata(Fire.FireHC, j) = aFireObject.ChemicalFormula(formula.H) * 1.00794 / (aFireObject.ChemicalFormula(formula.C) * 12.0107)
                                    If aFireObject.ChemicalFormula(formula.N) <> 0 Then firedata(Fire.FireHCN, j) = (1.00794 + 12.0107 + 14.01) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.N)
                                    If aFireObject.ChemicalFormula(formula.Cl) <> 0 Then firedata(Fire.FireHCl, j) = (1.00794 + 35.453) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.Cl)
                                Next
                                TempFires(TempFires.Count - 1).SetFireData(firedata)
                                TempFires(TempFires.Count - 1).Changed = False

                                fireComplete = 0
                            End If
                    End Select
                End If
                i += 1
            Loop
        ElseIf FileType = InsertDataType.ObjectFile Then
            Try
                ' Old format fire object file without much error checking
                For i = 1 To csv.MaxRow
                    If Not SkipLine(csv.CSVrow(i)) Then
                        rowidx(rdx) = i
                        rdx += 1
                    ElseIf Comment(csv.str(i, CFASTlnNum.keyWord)) Then
                        fireComments.Add(csv.strrow(i))
                    End If
                Next
                For i = 0 To TempFires.Count - 1
                    If csv.str(rowidx(0), 1) = TempFires.Item(i).Name Then
                        myUnits.SI = False
                        Exit Sub
                    End If
                Next
                ' Chemical compound is assumed to be methane for these old format files.
                TempFires.Add(New Fire(csv.str(rowidx(0), 1), ChemicalCompound, csv.num(rowidx(11), 1), csv.num(rowidx(6), 1)))
                ' Check for thermal property of the fire object and find it if necessary

                Dim firedata(12, CInt(csv.num(rowidx(1), 1) - 1)) As Double
                For i = 0 To csv.num(rowidx(1), 1) - 1
                    For j = 0 To 12
                        firedata(j, i) = csv.num(rowidx(1 + i), firefile(j))
                    Next
                Next
                TempFires(TempFires.Count - 1).SetFireData(firedata)
                fireFilesComments.Add(fireComments)
                TempFires(TempFires.Count - 1).CommentsIndex = fireFilesComments.Count

            Catch ex As Exception
            End Try
        End If

        If TempFires.Count > 0 Then TempFires(TempFires.Count - 1).Changed = False
        myUnits.SI = False
    End Sub
#End Region
#Region "Write Routines"
    Public Sub WriteInputFileNML(filename As String)
        Dim IO As Integer = 1
        Dim ln As String

        FileOpen(IO, filename, OpenMode.Output, OpenAccess.Write, OpenShare.Shared)

        WriteOutputFileNMLHead(IO, myEnvironment)
        WriteOutputFileNMLMHDR(IO, myMHeaders)
        WriteOutputFileNMLTime(IO, myEnvironment)
        WriteOutputFileNMLInit(IO, myEnvironment)
        WriteOutputFileNMLMisc(IO, myEnvironment)
        WriteOutputFileNMLDiag(IO, myEnvironment)
        WriteOutputFileNMLMatl(IO, myThermalProperties)
        WriteOutputFileNMLComp(IO, myCompartments)
        WriteOutputFileNMLVent(IO, myHVents, myVVents, myMVents)
        WriteOutputFileNMLFire(IO, myFireProperties)
        WriteOutputFileNMLDevc(IO, myTargets, myDetectors)
        WriteOutputFileNMLConn(IO, myCompartments, myHHeats, myVHeats)
        WriteOutputFileNMLIsoSlcf(IO, myVisuals)
        WriteOutputFileNMLMRND(IO, myMRandoms)
        WriteOutputFileNMLMFLD(IO, myMFields)
        WriteOutputFileNMLMFIR(IO, myMFires)
        WriteOutputFileNMLOutp(IO, myOutputs)
        WriteOutputFileNMLMSTT(IO, myMStats)

        PrintLine(IO, " ")
        ln = "&TAIL /"
        PrintLine(IO, ln)

        FileClose(IO)
    End Sub

    Private Sub WriteOutputFileNMLHead(IO As Integer, ByRef MyEnvironment As Environment)
        Dim ln As String

        'Writing HEAD namelist
        ln = "&HEAD VERSION = " + MyEnvironment.Version.ToString + ", " + "TITLE = " + "'" + MyEnvironment.Title + "' /"
        PrintLine(IO, ln)
    End Sub
    Private Sub WriteOutputFileNMLMHDR(IO As Integer, ByRef myMHeaders As MonteCarloCollection)
        Dim ln As String

        'Writing MHDR namelist
        If myMHeaders.Count > 0 Then
            Dim aMHeader As MonteCarlo = myMHeaders(0)
            Dim NumberofCases As Integer, Seeds(2) As Double, WriteSeeds As Boolean, ParameterFile, WorkFolder, OutputFolder As String
            NumberofCases = 0
            Seeds(1) = -1001.0
            Seeds(2) = -1001.0
            WriteSeeds = False
            ParameterFile = ""
            WorkFolder = ""
            OutputFolder = ""
            Call aMHeader.GetHeader(NumberofCases, Seeds, WriteSeeds, ParameterFile, WorkFolder, OutputFolder)
            If NumberofCases > 0 Then
                ln = "&MHDR NUMBER_OF_CASES = " + NumberofCases.ToString
                If Seeds(1) <> -1001 And Seeds(2) <> -1001 Then ln += "SEEDS = " + Seeds(1).ToString + ", " + Seeds(2).ToString
                If WriteSeeds = True Then ln += " WRITE_SEEDS = .TRUE."
                If ParameterFile <> "" Then ln += " PARAMETER_FILE = " + "'" + ParameterFile + "'"
                If WorkFolder <> "" Then ln += " WORK_FOLDER = " + "'" + WorkFolder + "'"
                If OutputFolder <> "" Then ln += " OUTPUT_FILE = " + "'" + OutputFolder + "'"
                ln += " /"
                PrintLine(IO, ln)
            End If
        End If
    End Sub
    Private Sub WriteOutputFileNMLTime(IO As Integer, ByRef MyEnvironment As Environment)
        Dim ln As String

        PrintLine(IO, " ")
        ln = "!! Scenario Configuration "
        PrintLine(IO, ln)
        'Writing TIME namelist
        ln = "&TIME SIMULATION = " + MyEnvironment.SimulationTime.ToString + " PRINT = " + MyEnvironment.OutputInterval.ToString + " SMOKEVIEW = " + MyEnvironment.SmokeviewInterval.ToString + " SPREADSHEET = " + MyEnvironment.SpreadsheetInterval.ToString + " / "
        PrintLine(IO, ln)
    End Sub
    Private Sub WriteOutputFileNMLInit(IO As Integer, ByRef MyEnvironment As Environment)
        Dim ln As String

        'Writing INIT namelist
        ln = "&INIT " + "PRESSURE = " + MyEnvironment.ExtAmbPressure.ToString + " RELATIVE_HUMIDITY = " + MyEnvironment.IntAmbRH.ToString
        ln += " INTERIOR_TEMPERATURE = " + Math.Round((MyEnvironment.IntAmbTemperature - 273.15), 2).ToString
        ln += " EXTERIOR_TEMPERATURE = " + Math.Round((MyEnvironment.ExtAmbTemperature - 273.15), 2).ToString + " /"
        PrintLine(IO, ln)
    End Sub
    Private Sub WriteOutputFileNMLMisc(IO As Integer, ByRef MyEnvironment As Environment)
        Dim ln As String, aFlag As Boolean

        'Writing MISC namelist
        ln = "&MISC "
        If MyEnvironment.AdiabaticWalls Or (MyEnvironment.MaximumTimeStep <> Environment.DefaultMaximumTimeStep And MyEnvironment.MaximumTimeStep > 0.0) Or MyEnvironment.LowerOxygenLimit <> 0.15 Or MyEnvironment.FlamingExtinctionCoefficient <> 8700 Or MyEnvironment.SmolderingExtinctionCoefficient <> 4400 Or MyEnvironment.Overwrite <> True Then
            aFlag = True
        Else
            aFlag = False
        End If
        If MyEnvironment.AdiabaticWalls <> False Then
            ln += " ADIABATIC = .TRUE. "
        End If
        If MyEnvironment.MaximumTimeStep <> Environment.DefaultMaximumTimeStep And MyEnvironment.MaximumTimeStep > 0 Then
            ln += " MAX_TIME_STEP = " + MyEnvironment.MaximumTimeStep.ToString
        End If
        If MyEnvironment.LowerOxygenLimit <> 0.15 Then
            ln += " LOWER_OXYGEN_LIMIT = " + MyEnvironment.LowerOxygenLimit.ToString
        End If
        If MyEnvironment.FlamingExtinctionCoefficient <> 8700 Or MyEnvironment.SmolderingExtinctionCoefficient <> 4400 Then
            ln += " SPECIFIC_EXTINCTION = " + MyEnvironment.FlamingExtinctionCoefficient.ToString + ", " + MyEnvironment.SmolderingExtinctionCoefficient.ToString
        End If
        If MyEnvironment.Overwrite <> True Then
            ln += " OVERWRITE = .FALSE."
        End If
        If aFlag Then
            ln += " / "
            PrintLine(IO, ln)
        End If
        MyEnvironment.Changed = False

    End Sub
    Private Sub WriteOutputFileNMLDiag(IO As Integer, ByRef Myenvironment As Environment)
        Dim ln As String, wrtDIAG As Boolean, wrtSlash As Boolean
        Dim x(0), f(0) As Double, i As Integer

        'Writing Diagnostics 
        If Myenvironment.DIAGRadSolver = "DEFAULT" Then
            wrtDIAG = True
            wrtSlash = False
        Else
            wrtDIAG = False
            wrtSlash = True
            ln = "&DIAG  RADSOLVER = '" + Myenvironment.DIAGRadSolver + "' "
            PrintLine(IO, ln)
        End If
        If Myenvironment.DIAGAdiabaticTargetVerification = True Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "ADIABATIC_TARGET_VERIFICATION = 'ON' RADIATIVE_INCIDENT_FLUX = " + Myenvironment.DIAGAdiabaticTargetFlux.ToString
            Print(IO, ln)
        End If
        If Myenvironment.DIAGUpperLayerThickness <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "UPPER_LAYER_THICKNESS = " + Myenvironment.DIAGUpperLayerThickness.ToString
            Print(IO, ln)
        End If
        If Myenvironment.DIAGFireHeatFlux <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "VERIFICATION_FIRE_HEAT_FLUX = " + Myenvironment.DIAGFireHeatFlux.ToString
            Print(IO, ln)
        End If
        If Myenvironment.DIAGVerificationTimeStep <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "VERIFICATION_TIME_STEP = " + Myenvironment.DIAGVerificationTimeStep.ToString
            Print(IO, ln)
        End If
        If Myenvironment.DIAGGasTemp <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "GAS_TEMPERATURE = " + Math.Round(Myenvironment.DIAGGasTemp, 2).ToString
            ln += " PARTIAL_PRESSURE_H2O = " + Math.Round(Myenvironment.DIAGPartPressH2O, 2).ToString
            ln += " PARTIAL_PRESSURE_CO2 = " + Math.Round(Myenvironment.DIAGPartPressCO2, 2).ToString
            Print(IO, ln)
        End If
        Myenvironment.GetDIAGf(f)
        Myenvironment.GetDIAGt(x)
        If f.GetUpperBound(0) = x.GetUpperBound(0) Then
            Dim numpoints As Integer = f.GetUpperBound(0)
            For i = 0 To numpoints
                If f(i) = Environment.DefaultNonValue Or x(i) = Environment.DefaultNonValue Then
                    numpoints = -1
                End If
            Next
            If numpoints >= 0 Then
                If wrtDIAG Then
                    ln = "&DIAG "
                    wrtDIAG = False
                    wrtSlash = True
                Else
                    ln = " "
                End If
                ln += " T = " + x(0).ToString
                For i = 1 To numpoints
                    ln += ", " + x(i).ToString
                Next
                PrintLine(IO, ln)
                ln = "      F = " + f(0).ToString
                For i = 1 To numpoints
                    ln += ", " + f(i).ToString
                Next
                PrintLine(IO, ln)
            End If
        End If
        If Myenvironment.DIAGfire <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "FIRE_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGhflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "HORIZONTAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGentrain <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "ENTRAINMENT_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGvflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "VERTICAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGcjet <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CEILING_JET_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGdfire <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DOOR_JET_FIRE_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGconvec <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CONVECTION_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGrad <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "RADIATION_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGgasabsorp <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "GAS_ABSORBTION_SUB_MODEL = 'CONSTANT' ")
        End If
        If Myenvironment.DIAGconduc <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CONDUCTION_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGdebugprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DEBUG_PRINT = 'ON' ")
        End If
        If Myenvironment.DIAGmflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "MECHANICAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If Myenvironment.DIAGkeyin <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "KEYBOARD_INPUT = 'OFF' ")
        End If
        If Myenvironment.DIAGsteadyint <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "STEADY_STATE_INITIAL_CONDITIONS = 'ON' ")
        End If
        If Myenvironment.DIAGdasslprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DASSL_DEBUG_PRINT = 'ON' ")
        End If
        If Myenvironment.DIAGoxygen <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "OXYGEN_TRACKING = 'ON' ")
        End If
        If Myenvironment.DIAGresiddbprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "RESIDUAL_DEBUG_PRINT = 'ON' ")
        End If
        If Myenvironment.DIAGlayermixing <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "LAYER_MIXING_SUB_MODEL = 'OFF' ")
        End If
        If wrtSlash Then
            ln = "/ "
            PrintLine(IO, ln)
        End If

    End Sub
    Private Sub WriteOutputFileNMLMatl(IO As Integer, MyThermalProperties As ThermalPropertiesCollection)
        Dim ln, field As String, i, j As Integer, aThermalProperty As New ThermalProperty

        'Writing MATL namelist
        If MyThermalProperties.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Material Properties "
            PrintLine(IO, ln)

            For i = 0 To MyThermalProperties.Count - 1
                aThermalProperty = MyThermalProperties.Item(i)
                If MyThermalProperties.NumberofConnections(aThermalProperty.ShortName) > 0 Then
                    ln = "&MATL" + " ID = '" + aThermalProperty.ShortName + "'"
                    If aThermalProperty.Name <> "" Then
                        If aThermalProperty.Name.IndexOf("'") > 0 Then
                            field = aThermalProperty.Name
                            j = 0
                            Do While (j <= field.Length - 1)
                                If field.Substring(j, 1) = "'" Then
                                    field = field.Insert(j, "'")
                                    j += 1
                                End If
                                j += 1
                            Loop
                            ln += " MATERIAL = '" + field + "' "
                        Else
                            ln += " MATERIAL = '" + aThermalProperty.Name + "', "
                        End If
                    End If
                    PrintLine(IO, ln)
                    ln = "      CONDUCTIVITY = " + aThermalProperty.Conductivity.ToString + " DENSITY = " + aThermalProperty.Density.ToString + " SPECIFIC_HEAT = " + (aThermalProperty.SpecificHeat / 1000.0).ToString + ", THICKNESS = " + aThermalProperty.Thickness.ToString + " EMISSIVITY = " + aThermalProperty.Emissivity.ToString
                    If aThermalProperty.FYI <> "" Then
                        ln += " /"
                        PrintLine(IO, ln)
                        ln = " FYI = '" + aThermalProperty.FYI + "'"
                    End If
                    ln += " /"
                    PrintLine(IO, ln)
                End If
                aThermalProperty.Changed = False
            Next
        End If

    End Sub
    Private Sub WriteOutputFileNMLComp(IO As Integer, MyCompartments As CompartmentCollection)
        Dim ln As String, aComp As Compartment, i, j, k As Integer
        Dim x(0), f(0) As Double
        Dim hasmaterials As Boolean

        ' Writing COMP namelist
        If MyCompartments.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Compartments "
            PrintLine(IO, ln)

            For i = 0 To MyCompartments.Count - 1
                aComp = MyCompartments.Item(i)
                ln = "&COMP " + "ID = '" + aComp.Name + "'"
                PrintLine(IO, ln)
                ln = "      DEPTH = " + aComp.RoomDepth.ToString + " HEIGHT = " + aComp.RoomHeight.ToString + " WIDTH = " + aComp.RoomWidth.ToString
                If aComp.Hall Then
                    ln += " HALL = .TRUE."
                ElseIf aComp.Shaft Then
                    ln += " SHAFT = .TRUE."
                End If
                PrintLine(IO, ln)

                hasmaterials = False
                ln = "     "
                If aComp.CeilingMaterial(1) <> "" Then
                    If aComp.CeilingMaterial(1) = "Off" Then

                        ln += " CEILING_MATL_ID = 'OFF'"
                        hasmaterials = True
                    Else
                        ln = "      CEILING_MATL_ID = '" + aComp.CeilingMaterial(1) + "'"
                        If aComp.CeilingMaterial(2) <> "" Then ln += ", '" + aComp.CeilingMaterial(2) + "'"
                        If aComp.CeilingMaterial(3) <> "" Then ln += ", '" + aComp.CeilingMaterial(3) + "'"
                        hasmaterials = True
                        If aComp.CeilingThickness(1) <> 0 Then
                            ln += " CEILING_THICKNESS = " + aComp.CeilingThickness(1).ToString
                            If aComp.CeilingMaterial(2) <> "" Then ln += ", " + aComp.CeilingThickness(2).ToString
                            If aComp.CeilingMaterial(3) <> "" Then ln += ", " + aComp.CeilingThickness(3).ToString
                        End If
                    End If
                End If

                If aComp.WallMaterial(1) <> "" Then
                    If aComp.WallMaterial(1) = "Off" Then
                        ln += " WALL_MATL_ID = 'OFF'"
                        hasmaterials = True
                    Else
                        ln += " WALL_MATL_ID = '" + aComp.WallMaterial(1) + "'"
                        If aComp.WallMaterial(2) <> "" Then ln += ", '" + aComp.WallMaterial(2) + "'"
                        If aComp.WallMaterial(3) <> "" Then ln += ", '" + aComp.WallMaterial(3) + "'"
                        hasmaterials = True
                        If aComp.WallThickness(1) <> 0 Then
                            ln += " WALL_THICKNESS = " + aComp.WallThickness(1).ToString
                            If aComp.WallMaterial(2) <> "" Then ln += ", " + aComp.WallThickness(2).ToString
                            If aComp.WallMaterial(3) <> "" Then ln += ", " + aComp.WallThickness(3).ToString
                        End If
                    End If
                End If
                If aComp.FloorMaterial(1) <> "" Then
                    If aComp.FloorMaterial(1) = "Off" Then
                        ln += " FLOOR_MATL_ID = 'OFF'"
                        hasmaterials = True
                    Else
                        ln += " FLOOR_MATL_ID = '" + aComp.FloorMaterial(1) + "'"
                        If aComp.FloorMaterial(2) <> "" Then ln += ", '" + aComp.FloorMaterial(2) + "'"
                        If aComp.FloorMaterial(3) <> "" Then ln += ", '" + aComp.FloorMaterial(3) + "'"
                        hasmaterials = True
                        If aComp.FloorThickness(1) <> 0 Then
                            ln += " FLOOR_THICKNESS = " + aComp.FloorThickness(1).ToString
                            If aComp.FloorMaterial(2) <> "" Then ln += ", " + aComp.FloorThickness(2).ToString
                            If aComp.FloorMaterial(3) <> "" Then ln += ", " + aComp.FloorThickness(3).ToString
                        End If
                    End If
                End If
                If hasmaterials Then PrintLine(IO, ln)
                aComp.GetVariableArea(f, x, j)
                If j > 1 Then
                    ln = "      CROSS_SECT_AREAS = " + f(1).ToString
                    For k = 2 To j
                        ln = ln + ", " + f(k).ToString
                    Next
                    PrintLine(IO, ln)
                    ln = "      CROSS_SECT_HEIGHTS = " + x(1).ToString
                    For k = 2 To j
                        ln = ln + ", " + x(k).ToString
                    Next
                    PrintLine(IO, ln)
                End If
                ln = "      ORIGIN = " + aComp.RoomOriginX.ToString + ", " + aComp.RoomOriginY.ToString + ", " + aComp.RoomOriginZ.ToString
                ln += " GRID = " + aComp.xGrid.ToString + ", " + aComp.yGrid.ToString + ", " + aComp.zGrid.ToString
                If aComp.WallLeak > 0 Or aComp.FloorLeak > 0 Then
                    ln += " LEAK_AREA_RATIO = " + aComp.WallLeak.ToString + ", " + aComp.FloorLeak.ToString
                End If
                If aComp.FYI <> "" Then
                    ln += " FYI = '" + aComp.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aComp.Changed = False
            Next
        End If

    End Sub
    Private Sub WriteOutputFileNMLVent(IO As Integer, ByRef myHvents As VentCollection, ByRef MyVVents As VentCollection, MyMVents As VentCollection)
        Dim ln As String, i, j As Integer, aVent As Vent

        ' Writing VENT namelist for wall vents
        If myHvents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Wall Vents"
            PrintLine(IO, ln)

            For i = 0 To myHvents.Count - 1
                aVent = myHvents.Item(i)
                If aVent.Name = "" Then
                    aVent.Name = "WallVent_" + (i + 1).ToString
                End If
                ln = "&VENT TYPE = 'WALL' ID = '" + aVent.Name + "'"
                If aVent.FirstCompartment < 0 Then
                    ln += " COMP_IDS = 'OUTSIDE', '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                ElseIf aVent.SecondCompartment < 0 Then
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' 'OUTSIDE' "
                Else
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "', '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                End If
                ln += ", BOTTOM = " + aVent.Bottom.ToString + " HEIGHT = " + aVent.Height.ToString + ", WIDTH = " + aVent.Width.ToString
                If aVent.Coeff > 0 Then ln += "  VENT_FLOW_COEFFICIENT = " + aVent.Coeff.ToString
                PrintLine(IO, ln)
                ln = "    "
                If aVent.OpenType = Vent.OpenbyTime Then
                    Dim ff(2), xx(2), numpoints As Double
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints > 1 Then
                        ln += "  CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + xx(j).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + ff(j).ToString
                        Next
                    ElseIf aVent.InitialOpening <> 1 Or aVent.FinalOpening <> 1 Then
                        ln += "  CRITERION = 'TIME'"
                        ln += "  T = " + aVent.InitialOpeningTime.ToString + ", " + aVent.FinalOpeningTime.ToString + " "
                        ln += "  F = " + aVent.InitialOpening.ToString + ", " + aVent.FinalOpening.ToString + " "
                    End If
                ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                    ln += "  CRITERION = 'TEMPERATURE', SETPOINT = " + Math.Round((aVent.OpenValue - 273.15), 2).ToString + " DEVC_ID = '" + aVent.Target + "'"
                    ln += "  PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                    ln += "  CRITERION = 'FLUX', SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + ", DEVC_ID = '" + aVent.Target + "' "
                    ln += "  PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                End If
                If aVent.Face = 2 Then
                    ln += "  FACE = 'RIGHT'"
                ElseIf aVent.Face = 3 Then
                    ln += "  FACE = 'REAR'"
                ElseIf aVent.Face = 4 Then
                    ln += "  FACE = 'LEFT'"
                Else
                    ln += "  FACE = 'FRONT'"
                End If
                ln += "  OFFSET = " + aVent.Offset.ToString
                If aVent.FYI <> "" Then
                    ln += "  FYI = '" + aVent.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing VENT namelist for ceiling/floor vents
        If MyVVents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Ceiling and Floor Vents "
            PrintLine(IO, ln)

            For i = 0 To MyVVents.Count - 1
                aVent = MyVVents.Item(i)
                If aVent.Name = "" Then
                    aVent.Name = "CeilFloorVent_" + (i + 1).ToString
                End If
                If aVent.FirstCompartment < 0 Then
                    ln = "&VENT TYPE = 'CEILING'"
                Else
                    ln = "&VENT TYPE = 'FLOOR'"
                End If
                ln += " ID = '" + aVent.Name + "'"
                If aVent.FirstCompartment < 0 Then
                    ln += " COMP_IDS = 'OUTSIDE', '" _
                        + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                ElseIf aVent.SecondCompartment < 0 Then
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "', 'OUTSIDE'"
                Else
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "', '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                End If
                If aVent.Shape = 1 Then
                    ln += " AREA = " + aVent.Area.ToString + ", SHAPE = 'ROUND' "
                Else
                    ln += " AREA = " + aVent.Area.ToString + ", SHAPE = 'SQUARE' "
                End If
                If aVent.OpenType = Vent.OpenbyTime Then
                    Dim ff(2), xx(2), numpoints As Double
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints >= 1 Then
                        PrintLine(IO, ln)
                        ln = "      CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + xx(j).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + ff(j).ToString
                        Next
                    End If
                ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                    PrintLine(IO, ln)
                    ln = "      CRITERION = 'TEMPERATURE', SETPOINT = " + Math.Round((aVent.OpenValue - 273.15), 2).ToString + ", DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                    PrintLine(IO, ln)
                    ln = "     CRITERION = 'FLUX', SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + ", DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                End If
                ln += " OFFSETS = " + aVent.OffsetX.ToString + ", " + aVent.OffsetY.ToString
                If aVent.FYI <> "" Then
                    ln += " FYI = '" + aVent.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing VENT namelist for mechanical vents
        If MyMVents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Mechanical Vents"
            PrintLine(IO, ln)

            For i = 0 To MyMVents.Count - 1
                aVent = MyMVents.Item(i)
                If aVent.Name = "" Then
                    aVent.Name = "MechanicalVent_" + (i + 1).ToString
                End If
                ln = "&VENT TYPE = 'MECHANICAL' ID = '" + aVent.Name + "'"
                If aVent.FirstCompartment < 0 Then
                    ln += " COMP_IDS = 'OUTSIDE', '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                ElseIf aVent.SecondCompartment < 0 Then
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "', 'OUTSIDE'"
                Else
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "', '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                End If
                PrintLine(IO, ln)

                ln = "      AREAS = " + aVent.FirstArea.ToString + ", " + aVent.SecondArea.ToString + " HEIGHTS = " + aVent.FirstCenterHeight.ToString + ", " + aVent.SecondCenterHeight.ToString
                If aVent.FirstOrientation = 1 Then
                    If aVent.SecondOrientation = 2 Then
                        ln += " ORIENTATIONS = 'VERTICAL', 'HORIZONTAL' "
                    End If
                Else
                    If aVent.SecondOrientation = 2 Then
                        ln += " ORIENTATIONS = 'HORIZONTAL', 'HORIZONTAL' "
                    Else
                        ln += " ORIENTATIONS = 'HORIZONTAL', 'VERTICAL' "
                    End If
                End If
                ln += " FLOW = " + aVent.FlowRate.ToString
                If aVent.BeginFlowDropoff <> Vent.default_min_cutoff_relp Or aVent.ZeroFlow <> Vent.default_max_cutoff_relp Then
                    ln += " CUTOFFS = " + aVent.BeginFlowDropoff.ToString + ", " + aVent.ZeroFlow.ToString
                End If
                ln += " OFFSETS = " + aVent.OffsetX.ToString + ", " + aVent.OffsetY.ToString
                If aVent.OpenType = Vent.OpenbyTime Then
                    Dim ff(2), xx(2), numpoints As Double
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints > 1 Then
                        PrintLine(IO, ln)
                        ln = "      CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + xx(j).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For j = 2 To numpoints
                            ln += ", " + ff(j).ToString
                        Next
                    ElseIf aVent.InitialOpening <> 1 Or aVent.FinalOpening <> 1 Then
                        PrintLine(IO, ln)
                        ln = "      CRITERION = 'TIME'"
                        ln += " T = " + aVent.InitialOpeningTime.ToString + ", " + aVent.FinalOpeningTime.ToString + " "
                        ln += " F = " + aVent.InitialOpening.ToString + ", " + aVent.FinalOpening.ToString + " "
                    End If
                ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                    PrintLine(IO, ln)
                    ln = "      CRITERION = 'TEMPERATURE' SETPOINT = " + Math.Round((aVent.OpenValue - 273.15), 2).ToString + ", DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                    PrintLine(IO, ln)
                    ln = "      CRITERION = 'FLUX' SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + " DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + " POST_FRACTION = " + aVent.FinalOpening.ToString
                End If
                ln += " FILTER_TIME = " + aVent.FilterTime.ToString + " FILTER_EFFICIENCY = " + aVent.FilterEfficiency.ToString
                If aVent.FYI <> "" Then
                    ln += " FYI = '" + aVent.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

    End Sub
    Private Sub WriteOutputFileNMLFire(IO As Integer, ByRef MyFireProperties As FireCollection)
        Dim ln As String, aFire As Fire, i, j, k, l As Integer
        Dim aFireCurves(12, 0) As Double

        'Writing Fires
        If myFires.Count + MyFireProperties.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Fires "
            PrintLine(IO, ln)

            For i = 0 To myFires.Count - 1
                aFire = myFires.Item(i)
                ln = "&FIRE ID = '" + aFire.Name + "' "
                If aFire.Compartment >= 0 And aFire.Compartment <= myCompartments.Count - 1 Then
                    ln += " COMP_ID = '" + myCompartments.Item(aFire.Compartment).Name + "', FIRE_ID = '" + aFire.ReferencedFireDefinition + "' "
                Else
                    ln += " COMP_ID = '', FIRE_ID = '" + aFire.ReferencedFireDefinition + "' "
                End If
                ln += " LOCATION = " + aFire.XPosition.ToString + ", " + aFire.YPosition.ToString
                If aFire.IgnitionType = Fire.FireIgnitionbyTime Then
                    If aFire.IgnitionValue > 0 Then
                        ln += " IGNITION_CRITERION = 'TIME', SETPOINT = " + aFire.IgnitionValue.ToString
                    End If
                ElseIf aFire.IgnitionType = Fire.FireIgnitionbyTemperature Then
                    ln += " IGNITION_CRITERION = 'TEMPERATURE', DEVC_ID = '" + aFire.Target + "', SETPOINT = " + Math.Round((aFire.IgnitionValue - 273.15), 2).ToString
                ElseIf aFire.IgnitionType = Fire.FireIgnitionbyFlux Then
                    ln += " IGNITION_CRITERION = 'FLUX', DEVC_ID = '" + aFire.Target + "', SETPOINT = " + (aFire.IgnitionValue / 1000.0).ToString
                End If
                If aFire.FYI <> "" Then
                    ln += " FYI = '" + aFire.FYI + "'"
                End If
                ln += " / "
                PrintLine(IO, ln)
                aFire.Changed = False
            Next

            For i = 0 To MyFireProperties.Count - 1
                If myFires.NumberofInstances(MyFireProperties.Item(i).Name) > 0 Then
                    aFire = MyFireProperties.Item(i)
                    ln = "&CHEM ID = '" + aFire.Name + "'"
                    ln += " CARBON = " + aFire.ChemicalFormula(formula.C).ToString + " CHLORINE = " + aFire.ChemicalFormula(formula.Cl).ToString + " HYDROGEN = " + aFire.ChemicalFormula(formula.H).ToString + " NITROGEN = " + aFire.ChemicalFormula(formula.N).ToString + " OXYGEN = " + aFire.ChemicalFormula(formula.O).ToString
                    ln += " HEAT_OF_COMBUSTION = " + (aFire.HeatofCombustion / 1000).ToString
                    ln += " RADIATIVE_FRACTION = " + aFire.RadiativeFraction.ToString
                    If aFire.FlamingTransitionTime <> 0 Then ln += " FLAMING_TRANSITION_TIME = " + aFire.FlamingTransitionTime.ToString
                    ln += " / "
                    PrintLine(IO, ln)

                    aFire.GetFireData(aFireCurves, k)
                    ln = "&TABL ID = '" + aFire.Name + "' LABELS = '" + aFire.ColNames(aFire.ColMap(0)) + "'"
                    For j = 1 To aFire.ColMapUpperBound
                        ln += ", '" + aFire.ColNames(aFire.ColMap(j)) + "' "
                    Next
                    ln += " /"
                    PrintLine(IO, ln)
                    For j = 0 To k
                        ln = "&TABL ID = '" + aFire.Name + "', DATA = " + aFireCurves(aFire.ColMap(0), j).ToString
                        For l = 1 To aFire.ColMapUpperBound
                            If aFire.ColMap(l) = 2 Then
                                ln += ", " + (aFireCurves(aFire.ColMap(l), j) / 1000).ToString
                            Else
                                ln += ", " + aFireCurves(aFire.ColMap(l), j).ToString
                            End If
                        Next
                        ln += " /"
                        PrintLine(IO, ln)
                    Next
                    aFire.Changed = False
                End If
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLDevc(IO As Integer, ByRef MyTargets As TargetCollection, ByRef MyDetectors As TargetCollection)
        Dim ln As String, i As Integer, aTarg As Target

        ' Writing devices (targets, detectors, sprinklers)
        If MyTargets.Count + MyDetectors.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Devices"
            PrintLine(IO, ln)

            For i = 0 To MyTargets.Count - 1
                aTarg = MyTargets.Item(i)
                ln = "&DEVC ID = '" + aTarg.Name + "' COMP_ID = '" + myCompartments.Item(aTarg.Compartment).Name + "'"
                ln += " LOCATION = " + aTarg.XPosition.ToString + ", " + aTarg.YPosition.ToString + ", " + aTarg.ZPosition.ToString
                If aTarg.SolutionType = Target.ThermallyThick Then
                    ln += " TYPE = 'PLATE'"
                Else
                    ln += " TYPE = 'CYLINDER'"
                End If
                ln += " MATL_ID = '" + myThermalProperties.Item(myThermalProperties.GetIndex(aTarg.Material)).ShortName + "' "
                If aTarg.TargetFacing = "-" Then
                    If aTarg.XNormal = 0 And aTarg.YNormal = 0 And aTarg.ZNormal = 1 Then
                        ln += "SURFACE_ORIENTATION = 'CEILING'"
                    ElseIf aTarg.XNormal = 0 And aTarg.YNormal = 0 And aTarg.ZNormal = -1 Then
                        ln += "SURFACE_ORIENTATION = 'FLOOR'"
                    ElseIf aTarg.XNormal = 1 And aTarg.YNormal = 0 And aTarg.ZNormal = 0 Then
                        ln += "SURFACE_ORIENTATION = 'FRONT WALL'"
                    ElseIf aTarg.XNormal = -1 And aTarg.YNormal = 0 And aTarg.ZNormal = 0 Then
                        ln += "SURFACE_ORIENTATION = 'BACK WALL'"
                    ElseIf aTarg.XNormal = 0 And aTarg.YNormal = 1 And aTarg.ZNormal = 0 Then
                        ln += "SURFACE_ORIENTATION = 'RIGHT WALL'"
                    ElseIf aTarg.XNormal = 0 And aTarg.YNormal = -1 And aTarg.ZNormal = 0 Then
                        ln += "SURFACE_ORIENTATION = 'LEFT WALL'"
                    Else
                        ln += "NORMAL = " + aTarg.XNormal.ToString + ", " + aTarg.YNormal.ToString + ", " + aTarg.ZNormal.ToString
                    End If
                Else
                    If InStr(Data.NormalPointsTo.ToUpper, aTarg.TargetFacing.ToUpper, CompareMethod.Text) > 0 Then
                        ln += "SURFACE_ORIENTATION = '" + aTarg.TargetFacing.ToUpper + "'"
                    Else
                        ln += "SURFACE_ORIENTATION = '" + aTarg.TargetFacing + "'"
                    End If
                End If
                PrintLine(IO, ln)
                ln = "    "
                If aTarg.Thickness > 0 Then ln += " THICKNESS = " + aTarg.Thickness.ToString
                ln += " TEMPERATURE_DEPTH = " + aTarg.InternalLocation.ToString
                ln += " DEPTH_UNITS = " + "'M'"
                If aTarg.FixedTemperature <> myEnvironment.IntAmbTemperature And aTarg.FixedTemperature <> -1001 Then
                    ln += " SURFACE_TEMPERATURE = " + aTarg.FixedTemperature.ToString
                End If
                If aTarg.Adiabatic = True Then
                    ln += " ADIABATIC_TARGET = .TRUE. CONVECTION_COEFFICIENTS = " + aTarg.Convection_Coefficient(1).ToString + ", " + aTarg.Convection_Coefficient(2).ToString
                End If
                If aTarg.FYI <> "" Then
                    ln += " FYI = '" + aTarg.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aTarg.Changed = False
            Next

            For i = 0 To MyDetectors.Count - 1
                aTarg = MyDetectors.Item(i)
                If aTarg.Name = "" Then
                    If aTarg.DetectorType = Target.TypeHeatDetector Then
                        aTarg.Name = "HeatDetector_" + (i + 1).ToString
                    ElseIf aTarg.DetectorType = Target.TypeSmokeDetector Then
                        aTarg.Name = "SmokeDetector_" + (i + 1).ToString
                    ElseIf aTarg.DetectorType = Target.TypeSprinkler Then
                        aTarg.Name = "Sprinkler_" + (i + 1).ToString
                    End If
                End If
                ln = "&DEVC ID = '" + aTarg.Name + "' COMP_ID = '" + myCompartments.Item(aTarg.Compartment).Name + "'"
                ln += " LOCATION = " + aTarg.XPosition.ToString + ", " + aTarg.YPosition.ToString + ", " + aTarg.ZPosition.ToString
                If aTarg.DetectorType = Target.TypeHeatDetector Then
                    ln += " TYPE = 'HEAT_DETECTOR' SETPOINT = " + Math.Round((aTarg.ActivationTemperature - 273.15), 2).ToString + ", RTI = " + aTarg.RTI.ToString + " /"
                ElseIf aTarg.DetectorType = Target.TypeSmokeDetector Then
                    If aTarg.ActivationObscurationSmoldering = 0 Then
                        ln += " TYPE = 'SMOKE_DETECTOR' SETPOINT = " + aTarg.ActivationObscurationFlaming.ToString + " /"
                    Else
                        ln += " TYPE = 'SMOKE_DETECTOR' SETPOINTS = " + aTarg.ActivationObscurationSmoldering.ToString + ", " + aTarg.ActivationObscurationFlaming.ToString + " /"
                    End If
                Else
                    ln += " TYPE = 'SPRINKLER' SETPOINT = " + Math.Round((aTarg.ActivationTemperature - 273.15), 2).ToString + ", RTI = " + aTarg.RTI.ToString + " SPRAY_DENSITY = " + aTarg.SprayDensity.ToString
                    If aTarg.FYI <> "" Then
                        ln += " FYI = '" + aTarg.FYI + "'"
                    End If
                    ln += " /"
                End If
                PrintLine(IO, ln)
                aTarg.Changed = False
            Next
        End If

    End Sub
    Private Sub WriteOutputFileNMLConn(IO As Integer, ByRef MyCompartments As CompartmentCollection, MyHHeats As VentCollection, MyVHeats As VentCollection)
        Dim ln As String, i, j As Integer, aVent As Vent

        'Writing Surface Connections
        If MyHHeats.Count + MyVHeats.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Surface Connections"
            PrintLine(IO, ln)

            Dim fracln As String
            For i = 0 To MyCompartments.Count - 1
                If MyHHeats.FromConnections(i) > 0 Then
                    ln = "&CONN TYPE = 'WALL'"
                    ln += " COMP_ID = '" + MyCompartments.Item(i).Name + "' "
                    fracln = "      F = "
                    ln += " COMP_IDS = "
                    For j = 0 To MyHHeats.Count - 1
                        aVent = MyHHeats.Item(j)
                        If aVent.FirstCompartment = i Then
                            If aVent.SecondCompartment = -1 Then
                                ln += " 'OUTSIDE'"
                            Else
                                ln += "  '" + MyCompartments.Item(aVent.SecondCompartment).Name + "'"
                            End If
                            fracln = fracln + aVent.InitialOpening.ToString + " "
                        End If
                        aVent.Changed = False
                    Next
                    PrintLine(IO, ln)
                    fracln += " /"
                    PrintLine(IO, fracln)
                End If
            Next

            For i = 0 To MyVHeats.Count - 1
                ln = "&CONN"
                aVent = MyVHeats.Item(i)
                If aVent.FirstCompartment = -1 Then
                    ln += " TYPE = 'CEILING'"
                Else
                    ln += " TYPE = 'FLOOR'"
                End If
                If aVent.FirstCompartment = -1 Then
                    ln += " COMP_ID = 'OUTSIDE'"
                Else
                    ln += " COMP_ID = '" + MyCompartments.Item(aVent.FirstCompartment).Name + "'"
                End If
                If aVent.SecondCompartment = -1 Then
                    ln += " COMP_IDS = 'OUTSIDE' "
                Else
                    ln += " COMP_IDS = '" + MyCompartments.Item(aVent.SecondCompartment).Name + "'"
                End If
                ln += " / "
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLIsoSlcf(IO As Integer, ByRef MyVisuals As VisualCollection)
        Dim ln As String, i As Integer, aVisual As Visual

        If MyVisuals.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Visualizations"
            PrintLine(IO, ln)

            For i = 0 To MyVisuals.Count - 1
                aVisual = MyVisuals.Item(i)
                If aVisual.Type = Visual.IsoSurface Then
                    ln = "&ISOF VALUE = " + Math.Round((aVisual.Value - 273.15), 2).ToString + " /"
                    PrintLine(IO, ln)
                Else
                    ln = "&SLCF"
                    If aVisual.Compartment > -1 Then
                        ln += " COMP_ID = '" + myCompartments.Item(aVisual.Compartment).Name + "'"
                    End If
                    If aVisual.Type = Visual.TwoD Then
                        ln += " DOMAIN = '2-D' "
                        ln += " POSITION = " + aVisual.Value.ToString + ", PLANE = '" + VisualAxisNames.Substring((aVisual.Axis) * 6, 1) + "'"
                    Else
                        ln += " DOMAIN = '3-D'"
                    End If
                    ln += " / "
                    PrintLine(IO, ln)
                End If
                aVisual.Changed = False
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLMRND(IO As Integer, ByRef myMRandoms As MonteCarloCollection)
        Dim ln As String, i As Integer, j As Integer, aRandom As MonteCarlo, max As Integer

        ' Writing &MRND
        If myMRandoms.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Monte Carlo Random Distributions"
            PrintLine(IO, ln)

            Dim Id As String, DistributionType As String, MinimumField As String, MaximumField As String, AddField As String, FYI As String
            Dim Minimum As Double, Maximum As Double, Mean As Double, StDev As Double, Alpha As Double, Beta As Double, Peak As Double, MinimumOffset As Double, MaximumOffset As Double
            Dim RandomSeeds(2) As Double, RealValues(0) As Double, IntegerValues(0) As Integer, LogicalValues(0) As Boolean, Probabilities(0) As Double, StringValues(0) As String
            Dim RealConstantValue As Double, IntegerConstantValue As Double, StringConstantValue As String, LogicalConstantValue As Boolean
            For i = 0 To myMRandoms.Count - 1
                Id = ""
                DistributionType = ""
                Minimum = 0.0
                Maximum = 0.0
                Mean = 0.0
                StDev = 0.0
                Alpha = 0.0
                Beta = 0.0
                RandomSeeds(1) = -1001.0
                RandomSeeds(2) = -1001.0
                RealConstantValue = 0.0
                IntegerConstantValue = 0
                StringConstantValue = ""
                LogicalConstantValue = False
                MinimumOffset = 0.0
                MaximumOffset = 0.0
                MinimumField = ""
                MaximumField = ""
                AddField = ""
                FYI = ""
                aRandom = myMRandoms.Item(i)
                aRandom.GetRandom(Id, DistributionType, Minimum, Maximum, Mean, StDev, Alpha, Beta, Peak, RandomSeeds, RealValues, RealConstantValue, IntegerValues, IntegerConstantValue, StringValues, StringConstantValue, LogicalValues, LogicalConstantValue, Probabilities, MinimumOffset, MaximumOffset, MinimumField, MaximumField, AddField, FYI)
                ln = "&MRND ID = '" + Id + "' DISTRIBUTION_TYPE = '" + DistributionType + "'"
                If DistributionType = "UNIFORM" Then
                    PrintLine(IO, ln)
                    If MinimumField = "" Then
                        ln = "      MINIMUM = " + Minimum.ToString
                    Else
                        ln = "      MINIMUM_FIELD = '" + MinimumField + "'"
                    End If
                    If MaximumField = "" Then
                        ln += " MAXIMUM = " + Maximum.ToString
                    Else
                        ln += " MAXIMUM_FIELD = '" + MaximumField + "'"
                    End If
                    If FYI <> "" Then
                        PrintLine(IO, ln)
                        ln = "      FYI = '" + FYI + "' /"
                    Else
                        ln += " /"
                    End If
                    PrintLine(IO, ln)
                ElseIf DistributionType = "TRIANGLE" Then
                    PrintLine(IO, ln)
                    If MinimumField = "" Then
                        ln = "      MINIMUM = " + Minimum.ToString
                    Else
                        ln = "      MINIMUM_FIELD = '" + MinimumField + "'"
                    End If
                    If MaximumField = "" Then
                        ln += " MAXIMUM = " + Maximum.ToString
                    Else
                        ln += " MAXIMUM_FIELD = '" + MaximumField + "'"
                    End If
                    ln += " PEAK = " + Peak.ToString
                    If FYI <> "" Then
                        PrintLine(IO, ln)
                        ln = "      FYI = '" + FYI + "' /"
                    Else
                        ln += " /"
                    End If
                    PrintLine(IO, ln)
                ElseIf DistributionType = "USER_DEFINED_DISCRETE" Or DistributionType = "USER_DEFINED_CONTINOUS_INTERVAL" Then
                    PrintLine(IO, ln)
                    ln = "      VALUES = "
                    If Not Data.IsArrayEmpty(RealValues) Then
                        max = RealValues.GetUpperBound(0)
                        If max > 0 Then
                            For j = 1 To max - 1
                                ln += RealValues(j).ToString + ", "
                            Next
                            ln += RealValues(max).ToString
                        End If
                    End If
                    PrintLine(IO, ln)
                    ln = "      PROBABILITIES = "
                    If Not Data.IsArrayEmpty(Probabilities) Then
                        max = Probabilities.GetUpperBound(0)
                        If max > 0 Then
                            For j = 1 To max - 1
                                ln += Probabilities(j).ToString + ", "
                            Next
                            ln += Probabilities(max).ToString
                        End If
                    End If
                    If FYI <> "" Then
                        PrintLine(IO, ln)
                        ln = "     FYI = '" + FYI + "' /"
                    Else
                        ln += " /"
                    End If
                    PrintLine(IO, ln)
                ElseIf DistributionType = "BETA" Then
                    ln += "  ALPHA = " + Alpha.ToString + "  BETA = " + Beta.ToString
                    PrintLine(IO, ln)
                    If MinimumField = "" Then
                        ln = "      MINIMUM = " + Minimum.ToString
                    Else
                        ln = "      MINIMUM_FIELD = '" + MinimumField + "'"
                    End If
                    If MaximumField = "" Then
                        ln += " MAXIMUM = " + Maximum.ToString
                    Else
                        ln += " MAXIMUM_FIELD = '" + MaximumField + "'"
                    End If
                    ln += " PEAK = " + Peak.ToString
                    If FYI <> "" Then
                        PrintLine(IO, ln)
                        ln = "     FYI = '" + FYI + "' /"
                    Else
                        ln += " /"
                    End If
                    PrintLine(IO, ln)
                ElseIf DistributionType = "GAMMA" Then
                    ln += "  ALPHA = " + Alpha.ToString + "  BETA = " + Beta.ToString + " /"
                    PrintLine(IO, ln)
                ElseIf DistributionType = "NORMAL" Or DistributionType = "LOG_NORMAL" Then
                    ln += "  MEAN = " + Mean.ToString + "  STDEV = " + StDev.ToString + " /"
                    PrintLine(IO, ln)
                ElseIf DistributionType = "TRUNCATED_NORMAL" Or DistributionType = "TRUNCATED_LOG_NORMAL" Then
                    ln += "  MEAN = " + Mean.ToString + "  STDEV = " + StDev.ToString
                    PrintLine(IO, ln)
                    If MinimumField = "" Then
                        ln = "      MINIMUM = " + Minimum.ToString
                    Else
                        ln = "      MINIMUM_FIELD = '" + MinimumField + "'"
                    End If
                    If MaximumField = "" Then
                        ln += " MAXIMUM = " + Maximum.ToString
                    Else
                        ln += " MAXIMUM_FIELD = '" + MaximumField + "'"
                    End If
                    If FYI <> "" Then
                        PrintLine(IO, ln)
                        ln = "      FYI = '" + FYI + "' /"
                    Else
                        ln += " /"
                    End If
                    PrintLine(IO, ln)
                ElseIf DistributionType = "CONSTANT" Then
                    ln += "  CONSTANT = " + RealConstantValue.ToString + " /"
                    PrintLine(IO, ln)
                End If
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLMFLD(IO As Integer, ByRef myMFields As MonteCarloCollection)
        Dim ln As String, i, j As Integer, aField As MonteCarlo
        Dim max As Integer
        ' Writing &OUTP
        If myMFields.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Monte-Carlo Field Definitions"
            PrintLine(IO, ln)
            Dim Id As String, RandId As String, FieldType As String, ParameterColumnLabel As String, FYI As String
            Dim RealValues(0) As Double, IntegerValues(0) As Integer, LogicalValues(0) As Boolean, StringValues(0) As String
            Dim Field(2) As String
            Dim AddToParameters As Boolean
            Dim BaseScalingValue As Double, Position As Integer
            For i = 0 To myMFields.Count - 1
                Id = ""
                FieldType = ""
                RandId = ""
                Field(1) = ""
                Field(2) = ""
                ParameterColumnLabel = ""
                AddToParameters = False
                BaseScalingValue = 1.0
                Position = -1001
                FYI = ""
                ReDim RealValues(0), IntegerValues(0), LogicalValues(0), StringValues(0)

                aField = myMFields.Item(i)
                aField.GetField(Id, FieldType, Field, RandId, ParameterColumnLabel, AddToParameters, RealValues, IntegerValues, StringValues, LogicalValues, BaseScalingValue, Position, FYI)

                If FieldType = "VALUE" Then
                    ln = "&MFLD ID = '" + Id + "'  FIELD_TYPE = '" + FieldType + "'" + "  RAND_ID = '" + RandId + "'  FIELD = '" + Field(1) + "', '" + Field(2) + "'"
                ElseIf FieldType = "LABEL" Then
                    ln = "&MFLD ID = '" + Id + "'  FIELD_TYPE = '" + FieldType + "'" + "  RAND_ID = '" + RandId + "'"
                    PrintLine(IO, ln)
                    If Not Data.IsArrayEmpty(StringValues) Then
                        max = StringValues.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      STRING_VALUES = "
                            For j = 1 To max - 1
                                ln += StringValues(j) + ", "
                            Next
                            ln += StringValues(max)
                        End If
                    End If
                ElseIf FieldType = "INDEX" Then
                    ln = "&MFLD ID = '" + Id + "'  FIELD_TYPE = '" + FieldType + "'" + "  RAND_ID = '" + RandId + "'" + "  FIELD = '" + Field(1) + "', '" + Field(2) + "'"
                    PrintLine(IO, ln)
                    If Not Data.IsArrayEmpty(RealValues) Then
                        max = RealValues.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      REAL_VALUES = "
                            For j = 1 To max - 1
                                ln += RealValues(j).ToString + ", "
                            Next
                            ln += RealValues(max).ToString
                        End If
                    End If
                    If Not Data.IsArrayEmpty(IntegerValues) Then
                        max = IntegerValues.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      INTEGER_VALUES = "
                            For j = 1 To max - 1
                                ln += IntegerValues(j).ToString + ", "
                            Next
                            ln += IntegerValues(max).ToString
                        End If
                    End If
                    If Not Data.IsArrayEmpty(StringValues) Then
                        max = StringValues.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      STRING_VALUES = "
                            For j = 1 To max - 1
                                ln += "'" + StringValues(j) + "', "
                            Next
                            ln += "'" + StringValues(max) + "'"
                        End If
                    End If
                    If Not Data.IsArrayEmpty(LogicalValues) Then
                        max = LogicalValues.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      LOGICAL_VALUES = "
                            For j = 1 To max - 1
                                ln += LogicalValues(j).ToString + ", "
                            Next
                            ln += LogicalValues(max).ToString
                        End If
                    End If
                ElseIf FieldType = "SCALING" Then
                    ln = "&MFLD ID = '" + Id + "'  FIELD_TYPE = '" + FieldType + "'"
                    If BaseScalingValue <> 1.0 Then
                        ln += +"  RAND_ID = '" + RandId + "'" + "  BASE_SCALING_VALUE = " + BaseScalingValue.ToString
                        ln += "'  FIELD = '" + Field(1) + "', '" + Field(2) + "'"
                    End If
                ElseIf FieldType = "POINTER" Then
                    ln = "&MFLD ID = '" + Id + "'  FIELD_TYPE = '" + FieldType + "'" + "  FIELD = '" + Field(1) + "', '" + Field(2) + "'"
                End If
                If ParameterColumnLabel <> "" Or FYI <> "" Or AddToParameters = False Then
                    PrintLine(IO, ln)
                    ln = "    "
                    If ParameterColumnLabel <> "" Then
                        ln += "  PARAMETER_COLUMN_LABEL = '" + ParameterColumnLabel + "'"
                    End If
                    If FYI <> "" Then
                        ln += "  FYI ='" + FYI + "'"
                    End If
                    If AddToParameters = False Then
                        ln += "  ADD_TO_PARAMETERS = .FALSE."
                    End If
                End If
                ln += " /"
                PrintLine(IO, ln)
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLMFIR(IO As Integer, ByRef myMFires As MonteCarloCollection)
        Dim ln As String, i As Integer, j As Integer, aFire As MonteCarlo, max As Integer

        ' These are for all fire types
        Dim id As String, FYI As String, FireID As String, BaseFireID As String
        Dim FireCompartmentRandomGeneratorID As String, FireCompartmentIDs() As String, FireCompartmentIDColumnLabel As String
        Dim ModifyFireAreatoMatchHRR As Boolean, AddFireCompartmentIDtoParameters As Boolean

        ' These are for incipient fire definitions
        Dim FlamingSmolderingIncipientRandomGeneratorID As String, IncipientFireTypes() As String
        Dim TypeofIncipientFireGrowth As String, FlamingIncipientDelayRandomGeneratorID As String, FlamingIncipientPeakRandomGeneratorID As String, SmolderingIncipientDelayRandomGeneratorID As String, SmolderingIncipientPeakRandomGeneratorID As String
        Dim IncipientTypeColumnLabel As String, IncipientTimeColumnLabel As String, IncipientPeakColumnLabel As String
        Dim AddIncipientTypetoParameters As Boolean, AddIncipientTimetoParameters As Boolean, AddIncipientPeaktoParameters As Boolean

        ' These are for scaling fires
        Dim ScalingFireHRRRandomGeneratorID As String, ScalingFireTimeRandomGeneratorID As String, HRRScaleColumnLabel As String, TimeScaleColumnLabel As String
        Dim AddHRRScaletoParameters As Boolean, AddTimeScaletoParameters As Boolean

        ' These are for time curve fire definitions, including power law fires
        Dim FireHRRGeneratorIDs() As String, FireTimeGeneratorIDs() As String, HRRLabels() As String, TimeLabels() As String
        Dim GeneratorIsTimeto1054kW As Boolean, AddFiretoParameters As Boolean, AddHRRtoParameters As Boolean, AddTimetoParameters As Boolean
        Dim NumberofGrowthPoints As Double, NumberofDecayPoints As Double, GrowthExponent As Double, DecayExponent As Double

        ' Writing &MFIR
        If myMFires.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Monte Carlo Fire Specifications"
            PrintLine(IO, ln)

            For i = 0 To myMFires.Count - 1
                id = ""
                FYI = ""
                FireID = ""
                BaseFireID = ""
                FireCompartmentRandomGeneratorID = ""
                AddFireCompartmentIDtoParameters = True
                FireCompartmentIDColumnLabel = ""

                FlamingSmolderingIncipientRandomGeneratorID = ""
                TypeofIncipientFireGrowth = ""
                FlamingIncipientDelayRandomGeneratorID = ""
                FlamingIncipientPeakRandomGeneratorID = ""
                SmolderingIncipientDelayRandomGeneratorID = ""
                SmolderingIncipientPeakRandomGeneratorID = ""
                AddIncipientTypetoParameters = True
                IncipientTypeColumnLabel = ""
                AddIncipientTimetoParameters = True
                IncipientTimeColumnLabel = ""
                IncipientPeakColumnLabel = ""
                IncipientPeakColumnLabel = ""

                ScalingFireHRRRandomGeneratorID = ""
                ScalingFireTimeRandomGeneratorID = ""
                AddHRRScaletoParameters = True
                HRRScaleColumnLabel = ""
                AddTimeScaletoParameters = True
                TimeScaleColumnLabel = True

                GeneratorIsTimeto1054kW = False
                AddFiretoParameters = True
                AddHRRtoParameters = True
                AddTimetoParameters = True
                NumberofGrowthPoints = -1001
                NumberofDecayPoints = -1001
                GrowthExponent = 1001.0
                DecayExponent = -1001.0

                aFire = myMFires.Item(i)
                aFire.GetFire(id, FYI, FireID, BaseFireID, ModifyFireAreatoMatchHRR, FireCompartmentRandomGeneratorID, FireCompartmentIDs, AddFireCompartmentIDtoParameters, FireCompartmentIDColumnLabel, FlamingSmolderingIncipientRandomGeneratorID, IncipientFireTypes, TypeofIncipientFireGrowth, FlamingIncipientDelayRandomGeneratorID, FlamingIncipientPeakRandomGeneratorID, SmolderingIncipientDelayRandomGeneratorID, SmolderingIncipientPeakRandomGeneratorID, AddIncipientTypetoParameters, AddIncipientTimetoParameters, AddIncipientPeaktoParameters, IncipientTypeColumnLabel, IncipientTimeColumnLabel, IncipientPeakColumnLabel, ScalingFireHRRRandomGeneratorID, ScalingFireTimeRandomGeneratorID, AddHRRScaletoParameters, HRRScaleColumnLabel, AddTimeScaletoParameters, TimeScaleColumnLabel, FireHRRGeneratorIDs, FireTimeGeneratorIDs, NumberofGrowthPoints, NumberofDecayPoints, GrowthExponent, DecayExponent, GeneratorIsTimeto1054kW, AddFiretoParameters, AddHRRtoParameters, AddTimetoParameters, HRRLabels, TimeLabels)
                ln = "&MFIR ID = '" + id + "'  FIRE_ID = '" + FireID + "'"
                PrintLine(IO, ln)
                ln = ""

                'Output for random fire compartment
                If FireCompartmentRandomGeneratorID <> "" Then
                    ln = "      FIRE_COMPARTMENT_RANDOM_GENERATOR_ID = " + FireCompartmentRandomGeneratorID
                    PrintLine(IO, ln)
                End If
                max = 0
                If Not Data.IsArrayEmpty(FireCompartmentIDs) Then
                    max = FireCompartmentIDs.GetUpperBound(0)
                    If max > 0 Then
                        ln = "      FIRE_COMPARTMENT_IDS = "
                        For j = 1 To max - 1
                            ln += "'" + FireCompartmentIDs(j) + "', "
                        Next
                        ln += "'" + FireCompartmentIDs(max) + "'"
                        PrintLine(IO, ln)
                    End If
                End If

                'Output for incipient fire
                If TypeofIncipientFireGrowth = "RANDOM" Then
                    ln = "      TYPE_OF_INCIPIENT_GROWTH = 'RANDOM'  FLAMING_SMOLDERING_INCIPIENT_RANDOM_GENERATOR_ID = '" + FlamingSmolderingIncipientRandomGeneratorID + "'"
                    PrintLine(IO, ln)
                    If Not Data.IsArrayEmpty(IncipientFireTypes) Then
                        max = IncipientFireTypes.GetUpperBound(0)
                        If max > 0 Then
                            ln = "      NUMBER_OF_INCIPIENT_FIRE_TYPES = " + max.ToString
                            PrintLine(IO, ln)
                            ln = "      INCIPIENT_FIRE_TYPES = "
                            For j = 1 To max - 1
                                ln += "'" + IncipientFireTypes(j) + "', "
                            Next
                            ln += "'" + IncipientFireTypes(max) + "'"
                            PrintLine(IO, ln)
                            ln = "      FLAMING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID = '" + FlamingIncipientDelayRandomGeneratorID + "'  FLAMING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID = '" + FlamingIncipientPeakRandomGeneratorID + "'"
                            PrintLine(IO, ln)
                            ln = "      SMOLDERING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID = '" + SmolderingIncipientDelayRandomGeneratorID + "'  SMOLDERING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID = '" + SmolderingIncipientPeakRandomGeneratorID + "'"
                            PrintLine(IO, ln)
                        End If
                    End If
                ElseIf TypeofIncipientFireGrowth = "FLAMING" Then
                    ln = "      TYPE_OF_INCIPIENT_GROWTH = 'FLAMING'"
                    PrintLine(IO, ln)
                    ln = "      FLAMING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID = '" + FlamingIncipientDelayRandomGeneratorID + "'  FLAMING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID = '" + FlamingIncipientPeakRandomGeneratorID + "'"
                    PrintLine(IO, ln)
                ElseIf TypeofIncipientFireGrowth = "SMOLDERING" Then
                    ln = "      TYPE_OF_INCIPIENT_GROWTH = 'SMOLDERING'"
                    PrintLine(IO, ln)
                    ln = "      SMOLDERING_INCIPIENT_DELAY_RANDOM_GENERATOR_ID = '" + SmolderingIncipientDelayRandomGeneratorID + "'  SMOLDERING_INCIPIENT_PEAK_RANDOM_GENERATOR_ID = '" + SmolderingIncipientPeakRandomGeneratorID + "'"
                    PrintLine(IO, ln)
                End If

                ' Output for a scaled fire
                If BaseFireID <> "" Then
                    ln = "      BASE_FIRE_ID = '" + BaseFireID + "'"
                    PrintLine(IO, ln)
                    ln = "      SCALING_FIRE_HRR_RANDOM_GENERATOR_ID = '" + ScalingFireHRRRandomGeneratorID + "'"
                    ln += "  SCALING_FIRE_TIME_RANDOM_GENERATOR_ID = '" + ScalingFireTimeRandomGeneratorID + "'"
                    If AddHRRScaletoParameters = False Or HRRScaleColumnLabel <> "" Or AddTimeScaletoParameters = False Or TimeScaleColumnLabel <> "" Then
                        PrintLine(IO, ln)
                        ln = "     "
                        If AddHRRScaletoParameters = False Then
                            ln += "  ADD_HRR_SCALE_TO_PARAMETERS = .FALSE."
                        Else
                            If HRRScaleColumnLabel <> "" Then ln += "  ADD_HRR_SCALE_TO_PARAMETERS = .TRUE.  HRR_SCALE_COLUMN_LABEL = '" + HRRScaleColumnLabel + "'"
                        End If
                        If AddTimeScaletoParameters = False Then
                            ln += "  ADD_TIME_SCALE_TO_PARAMETERS = .FALSE."
                        Else
                            If TimeScaleColumnLabel <> "" Then ln += "  ADD_TIME_SCALE_TO_PARAMETERS = .TRUE.  TIME_SCALE_COLUMN_LABEL = '" + TimeScaleColumnLabel + "'"
                        End If
                        If ln <> "     " Then PrintLine(IO, ln + " /")
                    Else
                        ln += " /"
                        PrintLine(IO, ln)
                    End If
                End If

                    ' Output for a timed or power law fire
                    If GeneratorIsTimeto1054kW = True Then
                    ln = "      GENERATOR_IS_TIME_TO_1054_KW = .TRUE."
                    PrintLine(IO, ln)
                End If
                max = 0
                If Not Data.IsArrayEmpty(FireTimeGeneratorIDs) Then
                    max = FireTimeGeneratorIDs.GetUpperBound(0)
                    If max > 0 Then
                        ln = "      FIRE_TIME_GENERATOR_IDS = "
                        For j = 1 To max - 1
                            ln += "'" + FireTimeGeneratorIDs(j) + "', "
                        Next
                        ln += "'" + FireTimeGeneratorIDs(max) + "'"
                        PrintLine(IO, ln)
                        ln = "      FIRE_HRR_GENERATOR_IDS = "
                        For j = 1 To max - 1
                            ln += "'" + FireHRRGeneratorIDs(j) + "', "
                        Next
                        ln += "'" + FireHRRGeneratorIDs(max) + "'"
                        If NumberofGrowthPoints <= 0 And NumberofDecayPoints <= 0 Then ln += " /"
                        PrintLine(IO, ln)
                    End If
                    If NumberofGrowthPoints > 0 Then
                        ln = "      NUMBER_OF_GROWTH_POINTS = " + NumberofGrowthPoints.ToString + "  GROWTH_EXPONENT = " + GrowthExponent.ToString
                    End If
                    If NumberofDecayPoints > 0 Then
                        ln += "  NUMBER_OF_DECAY_POINTS = " + NumberofDecayPoints.ToString + "  DECAY_EXPONENT = " + DecayExponent.ToString
                    End If
                    If NumberofGrowthPoints > 0 Or NumberofDecayPoints > 0 Then
                        ln += " /"
                        PrintLine(IO, ln)
                    End If
                End If
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLOutp(IO As Integer, ByRef myOutps As MonteCarloCollection)
        Dim ln As String, i As Integer, aOutput As MonteCarlo

        ' Writing &OUTP
        If myOutps.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! User-specified Outputs"
            PrintLine(IO, ln)

            Dim Id, FileType, Type, FirstMeasurement, FirstDevice, SecondMeasurement, SecondDevice, FYI As String
            Dim Criterion As Double
            For i = 0 To myOutps.Count - 1
                Id = ""
                FileType = ""
                Type = ""
                Criterion = 0.0
                FirstMeasurement = ""
                FirstDevice = ""
                SecondMeasurement = ""
                SecondDevice = ""
                FYI = ""
                aOutput = myOutps.Item(i)
                aOutput.GetOutput(Id, FileType, Type, Criterion, FirstMeasurement, FirstDevice, SecondMeasurement, SecondDevice, FYI)

                ln = "&OUTP ID = '" + Id + "'"
                PrintLine(IO, ln)
                ln = "      FILE = '" + FileType + "'  TYPE = '" + Type + "'"
                If Type <> "MINIMUM" And Type <> "MAXIMUM" And Type <> "CHECK_TOTAL_HRR" Then
                    ln += "  CRITERION = " + Criterion.ToString
                End If
                PrintLine(IO, ln)
                ln = "      FIRST_FIELD = '" + FirstDevice + "', '" + FirstMeasurement + "'"
                If Type <> "MINIMUM" And Type <> "MAXIMUM" And Type <> "CHECK_TOTAL_HRR" Then
                    ln += "  SECOND_FIELD = '" + SecondDevice + "', '" + SecondMeasurement + "'"
                End If
                If FYI <> "" Then
                    ln += " FYI = '" + FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
            Next
        End If
    End Sub
    Private Sub WriteOutputFileNMLMSTT(IO As Integer, ByRef myMStats As MonteCarloCollection)
        Dim ln As String, i As Integer, aStat As MonteCarlo

        ' Writing &MSTT
        If myMStats.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Monte-Carlo Statistics Outputs"
            PrintLine(IO, ln)

            Dim id As String, AnalysisType As String, InputFileName As String, OutputFileName As String, ErrorFileName As String, LogFileName As String, ColumnLabel As String, FYI As String
            For i = 0 To myMStats.Count - 1
                id = ""
                AnalysisType = ""
                InputFileName = ""
                OutputFileName = ""
                ErrorFileName = ""
                LogFileName = ""
                ColumnLabel = ""
                aStat = myMStats.Item(i)
                aStat.GetStat(id, FYI, AnalysisType, InputFileName, OutputFileName, ErrorFileName, LogFileName, ColumnLabel)
                ln = "&MSTT ID ='" + id + "'"
                If AnalysisType <> "" Then ln += "  ANALYSIS_TYPE = '" + AnalysisType + "'"
                If InputFileName <> "" Then ln += "  INPUT_FILE_NAME = '" + InputFileName + "'"
                If OutputFileName <> "" Then ln += "  OUTPUT_FILE_NAME = '" + OutputFileName + "'"
                If ErrorFileName <> "" Then ln += "  ERROR_FILE_NAME = '" + ErrorFileName + "'"
                If LogFileName <> "" Then ln += "  LOG_FILE_NAME = '" + LogFileName + "'"
                If ColumnLabel <> "" Then ln += "  COLUMN_LABEL = '" + ColumnLabel + "'"
                PrintLine(IO, ln)
                If FYI <> "" Then
                    ln = "      FYI = '" + FYI + "'"
                    PrintLine(IO, ln)
                End If
            Next
        End If
    End Sub
    Public Sub WriteDIAGsimpleln(IO As Integer, ByRef wrtDIAG As Boolean, ByRef wrtSlash As Boolean, line As String)
        Dim ln As String

        If wrtDIAG Then
            ln = "&DIAG "
            wrtDIAG = False
            wrtSlash = True
        Else
            ln = " "
        End If
        ln += line
        PrintLine(IO, ln)
    End Sub
#End Region
#Region "Support Routines"
    Private Sub AddHeadertoOutput(ByRef csv As CSVsheet, ByRef i As Integer, header As String)
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!" + header
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
    End Sub
    Private Function SkipLine(str As String) As Boolean
        If str = Nothing Then
            Return True
        End If
        If str.Length = 0 Then
            Return True
        ElseIf str.Substring(0, 1) = "!" Or str.Substring(0, 1) = "#" Then
            Return True
        Else
            Return False
        End If
    End Function
    Private Function HeaderComment(str As String) As Boolean
        If str = Nothing Then
            Return False
        End If
        If str.Length > 1 Then
            If str.Substring(0, 2) = "!*" Then
                Return True
            End If
        Else
            Return False
        End If
    End Function
    Private Function Comment(str As String) As Boolean
        If str = Nothing Then
            Return True
        End If
        If str.Length > 0 Then
            If str.Substring(0, 1) = "!" Then
                Return True
            End If
        Else
            Return False
        End If
    End Function
    Private Function DropComment(str As String) As Boolean
        If str = Nothing Then
            Return True
        End If
        If str.Length > 1 Then
            If str.Substring(0, 2) = "!!" Then
                Return True
            End If
        ElseIf str.Length = 0 Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Function GetCommandLineArg() As String
        ' Declare variables.
        Dim command As String = Microsoft.VisualBasic.Command()
        If command.Length > 0 Then
            If command.Substring(0, 1) = """" Then
                command = command.Remove(0, 1)
                command = command.Remove(command.Length - 1, 1)
            End If
        End If
        Return command
    End Function
#End Region
End Module

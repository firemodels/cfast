Imports System
Imports System.IO
Module IO
#Region "Read Routines"
    Public Sub ReadInputFile(ByVal Filename As String)
        Dim IO As Integer = 1
        Dim str As String

        FileOpen(IO, Filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
        str = LineInput(IO)
        If str.Substring(0, 1) = "&" Then
            FileClose(IO)
            ReadInputFileNML(Filename)
        ElseIf str.Substring(0, 5) = "VERSN" Then
            FileClose(IO)
            ReadInputFileCSV(Filename)
        End If
    End Sub
    Public Sub ReadInputFileCSV(ByVal Filename As String)
        'Read in a *.in file Filename is to include path as well as file name
        Dim csv As New CSVsheet(Filename)
        Dim i As Integer = 1, j As Integer
        Dim NewFileFormat As Boolean = False

        myErrors.Break(Filename)

        ' check for new format input file where fire objects and thermal properties are in the input file rather than in separate databases
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                    Case "MATL"
                        NewFileFormat = True
                    Case "FIRE"
                        NewFileFormat = True
                    Case "CHEMI"
                        If csv.num(i, 0) > 3 Then NewFileFormat = True
                End Select
            End If
            i += 1
        Loop

        ' Get thermal properties from the input file
        FindThermalProperties(csv, myThermalProperties)

        ' Define compartments with appropriate thermal properties
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                If csv.str(i, CFASTlnNum.keyWord) = "COMPA" Then
                    Dim compa As New Compartment
                    myCompartments.Add(compa)
                    compa.Name = csv.str(i, compaNum.Name)
                    compa.SetSize(csv.num(i, compaNum.Width), csv.num(i, compaNum.Depth), csv.num(i, compaNum.Height))
                    compa.SetPosition(csv.num(i, compaNum.AbsXPos), csv.num(i, compaNum.AbsYPos),
                            csv.num(i, compaNum.FlrHeight))
                    compa.SetMaterial(csv.str(i, compaNum.CeilingMat), csv.str(i, compaNum.WallMat),
                            csv.str(i, compaNum.FloorMat))
                    If csv.num(i, 0) > compaNum.WallMat Then compa.SetGrid(csv.num(i, compaNum.xGrid), csv.num(i, compaNum.yGrid), csv.num(i, compaNum.zGrid))
                    compa.Changed = False
                End If
            End If
            i += 1
        Loop

        ' Define targets before fires so that fires can utilize a user-chosen target for ignition
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                If csv.str(i, CFASTlnNum.keyWord) = "TARGET" Then
                    Dim aDetect As New Target
                    aDetect.Type = 0
                    aDetect.SetPosition(csv.num(i, targetNum.xPosition), csv.num(i, targetNum.yPosition),
                        csv.num(i, targetNum.zPosition), csv.num(i, targetNum.xNormal), csv.num(i, targetNum.yNormal),
                        csv.num(i, targetNum.zNormal))
                    Dim type As Integer
                    If csv.str(i, targetNum.equationType) = "CYL" Then
                        type = 1
                    ElseIf csv.str(i, targetNum.equationType) = "ODE" Then
                        type = 0    ' ODE is only for older files.  Automatically convert it to thermally thick
                        myErrors.Add("Target " + (myTargets.Count + 1).ToString + " specified no longer supported thermmaly thin calculation. Converted to thermally thick", ErrorMessages.TypeWarning)
                    Else ' PDE
                        type = 0
                    End If
                    aDetect.SetTarget(csv.num(i, targetNum.compartment) - 1, csv.str(i, targetNum.material), type)
                    If (csv.str(i, targetNum.internalLocation) <> "") Then
                        aDetect.InternalLocation = csv.num(i, targetNum.internalLocation)
                    Else
                        aDetect.InternalLocation = 0.5
                    End If
                    If csv.str(i, targetNum.name).Length > 0 Then
                        aDetect.Name = csv.str(i, targetNum.name)
                    Else
                        aDetect.Name = "Targ " + (myTargets.Count + 1).ToString
                    End If
                    aDetect.Changed = False
                    myTargets.Add(aDetect)
                End If
            End If
            i += 1
        Loop

        ' do other keywords
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                    Case "ADIAB"
                        myEnvironment.AdiabaticWalls = True
                    Case "CJET"         ' This is an obsolescent command
                    Case "COMPA"        ' Done in first loop
                    Case "DETECT"
                        Dim aDetect As New Target
                        aDetect.Type = Target.TypeDetector
                        aDetect.Compartment = csv.num(i, detectNum.compartment) - 1
                        aDetect.XPosition = csv.num(i, detectNum.xPosition)
                        aDetect.YPosition = csv.num(i, detectNum.yPosition)
                        aDetect.ZPosition = csv.num(i, detectNum.zPosition)
                        If csv.num(i, detectNum.type) = 1 Then
                            aDetect.DetectorType = Target.TypeSmokeDetector
                            aDetect.ActivationTemperature = csv.num(i, detectNum.activationTemp)
                        ElseIf csv.num(i, detectNum.type) = 2 Then
                            If csv.num(i, detectNum.suppression) = 1 Then
                                aDetect.DetectorType = Target.TypeSprinkler
                                aDetect.ActivationTemperature = csv.num(i, detectNum.activationTemp)
                                aDetect.RTI = csv.num(i, detectNum.RTI)
                                aDetect.SprayDensity = csv.num(i, detectNum.sprayDensity)
                            Else
                                aDetect.DetectorType = Target.TypeHeatDetector
                                aDetect.ActivationTemperature = csv.num(i, detectNum.activationTemp)
                                aDetect.RTI = csv.num(i, detectNum.RTI)
                            End If
                        ElseIf csv.str(i, detectNum.type) = "SMOKE" Then
                            aDetect.DetectorType = Target.TypeSmokeDetector
                            aDetect.ActivationObscuration = csv.num(i, detectNum.activationObscuration)
                        ElseIf csv.str(i, detectNum.type) = "HEAT" Then
                            aDetect.DetectorType = Target.TypeHeatDetector
                            aDetect.ActivationTemperature = csv.num(i, detectNum.activationTemp)
                            aDetect.RTI = csv.num(i, detectNum.RTI)
                        ElseIf csv.str(i, detectNum.type) = "SPRINKLER" Then
                            aDetect.DetectorType = Target.TypeSprinkler
                            aDetect.ActivationTemperature = csv.num(i, detectNum.activationTemp)
                            aDetect.RTI = csv.num(i, detectNum.RTI)
                            aDetect.SprayDensity = csv.num(i, detectNum.sprayDensity)
                        End If
                        aDetect.Changed = False
                        myDetectors.Add(aDetect)
                    Case "DJIGN"
                        myEnvironment.IgnitionTemp = csv.num(i, djignNum.igntemp)
                    Case "DTCHECK"              'ignored for now
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword DTCHECK not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "EAMB"
                        myEnvironment.ExtAmbTemperature = csv.num(i, ambNum.ambTemp)
                        myEnvironment.ExtAmbPressure = csv.num(i, ambNum.ambPress)
                        myEnvironment.ExtAmbElevation = csv.num(i, ambNum.refHeight)
                        myEnvironment.Changed = False
                    Case "EVENT"
                    Case "FIRE"
                        Dim aFire As New Fire
                        ReadEmbeddedFire(csv, i, aFire)
                        aFire.Changed = False
                        myFires.Add(aFire)
                        i += 8
                    Case "GLOBA"
                        If csv.num(i, 0) <= 3 Then
                            ' only process sshort form here ... sets global parameters
                            myEnvironment.LowerOxygenLimit = csv.num(i, chemieNum.limo2)
                            myEnvironment.IgnitionTemp = csv.num(i, chemieNum.igntemp)
                            myEnvironment.Changed = False
                        End If
                    Case "HALL"
                        If csv.num(i, hallNum.compartment) <= myCompartments.Count Then
                            j = csv.num(i, hallNum.compartment) - 1
                            If myCompartments(j).Shaft Then
                                myErrors.Add("Keyword HALL compartment  " + csv.str(i, hallNum.compartment) + " is already declared an one zone compartment and will be changed to a hall ", ErrorMessages.TypeError)
                            End If
                            If csv.num(i, 0) > 2 Then myErrors.Add("Keyword HALL is an outdated format " + csv.strrow(i) + " hallway flow inputs will be ignored", ErrorMessages.TypeWarning)
                            myCompartments(j).Hall = True
                            myCompartments(j).Changed = False
                        End If
                    Case "HHEAT"
                        If csv.num(i, hheatNum.num) > 1 Then
                            Dim cFirst As Integer, CSecond As Integer, cFraction As Single
                            For j = 1 To csv.num(i, hheatNum.num)
                                Dim aHeat As New Vent
                                cFirst = csv.num(i, hheatNum.firstCompartment)
                                CSecond = csv.num(i, hheatNum.secondCompartment + 2 * (j - 1))
                                cFraction = csv.num(i, hheatNum.fraction + 2 * (j - 1))
                                If CSecond > myCompartments.Count Then CSecond = 0
                                aHeat.SetVent(cFirst - 1, CSecond - 1, cFraction)
                                aHeat.Changed = False
                                myHHeats.Add(aHeat)
                            Next
                        ElseIf csv.num(i, hheatNum.num) = 1 Then
                            Dim aHeat As New Vent
                            If csv.num(i, hheatNum.secondCompartment) > myCompartments.Count Then csv.num(i, hheatNum.secondCompartment) = 0
                            aHeat.SetVent(csv.num(i, hheatNum.firstCompartment) - 1, csv.num(i, hheatNum.secondCompartment) - 1, csv.num(i, hheatNum.fraction))
                            aHeat.Changed = False
                            myHHeats.Add(aHeat)
                        ElseIf csv.num(i, hheatNum.num) = 0 Then
                            dataFileComments.Add("!" + csv.strrow(i))
                            myErrors.Add("Keyword HHEAT with single compartment specification not supported line" + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                        End If
                    Case "HVENT"
                        Dim hvent As New Vent
                        If csv.num(i, hventNum.firstcompartment) > myCompartments.Count Then _
                            csv.num(i, hventNum.firstcompartment) = 0
                        If csv.num(i, hventNum.secondcompartment) > myCompartments.Count Then _
                            csv.num(i, hventNum.secondcompartment) = 0
                        hvent.SetVent(csv.num(i, hventNum.firstcompartment) - 1, csv.num(i, hventNum.secondcompartment) - 1,
                            csv.num(i, hventNum.width), csv.num(i, hventNum.soffit), csv.num(i, hventNum.sill))

                        If csv.num(i, 0) > 12 Then
                            ' This is the new format that includes trigger by flux or temperature
                            hvent.Face = csv.str(i, hventNum.face)
                            hvent.Offset = csv.num(i, hventNum.hall1)
                            If InStr(OpenTypes, csv.str(i, hventNum.openingtype), CompareMethod.Text) > 0 Then
                                hvent.OpenType = InStr(OpenTypes, csv.str(i, hventNum.openingtype), CompareMethod.Text) / 4
                                If hvent.OpenType = Vent.OpenbyTime Then
                                    hvent.InitialOpeningTime = csv.num(i, hventNum.openinitialtime)
                                    hvent.InitialOpening = csv.num(i, hventNum.openinitialfraction)
                                    hvent.FinalOpeningTime = csv.num(i, hventNum.openfinaltime)
                                    hvent.FinalOpening = csv.num(i, hventNum.openfinalfraction)
                                Else
                                    hvent.OpenValue = csv.num(i, hventNum.opencriterion)
                                    hvent.Target = csv.str(i, hventNum.opentarget)
                                    hvent.InitialOpening = csv.num(i, hventNum.openinitialfraction)
                                    hvent.FinalOpening = csv.num(i, hventNum.openfinalfraction)
                                End If
                            Else
                                myErrors.Add("Keyword HVENT format error " + csv.strrow(i) + " Argument 11 must be TIME, TEMP, or FLUX. Input ignored", ErrorMessages.TypeFatal)
                            End If
                        ElseIf csv.num(i, 0) = 12 Then
                            ' This is the old format that had wind input (after sill) and second compartment offset (after hall1). This shifts the actually used inputs
                            hvent.Face = csv.str(i, hventNum.face + 1)
                            hvent.Offset = csv.num(i, hventNum.hall1 + 1)
                            hvent.InitialOpening = csv.num(i, hventNum.initialfraction + 2)
                            hvent.FinalOpening = csv.num(i, hventNum.initialfraction + 2)
                        Else
                            ' This is the not quite as old format input without the wind or second offset
                            hvent.Face = csv.str(i, hventNum.face - 1)
                            hvent.Offset = csv.num(i, hventNum.hall1)
                            hvent.InitialOpening = csv.num(i, hventNum.initialfraction - 1)
                            hvent.FinalOpening = csv.num(i, hventNum.initialfraction - 1) ' This is the default; it may be changed by an EVENT specification
                        End If
                        hvent.Changed = False
                        myHVents.Add(hvent)
                    Case "INTER"        'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword INTER not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "ISOF"
                        Dim aVisual As New Visual
                        aVisual.Type = Visual.IsoSurface
                        aVisual.Value = csv.num(i, visualNum.isoValue)
                        If csv.num(i, 0) >= visualNum.isoCompartment Then
                            aVisual.Compartment = csv.num(i, visualNum.isoCompartment) - 1
                        Else
                            aVisual.Compartment = -1
                        End If
                        aVisual.Changed = False
                        myVisuals.Add(aVisual)
                    Case "LFBO"         'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword LFBO not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "LFBT"         'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword LFBT not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "LIMO2"
                        myEnvironment.LowerOxygenLimit = csv.num(i, 2)
                        myEnvironment.Changed = False
                        'Case "MAINF"
                        'Dim aFire As New Fire
                        'aFire.Name = "mainfire"
                        'aFire.SetPosition(csv.num(i, fireNum.compartment) - 1, csv.num(i, fireNum.xPosition), csv.num(i, fireNum.yPosition), csv.num(i, fireNum.zposition))
                        'aFire.PlumeType = csv.num(i, fireNum.plumeType) - 1
                        'aFire.FireObject = myFireObjects.GetFireIndex(aFire.Name)
                        'aFire.Changed = False
                        'myFires.Add(aFire)
                    Case "MVFAN"        'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword MVFAN not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "MVDCT"        'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword MVDCT not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "MVENT"
                        Dim mvent As New Vent
                        If csv.num(i, mventNum.fromCompartment) > myCompartments.Count Then _
                            csv.num(i, mventNum.fromCompartment) = 0
                        If csv.num(i, mventNum.toCompartment) > myCompartments.Count Then _
                            csv.num(i, mventNum.toCompartment) = 0
                        mvent.SetVent(csv.num(i, mventNum.fromCompartment) - 1, csv.num(i, mventNum.fromArea),
                            csv.num(i, mventNum.fromHeight), csv.str(i, mventNum.fromOpenOrien),
                            csv.num(i, mventNum.toCompartment) - 1, csv.num(i, mventNum.toArea),
                            csv.num(i, mventNum.toHeight), csv.str(i, mventNum.toOpenOrien), csv.num(i, mventNum.flow),
                            csv.num(i, mventNum.beginFlowDrop), csv.num(i, mventNum.flowZero))
                        ' This is the new open format to open by TIME, TEMP, or FLUX
                        If InStr(OpenTypes, csv.str(i, mventNum.openingtype), CompareMethod.Text) > 0 Then
                            mvent.OpenType = InStr(OpenTypes, csv.str(i, mventNum.openingtype), CompareMethod.Text) / 4
                            If mvent.OpenType = Vent.OpenbyTime Then
                                mvent.InitialOpeningTime = csv.num(i, mventNum.openinitialtime)
                                mvent.InitialOpening = csv.num(i, mventNum.openinitialfraction)
                                mvent.FinalOpeningTime = csv.num(i, mventNum.openfinaltime)
                                mvent.FinalOpening = csv.num(i, mventNum.openfinalfraction)
                            Else
                                mvent.OpenValue = csv.num(i, mventNum.opencriterion)
                                mvent.Target = csv.str(i, mventNum.opentarget)
                                mvent.InitialOpening = csv.num(i, mventNum.openinitialfraction)
                                mvent.FinalOpening = csv.num(i, mventNum.openfinalfraction)
                            End If
                            mvent.OffsetX = csv.num(i, mventNum.xoffset)
                            mvent.OffsetY = csv.num(i, mventNum.yoffset)
                            mvent.Changed = False
                        Else
                            ' This is the old format that is just time and partially implemented in EVENT keyword
                            mvent.InitialOpening = csv.num(i, mventNum.initialfraction)
                            mvent.FinalOpening = csv.num(i, mventNum.initialfraction) ' This is the default; it may be changed by an EVENT specification
                            mvent.OffsetX = 0.0
                            mvent.OffsetY = -1
                            mvent.Changed = True
                        End If
                        myMVents.Add(mvent)
                    Case "OBJECT"
                        Dim FireFile As String
                        ' First look for the fire object in the current folder (which should be where the .in file is located)
                        If myFires.GetFireIndex(csv.str(i, objfireNum.name)) < 0 Then
                            FireFile = csv.str(i, objfireNum.name) + ".o"
                            readFires(FireFile, InsertDataType.ObjectFile)
                        End If
                        ' If we didn't find it there, look in the CFAST bin directory
                        If myFires.GetFireIndex(csv.str(i, objfireNum.name)) < 0 Then
                            FireFile = Application.StartupPath + "\" + csv.str(i, objfireNum.name) + ".o"
                            readFires(FireFile, InsertDataType.ObjectFile)
                        End If
                        If myFires.GetFireIndex(csv.str(i, objfireNum.name)) >= 0 Then
                            Dim aFire As New Fire
                            aFire.Name = csv.str(i, objfireNum.name)
                            aFire.SetPosition(csv.num(i, objfireNum.compartment) - 1, csv.num(i, objfireNum.xPosition),
                                csv.num(i, objfireNum.yPosition), csv.num(i, objfireNum.zposition))
                            aFire.PlumeType = csv.num(i, objfireNum.plumeType) - 1
                            aFire.IgnitionType = csv.num(i, objfireNum.ignType) - 1
                            aFire.IgnitionValue = csv.num(i, objfireNum.ignCriterion)
                            aFire.Changed = False
                            myFires.Add(aFire)
                        Else
                            myErrors.Add("Fire Object " + csv.str(i, objfireNum.name) + " does not exist and will not be added to the simulation", ErrorMessages.TypeError)
                        End If
                    Case "OBJFL"        'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword OBJFL not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "ONEZ"
                        If csv.num(i, 2) <= myCompartments.Count Then
                            If myCompartments(csv.num(i, 2) - 1).Hall Then
                                myErrors.Add("Keyword ONEZ room  " + csv.str(i, 2) + " is already declared a hall and will be changed to a one zone compartment ", ErrorMessages.TypeError)
                            End If
                            myCompartments(csv.num(i, 2) - 1).Shaft = True
                            myCompartments(csv.num(i, 2) - 1).Changed = False
                        End If
                    Case "ROOMA"
                        If csv.num(i, 2) <= myCompartments.Count Then
                            Dim aComp As Compartment = myCompartments(csv.num(i, 2) - 1)
                            Dim area(csv.num(i, 3)) As Single
                            For j = 1 To csv.num(i, 3)
                                area(j) = csv.num(i, j + 3)
                            Next
                            aComp.SetVariableAreaPoints(area)
                            aComp.Changed = False
                        End If
                    Case "ROOMH"
                        If csv.num(i, 2) <= myCompartments.Count Then
                            Dim aComp As Compartment = myCompartments(csv.num(i, 2) - 1)
                            Dim height(csv.num(i, 3)) As Single
                            For j = 1 To csv.num(i, 3)
                                height(j) = csv.num(i, j + 3)
                            Next
                            aComp.SetVariableAreasHeight(height)
                            aComp.Changed = False
                        End If
                    Case "SETP"         'ignored
                        dataFileComments.Add("!" + csv.strrow(i))
                        myErrors.Add("Keyword SETP not supported line " + csv.strrow(i) + " will be commented out", ErrorMessages.TypeWarning)
                    Case "SLCF"
                        Dim aVisual As New Visual
                        If csv.str(i, visualNum.sliceType) = "2-D" Then
                            aVisual.Type = Visual.TwoD
                            If csv.str(i, visualNum.slice2DAxis) = "X" Then aVisual.Axis = 0
                            If csv.str(i, visualNum.slice2DAxis) = "Y" Then aVisual.Axis = 1
                            If csv.str(i, visualNum.slice2DAxis) = "Z" Then aVisual.Axis = 2
                            aVisual.Value = csv.num(i, visualNum.slice2DPosition)
                            If csv.num(i, 0) >= visualNum.slice2DCompartment Then
                                aVisual.Compartment = csv.num(i, visualNum.slice2DCompartment) - 1
                            Else
                                aVisual.Compartment = -1
                            End If
                        Else
                            aVisual.Type = Visual.ThreeD
                            If csv.num(i, 0) >= visualNum.slice3DCompartment Then
                                aVisual.Compartment = csv.num(i, visualNum.slice3DCompartment) - 1
                            Else
                                aVisual.Compartment = -1
                            End If
                        End If
                        aVisual.Changed = False
                        myVisuals.Add(aVisual)
                    Case "STPMAX"
                        myEnvironment.MaximumTimeStep = csv.num(i, 2)
                        myEnvironment.Changed = False
                    Case "TAMB"
                        myEnvironment.IntAmbTemperature = csv.num(i, ambNum.ambTemp)
                        myEnvironment.IntAmbPressure = csv.num(i, ambNum.ambPress)
                        myEnvironment.IntAmbElevation = csv.num(i, ambNum.refHeight)
                        myEnvironment.IntAmbRH = csv.num(i, ambNum.relHumidity)
                        myEnvironment.Changed = False
                    Case "THRMF"
                        myThermalProperties.Clear()
                        ReadThermalProperties(".\" + csv.str(i, 2).Trim + ".csv", myThermalProperties)
                    Case "TIMES"
                        myEnvironment.SimulationTime = csv.num(i, timesNum.simTime)
                        myEnvironment.OutputInterval = Math.Abs(csv.num(i, timesNum.printInterval))
                        If csv.num(i, 0) = 5 Then
                            myEnvironment.SmokeviewInterval = csv.num(i, timesNum.smokeviewInterval)
                            myEnvironment.SpreadsheetInterval = csv.num(i, timesNum.spreadsheetInterval)
                        Else
                            ' This is the old format input file that has a history file entry
                            myEnvironment.SmokeviewInterval = csv.num(i, timesNum.smokeviewInterval + 1)
                            myEnvironment.SpreadsheetInterval = csv.num(i, timesNum.spreadsheetInterval + 1)
                        End If
                        myEnvironment.Changed = False
                    Case "VERSN"
                        Dim aTitle As String
                        aTitle = csv.str(i, CFASTlnNum.title)
                        If csv.num(i, 0) > CFASTlnNum.title Then
                            For j = CFASTlnNum.title + 1 To csv.num(i, 0)
                                aTitle = aTitle + " " + csv.str(i, j)
                            Next
                        End If
                        myEnvironment.Title = aTitle
                        myEnvironment.Changed = False
                    Case "VHEAT"
                        Dim vheat As New Vent
                        If csv.num(i, vheatNum.firstcompartment) > myCompartments.Count Then _
                            csv.num(i, vheatNum.firstcompartment) = 0
                        If csv.num(i, vheatNum.secondcompartment) > myCompartments.Count Then _
                            csv.num(i, vheatNum.secondcompartment) = 0
                        vheat.SetVent(csv.num(i, vheatNum.firstcompartment) - 1, csv.num(i, vheatNum.secondcompartment) - 1)
                        vheat.Changed = False
                        myVHeats.Add(vheat)
                    Case "VVENT"
                        Dim vvent As New Vent
                        If csv.num(i, vventNum.firstcompartment) > myCompartments.Count Then _
                                    csv.num(i, vventNum.firstcompartment) = 0
                        If csv.num(i, vventNum.secondcompartment) > myCompartments.Count Then _
                                    csv.num(i, vventNum.secondcompartment) = 0
                        If csv.num(i, 0) > 6 Then ' New format that allows more than one VVENT per compartment pair
                            vvent.SetVent(csv.num(i, vventNum.firstcompartment) - 1, csv.num(i, vventNum.secondcompartment) - 1,
                                        csv.num(i, vventNum.area), csv.num(i, vventNum.shape))
                            If InStr(OpenTypes, csv.str(i, vventNum.openingtype), CompareMethod.Text) > 0 Then
                                vvent.OpenType = InStr(OpenTypes, csv.str(i, vventNum.openingtype), CompareMethod.Text) / 4
                                If vvent.OpenType = Vent.OpenbyTime Then
                                    vvent.InitialOpeningTime = csv.num(i, vventNum.openinitialtime)
                                    vvent.InitialOpening = csv.num(i, vventNum.openinitialfraction)
                                    vvent.FinalOpeningTime = csv.num(i, vventNum.openfinaltime)
                                    vvent.FinalOpening = csv.num(i, vventNum.openfinalfraction)
                                Else
                                    vvent.OpenValue = csv.num(i, vventNum.opencriterion)
                                    vvent.Target = csv.str(i, vventNum.opentarget)
                                    vvent.InitialOpening = csv.num(i, vventNum.openinitialfraction)
                                    vvent.FinalOpening = csv.num(i, vventNum.openfinalfraction)
                                End If
                                vvent.OffsetX = csv.num(i, vventNum.xoffset)
                                vvent.OffsetY = csv.num(i, vventNum.yoffset)
                            Else
                                vvent.SetVent(csv.num(i, vventNum.firstcompartment) - 1, csv.num(i, vventNum.secondcompartment) - 1,
                                            csv.num(i, vventNum.area), csv.num(i, vventNum.shape))
                                vvent.OpenType = Vent.OpenbyTime
                                vvent.InitialOpeningTime = 0    ' This is the default; it may be changed by an EVENT specification
                                vvent.InitialOpening = csv.num(i, vventNum.intialfraction)
                                vvent.FinalOpeningTime = 0
                                vvent.FinalOpening = csv.num(i, vventNum.intialfraction)
                            End If
                            vvent.Changed = False
                        Else ' Old format that does not include vent number and thus only one per compartment pair
                            vvent.SetVent(csv.num(i, vventNum.firstcompartment) - 1, csv.num(i, vventNum.secondcompartment) - 1,
                                        csv.num(i, vventNum.area - 1), csv.num(i, vventNum.shape - 1))
                            vvent.InitialOpeningTime = 0    ' This is the default; it may be changed by an EVENT specification
                            vvent.InitialOpening = csv.num(i, vventNum.intialfraction - 1)
                            vvent.FinalOpeningTime = 0
                            vvent.FinalOpening = csv.num(i, vventNum.intialfraction - 1)
                            vvent.OffsetX = -1
                            vvent.OffsetY = -1
                            vvent.Changed = True
                        End If
                        myVVents.Add(vvent)
                    Case "WIND"
                        myEnvironment.ExtWindSpeed = csv.num(i, windNum.velocity)
                        myEnvironment.ExtScaleHeight = csv.num(i, windNum.refHeight)
                        myEnvironment.ExtPowerLawCoefficient = csv.num(i, windNum.expLapseRate)
                        myEnvironment.Changed = False
                End Select
            Else
                If HeaderComment(csv.str(i, 1)) Then
                    dataFileHeader.Add(csv.strrow(i))
                ElseIf DropComment(csv.strrow(i)) Then
                    'drop the comment
                Else
                    dataFileComments.Add(csv.strrow(i))
                End If
            End If
            i += 1
        Loop
        ' do EVENT Keyword. All vents need to be defined before we look at open/close or filter events
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                If csv.str(i, CFASTlnNum.keyWord).Trim = "EVENT" Then
                    If csv.str(i, eventNum.ventType).Trim = "H" Then
                        If csv.num(i, eventNum.firstCompartment) > myCompartments.Count Then csv.num(i, eventNum.firstCompartment) = 0
                        If csv.num(i, eventNum.secondCompartment) > myCompartments.Count Then csv.num(i, eventNum.secondCompartment) = 0
                        Dim index As Integer = myHVents.GetIndex(csv.num(i, eventNum.firstCompartment) - 1,
                            csv.num(i, eventNum.secondCompartment) - 1, csv.num(i, eventNum.ventNumber))
                        If index > -1 Then
                            Dim aVent As Vent = myHVents.Item(index)
                            aVent.InitialOpeningTime = csv.num(i, eventNum.time)
                            aVent.FinalOpeningTime = csv.num(i, eventNum.time) + csv.num(i, eventNum.decaytime)
                            aVent.FinalOpening = csv.num(i, eventNum.finalFraction)
                            aVent.Changed = False
                        Else
                            'error handling vent doesn't exist
                            myErrors.Add("Keyword EVENT Hvent " + csv.str(i, eventNum.ventNumber) + " between compartments " + csv.str(i, eventNum.firstCompartment) + " and " + csv.str(i, eventNum.secondCompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    ElseIf csv.str(i, eventNum.ventType).Trim = "V" Then
                        If csv.num(i, eventNum.firstCompartment) > myCompartments.Count Then csv.num(i, eventNum.firstCompartment) = 0
                        If csv.num(i, eventNum.secondCompartment) > myCompartments.Count Then csv.num(i, eventNum.secondCompartment) = 0
                        Dim index As Integer = myVVents.GetIndex(csv.num(i, eventNum.firstCompartment) - 1,
                            csv.num(i, eventNum.secondCompartment) - 1, csv.num(i, eventNum.ventNumber))
                        If index > -1 Then
                            Dim aVent As Vent = myVVents.Item(index)
                            aVent.InitialOpeningTime = csv.num(i, eventNum.time)
                            aVent.FinalOpeningTime = csv.num(i, eventNum.time) + csv.num(i, eventNum.decaytime)
                            aVent.FinalOpening = csv.num(i, eventNum.finalFraction)
                            aVent.Changed = False
                        Else
                            'error handling vent doesn't exist
                            myErrors.Add("Keyword EVENT Vvent " + csv.str(i, eventNum.ventNumber) + " between compartments " + csv.str(i, eventNum.firstCompartment) + " and " + csv.str(i, eventNum.secondCompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    ElseIf csv.str(i, eventNum.ventType).Trim = "M" Then
                        If csv.num(i, eventNum.firstCompartment) > myCompartments.Count Then csv.num(i, eventNum.firstCompartment) = 0
                        If csv.num(i, eventNum.secondCompartment) > myCompartments.Count Then csv.num(i, eventNum.secondCompartment) = 0
                        Dim index As Integer = myMVents.GetIndex(csv.num(i, eventNum.firstCompartment) - 1,
                            csv.num(i, eventNum.secondCompartment) - 1, csv.num(i, eventNum.ventNumber))
                        If index > -1 Then
                            Dim aVent As Vent = myMVents.Item(index)
                            aVent.InitialOpeningTime = csv.num(i, eventNum.time)
                            aVent.FinalOpeningTime = csv.num(i, eventNum.time) + csv.num(i, eventNum.decaytime)
                            aVent.FinalOpening = csv.num(i, eventNum.finalFraction)
                            aVent.Changed = False
                        Else
                            'error handling vent doesn't exist
                            myErrors.Add("Keyword EVENT Mvent " + csv.str(i, eventNum.ventNumber) + " between compartments " + csv.str(i, eventNum.firstCompartment) + " and " + csv.str(i, eventNum.secondCompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    ElseIf csv.str(i, eventNum.ventType).Trim = "F" Then
                        If csv.num(i, eventNum.firstCompartment) > myCompartments.Count Then csv.num(i, eventNum.firstCompartment) = 0
                        If csv.num(i, eventNum.secondCompartment) > myCompartments.Count Then csv.num(i, eventNum.secondCompartment) = 0
                        Dim index As Integer = myMVents.GetIndex(csv.num(i, eventNum.firstCompartment) - 1,
                            csv.num(i, eventNum.secondCompartment) - 1, csv.num(i, eventNum.ventNumber))
                        If index > -1 Then
                            Dim aVent As Vent = myMVents.Item(index)
                            aVent.FilterTime = csv.num(i, eventNum.time)
                            aVent.FilterEfficiency = csv.num(i, eventNum.finalFraction) * 100.0
                            aVent.Changed = False
                        Else
                            'error handling vent doesn't exist
                            myErrors.Add("Keyword EVENT Mvent Filter " + csv.str(i, eventNum.ventNumber) + " between compartments " + csv.str(i, eventNum.firstCompartment) + " and " + csv.str(i, eventNum.secondCompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    Else
                        'error handling wrong vent types
                        myErrors.Add("Keyword EVENT vent type " + csv.str(i, eventNum.ventType) + " is not recognized", ErrorMessages.TypeError)
                    End If
                End If
            End If
            i += 1
        Loop
        ' do RAMP Keyword
        i = 1
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                If csv.str(i, CFASTlnNum.keyWord).Trim = "RAMP" Then
                    If csv.num(i, rampNum.firstcompartment) > myCompartments.Count Then _
                        csv.num(i, rampNum.firstcompartment) = 0
                    If csv.num(i, rampNum.secondcompartment) > myCompartments.Count Then _
                        csv.num(i, rampNum.secondcompartment) = 0
                    Dim aVentIndex As Integer
                    If csv.str(i, rampNum.ventType) = "H" Then
                        aVentIndex = myHVents.GetIndex(csv.num(i, rampNum.firstcompartment) - 1, csv.num(i, rampNum.secondcompartment) - 1, csv.num(i, rampNum.ventnumber))
                        If aVentIndex > -1 And aVentIndex < myHVents.Count Then
                            Dim avent As Vent = myHVents(aVentIndex)
                            Dim VentTime(csv.num(i, rampNum.numpoints)), VentFraction(csv.num(i, rampNum.numpoints)) As Single
                            For j = 1 To csv.num(i, rampNum.numpoints)
                                VentTime(j) = csv.num(i, 2 * j + rampNum.numpoints - 1)
                                VentFraction(j) = csv.num(i, 2 * j + rampNum.numpoints)
                            Next
                            avent.SetRamp(VentTime, VentFraction)
                            avent.Changed = False
                        Else
                            myErrors.Add("Keyword RAMP Hvent " + csv.str(i, rampNum.ventnumber) + " between compartments " + csv.str(i, rampNum.firstcompartment) + " and " + csv.str(i, rampNum.secondcompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    ElseIf csv.str(i, rampNum.ventType) = "V" Then
                        aVentIndex = myVVents.GetIndex(csv.num(i, rampNum.firstcompartment) - 1, csv.num(i, rampNum.secondcompartment) - 1, csv.num(i, rampNum.ventnumber))
                        If aVentIndex > -1 And aVentIndex < myVVents.Count Then
                            Dim avent As Vent = myVVents(aVentIndex)
                            Dim VentTime(csv.num(i, rampNum.numpoints)), VentFraction(csv.num(i, rampNum.numpoints)) As Single
                            For j = 1 To csv.num(i, rampNum.numpoints)
                                VentTime(j) = csv.num(i, 2 * j + rampNum.numpoints - 1)
                                VentFraction(j) = csv.num(i, 2 * j + rampNum.numpoints)
                            Next
                            avent.SetRamp(VentTime, VentFraction)
                            avent.Changed = False
                        Else
                            myErrors.Add("Keyword RAMP Vvent " + csv.str(i, rampNum.ventnumber) + " between compartments " + csv.str(i, rampNum.firstcompartment) + " and " + csv.str(i, rampNum.secondcompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    ElseIf csv.str(i, rampNum.ventType) = "M" Then
                        aVentIndex = myMVents.GetIndex(csv.num(i, rampNum.firstcompartment) - 1, csv.num(i, rampNum.secondcompartment) - 1, csv.num(i, rampNum.ventnumber))
                        If aVentIndex > -1 And aVentIndex < myMVents.Count Then
                            Dim avent As Vent = myMVents(aVentIndex)
                            Dim VentTime(csv.num(i, rampNum.numpoints)), VentFraction(csv.num(i, rampNum.numpoints)) As Single
                            For j = 1 To csv.num(i, rampNum.numpoints)
                                VentTime(j) = csv.num(i, 2 * j + rampNum.numpoints - 1)
                                VentFraction(j) = csv.num(i, 2 * j + rampNum.numpoints)
                            Next
                            avent.SetRamp(VentTime, VentFraction)
                            avent.Changed = False
                        Else
                            myErrors.Add("Keyword RAMP Mvent " + csv.str(i, rampNum.ventnumber) + " between compartments " + csv.str(i, rampNum.firstcompartment) + " and " + csv.str(i, rampNum.secondcompartment) + " does not exist", ErrorMessages.TypeError)
                        End If
                    Else
                        'error handling wrong vent types
                        myErrors.Add("Keyword RAMP vent type " + csv.str(i, rampNum.ventType) + " is not recognized", ErrorMessages.TypeError)
                    End If
                End If
            End If
            i += 1
        Loop

    End Sub
    Public Sub ReadInputFileNML(ByVal Filename As String)
        'Filename is assumed to be the complete path plus name and extenstion
        Dim NMList As NameListFile
        NMList = New NameListFile(Filename)
        ReadInputFileNMLHead(NMList)
        ReadInputFileNMLTime(NMList)
        ReadInputFileNMLInit(NMList)
        ReadInputFileNMLMisc(NMList)
        ReadInputFileNMLMatl(NMList)
        ReadInputFileNMLRamp(NMList)
        ReadInputFileNMLComp(NMList)
        ReadInputFileNMLDevc(NMList)
        ReadInputFileNMLFire(NMList)
        ReadInputFileNMLVent(NMList)
        ReadInputFileNMLConn(NMList)
        ReadInputFileNMLISOF(NMList)
        ReadInputFileNMLSLCF(NMList)
    End Sub
    Public Sub ReadInputFileNMLHead(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim ver As Integer
        Dim title As String

        ver = 7300
        title = ""
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "HEAD") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "VERSION") Then
                        ver = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TITLE") Then
                        title = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In HEAD name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        myEnvironment.Title = title
        myEnvironment.Version = ver
        myEnvironment.Changed = False
    End Sub
    Public Sub ReadInputFileNMLTime(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim print, sim, smoke, ss As Integer

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
                        myErrors.Add("In TIME name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        myEnvironment.OutputInterval = print
        myEnvironment.SimulationTime = sim
        myEnvironment.SmokeviewInterval = smoke
        myEnvironment.SpreadsheetInterval = ss
        myEnvironment.Changed = False
    End Sub
    Public Sub ReadInputFileNMLInit(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim pressure, rh, intemp, extemp As Single

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
                        myErrors.Add("In INIT name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next
        myEnvironment.IntAmbTemperature = intemp + 273.15
        myEnvironment.ExtAmbTemperature = extemp + 273.15
        myEnvironment.IntAmbPressure = pressure
        myEnvironment.ExtAmbPressure = pressure
        myEnvironment.IntAmbRH = rh
        myEnvironment.Changed = False
    End Sub
    Public Sub ReadInputFileNMLMisc(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim adiabatic As Boolean
        Dim maxts, loxyl As Single

        adiabatic = False
        maxts = 2
        loxyl = 0.15
        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "MISC") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ADIABATIC") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            adiabatic = False
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            adiabatic = True
                        Else
                            myErrors.Add("In MISC name list for ADIABATIC " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MAX_TIME_STEP") Then
                        maxts = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "LOWER_OXYGEN_LIMIT") Then
                        loxyl = NMList.ForNMListVarGetNum(i, j, 1)
                    Else
                        myErrors.Add("In MISC name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
            End If
        Next

        myEnvironment.AdiabaticWalls = adiabatic
        myEnvironment.MaximumTimeStep = maxts
        myEnvironment.LowerOxygenLimit = loxyl
        myEnvironment.Changed = False
    End Sub
    Public Sub ReadInputFileNMLMatl(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim conduct, dens, emiss, spech, thick As Single
        Dim id, matl As String
        Dim valid As Boolean
        Dim hcl() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
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
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "CONDUCTIVITY") Then
                        conduct = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "DENSITY") Then
                        dens = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "EMISSIVITY") Then
                        emiss = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SPECIFIC_HEAT") Then
                        spech = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "THICKNESS") Then
                        thick = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "MATERIAL") Then
                        matl = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In MATL name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
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
                    If myThermalProperties.Count > 0 Then
                        iProp = myThermalProperties.GetIndex(id)
                        If iProp >= 0 Then
                            ' We already have a thermal property with this name.  If it's totally identical, then it's already been added.  If not, they are trying to add a second one with the same name.  We'll allow it but error checking with flag it as an issue.
                            Dim aProperty As New ThermalProperty
                            aProperty = myThermalProperties.Item(iProp)
                            If aProperty.Name = matl And aProperty.Conductivity = conduct And aProperty.SpecificHeat = spech And aProperty.Density = dens _
                                And aProperty.Thickness = thick And aProperty.Emissivity = emiss Then
                                'logic needs to be reworked
                            Else
                                myThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                                myThermalProperties.Item(myThermalProperties.Count - 1).SetHCl(hcl)
                                myThermalProperties.Item(myThermalProperties.Count - 1).Changed = False
                            End If
                        Else
                            myThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                            myThermalProperties.Item(myThermalProperties.Count - 1).SetHCl(hcl)
                            myThermalProperties.Item(myThermalProperties.Count - 1).Changed = False
                        End If
                    Else
                        myThermalProperties.Add(New ThermalProperty(id, matl, conduct, spech, dens, thick, emiss))
                        myThermalProperties.Item(myThermalProperties.Count - 1).SetHCl(hcl)
                        myThermalProperties.Item(myThermalProperties.Count - 1).Changed = False
                    End If
                End If
            End If
        Next
        Dim test As Integer = myThermalProperties.Count

    End Sub
    Public Sub ReadInputFileNMLRamp(ByVal NMList As NameListFile)
        Const maxnum As Integer = 1000
        Dim i, j, k As Integer
        Dim x(1), f(1) As Single
        Dim max As Integer
        Dim id, type As String
        Dim isT, def1, def2, deff As Boolean

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "RAMP") Then
                type = ""
                id = ""
                def1 = False
                def2 = False
                deff = False
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TYPE") Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "F" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= maxnum Then
                            ReDim f(max - 1)
                            For k = 1 To max
                                f(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In RAMP name list for F input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
                        End If
                        deff = True
                    ElseIf NMList.ForNMListGetVar(i, j) = "T" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= maxnum Then
                            ReDim x(max - 1)
                            For k = 1 To max
                                x(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In RAMP name list for T input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
                        End If
                        isT = True
                        If def1 Then
                            def2 = True
                        Else
                            def1 = True
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "Z" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= maxnum Then
                            ReDim x(max - 1)
                            For k = 1 To max
                                x(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In RAMP name list for Z input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
                        End If
                        isT = False
                        If def1 Then
                            def2 = True
                        Else
                            def1 = True
                        End If
                    Else
                        myErrors.Add("In RAMP name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                If def2 Then
                    myErrors.Add("In RAMP name list " + id + " has both t and z defined. Only one can be defined", ErrorMessages.TypeFatal)
                ElseIf def1 And deff Then
                    If Ramp.ValidRamp(id, type, x, f) Then
                        If type = "HRR" Then
                            For k = 0 To f.GetUpperBound(0)
                                f(k) = f(k) * 1000.0
                            Next
                        End If
                        myRamps.Add(New Ramp(id, type, x, f, isT))
                    Else
                        myErrors.Add("In RAMP name list " + id + " id not a valid ramp definition", ErrorMessages.TypeFatal)
                    End If
                Else
                    myErrors.Add("In RAMP name list " + id + " id not a valid ramp definition", ErrorMessages.TypeFatal)
                End If
            End If
        Next
        Dim test As Integer = myRamps.Count

    End Sub
    Public Sub ReadInputFileNMLComp(ByVal NMList As NameListFile)
        Dim i, j, k, max As Integer
        Dim ceilid, wallid, floorid, rampid, id As String
        Dim depth, height, width As Single
        Dim shaft, hall, valid As Boolean
        Dim grid(3) As Integer
        Dim origin(3) As Single

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "COMP") Then
                hall = False
                shaft = False
                depth = -1
                width = -1
                height = -1
                id = ""
                ceilid = ""
                floorid = ""
                wallid = ""
                rampid = ""
                For k = 0 To 2
                    grid(k) = 50
                    origin(k) = 0.0
                Next
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CEILING_MATL_ID") Then
                        ceilid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FLOOR_MATL_ID") Then
                        floorid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "WALL_MATL_ID") Then
                        wallid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "ROOM_AREA_RAMP") Then
                        rampid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "HALL") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            hall = True
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            hall = False
                        Else
                            myErrors.Add("In COMP name list for HALL " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SHAFT") Then
                        If NMList.ForNMListVarGetStr(i, j, 1) = ".TRUE." Then
                            shaft = True
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = ".FALSE." Then
                            shaft = False
                        Else
                            myErrors.Add("In COMP name list for SHAFT " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value. Must be either .TRUE. or .FALSE.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEPTH" Then
                        depth = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEIGHT" Then
                        height = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "WIDTH" Then
                        width = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "GRID" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= 3 Then
                            For k = 1 To max
                                grid(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In COMP name list for GRID input must be 1 to 3 positive integers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "ORIGIN" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 3 And max <= 3 Then
                            For k = 1 To max
                                origin(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In COMP name list for ORIGIN input must be 3 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    Else
                        myErrors.Add("In COMP name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then valid = False
                If depth <= 0 Then valid = False
                If width <= 0 Then valid = False
                If height <= 0 Then valid = False
                If valid Then
                    Dim compa As New Compartment
                    myCompartments.Add(compa)
                    compa.Name = id
                    compa.SetSize(width, depth, height)
                    If myThermalProperties.GetIndex(ceilid) < 0 And ceilid <> "OFF" Then
                        ceilid = "OFF"
                        myErrors.Add("In COMP name list " + id + " CEILING_MATL_ID " + ceilid + " is not valid switching ceiling to OFF", ErrorMessages.TypeWarning)
                    End If
                    If myThermalProperties.GetIndex(wallid) < 0 And wallid <> "OFF" Then
                        wallid = "OFF"
                        myErrors.Add("In COMP name list " + id + " WALL_MATL_ID " + wallid + " is not valid switching wall to OFF", ErrorMessages.TypeWarning)
                    End If
                    If myThermalProperties.GetIndex(floorid) < 0 And floorid <> "OFF" Then
                        floorid = "OFF"
                        myErrors.Add("In COMP name list " + id + "FLOOR_MATL_ID " + floorid + " is not valid switching floor to OFF", ErrorMessages.TypeWarning)
                    End If
                    compa.SetPosition(origin(LocationNum.x), origin(LocationNum.y), origin(LocationNum.z))
                    compa.SetMaterial(ceilid, wallid, floorid)
                    If rampid <> "" Then
                        If myRamps.GetRampIndex(rampid) >= 0 Then
                            If myRamps.Item(myRamps.GetRampIndex(rampid)).Type = Ramp.TypeArea Then
                                compa.AreaRampID = rampid
                            Else
                                myErrors.Add("In COMP name list " + id + " ROOM_AREA_RAMP " + rampid + " is not type AREA", ErrorMessages.TypeWarning)
                            End If
                        Else
                            myErrors.Add("In COMP name list " + id + " ROOM_AREA_RAMP " + rampid + " is not a valid ramp id", ErrorMessages.TypeWarning)
                        End If
                    End If
                Else
                    myErrors.Add("In COMP name list " + id + " is not fully defined", ErrorMessages.TypeWarning)
                End If
            End If
        Next
        Dim test As Integer = myCompartments.Count

    End Sub
    Public Sub ReadInputFileNMLDevc(ByVal NMList As NameListFile)
        Dim i, j, k, max As Integer
        Dim compid, matlid, id, type As String
        Dim tempdepth, rti, setp, sprayd As Single
        Dim loc(3), norm(3) As Single
        Dim valid, lvalid As Boolean
        Dim aTempOffset As Single = 273.15

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "DEVC") Then
                tempdepth = 0.5
                rti = 130
                setp = -1
                id = ""
                type = ""
                compid = ""
                matlid = ""
                For k = 0 To 2
                    loc(k) = -1
                    norm(k) = 0
                Next
                norm(2) = 1
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
                            If setp <= 0 Then setp = 74.0
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "HEAT_DETECTOR" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                            If setp <= 0 Then setp = 57.0
                        ElseIf NMList.ForNMListVarGetStr(i, j, 1) = "SMOKE_DETECTOR" Then
                            type = NMList.ForNMListVarGetStr(i, j, 1)
                            If setp <= 0 Then setp = 23.93
                        Else
                            myErrors.Add("In DEVC name list for TYPE " + NMList.ForNMListVarGetStr(i, j, 1) + " is not a valid value.", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "TEMPERATURE_DEPTH" Then
                        tempdepth = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "RTI" Then
                        rti = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINT" Then
                        setp = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SPRAY_DENSITY" Then
                        sprayd = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "LOCATION" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 3 And max <= 3 Then
                            For k = 1 To max
                                loc(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC name list for LOCATION input must be 3 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "NORMAL" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 And max <= 3 Then
                            For k = 1 To max
                                norm(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In DEVC name list for NORMAL input must be 3 numbers", ErrorMessages.TypeFatal)
                        End If
                    Else
                        myErrors.Add("In DEVC name list " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then
                    valid = False
                    myErrors.Add("DEVC name list is not a valid DEVC because it has no ID", ErrorMessages.TypeFatal)
                End If
                If type = "" Then
                    valid = False
                    myErrors.Add("DEVC name list " + id + " is not a valid DEVC because it has no type", ErrorMessages.TypeFatal)
                End If
                If compid = "" Then
                    valid = False
                    myErrors.Add("DEVC name list " + id + " is not a valid DEVC because it has no COMP_ID", ErrorMessages.TypeFatal)
                ElseIf myCompartments.GetCompIndex(compid) < 0 Then
                    valid = False
                    myErrors.Add("DEVC name list " + id + " is not a valid DEVC because COMP_ID " + compid + " does not refernce a valid compartment", ErrorMessages.TypeFatal)
                End If
                lvalid = True
                For k = 0 To 2
                    If loc(k) <= 0 Then
                        lvalid = False
                    End If
                Next
                If Not lvalid Then
                    myErrors.Add("DEVC name list " + id + " is not a valid DEVC because at least one of the components of the location has not been set", ErrorMessages.TypeFatal)
                End If
                If valid Then
                    Dim aDetect As New Target
                    If type = "PLATE" Or type = "CYLINDER" Then
                        aDetect.Type = Target.TypeTarget
                        aDetect.SetPosition(loc(LocationNum.x), loc(LocationNum.y), loc(LocationNum.z), norm(LocationNum.x), norm(LocationNum.y), norm(LocationNum.z))
                        Dim atype As Integer
                        If type = "CYLINDER" Then
                            atype = Target.Cylindrical
                        Else ' PDE
                            atype = Target.ThermallyThick
                        End If
                        aDetect.SetTarget(myCompartments.GetCompIndex(compid), matlid, atype)
                        aDetect.InternalLocation = tempdepth
                        aDetect.Name = id
                        aDetect.Changed = False
                        myTargets.Add(aDetect)
                    Else
                        aDetect.Type = Target.TypeDetector
                        aDetect.Name = id
                        aDetect.Compartment = myCompartments.GetCompIndex(compid)
                        If type = "HEAT_DETECTOR" Then
                            aDetect.DetectorType = Target.TypeHeatDetector
                            aDetect.RTI = rti
                            aDetect.ActivationTemperature = setp + aTempOffset
                            aDetect.SprayDensity = 0.0
                        ElseIf type = "SMOKE_DETECTOR" Then
                            aDetect.DetectorType = Target.TypeSmokeDetector
                            aDetect.ActivationObscuration = setp
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
                        myErrors.Add("In DEVC name list " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next
        Dim test As Integer = myTargets.Count

    End Sub
    Public Sub ReadInputFileNMLFire(ByVal NMList As NameListFile)
        Dim i, j, k, max As Integer
        Dim arearamp, compid, coramp, hclramp, hrrramp, id, sootramp, traceramp, devcid, hcnramp, ignitcrit As String
        Dim area, carbon, chlorine, coyield, hclyield, hcnyield, hoc, hrr, hydrogen, nitrogen, oxygen, radfrac As Single
        Dim setp, sootyield, traceyield As Single
        Dim loc(3) As Single
        Dim valid, locval As Boolean

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "FIRE") Then
                area = 0.3
                carbon = 1
                chlorine = 0
                coyield = -1
                hclyield = -1
                hcnyield = -1
                hoc = 50000
                hrr = -1
                hydrogen = 4
                ignitcrit = "TIME"
                nitrogen = 0
                oxygen = 0
                radfrac = 0.35
                setp = 0
                sootyield = -1
                traceyield = -1
                id = ""
                arearamp = ""
                coramp = ""
                hclramp = ""
                hcnramp = ""
                hrrramp = ""
                sootramp = ""
                traceramp = ""
                devcid = ""
                compid = ""
                For k = 0 To 2
                    loc(k) = -1
                Next
                For j = 1 To NMList.ForNMListNumVar(i)
                    If NMList.ForNMListGetVar(i, j) = "ID" Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREA_RAMP_ID" Then
                        arearamp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "COMP_ID" Then
                        compid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CO_YIELD_RAMP_ID" Then
                        coramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEVC_ID" Then
                        devcid = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HCL_YIELD_RAMP_ID" Then
                        hclramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HCN_YIELD_RAMP_ID" Then
                        hcnramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HRR_RAMP_ID" Then
                        hrrramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "IGNITION_CRITERION" Then
                        ignitcrit = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SOOT_YIELD_RAMP_ID" Then
                        sootramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TRACE_YIELD_RAMP_ID" Then
                        traceramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREA" Then
                        area = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CARBON" Then
                        carbon = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CHLORINE" Then
                        chlorine = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "CO_YIELD" Then
                        coyield = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HCL_YIELD" Then
                        hclyield = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HCN_YIELD" Then
                        hcnyield = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEAT_OF_COMBUSTION" Then
                        hoc = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HRR" Then
                        hrr = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "HYDROGEN" Then
                        hydrogen = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "NITROGEN" Then
                        nitrogen = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "OXYGEN" Then
                        oxygen = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "RADIATIVE_FRACTION" Then
                        radfrac = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SETPOINT" Then
                        setp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "SOOT_YIELD" Then
                        sootyield = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TRACE_YIELD" Then
                        traceyield = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "LOCATION" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 3 And max <= 3 Then
                            For k = 1 To max
                                loc(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In FIRE name list for LOCATION input must be 3 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    Else
                        myErrors.Add("In FIRE name list " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then
                    myErrors.Add("In FIRE name list ID parameter must be set", ErrorMessages.TypeFatal)
                    valid = False
                End If
                If hrr >= 0 Then
                    If myRamps.GetRampIndex(hrrramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both HRR And HRR_RAMP_ID are used CFAST will use HRR_RAMP_ID " + hrrramp, ErrorMessages.TypeNothing)
                    End If
                ElseIf myRamps.GetRampIndex(hrrramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list either HRR Or HRR_RAMP_ID parameters must be set", ErrorMessages.TypeFatal)
                End If
                If coyield >= 0 Then
                    If myRamps.GetRampIndex(coramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both CO_YIELD And CO_YEILD_RAMP_ID are used CFAST will use CO_YEILD_RAMP_ID " + coramp, ErrorMessages.TypeNothing)
                    End If
                End If
                If coramp <> "" And myRamps.GetRampIndex(coramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " CO_YEILD_RAMP " + coramp + " does Not reference a valid ramp", ErrorMessages.TypeFatal)
                End If
                If hclyield >= 0 Then
                    If myRamps.GetRampIndex(hclramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both HCL_YIELD And HCL_YEILD_RAMP_ID are used CFAST will use HCL_YEILD_RAMP_ID " + hclramp, ErrorMessages.TypeNothing)
                    End If
                End If
                If hclramp <> "" And myRamps.GetRampIndex(hclramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " HCL_YEILD_RAMP " + hclramp + " does Not reference a valid ramp", ErrorMessages.TypeFatal)
                End If
                If hcnyield >= 0 Then
                    If myRamps.GetRampIndex(hcnramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both HCN_YIELD And HCN_YEILD_RAMP_ID are used CFAST will use HCN_YEILD_RAMP_ID " + hcnramp, ErrorMessages.TypeNothing)
                    End If
                End If
                If hcnramp <> "" And myRamps.GetRampIndex(hcnramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " HCN_YEILD_RAMP " + hcnramp + " does Not reference a valid ramp", ErrorMessages.TypeFatal)
                End If
                If sootyield >= 0 Then
                    If myRamps.GetRampIndex(sootramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both SOOT_YIELD And SOOT_YEILD_RAMP_ID are used CFAST will use SOOT_YEILD_RAMP_ID " + hclramp, ErrorMessages.TypeNothing)
                    End If
                End If
                If sootramp <> "" And myRamps.GetRampIndex(sootramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " SOOT_YEILD_RAMP " + sootramp + " does Not reference a valid ramp", ErrorMessages.TypeFatal)
                End If
                If traceyield >= 0 Then
                    If myRamps.GetRampIndex(traceramp) >= 0 Then
                        myErrors.Add("In FIRE name list " + id + " both TRACE_YIELD And TRACE_YEILD_RAMP_ID are used CFAST will use TRACE_YEILD_RAMP_ID " + traceramp, ErrorMessages.TypeNothing)
                    End If
                End If
                If traceramp <> "" And myRamps.GetRampIndex(traceramp) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " TRACE_YEILD_RAMP " + traceramp + " does Not reference a valid ramp", ErrorMessages.TypeFatal)
                End If
                If compid = "" Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " COMP_ID parameter must be set", ErrorMessages.TypeFatal)
                ElseIf myCompartments.GetCompIndex(compid) < 0 Then
                    valid = False
                    myErrors.Add("In FIRE name list " + id + " COMP_ID " + compid + " does Not reference a valid compartment", ErrorMessages.TypeFatal)
                End If
                If ignitcrit <> "" Then
                    If ignitcrit <> "TIME" And ignitcrit <> "TEMPERATURE" And ignitcrit <> "FLUX" Then
                        valid = False
                        myErrors.Add("In FIRE name list " + id + " IGNITION_CRITERION parameter must be TIME, TEMPERATURE, Or FLUX", ErrorMessages.TypeFatal)
                    End If
                End If
                If ignitcrit = "TEMPERATURE" Or ignitcrit = "FLUX" Then
                    If devcid = "" Then
                        valid = False
                        myErrors.Add("In FIRE name list " + id + " DEVC_ID must be a valid target", ErrorMessages.TypeFatal)
                    ElseIf myTargets.GetIndex(devcid) < 0 Then
                        valid = False
                        myErrors.Add("In FIRE name list " + id + " DEVC_ID " + devcid + " Is Not a valid target", ErrorMessages.TypeFatal)
                    End If
                End If
                locval = True
                For k = 0 To 2
                    If loc(k) < 0 Then locval = False
                Next
                If Not locval Then
                    valid = False
                    myErrors.Add("In FIRE name list LOCATIOM parameter must be set with a real triplet >= 0", ErrorMessages.TypeFatal)
                End If
                If valid Then
                    Dim aFireObject As New Fire()
                    Dim aThermalProperty As New ThermalProperty()
                    aFireObject.Name = id
                    aFireObject.ChemicalFormula(formula.C) = carbon
                    aFireObject.ChemicalFormula(formula.H) = hydrogen
                    aFireObject.ChemicalFormula(formula.O) = oxygen
                    aFireObject.ChemicalFormula(formula.N) = nitrogen
                    aFireObject.ChemicalFormula(formula.Cl) = chlorine
                    aFireObject.HeatofCombustion = hoc * 1000.0
                    aFireObject.RadiativeFraction = radfrac
                    aFireObject.SetPosition(myCompartments.GetCompIndex(compid), loc(LocationNum.x), loc(LocationNum.y), loc(LocationNum.z))
                    If ignitcrit = "TIME" Then
                        aFireObject.IgnitionType = IgnitionCriteriaNum.time
                        aFireObject.IgnitionValue = setp
                    ElseIf ignitcrit = "TEMPERATURE" Then
                        aFireObject.IgnitionType = IgnitionCriteriaNum.temp
                        aFireObject.IgnitionValue = setp
                    ElseIf ignitcrit = "FLUX" Then
                        aFireObject.IgnitionValue = IgnitionCriteriaNum.flux
                        aFireObject.IgnitionValue = setp
                    End If
                    If devcid <> "" Then
                        aFireObject.Target = devcid
                    End If
                    If arearamp <> "" Then
                        aFireObject.AreaRampID = arearamp
                    End If
                    If coramp <> "" Then
                        aFireObject.CORampID = coramp
                    End If
                    If hclramp <> "" Then
                        aFireObject.HClRampID = hclramp
                    End If
                    If hcnramp <> "" Then
                        aFireObject.HCNRampID = hcnramp
                    End If
                    If hrrramp <> "" Then
                        aFireObject.HRRRampID = hrrramp
                    End If
                    If sootramp <> "" Then
                        aFireObject.SootRampID = sootramp
                    End If
                    If traceramp <> "" Then
                        aFireObject.TraceRampID = traceramp
                    End If
                    aFireObject.Changed = False
                    myFires.Add(aFireObject)
                Else
                    myErrors.Add("In FIRE name list " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next
        Dim test As Integer = myFires.Count
    End Sub
    Public Sub ReadInputFileNMLVent(ByVal NMList As NameListFile)
        Dim i, j, k, max As Integer
        Dim area, areas(2), bot, cutoffs(2), flow, heights(2), offset, offsets(2), top, width, setp, prefrac, postfrac, filttime, filteff As Single
        Dim compids(2), filtramp, openramp, face, orien(2), shape, type, id, crit, devcid As String
        Dim valid As Boolean
        Dim comp0dx, comp1dx As Integer

        For i = 1 To NMList.TotNMList
            If (NMList.GetNMListID(i) = "VENT") Then
                area = -1
                bot = -1
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
                For k = 0 To 1
                    areas(k) = -1
                    cutoffs(k) = -1
                    heights(k) = -1
                    offsets(k) = -1
                Next
                openramp = ""
                shape = ""
                top = -1
                type = ""
                width = -1
                For j = 1 To NMList.ForNMListNumVar(i)
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
                    ElseIf NMList.ForNMListGetVar(i, j) = "OPENING_RAMP_ID" Then
                        openramp = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "ORIENTATIONS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                orien(k - 1) = NMList.ForNMListVarGetStr(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for COMP_IDS there must be 2 compartment IDs", ErrorMessages.TypeFatal)
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
                            myErrors.Add("In VENT name list for COMP_IDS there must be 2 compartment IDs", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREA" Then
                        area = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "BOTTOM" Then
                        bot = NMList.ForNMListVarGetNum(i, j, 1)
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
                    ElseIf NMList.ForNMListGetVar(i, j) = "TOP" Then
                        top = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "WIDTH" Then
                        width = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "AREAS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                areas(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for AREAS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "CUTOFFS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                cutoffs(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for CUTOFFS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "HEIGHTS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                heights(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for HEIGHTS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "OFFSETS" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            For k = 1 To max
                                offsets(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for OFFSETS input must be 2 positive numbers", ErrorMessages.TypeFatal)
                        End If
                    Else
                        myErrors.Add("In VENT name list " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                valid = True
                If id = "" Then
                    valid = False
                    myErrors.Add("In VENT name list ID must be defined", ErrorMessages.TypeFatal)
                End If
                For k = 0 To 1
                    If myCompartments.GetCompIndex(compids(k)) < 0 And compids(k) <> "OUTSIDE" Then
                        valid = False
                        myErrors.Add("In VENT name list " + id + " does Not have two valid compartments", ErrorMessages.TypeFatal)
                    End If
                Next
                If type <> "CEILING" And type <> "FLOOR" And type <> "MECHANICAL" And type <> "WALL" Then
                    valid = False
                    myErrors.Add("In VENT name list " + id + " TYPE Is Not set to a valid value", ErrorMessages.TypeFatal)
                End If
                If valid Then
                    Dim aVent As New Vent
                    If type = "WALL" Then
                        aVent.VentType = Vent.TypeHVent
                        aVent.Name = id
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
                        aVent.SetVent(comp0dx, comp1dx, width, top, bot)
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
                            aVent.RampID = openramp
                            aVent.OpenType = Vent.OpenbyTime
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        myHVents.Add(aVent)
                    ElseIf type = "MECHANICAL" Then
                        aVent.VentType = Vent.TypeMVent
                        aVent.Name = id
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
                            aVent.RampID = openramp
                            aVent.OpenType = Vent.OpenbyTime
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        myMVents.Add(aVent)
                    ElseIf type = "CEILING" Or type = "FLOOR" Then
                        aVent.VentType = Vent.TypeVVent
                        aVent.Name = id
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
                            aVent.RampID = openramp
                            aVent.OpenType = Vent.OpenbyTime
                        ElseIf crit = "FLUX" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyFlux
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        ElseIf crit = "TEMPERATURE" Then
                            aVent.Target = devcid
                            aVent.OpenType = Vent.OpenbyTemperature
                            aVent.OpenValue = setp
                            aVent.InitialOpening = prefrac
                            aVent.FinalOpening = postfrac
                        End If
                        aVent.Changed = False
                        myVVents.Add(aVent)
                    End If
                Else
                    myErrors.Add("In VENT name list " + id + " Is Not fully defined", ErrorMessages.TypeFatal)
                End If
            End If
        Next
    End Sub
    Public Sub ReadInputFileNMLConn(ByVal NMList As NameListFile)
        Dim i, j, k, max, cFirst, cSecond As Integer
        Dim compid, compids(1), type As String
        Dim f(1) As Single

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
                            myErrors.Add("In VENT name list for COMP_IDS there must be at least 1 compartment ID", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "F" Then
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 1 Then
                            ReDim f(max - 1)
                            For k = 1 To max
                                f(k - 1) = NMList.ForNMListVarGetNum(i, j, k)
                            Next
                        Else
                            myErrors.Add("In VENT name list for F input must be 1 positive number", ErrorMessages.TypeFatal)
                        End If
                    ElseIf NMList.ForNMListGetVar(i, j) = "TYPE" Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In CONN name list " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
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
                        myHHeats.Add(aHeat)
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
                    myVHeats.Add(aHeat)
                End If
            End If
        Next

    End Sub
    Public Sub ReadInputFileNMLISOF(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim compid As String
        Dim value As Single
        Dim aTempOffset As Single = 273.15

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
                        myErrors.Add("In ISOF name list " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
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
                myVisuals.Add(aVisual)
            End If
        Next

    End Sub
    Public Sub ReadInputFileNMLSLCF(ByVal NMList As NameListFile)
        Dim i, j As Integer
        Dim compid, domain, plane As String
        Dim pos As Single

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
                        myErrors.Add("In SLCF name list " + NMList.ForNMListGetVar(i, j) + " Is Not a valid parameter", ErrorMessages.TypeFatal)
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
                myVisuals.Add(aVisual)
            End If
        Next

    End Sub
    Public Sub ReadThermalProperties(ByVal FileName As String, SomeThermalProperties As ThermalPropertiesCollection)
        'Simple read of only thermal properties from a file. 
        Dim csv As New CSVsheet(FileName)
        FindThermalProperties(csv, SomeThermalProperties)
        SomeThermalProperties.FileName = FileName
        SomeThermalProperties.FileChanged = False
    End Sub
    Public Sub FindThermalProperties(ByVal csv As CSVsheet, ByRef SomeThermalProperties As ThermalPropertiesCollection)
        Dim i As Integer
        ' do material properties so they are defined for compartments, fires, and targets
        Dim hcl() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
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
    Public Sub FindaThermalProperty(ByVal csv As CSVsheet, ByVal Material As String, ByRef aThermalPropery As ThermalProperty)
        ' look for a specific material in the current spreadsheet
        Dim SomeThermalProperties As New ThermalPropertiesCollection, aMaterial As New ThermalProperty, index As Integer
        FindThermalProperties(csv, SomeThermalProperties)
        If SomeThermalProperties.Count > 0 Then
            index = SomeThermalProperties.GetIndex(Material)
            If index >= 0 Then
                aThermalPropery = SomeThermalProperties.Item(index)
                Return
            End If
        End If
        aThermalPropery.ShortName = " "
    End Sub
    Public Sub ReadFireObjects(ByVal pathName As String)
        ' Simple routine that gets all *.o files in PathName and opens them
        Dim dir As String() = Directory.GetFiles(pathName, "*.o")
        Dim file As String
        For Each file In dir
            readFires(file, InsertDataType.ObjectFile)
        Next
    End Sub
    Public Sub readFires(ByVal Filename As String, FileType As Integer)
        Dim csv As New CSVsheet(Filename), i As Integer
        If csv.MaxRow > 0 Then
            FindFires(FileType, csv)
            If TempFires.Count > 0 Then
                For i = 1 To TempFires.Count
                    Dim aFire As New Fire
                    aFire = TempFires.Item(i - 1)
                    myFires.Add(aFire)
                Next
            End If
        End If
    End Sub
    Public Sub FindFires(ByVal FileType As Integer, ByVal csv As CSVsheet)
        'simple read of a fire object file
        Dim fireComments As New Collection
        Dim i, j, k, iStart, index As Integer
        Dim rowidx(csv.MaxRow) As Integer
        Dim rdx As Integer = 0
        Dim ChemicalCompound() As Single = {0.0, 1.0, 4.0, 0.0, 0.0, 0.0}
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

                                Dim hcl() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
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

                                Dim firedata(12, CInt(csv.num(iTime, 0) - 2)) As Single

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

                Dim firedata(12, CInt(csv.num(rowidx(1), 1) - 1)) As Single
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
    Public Sub ReadEmbeddedFire(ByVal csv As CSVsheet, ByVal iStart As Integer, ByRef aFire As Fire)
        Dim i, j, k, index As Integer
        i = iStart
        Do Until i > csv.MaxRow
            If Not SkipLine(csv.str(i, CFASTlnNum.keyWord)) Then
                Select Case csv.str(i, CFASTlnNum.keyWord).Trim
                    Case "FIRE"
                        Dim hcl() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
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
                        aFireObject.Name = csv.str(iFire, fireNum.name)
                        aFireObject.SetPosition(csv.num(iFire, fireNum.compartment) - 1, csv.num(iFire, fireNum.xPosition),
                            csv.num(iFire, fireNum.yPosition), csv.num(iFire, fireNum.zposition))
                        aFireObject.PlumeType = csv.num(iFire, fireNum.plumeType) - 1
                        If InStr(IgnitionTypes, csv.str(iFire, fireNum.ignType), CompareMethod.Text) > 0 Then
                            ' if it's the new format, ignition is just linked to an existing target
                            aFireObject.IgnitionType = InStr(IgnitionTypes, csv.str(iFire, fireNum.ignType), CompareMethod.Text) / 4
                            aFireObject.Target = csv.str(iFire, fireNum.ignTarget)
                        Else
                            ' if it's the old format, create a target just for the fire
                            aFireObject.IgnitionType = csv.num(iFire, fireNum.ignType) - 1
                            If aFireObject.IgnitionType <> Fire.FireIgnitionbyTime Then
                                Dim aTarget As New Target
                                aTarget.Type = Target.TypeTarget
                                aTarget.Name = "Ign_" + aFireObject.Name
                                aTarget.SetPosition(aFireObject.XPosition, aFireObject.YPosition, aFireObject.Height, csv.num(iFire, fireNum.xNormal), csv.num(iFire, fireNum.yNormal), csv.num(iFire, fireNum.zNormal))
                                aTarget.SetTarget(aFireObject.Compartment, csv.str(iChemie, chemieNum.Material), Target.ThermallyThick)
                                myTargets.Add(aTarget)
                                aFireObject.Target = aTarget.Name
                            End If
                        End If
                        aFireObject.IgnitionValue = csv.num(iFire, fireNum.ignCriterion)

                        Dim firedata(12, CInt(csv.num(iTime, 0) - 2)) As Single

                        For j = 0 To csv.num(iTime, 0) - 2
                            For k = 1 To NumFireCurves
                                firedata(FireCurveColumns(k), j) = csv.num(iTime + k - 1, j + 2)
                            Next
                            firedata(Fire.FireMdot, j) = firedata(Fire.FireHRR, j) / aFireObject.HeatofCombustion
                            firedata(Fire.FireHC, j) = aFireObject.ChemicalFormula(formula.H) * 1.00794 / (aFireObject.ChemicalFormula(formula.C) * 12.0107)
                            If aFireObject.ChemicalFormula(formula.N) <> 0 Then firedata(Fire.FireHCN, j) = (1.00794 + 12.0107 + 14.01) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.N)
                            If aFireObject.ChemicalFormula(formula.Cl) <> 0 Then firedata(Fire.FireHCl, j) = (1.00794 + 35.453) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.Cl)
                        Next
                        aFireObject.SetFireData(firedata)
                        aFireObject.Changed = False
                        aFire = aFireObject
                        Exit Do
                    Case Else
                        System.Windows.Forms.MessageBox.Show("Internal Error (User should Not see this). FIRE input line Not first line in fire specification.")
                        Exit Do
                End Select
            End If
            i += 1
        Loop

    End Sub
#End Region
#Region "Write Routines"
    Public Sub WriteInputFile(ByVal FileName As String)
        Dim csv As New CSVsheet
        Dim i As Integer = 1
        Dim j, k, l As Integer
        Dim x() As Single = {0}, y() As Single = {0}

        ' write header line
        csv.str(i, CFASTlnNum.keyWord) = "VERSN"
        csv.num(i, CFASTlnNum.version) = 7
        csv.str(i, CFASTlnNum.title) = myEnvironment.Title
        i += 1
        For j = 1 To dataFileHeader.Count
            csv.CSVrow(i) = """" + dataFileHeader.Item(j) + """"
            i += 1
        Next
        'comment for configuration section
        AddHeadertoOutput(csv, i, "Scenario Configuration")
        'Time line
        csv.str(i, CFASTlnNum.keyWord) = "TIMES"
        csv.num(i, timesNum.simTime) = myEnvironment.SimulationTime
        csv.num(i, timesNum.printInterval) = myEnvironment.OutputInterval
        csv.num(i, timesNum.smokeviewInterval) = myEnvironment.SmokeviewInterval
        csv.num(i, timesNum.spreadsheetInterval) = myEnvironment.SpreadsheetInterval
        i += 1
        'Exterior ambient conditions
        csv.str(i, CFASTlnNum.keyWord) = "EAMB"
        csv.num(i, ambNum.ambTemp) = myEnvironment.ExtAmbTemperature
        csv.num(i, ambNum.ambPress) = myEnvironment.ExtAmbPressure
        csv.num(i, ambNum.refHeight) = myEnvironment.ExtAmbElevation
        i += 1
        'Interior ambient conditions
        csv.str(i, CFASTlnNum.keyWord) = "TAMB"
        csv.num(i, ambNum.ambTemp) = myEnvironment.IntAmbTemperature
        csv.num(i, ambNum.ambPress) = myEnvironment.IntAmbPressure
        csv.num(i, ambNum.refHeight) = myEnvironment.IntAmbElevation
        csv.num(i, ambNum.relHumidity) = myEnvironment.IntAmbRH
        i += 1
        'adiabatic walls
        If myEnvironment.AdiabaticWalls = True Then
            csv.str(i, CFASTlnNum.keyWord) = "ADIAB"
            i += 1
        End If
        'comment header of material properties section
        If myThermalProperties.Count > 0 Then AddHeadertoOutput(csv, i, "Material Properties")
        Dim aThermalProperty As ThermalProperty
        'thermal properties
        For j = 0 To myThermalProperties.Count - 1
            aThermalProperty = myThermalProperties.Item(j)
            If myThermalProperties.NumberofConnections(aThermalProperty.ShortName) > 0 Then
                csv.str(i, CFASTlnNum.keyWord) = "MATL"
                csv.str(i, MaterialNum.shortName) = aThermalProperty.ShortName
                csv.num(i, MaterialNum.Conductivity) = aThermalProperty.Conductivity
                csv.num(i, MaterialNum.specificHeat) = aThermalProperty.SpecificHeat
                csv.num(i, MaterialNum.density) = aThermalProperty.Density
                csv.num(i, MaterialNum.thickness) = aThermalProperty.Thickness
                csv.num(i, MaterialNum.emissivity) = aThermalProperty.Emissivity
                csv.str(i, MaterialNum.longName) = aThermalProperty.Name
                i += 1
            End If
            aThermalProperty.Changed = False
        Next
        'comment header of compartment section
        If myCompartments.Count > 0 Then AddHeadertoOutput(csv, i, "Compartments")
        'compartments
        Dim aCompartment As Compartment
        For j = 0 To myCompartments.Count - 1
            aCompartment = myCompartments.Item(j)
            csv.str(i, CFASTlnNum.keyWord) = "COMPA"
            csv.str(i, compaNum.Name) = aCompartment.Name
            aCompartment.GetSize(csv.num(i, compaNum.Width), csv.num(i, compaNum.Depth), csv.num(i, compaNum.Height))
            aCompartment.GetPosition(csv.num(i, compaNum.AbsXPos), csv.num(i, compaNum.AbsYPos),
                    csv.num(i, compaNum.FlrHeight))
            aCompartment.GetMaterial(csv.str(i, compaNum.CeilingMat), csv.str(i, compaNum.WallMat),
                    csv.str(i, compaNum.FloorMat))
            If csv.str(i, compaNum.CeilingMat) = "Off" Then csv.str(i, compaNum.CeilingMat) = "OFF"
            If csv.str(i, compaNum.WallMat) = "Off" Then csv.str(i, compaNum.WallMat) = "OFF"
            If csv.str(i, compaNum.FloorMat) = "Off" Then csv.str(i, compaNum.FloorMat) = "OFF"
            aCompartment.GetGrid(csv.num(i, compaNum.xGrid), csv.num(i, compaNum.yGrid), csv.num(i, compaNum.zGrid))
            aCompartment.Changed = False
            i += 1
        Next
        'hall
        For j = 0 To myCompartments.Count - 1
            aCompartment = myCompartments.Item(j)
            If aCompartment.Hall Then
                csv.str(i, CFASTlnNum.keyWord) = "HALL"
                csv.num(i, hallNum.compartment) = j + 1
                i += 1
            End If
        Next
        'shaft
        For j = 0 To myCompartments.Count - 1
            aCompartment = myCompartments.Item(j)
            If aCompartment.Shaft Then
                csv.str(i, CFASTlnNum.keyWord) = "ONEZ"
                csv.num(i, hallNum.compartment) = j + 1
                i += 1
            End If
        Next
        'RoomA and RoomH
        For j = 0 To myCompartments.Count - 1
            aCompartment = myCompartments.Item(j)
            aCompartment.GetVariableAreaPoints(x)
            If x.GetUpperBound(0) > 0 Then
                csv.str(i, CFASTlnNum.keyWord) = "ROOMA"
                csv.num(i, 2) = j + 1
                csv.num(i, 3) = x.GetUpperBound(0)
                For k = 1 To x.GetUpperBound(0)
                    csv.num(i, k + 3) = x(k)
                Next
                i += 1
                csv.str(i, CFASTlnNum.keyWord) = "ROOMH"
                aCompartment.GetVariableAreasHeight(x)
                csv.num(i, 2) = j + 1
                csv.num(i, 3) = x.GetUpperBound(0)
                For k = 1 To x.GetUpperBound(0)
                    csv.num(i, k + 3) = x(k)
                Next
                i += 1
            End If
        Next
        'comment header for vents
        If myVVents.Count > 0 Or myHVents.Count > 0 Or myMVents.Count > 0 Then AddHeadertoOutput(csv, i, "Vents")
        Dim aVent As Vent
        'horizontal flow
        For j = 0 To myHVents.Count - 1
            csv.str(i, CFASTlnNum.keyWord) = "HVENT"
            aVent = myHVents.Item(j)
            csv.num(i, hventNum.firstcompartment) = aVent.FirstCompartment + 1
            csv.num(i, hventNum.secondcompartment) = aVent.SecondCompartment + 1
            If csv.num(i, hventNum.firstcompartment) = 0 Then _
                csv.num(i, hventNum.firstcompartment) = myCompartments.Count + 1
            If csv.num(i, hventNum.secondcompartment) = 0 Then _
                csv.num(i, hventNum.secondcompartment) = myCompartments.Count + 1
            csv.num(i, hventNum.ventnumber) = myHVents.VentNumber(j)
            csv.num(i, hventNum.width) = aVent.Width
            csv.num(i, hventNum.sill) = aVent.Sill
            csv.num(i, hventNum.soffit) = aVent.Soffit
            csv.num(i, hventNum.hall1) = aVent.Offset
            csv.str(i, hventNum.face) = aVent.Face
            csv.num(i, hventNum.initialfraction) = aVent.InitialOpening
            csv.str(i, hventNum.openingtype) = OpenTypes.Substring(aVent.OpenType * 4, 4)
            If aVent.OpenType = Vent.OpenbyTime Then
                csv.num(i, hventNum.openinitialtime) = aVent.InitialOpeningTime
                csv.num(i, hventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, hventNum.openfinaltime) = aVent.FinalOpeningTime
                csv.num(i, hventNum.openfinalfraction) = aVent.FinalOpening
            Else
                csv.num(i, hventNum.opencriterion) = aVent.OpenValue
                csv.str(i, hventNum.opentarget) = aVent.Target
                csv.num(i, hventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, hventNum.openfinalfraction) = aVent.FinalOpening
            End If
            csv.num(i, hventNum.xoffset) = aVent.OffsetX
            csv.num(i, hventNum.yoffset) = aVent.OffsetY
            aVent.Changed = False
            i += 1
        Next
        'vertical flow
        For j = 0 To myVVents.Count - 1
            aVent = myVVents.Item(j)
            csv.str(i, CFASTlnNum.keyWord) = "VVENT"
            csv.num(i, vventNum.firstcompartment) = aVent.FirstCompartment + 1
            If csv.num(i, vventNum.firstcompartment) = 0 Then _
                csv.num(i, vventNum.firstcompartment) = myCompartments.Count + 1
            csv.num(i, vventNum.secondcompartment) = aVent.SecondCompartment + 1
            If csv.num(i, vventNum.secondcompartment) = 0 Then _
                csv.num(i, vventNum.secondcompartment) = myCompartments.Count + 1
            csv.num(i, vventNum.ventnumber) = myVVents.VentNumber(j)
            csv.num(i, vventNum.area) = aVent.Area
            csv.num(i, vventNum.shape) = aVent.Shape
            csv.str(i, vventNum.openingtype) = OpenTypes.Substring(aVent.OpenType * 4, 4)
            If aVent.OpenType = Vent.OpenbyTime Then
                csv.num(i, vventNum.openinitialtime) = aVent.InitialOpeningTime
                csv.num(i, vventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, vventNum.openfinaltime) = aVent.FinalOpeningTime
                csv.num(i, vventNum.openfinalfraction) = aVent.FinalOpening
            Else
                csv.num(i, vventNum.opencriterion) = aVent.OpenValue
                csv.str(i, vventNum.opentarget) = aVent.Target
                csv.num(i, vventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, vventNum.openfinalfraction) = aVent.FinalOpening
            End If
            csv.num(i, vventNum.xoffset) = aVent.OffsetX
            csv.num(i, vventNum.yoffset) = aVent.OffsetY
            aVent.Changed = False
            i += 1
        Next
        'mvent
        For j = 0 To myMVents.Count - 1
            csv.str(i, CFASTlnNum.keyWord) = "MVENT"
            aVent = myMVents.Item(j)
            aVent.GetVent(csv.num(i, mventNum.fromCompartment), csv.num(i, mventNum.fromArea),
                csv.num(i, mventNum.fromHeight), csv.str(i, mventNum.fromOpenOrien), csv.num(i, mventNum.toCompartment),
                csv.num(i, mventNum.toArea), csv.num(i, mventNum.toHeight), csv.str(i, mventNum.toOpenOrien),
                csv.num(i, mventNum.flow), csv.num(i, mventNum.beginFlowDrop), csv.num(i, mventNum.flowZero))
            csv.num(i, mventNum.fromCompartment) += 1
            If csv.num(i, mventNum.fromCompartment) = 0 Then _
                csv.num(i, mventNum.fromCompartment) = myCompartments.Count + 1
            csv.num(i, mventNum.toCompartment) += 1
            If csv.num(i, mventNum.toCompartment) = 0 Then csv.num(i, mventNum.toCompartment) = myCompartments.Count + 1
            csv.num(i, mventNum.IDNumber) = j + 1
            csv.num(i, mventNum.initialfraction) = aVent.InitialOpening
            csv.str(i, mventNum.openingtype) = OpenTypes.Substring(aVent.OpenType * 4, 4)
            If aVent.OpenType = Vent.OpenbyTime Then
                csv.num(i, mventNum.openinitialtime) = aVent.InitialOpeningTime
                csv.num(i, mventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, mventNum.openfinaltime) = aVent.FinalOpeningTime
                csv.num(i, mventNum.openfinalfraction) = aVent.FinalOpening
            Else
                csv.num(i, mventNum.opencriterion) = aVent.OpenValue
                csv.str(i, mventNum.opentarget) = aVent.Target
                csv.num(i, mventNum.openinitialfraction) = aVent.InitialOpening
                csv.num(i, mventNum.openfinalfraction) = aVent.FinalOpening
            End If
            csv.num(i, mventNum.xoffset) = aVent.OffsetX
            csv.num(i, mventNum.yoffset) = aVent.OffsetY
            aVent.Changed = False
            i += 1
        Next
        'ramps
        For j = 0 To myHVents.Count - 1
            aVent = myHVents.Item(j)
            aVent.GetRampTimes(x)
            If x.GetUpperBound(0) > 0 Then
                aVent.GetRampFractions(y)
                csv.str(i, CFASTlnNum.keyWord) = "RAMP"
                csv.str(i, rampNum.ventType) = "H"
                csv.num(i, rampNum.firstcompartment) = aVent.FirstCompartment + 1
                csv.num(i, rampNum.secondcompartment) = aVent.SecondCompartment + 1
                If csv.num(i, rampNum.firstcompartment) = 0 Then _
                csv.num(i, rampNum.firstcompartment) = myCompartments.Count + 1
                If csv.num(i, rampNum.secondcompartment) = 0 Then _
                csv.num(i, rampNum.secondcompartment) = myCompartments.Count + 1
                csv.num(i, rampNum.ventnumber) = myHVents.VentNumber(j)
                csv.num(i, rampNum.numpoints) = x.GetUpperBound(0)
                For k = 1 To x.GetUpperBound(0)
                    csv.num(i, 2 * k + rampNum.numpoints - 1) = x(k)
                    csv.num(i, 2 * k + rampNum.numpoints) = y(k)
                Next
                i += 1
            End If
        Next
        For j = 0 To myVVents.Count - 1
            aVent = myVVents.Item(j)
            aVent.GetRampTimes(x)
            If x.GetUpperBound(0) > 0 Then
                aVent.GetRampFractions(y)
                csv.str(i, CFASTlnNum.keyWord) = "RAMP"
                csv.str(i, rampNum.ventType) = "V"
                csv.num(i, rampNum.firstcompartment) = aVent.FirstCompartment + 1
                csv.num(i, rampNum.secondcompartment) = aVent.SecondCompartment + 1
                If csv.num(i, rampNum.firstcompartment) = 0 Then _
                csv.num(i, rampNum.firstcompartment) = myCompartments.Count + 1
                If csv.num(i, rampNum.secondcompartment) = 0 Then _
                csv.num(i, rampNum.secondcompartment) = myCompartments.Count + 1
                csv.num(i, rampNum.ventnumber) = myVVents.VentNumber(j)
                csv.num(i, rampNum.numpoints) = x.GetUpperBound(0)
                For k = 1 To x.GetUpperBound(0)
                    csv.num(i, 2 * k + rampNum.numpoints - 1) = x(k)
                    csv.num(i, 2 * k + rampNum.numpoints) = y(k)
                Next
                i += 1
            End If
        Next
        For j = 0 To myMVents.Count - 1
            aVent = myMVents.Item(j)
            aVent.GetRampTimes(x)
            If x.GetUpperBound(0) > 0 Then
                aVent.GetRampFractions(y)
                csv.str(i, CFASTlnNum.keyWord) = "RAMP"
                csv.str(i, rampNum.ventType) = "M"
                csv.num(i, rampNum.firstcompartment) = aVent.FirstCompartment + 1
                csv.num(i, rampNum.secondcompartment) = aVent.SecondCompartment + 1
                If csv.num(i, rampNum.firstcompartment) = 0 Then _
                csv.num(i, rampNum.firstcompartment) = myCompartments.Count + 1
                If csv.num(i, rampNum.secondcompartment) = 0 Then _
                csv.num(i, rampNum.secondcompartment) = myCompartments.Count + 1
                csv.num(i, rampNum.ventnumber) = myMVents.VentNumber(j)
                csv.num(i, rampNum.numpoints) = x.GetUpperBound(0)
                For k = 1 To x.GetUpperBound(0)
                    csv.num(i, 2 * k + rampNum.numpoints - 1) = x(k)
                    csv.num(i, 2 * k + rampNum.numpoints) = y(k)
                Next
                i += 1
            End If
        Next
        'events (at this point, only filtering is writen out as an EVENT
        For j = 0 To myMVents.Count - 1
            aVent = myMVents.Item(j)
            ' Mechanical ventilation filtering fraction and time
            If aVent.FilterEfficiency <> 0 Then
                csv.str(i, CFASTlnNum.keyWord) = "EVENT"
                csv.str(i, eventNum.ventType) = "F"
                csv.num(i, eventNum.firstCompartment) = aVent.FirstCompartment + 1
                If csv.num(i, eventNum.firstCompartment) = 0 Then _
                    csv.num(i, eventNum.firstCompartment) = myCompartments.Count + 1
                csv.num(i, eventNum.secondCompartment) = aVent.SecondCompartment + 1
                If csv.num(i, eventNum.secondCompartment) = 0 Then _
                    csv.num(i, eventNum.secondCompartment) = myCompartments.Count + 1
                csv.num(i, eventNum.ventNumber) = j + 1
                csv.num(i, eventNum.time) = aVent.FilterTime
                csv.num(i, eventNum.filterEfficiency) = aVent.FilterEfficiency / 100.0
                csv.num(i, eventNum.decaytime) = 1.0
                i += 1
            End If
        Next
        'comment header for fire keywords
        If myFires.Count > 0 Then AddHeadertoOutput(csv, i, "Fires")

        If myEnvironment.LowerOxygenLimit <> Environment.DefaultLOI Then
            csv.str(i, CFASTlnNum.keyWord) = "LIMO2"
            csv.num(i, fireNum.limo2) = myEnvironment.LowerOxygenLimit
            i += 1
        End If

        Dim aFire As New Fire, firedata(12, 0) As Single, numFireDataPoints As Integer
        For j = 0 To myFires.Count - 1
            aFire = myFires.Item(j)

            csv.str(i, CFASTlnNum.keyWord) = "!!" + aFire.Name
            i += 1
            ' FIRE keyword, geometry information
            csv.str(i, CFASTlnNum.keyWord) = "FIRE"
            csv.num(i, fireNum.compartment) = aFire.Compartment + 1
            csv.num(i, fireNum.xPosition) = aFire.XPosition
            csv.num(i, fireNum.yPosition) = aFire.YPosition
            csv.num(i, fireNum.zposition) = aFire.Height
            csv.num(i, fireNum.plumeType) = aFire.PlumeType + 1
            csv.str(i, fireNum.ignType) = IgnitionTypes.Substring(aFire.IgnitionType * 4, 4)
            csv.num(i, fireNum.ignCriterion) = aFire.IgnitionValue
            If aFire.IgnitionType = Fire.FireIgnitionbyTime Then
                csv.num(i, fireNum.xNormal) = 0
            Else
                csv.str(i, fireNum.ignTarget) = aFire.Target
            End If
            csv.num(i, fireNum.yNormal) = 0
            csv.num(i, fireNum.zNormal) = 0
            csv.str(i, fireNum.name) = aFire.Name
            i += 1
            ' CHEMI keyword, chemistry information
            csv.str(i, CFASTlnNum.keyWord) = "CHEMI"
            csv.num(i, chemieNum.C) = aFire.ChemicalFormula(formula.C)
            csv.num(i, chemieNum.H) = aFire.ChemicalFormula(formula.H)
            csv.num(i, chemieNum.O) = aFire.ChemicalFormula(formula.O)
            csv.num(i, chemieNum.N) = aFire.ChemicalFormula(formula.N)
            csv.num(i, chemieNum.Cl) = aFire.ChemicalFormula(formula.Cl)
            csv.num(i, chemieNum.chiR) = aFire.RadiativeFraction
            csv.num(i, chemieNum.HoC) = aFire.HeatofCombustion
            i += 1
            ' Fire time series keywords, TIME, HRR, SOOT, CO, TRACE
            aFire.GetFireData(firedata, numFireDataPoints)
            For k = 1 To NumFireCurves
                csv.str(i, CFASTlnNum.keyWord) = Trim(FireCurveTypes.Substring(5 * (k - 1), 5))
                For l = 0 To numFireDataPoints
                    csv.num(i, l + 2) = firedata(FireCurveColumns(k), l)
                Next
                i += 1
            Next
            aFire.Changed = False
        Next

        'comment header of targets and detectors
        If myDetectors.Count > 0 Or myTargets.Count > 0 Then AddHeadertoOutput(csv, i, "Targets And detectors")
        'detectors
        Dim aDetect As New Target
        For j = 0 To myDetectors.Count - 1
            csv.str(i, CFASTlnNum.keyWord) = "DETECT"
            aDetect = myDetectors.Item(j)
            csv.num(i, detectNum.compartment) = aDetect.Compartment + 1
            csv.num(i, detectNum.xPosition) = aDetect.XPosition
            csv.num(i, detectNum.yPosition) = aDetect.YPosition
            csv.num(i, detectNum.zPosition) = aDetect.ZPosition
            If aDetect.DetectorType = Target.TypeSmokeDetector Then
                csv.str(i, detectNum.type) = "SMOKE"
                csv.num(i, detectNum.suppression) = 0
                csv.num(i, detectNum.activationObscuration) = aDetect.ActivationObscuration
            ElseIf aDetect.DetectorType = Target.TypeHeatDetector Then
                csv.str(i, detectNum.type) = "HEAT"
                csv.num(i, detectNum.suppression) = 0
                csv.num(i, detectNum.activationTemp) = aDetect.ActivationTemperature
            Else
                csv.str(i, detectNum.type) = "SPRINKLER"
                csv.num(i, detectNum.suppression) = 1
                csv.num(i, detectNum.activationTemp) = aDetect.ActivationTemperature
            End If
            csv.num(i, detectNum.RTI) = aDetect.RTI
            csv.num(i, detectNum.sprayDensity) = aDetect.SprayDensity
            aDetect.Changed = False
            i += 1
        Next
        'Targets
        For j = 0 To myTargets.Count - 1
            aDetect = myTargets.Item(j)
            csv.str(i, CFASTlnNum.keyWord) = "TARGET"
            csv.num(i, targetNum.compartment) = aDetect.Compartment + 1
            csv.num(i, targetNum.xPosition) = aDetect.XPosition
            csv.num(i, targetNum.yPosition) = aDetect.YPosition
            csv.num(i, targetNum.zPosition) = aDetect.ZPosition
            csv.num(i, targetNum.xNormal) = aDetect.XNormal
            csv.num(i, targetNum.yNormal) = aDetect.YNormal
            csv.num(i, targetNum.zNormal) = aDetect.ZNormal
            csv.str(i, targetNum.material) = aDetect.Material
            csv.num(i, targetNum.internalLocation) = aDetect.InternalLocation
            If aDetect.SolutionType = 1 Then
                csv.str(i, targetNum.equationType) = "CYL"
            Else
                csv.str(i, targetNum.equationType) = "PDE"
            End If
            csv.str(i, targetNum.method) = "EXPLICIT"
            csv.str(i, targetNum.name) = aDetect.Name
            aDetect.Changed = False
            i += 1
        Next

        'comment header for heat transfer section
        If myHHeats.Count > 0 Or myVHeats.Count > 0 Then AddHeadertoOutput(csv, i, "Intercompartment heat transfer")
        'HHeat and VHeat
        For j = 0 To myCompartments.Count - 1
            If myHHeats.FromConnections(j) > 0 Then
                l = 0
                csv.str(i, CFASTlnNum.keyWord) = "HHEAT"
                csv.num(i, hheatNum.firstCompartment) = j + 1
                csv.num(i, hheatNum.num) = myHHeats.FromConnections(j)
                For k = 0 To myHHeats.Count - 1
                    aVent = myHHeats.Item(k)
                    If aVent.FirstCompartment = j Then
                        l += 1
                        If aVent.SecondCompartment = -1 Then
                            csv.num(i, hheatNum.secondCompartment + 2 * (l - 1)) = myCompartments.Count + 1
                        Else
                            csv.num(i, hheatNum.secondCompartment + 2 * (l - 1)) = aVent.SecondCompartment + 1
                        End If
                        csv.num(i, hheatNum.fraction + 2 * (l - 1)) = aVent.InitialOpening
                        aVent.Changed = False
                    End If
                Next
                If myHHeats.ConnectedFraction(j) < 1 Then
                    csv.num(i, hheatNum.num) = myHHeats.FromConnections(j) + 1
                    l += 1
                    csv.num(i, hheatNum.secondCompartment + 2 * (l - 1)) = myCompartments.Count + 1
                    csv.num(i, hheatNum.fraction + 2 * (l - 1)) = 1.0 - myHHeats.ConnectedFraction(j)
                End If
                i += 1
            End If
        Next
        For j = 0 To myVHeats.Count - 1
            aVent = myVHeats.Item(j)
            csv.str(i, CFASTlnNum.keyWord) = "VHEAT"
            csv.num(i, vheatNum.firstcompartment) = aVent.FirstCompartment + 1
            csv.num(i, vheatNum.secondcompartment) = aVent.SecondCompartment + 1
            If csv.num(i, vheatNum.firstcompartment) = 0 Then _
                                            csv.num(i, vheatNum.firstcompartment) = myCompartments.Count + 1
            If csv.num(i, vheatNum.secondcompartment) = 0 Then _
                csv.num(i, vheatNum.secondcompartment) = myCompartments.Count + 1
            aVent.Changed = False
            i += 1
        Next

        'comment header of visualizations
        If myVisuals.Count > 0 Then
            AddHeadertoOutput(csv, i, "Visualizations")
            Dim aVisual As Visual
            For j = 0 To myVisuals.Count - 1
                aVisual = myVisuals.Item(j)

                Select Case aVisual.Type
                    Case Visual.TwoD
                        csv.str(i, CFASTlnNum.keyWord) = "SLCF"
                        csv.str(i, visualNum.sliceType) = "2-D"
                        csv.str(i, visualNum.slice2DAxis) = VisualAxisNames.Substring((aVisual.Axis) * 6, 1)
                        csv.num(i, visualNum.slice2DPosition) = aVisual.Value
                        If aVisual.Compartment > -1 Then csv.num(i, visualNum.slice2DCompartment) = aVisual.Compartment + 1
                    Case Visual.ThreeD
                        csv.str(i, CFASTlnNum.keyWord) = "SLCF"
                        csv.str(i, visualNum.sliceType) = "3-D"
                        If aVisual.Compartment > -1 Then csv.num(i, visualNum.slice3DCompartment) = aVisual.Compartment + 1
                    Case Visual.IsoSurface
                        csv.str(i, CFASTlnNum.keyWord) = "ISOF"
                        csv.num(i, visualNum.isoValue) = aVisual.Value
                        If aVisual.Compartment > -1 Then csv.num(i, visualNum.isoCompartment) = aVisual.Compartment + 1
                End Select
                aVisual.Changed = False
                i += 1
            Next
        End If

        'comment header of misc.
        If myEnvironment.MaximumTimeStep > 0 Or myThermalProperties.FileName <> "thermal" Then _
            AddHeadertoOutput(csv, i, "Misc.")
        'stepmax
        If myEnvironment.MaximumTimeStep > 0 Then
            csv.str(i, CFASTlnNum.keyWord) = "STPMAX"
            csv.num(i, 2) = myEnvironment.MaximumTimeStep
            i += 1
        End If
        myEnvironment.Changed = False

        'comments and dead keywords
        If dataFileComments.Count > 0 Then AddHeadertoOutput(csv, i, "comments And ignored keywords")
        'comments
        For j = 1 To dataFileComments.Count
            csv.CSVrow(i) = dataFileComments.Item(j)
            i += 1
        Next

        csv.WrtCSVfile(FileName)

    End Sub
    Public Sub WriteThermalProperties(ByVal FileName As String)
        'If myThermalProperties.Changed Then
        Dim csv As New CSVsheet
        Dim aThermalProperty As ThermalProperty
        Dim i As Integer, j As Integer
        'thermal properties
        i = 1
        For j = 0 To myThermalProperties.Count - 1
            aThermalProperty = myThermalProperties.Item(j)
            csv.str(i, CFASTlnNum.keyWord) = "MATL"
            csv.str(i, MaterialNum.shortName) = aThermalProperty.ShortName
            csv.num(i, MaterialNum.Conductivity) = aThermalProperty.Conductivity
            csv.num(i, MaterialNum.specificHeat) = aThermalProperty.SpecificHeat
            csv.num(i, MaterialNum.density) = aThermalProperty.Density
            csv.num(i, MaterialNum.thickness) = aThermalProperty.Thickness
            csv.num(i, MaterialNum.emissivity) = aThermalProperty.Emissivity
            csv.str(i, MaterialNum.longName) = aThermalProperty.Name
            i += 1
        Next
        csv.WrtCSVfile(FileName)
        myThermalProperties.FileChanged = False
        'End If
    End Sub
    Public Sub WriteInputFileNML(ByVal filename As String)
        Dim IO As Integer = 1
        Dim ln As String
        Dim i, j, k As Integer
        Dim aFlag As Boolean
        Dim aDummy As Single
        Dim x(0), f(0) As Single
        Dim aComp As Compartment
        Dim aTarg As Target
        Dim aVent As Vent
        Dim aFire As Fire
        Dim aVisual As Visual
        Dim doneRamps As New RampCollection

        FileOpen(IO, filename, OpenMode.Output, OpenAccess.Write, OpenShare.Shared)

        'Writing HEAD namelist
        ln = "&HEAD VERSION = " + myEnvironment.Version.ToString + " , "
        PrintLine(IO, ln)
        ln = "TITLE = " + "'" + myEnvironment.Title + "' "
        PrintLine(IO, ln)
        ln = "/"
        PrintLine(IO, ln)

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! Scenario Configuration "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        'Writing TIME namelist
        ln = "&TIME SIMULATION = " + myEnvironment.SimulationTime.ToString + " , PRINT = " + myEnvironment.OutputInterval.ToString +
            " , SMOKEVIEW = " + myEnvironment.SmokeviewInterval.ToString + " , SPREADSHEET = " + myEnvironment.SpreadsheetInterval.ToString + " / "
        PrintLine(IO, ln)

        'Writing INIT namelist
        aDummy = 293.15
        If myEnvironment.ExtAmbPressure <> 101325.0 Or myEnvironment.IntAmbRH <> 50.0 Or myEnvironment.IntAmbTemperature <> aDummy _
            Or myEnvironment.ExtAmbTemperature <> aDummy Then
            ln = "&INIT "
            PrintLine(IO, ln)
            aFlag = True
        Else
            aFlag = False
        End If
        If myEnvironment.ExtAmbPressure <> 101325.0 Then
            ln = "PRESSURE = " + myEnvironment.ExtAmbPressure.ToString
            PrintLine(IO, ln)
        End If
        If myEnvironment.IntAmbRH <> 50.0 Then
            ln = " RELATIVE_HUMIDITY = " + myEnvironment.IntAmbRH.ToString
            PrintLine(IO, ln)
        End If
        aDummy = 293.15
        If myEnvironment.IntAmbTemperature <> aDummy Then
            aDummy = 273.15
            aDummy = myEnvironment.IntAmbTemperature - aDummy
            ln = " INTERIOR_TEMPERATURE = " + aDummy.ToString
            PrintLine(IO, ln)
        End If
        aDummy = 293.15
        If myEnvironment.ExtAmbTemperature <> aDummy Then
            aDummy = 273.15
            aDummy = myEnvironment.ExtAmbTemperature - aDummy
            ln = " EXTERIOR_TEMPERATURE = " + aDummy.ToString
            PrintLine(IO, ln)
        End If
        If aFlag Then
            ln = " / "
            PrintLine(IO, ln)
        End If

        'Writing MISC namelist
        aDummy = 0.15
        If myEnvironment.AdiabaticWalls Or (myEnvironment.MaximumTimeStep <> 2.0 And myEnvironment.MaximumTimeStep > 0.0) Or myEnvironment.LowerOxygenLimit <> aDummy Then
            ln = "&MISC "
            PrintLine(IO, ln)
            aFlag = True
        Else
            aFlag = False
        End If
        If myEnvironment.AdiabaticWalls <> False Then
            ln = " ADIABATIC = .TRUE. "
            PrintLine(IO, ln)
        End If
        If myEnvironment.MaximumTimeStep <> 2.0 And myEnvironment.MaximumTimeStep > 0 Then
            ln = " MAX_TIME_STEP = " + myEnvironment.MaximumTimeStep.ToString
            PrintLine(IO, ln)
        End If
        aDummy = 0.15
        If myEnvironment.LowerOxygenLimit <> aDummy Then
            ln = " LOWER_OXYGEN_LIMIT = " + myEnvironment.LowerOxygenLimit.ToString
            PrintLine(IO, ln)
        End If
        If aFlag Then
            ln = " / "
            PrintLine(IO, ln)
        End If

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! Material Properties "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        'Writing MATL namelist
        For i = 0 To myThermalProperties.Count - 1
            ln = "&MATL"
            PrintLine(IO, ln)
            ln = " ID = '" + myThermalProperties.Item(i).ShortName + "' , "
            PrintLine(IO, ln)
            If myThermalProperties.Item(i).Name <> "" Then
                If myThermalProperties.Item(i).Name.IndexOf("'") > 0 Then
                    ln = " MATERIAL = """ + myThermalProperties.Item(i).Name + """ , "
                Else
                    ln = " MATERIAL = '" + myThermalProperties.Item(i).Name + "' , "
                End If
                PrintLine(IO, ln)
            End If
            ln = " CONDUCTIVITY = " + myThermalProperties.Item(i).Conductivity.ToString + " , DENSITY = " + myThermalProperties.Item(i).Density.ToString + " , "
            PrintLine(IO, ln)
            ln = " SPECIFIC_HEAT = " + (myThermalProperties.Item(i).SpecificHeat / 1000.0).ToString + " , THICKNESS = " + myThermalProperties.Item(i).Thickness.ToString
            PrintLine(IO, ln)
            aDummy = 0.9
            If myThermalProperties.Item(i).Emissivity <> aDummy Then
                ln = " EMISSIVITY = " + myThermalProperties.Item(i).Emissivity.ToString
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! Comparments "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        ' Writing COMP namelist

        For i = 0 To myCompartments.Count - 1
            ln = "&COMP "
            PrintLine(IO, ln)
            aComp = myCompartments.Item(i)
            ln = " ID = '" + aComp.Name + "' "
            PrintLine(IO, ln)
            ln = " DEPTH = " + aComp.RoomDepth.ToString + " , HEIGHT = " + aComp.RoomHeight.ToString + " , WIDTH = " + aComp.RoomWidth.ToString
            PrintLine(IO, ln)
            If aComp.CeilingMaterial <> "" Then
                If aComp.CeilingMaterial = "Off" Then
                    ln = " CEILING_MATL_ID = 'OFF' "
                Else
                    ln = " CEILING_MATL_ID = '" + aComp.CeilingMaterial + "' "
                End If
                PrintLine(IO, ln)
            End If
            If aComp.WallMaterial <> "" Then
                If aComp.WallMaterial = "Off" Then
                    ln = " WALL_MATL_ID = 'OFF' "
                Else
                    ln = " WALL_MATL_ID = '" + aComp.WallMaterial + "' "
                End If
                PrintLine(IO, ln)
            End If
            If aComp.FloorMaterial <> "" Then
                If aComp.FloorMaterial = "Off" Then
                    ln = " FLOOR_MATL_ID = 'OFF' "
                Else
                    ln = " FLOOR_MATL_ID = '" + aComp.FloorMaterial + "' "
                End If
                PrintLine(IO, ln)
            End If
            If aComp.RoomOriginX <> 0 Or aComp.RoomOriginY <> 0 Or aComp.RoomOriginZ <> 0 Then
                ln = " ORIGIN = " + aComp.RoomOriginX.ToString + " , " + aComp.RoomOriginY.ToString + " , " + aComp.RoomOriginZ.ToString
                PrintLine(IO, ln)
            End If
            If aComp.xGrid <> 50 Or aComp.yGrid <> 50 Or aComp.zGrid <> 50 Then
                ln = " GRID = " + aComp.xGrid.ToString + " , " + aComp.yGrid.ToString + " , " + aComp.zGrid.ToString
                PrintLine(IO, ln)
            End If
            If aComp.Hall Then
                ln = " HALL = .TRUE. "
                PrintLine(IO, ln)
            ElseIf aComp.Shaft Then
                ln = " SHAFT = .TRUE. "
                PrintLine(IO, ln)
            End If
            aComp.GetVariableArea(f, x, j)
            If j > 1 Then
                ln = " CROSS_SECT_AREAS = "
                For k = 1 To j
                    ln = ln + f(k).ToString + " , "
                Next
                PrintLine(IO, ln)
                ln = " CROSS_SECT_HEIGHTS = "
                For k = 1 To j
                    ln = ln + x(k).ToString + " , "
                Next
                PrintLine(IO, ln)
            End If
            'If aComp.AreaRampID <> "" Then
            'ln = " ROOM_AREA_RAMP = '" + aComp.AreaRampID + "' "
            'PrintLine(IO, ln)
            'End If
            ln = " / "
            PrintLine(IO, ln)
            'If aComp.AreaRampID <> "" Then
            'WriteRamp(IO, aComp.AreaRampID, doneRamps, 0)
            'End If
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! DEVC"
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        For i = 0 To myTargets.Count - 1
            aTarg = myTargets.Item(i)
            ln = "&DEVC "
            PrintLine(IO, ln)
            ln = " ID = '" + aTarg.Name + "' COMP_ID = '" + myCompartments.Item(aTarg.Compartment).Name + "' "
            PrintLine(IO, ln)
            ln = " LOCATION = " + aTarg.XPosition.ToString + " , " + aTarg.YPosition.ToString + " , " + aTarg.ZPosition.ToString
            PrintLine(IO, ln)
            If aTarg.SolutionType = Target.ThermallyThick Then
                ln = " TYPE = 'PLATE' "
            Else
                ln = " TYPE = 'CYLINDER' "
            End If
            PrintLine(IO, ln)
            ln = " MATL_ID = '" + myThermalProperties.Item(myThermalProperties.GetIndex(aTarg.Material)).ShortName + "' "
            PrintLine(IO, ln)
            ln = ""
            If aTarg.XNormal <> 0 Or aTarg.YNormal <> 0 Or aTarg.ZNormal <> 1 Then
                ln = " NORMAL = " + aTarg.XNormal.ToString + " , " + aTarg.YNormal.ToString + " , " + aTarg.ZNormal.ToString
            End If
            If aTarg.InternalLocation <> 0.5 Then
                If ln <> "" Then ln = ln + " ,"
                ln = ln + " TEMPERATURE_DEPTH = " + aTarg.InternalLocation.ToString
            End If
            If ln <> "" Then
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
        Next

        For i = 0 To myDetectors.Count - 1
            aTarg = myDetectors.Item(i)
            ln = "&DEVC "
            PrintLine(IO, ln)
            If aTarg.Name = "" Then
                If aTarg.DetectorType = Target.TypeHeatDetector Then
                    aTarg.Name = "HeatDetector_" + (i + 1).ToString
                ElseIf aTarg.DetectorType = Target.TypeSmokeDetector Then
                    aTarg.Name = "SmokeDetector_" + (i + 1).ToString
                ElseIf aTarg.DetectorType = Target.TypeSprinkler Then
                    aTarg.Name = "Sprinkler_" + (i + 1).ToString
                End If
            End If
            ln = " ID = '" + aTarg.Name + "' COMP_ID = '" + myCompartments.Item(aTarg.Compartment).Name + "' "
            PrintLine(IO, ln)
            ln = " LOCATION = " + aTarg.XPosition.ToString + " , " + aTarg.YPosition.ToString + " , " + aTarg.ZPosition.ToString
            PrintLine(IO, ln)
            If aTarg.DetectorType = Target.TypeHeatDetector Then
                ln = " TYPE = 'HEAT_DETECTOR' "
                If aTarg.ActivationTemperature <> Target.HeatDetectorActiviationTemperature Then
                    aDummy = 273.15
                    aDummy = aTarg.ActivationTemperature - aDummy
                    ln = ln + " , SETPOINT = " + aDummy.ToString
                End If
                If aTarg.RTI <> 130 Then
                    ln = ln + " , RTI = " + aTarg.RTI.ToString
                End If
            ElseIf aTarg.DetectorType = Target.TypeSmokeDetector Then
                ln = " TYPE = 'SMOKE_DETECTOR' "
                If aTarg.ActivationObscuration <> Target.SmokeDetectorActivationObscuration Then
                    ln = ln + " , SETPOINT = " + aTarg.ActivationObscuration.ToString
                End If
            Else
                ln = " TYPE = 'SPRINKLER' "
                If aTarg.ActivationTemperature <> Target.SprinklerActivationTemperature Then
                    aDummy = 273.15
                    aDummy = aTarg.ActivationTemperature - aDummy
                    ln = ln + " , SETPOINT = " + aDummy.ToString
                End If
                If aTarg.RTI <> 130 Then
                    ln = ln + " , RTI = " + aTarg.RTI.ToString
                End If
                ln = ln + " , SPRAY_DENSITY = " + aTarg.SprayDensity.ToString
            End If
            PrintLine(IO, ln)
            ln = " / "
            PrintLine(IO, ln)

        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! VENTS TYPE = WALL"
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        ' Writing VENT namelist for WALL vents
        For i = 0 To myHVents.Count - 1
            ln = "&VENT TYPE = 'WALL' , "
            PrintLine(IO, ln)
            aVent = myHVents.Item(i)
            If aVent.Name = "" Then
                aVent.Name = "WallVent_" + (i + 1).ToString
            End If
            ln = " ID = '" + aVent.Name + "' "
            PrintLine(IO, ln)
            If aVent.FirstCompartment < 0 Then
                ln = " COMP_IDS = 'OUTSIDE' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            ElseIf aVent.SecondCompartment < 0 Then
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , 'OUTSIDE' "
            Else
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            End If
            PrintLine(IO, ln)
            ln = " TOP = " + aVent.Soffit.ToString + " , BOTTOM = " + aVent.Sill.ToString + " , WIDTH = " + aVent.Width.ToString
            PrintLine(IO, ln)
            If aVent.Face > 0 Then
                If aVent.Face = 1 Then
                    ln = " FACE = 'FRONT'"
                ElseIf aVent.Face = 2 Then
                    ln = " FACE = 'RIGHT'"
                ElseIf aVent.Face = 3 Then
                    ln = " FACE = 'REAR'"
                ElseIf aVent.Face = 4 Then
                    ln = " FACE = 'LEFT'"
                End If
                PrintLine(IO, ln)
                If aVent.Offset > 0.0 Then
                    ln = " OFFSET = " + aVent.Offset.ToString
                    PrintLine(IO, ln)
                End If
            End If
            If aVent.OpenType = Vent.OpenbyTime Then
                If aVent.RampID <> "" Then
                    ln = " CRITERION = 'TIME' , OPENING_RAMP_ID = '" + aVent.RampID + "' "
                    PrintLine(IO, ln)
                ElseIf aVent.InitialOpening <> 1 Or aVent.FinalOpening <> 1 Then
                    Dim ff(2), xx(2) As Single
                    ff(0) = aVent.InitialOpening
                    ff(1) = aVent.InitialOpening
                    ff(2) = aVent.FinalOpening
                    xx(0) = 0.0
                    xx(1) = aVent.InitialOpeningTime
                    xx(2) = aVent.FinalOpeningTime
                    aVent.RampID = "VentFraction_" + myRamps.Count.ToString
                    Dim aRamp As Ramp
                    aRamp = New Ramp(aVent.RampID, "FRACTION", xx, ff, True)
                    myRamps.Add(aRamp)
                    ln = " CRITERION = 'TIME' , OPENING_RAMP_ID = '" + aVent.RampID + "' "
                    PrintLine(IO, ln)
                End If
            ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                aDummy = 273.15
                aDummy = aVent.OpenValue - aDummy
                ln = " CRITERION = 'TEMPERATURE' , SETPOINT = " + aDummy.ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                ln = " CRITERION = 'FLUX' , SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
            If aVent.RampID <> "" Then
                WriteRamp(IO, aVent.RampID, doneRamps, 0)
            End If
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! VENTS TYPE = CEILING/FLOOR "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        'Writing VENT namelist for CEILING/FLOOR vents
        For i = 0 To myVVents.Count - 1
            aVent = myVVents.Item(i)
            If aVent.FirstCompartment < 0 Then
                ln = "&VENT TYPE = 'CEILING' , "
            Else
                ln = "&VENT TYPE = 'FLOOR' , "
            End If
            PrintLine(IO, ln)
            If aVent.Name = "" Then
                aVent.Name = "CeilFloorVent_" + (i + 1).ToString
            End If
            ln = " ID = '" + aVent.Name + "' "
            PrintLine(IO, ln)
            If aVent.FirstCompartment < 0 Then
                ln = " COMP_IDS = 'OUTSIDE' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            ElseIf aVent.SecondCompartment < 0 Then
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , 'OUTSIDE' "
            Else
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            End If
            PrintLine(IO, ln)
            If aVent.Shape = 1 Then
                ln = " AREA = " + aVent.Area.ToString + " , SHAPE = 'ROUND' "
            Else
                ln = " AREA = " + aVent.Area.ToString + " , SHAPE = 'SQUARE' "
            End If
            PrintLine(IO, ln)
            If aVent.OffsetX <> 0 Or aVent.OffsetY <> 0 Then
                ln = " OFFSETS = " + aVent.OffsetX.ToString + " , " + aVent.OffsetY.ToString
                PrintLine(IO, ln)
            End If
            If aVent.OpenType = 0 Then
                If aVent.RampID <> "" Then
                    ln = " CRITERION = 'TIME' , OPENING_RAMP_ID = '" + aVent.RampID + "' "
                    PrintLine(IO, ln)
                End If
            ElseIf aVent.OpenType = 1 Then
                aDummy = 273.15
                aDummy = aVent.OpenValue - aDummy
                ln = " CRITERION = 'TEMPERATURE' , SETPOINT = " + aDummy.ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            ElseIf aVent.OpenType = 2 Then
                ln = " CRITERION = 'FLUX' , SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
            If aVent.RampID <> "" Then
                WriteRamp(IO, aVent.RampID, doneRamps, 1)
            End If
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! VENTS TYPE = MECHANICAL"
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        For i = 0 To myMVents.Count - 1
            aVent = myMVents.Item(i)
            ln = "&VENT TYPE = 'MECHANICAL' , "
            PrintLine(IO, ln)
            If aVent.Name = "" Then
                aVent.Name = "MechanicalVent_" + (i + 1).ToString
            End If
            ln = " ID = '" + aVent.Name + "' "
            PrintLine(IO, ln)
            If aVent.FirstCompartment < 0 Then
                ln = " COMP_IDS = 'OUTSIDE' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            ElseIf aVent.SecondCompartment < 0 Then
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , 'OUTSIDE' "
            Else
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' , '" _
                    + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            End If
            PrintLine(IO, ln)
            ln = " AREAS = " + aVent.FirstArea.ToString + " , " + aVent.SecondArea.ToString +
                " , HEIGHTS = " + aVent.FirstCenterHeight.ToString + " , " + aVent.SecondCenterHeight.ToString
            PrintLine(IO, ln)
            If aVent.FirstOrientation = 1 Then
                If aVent.SecondOrientation = 2 Then
                    ln = " ORIENTATIONS = 'VERTICAL' , 'HORIZONTAL' "
                    PrintLine(IO, ln)
                End If
            Else
                If aVent.SecondOrientation = 2 Then
                    ln = " ORIENTATIONS = 'HORIZONTAL' , 'HORIZANTAL' "
                    PrintLine(IO, ln)
                Else
                    ln = " ORIENTATIONS = 'HORIZONTAL' , 'VERTICAL' "
                    PrintLine(IO, ln)
                End If
            End If
            ln = " FLOW = " + aVent.FlowRate.ToString + " , CUTOFFS = " + aVent.BeginFlowDropoff.ToString + " , " + aVent.ZeroFlow.ToString
            PrintLine(IO, ln)
            ln = " OFFSETS = " + aVent.OffsetX.ToString + " , " + aVent.OffsetY.ToString
            PrintLine(IO, ln)
            If aVent.OpenType = 0 Then
                If aVent.RampID <> "" Then
                    ln = " CRITERION = 'TIME' , OPENING_RAMP_ID = '" + aVent.RampID + "' "
                    PrintLine(IO, ln)
                End If
            ElseIf aVent.OpenType = 1 Then
                aDummy = 273.15
                aDummy = aVent.OpenValue - aDummy
                ln = " CRITERION = 'TEMPERATURE' , SETPOINT = " + aDummy.ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            ElseIf aVent.OpenType = 2 Then
                ln = " CRITERION = 'FLUX' , SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + " , DEVC_ID = '" + aVent.Target + "' "
                PrintLine(IO, ln)
                ln = " PRE_FRACTION = " + aVent.InitialOpening.ToString + " , POST_FRACTION = " + aVent.FinalOpening.ToString
                PrintLine(IO, ln)
            End If
            If aVent.FilterTime > 0 And aVent.FilterEfficiency > 0 Then
                ln = " FILTER_TIME = " + aVent.FilterTime.ToString + " , FILTER_EFFICIENCY = " + aVent.FilterEfficiency.ToString
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
            If aVent.RampID <> "" Then
                WriteRamp(IO, aVent.RampID, doneRamps, 1)
            End If
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! FIRE "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        For i = 0 To myFires.Count - 1
            aFire = myFires.Item(i)
            ln = "&FIRE"
            PrintLine(IO, ln)
            ln = " ID = '" + aFire.Name + "' COMP_ID = '" + myCompartments.Item(aFire.Compartment).Name + "' "
            PrintLine(IO, ln)
            ln = " LOCATION = " + aFire.XPosition.ToString + " , " + aFire.YPosition.ToString + " , " + aFire.Height.ToString
            PrintLine(IO, ln)
            ln = " CARBON = " + aFire.ChemicalFormula(formula.C).ToString + " , CHLORINE = " + aFire.ChemicalFormula(formula.Cl).ToString +
                " , HYDROGEN = " + aFire.ChemicalFormula(formula.H).ToString + " , NITROGEN = " + aFire.ChemicalFormula(formula.N).ToString +
                " , OXYGEN = " + aFire.ChemicalFormula(formula.O).ToString
            PrintLine(IO, ln)
            If aFire.HeatofCombustion <> 50000000 Then
                ln = " HEAT_OF_COMBUSTION = " + (aFire.HeatofCombustion / 1000).ToString
                PrintLine(IO, ln)
            End If
            If aFire.RadiativeFraction <> 0.35 Then
                ln = " RADIATIVE_FRACTION = " + aFire.RadiativeFraction.ToString
                PrintLine(IO, ln)
            End If
            If aFire.IgnitionType = Fire.FireIgnitionbyTime Then
                If aFire.IgnitionType > 0 Then
                    ln = " IGNITION_CRITERION = 'TIME' , SETPOINT = " + aFire.IgnitionValue.ToString
                    PrintLine(IO, ln)
                End If
            ElseIf aFire.IgnitionType = Fire.FireIgnitionbyTemperature Then
                aDummy = 273.15
                aDummy = aFire.IgnitionValue - aDummy
                ln = " IGNITION_CRITERION = 'TEMPERATURE' , DEVC_ID = '" + aFire.Target + "' , SETPOINT = " + aDummy.ToString
                PrintLine(IO, ln)
            ElseIf aFire.IgnitionType = Fire.FireIgnitionbyFlux Then
                ln = " IGNITION_CRITERION = 'FLUX' , DEVC_ID = '" + aFire.Target + "' , SETPOINT = " + (aFire.IgnitionValue / 1000.0).ToString
                PrintLine(IO, ln)
            End If
            If myRamps.GetRampIndex(aFire.AreaRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.AreaRampID)).DimF > 0 Then
                    ln = " AREA_RAMP_ID = '" + aFire.AreaRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.CORampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.CORampID)).DimF > 0 Then
                    ln = " CO_YIELD_RAMP_ID = '" + aFire.CORampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.HClRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.HClRampID)).DimF > 0 Then
                    ln = " HCL_YIELD_RAMP_ID = '" + aFire.HClRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.HCNRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.HCNRampID)).DimF > 0 Then
                    ln = " HCN_YIELD_RAMP_ID = '" + aFire.HCNRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.HRRRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.HRRRampID)).DimF > 0 Then
                    ln = " HRR_RAMP_ID = '" + aFire.HRRRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.SootRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.SootRampID)).DimF > 0 Then
                    ln = " SOOT_YIELD_RAMP_ID = '" + aFire.SootRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            If myRamps.GetRampIndex(aFire.TraceRampID) >= 0 Then
                If myRamps.Item(myRamps.GetRampIndex(aFire.TraceRampID)).DimF > 0 Then
                    ln = " TRACE_YIELD_RAMP_ID = '" + aFire.TraceRampID + "' "
                    PrintLine(IO, ln)
                End If
            End If
            ln = " / "
            PrintLine(IO, ln)
            If myRamps.GetRampIndex(aFire.AreaRampID) >= 0 Then
                WriteRamp(IO, aFire.AreaRampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.CORampID) >= 0 Then
                WriteRamp(IO, aFire.CORampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.HClRampID) >= 0 Then
                WriteRamp(IO, aFire.HClRampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.HCNRampID) >= 0 Then
                WriteRamp(IO, aFire.HCNRampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.HRRRampID) >= 0 Then
                WriteRamp(IO, aFire.HRRRampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.SootRampID) >= 0 Then
                WriteRamp(IO, aFire.SootRampID, doneRamps, 0)
            End If
            If myRamps.GetRampIndex(aFire.TraceRampID) >= 0 Then
                WriteRamp(IO, aFire.TraceRampID, doneRamps, 0)
            End If
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! CONN "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        Dim fracln As String
        For i = 0 To myCompartments.Count - 1
            If myHHeats.FromConnections(i) > 0 Then
                ln = "&CONN "
                PrintLine(IO, ln)
                ln = " TYPE = 'WALL' "
                PrintLine(IO, ln)
                ln = " COMP_ID = '" + myCompartments.Item(i).Name + "' "
                PrintLine(IO, ln)
                fracln = " F = "
                ln = " COMP_IDS = "
                For j = 0 To myHHeats.Count - 1
                    aVent = myHHeats.Item(j)
                    If aVent.FirstCompartment = i Then
                        If aVent.SecondCompartment = -1 Then
                            ln = ln + " 'OUTSIDE'"
                        Else
                            ln = ln + "  '" + myCompartments.Item(aVent.SecondCompartment).Name + "' "
                        End If
                        fracln = fracln + aVent.InitialOpening.ToString + "  "
                    End If
                Next
                PrintLine(IO, ln)
                PrintLine(IO, fracln)
                ln = " / "
                PrintLine(IO, ln)

            End If
        Next

        For i = 0 To myVHeats.Count - 1
            ln = "&CONN "
            PrintLine(IO, ln)
            aVent = myVHeats.Item(i)
            If aVent.FirstCompartment = -1 Then
                ln = " TYPE = 'CEILING' "
            Else
                ln = " TYPE = 'FLOOR' "
            End If
            PrintLine(IO, ln)
            If aVent.FirstCompartment = -1 Then
                ln = " COMP_ID = 'OUTSIDE' "
            Else
                ln = " COMP_ID = '" + myCompartments.Item(aVent.FirstCompartment).Name + "' "
            End If
            PrintLine(IO, ln)
            If aVent.SecondCompartment = -1 Then
                ln = " COMP_IDS = 'OUTSIDE' "
            Else
                ln = " COMP_IDS = '" + myCompartments.Item(aVent.SecondCompartment).Name + "' "
            End If
            PrintLine(IO, ln)
            ln = " / "
            PrintLine(IO, ln)
        Next

        ln = "!! "
        PrintLine(IO, ln)
        ln = "!! ISOF and SLCF "
        PrintLine(IO, ln)
        ln = "!! "
        PrintLine(IO, ln)

        For i = 0 To myVisuals.Count - 1
            aVisual = myVisuals.Item(i)
            If aVisual.Type = Visual.IsoSurface Then
                ln = "&ISOF "
                PrintLine(IO, ln)
                aDummy = 273.15
                aDummy = aVisual.Value - aDummy
                ln = " VALUE = " + aDummy.ToString
                PrintLine(IO, ln)
                ln = " / "
                PrintLine(IO, ln)
            Else
                ln = "&SLCF "
                PrintLine(IO, ln)
                If aVisual.Compartment > -1 Then
                    ln = " COMP_ID = '" + myCompartments.Item(aVisual.Compartment).Name + "' "
                    PrintLine(IO, ln)
                End If
                If aVisual.Type = Visual.TwoD Then
                    ln = " DOMAIN = '2-D' "
                    PrintLine(IO, ln)
                    ln = " POSITION = " + aVisual.Value.ToString + " , PLANE = '" + VisualAxisNames.Substring((aVisual.Axis) * 6, 1) + "' "
                    PrintLine(IO, ln)
                Else
                    ln = " DOMAIN = '3-D' "
                    PrintLine(IO, ln)
                End If
                    ln = " / "
                    PrintLine(IO, ln)
                End If
        Next

        doneRamps.Clear()
        FileClose(IO)

    End Sub
    Public Sub WriteRamp(ByVal IO As Integer, ByVal name As String, ByRef doneRamps As RampCollection, ByVal StartValue As Integer)
        Dim ln As String
        Dim aRamp As Ramp
        Dim idx As Integer
        Dim aDenom As Single

        If myRamps.GetRampIndex(name) >= 0 And doneRamps.GetRampIndex(name) < 0 Then
            aRamp = myRamps.Item(myRamps.GetRampIndex(name))
            doneRamps.Add(aRamp)
            ln = "&RAMP "
            PrintLine(IO, ln)
            ln = " ID = '" + aRamp.Name + "' "
            PrintLine(IO, ln)
            ln = " TYPE = '" + aRamp.Type + "' "
            If aRamp.Type = "HRR" Then
                aDenom = 1000.0
            Else
                aDenom = 1.0
            End If
            PrintLine(IO, ln)
            If aRamp.DimF >= StartValue Then
                ln = " F = " + (aRamp.F(StartValue) / aDenom).ToString
                If aRamp.DimF > StartValue Then
                    For idx = StartValue + 1 To aRamp.DimF
                        ln = ln + " , " + (aRamp.F(idx) / aDenom).ToString
                    Next
                End If
                PrintLine(IO, ln)
            End If
            If aRamp.DimX >= StartValue Then
                If aRamp.IsT Then
                    ln = " T = " + aRamp.X(StartValue).ToString
                Else
                    ln = " Z = " + aRamp.X(StartValue).ToString
                End If
                If aRamp.DimX > StartValue Then
                    For idx = StartValue + 1 To aRamp.DimX
                        ln = ln + " , " + aRamp.X(idx).ToString
                    Next
                End If
                PrintLine(IO, ln)
            End If
            ln = " / "
            PrintLine(IO, ln)
        End If

    End Sub
#End Region
#Region "Support Routines"
    Public Sub AddHeadertoOutput(ByRef csv As CSVsheet, ByRef i As Integer, ByVal header As String)
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!" + header
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
    End Sub
    Public Function SkipLine(ByVal str As String) As Boolean
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
    Public Function HeaderComment(ByVal str As String) As Boolean
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
    Public Function Comment(ByVal str As String) As Boolean
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
    Public Function DropComment(ByVal str As String) As Boolean
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

Imports System
Imports System.IO
Module IO
#Region "Read Routines"
    Public Sub ReadInputFile(ByVal Filename As String)
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
                    compa.SetPosition(csv.num(i, compaNum.AbsXPos), csv.num(i, compaNum.AbsYPos), _
                            csv.num(i, compaNum.FlrHeight))
                    compa.SetMaterial(csv.str(i, compaNum.CeilingMat), csv.str(i, compaNum.WallMat), _
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
                    aDetect.SetPosition(csv.num(i, targetNum.xPosition), csv.num(i, targetNum.yPosition), _
                        csv.num(i, targetNum.zPosition), csv.num(i, targetNum.xNormal), csv.num(i, targetNum.yNormal), _
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
                            hvent.Face = csv.str(i, hventNum.face)
                            hvent.Offset = csv.num(i, hventNum.hall1)
                            hvent.InitialOpening = csv.num(i, hventNum.initialfraction)
                            hvent.FinalOpening = csv.num(i, hventNum.initialfraction) ' This is the default; it may be changed by an EVENT specification
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
                        Else
                            ' This is the old format that is just time and partially implemented in EVENT keyword
                            mvent.InitialOpening = csv.num(i, mventNum.initialfraction)
                            mvent.FinalOpening = csv.num(i, mventNum.initialfraction) ' This is the default; it may be changed by an EVENT specification
                        End If
                        mvent.Changed = False
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
                        vvent.SetVent(csv.num(i, vventNum.firstcompartment) - 1, csv.num(i, vventNum.secondcompartment) - 1,
                                    csv.num(i, vventNum.area), csv.num(i, vventNum.shape))
                        If csv.num(i, 0) > 6 Then ' New format that allows more than one VVENT per compartment pair
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
                                vvent.OpenType = Vent.OpenbyTime
                                vvent.InitialOpeningTime = 0    ' This is the default; it may be changed by an EVENT specification
                                vvent.InitialOpening = csv.num(i, vventNum.intialfraction)
                                vvent.FinalOpeningTime = 0
                                vvent.FinalOpening = csv.num(i, vventNum.intialfraction)
                            End If
                        Else ' Old format that does not include vent number and thus only one per compartment pair
                            vvent.InitialOpeningTime = 0    ' This is the default; it may be changed by an EVENT specification
                            vvent.InitialOpening = csv.num(i, vventNum.intialfraction - 1)
                            vvent.FinalOpeningTime = 0
                            vvent.FinalOpening = csv.num(i, vventNum.intialfraction - 1)
                        End If
                        vvent.Changed = False
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
                                aTarget.SetPosition(aFireObject.XPosition, aFireObject.YPosition, aFireObject.ZPosition, csv.num(iFire, fireNum.xNormal), csv.num(iFire, fireNum.yNormal), csv.num(iFire, fireNum.zNormal))
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
                        System.Windows.Forms.MessageBox.Show("Internal Error (User should not see this). FIRE input line not first line in fire specification.")
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
            csv.num(i, fireNum.zposition) = aFire.ZPosition
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
        If myDetectors.Count > 0 Or myTargets.Count > 0 Then AddHeadertoOutput(csv, i, "Targets and detectors")
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
        If dataFileComments.Count > 0 Then AddHeadertoOutput(csv, i, "comments and ignored keywords")
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

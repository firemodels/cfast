Imports System
Imports System.IO
Module IO
#Region "Read Routines"
    Public Sub ReadInputFile(ByVal Filename As String)
        Dim IO As Integer = 1
        Dim str As String

        FileOpen(IO, Filename, OpenMode.Input, OpenAccess.Read, OpenShare.Shared)
        str = LineInput(IO)
        FileClose(IO)
        If str.Substring(0, 1) = "&" Then
            ReadInputFileNML(Filename)
        ElseIf str.Substring(0, 5) = "VERSN" Then
            ReadInputFileCSV(Filename)
        End If
    End Sub
    Private Sub ReadInputFileCSV(ByVal Filename As String)
        'Read in a *.in file with the CSV format. Filename is to include path as well as file name
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
                    Case "ADIAB"
                        NewFileFormat = True
                End Select
            End If
            i += 1
        Loop
        If Not NewFileFormat Then
            System.Windows.Forms.MessageBox.Show("CFAST input file may be from an older version that cannot be read correctly. Thermal properties and fires will not be imported. Expect additional error messages and check imported file carefully.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If

        ' Get thermal properties from the input file
        FindThermalProperties(Filename, myThermalProperties)

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
                        myErrors.Add("Target " + (myTargets.Count + 1).ToString + " specified no longer supported thermmaly thin Dump. Converted to thermally thick", ErrorMessages.TypeWarning)
                    Else ' PDE
                        type = 0
                    End If
                    aDetect.SetTarget(csv.num(i, targetNum.compartment) - 1, csv.str(i, targetNum.material), type)
                    Dim thickness As Single = myThermalProperties(myThermalProperties.GetIndex(aDetect.Material)).Thickness
                    If (csv.str(i, targetNum.internalLocation) <> "") Then
                        aDetect.InternalLocation = Math.Max(0, Math.Min(csv.num(i, targetNum.internalLocation) * thickness, thickness))
                    Else
                        aDetect.InternalLocation = thickness / 2
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
                            aDetect.ActivationObscurationFlaming = csv.num(i, detectNum.ActivationObscuration)
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
                        myEnvironment.Changed = False
                    Case "EVENT"
                    Case "FIRE"
                        Dim aFire As New Fire
                        Dim aFireInstance As New Fire
                        aFireInstance.ObjectType = Fire.TypeInstance
                        ReadEmbeddedFire(csv, i, aFire, aFireInstance)
                        aFire.Changed = False
                        myFireProperties.Add(aFire)
                        aFireInstance.Changed = False
                        myFires.Add(aFireInstance)
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
                        'myFireProperties.Add(aFire)
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
                        If myFireProperties.GetFireIndex(csv.str(i, objfireNum.name)) < 0 Then
                            FireFile = csv.str(i, objfireNum.name) + ".o"
                            readFires(FireFile, InsertDataType.ObjectFile)
                        End If
                        ' If we didn't find it there, look in the CFAST bin directory
                        If myFireProperties.GetFireIndex(csv.str(i, objfireNum.name)) < 0 Then
                            FireFile = Application.StartupPath + "\" + csv.str(i, objfireNum.name) + ".o"
                            readFires(FireFile, InsertDataType.ObjectFile)
                        End If
                        If myFireProperties.GetFireIndex(csv.str(i, objfireNum.name)) >= 0 Then
                            Dim aFire As New Fire
                            aFire.Name = csv.str(i, objfireNum.name)
                            aFire.SetPosition(csv.num(i, objfireNum.compartment) - 1, csv.num(i, objfireNum.xPosition),
                                csv.num(i, objfireNum.yPosition), csv.num(i, objfireNum.zposition))
                            aFire.PlumeType = csv.num(i, objfireNum.plumeType) - 1
                            aFire.IgnitionType = csv.num(i, objfireNum.ignType) - 1
                            aFire.IgnitionValue = csv.num(i, objfireNum.ignCriterion)
                            aFire.Changed = False
                            myFireProperties.Add(aFire)
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
    Private Sub ReadInputFileNML(ByVal Filename As String)
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
        ReadInputFileNMLDump(NMList, myDumps)
    End Sub
    Private Sub ReadInputFileNMLHead(ByVal NMList As NameListFile, ByRef someEnvironment As Environment)
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
    Private Sub ReadInputFileNMLTime(ByVal NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j As Integer
        Dim print, sim, smoke, ss As Single

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
    Private Sub ReadInputFileNMLInit(ByVal NMList As NameListFile, ByRef someEnvironment As Environment)
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
    Private Sub ReadInputFileNMLMisc(ByVal NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j, max As Integer
        Dim adiabatic, overwrite As Boolean
        Dim maxts, loxyl, extinctionFlaming, extinctionSmoldering As Single

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
    Private Sub ReadInputFileNMLMatl(ByVal NMList As NameListFile, ByRef someThermalProperties As ThermalPropertiesCollection)
        Dim i, j As Integer
        Dim conduct, dens, emiss, spech, thick As Single
        Dim id, matl, fyi As String
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
    Private Sub ReadInputFileNMLRamp(ByVal NMList As NameListFile)
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
                            myErrors.Add("In RAMP namelist for F input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
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
                            myErrors.Add("In RAMP namelist for T input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
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
                            myErrors.Add("In RAMP namelist for Z input must be 1 to " + maxnum + " positive numbers", ErrorMessages.TypeFatal)
                        End If
                        isT = False
                        If def1 Then
                            def2 = True
                        Else
                            def1 = True
                        End If
                    Else
                        myErrors.Add("In RAMP namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                If def2 Then
                    myErrors.Add("In RAMP namelist " + id + " has both t and z defined. Only one can be defined", ErrorMessages.TypeFatal)
                ElseIf def1 And deff Then
                    If Ramp.ValidRamp(id, type, x, f) Then
                        If type = "HRR" Then
                            For k = 0 To f.GetUpperBound(0)
                                f(k) = f(k) * 1000.0
                            Next
                        End If
                        myRamps.Add(New Ramp(id, type, x, f, isT))
                    Else
                        myErrors.Add("In RAMP namelist " + id + " id not a valid ramp definition", ErrorMessages.TypeFatal)
                    End If
                Else
                    myErrors.Add("In RAMP namelist " + id + " id not a valid ramp definition", ErrorMessages.TypeFatal)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLComp(ByVal NMList As NameListFile, ByRef someCompartments As CompartmentCollection)
        Dim i, j, k, max As Integer
        Dim ceilid, wallid, floorid, rampid, id, fyi As String
        Dim depth, height, width As Single
        Dim shaft, hall, valid As Boolean
        Dim grid(3) As Integer
        Dim origin(3), leaks(2), comparea(0), compheight(0) As Single

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
                fyi = ""
                ReDim comparea(0), compheight(0)
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
                        max = NMList.ForNMListVarNumVal(i, j)
                        If max >= 2 And max <= 2 Then
                            leaks(1) = NMList.ForNMListVarGetNum(i, j, 1)
                            leaks(2) = NMList.ForNMListVarGetNum(i, j, 2)
                        Else
                            myErrors.Add("In COMP namelist for LEAK_AREA input must be 2 positive numbers", ErrorMessages.TypeFatal)
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
                    If myThermalProperties.GetIndex(ceilid) < 0 And ceilid <> "OFF" Then
                        ceilid = "OFF"
                        myErrors.Add("In COMP namelist " + id + " CEILING_MATL_ID " + ceilid + " is not valid switching ceiling to OFF", ErrorMessages.TypeWarning)
                    End If
                    If myThermalProperties.GetIndex(wallid) < 0 And wallid <> "OFF" Then
                        wallid = "OFF"
                        myErrors.Add("In COMP namelist " + id + " WALL_MATL_ID " + wallid + " is not valid switching wall to OFF", ErrorMessages.TypeWarning)
                    End If
                    If myThermalProperties.GetIndex(floorid) < 0 And floorid <> "OFF" Then
                        floorid = "OFF"
                        myErrors.Add("In COMP namelist " + id + "FLOOR_MATL_ID " + floorid + " is not valid switching floor to OFF", ErrorMessages.TypeWarning)
                    End If
                    aComp.SetPosition(origin(LocationNum.x), origin(LocationNum.y), origin(LocationNum.z))
                    aComp.SetMaterial(ceilid, wallid, floorid)
                    If comparea.GetUpperBound(0) > 0 Then
                        aComp.SetVariableArea(comparea, compheight)
                    End If
                    If hall Then
                        aComp.Hall = True
                    End If
                    If shaft Then
                        aComp.Shaft = True
                    End If
                    If leaks(1) >= 0 And leaks(2) >= 0 Then
                        aComp.WallLeak = leaks(1)
                        aComp.FloorLeak = leaks(2)
                    Else
                        myErrors.Add("In COMP namelist for LEAK_AREA input must be 2 positive numbers", ErrorMessages.TypeFatal)
                    End If
                    aComp.FYI = fyi
                    aComp.Changed = False
                Else
                    myErrors.Add("In COMP namelist " + id + " is not fully defined", ErrorMessages.TypeWarning)
                End If
            End If
        Next

    End Sub
    Private Sub ReadInputFileNMLDevc(ByVal NMList As NameListFile, ByRef someDetectors As TargetCollection)
        Dim i, j, k, max As Integer
        Dim compid, matlid, id, type, fyi As String
        Dim tempdepthunits As String = "FRACTION"
        Dim tempdepth, rti, setp, setps(2), sprayd As Single
        Dim loc(3), norm(3), coeffs(2) As Single
        Dim valid, lvalid, adiabatic As Boolean
        Dim aTempOffset As Single = 273.15

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
                    ElseIf NMList.ForNMListGetVar(i, j) = "DEPTH_UNITS" Then
                        tempdepthunits = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "TEMPERATURE_DEPTH" Then
                        tempdepth = NMList.ForNMListVarGetNum(i, j, 1)
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
                            myErrors.Add("In DEVC namelist for LOCATION input must be 3 positive numbers", ErrorMessages.TypeFatal)
                        End If
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
                lvalid = True
                For k = 0 To 2
                    If loc(k) < 0 Then
                        lvalid = False
                    End If
                Next
                If Not lvalid Then
                    myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because at least one of the components of the location has not been set", ErrorMessages.TypeFatal)
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
                        If tempdepthunits = "FRACTION" Then
                            Dim MaterialIndex As Integer = myThermalProperties.GetIndex(matlid)
                            If MaterialIndex >= 0 Then
                                Dim thickness As Single = myThermalProperties(MaterialIndex).Thickness
                                aDetect.InternalLocation = Math.Max(0, Math.Min(tempdepth * thickness, thickness))
                            Else
                                aDetect.InternalLocation = tempdepth
                                myErrors.Add("DEVC namelist " + id + " is not a valid DEVC because material ID has not been set", ErrorMessages.TypeFatal)
                            End If
                        Else
                            aDetect.InternalLocation = tempdepth
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
    Private Sub ReadInputFileNMLFire(ByVal NMList As NameListFile, ByRef someFireInstances As FireCollection)
        Dim i, j, k, max As Integer
        Dim compid, id, devcid, ignitcrit, fireid, fyi As String
        Dim setp As Single
        Dim loc(1) As Single
        Dim aDummy As Single = 273.15

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
    Private Sub ReadInputFileNMLChem(ByVal NMList As NameListFile, ByRef someFires As FireCollection)
        Dim i, j As Integer
        Dim id As String
        Dim carbon, chlorine, flametime, hoc, hydrogen, nitrogen, oxygen, radfrac As Single
        Dim aFireCurves(12, 0) As Single
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
    Private Sub ReadInputFileNMLTabl(ByVal NMList As NameListFile, ByVal id As String, ByRef aFireCurves(,) As Single, ByRef Valid As Boolean)
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
    Private Sub ReadInputFileNMLVent(ByVal NMList As NameListFile, ByRef someHVents As VentCollection, ByRef someMVents As VentCollection, ByRef someVVents As VentCollection)
        Dim i, j, k, max As Integer
        Dim area, areas(2), bot, cutoffs(2), flow, heights(2), offset, offsets(2), top, width, setp, prefrac, postfrac, filttime, filteff As Single
        Dim tt(0), ff(0) As Single
        Dim compids(2), filtramp, openramp, face, orien(2), shape, type, id, crit, devcid, fyi As String
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
                fyi = ""
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
                    ElseIf NMList.ForNMListGetVar(i, j) = "OPENING_RAMP_ID" Then
                        openramp = NMList.ForNMListVarGetStr(i, j, 1)
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
                            aVent.RampID = openramp
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
    Private Sub ReadInputFileNMLConn(ByVal NMList As NameListFile, ByRef someHHeats As VentCollection, ByRef someVHeats As VentCollection)
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
    Private Sub ReadInputFileNMLISOF(ByVal NMList As NameListFile, ByRef someVisuals As VisualCollection)
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
    Private Sub ReadInputFileNMLSLCF(ByVal NMList As NameListFile, ByRef someVisuals As VisualCollection)
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
    Public Sub ReadInputFileNMLDiag(ByVal NMList As NameListFile, ByRef someEnvironment As Environment)
        Dim i, j, k, max As Integer
        Dim f(0), t(0) As Single
        Dim ppco2, pph2o, gastemp, ULThickness, verificationstep, fireheatflux As Single
        Dim radsolv As String
        Dim fire, hflow, entrain, vflow, cjet, dfire, convec, rad, gasabsorp, conduc, debugprn, mflow, keyin, steadyint, dasslprn, oxygen As Integer
        Dim residdbprn, layermixing As Integer
        Dim dummy As String
        Dim adiabatic As Boolean, fluxAST As Single

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
    Private Sub ReadInputFileNMLDump(ByVal NMList As NameListFile, ByRef someDumps As DumpCollection)
        Dim i, j As Integer
        Dim id, filetype, type, firstmeasurement, secondmeasurement, firstdevice, seconddevice, fyi As String
        Dim criteria As Single

        For i = 1 To NMList.TotNMList
            id = ""
            filetype = ""
            type = ""
            criteria = 0
            firstmeasurement = ""
            firstdevice = ""
            secondmeasurement = ""
            seconddevice = ""
            fyi = ""
            If (NMList.GetNMListID(i) = "DUMP") Then
                For j = 1 To NMList.ForNMListNumVar(i)
                    If (NMList.ForNMListGetVar(i, j) = "ID") Then
                        id = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FILE_TYPE") Then
                        filetype = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "TYPE") Then
                        type = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "CRITERIA") Then
                        criteria = NMList.ForNMListVarGetNum(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRST_DEVICE") Then
                        firstdevice = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "FIRST_MEASUREMENT") Then
                        firstmeasurement = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SECOND_DEVICE") Then
                        seconddevice = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf (NMList.ForNMListGetVar(i, j) = "SECOND_MEASUREMENT") Then
                        secondmeasurement = NMList.ForNMListVarGetStr(i, j, 1)
                    ElseIf NMList.ForNMListGetVar(i, j) = "FYI" Then
                        fyi = NMList.ForNMListVarGetStr(i, j, 1)
                    Else
                        myErrors.Add("In DUMP namelist " + NMList.ForNMListGetVar(i, j) + " is not a valid parameter", ErrorMessages.TypeFatal)
                    End If
                Next
                someDumps.Add(New Dump(id, filetype, type, criteria, firstmeasurement, firstdevice, secondmeasurement, seconddevice, fyi))
            End If
        Next
    End Sub
    Public Sub ReadINIInput(ByRef x As Integer, ByVal label As String, ByVal value As String)
        If value = "ON" Then
            x = Environment.DIAGon
        ElseIf value = "OFF" Then
            x = Environment.DIAGoff
        Else
            myErrors.Add("In DIAG namelist for " + label + " " + value + " Is Not a valid parameter", ErrorMessages.TypeFatal)
        End If
    End Sub
    Public Sub ReadThermalProperties(ByVal FileName As String, SomeThermalProperties As ThermalPropertiesCollection)
        'Simple read of only thermal properties from a file. 
        FindThermalProperties(FileName, SomeThermalProperties)
        SomeThermalProperties.FileName = FileName
        SomeThermalProperties.FileChanged = False
    End Sub
    Public Sub FindThermalProperties(ByVal Filename As String, ByRef SomeThermalProperties As ThermalPropertiesCollection)
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
    Public Sub FindThermalPropertiesNML(ByVal Filename As String, ByRef SomeThermalProperties As ThermalPropertiesCollection)
        Dim NMList As New NameListFile(Filename)
        ReadInputFileNMLMatl(NMList, SomeThermalProperties)
    End Sub
    Public Sub FindThermalPropertiesCSV(ByVal csv As CSVsheet, ByRef SomeThermalProperties As ThermalPropertiesCollection)
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
    Public Sub FindaThermalProperty(ByVal Filename As String, ByVal Material As String, ByRef aThermalPropery As ThermalProperty)
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
    Private Sub readFires(ByVal Filename As String, FileType As Integer)
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
    Public Sub FindFires(ByVal FileType As Integer, ByVal Filename As String)
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
    Public Sub FindFiresNML(ByVal FileType As Integer, ByVal Filename As String)
        Dim NMList As New NameListFile(Filename)

        ReadInputFileNMLChem(NMList, TempFires)
    End Sub
    Public Sub FindFiresCSV(ByVal FileType As Integer, ByVal Filename As String)
        'simple read of a fire object file
        Dim csv As New CSVsheet(Filename)
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
    Private Sub ReadEmbeddedFire(ByVal csv As CSVsheet, ByVal iStart As Integer, ByRef aFire As Fire, ByRef aFireInstance As Fire)
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
                        Dim aFireObject As New Fire(), aFireIns As New Fire()
                        Dim aThermalProperty As New ThermalProperty()
                        aFireObject.Name = csv.str(iFire, fireNum.name).Trim + "_Fire"
                        aFireIns.Name = csv.str(iFire, fireNum.name)
                        aFireIns.ReferencedFireDefinition = aFireObject.Name
                        aFireObject.ChemicalFormula(formula.C) = csv.num(iChemie, chemieNum.C)
                        aFireObject.ChemicalFormula(formula.H) = csv.num(iChemie, chemieNum.H)
                        aFireObject.ChemicalFormula(formula.O) = csv.num(iChemie, chemieNum.O)
                        aFireObject.ChemicalFormula(formula.N) = csv.num(iChemie, chemieNum.N)
                        aFireObject.ChemicalFormula(formula.Cl) = csv.num(iChemie, chemieNum.Cl)
                        aFireObject.HeatofCombustion = csv.num(iChemie, chemieNum.HoC)
                        index = myThermalProperties.GetIndex(csv.str(iChemie, chemieNum.Material))

                        aFireObject.RadiativeFraction = csv.num(iChemie, chemieNum.chiR)
                        aFireIns.SetPosition(csv.num(iFire, fireNum.compartment) - 1, csv.num(iFire, fireNum.xPosition),
                            csv.num(iFire, fireNum.yPosition), csv.num(iFire, fireNum.zposition))
                        Dim z_position As Single
                        z_position = csv.num(iFire, fireNum.zposition)
                        aFireObject.PlumeType = csv.num(iFire, fireNum.plumeType) - 1
                        If InStr(IgnitionTypes, csv.str(iFire, fireNum.ignType), CompareMethod.Text) > 0 Then
                            ' if it's the new format, ignition is just linked to an existing target
                            aFireIns.IgnitionType = InStr(IgnitionTypes, csv.str(iFire, fireNum.ignType), CompareMethod.Text) / 4
                            aFireIns.Target = csv.str(iFire, fireNum.ignTarget)
                        Else
                            ' if it's the old format, create a target just for the fire
                            aFireIns.IgnitionType = csv.num(iFire, fireNum.ignType) - 1
                            If aFireIns.IgnitionType <> Fire.FireIgnitionbyTime Then
                                Dim aTarget As New Target
                                aTarget.Type = Target.TypeTarget
                                aTarget.Name = "Ign_" + aFireObject.Name
                                aTarget.SetPosition(aFireObject.XPosition, aFireObject.YPosition, aFireObject.Height, csv.num(iFire, fireNum.xNormal), csv.num(iFire, fireNum.yNormal), csv.num(iFire, fireNum.zNormal))
                                aTarget.SetTarget(aFireObject.Compartment, csv.str(iChemie, chemieNum.Material), Target.ThermallyThick)
                                myTargets.Add(aTarget)
                                aFireIns.Target = aTarget.Name
                            End If
                        End If
                        aFireIns.IgnitionValue = csv.num(iFire, fireNum.ignCriterion)

                        Dim firedata(12, CInt(csv.num(iTime, 0) - 2)) As Single

                        For j = 0 To csv.num(iTime, 0) - 2
                            For k = 1 To NumFireCurves
                                firedata(FireCurveColumns(k), j) = csv.num(iTime + k - 1, j + 2)
                                If FireCurveColumns(k) = Fire.FireHeight Then firedata(FireCurveColumns(k), j) += z_position
                            Next
                            firedata(Fire.FireMdot, j) = firedata(Fire.FireHRR, j) / aFireObject.HeatofCombustion
                            firedata(Fire.FireHC, j) = aFireObject.ChemicalFormula(formula.H) * 1.00794 / (aFireObject.ChemicalFormula(formula.C) * 12.0107)
                            If aFireObject.ChemicalFormula(formula.N) <> 0 Then firedata(Fire.FireHCN, j) = (1.00794 + 12.0107 + 14.01) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.N)
                            If aFireObject.ChemicalFormula(formula.Cl) <> 0 Then firedata(Fire.FireHCl, j) = (1.00794 + 35.453) / 1000.0 / aFireObject.MolarMass * aFireObject.ChemicalFormula(formula.Cl)
                        Next
                        aFireObject.SetFireData(firedata)
                        aFireObject.Changed = False
                        aFire = aFireObject
                        aFireIns.Changed = False
                        aFireInstance = aFireIns
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
    Public Sub WriteInputFileCSV(ByVal FileName As String)
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
        csv.num(i, ambNum.refHeight) = 0
        i += 1
        'Interior ambient conditions
        csv.str(i, CFASTlnNum.keyWord) = "TAMB"
        csv.num(i, ambNum.ambTemp) = myEnvironment.IntAmbTemperature
        csv.num(i, ambNum.ambPress) = myEnvironment.IntAmbPressure
        csv.num(i, ambNum.refHeight) = 0
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
                csv.num(i, rampNum.ventnumber) = j + 1
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
        If myFireProperties.Count > 0 Then AddHeadertoOutput(csv, i, "Fires")

        If myEnvironment.LowerOxygenLimit <> Environment.DefaultLOI Then
            csv.str(i, CFASTlnNum.keyWord) = "LIMO2"
            csv.num(i, fireNum.limo2) = myEnvironment.LowerOxygenLimit
            i += 1
        End If

        Dim aFire As New Fire, firedata(12, 0) As Single, numFireDataPoints As Integer
        For j = 0 To myFireProperties.Count - 1
            aFire = myFireProperties.Item(j)

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
                csv.num(i, detectNum.ActivationObscuration) = aDetect.ActivationObscurationFlaming
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
    Public Sub WriteInputFileNML(ByVal filename As String)
        Dim IO As Integer = 1
        Dim ln As String, field As String
        Dim i, j, k, l As Integer
        Dim aFlag As Boolean
        Dim x(0), f(0) As Single
        Dim aFireCurves(12, 0) As Single
        Dim aThermalProperty As New ThermalProperty
        Dim aComp As Compartment
        Dim aTarg As Target
        Dim aVent As Vent
        Dim aFire As Fire
        Dim aVisual As Visual
        Dim aDump As Dump

        FileOpen(IO, filename, OpenMode.Output, OpenAccess.Write, OpenShare.Shared)

        'Writing HEAD namelist
        ln = "&HEAD VERSION = " + myEnvironment.Version.ToString + ", " + "TITLE = " + "'" + myEnvironment.Title + "' /"
        PrintLine(IO, ln)
        PrintLine(IO, " ")
        ln = "!! Scenario Configuration "
        PrintLine(IO, ln)

        'Writing TIME namelist
        ln = "&TIME SIMULATION = " + myEnvironment.SimulationTime.ToString + " PRINT = " + myEnvironment.OutputInterval.ToString + " SMOKEVIEW = " + myEnvironment.SmokeviewInterval.ToString + " SPREADSHEET = " + myEnvironment.SpreadsheetInterval.ToString + " / "
        PrintLine(IO, ln)

        'Writing INIT namelist
        ln = "&INIT " + "PRESSURE = " + myEnvironment.ExtAmbPressure.ToString + " RELATIVE_HUMIDITY = " + myEnvironment.IntAmbRH.ToString
        ln += " INTERIOR_TEMPERATURE = " + Math.Round((myEnvironment.IntAmbTemperature - 273.15), 2).ToString
        ln += " EXTERIOR_TEMPERATURE = " + Math.Round((myEnvironment.ExtAmbTemperature - 273.15), 2).ToString + " /"
        PrintLine(IO, ln)

        'Writing MISC namelist
        If myEnvironment.AdiabaticWalls Or (myEnvironment.MaximumTimeStep <> Environment.DefaultMaximumTimeStep And myEnvironment.MaximumTimeStep > 0.0) Or myEnvironment.LowerOxygenLimit <> 0.15 Or myEnvironment.FlamingExtinctionCoefficient <> 8700 Or myEnvironment.SmolderingExtinctionCoefficient <> 4400 Or myEnvironment.Overwrite <> True Then
            ln = "&MISC "
            aFlag = True
        Else
            aFlag = False
        End If
        If myEnvironment.AdiabaticWalls <> False Then
            ln += " ADIABATIC = .TRUE. "
        End If
        If myEnvironment.MaximumTimeStep <> Environment.DefaultMaximumTimeStep And myEnvironment.MaximumTimeStep > 0 Then
            ln += " MAX_TIME_STEP = " + myEnvironment.MaximumTimeStep.ToString
        End If
        If myEnvironment.LowerOxygenLimit <> 0.15 Then
            ln += " LOWER_OXYGEN_LIMIT = " + myEnvironment.LowerOxygenLimit.ToString
        End If
        If myEnvironment.FlamingExtinctionCoefficient <> 8700 Or myEnvironment.SmolderingExtinctionCoefficient <> 4400 Then
            ln += " SPECIFIC_EXTINCTION = " + myEnvironment.FlamingExtinctionCoefficient.ToString + ", " + myEnvironment.SmolderingExtinctionCoefficient.ToString
        End If
        If myEnvironment.Overwrite <> True Then
            ln += " OVERWRITE = .FALSE."
        End If
        If aFlag Then
            ln += " / "
            PrintLine(IO, ln)
        End If
        myEnvironment.Changed = False

        'Writing Diagnostics 
        Dim wrtDIAG As Boolean
        Dim wrtSlash As Boolean
        If myEnvironment.DIAGRadSolver = "DEFAULT" Then
            wrtDIAG = True
            wrtSlash = False
        Else
            wrtDIAG = False
            wrtSlash = True
            ln = "&DIAG  RADSOLVER = '" + myEnvironment.DIAGRadSolver + "' "
            PrintLine(IO, ln)
        End If
        If myEnvironment.DIAGAdiabaticTargetVerification = True Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "ADIABATIC_TARGET_VERIFICATION = 'ON' RADIATIVE_INCIDENT_FLUX = " + myEnvironment.DIAGAdiabaticTargetFlux.ToString
            Print(IO, ln)
        End If
        If myEnvironment.DIAGUpperLayerThickness <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "UPPER_LAYER_THICKNESS = " + myEnvironment.DIAGUpperLayerThickness.ToString
            Print(IO, ln)
        End If
        If myEnvironment.DIAGFireHeatFlux <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "VERIFICATION_FIRE_HEAT_FLUX = " + myEnvironment.DIAGFireHeatFlux.ToString
            Print(IO, ln)
        End If
        If myEnvironment.DIAGVerificationTimeStep <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "VERIFICATION_TIME_STEP = " + myEnvironment.DIAGVerificationTimeStep.ToString
            Print(IO, ln)
        End If
        If myEnvironment.DIAGGasTemp <> Environment.DefaultNonValue Then
            If wrtDIAG Then
                ln = "&DIAG "
                wrtDIAG = False
                wrtSlash = True
            Else
                ln = " "
            End If
            ln += "GAS_TEMPERATURE = " + Math.Round(myEnvironment.DIAGGasTemp, 2).ToString
            ln += " PARTIAL_PRESSURE_H2O = " + Math.Round(myEnvironment.DIAGPartPressH2O, 2).ToString
            ln += " PARTIAL_PRESSURE_CO2 = " + Math.Round(myEnvironment.DIAGPartPressCO2, 2).ToString
            Print(IO, ln)
        End If
        myEnvironment.GetDIAGf(f)
        myEnvironment.GetDIAGt(x)
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
                For k = 1 To numpoints
                    ln += ", " + x(k).ToString
                Next
                PrintLine(IO, ln)
                ln = "      F = " + f(0).ToString
                For k = 1 To numpoints
                    ln += ", " + f(k).ToString
                Next
                PrintLine(IO, ln)
            End If
        End If
        If myEnvironment.DIAGfire <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "FIRE_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGhflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "HORIZONTAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGentrain <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "ENTRAINMENT_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGvflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "VERTICAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGcjet <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CEILING_JET_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGdfire <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DOOR_JET_FIRE_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGconvec <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CONVECTION_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGrad <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "RADIATION_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGgasabsorp <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "GAS_ABSORBTION_SUB_MODEL = 'CONSTANT' ")
        End If
        If myEnvironment.DIAGconduc <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "CONDUCTION_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGdebugprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DEBUG_PRINT = 'ON' ")
        End If
        If myEnvironment.DIAGmflow <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "MECHANICAL_FLOW_SUB_MODEL = 'OFF' ")
        End If
        If myEnvironment.DIAGkeyin <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "KEYBOARD_INPUT = 'OFF' ")
        End If
        If myEnvironment.DIAGsteadyint <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "STEADY_STATE_INITIAL_CONDITIONS = 'ON' ")
        End If
        If myEnvironment.DIAGdasslprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "DASSL_DEBUG_PRINT = 'ON' ")
        End If
        If myEnvironment.DIAGoxygen <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "OXYGEN_TRACKING = 'ON' ")
        End If
        If myEnvironment.DIAGresiddbprn <> Environment.DIAGoff Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "RESIDUAL_DEBUG_PRINT = 'ON' ")
        End If
        If myEnvironment.DIAGlayermixing <> Environment.DIAGon Then
            WriteDIAGsimpleln(IO, wrtDIAG, wrtSlash, "LAYER_MIXING_SUB_MODEL = 'OFF' ")
        End If
        If wrtSlash Then
            ln = "/ "
            PrintLine(IO, ln)
        End If

        'Writing MATL namelist
        If myThermalProperties.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Material Properties "
            PrintLine(IO, ln)

            For i = 0 To myThermalProperties.Count - 1
                aThermalProperty = myThermalProperties.Item(i)
                If myThermalProperties.NumberofConnections(aThermalProperty.ShortName) > 0 Then
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

        ' Writing COMP namelist
        If myCompartments.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Compartments "
            PrintLine(IO, ln)

            For i = 0 To myCompartments.Count - 1
                aComp = myCompartments.Item(i)
                ln = "&COMP " + "ID = '" + aComp.Name + "'"
                PrintLine(IO, ln)
                ln = "      DEPTH = " + aComp.RoomDepth.ToString + " HEIGHT = " + aComp.RoomHeight.ToString + " WIDTH = " + aComp.RoomWidth.ToString
                If aComp.CeilingMaterial <> "" Then
                    If aComp.CeilingMaterial = "Off" Then
                        ln += " CEILING_MATL_ID = 'OFF'"
                    Else
                        ln += " CEILING_MATL_ID = '" + aComp.CeilingMaterial + "'"
                    End If
                End If
                If aComp.WallMaterial <> "" Then
                    If aComp.WallMaterial = "Off" Then
                        ln += " WALL_MATL_ID = 'OFF'"
                    Else
                        ln += " WALL_MATL_ID = '" + aComp.WallMaterial + "'"
                    End If
                End If
                If aComp.FloorMaterial <> "" Then
                    If aComp.FloorMaterial = "Off" Then
                        ln += " FLOOR_MATL_ID = 'OFF'"
                    Else
                        ln += " FLOOR_MATL_ID = '" + aComp.FloorMaterial + "'"
                    End If
                End If
                If aComp.Hall Then
                    ln += " HALL = .TRUE."
                ElseIf aComp.Shaft Then
                    ln += " SHAFT = .TRUE."
                End If
                PrintLine(IO, ln)
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
                    ln += " LEAK_AREA = " + aComp.WallLeak.ToString + ", " + aComp.FloorLeak.ToString
                End If
                If aComp.FYI <> "" Then
                    ln += " FYI = '" + aComp.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aComp.Changed = False
            Next
        End If

        ' Writing VENT namelist for wall vents
        If myHVents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Wall Vents"
            PrintLine(IO, ln)

            For i = 0 To myHVents.Count - 1
                aVent = myHVents.Item(i)
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
                ln += " TOP = " + aVent.Soffit.ToString + ", BOTTOM = " + aVent.Sill.ToString + ", WIDTH = " + aVent.Width.ToString
                PrintLine(IO, ln)
                ln = " "
                If aVent.OpenType = Vent.OpenbyTime Then
                    Dim ff(2), xx(2), numpoints As Single
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints > 1 Then
                        ln += " CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + xx(k).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + ff(k).ToString
                        Next
                    ElseIf aVent.InitialOpening <> 1 Or aVent.FinalOpening <> 1 Then
                        ln += " CRITERION = 'TIME'"
                        ln += " T = " + aVent.InitialOpeningTime.ToString + ", " + aVent.FinalOpeningTime.ToString + " "
                        ln += " F = " + aVent.InitialOpening.ToString + ", " + aVent.FinalOpening.ToString + " "
                    End If
                ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                    ln += " CRITERION = 'TEMPERATURE', SETPOINT = " + Math.Round((aVent.OpenValue - 273.15), 2).ToString + " DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                    ln += " CRITERION = 'FLUX', SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + ", DEVC_ID = '" + aVent.Target + "' "
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                End If
                If aVent.Face = 2 Then
                    ln += " FACE = 'RIGHT'"
                ElseIf aVent.Face = 3 Then
                    ln += " FACE = 'REAR'"
                ElseIf aVent.Face = 4 Then
                    ln += " FACE = 'LEFT'"
                Else
                    ln += " FACE = 'FRONT'"
                End If
                ln += " OFFSET = " + aVent.Offset.ToString
                If aVent.FYI <> "" Then
                    ln += " FYI = '" + aVent.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing VENT namelist for ceiling/floor vents
        If myVVents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Ceiling and Floor Vents "
            PrintLine(IO, ln)

            For i = 0 To myVVents.Count - 1
                aVent = myVVents.Item(i)
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
                    Dim ff(2), xx(2), numpoints As Single
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints >= 1 Then
                        PrintLine(IO, ln)
                        ln = "      CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + xx(k).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + ff(k).ToString
                        Next
                    End If
                ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                    PrintLine(IO, ln)
                    ln = "      CRITERION = 'TEMPERATURE', SETPOINT = " + Math.Round((aVent.OpenValue - 273.15), 2).ToString + ", DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                    PrintLine(IO, ln)
                    ln = " CRITERION = 'FLUX', SETPOINT = " + (aVent.OpenValue / 1000.0).ToString + ", DEVC_ID = '" + aVent.Target + "'"
                    ln += " PRE_FRACTION = " + aVent.InitialOpening.ToString + ", POST_FRACTION = " + aVent.FinalOpening.ToString
                End If
                ln += " OFFSETS = " + aVent.OffsetX.ToString + ", " + aVent.OffsetY.ToString
                If aVent.FYI <> "" Then
                    ln += " FYI = '" + aVent.FYI + "'"
                End If
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing VENT namelist for mechanical vents
        If myMVents.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Mechanical Vents"
            PrintLine(IO, ln)

            For i = 0 To myMVents.Count - 1
                aVent = myMVents.Item(i)
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
                ln += " FLOW = " + aVent.FlowRate.ToString + " CUTOFFS = " + aVent.BeginFlowDropoff.ToString + ", " + aVent.ZeroFlow.ToString
                ln += " OFFSETS = " + aVent.OffsetX.ToString + ", " + aVent.OffsetY.ToString
                If aVent.OpenType = Vent.OpenbyTime Then
                    Dim ff(2), xx(2), numpoints As Single
                    aVent.GetRamp(xx, ff, numpoints)
                    If numpoints > 1 Then
                        PrintLine(IO, ln)
                        ln = "      CRITERION = 'TIME'"
                        ln += " T = " + xx(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + xx(k).ToString
                        Next
                        ln += " F = " + ff(1).ToString
                        For k = 2 To numpoints
                            ln += ", " + ff(k).ToString
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
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing Fires
        If myFires.Count + myFireProperties.Count > 0 Then
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

            For i = 0 To myFireProperties.Count - 1
                If myFires.NumberofInstances(myFireProperties.Item(i).Name) > 0 Then
                    aFire = myFireProperties.Item(i)
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

        ' Writing devices (targets, detectors, sprinklers)
        If myTargets.Count + myDetectors.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Devices"
            PrintLine(IO, ln)

            For i = 0 To myTargets.Count - 1
                aTarg = myTargets.Item(i)
                ln = "&DEVC ID = '" + aTarg.Name + "' COMP_ID = '" + myCompartments.Item(aTarg.Compartment).Name + "'"
                ln += " LOCATION = " + aTarg.XPosition.ToString + ", " + aTarg.YPosition.ToString + ", " + aTarg.ZPosition.ToString
                If aTarg.SolutionType = Target.ThermallyThick Then
                    ln += " TYPE = 'PLATE'"
                Else
                    ln += " TYPE = 'CYLINDER'"
                End If
                ln += " MATL_ID = '" + myThermalProperties.Item(myThermalProperties.GetIndex(aTarg.Material)).ShortName + "' "
                ln += " NORMAL = " + aTarg.XNormal.ToString + ", " + aTarg.YNormal.ToString + ", " + aTarg.ZNormal.ToString
                ln += " TEMPERATURE_DEPTH = " + aTarg.InternalLocation.ToString
                ln += " DEPTH_UNITS = " + "'M'"
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

            For i = 0 To myDetectors.Count - 1
                aTarg = myDetectors.Item(i)
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
                        ln += "  TYPE = 'SMOKE_DETECTOR' SETPOINT = " + aTarg.ActivationObscurationFlaming.ToString + " /"
                    Else
                        ln += "  TYPE = 'SMOKE_DETECTOR' SETPOINTS = " + aTarg.ActivationObscurationSmoldering.ToString + ", " + aTarg.ActivationObscurationFlaming.ToString + " /"
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

        'Writing Surface Connections
        If myHHeats.Count + myVHeats.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Surface Connections"
            PrintLine(IO, ln)

            Dim fracln As String
            For i = 0 To myCompartments.Count - 1
                If myHHeats.FromConnections(i) > 0 Then
                    ln = "&CONN TYPE = 'WALL'"
                    ln += " COMP_ID = '" + myCompartments.Item(i).Name + "' "
                    fracln = "      F = "
                    ln += " COMP_IDS = "
                    For j = 0 To myHHeats.Count - 1
                        aVent = myHHeats.Item(j)
                        If aVent.FirstCompartment = i Then
                            If aVent.SecondCompartment = -1 Then
                                ln += " 'OUTSIDE'"
                            Else
                                ln += "  '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
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

            For i = 0 To myVHeats.Count - 1
                ln = "&CONN"
                aVent = myVHeats.Item(i)
                If aVent.FirstCompartment = -1 Then
                    ln += " TYPE = 'CEILING'"
                Else
                    ln += " TYPE = 'FLOOR'"
                End If
                If aVent.FirstCompartment = -1 Then
                    ln += " COMP_ID = 'OUTSIDE'"
                Else
                    ln += " COMP_ID = '" + myCompartments.Item(aVent.FirstCompartment).Name + "'"
                End If
                If aVent.SecondCompartment = -1 Then
                    ln += " COMP_IDS = 'OUTSIDE' "
                Else
                    ln += " COMP_IDS = '" + myCompartments.Item(aVent.SecondCompartment).Name + "'"
                End If
                ln += " / "
                PrintLine(IO, ln)
                aVent.Changed = False
            Next
        End If

        'Writing Visualizations
        If myVisuals.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Visualizations"
            PrintLine(IO, ln)

            For i = 0 To myVisuals.Count - 1
                aVisual = myVisuals.Item(i)
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

        ' Writing Dumps
        If myDumps.Count > 0 Then
            PrintLine(IO, " ")
            ln = "!! Dumps"
            PrintLine(IO, ln)

            For i = 0 To myDumps.Count - 1
                aDump = myDumps.Item(i)

                ln = "&DUMP ID = '" + aDump.ID + "'"
                PrintLine(IO, ln)
                ln = "     FILE_TYPE = '" + aDump.FileType + "'  TYPE = '" + aDump.Type + "'"
                If aDump.Type <> "MINIMUM" And aDump.Type <> "MAXIMUM" And aDump.Type <> "CHECK_TOTAL_HRR" Then
                    ln += "  CRITERIA = " + aDump.Criteria.ToString
                End If
                PrintLine(IO, ln)
                ln = "     FIRST_DEVICE = '" + aDump.FirstDevice + "'  FIRST_MEASUREMENT = '" + aDump.FirstMeasurement + "'"
                If aDump.Type <> "MINIMUM" And aDump.Type <> "MAXIMUM" And aDump.Type <> "CHECK_TOTAL_HRR" Then
                    ln += "  SECOND_DEVICE = '" + aDump.SecondDevice + "'  SECOND_MEASUREMENT = '" + aDump.SecondMeasurement + "'"
                End If
                If aDump.FYI <> "" Then
                    ln += " FYI = '" + aDump.FYI + "'"
                End If
                ln += " /"
                PrintLine(IO, ln)
            Next
        End If

        FileClose(IO)

    End Sub
    Public Sub WriteDIAGsimpleln(ByVal IO As Integer, ByRef wrtDIAG As Boolean, ByRef wrtSlash As Boolean, ByVal line As String)
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
                        ln = ln + ", " + (aRamp.F(idx) / aDenom).ToString
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
                        ln = ln + ", " + aRamp.X(idx).ToString
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
    Private Sub AddHeadertoOutput(ByRef csv As CSVsheet, ByRef i As Integer, ByVal header As String)
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!" + header
        i += 1
        csv.str(i, CFASTlnNum.keyWord) = "!!"
        i += 1
    End Sub
    Private Function SkipLine(ByVal str As String) As Boolean
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
    Private Function HeaderComment(ByVal str As String) As Boolean
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
    Private Function Comment(ByVal str As String) As Boolean
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
    Private Function DropComment(ByVal str As String) As Boolean
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

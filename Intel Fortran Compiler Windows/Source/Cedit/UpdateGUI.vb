Imports System.Reflection
Imports C1.Win.C1FlexGrid
Public Class UpdateGUI
    Public DoErrorCheck As Boolean = True

    Private MainWin As CeditMain
    Private AreaPoints() As Double, HeightPoints() As Double, OpeningTimes() As Double, OpeningFractions() As Double
    Private NumPoints As Integer, i As Integer, j As Integer, NumCompartments As Integer
    Private NumHVents As Integer, numVVents As Integer, numMVents As Integer, NumVHeats As Integer, NumHHeats As Integer,
    numTargets As Integer, numDetectors As Integer, numHeats As Integer, numFires As Integer, numVisuals As Integer
    Public Sub New(ByVal ParentWindow As Object)
        MainWin = ParentWindow
    End Sub
    Public Sub Menu()
        Dim FileName As String
        If myRecentFiles.Count > 0 Then
            MainWin.MenuRecentSeparator.Visible = True
            Dim i As Integer, Item As MenuItem
            For i = 0 To myRecentFiles.Count - 1
                Item = MainWin.MenuRecent1
                If i = 1 Then Item = MainWin.MenuRecent2
                If i = 2 Then Item = MainWin.MenuRecent3
                If i = 3 Then Item = MainWin.MenuRecent4
                Item.Text = "&" + (i + 1).ToString + " " + System.IO.Path.GetFileName(myRecentFiles.Filenames(i))
                Item.Visible = True
            Next
        End If
        FileName = myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in"
        If System.IO.File.Exists(FileName) Then MainWin.MenuViewInput.Enabled = True
        FileName = myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".out"
        If System.IO.File.Exists(FileName) Then MainWin.MenuViewOutput.Enabled = True
        FileName = myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".log"
        If System.IO.File.Exists(FileName) Then MainWin.MenuViewLog.Enabled = True
    End Sub
    Public Sub General()
        Dim sLen As Integer, aVersion As String
        sLen = Len(Application.ProductVersion)
        aVersion = Application.ProductVersion.Substring(0, sLen - 2)
        If Data.Update = True Then
            MainWin.Text = "CEdit " + aVersion + " (Updating " + System.IO.Path.GetFileName(myEnvironment.InputFileName) + ")"
        Else
            If myEnvironment.InputFileName = Nothing Then
                MainWin.Text = "CEdit " + aVersion + " (Newfile)"
            Else
                MainWin.Text = "CEdit " + aVersion + " (" + System.IO.Path.GetFileName(myEnvironment.InputFileName) + ")"
            End If
        End If
        If myEnvironment.FileChanged Then MainWin.Text = MainWin.Text + " *"
        If DoErrorCheck Then
            ' myErrors.Queue.Clear()
            UpdateErrorCheck()
        End If

        MainWin.OutputShowCFAST.Checked = CommandWindowVisible
        MainWin.OutputValidation.Checked = ValidationOutput
        MainWin.OutputCompartments.Checked = SSOutputCompartments
        MainWin.OutputDevices.Checked = SSOutputDevices
        MainWin.OutputMasses.Checked = SSOutputMasses
        MainWin.OutputVents.Checked = SSOutputVents
        MainWin.OutputWalls.Checked = SSOutputWalls

        Dim OutputOptions As String
        If ValidationOutput Or DebugOutput Or CommandWindowVisible Then
            OutputOptions = "Output: "
            If ValidationOutput Then OutputOptions = OutputOptions + "Validation   "
            If DebugOutput Then OutputOptions = OutputOptions + "Debug   "
            If CommandWindowVisible Then OutputOptions = OutputOptions + "CFAST Window"
            MainWin.StatusBar.Panels(2).Text = OutputOptions
        Else
            MainWin.StatusBar.Panels(2).Text = ""
        End If
    End Sub
    Private Sub UpdateErrorCheck()
        Dim ErrorCount As Integer
        ErrorCount = myEnvironment.IsValid + myThermalProperties.IsValid + myCompartments.IsValid + myHVents.IsValid + myVVents.IsValid + myMVents.IsValid + myDetectors.IsValid +
            myTargets.IsValid + myFireProperties.IsValid + myFires.IsValid + myHHeats.IsValid + myVHeats.IsValid + myVisuals.IsValid
        If ErrorCount > 0 Then
            myErrors.Break(System.IO.Path.GetFileName(myEnvironment.InputFileName))
            MainWin.StatusBar.Panels(0).Text = ErrorCount.ToString + " Errors or Messages: "
            MainWin.StatusBar.Panels(1).Text = myErrors.TopError
        Else
            MainWin.StatusBar.Panels(0).Text = "No Errors"
            MainWin.StatusBar.Panels(1).Text = ""
        End If
    End Sub
#Region "Tab Updates "
    Public Sub Environment()
        General()
        MainWin.EnvTitle.Text = myEnvironment.Title
        MainWin.EnvSimTime.Text = myEnvironment.SimulationTime.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvTextOutInterval.Text = myEnvironment.OutputInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvSpreadOutInterval.Text = myEnvironment.SpreadsheetInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvSmokeviewInterval.Text = myEnvironment.SmokeviewInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvIntAmbTemp.Text = myEnvironment.IntAmbTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
        MainWin.EnvIntAmbRH.Text = myEnvironment.IntAmbRH.ToString + " %"
        MainWin.EnvExtAmbTemp.Text = myEnvironment.ExtAmbTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
        MainWin.EnvExtAmbPress.Text = myEnvironment.ExtAmbPressure.ToString + myUnits.Convert(UnitsNum.Pressure).Units
        MainWin.EnvAdiabatic.Checked = myEnvironment.AdiabaticWalls
        MainWin.EnvLOI.Text = myEnvironment.LowerOxygenLimit.ToString

        Dim value As Double
        value = Val(MainWin.EnvTimeStep.Text)
        If value > 0 Then
            MainWin.EnvTimeStep.Text = value.ToString + myUnits.Convert(UnitsNum.Time).Units
        Else
            MainWin.EnvTimeStep.Text = "Default"
        End If

        MainWin.EnvErrors.Text = ""
        If myErrors.Count > 0 Then
            Dim myEnumerator As System.Collections.IEnumerator = myErrors.Queue.GetEnumerator()
            While myEnumerator.MoveNext()
                MainWin.EnvErrors.Text = myEnumerator.Current + ControlChars.CrLf + MainWin.EnvErrors.Text
            End While
        End If
    End Sub
    Public Sub Thermals(ByVal index As Integer)
        General()
        If myThermalProperties.Count = 0 Then
            ClearGrid(MainWin.ThermalSummary)
            MainWin.GroupThermal.Enabled = False
        Else
            MainWin.GroupThermal.Enabled = True
            If index >= 0 And index < myThermalProperties.Count Then
                MainWin.GroupThermal.Text = "Thermal Property " + index.ToString + " of(" + myThermalProperties.Count.ToString + ")"
                Dim aThermalProperty As New ThermalProperty
                Dim i As Integer
                aThermalProperty = myThermalProperties(index)
                MainWin.ThermalShortName.Text = aThermalProperty.ShortName
                MainWin.ThermalLongName.Text = aThermalProperty.Name
                MainWin.ThermalConductivity.Text = aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
                MainWin.ThermalSpecificHeat.Text = aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
                MainWin.ThermalDensity.Text = aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
                MainWin.ThermalThickness.Text = aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
                MainWin.ThermalEmissivity.Text = aThermalProperty.Emissivity.ToString
                ClearGrid(MainWin.ThermalSummary)
                For i = 1 To myThermalProperties.Count
                    aThermalProperty = myThermalProperties(i - 1)
                    MainWin.ThermalSummary(i, 0) = aThermalProperty.Name
                    MainWin.ThermalSummary(i, 1) = aThermalProperty.ShortName
                    MainWin.ThermalSummary(i, 2) = aThermalProperty.Conductivity.ToString
                    MainWin.ThermalSummary(i, 3) = aThermalProperty.SpecificHeat.ToString
                    MainWin.ThermalSummary(i, 4) = aThermalProperty.Density.ToString
                    MainWin.ThermalSummary(i, 5) = aThermalProperty.Thickness.ToString
                    MainWin.ThermalSummary(i, 6) = aThermalProperty.Emissivity.ToString
                Next
                MainWin.ThermalSummary.Select(index + 1, 0, index + 1, MainWin.ThermalSummary.Cols.Count - 1, True)
            End If

            Dim SaveTargetMaterial As String, SaveFireComp As String
            SaveTargetMaterial = MainWin.TargetMaterial.Text
            SaveFireComp = MainWin.FireComp.Text
            myCompartments.DoChange = False
            myTargets.DoChange = False
            myFireProperties.DoChange = False
            InitThermalPropertyList(MainWin.TargetMaterial)
            MainWin.TargetMaterial.Text = SaveTargetMaterial
            MainWin.FireComp.Text = SaveFireComp
            myCompartments.DoChange = True
            myTargets.DoChange = True
            myFireProperties.DoChange = True
        End If
    End Sub
    Public Sub Visuals(ByVal indexVisual As Integer, ByVal indexCompartment As Integer)
        General()
        If indexVisual < 0 Or myVisuals.Count = 0 Then
            ClearGrid(MainWin.VisualSummary)
            ClearGrid(MainWin.VisualResolutionSummary)
        Else
            ' fill the visualization widgets from the supplied visualization data
            Dim aVisual As Visual
            aVisual = myVisuals.Item(indexVisual)
            If aVisual.Compartment <= myCompartments.Count - 1 Then
                MainWin.VisualizationComp.SelectedIndex = aVisual.Compartment + 1
            End If
            MainWin.VisualizationType.SelectedIndex = aVisual.Type
            If aVisual.Type = Visual.IsoSurface Then
                MainWin.VisualizationValue.Text = aVisual.Value.ToString + myUnits.Convert(UnitsNum.Temperature).Units
            Else
                MainWin.VisualizationValue.Text = aVisual.Value.ToString + myUnits.Convert(UnitsNum.Length).Units
            End If
            MainWin.VisualizationAxis.SelectedIndex = aVisual.Axis
            numVisuals = myVisuals.Count
            ClearGrid(MainWin.VisualSummary)
            If numVisuals > 0 Then
                For i = 1 To numVisuals
                    aVisual = myVisuals.Item(i - 1)
                    MainWin.VisualSummary(i, 0) = i.ToString
                    MainWin.VisualSummary(i, 1) = VisualTypeNames.Substring((aVisual.Type) * 10, 10)
                    If aVisual.Compartment >= 0 And aVisual.Compartment <= myCompartments.Count - 1 Then
                        MainWin.VisualSummary(i, 2) = myCompartments(aVisual.Compartment).Name
                    ElseIf aVisual.Compartment = -1 Then
                        MainWin.VisualSummary(i, 2) = "All"
                    Else
                        MainWin.VisualSummary(i, 2) = "Not defined"
                    End If
                    If aVisual.Type = Visual.TwoD Then
                        MainWin.VisualSummary(i, 3) = VisualAxisNames.Substring((aVisual.Axis) * 6, 6)
                    Else
                        MainWin.VisualSummary(i, 3) = "-"
                    End If
                    If aVisual.Type = Visual.IsoSurface Then
                        MainWin.VisualSummary(i, 4) = aVisual.Value.ToString + myUnits.Convert(UnitsNum.Temperature).Units
                    ElseIf aVisual.Type = Visual.TwoD Then
                        MainWin.VisualSummary(i, 4) = aVisual.Value.ToString + myUnits.Convert(UnitsNum.Length).Units
                    Else
                        MainWin.VisualSummary(i, 4) = "-"
                    End If
                Next
                MainWin.VisualSummary.Select(indexVisual + 1, 0, indexVisual + 1, MainWin.VisualSummary.Cols.Count - 1, True)
            End If
            NumCompartments = myCompartments.Count
            ClearGrid(MainWin.VisualResolutionSummary)
            Dim aCompartment As New Compartment
            If numVisuals > 0 And NumCompartments > 0 Then
                aCompartment = myCompartments.Item(indexCompartment)
                MainWin.VisualizationX.Text = aCompartment.xGrid.ToString
                MainWin.VisualizationY.Text = aCompartment.yGrid.ToString
                MainWin.VisualizationZ.Text = aCompartment.zGrid.ToString
                For i = 1 To NumCompartments
                    aCompartment = myCompartments.Item(i - 1)
                    MainWin.VisualResolutionSummary(i, 0) = aCompartment.Name
                    MainWin.VisualResolutionSummary(i, 1) = i.ToString
                    MainWin.VisualResolutionSummary(i, 2) = aCompartment.xGrid.ToString
                    MainWin.VisualResolutionSummary(i, 3) = aCompartment.yGrid.ToString
                    MainWin.VisualResolutionSummary(i, 4) = aCompartment.zGrid.ToString
                Next
                MainWin.VisualResolutionSummary.Select(indexCompartment + 1, 0, indexCompartment + 1, MainWin.VisualResolutionSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Compartment(ByVal index As Integer)
        General()
        If index < 0 Or myCompartments.Count = 0 Then
            ClearGrid(MainWin.CompSummary)
            ClearGrid(MainWin.CompVariableArea)
            ClearGrid(MainWin.CompMaterials)
            MainWin.TabHorizontalFlow.Enabled = False
            MainWin.TabVerticalFlow.Enabled = False
            MainWin.TabMechanicalFlow.Enabled = False
            MainWin.TabTargets.Enabled = False
            MainWin.TabDetection.Enabled = False
            MainWin.TabHeatTransfer.Enabled = False
            MainWin.TabFires.Enabled = False
            MainWin.TabOutput.Enabled = False
            MainWin.GroupCompartments.Enabled = False
        Else
            Dim aCompartment As New Compartment
            aCompartment = myCompartments.Item(index)
            ' fill the widgets on the geometry page from the supplied compartment data
            MainWin.TabHorizontalFlow.Enabled = True
            MainWin.TabVerticalFlow.Enabled = True
            MainWin.TabMechanicalFlow.Enabled = True
            MainWin.TabTargets.Enabled = True
            MainWin.TabDetection.Enabled = True
            MainWin.TabHeatTransfer.Enabled = True
            MainWin.TabFires.Enabled = True
            MainWin.TabOutput.Enabled = True
            MainWin.GroupCompartments.Enabled = True
            MainWin.GroupCompartments.Text = "Compartment " + (index + 1).ToString + " (of " + myCompartments.Count.ToString + ")"
            MainWin.CompName.Text = aCompartment.Name
            MainWin.CompWidth.Text = aCompartment.RoomWidth.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompDepth.Text = aCompartment.RoomDepth.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompHeight.Text = aCompartment.RoomHeight.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompXPosition.Text = aCompartment.RoomOriginX.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompYPosition.Text = aCompartment.RoomOriginY.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompZPosition.Text = aCompartment.RoomOriginZ.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompWallLeak.Text = aCompartment.WallLeak.ToString + myUnits.Convert(UnitsNum.Area).Units + "/" + myUnits.Convert(UnitsNum.Area).Units
            MainWin.CompFloorLeak.Text = aCompartment.FloorLeak.ToString + myUnits.Convert(UnitsNum.Area).Units + "/" + myUnits.Convert(UnitsNum.Area).Units

            If myEnvironment.AdiabaticWalls Then
                MainWin.GroupCompSurfaces.Enabled = False
                MainWin.CompMaterials.Enabled = False
            Else
                MainWin.GroupCompSurfaces.Enabled = True
                MainWin.CompMaterials.Enabled = True
                ClearGrid(MainWin.CompMaterials)
                MainWin.CompMaterials.ComboList = myThermalProperties.MaterialsList
                For i = 1 To 3
                    MainWin.CompMaterials(i, 0) = i
                    If aCompartment.CeilingMaterial(i) <> "" Then
                        If aCompartment.CeilingMaterial(i) = "Off" Then
                            MainWin.CompMaterials(i, CompMaterialsColNum.CeilingMaterial) = ""
                            MainWin.CompMaterials(i, CompMaterialsColNum.CeilingThickness) = ""
                        Else
                            MainWin.CompMaterials(i, CompMaterialsColNum.CeilingMaterial) = myThermalProperties.GetLongName(aCompartment.CeilingMaterial(i))
                            MainWin.CompMaterials(i, CompMaterialsColNum.CeilingThickness) = aCompartment.CeilingThickness(i).ToString + myUnits.Convert(UnitsNum.Length).Units
                        End If
                    End If
                    If aCompartment.WallMaterial(i) <> "" Then
                        If aCompartment.WallMaterial(i) = "Off" Then
                            MainWin.CompMaterials(i, CompMaterialsColNum.WallMaterial) = ""
                            MainWin.CompMaterials(i, CompMaterialsColNum.WallThickness) = ""
                        Else
                            MainWin.CompMaterials(i, CompMaterialsColNum.WallMaterial) = myThermalProperties.GetLongName(aCompartment.WallMaterial(i))
                            MainWin.CompMaterials(i, CompMaterialsColNum.WallThickness) = aCompartment.WallThickness(i).ToString + myUnits.Convert(UnitsNum.Length).Units
                        End If
                    End If
                    If aCompartment.FloorMaterial(i) <> "" Then
                        If aCompartment.FloorMaterial(i) = "Off" Then
                            MainWin.CompMaterials(i, CompMaterialsColNum.FloorMaterial) = ""
                            MainWin.CompMaterials(i, CompMaterialsColNum.FloorThickness) = ""
                        Else
                            MainWin.CompMaterials(i, CompMaterialsColNum.FloorMaterial) = myThermalProperties.GetLongName(aCompartment.FloorMaterial(i))
                            MainWin.CompMaterials(i, CompMaterialsColNum.FloorThickness) = aCompartment.FloorThickness(i).ToString + myUnits.Convert(UnitsNum.Length).Units
                        End If
                    End If
                Next
            End If

            If aCompartment.Shaft = True Then
                MainWin.CompShaft.Checked = True
            ElseIf aCompartment.Hall = True Then
                MainWin.CompCorridor.Checked = True
            Else
                MainWin.CompNormal.Checked = True
            End If

            aCompartment.GetVariableArea(AreaPoints, HeightPoints, NumPoints)
            ClearGrid(MainWin.CompVariableArea)
            If NumPoints > 0 Then
                For i = 1 To NumPoints
                    MainWin.CompVariableArea(i, 0) = HeightPoints(i).ToString + myUnits.Convert(UnitsNum.Length).Units
                    MainWin.CompVariableArea(i, 1) = AreaPoints(i).ToString + myUnits.Convert(UnitsNum.Area).Units
                Next
            End If

            NumCompartments = myCompartments.Count
            ClearGrid(MainWin.CompSummary)
            If NumCompartments > 0 Then
                For i = 1 To NumCompartments
                    aCompartment = myCompartments.Item(i - 1)
                    MainWin.CompSummary(i, 0) = aCompartment.Name
                    MainWin.CompSummary(i, 1) = i.ToString
                    MainWin.CompSummary(i, 2) = aCompartment.RoomWidth.ToString
                    MainWin.CompSummary(i, 3) = aCompartment.RoomDepth.ToString
                    MainWin.CompSummary(i, 4) = aCompartment.RoomHeight.ToString
                    MainWin.CompSummary(i, 5) = aCompartment.RoomOriginX.ToString
                    MainWin.CompSummary(i, 6) = aCompartment.RoomOriginY.ToString
                    MainWin.CompSummary(i, 7) = aCompartment.RoomOriginZ.ToString
                    Dim nCeilings As Integer = 0, nWalls As Integer = 0, nFloors As Integer = 0
                    If aCompartment.CeilingMaterial(1) <> "" Then nCeilings += 1
                    If aCompartment.CeilingMaterial(2) <> "" Then nCeilings += 1
                    If aCompartment.CeilingMaterial(3) <> "" Then nCeilings += 1
                    If nCeilings = 0 Then
                        MainWin.CompSummary(i, 8) = ""
                    ElseIf nCeilings = 1 Then
                        MainWin.CompSummary(i, 8) = aCompartment.CeilingMaterial(1).ToLower
                    Else
                        MainWin.CompSummary(i, 8) = nCeilings.ToString + " Layers"
                    End If
                    If aCompartment.WallMaterial(1) <> "" Then nWalls += 1
                    If aCompartment.WallMaterial(2) <> "" Then nWalls += 1
                    If aCompartment.WallMaterial(3) <> "" Then nWalls += 1
                    If nWalls = 0 Then
                        MainWin.CompSummary(i, 9) = ""
                    ElseIf nWalls = 1 Then
                        MainWin.CompSummary(i, 9) = aCompartment.WallMaterial(1).ToLower
                    Else
                        MainWin.CompSummary(i, 9) = nWalls.ToString + " Layers"
                    End If
                    If aCompartment.FloorMaterial(1) <> "" Then nFloors += 1
                    If aCompartment.FloorMaterial(2) <> "" Then nFloors += 1
                    If aCompartment.FloorMaterial(3) <> "" Then nFloors += 1
                    If nFloors = 0 Then
                        MainWin.CompSummary(i, 10) = ""
                    ElseIf nFloors = 1 Then
                        MainWin.CompSummary(i, 10) = aCompartment.FloorMaterial(1).ToLower
                    Else
                        MainWin.CompSummary(i, 10) = nFloors.ToString + " Layers"
                    End If
                    MainWin.CompSummary(i, 11) = myFireProperties.NumberofConnections(i - 1)
                    MainWin.CompSummary(i, 12) = myHVents.NumberofConnections(i - 1)
                    MainWin.CompSummary(i, 13) = myVVents.NumberofConnections(i - 1)
                    MainWin.CompSummary(i, 14) = myMVents.NumberofConnections(i - 1)
                    MainWin.CompSummary(i, 15) = myDetectors.NumberofConnections(i - 1)
                    MainWin.CompSummary(i, 16) = myTargets.NumberofConnections(i - 1)
                Next
                MainWin.CompSummary.Select(index + 1, 0, index + 1, MainWin.CompSummary.Cols.Count - 1, True)
                InitCompartmentList(MainWin.HVentComp1)
                InitCompartmentList(MainWin.HVentComp2)
                InitCompartmentList(MainWin.VVentCompTop)
                InitCompartmentList(MainWin.VVentCompBottom)
                InitCompartmentList(MainWin.MVentFromComp)
                InitCompartmentList(MainWin.MventToComp)
                InitCompartmentList(MainWin.TargetComp)
                InitCompartmentList(MainWin.DetectorComp)
                InitCompartmentList(MainWin.HHeatComp1)
                InitCompartmentList(MainWin.HHeatComp2)
                InitCompartmentList(MainWin.VHeatComp1)
                InitCompartmentList(MainWin.VHeatComp2)
                InitCompartmentList(MainWin.FireComp)
                InitCompartmentList(MainWin.VisualizationComp)
            End If
        End If
    End Sub
    Public Sub HVents(ByVal index As Integer)
        General()
        Dim OpenTypeLabel As String = ""
        If index < 0 Or index >= myHVents.Count Then
            ClearGrid(MainWin.HVentSummary)
            ClearGrid(MainWin.HVentFractions)
            MainWin.GroupHVentGeometry.Enabled = False
        Else
            MainWin.GroupHVentGeometry.Enabled = True
            MainWin.GroupHVentGeometry.Text = "Vent " + (index + 1).ToString + " (of " + myHVents.Count.ToString + ") Geometry"
            Dim aVent As New Vent
            aVent = myHVents(index)
            MainWin.HVentName.Text = aVent.Name
            If aVent.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.HVentComp1.SelectedIndex = aVent.FirstCompartment + 1
            End If
            If aVent.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.HVentComp2.SelectedIndex = aVent.SecondCompartment + 1
            End If
            MainWin.HVentOffset.Text = aVent.Offset.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentBottom.Text = aVent.Bottom.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentHeight.Text = aVent.Height.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentWidth.Text = aVent.Width.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentFace.SelectedIndex = aVent.Face - 1

            MainWin.HVentOpenCriterion.SelectedIndex = aVent.OpenType
            If aVent.OpenType = Vent.OpenbyTime Then
                MainWin.HVentOpenValue.Visible = False
                MainWin.HVentOpenValueLabel.Visible = False
                MainWin.HVentTarget.Visible = False
                MainWin.HVentTargetLabel.Visible = False
                MainWin.HVentFinalFraction.Visible = False
                MainWin.HVentFinalLabel.Visible = False
                MainWin.HVentInitialFraction.Visible = False
                MainWin.HVentInitialLabel.Visible = False
                MainWin.HVentFractions.Visible = True
            ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.Temperature).Units
                MainWin.HVentOpenValue.Visible = True
                MainWin.HVentOpenValueLabel.Visible = True
                MainWin.HVentTarget.Visible = True
                MainWin.HVentTargetLabel.Visible = True
                MainWin.HVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.HVentFinalFraction.Visible = True
                MainWin.HVentFinalLabel.Visible = True
                MainWin.HVentInitialFraction.Visible = True
                MainWin.HVentInitialLabel.Visible = True
                MainWin.HVentFractions.Visible = False
            ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.HeatFlux).Units
                MainWin.HVentOpenValue.Visible = True
                MainWin.HVentOpenValueLabel.Visible = True
                MainWin.HVentTarget.Visible = True
                MainWin.HVentTargetLabel.Visible = True
                MainWin.HVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.HVentFinalFraction.Visible = True
                MainWin.HVentFinalLabel.Visible = True
                MainWin.HVentInitialFraction.Visible = True
                MainWin.HVentInitialLabel.Visible = True
                MainWin.HVentFractions.Visible = False
            End If
            MainWin.HVentOpenValue.Text = " "
            ClearGrid(MainWin.HVentFractions)
            If aVent.OpenType = Vent.OpenbyTime Then
                aVent.GetRamp(OpeningTimes, OpeningFractions, NumPoints)
                For i = 1 To NumPoints
                    MainWin.HVentFractions(i, 0) = OpeningTimes(i)
                    MainWin.HVentFractions(i, 1) = OpeningFractions(i)
                Next
            Else
                ClearGrid(MainWin.HVentFractions)
                MainWin.HVentInitialFraction.Text = aVent.InitialOpening.ToString
                MainWin.HVentFinalFraction.Text = aVent.FinalOpening.ToString
                MainWin.HVentOpenValue.Text = aVent.OpenValue.ToString + OpenTypeLabel
            End If

            NumHVents = myHVents.Count
            ClearGrid(MainWin.HVentSummary)
            If NumHVents > 0 Then
                For i = 1 To NumHVents
                    aVent = myHVents(i - 1)
                    MainWin.HVentSummary(i, 0) = i.ToString
                    MainWin.HVentSummary(i, 1) = aVent.Name
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.HVentSummary(i, 2) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.HVentSummary(i, 2) = "Outside"
                    Else
                        MainWin.HVentSummary(i, 2) = "Not defined"
                    End If
                    MainWin.HVentSummary(i, 3) = aVent.Offset.ToString
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.HVentSummary(i, 4) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.HVentSummary(i, 4) = "Outside"
                    Else
                        MainWin.HVentSummary(i, 4) = "Not defined"
                    End If
                    MainWin.HVentSummary(i, 6) = aVent.Bottom.ToString
                    MainWin.HVentSummary(i, 7) = aVent.Height.ToString
                    MainWin.HVentSummary(i, 8) = aVent.Width.ToString
                    MainWin.HVentSummary(i, 9) = aVent.InitialOpening.ToString
                    MainWin.HVentSummary(i, 10) = FaceNames.Substring((aVent.Face - 1) * 5, 5)
                    MainWin.HVentSummary(i, 11) = aVent.Offset.ToString
                Next
                MainWin.HVentSummary.Select(index + 1, 0, index + 1, MainWin.HVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub VVents(ByVal index As Integer)
        General()
        Dim OpenTypeLabel As String = ""
        If index < 0 Or index >= myVVents.Count Then
            ClearGrid(MainWin.VVentSummary)
            ClearGrid(MainWin.VVentFractions)
            MainWin.GroupVVents.Enabled = False
        Else
            MainWin.GroupVVents.Enabled = True
            MainWin.GroupVVents.Text = "Vent " + (index + 1).ToString + " (of " + myVVents.Count.ToString + ") Geometry"
            Dim aVent As New Vent
            aVent = myVVents(index)
            MainWin.VVentName.Text = aVent.Name
            If aVent.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.VVentCompTop.SelectedIndex = aVent.FirstCompartment + 1
            End If
            If aVent.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.VVentCompBottom.SelectedIndex = aVent.SecondCompartment + 1
            End If
            MainWin.VVentArea.Text = aVent.Area.ToString + myUnits.Convert(UnitsNum.Area).Units
            MainWin.VVentShape.SelectedIndex = aVent.Shape - 1
            MainWin.VVentXOffset.Text = aVent.OffsetX.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.VVentYOffset.Text = aVent.OffsetY.ToString + myUnits.Convert(UnitsNum.Length).Units

            MainWin.VVentOpenCriterion.SelectedIndex = aVent.OpenType
            If aVent.OpenType = Vent.OpenbyTime Then
                MainWin.VVentOpenValue.Visible = False
                MainWin.VVentOpenValueLabel.Visible = False
                MainWin.VVentTarget.Visible = False
                MainWin.VVentTargetLabel.Visible = False
                MainWin.VVentFinalFraction.Visible = False
                MainWin.VVentFinalLabel.Visible = False
                MainWin.VVentInitialFraction.Visible = False
                MainWin.VVentInitialLabel.Visible = False
                MainWin.VVentFractions.Visible = True
            ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.Temperature).Units
                MainWin.VVentOpenValue.Visible = True
                MainWin.VVentOpenValueLabel.Visible = True
                MainWin.VVentTarget.Visible = True
                MainWin.VVentTargetLabel.Visible = True
                MainWin.VVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.VVentFinalFraction.Visible = True
                MainWin.VVentFinalLabel.Visible = True
                MainWin.VVentInitialFraction.Visible = True
                MainWin.VVentInitialLabel.Visible = True
                MainWin.VVentFractions.Visible = False
            ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.HeatFlux).Units
                MainWin.VVentOpenValue.Enabled = True
                MainWin.VVentOpenValueLabel.Visible = True
                MainWin.VVentTarget.Enabled = True
                MainWin.VVentTargetLabel.Visible = True
                MainWin.VVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.VVentFinalFraction.Visible = True
                MainWin.VVentFinalLabel.Visible = True
                MainWin.VVentInitialFraction.Visible = True
                MainWin.VVentInitialLabel.Visible = True
                MainWin.VVentFractions.Visible = False
            End If
            MainWin.VVentOpenValue.Text = " "
            ClearGrid(MainWin.VVentFractions)
            If aVent.OpenType = Vent.OpenbyTime Then
                aVent.GetRamp(OpeningTimes, OpeningFractions, NumPoints)
                For i = 1 To NumPoints
                    MainWin.VVentFractions(i, 0) = OpeningTimes(i)
                    MainWin.VVentFractions(i, 1) = OpeningFractions(i)
                Next
            Else
                ClearGrid(MainWin.VVentFractions)
                MainWin.VVentInitialFraction.Text = aVent.InitialOpening.ToString
                MainWin.VVentFinalFraction.Text = aVent.FinalOpening.ToString
                MainWin.VVentOpenValue.Text = aVent.OpenValue.ToString + OpenTypeLabel
            End If

            numVVents = myVVents.Count
            ClearGrid(MainWin.VVentSummary)
            If numVVents > 0 Then
                For i = 1 To numVVents
                    aVent = myVVents(i - 1)
                    MainWin.VVentSummary(i, 0) = i.ToString
                    MainWin.VVentSummary(i, 1) = aVent.Name
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.VVentSummary(i, 2) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.VVentSummary(i, 2) = "Outside"
                    Else
                        MainWin.VVentSummary(i, 2) = "Not defined"
                    End If
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.VVentSummary(i, 3) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.VVentSummary(i, 3) = "Outside"
                    Else
                        MainWin.VVentSummary(i, 3) = "Not defined"
                    End If
                    MainWin.VVentSummary(i, 4) = aVent.Area.ToString
                    MainWin.VVentSummary(i, 5) = ShapeNames.Substring((aVent.Shape - 1) * 6, 6)
                Next
                MainWin.VVentSummary.Select(index + 1, 0, index + 1, MainWin.VVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub MVents(ByVal index As Integer)
        General()
        Dim OpenTypeLabel As String = ""
        If index < 0 Or index >= myMVents.Count Then
            ClearGrid(MainWin.MVentSummary)
            ClearGrid(MainWin.MVentFractions)
            MainWin.GroupMVents.Enabled = False
        Else
            MainWin.GroupMVents.Enabled = True
            MainWin.GroupMVents.Text = "Vent " + (index + 1).ToString + " (of " + myMVents.Count.ToString + ") Geometry"
            Dim aVent As New Vent
            aVent = myMVents(index)
            If aVent.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.MVentFromComp.SelectedIndex = aVent.FirstCompartment + 1
            End If
            MainWin.MVentFromArea.Text = aVent.FirstArea.ToString + myUnits.Convert(UnitsNum.Area).Units
            MainWin.MVentFromHeight.Text = aVent.FirstCenterHeight.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.MVentFromOrientation.SelectedIndex = aVent.FirstOrientation - 1

            If aVent.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.MventToComp.SelectedIndex = aVent.SecondCompartment + 1
            End If
            MainWin.MVentToArea.Text = aVent.SecondArea.ToString + myUnits.Convert(UnitsNum.Area).Units
            MainWin.MVentToHeight.Text = aVent.SecondCenterHeight.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.MVentToOrientation.SelectedIndex = aVent.SecondOrientation - 1

            MainWin.MVentName.Text = aVent.Name
            MainWin.MVentFlow.Text = aVent.FlowRate.ToString + myUnits.Convert(UnitsNum.Flowrate).Units
            MainWin.MVentDropoff.Text = aVent.BeginFlowDropoff.ToString + myUnits.Convert(UnitsNum.Pressure).Units
            MainWin.MVentZero.Text = aVent.ZeroFlow.ToString + myUnits.Convert(UnitsNum.Pressure).Units

            MainWin.MVentFilterEfficiency.Text = aVent.FilterEfficiency.ToString + " %"
            MainWin.MVentFilterTime.Text = aVent.FilterTime.ToString + myUnits.Convert(UnitsNum.Time).Units

            MainWin.MVentXOffset.Text = aVent.OffsetX.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.MVentYOffset.Text = aVent.OffsetY.ToString + myUnits.Convert(UnitsNum.Length).Units

            MainWin.MVentOpenCriterion.SelectedIndex = aVent.OpenType
            If aVent.OpenType = Vent.OpenbyTime Then
                MainWin.MVentOpenValue.Visible = False
                MainWin.MVentOpenValueLabel.Visible = False
                MainWin.MVentTarget.Visible = False
                MainWin.MVentTargetLabel.Visible = False
                MainWin.MVentFinalFraction.Visible = False
                MainWin.MventFinalLabel.Visible = False
                MainWin.MVentInitialFraction.Visible = False
                MainWin.MVentInitialLabel.Visible = False
                MainWin.MVentFractions.Visible = True
            ElseIf aVent.OpenType = Vent.OpenbyTemperature Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.Temperature).Units
                MainWin.MVentOpenValue.Visible = True
                MainWin.MVentOpenValueLabel.Visible = True
                MainWin.MVentTarget.Visible = True
                MainWin.MVentTargetLabel.Visible = True
                MainWin.MVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.MVentFinalFraction.Visible = True
                MainWin.MventFinalLabel.Visible = True
                MainWin.MVentInitialFraction.Visible = True
                MainWin.MVentInitialLabel.Visible = True
                MainWin.MVentFractions.Visible = False
            ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                OpenTypeLabel = myUnits.Convert(UnitsNum.HeatFlux).Units
                MainWin.MVentOpenValue.Visible = True
                MainWin.MVentOpenValueLabel.Visible = True
                MainWin.MVentTarget.Visible = True
                MainWin.MVentTargetLabel.Visible = True
                MainWin.MVentTarget.SelectedIndex = myTargets.GetIndex(aVent.Target)
                MainWin.MVentFinalFraction.Visible = True
                MainWin.MventFinalLabel.Visible = True
                MainWin.MVentInitialFraction.Visible = True
                MainWin.MVentInitialLabel.Visible = True
                MainWin.MVentFractions.Visible = False
            End If
            MainWin.MVentOpenValue.Text = " "
            ClearGrid(MainWin.MVentFractions)
            If aVent.OpenType = Vent.OpenbyTime Then
                aVent.GetRamp(OpeningTimes, OpeningFractions, NumPoints)
                If NumPoints > 0 Then
                    For i = 1 To NumPoints
                        MainWin.MVentFractions(i, 0) = OpeningTimes(i)
                        MainWin.MVentFractions(i, 1) = OpeningFractions(i)
                    Next
                End If
            Else
                ClearGrid(MainWin.MVentFractions)
                MainWin.MVentInitialFraction.Text = aVent.InitialOpening.ToString
                MainWin.MVentFinalFraction.Text = aVent.FinalOpening.ToString
                MainWin.MVentOpenValue.Text = aVent.OpenValue.ToString + OpenTypeLabel
            End If

            numMVents = myMVents.Count
            ClearGrid(MainWin.MVentSummary)
            If numMVents > 0 Then
                For i = 1 To numMVents
                    aVent = myMVents(i - 1)
                    MainWin.MVentSummary(i, 0) = i.ToString
                    MainWin.MVentSummary(i, 1) = aVent.Name
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.MVentSummary(i, 2) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.MVentSummary(i, 2) = "Outside"
                    Else
                        MainWin.MVentSummary(i, 2) = "Not defined"
                    End If
                    MainWin.MVentSummary(i, 3) = aVent.FirstArea.ToString
                    MainWin.MVentSummary(i, 4) = aVent.FirstCenterHeight.ToString
                    MainWin.MVentSummary(i, 5) = OrientationNames.Substring((aVent.FirstOrientation - 1) * 10, 10)
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.MVentSummary(i, 6) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.MVentSummary(i, 6) = "Outside"
                    Else
                        MainWin.MVentSummary(i, 6) = "Not defined"
                    End If
                    MainWin.MVentSummary(i, 7) = aVent.SecondArea.ToString
                    MainWin.MVentSummary(i, 8) = aVent.SecondCenterHeight.ToString
                    MainWin.MVentSummary(i, 9) = OrientationNames.Substring((aVent.SecondOrientation - 1) * 10, 10)
                    MainWin.MVentSummary(i, 10) = aVent.FlowRate.ToString
                    MainWin.MVentSummary(i, 11) = aVent.BeginFlowDropoff.ToString
                    MainWin.MVentSummary(i, 12) = aVent.ZeroFlow.ToString
                Next
                MainWin.MVentSummary.Select(index + 1, 0, index + 1, MainWin.MVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Targets(ByVal index As Integer)
        General()
        If index < 0 Or index >= myTargets.Count Then
            ClearGrid(MainWin.TargetSummary)
            MainWin.GroupTargets.Enabled = False
        Else
            MainWin.GroupTargets.Enabled = True
            MainWin.GroupTargets.Text = "Target " + (index + 1).ToString + " (of " + myTargets.Count.ToString + ") Geometry"
            Dim aTarget As New Target, aCompartment As Compartment
            aTarget = myTargets(index)
            MainWin.TargetName.Text = aTarget.Name
            If aTarget.Compartment <= -1 Then
                MainWin.TargetComp.SelectedIndex = -1
            ElseIf aTarget.Compartment >= 0 And aTarget.Compartment <= myCompartments.Count - 1 Then
                MainWin.TargetComp.SelectedIndex = aTarget.Compartment
                aCompartment = myCompartments(aTarget.Compartment)
            End If

            MainWin.TargetXPosition.Text = aTarget.XPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.TargetYPosition.Text = aTarget.YPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.TargetZPosition.Text = aTarget.ZPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            If aTarget.CheckTargetFacing(aTarget.TargetFacing) <> "-" Then
                Dim i As Integer = MainWin.TargetNormalType.FindString(aTarget.TargetFacing)
                If i > 0 Then
                    MainWin.TargetXNormal.Enabled = False
                    MainWin.TargetYNormal.Enabled = False
                    MainWin.TargetZNormal.Enabled = False
                    MainWin.TargetNormalType.SelectedIndex = i
                Else
                    numFires = myFires.Count
                    If numFires > 0 Then
                        Dim aFire As Fire
                        For j = 1 To numFires
                            aFire = myFires(j - 1)
                            If aTarget.Compartment = aFire.Compartment Then
                                If aFire.Name = aTarget.CheckTargetFacing("Fire " + j.ToString + ", " + aFire.Name) Then
                                    i = MainWin.TargetNormalType.FindString("Fire " + j.ToString + ", " + aFire.Name)
                                    MainWin.TargetXNormal.Enabled = False
                                    MainWin.TargetYNormal.Enabled = False
                                    MainWin.TargetZNormal.Enabled = False
                                    MainWin.TargetNormalType.SelectedIndex = i
                                Else
                                    MainWin.TargetXNormal.Enabled = True
                                    MainWin.TargetXNormal.Text = aTarget.XNormal.ToString
                                    MainWin.TargetYNormal.Enabled = True
                                    MainWin.TargetYNormal.Text = aTarget.YNormal.ToString
                                    MainWin.TargetZNormal.Enabled = True
                                    MainWin.TargetZNormal.Text = aTarget.ZNormal.ToString
                                    MainWin.TargetNormalType.SelectedIndex = 0
                                End If
                            End If
                        Next
                    End If
                End If
            Else
                MainWin.TargetXNormal.Enabled = True
                MainWin.TargetXNormal.Text = aTarget.XNormal.ToString
                MainWin.TargetYNormal.Enabled = True
                MainWin.TargetYNormal.Text = aTarget.YNormal.ToString
                MainWin.TargetZNormal.Enabled = True
                MainWin.TargetZNormal.Text = aTarget.ZNormal.ToString
                MainWin.TargetNormalType.SelectedIndex = 0
            End If
            MainWin.TargetMaterial.Text = myThermalProperties.GetLongName(aTarget.Material)
            MainWin.TargetSolutionType.SelectedIndex = aTarget.SolutionType
            MainWin.TargetInternalLocation.Text = aTarget.InternalLocation.ToString + myUnits.Convert(UnitsNum.Length).Units
            If aTarget.Material <> "Default" And aTarget.Material <> "Off" Then
                MainWin.TargetThickness.Enabled = True
                Dim aThermalProperty As New ThermalProperty
                aThermalProperty = myThermalProperties(myThermalProperties.GetIndex(aTarget.Material))
                If MainWin.TargetSolutionType.SelectedIndex = Target.Cylindrical Then
                    MainWin.TargetThicknessLabel.Text = "Diameter: "
                Else
                    MainWin.TargetThicknessLabel.Text = "Thickness: "
                End If
                If aTarget.Thickness = 0 Then
                    MainWin.TargetThickness.Text = aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
                Else
                    MainWin.TargetThickness.Text = aTarget.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
                End If
            Else
                MainWin.TargetThicknessLabel.Text = "Thickness: "
                MainWin.TargetThickness.Text = ""
                MainWin.TargetThickness.Enabled = False
            End If

            numTargets = myTargets.Count
            ClearGrid(MainWin.TargetSummary)
            If numTargets > 0 Then
                For i = 1 To numTargets
                    aTarget = myTargets(i - 1)
                    MainWin.TargetSummary(i, 0) = i.ToString
                    MainWin.TargetSummary(i, 1) = aTarget.Name
                    If aTarget.Compartment >= 0 And aTarget.Compartment <= myCompartments.Count - 1 Then
                        MainWin.TargetSummary(i, 2) = myCompartments(aTarget.Compartment).Name
                    ElseIf aTarget.Compartment = -1 Then
                        MainWin.TargetSummary(i, 2) = "Outside"
                    Else
                        MainWin.TargetSummary(i, 2) = "Not defined"
                    End If
                    MainWin.TargetSummary(i, 3) = aTarget.XPosition.ToString
                    MainWin.TargetSummary(i, 4) = aTarget.YPosition.ToString
                    MainWin.TargetSummary(i, 5) = aTarget.ZPosition.ToString
                    MainWin.TargetSummary(i, 6) = aTarget.XNormal.ToString
                    MainWin.TargetSummary(i, 7) = aTarget.YNormal.ToString
                    MainWin.TargetSummary(i, 8) = aTarget.ZNormal.ToString
                    MainWin.TargetSummary(i, 9) = aTarget.Material
                    MainWin.TargetSummary(i, 11) = SolutionTypeNames.Substring((aTarget.SolutionType) * 11, 11)
                Next
                MainWin.TargetSummary.Select(index + 1, 0, index + 1, MainWin.TargetSummary.Cols.Count - 1, True)
            End If
            InitTargetList(MainWin.FireTarget)
            InitTargetList(MainWin.HVentTarget)
            InitTargetList(MainWin.VVentTarget)
            InitTargetList(MainWin.MVentTarget)
        End If
    End Sub
    Public Sub Detectors(ByVal index As Integer)
        General()
        If index < 0 Or index >= myDetectors.Count Then
            ClearGrid(MainWin.DetectorSummary)
            MainWin.GroupDetectors.Enabled = False
        Else
            MainWin.GroupDetectors.Enabled = True
            MainWin.GroupDetectors.Text = "Alarm " + (index + 1).ToString + " (of " + myDetectors.Count.ToString + ")"
            Dim aDetector As New Target
            aDetector = myDetectors(index)
            MainWin.DetectorName.Text = aDetector.Name
            MainWin.DetectorType.SelectedIndex = aDetector.DetectorType
            If aDetector.Compartment <= -1 Then
                MainWin.DetectorComp.SelectedIndex = -1
            ElseIf aDetector.Compartment >= 0 And aDetector.Compartment <= myCompartments.Count - 1 Then
                MainWin.DetectorComp.SelectedIndex = aDetector.Compartment
            End If
            MainWin.DetectorActivationTemperature.Text = aDetector.ActivationTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
            MainWin.DetectorActivationObscuration.Text = aDetector.ActivationObscurationFlaming.ToString + myUnits.Convert(UnitsNum.Smoke).Units
            If aDetector.ActivationType = Target.ActivationbyTemperature Then
                MainWin.DetectorActivationTemperature.Enabled = True
                MainWin.DetectorActivationObscuration.Enabled = False
            Else
                MainWin.DetectorActivationTemperature.Enabled = False
                MainWin.DetectorActivationObscuration.Enabled = True
            End If
            MainWin.DetectorXPosition.Text = aDetector.XPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.DetectorYPosition.Text = aDetector.YPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.DetectorZPosition.Text = aDetector.ZPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.DetectorRTI.Text = aDetector.RTI.ToString + myUnits.Convert(UnitsNum.RTI).Units
            MainWin.DetectorSprayDensity.Text = aDetector.SprayDensity.ToString + myUnits.Convert(UnitsNum.Velocity).Units
            If aDetector.DetectorType = Target.TypeSprinkler Then
                MainWin.DetectorRTI.Enabled = True
                MainWin.DetectorSprayDensity.Enabled = True
            ElseIf aDetector.DetectorType = Target.TypeHeatDetector Then
                MainWin.DetectorRTI.Enabled = True
                MainWin.DetectorSprayDensity.Enabled = False
            Else
                MainWin.DetectorRTI.Enabled = False
                MainWin.DetectorSprayDensity.Enabled = False
            End If
            numDetectors = myDetectors.Count
            ClearGrid(MainWin.DetectorSummary)
            If numDetectors > 0 Then
                For i = 1 To numDetectors
                    aDetector = myDetectors(i - 1)
                    MainWin.DetectorSummary(i, 0) = i.ToString
                    MainWin.DetectorSummary(i, 1) = aDetector.Name
                    If aDetector.Compartment >= 0 And aDetector.Compartment <= myCompartments.Count - 1 Then
                        MainWin.DetectorSummary(i, 2) = myCompartments(aDetector.Compartment).Name
                    ElseIf aDetector.Compartment = -1 Then
                        MainWin.DetectorSummary(i, 2) = "Outside"
                    Else
                        MainWin.DetectorSummary(i, 2) = "Not defined"
                    End If
                    MainWin.DetectorSummary(i, 3) = DetectorTypes.Substring(aDetector.DetectorType * 9, 9)
                    MainWin.DetectorSummary(i, 4) = aDetector.XPosition.ToString
                    MainWin.DetectorSummary(i, 5) = aDetector.YPosition.ToString
                    MainWin.DetectorSummary(i, 6) = aDetector.ZPosition.ToString
                    If aDetector.ActivationType = Target.ActivationbyTemperature Then
                        MainWin.DetectorSummary(i, 7) = aDetector.ActivationTemperature.ToString
                    Else
                        MainWin.DetectorSummary(i, 7) = aDetector.ActivationObscurationFlaming.ToString
                    End If
                    MainWin.DetectorSummary(i, 8) = aDetector.RTI.ToString
                    MainWin.DetectorSummary(i, 9) = aDetector.SprayDensity.ToString
                Next
                MainWin.DetectorSummary.Select(index + 1, 0, index + 1, MainWin.DetectorSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Heats(ByVal Hindex As Integer, ByVal Vindex As Integer)
        General()
        ' Horizontal heat transfer first
        If Hindex < 0 Or Hindex >= myHHeats.Count Then
            ClearGrid(MainWin.HHeatSummary)
            MainWin.GroupHHeats.Enabled = False
        Else
            MainWin.GroupHHeats.Enabled = True
            MainWin.GroupHHeats.Text = "Connection " + (Hindex + 1).ToString + " (of " + myHHeats.Count.ToString + ")"
            Dim aHeat As New Vent
            aHeat = myHHeats(Hindex)
            If aHeat.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.HHeatComp1.SelectedIndex = aHeat.FirstCompartment + 1
            End If
            If aHeat.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.HHeatComp2.SelectedIndex = aHeat.SecondCompartment + 1
            End If
            MainWin.HHeatFraction.Text = aHeat.InitialOpening.ToString
            numHeats = myHHeats.Count
            ClearGrid(MainWin.HHeatSummary)
            If numHeats > 0 Then
                For i = 1 To numHeats
                    aHeat = myHHeats(i - 1)
                    MainWin.HHeatSummary(i, 0) = i.ToString
                    MainWin.HHeatSummary(i, 1) = OrientationNames.Substring((aHeat.VentType - 3) * 10, 10)
                    If aHeat.FirstCompartment >= 0 And aHeat.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.HHeatSummary(i, 2) = myCompartments(aHeat.FirstCompartment).Name
                    ElseIf aHeat.FirstCompartment = -1 Then
                        MainWin.HHeatSummary(i, 2) = "Outside"
                    Else
                        MainWin.HHeatSummary(i, 2) = "Not defined"
                    End If
                    If aHeat.SecondCompartment >= 0 And aHeat.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.HHeatSummary(i, 3) = myCompartments(aHeat.SecondCompartment).Name
                    ElseIf aHeat.SecondCompartment = -1 Then
                        MainWin.HHeatSummary(i, 3) = "Outside"
                    Else
                        MainWin.HHeatSummary(i, 3) = "Not defined"
                    End If
                    MainWin.HHeatSummary(i, 4) = aHeat.InitialOpening.ToString
                Next
                MainWin.HHeatSummary.Select(Hindex + 1, 0, Hindex + 1, MainWin.HHeatSummary.Cols.Count - 1, True)
            End If
        End If
        If Vindex < 0 Or Vindex >= myVHeats.Count Then
            ClearGrid(MainWin.VHeatSummary)
            MainWin.GroupVHeats.Enabled = False
        Else
            MainWin.GroupVHeats.Enabled = True
            MainWin.GroupVHeats.Text = "Connection " + (Vindex + 1).ToString + " (of " + myVHeats.Count.ToString + ")"
            Dim aHeat As New Vent
            aHeat = myVHeats(Vindex)
            If aHeat.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.VHeatComp1.SelectedIndex = aHeat.FirstCompartment + 1
            End If
            If aHeat.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.VHeatComp2.SelectedIndex = aHeat.SecondCompartment + 1
            End If
            aHeat.InitialOpening = 1.0
            numHeats = myVHeats.Count
            ClearGrid(MainWin.VHeatSummary)
            If numHeats > 0 Then
                For i = 1 To numHeats
                    aHeat = myVHeats(i - 1)
                    MainWin.VHeatSummary(i, 0) = i.ToString
                    MainWin.VHeatSummary(i, 1) = OrientationNames.Substring((aHeat.VentType - 3) * 10, 10)
                    If aHeat.FirstCompartment >= 0 And aHeat.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.VHeatSummary(i, 2) = myCompartments(aHeat.FirstCompartment).Name
                    ElseIf aHeat.FirstCompartment = -1 Then
                        MainWin.VHeatSummary(i, 2) = "Outside"
                    Else
                        MainWin.VHeatSummary(i, 2) = "Not defined"
                    End If
                    If aHeat.SecondCompartment >= 0 And aHeat.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.VHeatSummary(i, 3) = myCompartments(aHeat.SecondCompartment).Name
                    ElseIf aHeat.SecondCompartment = -1 Then
                        MainWin.VHeatSummary(i, 3) = "Outside"
                    Else
                        MainWin.VHeatSummary(i, 3) = "Not defined"
                    End If
                Next
                MainWin.VHeatSummary.Select(Vindex + 1, 0, Vindex + 1, MainWin.VHeatSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Fires(ByVal index As Integer)
        General()
        Dim afireTimeSeries(12, 0) As Double, NumPoints As Integer
        Dim IgnitionTypeLabel As String = ""
        Dim ir, ic As Integer

        ' Update fire instances
        If index < 0 Or index >= myFires.Count Then
            MainWin.FireDefinitionName.Enabled = False
            MainWin.FireH.Enabled = False
            MainWin.FireC.Enabled = False
            MainWin.FireO.Enabled = False
            MainWin.FireN.Enabled = False
            MainWin.FireCl.Enabled = False
            MainWin.FireHoC.Enabled = False
            MainWin.FireRadiativeFraction.Enabled = False

            ClearGrid(MainWin.FireSummary)
            ClearGrid(MainWin.FireDataSS)
        Else
            MainWin.FireDefinitionName.Enabled = True
            MainWin.FireH.Enabled = True
            MainWin.FireC.Enabled = True
            MainWin.FireO.Enabled = True
            MainWin.FireN.Enabled = True
            MainWin.FireCl.Enabled = True
            MainWin.FireHoC.Enabled = True
            MainWin.FireRadiativeFraction.Enabled = True

            Dim aFire As New Fire, aFireInstance As New Fire, FireIndex As Integer
            aFireInstance = myFires(index)
            FireIndex = myFireProperties.GetFireIndex(aFireInstance.ReferencedFireDefinition)
            MainWin.FireInstanceName.Text = aFireInstance.Name
            MainWin.FireXPosition.Text = aFireInstance.XPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.FireYPosition.Text = aFireInstance.YPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.FireIgnitionCriteria.SelectedIndex = aFireInstance.IgnitionType
            If aFireInstance.IgnitionType = Fire.FireIgnitionbyTime Then
                IgnitionTypeLabel = myUnits.Convert(UnitsNum.Time).Units
                MainWin.FireTarget.Enabled = False
            ElseIf aFireInstance.IgnitionType = Fire.FireIgnitionbyTemperature Then
                IgnitionTypeLabel = myUnits.Convert(UnitsNum.Temperature).Units
                MainWin.FireTarget.Enabled = True
                MainWin.FireTarget.SelectedIndex = myTargets.GetIndex(aFireInstance.Target)
            ElseIf aFireInstance.IgnitionType = Fire.FireIgnitionbyFlux Then
                IgnitionTypeLabel = myUnits.Convert(UnitsNum.HeatFlux).Units
                MainWin.FireTarget.Enabled = True
                MainWin.FireTarget.SelectedIndex = myTargets.GetIndex(aFireInstance.Target)
            End If
            MainWin.FireIgnitionValue.Text = " "
            If aFireInstance.IgnitionType >= 0 Then MainWin.FireIgnitionValue.Text = aFireInstance.IgnitionValue.ToString + IgnitionTypeLabel

            numFires = myFires.Count
            ClearGrid(MainWin.FireSummary)
            If numFires > 0 Then
                For i = 1 To numFires
                    aFireInstance = myFires(i - 1)

                    MainWin.FireSummary(i, FireSummaryNum.Fire) = i.ToString
                    If aFireInstance.Compartment >= 0 And aFireInstance.Compartment <= myCompartments.Count - 1 Then
                        MainWin.FireSummary(i, FireSummaryNum.Compartment) = myCompartments(aFireInstance.Compartment).Name
                    ElseIf aFireInstance.Compartment = -1 Then
                        MainWin.FireSummary(i, FireSummaryNum.Compartment) = "Outside"
                    Else
                        MainWin.FireSummary(i, FireSummaryNum.Compartment) = "Not defined"
                    End If
                    If aFireInstance.IgnitionType >= 0 Then
                        MainWin.FireSummary(i, FireSummaryNum.IgnitionType) = IgnitionNames.Substring((aFireInstance.IgnitionType) * 11, 11)
                        MainWin.FireSummary(i, FireSummaryNum.SetPoint) = aFireInstance.IgnitionValue
                        If aFireInstance.IgnitionType = Fire.FireIgnitionbyFlux Then
                            MainWin.FireSummary(i, FireSummaryNum.Target) = aFireInstance.Target
                        Else
                            MainWin.FireSummary(i, FireSummaryNum.Target) = ""
                        End If
                    End If
                    MainWin.FireSummary(i, FireSummaryNum.X) = aFireInstance.XPosition.ToString
                    MainWin.FireSummary(i, FireSummaryNum.Y) = aFireInstance.YPosition.ToString

                    FireIndex = myFireProperties.GetFireIndex(aFireInstance.ReferencedFireDefinition)
                    If FireIndex >= 0 Then
                        MainWin.FireSummary(i, FireSummaryNum.FireID) = aFireInstance.Name
                        aFire = myFireProperties(FireIndex)
                        MainWin.FireSummary(i, FireSummaryNum.FirePropertyID) = aFire.Name
                        MainWin.FireSummary(i, FireSummaryNum.Fuel) = aFire.ChemicalFormula()
                        MainWin.FireSummary(i, FireSummaryNum.HRR) = aFire.Peak(Fire.FireHRR).ToString
                    Else
                        MainWin.FireSummary(i, FireSummaryNum.FirePropertyID) = ""
                        MainWin.FireSummary(i, FireSummaryNum.Fuel) = ""
                        MainWin.FireSummary(i, FireSummaryNum.HRR) = ""
                    End If
                Next
                MainWin.FireSummary.Select(index + 1, 0, index + 1, MainWin.FireSummary.Cols.Count - 1, True)
            End If

            ' Update fire definitions
            aFireInstance = myFires(index)
            FireIndex = myFireProperties.GetFireIndex(aFireInstance.ReferencedFireDefinition)

            If FireIndex < 0 Or FireIndex >= myFireProperties.Count Then
                ClearGrid(MainWin.FireDataSS)
            Else
                MainWin.FireDefinitionName.Enabled = True
                MainWin.FireH.Enabled = True
                MainWin.FireC.Enabled = True
                MainWin.FireO.Enabled = True
                MainWin.FireN.Enabled = True
                MainWin.FireCl.Enabled = True
                MainWin.FireHoC.Enabled = True
                MainWin.FireRadiativeFraction.Enabled = True

                aFire = myFireProperties(FireIndex)
                MainWin.FireDefinitionName.Text = aFire.Name
                MainWin.FireC.Text = aFire.ChemicalFormula(formula.C).ToString
                MainWin.FireH.Text = aFire.ChemicalFormula(formula.H).ToString
                MainWin.FireO.Text = aFire.ChemicalFormula(formula.O).ToString
                MainWin.FireN.Text = aFire.ChemicalFormula(formula.N).ToString
                MainWin.FireCl.Text = aFire.ChemicalFormula(formula.Cl).ToString
                MainWin.FireHoC.Text = aFire.HeatofCombustion.ToString + myUnits.Convert(UnitsNum.HoC).Units
                MainWin.FireRadiativeFraction.Text = aFire.RadiativeFraction.ToString

                ClearGrid(MainWin.FireDataSS)
                MainWin.FireDataSS(0, 0) = "Time" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireTime).Units.Substring(1) + ")"
                MainWin.FireDataSS(0, 1) = "Mdot" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireMdot).Units.Substring(1) + ")"
                MainWin.FireDataSS(0, 2) = "HRR" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireQdot).Units.Substring(1) + ")"
                MainWin.FireDataSS(0, 3) = "Height" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireHeight).Units.Substring(1) + ")"
                MainWin.FireDataSS(0, 4) = "Area" + Chr(10) + "(" + myUnits.ConvertFireData(UnitsNum.FireArea).Units.Substring(1) + ")"
                MainWin.FireDataSS.AutoSizeRow(0)

                aFire.GetFireData(afireTimeSeries, NumPoints)
                If NumPoints >= 0 Then
                    For ir = 0 To NumPoints
                        For ic = 0 To 12
                            MainWin.FireDataSS(ir + 1, ic) = afireTimeSeries(ic, ir)
                        Next
                    Next
                End If

                UpdateFirePlot(FireIndex)

                If aFireInstance.Compartment >= 0 And aFireInstance.Compartment <= myCompartments.Count - 1 Then
                    MainWin.FireComp.SelectedIndex = aFireInstance.Compartment
                Else
                    MainWin.FireComp.SelectedIndex = -1
                End If

                InitFireList(MainWin.ReferencedFireDefinition)
                If FireIndex >= 0 Then
                    MainWin.ReferencedFireDefinition.SelectedIndex = FireIndex + 1
                Else
                    MainWin.ReferencedFireDefinition.SelectedIndex = -1
                End If

            End If
        End If
    End Sub
    Private Sub UpdateFirePlot(ByVal index As Integer)
        Dim aFire As New Fire
        Dim aFireData(12, 0) As Double, numPoints As Integer, iSelectedColumn As Integer
        Dim x() As Double, y() As Double, j As Integer
        aFire = myFireProperties(index)
        MainWin.FirePlot.Clear()
        aFire.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        iSelectedColumn = MainWin.FireDataSS.ColSel
        If iSelectedColumn < 1 Then iSelectedColumn = Fire.FireHRR
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(iSelectedColumn, j)
        Next
        Dim lp As New NPlot.LinePlot(y, x)
        MainWin.FirePlot.Add(lp)
        MainWin.FirePlot.Title = aFire.Name + ": " + MainWin.FireDataSS(0, iSelectedColumn).ToString.Replace(Chr(10), " ")
        MainWin.FirePlot.Refresh()
    End Sub
#End Region
#Region "Support Routines"
    Public Sub ClearGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        ' Erase the contents of a grid, leaving only the header row
        For i = 2 To obj.Rows.Count
            obj.Clear(C1.Win.C1FlexGrid.ClearFlags.Content, i - 1, 0, i - 1, obj.Cols.Count - 1)
        Next
    End Sub
    Public ReadOnly Property CountGridPoints(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid) As Integer
        ' Find the last non-blank row of a grid on the GUI
        Get
            Dim LastRow As Integer, ir As Integer
            Dim s As String
            LastRow = -1
            ir = obj.Rows.Count
            Do
                ir = ir - 1
                s = CType(obj(ir, 0), String) + " "
            Loop Until s <> " " Or ir < 0
            If LastRow < ir Then LastRow = ir
            Return LastRow
        End Get
    End Property
    Public Sub InitTargetList(ByVal obj As ComboBox)
        Dim i As Integer, current As String
        current = obj.Text
        obj.Items.Clear()
        If myTargets.Count > 0 Then
            For i = 0 To myTargets.Count - 1
                obj.Items.Add(myTargets.Item(i).Name)
                If myTargets.Item(i).Name = current Then
                    obj.SelectedIndex = i
                End If
            Next
        End If
    End Sub
    Public Sub InitThermalPropertyList(ByVal obj As ComboBox)
        Dim i As Integer, current As String
        current = obj.Text
        obj.Items.Clear()
        obj.Items.Add("Off")
        'obj.SelectedIndex = 0
        If myThermalProperties.Count > 0 Then
            For i = 0 To myThermalProperties.Count - 1
                obj.Items.Add(myThermalProperties.Item(i).Name)
                If myThermalProperties.Item(i).Name = current Then
                    obj.SelectedIndex = i + 1
                End If
            Next
        End If
    End Sub
    Public Sub InitCompartmentList(ByVal obj As ComboBox)
        Dim i As Integer
        obj.Items.Clear()
        If obj Is MainWin.VisualizationComp Then
            obj.Items.Add("All")
        ElseIf obj Is MainWin.HVentComp1 Or obj Is MainWin.HVentComp2 Or obj Is MainWin.VVentCompTop Or obj Is MainWin.VVentCompBottom Or obj Is MainWin.MVentFromComp Or obj Is MainWin.MventToComp _
            Or obj Is MainWin.HHeatComp1 Or obj Is MainWin.HHeatComp2 Or obj Is MainWin.VHeatComp1 Or obj Is MainWin.VHeatComp2 Then
            obj.Items.Add("Outside")
        End If
        If myCompartments.Count > 0 Then
            For i = 0 To myCompartments.Count - 1
                obj.Items.Add(myCompartments.Item(i).Name)
            Next
        End If
    End Sub
    Public Sub InitFireList(ByVal obj As ComboBox)
        Dim i As Integer, current As String
        current = obj.Text
        obj.Items.Clear()
        obj.Items.Add("New")
        obj.SelectedIndex = 0
        If myFireProperties.Count > 0 Then
            For i = 0 To myFireProperties.Count - 1
                obj.Items.Add(myFireProperties.Item(i).Name)
            Next
        End If
    End Sub
    Public Sub InitSummaryGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        obj.Cols.Fixed = 0
        obj.Rows.Fixed = 1
        obj.AllowEditing = False
        obj.SelectionMode = SelectionModeEnum.Row
        obj.FocusRect = FocusRectEnum.None
    End Sub
    Public Sub InitEditGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
        obj.Cols.Fixed = 0
        obj.Rows.Fixed = 1
        obj.AllowEditing = True
        obj.SelectionMode = SelectionModeEnum.Default
        obj.Select(1, 0, 1, 0, True)
    End Sub
    Public Sub InitTargetNormalList(ByVal index As Integer)
        Dim aTarget As Target
        aTarget = myTargets(index)
        MainWin.TargetNormalType.Items.Clear()
        MainWin.TargetNormalType.Items.Add("User Specified")
        If aTarget.Compartment >= 0 Then
            If aTarget.ZPosition <> myCompartments(aTarget.Compartment).RoomHeight Then MainWin.TargetNormalType.Items.Add("Ceiling")
        End If
        If aTarget.ZPosition <> 0 Then MainWin.TargetNormalType.Items.Add("Floor")
        If aTarget.YPosition <> 0 Then MainWin.TargetNormalType.Items.Add("Front Wall")
        If aTarget.Compartment >= 0 Then
            If aTarget.YPosition <> myCompartments(aTarget.Compartment).RoomDepth Then MainWin.TargetNormalType.Items.Add("Back Wall")
        End If
        If aTarget.XPosition <> 0 Then MainWin.TargetNormalType.Items.Add("Left Wall")
        If aTarget.Compartment >= 0 Then
            If aTarget.XPosition <> myCompartments(aTarget.Compartment).RoomWidth Then MainWin.TargetNormalType.Items.Add("Right Wall")
        End If
        numFires = myFires.Count
        If numFires > 0 Then
            Dim aFire As Fire
            For i = 1 To numFires
                aFire = myFires(i - 1)
                If aTarget.Compartment = aFire.Compartment And (aFire.XPosition - aTarget.XPosition <> 0 Or aFire.YPosition - aTarget.YPosition <> 0 Or aTarget.ZPosition <> 0) Then
                    MainWin.TargetNormalType.Items.Add("Fire " + i.ToString + ", " + aFire.Name)
                End If
            Next
        End If
    End Sub
    Public Sub UpdateLogFile(LogTextBox As TextBox)
        Dim LogFileExists As Boolean, FileName As String, IO As Integer = 1
        FileName = myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".log"
        LogFileExists = System.IO.File.Exists(FileName)
        If LogFileExists Then
            LogTextBox.Text = ""
            Dim ln As String
            FileOpen(IO, FileName, OpenMode.Input)
            Do Until EOF(IO)
                ln = LineInput(IO)
                If Not ln.StartsWith("Write to the history") Then myErrors.Add(ln, ErrorMessages.TypeCFastLog)
            Loop
            FileClose(IO)
            Menu()
            Environment()
        End If
    End Sub
#End Region
End Class
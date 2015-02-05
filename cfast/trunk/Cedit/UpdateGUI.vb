Imports C1.Win.C1FlexGrid
Public Class UpdateGUI
    Public DoErrorCheck As Boolean = True

    Private MainWin As CeditMain
    Private AreaPoints() As Single, HeightPoints() As Single, OpeningTimes() As Single, OpeningFractions() As Single
    Private NumPoints As Integer, i As Integer, j As Integer, NumberofCompartments As Integer
    Private NumHVents As Integer, numVVents As Integer, numMVents As Integer, NumVHeats As Integer, NumHHeats As Integer, _
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

        MainWin.MenuDetailedOutput.Checked = DetailedCFASTOutput
        MainWin.MenuTotalMassOutput.Checked = TotalMassCFASTOutput
        MainWin.MenuNetHeatFluxOutput.Checked = NetHeatFluxCFASTOutput
        MainWin.MenuShowCFAST.Checked = CommandWindowVisible
    End Sub
    Public Sub General()
        If myEnvironment.InputFileName = Nothing Then
            MainWin.Text = "Newfile"
        Else
            MainWin.Text = "CEdit (" + System.IO.Path.GetFileName(myEnvironment.InputFileName) + ")"
        End If
        If myEnvironment.FileChanged Then MainWin.Text = MainWin.Text + " *"
        If DoErrorCheck Then UpdateErrorCheck()
    End Sub
    Private Sub UpdateErrorCheck()
        Dim ErrorCount As Integer
        ErrorCount = myEnvironment.IsValid + myCompartments.IsValid + myHVents.IsValid + myVVents.IsValid + myMVents.IsValid + myDetectors.IsValid + _
            myTargets.IsValid + myFires.IsValid + myFireObjects.IsValid + myHHeats.IsValid + myVHeats.IsValid
        If ErrorCount > 0 Then
            myErrors.Break()
            MainWin.StatusBar.Panels(0).Text = ErrorCount.ToString + " Errors or Messages"
            MainWin.StatusBar.Panels(1).Text = myErrors.TopError
        Else
            MainWin.StatusBar.Panels(0).Text = "No Errors"
            MainWin.StatusBar.Panels(1).Text = ""
        End If
    End Sub
    Public Sub Environment()
        Me.General()
        MainWin.EnvTitle.Text = myEnvironment.Title
        MainWin.EnvSimTime.Text = myEnvironment.SimulationTime.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvTextOutInterval.Text = myEnvironment.OutputInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvSpreadOutInterval.Text = myEnvironment.SpreadsheetInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvSmokeviewInterval.Text = myEnvironment.SmokeviewInterval.ToString + myUnits.Convert(UnitsNum.Time).Units
        MainWin.EnvIntAmbTemp.Text = myEnvironment.IntAmbTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
        MainWin.EnvIntAmbPress.Text = myEnvironment.IntAmbPressure.ToString + myUnits.Convert(UnitsNum.Pressure).Units
        MainWin.EnvIntAmbElevation.Text = myEnvironment.IntAmbElevation.ToString + myUnits.Convert(UnitsNum.Length).Units
        MainWin.EnvExtAmbTemp.Text = myEnvironment.ExtAmbTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
        MainWin.EnvExtAmbPress.Text = myEnvironment.ExtAmbPressure.ToString + myUnits.Convert(UnitsNum.Pressure).Units
        MainWin.EnvExtAmbElevation.Text = myEnvironment.ExtAmbElevation.ToString + myUnits.Convert(UnitsNum.Length).Units

        Dim value As Single
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
    Public Sub Visuals(ByVal index As Integer)
        Dim aVisual As Visual
        Me.General()
        If index < 0 Or myVisuals.Count = 0 Then
            ClearGrid(MainWin.VisualSummary)
        Else
            ' fill the visualization widgets from the supplied visualization data
            aVisual = myVisuals.Item(index)
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
        End If
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
            MainWin.VisualSummary.Select(index + 1, 0, index + 1, MainWin.VisualSummary.Cols.Count - 1, True)
        End If
    End Sub
    Public Sub Geometry(ByVal index As Integer)
        Me.General()
        If index < 0 Or myCompartments.Count = 0 Then
            ClearGrid(MainWin.CompSummary)
            MainWin.TabHorizontalFlow.Enabled = False
            MainWin.TabVerticalFlow.Enabled = False
            MainWin.TabMechanicalFlow.Enabled = False
            MainWin.TabTargets.Enabled = False
            MainWin.TabDetection.Enabled = False
            MainWin.TabHeatTransfer.Enabled = False
            MainWin.TabFires.Enabled = False
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
            MainWin.GroupCompartments.Enabled = True
            MainWin.GroupCompartments.Text = "Compartment " + (index + 1).ToString + " (of " + myCompartments.Count.ToString + ")"
            MainWin.CompName.Text = aCompartment.Name
            MainWin.CompWidth.Text = aCompartment.RoomWidth.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompDepth.Text = aCompartment.RoomDepth.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompHeight.Text = aCompartment.RoomHeight.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompXPosition.Text = aCompartment.RoomOriginX.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompYPosition.Text = aCompartment.RoomOriginY.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.CompZPosition.Text = aCompartment.RoomOriginZ.ToString + myUnits.Convert(UnitsNum.Length).Units

            MainWin.CompCeiling.Text = myThermalProperties.GetLongName(aCompartment.CeilingMaterial)
            If MainWin.CompCeiling.Text <> "Default" And MainWin.CompCeiling.Text <> "Off" Then
                Dim aThermalProperty As New ThermalProperty
                aThermalProperty = myThermalProperties(myThermalProperties.GetIndex(aCompartment.CeilingMaterial))
                MainWin.CompConductCeiling.Text = "Conductivity: " + aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
                MainWin.CompSpecHeatCeiling.Text = "Specific Heat: " + aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
                MainWin.CompDensityCeiling.Text = "Density: " + aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
                MainWin.CompThicknessCeiling.Text = "Thickness: " + aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
            Else
                MainWin.CompConductCeiling.Text = "Conductivity:"
                MainWin.CompSpecHeatCeiling.Text = "Specific Heat: "
                MainWin.CompDensityCeiling.Text = "Density: "
                MainWin.CompThicknessCeiling.Text = "Thickness: "
            End If
            MainWin.CompWalls.Text = myThermalProperties.GetLongName(aCompartment.WallMaterial)
            If MainWin.CompWalls.Text <> "Default" And MainWin.CompWalls.Text <> "Off" Then
                Dim aThermalProperty As New ThermalProperty
                aThermalProperty = myThermalProperties(myThermalProperties.GetIndex(aCompartment.WallMaterial))
                MainWin.CompConductWalls.Text = "Conductivity: " + aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
                MainWin.CompSpecHeatWalls.Text = "Specific Heat: " + aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
                MainWin.CompDensityWalls.Text = "Density: " + aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
                MainWin.CompThicknessWalls.Text = "Thickness: " + aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
            Else
                MainWin.CompConductWalls.Text = "Conductivity:"
                MainWin.CompSpecHeatWalls.Text = "Specific Heat: "
                MainWin.CompDensityWalls.Text = "Density: "
                MainWin.CompThicknessWalls.Text = "Thickness: "
            End If
            MainWin.CompFloor.Text = myThermalProperties.GetLongName(aCompartment.FloorMaterial)
            If MainWin.CompFloor.Text <> "Default" And MainWin.CompFloor.Text <> "Off" Then
                Dim aThermalProperty As New ThermalProperty
                aThermalProperty = myThermalProperties(myThermalProperties.GetIndex(aCompartment.FloorMaterial))
                MainWin.CompConductFloor.Text = "Conductivity: " + aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
                MainWin.CompSpecHeatFloor.Text = "Specific Heat: " + aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
                MainWin.CompDensityFloor.Text = "Density: " + aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
                MainWin.CompThicknessFloor.Text = "Thickness: " + aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
            Else
                MainWin.CompConductFloor.Text = "Conductivity:"
                MainWin.CompSpecHeatFloor.Text = "Specific Heat: "
                MainWin.CompDensityFloor.Text = "Density: "
                MainWin.CompThicknessFloor.Text = "Thickness: "
            End If
            If aCompartment.Shaft = True Then
                MainWin.CompFlow.SelectedIndex = Compartment.TypeShaft
                MainWin.CompDecayVelocity.Text = " "
                MainWin.CompDecayDepth.Text = " "
                MainWin.CompDecayDistance.Text = " "
                MainWin.CompDecayVelocity.Visible = False
                MainWin.CompDecayDepth.Visible = False
                MainWin.CompDecayDistance.Visible = False
                MainWin.CompVelocityLabel.Visible = False
                MainWin.CompDepthLabel.Visible = False
                MainWin.CompDistanceLabel.Visible = False
            End If
            If aCompartment.Hall = True Then
                MainWin.CompFlow.SelectedIndex = Compartment.TypeCorridor
                If aCompartment.HallVelocity <= 0 Then
                    MainWin.CompDecayVelocity.Text = "Default"
                Else
                    MainWin.CompDecayVelocity.Text = aCompartment.HallVelocity.ToString + myUnits.Convert(UnitsNum.Velocity).Units
                End If
                If aCompartment.HallDecayDistance <= 0 Then
                    MainWin.CompDecayDistance.Text = "Default"
                Else
                    MainWin.CompDecayDistance.Text = aCompartment.HallDecayDistance.ToString + myUnits.Convert(UnitsNum.Length).Units
                End If
                If aCompartment.HallDepth <= 0 Then
                    MainWin.CompDecayDepth.Text = "Default"
                Else
                    MainWin.CompDecayDepth.Text = aCompartment.HallDepth.ToString + myUnits.Convert(UnitsNum.Length).Units
                End If
                MainWin.CompDecayVelocity.Visible = True
                MainWin.CompDecayDepth.Visible = True
                MainWin.CompDecayDistance.Visible = True
                MainWin.CompVelocityLabel.Visible = True
                MainWin.CompDepthLabel.Visible = True
                MainWin.CompDistanceLabel.Visible = True
            End If
            If aCompartment.Shaft = False And aCompartment.Hall = False Then
                MainWin.CompFlow.SelectedIndex = Compartment.TypeNormal
                MainWin.CompDecayVelocity.Visible = False
                MainWin.CompDecayDepth.Visible = False
                MainWin.CompDecayDistance.Visible = False
                MainWin.CompVelocityLabel.Visible = False
                MainWin.CompDepthLabel.Visible = False
                MainWin.CompDistanceLabel.Visible = False
            End If
            aCompartment.GetVariableArea(AreaPoints, HeightPoints, NumPoints)
            ClearGrid(MainWin.CompVariableArea)
            If NumPoints > 0 Then
                For i = 1 To NumPoints
                    MainWin.CompVariableArea(i, 0) = HeightPoints(i).ToString + myUnits.Convert(UnitsNum.Length).Units
                    MainWin.CompVariableArea(i, 1) = AreaPoints(i).ToString + myUnits.Convert(UnitsNum.Area).Units
                Next
            End If
            NumberofCompartments = myCompartments.Count
            ClearGrid(MainWin.CompSummary)
            If NumberofCompartments > 0 Then
                For i = 1 To NumberofCompartments
                    aCompartment = myCompartments.Item(i - 1)
                    MainWin.CompSummary(i, 0) = aCompartment.Name
                    MainWin.CompSummary(i, 1) = i.ToString
                    MainWin.CompSummary(i, 2) = aCompartment.RoomWidth.ToString
                    MainWin.CompSummary(i, 3) = aCompartment.RoomDepth.ToString
                    MainWin.CompSummary(i, 4) = aCompartment.RoomHeight.ToString
                    MainWin.CompSummary(i, 5) = aCompartment.RoomOriginX.ToString
                    MainWin.CompSummary(i, 6) = aCompartment.RoomOriginY.ToString
                    MainWin.CompSummary(i, 7) = aCompartment.RoomOriginZ.ToString
                    MainWin.CompSummary(i, 8) = aCompartment.CeilingMaterial.ToLower
                    MainWin.CompSummary(i, 9) = aCompartment.WallMaterial.ToLower
                    MainWin.CompSummary(i, 10) = aCompartment.FloorMaterial.ToLower
                    MainWin.CompSummary(i, 11) = myFires.NumberofConnections(i - 1)
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
        Me.General()
        If index < 0 Or index >= myHVents.Count Then
            ClearGrid(MainWin.HVentSummary)
            MainWin.GroupHVentGeometry.Enabled = False
        Else
            MainWin.GroupHVentGeometry.Enabled = True
            MainWin.GroupHVentGeometry.Text = "Vent " + (index + 1).ToString + " (of " + myHVents.Count.ToString + ") Geometry"
            Dim aVent As New Vent
            aVent = myHVents(index)
            If aVent.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.HVentComp1.SelectedIndex = aVent.FirstCompartment + 1
            End If
            If aVent.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.HVentComp2.SelectedIndex = aVent.SecondCompartment + 1
            End If
            MainWin.HVentOffset1.Text = aVent.FirstOffset.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentOffset2.Text = aVent.SecondOffset.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentSill.Text = aVent.Sill.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentSoffit.Text = aVent.Soffit.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentWidth.Text = aVent.Width.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.HVentWindAngle.Text = aVent.WindAngle.ToString + " °"
            MainWin.HVentInitialFraction.Text = aVent.InitialOpening.ToString
            MainWin.HVentFinalFraction.Text = aVent.FinalOpening.ToString
            MainWin.HVentFractionTime.Text = aVent.FinalOpeningTime.ToString + myUnits.Convert(UnitsNum.Time).Units
            MainWin.HVentFace.SelectedIndex = aVent.Face - 1
            NumHVents = myHVents.Count
            ClearGrid(MainWin.HVentSummary)
            If NumHVents > 0 Then
                For i = 1 To NumHVents
                    aVent = myHVents(i - 1)
                    MainWin.HVentSummary(i, 0) = i.ToString
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.HVentSummary(i, 1) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.HVentSummary(i, 1) = "Outside"
                    Else
                        MainWin.HVentSummary(i, 1) = "Not defined"
                    End If
                    MainWin.HVentSummary(i, 2) = aVent.FirstOffset.ToString
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.HVentSummary(i, 3) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.HVentSummary(i, 3) = "Outside"
                    Else
                        MainWin.HVentSummary(i, 3) = "Not defined"
                    End If
                    MainWin.HVentSummary(i, 4) = aVent.SecondOffset.ToString
                    MainWin.HVentSummary(i, 5) = aVent.Sill.ToString
                    MainWin.HVentSummary(i, 6) = aVent.Soffit.ToString
                    MainWin.HVentSummary(i, 7) = aVent.Width.ToString
                    MainWin.HVentSummary(i, 8) = aVent.WindAngle.ToString
                    MainWin.HVentSummary(i, 9) = aVent.InitialOpening.ToString
                    MainWin.HVentSummary(i, 10) = FaceNames.Substring((aVent.Face - 1) * 5, 5)
                Next
                MainWin.HVentSummary.Select(index + 1, 0, index + 1, MainWin.HVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub VVents(ByVal index As Integer)
        Me.General()
        If index < 0 Or index >= myVVents.Count Then
            ClearGrid(MainWin.VVentSummary)
            MainWin.GroupVVents.Enabled = False
        Else
            MainWin.GroupVVents.Enabled = True
            MainWin.GroupVVents.Text = "Vent " + (index + 1).ToString + " (of " + myVVents.Count.ToString + ") Geometry"
            Dim aVent As New Vent
            aVent = myVVents(index)
            If aVent.FirstCompartment <= myCompartments.Count - 1 Then
                MainWin.VVentCompTop.SelectedIndex = aVent.FirstCompartment + 1
            End If
            If aVent.SecondCompartment <= myCompartments.Count - 1 Then
                MainWin.VVentCompBottom.SelectedIndex = aVent.SecondCompartment + 1
            End If
            MainWin.VVentArea.Text = aVent.Area.ToString + myUnits.Convert(UnitsNum.Area).Units
            MainWin.VVentShape.SelectedIndex = aVent.Shape - 1
            MainWin.VVentInitialFraction.Text = aVent.InitialOpening.ToString
            MainWin.VVentFinalFraction.Text = aVent.FinalOpening.ToString
            MainWin.VVentFractionTime.Text = aVent.FinalOpeningTime.ToString + myUnits.Convert(UnitsNum.Time).Units
            numVVents = myVVents.Count
            ClearGrid(MainWin.VVentSummary)
            If numVVents > 0 Then
                For i = 1 To numVVents
                    aVent = myVVents(i - 1)
                    MainWin.VVentSummary(i, 0) = i.ToString
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.VVentSummary(i, 1) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.VVentSummary(i, 1) = "Outside"
                    Else
                        MainWin.VVentSummary(i, 1) = "Not defined"
                    End If
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.VVentSummary(i, 2) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.VVentSummary(i, 2) = "Outside"
                    Else
                        MainWin.VVentSummary(i, 2) = "Not defined"
                    End If
                    MainWin.VVentSummary(i, 3) = aVent.Area.ToString
                    MainWin.VVentSummary(i, 4) = ShapeNames.Substring((aVent.Shape - 1) * 6, 6)
                Next
                MainWin.VVentSummary.Select(index + 1, 0, index + 1, MainWin.VVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub MVents(ByVal index As Integer)
        Me.General()
        If index < 0 Or index >= myMVents.Count Then
            ClearGrid(MainWin.MVentSummary)
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

            MainWin.MVentFlow.Text = aVent.FlowRate.ToString + myUnits.Convert(UnitsNum.Flowrate).Units
            MainWin.MVentDropoff.Text = aVent.BeginFlowDropoff.ToString + myUnits.Convert(UnitsNum.Pressure).Units
            MainWin.MVentZero.Text = aVent.ZeroFlow.ToString + myUnits.Convert(UnitsNum.Pressure).Units

            MainWin.MVentInitialFraction.Text = aVent.InitialOpening.ToString
            MainWin.MVentFinalFraction.Text = aVent.FinalOpening.ToString
            MainWin.MVentFractionTime.Text = aVent.FinalOpeningTime.ToString + myUnits.Convert(UnitsNum.Time).Units

            MainWin.MVentFilterEfficiency.Text = aVent.FilterEfficiency.ToString + " %"
            MainWin.MVentFilterTime.Text = aVent.FilterTime.ToString + myUnits.Convert(UnitsNum.Time).Units

            numMVents = myMVents.Count
            ClearGrid(MainWin.MVentSummary)
            If numMVents > 0 Then
                For i = 1 To numMVents
                    aVent = myMVents(i - 1)
                    MainWin.MVentSummary(i, 0) = i.ToString
                    If aVent.FirstCompartment >= 0 And aVent.FirstCompartment <= myCompartments.Count - 1 Then
                        MainWin.MVentSummary(i, 1) = myCompartments(aVent.FirstCompartment).Name
                    ElseIf aVent.FirstCompartment = -1 Then
                        MainWin.MVentSummary(i, 1) = "Outside"
                    Else
                        MainWin.MVentSummary(i, 1) = "Not defined"
                    End If
                    MainWin.MVentSummary(i, 2) = aVent.FirstArea.ToString
                    MainWin.MVentSummary(i, 3) = aVent.FirstCenterHeight.ToString
                    MainWin.MVentSummary(i, 4) = OrientationNames.Substring((aVent.FirstOrientation - 1) * 10, 10)
                    If aVent.SecondCompartment >= 0 And aVent.SecondCompartment <= myCompartments.Count - 1 Then
                        MainWin.MVentSummary(i, 5) = myCompartments(aVent.SecondCompartment).Name
                    ElseIf aVent.SecondCompartment = -1 Then
                        MainWin.MVentSummary(i, 5) = "Outside"
                    Else
                        MainWin.MVentSummary(i, 5) = "Not defined"
                    End If
                    MainWin.MVentSummary(i, 6) = aVent.SecondArea.ToString
                    MainWin.MVentSummary(i, 7) = aVent.SecondCenterHeight.ToString
                    MainWin.MVentSummary(i, 8) = OrientationNames.Substring((aVent.SecondOrientation - 1) * 10, 10)
                    MainWin.MVentSummary(i, 9) = aVent.FlowRate.ToString
                    MainWin.MVentSummary(i, 10) = aVent.BeginFlowDropoff.ToString
                    MainWin.MVentSummary(i, 11) = aVent.ZeroFlow.ToString
                Next
                MainWin.MVentSummary.Select(index + 1, 0, index + 1, MainWin.MVentSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Targets(ByVal index As Integer)
        Me.General()
        If index < 0 Or index >= myTargets.Count Then
            ClearGrid(MainWin.TargetSummary)
            MainWin.GroupTargets.Enabled = False
        Else
            MainWin.GroupTargets.Enabled = True
            MainWin.GroupTargets.Text = "Target " + (index + 1).ToString + " (of " + myTargets.Count.ToString + ") Geometry"
            Dim aTarget As New Target, aCompartment As Compartment
            aTarget = myTargets(index)
            If aTarget.Compartment <= -1 Then
                MainWin.TargetComp.SelectedIndex = aTarget.Compartment + 1
            ElseIf aTarget.Compartment >= 0 And aTarget.Compartment <= myCompartments.Count - 1 Then
                MainWin.TargetComp.SelectedIndex = aTarget.Compartment + 1
                aCompartment = myCompartments(aTarget.Compartment)
            End If

            MainWin.TargetXPosition.Text = aTarget.XPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.TargetYPosition.Text = aTarget.YPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.TargetZPosition.Text = aTarget.ZPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.TargetXNormal.Text = aTarget.XNormal.ToString
            MainWin.TargetYNormal.Text = aTarget.YNormal.ToString
            MainWin.TargetZNormal.Text = aTarget.ZNormal.ToString
            MainWin.TargetMaterial.Text = myThermalProperties.GetLongName(aTarget.Material)
            MainWin.TargetSolutionMethod.SelectedIndex = aTarget.SolutionMethod
            MainWin.TargetSolutionThickness.SelectedIndex = aTarget.SolutionThickness
            If aTarget.SolutionThickness = 2 Then
                MainWin.TargetSolutionMethod.Enabled = False
            Else
                MainWin.TargetSolutionMethod.Enabled = True
            End If
            MainWin.TargetInternalLocation.Text = aTarget.InternalLocation.ToString
            If aTarget.Material <> "Default" And aTarget.Material <> "Off" Then
                Dim aThermalProperty As New ThermalProperty
                aThermalProperty = myThermalProperties(myThermalProperties.GetIndex(aTarget.Material))
                MainWin.TargetConduct.Text = "Conductivity: " + aThermalProperty.Conductivity.ToString + myUnits.Convert(UnitsNum.Conductivity).Units
                MainWin.TargetSpecHeat.Text = "Specific Heat: " + aThermalProperty.SpecificHeat.ToString + myUnits.Convert(UnitsNum.SpecificHeat).Units
                MainWin.TargetDensity.Text = "Density: " + aThermalProperty.Density.ToString + myUnits.Convert(UnitsNum.Density).Units
                If MainWin.TargetSolutionThickness.SelectedIndex = Target.Cylindrical Then
                    MainWin.TargetThickness.Text = "Diameter: " + aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
                Else
                    MainWin.TargetThickness.Text = "Thickness: " + aThermalProperty.Thickness.ToString + myUnits.Convert(UnitsNum.Length).Units
                End If
            Else
                MainWin.TargetConduct.Text = "Conductivity:"
                MainWin.TargetSpecHeat.Text = "Specific Heat: "
                MainWin.TargetDensity.Text = "Density: "
                MainWin.TargetThickness.Text = "Thickness: "
            End If

            numTargets = myTargets.Count
            ClearGrid(MainWin.TargetSummary)
            If numTargets > 0 Then
                For i = 1 To numTargets
                    aTarget = myTargets(i - 1)
                    MainWin.TargetSummary(i, 0) = i.ToString
                    If aTarget.Compartment >= 0 And aTarget.Compartment <= myCompartments.Count - 1 Then
                        MainWin.TargetSummary(i, 1) = myCompartments(aTarget.Compartment).Name
                    ElseIf aTarget.Compartment = -1 Then
                        MainWin.TargetSummary(i, 1) = "Outside"
                    Else
                        MainWin.TargetSummary(i, 1) = "Not defined"
                    End If
                    MainWin.TargetSummary(i, 2) = aTarget.XPosition.ToString
                    MainWin.TargetSummary(i, 3) = aTarget.YPosition.ToString
                    MainWin.TargetSummary(i, 4) = aTarget.ZPosition.ToString
                    MainWin.TargetSummary(i, 5) = aTarget.XNormal.ToString
                    MainWin.TargetSummary(i, 6) = aTarget.YNormal.ToString
                    MainWin.TargetSummary(i, 7) = aTarget.ZNormal.ToString
                    MainWin.TargetSummary(i, 8) = aTarget.Material
                    MainWin.TargetSummary(i, 9) = SolutionMethodNames.Substring((aTarget.SolutionMethod) * 8, 8)
                    MainWin.TargetSummary(i, 10) = SolutionThicknessNames.Substring((aTarget.SolutionThickness) * 11, 11)
                Next
                MainWin.TargetSummary.Select(index + 1, 0, index + 1, MainWin.TargetSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Detectors(ByVal index As Integer)
        Me.General()
        If index < 0 Or index >= myDetectors.Count Then
            ClearGrid(MainWin.DetectorSummary)
            MainWin.GroupDetectors.Enabled = False
        Else
            MainWin.GroupDetectors.Enabled = True
            MainWin.GroupDetectors.Text = "Alarm " + (index + 1).ToString + " (of " + myDetectors.Count.ToString + ")"
            Dim aDetector As New Target
            aDetector = myDetectors(index)
            MainWin.DetectorType.SelectedIndex = aDetector.DetectorType
            If aDetector.Compartment <= myCompartments.Count - 1 Then
                MainWin.DetectorComp.SelectedIndex = aDetector.Compartment + 1
            End If
            MainWin.DetectorActivation.Text = aDetector.ActivationTemperature.ToString + myUnits.Convert(UnitsNum.Temperature).Units
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
                    If aDetector.Compartment >= 0 And aDetector.Compartment <= myCompartments.Count - 1 Then
                        MainWin.DetectorSummary(i, 1) = myCompartments(aDetector.Compartment).Name
                    ElseIf aDetector.Compartment = -1 Then
                        MainWin.DetectorSummary(i, 1) = "Outside"
                    Else
                        MainWin.DetectorSummary(i, 1) = "Not defined"
                    End If
                    MainWin.DetectorSummary(i, 2) = DetectorTypes.Substring(aDetector.DetectorType * 9, 9)
                    MainWin.DetectorSummary(i, 3) = aDetector.XPosition.ToString
                    MainWin.DetectorSummary(i, 4) = aDetector.YPosition.ToString
                    MainWin.DetectorSummary(i, 5) = aDetector.ZPosition.ToString
                    MainWin.DetectorSummary(i, 6) = aDetector.ActivationTemperature.ToString
                    MainWin.DetectorSummary(i, 7) = aDetector.RTI.ToString
                    MainWin.DetectorSummary(i, 8) = aDetector.SprayDensity.ToString
                Next
                MainWin.DetectorSummary.Select(index + 1, 0, index + 1, MainWin.DetectorSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Public Sub Heats(ByVal Hindex As Integer, ByVal Vindex As Integer)
        Me.General()
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
        Me.General()
        Dim afireTimeSeries(12, 0) As Single, NumPoints As Integer
        Dim PeakHRR As Single, xFire As Single, yFire As Single, xRoom As Single, yRoom As Single
        Dim IgnitionTypeLabel As String = ""
        If index < 0 Or index >= myFires.Count Then
            ClearGrid(MainWin.FireSummary)
            MainWin.GroupFire.Enabled = False
            MainWin.GroupFireObject.Enabled = False
        Else
            Dim aFireObject As New Fire
            MainWin.FireLOL.Text = myEnvironment.LowerOxygenLimit.ToString + " %"
            MainWin.FireIgnitionTemperature.Text = myEnvironment.IgnitionTemp.ToString + myUnits.Convert(UnitsNum.Temperature).Units

            MainWin.GroupFire.Enabled = True
            MainWin.GroupFire.Text = "Location, Fire " + (index + 1).ToString + " (of " + myFires.Count.ToString + ")"
            MainWin.GroupFireObject.Enabled = True
            MainWin.FireType.Text = "Constrained"
            Dim aFire As New Fire
            aFire = myFires(index)
            If aFire.Compartment >= 0 And aFire.Compartment <= myCompartments.Count - 1 Then
                MainWin.FireComp.SelectedIndex = aFire.Compartment + 1
                Dim aCompartment As New Compartment
                aCompartment = myCompartments(aFire.Compartment)
                xFire = aFire.XPosition
                yFire = aFire.YPosition
                xRoom = aCompartment.RoomWidth
                yRoom = aCompartment.RoomDepth
                If xFire = 0.0 Or xFire = xRoom Or yFire = 0.0 Or yFire = yRoom Then MainWin.FireType.Text = "Constrained, Wall Fire"
                If (xFire = 0.0 And yFire = 0.0) Or (xFire = 0.0 And yFire = yRoom) Or (xFire = xRoom And yFire = 0.0) Or (xFire = xRoom And yFire = yRoom) Then MainWin.FireType.Text = "Constrained, Corner Fire"

            End If

            MainWin.FireXPosition.Text = aFire.XPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.FireYPosition.Text = aFire.YPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.FireZPosition.Text = aFire.ZPosition.ToString + myUnits.Convert(UnitsNum.Length).Units
            MainWin.FireXNormal.Text = aFire.XNormal.ToString
            MainWin.FireYNormal.Text = aFire.YNormal.ToString
            MainWin.FireZNormal.Text = aFire.ZNormal.ToString
            MainWin.FirePlumeType.SelectedIndex = aFire.PlumeType
            MainWin.FireIgnitionCriteria.SelectedIndex = aFire.IgnitionType
            If aFire.IgnitionType = Fire.FireIgnitionbyTime Then IgnitionTypeLabel = myUnits.Convert(UnitsNum.Time).Units
            If aFire.IgnitionType = Fire.FireIgnitionbyTemperature Then IgnitionTypeLabel = myUnits.Convert(UnitsNum.Temperature).Units
            If aFire.IgnitionType = Fire.FireIgnitionbyFlux Then IgnitionTypeLabel = myUnits.Convert(UnitsNum.HeatFlux).Units
            MainWin.FireIgnitionValue.Text = " "
            If aFire.IgnitionType >= 0 Then MainWin.FireIgnitionValue.Text = aFire.IgnitionValue.ToString + IgnitionTypeLabel
            MainWin.FireName.SelectedIndex = aFire.FireObject
            If aFire.FireObject < 0 Or myFireObjects.Count <= aFire.FireObject Then
                MainWin.FireName.SelectedIndex = -1
                MainWin.FireMaterial.Text = "Material:"
                MainWin.FireFormula.Text = "Formula:"
                MainWin.FireMolarMass.Text = "Molar Mass:"""
                MainWin.FireHoC.Text = "Heat of Combustion:"
                MainWin.FirePeakCO.Text = "Peak CO Yield:"
                MainWin.FirePeakHCN.Text = "Peak HCN Yield:"
                MainWin.FirePeakHCl.Text = "Peak HCl Yield:"
                MainWin.FirePeakSoot.Text = "Peak Soot Yield:"
                MainWin.FirePeakHeight.Text = "Peak Fire Height:"
                MainWin.FirePeakArea.Text = "Peak Area::"
                MainWin.FireRadiativeFraction.Text = "Radiative Fraction:"
                MainWin.FireObjectPlot.Clear()
                MainWin.FireObjectPlot.Refresh()
            Else
                aFireObject = myFireObjects(aFire.FireObject)
                MainWin.FireMaterial.Text = "Material: " + myThermalProperties.GetLongName(aFireObject.Material)
                MainWin.FireFormula.Text = "Formula: " + aFireObject.ChemicalFormula()
                MainWin.FireMolarMass.Text = "Molar Mass: " + aFireObject.MolarMass.ToString + myUnits.Convert(UnitsNum.Mass).Units + "/mol"
                MainWin.FireHoC.Text = "Heat of Combustion: " + aFireObject.HeatofCombustion.ToString + myUnits.Convert(UnitsNum.HoC).Units
                MainWin.FirePeakCO.Text = "Peak CO Yield: " + aFireObject.Peak(Fire.FireCO).ToString + myUnits.Convert(UnitsNum.Mass).Units + "/" + myUnits.Convert(UnitsNum.Mass).Units
                MainWin.FirePeakHCN.Text = "Peak HCN Yield: " + aFireObject.Peak(Fire.FireHCN).ToString + myUnits.Convert(UnitsNum.Mass).Units + "/" + myUnits.Convert(UnitsNum.Mass).Units
                MainWin.FirePeakHCl.Text = "Peak HCl Yield: " + aFireObject.Peak(Fire.FireHCl).ToString + myUnits.Convert(UnitsNum.Mass).Units + "/" + myUnits.Convert(UnitsNum.Mass).Units
                MainWin.FirePeakSoot.Text = "Peak Soot Yield: " + aFireObject.Peak(Fire.FireSoot).ToString + myUnits.Convert(UnitsNum.Mass).Units + "/" + myUnits.Convert(UnitsNum.Mass).Units
                MainWin.FirePeakHeight.Text = "Peak Fire Height: " + aFireObject.Peak(Fire.FireHeight).ToString + myUnits.Convert(UnitsNum.Length).Units
                MainWin.FirePeakArea.Text = "Peak Fire Area: " + aFireObject.Peak(Fire.FireArea).ToString + myUnits.Convert(UnitsNum.Area).Units
                MainWin.FireRadiativeFraction.Text = "Radiative Fraction: " + aFireObject.RadiativeFraction.ToString
                UpdateFirePlot(aFire.FireObject)
            End If
            numFires = myFires.Count
            ClearGrid(MainWin.FireSummary)
            If numFires > 0 Then
                For i = 1 To numFires
                    aFire = myFires(i - 1)
                    MainWin.FireSummary(i, 0) = i.ToString
                    If aFire.Compartment >= 0 And aFire.Compartment <= myCompartments.Count - 1 Then
                        MainWin.FireSummary(i, 1) = myCompartments(aFire.Compartment).Name
                    ElseIf aFire.Compartment = -1 Then
                        MainWin.FireSummary(i, 1) = "Outside"
                    Else
                        MainWin.FireSummary(i, 1) = "Not defined"
                    End If
                    If aFire.FireType >= 0 Then MainWin.FireSummary(i, 3) = FireTypeNames.Substring((aFire.FireType) * 11, 11)
                    If aFire.IgnitionType >= 0 Then
                        MainWin.FireSummary(i, 4) = IgnitionNames.Substring((aFire.IgnitionType) * 11, 11)
                        MainWin.FireSummary(i, 5) = aFire.IgnitionValue
                    End If
                    MainWin.FireSummary(i, 6) = aFire.XPosition.ToString
                    MainWin.FireSummary(i, 7) = aFire.YPosition.ToString
                    MainWin.FireSummary(i, 8) = aFire.ZPosition.ToString
                    If aFire.FireObject < 0 Or myFireObjects.Count <= aFire.FireObject Then
                        MainWin.FireSummary(i, 2) = "Not Defined"
                        MainWin.FireSummary(i, 9) = " "
                    Else
                        aFireObject = myFireObjects(aFire.FireObject)
                        MainWin.FireSummary(i, 2) = aFireObject.Name
                        PeakHRR = 0.0
                        aFireObject.GetFireData(afireTimeSeries, NumPoints)
                        For j = 0 To NumPoints
                            If afireTimeSeries(Fire.FireHRR, j) > PeakHRR Then PeakHRR = afireTimeSeries(Fire.FireHRR, j)
                        Next
                        MainWin.FireSummary(i, 9) = PeakHRR.ToString
                    End If
                Next
                MainWin.FireSummary.Select(index + 1, 0, index + 1, MainWin.FireSummary.Cols.Count - 1, True)
            End If
        End If
    End Sub
    Private Sub UpdateFirePlot(ByVal index As Integer)
        Dim aFireObject As New Fire
        Dim aFireData(12, 0) As Single, numPoints As Integer
        Dim x() As Single, y() As Single, j As Integer
        aFireObject = myFireObjects(index)
        MainWin.FireObjectPlot.Clear()
        aFireObject.GetFireData(aFireData, numPoints)
        ReDim x(numPoints), y(numPoints)
        For j = 0 To numPoints
            x(j) = aFireData(Fire.FireTime, j)
            y(j) = aFireData(Fire.FireHRR, j)
        Next
        Dim lp As New NPlot.LinePlot(y, x)
        MainWin.FireObjectPlot.Add(lp)
        MainWin.FireObjectPlot.Title = aFireObject.Name + " HRR"
        MainWin.FireObjectPlot.Refresh()
    End Sub
    Private Sub ClearGrid(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid)
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
    Public Sub InitThermalPropertyList(ByVal obj As ComboBox)
        Dim i As Integer
        obj.Items.Clear()
        obj.Items.Add("Off")
        obj.SelectedIndex = 0
        If myThermalProperties.Count > 0 Then
            For i = 0 To myThermalProperties.Count - 1
                obj.Items.Add(myThermalProperties.Item(i).Name)
            Next
        End If
    End Sub
    Public Sub InitCompartmentList(ByVal obj As ComboBox)
        Dim i As Integer
        obj.Items.Clear()
        If obj Is MainWin.VisualizationComp Then
            obj.Items.Add("All")
        Else
            obj.Items.Add("Outside")
        End If
        If myCompartments.Count > 0 Then
            For i = 0 To myCompartments.Count - 1
                obj.Items.Add(myCompartments.Item(i).Name)
            Next
        End If
    End Sub
    Public Sub InitFireObjectList(ByVal obj As ComboBox)
        Dim i As Integer
        obj.Items.Clear()
        If myFireObjects.Count > 0 Then
            For i = 0 To myFireObjects.Count - 1
                obj.Items.Add(myFireObjects.Item(i).Name)
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
        MainWin.TargetNormalCalc.Items.Clear()
        MainWin.TargetNormalCalc.Items.Add("User Specified")
        numFires = myFires.Count
        If numFires > 0 Then
            Dim aFire As Fire, aFireObject As Fire
            For i = 1 To numFires
                aFire = myFires(i - 1)
                If aTarget.Compartment = aFire.Compartment And (aFire.XPosition - aTarget.XPosition <> 0 Or aFire.YPosition - aTarget.YPosition <> 0 Or aFire.ZPosition - aTarget.ZPosition <> 0) Then
                    aFireObject = myFireObjects(aFire.FireObject)
                    MainWin.TargetNormalCalc.Items.Add("Fire " + i.ToString + ", " + aFireObject.Name)
                End If
            Next
        End If
        If aTarget.XPosition <> 0 Then MainWin.TargetNormalCalc.Items.Add("Left Wall")
        If aTarget.Compartment >= 0 Then
            If aTarget.XPosition <> myCompartments(aTarget.Compartment).RoomWidth Then MainWin.TargetNormalCalc.Items.Add("Right Wall")
        End If
        If aTarget.YPosition <> 0 Then MainWin.TargetNormalCalc.Items.Add("Front Wall")
        If aTarget.Compartment >= 0 Then
            If aTarget.YPosition <> myCompartments(aTarget.Compartment).RoomDepth Then MainWin.TargetNormalCalc.Items.Add("Rear Wall")
        End If
        If aTarget.ZPosition <> 0 Then MainWin.TargetNormalCalc.Items.Add("Floor")
        If aTarget.Compartment >= 0 Then
            If aTarget.ZPosition <> myCompartments(aTarget.Compartment).RoomHeight Then MainWin.TargetNormalCalc.Items.Add("Ceiling")
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
            Me.Menu()
            Me.Environment()
        End If
    End Sub
End Class
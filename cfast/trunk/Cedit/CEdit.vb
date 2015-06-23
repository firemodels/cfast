Public Class CeditMain

    Inherits System.Windows.Forms.Form
    Friend UpdateGUI As New UpdateGUI(Me)
    Private UserUnits As New User_Units
    Private About As New About
    Private ViewFile As New ViewFile
    'Private RunSimulation As New RunModel
    Private CurrentThermalProperty As Integer = 0, CurrentCompartment As Integer = 0, CurrentHVent As Integer = 0, CurrentVVent As Integer = 0, _
    CurrentMVent As Integer = 0, CurrentTarget As Integer = 0, CurrentDetector As Integer = 0, CurrentHHeat As Integer = 0, _
    CurrentVHeat As Integer = 0, CurrentFire As Integer = 0, CurrentVisual As Integer = 0
    Friend WithEvents MenuItem5 As System.Windows.Forms.MenuItem
#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Private Const OK As Integer = 1, Cancel As Integer = 2
    Friend WithEvents TabMaterials As System.Windows.Forms.TabPage
    Friend WithEvents ThermalFromFile As System.Windows.Forms.Button
    Friend WithEvents ThermalRemove As System.Windows.Forms.Button
    Friend WithEvents ThermalAdd As System.Windows.Forms.Button
    Friend WithEvents ThermalDup As System.Windows.Forms.Button
    Friend WithEvents ThermalSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents GroupThermal As System.Windows.Forms.GroupBox
    Friend WithEvents ThermalThickness As System.Windows.Forms.TextBox
    Friend WithEvents Label53 As System.Windows.Forms.Label
    Friend WithEvents ThermalConductivity As System.Windows.Forms.TextBox
    Friend WithEvents Label62 As System.Windows.Forms.Label
    Friend WithEvents ThermalSpecificHeat As System.Windows.Forms.TextBox
    Friend WithEvents Label84 As System.Windows.Forms.Label
    Friend WithEvents ThermalLongName As System.Windows.Forms.TextBox
    Friend WithEvents Label94 As System.Windows.Forms.Label
    Friend WithEvents ThermalDensity As System.Windows.Forms.TextBox
    Friend WithEvents Label102 As System.Windows.Forms.Label
    Friend WithEvents Label103 As System.Windows.Forms.Label
    Friend WithEvents ThermalEmissivity As System.Windows.Forms.TextBox
    Friend WithEvents Label104 As System.Windows.Forms.Label
    Friend WithEvents FireFromFile As System.Windows.Forms.Button
    Friend WithEvents FireDataSS As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents FireCOYield As System.Windows.Forms.TextBox
    Friend WithEvents Label71 As System.Windows.Forms.Label
    Friend WithEvents FireSootYield As System.Windows.Forms.TextBox
    Friend WithEvents Label105 As System.Windows.Forms.Label
    Friend WithEvents FireHoC As System.Windows.Forms.TextBox
    Friend WithEvents Label108 As System.Windows.Forms.Label
    Friend WithEvents FireMaterial As System.Windows.Forms.ComboBox
    Friend WithEvents Label113 As System.Windows.Forms.Label
    Friend WithEvents Label107 As System.Windows.Forms.Label
    Friend WithEvents FireRadiativeFraction As System.Windows.Forms.TextBox
    Friend WithEvents FireCl As System.Windows.Forms.TextBox
    Friend WithEvents Label106 As System.Windows.Forms.Label
    Friend WithEvents FireN As System.Windows.Forms.TextBox
    Friend WithEvents FireH As System.Windows.Forms.TextBox
    Friend WithEvents Label109 As System.Windows.Forms.Label
    Friend WithEvents FireC As System.Windows.Forms.TextBox
    Friend WithEvents FireO As System.Windows.Forms.TextBox
    Friend WithEvents Label110 As System.Windows.Forms.Label
    Friend WithEvents Label111 As System.Windows.Forms.Label
    Friend WithEvents Label112 As System.Windows.Forms.Label
    Friend WithEvents FireName As System.Windows.Forms.TextBox
    Friend WithEvents Label114 As System.Windows.Forms.Label
    Friend WithEvents FirePlot As NPlot.Windows.PlotSurface2D
    Friend WithEvents EnvAdiabatic As System.Windows.Forms.CheckBox
    Friend WithEvents ThermalShortName As System.Windows.Forms.TextBox
    Friend WithEvents GroupVisualResolution As System.Windows.Forms.GroupBox
    Friend WithEvents VisualResolution As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label31 As System.Windows.Forms.Label
    Friend WithEvents VisualizationX As System.Windows.Forms.TextBox
    Friend WithEvents Label35 As System.Windows.Forms.Label
    Friend WithEvents VisualizationZ As System.Windows.Forms.TextBox
    Friend WithEvents Label32 As System.Windows.Forms.Label
    Friend WithEvents VisualizationY As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents Label30 As System.Windows.Forms.Label
    Friend WithEvents EnvIntAmbRH As System.Windows.Forms.TextBox
    Friend WithEvents CompCorridor As System.Windows.Forms.RadioButton
    Friend WithEvents CompShaft As System.Windows.Forms.RadioButton
    Friend WithEvents CompNormal As System.Windows.Forms.RadioButton
    Friend WithEvents MVentFilterTime As System.Windows.Forms.TextBox
    Friend WithEvents Label38 As System.Windows.Forms.Label
    Friend WithEvents MVentFilterEfficiency As System.Windows.Forms.TextBox
    Friend WithEvents MainGeometry As System.Windows.Forms.Button
    Friend WithEvents Label56 As System.Windows.Forms.Label
    Friend WithEvents Label57 As System.Windows.Forms.Label
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents TargetConduct As System.Windows.Forms.Label
    Friend WithEvents TargetSpecHeat As System.Windows.Forms.Label
    Friend WithEvents TargetDensity As System.Windows.Forms.Label
    Friend WithEvents TargetThickness As System.Windows.Forms.Label
    Friend WithEvents TargetInternalLocation As System.Windows.Forms.TextBox
    Friend WithEvents Label59 As System.Windows.Forms.Label
    Friend WithEvents Label65 As System.Windows.Forms.Label
    Friend WithEvents Label64 As System.Windows.Forms.Label
    Friend WithEvents CompSpecHeatFloor As System.Windows.Forms.Label
    Friend WithEvents CompDensityFloor As System.Windows.Forms.Label
    Friend WithEvents CompThicknessFloor As System.Windows.Forms.Label
    Friend WithEvents CompConductFloor As System.Windows.Forms.Label
    Friend WithEvents CompSpecHeatWalls As System.Windows.Forms.Label
    Friend WithEvents CompDensityWalls As System.Windows.Forms.Label
    Friend WithEvents CompThicknessWalls As System.Windows.Forms.Label
    Friend WithEvents CompConductWalls As System.Windows.Forms.Label
    Friend WithEvents CompSpecHeatCeiling As System.Windows.Forms.Label
    Friend WithEvents CompDensityCeiling As System.Windows.Forms.Label
    Friend WithEvents CompThicknessCeiling As System.Windows.Forms.Label
    Friend WithEvents CompConductCeiling As System.Windows.Forms.Label
    Friend WithEvents MenuItem2 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuDetailedOutput As System.Windows.Forms.MenuItem
    Friend WithEvents MenuTotalMassOutput As System.Windows.Forms.MenuItem
    Friend WithEvents MenuNetHeatFluxOutput As System.Windows.Forms.MenuItem
    Friend WithEvents MenuShowCFAST As System.Windows.Forms.MenuItem
    Friend WithEvents MainOpen As System.Windows.Forms.Button
    Friend WithEvents MenuValidationOutput As System.Windows.Forms.MenuItem
    Friend WithEvents FireAddt2 As System.Windows.Forms.Button
    Friend WithEvents FireAdd As System.Windows.Forms.Button
    Friend WithEvents FireType As System.Windows.Forms.Label
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight
    Friend WithEvents MenuItem4 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuDebugOutput As System.Windows.Forms.MenuItem
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents EnvTimeStep As System.Windows.Forms.TextBox
    Friend WithEvents TabVisuals As System.Windows.Forms.TabPage
    Friend WithEvents VisualizationValueLabel As System.Windows.Forms.Label
    Friend WithEvents Label29 As System.Windows.Forms.Label
    Friend WithEvents VisualSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents VisualizationRemove As System.Windows.Forms.Button
    Friend WithEvents VisualizationValue As System.Windows.Forms.TextBox
    Friend WithEvents VisualizationDefaults As System.Windows.Forms.Button
    Friend WithEvents VisualizationComp As System.Windows.Forms.ComboBox
    Friend WithEvents VisualizationDup As System.Windows.Forms.Button
    Friend WithEvents VisualizationType As System.Windows.Forms.ComboBox
    Friend WithEvents VisualizationAdd As System.Windows.Forms.Button
    Friend WithEvents VisualizationAxisLabel As System.Windows.Forms.Label
    Friend WithEvents VisualizationAxis As System.Windows.Forms.ComboBox
    Friend WithEvents Label54 As System.Windows.Forms.Label
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents VHeatSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents GroupVHeats As System.Windows.Forms.GroupBox
    Friend WithEvents VHeatComp2 As System.Windows.Forms.ComboBox
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents VHeatComp1 As System.Windows.Forms.ComboBox
    Friend WithEvents Label39 As System.Windows.Forms.Label
    Friend WithEvents VHeatDup As System.Windows.Forms.Button
    Friend WithEvents VHeatRemove As System.Windows.Forms.Button
    Friend WithEvents VHeatAdd As System.Windows.Forms.Button
    Friend WithEvents TabEnvironment As System.Windows.Forms.TabPage
    Friend WithEvents TabHorizontalFlow As System.Windows.Forms.TabPage
    Friend WithEvents TabVerticalFlow As System.Windows.Forms.TabPage
    Friend WithEvents Label40 As System.Windows.Forms.Label
    Friend WithEvents GroupBox17 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox18 As System.Windows.Forms.GroupBox
    Friend WithEvents Label46 As System.Windows.Forms.Label
    Friend WithEvents TabGeometry As System.Windows.Forms.TabPage
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents TabMechanicalFlow As System.Windows.Forms.TabPage
    Friend WithEvents Label41 As System.Windows.Forms.Label
    Friend WithEvents GroupBox20 As System.Windows.Forms.GroupBox
    Friend WithEvents Label43 As System.Windows.Forms.Label
    Friend WithEvents Label48 As System.Windows.Forms.Label
    Friend WithEvents Label49 As System.Windows.Forms.Label
    Friend WithEvents Label42 As System.Windows.Forms.Label
    Friend WithEvents GroupBox21 As System.Windows.Forms.GroupBox
    Friend WithEvents Label44 As System.Windows.Forms.Label
    Friend WithEvents Label50 As System.Windows.Forms.Label
    Friend WithEvents Label51 As System.Windows.Forms.Label
    Friend WithEvents Label45 As System.Windows.Forms.Label
    Friend WithEvents Label47 As System.Windows.Forms.Label
    Friend WithEvents TabFires As System.Windows.Forms.TabPage
    Friend WithEvents TabDetection As System.Windows.Forms.TabPage
    Friend WithEvents TabTargets As System.Windows.Forms.TabPage
    Friend WithEvents TabHeatTransfer As System.Windows.Forms.TabPage
    Friend WithEvents TabMain As System.Windows.Forms.TabControl
    Friend WithEvents Label60 As System.Windows.Forms.Label
    Friend WithEvents Label69 As System.Windows.Forms.Label
    Friend WithEvents Label70 As System.Windows.Forms.Label
    Friend WithEvents Label52 As System.Windows.Forms.Label
    Friend WithEvents Label55 As System.Windows.Forms.Label
    Friend WithEvents Label61 As System.Windows.Forms.Label
    Friend WithEvents Label72 As System.Windows.Forms.Label
    Friend WithEvents Label73 As System.Windows.Forms.Label
    Friend WithEvents Label79 As System.Windows.Forms.Label
    Friend WithEvents Label80 As System.Windows.Forms.Label
    Friend WithEvents Label78 As System.Windows.Forms.Label
    Friend WithEvents Label74 As System.Windows.Forms.Label
    Friend WithEvents GroupBox28 As System.Windows.Forms.GroupBox
    Friend WithEvents Label75 As System.Windows.Forms.Label
    Friend WithEvents Label76 As System.Windows.Forms.Label
    Friend WithEvents Label77 As System.Windows.Forms.Label
    Friend WithEvents Label28 As System.Windows.Forms.Label
    Friend WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox12 As System.Windows.Forms.GroupBox
    Friend WithEvents Label24 As System.Windows.Forms.Label
    Friend WithEvents Label26 As System.Windows.Forms.Label
    Friend WithEvents Label27 As System.Windows.Forms.Label
    Friend WithEvents GroupBox11 As System.Windows.Forms.GroupBox
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label87 As System.Windows.Forms.Label
    Friend WithEvents GroupBox33 As System.Windows.Forms.GroupBox
    Friend WithEvents Label88 As System.Windows.Forms.Label
    Friend WithEvents Label89 As System.Windows.Forms.Label
    Friend WithEvents Label90 As System.Windows.Forms.Label
    Friend WithEvents Label91 As System.Windows.Forms.Label
    Friend WithEvents Label92 As System.Windows.Forms.Label
    Friend WithEvents Label81 As System.Windows.Forms.Label
    Friend WithEvents Label83 As System.Windows.Forms.Label
    Friend WithEvents EnvTitle As System.Windows.Forms.TextBox
    Friend WithEvents EnvExtAmbPress As System.Windows.Forms.TextBox
    Friend WithEvents EnvIntAmbPress As System.Windows.Forms.TextBox
    Friend WithEvents EnvIntAmbTemp As System.Windows.Forms.TextBox
    Friend WithEvents EnvSpreadOutInterval As System.Windows.Forms.TextBox
    Friend WithEvents EnvTextOutInterval As System.Windows.Forms.TextBox
    Friend WithEvents EnvSimTime As System.Windows.Forms.TextBox
    Friend WithEvents EnvErrors As System.Windows.Forms.TextBox
    Friend WithEvents HVentRemove As System.Windows.Forms.Button
    Friend WithEvents HVentAdd As System.Windows.Forms.Button
    Friend WithEvents HVentMoveUp As System.Windows.Forms.Button
    Friend WithEvents HVentDup As System.Windows.Forms.Button
    Friend WithEvents VVentRemove As System.Windows.Forms.Button
    Friend WithEvents VVentAdd As System.Windows.Forms.Button
    Friend WithEvents VVentDup As System.Windows.Forms.Button
    Friend WithEvents VVentShape As System.Windows.Forms.ComboBox
    Friend WithEvents VVentCompBottom As System.Windows.Forms.ComboBox
    Friend WithEvents VVentCompTop As System.Windows.Forms.ComboBox
    Friend WithEvents VVentArea As System.Windows.Forms.TextBox
    Friend WithEvents CompRemove As System.Windows.Forms.Button
    Friend WithEvents CompAdd As System.Windows.Forms.Button
    Friend WithEvents CompMoveDown As System.Windows.Forms.Button
    Friend WithEvents CompMoveUp As System.Windows.Forms.Button
    Friend WithEvents CompDup As System.Windows.Forms.Button
    Friend WithEvents CompName As System.Windows.Forms.TextBox
    Friend WithEvents CompZPosition As System.Windows.Forms.TextBox
    Friend WithEvents CompYPosition As System.Windows.Forms.TextBox
    Friend WithEvents CompXPosition As System.Windows.Forms.TextBox
    Friend WithEvents CompHeight As System.Windows.Forms.TextBox
    Friend WithEvents CompDepth As System.Windows.Forms.TextBox
    Friend WithEvents GroupCompSurfaces As System.Windows.Forms.GroupBox
    Friend WithEvents MVentRemove As System.Windows.Forms.Button
    Friend WithEvents MVentAdd As System.Windows.Forms.Button
    Friend WithEvents MVentDup As System.Windows.Forms.Button
    Friend WithEvents MVentZero As System.Windows.Forms.TextBox
    Friend WithEvents MVentToOrientation As System.Windows.Forms.ComboBox
    Friend WithEvents MVentToHeight As System.Windows.Forms.TextBox
    Friend WithEvents MVentToArea As System.Windows.Forms.TextBox
    Friend WithEvents MventToComp As System.Windows.Forms.ComboBox
    Friend WithEvents MVentDropoff As System.Windows.Forms.TextBox
    Friend WithEvents MVentFromOrientation As System.Windows.Forms.ComboBox
    Friend WithEvents MVentFromHeight As System.Windows.Forms.TextBox
    Friend WithEvents MVentFromArea As System.Windows.Forms.TextBox
    Friend WithEvents MVentFromComp As System.Windows.Forms.ComboBox
    Friend WithEvents MVentFlow As System.Windows.Forms.TextBox
    Friend WithEvents FireComp As System.Windows.Forms.ComboBox
    Friend WithEvents FireZPosition As System.Windows.Forms.TextBox
    Friend WithEvents FireYPosition As System.Windows.Forms.TextBox
    Friend WithEvents FireXPosition As System.Windows.Forms.TextBox
    Friend WithEvents FireRemove As System.Windows.Forms.Button
    Friend WithEvents FireDup As System.Windows.Forms.Button
    Friend WithEvents FireLOL As System.Windows.Forms.TextBox
    Friend WithEvents DetectorSprayDensity As System.Windows.Forms.TextBox
    Friend WithEvents DetectorRTI As System.Windows.Forms.TextBox
    Friend WithEvents DetectorActivation As System.Windows.Forms.TextBox
    Friend WithEvents DetectorType As System.Windows.Forms.ComboBox
    Friend WithEvents DetectorComp As System.Windows.Forms.ComboBox
    Friend WithEvents DetectorZPosition As System.Windows.Forms.TextBox
    Friend WithEvents DetectorYPosition As System.Windows.Forms.TextBox
    Friend WithEvents DetectorXPosition As System.Windows.Forms.TextBox
    Friend WithEvents DetectorRemove As System.Windows.Forms.Button
    Friend WithEvents DetectorAdd As System.Windows.Forms.Button
    Friend WithEvents DetectorMoveDown As System.Windows.Forms.Button
    Friend WithEvents DetectorMoveUp As System.Windows.Forms.Button
    Friend WithEvents DetectorDup As System.Windows.Forms.Button
    Friend WithEvents TargetRemove As System.Windows.Forms.Button
    Friend WithEvents TargetAdd As System.Windows.Forms.Button
    Friend WithEvents TargetMoveDown As System.Windows.Forms.Button
    Friend WithEvents TargetMoveUp As System.Windows.Forms.Button
    Friend WithEvents TargetDup As System.Windows.Forms.Button
    Friend WithEvents StatusBar As System.Windows.Forms.StatusBar
    Friend WithEvents HVentMoveDown As System.Windows.Forms.Button
    Friend WithEvents TargetMaterial As System.Windows.Forms.ComboBox
    Friend WithEvents TargetZNormal As System.Windows.Forms.TextBox
    Friend WithEvents TargetYNormal As System.Windows.Forms.TextBox
    Friend WithEvents TargetXNormal As System.Windows.Forms.TextBox
    Friend WithEvents TargetComp As System.Windows.Forms.ComboBox
    Friend WithEvents TargetZPosition As System.Windows.Forms.TextBox
    Friend WithEvents TargetYPosition As System.Windows.Forms.TextBox
    Friend WithEvents TargetXPosition As System.Windows.Forms.TextBox
    Friend WithEvents EnvExtAmbElevation As System.Windows.Forms.TextBox
    Friend WithEvents EnvExtAmbTemp As System.Windows.Forms.TextBox
    Friend WithEvents FireIgnitionCriteria As System.Windows.Forms.ComboBox
    Friend WithEvents FireZNormal As System.Windows.Forms.TextBox
    Friend WithEvents FireYNormal As System.Windows.Forms.TextBox
    Friend WithEvents FireXNormal As System.Windows.Forms.TextBox
    Friend WithEvents Label68 As System.Windows.Forms.Label
    Friend WithEvents FireIgnitionValue As System.Windows.Forms.TextBox
    Friend WithEvents Label63 As System.Windows.Forms.Label
    Friend WithEvents Label58 As System.Windows.Forms.Label
    Friend WithEvents Label95 As System.Windows.Forms.Label
    Friend WithEvents GroupVVents As System.Windows.Forms.GroupBox
    Friend WithEvents GroupCompartments As System.Windows.Forms.GroupBox
    Friend WithEvents GroupMVents As System.Windows.Forms.GroupBox
    Friend WithEvents GroupDetectors As System.Windows.Forms.GroupBox
    Friend WithEvents GroupTargets As System.Windows.Forms.GroupBox
    Friend WithEvents HVentFace As System.Windows.Forms.ComboBox
    Friend WithEvents Label37 As System.Windows.Forms.Label
    Friend WithEvents HVentInitialFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label36 As System.Windows.Forms.Label
    Friend WithEvents GroupBox14 As System.Windows.Forms.GroupBox
    Friend WithEvents HVentComp2 As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox13 As System.Windows.Forms.GroupBox
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents HVentOffset1 As System.Windows.Forms.TextBox
    Friend WithEvents HVentComp1 As System.Windows.Forms.ComboBox
    Friend WithEvents HVentSoffit As System.Windows.Forms.TextBox
    Friend WithEvents Label34 As System.Windows.Forms.Label
    Friend WithEvents HVentSill As System.Windows.Forms.TextBox
    Friend WithEvents Label33 As System.Windows.Forms.Label
    Friend WithEvents HVentWidth As System.Windows.Forms.TextBox
    Friend WithEvents Label23 As System.Windows.Forms.Label
    Friend WithEvents GroupFlowCharacteristics As System.Windows.Forms.GroupBox
    Friend WithEvents CompSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents CompVariableArea As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents CompFloor As System.Windows.Forms.ComboBox
    Friend WithEvents CompWalls As System.Windows.Forms.ComboBox
    Friend WithEvents CompCeiling As System.Windows.Forms.ComboBox
    Friend WithEvents Label21 As System.Windows.Forms.Label
    Friend WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents HVentSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents GroupHVentGeometry As System.Windows.Forms.GroupBox
    Friend WithEvents VVentSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents MVentSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents TargetSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents TargetSolutionThickness As System.Windows.Forms.ComboBox
    Friend WithEvents TargetSolutionMethod As System.Windows.Forms.ComboBox
    Friend WithEvents DetectorSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents HHeatSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents HHeatRemove As System.Windows.Forms.Button
    Friend WithEvents HHeatAdd As System.Windows.Forms.Button
    Friend WithEvents HHeatDup As System.Windows.Forms.Button
    Friend WithEvents HHeatFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label93 As System.Windows.Forms.Label
    Friend WithEvents Label85 As System.Windows.Forms.Label
    Friend WithEvents HHeatComp2 As System.Windows.Forms.ComboBox
    Friend WithEvents Label86 As System.Windows.Forms.Label
    Friend WithEvents HHeatComp1 As System.Windows.Forms.ComboBox
    Friend WithEvents GroupHHeats As System.Windows.Forms.GroupBox
    Friend WithEvents GroupFire As System.Windows.Forms.GroupBox
    Friend WithEvents FireSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents Label66 As System.Windows.Forms.Label
    Friend WithEvents MenuUnits As System.Windows.Forms.MenuItem
    Friend WithEvents MenuSave As System.Windows.Forms.MenuItem
    Friend WithEvents MenuSaveAs As System.Windows.Forms.MenuItem
    Friend WithEvents MenuOpen As System.Windows.Forms.MenuItem
    Friend WithEvents EnvIntAmbElevation As System.Windows.Forms.TextBox
    Friend WithEvents CompWidth As System.Windows.Forms.TextBox
    Friend WithEvents Label25 As System.Windows.Forms.Label
    Friend WithEvents EnvSmokeviewInterval As System.Windows.Forms.TextBox
    Friend WithEvents MenuNew As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRunCFast As System.Windows.Forms.MenuItem
    Friend WithEvents SaveDataFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents OpenDataFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents MenuExit As System.Windows.Forms.MenuItem
    Friend WithEvents Label67 As System.Windows.Forms.Label
    Friend WithEvents Label82 As System.Windows.Forms.Label
    Friend WithEvents HVentFractionTime As System.Windows.Forms.TextBox
    Friend WithEvents Errors As System.Windows.Forms.StatusBarPanel
    Friend WithEvents Message As System.Windows.Forms.StatusBarPanel
    Friend WithEvents HVentFinalFraction As System.Windows.Forms.TextBox
    Friend WithEvents MenuSMVGeometry As System.Windows.Forms.MenuItem
    Friend WithEvents MenuAbout As System.Windows.Forms.MenuItem
    Friend WithEvents MainMenu As System.Windows.Forms.MainMenu
    Friend WithEvents MenuRecent1 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRecent2 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRecent3 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRecent4 As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRecentSeparator As System.Windows.Forms.MenuItem
    Friend WithEvents HelpProvider As System.Windows.Forms.HelpProvider
    Friend WithEvents MenuCFASTWeb As System.Windows.Forms.MenuItem
    Friend WithEvents MenuViewOutput As System.Windows.Forms.MenuItem
    Friend WithEvents MenuViewInput As System.Windows.Forms.MenuItem
    Friend WithEvents MenuViewLog As System.Windows.Forms.MenuItem
    Friend WithEvents MenuFile As System.Windows.Forms.MenuItem
    Friend WithEvents MenuRun As System.Windows.Forms.MenuItem
    Friend WithEvents MenuView As System.Windows.Forms.MenuItem
    Friend WithEvents MenuShowHelp As System.Windows.Forms.MenuItem
    Friend WithEvents MenuHelp As System.Windows.Forms.MenuItem
    Friend WithEvents VVentFractionTime As System.Windows.Forms.TextBox
    Friend WithEvents Label96 As System.Windows.Forms.Label
    Friend WithEvents VVentFinalFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label97 As System.Windows.Forms.Label
    Friend WithEvents VVentInitialFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label98 As System.Windows.Forms.Label
    Friend WithEvents MVentFractionTime As System.Windows.Forms.TextBox
    Friend WithEvents Label99 As System.Windows.Forms.Label
    Friend WithEvents MVentFinalFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label100 As System.Windows.Forms.Label
    Friend WithEvents MVentInitialFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label101 As System.Windows.Forms.Label
    Friend WithEvents MenuSMVSimulation As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents ErrorProvider1 As System.Windows.Forms.ErrorProvider
    Friend WithEvents TargetNormalCalc As System.Windows.Forms.ComboBox
    Friend WithEvents FireIgnitionTemperature As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents MainView As System.Windows.Forms.Button
    Friend WithEvents MainSave As System.Windows.Forms.Button
    Friend WithEvents MainRun As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(CeditMain))
        Me.StatusBar = New System.Windows.Forms.StatusBar()
        Me.Errors = New System.Windows.Forms.StatusBarPanel()
        Me.Message = New System.Windows.Forms.StatusBarPanel()
        Me.MainMenu = New System.Windows.Forms.MainMenu(Me.components)
        Me.MenuFile = New System.Windows.Forms.MenuItem()
        Me.MenuNew = New System.Windows.Forms.MenuItem()
        Me.MenuOpen = New System.Windows.Forms.MenuItem()
        Me.MenuSave = New System.Windows.Forms.MenuItem()
        Me.MenuSaveAs = New System.Windows.Forms.MenuItem()
        Me.MenuExit = New System.Windows.Forms.MenuItem()
        Me.MenuRecentSeparator = New System.Windows.Forms.MenuItem()
        Me.MenuRecent1 = New System.Windows.Forms.MenuItem()
        Me.MenuRecent2 = New System.Windows.Forms.MenuItem()
        Me.MenuRecent3 = New System.Windows.Forms.MenuItem()
        Me.MenuRecent4 = New System.Windows.Forms.MenuItem()
        Me.MenuRun = New System.Windows.Forms.MenuItem()
        Me.MenuSMVGeometry = New System.Windows.Forms.MenuItem()
        Me.MenuRunCFast = New System.Windows.Forms.MenuItem()
        Me.MenuSMVSimulation = New System.Windows.Forms.MenuItem()
        Me.MenuItem1 = New System.Windows.Forms.MenuItem()
        Me.MenuItem2 = New System.Windows.Forms.MenuItem()
        Me.MenuDetailedOutput = New System.Windows.Forms.MenuItem()
        Me.MenuTotalMassOutput = New System.Windows.Forms.MenuItem()
        Me.MenuNetHeatFluxOutput = New System.Windows.Forms.MenuItem()
        Me.MenuValidationOutput = New System.Windows.Forms.MenuItem()
        Me.MenuItem4 = New System.Windows.Forms.MenuItem()
        Me.MenuDebugOutput = New System.Windows.Forms.MenuItem()
        Me.MenuShowCFAST = New System.Windows.Forms.MenuItem()
        Me.MenuView = New System.Windows.Forms.MenuItem()
        Me.MenuUnits = New System.Windows.Forms.MenuItem()
        Me.MenuItem5 = New System.Windows.Forms.MenuItem()
        Me.MenuViewInput = New System.Windows.Forms.MenuItem()
        Me.MenuViewOutput = New System.Windows.Forms.MenuItem()
        Me.MenuViewLog = New System.Windows.Forms.MenuItem()
        Me.MenuHelp = New System.Windows.Forms.MenuItem()
        Me.MenuShowHelp = New System.Windows.Forms.MenuItem()
        Me.MenuCFASTWeb = New System.Windows.Forms.MenuItem()
        Me.MenuAbout = New System.Windows.Forms.MenuItem()
        Me.TabEnvironment = New System.Windows.Forms.TabPage()
        Me.EnvErrors = New System.Windows.Forms.TextBox()
        Me.EnvTitle = New System.Windows.Forms.TextBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.EnvAdiabatic = New System.Windows.Forms.CheckBox()
        Me.GroupBox12 = New System.Windows.Forms.GroupBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.EnvExtAmbElevation = New System.Windows.Forms.TextBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.EnvExtAmbPress = New System.Windows.Forms.TextBox()
        Me.Label27 = New System.Windows.Forms.Label()
        Me.EnvExtAmbTemp = New System.Windows.Forms.TextBox()
        Me.GroupBox11 = New System.Windows.Forms.GroupBox()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.EnvIntAmbRH = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.EnvIntAmbElevation = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.EnvIntAmbPress = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.EnvIntAmbTemp = New System.Windows.Forms.TextBox()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.EnvTimeStep = New System.Windows.Forms.TextBox()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.EnvSmokeviewInterval = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.EnvSpreadOutInterval = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.EnvTextOutInterval = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.EnvSimTime = New System.Windows.Forms.TextBox()
        Me.TabHorizontalFlow = New System.Windows.Forms.TabPage()
        Me.HVentSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupHVentGeometry = New System.Windows.Forms.GroupBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.HVentFractionTime = New System.Windows.Forms.TextBox()
        Me.HVentOffset1 = New System.Windows.Forms.TextBox()
        Me.Label82 = New System.Windows.Forms.Label()
        Me.HVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.Label67 = New System.Windows.Forms.Label()
        Me.HVentFace = New System.Windows.Forms.ComboBox()
        Me.Label37 = New System.Windows.Forms.Label()
        Me.HVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.Label36 = New System.Windows.Forms.Label()
        Me.GroupBox14 = New System.Windows.Forms.GroupBox()
        Me.HVentComp2 = New System.Windows.Forms.ComboBox()
        Me.GroupBox13 = New System.Windows.Forms.GroupBox()
        Me.HVentComp1 = New System.Windows.Forms.ComboBox()
        Me.HVentSoffit = New System.Windows.Forms.TextBox()
        Me.Label34 = New System.Windows.Forms.Label()
        Me.HVentSill = New System.Windows.Forms.TextBox()
        Me.Label33 = New System.Windows.Forms.Label()
        Me.HVentWidth = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.HVentRemove = New System.Windows.Forms.Button()
        Me.HVentAdd = New System.Windows.Forms.Button()
        Me.HVentMoveDown = New System.Windows.Forms.Button()
        Me.HVentMoveUp = New System.Windows.Forms.Button()
        Me.HVentDup = New System.Windows.Forms.Button()
        Me.TabVerticalFlow = New System.Windows.Forms.TabPage()
        Me.VVentSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.VVentRemove = New System.Windows.Forms.Button()
        Me.VVentAdd = New System.Windows.Forms.Button()
        Me.VVentDup = New System.Windows.Forms.Button()
        Me.GroupVVents = New System.Windows.Forms.GroupBox()
        Me.VVentFractionTime = New System.Windows.Forms.TextBox()
        Me.Label96 = New System.Windows.Forms.Label()
        Me.VVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.Label97 = New System.Windows.Forms.Label()
        Me.VVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.Label98 = New System.Windows.Forms.Label()
        Me.VVentShape = New System.Windows.Forms.ComboBox()
        Me.Label40 = New System.Windows.Forms.Label()
        Me.GroupBox17 = New System.Windows.Forms.GroupBox()
        Me.VVentCompBottom = New System.Windows.Forms.ComboBox()
        Me.GroupBox18 = New System.Windows.Forms.GroupBox()
        Me.VVentCompTop = New System.Windows.Forms.ComboBox()
        Me.VVentArea = New System.Windows.Forms.TextBox()
        Me.Label46 = New System.Windows.Forms.Label()
        Me.TabGeometry = New System.Windows.Forms.TabPage()
        Me.CompSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.CompRemove = New System.Windows.Forms.Button()
        Me.CompAdd = New System.Windows.Forms.Button()
        Me.CompMoveDown = New System.Windows.Forms.Button()
        Me.CompMoveUp = New System.Windows.Forms.Button()
        Me.CompDup = New System.Windows.Forms.Button()
        Me.GroupCompartments = New System.Windows.Forms.GroupBox()
        Me.CompName = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.CompWidth = New System.Windows.Forms.TextBox()
        Me.CompZPosition = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.CompYPosition = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.CompXPosition = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.CompHeight = New System.Windows.Forms.TextBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.CompDepth = New System.Windows.Forms.TextBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.GroupFlowCharacteristics = New System.Windows.Forms.GroupBox()
        Me.CompNormal = New System.Windows.Forms.RadioButton()
        Me.CompCorridor = New System.Windows.Forms.RadioButton()
        Me.CompShaft = New System.Windows.Forms.RadioButton()
        Me.Label65 = New System.Windows.Forms.Label()
        Me.Label64 = New System.Windows.Forms.Label()
        Me.CompVariableArea = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupCompSurfaces = New System.Windows.Forms.GroupBox()
        Me.CompSpecHeatFloor = New System.Windows.Forms.Label()
        Me.CompDensityFloor = New System.Windows.Forms.Label()
        Me.CompThicknessFloor = New System.Windows.Forms.Label()
        Me.CompConductFloor = New System.Windows.Forms.Label()
        Me.CompSpecHeatWalls = New System.Windows.Forms.Label()
        Me.CompDensityWalls = New System.Windows.Forms.Label()
        Me.CompThicknessWalls = New System.Windows.Forms.Label()
        Me.CompConductWalls = New System.Windows.Forms.Label()
        Me.CompSpecHeatCeiling = New System.Windows.Forms.Label()
        Me.CompDensityCeiling = New System.Windows.Forms.Label()
        Me.CompThicknessCeiling = New System.Windows.Forms.Label()
        Me.CompConductCeiling = New System.Windows.Forms.Label()
        Me.CompFloor = New System.Windows.Forms.ComboBox()
        Me.CompWalls = New System.Windows.Forms.ComboBox()
        Me.CompCeiling = New System.Windows.Forms.ComboBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.MainView = New System.Windows.Forms.Button()
        Me.TabMechanicalFlow = New System.Windows.Forms.TabPage()
        Me.MVentSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.MVentRemove = New System.Windows.Forms.Button()
        Me.MVentAdd = New System.Windows.Forms.Button()
        Me.MVentDup = New System.Windows.Forms.Button()
        Me.GroupMVents = New System.Windows.Forms.GroupBox()
        Me.MVentFilterTime = New System.Windows.Forms.TextBox()
        Me.Label38 = New System.Windows.Forms.Label()
        Me.MVentFilterEfficiency = New System.Windows.Forms.TextBox()
        Me.Label54 = New System.Windows.Forms.Label()
        Me.MVentFractionTime = New System.Windows.Forms.TextBox()
        Me.Label99 = New System.Windows.Forms.Label()
        Me.MVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.Label100 = New System.Windows.Forms.Label()
        Me.MVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.Label101 = New System.Windows.Forms.Label()
        Me.MVentZero = New System.Windows.Forms.TextBox()
        Me.Label41 = New System.Windows.Forms.Label()
        Me.GroupBox20 = New System.Windows.Forms.GroupBox()
        Me.MVentToOrientation = New System.Windows.Forms.ComboBox()
        Me.Label43 = New System.Windows.Forms.Label()
        Me.MVentToHeight = New System.Windows.Forms.TextBox()
        Me.Label48 = New System.Windows.Forms.Label()
        Me.MVentToArea = New System.Windows.Forms.TextBox()
        Me.Label49 = New System.Windows.Forms.Label()
        Me.MventToComp = New System.Windows.Forms.ComboBox()
        Me.MVentDropoff = New System.Windows.Forms.TextBox()
        Me.Label42 = New System.Windows.Forms.Label()
        Me.GroupBox21 = New System.Windows.Forms.GroupBox()
        Me.MVentFromOrientation = New System.Windows.Forms.ComboBox()
        Me.Label44 = New System.Windows.Forms.Label()
        Me.MVentFromHeight = New System.Windows.Forms.TextBox()
        Me.Label50 = New System.Windows.Forms.Label()
        Me.MVentFromArea = New System.Windows.Forms.TextBox()
        Me.Label51 = New System.Windows.Forms.Label()
        Me.MVentFromComp = New System.Windows.Forms.ComboBox()
        Me.MVentFlow = New System.Windows.Forms.TextBox()
        Me.Label45 = New System.Windows.Forms.Label()
        Me.Label47 = New System.Windows.Forms.Label()
        Me.TabFires = New System.Windows.Forms.TabPage()
        Me.FireAdd = New System.Windows.Forms.Button()
        Me.FireFromFile = New System.Windows.Forms.Button()
        Me.FireAddt2 = New System.Windows.Forms.Button()
        Me.FireIgnitionTemperature = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.FireSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.FireRemove = New System.Windows.Forms.Button()
        Me.FireDup = New System.Windows.Forms.Button()
        Me.GroupFire = New System.Windows.Forms.GroupBox()
        Me.FirePlot = New NPlot.Windows.PlotSurface2D()
        Me.FireCOYield = New System.Windows.Forms.TextBox()
        Me.FireHoC = New System.Windows.Forms.TextBox()
        Me.FireMaterial = New System.Windows.Forms.ComboBox()
        Me.Label107 = New System.Windows.Forms.Label()
        Me.Label71 = New System.Windows.Forms.Label()
        Me.Label108 = New System.Windows.Forms.Label()
        Me.FireCl = New System.Windows.Forms.TextBox()
        Me.FireName = New System.Windows.Forms.TextBox()
        Me.FireSootYield = New System.Windows.Forms.TextBox()
        Me.Label106 = New System.Windows.Forms.Label()
        Me.Label114 = New System.Windows.Forms.Label()
        Me.FireDataSS = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.Label105 = New System.Windows.Forms.Label()
        Me.FireType = New System.Windows.Forms.Label()
        Me.Label66 = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.FireN = New System.Windows.Forms.TextBox()
        Me.Label95 = New System.Windows.Forms.Label()
        Me.Label60 = New System.Windows.Forms.Label()
        Me.FireH = New System.Windows.Forms.TextBox()
        Me.Label113 = New System.Windows.Forms.Label()
        Me.Label109 = New System.Windows.Forms.Label()
        Me.FireYPosition = New System.Windows.Forms.TextBox()
        Me.Label69 = New System.Windows.Forms.Label()
        Me.FireC = New System.Windows.Forms.TextBox()
        Me.FireRadiativeFraction = New System.Windows.Forms.TextBox()
        Me.FireXPosition = New System.Windows.Forms.TextBox()
        Me.FireO = New System.Windows.Forms.TextBox()
        Me.Label70 = New System.Windows.Forms.Label()
        Me.Label110 = New System.Windows.Forms.Label()
        Me.FireZPosition = New System.Windows.Forms.TextBox()
        Me.FireComp = New System.Windows.Forms.ComboBox()
        Me.Label111 = New System.Windows.Forms.Label()
        Me.Label52 = New System.Windows.Forms.Label()
        Me.Label112 = New System.Windows.Forms.Label()
        Me.FireIgnitionCriteria = New System.Windows.Forms.ComboBox()
        Me.FireIgnitionValue = New System.Windows.Forms.TextBox()
        Me.Label63 = New System.Windows.Forms.Label()
        Me.Label58 = New System.Windows.Forms.Label()
        Me.FireXNormal = New System.Windows.Forms.TextBox()
        Me.FireYNormal = New System.Windows.Forms.TextBox()
        Me.FireZNormal = New System.Windows.Forms.TextBox()
        Me.Label68 = New System.Windows.Forms.Label()
        Me.FireLOL = New System.Windows.Forms.TextBox()
        Me.Label55 = New System.Windows.Forms.Label()
        Me.TabDetection = New System.Windows.Forms.TabPage()
        Me.DetectorSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupDetectors = New System.Windows.Forms.GroupBox()
        Me.DetectorSprayDensity = New System.Windows.Forms.TextBox()
        Me.Label81 = New System.Windows.Forms.Label()
        Me.DetectorRTI = New System.Windows.Forms.TextBox()
        Me.Label83 = New System.Windows.Forms.Label()
        Me.DetectorActivation = New System.Windows.Forms.TextBox()
        Me.Label92 = New System.Windows.Forms.Label()
        Me.DetectorType = New System.Windows.Forms.ComboBox()
        Me.Label91 = New System.Windows.Forms.Label()
        Me.DetectorComp = New System.Windows.Forms.ComboBox()
        Me.Label87 = New System.Windows.Forms.Label()
        Me.GroupBox33 = New System.Windows.Forms.GroupBox()
        Me.DetectorZPosition = New System.Windows.Forms.TextBox()
        Me.Label88 = New System.Windows.Forms.Label()
        Me.DetectorYPosition = New System.Windows.Forms.TextBox()
        Me.Label89 = New System.Windows.Forms.Label()
        Me.DetectorXPosition = New System.Windows.Forms.TextBox()
        Me.Label90 = New System.Windows.Forms.Label()
        Me.DetectorRemove = New System.Windows.Forms.Button()
        Me.DetectorAdd = New System.Windows.Forms.Button()
        Me.DetectorMoveDown = New System.Windows.Forms.Button()
        Me.DetectorMoveUp = New System.Windows.Forms.Button()
        Me.DetectorDup = New System.Windows.Forms.Button()
        Me.TabTargets = New System.Windows.Forms.TabPage()
        Me.TargetSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupTargets = New System.Windows.Forms.GroupBox()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.TargetInternalLocation = New System.Windows.Forms.TextBox()
        Me.Label59 = New System.Windows.Forms.Label()
        Me.TargetSpecHeat = New System.Windows.Forms.Label()
        Me.TargetDensity = New System.Windows.Forms.Label()
        Me.TargetThickness = New System.Windows.Forms.Label()
        Me.TargetConduct = New System.Windows.Forms.Label()
        Me.TargetMaterial = New System.Windows.Forms.ComboBox()
        Me.Label78 = New System.Windows.Forms.Label()
        Me.Label80 = New System.Windows.Forms.Label()
        Me.Label79 = New System.Windows.Forms.Label()
        Me.TargetSolutionMethod = New System.Windows.Forms.ComboBox()
        Me.TargetSolutionThickness = New System.Windows.Forms.ComboBox()
        Me.TargetComp = New System.Windows.Forms.ComboBox()
        Me.Label74 = New System.Windows.Forms.Label()
        Me.GroupBox28 = New System.Windows.Forms.GroupBox()
        Me.Label57 = New System.Windows.Forms.Label()
        Me.Label56 = New System.Windows.Forms.Label()
        Me.TargetNormalCalc = New System.Windows.Forms.ComboBox()
        Me.TargetZPosition = New System.Windows.Forms.TextBox()
        Me.TargetZNormal = New System.Windows.Forms.TextBox()
        Me.Label75 = New System.Windows.Forms.Label()
        Me.Label61 = New System.Windows.Forms.Label()
        Me.TargetYPosition = New System.Windows.Forms.TextBox()
        Me.TargetYNormal = New System.Windows.Forms.TextBox()
        Me.Label72 = New System.Windows.Forms.Label()
        Me.Label76 = New System.Windows.Forms.Label()
        Me.TargetXNormal = New System.Windows.Forms.TextBox()
        Me.TargetXPosition = New System.Windows.Forms.TextBox()
        Me.Label73 = New System.Windows.Forms.Label()
        Me.Label77 = New System.Windows.Forms.Label()
        Me.TargetRemove = New System.Windows.Forms.Button()
        Me.TargetMoveDown = New System.Windows.Forms.Button()
        Me.TargetAdd = New System.Windows.Forms.Button()
        Me.TargetMoveUp = New System.Windows.Forms.Button()
        Me.TargetDup = New System.Windows.Forms.Button()
        Me.TabHeatTransfer = New System.Windows.Forms.TabPage()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.VHeatSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupVHeats = New System.Windows.Forms.GroupBox()
        Me.VHeatComp2 = New System.Windows.Forms.ComboBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.VHeatComp1 = New System.Windows.Forms.ComboBox()
        Me.Label39 = New System.Windows.Forms.Label()
        Me.VHeatDup = New System.Windows.Forms.Button()
        Me.VHeatRemove = New System.Windows.Forms.Button()
        Me.VHeatAdd = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.HHeatSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupHHeats = New System.Windows.Forms.GroupBox()
        Me.HHeatComp2 = New System.Windows.Forms.ComboBox()
        Me.Label86 = New System.Windows.Forms.Label()
        Me.HHeatFraction = New System.Windows.Forms.TextBox()
        Me.HHeatComp1 = New System.Windows.Forms.ComboBox()
        Me.Label93 = New System.Windows.Forms.Label()
        Me.Label85 = New System.Windows.Forms.Label()
        Me.HHeatDup = New System.Windows.Forms.Button()
        Me.HHeatRemove = New System.Windows.Forms.Button()
        Me.HHeatAdd = New System.Windows.Forms.Button()
        Me.TabMain = New System.Windows.Forms.TabControl()
        Me.TabMaterials = New System.Windows.Forms.TabPage()
        Me.ThermalFromFile = New System.Windows.Forms.Button()
        Me.ThermalRemove = New System.Windows.Forms.Button()
        Me.ThermalAdd = New System.Windows.Forms.Button()
        Me.ThermalDup = New System.Windows.Forms.Button()
        Me.ThermalSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupThermal = New System.Windows.Forms.GroupBox()
        Me.ThermalThickness = New System.Windows.Forms.TextBox()
        Me.Label53 = New System.Windows.Forms.Label()
        Me.ThermalConductivity = New System.Windows.Forms.TextBox()
        Me.Label62 = New System.Windows.Forms.Label()
        Me.ThermalSpecificHeat = New System.Windows.Forms.TextBox()
        Me.Label84 = New System.Windows.Forms.Label()
        Me.ThermalLongName = New System.Windows.Forms.TextBox()
        Me.Label94 = New System.Windows.Forms.Label()
        Me.ThermalDensity = New System.Windows.Forms.TextBox()
        Me.Label102 = New System.Windows.Forms.Label()
        Me.Label103 = New System.Windows.Forms.Label()
        Me.ThermalEmissivity = New System.Windows.Forms.TextBox()
        Me.Label104 = New System.Windows.Forms.Label()
        Me.ThermalShortName = New System.Windows.Forms.TextBox()
        Me.TabVisuals = New System.Windows.Forms.TabPage()
        Me.GroupVisualResolution = New System.Windows.Forms.GroupBox()
        Me.Label35 = New System.Windows.Forms.Label()
        Me.VisualizationZ = New System.Windows.Forms.TextBox()
        Me.Label32 = New System.Windows.Forms.Label()
        Me.VisualizationY = New System.Windows.Forms.TextBox()
        Me.Label31 = New System.Windows.Forms.Label()
        Me.VisualizationX = New System.Windows.Forms.TextBox()
        Me.VisualResolution = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.VisualSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.VisualizationAxisLabel = New System.Windows.Forms.Label()
        Me.VisualizationAdd = New System.Windows.Forms.Button()
        Me.VisualizationAxis = New System.Windows.Forms.ComboBox()
        Me.VisualizationType = New System.Windows.Forms.ComboBox()
        Me.VisualizationValueLabel = New System.Windows.Forms.Label()
        Me.VisualizationDup = New System.Windows.Forms.Button()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.VisualizationComp = New System.Windows.Forms.ComboBox()
        Me.VisualizationDefaults = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.VisualizationValue = New System.Windows.Forms.TextBox()
        Me.VisualizationRemove = New System.Windows.Forms.Button()
        Me.OpenDataFileDialog = New System.Windows.Forms.OpenFileDialog()
        Me.SaveDataFileDialog = New System.Windows.Forms.SaveFileDialog()
        Me.HelpProvider = New System.Windows.Forms.HelpProvider()
        Me.ErrorProvider1 = New System.Windows.Forms.ErrorProvider(Me.components)
        Me.MainSave = New System.Windows.Forms.Button()
        Me.MainRun = New System.Windows.Forms.Button()
        Me.MainGeometry = New System.Windows.Forms.Button()
        Me.MainOpen = New System.Windows.Forms.Button()
        Me.C1SizerLight1 = New C1.Win.C1Sizer.C1SizerLight(Me.components)
        CType(Me.Errors, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.Message, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabEnvironment.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        Me.GroupBox12.SuspendLayout()
        Me.GroupBox11.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.TabHorizontalFlow.SuspendLayout()
        CType(Me.HVentSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupHVentGeometry.SuspendLayout()
        Me.GroupBox14.SuspendLayout()
        Me.GroupBox13.SuspendLayout()
        Me.TabVerticalFlow.SuspendLayout()
        CType(Me.VVentSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupVVents.SuspendLayout()
        Me.GroupBox17.SuspendLayout()
        Me.GroupBox18.SuspendLayout()
        Me.TabGeometry.SuspendLayout()
        CType(Me.CompSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupCompartments.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupFlowCharacteristics.SuspendLayout()
        CType(Me.CompVariableArea, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupCompSurfaces.SuspendLayout()
        Me.TabMechanicalFlow.SuspendLayout()
        CType(Me.MVentSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupMVents.SuspendLayout()
        Me.GroupBox20.SuspendLayout()
        Me.GroupBox21.SuspendLayout()
        Me.TabFires.SuspendLayout()
        CType(Me.FireSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupFire.SuspendLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabDetection.SuspendLayout()
        CType(Me.DetectorSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupDetectors.SuspendLayout()
        Me.GroupBox33.SuspendLayout()
        Me.TabTargets.SuspendLayout()
        CType(Me.TargetSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupTargets.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox28.SuspendLayout()
        Me.TabHeatTransfer.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.VHeatSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupVHeats.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.HHeatSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupHHeats.SuspendLayout()
        Me.TabMain.SuspendLayout()
        Me.TabMaterials.SuspendLayout()
        CType(Me.ThermalSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupThermal.SuspendLayout()
        Me.TabVisuals.SuspendLayout()
        Me.GroupVisualResolution.SuspendLayout()
        CType(Me.VisualResolution, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox4.SuspendLayout()
        CType(Me.VisualSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ErrorProvider1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'StatusBar
        '
        Me.StatusBar.Location = New System.Drawing.Point(0, 684)
        Me.StatusBar.Name = "StatusBar"
        Me.StatusBar.Panels.AddRange(New System.Windows.Forms.StatusBarPanel() {Me.Errors, Me.Message})
        Me.StatusBar.ShowPanels = True
        Me.StatusBar.Size = New System.Drawing.Size(1004, 22)
        Me.StatusBar.TabIndex = 2
        '
        'Errors
        '
        Me.Errors.BorderStyle = System.Windows.Forms.StatusBarPanelBorderStyle.None
        Me.Errors.Name = "Errors"
        Me.Errors.Text = "No Errors"
        Me.Errors.Width = 200
        '
        'Message
        '
        Me.Message.BorderStyle = System.Windows.Forms.StatusBarPanelBorderStyle.None
        Me.Message.Name = "Message"
        Me.Message.Text = "0"
        Me.Message.Width = 800
        '
        'MainMenu
        '
        Me.MainMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuFile, Me.MenuRun, Me.MenuView, Me.MenuHelp})
        '
        'MenuFile
        '
        Me.MenuFile.Index = 0
        Me.MenuFile.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuNew, Me.MenuOpen, Me.MenuSave, Me.MenuSaveAs, Me.MenuExit, Me.MenuRecentSeparator, Me.MenuRecent1, Me.MenuRecent2, Me.MenuRecent3, Me.MenuRecent4})
        Me.MenuFile.Text = "File"
        '
        'MenuNew
        '
        Me.MenuNew.Index = 0
        Me.MenuNew.Shortcut = System.Windows.Forms.Shortcut.CtrlN
        Me.MenuNew.Text = "New"
        '
        'MenuOpen
        '
        Me.MenuOpen.Index = 1
        Me.MenuOpen.Shortcut = System.Windows.Forms.Shortcut.CtrlO
        Me.MenuOpen.Text = "Open"
        '
        'MenuSave
        '
        Me.MenuSave.Index = 2
        Me.MenuSave.Shortcut = System.Windows.Forms.Shortcut.CtrlS
        Me.MenuSave.Text = "Save"
        '
        'MenuSaveAs
        '
        Me.MenuSaveAs.Index = 3
        Me.MenuSaveAs.Text = "Save As"
        '
        'MenuExit
        '
        Me.MenuExit.Index = 4
        Me.MenuExit.Text = "Exit"
        '
        'MenuRecentSeparator
        '
        Me.MenuRecentSeparator.Index = 5
        Me.MenuRecentSeparator.Text = "-"
        Me.MenuRecentSeparator.Visible = False
        '
        'MenuRecent1
        '
        Me.MenuRecent1.Index = 6
        Me.MenuRecent1.Text = "1 File 1"
        Me.MenuRecent1.Visible = False
        '
        'MenuRecent2
        '
        Me.MenuRecent2.Index = 7
        Me.MenuRecent2.Text = "2 File 2"
        Me.MenuRecent2.Visible = False
        '
        'MenuRecent3
        '
        Me.MenuRecent3.Index = 8
        Me.MenuRecent3.Text = "3 File 3"
        Me.MenuRecent3.Visible = False
        '
        'MenuRecent4
        '
        Me.MenuRecent4.Index = 9
        Me.MenuRecent4.Text = "4 File 4"
        Me.MenuRecent4.Visible = False
        '
        'MenuRun
        '
        Me.MenuRun.Index = 1
        Me.MenuRun.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuSMVGeometry, Me.MenuRunCFast, Me.MenuSMVSimulation, Me.MenuItem1, Me.MenuItem2})
        Me.MenuRun.Text = "Run!"
        '
        'MenuSMVGeometry
        '
        Me.MenuSMVGeometry.Index = 0
        Me.MenuSMVGeometry.Text = "Create Geometry File"
        '
        'MenuRunCFast
        '
        Me.MenuRunCFast.Index = 1
        Me.MenuRunCFast.Shortcut = System.Windows.Forms.Shortcut.CtrlR
        Me.MenuRunCFast.Text = "Model Simulation, CFAST"
        '
        'MenuSMVSimulation
        '
        Me.MenuSMVSimulation.Index = 2
        Me.MenuSMVSimulation.Text = "Simulation Visualization, Smokeview"
        '
        'MenuItem1
        '
        Me.MenuItem1.Index = 3
        Me.MenuItem1.Text = "-"
        '
        'MenuItem2
        '
        Me.MenuItem2.Index = 4
        Me.MenuItem2.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuDetailedOutput, Me.MenuTotalMassOutput, Me.MenuNetHeatFluxOutput, Me.MenuValidationOutput, Me.MenuItem4, Me.MenuDebugOutput, Me.MenuShowCFAST})
        Me.MenuItem2.Text = "Output Options"
        '
        'MenuDetailedOutput
        '
        Me.MenuDetailedOutput.Checked = True
        Me.MenuDetailedOutput.Index = 0
        Me.MenuDetailedOutput.Text = "Detailed Output File"
        '
        'MenuTotalMassOutput
        '
        Me.MenuTotalMassOutput.Index = 1
        Me.MenuTotalMassOutput.Text = "Total Mass Output File"
        '
        'MenuNetHeatFluxOutput
        '
        Me.MenuNetHeatFluxOutput.Checked = True
        Me.MenuNetHeatFluxOutput.Index = 2
        Me.MenuNetHeatFluxOutput.Text = "Net Heat Flux"
        '
        'MenuValidationOutput
        '
        Me.MenuValidationOutput.Index = 3
        Me.MenuValidationOutput.Text = "CFAST Validation Output"
        '
        'MenuItem4
        '
        Me.MenuItem4.Index = 4
        Me.MenuItem4.Text = "-"
        '
        'MenuDebugOutput
        '
        Me.MenuDebugOutput.Index = 5
        Me.MenuDebugOutput.Shortcut = System.Windows.Forms.Shortcut.CtrlShiftD
        Me.MenuDebugOutput.Text = "Debug Output"
        '
        'MenuShowCFAST
        '
        Me.MenuShowCFAST.Index = 6
        Me.MenuShowCFAST.Text = "Show CFAST Window"
        '
        'MenuView
        '
        Me.MenuView.Index = 2
        Me.MenuView.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuUnits, Me.MenuItem5, Me.MenuViewInput, Me.MenuViewOutput, Me.MenuViewLog})
        Me.MenuView.Text = "View"
        '
        'MenuUnits
        '
        Me.MenuUnits.Index = 0
        Me.MenuUnits.Text = "Select Engineering Units"
        '
        'MenuItem5
        '
        Me.MenuItem5.Index = 1
        Me.MenuItem5.Text = "-"
        '
        'MenuViewInput
        '
        Me.MenuViewInput.Index = 2
        Me.MenuViewInput.Text = "View CFAST Input File"
        '
        'MenuViewOutput
        '
        Me.MenuViewOutput.Index = 3
        Me.MenuViewOutput.Text = "View CFAST Output File"
        '
        'MenuViewLog
        '
        Me.MenuViewLog.Index = 4
        Me.MenuViewLog.Text = "View CFAST Log File"
        '
        'MenuHelp
        '
        Me.MenuHelp.Index = 3
        Me.MenuHelp.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuShowHelp, Me.MenuCFASTWeb, Me.MenuAbout})
        Me.MenuHelp.Text = "Help"
        '
        'MenuShowHelp
        '
        Me.MenuShowHelp.Index = 0
        Me.MenuShowHelp.Text = "Documentation"
        '
        'MenuCFASTWeb
        '
        Me.MenuCFASTWeb.Index = 1
        Me.MenuCFASTWeb.Text = "CFast Web Site"
        '
        'MenuAbout
        '
        Me.MenuAbout.Index = 2
        Me.MenuAbout.Text = "About"
        '
        'TabEnvironment
        '
        Me.TabEnvironment.Controls.Add(Me.EnvErrors)
        Me.TabEnvironment.Controls.Add(Me.EnvTitle)
        Me.TabEnvironment.Controls.Add(Me.Label28)
        Me.TabEnvironment.Controls.Add(Me.GroupBox8)
        Me.TabEnvironment.Controls.Add(Me.GroupBox7)
        Me.TabEnvironment.Location = New System.Drawing.Point(4, 22)
        Me.TabEnvironment.Name = "TabEnvironment"
        Me.TabEnvironment.Size = New System.Drawing.Size(976, 592)
        Me.TabEnvironment.TabIndex = 0
        Me.TabEnvironment.Text = "Simulation"
        '
        'EnvErrors
        '
        Me.EnvErrors.Location = New System.Drawing.Point(33, 338)
        Me.EnvErrors.Multiline = True
        Me.EnvErrors.Name = "EnvErrors"
        Me.EnvErrors.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.EnvErrors.Size = New System.Drawing.Size(910, 163)
        Me.EnvErrors.TabIndex = 19
        Me.EnvErrors.TabStop = False
        Me.EnvErrors.Text = "No Errors"
        '
        'EnvTitle
        '
        Me.EnvTitle.Location = New System.Drawing.Point(272, 14)
        Me.EnvTitle.Name = "EnvTitle"
        Me.EnvTitle.Size = New System.Drawing.Size(472, 20)
        Me.EnvTitle.TabIndex = 1
        Me.EnvTitle.Text = "CFAST simulation"
        '
        'Label28
        '
        Me.Label28.Location = New System.Drawing.Point(232, 14)
        Me.Label28.Name = "Label28"
        Me.Label28.Size = New System.Drawing.Size(32, 23)
        Me.Label28.TabIndex = 101
        Me.Label28.Text = "Title:"
        Me.Label28.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox8
        '
        Me.GroupBox8.Controls.Add(Me.EnvAdiabatic)
        Me.GroupBox8.Controls.Add(Me.GroupBox12)
        Me.GroupBox8.Controls.Add(Me.GroupBox11)
        Me.GroupBox8.Location = New System.Drawing.Point(496, 58)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.Size = New System.Drawing.Size(392, 227)
        Me.GroupBox8.TabIndex = 11
        Me.GroupBox8.TabStop = False
        Me.GroupBox8.Text = "Simulaiton Conditions"
        '
        'EnvAdiabatic
        '
        Me.EnvAdiabatic.AutoSize = True
        Me.EnvAdiabatic.Location = New System.Drawing.Point(27, 204)
        Me.EnvAdiabatic.Name = "EnvAdiabatic"
        Me.EnvAdiabatic.Size = New System.Drawing.Size(180, 17)
        Me.EnvAdiabatic.TabIndex = 18
        Me.EnvAdiabatic.Text = "Adiabatic Compartment Surfaces"
        Me.EnvAdiabatic.UseVisualStyleBackColor = True
        '
        'GroupBox12
        '
        Me.GroupBox12.Controls.Add(Me.Label24)
        Me.GroupBox12.Controls.Add(Me.EnvExtAmbElevation)
        Me.GroupBox12.Controls.Add(Me.Label26)
        Me.GroupBox12.Controls.Add(Me.EnvExtAmbPress)
        Me.GroupBox12.Controls.Add(Me.Label27)
        Me.GroupBox12.Controls.Add(Me.EnvExtAmbTemp)
        Me.GroupBox12.Location = New System.Drawing.Point(16, 112)
        Me.GroupBox12.Name = "GroupBox12"
        Me.GroupBox12.Size = New System.Drawing.Size(368, 86)
        Me.GroupBox12.TabIndex = 17
        Me.GroupBox12.TabStop = False
        Me.GroupBox12.Text = "Exterior"
        '
        'Label24
        '
        Me.Label24.Location = New System.Drawing.Point(200, 16)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(56, 23)
        Me.Label24.TabIndex = 108
        Me.Label24.Text = "Elevation:"
        Me.Label24.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvExtAmbElevation
        '
        Me.EnvExtAmbElevation.Location = New System.Drawing.Point(264, 16)
        Me.EnvExtAmbElevation.Name = "EnvExtAmbElevation"
        Me.EnvExtAmbElevation.Size = New System.Drawing.Size(96, 20)
        Me.EnvExtAmbElevation.TabIndex = 19
        Me.EnvExtAmbElevation.Text = "0 m"
        '
        'Label26
        '
        Me.Label26.Location = New System.Drawing.Point(8, 48)
        Me.Label26.Name = "Label26"
        Me.Label26.Size = New System.Drawing.Size(72, 23)
        Me.Label26.TabIndex = 109
        Me.Label26.Text = "Pressure:"
        Me.Label26.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvExtAmbPress
        '
        Me.EnvExtAmbPress.Location = New System.Drawing.Point(88, 48)
        Me.EnvExtAmbPress.Name = "EnvExtAmbPress"
        Me.EnvExtAmbPress.Size = New System.Drawing.Size(96, 20)
        Me.EnvExtAmbPress.TabIndex = 20
        Me.EnvExtAmbPress.Text = "101325 Pa"
        '
        'Label27
        '
        Me.Label27.Location = New System.Drawing.Point(8, 16)
        Me.Label27.Name = "Label27"
        Me.Label27.Size = New System.Drawing.Size(72, 23)
        Me.Label27.TabIndex = 110
        Me.Label27.Text = "Temperature:"
        Me.Label27.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvExtAmbTemp
        '
        Me.EnvExtAmbTemp.Location = New System.Drawing.Point(88, 16)
        Me.EnvExtAmbTemp.Name = "EnvExtAmbTemp"
        Me.EnvExtAmbTemp.Size = New System.Drawing.Size(96, 20)
        Me.EnvExtAmbTemp.TabIndex = 18
        Me.EnvExtAmbTemp.Text = "20 °C"
        '
        'GroupBox11
        '
        Me.GroupBox11.Controls.Add(Me.Label30)
        Me.GroupBox11.Controls.Add(Me.EnvIntAmbRH)
        Me.GroupBox11.Controls.Add(Me.Label8)
        Me.GroupBox11.Controls.Add(Me.EnvIntAmbElevation)
        Me.GroupBox11.Controls.Add(Me.Label6)
        Me.GroupBox11.Controls.Add(Me.EnvIntAmbPress)
        Me.GroupBox11.Controls.Add(Me.Label5)
        Me.GroupBox11.Controls.Add(Me.EnvIntAmbTemp)
        Me.GroupBox11.Location = New System.Drawing.Point(16, 16)
        Me.GroupBox11.Name = "GroupBox11"
        Me.GroupBox11.Size = New System.Drawing.Size(368, 86)
        Me.GroupBox11.TabIndex = 12
        Me.GroupBox11.TabStop = False
        Me.GroupBox11.Text = "Interior"
        '
        'Label30
        '
        Me.Label30.Location = New System.Drawing.Point(200, 48)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(56, 23)
        Me.Label30.TabIndex = 117
        Me.Label30.Text = "Humidity"
        Me.Label30.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbRH
        '
        Me.EnvIntAmbRH.Location = New System.Drawing.Point(264, 48)
        Me.EnvIntAmbRH.Name = "EnvIntAmbRH"
        Me.EnvIntAmbRH.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbRH.TabIndex = 116
        Me.EnvIntAmbRH.Text = "50 %"
        '
        'Label8
        '
        Me.Label8.Location = New System.Drawing.Point(200, 16)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(56, 23)
        Me.Label8.TabIndex = 112
        Me.Label8.Text = "Elevation:"
        Me.Label8.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbElevation
        '
        Me.EnvIntAmbElevation.Location = New System.Drawing.Point(264, 16)
        Me.EnvIntAmbElevation.Name = "EnvIntAmbElevation"
        Me.EnvIntAmbElevation.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbElevation.TabIndex = 14
        Me.EnvIntAmbElevation.Text = "0 m"
        '
        'Label6
        '
        Me.Label6.Location = New System.Drawing.Point(8, 48)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(72, 23)
        Me.Label6.TabIndex = 114
        Me.Label6.Text = "Pressure:"
        Me.Label6.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbPress
        '
        Me.EnvIntAmbPress.Location = New System.Drawing.Point(88, 48)
        Me.EnvIntAmbPress.Name = "EnvIntAmbPress"
        Me.EnvIntAmbPress.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbPress.TabIndex = 15
        Me.EnvIntAmbPress.Text = "101325 Pa"
        '
        'Label5
        '
        Me.Label5.Location = New System.Drawing.Point(8, 16)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(72, 23)
        Me.Label5.TabIndex = 115
        Me.Label5.Text = "Temperature:"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbTemp
        '
        Me.EnvIntAmbTemp.Location = New System.Drawing.Point(88, 16)
        Me.EnvIntAmbTemp.Name = "EnvIntAmbTemp"
        Me.EnvIntAmbTemp.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbTemp.TabIndex = 13
        Me.EnvIntAmbTemp.Text = "20 °C"
        '
        'GroupBox7
        '
        Me.GroupBox7.Controls.Add(Me.Label3)
        Me.GroupBox7.Controls.Add(Me.EnvTimeStep)
        Me.GroupBox7.Controls.Add(Me.Label25)
        Me.GroupBox7.Controls.Add(Me.EnvSmokeviewInterval)
        Me.GroupBox7.Controls.Add(Me.Label4)
        Me.GroupBox7.Controls.Add(Me.EnvSpreadOutInterval)
        Me.GroupBox7.Controls.Add(Me.Label2)
        Me.GroupBox7.Controls.Add(Me.EnvTextOutInterval)
        Me.GroupBox7.Controls.Add(Me.Label1)
        Me.GroupBox7.Controls.Add(Me.EnvSimTime)
        Me.GroupBox7.Location = New System.Drawing.Point(113, 63)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.Size = New System.Drawing.Size(304, 216)
        Me.GroupBox7.TabIndex = 2
        Me.GroupBox7.TabStop = False
        Me.GroupBox7.Text = "Simulation Times"
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(20, 149)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(152, 23)
        Me.Label3.TabIndex = 123
        Me.Label3.Text = "Maximum Time Step:"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvTimeStep
        '
        Me.EnvTimeStep.Location = New System.Drawing.Point(188, 150)
        Me.EnvTimeStep.Name = "EnvTimeStep"
        Me.EnvTimeStep.Size = New System.Drawing.Size(96, 20)
        Me.EnvTimeStep.TabIndex = 122
        Me.EnvTimeStep.Text = "Default"
        '
        'Label25
        '
        Me.Label25.Location = New System.Drawing.Point(20, 123)
        Me.Label25.Name = "Label25"
        Me.Label25.Size = New System.Drawing.Size(152, 23)
        Me.Label25.TabIndex = 117
        Me.Label25.Text = "Smokeview Output Interval:"
        Me.Label25.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvSmokeviewInterval
        '
        Me.EnvSmokeviewInterval.Location = New System.Drawing.Point(188, 124)
        Me.EnvSmokeviewInterval.Name = "EnvSmokeviewInterval"
        Me.EnvSmokeviewInterval.Size = New System.Drawing.Size(96, 20)
        Me.EnvSmokeviewInterval.TabIndex = 7
        Me.EnvSmokeviewInterval.Text = "10 s"
        '
        'Label4
        '
        Me.Label4.Location = New System.Drawing.Point(20, 97)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(152, 23)
        Me.Label4.TabIndex = 118
        Me.Label4.Text = "Spreadsheet Output Interval:"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvSpreadOutInterval
        '
        Me.EnvSpreadOutInterval.Location = New System.Drawing.Point(188, 98)
        Me.EnvSpreadOutInterval.Name = "EnvSpreadOutInterval"
        Me.EnvSpreadOutInterval.Size = New System.Drawing.Size(96, 20)
        Me.EnvSpreadOutInterval.TabIndex = 6
        Me.EnvSpreadOutInterval.Text = "10 s"
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(20, 71)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(152, 23)
        Me.Label2.TabIndex = 120
        Me.Label2.Text = "Text Output Interval:"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvTextOutInterval
        '
        Me.EnvTextOutInterval.Location = New System.Drawing.Point(188, 72)
        Me.EnvTextOutInterval.Name = "EnvTextOutInterval"
        Me.EnvTextOutInterval.Size = New System.Drawing.Size(96, 20)
        Me.EnvTextOutInterval.TabIndex = 4
        Me.EnvTextOutInterval.Text = "50 s"
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(20, 45)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(152, 23)
        Me.Label1.TabIndex = 121
        Me.Label1.Text = "Simulation Time:"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvSimTime
        '
        Me.EnvSimTime.Location = New System.Drawing.Point(188, 46)
        Me.EnvSimTime.Name = "EnvSimTime"
        Me.EnvSimTime.Size = New System.Drawing.Size(96, 20)
        Me.EnvSimTime.TabIndex = 3
        Me.EnvSimTime.Text = "900 s"
        '
        'TabHorizontalFlow
        '
        Me.TabHorizontalFlow.Controls.Add(Me.HVentSummary)
        Me.TabHorizontalFlow.Controls.Add(Me.GroupHVentGeometry)
        Me.TabHorizontalFlow.Controls.Add(Me.HVentRemove)
        Me.TabHorizontalFlow.Controls.Add(Me.HVentAdd)
        Me.TabHorizontalFlow.Controls.Add(Me.HVentMoveDown)
        Me.TabHorizontalFlow.Controls.Add(Me.HVentMoveUp)
        Me.TabHorizontalFlow.Controls.Add(Me.HVentDup)
        Me.TabHorizontalFlow.Location = New System.Drawing.Point(4, 22)
        Me.TabHorizontalFlow.Name = "TabHorizontalFlow"
        Me.TabHorizontalFlow.Size = New System.Drawing.Size(976, 592)
        Me.TabHorizontalFlow.TabIndex = 4
        Me.TabHorizontalFlow.Text = "Wall Vents"
        '
        'HVentSummary
        '
        Me.HVentSummary.AllowEditing = False
        Me.HVentSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.HVentSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.HVentSummary.ColumnInfo = resources.GetString("HVentSummary.ColumnInfo")
        Me.HVentSummary.ExtendLastCol = True
        Me.HVentSummary.FocusRect = C1.Win.C1FlexGrid.FocusRectEnum.None
        Me.HVentSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.HVentSummary.Location = New System.Drawing.Point(97, 41)
        Me.HVentSummary.Name = "HVentSummary"
        Me.HVentSummary.Rows.Count = 101
        Me.HVentSummary.Rows.DefaultSize = 19
        Me.HVentSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.HVentSummary.SelectionMode = C1.Win.C1FlexGrid.SelectionModeEnum.Row
        Me.HVentSummary.ShowSortPosition = C1.Win.C1FlexGrid.ShowSortPositionEnum.None
        Me.HVentSummary.Size = New System.Drawing.Size(782, 192)
        Me.HVentSummary.StyleInfo = resources.GetString("HVentSummary.StyleInfo")
        Me.HVentSummary.TabIndex = 0
        Me.HVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupHVentGeometry
        '
        Me.GroupHVentGeometry.Controls.Add(Me.Label19)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFractionTime)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentOffset1)
        Me.GroupHVentGeometry.Controls.Add(Me.Label82)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFinalFraction)
        Me.GroupHVentGeometry.Controls.Add(Me.Label67)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFace)
        Me.GroupHVentGeometry.Controls.Add(Me.Label37)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentInitialFraction)
        Me.GroupHVentGeometry.Controls.Add(Me.Label36)
        Me.GroupHVentGeometry.Controls.Add(Me.GroupBox14)
        Me.GroupHVentGeometry.Controls.Add(Me.GroupBox13)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentSoffit)
        Me.GroupHVentGeometry.Controls.Add(Me.Label34)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentSill)
        Me.GroupHVentGeometry.Controls.Add(Me.Label33)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentWidth)
        Me.GroupHVentGeometry.Controls.Add(Me.Label23)
        Me.GroupHVentGeometry.Location = New System.Drawing.Point(66, 297)
        Me.GroupHVentGeometry.Name = "GroupHVentGeometry"
        Me.GroupHVentGeometry.Size = New System.Drawing.Size(844, 220)
        Me.GroupHVentGeometry.TabIndex = 7
        Me.GroupHVentGeometry.TabStop = False
        Me.GroupHVentGeometry.Text = "Vent 1 Geometry"
        '
        'Label19
        '
        Me.Label19.Location = New System.Drawing.Point(572, 114)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(72, 23)
        Me.Label19.TabIndex = 20
        Me.Label19.Text = "Vent Offset:"
        Me.Label19.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentFractionTime
        '
        Me.HVentFractionTime.Location = New System.Drawing.Point(418, 144)
        Me.HVentFractionTime.Name = "HVentFractionTime"
        Me.HVentFractionTime.Size = New System.Drawing.Size(96, 20)
        Me.HVentFractionTime.TabIndex = 4
        '
        'HVentOffset1
        '
        Me.HVentOffset1.Location = New System.Drawing.Point(652, 114)
        Me.HVentOffset1.Name = "HVentOffset1"
        Me.HVentOffset1.Size = New System.Drawing.Size(96, 20)
        Me.HVentOffset1.TabIndex = 1
        Me.HVentOffset1.Text = "0 m"
        '
        'Label82
        '
        Me.Label82.AutoSize = True
        Me.Label82.Location = New System.Drawing.Point(306, 146)
        Me.Label82.Name = "Label82"
        Me.Label82.Size = New System.Drawing.Size(101, 13)
        Me.Label82.TabIndex = 34
        Me.Label82.Text = "Change Fraction At:"
        Me.Label82.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentFinalFraction
        '
        Me.HVentFinalFraction.Location = New System.Drawing.Point(418, 176)
        Me.HVentFinalFraction.Name = "HVentFinalFraction"
        Me.HVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.HVentFinalFraction.TabIndex = 5
        '
        'Label67
        '
        Me.Label67.Location = New System.Drawing.Point(282, 174)
        Me.Label67.Name = "Label67"
        Me.Label67.Size = New System.Drawing.Size(128, 24)
        Me.Label67.TabIndex = 32
        Me.Label67.Text = "Final Opening Fraction:"
        Me.Label67.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentFace
        '
        Me.HVentFace.ItemHeight = 13
        Me.HVentFace.Items.AddRange(New Object() {"Front", "Right", "Rear", "Left"})
        Me.HVentFace.Location = New System.Drawing.Point(652, 146)
        Me.HVentFace.Name = "HVentFace"
        Me.HVentFace.Size = New System.Drawing.Size(96, 21)
        Me.HVentFace.TabIndex = 7
        Me.HVentFace.Text = "Front"
        '
        'Label37
        '
        Me.Label37.Location = New System.Drawing.Point(604, 143)
        Me.Label37.Name = "Label37"
        Me.Label37.Size = New System.Drawing.Size(40, 23)
        Me.Label37.TabIndex = 30
        Me.Label37.Text = "Face:"
        Me.Label37.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentInitialFraction
        '
        Me.HVentInitialFraction.Location = New System.Drawing.Point(418, 112)
        Me.HVentInitialFraction.Name = "HVentInitialFraction"
        Me.HVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.HVentInitialFraction.TabIndex = 3
        Me.HVentInitialFraction.Text = "1"
        '
        'Label36
        '
        Me.Label36.Location = New System.Drawing.Point(282, 110)
        Me.Label36.Name = "Label36"
        Me.Label36.Size = New System.Drawing.Size(128, 24)
        Me.Label36.TabIndex = 28
        Me.Label36.Text = "Initial Opening Fraction:"
        Me.Label36.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox14
        '
        Me.GroupBox14.Controls.Add(Me.HVentComp2)
        Me.GroupBox14.Location = New System.Drawing.Point(448, 24)
        Me.GroupBox14.Name = "GroupBox14"
        Me.GroupBox14.Size = New System.Drawing.Size(344, 54)
        Me.GroupBox14.TabIndex = 11
        Me.GroupBox14.TabStop = False
        Me.GroupBox14.Text = "Second Compartment"
        '
        'HVentComp2
        '
        Me.HVentComp2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.HVentComp2.ItemHeight = 13
        Me.HVentComp2.Location = New System.Drawing.Point(8, 16)
        Me.HVentComp2.Name = "HVentComp2"
        Me.HVentComp2.Size = New System.Drawing.Size(328, 21)
        Me.HVentComp2.TabIndex = 0
        '
        'GroupBox13
        '
        Me.GroupBox13.Controls.Add(Me.HVentComp1)
        Me.GroupBox13.Location = New System.Drawing.Point(88, 24)
        Me.GroupBox13.Name = "GroupBox13"
        Me.GroupBox13.Size = New System.Drawing.Size(344, 54)
        Me.GroupBox13.TabIndex = 8
        Me.GroupBox13.TabStop = False
        Me.GroupBox13.Text = "First Compartment"
        '
        'HVentComp1
        '
        Me.HVentComp1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.HVentComp1.ItemHeight = 13
        Me.HVentComp1.Location = New System.Drawing.Point(8, 16)
        Me.HVentComp1.Name = "HVentComp1"
        Me.HVentComp1.Size = New System.Drawing.Size(328, 21)
        Me.HVentComp1.TabIndex = 0
        '
        'HVentSoffit
        '
        Me.HVentSoffit.Location = New System.Drawing.Point(146, 144)
        Me.HVentSoffit.Name = "HVentSoffit"
        Me.HVentSoffit.Size = New System.Drawing.Size(96, 20)
        Me.HVentSoffit.TabIndex = 1
        '
        'Label34
        '
        Me.Label34.Location = New System.Drawing.Point(98, 143)
        Me.Label34.Name = "Label34"
        Me.Label34.Size = New System.Drawing.Size(40, 23)
        Me.Label34.TabIndex = 19
        Me.Label34.Text = "Soffit:"
        Me.Label34.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentSill
        '
        Me.HVentSill.Location = New System.Drawing.Point(146, 112)
        Me.HVentSill.Name = "HVentSill"
        Me.HVentSill.Size = New System.Drawing.Size(96, 20)
        Me.HVentSill.TabIndex = 0
        '
        'Label33
        '
        Me.Label33.Location = New System.Drawing.Point(98, 111)
        Me.Label33.Name = "Label33"
        Me.Label33.Size = New System.Drawing.Size(40, 23)
        Me.Label33.TabIndex = 17
        Me.Label33.Text = "Sill:"
        Me.Label33.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentWidth
        '
        Me.HVentWidth.Location = New System.Drawing.Point(146, 176)
        Me.HVentWidth.Name = "HVentWidth"
        Me.HVentWidth.Size = New System.Drawing.Size(96, 20)
        Me.HVentWidth.TabIndex = 2
        '
        'Label23
        '
        Me.Label23.Location = New System.Drawing.Point(98, 176)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(40, 23)
        Me.Label23.TabIndex = 15
        Me.Label23.Text = "Width:"
        Me.Label23.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentRemove
        '
        Me.HVentRemove.Location = New System.Drawing.Point(675, 257)
        Me.HVentRemove.Name = "HVentRemove"
        Me.HVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.HVentRemove.TabIndex = 5
        Me.HVentRemove.Text = "Remove"
        '
        'HVentAdd
        '
        Me.HVentAdd.Location = New System.Drawing.Point(227, 257)
        Me.HVentAdd.Name = "HVentAdd"
        Me.HVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.HVentAdd.TabIndex = 1
        Me.HVentAdd.Text = "Add"
        '
        'HVentMoveDown
        '
        Me.HVentMoveDown.Location = New System.Drawing.Point(515, 257)
        Me.HVentMoveDown.Name = "HVentMoveDown"
        Me.HVentMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.HVentMoveDown.TabIndex = 4
        Me.HVentMoveDown.Text = "Move Down"
        '
        'HVentMoveUp
        '
        Me.HVentMoveUp.Location = New System.Drawing.Point(419, 257)
        Me.HVentMoveUp.Name = "HVentMoveUp"
        Me.HVentMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.HVentMoveUp.TabIndex = 3
        Me.HVentMoveUp.Text = "Move Up"
        '
        'HVentDup
        '
        Me.HVentDup.Location = New System.Drawing.Point(323, 257)
        Me.HVentDup.Name = "HVentDup"
        Me.HVentDup.Size = New System.Drawing.Size(75, 23)
        Me.HVentDup.TabIndex = 2
        Me.HVentDup.Text = "Duplicate"
        '
        'TabVerticalFlow
        '
        Me.TabVerticalFlow.Controls.Add(Me.VVentSummary)
        Me.TabVerticalFlow.Controls.Add(Me.VVentRemove)
        Me.TabVerticalFlow.Controls.Add(Me.VVentAdd)
        Me.TabVerticalFlow.Controls.Add(Me.VVentDup)
        Me.TabVerticalFlow.Controls.Add(Me.GroupVVents)
        Me.TabVerticalFlow.Location = New System.Drawing.Point(4, 22)
        Me.TabVerticalFlow.Name = "TabVerticalFlow"
        Me.TabVerticalFlow.Size = New System.Drawing.Size(976, 592)
        Me.TabVerticalFlow.TabIndex = 5
        Me.TabVerticalFlow.Text = "Ceiling/Floor Vents"
        '
        'VVentSummary
        '
        Me.VVentSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.VVentSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.VVentSummary.ColumnInfo = resources.GetString("VVentSummary.ColumnInfo")
        Me.VVentSummary.ExtendLastCol = True
        Me.VVentSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VVentSummary.Location = New System.Drawing.Point(278, 64)
        Me.VVentSummary.Name = "VVentSummary"
        Me.VVentSummary.Rows.Count = 101
        Me.VVentSummary.Rows.DefaultSize = 19
        Me.VVentSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VVentSummary.Size = New System.Drawing.Size(421, 114)
        Me.VVentSummary.StyleInfo = resources.GetString("VVentSummary.StyleInfo")
        Me.VVentSummary.TabIndex = 0
        Me.VVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'VVentRemove
        '
        Me.VVentRemove.Location = New System.Drawing.Point(575, 224)
        Me.VVentRemove.Name = "VVentRemove"
        Me.VVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.VVentRemove.TabIndex = 3
        Me.VVentRemove.Text = "Remove"
        '
        'VVentAdd
        '
        Me.VVentAdd.Location = New System.Drawing.Point(327, 224)
        Me.VVentAdd.Name = "VVentAdd"
        Me.VVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.VVentAdd.TabIndex = 1
        Me.VVentAdd.Text = "Add"
        '
        'VVentDup
        '
        Me.VVentDup.Location = New System.Drawing.Point(423, 224)
        Me.VVentDup.Name = "VVentDup"
        Me.VVentDup.Size = New System.Drawing.Size(75, 23)
        Me.VVentDup.TabIndex = 2
        Me.VVentDup.Text = "Duplicate"
        '
        'GroupVVents
        '
        Me.GroupVVents.Controls.Add(Me.VVentFractionTime)
        Me.GroupVVents.Controls.Add(Me.Label96)
        Me.GroupVVents.Controls.Add(Me.VVentFinalFraction)
        Me.GroupVVents.Controls.Add(Me.Label97)
        Me.GroupVVents.Controls.Add(Me.VVentInitialFraction)
        Me.GroupVVents.Controls.Add(Me.Label98)
        Me.GroupVVents.Controls.Add(Me.VVentShape)
        Me.GroupVVents.Controls.Add(Me.Label40)
        Me.GroupVVents.Controls.Add(Me.GroupBox17)
        Me.GroupVVents.Controls.Add(Me.GroupBox18)
        Me.GroupVVents.Controls.Add(Me.VVentArea)
        Me.GroupVVents.Controls.Add(Me.Label46)
        Me.GroupVVents.Location = New System.Drawing.Point(124, 275)
        Me.GroupVVents.Name = "GroupVVents"
        Me.GroupVVents.Size = New System.Drawing.Size(728, 237)
        Me.GroupVVents.TabIndex = 5
        Me.GroupVVents.TabStop = False
        Me.GroupVVents.Text = "Vent 1 Geometry"
        '
        'VVentFractionTime
        '
        Me.VVentFractionTime.Location = New System.Drawing.Point(560, 160)
        Me.VVentFractionTime.Name = "VVentFractionTime"
        Me.VVentFractionTime.Size = New System.Drawing.Size(96, 20)
        Me.VVentFractionTime.TabIndex = 3
        '
        'Label96
        '
        Me.Label96.AutoSize = True
        Me.Label96.Location = New System.Drawing.Point(448, 162)
        Me.Label96.Name = "Label96"
        Me.Label96.Size = New System.Drawing.Size(101, 13)
        Me.Label96.TabIndex = 52
        Me.Label96.Text = "Change Fraction At:"
        Me.Label96.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentFinalFraction
        '
        Me.VVentFinalFraction.Location = New System.Drawing.Point(560, 192)
        Me.VVentFinalFraction.Name = "VVentFinalFraction"
        Me.VVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.VVentFinalFraction.TabIndex = 4
        '
        'Label97
        '
        Me.Label97.Location = New System.Drawing.Point(424, 190)
        Me.Label97.Name = "Label97"
        Me.Label97.Size = New System.Drawing.Size(128, 24)
        Me.Label97.TabIndex = 50
        Me.Label97.Text = "Final Opening Fraction:"
        Me.Label97.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentInitialFraction
        '
        Me.VVentInitialFraction.Location = New System.Drawing.Point(560, 128)
        Me.VVentInitialFraction.Name = "VVentInitialFraction"
        Me.VVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.VVentInitialFraction.TabIndex = 2
        Me.VVentInitialFraction.Text = "1"
        '
        'Label98
        '
        Me.Label98.Location = New System.Drawing.Point(424, 126)
        Me.Label98.Name = "Label98"
        Me.Label98.Size = New System.Drawing.Size(128, 24)
        Me.Label98.TabIndex = 48
        Me.Label98.Text = "Initial Opening Fraction:"
        Me.Label98.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentShape
        '
        Me.VVentShape.ItemHeight = 13
        Me.VVentShape.Items.AddRange(New Object() {"Round", "Square"})
        Me.VVentShape.Location = New System.Drawing.Point(176, 176)
        Me.VVentShape.Name = "VVentShape"
        Me.VVentShape.Size = New System.Drawing.Size(96, 21)
        Me.VVentShape.TabIndex = 1
        Me.VVentShape.Text = "Round"
        '
        'Label40
        '
        Me.Label40.Location = New System.Drawing.Point(72, 175)
        Me.Label40.Name = "Label40"
        Me.Label40.Size = New System.Drawing.Size(96, 23)
        Me.Label40.TabIndex = 32
        Me.Label40.Text = "Shape:"
        Me.Label40.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox17
        '
        Me.GroupBox17.Controls.Add(Me.VVentCompBottom)
        Me.GroupBox17.Location = New System.Drawing.Point(376, 24)
        Me.GroupBox17.Name = "GroupBox17"
        Me.GroupBox17.Size = New System.Drawing.Size(344, 80)
        Me.GroupBox17.TabIndex = 8
        Me.GroupBox17.TabStop = False
        Me.GroupBox17.Text = "Bottom Compartment"
        '
        'VVentCompBottom
        '
        Me.VVentCompBottom.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VVentCompBottom.ItemHeight = 13
        Me.VVentCompBottom.Location = New System.Drawing.Point(8, 16)
        Me.VVentCompBottom.Name = "VVentCompBottom"
        Me.VVentCompBottom.Size = New System.Drawing.Size(328, 21)
        Me.VVentCompBottom.TabIndex = 0
        '
        'GroupBox18
        '
        Me.GroupBox18.Controls.Add(Me.VVentCompTop)
        Me.GroupBox18.Location = New System.Drawing.Point(8, 24)
        Me.GroupBox18.Name = "GroupBox18"
        Me.GroupBox18.Size = New System.Drawing.Size(344, 80)
        Me.GroupBox18.TabIndex = 6
        Me.GroupBox18.TabStop = False
        Me.GroupBox18.Text = "Top Compartment"
        '
        'VVentCompTop
        '
        Me.VVentCompTop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VVentCompTop.ItemHeight = 13
        Me.VVentCompTop.Location = New System.Drawing.Point(8, 16)
        Me.VVentCompTop.Name = "VVentCompTop"
        Me.VVentCompTop.Size = New System.Drawing.Size(328, 21)
        Me.VVentCompTop.TabIndex = 0
        '
        'VVentArea
        '
        Me.VVentArea.Location = New System.Drawing.Point(176, 144)
        Me.VVentArea.Name = "VVentArea"
        Me.VVentArea.Size = New System.Drawing.Size(96, 20)
        Me.VVentArea.TabIndex = 0
        Me.VVentArea.Text = "1 m"
        '
        'Label46
        '
        Me.Label46.AutoSize = True
        Me.Label46.Location = New System.Drawing.Point(56, 146)
        Me.Label46.Name = "Label46"
        Me.Label46.Size = New System.Drawing.Size(108, 13)
        Me.Label46.TabIndex = 17
        Me.Label46.Text = "Cross-Sectional Area:"
        Me.Label46.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TabGeometry
        '
        Me.TabGeometry.Controls.Add(Me.CompSummary)
        Me.TabGeometry.Controls.Add(Me.CompRemove)
        Me.TabGeometry.Controls.Add(Me.CompAdd)
        Me.TabGeometry.Controls.Add(Me.CompMoveDown)
        Me.TabGeometry.Controls.Add(Me.CompMoveUp)
        Me.TabGeometry.Controls.Add(Me.CompDup)
        Me.TabGeometry.Controls.Add(Me.GroupCompartments)
        Me.TabGeometry.Location = New System.Drawing.Point(4, 22)
        Me.TabGeometry.Name = "TabGeometry"
        Me.TabGeometry.Size = New System.Drawing.Size(976, 592)
        Me.TabGeometry.TabIndex = 0
        Me.TabGeometry.Text = "Compartments"
        '
        'CompSummary
        '
        Me.CompSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.CompSummary.AllowEditing = False
        Me.CompSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.CompSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.CompSummary.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CompSummary.AutoGenerateColumns = False
        Me.CompSummary.ColumnInfo = resources.GetString("CompSummary.ColumnInfo")
        Me.CompSummary.ExtendLastCol = True
        Me.CompSummary.FocusRect = C1.Win.C1FlexGrid.FocusRectEnum.None
        Me.CompSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.CompSummary.Location = New System.Drawing.Point(32, 16)
        Me.CompSummary.Name = "CompSummary"
        Me.CompSummary.Rows.DefaultSize = 19
        Me.CompSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.CompSummary.Size = New System.Drawing.Size(902, 106)
        Me.CompSummary.StyleInfo = resources.GetString("CompSummary.StyleInfo")
        Me.CompSummary.TabIndex = 0
        Me.CompSummary.TabStop = False
        Me.CompSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'CompRemove
        '
        Me.CompRemove.Location = New System.Drawing.Point(675, 128)
        Me.CompRemove.Name = "CompRemove"
        Me.CompRemove.Size = New System.Drawing.Size(75, 23)
        Me.CompRemove.TabIndex = 5
        Me.CompRemove.Text = "Remove"
        '
        'CompAdd
        '
        Me.CompAdd.Location = New System.Drawing.Point(227, 128)
        Me.CompAdd.Name = "CompAdd"
        Me.CompAdd.Size = New System.Drawing.Size(75, 23)
        Me.CompAdd.TabIndex = 1
        Me.CompAdd.Text = "Add"
        '
        'CompMoveDown
        '
        Me.CompMoveDown.Location = New System.Drawing.Point(515, 128)
        Me.CompMoveDown.Name = "CompMoveDown"
        Me.CompMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.CompMoveDown.TabIndex = 4
        Me.CompMoveDown.Text = "Move Down"
        '
        'CompMoveUp
        '
        Me.CompMoveUp.Location = New System.Drawing.Point(419, 128)
        Me.CompMoveUp.Name = "CompMoveUp"
        Me.CompMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.CompMoveUp.TabIndex = 3
        Me.CompMoveUp.Text = "Move Up"
        '
        'CompDup
        '
        Me.CompDup.Location = New System.Drawing.Point(323, 128)
        Me.CompDup.Name = "CompDup"
        Me.CompDup.Size = New System.Drawing.Size(75, 23)
        Me.CompDup.TabIndex = 2
        Me.CompDup.Text = "Duplicate"
        '
        'GroupCompartments
        '
        Me.GroupCompartments.Controls.Add(Me.CompName)
        Me.GroupCompartments.Controls.Add(Me.Label9)
        Me.GroupCompartments.Controls.Add(Me.GroupBox5)
        Me.GroupCompartments.Controls.Add(Me.GroupFlowCharacteristics)
        Me.GroupCompartments.Controls.Add(Me.GroupCompSurfaces)
        Me.GroupCompartments.Location = New System.Drawing.Point(16, 168)
        Me.GroupCompartments.Name = "GroupCompartments"
        Me.GroupCompartments.Size = New System.Drawing.Size(944, 368)
        Me.GroupCompartments.TabIndex = 7
        Me.GroupCompartments.TabStop = False
        Me.GroupCompartments.Text = "Compartment 1"
        '
        'CompName
        '
        Me.CompName.Location = New System.Drawing.Point(384, 16)
        Me.CompName.Name = "CompName"
        Me.CompName.Size = New System.Drawing.Size(208, 20)
        Me.CompName.TabIndex = 0
        Me.CompName.Text = "Compartment 1"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(264, 16)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(103, 13)
        Me.Label9.TabIndex = 29
        Me.Label9.Text = "Compartment Name:"
        Me.Label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox5
        '
        Me.GroupBox5.Controls.Add(Me.CompWidth)
        Me.GroupBox5.Controls.Add(Me.CompZPosition)
        Me.GroupBox5.Controls.Add(Me.Label15)
        Me.GroupBox5.Controls.Add(Me.CompYPosition)
        Me.GroupBox5.Controls.Add(Me.Label14)
        Me.GroupBox5.Controls.Add(Me.CompXPosition)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.CompHeight)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.CompDepth)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Controls.Add(Me.Label10)
        Me.GroupBox5.Location = New System.Drawing.Point(32, 48)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(368, 166)
        Me.GroupBox5.TabIndex = 9
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "Geometry"
        '
        'CompWidth
        '
        Me.CompWidth.Location = New System.Drawing.Point(69, 31)
        Me.CompWidth.Name = "CompWidth"
        Me.CompWidth.Size = New System.Drawing.Size(96, 20)
        Me.CompWidth.TabIndex = 0
        '
        'CompZPosition
        '
        Me.CompZPosition.Location = New System.Drawing.Point(261, 95)
        Me.CompZPosition.Name = "CompZPosition"
        Me.CompZPosition.Size = New System.Drawing.Size(96, 20)
        Me.CompZPosition.TabIndex = 5
        Me.CompZPosition.Text = "0 m"
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.Location = New System.Drawing.Point(237, 99)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(17, 13)
        Me.Label15.TabIndex = 37
        Me.Label15.Text = "Z:"
        Me.Label15.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'CompYPosition
        '
        Me.CompYPosition.Location = New System.Drawing.Point(261, 63)
        Me.CompYPosition.Name = "CompYPosition"
        Me.CompYPosition.Size = New System.Drawing.Size(96, 20)
        Me.CompYPosition.TabIndex = 4
        Me.CompYPosition.Text = "0 m"
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(236, 67)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(17, 13)
        Me.Label14.TabIndex = 35
        Me.Label14.Text = "Y:"
        Me.Label14.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'CompXPosition
        '
        Me.CompXPosition.Location = New System.Drawing.Point(261, 31)
        Me.CompXPosition.Name = "CompXPosition"
        Me.CompXPosition.Size = New System.Drawing.Size(96, 20)
        Me.CompXPosition.TabIndex = 3
        Me.CompXPosition.Text = "0 m"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(190, 35)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(60, 13)
        Me.Label13.TabIndex = 33
        Me.Label13.Text = "Position, X:"
        Me.Label13.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'CompHeight
        '
        Me.CompHeight.Location = New System.Drawing.Point(69, 95)
        Me.CompHeight.Name = "CompHeight"
        Me.CompHeight.Size = New System.Drawing.Size(96, 20)
        Me.CompHeight.TabIndex = 2
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(11, 99)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(57, 13)
        Me.Label12.TabIndex = 31
        Me.Label12.Text = "Height (Z):"
        Me.Label12.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'CompDepth
        '
        Me.CompDepth.Location = New System.Drawing.Point(69, 63)
        Me.CompDepth.Name = "CompDepth"
        Me.CompDepth.Size = New System.Drawing.Size(96, 20)
        Me.CompDepth.TabIndex = 1
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(13, 67)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(55, 13)
        Me.Label11.TabIndex = 209
        Me.Label11.Text = "Depth (Y):"
        Me.Label11.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(15, 35)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(54, 13)
        Me.Label10.TabIndex = 27
        Me.Label10.Text = "Width (X):"
        Me.Label10.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupFlowCharacteristics
        '
        Me.GroupFlowCharacteristics.Controls.Add(Me.CompNormal)
        Me.GroupFlowCharacteristics.Controls.Add(Me.CompCorridor)
        Me.GroupFlowCharacteristics.Controls.Add(Me.CompShaft)
        Me.GroupFlowCharacteristics.Controls.Add(Me.Label65)
        Me.GroupFlowCharacteristics.Controls.Add(Me.Label64)
        Me.GroupFlowCharacteristics.Controls.Add(Me.CompVariableArea)
        Me.GroupFlowCharacteristics.Location = New System.Drawing.Point(480, 48)
        Me.GroupFlowCharacteristics.Name = "GroupFlowCharacteristics"
        Me.GroupFlowCharacteristics.Size = New System.Drawing.Size(432, 166)
        Me.GroupFlowCharacteristics.TabIndex = 20
        Me.GroupFlowCharacteristics.TabStop = False
        Me.GroupFlowCharacteristics.Text = "Advanced"
        '
        'CompNormal
        '
        Me.CompNormal.AutoSize = True
        Me.CompNormal.Checked = True
        Me.CompNormal.Location = New System.Drawing.Point(37, 58)
        Me.CompNormal.Name = "CompNormal"
        Me.CompNormal.Size = New System.Drawing.Size(145, 17)
        Me.CompNormal.TabIndex = 50
        Me.CompNormal.TabStop = True
        Me.CompNormal.Text = "Normal (Two-zone model)"
        Me.CompNormal.UseVisualStyleBackColor = True
        '
        'CompCorridor
        '
        Me.CompCorridor.AutoSize = True
        Me.CompCorridor.Location = New System.Drawing.Point(37, 104)
        Me.CompCorridor.Name = "CompCorridor"
        Me.CompCorridor.Size = New System.Drawing.Size(156, 17)
        Me.CompCorridor.TabIndex = 49
        Me.CompCorridor.Text = "Corridor (Revised ceiling jet)"
        Me.CompCorridor.UseVisualStyleBackColor = True
        '
        'CompShaft
        '
        Me.CompShaft.AutoSize = True
        Me.CompShaft.Location = New System.Drawing.Point(37, 81)
        Me.CompShaft.Name = "CompShaft"
        Me.CompShaft.Size = New System.Drawing.Size(145, 17)
        Me.CompShaft.TabIndex = 48
        Me.CompShaft.Text = "Shaft (Single-zone model)"
        Me.CompShaft.UseVisualStyleBackColor = True
        '
        'Label65
        '
        Me.Label65.AutoSize = True
        Me.Label65.Location = New System.Drawing.Point(259, 16)
        Me.Label65.Name = "Label65"
        Me.Label65.Size = New System.Drawing.Size(144, 13)
        Me.Label65.TabIndex = 47
        Me.Label65.Text = "Variable Cross-sectional Area"
        '
        'Label64
        '
        Me.Label64.AutoSize = True
        Me.Label64.Location = New System.Drawing.Point(56, 16)
        Me.Label64.Name = "Label64"
        Me.Label64.Size = New System.Drawing.Size(101, 13)
        Me.Label64.TabIndex = 46
        Me.Label64.Text = "Flow Characteristics"
        '
        'CompVariableArea
        '
        Me.CompVariableArea.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.CompVariableArea.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.CompVariableArea.AutoClipboard = True
        Me.CompVariableArea.ColumnInfo = resources.GetString("CompVariableArea.ColumnInfo")
        Me.CompVariableArea.ExtendLastCol = True
        Me.CompVariableArea.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.CompVariableArea.Location = New System.Drawing.Point(236, 35)
        Me.CompVariableArea.Name = "CompVariableArea"
        Me.CompVariableArea.Rows.DefaultSize = 19
        Me.CompVariableArea.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.CompVariableArea.Size = New System.Drawing.Size(190, 111)
        Me.CompVariableArea.StyleInfo = resources.GetString("CompVariableArea.StyleInfo")
        Me.CompVariableArea.TabIndex = 4
        Me.CompVariableArea.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupCompSurfaces
        '
        Me.GroupCompSurfaces.Controls.Add(Me.CompSpecHeatFloor)
        Me.GroupCompSurfaces.Controls.Add(Me.CompDensityFloor)
        Me.GroupCompSurfaces.Controls.Add(Me.CompThicknessFloor)
        Me.GroupCompSurfaces.Controls.Add(Me.CompConductFloor)
        Me.GroupCompSurfaces.Controls.Add(Me.CompSpecHeatWalls)
        Me.GroupCompSurfaces.Controls.Add(Me.CompDensityWalls)
        Me.GroupCompSurfaces.Controls.Add(Me.CompThicknessWalls)
        Me.GroupCompSurfaces.Controls.Add(Me.CompConductWalls)
        Me.GroupCompSurfaces.Controls.Add(Me.CompSpecHeatCeiling)
        Me.GroupCompSurfaces.Controls.Add(Me.CompDensityCeiling)
        Me.GroupCompSurfaces.Controls.Add(Me.CompThicknessCeiling)
        Me.GroupCompSurfaces.Controls.Add(Me.CompConductCeiling)
        Me.GroupCompSurfaces.Controls.Add(Me.CompFloor)
        Me.GroupCompSurfaces.Controls.Add(Me.CompWalls)
        Me.GroupCompSurfaces.Controls.Add(Me.CompCeiling)
        Me.GroupCompSurfaces.Controls.Add(Me.Label21)
        Me.GroupCompSurfaces.Controls.Add(Me.Label22)
        Me.GroupCompSurfaces.Controls.Add(Me.Label20)
        Me.GroupCompSurfaces.Location = New System.Drawing.Point(32, 220)
        Me.GroupCompSurfaces.Name = "GroupCompSurfaces"
        Me.GroupCompSurfaces.Size = New System.Drawing.Size(880, 133)
        Me.GroupCompSurfaces.TabIndex = 16
        Me.GroupCompSurfaces.TabStop = False
        Me.GroupCompSurfaces.Text = "Materials"
        '
        'CompSpecHeatFloor
        '
        Me.CompSpecHeatFloor.AutoSize = True
        Me.CompSpecHeatFloor.Location = New System.Drawing.Point(642, 66)
        Me.CompSpecHeatFloor.Name = "CompSpecHeatFloor"
        Me.CompSpecHeatFloor.Size = New System.Drawing.Size(74, 13)
        Me.CompSpecHeatFloor.TabIndex = 52
        Me.CompSpecHeatFloor.Text = "Specific Heat:"
        '
        'CompDensityFloor
        '
        Me.CompDensityFloor.AutoSize = True
        Me.CompDensityFloor.Location = New System.Drawing.Point(642, 87)
        Me.CompDensityFloor.Name = "CompDensityFloor"
        Me.CompDensityFloor.Size = New System.Drawing.Size(45, 13)
        Me.CompDensityFloor.TabIndex = 51
        Me.CompDensityFloor.Text = "Density:"
        '
        'CompThicknessFloor
        '
        Me.CompThicknessFloor.AutoSize = True
        Me.CompThicknessFloor.Location = New System.Drawing.Point(642, 108)
        Me.CompThicknessFloor.Name = "CompThicknessFloor"
        Me.CompThicknessFloor.Size = New System.Drawing.Size(59, 13)
        Me.CompThicknessFloor.TabIndex = 50
        Me.CompThicknessFloor.Text = "Thickness:"
        '
        'CompConductFloor
        '
        Me.CompConductFloor.AutoSize = True
        Me.CompConductFloor.Location = New System.Drawing.Point(642, 45)
        Me.CompConductFloor.Name = "CompConductFloor"
        Me.CompConductFloor.Size = New System.Drawing.Size(68, 13)
        Me.CompConductFloor.TabIndex = 49
        Me.CompConductFloor.Text = "Conductivity:"
        '
        'CompSpecHeatWalls
        '
        Me.CompSpecHeatWalls.AutoSize = True
        Me.CompSpecHeatWalls.Location = New System.Drawing.Point(349, 65)
        Me.CompSpecHeatWalls.Name = "CompSpecHeatWalls"
        Me.CompSpecHeatWalls.Size = New System.Drawing.Size(74, 13)
        Me.CompSpecHeatWalls.TabIndex = 48
        Me.CompSpecHeatWalls.Text = "Specific Heat:"
        '
        'CompDensityWalls
        '
        Me.CompDensityWalls.AutoSize = True
        Me.CompDensityWalls.Location = New System.Drawing.Point(349, 86)
        Me.CompDensityWalls.Name = "CompDensityWalls"
        Me.CompDensityWalls.Size = New System.Drawing.Size(45, 13)
        Me.CompDensityWalls.TabIndex = 47
        Me.CompDensityWalls.Text = "Density:"
        '
        'CompThicknessWalls
        '
        Me.CompThicknessWalls.AutoSize = True
        Me.CompThicknessWalls.Location = New System.Drawing.Point(349, 107)
        Me.CompThicknessWalls.Name = "CompThicknessWalls"
        Me.CompThicknessWalls.Size = New System.Drawing.Size(59, 13)
        Me.CompThicknessWalls.TabIndex = 46
        Me.CompThicknessWalls.Text = "Thickness:"
        '
        'CompConductWalls
        '
        Me.CompConductWalls.AutoSize = True
        Me.CompConductWalls.Location = New System.Drawing.Point(349, 44)
        Me.CompConductWalls.Name = "CompConductWalls"
        Me.CompConductWalls.Size = New System.Drawing.Size(68, 13)
        Me.CompConductWalls.TabIndex = 45
        Me.CompConductWalls.Text = "Conductivity:"
        '
        'CompSpecHeatCeiling
        '
        Me.CompSpecHeatCeiling.AutoSize = True
        Me.CompSpecHeatCeiling.Location = New System.Drawing.Point(58, 65)
        Me.CompSpecHeatCeiling.Name = "CompSpecHeatCeiling"
        Me.CompSpecHeatCeiling.Size = New System.Drawing.Size(74, 13)
        Me.CompSpecHeatCeiling.TabIndex = 44
        Me.CompSpecHeatCeiling.Text = "Specific Heat:"
        '
        'CompDensityCeiling
        '
        Me.CompDensityCeiling.AutoSize = True
        Me.CompDensityCeiling.Location = New System.Drawing.Point(58, 86)
        Me.CompDensityCeiling.Name = "CompDensityCeiling"
        Me.CompDensityCeiling.Size = New System.Drawing.Size(45, 13)
        Me.CompDensityCeiling.TabIndex = 43
        Me.CompDensityCeiling.Text = "Density:"
        '
        'CompThicknessCeiling
        '
        Me.CompThicknessCeiling.AutoSize = True
        Me.CompThicknessCeiling.Location = New System.Drawing.Point(58, 107)
        Me.CompThicknessCeiling.Name = "CompThicknessCeiling"
        Me.CompThicknessCeiling.Size = New System.Drawing.Size(59, 13)
        Me.CompThicknessCeiling.TabIndex = 42
        Me.CompThicknessCeiling.Text = "Thickness:"
        '
        'CompConductCeiling
        '
        Me.CompConductCeiling.AutoSize = True
        Me.CompConductCeiling.Location = New System.Drawing.Point(58, 44)
        Me.CompConductCeiling.Name = "CompConductCeiling"
        Me.CompConductCeiling.Size = New System.Drawing.Size(68, 13)
        Me.CompConductCeiling.TabIndex = 41
        Me.CompConductCeiling.Text = "Conductivity:"
        '
        'CompFloor
        '
        Me.CompFloor.ItemHeight = 13
        Me.CompFloor.Location = New System.Drawing.Point(645, 21)
        Me.CompFloor.Name = "CompFloor"
        Me.CompFloor.Size = New System.Drawing.Size(192, 21)
        Me.CompFloor.TabIndex = 2
        '
        'CompWalls
        '
        Me.CompWalls.ItemHeight = 13
        Me.CompWalls.Location = New System.Drawing.Point(350, 20)
        Me.CompWalls.Name = "CompWalls"
        Me.CompWalls.Size = New System.Drawing.Size(192, 21)
        Me.CompWalls.TabIndex = 1
        '
        'CompCeiling
        '
        Me.CompCeiling.ItemHeight = 13
        Me.CompCeiling.Location = New System.Drawing.Point(61, 19)
        Me.CompCeiling.Name = "CompCeiling"
        Me.CompCeiling.Size = New System.Drawing.Size(192, 21)
        Me.CompCeiling.TabIndex = 0
        '
        'Label21
        '
        Me.Label21.AutoSize = True
        Me.Label21.Location = New System.Drawing.Point(302, 21)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(36, 13)
        Me.Label21.TabIndex = 39
        Me.Label21.Text = "Walls:"
        Me.Label21.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label22
        '
        Me.Label22.AutoSize = True
        Me.Label22.Location = New System.Drawing.Point(5, 20)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(41, 13)
        Me.Label22.TabIndex = 38
        Me.Label22.Text = "Ceiling:"
        Me.Label22.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label20
        '
        Me.Label20.AutoSize = True
        Me.Label20.Location = New System.Drawing.Point(597, 22)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(33, 13)
        Me.Label20.TabIndex = 40
        Me.Label20.Text = "Floor:"
        Me.Label20.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MainView
        '
        Me.MainView.Location = New System.Drawing.Point(697, 638)
        Me.MainView.Name = "MainView"
        Me.MainView.Size = New System.Drawing.Size(75, 23)
        Me.MainView.TabIndex = 4
        Me.MainView.Text = "View"
        '
        'TabMechanicalFlow
        '
        Me.TabMechanicalFlow.Controls.Add(Me.MVentSummary)
        Me.TabMechanicalFlow.Controls.Add(Me.MVentRemove)
        Me.TabMechanicalFlow.Controls.Add(Me.MVentAdd)
        Me.TabMechanicalFlow.Controls.Add(Me.MVentDup)
        Me.TabMechanicalFlow.Controls.Add(Me.GroupMVents)
        Me.TabMechanicalFlow.Location = New System.Drawing.Point(4, 22)
        Me.TabMechanicalFlow.Name = "TabMechanicalFlow"
        Me.TabMechanicalFlow.Size = New System.Drawing.Size(976, 592)
        Me.TabMechanicalFlow.TabIndex = 6
        Me.TabMechanicalFlow.Text = "Mechanical Ventilation"
        '
        'MVentSummary
        '
        Me.MVentSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.MVentSummary.AllowEditing = False
        Me.MVentSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.BothUniform
        Me.MVentSummary.ColumnInfo = resources.GetString("MVentSummary.ColumnInfo")
        Me.MVentSummary.ExtendLastCol = True
        Me.MVentSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.MVentSummary.Location = New System.Drawing.Point(57, 57)
        Me.MVentSummary.Name = "MVentSummary"
        Me.MVentSummary.Rows.Count = 101
        Me.MVentSummary.Rows.DefaultSize = 19
        Me.MVentSummary.Size = New System.Drawing.Size(862, 120)
        Me.MVentSummary.StyleInfo = resources.GetString("MVentSummary.StyleInfo")
        Me.MVentSummary.TabIndex = 0
        Me.MVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'MVentRemove
        '
        Me.MVentRemove.Location = New System.Drawing.Point(675, 199)
        Me.MVentRemove.Name = "MVentRemove"
        Me.MVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.MVentRemove.TabIndex = 3
        Me.MVentRemove.Text = "Remove"
        '
        'MVentAdd
        '
        Me.MVentAdd.Location = New System.Drawing.Point(227, 199)
        Me.MVentAdd.Name = "MVentAdd"
        Me.MVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.MVentAdd.TabIndex = 1
        Me.MVentAdd.Text = "Add"
        '
        'MVentDup
        '
        Me.MVentDup.Location = New System.Drawing.Point(323, 199)
        Me.MVentDup.Name = "MVentDup"
        Me.MVentDup.Size = New System.Drawing.Size(75, 23)
        Me.MVentDup.TabIndex = 2
        Me.MVentDup.Text = "Duplicate"
        '
        'GroupMVents
        '
        Me.GroupMVents.Controls.Add(Me.MVentFilterTime)
        Me.GroupMVents.Controls.Add(Me.Label38)
        Me.GroupMVents.Controls.Add(Me.MVentFilterEfficiency)
        Me.GroupMVents.Controls.Add(Me.Label54)
        Me.GroupMVents.Controls.Add(Me.MVentFractionTime)
        Me.GroupMVents.Controls.Add(Me.Label99)
        Me.GroupMVents.Controls.Add(Me.MVentFinalFraction)
        Me.GroupMVents.Controls.Add(Me.Label100)
        Me.GroupMVents.Controls.Add(Me.MVentInitialFraction)
        Me.GroupMVents.Controls.Add(Me.Label101)
        Me.GroupMVents.Controls.Add(Me.MVentZero)
        Me.GroupMVents.Controls.Add(Me.Label41)
        Me.GroupMVents.Controls.Add(Me.GroupBox20)
        Me.GroupMVents.Controls.Add(Me.MVentDropoff)
        Me.GroupMVents.Controls.Add(Me.Label42)
        Me.GroupMVents.Controls.Add(Me.GroupBox21)
        Me.GroupMVents.Controls.Add(Me.MVentFlow)
        Me.GroupMVents.Controls.Add(Me.Label45)
        Me.GroupMVents.Controls.Add(Me.Label47)
        Me.GroupMVents.Location = New System.Drawing.Point(128, 247)
        Me.GroupMVents.Name = "GroupMVents"
        Me.GroupMVents.Size = New System.Drawing.Size(720, 291)
        Me.GroupMVents.TabIndex = 5
        Me.GroupMVents.TabStop = False
        Me.GroupMVents.Text = "Vent 1 Geometry"
        '
        'MVentFilterTime
        '
        Me.MVentFilterTime.Location = New System.Drawing.Point(591, 215)
        Me.MVentFilterTime.Name = "MVentFilterTime"
        Me.MVentFilterTime.Size = New System.Drawing.Size(96, 20)
        Me.MVentFilterTime.TabIndex = 7
        '
        'Label38
        '
        Me.Label38.AutoSize = True
        Me.Label38.Location = New System.Drawing.Point(510, 218)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(75, 13)
        Me.Label38.TabIndex = 62
        Me.Label38.Text = "Begin Filter At:"
        Me.Label38.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFilterEfficiency
        '
        Me.MVentFilterEfficiency.Location = New System.Drawing.Point(591, 183)
        Me.MVentFilterEfficiency.Name = "MVentFilterEfficiency"
        Me.MVentFilterEfficiency.Size = New System.Drawing.Size(96, 20)
        Me.MVentFilterEfficiency.TabIndex = 6
        Me.MVentFilterEfficiency.Text = "1"
        '
        'Label54
        '
        Me.Label54.AutoSize = True
        Me.Label54.Location = New System.Drawing.Point(504, 186)
        Me.Label54.Name = "Label54"
        Me.Label54.Size = New System.Drawing.Size(81, 13)
        Me.Label54.TabIndex = 61
        Me.Label54.Text = "Filter Efficiency:"
        Me.Label54.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFractionTime
        '
        Me.MVentFractionTime.Location = New System.Drawing.Point(380, 200)
        Me.MVentFractionTime.Name = "MVentFractionTime"
        Me.MVentFractionTime.Size = New System.Drawing.Size(96, 20)
        Me.MVentFractionTime.TabIndex = 4
        '
        'Label99
        '
        Me.Label99.AutoSize = True
        Me.Label99.Location = New System.Drawing.Point(268, 204)
        Me.Label99.Name = "Label99"
        Me.Label99.Size = New System.Drawing.Size(101, 13)
        Me.Label99.TabIndex = 58
        Me.Label99.Text = "Change Fraction At:"
        Me.Label99.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFinalFraction
        '
        Me.MVentFinalFraction.Location = New System.Drawing.Point(380, 232)
        Me.MVentFinalFraction.Name = "MVentFinalFraction"
        Me.MVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.MVentFinalFraction.TabIndex = 5
        '
        'Label100
        '
        Me.Label100.Location = New System.Drawing.Point(244, 230)
        Me.Label100.Name = "Label100"
        Me.Label100.Size = New System.Drawing.Size(128, 24)
        Me.Label100.TabIndex = 56
        Me.Label100.Text = "Final Flow Fraction:"
        Me.Label100.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentInitialFraction
        '
        Me.MVentInitialFraction.Location = New System.Drawing.Point(380, 168)
        Me.MVentInitialFraction.Name = "MVentInitialFraction"
        Me.MVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.MVentInitialFraction.TabIndex = 3
        Me.MVentInitialFraction.Text = "1"
        '
        'Label101
        '
        Me.Label101.Location = New System.Drawing.Point(244, 166)
        Me.Label101.Name = "Label101"
        Me.Label101.Size = New System.Drawing.Size(128, 24)
        Me.Label101.TabIndex = 54
        Me.Label101.Text = "Initial Flow Fraction:"
        Me.Label101.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentZero
        '
        Me.MVentZero.Location = New System.Drawing.Point(129, 232)
        Me.MVentZero.Name = "MVentZero"
        Me.MVentZero.Size = New System.Drawing.Size(96, 20)
        Me.MVentZero.TabIndex = 2
        Me.MVentZero.Text = "300 Pa"
        '
        'Label41
        '
        Me.Label41.AutoSize = True
        Me.Label41.Location = New System.Drawing.Point(49, 234)
        Me.Label41.Name = "Label41"
        Me.Label41.Size = New System.Drawing.Size(70, 13)
        Me.Label41.TabIndex = 33
        Me.Label41.Text = "Zero Flow At:"
        Me.Label41.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox20
        '
        Me.GroupBox20.Controls.Add(Me.MVentToOrientation)
        Me.GroupBox20.Controls.Add(Me.Label43)
        Me.GroupBox20.Controls.Add(Me.MVentToHeight)
        Me.GroupBox20.Controls.Add(Me.Label48)
        Me.GroupBox20.Controls.Add(Me.MVentToArea)
        Me.GroupBox20.Controls.Add(Me.Label49)
        Me.GroupBox20.Controls.Add(Me.MventToComp)
        Me.GroupBox20.Location = New System.Drawing.Point(368, 24)
        Me.GroupBox20.Name = "GroupBox20"
        Me.GroupBox20.Size = New System.Drawing.Size(344, 112)
        Me.GroupBox20.TabIndex = 11
        Me.GroupBox20.TabStop = False
        Me.GroupBox20.Text = "To Compartment"
        '
        'MVentToOrientation
        '
        Me.MVentToOrientation.Items.AddRange(New Object() {"Vertical", "Horizontal"})
        Me.MVentToOrientation.Location = New System.Drawing.Point(160, 80)
        Me.MVentToOrientation.Name = "MVentToOrientation"
        Me.MVentToOrientation.Size = New System.Drawing.Size(96, 21)
        Me.MVentToOrientation.TabIndex = 3
        Me.MVentToOrientation.Text = "Vertical"
        '
        'Label43
        '
        Me.Label43.Location = New System.Drawing.Point(88, 80)
        Me.Label43.Name = "Label43"
        Me.Label43.Size = New System.Drawing.Size(64, 23)
        Me.Label43.TabIndex = 36
        Me.Label43.Text = "Orientation:"
        Me.Label43.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentToHeight
        '
        Me.MVentToHeight.Location = New System.Drawing.Point(240, 48)
        Me.MVentToHeight.Name = "MVentToHeight"
        Me.MVentToHeight.Size = New System.Drawing.Size(96, 20)
        Me.MVentToHeight.TabIndex = 2
        Me.MVentToHeight.Text = "1 m"
        '
        'Label48
        '
        Me.Label48.Location = New System.Drawing.Point(152, 48)
        Me.Label48.Name = "Label48"
        Me.Label48.Size = New System.Drawing.Size(80, 24)
        Me.Label48.TabIndex = 34
        Me.Label48.Text = "Center Height:"
        Me.Label48.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentToArea
        '
        Me.MVentToArea.Location = New System.Drawing.Point(56, 48)
        Me.MVentToArea.Name = "MVentToArea"
        Me.MVentToArea.Size = New System.Drawing.Size(96, 20)
        Me.MVentToArea.TabIndex = 1
        Me.MVentToArea.Text = "1 m ²"
        '
        'Label49
        '
        Me.Label49.Location = New System.Drawing.Point(8, 48)
        Me.Label49.Name = "Label49"
        Me.Label49.Size = New System.Drawing.Size(40, 23)
        Me.Label49.TabIndex = 32
        Me.Label49.Text = "Area:"
        Me.Label49.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MventToComp
        '
        Me.MventToComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.MventToComp.Location = New System.Drawing.Point(8, 16)
        Me.MventToComp.Name = "MventToComp"
        Me.MventToComp.Size = New System.Drawing.Size(328, 21)
        Me.MventToComp.TabIndex = 0
        '
        'MVentDropoff
        '
        Me.MVentDropoff.Location = New System.Drawing.Point(129, 200)
        Me.MVentDropoff.Name = "MVentDropoff"
        Me.MVentDropoff.Size = New System.Drawing.Size(96, 20)
        Me.MVentDropoff.TabIndex = 1
        Me.MVentDropoff.Text = "200 Pa"
        '
        'Label42
        '
        Me.Label42.AutoSize = True
        Me.Label42.Location = New System.Drawing.Point(33, 202)
        Me.Label42.Name = "Label42"
        Me.Label42.Size = New System.Drawing.Size(88, 13)
        Me.Label42.TabIndex = 28
        Me.Label42.Text = "Begin Dropoff At:"
        Me.Label42.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox21
        '
        Me.GroupBox21.Controls.Add(Me.MVentFromOrientation)
        Me.GroupBox21.Controls.Add(Me.Label44)
        Me.GroupBox21.Controls.Add(Me.MVentFromHeight)
        Me.GroupBox21.Controls.Add(Me.Label50)
        Me.GroupBox21.Controls.Add(Me.MVentFromArea)
        Me.GroupBox21.Controls.Add(Me.Label51)
        Me.GroupBox21.Controls.Add(Me.MVentFromComp)
        Me.GroupBox21.Location = New System.Drawing.Point(8, 24)
        Me.GroupBox21.Name = "GroupBox21"
        Me.GroupBox21.Size = New System.Drawing.Size(344, 112)
        Me.GroupBox21.TabIndex = 6
        Me.GroupBox21.TabStop = False
        Me.GroupBox21.Text = "From Compartment"
        '
        'MVentFromOrientation
        '
        Me.MVentFromOrientation.Items.AddRange(New Object() {"Vertical", "Horizontal"})
        Me.MVentFromOrientation.Location = New System.Drawing.Point(160, 80)
        Me.MVentFromOrientation.Name = "MVentFromOrientation"
        Me.MVentFromOrientation.Size = New System.Drawing.Size(96, 21)
        Me.MVentFromOrientation.TabIndex = 3
        Me.MVentFromOrientation.Text = "Vertical"
        '
        'Label44
        '
        Me.Label44.Location = New System.Drawing.Point(88, 80)
        Me.Label44.Name = "Label44"
        Me.Label44.Size = New System.Drawing.Size(64, 23)
        Me.Label44.TabIndex = 36
        Me.Label44.Text = "Orientation:"
        Me.Label44.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFromHeight
        '
        Me.MVentFromHeight.Location = New System.Drawing.Point(240, 48)
        Me.MVentFromHeight.Name = "MVentFromHeight"
        Me.MVentFromHeight.Size = New System.Drawing.Size(96, 20)
        Me.MVentFromHeight.TabIndex = 2
        Me.MVentFromHeight.Text = "1 m"
        '
        'Label50
        '
        Me.Label50.Location = New System.Drawing.Point(152, 48)
        Me.Label50.Name = "Label50"
        Me.Label50.Size = New System.Drawing.Size(80, 24)
        Me.Label50.TabIndex = 34
        Me.Label50.Text = "Center Height:"
        Me.Label50.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFromArea
        '
        Me.MVentFromArea.Location = New System.Drawing.Point(56, 48)
        Me.MVentFromArea.Name = "MVentFromArea"
        Me.MVentFromArea.Size = New System.Drawing.Size(96, 20)
        Me.MVentFromArea.TabIndex = 1
        Me.MVentFromArea.Text = "1 m²"
        '
        'Label51
        '
        Me.Label51.Location = New System.Drawing.Point(8, 48)
        Me.Label51.Name = "Label51"
        Me.Label51.Size = New System.Drawing.Size(40, 23)
        Me.Label51.TabIndex = 32
        Me.Label51.Text = "Area:"
        Me.Label51.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFromComp
        '
        Me.MVentFromComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.MVentFromComp.Location = New System.Drawing.Point(8, 16)
        Me.MVentFromComp.Name = "MVentFromComp"
        Me.MVentFromComp.Size = New System.Drawing.Size(328, 21)
        Me.MVentFromComp.TabIndex = 0
        '
        'MVentFlow
        '
        Me.MVentFlow.Location = New System.Drawing.Point(129, 168)
        Me.MVentFlow.Name = "MVentFlow"
        Me.MVentFlow.Size = New System.Drawing.Size(96, 20)
        Me.MVentFlow.TabIndex = 0
        Me.MVentFlow.Text = "1 m^3/s"
        '
        'Label45
        '
        Me.Label45.Location = New System.Drawing.Point(49, 166)
        Me.Label45.Name = "Label45"
        Me.Label45.Size = New System.Drawing.Size(72, 24)
        Me.Label45.TabIndex = 21
        Me.Label45.Text = "Flow Rate:"
        Me.Label45.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label47
        '
        Me.Label47.Location = New System.Drawing.Point(304, 112)
        Me.Label47.Name = "Label47"
        Me.Label47.Size = New System.Drawing.Size(40, 23)
        Me.Label47.TabIndex = 19
        Me.Label47.Text = "Soffit:"
        Me.Label47.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TabFires
        '
        Me.TabFires.Controls.Add(Me.FireAdd)
        Me.TabFires.Controls.Add(Me.FireFromFile)
        Me.TabFires.Controls.Add(Me.FireAddt2)
        Me.TabFires.Controls.Add(Me.FireIgnitionTemperature)
        Me.TabFires.Controls.Add(Me.Label17)
        Me.TabFires.Controls.Add(Me.FireSummary)
        Me.TabFires.Controls.Add(Me.FireRemove)
        Me.TabFires.Controls.Add(Me.FireDup)
        Me.TabFires.Controls.Add(Me.GroupFire)
        Me.TabFires.Controls.Add(Me.FireLOL)
        Me.TabFires.Controls.Add(Me.Label55)
        Me.TabFires.Location = New System.Drawing.Point(4, 22)
        Me.TabFires.Name = "TabFires"
        Me.TabFires.Size = New System.Drawing.Size(976, 592)
        Me.TabFires.TabIndex = 1
        Me.TabFires.Text = "Fires"
        Me.TabFires.Visible = False
        '
        'FireAdd
        '
        Me.FireAdd.Location = New System.Drawing.Point(81, 103)
        Me.FireAdd.Name = "FireAdd"
        Me.FireAdd.Size = New System.Drawing.Size(75, 23)
        Me.FireAdd.TabIndex = 3
        Me.FireAdd.Text = "Add New"
        '
        'FireFromFile
        '
        Me.FireFromFile.Location = New System.Drawing.Point(362, 103)
        Me.FireFromFile.Name = "FireFromFile"
        Me.FireFromFile.Size = New System.Drawing.Size(75, 23)
        Me.FireFromFile.TabIndex = 72
        Me.FireFromFile.Text = "From File"
        '
        'FireAddt2
        '
        Me.FireAddt2.Location = New System.Drawing.Point(165, 103)
        Me.FireAddt2.Name = "FireAddt2"
        Me.FireAddt2.Size = New System.Drawing.Size(75, 23)
        Me.FireAddt2.TabIndex = 2
        Me.FireAddt2.Text = "Add t²"
        '
        'FireIgnitionTemperature
        '
        Me.FireIgnitionTemperature.Location = New System.Drawing.Point(844, 71)
        Me.FireIgnitionTemperature.Name = "FireIgnitionTemperature"
        Me.FireIgnitionTemperature.Size = New System.Drawing.Size(80, 20)
        Me.FireIgnitionTemperature.TabIndex = 7
        Me.FireIgnitionTemperature.Text = "120 °C"
        '
        'Label17
        '
        Me.Label17.Location = New System.Drawing.Point(732, 65)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(107, 32)
        Me.Label17.TabIndex = 71
        Me.Label17.Text = "Gaseous Ignition Temperature:"
        Me.Label17.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireSummary
        '
        Me.FireSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.FireSummary.AllowEditing = False
        Me.FireSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.FireSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.FireSummary.ColumnInfo = resources.GetString("FireSummary.ColumnInfo")
        Me.FireSummary.ExtendLastCol = True
        Me.FireSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.FireSummary.Location = New System.Drawing.Point(16, 16)
        Me.FireSummary.Name = "FireSummary"
        Me.FireSummary.Rows.Count = 101
        Me.FireSummary.Rows.DefaultSize = 19
        Me.FireSummary.Size = New System.Drawing.Size(682, 81)
        Me.FireSummary.StyleInfo = resources.GetString("FireSummary.StyleInfo")
        Me.FireSummary.TabIndex = 1
        Me.FireSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'FireRemove
        '
        Me.FireRemove.Location = New System.Drawing.Point(538, 103)
        Me.FireRemove.Name = "FireRemove"
        Me.FireRemove.Size = New System.Drawing.Size(75, 23)
        Me.FireRemove.TabIndex = 4
        Me.FireRemove.Text = "Remove"
        '
        'FireDup
        '
        Me.FireDup.Location = New System.Drawing.Point(266, 103)
        Me.FireDup.Name = "FireDup"
        Me.FireDup.Size = New System.Drawing.Size(75, 23)
        Me.FireDup.TabIndex = 3
        Me.FireDup.Text = "Duplicate"
        '
        'GroupFire
        '
        Me.GroupFire.Controls.Add(Me.FirePlot)
        Me.GroupFire.Controls.Add(Me.FireCOYield)
        Me.GroupFire.Controls.Add(Me.FireHoC)
        Me.GroupFire.Controls.Add(Me.FireMaterial)
        Me.GroupFire.Controls.Add(Me.Label107)
        Me.GroupFire.Controls.Add(Me.Label71)
        Me.GroupFire.Controls.Add(Me.Label108)
        Me.GroupFire.Controls.Add(Me.FireCl)
        Me.GroupFire.Controls.Add(Me.FireName)
        Me.GroupFire.Controls.Add(Me.FireSootYield)
        Me.GroupFire.Controls.Add(Me.Label106)
        Me.GroupFire.Controls.Add(Me.Label114)
        Me.GroupFire.Controls.Add(Me.FireDataSS)
        Me.GroupFire.Controls.Add(Me.Label105)
        Me.GroupFire.Controls.Add(Me.FireType)
        Me.GroupFire.Controls.Add(Me.Label66)
        Me.GroupFire.Controls.Add(Me.Label18)
        Me.GroupFire.Controls.Add(Me.FireN)
        Me.GroupFire.Controls.Add(Me.Label95)
        Me.GroupFire.Controls.Add(Me.Label60)
        Me.GroupFire.Controls.Add(Me.FireH)
        Me.GroupFire.Controls.Add(Me.Label113)
        Me.GroupFire.Controls.Add(Me.Label109)
        Me.GroupFire.Controls.Add(Me.FireYPosition)
        Me.GroupFire.Controls.Add(Me.Label69)
        Me.GroupFire.Controls.Add(Me.FireC)
        Me.GroupFire.Controls.Add(Me.FireRadiativeFraction)
        Me.GroupFire.Controls.Add(Me.FireXPosition)
        Me.GroupFire.Controls.Add(Me.FireO)
        Me.GroupFire.Controls.Add(Me.Label70)
        Me.GroupFire.Controls.Add(Me.Label110)
        Me.GroupFire.Controls.Add(Me.FireZPosition)
        Me.GroupFire.Controls.Add(Me.FireComp)
        Me.GroupFire.Controls.Add(Me.Label111)
        Me.GroupFire.Controls.Add(Me.Label52)
        Me.GroupFire.Controls.Add(Me.Label112)
        Me.GroupFire.Controls.Add(Me.FireIgnitionCriteria)
        Me.GroupFire.Controls.Add(Me.FireIgnitionValue)
        Me.GroupFire.Controls.Add(Me.Label63)
        Me.GroupFire.Controls.Add(Me.Label58)
        Me.GroupFire.Controls.Add(Me.FireXNormal)
        Me.GroupFire.Controls.Add(Me.FireYNormal)
        Me.GroupFire.Controls.Add(Me.FireZNormal)
        Me.GroupFire.Controls.Add(Me.Label68)
        Me.GroupFire.Location = New System.Drawing.Point(3, 132)
        Me.GroupFire.Name = "GroupFire"
        Me.GroupFire.Size = New System.Drawing.Size(970, 457)
        Me.GroupFire.TabIndex = 0
        Me.GroupFire.TabStop = False
        Me.GroupFire.Text = "Fire 1"
        '
        'FirePlot
        '
        Me.FirePlot.AutoScaleAutoGeneratedAxes = True
        Me.FirePlot.AutoScaleTitle = False
        Me.FirePlot.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.FirePlot.DateTimeToolTip = False
        Me.FirePlot.Legend = Nothing
        Me.FirePlot.LegendZOrder = -1
        Me.FirePlot.Location = New System.Drawing.Point(618, 183)
        Me.FirePlot.Name = "FirePlot"
        Me.FirePlot.RightMenu = Nothing
        Me.FirePlot.ShowCoordinates = True
        Me.FirePlot.Size = New System.Drawing.Size(338, 250)
        Me.FirePlot.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None
        Me.FirePlot.TabIndex = 157
        Me.FirePlot.TabStop = False
        Me.FirePlot.Title = ""
        Me.FirePlot.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.FirePlot.XAxis1 = Nothing
        Me.FirePlot.XAxis2 = Nothing
        Me.FirePlot.YAxis1 = Nothing
        Me.FirePlot.YAxis2 = Nothing
        '
        'FireCOYield
        '
        Me.FireCOYield.Location = New System.Drawing.Point(206, 118)
        Me.FireCOYield.Name = "FireCOYield"
        Me.FireCOYield.Size = New System.Drawing.Size(80, 20)
        Me.FireCOYield.TabIndex = 139
        '
        'FireHoC
        '
        Me.FireHoC.Location = New System.Drawing.Point(206, 55)
        Me.FireHoC.Name = "FireHoC"
        Me.FireHoC.Size = New System.Drawing.Size(80, 20)
        Me.FireHoC.TabIndex = 13
        Me.FireHoC.Text = "50000000 J/kg"
        '
        'FireMaterial
        '
        Me.FireMaterial.ItemHeight = 13
        Me.FireMaterial.Location = New System.Drawing.Point(396, 147)
        Me.FireMaterial.Name = "FireMaterial"
        Me.FireMaterial.Size = New System.Drawing.Size(98, 21)
        Me.FireMaterial.TabIndex = 7
        Me.FireMaterial.Text = "GYPSUM"
        '
        'Label107
        '
        Me.Label107.AutoSize = True
        Me.Label107.Location = New System.Drawing.Point(340, 151)
        Me.Label107.Name = "Label107"
        Me.Label107.Size = New System.Drawing.Size(47, 13)
        Me.Label107.TabIndex = 134
        Me.Label107.Text = "Material:"
        Me.Label107.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label71
        '
        Me.Label71.AutoSize = True
        Me.Label71.Location = New System.Drawing.Point(147, 122)
        Me.Label71.Name = "Label71"
        Me.Label71.Size = New System.Drawing.Size(51, 13)
        Me.Label71.TabIndex = 140
        Me.Label71.Text = "CO Yield:"
        Me.Label71.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label108
        '
        Me.Label108.Location = New System.Drawing.Point(126, 51)
        Me.Label108.Name = "Label108"
        Me.Label108.Size = New System.Drawing.Size(72, 28)
        Me.Label108.TabIndex = 116
        Me.Label108.Text = "Heat of Combustion:"
        Me.Label108.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireCl
        '
        Me.FireCl.Location = New System.Drawing.Point(42, 179)
        Me.FireCl.Name = "FireCl"
        Me.FireCl.Size = New System.Drawing.Size(80, 20)
        Me.FireCl.TabIndex = 135
        '
        'FireName
        '
        Me.FireName.Location = New System.Drawing.Point(242, 16)
        Me.FireName.Name = "FireName"
        Me.FireName.Size = New System.Drawing.Size(98, 20)
        Me.FireName.TabIndex = 155
        Me.FireName.Text = "New Fire"
        '
        'FireSootYield
        '
        Me.FireSootYield.Location = New System.Drawing.Point(206, 87)
        Me.FireSootYield.Name = "FireSootYield"
        Me.FireSootYield.Size = New System.Drawing.Size(80, 20)
        Me.FireSootYield.TabIndex = 137
        '
        'Label106
        '
        Me.Label106.AutoSize = True
        Me.Label106.Location = New System.Drawing.Point(10, 183)
        Me.Label106.Name = "Label106"
        Me.Label106.Size = New System.Drawing.Size(19, 13)
        Me.Label106.TabIndex = 136
        Me.Label106.Text = "Cl:"
        Me.Label106.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label114
        '
        Me.Label114.AutoSize = True
        Me.Label114.Location = New System.Drawing.Point(198, 20)
        Me.Label114.Name = "Label114"
        Me.Label114.Size = New System.Drawing.Size(38, 13)
        Me.Label114.TabIndex = 156
        Me.Label114.Text = "Name:"
        Me.Label114.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireDataSS
        '
        Me.FireDataSS.AllowDelete = True
        Me.FireDataSS.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.FireDataSS.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.FireDataSS.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.FireDataSS.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom), System.Windows.Forms.AnchorStyles)
        Me.FireDataSS.AutoClipboard = True
        Me.FireDataSS.ColumnInfo = resources.GetString("FireDataSS.ColumnInfo")
        Me.FireDataSS.ExtendLastCol = True
        Me.FireDataSS.Location = New System.Drawing.Point(15, 214)
        Me.FireDataSS.Name = "FireDataSS"
        Me.FireDataSS.Rows.Count = 101
        Me.FireDataSS.Rows.DefaultSize = 17
        Me.FireDataSS.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireDataSS.Size = New System.Drawing.Size(597, 191)
        Me.FireDataSS.TabIndex = 152
        '
        'Label105
        '
        Me.Label105.AutoSize = True
        Me.Label105.Location = New System.Drawing.Point(140, 91)
        Me.Label105.Name = "Label105"
        Me.Label105.Size = New System.Drawing.Size(58, 13)
        Me.Label105.TabIndex = 138
        Me.Label105.Text = "Soot Yield:"
        Me.Label105.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireType
        '
        Me.FireType.AutoSize = True
        Me.FireType.Location = New System.Drawing.Point(393, 59)
        Me.FireType.Name = "FireType"
        Me.FireType.Size = New System.Drawing.Size(102, 13)
        Me.FireType.TabIndex = 121
        Me.FireType.Text = "Constrained, Normal"
        Me.FireType.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label66
        '
        Me.Label66.AutoSize = True
        Me.Label66.Location = New System.Drawing.Point(680, 122)
        Me.Label66.Name = "Label66"
        Me.Label66.Size = New System.Drawing.Size(56, 13)
        Me.Label66.TabIndex = 120
        Me.Label66.Text = "Normal, Z:"
        Me.Label66.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(679, 91)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(56, 13)
        Me.Label18.TabIndex = 119
        Me.Label18.Text = "Normal, Y:"
        Me.Label18.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireN
        '
        Me.FireN.Location = New System.Drawing.Point(42, 147)
        Me.FireN.Name = "FireN"
        Me.FireN.Size = New System.Drawing.Size(80, 20)
        Me.FireN.TabIndex = 12
        '
        'Label95
        '
        Me.Label95.AutoSize = True
        Me.Label95.Location = New System.Drawing.Point(322, 59)
        Me.Label95.Name = "Label95"
        Me.Label95.Size = New System.Drawing.Size(66, 13)
        Me.Label95.TabIndex = 117
        Me.Label95.Text = "Plume Type:"
        Me.Label95.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label60
        '
        Me.Label60.AutoSize = True
        Me.Label60.Location = New System.Drawing.Point(519, 122)
        Me.Label60.Name = "Label60"
        Me.Label60.Size = New System.Drawing.Size(57, 13)
        Me.Label60.TabIndex = 69
        Me.Label60.Text = "Position Z:"
        Me.Label60.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireH
        '
        Me.FireH.Location = New System.Drawing.Point(42, 87)
        Me.FireH.Name = "FireH"
        Me.FireH.Size = New System.Drawing.Size(80, 20)
        Me.FireH.TabIndex = 9
        '
        'Label113
        '
        Me.Label113.Location = New System.Drawing.Point(142, 142)
        Me.Label113.Name = "Label113"
        Me.Label113.Size = New System.Drawing.Size(56, 31)
        Me.Label113.TabIndex = 124
        Me.Label113.Text = "Radiative Fraction:"
        Me.Label113.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label109
        '
        Me.Label109.AutoSize = True
        Me.Label109.Location = New System.Drawing.Point(12, 59)
        Me.Label109.Name = "Label109"
        Me.Label109.Size = New System.Drawing.Size(17, 13)
        Me.Label109.TabIndex = 118
        Me.Label109.Text = "C:"
        Me.Label109.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireYPosition
        '
        Me.FireYPosition.Location = New System.Drawing.Point(585, 87)
        Me.FireYPosition.Name = "FireYPosition"
        Me.FireYPosition.Size = New System.Drawing.Size(80, 20)
        Me.FireYPosition.TabIndex = 2
        Me.FireYPosition.Text = "Center"
        '
        'Label69
        '
        Me.Label69.AutoSize = True
        Me.Label69.Location = New System.Drawing.Point(521, 91)
        Me.Label69.Name = "Label69"
        Me.Label69.Size = New System.Drawing.Size(57, 13)
        Me.Label69.TabIndex = 67
        Me.Label69.Text = "Position Y:"
        Me.Label69.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireC
        '
        Me.FireC.Location = New System.Drawing.Point(42, 55)
        Me.FireC.Name = "FireC"
        Me.FireC.Size = New System.Drawing.Size(80, 20)
        Me.FireC.TabIndex = 8
        '
        'FireRadiativeFraction
        '
        Me.FireRadiativeFraction.Location = New System.Drawing.Point(206, 147)
        Me.FireRadiativeFraction.Name = "FireRadiativeFraction"
        Me.FireRadiativeFraction.Size = New System.Drawing.Size(80, 20)
        Me.FireRadiativeFraction.TabIndex = 17
        Me.FireRadiativeFraction.Text = "0.3"
        '
        'FireXPosition
        '
        Me.FireXPosition.Location = New System.Drawing.Point(586, 55)
        Me.FireXPosition.Name = "FireXPosition"
        Me.FireXPosition.Size = New System.Drawing.Size(80, 20)
        Me.FireXPosition.TabIndex = 1
        Me.FireXPosition.Text = "Center"
        '
        'FireO
        '
        Me.FireO.Location = New System.Drawing.Point(42, 118)
        Me.FireO.Name = "FireO"
        Me.FireO.Size = New System.Drawing.Size(80, 20)
        Me.FireO.TabIndex = 10
        '
        'Label70
        '
        Me.Label70.AutoSize = True
        Me.Label70.Location = New System.Drawing.Point(519, 59)
        Me.Label70.Name = "Label70"
        Me.Label70.Size = New System.Drawing.Size(60, 13)
        Me.Label70.TabIndex = 65
        Me.Label70.Text = "Position, X:"
        Me.Label70.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label110
        '
        Me.Label110.AutoSize = True
        Me.Label110.Location = New System.Drawing.Point(11, 122)
        Me.Label110.Name = "Label110"
        Me.Label110.Size = New System.Drawing.Size(18, 13)
        Me.Label110.TabIndex = 122
        Me.Label110.Text = "O:"
        Me.Label110.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireZPosition
        '
        Me.FireZPosition.Location = New System.Drawing.Point(586, 118)
        Me.FireZPosition.Name = "FireZPosition"
        Me.FireZPosition.Size = New System.Drawing.Size(80, 20)
        Me.FireZPosition.TabIndex = 3
        Me.FireZPosition.Text = "0"
        '
        'FireComp
        '
        Me.FireComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.FireComp.ItemHeight = 13
        Me.FireComp.Location = New System.Drawing.Point(546, 16)
        Me.FireComp.Name = "FireComp"
        Me.FireComp.Size = New System.Drawing.Size(208, 21)
        Me.FireComp.TabIndex = 0
        '
        'Label111
        '
        Me.Label111.AutoSize = True
        Me.Label111.Location = New System.Drawing.Point(11, 91)
        Me.Label111.Name = "Label111"
        Me.Label111.Size = New System.Drawing.Size(18, 13)
        Me.Label111.TabIndex = 119
        Me.Label111.Text = "H:"
        Me.Label111.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label52
        '
        Me.Label52.AutoSize = True
        Me.Label52.Location = New System.Drawing.Point(466, 20)
        Me.Label52.Name = "Label52"
        Me.Label52.Size = New System.Drawing.Size(72, 13)
        Me.Label52.TabIndex = 3
        Me.Label52.Text = "Compartment:"
        Me.Label52.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label112
        '
        Me.Label112.AutoSize = True
        Me.Label112.Location = New System.Drawing.Point(11, 151)
        Me.Label112.Name = "Label112"
        Me.Label112.Size = New System.Drawing.Size(18, 13)
        Me.Label112.TabIndex = 116
        Me.Label112.Text = "N:"
        Me.Label112.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireIgnitionCriteria
        '
        Me.FireIgnitionCriteria.ItemHeight = 13
        Me.FireIgnitionCriteria.Items.AddRange(New Object() {"Time", "Temperature", "Heat Flux"})
        Me.FireIgnitionCriteria.Location = New System.Drawing.Point(396, 87)
        Me.FireIgnitionCriteria.Name = "FireIgnitionCriteria"
        Me.FireIgnitionCriteria.Size = New System.Drawing.Size(80, 21)
        Me.FireIgnitionCriteria.TabIndex = 8
        Me.FireIgnitionCriteria.Text = "Time"
        '
        'FireIgnitionValue
        '
        Me.FireIgnitionValue.Location = New System.Drawing.Point(396, 118)
        Me.FireIgnitionValue.Name = "FireIgnitionValue"
        Me.FireIgnitionValue.Size = New System.Drawing.Size(80, 20)
        Me.FireIgnitionValue.TabIndex = 9
        Me.FireIgnitionValue.Text = "0 s"
        '
        'Label63
        '
        Me.Label63.AutoSize = True
        Me.Label63.Location = New System.Drawing.Point(316, 122)
        Me.Label63.Name = "Label63"
        Me.Label63.Size = New System.Drawing.Size(74, 13)
        Me.Label63.TabIndex = 82
        Me.Label63.Text = "Ignition Value:"
        Me.Label63.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label58
        '
        Me.Label58.AutoSize = True
        Me.Label58.Location = New System.Drawing.Point(305, 91)
        Me.Label58.Name = "Label58"
        Me.Label58.Size = New System.Drawing.Size(85, 13)
        Me.Label58.TabIndex = 79
        Me.Label58.Text = "Ignition Criterion:"
        Me.Label58.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireXNormal
        '
        Me.FireXNormal.Location = New System.Drawing.Point(743, 55)
        Me.FireXNormal.Name = "FireXNormal"
        Me.FireXNormal.Size = New System.Drawing.Size(80, 20)
        Me.FireXNormal.TabIndex = 4
        Me.FireXNormal.Text = "0"
        '
        'FireYNormal
        '
        Me.FireYNormal.Location = New System.Drawing.Point(743, 87)
        Me.FireYNormal.Name = "FireYNormal"
        Me.FireYNormal.Size = New System.Drawing.Size(80, 20)
        Me.FireYNormal.TabIndex = 5
        Me.FireYNormal.Text = "0"
        '
        'FireZNormal
        '
        Me.FireZNormal.Location = New System.Drawing.Point(743, 118)
        Me.FireZNormal.Name = "FireZNormal"
        Me.FireZNormal.Size = New System.Drawing.Size(80, 20)
        Me.FireZNormal.TabIndex = 6
        Me.FireZNormal.Text = "1"
        '
        'Label68
        '
        Me.Label68.AutoSize = True
        Me.Label68.Location = New System.Drawing.Point(679, 59)
        Me.Label68.Name = "Label68"
        Me.Label68.Size = New System.Drawing.Size(56, 13)
        Me.Label68.TabIndex = 84
        Me.Label68.Text = "Normal, X:"
        Me.Label68.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireLOL
        '
        Me.FireLOL.Location = New System.Drawing.Point(844, 39)
        Me.FireLOL.Name = "FireLOL"
        Me.FireLOL.Size = New System.Drawing.Size(80, 20)
        Me.FireLOL.TabIndex = 6
        Me.FireLOL.Text = "10 %"
        '
        'Label55
        '
        Me.Label55.AutoSize = True
        Me.Label55.Location = New System.Drawing.Point(732, 39)
        Me.Label55.Name = "Label55"
        Me.Label55.Size = New System.Drawing.Size(102, 13)
        Me.Label55.TabIndex = 59
        Me.Label55.Text = "Lower Oxygen Limit:"
        Me.Label55.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TabDetection
        '
        Me.TabDetection.Controls.Add(Me.DetectorSummary)
        Me.TabDetection.Controls.Add(Me.GroupDetectors)
        Me.TabDetection.Controls.Add(Me.DetectorRemove)
        Me.TabDetection.Controls.Add(Me.DetectorAdd)
        Me.TabDetection.Controls.Add(Me.DetectorMoveDown)
        Me.TabDetection.Controls.Add(Me.DetectorMoveUp)
        Me.TabDetection.Controls.Add(Me.DetectorDup)
        Me.TabDetection.Location = New System.Drawing.Point(4, 22)
        Me.TabDetection.Name = "TabDetection"
        Me.TabDetection.Size = New System.Drawing.Size(976, 592)
        Me.TabDetection.TabIndex = 2
        Me.TabDetection.Text = "Detection / Suppression"
        Me.TabDetection.Visible = False
        '
        'DetectorSummary
        '
        Me.DetectorSummary.AllowEditing = False
        Me.DetectorSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.DetectorSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.DetectorSummary.AutoGenerateColumns = False
        Me.DetectorSummary.ColumnInfo = resources.GetString("DetectorSummary.ColumnInfo")
        Me.DetectorSummary.ExtendLastCol = True
        Me.DetectorSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.DetectorSummary.Location = New System.Drawing.Point(117, 53)
        Me.DetectorSummary.Name = "DetectorSummary"
        Me.DetectorSummary.Rows.Count = 101
        Me.DetectorSummary.Rows.DefaultSize = 19
        Me.DetectorSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.DetectorSummary.Size = New System.Drawing.Size(742, 136)
        Me.DetectorSummary.StyleInfo = resources.GetString("DetectorSummary.StyleInfo")
        Me.DetectorSummary.TabIndex = 0
        Me.DetectorSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupDetectors
        '
        Me.GroupDetectors.Controls.Add(Me.DetectorSprayDensity)
        Me.GroupDetectors.Controls.Add(Me.Label81)
        Me.GroupDetectors.Controls.Add(Me.DetectorRTI)
        Me.GroupDetectors.Controls.Add(Me.Label83)
        Me.GroupDetectors.Controls.Add(Me.DetectorActivation)
        Me.GroupDetectors.Controls.Add(Me.Label92)
        Me.GroupDetectors.Controls.Add(Me.DetectorType)
        Me.GroupDetectors.Controls.Add(Me.Label91)
        Me.GroupDetectors.Controls.Add(Me.DetectorComp)
        Me.GroupDetectors.Controls.Add(Me.Label87)
        Me.GroupDetectors.Controls.Add(Me.GroupBox33)
        Me.GroupDetectors.Location = New System.Drawing.Point(79, 275)
        Me.GroupDetectors.Name = "GroupDetectors"
        Me.GroupDetectors.Size = New System.Drawing.Size(819, 226)
        Me.GroupDetectors.TabIndex = 7
        Me.GroupDetectors.TabStop = False
        Me.GroupDetectors.Text = "Smoke Alarm 1"
        '
        'DetectorSprayDensity
        '
        Me.DetectorSprayDensity.Location = New System.Drawing.Point(593, 136)
        Me.DetectorSprayDensity.Name = "DetectorSprayDensity"
        Me.DetectorSprayDensity.Size = New System.Drawing.Size(96, 20)
        Me.DetectorSprayDensity.TabIndex = 4
        Me.DetectorSprayDensity.Text = "7.0 E-5 m/s"
        '
        'Label81
        '
        Me.Label81.AutoSize = True
        Me.Label81.Location = New System.Drawing.Point(505, 136)
        Me.Label81.Name = "Label81"
        Me.Label81.Size = New System.Drawing.Size(75, 13)
        Me.Label81.TabIndex = 48
        Me.Label81.Text = "Spray Density:"
        Me.Label81.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorRTI
        '
        Me.DetectorRTI.Location = New System.Drawing.Point(593, 104)
        Me.DetectorRTI.Name = "DetectorRTI"
        Me.DetectorRTI.Size = New System.Drawing.Size(96, 20)
        Me.DetectorRTI.TabIndex = 3
        Me.DetectorRTI.Text = "5 m^1/2 s^1/2"
        '
        'Label83
        '
        Me.Label83.AutoSize = True
        Me.Label83.Location = New System.Drawing.Point(553, 104)
        Me.Label83.Name = "Label83"
        Me.Label83.Size = New System.Drawing.Size(28, 13)
        Me.Label83.TabIndex = 46
        Me.Label83.Text = "RTI:"
        Me.Label83.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorActivation
        '
        Me.DetectorActivation.Location = New System.Drawing.Point(697, 24)
        Me.DetectorActivation.Name = "DetectorActivation"
        Me.DetectorActivation.Size = New System.Drawing.Size(96, 20)
        Me.DetectorActivation.TabIndex = 2
        Me.DetectorActivation.Text = "30"
        '
        'Label92
        '
        Me.Label92.AutoSize = True
        Me.Label92.Location = New System.Drawing.Point(569, 24)
        Me.Label92.Name = "Label92"
        Me.Label92.Size = New System.Drawing.Size(120, 13)
        Me.Label92.TabIndex = 44
        Me.Label92.Text = "Activation Temperature:"
        Me.Label92.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorType
        '
        Me.DetectorType.ItemHeight = 13
        Me.DetectorType.Items.AddRange(New Object() {"Smoke Alarm", "Heat Alarm", "Sprinkler"})
        Me.DetectorType.Location = New System.Drawing.Point(65, 24)
        Me.DetectorType.Name = "DetectorType"
        Me.DetectorType.Size = New System.Drawing.Size(112, 21)
        Me.DetectorType.TabIndex = 0
        Me.DetectorType.Text = "Smoke Alarm"
        '
        'Label91
        '
        Me.Label91.AutoSize = True
        Me.Label91.Location = New System.Drawing.Point(25, 24)
        Me.Label91.Name = "Label91"
        Me.Label91.Size = New System.Drawing.Size(34, 13)
        Me.Label91.TabIndex = 42
        Me.Label91.Text = "Type:"
        Me.Label91.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorComp
        '
        Me.DetectorComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.DetectorComp.ItemHeight = 13
        Me.DetectorComp.Location = New System.Drawing.Point(321, 24)
        Me.DetectorComp.Name = "DetectorComp"
        Me.DetectorComp.Size = New System.Drawing.Size(208, 21)
        Me.DetectorComp.TabIndex = 1
        '
        'Label87
        '
        Me.Label87.AutoSize = True
        Me.Label87.Location = New System.Drawing.Point(233, 24)
        Me.Label87.Name = "Label87"
        Me.Label87.Size = New System.Drawing.Size(72, 13)
        Me.Label87.TabIndex = 34
        Me.Label87.Text = "Compartment:"
        Me.Label87.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox33
        '
        Me.GroupBox33.Controls.Add(Me.DetectorZPosition)
        Me.GroupBox33.Controls.Add(Me.Label88)
        Me.GroupBox33.Controls.Add(Me.DetectorYPosition)
        Me.GroupBox33.Controls.Add(Me.Label89)
        Me.GroupBox33.Controls.Add(Me.DetectorXPosition)
        Me.GroupBox33.Controls.Add(Me.Label90)
        Me.GroupBox33.Location = New System.Drawing.Point(153, 72)
        Me.GroupBox33.Name = "GroupBox33"
        Me.GroupBox33.Size = New System.Drawing.Size(200, 136)
        Me.GroupBox33.TabIndex = 11
        Me.GroupBox33.TabStop = False
        Me.GroupBox33.Text = "Position"
        '
        'DetectorZPosition
        '
        Me.DetectorZPosition.Location = New System.Drawing.Point(80, 96)
        Me.DetectorZPosition.Name = "DetectorZPosition"
        Me.DetectorZPosition.Size = New System.Drawing.Size(96, 20)
        Me.DetectorZPosition.TabIndex = 2
        '
        'Label88
        '
        Me.Label88.AutoSize = True
        Me.Label88.Location = New System.Drawing.Point(16, 96)
        Me.Label88.Name = "Label88"
        Me.Label88.Size = New System.Drawing.Size(57, 13)
        Me.Label88.TabIndex = 31
        Me.Label88.Text = "Height (Z):"
        Me.Label88.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorYPosition
        '
        Me.DetectorYPosition.Location = New System.Drawing.Point(80, 64)
        Me.DetectorYPosition.Name = "DetectorYPosition"
        Me.DetectorYPosition.Size = New System.Drawing.Size(96, 20)
        Me.DetectorYPosition.TabIndex = 1
        '
        'Label89
        '
        Me.Label89.AutoSize = True
        Me.Label89.Location = New System.Drawing.Point(16, 64)
        Me.Label89.Name = "Label89"
        Me.Label89.Size = New System.Drawing.Size(55, 13)
        Me.Label89.TabIndex = 29
        Me.Label89.Text = "Depth (Y):"
        Me.Label89.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorXPosition
        '
        Me.DetectorXPosition.Location = New System.Drawing.Point(80, 32)
        Me.DetectorXPosition.Name = "DetectorXPosition"
        Me.DetectorXPosition.Size = New System.Drawing.Size(96, 20)
        Me.DetectorXPosition.TabIndex = 0
        '
        'Label90
        '
        Me.Label90.AutoSize = True
        Me.Label90.Location = New System.Drawing.Point(16, 32)
        Me.Label90.Name = "Label90"
        Me.Label90.Size = New System.Drawing.Size(54, 13)
        Me.Label90.TabIndex = 27
        Me.Label90.Text = "Width (X):"
        Me.Label90.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorRemove
        '
        Me.DetectorRemove.Location = New System.Drawing.Point(675, 219)
        Me.DetectorRemove.Name = "DetectorRemove"
        Me.DetectorRemove.Size = New System.Drawing.Size(75, 23)
        Me.DetectorRemove.TabIndex = 5
        Me.DetectorRemove.Text = "Remove"
        '
        'DetectorAdd
        '
        Me.DetectorAdd.Location = New System.Drawing.Point(227, 219)
        Me.DetectorAdd.Name = "DetectorAdd"
        Me.DetectorAdd.Size = New System.Drawing.Size(75, 23)
        Me.DetectorAdd.TabIndex = 1
        Me.DetectorAdd.Text = "Add"
        '
        'DetectorMoveDown
        '
        Me.DetectorMoveDown.Location = New System.Drawing.Point(515, 219)
        Me.DetectorMoveDown.Name = "DetectorMoveDown"
        Me.DetectorMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.DetectorMoveDown.TabIndex = 4
        Me.DetectorMoveDown.Text = "Move Down"
        '
        'DetectorMoveUp
        '
        Me.DetectorMoveUp.Location = New System.Drawing.Point(419, 219)
        Me.DetectorMoveUp.Name = "DetectorMoveUp"
        Me.DetectorMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.DetectorMoveUp.TabIndex = 3
        Me.DetectorMoveUp.Text = "Move Up"
        '
        'DetectorDup
        '
        Me.DetectorDup.Location = New System.Drawing.Point(323, 219)
        Me.DetectorDup.Name = "DetectorDup"
        Me.DetectorDup.Size = New System.Drawing.Size(75, 23)
        Me.DetectorDup.TabIndex = 2
        Me.DetectorDup.Text = "Duplicate"
        '
        'TabTargets
        '
        Me.TabTargets.Controls.Add(Me.TargetSummary)
        Me.TabTargets.Controls.Add(Me.GroupTargets)
        Me.TabTargets.Controls.Add(Me.TargetRemove)
        Me.TabTargets.Controls.Add(Me.TargetMoveDown)
        Me.TabTargets.Controls.Add(Me.TargetAdd)
        Me.TabTargets.Controls.Add(Me.TargetMoveUp)
        Me.TabTargets.Controls.Add(Me.TargetDup)
        Me.TabTargets.Location = New System.Drawing.Point(4, 22)
        Me.TabTargets.Name = "TabTargets"
        Me.TabTargets.Size = New System.Drawing.Size(976, 592)
        Me.TabTargets.TabIndex = 3
        Me.TabTargets.Text = "Targets"
        Me.TabTargets.Visible = False
        '
        'TargetSummary
        '
        Me.TargetSummary.ColumnInfo = resources.GetString("TargetSummary.ColumnInfo")
        Me.TargetSummary.ExtendLastCol = True
        Me.TargetSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.TargetSummary.Location = New System.Drawing.Point(127, 55)
        Me.TargetSummary.Name = "TargetSummary"
        Me.TargetSummary.Rows.Count = 101
        Me.TargetSummary.Rows.DefaultSize = 19
        Me.TargetSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.TargetSummary.Size = New System.Drawing.Size(722, 136)
        Me.TargetSummary.StyleInfo = resources.GetString("TargetSummary.StyleInfo")
        Me.TargetSummary.TabIndex = 0
        Me.TargetSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupTargets
        '
        Me.GroupTargets.Controls.Add(Me.GroupBox3)
        Me.GroupTargets.Controls.Add(Me.Label80)
        Me.GroupTargets.Controls.Add(Me.Label79)
        Me.GroupTargets.Controls.Add(Me.TargetSolutionMethod)
        Me.GroupTargets.Controls.Add(Me.TargetSolutionThickness)
        Me.GroupTargets.Controls.Add(Me.TargetComp)
        Me.GroupTargets.Controls.Add(Me.Label74)
        Me.GroupTargets.Controls.Add(Me.GroupBox28)
        Me.GroupTargets.Location = New System.Drawing.Point(61, 275)
        Me.GroupTargets.Name = "GroupTargets"
        Me.GroupTargets.Size = New System.Drawing.Size(855, 270)
        Me.GroupTargets.TabIndex = 7
        Me.GroupTargets.TabStop = False
        Me.GroupTargets.Text = "Target 1"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.TargetInternalLocation)
        Me.GroupBox3.Controls.Add(Me.Label59)
        Me.GroupBox3.Controls.Add(Me.TargetSpecHeat)
        Me.GroupBox3.Controls.Add(Me.TargetDensity)
        Me.GroupBox3.Controls.Add(Me.TargetThickness)
        Me.GroupBox3.Controls.Add(Me.TargetConduct)
        Me.GroupBox3.Controls.Add(Me.TargetMaterial)
        Me.GroupBox3.Controls.Add(Me.Label78)
        Me.GroupBox3.Location = New System.Drawing.Point(521, 75)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(267, 175)
        Me.GroupBox3.TabIndex = 56
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Target Construction"
        '
        'TargetInternalLocation
        '
        Me.TargetInternalLocation.Location = New System.Drawing.Point(133, 140)
        Me.TargetInternalLocation.Name = "TargetInternalLocation"
        Me.TargetInternalLocation.Size = New System.Drawing.Size(96, 20)
        Me.TargetInternalLocation.TabIndex = 1
        Me.TargetInternalLocation.Text = "0.5"
        '
        'Label59
        '
        Me.Label59.AutoSize = True
        Me.Label59.Location = New System.Drawing.Point(7, 144)
        Me.Label59.Name = "Label59"
        Me.Label59.Size = New System.Drawing.Size(120, 13)
        Me.Label59.TabIndex = 40
        Me.Label59.Text = "Internal Temperature at:"
        '
        'TargetSpecHeat
        '
        Me.TargetSpecHeat.AutoSize = True
        Me.TargetSpecHeat.Location = New System.Drawing.Point(7, 75)
        Me.TargetSpecHeat.Name = "TargetSpecHeat"
        Me.TargetSpecHeat.Size = New System.Drawing.Size(74, 13)
        Me.TargetSpecHeat.TabIndex = 39
        Me.TargetSpecHeat.Text = "Specific Heat:"
        '
        'TargetDensity
        '
        Me.TargetDensity.AutoSize = True
        Me.TargetDensity.Location = New System.Drawing.Point(7, 98)
        Me.TargetDensity.Name = "TargetDensity"
        Me.TargetDensity.Size = New System.Drawing.Size(45, 13)
        Me.TargetDensity.TabIndex = 38
        Me.TargetDensity.Text = "Density:"
        '
        'TargetThickness
        '
        Me.TargetThickness.AutoSize = True
        Me.TargetThickness.Location = New System.Drawing.Point(7, 121)
        Me.TargetThickness.Name = "TargetThickness"
        Me.TargetThickness.Size = New System.Drawing.Size(59, 13)
        Me.TargetThickness.TabIndex = 37
        Me.TargetThickness.Text = "Thickness:"
        '
        'TargetConduct
        '
        Me.TargetConduct.AutoSize = True
        Me.TargetConduct.Location = New System.Drawing.Point(7, 52)
        Me.TargetConduct.Name = "TargetConduct"
        Me.TargetConduct.Size = New System.Drawing.Size(68, 13)
        Me.TargetConduct.TabIndex = 36
        Me.TargetConduct.Text = "Conductivity:"
        '
        'TargetMaterial
        '
        Me.TargetMaterial.ItemHeight = 13
        Me.TargetMaterial.Location = New System.Drawing.Point(60, 19)
        Me.TargetMaterial.Name = "TargetMaterial"
        Me.TargetMaterial.Size = New System.Drawing.Size(192, 21)
        Me.TargetMaterial.TabIndex = 0
        Me.TargetMaterial.Text = "GYPSUM"
        '
        'Label78
        '
        Me.Label78.AutoSize = True
        Me.Label78.Location = New System.Drawing.Point(7, 22)
        Me.Label78.Name = "Label78"
        Me.Label78.Size = New System.Drawing.Size(47, 13)
        Me.Label78.TabIndex = 35
        Me.Label78.Text = "Material:"
        Me.Label78.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label80
        '
        Me.Label80.AutoSize = True
        Me.Label80.Location = New System.Drawing.Point(596, 23)
        Me.Label80.Name = "Label80"
        Me.Label80.Size = New System.Drawing.Size(87, 13)
        Me.Label80.TabIndex = 29
        Me.Label80.Text = "Solution Method:"
        Me.Label80.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label79
        '
        Me.Label79.AutoSize = True
        Me.Label79.Location = New System.Drawing.Point(373, 23)
        Me.Label79.Name = "Label79"
        Me.Label79.Size = New System.Drawing.Size(68, 13)
        Me.Label79.TabIndex = 40
        Me.Label79.Text = "Target Type:"
        Me.Label79.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetSolutionMethod
        '
        Me.TargetSolutionMethod.ItemHeight = 13
        Me.TargetSolutionMethod.Items.AddRange(New Object() {"Implicit", "Explicit", "Steady"})
        Me.TargetSolutionMethod.Location = New System.Drawing.Point(692, 19)
        Me.TargetSolutionMethod.Name = "TargetSolutionMethod"
        Me.TargetSolutionMethod.Size = New System.Drawing.Size(104, 21)
        Me.TargetSolutionMethod.TabIndex = 2
        Me.TargetSolutionMethod.Text = "Implicit"
        '
        'TargetSolutionThickness
        '
        Me.TargetSolutionThickness.ItemHeight = 13
        Me.TargetSolutionThickness.Items.AddRange(New Object() {"Thermally Thick", "Thermally Thin", "Cylindrical"})
        Me.TargetSolutionThickness.Location = New System.Drawing.Point(450, 19)
        Me.TargetSolutionThickness.Name = "TargetSolutionThickness"
        Me.TargetSolutionThickness.Size = New System.Drawing.Size(104, 21)
        Me.TargetSolutionThickness.TabIndex = 1
        Me.TargetSolutionThickness.Text = "Thermally Thick"
        '
        'TargetComp
        '
        Me.TargetComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.TargetComp.ItemHeight = 13
        Me.TargetComp.Location = New System.Drawing.Point(140, 19)
        Me.TargetComp.Name = "TargetComp"
        Me.TargetComp.Size = New System.Drawing.Size(208, 21)
        Me.TargetComp.TabIndex = 0
        '
        'Label74
        '
        Me.Label74.AutoSize = True
        Me.Label74.Location = New System.Drawing.Point(59, 23)
        Me.Label74.Name = "Label74"
        Me.Label74.Size = New System.Drawing.Size(72, 13)
        Me.Label74.TabIndex = 34
        Me.Label74.Text = "Compartment:"
        Me.Label74.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox28
        '
        Me.GroupBox28.Controls.Add(Me.Label57)
        Me.GroupBox28.Controls.Add(Me.Label56)
        Me.GroupBox28.Controls.Add(Me.TargetNormalCalc)
        Me.GroupBox28.Controls.Add(Me.TargetZPosition)
        Me.GroupBox28.Controls.Add(Me.TargetZNormal)
        Me.GroupBox28.Controls.Add(Me.Label75)
        Me.GroupBox28.Controls.Add(Me.Label61)
        Me.GroupBox28.Controls.Add(Me.TargetYPosition)
        Me.GroupBox28.Controls.Add(Me.TargetYNormal)
        Me.GroupBox28.Controls.Add(Me.Label72)
        Me.GroupBox28.Controls.Add(Me.Label76)
        Me.GroupBox28.Controls.Add(Me.TargetXNormal)
        Me.GroupBox28.Controls.Add(Me.TargetXPosition)
        Me.GroupBox28.Controls.Add(Me.Label73)
        Me.GroupBox28.Controls.Add(Me.Label77)
        Me.GroupBox28.Location = New System.Drawing.Point(66, 75)
        Me.GroupBox28.Name = "GroupBox28"
        Me.GroupBox28.Size = New System.Drawing.Size(378, 175)
        Me.GroupBox28.TabIndex = 55
        Me.GroupBox28.TabStop = False
        Me.GroupBox28.Text = "Target Geometry"
        '
        'Label57
        '
        Me.Label57.AutoSize = True
        Me.Label57.Location = New System.Drawing.Point(54, 14)
        Me.Label57.Name = "Label57"
        Me.Label57.Size = New System.Drawing.Size(78, 13)
        Me.Label57.TabIndex = 33
        Me.Label57.Text = "Target Position"
        '
        'Label56
        '
        Me.Label56.AutoSize = True
        Me.Label56.Location = New System.Drawing.Point(222, 14)
        Me.Label56.Name = "Label56"
        Me.Label56.Size = New System.Drawing.Size(118, 13)
        Me.Label56.TabIndex = 32
        Me.Label56.Text = "Normal Vector Points to"
        '
        'TargetNormalCalc
        '
        Me.TargetNormalCalc.Items.AddRange(New Object() {"User Specified"})
        Me.TargetNormalCalc.Location = New System.Drawing.Point(203, 41)
        Me.TargetNormalCalc.Name = "TargetNormalCalc"
        Me.TargetNormalCalc.Size = New System.Drawing.Size(157, 21)
        Me.TargetNormalCalc.TabIndex = 3
        Me.TargetNormalCalc.Text = "User Specified"
        '
        'TargetZPosition
        '
        Me.TargetZPosition.Location = New System.Drawing.Point(75, 141)
        Me.TargetZPosition.Name = "TargetZPosition"
        Me.TargetZPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetZPosition.TabIndex = 2
        '
        'TargetZNormal
        '
        Me.TargetZNormal.Location = New System.Drawing.Point(264, 141)
        Me.TargetZNormal.Name = "TargetZNormal"
        Me.TargetZNormal.Size = New System.Drawing.Size(96, 20)
        Me.TargetZNormal.TabIndex = 6
        '
        'Label75
        '
        Me.Label75.AutoSize = True
        Me.Label75.Location = New System.Drawing.Point(11, 145)
        Me.Label75.Name = "Label75"
        Me.Label75.Size = New System.Drawing.Size(57, 13)
        Me.Label75.TabIndex = 31
        Me.Label75.Text = "Height (Z):"
        Me.Label75.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label61
        '
        Me.Label61.AutoSize = True
        Me.Label61.Location = New System.Drawing.Point(200, 145)
        Me.Label61.Name = "Label61"
        Me.Label61.Size = New System.Drawing.Size(59, 13)
        Me.Label61.TabIndex = 31
        Me.Label61.Text = "Normal (Z):"
        Me.Label61.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetYPosition
        '
        Me.TargetYPosition.Location = New System.Drawing.Point(75, 108)
        Me.TargetYPosition.Name = "TargetYPosition"
        Me.TargetYPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetYPosition.TabIndex = 1
        '
        'TargetYNormal
        '
        Me.TargetYNormal.Location = New System.Drawing.Point(264, 109)
        Me.TargetYNormal.Name = "TargetYNormal"
        Me.TargetYNormal.Size = New System.Drawing.Size(96, 20)
        Me.TargetYNormal.TabIndex = 5
        '
        'Label72
        '
        Me.Label72.AutoSize = True
        Me.Label72.Location = New System.Drawing.Point(200, 113)
        Me.Label72.Name = "Label72"
        Me.Label72.Size = New System.Drawing.Size(59, 13)
        Me.Label72.TabIndex = 29
        Me.Label72.Text = "Normal (Y):"
        Me.Label72.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label76
        '
        Me.Label76.AutoSize = True
        Me.Label76.Location = New System.Drawing.Point(11, 112)
        Me.Label76.Name = "Label76"
        Me.Label76.Size = New System.Drawing.Size(55, 13)
        Me.Label76.TabIndex = 29
        Me.Label76.Text = "Depth (Y):"
        Me.Label76.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetXNormal
        '
        Me.TargetXNormal.Location = New System.Drawing.Point(264, 77)
        Me.TargetXNormal.Name = "TargetXNormal"
        Me.TargetXNormal.Size = New System.Drawing.Size(96, 20)
        Me.TargetXNormal.TabIndex = 4
        '
        'TargetXPosition
        '
        Me.TargetXPosition.Location = New System.Drawing.Point(75, 75)
        Me.TargetXPosition.Name = "TargetXPosition"
        Me.TargetXPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetXPosition.TabIndex = 0
        '
        'Label73
        '
        Me.Label73.AutoSize = True
        Me.Label73.Location = New System.Drawing.Point(200, 81)
        Me.Label73.Name = "Label73"
        Me.Label73.Size = New System.Drawing.Size(59, 13)
        Me.Label73.TabIndex = 27
        Me.Label73.Text = "Normal (X):"
        Me.Label73.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label77
        '
        Me.Label77.AutoSize = True
        Me.Label77.Location = New System.Drawing.Point(11, 79)
        Me.Label77.Name = "Label77"
        Me.Label77.Size = New System.Drawing.Size(54, 13)
        Me.Label77.TabIndex = 27
        Me.Label77.Text = "Width (X):"
        Me.Label77.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetRemove
        '
        Me.TargetRemove.Location = New System.Drawing.Point(675, 219)
        Me.TargetRemove.Name = "TargetRemove"
        Me.TargetRemove.Size = New System.Drawing.Size(75, 23)
        Me.TargetRemove.TabIndex = 5
        Me.TargetRemove.Text = "Remove"
        '
        'TargetMoveDown
        '
        Me.TargetMoveDown.Location = New System.Drawing.Point(515, 219)
        Me.TargetMoveDown.Name = "TargetMoveDown"
        Me.TargetMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.TargetMoveDown.TabIndex = 4
        Me.TargetMoveDown.Text = "Move Down"
        '
        'TargetAdd
        '
        Me.TargetAdd.Location = New System.Drawing.Point(227, 219)
        Me.TargetAdd.Name = "TargetAdd"
        Me.TargetAdd.Size = New System.Drawing.Size(75, 23)
        Me.TargetAdd.TabIndex = 1
        Me.TargetAdd.Text = "Add"
        '
        'TargetMoveUp
        '
        Me.TargetMoveUp.Location = New System.Drawing.Point(419, 219)
        Me.TargetMoveUp.Name = "TargetMoveUp"
        Me.TargetMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.TargetMoveUp.TabIndex = 3
        Me.TargetMoveUp.Text = "Move Up"
        '
        'TargetDup
        '
        Me.TargetDup.Location = New System.Drawing.Point(323, 219)
        Me.TargetDup.Name = "TargetDup"
        Me.TargetDup.Size = New System.Drawing.Size(75, 23)
        Me.TargetDup.TabIndex = 2
        Me.TargetDup.Text = "Duplicate"
        '
        'TabHeatTransfer
        '
        Me.TabHeatTransfer.Controls.Add(Me.GroupBox2)
        Me.TabHeatTransfer.Controls.Add(Me.GroupBox1)
        Me.TabHeatTransfer.Location = New System.Drawing.Point(4, 22)
        Me.TabHeatTransfer.Name = "TabHeatTransfer"
        Me.TabHeatTransfer.Size = New System.Drawing.Size(976, 592)
        Me.TabHeatTransfer.TabIndex = 7
        Me.TabHeatTransfer.Text = "Surface Connections"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.VHeatSummary)
        Me.GroupBox2.Controls.Add(Me.GroupVHeats)
        Me.GroupBox2.Controls.Add(Me.VHeatDup)
        Me.GroupBox2.Controls.Add(Me.VHeatRemove)
        Me.GroupBox2.Controls.Add(Me.VHeatAdd)
        Me.GroupBox2.Location = New System.Drawing.Point(501, 42)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(441, 474)
        Me.GroupBox2.TabIndex = 7
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Vertical Connections"
        '
        'VHeatSummary
        '
        Me.VHeatSummary.AllowEditing = False
        Me.VHeatSummary.ColumnInfo = resources.GetString("VHeatSummary.ColumnInfo")
        Me.VHeatSummary.ExtendLastCol = True
        Me.VHeatSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VHeatSummary.Location = New System.Drawing.Point(38, 43)
        Me.VHeatSummary.Name = "VHeatSummary"
        Me.VHeatSummary.Rows.Count = 181
        Me.VHeatSummary.Rows.DefaultSize = 19
        Me.VHeatSummary.Size = New System.Drawing.Size(364, 168)
        Me.VHeatSummary.StyleInfo = resources.GetString("VHeatSummary.StyleInfo")
        Me.VHeatSummary.TabIndex = 0
        Me.VHeatSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupVHeats
        '
        Me.GroupVHeats.Controls.Add(Me.VHeatComp2)
        Me.GroupVHeats.Controls.Add(Me.Label16)
        Me.GroupVHeats.Controls.Add(Me.VHeatComp1)
        Me.GroupVHeats.Controls.Add(Me.Label39)
        Me.GroupVHeats.Location = New System.Drawing.Point(36, 288)
        Me.GroupVHeats.Name = "GroupVHeats"
        Me.GroupVHeats.Size = New System.Drawing.Size(368, 143)
        Me.GroupVHeats.TabIndex = 5
        Me.GroupVHeats.TabStop = False
        Me.GroupVHeats.Text = "Heat Transfer Connection 1"
        '
        'VHeatComp2
        '
        Me.VHeatComp2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VHeatComp2.ItemHeight = 13
        Me.VHeatComp2.Location = New System.Drawing.Point(139, 77)
        Me.VHeatComp2.Name = "VHeatComp2"
        Me.VHeatComp2.Size = New System.Drawing.Size(208, 21)
        Me.VHeatComp2.TabIndex = 1
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.Location = New System.Drawing.Point(35, 48)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(94, 13)
        Me.Label16.TabIndex = 89
        Me.Label16.Text = "Top Compartment:"
        Me.Label16.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VHeatComp1
        '
        Me.VHeatComp1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VHeatComp1.ItemHeight = 13
        Me.VHeatComp1.Location = New System.Drawing.Point(139, 45)
        Me.VHeatComp1.Name = "VHeatComp1"
        Me.VHeatComp1.Size = New System.Drawing.Size(208, 21)
        Me.VHeatComp1.TabIndex = 0
        '
        'Label39
        '
        Me.Label39.AutoSize = True
        Me.Label39.Location = New System.Drawing.Point(21, 80)
        Me.Label39.Name = "Label39"
        Me.Label39.Size = New System.Drawing.Size(108, 13)
        Me.Label39.TabIndex = 91
        Me.Label39.Text = "Bottom Compartment:"
        Me.Label39.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VHeatDup
        '
        Me.VHeatDup.Location = New System.Drawing.Point(155, 242)
        Me.VHeatDup.Name = "VHeatDup"
        Me.VHeatDup.Size = New System.Drawing.Size(75, 23)
        Me.VHeatDup.TabIndex = 2
        Me.VHeatDup.Text = "Duplicate"
        '
        'VHeatRemove
        '
        Me.VHeatRemove.Location = New System.Drawing.Point(307, 242)
        Me.VHeatRemove.Name = "VHeatRemove"
        Me.VHeatRemove.Size = New System.Drawing.Size(75, 23)
        Me.VHeatRemove.TabIndex = 3
        Me.VHeatRemove.Text = "Remove"
        '
        'VHeatAdd
        '
        Me.VHeatAdd.Location = New System.Drawing.Point(59, 242)
        Me.VHeatAdd.Name = "VHeatAdd"
        Me.VHeatAdd.Size = New System.Drawing.Size(75, 23)
        Me.VHeatAdd.TabIndex = 1
        Me.VHeatAdd.Text = "Add"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.HHeatSummary)
        Me.GroupBox1.Controls.Add(Me.GroupHHeats)
        Me.GroupBox1.Controls.Add(Me.HHeatDup)
        Me.GroupBox1.Controls.Add(Me.HHeatRemove)
        Me.GroupBox1.Controls.Add(Me.HHeatAdd)
        Me.GroupBox1.Location = New System.Drawing.Point(34, 42)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(441, 474)
        Me.GroupBox1.TabIndex = 6
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Horizontal Connections"
        '
        'HHeatSummary
        '
        Me.HHeatSummary.AllowEditing = False
        Me.HHeatSummary.ColumnInfo = resources.GetString("HHeatSummary.ColumnInfo")
        Me.HHeatSummary.ExtendLastCol = True
        Me.HHeatSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.HHeatSummary.Location = New System.Drawing.Point(9, 43)
        Me.HHeatSummary.Name = "HHeatSummary"
        Me.HHeatSummary.Rows.Count = 181
        Me.HHeatSummary.Rows.DefaultSize = 19
        Me.HHeatSummary.Size = New System.Drawing.Size(423, 168)
        Me.HHeatSummary.StyleInfo = resources.GetString("HHeatSummary.StyleInfo")
        Me.HHeatSummary.TabIndex = 0
        Me.HHeatSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupHHeats
        '
        Me.GroupHHeats.Controls.Add(Me.HHeatComp2)
        Me.GroupHHeats.Controls.Add(Me.Label86)
        Me.GroupHHeats.Controls.Add(Me.HHeatFraction)
        Me.GroupHHeats.Controls.Add(Me.HHeatComp1)
        Me.GroupHHeats.Controls.Add(Me.Label93)
        Me.GroupHHeats.Controls.Add(Me.Label85)
        Me.GroupHHeats.Location = New System.Drawing.Point(36, 288)
        Me.GroupHHeats.Name = "GroupHHeats"
        Me.GroupHHeats.Size = New System.Drawing.Size(368, 143)
        Me.GroupHHeats.TabIndex = 5
        Me.GroupHHeats.TabStop = False
        Me.GroupHHeats.Text = "Heat Transfer Connection 1"
        '
        'HHeatComp2
        '
        Me.HHeatComp2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.HHeatComp2.ItemHeight = 13
        Me.HHeatComp2.Location = New System.Drawing.Point(139, 61)
        Me.HHeatComp2.Name = "HHeatComp2"
        Me.HHeatComp2.Size = New System.Drawing.Size(208, 21)
        Me.HHeatComp2.TabIndex = 1
        '
        'Label86
        '
        Me.Label86.AutoSize = True
        Me.Label86.Location = New System.Drawing.Point(39, 32)
        Me.Label86.Name = "Label86"
        Me.Label86.Size = New System.Drawing.Size(94, 13)
        Me.Label86.TabIndex = 89
        Me.Label86.Text = "First Compartment:"
        Me.Label86.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HHeatFraction
        '
        Me.HHeatFraction.Location = New System.Drawing.Point(139, 93)
        Me.HHeatFraction.Name = "HHeatFraction"
        Me.HHeatFraction.Size = New System.Drawing.Size(96, 20)
        Me.HHeatFraction.TabIndex = 2
        Me.HHeatFraction.Text = "1"
        '
        'HHeatComp1
        '
        Me.HHeatComp1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.HHeatComp1.ItemHeight = 13
        Me.HHeatComp1.Location = New System.Drawing.Point(139, 29)
        Me.HHeatComp1.Name = "HHeatComp1"
        Me.HHeatComp1.Size = New System.Drawing.Size(208, 21)
        Me.HHeatComp1.TabIndex = 0
        '
        'Label93
        '
        Me.Label93.AutoSize = True
        Me.Label93.Location = New System.Drawing.Point(85, 96)
        Me.Label93.Name = "Label93"
        Me.Label93.Size = New System.Drawing.Size(48, 13)
        Me.Label93.TabIndex = 92
        Me.Label93.Text = "Fraction:"
        Me.Label93.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label85
        '
        Me.Label85.AutoSize = True
        Me.Label85.Location = New System.Drawing.Point(21, 64)
        Me.Label85.Name = "Label85"
        Me.Label85.Size = New System.Drawing.Size(112, 13)
        Me.Label85.TabIndex = 91
        Me.Label85.Text = "Second Compartment:"
        Me.Label85.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HHeatDup
        '
        Me.HHeatDup.Location = New System.Drawing.Point(155, 242)
        Me.HHeatDup.Name = "HHeatDup"
        Me.HHeatDup.Size = New System.Drawing.Size(75, 23)
        Me.HHeatDup.TabIndex = 2
        Me.HHeatDup.Text = "Duplicate"
        '
        'HHeatRemove
        '
        Me.HHeatRemove.Location = New System.Drawing.Point(307, 242)
        Me.HHeatRemove.Name = "HHeatRemove"
        Me.HHeatRemove.Size = New System.Drawing.Size(75, 23)
        Me.HHeatRemove.TabIndex = 3
        Me.HHeatRemove.Text = "Remove"
        '
        'HHeatAdd
        '
        Me.HHeatAdd.Location = New System.Drawing.Point(59, 242)
        Me.HHeatAdd.Name = "HHeatAdd"
        Me.HHeatAdd.Size = New System.Drawing.Size(75, 23)
        Me.HHeatAdd.TabIndex = 1
        Me.HHeatAdd.Text = "Add"
        '
        'TabMain
        '
        Me.TabMain.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.TabMain.Controls.Add(Me.TabEnvironment)
        Me.TabMain.Controls.Add(Me.TabMaterials)
        Me.TabMain.Controls.Add(Me.TabGeometry)
        Me.TabMain.Controls.Add(Me.TabHorizontalFlow)
        Me.TabMain.Controls.Add(Me.TabVerticalFlow)
        Me.TabMain.Controls.Add(Me.TabMechanicalFlow)
        Me.TabMain.Controls.Add(Me.TabFires)
        Me.TabMain.Controls.Add(Me.TabTargets)
        Me.TabMain.Controls.Add(Me.TabDetection)
        Me.TabMain.Controls.Add(Me.TabHeatTransfer)
        Me.TabMain.Controls.Add(Me.TabVisuals)
        Me.TabMain.ItemSize = New System.Drawing.Size(122, 18)
        Me.TabMain.Location = New System.Drawing.Point(10, 2)
        Me.TabMain.Name = "TabMain"
        Me.TabMain.SelectedIndex = 0
        Me.TabMain.Size = New System.Drawing.Size(984, 618)
        Me.TabMain.TabIndex = 0
        '
        'TabMaterials
        '
        Me.TabMaterials.Controls.Add(Me.ThermalFromFile)
        Me.TabMaterials.Controls.Add(Me.ThermalRemove)
        Me.TabMaterials.Controls.Add(Me.ThermalAdd)
        Me.TabMaterials.Controls.Add(Me.ThermalDup)
        Me.TabMaterials.Controls.Add(Me.ThermalSummary)
        Me.TabMaterials.Controls.Add(Me.GroupThermal)
        Me.TabMaterials.Location = New System.Drawing.Point(4, 22)
        Me.TabMaterials.Name = "TabMaterials"
        Me.TabMaterials.Size = New System.Drawing.Size(976, 592)
        Me.TabMaterials.TabIndex = 9
        Me.TabMaterials.Text = "Thermal Properties"
        Me.TabMaterials.UseVisualStyleBackColor = True
        '
        'ThermalFromFile
        '
        Me.ThermalFromFile.Location = New System.Drawing.Point(476, 297)
        Me.ThermalFromFile.Name = "ThermalFromFile"
        Me.ThermalFromFile.Size = New System.Drawing.Size(75, 23)
        Me.ThermalFromFile.TabIndex = 39
        Me.ThermalFromFile.Text = "From File"
        '
        'ThermalRemove
        '
        Me.ThermalRemove.Location = New System.Drawing.Point(626, 297)
        Me.ThermalRemove.Name = "ThermalRemove"
        Me.ThermalRemove.Size = New System.Drawing.Size(75, 23)
        Me.ThermalRemove.TabIndex = 38
        Me.ThermalRemove.Text = "Remove"
        '
        'ThermalAdd
        '
        Me.ThermalAdd.Location = New System.Drawing.Point(276, 297)
        Me.ThermalAdd.Name = "ThermalAdd"
        Me.ThermalAdd.Size = New System.Drawing.Size(75, 23)
        Me.ThermalAdd.TabIndex = 36
        Me.ThermalAdd.Text = "Add"
        '
        'ThermalDup
        '
        Me.ThermalDup.Location = New System.Drawing.Point(376, 297)
        Me.ThermalDup.Name = "ThermalDup"
        Me.ThermalDup.Size = New System.Drawing.Size(75, 23)
        Me.ThermalDup.TabIndex = 37
        Me.ThermalDup.Text = "Duplicate"
        '
        'ThermalSummary
        '
        Me.ThermalSummary.ColumnInfo = resources.GetString("ThermalSummary.ColumnInfo")
        Me.ThermalSummary.ExtendLastCol = True
        Me.ThermalSummary.Location = New System.Drawing.Point(112, 106)
        Me.ThermalSummary.Name = "ThermalSummary"
        Me.ThermalSummary.Rows.Count = 126
        Me.ThermalSummary.Rows.DefaultSize = 17
        Me.ThermalSummary.Size = New System.Drawing.Size(752, 176)
        Me.ThermalSummary.TabIndex = 34
        '
        'GroupThermal
        '
        Me.GroupThermal.Controls.Add(Me.ThermalThickness)
        Me.GroupThermal.Controls.Add(Me.Label53)
        Me.GroupThermal.Controls.Add(Me.ThermalConductivity)
        Me.GroupThermal.Controls.Add(Me.Label62)
        Me.GroupThermal.Controls.Add(Me.ThermalSpecificHeat)
        Me.GroupThermal.Controls.Add(Me.Label84)
        Me.GroupThermal.Controls.Add(Me.ThermalLongName)
        Me.GroupThermal.Controls.Add(Me.Label94)
        Me.GroupThermal.Controls.Add(Me.ThermalDensity)
        Me.GroupThermal.Controls.Add(Me.Label102)
        Me.GroupThermal.Controls.Add(Me.Label103)
        Me.GroupThermal.Controls.Add(Me.ThermalEmissivity)
        Me.GroupThermal.Controls.Add(Me.Label104)
        Me.GroupThermal.Controls.Add(Me.ThermalShortName)
        Me.GroupThermal.Location = New System.Drawing.Point(112, 339)
        Me.GroupThermal.Name = "GroupThermal"
        Me.GroupThermal.Size = New System.Drawing.Size(752, 113)
        Me.GroupThermal.TabIndex = 35
        Me.GroupThermal.TabStop = False
        Me.GroupThermal.Text = "Property 1 (of 1)"
        '
        'ThermalThickness
        '
        Me.ThermalThickness.Location = New System.Drawing.Point(392, 79)
        Me.ThermalThickness.Name = "ThermalThickness"
        Me.ThermalThickness.Size = New System.Drawing.Size(80, 20)
        Me.ThermalThickness.TabIndex = 7
        '
        'Label53
        '
        Me.Label53.AutoSize = True
        Me.Label53.Location = New System.Drawing.Point(272, 47)
        Me.Label53.Name = "Label53"
        Me.Label53.Size = New System.Drawing.Size(109, 13)
        Me.Label53.TabIndex = 1
        Me.Label53.Text = "Thermal Conductivity:"
        Me.Label53.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalConductivity
        '
        Me.ThermalConductivity.Location = New System.Drawing.Point(392, 47)
        Me.ThermalConductivity.Name = "ThermalConductivity"
        Me.ThermalConductivity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalConductivity.TabIndex = 4
        '
        'Label62
        '
        Me.Label62.AutoSize = True
        Me.Label62.Location = New System.Drawing.Point(536, 47)
        Me.Label62.Name = "Label62"
        Me.Label62.Size = New System.Drawing.Size(74, 13)
        Me.Label62.TabIndex = 5
        Me.Label62.Text = "Specific Heat:"
        Me.Label62.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalSpecificHeat
        '
        Me.ThermalSpecificHeat.Location = New System.Drawing.Point(616, 47)
        Me.ThermalSpecificHeat.Name = "ThermalSpecificHeat"
        Me.ThermalSpecificHeat.Size = New System.Drawing.Size(80, 20)
        Me.ThermalSpecificHeat.TabIndex = 5
        '
        'Label84
        '
        Me.Label84.AutoSize = True
        Me.Label84.Location = New System.Drawing.Point(244, 15)
        Me.Label84.Name = "Label84"
        Me.Label84.Size = New System.Drawing.Size(47, 13)
        Me.Label84.TabIndex = 7
        Me.Label84.Text = "Material:"
        Me.Label84.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalLongName
        '
        Me.ThermalLongName.Location = New System.Drawing.Point(300, 15)
        Me.ThermalLongName.Name = "ThermalLongName"
        Me.ThermalLongName.Size = New System.Drawing.Size(208, 20)
        Me.ThermalLongName.TabIndex = 2
        '
        'Label94
        '
        Me.Label94.AutoSize = True
        Me.Label94.Location = New System.Drawing.Point(79, 79)
        Me.Label94.Name = "Label94"
        Me.Label94.Size = New System.Drawing.Size(45, 13)
        Me.Label94.TabIndex = 9
        Me.Label94.Text = "Density:"
        Me.Label94.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalDensity
        '
        Me.ThermalDensity.Location = New System.Drawing.Point(128, 79)
        Me.ThermalDensity.Name = "ThermalDensity"
        Me.ThermalDensity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalDensity.TabIndex = 6
        '
        'Label102
        '
        Me.Label102.AutoSize = True
        Me.Label102.Location = New System.Drawing.Point(320, 79)
        Me.Label102.Name = "Label102"
        Me.Label102.Size = New System.Drawing.Size(59, 13)
        Me.Label102.TabIndex = 11
        Me.Label102.Text = "Thickness:"
        Me.Label102.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label103
        '
        Me.Label103.AutoSize = True
        Me.Label103.Location = New System.Drawing.Point(552, 79)
        Me.Label103.Name = "Label103"
        Me.Label103.Size = New System.Drawing.Size(55, 13)
        Me.Label103.TabIndex = 13
        Me.Label103.Text = "Emissivity:"
        Me.Label103.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalEmissivity
        '
        Me.ThermalEmissivity.Location = New System.Drawing.Point(616, 79)
        Me.ThermalEmissivity.Name = "ThermalEmissivity"
        Me.ThermalEmissivity.Size = New System.Drawing.Size(80, 20)
        Me.ThermalEmissivity.TabIndex = 8
        '
        'Label104
        '
        Me.Label104.AutoSize = True
        Me.Label104.Location = New System.Drawing.Point(56, 47)
        Me.Label104.Name = "Label104"
        Me.Label104.Size = New System.Drawing.Size(66, 13)
        Me.Label104.TabIndex = 15
        Me.Label104.Text = "Short Name:"
        Me.Label104.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalShortName
        '
        Me.ThermalShortName.Location = New System.Drawing.Point(128, 47)
        Me.ThermalShortName.Name = "ThermalShortName"
        Me.ThermalShortName.Size = New System.Drawing.Size(80, 20)
        Me.ThermalShortName.TabIndex = 3
        '
        'TabVisuals
        '
        Me.TabVisuals.Controls.Add(Me.GroupVisualResolution)
        Me.TabVisuals.Controls.Add(Me.GroupBox4)
        Me.TabVisuals.Location = New System.Drawing.Point(4, 22)
        Me.TabVisuals.Name = "TabVisuals"
        Me.TabVisuals.Padding = New System.Windows.Forms.Padding(3)
        Me.TabVisuals.Size = New System.Drawing.Size(976, 592)
        Me.TabVisuals.TabIndex = 8
        Me.TabVisuals.Text = "Visualizations"
        Me.TabVisuals.UseVisualStyleBackColor = True
        '
        'GroupVisualResolution
        '
        Me.GroupVisualResolution.Controls.Add(Me.Label35)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationZ)
        Me.GroupVisualResolution.Controls.Add(Me.Label32)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationY)
        Me.GroupVisualResolution.Controls.Add(Me.Label31)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationX)
        Me.GroupVisualResolution.Controls.Add(Me.VisualResolution)
        Me.GroupVisualResolution.Location = New System.Drawing.Point(25, 315)
        Me.GroupVisualResolution.Name = "GroupVisualResolution"
        Me.GroupVisualResolution.Size = New System.Drawing.Size(931, 223)
        Me.GroupVisualResolution.TabIndex = 117
        Me.GroupVisualResolution.TabStop = False
        Me.GroupVisualResolution.Text = "Resolution"
        '
        'Label35
        '
        Me.Label35.Location = New System.Drawing.Point(533, 126)
        Me.Label35.Name = "Label35"
        Me.Label35.Size = New System.Drawing.Size(90, 23)
        Me.Label35.TabIndex = 119
        Me.Label35.Text = "Height (Z) Grid:"
        Me.Label35.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationZ
        '
        Me.VisualizationZ.Location = New System.Drawing.Point(629, 129)
        Me.VisualizationZ.Name = "VisualizationZ"
        Me.VisualizationZ.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationZ.TabIndex = 118
        '
        'Label32
        '
        Me.Label32.Location = New System.Drawing.Point(533, 100)
        Me.Label32.Name = "Label32"
        Me.Label32.Size = New System.Drawing.Size(90, 23)
        Me.Label32.TabIndex = 117
        Me.Label32.Text = "Depth (Y) Grid:"
        Me.Label32.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationY
        '
        Me.VisualizationY.Location = New System.Drawing.Point(629, 103)
        Me.VisualizationY.Name = "VisualizationY"
        Me.VisualizationY.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationY.TabIndex = 116
        '
        'Label31
        '
        Me.Label31.Location = New System.Drawing.Point(530, 74)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(93, 23)
        Me.Label31.TabIndex = 115
        Me.Label31.Text = "Width (X) Grid:"
        Me.Label31.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationX
        '
        Me.VisualizationX.Location = New System.Drawing.Point(629, 77)
        Me.VisualizationX.Name = "VisualizationX"
        Me.VisualizationX.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationX.TabIndex = 114
        '
        'VisualResolution
        '
        Me.VisualResolution.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.VisualResolution.AllowEditing = False
        Me.VisualResolution.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.VisualResolution.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.VisualResolution.AutoGenerateColumns = False
        Me.VisualResolution.ColumnInfo = resources.GetString("VisualResolution.ColumnInfo")
        Me.VisualResolution.ExtendLastCol = True
        Me.VisualResolution.FocusRect = C1.Win.C1FlexGrid.FocusRectEnum.None
        Me.VisualResolution.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VisualResolution.Location = New System.Drawing.Point(24, 18)
        Me.VisualResolution.Name = "VisualResolution"
        Me.VisualResolution.Rows.DefaultSize = 19
        Me.VisualResolution.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VisualResolution.Size = New System.Drawing.Size(417, 187)
        Me.VisualResolution.StyleInfo = resources.GetString("VisualResolution.StyleInfo")
        Me.VisualResolution.TabIndex = 1
        Me.VisualResolution.TabStop = False
        Me.VisualResolution.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.VisualSummary)
        Me.GroupBox4.Controls.Add(Me.VisualizationAxisLabel)
        Me.GroupBox4.Controls.Add(Me.VisualizationAdd)
        Me.GroupBox4.Controls.Add(Me.VisualizationAxis)
        Me.GroupBox4.Controls.Add(Me.VisualizationType)
        Me.GroupBox4.Controls.Add(Me.VisualizationValueLabel)
        Me.GroupBox4.Controls.Add(Me.VisualizationDup)
        Me.GroupBox4.Controls.Add(Me.Label29)
        Me.GroupBox4.Controls.Add(Me.VisualizationComp)
        Me.GroupBox4.Controls.Add(Me.VisualizationDefaults)
        Me.GroupBox4.Controls.Add(Me.Label7)
        Me.GroupBox4.Controls.Add(Me.VisualizationValue)
        Me.GroupBox4.Controls.Add(Me.VisualizationRemove)
        Me.GroupBox4.Location = New System.Drawing.Point(25, 15)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(931, 294)
        Me.GroupBox4.TabIndex = 116
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Visualizations"
        '
        'VisualSummary
        '
        Me.VisualSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.VisualSummary.AllowEditing = False
        Me.VisualSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.VisualSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.VisualSummary.AutoGenerateColumns = False
        Me.VisualSummary.ColumnInfo = resources.GetString("VisualSummary.ColumnInfo")
        Me.VisualSummary.ExtendLastCol = True
        Me.VisualSummary.FocusRect = C1.Win.C1FlexGrid.FocusRectEnum.None
        Me.VisualSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VisualSummary.Location = New System.Drawing.Point(34, 28)
        Me.VisualSummary.Name = "VisualSummary"
        Me.VisualSummary.Rows.DefaultSize = 19
        Me.VisualSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VisualSummary.Size = New System.Drawing.Size(407, 197)
        Me.VisualSummary.StyleInfo = resources.GetString("VisualSummary.StyleInfo")
        Me.VisualSummary.TabIndex = 102
        Me.VisualSummary.TabStop = False
        Me.VisualSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'VisualizationAxisLabel
        '
        Me.VisualizationAxisLabel.Location = New System.Drawing.Point(532, 178)
        Me.VisualizationAxisLabel.Name = "VisualizationAxisLabel"
        Me.VisualizationAxisLabel.Size = New System.Drawing.Size(91, 23)
        Me.VisualizationAxisLabel.TabIndex = 115
        Me.VisualizationAxisLabel.Text = "Axis:"
        Me.VisualizationAxisLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationAdd
        '
        Me.VisualizationAdd.Location = New System.Drawing.Point(34, 244)
        Me.VisualizationAdd.Name = "VisualizationAdd"
        Me.VisualizationAdd.Size = New System.Drawing.Size(75, 23)
        Me.VisualizationAdd.TabIndex = 106
        Me.VisualizationAdd.Text = "Add"
        '
        'VisualizationAxis
        '
        Me.VisualizationAxis.ItemHeight = 13
        Me.VisualizationAxis.Items.AddRange(New Object() {"X-axis (Width)", "Y-axis (Depth)", "Z-axis (Height)"})
        Me.VisualizationAxis.Location = New System.Drawing.Point(629, 180)
        Me.VisualizationAxis.MaxDropDownItems = 3
        Me.VisualizationAxis.Name = "VisualizationAxis"
        Me.VisualizationAxis.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationAxis.TabIndex = 114
        '
        'VisualizationType
        '
        Me.VisualizationType.ItemHeight = 13
        Me.VisualizationType.Items.AddRange(New Object() {"2-D", "3-D", "Isosurface"})
        Me.VisualizationType.Location = New System.Drawing.Point(629, 94)
        Me.VisualizationType.MaxDropDownItems = 3
        Me.VisualizationType.Name = "VisualizationType"
        Me.VisualizationType.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationType.TabIndex = 107
        '
        'VisualizationValueLabel
        '
        Me.VisualizationValueLabel.Location = New System.Drawing.Point(551, 149)
        Me.VisualizationValueLabel.Name = "VisualizationValueLabel"
        Me.VisualizationValueLabel.Size = New System.Drawing.Size(72, 23)
        Me.VisualizationValueLabel.TabIndex = 113
        Me.VisualizationValueLabel.Text = "Value:"
        Me.VisualizationValueLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationDup
        '
        Me.VisualizationDup.Location = New System.Drawing.Point(130, 244)
        Me.VisualizationDup.Name = "VisualizationDup"
        Me.VisualizationDup.Size = New System.Drawing.Size(75, 23)
        Me.VisualizationDup.TabIndex = 104
        Me.VisualizationDup.Text = "Duplicate"
        '
        'Label29
        '
        Me.Label29.Location = New System.Drawing.Point(532, 123)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(91, 23)
        Me.Label29.TabIndex = 112
        Me.Label29.Text = "Compartment:"
        Me.Label29.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationComp
        '
        Me.VisualizationComp.ItemHeight = 13
        Me.VisualizationComp.Location = New System.Drawing.Point(629, 123)
        Me.VisualizationComp.Name = "VisualizationComp"
        Me.VisualizationComp.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationComp.TabIndex = 108
        '
        'VisualizationDefaults
        '
        Me.VisualizationDefaults.Location = New System.Drawing.Point(321, 244)
        Me.VisualizationDefaults.Name = "VisualizationDefaults"
        Me.VisualizationDefaults.Size = New System.Drawing.Size(109, 23)
        Me.VisualizationDefaults.TabIndex = 103
        Me.VisualizationDefaults.Text = "Add Defaults"
        '
        'Label7
        '
        Me.Label7.Location = New System.Drawing.Point(519, 94)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(104, 23)
        Me.Label7.TabIndex = 111
        Me.Label7.Text = "Visualization Type:"
        Me.Label7.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationValue
        '
        Me.VisualizationValue.Location = New System.Drawing.Point(629, 152)
        Me.VisualizationValue.Name = "VisualizationValue"
        Me.VisualizationValue.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationValue.TabIndex = 109
        '
        'VisualizationRemove
        '
        Me.VisualizationRemove.Location = New System.Drawing.Point(226, 244)
        Me.VisualizationRemove.Name = "VisualizationRemove"
        Me.VisualizationRemove.Size = New System.Drawing.Size(75, 23)
        Me.VisualizationRemove.TabIndex = 105
        Me.VisualizationRemove.Text = "Remove"
        '
        'OpenDataFileDialog
        '
        Me.OpenDataFileDialog.DefaultExt = "in"
        Me.OpenDataFileDialog.Filter = "CFast files|*.in|Object files|*.o|Spreadsheet files|*.csv|All files|*.*"
        Me.OpenDataFileDialog.Title = "Open"
        '
        'SaveDataFileDialog
        '
        Me.SaveDataFileDialog.DefaultExt = "in"
        Me.SaveDataFileDialog.Filter = "CFast files|*.in|All files|*.*"
        Me.SaveDataFileDialog.Title = "Save As"
        '
        'HelpProvider
        '
        Me.HelpProvider.HelpNamespace = "CFAST6.chm"
        '
        'ErrorProvider1
        '
        Me.ErrorProvider1.ContainerControl = Me
        '
        'MainSave
        '
        Me.MainSave.Location = New System.Drawing.Point(313, 638)
        Me.MainSave.Name = "MainSave"
        Me.MainSave.Size = New System.Drawing.Size(75, 23)
        Me.MainSave.TabIndex = 1
        Me.MainSave.Text = "Save"
        '
        'MainRun
        '
        Me.MainRun.Location = New System.Drawing.Point(543, 638)
        Me.MainRun.Name = "MainRun"
        Me.MainRun.Size = New System.Drawing.Size(75, 23)
        Me.MainRun.TabIndex = 3
        Me.MainRun.Text = "Run"
        '
        'MainGeometry
        '
        Me.MainGeometry.Location = New System.Drawing.Point(462, 638)
        Me.MainGeometry.Name = "MainGeometry"
        Me.MainGeometry.Size = New System.Drawing.Size(75, 23)
        Me.MainGeometry.TabIndex = 2
        Me.MainGeometry.Text = "Geometry"
        '
        'MainOpen
        '
        Me.MainOpen.Location = New System.Drawing.Point(232, 638)
        Me.MainOpen.Name = "MainOpen"
        Me.MainOpen.Size = New System.Drawing.Size(75, 23)
        Me.MainOpen.TabIndex = 0
        Me.MainOpen.Text = "Open"
        '
        'CeditMain
        '
        Me.C1SizerLight1.SetAutoResize(Me, True)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.None
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(1004, 706)
        Me.Controls.Add(Me.MainOpen)
        Me.Controls.Add(Me.MainView)
        Me.Controls.Add(Me.MainGeometry)
        Me.Controls.Add(Me.MainRun)
        Me.Controls.Add(Me.MainSave)
        Me.Controls.Add(Me.StatusBar)
        Me.Controls.Add(Me.TabMain)
        Me.HelpProvider.SetHelpNavigator(Me, System.Windows.Forms.HelpNavigator.TableOfContents)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximumSize = New System.Drawing.Size(1280, 1024)
        Me.Menu = Me.MainMenu
        Me.MinimumSize = New System.Drawing.Size(1020, 744)
        Me.Name = "CeditMain"
        Me.HelpProvider.SetShowHelp(Me, True)
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "CEdit"
        CType(Me.Errors, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Message, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabEnvironment.ResumeLayout(False)
        Me.TabEnvironment.PerformLayout()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        Me.GroupBox12.ResumeLayout(False)
        Me.GroupBox12.PerformLayout()
        Me.GroupBox11.ResumeLayout(False)
        Me.GroupBox11.PerformLayout()
        Me.GroupBox7.ResumeLayout(False)
        Me.GroupBox7.PerformLayout()
        Me.TabHorizontalFlow.ResumeLayout(False)
        CType(Me.HVentSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupHVentGeometry.ResumeLayout(False)
        Me.GroupHVentGeometry.PerformLayout()
        Me.GroupBox14.ResumeLayout(False)
        Me.GroupBox13.ResumeLayout(False)
        Me.TabVerticalFlow.ResumeLayout(False)
        CType(Me.VVentSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupVVents.ResumeLayout(False)
        Me.GroupVVents.PerformLayout()
        Me.GroupBox17.ResumeLayout(False)
        Me.GroupBox18.ResumeLayout(False)
        Me.TabGeometry.ResumeLayout(False)
        CType(Me.CompSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupCompartments.ResumeLayout(False)
        Me.GroupCompartments.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupFlowCharacteristics.ResumeLayout(False)
        Me.GroupFlowCharacteristics.PerformLayout()
        CType(Me.CompVariableArea, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupCompSurfaces.ResumeLayout(False)
        Me.GroupCompSurfaces.PerformLayout()
        Me.TabMechanicalFlow.ResumeLayout(False)
        CType(Me.MVentSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupMVents.ResumeLayout(False)
        Me.GroupMVents.PerformLayout()
        Me.GroupBox20.ResumeLayout(False)
        Me.GroupBox20.PerformLayout()
        Me.GroupBox21.ResumeLayout(False)
        Me.GroupBox21.PerformLayout()
        Me.TabFires.ResumeLayout(False)
        Me.TabFires.PerformLayout()
        CType(Me.FireSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupFire.ResumeLayout(False)
        Me.GroupFire.PerformLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabDetection.ResumeLayout(False)
        CType(Me.DetectorSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupDetectors.ResumeLayout(False)
        Me.GroupDetectors.PerformLayout()
        Me.GroupBox33.ResumeLayout(False)
        Me.GroupBox33.PerformLayout()
        Me.TabTargets.ResumeLayout(False)
        CType(Me.TargetSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupTargets.ResumeLayout(False)
        Me.GroupTargets.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox28.ResumeLayout(False)
        Me.GroupBox28.PerformLayout()
        Me.TabHeatTransfer.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.VHeatSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupVHeats.ResumeLayout(False)
        Me.GroupVHeats.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.HHeatSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupHHeats.ResumeLayout(False)
        Me.GroupHHeats.PerformLayout()
        Me.TabMain.ResumeLayout(False)
        Me.TabMaterials.ResumeLayout(False)
        CType(Me.ThermalSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupThermal.ResumeLayout(False)
        Me.GroupThermal.PerformLayout()
        Me.TabVisuals.ResumeLayout(False)
        Me.GroupVisualResolution.ResumeLayout(False)
        Me.GroupVisualResolution.PerformLayout()
        CType(Me.VisualResolution, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        CType(Me.VisualSummary, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.ErrorProvider1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub Cedit_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Main entry point for the program ... handles initialization needed at startup
        Dim Argument As String, SettingsSize, iSet As Integer, CSet As String
        myRecentFiles = New RecentFiles("CFAST")
        Dim RegistryOptions(,) As String
        RegistryOptions = GetAllSettings("CFAST", "Options")

        Try
            SettingsSize = RegistryOptions.GetUpperBound(0)
            If SettingsSize >= 0 Then
                For iSet = 0 To SettingsSize
                    CSet = CType(RegistryOptions(iSet, 0), String)
                    Select Case CSet
                        Case "DetailedOutput"
                            DetailedCFASTOutput = CType(RegistryOptions(iSet, 1), Boolean)
                        Case "ShowCFASTOutput"
                            CommandWindowVisible = CType(RegistryOptions(iSet, 1), Boolean)
                        Case "MassOutput"
                            TotalMassCFASTOutput = CType(RegistryOptions(iSet, 1), Boolean)
                        Case "NetHeatFlux"
                            NetHeatFluxCFASTOutput = CType(RegistryOptions(iSet, 1), Boolean)
                        Case "Validation"
                            ValidationOutput = CType(RegistryOptions(iSet, 1), Boolean)
                    End Select
                Next
            End If
        Catch ex As Exception
        End Try
        UpdateGUI.Menu()
        InitNew()
        Argument = GetCommandLineArg()
        If Argument <> "" Then
            OpenDataFile(Argument)
            UpdateAll()
        End If
    End Sub

    Private Sub MainExit(ByVal sender As System.Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        ' Handle any last minute operations before exiting
        If myEnvironment.FileChanged Then SaveDataFile(True)
        myRecentFiles.Save()
    End Sub

#Region " Simulation Tab "
    ' This section of code handles events related to the environment tab
    Private Sub Env_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EnvSimTime.Leave, EnvTextOutInterval.Leave, EnvSpreadOutInterval.Leave, EnvSmokeviewInterval.Leave, _
        EnvTitle.Leave, EnvIntAmbTemp.Leave, EnvIntAmbElevation.Leave, EnvIntAmbPress.Leave, EnvExtAmbTemp.Leave, EnvExtAmbElevation.Leave, EnvExtAmbPress.Leave, EnvTimeStep.Leave, EnvIntAmbRH.Leave, _
        EnvAdiabatic.CheckedChanged
        If sender Is Me.EnvTitle Then myEnvironment.Title = Me.EnvTitle.Text
        If sender Is Me.EnvSimTime Then myEnvironment.SimulationTime = Val(Me.EnvSimTime.Text)
        If sender Is Me.EnvTextOutInterval Then myEnvironment.OutputInterval = Val(Me.EnvTextOutInterval.Text)
        If sender Is Me.EnvSpreadOutInterval Then myEnvironment.SpreadsheetInterval = Val(Me.EnvSpreadOutInterval.Text)
        If sender Is Me.EnvSmokeviewInterval Then myEnvironment.SmokeviewInterval = Val(Me.EnvSmokeviewInterval.Text)
        If sender Is Me.EnvIntAmbTemp Then myEnvironment.IntAmbTemperature = Val(Me.EnvIntAmbTemp.Text)
        If sender Is Me.EnvIntAmbElevation Then myEnvironment.IntAmbElevation = Val(Me.EnvIntAmbElevation.Text)
        If sender Is Me.EnvIntAmbPress Then myEnvironment.IntAmbPressure = Val(Me.EnvIntAmbPress.Text)
        If sender Is Me.EnvIntAmbRH Then myEnvironment.IntAmbRH = Val(Me.EnvIntAmbRH.Text)
        If sender Is Me.EnvExtAmbTemp Then myEnvironment.ExtAmbTemperature = Val(Me.EnvExtAmbTemp.Text)
        If sender Is Me.EnvExtAmbElevation Then myEnvironment.ExtAmbElevation = Val(Me.EnvExtAmbElevation.Text)
        If sender Is Me.EnvExtAmbPress Then myEnvironment.ExtAmbPressure = Val(Me.EnvExtAmbPress.Text)
        If sender Is Me.EnvTimeStep Then myEnvironment.MaximumTimeStep = Val(EnvTimeStep.Text)
        If sender Is Me.EnvAdiabatic Then
            myEnvironment.AdiabaticWalls = Me.EnvAdiabatic.Checked
            Dim ir As Integer, aCompartment As New Compartment, SavedCompartment As Integer
            SavedCompartment = CurrentCompartment
            For ir = 0 To myCompartments.Count - 1
                If Me.EnvAdiabatic.Checked Then
                    CurrentCompartment = ir
                    aCompartment = myCompartments.Item(ir)
                    aCompartment.SetMaterial("OFF", "OFF", "OFF")
                    myCompartments.Item(CurrentCompartment) = aCompartment
                End If
            Next
            CurrentCompartment = SavedCompartment
            UpdateGUI.Geometry(CurrentCompartment)
        End If
        UpdateGUI.Environment()
    End Sub
#End Region

#Region " Thermal Properties Tab "
    ' This section of code handles events related to the environment tab

    Private Sub Thermal_Changed(sender As Object, e As EventArgs) Handles ThermalLongName.Leave, ThermalShortName.Leave, ThermalConductivity.Leave, ThermalSpecificHeat.Leave, ThermalDensity.Leave, ThermalThickness.Leave, ThermalEmissivity.Leave
        If CurrentThermalProperty >= 0 And CurrentThermalProperty < myThermalProperties.Count Then
            Dim aProperty As New ThermalProperty
            aProperty = myThermalProperties(CurrentThermalProperty)
            If sender Is Me.ThermalLongName Then aProperty.Name = Me.ThermalLongName.Text
            If sender Is Me.ThermalShortName Then aProperty.ShortName = Me.ThermalShortName.Text
            If sender Is Me.ThermalConductivity Then aProperty.Conductivity = Val(Me.ThermalConductivity.Text)
            If sender Is Me.ThermalSpecificHeat Then aProperty.SpecificHeat = Val(Me.ThermalSpecificHeat.Text)
            If sender Is Me.ThermalDensity Then aProperty.Density = Val(Me.ThermalDensity.Text)
            If sender Is Me.ThermalThickness Then aProperty.Thickness = Val(Me.ThermalThickness.Text)
            If sender Is Me.ThermalEmissivity Then aProperty.Emissivity = Val(Me.ThermalEmissivity.Text)
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalSummary.Click
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.ThermalSummary.RowSel - 1
        If index >= 0 And index <= myThermalProperties.Count - 1 Then
            CurrentThermalProperty = index
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles ThermalSummary.AfterSelChange
        ' The currently selected ThermalProperties has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.ThermalSummary.RowSel - 1
        If index >= 0 And index <= myThermalProperties.Count - 1 Then
            CurrentThermalProperty = index
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalAdd.Click
        ' Add a blank material to the end of the thermal property list
        If myThermalProperties.Count + 1 < myThermalProperties.Maximum Then
            Dim aProperty As New ThermalProperty
            aProperty.Name = "New Material " + (myThermalProperties.Count + 1).ToString
            aProperty.ShortName = "NM " + (myThermalProperties.Count + 1).ToString
            myThermalProperties.Add(aProperty)
            CurrentThermalProperty = myThermalProperties.Count - 1
            UpdateGUI.Thermals(CurrentThermalProperty)
        Else
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub ThermalDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalDup.Click
        ' Copy the current thermal property, adding it to the end of the property list
        If CurrentThermalProperty >= 0 And myThermalProperties.Count > 0 And CurrentThermalProperty + 1 < myThermalProperties.Maximum Then
            myThermalProperties.Add(New ThermalProperty)
            myThermalProperties.Copy(CurrentThermalProperty, myThermalProperties.Count - 1)
            CurrentThermalProperty = myThermalProperties.Count - 1
            myThermalProperties(myThermalProperties.Count - 1).Name = "New Material " + myThermalProperties.Count.ToString
            myThermalProperties(myThermalProperties.Count - 1).ShortName = "NM " + (myThermalProperties.Count).ToString
            UpdateGUI.Thermals(CurrentThermalProperty)
        ElseIf CurrentThermalProperty + 1 >= myThermalProperties.Maximum Then
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub ThermalRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalRemove.Click
        Dim ReturnedButton As Integer, TotalConnections As Integer
        If CurrentThermalProperty >= 0 And myThermalProperties.Count > 0 Then
            Dim aProperty As New ThermalProperty
            aProperty = myThermalProperties.Item(CurrentThermalProperty)
            TotalConnections = myThermalProperties.NumberofConnections(aProperty.ShortName)
            If TotalConnections > 0 Then
                ReturnedButton = MessageBox.Show(aProperty.Name + " is used " + TotalConnections.ToString + _
                " times to define materials in the current simulation. These will be changed to Off.", Me.Text, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button2, MessageBoxOptions.DefaultDesktopOnly)
                If ReturnedButton = OK Then
                    myThermalProperties.Remove(CurrentThermalProperty)
                    If CurrentThermalProperty > 0 Then CurrentThermalProperty -= 1
                    myEnvironment.Changed = True
                End If
            Else
                myThermalProperties.Remove(CurrentThermalProperty)
                If CurrentThermalProperty > 0 Then CurrentThermalProperty -= 1
                myEnvironment.Changed = True
            End If
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
#End Region

#Region " Compartments Tab "
    ' This section of code handles events related to the compartments tab
    Private Sub CompAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompAdd.Click
        ' Add a compartment to the end of the list of compartments
        If myCompartments.Count + 1 <= myCompartments.Maximum Then
            Dim aCompartment As New Compartment
            aCompartment.Name = "Comp " + (myCompartments.Count + 1).ToString
            myCompartments.Add(aCompartment)
            CurrentCompartment = myCompartments.Count - 1
            UpdateGUI.Geometry(CurrentCompartment)
        Else
            MessageBox.Show("A maximum of " + Compartment.MaximumCompartments.ToString + " compartments are allowed. New compartment not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub CompDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompDup.Click
        ' Copy the current compartment, adding it to the end of the list of compartments
        If CurrentCompartment >= 0 And myCompartments.Count > 0 And CurrentCompartment + 1 < Compartment.MaximumCompartments Then
            myCompartments.Add(New Compartment)
            myCompartments.Copy(CurrentCompartment, myCompartments.Count - 1)
            CurrentCompartment = myCompartments.Count - 1
            myCompartments(myCompartments.Count - 1).Name = "Comp " + myCompartments.Count.ToString
            UpdateGUI.Geometry(CurrentCompartment)
        ElseIf CurrentCompartment + 1 >= Compartment.MaximumCompartments Then
            MessageBox.Show("A maximum of " + Compartment.MaximumCompartments.ToString + " compartments are allowed. New compartment not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub CompMoveUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompMoveUp.Click
        ' Move the currently selected compartment up one in the list of compartments
        If CurrentCompartment >= 1 And myCompartments.Count > 1 Then
            myCompartments.Swap(CurrentCompartment, CurrentCompartment - 1)
            CurrentCompartment -= 1
            myEnvironment.Changed = True
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub CompMoveDown_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompMoveDown.Click
        ' Move the current compartment down one in the list of compartments
        If CurrentCompartment >= 0 And CurrentCompartment < myCompartments.Count - 1 Then
            myCompartments.Swap(CurrentCompartment, CurrentCompartment + 1)
            CurrentCompartment += 1
            myEnvironment.Changed = True
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub CompRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompRemove.Click
        ' Remove the current compartment from the list of compartments
        Dim ReturnedButton As Integer, TotalConnections As Integer
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            TotalConnections = myHVents.NumberofConnections(CurrentCompartment) + myVVents.NumberofConnections(CurrentCompartment) + myMVents.NumberofConnections(CurrentCompartment) + _
            myDetectors.NumberofConnections(CurrentCompartment) + myTargets.NumberofConnections(CurrentCompartment) + myHHeats.NumberofConnections(CurrentCompartment) + _
            myVHeats.NumberofConnections(CurrentCompartment) + myFires.NumberofConnections(CurrentCompartment) + myVisuals.NumberofConnections(CurrentCompartment)
            If TotalConnections > 0 Then
                ReturnedButton = MessageBox.Show("Compartment " + (CurrentCompartment + 1).ToString + " has " + TotalConnections.ToString + _
                " connection(s) that will be removed.", Me.Text, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button2, MessageBoxOptions.DefaultDesktopOnly)
                If ReturnedButton = OK Then
                    myHVents.RemoveAll(CurrentCompartment) : CurrentHVent = 0
                    myVVents.RemoveAll(CurrentCompartment) : CurrentVVent = 0
                    myMVents.RemoveAll(CurrentCompartment) : CurrentMVent = 0
                    myDetectors.RemoveAll(CurrentCompartment) : CurrentDetector = 0
                    myTargets.RemoveAll(CurrentCompartment) : CurrentTarget = 0
                    myHHeats.RemoveAll(CurrentCompartment) : CurrentHHeat = 0
                    myHHeats.RemoveAll(CurrentCompartment) : CurrentVHeat = 0
                    myFires.RemoveAll(CurrentCompartment) : CurrentFire = 0
                    myVisuals.RemoveAll(CurrentCompartment) : CurrentVisual = 0
                    myCompartments.Remove(CurrentCompartment)
                    If CurrentCompartment > 0 Then CurrentCompartment -= 1
                    myEnvironment.Changed = True
                End If
            Else
                myCompartments.Remove(CurrentCompartment)
                If CurrentCompartment > 0 Then CurrentCompartment -= 1
                myEnvironment.Changed = True
            End If
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub CompSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompSummary.Click, CompSummary.AfterSelChange
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.CompSummary.RowSel - 1
        If index >= 0 And index <= myCompartments.Count - 1 Then
            CurrentCompartment = index
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub Comp_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompName.Leave, CompWidth.Leave, CompDepth.Leave, CompHeight.Leave, CompXPosition.Leave, CompYPosition.Leave, CompZPosition.Leave
        ' Update stored data and summary spreadsheet with newly typed info
        Dim aCompartment As New Compartment
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            aCompartment = myCompartments.Item(CurrentCompartment)
            If sender Is Me.CompName Then aCompartment.Name = Me.CompName.Text
            If sender Is Me.CompWidth Then aCompartment.RoomWidth = Val(Me.CompWidth.Text)
            If sender Is Me.CompDepth Then aCompartment.RoomDepth = Val(Me.CompDepth.Text)
            If sender Is Me.CompHeight Then aCompartment.RoomHeight = Val(Me.CompHeight.Text)
            If sender Is Me.CompXPosition Then aCompartment.RoomOriginX = Val(Me.CompXPosition.Text)
            If sender Is Me.CompYPosition Then aCompartment.RoomOriginY = Val(Me.CompYPosition.Text)
            If sender Is Me.CompZPosition Then aCompartment.RoomOriginZ = Val(Me.CompZPosition.Text)
            myCompartments.Item(CurrentCompartment) = aCompartment
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub Comp_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompCeiling.SelectedIndexChanged, CompWalls.SelectedIndexChanged, CompFloor.SelectedIndexChanged
        Dim aCompartment As New Compartment
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            aCompartment = myCompartments.Item(CurrentCompartment)
            If sender Is Me.CompCeiling Then
                aCompartment.CeilingMaterial = myThermalProperties.GetShortName(sender.text)
            ElseIf sender Is Me.CompWalls Then
                aCompartment.WallMaterial = myThermalProperties.GetShortName(sender.text)
            ElseIf sender Is Me.CompFloor Then
                aCompartment.FloorMaterial = myThermalProperties.GetShortName(sender.text)
            End If
            myCompartments.Item(CurrentCompartment) = aCompartment
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub CompNormal_CheckedChanged(sender As Object, e As EventArgs) Handles CompNormal.CheckedChanged, CompShaft.CheckedChanged, CompCorridor.CheckedChanged
        If CurrentCompartment >= 0 And CurrentCompartment <= myCompartments.Count - 1 Then
            Dim aCompartment As New Compartment
            aCompartment = myCompartments.Item(CurrentCompartment)
            If sender Is Me.CompShaft And Me.CompShaft.Checked = True Then
                aCompartment.Shaft = True
                aCompartment.Hall = False
                myCompartments.Item(CurrentCompartment) = aCompartment
            ElseIf sender Is Me.CompCorridor And Me.CompCorridor.Checked = True Then
                aCompartment.Hall = True
                aCompartment.Shaft = False
                myCompartments.Item(CurrentCompartment) = aCompartment
            ElseIf sender Is Me.CompNormal And Me.CompNormal.Checked = True Then
                aCompartment.Hall = False
                aCompartment.Shaft = False
                myCompartments.Item(CurrentCompartment) = aCompartment
            End If
        End If
    End Sub
    Private Sub CompVariableArea_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles CompVariableArea.BeforeRowColChange
        Dim aCompartment As New Compartment
        Dim numPoints As Integer, ir As Integer
        Dim aArea As Single
        numPoints = UpdateGUI.CountGridPoints(Me.CompVariableArea)
        For ir = 1 To numPoints
            If CType(Me.CompVariableArea(ir, 0), String) + " " = " " Then
                Me.CompVariableArea(ir, 0) = Nothing
                Me.CompVariableArea(ir, 1) = Nothing
            End If
        Next
        ' Copy the values from the spreadsheet to the two vectors for height and area, then put them in the compartment data structure
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            aCompartment = myCompartments.Item(CurrentCompartment)
            numPoints = UpdateGUI.CountGridPoints(Me.CompVariableArea)
            If numPoints = 0 Then
                Dim AreaPoints(0) As Single, HeightPoints(0) As Single
                aCompartment.SetVariableArea(AreaPoints, HeightPoints)
                myCompartments.Item(CurrentCompartment) = aCompartment
                UpdateGUI.Geometry(CurrentCompartment)
            ElseIf numPoints > 0 Then
                Dim AreaPoints(numPoints) As Single, HeightPoints(numPoints) As Single
                For ir = 1 To numPoints
                    HeightPoints(ir) = Val(Me.CompVariableArea(ir, 0))
                    AreaPoints(ir) = Val(Me.CompVariableArea(ir, 1))
                    If AreaPoints(ir) <= 0 Then
                        myUnits.SI = True
                        aArea = aCompartment.RoomWidth * aCompartment.RoomDepth
                        myUnits.SI = False
                        AreaPoints(ir) = myUnits.Convert(UnitsNum.Area).FromSI(aArea)
                    End If
                Next
                aCompartment.SetVariableArea(AreaPoints, HeightPoints)
                myCompartments.Item(CurrentCompartment) = aCompartment
                UpdateGUI.Geometry(CurrentCompartment)
            End If
        End If
    End Sub
#End Region

#Region " Wall Vents Tab "
    ' This section of code handles the events related to the horizontal flow vents tab
    Private Sub HVentAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentAdd.Click
        If myHVents.Count + 1 <= Vent.MaximumHVents Then
            Dim aVent As New Vent
            aVent.VentType = Vent.TypeHVent
            myHVents.Add(aVent)
            CurrentHVent = myHVents.Count - 1
            UpdateGUI.HVents(CurrentHVent)
        Else
            MessageBox.Show("A maximum of " + Vent.MaximumHVents.ToString + " vents are allowed. New vent was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub HVentDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentDup.Click
        ' Copy the current vent, adding it to the end of the list of vents
        If CurrentHVent >= 0 And myHVents.Count > 0 And CurrentHVent + 1 <= myHVents.Maximum And myHVents.Count + 1 <= Vent.MaximumHVents Then
            myHVents.Add(New Vent)
            myHVents.Copy(CurrentHVent, myHVents.Count - 1)
            CurrentHVent = myHVents.Count - 1
            UpdateGUI.HVents(CurrentHVent)
        Else
            MessageBox.Show("A maximum of " + myHVents.Maximum.ToString + " vents are allowed. New vent not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub HVentMoveUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentMoveUp.Click
        ' Move the currently selected hvent up one in the list of hvents
        If CurrentHVent >= 1 And myHVents.Count > 1 Then
            Dim aVent As New Vent
            aVent = myHVents(CurrentHVent)
            myHVents(CurrentHVent) = myHVents(CurrentHVent - 1)
            myHVents(CurrentHVent - 1) = aVent
            CurrentHVent -= 1
            myEnvironment.Changed = True
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVentMoveDown_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentMoveDown.Click
        ' Move the current hvent down one in the list of hvents
        If CurrentHVent >= 0 And CurrentHVent < myHVents.Count - 1 Then
            Dim aVent As New Vent
            aVent = myHVents(CurrentHVent)
            myHVents(CurrentHVent) = myHVents(CurrentHVent + 1)
            myHVents(CurrentHVent + 1) = aVent
            CurrentHVent += 1
            myEnvironment.Changed = True
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVentRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentRemove.Click
        ' Remove the current hvent from the list of hvents
        If CurrentHVent >= 0 And myHVents.Count > 0 Then
            myHVents.Remove(CurrentHVent)
            If CurrentHVent > 0 Then CurrentHVent -= 1
            myEnvironment.Changed = True
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVentSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentSummary.Click
        ' The currently selected hvent has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.HVentSummary.RowSel - 1
        If index >= 0 And index <= myHVents.Count - 1 Then
            CurrentHVent = index
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles HVentSummary.AfterSelChange
        ' The currently selected hvent has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = Me.HVentSummary.RowSel - 1
        If index >= 0 And index <= myHVents.Count - 1 Then
            CurrentHVent = index
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentOffset1.Leave, HVentSill.Leave, HVentSoffit.Leave, HVentWidth.Leave, HVentInitialFraction.Leave, HVentFace.SelectedIndexChanged, HVentComp1.SelectedIndexChanged, HVentComp2.SelectedIndexChanged, HVentFinalFraction.Leave, HVentFractionTime.Leave
        Dim aVent As New Vent
        If CurrentHVent >= 0 And myHVents.Count > 0 Then
            aVent = myHVents.Item(CurrentHVent)
            If sender Is Me.HVentOffset1 Then aVent.FirstOffset = Val(Me.HVentOffset1.Text)
            If sender Is Me.HVentSill Then aVent.Sill = Val(Me.HVentSill.Text)
            If sender Is Me.HVentSoffit Then aVent.Soffit = Val(Me.HVentSoffit.Text)
            If sender Is Me.HVentWidth Then aVent.Width = Val(Me.HVentWidth.Text)
            If sender Is Me.HVentInitialFraction Then aVent.InitialOpening = Val(Me.HVentInitialFraction.Text)
            If sender Is Me.HVentFractionTime Then aVent.FinalOpeningTime = Val(Me.HVentFractionTime.Text)
            If sender Is Me.HVentFinalFraction Then aVent.FinalOpening = Val(Me.HVentFinalFraction.Text)
            If sender Is Me.HVentFace Then aVent.Face = Me.HVentFace.SelectedIndex + 1
            If sender Is Me.HVentComp1 Then aVent.FirstCompartment = Me.HVentComp1.SelectedIndex - 1
            If sender Is Me.HVentComp2 Then aVent.SecondCompartment = Me.HVentComp2.SelectedIndex - 1
            ' CFast expects the from compartment to be the lower number of the pair and the outside to be the to compartment
            myHVents(CurrentHVent) = aVent
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub

#End Region

#Region " Floor/Ceiling Vents Tab "
    ' This section of code handles the events related to the vertical flow vents tab
    Private Sub VVentAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentAdd.Click
        If myVVents.Count + 1 <= Vent.MaximumVVents Then
            Dim aVent As New Vent
            aVent.VentType = Vent.TypeVVent
            myVVents.Add(aVent)
            CurrentVVent = myVVents.Count - 1
            UpdateGUI.VVents(CurrentVVent)
        Else
            MessageBox.Show("A maximum of " + myVVents.Maximum.ToString + " vents are allowed. New vent was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub VVentDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentDup.Click
        ' Copy the current vent, adding it to the end of the list of vents
        If CurrentVVent >= 0 And myVVents.Count > 0 And CurrentVVent + 1 <= myVVents.Maximum And myVVents.Count + 1 <= Vent.MaximumVVents Then
            myVVents.Add(New Vent)
            myVVents.Copy(CurrentVVent, myVVents.Count - 1)
            CurrentVVent = myVVents.Count - 1
            UpdateGUI.VVents(CurrentVVent)
        Else
            MessageBox.Show("A maximum of " + myVVents.Maximum.ToString + " vents are allowed. New vent not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub VVentRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentRemove.Click
        ' Remove the current vvent from the list of hvents
        If CurrentVVent >= 0 And myVVents.Count > 0 Then
            myVVents.Remove(CurrentVVent)
            If CurrentVVent > 0 Then CurrentVVent -= 1
            myEnvironment.Changed = True
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
    Private Sub VVentSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentSummary.Click
        ' The currently selected vvent has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.VVentSummary.RowSel - 1
        If index >= 0 And index <= myVVents.Count - 1 Then
            CurrentVVent = index
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
    Private Sub VVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles VVentSummary.AfterSelChange
        ' The currently selected vvent has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.VVentSummary.RowSel - 1
        If index >= 0 And index <= myVVents.Count - 1 Then
            CurrentVVent = index
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
    Private Sub VVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentCompTop.SelectedIndexChanged, VVentCompBottom.SelectedIndexChanged, VVentArea.Leave, VVentInitialFraction.Leave, VVentFractionTime.Leave, VVentFinalFraction.Leave, VVentShape.SelectedIndexChanged
        Dim aVent As New Vent
        If CurrentVVent >= 0 And myVVents.Count > 0 Then
            aVent = myVVents.Item(CurrentVVent)
            If sender Is Me.VVentArea Then aVent.Area = Val(Me.VVentArea.Text)
            If sender Is Me.VVentShape Then aVent.Shape = Me.VVentShape.SelectedIndex + 1
            If sender Is Me.VVentCompTop Then aVent.FirstCompartment = Me.VVentCompTop.SelectedIndex - 1
            If sender Is Me.VVentCompBottom Then aVent.SecondCompartment = Me.VVentCompBottom.SelectedIndex - 1
            If sender Is Me.VVentInitialFraction Then aVent.InitialOpening = Val(Me.VVentInitialFraction.Text)
            If sender Is Me.VVentFractionTime Then aVent.FinalOpeningTime = Val(Me.VVentFractionTime.Text)
            If sender Is Me.VVentFinalFraction Then aVent.FinalOpening = Val(Me.VVentFinalFraction.Text)
            myVVents(CurrentVVent) = aVent
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
#End Region

#Region "Mechanical Vents Tab "

    ' This section of code handles the events related to the mechanical flow vents tab
    Private Sub MVentAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentAdd.Click
        If myMVents.Count + 1 <= Vent.MaximumMVents Then
            Dim aVent As New Vent
            aVent.VentType = Vent.TypeMVent
            myMVents.Add(aVent)
            CurrentMVent = myMVents.Count - 1
            UpdateGUI.MVents(CurrentMVent)
        Else
            MessageBox.Show("A maximum of " + myMVents.Maximum.ToString + " vents are allowed. New vent was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub MVentDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentDup.Click
        ' Copy the current vent, adding it to the end of the list of vents
        If CurrentMVent >= 0 And myMVents.Count > 0 And CurrentMVent + 1 <= myMVents.Maximum And myMVents.Count + 1 <= Vent.MaximumMVents Then
            myMVents.Add(New Vent)
            myMVents.Copy(CurrentMVent, myMVents.Count - 1)
            CurrentMVent = myMVents.Count - 1
            UpdateGUI.MVents(CurrentMVent)
        Else
            MessageBox.Show("A maximum of " + myMVents.Maximum.ToString + " vents are allowed. New vent not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub MVentRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentRemove.Click
        ' Remove the current vvent from the list of mvents
        If CurrentMVent >= 0 And myMVents.Count > 0 Then
            myMVents.Remove(CurrentMVent)
            If CurrentMVent > 0 Then CurrentMVent -= 1
            myEnvironment.Changed = True
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
    Private Sub MVentSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentSummary.Click
        ' The currently selected mvent has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.MVentSummary.RowSel - 1
        If index >= 0 And index <= myMVents.Count - 1 Then
            CurrentMVent = index
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
    Private Sub mVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles MVentSummary.AfterSelChange
        ' The currently selected mvent has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.MVentSummary.RowSel - 1
        If index >= 0 And index <= myMVents.Count - 1 Then
            CurrentMVent = index
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
    Private Sub mVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentFromComp.SelectedIndexChanged, MventToComp.SelectedIndexChanged, MVentFromOrientation.SelectedIndexChanged, MVentToOrientation.SelectedIndexChanged, MVentFromArea.Leave, MVentFromHeight.Leave, MVentToArea.Leave, MVentToHeight.Leave, MVentFlow.Leave, MVentDropoff.Leave, MVentZero.Leave, MVentInitialFraction.Leave, MVentFractionTime.Leave, MVentFinalFraction.Leave, MVentFilterEfficiency.Leave, MVentFilterTime.Leave
        Dim aVent As New Vent
        If CurrentMVent >= 0 And myMVents.Count > 0 Then
            aVent = myMVents.Item(CurrentMVent)
            If sender Is Me.MVentFromComp Then aVent.FirstCompartment = Me.MVentFromComp.SelectedIndex - 1
            If sender Is Me.MVentFromArea Then aVent.FirstArea = Val(Me.MVentFromArea.Text)
            If sender Is Me.MVentFromHeight Then aVent.FirstCenterHeight = Val(Me.MVentFromHeight.Text)
            If sender Is Me.MVentFromOrientation Then aVent.FirstOrientation = Me.MVentFromOrientation.SelectedIndex + 1

            If sender Is Me.MventToComp Then aVent.SecondCompartment = Me.MventToComp.SelectedIndex - 1
            If sender Is Me.MVentToArea Then aVent.SecondArea = Val(Me.MVentToArea.Text)
            If sender Is Me.MVentToHeight Then aVent.SecondCenterHeight = Val(Me.MVentToHeight.Text)
            If sender Is Me.MVentToOrientation Then aVent.SecondOrientation = Me.MVentToOrientation.SelectedIndex + 1

            If sender Is Me.MVentFlow Then aVent.FlowRate = Val(Me.MVentFlow.Text)
            If sender Is Me.MVentDropoff Then aVent.BeginFlowDropoff = Val(Me.MVentDropoff.Text)
            If sender Is Me.MVentZero Then aVent.ZeroFlow = Val(Me.MVentZero.Text)

            If sender Is Me.MVentInitialFraction Then aVent.InitialOpening = Val(Me.MVentInitialFraction.Text)
            If sender Is Me.MVentFractionTime Then aVent.FinalOpeningTime = Val(Me.MVentFractionTime.Text)
            If sender Is Me.MVentFinalFraction Then aVent.FinalOpening = Val(Me.MVentFinalFraction.Text)
            If sender Is Me.MVentFilterEfficiency Then aVent.FilterEfficiency = Val(Me.MVentFilterEfficiency.Text)
            If sender Is Me.MVentFilterTime Then aVent.FilterTime = Val(Me.MVentFilterTime.Text)

            myMVents(CurrentMVent) = aVent
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
#End Region

#Region "Fires Tab "
    ' This section of code handles the events related to the fires tab
    Private Sub FireAdd_Click(sender As System.Object, e As System.EventArgs) Handles FireAdd.Click, FireAddt2.Click
        If myFires.Count + 1 <= Fire.MaximumFires Then
            If sender Is FireAddt2 Then
                Dim t2FireDialog As New t2Fire
                Dim iReturn As Integer
                iReturn = t2FireDialog.ShowDialog(Me)
                If iReturn = Windows.Forms.DialogResult.OK Then
                    Dim aFire As New Fire(t2FireDialog.GrowthTime, t2FireDialog.PeakHRR, t2FireDialog.SteadyTime, t2FireDialog.DecayTime)
                    myFires.Add(aFire)
                    UpdateGUI.Fires(CurrentFire)
                End If
            ElseIf sender Is FireAdd Then
                Dim aFire As New Fire()
                myFires.Add(aFire)
                UpdateGUI.Fires(CurrentFire)
            End If
        End If
    End Sub
    Private Sub FireDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireDup.Click
        ' Copy the current vent, adding it to the end of the list of vents
        If CurrentFire >= 0 And myFires.Count > 0 And CurrentFire + 1 <= myFires.Maximum And myFires.Count + 1 <= Fire.MaximumFires Then
            myFires.Add(New Fire)
            myFires.Copy(CurrentFire, myFires.Count - 1)
            CurrentFire = myFires.Count - 1
            UpdateGUI.Fires(CurrentFire)
        ElseIf CurrentFire + 1 > myFires.Maximum Then
            MessageBox.Show("A maximum of " + myFires.Maximum.ToString + " Fires are allowed. New fire not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub FireRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireRemove.Click
        ' Remove the current Fire from the list of hvents
        If CurrentFire >= 0 And myFires.Count > 0 Then
            myFires.Remove(CurrentFire)
            If CurrentFire > 0 Then
                CurrentFire -= 1
            End If
            myEnvironment.Changed = True
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireSummary.Click
        ' The currently selected Fire has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.FireSummary.RowSel - 1
        If index >= 0 And index <= myFires.Count - 1 Then
            CurrentFire = index
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireSummary.AfterSelChange
        ' The currently selected Fire has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.FireSummary.RowSel - 1
        If index >= 0 And index <= myFires.Count - 1 Then
            CurrentFire = index
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireComp.SelectedIndexChanged, FireIgnitionCriteria.SelectedIndexChanged, FireXPosition.Leave, FireYPosition.Leave, FireZPosition.Leave, FireXNormal.Leave, FireYNormal.Leave, FireZNormal.Leave, FireIgnitionValue.Leave, FireLOL.Leave, FireIgnitionTemperature.Leave, FireName.Leave
        Dim aFire As New Fire
        Dim aFireTimeSeries(12, 0) As Single, numPoints As Integer
        Dim ir As Integer
        If sender Is Me.FireLOL Then myEnvironment.LowerOxygenLimit = Val(Me.FireLOL.Text)
        If sender Is Me.FireIgnitionTemperature Then myEnvironment.IgnitionTemp = Val(Me.FireIgnitionTemperature.Text)
        If CurrentFire >= 0 And myFires.Count > 0 Then
            aFire = myFires(CurrentFire)
            If sender Is Me.FireComp Then
                aFire.Compartment = Me.FireComp.SelectedIndex
                If Val(Me.FireXPosition.Text) = -1 Then aFire.XPosition = Val(Me.FireXPosition.Text)
                If Val(Me.FireYPosition.Text) = -1 Then aFire.YPosition = Val(Me.FireYPosition.Text)
                If Val(Me.FireZPosition.Text) = -1 Then aFire.ZPosition = Val(Me.FireZPosition.Text)
            End If
            If sender Is Me.FireXPosition Then aFire.XPosition = Val(Me.FireXPosition.Text)
            If sender Is Me.FireYPosition Then aFire.YPosition = Val(Me.FireYPosition.Text)
            If sender Is Me.FireZPosition Then aFire.ZPosition = Val(Me.FireZPosition.Text)
            If sender Is Me.FireIgnitionCriteria Then aFire.IgnitionType = Me.FireIgnitionCriteria.SelectedIndex
            If sender Is Me.FireXNormal Then aFire.XNormal = Val(Me.FireXNormal.Text)
            If sender Is Me.FireYNormal Then aFire.YNormal = Val(Me.FireYNormal.Text)
            If sender Is Me.FireZNormal Then aFire.ZNormal = Val(Me.FireZNormal.Text)
            If sender Is Me.FireIgnitionValue Then aFire.IgnitionValue = Val(Me.FireIgnitionValue.Text)
            If sender Is Me.FireName Then
                aFire.Name = Me.FireName.Text
            End If
            If sender Is Me.FireMaterial Then aFire.Material = myThermalProperties.GetShortName(Me.FireMaterial.Text)
            If sender Is Me.FireC Then
                If Val(Me.FireC.Text) > 0 Then
                    aFire.ChemicalFormula(formula.C) = Val(Me.FireC.Text)
                Else
                    aFire.ChemicalFormula(formula.C) = 1
                End If
            End If
            If sender Is Me.FireH Then aFire.ChemicalFormula(formula.H) = Val(Me.FireH.Text)
            If sender Is Me.FireO Then aFire.ChemicalFormula(formula.O) = Val(Me.FireO.Text)
            If sender Is Me.FireN Then aFire.ChemicalFormula(formula.N) = Val(Me.FireN.Text)
            If sender Is Me.FireCl Then aFire.ChemicalFormula(formula.Cl) = Val(Me.FireCl.Text)
            If sender Is Me.FireC Or sender Is Me.FireH Or sender Is Me.FireO Or sender Is Me.FireN Or sender Is Me.FireCl Then
                numPoints = CountGridPoints(Me.FireDataSS)
                For ir = 1 To numPoints
                    ' Note that though these are calculated, they are actually hidden from the user since they only support the older format CFAST input
                    Me.FireDataSS(ir, Fire.FireHC) = aFire.ChemicalFormula(formula.H) * 1.00794 / (aFire.ChemicalFormula(formula.C) * 12.0107)
                    Me.FireDataSS(ir, Fire.FireHCN) = (1.00794 + 12.0107 + 14.01) / 1000.0 / aFire.MolarMass * aFire.ChemicalFormula(formula.N)
                    Me.FireDataSS(ir, Fire.FireHCl) = (1.00794 + 35.453) / 1000.0 / aFire.MolarMass * aFire.ChemicalFormula(formula.Cl)
                Next
                CopyFireData(aFire)
            End If
            If sender Is Me.FireHoC Then
                If Val(Me.FireHoC.Text) <> aFire.HeatofCombustion Then
                    aFire.HeatofCombustion = Val(Me.FireHoC.Text)
                    numPoints = CountGridPoints(Me.FireDataSS)
                    For ir = 1 To numPoints
                        Me.FireDataSS(ir, Fire.FireMdot) = Me.FireDataSS(ir, Fire.FireHRR) / aFire.HeatofCombustion
                    Next
                End If
                CopyFireData(aFire)
            End If
            If sender Is Me.FireSootYield Then
                numPoints = CountGridPoints(Me.FireDataSS)
                For ir = 1 To numPoints
                    Me.FireDataSS(ir, Fire.FireSoot) = Val(Me.FireSootYield.Text)
                Next
                CopyFireData(aFire)
            End If
            If sender Is Me.FireCOYield Then
                numPoints = CountGridPoints(Me.FireDataSS)
                For ir = 1 To numPoints
                    If Me.FireCOYield.Text <> "" Then
                        Me.FireDataSS(ir, Fire.FireCO) = Val(Me.FireCOYield.Text)
                    Else
                        Me.FireDataSS(ir, Fire.FireCO) = (0.012 * Val(Me.FireC.Text) / aFire.MolarMass * 0.0014 + 0.37 * Me.FireDataSS(ir, Fire.FireSoot))
                    End If
                Next
                CopyFireData(aFire)
            End If
            If sender Is Me.FireRadiativeFraction Then aFire.RadiativeFraction = Val(Me.FireRadiativeFraction.Text)

            If CurrentFire >= 0 Then myFires(CurrentFire) = aFire
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.BeforeRowColChange
        Dim aFire As New Fire
        Dim numPoints As Integer
        If CurrentFire >= 0 And myFires.Count > 0 Then
            aFire = myFires(CurrentFire)
            numPoints = CountGridPoints(Me.FireDataSS)
            ' Copy the values from the spreadsheet to the array for fire data, then put them in the FireObject data structure
            If Me.FireDataSS.ColSel = Fire.FireHRR Then
                Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireMdot) = Me.FireDataSS(Me.FireDataSS.RowSel, Fire.FireHRR) / aFire.HeatofCombustion
            End If
            CopyFireData(aFire)
            myFires(CurrentFire) = aFire
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.AfterRowColChange
        UpdateGUI.Fires(CurrentFire)
    End Sub
    Private Sub FireData_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FireDataSS.KeyDown
        Dim aChar As String
        aChar = e.KeyCode.ToString
        If e.Control Then
            If aChar = "Delete" Then
                If FireDataSS.Row > 0 And FireDataSS.Col >= 0 Then
                    FireDataSS(FireDataSS.Row, FireDataSS.Col) = " "
                End If
            End If
        End If
        If e.Alt Then
            If aChar = "Delete" Then
                If FireDataSS.Row > 0 And FireDataSS.Col >= 0 Then
                    If CurrentFire >= 0 And CurrentFire < myFires.Count Then
                        Dim numPoints As Integer, i As Integer, j As Integer
                        numPoints = CountGridPoints(Me.FireDataSS)
                        If numPoints > FireDataSS.Row Then
                            For i = FireDataSS.Row To numPoints
                                For j = 0 To FireDataSS.Cols.Count - 1
                                    FireDataSS(i, j) = FireDataSS(i + 1, j)
                                Next
                            Next
                        End If
                        For j = 0 To FireDataSS.Cols.Count - 1
                            FireDataSS(numPoints, j) = Nothing
                        Next
                        Dim aFire As New Fire
                        aFire = myFires(CurrentFire)
                        CopyFireData(aFire)
                        myFires(CurrentFire) = aFire
                        UpdateGUI.Fires(CurrentFire)
                    End If
                End If
            End If
            If aChar = "Insert" Then
                If FireDataSS.Row > 0 Then
                    Dim numPoints As Integer, i As Integer, j As Integer
                    numPoints = Me.CountGridPoints(Me.FireDataSS)
                    If numPoints > FireDataSS.Row Then
                        For i = numPoints To FireDataSS.Row Step -1
                            For j = 0 To FireDataSS.Cols.Count - 1
                                FireDataSS(i + 1, j) = FireDataSS(i, j)
                            Next
                        Next
                        For j = 0 To FireDataSS.Cols.Count - 1
                            FireDataSS(FireDataSS.Row, j) = 0
                        Next
                    End If

                End If
            End If
        End If
    End Sub

    Private ReadOnly Property CountGridPoints(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid) As Integer
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
    Private Sub CopyFireData(ByVal aFire As Fire)
        ' Copies time dependent data from the display spreadsheet to the appropriate fire object data array
        Dim numPoints As Integer, ir As Integer, ic As Integer
        numPoints = CountGridPoints(Me.FireDataSS)
        If numPoints > 0 Then
            Dim aFireTimeSeries(12, numPoints - 1) As Single
            For ir = 0 To numPoints - 1
                For ic = 0 To 12
                    aFireTimeSeries(ic, ir) = Val(Me.FireDataSS(ir + 1, ic))
                Next
            Next
            aFire.SetFireData(aFireTimeSeries)
        End If
    End Sub
#End Region

#Region " Targets Tab "
    ' This section of code handles the events related to the targets tab
    Private Sub TargetAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetAdd.Click
        If myTargets.Count + 1 <= Target.MaximumTargets Then
            Dim aTarget As New Target
            aTarget.Type = Target.TypeTarget
            myTargets.Add(aTarget)
            CurrentTarget = myTargets.Count - 1
            UpdateGUI.Targets(CurrentTarget)
        Else
            MessageBox.Show("A maximum of " + myTargets.Maximum.ToString + " targets are allowed. New target was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub TargetDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetDup.Click
        ' Copy the current target, adding it to the end of the list of targets
        If CurrentTarget >= 0 And myTargets.Count > 0 And CurrentTarget + 1 <= myTargets.Maximum And myTargets.Count + 1 <= Target.MaximumTargets Then
            myTargets.Add(New Target)
            myTargets.Copy(CurrentTarget, myTargets.Count - 1)
            CurrentTarget = myTargets.Count - 1
            UpdateGUI.Targets(CurrentTarget)
        Else
            MessageBox.Show("A maximum of " + myTargets.Maximum.ToString + " Targets are allowed. New Target not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub TargetRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetRemove.Click
        ' Remove the current vvent from the list of Targets
        If CurrentTarget >= 0 And myTargets.Count > 0 Then
            myTargets.Remove(CurrentTarget)
            If CurrentTarget > 0 Then CurrentTarget -= 1
            myEnvironment.Changed = True
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
    Private Sub TargetSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetSummary.Click
        ' The currently selected Target has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.TargetSummary.RowSel - 1
        If index >= 0 And index <= myTargets.Count - 1 Then
            CurrentTarget = index
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
    Private Sub TargetSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles TargetSummary.AfterSelChange
        ' The currently selected Target has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.TargetSummary.RowSel - 1
        If index >= 0 And index <= myTargets.Count - 1 Then
            CurrentTarget = index
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
    Private Sub Target_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetComp.SelectedIndexChanged, TargetMaterial.SelectedIndexChanged, TargetSolutionMethod.SelectedIndexChanged, TargetSolutionThickness.SelectedIndexChanged, TargetXPosition.Leave, TargetYPosition.Leave, TargetZPosition.Leave, TargetXNormal.Leave, TargetYNormal.Leave, TargetZNormal.Leave, TargetNormalCalc.SelectedIndexChanged, TargetInternalLocation.Leave
        Dim aTarget As New Target, numFires As Integer, i As Integer
        If CurrentTarget >= 0 And myTargets.Count > 0 Then
            aTarget = myTargets.Item(CurrentTarget)
            If sender Is Me.TargetComp Then
                aTarget.Compartment = Me.TargetComp.SelectedIndex
                If Val(Me.TargetXPosition.Text) = -1 Then aTarget.XPosition = Val(Me.TargetXPosition.Text)
                If Val(Me.TargetYPosition.Text) = -1 Then aTarget.YPosition = Val(Me.TargetYPosition.Text)
                If Val(Me.TargetZPosition.Text) = -1 Then aTarget.ZPosition = Val(Me.TargetZPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is Me.TargetMaterial Then aTarget.Material = myThermalProperties.GetShortName(sender.text)
            If sender Is Me.TargetSolutionMethod Then aTarget.SolutionMethod = Me.TargetSolutionMethod.SelectedIndex
            If sender Is Me.TargetSolutionThickness Then
                aTarget.SolutionThickness = Me.TargetSolutionThickness.SelectedIndex
                If aTarget.SolutionThickness = 2 Then aTarget.SolutionMethod = 1
            End If
            If sender Is Me.TargetInternalLocation Then aTarget.InternalLocation = Val(Me.TargetInternalLocation.Text)
            If sender Is Me.TargetXPosition Then
                aTarget.XPosition = Val(Me.TargetXPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is Me.TargetYPosition Then
                aTarget.YPosition = Val(Me.TargetYPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is Me.TargetZPosition Then
                aTarget.ZPosition = Val(Me.TargetZPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is Me.TargetXNormal Then aTarget.XNormal = Val(Me.TargetXNormal.Text)
            If sender Is Me.TargetYNormal Then aTarget.YNormal = Val(Me.TargetYNormal.Text)
            If sender Is Me.TargetZNormal Then aTarget.ZNormal = Val(Me.TargetZNormal.Text)
            If sender Is Me.TargetNormalCalc Then
                If Me.TargetNormalCalc.Text = "Right Wall" Then
                    aTarget.XNormal = 1
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 0
                ElseIf Me.TargetNormalCalc.Text = "Left Wall" Then
                    aTarget.XNormal = -1
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 0
                ElseIf Me.TargetNormalCalc.Text = "Rear Wall" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 1
                    aTarget.ZNormal = 0
                ElseIf Me.TargetNormalCalc.Text = "Front Wall" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = -1
                    aTarget.ZNormal = 0
                ElseIf Me.TargetNormalCalc.Text = "Floor" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 0
                    aTarget.ZNormal = -1
                ElseIf Me.TargetNormalCalc.Text = "Ceiling" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 1
                Else
                    numFires = myFires.Count
                    If numFires > 0 Then
                        Dim aFire As Fire, aFireObject As Fire
                        For i = 1 To numFires
                            aFire = myFires(i - 1)
                            If aTarget.Compartment = aFire.Compartment Then
                                aFireObject = myFireObjects(aFire.FireObject)
                                If Me.TargetNormalCalc.Text = "Fire " + i.ToString + ", " + aFireObject.Name Then
                                    Dim Hypotenuse As Single
                                    Hypotenuse = Math.Sqrt((aFire.XPosition - aTarget.XPosition) ^ 2 + (aFire.YPosition - aTarget.YPosition) ^ 2 + (aFire.ZPosition - aTarget.ZPosition) ^ 2)
                                    If Hypotenuse <> 0 Then
                                        aTarget.XNormal = (aFire.XPosition - aTarget.XPosition) / Hypotenuse
                                        aTarget.YNormal = (aFire.YPosition - aTarget.YPosition) / Hypotenuse
                                        aTarget.ZNormal = (aFire.ZPosition - aTarget.ZPosition) / Hypotenuse
                                    End If
                                End If
                            End If
                        Next
                    End If
                End If
            End If
            myTargets(CurrentTarget) = aTarget
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
#End Region

#Region "Detectors Tab "
    ' This section of code handles the events related to the detector tab
    Private Sub DetectorAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorAdd.Click
        If myDetectors.Count + 1 <= Target.MaximumTargets Then
            Dim aDetector As New Target
            aDetector.Type = Target.TypeDetector
            myDetectors.Add(aDetector)
            CurrentDetector = myDetectors.Count - 1
            UpdateGUI.Detectors(CurrentDetector)
        Else
            MessageBox.Show("A maximum of " + myDetectors.Maximum.ToString + " Detectors are allowed. New Detector was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub DetectorDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorDup.Click
        ' Copy the current Detector, adding it to the end of the list of Detectors
        If CurrentDetector >= 0 And myDetectors.Count > 0 And CurrentDetector + 1 <= myDetectors.Maximum And myDetectors.Count + 1 <= Target.MaximumTargets Then
            myDetectors.Add(New Target)
            myDetectors.Copy(CurrentDetector, myDetectors.Count - 1)
            CurrentDetector = myDetectors.Count - 1
            UpdateGUI.Detectors(CurrentDetector)
        Else
            MessageBox.Show("A maximum of " + myDetectors.Maximum.ToString + " Detectors are allowed. New Detector not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub DetectorRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorRemove.Click
        ' Remove the current vvent from the list of Detectors
        If CurrentDetector >= 0 And myDetectors.Count > 0 Then
            myDetectors.Remove(CurrentDetector)
            If CurrentDetector > 0 Then CurrentDetector -= 1
            myEnvironment.Changed = True
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub DetectorMoveUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorMoveUp.Click
        ' Move the currently selected detector up one in the list of hvents
        If CurrentDetector >= 1 And myDetectors.Count > 1 Then
            Dim aDetector As New Target
            aDetector = myDetectors(CurrentDetector)
            myDetectors(CurrentDetector) = myDetectors(CurrentDetector - 1)
            myDetectors(CurrentDetector - 1) = aDetector
            CurrentDetector -= 1
            myEnvironment.Changed = True
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub DetectorMoveDown_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorMoveDown.Click
        ' Move the current detector down one in the list of hvents
        If CurrentDetector >= 0 And CurrentDetector < myDetectors.Count - 1 Then
            Dim aDetector As New Target
            aDetector = myDetectors(CurrentDetector)
            myDetectors(CurrentDetector) = myDetectors(CurrentDetector + 1)
            myDetectors(CurrentDetector + 1) = aDetector
            CurrentDetector += 1
            myEnvironment.Changed = True
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub DetectorSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorSummary.Click
        ' The currently selected Detector has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        index = Me.DetectorSummary.RowSel - 1
        If index >= 0 And index <= myDetectors.Count - 1 Then
            CurrentDetector = index
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub DetectorSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles DetectorSummary.AfterSelChange
        ' The currently selected Detector has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = Me.DetectorSummary.RowSel - 1
        If index >= 0 And index <= myDetectors.Count - 1 Then
            CurrentDetector = index
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub Detector_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorComp.SelectedIndexChanged, DetectorType.SelectedIndexChanged, DetectorActivation.Leave, DetectorXPosition.Leave, DetectorYPosition.Leave, DetectorZPosition.Leave, DetectorSprayDensity.Leave, DetectorRTI.Leave
        Dim aDetector As New Target
        If CurrentDetector >= 0 And myDetectors.Count > 0 Then
            aDetector = myDetectors(CurrentDetector)
            If sender Is Me.DetectorType Then aDetector.DetectorType = Me.DetectorType.SelectedIndex
            If sender Is Me.DetectorComp Then
                aDetector.Compartment = Me.DetectorComp.SelectedIndex
                If Val(Me.DetectorXPosition.Text) = -1 Then aDetector.XPosition = Val(Me.DetectorXPosition.Text)
                If Val(Me.DetectorYPosition.Text) = -1 Then aDetector.YPosition = Val(Me.DetectorYPosition.Text)
                If Val(Me.DetectorZPosition.Text) = -1 Then aDetector.ZPosition = Val(Me.DetectorZPosition.Text)
            End If
            If sender Is Me.DetectorActivation Then aDetector.ActivationTemperature = Val(Me.DetectorActivation.Text)
            If sender Is Me.DetectorXPosition Then aDetector.XPosition = Val(Me.DetectorXPosition.Text)
            If sender Is Me.DetectorYPosition Then aDetector.YPosition = Val(Me.DetectorYPosition.Text)
            If sender Is Me.DetectorZPosition Then aDetector.ZPosition = Val(Me.DetectorZPosition.Text)
            If sender Is Me.DetectorRTI Then aDetector.RTI = Val(Me.DetectorRTI.Text)
            If sender Is Me.DetectorSprayDensity Then aDetector.SprayDensity = Val(Me.DetectorSprayDensity.Text)
            myDetectors(CurrentDetector) = aDetector
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
#End Region

#Region " Surface Connections Tab "
    ' This section of code handles the events related to the surface connections tab
    Private Sub HeatAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HHeatAdd.Click, VHeatAdd.Click
        ' Add a heat transfer connection
        If sender Is HHeatAdd Then
            If myHHeats.Count + 1 <= Vent.MaximumHHeats Then
                Dim aHeat As New Vent
                aHeat.VentType = Vent.TypeHHeat
                myHHeats.Add(aHeat)
                CurrentHHeat = myHHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myHHeats.Maximum.ToString + " heat transfer connections are allowed. New connection was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        ElseIf sender Is VHeatAdd Then
            If myVHeats.Count + 1 <= Vent.MaximumVHeats Then
                Dim aHeat As New Vent
                aHeat.VentType = Vent.TypeVHeat
                myVHeats.Add(aHeat)
                CurrentVHeat = myVHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myVHeats.Maximum.ToString + " heat transfer connections are allowed. New connection was not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        End If
    End Sub
    Private Sub HeatDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HHeatDup.Click, VHeatDup.Click
        ' Copy the current connection, adding it to the end of the list of heat connections
        If sender Is HHeatDup Then
            If CurrentHHeat >= 0 And myHHeats.Count > 0 And CurrentHHeat + 1 <= myHHeats.Maximum And myHHeats.Count + 1 <= Vent.MaximumHHeats Then
                myHHeats.Add(New Vent)
                myHHeats.Copy(CurrentHHeat, myHHeats.Count - 1)
                CurrentHHeat = myHHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myHHeats.Maximum.ToString + " heat transfer connections are allowed. New connection not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        ElseIf sender Is VHeatDup Then
            If CurrentVHeat >= 0 And myVHeats.Count > 0 And CurrentVHeat + 1 <= myVHeats.Maximum And myVHeats.Count + 1 <= Vent.MaximumHHeats Then
                myVHeats.Add(New Vent)
                myVHeats.Copy(CurrentVHeat, myVHeats.Count - 1)
                CurrentVHeat = myVHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myVHeats.Maximum.ToString + " heat transfer connections are allowed. New connection not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        End If
    End Sub
    Private Sub HeatRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HHeatRemove.Click, VHeatRemove.Click
        ' Remove the current connection from the list of heat connections
        If sender Is HHeatRemove Then
            If CurrentHHeat >= 0 And myHHeats.Count > 0 Then
                myHHeats.Remove(CurrentHHeat)
                If CurrentHHeat > 0 Then CurrentHHeat -= 1
                myEnvironment.Changed = True
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatRemove Then
            If CurrentVHeat >= 0 And myVHeats.Count > 0 Then
                myVHeats.Remove(CurrentVHeat)
                If CurrentVHeat > 0 Then CurrentVHeat -= 1
                myEnvironment.Changed = True
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        End If
    End Sub
    Private Sub HeatSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HHeatSummary.Click, VHeatSummary.Click
        ' The currently selected Heat has been changed by selecting a row of the summary spreadsheet with a mouse click
        Dim index As Integer
        If sender Is HHeatSummary Then
            index = Me.HHeatSummary.RowSel - 1
            If index >= 0 And index <= myHHeats.Count - 1 Then
                CurrentHHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatSummary Then
            index = Me.VHeatSummary.RowSel - 1
            If index >= 0 And index <= myVHeats.Count - 1 Then
                CurrentVHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        End If
    End Sub
    Private Sub HeatSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles HHeatSummary.AfterSelChange, VHeatSummary.AfterSelChange
        ' The currently selected Heat has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        If sender Is HHeatSummary Then
            index = Me.HHeatSummary.RowSel - 1
            If index >= 0 And index <= myHHeats.Count - 1 Then
                CurrentHHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatSummary Then
            index = Me.VHeatSummary.RowSel - 1
            If index >= 0 And index <= myVHeats.Count - 1 Then
                CurrentVHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        End If
    End Sub
    Private Sub Heat_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HHeatComp1.SelectedIndexChanged, HHeatComp2.SelectedIndexChanged, HHeatFraction.Leave, VHeatComp1.SelectedIndexChanged, VHeatComp2.SelectedIndexChanged
        Dim aVent As New Vent
        If sender Is HHeatComp1 Or sender Is HHeatComp2 Or sender Is HHeatFraction Then
            If CurrentHHeat >= 0 And myHHeats.Count > 0 Then
                aVent = myHHeats.Item(CurrentHHeat)
                If sender Is Me.HHeatFraction Then aVent.InitialOpening = Val(Me.HHeatFraction.Text)
                aVent.VentType = Vent.TypeHHeat
                If sender Is Me.HHeatComp1 Then aVent.FirstCompartment = Me.HHeatComp1.SelectedIndex - 1
                If sender Is Me.HHeatComp2 Then aVent.SecondCompartment = Me.HHeatComp2.SelectedIndex - 1
                myHHeats(CurrentHHeat) = aVent
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatComp1 Or sender Is VHeatComp2 Then
            If CurrentVHeat >= 0 And myVHeats.Count > 0 Then
                aVent = myVHeats.Item(CurrentVHeat)
                aVent.VentType = Vent.TypeVHeat
                If sender Is Me.VHeatComp1 Then aVent.FirstCompartment = Me.VHeatComp1.SelectedIndex - 1
                If sender Is Me.VHeatComp2 Then aVent.SecondCompartment = Me.VHeatComp2.SelectedIndex - 1
                myVHeats(CurrentVHeat) = aVent
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        End If
    End Sub
#End Region

#Region "Visualization Tab "
    ' This section handles the visualization tab
    Private Sub VisualizationAdd_Click(sender As Object, e As EventArgs) Handles VisualizationAdd.Click
        ' Add a Visualization to the end of the list of visualizations
        If myVisuals.Count + 1 <= myVisuals.Maximum Then
            Dim aVisual As New Visual
            myVisuals.Add(aVisual)
            CurrentVisual = myVisuals.Count - 1
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        Else
            MessageBox.Show("A maximum of " + Visual.MaximumVisuals.ToString + " visulaizations are allowed. New visual not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub VisualizationDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VisualizationDup.Click
        ' Copy the current visualization, adding it to the end of the list of visualizations
        If CurrentVisual >= 0 And myVisuals.Count > 0 And CurrentVisual + 1 <= myVisuals.Maximum And myVisuals.Count + 1 <= Visual.MaximumVisuals Then
            myVisuals.Add(New Visual)
            myVisuals.Copy(CurrentVisual, myVisuals.Count - 1)
            CurrentVisual = myVisuals.Count - 1
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        Else
            MessageBox.Show("A maximum of " + myVisuals.Maximum.ToString + " Visuals are allowed. New Visual not added.", Me.Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub VisualizationRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VisualizationRemove.Click
        ' Remove the current vvent from the list of Visuals
        If CurrentVisual >= 0 And myVisuals.Count > 0 Then
            myVisuals.Remove(CurrentVisual)
            If CurrentVisual > 0 Then CurrentVisual -= 1
            myEnvironment.Changed = True
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        End If
    End Sub
    Private Sub VisualizationDefaults_Click(sender As Object, e As EventArgs) Handles VisualizationDefaults.Click
        ' Add a set of default visualizations to the list of visualizations
        If myCompartments.Count > 0 Then
            Dim i As Integer
            Dim aVisual As Visual
            Dim aCompartment As New Compartment
            For i = 0 To myCompartments.Count - 1
                aCompartment = myCompartments.Item(i)
                aVisual = New Visual(Visual.TwoD, 0, aCompartment.RoomWidth / 2, i) : myVisuals.Add(aVisual)
                aVisual.Changed = True
                aVisual = New Visual(Visual.TwoD, 1, aCompartment.RoomDepth / 2, i) : myVisuals.Add(aVisual)
                aVisual.Changed = True
                aVisual = New Visual(Visual.TwoD, 2, aCompartment.RoomHeight * 0.99, i) : myVisuals.Add(aVisual)
                aVisual.Changed = True
            Next
            aVisual = New Visual(Visual.ThreeD, 0, 0.0, -1) : myVisuals.Add(aVisual)
            aVisual.Changed = True
            CurrentVisual = myVisuals.Count - 1
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        End If
    End Sub
    Private Sub VisualizationChanged(sender As Object, e As EventArgs) Handles VisualizationValue.Leave, VisualizationX.Leave, VisualizationY.Leave, VisualizationZ.Leave
        If CurrentVisual >= 0 And myVisuals.Count > 0 Then
            If sender Is VisualizationValue Then
                Dim aVisual As New Visual
                aVisual = myVisuals.Item(CurrentVisual)
                aVisual.Value = Val(Me.VisualizationValue.Text)
                myVisuals.Item(CurrentVisual) = aVisual
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
            Else
                Dim aCompartment As New Compartment
                aCompartment = myCompartments.Item(CurrentCompartment)
                If sender Is VisualizationX Then aCompartment.xGrid = Val(Me.VisualizationX.Text)
                If sender Is VisualizationY Then aCompartment.yGrid = Val(Me.VisualizationY.Text)
                If sender Is VisualizationZ Then aCompartment.zGrid = Val(Me.VisualizationZ.Text)
                myCompartments.Item(CurrentCompartment) = aCompartment
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
                UpdateGUI.Geometry(CurrentCompartment)
            End If
        End If
    End Sub
    Private Sub Visualization_SelectedIndexChanged(sender As Object, e As EventArgs) Handles VisualizationType.SelectedIndexChanged, VisualizationComp.SelectedIndexChanged, VisualizationAxis.SelectedIndexChanged
        If CurrentVisual >= 0 And myVisuals.Count > 0 Then
            Dim aVisual As New Visual
            aVisual = myVisuals.Item(CurrentVisual)
            If sender Is VisualizationType Then
                aVisual.Type = Me.VisualizationType.SelectedIndex
                If Me.VisualizationType.SelectedIndex = 0 Then
                    Me.VisualizationValueLabel.Text = "Position:"
                    Me.VisualizationValueLabel.Visible = True
                    Me.VisualizationValue.Visible = True
                    Me.VisualizationAxis.Visible = True
                    Me.VisualizationAxisLabel.Visible = True
                ElseIf Me.VisualizationType.SelectedIndex = 2 Then
                    Me.VisualizationValueLabel.Text = "Temperature:"
                    Me.VisualizationValueLabel.Visible = True
                    Me.VisualizationValue.Visible = True
                    Me.VisualizationAxis.Visible = False
                    Me.VisualizationAxisLabel.Visible = False
                Else
                    Me.VisualizationValueLabel.Visible = False
                    Me.VisualizationValue.Visible = False
                    Me.VisualizationAxis.Visible = False
                    Me.VisualizationAxisLabel.Visible = False
                End If
            ElseIf sender Is VisualizationComp Then
                aVisual.Compartment = Me.VisualizationComp.SelectedIndex - 1
            ElseIf sender Is VisualizationAxis Then
                aVisual.Axis = Me.VisualizationAxis.SelectedIndex
            End If
            myVisuals.Item(CurrentVisual) = aVisual
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        End If
    End Sub
    Private Sub VisualSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VisualSummary.Click, VisualSummary.AfterSelChange, VisualResolution.Click, VisualResolution.AfterSelChange
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        If sender Is VisualSummary Then
            index = Me.VisualSummary.RowSel - 1
            If index >= 0 And index <= myVisuals.Count - 1 Then
                CurrentVisual = index
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
            End If
        ElseIf sender Is VisualResolution Then
            index = Me.VisualResolution.RowSel - 1
            If index >= 0 And index <= myCompartments.Count - 1 Then
                CurrentCompartment = index
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
                UpdateGUI.Geometry(CurrentCompartment)
            End If
        End If
    End Sub
#End Region

#Region " Menus and Buttons "
    ' This section handles things related to the menus, buttons, etc. on the main screen
    Private Sub TabMain_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabMain.Click
        ' Anytime your change tab pages, you need to update everything to make it consistent
        UpdateGUI.DoErrorCheck = False
        UpdateAll()
        UpdateGUI.DoErrorCheck = True
    End Sub
    Private Sub MainGeometry_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainGeometry.Click
        MainGeometry.Enabled = False
        RunSMVGeometry()
        MainGeometry.Enabled = True
    End Sub
    Private Sub MainRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainRun.Click
        RunCFAST()
    End Sub
    Private Sub MainView_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainView.Click
        Dim LastAccessTimeIn As Date, LastAccessTimeSMV As Date, SMVExists As Boolean
        SMVExists = System.IO.File.Exists(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".smv")
        LastAccessTimeSMV = System.IO.File.GetLastAccessTimeUtc(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".smv")
        LastAccessTimeIn = System.IO.File.GetLastAccessTimeUtc(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
        If LastAccessTimeSMV < LastAccessTimeIn Or SMVExists = False Then
            RunSMVGeometry()
        End If
        RunSmokeView()
    End Sub
    Private Sub MainOpen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainOpen.Click
        Me.OpenDataFileDialog.FilterIndex = 1
        Me.OpenDataFileDialog.ShowDialog()
        If OpenDataFileDialog.FileNames.Length > 0 Then
            Dim FileName As String
            For Each FileName In OpenDataFileDialog.FileNames
                OpenDataFile(FileName)
            Next
        End If
        UpdateAll()
    End Sub
    Private Sub MainSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainSave.Click
        SaveDataFile(False)
    End Sub
    Private Sub MenuUnits_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuUnits.Click
        UserUnits.ShowDialog(Me)
        UpdateAll()
    End Sub
    Private Sub MenuNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuNew.Click
        InitNew()
        UpdateAll()
    End Sub
    Private Sub MenuOpen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuOpen.Click
        Me.OpenDataFileDialog.FilterIndex = 1
        Me.OpenDataFileDialog.ShowDialog()
        If OpenDataFileDialog.FileNames.Length > 0 Then
            Dim FileName As String
            For Each FileName In OpenDataFileDialog.FileNames
                InitNew()
                OpenDataFile(FileName)
            Next
        End If
        UpdateAll()
    End Sub
    Private Sub MenuSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuSave.Click
        SaveDataFile(False)
    End Sub
    Private Sub MenuSaveAs_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuSaveAs.Click
        Dim PathName As String
        myUnits.SI = True
        Me.SaveDataFileDialog.FileName = myEnvironment.InputFileName + ".in"
        Me.SaveDataFileDialog.Title = "Save As"
        Me.SaveDataFileDialog.OverwritePrompt = True
        If Me.SaveDataFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            If Me.SaveDataFileDialog.FileName <> " " Then
                ' Write out the data file since it has been changed
                WriteInputFile(Me.SaveDataFileDialog.FileName)
                myEnvironment.InputFileName = Me.SaveDataFileDialog.FileName
                myEnvironment.InputFilePath = Me.SaveDataFileDialog.FileName
                Me.Text = "CEdit (" + System.IO.Path.GetFileName(Me.SaveDataFileDialog.FileName) + ")"
                myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
            End If
            PathName = System.IO.Path.GetDirectoryName(Me.SaveDataFileDialog.FileName) & "\"
            ChDir(PathName)
        End If
        myUnits.SI = False
        UpdateGUI.Menu()
        UpdateGUI.General()
    End Sub
    Private Sub MenuRunCFast_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuRunCFast.Click
        RunCFAST()
    End Sub
    Private Sub MenuSMVGeometry_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuSMVGeometry.Click
        RunSMVGeometry()
    End Sub
    Private Sub MenuSMVSimulation_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuSMVSimulation.Click
        RunSmokeView()
    End Sub
    Private Sub MenuShowCFAST_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuShowCFAST.Click
        If Me.MenuShowCFAST.Checked Then
            Me.MenuShowCFAST.Checked = False
            CommandWindowVisible = False
        Else
            Me.MenuShowCFAST.Checked = True
            CommandWindowVisible = True
        End If
        SaveSetting("CFAST", "Options", "ShowCFASTOutput", CommandWindowVisible.ToString)
    End Sub
    Private Sub MenuDetailedOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuDetailedOutput.Click
        If Me.MenuDetailedOutput.Checked Then
            DetailedCFASTOutput = False
        Else
            DetailedCFASTOutput = True
        End If
        Me.MenuDetailedOutput.Checked = DetailedCFASTOutput
        SaveSetting("CFAST", "Options", "DetailedOutput", TotalMassCFASTOutput.ToString)
    End Sub
    Private Sub MenuTotalMassOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuTotalMassOutput.Click
        If Me.MenuTotalMassOutput.Checked Then
            TotalMassCFASTOutput = False
        Else
            TotalMassCFASTOutput = True
        End If
        Me.MenuTotalMassOutput.Checked = TotalMassCFASTOutput
        SaveSetting("CFAST", "Options", "MassOutput", TotalMassCFASTOutput.ToString)
    End Sub
    Private Sub MenuNetHeatFluxOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuNetHeatFluxOutput.Click
        If Me.MenuNetHeatFluxOutput.Checked Then
            NetHeatFluxCFASTOutput = False
        Else
            NetHeatFluxCFASTOutput = True
        End If
        Me.MenuNetHeatFluxOutput.Checked = NetHeatFluxCFASTOutput
        SaveSetting("CFAST", "Options", "NetHeatFlux", NetHeatFluxCFASTOutput.ToString)
    End Sub
    Private Sub MenuExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuExit.Click
        Application.Exit()
    End Sub
    Private Sub MenuAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuAbout.Click
        About.ShowDialog()
    End Sub
    Private Sub MenuRecent_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuRecent1.Click, MenuRecent2.Click, MenuRecent3.Click, MenuRecent4.Click
        Dim i As Integer
        If sender Is Me.MenuRecent1 Then i = 0
        If sender Is Me.MenuRecent2 Then i = 1
        If sender Is Me.MenuRecent3 Then i = 2
        If sender Is Me.MenuRecent4 Then i = 3
        OpenDataFile(myRecentFiles.Filenames(i))
        UpdateAll()
    End Sub
    Private Sub MenuHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuShowHelp.Click
        System.Windows.Forms.MessageBox.Show("Documentation on the use of CFAST is available in the CFAST User's Guide installed with the software. Go to Start, All Programs, CFAST7, Documents for all of the CFAST documentation", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub
    Private Sub MenuCFASTweb_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuCFASTWeb.Click
        Process.Start("http://cfast.nist.gov")
    End Sub
    Private Sub MenuViewOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuViewOutput.Click, MenuViewInput.Click, MenuViewLog.Click
        If sender Is MenuViewOutput Then ViewFile.FileName = System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + ".out"
        If sender Is MenuViewInput Then ViewFile.FileName = System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + ".in"
        If sender Is MenuViewLog Then ViewFile.FileName = System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + ".log"
        If System.IO.File.Exists(ViewFile.FileName) Then
            MenuView.Enabled = False
            ViewFile.ShowDialog()
            MenuView.Enabled = True
        End If
    End Sub
    Private Sub MenuValidationOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuValidationOutput.Click
        If Me.MenuValidationOutput.Checked Then
            ValidationOutput = False
        Else
            ValidationOutput = True
        End If
        Me.MenuValidationOutput.Checked = ValidationOutput
        SaveSetting("CFAST", "Options", "Validation", ValidationOutput.ToString)
    End Sub
    Private Sub MenuDebugOutput_Click(sender As System.Object, e As System.EventArgs) Handles MenuDebugOutput.Click
        If Me.MenuDebugOutput.Checked Then
            DebugOutput = False
        Else
            DebugOutput = True
        End If
        Me.MenuDebugOutput.Checked = DebugOutput
    End Sub
    Private Sub FromFileInserts_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalFromFile.Click, FireFromFile.Click
        Dim iReturn As Integer
        Me.OpenDataFileDialog.FilterIndex = 1
        iReturn = OpenDataFileDialog.ShowDialog()
        If iReturn = Windows.Forms.DialogResult.OK And OpenDataFileDialog.FileNames.Length > 0 Then
            Dim FileName As String, Type As Integer, AddFiresList As New FireCollection, InsertDialog As New InsertData(Me)
            If sender Is FireFromFile Then Type = InsertDataType.Fire
            If sender Is ThermalFromFile Then Type = InsertDataType.ThermalProperty
            InsertDialog.SetupData(Type)
            For Each FileName In OpenDataFileDialog.FileNames
                InsertDialog.AddSelectedData(Type, FileName)
            Next
            iReturn = InsertDialog.ShowDialog()
            If iReturn = Windows.Forms.DialogResult.OK Then

                If sender Is ThermalFromFile Then
                    Dim i As Integer, aThermalProperty As New ThermalProperty
                    For i = 1 To InsertDialog.Count
                        If InsertDialog.Selected(i) Then
                            aThermalProperty = TempThermalProperties.Item(i - 1)
                            myThermalProperties.Add(aThermalProperty)
                        End If
                    Next
                ElseIf sender Is FireFromFile Then
                    Dim i As Integer, aFire As New Fire()
                    For i = 1 To InsertDialog.Count
                        If InsertDialog.Selected(i) Then
                            aFire = TempFireObjects.Item(i - 1)
                            myFires.Add(aFire)
                        End If
                    Next
                End If
            End If
        End If
        UpdateAll()
    End Sub
#End Region

#Region " Support Routines "

    Private Sub OpenDataFile(ByVal FileName As String)
        Dim PathName As String
        If My.Computer.FileSystem.FileExists(FileName) Then
            myUnits.SI = True
            InitNew()
            PathName = System.IO.Path.GetDirectoryName(FileName) & "\"
            ChDir(PathName)
            ReadInputFile(FileName)
            myEnvironment.InputFileName = FileName
            myEnvironment.InputFilePath = FileName
            myRecentFiles.Add(FileName)
            myUnits.SI = False
            UpdateAll()
            myCompartments(CurrentCompartment).Changed = False
        Else
            MsgBox("Error opening file:" & Chr(13) & FileName & Chr(13) & "File does not exist", MsgBoxStyle.Exclamation)
        End If
    End Sub
    Private Sub SaveDataFile(ByVal Prompt As Boolean)
        myUnits.SI = True
        If myEnvironment.FileChanged() Then
            If Prompt Or myEnvironment.InputFileName = Nothing Or myEnvironment.InputFileName = "" Then
                Me.SaveDataFileDialog.Title = "Save"
                Me.SaveDataFileDialog.OverwritePrompt = True
                If SaveDataFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                    If Me.SaveDataFileDialog.FileName <> " " Then
                        ' Write out the data file since it has been changed
                        WriteInputFile(Me.SaveDataFileDialog.FileName)
                        myEnvironment.InputFileName = Me.SaveDataFileDialog.FileName
                        myEnvironment.InputFilePath = Me.SaveDataFileDialog.FileName
                        myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
                    End If
                End If
            Else
                WriteInputFile(myEnvironment.InputFileName + ".in")
                myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
            End If
        End If
        myUnits.SI = False
        UpdateGUI.Menu()
        UpdateGUI.General()
    End Sub
    Private Sub RunCFAST()
        If myEnvironment.FileChanged Then SaveDataFile(True)
        If System.IO.File.Exists(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in") Then
            Dim RunSimulation As New RunModel
            CFASTSimulationTime = myEnvironment.SimulationTime
            CFastInputFile = myEnvironment.InputFileName
            RunSimulation.Text = "Run Model (" + System.IO.Path.GetFileName(CFastInputFile) + ")"
            RunSimulation.ShowDialog()

            UpdateGUI.Menu()
            UpdateGUI.Environment()
        End If
    End Sub
    Private Sub RunSMVGeometry()
        Dim CommandString As String, found As Integer, ProcessID As Integer
        If myEnvironment.FileChanged Then SaveDataFile(True)
        Try
            found = myEnvironment.InputFileName.IndexOf(" ", 0)
            If found <= 0 Then
                CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + " /I"
            Else
                CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + """" + System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + """" + " /I"
            End If
            If MenuShowCFAST.Checked Then
                ProcessID = Shell(CommandString, AppWinStyle.NormalNoFocus, True, 5000)
            Else
                ProcessID = Shell(CommandString, AppWinStyle.Hide, True, 5000)
            End If
        Catch ex As Exception
        End Try
        UpdateGUI.UpdateLogFile(Me.EnvErrors)
    End Sub
    Private Sub RunSmokeView()
        Dim CommandString As String, found As Integer, ProcessID As Integer
        If myEnvironment.FileChanged Then SaveDataFile(True)
        Try
            found = myEnvironment.InputFileName.IndexOf(" ", 0)
            If found <= 0 Then
                CommandString = """" + Application.StartupPath + "\smokeview.exe"" " + System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName)
            Else
                CommandString = """" + Application.StartupPath + "\smokeview.exe"" " + """" + System.IO.Path.GetFileNameWithoutExtension(myEnvironment.InputFileName) + """"
            End If
            ProcessID = Shell(CommandString, AppWinStyle.NormalFocus, True)
        Catch ex As Exception
        End Try
    End Sub
    Private Sub UpdateAll()
        UpdateGUI.Menu()
        UpdateGUI.General()
        UpdateGUI.DoErrorCheck = False
        UpdateGUI.Environment()
        UpdateGUI.Geometry(CurrentCompartment)
        UpdateGUI.Fires(CurrentFire)
        UpdateGUI.Targets(CurrentTarget)
        UpdateGUI.Thermals(CurrentThermalProperty)
        UpdateGUI.HVents(CurrentHVent)
        UpdateGUI.VVents(CurrentVVent)
        UpdateGUI.MVents(CurrentMVent)
        UpdateGUI.Detectors(CurrentDetector)
        UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
        UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        UpdateGUI.DoErrorCheck = True
    End Sub
    Private Sub InitNew()
        ' Start with a clean slate and a default set of inputs
        myEnvironment = New Environment
        Me.Text = "CEdit"
        myCompartments.Clear()
        myEnvironment.AdiabaticWalls = False
        myHVents.Clear()
        myVVents.Clear()
        myMVents.Clear()
        myHHeats.Clear()
        myVHeats.Clear()
        myTargets.Clear()
        myDetectors.Clear()
        myFires.Clear()
        myFireObjects.Clear()
        TempFireObjects.Clear()
        myVisuals.Clear()
        myThermalProperties.Clear()
        TempThermalProperties.Clear()
        myErrors.Queue.Clear()
        Do While (dataFileHeader.Count > 0)
            dataFileHeader.Remove(dataFileHeader.Count)
        Loop
        Do While (dataFileComments.Count > 0)
            dataFileComments.Remove(dataFileComments.Count)
        Loop
        Do While (thermalFileComments.Count > 0)
            thermalFileComments.Remove(thermalFileComments.Count)
        Loop
        Do While (fireFilesComments.Count > 0)
            fireFilesComments.Remove(fireFilesComments.Count)
        Loop
        CurrentCompartment = 0
        CurrentHVent = 0
        CurrentVVent = 0
        CurrentMVent = 0
        CurrentTarget = 0
        CurrentDetector = 0
        CurrentHHeat = 0
        CurrentVHeat = 0
        CurrentFire = 0

        UpdateGUI.InitThermalPropertyList(Me.CompCeiling)
        UpdateGUI.InitThermalPropertyList(Me.CompWalls)
        UpdateGUI.InitThermalPropertyList(Me.CompFloor)
        UpdateGUI.InitThermalPropertyList(Me.TargetMaterial)

        ' Initialize spreadsheets for input or no input (summary tables) as appropriate
        UpdateGUI.InitSummaryGrid(Me.ThermalSummary)
        UpdateGUI.InitSummaryGrid(Me.CompSummary)
        UpdateGUI.InitSummaryGrid(Me.HVentSummary)
        UpdateGUI.InitSummaryGrid(Me.VVentSummary)
        UpdateGUI.InitSummaryGrid(Me.MVentSummary)
        UpdateGUI.InitSummaryGrid(Me.TargetSummary)
        UpdateGUI.InitSummaryGrid(Me.DetectorSummary)
        UpdateGUI.InitSummaryGrid(Me.FireSummary)

        UpdateGUI.InitEditGrid(Me.CompVariableArea)

        ' Turn off all input except the simulation environment and compartment add since all others stuff depends on have a compartment
        Me.TabHorizontalFlow.Enabled = False
        Me.TabVerticalFlow.Enabled = False
        Me.TabMechanicalFlow.Enabled = False
        Me.TabTargets.Enabled = False
        Me.TabDetection.Enabled = False
        Me.TabHeatTransfer.Enabled = False
        Me.TabFires.Enabled = False
        Me.GroupCompartments.Enabled = False
        Me.MenuViewInput.Enabled = False
        Me.MenuViewOutput.Enabled = False
        Me.MenuViewLog.Enabled = False
        Me.MenuDetailedOutput.Checked = True
        UpdateAll()
    End Sub
#End Region

End Class
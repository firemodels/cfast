Public Class CeditMain

    Inherits System.Windows.Forms.Form
    Friend UpdateGUI As New UpdateGUI(Me)
    Private UserUnits As New User_Units
    Private About As New About
    Private ViewFile As New ViewFile
    'Private RunSimulation As New RunModel
    Private CurrentThermalProperty As Integer = 0, CurrentCompartment As Integer = 0, CurrentHVent As Integer = 0, CurrentVVent As Integer = 0,
    CurrentMVent As Integer = 0, CurrentTarget As Integer = 0, CurrentDetector As Integer = 0, CurrentHHeat As Integer = 0,
    CurrentVHeat As Integer = 0, CurrentFire As Integer = 0, CurrentVisual As Integer = 0
    Private Const OK As Integer = 1, Cancel As Integer = 2

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
    Friend WithEvents HVentName As TextBox
    Friend WithEvents VVentName As TextBox
    Friend WithEvents Label8 As Label
    Friend WithEvents MVentName As TextBox
    Friend WithEvents Label24 As Label
    Friend WithEvents DetectorName As TextBox
    Friend WithEvents Label67 As Label
    Friend WithEvents Label6 As Label
    Friend WithEvents FireRemoveInstance As Button
    Friend WithEvents Label60 As Label
    Friend WithEvents FireDefinitionName As TextBox
    Friend WithEvents FireDataSS As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents FirePlot As NPlot.Windows.PlotSurface2D
    Friend WithEvents ReferencedFireDefinition As ComboBox
    Friend WithEvents Label36 As Label
    Friend WithEvents Label18 As Label
    Friend WithEvents EnvLOI As TextBox
    Friend WithEvents HVentFinalLabel As Label
    Friend WithEvents HVentFinalFraction As TextBox
    Friend WithEvents HVentInitialLabel As Label
    Friend WithEvents HVentInitialFraction As TextBox
    Friend WithEvents MventFinalLabel As Label
    Friend WithEvents MVentFinalFraction As TextBox
    Friend WithEvents MVentInitialLabel As Label
    Friend WithEvents MVentInitialFraction As TextBox
    Friend WithEvents VVentFinalLabel As Label
    Friend WithEvents VVentFinalFraction As TextBox
    Friend WithEvents VVentInitialLabel As Label
    Friend WithEvents VVentInitialFraction As TextBox
    Friend WithEvents VVentYOffset As TextBox
    Friend WithEvents Label68 As Label
    Friend WithEvents VVentXOffset As TextBox
    Friend WithEvents Label55 As Label
    Friend WithEvents VVentOpenValueLabel As Label
    Friend WithEvents HVentOpenValueLabel As Label
    Friend WithEvents HVentOpenValue As TextBox
    Friend WithEvents HVentTarget As ComboBox
    Friend WithEvents HVentTargetLabel As Label
    Friend WithEvents HVentOpenCriterion As ComboBox
    Friend WithEvents Label82 As Label
    Friend WithEvents HVentFractions As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents MVentYOffset As TextBox
    Friend WithEvents Label98 As Label
    Friend WithEvents MVentXOffset As TextBox
    Friend WithEvents Label99 As Label
    Friend WithEvents MVentOpenValueLabel As Label
    Friend WithEvents MVentOpenValue As TextBox
    Friend WithEvents MVentTarget As ComboBox
    Friend WithEvents MVentTargetLabel As Label
    Friend WithEvents MVentOpenCriterion As ComboBox
    Friend WithEvents Label97 As Label
    Friend WithEvents MVentFractions As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents VVentOpenValue As TextBox
    Friend WithEvents VVentFractions As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents VVentTarget As ComboBox
    Friend WithEvents VVentTargetLabel As Label
    Friend WithEvents VVentOpenCriterion As ComboBox
    Friend WithEvents GroupBox4 As GroupBox
    Friend WithEvents Label80 As Label
    Friend WithEvents DetectorActivationObscuration As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents TargetName As System.Windows.Forms.TextBox
    Friend WithEvents Label116 As System.Windows.Forms.Label
    Friend WithEvents FireTarget As System.Windows.Forms.ComboBox
    Friend WithEvents OutputValidation As System.Windows.Forms.CheckBox
    Friend WithEvents OutputShowCFAST As System.Windows.Forms.CheckBox
    Friend WithEvents OutputDebug As System.Windows.Forms.CheckBox
    Friend WithEvents Output As System.Windows.Forms.StatusBarPanel
    Friend WithEvents MenuHelpUpdate As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem5 As System.Windows.Forms.MenuItem
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
    Friend WithEvents FireHoC As System.Windows.Forms.TextBox
    Friend WithEvents Label108 As System.Windows.Forms.Label
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
    Friend WithEvents FireInstanceName As System.Windows.Forms.TextBox
    Friend WithEvents Label114 As System.Windows.Forms.Label
    Friend WithEvents EnvAdiabatic As System.Windows.Forms.CheckBox
    Friend WithEvents ThermalShortName As System.Windows.Forms.TextBox
    Friend WithEvents GroupVisualResolution As System.Windows.Forms.GroupBox
    Friend WithEvents VisualResolutionSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents Label31 As System.Windows.Forms.Label
    Friend WithEvents VisualizationX As System.Windows.Forms.TextBox
    Friend WithEvents Label35 As System.Windows.Forms.Label
    Friend WithEvents VisualizationZ As System.Windows.Forms.TextBox
    Friend WithEvents Label32 As System.Windows.Forms.Label
    Friend WithEvents VisualizationY As System.Windows.Forms.TextBox
    Friend WithEvents GroupVisualizations As System.Windows.Forms.GroupBox
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
    Friend WithEvents MainOpen As System.Windows.Forms.Button
    Friend WithEvents FireAddt2 As System.Windows.Forms.Button
    Friend WithEvents FireAdd As System.Windows.Forms.Button
    Friend WithEvents C1SizerLight1 As C1.Win.C1Sizer.C1SizerLight
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents EnvTimeStep As System.Windows.Forms.TextBox
    Friend WithEvents TabOutput As System.Windows.Forms.TabPage
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
    Friend WithEvents Label69 As System.Windows.Forms.Label
    Friend WithEvents Label70 As System.Windows.Forms.Label
    Friend WithEvents Label52 As System.Windows.Forms.Label
    Friend WithEvents Label61 As System.Windows.Forms.Label
    Friend WithEvents Label72 As System.Windows.Forms.Label
    Friend WithEvents Label73 As System.Windows.Forms.Label
    Friend WithEvents Label79 As System.Windows.Forms.Label
    Friend WithEvents Label78 As System.Windows.Forms.Label
    Friend WithEvents Label74 As System.Windows.Forms.Label
    Friend WithEvents GroupBox28 As System.Windows.Forms.GroupBox
    Friend WithEvents Label75 As System.Windows.Forms.Label
    Friend WithEvents Label76 As System.Windows.Forms.Label
    Friend WithEvents Label77 As System.Windows.Forms.Label
    Friend WithEvents Label28 As System.Windows.Forms.Label
    Friend WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox12 As System.Windows.Forms.GroupBox
    Friend WithEvents Label26 As System.Windows.Forms.Label
    Friend WithEvents Label27 As System.Windows.Forms.Label
    Friend WithEvents GroupBox11 As System.Windows.Forms.GroupBox
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
    Friend WithEvents FireYPosition As System.Windows.Forms.TextBox
    Friend WithEvents FireXPosition As System.Windows.Forms.TextBox
    Friend WithEvents DetectorSprayDensity As System.Windows.Forms.TextBox
    Friend WithEvents DetectorRTI As System.Windows.Forms.TextBox
    Friend WithEvents DetectorActivationTemperature As System.Windows.Forms.TextBox
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
    Friend WithEvents EnvExtAmbTemp As System.Windows.Forms.TextBox
    Friend WithEvents FireIgnitionCriteria As System.Windows.Forms.ComboBox
    Friend WithEvents FireIgnitionValue As System.Windows.Forms.TextBox
    Friend WithEvents Label63 As System.Windows.Forms.Label
    Friend WithEvents Label58 As System.Windows.Forms.Label
    Friend WithEvents GroupVVents As System.Windows.Forms.GroupBox
    Friend WithEvents GroupCompartments As System.Windows.Forms.GroupBox
    Friend WithEvents GroupMVents As System.Windows.Forms.GroupBox
    Friend WithEvents GroupDetectors As System.Windows.Forms.GroupBox
    Friend WithEvents GroupTargets As System.Windows.Forms.GroupBox
    Friend WithEvents HVentFace As System.Windows.Forms.ComboBox
    Friend WithEvents Label37 As System.Windows.Forms.Label
    Friend WithEvents GroupBox14 As System.Windows.Forms.GroupBox
    Friend WithEvents HVentComp2 As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox13 As System.Windows.Forms.GroupBox
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents HVentOffset As System.Windows.Forms.TextBox
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
    Friend WithEvents TargetSolutionType As System.Windows.Forms.ComboBox
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
    Friend WithEvents FireSummary As C1.Win.C1FlexGrid.C1FlexGrid
    Friend WithEvents MenuUnits As System.Windows.Forms.MenuItem
    Friend WithEvents MenuSave As System.Windows.Forms.MenuItem
    Friend WithEvents MenuSaveAs As System.Windows.Forms.MenuItem
    Friend WithEvents MenuOpen As System.Windows.Forms.MenuItem
    Friend WithEvents CompWidth As System.Windows.Forms.TextBox
    Friend WithEvents Label25 As System.Windows.Forms.Label
    Friend WithEvents EnvSmokeviewInterval As System.Windows.Forms.TextBox
    Friend WithEvents MenuNew As System.Windows.Forms.MenuItem
    Friend WithEvents SaveDataFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents OpenDataFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents MenuExit As System.Windows.Forms.MenuItem
    Friend WithEvents Errors As System.Windows.Forms.StatusBarPanel
    Friend WithEvents Message As System.Windows.Forms.StatusBarPanel
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
    Friend WithEvents MenuView As System.Windows.Forms.MenuItem
    Friend WithEvents MenuShowHelp As System.Windows.Forms.MenuItem
    Friend WithEvents MenuHelp As System.Windows.Forms.MenuItem
    Friend WithEvents MenuSMVSimulation As System.Windows.Forms.MenuItem
    Friend WithEvents MenuItem1 As System.Windows.Forms.MenuItem
    Friend WithEvents ErrorProvider1 As System.Windows.Forms.ErrorProvider
    Friend WithEvents TargetNormalCalc As System.Windows.Forms.ComboBox
    Friend WithEvents MainView As System.Windows.Forms.Button
    Friend WithEvents MainSave As System.Windows.Forms.Button
    Friend WithEvents MainRun As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(CeditMain))
        Me.StatusBar = New System.Windows.Forms.StatusBar()
        Me.Errors = New System.Windows.Forms.StatusBarPanel()
        Me.Message = New System.Windows.Forms.StatusBarPanel()
        Me.Output = New System.Windows.Forms.StatusBarPanel()
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
        Me.MenuView = New System.Windows.Forms.MenuItem()
        Me.MenuUnits = New System.Windows.Forms.MenuItem()
        Me.MenuItem5 = New System.Windows.Forms.MenuItem()
        Me.MenuViewInput = New System.Windows.Forms.MenuItem()
        Me.MenuViewOutput = New System.Windows.Forms.MenuItem()
        Me.MenuViewLog = New System.Windows.Forms.MenuItem()
        Me.MenuHelp = New System.Windows.Forms.MenuItem()
        Me.MenuHelpUpdate = New System.Windows.Forms.MenuItem()
        Me.MenuShowHelp = New System.Windows.Forms.MenuItem()
        Me.MenuCFASTWeb = New System.Windows.Forms.MenuItem()
        Me.MenuAbout = New System.Windows.Forms.MenuItem()
        Me.TabEnvironment = New System.Windows.Forms.TabPage()
        Me.EnvErrors = New System.Windows.Forms.TextBox()
        Me.EnvTitle = New System.Windows.Forms.TextBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.EnvLOI = New System.Windows.Forms.TextBox()
        Me.EnvAdiabatic = New System.Windows.Forms.CheckBox()
        Me.GroupBox12 = New System.Windows.Forms.GroupBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.EnvExtAmbPress = New System.Windows.Forms.TextBox()
        Me.Label27 = New System.Windows.Forms.Label()
        Me.EnvExtAmbTemp = New System.Windows.Forms.TextBox()
        Me.GroupBox11 = New System.Windows.Forms.GroupBox()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.EnvIntAmbRH = New System.Windows.Forms.TextBox()
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
        Me.HVentName = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.HVentFinalLabel = New System.Windows.Forms.Label()
        Me.HVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.HVentInitialLabel = New System.Windows.Forms.Label()
        Me.HVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.HVentOpenValueLabel = New System.Windows.Forms.Label()
        Me.HVentOpenValue = New System.Windows.Forms.TextBox()
        Me.HVentTarget = New System.Windows.Forms.ComboBox()
        Me.HVentTargetLabel = New System.Windows.Forms.Label()
        Me.HVentOpenCriterion = New System.Windows.Forms.ComboBox()
        Me.Label82 = New System.Windows.Forms.Label()
        Me.HVentFractions = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.HVentOffset = New System.Windows.Forms.TextBox()
        Me.HVentFace = New System.Windows.Forms.ComboBox()
        Me.Label37 = New System.Windows.Forms.Label()
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
        Me.VVentName = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.VVentFinalLabel = New System.Windows.Forms.Label()
        Me.VVentFractions = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.VVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.VVentInitialLabel = New System.Windows.Forms.Label()
        Me.VVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.VVentYOffset = New System.Windows.Forms.TextBox()
        Me.Label68 = New System.Windows.Forms.Label()
        Me.VVentXOffset = New System.Windows.Forms.TextBox()
        Me.Label55 = New System.Windows.Forms.Label()
        Me.VVentOpenValueLabel = New System.Windows.Forms.Label()
        Me.VVentOpenValue = New System.Windows.Forms.TextBox()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.VVentCompBottom = New System.Windows.Forms.ComboBox()
        Me.VVentTarget = New System.Windows.Forms.ComboBox()
        Me.VVentTargetLabel = New System.Windows.Forms.Label()
        Me.VVentOpenCriterion = New System.Windows.Forms.ComboBox()
        Me.Label80 = New System.Windows.Forms.Label()
        Me.VVentShape = New System.Windows.Forms.ComboBox()
        Me.Label40 = New System.Windows.Forms.Label()
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
        Me.MVentName = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.MventFinalLabel = New System.Windows.Forms.Label()
        Me.MVentFinalFraction = New System.Windows.Forms.TextBox()
        Me.MVentInitialLabel = New System.Windows.Forms.Label()
        Me.MVentInitialFraction = New System.Windows.Forms.TextBox()
        Me.MVentYOffset = New System.Windows.Forms.TextBox()
        Me.Label98 = New System.Windows.Forms.Label()
        Me.MVentXOffset = New System.Windows.Forms.TextBox()
        Me.Label99 = New System.Windows.Forms.Label()
        Me.MVentOpenValueLabel = New System.Windows.Forms.Label()
        Me.MVentOpenValue = New System.Windows.Forms.TextBox()
        Me.MVentTarget = New System.Windows.Forms.ComboBox()
        Me.MVentTargetLabel = New System.Windows.Forms.Label()
        Me.MVentOpenCriterion = New System.Windows.Forms.ComboBox()
        Me.Label97 = New System.Windows.Forms.Label()
        Me.MVentFractions = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.MVentFilterTime = New System.Windows.Forms.TextBox()
        Me.Label38 = New System.Windows.Forms.Label()
        Me.MVentFilterEfficiency = New System.Windows.Forms.TextBox()
        Me.Label54 = New System.Windows.Forms.Label()
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
        Me.Label60 = New System.Windows.Forms.Label()
        Me.FireDataSS = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.FireAdd = New System.Windows.Forms.Button()
        Me.FirePlot = New NPlot.Windows.PlotSurface2D()
        Me.Label113 = New System.Windows.Forms.Label()
        Me.FireRemoveInstance = New System.Windows.Forms.Button()
        Me.Label111 = New System.Windows.Forms.Label()
        Me.FireDefinitionName = New System.Windows.Forms.TextBox()
        Me.FireYPosition = New System.Windows.Forms.TextBox()
        Me.FireHoC = New System.Windows.Forms.TextBox()
        Me.Label109 = New System.Windows.Forms.Label()
        Me.FireC = New System.Windows.Forms.TextBox()
        Me.FireH = New System.Windows.Forms.TextBox()
        Me.FireSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.Label108 = New System.Windows.Forms.Label()
        Me.Label110 = New System.Windows.Forms.Label()
        Me.Label114 = New System.Windows.Forms.Label()
        Me.Label106 = New System.Windows.Forms.Label()
        Me.FireCl = New System.Windows.Forms.TextBox()
        Me.FireAddt2 = New System.Windows.Forms.Button()
        Me.FireRadiativeFraction = New System.Windows.Forms.TextBox()
        Me.FireInstanceName = New System.Windows.Forms.TextBox()
        Me.ReferencedFireDefinition = New System.Windows.Forms.ComboBox()
        Me.FireO = New System.Windows.Forms.TextBox()
        Me.Label63 = New System.Windows.Forms.Label()
        Me.Label112 = New System.Windows.Forms.Label()
        Me.Label58 = New System.Windows.Forms.Label()
        Me.FireN = New System.Windows.Forms.TextBox()
        Me.Label36 = New System.Windows.Forms.Label()
        Me.FireXPosition = New System.Windows.Forms.TextBox()
        Me.Label70 = New System.Windows.Forms.Label()
        Me.FireIgnitionValue = New System.Windows.Forms.TextBox()
        Me.FireFromFile = New System.Windows.Forms.Button()
        Me.Label107 = New System.Windows.Forms.Label()
        Me.FireIgnitionCriteria = New System.Windows.Forms.ComboBox()
        Me.FireTarget = New System.Windows.Forms.ComboBox()
        Me.Label52 = New System.Windows.Forms.Label()
        Me.Label69 = New System.Windows.Forms.Label()
        Me.FireComp = New System.Windows.Forms.ComboBox()
        Me.TabDetection = New System.Windows.Forms.TabPage()
        Me.DetectorSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.GroupDetectors = New System.Windows.Forms.GroupBox()
        Me.DetectorName = New System.Windows.Forms.TextBox()
        Me.Label67 = New System.Windows.Forms.Label()
        Me.DetectorActivationObscuration = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.DetectorSprayDensity = New System.Windows.Forms.TextBox()
        Me.Label81 = New System.Windows.Forms.Label()
        Me.DetectorRTI = New System.Windows.Forms.TextBox()
        Me.Label83 = New System.Windows.Forms.Label()
        Me.DetectorActivationTemperature = New System.Windows.Forms.TextBox()
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
        Me.TargetName = New System.Windows.Forms.TextBox()
        Me.Label116 = New System.Windows.Forms.Label()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.TargetInternalLocation = New System.Windows.Forms.TextBox()
        Me.Label59 = New System.Windows.Forms.Label()
        Me.TargetSpecHeat = New System.Windows.Forms.Label()
        Me.TargetDensity = New System.Windows.Forms.Label()
        Me.TargetThickness = New System.Windows.Forms.Label()
        Me.TargetConduct = New System.Windows.Forms.Label()
        Me.TargetMaterial = New System.Windows.Forms.ComboBox()
        Me.Label78 = New System.Windows.Forms.Label()
        Me.Label79 = New System.Windows.Forms.Label()
        Me.TargetComp = New System.Windows.Forms.ComboBox()
        Me.Label74 = New System.Windows.Forms.Label()
        Me.TargetSolutionType = New System.Windows.Forms.ComboBox()
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
        Me.TabOutput = New System.Windows.Forms.TabPage()
        Me.OutputValidation = New System.Windows.Forms.CheckBox()
        Me.OutputShowCFAST = New System.Windows.Forms.CheckBox()
        Me.GroupVisualResolution = New System.Windows.Forms.GroupBox()
        Me.Label35 = New System.Windows.Forms.Label()
        Me.VisualizationZ = New System.Windows.Forms.TextBox()
        Me.Label32 = New System.Windows.Forms.Label()
        Me.VisualizationY = New System.Windows.Forms.TextBox()
        Me.Label31 = New System.Windows.Forms.Label()
        Me.VisualizationX = New System.Windows.Forms.TextBox()
        Me.VisualResolutionSummary = New C1.Win.C1FlexGrid.C1FlexGrid()
        Me.OutputDebug = New System.Windows.Forms.CheckBox()
        Me.GroupVisualizations = New System.Windows.Forms.GroupBox()
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
        CType(Me.Output, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabEnvironment.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        Me.GroupBox12.SuspendLayout()
        Me.GroupBox11.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.TabHorizontalFlow.SuspendLayout()
        CType(Me.HVentSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupHVentGeometry.SuspendLayout()
        CType(Me.HVentFractions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox14.SuspendLayout()
        Me.GroupBox13.SuspendLayout()
        Me.TabVerticalFlow.SuspendLayout()
        CType(Me.VVentSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupVVents.SuspendLayout()
        CType(Me.VVentFractions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox4.SuspendLayout()
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
        CType(Me.MVentFractions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox20.SuspendLayout()
        Me.GroupBox21.SuspendLayout()
        Me.TabFires.SuspendLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FireSummary, System.ComponentModel.ISupportInitialize).BeginInit()
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
        Me.TabOutput.SuspendLayout()
        Me.GroupVisualResolution.SuspendLayout()
        CType(Me.VisualResolutionSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupVisualizations.SuspendLayout()
        CType(Me.VisualSummary, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.ErrorProvider1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C1SizerLight1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'StatusBar
        '
        Me.StatusBar.Location = New System.Drawing.Point(0, 686)
        Me.StatusBar.Name = "StatusBar"
        Me.StatusBar.Panels.AddRange(New System.Windows.Forms.StatusBarPanel() {Me.Errors, Me.Message, Me.Output})
        Me.StatusBar.ShowPanels = True
        Me.StatusBar.Size = New System.Drawing.Size(1008, 22)
        Me.StatusBar.TabIndex = 2
        '
        'Errors
        '
        Me.Errors.AutoSize = System.Windows.Forms.StatusBarPanelAutoSize.Contents
        Me.Errors.BorderStyle = System.Windows.Forms.StatusBarPanelBorderStyle.None
        Me.Errors.Name = "Errors"
        Me.Errors.Text = "No Errors"
        Me.Errors.Width = 62
        '
        'Message
        '
        Me.Message.BorderStyle = System.Windows.Forms.StatusBarPanelBorderStyle.None
        Me.Message.Name = "Message"
        Me.Message.Text = "0"
        Me.Message.Width = 500
        '
        'Output
        '
        Me.Output.BorderStyle = System.Windows.Forms.StatusBarPanelBorderStyle.None
        Me.Output.Name = "Output"
        Me.Output.Width = 400
        '
        'MainMenu
        '
        Me.MainMenu.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuFile, Me.MenuView, Me.MenuHelp})
        '
        'MenuFile
        '
        Me.MenuFile.Index = 0
        Me.MenuFile.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuNew, Me.MenuOpen, Me.MenuSave, Me.MenuSaveAs, Me.MenuExit, Me.MenuRecentSeparator, Me.MenuRecent1, Me.MenuRecent2, Me.MenuRecent3, Me.MenuRecent4})
        Me.MenuFile.Shortcut = System.Windows.Forms.Shortcut.CtrlF
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
        'MenuView
        '
        Me.MenuView.Index = 1
        Me.MenuView.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuUnits, Me.MenuItem5, Me.MenuViewInput, Me.MenuViewOutput, Me.MenuViewLog})
        Me.MenuView.Shortcut = System.Windows.Forms.Shortcut.CtrlV
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
        Me.MenuHelp.Index = 2
        Me.MenuHelp.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.MenuHelpUpdate, Me.MenuShowHelp, Me.MenuCFASTWeb, Me.MenuAbout})
        Me.MenuHelp.Shortcut = System.Windows.Forms.Shortcut.CtrlH
        Me.MenuHelp.Text = "Help"
        '
        'MenuHelpUpdate
        '
        Me.MenuHelpUpdate.Index = 0
        Me.MenuHelpUpdate.Shortcut = System.Windows.Forms.Shortcut.CtrlU
        Me.MenuHelpUpdate.Text = "Update Input Files"
        '
        'MenuShowHelp
        '
        Me.MenuShowHelp.Index = 1
        Me.MenuShowHelp.Text = "Documentation"
        '
        'MenuCFASTWeb
        '
        Me.MenuCFASTWeb.Index = 2
        Me.MenuCFASTWeb.Text = "CFast Web Site"
        '
        'MenuAbout
        '
        Me.MenuAbout.Index = 3
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
        Me.EnvErrors.TabIndex = 18
        Me.EnvErrors.TabStop = False
        Me.EnvErrors.Text = "No Errors"
        '
        'EnvTitle
        '
        Me.EnvTitle.Location = New System.Drawing.Point(272, 14)
        Me.EnvTitle.Name = "EnvTitle"
        Me.EnvTitle.Size = New System.Drawing.Size(472, 20)
        Me.EnvTitle.TabIndex = 101
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
        Me.GroupBox8.Controls.Add(Me.Label18)
        Me.GroupBox8.Controls.Add(Me.EnvLOI)
        Me.GroupBox8.Controls.Add(Me.EnvAdiabatic)
        Me.GroupBox8.Controls.Add(Me.GroupBox12)
        Me.GroupBox8.Controls.Add(Me.GroupBox11)
        Me.GroupBox8.Location = New System.Drawing.Point(496, 58)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.Size = New System.Drawing.Size(392, 257)
        Me.GroupBox8.TabIndex = 11
        Me.GroupBox8.TabStop = False
        Me.GroupBox8.Text = "Simulation Conditions"
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(94, 229)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(102, 13)
        Me.Label18.TabIndex = 115
        Me.Label18.Text = "Lower Oxygen Limit:"
        Me.Label18.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvLOI
        '
        Me.EnvLOI.Location = New System.Drawing.Point(202, 226)
        Me.EnvLOI.Name = "EnvLOI"
        Me.EnvLOI.Size = New System.Drawing.Size(96, 20)
        Me.EnvLOI.TabIndex = 116
        Me.EnvLOI.Text = "0.15"
        '
        'EnvAdiabatic
        '
        Me.EnvAdiabatic.AutoSize = True
        Me.EnvAdiabatic.Location = New System.Drawing.Point(97, 204)
        Me.EnvAdiabatic.Name = "EnvAdiabatic"
        Me.EnvAdiabatic.Size = New System.Drawing.Size(180, 17)
        Me.EnvAdiabatic.TabIndex = 114
        Me.EnvAdiabatic.Text = "Adiabatic Compartment Surfaces"
        Me.EnvAdiabatic.UseVisualStyleBackColor = True
        '
        'GroupBox12
        '
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
        'Label26
        '
        Me.Label26.Location = New System.Drawing.Point(184, 32)
        Me.Label26.Name = "Label26"
        Me.Label26.Size = New System.Drawing.Size(72, 23)
        Me.Label26.TabIndex = 109
        Me.Label26.Text = "Pressure:"
        Me.Label26.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvExtAmbPress
        '
        Me.EnvExtAmbPress.Location = New System.Drawing.Point(264, 33)
        Me.EnvExtAmbPress.Name = "EnvExtAmbPress"
        Me.EnvExtAmbPress.Size = New System.Drawing.Size(96, 20)
        Me.EnvExtAmbPress.TabIndex = 113
        Me.EnvExtAmbPress.Text = "101325 Pa"
        '
        'Label27
        '
        Me.Label27.Location = New System.Drawing.Point(8, 32)
        Me.Label27.Name = "Label27"
        Me.Label27.Size = New System.Drawing.Size(72, 23)
        Me.Label27.TabIndex = 110
        Me.Label27.Text = "Temperature:"
        Me.Label27.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvExtAmbTemp
        '
        Me.EnvExtAmbTemp.Location = New System.Drawing.Point(88, 33)
        Me.EnvExtAmbTemp.Name = "EnvExtAmbTemp"
        Me.EnvExtAmbTemp.Size = New System.Drawing.Size(96, 20)
        Me.EnvExtAmbTemp.TabIndex = 111
        Me.EnvExtAmbTemp.Text = "20 °C"
        '
        'GroupBox11
        '
        Me.GroupBox11.Controls.Add(Me.Label30)
        Me.GroupBox11.Controls.Add(Me.EnvIntAmbRH)
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
        Me.Label30.Location = New System.Drawing.Point(200, 32)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(56, 23)
        Me.Label30.TabIndex = 117
        Me.Label30.Text = "Humidity"
        Me.Label30.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbRH
        '
        Me.EnvIntAmbRH.Location = New System.Drawing.Point(264, 33)
        Me.EnvIntAmbRH.Name = "EnvIntAmbRH"
        Me.EnvIntAmbRH.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbRH.TabIndex = 110
        Me.EnvIntAmbRH.Text = "50 %"
        '
        'Label5
        '
        Me.Label5.Location = New System.Drawing.Point(8, 32)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(72, 23)
        Me.Label5.TabIndex = 115
        Me.Label5.Text = "Temperature:"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'EnvIntAmbTemp
        '
        Me.EnvIntAmbTemp.Location = New System.Drawing.Point(88, 33)
        Me.EnvIntAmbTemp.Name = "EnvIntAmbTemp"
        Me.EnvIntAmbTemp.Size = New System.Drawing.Size(96, 20)
        Me.EnvIntAmbTemp.TabIndex = 107
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
        Me.EnvTimeStep.TabIndex = 106
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
        Me.EnvSmokeviewInterval.TabIndex = 105
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
        Me.EnvSpreadOutInterval.TabIndex = 104
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
        Me.EnvTextOutInterval.TabIndex = 103
        Me.EnvTextOutInterval.Text = "60 s"
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
        Me.EnvSimTime.TabIndex = 102
        Me.EnvSimTime.Text = "3600 s"
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
        Me.HVentSummary.Location = New System.Drawing.Point(121, 30)
        Me.HVentSummary.Name = "HVentSummary"
        Me.HVentSummary.Rows.Count = 2501
        Me.HVentSummary.Rows.DefaultSize = 19
        Me.HVentSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.HVentSummary.SelectionMode = C1.Win.C1FlexGrid.SelectionModeEnum.Row
        Me.HVentSummary.ShowSortPosition = C1.Win.C1FlexGrid.ShowSortPositionEnum.None
        Me.HVentSummary.Size = New System.Drawing.Size(735, 158)
        Me.HVentSummary.StyleInfo = resources.GetString("HVentSummary.StyleInfo")
        Me.HVentSummary.TabIndex = 401
        Me.HVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupHVentGeometry
        '
        Me.GroupHVentGeometry.Controls.Add(Me.HVentName)
        Me.GroupHVentGeometry.Controls.Add(Me.Label6)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFinalLabel)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFinalFraction)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentInitialLabel)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentInitialFraction)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentOpenValueLabel)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentOpenValue)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentTarget)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentTargetLabel)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentOpenCriterion)
        Me.GroupHVentGeometry.Controls.Add(Me.Label82)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFractions)
        Me.GroupHVentGeometry.Controls.Add(Me.Label19)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentOffset)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentFace)
        Me.GroupHVentGeometry.Controls.Add(Me.Label37)
        Me.GroupHVentGeometry.Controls.Add(Me.GroupBox14)
        Me.GroupHVentGeometry.Controls.Add(Me.GroupBox13)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentSoffit)
        Me.GroupHVentGeometry.Controls.Add(Me.Label34)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentSill)
        Me.GroupHVentGeometry.Controls.Add(Me.Label33)
        Me.GroupHVentGeometry.Controls.Add(Me.HVentWidth)
        Me.GroupHVentGeometry.Controls.Add(Me.Label23)
        Me.GroupHVentGeometry.Location = New System.Drawing.Point(77, 234)
        Me.GroupHVentGeometry.Name = "GroupHVentGeometry"
        Me.GroupHVentGeometry.Size = New System.Drawing.Size(844, 346)
        Me.GroupHVentGeometry.TabIndex = 7
        Me.GroupHVentGeometry.TabStop = False
        Me.GroupHVentGeometry.Text = "Vent 1 Geometry"
        '
        'HVentName
        '
        Me.HVentName.Location = New System.Drawing.Point(332, 29)
        Me.HVentName.Name = "HVentName"
        Me.HVentName.Size = New System.Drawing.Size(208, 20)
        Me.HVentName.TabIndex = 752
        Me.HVentName.Text = "HVent 1"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(305, 32)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(21, 13)
        Me.Label6.TabIndex = 751
        Me.Label6.Text = "ID:"
        Me.Label6.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentFinalLabel
        '
        Me.HVentFinalLabel.AutoSize = True
        Me.HVentFinalLabel.Location = New System.Drawing.Point(514, 194)
        Me.HVentFinalLabel.Name = "HVentFinalLabel"
        Me.HVentFinalLabel.Size = New System.Drawing.Size(122, 13)
        Me.HVentFinalLabel.TabIndex = 750
        Me.HVentFinalLabel.Text = "Post-Activation Fraction:"
        Me.HVentFinalLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.HVentFinalLabel.Visible = False
        '
        'HVentFinalFraction
        '
        Me.HVentFinalFraction.Location = New System.Drawing.Point(644, 191)
        Me.HVentFinalFraction.Name = "HVentFinalFraction"
        Me.HVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.HVentFinalFraction.TabIndex = 749
        Me.HVentFinalFraction.Text = "1 m"
        Me.HVentFinalFraction.Visible = False
        '
        'HVentInitialLabel
        '
        Me.HVentInitialLabel.AutoSize = True
        Me.HVentInitialLabel.Location = New System.Drawing.Point(519, 167)
        Me.HVentInitialLabel.Name = "HVentInitialLabel"
        Me.HVentInitialLabel.Size = New System.Drawing.Size(117, 13)
        Me.HVentInitialLabel.TabIndex = 748
        Me.HVentInitialLabel.Text = "Pre-Activation Fraction:"
        Me.HVentInitialLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.HVentInitialLabel.Visible = False
        '
        'HVentInitialFraction
        '
        Me.HVentInitialFraction.Location = New System.Drawing.Point(644, 165)
        Me.HVentInitialFraction.Name = "HVentInitialFraction"
        Me.HVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.HVentInitialFraction.TabIndex = 747
        Me.HVentInitialFraction.Text = "1 m"
        Me.HVentInitialFraction.Visible = False
        '
        'HVentOpenValueLabel
        '
        Me.HVentOpenValueLabel.AutoSize = True
        Me.HVentOpenValueLabel.Location = New System.Drawing.Point(588, 115)
        Me.HVentOpenValueLabel.Name = "HVentOpenValueLabel"
        Me.HVentOpenValueLabel.Size = New System.Drawing.Size(50, 13)
        Me.HVentOpenValueLabel.TabIndex = 745
        Me.HVentOpenValueLabel.Text = "SetPoint:"
        Me.HVentOpenValueLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.HVentOpenValueLabel.Visible = False
        '
        'HVentOpenValue
        '
        Me.HVentOpenValue.Location = New System.Drawing.Point(646, 112)
        Me.HVentOpenValue.Name = "HVentOpenValue"
        Me.HVentOpenValue.Size = New System.Drawing.Size(96, 20)
        Me.HVentOpenValue.TabIndex = 744
        Me.HVentOpenValue.Text = "1 m"
        Me.HVentOpenValue.Visible = False
        '
        'HVentTarget
        '
        Me.HVentTarget.ItemHeight = 13
        Me.HVentTarget.Location = New System.Drawing.Point(644, 138)
        Me.HVentTarget.Name = "HVentTarget"
        Me.HVentTarget.Size = New System.Drawing.Size(98, 21)
        Me.HVentTarget.TabIndex = 743
        Me.HVentTarget.Visible = False
        '
        'HVentTargetLabel
        '
        Me.HVentTargetLabel.AutoSize = True
        Me.HVentTargetLabel.Location = New System.Drawing.Point(561, 141)
        Me.HVentTargetLabel.Name = "HVentTargetLabel"
        Me.HVentTargetLabel.Size = New System.Drawing.Size(77, 13)
        Me.HVentTargetLabel.TabIndex = 741
        Me.HVentTargetLabel.Text = "Trigger Target:"
        Me.HVentTargetLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.HVentTargetLabel.Visible = False
        '
        'HVentOpenCriterion
        '
        Me.HVentOpenCriterion.ItemHeight = 13
        Me.HVentOpenCriterion.Items.AddRange(New Object() {"Time", "Temperature", "Heat Flux"})
        Me.HVentOpenCriterion.Location = New System.Drawing.Point(644, 85)
        Me.HVentOpenCriterion.Name = "HVentOpenCriterion"
        Me.HVentOpenCriterion.Size = New System.Drawing.Size(98, 21)
        Me.HVentOpenCriterion.TabIndex = 742
        Me.HVentOpenCriterion.Text = "Time"
        '
        'Label82
        '
        Me.Label82.AutoSize = True
        Me.Label82.Location = New System.Drawing.Point(530, 89)
        Me.Label82.Name = "Label82"
        Me.Label82.Size = New System.Drawing.Size(108, 13)
        Me.Label82.TabIndex = 740
        Me.Label82.Text = "Open/Close Criterion:"
        Me.Label82.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentFractions
        '
        Me.HVentFractions.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.HVentFractions.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.HVentFractions.AutoClipboard = True
        Me.HVentFractions.ColumnInfo = resources.GetString("HVentFractions.ColumnInfo")
        Me.HVentFractions.ExtendLastCol = True
        Me.HVentFractions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.HVentFractions.Location = New System.Drawing.Point(574, 138)
        Me.HVentFractions.Name = "HVentFractions"
        Me.HVentFractions.Rows.DefaultSize = 19
        Me.HVentFractions.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.HVentFractions.Size = New System.Drawing.Size(190, 176)
        Me.HVentFractions.StyleInfo = resources.GetString("HVentFractions.StyleInfo")
        Me.HVentFractions.TabIndex = 739
        Me.HVentFractions.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'Label19
        '
        Me.Label19.Location = New System.Drawing.Point(132, 285)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(72, 23)
        Me.Label19.TabIndex = 20
        Me.Label19.Text = "Vent Offset:"
        Me.Label19.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentOffset
        '
        Me.HVentOffset.Location = New System.Drawing.Point(212, 285)
        Me.HVentOffset.Name = "HVentOffset"
        Me.HVentOffset.Size = New System.Drawing.Size(96, 20)
        Me.HVentOffset.TabIndex = 415
        Me.HVentOffset.Text = "0 m"
        '
        'HVentFace
        '
        Me.HVentFace.ItemHeight = 13
        Me.HVentFace.Items.AddRange(New Object() {"Front", "Right", "Rear", "Left"})
        Me.HVentFace.Location = New System.Drawing.Point(212, 311)
        Me.HVentFace.Name = "HVentFace"
        Me.HVentFace.Size = New System.Drawing.Size(96, 21)
        Me.HVentFace.TabIndex = 416
        Me.HVentFace.Text = "Front"
        '
        'Label37
        '
        Me.Label37.Location = New System.Drawing.Point(164, 308)
        Me.Label37.Name = "Label37"
        Me.Label37.Size = New System.Drawing.Size(40, 23)
        Me.Label37.TabIndex = 30
        Me.Label37.Text = "Face:"
        Me.Label37.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox14
        '
        Me.GroupBox14.Controls.Add(Me.HVentComp2)
        Me.GroupBox14.Location = New System.Drawing.Point(88, 127)
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
        Me.HVentComp2.TabIndex = 408
        '
        'GroupBox13
        '
        Me.GroupBox13.Controls.Add(Me.HVentComp1)
        Me.GroupBox13.Location = New System.Drawing.Point(88, 67)
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
        Me.HVentComp1.TabIndex = 407
        '
        'HVentSoffit
        '
        Me.HVentSoffit.Location = New System.Drawing.Point(212, 233)
        Me.HVentSoffit.Name = "HVentSoffit"
        Me.HVentSoffit.Size = New System.Drawing.Size(96, 20)
        Me.HVentSoffit.TabIndex = 410
        '
        'Label34
        '
        Me.Label34.Location = New System.Drawing.Point(164, 232)
        Me.Label34.Name = "Label34"
        Me.Label34.Size = New System.Drawing.Size(40, 23)
        Me.Label34.TabIndex = 19
        Me.Label34.Text = "Soffit:"
        Me.Label34.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentSill
        '
        Me.HVentSill.Location = New System.Drawing.Point(212, 207)
        Me.HVentSill.Name = "HVentSill"
        Me.HVentSill.Size = New System.Drawing.Size(96, 20)
        Me.HVentSill.TabIndex = 409
        '
        'Label33
        '
        Me.Label33.Location = New System.Drawing.Point(164, 206)
        Me.Label33.Name = "Label33"
        Me.Label33.Size = New System.Drawing.Size(40, 23)
        Me.Label33.TabIndex = 17
        Me.Label33.Text = "Sill:"
        Me.Label33.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentWidth
        '
        Me.HVentWidth.Location = New System.Drawing.Point(212, 259)
        Me.HVentWidth.Name = "HVentWidth"
        Me.HVentWidth.Size = New System.Drawing.Size(96, 20)
        Me.HVentWidth.TabIndex = 411
        '
        'Label23
        '
        Me.Label23.Location = New System.Drawing.Point(164, 259)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(40, 23)
        Me.Label23.TabIndex = 15
        Me.Label23.Text = "Width:"
        Me.Label23.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'HVentRemove
        '
        Me.HVentRemove.Location = New System.Drawing.Point(679, 205)
        Me.HVentRemove.Name = "HVentRemove"
        Me.HVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.HVentRemove.TabIndex = 406
        Me.HVentRemove.Text = "Remove"
        '
        'HVentAdd
        '
        Me.HVentAdd.Location = New System.Drawing.Point(231, 205)
        Me.HVentAdd.Name = "HVentAdd"
        Me.HVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.HVentAdd.TabIndex = 402
        Me.HVentAdd.Text = "Add"
        '
        'HVentMoveDown
        '
        Me.HVentMoveDown.Location = New System.Drawing.Point(519, 205)
        Me.HVentMoveDown.Name = "HVentMoveDown"
        Me.HVentMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.HVentMoveDown.TabIndex = 405
        Me.HVentMoveDown.Text = "Move Down"
        '
        'HVentMoveUp
        '
        Me.HVentMoveUp.Location = New System.Drawing.Point(423, 205)
        Me.HVentMoveUp.Name = "HVentMoveUp"
        Me.HVentMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.HVentMoveUp.TabIndex = 404
        Me.HVentMoveUp.Text = "Move Up"
        '
        'HVentDup
        '
        Me.HVentDup.Location = New System.Drawing.Point(327, 205)
        Me.HVentDup.Name = "HVentDup"
        Me.HVentDup.Size = New System.Drawing.Size(75, 23)
        Me.HVentDup.TabIndex = 403
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
        Me.VVentSummary.Location = New System.Drawing.Point(227, 25)
        Me.VVentSummary.Name = "VVentSummary"
        Me.VVentSummary.Rows.Count = 2501
        Me.VVentSummary.Rows.DefaultSize = 19
        Me.VVentSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VVentSummary.Size = New System.Drawing.Size(523, 154)
        Me.VVentSummary.StyleInfo = resources.GetString("VVentSummary.StyleInfo")
        Me.VVentSummary.TabIndex = 501
        Me.VVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'VVentRemove
        '
        Me.VVentRemove.Location = New System.Drawing.Point(575, 185)
        Me.VVentRemove.Name = "VVentRemove"
        Me.VVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.VVentRemove.TabIndex = 504
        Me.VVentRemove.Text = "Remove"
        '
        'VVentAdd
        '
        Me.VVentAdd.Location = New System.Drawing.Point(327, 185)
        Me.VVentAdd.Name = "VVentAdd"
        Me.VVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.VVentAdd.TabIndex = 502
        Me.VVentAdd.Text = "Add"
        '
        'VVentDup
        '
        Me.VVentDup.Location = New System.Drawing.Point(423, 185)
        Me.VVentDup.Name = "VVentDup"
        Me.VVentDup.Size = New System.Drawing.Size(75, 23)
        Me.VVentDup.TabIndex = 503
        Me.VVentDup.Text = "Duplicate"
        '
        'GroupVVents
        '
        Me.GroupVVents.Controls.Add(Me.VVentName)
        Me.GroupVVents.Controls.Add(Me.Label8)
        Me.GroupVVents.Controls.Add(Me.VVentFinalLabel)
        Me.GroupVVents.Controls.Add(Me.VVentFractions)
        Me.GroupVVents.Controls.Add(Me.VVentFinalFraction)
        Me.GroupVVents.Controls.Add(Me.VVentInitialLabel)
        Me.GroupVVents.Controls.Add(Me.VVentInitialFraction)
        Me.GroupVVents.Controls.Add(Me.VVentYOffset)
        Me.GroupVVents.Controls.Add(Me.Label68)
        Me.GroupVVents.Controls.Add(Me.VVentXOffset)
        Me.GroupVVents.Controls.Add(Me.Label55)
        Me.GroupVVents.Controls.Add(Me.VVentOpenValueLabel)
        Me.GroupVVents.Controls.Add(Me.VVentOpenValue)
        Me.GroupVVents.Controls.Add(Me.GroupBox4)
        Me.GroupVVents.Controls.Add(Me.VVentTarget)
        Me.GroupVVents.Controls.Add(Me.VVentTargetLabel)
        Me.GroupVVents.Controls.Add(Me.VVentOpenCriterion)
        Me.GroupVVents.Controls.Add(Me.Label80)
        Me.GroupVVents.Controls.Add(Me.VVentShape)
        Me.GroupVVents.Controls.Add(Me.Label40)
        Me.GroupVVents.Controls.Add(Me.GroupBox18)
        Me.GroupVVents.Controls.Add(Me.VVentArea)
        Me.GroupVVents.Controls.Add(Me.Label46)
        Me.GroupVVents.Location = New System.Drawing.Point(116, 234)
        Me.GroupVVents.Name = "GroupVVents"
        Me.GroupVVents.Size = New System.Drawing.Size(728, 332)
        Me.GroupVVents.TabIndex = 5
        Me.GroupVVents.TabStop = False
        Me.GroupVVents.Text = "Vent 1 Geometry"
        '
        'VVentName
        '
        Me.VVentName.Location = New System.Drawing.Point(274, 33)
        Me.VVentName.Name = "VVentName"
        Me.VVentName.Size = New System.Drawing.Size(208, 20)
        Me.VVentName.TabIndex = 754
        Me.VVentName.Text = "VVent 1"
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(247, 36)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(21, 13)
        Me.Label8.TabIndex = 753
        Me.Label8.Text = "ID:"
        Me.Label8.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentFinalLabel
        '
        Me.VVentFinalLabel.AutoSize = True
        Me.VVentFinalLabel.Location = New System.Drawing.Point(459, 191)
        Me.VVentFinalLabel.Name = "VVentFinalLabel"
        Me.VVentFinalLabel.Size = New System.Drawing.Size(122, 13)
        Me.VVentFinalLabel.TabIndex = 746
        Me.VVentFinalLabel.Text = "Post-Activation Fraction:"
        Me.VVentFinalLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.VVentFinalLabel.Visible = False
        '
        'VVentFractions
        '
        Me.VVentFractions.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.VVentFractions.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.VVentFractions.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.VVentFractions.AutoClipboard = True
        Me.VVentFractions.ColumnInfo = resources.GetString("VVentFractions.ColumnInfo")
        Me.VVentFractions.ExtendLastCol = True
        Me.VVentFractions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VVentFractions.Location = New System.Drawing.Point(515, 135)
        Me.VVentFractions.Name = "VVentFractions"
        Me.VVentFractions.Rows.DefaultSize = 19
        Me.VVentFractions.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VVentFractions.Size = New System.Drawing.Size(190, 176)
        Me.VVentFractions.StyleInfo = resources.GetString("VVentFractions.StyleInfo")
        Me.VVentFractions.TabIndex = 509
        Me.VVentFractions.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'VVentFinalFraction
        '
        Me.VVentFinalFraction.Location = New System.Drawing.Point(589, 188)
        Me.VVentFinalFraction.Name = "VVentFinalFraction"
        Me.VVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.VVentFinalFraction.TabIndex = 745
        Me.VVentFinalFraction.Text = "1 m"
        Me.VVentFinalFraction.Visible = False
        '
        'VVentInitialLabel
        '
        Me.VVentInitialLabel.AutoSize = True
        Me.VVentInitialLabel.Location = New System.Drawing.Point(464, 164)
        Me.VVentInitialLabel.Name = "VVentInitialLabel"
        Me.VVentInitialLabel.Size = New System.Drawing.Size(117, 13)
        Me.VVentInitialLabel.TabIndex = 744
        Me.VVentInitialLabel.Text = "Pre-Activation Fraction:"
        Me.VVentInitialLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.VVentInitialLabel.Visible = False
        '
        'VVentInitialFraction
        '
        Me.VVentInitialFraction.Location = New System.Drawing.Point(589, 162)
        Me.VVentInitialFraction.Name = "VVentInitialFraction"
        Me.VVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.VVentInitialFraction.TabIndex = 743
        Me.VVentInitialFraction.Text = "1 m"
        Me.VVentInitialFraction.Visible = False
        '
        'VVentYOffset
        '
        Me.VVentYOffset.Location = New System.Drawing.Point(206, 299)
        Me.VVentYOffset.Name = "VVentYOffset"
        Me.VVentYOffset.Size = New System.Drawing.Size(96, 20)
        Me.VVentYOffset.TabIndex = 742
        Me.VVentYOffset.Text = "1 m"
        '
        'Label68
        '
        Me.Label68.AutoSize = True
        Me.Label68.Location = New System.Drawing.Point(177, 302)
        Me.Label68.Name = "Label68"
        Me.Label68.Size = New System.Drawing.Size(17, 13)
        Me.Label68.TabIndex = 741
        Me.Label68.Text = "Y:"
        Me.Label68.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentXOffset
        '
        Me.VVentXOffset.Location = New System.Drawing.Point(206, 273)
        Me.VVentXOffset.Name = "VVentXOffset"
        Me.VVentXOffset.Size = New System.Drawing.Size(96, 20)
        Me.VVentXOffset.TabIndex = 740
        Me.VVentXOffset.Text = "1 m"
        '
        'Label55
        '
        Me.Label55.AutoSize = True
        Me.Label55.Location = New System.Drawing.Point(121, 276)
        Me.Label55.Name = "Label55"
        Me.Label55.Size = New System.Drawing.Size(73, 13)
        Me.Label55.TabIndex = 739
        Me.Label55.Text = "Vent Offset X:"
        Me.Label55.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentOpenValueLabel
        '
        Me.VVentOpenValueLabel.AutoSize = True
        Me.VVentOpenValueLabel.Location = New System.Drawing.Point(531, 112)
        Me.VVentOpenValueLabel.Name = "VVentOpenValueLabel"
        Me.VVentOpenValueLabel.Size = New System.Drawing.Size(50, 13)
        Me.VVentOpenValueLabel.TabIndex = 738
        Me.VVentOpenValueLabel.Text = "SetPoint:"
        Me.VVentOpenValueLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.VVentOpenValueLabel.Visible = False
        '
        'VVentOpenValue
        '
        Me.VVentOpenValue.Location = New System.Drawing.Point(589, 109)
        Me.VVentOpenValue.Name = "VVentOpenValue"
        Me.VVentOpenValue.Size = New System.Drawing.Size(96, 20)
        Me.VVentOpenValue.TabIndex = 737
        Me.VVentOpenValue.Text = "1 m"
        Me.VVentOpenValue.Visible = False
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.VVentCompBottom)
        Me.GroupBox4.Location = New System.Drawing.Point(24, 131)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(393, 51)
        Me.GroupBox4.TabIndex = 736
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Bottom Compartment"
        '
        'VVentCompBottom
        '
        Me.VVentCompBottom.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VVentCompBottom.ItemHeight = 13
        Me.VVentCompBottom.Location = New System.Drawing.Point(32, 19)
        Me.VVentCompBottom.Name = "VVentCompBottom"
        Me.VVentCompBottom.Size = New System.Drawing.Size(328, 21)
        Me.VVentCompBottom.TabIndex = 506
        '
        'VVentTarget
        '
        Me.VVentTarget.ItemHeight = 13
        Me.VVentTarget.Location = New System.Drawing.Point(587, 135)
        Me.VVentTarget.Name = "VVentTarget"
        Me.VVentTarget.Size = New System.Drawing.Size(98, 21)
        Me.VVentTarget.TabIndex = 735
        Me.VVentTarget.Visible = False
        '
        'VVentTargetLabel
        '
        Me.VVentTargetLabel.AutoSize = True
        Me.VVentTargetLabel.Location = New System.Drawing.Point(490, 138)
        Me.VVentTargetLabel.Name = "VVentTargetLabel"
        Me.VVentTargetLabel.Size = New System.Drawing.Size(91, 13)
        Me.VVentTargetLabel.TabIndex = 732
        Me.VVentTargetLabel.Text = "Activation Target:"
        Me.VVentTargetLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.VVentTargetLabel.Visible = False
        '
        'VVentOpenCriterion
        '
        Me.VVentOpenCriterion.ItemHeight = 13
        Me.VVentOpenCriterion.Items.AddRange(New Object() {"Time", "Temperature", "Heat Flux"})
        Me.VVentOpenCriterion.Location = New System.Drawing.Point(587, 82)
        Me.VVentOpenCriterion.Name = "VVentOpenCriterion"
        Me.VVentOpenCriterion.Size = New System.Drawing.Size(98, 21)
        Me.VVentOpenCriterion.TabIndex = 733
        Me.VVentOpenCriterion.Text = "Time"
        '
        'Label80
        '
        Me.Label80.AutoSize = True
        Me.Label80.Location = New System.Drawing.Point(473, 86)
        Me.Label80.Name = "Label80"
        Me.Label80.Size = New System.Drawing.Size(108, 13)
        Me.Label80.TabIndex = 730
        Me.Label80.Text = "Open/Close Criterion:"
        Me.Label80.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VVentShape
        '
        Me.VVentShape.ItemHeight = 13
        Me.VVentShape.Items.AddRange(New Object() {"Round", "Square"})
        Me.VVentShape.Location = New System.Drawing.Point(206, 246)
        Me.VVentShape.Name = "VVentShape"
        Me.VVentShape.Size = New System.Drawing.Size(96, 21)
        Me.VVentShape.TabIndex = 508
        Me.VVentShape.Text = "Round"
        '
        'Label40
        '
        Me.Label40.Location = New System.Drawing.Point(102, 245)
        Me.Label40.Name = "Label40"
        Me.Label40.Size = New System.Drawing.Size(96, 23)
        Me.Label40.TabIndex = 32
        Me.Label40.Text = "Shape:"
        Me.Label40.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'GroupBox18
        '
        Me.GroupBox18.Controls.Add(Me.VVentCompTop)
        Me.GroupBox18.Location = New System.Drawing.Point(24, 74)
        Me.GroupBox18.Name = "GroupBox18"
        Me.GroupBox18.Size = New System.Drawing.Size(393, 51)
        Me.GroupBox18.TabIndex = 6
        Me.GroupBox18.TabStop = False
        Me.GroupBox18.Text = "Top Compartment"
        '
        'VVentCompTop
        '
        Me.VVentCompTop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.VVentCompTop.ItemHeight = 13
        Me.VVentCompTop.Location = New System.Drawing.Point(33, 19)
        Me.VVentCompTop.Name = "VVentCompTop"
        Me.VVentCompTop.Size = New System.Drawing.Size(328, 21)
        Me.VVentCompTop.TabIndex = 505
        '
        'VVentArea
        '
        Me.VVentArea.Location = New System.Drawing.Point(206, 220)
        Me.VVentArea.Name = "VVentArea"
        Me.VVentArea.Size = New System.Drawing.Size(96, 20)
        Me.VVentArea.TabIndex = 507
        Me.VVentArea.Text = "1 m"
        '
        'Label46
        '
        Me.Label46.AutoSize = True
        Me.Label46.Location = New System.Drawing.Point(86, 223)
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
        Me.CompSummary.Rows.Count = 101
        Me.CompSummary.Rows.DefaultSize = 19
        Me.CompSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.CompSummary.Size = New System.Drawing.Size(902, 161)
        Me.CompSummary.StyleInfo = resources.GetString("CompSummary.StyleInfo")
        Me.CompSummary.TabIndex = 301
        Me.CompSummary.TabStop = False
        Me.CompSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'CompRemove
        '
        Me.CompRemove.Location = New System.Drawing.Point(675, 183)
        Me.CompRemove.Name = "CompRemove"
        Me.CompRemove.Size = New System.Drawing.Size(75, 23)
        Me.CompRemove.TabIndex = 306
        Me.CompRemove.Text = "Remove"
        '
        'CompAdd
        '
        Me.CompAdd.Location = New System.Drawing.Point(227, 183)
        Me.CompAdd.Name = "CompAdd"
        Me.CompAdd.Size = New System.Drawing.Size(75, 23)
        Me.CompAdd.TabIndex = 302
        Me.CompAdd.Text = "Add"
        '
        'CompMoveDown
        '
        Me.CompMoveDown.Location = New System.Drawing.Point(515, 183)
        Me.CompMoveDown.Name = "CompMoveDown"
        Me.CompMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.CompMoveDown.TabIndex = 305
        Me.CompMoveDown.Text = "Move Down"
        '
        'CompMoveUp
        '
        Me.CompMoveUp.Location = New System.Drawing.Point(419, 183)
        Me.CompMoveUp.Name = "CompMoveUp"
        Me.CompMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.CompMoveUp.TabIndex = 304
        Me.CompMoveUp.Text = "Move Up"
        '
        'CompDup
        '
        Me.CompDup.Location = New System.Drawing.Point(323, 183)
        Me.CompDup.Name = "CompDup"
        Me.CompDup.Size = New System.Drawing.Size(75, 23)
        Me.CompDup.TabIndex = 303
        Me.CompDup.Text = "Duplicate"
        '
        'GroupCompartments
        '
        Me.GroupCompartments.Controls.Add(Me.CompName)
        Me.GroupCompartments.Controls.Add(Me.Label9)
        Me.GroupCompartments.Controls.Add(Me.GroupBox5)
        Me.GroupCompartments.Controls.Add(Me.GroupFlowCharacteristics)
        Me.GroupCompartments.Controls.Add(Me.GroupCompSurfaces)
        Me.GroupCompartments.Location = New System.Drawing.Point(16, 212)
        Me.GroupCompartments.Name = "GroupCompartments"
        Me.GroupCompartments.Size = New System.Drawing.Size(944, 368)
        Me.GroupCompartments.TabIndex = 7
        Me.GroupCompartments.TabStop = False
        Me.GroupCompartments.Text = "Compartment 1"
        '
        'CompName
        '
        Me.CompName.Location = New System.Drawing.Point(382, 15)
        Me.CompName.Name = "CompName"
        Me.CompName.Size = New System.Drawing.Size(208, 20)
        Me.CompName.TabIndex = 307
        Me.CompName.Text = "Compartment 1"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(355, 18)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(21, 13)
        Me.Label9.TabIndex = 29
        Me.Label9.Text = "ID:"
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
        Me.CompWidth.TabIndex = 308
        '
        'CompZPosition
        '
        Me.CompZPosition.Location = New System.Drawing.Point(261, 95)
        Me.CompZPosition.Name = "CompZPosition"
        Me.CompZPosition.Size = New System.Drawing.Size(96, 20)
        Me.CompZPosition.TabIndex = 313
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
        Me.CompYPosition.TabIndex = 312
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
        Me.CompXPosition.TabIndex = 311
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
        Me.CompHeight.TabIndex = 310
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
        Me.CompDepth.TabIndex = 309
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
        Me.CompNormal.TabIndex = 314
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
        Me.CompCorridor.TabIndex = 316
        Me.CompCorridor.Text = "Corridor (Revised ceiling jet)"
        Me.CompCorridor.UseVisualStyleBackColor = True
        '
        'CompShaft
        '
        Me.CompShaft.AutoSize = True
        Me.CompShaft.Location = New System.Drawing.Point(37, 81)
        Me.CompShaft.Name = "CompShaft"
        Me.CompShaft.Size = New System.Drawing.Size(145, 17)
        Me.CompShaft.TabIndex = 315
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
        Me.CompVariableArea.TabIndex = 317
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
        Me.CompFloor.TabIndex = 320
        '
        'CompWalls
        '
        Me.CompWalls.ItemHeight = 13
        Me.CompWalls.Location = New System.Drawing.Point(350, 20)
        Me.CompWalls.Name = "CompWalls"
        Me.CompWalls.Size = New System.Drawing.Size(192, 21)
        Me.CompWalls.TabIndex = 319
        '
        'CompCeiling
        '
        Me.CompCeiling.ItemHeight = 13
        Me.CompCeiling.Location = New System.Drawing.Point(61, 19)
        Me.CompCeiling.Name = "CompCeiling"
        Me.CompCeiling.Size = New System.Drawing.Size(192, 21)
        Me.CompCeiling.TabIndex = 318
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
        Me.MainView.TabIndex = 5
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
        Me.MVentSummary.Location = New System.Drawing.Point(53, 25)
        Me.MVentSummary.Name = "MVentSummary"
        Me.MVentSummary.Rows.Count = 2501
        Me.MVentSummary.Rows.DefaultSize = 19
        Me.MVentSummary.Size = New System.Drawing.Size(862, 120)
        Me.MVentSummary.StyleInfo = resources.GetString("MVentSummary.StyleInfo")
        Me.MVentSummary.TabIndex = 601
        Me.MVentSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'MVentRemove
        '
        Me.MVentRemove.Location = New System.Drawing.Point(678, 151)
        Me.MVentRemove.Name = "MVentRemove"
        Me.MVentRemove.Size = New System.Drawing.Size(75, 23)
        Me.MVentRemove.TabIndex = 604
        Me.MVentRemove.Text = "Remove"
        '
        'MVentAdd
        '
        Me.MVentAdd.Location = New System.Drawing.Point(230, 151)
        Me.MVentAdd.Name = "MVentAdd"
        Me.MVentAdd.Size = New System.Drawing.Size(75, 23)
        Me.MVentAdd.TabIndex = 602
        Me.MVentAdd.Text = "Add"
        '
        'MVentDup
        '
        Me.MVentDup.Location = New System.Drawing.Point(326, 151)
        Me.MVentDup.Name = "MVentDup"
        Me.MVentDup.Size = New System.Drawing.Size(75, 23)
        Me.MVentDup.TabIndex = 603
        Me.MVentDup.Text = "Duplicate"
        '
        'GroupMVents
        '
        Me.GroupMVents.Controls.Add(Me.MVentName)
        Me.GroupMVents.Controls.Add(Me.Label24)
        Me.GroupMVents.Controls.Add(Me.MventFinalLabel)
        Me.GroupMVents.Controls.Add(Me.MVentFinalFraction)
        Me.GroupMVents.Controls.Add(Me.MVentInitialLabel)
        Me.GroupMVents.Controls.Add(Me.MVentInitialFraction)
        Me.GroupMVents.Controls.Add(Me.MVentYOffset)
        Me.GroupMVents.Controls.Add(Me.Label98)
        Me.GroupMVents.Controls.Add(Me.MVentXOffset)
        Me.GroupMVents.Controls.Add(Me.Label99)
        Me.GroupMVents.Controls.Add(Me.MVentOpenValueLabel)
        Me.GroupMVents.Controls.Add(Me.MVentOpenValue)
        Me.GroupMVents.Controls.Add(Me.MVentTarget)
        Me.GroupMVents.Controls.Add(Me.MVentTargetLabel)
        Me.GroupMVents.Controls.Add(Me.MVentOpenCriterion)
        Me.GroupMVents.Controls.Add(Me.Label97)
        Me.GroupMVents.Controls.Add(Me.MVentFractions)
        Me.GroupMVents.Controls.Add(Me.MVentFilterTime)
        Me.GroupMVents.Controls.Add(Me.Label38)
        Me.GroupMVents.Controls.Add(Me.MVentFilterEfficiency)
        Me.GroupMVents.Controls.Add(Me.Label54)
        Me.GroupMVents.Controls.Add(Me.MVentZero)
        Me.GroupMVents.Controls.Add(Me.Label41)
        Me.GroupMVents.Controls.Add(Me.GroupBox20)
        Me.GroupMVents.Controls.Add(Me.MVentDropoff)
        Me.GroupMVents.Controls.Add(Me.Label42)
        Me.GroupMVents.Controls.Add(Me.GroupBox21)
        Me.GroupMVents.Controls.Add(Me.MVentFlow)
        Me.GroupMVents.Controls.Add(Me.Label45)
        Me.GroupMVents.Controls.Add(Me.Label47)
        Me.GroupMVents.Location = New System.Drawing.Point(128, 189)
        Me.GroupMVents.Name = "GroupMVents"
        Me.GroupMVents.Size = New System.Drawing.Size(720, 390)
        Me.GroupMVents.TabIndex = 5
        Me.GroupMVents.TabStop = False
        Me.GroupMVents.Text = "Vent 1 Geometry"
        '
        'MVentName
        '
        Me.MVentName.Location = New System.Drawing.Point(270, 18)
        Me.MVentName.Name = "MVentName"
        Me.MVentName.Size = New System.Drawing.Size(208, 20)
        Me.MVentName.TabIndex = 756
        Me.MVentName.Text = "MVent 1"
        '
        'Label24
        '
        Me.Label24.AutoSize = True
        Me.Label24.Location = New System.Drawing.Point(243, 21)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(21, 13)
        Me.Label24.TabIndex = 755
        Me.Label24.Text = "ID:"
        Me.Label24.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MventFinalLabel
        '
        Me.MventFinalLabel.AutoSize = True
        Me.MventFinalLabel.Location = New System.Drawing.Point(457, 177)
        Me.MventFinalLabel.Name = "MventFinalLabel"
        Me.MventFinalLabel.Size = New System.Drawing.Size(122, 13)
        Me.MventFinalLabel.TabIndex = 753
        Me.MventFinalLabel.Text = "Post-Activation Fraction:"
        Me.MventFinalLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.MventFinalLabel.Visible = False
        '
        'MVentFinalFraction
        '
        Me.MVentFinalFraction.Location = New System.Drawing.Point(587, 174)
        Me.MVentFinalFraction.Name = "MVentFinalFraction"
        Me.MVentFinalFraction.Size = New System.Drawing.Size(96, 20)
        Me.MVentFinalFraction.TabIndex = 752
        Me.MVentFinalFraction.Text = "1 m"
        Me.MVentFinalFraction.Visible = False
        '
        'MVentInitialLabel
        '
        Me.MVentInitialLabel.AutoSize = True
        Me.MVentInitialLabel.Location = New System.Drawing.Point(462, 150)
        Me.MVentInitialLabel.Name = "MVentInitialLabel"
        Me.MVentInitialLabel.Size = New System.Drawing.Size(117, 13)
        Me.MVentInitialLabel.TabIndex = 751
        Me.MVentInitialLabel.Text = "Pre-Activation Fraction:"
        Me.MVentInitialLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.MVentInitialLabel.Visible = False
        '
        'MVentInitialFraction
        '
        Me.MVentInitialFraction.Location = New System.Drawing.Point(587, 148)
        Me.MVentInitialFraction.Name = "MVentInitialFraction"
        Me.MVentInitialFraction.Size = New System.Drawing.Size(96, 20)
        Me.MVentInitialFraction.TabIndex = 750
        Me.MVentInitialFraction.Text = "1 m"
        Me.MVentInitialFraction.Visible = False
        '
        'MVentYOffset
        '
        Me.MVentYOffset.Location = New System.Drawing.Point(319, 337)
        Me.MVentYOffset.Name = "MVentYOffset"
        Me.MVentYOffset.Size = New System.Drawing.Size(96, 20)
        Me.MVentYOffset.TabIndex = 749
        Me.MVentYOffset.Text = "1 m"
        '
        'Label98
        '
        Me.Label98.AutoSize = True
        Me.Label98.Location = New System.Drawing.Point(290, 340)
        Me.Label98.Name = "Label98"
        Me.Label98.Size = New System.Drawing.Size(17, 13)
        Me.Label98.TabIndex = 748
        Me.Label98.Text = "Y:"
        Me.Label98.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentXOffset
        '
        Me.MVentXOffset.Location = New System.Drawing.Point(319, 311)
        Me.MVentXOffset.Name = "MVentXOffset"
        Me.MVentXOffset.Size = New System.Drawing.Size(96, 20)
        Me.MVentXOffset.TabIndex = 747
        Me.MVentXOffset.Text = "1 m"
        '
        'Label99
        '
        Me.Label99.AutoSize = True
        Me.Label99.Location = New System.Drawing.Point(234, 314)
        Me.Label99.Name = "Label99"
        Me.Label99.Size = New System.Drawing.Size(73, 13)
        Me.Label99.TabIndex = 746
        Me.Label99.Text = "Vent Offset X:"
        Me.Label99.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentOpenValueLabel
        '
        Me.MVentOpenValueLabel.AutoSize = True
        Me.MVentOpenValueLabel.Location = New System.Drawing.Point(529, 98)
        Me.MVentOpenValueLabel.Name = "MVentOpenValueLabel"
        Me.MVentOpenValueLabel.Size = New System.Drawing.Size(50, 13)
        Me.MVentOpenValueLabel.TabIndex = 745
        Me.MVentOpenValueLabel.Text = "SetPoint:"
        Me.MVentOpenValueLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.MVentOpenValueLabel.Visible = False
        '
        'MVentOpenValue
        '
        Me.MVentOpenValue.Location = New System.Drawing.Point(587, 95)
        Me.MVentOpenValue.Name = "MVentOpenValue"
        Me.MVentOpenValue.Size = New System.Drawing.Size(96, 20)
        Me.MVentOpenValue.TabIndex = 744
        Me.MVentOpenValue.Text = "1 m"
        Me.MVentOpenValue.Visible = False
        '
        'MVentTarget
        '
        Me.MVentTarget.ItemHeight = 13
        Me.MVentTarget.Location = New System.Drawing.Point(585, 121)
        Me.MVentTarget.Name = "MVentTarget"
        Me.MVentTarget.Size = New System.Drawing.Size(98, 21)
        Me.MVentTarget.TabIndex = 743
        Me.MVentTarget.Visible = False
        '
        'MVentTargetLabel
        '
        Me.MVentTargetLabel.AutoSize = True
        Me.MVentTargetLabel.Location = New System.Drawing.Point(502, 124)
        Me.MVentTargetLabel.Name = "MVentTargetLabel"
        Me.MVentTargetLabel.Size = New System.Drawing.Size(77, 13)
        Me.MVentTargetLabel.TabIndex = 741
        Me.MVentTargetLabel.Text = "Trigger Target:"
        Me.MVentTargetLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.MVentTargetLabel.Visible = False
        '
        'MVentOpenCriterion
        '
        Me.MVentOpenCriterion.ItemHeight = 13
        Me.MVentOpenCriterion.Items.AddRange(New Object() {"Time", "Temperature", "Heat Flux"})
        Me.MVentOpenCriterion.Location = New System.Drawing.Point(585, 68)
        Me.MVentOpenCriterion.Name = "MVentOpenCriterion"
        Me.MVentOpenCriterion.Size = New System.Drawing.Size(98, 21)
        Me.MVentOpenCriterion.TabIndex = 742
        Me.MVentOpenCriterion.Text = "Time"
        '
        'Label97
        '
        Me.Label97.AutoSize = True
        Me.Label97.Location = New System.Drawing.Point(471, 72)
        Me.Label97.Name = "Label97"
        Me.Label97.Size = New System.Drawing.Size(108, 13)
        Me.Label97.TabIndex = 740
        Me.Label97.Text = "Open/Close Criterion:"
        Me.Label97.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFractions
        '
        Me.MVentFractions.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.MVentFractions.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.MVentFractions.AutoClipboard = True
        Me.MVentFractions.ColumnInfo = resources.GetString("MVentFractions.ColumnInfo")
        Me.MVentFractions.ExtendLastCol = True
        Me.MVentFractions.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.MVentFractions.Location = New System.Drawing.Point(515, 121)
        Me.MVentFractions.Name = "MVentFractions"
        Me.MVentFractions.Rows.DefaultSize = 19
        Me.MVentFractions.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.MVentFractions.Size = New System.Drawing.Size(190, 176)
        Me.MVentFractions.StyleInfo = resources.GetString("MVentFractions.StyleInfo")
        Me.MVentFractions.TabIndex = 739
        Me.MVentFractions.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'MVentFilterTime
        '
        Me.MVentFilterTime.Location = New System.Drawing.Point(587, 337)
        Me.MVentFilterTime.Name = "MVentFilterTime"
        Me.MVentFilterTime.Size = New System.Drawing.Size(96, 20)
        Me.MVentFilterTime.TabIndex = 620
        '
        'Label38
        '
        Me.Label38.AutoSize = True
        Me.Label38.Location = New System.Drawing.Point(506, 340)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(75, 13)
        Me.Label38.TabIndex = 62
        Me.Label38.Text = "Begin Filter At:"
        Me.Label38.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentFilterEfficiency
        '
        Me.MVentFilterEfficiency.Location = New System.Drawing.Point(587, 311)
        Me.MVentFilterEfficiency.Name = "MVentFilterEfficiency"
        Me.MVentFilterEfficiency.Size = New System.Drawing.Size(96, 20)
        Me.MVentFilterEfficiency.TabIndex = 619
        Me.MVentFilterEfficiency.Text = "1"
        '
        'Label54
        '
        Me.Label54.AutoSize = True
        Me.Label54.Location = New System.Drawing.Point(500, 314)
        Me.Label54.Name = "Label54"
        Me.Label54.Size = New System.Drawing.Size(81, 13)
        Me.Label54.TabIndex = 61
        Me.Label54.Text = "Filter Efficiency:"
        Me.Label54.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'MVentZero
        '
        Me.MVentZero.Location = New System.Drawing.Point(111, 363)
        Me.MVentZero.Name = "MVentZero"
        Me.MVentZero.Size = New System.Drawing.Size(96, 20)
        Me.MVentZero.TabIndex = 615
        Me.MVentZero.Text = "300 Pa"
        '
        'Label41
        '
        Me.Label41.AutoSize = True
        Me.Label41.Location = New System.Drawing.Point(31, 365)
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
        Me.GroupBox20.Location = New System.Drawing.Point(71, 162)
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
        Me.MVentToOrientation.TabIndex = 612
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
        Me.MVentToHeight.TabIndex = 611
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
        Me.MVentToArea.TabIndex = 610
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
        Me.MventToComp.TabIndex = 609
        '
        'MVentDropoff
        '
        Me.MVentDropoff.Location = New System.Drawing.Point(111, 337)
        Me.MVentDropoff.Name = "MVentDropoff"
        Me.MVentDropoff.Size = New System.Drawing.Size(96, 20)
        Me.MVentDropoff.TabIndex = 614
        Me.MVentDropoff.Text = "200 Pa"
        '
        'Label42
        '
        Me.Label42.AutoSize = True
        Me.Label42.Location = New System.Drawing.Point(15, 339)
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
        Me.GroupBox21.Location = New System.Drawing.Point(71, 44)
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
        Me.MVentFromOrientation.TabIndex = 608
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
        Me.MVentFromHeight.TabIndex = 607
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
        Me.MVentFromArea.TabIndex = 606
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
        Me.MVentFromComp.TabIndex = 605
        '
        'MVentFlow
        '
        Me.MVentFlow.Location = New System.Drawing.Point(111, 311)
        Me.MVentFlow.Name = "MVentFlow"
        Me.MVentFlow.Size = New System.Drawing.Size(96, 20)
        Me.MVentFlow.TabIndex = 613
        Me.MVentFlow.Text = "1 m^3/s"
        '
        'Label45
        '
        Me.Label45.Location = New System.Drawing.Point(31, 309)
        Me.Label45.Name = "Label45"
        Me.Label45.Size = New System.Drawing.Size(72, 24)
        Me.Label45.TabIndex = 21
        Me.Label45.Text = "Flow Rate:"
        Me.Label45.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label47
        '
        Me.Label47.Location = New System.Drawing.Point(313, 136)
        Me.Label47.Name = "Label47"
        Me.Label47.Size = New System.Drawing.Size(40, 23)
        Me.Label47.TabIndex = 19
        Me.Label47.Text = "Soffit:"
        Me.Label47.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TabFires
        '
        Me.TabFires.Controls.Add(Me.Label60)
        Me.TabFires.Controls.Add(Me.FireDataSS)
        Me.TabFires.Controls.Add(Me.FireAdd)
        Me.TabFires.Controls.Add(Me.FirePlot)
        Me.TabFires.Controls.Add(Me.Label113)
        Me.TabFires.Controls.Add(Me.FireRemoveInstance)
        Me.TabFires.Controls.Add(Me.Label111)
        Me.TabFires.Controls.Add(Me.FireDefinitionName)
        Me.TabFires.Controls.Add(Me.FireYPosition)
        Me.TabFires.Controls.Add(Me.FireHoC)
        Me.TabFires.Controls.Add(Me.Label109)
        Me.TabFires.Controls.Add(Me.FireC)
        Me.TabFires.Controls.Add(Me.FireH)
        Me.TabFires.Controls.Add(Me.FireSummary)
        Me.TabFires.Controls.Add(Me.Label108)
        Me.TabFires.Controls.Add(Me.Label110)
        Me.TabFires.Controls.Add(Me.Label114)
        Me.TabFires.Controls.Add(Me.Label106)
        Me.TabFires.Controls.Add(Me.FireCl)
        Me.TabFires.Controls.Add(Me.FireAddt2)
        Me.TabFires.Controls.Add(Me.FireRadiativeFraction)
        Me.TabFires.Controls.Add(Me.FireInstanceName)
        Me.TabFires.Controls.Add(Me.ReferencedFireDefinition)
        Me.TabFires.Controls.Add(Me.FireO)
        Me.TabFires.Controls.Add(Me.Label63)
        Me.TabFires.Controls.Add(Me.Label112)
        Me.TabFires.Controls.Add(Me.Label58)
        Me.TabFires.Controls.Add(Me.FireN)
        Me.TabFires.Controls.Add(Me.Label36)
        Me.TabFires.Controls.Add(Me.FireXPosition)
        Me.TabFires.Controls.Add(Me.Label70)
        Me.TabFires.Controls.Add(Me.FireIgnitionValue)
        Me.TabFires.Controls.Add(Me.FireFromFile)
        Me.TabFires.Controls.Add(Me.Label107)
        Me.TabFires.Controls.Add(Me.FireIgnitionCriteria)
        Me.TabFires.Controls.Add(Me.FireTarget)
        Me.TabFires.Controls.Add(Me.Label52)
        Me.TabFires.Controls.Add(Me.Label69)
        Me.TabFires.Controls.Add(Me.FireComp)
        Me.TabFires.Location = New System.Drawing.Point(4, 22)
        Me.TabFires.Name = "TabFires"
        Me.TabFires.Size = New System.Drawing.Size(976, 592)
        Me.TabFires.TabIndex = 1
        Me.TabFires.Text = "Fires"
        Me.TabFires.Visible = False
        '
        'Label60
        '
        Me.Label60.AutoSize = True
        Me.Label60.Location = New System.Drawing.Point(46, 431)
        Me.Label60.Name = "Label60"
        Me.Label60.Size = New System.Drawing.Size(91, 13)
        Me.Label60.TabIndex = 732
        Me.Label60.Text = "Fire Properties ID:"
        Me.Label60.TextAlign = System.Drawing.ContentAlignment.MiddleRight
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
        Me.FireDataSS.Location = New System.Drawing.Point(369, 238)
        Me.FireDataSS.Name = "FireDataSS"
        Me.FireDataSS.Rows.Count = 101
        Me.FireDataSS.Rows.DefaultSize = 17
        Me.FireDataSS.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.FireDataSS.Size = New System.Drawing.Size(596, 113)
        Me.FireDataSS.TabIndex = 731
        '
        'FireAdd
        '
        Me.FireAdd.Location = New System.Drawing.Point(323, 156)
        Me.FireAdd.Name = "FireAdd"
        Me.FireAdd.Size = New System.Drawing.Size(75, 23)
        Me.FireAdd.TabIndex = 702
        Me.FireAdd.Text = "Add New"
        '
        'FirePlot
        '
        Me.FirePlot.AutoScaleAutoGeneratedAxes = True
        Me.FirePlot.AutoScaleTitle = False
        Me.FirePlot.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.FirePlot.DateTimeToolTip = False
        Me.FirePlot.Legend = Nothing
        Me.FirePlot.LegendZOrder = -1
        Me.FirePlot.Location = New System.Drawing.Point(369, 387)
        Me.FirePlot.Name = "FirePlot"
        Me.FirePlot.RightMenu = Nothing
        Me.FirePlot.ShowCoordinates = True
        Me.FirePlot.Size = New System.Drawing.Size(596, 194)
        Me.FirePlot.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None
        Me.FirePlot.TabIndex = 732
        Me.FirePlot.TabStop = False
        Me.FirePlot.Title = ""
        Me.FirePlot.TitleFont = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Pixel)
        Me.FirePlot.XAxis1 = Nothing
        Me.FirePlot.XAxis2 = Nothing
        Me.FirePlot.YAxis1 = Nothing
        Me.FirePlot.YAxis2 = Nothing
        '
        'Label113
        '
        Me.Label113.AutoSize = True
        Me.Label113.Location = New System.Drawing.Point(41, 564)
        Me.Label113.Name = "Label113"
        Me.Label113.Size = New System.Drawing.Size(96, 13)
        Me.Label113.TabIndex = 124
        Me.Label113.Text = "Radiative Fraction:"
        Me.Label113.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireRemoveInstance
        '
        Me.FireRemoveInstance.Location = New System.Drawing.Point(578, 156)
        Me.FireRemoveInstance.Name = "FireRemoveInstance"
        Me.FireRemoveInstance.Size = New System.Drawing.Size(75, 23)
        Me.FireRemoveInstance.TabIndex = 737
        Me.FireRemoveInstance.Text = "Remove"
        '
        'Label111
        '
        Me.Label111.AutoSize = True
        Me.Label111.Location = New System.Drawing.Point(119, 486)
        Me.Label111.Name = "Label111"
        Me.Label111.Size = New System.Drawing.Size(18, 13)
        Me.Label111.TabIndex = 119
        Me.Label111.Text = "H:"
        Me.Label111.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireDefinitionName
        '
        Me.FireDefinitionName.Location = New System.Drawing.Point(143, 427)
        Me.FireDefinitionName.Name = "FireDefinitionName"
        Me.FireDefinitionName.Size = New System.Drawing.Size(208, 20)
        Me.FireDefinitionName.TabIndex = 733
        Me.FireDefinitionName.Text = "New Fire"
        '
        'FireYPosition
        '
        Me.FireYPosition.Location = New System.Drawing.Point(271, 292)
        Me.FireYPosition.Name = "FireYPosition"
        Me.FireYPosition.Size = New System.Drawing.Size(80, 20)
        Me.FireYPosition.TabIndex = 723
        Me.FireYPosition.Text = "Center"
        '
        'FireHoC
        '
        Me.FireHoC.Location = New System.Drawing.Point(143, 534)
        Me.FireHoC.Name = "FireHoC"
        Me.FireHoC.Size = New System.Drawing.Size(80, 20)
        Me.FireHoC.TabIndex = 714
        Me.FireHoC.Text = "50000000 J/kg"
        '
        'Label109
        '
        Me.Label109.AutoSize = True
        Me.Label109.Location = New System.Drawing.Point(120, 460)
        Me.Label109.Name = "Label109"
        Me.Label109.Size = New System.Drawing.Size(17, 13)
        Me.Label109.TabIndex = 118
        Me.Label109.Text = "C:"
        Me.Label109.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireC
        '
        Me.FireC.Location = New System.Drawing.Point(143, 456)
        Me.FireC.Name = "FireC"
        Me.FireC.Size = New System.Drawing.Size(80, 20)
        Me.FireC.TabIndex = 709
        '
        'FireH
        '
        Me.FireH.Location = New System.Drawing.Point(143, 482)
        Me.FireH.Name = "FireH"
        Me.FireH.Size = New System.Drawing.Size(80, 20)
        Me.FireH.TabIndex = 710
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
        Me.FireSummary.Location = New System.Drawing.Point(27, 15)
        Me.FireSummary.Name = "FireSummary"
        Me.FireSummary.Rows.Count = 401
        Me.FireSummary.Rows.DefaultSize = 19
        Me.FireSummary.Size = New System.Drawing.Size(922, 124)
        Me.FireSummary.StyleInfo = resources.GetString("FireSummary.StyleInfo")
        Me.FireSummary.TabIndex = 701
        Me.FireSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'Label108
        '
        Me.Label108.AutoSize = True
        Me.Label108.Location = New System.Drawing.Point(34, 538)
        Me.Label108.Name = "Label108"
        Me.Label108.Size = New System.Drawing.Size(103, 13)
        Me.Label108.TabIndex = 116
        Me.Label108.Text = "Heat of Combustion:"
        Me.Label108.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label110
        '
        Me.Label110.AutoSize = True
        Me.Label110.Location = New System.Drawing.Point(119, 512)
        Me.Label110.Name = "Label110"
        Me.Label110.Size = New System.Drawing.Size(18, 13)
        Me.Label110.TabIndex = 122
        Me.Label110.Text = "O:"
        Me.Label110.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label114
        '
        Me.Label114.AutoSize = True
        Me.Label114.Location = New System.Drawing.Point(54, 242)
        Me.Label114.Name = "Label114"
        Me.Label114.Size = New System.Drawing.Size(41, 13)
        Me.Label114.TabIndex = 156
        Me.Label114.Text = "Fire ID:"
        Me.Label114.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label106
        '
        Me.Label106.AutoSize = True
        Me.Label106.Location = New System.Drawing.Point(252, 486)
        Me.Label106.Name = "Label106"
        Me.Label106.Size = New System.Drawing.Size(19, 13)
        Me.Label106.TabIndex = 136
        Me.Label106.Text = "Cl:"
        Me.Label106.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireCl
        '
        Me.FireCl.Location = New System.Drawing.Point(271, 482)
        Me.FireCl.Name = "FireCl"
        Me.FireCl.Size = New System.Drawing.Size(80, 20)
        Me.FireCl.TabIndex = 713
        '
        'FireAddt2
        '
        Me.FireAddt2.Location = New System.Drawing.Point(408, 156)
        Me.FireAddt2.Name = "FireAddt2"
        Me.FireAddt2.Size = New System.Drawing.Size(75, 23)
        Me.FireAddt2.TabIndex = 703
        Me.FireAddt2.Text = "Add t²"
        '
        'FireRadiativeFraction
        '
        Me.FireRadiativeFraction.Location = New System.Drawing.Point(143, 560)
        Me.FireRadiativeFraction.Name = "FireRadiativeFraction"
        Me.FireRadiativeFraction.Size = New System.Drawing.Size(80, 20)
        Me.FireRadiativeFraction.TabIndex = 718
        Me.FireRadiativeFraction.Text = "0.3"
        '
        'FireInstanceName
        '
        Me.FireInstanceName.Location = New System.Drawing.Point(103, 238)
        Me.FireInstanceName.Name = "FireInstanceName"
        Me.FireInstanceName.Size = New System.Drawing.Size(208, 20)
        Me.FireInstanceName.TabIndex = 707
        Me.FireInstanceName.Text = "New Fire"
        '
        'ReferencedFireDefinition
        '
        Me.ReferencedFireDefinition.ItemHeight = 13
        Me.ReferencedFireDefinition.Location = New System.Drawing.Point(103, 371)
        Me.ReferencedFireDefinition.Name = "ReferencedFireDefinition"
        Me.ReferencedFireDefinition.Size = New System.Drawing.Size(208, 21)
        Me.ReferencedFireDefinition.TabIndex = 734
        '
        'FireO
        '
        Me.FireO.Location = New System.Drawing.Point(143, 508)
        Me.FireO.Name = "FireO"
        Me.FireO.Size = New System.Drawing.Size(80, 20)
        Me.FireO.TabIndex = 711
        '
        'Label63
        '
        Me.Label63.AutoSize = True
        Me.Label63.Location = New System.Drawing.Point(44, 346)
        Me.Label63.Name = "Label63"
        Me.Label63.Size = New System.Drawing.Size(53, 13)
        Me.Label63.TabIndex = 82
        Me.Label63.Text = "Set Point:"
        Me.Label63.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label112
        '
        Me.Label112.AutoSize = True
        Me.Label112.Location = New System.Drawing.Point(253, 460)
        Me.Label112.Name = "Label112"
        Me.Label112.Size = New System.Drawing.Size(18, 13)
        Me.Label112.TabIndex = 116
        Me.Label112.Text = "N:"
        Me.Label112.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label58
        '
        Me.Label58.AutoSize = True
        Me.Label58.Location = New System.Drawing.Point(12, 321)
        Me.Label58.Name = "Label58"
        Me.Label58.Size = New System.Drawing.Size(85, 13)
        Me.Label58.TabIndex = 79
        Me.Label58.Text = "Ignition Criterion:"
        Me.Label58.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireN
        '
        Me.FireN.Location = New System.Drawing.Point(271, 456)
        Me.FireN.Name = "FireN"
        Me.FireN.Size = New System.Drawing.Size(80, 20)
        Me.FireN.TabIndex = 712
        '
        'Label36
        '
        Me.Label36.Location = New System.Drawing.Point(11, 364)
        Me.Label36.Name = "Label36"
        Me.Label36.Size = New System.Drawing.Size(86, 34)
        Me.Label36.TabIndex = 733
        Me.Label36.Text = "Referenced Fire Properties ID:"
        Me.Label36.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireXPosition
        '
        Me.FireXPosition.Location = New System.Drawing.Point(103, 291)
        Me.FireXPosition.Name = "FireXPosition"
        Me.FireXPosition.Size = New System.Drawing.Size(80, 20)
        Me.FireXPosition.TabIndex = 722
        Me.FireXPosition.Text = "Center"
        '
        'Label70
        '
        Me.Label70.AutoSize = True
        Me.Label70.Location = New System.Drawing.Point(36, 295)
        Me.Label70.Name = "Label70"
        Me.Label70.Size = New System.Drawing.Size(60, 13)
        Me.Label70.TabIndex = 65
        Me.Label70.Text = "Position, X:"
        Me.Label70.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireIgnitionValue
        '
        Me.FireIgnitionValue.Location = New System.Drawing.Point(103, 342)
        Me.FireIgnitionValue.Name = "FireIgnitionValue"
        Me.FireIgnitionValue.Size = New System.Drawing.Size(80, 20)
        Me.FireIgnitionValue.TabIndex = 720
        Me.FireIgnitionValue.Text = "0 s"
        '
        'FireFromFile
        '
        Me.FireFromFile.Location = New System.Drawing.Point(493, 156)
        Me.FireFromFile.Name = "FireFromFile"
        Me.FireFromFile.Size = New System.Drawing.Size(75, 23)
        Me.FireFromFile.TabIndex = 705
        Me.FireFromFile.Text = "From File"
        '
        'Label107
        '
        Me.Label107.AutoSize = True
        Me.Label107.Location = New System.Drawing.Point(187, 343)
        Me.Label107.Name = "Label107"
        Me.Label107.Size = New System.Drawing.Size(78, 13)
        Me.Label107.TabIndex = 134
        Me.Label107.Text = "Ignition Target:"
        Me.Label107.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireIgnitionCriteria
        '
        Me.FireIgnitionCriteria.ItemHeight = 13
        Me.FireIgnitionCriteria.Items.AddRange(New Object() {"Time", "Temperature", "Heat Flux"})
        Me.FireIgnitionCriteria.Location = New System.Drawing.Point(103, 317)
        Me.FireIgnitionCriteria.Name = "FireIgnitionCriteria"
        Me.FireIgnitionCriteria.Size = New System.Drawing.Size(80, 21)
        Me.FireIgnitionCriteria.TabIndex = 719
        Me.FireIgnitionCriteria.Text = "Time"
        '
        'FireTarget
        '
        Me.FireTarget.ItemHeight = 13
        Me.FireTarget.Location = New System.Drawing.Point(271, 338)
        Me.FireTarget.Name = "FireTarget"
        Me.FireTarget.Size = New System.Drawing.Size(80, 21)
        Me.FireTarget.TabIndex = 729
        '
        'Label52
        '
        Me.Label52.AutoSize = True
        Me.Label52.Location = New System.Drawing.Point(23, 268)
        Me.Label52.Name = "Label52"
        Me.Label52.Size = New System.Drawing.Size(72, 13)
        Me.Label52.TabIndex = 3
        Me.Label52.Text = "Compartment:"
        Me.Label52.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label69
        '
        Me.Label69.AutoSize = True
        Me.Label69.Location = New System.Drawing.Point(207, 296)
        Me.Label69.Name = "Label69"
        Me.Label69.Size = New System.Drawing.Size(57, 13)
        Me.Label69.TabIndex = 67
        Me.Label69.Text = "Position Y:"
        Me.Label69.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'FireComp
        '
        Me.FireComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.FireComp.ItemHeight = 13
        Me.FireComp.Location = New System.Drawing.Point(103, 264)
        Me.FireComp.Name = "FireComp"
        Me.FireComp.Size = New System.Drawing.Size(208, 21)
        Me.FireComp.TabIndex = 708
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
        Me.DetectorSummary.Location = New System.Drawing.Point(62, 53)
        Me.DetectorSummary.Name = "DetectorSummary"
        Me.DetectorSummary.Rows.Count = 401
        Me.DetectorSummary.Rows.DefaultSize = 19
        Me.DetectorSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.DetectorSummary.Size = New System.Drawing.Size(852, 195)
        Me.DetectorSummary.StyleInfo = resources.GetString("DetectorSummary.StyleInfo")
        Me.DetectorSummary.TabIndex = 901
        Me.DetectorSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupDetectors
        '
        Me.GroupDetectors.Controls.Add(Me.DetectorName)
        Me.GroupDetectors.Controls.Add(Me.Label67)
        Me.GroupDetectors.Controls.Add(Me.DetectorActivationObscuration)
        Me.GroupDetectors.Controls.Add(Me.Label17)
        Me.GroupDetectors.Controls.Add(Me.DetectorSprayDensity)
        Me.GroupDetectors.Controls.Add(Me.Label81)
        Me.GroupDetectors.Controls.Add(Me.DetectorRTI)
        Me.GroupDetectors.Controls.Add(Me.Label83)
        Me.GroupDetectors.Controls.Add(Me.DetectorActivationTemperature)
        Me.GroupDetectors.Controls.Add(Me.Label92)
        Me.GroupDetectors.Controls.Add(Me.DetectorType)
        Me.GroupDetectors.Controls.Add(Me.Label91)
        Me.GroupDetectors.Controls.Add(Me.DetectorComp)
        Me.GroupDetectors.Controls.Add(Me.Label87)
        Me.GroupDetectors.Controls.Add(Me.GroupBox33)
        Me.GroupDetectors.Location = New System.Drawing.Point(79, 325)
        Me.GroupDetectors.Name = "GroupDetectors"
        Me.GroupDetectors.Size = New System.Drawing.Size(819, 253)
        Me.GroupDetectors.TabIndex = 7
        Me.GroupDetectors.TabStop = False
        Me.GroupDetectors.Text = "Smoke Alarm 1"
        '
        'DetectorName
        '
        Me.DetectorName.Location = New System.Drawing.Point(319, 19)
        Me.DetectorName.Name = "DetectorName"
        Me.DetectorName.Size = New System.Drawing.Size(208, 20)
        Me.DetectorName.TabIndex = 918
        Me.DetectorName.Text = "Detection/Surprssion 1"
        '
        'Label67
        '
        Me.Label67.AutoSize = True
        Me.Label67.Location = New System.Drawing.Point(292, 22)
        Me.Label67.Name = "Label67"
        Me.Label67.Size = New System.Drawing.Size(21, 13)
        Me.Label67.TabIndex = 917
        Me.Label67.Text = "ID:"
        Me.Label67.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorActivationObscuration
        '
        Me.DetectorActivationObscuration.Location = New System.Drawing.Point(696, 81)
        Me.DetectorActivationObscuration.Name = "DetectorActivationObscuration"
        Me.DetectorActivationObscuration.Size = New System.Drawing.Size(96, 20)
        Me.DetectorActivationObscuration.TabIndex = 916
        Me.DetectorActivationObscuration.Text = "8 %/ft"
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.Location = New System.Drawing.Point(571, 85)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(117, 13)
        Me.Label17.TabIndex = 915
        Me.Label17.Text = "Activation Obscuration:"
        Me.Label17.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorSprayDensity
        '
        Me.DetectorSprayDensity.Location = New System.Drawing.Point(593, 197)
        Me.DetectorSprayDensity.Name = "DetectorSprayDensity"
        Me.DetectorSprayDensity.Size = New System.Drawing.Size(96, 20)
        Me.DetectorSprayDensity.TabIndex = 914
        Me.DetectorSprayDensity.Text = "7.0 E-5 m/s"
        '
        'Label81
        '
        Me.Label81.AutoSize = True
        Me.Label81.Location = New System.Drawing.Point(505, 197)
        Me.Label81.Name = "Label81"
        Me.Label81.Size = New System.Drawing.Size(75, 13)
        Me.Label81.TabIndex = 48
        Me.Label81.Text = "Spray Density:"
        Me.Label81.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorRTI
        '
        Me.DetectorRTI.Location = New System.Drawing.Point(593, 165)
        Me.DetectorRTI.Name = "DetectorRTI"
        Me.DetectorRTI.Size = New System.Drawing.Size(96, 20)
        Me.DetectorRTI.TabIndex = 913
        Me.DetectorRTI.Text = "5 m^1/2 s^1/2"
        '
        'Label83
        '
        Me.Label83.AutoSize = True
        Me.Label83.Location = New System.Drawing.Point(553, 165)
        Me.Label83.Name = "Label83"
        Me.Label83.Size = New System.Drawing.Size(28, 13)
        Me.Label83.TabIndex = 46
        Me.Label83.Text = "RTI:"
        Me.Label83.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'DetectorActivationTemperature
        '
        Me.DetectorActivationTemperature.Location = New System.Drawing.Point(696, 53)
        Me.DetectorActivationTemperature.Name = "DetectorActivationTemperature"
        Me.DetectorActivationTemperature.Size = New System.Drawing.Size(96, 20)
        Me.DetectorActivationTemperature.TabIndex = 909
        Me.DetectorActivationTemperature.Text = "30"
        '
        'Label92
        '
        Me.Label92.AutoSize = True
        Me.Label92.Location = New System.Drawing.Point(568, 57)
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
        Me.DetectorType.Location = New System.Drawing.Point(64, 53)
        Me.DetectorType.Name = "DetectorType"
        Me.DetectorType.Size = New System.Drawing.Size(112, 21)
        Me.DetectorType.TabIndex = 907
        Me.DetectorType.Text = "Smoke Alarm"
        '
        'Label91
        '
        Me.Label91.AutoSize = True
        Me.Label91.Location = New System.Drawing.Point(24, 57)
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
        Me.DetectorComp.Location = New System.Drawing.Point(320, 53)
        Me.DetectorComp.Name = "DetectorComp"
        Me.DetectorComp.Size = New System.Drawing.Size(208, 21)
        Me.DetectorComp.TabIndex = 908
        '
        'Label87
        '
        Me.Label87.AutoSize = True
        Me.Label87.Location = New System.Drawing.Point(232, 57)
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
        Me.GroupBox33.Location = New System.Drawing.Point(152, 101)
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
        Me.DetectorZPosition.TabIndex = 912
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
        Me.DetectorYPosition.TabIndex = 911
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
        Me.DetectorXPosition.TabIndex = 910
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
        Me.DetectorRemove.Location = New System.Drawing.Point(675, 285)
        Me.DetectorRemove.Name = "DetectorRemove"
        Me.DetectorRemove.Size = New System.Drawing.Size(75, 23)
        Me.DetectorRemove.TabIndex = 906
        Me.DetectorRemove.Text = "Remove"
        '
        'DetectorAdd
        '
        Me.DetectorAdd.Location = New System.Drawing.Point(227, 285)
        Me.DetectorAdd.Name = "DetectorAdd"
        Me.DetectorAdd.Size = New System.Drawing.Size(75, 23)
        Me.DetectorAdd.TabIndex = 902
        Me.DetectorAdd.Text = "Add"
        '
        'DetectorMoveDown
        '
        Me.DetectorMoveDown.Location = New System.Drawing.Point(515, 285)
        Me.DetectorMoveDown.Name = "DetectorMoveDown"
        Me.DetectorMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.DetectorMoveDown.TabIndex = 905
        Me.DetectorMoveDown.Text = "Move Down"
        '
        'DetectorMoveUp
        '
        Me.DetectorMoveUp.Location = New System.Drawing.Point(419, 285)
        Me.DetectorMoveUp.Name = "DetectorMoveUp"
        Me.DetectorMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.DetectorMoveUp.TabIndex = 904
        Me.DetectorMoveUp.Text = "Move Up"
        '
        'DetectorDup
        '
        Me.DetectorDup.Location = New System.Drawing.Point(323, 285)
        Me.DetectorDup.Name = "DetectorDup"
        Me.DetectorDup.Size = New System.Drawing.Size(75, 23)
        Me.DetectorDup.TabIndex = 903
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
        Me.TargetSummary.Location = New System.Drawing.Point(117, 55)
        Me.TargetSummary.Name = "TargetSummary"
        Me.TargetSummary.Rows.Count = 1001
        Me.TargetSummary.Rows.DefaultSize = 19
        Me.TargetSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.TargetSummary.Size = New System.Drawing.Size(742, 158)
        Me.TargetSummary.StyleInfo = resources.GetString("TargetSummary.StyleInfo")
        Me.TargetSummary.TabIndex = 801
        Me.TargetSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'GroupTargets
        '
        Me.GroupTargets.Controls.Add(Me.TargetName)
        Me.GroupTargets.Controls.Add(Me.Label116)
        Me.GroupTargets.Controls.Add(Me.GroupBox3)
        Me.GroupTargets.Controls.Add(Me.Label79)
        Me.GroupTargets.Controls.Add(Me.TargetComp)
        Me.GroupTargets.Controls.Add(Me.Label74)
        Me.GroupTargets.Controls.Add(Me.TargetSolutionType)
        Me.GroupTargets.Controls.Add(Me.GroupBox28)
        Me.GroupTargets.Location = New System.Drawing.Point(61, 275)
        Me.GroupTargets.Name = "GroupTargets"
        Me.GroupTargets.Size = New System.Drawing.Size(855, 270)
        Me.GroupTargets.TabIndex = 7
        Me.GroupTargets.TabStop = False
        Me.GroupTargets.Text = "Target 1"
        '
        'TargetName
        '
        Me.TargetName.Location = New System.Drawing.Point(113, 34)
        Me.TargetName.Name = "TargetName"
        Me.TargetName.Size = New System.Drawing.Size(98, 20)
        Me.TargetName.TabIndex = 807
        Me.TargetName.Text = "New Target"
        '
        'Label116
        '
        Me.Label116.AutoSize = True
        Me.Label116.Location = New System.Drawing.Point(86, 37)
        Me.Label116.Name = "Label116"
        Me.Label116.Size = New System.Drawing.Size(21, 13)
        Me.Label116.TabIndex = 810
        Me.Label116.Text = "ID:"
        Me.Label116.TextAlign = System.Drawing.ContentAlignment.MiddleRight
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
        Me.TargetInternalLocation.TabIndex = 819
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
        Me.TargetMaterial.TabIndex = 818
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
        'Label79
        '
        Me.Label79.AutoSize = True
        Me.Label79.Location = New System.Drawing.Point(607, 37)
        Me.Label79.Name = "Label79"
        Me.Label79.Size = New System.Drawing.Size(68, 13)
        Me.Label79.TabIndex = 40
        Me.Label79.Text = "Target Type:"
        Me.Label79.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetComp
        '
        Me.TargetComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.TargetComp.ItemHeight = 13
        Me.TargetComp.Location = New System.Drawing.Point(343, 33)
        Me.TargetComp.Name = "TargetComp"
        Me.TargetComp.Size = New System.Drawing.Size(208, 21)
        Me.TargetComp.TabIndex = 808
        '
        'Label74
        '
        Me.Label74.AutoSize = True
        Me.Label74.Location = New System.Drawing.Point(262, 37)
        Me.Label74.Name = "Label74"
        Me.Label74.Size = New System.Drawing.Size(72, 13)
        Me.Label74.TabIndex = 34
        Me.Label74.Text = "Compartment:"
        Me.Label74.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'TargetSolutionType
        '
        Me.TargetSolutionType.ItemHeight = 13
        Me.TargetSolutionType.Items.AddRange(New Object() {"Plate", "Cylinder"})
        Me.TargetSolutionType.Location = New System.Drawing.Point(684, 33)
        Me.TargetSolutionType.Name = "TargetSolutionType"
        Me.TargetSolutionType.Size = New System.Drawing.Size(104, 21)
        Me.TargetSolutionType.TabIndex = 809
        Me.TargetSolutionType.Text = "Plate"
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
        Me.TargetNormalCalc.TabIndex = 814
        Me.TargetNormalCalc.Text = "User Specified"
        '
        'TargetZPosition
        '
        Me.TargetZPosition.Location = New System.Drawing.Point(79, 107)
        Me.TargetZPosition.Name = "TargetZPosition"
        Me.TargetZPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetZPosition.TabIndex = 813
        '
        'TargetZNormal
        '
        Me.TargetZNormal.Location = New System.Drawing.Point(264, 141)
        Me.TargetZNormal.Name = "TargetZNormal"
        Me.TargetZNormal.Size = New System.Drawing.Size(96, 20)
        Me.TargetZNormal.TabIndex = 817
        '
        'Label75
        '
        Me.Label75.AutoSize = True
        Me.Label75.Location = New System.Drawing.Point(15, 111)
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
        Me.TargetYPosition.Location = New System.Drawing.Point(79, 74)
        Me.TargetYPosition.Name = "TargetYPosition"
        Me.TargetYPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetYPosition.TabIndex = 812
        '
        'TargetYNormal
        '
        Me.TargetYNormal.Location = New System.Drawing.Point(264, 109)
        Me.TargetYNormal.Name = "TargetYNormal"
        Me.TargetYNormal.Size = New System.Drawing.Size(96, 20)
        Me.TargetYNormal.TabIndex = 816
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
        Me.Label76.Location = New System.Drawing.Point(15, 78)
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
        Me.TargetXNormal.TabIndex = 815
        '
        'TargetXPosition
        '
        Me.TargetXPosition.Location = New System.Drawing.Point(79, 41)
        Me.TargetXPosition.Name = "TargetXPosition"
        Me.TargetXPosition.Size = New System.Drawing.Size(96, 20)
        Me.TargetXPosition.TabIndex = 811
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
        Me.Label77.Location = New System.Drawing.Point(15, 45)
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
        Me.TargetRemove.TabIndex = 806
        Me.TargetRemove.Text = "Remove"
        '
        'TargetMoveDown
        '
        Me.TargetMoveDown.Location = New System.Drawing.Point(515, 219)
        Me.TargetMoveDown.Name = "TargetMoveDown"
        Me.TargetMoveDown.Size = New System.Drawing.Size(75, 23)
        Me.TargetMoveDown.TabIndex = 805
        Me.TargetMoveDown.Text = "Move Down"
        '
        'TargetAdd
        '
        Me.TargetAdd.Location = New System.Drawing.Point(227, 219)
        Me.TargetAdd.Name = "TargetAdd"
        Me.TargetAdd.Size = New System.Drawing.Size(75, 23)
        Me.TargetAdd.TabIndex = 802
        Me.TargetAdd.Text = "Add"
        '
        'TargetMoveUp
        '
        Me.TargetMoveUp.Location = New System.Drawing.Point(419, 219)
        Me.TargetMoveUp.Name = "TargetMoveUp"
        Me.TargetMoveUp.Size = New System.Drawing.Size(75, 23)
        Me.TargetMoveUp.TabIndex = 804
        Me.TargetMoveUp.Text = "Move Up"
        '
        'TargetDup
        '
        Me.TargetDup.Location = New System.Drawing.Point(323, 219)
        Me.TargetDup.Name = "TargetDup"
        Me.TargetDup.Size = New System.Drawing.Size(75, 23)
        Me.TargetDup.TabIndex = 803
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
        Me.GroupBox2.Location = New System.Drawing.Point(501, 59)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(441, 474)
        Me.GroupBox2.TabIndex = 7
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Ceiling/Floor Connections"
        '
        'VHeatSummary
        '
        Me.VHeatSummary.AllowEditing = False
        Me.VHeatSummary.ColumnInfo = resources.GetString("VHeatSummary.ColumnInfo")
        Me.VHeatSummary.ExtendLastCol = True
        Me.VHeatSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VHeatSummary.Location = New System.Drawing.Point(38, 43)
        Me.VHeatSummary.Name = "VHeatSummary"
        Me.VHeatSummary.Rows.Count = 1001
        Me.VHeatSummary.Rows.DefaultSize = 19
        Me.VHeatSummary.Size = New System.Drawing.Size(364, 168)
        Me.VHeatSummary.StyleInfo = resources.GetString("VHeatSummary.StyleInfo")
        Me.VHeatSummary.TabIndex = 1009
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
        Me.VHeatComp2.TabIndex = 1014
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
        Me.VHeatComp1.TabIndex = 1013
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
        Me.VHeatDup.TabIndex = 1011
        Me.VHeatDup.Text = "Duplicate"
        '
        'VHeatRemove
        '
        Me.VHeatRemove.Location = New System.Drawing.Point(307, 242)
        Me.VHeatRemove.Name = "VHeatRemove"
        Me.VHeatRemove.Size = New System.Drawing.Size(75, 23)
        Me.VHeatRemove.TabIndex = 1012
        Me.VHeatRemove.Text = "Remove"
        '
        'VHeatAdd
        '
        Me.VHeatAdd.Location = New System.Drawing.Point(59, 242)
        Me.VHeatAdd.Name = "VHeatAdd"
        Me.VHeatAdd.Size = New System.Drawing.Size(75, 23)
        Me.VHeatAdd.TabIndex = 1010
        Me.VHeatAdd.Text = "Add"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.HHeatSummary)
        Me.GroupBox1.Controls.Add(Me.GroupHHeats)
        Me.GroupBox1.Controls.Add(Me.HHeatDup)
        Me.GroupBox1.Controls.Add(Me.HHeatRemove)
        Me.GroupBox1.Controls.Add(Me.HHeatAdd)
        Me.GroupBox1.Location = New System.Drawing.Point(34, 59)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(441, 474)
        Me.GroupBox1.TabIndex = 6
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Wall Connections"
        '
        'HHeatSummary
        '
        Me.HHeatSummary.AllowEditing = False
        Me.HHeatSummary.ColumnInfo = resources.GetString("HHeatSummary.ColumnInfo")
        Me.HHeatSummary.ExtendLastCol = True
        Me.HHeatSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.HHeatSummary.Location = New System.Drawing.Point(9, 43)
        Me.HHeatSummary.Name = "HHeatSummary"
        Me.HHeatSummary.Rows.Count = 1001
        Me.HHeatSummary.Rows.DefaultSize = 19
        Me.HHeatSummary.Size = New System.Drawing.Size(423, 168)
        Me.HHeatSummary.StyleInfo = resources.GetString("HHeatSummary.StyleInfo")
        Me.HHeatSummary.TabIndex = 1001
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
        Me.HHeatComp2.TabIndex = 1007
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
        Me.HHeatFraction.TabIndex = 1008
        Me.HHeatFraction.Text = "1"
        '
        'HHeatComp1
        '
        Me.HHeatComp1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.HHeatComp1.ItemHeight = 13
        Me.HHeatComp1.Location = New System.Drawing.Point(139, 29)
        Me.HHeatComp1.Name = "HHeatComp1"
        Me.HHeatComp1.Size = New System.Drawing.Size(208, 21)
        Me.HHeatComp1.TabIndex = 1006
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
        Me.HHeatDup.TabIndex = 1003
        Me.HHeatDup.Text = "Duplicate"
        '
        'HHeatRemove
        '
        Me.HHeatRemove.Location = New System.Drawing.Point(307, 242)
        Me.HHeatRemove.Name = "HHeatRemove"
        Me.HHeatRemove.Size = New System.Drawing.Size(75, 23)
        Me.HHeatRemove.TabIndex = 1004
        Me.HHeatRemove.Text = "Remove"
        '
        'HHeatAdd
        '
        Me.HHeatAdd.Location = New System.Drawing.Point(59, 242)
        Me.HHeatAdd.Name = "HHeatAdd"
        Me.HHeatAdd.Size = New System.Drawing.Size(75, 23)
        Me.HHeatAdd.TabIndex = 1002
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
        Me.TabMain.Controls.Add(Me.TabOutput)
        Me.TabMain.ItemSize = New System.Drawing.Size(122, 18)
        Me.TabMain.Location = New System.Drawing.Point(22, 1)
        Me.TabMain.Name = "TabMain"
        Me.TabMain.SelectedIndex = 0
        Me.TabMain.Size = New System.Drawing.Size(984, 618)
        Me.TabMain.TabIndex = 9999
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
        Me.ThermalFromFile.TabIndex = 204
        Me.ThermalFromFile.Text = "From File"
        '
        'ThermalRemove
        '
        Me.ThermalRemove.Location = New System.Drawing.Point(626, 297)
        Me.ThermalRemove.Name = "ThermalRemove"
        Me.ThermalRemove.Size = New System.Drawing.Size(75, 23)
        Me.ThermalRemove.TabIndex = 205
        Me.ThermalRemove.Text = "Remove"
        '
        'ThermalAdd
        '
        Me.ThermalAdd.Location = New System.Drawing.Point(276, 297)
        Me.ThermalAdd.Name = "ThermalAdd"
        Me.ThermalAdd.Size = New System.Drawing.Size(75, 23)
        Me.ThermalAdd.TabIndex = 202
        Me.ThermalAdd.Text = "Add"
        '
        'ThermalDup
        '
        Me.ThermalDup.Location = New System.Drawing.Point(376, 297)
        Me.ThermalDup.Name = "ThermalDup"
        Me.ThermalDup.Size = New System.Drawing.Size(75, 23)
        Me.ThermalDup.TabIndex = 203
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
        Me.ThermalSummary.TabIndex = 201
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
        Me.ThermalThickness.TabIndex = 211
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
        Me.ThermalConductivity.TabIndex = 208
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
        Me.ThermalSpecificHeat.TabIndex = 209
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
        Me.ThermalLongName.TabIndex = 206
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
        Me.ThermalDensity.TabIndex = 210
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
        Me.ThermalEmissivity.TabIndex = 212
        '
        'Label104
        '
        Me.Label104.AutoSize = True
        Me.Label104.Location = New System.Drawing.Point(103, 51)
        Me.Label104.Name = "Label104"
        Me.Label104.Size = New System.Drawing.Size(21, 13)
        Me.Label104.TabIndex = 15
        Me.Label104.Text = "ID:"
        Me.Label104.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ThermalShortName
        '
        Me.ThermalShortName.Location = New System.Drawing.Point(128, 47)
        Me.ThermalShortName.Name = "ThermalShortName"
        Me.ThermalShortName.Size = New System.Drawing.Size(80, 20)
        Me.ThermalShortName.TabIndex = 207
        '
        'TabOutput
        '
        Me.TabOutput.Controls.Add(Me.OutputValidation)
        Me.TabOutput.Controls.Add(Me.OutputShowCFAST)
        Me.TabOutput.Controls.Add(Me.GroupVisualResolution)
        Me.TabOutput.Controls.Add(Me.OutputDebug)
        Me.TabOutput.Controls.Add(Me.GroupVisualizations)
        Me.TabOutput.Location = New System.Drawing.Point(4, 22)
        Me.TabOutput.Name = "TabOutput"
        Me.TabOutput.Padding = New System.Windows.Forms.Padding(3)
        Me.TabOutput.Size = New System.Drawing.Size(976, 592)
        Me.TabOutput.TabIndex = 8
        Me.TabOutput.Text = "Output"
        Me.TabOutput.UseVisualStyleBackColor = True
        '
        'OutputValidation
        '
        Me.OutputValidation.AutoSize = True
        Me.OutputValidation.Location = New System.Drawing.Point(818, 253)
        Me.OutputValidation.Name = "OutputValidation"
        Me.OutputValidation.Size = New System.Drawing.Size(107, 17)
        Me.OutputValidation.TabIndex = 118
        Me.OutputValidation.Text = "Validation Output"
        Me.OutputValidation.UseVisualStyleBackColor = True
        '
        'OutputShowCFAST
        '
        Me.OutputShowCFAST.AutoSize = True
        Me.OutputShowCFAST.Location = New System.Drawing.Point(818, 323)
        Me.OutputShowCFAST.Name = "OutputShowCFAST"
        Me.OutputShowCFAST.Size = New System.Drawing.Size(132, 17)
        Me.OutputShowCFAST.TabIndex = 120
        Me.OutputShowCFAST.Text = "Show CFAST Window"
        Me.OutputShowCFAST.UseVisualStyleBackColor = True
        '
        'GroupVisualResolution
        '
        Me.GroupVisualResolution.Controls.Add(Me.Label35)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationZ)
        Me.GroupVisualResolution.Controls.Add(Me.Label32)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationY)
        Me.GroupVisualResolution.Controls.Add(Me.Label31)
        Me.GroupVisualResolution.Controls.Add(Me.VisualizationX)
        Me.GroupVisualResolution.Controls.Add(Me.VisualResolutionSummary)
        Me.GroupVisualResolution.Location = New System.Drawing.Point(25, 315)
        Me.GroupVisualResolution.Name = "GroupVisualResolution"
        Me.GroupVisualResolution.Size = New System.Drawing.Size(743, 223)
        Me.GroupVisualResolution.TabIndex = 117
        Me.GroupVisualResolution.TabStop = False
        Me.GroupVisualResolution.Text = "Resolution"
        '
        'Label35
        '
        Me.Label35.Location = New System.Drawing.Point(487, 126)
        Me.Label35.Name = "Label35"
        Me.Label35.Size = New System.Drawing.Size(90, 23)
        Me.Label35.TabIndex = 119
        Me.Label35.Text = "Height (Z) Grid:"
        Me.Label35.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationZ
        '
        Me.VisualizationZ.Location = New System.Drawing.Point(583, 129)
        Me.VisualizationZ.Name = "VisualizationZ"
        Me.VisualizationZ.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationZ.TabIndex = 1114
        '
        'Label32
        '
        Me.Label32.Location = New System.Drawing.Point(487, 100)
        Me.Label32.Name = "Label32"
        Me.Label32.Size = New System.Drawing.Size(90, 23)
        Me.Label32.TabIndex = 117
        Me.Label32.Text = "Depth (Y) Grid:"
        Me.Label32.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationY
        '
        Me.VisualizationY.Location = New System.Drawing.Point(583, 103)
        Me.VisualizationY.Name = "VisualizationY"
        Me.VisualizationY.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationY.TabIndex = 1113
        '
        'Label31
        '
        Me.Label31.Location = New System.Drawing.Point(484, 74)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(93, 23)
        Me.Label31.TabIndex = 115
        Me.Label31.Text = "Width (X) Grid:"
        Me.Label31.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationX
        '
        Me.VisualizationX.Location = New System.Drawing.Point(583, 77)
        Me.VisualizationX.Name = "VisualizationX"
        Me.VisualizationX.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationX.TabIndex = 1111
        '
        'VisualResolutionSummary
        '
        Me.VisualResolutionSummary.AllowDragging = C1.Win.C1FlexGrid.AllowDraggingEnum.None
        Me.VisualResolutionSummary.AllowEditing = False
        Me.VisualResolutionSummary.AllowResizing = C1.Win.C1FlexGrid.AllowResizingEnum.None
        Me.VisualResolutionSummary.AllowSorting = C1.Win.C1FlexGrid.AllowSortingEnum.None
        Me.VisualResolutionSummary.AutoGenerateColumns = False
        Me.VisualResolutionSummary.ColumnInfo = resources.GetString("VisualResolutionSummary.ColumnInfo")
        Me.VisualResolutionSummary.ExtendLastCol = True
        Me.VisualResolutionSummary.FocusRect = C1.Win.C1FlexGrid.FocusRectEnum.None
        Me.VisualResolutionSummary.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.VisualResolutionSummary.Location = New System.Drawing.Point(24, 18)
        Me.VisualResolutionSummary.Name = "VisualResolutionSummary"
        Me.VisualResolutionSummary.Rows.Count = 101
        Me.VisualResolutionSummary.Rows.DefaultSize = 19
        Me.VisualResolutionSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VisualResolutionSummary.Size = New System.Drawing.Size(417, 187)
        Me.VisualResolutionSummary.StyleInfo = resources.GetString("VisualResolutionSummary.StyleInfo")
        Me.VisualResolutionSummary.TabIndex = 1110
        Me.VisualResolutionSummary.TabStop = False
        Me.VisualResolutionSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'OutputDebug
        '
        Me.OutputDebug.AutoSize = True
        Me.OutputDebug.Location = New System.Drawing.Point(818, 288)
        Me.OutputDebug.Name = "OutputDebug"
        Me.OutputDebug.Size = New System.Drawing.Size(93, 17)
        Me.OutputDebug.TabIndex = 119
        Me.OutputDebug.Text = "Debug Output"
        Me.OutputDebug.UseVisualStyleBackColor = True
        '
        'GroupVisualizations
        '
        Me.GroupVisualizations.Controls.Add(Me.VisualSummary)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationAxisLabel)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationAdd)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationAxis)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationType)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationValueLabel)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationDup)
        Me.GroupVisualizations.Controls.Add(Me.Label29)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationComp)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationDefaults)
        Me.GroupVisualizations.Controls.Add(Me.Label7)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationValue)
        Me.GroupVisualizations.Controls.Add(Me.VisualizationRemove)
        Me.GroupVisualizations.Location = New System.Drawing.Point(25, 15)
        Me.GroupVisualizations.Name = "GroupVisualizations"
        Me.GroupVisualizations.Size = New System.Drawing.Size(743, 294)
        Me.GroupVisualizations.TabIndex = 116
        Me.GroupVisualizations.TabStop = False
        Me.GroupVisualizations.Text = "Visualizations"
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
        Me.VisualSummary.Rows.Count = 401
        Me.VisualSummary.Rows.DefaultSize = 19
        Me.VisualSummary.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.VisualSummary.Size = New System.Drawing.Size(407, 197)
        Me.VisualSummary.StyleInfo = resources.GetString("VisualSummary.StyleInfo")
        Me.VisualSummary.TabIndex = 1101
        Me.VisualSummary.TabStop = False
        Me.VisualSummary.VisualStyle = C1.Win.C1FlexGrid.VisualStyle.System
        '
        'VisualizationAxisLabel
        '
        Me.VisualizationAxisLabel.Location = New System.Drawing.Point(486, 178)
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
        Me.VisualizationAdd.TabIndex = 1102
        Me.VisualizationAdd.Text = "Add"
        '
        'VisualizationAxis
        '
        Me.VisualizationAxis.ItemHeight = 13
        Me.VisualizationAxis.Items.AddRange(New Object() {"X-axis (Width)", "Y-axis (Depth)", "Z-axis (Height)"})
        Me.VisualizationAxis.Location = New System.Drawing.Point(583, 180)
        Me.VisualizationAxis.MaxDropDownItems = 3
        Me.VisualizationAxis.Name = "VisualizationAxis"
        Me.VisualizationAxis.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationAxis.TabIndex = 1109
        '
        'VisualizationType
        '
        Me.VisualizationType.ItemHeight = 13
        Me.VisualizationType.Items.AddRange(New Object() {"2-D", "3-D", "Isosurface"})
        Me.VisualizationType.Location = New System.Drawing.Point(583, 94)
        Me.VisualizationType.MaxDropDownItems = 3
        Me.VisualizationType.Name = "VisualizationType"
        Me.VisualizationType.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationType.TabIndex = 1106
        '
        'VisualizationValueLabel
        '
        Me.VisualizationValueLabel.Location = New System.Drawing.Point(505, 149)
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
        Me.VisualizationDup.TabIndex = 1103
        Me.VisualizationDup.Text = "Duplicate"
        '
        'Label29
        '
        Me.Label29.Location = New System.Drawing.Point(486, 123)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(91, 23)
        Me.Label29.TabIndex = 112
        Me.Label29.Text = "Compartment:"
        Me.Label29.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationComp
        '
        Me.VisualizationComp.ItemHeight = 13
        Me.VisualizationComp.Location = New System.Drawing.Point(583, 123)
        Me.VisualizationComp.Name = "VisualizationComp"
        Me.VisualizationComp.Size = New System.Drawing.Size(140, 21)
        Me.VisualizationComp.TabIndex = 1107
        '
        'VisualizationDefaults
        '
        Me.VisualizationDefaults.Location = New System.Drawing.Point(321, 244)
        Me.VisualizationDefaults.Name = "VisualizationDefaults"
        Me.VisualizationDefaults.Size = New System.Drawing.Size(109, 23)
        Me.VisualizationDefaults.TabIndex = 1105
        Me.VisualizationDefaults.Text = "Add Defaults"
        '
        'Label7
        '
        Me.Label7.Location = New System.Drawing.Point(473, 94)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(104, 23)
        Me.Label7.TabIndex = 111
        Me.Label7.Text = "Visualization Type:"
        Me.Label7.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'VisualizationValue
        '
        Me.VisualizationValue.Location = New System.Drawing.Point(583, 152)
        Me.VisualizationValue.Name = "VisualizationValue"
        Me.VisualizationValue.Size = New System.Drawing.Size(140, 20)
        Me.VisualizationValue.TabIndex = 1108
        '
        'VisualizationRemove
        '
        Me.VisualizationRemove.Location = New System.Drawing.Point(226, 244)
        Me.VisualizationRemove.Name = "VisualizationRemove"
        Me.VisualizationRemove.Size = New System.Drawing.Size(75, 23)
        Me.VisualizationRemove.TabIndex = 1104
        Me.VisualizationRemove.Text = "Remove"
        '
        'OpenDataFileDialog
        '
        Me.OpenDataFileDialog.DefaultExt = "in"
        Me.OpenDataFileDialog.Filter = "CFAST files|*.in|All files|*.*"
        Me.OpenDataFileDialog.Title = "Open"
        '
        'SaveDataFileDialog
        '
        Me.SaveDataFileDialog.DefaultExt = "cfast"
        Me.SaveDataFileDialog.Filter = "CFAST files|*.in|CFAST files (Old format)|*.in|All files|*.*"
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
        Me.MainSave.TabIndex = 2
        Me.MainSave.Text = "Save"
        '
        'MainRun
        '
        Me.MainRun.Location = New System.Drawing.Point(543, 638)
        Me.MainRun.Name = "MainRun"
        Me.MainRun.Size = New System.Drawing.Size(75, 23)
        Me.MainRun.TabIndex = 4
        Me.MainRun.Text = "Run"
        '
        'MainGeometry
        '
        Me.MainGeometry.Location = New System.Drawing.Point(462, 638)
        Me.MainGeometry.Name = "MainGeometry"
        Me.MainGeometry.Size = New System.Drawing.Size(75, 23)
        Me.MainGeometry.TabIndex = 3
        Me.MainGeometry.Text = "Geometry"
        '
        'MainOpen
        '
        Me.MainOpen.Location = New System.Drawing.Point(232, 638)
        Me.MainOpen.Name = "MainOpen"
        Me.MainOpen.Size = New System.Drawing.Size(75, 23)
        Me.MainOpen.TabIndex = 1
        Me.MainOpen.Text = "Open"
        '
        'CeditMain
        '
        Me.C1SizerLight1.SetAutoResize(Me, True)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.None
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(1008, 708)
        Me.Controls.Add(Me.MainOpen)
        Me.Controls.Add(Me.MainView)
        Me.Controls.Add(Me.MainGeometry)
        Me.Controls.Add(Me.MainRun)
        Me.Controls.Add(Me.MainSave)
        Me.Controls.Add(Me.StatusBar)
        Me.Controls.Add(Me.TabMain)
        Me.HelpProvider.SetHelpNavigator(Me, System.Windows.Forms.HelpNavigator.TableOfContents)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Menu = Me.MainMenu
        Me.MinimumSize = New System.Drawing.Size(1020, 744)
        Me.Name = "CeditMain"
        Me.HelpProvider.SetShowHelp(Me, True)
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "CEdit"
        CType(Me.Errors, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Message, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.Output, System.ComponentModel.ISupportInitialize).EndInit()
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
        CType(Me.HVentFractions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox14.ResumeLayout(False)
        Me.GroupBox13.ResumeLayout(False)
        Me.TabVerticalFlow.ResumeLayout(False)
        CType(Me.VVentSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupVVents.ResumeLayout(False)
        Me.GroupVVents.PerformLayout()
        CType(Me.VVentFractions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox4.ResumeLayout(False)
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
        CType(Me.MVentFractions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox20.ResumeLayout(False)
        Me.GroupBox20.PerformLayout()
        Me.GroupBox21.ResumeLayout(False)
        Me.GroupBox21.PerformLayout()
        Me.TabFires.ResumeLayout(False)
        Me.TabFires.PerformLayout()
        CType(Me.FireDataSS, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.FireSummary, System.ComponentModel.ISupportInitialize).EndInit()
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
        Me.TabOutput.ResumeLayout(False)
        Me.TabOutput.PerformLayout()
        Me.GroupVisualResolution.ResumeLayout(False)
        Me.GroupVisualResolution.PerformLayout()
        CType(Me.VisualResolutionSummary, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupVisualizations.ResumeLayout(False)
        Me.GroupVisualizations.PerformLayout()
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
                        Case "ShowCFASTOutput"
                            CommandWindowVisible = CType(RegistryOptions(iSet, 1), Boolean)
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
        If myEnvironment.FileChanged Then SaveDataFile(True, False)
        myRecentFiles.Save()
    End Sub
#Region " Simulation Tab "
    ' This section of code handles events related to the environment tab
    Private Sub Environment_Changed(sender As Object, e As EventArgs) Handles EnvSimTime.Leave, EnvTextOutInterval.Leave, EnvSpreadOutInterval.Leave, EnvSmokeviewInterval.Leave, EnvTitle.Leave, EnvIntAmbTemp.Leave, EnvExtAmbTemp.Leave, EnvExtAmbPress.Leave, EnvLOI.Leave, EnvTimeStep.Leave, EnvIntAmbRH.Leave, EnvAdiabatic.CheckedChanged
        If sender Is EnvTitle Then myEnvironment.Title = EnvTitle.Text
        If sender Is EnvSimTime Then myEnvironment.SimulationTime = Val(EnvSimTime.Text)
        If sender Is EnvTextOutInterval Then myEnvironment.OutputInterval = Val(EnvTextOutInterval.Text)
        If sender Is EnvSpreadOutInterval Then myEnvironment.SpreadsheetInterval = Val(EnvSpreadOutInterval.Text)
        If sender Is EnvSmokeviewInterval Then myEnvironment.SmokeviewInterval = Val(EnvSmokeviewInterval.Text)
        If sender Is EnvIntAmbTemp Then myEnvironment.IntAmbTemperature = Val(EnvIntAmbTemp.Text)
        If sender Is EnvIntAmbRH Then myEnvironment.IntAmbRH = Val(EnvIntAmbRH.Text)
        If sender Is EnvExtAmbTemp Then myEnvironment.ExtAmbTemperature = Val(EnvExtAmbTemp.Text)
        If sender Is EnvExtAmbPress Then myEnvironment.ExtAmbPressure = Val(EnvExtAmbPress.Text)
        If sender Is EnvLOI Then myEnvironment.LowerOxygenLimit = Val(EnvLOI.Text)
        If sender Is EnvTimeStep Then myEnvironment.MaximumTimeStep = Val(EnvTimeStep.Text)
        If sender Is EnvAdiabatic Then
            myEnvironment.AdiabaticWalls = EnvAdiabatic.Checked
            Dim ir As Integer, aCompartment As New Compartment, SavedCompartment As Integer
            SavedCompartment = CurrentCompartment
            For ir = 0 To myCompartments.Count - 1
                If EnvAdiabatic.Checked Then
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
            If sender Is ThermalLongName Then aProperty.Name = ThermalLongName.Text
            If sender Is ThermalShortName Then aProperty.ShortName = ThermalShortName.Text
            If sender Is ThermalConductivity Then aProperty.Conductivity = Val(ThermalConductivity.Text)
            If sender Is ThermalSpecificHeat Then aProperty.SpecificHeat = Val(ThermalSpecificHeat.Text)
            If sender Is ThermalDensity Then aProperty.Density = Val(ThermalDensity.Text)
            If sender Is ThermalThickness Then aProperty.Thickness = Val(ThermalThickness.Text)
            If sender Is ThermalEmissivity Then aProperty.Emissivity = Val(ThermalEmissivity.Text)
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalSummary.Click
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = ThermalSummary.RowSel - 1
        If index >= 0 And index <= myThermalProperties.Count - 1 Then
            CurrentThermalProperty = index
            UpdateGUI.Thermals(CurrentThermalProperty)
        End If
    End Sub
    Private Sub ThermalSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles ThermalSummary.AfterSelChange
        ' The currently selected ThermalProperties has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = ThermalSummary.RowSel - 1
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
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + ThermalProperty.MaximumProperties.ToString + " thermal properties are allowed. New property not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub ThermalRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalRemove.Click
        Dim ReturnedButton As Integer, TotalConnections As Integer
        If CurrentThermalProperty >= 0 And myThermalProperties.Count > 0 Then
            Dim aProperty As New ThermalProperty
            aProperty = myThermalProperties.Item(CurrentThermalProperty)
            TotalConnections = myThermalProperties.NumberofConnections(aProperty.ShortName)
            If TotalConnections > 0 Then
                ReturnedButton = MessageBox.Show(aProperty.Name + " is used " + TotalConnections.ToString +
                " times to define materials in the current simulation. These will be changed to Off.", Text, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button2, MessageBoxOptions.DefaultDesktopOnly)
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
            MessageBox.Show("A maximum of " + Compartment.MaximumCompartments.ToString + " compartments are allowed. New compartment not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + Compartment.MaximumCompartments.ToString + " compartments are allowed. New compartment not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            TotalConnections = myHVents.NumberofConnections(CurrentCompartment) + myVVents.NumberofConnections(CurrentCompartment) + myMVents.NumberofConnections(CurrentCompartment) +
            myDetectors.NumberofConnections(CurrentCompartment) + myTargets.NumberofConnections(CurrentCompartment) + myHHeats.NumberofConnections(CurrentCompartment) +
            myVHeats.NumberofConnections(CurrentCompartment) + myFireProperties.NumberofConnections(CurrentCompartment) + myVisuals.NumberofConnections(CurrentCompartment)
            If TotalConnections > 0 Then
                ReturnedButton = MessageBox.Show("Compartment " + (CurrentCompartment + 1).ToString + " has " + TotalConnections.ToString +
                " connection(s) that will be removed.", Text, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button2, MessageBoxOptions.DefaultDesktopOnly)
                If ReturnedButton = OK Then
                    myHVents.RemoveAll(CurrentCompartment) : CurrentHVent = 0
                    myVVents.RemoveAll(CurrentCompartment) : CurrentVVent = 0
                    myMVents.RemoveAll(CurrentCompartment) : CurrentMVent = 0
                    myDetectors.RemoveAll(CurrentCompartment) : CurrentDetector = 0
                    myTargets.RemoveAll(CurrentCompartment) : CurrentTarget = 0
                    myHHeats.RemoveAll(CurrentCompartment) : CurrentHHeat = 0
                    myHHeats.RemoveAll(CurrentCompartment) : CurrentVHeat = 0
                    myFireProperties.RemoveAll(CurrentCompartment) : CurrentFire = 0
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
        index = CompSummary.RowSel - 1
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
            If sender Is CompName Then aCompartment.Name = CompName.Text
            If sender Is CompWidth Then aCompartment.RoomWidth = Val(CompWidth.Text)
            If sender Is CompDepth Then aCompartment.RoomDepth = Val(CompDepth.Text)
            If sender Is CompHeight Then aCompartment.RoomHeight = Val(CompHeight.Text)
            If sender Is CompXPosition Then aCompartment.RoomOriginX = Val(CompXPosition.Text)
            If sender Is CompYPosition Then aCompartment.RoomOriginY = Val(CompYPosition.Text)
            If sender Is CompZPosition Then aCompartment.RoomOriginZ = Val(CompZPosition.Text)
            myCompartments.Item(CurrentCompartment) = aCompartment
            UpdateGUI.Geometry(CurrentCompartment)
        End If
    End Sub
    Private Sub Comp_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CompCeiling.SelectedIndexChanged, CompWalls.SelectedIndexChanged, CompFloor.SelectedIndexChanged
        Dim aCompartment As New Compartment
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            aCompartment = myCompartments.Item(CurrentCompartment)
            If sender Is CompCeiling Then
                aCompartment.CeilingMaterial = myThermalProperties.GetShortName(sender.text)
            ElseIf sender Is CompWalls Then
                aCompartment.WallMaterial = myThermalProperties.GetShortName(sender.text)
            ElseIf sender Is CompFloor Then
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
            If sender Is CompShaft And CompShaft.Checked = True Then
                aCompartment.Shaft = True
                aCompartment.Hall = False
                myCompartments.Item(CurrentCompartment) = aCompartment
            ElseIf sender Is CompCorridor And CompCorridor.Checked = True Then
                aCompartment.Hall = True
                aCompartment.Shaft = False
                myCompartments.Item(CurrentCompartment) = aCompartment
            ElseIf sender Is CompNormal And CompNormal.Checked = True Then
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
        numPoints = UpdateGUI.CountGridPoints(CompVariableArea)
        For ir = 1 To numPoints
            If CType(CompVariableArea(ir, 0), String) + " " = " " Then
                CompVariableArea(ir, 0) = Nothing
                CompVariableArea(ir, 1) = Nothing
            End If
        Next
        ' Copy the values from the spreadsheet to the two vectors for height and area, then put them in the compartment data structure
        If CurrentCompartment >= 0 And myCompartments.Count > 0 Then
            aCompartment = myCompartments.Item(CurrentCompartment)
            numPoints = UpdateGUI.CountGridPoints(CompVariableArea)
            If numPoints = 0 Then
                Dim AreaPoints(0) As Single, HeightPoints(0) As Single
                aCompartment.SetVariableArea(AreaPoints, HeightPoints)
                myCompartments.Item(CurrentCompartment) = aCompartment
                UpdateGUI.Geometry(CurrentCompartment)
            ElseIf numPoints > 0 Then
                Dim AreaPoints(numPoints) As Single, HeightPoints(numPoints) As Single
                For ir = 1 To numPoints
                    HeightPoints(ir) = Val(CompVariableArea(ir, 0))
                    AreaPoints(ir) = Val(CompVariableArea(ir, 1))
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
            MessageBox.Show("A maximum of " + Vent.MaximumHVents.ToString + " vents are allowed. New vent was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + myHVents.Maximum.ToString + " vents are allowed. New vent not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
        index = HVentSummary.RowSel - 1
        If index >= 0 And index <= myHVents.Count - 1 Then
            CurrentHVent = index
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles HVentSummary.AfterSelChange
        ' The currently selected hvent has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        index = HVentSummary.RowSel - 1
        If index >= 0 And index <= myHVents.Count - 1 Then
            CurrentHVent = index
            UpdateGUI.HVents(CurrentHVent)
        End If
    End Sub
    Private Sub HVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HVentOffset.Leave, HVentSill.Leave, HVentSoffit.Leave, HVentWidth.Leave, HVentFace.SelectedIndexChanged, HVentComp1.SelectedIndexChanged, HVentComp2.SelectedIndexChanged, HVentOpenCriterion.SelectedIndexChanged, HVentOpenValue.Leave, HVentTarget.SelectedIndexChanged, HVentInitialFraction.Leave, HVentFinalFraction.Leave, HVentName.Leave
        Dim aVent As New Vent
        If CurrentHVent >= 0 And myHVents.Count > 0 Then
            aVent = myHVents.Item(CurrentHVent)
            If sender Is HVentName Then aVent.Name = HVentName.Text
            If sender Is HVentOffset Then aVent.Offset = Val(HVentOffset.Text)
            If sender Is HVentSill Then aVent.Sill = Val(HVentSill.Text)
            If sender Is HVentSoffit Then aVent.Soffit = Val(HVentSoffit.Text)
            If sender Is HVentWidth Then aVent.Width = Val(HVentWidth.Text)
            If sender Is HVentFace Then aVent.Face = HVentFace.SelectedIndex + 1
            If sender Is HVentComp1 Then aVent.FirstCompartment = HVentComp1.SelectedIndex - 1
            If sender Is HVentComp2 Then aVent.SecondCompartment = HVentComp2.SelectedIndex - 1
            ' CFast expects the from compartment to be the lower number of the pair and the outside to be the to compartment
            If sender Is HVentOpenCriterion Then
                If aVent.OpenType <> HVentOpenCriterion.SelectedIndex Then
                    aVent.OpenType = HVentOpenCriterion.SelectedIndex
                    If aVent.OpenType = Vent.OpenbyTemperature Then
                        aVent.OpenValue = 0.0
                        aVent.OpenValue = myEnvironment.IntAmbTemperature
                    ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                        aVent.OpenValue = 0.0
                    End If
                End If
            End If
            If sender Is HVentOpenValue Then aVent.OpenValue = Val(HVentOpenValue.Text)
            If sender Is HVentTarget Then aVent.Target = myTargets.Item(HVentTarget.SelectedIndex).Name
            If sender Is HVentInitialFraction Then aVent.InitialOpening = Val(HVentInitialFraction.Text)
            If sender Is HVentFinalFraction Then aVent.FinalOpening = Val(HVentFinalFraction.Text)

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
            MessageBox.Show("A maximum of " + myVVents.Maximum.ToString + " vents are allowed. New vent was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + myVVents.Maximum.ToString + " vents are allowed. New vent not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
        index = VVentSummary.RowSel - 1
        If index >= 0 And index <= myVVents.Count - 1 Then
            CurrentVVent = index
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
    Private Sub VVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles VVentSummary.AfterSelChange
        ' The currently selected vvent has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = VVentSummary.RowSel - 1
        If index >= 0 And index <= myVVents.Count - 1 Then
            CurrentVVent = index
            UpdateGUI.VVents(CurrentVVent)
        End If
    End Sub
    Private Sub VVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVentCompTop.SelectedIndexChanged, VVentCompBottom.SelectedIndexChanged, VVentArea.Leave, VVentShape.SelectedIndexChanged, VVentXOffset.Leave, VVentYOffset.Leave, VVentInitialFraction.Leave, VVentFinalFraction.Leave, VVentOpenCriterion.SelectedIndexChanged, VVentOpenValue.Leave, VVentTarget.SelectedIndexChanged, VVentName.Leave
        Dim aVent As New Vent
        If CurrentVVent >= 0 And myVVents.Count > 0 Then
            aVent = myVVents.Item(CurrentVVent)
            If sender Is VVentName Then aVent.Name = VVentName.Text
            If sender Is VVentArea Then aVent.Area = Val(VVentArea.Text)
            If sender Is VVentXOffset Then aVent.OffsetX = Val(VVentXOffset.Text)
            If sender Is VVentYOffset Then aVent.OffsetY = Val(VVentYOffset.Text)
            If sender Is VVentShape Then aVent.Shape = VVentShape.SelectedIndex + 1
            If sender Is VVentCompTop Then aVent.FirstCompartment = VVentCompTop.SelectedIndex - 1
            If sender Is VVentCompBottom Then aVent.SecondCompartment = VVentCompBottom.SelectedIndex - 1
            If sender Is VVentOpenCriterion Then
                If aVent.OpenType <> VVentOpenCriterion.SelectedIndex Then
                    aVent.OpenType = VVentOpenCriterion.SelectedIndex
                    If aVent.OpenType = Vent.OpenbyTemperature Then
                        aVent.OpenValue = 0.0
                        aVent.OpenValue = myEnvironment.IntAmbTemperature
                    ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                        aVent.OpenValue = 0.0
                    End If
                End If
            End If
            If sender Is VVentOpenValue Then aVent.OpenValue = Val(VVentOpenValue.Text)
            If sender Is VVentTarget Then aVent.Target = myTargets.Item(VVentTarget.SelectedIndex).Name
            If sender Is VVentInitialFraction Then aVent.InitialOpening = Val(VVentInitialFraction.Text)
            If sender Is VVentFinalFraction Then aVent.FinalOpening = Val(VVentFinalFraction.Text)
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
            MessageBox.Show("A maximum of " + myMVents.Maximum.ToString + " vents are allowed. New vent was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + myMVents.Maximum.ToString + " vents are allowed. New vent not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
        index = MVentSummary.RowSel - 1
        If index >= 0 And index <= myMVents.Count - 1 Then
            CurrentMVent = index
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
    Private Sub mVentSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles MVentSummary.AfterSelChange
        ' The currently selected mvent has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = MVentSummary.RowSel - 1
        If index >= 0 And index <= myMVents.Count - 1 Then
            CurrentMVent = index
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
    Private Sub mVent_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MVentFromComp.SelectedIndexChanged, MventToComp.SelectedIndexChanged, MVentFromOrientation.SelectedIndexChanged, MVentToOrientation.SelectedIndexChanged, MVentFromArea.Leave, MVentFromHeight.Leave, MVentToArea.Leave, MVentToHeight.Leave, MVentFlow.Leave, MVentDropoff.Leave, MVentZero.Leave, MVentFilterEfficiency.Leave, MVentFilterTime.Leave, MVentOpenCriterion.SelectedIndexChanged, MVentOpenValue.Leave, MVentTarget.SelectedIndexChanged, MVentInitialFraction.Leave, MVentFinalFraction.Leave, MVentXOffset.Leave, MVentYOffset.Leave, MVentName.Leave
        Dim aVent As New Vent
        If CurrentMVent >= 0 And myMVents.Count > 0 Then
            aVent = myMVents.Item(CurrentMVent)
            If sender Is MVentName Then aVent.Name = MVentName.Text
            If sender Is MVentFromComp Then aVent.FirstCompartment = MVentFromComp.SelectedIndex - 1
            If sender Is MVentFromArea Then aVent.FirstArea = Val(MVentFromArea.Text)
            If sender Is MVentFromHeight Then aVent.FirstCenterHeight = Val(MVentFromHeight.Text)
            If sender Is MVentFromOrientation Then aVent.FirstOrientation = MVentFromOrientation.SelectedIndex + 1

            If sender Is MventToComp Then aVent.SecondCompartment = MventToComp.SelectedIndex - 1
            If sender Is MVentToArea Then aVent.SecondArea = Val(MVentToArea.Text)
            If sender Is MVentToHeight Then aVent.SecondCenterHeight = Val(MVentToHeight.Text)
            If sender Is MVentToOrientation Then aVent.SecondOrientation = MVentToOrientation.SelectedIndex + 1

            If sender Is MVentFlow Then aVent.FlowRate = Val(MVentFlow.Text)
            If sender Is MVentDropoff Then aVent.BeginFlowDropoff = Val(MVentDropoff.Text)
            If sender Is MVentZero Then aVent.ZeroFlow = Val(MVentZero.Text)

            If sender Is MVentFilterEfficiency Then aVent.FilterEfficiency = Val(MVentFilterEfficiency.Text)
            If sender Is MVentFilterTime Then aVent.FilterTime = Val(MVentFilterTime.Text)

            If sender Is MVentXOffset Then aVent.OffsetX = Val(MVentXOffset.Text)
            If sender Is MVentYOffset Then aVent.OffsetY = Val(MVentYOffset.Text)

            If sender Is MVentOpenCriterion Then
                If aVent.OpenType <> MVentOpenCriterion.SelectedIndex Then
                    aVent.OpenType = MVentOpenCriterion.SelectedIndex
                    If aVent.OpenType = Vent.OpenbyTemperature Then
                        aVent.OpenValue = 0.0
                        aVent.OpenValue = myEnvironment.IntAmbTemperature
                    ElseIf aVent.OpenType = Vent.OpenbyFlux Then
                        aVent.OpenValue = 0.0
                    End If
                End If
            End If
            If sender Is MVentOpenValue Then aVent.OpenValue = Val(MVentOpenValue.Text)
            If sender Is MVentTarget Then aVent.Target = myTargets.Item(MVentTarget.SelectedIndex).Name
            If sender Is MVentInitialFraction Then aVent.InitialOpening = Val(MVentInitialFraction.Text)
            If sender Is MVentFinalFraction Then aVent.FinalOpening = Val(MVentFinalFraction.Text)

            myMVents(CurrentMVent) = aVent
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
#End Region
#Region "Fires Tab "
    ' This section of code handles the events related to the fires tab
    ' A fire consists of two parts, a definitions of fire properties (HRR, etc) and a location. A fire definition can be used for more than one fire location
    Private Sub FireAdd_Click(sender As System.Object, e As System.EventArgs) Handles FireAdd.Click, FireAddt2.Click
        If myFireProperties.Count + 1 <= Fire.MaximumFires Then
            If sender Is FireAddt2 Then
                Dim t2FireDialog As New t2Fire
                Dim iReturn As Integer
                iReturn = t2FireDialog.ShowDialog(Me)
                If iReturn = Windows.Forms.DialogResult.OK Then
                    Dim aFire As New Fire(t2FireDialog.GrowthTime, t2FireDialog.PeakHRR, t2FireDialog.SteadyTime, t2FireDialog.DecayTime)
                    aFire.Name = "New Fire " + (myFireProperties.Count + 1).ToString
                    myFireProperties.Add(aFire)
                    Dim aFireInstance As New Fire
                    aFireInstance.Name = "New Fire " + (myFires.Count + 1).ToString
                    aFireInstance.ObjectType = Fire.TypeInstance
                    aFireInstance.ReferencedFireDefinition = aFire.Name
                    myFires.Add(aFireInstance)
                    CurrentFire = myFires.Count - 1
                    UpdateGUI.Fires(CurrentFire)
                End If
            ElseIf sender Is FireAdd Then
                Dim aFire As New Fire()
                aFire.Name = "New Fire " + (myFireProperties.Count + 1).ToString
                myFireProperties.Add(aFire)
                Dim aFireInstance As New Fire
                aFireInstance.Name = "New Fire " + (myFires.Count + 1).ToString
                aFireInstance.ObjectType = Fire.TypeInstance
                aFireInstance.ReferencedFireDefinition = aFire.Name
                myFires.Add(aFireInstance)
                CurrentFire = myFires.Count - 1
                UpdateGUI.Fires(CurrentFire)
            End If
        End If
    End Sub
    Private Sub FireRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireRemoveInstance.Click
        ' Remove the current Fire instance from the list of fire instances
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
        index = FireSummary.RowSel - 1
        If index >= 0 And index <= myFires.Count - 1 Then
            CurrentFire = index
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireSummary.AfterSelChange
        ' The currently selected Fire has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = FireSummary.RowSel - 1
        If index >= 0 And index <= myFires.Count - 1 Then
            CurrentFire = index
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub Referenced_Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ReferencedFireDefinition.SelectedIndexChanged
        If CurrentFire >= 0 And myFires.Count > 0 Then
            Dim aFireTimeSeries(12, 0) As Single
            Dim aFire As New Fire, aFireInstance As New Fire, FireIndex As Integer
            aFireInstance = myFires(CurrentFire)
            FireIndex = ReferencedFireDefinition.SelectedIndex - 1
            If FireIndex >= 0 Then
                aFire = myFireProperties(FireIndex)
                If aFireInstance.ReferencedFireDefinition <> aFire.Name Then
                    aFireInstance.ReferencedFireDefinition = aFire.Name
                    If CurrentFire >= 0 Then
                        myFires(CurrentFire) = aFireInstance
                    End If
                    UpdateGUI.Fires(CurrentFire)
                End If
            End If
        End If
    End Sub
    Private Sub Fire_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FireComp.SelectedIndexChanged, FireIgnitionCriteria.SelectedIndexChanged, FireXPosition.Leave, FireYPosition.Leave, FireIgnitionValue.Leave, FireInstanceName.Leave, FireC.Leave, FireH.Leave, FireO.Leave, FireN.Leave, FireCl.Leave, FireHoC.Leave, FireRadiativeFraction.Leave, FireTarget.SelectedIndexChanged, FireDefinitionName.Leave
        If CurrentFire >= 0 And myFires.Count > 0 Then
            Dim aFireTimeSeries(12, 0) As Single
            Dim aFire As New Fire, aFireInstance As New Fire
            aFireInstance = myFires(CurrentFire)

            ' Inputs related to fire instance

            If sender Is FireComp Then
                If FireComp.SelectedIndex > -1 Then aFireInstance.Compartment = FireComp.SelectedIndex
                If Val(FireXPosition.Text) = -1 Then aFireInstance.XPosition = Val(FireXPosition.Text)
                If Val(FireYPosition.Text) = -1 Then aFireInstance.YPosition = Val(FireYPosition.Text)
            End If
            If sender Is FireXPosition Then aFireInstance.XPosition = Val(FireXPosition.Text)
            If sender Is FireYPosition Then aFireInstance.YPosition = Val(FireYPosition.Text)
            If sender Is FireIgnitionCriteria Then
                If aFireInstance.IgnitionType <> FireIgnitionCriteria.SelectedIndex Then
                    aFireInstance.IgnitionType = FireIgnitionCriteria.SelectedIndex
                    If aFireInstance.IgnitionType = Fire.FireIgnitionbyTime Then aFireInstance.IgnitionValue = 0.0
                    If aFireInstance.IgnitionType = Fire.FireIgnitionbyTemperature Then aFireInstance.IgnitionValue = myEnvironment.IntAmbTemperature
                    If aFireInstance.IgnitionType = Fire.FireIgnitionbyFlux Then aFireInstance.IgnitionValue = 0.0
                End If
            End If
            If sender Is FireIgnitionValue Then aFireInstance.IgnitionValue = Val(FireIgnitionValue.Text)
            If sender Is FireTarget Then
                aFireInstance.Target = myTargets.Item(FireTarget.SelectedIndex).Name
            End If
            If sender Is FireInstanceName Then aFireInstance.Name = FireInstanceName.Text

            ' Inputs related to fire definition
            Dim fireIndex As Integer
            fireIndex = myFireProperties.GetFireIndex(aFireInstance.ReferencedFireDefinition)
            If fireIndex >= 0 Then
                aFire = myFireProperties(fireIndex)
                If sender Is FireDefinitionName Then
                    aFire.Name = FireDefinitionName.Text
                    aFireInstance.ReferencedFireDefinition = aFire.Name
                End If
                If sender Is FireRadiativeFraction Then aFire.RadiativeFraction = Val(FireRadiativeFraction.Text)
                If sender Is FireC Then
                    If Val(FireC.Text) > 0 Then
                        aFire.ChemicalFormula(formula.C) = Val(FireC.Text)
                    Else
                        aFire.ChemicalFormula(formula.C) = 1
                    End If
                End If
                If sender Is FireH Then aFire.ChemicalFormula(formula.H) = Val(FireH.Text)
                If sender Is FireO Then aFire.ChemicalFormula(formula.O) = Val(FireO.Text)
                If sender Is FireN Then aFire.ChemicalFormula(formula.N) = Val(FireN.Text)
                If sender Is FireCl Then aFire.ChemicalFormula(formula.Cl) = Val(FireCl.Text)
                If sender Is FireHoC Then aFire.HeatofCombustion = Val(FireHoC.Text)
                If sender Is FireRadiativeFraction Then aFire.RadiativeFraction = Val(FireRadiativeFraction.Text)

                ' Dim numPoints As Integer, ir As Integer
                ' If Val(FireHoC.Text) <> aFire.HeatofCombustion Then
                ' aFire.HeatofCombustion = Val(FireHoC.Text)
                ' numPoints = CountGridPoints(FireDataSS)
                ' For ir = 1 To numPoints
                ' FireDataSS(ir, Fire.FireMdot) = FireDataSS(ir, Fire.FireHRR) / aFire.HeatofCombustion
                ' Next
                'End If
                '    CopyFireData(aFire)

                myFireProperties(fireIndex) = aFire
            End If
            myFires(CurrentFire) = aFireInstance
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireData_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.BeforeRowColChange
        If CurrentFire >= 0 And myFires.Count > 0 Then
            Dim aFire As New Fire, aFireInstance As New Fire, fireIndex As Integer
            aFireInstance = myFires(CurrentFire)
            fireIndex = myFireProperties.GetFireIndex(aFireInstance.ReferencedFireDefinition)
            aFire = myFireProperties(fireIndex)
            CopyFireData(aFire)
            myFireProperties(fireIndex) = aFire
            UpdateGUI.Fires(CurrentFire)
        End If
    End Sub
    Private Sub FireData_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles FireDataSS.AfterRowColChange
        UpdateGUI.Fires(CurrentFire)
    End Sub
#End Region
#Region " Targets Tab "
    ' This section of code handles the events related to the targets tab
    Private Sub TargetAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetAdd.Click
        If myTargets.Count + 1 <= Target.MaximumTargets Then
            Dim aTarget As New Target
            aTarget.Type = Target.TypeTarget
            aTarget.Name = "Targ " + (myTargets.Count + 1).ToString
            myTargets.Add(aTarget)
            CurrentTarget = myTargets.Count - 1
            UpdateGUI.Targets(CurrentTarget)
        Else
            MessageBox.Show("A maximum of " + myTargets.Maximum.ToString + " targets are allowed. New target was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
    Private Sub TargetDup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetDup.Click
        ' Copy the current target, adding it to the end of the list of targets
        If CurrentTarget >= 0 And myTargets.Count > 0 And CurrentTarget + 1 <= myTargets.Maximum And myTargets.Count + 1 <= Target.MaximumTargets Then
            myTargets.Add(New Target)
            myTargets.Copy(CurrentTarget, myTargets.Count - 1)
            CurrentTarget = myTargets.Count - 1
            myTargets(myTargets.Count - 1).Name = "Targ " + myTargets.Count.ToString
            UpdateGUI.Targets(CurrentTarget)
        Else
            MessageBox.Show("A maximum of " + myTargets.Maximum.ToString + " Targets are allowed. New Target not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
        index = TargetSummary.RowSel - 1
        If index >= 0 And index <= myTargets.Count - 1 Then
            CurrentTarget = index
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
    Private Sub TargetSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles TargetSummary.AfterSelChange
        ' The currently selected Target has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = TargetSummary.RowSel - 1
        If index >= 0 And index <= myTargets.Count - 1 Then
            CurrentTarget = index
            UpdateGUI.Targets(CurrentTarget)
        End If
    End Sub
    Private Sub Target_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TargetComp.SelectedIndexChanged, TargetMaterial.SelectedIndexChanged, TargetSolutionType.SelectedIndexChanged, TargetXPosition.Leave, TargetYPosition.Leave, TargetZPosition.Leave, TargetXNormal.Leave, TargetYNormal.Leave, TargetZNormal.Leave, TargetNormalCalc.SelectedIndexChanged, TargetInternalLocation.Leave, TargetName.Leave
        Dim aTarget As New Target, numFires As Integer, i As Integer
        If CurrentTarget >= 0 And myTargets.Count > 0 Then
            aTarget = myTargets.Item(CurrentTarget)
            If sender Is TargetName Then aTarget.Name = TargetName.Text
            If sender Is TargetComp Then
                aTarget.Compartment = TargetComp.SelectedIndex
                If Val(TargetXPosition.Text) = -1 Then aTarget.XPosition = Val(TargetXPosition.Text)
                If Val(TargetYPosition.Text) = -1 Then aTarget.YPosition = Val(TargetYPosition.Text)
                If Val(TargetZPosition.Text) = -1 Then aTarget.ZPosition = Val(TargetZPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is TargetMaterial Then aTarget.Material = myThermalProperties.GetShortName(sender.text)
            If sender Is TargetSolutionType Then
                aTarget.SolutionType = TargetSolutionType.SelectedIndex
            End If
            If sender Is TargetInternalLocation Then aTarget.InternalLocation = Val(TargetInternalLocation.Text)
            If sender Is TargetXPosition Then
                aTarget.XPosition = Val(TargetXPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is TargetYPosition Then
                aTarget.YPosition = Val(TargetYPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is TargetZPosition Then
                aTarget.ZPosition = Val(TargetZPosition.Text)
                UpdateGUI.InitTargetNormalList(CurrentTarget)
            End If
            If sender Is TargetXNormal Then aTarget.XNormal = Val(TargetXNormal.Text)
            If sender Is TargetYNormal Then aTarget.YNormal = Val(TargetYNormal.Text)
            If sender Is TargetZNormal Then aTarget.ZNormal = Val(TargetZNormal.Text)
            If sender Is TargetNormalCalc Then
                If TargetNormalCalc.Text = "Right Wall" Then
                    aTarget.XNormal = 1
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 0
                ElseIf TargetNormalCalc.Text = "Left Wall" Then
                    aTarget.XNormal = -1
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 0
                ElseIf TargetNormalCalc.Text = "Rear Wall" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 1
                    aTarget.ZNormal = 0
                ElseIf TargetNormalCalc.Text = "Front Wall" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = -1
                    aTarget.ZNormal = 0
                ElseIf TargetNormalCalc.Text = "Floor" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 0
                    aTarget.ZNormal = -1
                ElseIf TargetNormalCalc.Text = "Ceiling" Then
                    aTarget.XNormal = 0
                    aTarget.YNormal = 0
                    aTarget.ZNormal = 1
                Else
                    numFires = myFires.Count
                    If numFires > 0 Then
                        Dim aFire As Fire
                        For i = 1 To numFires
                            aFire = myFires(i - 1)
                            If aTarget.Compartment = aFire.Compartment Then
                                If TargetNormalCalc.Text = "Fire " + i.ToString + ", " + aFire.Name Then
                                    Dim Hypotenuse As Single
                                    Hypotenuse = Math.Sqrt((aFire.XPosition - aTarget.XPosition) ^ 2 + (aFire.YPosition - aTarget.YPosition) ^ 2 + (aFire.Height - aTarget.ZPosition) ^ 2)
                                    If Hypotenuse <> 0 Then
                                        aTarget.XNormal = (aFire.XPosition - aTarget.XPosition) / Hypotenuse
                                        aTarget.YNormal = (aFire.YPosition - aTarget.YPosition) / Hypotenuse
                                        aTarget.ZNormal = (aFire.Height - aTarget.ZPosition) / Hypotenuse
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
            MessageBox.Show("A maximum of " + myDetectors.Maximum.ToString + " Detectors are allowed. New Detector was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + myDetectors.Maximum.ToString + " Detectors are allowed. New Detector not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
        index = DetectorSummary.RowSel - 1
        If index >= 0 And index <= myDetectors.Count - 1 Then
            CurrentDetector = index
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub DetectorSummary_AfterSelChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles DetectorSummary.AfterSelChange
        ' The currently selected Detector has been changed by selecting a row of the summary spreadsheet with the keyboard
        Dim index As Integer
        index = DetectorSummary.RowSel - 1
        If index >= 0 And index <= myDetectors.Count - 1 Then
            CurrentDetector = index
            UpdateGUI.Detectors(CurrentDetector)
        End If
    End Sub
    Private Sub Detector_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DetectorComp.SelectedIndexChanged, DetectorType.SelectedIndexChanged, DetectorActivationTemperature.Leave, DetectorActivationObscuration.Leave, DetectorXPosition.Leave, DetectorYPosition.Leave, DetectorZPosition.Leave, DetectorSprayDensity.Leave, DetectorRTI.Leave, DetectorName.Leave
        Dim aDetector As New Target
        If CurrentDetector >= 0 And myDetectors.Count > 0 Then
            aDetector = myDetectors(CurrentDetector)
            If sender Is DetectorName Then aDetector.Name = DetectorName.Text
            If sender Is DetectorType Then aDetector.DetectorType = DetectorType.SelectedIndex
            If sender Is DetectorComp Then
                aDetector.Compartment = DetectorComp.SelectedIndex
                If Val(DetectorXPosition.Text) = -1 Then aDetector.XPosition = Val(DetectorXPosition.Text)
                If Val(DetectorYPosition.Text) = -1 Then aDetector.YPosition = Val(DetectorYPosition.Text)
                If Val(DetectorZPosition.Text) = -1 Then aDetector.ZPosition = Val(DetectorZPosition.Text)
            End If
            If sender Is DetectorActivationTemperature Then aDetector.ActivationTemperature = Val(DetectorActivationTemperature.Text)
            If sender Is DetectorActivationObscuration Then aDetector.ActivationObscurationFlaming = Val(DetectorActivationObscuration.Text)
            If sender Is DetectorXPosition Then aDetector.XPosition = Val(DetectorXPosition.Text)
            If sender Is DetectorYPosition Then aDetector.YPosition = Val(DetectorYPosition.Text)
            If sender Is DetectorZPosition Then aDetector.ZPosition = Val(DetectorZPosition.Text)
            If sender Is DetectorRTI Then aDetector.RTI = Val(DetectorRTI.Text)
            If sender Is DetectorSprayDensity Then aDetector.SprayDensity = Val(DetectorSprayDensity.Text)
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
                MessageBox.Show("A maximum of " + myHHeats.Maximum.ToString + " heat transfer connections are allowed. New connection was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        ElseIf sender Is VHeatAdd Then
            If myVHeats.Count + 1 <= Vent.MaximumVHeats Then
                Dim aHeat As New Vent
                aHeat.VentType = Vent.TypeVHeat
                myVHeats.Add(aHeat)
                CurrentVHeat = myVHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myVHeats.Maximum.ToString + " heat transfer connections are allowed. New connection was not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
                MessageBox.Show("A maximum of " + myHHeats.Maximum.ToString + " heat transfer connections are allowed. New connection not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
            End If
        ElseIf sender Is VHeatDup Then
            If CurrentVHeat >= 0 And myVHeats.Count > 0 And CurrentVHeat + 1 <= myVHeats.Maximum And myVHeats.Count + 1 <= Vent.MaximumHHeats Then
                myVHeats.Add(New Vent)
                myVHeats.Copy(CurrentVHeat, myVHeats.Count - 1)
                CurrentVHeat = myVHeats.Count - 1
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            Else
                MessageBox.Show("A maximum of " + myVHeats.Maximum.ToString + " heat transfer connections are allowed. New connection not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            index = HHeatSummary.RowSel - 1
            If index >= 0 And index <= myHHeats.Count - 1 Then
                CurrentHHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatSummary Then
            index = VHeatSummary.RowSel - 1
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
            index = HHeatSummary.RowSel - 1
            If index >= 0 And index <= myHHeats.Count - 1 Then
                CurrentHHeat = index
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatSummary Then
            index = VHeatSummary.RowSel - 1
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
                If sender Is HHeatFraction Then aVent.InitialOpening = Val(HHeatFraction.Text)
                aVent.VentType = Vent.TypeHHeat
                If sender Is HHeatComp1 Then aVent.FirstCompartment = HHeatComp1.SelectedIndex - 1
                If sender Is HHeatComp2 Then aVent.SecondCompartment = HHeatComp2.SelectedIndex - 1
                myHHeats(CurrentHHeat) = aVent
                UpdateGUI.Heats(CurrentHHeat, CurrentVHeat)
            End If
        ElseIf sender Is VHeatComp1 Or sender Is VHeatComp2 Then
            If CurrentVHeat >= 0 And myVHeats.Count > 0 Then
                aVent = myVHeats.Item(CurrentVHeat)
                aVent.VentType = Vent.TypeVHeat
                If sender Is VHeatComp1 Then aVent.FirstCompartment = VHeatComp1.SelectedIndex - 1
                If sender Is VHeatComp2 Then aVent.SecondCompartment = VHeatComp2.SelectedIndex - 1
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
            MessageBox.Show("A maximum of " + Visual.MaximumVisuals.ToString + " visulaizations are allowed. New visual not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
            MessageBox.Show("A maximum of " + myVisuals.Maximum.ToString + " Visuals are allowed. New Visual not added.", Text, MessageBoxButtons.OK, MessageBoxIcon.Warning)
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
                aVisual.Value = Val(VisualizationValue.Text)
                myVisuals.Item(CurrentVisual) = aVisual
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
            Else
                Dim aCompartment As New Compartment
                aCompartment = myCompartments.Item(CurrentCompartment)
                If sender Is VisualizationX Then aCompartment.xGrid = Val(VisualizationX.Text)
                If sender Is VisualizationY Then aCompartment.yGrid = Val(VisualizationY.Text)
                If sender Is VisualizationZ Then aCompartment.zGrid = Val(VisualizationZ.Text)
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
                aVisual.Type = VisualizationType.SelectedIndex
                If VisualizationType.SelectedIndex = 0 Then
                    VisualizationValueLabel.Text = "Position:"
                    VisualizationValueLabel.Visible = True
                    VisualizationValue.Visible = True
                    VisualizationAxis.Visible = True
                    VisualizationAxisLabel.Visible = True
                ElseIf VisualizationType.SelectedIndex = 2 Then
                    VisualizationValueLabel.Text = "Temperature:"
                    VisualizationValueLabel.Visible = True
                    VisualizationValue.Visible = True
                    VisualizationAxis.Visible = False
                    VisualizationAxisLabel.Visible = False
                Else
                    VisualizationValueLabel.Visible = False
                    VisualizationValue.Visible = False
                    VisualizationAxis.Visible = False
                    VisualizationAxisLabel.Visible = False
                End If
            ElseIf sender Is VisualizationComp Then
                aVisual.Compartment = VisualizationComp.SelectedIndex - 1
            ElseIf sender Is VisualizationAxis Then
                aVisual.Axis = VisualizationAxis.SelectedIndex
            End If
            myVisuals.Item(CurrentVisual) = aVisual
            UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
        End If
    End Sub
    Private Sub VisualSummary_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VisualSummary.Click, VisualSummary.AfterSelChange, VisualResolutionSummary.Click, VisualResolutionSummary.AfterSelChange
        ' The currently selected compartment has been changed by selecting a row of the summary spreadsheet
        Dim index As Integer
        If sender Is VisualSummary Then
            index = VisualSummary.RowSel - 1
            If index >= 0 And index <= myVisuals.Count - 1 Then
                CurrentVisual = index
                UpdateGUI.Visuals(CurrentVisual, CurrentCompartment)
            End If
        ElseIf sender Is VisualResolutionSummary Then
            index = VisualResolutionSummary.RowSel - 1
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
        OpenDataFileDialog.FilterIndex = 1
        OpenDataFileDialog.ShowDialog()
        If OpenDataFileDialog.FileNames.Length > 0 Then
            Dim FileName As String
            For Each FileName In OpenDataFileDialog.FileNames
                OpenDataFile(FileName)
            Next
        End If
        UpdateAll()
    End Sub
    Private Sub MainSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainSave.Click, MenuSave.Click
        SaveDataFile(False, False)
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
        OpenDataFileDialog.FilterIndex = 1
        OpenDataFileDialog.ShowDialog()
        If OpenDataFileDialog.FileNames.Length > 0 Then
            Dim FileName As String
            For Each FileName In OpenDataFileDialog.FileNames
                InitNew()
                OpenDataFile(FileName)
            Next
        End If
        UpdateAll()
    End Sub
    Private Sub MenuSaveAs_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuSaveAs.Click
        Dim PathName As String, FileExtension As String
        myUnits.SI = True
        SaveDataFileDialog.FileName = myEnvironment.InputFileName + ".in"
        SaveDataFileDialog.FilterIndex = 1
        SaveDataFileDialog.Title = "Save As"
        SaveDataFileDialog.OverwritePrompt = True
        If Me.SaveDataFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
            If SaveDataFileDialog.FileName <> " " Then
                If SaveDataFileDialog.FilterIndex = 1 Then
                    WriteInputFileNML(SaveDataFileDialog.FileName)
                    FileExtension = ".in"
                Else
                    WriteInputFileCSV(SaveDataFileDialog.FileName)
                    FileExtension = ".in"
                End If
                myEnvironment.InputFileName = SaveDataFileDialog.FileName
                myEnvironment.InputFilePath = SaveDataFileDialog.FileName
                Text = "CEdit (" + System.IO.Path.GetFileName(SaveDataFileDialog.FileName) + ")"
                myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + FileExtension)
            End If
            PathName = System.IO.Path.GetDirectoryName(SaveDataFileDialog.FileName) & "\"
            ChDir(PathName)
        End If
        myUnits.SI = False
        UpdateGUI.Menu()
        UpdateGUI.General()
    End Sub
    Private Sub MenuExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuExit.Click
        Application.Exit()
    End Sub
    Private Sub MenuAbout_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuAbout.Click
        About.ShowDialog()
    End Sub
    Private Sub MenuRecent_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuRecent1.Click, MenuRecent2.Click, MenuRecent3.Click, MenuRecent4.Click
        Dim i As Integer
        If sender Is MenuRecent1 Then i = 0
        If sender Is MenuRecent2 Then i = 1
        If sender Is MenuRecent3 Then i = 2
        If sender Is MenuRecent4 Then i = 3
        OpenDataFile(myRecentFiles.Filenames(i))
        UpdateAll()
    End Sub
    Private Sub MenuHelpUpdate_Click(sender As Object, e As EventArgs) Handles MenuHelpUpdate.Click
        OpenDataFileDialog.FilterIndex = 1
        OpenDataFileDialog.Multiselect = True
        OpenDataFileDialog.ShowDialog()
        If OpenDataFileDialog.FileNames.Length > 0 Then
            Dim myInputFiles As New UpdateInputFiles
            Dim FileName As String
            For Each FileName In OpenDataFileDialog.FileNames
                InitNew()
                OpenDataFile(FileName)
                SaveDataFile(False, True)
                If myErrors.Count > 0 Then
                    Dim myEnumerator As System.Collections.IEnumerator = myErrors.Queue.GetEnumerator()
                    While myEnumerator.MoveNext()
                        myInputFiles.UpdateErrors.Text = myEnumerator.Current + ControlChars.CrLf + myInputFiles.UpdateErrors.Text
                    End While
                End If
                myInputFiles.Update()
            Next
            myInputFiles.ShowDialog()
        End If
        OpenDataFileDialog.Multiselect = False
        InitNew()
    End Sub
    Private Sub MenuHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuShowHelp.Click
        System.Windows.Forms.MessageBox.Show("Documentation on the use of CFAST is available in the CFAST User's Guide installed with the software. Go to Start, All Programs, CFAST7, Documents for all of the CFAST documentation", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub
    Private Sub MenuCFASTweb_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuCFASTWeb.Click
        Process.Start("http://cfast.nist.gov")
    End Sub
    Private Sub MenuViewOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MenuViewOutput.Click, MenuViewInput.Click, MenuViewLog.Click
        If sender Is MenuViewOutput Then ViewFile.FileName = myEnvironment.InputFileName + ".out"
        If sender Is MenuViewInput Then ViewFile.FileName = myEnvironment.InputFileName + ".in"
        If sender Is MenuViewLog Then ViewFile.FileName = myEnvironment.InputFileName + ".log"
        If System.IO.File.Exists(ViewFile.FileName) Then
            MenuView.Enabled = False
            ViewFile.ShowDialog()
            MenuView.Enabled = True
        End If
    End Sub
    Private Sub ValidationOutput_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OutputValidation.CheckedChanged
        If OutputValidation.Checked Then
            ValidationOutput = True
        Else
            ValidationOutput = False
        End If
        SaveSetting("CFAST", "Options", "Validation", ValidationOutput.ToString)
        UpdateGUI.General()
    End Sub
    Private Sub DebugOutput_Click(sender As System.Object, e As System.EventArgs) Handles OutputDebug.CheckedChanged
        If OutputDebug.Checked Then
            DebugOutput = True
        Else
            DebugOutput = False
        End If
        UpdateGUI.General()
    End Sub
    Private Sub ShowCFAST_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OutputShowCFAST.CheckedChanged
        If OutputShowCFAST.Checked Then
            CommandWindowVisible = True
        Else
            CommandWindowVisible = False
        End If
        SaveSetting("CFAST", "Options", "ShowCFASTOutput", CommandWindowVisible.ToString)
        UpdateGUI.General()
    End Sub
    Private Sub FromFileInserts_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ThermalFromFile.Click, FireFromFile.Click
        Dim iReturn As Integer
        OpenDataFileDialog.FilterIndex = 1
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
                            aFire = TempFires.Item(i - 1)
                            myFireProperties.Add(aFire)
                        End If
                    Next
                End If
            End If
        End If
        UpdateAll()
    End Sub
#End Region
#Region " Support Routines "

    Private Sub Data_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FireDataSS.KeyDown, VVentFractions.KeyDown, HVentFractions.KeyDown, MVentFractions.KeyDown
        Dim aChar As String
        aChar = e.KeyCode.ToString
        If e.Control Then
            If aChar = "Delete" Then
                If sender.Row > 0 And sender.Col >= 0 Then
                    sender(sender.Row, sender.Col) = " "
                End If
            End If
        End If
        If e.Alt Then
            If aChar = "Delete" Then
                Dim Current As Integer, Count As Integer
                If sender Is FireDataSS Then
                    Current = CurrentFire
                    Count = myFireProperties.Count
                ElseIf sender Is HVentFractions Then
                    Current = CurrentHVent
                    Count = myHVents.Count
                ElseIf sender Is VVentFractions Then
                    Current = CurrentVVent
                    Count = myVVents.Count
                ElseIf sender Is MVentFractions Then
                    Current = CurrentMVent
                    Count = myMVents.Count
                End If
                If sender.Row > 0 And sender.Col >= 0 Then
                    If Current >= 0 And Current < Count Then
                        Dim numPoints As Integer, i As Integer, j As Integer
                        numPoints = CountGridPoints(sender)
                        If numPoints > sender.Row Then
                            For i = sender.Row To numPoints
                                For j = 0 To sender.Cols.Count - 1
                                    sender(i, j) = sender(i + 1, j)
                                Next
                            Next
                        End If
                        For j = 0 To sender.Cols.Count - 1
                            sender(numPoints, j) = Nothing
                        Next
                        If sender Is FireDataSS Then
                            Dim aFire As New Fire
                            aFire = myFireProperties(CurrentFire)
                            CopyFireData(aFire)
                            myFireProperties(CurrentFire) = aFire
                            UpdateGUI.Fires(CurrentFire)
                        ElseIf sender Is HVentFractions Then
                            Dim aVent As New Vent
                            aVent = myHVents(CurrentVVent)
                            CopyVentData(aVent, sender)
                            myHVents(CurrentHVent) = aVent
                            UpdateGUI.HVents(CurrentVVent)
                        ElseIf sender Is VVentFractions Then
                            Dim aVent As New Vent
                            aVent = myVVents(CurrentVVent)
                            CopyVentData(aVent, sender)
                            myVVents(CurrentVVent) = aVent
                            UpdateGUI.VVents(CurrentVVent)
                        ElseIf sender Is MVentFractions Then
                            Dim aVent As New Vent
                            aVent = myMVents(CurrentMVent)
                            CopyVentData(aVent, sender)
                            myMVents(CurrentMVent) = aVent
                            UpdateGUI.MVents(CurrentVVent)
                        End If
                    End If
                End If
            End If
            If aChar = "Insert" Then
                If sender.Row > 0 Then
                    Dim numPoints As Integer, i As Integer, j As Integer
                    numPoints = CountGridPoints(sender)
                    If numPoints > sender.Row Then
                        For i = numPoints To sender.Row Step -1
                            For j = 0 To sender.Cols.Count - 1
                                sender(i + 1, j) = sender(i, j)
                            Next
                        Next
                        For j = 0 To sender.Cols.Count - 1
                            sender(sender.Row, j) = 0
                        Next
                    End If

                End If
            End If
        End If
    End Sub

    Friend Sub CopyFireData(ByVal aFire As Fire)
        ' Copies time dependent data from the display spreadsheet to the appropriate fire object data array
        Dim numPoints As Integer, ir As Integer, ic As Integer
        numPoints = CountGridPoints(FireDataSS)
        If numPoints > 0 Then
            Dim aFireTimeSeries(12, numPoints - 1) As Single
            For ir = 0 To numPoints - 1
                For ic = 0 To 12
                    aFireTimeSeries(ic, ir) = Val(FireDataSS(ir + 1, ic))
                Next
            Next
            aFire.SetFireData(aFireTimeSeries)
            End If
    End Sub

    Friend ReadOnly Property CountGridPoints(ByVal obj As C1.Win.C1FlexGrid.C1FlexGrid) As Integer
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
    Private Sub CopyVentData(ByVal aVent As Vent, ByVal ss As C1.Win.C1FlexGrid.C1FlexGrid)
        ' Copies time dependent data from the display spreadsheet to the appropriate vent object data array
        Dim NumPoints As Integer, ir As Integer
        NumPoints = CountGridPoints(ss)
        If NumPoints = 0 Then
            Dim TimePoints(0) As Single, FractionPoints(0) As Single
            aVent.SetRamp(TimePoints, FractionPoints)
        Else
            Dim TimePoints(NumPoints) As Single, FractionPoints(NumPoints) As Single
            For ir = 1 To NumPoints
                TimePoints(ir) = Val(ss(ir, 0))
                FractionPoints(ir) = Val(ss(ir, 1))
                If FractionPoints(ir) <= 0 Then FractionPoints(ir) = 0
            Next
            aVent.SetRamp(TimePoints, FractionPoints)
        End If
    End Sub
    Private Sub Vent_Fraction_BeforeRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles HVentFractions.BeforeRowColChange, VVentFractions.BeforeRowColChange, MVentFractions.BeforeRowColChange
        Dim aVent As New Vent
        If sender Is HVentFractions Then
            If CurrentHVent >= 0 And myHVents.Count > 0 Then
                aVent = myHVents.Item(CurrentHVent)
                CopyVentData(aVent, HVentFractions)
                myHVents.Item(CurrentHVent) = aVent
                UpdateGUI.HVents(CurrentHVent)
            End If
        ElseIf sender Is VVentFractions Then
            If CurrentVVent >= 0 And myVVents.Count > 0 Then
                aVent = myVVents.Item(CurrentVVent)
                CopyVentData(aVent, VVentFractions)
                myVVents.Item(CurrentVVent) = aVent
                UpdateGUI.VVents(CurrentVVent)
            End If
        ElseIf sender Is MVentFractions Then
            If CurrentMVent >= 0 And myMVents.Count > 0 Then
                aVent = myMVents.Item(CurrentMVent)
                CopyVentData(aVent, MVentFractions)
                myMVents.Item(CurrentMVent) = aVent
                UpdateGUI.MVents(CurrentMVent)
            End If
        End If
    End Sub
    Private Sub Vent_Fraction_AfterRowColChange(ByVal sender As Object, ByVal e As C1.Win.C1FlexGrid.RangeEventArgs) Handles HVentFractions.AfterRowColChange, VVentFractions.AfterRowColChange, MVentFractions.AfterRowColChange
        If sender Is HVentFractions Then
            UpdateGUI.HVents(CurrentHVent)
        ElseIf sender Is VVentFractions Then
            UpdateGUI.VVents(CurrentVVent)
        ElseIf sender Is MVentFractions Then
            UpdateGUI.MVents(CurrentMVent)
        End If
    End Sub
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
    Private Sub SaveDataFile(ByVal Prompt As Boolean, ByVal ForceWrite As Boolean)
        Dim PathName As String
        myUnits.SI = True
        If myEnvironment.FileChanged() Or ForceWrite Then
            If Prompt Or myEnvironment.InputFileName = Nothing Or myEnvironment.InputFileName = "" Then
                SaveDataFileDialog.Title = "Save"
                SaveDataFileDialog.OverwritePrompt = True
                If SaveDataFileDialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                    If SaveDataFileDialog.FileName <> " " Then
                        PathName = System.IO.Path.GetDirectoryName(SaveDataFileDialog.FileName) & "\"
                        ChDir(PathName)
                        ' Write out the data file since it has been changed
                        WriteInputFileNML(SaveDataFileDialog.FileName)
                        myEnvironment.InputFileName = SaveDataFileDialog.FileName
                        myEnvironment.InputFilePath = SaveDataFileDialog.FileName
                        myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
                    End If
                End If
            Else
                WriteInputFileNML(myEnvironment.InputFileName + ".in")
                myRecentFiles.Add(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in")
            End If
        End If
        myUnits.SI = False
        UpdateGUI.Menu()
        UpdateGUI.General()
    End Sub
    Private Sub RunCFAST()
        If myEnvironment.FileChanged Then SaveDataFile(True, False)
        If System.IO.File.Exists(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".cfast") Then
            Dim RunSimulation As New RunModel
            CFASTSimulationTime = myEnvironment.SimulationTime
            CFastInputFile = myEnvironment.InputFileName
            CFastInputFilewithExtension = CFastInputFile + ".cfast"
            RunSimulation.Text = "Run Model (" + CFastInputFilewithExtension + ")"
            RunSimulation.ShowDialog()

            UpdateGUI.Menu()
            UpdateGUI.Environment()
        ElseIf System.IO.File.Exists(myEnvironment.InputFilePath + "\" + myEnvironment.InputFileName + ".in") Then
            Dim RunSimulation As New RunModel
            CFASTSimulationTime = myEnvironment.SimulationTime
            CFastInputFile = myEnvironment.InputFileName
            CFastInputFilewithExtension = CFastInputFile + ".in"
            RunSimulation.Text = "Run Model (" + CFastInputFilewithExtension + ")"
            RunSimulation.ShowDialog()

            UpdateGUI.Menu()
            UpdateGUI.Environment()
        End If
    End Sub
    Private Sub RunSMVGeometry()
        Dim CommandString As String, found As Integer, ProcessID As Integer
        If myEnvironment.FileChanged Then SaveDataFile(True, False)
        Try
            found = myEnvironment.InputFileName.IndexOf(" ", 0)
            If found <= 0 Then
                CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + myEnvironment.InputFileName + " -I"
            Else
                CommandString = """" + Application.StartupPath + "\CFAST.exe"" " + """" + myEnvironment.InputFileName + """" + " -I"
            End If
            If OutputShowCFAST.Checked Then
                ProcessID = Shell(CommandString, AppWinStyle.NormalNoFocus, True, 5000)
            Else
                ProcessID = Shell(CommandString, AppWinStyle.Hide, True, 5000)
            End If
        Catch ex As Exception
        End Try
        UpdateGUI.UpdateLogFile(EnvErrors)
    End Sub
    Private Sub RunSmokeView()
        Dim CommandString As String, found As Integer, ProcessID As Integer
        If myEnvironment.FileChanged Then SaveDataFile(True, False)
        Try
            found = myEnvironment.InputFileName.IndexOf(" ", 0)
            If found <= 0 Then
                CommandString = """" + Application.StartupPath + "\..\SMV6\smokeview.exe"" " + myEnvironment.InputFileName
            Else
                CommandString = """" + Application.StartupPath + "\..\SMV6\smokeview.exe"" " + """" + myEnvironment.InputFileName + """"
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
        UpdateGUI.Targets(CurrentTarget)
        UpdateGUI.Fires(CurrentFire)
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
        Text = "CEdit"
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
        myFireProperties.Clear()
        TempFires.Clear()
        myVisuals.Clear()
        myThermalProperties.Clear()
        TempThermalProperties.Clear()
        myErrors.Queue.Clear()
        myRamps.Clear()
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

        UpdateGUI.InitThermalPropertyList(CompCeiling)
        UpdateGUI.InitThermalPropertyList(CompWalls)
        UpdateGUI.InitThermalPropertyList(CompFloor)
        UpdateGUI.InitThermalPropertyList(TargetMaterial)

        ' Initialize spreadsheets for input or no input (summary tables) as appropriate
        UpdateGUI.InitSummaryGrid(ThermalSummary)
        UpdateGUI.InitSummaryGrid(CompSummary)
        UpdateGUI.InitSummaryGrid(HVentSummary)
        UpdateGUI.InitSummaryGrid(VVentSummary)
        UpdateGUI.InitSummaryGrid(MVentSummary)
        UpdateGUI.InitSummaryGrid(TargetSummary)
        UpdateGUI.InitSummaryGrid(DetectorSummary)
        UpdateGUI.InitSummaryGrid(FireSummary)

        UpdateGUI.InitEditGrid(CompVariableArea)
        UpdateGUI.InitEditGrid(HVentFractions)
        UpdateGUI.InitEditGrid(VVentFractions)
        UpdateGUI.InitEditGrid(MVentFractions)

        ' Turn off all input except the simulation environment and compartment add since all others stuff depends on have a compartment
        TabHorizontalFlow.Enabled = False
        TabVerticalFlow.Enabled = False
        TabMechanicalFlow.Enabled = False
        TabTargets.Enabled = False
        TabDetection.Enabled = False
        TabHeatTransfer.Enabled = False
        TabFires.Enabled = False
        GroupCompartments.Enabled = False
        MenuViewInput.Enabled = False
        MenuViewOutput.Enabled = False
        MenuViewLog.Enabled = False
        UpdateAll()
    End Sub
#End Region
End Class
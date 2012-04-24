<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmEstimator
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmEstimator))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.StairHallExitRate = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.StairHallWidth = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.StairHallLength = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.StairEntryRate = New System.Windows.Forms.TextBox()
        Me.FirstFloorUseStairwells = New System.Windows.Forms.CheckBox()
        Me.ExitHallEntryRate = New System.Windows.Forms.TextBox()
        Me.NumStairwells = New System.Windows.Forms.TextBox()
        Me.HallEntryRate = New System.Windows.Forms.TextBox()
        Me.StairTreadDepth = New System.Windows.Forms.TextBox()
        Me.StairRiserHeight = New System.Windows.Forms.TextBox()
        Me.HallWidth = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.StairsPerFlight = New System.Windows.Forms.TextBox()
        Me.FlightsPerFloor = New System.Windows.Forms.TextBox()
        Me.HallLength = New System.Windows.Forms.TextBox()
        Me.StairWidth = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.BuildingFile = New System.Windows.Forms.CheckBox()
        Me.ElevatorFraction = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.NumOccupants = New System.Windows.Forms.TextBox()
        Me.NumFloors = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.btnDebug = New System.Windows.Forms.Button()
        Me.Estimate = New System.Windows.Forms.Button()
        Me.Progress = New System.Windows.Forms.ProgressBar()
        Me.EndEstimate = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.SaveFileDialog = New System.Windows.Forms.SaveFileDialog()
        Me.timerEstimate = New System.Windows.Forms.Timer(Me.components)
        Me.EgressTime = New System.Windows.Forms.Label()
        Me.NumberEvacuated = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.ElevatorCustomCapacity = New System.Windows.Forms.TextBox()
        Me.ElevatorCapacity = New System.Windows.Forms.ComboBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.DoorType = New System.Windows.Forms.ComboBox()
        Me.ElevatorStartup = New System.Windows.Forms.TextBox()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.ElevatorAcceleration = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.ElevatorVelocity = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.NumElevators = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.EndEstimateTime = New System.Windows.Forms.TextBox()
        Me.Batchmode = New System.Windows.Forms.Button()
        Me.BatchRunCounter = New System.Windows.Forms.Label()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label14)
        Me.GroupBox1.Controls.Add(Me.StairHallExitRate)
        Me.GroupBox1.Controls.Add(Me.Label15)
        Me.GroupBox1.Controls.Add(Me.StairHallWidth)
        Me.GroupBox1.Controls.Add(Me.Label16)
        Me.GroupBox1.Controls.Add(Me.StairHallLength)
        Me.GroupBox1.Controls.Add(Me.Label20)
        Me.GroupBox1.Controls.Add(Me.StairEntryRate)
        Me.GroupBox1.Controls.Add(Me.FirstFloorUseStairwells)
        Me.GroupBox1.Controls.Add(Me.ExitHallEntryRate)
        Me.GroupBox1.Controls.Add(Me.NumStairwells)
        Me.GroupBox1.Controls.Add(Me.HallEntryRate)
        Me.GroupBox1.Controls.Add(Me.StairTreadDepth)
        Me.GroupBox1.Controls.Add(Me.StairRiserHeight)
        Me.GroupBox1.Controls.Add(Me.HallWidth)
        Me.GroupBox1.Controls.Add(Me.Label13)
        Me.GroupBox1.Controls.Add(Me.StairsPerFlight)
        Me.GroupBox1.Controls.Add(Me.FlightsPerFloor)
        Me.GroupBox1.Controls.Add(Me.HallLength)
        Me.GroupBox1.Controls.Add(Me.StairWidth)
        Me.GroupBox1.Controls.Add(Me.Label9)
        Me.GroupBox1.Controls.Add(Me.Label12)
        Me.GroupBox1.Controls.Add(Me.Label8)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.Label11)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.Label10)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Location = New System.Drawing.Point(13, 70)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(489, 295)
        Me.GroupBox1.TabIndex = 2
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Stairwells"
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(298, 241)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(72, 13)
        Me.Label14.TabIndex = 40
        Me.Label14.Text = "Exit Width (m)"
        Me.Label14.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'StairHallExitRate
        '
        Me.StairHallExitRate.Location = New System.Drawing.Point(379, 263)
        Me.StairHallExitRate.Name = "StairHallExitRate"
        Me.StairHallExitRate.Size = New System.Drawing.Size(100, 20)
        Me.StairHallExitRate.TabIndex = 44
        '
        'Label15
        '
        Me.Label15.Location = New System.Drawing.Point(7, 234)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(122, 26)
        Me.Label15.TabIndex = 39
        Me.Label15.Text = "Distance to Exit Discharge (m)"
        Me.Label15.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'StairHallWidth
        '
        Me.StairHallWidth.Location = New System.Drawing.Point(379, 237)
        Me.StairHallWidth.Name = "StairHallWidth"
        Me.StairHallWidth.Size = New System.Drawing.Size(100, 20)
        Me.StairHallWidth.TabIndex = 43
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.Location = New System.Drawing.Point(61, 267)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(309, 13)
        Me.Label16.TabIndex = 41
        Me.Label16.Text = "Maximum Rate Occupants Can Leave Exit Discharge (people/s)"
        '
        'StairHallLength
        '
        Me.StairHallLength.Location = New System.Drawing.Point(136, 237)
        Me.StairHallLength.Name = "StairHallLength"
        Me.StairHallLength.Size = New System.Drawing.Size(100, 20)
        Me.StairHallLength.TabIndex = 42
        '
        'Label20
        '
        Me.Label20.AutoSize = True
        Me.Label20.Location = New System.Drawing.Point(28, 87)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(103, 13)
        Me.Label20.TabIndex = 32
        Me.Label20.Text = "Number of Stairwells"
        '
        'StairEntryRate
        '
        Me.StairEntryRate.Location = New System.Drawing.Point(380, 185)
        Me.StairEntryRate.Name = "StairEntryRate"
        Me.StairEntryRate.Size = New System.Drawing.Size(100, 20)
        Me.StairEntryRate.TabIndex = 25
        '
        'FirstFloorUseStairwells
        '
        Me.FirstFloorUseStairwells.AutoSize = True
        Me.FirstFloorUseStairwells.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.FirstFloorUseStairwells.Location = New System.Drawing.Point(270, 86)
        Me.FirstFloorUseStairwells.Name = "FirstFloorUseStairwells"
        Me.FirstFloorUseStairwells.Size = New System.Drawing.Size(210, 17)
        Me.FirstFloorUseStairwells.TabIndex = 6
        Me.FirstFloorUseStairwells.Text = "Occupants on First Floor Use Stairwells"
        Me.FirstFloorUseStairwells.UseVisualStyleBackColor = True
        '
        'ExitHallEntryRate
        '
        Me.ExitHallEntryRate.Location = New System.Drawing.Point(380, 211)
        Me.ExitHallEntryRate.Name = "ExitHallEntryRate"
        Me.ExitHallEntryRate.Size = New System.Drawing.Size(100, 20)
        Me.ExitHallEntryRate.TabIndex = 37
        '
        'NumStairwells
        '
        Me.NumStairwells.Location = New System.Drawing.Point(136, 84)
        Me.NumStairwells.Name = "NumStairwells"
        Me.NumStairwells.Size = New System.Drawing.Size(100, 20)
        Me.NumStairwells.TabIndex = 31
        '
        'HallEntryRate
        '
        Me.HallEntryRate.Location = New System.Drawing.Point(347, 36)
        Me.HallEntryRate.Name = "HallEntryRate"
        Me.HallEntryRate.Size = New System.Drawing.Size(100, 20)
        Me.HallEntryRate.TabIndex = 31
        '
        'StairTreadDepth
        '
        Me.StairTreadDepth.Location = New System.Drawing.Point(380, 159)
        Me.StairTreadDepth.Name = "StairTreadDepth"
        Me.StairTreadDepth.Size = New System.Drawing.Size(100, 20)
        Me.StairTreadDepth.TabIndex = 24
        '
        'StairRiserHeight
        '
        Me.StairRiserHeight.Location = New System.Drawing.Point(136, 159)
        Me.StairRiserHeight.Name = "StairRiserHeight"
        Me.StairRiserHeight.Size = New System.Drawing.Size(100, 20)
        Me.StairRiserHeight.TabIndex = 23
        '
        'HallWidth
        '
        Me.HallWidth.Location = New System.Drawing.Point(347, 10)
        Me.HallWidth.Name = "HallWidth"
        Me.HallWidth.Size = New System.Drawing.Size(100, 20)
        Me.HallWidth.TabIndex = 30
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(94, 214)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(280, 13)
        Me.Label13.TabIndex = 32
        Me.Label13.Text = "Maximum Rate Occupants Can Leave Stairwell (people/s)"
        '
        'StairsPerFlight
        '
        Me.StairsPerFlight.Location = New System.Drawing.Point(380, 133)
        Me.StairsPerFlight.Name = "StairsPerFlight"
        Me.StairsPerFlight.Size = New System.Drawing.Size(100, 20)
        Me.StairsPerFlight.TabIndex = 22
        '
        'FlightsPerFloor
        '
        Me.FlightsPerFloor.Location = New System.Drawing.Point(136, 133)
        Me.FlightsPerFloor.Name = "FlightsPerFloor"
        Me.FlightsPerFloor.Size = New System.Drawing.Size(100, 20)
        Me.FlightsPerFloor.TabIndex = 21
        '
        'HallLength
        '
        Me.HallLength.Location = New System.Drawing.Point(164, 10)
        Me.HallLength.Name = "HallLength"
        Me.HallLength.Size = New System.Drawing.Size(100, 20)
        Me.HallLength.TabIndex = 29
        '
        'StairWidth
        '
        Me.StairWidth.Location = New System.Drawing.Point(136, 109)
        Me.StairWidth.Name = "StairWidth"
        Me.StairWidth.Size = New System.Drawing.Size(100, 20)
        Me.StairWidth.TabIndex = 20
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(99, 188)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(275, 13)
        Me.Label9.TabIndex = 18
        Me.Label9.Text = "Maximum Rate Occupants Can Enter Stairwell (people/s)"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(68, 39)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(273, 13)
        Me.Label12.TabIndex = 26
        Me.Label12.Text = "Maximum Rate Occupants Can Enter Hallway (people/s)"
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(54, 112)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(76, 13)
        Me.Label8.TabIndex = 15
        Me.Label8.Text = "Stair Width (m)"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(242, 137)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(132, 13)
        Me.Label7.TabIndex = 13
        Me.Label7.Text = "Number of Stairs Per Flight"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(268, 13)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(73, 13)
        Me.Label11.TabIndex = 24
        Me.Label11.Text = "Hall Width (m)"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(258, 162)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(116, 13)
        Me.Label6.TabIndex = 11
        Me.Label6.Text = "Stair Tread Depth (mm)"
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(6, 13)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(146, 13)
        Me.Label10.TabIndex = 22
        Me.Label10.Text = "Maximum Travel Distance (m)"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(16, 162)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(114, 13)
        Me.Label5.TabIndex = 9
        Me.Label5.Text = "Stair Riser Height (mm)"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(7, 137)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(123, 13)
        Me.Label4.TabIndex = 7
        Me.Label4.Text = "Flights of Stairs Per Floor"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.BuildingFile)
        Me.GroupBox2.Controls.Add(Me.ElevatorFraction)
        Me.GroupBox2.Controls.Add(Me.Label17)
        Me.GroupBox2.Controls.Add(Me.NumOccupants)
        Me.GroupBox2.Controls.Add(Me.NumFloors)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.Label1)
        Me.GroupBox2.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(990, 52)
        Me.GroupBox2.TabIndex = 3
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Building and Population"
        '
        'BuildingFile
        '
        Me.BuildingFile.AutoSize = True
        Me.BuildingFile.Location = New System.Drawing.Point(720, 20)
        Me.BuildingFile.Name = "BuildingFile"
        Me.BuildingFile.Size = New System.Drawing.Size(124, 17)
        Me.BuildingFile.TabIndex = 36
        Me.BuildingFile.Text = "Use building data file"
        Me.BuildingFile.UseVisualStyleBackColor = True
        '
        'ElevatorFraction
        '
        Me.ElevatorFraction.Location = New System.Drawing.Point(635, 16)
        Me.ElevatorFraction.Name = "ElevatorFraction"
        Me.ElevatorFraction.Size = New System.Drawing.Size(49, 20)
        Me.ElevatorFraction.TabIndex = 35
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.Location = New System.Drawing.Point(427, 19)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(202, 13)
        Me.Label17.TabIndex = 34
        Me.Label17.Text = "Fraction of Occupants that Use Elevators"
        '
        'NumOccupants
        '
        Me.NumOccupants.Location = New System.Drawing.Point(358, 16)
        Me.NumOccupants.Name = "NumOccupants"
        Me.NumOccupants.Size = New System.Drawing.Size(63, 20)
        Me.NumOccupants.TabIndex = 28
        '
        'NumFloors
        '
        Me.NumFloors.AcceptsReturn = True
        Me.NumFloors.Location = New System.Drawing.Point(103, 16)
        Me.NumFloors.Name = "NumFloors"
        Me.NumFloors.Size = New System.Drawing.Size(61, 20)
        Me.NumFloors.TabIndex = 27
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(170, 19)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(182, 13)
        Me.Label2.TabIndex = 5
        Me.Label2.Text = "Number of Occupants On Each Floor"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(7, 20)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(87, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "Number of Floors"
        '
        'btnDebug
        '
        Me.btnDebug.Enabled = False
        Me.btnDebug.Location = New System.Drawing.Point(9, 412)
        Me.btnDebug.Name = "btnDebug"
        Me.btnDebug.Size = New System.Drawing.Size(99, 23)
        Me.btnDebug.TabIndex = 33
        Me.btnDebug.Text = "Turn Debug Off"
        Me.btnDebug.UseVisualStyleBackColor = True
        Me.btnDebug.Visible = False
        '
        'Estimate
        '
        Me.Estimate.Location = New System.Drawing.Point(481, 412)
        Me.Estimate.Name = "Estimate"
        Me.Estimate.Size = New System.Drawing.Size(75, 23)
        Me.Estimate.TabIndex = 5
        Me.Estimate.Text = "Estimate"
        Me.Estimate.UseVisualStyleBackColor = True
        '
        'Progress
        '
        Me.Progress.Location = New System.Drawing.Point(12, 448)
        Me.Progress.Name = "Progress"
        Me.Progress.Size = New System.Drawing.Size(990, 10)
        Me.Progress.TabIndex = 6
        '
        'EndEstimate
        '
        Me.EndEstimate.FormattingEnabled = True
        Me.EndEstimate.Items.AddRange(New Object() {"When last occupant leaves building", "At time = "})
        Me.EndEstimate.Location = New System.Drawing.Point(490, 385)
        Me.EndEstimate.Name = "EndEstimate"
        Me.EndEstimate.Size = New System.Drawing.Size(195, 21)
        Me.EndEstimate.TabIndex = 7
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(224, 388)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(260, 13)
        Me.Label3.TabIndex = 8
        Me.Label3.Text = "End Estimate (enter time in seconds for custom timing)"
        '
        'SaveFileDialog
        '
        Me.SaveFileDialog.DefaultExt = "csv"
        Me.SaveFileDialog.Filter = "comma delimited files|*.csv|All files |*.*"
        '
        'timerEstimate
        '
        '
        'EgressTime
        '
        Me.EgressTime.AutoSize = True
        Me.EgressTime.Location = New System.Drawing.Point(612, 417)
        Me.EgressTime.Name = "EgressTime"
        Me.EgressTime.Size = New System.Drawing.Size(83, 13)
        Me.EgressTime.TabIndex = 43
        Me.EgressTime.Text = "Time(s) 123.453"
        '
        'NumberEvacuated
        '
        Me.NumberEvacuated.AutoSize = True
        Me.NumberEvacuated.Location = New System.Drawing.Point(312, 417)
        Me.NumberEvacuated.Name = "NumberEvacuated"
        Me.NumberEvacuated.Size = New System.Drawing.Size(135, 13)
        Me.NumberEvacuated.TabIndex = 44
        Me.NumberEvacuated.Text = "Evacuated (persons) = 000"
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.ElevatorCustomCapacity)
        Me.GroupBox4.Controls.Add(Me.ElevatorCapacity)
        Me.GroupBox4.Controls.Add(Me.Label26)
        Me.GroupBox4.Controls.Add(Me.DoorType)
        Me.GroupBox4.Controls.Add(Me.ElevatorStartup)
        Me.GroupBox4.Controls.Add(Me.Label25)
        Me.GroupBox4.Controls.Add(Me.ElevatorAcceleration)
        Me.GroupBox4.Controls.Add(Me.Label24)
        Me.GroupBox4.Controls.Add(Me.ElevatorVelocity)
        Me.GroupBox4.Controls.Add(Me.Label23)
        Me.GroupBox4.Controls.Add(Me.NumElevators)
        Me.GroupBox4.Controls.Add(Me.Label21)
        Me.GroupBox4.Controls.Add(Me.Label22)
        Me.GroupBox4.Location = New System.Drawing.Point(508, 118)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(494, 199)
        Me.GroupBox4.TabIndex = 45
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Elevators"
        '
        'ElevatorCustomCapacity
        '
        Me.ElevatorCustomCapacity.Location = New System.Drawing.Point(331, 65)
        Me.ElevatorCustomCapacity.Name = "ElevatorCustomCapacity"
        Me.ElevatorCustomCapacity.Size = New System.Drawing.Size(100, 20)
        Me.ElevatorCustomCapacity.TabIndex = 44
        '
        'ElevatorCapacity
        '
        Me.ElevatorCapacity.FormattingEnabled = True
        Me.ElevatorCapacity.Items.AddRange(New Object() {"900 kg (12 people)", "1200 kg (16 people)", "1400 kg (19 people)", "1600 kg (21 people)", "1800 kg (24 people)", "Custom (in kg)"})
        Me.ElevatorCapacity.Location = New System.Drawing.Point(225, 65)
        Me.ElevatorCapacity.Name = "ElevatorCapacity"
        Me.ElevatorCapacity.Size = New System.Drawing.Size(100, 21)
        Me.ElevatorCapacity.TabIndex = 43
        '
        'Label26
        '
        Me.Label26.AutoSize = True
        Me.Label26.Location = New System.Drawing.Point(162, 95)
        Me.Label26.Name = "Label26"
        Me.Label26.Size = New System.Drawing.Size(57, 13)
        Me.Label26.TabIndex = 42
        Me.Label26.Text = "Door Type"
        '
        'DoorType
        '
        Me.DoorType.FormattingEnabled = True
        Me.DoorType.Items.AddRange(New Object() {"Single-Slide, 0.91 m (36 in)", "Two-Speed, 0.91 m (36 in)", "Center-Opening, 0.91 m (36 in)", "Single-Slide, 1.07 m (42 in)", "Two-Speed, 1.07 m (42 in)", "Center-Opening, 1.07 m (42 in)", "Two-Speed, 1.22 m (48 in)", "Center-Opening, 1.22 m (48 in)", "Two-Speed, 1.37 m (54 in)", "Center-Opening, 1.37 m (54 in)", "Two-Speed, 1.52 m (60 in)", "Center-Opening, 1.52 m (60 in)"})
        Me.DoorType.Location = New System.Drawing.Point(225, 92)
        Me.DoorType.Name = "DoorType"
        Me.DoorType.Size = New System.Drawing.Size(195, 21)
        Me.DoorType.TabIndex = 41
        '
        'ElevatorStartup
        '
        Me.ElevatorStartup.Location = New System.Drawing.Point(225, 171)
        Me.ElevatorStartup.Name = "ElevatorStartup"
        Me.ElevatorStartup.Size = New System.Drawing.Size(100, 20)
        Me.ElevatorStartup.TabIndex = 40
        '
        'Label25
        '
        Me.Label25.AutoSize = True
        Me.Label25.Location = New System.Drawing.Point(96, 174)
        Me.Label25.Name = "Label25"
        Me.Label25.Size = New System.Drawing.Size(123, 13)
        Me.Label25.TabIndex = 39
        Me.Label25.Text = "Elevator Startup Time (s)"
        '
        'ElevatorAcceleration
        '
        Me.ElevatorAcceleration.Location = New System.Drawing.Point(225, 145)
        Me.ElevatorAcceleration.Name = "ElevatorAcceleration"
        Me.ElevatorAcceleration.Size = New System.Drawing.Size(100, 20)
        Me.ElevatorAcceleration.TabIndex = 38
        '
        'Label24
        '
        Me.Label24.AutoSize = True
        Me.Label24.Location = New System.Drawing.Point(81, 148)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(138, 13)
        Me.Label24.TabIndex = 37
        Me.Label24.Text = "Elevator Acceleration (m/s²)"
        '
        'ElevatorVelocity
        '
        Me.ElevatorVelocity.Location = New System.Drawing.Point(225, 119)
        Me.ElevatorVelocity.Name = "ElevatorVelocity"
        Me.ElevatorVelocity.Size = New System.Drawing.Size(100, 20)
        Me.ElevatorVelocity.TabIndex = 36
        '
        'Label23
        '
        Me.Label23.AutoSize = True
        Me.Label23.Location = New System.Drawing.Point(63, 122)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(156, 13)
        Me.Label23.TabIndex = 35
        Me.Label23.Text = "Normal Operating Velocity (m/s)"
        '
        'NumElevators
        '
        Me.NumElevators.Location = New System.Drawing.Point(225, 36)
        Me.NumElevators.Name = "NumElevators"
        Me.NumElevators.Size = New System.Drawing.Size(100, 20)
        Me.NumElevators.TabIndex = 33
        '
        'Label21
        '
        Me.Label21.AutoSize = True
        Me.Label21.Location = New System.Drawing.Point(71, 67)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(148, 13)
        Me.Label21.TabIndex = 32
        Me.Label21.Text = "Elevator Capacity (kg/people)"
        Me.Label21.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label22
        '
        Me.Label22.AutoSize = True
        Me.Label22.Location = New System.Drawing.Point(116, 39)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(103, 13)
        Me.Label22.TabIndex = 31
        Me.Label22.Text = "Number of Elevators"
        '
        'EndEstimateTime
        '
        Me.EndEstimateTime.Location = New System.Drawing.Point(691, 386)
        Me.EndEstimateTime.Name = "EndEstimateTime"
        Me.EndEstimateTime.Size = New System.Drawing.Size(100, 20)
        Me.EndEstimateTime.TabIndex = 46
        Me.EndEstimateTime.Visible = False
        '
        'Batchmode
        '
        Me.Batchmode.Location = New System.Drawing.Point(874, 412)
        Me.Batchmode.Name = "Batchmode"
        Me.Batchmode.Size = New System.Drawing.Size(36, 23)
        Me.Batchmode.TabIndex = 47
        Me.Batchmode.Text = "Start estimating in batch mode"
        Me.Batchmode.UseVisualStyleBackColor = True
        '
        'BatchRunCounter
        '
        Me.BatchRunCounter.AutoSize = True
        Me.BatchRunCounter.Location = New System.Drawing.Point(916, 417)
        Me.BatchRunCounter.Name = "BatchRunCounter"
        Me.BatchRunCounter.Size = New System.Drawing.Size(33, 13)
        Me.BatchRunCounter.TabIndex = 48
        Me.BatchRunCounter.Text = "Run: "
        '
        'frmEstimator
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1014, 463)
        Me.Controls.Add(Me.BatchRunCounter)
        Me.Controls.Add(Me.Batchmode)
        Me.Controls.Add(Me.EndEstimateTime)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.NumberEvacuated)
        Me.Controls.Add(Me.btnDebug)
        Me.Controls.Add(Me.EgressTime)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.EndEstimate)
        Me.Controls.Add(Me.Progress)
        Me.Controls.Add(Me.Estimate)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.HelpButton = True
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmEstimator"
        Me.Text = "Egress Estimator"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub


    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents FirstFloorUseStairwells As System.Windows.Forms.CheckBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Estimate As System.Windows.Forms.Button
    Friend WithEvents Progress As System.Windows.Forms.ProgressBar
    Friend WithEvents EndEstimate As System.Windows.Forms.ComboBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents SaveFileDialog As System.Windows.Forms.SaveFileDialog
    Friend WithEvents HallLength As System.Windows.Forms.TextBox
    Friend WithEvents NumOccupants As System.Windows.Forms.TextBox
    Friend WithEvents NumFloors As System.Windows.Forms.TextBox
    Friend WithEvents FlightsPerFloor As System.Windows.Forms.TextBox
    Friend WithEvents StairWidth As System.Windows.Forms.TextBox
    Friend WithEvents HallWidth As System.Windows.Forms.TextBox
    Friend WithEvents StairEntryRate As System.Windows.Forms.TextBox
    Friend WithEvents StairTreadDepth As System.Windows.Forms.TextBox
    Friend WithEvents StairRiserHeight As System.Windows.Forms.TextBox
    Friend WithEvents StairsPerFlight As System.Windows.Forms.TextBox
    Friend WithEvents ExitHallEntryRate As System.Windows.Forms.TextBox
    Friend WithEvents HallEntryRate As System.Windows.Forms.TextBox
    Friend WithEvents timerEstimate As System.Windows.Forms.Timer
    Friend WithEvents btnDebug As System.Windows.Forms.Button
    Friend WithEvents EgressTime As System.Windows.Forms.Label
    Friend WithEvents NumberEvacuated As System.Windows.Forms.Label
    Friend WithEvents ElevatorFraction As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents NumStairwells As System.Windows.Forms.TextBox
    Friend WithEvents NumElevators As System.Windows.Forms.TextBox
    Friend WithEvents Label21 As System.Windows.Forms.Label
    Friend WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents ElevatorVelocity As System.Windows.Forms.TextBox
    Friend WithEvents Label23 As System.Windows.Forms.Label
    Friend WithEvents Label26 As System.Windows.Forms.Label
    Friend WithEvents DoorType As System.Windows.Forms.ComboBox
    Friend WithEvents ElevatorStartup As System.Windows.Forms.TextBox
    Friend WithEvents Label25 As System.Windows.Forms.Label
    Friend WithEvents ElevatorAcceleration As System.Windows.Forms.TextBox
    Friend WithEvents Label24 As System.Windows.Forms.Label
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents StairHallExitRate As System.Windows.Forms.TextBox
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents StairHallWidth As System.Windows.Forms.TextBox
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents StairHallLength As System.Windows.Forms.TextBox
    Friend WithEvents ElevatorCapacity As System.Windows.Forms.ComboBox
    Friend WithEvents EndEstimateTime As System.Windows.Forms.TextBox
    Friend WithEvents ElevatorCustomCapacity As System.Windows.Forms.TextBox
    Friend WithEvents BuildingFile As System.Windows.Forms.CheckBox
    Friend WithEvents Batchmode As System.Windows.Forms.Button
    Friend WithEvents BatchRunCounter As System.Windows.Forms.Label


End Class

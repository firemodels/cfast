
    ! --------------------------- cparams -------------------------------------------

    module cparams
    
    use precision_parameters

    integer, parameter :: lbufln=1024           ! default line length for all inputs

    ! geometry parameters
    integer, parameter :: mxrooms = 101         ! maximum number of compartments
    integer, parameter :: mxslb = 6             ! maximum number of slabs in a surface material 
                                                ! (at the moment, the gui only supports 1)
    integer, parameter :: nwal = 4              ! number of compartment surfaces (ceiling, upper walls, lower walls, floor)
    integer, parameter :: mxwal = mxrooms*nwal  ! maximum total number of compartment surfaces
    integer, parameter :: nnodes = 61           ! number of nodes in a material for conduction calculation (should be odd number)
    integer, parameter :: mxslice = 5*mxrooms   ! maximum number of slices and isosurfaces in an input file
    integer, parameter :: mxitems = 25*mxrooms  ! maximum number of vents, targets, etc.
    integer, parameter :: mxss = 32767          ! maximum number of spreadsheet columns for output

    ! fire related input parameters
    integer, parameter :: mxpts = 3600               ! maximum number of data points in a input curve/ramp
    integer, parameter :: ns = 23                   ! number of species
    integer, parameter :: ns_mass = 9               ! number of species that count toward total mass
    integer, parameter :: mxfires = mxitems         ! maximum number of fires
    integer, parameter :: mxtabls = mxfires         ! maximum number of table inputs, currently only used for fires
    integer, parameter :: mxtablcols = ns+2         ! maximum columns in a table of fire inputs
    real(eb), parameter :: o2f = 1.31e7_eb          ! HRR by oxygen consumption constant in MJ/kg
    real(eb), parameter :: t_max = 900._eb          ! maximum flame temperature

    integer, parameter :: mxmatl = mxitems         ! maximum number of thermal properties
    integer, parameter :: mxthrmplen = 16           ! maximum length for thermal property short names

    integer, parameter :: trigger_by_time = 1       ! indicies for fire ignition type (also used by vents)
    integer, parameter :: trigger_by_temp = 2
    integer, parameter :: trigger_by_flux = 3
    integer, parameter :: flaming = 1               ! used for specific extinction coefficients
    integer, parameter :: smoldering = 2    

    ! ventilation parameters
    integer, parameter :: mxhvents = mxitems        ! maximum number of horizontal flow vents
    integer, parameter :: mxfslab = 10              ! maximum number of slabs in a horizontal flow calculation
    
    integer, parameter :: mxleaks = mxitems         ! maximum numer of automatically-generated leakage vents

    integer, parameter :: mxvvents=mxitems          ! maximum number of vertical flow vents

    integer, parameter :: mxmvents=mxitems          ! maximum number of mechanical ventilation systems
    integer, parameter :: mxfan = mxmvents/2        ! maximum number of fans in a mechanical ventilation system
    integer, parameter :: mxcoeff = 1               ! maximum order of fan curve (as a polynomial). at the moment,
                                                    !   the gui limits to constant flow
    integer, parameter :: mxcon = 3                 ! maximum number of connections to a node in a mechanical ventilation system
    integer, parameter :: mxduct = mxfan+2          ! maximum number of ducts in a mechanical ventilation system
    integer, parameter :: mxnode = 2*mxduct         ! maximum number of nodes in a mechanical ventilation system
    integer, parameter :: mxext = mxitems           ! maximum number of external connections in a mechanical ventilation system
    integer, parameter :: mxbranch = mxfan+mxduct   ! maximum number of branches in a mechanical ventilation system

    integer, parameter :: mxramps = mxitems         ! maximum number of possible time-based ramps
    integer, parameter :: mxdiscon = mxitems        ! maximum number of DASSL discontinuities
    integer, parameter :: initial_time = 1          ! indicies for simple vent opening data
    integer, parameter :: initial_fraction = 2
    integer, parameter :: final_time = 3
    integer, parameter :: final_fraction = 4
    integer, parameter :: face_front = 1            ! indicates wall face where a wall vent is located
    integer, parameter :: face_right = 2
    integer, parameter :: face_back = 3
    integer, parameter :: face_left = 4

    ! room related parameters
    real(eb), parameter :: cjetvelocitymin = 0.1_eb ! default minimum ceiling jet velocity
    real(eb), parameter :: vminfrac = 1.0e-4_eb     ! minimum layer volume as a fraction of room volume
    integer, parameter :: radiation_fix = 1         ! adjust radiation calculation when layer reaches minimum volume 

    real(eb), parameter :: mx_vsep=0.01_eb          ! maximum vertical distance between elements before they are considered
                                                    ! separate elements (connected compartments for example)
    real(eb), parameter :: mx_hsep = 1.0e-3_eb      ! maximum horizontal distance below which fire is assumed to
                                                    ! be on a surface for entrainmnt
    real(eb), parameter :: xlrg = 1.0e+5_eb         ! sizes for outside room
    real(eb), parameter :: deltatemp_min = 1.0_eb   ! minimum temperature difference for bouyancy to deposit all into a layer
    integer, parameter :: interior = 1              ! compartment interior
    integer, parameter :: exterior = 2              ! compartment exterior
    integer, parameter :: default_grid = 50         ! number of grid cells in each direction at initialization

    ! target parameters
    integer, parameter :: mxtarg = mxitems                  ! maximum number of targets
    integer, parameter :: nnodes_trg = nnodes-1             ! number of interior nodes in a target for conduction calculation
    integer, parameter :: idx_tempf_trg = 1                 ! position of front temperature of target (front surface temperature)
    integer, parameter :: idx_tempb_trg = idx_tempf_trg+nnodes_trg-1 ! position of back temperature of target
    ! (back surface temperature)
    integer, parameter :: mxr_trg = idx_tempb_trg           ! upper bound of real target array
    integer, parameter :: mxi_trg = 7                       ! upper bound of integer target array

    integer, parameter :: mxdtect=mxitems                   ! maximum number of detectors

    integer, parameter :: check_state = 0                   ! index to check state of detectors and targets
    integer, parameter :: set_state = 1                     ! index to calculate full state of detectors and targets
    integer, parameter :: update_state = 2                  ! index to update state of detectors and targets on
    ! successful equation set solution

    integer, parameter :: pde = 1                           ! plate targets (cartesian coordinates)
    integer, parameter :: cylpde = 2                        ! cylindrical targets (cylindrical coordinates)

    ! parameters for equation solver
    ! nt = 4*mxrooms(main equ) + 2*mxrooms*ns(species)
    integer, parameter :: nt = 12*mxrooms + 2*mxrooms*ns ! total number of main equations for dae solver
    integer, parameter :: maxjeq = 6*mxrooms + mxnode + mxbranch
    integer, parameter :: maxeq = maxjeq + nwal*mxrooms
    integer, parameter :: maxteq = maxeq+2*mxrooms*ns

    ! define indices for flow arrays
    integer, parameter :: l = 2     ! lower layer
    integer, parameter :: u = 1     ! upper layer
    integer, parameter :: m = 1     ! mass
    integer, parameter :: q = 2     ! energy
    integer, parameter :: pp = 3    ! beginning of species
    integer, parameter :: in = 1    ! flow into compartment
    integer, parameter :: out = 2   ! flow out of compartment

    ! define indicies for species arrays
    integer, parameter :: n2 = 1
    integer, parameter :: o2 = 2
    integer, parameter :: co2 = 3
    integer, parameter :: co = 4
    integer, parameter :: hcn = 5
    integer, parameter :: hcl = 6
    integer, parameter :: fuel = 7
    integer, parameter :: h2o = 8
    integer, parameter :: soot = 9
    integer, parameter :: soot_flaming = 10
    integer, parameter :: soot_smolder = 11
    integer, parameter :: ct = 12
    integer, parameter :: ts = 13
    integer, parameter :: fuel_moles = 14
    integer, parameter :: fuel_Q = 15
    integer, parameter :: fuel_n2 = 16
    integer, parameter :: fuel_o2 = 17
    integer, parameter :: fuel_co2 = 18
    integer, parameter :: fuel_co = 19
    integer, parameter :: fuel_hcn = 20
    integer, parameter :: fuel_hcl = 21
    integer, parameter :: fuel_h2o = 22
    integer, parameter :: fuel_soot = 23

    ! detector types
    integer, parameter :: smoked = 1    ! smoke detector
    integer, parameter :: heatd = 2     ! heat detector
    integer, parameter :: sprinkd = 3   ! sprinkler
    
    ! wall pointers
    integer, parameter :: w_from_room = 1
    integer, parameter :: w_from_wall = 2
    integer, parameter :: w_to_room = 3
    integer, parameter :: w_to_wall = 4
    integer, parameter :: w_boundary_condition = 5
    
    ! Post-run calculation parameters
    integer, parameter :: mx_dumps = mxitems  ! Maximum number of columns that will be generated

    end module cparams

    ! --------------------------- defaults -------------------------------------------

    module defaults
    
    use precision_parameters

    integer, parameter :: default_version = 0

    ! time-related default values
    real(eb), parameter :: default_simulation_time = 900._eb
    real(eb), parameter :: default_print_out_interval = 60._eb
    real(eb), parameter :: default_smv_out_interval = 15._eb
    real(eb), parameter :: default_ss_out_interval = 15._eb
    real(eb), parameter :: default_stpmax = 1._eb

    ! ambient condition default values
    real(eb), parameter :: default_temperature = 293.15_eb
    real(eb), parameter :: default_pressure = 101325._eb
    real(eb), parameter :: default_relative_humidity = 0.5_eb
    
    ! fire-related default values
    real(eb), parameter :: default_lower_oxygen_limit = 0.15_eb
    real(eb), parameter :: default_radiative_fraction = 0.35_eb
    real(eb), dimension(2), parameter :: default_sigma_s = (/8700._eb, 4400._eb/) ! mulholland, F&M 24, 227(2000)
    
    ! sprinkler/detector defaults
    real(eb), parameter :: default_rti = 50._eb
    real(eb), parameter :: default_activation_temperature = (135._eb - 32._eb)/1.8_eb
    real(eb), parameter :: default_activation_obscuration = 100._eb*(1._eb-(1._eb-8._eb/100._eb)**(1._eb/0.3048_eb))
    
    ! vents defaults
    real(eb), parameter :: default_min_cutoff_relp = 200._eb, default_max_cutoff_relp = 300._eb

    end module defaults
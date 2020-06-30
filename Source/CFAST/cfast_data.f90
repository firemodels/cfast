
! --------------------------- cenviro -------------------------------------------

module cenviro

    use precision_parameters
    
    implicit none
    save

    integer, parameter :: constvar = 1 ,odevara = 2 ,odevarb = 4, odevarc = 8
    
    real(eb), parameter :: cp = 1012.0_eb
    real(eb), parameter :: gamma = 1.40_eb
    real(eb), parameter :: rgas = (gamma-1.0_eb)/gamma*cp

end module cenviro

! --------------------------- devc_data -------------------------------------------

module devc_data

    use precision_parameters

    use  cfast_types, only: target_type, detector_type
    
    use cparams, only: mxthrmplen, mxtarg, mxdtect
    
    implicit none
    save

    ! variables for calculation of flux to a target

    integer :: idset    ! compartment where detector just went off. more than one
                        ! sprinkler in a compartment is meaningless to CFAST

    integer :: n_targets                                                        ! number of detectors in the simulation
    type (target_type), allocatable, dimension(:), target  :: targetinfo        ! structured target data

    integer :: n_detectors                                                      ! number of detectors in the simulation
    type (detector_type), allocatable, dimension(:), target  :: detectorinfo    ! structured detector data
    
    logical :: alloc_devc = .true., init_devc = .true.

end module devc_data

! --------------------------- diag_data -------------------------------------------

module  diag_data

    use precision_parameters
    
    implicit none
    save

    logical :: residprn, jacprn
    logical :: residfirst = .true.
    logical :: nwline=.true.
    logical :: prnslab
    integer :: ioresid, ioslab
    real(eb) ::   dbtime
    character(len=256) :: residfile, residcsv, slabcsv
    
    ! Verification flag
    logical :: radi_verification_flag = .false.
    real(eb) :: verification_time_step = 0._eb
    ! Diagnostic variables for radiative properties
    real(eb) :: partial_pressure_h2o, partial_pressure_co2, gas_temperature
    ! Diagnostic veriables for radiation solver
    character(len=64) :: rad_solver
    logical :: radi_radnnet_flag = .false.
    ! Diagnostic variables for adiabatic target surface temperature
    logical :: verification_ast=.false.
    real(eb) :: radiative_incident_flux_AST = 0._eb
    ! Diagnostic variable for surface opening fraction
    real(eb) :: upper_layer_thickness = 0._eb
    ! Diagnostic variable for fire to target radiative heat flux
    real(eb) :: verification_fire_heat_flux = 0._eb

end module diag_data

! -------------------------dump_data---------------------------------------
    
module dump_data
    
    use precision_parameters
    
    use cfast_types, only: dump_type
    
    use cparams, only: mx_dumps, mxitems
    
    implicit none
    save
    
    integer, parameter :: num_csvfiles = 5 
    integer, parameter :: iocsv_compartments = 1, iocsv_devices = 2, iocsv_masses = 3, iocsv_vents = 4, iocsv_walls = 5
    
    character(len=24), parameter, dimension(num_csvfiles) :: csvnames = &
        (/'COMPARTMENTS', 'DEVICES     ', 'MASSES      ', 'VENTS       ', 'WALLS       '/)
    integer, dimension(num_csvfiles) :: iocsv
    
    integer :: n_dumps
    type (dump_type), allocatable, dimension(:), target :: dumpinfo
    
    logical :: alloc_dump = .true., init_dump = .true.
    
    end module dump_data

! --------------------------- fire_data -------------------------------------------

module fire_data

    use precision_parameters
    
    use cfast_types, only: fire_type, table_type
    
    use cparams, only: mxpts
    use defaults, only: default_sigma_s
    
    implicit none
    save

    ! fire variables

    real(eb) :: tgignt                                  ! gaseous ignition temperature for burning in upper layer and door jets
    real(eb) :: lower_o2_limit                          ! minimum oxygen level for combustion
    real(eb) :: summed_total_trace                      ! total trace species released by all fires
    real(eb), dimension(2) :: sigma_s = default_sigma_s ! extinction coefficient for flaming and smoldering smoke

    integer :: n_fires                                  ! number of fires in the current simulation
    type(fire_type), allocatable, dimension(:), target :: fireinfo
    
    integer :: n_tabls                                  ! number of tables of fire data in the current simulation
    type(table_type), allocatable, dimension(:), target :: tablinfo

    integer :: n_furn                                   ! number of data points in furnace temperature curve
    real(eb), dimension(mxpts) :: furn_time, furn_temp  ! time and furnace temperature as a function of time
    real(eb) :: qfurnout                                ! just sigma * furn_temp(t)^4

    logical :: alloc_fire = .true., init_fire = .true. 

    end module fire_data

! --------------------------- material_data -------------------------------------------

module material_data

    use precision_parameters

    use cfast_types, only: material_type
    
    use cparams, only : mxmatl
    
    implicit none
    save

    integer n_matl                                                              ! number of thermal properties in the simulation
    type (material_type), allocatable, dimension(:), target  :: material_info   ! structured thermal property data
    
    logical :: alloc_matl = .true., init_matl = .true. 

    end module material_data
    
! --------------------------- namelist_data -------------------------------------------

    module namelist_data

    use precision_parameters

    implicit none
    save
    
    logical :: convert_negative_distances = .true.  ! true to convert negative vent, fire, and target locations
                                                    ! to distance from compartment origin
    logical :: nmlflag = .true.                     ! true if input file is in namelist format
    integer :: input_file_line_number               ! current line read in a namelist-format input file
    logical :: compflag = .false.                     ! true if each namelist type has been read in
    logical :: connflag = .false.
    logical :: devcflag = .false.
    logical :: tablflag = .false.
    logical :: insfflag = .false.
    logical :: fireflag = .false.
    logical :: headflag = .false.
    logical :: initflag = .false.
    logical :: isofflag = .false.
    logical :: matlflag = .false.
    logical :: miscflag = .false.
    logical :: rampflag = .false.
    logical :: slcfflag = .false.
    logical :: timeflag = .false.
    logical :: ventflag = .false. 
    logical :: diagflag = .false.
    logical :: dumpflag = .false.

    end module namelist_data
    
! --------------------------- option_data -------------------------------------------

module option_data

    use precision_parameters
    
    implicit none
    save

    integer, parameter :: mxopt = 18

    integer, parameter :: off = 0
    integer, parameter :: on = 1

    integer, parameter :: ffire = 1
    integer, parameter :: fhflow = 2
    integer, parameter :: fentrain = 3
    integer, parameter :: fvflow = 4
    integer, parameter :: fcjet = 5
    integer, parameter :: fdfire = 6
    integer, parameter :: fconvec = 7
    integer, parameter :: frad = 8
    integer, parameter :: fconduc = 9
    integer, parameter :: fdebug = 10
    integer, parameter :: fgasabsorb = 11
    integer, parameter :: fmflow = 12
    integer, parameter :: fkeyeval = 13
    integer, parameter :: fpsteady = 14
    integer, parameter :: fpdassl = 15
    integer, parameter :: foxygen = 16
    integer, parameter :: fresidprn = 17
    integer, parameter :: flayermixing = 18

    integer, dimension(mxopt) :: option = &
        !   fire,       hflow,      entrain,    vflow,      cjet
        (/  on,          on,          on,        on,         on,  &
        !   door-fire,  convec,     rad,        conduct,    debug
            on,         on,          on,          on,          off,  &
        !   gas absorb,          h_mflow,   keyboard,   steady state initialization,    dassl debug
        !   on means calculate
        !   gas absorb for layer
        !   off means use constant
        !   value
            on,                   on,          on,          off,                           off,  &
        !   oxygen dassl solve    Residual print   convection between layers
            off,                  off,                  on  /)

    real(eb) :: cutjac, stptime, prttime, tottime, ovtime, tovtime

    integer :: iprtalg = 0, jacchk = 0
    integer :: numjac = 0, numstep = 0, numresd = 0, numitr = 0, totjac = 0, totstep = 0, totresd = 0, totitr = 0, total_steps = 0

      end module option_data

! --------------------------- ramp_data -------------------------------------------

module ramp_data

    use precision_parameters
    
    use cfast_types, only: ramp_type
    
    use cparams, only: mxramps
    
    implicit none
    save

    ! ramping variables
    integer :: n_ramps = 0
    type(ramp_type), target :: rampinfo(mxramps)

end module ramp_data

! --------------------------- room_data -------------------------------------------

module room_data

    use precision_parameters
    
    use cfast_types, only: room_type
    
    use cparams, only: nnodes, ns, mxwal
    
    implicit none
    save

    ! compartment variables

    real(eb) :: relative_humidity, interior_abs_pressure, exterior_abs_pressure, pressure_offset, pressure_ref, t_ref, &
        initial_mass_fraction(ns), interior_rho, exterior_rho, interior_ambient_temperature, exterior_ambient_temperature
    
    integer :: n_rooms
    type(room_type), allocatable, dimension(:), target :: roominfo
    
    logical :: alloc_room = .true., init_room = .true. 

    ! wall variables
    integer :: nwpts = (nnodes-1)/2                                     ! number of wall nodes 
    integer :: iwbound = 3                                              ! boundary condition type 
                                                                        !   1 = constant exterior surface temperature, 
                                                                        !   2 = insulated exterior surface, 
                                                                        !   3 = radiates to ambient
    real(eb), dimension(3) :: slab_splits = (/0.50_eb, 0.17_eb, 0.33_eb/)    ! computed values for slab thickness, 
                                                                        ! initial fractions for inner, middle and outer wall slab
    
    integer :: n_cons
    integer, dimension(mxwal,5) :: surface_connections  ! defines all surfaces for conduction, 1 entry for each wall that's on
                                                        !   1 = from room number
                                                        !   2 = from wall number (ceiling, upper walls, lower walls, floor)
                                                        !   3 = to room number
                                                        !   4 = to wall number
                                                        !   5 = boundary condition type for exterior surface
    integer :: n_vcons
    integer, dimension(mxwal,4) :: vertical_connections ! list of connected compartments for vertical heat transfer
                                                        !   1 = from room number
                                                        !   2 = from wall number (ceiling, upper walls, lower walls, floor)
                                                        !   3 = to room number
                                                        !   4 = to wall number

    logical :: adiabatic_walls  ! true if all surfaces are adiabatic

end module room_data

! --------------------------- setup_data -------------------------------------------

module setup_data

    use precision_parameters
    
    implicit none
    save
    
    integer :: i_time_end, i_time_step
    real(eb) :: ss_out_interval = 0, print_out_interval = 0, smv_out_interval = 0, time_end
    real(eb) :: stime, deltat

    character(len=128) :: title

    logical :: nokbd=.false., initializeonly=.false., overwrite_testcase=.true.
    logical :: debugging = .false., validation_flag = .false., netheatflux = .false.
    integer :: cfast_version, outputformat = 0
    integer, dimension(3) :: rundat
    character(len=60) :: nnfile = " ", datafile
    integer :: cfast_input_file_position = 2
    logical :: init_scalars = .true.  
    
    !File descriptors for cfast
    integer :: iofili, iofill, iofilg, iofilo, iofilstat, iofilsmv, iofilsmvplt, iofilsmvzone, &
        iofilssdiag, iofilcalc, iofilssc, iofilssd, iofilssw, iofilssm, iofilssv
    character(len=6), parameter :: heading = "VERSN"
    character(len=64) :: project, extension
    character(len=256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, smvsinfo, sscompartment, ssdevice, &
        sswall, ssmasses, ssvent, ssdiag, gitfile, errorlogging, stopfile, queryfile, statusfile, sscalculation

    ! Work arrays for the csv input routines
    integer, parameter :: nrow = 10000, ncol = 100
    real(eb) :: rarray(nrow,ncol)
    character(len=128) :: carray(nrow,ncol)

end module setup_data

! --------------------------- smkview_data -------------------------------------------

module smkview_data

    use precision_parameters
    
    use cfast_types, only: iso_type, slice_type, visual_type
    
    use cparams, only: mxfires, mxrooms, mxslice
    
    implicit none
    save

    integer, dimension (mxfires) :: smv_room
    real(eb), dimension(mxfires) :: smv_qdot, smv_zfire, smv_xfire, smv_yfire, smv_height
    real(eb), dimension(mxrooms) :: smv_relp,smv_zlay,smv_tl,smv_tu
    
    ! visualization variables
    integer :: n_visual = 0
    type(visual_type), dimension (mxslice), target :: visualinfo

    integer :: n_slice = 0
    type(slice_type), allocatable, dimension(:), target :: sliceinfo

    integer :: n_iso = 0
    type(iso_type), allocatable, dimension(:), target :: isoinfo

end module smkview_data

! --------------------------- solver_data -------------------------------------------

module solver_data

    use precision_parameters
    
    use cparams, only: nt, maxteq, ns, mxrooms, mxdiscon
    use defaults, only: default_stpmax  
    
    implicit none
    save
    ! default solver tolerences
    real(eb) :: aptol = 1.0e-6_eb               ! absolute pressure tolerance
    real(eb) :: rptol = 1.0e-6_eb               ! relative pressure tolerance
    real(eb) :: atol = 1.0e-5_eb                ! absolute other tolerance
    real(eb) :: rtol = 1.0e-5_eb                ! relative other tolerance
    real(eb) :: awtol = 1.0e-2_eb               ! absolute wall tolerance
    real(eb) :: rwtol = 1.0e-2_eb               ! relative wall tolerance
    real(eb) :: algtol = 1.0e-8_eb              ! initialization tolerance
    real(eb) :: ahvptol = 1.0e-6_eb             ! absolute HVAC pressure tolerance
    real(eb) :: rhvptol = 1.0e-6_eb             ! relative HVAC pressure tolerance
    real(eb) :: ahvttol = 1.0e-5_eb             ! absolute HVAC temperature tolerance
    real(eb) :: rhvttol = 1.0e-5_eb             ! relative HVAC temperature tolerance

    real(eb), dimension(nt) :: pinit
    real(eb), dimension(1) :: rpar2
    integer, dimension(3) :: ipar2
    
    ! time step setup values
    real(eb) :: stpmax = default_stpmax         ! maximum solver time step, if negative, then solver will decide
    real(eb) :: stpfirst = 0.005_eb             ! first time step for DASSL
    logical :: stpminflag                       ! true if CFAST should check for too small time steps
    real(eb) :: stpmin                          ! minimum time step below which DASSL may be failing to find a solution.
    integer :: stpmin_cnt                       ! current count of time steps below stpmin
    integer :: stpmin_cnt_max                   ! maximum number of time steps below stpmin before DASSL calls it quits
    
    ! solver variables
    integer :: nofp, noftu, nofvu, noftl, nofoxyl, nofoxyu, nofwt, nofprd, nofhvpr, nequals
    real(eb), dimension(maxteq) :: p, pold, pdold
    real(eb) :: told, dt

    integer, dimension(ns+2) :: i_speciesmap    ! maps species to corresponding DASSL equations
    integer, dimension(mxrooms,4) :: i_wallmap  ! maps wall surface temperatures to corresponding DASSL equations
    
    integer :: jaccol
    integer :: jacdim
                             
    integer :: ndisc                            ! number of discontinuities fed to DASSL
    real(eb), dimension(0:mxdiscon) :: discon    ! list of discontinuities fed to DASSL to ease solution

end module solver_data

! --------------------------- smkview_data -------------------------------------------

module spreadsheet_output_data
    
    use precision_parameters
    use cfast_types, only: ssout_type
    use cparams, only: mxss
    
    integer :: n_sscomp = 0
    type(ssout_type), allocatable, dimension(:), target :: sscompinfo
    
    integer :: n_ssdevice = 0
    type(ssout_type), allocatable, dimension(:), target :: ssdeviceinfo
    
    integer :: n_sswall = 0
    type(ssout_type), allocatable, dimension(:), target :: sswallinfo
    
    integer :: n_ssmass = 0
    type(ssout_type), allocatable, dimension(:), target :: ssmassinfo
    
    integer :: n_ssvent = 0
    type(ssout_type), allocatable, dimension (:), target :: ssventinfo
    
    real(eb) :: outarray(mxss)
    
    logical :: alloc_ss = .true., init_ss = .true.
    
end module spreadsheet_output_data

! --------------------------- vent_data -------------------------------------------

module vent_data

    use precision_parameters

    use cfast_types, only: vent_type
    
    use cparams, only: mxhvents, mxfslab
    
    implicit none
    save

    ! hvent variables
    integer :: n_hvents                                                 ! number of horizontal vents
    type (vent_type), allocatable, dimension(:), target  :: hventinfo   ! structured horizontal vent data
    
    real(eb), dimension(2,mxhvents) :: vss, vsa, vas, vaa, vsas, vasa   ! individual flows for door jet fires (u or l)
    
    ! horizontal vent flow slab data by elevation in vent
    integer :: nvelev                                                   ! current number of slabs
    real(eb), dimension(mxfslab) :: yvelev                              ! elevations of breakpoints in vent flow
    real(eb), dimension(mxfslab) :: dpv1m2                              ! pressure in room 1 - pressure in room 2 in each slab
    integer, dimension(mxfslab) ::  dirs12                              ! direction of flow in each slab

    ! vvent variables
    integer :: n_vvents                                                 ! number of vertical flow vents
    type (vent_type), allocatable, dimension(:), target :: vventinfo    ! structured vertical vent data

    ! hvac variables
    integer :: n_mvents                                                 ! number of mechanical flow vents
    type(vent_type), allocatable, dimension(:), target :: mventinfo
    
    ! leakage variables
    integer :: n_leaks                                                  ! number of automatically-generated leakage vents
    type(vent_type), allocatable, dimension(:), target :: leakinfo
    
    logical :: alloc_vent = .true., init_vent = .true.

    end module vent_data
    

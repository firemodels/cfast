
! --------------------------- cenviro -------------------------------------------

module cenviro

    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    integer, parameter :: constvar = 1 ,odevara = 2 ,odevarb = 4, odevarc = 8
    
    real(eb), parameter :: cp = 1012.0_eb
    real(eb), parameter :: gamma = 1.40_eb
    real(eb), parameter :: rgas = (gamma-1.0_eb)/gamma*cp

end module cenviro

! --------------------------- diag_data -------------------------------------------

module  diag_data

    use precision_parameters
    implicit none

    logical :: residprn, jacprn
    logical :: residfirst = .true.
    logical :: nwline=.true.
    logical :: prnslab
    integer :: ioresid, ioslab
    real(eb) ::   dbtime
    character(256) :: residfile, residcsv, slabcsv
    
    ! Diagnosis variables
    real(eb) :: partial_pressure_h2o, partial_pressure_co2, gas_temperature
    character(64) :: rad_solver
    logical :: radi_verification_flag=.false., radi_radnnet_flag=.false.

end module diag_data

! --------------------------- fire_data -------------------------------------------

module fire_data

    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    ! fire variables

    real(eb) :: tgignt                              ! gaseous ignition temperature for burning in upper layer and door jets
    real(eb) :: lower_o2_limit                      ! minimum oxygen level for combustion
    real(eb) :: tradio     

    integer :: n_fires                              ! number of fires in the current simulation
    type(fire_type), target :: fireinfo(mxfires)
    
    integer :: n_tabls                              ! number of tables of fire data in the current simulation
    type(table_type), target :: tablinfo(mxfires)

    integer :: n_furn                                    ! number of data points in furnace temperature curve
    real(eb), dimension(mxpts) :: furn_time, furn_temp  ! time and furnace temperature as a function of time
    real(eb) :: qfurnout                                ! just sigma * furn_temp(t)^4

    end module fire_data
    
! --------------------------- namelist_data -------------------------------------------

    module namelist_data

    use precision_parameters
    use cparams

    implicit none
    save
    
    logical :: nmlflag = .false.                    ! true if input file is in namelist format
    integer :: input_file_line_number               ! current line read in a namelist-format input file
    logical :: compflag=.false.                     ! true if each namelist type has been read in
    logical :: connflag=.false.
    logical :: devcflag=.false.
    logical :: tablflag=.false.
    logical :: insfflag=.false.
    logical :: fireflag=.false.
    logical :: headflag=.false.
    logical :: initflag=.false.
    logical :: isofflag=.false.
    logical :: matlflag=.false.
    logical :: miscflag=.false.
    logical :: rampflag=.false.
    logical :: slcfflag=.false.
    logical :: timeflag=.false.
    logical :: ventflag=.false. 
    logical :: diagflag=.false.

    end module namelist_data
    
! --------------------------- option_data -------------------------------------------

module option_data

    use precision_parameters
    use cparams
    implicit none
    save

    integer, parameter :: mxdebug = 19
    integer, parameter :: mxopt = 19

    integer, parameter :: off = 0
    integer, parameter :: on = 1

    integer, parameter :: fccfm = 1
    integer, parameter :: fcfast = 2

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
    integer, parameter :: fode=11
    integer, parameter :: fhcl=12
    integer, parameter :: fmvent=13
    integer, parameter :: fkeyeval=14
    integer, parameter :: fpsteady=15
    integer, parameter :: fhvloss=16
    integer, parameter :: fmodjac=17
    integer, parameter :: fpdassl=18
    integer, parameter :: foxygen=19

    integer, dimension(mxopt) :: option = &
        !   fire,       hflow,      entrain,    vflow,      cjet
        (/  2,          1,          1,          1,          1,  &
        !   door-fire,  convec,     rad,        conduct,    debug
        !                           changing default rad option from 2 to 4 causes absorption coefs 
        !                           to take on constant default values rather than computed from data
            1,          1,          2,          1,          0,  &
        !   exact ode,  hcl,        h_mflow,      keyboard,   type of initialization
            1,          0,          1,          1,          0,  &
        !   mv heat loss,   mod jac,    dassl debug,    oxygen dassl solve
            0,              0,          0,              0/)

    real(eb) :: cutjac, stptime, prttime, tottime, ovtime, tovtime

    integer :: iprtalg = 0, jacchk = 0
    integer :: numjac = 0, numstep = 0, numresd = 0, numitr = 0, totjac = 0, totstep = 0, totresd = 0, totitr = 0, total_steps = 0

      end module option_data

! --------------------------- ramp_data -------------------------------------------

module ramp_data
    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    ! ramping variables
    integer :: n_ramps = 0
    type(ramp_type), target :: rampinfo(mxramps)

end module ramp_data

! --------------------------- room_data -------------------------------------------

module room_data

    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    ! compartment variables
    integer nr, nrm1

    real(eb) :: relative_humidity, interior_abs_pressure, exterior_abs_pressure, pressure_offset, pressure_ref, t_ref, &
        initial_mass_fraction(ns), interior_rho, exterior_rho, interior_ambient_temperature, exterior_ambient_temperature
    
    type(room_type), target :: roominfo(mxrooms)

    ! wall variables
    integer :: nwpts = (nnodes-1)/2                                     ! number of wall nodes 
    integer :: iwbound = 3                                              ! boundary condition type 
                                                                        !   1 = constant exterior surface temperature, 
                                                                        !   2 = insulated exterior surface, 
                                                                        !   3 =radiates to ambient
    real(eb), dimension(3) :: wsplit = (/0.50_eb, 0.17_eb, 0.33_eb/)    ! computed values for slab thickness, 
                                                                        ! initial fractions for inner, middle and outer wall slab
    
    integer :: nhcons
    integer, dimension(mxwal,5) :: i_hconnections   ! defines all surfaces for conduction routine, 1 entry for each wall that's on
                                                    !   1 = from room number
                                                    !   2 = from wall number (ceiling, upper walls, lower walls, floor)
                                                    !   3 = to room number
                                                    !   4 = to wall number
                                                    !   5 = boundary condition type for exterior surface
    integer :: nvcons
    integer, dimension(mxwal,4) :: i_vconnections   ! list of connected compartments for vertical heat transfer
                                                    !   1 = from room number
                                                    !   2 = from wall number (ceiling, upper walls, lower walls, floor)
                                                    !   3 = to room number
                                                    !   4 = to wall number

    logical :: adiabatic_walls                      ! true if all surfaces are adiabatic

end module room_data

! --------------------------- setup_data -------------------------------------------

module setup_data

    use precision_parameters
    implicit none
    save
    
    integer :: ss_out_interval, print_out_interval, smv_out_interval, time_end, i_time_end, i_time_step
    real(eb) :: stime, deltat

    character(128) :: title

    logical :: nokbd=.false., initializeonly=.false.
    logical :: debugging=.false., validate=.false., netheatflux=.false.
    integer :: cfast_version, outputformat=0
    integer, dimension(3) :: rundat
    character(60) :: nnfile=" ", datafile
    character(32) :: mpsdatc
    
    !File descriptors for cfast
    integer :: iofili=1, iofilo=6, iofill=3, iofilkernel, iofilstat=12
    character(6), parameter :: heading="VERSN"
    character(64) :: project, extension
    character(256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, &
        ssflow, ssnormal, ssspecies, ssspeciesmass, sswall, gitfile, errorlogging, stopfile, solverini, &
        queryfile, statusfile, kernelisrunning

    ! Work arrays for the csv input routines
    integer, parameter :: nrow=10000, ncol=100
    real(eb) :: rarray(nrow,ncol)
    character(128) :: carray(nrow,ncol)

end module setup_data

! --------------------------- smkview_data -------------------------------------------

module smkview_data

    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    integer, dimension (mxfires) :: smv_room
    real(eb), dimension(mxfires) :: smv_qdot, smv_zfire, smv_xfire, smv_yfire, smv_height
    real(eb), dimension(mxrooms) :: smv_relp,smv_zlay,smv_tl,smv_tu
    
    ! visualization variables
    integer :: nvisualinfo = 0
    type(visual_type), dimension (mxslice), target :: visualinfo

    integer :: nsliceinfo = 0
    type(slice_type), allocatable, dimension(:), target :: sliceinfo

    integer :: nisoinfo = 0
    type(iso_type), allocatable, dimension(:), target :: isoinfo

end module smkview_data

! --------------------------- solver_data -------------------------------------------

module solver_data

    use precision_parameters
    use cparams
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
    real(eb) :: stpmax = 1.0_eb                 ! maximum solver time step, if negative, then solver will decide
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

! --------------------------- target_data -------------------------------------------

module target_data
    use precision_parameters
    use cparams, only: mxthrmplen, mxtarg, mxdtect
    use  cfast_types, only: target_type, detector_type
    implicit none
    save

    ! variables for calculation of flux to a target

    integer :: idset                                                ! compartment where detector just went off. more than one
                                                                    ! sprinkler in a compartment is meaningless to CFAST

    integer :: n_targets                                            ! number of detectors in the simulation
    type (target_type), dimension(mxtarg), target :: targetinfo     ! structured target data

    integer :: n_detectors                                          ! number of detectors in the simulation
    type (detector_type), dimension(mxdtect), target :: detectorinfo! structured detector data

end module target_data

! --------------------------- thermal_data -------------------------------------------

module thermal_data

    use precision_parameters
    use cparams, only : mxthrmp
    use cfast_types, only: thermal_type
    implicit none
    save

    integer n_thrmp                                                  ! number of unique thermal properties in the simulation
    type (thermal_type), dimension(mxthrmp), target :: thermalinfo  ! structured thermal property data

    end module thermal_data

! --------------------------- vent_data -------------------------------------------

module vent_data

    use precision_parameters
    use cparams
    use cfast_types
    implicit none
    save

    ! hvent variables
    integer :: n_hvents                                                 ! number of horizontal vents
    type (vent_type), dimension(mxhvents), target :: hventinfo          ! structured horizontal vent data
    
    real(eb), dimension(2,mxhvents) :: vss, vsa, vas, vaa, vsas, vasa   ! individual flows for door jet fires (u or l)
    
    ! horizontal vent flow slab data by elevation in vent
    integer :: nvelev                                                   ! current number of slabs
    real(eb), dimension(mxfslab) :: yvelev                              ! elevations of breakpoints in vent flow
    real(eb), dimension(mxfslab) :: dpv1m2                              ! pressure in room 1 - pressure in room 2 in each slab
    integer, dimension(mxfslab) ::  dirs12                              ! direction of flow in each slab

    ! vvent variables
    integer :: n_vvents                                                 ! number of vertical flow vents
    type (vent_type), dimension(mxvvents), target :: vventinfo          ! structured vertical vent data

    ! hvac variables
    integer :: n_mvents                                                 ! number of mechanical flow vents
    type(vent_type), dimension(mxmvents), target :: mventinfo 

end module vent_data
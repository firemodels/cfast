
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
    
    real(eb), dimension(mxhvsys) :: zzhvm           ! total mass of gas in hvac system
    real(eb), dimension(mxhvsys,ns) :: zzhvspec     ! mass of each species in hvac system

end module cenviro

! --------------------------- debug_data -------------------------------------------

module  debug_data

    use precision_parameters
    implicit none

    logical :: residprn, jacprn
    logical :: residfirst = .true.
    logical :: jacfirst = .true.
    logical :: nwline=.true.
    logical :: prnslab
    integer :: ioresid, iojac, ioslab
    real(eb) ::   dbtime
    character(256) :: residfile, jacfile, residcsv, jaccsv, slabcsv

end module debug_data

! --------------------------- fire_data -------------------------------------------

module fire_data

    use precision_parameters
    use cfast_types
    use cparams
    implicit none
    save

    ! fire variables
    integer :: nfire, objrm(0:mxfires), objign(mxfires),  froom(0:mxfire), numobjl, iquench(mxrooms), ifroom(mxfire), &
        ifrpnt(mxrooms,2), heatfr, obj_fpos(0:mxfires)
    real(eb) :: lower_o2_limit, qf(mxrooms), objmaspy(0:mxfire), heatup(mxrooms), heatlp(mxrooms), oplume(3,mxfires), &
        qspray(0:mxfire,2), xfire(mxfire,mxfirp), objxyz(4,mxfires), radconsplit(0:mxfire),heatfp(3), tradio, &
        radio(0:mxfire), fopos(3,0:mxfire), femr(0:mxfire), objpos(3,0:mxfires),fpos(3), &
        femp(0:mxfire),fems(0:mxfire),fqf(0:mxfire), fqfc(0:mxfire), fqlow(0:mxfire), fqupr(0:mxfire),fqdj(mxrooms), &
        farea(0:mxfire), tgignt
    logical objon(0:mxfires), heatfl
    type(fire_type), target :: fireinfo(mxfire)

    logical, dimension(0:mxfires) :: objld
    character(64), dimension(0:mxfires) :: odbnam
    character(256), dimension(0:mxfires) :: objnin
    integer, dimension(0:mxfires) :: objpnt

    logical, dimension(0:mxfires) :: objdef
    character(60), dimension(0:mxfires) :: omatl
    integer, dimension(mxfires) :: objlfm,objtyp,obtarg, objset

    real(eb), dimension(mxfires) :: obj_c, obj_h, obj_o, obj_n, obj_cl
    real(eb), dimension(3,0:mxfires) :: objcri, objort
    real(eb), dimension(0:mxfires) :: objmas, objgmw, objclen
    real(eb), dimension(mxpts,0:mxfires) :: objhc, omass, oarea, ohigh, oqdot ,oco, ohcr, ood, ooc
    real(eb), dimension(mxpts,ns,mxfires) :: omprodr
    real(eb), dimension(mxpts,mxfires) :: otime
    real(eb), dimension(2,0:mxfires) :: obcond
    real(eb) :: objmint, objphi, objhgas, objqarea, pnlds, dypdt, dxpdt, dybdt, dxbdt, dqdt

end module fire_data

! --------------------------- option_data -------------------------------------------

module option_data

    use precision_parameters
    use cparams
    implicit none
    save

    integer, parameter :: mxdebug = 19
    integer, parameter :: mxopt = 21

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
    integer, parameter :: fbtdtect=20
    integer, parameter :: fbtobj=21

    integer, parameter :: d_jac = 17
    integer, parameter :: d_cnt = 1
    integer, parameter :: d_prn = 2
    integer, parameter :: d_mass = 1
    integer, parameter :: d_hvac = 2
    integer, parameter :: d_hflw = 3
    integer, parameter :: d_vflw = 4
    integer, parameter :: d_mvnt = 5
    integer, parameter :: d_dpdt = 18
    integer, parameter :: d_diag = 19

    integer, dimension(mxopt) :: option = &
        ! fire, hflow, entrain, vflow, cjet
        (/   2,     1,       1,     1,    1,  &
        ! door-fire, convec, rad, conduct, debug
                  1,      1,   2,       1,     0,  &
        ! exact ode,  hcl, mflow, keyboard, type of initialization
                  1,    0,     1,        1,                      0,  &
        !  mv heat loss, mod jac, dassl debug, oxygen dassl solve, back track on dtect, back track on objects
                      0,       0,           0,                  0,                   0,                    0/)
!*** in above change default rad option from 2 to 4
!*** this causes absorption coefs to take on constant default values rather than computed from data
    integer, dimension(mxopt) :: d_debug = 0

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
    integer :: nramps = 0
    real(eb) :: qcvh(4,mxhvents), qcvv(4,mxvvents), qcvm(4,mxfan), qcvf(4,mxfan)
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
    integer nr, nrm1, n_species

    real(eb) :: relative_humidity, interior_abs_pressure, exterior_abs_pressure, pressure_offset, pressure_ref, t_ref, &
        initial_mass_fraction(ns), interior_rho, exterior_rho, interior_temperature, exterior_temperature
    
    type(room_type), target :: roominfo(mxrooms)

    ! wall variables
    integer :: nwpts = (nnodes-1)/2                                     ! number of wall nodes 
    integer :: iwbound = 3                                              ! boundary condition type 
                                                                        !   1 = constant exterior surface temperature, 
                                                                        !   2 = insulated exterior surface, 
                                                                        !   3 =radiates to ambient
    real(eb), dimension(3) :: wsplit = (/0.50_eb, 0.17_eb, 0.33_eb/)    ! computed values for slab thickness, 
                                                                        ! initially fractions for inner, middle and outer wall slab
    
    integer :: numnode(mxslb+1,4,mxrooms), nslb(nwal,mxrooms),nwalls, nfurn
    real(eb) :: rdqout(4,mxrooms), fkw(mxslb,nwal,mxrooms), cw(mxslb,nwal,mxrooms), rw(mxslb,nwal,mxrooms), &
        flw(mxslb,nwal,mxrooms), epw(nwal,mxrooms), twj(nnodes,mxrooms,nwal)
    integer, dimension(4*mxrooms,5) :: izwall       ! defines all surfaces for conduction routine
    logical :: adiabatic_wall
    
    real(eb), dimension (mxrooms,4) :: wlength
    real(eb), dimension (nnodes,mxrooms,4) :: walldx
    real(eb), dimension(mxpts) :: furn_time, furn_temp
    real(eb) :: qfurnout
    
    ! room to room heat transfer
    real(eb), dimension(mxrooms,mxrooms) :: heat_frac
    integer, dimension(0:mxrooms) :: iheat
    integer, dimension(mxrooms,0:mxrooms) :: iheat_connections

    integer, dimension(ns+2) :: izpmap              ! maps species to corresponding DASSL equations
    integer, dimension(mxrooms,4) :: izwmap         ! maps walls to corresponding DASSL equations
    integer, dimension(mxrooms,4) :: izswal         ! maps connecting walls between compartments for conduction
    integer :: nswal

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
    integer :: version, outputformat=0
    integer, dimension(3) :: rundat
    character(128) :: thrmfile="thermal"
    character(60) :: nnfile=" ", datafile
    character(32) :: mpsdatc
    
    !File descriptors for cfast
    integer :: iofili=1, iofilo=6, logerr=3
    character(6), parameter :: heading="VERSN"
    character(64) :: project
    character(256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, &
        ssflow, ssnormal, ssspecies, sswall, errorlogging, stopfile, solverini, &
        historyfile, queryfile, statusfile, kernelisrunning

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

    integer :: smkunit, spltunit, flocal(mxfire+1)
    character(60) :: smkgeom, smkplot, smkplottrunc
    logical :: remapfiresdone
    real(eb), dimension(mxfire+1) :: fqlocal, fzlocal, fxlocal, fylocal, fhlocal
    real(eb), dimension(mxrooms) :: smv_relp,smv_zlay,smv_tl,smv_tu         ! temp arrays to pass info to smokeview
    real(eb), dimension(mxfires) :: smv_qdot,smv_height                     ! temp arrays to pass info to smokeview
    
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
    real(eb) :: aptol = 1.0e-6_eb        ! absolute pressure tolerance
    real(eb) :: rptol = 1.0e-6_eb        ! relative pressure tolerance
    real(eb) :: atol = 1.0e-5_eb         ! absolute other tolerance
    real(eb) :: rtol = 1.0e-5_eb         ! relative other tolerance
    real(eb) :: awtol = 1.0e-2_eb        ! absolute wall tolerance
    real(eb) :: rwtol = 1.0e-2_eb        ! relative wall tolerance
    real(eb) :: algtol = 1.0e-8_eb       ! initialization tolerance
    real(eb) :: ahvptol = 1.0e-6_eb      ! absolute HVAC pressure tolerance
    real(eb) :: rhvptol = 1.0e-6_eb      ! relative HVAC pressure tolerance
    real(eb) :: ahvttol = 1.0e-5_eb      ! absolute HVAC temperature tolerance
    real(eb) :: rhvttol = 1.0e-5_eb      ! relative HVAC temperature tolerance

    real(eb), dimension(nt) :: pinit
    real(eb), dimension(1) :: rpar2
    integer, dimension(3) :: ipar2
    
    ! time step setup values
    real(eb) :: stpmax = 1.0_eb         ! maximum solver time step, if negative, then solver will decide
    real(eb) :: stpfirst = 0.005_eb     ! first time step for DASSL
    logical :: stpminflag               ! true if CFAST should check for too small time steps
    real(eb) :: stpmin                  ! minimum time step below which DASSL may be failing to find a solution.
    integer :: stpmin_cnt               ! current count of time steps below stpmin
    integer :: stpmin_cnt_max           ! maximum number of time steps below stpmin before DASSL calls it quits
    
    ! solver variables
    integer :: nofp, nofpmv, noftmv, noftu, nofvu, noftl, nofoxyl, nofoxyu, nofwt, nofprd, &
        nofhvpr, nequals, noffsm
    real(eb), dimension(maxteq) :: p, pold, pdold
    real(eb) :: told, dt
    
    integer :: jaccol
    integer :: jacn1, jacn2, jacn3, jacdim
                             
    integer :: ndisc                            ! number of discontinuities fed to DASSL
    real(eb), dimension(0:mxpts+1) :: discon    ! list of discontinuities fed to DASSL to ease solution

end module solver_data

! --------------------------- target_data -------------------------------------------

module target_data
    use precision_parameters
    use cparams, only: mxthrmplen, mxtarg, mxdtect
    use  cfast_types, only: target_type, detector_type
    implicit none
    save

    ! variables for calculation of flux to a target

    integer, parameter :: pde = 1                                   ! plate targets (cartesian coordinates)
    integer, parameter :: cylpde = 2                                ! cylindrical targets (cylindrical coordinates)
    integer, parameter :: interior = 1                              ! back surface of target is exposed to compartment interior
    integer, parameter :: exterior = 2                              ! back surface of target is exposed to compartment exterior

    integer :: ndtect                                               ! number of detectors in the simulation
    integer :: ntarg                                                ! number of detectors in the simulation
    integer :: idset                                                ! compartment where detector just went off. more than one
                                                                    ! sprinkler in a compartment is meaningless to CFAST

    type (target_type), dimension(mxtarg), target :: targetinfo     ! structured target data

    type (detector_type), dimension(mxdtect), target :: detectorinfo! structured detector data

end module target_data

! --------------------------- thermal_data -------------------------------------------

module thermal_data

    use precision_parameters
    use cparams
    implicit none
    save

    real(eb), dimension(mxslb,mxthrmp) :: lfkw, lcw, lrw, lflw
    real(eb), dimension(mxthrmp) :: lepw

    integer maxct, numthrm
    integer, dimension(mxthrmp) :: lnslb
    character(mxthrmplen), dimension(mxthrmp) :: nlist

    end module thermal_data

! --------------------------- vent_data -------------------------------------------

module vent_data

    use precision_parameters
    use cparams
    use cfast_types
    implicit none
    save

    ! hvent variables
    integer :: ihvent_connections(mxrooms,mxrooms), ijk(mxrooms,mxrooms,mxccv), vface(mxhvents), nventijk
    real(eb) :: hhp(mxhvents), bw(mxhvents), hh(mxhvents), hl(mxhvents), ventoffset(mxhvents,2), &
    hlp(mxhvents)

    ! vvent variables
    integer :: ivvent_connections(mxrooms,mxrooms), vshape(mxrooms,mxrooms)
    real(eb) :: vvarea(mxrooms,mxrooms), vmflo(mxrooms,mxrooms,2), qcvpp(4,mxrooms,mxrooms)

    ! hvac variables
    integer :: hvorien(mxext), hvnode(2,mxext), na(mxbranch),  &
        ncnode(mxnode), ne(mxbranch), mvintnode(mxnode,mxcon), icmv(mxnode,mxcon), nfc(mxfan), &
        nf(mxbranch),  ibrd(mxduct), nfilter, ndt, next, nnode, nfan, nbr, &
        izhvmapi(mxnode), izhvmape(mxnode), izhvie(mxnode), izhvsys(mxnode), izhvbsys(mxbranch), nhvpvar, nhvtvar, nhvsys
    real(eb) :: hveflo(2,mxext), hveflot(2,mxext), hvextt(mxext,2), &
        arext(mxext), hvelxt(mxext), ce(mxbranch), hvdvol(mxbranch), tbr(mxbranch), rohb(mxbranch), bflo(mxbranch), &
        hvp(mxnode), hvght(mxnode), dpz(mxnode,mxcon), hvflow(mxnode,mxcon), &
        qmax(mxfan), hmin(mxfan), hmax(mxfan), hvbco(mxfan,mxcoeff), eff_duct_diameter(mxduct), duct_area(mxduct),&
        duct_length(mxduct),hvconc(mxbranch,ns), hvexcn(mxext,ns,2), tracet(2,mxext), traces(2,mxext), hvfrac(2,mxext), &
        chv(mxbranch), dhvprsys(mxnode,ns), hvtm(mxhvsys), hvmfsys(mxhvsys),hvdara(mxbranch), ductcv
    logical :: mvcalc_on

    integer, dimension(mxhvent,2) :: ivvent
    integer :: n_hvents, n_vvents

    real(eb), dimension(mxrooms,mxhvent) :: zzventdist
    real(eb), dimension(2,mxhvent) :: vss, vsa, vas, vaa, vsas, vasa
    
    !slab data
    real(eb), dimension(mxfslab) :: yvelev, dpv1m2
    integer, dimension(mxfslab) ::  dirs12
    integer :: nvelev, ioutf

    type (vent_type), dimension(mxhvent), target :: hventinfo
    type (vent_type), dimension(mxvvent), target :: vventinfo
    type (vent_type), dimension(mxext), target :: mventinfo

end module vent_data
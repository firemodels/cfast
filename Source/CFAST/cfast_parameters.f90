
! --------------------------- cparams -------------------------------------------

module cparams
    use precision_parameters

    integer, parameter :: lbufln=1024           ! default line length for all inputs

    ! geometry parameters
    integer, parameter :: mxrooms = 101         ! maximum number of compartments
    integer, parameter :: mxslb = 6             ! maximum number of slabs in a surface material
                                                !                        (at the moment, the gui only support 1)
    integer, parameter :: nwal = 4              ! number of compartment surfaces (ceiling, upper walls, lower walls, floor)
    integer, parameter :: mxwal = mxrooms*nwal  ! maximum total number of compartment surfaces
    integer, parameter :: mxcross=21            ! maximum number of data points for variable cross-sectional area of a compartment
    integer, parameter :: nnodes = 61           ! number of nodes in a material for conduction calculation (should be odd number)
    integer, parameter :: mxslice = 5*mxrooms   ! maximum number of slices and isosurfaces in an input file

    ! fire related input parameters
    integer, parameter :: mxpts = 199           ! maximum number of data points in a time-dependent input curve
    integer, parameter :: ns = 11               ! number of species
    integer, parameter :: mxfires = 202         ! maximum number of fire objects
    integer, parameter :: mxfirp = 20           ! number of parameters for each fire object
    integer, parameter :: igntemp = 1           ! object ignition criteria index for temperature
    integer, parameter :: ignflux = 2           ! object ignition criteria index for heat flux

    integer, parameter :: mxthrmp = 125         ! maximum number of thermal properties
    integer, parameter :: mxthrmplen = 16       ! maximum length for thermal property short names

    ! ventilation parameters
    integer, parameter :: mxccv = 25            ! maximum number of vent connections between compartment pairs
    integer, parameter :: mxhvents = mxccv*mxrooms   ! maximum number of horizontal flow vents
    integer, parameter :: mxhvent = mxhvents*2  ! maximum number of connections in horizontal flow vents
                                                !   (one for "from" one to "to")
    integer, parameter :: mxfslab = 10          ! number of slabs in a horizontal flow calculation
    integer, parameter :: mxfprd = ns           ! maximum number of products tracked in the horizontal flow calculation
                                                !   (should be the same as ns)

    integer, parameter :: mxvvents=2*mxrooms        ! maximum number of vertical flow vents
    integer, parameter :: mxvvent = mxvvents*2      ! maximum number of connections in vertical flow vents

    integer, parameter :: mxhvsys=2*mxrooms         ! maximum number of mechanical ventilation systems
    integer, parameter :: mxfan = mxrooms           ! maximum number of fans in a mechanical ventilation system
    integer, parameter :: mxcoeff = 5               ! maximum order of fan curve (here, 5th order polynomial. at the moment,
                                                    !   the gui limits to constant flow)
    integer, parameter :: mxcon = 3                 ! maximum number of connections to a node in a mechanical ventilation system
    integer, parameter :: mxduct = mxrooms+2        ! maximum number of ducts in a mechanical ventilation system
    integer, parameter :: mxnode = 2*mxduct         ! maximum number of nodes in a mechanical ventilation system
    integer, parameter :: mxext = 2*mxrooms         ! maximum number of external connections in a mechanical ventilation system
    integer, parameter :: mxbranch = mxfan+mxduct   ! maximum number of branches in a mechanical ventilation system

    integer, parameter :: mxramps = 8*mxfires+mxhvent+mxvvents+mxhvsys ! maximum number of possible time-based ramps

    ! room related parameters
    real(eb), parameter :: vminfrac = 1.0e-4_eb     ! minimum layer volume as a fraction of room volume

    real(eb), parameter :: mx_vsep=0.01_eb          ! maximum vertical distance between elements before they are considered
                                                    ! separate elements (connected compartments for example)
    real(eb), parameter :: mx_hsep = 1.0e-3_eb      ! maximum horizontal distance below which fire is assumed to
                                                    ! be on a surface for entrainmnt
    real(eb), parameter :: xlrg = 1.0e+5_eb         ! sizes for outsize room
    real(eb), parameter :: deltatemp_min = 0.01_eb  ! minimum temperature difference for bouyancy to deposit all into a layer

    ! target parameters
    integer, parameter :: mxtarg = 10*mxrooms               ! maximum number of targets
    integer, parameter :: nnodes_trg = nnodes-1             ! number of interior nodes in a target for conduction calculation
    integer, parameter :: idx_tempf_trg = 1                 ! position of front temperature of target (front surface temperature)
    integer, parameter :: idx_tempb_trg = idx_tempf_trg+nnodes_trg-1 ! position of back temperature of target
                                                            ! (back surface temperature)
    integer, parameter :: mxr_trg = idx_tempb_trg           ! upper bound of real target array
    integer, parameter :: mxi_trg = 7                       ! upper bound of integer target array

    integer, parameter :: mxdtect=10*mxrooms                ! maximum number of detectors

    integer, parameter :: check_detector_state = 0          ! index to check state of detectors and targets
    integer, parameter :: set_detector_state = 1            ! index to calculate full state of detectors and targets
    integer, parameter :: update_detector_state = 2         ! index to update state of detectors and targets on
                                                            ! successful equation set solution

    ! parameters for equation solver
    ! nt = 4*mxrooms(main equ) + 2*mxrooms*ns(species) + mxhvsys*ns(hvac species)
    integer, parameter :: nt = 12*mxrooms + 2*mxrooms*ns + mxhvsys*ns ! total number of main equations for dae solver
    integer, parameter :: maxjeq = 6*mxrooms + mxnode + mxbranch
    integer, parameter :: maxeq = maxjeq + nwal*mxrooms
    integer, parameter :: maxteq = maxeq+2*mxrooms*ns+mxhvsys*ns+4*mxrooms*3
    
    ! define indices for flow arrays
    integer, parameter :: l = 2                             ! lower layer
    integer, parameter :: u = 1                             ! upper layer
    integer, parameter :: m = 1                             ! mass
    integer, parameter :: q = 2                             ! energy
    integer, parameter :: pp = 3                            ! beginning of species
    
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
    integer, parameter :: ct = 10
    integer, parameter :: ts = 11

end module cparams

! --------------------------- detectorptrs -------------------------------------------

module detectorptrs

    implicit none

    ! detector types
    integer, parameter :: smoked = 1    ! smoke detector
    integer, parameter :: heatd = 2     ! heat detector
    integer, parameter :: sprinkd = 3   ! sprinkler

end module detectorptrs

! --------------------------- wallptrs -------------------------------------------

module wallptrs
   integer, parameter :: w_from_room=1
   integer, parameter :: w_from_wall=2
   integer, parameter :: w_to_room=3
   integer, parameter :: w_to_wall=4
   integer, parameter :: w_boundary_condition=5
end module wallptrs

! --------------------------- fireptrs -------------------------------------------

module fireptrs

    integer, parameter :: free = 1
    integer, parameter :: cont = 2
    integer, parameter :: fsm = 3
    integer, parameter :: pool = 4

    integer, parameter :: f_fire_xpos=1
    integer, parameter :: f_fire_ypos=2
    integer, parameter :: f_fire_zpos=3
    integer, parameter :: f_plume_zpos=4
    integer, parameter :: f_plume_xpos=5
    integer, parameter :: f_plume_ypos=6
    integer, parameter :: f_qfc=7
    integer, parameter :: f_qfr=8
    integer, parameter :: f_heatlpup=9
    integer, parameter :: f_heatlp=10
    integer, parameter :: f_heatup=11
    integer, parameter :: f_objct=12
    integer, parameter :: f_ysoot=13
    integer, parameter :: f_yco=14
    integer, parameter :: f_obj_length=19
    integer, parameter :: f_obj_area=20
end module fireptrs
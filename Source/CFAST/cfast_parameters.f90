
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

    integer, parameter :: mxthrmp = 125         ! maximum number of thermal properties
    integer, parameter :: mxthrmplen = 16       ! maximum length for thermal property short names
    
    integer, parameter :: trigger_by_time = 1   ! indicies for fire ignition type (also used by vents)
    integer, parameter :: trigger_by_temp = 2
    integer, parameter :: trigger_by_flux = 3

    ! ventilation parameters
    integer, parameter :: mxccv = 25                ! maximum number of vent connections between compartment pairs
    integer, parameter :: mxhvents = mxccv*mxrooms  ! maximum number of horizontal flow vents
    integer, parameter :: mxfslab = 10              ! maximum number of slabs in a horizontal flow calculation

    integer, parameter :: mxvvents=mxccv*mxrooms    ! maximum number of vertical flow vents

    integer, parameter :: mxmvents=2*mxrooms         ! maximum number of mechanical ventilation systems
    integer, parameter :: mxfan = mxrooms           ! maximum number of fans in a mechanical ventilation system
    integer, parameter :: mxcoeff = 1               ! maximum order of fan curve (as a polynomial). at the moment,
                                                    !   the gui limits to constant flow
    integer, parameter :: mxcon = 3                 ! maximum number of connections to a node in a mechanical ventilation system
    integer, parameter :: mxduct = mxrooms+2        ! maximum number of ducts in a mechanical ventilation system
    integer, parameter :: mxnode = 2*mxduct         ! maximum number of nodes in a mechanical ventilation system
    integer, parameter :: mxext = 2*mxrooms         ! maximum number of external connections in a mechanical ventilation system
    integer, parameter :: mxbranch = mxfan+mxduct   ! maximum number of branches in a mechanical ventilation system

    integer, parameter :: mxramps = 8*mxfires+mxhvents+mxvvents+mxmvents ! maximum number of possible time-based ramps
    integer, parameter :: initial_time = 1          ! indicies for simple vent opening data
    integer, parameter :: initial_fraction = 2
    integer, parameter :: final_time = 3
    integer, parameter :: final_fraction = 4

    ! room related parameters
    real(eb), parameter :: vminfrac = 1.0e-4_eb     ! minimum layer volume as a fraction of room volume

    real(eb), parameter :: mx_vsep=0.01_eb          ! maximum vertical distance between elements before they are considered
                                                    ! separate elements (connected compartments for example)
    real(eb), parameter :: mx_hsep = 1.0e-3_eb      ! maximum horizontal distance below which fire is assumed to
                                                    ! be on a surface for entrainmnt
    real(eb), parameter :: xlrg = 1.0e+5_eb         ! sizes for outside room
    real(eb), parameter :: deltatemp_min = 1.0_eb   ! minimum temperature difference for bouyancy to deposit all into a layer
    integer, parameter :: interior = 1              ! compartment interior
    integer, parameter :: exterior = 2              ! compartment exterior

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
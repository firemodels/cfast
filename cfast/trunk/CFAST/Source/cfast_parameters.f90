
! --------------------------- cparams -------------------------------------------

module cparams
    use precision_parameters

    ! geometry parameters
    integer, parameter :: nr = 31               ! maximum number of compartments
    integer, parameter :: mxslb = 6             ! maximum number of slabs in a surface material (at the moment, the gui only support 1)
    integer, parameter :: nwal = 4              ! number of compartment surfaces (ceiling, upper walls, lower walls, floor)
    integer, parameter :: mxcross=21            ! maximum number of data points for variable cross-sectional area of a compartment
    integer, parameter :: nnodes = 61           ! number of nodes in a material for conduction calculation
    integer, parameter :: mxslice = 5*nr        ! maximum number of slices and isosurfaces in an input file
    
    ! fire related input parameters    
    integer, parameter :: mxpts = 199           ! maximum number of data points in a time-dependent input curve
    integer, parameter :: ns = 11               ! number of species
    integer, parameter :: mxfires = 125         ! maximum number of fire objects
    integer, parameter :: mxfirp = 20           ! number of parameters for each fire object
    integer, parameter :: mxfire = 2*mxfires    ! maximum number of fires in a single simulation
    integer, parameter :: igntemp = 1           ! object ignition criteria index for temperature
    integer, parameter :: ignflux = 2           ! object ignition criteria index for heat flux
    
    integer, parameter :: mxthrmp = 125         ! maximum number of thermal properties
 
    ! ventilation parameters
    integer, parameter :: mxccv = 25            ! maximum number of vent connections between compartment pairs
    integer, parameter :: mxvents = mxccv*nr    ! maximum number of horizontal flow vents
    integer, parameter :: mxvent = mxvents*2    ! maximum number of connections in horizontal flow vents 
                                                !   (one for "from" one to "to")
    integer, parameter :: mxfslab = 10          ! number of slabs in a horizontal flow calculation
    integer, parameter :: mxfprd = ns           ! maximum number of products tracked in the horizontal flow calculation 
                                                !   (should be the same as ns)
    
    integer, parameter :: mxvv=2*nr                 ! maximum number of vertical flow vents
    integer, parameter :: mxhvsys=60                ! maximum number of mechanical ventilation systems
    integer, parameter :: mxfan = 15                ! maximum number of fans in a mechanical ventilation system
    integer, parameter :: mxcoeff = 5               ! maximum order of fan curve (here, 5th order polynomial. at the moment, 
                                                    !   the gui limits to constant flow)
    integer, parameter :: mxcon = 3        ! maximum number of connections to a single node in a mechanical ventilation system
    integer, parameter :: mxduct = nr+2             ! maximum number of ducts in a mechanical ventilation system
    integer, parameter :: mxnode = 2*mxduct         ! maximum number of nodes in a mechanical ventilation system
    integer, parameter :: mxext = 2*nr              ! maximum number of external connections in a mechanical ventilation system
    integer, parameter :: mxbranch = mxfan+mxduct   ! maximum number of branches in a mechanical ventilation system
    
    integer, parameter :: mxramps = 8*mxfire+mxvent+mxvv+mxhvsys ! maximum number of possible time-based ramps
    
    real(eb), parameter :: mx_vsep=0.01_eb     ! maximum vertical distance between elements before they are considered 
                                               ! separate elements (connected compartments for example)
    real(eb), parameter :: mx_hsep = 1.0e-3_eb ! maximum horizontal distance below which fire is assumed to 
                                               ! be on a surface for entrainmnt
    
    ! target parameters
    integer, parameter :: mxtarg = 10*nr                            ! maximum number of targets
    integer, parameter :: nnodes_trg = nnodes-1             ! number of interior nodes in a target for conduction calculation
    integer, parameter :: idxtempf_trg = 17                 ! position of front temperature of target (front surface temperature)
    integer, parameter :: idx_tempb_trg = idxtempf_trg+nnodes_trg-1 ! position of back temperature of target 
                                                                    ! (back surface temperature)
    integer, parameter :: mxr_trg = idx_tempb_trg                   ! upper bound of real target array 
    integer, parameter :: mxi_trg = 7                               ! upper bound of integer target array
    
    integer, parameter :: check_detector_state = 0      ! index to check state of detectors and targets
    integer, parameter :: set_detector_state = 1        ! index to calculate full state of detectors and targets
    integer, parameter :: update_detector_state = 2     ! index to update state of detectors and targets on 
                                                        ! successful equation set solution
    
    integer, parameter :: upper = 1         ! index for upper layer
    integer, parameter :: lower = 2         ! index for lower layer
    
    ! parameters for equation solver
    ! nt = 4*nr(main equ) + 2*nr*ns(species) + mxhvsys*ns(hvac species)
    integer, parameter :: nt = 12*nr + 2*nr*ns + mxhvsys*ns ! total number of main equations for dae solver
    integer, parameter :: maxjeq = 6*nr + mxnode + mxbranch
    integer, parameter :: maxeq = maxjeq + nwal*nr
    integer, parameter :: maxteq = maxeq+2*nr*ns+mxhvsys*ns+4*nr*3

end module cparams

! --------------------------- dsize -------------------------------------------

module dsize

    implicit none

    integer, parameter :: mxdtect=100   ! maximum number of detectors
    integer, parameter :: dtxcol=15     ! number of floating point columns in detector data structure
    integer, parameter :: dticol=4      ! number of integer columns in detector data structure
    
    ! detector types
    integer, parameter :: smoked = 1    ! smoke detector
    integer, parameter :: heatd = 2     ! heat detector

    ! pointers into floating point detector data structure
    integer, parameter :: drti=1        ! RTI value for detector / sprinkler response
    integer, parameter :: dxloc=2       ! X location of detector
    integer, parameter :: dyloc=3       ! Y location of detector
    integer, parameter :: dzloc=4       ! Z location of detector
    integer, parameter :: dtrig=5
    integer, parameter :: dtemp=6
    integer, parameter :: dtempo=7
    integer, parameter :: dvel=8
    integer, parameter :: dvelo=9
    integer, parameter :: dtact=10
    integer, parameter :: dtjet=11
    integer, parameter :: dtjeto=12
    integer, parameter :: dspray=13
    integer, parameter :: drate=14
    integer, parameter :: dthalf=15

    ! pointers into integer detector data structure
    integer, parameter :: droom=1
    integer, parameter :: dtype=2
    integer, parameter :: dquench=3
    integer, parameter :: dact=4

end module dsize

! --------------------------- targptrs -------------------------------------------

module targptrs
   integer, parameter :: t_total=1
   integer, parameter :: t_ftotal=2
   integer, parameter :: t_wtotal=3
   integer, parameter :: t_gtotal=4
   integer, parameter :: t_ctotal=5
end module targptrs
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

! --------------------------- flwptrs -------------------------------------------

module flwptrs

    ! define indexes for use with product arrays
    integer, parameter :: l = 2
    integer, parameter :: u = 1
    integer, parameter :: m = 1
    integer, parameter :: q = 2
    integer, parameter :: o = 3
    integer, parameter :: pp = 3
      
end module flwptrs
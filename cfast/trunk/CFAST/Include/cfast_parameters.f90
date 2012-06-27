module cparams

    ! geometry parameters
    integer, parameter :: nr = 31           ! maximum number of compartments
    integer, parameter :: nn = 61           ! number of nodes in a material for conduction calculation
    integer, parameter :: mxslb = 6         ! maximum number of slabs in a surface material (at the moment, the gui only support 1)
    integer, parameter :: nwal = 4          ! number of compartment surfaces (ceiling, upper walls, lower walls, floor)
    integer, parameter :: mxpts=21          ! maximum number of data points for variable cross-sectional area of a compartment
    
    ! fire related input parameters    
    integer, parameter :: nv = 199          ! maximum number of data points in a time-dependent input curve
    integer, parameter :: ns = 11           ! number of species
    integer, parameter :: mxoin = nr        ! maximum number of fire objects
    integer, parameter :: mxfirp = 19       ! number of parameters for each fire object
    integer, parameter :: mxfire = 2*mxoin  ! maximum number of fires in a single simulation
    integer, parameter :: obotemp = 1       ! object ignition criteria index for temperature
    integer, parameter :: oboflux = 2       ! object ignition criteria index for heat flux
    
    integer, parameter :: nthmax = 125      ! maximum number of thermal properties
 
    ! ventilation parameters
    integer, parameter :: mxccv = 25        ! maximum number of vent connections between compartment pairs
    integer, parameter :: mxvents = mxccv*nr    ! maximum number of horizontal flow vents
    integer, parameter :: mxvent = mxvents*2    ! maximum number of connections in horizontal flow vents (one for "from" one to "to")
    integer, parameter :: mxslab = 10       ! number of slabs in a horizontal flow calculation
    integer, parameter :: mxprd = 11        ! maximum number of products tracked in the horizontal flow calculation (should be the same as ns)
    
    integer, parameter :: mxvv=2*nr         ! maximum number of vertical flow vents
    integer, parameter :: mxhvsys=60        ! maximum number of mechanical ventilation systems
    integer, parameter :: mfan = 15         ! maximum number of fans
    integer, parameter :: mfcoe = 5         ! maximum order of fan curve (here, 5th order polynomial. at the moment, the gui limits to constant flow)
    integer, parameter :: mcon = 3          ! maximum number of connections to a single node in a mechanical ventilation system
    integer, parameter :: mdt = nr+2        ! maximum number of ducts
    integer, parameter :: mnode = 2*mdt     ! maximum number of nodes      !
    integer, parameter :: mext = 2*nr       ! maximum number of external connections
    integer, parameter :: mbr = mfan+mdt    ! maximum number of branches in a system
    
    real(8), parameter :: vfmaxdz=0.01    ! maximum vertical distance between elements before they are considered separate elements (connected compartments for example)
    
    ! target parameters
    integer, parameter :: mxtarg = 3*nr     ! maximum number of targets
    integer, parameter :: trgtempf = 17     ! position of front temperature of target (front surface temperature)
    integer, parameter :: trgtnum = 50      ! number of interior nodes in a target for conduction calculation
    integer, parameter :: trgtempb = trgtempf+trgtnum-1   ! position of back temperature of target (back surface temperature)
    integer, parameter :: trgxrow = trgtempb ! upper bound of real target array 
    integer, parameter :: trgirow = 7       ! upper bound of integer target array
    
    integer, parameter :: mdchk = 0         ! index to check state of detectors and targets
    integer, parameter :: mdset = 1         ! index to calculate full state of detectors and targets
    integer, parameter :: mdupdt = 2        ! index to update state of detectors and targets on successful equation set solution
    
    integer, parameter :: upper = 1         ! index for upper layer
    integer, parameter :: lower = 2         ! index for lower layer
    
    ! parameters for equation solver
    ! nt = 4*nr(main equ) + 2*nr*ns(species) * 4*nr(hcl) + 4*nr(smoke) + mxhvsys*ns(hvac species)
    integer, parameter :: nt = 12*nr + 2*nr*ns + mxhvsys*ns ! total number of main equations for dae solver
    integer, parameter :: maxjeq = 6*nr + mnode + mbr
    integer, parameter :: maxeq = maxjeq + nwal*nr
    integer, parameter :: maxteq = maxeq+2*nr*ns+mxhvsys*ns+4*nr*3

end module cparams

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
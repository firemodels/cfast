module  iofiles

    implicit none
    
!File descriptors for cfast

    character(6), parameter :: heading="VERSN"
	character(64) ::  project
	character(256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, &
	      ssflow, ssnormal, ssspecies, sswall, errorlogging, stopfile, solverini, &
	      historyfile, queryfile, statusfile, kernelisrunning

! Work arrays for the csv input routines

    integer, parameter :: nrow=200, ncol=200
    real*8 rarray(nrow,ncol) 
    character(128) :: carray(nrow,ncol)

end module iofiles

module fltarget
    use cparams
    implicit none
    
    ! variables for calculation of flux to a target
      
    ! indices into floating point target data structure (XXTARG)      
    integer, parameter :: trgcenx = 1
    integer, parameter :: trgceny = 2
    integer, parameter :: trgcenz = 3
    integer, parameter :: trgnormx = 4
    integer, parameter :: trgnormy = 5
    integer, parameter :: trgnormz = 6
    integer, parameter :: trgk = 7
    integer, parameter :: trgrho = 8
    integer, parameter :: trgcp = 9
    integer, parameter :: trgl = 10
    integer, parameter :: trginterior = 11
    integer, parameter :: trgemis = 12
    integer, parameter :: trgtfluxf = 13
    integer, parameter :: trgtfluxb = 14
    integer, parameter :: trgnfluxf = 15
    integer, parameter :: trgnfluxb = 16

    ! indices into integer target data structure (IXTARG)
    integer, parameter :: trgroom = 1
    integer, parameter :: trglayer = 2
    integer, parameter :: trgwall = 3
    integer, parameter :: trgmeth = 4
    integer, parameter :: trgeq = 5
    integer, parameter :: trgback = 6
    
    integer, parameter :: ode = 1
    integer, parameter :: pde = 2
    integer, parameter :: cylpde = 3
    integer, parameter :: steady = 1
    integer, parameter :: mplicit = 2
    integer, parameter :: xplicit = 3
    integer, parameter :: int = 1
    integer, parameter :: ext =2

    character(8) :: cxtarg(mxtarg)

      real(8), dimension(mxtarg,2) :: QTFLUX, QTCFLUX, QTFFLUX, QTWFLUX, QTGFLUX
      real(8), dimension(mxtarg) :: TGTARG
      real(8), dimension(mxtarg,5)  :: GTFLUX
      integer, dimension(3) :: NEQTARG
end module fltarget

module cparams

    ! geometry parameters
    integer, parameter :: nr = 31           ! maximum number of compartments
    integer, parameter :: nn = 61           ! number of nodes in a material for conduction calculation
    integer, parameter :: mxslb = 6         ! maximum number of slabs in a surface material (at the moment, the gui only support 1)
    integer, parameter :: nwal = 4          ! number of compartment surfaces (ceiling, upper walls, lower walls, floor)
    
    ! fire related input parameters    
    integer, parameter :: nv = 199          ! maximum number of data points in a time-dependent input curve
    integer, parameter :: ns = 11           ! number of species
    integer, parameter :: mxoin = nr        ! maximum number of fire objects
    integer, parameter :: mxfirp = 19       ! number of parameters for each fire object
    integer, parameter :: mxfire = 2*mxoin  ! maximum number of fires in a single simulation
    
    integer, parameter :: nthmx = 125       ! maximum number of thermal properties
 
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
    integer, parameter :: trgtempf=17       ! position of front temperature of target (front surface temperature)
    integer, parameter :: trgtnum=50        ! number of interior nodes in a target for conduction calculation
    integer, parameter :: trgtempb=trgtempf+trgtnum-1   ! position of back temperature of target (back surface temperature)
    integer, parameter :: trgxrow=trgtempb  ! upper bound of real target array 
    integer, parameter :: trgirow=7         ! upper bound of integer target array
    
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

module  interfaces

interface
    subroutine chemie (pyrolysis_rate, molar_mass, entrainment_rate, source_room, h_c, y_soot, y_co, & 
    n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, activated_room, activated_sprinkler, activated_time, &
    activated_rate, model_time, hrr_at_activation, hrr_constrained, pyrolysis_rate_constrained, species_rates)

    integer, intent(in) :: source_room, activated_room, activated_sprinkler
    real*8, intent(in) :: pyrolysis_rate, molar_mass, entrainment_rate, h_c, y_soot, y_co, &
    n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, activated_time, activated_rate, model_time
    real*8, intent(out) :: hrr_constrained, hrr_at_activation, pyrolysis_rate_constrained, species_rates(:)
    end subroutine chemie
end interface
end module interfaces

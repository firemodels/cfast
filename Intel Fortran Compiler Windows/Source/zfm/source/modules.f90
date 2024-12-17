
module zonedata
  use precision
  implicit none
  save

  integer, parameter :: maxspecies=4, mass=-1, enthalpy=0, oxygen=1
 ! integer, parameter :: nitrogen=2, fuel=3, co=4, co2=5, soot=6	
  integer, parameter :: co=2, co2=3, smoke=4

  type zone_data
    real(kind=eb) :: temperature, density, volume, mass, o2index
    real(kind=eb), dimension(1:maxspecies) :: s_mass, s_con
  end type zone_data

  type flow_data
    real(kind=eb) :: mdot, qdot, qtotal, temperature, density, rel_height, abs_height
    real(kind=eb), dimension(1:maxspecies) :: sdot
  	logical :: fromlower, fromupper, zero
  end type flow_data

  type fire_data
    integer :: room_number, type
    real(kind=eb), dimension(4) :: taufl, taufu, angle
    real(kind=eb) :: heat_c, temp, x0,y0,z0,dz, time_rate, time_start
    real(kind=eb), dimension(maxspecies) :: yield
    real(kind=eb) :: mtotal, qtotal, qconvec, qrad, chi_rad
    real(kind=eb), pointer, dimension(:) :: times, q_pyrol
	  integer :: npoints
    type(flow_data) :: fire_flow, entrain_flow
  end type fire_data

  type wall_data
    integer :: n
    real(kind=eb), pointer, dimension(:) :: dx, wtemp
    real(kind=eb) :: k, rho, c, emis
    integer :: dir,from,to,wallmatindex
    character(len=30) :: wallmat
    real(kind=eb) :: temp, area, qdot
    logical :: defined
  end type wall_data

  type room_data
    type(zone_data) :: layer(2)
  	integer :: singlezone
    real(kind=eb) :: x0, y0, z0, &
                     dx, dy, dz, &
                     abs_pressure, rel_pressure, &
                     rel_layer_height, abs_layer_height, &
                     VU, VL, volume, volmax, volmin,    &
                     upper_area, lower_area, floor_area, &
                     fig14
    type(wall_data) :: wall(4)
  end type room_data
  type slab_data
    real(kind=eb) :: dp, height, bot, top, area
  	integer :: from, to
    type(flow_data) :: slab_flow, entrain_flow
  end type slab_data

  type hvac_data
    real(kind=eb) :: vfan, mfan, qfan, tfan, rhofan, dpfan, height
    real(kind=eb) :: rel_frombot, rel_tobot, rel_fromtop, rel_totop
    real(kind=eb) :: abs_frombot, abs_tobot, abs_fromtop, abs_totop
    real(kind=eb) :: fromupperfrac, fromlowerfrac
    real(kind=eb) :: toupperfrac, tolowerfrac
	  logical :: specifiedtemp
  	integer :: fromroom, toroom
  	type(slab_data) :: fromslab(2), toslab(2), totalslab
  end type hvac_data

  type vent_data
    integer :: nslabs, from, to, nneutrals,face
    real(kind=eb) :: relbot, reltop, width, offset
    real(kind=eb) :: absbot, abstop
    type(slab_data), dimension(6) :: slab
    real(kind=eb), dimension(7) :: abs_yelev, dpelev, dpslab
    real(kind=eb), dimension(3) :: yneutral
  end type vent_data

  integer, parameter :: lower=1, upper=2
  integer, parameter :: p_coldwall=-2, p_thinwall=-1, p_thickwall=0, p_nowall=-3
  integer, parameter :: p_ceiling=1, p_floor=2, p_wall=3
  integer, parameter :: p_fireflow=1, p_ventflow=2
  logical :: printresid, debugprint, solveoxy, solveprods
  type(room_data), allocatable, target, dimension(:) :: rooms
  type(vent_data), allocatable, target, dimension(:) :: vents
  type(fire_data), allocatable, target, dimension(:) :: fires
  type(hvac_data), allocatable, target, dimension(:) :: hvacs
  type(flow_data), allocatable, target, dimension(:,:) :: cflow, fflow, hflow, hvflow, totalflow

  type(flow_data) :: zeroflow

  real(kind=eb), parameter :: cp = 1004._eb, g=9.8_eb, cvent=0.7_eb
  real(kind=eb), parameter :: gamma=1.4_eb, cv=cp/gamma, rgas=cp-cv
  real(kind=eb), parameter :: pabs_ref=101300._eb
  real(kind=eb) :: tamb, pamb
  real(kind=eb), parameter :: twothirds=2.0_eb/3.0_eb, zero=0.0_eb
  real(kind=eb), parameter :: onethird=1.0_eb/3.0_eb
  real(kind=eb) :: rhoamb
  real(kind=eb), dimension(2) :: absorb

  integer :: offset_p, offset_vu, offset_tl, offset_tu, offset_oxyl, offset_oxyu
  integer, dimension(maxspecies,2) :: offset_SPECIES
  integer :: offset_col, offset_cou, offset_co2l, offset_co2u, offset_smokel, offset_smokeu
  integer, dimension(:,:), pointer :: nabor, nabor2

  integer, parameter :: constant=1, tsquared=2, general=3
  real(kind=eb) :: tnow, tstart, tfinal, tprint, tdump, tplot, tout
  real(kind=eb) :: tstartprint, tstartplot, tstartdump
  real(kind=eb) :: pi
  integer :: iprint, iplot, idump
  real(kind=eb) :: dprint=1.0_eb, ddump=10.0_eb, dplot=10.0_eb
  real(kind=eb) :: heat_c, heat_o2, chi_rad, amb_oxy_con, o2limit
  real(kind=eb), dimension(maxspecies) :: yield_SPECIES
  integer :: nvents, nrooms, nspecies, nfires, noldfires, nhvacs, neq, lrw, liw
  real(kind=eb), dimension(:), allocatable :: rwork
  integer, dimension(:), allocatable :: iwork
  integer :: n_single

  real(kind=eb), allocatable, dimension(:) :: vatol, vrtol, & 
         pprime, p, p_compact, pdzero, delta, xpsolve, dummysoln, zerosoln
  real(kind=eb) :: aptol, rptol, atol, rtol
  character(len=128) :: smvfile, plotfile, csvfile, plotfilebase, dumpfile
  character(len=30) :: allwallsmat
  logical :: allwalls

  integer, parameter :: smvunit=21, plotunit=22, csvunit=23, inunit=24, outunit=25, iniunit=26

  !  overload +, -, * and = so that these operators will work with flows!

  interface operator(+)
    module procedure addflow
  end interface
  interface operator(-)
    module procedure subtractflow
  end interface
  interface operator(*)
    module procedure realtimesflow
    module procedure flowtimesreal
  end interface
  interface assignment(=)
    module procedure assignflow
  end interface

  contains

! --------------- addflow ----------------------

type(flow_data) function addflow(flow1,flow2)
    type(flow_data), intent(in) :: flow1, flow2
    real(kind=eb) :: total_mdot, total_qdot, total_qtotal

  addflow%zero = .false.
	if(flow1%zero)then
	  addflow = flow2
	  return
	endif
	if(flow2%zero)then
	  addflow = flow1
	  return
	endif

  total_mdot = flow1%mdot + flow2%mdot
  total_qdot = flow1%qdot + flow2%qdot
  total_qtotal = flow1%qtotal + flow2%qtotal
  addflow%mdot = total_mdot
  addflow%qdot = total_qdot
  addflow%sdot(1:nspecies) = flow1%sdot(1:nspecies) + flow2%sdot(1:nspecies)
	if(total_mdot.ne.zero)then
    addflow%temperature = total_qdot/(cp*total_mdot)
	 else
    addflow%temperature = zero
    addflow%zero = .true.
	endif
	if(addflow%temperature.ne.zero)then
	  addflow%density = pabs_ref/(addflow%temperature*rgas)
	 else
	  addflow%density = zero
	endif
end function addflow


! --------------- subtractflow ----------------------

type(flow_data) function subtractflow(flow1,flow2)
    type(flow_data), intent(in) :: flow1, flow2
    real(kind=eb) :: total_mdot, total_qdot, total_qtotal, x

  subtractflow%zero = .false.
	if(flow1%zero)then
	  x = -1._eb
	  subtractflow = x*flow2
	  return
	endif
	if(flow2%zero)then
	  subtractflow = flow1
	  return
	endif

  total_mdot = flow1%mdot - flow2%mdot
  total_qdot = flow1%qdot - flow2%qdot
  total_qtotal = flow1%qtotal - flow2%qtotal
  subtractflow%mdot = total_mdot
  subtractflow%qdot = total_qdot
  subtractflow%qtotal = total_qtotal
  subtractflow%sdot(1:nspecies) = flow1%sdot(1:nspecies) - flow2%sdot(1:nspecies)
	if(total_mdot.ne.zero)then
    subtractflow%temperature = total_qdot/(cp*total_mdot)
	 else
    subtractflow%zero = .true.
    subtractflow%temperature = zero
	endif
	if(subtractflow%temperature.ne.zero)then
	  subtractflow%density = pabs_ref/(subtractflow%temperature*rgas)
	 else
	  subtractflow%density = zero
	endif
end function subtractflow

! --------------- realtimesflow ----------------------

type(flow_data) function realtimesflow(scale,flow1)
  type(flow_data), intent(in) :: flow1
	real(kind=eb), intent(in) :: scale

  if(scale.eq.0.0_eb)then
    realtimesflow%zero = .true.
   else
    realtimesflow%zero = flow1%zero
  endif
  if(realtimesflow%zero)return
  realtimesflow%mdot = scale*flow1%mdot
  realtimesflow%qdot = scale*flow1%qdot
  realtimesflow%qtotal = scale*flow1%qtotal
  realtimesflow%sdot(1:nspecies) = scale*flow1%sdot(1:nspecies)
  realtimesflow%temperature = flow1%temperature
  realtimesflow%density = flow1%temperature

end function realtimesflow

! --------------- flowtimesreal ----------------------

type(flow_data) function flowtimesreal(flow1,scale)
  type(flow_data), intent(in) :: flow1
	real(kind=eb), intent(in) :: scale

  if(scale.eq.0.0_eb)then
    flowtimesreal%zero = .true.
   else
    flowtimesreal%zero = flow1%zero
  endif
  if(flowtimesreal%zero)return
  flowtimesreal%mdot = scale*flow1%mdot
  flowtimesreal%qdot = scale*flow1%qdot
  flowtimesreal%qtotal = scale*flow1%qtotal
  flowtimesreal%sdot(1:nspecies) = scale*flow1%sdot(1:nspecies)
  flowtimesreal%temperature = flow1%temperature
  flowtimesreal%density = flow1%temperature
end function flowtimesreal

! --------------- assignflow ----------------------

subroutine assignflow(flowout,flowin)
  type(flow_data), intent(out) :: flowout
  type(flow_data), intent(in) :: flowin

	if(flowin%zero)then
	  flowout%zero = .true.
	  return
	endif

  flowout%mdot = flowin%mdot
  flowout%qdot = flowin%qdot
  flowout%qtotal = flowin%qtotal
  flowout%temperature = flowin%temperature
  flowout%density = flowin%density
  flowout%sdot(1:nspecies) = flowin%sdot(1:nspecies)
  flowout%zero = flowin%zero
end subroutine assignflow

end module zonedata

module zoneinterfaces
  interface
    subroutine f_entrain(flowroom,fire)
    use precision
    use zonedata
    implicit none
    type(room_data), pointer :: flowroom
    type(fire_data), pointer :: fire
    end subroutine f_entrain
  end interface
  interface
    subroutine setfire(qtotal,fire)
      use precision
      use zonedata
      implicit none

      real(kind=eb), intent(in) :: qtotal
      type(fire_data), pointer :: fire
    end subroutine setfire
  end interface

  interface
    subroutine v_entrain(flowroom,source_flow,entrain_flow)
    use precision
    use zonedata
    implicit none
    type(room_data), pointer :: flowroom
    type(flow_data), pointer :: source_flow, entrain_flow
    end subroutine v_entrain
  end interface
  interface
    subroutine flowgo(slabfrom,nfromslab,slabto,ntoslab,odeflow)
      use precision
      use zonedata
      implicit none
      integer, intent(in) :: nfromslab, ntoslab
      type(slab_data), target, dimension(nfromslab) :: slabfrom
      type(slab_data), target, dimension(ntoslab) :: slabto
      type(flow_data), dimension(0:nrooms,2) :: odeflow
    end subroutine flowgo
  end interface
  interface
    subroutine entrainfl(zlength,qsource,mentrain,dmdq,factor)
      use precision
      implicit none

      real(kind=eb), intent(in) :: zlength, qsource,factor
      real(kind=eb), intent(out) :: mentrain, dmdq
    end subroutine
  end interface


end module zoneinterfaces

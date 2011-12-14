
! --------------- fireflow ----------------------

subroutine fireflow
  use precision
  use zonedata
  implicit none
  integer :: ifire, iroom
  type(fire_data), pointer :: fire

  ! get fire flows for each fire

  call getfires

  ! put fire and entrainment flows into appropriate layers

  do ifire = 1, nfires
    fire => fires(ifire)
    iroom = fire%room_number
    fflow(iroom,lower) = fflow(iroom,lower) - fire%entrain_flow
    fflow(iroom,upper) = fflow(iroom,upper) + fire%plume_flow
  end do
end subroutine fireflow

! --------------- getfires ----------------------

subroutine getfires
  use precision
  use zonedata
  implicit none
  integer :: ifire
  type(fire_data), pointer :: fire
  type(room_data), pointer :: fireroom
  type(flow_data), pointer :: fire_flow, entrain_flow, plume_flow
  real(kind=dd), pointer :: mfire, qfire
  real(kind=dd) :: tentrain, zlength
  interface
    subroutine entrain(flowroom,source_flow,entrain_flow,flowtype)
    use precision
    use zonedata
    implicit none
    type(room_data), pointer :: flowroom
    type(flow_data), pointer :: source_flow, entrain_flow
    integer, intent(in) :: flowtype
    end subroutine entrain
  end interface

! compute fire flow

   do ifire = 1, nfires
     fire => fires(ifire)
     mfire => fire%fire_flow%mdot
     qfire => fire%fire_flow%qdot
	   fireroom => rooms(fire%room_number)
	   tentrain = fireroom%layer(lower)%temperature
	   zlength = fireroom%rel_layer_height
	   fire_flow => fire%fire_flow
	   entrain_flow => fire%entrain_flow
	   plume_flow => fire%plume_flow

	   if(mfire.eq.0.0_dd)then
       fire_flow%zeroflowflag = .true.
       entrain_flow%zeroflowflag = .true.
       plume_flow%zeroflowflag = .true.
	     cycle
	    else
       fire_flow%zeroflowflag = .false.
       entrain_flow%zeroflowflag = .false.
       plume_flow%zeroflowflag = .false.
	   endif
#ifdef pp_solveoxy
     fire_flow%sdot(oxygen) = -fire%qtotal/heat_o2
#endif
     fire_flow%temperature = qfire/(cp*mfire)
     fire_flow%density=fireroom%abs_pressure/(rgas*fire_flow%temperature)
	   fire_flow%rel_height = fire%z0
	   fire_flow%abs_height = fire%z0 + rooms(fire%room_number)%z0

! compute entrainment flow

	   call entrain(fireroom,fire_flow,entrain_flow,1)
	   plume_flow = fire_flow + entrain_flow
   end do

end subroutine getfires

! --------------- setfireflow ----------------------

subroutine setfireflow(tsec)
  use precision
  use zonedata
  implicit none
  real(kind=dd), intent(in) :: tsec
  real(kind=dd) :: qtotal, o2index
  type(fire_data), pointer :: fire
  type(flow_data), pointer :: fire_flow
  integer :: ifire


  do ifire = 1, nfires
    fire => fires(ifire)
    fire_flow => fire%fire_flow

	  if(fire%type.eq.constant)then
      qtotal = fire%q_pyrol(1)
     elseif(fire%type.eq.tsquared)then
     elseif(fire%type.eq.general)then
      call interp(tsec,fire%times,fire%q_pyrol,fire%npoints,qtotal)
    endif
    fire%qtotal = qtotal
#ifdef pp_solveoxy
    o2index=rooms(fire%room_number)%layer(upper)%o2index
    qtotal = qtotal*o2index
    fire%qtotal = qtotal
#endif
    fire%qconvec = (1.0_dd - fire%chi_rad)*qtotal
    fire%qrad = fire%chi_rad*qtotal
    fire%mtotal = qtotal/fire%heat_c

    fire_flow%mdot = fire%mtotal
    fire_flow%qdot = fire%qconvec
    fire_flow%temperature = fire%qconvec/cp/fire%mtotal
    fire_flow%density = pabs_ref/rgas/fire_flow%temperature
  end do

  return
end subroutine setfireflow

! --------------- entrain ----------------------

subroutine entrain(flowroom,source_flow,entrain_flow,flowtype)
  use precision
  use zonedata
  implicit none

  type(room_data), pointer :: flowroom
  type(flow_data), pointer :: source_flow, entrain_flow
  integer, intent(in) :: flowtype

  real(kind=dd) :: qsource,zlength
  real(kind=dd) :: mentrain, qentrain
  type(zone_data), pointer :: entrainsourcelayer
  real(kind=dd) :: tsource, tentrain, zstar

  zlength = flowroom%rel_layer_height - source_flow%rel_height
  if(zlength.gt.0)then
    entrainsourcelayer => flowroom%layer(lower)
    entrain_flow%fromlower = .true.
    entrain_flow%fromupper = .false.
   else
    entrainsourcelayer => flowroom%layer(upper)
    entrain_flow%fromlower = .false.
    entrain_flow%fromupper = .true.
  endif
  tentrain = entrainsourcelayer%temperature
  tsource = source_flow%temperature
  if(flowtype.eq.1)then
    qsource = source_flow%qdot
   else
    qsource = abs(cp*(tsource-tentrain)*source_flow%mdot)
  endif
  entrain_flow%zeroflowflag = .true.
  qentrain = zero
  entrain_flow%zeroflowflag = .true.
  if(entrain_flow%fromlower.and.tsource.le.tentrain+5.0_dd)return
  if(entrain_flow%fromupper.and.tsource+5.0_dd.ge.tentrain)return
  if(zlength.eq.0.0_dd)return
  zlength = abs(zlength)
  entrain_flow%zeroflowflag = .false.
  zstar = abs(zlength/(qsource/1000.0_dd)**(0.4_dd))
  if(zstar.lt.0.08_dd)then
    mentrain = 0.011_dd*zstar**0.566_dd
   else if(zstar.ge.0.08_dd.and.zstar.lt.0.20_dd)then
    mentrain = 0.026_dd*zstar**0.909_dd
   else
    mentrain = 0.124_dd*zstar**1.895_dd
  endif
  mentrain = mentrain*qsource/1000.0_dd
  qentrain = cp*mentrain*tentrain
  entrain_flow%mdot = mentrain
  entrain_flow%qdot = qentrain
  entrain_flow%temperature = tentrain
  entrain_flow%density=flowroom%abs_pressure/(rgas*tentrain)
  entrain_flow%sdot(1:nspecies) = entrainsourcelayer%s_con(1:nspecies)*mentrain
end subroutine entrain


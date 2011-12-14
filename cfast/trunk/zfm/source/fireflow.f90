
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
    fflow(iroom,lower) = fflow(iroom,lower) - fire%entrainl_flow
    fflow(iroom,upper) = fflow(iroom,upper) + fire%plume_flow
  end do
end subroutine fireflow

! --------------- getfires ----------------------

subroutine getfires
  use precision
  use zonedata
  use zoneinterfaces
  implicit none
  integer :: ifire
  type(fire_data), pointer :: fire
  type(room_data), pointer :: fireroom
  type(flow_data), pointer :: fire_flow, entrainl_flow, entrainu_flow, plume_flow
  real(kind=dd), pointer :: mfire, qfire
  real(kind=dd) :: qqfire, qqofire
  real(kind=dd) :: tentrain, zlength, qtotal
  real(kind=dd) :: o2frac, o2index, o2ufrac, o2uindex
! compute fire flow

   do ifire = 1, nfires
     fire => fires(ifire)
     mfire => fire%fire_flow%mdot
     qfire => fire%fire_flow%qdot
	   fireroom => rooms(fire%room_number)
	   tentrain = fireroom%layer(lower)%temperature
	   zlength = fireroom%rel_layer_height
	   fire_flow => fire%fire_flow
	   entrainl_flow => fire%entrainl_flow
	   entrainu_flow => fire%entrainu_flow
	   plume_flow => fire%plume_flow

	   if(mfire.eq.0.0_dd)then
       fire_flow%zero = .true.
       entrainl_flow%zero = .true.
       entrainu_flow%zero = .true.
       plume_flow%zero = .true.
	     cycle
	    else
       fire_flow%zero = .false.
       entrainl_flow%zero = .false.
       entrainu_flow%zero = .false.
       plume_flow%zero = .false.
	   endif

	   fire_flow%rel_height = fire%z0
	   fire_flow%abs_height = fire%z0 + rooms(fire%room_number)%z0

! compute entrainment flow

	   call entrain(fireroom,fire_flow,entrainl_flow,entrainu_flow,p_fireflow)
     if(solveoxy.and.(.not.entrainl_flow%zero.or..not.entrainu_flow%zero))then
       qqfire = fire_flow%qtotal
       o2frac=entrainl_flow%sdot(oxygen)/entrainl_flow%mdot
       o2ufrac=fireroom%layer(upper)%s_con(oxygen)

       o2index = 0.50_dd*(tanh(800.0_dd*(o2frac-o2limit)-4.0_dd)+1.0_dd)
       o2uindex = 0.50_dd*(tanh(800.0_dd*(o2ufrac-o2limit)-4.0_dd)+1.0_dd)
       qqofire = heat_o2*entrainl_flow%sdot(oxygen)*o2index
       qqofire = qqofire + heat_o2*entrainu_flow%sdot(oxygen)*o2uindex
       qtotal = min(qqfire,qqofire)
       if(qqofire.lt.qqfire)then
         call setfire(qtotal,ifire)
         call entrain(fireroom,fire_flow,entrainl_flow,entrainu_flow,p_fireflow)
         qqfire = fire_flow%qtotal
         qqofire = heat_o2*entrainl_flow%sdot(oxygen)*o2index
         qqofire = qqofire + heat_o2*entrainu_flow%sdot(oxygen)*o2uindex
         qtotal = min(qqfire,qqofire)
         call setfire(qtotal,ifire)
       endif
       fire_flow%sdot(oxygen) = -fire%qtotal/heat_o2
     endif
     fire_flow%temperature = qfire/(cp*mfire)
     fire_flow%density=fireroom%abs_pressure/(rgas*fire_flow%temperature)
	   plume_flow = fire_flow + entrainl_flow
   end do

end subroutine getfires

! --------------- setfireflow ----------------------

subroutine setfireflow(tsec)
  use precision
  use zonedata
  implicit none
  real(kind=dd), intent(in) :: tsec
  real(kind=dd) :: qtotal
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
    call setfire(qtotal,ifire)
  end do

  return
end subroutine setfireflow

! --------------- setfire ----------------------

subroutine setfire(qtotal,ifire)
  use precision
  use zonedata
  implicit none

  real(kind=dd), intent(in) :: qtotal
  integer, intent(in) :: ifire

  type(fire_data), pointer :: fire
  type(flow_data), pointer :: fire_flow

  fire => fires(ifire)
  fire_flow => fire%fire_flow


  fire%qtotal = qtotal
  fire%qconvec = (1.0_dd - fire%chi_rad)*qtotal
  fire%qrad = fire%chi_rad*qtotal
  fire%mtotal = qtotal/fire%heat_c

  fire_flow%mdot = fire%mtotal
  fire_flow%qdot = fire%qconvec
  fire_flow%qtotal = fire%qtotal
  fire_flow%temperature = fire%qconvec/cp/fire%mtotal
  fire_flow%density = pabs_ref/rgas/fire_flow%temperature

  return
end subroutine setfire

! --------------- entrain ----------------------

subroutine entrain(flowroom,source_flow,entrainl_flow,entrainu_flow,flowtype)
  use precision
  use zonedata
  use zoneinterfaces
  implicit none

  type(room_data), pointer :: flowroom
  type(flow_data), pointer :: source_flow, entrainl_flow, entrainu_flow
  integer, intent(in) :: flowtype

  real(kind=dd) :: qsource,msource,zl,zu
  real(kind=dd) :: mentrain, maxentrain, qentrain, muentrain, quentrain
  type(zone_data), pointer :: entrainsourcelayer, entraindestlayer
  real(kind=dd) :: tsource, tdest, tentrain, tuentrain, oentrain
  real(kind=dd) :: base, exponent, baseu, exponentu, oxysource

  zl = flowroom%rel_layer_height - source_flow%rel_height
  if(zl.gt.0)then
    entrainsourcelayer => flowroom%layer(lower)
    entraindestlayer => flowroom%layer(upper)
    entrainl_flow%fromlower = .true.
    entrainl_flow%fromupper = .false.
   else
    entrainsourcelayer => flowroom%layer(upper)
    entraindestlayer => flowroom%layer(lower)
    entrainl_flow%fromlower = .false.
    entrainl_flow%fromupper = .true.
  endif
  tentrain = entrainsourcelayer%temperature
  tdest = entraindestlayer%temperature
  tsource = source_flow%temperature

  if(flowtype.eq.p_fireflow)then
    qsource = source_flow%qdot
    msource = source_flow%mdot
    if(solveoxy)oxysource = entrainsourcelayer%s_con(oxygen)
   else
    qsource = abs(cp*(tsource-tentrain)*source_flow%mdot)
  endif

  entrainl_flow%zero = .true.
  if(flowtype.eq.p_fireflow)entrainu_flow%zero = .true.
  qentrain = zero
  entrainl_flow%zero = .true.
  if(entrainl_flow%fromlower.and.tsource.le.tentrain+5.0_dd)return
  if(entrainl_flow%fromupper.and.tsource+5.0_dd.ge.tentrain)return
  if(flowtype.eq.p_ventflow.and.zl.eq.0.0_dd)return

  if(flowtype.eq.p_fireflow.and.zl.lt.0.0_dd)zl=0.0_dd  ! fire flow only goes up
                                      !
  ! lower layer entrainment

  zl = abs(zl)
  entrainl_flow%zero = .false.
  call entrainfl(zl,qsource,mentrain,base,exponent)

  ! limit entrainment so that plume flow will exceed destination layer temperature

  ! tplume > tdest ==> cp*mp*tp=cp*(ms+me)*tp=qs+cp*me*te
  !                    tp=(qs+cp*me*te)/(cp*(ms+me))>td  
  ! solve for me where                                   
  ! qs==qsource, me==mentrain, te=tentrain, td=tdest     

  if(tdest.ne.tentrain)then 
    maxentrain = abs((qsource/cp-msource*tdest)/(tdest-tentrain))
    if(maxentrain.lt.mentrain)then
      mentrain = maxentrain
    endif
  endif

  qentrain = cp*mentrain*tentrain
  entrainl_flow%mdot = mentrain
  entrainl_flow%qdot = qentrain
  entrainl_flow%qtotal = qentrain
  if(solveoxy)then
    oentrain = mentrain*oxysource
    entrainl_flow%sdot(oxygen)=oentrain
  endif
  entrainl_flow%temperature = tentrain
  entrainl_flow%density=flowroom%abs_pressure/(rgas*tentrain)
  entrainl_flow%sdot(1:nspecies) = entrainsourcelayer%s_con(1:nspecies)*mentrain

  ! upper layer entrainment (only if this is a fire plume ande solveoxy is true)

  if(solveoxy.and.flowtype.eq.p_fireflow)then
    zu = flowroom%dz - source_flow%rel_height
    entrainu_flow%zero = .false.
    call entrainfl(zu,qsource,muentrain,baseu,exponentu)
    muentrain = muentrain - mentrain  ! only interested in net upper layer entrainment
    tuentrain = flowroom%layer(upper)%temperature
    quentrain = cp*muentrain*tuentrain
    entrainu_flow%mdot = muentrain
    entrainu_flow%qdot = quentrain
    entrainu_flow%qtotal = quentrain
    entrainu_flow%sdot(oxygen)=muentrain*flowroom%layer(upper)%s_con(oxygen)
    entrainu_flow%temperature = tuentrain
    entrainu_flow%density=flowroom%abs_pressure/(rgas*tuentrain)
    entrainu_flow%sdot(1:nspecies) = flowroom%layer(upper)%s_con(1:nspecies)*muentrain
  endif

end subroutine entrain

! --------------- entrainfl ----------------------

subroutine entrainfl(zlength,qsource,mentrain,base,exponent)
  use precision
  implicit none

  real(kind=dd), intent(in) :: zlength, qsource
  real(kind=dd), intent(out) :: mentrain,base,exponent

  real(kind=dd) :: zstar
  real(kind=dd), parameter :: factor=1000.0_dd**(0.4_dd)
  real(kind=dd) :: base2, exponent2

  zstar = abs(zlength/(qsource/1000.0_dd)**(0.4_dd))
  if(zstar.lt.0.08_dd)then
    exponent2 = 0.566_dd
    base2 = 0.011_dd
   else if(zstar.ge.0.08_dd.and.zstar.lt.0.20_dd)then
    exponent2 = 0.909_dd
    base2 = 0.026_dd
   else
    exponent2 = 1.895_dd
    base2 = 0.124_dd
  endif
  mentrain = (qsource/1000.0_dd)*base2*zstar**exponent2
  base = base2*(factor*abs(zlength))**exponent2/1000.0_dd
  exponent = 1.0_dd - 0.4_dd*exponent2 
  ! mentrain = base*qsource**exponent = (qsource/1000)*base2*zstar**exponent2
  return
end subroutine entrainfl


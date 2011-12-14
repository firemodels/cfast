
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
    fflow(iroom,upper) = fflow(iroom,upper) + fire%entrain_flow + fire%fire_flow
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
  type(flow_data), pointer :: fire_flow, entrain_flow, plume_flow
  real(kind=dd), pointer :: mfire, qfire
  real(kind=dd) :: tentrain, zlength
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
       fire_flow%zero = .true.
       entrain_flow%zero = .true.
       plume_flow%zero = .true.
	     cycle
	   endif

     fire_flow%zero = .false.
     entrain_flow%zero = .false.
     plume_flow%zero = .false.

	   fire_flow%rel_height = fire%z0
	   fire_flow%abs_height = fire%z0 + rooms(fire%room_number)%z0

! compute entrainment flow

	   call f_entrain(fireroom,fire)
	   plume_flow = fire_flow + entrain_flow
   end do

end subroutine getfires

! --------------- setfireflow ----------------------

subroutine setfireflow(tsec)
  use precision
  use zonedata
  use zoneinterfaces
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
    call setfire(qtotal,fire)
  end do

  return
end subroutine setfireflow

! --------------- setfire ----------------------

subroutine setfire(qtotal,fire)
  use precision
  use zonedata
  implicit none

  real(kind=dd), intent(in) :: qtotal

  type(fire_data), pointer :: fire
  type(flow_data), pointer :: fire_flow

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
  if(solveoxy)fire_flow%sdot(oxygen) = -qtotal/heat_o2

  return
end subroutine setfire

! --------------- entrain ----------------------

subroutine f_entrain(flowroom,fire)
  use precision
  use zonedata
  use zoneinterfaces
  implicit none

  type(room_data), pointer :: flowroom
  type(flow_data), pointer :: fire_flow, entrain_flow
  type(fire_data), pointer :: fire

  real(kind=dd) :: qtotal,qfire,mfire,zl,zu
  real(kind=dd) :: mentrain, maxentrain, qentrain, muentrain
  type(zone_data), pointer :: lowerlayer, upperlayer
  real(kind=dd) :: tfire, tupper, tentrain
  real(kind=dd) :: oxyl, oxyu
  real(kind=dd) :: qtotal_oxy, qfire_new, relerror, dmldq, dmudq
  integer :: i, numiters

  fire_flow => fire%fire_flow
  entrain_flow => fire%entrain_flow
  zl = max(0.0_dd,flowroom%rel_layer_height - fire_flow%rel_height)
  zu = flowroom%dz - fire_flow%rel_height

  entrain_flow%fromlower = .true.
  entrain_flow%fromupper = .false.
  lowerlayer => flowroom%layer(lower)
  upperlayer => flowroom%layer(upper)

  tentrain = lowerlayer%temperature
  tupper = upperlayer%temperature
  tfire = fire_flow%temperature

  qfire = fire_flow%qdot
  mfire = fire_flow%mdot

  entrain_flow%zero = .false.
  call entrainfl(zl,qfire,mentrain,dmldq)
  numiters = 0
  if(solveoxy)then
    oxyl = lowerlayer%s_con(oxygen)
    oxyl = oxyl*0.50_dd*(tanh(800.0_dd*(oxyl-o2limit)-4.0_dd)+1.0_dd)
    qtotal = fire_flow%qtotal
    qtotal_oxy = heat_o2*oxyl*mentrain
    if(qtotal_oxy.lt.qtotal)then
      oxyu = upperlayer%s_con(oxygen)
      oxyu = oxyu*0.50_dd*(tanh(800.0_dd*(oxyu-o2limit)-4.0_dd)+1.0_dd)
      call entrainfl(zu,qfire,muentrain,dmudq)
      muentrain = muentrain - mentrain  
      qtotal_oxy = heat_o2*(oxyl*mentrain + oxyu*muentrain)
      qfire_new = (1.0_dd-fire%chi_rad)*qtotal_oxy
      if(qtotal_oxy.lt.qtotal)then
        do i = 1, 5
          numiters = i
          qfire = qfire_new
          call entrainfl(zl,qfire,mentrain,dmldq)
          call entrainfl(zu,qfire,muentrain,dmudq)
          muentrain = muentrain - mentrain
          dmudq = dmudq - dmldq  
          qtotal_oxy = heat_o2*(oxyl*mentrain+oxyu*muentrain) 
          qtotal = qtotal_oxy
          qfire_new = (1.0_dd-fire%chi_rad)*qtotal_oxy  
          qfire_new = qfire - (qfire-qfire_new)/&
          (1.0_dd - heat_o2*(1.0_dd-fire%chi_rad)*(oxyl*dmldq - oxyu*dmudq))
          if(qfire_new.eq.0.0)exit
          relerror = abs((qfire_new-qfire)/qfire_new)
          if(relerror.lt.0.0001_dd)exit
        end do
      endif
    endif
    call setfire(qtotal,fire)
  endif


  ! limit entrainment so that plume flow will exceed destination layer temperature

  ! tplume > tupper ==> cp*mp*tp=cp*(ms+me)*tp=qs+cp*me*te
  !                    tp=(qs+cp*me*te)/(cp*(ms+me))>td  
  ! solve for me where                                   
  ! qs==qfire, me==mentrain, te=tentrain, td=tupper     

!  if(tupper.ne.tentrain)then 
!    maxentrain = abs((qfire/cp-mfire*tupper)/(tupper-tentrain))
!    if(maxentrain.lt.mentrain)then
!      mentrain = maxentrain
!    endif
!  endif

  qentrain = cp*mentrain*tentrain
  entrain_flow%mdot = mentrain
  entrain_flow%qdot = qentrain
  entrain_flow%qtotal = qentrain

end subroutine f_entrain



! --------------- entrain ----------------------

subroutine v_entrain(flowroom,source_flow,entrain_flow)
  use precision
  use zonedata
  use zoneinterfaces
  implicit none

  type(room_data), pointer :: flowroom
  type(flow_data), pointer :: source_flow, entrain_flow

  real(kind=dd) :: qsource,zl
  real(kind=dd) :: mentrain, qentrain
  type(zone_data), pointer :: entrainsourcelayer, entraindestlayer
  real(kind=dd) :: tsource, tdest, tentrain
  real(kind=dd) :: oxysource, dmdq

  zl = flowroom%rel_layer_height - source_flow%rel_height
  if(zl.gt.0)then
    entrainsourcelayer => flowroom%layer(lower)
    entraindestlayer => flowroom%layer(upper)
    entrain_flow%fromlower = .true.
    entrain_flow%fromupper = .false.
   else
    entrainsourcelayer => flowroom%layer(upper)
    entraindestlayer => flowroom%layer(lower)
    entrain_flow%fromlower = .false.
    entrain_flow%fromupper = .true.
  endif
  tentrain = entrainsourcelayer%temperature
  tdest = entraindestlayer%temperature
  tsource = source_flow%temperature
  if(solveoxy)oxysource = entrainsourcelayer%s_con(oxygen)
  qsource = abs(cp*(tsource-tentrain)*source_flow%mdot)

  entrain_flow%zero = .true.
  qentrain = zero
  entrain_flow%zero = .true.
  if(entrain_flow%fromlower.and.tsource.le.tentrain+5.0_dd)return
  if(entrain_flow%fromupper.and.tsource+5.0_dd.ge.tentrain)return
  if(zl.eq.0.0_dd)return

                                      !
  ! lower layer entrainment

  zl = abs(zl)
  entrain_flow%zero = .false.
  call entrainfl(zl,qsource,mentrain,dmdq)

  qentrain = cp*mentrain*tentrain
  entrain_flow%mdot = mentrain
  entrain_flow%qdot = qentrain
  entrain_flow%qtotal = qentrain
  if(solveoxy)entrain_flow%sdot(oxygen)=mentrain*oxysource
  entrain_flow%temperature = tentrain
  entrain_flow%density=flowroom%abs_pressure/(rgas*tentrain)
  entrain_flow%sdot(1:nspecies) = entrainsourcelayer%s_con(1:nspecies)*mentrain

  ! upper layer entrainment (only if this is a fire plume ande solveoxy is true)

end subroutine v_entrain


! --------------- entrainfl ----------------------

subroutine entrainfl(zlength,qsource,mentrain,dmdq)
  use precision
  implicit none

  real(kind=dd), intent(in) :: zlength, qsource
  real(kind=dd), intent(out) :: mentrain, dmdq
  real(kind=dd) :: base,exponent

  real(kind=dd) :: zstar
  real(kind=dd), parameter :: factor=1000.0_dd**(0.4_dd)
  real(kind=dd) :: base2, exponent2

  mentrain = 0.0_dd
  if(qsource.eq.0.0_dd)return
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
  dmdq = base*exponent*qsource**(exponent-1.0_dd)
  return
end subroutine entrainfl


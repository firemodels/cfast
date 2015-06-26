
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
  type(flow_data), pointer :: fire_flow, entrain_flow
  real(kind=eb), pointer :: mfire

! compute fire flow

   do ifire = 1, nfires
     fire => fires(ifire)
     mfire => fire%fire_flow%mdot
	   fireroom => rooms(fire%room_number)
	   fire_flow => fire%fire_flow
	   entrain_flow => fire%entrain_flow

	   if(mfire.eq.0.0_eb)then
       fire_flow%zero = .true.
       entrain_flow%zero = .true.
	     cycle
	   endif

     fire_flow%zero = .false.
     entrain_flow%zero = .false.

	   fire_flow%rel_height = fire%z0
	   fire_flow%abs_height = fire%z0 + fireroom%z0

! compute entrainment flow

	   call f_entrain(fireroom,fire)
   end do

end subroutine getfires

! --------------- setfireflow ----------------------

subroutine setfireflow(tsec)
  use precision
  use zonedata
  use zoneinterfaces
  implicit none
  real(kind=eb), intent(in) :: tsec
  real(kind=eb) :: qtotal
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

  real(kind=eb), intent(in) :: qtotal

  type(fire_data), pointer :: fire
  type(flow_data), pointer :: fire_flow
  integer :: ispec

  fire_flow => fire%fire_flow


  fire%qtotal = qtotal
  fire%qconvec = (1.0_eb - fire%chi_rad)*qtotal
  fire%qrad = fire%chi_rad*qtotal
  fire%mtotal = qtotal/fire%heat_c

  fire_flow%mdot = fire%mtotal
  fire_flow%qdot = fire%qconvec
  fire_flow%qtotal = fire%qtotal
  fire_flow%temperature = fire%qconvec/cp/fire%mtotal
  fire_flow%density = pabs_ref/rgas/fire_flow%temperature
  if(solveoxy)fire_flow%sdot(oxygen) = -qtotal/heat_o2
  if(solveprods)then
    do ispec = 2, maxspecies
      fire_flow%sdot(ispec) = qtotal/fire%heat_c*fire%yield(ispec)
    end do
  endif

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

  real(kind=eb) :: qtotal,qfire,mfire,zl,zu
  real(kind=eb) :: mentrain, qentrain, muentrain
  type(zone_data), pointer :: lowerlayer, upperlayer
  real(kind=eb) :: tfire, tupper, tentrain
  real(kind=eb) :: oxyl, oxyu
  real(kind=eb) :: qtotal_oxy, qfire_new, relerror, dmldq, dmudq, factor
  integer :: i

  fire_flow => fire%fire_flow
  entrain_flow => fire%entrain_flow
  zl = max(0.0_eb,flowroom%rel_layer_height - fire_flow%rel_height)
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
  qtotal = fire_flow%qtotal

  entrain_flow%zero = .false.
  factor = -1.0_eb
  if(.not.solveoxy)call entrainfl(zl,qfire,mentrain,dmldq,factor)
  if(solveoxy)then
    oxyl = lowerlayer%s_con(oxygen)
    oxyl = oxyl*(tanh(800.0_eb*(oxyl-o2limit)-4.0_eb)+1.0_eb)/2.0_eb
    oxyu = upperlayer%s_con(oxygen)
    oxyu = oxyu*(tanh(800.0_eb*(oxyu-o2limit)-4.0_eb)+1.0_eb)/2.0_eb
    factor = (1.0_eb-fire%chi_rad)*heat_o2*oxyl

    call entrainfl(zl,qfire,mentrain,dmldq,factor)
    qtotal_oxy = heat_o2*oxyl*mentrain
    dmudq = 0.0_eb
    if(oxyu.gt.0.0_eb)then
      factor = -1.0_eb
      call entrainfl(zu,qfire,muentrain,dmudq,factor)
      muentrain = muentrain - mentrain  
      dmudq = dmudq - dmldq  
      qtotal_oxy = qtotal_oxy + heat_o2*oxyu*muentrain
    endif

    factor = -1.0_eb
    qfire_new = (1.0_eb-fire%chi_rad)*qtotal_oxy
    relerror=0.0_eb
    if(qfire_new.ne.0.0_eb)relerror = abs( (qfire_new-qfire)/qfire_new )
    if(qtotal_oxy.lt.qtotal)then
      do i = 1, 5
        qfire = qfire_new
        if(relerror.lt.0.000001_eb)then
          if((1.0_eb-fire%chi_rad).ne.0.0_eb)qtotal = qfire/(1.0_eb-fire%chi_rad)
          exit
        endif

        factor = -1.0_eb
        call entrainfl(zl,qfire,mentrain,dmldq,factor)
        qtotal_oxy = heat_o2*oxyl*mentrain
        dmudq = 0.0_eb
        if(oxyu.gt.0.0_eb)then
          factor = -1.0_eb
          call entrainfl(zu,qfire,muentrain,dmudq,factor)
          muentrain = muentrain - mentrain  
          dmudq = dmudq - dmldq  
          qtotal_oxy = qtotal_oxy + heat_o2*oxyu*muentrain
        endif

        qfire_new = (1.0_eb-fire%chi_rad)*qtotal_oxy  

        qtotal = qtotal_oxy
        qfire_new = qfire - (qfire-qfire_new)/(1.0_eb - heat_o2*(1.0_eb-fire%chi_rad)*(oxyl*dmldq + oxyu*dmudq))
        if(qfire_new.lt.0.0)then
          qfire_new = 0.0_eb
          cycle
        endif
        relerror = abs((qfire_new-qfire)/qfire_new)
      end do
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

  entrain_flow%temperature = tentrain
  entrain_flow%density=flowroom%abs_pressure/(rgas*tentrain)
  entrain_flow%sdot(1:nspecies) = lowerlayer%s_con(1:nspecies)*mentrain

  return
end subroutine f_entrain



! --------------- entrain ----------------------

subroutine v_entrain(flowroom,source_flow,entrain_flow)
  use precision
  use zonedata
  use zoneinterfaces
  implicit none

  type(room_data), pointer :: flowroom
  type(flow_data), pointer :: source_flow, entrain_flow

  real(kind=eb) :: qsource,zl
  real(kind=eb) :: mentrain, qentrain
  type(zone_data), pointer :: entrainsourcelayer, entraindestlayer
  real(kind=eb) :: tsource, tdest, tentrain
  real(kind=eb) :: oxysource, dmdq, factor

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
!  if(solveoxy)oxysource = entrainsourcelayer%s_con(oxygen)
  qsource = abs(cp*(tsource-tentrain)*source_flow%mdot)

  entrain_flow%zero = .true.
  qentrain = zero
  entrain_flow%zero = .true.
  if(entrain_flow%fromlower.and.tsource.le.tentrain+5.0_eb)return
  if(entrain_flow%fromupper.and.tsource+5.0_eb.ge.tentrain)return
  if(zl.eq.0.0_eb)return

                                      !
  ! lower layer entrainment

  zl = abs(zl)
  entrain_flow%zero = .false.
  factor = -1.0_eb
  call entrainfl(zl,qsource,mentrain,dmdq,factor)

  qentrain = cp*mentrain*tentrain
  entrain_flow%mdot = mentrain
  entrain_flow%qdot = qentrain
  entrain_flow%qtotal = qentrain
 ! if(solveoxy)entrain_flow%sdot(oxygen)=mentrain*oxysource
  entrain_flow%temperature = tentrain
  entrain_flow%density=flowroom%abs_pressure/(rgas*tentrain)
  entrain_flow%sdot(1:nspecies) = entrainsourcelayer%s_con(1:nspecies)*mentrain

  return
end subroutine v_entrain


! --------------- entrainfl ----------------------

subroutine entrainfl(zlength,qsource,mentrain,dmdq,factor)
  use precision
  implicit none

  real(kind=eb), intent(in) :: zlength, factor
  real(kind=eb), intent(inout) :: qsource
  real(kind=eb), intent(out) :: mentrain, dmdq
  real(kind=eb) :: base,exponent

  real(kind=eb) :: zstar
  real(kind=eb), parameter :: factor2=1000.0_eb**(0.4_eb)
  real(kind=eb) :: base2, exponent2

  mentrain = 0.0_eb
  if(qsource.eq.0.0_eb)return
  if(qsource.lt.0.0_eb)then
    write(6,*)"qsource negative in f_entrain"
  endif
  zstar = abs(zlength/(qsource/1000.0_eb)**(0.4_eb))
  if(zstar.lt.0.08_eb)then
    exponent2 = 0.566_eb
    base2 = 0.011_eb
   else if(zstar.ge.0.08_eb.and.zstar.lt.0.20_eb)then
    exponent2 = 0.909_eb
    base2 = 0.026_eb
   else
    exponent2 = 1.895_eb
    base2 = 0.124_eb
  endif
  mentrain = (qsource/1000.0_eb)*base2*zstar**exponent2
  base = base2*(factor2*abs(zlength))**exponent2/1000.0_eb
  exponent = 1.0_eb - 0.4_eb*exponent2 
  if(factor.gt.0.0_eb.and.qsource.gt.0.0_eb)then
    qsource = (factor*base)**(1.0_eb/(1.0_eb-exponent))
    mentrain = base*qsource**exponent
  endif
  ! mentrain = base*qsource**exponent = (qsource/1000)*base2*zstar**exponent2
  if(qsource.le.0.0_eb)then
    dmdq = 0.0_eb
   else
    dmdq = base*exponent*qsource**(exponent-1.0_eb)
  endif
  return
end subroutine entrainfl


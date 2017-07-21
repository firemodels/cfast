module conversion_routines

    use precision_parameters

    use wallptrs
    use cenviro
    use ramp_data
    use cparams
    use setup_data
    use detectorptrs
    use target_data
    use fire_data
    use solver_data
    use smkview_data
    use thermal_data
    use vent_data
    use room_data
    use namelist_data

    implicit none
      
    
    contains    
    ! --------------------------- namelist_input ----------------------------------
    subroutine namelist_conversion(aversion,ivers)
      
    implicit none
    
    integer :: ivers
    character :: aversion*5
    integer :: i

    if (versnflag) call versncon(aversion,ivers)
    if (stpmaflag) call stpmacon
    if (ctimeflag) call ctimecon
    if (tambiflag) call tambicon
    if (eambiflag) call eambicon
    if (limo2flag) call limo2con
    if (furncflag) call furnccon
    if (dtcheflag) call dtchecon
    if (adiabflag) call adiabcon
    if (matrlflag) then
        do i=1,n_thrmp
            call matrlcon(i)
        end do
    end if
    if (compaflag) then
        do i=1,nr-1
            call compacon(i)
        end do
    end if
    if (targeflag) then
        do i=1,n_targets
            call targecon(i)
        end do
    end if
    if (cfireflag) then
        do i=1,n_fires
            call cfirecon(i)
        end do
    end if
    if (hventflag) then
        do i=1,n_hvents
            call hventcon(i)
        end do
    end if
    if (vventflag) then
        do i=1,n_vvents
            call vventcon(i)
        end do
    end if
    if (mventflag) then
        do i=1,n_mvents
            call mventcon(i)
        end do
    end if
    if (crampflag) then
        do i=1,nramps
            call crampcon(i)
        end do
    end if
    if (event_hflag .or. event_vflag .or. event_mflag .or. event_fflag) call eventcon
    if (detecflag) then 
        do i=1,n_detectors
            call deteccon(i)
        end do
    end if
    if (hheatflag) then 
        do i=1,nr-1
            call hheatcon(i)
        end do
    end if
    if (vheatflag) then 
        do i=1,nvcons
            call vheatcon(i)
        end do
    end if
    if (conezflag) then 
        do i=1,nr-1
            call conezcon(i)
        end do
    end if
    if (challflag) then 
        do i=1,nr-1
            call challcon(i)
        end do
    end if
    if (deadrflag) then
        do i=1,nr-1
            call deadrcon(i)
        end do
    end if
    if (roomaflag) then 
        do i=1,nr-1
            call roomacon(i)
        end do
    end if
    if (roomhflag) then 
        do i=1,nr-1
            call roomhcon(i)
        end do
    end if
    if (cslcfflag)then 
        do i=1,nvisualinfo
            call cslcfcon(i)
        end do
    end if
    if (cisofflag)then 
        do i=1,nvisualinfo
            call cisofcon(i)
        end do
    end if


    end subroutine namelist_conversion
    
    
    ! --------------------------- versncon --------------------------------------
    subroutine versncon(aversion,ivers)

    integer :: ivers
    character :: aversion*5
      
    c1=len_trim(title)
    call versnprint(aversion,ivers)    
  

    contains
  
    subroutine versnprint(aversion,ivers)  
        
    character(5) :: head
    integer :: ivers
    character :: aversion*5
    character(c1) :: pgrm_name
    namelist /VERSN/ head,ivers,pgrm_name
      
    head=aversion
    pgrm_name=title
    write (31,VERSN)
  
    end subroutine versnprint
    end subroutine versncon

    
    ! --------------------------- stpmacon --------------------------------------
    subroutine stpmacon
   
    call stpmaprint    
  

    contains  
    subroutine stpmaprint  
        
    real(eb) :: stepmax
    namelist /STPMA/stepmax

    stepmax=stpmax
    write (31,'(/)')
    write (31,STPMA)
  
    end subroutine stpmaprint
    end subroutine stpmacon

    
    ! --------------------------- matrlcon --------------------------------------
    subroutine matrlcon(i)
    
    integer :: i
    type(thermal_type), pointer :: thrmpptr
    
    thrmpptr => thermalinfo(i)
    
    c1=len_trim(thrmpptr%name)
   
    call matrlprint(i)

    contains  
    subroutine matrlprint(i)
        

    integer :: i,nslab
    real(eb) :: c,eps,k,rho,thickness
    character(c1) :: matrl_id
    namelist /MATRL/matrl_id,k,c,eps,nslab,rho,thickness
    
    type(thermal_type), pointer :: thrmpptr
    
    thrmpptr => thermalinfo(i)
  
    matrl_id=thrmpptr%name
    nslab=thrmpptr%nslab         
    k=thrmpptr%k(1)
    c=thrmpptr%c(1)
    rho=thrmpptr%rho(1)
    thickness=thrmpptr%thickness(1)
    eps=thrmpptr%eps

    write (31,'(/)')
    write (31,MATRL)
         
    end subroutine matrlprint
    end subroutine matrlcon

    
    ! --------------------------- compacon --------------------------------------
    subroutine compacon(i)
    
    integer :: i
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    
    c1=len_trim(roomptr%name)
    c2=len_trim(roomptr%matl(1))
    c3=len_trim(roomptr%matl(2))
    c4=len_trim(roomptr%matl(3))
   
    call compaprint(i)

    contains  
    subroutine compaprint(i)
        
    integer :: i,xgrid,ygrid,zgrid
    real(eb) :: depth,width,height,x0,y0,z0
    character(c1) :: compa_id
    character(c2) :: ceiling
    character(c3) :: floor
    character(c4) :: wall
    namelist /COMPA/ceiling,compa_id,depth,floor,height,wall,width,x0,xgrid,y0,ygrid,z0,zgrid
    
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
  
    compa_id=roomptr%name
    width=roomptr%cwidth
    depth=roomptr%cdepth
    height=roomptr%cheight
    x0=roomptr%x0
    y0=roomptr%y0
    z0=roomptr%z0
    ceiling=roomptr%matl(1)
    floor=roomptr%matl(2)
    wall=roomptr%matl(3)
    xgrid=roomptr%ibar
    ygrid=roomptr%jbar
    zgrid=roomptr%kbar

    write (31,'(/)')
    write (31,COMPA)
         
    end subroutine compaprint
    end subroutine compacon

    
    ! --------------------------- targecon --------------------------------------
    subroutine targecon(i)
    
    integer :: i
    character(64) :: tcname
    type(target_type), pointer :: targptr
    
    targptr => targetinfo(i)
    
    c1=len_trim(targptr%name)
    c2=len_trim('EXPLICIT')
    c3=len_trim(targptr%material)
    
    if (targptr%equaton_type==1) tcname='PDE'
    if (targptr%equaton_type==2) tcname='CYL'
    c4=len_trim(tcname)
   
    call targeprint(i,tcname)

    contains  
    subroutine targeprint(i,tcname)

    character(64) :: tcname
    integer :: i,compartment
    real(eb) :: x,y,z,internallocation,normalx,normaly,normalz
    character(c1) :: targe_id
    character(c2) :: method
    character(c3) :: material
    character(c4) :: equationtype
    namelist /TARGE/compartment,x,y,z,normalx,normaly,normalz,material,method,equationtype,internallocation,targe_id
    
    type(target_type), pointer :: targptr
    
    targptr => targetinfo(i)
  
    compartment=targptr%room
    x=targptr%center(1)
    y=targptr%center(2)
    z=targptr%center(3)
    normalx=targptr%normal(1)
    normaly=targptr%normal(2)
    normalz=targptr%normal(3)
    internallocation=targptr%depth_loc
    targe_id=targptr%name
    material=targptr%material
    equationtype=tcname
    method='EXPLICIT'

    write (31,'(/)')
    write (31,TARGE)
         
    end subroutine targeprint
    end subroutine targecon

    
    ! --------------------------- cfirecon --------------------------------------
    subroutine cfirecon(i)
    
    integer :: i
    character(64) :: tcname

    type(fire_type), pointer :: fireptr
    type(target_type), pointer :: targptr
    
    fireptr => fireinfo(i)
    
    c1=len_trim(fireptr%name)
    
    if (fireptr%ignition_type==1) tcname='TIME'
    if (fireptr%ignition_type==2) tcname='TEMP'
    if (fireptr%ignition_type==3) tcname='FLUX'
    c2=len_trim(tcname)
    
    if (fireptr%ignition_target /= 0) then
        targptr => targetinfo(fireptr%ignition_target)
        c3=len_trim(targptr%name)
    else
        c3=0
    end if
   
    call cfireprint(i,tcname)
    
    if (chemiflag) call chemiprint(i,fireptr%npoints)

    contains  
    subroutine cfireprint(i,tcname)

    character (64) :: tcname
    integer :: i,compartment,plume,dummy1,dummy2,iroom
    real(eb) :: x,y,z,criterion
    character(c1) :: fire_id
    character(c2) :: type
    character(c3) :: target
    namelist /CFIRE/compartment,x,y,z,plume,type,criterion,target,dummy1,dummy2,fire_id

    type(fire_type), pointer :: fireptr
    type(target_type), pointer :: targptr
    
    fireptr => fireinfo(i)
  
    compartment=fireptr%room
    x=fireptr%x_position
    y=fireptr%y_position
    z=fireptr%z_position
    plume=fireptr%modified_plume
    type=tcname
    if (type=='TIME') criterion=fireptr%ignition_time
    if (type=='TEMP' .or. type=='FLUX') criterion=fireptr%ignition_criterion
    if (fireptr%ignition_target /= 0) then
        targptr => targetinfo(fireptr%ignition_target)
        target=targptr%name
    else
        target=''
    end if
    dummy1=0
    dummy2=0
    fire_id=fireptr%name

    write (31,'(/)')
    write (31,CFIRE)
         
    end subroutine cfireprint  
    
    subroutine chemiprint(i,nret)

    integer :: i,j,nret
    real(eb) :: c,h,o,n,cl,chir,hoc
    real(eb), dimension(nret) :: time,fv,c_co,trace,area,height,c_hrr
    character(c1) :: chemi_id
    namelist /CHEMI/chemi_id,c,h,o,n,cl,chir,hoc,nret,time,c_hrr,fv,c_co,trace,area,height

    type(fire_type), pointer :: fireptr
    
    fireptr => fireinfo(i)

    chemi_id=fireptr%name
    c=fireptr%n_c
    h=fireptr%n_h
    o=fireptr%n_o
    n=fireptr%n_n
    cl=fireptr%n_cl
    chir=fireptr%chirad
    hoc=fireptr%hoc(1)
    do j=1,nret
        time(j)=fireptr%time(j)
        fv(j)=fireptr%y_soot(j)
        c_co(j)=fireptr%y_co(j)
        trace(j)=fireptr%y_trace(j)
        area(j)=fireptr%area(j)
        height(j)=fireptr%height(j)
        c_hrr(j)=fireptr%qdot(j)
    end do
    
    write (31,'(/)')
    write (31,CHEMI)
         
    end subroutine chemiprint
    
    
    end subroutine cfirecon

    
    ! --------------------------- ctimecon --------------------------------------
    subroutine ctimecon
    
    call ctimeprint

    contains  
    subroutine ctimeprint

    integer :: simulation,print,spreadsheet,smokeview
    namelist /CTIME/simulation,print,spreadsheet,smokeview

    simulation=time_end
    print=print_out_interval
    smokeview=smv_out_interval
    spreadsheet=ss_out_interval

    write (31,'(/)')
    write (31,CTIME)
         
    end subroutine ctimeprint  
    
    end subroutine ctimecon

    
    ! --------------------------- tambicon --------------------------------------
    subroutine tambicon
    
    call tambiprint

    contains  
    subroutine tambiprint

    real(eb) :: temp,pres,rh
    namelist /TAMBI/temp,pres,rh
      
    temp=interior_temperature
    pres=interior_abs_pressure
    rh=100.0_eb*relative_humidity

    write (31,'(/)')
    write (31,TAMBI)
         
    end subroutine tambiprint  
    
    end subroutine tambicon

    
    ! --------------------------- eambicon --------------------------------------
    subroutine eambicon
    
    call eambiprint

    contains  
    subroutine eambiprint

    real(eb) :: temp,pres
    namelist /EAMBI/temp,pres
      
    temp=exterior_temperature
    pres=exterior_abs_pressure

    write (31,'(/)')
    write (31,EAMBI)
         
    end subroutine eambiprint  
    
    end subroutine eambicon

    
    ! --------------------------- limo2con --------------------------------------
    subroutine limo2con
    
    call limo2print

    contains  
    subroutine limo2print

    real(eb) :: o2index
    namelist /LIMO2/o2index
 
    o2index=lower_o2_limit

    write (31,'(/)')
    write (31,LIMO2)
         
    end subroutine limo2print  
    
    end subroutine limo2con

    
    ! --------------------------- hventcon --------------------------------------
    subroutine hventcon(i)
    
    integer :: i
    character(64) :: tcname
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    
    ventptr => hventinfo(i)
    
    if (ventptr%opening_type==1) tcname='TIME'
    if (ventptr%opening_type==2) tcname='TEMP'
    if (ventptr%opening_type==3) tcname='FLUX'
    c1=len_trim(tcname)
    
    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        c2=len_trim(targptr%name)
    else
        c2=0
    end if
    
    call hventprint(i,tcname)

    contains  
    subroutine hventprint(i,tcname)
      
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    
    character(64) :: tcname
    integer :: i,first,second,hvent_id,face
    real(eb) :: width,soffit,sill,offset1,offset2,criterion,initialtime,finaltime,initialfraction,finalfraction
    character(c1) :: type
    character(c2) :: target
    namelist /HVENT/first,second,hvent_id,width,soffit,sill,offset1,offset2,face,type, &
                    criterion,target,initialtime,initialfraction,finaltime,finalfraction
    
    ventptr => hventinfo(i)
    
    first=ventptr%room1
    second=ventptr%room2
    hvent_id=ventptr%counter
      
    width=ventptr%width
    soffit=ventptr%soffit
    sill=ventptr%sill
      
    offset1=ventptr%offset(1)
    offset2=0.0_eb
    face=ventptr%face
    type=tcname
    criterion=ventptr%opening_criterion
    
    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        target=targptr%name
    else
        target=''
    end if
    
    initialtime=0.0_eb
    initialfraction=0.0_eb
    finaltime=0.0_eb
    finalfraction=0.0_eb

    initialtime=ventptr%opening_initial_time
    initialfraction=ventptr%opening_initial_fraction
    finaltime=ventptr%opening_final_time
    finalfraction=ventptr%opening_final_fraction

    write (31,'(/)')
    write (31,HVENT)
         
    end subroutine hventprint
    end subroutine hventcon

    
    ! --------------------------- deadrcon --------------------------------------
    subroutine deadrcon(i)
    
    integer :: i
    
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    if (roomptr%deadroom) call deadrprint(i)

    contains  
    subroutine deadrprint(i)
    
    integer :: i
      
    integer :: compartment,connected
    namelist /DEADR/compartment,connected

    type(room_type), pointer :: roomptr
 
    roomptr => roominfo(i)
    connected=roomptr%deadroom
    compartment=i

    write (31,'(/)')
    write (31,DEADR)
         
    end subroutine deadrprint  
    
    end subroutine deadrcon

    
    ! --------------------------- eventcon --------------------------------------
    subroutine eventcon
    
    integer :: i,j,k,iijk
    
    do i=1,nr !first max 50
        do j=1,nr !second max 50
            do k=1,10 !id with max 10
                do iijk=1,n_hvents
                    if (hflag(i,j,k,iijk)) call vent_print (1,iijk,i,j,k)
                end do
                do iijk=1,n_vvents
                    if (vflag(i,j,k,iijk)) call vent_print (2,iijk,i,j,k)
                end do
                do iijk=1,n_mvents
                    if (mflag(i,j,k,iijk)) call vent_print (3,iijk,i,j,k)
                end do
            end do
        end do
    end do
    
    do i=1,nr !first max 50
        do j=1,nr !second max 50
            do k=1,n_mvents
                if (fflag(i,j,k)) call fan_print (i,j,k)
            end do
        end do
    end do
 
    contains  
    subroutine vent_print (n,iijk,i,j,k)
    
    integer :: i,j,k,iijk,n
 
    type(vent_type), pointer :: ventptr
 
    integer :: first,second,event_id
    real(eb) :: fraction,decay,time
    character(1) :: type
    namelist /EVENT/first,second,event_id,type,time,fraction,decay
    
    first=i
    second=j
    event_id=k
    if (n==1) then
        ventptr => hventinfo(iijk)
        type='H'
    end if
    if (n==2) then
        ventptr => vventinfo(iijk)
        type='V'
    end if
    if (n==3) then
        ventptr => mventinfo(iijk)
        type='M'
    end if
    time=ventptr%opening_initial_time
    fraction=ventptr%opening_final_fraction
    decay=ventptr%opening_final_time-ventptr%opening_initial_time

    write (31,'(/)')
    write (31,EVENT)
         
    end subroutine vent_print
    
    
    subroutine fan_print (i,j,k)
    
    integer :: i,j,k
 
    type(vent_type), pointer :: ventptr
 
    integer :: first,second,event_id
    real(eb) :: fraction,decay,time
    character(1) :: type
    namelist /EVENT/first,second,event_id,type,time,fraction,decay
    
    first=i
    second=j
    event_id=k
    ventptr => mventinfo(k)
    type='F'
    time=ventptr%filter_initial_time
    fraction=ventptr%filter_final_fraction
    decay=ventptr%filter_final_time-ventptr%filter_initial_time

    write (31,'(/)')
    write (31,EVENT)
         
    end subroutine fan_print
    
    end subroutine eventcon
    
    
    ! --------------------------- crampcon --------------------------------------
    subroutine crampcon(i)
    
    integer :: i

    type(ramp_type), pointer :: rampptr
    
    rampptr=>rampinfo(i)
    
    c1=len_trim(rampptr%type)
   
    call crampprint(i,rampptr%npoints)

    contains  
    subroutine crampprint(i,nret)

    type(ramp_type), pointer :: rampptr
      
    integer :: nret,i,j
    integer :: first,second,ramp_id,ramp_n
    real(eb), dimension(nret) :: time,fraction
    character(c1) :: type
    namelist /CRAMP/type,first,second,ramp_id,ramp_n,time,fraction
    
    rampptr=>rampinfo(i)
    
    type=rampptr%type
    first=rampptr%room1
    second=rampptr%room2
    ramp_id=rampptr%counter
    ramp_n=rampptr%npoints
    do j = 1,nret
        time(j)=rampptr%time(j)
        fraction(j)=rampptr%value(j)
    end do

    write (31,'(/)')
    write (31,CRAMP)
         
    end subroutine crampprint
    end subroutine crampcon

    
    ! --------------------------- vventcon --------------------------------------
    subroutine vventcon(i)
    
    integer :: i
    character(64) :: tcname
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    
    ventptr => vventinfo(i)
    
    if (ventptr%opening_type==1) tcname='TIME'
    if (ventptr%opening_type==2) tcname='TEMP'
    if (ventptr%opening_type==3) tcname='FLUX'
    c1=len_trim(tcname)
    
    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        c2=len_trim(targptr%name)
    else
        c2=0
    end if
   
    call vventprint(i,tcname)

    contains  
    subroutine vventprint(i,tcname)
    
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
      
    integer :: i
    character(64) :: tcname
    integer :: top,bottom,vvent_id,shape,initialtime,finaltime
    real(eb) :: area,offsetx,offsety,criterion,initialfraction,finalfraction
    character(c1) :: type
    character(c2) :: target
    namelist /VVENT/top,bottom,vvent_id,area,shape,type,criterion,target,initialtime, &
                      initialfraction,finaltime,finalfraction,offsetx,offsety
    
    ventptr => vventinfo(i)
    
    top=ventptr%room1
    bottom=ventptr%room2
    vvent_id=ventptr%counter
    area=ventptr%area
    
    shape=ventptr%shape
    type=tcname
    
    initialtime=0.0_eb
    initialfraction=0.0_eb
    finaltime=0.0_eb
    finalfraction=0.0_eb
    
    initialtime=ventptr%opening_initial_time
    initialfraction=ventptr%opening_initial_fraction

    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        target=targptr%name
    else
        target=''
    end if
    criterion=ventptr%opening_criterion
    
    finaltime=ventptr%opening_final_time
    finalfraction=ventptr%opening_final_fraction
    offsetx=ventptr%xoffset
    offsety=ventptr%yoffset

    write (31,'(/)')
    write (31,VVENT)
         
    end subroutine vventprint
    end subroutine vventcon

    
    ! --------------------------- mventcon --------------------------------------
    subroutine mventcon(i)
    
    integer :: i
    character(64) :: tcname,tcname2,tcname3
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    
    ventptr => mventinfo(i)
    
    if (ventptr%opening_type==1) tcname='TIME'
    if (ventptr%opening_type==2) tcname='TEMP'
    if (ventptr%opening_type==3) tcname='FLUX'
    c1=len_trim(tcname)
    
    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        c2=len_trim(targptr%name)
    else
        c2=0
    end if
    
    if (ventptr%orientation(1)==1) tcname2='V'
    if (ventptr%orientation(1)==2) tcname2='H'
    c3=len_trim(tcname2)
    
    if (ventptr%orientation(2)==1) tcname3='V'
    if (ventptr%orientation(2)==2) tcname3='H'
    c4=len_trim(tcname3)
   
    call mventprint(i,tcname,tcname2,tcname3)

    contains  
    subroutine mventprint(i,tcname,tcname2,tcname3)
      
    type(vent_type), pointer :: ventptr
    type(target_type), pointer :: targptr

    integer :: i
    character(64) :: tcname,tcname2,tcname3
    integer :: first,second,mvent_id
    real(eb) :: height1,area1,height2,area2,plower,pupper,offsetx,offsety,criterion, &
                initialfraction,finalfraction,initialtime,finaltime,flow
    character(c1) :: type
    character(c2) :: target
    character(c3) :: orientation1
    character(c4) :: orientation2
    namelist /MVENT/first,second,mvent_id,orientation1,height1,area1,orientation2, &
                    height2,area2,flow,plower,pupper,type,criterion,target,initialtime, &
                    initialfraction,finaltime,finalfraction,offsetx,offsety
    
    ventptr => mventinfo(i)
    
    first=ventptr%room1
    second=ventptr%room2
    mvent_id=ventptr%counter
      
    orientation1=tcname2
    height1=ventptr%height(1)
    area1=ventptr%diffuser_area(1)
    
    orientation2=tcname3
    height2=ventptr%height(2)
    area2=ventptr%diffuser_area(2)
    
    flow=ventptr%maxflow
    plower=ventptr%min_cutoff_relp
    pupper=ventptr%max_cutoff_relp
    
    type=tcname
    criterion=ventptr%opening_criterion

    if (ventptr%opening_target /= 0) then
        targptr => targetinfo(ventptr%opening_target)
        target=targptr%name
    else
        target=''
    end if
    
    initialtime=0.0_eb
    initialfraction=0.0_eb
    finaltime=0.0_eb
    finalfraction=0.0_eb
    
    initialtime=ventptr%opening_initial_time
    initialfraction=ventptr%opening_initial_fraction
    finaltime=ventptr%opening_final_time
    finalfraction=ventptr%opening_final_fraction
    offsetx=ventptr%xoffset
    offsety=ventptr%yoffset

    write (31,'(/)')
    write (31,MVENT)
         
    end subroutine mventprint
    end subroutine mventcon

    
    ! --------------------------- deteccon --------------------------------------
    subroutine deteccon(i)
    
    integer :: i
   
    call detecprint(i)

    contains  
    subroutine detecprint(i)

    type(detector_type), pointer :: dtectptr
    type(room_type), pointer :: roomptr

    integer :: i,type,compartment,suppression
    real(eb) :: trigger,x,y,z,rti,spray_density
    namelist /detec/type,compartment,trigger,x,y,z,rti,suppression,spray_density

    dtectptr => detectorinfo(i)
        
    type=dtectptr%dtype
    compartment=dtectptr%room
    trigger=dtectptr%trigger
    
    x=dtectptr%center(1)
    y=dtectptr%center(2)
    z=dtectptr%center(3)
    
    rti=dtectptr%rti
    if (dtectptr%quench) then
        suppression=1
    else
        suppression=0
    end if
    spray_density=dtectptr%spray_density/1000.0_eb

    write (31,'(/)')
    write (31,DETEC)
         
    end subroutine detecprint
    end subroutine deteccon

    
    ! --------------------------- vheatcon --------------------------------------
    subroutine vheatcon(i)
    
    integer :: i
   
    call vheatprint(i)

    contains  
    subroutine vheatprint(i)

    integer :: i
    integer :: top,bottom
    namelist /VHEAT/top,bottom
      
    top=i_vconnections(i,w_from_room)
    bottom=i_vconnections(i,w_to_room)

    write (31,'(/)')
    write (31,VHEAT)
         
    end subroutine vheatprint
    end subroutine vheatcon

    
    ! --------------------------- conezcon --------------------------------------
    subroutine conezcon(i)
    
    integer :: i
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    if (roomptr%shaft) call conezprint(i)

    contains  
    subroutine conezprint(i)
      
    integer :: i
    integer :: compartment
    namelist /CONEZ/compartment
      
    compartment=i

    write (31,'(/)')
    write (31,CONEZ)
         
    end subroutine conezprint
    end subroutine conezcon

    
    ! --------------------------- challcon --------------------------------------
    subroutine challcon(i)
    
    integer :: i
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    if (roomptr%hall) call challprint(i)

    contains  
    subroutine challprint(i)

    integer :: i
    integer :: compartment
    namelist /CHALL/compartment
      
    compartment=i

    write (31,'(/)')
    write (31,CHALL)
         
    end subroutine challprint
    end subroutine challcon

    
    ! --------------------------- roomacon --------------------------------------
    subroutine roomacon(i)
    
    integer :: i
      
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    
    if (roomptr%nvars /= 0) call roomaprint(i,roomptr%nvars)

    contains  
    subroutine roomaprint(i,nret)
      
    type(room_type), pointer :: roomptr

    integer :: i,nret,j
    integer :: compartment,number
    real(eb), dimension(nret) :: area
    namelist /ROOMA/compartment,number,area
      
    roomptr => roominfo(i)
    
    compartment=i
    number=nret
    do j=1,nret
        area(j)=roomptr%var_area(j)
    end do

    write (31,'(/)')
    write (31,ROOMA)
         
    end subroutine roomaprint
    end subroutine roomacon

    
    ! --------------------------- roomhcon --------------------------------------
    subroutine roomhcon(i)
    
    integer :: i
      
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(i)
    
    if (roomptr%nvars /= 0) call roomhprint(i,roomptr%nvars)

    contains  
    subroutine roomhprint(i,nret)
      
    type(room_type), pointer :: roomptr

    integer :: i,nret,j
    integer :: compartment,number
    real(eb), dimension(nret) :: height
    namelist /ROOMH/compartment,number,height
      
    roomptr => roominfo(i)
      
    compartment=i
    number=nret
    do j=1,nret
        height(j)=roomptr%var_height(j)
    end do

    write (31,'(/)')
    write (31,ROOMH)
         
    end subroutine roomhprint
    end subroutine roomhcon

    
    ! --------------------------- dtchecon --------------------------------------
    subroutine dtchecon
   
    call dtcheprint

    contains  
    subroutine dtcheprint

    integer :: i
    integer :: count
    real(eb) :: time
    namelist/DTCHE/time,count
      

    time=stpmin
    count=stpmin_cnt_max

    write (31,'(/)')
    write (31,DTCHE)
         
    end subroutine dtcheprint
    end subroutine dtchecon

    
    ! --------------------------- hheatcon --------------------------------------
    subroutine hheatcon(i)
    
    integer :: i
      
    type(room_type), pointer :: roomptr
      
    roomptr => roominfo(i)
   
    if (roomptr%iheat /= 0) call hheatprint(i,hheatnto)

    contains  
    subroutine hheatprint(i,nret)
      
    type(room_type), pointer :: roomptr

    integer :: i,j,nret
    integer :: first,nop
    real(eb), dimension(nret) :: npos_comp,npos_frac
    namelist/HHEAT/first,nop,npos_comp,npos_frac
      
    roomptr => roominfo(i)
      
    first=i
    nop=nret
    do j=1,nret
        npos_comp(j)=hheat_comp(j)
        npos_frac(j)=roomptr%heat_frac(int(npos_comp(j)))
    end do

    write (31,'(/)')
    write (31,HHEAT)
         
    end subroutine hheatprint
    end subroutine hheatcon

    
    ! --------------------------- furnccon --------------------------------------
    subroutine furnccon
   
    call furncprint(nfurncount)

    contains  
    subroutine furncprint(nret)

    integer :: i,nret
    integer :: number
    real(eb), dimension(nret) :: time,temp
    namelist/FURNC/number,time,temp
      
    number=nret
    do i = 1, nret
         time(i)=furn_time(i)
         temp(i)=furn_temp(i)
    end do
      
    write (31,'(/)')
    write (31,FURNC)
         
    end subroutine furncprint
    end subroutine furnccon

    
    ! --------------------------- adiabcon --------------------------------------
    subroutine adiabcon
    
    character(64) :: tcname
    
    if (adiabatic_walls .eqv. .true.) then
        tcname='TRUE'
        c1=len_trim(tcname)
    end if
   
    call adiabprint(tcname)

    contains  
    subroutine adiabprint(tcname)
    
    character(64) :: tcname
    
    character(c1) :: adiab_walls
    namelist/ADIAB/adiab_walls
      
    adiab_walls=tcname
      
    write (31,'(/)')
    write (31,ADIAB)
         
    end subroutine adiabprint
    end subroutine adiabcon

    
    ! --------------------------- cslcfcon --------------------------------------
    subroutine cslcfcon(i)
    
    integer :: i
    character(64) :: tcname,tcname2
      
    type(visual_type), pointer :: sliceptr
    
    sliceptr => visualinfo(i)
    
    if (sliceptr%vtype==1 .or.sliceptr%vtype==2) then
        if (sliceptr%vtype==1) tcname='2-D'
        if (sliceptr%vtype==2) tcname='3-D'
        c1=len_trim(tcname)
        
        if (sliceptr%axis==1) tcname2='X'
        if (sliceptr%axis==2) tcname2='Y'
        if (sliceptr%axis==3) tcname2='Z'
        c2=len_trim(tcname2)
   
        call cslcfprint(i,tcname,tcname2)
    else
        continue
    end if

    contains  
    subroutine cslcfprint(i,tcname,tcname2)

    type(visual_type), pointer :: sliceptr

    integer :: i,j
    character(64) :: tcname,tcname2
    integer :: number
    integer, dimension(1) :: comp
    real(eb) :: position
    character (c1) :: domain
    character (c2) :: plane
    namelist /CSLCF/domain,plane,number,position,comp

    sliceptr => visualinfo(i)

    number=1 !currently cfast only takes one value
    position=sliceptr%position
    domain=tcname
    plane=tcname2
    do j=1,number
        comp(j)=sliceptr%roomnum
    end do

    write (31,'(/)')
    write (31,CSLCF)
         
    end subroutine cslcfprint
    end subroutine cslcfcon
    
    
    ! --------------------------- ciosfcon --------------------------------------
    subroutine cisofcon(i)
    
    integer :: i
      
    type(visual_type), pointer :: sliceptr
    
    sliceptr => visualinfo(i)
    
    if (sliceptr%vtype==3) then   
        call cisofprint(i)
    else
        continue
    end if

    contains  
    subroutine cisofprint(i)

    type(visual_type), pointer :: sliceptr
    
    integer :: i
    integer :: comp
    real(eb) :: value
    namelist /CISOF/value,comp

    sliceptr => visualinfo(i)

    value=sliceptr%value
    comp=sliceptr%roomnum

    write (31,'(/)')
    write (31,CISOF)
         
    end subroutine cisofprint
    end subroutine cisofcon

    

end module conversion_routines
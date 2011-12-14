
! --------------- readini ----------------------

subroutine readini
  use precision
  use zonedata
  implicit none

!  type(room_data), pointer :: room
  integer :: iin
  integer :: error
  character(len=256) :: line, line2
  
  iin = funit(70)
  rptol = 1.0d-5
  aptol = 1.0d-5
  rtol = 1.0d-5
  atol = 1.0d-5 
  pamb = 0.0_dd
  tamb = 288.0_dd
  dprint = 10.0_dd
  ddump = 10.0_dd
  dplot = 10.0_dd
  open(unit=iin, file='zfm.ini',iostat=error) 

  if(error.ne.0)go to 999
  do
    read(iin,'(a)',end=999)line
    line2=adjustl(line)
    line=line2
    if(line(1:1).eq."#".or.line.eq."")cycle

    if(line(1:3).eq."AMB")then
      read(iin,*)pamb, tamb
      cycle
    endif

    if(line(1:3).eq."TOL")then
      read(iin,*)rptol, aptol, rtol, atol
      cycle
    endif

    if(line(1:).eq."SHOWTIME")then
      read(iin,*)dprint,ddump,dplot
      cycle
    endif

  end do
999 continue
  
  rhoamb = (pabs_ref+pamb)/(rgas*tamb)

  close(iin)

  return
end  subroutine readini


! --------------- loadcase ----------------------

subroutine dumpcase(fileout,error,append)
  use precision
  use zonedata
  implicit none
  character(len=*), intent(in) :: fileout
  integer, intent(out) :: error
  logical, intent(in) :: append


  integer :: outunit
  type(room_data), pointer :: room
  type(vent_data), pointer :: vent
  type(fire_data), pointer :: fire
  type(hvac_data), pointer :: h
  integer :: iroom, i, ivent, ifire, ihvac

  outunit = funit(70)
  if(append)then
    open(unit=outunit,file=fileout,position="append",iostat=error)
   else
    open(unit=outunit, file=fileout, status="replace",iostat=error)
  endif
  if(error.ne.0)return

  if(append)go to 999
  if(nrooms.gt.0)then
    write(outunit,"(a)")"ROOMS"
    write(outunit,*)nrooms
    do iroom = 1, nrooms
      room => rooms(iroom)
      write(outunit,"(4e11.4)")room%z0,room%dx,room%dy,room%dz
    end do
  endif

  if(nvents.gt.0)then
    write(outunit,"(a)")"VENTS"
    write(outunit,*)nvents
    do ivent = 1, nvents
      vent => vents(ivent)
      write(outunit,"(i3,1x,i3,3(e13.6,1x))")vent%from,vent%to,vent%relbot,vent%reltop,vent%width
    end do
  endif

  if(solveoxy)write(outunit,"(a)")"SOLVEOXY"

  if(allwalls)then
    write(outunit,"(a)")"ALLWALLS"
    write(outunit,"(a)")trim(allwallsmat)
  endif

  if(nfires.gt.0)then
    write(outunit,"(a)")"FIRES"
    write(outunit,*)nfires
    do ifire = 1, nfires
      fire=>fires(ifire)
      write(outunit,*)fire%room_number,fire%type,fire%z0
    	if(fire%type.eq.constant)then
        write(outunit,*)fire%q_pyrol(1)
    	 elseif(fire%type.eq.general)then
    	  write(outunit,*)fire%npoints
    	  do i = 1, fire%npoints
    	    write(outunit,"(e13.6,1x,e13.6)")fire%times(i),fire%q_pyrol(i)/1000.00_dd
    	  end do
      end if
    end do
  endif

  if(nhvacs.gt.0)then
    write(outunit,"(a)")"HVACS"
    write(outunit,*)nhvacs
    do ihvac = 1, nhvacs
      h => hvacs(ihvac)
      write(outunit,"(e13.6,1x,l2,1x,e13.6)")h%vfan,h%specifiedtemp,h%tfan
      write(outunit,"(2(i3,1x,e13.6,1x,e13.6,1x))")h%fromroom,h%rel_frombot,h%rel_fromtop,h%toroom,h%rel_tobot,h%rel_totop
    end do
  endif

  if(nrooms.gt.0)then
    write(outunit,"(a)")"GEOM"
    write(outunit,*)nrooms
    do iroom = 1, nrooms
      room => rooms(iroom)
      write(outunit,"(i3,e13.6,1x,e13.6)")iroom,room%x0,room%y0
    end do
  endif

  if(nvents.gt.0)then
    write(outunit,"(a)")"VENTGEOM"
    write(outunit,"(i3)")nvents
    do ivent = 1, nvents
      vent=>vents(ivent)
      write(outunit,"(i3,1x,i3,1x,e13.6)")ivent,vent%face,vent%offset
    end do
  endif

  if(plotfilebase.ne."")then
    write(outunit,"(a)")"PLOTFILEBASE"
    write(outunit,"(a)")trim(plotfilebase)
  endif

999 continue
  write(outunit,"(a)")"TIME"
  write(outunit,"(e13.6,1x,e13.6)")tnow,tfinal
  do iroom = 1, nrooms
    room => rooms(iroom)
    write(outunit,"(4(e13.6,1x))")room%rel_pressure,room%rel_layer_height, &
      room%layer(lower)%temperature,room%layer(upper)%temperature
  end do

  close(outunit)
end subroutine dumpcase


! --------------- loadcase ----------------------

recursive subroutine loadcase(filein,error)
  use precision
  use zonedata
  implicit none
  character(len=*), intent(in) :: filein
  integer, intent(out) :: error
  type(room_data), pointer :: room
  type(vent_data), pointer :: vent
  type(fire_data), pointer :: fire
  type(hvac_data), pointer :: h
  integer :: iroom, ivent, jvent, ifire, ihvac,iwall,iw,nwalls
  integer :: iin, npoints, i
  real(kind=dd) :: t1,tr1,dt,tr2,t2,t3,t4,qlevel,x,y,offset
  real(kind=dd) :: dx, dy
  character(len=256) :: line,line2,loadfile
  integer :: j, mrooms, face,mvents
  integer :: walltype
  character(len=30) :: wallmat
  integer :: getwalltype

  ! read in and allocate room data
  iin = funit(70)
  error = 0
  
  open(unit=iin, file=filein) 

  do
    read(iin,'(a)',end=999)line
    line2=adjustl(line)
    line=line2
    if(line(1:1).eq."#".or.line.eq."")cycle

    if(line(1:5).eq.'ROOMS')then
      read(iin,*)nrooms 
      if(allocated(rooms))deallocate(rooms)
      error=0
      if(nrooms.gt.0)allocate(rooms(0:nrooms),stat=error)
      if(nrooms.le.0)then
        write(6,*)"number of rooms must be greater than zero"
        stop
      endif
      if(error.ne.0)then
        write(6,*)'error allocating rooms'
        stop
      end if

      ! define room 0 using data read in or in readini

      room => rooms(0)
      room%z0 = 0.0_dd
      room%dx = 1000._dd
      room%dy = 1000._dd
      room%dz = 1000._dd
      room%rel_layer_height = 1000._dd
      room%floor_area = room%dx*room%dy
      room%abs_pressure = pabs_ref
      room%rel_pressure = 0.0_dd
      room%abs_layer_height = room%z0 + room%rel_layer_height
      room%volume = room%floor_area*room%dz
      room%VU = 0.0_dd

      room%layer(lower)%temperature = tamb
      room%layer(upper)%temperature = tamb
      room%layer(lower)%density = rhoamb
      room%layer(upper)%density = rhoamb
      room%layer(lower)%volume = room%floor_area*room%dz
      room%layer(lower)%mass = rhoamb*room%volume
      room%layer(lower)%s_mass = room%layer(lower)%mass*amb_oxy_con
      room%layer(lower)%s_con = amb_oxy_con
      room%layer(upper)%s_mass = room%layer(upper)%mass*amb_oxy_con
      room%layer(upper)%s_con = amb_oxy_con
      room%layer(upper)%volume = 0.0_dd
      room%layer(upper)%mass = 0.0_dd

! read in data for the indoor environment

      do iroom = 1, nrooms
        room => rooms(iroom)
        read(iin,*)room%z0,room%dx,room%dy,room%dz
        do iwall = 1, 4
          room%wall(iwall)%wallmatindex = p_nowall
        end do
        room%x0=0.0
        room%y0=0.0
      end do

! allocate space for horizontal vent and fire flow data structures

      if(allocated(totalflow))deallocate(totalflow)
      if(allocated(hflow))deallocate(hflow)
      if(allocated(fflow))deallocate(fflow)
      if(allocated(hvflow))deallocate(hvflow)
      if(allocated(cflow))deallocate(cflow)
      error=0
      if(error.eq.0)allocate(totalflow(0:nrooms,2),stat=error)
      if(error.eq.0)allocate(hflow(0:nrooms,2),stat=error)
      if(error.eq.0)allocate(fflow(0:nrooms,2),stat=error)
      if(error.eq.0)allocate(hvflow(0:nrooms,2),stat=error)
      if(error.eq.0)allocate(cflow(0:nrooms,2),stat=error)
      if(error.ne.0)then
        write(6,*)"error allocating flow variables"
        stop
      endif
      cycle
    endif

    if(line(1:5).eq."DEBUG")then
      debugprint=.true.
      cycle
    endif

    if(line(1:8).eq."SOLVEOXY")then
      solveoxy=.true.
      cycle
    endif

    if(line(1:4).eq."LOAD")then
      read(iin,"(a)")loadfile
      call loadcase(loadfile,error)
    endif

    if(line(1:8).eq."ALLWALLS")then
      allwalls=.true.
      read(iin,"(a)")allwallsmat
      walltype = getwalltype(allwallsmat)
      do iroom=1, nrooms
        do iw = 1, 4
          rooms(iroom)%wall(iw)%wallmatindex=walltype
          rooms(iroom)%wall(iw)%wallmat=allwallsmat
        end do
      end do
      cycle
    endif

    if(line(1:5).eq."WALLS")then
      allwalls = .false.
      read(iin,*)nwalls
      do iwall=1,nwalls
        read(iin,*)iroom,iw
        read(iin,"(a)")wallmat
        if(iroom.ge.1.and.iroom.le.nrooms)then
          walltype = getwalltype(wallmat)
          if(iw.ge.1.and.iw.le.3)then
            rooms(iroom)%wall(iw)%wallmat=wallmat
            rooms(iroom)%wall(iw)%wallmatindex=walltype
            rooms(iroom)%wall(iw)%dir=iw
            if(iw.eq.3)then
              rooms(iroom)%wall(4)%wallmatindex=walltype
              rooms(iroom)%wall(4)%wallmat=wallmat
              rooms(iroom)%wall(4)%dir=4
            endif
          endif
        endif
      end do
      cycle
    endif

    if(line(1:4).eq."GEOM")then
      read(iin,*)mrooms
      do iroom = 1, mrooms
        read(iin,*)j,x,y
        if(j.ge.1.and.j.le.nrooms)then
          room=>rooms(j)
          room%x0=x
          room%y0=y
        endif
      end do
      cycle
    endif

    if(line(1:8).eq."VENTGEOM")then
      read(iin,*)mvents
      do jvent = 1, mvents
        read(iin,*)ivent,face,offset
        if(ivent.ge.1.and.ivent.le.nvents)then
          vent=>vents(ivent)
          vent%face=face
          vent%offset=offset
        endif
      end do
      cycle
    endif

    if(line(1:4).eq."PLOT")then
      read(iin,"(a)")plotfilebase
      csvfile=trim(plotfilebase)//'.csv'
      smvfile=trim(plotfilebase)//'.smv'
      plotfile=trim(plotfilebase)//'.zfm'
      dumpfile=trim(plotfilebase)//'.dmp'
      cycle
    endif

  ! read in vent data and allocate space for vent data structures

    if(line(1:5).eq."VENTS")then
      read(iin,*)nvents
      if(allocated(vents))deallocate(vents)
      error=0
      if(nvents.gt.0)allocate(vents(nvents),stat=error)
      if(error.ne.0)then
        write(6,*)'allocation error for vents'
        stop
      end if
      do ivent = 1, nvents
        vent => vents(ivent)
        read(iin,*)vent%from,vent%to,vent%relbot,vent%reltop,vent%width
      	iroom = vent%from
      	if(iroom.ge.0.and.iroom.le.nrooms)then
          vent%absbot = rooms(iroom)%z0 + vent%relbot
          vent%abstop = rooms(iroom)%z0 + vent%reltop
      	 else
          error = 1
      	  write(6,*)"vent error: from room out of bounds"
      	  stop
      	endif
      	if(vent%to.eq.vent%from.or.vent%to.lt.0.or.vent%to.gt.nrooms)then
      	  write(6,*)"vent error: to room out of bounds"
      	  error=2
      	endif
      end do
      cycle
    endif
  
  ! read in species data 

  ! read(iin,*)nspecies
    
    if(solveoxy)then
      nspecies = 1
     else
      nspecies = 0
    endif

  ! read in fire info

    if(line(1:5).eq."FIRES")then
      read(iin,*)nfires
      if(allocated(fires))then
        do ifire = 1, noldfires
          fire=>fires(ifire)
          deallocate(fire%times,fire%q_pyrol)
        enddo
        deallocate(fires)
      endif
      error=0
      if(nfires.gt.0)allocate(fires(nfires),stat=error)
      if(error.ne.0)then
        write(6,*)"error allocating fires"
        stop
      endif
      noldfires=nfires
      do ifire = 1, nfires
        fire=>fires(ifire)
        read(iin,*)iroom,fire%type,fire%z0
        fire%room_number = iroom
        dx=rooms(iroom)%dx*0.50_dd
        dy=rooms(iroom)%dy*0.50_dd
        fire%x0=dx
        fire%y0=dy
        fire%dz=-1.0_dd
      	fire%heat_c = heat_c
      	fire%chi_rad = chi_rad
      	fire%fire_flow%rel_height = fire%z0
      	iroom = fire%room_number
      	fire%fire_flow%abs_height = fire%z0 + rooms(iroom)%z0
  
        if(fire%type.eq.tsquared)then
      	  npoints = 5
      	  fire%npoints = npoints
      	  allocate(fire%times(npoints),fire%q_pyrol(npoints),stat=error)
          if(error.ne.0)then
            write(6,*)"error allocating fire points"
            stop
          endif
          read(iin,*)t1,tr1,dt,qlevel,tr2
          qlevel=qlevel*1000.0_dd
      	  fire%times(1) = 0.0
      	  fire%q_pyrol(1) = 0.0
   
      	  fire%times(2) = t1
      	  fire%q_pyrol(2) = 0.0
    
      	  t2 = t1 + tr1*sqrt(qlevel/1000000.0)
      	  fire%times(3) = t2
      	  fire%q_pyrol(3) = qlevel
   
      	  t3 = t2 + dt
      	  fire%times(4) = t3
      	  fire%q_pyrol(4) = qlevel
    
      	  t4 = t3 + tr2*sqrt(qlevel/10000000.0)
      	  fire%times(5) = t4
      	  fire%q_pyrol(5) = 0.0
  
      	 elseif(fire%type.eq.constant)then
      	  npoints = 1
      	  fire%type = constant
      	  fire%npoints = npoints
      	  allocate(fire%times(npoints),fire%q_pyrol(npoints),stat=error)
          if(error.ne.0)then
            write(6,*)"error allocating fire points"
            stop
          endif
          read(iin,*)fire%q_pyrol(1)
          fire%q_pyrol(1) = fire%q_pyrol(1)*1000.0_dd
      	  fire%times(1) = 0.0
      	 elseif(fire%type.eq.general)then
      	  read(iin,*)npoints
      	  if(npoints.eq.1)fire%type = constant
      	  fire%npoints = npoints
      	  allocate(fire%times(npoints),fire%q_pyrol(npoints),stat=error)
          if(error.ne.0)then
            write(6,*)"error allocating fire points"
            stop
          endif
      	  do i = 1, npoints
      	    read(iin,*)fire%times(i),fire%q_pyrol(i)
            fire%q_pyrol(i) = fire%q_pyrol(i)*1000.0_dd
      	  end do
        end if
      end do
      cycle
    endif
  
  ! read in hvac data

    if(line(1:4).eq."HVAC")then
      read(iin,*)nhvacs
      if(allocated(hvacs))deallocate(hvacs)
      error=0
      if(nhvacs.gt.0)allocate(hvacs(nhvacs),stat=error)
      if(error.ne.0)then
        write(6,*)"error allocating hvacs"
        stop
      endif
      do ihvac = 1, nhvacs
        h => hvacs(ihvac)
        read(iin,*)h%vfan,h%specifiedtemp,h%tfan
        if(h%specifiedtemp)h%rhofan=pabs_ref/rgas/h%tfan
        read(iin,*)h%fromroom,h%rel_frombot,h%rel_fromtop,h%toroom,h%rel_tobot,h%rel_totop
      end do
      cycle
    endif
  
  ! **** now read in time dependent data ******

    if(line(1:4).eq."TIME")then
      tstart = 0.0
      tfinal = 100.0
      read(iin,*)tstart,tfinal
      if(dprint.ne.0.0)then
        tprint=tstart+dprint - mod(tstart+dprint,dprint)
       else
        tprint=tfinal+1.0
      endif
      tstartprint = tprint
      if(dplot.ne.0.0)then
        tplot=tstart + dplot - mod(tstart+dplot,dplot)
       else
        tplot=tfinal+1.0
      endif
      tstartplot = tplot
      if(ddump.ne.0.0)then
        tdump=tstart + ddump - mod(tstart+ddump,ddump)
       else
        tdump=tfinal+1.0
      endif
      tstartdump = tdump

      do iroom = 1, nrooms
        room => rooms(iroom)
    	  read(iin,*)room%rel_pressure,room%rel_layer_height, &
          room%layer(lower)%temperature,room%layer(upper)%temperature
      end do
      cycle
    endif
  end do
999 continue
  close(iin)
  tnow = tstart
end subroutine loadcase

! --------------- writedata ----------------------

subroutine writedata
  use precision
  use zonedata
  implicit none
  type(room_data), pointer :: room
  type(vent_data), pointer :: vent
  type(fire_data), pointer :: fire
  type(hvac_data), pointer :: h
  integer :: error, iroom, ivent, ifire, ihvac

  ! read in and allocate room data 

  write(6,*)'nrooms=',nrooms 
  do iroom = 1, nrooms
    room => rooms(iroom)
    write(6,*)'f. height=',room%z0,'width=',room%dx
  	write(6,*)'depth=',room%dy,'c. height=',room%dz
  end do

  ! read in vent data and allocate space for vent data structures

  write(6,*)'nvents=',nvents
  do ivent = 1, nvents
    vent => vents(ivent)
    write(6,*)'from room=',vent%from,'to room=',vent%to
  	write(6,*)'vent bottom=',vent%relbot,'vent top=',vent%reltop
  	write(6,*)'vent width=',vent%width
  end do

  write(6,*)'nfires=',nfires
  do ifire = 1, nfires
    fire=>fires(ifire)
    write(6,*)'iroom=',fire%room_number,'itype=',fire%type
  	write(6,*)'heatc=',fire%heat_c,' fire base=',fire%z0
    if(fire%type.eq.tsquared)then
       write(6,*)'start=',fire%time_start,'rate=',fire%time_rate
    end if
  end do

  ! read in hvac data
  write(6,*)'nhvacs=',nhvacs
  do ihvac = 1, nhvacs
    h => hvacs(ihvac)
    write(6,*)h%vfan,h%fromroom,h%rel_frombot,h%rel_fromtop,h%toroom,h%rel_tobot,h%rel_totop
  end do

  ! read in solver data

  write(6,*)'rptol=',rptol, 'aptol=',aptol
  write(6,*)'rtol=',rtol, 'atol=',atol

end subroutine writedata


integer function getwalltype(name)
  use precision
  use zonedata
  implicit none

  character(len=*) :: name

  if(name.eq."THIN")then
    getwalltype = p_thinwall
   elseif(name.eq."COLD")then
    getwalltype = p_coldwall
   elseif(name.eq."THICK")then
    getwalltype = p_thickwall
   else
    getwalltype = p_nowall
  endif

end function getwalltype

module namelist_routines

      use precision_parameters
      
      use fire_routines, only: flame_height
      use utility_routines, only: upperall
      
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
      
      logical :: exsets=.false.
      
      
      contains    
      ! --------------------------- namelist_input ----------------------------------
      subroutine namelist_input
      
      implicit none

      integer :: ios,ncomp

      ios = 1

      close (iofili)
      open (unit=iofili,file=inputfile,status='OLD',iostat=ios)
      if (ios/=0) then
          if (iofill>0) then
              write (*,5050) mod(ios,256)
              write (iofill,5050) mod(ios,256)
          else
              write (*,5050) mod(ios,256)
          end if
          stop
      end if

      ncomp = 0

      call read_versn(iofili)
      call read_stpma(iofili)
      call read_matrl(iofili)
      call read_compa(iofili,ncomp)
      call read_targe(iofili)
      call read_cfire(iofili)
      call read_chemi(iofili)
      call read_ctime(iofili)
      call read_eambi(iofili,exsets)
      call read_tambi(iofili,exsets)
      call read_limo2(iofili)
      call read_hvent(iofili)
      call read_deadr(iofili)
      call read_cramp(iofili)
      call read_vvent(iofili)
      call read_mvent(iofili)
      call read_event(iofili)
      call read_detec(iofili)
      call read_vheat(iofili)
      call read_conez(iofili)
      call read_chall(iofili)
      call read_rooma(iofili)
      call read_roomh(iofili)
      call read_dtche(iofili)
      call read_hheat(iofili)
      call read_furnc(iofili)
      call read_adiab(iofili)
      call read_cslcf(iofili)
      call read_cisof(iofili)
      call read_diagn(iofili)
      
      close (iofili)

      ! read format list
5050  format ('***Error: Error opening the input file = ',I6)


    end subroutine namelist_input

    
      ! --------------------------- read_versn --------------------------------------
      subroutine read_versn(LU)

      integer :: ios,iversion
      integer :: LU

      integer :: ivers
      character(256) :: head, pgrm_name
      namelist /VERSN/ head,ivers,pgrm_name

      ios = 1
      iversion = 0

      rewind(LU)
      input_file_line_number = 0

      ! Scan entire file to look for 'VERSN'
      versn_loop: do
         call checkread ('VERSN', LU, ios)
         if (ios==0) versnflag=.true.
         if (ios==1) then
             exit versn_loop
         end if 
         read(LU,VERSN,err=34,iostat=ios)
34       if (ios>0) then
            write(iofill, '(a,i4)') 'Error: Problem with VERSN input, line number', input_file_line_number
            stop
         end if
      end do versn_loop

      if (.not.versnflag) then
         write (*, '(/, "Inputs for VERSN are required.")')
         write (iofill, '(/, "Inputs for VERSN are required.")')
         stop
      end if

      versn_flag: if (versnflag) then
  
        rewind (LU) 
        input_file_line_number = 0

        call checkread('VERSN',LU,ios)
        call set_versn_defaults
        read(LU,VERSN)
  
        if (version>=1000) then
          iversion = version/1000
        else
          iversion = version/100
        end if

        title=pgrm_name

      end if versn_flag
  
      if (head==heading.and.ivers==iversion-1) then
          write (*,5004) ivers, iversion
          write (iofill,5004) ivers, iversion
      else if (head/=heading.or.ivers/=iversion) then
          write (*,5002) head,heading,ivers,iversion
          write (iofill,5002) head,heading,ivers,iversion
          stop
      end if

5002  format ('***Error: Not a compatible version ',2a8,2x,2i10)
5004  format ('Opening a version ',i2,' file with version ',i2,'. Fire inputs may need to be updated.')
  


      contains
  
      subroutine set_versn_defaults
  
      head       = 'null'
      pgrm_name   = 'null'
      ivers       = 0
  
      end subroutine set_versn_defaults
    
      end subroutine read_versn


      ! --------------------------- CHECKREAD ---------------------------------------
      SUBROUTINE CHECKREAD(NAME,LU,IOS)

      ! Look for the namelist variable NAME and then stop at that line.

      INTEGER :: II
      INTEGER, INTENT(OUT) :: IOS
      INTEGER, INTENT(IN) :: LU
      CHARACTER(5), INTENT(IN) :: NAME
      CHARACTER(80) TEXT
      IOS = 1

      READLOOP: DO
         READ(LU,'(A)',END=10) TEXT
         INPUT_FILE_LINE_NUMBER = INPUT_FILE_LINE_NUMBER + 1
         TLOOP: DO II=1,72
            IF (TEXT(II:II)/='&' .AND. TEXT(II:II)/=' ') EXIT TLOOP
            IF (TEXT(II:II)=='&') THEN
               IF (TEXT(II+1:II+5)==NAME) THEN
                  BACKSPACE(LU)
                  IOS = 0
                  EXIT READLOOP
               ELSE
                  CYCLE READLOOP
               ENDIF
            ENDIF
         ENDDO TLOOP
      ENDDO READLOOP
  
10    RETURN
  
      END SUBROUTINE CHECKREAD


      ! --------------------------- STPMA -------------------------------------------
      subroutine read_stpma(LU)

      integer :: ios
      integer :: LU

      real(eb) :: stepmax
      namelist /STPMA/stepmax

      ios = 1

      rewind(LU) ; input_file_line_number = 0

      ! Scan entire file to look for 'STPMA'
      stpma_loop: do
         call checkread ('STPMA',LU,ios)
         if (ios==0) stpmaflag=.true.
         if (ios==1) then
             exit stpma_loop
         end if 
         read(LU,STPMA,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(a,i5)') 'Error: Problem with STPMA input, line number', input_file_line_number
             stop
         end if
      end do stpma_loop

      stpma_flag: if (stpmaflag) then

        rewind (LU) 
        input_file_line_number = 0
  
        call checkread('STPMA',LU,ios)
        call set_stpma_defaults       
        read(LU,STPMA)
  
        stpmax=stepmax

      end if stpma_flag

      contains

      subroutine set_stpma_defaults

      stepmax       = 1.0_eb          ! s

      end subroutine set_stpma_defaults

      end subroutine read_stpma


      ! --------------------------- MATRL -------------------------------------------
      subroutine read_matrl(LU)
      

      integer :: ios,ii
      integer :: LU
      type(thermal_type), pointer :: thrmpptr

      integer :: nslab
      real(eb) :: c,eps,k,rho,thickness
      character(mxthrmplen) :: matrl_id
      namelist /MATRL/c,eps,k,matrl_id,nslab,rho,thickness

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'MATRL'
      n_thrmp = 0 
      matrl_loop: do
         call checkread ('MATRL',LU,ios)
         if (ios==0) matrlflag=.true.
         if (ios==1) then
             exit matrl_loop
         end if 
         read(LU,MATRL,err=34,iostat=ios)
         n_thrmp = n_thrmp + 1
34       if (ios>0) then
            write(iofill, '(a,i3,a,i5)') 'Error: Problem with MATRL number ', n_thrmp+1, ', line number ', input_file_line_number
            stop
         end if
      end do matrl_loop

      if (.not.matrlflag) then
         write (*, '(/, "Inputs for MATRL are required.")')
         write (iofill, '(/, "Inputs for MATRL are required.")')
         stop
      end if
  
      if (n_thrmp>mxthrmp) then
         write (*,'(a,i3)') '***Error: Bad MATRL input. Too many thermal properties in input data file. limit is ', mxthrmp
         write (iofill,'(a,i3)') '***Error: Bad MATRL input. Too many thermal properties in input data file. limit is ', mxthrmp
         stop
      end if

      matrl_flag: if (matrlflag) then

        rewind (LU) 
        input_file_line_number = 0
  
        ! Assign value to CFAST variables for further calculations
        read_matrl_loop: do ii=1,n_thrmp

         thrmpptr => thermalinfo(ii)

         call checkread('MATRL',LU,ios)
         call set_matrl_defaults        
         read(LU,MATRL)

         thrmpptr%name          = matrl_id
         thrmpptr%nslab         = nslab
         thrmpptr%k(1)          = k
         thrmpptr%c(1)          = c
         thrmpptr%rho(1)        = rho
         thrmpptr%thickness(1)  = thickness
         thrmpptr%eps           = eps

        end do read_matrl_loop
      
      end if matrl_flag


      contains

      subroutine set_matrl_defaults

      c                      = 0.0_eb        !j/kg-k
      eps                    = 1.0_eb
      k                      = 0.0_eb        !w/m-k
      matrl_id               = 'null'     
      nslab                  = 1          
      rho                    = 0.0_eb        !kg/m3
      thickness              = 0.0_eb        !m

      end subroutine set_matrl_defaults

      end subroutine read_matrl


      ! --------------------------- COMPA -------------------------------------------
      subroutine read_compa(LU,ncomp)

      integer :: ios,ii
      integer :: LU,ncomp
      type(room_type), pointer :: roomptr
      character :: tcname*64

      integer :: xgrid,ygrid,zgrid
      real(eb) :: depth,width,height,x0,y0,z0
      character(128) :: compa_id
      character(mxthrmplen) :: ceiling,floor,wall
      namelist /COMPA/ceiling,compa_id,depth,floor,height,wall,width,x0,xgrid,y0,ygrid,z0,zgrid

      ios = 1

      rewind(LU)
      input_file_line_number = 0
        
      ! Scan entire file to look for 'COMPA'          
      compa_loop: do
         call checkread('COMPA',LU,ios)
         if (ios==0) compaflag=.true.
         if (ios==1) then
             exit compa_loop
         end if 
         read(LU,COMPA,err=34,iostat=ios)
         ncomp = ncomp + 1
34       if (ios>0) then
            write(iofill, '(a,i3,a,i5)') 'Error: Problem with COMPA number ', ncomp+1, ', line number ', input_file_line_number
            stop
         end if
      end do compa_loop

      if (ncomp>mxrooms) then
         write (*,'(a,i3)') '***Error: Bad COMPA input. Too many compartments in input data file. limit is ', mxrooms
         write (iofill,'(a,i3)') '***Error: Bad COMPA input. Too many compartments in input data file. limit is ', mxrooms
         stop
      end if

      if (.not.compaflag) then
         write (*, '(/, "Inputs for COMPA are required.")')
         write (iofill, '(/, "Inputs for COMPA are required.")')
         stop
      end if

      compa_flag: if (compaflag) then

      rewind (LU)
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_compa_loop: do ii=1,ncomp

         roomptr => roominfo(ii)

         call checkread('COMPA',LU,ios)
         call set_compa_defaults
         read(LU,COMPA)

         roomptr%name    = compa_id
         roomptr%cwidth  = width
         roomptr%cdepth  = depth
         roomptr%cheight = height
         roomptr%x0 = x0
         roomptr%y0 = y0
         roomptr%z0 = z0

         ! ceiling
         tcname = ceiling 
         if (tcname/='OFF') then 
            roomptr%surface_on(1) = .true. 
            roomptr%matl(1) = tcname 
         end if

         ! floor
         tcname = floor
         if (tcname/='OFF') then
            roomptr%surface_on(2) = .true.
            roomptr%matl(2) = tcname
         end if

         ! walls
         tcname = wall
         if (tcname/='OFF') then
            roomptr%surface_on(3) = .true.
            roomptr%matl(3) = tcname
            roomptr%surface_on(4) = .true.
            roomptr%matl(4) = tcname
         end if

         roomptr%ibar = xgrid
         roomptr%jbar = ygrid
         roomptr%kbar = zgrid

         ! reset this each time in case this is the last entry
         nr = ncomp + 1

      end do read_compa_loop

      end if compa_flag


      contains

      subroutine set_compa_defaults

      ceiling                 = 'off'
      compa_id                = 'null'
      depth                   = 0.0_eb
      floor                   = 'off'
      height                  = 0.0_eb
      wall                    = 'off'
      width                   = 0.0_eb
      x0                      = 0.0_eb
      xgrid                   = 50
      y0                      = 0.0_eb
      ygrid                   = 50
      z0                      = 0.0_eb
      zgrid                   = 50

      end subroutine set_compa_defaults

      end subroutine read_compa


      ! --------------------------- TARGE -------------------------------------------
      subroutine read_targe(LU)

      integer :: ios,ii,iroom
      integer :: LU
      character :: tcname*64, eqtype*3

      type(target_type), pointer :: targptr

      integer :: compartment
      real(eb) :: x,y,z,internallocation,normalx,normaly,normalz
      character(mxthrmplen) :: material,method
      character :: equationtype*3,targe_id*128
      namelist /TARGE/compartment,x,y,z,normalx,normaly,normalz,material,method,equationtype,internallocation,targe_id

      ios = 1

      rewind(LU)
      input_file_line_number = 0
        
      ! Scan entire file to look for 'TARGE'  
      n_targets = 0
      targe_loop: do
         call checkread ('TARGE',LU,ios)
         if (ios==0) targeflag=.true.
         if (ios==1) then
             exit targe_loop
         end if 
         read(LU,TARGE,err=34,iostat=ios)
         n_targets =n_targets + 1
34          if (ios>0) then
               write(iofill, '(a,i3,a,i5)') 'Error: Problem with TARGE number ', n_targets+1, ', line number ', input_file_line_number
               stop
            end if
      end do targe_loop

      if (n_targets>mxtarg) then
         write (*,'(a,i3)') '***Error: Bad TARGE input. Too many targets in input data file. limit is ', mxtarg
         write (iofill,'(a,i3)') '***Error: Bad TARGE input. Too many targets in input data file. limit is ', mxtarg
         stop
      end if

      targe_flag: if (targeflag) then

      rewind (LU)
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_targe_loop: do ii=1,n_targets

         targptr => targetinfo(ii)

         call checkread('TARGE',LU,ios)
         call set_targe_defaults 
         read(LU,TARGE)

         iroom = compartment

         if (iroom<1.or.iroom>nr) then
            write (*,5003) iroom 
            write (iofill,5003) iroom
            stop
         end if

         targptr%room = iroom

         ! position and normal vector
         targptr%center(1) = x
         targptr%center(2) = y
         targptr%center(3) = z
         targptr%normal(1) = normalx
         targptr%normal(2) = normaly
         targptr%normal(3) = normalz

         targptr%depth_loc = internallocation

         ! target name
         targptr%name = targe_id

         ! material type
         tcname = material
         if (tcname==' ') tcname='DEFAULT'
         targptr%material = tcname
         targptr%wall = 0

         ! equation type, pde or cyl.  ode is outdated and changed to pde if it's in an input file.
         eqtype = ' '
         eqtype = equationtype
         call upperall(eqtype)
         if (eqtype/=' ') then
            if (eqtype(1:3)=='ODE') then
               targptr%equaton_type = pde
               write (*,913) 'Warning', eqtype
               write (iofill,913) 'Warning', eqtype
            else if (eqtype(1:3)=='PDE') then
               targptr%equaton_type = pde
            else if (eqtype(1:3)=='CYL') then
               targptr%equaton_type = cylpde
            else
               write (*,913) 'Error',eqtype
               write (iofill,913) 'Error',eqtype
               stop
            end if
         end if

      end do read_targe_loop

      end if targe_flag

913   format('***',A,': BAD TARGE input. Invalid equation type:',A3,' Valid choices are: PDE or CYL')
5003  format ('***Error: BAD TARGE input. The compartment specified by TARGET does not exist ',i0)



      contains

      subroutine set_targe_defaults

      compartment                     = 0
      x                               = 0.0_eb
      y                               = 0.0_eb
      z                               = 0.0_eb
      normalx                         = 0.0_eb
      normaly                         = 0.0_eb
      normalz                         = 0.0_eb
      material                        = ' '
      method                          = 'EXPLICIT'
      equationtype                    = ' '
      internallocation                = 0.5_eb
      targe_id                        = 'null'

      end subroutine set_targe_defaults


      end subroutine read_targe


      ! --------------------------- CFIRE -------------------------------------------
      subroutine read_cfire(LU)
      
      integer :: ios,ii,i
      integer :: LU
      real(eb) :: tmpcond

      type(fire_type), pointer :: fireptr
      type(room_type), pointer :: roomptr
      type(target_type), pointer :: targptr

      integer :: compartment,plume,dummy1,dummy2,iroom
      real(eb) :: x,y,z,criterion
      character(128) :: fire_id
      character(mxthrmplen) :: type, target
      namelist /CFIRE/compartment,x,y,z,plume,type,criterion,target,dummy1,dummy2,fire_id

      ios = 1
      tmpcond = 0.0

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CFIRE'
      n_fires = 0
      fire_loop: do
         call checkread ('CFIRE', LU, ios)
         if (ios==0) cfireflag=.true.
         if (ios==1) then
             exit fire_loop
         end if 
         read(LU,CFIRE,err=34,iostat=ios)
         n_fires =n_fires + 1
34       if (ios>0) then
            write(iofill, '(a,i3,a,i5)') 'Error: Problem with CFIRE number ', n_fires+1, ', line number ', input_file_line_number
            stop
         end if
      end do fire_loop

      if (n_fires>mxfires) then
         write (*,'(a,i3)') '***Error: Bad CFIRE input. Too many CFIRE in input data file. limit is ', mxfires
         write (iofill,'(a,i3)') '***Error: Bad CFIRE input. Too many CFIRE in input data file. limit is ', mxfires
         stop
      end if

      cfire_flag: if (cfireflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_fire_loop: do ii=1,n_fires

         fireptr => fireinfo(ii)

         call checkread('CFIRE',LU,ios)
         call set_fire_defaults
         read(LU,CFIRE)

         iroom = compartment
         if (iroom<1.or.iroom>nr-1) then
            write (*,5320) iroom
            write (iofill,5320) iroom
            stop
         end if
         roomptr => roominfo(iroom)

         ! Only constrained fires
         fireptr%chemistry_type = 2
         if (fireptr%chemistry_type>2) then
            write (*,5321) fireptr%chemistry_type
            write (iofill,5321) fireptr%chemistry_type
            stop
         end if

         fireptr%x_position = x
         fireptr%y_position = y
         fireptr%z_position = z
         if (fireptr%x_position>roomptr%cwidth.or.fireptr%y_position>roomptr%cdepth.or.fireptr%z_position>roomptr%cheight) then
            write (*,5323) n_fires
            write (iofill,5323) n_fires
            stop
         end if

         fireptr%modified_plume = 1
         if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .or. &
             min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 2
         if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .and. &
             min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 3

         if (type=='TIME' .or. type=='TEMP' .or. type=='FLUX') then
            ! it's a new format fire line that point to an existing target rather than to one created for the fire
            if (type=='TIME') fireptr%ignition_type = trigger_by_time
            if (type=='TEMP') fireptr%ignition_type = trigger_by_temp
            if (type=='FLUX') fireptr%ignition_type = trigger_by_flux
            tmpcond = criterion
            fireptr%ignition_target = 0
            if (type=='TEMP' .or. type=='FLUX') then
               do i = 1,n_targets
                  targptr => targetinfo(i)
                  if (targptr%name==target) fireptr%ignition_target = i
               end do
               if (fireptr%ignition_target==0) then
                  write (*,5324) n_fires
                  write (iofill,5324) n_fires
                  stop
               end if
            end if
         else
            write (*,5322)
            write (iofill,5322)
            stop
         end if

         fireptr%room = iroom
         fireptr%name = fire_id
      
         ! note that ignition type 1 is time, type 2 is temperature and 3 is flux
         if (tmpcond>0.0_eb) then
            fireptr%ignited = .false.
            if (fireptr%ignition_type==trigger_by_time) then
               fireptr%ignition_time = tmpcond
               fireptr%ignition_criterion = 1.0e30_eb
            else if (fireptr%ignition_type==trigger_by_temp.or.fireptr%ignition_type==trigger_by_flux) then
               fireptr%ignition_time = 1.0e30_eb
               fireptr%ignition_criterion = tmpcond
               if (stpmax>0) then
                   stpmax = min(stpmax,1.0_eb)
               else
                   stpmax = 1.0_eb
               end if
            else
               write (*,5358) fireptr%ignition_type
               write (iofill,5358) fireptr%ignition_type
               stop
            end if
         else
            fireptr%ignited  = .true.
            fireptr%reported = .true.
         end if

      end do read_fire_loop

      end if cfire_flag
      
5320  format ('***Error: Bad FIRE input. Fire specification error, room ',i0,' out of range')
5321  format ('***Error: Bad FIRE input. Fire specification error, not an allowed fire type',i0)
5322  format ('***Error: Bad FIRE input. Fire specification is outdated and must include target for ignition')
5323  format ('***Error: Bad FIRE input. Fire location ',i0,' is outside its compartment')
5324  format ('***Error: Bad FIRE input. Target specified for fire ',i0, ' does not exist')
5358  format ('***Error: Bad FIRE input. Not a valid ignition criterion ',i0)


      contains

      subroutine set_fire_defaults

      compartment                     = 0
      x                               = 0.0_eb
      y                               = 0.0_eb
      z                               = 0.0_eb
      plume                           = 1
      type                            = 'null'
      criterion                       = 0.0_eb
      target                          = 'null'
      dummy1                          = 0
      dummy2                          = 0
      fire_id                         = 'null'

      end subroutine set_fire_defaults

      end subroutine read_cfire


      ! --------------------------- CHEMI -------------------------------------------
      subroutine read_chemi(LU)
      
      integer :: ios,ii,i,j
      integer :: LU
      real(eb) :: ohcomb,max_hrr,max_area,flamelength,hrrpm3
      integer :: midpoint = 1, base = 2

      type(fire_type), pointer :: fireptr
      type(room_type), pointer :: roomptr

      integer :: nret
      real(eb) :: c,h,o,n,cl,chir,hoc
      real(eb), dimension(mxpts) :: time,fv,c_co,trace,area,height,c_hrr
      character(128) :: chemi_id
      namelist /CHEMI/chemi_id,c,h,o,n,cl,chir,hoc,nret,time,c_hrr,fv,c_co,trace,area,height

      ios = 1
      nret = 0
      ohcomb = 0.0

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CHEMI'
      n_chemi = 0
      chemi_loop: do
         call checkread ('CHEMI',LU,ios)
         if (ios==0) chemiflag=.true.
         if (ios==1) then
             exit chemi_loop
         end if 
         n_chemi =n_chemi + 1
         read(LU,CHEMI,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(a,i3,a,i5)') 'Error: Problem with CHEMI number ', n_chemi+1, ', line number ', input_file_line_number
             stop
         end if
      end do chemi_loop

      if (cfireflag) then
         if (.not.chemiflag) then
            write (*, '(/, "CFIRE detected. Missing CHEMI")')
            write (iofill, '(/, "CFIRE detected. Missing CHEMI")')
            stop
         end if
      end if

      chemi_flag: if (chemiflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_chemi_loop: do ii=1,n_chemi

         call checkread('CHEMI',LU,ios)
         call set_chemi_defaults
         read(LU,CHEMI)

         match_loop: do j=1,n_fires

            fireptr => fireinfo(j)

            if (chemi_id==fireptr%name) then 
                exit match_loop
            else
                continue
            end if
         end do match_loop   

         if (j==n_fires+1) then
             write (*, '(/, "Chemi_id and Fire_id do not match")')
             write (iofill, '(/, "Chemi_id and Fire_id do not match")')
             stop
         end if

         ! Define chemical formula
         fireptr%n_c  = c
         fireptr%n_h  = h
         fireptr%n_o  = o
         fireptr%n_n  = n
         fireptr%n_cl = cl
         fireptr%molar_mass = (12.01_eb*fireptr%n_c + 1.008_eb*fireptr%n_h + 16.0_eb*fireptr%n_o + &
                               14.01_eb*fireptr%n_n + 35.45_eb*fireptr%n_cl)/1000.0_eb
         fireptr%chirad = chir
         ohcomb = hoc
         if (ohcomb<=0.0_eb) then
            write (*,5001) ohcomb
            write (iofill,5001) ohcomb
            stop
         end if

         ! Define time
         fireptr%npoints = nret
         do i = 1, nret
            fireptr%time(i) = time(i)
         end do

         ! Define hrr
         max_hrr = 0.0_eb
         do i = 1, nret
            fireptr%qdot(i) = c_hrr(i)
            max_hrr = max(max_hrr, fireptr%qdot(i))
            fireptr%mdot(i) = fireptr%qdot(i)/ohcomb
         end do

         ! Define soot
         do i = 1, nret
            fireptr%y_soot(i) = fv(i)
         end do

         ! define co
         do i = 1, nret
            fireptr%y_co(i) = c_co(i)
         end do

         ! define trace
         ! note that ct, tuhc and ts are carried in the mprodr array - all other species have their own array
         do i = 1, nret
            fireptr%y_trace(i) = trace(i)
         end do

         ! define area
         max_area = 0.0_eb
         do i = 1, nret
            ! the minimum area is to stop dassl from a floating point underflow when it tries to extrapolate back to the
            ! ignition point. it only occurs for objects which are on the floor and ignite after t=0. the assumed minimum fire
            ! diameter of 0.2 m below is the minimum valid fire diameter for heskestad's plume correlation
            ! (from sfpe handbook chapter)
            if (area(i)==0.0_eb) then
               write (*,5002)
               write (iofill,5002)
               stop
            end if
            fireptr%area(i) = max(area(i),pio4*0.2_eb**2)
            max_area = max(max_area,fireptr%area(i))
         end do

         ! calculate a characteristic length of an object (we assume the diameter).
         ! this is used for point source radiation fire to target calculation as a minimum effective
         ! distance between the fire and the target which only impact very small fire to target distances
         fireptr%characteristic_length = sqrt(max_area/pio4)
        
         ! define heigh
         do i = 1, nret
            fireptr%height(i) = max(height(i),0.0_eb)
         end do

         ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
         call set_heat_of_combustion (fireptr%npoints, fireptr%mdot, fireptr%qdot, fireptr%hoc, ohcomb)

         ! Position the object
         roomptr => roominfo(fireptr%room)
         call positionobject(fireptr%x_position,roomptr%cwidth,midpoint,mx_hsep)
         call positionobject(fireptr%y_position,roomptr%cdepth,midpoint,mx_hsep)
         call positionobject(fireptr%z_position,roomptr%cheight,base,mx_hsep)

         ! Diagnostic - check for the maximum heat release per unit volume.
         ! First, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
         call flame_height(max_hrr, max_area, flamelength)
         flamelength = max (0.0_eb, flamelength)
            
         ! Now the heat realease per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
         hrrpm3 = max_hrr/(pio4*fireptr%characteristic_length**2*(fireptr%characteristic_length+flamelength))
         if (hrrpm3>4.0e6_eb) then
             write (*,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
             write (*, 5108)
             write (iofill,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
             write (iofill, 5108)
             stop
         else if (hrrpm3>2.0e6_eb) then
             write (*,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
             write (*, 5108)
             write (iofill,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
             write (iofill, 5108)
         end if

      end do read_chemi_loop

      end if chemi_flag

5001  format ('***Error: invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002  format ('***Error: invalid fire area. all input values must be greater than zero')
5106  format ('***Error: object ',a,' position set to ',3f7.3,'; maximum hrr per m^3 = ',1pg10.3,' exceeds physical limits')
5107  format ('Object ',a,' position set to ',3f7.3,'; maximum c_hrr per m^3 = ',1pg10.3,' exceeds nominal limits')
5108  format ('Typically, this is caused by too small fire area inputs. check c_hrr and fire area inputs')
5000  format ('***Error: the key word ',a5,' is not part of a fire definition. fire keyword are likely out of order')

      contains

      subroutine set_chemi_defaults

      chemi_id          = 'null'
      c                 = 0.0_eb
      h                 = 0.0_eb
      o                 = 0.0_eb
      n                 = 0.0_eb
      cl                = 0.0_eb
      chir              = 0.35_eb
      hoc               = 0.0_eb
      nret              = 0
      time(1:mxpts)     = 0.0_eb
      c_hrr(1:mxpts)    = 0.0_eb
      fv(1:mxpts)       = 0.0_eb
      c_co(1:mxpts)     = 0.0_eb
      trace(1:mxpts)    = 0.0_eb
      area(1:mxpts)     = 0.0_eb
      height(1:mxpts)   = 0.0_eb

      end subroutine set_chemi_defaults



      ! --------------------------- set_heat_of_combustion -------------------------------------------

      subroutine set_heat_of_combustion (maxint, mdot, qdot, hdot, hinitial)

      !	Routine to implement the algorithm to set the heat of combustion for all fires

      integer, intent(in) :: maxint
      real(eb), intent(in) :: qdot(maxint), hinitial
      real(eb), intent(out) :: mdot(maxint), hdot(maxint)

      integer :: i
      real(eb) :: hcmax = 1.0e8_eb, hcmin = 1.0e6_eb

      do i = 1, maxint
          if (i>1) then
              if (mdot(i)*qdot(i)<=0.0_eb) then
                  hdot(i) = hinitial
              else
                  hdot(i) = min(hcmax,max(qdot(i)/mdot(i),hcmin))
                  mdot(i) = qdot(i)/hdot(i)
              end if
          else
              hdot(1) = hinitial
          end if
      end do

      return

      end subroutine set_heat_of_combustion



      ! --------------------------- positionobject -------------------------------------------

      subroutine positionobject (xyorz,pos_max,defaultposition,minimumseparation)

      !     routine: positionobject
      !     purpose: Position an object in a compartment
      !     arguments: xyorz: object position in the x, y, or z direction
      !		         pos_max: the maximum extent
      !		         defaultposition: to set to zero (base)(2) or midpoint(1)
      !		         minimumseparation: the closest the object can be to a wall

      integer, intent(in) :: defaultposition
      real(eb), intent(in) :: minimumseparation, pos_max
      real(eb), intent(inout) :: xyorz

      if ((xyorz<0.0_eb).or.(xyorz>pos_max)) then
          select case (defaultposition)
          case (1)
              xyorz = pos_max / 2.0_eb
          case (2)
                xyorz = minimumseparation
            case default
              write (*,*) 'Fire objects positioned specified outside compartment bounds.'
              write (iofill,*) 'Fire objects positioned specified outside compartment bounds.'
              stop
          end select
      else if (xyorz==0.0_eb) then
          xyorz = minimumseparation
      else if (xyorz==pos_max) then
          xyorz = pos_max - minimumseparation
      end if

      return

      end subroutine positionobject



      end subroutine read_chemi


      ! --------------------------- CTIME -------------------------------------------
      subroutine read_ctime(LU)

      integer :: ios
      integer :: LU

      integer :: simulation,print,spreadsheet,smokeview
      namelist /CTIME/simulation,print,spreadsheet,smokeview

      ios = 1
      
      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CTIME'
      time_loop: do
         call checkread ('CTIME',LU,ios)
         if (ios==0) ctimeflag=.true.
         if (ios==1) then
             exit time_loop
         end if 
         read(LU,CTIME,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(a,i5)') 'Error: Problem with CTIME input, line number', input_file_line_number
             stop
         end if
      end do time_loop

      if (.not.ctimeflag) then
         write (*, '(/, "Inputs for CTIME are required.")')
         write (iofill, '(/, "Inputs for CTIME are required.")')
         stop
      end if

      ctime_flag: if (ctimeflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('CTIME',LU,ios)
      call set_time_defaults
      read(LU,CTIME)

      time_end=simulation
      print_out_interval=print
      smv_out_interval=smokeview
      ss_out_interval=spreadsheet

      end if ctime_flag



      contains
      
      subroutine set_time_defaults
      
      simulation             = 0_eb          ! s
      print                  = 50_eb         ! s
      smokeview              = 10_eb         ! s
      spreadsheet            = 10_eb         ! s
      
      end subroutine set_time_defaults
      
      end subroutine read_ctime


      ! --------------------------- TAMBI -------------------------------------------
      subroutine read_tambi(LU,exsets)

      integer :: ios
      integer :: LU
      logical :: exsets

      real(eb) :: temp,pres,rh
      namelist /TAMBI/temp,pres,rh

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'TAMBI'
      tamb_loop: do
         call checkread ('TAMBI',LU,ios)
         if (ios==0) tambiflag=.true.
         if (ios==1) then
             exit tamb_loop
         end if 
         read(LU,TAMBI,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(a,i5)') 'Error: Problem with TAMBI number, line number', input_file_line_number
             stop
          end if
      end do tamb_loop

      tambi_flag: if (tambiflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('TAMBI',LU,ios)
      call set_tamb_defaults
      read(LU,TAMBI)
      
      interior_temperature  = temp
      interior_abs_pressure = pres
      relative_humidity     = rh*0.01_eb
      if (.not.exsets) then
          exterior_temperature = interior_temperature
          exterior_abs_pressure = interior_abs_pressure
          exterior_rho = interior_rho
      end if

      tgignt = interior_temperature + 200.0_eb

      end if tambi_flag
      
      contains
      
      subroutine set_tamb_defaults
      
      temp                     = 293.15_eb        !K
      pres                     = 101325.0_eb      !Pa
      rh                       = 50_eb            !Percentage
      
      end subroutine set_tamb_defaults
      
      end subroutine read_tambi


      ! --------------------------- EAMBI -------------------------------------------
      subroutine read_eambi(LU,exsets)

      integer :: ios
      integer :: LU
      logical :: exsets
      
      real(eb) :: temp,pres
      namelist /EAMBI/temp,pres

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'EAMBI'
      eamb_loop: do
         call checkread ('EAMBI',LU,ios)
         if (ios==0) eambiflag=.true.
         if (ios==1) then
             exit eamb_loop
         end if 
         read(LU,EAMBI,err=34,iostat=ios)
         34 if (ios>0) then
         write(iofill, '(a,i5)') 'Error: Problem with EAMBI number, line number', input_file_line_number
         stop
      end if
      end do eamb_loop

      eambi_flag: if (eambiflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('EAMBI',LU,ios)
      call set_eamb_defaults
      read(LU,EAMBI)
      
      exterior_temperature  = temp
      exterior_abs_pressure = pres
      exsets = .true.

      end if eambi_flag
      
           
      contains
      
      subroutine set_eamb_defaults
      
      temp                        = 293.15_eb        !K
      pres                        = 101325.0_eb      !Pa
      
      end subroutine set_eamb_defaults
      
      end subroutine read_eambi


      ! --------------------------- LIMO2 -------------------------------------------
      subroutine read_limo2(LU)
      
      integer :: ios,LU
      real(eb) :: o2index
      namelist /LIMO2/o2index

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'LIMO2'
      limo2_loop: do
         call checkread ('LIMO2',LU,ios)
         if (ios==0) limo2flag=.true.
         if (ios==1) then
             exit limo2_loop
         end if 
         read(LU,LIMO2,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(a,i5)') 'Error: Problem with LIMO2 number, line number', input_file_line_number
             stop
         end if
      end do limo2_loop

      limo2_flag: if (limo2flag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('LIMO2',LU,ios)
      call set_limo2_defaults
      read(LU,LIMO2)
      
      lower_o2_limit  = o2index

      end if limo2_flag
      
      contains
      
      subroutine set_limo2_defaults
      
      o2index                  = 0.15_eb
      
      end subroutine set_limo2_defaults
      
      end subroutine read_limo2


      ! --------------------------- HVENT -------------------------------------------
      subroutine read_hvent(LU)
      
      integer :: ios,ii,i,j,k,imin,jmax
      integer :: LU
      
      type(room_type), pointer :: roomptr
      type(target_type), pointer :: targptr
      type(vent_type), pointer :: ventptr
      
      integer   :: first,second,hvent_id,face
      real(eb)      :: width,soffit,sill,offset1,offset2,criterion,initialtime,finaltime,initialfraction,finalfraction
      character(48) :: type,target
      namelist /HVENT/first,second,hvent_id,width,soffit,sill,offset1,offset2,face,type, &
                      criterion,target,initialtime,initialfraction,finaltime,finalfraction

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'HVENT'
      n_hvents = 0
      hvent_loop: do
         call checkread ('HVENT',LU,ios)
         if (ios==0) hventflag=.true.
         if (ios==1) then
             exit hvent_loop
         end if 
         read(LU,HVENT,err=34,iostat=ios)
         n_hvents =n_hvents + 1
         34 if (ios>0) then
               write(iofill, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_hvents+1, ', line number ', input_file_line_number
               stop
            end if
      end do hvent_loop

      if (n_hvents>mxhvents) then
         write (*,'(a,i3)') '***Error: Bad hvent input. Too many hvent in input data file. limit is ', mxhvents
         write (iofill,'(a,i3)') '***Error: Bad hvent input. Too many hvent in input data file. limit is ', mxhvents
         stop
      end if

      hvent_flag: if (hventflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_hvent_loop: do ii=1,n_hvents

         call checkread('HVENT',LU,ios)
         call set_hvent_defaults
         read(LU,HVENT)
    
         i = first
         j = second
         k = hvent_id
         imin = min(i,j)
         jmax = max(i,j)
      
         if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
            write (*,5070) i, j
            write (iofill,5070) i, j
            stop
         end if
         if (k>mxccv) then
            write (*,5080) i, j, k
            write (iofill,5080) i, j, k
            stop
         end if
      
         ventptr => hventinfo(ii)
         ventptr%room1 = imin
         ventptr%room2 = jmax
         ventptr%counter = hvent_id
      
         if (n_hvents>mxhvents) then
            write (*,5081) i,j,k
            write (iofill,5081) i,j,k
            stop
         end if
      
         ventptr%width  = width
         ventptr%soffit = soffit
         ventptr%sill   = sill
      
         if  (type=='TIME' .or. type=='TEMP' .or. type=='FLUX') then
             ventptr%offset(1) = offset1
             ventptr%offset(2) = offset2
             ventptr%face = face
             if (type=='TIME') then
                ventptr%opening_type = trigger_by_time
                ventptr%opening_initial_time     = initialtime
                ventptr%opening_initial_fraction = initialfraction
                ventptr%opening_final_time       = finaltime
                ventptr%opening_final_fraction   = finalfraction
             else
                if (type=='TEMP') ventptr%opening_type = trigger_by_temp
                if (type=='FLUX') ventptr%opening_type = trigger_by_flux
                ventptr%opening_criterion = criterion
                ventptr%opening_target = 0
                do i = 1,n_targets
                   targptr => targetinfo(i)
                   if (targptr%name==target) ventptr%opening_target = i
                end do
                if (ventptr%opening_target==0) then
                   write (*,*) '***Error: Bad hvent input. Vent opening specification requires an associated target.'
                   write (iofill,*) '***Error: Bad hvent input. Vent opening specification requires an associated target.'
                   stop
                end if   
                ventptr%opening_initial_fraction = initialfraction
                ventptr%opening_final_fraction   = finalfraction
                if (stpmax>0) then
                stpmax = min(stpmax,1.0_eb)
                else
                stpmax = 1.0_eb
                end if
             end if
         else
            write (*,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            write (iofill,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if

         roomptr => roominfo(ventptr%room1)
         ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
         ventptr%absolute_sill = ventptr%sill + roomptr%z0

      end do read_hvent_loop

      end if hvent_flag

5070  format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)
5080  format ('***Error: Bad HVENT input. Too many pairwise horizontal connections',3I5)
5081  format ('***Error: Too many horizontal connections ',3i5)
      
      contains
      
      subroutine set_hvent_defaults
      
      first                   = 0
      second                  = 0
      hvent_id                = 0
      width                   = 0.0_eb
      soffit                  = 0.0_eb
      sill                    = 0.0_eb
      offset1                 = 0.0_eb
      offset2                 = 0.0_eb
      face                    = 0
      type                    = 'null'
      criterion               = 0.0_eb
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0_eb
      finaltime               = 1.0_eb
      finalfraction           = 0.0_eb
      
      end subroutine set_hvent_defaults
      
      end subroutine read_hvent


      ! --------------------------- DEADR -------------------------------------------
      subroutine read_deadr(LU)
      
      integer :: ios,i,j,k
      integer :: LU

      type(room_type), pointer :: roomptr
      
      integer :: compartment,connected
      namelist /DEADR/compartment,connected

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'DEADR'
      nmlcount=0
      deadr_loop: do
         call checkread ('DEADR',LU,ios)
         if (ios==0) deadrflag=.true.
         if (ios==1) then
             exit deadr_loop
         end if 
         read(LU,DEADR,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
             write(iofill, '(a,i5)') 'Error: Problem with deadr number, line number', input_file_line_number
             stop
         end if
      end do deadr_loop

      deadr_flag: if (deadrflag) then  

      rewind (LU) 
      input_file_line_number = 0      
      
      countloop: do k=1,nmlcount

      call checkread('DEADR',LU,ios)
      call set_deadr_defaults
      read(LU,DEADR)
      
      i = compartment
      j = connected
      
      if (i.ge.1.and.i.le.mxrooms.and.j.le.1.and.j.le.mxrooms.and.i.ne.j) then
         roomptr => roominfo(i)
         roomptr%deadroom = j
      end if
      
      end do countloop
      
      end if deadr_flag                  


      contains
      
      subroutine set_deadr_defaults
      
      compartment                  = 0
      connected                    = 0
      
      end subroutine set_deadr_defaults
      
      end subroutine read_deadr


      ! --------------------------- EVENT -------------------------------------------
      subroutine read_event(LU)

      integer :: ios,i,j,k,iijk,fannumber,kk
      integer :: LU
      character(48) :: venttype

      type(vent_type), pointer :: ventptr

      integer :: first,second,event_id
      real(eb) :: fraction,decay,time
      character(48) :: type
      namelist /EVENT/first,second,event_id,type,time,fraction,decay

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'EVENT'
      nmlcount=0
      event_loop: do
         call checkread ('EVENT',LU,ios)
         if (ios==0) eventflag=.true.
         if (ios==1) then
             exit event_loop
         end if 
         read(LU,EVENT,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
            write(iofill, '(a,i5)') 'Error: Problem with EVENT number, line number', input_file_line_number
            stop
         end if
      end do event_loop

      event_flag: if (eventflag) then

      rewind (LU) 
      input_file_line_number = 0
      
      countloop: do kk=1,nmlcount

      call checkread('EVENT',LU,ios)
      call set_event_defaults
      read(LU,EVENT)
      
      venttype = type
      
      if (fraction<0.0_eb.or.fraction>1.0_eb) then
         write (iofill,*) '****Error: Bad event input. Final_fraction must be between 0 and 1 inclusive.'
         stop
      end if
      
      select case (venttype)
      case ('H')
         i = first
         j = second
         k = event_id
         do iijk = 1, n_hvents
            ventptr => hventinfo(iijk)
            if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
               ventptr%opening_initial_time = time
               ventptr%opening_final_time = time + decay
               ventptr%opening_final_fraction = fraction
            end if
         end do
      case ('V')
         i = first
         j = second
         k = event_id
         do iijk = 1, n_vvents
            ventptr => vventinfo(iijk)
            if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
               ventptr%opening_initial_time = time
               ventptr%opening_final_time = time + decay
               ventptr%opening_final_fraction = fraction
            end if
         end do
      case ('M')
         i = first
         j = second
         k = event_id
         do iijk = 1, n_mvents
            ventptr => mventinfo(iijk)
            if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
               ventptr%opening_initial_time = time
               ventptr%opening_final_time = time + decay
               ventptr%opening_final_fraction = fraction
            end if
         end do
      case ('F')
         fannumber = event_id
         if (fannumber>n_mvents) then
            write (*,5196) fannumber
            write (iofill,5196) fannumber
            stop
         end if
         ventptr => mventinfo(fannumber)
         ventptr%filter_initial_time = time
         ventptr%filter_final_time = time + decay
         ventptr%filter_final_fraction = fraction
      case default
         write (*,*) '***Error: Bad event input. Type must be H, V, M, or F.'
         write (iofill,*) '***Error: Bad event input. Type must be H, V, M, or F.'
         stop
      end select

      end do countloop
      end if event_flag

5196  format ('***Error: Bad EVENT input. Fan has not been defined for this filter ',i0)
      
      contains
      
      subroutine set_event_defaults
      
      first                   = 0
      second                  = 0
      event_id                = 0
      type                    = 'null'
      time                    = 0.0_eb
      fraction                = 0.0_eb
      decay                   = 0.0_eb
      
      end subroutine set_event_defaults
      
      end subroutine read_event


      ! --------------------------- CRAMP -------------------------------------------
      subroutine read_cramp(LU)
           
      integer :: ios,ii,iramp
      integer :: LU

      type(ramp_type), pointer :: rampptr
      
      integer :: first,second,ramp_id,ramp_n
      real(eb), dimension(mxpts) :: time,fraction
      character(64) :: type
      namelist /CRAMP/type,first,second,ramp_id,ramp_n,time,fraction

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CRAMP'
      nramps = 0
      ramp_loop: do
         call checkread ('CRAMP',LU,ios)
         if (ios==0) crampflag=.true.
         if (ios==1) then
             exit ramp_loop
         end if 
         read(LU,CRAMP,err=34,iostat=ios)
         nramps =nramps + 1
34       if (ios>0) then
             write(iofill, '(a,i3,a,i5)') 'Error: Problem with CRAMP number ', nramps+1, ', line number ', input_file_line_number
             stop
         end if
      end do ramp_loop

      if (nramps>mxramps) then
         write (*,'(a,i3)') '***Error: Bad ramp input. Too many ramp in input data file. limit is ', mxramps
         write (iofill,'(a,i3)') '***Error: Bad ramp input. Too many ramp in input data file. limit is ', mxramps
         stop
      end if

      cramp_flag: if (crampflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations      
      read_ramp_loop: do ii=1,nramps
      
         call checkread('CRAMP',LU,ios)
         call set_ramp_defaults
         read(LU,CRAMP)
      
         rampptr=>rampinfo(ii)
         rampptr%type    = type
         rampptr%room1   = first
         rampptr%room2   = second
         rampptr%counter = ramp_id
         rampptr%npoints = ramp_n
         do iramp = 1,rampptr%npoints
            rampptr%time(iramp)  = time(iramp)
            rampptr%value(iramp) = fraction(iramp)
         end do
      
      end do read_ramp_loop

      end if cramp_flag
      
      
      contains
      
      subroutine set_ramp_defaults
      
      type                            = 'null'
      first                           = 0 
      second                          = 0 
      ramp_id                         = 0
      ramp_n                          = 0 
      time                            = 0.0_eb
      fraction                        = 0.0_eb
      
      end subroutine set_ramp_defaults
      
      end subroutine read_cramp


      ! --------------------------- VVENT -------------------------------------------
      subroutine read_vvent(LU)
      
      integer :: ios,ii,i,j,k
      integer :: LU
      
      type(target_type), pointer :: targptr
      type(vent_type), pointer :: ventptr
      
      integer   :: top,bottom,vvent_id,shape,initialtime,finaltime
      real(eb)      :: area,offsetx,offsety,criterion,initialfraction,finalfraction
      character(64) :: type,target
      namelist /VVENT/top,bottom,vvent_id,area,shape,type,criterion,target,initialtime, &
                      initialfraction,finaltime,finalfraction,offsetx,offsety

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'VVENT'
      n_vvents = 0
      vvent_loop: do
         call checkread ('VVENT',LU,ios)
         if (ios==0) vventflag=.true.
         if (ios==1) then
             exit vvent_loop
         end if 
         read(LU,VVENT,err=34,iostat=ios)
         n_vvents =n_vvents + 1
34       if (ios>0) then
             write(iofill, '(a,i3,a,i5)') 'Error: Problem with VVENT number ', n_vvents+1, ', line number ', input_file_line_number
             stop
         end if
      end do vvent_loop

      if (n_vvents>mxvvents) then
         write (*,'(a,i3)') '***Error: Bad VVENT input. Too many vvent in input data file. limit is ', mxvvents
         write (iofill,'(a,i3)') '***Error: Bad VVENT input. Too many vvent in input data file. limit is ', mxvvents
         stop
      end if

      vvent_flag: if (vventflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_vvent_loop: do ii=1,n_vvents
      
         call checkread('VVENT',LU,ios)
         call set_vvent_defaults
         read(LU,VVENT)
      
         i = top
         j = bottom
         k = vvent_id
      
         ! check for outside of compartment space; self pointers are covered in read_input_file
         if (i>mxrooms.or.j>mxrooms) then
            write (*,5070) i, j
            write (iofill,5070) i, j
            stop
         end if
      
         ventptr => vventinfo(ii)
         ventptr%room1   = i
         ventptr%room2   = j
         ventptr%counter = k
         ! read_input_file will verify the orientation (i is on top of j)
         ventptr%area = area
         ! check the shape parameter. the default (1) is a circle)
         if (shape<1.or.shape>2) then
            ventptr%shape = 1
         else
            ventptr%shape = shape
         end if
         if (type=='TIME' .or. type=='TEMP' .or. type=='FLUX') then
            if (type=='TIME') then
               ventptr%opening_type = trigger_by_time
               ventptr%opening_initial_time = initialtime
               ventptr%opening_initial_fraction = initialfraction
               ventptr%opening_final_time = finaltime
               ventptr%opening_final_fraction = finalfraction
            else
               if (type=='TEMP') ventptr%opening_type = trigger_by_temp
               if (type=='FLUX') ventptr%opening_type = trigger_by_flux
               ventptr%opening_criterion = criterion
               ventptr%opening_target = 0
               do i = 1,n_targets
                  targptr => targetinfo(i)
                  if (targptr%name==target) ventptr%opening_target = i
               end do
               if (ventptr%opening_target==0) then
                  write (*,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                  write (iofill,*) '***Error: Bad hvent input. Vent opening specification requires an associated target.'
                  stop
               end if  
               ventptr%opening_initial_fraction = initialfraction
               ventptr%opening_final_fraction = finalfraction
               if (stpmax>0) then
                  stpmax = min(stpmax,1.0_eb)
               else
                  stpmax = 1.0_eb
               end if
            end if
            ventptr%xoffset = offsetx
            ventptr%yoffset = offsety
         else
            write (*,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            write (iofill,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if
      
      end do read_vvent_loop

      end if vvent_flag
      
5070  format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)

      
      contains
      
      subroutine set_vvent_defaults
      
      top                     = 0
      bottom                  = 0
      vvent_id                = 0
      area                    = 0.0_eb
      shape                   = 1
      type                    = 'null'
      criterion               = 0.0_eb
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0_eb
      finaltime               = 1.0_eb
      finalfraction           = 0.0_eb
      offsetx                 = 0.0_eb
      offsety                 = 0.0_eb
      
      end subroutine set_vvent_defaults
      
      end subroutine read_vvent


      ! --------------------------- MVENT -------------------------------------------
      subroutine read_mvent(LU)
      
      integer :: ios,ii,i,j,k
      integer :: LU
      
      type(vent_type), pointer :: ventptr
      type(target_type), pointer :: targptr

      integer :: first,second,mvent_id
      real(eb) :: height1,area1,height2,area2,plower,pupper,offsetx,offsety,criterion, &
                  initialfraction,finalfraction,initialtime,finaltime,flow
      character(48) :: type,target
      character(48) :: orientation1,orientation2
      namelist /MVENT/first,second,mvent_id,orientation1,height1,area1,orientation2, &
                      height2,area2,flow,plower, pupper,type,criterion,target,initialtime, &
                      initialfraction,finaltime,finalfraction,offsetx,offsety

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'MVENT'
      n_mvents = 0
      mvent_loop: do
         call checkread ('MVENT',LU,ios)
         if (ios==0) mventflag=.true.
         if (ios==1) then
             exit mvent_loop
         end if 
         read(LU,MVENT,err=34,iostat=ios)
         n_mvents =n_mvents + 1
34       if (ios>0) then
             write(iofill, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_mvents+1, ', line number ', input_file_line_number
             stop
          end if
      end do mvent_loop

      if (n_mvents>mxmvents) then
         write (*,'(a,i3)') '***Error: Bad MVENT input. Too many mvent in input data file. limit is ', mxmvents 
         write (iofill,'(a,i3)') '***Error: Bad MVENT input. Too many mvent in input data file. limit is ', mxmvents
         stop
      end if

      mvent_flag: if (mventflag) then

      rewind (LU) 
      input_file_line_number = 0
      
      ! Assign value to CFAST variables for further calculations
      read_mvent_loop: do ii=1,n_mvents
      
         call checkread('MVENT',LU,ios)
         call set_mvent_defaults
         read(LU,MVENT)
      
         i = first
         j = second
         k = mvent_id
         if (i>nr.or.j>nr) then
            write (*,5191) i, j
            write (iofill,5191) i, j
            stop
         end if
      
         ventptr => mventinfo(ii)
         ventptr%room1 = i
         ventptr%room2 = j
         ventptr%counter = k
      
         if (orientation1=='V') then
            ventptr%orientation(1) = 1
         else
            ventptr%orientation(1) = 2
         end if
         ventptr%height(1)        = height1
         ventptr%diffuser_area(1) = area1
      
         if (orientation2=='V') then
            ventptr%orientation(2) = 1
         else
            ventptr%orientation(2) = 2
         end if
         ventptr%height(2)        = height2
         ventptr%diffuser_area(2) = area2
      
         ventptr%n_coeffs = 1
         ventptr%coeff = 0.0_eb
         ventptr%coeff(1) = flow
         ventptr%maxflow = flow
         ventptr%min_cutoff_relp = plower
         ventptr%max_cutoff_relp = pupper
      
         if (type=='TIME' .or. type=='TEMP' .or. type=='FLUX') then
            if (type=='TIME') then
               ventptr%opening_type = trigger_by_time
               ventptr%opening_initial_time = initialtime
               ventptr%opening_initial_fraction = initialfraction
               ventptr%opening_final_time = finaltime
               ventptr%opening_final_fraction = finalfraction
            else
               if (type=='TEMP') ventptr%opening_type = trigger_by_temp
               if (type=='FLUX') ventptr%opening_type = trigger_by_flux
               ventptr%opening_criterion = criterion
               ventptr%opening_target = 0
               do i = 1,n_targets
                  targptr => targetinfo(i)
                  if (targptr%name==target) ventptr%opening_target = i
               end do
               if (ventptr%opening_target==0) then
                  write (*,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                  write (iofill,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                  stop
               end if  
               ventptr%opening_initial_fraction = initialfraction
               ventptr%opening_final_fraction = finalfraction
            end if
            ventptr%xoffset = offsetx
            ventptr%yoffset = offsety
            if (stpmax>0) then
               stpmax = min(stpmax,1.0_eb)
            else
               stpmax = 1.0_eb
            end if
         else
            write (*,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            write (iofill,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if
      
      end do read_mvent_loop

      end if mvent_flag
      
5191  format ('***Error: Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)
      

      contains
      
      subroutine set_mvent_defaults
      
      first                   = 0
      second                  = 0
      mvent_id                = 0
      orientation1            = ''
      height1                 = 0.0_eb
      area1                   = 0.0_eb
      orientation2            = ''
      height2                 = 0.0_eb
      area2                   = 0.0_eb
      flow                    = 0.0_eb
      plower                  = 0.0_eb
      pupper                  = 0.0_eb
      type                    = 'null'
      criterion               = 0.0_eb
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0_eb
      finaltime               = 1.0_eb
      finalfraction           = 0.0_eb
      offsetx                 = 0.0_eb
      offsety                 = 0.0_eb
      
      end subroutine set_mvent_defaults
      
      end subroutine read_mvent


      ! --------------------------- DETEC -------------------------------------------
      subroutine read_detec(LU)
              
      integer :: ios,ii,i1,i2,iroom
      integer :: LU

      type(detector_type), pointer :: dtectptr
      type(room_type), pointer :: roomptr

      integer :: type,compartment,suppression
      real(eb) :: trigger,x,y,z,rti,spray_density
      namelist /detec/type,compartment,trigger,x,y,z,rti,suppression,spray_density

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'DETEC'
      n_detectors = 0
      detec_loop: do
         call checkread ('DETEC', LU, ios)
         if (ios==0) detecflag=.true.
         if (ios==1) then
             exit detec_loop
         end if 
         if (ios==1) exit detec_loop
         read(LU,detec,err=34,iostat=ios)
         n_detectors =n_detectors + 1
34       if (ios>0) then
             write(iofill, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_detectors+1, ', line number ', input_file_line_number
             stop
          end if
        end do detec_loop

        if (n_detectors>mxdtect) then 
           write (*,'(a,i3)') '***Error: Bad DETEC input. Too many mvent in input data file. limit is ', mxdtect 
           write (iofill,'(a,i3)') '***Error: Bad DETEC input. Too many mvent in input data file. limit is ', mxdtect
           stop
        end if

        detec_flag: if (detecflag) then

        rewind (LU) 
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_detec_loop: do ii=1,n_detectors
        
           call checkread('DETEC',LU,ios)
           call set_detec_defaults
           read(LU,DETEC)
        
           dtectptr => detectorinfo(ii)
        
           i1 = type
           ! Force to heat detector if out of range
           if (i1>3) i1 = heatd
        
           dtectptr%dtype = i1
        
           i2 = compartment
           iroom = i2
           dtectptr%room = iroom
           if (iroom<1.or.iroom>mxrooms) then
              write (*,5342) i2
              write (iofill,5342) i2
              stop
           end if
        
           dtectptr%trigger = trigger
           dtectptr%center(1) = x
           dtectptr%center(2) = y
           dtectptr%center(3) = z
           dtectptr%rti =  rti

           if (suppression/=0) then
              dtectptr%quench = .true.
           else
              dtectptr%quench = .false.
           end if
           dtectptr%spray_density = spray_density*1000.0_eb

           ! if spray density is zero, then turn off the sprinkler
           if (dtectptr%spray_density==0.0_eb) then
              dtectptr%quench = .false.
           end if
           ! if there's a sprinkler that can go off, then make sure the time step is small enough to report it accurately
           if (dtectptr%quench) then
              if (stpmax>0) then
                 stpmax = min(stpmax,1.0_eb)
              else
                 stpmax = 1.0_eb
              end if
           end if

           roomptr => roominfo(iroom)
           if (roomptr%name==' ') then !check consistency
              write (*,5344) i2
              write (iofill,5344) i2
              stop
           end if
        
           if (dtectptr%center(1)>roomptr%cwidth.or. &
               dtectptr%center(2)>roomptr%cdepth.or.dtectptr%center(3)>roomptr%cheight) then
              write (*,5339) n_detectors,roomptr%name
              write (iofill,5339) n_detectors,roomptr%name
              stop
           end if
        
        end do read_detec_loop

        end if detec_flag
        
5339    format ('***Error: Bad DETEC input. Detector ',i0,' is outside of compartment ',a)
5342    format ('***Error: Bad DETEC input. Invalid DETECTOR specification - room ',i0)
5344    format ('***Error: Bad DETEC input. A referenced compartment is not yet defined ',i0)


        contains
        
        subroutine set_detec_defaults
        
        type                            = 2
        compartment                     = 1
        trigger                         = 330.3722_eb
        x                               = -1.0_eb
        y                               = -1.0_eb
        z                               = -3.0_eb/39.37_eb
        rti                             = 50.0_eb
        suppression                     = 0
        spray_density                   = -300.0_eb
        
        end subroutine set_detec_defaults
        
        end subroutine read_detec


      ! --------------------------- VHEAT -------------------------------------------
      subroutine read_vheat(LU)
      
      integer :: ios,ii,i1,i2
      integer :: LU

      integer :: top,bottom
      namelist /VHEAT/top,bottom

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'VHEAT'
      nvcons=0
      vheat_loop: do
      call checkread ('VHEAT',LU,ios)
         if (ios==0) vheatflag=.true.
         if (ios==1) then
             exit vheat_loop
         end if 
         read(LU,VHEAT,err=34,iostat=ios)
         nvcons = nvcons + 1
34       if (ios>0) then
               write(iofill, '(a,i5)') 'Error: Problem with VHEAT number, line number', input_file_line_number
               stop
            end if
      end do vheat_loop

      vheat_flag: if (vheatflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_vheat_loop: do ii=1,nvcons
      
         call checkread('VHEAT',LU,ios)
         call set_vheat_defaults
         read(LU,VHEAT)
      
         i1 = top
         i2 = bottom
         if (i1<1.or.i2<1.or.i1>nr.or.i2>nr) then
            write (*,5345) i1, i2
            write (iofill,5345) i1, i2
            stop
         end if
      
         i_vconnections(ii,w_from_room) = i1
         i_vconnections(ii,w_from_wall) = 2
         i_vconnections(ii,w_to_room) = i2
         i_vconnections(ii,w_to_wall) = 1
      
      end do read_vheat_loop

      end if vheat_flag
      
5345  format ('***Error: Bad VHEAT input. A referenced compartment does not exist')
      

      contains
      
      subroutine set_vheat_defaults
      
      top                       = 0
      bottom                    = 0
      
      end subroutine set_vheat_defaults
      
      end subroutine read_vheat
      
     
      ! --------------------------- CONEZ -------------------------------------------
      subroutine read_conez(LU)
      
      integer :: ios,iroom,i
      integer :: LU
      
      type(room_type), pointer :: roomptr
      
      integer   :: compartment
      namelist /CONEZ/compartment

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CONEZ'
      nmlcount=0
      onez_loop: do
         call checkread ('CONEZ',LU,ios)
         if (ios==0) conezflag=.true.
         if (ios==1) then
             exit onez_loop
         end if 
         read(LU,CONEZ,err=34,iostat=ios)
         nmlcount= nmlcount + 1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with fire number, line number', input_file_line_number
             stop
         end if
      end do onez_loop

      conez_flag: if (conezflag) then

      rewind (LU) 
      input_file_line_number = 0

      do i=1,nmlcount
         call checkread('CONEZ',LU,ios)
         call set_onez_defaults
         read(LU,CONEZ)
      
         iroom = compartment
         if (iroom<1.or.iroom>nr) then
            write (*, 5001) iroom
            write (iofill, 5001) iroom
            stop
         end if
         roomptr => roominfo(iroom)
         roomptr%shaft = .true.
      
      end do

      end if conez_flag
      
5001  format ('***Error: Bad ONEZ input. Referenced compartment is not defined ',i0)
      
      contains
      
      subroutine set_onez_defaults
      
      compartment                     = 0
      
      end subroutine set_onez_defaults
      
      end subroutine read_conez


      ! --------------------------- CHALL -------------------------------------------
      subroutine read_chall(LU)
      
      integer :: ios,iroom,i
      integer :: LU
      
      type(room_type), pointer :: roomptr

      integer :: compartment
      namelist /CHALL/compartment

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CHALL'
      nmlcount=0
      hall_loop: do
         call checkread ('CHALL',LU,ios)
         if (ios==0) challflag=.true.
         if (ios==1) then
             exit hall_loop
         end if 
         read(LU,CHALL,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with CHALL number, line number', input_file_line_number
             stop
         end if
      end do hall_loop

      chall_flag: if (challflag) then

      rewind (LU) 
      input_file_line_number = 0

      countloop : do i=1,nmlcount
          
      call checkread('CHALL',LU,ios)
      call set_hall_defaults
      read(LU,CHALL)
      
      iroom = compartment
      if (iroom<1.or.iroom>nr) then
         write (*, 5346) iroom
         write (iofill, 5346) iroom
         stop
      end if
      roomptr => roominfo(iroom)
      roomptr%hall = .true.

      end do countloop
      
      end if chall_flag
      
5346  format ('***Error: Bad HALL input. A referenced compartment does not exist ',i0)

      contains
      
      subroutine set_hall_defaults
      
      compartment                     = 0
      
      end subroutine set_hall_defaults
      
      end subroutine read_chall


      ! --------------------------- ROOMA -------------------------------------------
      subroutine read_rooma(LU)
      
      integer :: ios,iroom,i,npts,k
      integer :: LU
      
      type(room_type), pointer :: roomptr

      integer :: compartment,number
      real(eb), dimension(mxpts) :: area
      namelist /rooma/compartment,number,area

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'ROOMA'
      nmlcount=0
      rooma_loop: do
         call checkread ('ROOMA',LU,ios)
         if (ios==0) roomaflag=.true.
         if (ios==1) then
             exit rooma_loop
         end if 
         read(LU,ROOMA,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
               write(iofill, '(A,i5)') 'Error: Problem with ROOMA number, line number', input_file_line_number
               stop
         end if
      end do rooma_loop

      rooma_flag: if (roomaflag) then

      rewind (LU) 
      input_file_line_number = 0
      
      countloop : do k=1,nmlcount

      call checkread('ROOMA',LU,ios)
      call set_rooma_defaults
      read(LU,ROOMA)
      
      iroom = compartment
      roomptr => roominfo(iroom)
      
      ! make sure the room number is valid
      if (iroom<1.or.iroom>nr) then
         write (*,5347) iroom
         write (iofill,5347) iroom
         stop
      end if
      
      ! make sure the number of points is valid
      npts = number
      if (npts>mxcross.or.npts<=0) then
         write (*,5347) npts
         write (iofill,5347) npts
         stop
      end if
      if (roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
      roomptr%nvars = npts
      
      ! make sure all data is positive
      do  i = 1, npts
         if (area(i)<0.0_eb) then
            write (*,5348) area(i)
            write (iofill,5348) area(i)
            stop
         end if
      end do
      
      ! put the data in its place
      do i = 1, npts
         roomptr%var_area(i) = area(i)
      end do
      
      end do countloop

      end if rooma_flag
      
5347  format ('***Error: Bad ROOMA input. Compartment specified by ROOMA does not exist ',i0)
5348  format ('***Error: Bad ROOMA or ROOMH input. Data on the ROOMA (or H) line must be positive ',1pg12.3)

      contains
      
      subroutine set_rooma_defaults
      
      compartment                     = 0 
      number                          = 0 
      area(1:mxpts)                   = 0.0_eb
      
      end subroutine set_rooma_defaults
      
      end subroutine read_rooma


      ! --------------------------- ROOMH -------------------------------------------
      subroutine read_roomh(LU)

      integer :: ios,iroom,i,npts,k
      integer :: LU
      
      type(room_type), pointer :: roomptr
      integer :: compartment,number
      real(eb), dimension(mxpts) :: height
      namelist /ROOMH/compartment,number,height

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'ROOMH'
      nmlcount=0
      roomh_loop: do
         call checkread ('ROOMH',LU,ios)
         if (ios==0) roomhflag=.true.
         if (ios==1) then
             exit roomh_loop
         end if 
         read(LU,ROOMH,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with ROOMH number, line number', input_file_line_number
             stop
         end if
      end do roomh_loop

      roomh_flag: if (roomhflag) then

      rewind (LU) 
      input_file_line_number = 0
      
      countloop : do k=1,nmlcount

      call checkread('ROOMH',LU,ios)
      call set_roomh_defaults
      read(LU,ROOMH)
      
      iroom = compartment
      roomptr => roominfo(iroom)
      
      ! make sure the room number is valid
      if (iroom<1.or.iroom>nr) then
         write (*,5349) iroom
         write (iofill,5349) iroom
         stop
      end if
      
      ! make sure the number of points is valid
      npts = number
      if (npts>mxcross.or.npts<=0) then
         write (*,5350) npts
         write (iofill,5350) npts
         stop
      end if
      if (roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
      roomptr%nvars = npts
      
      ! make sure all data is positive
      do  i = 1, npts
         if (height(i)<0.0_eb) then
            write (*,5348) height(i)
            write (iofill,5348) height(i)
            stop
         end if
      end do
      
      ! put the data in its place
      do i = 1, npts
         roomptr%var_height(i) = height(i)
      end do
      
      end do countloop

      end if roomh_flag

5348  format ('***Error: Bad ROOMA or ROOMH input. Data on the ROOMA (or H) line must be positive ',1pg12.3)
5349  format ('***Error: Bad ROOMH input. Compartment specified by ROOMH is not defined ',i0)
5350  format ('***Error: Bad ROOMH input. ROOMH error on data line ',i0)



      contains
      
      subroutine set_roomh_defaults
      
      compartment                     = 0 
      number                          = 0 
      height(1:mxpts)                 = 0.0_eb
      
      end subroutine set_roomh_defaults
      
      end subroutine read_roomh


      ! --------------------------- HHEAT -------------------------------------------
      subroutine read_hheat(LU)
      
      integer :: ios,nto,ifrom,ito,i,k
      integer :: LU
      real(eb) :: frac
      
      type(room_type), pointer :: roomptr

      integer :: first,nop
      real(eb), dimension(mxpts) :: npos_comp,npos_frac
      namelist/HHEAT/first,nop,npos_comp,npos_frac

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'HHEAT'
      nmlcount=0
      hheat_loop: do
         call checkread ('HHEAT', LU, ios)
         if (ios==0) hheatflag=.true.
         if (ios==1) then
             exit hheat_loop
         end if 
         read(LU,HHEAT,err=34,iostat=ios)
         nmlcount=nmlcount+1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with HHEAT number, line number', input_file_line_number
             stop
         end if
      end do hheat_loop

      hheat_flag: if (hheatflag) then

      rewind (LU) 
      input_file_line_number = 0
      
      countloop : do k=1,nmlcount

      call checkread('HHEAT',LU,ios)
      call set_hheat_defaults
      read(LU,HHEAT)

      nto = 0
      
      ifrom = first
      roomptr => roominfo(ifrom)
      nto = nop
      roomptr%iheat = 2
      
      do i = 1, nto
         ito = npos_comp(i) ! bug: if all = 2, will pass to same ito
         frac = npos_frac(i)
         if (ito<1.or.ito==ifrom.or.ito>nr) then
            write (*, 5356) ifrom,ito
            write (iofill, 5356) ifrom,ito
            stop
         end if
         if (frac<0.0_eb.or.frac>1.0_eb) then
            write (*, 5357) ifrom,ito,frac
            write (iofill, 5357) ifrom,ito,frac
            stop
         end if
         roomptr%heat_frac(ito) = frac
      end do
      
      end do countloop

      end if hheat_flag

5356  format ('***Error: Bad HHEAT input. HHEAT specification error in compartment pairs: ',2i3)
5357  format ('***Error: Bad HHEAT input. Error in fraction for HHEAT:',2i3,f6.3)



      contains
      
      subroutine set_hheat_defaults
      
      first                     = 0 
      nop                       = 0 
      npos_comp(1:mxpts)        = 0.0_eb
      npos_frac(1:mxpts)        = 0.0_eb
      
      end subroutine set_hheat_defaults

      end subroutine read_hheat


      ! --------------------------- DTCHE -------------------------------------------
      subroutine read_dtche(LU)
      
      integer :: ios
      integer :: LU

      integer :: count
      real(eb) :: time
      namelist/DTCHE/time,count

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'DTCHE'
      dtche_loop: do
         call checkread ('DTCHE',LU,ios)
         if (ios==0) dtcheflag=.true.
         if (ios==1) then
             exit dtche_loop
         end if 
         read(LU,DTCHE,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with DTCHE number, line number', input_file_line_number
             stop
         end if
      end do dtche_loop

      dtche_flag: if (dtcheflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('DTCHE',LU,ios)
      call set_dtche_defaults
      read(LU,DTCHE)

      stpmin = abs(time)
      stpmin_cnt_max = abs(count)
      ! a negative turns off the check
      if (count<=0) stpminflag = .false.
      
      end if dtche_flag



      contains
      
      subroutine set_dtche_defaults
      
      time                     = 0.0_eb
      count                    = 0
      
      end subroutine set_dtche_defaults

      end subroutine read_dtche


      ! --------------------------- FURNC -------------------------------------------
      subroutine read_furnc(LU)
      
      integer :: ios,i
      integer :: LU

      integer :: number
      real(eb), dimension(mxpts) :: time,temp
      namelist/FURNC/number,time,temp

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'FURNC'
      furn_loop: do
         call checkread ('FURNC',LU,ios)
         if (ios==0) furncflag=.true.
         if (ios==1) then
             exit furn_loop
         end if 
         read(LU,FURNC,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with FURNC number, line number', input_file_line_number
             stop
         end if
      end do furn_loop

      furnc_flag: if (furncflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('FURNC',LU,ios)
      call set_furn_defaults
      read(LU,FURNC)

      nfurn=number+0.5 !why adding 0.5
      do i = 1, nfurn
         furn_time(i)=time(i)
         furn_temp(i)=temp(i)
      end do

      end if furnc_flag



      contains

      subroutine set_furn_defaults

      number                  = 0 
      time                    = 0.0
      temp                    = 0.0

      end subroutine set_furn_defaults

      end subroutine read_furnc


      ! --------------------------- ADIAB --------------------------------------------
      subroutine read_adiab(LU)

      integer :: ios,LU
      character(64) :: adiab_walls
      namelist/ADIAB/adiab_walls

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'ADIAB'
      adiab_loop: do
         call checkread ('ADIAB',LU,ios)
         if (ios==0) adiabflag=.true.
         if (ios==1) then
             exit adiab_loop
         end if 
         read(LU,ADIAB,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with ADIAB number, line number', input_file_line_number
             stop
         end if
      end do adiab_loop

      adiab_flag: if (adiabflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('ADIAB',LU,ios)
      call set_adiab_defaults
      read(LU,ADIAB)

      adiabatic_walls=.false.

      if (adiab_walls=='TRUE') adiabatic_walls=.true.

      end if adiab_flag



      contains

      subroutine set_adiab_defaults

      adiab_walls                     = 'true'

      end subroutine set_adiab_defaults

      end subroutine read_adiab


      ! --------------------------- CSLCF --------------------------------------------
      subroutine read_cslcf(LU)
      
      integer :: ios,ii
      integer :: LU

      type(room_type), pointer :: roomptr
      type(visual_type), pointer :: sliceptr

      integer :: number
      integer, dimension(mxpts) :: comp
      real(eb) :: position
      character (64) :: domain,plane
      namelist /CSLCF/domain,plane,number,position,comp

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'CSLCF'
      nvisualinfo=0
      slcf_loop: do
         call checkread ('CSLCF',LU,ios)
         if (ios==0) cslcfflag=.true.
         if (ios==1) then
             exit slcf_loop
         end if 
         read(LU,CSLCF,err=34,iostat=ios)
         nvisualinfo=nvisualinfo+1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with CSLCF number, line number', input_file_line_number
             stop
         end if
      end do slcf_loop

      cslcf_flag: if (cslcfflag) then

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_slcf_loop: do ii=1,nvisualinfo

        call checkread('CSLCF',LU,ios)
        call set_slcf_defaults
        read(LU,CSLCF)

        sliceptr => visualinfo(ii)
        if (domain=='2-D') then
           sliceptr%vtype = 1
        else if (domain=='3-D') then
           sliceptr%vtype = 2
        else
           write (*, 5403) nvisualinfo
           write (iofill, 5403) nvisualinfo
           stop
        end if
       
        ! 2-D slice file
        if (sliceptr%vtype==1) then
           ! get position (required) and compartment (optional) first so we can check to make sure
           ! desired position is within the compartment(s)
           sliceptr%position = position
           sliceptr%roomnum  = comp(1)  !it is buggy here, only taking one value
           if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
              write (*, 5403) nvisualinfo
              write (iofill, 5403) nvisualinfo
              stop
           end if
           if (plane =='X') then
              sliceptr%axis = 1
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (iofill, 5403) nvisualinfo
                    stop
                 end if
              end if
           else if (plane =='Y') then
              sliceptr%axis = 2
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (iofill, 5403) nvisualinfo
                    stop
                 end if
              end if
           else if (plane =='Z') then
              sliceptr%axis = 3
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (iofill, 5403) nvisualinfo
                    stop
                 end if
              end if
           else
              write (*, 5403) nvisualinfo
              write (iofill, 5403) nvisualinfo
              stop
           end if

        ! 3-D slice
        else if (sliceptr%vtype==2) then
           if (number>1) then
              sliceptr%roomnum = comp(1) !it is buggy here, only taking 1 value
           else
              sliceptr%roomnum = 0
           end if
           if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
              write (*, 5403) nvisualinfo
              write (iofill, 5403) nvisualinfo
              stop
           end if
        end if

      end do read_slcf_loop

      end if cslcf_flag

5403  format ('***Error: Bad SLCF input. Invalid SLCF specification in visualization input ',i0)


      contains

      subroutine set_slcf_defaults

      domain                  = 'null'
      plane                   = 'null'
      position                = 0.0_eb
      comp                    = 0
      number                  = 0

      end subroutine set_slcf_defaults

      end subroutine read_cslcf


      ! --------------------------- CISOF --------------------------------------------
      subroutine read_cisof(LU)
      
      integer :: ios,ii
      integer :: LU

      type(visual_type), pointer :: sliceptr

      integer :: comp
      real(eb) :: value
      namelist /CISOF/value,comp

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'SISOF'
      nvisualinfo=0
      isof_loop: do
         call checkread ('CISOF',LU,ios)
         if (ios==0) cisofflag=.true.
         if (ios==1) then
             exit isof_loop
         end if 
         read(LU,CISOF,err=34,iostat=ios)
         nvisualinfo=nvisualinfo+1
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with CISOF number, line number', input_file_line_number
             stop
         end if
      end do isof_loop

      cisof_flag: if (cisofflag) then 

      rewind (LU) 
      input_file_line_number = 0

      ! Assign value to CFAST variables for further calculations
      read_isof_loop: do ii=1,nvisualinfo 

         call checkread('CISOF',LU,ios)
         call set_isof_defaults
         read(LU,CISOF)

         sliceptr => visualinfo(ii)
         sliceptr%vtype = 3
         sliceptr%value = value
         sliceptr%roomnum = comp

         if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
            write (*, 5404) nvisualinfo
            write (iofill, 5404) nvisualinfo
            stop
         end if

      end do read_isof_loop

      end if cisof_flag

5404  format ('***Error: Bad ISOF input. Invalid ISOF specification in visualization input ',i0)



      contains

      subroutine set_isof_defaults

      value                   = 0.0_eb
      comp                    = 0

      end subroutine set_isof_defaults

      end subroutine read_cisof
      
      
      ! --------------------------- DIAGN -------------------------------------------
      subroutine read_diagn(LU)
      
      integer :: ios
      integer :: LU
      
      type(room_type), pointer :: roomptr

      character(64) :: htmode
      real(eb) :: T1,T2,T3,T4,TU
      integer :: compa
      namelist/DIAGN/htmode,compa,T1,T2,T3,T4,TU

      ios = 1

      rewind(LU) 
      input_file_line_number = 0

      ! Scan entire file to look for 'DTCHE'
      diagn_loop: do
         call checkread ('DIAGN',LU,ios)
         if (ios==0) diagnflag=.true.
         if (ios==1) then
             exit diagn_loop
         end if 
         read(LU,DIAGN,err=34,iostat=ios)
34       if (ios>0) then
             write(iofill, '(A,i5)') 'Error: Problem with DIAGN number, line number', input_file_line_number
             stop
         end if
      end do diagn_loop

      diagn_flag: if (diagnflag) then

      rewind (LU) 
      input_file_line_number = 0

      call checkread('DIAGN',LU,ios)
      call set_diagn_defaults
      read(LU,DIAGN)
      
      ! 1,2,3,4 denotes ceiling, upper wall, lower wall and floor, respectively.
      if (htmode == 'RAD') diradflag = .true.
      
      roomptr => roominfo(compa)
      roomptr%temp(1) = TU
      roomptr%t_surfaces(1,1) = T1
      roomptr%t_surfaces(1,2) = T2
      roomptr%t_surfaces(1,3) = T3
      roomptr%t_surfaces(1,4) = T4
            
      end if diagn_flag



      contains
      
      subroutine set_diagn_defaults
      
      htmode                   = 'RAD'
      compa                    = 0
      T1                       = 273.15_eb
      T2                       = 273.15_eb
      T3                       = 273.15_eb
      T4                       = 273.15_eb
      TU                       = 273.15_eb
      
      end subroutine set_diagn_defaults

      end subroutine read_diagn
      

end module namelist_routines
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

      open (unit=99,file='input.out')
      open (unit=98,file='namelist.out')

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
      
      ! For trouble-shooting
!      call testing(ncomp)
  
      close (98)
      close (99)
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

      write(99, '(/, "Entering read_versn")')

      rewind(LU)
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'VERSN'
      write(99, '(/, "Entering versn_loop")')
      versn_loop: do
         call checkread ('VERSN', LU, ios)
         if (ios==0) versnflag=.true.

         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving versn_loop")')
             exit versn_loop
         end if 
         write(99, '(/, "Found VERSN, trying to read it")')
         read(LU,VERSN,err=34,iostat=ios)
34       if (ios>0) then
            write(99, '(a,i4)') 'Error: Problem with VERSN input, line number', input_file_line_number
            stop
         end if
      end do versn_loop

      if (.not.versnflag) then
         write (*, '(/, "Inputs for VERSN are required.")')
         write (99, '(/, "Inputs for VERSN are required.")')
         stop
      end if

      versn_flag: if (versnflag) then
        write(99, '(/, "on the way")')
  
        rewind (LU) 
        input_file_line_number = 0
        write(99, '(/, "  ios = ", i3, &
                    /, "  input_file_line_number = ", i3)') &
                    ios, input_file_line_number

        call checkread('VERSN',LU,ios)
        call set_versn_defaults
        write(99, '(/)')
        write(99,VERSN)
    
        read(LU,VERSN)
        write(99,VERSN)
  
        if (version>=1000) then
          iversion = version/1000
        else
          iversion = version/100
        end if

        title=pgrm_name

      end if versn_flag

      write(99, '(/, "  versnflag = ", L3, &
                  /, "  head = ", A, &
                  /, "  heading = ", A, &
                  /, "  ivers = ", i3, &
                  /, "  iversion = ", i3, &
                  /, "  pgrm_name = ", A, &
                  /, "  title = ", A)') &
                  versnflag,head,heading,ivers,iversion,pgrm_name,title
  
      if (head==heading.and.ivers==iversion-1) then
          write (*,5004) ivers, iversion
          write (99,5004) ivers, iversion
      else if (head/=heading.or.ivers/=iversion) then
          write (*,5002) head,heading,ivers,iversion
          write (99,5002) head,heading,ivers,iversion
          stop
      end if
      
      write(99, '(/, "Leaving read_versn (Done)")')

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

      write(99, '(/, "Enter checkread", &
                  /, "input_file_line_number = ", i3)') &
                  INPUT_FILE_LINE_NUMBER

      READLOOP: DO
         READ(LU,'(A)',END=10) TEXT
         INPUT_FILE_LINE_NUMBER = INPUT_FILE_LINE_NUMBER + 1
         TLOOP: DO II=1,72
            IF (TEXT(II:II)/='&' .AND. TEXT(II:II)/=' ') EXIT TLOOP
            IF (TEXT(II:II)=='&') THEN
               IF (TEXT(II+1:II+5)==NAME) THEN
                  BACKSPACE(LU)
                  IOS = 0
                  write(99, '(/, "Found object", &
                              /, "input_file_line_number = ", i3, &
                              /, "target = ", A, &
                              /, "text found = ", A)') &
                              input_file_line_number, NAME, TEXT(II+1:II+5)
                  EXIT READLOOP
               ELSE
                  write(99, '(/, "Not matching, cycling checkread", &
                              /, "input_file_line_number = ", i3, &
                              /, "target = ", A, &
                              /, "text found = ", A)') &
                              input_file_line_number, NAME, TEXT(II+1:II+5)
                  CYCLE READLOOP
               ENDIF
            ENDIF
         ENDDO TLOOP
      ENDDO READLOOP
  
10    write(99, '(/, "Leaving checkread")')
      RETURN
  
      END SUBROUTINE CHECKREAD


      ! --------------------------- STPMA -------------------------------------------
      subroutine read_stpma(LU)

      integer :: ios
      integer :: LU

      real(eb) :: stepmax
      namelist /STPMA/stepmax

      ios = 1

      write(99, '(/, "Entering read_stpmax")')

      rewind(LU) ; input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'STPMA'
      stpma_loop: do
         call checkread ('STPMA',LU,ios)
         if (ios==0) stpmaflag=.true.

         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving stpma_loop")')
             exit stpma_loop
         end if 
         write(99, '(/, "Found STPMA, trying to read it")')
         read(LU,STPMA,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i5)') 'Error: Problem with STPMA input, line number', input_file_line_number
             stop
         end if
      end do stpma_loop

      stpma_flag: if (stpmaflag) then
        write(99, '(/, "on the way")')

        rewind (LU) 
        input_file_line_number = 0
        write(99, '(/, "  ios = ", i3, &
                    /, "  input_file_line_number = ", i3)') &
                    ios, input_file_line_number
  
        call checkread('STPMA',LU,ios)
        call set_stpma_defaults
        write(99, '(/)')
        write(99,STPMA)
       
        read(LU,STPMA)
        write(99, '(/)')
        write(99,STPMA)
  
        stpmax=stepmax

        write(99, '(/, "  stepmax = ", f10.4, &
                    /, "  stpmax = ", f10.4)') &
                    stepmax,stpmax

      end if stpma_flag
      
      write(99, '(/, "Leaving read_stpmax (Done)")')

      contains

      subroutine set_stpma_defaults

      stepmax       = 1.0          ! s

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

      write(99, '(/, "Entering read_matrl")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'MATRL'
      n_thrmp = 0 
      matrl_loop: do
         call checkread ('MATRL',LU,ios)
         if (ios==0) matrlflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving matrl_loop")')
             exit matrl_loop
         end if 
         write(99, '(/, "Found MATRL, trying to read it")')
         read(LU,MATRL,err=34,iostat=ios)
         n_thrmp = n_thrmp + 1
         write(99, '(/, "  n_thrmp = ", i3)') n_thrmp
34       if (ios>0) then
            write(99, '(a,i3,a,i5)') 'Error: Problem with MATRL number ', n_thrmp+1, ', line number ', input_file_line_number
            stop
         end if
      end do matrl_loop

      if (.not.matrlflag) then
         write (*, '(/, "Inputs for MATRL are required.")')
         write (99, '(/, "Inputs for MATRL are required.")')
         stop
      end if

      matrl_flag: if (matrlflag) then
        write(99, '(/, "on the way")')
  
        if (n_thrmp>mxthrmp) then
           write (*,'(a,i3)') '***Error: Bad MATRL input. Too many thermal properties in input data file. limit is ', mxthrmp
           write (99,'(a,i3)') '***Error: Bad MATRL input. Too many thermal properties in input data file. limit is ', mxthrmp
           stop
        end if

        rewind (LU) 
        input_file_line_number = 0
  
        write(99, '(/, "Entering read_matrl_loop")')
        ! Assign value to CFAST variables for further calculations
        read_matrl_loop: do ii=1,n_thrmp

         thrmpptr => thermalinfo(ii)
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_thrmp = ", i3)') &
                     ii, n_thrmp

         call checkread('MATRL',LU,ios)
         call set_matrl_defaults
         write(99, '(/)')
         write(99,MATRL)
        
         read(LU,MATRL)
         write(99, '(/)')
         write(99,MATRL)

         thrmpptr%name          = matrl_id
         thrmpptr%nslab         = nslab
         thrmpptr%k(1)          = k
         thrmpptr%c(1)          = c
         thrmpptr%rho(1)        = rho
         thrmpptr%thickness(1)  = thickness
         thrmpptr%eps           = eps

         write(99, '(/, "  matrlflag = ", L3, &
                     /, "  thrmpptr%name = ", A, &
                     /, "  thrmpptr%nslab = ", i3, &
                     /, "  thrmpptr%k(1) = ", f10.4, &
                     /, "  thrmpptr%c(1) = ", f10.4, &
                     /, "  thrmpptr%rho(1) = ", f10.4, &
                     /, "  thrmpptr%thickness(1) = ", f10.4, &
                     /, "  thrmpptr%eps = ", f10.4)') &
                     matrlflag,thrmpptr%name,thrmpptr%nslab,thrmpptr%k(1),thrmpptr%c(1), &
                     thrmpptr%rho(1),thrmpptr%thickness(1),thrmpptr%eps

        end do read_matrl_loop

        write(99, '(/, "Left read_matrl_loop")')
      
      end if matrl_flag

      write(99, '(/, "leaving read_matrl (Done)")')


      contains

      subroutine set_matrl_defaults

      c                      = 0.0        !j/kg-k
      eps                    = 1.0
      k                      = 0.0        !w/m-k
      matrl_id               = 'null'     
      nslab                  = 1          
      rho                    = 0.0        !kg/m3
      thickness              = 0.0        !m

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

      write(99, '(/, "Entering read_compa")')

      rewind(LU)
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number
        
      ! Scan entire file to look for 'COMPA'          
      compa_loop: do
         call checkread('COMPA',LU,ios)
         if (ios==0) compaflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving compa_loop")')
             exit compa_loop
         end if 
         write(99, '(/, "Found COMPA, trying to read it")')
         read(LU,COMPA,err=34,iostat=ios)
         ncomp = ncomp + 1
         write(99, '(/, "  ncomp = ", i3)') ncomp
34       if (ios>0) then
            write(99, '(a,i3,a,i5)') 'Error: Problem with COMPA number ', ncomp+1, ', line number ', input_file_line_number
            stop
         end if
      end do compa_loop

      if (ncomp>mxrooms) then
         write (*,'(a,i3)') '***Error: Bad COMPA input. Too many compartments in input data file. limit is ', mxrooms
         write (99,'(a,i3)') '***Error: Bad COMPA input. Too many compartments in input data file. limit is ', mxrooms
         stop
      end if

      if (.not.compaflag) then
         write (*, '(/, "Inputs for COMPA are required.")')
         write (99, '(/, "Inputs for COMPA are required.")')
         stop
      end if

      compa_flag: if (compaflag) then
      write(99, '(/, "on the way")')

      rewind (LU)
      input_file_line_number = 0

      write(99, '(/, "Entering read_matrl_loop")')
      ! Assign value to CFAST variables for further calculations
      read_compa_loop: do ii=1,ncomp

         roomptr => roominfo(ii)
         write(99, '(/, "  n = ", i3, &
                     /, "  ncomp = ", i3)') &
                     ii, ncomp

         call checkread('COMPA',LU,ios)
         call set_compa_defaults
         write(99, '(/)')
         write(99,COMPA)

         read(LU,COMPA)
         write(99, '(/)')
         write(99,COMPA)

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
         
         write(99, '(/, "  compaflag = ", L3, &
                     /, "  nr = ", i3, &
                     /, "  roomptr%name = ", A12, &
                     /, "  roomptr%cwidth = ", f10.4, &
                     /, "  roomptr%cdepth = ", f10.4, &
                     /, "  roomptr%cheight = ", f10.4, &
                     /, "  roomptr%x0 = ", f10.4, &
                     /, "  roomptr%y0 = ", f10.4, &
                     /, "  roomptr%z0 = ", f10.4, &
                     /, "  roomptr%ibar = ", i3, &
                     /, "  roomptr%jbar = ", i3, &
                     /, "  roomptr%kbar = ", i3, &
                     /, "  roomptr%surface_on(1-4) = ", 4L4, &
                     /, "  roomptr%matl(1-4) = ", 4A12)') &
                     compaflag,nr,roomptr%name,roomptr%cwidth,roomptr%cdepth,roomptr%cheight, &
                     roomptr%x0,roomptr%y0,roomptr%z0,roomptr%ibar,roomptr%jbar, &
                     roomptr%kbar,roomptr%surface_on(1:4),roomptr%matl(1:4)

      end do read_compa_loop
      
      write(99, '(/, "Left read_compa_loop")')

      end if compa_flag
      
      write(99, '(/, "leaving read_compa (Done)")')


      contains

      subroutine set_compa_defaults

      ceiling                 = 'off'
      compa_id                = 'null'
      depth                   = 0.0
      floor                   = 'off'
      height                  = 0.0
      wall                    = 'off'
      width                   = 0.0
      x0                      = 0.0
      xgrid                   = 50
      y0                      = 0.0
      ygrid                   = 50
      z0                      = 0.0
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

      write(99, '(/, "Entering read_targe")')

      rewind(LU)
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number
        
      ! Scan entire file to look for 'TARGE'  
      n_targets = 0
      targe_loop: do
         call checkread ('TARGE',LU,ios)
         if (ios==0) targeflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving targe_loop")')
             exit targe_loop
         end if 
         write(99, '(/, "Found TARGE, trying to read it")')
         read(LU,TARGE,err=34,iostat=ios)
         n_targets =n_targets + 1
34          if (ios>0) then
               write(99, '(a,i3,a,i5)') 'Error: Problem with TARGE number ', n_targets+1, ', line number ', input_file_line_number
               stop
            end if
      end do targe_loop

      if (n_targets>mxtarg) then
         write (*,'(a,i3)') '***Error: Bad TARGE input. Too many targets in input data file. limit is ', mxtarg
         write (99,'(a,i3)') '***Error: Bad TARGE input. Too many targets in input data file. limit is ', mxtarg
         stop
      end if

      targe_flag: if (targeflag) then
      write(99, '(/, "on the way")')

      rewind (LU)
      input_file_line_number = 0

      write(99, '(/, "Entering read_matrl_loop")')
      ! Assign value to CFAST variables for further calculations
      read_targe_loop: do ii=1,n_targets

         targptr => targetinfo(ii)
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_targets = ", i3)') &
                     ii, n_targets

         call checkread('TARGE',LU,ios)
         call set_targe_defaults 
         write(99, '(/)')
         write(99,TARGE)

         read(LU,TARGE)
         write(99, '(/)')
         write(99,TARGE) 

         iroom = compartment

         if (iroom<1.or.iroom>nr) then
            write (*,5003) iroom 
            write (99,5003) iroom
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
               write (99,913) 'Warning', eqtype
            else if (eqtype(1:3)=='PDE') then
               targptr%equaton_type = pde
            else if (eqtype(1:3)=='CYL') then
               targptr%equaton_type = cylpde
            else
               write (*,913) 'Error',eqtype
               write (99,913) 'Error',eqtype
               stop
            end if
         end if
         
         write(99, '(/, "  targeflag = ", L3, &
                     /, "  targptr%room = ", i3, &
                     /, "  targptr%center(1) = ", f10.4, &
                     /, "  targptr%center(2) = ", f10.4, &
                     /, "  targptr%center(3) = ", f10.4, &
                     /, "  targptr%normal(1) = ", f10.4, &
                     /, "  targptr%normal(2) = ", f10.4, &
                     /, "  targptr%normal(3) = ", f10.4, &
                     /, "  targptr%depth_loc = ", f10.4, &
                     /, "  targptr%name = ", A, &
                     /, "  targptr%material = ", A, &
                     /, "  targptr%equaton_type = ", i3, &
                     /, "  targptr%wall = ", i3)') &
                     targeflag,targptr%room,targptr%center(1),targptr%center(2), &
                     targptr%center(3), targptr%normal(1),targptr%normal(2), &
                     targptr%normal(3),targptr%depth_loc,targptr%name, &
                     targptr%material,targptr%equaton_type,targptr%wall

      end do read_targe_loop
      
      write(99, '(/, "Left read_targe_loop")')

      end if targe_flag
      
      write(99, '(/, "leaving read_targe (Done)")')

913   format('***',A,': BAD TARGE input. Invalid equation type:',A3,' Valid choices are: PDE or CYL')
5003  format ('***Error: BAD TARGE input. The compartment specified by TARGET does not exist ',i0)



      contains

      subroutine set_targe_defaults

      compartment                     = 0
      x                               = 0.0
      y                               = 0.0
      z                               = 0.0
      normalx                         = 0.0
      normaly                         = 0.0
      normalz                         = 0.0
      material                        = ' '
      method                          = 'EXPLICIT'
      equationtype                    = ' '
      internallocation                = 0.5
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

      write(99, '(/, "Entering read_cfire")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CFIRE'
      n_fires = 0
      fire_loop: do
         call checkread ('CFIRE', LU, ios)
         if (ios==0) cfireflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving fire_loop")')
             exit fire_loop
         end if 
         write(99, '(/, "Found CFIRE, trying to read it")')
         read(LU,CFIRE,err=34,iostat=ios)
         n_fires =n_fires + 1
34       if (ios>0) then
            write(99, '(a,i3,a,i5)') 'Error: Problem with CFIRE number ', n_fires+1, ', line number ', input_file_line_number
            stop
         end if
      end do fire_loop

      if (n_fires>mxfires) then
         write (*,'(a,i3)') '***Error: Bad CFIRE input. Too many CFIRE in input data file. limit is ', mxfires
         write (99,'(a,i3)') '***Error: Bad CFIRE input. Too many CFIRE in input data file. limit is ', mxfires
         stop
      end if

      cfire_flag: if (cfireflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_fire_loop")')
      ! Assign value to CFAST variables for further calculations
      read_fire_loop: do ii=1,n_fires

         fireptr => fireinfo(ii)
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_fires = ", i3)') &
                     ii, n_fires

         call checkread('CFIRE',LU,ios)
         call set_fire_defaults
         write(99, '(/)')
         write(99,CFIRE)
        
         read(LU,CFIRE)
         write(99, '(/)')
         write(99,CFIRE)

         iroom = compartment
         if (iroom<1.or.iroom>nr-1) then
            write (*,5320) iroom
            write (99,5320) iroom
            stop
         end if
         roomptr => roominfo(iroom)

         ! Only constrained fires
         fireptr%chemistry_type = 2
         if (fireptr%chemistry_type>2) then
            write (*,5321) fireptr%chemistry_type
            write (99,5321) fireptr%chemistry_type
            stop
         end if

         fireptr%x_position = x
         fireptr%y_position = y
         fireptr%z_position = z
         if (fireptr%x_position>roomptr%cwidth.or.fireptr%y_position>roomptr%cdepth.or.fireptr%z_position>roomptr%cheight) then
            write (*,5323) n_fires
            write (99,5323) n_fires
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
                  write (99,5324) n_fires
                  stop
               end if
            end if
         else
            write (*,5322)
            write (99,5322)
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
               write (99,5358) fireptr%ignition_type
               stop
            end if
         else
            fireptr%ignited  = .true.
            fireptr%reported = .true.
         end if
         
         write(99, '(/, "  cfireflag = ", L3, &
                     /, "  fireptr%name = ", A12, &
                     /, "  fireptr%room = ", i3, &
                     /, "  fireptr%chemistry_type = ", i3, &
                     /, "  fireptr%x_position = ", f10.4, &
                     /, "  fireptr%y_position = ", f10.4, &
                     /, "  fireptr%z_position = ", f10.4, &
                     /, "  fireptr%modified_plume = ", i3, &
                     /, "  fireptr%ignition_type = ", i3, &
                     /, "  fireptr%ignition_target = ", i3, &
                     /, "  fireptr%ignited = ", L3, &
                     /, "  fireptr%reported = ", L3, &
                     /, "  fireptr%ignition_criterion = ", e10.4)') &
                     cfireflag,fireptr%name,fireptr%room,fireptr%chemistry_type,fireptr%x_position, &
                     fireptr%y_position,fireptr%z_position,fireptr%modified_plume,fireptr%ignition_type, &
                     fireptr%ignition_target,fireptr%ignited,fireptr%reported, &
                     fireptr%ignition_criterion

      end do read_fire_loop
      
      write(99, '(/, "Left read_fire_loop")')

      end if cfire_flag
      
      write(99, '(/, "leaving read_cfire (Done)")')
      
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
      plume                           = 0
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
      character(len=30) :: myfmt

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

      write(99, '(/, "Entering read_chemi")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CHEMI'
      n_chemi = 0
      chemi_loop: do
         call checkread ('CHEMI',LU,ios)
         if (ios==0) chemiflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving chemi_loop")')
             exit chemi_loop
         end if 
         write(99, '(/, "Found CHEMI, trying to read it")')
         n_chemi =n_chemi + 1
         read(LU,CHEMI,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i3,a,i5)') 'Error: Problem with CHEMI number ', n_chemi+1, ', line number ', input_file_line_number
             stop
         end if
      end do chemi_loop

      if (cfireflag) then
         if (.not.chemiflag) then
            write (*, '(/, "CFIRE detected. Missing CHEMI")')
            write (99, '(/, "CFIRE detected. Missing CHEMI")')
            stop
         end if
      end if

      chemi_flag: if (chemiflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_chemi_loop")')
      ! Assign value to CFAST variables for further calculations
      read_chemi_loop: do ii=1,n_chemi

         write(99, '(/, "  ii = ", i3, &
                     /, "  n_chemi = ", i3)') &
                     ii, n_chemi

         call checkread('CHEMI',LU,ios)
         call set_chemi_defaults
         write(99, '(/)')
         write(99,CHEMI)
        
         read(LU,CHEMI)
         write(99, '(/)')
         write(99,CHEMI)

         match_loop: do j=1,n_fires

            fireptr => fireinfo(j)

            if (chemi_id==fireptr%name) then 
                write (99, '(/, "Chemi_id and Fire_id match and procced")')
                exit match_loop
            else
                write (99, '(/, "Still matching")')
            end if
         end do match_loop   

         if (j==n_fires+1) then
             write (*, '(/, "Chemi_id and Fire_id do not match")')
             write (99, '(/, "Chemi_id and Fire_id do not match")')
             stop
         end if

         ! Define chemical formula
         fireptr%n_c  = c
         fireptr%n_h  = h
         fireptr%n_o  = o
         fireptr%n_n  = n
         fireptr%n_cl = cl
         fireptr%molar_mass = (12.01*fireptr%n_c + 1.008*fireptr%n_h + 16.0*fireptr%n_o + &
                         14.01*fireptr%n_n + 35.45*fireptr%n_cl)/1000.0
         fireptr%chirad = chir
         ohcomb = hoc
         if (ohcomb<=0.0_eb) then
            write (*,5001) ohcomb
            write (99,5001) ohcomb
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
               write (99,5002)
               stop
            end if
            fireptr%area(i) = max(area(i),pio4*0.2_eb**2)
            max_area = max(max_area,fireptr%area(i))

            ! calculate a characteristic length of an object (we assume the diameter).
            ! this is used for point source radiation fire to target calculation as a minimum effective
            ! distance between the fire and the target which only impact very small fire to target distances
            fireptr%characteristic_length = sqrt(max_area/pio4)

            ! Diagnostic - check for the maximum heat release per unit volume.
            ! First, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
            call flame_height(max_hrr, max_area, flamelength)
            flamelength = max (0.0_eb, flamelength)
            
            ! Now the heat realease per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
            hrrpm3 = max_hrr/(pio4*fireptr%characteristic_length**2*(fireptr%characteristic_length+flamelength))
            if (hrrpm3>4.0e6_eb) then
                write (*,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (*, 5108)
                write (99,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (99, 5108)
                stop
            else if (hrrpm3>2.0e6_eb) then
                write (*,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (*, 5108)
                write (99,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (99, 5108)
            end if
         end do
        
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
         
         write(myfmt, '("("i3,"(f15.4))")')  nret

         write(99, '(/, "  cfireflag = ", L3, &
                     /, "  chemiflag = ", L3, &
                     /, "  fireptr%n_c = ", f10.4, &
                     /, "  fireptr%n_h = ", f10.4, &
                     /, "  fireptr%n_o = ", f10.4, &
                     /, "  fireptr%n_n = ", f10.4, &
                     /, "  fireptr%n_cl = ", f10.4, &
                     /, "  fireptr%molar_mass = ", f10.4, &
                     /, "  fireptr%chirad = ", f10.4, &
                     /, "  fireptr%npoints = ", i3)') &
                     cfireflag,chemiflag,fireptr%n_c,fireptr%n_h,fireptr%n_o,fireptr%n_n, &
                     fireptr%n_cl,fireptr%molar_mass,fireptr%chirad,fireptr%npoints

         write(99, '(/, "  fireptr%time = ")')
         write(99, fmt=myfmt) fireptr%time(1:nret)
         write(99, '(/, "  fireptr%qdot = ")')
         write(99, fmt=myfmt) fireptr%qdot(1:nret)
         write(99, '(/, "  fireptr%mdot = ")')
         write(99, fmt=myfmt) fireptr%mdot(1:nret)
         write(99, '(/, "  fireptr%y_soot = ")')
         write(99, fmt=myfmt) fireptr%y_soot(1:nret)
         write(99, '(/, "  fireptr%y_co = ")')
         write(99, fmt=myfmt) fireptr%y_co(1:nret)
         write(99, '(/, "  fireptr%y_trace = ")')
         write(99, fmt=myfmt) fireptr%y_trace(1:nret)
         write(99, '(/, "  fireptr%area = ")')
         write(99, fmt=myfmt) fireptr%area(1:nret)
         write(99, '(/, "  fireptr%height = ")')
         write(99, fmt=myfmt) fireptr%height(1:nret)

      end do read_chemi_loop

      write(99, '(/, "Left read_chemi_loop")')

      end if chemi_flag
      
      write(99, '(/, "leaving read_chemi (Done)")')

5001  format ('***Error: invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002  format ('***Error: invalid fire area. all input values must be greater than zero')
5106  format ('***Error: object ',a,' position set to ',3f7.3,'; maximum hrr per m^3 = ',1pg10.3,' exceeds physical limits')
5107  format ('Object ',a,' position set to ',3f7.3,'; maximum c_hrr per m^3 = ',1pg10.3,' exceeds nominal limits')
5108  format ('Typically, this is caused by too small fire area inputs. check c_hrr and fire area inputs')
5000  format ('***Error: the key word ',a5,' is not part of a fire definition. fire keyword are likely out of order')

      contains

      subroutine set_chemi_defaults

      chemi_id          = 'null'
      c                 = 0.0
      h                 = 0.0
      o                 = 0.0
      n                 = 0.0
      cl                = 0.0
      chir              = 0.35_eb
      hoc               = 0.0
      nret              = 0
      time(1:mxpts)     = 0.0
      c_hrr(1:mxpts)    = 0.0
      fv(1:mxpts)       = 0.0
      c_co(1:mxpts)     = 0.0
      trace(1:mxpts)    = 0.0
      area(1:mxpts)     = 0.0
      height(1:mxpts)   = 0.0

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
              write (99,*) 'Fire objects positioned specified outside compartment bounds.'
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

      write(99, '(/, "Entering read_ctime")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CTIME'
      time_loop: do
         call checkread ('CTIME',LU,ios)
         if (ios==0) ctimeflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving time_loop")')
             exit time_loop
         end if 
         write(99, '(/, "Found CTIME, trying to read it")')
         read(LU,CTIME,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i5)') 'Error: Problem with CTIME input, line number', input_file_line_number
             stop
         end if
      end do time_loop

      if (.not.ctimeflag) then
         write (*, '(/, "Inputs for CTIME are required.")')
         write (99, '(/, "Inputs for CTIME are required.")')
         stop
      end if

      ctime_flag: if (ctimeflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('CTIME',LU,ios)
      call set_time_defaults
      write(99, '(/)')
      write(99,CTIME)
        
      read(LU,CTIME)
      write(99, '(/)')
      write(99,CTIME)

      time_end=simulation
      print_out_interval=print
      smv_out_interval=smokeview
      ss_out_interval=spreadsheet

      write(99, '(/, "  time_end = ", i10, &
                  /, "  print_out_interval = ", i3, &
                  /, "  smv_out_interval = ", i3, &
                  /, "  ss_out_interval = ", i3)') &
                  time_end,print_out_interval,smv_out_interval,ss_out_interval

      end if ctime_flag

      write(99, '(/, "leaving read_ctime (Done)")')



      contains
      
      subroutine set_time_defaults
      
      simulation             = 0          ! s
      print                  = 50         ! s
      smokeview              = 10         ! s
      spreadsheet            = 10         ! s
      
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

      write(99, '(/, "Entering read_tambi")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'TAMBI'
      tamb_loop: do
         call checkread ('TAMBI',LU,ios)
         if (ios==0) tambiflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving tamb_loop")')
             exit tamb_loop
         end if 
         write(99, '(/, "Found TAMBI, trying to read it")')
         read(LU,TAMBI,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i5)') 'Error: Problem with TAMBI number, line number', input_file_line_number
             stop
          end if
      end do tamb_loop

      tambi_flag: if (tambiflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('TAMBI',LU,ios)
      call set_tamb_defaults
      write(99, '(/)')
      write(99,TAMBI)
        
      read(LU,TAMBI)
      write(98, '(/)')
      write(98,TAMBI)
      write(99, '(/)')
      write(99,TAMBI)
      
      interior_temperature  = temp
      interior_abs_pressure = pres
      relative_humidity     = rh*0.01_eb
      if (.not.exsets) then
          exterior_temperature = interior_temperature
          exterior_abs_pressure = interior_abs_pressure
          exterior_rho = interior_rho
      end if

      tgignt = interior_temperature + 200.0_eb

      write(99, '(/, "  interior_temperature = ", f10.4, &
                  /, "  interior_abs_pressure = ", f15.4, &
                  /, "  tgignt = ", f10.4, &
                  /, "  relative_humidity = ", f10.4)') &
                  interior_temperature,interior_abs_pressure,tgignt,relative_humidity

      end if tambi_flag


      write(99, '(/, "leaving read_tambi (Done)")')
      
      contains
      
      subroutine set_tamb_defaults
      
      temp                     = 293.17        !K
      pres                     = 101325.0      !Pa
      rh                       = 50            !Percentage
      
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

      write(99, '(/, "Entering read_eambi")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'EAMBI'
      eamb_loop: do
         call checkread ('EAMBI',LU,ios)
         if (ios==0) eambiflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving eamb_loop")')
             exit eamb_loop
         end if 
         write(99, '(/, "Found EAMBI, trying to read it")')
         read(LU,EAMBI,err=34,iostat=ios)
         34 if (ios>0) then
         write(99, '(a,i5)') 'Error: Problem with EAMBI number, line number', input_file_line_number
         stop
      end if
      end do eamb_loop

      eambi_flag: if (eambiflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('EAMBI',LU,ios)
      call set_eamb_defaults
      write(99, '(/)')
      write(99,EAMBI)
        
      read(LU,EAMBI)
      write(98, '(/)')
      write(98,EAMBI)
      write(99, '(/)')
      write(99,EAMBI)
      
      exterior_temperature  = temp
      exterior_abs_pressure = pres
      exsets = .true.

      write(99, '(/, "  exterior_temperature = ", f10.4, &
                  /, "  exterior_abs_pressure = ", f15.4)') &
                  exterior_temperature,exterior_abs_pressure

      end if eambi_flag

      write(99, '(/, "leaving read_eambi (Done)")')
      
           
      contains
      
      subroutine set_eamb_defaults
      
      temp                        = 293.17        !K
      pres                        = 101325.0      !Pa
      
      end subroutine set_eamb_defaults
      
      end subroutine read_eambi


      ! --------------------------- LIMO2 -------------------------------------------
      subroutine read_limo2(LU)
      
      integer :: ios,LU
      real(eb) :: o2index
      namelist /LIMO2/o2index

      ios = 1

      write(99, '(/, "Entering read_limo2")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'LIMO2'
      limo2_loop: do
         call checkread ('LIMO2',LU,ios)
         if (ios==0) limo2flag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving limo2_loop")')
             exit limo2_loop
         end if 
         write(99, '(/, "Found LIMO2, trying to read it")')
         read(LU,LIMO2,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i5)') 'Error: Problem with LIMO2 number, line number', input_file_line_number
             stop
         end if
      end do limo2_loop

      limo2_flag: if (limo2flag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('LIMO2',LU,ios)
      call set_limo2_defaults
      write(99, '(/)')
      write(99,LIMO2)
        
      read(LU,LIMO2)
      write(99, '(/)')
      write(99,LIMO2)
      
      lower_o2_limit  = o2index

      write(99, '(/, "  limo2flag = ", L3, &
                  /, "  lower_o2_limit = ", f10.4)') &
                  limo2flag,lower_o2_limit

      end if limo2_flag
      
      write(99, '(/, "leaving read_limo2 (Done)")')
      
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

      write(99, '(/, "Entering HVENT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'HVENT'
      n_hvents = 0
      hvent_loop: do
         call checkread ('HVENT',LU,ios)
         if (ios==0) hventflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving hvent_loop")')
             exit hvent_loop
         end if 
         write(99, '(/, "Found HVENT, trying to read it")')
         read(LU,HVENT,err=34,iostat=ios)
         n_hvents =n_hvents + 1
         34 if (ios>0) then
               write(99, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_hvents+1, ', line number ', input_file_line_number
               stop
            end if
      end do hvent_loop

      hvent_flag: if (hventflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      if (n_hvents>mxhvents) then
         write (*,'(a,i3)') '***Error: Bad hvent input. Too many hvent in input data file. limit is ', mxhvents
         write (99,'(a,i3)') '***Error: Bad hvent input. Too many hvent in input data file. limit is ', mxhvents
         stop
      end if

      write(99, '(/, "Entering read_hvent_loop")')
      ! Assign value to CFAST variables for further calculations
      read_hvent_loop: do ii=1,n_hvents
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_fires = ", i3)') &
                     ii, n_fires

         call checkread('HVENT',LU,ios)
         call set_hvent_defaults
         write(99, '(/)')
         write(99,HVENT)
        
         read(LU,HVENT)
         write(99, '(/)')
         write(99,HVENT)
    
         i = first
         j = second
         k = hvent_id
         imin = min(i,j)
         jmax = max(i,j)
      
         if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
            write (*,5070) i, j
            write (99,5070) i, j
            stop
         end if
         if (k>mxccv) then
            write (*,5080) i, j, k
            write (99,5080) i, j, k
            stop
         end if
      
         ventptr => hventinfo(ii)
         ventptr%room1 = imin
         ventptr%room2 = jmax
         ventptr%counter = hvent_id
      
         if (n_hvents>mxhvents) then
            write (*,5081) i,j,k
            write (99,5081) i,j,k
            stop
         end if
      
         ventptr%width  = width
         ventptr%soffit = soffit
         ventptr%sill   = sill
      
         if  (type=='TIME' .or. type=='TEMP' .or. type=='FLUX') then
             ventptr%offset(1) = offset1
             ventptr%offset(2) = 0.0_eb
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
                   write (99,*) '***Error: Bad hvent input. Vent opening specification requires an associated target.'
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
            write (99,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if

         roomptr => roominfo(ventptr%room1)
         ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
         ventptr%absolute_sill = ventptr%sill + roomptr%z0
         
         write(99, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%width = ", f10.4, &
                     /, "  ventptr%soffit = ", f10.4, &
                     /, "  ventptr%sill = ", f10.4, &
                     /, "  ventptr%offset(1) = ", f10.4, &
                     /, "  ventptr%offset(2) = ", f10.4, &
                     /, "  ventptr%face = ", i3, &
                     /, "  ventptr%opening_type = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_initial_fraction = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4, &
                     /, "  ventptr%opening_criterion = ", f10.4, &
                     /, "  ventptr%opening_target = ", i3, &
                     /, "  ventptr%absolute_soffit = ", f10.4, &
                     /, "  ventptr%absolute_sill = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%width, &
                     ventptr%soffit,ventptr%sill,ventptr%offset(1),ventptr%offset(2), &
                     ventptr%face,ventptr%opening_type,ventptr%opening_initial_time, &
                     ventptr%opening_initial_fraction,ventptr%opening_final_time, &
                     ventptr%opening_final_fraction, ventptr%opening_criterion, &
                     ventptr%opening_target,ventptr%absolute_soffit,ventptr%absolute_sill

      end do read_hvent_loop
      
      write(99, '(/, "Left read_hvent_loop")')

      end if hvent_flag
      
      write(99, '(/, "leaving read_hvent (Done)")')

5070  format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)
5080  format ('***Error: Bad HVENT input. Too many pairwise horizontal connections',3I5)
5081  format ('***Error: Too many horizontal connections ',3i5)
      
      contains
      
      subroutine set_hvent_defaults
      
      first                   = 0
      second                  = 0
      hvent_id                = 0
      width                   = 0.0
      soffit                  = 0.0
      sill                    = 0.0
      offset1                 = 0.0
      offset2                 = 0.0
      face                    = 0
      type                    = 'null'
      criterion               = 0.0
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0
      finaltime               = 1.0_eb
      finalfraction           = 0.0
      
      end subroutine set_hvent_defaults
      
      end subroutine read_hvent


      ! --------------------------- DEADR -------------------------------------------
      subroutine read_deadr(LU)
      
      integer :: ios,i,j
      integer :: LU

      type(room_type), pointer :: roomptr
      
      integer :: compartment,connected
      namelist /DEADR/compartment,connected

      ios = 1

      write(99, '(/, "Entering DEADR")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'DEADR'
      deadr_loop: do
         call checkread ('DEADR',LU,ios)
         if (ios==0) deadrflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving deadr_loop")')
             exit deadr_loop
         end if 
         write(99, '(/, "Found DEADR, trying to read it")')
         read(LU,DEADR,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(a,i5)') 'Error: Problem with deadr number, line number', input_file_line_number
             stop
         end if
      end do deadr_loop

      deadr_flag: if (deadrflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('DEADR',LU,ios)
      call set_deadr_defaults
      write(99, '(/)')
      write(99,DEADR)
        
      read(LU,DEADR)
      write(99, '(/)')
      write(99,DEADR)
      
      i = compartment
      j = connected
      
      if (i.ge.1.and.i.le.mxrooms.and.j.le.1.and.j.le.mxrooms.and.i.ne.j) then
         roomptr => roominfo(i)
         roomptr%deadroom = j
      end if

      write(99, '(/, "  i = ", i4, &
                  /, "  roomptr%deadroom = ", i4)') &
                  i,roomptr%deadroom

      end if deadr_flag                  

      write(99, '(/, "leaving read_deadr (Done)")')


      contains
      
      subroutine set_deadr_defaults
      
      compartment                  = 0
      connected                    = 0
      
      end subroutine set_deadr_defaults
      
      end subroutine read_deadr


      ! --------------------------- EVENT -------------------------------------------
      subroutine read_event(LU)

      integer :: ios,i,j,k,iijk,fannumber
      integer :: LU
      character(48) :: venttype

      type(vent_type), pointer :: ventptr

      integer :: first,second,event_id
      real(eb) :: fraction,decay,time
      character(48) :: type
      namelist /EVENT/first,second,event_id,type,time,fraction,decay

      ios = 1

      write(99, '(/, "Entering EVENT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'EVENT'
      event_loop: do
         call checkread ('EVENT',LU,ios)
         if (ios==0) eventflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving event_loop")')
             exit event_loop
         end if 
         write(99, '(/, "Found EVENT, trying to read it")')
         read(LU,EVENT,err=34,iostat=ios)
34       if (ios>0) then
            write(99, '(a,i5)') 'Error: Problem with EVENT number, line number', input_file_line_number
            stop
         end if
      end do event_loop

      event_flag: if (eventflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('EVENT',LU,ios)
      call set_event_defaults
      write(99, '(/)')
      write(99,EVENT)
        
      read(LU,EVENT)
      write(99, '(/)')
      write(99,EVENT)
      
      venttype = type
      
      if (fraction<0.0_eb.or.fraction>1.0_eb) then
         write (99,*) '****Error: Bad event input. Final_fraction must be between 0 and 1 inclusive.'
         stop
      end if

      write(99, '(/, "  n_hvents = ", i3, &
                  /, "  n_vvents = ", i3, &
                  /, "  n_mvents = ", i3)') &
                  n_hvents,n_vvents,n_mvents
      
      select case (venttype)
      case ('H')
         i = first
         j = second
         k = event_id
         do iijk = 1, n_hvents
            ventptr => hventinfo(iijk)
         write(99, '(/, "BEFORE")')
         write(99, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%opening_initial_time, &
                     ventptr%opening_final_time,ventptr%opening_final_fraction

            if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
               ventptr%opening_initial_time = time
               ventptr%opening_final_time = time + decay
               ventptr%opening_final_fraction = fraction
            end if

         write(99, '(/, "AFTER")')
         write(99, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%opening_initial_time, &
                     ventptr%opening_final_time,ventptr%opening_final_fraction
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
            write (99,5196) fannumber
            stop
         end if
         ventptr => mventinfo(fannumber)
         ventptr%opening_initial_time = time
         ventptr%opening_final_time = time + decay
         ventptr%opening_final_fraction = fraction
      case default
         write (*,*) '***Error: Bad event input. Type must be H, V, M, or F.'
         write (99,*) '***Error: Bad event input. Type must be H, V, M, or F.'
         stop
      end select

      end if event_flag

      write(99, '(/, "leaving read_event (Done)")')
5196  format ('***Error: Bad EVENT input. Fan has not been defined for this filter ',i0)
      
      contains
      
      subroutine set_event_defaults
      
      first                   = 0
      second                  = 0
      event_id                = 0
      type                    = 'null'
      time                    = 0.0
      fraction                = 0.0
      decay                   = 0.0
      
      end subroutine set_event_defaults
      
      end subroutine read_event


      ! --------------------------- CRAMP -------------------------------------------
      subroutine read_cramp(LU)
           
      integer :: ios,ii,iramp
      integer :: LU
      character(64) :: myfmt

      type(ramp_type), pointer :: rampptr
      
      integer :: first,second,ramp_id,ramp_n
      real(eb), dimension(mxpts) :: time,fraction
      character(64) :: type
      namelist /CRAMP/type,first,second,ramp_id,ramp_n,time,fraction

      ios = 1

      write(99, '(/, "Entering CRAMP")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CRAMP'
      nramps = 0
      ramp_loop: do
         call checkread ('CRAMP',LU,ios)
         if (ios==0) crampflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving ramp_loop")')
             exit ramp_loop
         end if 
         write(99, '(/, "Found CRAMP, trying to read it")')
         read(LU,CRAMP,err=34,iostat=ios)
         nramps =nramps + 1
34       if (ios>0) then
             write(99, '(a,i3,a,i5)') 'Error: Problem with CRAMP number ', nramps+1, ', line number ', input_file_line_number
             stop
         end if
      end do ramp_loop

      if (nramps>mxramps) then
         write (*,'(a,i3)') '***Error: Bad ramp input. Too many ramp in input data file. limit is ', mxramps
         write (99,'(a,i3)') '***Error: Bad ramp input. Too many ramp in input data file. limit is ', mxramps
         stop
      end if

      cramp_flag: if (crampflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_ramp_loop")')
      ! Assign value to CFAST variables for further calculations      
      read_ramp_loop: do ii=1,nramps
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  nramps = ", i3)') &
                     ii, nramps
      
         call checkread('CRAMP',LU,ios)
         call set_ramp_defaults
         write(99, '(/)')
         write(99,CRAMP)
        
         read(LU,CRAMP)
         write(99, '(/)')
         write(99,CRAMP)
      
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

         write(myfmt, '("("i3,"(f10.4))")')  rampptr%npoints

         write(99, '(/, "  rampptr%type = ", A12, &
                     /, "  rampptr%room1 = ", i3, &
                     /, "  rampptr%room2 = ", i3, &
                     /, "  rampptr%counter = ", i3, &
                     /, "  rampptr%npoints = ", i3)') &
                     rampptr%type,rampptr%room1,rampptr%room2,rampptr%counter, &
                     rampptr%npoints

         write(99, '(/, "  rampptr%time = ")')
         write(99, fmt=myfmt) rampptr%time(1:rampptr%npoints)
         write(99, '(/, "  rampptr%value = ")')
         write(99, fmt=myfmt) rampptr%value(1:rampptr%npoints)
      
      end do read_ramp_loop

      write(99, '(/, "Left read_ramp_loop")')

      end if cramp_flag
      
      write(99, '(/, "leaving read_ramp (Done)")')
      
      
      contains
      
      subroutine set_ramp_defaults
      
      type                            = 'null'
      first                           = 0 
      second                          = 0 
      ramp_id                         = 0
      ramp_n                          = 0 
      time                            = 0.0
      fraction                        = 0.0
      
      end subroutine set_ramp_defaults
      
      end subroutine read_cramp


      ! --------------------------- VVENT -------------------------------------------
      subroutine read_vvent(LU)
      
      integer :: ios,ii,i,j,k
      integer :: LU
      
!      type(room_type), pointer :: roomptr
      type(target_type), pointer :: targptr
      type(vent_type), pointer :: ventptr
      
      integer   :: top,bottom,vvent_id,shape,initialtime,finaltime
      real(eb)      :: area,offsetx,offsety,criterion,initialfraction,finalfraction
      character(64) :: type,target
      namelist /VVENT/top,bottom,vvent_id,area,shape,type,criterion,target,initialtime, &
                      initialfraction,finaltime,finalfraction,offsetx,offsety

      ios = 1

      write(99, '(/, "Entering VVENT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'VVENT'
      n_vvents = 0 ! has to be declared in data
      vvent_loop: do
         call checkread ('VVENT',LU,ios)
         if (ios==0) vventflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving vvent_loop")')
             exit vvent_loop
         end if 
         write(99, '(/, "Found VVENT, trying to read it")')
         read(LU,VVENT,err=34,iostat=ios)
         n_vvents =n_vvents + 1
34       if (ios>0) then
             write(99, '(a,i3,a,i5)') 'Error: Problem with VVENT number ', n_vvents+1, ', line number ', input_file_line_number
             stop
         end if
      end do vvent_loop

      if (n_vvents>mxvvents) then
         write (*,'(a,i3)') '***Error: Bad VVENT input. Too many vvent in input data file. limit is ', mxvvents
         write (99,'(a,i3)') '***Error: Bad VVENT input. Too many vvent in input data file. limit is ', mxvvents
         stop
      end if

      vvent_flag: if (vventflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_vvent_loop")')
      ! Assign value to CFAST variables for further calculations
      read_vvent_loop: do ii=1,n_vvents
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_vvents = ", i3)') &
                     ii, n_vvents
      
         call checkread('VVENT',LU,ios)
         call set_vvent_defaults
         write(99, '(/)')
         write(99,VVENT)
        
         read(LU,VVENT)
         write(99, '(/)')
         write(99,VVENT)
      
         i = top
         j = bottom
         k = vvent_id
      
         ! check for outside of compartment space; self pointers are covered in read_input_file
         if (i>mxrooms.or.j>mxrooms) then
            write (*,5070) i, j
            write (99,5070) i, j
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
                  write (99,*) '***Error: Bad hvent input. Vent opening specification requires an associated target.'
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
            write (99,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if
         
         write(99, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%area = ", f10.4, &
                     /, "  ventptr%shape = ", i3, &
                     /, "  ventptr%opening_type = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_initial_fraction = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4, &
                     /, "  ventptr%opening_criterion = ", f10.4, &
                     /, "  ventptr%opening_target = ", i3, &
                     /, "  ventptr%xoffset = ", f10.4, &
                     /, "  ventptr%yoffset = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%area, &
                     ventptr%shape,ventptr%opening_type,ventptr%opening_initial_time, &
                     ventptr%opening_initial_fraction,ventptr%opening_final_time, &
                     ventptr%opening_final_fraction, ventptr%opening_criterion, &
                     ventptr%opening_target,ventptr%xoffset,ventptr%yoffset
      
      end do read_vvent_loop
      
      write(99, '(/, "Left read_vvent_loop")')

      end if vvent_flag
      write(99, '(/, "leaving read_vvent (Done)")')
5070  format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)

      
      contains
      
      subroutine set_vvent_defaults
      
      top                     = 0
      bottom                  = 0
      vvent_id                = 0
      area                    = 0.0
      shape                   = 1
      type                    = 'null'
      criterion               = 0
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0
      finaltime               = 1.0_eb
      finalfraction           = 0.0
      offsetx                 = 0.0
      offsety                 = 0.0
      
      end subroutine set_vvent_defaults
      
      end subroutine read_vvent


      ! --------------------------- MVENT -------------------------------------------
      subroutine read_mvent(LU)
      
      integer :: ios,ii,i,j,k
      integer :: LU
      
      type(vent_type), pointer :: ventptr
!      type(room_type), pointer :: roomptr
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

      write(99, '(/, "Entering MVENT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'MVENT'
      n_mvents = 0
      mvent_loop: do
         call checkread ('MVENT',LU,ios)
         if (ios==0) mventflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving mvent_loop")')
             exit mvent_loop
         end if 
         write(99, '(/, "Found MVENT, trying to read it")')
         read(LU,MVENT,err=34,iostat=ios)
         n_mvents =n_mvents + 1
34       if (ios>0) then
             write(99, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_mvents+1, ', line number ', input_file_line_number
             stop
          end if
      end do mvent_loop

      if (n_mvents>mxmvents) then
         write (*,'(a,i3)') '***Error: Bad MVENT input. Too many mvent in input data file. limit is ', mxmvents 
         write (99,'(a,i3)') '***Error: Bad MVENT input. Too many mvent in input data file. limit is ', mxmvents
         stop
      end if

      mvent_flag: if (mventflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0
      
      write(99, '(/, "Entering read_mvent_loop")')
      ! Assign value to CFAST variables for further calculations
      read_mvent_loop: do ii=1,n_mvents
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_mvents = ", i3)') &
                     ii, n_mvents
      
         call checkread('MVENT',LU,ios)
         call set_mvent_defaults
         write(99, '(/)')
         write(99,MVENT)
        
         read(LU,MVENT)
         write(99, '(/)')
         write(99,MVENT)
      
         i = first
         j = second
         k = mvent_id
         if (i>nr.or.j>nr) then
            write (*,5191) i, j
            write (99,5191) i, j
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
                  write (99,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
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
            write (99,*) 'Type has to be "TIME", "TEMP", or "FLUX".'
            stop
         end if
         
         write(99, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%orientation(1) = ", i3, &
                     /, "  ventptr%height(1) = ", f10.4, &
                     /, "  ventptr%diffuser_area(1) = ", f10.4, &
                     /, "  ventptr%orientation(1) = ", i3, &
                     /, "  ventptr%height(2) = ", f10.4, &
                     /, "  ventptr%diffuser_area(2) = ", f10.4, &
                     /, "  ventptr%coeff(1) = ", f10.4, &
                     /, "  ventptr%maxflow = ", f10.4, &
                     /, "  ventptr%min_cutoff_relp = ", f10.4, &
                     /, "  ventptr%max_cutoff_relp = ", f10.4, &
                     /, "  ventptr%opening_type = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_initial_fraction = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4, &
                     /, "  ventptr%opening_criterion = ", f10.4, &
                     /, "  ventptr%opening_target = ", i3, &
                     /, "  ventptr%xoffset = ", f10.4, &
                     /, "  ventptr%yoffset = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%orientation(1), &
                     ventptr%height(1),ventptr%diffuser_area(1),ventptr%orientation(1),ventptr%height(2), &
                     ventptr%diffuser_area(2),ventptr%coeff(1),ventptr%maxflow, &
                     ventptr%min_cutoff_relp,ventptr%max_cutoff_relp, &
                     ventptr%opening_type, ventptr%opening_initial_time, &
                     ventptr%opening_initial_fraction,ventptr%opening_final_time,ventptr%opening_final_fraction, &
                     ventptr%opening_criterion,ventptr%opening_target,ventptr%xoffset, &
                     ventptr%yoffset
      
      end do read_mvent_loop

      write(99, '(/, "Left read_mvent_loop")')

      end if mvent_flag
      
      write(99, '(/, "leaving read_mvent (Done)")')
      
5191  format ('***Error: Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)
      

      contains
      
      subroutine set_mvent_defaults
      
      first                   = 0
      second                  = 0
      mvent_id                = 0
      orientation1            = ''
      height1                 = 0.0
      area1                   = 0.0
      orientation2            = ''
      height2                 = 0.0
      area2                   = 0.0
      flow                    = 0.0
      plower                  = 0.0
      pupper                  = 0.0
      type                    = 'null'
      criterion               = 0
      target                  = 'null'
      initialtime             = 1.0_eb
      initialfraction         = 0.0
      finaltime               = 1.0_eb
      finalfraction           = 0.0
      offsetx                 = 0.0
      offsety                 = 0.0
      
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

      write(99, '(/, "Entering DETEC")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'DETEC'
      n_detectors = 0
      detec_loop: do
         call checkread ('DETEC', LU, ios)
         if (ios==0) detecflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving detec_loop")')
             exit detec_loop
         end if 
         write(99, '(/, "Found DETEC, trying to read it")')
         if (ios==1) exit detec_loop
         read(LU,detec,err=34,iostat=ios)
         n_detectors =n_detectors + 1
34       if (ios>0) then
             write(99, '(a,i3,a,i5)') 'Error: Problem with fire number ', n_detectors+1, ', line number ', input_file_line_number
             stop
          end if
        end do detec_loop

        if (n_detectors>mxdtect) then 
           write (*,'(a,i3)') '***Error: Bad DETEC input. Too many mvent in input data file. limit is ', mxdtect 
           write (99,'(a,i3)') '***Error: Bad DETEC input. Too many mvent in input data file. limit is ', mxdtect
           stop
        end if

        detec_flag: if (detecflag) then
        write(99, '(/, "on the way")')

        rewind (LU) 
        input_file_line_number = 0

        write(99, '(/, "Entering read_detec_loop")')
        ! Assign value to CFAST variables for further calculations
        read_detec_loop: do ii=1,n_detectors
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  n_detectors = ", i3)') &
                     ii, n_detectors
        
           call checkread('DETEC',LU,ios)
           call set_detec_defaults
           write(99, '(/)')
           write(99,DETEC)
        
           read(LU,DETEC)
           write(99, '(/)')
           write(99,DETEC)
        
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
              write (99,5342) i2
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
              write (99,5344) i2
              stop
           end if
        
           if (dtectptr%center(1)>roomptr%cwidth.or. &
               dtectptr%center(2)>roomptr%cdepth.or.dtectptr%center(3)>roomptr%cheight) then
              write (*,5339) n_detectors,roomptr%name
              write (99,5339) n_detectors,roomptr%name
              stop
           end if
         
           write(99, '(/, "  dtectptr%dtype = ", i3, &
                       /, "  dtectptr%room = ", i3, &
                       /, "  dtectptr%trigger = ", f10.4, &
                       /, "  dtectptr%center(1) = ", f10.4, &
                       /, "  dtectptr%center(2) = ", f10.4, &
                       /, "  dtectptr%center(3) = ", f10.4, &
                       /, "  dtectptr%rti = ", f10.4, &
                       /, "  dtectptr%quench = ", L3, &
                       /, "  dtectptr%spray_density = ", f15.4)') &
                       dtectptr%dtype,dtectptr%room,dtectptr%trigger,dtectptr%center(1), &
                       dtectptr%center(2),dtectptr%center(3),dtectptr%rti,dtectptr%quench, &
                       dtectptr%spray_density
        
        end do read_detec_loop
      
        write(99, '(/, "Left read_detec_loop")')

        end if detec_flag

        write(99, '(/, "leaving read_detec (Done)")')
        
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

      write(99, '(/, "Entering VHEAT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'VHEAT'
      nvcons=0
      vheat_loop: do
      call checkread ('VHEAT',LU,ios)
         if (ios==0) vheatflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving vheat_loop")')
             exit vheat_loop
         end if 
         write(99, '(/, "Found VHEAT, trying to read it")')
         read(LU,VHEAT,err=34,iostat=ios)
         nvcons = nvcons + 1
34       if (ios>0) then
               write(99, '(a,i5)') 'Error: Problem with VHEAT number, line number', input_file_line_number
               stop
            end if
      end do vheat_loop

      vheat_flag: if (vheatflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_detec_loop")')
      ! Assign value to CFAST variables for further calculations
      read_vheat_loop: do ii=1,nvcons
      
         call checkread('VHEAT',LU,ios)
         call set_vheat_defaults
         write(99, '(/)')
         write(99,VHEAT)
        
         read(LU,VHEAT)
         write(99, '(/)')
         write(99,VHEAT)
      
         i1 = top
         i2 = bottom
         if (i1<1.or.i2<1.or.i1>nr.or.i2>nr) then
            write (*,5345) i1, i2
            write (99,5345) i1, i2
            stop
         end if
      
         i_vconnections(ii,w_from_room) = i1
         i_vconnections(ii,w_from_wall) = 2
         i_vconnections(ii,w_to_room) = i2
         i_vconnections(ii,w_to_wall) = 1
         
         write(99, '(/, "  vheatflag = ", L3, &
                     /, "  ii = ", i3, &
                     /, "  i_vconnections(ii,w_from_room) = ", i3, &
                     /, "  i_vconnections(ii,w_from_wall) = ", i3, &
                     /, "  i_vconnections(ii,w_to_room) = ", i3, &
                     /, "  i_vconnections(ii,w_to_wall) = ", i3)') &
                     vheatflag,ii,i_vconnections(ii,w_from_room), &
                     i_vconnections(ii,w_from_wall),i_vconnections(ii,w_to_room), &
                     i_vconnections(ii,w_to_wall)
      
      end do read_vheat_loop
      
      write(99, '(/, "Left read_vheat_loop")')   

      end if vheat_flag

      write(99, '(/, "leaving vheat_flag (Done)")')
      
5345  format ('***Error: Bad VHEAT input. A referenced compartment does not exist')
      

      contains
      
      subroutine set_vheat_defaults
      
      top                       = 0
      bottom                    = 0
      
      end subroutine set_vheat_defaults
      
      end subroutine read_vheat
      
     
      ! --------------------------- CONEZ -------------------------------------------
      subroutine read_conez(LU)
      
      integer :: ios,iroom
      integer :: LU
      
      type(room_type), pointer :: roomptr
      
      integer   :: compartment
      namelist /CONEZ/compartment

      ios = 1

      write(99, '(/, "Entering CONEZ")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CONEZ'
      onez_loop: do
         call checkread ('CONEZ',LU,ios)
         if (ios==0) conezflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving onez_loop")')
             exit onez_loop
         end if 
         write(99, '(/, "Found CONEZ, trying to read it")')
         read(LU,CONEZ,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with fire number, line number', input_file_line_number
             stop
         end if
      end do onez_loop

      conez_flag: if (conezflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('CONEZ',LU,ios)
      call set_onez_defaults
      write(99, '(/)')
      write(99,CONEZ)
        
      read(LU,CONEZ)
      write(99, '(/)')
      write(99,CONEZ)
      
      iroom = compartment
      if (iroom<1.or.iroom>nr) then
         write (*, 5001) iroom ! why i1 (bug)
         write (99, 5001) iroom
         stop
      end if
      roomptr => roominfo(iroom)
      roomptr%shaft = .true.
         
      write(99, '(/, "  roomptr%shaft%dtype = ", L3)') &
                  roomptr%shaft

      end if conez_flag

      write(99, '(/, "leaving read_onez (Done)")')
      
5001  format ('***Error: Bad ONEZ input. Referenced compartment is not defined ',i0)
      
      contains
      
      subroutine set_onez_defaults
      
      compartment                     = 0
      
      end subroutine set_onez_defaults
      
      end subroutine read_conez


      ! --------------------------- CHALL -------------------------------------------
      subroutine read_chall(LU)
      
      integer :: ios,iroom
      integer :: LU
      
      type(room_type), pointer :: roomptr

      integer :: compartment
      namelist /CHALL/compartment

      ios = 1

      write(99, '(/, "Entering CHALL")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CHALL'
      hall_loop: do
         call checkread ('CHALL',LU,ios)
         if (ios==0) challflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving hall_loop")')
             exit hall_loop
         end if 
         write(99, '(/, "Found CHALL, trying to read it")')
         read(LU,CHALL,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with CHALL number, line number', input_file_line_number
             stop
         end if
      end do hall_loop

      chall_flag: if (challflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('CHALL',LU,ios)
      call set_hall_defaults
      write(99, '(/)')
      write(99,CHALL)
        
      read(LU,CHALL)
      write(99, '(/)')
      write(99,CHALL)
      
      iroom = compartment
      if (iroom<1.or.iroom>nr) then
         write (*, 5346) iroom
         write (99, 5346) iroom
         stop
      end if
      roomptr => roominfo(iroom)
      roomptr%hall = .true.
         
      write(99, '(/, "  roomptr%shaft%dtype = ", L3)') &
                  roomptr%shaft

      end if chall_flag

      write(99, '(/, "leaving read_chall (Done)")')
      
5346  format ('***Error: Bad HALL input. A referenced compartment does not exist ',i0)

      contains
      
      subroutine set_hall_defaults
      
      compartment                     = 0
      
      end subroutine set_hall_defaults
      
      end subroutine read_chall


      ! --------------------------- ROOMA -------------------------------------------
      subroutine read_rooma(LU)
      
      integer :: ios,iroom,i,npts
      integer :: LU
      character(48) :: myfmt
      
      type(room_type), pointer :: roomptr

      integer :: compartment,number
      real(eb), dimension(mxpts) :: area
      namelist /rooma/compartment,number,area

      ios = 1

      write(99, '(/, "Entering CHROOMAALL")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'ROOMA'
      rooma_loop: do
         call checkread ('ROOMA',LU,ios)
         if (ios==0) roomaflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving rooma_loop")')
             exit rooma_loop
         end if 
         write(99, '(/, "Found ROOMA, trying to read it")')
         read(LU,ROOMA,err=34,iostat=ios)
34       if (ios>0) then
               write(99, '(A,i5)') 'Error: Problem with ROOMA number, line number', input_file_line_number
               stop
         end if
      end do rooma_loop

      rooma_flag: if (roomaflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('ROOMA',LU,ios)
      call set_rooma_defaults
      write(99, '(/)')
      write(99,ROOMA)
        
      read(LU,ROOMA)
      write(99, '(/)')
      write(99,ROOMA)
      
      iroom = compartment
      roomptr => roominfo(iroom)
      
      ! make sure the room number is valid
      if (iroom<1.or.iroom>nr) then
         write (*,5347) iroom
         write (99,5347) iroom
         stop
      end if
      
      ! make sure the number of points is valid
      npts = number
      if (npts>mxcross.or.npts<=0) then
         write (*,5347) npts
         write (99,5347) npts
         stop
      end if
      if (roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
      roomptr%nvars = npts
      
      ! make sure all data is positive
      do  i = 1, npts
         if (area(i)<0.0_eb) then
            write (*,5348) area(i)
            write (99,5348) area(i)
            stop
         end if
      end do
      
      ! put the data in its place
      do i = 1, npts
         roomptr%var_area(i) = area(i)
      end do
                  
      write(myfmt, '("("i3,"(f10.4))")')  npts

      write(99, '(/, "  roomptr%shaft%var_area = ")')
      write(99, fmt=myfmt) roomptr%var_area(1:npts)

      end if rooma_flag

      write(99, '(/, "leaving read_rooma (Done)")')
      
5347  format ('***Error: Bad ROOMA input. Compartment specified by ROOMA does not exist ',i0)
5348  format ('***Error: Bad ROOMA or ROOMH input. Data on the ROOMA (or H) line must be positive ',1pg12.3)

      contains
      
      subroutine set_rooma_defaults
      
      compartment                     = 0 
      number                          = 0 
      area(1:mxpts)                   = 0.0
      
      end subroutine set_rooma_defaults
      
      end subroutine read_rooma


      ! --------------------------- ROOMH -------------------------------------------
      subroutine read_roomh(LU)

      integer :: ios,iroom,i,npts
      integer :: LU
      character(48) :: myfmt
      
      type(room_type), pointer :: roomptr
      integer :: compartment,number
      real(eb), dimension(mxpts) :: height
      namelist /ROOMH/compartment,number,height

      ios = 1

      write(99, '(/, "Entering ROOMH")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'ROOMH'
      roomh_loop: do
         call checkread ('ROOMH',LU,ios)
         if (ios==0) roomhflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving roomh_loop")')
             exit roomh_loop
         end if 
         write(99, '(/, "Found ROOMH, trying to read it")')
         read(LU,ROOMH,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with ROOMH number, line number', input_file_line_number
             stop
         end if
      end do roomh_loop

      roomh_flag: if (roomhflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('ROOMH',LU,ios)
      call set_roomh_defaults
      write(99, '(/)')
      write(99,ROOMH)
        
      read(LU,ROOMH)
      write(99, '(/)')
      write(99,ROOMH)
      
      iroom = compartment
      roomptr => roominfo(iroom)
      
      ! make sure the room number is valid
      if (iroom<1.or.iroom>nr) then
         write (*,5349) iroom
         write (99,5349) iroom
         stop
      end if
      
      ! make sure the number of points is valid
      npts = number
      if (npts>mxcross.or.npts<=0) then
         write (*,5350) npts
         write (99,5350) npts
         stop
      end if
      if (roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
      roomptr%nvars = npts
      
      ! make sure all data is positive
      do  i = 1, npts
         if (height(i)<0.0_eb) then
            write (*,5348) height(i)
            write (99,5348) height(i)
            stop
         end if
      end do
      
      ! put the data in its place
      do i = 1, npts
         roomptr%var_height(i) = height(i)
      end do
                        
      write(myfmt, '("("i3,"(f10.4))")')  npts

      write(99, '(/, "  roomptr%shaft%var_height = ")')
      write(99, fmt=myfmt) roomptr%var_height(1:npts)

      end if roomh_flag

      write(99, '(/, "leaving read_roomh (Done)")')

5348  format ('***Error: Bad ROOMA or ROOMH input. Data on the ROOMA (or H) line must be positive ',1pg12.3)
5349  format ('***Error: Bad ROOMH input. Compartment specified by ROOMH is not defined ',i0)
5350  format ('***Error: Bad ROOMH input. ROOMH error on data line ',i0)



      contains
      
      subroutine set_roomh_defaults
      
      compartment                     = 0 
      number                          = 0 
      height(1:mxpts)                 = 0.0
      
      end subroutine set_roomh_defaults
      
      end subroutine read_roomh


      ! --------------------------- HHEAT -------------------------------------------
      subroutine read_hheat(LU)
      
      integer :: ios,nto,ifrom,ito,i
      integer :: LU
      real(eb) :: frac
      character(64) :: myfmt
      
      type(room_type), pointer :: roomptr

      integer :: first,nop
      real(eb), dimension(mxpts) :: npos_comp,npos_frac
      namelist/HHEAT/first,nop,npos_comp,npos_frac

      ios = 1

      write(99, '(/, "Entering HHEAT")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'HHEAT'
      hheat_loop: do
         call checkread ('HHEAT', LU, ios)
         if (ios==0) hheatflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving hheat_loop")')
             exit hheat_loop
         end if 
         write(99, '(/, "Found HHEAT, trying to read it")')
         read(LU,HHEAT,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with HHEAT number, line number', input_file_line_number
             stop
         end if
      end do hheat_loop

      hheat_flag: if (hheatflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('HHEAT',LU,ios)
      call set_hheat_defaults
      write(99, '(/)')
      write(99,HHEAT)
        
      read(LU,HHEAT)
      write(99, '(/)')
      write(99,HHEAT)

      nto = 0
      
      ifrom = first
      roomptr => roominfo(ifrom)
      nto = nop
      roomptr%iheat = 2
      
      do i = 1, nto
         ito = npos_comp(i) ! if all = 2, will pass to same ito
         frac = npos_frac(i)
         if (ito<1.or.ito==ifrom.or.ito>nr) then
            write (*, 5356) ifrom,ito
            write (99, 5356) ifrom,ito
            stop
         end if
         if (frac<0.0_eb.or.frac>1.0_eb) then
            write (*, 5357) ifrom,ito,frac
            write (99, 5357) ifrom,ito,frac
            stop
         end if
         roomptr%heat_frac(ito) = frac
      end do
                        
      write(myfmt, '("("i3,"(f10.4))")')  nto

      write(99, '(/, "  roomptr%heat_frac = ")')
      write(99, fmt=myfmt) roomptr%heat_frac(1:nto)

      end if hheat_flag

      write(99, '(/, "leaving read_hheat (Done)")')

5356  format ('***Error: Bad HHEAT input. HHEAT specification error in compartment pairs: ',2i3)
5357  format ('***Error: Bad HHEAT input. Error in fraction for HHEAT:',2i3,f6.3)



      contains
      
      subroutine set_hheat_defaults
      
      first                     = 0 
      nop                       = 0 
      npos_comp(1:mxpts)        = 0.0
      npos_frac(1:mxpts)        = 0.0
      
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

      write(99, '(/, "Entering DTCHE")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'DTCHE'
      dtche_loop: do
         call checkread ('DTCHE',LU,ios)
         if (ios==0) dtcheflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving dtche_loop")')
             exit dtche_loop
         end if 
         write(99, '(/, "Found DTCHE, trying to read it")')
         read(LU,DTCHE,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with DTCHE number, line number', input_file_line_number
             stop
         end if
      end do dtche_loop

      dtche_flag: if (dtcheflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('DTCHE',LU,ios)
      call set_dtche_defaults
      write(99, '(/)')
      write(99,DTCHE)
        
      read(LU,DTCHE)
      write(99, '(/)')
      write(99,DTCHE)

      stpmin = abs(time)
      stpmin_cnt_max = abs(count)
      ! a negative turns off the check
      if (count<=0) stpminflag = .false.
         
      write(99, '(/, "  stpmin = ", e10.4, &
                  /, "  stpmin_cnt_max = ", i3, &
                  /, "  stpminflag = ", L3)') &
                  stpmin,stpmin_cnt_max,stpminflag

      end if dtche_flag

      write(99, '(/, "leaving read_dtche (Done)")')



      contains
      
      subroutine set_dtche_defaults
      
      time                     = 0.0
      count                    = 0
      
      end subroutine set_dtche_defaults

      end subroutine read_dtche


      ! --------------------------- FURNC -------------------------------------------
      subroutine read_furnc(LU)
      
      integer :: ios,i
      integer :: LU

      character(64) :: myfmt

      integer :: number
      real(eb), dimension(mxpts) :: time,temp
      namelist/FURNC/number,time,temp

      ios = 1

      write(99, '(/, "Entering FURNC")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'FURNC'
      furn_loop: do
         call checkread ('FURNC',LU,ios)
         if (ios==0) furncflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving furn_loop")')
             exit furn_loop
         end if 
         write(99, '(/, "Found FURNC, trying to read it")')
         read(LU,FURNC,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with FURNC number, line number', input_file_line_number
             stop
         end if
      end do furn_loop

      furnc_flag: if (furncflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('FURNC',LU,ios)
      call set_furn_defaults
      write(99, '(/)')
      write(99,FURNC)
        
      read(LU,FURNC)
      write(99, '(/)')
      write(99,FURNC)

      nfurn=number+0.5 !why adding 0.5
      do i = 1, nfurn
         furn_time(i)=time(i)
         furn_temp(i)=temp(i)
      end do
                        
      write(myfmt, '("("i3,"(f10.4))")')  nfurn

      write(99, '(/, "  furn_time = ")')
      write(99, fmt=myfmt) time(1:nfurn)

      write(99, '(/, "  rfurn_temp = ")')
      write(99, fmt=myfmt) temp(1:nfurn)

      end if furnc_flag

      write(99, '(/, "leaving read_furnc (Done)")')



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

      write(99, '(/, "Entering ADIAB")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'ADIAB'
      adiab_loop: do
         call checkread ('ADIAB',LU,ios)
         if (ios==0) adiabflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving adiab_loop")')
             exit adiab_loop
         end if 
         write(99, '(/, "Found ADIAB, trying to read it")')
         read(LU,ADIAB,err=34,iostat=ios)
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with ADIAB number, line number', input_file_line_number
             stop
         end if
      end do adiab_loop

      adiab_flag: if (adiabflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      call checkread('ADIAB',LU,ios)
      call set_adiab_defaults
      write(99, '(/)')
      write(99,ADIAB)
        
      read(LU,ADIAB)
      write(99, '(/)')
      write(99,ADIAB)

      adiabatic_walls=.false.

      if (adiab_walls=='TRUE') adiabatic_walls=.true.
         
      write(99, '(/, "  adiabatic_walls = ", L3)') &
                  adiabatic_walls

      end if adiab_flag

      write(99, '(/, "leaving read_adiab (Done)")')



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

      write(99, '(/, "Entering CSLCF")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'CSLCF'
      nvisualinfo=0
      slcf_loop: do
         call checkread ('CSLCF',LU,ios)
         if (ios==0) cslcfflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving slcf_loop")')
             exit slcf_loop
         end if 
         write(99, '(/, "Found CSLCF, trying to read it")')
         read(LU,CSLCF,err=34,iostat=ios)
         nvisualinfo=nvisualinfo+1
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with CSLCF number, line number', input_file_line_number
             stop
         end if
      end do slcf_loop

      cslcf_flag: if (cslcfflag) then
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_slcf_loop")')
      ! Assign value to CFAST variables for further calculations
      read_slcf_loop: do ii=1,nvisualinfo
      
        write(99, '(/, "  ii = ", i3, &
                    /, "  nvisualinfo = ", i3)') &
                    ii, nvisualinfo

        call checkread('CSLCF',LU,ios)
        call set_slcf_defaults
        write(99, '(/)')
        write(99,CSLCF)
        
        read(LU,CSLCF)
        write(99, '(/)')
        write(99,CSLCF)

        sliceptr => visualinfo(ii)
        if (domain=='2-D') then
           sliceptr%vtype = 1
        else if (domain=='3-D') then
           sliceptr%vtype = 2
        else
           write (*, 5403) nvisualinfo
           write (99, 5403) nvisualinfo
           stop
        end if
       
        ! 2-D slice file
        if (sliceptr%vtype==1) then
           ! get position (required) and compartment (optional) first so we can check to make sure
           ! desired position is within the compartment(s)
           sliceptr%position = position
           sliceptr%roomnum  = comp(1)  !it is buggy here, only taking 1 value
           if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
              write (*, 5403) nvisualinfo
              write (99, 5403) nvisualinfo
              stop
           end if
           if (plane =='X') then
              sliceptr%axis = 1
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (99, 5403) nvisualinfo
                    stop
                 end if
              end if
           else if (plane =='Y') then
              sliceptr%axis = 2
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (99, 5403) nvisualinfo
                    stop
                 end if
              end if
           else if (plane =='Z') then
              sliceptr%axis = 3
              if (sliceptr%roomnum>0) then
                 roomptr => roominfo(sliceptr%roomnum)
                 if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                    write (*, 5403) nvisualinfo
                    write (99, 5403) nvisualinfo
                    stop
                 end if
              end if
           else
              write (*, 5403) nvisualinfo
              write (99, 5403) nvisualinfo
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
              write (99, 5403) nvisualinfo
              stop
           end if
        end if
         
        write(99, '(/, "  sliceptr%vtype = ", i3, &
                    /, "  sliceptr%position = ", f10.4, &
                    /, "  number = ", i3, &
                    /, "  sliceptr%roomnum = ", i3)') &
                    sliceptr%vtype,sliceptr%position,number,sliceptr%roomnum

      end do read_slcf_loop
      
      write(99, '(/, "Left read_slcf_loop")')

      end if cslcf_flag

      write(99, '(/, "leaving read_cslcf (Done)")')

5403  format ('***Error: Bad SLCF input. Invalid SLCF specification in visualization input ',i0)


      contains

      subroutine set_slcf_defaults

      domain                  = 'null'
      plane                   = 'null'
      position                = 0.0
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

      write(99, '(/, "Entering SISOF")')

      rewind(LU) 
      input_file_line_number = 0
      write(99, '(/, "input_file_line_number = ", i3)') &
                  input_file_line_number

      ! Scan entire file to look for 'SISOF'
      nvisualinfo=0
      isof_loop: do
         call checkread ('CISOF',LU,ios)
         if (ios==0) cisofflag=.true.
         write(99, '(/, "  ios = ", i3, &
                     /, "  input_file_line_number = ", i3)') &
                     ios, input_file_line_number
         if (ios==1) then
             write(99, '(/, "End of file, leaving isof_loop")')
             exit isof_loop
         end if 
         write(99, '(/, "Found CISOF, trying to read it")')
         read(LU,CISOF,err=34,iostat=ios)
         nvisualinfo=nvisualinfo+1
34       if (ios>0) then
             write(99, '(A,i5)') 'Error: Problem with CISOF number, line number', input_file_line_number
             stop
         end if
      end do isof_loop

      cisof_flag: if (cisofflag) then 
      write(99, '(/, "on the way")')

      rewind (LU) 
      input_file_line_number = 0

      write(99, '(/, "Entering read_isof_loop")')
      ! Assign value to CFAST variables for further calculations
      read_isof_loop: do ii=1,nvisualinfo 
      
         write(99, '(/, "  ii = ", i3, &
                     /, "  nvisualinfo = ", i3)') &
                     ii, nvisualinfo

         call checkread('CISOF',LU,ios)
         call set_isof_defaults
         write(99, '(/)')
         write(99,CISOF)
        
         read(LU,CISOF)
         write(99, '(/)')
         write(99,CISOF)

         sliceptr => visualinfo(ii)
         sliceptr%vtype = 3
         sliceptr%value = value
         sliceptr%roomnum = comp

         if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
            write (*, 5404) nvisualinfo
            write (99, 5404) nvisualinfo
            stop
         end if
         
      write(99, '(/, "  sliceptr%vtype = ", i3, &
                  /, "  sliceptr%value = ", f10.4, &
                  /, "  sliceptr%roomnum = ", i3)') &
                  sliceptr%vtype,sliceptr%value,sliceptr%roomnum

      end do read_isof_loop
      
      write(99, '(/, "Left read_isof_loop")')

      end if cisof_flag

      write(99, '(/, "leaving read_sisof (Done)")')

5404  format ('***Error: Bad ISOF input. Invalid ISOF specification in visualization input ',i0)



      contains

      subroutine set_isof_defaults

      value                   = 0.0
      comp                    = 0

      end subroutine set_isof_defaults

      end subroutine read_cisof
      

      ! --------------------------- Testing printout ----------------------------------
      subroutine testing(ncomp)
      
      integer :: i,ncomp
      character(20) :: myfmt
      
      type(room_type), pointer :: roomptr!,roomptr2
      type(thermal_type), pointer :: thrmpptr
      type(target_type), pointer :: targptr
      type(fire_type), pointer :: fireptr
      type(vent_type), pointer :: ventptr
!      type(detector_type), pointer :: dtectptr
          
      
      write(98, '(/, "  stpmaflag = ", L3, &
                  /, "  stpmax = ", f10.4)') &
                  stpmaflag,stpmax
      
      do i=1,n_thrmp
         thrmpptr => thermalinfo(i)
         write(98, '(/, "  matrlflag = ", L3, &
                     /, "  thrmpptr%name = ", A, &
                     /, "  thrmpptr%nslab = ", i3, &
                     /, "  thrmpptr%k(1) = ", f10.4, &
                     /, "  thrmpptr%c(1) = ", f10.4, &
                     /, "  thrmpptr%rho(1) = ", f10.4, &
                     /, "  thrmpptr%thickness(1) = ", f10.4, &
                     /, "  thrmpptr%eps = ", f10.4)') &
                     matrlflag,thrmpptr%name,thrmpptr%nslab,thrmpptr%k(1),thrmpptr%c(1), &
                     thrmpptr%rho(1),thrmpptr%thickness(1),thrmpptr%eps
      end do
      
      do i=1,ncomp
         roomptr => roominfo(i)
         
         write(98, '(/, "  compaflag = ", L3, &
                     /, "  nr = ", i3, &
                     /, "  roomptr%name = ", A12, &
                     /, "  roomptr%cwidth = ", f10.4, &
                     /, "  roomptr%cdepth = ", f10.4, &
                     /, "  roomptr%cheight = ", f10.4, &
                     /, "  roomptr%x0 = ", f10.4, &
                     /, "  roomptr%y0 = ", f10.4, &
                     /, "  roomptr%z0 = ", f10.4, &
                     /, "  roomptr%ibar = ", i3, &
                     /, "  roomptr%jbar = ", i3, &
                     /, "  roomptr%kbar = ", i3, &
                     /, "  roomptr%surface_on(1-4) = ", 4L4, &
                     /, "  roomptr%matl(1-4) = ", 4A12)') &
                     compaflag,nr,roomptr%name,roomptr%cwidth,roomptr%cdepth,roomptr%cheight, &
                     roomptr%x0,roomptr%y0,roomptr%z0,roomptr%ibar,roomptr%jbar, &
                     roomptr%kbar,roomptr%surface_on(1:4),roomptr%matl(1:4)
      end do
      
      do i=1,n_targets
         targptr => targetinfo(i)
         
         write(98, '(/, "  targeflag = ", L3, &
                     /, "  targptr%room = ", i3, &
                     /, "  targptr%center(1) = ", f10.4, &
                     /, "  targptr%center(2) = ", f10.4, &
                     /, "  targptr%center(3) = ", f10.4, &
                     /, "  targptr%normal(1) = ", f10.4, &
                     /, "  targptr%normal(2) = ", f10.4, &
                     /, "  targptr%normal(3) = ", f10.4, &
                     /, "  targptr%depth_loc = ", f10.4, &
                     /, "  targptr%name = ", A, &
                     /, "  targptr%material = ", A, &
                     /, "  targptr%equaton_type = ", i3, &
                     /, "  targptr%wall = ", i3)') &
                     targeflag,targptr%room,targptr%center(1),targptr%center(2), &
                     targptr%center(3), targptr%normal(1),targptr%normal(2), &
                     targptr%normal(3),targptr%depth_loc,targptr%name, &
                     targptr%material,targptr%equaton_type,targptr%wall
      end do
      
      do i=1,n_fires
         fireptr => fireinfo(i)
         
         write(98, '(/, "  cfireflag = ", L3, &
                     /, "  fireptr%name = ", A12, &
                     /, "  fireptr%room = ", i3, &
                     /, "  fireptr%chemistry_type = ", i3, &
                     /, "  fireptr%x_position = ", f10.4, &
                     /, "  fireptr%y_position = ", f10.4, &
                     /, "  fireptr%z_position = ", f10.4, &
                     /, "  fireptr%modified_plume = ", i3, &
                     /, "  fireptr%ignition_type = ", i3, &
                     /, "  fireptr%ignition_target = ", i3, &
                     /, "  fireptr%ignited = ", L3, &
                     /, "  fireptr%reported = ", L3, &
                     /, "  fireptr%ignition_criterion = ", e10.4)') &
                     cfireflag,fireptr%name,fireptr%room,fireptr%chemistry_type,fireptr%x_position, &
                     fireptr%y_position,fireptr%z_position,fireptr%modified_plume,fireptr%ignition_type, &
                     fireptr%ignition_target,fireptr%ignited,fireptr%reported, &
                     fireptr%ignition_criterion
      end do
      
      do i=1,n_chemi
         fireptr => fireinfo(i)

         write(98, '(/, "  cfireflag = ", L3, &
                     /, "  chemiflag = ", L3, &
                     /, "  fireptr%n_c = ", f10.4, &
                     /, "  fireptr%n_h = ", f10.4, &
                     /, "  fireptr%n_o = ", f10.4, &
                     /, "  fireptr%n_n = ", f10.4, &
                     /, "  fireptr%n_cl = ", f10.4, &
                     /, "  fireptr%molar_mass = ", f10.4, &
                     /, "  fireptr%chirad = ", f10.4, &
                     /, "  fireptr%npoints = ", i3)') &
                     cfireflag,chemiflag,fireptr%n_c,fireptr%n_h,fireptr%n_o,fireptr%n_n, &
                     fireptr%n_cl,fireptr%molar_mass,fireptr%chirad,fireptr%npoints
      
         write(myfmt, '("("i3,"(f10.4))")')  fireptr%npoints  

         write(98, '(/, "  fireptr%time = ")')
         write(98, fmt=myfmt) fireptr%time(1:fireptr%npoints)
         write(98, '(/, "  fireptr%qdot = ")')
         write(98, fmt=myfmt) fireptr%qdot(1:fireptr%npoints)
         write(98, '(/, "  fireptr%mdot = ")')
         write(98, fmt=myfmt) fireptr%mdot(1:fireptr%npoints)
         write(98, '(/, "  fireptr%y_soot = ")')
         write(98, fmt=myfmt) fireptr%y_soot(1:fireptr%npoints)
         write(98, '(/, "  fireptr%y_co = ")')
         write(98, fmt=myfmt) fireptr%y_co(1:fireptr%npoints)
         write(98, '(/, "  fireptr%y_trace = ")')
         write(98, fmt=myfmt) fireptr%y_trace(1:fireptr%npoints)
         write(98, '(/, "  fireptr%area = ")')
         write(98, fmt=myfmt) fireptr%area(1:fireptr%npoints)
         write(98, '(/, "  fireptr%height = ")')
         write(98, fmt=myfmt) fireptr%height(1:fireptr%npoints)
      end do
      
      write(98, '(/, "  ctimeflag = ", L3, &
                  /, "  time_end = ", i8, &
                  /, "  print_out_interval = ", i3, &
                  /, "  smv_out_interval = ", i3, &
                  /, "  ss_out_interval = ", i3)') &
                  ctimeflag,time_end,print_out_interval,smv_out_interval,ss_out_interval

      write(98, '(/, "  eambiflag = ", L3, &
                  /, "  exterior_temperature = ", f10.4, &
                  /, "  exterior_abs_pressure = ", f15.4)') &
                  eambiflag,exterior_temperature,exterior_abs_pressure

      write(98, '(/, "  tambiflag = ", L3, &
                  /, "  interior_temperature = ", f10.4, &
                  /, "  interior_abs_pressure = ", f15.4, &
                  /, "  tgignt = ", f10.4, &
                  /, "  relative_humidity = ", f10.4)') &
                  tambiflag,interior_temperature,interior_abs_pressure,tgignt,relative_humidity

      write(98, '(/, "  limo2flag = ", L3, &
                  /, "  lower_o2_limit = ", f10.4)') &
                  limo2flag,lower_o2_limit
      
      do i=1,n_hvents
         ventptr => hventinfo(i)

         write(98, '(/, "  ventptr%room1 = ", i3, &
                     /, "  ventptr%room2 = ", i3, &
                     /, "  ventptr%counter = ", i3, &
                     /, "  ventptr%width = ", f10.4, &
                     /, "  ventptr%soffit = ", f10.4, &
                     /, "  ventptr%sill = ", f10.4, &
                     /, "  ventptr%offset(1) = ", f10.4, &
                     /, "  ventptr%offset(2) = ", f10.4, &
                     /, "  ventptr%face = ", i3, &
                     /, "  ventptr%opening_type = ", i3, &
                     /, "  ventptr%opening_initial_time = ", f10.4, &
                     /, "  ventptr%opening_initial_fraction = ", f10.4, &
                     /, "  ventptr%opening_final_time = ", f10.4, &
                     /, "  ventptr%opening_final_fraction = ", f10.4, &
                     /, "  ventptr%opening_criterion = ", f10.4, &
                     /, "  ventptr%opening_target = ", i3, &
                     /, "  ventptr%absolute_soffit = ", f10.4, &
                     /, "  ventptr%absolute_sill = ", f10.4)') &
                     ventptr%room1,ventptr%room2,ventptr%counter,ventptr%width, &
                     ventptr%soffit,ventptr%sill,ventptr%offset(1),ventptr%offset(2), &
                     ventptr%face,ventptr%opening_type,ventptr%opening_initial_time, &
                     ventptr%opening_initial_fraction,ventptr%opening_final_time, &
                     ventptr%opening_final_fraction, ventptr%opening_criterion, &
                     ventptr%opening_target,ventptr%absolute_soffit,ventptr%absolute_sill
      end do
      
      end subroutine testing
      
            

end module namelist_routines
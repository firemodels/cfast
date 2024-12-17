    program Create_scripts

    use ifport
    
    implicit none

    integer i, iofilwin, iofillinux, ios
    character(60) :: WindowsFileName, LinuxFileName
    logical :: true = .true., false = .false.
    
    ! set up windows batch file
    WindowsFileName = 'RunCFASTcases.bat'
    open (newunit=iofilwin, file=WindowsFileName, action='write', iostat=ios)
    if (ios/=0) then
        write (*,*) 'Error opening Windows batch file, ios = ',ios
        stop
    end if
    write (iofilwin,'(a)') 'echo off'
    write (iofilwin,'(a)') 'rem change the path to background.exe and cfast.exe as appropriate. Here we just assume it is in the path'
    write (iofilwin,'(a)') 'set bgexe=background.exe'
    write (iofilwin,'(a)') 'set CFAST_EXE=cfast.exe'
    write (iofilwin,'(a)') 'set MAX_ITER=100000'
    write (iofilwin,'(a)') ' '
    write (iofilwin,'(a)') 'rem you should not need to change anything from here on'
    write (iofilwin,'(a)') 'set bg=%bgexe% -u 85 -d 0.1'
    write (iofilwin,'(a)') 'set CFAST=%bg% %CFAST_EXE%'
    
    ! Here we assume 10 cases begin with 'case' plus a number followed by the file extension
    
    call WriteFileList(iofilwin, .true., '%CFAST% ', i)
    !do i =  0, nfiles
    !    write (iofilwin, '(a,i0,a)') 'echo %MAX_ITER% > case',i,'.stop'
    !    write (iofilwin,'(a,i0,a)') '%CFAST% case',i,'.in -v'
    !end do
    
    ! finish up Windows batch file'
    write (iofilwin,'(a)') ' '
    write (iofilwin,'(a)') ':loop1'
    write (iofilwin,'(a)') 'tasklist | find /i /c "CFAST" > temp.out'
    write (iofilwin,'(a)') 'set /p numexe=<temp.out'
    write (iofilwin,'(a)') 'echo waiting for %numexe% jobs to finish'
    write (iofilwin,'(a)') 'if %numexe% == 0 goto finished'
    write (iofilwin,'(a)') 'Timeout /t 30 >nul' 
    write (iofilwin,'(a)') 'goto loop1'
    write (iofilwin,'(a)') ':finished'
    close (unit = iofilwin)
    
    ! set up linux batch file
    LinuxFileName = 'RunCFASTcases.sh'
    open (newunit=iofillinux, file=linuxFileName, action='write', iostat=ios)
    if (ios/=0) then
        write (*,*) 'Error opening Linux batch file, ios = ',ios
        stop
    end if
    write (iofillinux,'(a)') '#/bin/bash'
    write (iofillinux,'(a)') 'CFAST=~/firemodels/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64'
    write (iofillinux,'(a)') 'DELAY=3'
    write (iofillinux,'(a)') 'BATCH=batch3'
    write (iofillinux,'(a)') ''
    write (iofillinux,'(a)') 'export STOPFDSMAXITER=100000'
    write (iofillinux,'(a)') ''
    
    call WriteFileList(iofillinux, .false., 'qfds.sh -D $DELAY -e $CFAST -q $BATCH  ', i)
   
    write (iofillinux,'(a)') '# Scans qstat and waits for V&V cases to start'
    write (iofillinux,'(a)') "while [[ `qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q` != '' ]]; do"
    write (iofillinux,'(a)') "   JOBS_REMAINING=`qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q | wc -l`"
    write (iofillinux,'(a)') '   echo "Waiting for ${JOBS_REMAINING} V&V cases to start."'
    write (iofillinux,'(a)') '   sleep 30'
    write (iofillinux,'(a)') 'done'
    write (iofillinux,'(a)') 
    write (iofillinux,'(a)') 
    close (unit = iofillinux)
   
   write (*,'(a,i0)') 'Windows and Linux batch scripts created, total input files = ', i
   
    end program Create_scripts
    
    subroutine WriteFileList (iofileout, includestop, cmd, nfiles)
    
    use ifport
    implicit none
    
    character (len=*), intent(in) :: cmd
    integer, intent(out) :: nfiles
    logical, intent(in) :: includestop
    
    character (len=128) :: afile, filewithoutextension
    integer :: ios, i, iloc, iofileout,iofilelist
    
    ! Find all files in the current folder with a .in extension
    
    ios = system ('dir /b *.in >filelist.txt')
    if (ios==-1) then
        ios = ierrno()
        write (*,'(a,i0)') 'Error finding CFAST input files, error = ', ios
        stop
    end if
    open (newunit=iofilelist, iostat=ios, file='filelist.txt', status='old')
    if (ios==-1) then
        write (*,'(a,i0)') 'Error reading CFAST input files, error = ', ios
        stop
    end if
    i=0
10  read (iofilelist,'(a)',end=20) afile
    i = i + 1
    
    if (includestop) then 
        iloc = index(afile,'.in')-1
        filewithoutextension = afile(1:iloc)
        write (iofileout,'(3a)') 'echo %MAX_ITER% >',trim(filewithoutextension),'.stop'
    end if
    write (iofileout,'(2a)') cmd, trim(afile)
    go to 10
    
20  continue 
    
    close (unit=iofilelist)
    nfiles = i
    return
    end subroutine WriteFileList


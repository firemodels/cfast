    program WriteandExecuteBatchFiles

    implicit none

    integer i, iofilwin, iofillinux, ios
    character(60) :: WindowsFileName, LinuxFileName
    
    ! set up windows batch file
    WindowsFileName = 'RunCFASTcases.bat'
    open (newunit=iofilwin, file=WindowsFileName, action='write', iostat=ios, status='new')
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
    do i =  0, 9
        write (iofilwin, '(a,i0,a)') 'echo %MAX_ITER% > case',i,'.stop'
        write (iofilwin,'(a,i0,a)') '%CFAST% case',i,'.in -v'
    end do
    
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
    
    end program WriteandExecuteBatchFiles


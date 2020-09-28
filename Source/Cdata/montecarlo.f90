 module montecarlo_routines

    use precision_parameters
    
    use exit_routines, only: cfastexit
    
    use setup_data, only: datapath, project, extension
    
    use montecarlo_data, only: mc_filename_pattern, mc_number_of_cases, mc_datapath, workpath
    use ppfilehandeling, only: pp_num, pp_digit, ppinfile_prefix, ppinfile_suffix, ppinfile_numfrm, ppinfile_extension
    
    implicit none  
    
    private

    public create_mc_filename, process_mc_filename_pattern
    
    contains
    
!-----------------create_mc_filename---------------------------------------------
    
    subroutine create_mc_filename(num, filename)
    
    integer, intent(in) :: num
    character, intent(out) :: filename*(*)
    
    character(len=512) :: buf
    character(len=10) :: nbuf
    integer :: lname, lbuf
    
    lname = len(filename)
    buf = ' '
    write(nbuf,ppinfile_numfrm) num
    buf = trim(mc_datapath) // trim(ppinfile_prefix) // trim(adjustl(nbuf))
    if (len_trim(ppinfile_suffix)>0) then
        buf = trim(buf) // trim(adjustl(ppinfile_suffix))
    end if 
    buf = trim(buf) // trim(ppinfile_extension)
    lbuf = len_trim(buf)
    if(lbuf > lname) then
        write(*,*) 'Error in CREATE_MC_FILENAME name length is too long: lname, lbuf', lname, lbuf
        call cfastexit('CREATE_MC_FILENAME',1)
    end if
    filename = ' '
    filename = trim(buf)
    
    return
    
    end subroutine create_mc_filename
    
!-----------------process_mc_filename_pattern---------------------------------------------
    
    subroutine process_mc_filename_pattern()
    
    integer :: fieldw, idxnum, idxfirstdig, idxlastdig
    
    if (mc_number_of_cases <= 0) then
        write(*,*)'Error in PROCESS_MC_FILENAME_PATTERN, MC_NUMBER_OF_CASES = ', mc_number_of_cases
        write(*,*)'MC_NUMBER_OF_CASES must be >0'
        call cfastexit('PROCESS_MC_FILENAME_PATTERN',1)
    elseif (mc_number_of_cases >= 1e6) then
        write(*,*)'Error in PROCESS_MC_FILENAME_PATTERN, MC_NUMBER_OF_CASES to large ', mc_number_of_cases
        write(*,*)'MC_NUMBER_OF_CASES must be < ',1e6
        call cfastexit('PROCESS_MC_FILENAME_PATTERN',2)
    end if
    
    fieldw = int(dlog10(mc_number_of_cases))+1
    ppinfile_prefix = ' '
    ppinfile_suffix = ' '
    ppinfile_numfrm = ' '
    mc_datapath = trim(workpath)
    
    ppinfile_prefix = trim(project) // '-'
    ppinfile_extension = trim(extension)
    write(ppinfile_numfrm,'(a2,i1,a1)') '(i',fieldw,')'
    return
    
    end subroutine process_mc_filename_pattern
    
    end module montecarlo_routines
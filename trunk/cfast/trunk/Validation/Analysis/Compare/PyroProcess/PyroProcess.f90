program PyroProcess

    ! reads and calculates relative differences from a PyroGraph debug file to stuff it into
    ! the excel spreadsheet table for validation guide tables and stats
    
    implicit none

    ! Variables
    integer, parameter :: max_diff = 1, min_diff = 2
    
    character fname*128, in*128, pyrograph_file*128, quantity*128, achar
    real exp_0, exp_drop, exp_rise, mod_0, mod_drop, mod_rise, exp_peak, mod_peak, relative_difference
    integer read_status, min_or_max, ic, ir, i
    

    ! Body of PyroProcess
    min_or_max = 0
    ir = 0
    
    pyrograph_file = 'pyrograph.txt'
    open (unit=8,file=pyrograph_file,form='formatted')
	open (unit=9, file='list.txt',form='formatted')
	
10  read (8,'(A)',iostat=read_status) in
    if (read_status.eq.0) then
        if (index(in,'Processing Data for: ').ne.0) then
            ic = index(in,'Processing Data for: ') + 21
            fname = in(ic:len(in))
            quantity = ' '
            if (index(fname,'iBMB_Pool').ne.0) then
                ic = index(fname,'iBMB_Pool')+9
                quantity = fname(ic+1:len(fname))
                fname(ic:len(fname)) = ' '
            else if (index(fname,'iBMB_Cable').ne.0) then
                ic = index(fname,'iBMB_Cable')+10
                quantity = fname(ic+1:len(fname))
                fname(ic:len(fname)) = ' '
            else
                do i = 1, len(fname)-1
                    ic = len(fname)-i
                    achar = fname(ic:ic)
                    if (achar.ge.'0'.and.achar.le.'9') then
                        if(fname(ic+1:ic+1).eq.'_') then
                            quantity = fname(ic+2:len(fname))
                            fname(ic+1:len(fname)) = ' '
                            exit
                        end if
                    end if
                end do
            end if
        end if
        if (index(in,'*** Compute Rise ***').ne.0) then
            min_or_max = max_diff
            read (8,'(29x,f12.0)') exp_0
            read (8,'(26x,f10.0)') exp_rise
            exp_peak = exp_0 + exp_rise
            read (8,'(29x,f12.0)') mod_0
            read (8,'(26x,f10.0)') mod_rise
            mod_peak = mod_0 + mod_rise
        end if
        if (index(in,'*** Compute Drop ***').ne.0) then
            min_or_max = min_diff
            read (8,'(29x,f12.0)') exp_0
            read (8,'(26x,f10.0)') exp_drop
            exp_peak = exp_0 - exp_drop
            read (8,'(29x,f12.0)') mod_0
            read (8,'(26x,f10.0)') mod_drop
            mod_peak = mod_0 - mod_drop
        end if
        if (min_or_max.ne.0) then
            ir = ir + 1
            relative_difference = 100.*((mod_peak - mod_0) - (exp_peak - exp_0)) / (exp_peak - exp_0)
            write (9,'(i3,2x,a40,2x,a20,2x,4f12.4,3x,f12.5)') ir, trim(fname), trim(quantity), exp_0, exp_peak, mod_0, mod_peak, relative_difference
            min_or_max = 0
        end if
        go to 10
    end if
    close (unit=8)
    close (unit=9)
end program PyroProcess


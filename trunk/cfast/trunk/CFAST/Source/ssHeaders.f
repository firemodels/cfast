      subroutine ssHeaders (ssType)
      
      ! This is the header information for the spreadsheet output
      
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      !include "fltarget.fi"
      include "objects1.fi"
      !include "iofiles77.fi"
      
      ! local variables
      integer ssType, Normal, Species, Flow, Walls
      parameter (Normal = 1, Species = 2, Flow = 3, Walls = 4)      
      parameter (maxhead = 1+7*nr+5+7*mxfire)
      character*30 headertext(3,maxhead), cTemp,
     *  NormalLabels(15), NormalLabelsShort(15), NormalUnits(15)
     *  
     
      data NormalLabels / 'Time', 
     *                    'Upper Layer Temperature', 
     *                    'Lower Layer Temperature',
     *                    'Layer Height',
     *                    'Upper Layer Volume',
     *                    'Pressure',
     *                    'Ambient Temp Target Flux',
     *                    'Floor Temp Target Flux',
     *                    'Plume Entrainment Rate',
     *                    'Pyrolysis Rate',
     *                    'HRR',
     *                    'Flame Height',
     *                    'Convective HRR',
     *                    'Total Pyrolysate Released',
     *                    'Total Trace Species Released' /
      data NormalLabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'VOL_',
     *                         'PRS_', 'ATARG_', 'FTARG_', 'PLUM_', 
     *                         'PYROL_', 'HRR_', 'FHGT_', 'HRR_C_',
     *                         'PYROL_T_', 'TRACE_T_' /
      data NormalUnits / 's', 'C', 'C', 'm', 'm^3', 'Pa', 'W/m^2', 
     *                   'W/m^2', 'kg/s', 'kg/s', 'W', 'm', 'W',
     *                   'kg', 'kg' /
      
      if (sstype.eq.Normal) then
      
        ! Normal spreadsheet header
        if (validation) then
          headertext(1,1) = NormalLabelsShort(1)
          headertext(2,1) = NormalUnits(1)
          headertext(3,1) = ' '
        else
          headertext(1,1) = NormalLabels(1)
          headertext(2,1) = ' '
          headertext(3,1) = NormalUnits(1)
        end if
        position = 1
        
        ! Compartment variables
       	do j = 1, nm1
	    do i = 1, 7
	      if (i.ne.2.or.izshaft(j).eq.0) then
	        if (i.ne.3.or.izshaft(j).eq.0) then
	          position = position + 1
	          if (validation) then
	            if (j.lt.10) then
	              write (cTemp,'(a,i1)') trim(NormalLabelsShort(i+1)), j
	            else
	              write (cTemp,'(a,i2)') trim(NormalLabelsShort(i+1)), j
	            end if
	            headertext(1,position) = cTemp
	            headertext(2,position) = NormalUnits(i+1)
	            headertext(3,1) = ' '
	          else
	            headertext(1,position) = NormalLabels(i+1)
	            headertext(2,position) = compartmentnames(j)
	            headertext(3,position) = NormalUnits(i+1)
	          end if
   	        end if
            end if
          end do
        end do
        
        ! Fire variables. Main fire first, then object fires
        if (lfbo.gt.0) then
		  do i = 1, 7
		    position = position + 1
		    if (validation) then
		      write (cTemp,'(a,i1)') trim(NormalLabelsShort(i+8)), 0
	        headertext(1,position) = cTemp
	        headertext(2,position) = NormalUnits(i+8)
	        headertext(3,1) = ' '
	      else
	        headertext(1,position) = NormalLabels(i+8)
	        headertext(2,position) = 'Mainfire'
	        headertext(3,position) = NormalUnits(i+8)
	      end if  
   		  end do
	  endif
	  do j = 1, numobjl
	    do i = 1, 7
	      position = position + 1
	      if (validation) then
	        if (j.lt.10) then
	          write (cTemp,'(a,i1)') trim(NormalLabelsShort(i+8)), j
	        else
	          write (cTemp,'(a,i2)') trim(NormalLabelsShort(i+8)), j
	        end if
	        headertext(1,position) = cTemp
	        headertext(2,position) = NormalUnits(i+8)
	        headertext(3,1) = ' '
	      else
	        headertext(1,position) = NormalLabels(i+8)
	        headertext(2,position) = objnin(j)
	        headertext(3,position) = NormalUnits(i+8)
	      end if
	    end do
        end do
        
        ! write out header
	  write(15,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
	  write(15,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
        write(15,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)
	  	  
      end if
      end subroutine ssHeaders
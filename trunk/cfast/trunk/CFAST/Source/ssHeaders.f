      subroutine ssHeadersNormal
      
      ! This is the header information for the normal spreadsheet output
      
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "objects1.fi"
      
      ! local variables     
      parameter (maxhead = 1+7*nr+5+7*mxfire)
      character*35 headertext(3,maxhead), cTemp, cRoom, cFire,
     *  Labels(15), LabelsShort(15), LabelUnits(15), toIntString
     
      data Labels / 'Time', 
     *              'Upper Layer Temperature', 
     *              'Lower Layer Temperature',
     *              'Layer Height',
     *              'Upper Layer Volume',
     *              'Pressure',
     *              'Ambient Temp Target Flux',
     *              'Floor Temp Target Flux',
     *              'Plume Entrainment Rate',
     *              'Pyrolysis Rate',
     *              'HRR',
     *              'Flame Height',
     *              'Convective HRR',
     *              'Total Pyrolysate Released',
     *              'Total Trace Species Released' /
      data LabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'VOL_',
     *                   'PRS_', 'ATARG_', 'FTARG_', 'PLUM_', 
     *                   'PYROL_', 'HRR_', 'FHGT_', 'HRR_C_',
     *                   'PYROL_T_', 'TRACE_T_' /
      data LabelUnits / 's', 'C', 'C', 'm', 'm^3', 'Pa', 'W/m^2', 
     *                  'W/m^2', 'kg/s', 'kg/s', 'W', 'm', 'W',
     *                  'kg', 'kg' /

      !  spreadsheet header
      if (validation) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
      else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
      end if
      position = 1
        
      ! Compartment variables
      do j = 1, nm1
        do i = 1, 7
          if (i.ne.2.or.izshaft(j).eq.0) then
            if (i.ne.3.or.izshaft(j).eq.0) then
              position = position + 1
              if (validation) then
                cRoom = toIntString(j)
                headertext(1,position) = trim(LabelsShort(i+1)) // 
     *                                   trim(cRoom)
                headertext(2,position) = LabelUnits(i+1)
                headertext(3,1) = ' '
              else
                headertext(1,position) = Labels(i+1)
                headertext(2,position) = compartmentnames(j)
                headertext(3,position) = LabelUnits(i+1)
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
            write (cTemp,'(a,i1)') trim(LabelsShort(i+8)), 0
            headertext(1,position) = cTemp
            headertext(2,position) = LabelUnits(i+8)
            headertext(3,1) = ' '
          else
            headertext(1,position) = Labels(i+8)
            headertext(2,position) = 'Mainfire'
            headertext(3,position) = LabelUnits(i+8)
          end if  
        end do
      endif
        do j = 1, numobjl
        do i = 1, 7
          position = position + 1
          if (validation) then
            cFire = toIntString(j)
            headertext(1,position) = trim(LabelsShort(i+8))//trim(cFire)
            headertext(2,position) = LabelUnits(i+8)
            headertext(3,1) = ' '
          else
            headertext(1,position) = Labels(i+8)
            headertext(2,position) = objnin(j)
            headertext(3,position) = LabelUnits(i+8)
          end if
        end do
      end do
        
      ! write out header
      write(15,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
      write(15,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
      if (.not.validation)
     *  write(15,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)
      
      end subroutine ssHeadersNormal
      
      subroutine ssHeadersSpecies
      
      ! This is the header information for the spreadsheet output
      
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      
      ! local variables     
      parameter (maxhead = 1+7*nr+5+7*mxfire)
      character*35 headertext(3,maxhead), cTemp, cRoom,
     *  Labels(23), LabelsShort(23), LabelUnits(23), toIntString
      logical tooutput(NS)/.false.,5*.true.,.false.,4*.true./
      logical molfrac(NS) /3*.true.,3*.false.,2*.true.,3*.false./
      integer position
     
      data Labels / 'Time', 
     *              'N2 Upper Layer',
     *              'O2 Upper Layer', 
     *              'CO2 Upper Layer',
     *              'CO Upper Layer',
     *              'HCN Upper Layer',
     *              'HCL Upper Layer',
     *              'Unburned Hydrocarbons Upper Layer',
     *              'H2O Upper Layer',
     *              'Optical Density Upper Layer',
     *              'C-T Product Upper Layer',
     *              'Trace Species Upper Layer', 
     *              'N2 Lower Layer',
     *              'O2 Lower Layer', 
     *              'CO2 Lower Layer',
     *              'CO Lower Layer',
     *              'HCN Lower Layer',
     *              'HCL Lower Layer',
     *              'Unburned Hydrocarbons Lower Layer',
     *              'H2O Lower Layer',
     *              'Optical Density Lower Layer',
     *              'C-T Product Lower Layer',
     *              'Trace Species Lower Layer' / 
      data LabelsShort / 'Time', 'ULN2', 'ULO2_', 'ULCO2_', 'ULCO_', 
     *                   'ULHCN_', 'ULHCL_', 'ULTUHC_', 'ULH2O_', 
     *                   'ULOD_', 'ULCT_', 'ULTS_', 'LLN2', 'LLO2_', 
     *                   'LLCO2_', 'LLCO_', 'LLHCN_', 'LLHCL_', 
     *                   'LLTUHC_', 'LLH2O_', 'LLOD_', 'LLCT_', 'LLTS_'/
      data LabelUnits / 's', 'mol %', 'mol %', 'mol %', 'PPM', 'PPM',
     *                  'PPM', 'mol %', 'mol %', '1/m', 'g-min/m^3', 
     *                  'kg', 'mol %', 'mol %', 'mol %', 'PPM', 'PPM', 
     *                  'PPM', 'mol %', 'mol %', '1/m', 'g-min/m^3', 
     *                  'kg' /

      !  spreadsheet header
      if (validation) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
      else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
      end if
      position = 1
      
      ! Species by compartment, then layer, then species type
      do i = 1, nm1
        do j = upper, lower
          if (j.eq.upper.or.izshaft(j).EQ.0) then
            do lsp = 1, NS
              if(tooutput(lsp)) then
                position = position + 1
                if (validation) then
                  cRoom = toIntString(i)
                  headertext(1,position) = 
     *              trim(LabelsShort((j-1)*11+lsp+1)) // trim(cRoom)
                  headertext(2,position) = LabelUnits((j-1)*11+lsp+1)
                  if (molfrac(lsp)) headertext(2,position) = 'mol frac'
                  if (lsp.eq.9) headertext(2,position) = 'mg/m^3'
                  headertext(3,1) = ' '
                else
                  headertext(1,position) = Labels((j-1)*11+lsp+1)
                  headertext(2,position) = compartmentnames(i)
                  headertext(3,position) = LabelUnits((j-1)*11+lsp+1)
                end if
              end if
            end do
          end if
         end do
      end do
            
      ! write out header
      write(17,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
      write(17,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
      if (.not.validation)
     *  write(17,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)
      
      end subroutine ssHeadersSpecies
      
      subroutine ssHeadersFlux

! This routine spools the headers for the surface temperature and flux results.

! Format

!blank     c1     c1      c1    c1      c1   c1    c1      c1   c1         c2     c2      c2    c2       c2   c2   c2     c2   c2       ....
!time   ceiling	u-wall  l-wall floor  flux  fire surface gas convect   ceiling u-wall  l-wall floor  flux  fire surface gas convect    ....
       

!.....  target number
!.....  gas temp, surface temp, center temp, flux, fire, surface, gas, convect


!.....  sensor number
!.....  compartment name, type, sensor temperature, activated, smoke temperature, smoke velocity


      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"
      
      ! local variables     
      parameter (maxhead = 1+9*nr+8*mxtarg+4*mxdtect)
      character*35 headertext(3,maxhead), cTemp, cType, cDet, cRoom,
     *  Labels(18), LabelsShort(18), LabelUnits(18), toIntString
      integer position 
     
      data Labels / 'Time', 
     *              'Ceiling Temperature',
     *              'Upper Wall Temperature',
     *              'Lower Wall Temperature', 
     *              'Floor Temperature',
     *              'Target Surrounding Gas Temperature',
     *              'Target Surface Temperature',
     *              'Target Center Temperature',
     *              'Target Total Flux',
     *              'Target Convective Flux',
     *              'Target Radiative Flux',
     *              'Target Fire Radiative Flux',
     *              'Target Surface Radiative Flux',
     *              'Target Gas Radiative Flux',
     *              'Sensor Temperature',
     *              'Sensor Activation',
     *              'Sensor Surrounding Gas Temperature',
     *              'Sensor Surrounding Gas Velocity' /
     
      data LabelsShort /'Time', 'CEILT_', 'UWALLT_', 'LWALLT_', 
     *                  'FLOORT_', 'TARGGAST_', 'TARGSURT_', 
     *                  'TARGCENT_', 'TARGFLUXT_', 'TARGFLUXC_', 
     *                  'TARGFLUXR_','TARGFLUXF_', 'TARGFLUXS_',
     *                  'TARGFLUXG_',  'SENST_', 'SENSACT_', 
     *                  'SENSGAST_', 'SENSGASVEL_' /
     
      data LabelUnits / 's', 7*'C', 6*'W', 'C', '1=yes',
     *                  'C', 'm/s' /

      !  spreadsheet header
      if (validation) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
      else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
      end if
      position = 1

      ! Compartment surfaces and the floor target
      do i = 1, nm1
	  do j = 1, 4
	    position = position + 1
	    if (validation) then
	      cRoom = toIntString(i)
            headertext(1,position) = trim(LabelsShort(j+1))//trim(cRoom)
            headertext(2,position) = LabelUnits(j+1)
            headertext(3,position) = ' '
	    else
	      headertext(1,position) = Labels(j+1)
	      headertext(2,position) = compartmentnames(i)
	      headertext(3,position) = LabelUnits(j+1)
	    end if
	  end do
	end do

!	All the additional targets

      do i = 1, nm1
        IF (NTARG.GT.NM1) then
          DO ITARG = 1, NTARG-NM1
            if (IXTARG(TRGROOM,ITARG).EQ.I) then
			  cDet = toIntString(itarg)
			  do j = 1, 9
			    position = position + 1
			    if (validation) then
                  headertext(1,position) = trim(LabelsShort(j+5)) // 
     *                                     trim(cDet)
                  headertext(2,position) = LabelUnits(j+5)
                  if (LabelUnits(j+1).eq.'W') LabelUnits(j+1) = 'kW'
                  headertext(3,position) = ' '
	          else
	            headertext(1,position) = Labels(j+5)
	            headertext(2,position) = 'Target ' // trim(cDet)
	            headertext(3,position) = LabelUnits(j+5)
	          end if
			  end do  
            end if
   30     end do
        end if
      end do

!	Hall flow needs to go here

!	Detectors

	do i = 1, ndtect
		cDet = toIntString(i)
        itype = ixdtect(i,dtype)
		if (itype.eq.smoked) then
		  cType = 'Smoke'
		elseif (itype.eq.heatd) then
	 	  cType = 'Heat'
	  else
	    cType = 'Other'
		endif
	  do j = 1, 4
	    position = position + 1
	    if (validation) then
	      headertext(1,position) = trim(LabelsShort(j+14)) // trim(cDet)
	      headertext(2,position) = LabelUnits(j+14)
	      headertext(3,position) = ' '
	    else
	      headertext(1,position) = Labels(j+14)
	      write (cTemp,'(a,1x,a,1x,a)') trim(cType), 'Sensor', trim(cDet)
	      headertext(2,position) = cTemp
	      headertext(3,position) = LabelUnits(j+14)
	    end if
        end do
      end do
            
      ! write out header
      write(18,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
      write(18,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
      if (.not.validation)
     *  write(18,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)
      
      return
      end subroutine ssHeadersFlux
      
      SUBROUTINE ssHeadersFlow

!	This is the header information for the flow spreadsheet and is called once
!	The logic is identical to SpreadSheetFlow so the output should be parallel

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "vents.fi"

      ! local variables     
      parameter (maxhead = mxvents+2*mxvv+2*mxhvsys+mfan)
      character*35 headertext(3,maxhead), cTemp, cFrom, cTo, cVent,
     *  Labels(11), LabelsShort(11), LabelUnits(11), toIntString
      integer position  
     
      data Labels / 'Time', 
     *              'HVENT Inflow',
     *              'HVENT Outflow',
     *              'HVENT Mixing to Upper Layer', 
     *              'HVENT Mixing to Lower Layer',
     *              'VVENT Inflow',
     *              'VVENT Outflow',
     *              'MVENT Inflow',
     *              'MVENT Outflow',
     *              'MVENT Trace Species Flow',
     *              'MVENT Trace Species Filtered' /
     
      data LabelsShort /'Time', 'H_IN_', 'H_OUT_', 'H_MIXUP_', 
     *                  'H_MIXLOW_', 'V_IN_', 'V_OUT_', 'MV_IN_',
     *                  'MV_OUT_', 'MV_TRACE_', 'MV_FILTERED_' /
     
      data LabelUnits / 9*'kg/s', 2*'kg' /

      !  spreadsheet header
      if (validation) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
      else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
      end if
      position = 1

      !	Do the output by compartments

      do irm = 1, n
        i = irm

C     Natural flow through vertical vents (horizontal flow)

        do j = 1, n
          do k = 1, mxccv
            if (iand(1,ishft(nw(i,j),-k)).ne.0) then
              iijk = ijk(i,j,k)
              if (j.eq.n) then
                cFrom = 'Outside'
              else
                cFrom = toIntString(j)
              end if
              do ih = 1, 4
                if (j.ne.n.or.ih.lt.3) then
                  position = position + 1
                  cVent = toIntString(k)
                  cTo = toIntString(i)
                  headertext(1,position) = Labels(ih+1)
                  if (validation) then
                    write (ctemp,'(6a)') 
     *                trim(LabelsShort(ih+1)),trim(cVent),'_',
     *                trim(cFrom),'-',trim(cTo)
                    headertext(1,position) = ctemp
                    headertext(2,position) = LabelUnits(ih+1)
                    headertext(3,position) = ' '
                  else
                    write (ctemp,'(a,1x,a,1x,3a)') 
     *                    'Vent #',trim(cVent),trim(cFrom),'-',trim(cTo)
                    headertext(2,position) = ctemp
                    headertext(3,position) = LabelUnits(ih+1)
                  end if
                end if
              end do
            end if
          end do
        end do

!	Natural flow through horizontal vents (vertical flow)

        do j = 1, n
          if (nwv(i,j).ne.0.or.nwv(j,i).ne.0) then
            cFrom = toIntString(i)
            if (i.eq.n) cFrom = 'Outside'
            cTo = toIntString(j)
			if (j.eq.n) cTo = 'Outside'
		    do ih = 1,2
		      position = position + 1
		      if (validation) then
		        write (ctemp,'(5a)') 
     *            trim(LabelsShort(ih+5)),'_',trim(cFrom),'-',trim(cTo)
		        headertext(1,position) = cTemp
		        headertext(2,position) = LabelUnits(ih+5)
		        headertext(3,position) = ' '
		      else
		        headertext(1,position) = Labels(ih+5)
		        write (ctemp,'(a,1x,3a)') 
     *                'Vent #',trim(cFrom),'-',trim(cTo)
		        headertext(2,position) = cTemp
		        headertext(3,position) = LabelUnits(ih+5)
		      end if
		    end do
          end if
        end do

!	Mechanical ventilation

        if (nnode.ne.0.and.next.ne.0) then
          do i = 1, next
            ii = hvnode(1,i)
            if (ii.eq.irm) then
              inode = hvnode(2,i)
	        cFrom = toIntString(ii)
              if (ii.eq.n) cnum = 'Outside'
              cTo = toIntString(inode)
              do ih = 1,4
                position = position + 1
                if (validation) then
                  if (ih.le.2) then
                    headertext(1,position) = trim(LabelsShort(ih+7)) 
     *                //'Vent_' // trim(cFrom) // '-' // trim(cTo)
                  else
                    headertext(1,position) = trim(LabelsShort(ih+7)) //
     *               'Fan_' // trim(cTo)
                  end if
                  headertext(2,position) = LabelUnits(ih+7)
                  headertext(3,position) = ' '
                else
                  headertext(1,position) = Labels(ih+7)
                  if (ih.le.2) then
                    headertext(2,position) = 'Vent Connection at Node '
     *                                // trim(cFrom) // '-' // trim(cTo)
                  else
                    headertext(2,position) = 'Fan at Node ' // trim(cTo)
                  end if
                  headertext(3,position) = LabelUnits(ih+7)
                end if
              end do
            end if
          end do
        end if
      end do
            
      ! write out header
      write(16,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
      write(16,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
      if (.not.validation)
     *  write(16,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

	return

	end subroutine ssHeadersFlow

      character*(*) function toIntString(i)
      integer i
      character string*256
      if (i.lt.10) then
        write (string,'(i1)') i
      else if (i.lt.100) then
        write (string,'(i2)') i
      else if (i.lt.1000) then
        write (string,'(i3)') i
      else if (i.lt.10000) then
        write (string,'(i4)') i
      else if (i.lt.100000) then
        write (string,'(i5)') i
      else if (i.lt.1000000) then
        write (string,'(i6)') i
      else
        string = 'error'
      end if
      toIntString = trim(string)
      return
      end function to IntString
      

      SUBROUTINE SpreadSheetNormal (time, errorcode)

! This routine writes to the {project}.n.csv file, the compartment information and the fires

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "iofiles77.fi"

	parameter (maxhead = 1+7*nr+5+7*mxfire)
	character*16 headline(3,maxhead)
	character*16 compartmentlabel(7)
	character*16 firelabel(7)
	double precision time, outarray(maxhead)
	logical firstc
	integer position

	data compartmentlabel/'Upper Layer Temp','Lower Layer Temp',
     . 'Layer Height','Volume','Pressure','Ambient Target',
     . 'Floor Target'/
	data firelabel/'Plume','Pyrolysis','Fire Size','Flame Height',
     . 'Convective flow','Pyrolosate','Total Trace'/
	data firstc/.true./
	save firstc

! Headers

!! Note: this more properly belongs in a separate routine as is done with the flow and species spreadsheets, but this one
!! is sufficiently short to make that more work than this simple approach

	If (firstc) then

	headline(1,1)= 'NORMAL'
	headline(2,1) = 'Time'
	headline(3,1) = ' '

	do 10 j = 1, nm1
	do 10 i = 1, 7
	headline(2,i+1+(j-1)*7) = compartmentlabel(i)
	headline(3,i+1+(j-1)*7) = ' '
   10	headline(1,i+1+(j-1)*7) = compartmentnames(j)

	nfires = 0
	if (lfbo.gt.0) then
		 do 20 i = 1, 7
		 headline(3,i+7*nm1+1) = firelabel(i)
		 headline(2,i+7*nm1+1) = compartmentnames(froom(0))
   20		 headline(1,i+7*nm1+1) = 'Mainfire'
		 nfires = 1
	endif

	do 30 j = 1, numobjl
	do 30 i = 1, 7
	headline(3,i+7*nm1+1+7*nfires+7*(j-1)) = firelabel(i)
	headline(2,i+7*nm1+1+7*nfires+7*(j-1)) = compartmentnames(froom(j))
   30 headline(1,i+7*nm1+1+7*nfires+7*(j-1)) = objnin(j)

	nfires = nfires + numobjl

	nheadings = 1+7*nm1+7*nfires
	if(nheadings.gt.maxhead.or.nheadings.gt.16000) then
		 errorcode = 211
		 return
	endif

	write(15,"(1024(a,','))") (trim(headline(1,i)),i=1,nheadings)
	write(15,"(1024(a,','))") (trim(headline(2,i)),i=1,nheadings)
	write(15,"(1024(a,','))") (trim(headline(3,i)),i=1,nheadings)
	firstc = .false.

	endif

! End of header information
 
	position = 0

      CALL SSaddtolist (position,TIME,outarray)

! Compartment information

        DO 100 I = 1, NM1
        ITARG = NTARG - NM1 + I
        IZZVOL = ZZVOL(I,UPPER)/VR(I)*100.D0+0.5D0
        CALL SSaddtolist (position,ZZTEMP(I,UPPER)-273.15,outarray)
        CALL SSaddtolist (position,ZZTEMP(I,LOWER)-273.15,outarray)
        CALL SSaddtolist (position,ZZHLAY(I,LOWER),outarray)
        CALL SSaddtolist (position,ZZVOL(I,UPPER),outarray)
        CALL SSaddtolist (position,ZZRELP(I) - PAMB(I),outarray)
        CALL SSaddtolist (position,ONTARGET(I),outarray)
        CALL SSaddtolist (position,XXTARG(TRGNFLUXF,ITARG),outarray)
  100 CONTINUE

! Fires

      XX0 = 0.0D0
      IF (LFMAX.GT.0.AND.LFBT.GT.0.AND.LFBO.GT.0) THEN
        CALL FLAMHGT (FROOM(0),FQF(0),FAREA(0),FHEIGHT)
        CALL SSaddtolist (position,FEMS(0),outarray)
        CALL SSaddtolist (position,FEMP(0),outarray)
        CALL SSaddtolist (position,FQF(0),outarray)
        CALL SSaddtolist (position,FHEIGHT,outarray)
        CALL SSaddtolist (position,FQFC(0),outarray)
        CALL SSaddtolist (position,objmaspy(0),outarray)
        CALL SSaddtolist (position,radio(0),outarray)
      END IF

      IF (NUMOBJL.NE.0) THEN
        DO 200 I = 1, NUMOBJL
          CALL FLAMHGT (FROOM(I),FQF(I),FAREA(I),FHEIGHT)
          CALL SSaddtolist (position,FEMS(I),outarray)
          CALL SSaddtolist (position,FEMP(I),outarray)
          CALL SSaddtolist (position,FQF(I),outarray)
          CALL SSaddtolist (position,FHEIGHT,outarray)
          CALL SSaddtolist (position,FQFC(I),outarray)
          CALL SSaddtolist (position,objmaspy(i),outarray)
          CALL SSaddtolist (position,radio(i),outarray)          
  200   CONTINUE
      END IF

      CALL SSprintresults (15, position, outarray)

      RETURN
      END

      SUBROUTINE SSaddtolist (ic, VALUE, array)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "cfin.fi"

	double precision array(*), value

      IC = IC + 1
!	We are imposing an arbitrary limit of 512 columns
	if (ic.gt.32000) return
      array(IC) = VALUE
      RETURN
      
      ENTRY SSprintresults (iounit,ic,array)
      
      WRITE (iounit,"(1024(e12.6,','))" ) (array(i),i=1,ic)
      RETURN
      END

      SUBROUTINE SpreadSheetFlow (Time, errorcode)

!	Routine to output the flow data to the flow spreadsheet {project}.f.csv

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

	parameter (maxoutput = 512)
	double precision time, outarray(maxoutput),sum1,sum2,sum3,sum4,
     . sum5,sum6, flow(6), sumin, sumout
	logical firstc/.true./
	integer position, errorcode
	save firstc

	if (firstc) then
		 call SpreadSheetFlowHeader
		 firstc = .false.
	endif
		 
      XX0 = 0.0D0
	position = 0

!	First the time

      CALL SSaddtolist (position,TIME,outarray)

      DO 70 IRM = 1, N

!	Next the horizontal flow through vertical vents

      DO 20 J = 1, N
          DO 10 K = 1, mxccv
            I = IRM
            IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
               IIJK = IJK(I,J,K)
               IF (I.LT.J)THEN
                 SUM1 = SS2(IIJK) + SA2(IIJK)
                 SUM2 = SS1(IIJK) + SA1(IIJK)
                 SUM3 = AA2(IIJK) + AS2(IIJK)
                 SUM4 = AA1(IIJK) + AS1(IIJK)
               ELSE
                 SUM1 = SS1(IIJK) + SA1(IIJK)
                 SUM2 = SS2(IIJK) + SA2(IIJK)
                 SUM3 = AA1(IIJK) + AS1(IIJK)
                 SUM4 = AA2(IIJK) + AS2(IIJK)
              ENDIF
              IF (J.EQ.N) THEN
				  sumin = sum1 + sum3
				  sumout = sum2 + sum4
                 CALL SSAddtolist (position,SUMin,outarray)
                 CALL SSAddtolist (position,SUMout,outarray)
              ELSE
                 IF (I.LT.J)THEN
                    SUM5 = SAU2(IIJK)
                    SUM6 = ASL2(IIJK)
                 ELSE
                    SUM5 = SAU1(IIJK)
                    SUM6 = ASL1(IIJK)
                 ENDIF
!	We show only net flow in the spreadsheets
				  sumin = sum1 + sum3
				  sumout = sum2 + sum4
                 CALL SSAddtolist (position,SUMin,outarray)
                 CALL SSAddtolist (position,SUMout,outarray)
                 CALL SSAddtolist (position,SUM5,outarray)
                 CALL SSAddtolist (position,SUM6,outarray)
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE

!	Next natural flow through horizontal vents (vertical flow)

        DO 40 J = 1, N
          IF (NWV(I,J).NE.0.OR.NWV(J,I).NE.0) THEN
            DO 30 II = 1, 6
              FLOW(II) = XX0
   30       CONTINUE
            IF (VMFLO(J,I,UPPER).GE.XX0) FLOW(1) = VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,UPPER).LT.XX0) FLOW(2) = -VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,LOWER).GE.XX0) FLOW(3) = VMFLO(J,I,LOWER)
            IF (VMFLO(J,I,LOWER).LT.XX0) FLOW(4) = -VMFLO(J,I,LOWER)
!	We show only net flow in the spreadsheets
		    sumin = flow(1) + flow(3)
			sumout = flow(2) + flow(4)
            CALL SSAddtolist (position,sumin,outarray)
            CALL SSAddtolist (position,sumout,outarray)
          END IF
   40   CONTINUE

!	Finally, mechanical ventilation

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
              DO 50 III = 1, 6
                FLOW(III) = XX0
   50         CONTINUE
              IF (HVEFLO(UPPER,I).GE.XX0) FLOW(1) = HVEFLO(UPPER,I)
              IF (HVEFLO(UPPER,I).LT.XX0) FLOW(2) = -HVEFLO(UPPER,I)
              IF (HVEFLO(LOWER,I).GE.XX0) FLOW(3) = HVEFLO(LOWER,I)
              IF (HVEFLO(LOWER,I).LT.XX0) FLOW(4) = -HVEFLO(LOWER,I)
			  sumin = flow(1) + flow(3)
			  sumout = flow(2) + flow(4)
              flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
              flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))			  
			  call SSAddtolist (position, sumin, outarray)
			  call SSAddtolist (position, sumout, outarray)
			  call SSAddtolist (position, flow(5), outarray)
			  call SSAddtolist (position, flow(6), outarray)
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

	call ssprintresults(16, position, outarray)
	return

	END

      SUBROUTINE SpreadSheetFlowHeader

!	This is the header information for the flow spreadsheet and is called once
!	The logic is identical to SpreadSheetFlow so the output should be parallel

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

	parameter (maxoutput = 512)
	character*30 heading(maxoutput)
      CHARACTER ostring*30, CNUM*3, CNUM2*3
	integer position

	do 5 i = 1, maxoutput
    5 heading(i) = ' '

	position = 1

	ostring = 'Time'
	call SSFlowAtoH (position, maxoutput, heading, ostring)

!	Do the output by compartments

      DO 70 IRM = 1, N

C     Natural flow through vertical vents (horizontal flow)

      DO 20 J = 1, N
          DO 10 K = 1, mxccv
            I = IRM
            IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
              IIJK = IJK(I,J,K)
              IF (J.EQ.N) THEN
                 WRITE (CNUM,'(I2)') I
                 ostring = 'Inflow from outside to'//CNUM
				  call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Outflow to outside from'//CNUM
	           call SSFlowAtoH (position, maxoutput, heading, ostring)
              ELSE
                 WRITE (CNUM,'(I2)') I
                 WRITE (CNUM2,'(I2)') J
!	We show only net flow in the spreadsheets
                 ostring = 'Inflow from '//cnum2//' to'//CNUM
				  call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Outflow to '//cnum2//' from'//CNUM
	           call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Mixing to Upper'//CNUM//' ('//CNUM2//')'
			     call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Mixing to Lower'//CNUM//' ('//CNUM2//')'
			     call SSFlowAtoH (position, maxoutput, heading, ostring)
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE

!	Natural flow through horizontal vents (vertical flow)

        DO 40 J = 1, N
          IF (NWV(I,J).NE.0.OR.NWV(J,I).NE.0) THEN
            WRITE (CNUM,'(I2)') I
            WRITE (CNUM2,'(I2)') J
            IF (J.EQ.N) CNUM2 = 'Out'
			if (i.eq.n) cnum = 'Out'
!	We show only net flow in the spreadsheets
		   ostring = 'V Outflow from '//cnum//' to '//cnum2
			call SSFlowAtoH (position, maxoutput, heading, ostring)
		   ostring = 'V Inflow from '//cnum//' to '//cnum2
			call SSFlowAtoH (position, maxoutput, heading, ostring)
          END IF
   40   CONTINUE

!	Mechanical ventilation

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
	        WRITE (CNUM,'(I2)') II
              IF (II.EQ.N) CNUM = 'Out'
              WRITE (CNUM2,'(I2)') INODE
			  ostring = "MV Inflow to "//cnum
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "MV Outflow from "//cnum
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "Trace Species through node "//cnum2
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "Trace captured at node "//cnum2
			  call SSFlowAtoH (position, maxoutput, heading, ostring)			  
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

	write(16,80) 'FLOW'
	write(16,80) (trim(heading(i)),i=1,position)
	write(16,80) ' '
   80	format(512a)
	return

	end

	subroutine SSFlowAtoH (position, maxoutput, heading, ostring)

	character heading*30(maxoutput), ostring*30
	integer position, length

	if (position.gt.maxoutput) return

	position = position + 1
	heading(position) = trim (ostring) // ','
	return
	
	END

	subroutine SpreadSheetFlux (Time, errorcode)
	
!     Output the temperatures and fluxes on surfaces and targets at the current time

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"

	parameter (maxoutput=512)
	double precision outarray(maxoutput), time, xiroom, zdetect,
     . tjet, vel, tlink, xact, rtotal, ftotal, wtotal, gtotal,
     . ctotal, tttemp, tlay
      INTEGER IWPTR(4), errocode
      EXTERNAL LENGTH
      DATA IWPTR /1, 3, 4, 2/
      CHARACTER CTYPE*5, cact*3
	logical firstc/.true./
	save firstc

	if (firstc) then
		 call SpreadSheetFluxHeader
		 firstc = .false.
	endif

      XX0 = 0.0D0
      X100 = 100.0D0

	position = 0

!	First the time

      CALL SSaddtolist (position,TIME,outarray)

!     First the temperature and fluxes for each compartment

      DO 10 I=1,NM1
        ITARG = NTARG-NM1+I
        RTOTAL = XXTARG(TRGTFLUXF,ITARG)
        FTOTAL = QTFFLUX(ITARG,1)
        WTOTAL = QTWFLUX(ITARG,1)
        GTOTAL = QTGFLUX(ITARG,1)
        CTOTAL = QTCFLUX(ITARG,1)
	  do 11 iw = 1, 4
   11   call SSaddtolist (position,twj(1,i,iwptr(iw))-273.15,outarray)
! a target 0 is the floor
	  call SSaddtolist (position,rtotal,outarray)
	  call SSaddtolist (position,FTOTAL,outarray)
	  call SSaddtolist (position,WTOTAL,outarray)
	  call SSaddtolist (position,GTOTAL,outarray)
	  call SSaddtolist (position,ctotal,outarray)
   10 CONTINUE

! Now do the additional targets if defined
	do 40 i = 1, nm1
        IF (NTARG.GT.NM1) THEN
          DO 20 ITARG = 1, NTARG-NM1
            IF (IXTARG(TRGROOM,ITARG).EQ.I) THEN
			TTTEMP = XXTARG(TRGTEMPF,ITARG)
			RTOTAL = XXTARG(TRGTFLUXF,ITARG)
              FTOTAL = QTFFLUX(ITARG,1)
              WTOTAL = QTWFLUX(ITARG,1)
              GTOTAL = QTGFLUX(ITARG,1)
              CTOTAL = QTCFLUX(ITARG,1)
			call SSaddtolist (position, tttemp-273.15, outarray)
              call SSaddtolist (position, rtotal, outarray)
              call SSaddtolist (position, ftotal, outarray)
              call SSaddtolist (position, wtotal, outarray)
              call SSaddtolist (position, gtotal, outarray)
              call SSaddtolist (position, ctotal, outarray)
            END IF
   20     CONTINUE
        END IF
   40 continue

!     Hallways

c      DO 40 I = 1, NM1
c        IF(IZHALL(I,IHROOM).EQ.0)GO TO 40
c        TSTART = ZZHALL(I,IHTIME0)
c        VEL = ZZHALL(I,IHVEL)
c        DEPTH = ZZHALL(I,IHDEPTH)
c        DIST = ZZHALL(I,IHDIST)
c        IF(DIST.GT.ZZHALL(I,IHMAXLEN))DIST = ZZHALL(I,IHMAXLEN)
c        WRITE(IOFILO,5050)I,TSTART,VEL,DEPTH,DIST
c   40 CONTINUE

!    Detectors (including sprinklers)

      CJETMIN = 0.10D0
      DO 60 I = 1, NDTECT
		 IROOM = IXDTECT(I,DROOM)
		 xiroom = iroom

		 ZDETECT = XDTECT(I,DZLOC)
		 IF(ZDETECT.GT.ZZHLAY(IROOM,LOWER))THEN
			  TLAY = ZZTEMP(IROOM,UPPER)
		 ELSE	
			  TLAY = ZZTEMP(IROOM,LOWER)
		 ENDIF

		 xact = IXDTECT(I,DACT)
		 TJET = MAX(XDTECT(I,DTJET),TLAY)
		 VEL = MAX(XDTECT(I,DVEL),CJETMIN)
		 TLINK =  XDTECT(I,DTEMP)
		 call SSaddtolist(position, xiroom, outarray)
		 call SSaddtolist(position, tlink-273.15, outarray)
		 call SSaddtolist(position, xact, outarray)
		 call SSaddtolist(position, tjet-273.15, outarray)
		 call SSaddtolist(position, vel, outarray)
   60 CONTINUE

	call SSPrintResults (18, position, outarray)

      RETURN

 5050 FORMAT(4x,I2,7x,1PG10.3,5x,1PG10.3,3x,1PG10.3,5x,1PG10.3)
      END

	subroutine SpreadSheetFluxHeader

! This routine spools the headers for the surface temperature and flux results.

! Format

!blank     c1     c1      c1    c1      c1   c1    c1      c1   c1         c2     c2      c2    c2       c2   c2   c2     c2   c2       ....
!time   ceiling	u-wall  l-wall floor  flux  fire surface gas convect   ceiling u-wall  l-wall floor  flux  fire surface gas convect    ....
       

!.....  target number
!.....  compartment name, flux, fire, surface, gas, convect


!.....  sensor number
!.....  compartment name, type, sensor temperature, activated, smoke temperature, smoke velocity


      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"

      parameter (maxoutput=512)
	integer position
      character heading*30(maxoutput,2), clabels*30(9), tlables*30(6),
     . tnum*2, ostring*30, slables*30(5), xtype*5, blank

      data clabels/"ceiling","upper wall","lower wall","floor",
     . "flux to target","total fire rad.","surface rad.","gas rad.",
     . "convective flux"/, blank/''/

      data tlables/"compartment","total flux","fire flux",
     . "surface flux","gas flux","convective flux"/

	data slables/"compartment","sensor temp.", "activated",
     . "smoke temp.","smoke velocity"/

	heading(1,1) = 'WALL'
	heading(1,2) = 'Time '
	position = 1

!	Compartment surfaces and the floor target

      do 10 i = 1, nm1
	do 20 j = 1, 9
	position = position + 1
	heading(position,1) = compartmentnames(i)
	heading(position,2) = clabels(j)
   20 continue
   10 continue

!	All the additional targets

      do 40 i = 1, nm1
      IF (NTARG.GT.NM1) THEN
        DO 30 ITARG = 1, NTARG-NM1
          IF (IXTARG(TRGROOM,ITARG).EQ.I) THEN
			  write(tnum,"(i2)") itarg
			  do 31 j = 1, 6
			  heading(position+j,1) = "Target "//tnum
   31			  heading(position+j,2) = tlables(j)
			  heading(position+1,2) = compartmentnames(i)
			  position = position + 6
          END IF
   30   CONTINUE
      END IF
   40 continue

!	Hall flow needs to go here

!	Detectors

	do 50 i = 1, ndtect
		 IROOM = IXDTECT(I,DROOM)
          ITYPE = IXDTECT(I,DTYPE)
		 write(tnum,"(i2)") i
		 if (itype.eq.smoked) then
			  xtype = 'Smoke'
		 elseif (itype.eq.heatd) then
		 	  xtype = 'Heat'
		 else
			  xtype = 'Other'
		 endif
		 do 51 j = 1, 5
		 heading(position+j,1) = 
     .         "Sensor "//tnum//' is a '//xtype//' detector'
   51		 heading(position+j,2) = slables(j)
		 heading(position+1,2) = compartmentnames(iroom)
		 position = position + 5
   50 continue

   	write(18,"(512(a,','))") (trim(heading(j,1)),j=1,position)
	write(18,"(512(a,','))") (trim(heading(j,2)),j=1,position)
	write(18,"(512(a,','))") (blank,j=1,position)

      return
      end

      SUBROUTINE SpreadSheetSpecies (time, errorcode)

!	Write out the species to the spread sheet file

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"

	parameter (maxhead = 1+18*nr)
	character*16 heading(3,maxhead)
	double precision time, outarray(maxhead)
	integer position

      CHARACTER STYPE(NS)*10, LNAMES(2)*5
	integer layer
      DATA LNAMES /'Upper', 'Lower'/
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O',
     +    'OD', 'CT', 'TS'/
      logical tooutput(11)/.false.,5*.true.,.false.,4*.true./,
     .  firstc/.true./
	
	save outarray, firstc

! If there are no species, then don't do the output
	if (nlspct.eq.0) return

! Set up the headings
	if (firstc) then
! Count species	nls is then the offset
		 nls = 0
		 do 9 i = 1, ns
			  if(tooutput(i)) nls = nls + 1
    9		 continue
		 maxsize = 2 * nls * nm1 + 1
		 if (maxsize.gt.maxhead) then
! We can only output to the maximum array size; this is not deemed to be a fatal error!
			  write(logerr,11) maxsize
			  maxsize = min (maxsize, maxhead)
		 endif

		 heading(1,1)='SPECIES'
		 heading(2,1) = 'Time'
		 heading(3,1) = ' '

		 do 10 i = 1, nm1
		 do 10 j = UPPER, LOWER
		 ins = 0
		 do 10 lsp = 1, NS
		 if(tooutput(lsp)) then
			  ins = ins + 1
			  IJKs = (ins-1)*2+(i-1)*2*nls+j+1
			  heading(3,ijks) = stype(lsp)
			  heading(2,ijks) = lnames(j)
   			  heading(1,ijks) = compartmentnames(i)
		 endif
   10		 continue
		 write(17,"(1024(a,','))") (trim(heading(1,i)),i=1,maxsize)
		 write(17,"(1024(a,','))") (trim(heading(2,i)),i=1,maxsize)
		 write(17,"(1024(a,','))") (trim(heading(3,i)),i=1,maxsize)
		 firstc = .false.
	endif

!	From now on, just the data, please

	position = 0

      CALL SSaddtolist (position,TIME,outarray)

      DO 70 I = 1, NM1
        DO 50 LSP = 1, NS
	    DO 80 LAYER = UPPER, LOWER
            IF (tooutput(LSP)) THEN
	        CALL SSaddtolist (position,TOXICT(I,LAYER,LSP),outarray)
! We can only output to the maximum array size; this is not deemed to be a fatal error!
			  if (position.ge.maxhead) go to 90
            END IF
   80	    CONTINUE
   50   CONTINUE
   70 CONTINUE

   90	call SSprintresults (17,position, outarray)

      RETURN

   11	format('Exceeded size of output files in species spread sheet')
      END

      integer function rev_outputspreadsheet
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_outputspreadsheet = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_outputspreadsheet
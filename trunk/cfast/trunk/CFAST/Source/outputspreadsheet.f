      SUBROUTINE SpreadSheetNormal (time, errorcode)

! This routine writes to the {project}.n.csv file, the compartment information and the fires

      use iofiles
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"
      include "objects1.fi"

	parameter (maxhead = 1+7*nr+5+7*mxfire)
	character*16 headline(3,maxhead)
	character*16 compartmentlabel(7)
	character*16 firelabel(7)
	double precision time, outarray(maxhead)
	logical firstc
	integer position, errorcode

	data compartmentlabel/'Upper Layer Temp','Lower Layer Temp',
     . 'Layer Height','Volume','Pressure','Ambient Target',
     . 'Floor Target'/
	data firelabel/'Plume','Pyrolysis','Fire Size','Flame Height',
     . 'Convective flow','Pyrolosate','Total Trace'/
	data firstc/.true./
	save firstc

! Headers
	if (firstc) then
        call ssHeadersNormal
	  firstc = .false.
	endif

	position = 0
      CALL SSaddtolist (position,TIME,outarray)

! Compartment information

        DO 100 I = 1, NM1
        ITARG = NTARG - NM1 + I
        IZZVOL = ZZVOL(I,UPPER)/VR(I)*100.D0+0.5D0
        CALL SSaddtolist (position,ZZTEMP(I,UPPER)-273.15,outarray)
        if (izshaft(i).eq.0) then
          CALL SSaddtolist (position,ZZTEMP(I,LOWER)-273.15,outarray)
          CALL SSaddtolist (position,ZZHLAY(I,LOWER),outarray)
        end if
        CALL SSaddtolist (position,ZZVOL(I,UPPER),outarray)
        CALL SSaddtolist (position,ZZRELP(I) - PAMB(I),outarray)
        CALL SSaddtolist (position,ONTARGET(I),outarray)
        CALL SSaddtolist (position,XXTARG(TRGNFLUXF,ITARG),outarray)
  100 CONTINUE

! Fires

      XX0 = 0.0D0
      IF (LFMAX.GT.0.AND.LFBT.GT.0.AND.LFBO.GT.0) THEN
        CALL FLAMHGT (FQF(0),FAREA(0),FHEIGHT)
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
          CALL FLAMHGT (FQF(I),FAREA(I),FHEIGHT)
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

      subroutine SSaddtolist (ic, valu, array)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "cfin.fi"

	double precision array(*), valu
	integer ic

      IC = IC + 1
!	We are imposing an arbitrary limit of 32000 columns
	if (ic.gt.32000) return
      array(IC) = valu
      return
      
      entry SSprintresults (iounit,ic,array)
      
      write (iounit,"(1024(e12.6,','))" ) (array(i),i=1,ic)
      return
      end subroutine SSaddtolist

      SUBROUTINE SpreadSheetFlow (Time, errorcode)

!	Routine to output the flow data to the flow spreadsheet {project}.f.csv

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "vents.fi"

	parameter (maxoutput = 512)
	double precision time, outarray(maxoutput),sum1,sum2,sum3,sum4,
     . sum5,sum6, flow(6), sumin, sumout
	logical firstc/.true./
	integer position, errorcode
	save firstc

	if (firstc) then
		 call ssHeadersFlow
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
     . ctotal, tttemp, tctemp, tlay
      INTEGER IWPTR(4), errorcode, position
      EXTERNAL LENGTH
      DATA IWPTR /1, 3, 4, 2/
      CHARACTER CTYPE*5, cact*3
	logical firstc/.true./
	save firstc

	if (firstc) then
		 call ssHeadersFlux
		 firstc = .false.
	endif

      XX0 = 0.0D0
      X100 = 100.0D0

	position = 0

!	First the time

      CALL SSaddtolist (position,TIME,outarray)

!     First the temperatures for each compartment

      do i=1,nm1
        do iw = 1, 4
          call ssaddtolist (position,twj(1,i,iwptr(iw))-273.15,outarray)
        end do
      end do

! Now do the additional targets if defined
	do 40 i = 1, nm1
        IF (NTARG.GT.NM1) THEN
          DO 20 ITARG = 1, NTARG-NM1
            IF (IXTARG(TRGROOM,ITARG).EQ.I) THEN
              TGTEMP = TGTARG(ITARG)
              if (stime.ge.800.) then
                continue
              end if
              if (IXTARG(TRGEQ,ITARG).eq.CYLPDE) then
                tttemp = xxtarg(trgtempb,itarg)
                ITCTEMP = TRGTEMPF+ xxtarg(trginterior,itarg)*
     +              (TRGTEMPB-TRGTEMPF)
                tctemp = xxtarg(itctemp,itarg)
              else
			    TTTEMP = XXTARG(TRGTEMPF,ITARG)
                ITCTEMP = (TRGTEMPF+TRGTEMPB)/2
                TCTEMP = XXTARG(ITCTEMP,ITARG)
              end if
              IF (IXTARG(TRGEQ,ITARG).EQ.ODE) TCTEMP = TTTEMP
              IF (IXTARG(TRGMETH,ITARG).EQ.STEADY) TCTEMP = TTTEMP
			  if (validate) then
                TOTAL = GTFLUX(ITARG,1) /1000.d0
                FTOTAL = GTFLUX(ITARG,2) /1000.d0
                WTOTAL = GTFLUX(ITARG,3) /1000.d0
                GTOTAL = GTFLUX(ITARG,4) /1000.d0
                CTOTAL = GTFLUX(itarg,5) /1000.d0
                RTOTAL = TOTAL - CTOTAL
              else
                TOTAL = XXTARG(TRGTFLUXF,ITARG)
                FTOTAL = QTFFLUX(ITARG,1)
                WTOTAL = QTWFLUX(ITARG,1)
                GTOTAL = QTGFLUX(ITARG,1)
                CTOTAL = QTCFLUX(ITARG,1)
                RTOTAL = TOTAL - CTOTAL
              end if
              call SSaddtolist (position, tgtemp-273.15, outarray)
			  call SSaddtolist (position, tttemp-273.15, outarray)
			  call SSaddtolist (position, tctemp-273.15, outarray)
              call SSaddtolist (position, total, outarray)
              call SSaddtolist (position, ctotal, outarray)
              call SSaddtolist (position, rtotal, outarray)
              call SSaddtolist (position, ftotal, outarray)
              call SSaddtolist (position, wtotal, outarray)
              call SSaddtolist (position, gtotal, outarray)
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

      cjetmin = 0.10d0
      do 60 i = 1, ndtect
		 iroom = ixdtect(i,droom)
		 zdetect = xdtect(i,dzloc)
		 if(zdetect.gt.zzhlay(iroom,lower))then
			  tlay = zztemp(iroom,upper)
		 else	
			  tlay = zztemp(iroom,lower)
		 endif
		 xact = ixdtect(i,dact)
		 tjet = max(xdtect(i,dtjet),tlay)
		 vel = max(xdtect(i,dvel),cjetmin)
		 tlink =  xdtect(i,dtemp)
		 call ssaddtolist(position, tlink-273.15, outarray)
		 call ssaddtolist(position, xact, outarray)
		 call ssaddtolist(position, tjet-273.15, outarray)
		 call ssaddtolist(position, vel, outarray)
   60 continue

	call SSPrintResults (18, position, outarray)

      RETURN

 5050 FORMAT(4x,I2,7x,1PG10.3,5x,1PG10.3,3x,1PG10.3,5x,1PG10.3)
      END
	
      SUBROUTINE SpreadSheetSpecies (time, errorcode)

!	Write out the species to the spread sheet file

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"

	parameter (maxhead = 1+22*nr)
	character*16 heading(3,maxhead)
	double precision time, outarray(maxhead)
	integer position, errorcode

	integer layer
      logical tooutput(NS)/.false.,5*.true.,.false.,4*.true. /,
     *        firstc/.true./
      logical molfrac(NS) /3*.true.,3*.false.,2*.true.,3*.false./
	
	save outarray, firstc

! If there are no species, then don't do the output
	if (nlspct.eq.0) return

! Set up the headings
	if (firstc) then
	  call ssHeadersSpecies
		firstc = .false.
	endif

!	From now on, just the data, please

	position = 0

      CALL SSaddtolist (position,TIME,outarray)

      DO 70 I = 1, NM1
        DO 50 LAYER = UPPER, LOWER
	    DO 80 LSP = 1, NS
	      if (layer.eq.upper.or.IZSHAFT(I).EQ.0) then
              IF (tooutput(LSP)) THEN
                ssvalue = TOXICT(I,LAYER,LSP)
                if (validate.and.molfrac(LSP))ssvalue = ssvalue*0.01D0
                if (validate.and.lsp.eq.9) ssvalue = ssvalue *261.959
	          CALL SSaddtolist (position,ssvalue,outarray)
! We can only output to the maximum array size; this is not deemed to be a fatal error!
			    if (position.ge.maxhead) go to 90
              END IF
            end if
   80	    CONTINUE
   50   CONTINUE
   70 CONTINUE

   90	call SSprintresults (17,position, outarray)

      RETURN

  110	format('Exceeded size of output files in species spread sheet')
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
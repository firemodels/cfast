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

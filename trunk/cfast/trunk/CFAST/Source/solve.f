      SUBROUTINE SOLVE(TSTOP,IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SOLVE
C
C     Source File: SOLVE.SOR
C
C     Functional Class:
C
C     Description:
C
C     Arguments: TSTOP   The final time to which CFAST should run to
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        April 10, 1992 change offsets to those shown below
C        Modified by GPF 6/14/92
C             Defined error tolerance arrays used by HVAC.  Changed
C             initial solution to speed up calculation at start
C        Modified by GPF and PAR
C             Added code in support of the reduced Jacobian option.
C        Modified 10/14/93 by GPF, added detection/suppression
C        Modified 10/10/94 by GPF, added target flux/temperature calculation option
C        Modified: 4/26/1995 gpf:
C                  added argument to TARGET, added calls to TRHEAT to update explicit
C                  and implicit TARGET temperatures.  Defined error tolerances for
C                  implicit targets.
C        Modified: 6/30/1995 gpf:
C                  added surface wall temperatures to dassl debug print.
C                  added code to implement dassl's solution of oxygen
C        Modified: 7/13/1995 gpf:
C                  added calls to SETDERV to define the how RESID is being
C                  called (for use by the Jacobian speedup option)
C        Modified: 8/15/1995 par:
C              modified detection/suppression and added to it object
C                  ignition.
C        Modified: 9/5/1995 at 10:27 by PAR:
C                  Added support for IERROR and returning stops to main
C        Modified: 1/24/1996 at 14:36 by RWP:
C                  New support for ADUMP keyword
C        Modified: 2/5/96 by GPF:
C                  changed calls to SETDERV
C        Modified: 7/22/96 by GPF:
C                  Set and unset UPDATEHALL flag around calls to
C                  resid (the resid call that occurs after the call
C                  to DASSL).
C        Modified:  4/28/1997 by gpf
C                   added DTCHECK command to prevent CFAST from hanging.
C                   If III consecutive time steps are less than XXX then
C                   SOLVE sigals that CFAST is hanging.  Format of command
C                   is
C                   DTCHECK XXX III
C                   Default values are XXX=1.0D-09 and III=100
C        MODIFIED: 10/9/97 by GPF:
C                return sensor number instead of "1" in IFDTECT
C                if a sensor has activated.
C        Modified: 10/20/97 by gpf:
C                tell dassl about discontinuities using ZZDISC array
C                defined in DATACOPY .
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
!        Modifies 1/25/07 by wwj to include a calculation of the total pyrolosate
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

C     Offset in the following context is the beginning of the vector for
C     that particular variable minus one.  Thus, the actual pressure array
C     goes from NOFP+1 to NOFP+nm1.  The total number of equations to be
C     considered is NEQUALS, and is the last element in the last vector.
C     Each physical interface routine is responsible for the COUNT of the
C     number of elements in the vector for which it is resonsible.

C     This set of parameters is set by NPUTP and is kept in the environment
C     common block CFAST.INC.  To index a variable, the list is something
C     like (for temperature in this case)

C     NOFTU+1, NOFTU+NM1

C     The structure of the solver array is

C     NOFP = offset for the main pressure; the array of base pressures for each compartment
C     NOFPMV = offset for HVAC node pressuers
C     NOFTMV = offset for HVAC branch temperatures
C     NOFTU = upper layer temperature
C     NOFVU = upper layer volume
C     NOFTL = lower layer temperature
C     NOFWT = wall surface temperatures (equivalent to the number of profiles)
C     NOFPRD = species
C     NOFHCL = surface deposition of hydrogen chloride
C     NOFSMKW = surface deposition of soot
C     NOFSMK = gas phase agglomeration of soot
C     NEQUALS = last element in the array.

C     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

C     An important note - solve sets the last variable to be solved to NOFPRD
C     which is the beginning of the species (-1) and the end of the array which
C     is presently used by DASSL. The important point is that NODES is set to
C     NOFPRD which is the equivalent to NOFWT+NWALLS

C     update history

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "params.fi"
      include "cenviro.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "dervs.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "cfin.fi"
      include "iofiles77.fi"
      include "vents.fi"
      include "smkview.fi"

      PARAMETER (MAXORD = 5)
      PARAMETER (LRW = 40+(MAXORD+4)*MAXEQ+MAXEQ**2,LIW = 20+MAXEQ)

      DIMENSION RWORK(LRW), IWORK(LIW), INFO(15), IPAR(3), RPAR(1)
      DIMENSION INFO2(15)
      DIMENSION PPRIME(MAXTEQ)
      DIMENSION PDNEW(MAXTEQ)
      DIMENSION P0(MAXTEQ), IZP0(0:MAXTEQ)
      DIMENSION PMXMN(MAXTEQ,2), IZPMXMN(0:MAXTEQ,2)
      DIMENSION VATOL(MAXEQ), VRTOL(MAXEQ), PDZERO(MAXTEQ)
      LOGICAL IPRINT, IDUMP, IPLOT, LTARG, exists, ispread, 
     . firstpassforsmokeview
      INTEGER ALL, SOME, IOS, ierror
	integer*2 filecount
	double precision ton, toff
      CHARACTER*133 MESSG
      PARAMETER (ALL = 1,SOME = 0)
      EXTERNAL RESID, JAC
      DATA PDZERO /MAXTEQ * 0.0D0/

      CALL CPTIME(TOFF)
      XX0 = 0.0D0
      XX1 = 1.0D0
      X0001 = 0.0001D0
	IERROR = 0
      TPAWS = TSTOP + XX1
      TSTART = ITMSTP - 1
      TOLD = TSTART
      DT = TSTOP - TSTART
      DPRINT = ABS(LPRINT)
      DPLOT = ABS(LDIAGP)
      DDUMP = ABS(LDIAGO)
	dspread = abs(lcopyss)
      RPAR(1) = RPTOL

C*** initialize print, dump, plot times

      T = TSTART
      TPRINT = T
      TDUMP = T
      TPLOT = T
	tspread = t
      IDID = 1
	firstpassforsmokeview = .true.

C     DECIDE WHETHER WE WILL PRINT, DUMP AND/OR PLOT

      IF (DPRINT.LT.X0001.OR.LPRINT.EQ.0) THEN
        IPRINT = .FALSE.
        TPRINT = TSTOP + XX1
      ELSE
        IPRINT = .TRUE.
      END IF

      IF (DPLOT.LT.X0001.OR.LDIAGP.LE.0) THEN
        IPLOT = .FALSE.
        TPLOT = TSTOP + XX1
      ELSE
        IPLOT = .TRUE.
      END IF

      IF (DDUMP.LT.X0001.OR.LDIAGO.LE.0) THEN
        IDUMP = .FALSE.
        TDUMP = TSTOP + XX1
      ELSE
        IDUMP = .TRUE.
      END IF

      IF (dspread.LT.X0001.OR.lcopyss.LE.0) THEN
        ispread = .FALSE.
        tspread = TSTOP + XX1
      ELSE
        ispread = .TRUE.
      END IF

      CALL SETINFO (INFO, RWORK)

C    COPY ERROR TOLERANCES INTO ARRAYS. IF THE LOCATION OF PRESSURE IS
C    CHANGED IN THE SOLVER ARRAY THEN THE FOLLOWING CODE HAS TO BE CHANGED
      DO 20 I = 1, NM1
        VATOL(I+NOFP) = APTOL
        VRTOL(I+NOFP) = RPTOL
        VATOL(I+NOFTU) = ATOL
        VRTOL(I+NOFTU) = RTOL
        VATOL(I+NOFVU) = ATOL
        VRTOL(I+NOFVU) = RTOL
        VATOL(I+NOFTL) = ATOL
        VRTOL(I+NOFTL) = RTOL
        IF(OPTION(FOXYGEN).EQ.ON)THEN
           VATOL(I+NOFOXYU)=ATOL
           VRTOL(I+NOFOXYU)=RTOL
           VATOL(I+NOFOXYL)=ATOL
           VRTOL(I+NOFOXYL)=RTOL
        ENDIF
   20 CONTINUE
      DO 30 I = 1, NHVPVAR
        VATOL(I+NOFPMV) = AHVPTOL
        VRTOL(I+NOFPMV) = RHVPTOL
   30 CONTINUE
      DO 40 I = 1, NHVTVAR
        VATOL(I+NOFTMV) = AHVTTOL
        VRTOL(I+NOFTMV) = RHVTTOL
   40 CONTINUE
      DO 50 I = 1, NWALLS
        VATOL(I+NOFWT) = AWTOL
        VRTOL(I+NOFWT) = RWTOL
   50 CONTINUE
      DO 55 I = 1, NEQTARG(MPLICIT)
        VATOL(I+NOFTT) = AWTOL
        VRTOL(I+NOFTT) = RWTOL
   55 CONTINUE
      DO 57 I = 1, FSMTYPE
      DO 57 J = 1, 5
         VATOL((I-1)*5+J+NOFFSM) = ATOL
         VRTOL((I-1)*5+J+NOFFSM) = RTOL
   57 CONTINUE

      OVTIME = XX0
      TOTTIME = XX0
      PRTTIME = XX0

C     SEE NOTE IN COMMENTS ABOUT THE NODES=NOFPRD LINE BELOW

      NODES = NOFPRD
      IPAR(1) = NODES
      IPAR(2) = ALL
      IDSET = 0

C     SETTING INTIAL VECTOR

      CALL SETP0(P0, IZP0, PMXMN, IZPMXMN, iofili, IERROR)
      IF (IERROR.GT.0) THEN
        RETURN
      END IF
      IF (IZP0(0).EQ.ON) THEN
        DO 65 I = 1, NODES
          IF (IZP0(I).EQ.ON) P(I) = P0(I)
   65   CONTINUE

c*** if we set pressures with SETP0 then over-ride steady state pressure
c    initialization

        DO 66 I = 1, NM1
          IF(IZP0(I+NOFP).EQ.ON)OPTION(FPSTEADY) = OFF
   66   CONTINUE
      END IF

C    CONSTRUCT INITIAL SOLUTION

      DO 60 I = 1, NEQUALS
        PDOLD(I) = 0.0D0
        POLD(I) = P(I)
   60 CONTINUE
      CALL INITSOLN(T,PDOLD,PDZERO,RPAR,IPAR)
      DO 70 I = 1, NEQUALS
        PPRIME(I) = PDOLD(I)
        POLD(I) = P(I)
   70 CONTINUE

!     calculate the mass of objects that have been pyrolized
!     at the moment we do only the total and the radiological species
!     make sure that the INTEGRATE routine is called before TOXIC
         call integrate mass (t, dt)

C     moved from initspec to here 2/10/93 - needs to be called after datacopy
      CALL TOXIC(XX0)
      
!	If we are running only an initialization test then we do not need to solve anything
	if (initializeonly) then
		 CALL TARGET(STEADY)
!	Normally, this only needs to be done while running. However, if we are doing an initialonly run then we need the output now
	    CALL REMAPFIRES (NFIRES, FLOCAL, FXLOCAL, FYLOCAL, 
     .		  FZLOCAL, FQLOCAL, FHLOCAL)
	    CALL SVOUT(smvdata, PREF, PA, TA, 
     .		  NM1, CXABS, CYABS, HRL, BR, DR, HR,
     .		  NVENTS,IZVENT(1,1),IZVENT(1,2),izvent(1,6),
     .          zzvent(1,3),zzvent(1,4),zzvent(1,1),zzvent(1,2),
     .          nvvent,
     .		  NFIRES, FLOCAL, FXLOCAL, FYLOCAL, FZLOCAL, 
     .          ntarg, 0.0D0, 1)
		 icode = 0
		 write (logerr, 5004)
		 return
	endif
 5004 format ('Initialize only')

C     MAIN SOLVE LOOP

      NUMJAC = 0
      NUMSTEP = 0
      NUMRESD = 0

   80 CONTINUE

C    DASSL EQUATION WITH MOST ERROR

      IEQMAX = 0

!     Check for interactive commands
!		 IF A KEY HAS BEEN PRESSED (and we are wathcing the keyboard) FIGURE OUT WHAT TO DO
!		 The escape key returns a code of 1

	IF (.NOT.NOKBD) CALL NTRACT(T,ICODE,TPAWS,TOUT,IEQMAX)
	inquire (file=stopfile, exist =exists)
	if (exists) then
		 icode = 1
	endif
!	If the stop file exists or the esc key has been pressed, then quit
	if (icode.eq.1) then
          write (logerr, 5000) t, dt
		 return
	endif
 5000 FORMAT (/,'Stopped by request at T = ',1PG11.3,' DT = ',G11.3)

!	Check the .query file. If it does not exist, do nothing. If if DOES exist, then
!	rewind/write the status file and delete the query file (in that order).
!	Ignore errors from deleting the file. It may not exist

	inquire (file=queryfile, exist = exists)
	if (exists) then
		call StatusOutput (T, dT, errorcode)
    		filecount = delfilesqq(queryfile)
	endif
		 
!	Now do normal output (printout, spreadsheets, ...)

      IF (IDID.GT.0) THEN
        LTARG = .FALSE.

        IF (T+X0001.GT.MIN(TPRINT,TSTOP).AND.IPRINT) THEN

C***  UPDATE TARGET TEMPERATURES (ONLY NEED TO UPDATE JUST BEFORE WE PRINT TARGET TEMPERATURES).
C	IF WE ACTUALLY USE TARGET TEMPERATURES IN A CALCULATION THEN THIS CALL WILL NEED TO BE MOVED TO INSIDE RESID.

          IF(.NOT.LTARG)THEN
             CALL TARGET(STEADY)
             LTARG = .TRUE.
          ENDIF

          ITMSTP = TPRINT
          CALL RESULT(T,1)
		  call StatusOutput (T, dT, errorcode)
          CALL OUTJCNT(T)
          TPRINT = TPRINT + DPRINT
          NUMJAC = 0
          NUMSTEP = 0
          NUMRESD = 0
          PRTTIME = XX0
        END IF

        IF (T+X0001.GT.MIN(TDUMP,TSTOP).AND.IDUMP) THEN
          ITMSTP = TDUMP + 1.0D0
          IF(.NOT.LTARG)THEN
             CALL TARGET(STEADY)
             LTARG = .TRUE.
          ENDIF
          CALL DUMPER(ITMSTP,IERROR)
          IF (IERROR.NE.0) RETURN
          TDUMP = TDUMP + DDUMP
		call StatusOutput (T, dT, errorcode)
        END IF

        IF (T+X0001.GT.MIN(TPLOT,TSTOP).AND.IPLOT) THEN
          ITMSTP = TPLOT
          IF(.NOT.LTARG)THEN
             CALL TARGET(STEADY)
             LTARG = .TRUE.
          ENDIF
!	Note: svout writes the .smv file. We do not close the file but only rewind so that smokeview 
!	can have the latest time step information. Remapfires just puts all of the information in a single list
	    CALL REMAPFIRES (NFIRES, FLOCAL, FXLOCAL, FYLOCAL, 
     .		               FZLOCAL, FQLOCAL, FHLOCAL)
	    CALL SVOUT(smvdata, PREF, PA, TA, NM1, CXABS, CYABS, HRL, BR,
     .               DR, HR, NVENTS,IZVENT(1,1),IZVENT(1,2),izvent(1,6),
     .               zzvent(1,3),zzvent(1,4),zzvent(1,1),zzvent(1,2),
     .               nvvent,
     .               NFIRES, FLOCAL, FXLOCAL, FYLOCAL,FZLOCAL,
     .			   ntarg,T,itmstp)
!	This ought to go earlier and drop the logical test. However, not all of the information 
!	is available until this point
		 if (firstpassforsmokeview) then
			 firstpassforsmokeview = .false.
			 call svplothdr (version,nm1,nfires)
		 endif
		 call svplotdata(T,NM1,ZZRELP,ZZHLAY(1,LOWER),
     .	                 ZZTEMP(1,2),ZZTEMP(1,1),NFIRES, FQLOCAL,
     .					     FHLOCAL)
           TPLOT = TPLOT + DPLOT
		 call StatusOutput (T, dT, errorcode)
        END IF

        IF (T+X0001.GT.MIN(tspread,TSTOP).AND.ispread) THEN
          ITMSTP = tspread
          IF(.NOT.LTARG)THEN
             CALL TARGET(STEADY)
             LTARG = .TRUE.
          ENDIF
         call SpreadSheetNormal (T, ierror)
		 call SpreadSheetSpecies (T, ierror)
		 call SpreadSheetFlow (T, ierror)
		 call SpreadSheetFlux (T, ierror)
		 if (ierror.ne.0) return
           tspread =tspread + dspread
		 call StatusOutput (T, dT, errorcode)
        END IF

!	Diagnostics
        IF (T+X0001.GT.TPAWS) THEN
          ITMSTP = TPAWS
          CALL RESULT(T,1)
          CALL DEBUGPR(1,T,DT,IEQMAX)
          TPAWS = TSTOP + 1.0D0
		 call StatusOutput (T, dT, errorcode)
        END IF

C*** find the interval next discontinuity is in

        IDISC = 0
        DO 104 I = 1, IZNDISC
          IF(T.GE.ZZDISC(I-1).AND.T.LT.ZZDISC(I))THEN
            IDISC = I
            GO TO 105
          ENDIF
  104   CONTINUE
  105   CONTINUE
        TOUT = MIN(TPRINT,TPLOT,TDUMP,tspread,TPAWS,TSTOP)

C*** if there is a discontinuity then tell dassl

        IF(IDISC.NE.0)THEN
          TOUT = MIN(TOUT,ZZDISC(IDISC))
          RWORK(1) = ZZDISC(IDISC)
          INFO(4) = 1
         ELSE
          INFO(4) = 0
        ENDIF
      END IF

      IF (T.LT.TSTOP) THEN
        IDSET = 0
        IPAR(2) = SOME
        TOLD = T
        CALL SETDERV(-1)
        CALL CPTIME(TON)
        CALL DDASSL(RESID,NODES,T,P,PPRIME,TOUT,INFO,VRTOL,VATOL,IDID,
     +      RWORK,LRW,IWORK,LIW,RPAR,IPAR,JAC)
C*** CALL CPU TIMER AND MEASURE, SOLVER TIME WITHIN DASSL AND OVERHEAD TIME (EVERYTHING ELSE).
        CALL SETDERV(-2)
        IEQMAX = IPAR(3)
        IF (OPTION(FPDASSL).EQ.ON) CALL DEBUGPR (3,T,DT,IEQMAX)
        OSTPTIME = TON - TOFF
        CALL CPTIME(TOFF)
        STIME = T
        STPTIME = TOFF - TON
        PRTTIME = PRTTIME + STPTIME
        TOTTIME = TOTTIME + STPTIME
        OVTIME = OVTIME + OSTPTIME
        TOVTIME = TOVTIME + OSTPTIME

C*** MAKE SURE DASSL IS HAPPY

        IF (IDID.LT.0) THEN
           CALL FND_COMP(IOFILO,IEQMAX)
           WRITE (MESSG,101)IDID
  101      FORMAT('ERROR, DASSL - IDID=', I3)
           CALL XERROR(MESSG,0,1,1)
           IERROR = IDID
           RETURN
        END IF

        DT = T - TOLD
        IF(IZDTFLAG)THEN
          IF(DT.LT.ZZDTCRIT)THEN
            IZDTNUM = IZDTNUM + 1
            IF(IZDTNUM.GT.IZDTMAX)THEN
C*** model has hung (IZDTMAX consective time step sizes were below ZZDTCRIT)
              WRITE(MESSG,103)IZDTMAX,ZZDTCRIT,T
  103         FORMAT(1x,I4,'consecutive time steps with size below',
     .               E11.4,' at t=',E11.4)
              CALL XERROR(MESSG,0,1,1)
              IZDTNUM = 0
            ENDIF
           ELSE
C*** this time step is above the critical size so reset counter
            IZDTNUM = 0
          ENDIF
        ENDIF

        IPAR(2) = ALL
        UPDATEHALL = .TRUE.
        CALL RESID(T,P,PDZERO,PDNEW,IRES,RPAR,IPAR)
        UPDATEHALL = .FALSE.
        CALL UPDREST(NODES, NEQUALS, NLSPCT, T, TOLD, P, POLD, PDNEW,
     .               PDOLD, PDZERO)

C    ADVANCE THE DETECTOR TEMPERATURE SOLUTIONS AND CHECK FOR OBJECT IGNITION

         IDSAVE = 0
         CALL UPDTECT(MDCHK,TOLD,DT,NDTECT,ZZHLAY,ZZTEMP,
     .                XDTECT,IXDTECT,IQUENCH,IDSET,IFDTECT,TDTECT)
         CALL UPDOBJ(MDCHK,TOLD,DT,IFOBJ,TOBJ,IERROR)
         TD = MIN(TDTECT,TOBJ)

C     A DETECTOR IS THE FIRST THING THAT WENT OFF

         IF (IFDTECT.GT.0.AND.TDTECT.LE.TD) THEN
           ISENSOR = IFDTECT
           ISROOM = IXDTECT(ISENSOR,DROOM)
           CALL UPDTECT(MDSET,TOLD,DT,NDTECT,ZZHLAY,ZZTEMP,
     .                  XDTECT,IXDTECT,IQUENCH,IDSET,IFDTECT,TDTECT)
           WRITE(LBUF,*) ' '
           CALL XERROR(LBUF,0,1,0)
           WRITE(LBUF,76)ISENSOR,TDTECT,ISROOM
   76      FORMAT('*** Sensor ',I3,' has activated at ',
     .            F6.1,' seconds in compartment ',i3,' ***')
           CALL XERROR(LBUF,0,1,0)
C       Check to see if we are backing up for detectors going off
           IF (OPTION(FBTDTECT).EQ.ON) THEN
              IDSAVE = IDSET
           ELSE
              IDSAVE = IFOBJ
              TD = TOBJ
              CALL RESID (T, P, PDZERO, PDNEW, IRES, RPAR, IPAR)
              IDSET = 0
           END IF
         ELSE
           CALL UPDTECT(MDUPDT,TOLD,DT,NDTECT,ZZHLAY,ZZTEMP,
     .                  XDTECT,IXDTECT,IQUENCH,IDSET,IFDTECT,TDTECT)
         END IF
C     OBJECT IGNITION IS THE FIRST THING TO HAPPEN
         IF (IFOBJ.GT.0.AND.TOBJ.LE.TD) THEN
           CALL UPDOBJ(MDSET,TOLD,DT,IFOBJ,TOBJ,IERROR)
           WRITE(IOFILO,5003) IFOBJ,trim(objnin(ifobj)),
!	Need a trick to keep from reporting an object which ignited prior to the simulation from
!	showing a negative time
     .           max(tobj,xx0)
 5003		  format(/,' Object #',i3,' (',a,') ignited at ',
     .            f10.3,' seconds')
c       Check to see if we are backing up objects igniting
           IF (OPTION(FBTOBJ).EQ.ON) THEN
             IDSAVE = IFOBJ
           ELSE
             IDSAVE = IDSET
             TD = TDTECT
             OBJON(IFOBJ) = .TRUE.
             OBJSET(IFOBJ) = 0
             CALL SETINFO(INFO,RWORK)
             IFOBJ = 0
           END IF
           ELSE
           CALL UPDOBJ(MDUPDT,TOLD,DT,IFOBJ,TOBJ,IERROR)
         END IF

      IF (IDSAVE.NE.0)THEN

C*** A DETECTOR HAS ACTIVATED SO CALL DASSL TO INTEGRATE BACKWARDS
C    IN TIME TO T=TD.  THIS IS BETTER THAN USING SIMPLE LINEAR INTERPOLATION
C    BECAUSE IN GENERAL DASSL COULD BE TAKING VERY BIG TIME STEPS

         IF(TOLD.LE.TD.AND.TD.LT.T)THEN
            CALL RESULT(T,1)
            IPAR(2) = SOME
            TDOUT = TD
            DO 74 I = 1, 11
   74       INFO2(I) = 0
            INFO2(2) = 1
            TOLD = T
            CALL DDASSL(RESID,NODES,T,P,PPRIME,TDOUT,INFO2,VRTOL,VATOL,
     +                   IDID,RWORK,LRW,IWORK,LIW,RPAR,IPAR,JAC)

C*** MAKE SURE DASSL IS HAPPY (AGAIN)

            IF (IDID.LT.0) THEN
               CALL FND_COMP(IOFILO,IPAR(3))
               WRITE (MESSG,101)IDID
               CALL XERROR(MESSG,0,1,-2)
               WRITE(messg,'(A13,f10.5,1x,A8,f10.5)')
     .                  'Backing from ',t,'to time ',tdout
               CALL XERROR(MESSG,0,1,1)
               CALL XERROR('ERROR IN DASSL WHILE BACKING UP',0,1,1)
               IERROR = IDID
               RETURN
            END IF

C*** RESET DASSL FLAGS TO INTEGRATE FORWARD FROM T=TD AND
C    CALL RESID TO GET PRODUCT INFO AT SPRINKLER ACTIVATION TIME

            IF (IFDTECT.GT.0) IDSET = IDSAVE
              DT = T - TOLD
              IPAR(2) = ALL

C*** CALL RESID TO GET PRODUCT INFO AT THE CORRECT TIME AND
C    TO SAVE FIRE RELEASE RATES IN ROOM WHERE DETECTOR HAS
C    ACTIVATED.  (THIS HAPPENS BECAUSE IDSET .NE. 0)

              CALL RESID (T, P, PDZERO, PDNEW, IRES, RPAR, IPAR)
              CALL UPDREST(NODES, NEQUALS, NLSPCT, T, TOLD, P, POLD,
     .                    PDNEW, PDOLD, PDZERO)
              CALL SETINFO(INFO,RWORK)
            ELSE IF (TD.EQ.T) THEN
              CALL SETINFO(INFO,RWORK)
              CALL RESID (T, P, PDZERO, PDNEW, IRES, RPAR, IPAR)
            ELSE
C    UPDTECT SAID THAT A SPRINKLER HAS GONE OFF BUT THE TIME IS WRONG!!
               WRITE(MESSG,'(A7,F10.5,A13,F10.5,A22,F10.5)')'TIME = '
     .            ,T,' LAST TIME = ',TOLD,' NEED TO BACK STEP TO ',TD
               CALL XERROR(MESSG,0,1,1)
               CALL XERROR('BACK STEP TO LARGE',0,1,1)
               CALL XERROR(' ',0,1,1)
               IERROR = IDID
               RETURN
            ENDIF
            DO  75 I = 1, MXOIN
                 OBJSET(I) = 0
   75       CONTINUE
         ENDIF

!     calculate the mass of objects that have been pyrolized
!     at the moment we do only the total and the radiological species
!     It is important to call the routine to integrate the mass before call the toxicology calculatino
         call integrate mass (t, dt)

C     CALCULATE GAS DOSAGE
         CALL TOXIC(DT)
         
         IF (OPTION(FDEBUG).EQ.ON) CALL DEBUGPR(2,T,DT,IEQMAX)
         NUMSTEP = NUMSTEP + 1
         GO TO 80
      END IF
      RETURN

      END

      SUBROUTINE UPDREST(NODES, NEQUALS, NLSPCT,  T, TOLD, P, POLD,
     .                   PDNEW, PDOLD, PDZERO)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDREST
C
C     Source File: SOLVE
C
C     Functional Class:
C
C     Description:
C
C     Arguments: NODES
C                NEQUALS
C                NLSPCT
C                T
C                TOLD
C                P
C                POLD
C                PDNEW
C                PDOLD
C                PDZERO
C
C     Revision History:
C        Created:  05/31/1995 at 9:57 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------


      include "precis.fi"
      include "cparams.fi"
      include "fltarget.fi"

      DIMENSION P(*), POLD(*), PDNEW(*), PDOLD(*), PDZERO(*)

      DT = T - TOLD
      XX0 = 0.D0

C    ADVANCE SPECIES

      DO 10 I = NODES + 1, NEQUALS
        P(I) = P(I) + DT*PDOLD(I)
        P(I) = MAX (XX0, P(I))
        PDOLD(I) = PDNEW(I)
   10 CONTINUE

c
c*** advance explicit target temperatures and update implicit temperatures
c

      CALL TRHEAT(1,XPLICIT,DT,PDZERO,PDNEW)
      CALL TRHEAT(1,MPLICIT,DT,PDZERO,PDNEW)
      IF (NLSPCT.GT.0) CALL RESYNC(P,NODES+1)

      DO 20 I = 1, NEQUALS
         POLD(I) = P(I)
   20 CONTINUE

      RETURN
      END

      SUBROUTINE NTRACT(T,ICODE,TPAWS,TOUT,IEQMAX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     NTRACT
C
C     Source File: SOLVE.SOR
C
C     Functional Class:
C
C     Description:
C
C     Arguments: T
C                ICODE
C                TPAWS
C                TOUT
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "opt.fi"
      include "dervs.fi"
      include "cenviro.fi"

      LOGICAL SLVHELP
      INTEGER*2 CH, HIT

      ICODE = 0
      CALL GRABKY(CH,HIT)
      IF (HIT.GT.0) THEN
        IF (CH.EQ.27) THEN
          ICODE = 1
          RETURN
        ELSEIF (HIT.GT.1) THEN
          IF (OPTION(FKEYEVAL).EQ.ON) THEN
            IF (CH.EQ.59) THEN
              WRITE (*,5010) T, DT
              IF (SLVHELP()) ICODE = 1
            ELSE IF (CH.EQ.60) THEN
              IF (OPTION(FDEBUG).EQ.ON) THEN
                OPTION(FDEBUG) = OFF
                WRITE (*,*) 'Debug is now off'
                WRITE (*,*)
              ELSE
                OPTION(FDEBUG) = ON
              END IF
            ELSE IF (CH.EQ.61) THEN
              SWITCH(1,NR) = .NOT. SWITCH(1,NR)
              WRITE (*,*) 'Toggle flow field printing to ',
     +            SWITCH(1,NR)
            ELSE IF (CH.EQ.62) THEN
              CALL DEBUGPR(1,T,DT,IEQMAX)
            ELSE IF (CH.EQ.63) THEN
              WRITE (*,5010) T, DT
            ELSE IF (CH.EQ.64) THEN
              WRITE (*,5010) T, DT
              WRITE (*,*) 'Enter time at which to pause: '
              READ (*,*) RCODE
              TPAWS = RCODE
              TOUT = MIN(TPAWS,TOUT)
            ELSE IF (CH.EQ.65) THEN
              IF (OPTION(FPDASSL).EQ.ON) THEN
                OPTION(FPDASSL) = OFF
                WRITE (*,*) 'DASSL debug is now off - continuing'
              ELSE
                OPTION(FPDASSL) = ON
              END IF
            END IF
          ELSE
            WRITE (*,5010) T, DT
          END IF
        END IF
      END IF
	
      RETURN
 5010 FORMAT (' Time = ',1PG12.4,', dt = ',1PG12.4)
      END

      LOGICAL FUNCTION SLVHELP()
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SLVHELP
C
C     Source File: SOLVE.SOR
C
C     Functional Class:
C
C     Description:
C
C     Arguments:
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "opt.fi"

      INTEGER*2 CH, HIT

      WRITE (IOFILO,*) '***Options Set***'
      WRITE (IOFILO,5000) (OPTION(II),II = 1,MXOPT)
      WRITE (IOFILO,*)
     + '************************************************************'
      WRITE (IOFILO,*)
     + '1=Help,2=debug,3=flow,4=pause,5=time,6=pause time,7=dassl(t)'
      WRITE (IOFILO,*)
     + 'Press <esc> to quit, any other key to continue'
      WRITE (IOFILO,*)
     + '************************************************************'

   10 CALL GRABKY(CH,HIT)
      IF (HIT.EQ.0) GO TO 10
      IF (CH.EQ.27) THEN
        SLVHELP = .TRUE.
        WRITE (IOFILO,*) 'Run terminated at user request'
      ELSE
        SLVHELP = .FALSE.
        WRITE (IOFILO,*) 'Continuing'
        WRITE (IOFILO,*)
      END IF
      RETURN
 5000 FORMAT (1X,20I3)
      END

      SUBROUTINE DEBUGPR(IKEY,T,DT,IEQMAX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DEBUGPR
C
C     Source File: SOLVE.SOR
C
C     Functional Class:
C
C     Description:
C
C     Arguments: IKEY
C                T
C                DT
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by PAR
C        Modified: 6/30/95 by GPF
C                  added debug print for oxygen concentrations and fire size
C                  when returning from dassl
C        Modified: 1/24/1996 at 14:38 by RWP:
C                  Call XERROR to display messages
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "opt.fi"
      include "params.fi"
      include "objects2.fi"
      include "fltarget.fi"

      INTEGER*2 CH, HIT
      CHARACTER SPNAME(NS)*5, CCC*3
      INTEGER BMAP(MBR)
      LOGICAL FIRSTC
      DATA SPNAME /'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%',
     +    '  TUH', ' H2O%', '   OD', '   CT', 'TS'/
      DATA FIRSTC /.TRUE./
      SAVE BMAP

C     DEBUG PRINTING

      IF (FIRSTC) THEN
        FIRSTC = .FALSE.
        DO 30 I = 1, NBR
          DO 10 J = 1, NCNODE(NA(I))
            IF (I.EQ.ICMV(NA(I),J)) THEN
              BMAP(I) = J
              GO TO 20
            END IF
   10     CONTINUE
   20     CONTINUE
   30   CONTINUE
      END IF

      IF (IKEY.EQ.1) THEN
        WRITE (*,*) 'Pause at time = ', T,
     +      ',  Press any key to continue'
   40   CALL GRABKY(CH,HIT)
        IF (HIT.EQ.0) GO TO 40
        WRITE (*,*) 'Continuing'
        WRITE (*,*)
      ELSE IF (IKEY.EQ.2) THEN
        WRITE (IOFILO,5000) T, DT
        DO 60 I = 1, NM1
          WRITE (*,5010) I
          WRITE (*,5020) '   Upper temp(K)', ZZTEMP(I,UPPER)
          WRITE (*,5020) '   Lower temp(K)', ZZTEMP(I,LOWER)
          WRITE (*,5020) ' Interface ht(m)', ZZHLAY(I,LOWER)
          WRITE (*,5020) '   Pressure (pa)', ZZRELP(I)
          IF (NLSPCT.GT.0) WRITE (*,*) ' SPECIES MASS FRACTIONS ',
     +        ' UPPER           LOWER'
          DO 50 IPROD = 1, NS
            IF (ACTIVS(IPROD)) THEN
              WRITE (*,5030) SPNAME(IPROD), (ZZCSPEC(I,IL,IPROD),IL
     +            = UPPER,LOWER)
            END IF
   50     CONTINUE
          IF (NWALLS.NE.0) WRITE (*,*) ' WALL TEMPERATURES'
          IF (SWITCH(1,I)) THEN
            WRITE (*,5040) ZZWTEMP(I,1,1)
          END IF
          IF (SWITCH(3,I)) THEN
            WRITE (*,5060) ZZWTEMP(I,3,1)
          END IF
          IF (SWITCH(4,I)) THEN
            WRITE (IOFILO,5070) ZZWTEMP(I,4,1)
          END IF
          IF (SWITCH(2,I)) THEN
            WRITE (IOFILO,5050) ZZWTEMP(I,2,1)
          END IF
   60   CONTINUE
        WRITE (*,*) ' '
        WRITE (*,*) 'HVAC PRINT BY SYSTEMS'
        DO 90 ISYS = 1, NHVSYS
          WRITE (*,*) 'FOR SYSTEM ', ISYS
          WRITE (*,*) 'MASS FLOW OF SYSTEM ', HVMFSYS(ISYS)
          WRITE (*,*) 'MASS OF GAS IN SYSTEM ', ZZHVM(ISYS)
          DO 70 IPROD = 1, NS
            WRITE (*,*) 'MASS OF ', SPNAME(IPROD), ' ',
     +          ZZHVPR(ISYS,IPROD)
   70     CONTINUE
          DO 80 IDT = 1, NBR
            IF (IZHVBSYS(IDT).EQ.ISYS) THEN
              WRITE (*,5080) NA(IDT), HVP(NA(IDT)), NE(IDT),
     +            HVP(NE(IDT)), HVFLOW(NA(IDT),BMAP(IDT)), TBR(IDT)
            END IF
   80     CONTINUE
   90   CONTINUE
        IF (NDTECT.NE.0)THEN
           WRITE(*,*)'DETECTOR INFO'
           WRITE(*,100)
  100      FORMAT('  N ',3X,'D TEMP',6X,'J TEMP',6X,' ACT')
           DO 101 I = 1, NDTECT
              IROOM = IXDTECT(I,DROOM)
              IF (IQUENCH(IROOM).EQ.I)THEN
                 CCC='***'
              ELSE
                 CCC = '   '
              ENDIF
              WRITE(*,102)I,XDTECT(I,DTEMP),XDTECT(I,DTJET),
     .             XDTECT(I,DVEL),XDTECT(I,DTACT),CCC
  102         FORMAT(1X,I2,1X,4(E11.4,1X),A3)
  101      CONTINUE
        ENDIF
        WRITE (*,*) ' '
      ELSE IF (IKEY.EQ.3) THEN
        WRITE (*,5090) T, DT
        CALL FND_COMP(IOFILO,IEQMAX)
        WRITE(*,6030)
        DO 201 IROOM = 1, NM1
           WRITE(*,6000)IROOM,ZZRELP(IROOM),ZZHLAY(IROOM,LOWER),
     .                       ZZTEMP(IROOM,LOWER),ZZTEMP(IROOM,UPPER),
     .                   ZZCSPEC(IROOM,LOWER,2),ZZCSPEC(IROOM,UPPER,2)
  201   CONTINUE
        IF(NHVPVAR.GT.0)WRITE(*,6010)(P(NOFPMV+I),I=1,NHVPVAR)
        IF(NHVTVAR.GT.0)WRITE(*,6020)(P(NOFTMV+I),I=1,NHVTVAR)
        IF(NNODE.GT.0)WRITE(*,6040)
        DO 210 I = 1, NNODE
          DO 220 J = 1, NCNODE(I)
             DP = HVP(IN(I,J)) - HVP(I) + DPZ(I,J)
             WRITE(*,6050)I,IN(I,J),DP,HVP(I),HVP(IN(I,J)),
     .                         HVGHT(I)
  220     CONTINUE
  210   CONTINUE
        WRITE(*,6070)
        DO 230 IROOM = 1, NM1
           XQF = 0.
           DO 202 IOBJ = 0, NUMOBJL
             IF (IROOM.EQ.FROOM(IOBJ))XQF = XQF + FQF(IOBJ)
  202      CONTINUE
           XQF = XQF + FQDJ(IROOM)
          WRITE(*,6060)IROOM,ZZWTEMP(IROOM,1,1),ZZWTEMP(IROOM,3,1),
     .                            ZZWTEMP(IROOM,4,1),ZZWTEMP(IROOM,2,1),
     .                            XQF
  230   CONTINUE
        IF(NUMOBJL.GT.0)THEN
          WRITE(*,6080)
          DO 240 IOBJ = 1, NUMOBJL
           WRITE(*,6085)IOBJ,XFIRE(IOBJ,10),XFIRE(IOBJ,11)
  240     CONTINUE
        ENDIF
        IF(NTARG.GT.0)THEN
          WRITE(*,6090)
          DO 250 ITARG = 1, NTARG
            WRITE(*,6095)ITARG,XXTARG(TRGTEMPF,ITARG)
  250     CONTINUE
        ENDIF
      END IF
      RETURN

 5000 FORMAT (' T = ',1PG12.4,' DT = ',1PG12.4)
 5010 FORMAT (' For room ',I3,' at time      T ')
 5020 FORMAT (A16,5X,E14.7,3X,E14.7)
 5030 FORMAT (15X,A5,1X,2(E14.7,3X))
 5040 FORMAT ('  Ceiling temp(K) ',F12.2)
 5050 FORMAT ('  Floor   temp(K) ',F12.2)
 5060 FORMAT ('  Up wall temp(K) ',F12.2)
 5070 FORMAT (' Low wall temp(K) ',E12.2)
 5080 FORMAT (' from ',I2,' pressure ',E10.3,' to ',I2,' pressure ',
     + G10.3,' mass flow is ',G10.3,' temp ',G10.3)
 5090 FORMAT (' Returned from dassl at T = ',1PG14.6,',  dt = ',1PG12.4)
 5095 FORMAT (' Solution Component with the most error: ',I3)
 6000 FORMAT(1X,I3,1X,6E13.6)
 6010 FORMAT(' HVAC   PRESSURES:',4E13.6)
 6020 FORMAT(' HVAC TEMPERATUES:',4E13.6)
 6030 FORMAT(T2,'ROOM',T9,'PRESSURE',T20,'LAYER HEIGHT',T35,'L. TEMP',
     .       T48,'U. TEMP',T62,'L. Oxy',T75,'U. Oxy')
 6040 FORMAT(T3,'NODES',T12,'DELTA P',T23,'P AT FIRST NODE',
     .       T39,'P AT 2ND NODE',T57,'HEIGHT')
 6050 FORMAT(1X,2I3,1X,4(E13.6,2x))
 6060 FORMAT(1X,I3,1X,5E13.6)
 6070 FORMAT(T2,'ROOM',T11,'Ceiling',T21,'Upper Wall',
     .      T36,'Lower Wall',T49,'Floor',T61,'Fire Size')
 6080 FORMAT(T2,'Object',T11,'Heat in lower ',T26,'Heat in upper')
 6085 FORMAT(1X,I2,4X,2E13.6)
 6090 FORMAT(T2,'Target',T11,'Temp')
 6095 FORMAT(1X,I2,4X,E13.6)

      END

      SUBROUTINE SETINFO(INFO,RWORK)
      include "precis.fi"
      include "cparams.fi"
      include "solvprm.fi"
      include "opt.fi"
      DIMENSION INFO(*), RWORK(*)
      XX0 = 0.0D0
      DO 10 I = 1, 11
   10 INFO(I) = 0
      INFO(3) = 1
      INFO(2) = 1
      IF (STPMAX.LE.XX0) THEN
         INFO(7) = 0
      ELSE
         INFO(7) = 1
         RWORK(2) = STPMAX
      END IF
      IF (DASSLFTS.LT.XX0) THEN
         INFO(8) = 0
      ELSE
         INFO(8) = 1
         RWORK(3) = DASSLFTS
      END IF

C     SETTING JACOBIAN FLAG

      INFO(5) = 0
      INFO(11) = 1
      RETURN
      END

      SUBROUTINE FND_COMP(IOUNIT,ICOMP)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FND_COMP
C
C     Source File: SOLVE.f
C
C     Functional Class:
C
C     Description:
C
C     Arguments: IOUNIT
C                ICOMP
C
C     Revision History:
C        Modified: 3/4/97 by GPF:
C                  Change the messages so that they "read" better
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cfin.fi"
      include "cenviro.fi"
      include "opt.fi"

      WRITE(LBUF,*)'Solution component with the greatest error is'
      CALL XERROR(LBUF,0,1,0)
      IF (ICOMP.LE.NOFP+NM1) THEN
         WRITE(LBUF,'(A18,I2)')' pressure in room ',icomp
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTU) THEN
         WRITE(LBUF,'(A18,I2)')' either HVAC or FSM ',icomp-nm1
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFVU) THEN
         WRITE(LBUF,'(A27,I2)')' upper layer temp in room ',
     .            icomp-noftu
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTL) THEN
         WRITE(LBUF,'(A26,I2)')' upper layer vol in room ',
     .            icomp-nofvu
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTL+NM1) THEN
         WRITE(LBUF,'(A27,I2)')' lower layer temp in room ',
     .            icomp-noftl
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFWT) THEN
         IF (OPTION(FOXYGEN).EQ.ON) THEN
            WRITE(LBUF,'(A18,I2)')' oxygen component ',icomp-nofoxyl
            CALL XERROR(LBUF,0,1,0)
         ELSE
            WRITE(LBUF,'(A15,I2)')' target number ',
     .             icomp-noftt
            CALL XERROR(LBUF,0,1,0)
         ENDIF
      ELSE IF (ICOMP.LE.NOFPRD) THEN
         ITMP = ICOMP - NOFWT
         IRM = IZWALL(ITMP,1)
         IW = IZWALL(ITMP,2)
         IF (IW.EQ.1) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)')
     .      ' wall temp in room ',IRM,' ceiling '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.2) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)')
     .      ' wall temp in room ',IRM,' floor   '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.3) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)')
     .      ' wall temp in room ',IRM,' upper wall '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.4) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)')
     .      ' wall temp in room ',IRM,' lower wall '
            CALL XERROR(LBUF,0,1,0)
         END IF
      END IF

      RETURN
      END


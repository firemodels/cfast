            program cfast

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "params.fi"
      include "thermp.fi"
      include "objects1.fi"
      include "cenviro.fi"
      include "iofiles77.fi"

      logical error
	integer errorcode
      errorcode = 0

c     initialize the basic memory configuration

      call initmm
      call initob
      call readop
c     
	call readcf1 (errorcode)
c     
	write (logerr, 5000) mpsdatc
	if (errorcode.gt.0) then
		write (*, 5001) errorcode
		stop 
	else
		write (logerr, 5002) project
	endif

c     creation date

      mpsdat(1) = rundat(1)
      mpsdat(2) = rundat(2)
      mpsdat(3) = rundat(3)

!	version information to the log file
	call versionout (version)

      call initslv
c     call the input routine for a normal start

      call readinputfile (errorcode)
	if (errorcode.gt.0) go to 10

!	version information to the output file
      if (header) call disclaim('CFAST')

c     initialize variables associated with species 

      call initspec

      xdelt = nsmax / deltat
      itmmax = xdelt + 1
      tstop = itmmax - 1

c     read the the databases

	call inputtpp (thrmfile, errorcode)
	if (errorcode.ne.0) go to 10

      call initwall(tstop,errorcode)
      if (errorcode.gt.0) go to 10

c     time step criterion

      stime = 0.0d0
      itmstp = 1

      ih = -1

      xdelt = nsmax / deltat
      itmmax = xdelt + 1
      tstop = itmmax - 1

      call outinitial(1)
	call cptime(tbeg)
	call solve(tstop,errorcode)
	call cptime(tend)
	write (logerr,5003) tend - tbeg
	call cfastexit ('CFAST', 0)
      stop

c     errors

   10 call cfastexit ('CFAST', errorcode)

 5000 format ('Date stamp from CFAST initialization ',a14)
 5001 format ('Error encountered in opening data files; code = ',i4)
 5002 format ('The project files are based on the root: ',a64)
 5003 format ('Total execution time = ',1pg10.3,' seconds')
 5020 format ('Error exit during initialization from CFAST main')
      END

      block data initcs

c     define the basic parameters

      include "cparams.fi"
      include "cshell.fi"
      include "cplotd.fi"

      data thrmfile/'thermal'/, gfile/'GEOMETRY.DAT'/, 
     . pfile/'PARTITIN.DAT'/, ofile/'OBJECTS.DAT'/, dpath/' '/, 
     . dfile/'DATA.DAT'/, path/' '/, dlen/0/, plen/0/, advfea/.false./,
     . config/'HV6.CF'/, shell/.false./, csopen/.false./,
     . logerr/0/, nnfile/' '/, current/' '/,outfile/'<stdout>'/,
     . ICBG,ICTXT,ICHDR,ICSUB,ICPRO,ICMSG,ICMBG,ICHLP,ICHBG,ICEBG,
     . ICEMS/0,15,14,10,7,15,3,15,2,1,10/, DUMPF/' '/,RFILE/' '/,
     . RMFILE/1/, HEADER/.FALSE./, UNITS/0,5,12,18,31,37,24,27/,
     . ADUMPF/' '/, NOKBD/.FALSE./, GPAUSE/.FALSE./,QUICKEST/.FALSE./,
     . WEBPAGE/' '/, GUISURF/'DEFAULT'/, GUIOBJ/'DEFAULT'/,
     . CPFLSYNCH/'Synchronize gui and base models - do not edit'/,
     . GUIPROJ/' '/, GUIREF/' '/,GUILIC/' '/, initializeonly/.false./,
     . outputformat /0/, IOFILO/6/, REPORTO/0/, IOFILI/1/, LOGERR/3/,
     . OBFILI/99/, AHFILO/98/,WEBFILE/97/,
    
     . VERSION/6100/
! 
      DATA IDEF /1/, ITODEF /2/, LAYDEF /1/, ISPDEF /3/, ICHRS /4/,
     . IDEVO /6/, IDEVC /1/, SETAXP /.FALSE./, OPENPC /.FALSE./,
     . LSABCI /999/, LAFRAM /1/, IDTYPE /2/, SETPLT /.FALSE./,
     . WCOLOR/.TRUE./, WDASH/.TRUE./, SETCFP/.FALSE./
      
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC',
     . 'H2O', 'OD', 'CT', 'TS'/
       
      END

      block data initcf

      include "precis.fi"
      include "cfast.fi"

      DATA CRDATE/2007,11,29/
      END
      
      SUBROUTINE INITSOLN(T,PDOLD,PDZERO,RPAR,IPAR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSOLN
C
C     Source File: INITSOLN.SOR
C
C     Functional Class:
C
C     Description:  This routine determines an initial solution to
C                   the zone fire modeling equations.  A non-linear
C                   algebraic solver (SNSQE) is used to calculate initial
C                   room pressures that make dP/dt zero.  If an HVAC system
C                   is modeled then HVAC node pressures and hvac duct
C                   temperatures are also determined to force mass and energy
C                   conservation.
C
C     Arguments: T
C                PDOLD
C                PDZERO
C                RPAR
C                IPAR
C
C     Revision History:
C        Created:  06/14/1992 at 12:43 by GPF
C        Modified: 12/1/1992 at 12:45 by PAR
C                  RESYNC was added so that species mass adds up to total mass
C                  at new pressure
C        Modified: 8/15/1995 at 11:13 by PAR
C                    Added initalization for flame spread dif eqs if we have a
C                    flame spread object
C        Modified: 2/5/95: GPF
C                    Ignore pressure solution for rooms that are isolated from outside.
C        Modified: 10/20/97: GPF
C                    retry solution solve if the first try used the "steady"
C                    option and it failed.
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "params.fi"
      include "solvprm.fi"
      include "opt.fi"
      include "objects1.fi"
      include "objects2.fi"

      DIMENSION IPAR(*), PDOLD(*), PDZERO(*), RPAR(*)
      EXTERNAL GRES, GRES2, GRES3
      PARAMETER (MXALG = 4*NR+MNODE+MBR)
      DIMENSION DELTAMV(MXALG), HHVP(MXALG)
      PARAMETER (LRW = (3*MXALG**2+13*MXALG)/2)
      DIMENSION WORK(LRW)
      CHARACTER*132 MESSG
      EXTERNAL GJAC


    1 CONTINUE

      CALL ROOMCON(T)

      XX0 = 0.0D0
      RPAR2(1) = RPAR(1)
      IPAR2(1) = IPAR(1)
      IPAR2(2) = IPAR(2)
      CALL SETDERV(-1)
      CALL RESID(T,P,PDZERO,PDOLD,IRES,RPAR2,IPAR2)
      IOPT = 2
      TOL = ALGTOL
      NHVALG = NHVPVAR + NHVTVAR
      NALG0 = NHVALG
      NALG1 = NM1 + NHVALG
      NPRINT = -1

c*** room pressures

      DO 10 I = 1, NM1
        HHVP(I) = P(I+NOFP)
   10 CONTINUE

c*** hvac pressures

      DO 20 I = 1, NHVPVAR
        HHVP(I+NM1) = P(I+NOFPMV)
   20 CONTINUE

c*** hvac temperatures

      DO 30 I = 1, NHVTVAR
        HHVP(I+NM1+NHVPVAR) = P(I+NOFTMV)
   30 CONTINUE

      DO 40 I = 1, NEQUALS
        PINIT(I) = P(I)
   40 CONTINUE
      IF (OPTION(FPSTEADY).EQ.1) THEN
        CALL SNSQE(GRES,GJAC,IOPT,NALG1,HHVP,DELTAMV,TOL,NPRINT,INFO,
     *      WORK,LRW)
       ELSEIF (OPTION(FPSTEADY).EQ.2) THEN
        IOFF0 = NALG1

c*** upper layer temperatures

        NALG2 = NALG1 + 1
        HHVP(1+IOFF0) = ZZFTEMP(LFBO,UPPER)

c*** wall temperatures

c*** copy wall temperatures

        II = 0
        IEQ1 = IZWMAP2(1,LFBO)
        IEQ2 = IZWMAP2(3,LFBO)
        IF(IEQ1.NE.0)THEN
          II = II + 1
          NALG2 = NALG2 + 1
          HHVP(II+IOFF0+1) = P(IEQ1)
        ENDIF
        IF(IEQ2.NE.0)THEN
          II = II + 1
          NALG2 = NALG2 + 1
          HHVP(II+IOFF0+1) = P(IEQ2)
        ENDIF

        CALL SNSQE(GRES3,GJAC,IOPT,NALG2,HHVP,DELTAMV,TOL,NPRINT,INFO,
     *      WORK,LRW)
       ELSE
        IF (NHVALG.GT.0) THEN
          CALL SNSQE(GRES2,GJAC,IOPT,NALG0,HHVP(1+NM1),DELTAMV(1+NM1),
     *               TOL,NPRINT,INFO,WORK,LRW)
         ELSE
          INFO = 1
        ENDIF
      ENDIF

C*** couldn't find a solution.  either try to recover or stop

      IF (INFO.NE.1) THEN
        IF(OPTION(FPSTEADY).NE.OFF)THEN
          OPTION(FPSTEADY) = OFF
          CALL XERROR('Trying non-steady initial guess' ,
     .              0,101,1)
          GO TO 1
        ENDIF
        CALL XERROR('Solver could not find an initial solution' ,
     .              0,102,2)
      ENDIF

C*** if a room is not connected to any other room via a horizontal or
C    vertical vent then do not use the SNSQE pressure solution,
C    use the original pressure solution that was based on rho*g*h.

      DO 50 I = 1, NM1
        IF(IZCON(I))P(I+NOFP) = HHVP(I)
   50 CONTINUE
      DO 60 I = 1, NHVPVAR
        P(I+NOFPMV) = HHVP(I+NM1)
   60 CONTINUE
      DO 70 I = 1, NHVTVAR
        P(I+NOFTMV) = HHVP(I+NM1+NHVPVAR)
   70 CONTINUE
      IF (OPTION(FPSTEADY).EQ.2) THEN
        P(LFBO+NOFTU) = HHVP(1+IOFF0)
        II = 0
        IF(IEQ1.NE.0)THEN
          II = II + 1
          P(IEQ1) = HHVP(II+IOFF0+1)
        ENDIF
        IF(IEQ2.NE.0)THEN
          II = II + 1
          P(IEQ2) = HHVP(II+IOFF0+1)
        ENDIF
      ENDIF
      IF (FSMTYPE.GT.0) THEN
        DO 75 I = 1, NUMOBJL
          IF (OBJTYP(I).EQ.3) THEN
            P(NOFFSM+1+(I-1)*5) = 0.0D0
            P(NOFFSM+2+(I-1)*5) = 0.0D0
            P(NOFFSM+3+(I-1)*5) = 0.0D0
            P(NOFFSM+4+(I-1)*5) = 0.0D0
            P(NOFFSM+5+(I-1)*5) = objqarea
          END IF
   75   CONTINUE
      END IF
      CALL RESID(T,P,PDZERO,PDOLD,IRES,RPAR,IPAR)

C     Added to resync the species mass with the total mass of each layer at
C     the new pressure  12/01/92
C
      NODES = NOFPRD+1
      CALL RESYNC(P,NODES)
      DO 80 I = 1, NHVPVAR
        PDOLD(I+NOFPMV) = XX0
   80 CONTINUE
      DO 90 I = 1, NHVTVAR
        PDOLD(I+NOFTMV) = XX0
   90 CONTINUE
      DO 100 I = 1, NWALLS
        PDOLD(I+NOFWT) = XX0
  100 CONTINUE
      RETURN
      END


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

      SUBROUTINE RESID (TSEC,X,XPSOLVE,DELTA,IRES,RPAR,IPAR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RESID
C
C     Functional Class:
C
C     Description:  Calculates the residual F(t,y,dy/dt) for CFAST
C                   differential and algebraic equations.  For the gas
C                   differential equations (pressure, layer volume,
C                   upper/lower layer temperature) F(t,y,dy/dt) takes
C                   the form F(t,y,dy/dt) = dy/dt - f(t,y) where f(t,y) is
C                   related to the conservation of mass and and energy.
C                   For the wall temperature equations, F is just Fourier's
C                   law taking the form of
C                   F(t,y,dy/dt) = q''(t,y) + K dT/dx
C                   where q'' is the flux striking the wall, K is the wall's
C                   thermal conductivity and dT/dx is the surface wall
C                   temperature gradient.
C
C     Arguments: TSEC    Current simulation time (T above in s)
C                X       Current guess at solution vector (Y above)
C                XPSOLVE XPSOLVE Current guess at derivative of solution
C                        vector (Y' above)
C                DELTA   Residual or value of F(t,y,dy/dt)
C                IRES    Outputs:  IRES    Integer flag which is always equal to
C                        zero on input. RESID should alter IRES
C                        only if it encounters an illegal value of Y or
C                        a stop condition. Set IRES = -1 if an input
C                        value is illegal, and DDASSL will try to solve
C                        the problem without getting IRES = -1. If
C                        IRES = -2, DASSL return control to the calling
C                        program with IDID = -11.
C                RPAR    real parameter arrays
C                        are used for communication between SOLVE and
C                        RESID via DASSL. They are not altered by DASSL.
C                        Currently, only IPAR is used in RESID to pass
C                        a partial / total flag for solution of the
C                        species equations.
C                IPAR
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:07 by GPF:
C                  Installed HVAC (solved by DASSL instead of by time splitting)
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have
C                  the number of fires as the first subscript.
C        Modified: 10/14/93 by GPF:
C                  added detection/suppression
C                  Defined STIME
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/94 & 3/10/95
C                  the array xfire was formerly a local variable in
C                  resid.  the target calculation routines are not
C                  called by resid and needed access to xfire.
C                  therefor xfire was moved to main common block.
C        Modified: 4/26/1995 gpf:
C                  added call to TRHEAT to calculate TARGET residuals for targets
C                  temperatures computed by DASSL
C        Modified: 6/30/1995 gpf:
C                  added equations for oxygen
C        Modified: 7/13/1995 gpf:
C                  changed MXIRM to NR in several dimension statements
C          Modified: 8/15/1995 par:
C                    changed the argument list for FIRES to match the changed
C                    FIRES subroutine.  Added calculating the residuals for
C                    flame spread objects.  Added OBJECTS2.INC to get resid.
C        Modified: 9/5/1995 at 10:23 by PAR:
C                  Added support for IERROR and returning error codes to DASSL
C        Modified: 7/22/1996 by GPF
C                  changed product reference from IP to M.  (product
C                  flow logic depends on where mass is flowing, IP was not
C                  correct)
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C     Commons:

C     WRITTEN:  Dt       Qc       Qf       Qscnv
C      PASSED:  Dt
C        USED:  Activs   Ar       Cp       Gamma    Hr       Izpmap
C               N        Nlspct   Nm1      Option   Qf       Switch
C               Told     Zzhlay   Zzmass   Zzpabs   Zzrho    Zztemp
C               Zzvol


      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "opt.fi"
      include "params.fi"
      include "dervs.fi"
      include "wnodes.fi"
      include "flwptrs.fi"
      include "fltarget.fi"
      include "objects2.fi"

C     TEMPORAY DECLARATIONS AND ASSIGNMENTS

      INTEGER ALL, SOME, UU, LL
      PARAMETER (ALL = 1,SOME = 0, UU = UPPER,LL = LOWER)

C     DATA STRUCTURES FOR DASSL, THE NUMERICAL SOLVER

      DIMENSION IPAR(*), RPAR(*)
      DIMENSION X(*), XPSOLVE(*), XPRIME(MAXTEQ), DELTA(*)

C     DATA STRUCTURE FOR TOTAL FLOWS AND FLUXES

      DIMENSION FLWTOT(NR,MXPRD+2,2), FLXTOT(NR,NWAL)

C     DATA STRUCTURES FOR FLOW THROUGH VENTS

      DIMENSION FLWNVNT(NR,MXPRD+2,2)
      DIMENSION FLWHVNT(NR,NS+2,2)

C     DATA STRUCTURES FOR FIRES

      DIMENSION FLWF(NR,NS+2,2)

C     DATA STRUCTURES FOR CONVECTION, RADIATION, AND CEILING JETS

      DIMENSION FLWCV(NR,2), FLXCV(NR,NWAL)
      DIMENSION FLWRAD(NR,2), FLXRAD(NR,NWAL)
      DIMENSION FLWCJET(NR,2), FLXCJET(NR,NWAL)

C     DATA STRUCTURES FOR MECHANICAL VENTS

      DIMENSION FLWMV(NR,NS+2,2), filtered(nr,ns+2,2)

C     DATA STRUCTURES FOR HCL DEPOSITION

      DIMENSION FLWHCL(NR,NS+2,2), FLXHCL(NR,4)

C     DATA STRUCTURES FOR DOOR JET FIRES

      DIMENSION FLWDJF(NR,NS+2,2)
      INTEGER UPDATE

      LOGICAL VFLOWFLG, HVACFLG, DJETFLG

      IERROR = 0
      XX0 = 0.0D0
      NPROD = NLSPCT
      DT = TSEC - TOLD
      NUMRESD = NUMRESD + 1
      STIME = TSEC

      NIRM = NM1

      CALL DATACOPY(X,ODEVARA)
      CALL DATACOPY(X,ODEVARB)

C***  If RESID is called by SOLVE THEN IPAR(2)==ALL all residuals
C     are computed.  If RESID is called by DASSL residuals are not
C     computed for species.  Further, temperature profiles are only
C     updated when RESID is called by SOLVE.

      IF (IPAR(2).EQ.SOME) THEN
        UPDATE = 0
      ELSE
        UPDATE = 1
      END IF

      EPSP = RPAR(1)

      DO 1 I = 1, N
    1 QF(I) = XX0

C     CALCULATE FLOW DUE TO UNFORCED VENTS (HFLOW FOR HORIZONTAL FLOW
C     THROUGH VERTICAL VENTS AND VFLOW FOR VERTICAL FLOW THROUGH
C     HORIZONTAL VENTS)
      CALL HFLOW (TSEC,EPSP,NPROD,FLWNVNT)
      CALL VFLOW (TSEC,FLWHVNT,VFLOWFLG)
      CALL MVENT (TSEC,X(NOFPMV+1),X(NOFTMV+1),XPSOLVE(NOFTMV+1),
     .            FLWMV,DELTA(NOFPMV+1),DELTA(NOFTMV+1),
     .            XPRIME(NOFHVPR+1),NPROD,IERROR,HVACFLG,filtered)

      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF
C     CALCULATE HEAT AND MASS FLOWS DUE TO FIRES

      CALL FIRES (TSEC,FLWF,UPDATE)
      CALL SORTFR (NFIRE,IFROOM,XFIRE,IFRPNT,NM1)
      CALL DJET (FLWDJF,DJETFLG)

C     CALCULATE FLOW AND FLUX DUE TO HEAT TRANSFER (CEILING JETs, CONVECTION AND RADIATION

      CALL CJET (FLWCJET,FLXCJET)
      CALL CVHEAT (FLWCV,FLXCV)
      CALL RDHEAT (FLWRAD,FLXRAD,IERROR)
      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF

C     CALCULATE HCL DEPOSITION TO WALLS

      CALL HCL (FLWHCL, FLXHCL,IERROR)
      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF

C     RESET PARALLEL DATA STRUCTURES

      DO 20 I = 1, NM1
         QC(LL,I) = FLWCJET(I,LL) + FLWCV(I,LL)
         QC(UU,I) = FLWCJET(I,UU) + FLWCV(I,UU)
C     ADD IN VENT FIRES TO THE TOTAL.  DOFIRE DOES THE TOTAL OF
C     QF FOR NORMAL FIRES, BUT VENT FIRES ARE DONE AFTERWARDS WITH DJET
         DO 10 J = 1, NWAL
            QSCNV(J,I) = FLXCJET(I,J) + FLXCV(I,J)
   10    CONTINUE
   20 CONTINUE
      IF(DJETFLG)THEN
        DO 21 I = 1, NM1
         QF(I) = QF(I) + FLWDJF(I,Q,LL) + FLWDJF(I,Q,UU)
   21   CONTINUE
      ENDIF


C     SUM FLOW FOR INSIDE ROOMS

      DO 40 IROOM = 1, NIRM

        DO 30 IPROD = 1, NPROD + 2
          IP = IZPMAP(IPROD)
          FLWTOT(IROOM,IPROD,LL) = FLWNVNT(IROOM,IPROD,LL) +
     +        FLWF(IROOM,IP,LL)
          FLWTOT(IROOM,IPROD,UU) = FLWNVNT(IROOM,IPROD,UU) +
     +        FLWF(IROOM,IP,UU)
   30   CONTINUE
        IF(VFLOWFLG)THEN
          DO 31 IPROD = 1, NPROD + 2
          IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWHVNT(IROOM,IP,LL)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWHVNT(IROOM,IP,UU)
   31     CONTINUE
        ENDIF
        IF(HVACFLG)THEN
          DO 32 IPROD = 1, NPROD + 2
            IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWMV(IROOM,IP,LL) - filtered(iroom,iprod,ll)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWMV(IROOM,IP,UU) - filtered(iroom,iprod,uu)
   32     CONTINUE
        ENDIF
        IF(DJETFLG)THEN
          DO 33 IPROD = 1, NPROD + 2
            IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWDJF(IROOM,IP,LL)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWDJF(IROOM,IP,UU)
   33     CONTINUE
        ENDIF

C     ADD IN HCL CONTRIBUTION TO FLWTOT

        IF (ACTIVS(6)) THEN
           FLWTOT(IROOM,1,LL) = FLWTOT(IROOM,1,LL) + FLWHCL(IROOM,1,LL)
           FLWTOT(IROOM,1,UU) = FLWTOT(IROOM,1,UU) + FLWHCL(IROOM,1,UU)
           FLWTOT(IROOM,8,LL) = FLWTOT(IROOM,8,LL) + FLWHCL(IROOM,8,LL)
           FLWTOT(IROOM,8,UU) = FLWTOT(IROOM,8,UU) + FLWHCL(IROOM,8,UU)
        END IF

        FLWTOT(IROOM,Q,LL) = FLWTOT(IROOM,Q,LL) + FLWCV(IROOM,LL) +
     +      FLWRAD(IROOM,LL) + FLWCJET(IROOM,LL)
        FLWTOT(IROOM,Q,UU) = FLWTOT(IROOM,Q,UU) + FLWCV(IROOM,UU) +
     +      FLWRAD(IROOM,UU) + FLWCJET(IROOM,UU)

C*** IF THIS ROOM IS A SHAFT THEN SOLVE FOR ONLY ONE ZONE.
C    THIS IS DONE BY COMBINING FLOWS FROM TO BOTH
C    LAYERS INTO UPPER LAYER FLOW AND SETTING LOWER LAYER FLOW TO
C    ZERO.

        IF(IZSHAFT(IROOM).EQ.1)THEN
           DO 45 IPROD = 1, NPROD + 2
              FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     .                                 FLWTOT(IROOM,IPROD,LL)
              FLWTOT(IROOM,IPROD,LL) = XX0
   45      CONTINUE
        ENDIF

c*** calculate temperature of flow going into the upper layer
c    of each room

        IF(JACCOL.LE.0)THEN
          XQU = FLWTOT(IROOM,Q,UPPER)
          XMU = FLWTOT(IROOM,M,UPPER)
          IF(XMU.NE.0.0D0)THEN
            ZZFTEMP(IROOM,UPPER) = XQU/(CP*XMU)
           ELSE
            ZZFTEMP(IROOM,UPPER) = TAMB(IROOM)
          ENDIF
        ENDIF


   40 CONTINUE

C     SUM FLUX FOR INSIDE ROOMS

      DO 60 IROOM = 1, NIRM
        DO 50 IWALL = 1, NWAL
          IF (SWITCH(IWALL,IROOM)) THEN
            FLXTOT(IROOM,IWALL) = FLXCV(IROOM,IWALL) +
     +          FLXRAD(IROOM,IWALL) + FLXCJET(IROOM,IWALL)
          END IF
   50   CONTINUE
   60 CONTINUE

C     SET NPROD TO ZERO WHEN WE ARE ONLY SOLVING "SOME" OF THE ODE'S

      IF (IPAR(2).EQ.SOME) THEN
         NPRODSV = NPROD
         NPROD = 0
      END IF

C     CALCULATE RHS OF ODE'S FOR EACH ROOM

      DO 70 IROOM = 1, NIRM
         AROOM = AR(IROOM)
         HCEIL = HR(IROOM)
         PABS = ZZPABS(IROOM)
         HINTER = ZZHLAY(IROOM,LL)
         QL = FLWTOT(IROOM,Q,LL)
         QU = FLWTOT(IROOM,Q,UU)
         TMU = FLWTOT(IROOM,M,UU)
         TML = FLWTOT(IROOM,M,LL)

         IF(OPTION(FOXYGEN).EQ.ON)THEN
           OXYDU = FLWTOT(IROOM,4,UU)
           OXYDL = FLWTOT(IROOM,4,LL)
         ENDIF

C     PRESSURE EQUATION

         PDOT = (GAMMA-1.0D0) * (QL + QU) / (AROOM*HCEIL)

C     UPPER LAYER TEMPERATURE EQUATION

         TLAYDU = (QU-CP*TMU*ZZTEMP(IROOM,UU)) / (CP*ZZMASS(IROOM,UU))
         IF (OPTION(FODE).EQ.ON) THEN
            TLAYDU = TLAYDU + PDOT / (CP*ZZRHO(IROOM,UU))
         END IF

C     UPPER LAYER VOLUME EQUATION

         VLAYD = (GAMMA-1.0D0) * QU / (GAMMA*PABS)
         IF (OPTION(FODE).EQ.ON) THEN
            VLAYD = VLAYD - ZZVOL(IROOM,UU) * PDOT / (GAMMA*PABS)
         END IF
         IF(IZSHAFT(IROOM).EQ.1)VLAYD = XX0

C     LOWER LAYER TEMPERATURE EQUATION

         TLAYDL = (QL-CP*TML*ZZTEMP(IROOM,LL)) / (CP*ZZMASS(IROOM,LL))
         IF (OPTION(FODE).EQ.ON) THEN
            TLAYDL = TLAYDL + PDOT / (CP*ZZRHO(IROOM,LL))
         END IF
C
C*** IF THE LOWER LAYER IS SMALL AND GETTING SMALLER THEN WE MAKE IT STOP
C
C         IF(ZZVOL(IROOM,LL).LE.ZZVMIN(IROOM).AND.VLAYD.GT.0.0D0)THEN
C            VLAYD = 0.0D0
C            TLAYDL = 0.0D0
C         ENDIF
C
C*** IF THE UPPER LAYER IS BIG AND GETTING BIGGER THEN WE MAKE IT STOP
C
C         IF(ZZVOL(IROOM,UU).GT.ZZVMAX(IROOM).AND.VLAYD.LT.0.0D0)THEN
C            VLAYD = 0.0D0
C            TLAYDU = 0.0D0
C         ENDIF
C
         XPRIME(IROOM) = PDOT
         XPRIME(IROOM+NOFTL) = TLAYDL
         XPRIME(IROOM+NOFVU) = VLAYD
         XPRIME(IROOM+NOFTU) = TLAYDU

         IF(OPTION(FOXYGEN).EQ.ON)THEN
           XPRIME(IROOM+NOFOXYU) = OXYDU
           XPRIME(IROOM+NOFOXYL) = OXYDL
         ENDIF
   70 CONTINUE

C     COMPUTE PRODUCT OF COMBUSTION TERMS

      IF (NPROD.GT.0.AND.IPAR(2).EQ.ALL) THEN
         IPRODU = NOFPRD - 1
         DO 90 IPROD = 1, NPROD
            DO 80 IROOM = 1, NM1
            HCEIL = HR(IROOM)
            HINTER = ZZHLAY(IROOM,LL)
            IPRODU = IPRODU + 2
            IPRODL = IPRODU + 1
            PRODL = FLWTOT(IROOM,IPROD+2,LL)

C*** If this room is a hall and the jet has not reached the end
C    of the hall then don't solve for it using dassl

            PRODU = FLWTOT(IROOM,IPROD+2,UU)

            IF (HINTER.LT.HCEIL) THEN
               XPRIME(IPRODU) = PRODU
            ELSE IF (HINTER.GE.HCEIL.AND.FLWTOT(IROOM,M,UU).LT.XX0)
     +       THEN
               XPRIME(IPRODU) = PRODU
            ELSE
               XPRIME(IPRODU) = XX0
            END IF
            IF (HINTER.GT.XX0) THEN
               XPRIME(IPRODL) = PRODL
            ELSE IF (HINTER.LE.XX0.AND.FLWTOT(IROOM,M,LL).GT.XX0)
     +          THEN
               XPRIME(IPRODL) = PRODL
            ELSE
               XPRIME(IPRODL) = XX0
            END IF
   80     CONTINUE
   90   CONTINUE

C     HCL DEPOSITION.  NOTE THAT THESE ARE DONE ONLY IF HCLDEP IS SET

        IF (HCLDEP.NE.0) THEN
           IWHCL = NOFHCL
           DO 92 IROOM = 1, NM1
              DO 91 IWALL = 1, NWAL
                IWHCL = IWHCL + 1
                XPRIME(IWHCL) = FLXHCL(IROOM,IWALL)
  91          CONTINUE
  92       CONTINUE
        END IF

C     SMOKE DEPOSITION AND AGGLOMERATION.
C     NOTE THAT THESE ARE DONE ONLY IF SMKAGL IS SET

        DO 93 I = NOFSMKW + 1, NOFSMKW + 4 * NM1 * (SMKAGL+SMKAGL)
          XPRIME(I) = XX0
  93    CONTINUE
      END IF

C     RESIDUALS FOR PRESSURE

      DO 100 I = NOFP + 1, NOFP + NM1
         DELTA(I) = XPRIME(I) - XPSOLVE(I)
  100 CONTINUE

C       RESIDUALS FOR FLAME SPREAD

      IF (FSMTYPE.NE.0) THEN
         DELTA(NOFFSM+1) = DYPDT - XPSOLVE(NOFFSM+1)
         DELTA(NOFFSM+2) = DXPDT - XPSOLVE(NOFFSM+2)
         DELTA(NOFFSM+3) = DYBDT - XPSOLVE(NOFFSM+3)
         DELTA(NOFFSM+4) = DXBDT - XPSOLVE(NOFFSM+4)
         DELTA(NOFFSM+5) = DQDT -  XPSOLVE(NOFFSM+5)
      END IF

C     RESIDUALS FOR LAYER VOLUME, AND LAYER TEMPERATURES

      DO 105 I = NOFTU + 1, NOFTU + 3*NM1
         DELTA(I) = XPRIME(I) - XPSOLVE(I)
  105 CONTINUE

C     RESIDUAL FOR OXYGEN

      IF(OPTION(FOXYGEN).EQ.ON)THEN
        DO 106 I = 1, NM1
           DELTA(I+NOFOXYU) = XPRIME(I+NOFOXYU) - XPSOLVE(I+NOFOXYU)
           DELTA(I+NOFOXYL) = XPRIME(I+NOFOXYL) - XPSOLVE(I+NOFOXYL)
  106   CONTINUE
      ENDIF

C     CONDUCTION RESIDUAL

      CALL CNHEAT(UPDATE,DT,FLXTOT,DELTA)

C     target residual

      CALL TRHEAT(0,MPLICIT,DT,XPSOLVE,DELTA)

C     RESIDUALS FOR STUFF THAT IS SOLVED IN SOLVE ITSELF, AND NOT BY DASSL

      IF (NPROD.NE.0) THEN

C       RESIDUALS FOR GAS LAYER SPECIES
        DO 110 I = NOFPRD + 1, NOFPRD + 2*NPROD*NM1
          DELTA(I) = XPRIME(I) - XPSOLVE(I)
  110   CONTINUE

C       RESIDUALS FOR HCL DEPOSITION, SMOKE DEPOSITION AND SMOKE AGGLOMERATION
        DO 111 I = NOFHCL+1, NOFHCL + 4*NM1*(HCLDEP+SMKAGL+SMKAGL)
           DELTA(I) = XPRIME(I) - XPSOLVE(I)
  111   CONTINUE

C       RESIDUAL FOR hvac SPECIES
        DO 112 I = NOFHVPR+1, NOFHVPR+NLSPCT*NHVSYS
            DELTA(I) = XPRIME(I) - XPSOLVE(I)
  112   CONTINUE
      ENDIF

      IF (IPAR(2).EQ.SOME) THEN
        NPROD = NPRODSV
      END IF

      RETURN
      END

      SUBROUTINE DATACOPY(PDIF,IFLAG)


C     Routine:     DATACOPY

C     Description:  Calculate environment variables from the solver vector
C
C     Arguments: PDIF   Solver vector
C                IFLAG  Action flag:
C
C     IFLAG = CONSTVAR ==> Constant data (data that does not change 
C                          with time)
C     IFLAG = ODEVARA  ==> ODE variables: pressure, temperature and upper 
C                          layer volume
C     IFLAG = ODEVARB  ==> Species data and wall temperature profile.  
C                          Use pold and pdold to estimate species
C     IFLAG = ODEVARC  ==> Species data and wall temperature profile.
C                          Use pdif array for species
C
C     Revision History:
C        Created:  06/21/1990 by WWJ
C        Modified: 07/26/1990 by WWJ:
C                  Changed mapping to eliminate outside room fixed index in 
C                  wall species do loop
C        Modified: 03/23/1991 by WWJ:
C                  Fixed initialization error
C        Modified: 06/14/1992 by GPF:
C                  Add HVAC environment varibles for species and mass
C        Modified: 11/28/1992 by PAR:
C                  Changed th calculation of ZZCSPEC so that it is always 
C                  calculated
C        Modified: 11/28/1992 by PAR:
C                  Changed the manner in which VOLFRAC is calculated for
C                  VFLOW
C        Modified: 12/22/1992 by RDP:
C                  Added calculation of a "mass fraction" for CT for flow 
C                  calculation
C        Modified: 2/5/1993 by GPF and PAR:
C                  Added wall area environment variables and pointers
C                  into the Jacobian matrix in support of the reduced
C                  Jacobian option
C        Modified: 3/19/1993 by RDP:
C                  Added flag value ODEVARC to use pdif array directly for 
C                  species data.
C        Modified: 2/25/1994 by GPF:
C                  defined data structures for the room to room heat transfer
C                  option
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/1994 by GPF:
C                  define data structures used by target calculation
C        Modified: 4/25/95 by GPF:
C                  Remove ZZ variables no longer used (ZZxMIN, ZZxMAX where x=P,G,W).
C                  Added definition of iztarg,  For implicit targets defined temperature
C                  in xxtarg every iteration
C        Modified: 6/30/95 by GPF:
C                  if dassl oxygen option is active (OPTION(FOXYGEN).eq.ON) then
C                  get oxygen mass and concentration info from DASSL solver array
C                  rather than portion of array solved explicitly 
C        Modified: 7/13/95 by GPF:
C                  defined IZEQMAP and IZHVAC, two mapping arrays used for the new
C                  Jacobian speed up option
C        Modified: 8/15/95 by PAR:
C	             added setting the enviromental varibles for a flame spread
C	             type object.
C        Modified: 2/5/96 by GPF:  removed reduced jacobian option added on 2/5/93.
C        Modified: 2/9/96 by GPF:  fixed problems with flow withdrawel from a layer
C                                  when the layer is very small.
C        Modified: 3/19/96 by GPF:  fixed defn of lower layer product concentrations
C                                   when using shaft option
C        Modified: 7/22/96 by GPF: added data structure definitions for use by
C                                  the hall option
C        Modified:11/25/96 by GPF: added non-rectangular room option
C        Modified:2/14/97  by GPF: made memory requirements smaller by changing
C                                  several o(n**2) data structures to o(n)
C                                  Also, associated added wind pressure rise calculation
C                                  to vents instead of rooms (now we can have wind come in
C                                  one window of a room and go out another.
C        Modified:10/23/97 by GPF: defined ZZDISC array.  This array contains
C                                  discontinuity time points (now where vents
C                                  change opening areas.
!	   Modified: 8/05 by wwj; extend compartment pairwise connections to 25
C        
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "vents.fi"
      include "wnodes.fi"
      include "dervs.fi"
      include "params.fi"
      include "wdervs.fi"
      include "opt.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"

C     ORDER OF VARIABLES IS DEFINED IN THE ROUTINE OFFSET

      DIMENSION PDIF(*)
      INTEGER FRMASK(mxccv)

      XX0 = 0.0D0
      XX2 = 2.0D0
      XX1 = 1.0D0
      VMINFRAC = 1.0D-4
      IF (IFLAG.EQ.CONSTVAR) THEN
        DO 10 IROOM = 1, N
          ZZVMIN(IROOM) = MIN(VMINFRAC * VR(IROOM), XX1)
          ZZVMAX(IROOM) = VR(IROOM) - ZZVMIN(IROOM)
   10   CONTINUE
        DO 20 IROOM = 1, NM1
          ZZYFLOR(IROOM) = HFLR(IROOM)
          ZZYCEIL(IROOM) = HRP(IROOM)

C*** DEFINE WALL CENTERS

          XX = bR(IROOM)
          XX2 = XX/2.0D0
          YY = dR(IROOM)
          YY2 = YY/2.0D0
          ZZ = HRP(IROOM)
          ZZWCEN(IROOM,1,1) = XX2
          ZZWCEN(IROOM,1,2) = YY2
          ZZWCEN(IROOM,1,3) = ZZ

          ZZWCEN(IROOM,2,1) = XX2
          ZZWCEN(IROOM,2,2) = YY

          ZZWCEN(IROOM,3,1) = XX
          ZZWCEN(IROOM,3,2) = YY2

          ZZWCEN(IROOM,4,1) = XX2
          ZZWCEN(IROOM,4,2) = 0.0D0

          ZZWCEN(IROOM,5,1) = 0.0D0
          ZZWCEN(IROOM,5,2) = YY2

          ZZWCEN(IROOM,6,1) = XX2
          ZZWCEN(IROOM,6,2) = YY

          ZZWCEN(IROOM,7,1) = XX
          ZZWCEN(IROOM,7,2) = YY2

          ZZWCEN(IROOM,8,1) = XX2
          ZZWCEN(IROOM,8,2) = 0.0D0

          ZZWCEN(IROOM,9,1) = 0.0D0
          ZZWCEN(IROOM,9,2) = YY2

          ZZWCEN(IROOM,10,1) = XX2
          ZZWCEN(IROOM,10,2) = YY2
          ZZWCEN(IROOM,10,3) = 0.0D0

   20   CONTINUE
        ZZYFLOR(N) = 0.0D0
        ZZYCEIL(N) = 100000.D0
        ZZVOL(N,UPPER) = 0.0D0
        ZZVOL(N,LOWER) = 100000.0D0
        ZZHLAY(N,UPPER) = 0.0D0
        ZZHLAY(N,LOWER) = 100000.0D0
        ZZRELP(N) = XX0
        ZZPABS(N) = POFSET
        ZZTEMP(N,UPPER) = 300.0D0
        ZZTEMP(N,LOWER) = 300.0D0
        DO 30 LSP = 3, NS
          ZZCSPEC(N,UPPER,LSP) = XX0
          ZZCSPEC(N,LOWER,LSP) = XX0
          zzgspec(n,lower,lsp) = xx0
          zzgspec(n,upper,lsp) = xx0
   30   CONTINUE
        ZZCSPEC(N,UPPER,1) = 0.770D0
        ZZCSPEC(N,LOWER,1) = 0.770D0
        ZZCSPEC(N,UPPER,2) = 0.230D0
        ZZCSPEC(N,LOWER,2) = 0.230D0
        DO 40 LAYER = UPPER, LOWER
          ZZRHO(N,LAYER) = ZZPABS(N) / RGAS / ZZTEMP(N,LAYER)
          ZZMASS(N,LAYER) = ZZRHO(N,LAYER) * ZZVOL(N,LAYER)
   40   CONTINUE

C*** DEFINE VENT DATA STRUCTURES

	  do 41 i = 1, mxccv
   41   frmask(i) = 2 ** i
        NVENTS = 0
        DO 70 I = 1, NM1
          DO 60 J = I + 1, N
            IF (NW(I,J).NE.0) THEN
              DO 50 K = 1, mxccv
                IF (IAND(FRMASK(K),NW(I,J)).NE.0) THEN
                  NVENTS = NVENTS + 1
                  IIJK = IJK(I,J,K)
                  ZZVENT(NVENTS,1) = HL(IIJK)
                  ZZVENT(NVENTS,2) = HH(IIJK)
                  ZZVENT(NVENTS,3) = BW(IIJK)
                  ZZVENT(NVENTS,4) = HALLDIST(IIJK,1)
                  ZZVENT(NVENTS,5) = HALLDIST(IIJK,2)
                  IZVENT(NVENTS,1) = I
                  IZVENT(NVENTS,2) = J
                  IZVENT(NVENTS,3) = K

C*** is "from" room a hall?

                  IF(IZHALL(I,IHROOM).EQ.1)THEN
                    IZVENT(NVENTS,4) = 1
                   ELSE
                    IZVENT(NVENTS,4) = 0
                  ENDIF

C*** is "to" room a hall?

                  IF(IZHALL(J,IHROOM).EQ.1)THEN
                    IZVENT(NVENTS,5) = 1
                   ELSE
                    IZVENT(NVENTS,5) = 0
                  ENDIF

!	add face (vface) to the data structure

				izvent(nvents,6) = vface(iijk)

C*** compute pressure rise due to wind.  This value is only defined for outside rooms

                  WCOS = WINDC(IIJK)
                  IF(J.EQ.N.AND.WCOS.NE.XX0)THEN

C*** compute wind velocity and pressure rise at the average vent height

                    HAVG = (ZZVENT(NVENTS,1)+ZZVENT(NVENTS,2))/2.0D0 
                    HAVG = HAVG + ZZYFLOR(I) 

C*** we used to compute wind speed at the floor.  If we use this height
C    then we get identical answers to previous CFAST calculations
C                   HAVG = ZZYFLOR(I)

                    IF(WINDRF.NE.XX0)THEN
                      WINDVNEW = WINDV*(HAVG/WINDRF)**WINDPW
                     ELSE
                      WINDVNEW = WINDV
                    ENDIF
                    WINDDP = WCOS*EXRA*WINDVNEW**2/2.0D0
                    ZZVENT(NVENTS,6) = WINDDP
                   ELSE
                    ZZVENT(NVENTS,6) = XX0
                  ENDIF

                END IF
   50         CONTINUE
            END IF
   60     CONTINUE
   70   CONTINUE

C*** define vents for vertical flow

        NVVENT = 0
        DO 90 I = 1, N
          DO 80 J = 1, N
            IF (NWV(I,J).NE.0) THEN
              NVVENT = NVVENT + 1
              IVVENT(NVVENT,1) = I
              IVVENT(NVVENT,2) = J
			qcvv(1,nvvent) = qcvpp(1,i,j)
			qcvv(2,nvvent) = qcvpp(2,i,j)
			qcvv(3,nvvent) = qcvpp(3,i,j)
			qcvv(4,nvvent) = qcvpp(4,i,j)
            END IF
   80     CONTINUE
   90   CONTINUE
C*** define discontinuity array.  first we look at vent openings

        XDELT = NSMAX / DELTAT
        ITSTOP = XDELT + 1
        TSTOP = ITSTOP - 1

        ZZDISC(0) = 0.0D0
	  zzdisc(1) = tstop
        III = 1
!	Add each of the change arrays to the discontinuity list
	  do  i = 1, nvents
		 iii = iii + 1
		 zzdisc(iii) = qcvh(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvh(3,i)
        enddo
	  do  i = 1, nvvent
		 iii = iii + 1
		 zzdisc(iii) = qcvv(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvv(3,i)
	  enddo
	  do  i = 1, nfan
		 iii = iii + 1
		 zzdisc(iii) = qcvm(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvm(3,i)
	  enddo
	  do i = 1, nfilter
		 iii = iii + 1
		 zzdisc(iii) = qcvf(1,i)
		 iii = iii + 1
	     zzdisc(iii) = qcvf(3,i)
	  end do
        IZNDISC = III

!	Put the discontinuity array into order
	  call shellsort (zzdisc(0), izndisc+1)

C*** DEFINE IZWMAP FOR JAC AND OTHER CONSTANTS FOR THE CUSTOM LINEAR
C***   ALGEBRA ROUTINES THAT ARE CALLED IN DASSL

        ICOL = 0
        IEQ = NOFWT
c     set izwmap2 for the outside room first
        DO 112 IWALL = 1,4
          IZWMAP2(IWALL,NM1+1) = 0
  112   CONTINUE
        DO 110 IROOM = 1, NM1
          ICNT = 0
          IZNWALL(IROOM) = 0
          DO 100 IWALL = 1, 4
            IF (SWITCH(IWALL,IROOM)) THEN
              IEQ = IEQ + 1
              IZWMAP2(IWALL,IROOM) = IEQ
              ICNT = ICNT + 1
              ICOL = ICOL + 1
              IZNWALL(IROOM) = IZNWALL(IROOM) + 1

c*** define izwall, to describe ceiling-floor connections
c    first assume that walls are connected to the outside

              II = IEQ - NOFWT
              IZWALL(II,1) = IROOM
              IZWALL(II,2) = IWALL
              IZWALL(II,3) = NM1 + 1
              IF(IWALL.EQ.1.OR.IWALL.EQ.2)THEN
                IWFAR = 3 - IWALL
               ELSE
                IWFAR = IWALL
              ENDIF
              IZWALL(II,4) = IWFAR
              IZWALL(II,5) = IWBOUND

            ELSE
              IZWMAP2(IWALL,IROOM) = 0
            END IF
  100     CONTINUE
          IZWMAP(1,IROOM) = ICOL - ICNT + 1
          IZWMAP(2,IROOM) = ICNT
  110   CONTINUE

c*** update izwall for ceiling/floors that are connected 

        DO 115 I = 1, NSWAL
          IFROMR = IZSWAL(I,1)
          IFROMW = IZSWAL(I,2)
          ITOR = IZSWAL(I,3)
          ITOW = IZSWAL(I,4)
          IEQFROM = IZWMAP2(IFROMW,IFROMR) - NOFWT
          IEQTO = IZWMAP2(ITOW,ITOR) - NOFWT

          IZWALL(IEQFROM,3) = ITOR
          IZWALL(IEQFROM,4) = ITOW
          IZWALL(IEQFROM,5) = 1

          IZWALL(IEQTO,3) = IFROMR
          IZWALL(IEQTO,4) = IFROMW
          IZWALL(IEQTO,5) = 1

  115   CONTINUE 

        JACN1 = NOFPMV - NOFP
        JACN2 = NOFWT - NOFPMV
        JACN3 = NOFPRD - NOFWT
        JACDIM = JACN1 + JACN2 + JACN3

c*** define maps for dassl eqs <--> target data structures

        IEQ = 0
        DO 300 ITARG = 1, NTARG
           IF(IXTARG(TRGMETH,ITARG).EQ.MPLICIT)THEN
             IEQ = IEQ + 1
             IZTARG(ITARG) = IEQ
            ELSE
             IZTARG(ITARG) = 0
           ENDIF
  300   CONTINUE

C*** associate an equation type (ie pressure, temperature etc as defined by offsets)
C    with each dassl equation

        IEQ = 0
        DO 320 ITYPE = 1, NEQOFF
          IBEG = NOFSETS(ITYPE)
          IEND = NOFSETS(ITYPE+1)-1
          DO 330 I = IBEG, IEND
            IEQ = IEQ + 1
            IROOM = I + 1 - IBEG
            IZEQMAP(IEQ,1) = ITYPE
            IZEQMAP(IEQ,2) = IROOM
  330     CONTINUE
  320   CONTINUE

C*** indicate which rooms are connected to an hvac system

         DO 340 I = 1, NM1
           IZHVAC(I) = .FALSE.
  340    CONTINUE
         DO 350 II = 1, NEXT
            I = HVNODE(1,II)
            IZHVAC(I) = .TRUE.
  350    CONTINUE

      ELSE IF (IFLAG.EQ.ODEVARA) THEN
        DO 130 IROOM = 1, NM1
          ZZVOL(IROOM,UPPER) = MAX(PDIF(IROOM+NOFVU),ZZVMIN(IROOM))
          ZZVOL(IROOM,UPPER) = MIN(ZZVOL(IROOM,UPPER),ZZVMAX(IROOM))
          ZZVOL(IROOM,LOWER) = 
     +        MAX(VR(IROOM)-ZZVOL(IROOM,UPPER),ZZVMIN(IROOM))
          ZZVOL(IROOM,LOWER) = MIN(ZZVOL(IROOM,LOWER),ZZVMAX(IROOM))

C*** prevent flow from being withdrawn from a layer if the layer
C    is at the minium size

		VOLFRU(IROOM) = (ZZVOL(IROOM,UPPER)-VMINFRAC*VR(IROOM))/ 
     .                    VR(IROOM)*(1.0D0-2.0D0*VMINFRAC)
          VOLFRU(IROOM) = MAX(MIN(VOLFRU(IROOM),XX1),XX0)
          VOLFRL(IROOM) = 1.0D0 - VOLFRU(IROOM)
          VOLFRL(IROOM) = MAX(MIN(VOLFRL(IROOM),XX1),XX0)

c*** calculate layer height for non-rectangular rooms

          NPTS = IZRVOL(IROOM)
          IF(NPTS.EQ.0)THEN
            ZZHLAY(IROOM,UPPER) = ZZVOL(IROOM,UPPER) / AR(IROOM)
            ZZHLAY(IROOM,LOWER) = ZZVOL(IROOM,LOWER) / AR(IROOM)
           ELSE
            CALL INTERP(ZZRVOL(1,IROOM),ZZRHGT(1,IROOM),NPTS,
     .                  ZZVOL(IROOM,LOWER),1,ZZHLAY(IROOM,LOWER))
            ZZHLAY(IROOM,UPPER) = HR(IROOM) - ZZHLAY(IROOM,LOWER)            
          ENDIF

          ZZRELP(IROOM) = PDIF(IROOM)
          ZZPABS(IROOM) = PDIF(IROOM) + POFSET
          ZZTEMP(IROOM,UPPER) = PDIF(IROOM+NOFTU)
          ZZTEMP(IROOM,LOWER) = PDIF(IROOM+NOFTL)

C*** There is a problem with how flow is being withdrawn from layers
C    when the layers are small and the flow is large (for example with
C    ceiling vents.  As a result, DASSL, can predict a negative TEMPERATURE
C    (because the rhs of the temperature equation is wrong).  The following
C    code causes the temperature of the opposite layer to be used in these
C    situations.

          IF(ZZTEMP(IROOM,UPPER).LT.0.0D0)THEN
            ZZTEMP(IROOM,UPPER)=ZZTEMP(IROOM,LOWER)
          ENDIF
          IF(ZZTEMP(IROOM,LOWER).LT.0.0D0)THEN
            ZZTEMP(IROOM,LOWER)=ZZTEMP(IROOM,UPPER)
          ENDIF

          IF(IZSHAFT(IROOM).EQ.1)THEN
             ZZTEMP(IROOM,LOWER) = ZZTEMP(IROOM,UPPER)
          ENDIF

C*** COMPUTE AREA OF 10 WALL SEGMENTS

          XX = bR(IROOM)
          YY = dR(IROOM)
          ZZU = ZZHLAY(IROOM,UPPER)
          ZZL = ZZHLAY(IROOM,LOWER)
          ZZWAREA2(IROOM,1) = AR(IROOM)
          ZZWAREA2(IROOM,2) = ZZU*XX
          ZZWAREA2(IROOM,3) = ZZU*YY
          ZZWAREA2(IROOM,4) = ZZU*XX
          ZZWAREA2(IROOM,5) = ZZU*YY
          ZZWAREA2(IROOM,6) = ZZL*XX
          ZZWAREA2(IROOM,7) = ZZL*YY
          ZZWAREA2(IROOM,8) = ZZL*XX
          ZZWAREA2(IROOM,9) = ZZL*YY
          ZZWAREA2(IROOM,10) = AR(IROOM)

C*** COMPUTE AREA OF 4 WALL SEGMENTS

          ZZWAREA(IROOM,1) = AR(IROOM)
          ZZWAREA(IROOM,2) = AR(IROOM)
          ZZWAREA(IROOM,3) = (YY + XX)*ZZU * XX2
          ZZWAREA(IROOM,4) = MAX(XX0,(YY+XX)*ZZL*XX2)

C*** DEFINE Z WALL CENTERS (THE Z COORDINATE CHANGES WITH TIME)
C    (OTHER COORDINATES ARE STATIC AND ARE DEFINED EARLIER)

          DO 116 I = 1, 4
             YLAY = ZZHLAY(IROOM,LOWER)
             ZZWCEN(IROOM,I+1,3) =  (ZZYCEIL(IROOM)+YLAY)/2.0D0
             ZZWCEN(IROOM,I+5,3) = YLAY/2.0D0
  116     CONTINUE

          DO 120 LAYER = UPPER, LOWER
            ZZRHO(IROOM,LAYER) = ZZPABS(IROOM) / RGAS / 
     +          ZZTEMP(IROOM,LAYER)
            ZZMASS(IROOM,LAYER) = ZZRHO(IROOM,LAYER) * 
     +          ZZVOL(IROOM,LAYER)
  120     CONTINUE
  130   CONTINUE

C*** RECORD WHICH LAYER TARGET IS IN

        DO 135 ITARG = 1, NTARG
           IROOM = IXTARG(TRGROOM,ITARG)
           YLAY = ZZHLAY(IROOM,LOWER)
           YTARG = XXTARG(TRGCENZ,ITARG)
           IF(YTARG.GE.YLAY)THEN
              IXTARG(TRGLAYER,ITARG) = UPPER
             ELSE
              IXTARG(TRGLAYER,ITARG) = LOWER
           ENDIF
  135   CONTINUE

c*** stuff dassl estimate of target temperature's solved implicitly
c    (ie solved by dassl)

        DO 310 ITARG = 1, NTARG
          IF(IXTARG(TRGMETH,ITARG).NE.MPLICIT)GO TO 310
          IEQ = IZTARG(ITARG)
          XXTARG(TRGTEMPF,ITARG) = P(IEQ+NOFTT)
  310   CONTINUE

C*** define surface wall temperatures (interior=1,exterior=2)

      ELSE IF (IFLAG.EQ.ODEVARB.OR.IFLAG.EQ.ODEVARC) THEN
        ISOF = NOFWT
        DO 150 IROOM = 1, NM1
          DO 140 IWALL = 1, NWAL
            IWALLEQ = IZWMAP2(IWALL,IROOM)
            IF(IWALLEQ.NE.0)THEN
              IEQFROM = IWALLEQ - NOFWT
              IFROMR = IZWALL(IEQFROM,1)
              IFROMW = IZWALL(IEQFROM,2)
              ITOR = IZWALL(IEQFROM,3)
              ITOW = IZWALL(IEQFROM,4)
              ZZWTEMP(IROOM,IWALL,1) = PDIF(IWALLEQ)
              IWALLEQ2 = IZWMAP2(ITOW,ITOR)
              IINODE = NUMNODE(1,IWALL,IROOM)
              IF(IWALLEQ2.EQ.0)THEN
                ZZWTEMP(IROOM,IWALL,2) = TWJ(IINODE,IROOM,IWALL)
               ELSE
                ZZWTEMP(IROOM,IWALL,2) = PDIF(IWALLEQ2)
              ENDIF
             ELSE

c*** if we're not solving for the wall temperature then set it
c    to the layer temperature that it is adjacent too.  Note,
c    zzwtemp(iroom,iwall,2) is only referenced if the iwall'th
c    wall in room iroom is being solved with the heat equation
c
              IF(IWALL.EQ.1.OR.IWALL.EQ.3)THEN
                ILAY = UPPER
               ELSE
                ILAY = LOWER
              ENDIF
              ZZWTEMP(IROOM,IWALL,1) = ZZTEMP(IROOM,ILAY)
            ENDIF
  140     CONTINUE
  150   CONTINUE

C*** DEFINE SPECIES amounts

        ISOF = NOFPRD
        DO 170 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            DO 160 IROOM = 1, NM1
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPGAS = PDIF(ISOF)
              END IF
              ZZGSPEC(IROOM,UPPER,LSP) = MAX(PPGAS,XX0)
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPGAS = PDIF(ISOF)
              END IF
              ZZGSPEC(IROOM,LOWER,LSP) = MAX(PPGAS,XX0)
  160       CONTINUE
          END IF
  170   CONTINUE

C     DEFINE SPECIES mass fractions: normalize to total product mass 
C     rather than total mass (this is equivalent to what was begin done 
C     in chemie)

        DO 200 IROOM = 1, NM1
          TOTL = XX0
          TOTU = XX0
          DO 180 LSP = 1, MIN(9,NS)
            IF (ACTIVS(LSP)) THEN
              TOTU = TOTU + ZZGSPEC(IROOM,UPPER,LSP)
              TOTL = TOTL + ZZGSPEC(IROOM,LOWER,LSP)
            END IF
  180     CONTINUE
          RTOTL = 1.0D0
          RTOTU = 1.0D0
          IF (TOTL.GT.XX0) RTOTL = 1.0D0 / TOTL
          IF (TOTU.GT.XX0) RTOTU = 1.0D0 / TOTU
          DO 190 LSP = 1, NS
            IF (ACTIVS(LSP)) THEN
              ZZCSPEC(IROOM,UPPER,LSP) = ZZGSPEC(IROOM,UPPER,LSP) * 
     +            RTOTU
              ZZCSPEC(IROOM,LOWER,LSP) = ZZGSPEC(IROOM,LOWER,LSP) * 
     +            RTOTL
              IF(IZSHAFT(IROOM).EQ.1)THEN
                ZZCSPEC(IROOM,LOWER,LSP) = ZZCSPEC(IROOM,UPPER,LSP)
              ENDIF
            END IF
  190     CONTINUE

c*** if oxygen is a dassl variable then use dassl solve array to define
c    zzgspec and zzcspec values for oxygen.
C    make sure oxygen never goes negative

          IF(OPTION(FOXYGEN).EQ.ON)THEN
             OXYL = MAX(P(IROOM+NOFOXYL),XX0)
             OXYU = MAX(P(IROOM+NOFOXYU),XX0)
             ZZGSPEC(IROOM,LOWER,2) = OXYL
             ZZGSPEC(IROOM,UPPER,2) = OXYU
             ZZCSPEC(IROOM,LOWER,2) = OXYL/
     .                                ZZMASS(IROOM,LOWER)
             ZZCSPEC(IROOM,UPPER,2) = OXYU/
     .                                ZZMASS(IROOM,UPPER)
             IF(IZSHAFT(IROOM).EQ.1)THEN
               ZZCSPEC(IROOM,LOWER,2) = ZZCSPEC(IROOM,UPPER,2)
             ENDIF
          ENDIF
  200   CONTINUE

C     define hcl absorption

        IF (ACTIVS(6)) THEN
          ISOF = NOFHCL
          DO 220 IROOM = 1, NM1
            DO 210 LSP = 1, NWAL
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPWGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPWGAS = PDIF(ISOF)
              END IF
              ZZWSPEC(IROOM,LSP) = PPWGAS
  210       CONTINUE
  220     CONTINUE
        END IF
      END IF

C     copy hvac product values for each hvac system

      IF (NHVSYS.NE.0.AND.NS.NE.0) THEN
        ISOF = NOFHVPR
        DO 230 ISYS = 1, NHVSYS
          ZZHVM(ISYS) = XX0
  230   CONTINUE
        DO 250 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            DO 240 ISYS = 1, NHVSYS
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPHV = MAX(XX0,POLD(ISOF)+DT*PDOLD(ISOF))
              ELSE
                PPHV = MAX(XX0,PDIF(ISOF))
              END IF
              ZZHVPR(ISYS,LSP) = PPHV
              ZZHVM(ISYS) = ZZHVM(ISYS) + ZZHVPR(ISYS,LSP)
  240       CONTINUE
          END IF
  250   CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE RESYNC(PDIF,IBEG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RESYNC
C
C     Source File: RESYNC.SOR
C
C     Functional Class:   
C                        
C
C     Description:  Routine to resyncronize the total mass of the
C                   species with that of the total mass
C
C     Arguments: PDIF   The P array to resync
C                IBEG   The point at which species are started in P array
C
C     Revision History:
C        Created:  12/1/1992 at 12:39 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      DIMENSION PDIF(*)
      DIMENSION FACTOR(NR,2)
C
      XX0 = 0.0D0
      DO 10 IROOM = 1,NM1
         FACTOR(IROOM,UPPER) = XX0
         FACTOR(IROOM,LOWER) = XX0
   10 CONTINUE

      ISOF = IBEG
      DO 20 IPROD = 1, MIN(NS,9)
         IF (ACTIVS(IPROD)) THEN
            DO 30 IROOM = 1, NM1
               FACTOR(IROOM,UPPER) = FACTOR(IROOM,UPPER) + PDIF(ISOF)
               ISOF = ISOF + 1
               FACTOR(IROOM,LOWER) = FACTOR(IROOM,LOWER) + PDIF(ISOF)
               ISOF = ISOF + 1
   30       CONTINUE
         ENDIF
   20 CONTINUE
      DO 60 IROOM = 1, NM1
         IF (FACTOR(IROOM,UPPER).GT.XX0.AND.
     .                     ZZMASS(IROOM,UPPER).GT.XX0) THEN
           FACTOR(IROOM,UPPER) = ZZMASS(IROOM,UPPER)/FACTOR(IROOM,UPPER)
         ELSE
            FACTOR(IROOM,UPPER) = 1.0D0
         END IF
         IF (FACTOR(IROOM,LOWER).GT.XX0.AND.
     .                     ZZMASS(IROOM,LOWER).GT.XX0) THEN
           FACTOR(IROOM,LOWER) = ZZMASS(IROOM,LOWER)/FACTOR(IROOM,LOWER)
         ELSE
            FACTOR(IROOM,LOWER) = 1.0D0
         END IF
   60 CONTINUE

      ISOF = IBEG
      DO 50 IPROD = 1, MIN(NS,9)
        IF (ACTIVS(IPROD)) THEN
          DO 40 IROOM = 1, NM1
            PDIF(ISOF) = PDIF(ISOF) * FACTOR(IROOM,UPPER)
            ISOF = ISOF + 1
            PDIF(ISOF) = PDIF(ISOF) * FACTOR(IROOM,LOWER)
            ISOF = ISOF + 1
   40     CONTINUE
        END IF
   50 CONTINUE

      RETURN
      END
      
      SUBROUTINE Get_rev_cfast (rev_number, rev_date)
     
     
      INTEGER,INTENT(INOUT) :: REV_NUMBER
      CHARACTER(255),INTENT(INOUT) :: REV_DATE
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: mainid='$Id$'
      CHARACTER(255), PARAMETER :: mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      WRITE(MODULE_DATE,'(A)') maindate
      
      end subroutine Get_rev_cfast

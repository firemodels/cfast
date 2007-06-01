      SUBROUTINE INITSLV
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSLV
C
C     Source File: INITSLV.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: 
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:28 by GPF:
C                  Added parameters for new initialization
C                  heuristic and various parameters for hvac
C                  (error tolerances, convective loss through
C                  duct walls)
C        Modified: 2/3/93 by GPF
C                  Added parameters in support of the reduced Jacobian
C                  option.  Added debug print option to print out
C                  SNSQE progress
C        Modified: 6/30/95 by GPF
C                  Reallocated points in wall, adding more points to the rear.
C                  Added oxygen dassl option flag.
C        Modified: 10/16/97 by gpf
C                  made STPMAX initializations consistent
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "params.fi"
      include "iofiles77.fi"

      LOGICAL EXISTED

C     THIS GUY IS IN UNLABELED COMMON SO WE CAN NOT PUT IT INTO A DATA STATEMENT
      DUCTCV = 0.0D0

      INQUIRE (FILE=solverini,EXIST=EXISTED)
      IF (.NOT.EXISTED) return
      CLOSE (IOFILI)
	write (logerr, 1) solverini
	OPEN (IOFILI,FILE=solverini)

C*** READ IN SOLVER ERROR TOLERANCES

      READ (IOFILI,*)
      READ (IOFILI,*) APTOL, RPTOL, ATOL, RTOL
      READ (IOFILI,*)
      READ (IOFILI,*) AWTOL, RWTOL, ALGTOL
      READ (IOFILI,*)
      READ (IOFILI,*) AHVPTOL, RHVPTOL, AHVTTOL, RHVTTOL

C     READ IN PHYSICAL SUB-MODEL OPTION LIST

      READ (IOFILI,*)
      READ (IOFILI,*) NOPT
      NOPT = MAX(0, MIN(MXOPT, NOPT))
      DO 10 I = 1, (NOPT-1) / 5 + 1
        IBEG = 1 + (I-1) * 5
        IEND = MIN(IBEG+4,NOPT)
        READ (IOFILI,*)
        READ (IOFILI,*) (OPTION(J),J = IBEG,IEND)
   10 CONTINUE
C     SINCE THE SOLVER.INI FILE IS ON, TURN ON DEBUG HELP
      OPTION(FKEYEVAL) = 1

C     SET DEBUG PRINT

      IF (OPTION(FDEBUG).EQ.2) THEN
         OPTION(FDEBUG) = OFF
         SWITCH(1,NR) = .TRUE.
      ELSE IF (OPTION(FDEBUG).GE.3) THEN
         OPTION(FDEBUG) = ON
         SWITCH(1,NR) = .TRUE.
      END IF

C     READ IN WALL INFO

      READ (IOFILI,*)
      READ (IOFILI,*) NWPTS, FRACT1, FRACT2, FRACT3
      READ (IOFILI,*)
      READ (IOFILI,*) IWBOUND
      FSUM = ABS(FRACT1) + ABS(FRACT2) + ABS(FRACT3)
      WSPLIT(1) = ABS(FRACT1) / FSUM
      WSPLIT(2) = ABS(FRACT2) / FSUM
      WSPLIT(3) = ABS(FRACT3) / FSUM

C     READ IN MAXIMUM DESIRED SOLVE STEP SIZE, 
C     IF NEGATIVE THEN THEN SOLVE WILL DECIDE

      READ (IOFILI,*)
      READ (IOFILI,*) STPMAX, DASSLFTS

C*** read in hvac convection coefficient

      READ(IOFILI,*)
      READ(IOFILI,*) DUCTCV

C*** READ IN JACOBIAN AND SNSQE PRINT FLAGS

      READ(IOFILI,*)
      READ(IOFILI,*) JACCHK, CUTJAC, IPRTALG
      CLOSE (IOFILI)

	RETURN

    1	FORMAT ('***** Modify dassl tolerances with ',a256)
      END

      BLOCKDATA INITSLVB

C     THIS INITIALIZES THE SOLVER VARIABLES

      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "params.fi"

C     ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL, REL OTHER TOL
      DATA APTOL, RPTOL, ATOL, RTOL/1.0D-6, 1.0D-6, 1.0D-5, 1.0D-5/
C     ABS WALL TOL, REL WALL TOL
      DATA AWTOL, RWTOL, ALGTOL/1.0D-2, 1.0D-2, 1.0D-8/
C     ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, REL HVAC TEMP
      DATA AHVPTOL,RHVPTOL,AHVTTOL,RHVTTOL/1.0D-6,1.0D-6,1.0D-5,1.0D-5/
C     OPTIONS FIRE, HFLOW, ENTRAIN, VFLOW, CJET, DOOR-FIRE, CONVEC, RAD,
      DATA (OPTION(J),J=1,21)/ 2, 1, 1, 1, 2, 1, 1, 2, 
C         CONDUCT, DEBUG, EXACT ODE,  HCL , MFLOW, KEYBOARD, 
     +        1,     0,     1,         1,     1,      1,
C         TYPE OF INITIALIZATION,   MV HEAT LOSS, MOD JAC, DASSL DEBUG
     +        1,                       0,          1,     0,
C         OXYGEN DASSL SOLVE, BACK TRACK ON DTECT,  BACK TRACK ON OBJECTS
     .        0,                       0,                 0    /
C     NUMBER OF WALL NODES, FRACTIONS FOR FIRST, MIDDLE AND LAST WALL SLAB
      DATA NWPTS /30/
C     BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE, 2=INSULATED 3=FLUX)
      DATA IWBOUND /3/
C     COMPUTED VALUES FOR BOUNDARY THICKNESS
      DATA (WSPLIT(J),J=1,3)  /0.50, 0.17, 0.33/
C     TURN DEBUGGING OPTIONS OFF - THIS IS NOT CURRENTLY USED
      DATA DEBUG /MXOPT*0/
C     MAXIMUM STEP SIZE, IF NEGATIVE THEN SOLVER WILL DECIDE
      DATA STPMAX /1.0D0/, DASSLFTS/0.005D0/
C
      DATA JACCHK/0/, CUTJAC/0.0D0/, IPRTALG/0/
      END

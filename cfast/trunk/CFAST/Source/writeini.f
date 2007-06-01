      SUBROUTINE WRITEINI(FILE)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WRITEINI
C
C     Source File: WRITEINI
C
C     Functional Class:  
C
C     Description:  this routine creates a solver.ini file for the current
C                   version of CFAST.  It is created using:
C                   cfast -s filename
C                   where filename is the name of the file to contain
C                   the solver.ini options .  The default name is 
C                   SOLVE.INI (so as to not overwrite SOLVER.INI if
C                   it is present)
C
C     Arguments: 
C
C     Revision History:
C        Created:  10/21/97 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "params.fi"

      CHARACTER*(*) FILE
      INTEGER FUNIT

      NNNOPT = 21

      IUNIT = FUNIT(70)
      OPEN(UNIT=IUNIT,FILE=FILE)

      WRITE(IUNIT,10)
   10 FORMAT(' ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL,',
     .       ' REL OTHER TOL')
      WRITE (IUNIT,11) APTOL, RPTOL, ATOL, RTOL
   11 FORMAT(1X,5(1PG11.4,1X))

      WRITE(IUNIT,20)
   20 FORMAT(' ABS WALL TOL, REL WALL TOL, INITIALIZATION TOLERANCE')
      WRITE (IUNIT,11) AWTOL, RWTOL, ALGTOL

      WRITE(IUNIT,30)
   30 FORMAT(' ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, ',
     .       ' REL HVAC TEMP')
      WRITE (IUNIT,11) AHVPTOL, RHVPTOL, AHVTTOL, RHVTTOL

      WRITE(IUNIT,40)
   40 FORMAT(' NUMBER OF PHYSICAL OPTION FLAGS')
      WRITE (IUNIT,*) NNNOPT

      WRITE(IUNIT,50)
   50 FORMAT(' FIRE,      HFLOW,  ENTRAIN, VFLOW,       CJET')
      WRITE (IUNIT,*) (OPTION(J),J = 1,5)

      WRITE(IUNIT,60)
   60 FORMAT(' DOOR-FIRE, CONVEC, RAD,     CONDUCT, DEBUG PRINT  ')
      WRITE (IUNIT,*) (OPTION(J),J = 6,10)

      WRITE(IUNIT,70)
   70 FORMAT(' EXACT ODE, HCL,   MFLOW,    KEYBOARD, ',
     .        'TYPE OF INITIALIZATION')
      WRITE (IUNIT,*) (OPTION(J),J = 11,15)

      WRITE(IUNIT,80)
   80 FORMAT(' MV HEAT LOSS, USE MODIFIED JACOBIAN, DASSL DEBUG, ',
     .       'OXYGEN SOLVE    DETECTORS')
      WRITE (IUNIT,*) (OPTION(J),J = 16,20)

      WRITE(IUNIT,90)
   90 FORMAT(' OBJECT BACKTRACKING')
      WRITE (IUNIT,*) (OPTION(J),J = 21,21)

      WRITE(IUNIT,100)
  100 FORMAT(' NUMBER OF WALL NODES, FRACTIONS FOR FIRST, ',
     .       'MIDDLE AND LAST WALL SLAB')
      WRITE (IUNIT,101) NWPTS, (WSPLIT(I),I=1,3)
  101 FORMAT(1X,I3,1X,3(1PG11.4,1X))

      WRITE(IUNIT,110)
  110 FORMAT(' BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE,',
     .       '   2=INSULATED 3=FLUX)')
      WRITE (IUNIT,*) IWBOUND

      WRITE(IUNIT,120)
  120 FORMAT(' MAXIMUM STEP SIZE,  MAX FIRST STEP - ',
     .       ' IF EITHER <0 THEN SOLVER DECIDES')
      WRITE (IUNIT,11) STPMAX, DASSLFTS

      WRITE(IUNIT,130)
  130 FORMAT(' HVAC CONVECTION COEFFICIENT')
      WRITE(IUNIT,11) DUCTCV

      WRITE(IUNIT,140)
  140 FORMAT(' JAC CHECK (>0 CHECK JACOBIAN), JACOBIAN CUTOFF, ',
     .       '  SNSQE PRINT (1=ON)')
      WRITE(IUNIT,141) JACCHK, CUTJAC, IPRTALG
  141 FORMAT(1X,I3,1X,1PG11.4,I3)

      IF(1.EQ.1)STOP
      RETURN
      END

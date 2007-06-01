      SUBROUTINE INITWALL(TSTOP,IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITWALL
C
C     Description:  This routine initializes data structures associated
C                   with walls and targets
C
C     Arguments: TSTOP
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  by gpf
C        Modified: 2/25/1994 by gpf:
C          Redefine wall slab properties for walls that connect 
C          interior rooms.  For example, if the ceiling in room 1
C          is connected to the floor of room 2 then this routine
C          will concatenate the slab property variables for conductivity,
C          specific heat etc.  The heat transfer can then be calculated
C          using the same routine, CNDUCT, but with the new slab variables.
C        Modified: 10/10/1994 by gpf:
C          Added initialization of target data structures.  Targets
C          are just thin walls.
C        Modified: 6/14/1995 by gpf:
C          Added exterior temperature parameter to WSET so that the temperature
C          profile is defined to be a linear ramp between the interior and exterior
C          wall surfaces temperature.
C        Modified: 6/30/1995 by gpf:
C          added CSHELL include file
C        Modified: 9/5/1995 at 9:57 by PAR:
C                  Added support for IERROR and returning stops to main
!		Modified 2/25/5 to use direct properties lookup
C
C        FKW = THERMAL CONDUCTIVITY
C        CW = SPECIFIC HEAT (J/KG)
C        RW = DENSITY OF THE WALL (KG/M**3)
C        FLW = THICKNESS OF THE WALL (M)
C        EPW = EMMISIVITY OF THE WALL
C        NSLB = DISCRETIZATION OF THE WALL SLABS (NUMBER OF NODES)
C        CNAME CONTAINS THE NAME OF THE THERMAL DATA SUBSET IN THE tpp datafile 
C        SWITCH IS A LOGICAL FOR THERMAL CALCULATION ON/OFF
C        THSET IS A SWITCH FOR A PROPERLY TRANSFERRED DATA SET
C        MAXCT IS A COUNT OF THE NUMBER OF tpp DATA SETs in the database
C        SWITCH IS SET IF CALCULATION IS CALLED FOR
C        THSET IS SET IF A NAME IN THE LIST OF REQUESTED DATA SETS matches ONE OF THE NAMES IN THE LIST OF DATA SET NAMES (NLIST).
C        THE DATA FROM THE DATA BASE IS STORED IN THE LOCAL VARIABLES LFKW,LCW,LRS,LFLW AND LEPW AND IS TRANSFERRED TO FKW...

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "thermp.fi"
      include "fltarget.fi"

      CHARACTER OFF*8, NONE*8, tcname*8
!	TP is the pointer into the data base for each material
      INTEGER tp

      DATA OFF /'OFF'/, NONE /'NONE'/

C     MAP THE THERMAL DATA INTO ITS APPROPRIATE WALL SPECIFICATION
C     IF NAME IS "OFF" OR "NONE" THEN JUST TURN ALL OFF

	DO 170 I = 1, NWAL
      DO 160 J = 1, NM1
         THSET(I,J) = .FALSE.
         IF (SWITCH(I,J)) THEN
            IF (CNAME(I,J).EQ.OFF.OR.CNAME(I,J).EQ.NONE) THEN
               SWITCH(I,J) = .FALSE.
               GO TO 160
            END IF
            CALL GETTPP(CNAME(I,J),tp,IERROR)
            IF (IERROR.NE.0) RETURN
            NSLB(I,J) = lnslb(tp)
            DO 140 K = 1, NSLB(I,J)
               FKW(K,I,J) = lfkw(k,tp)
               CW(K,I,J) = lcw(k,tp)
               RW(K,I,J) = lrw(k,tp)
               FLW(K,I,J) = lflw(k,tp)
  140       CONTINUE
            EPW(I,J) = lepw(tp)
            DO 150 K = 1, 7
               HCLBF(K,I,J) = lhclbf(k,tp)
  150       CONTINUE
         END IF
  160 CONTINUE
  170 CONTINUE

!	Initialize the interior temperatures to the interior ambient

      DO 270 I = 1, NM1
      DO 260 J = 1, NWAL
         TWE(J,I) = ETA(I)
         DO 250 K = 1, NN 
            TWJ(K,I,J) = TAMB(I)
  250    CONTINUE
  260 CONTINUE
  270 CONTINUE

C*** initialize temperature profile data structures

      DO 20 I = 1, NM1
        DO 10 J = 1, NWAL
          IF (SWITCH(J,I)) THEN
            CALL WSET(NUMNODE(1,J,I),NSLB(J,I),TSTOP,WALLDX(1,I,J),
     +          WSPLIT,FKW(1,J,I),CW(1,J,I),RW(1,J,I),FLW(1,J,I),
     +          WLENGTH(I,J),TWJ(1,I,J),TAMB(I),ETA(I))
          END IF
   10   CONTINUE
   20 CONTINUE

C*** concatenate slab properties of wall nodes that are connected
C    to each other

      DO 30 I = 1, NSWAL
         IFROMR = IZSWAL(I,1)
         IFROMW = IZSWAL(I,2)
         ITOR = IZSWAL(I,3)
         ITOW = IZSWAL(I,4)

         NSLABF = NSLB(IFROMW,IFROMR)
         NSLABT = NSLB(ITOW,ITOR)
         NSLB(IFROMW,IFROMR) = NSLABF + NSLABT
         NSLB(ITOW,ITOR) = NSLABF + NSLABT

         NPTSF = NUMNODE(1,IFROMW,IFROMR)
         NPTST = NUMNODE(1,ITOW,ITOR)
         NUMNODE(1,ITOW,ITOR) = NPTSF + NPTST - 1
         NUMNODE(1,IFROMW,IFROMR) = NPTSF + NPTST - 1

         WFROM = WLENGTH(IFROMR,IFROMW)
         WTO = WLENGTH(ITOR,ITOW)
         WLENGTH(IFROMR,IFROMW) = WFROM + WTO
         WLENGTH(ITOR,ITOW) = WFROM + WTO

         JJ = NSLABT + 1
         DO 40 J = NSLABF+1, NSLABF+NSLABT
            JJ = JJ - 1
            FKW(J,IFROMW,IFROMR) = FKW(JJ,ITOW,ITOR)
             CW(J,IFROMW,IFROMR) =  CW(JJ,ITOW,ITOR)
             RW(J,IFROMW,IFROMR) =  RW(JJ,ITOW,ITOR)
            FLW(J,IFROMW,IFROMR) = FLW(JJ,ITOW,ITOR)
             NUMNODE(J+1,IFROMW,IFROMR) = NUMNODE(JJ+1,ITOW,ITOR)
   40    CONTINUE

         JJ = NSLABF + 1
         DO 50 J = NSLABT+1, NSLABT+NSLABF
            JJ = JJ - 1
            FKW(J,ITOW,ITOR) = FKW(JJ,IFROMW,IFROMR)
            CW(J,ITOW,ITOR) =  CW(JJ,IFROMW,IFROMR)
            RW(J,ITOW,ITOR) =  RW(JJ,IFROMW,IFROMR)
            FLW(J,ITOW,ITOR) = FLW(JJ,IFROMW,IFROMR)
            NUMNODE(J+1,ITOW,ITOR) = NUMNODE(JJ+1,IFROMW,IFROMR)
   50    CONTINUE

         JJ = NPTST 
         DO 60 J = NPTSF+1,NPTSF+NPTST - 1
            JJ = JJ - 1
            TWJ(J,IFROMR,IFROMW) = TWJ(JJ,ITOR,ITOW)
            WALLDX(J-1,IFROMR,IFROMW) = WALLDX(JJ,ITOR,ITOW)
   60    CONTINUE

         JJ = NPTSF 
         DO 70 J = NPTST+1,NPTST+NPTSF - 1
            JJ = JJ - 1
            TWJ(J,ITOR,ITOW) = TWJ(JJ,IFROMR,IFROMW)
            WALLDX(J-1,ITOR,ITOW) = WALLDX(JJ,IFROMR,IFROMW)
   70    CONTINUE

   30 CONTINUE

C*** INITIALIZE TARGET DATA STRUCTURES

      DO 100 ITARG = 1, NTARG
        TCNAME = CXTARG(ITARG)
        IF(TCNAME.EQ.' ')THEN
          TCNAME = 'DEFAULT'
          CXTARG(ITARG) = TCNAME
        ENDIF
	  ICODE = 0
	  CALL GETTPP(TCNAME,tp,IERROR)
        IF (IERROR.NE.0) RETURN
        XXTARG(TRGK,ITARG) = lfkw(1,tp)
        XXTARG(TRGCP,ITARG) = lcw(1,tp)
        XXTARG(TRGRHO,ITARG) = lrw(1,tp)
        XXTARG(TRGL,ITARG) = lflw(1,tp)
        XXTARG(TRGEMIS,ITARG) = lepw(tp)
  100 CONTINUE

      RETURN
      END

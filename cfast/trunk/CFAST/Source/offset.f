      SUBROUTINE OFFSET (IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OFFSET
C
C     Source File: OFFSET.SOR
C
C     Functional Class:  
C
C     Description:  
C     Offset in the following context is the beginning of the vector for
C     that particular variable minus one.  Thus, the actual pressure array
C     goes from NOFP+1 to NOFP+nm1.  The total number of equations to be
C     considered is NEQUALS, and is the last element in the last vector.
C     Each physical interface routine is responsible for the COUNT of the
C     number of elements in the vector for which it is resonsible.

C     This set of parameters is set by NPUTP and is kept in the environment
C     common block CENVIRO.INC.  To index a variable, the list is something
C     like (for temperature in this case)

C     NOFTU+1, NOFTU+NM1

C     The structure of the solver array is

C     NOFP = offset for the main pressure; the array of base pressures for each compartment
C     NOFPMV = offset for HVAC node pressuers
C     NOFTMV = offset for HVAC branch temperatures
C     NOFTU = upper layer temperature
C     NOFVU = upper layer volume
C     NOFTL = lower layer temperature
C     NOFTT = target temperatures
C     NOFWT = wall surface temperatures (equivalent to the number of profiles)
C     NOFPRD = species
C     NOFHCL = surface deposition of hydrogen chloride
C     NOFSMKW = surface deposition of soot
C     NOFSMK = gas phase agglomeration of soot
C     NEQUALS = last element in the array.

C     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

C     An important note - solve sets the last variable to be solved to NOFPRD
C     which is the beginning of the species (-1) and the end of the array which
C     is presently used by DASSL.
C
C     Arguments: IERROR  Returns error codes
C
C     Revision History:
C
C     created May 19, 1992
C             June 14, 1992 added offsets for HVAC duct temperatures
C        Modified: 4/26/1995 gpf:
C                  added offset parameter, NOFTT, for implicit targets
C        Modified: 6/30/1995 gpf:
C                  added oxygen offsets
C        Modified: 8/15/1995 par:
C                  added flame spread offsets.  Fixed for LFBT = 0 to
C	             treated as a type 2 fire.
C        Modified: 9/5/1995 at 10:12 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "params.fi"
      include "wnodes.fi"
      include "fltarget.fi"
      include "opt.fi"
      include "objects2.fi"

C     COUNT THE OF NODES (LARGEST OF NS AND NE)

      NNODE = MAX(NA(1),NE(1))
      DO 50 IB = 2, NBR
        NNODE = MAX(NNODE,NA(IB),NE(IB))
   50 CONTINUE
      IF (NNODE.GT.MNODE) THEN
#ifndef pp_dll
        CALL XERROR('OFFSET - Node range exceeded for HVAC',0,1,1)
#endif
        IERROR = 16
        RETURN
      END IF

C     SET THE NUMBER OF COMPARTMENTS AND OFFSETS

      NM1 = N - 1

C     COUNT THE SPECIES 

      NLSPCT = 0

      IF (LFBT.EQ.1) THEN
         DO 90 I = 1, NS
           IF (ALLOWED(I).AND.ACTIVS(I)) THEN
              NLSPCT = NLSPCT + 1
           END IF
   90    CONTINUE
      ELSE IF (LFBT.EQ.2.OR.LFBT.EQ.0) THEN
         DO 110 I = 1, NS
            IF (ALLOWED(I)) THEN
               IF (ACTIVS(I)) THEN
                  NLSPCT = NLSPCT + 1
               END IF
            ELSE IF (I.NE.7) THEN
               NLSPCT = NLSPCT + 1
            END IF
  110    CONTINUE
         NLSPCT = NLSPCT + 1
      ELSE
         STOP ' NOT AN ALLOWED FIRE TYPE'
      ENDIF

C     COUNT THE NUMBER OF WALLS

      NWALLS = 0
      DO 270 I = 1, NM1
      DO 260 J = 1, NWAL
         IF (SWITCH(J,I)) THEN
            NWALLS = NWALLS + 1
         END IF
         IF (NWPTS.NE.0) NUMNODE(1,J,I) = NWPTS
  260 CONTINUE
  270 CONTINUE

C     count the number of implicit targets

      NIMTARG = 0
      NEQTARG(MPLICIT) = 0
      NEQTARG(STEADY) = 0
      NEQTARG(XPLICIT) = 0
      DO 300 ITARG = 1, NTARG
         IF(IXTARG(TRGMETH,ITARG).EQ.MPLICIT)THEN
            NIMTARG = NIMTARG + 1
            NEQTARG(MPLICIT) = NEQTARG(MPLICIT) + 1
           ELSEIF(IXTARG(TRGMETH,ITARG).EQ.STEADY)THEN
            NEQTARG(STEADY) = NEQTARG(STEADY) + 1
           ELSEIF(IXTARG(TRGMETH,ITARG).EQ.XPLICIT)THEN
            NEQTARG(XPLICIT) = NEQTARG(STEADY) + 1
         ENDIF
  300 CONTINUE

C    SET NUMBER OF IMPLICIT OXYGEN VARIABLES

C*** note we never let dassl solve for oxygen when we have a type 1 fire

      IF(LFBT.EQ.1)OPTION(FOXYGEN) = OFF
      IF(OPTION(FOXYGEN).EQ.ON)THEN
         NOXYGEN = NM1
        ELSE
         NOXYGEN = 0
      ENDIF

C	SET NUMBER OF FLAMESPREAD EQAATIONS

	IF (FSMTYPE.GT.0) THEN
	  NFSM = FSMTYPE*5
	ELSE
	  NFSM = 0
	END IF
      
C     NOW DO ALL THE EQUATION OFFSETS

      NHVPVAR = NNODE - NEXT
      NHVTVAR = NBR
      NOFP = 0
      NOFPMV = NOFP + NM1
      NOFTMV = NOFPMV + NHVPVAR
	NOFFSM = NOFTMV + NHVTVAR
	NOFTU = NOFFSM + NFSM
      NOFVU = NOFTU + NM1
      NOFTL = NOFVU + NM1
      NOFOXYL = NOFTL + NM1
      NOFOXYU = NOFOXYL + NOXYGEN
      NOFTT = NOFOXYU + NOXYGEN
      NOFWT = NOFTT + NIMTARG
      NOFPRD = NOFWT + NWALLS
      NOFHCL = NOFPRD + 2 * NM1 * NLSPCT
      NOFSMKW = NOFHCL + 4 * NM1 * HCLDEP
      NOFSMK = NOFSMKW + 4 * NM1 * SMKAGL
      NOFHVPR = NOFSMK + 4 * NM1 * SMKAGL
  
C     If the hvac model is used then nequals needs to be redefined in 
C     HVMAP since the variable NHVSYS is not defined yet.  After NHVSYS 
C     is defined the following statement can be used to define nequals

C     NEQUALS = NOFHVPR + NHVSYS*NLSPCT

      NEQUALS = NOFHVPR

      RETURN
      END

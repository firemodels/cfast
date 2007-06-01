      SUBROUTINE HVSFLO (TPRIME, DELTTMV)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVSFLO
C
C     Source File: HVSFLO.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: TPRIME
C                DELTTMV
C
C     Revision History:
C
C     June 14, 1992 This routine now only computes the 
C                   differential equation for temperature.  this
C                   equation is now solved by dassl.  The de's
C                   for smoke (species) are now performed in hvtoex.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "opt.fi"

      DIMENSION TPRIME(*), DELTTMV(*)

      XX0 = 0.0D0
      DO 41 IB = 1, NBR
         DELTTMV(IB) = ROHB(IB)*HVDVOL(IB)*TPRIME(IB)/GAMMA
   41 CONTINUE

      DO 30 I = 1, NNODE

C     CALCULATE TEMPERATURES & SMOKE FLOWS IN FOLLOWING LOOP AT THE
C     CONNECTING NODES

         HVTA = XX0
         FLOWIN = XX0
         DO 20 J = 1, NCNODE(I)
            IF (HVFLOW(I,J).GT.XX0) THEN
               FLOWIN = FLOWIN + HVFLOW(I,J)
               IB = ICMV(I,J)
               HVTEMP = HVFLOW(I,J)
               HVTA = HVTA + HVTEMP*TBR(IB)
            ENDIF
   20    CONTINUE
         IF (FLOWIN.GT.XX0) THEN
            HVTA = HVTA / FLOWIN
         ELSE
            DO 21 II = 1, NEXT
               IF (HVNODE(2,II).EQ.I) THEN
                  HVTA = HVEXTT(II,UPPER)
                  GO TO 22
               ENDIF
   21       CONTINUE

C           THIS IS A BAD SITUATION.  WE HAVE NO FLOW, YET MUST CALCULATE
C           THE INFLOW CONCENTRATIONS.  IF THERE IS FLOW, THEN THIS
C           SECTION WILL BE SKIPPED.

            HVTA = TBR(1)
   22       CONTINUE
         ENDIF
C
C     NOW CALCULATE THE RESULTING TEMPERATURE AND CONCENTRATIONS
C     IN THE DUCTS AND FANS
C
         DO 40 J = 1, NCNODE(I)
         IF (HVFLOW(I,J).LT.XX0) THEN
            IB = ICMV (I,J)
            DELTTMV(IB) = DELTTMV(IB) - (HVTA-TBR(IB))*ABS(HVFLOW(I,J))
            IF(OPTION(FHVLOSS).EQ.ON)THEN
               DELTTMV(IB) = DELTTMV(IB) + 
     .                       CHV(IB)*(TBR(IB)-TAMB(1))*HVDARA(IB)
            ENDIF
         ENDIF
         II = IZHVIE(IN(I,J))
         IF(II.NE.0.AND.HVFLOW(I,J).GT.XX0) THEN
            IB = ICMV(I,J)
            HVTA = HVEXTT(II,UPPER)
            DELTTMV(IB) = DELTTMV(IB) - (HVTA-TBR(IB))*HVFLOW(I,J)
            IF(OPTION(FHVLOSS).EQ.ON)THEN
               DELTTMV(IB) = DELTTMV(IB) + 
     .                       CHV(IB)*(TBR(IB)-TAMB(1))*HVDARA(IB)
            ENDIF
         ENDIF
   40    CONTINUE
   30 CONTINUE
      RETURN
      END

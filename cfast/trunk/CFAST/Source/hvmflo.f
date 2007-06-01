      SUBROUTINE HVMFLO (tsec, DELTPMV,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVMFLO
C
C     Source File: HVMFLO.SOR
C
C     Functional Class:  
C
C     Description:  Routine for mass flow solution & calculation
C
C     Arguments: DELTPMV
C                IERROR   Returns error codes
C
C     Revision History:
C     May 15, 1991 set the convergence criterion to 0.01 rather than 0.00001
C                  also set dpz to zero.  The rational is that the static
C                  pressure differences should not matter in the flow.
C     June 14, 1992 essentially re-written. Formerly this routine used
C                   a newton like procedure to compute mass flow in each
C                   portion of the hvac network.  Now this routine computes
C                   mass conservation error.  The newton iteration is performed
C                   by dassl to reduce the the above error to nearly zero
C        Modified: 9/5/1995 at 9:53 by PAR:
C                  Added support of IERROR and returning stops to main
C        Modified: 2/5/96 by GPF:
C                  work around fix to HVAC density initialization using
C                  ambient pressure rather than average duct pressure
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"

      DIMENSION DELTPMV(*)
	double precision tsec

C     CALCULATE AVERAGE TEMPERATURES AND DENSITIES FOR EACH BRANCH
C 
      XX1 = 1.0D0
      XX2 = 2.0D0
      XX0 = 0.0D0

      XX1O2 = XX1 / XX2
      DO 9 IB = 1, NBR
C         PAV = (HVP(NA(IB)) + HVP(NE(IB)))*XX1O2 + POFSET
C     fix 1/30/96 to solve pressure initialization in the beginning
         PAV = POFSET
         ROHB(IB) = PAV/(HVRGAS*TBR(IB))
         BFLO(IB) = XX1
    9 CONTINUE

C     START THE ITERATION CYCLE

      NITER = 2
      DO 1 ITER = 1, NITER

C     INITIALIZE CONDUCTANCE

      DO 11 IB = 1, NBR
         CE(IB)=XX0
   11 CONTINUE

!     CONVERT FROM PRESSURE TO MASS FLOW RATE COEFFICIENTS

      DO 40 IB = 1, NBR
         IF (CE(IB).NE.XX0) THEN
            XTEMP = XX1 / SQRT(ABS(CE(IB)))
            CE (IB) = SIGN(XTEMP, CE(IB))
         ENDIF
   40 CONTINUE

C     CALCULATE HYDROSTATIC PRESSURE DIFFERENCE TERMS

      DO 70 I = 1, NNODE
      DO 70 J = 1, NCNODE(I)
         DPZ(I,J) = ROHB(ICMV(I,J))*HVGRAV*(HVGHT(IN(I,J)) - HVGHT(I))
   70 CONTINUE

C     Find mass flow for each branch and mass residual at each node

      DO 30 I = 1, NNODE
         F = 0.0D0
            DO 25 J = 1, NCNODE(I)
               DP = HVP(IN(I,J)) - HVP(I) + DPZ(I,J)
               IF (NF(ICMV(I,J)).EQ.0) THEN

C        RESISTIVE BRANCH CONNECTION 

                  HVFLOW(I,J) = SIGN(CE(ICMV(I,J))*SQRT(ABS(DP)), DP)
                  BFLO(ICMV(I,J)) = ABS(HVFLOW(I,J))
                 ELSE

C        FAN BRANCH CONNECTION

                  K = NF(ICMV(I,J))
                  IF(NE(ICMV(I,J)) .NE. I)THEN
C*** FLOW IS AT FAN INLET
                     HVFLOW(I,J) = -HVFAN(tsec,I,J,K,DP)
                    ELSE
C*** FLOW IS AT FAN EXIT
                     DP = -DP
                     HVFLOW(I,J) = HVFAN(tsec,I,J,K,DP)
                  ENDIF
               ENDIF
               F = F + HVFLOW(I,J)
               II = IZHVIE(IN(I,J))
               IF(II.NE.0)HVFLOW(IN(I,J),1) = -HVFLOW(I,J)
   25       CONTINUE
         ii = IZHVMAPE(I)
         IF(II.GT.0) DELTPMV(ii) = F
   30 CONTINUE
    1 CONTINUE

      RETURN
      END

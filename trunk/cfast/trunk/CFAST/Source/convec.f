      SUBROUTINE CONVEC(IW,TG,TW,QDINL)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CONVEC
C
C     Source File: CONVEC.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Calculate convective heat transfer for a wall segment
C                   Note that we have simplified the convection calculation
C                   by assuming turbulent flow.  This allows us to remove the
C                   dependency on the characterisitic length and avoid a divide
C                   by zero as the surface vanishes.  If a more general
C                   relationship is desired, the code will have to be reworked
C                   to include the characteristic length in the calculation.
C
C     Arguments: IW     Wall number, standand CFAST numbering convention
C                TG     Temperature of gas layer adjacent to wall surface
C                TW     Wall surface temperature
C                QDINL  Convective flux into wall surface IW
C
C     Revision History:
C        Created:  by WWJ
C        Modified: 1/2/1987 at 16:19 by WWJ:
C                  change number of surfaces to be consistent with four-wall 
C                  numbering. QDINAL is now multiplied by (TG-TW). This was 
C                  missing.
C        Modified: 1/4/1993 at 16:20 by RDP & GPF:
C                  changed calculation to be consistent with SPFE Handbook 
C                  chapter on convective heat transfer. Calculation of thermal 
C                  properties and correlations come from there.  Smooth
C                  transition between correlations added with TANH.  
C                  Vertical tangent in Nusselt number was eliminated (when
C                  gas and wall temperatures are close) in order to improve
C                  numerical characteristics.  Note this has little effect
C                  on flux calculation.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
#ifdef pp_double     
      DOUBLE PRECISION NUOVERL, K
#else
      REAL NUOVERL, K
#endif
      LOGICAL FIRST
      SAVE FIRST, G, X1DEL, XTHIRD, TDEL, XXHALF
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         G = 9.80D0
         TDEL = 5.0D0
         X1DEL = 1.0D0 / TDEL
         XTHIRD = 1.0D0 / 3.0D0
         XXHALF = 0.50D0
      END IF
C
C
      QDINL = 0.0D0
      TF = (TW+TG) * XXHALF
C
C*** To keep K positive, make sure TF is below 3000.  Of course the
C    calculation will have long since lost any semblance to reality.
C
      T3000 = 3000.0D0
      TFF = MIN(TF,T3000)
      IF (TF.LE.0.0D0) RETURN
      ALPHA = 1.D-9 * TF ** (1.75D0)
      K = (0.0209D0+2.33D-5*TFF) / (1.D0-2.67D-4*TFF)
      PR = 0.72D0
C
C     CEILINGS AND FLOORS
C     Use the hyperbolic tangent to smooth the coefficient C 
C     from CUP to CDOWN over a temperature range of TDEL degress. 
C     Note: Tanh(x>>0)=1 and Tanh(x<<0)=-1 .
C
      CUP = 0.16D0
      CDOWN = 0.13D0
      IF (IW.EQ.1) THEN
        C = (CUP+CDOWN+(CUP-CDOWN)*TANH((TG-TW)*X1DEL)) * XXHALF
      ELSE IF (IW.EQ.2) THEN
        C = (CUP+CDOWN-(CUP-CDOWN)*TANH((TG-TW)*X1DEL)) * XXHALF
C
C     VERTICAL SURFACES
C
      ELSE
        C = 0.121D0
      END IF
C
C     Prevent the vertical tangent in the calculation of NUOVERL
C     by keeping ABSTWTG from going to zero.  
C
      ABSTWTG = ABS(TW-TG)
      IF(ABSTWTG.LT.TDEL)ABSTWTG = TDEL
      NUOVERL = C * (G*ABSTWTG*PR/(TF*ALPHA**2)) ** XTHIRD
C
      QDINL = NUOVERL * K * (TG-TW)
      RETURN
      END

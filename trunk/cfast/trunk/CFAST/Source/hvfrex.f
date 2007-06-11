      SUBROUTINE HVFREX (tsec, HVPSOLV, HVTSOLV)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVFREX
C
C     Source File: HVFREX.SOR
C
C     Functional Class:  
C
C     Description:  UPDATE ARRAYS AND ASSIGN COMPARTMENT PRESSURES, TEMPERATURES AND
C                   CONCENTRATIONS TO EXTERIOR NODES
C
C     Arguments: HVPSOLV
C                HVTSOLV
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 05/15/1991 at 10:15 by WWJ:
C                  Add a test for the outside.  This is done differently
C                  with cfast than was done for fast
C        Modified: 06/14/1992 at 10:16 by GPF:
C                  Compare effective temperature using harmonic
C                  rather than arithmetic averages.  define branch
C                  temperatures and node pressures using values
C                  supplied by the dae solver dassl
C        Modified: 11/30/1992 at 10:32 by PAR:
C                  Changed the calculation of HVCVEN to reflect the 
C                  concentration of the layer the flow is actually coming out of
C        Modified: 11/30/1992 at 10:33 by PAR:
C                  Changed the calculation of the HVFRAC to make sure the vent 
C                  area is in the compartment
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      DIMENSION HVPSOLV(*), HVTSOLV(*)
	double precision tsec, xx1, xx0

!
!    INITIALIZE THE PARAMETERS USED BY THE MECHANICAL VENTILATION ROUTINES

      XX0 = 0.0D0
      XX1 = 1.0D0
      XXTENTH = 0.1D0
      XX0P5 = 0.5D0

      DO 10 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        Z = ZZHLAY(I,LOWER)
        IF (HVORIEN(II).EQ.1) THEN
C     WE HAVE AN OPENING WHICH IS ORIENTED VERTICALLY - USE A SMOOTH CROSSOVER
C     FIRST, CALCULATE THE SCALING LENGTH OF THE DUCT
           XXL = SQRT(AREXT(II))
        ELSE
           XXL = SQRT(AREXT(II)) * XXTENTH
        ENDIF
C     THEN THE BOTTOM OF THE VENT (ABOVE THE FLOOR)
        XXLL = MAX(XX0,MIN((HVELXT(II) - XX0P5 * XXL),(HR(I)-XXL)))
C     THESE ARE THE RELATIVE FRACTION OF THE UPPER AND LOWER 
C     LAYER THAT THE DUCT "SEES" THESE PARAMETERS GO FROM 0 TO 1
        FRACTION = MAX(XX0,MIN(XX1,MAX(XX0,(Z-XXLL)/XXL)))
        HVFRAC(UPPER,II) = MIN(XX1,MAX(XX1-FRACTION,XX0))
        HVFRAC(LOWER,II) = MIN(XX1,MAX(FRACTION,XX0))
   10 CONTINUE

C     THIS IS THE ACTUAL DUCT INITIALIZATION

      DO 30 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        IF (I.LT.N) THEN
          Z = ZZHLAY(I,LOWER)
          ZL = MIN(Z,HVELXT(II))
          ZU = MIN(XX0,HVELXT(II)-ZL)
          RU = ZZRHO(I,UPPER)
          RL = ZZRHO(I,LOWER)
          HVP(J) = ZZRELP(I) - (RU*ZU+RL*ZL) * HVGRAV
          HVEXTT(II,UPPER) = ZZTEMP(I,UPPER)
          HVEXTT(II,LOWER) = ZZTEMP(I,LOWER)
        ELSE
          HVEXTT(II,UPPER) = EXTA
          HVEXTT(II,LOWER) = EXTA
          HVP(J) =  EXPA - EXRA * HVGRAV * HVELXT(II)
        END IF
        DO 20 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            IF (I.LT.N) THEN
               HVEXCN(II,LSP,UPPER) = ZZCSPEC(I,UPPER,LSP)
               HVEXCN(II,LSP,LOWER) = ZZCSPEC(I,LOWER,LSP)
            ELSE
               XXRHO = O2N2(LSP) * EXRA
               HVEXCN(II,LSP,UPPER) = XXRHO
               HVEXCN(II,LSP,LOWER) = XXRHO
            END IF
          END IF
   20   CONTINUE
   30 CONTINUE
      DO 40 I = 1, NHVPVAR
         II = IZHVMAPI(I)
         HVP(II) = HVPSOLV(I)
   40 CONTINUE 
      DO 41 IB = 1, NHVTVAR
         TBR(IB) = HVTSOLV(IB)
   41 CONTINUE
      RETURN
      END

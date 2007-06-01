      SUBROUTINE HVINIT (IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVINIT
C
C     Source File: HVINIT.SOR
C
C     Functional Class:  
C
C     Description:  THIS ROUTINE SETS UP THE ARRAYS NEEDED TO FOR HVAC
C                   SIMULATION AND INITIALIZES TEMPERATURES AND CONCENTRATIONS
C
C     Arguments: IERROR  Returns error codes
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 05/15/1991 at 10:19 by WWJ:
C                  Add initialization for the outside connections
C        Modified: 06/14/1992 at 10:21 by GPF:
C                  Added Initialization of various mapping arrays.  define surface area
C                  of each duct (used to compute heat loss through duct walls).
C        Modified: 11/28/1992 at 10:35 by GPF:
C                  Changed the initilization of the extrnal nodes connected to 
C                  the outside to use the correct values
C        Modified: 9/5/1995 at 9:50 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

C
C     THIS FUNCTION HAS BEEN MODIFIED TO PREVENT NEGATIVE FLOW.  A MAX FUNCTION
C     WAS INSERTED JUST AFTER THE CALCULATION OFF, THE FLOW TO DO THIS.  IF
C     THE FLOW IS ALLOWED TO BE NEGATIVE (FLOW REVERSAL) THEN THIS STATEMENT
C     MUST BE REMOVED.

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "cshell.fi"

      DIMENSION C3(NS)
  
C    CALCULATE MIN & MAX VALUES FOR FAN CURVE

      X1 = 1.0D0
      X0 = 0.0D0
      PI = 4.0D0 * ATAN(X1)
C          
      DO 20 K = 1, NFAN
        F = HVBCO(K,1)
        DF = X0
        XX = X1
        DO 10 J = 2, NFC(K)
          XXJM1 = J - 1
          DF = DF + XXJM1 * HVBCO(K,J) * XX
          XX = XX * HMIN(K)
          F = F + HVBCO(K,J) * XX
   10   CONTINUE
C  ----- PREVENT NEGATIVE FLOW
        QMIN(K) = MAX(X0,F)
        DFMIN(K) = DF
   20 CONTINUE
      DO 40 K = 1, NFAN
        F = HVBCO(K,1)
        DF = X0
        XX = X1
        DO 30 J = 2, NFC(K)
          XXJM1 = J - 1
          DF = DF + XXJM1 * HVBCO(K,J) * XX
          XX = XX * HMAX(K)
          F = F + HVBCO(K,J) * XX
   30   CONTINUE
C  ----- PREVENT NEGATIVE FLOW
        QMAX(K) = MAX(X0,F)
        DFMAX(K) = DF
   40 CONTINUE
C
C     IF THERE ARE NO CONNECTIONS BETWEEN THE HVAC SYSTEM AND THE
C     OUTSIDE WORLD, WE DO NOT NEED TO GO ANY FURTHER
C
      IF (NEXT.LE.0) RETURN
C
C     ARRANGE DATA ON NODE BASIS
C
      DO 70 I = 1, NNODE
        K = 0
        DO 60 IB = 1, NBR
          IF (I.EQ.NA(IB)) THEN
            K = K + 1
            ICMV(I,K) = IB
            IN(I,K) = NE(IB)
          ELSE IF (I.EQ.NE(IB)) THEN
            K = K + 1
            ICMV(I,K) = IB
            IN(I,K) = NA(IB)
          END IF
   60   CONTINUE
        NCNODE(I) = K
   70 CONTINUE

C     CHECK INTERIOR NODES

      DO 80 I = 1, NNODE
        IF (NCNODE(I).LT.1.OR.NCNODE(I).GT.MCON) THEN
C     STOP 'INTERIOR NODE HAS TOO MANY OR TOO FEW CONNECTIONS'
          CALL XERROR(
     .  'HVINIT - Interior node has too many or too few connections',
     .   0,1,1)
          IERROR = 223
          RETURN
        END IF
   80 CONTINUE

C     LIMIT THE RANGE OF HVELXT AND SET THE ABSOLUTE HEIGHT OF THE INTERIOR NODE

      DO 90 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        IF (NCNODE(J).GT.1) then
		ierror = 223
		return
	  endif
        HVELXT(II) = MIN(HR(I),MAX(X0,HVELXT(II)))
        HVGHT(J) = HVELXT(II) + HFLR(I)
   90 CONTINUE
C
C     ASSIGN COMPARTMENT PRESSURE & TEMPERATURE DATA TO EXTERIOR NODES
C     OF THE HVAC NETWORK
C
      DO 100 I = 1, NNODE
        HVP(I) = -1.0D0
  100 CONTINUE
      DO 110 I = 1, NBR
        HVDARA(I) = X0
        HVDVOL(I) = X0
        HVCONC(I,1) = -X1
        TBR(I) = -X1
  110 CONTINUE
C
      S1 = X0
      S2 = X0
      DO 120 LSP = 1, NS
        C3(LSP) = X0
  120 CONTINUE
      DO 140 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        IB = ICMV(J,1)
C        THE OUTSIDE IS DEFINED TO BE AT THE BASE OF THE STRUCTURE FOR MV
        IF (I.LT.N) THEN
          HVEXTT(II,UPPER) = TAMB(I)
          HVEXTT(II,LOWER) = TAMB(I)
          HVP(J) = ZZRELP(I) - HVGRAV * RAMB(I) * HVELXT(II)
        ELSE
          HVEXTT(II,UPPER) = EXTA
          HVEXTT(II,LOWER) = EXTA
          HVP(J) = EXPA - HVGRAV * EXRA * HVELXT(II)
        END IF
        TBR(IB) = HVEXTT(II,UPPER)
        S1 = S1 + HVP(J)
        S2 = S2 + TBR(IB)
        DO 130 LSP = 1, NS
C           THE OUTSIDE IS DEFINED TO BE AT THE BASE OF THE STRUCTURE FOR MV
          IF (I.LT.N) THEN
            HVEXCN(II,LSP,UPPER) = O2N2(LSP) * RAMB(I)
            HVEXCN(II,LSP,LOWER) = O2N2(LSP) * RAMB(I)
          ELSE
            HVEXCN(II,LSP,UPPER) = O2N2(LSP) * EXRA
            HVEXCN(II,LSP,LOWER) = O2N2(LSP) * EXRA
          END IF
          HVCONC(J,LSP) = HVEXCN(II,LSP,UPPER)
          C3(LSP) = C3(LSP) + HVEXCN(II,LSP,UPPER)
  130   CONTINUE
  140 CONTINUE
C     THIS IS TO INITIALIZE THE NODES AND BRANCHES TO SOMETHING
C     WE WILL THEN LET THE SYSTEM EQUILIBRATE TO GIVE US THE TRUE ANSWER
      
      XNEXT = NEXT
      PAV = S1 / XNEXT
      TAV = S2 / XNEXT
      DO 150 LSP = 1, NS
        C3(LSP) = C3(LSP) / XNEXT
  150 CONTINUE
      DO 160 I = 1, NNODE
        IF (HVP(I).LT.X0) THEN
          HVP(I) = PAV
        END IF
  160 CONTINUE
      DO 180 I = 1, NBR
        IF (TBR(I).LE.X0) TBR(I) = TAV
        IF (HVCONC(I,1).LT.X0) THEN
          DO 170 LSP = 1, NS
            HVCONC(I,LSP) = C3(LSP)
  170     CONTINUE
        END IF
  180 CONTINUE

C     CALCULATE AREA, RELATIVE ROUGHNESS, EFFECTIVE
C     DIAMETER AND VOLUME OF DUCTS

C     VOLUME AND ROUGHNESS

      DO 190 ID = 1, NDT
        DA(ID) = (PI*DE(ID)**2) / 4.0D0
        RR(ID) = DUCTAR(ID) / DE(ID)
        IB = IBRD(ID)
        HVDVOL(IB) = HVDVOL(IB) + DA(ID) * DL(ID)
        HVDARA(IB) = HVDARA(IB) + PI*DE(ID)*DL(ID)
  190 CONTINUE

C
C*** CONSTRUCT HVMAP ARRAYS
C
      CALL HVMAP
C
C*** DEFINE TOTAL MASS FOR EACH HVAC SYSTEM
C
      DO 230 ISYS = 1, NHVSYS
         HVTM(ISYS) = X0
  230 CONTINUE      
      DO 240 IB = 1, NBR
         ISYS = IZHVBSYS(IB)
         RDEN = (POFSET+PAV)/(HVRGAS*TBR(IB))
         HVTM(ISYS) = HVTM(ISYS) + RDEN*HVDVOL(IB)
  240 CONTINUE
C
C     NOW THAT EVERYTHING IS OK, WE CAN TURN ON VENTILATION
C
      MVCALC = .TRUE.
      RETURN
C
 5000 format ('HVINIT - fan not properly defined',i3)
      END

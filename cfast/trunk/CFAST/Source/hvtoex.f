      SUBROUTINE HVTOEX(tsec,PRPRIME,NPROD)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVTOEX
C
C     Source File: HVTOEX.SOR
C
C     Functional Class:  
C
C     Description:  ASSIGN RESULTS OF HVAC SIMULATION TO THE TRANSFER
C                   VARIABLES (HVEXTT, HVEXCN)
C
C     Arguments: PRPRIME
C                NPROD
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:24 by GPF:
C                  Added computation of species de's.  these
C                  equations are time split just like the gas layer species 
C                  equations.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      DIMENSION PRPRIME(*)
	double precision tsec, xx1, xx0

C    SUM PRODUCT FLOWS ENTERING SYSTEM

      XX0 = 0.0D0
	xx1 = 1.0d0
      NHVPR = NLSPCT*NHVSYS
      IF(NPROD.NE.0)THEN
         DO 3 I = 1, NHVPR
            PRPRIME(I) = xx0
    3    CONTINUE
      ENDIF
      IF(NS.GT.0)THEN
         DO 1 ISYS = 1, NHVSYS
            HVMFSYS(ISYS) = XX0
            DO 2 K = 1, NS
               DHVPRSYS(ISYS,K) = XX0
    2       CONTINUE
    1    CONTINUE
      ENDIF

C     FLOW INTO THE ISYS SYSTEM

      DO 20 II = 1, NEXT
        J = HVNODE(2,II)
        IB = ICMV(J,1)
        HVEFLO(UPPER,II) = HVFLOW(J,1) * HVFRAC(UPPER,II)
        HVEFLO(LOWER,II) = HVFLOW(J,1) * HVFRAC(LOWER,II)
        ISYS = IZHVSYS(J)
        IF (HVFLOW(J,1).LT.XX0) THEN
          HVMFSYS(ISYS) = HVMFSYS(ISYS) + HVFLOW(J,1)
          IF(NPROD.NE.0)THEN
             DO 10 K = 1, ns
               IF (ACTIVS(K)) DHVPRSYS(ISYS,K) = DHVPRSYS(ISYS,K) + 
     .                      ABS(HVEFLO(UPPER,II))*HVEXCN(II,K,UPPER) +
     .                      ABS(HVEFLO(LOWER,II))*HVEXCN(II,K,LOWER)
   10        CONTINUE
          ENDIF
        END IF
   20 CONTINUE

C     FLOW OUT OF THE ISYS SYSTEM

      IF(NPROD.NE.0)THEN
         DO 24 K = 1, MIN(NS,9)
            IF(ACTIVS(K))THEN
               DO 25 ISYS = 1, NHVSYS
                  IF (ZZHVM(ISYS).NE.XX0)THEN
                     DHVPRSYS(ISYS,K) = DHVPRSYS(ISYS,K) -
     .                   ABS(HVMFSYS(ISYS))*ZZHVPR(ISYS,K)/ZZHVM(ISYS)
                  ENDIF
   25          CONTINUE
            ENDIF
   24    CONTINUE
!	Do a special case for the non-reacting gas(es)
		  k = 11
            IF(ACTIVS(K))THEN
               DO 255 ISYS = 1, NHVSYS
                  IF (ZZHVM(ISYS).NE.XX0)THEN
                     DHVPRSYS(ISYS,k) = DHVPRSYS(ISYS,k) -
     .                   ABS(HVMFSYS(ISYS))*ZZHVPR(ISYS,k)/ZZHVM(ISYS)
                  ENDIF
  255          CONTINUE
            ENDIF
 
C     PACK THE SPECIES CHANGE FOR DASSL (ACTUALLY RESID)

         ISOF = 0
         DO 26 K = 1, MIN(NS,9)
            IF(ACTIVS(K))THEN
               DO 27 ISYS = 1, NHVSYS
                  ISOF = ISOF + 1
                  IF (ZZHVM(ISYS).NE.XX0)THEN
                     PRPRIME(ISOF) = DHVPRSYS(ISYS,K)
                   ELSE
	               PRPRIME(ISOF) = XX0
                   ENDIF
   27          CONTINUE
            ENDIF
   26    CONTINUE
!	Do a special case for the non-reacting gas(es)
            k = 11
            IF(ACTIVS(K))THEN
               DO 277 ISYS = 1, NHVSYS
                  ISOF = ISOF + 1
                  IF (ZZHVM(ISYS).NE.XX0)THEN
                     PRPRIME(ISOF) = DHVPRSYS(ISYS,k)
                   ELSE
	               PRPRIME(ISOF) = XX0
                   ENDIF
  277          CONTINUE
            ENDIF
      ENDIF           

C    DEFINE FLOWS OR TEMPERATURE LEAVING SYSTEM

      DO 30 II = 1, NEXT
         J = HVNODE(2,II)
         ISYS = IZHVSYS(J)
C        WE ALLOW ONLY ONE CONNECTION FROM A NODE TO AN EXTERNAL DUCT
         IB = ICMV(J,1)
         IF (HVFLOW(J,1).GT.XX0) THEN
            HVEXTT(II,UPPER) = TBR(IB)
            HVEXTT(II,LOWER) = TBR(IB)
            DO 40 K = 1, NS
               IF (ACTIVS(K))THEN
! Case 1 - finite volume and finite mass in the ISYS mechanical ventilation system
                 IF (ZZHVM(ISYS).NE.XX0) THEN
                    HVEXCN(II,K,UPPER) = ZZHVPR(ISYS,K)/ZZHVM(ISYS)
                    HVEXCN(II,K,LOWER) = HVEXCN(II,K,UPPER)
! Case 2 - zero volume (no duct). Flow through the system is mdot(product)/mdot(total mass) - see keywordcases to change this
                 ELSEIF(HVMFSYS(ISYS).NE.XX0) THEN
                    HVEXCN(II,K,UPPER) = 
     .                          -(DHVPRSYS(ISYS,K)/HVMFSYS(ISYS))
                    HVEXCN(II,K,LOWER) = HVEXCN(II,K,UPPER)
                 ELSE
                    HVEXCN(II,K,UPPER) = XX0
                    HVEXCN(II,K,LOWER) = XX0
                 ENDIF
               ENDIF
   40       CONTINUE
         END IF
   30 CONTINUE
      RETURN
      END

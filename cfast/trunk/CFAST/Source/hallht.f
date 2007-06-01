      SUBROUTINE HALLHT(IROOM,IDSTART,ND)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HEATHT
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the velocity and temperature
C                   of the ceiling jet at each detector location in
C                   a corridor.
C
C     Arguments: IROOM - room number of corridor
C                IDSTART - index of first detector in corridor IROOM
C                ND - number of detectors in room IROOM
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      DO 10 ID = IDSTART, IDSTART + ND - 1
        XX = XDTECT(ID,DXLOC)
        YY = XDTECT(ID,DYLOC)
        ZZ = XDTECT(ID,DZLOC)
        IF(IZHALL(IROOM,IHXY).EQ.1)THEN
          XLEN = XX
         ELSE
          XLEN = YY
        ENDIF
        CALL HALLTRV(IROOM,XLEN,ZZ,TEMP,RHO,VEL)
        XDTECT(ID,DTJET) = TEMP
        XDTECT(ID,DVEL) = VEL
   10 CONTINUE
      RETURN
      END


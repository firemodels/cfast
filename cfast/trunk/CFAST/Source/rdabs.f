      SUBROUTINE RDABS(
     I                  NZONE,NUP,E,DQDE,EMIS2,AREA,
     I                  FIGS,TAUU,TAUL,
     O                  QLLAY,QULAY
     .                 )
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDABS
C
C     Source File: RDABS.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the energy absorbed by the upper and
C           lower layer due to radiation given off by heat emiiting rectangles
C           forming the enclosure.  Coming into this routine, qllay and qulay
C           were previously defined to be the heat absorbed by the lower and
C           upper layers due to gas emissions and fires.  this routine just
C           adds onto these values.
C
C     Arguments: NZONE
C                NUP
C                E
C                DQDE
C                EMIS2
C                AREA
C                FIGS
C                TAUU
C                TAUL
C                QLLAY
C                QULAY 
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
C
      DIMENSION E(*), EMIS2(*), AREA(*),DQDE(*)
      DIMENSION FIGS(NZONE,*), TAUU(NZONE,*),TAUL(NZONE,*)
C
      DO 300 K = 1, NUP
         QOUT = E(K) - DQDE(K)*(1.0D0-EMIS2(K))
         QK = QOUT*AREA(K)
         DO 310 J = 1,NUP
            QULAY = QULAY + QK*FIGS(K,J)*(1.0D0-TAUU(K,J))
  310    CONTINUE
         DO 320 J = NUP + 1, NZONE
            QULAY = QULAY + QK*FIGS(K,J)*(1.0D0-TAUU(K,J))
            QLLAY = QLLAY + QK*FIGS(K,J)*TAUU(K,J)*(1.0D0-TAUL(K,J))
  320    CONTINUE
  300 CONTINUE
C
      DO 330 K = NUP+1,NZONE
         QOUT = E(K) - DQDE(K)*(1.0D0-EMIS2(K))
         QK = QOUT*AREA(K)
         DO 340 J = 1,NUP
            QULAY = QULAY + QK*FIGS(K,J)*TAUL(K,J)*(1.0D0-TAUU(K,J))
            QLLAY = QLLAY + QK*FIGS(K,J)*(1.0D0-TAUL(K,J))
  340    CONTINUE
         DO 350 J = NUP+1,NZONE
            QLLAY = QLLAY + QK*FIGS(K,J)*(1.0D0-TAUL(K,J))
  350    CONTINUE
  330 CONTINUE
C
      RETURN
      END

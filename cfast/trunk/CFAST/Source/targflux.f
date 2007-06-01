      SUBROUTINE TARGFLUX(ITER,ITARG,TTARG,FLUX,DFLUX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TARGFLUX
C
C     Source File: TARGFLUX.SOR
C
C     Functional Class:  
C
C     Description:  Routine to calculate flux (and later, temperature)
C                   of a target.
C
C     Arguments: ITER   iteration number
C                ITARG  targetnumber
C                TTARG  front and back target input temperature
C                FLUX   front and back output flux
C                DFLUX  front and back output flux derivative
C
C     Revision History:
C     Created by GPF 9/26/94
C        Modified: 4/26/1995 gpf:
C                  expanded this routine to handle fluxes for both the transient and
C                  steady state target temperatures
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fltarget.fi"
      include "objects2.fi"

      DIMENSION MAP10(10), SVECT(3), FLUX(2), DFLUX(2)
      DIMENSION TTARG(2),QWTSUM(2),AWALLSUM(2),QGASSUM(2)
      LOGICAL FIRST
      SAVE FIRST, PI, SIGMA, COS45

      DATA FIRST/.TRUE./
      DATA MAP10/1,3,3,3,3,4,4,4,4,2/

      IF(FIRST)THEN
         FIRST = .FALSE.
         XX1 = 1.0D0
         PI = 4.0D0*ATAN(XX1)
         SIGMA = 5.67D-8
         COS45 = SQRT(XX1+XX1)/2.0D0
      ENDIF

      ABSU = 0.50D0
      ABSL = 0.01D0
      IROOM = IXTARG(TRGROOM,ITARG)

C*** TERMS THAT DO NOT DEPEND UPON THE TARGET TEMPERATURE ONLY NEED TO BE CALCULATED ONCE

      IF(ITER.EQ.1)THEN

C*** INITIALIZE FLUX COUNTERS: TOTAL, FIRE, WALL, GAS 

        DO 10 I = 1, 2
          QTFFLUX(ITARG,I) = 0.0D0
          QTGFLUX(ITARG,I) = 0.0D0
          QTWFLUX(ITARG,I) = 0.0D0
   10   CONTINUE

        NFIRE = IFRPNT(IROOM,1)
        ISTART = IFRPNT(IROOM,2)

C*** COMPUTE RADIATIVE FLUX FROM FIRE

        DO 210 IFIRE = ISTART, ISTART + NFIRE - 1
          SVECT(1) = XXTARG(TRGCENX,ITARG) - XFIRE(IFIRE,1)
          SVECT(2) = XXTARG(TRGCENY,ITARG) - XFIRE(IFIRE,2)
          SVECT(3) = XXTARG(TRGCENZ,ITARG) - XFIRE(IFIRE,3)
          COSANG = 0.0D0
          S = max(DNRM2(3,SVECT,1),objcl(ifire))
          IF(S.NE.0.0D0)THEN
            COSANG = -DDOT(3,SVECT,1,XXTARG(TRGNORMX,ITARG),1)/S
          ENDIF
          ZFIRE = XFIRE(IFIRE,3)
          ZTARG = XXTARG(TRGCENZ,ITARG)
          ZLAY = ZZHLAY(IROOM,LOWER)

C*** COMPUTE PORTION OF PATH IN LOWER AND UPPER LAYERS

          CALL GETYLYU(ZFIRE,ZLAY,ZTARG,S,ZL,ZU)
          TAUL = EXP(-ABSL*ZL)
          TAUU = EXP(-ABSU*ZU)
          QFIRE = XFIRE(IFIRE,8)
          IF(S.NE.0.0D0)THEN
            QFT = QFIRE*ABS(COSANG)*TAUU*TAUL/(4.0D0*PI*S**2)
           ELSE
            QFT = 0.0D0
          ENDIF

C*** DECIDE WHETHER FLUX IS HITTING FRONT OR BACK OF TARGET
C    IF IT'S HITTING THE BACK TARGET ONLY ADD CONTRIBUTION IF THE TARGET
C    IS INTERIOR TO THE ROOM

          IF(COSANG.GE.0.0D0)THEN
             QTFFLUX(ITARG,1) = QTFFLUX(ITARG,1) + QFT
            ELSE
             IF(IXTARG(TRGBACK,ITARG).EQ.INT)THEN
               QTFFLUX(ITARG,2) = QTFFLUX(ITARG,2) + QFT
             ENDIF
          ENDIF

  210   CONTINUE

!*** COMPUTE RADIATIVE FLUX FROM WALLS AND GAS

        DO 215 I = 1, 2
          AWALLSUM(I) = 0.0D0   
          QWTSUM(I) = 0.0D0
          QGASSUM(I) = 0.0D0
  215   CONTINUE
        DO 220 IWALL = 1, 10
          QOUT = RDQOUT(MAP10(IWALL),IROOM)
          SVECT(1) = XXTARG(TRGCENX,ITARG) - ZZWCEN(IROOM,IWALL,1)
          SVECT(2) = XXTARG(TRGCENY,ITARG) - ZZWCEN(IROOM,IWALL,2)
          SVECT(3) = XXTARG(TRGCENZ,ITARG) - ZZWCEN(IROOM,IWALL,3)
          COSANGT = 0.0D0
          S = DNRM2(3,SVECT,1)
          IF(S.NE.0.0D0)THEN
            COSANGT = -DDOT(3,SVECT,1,XXTARG(TRGNORMX,ITARG),1)/S
          ENDIF
          ZWALL = ZZWCEN(IROOM,IWALL,3)
          ZTARG = XXTARG(TRGCENZ,ITARG)
          ZLAY = ZZHLAY(IROOM,LOWER)
          TL = ZZTEMP(IROOM,LOWER)
          TU = ZZTEMP(IROOM,UPPER)

C*** COMPUTE PATH LENGH IN LOWER (ZL) AND UPPER (ZU) LAYER

          CALL GETYLYU(ZWALL,ZLAY,ZTARG,S,ZL,ZU)

C*** FIND FRACTIONS TRANSMITTED AND ABSORBED IN LOWER AND UPPER LAYER

          TAUL = EXP(-ABSL*ZL)
          ALPHAL = 1.0D0 - TAUL
          TAUU = EXP(-ABSU*ZU)
          ALPHAU = 1.0D0 - TAUU

          AWALL = ZZWAREA2(IROOM,IWALL)
          QWT = QOUT*TAUL*TAUU
          IF(IWALL.LE.5)THEN
            QGAS = TL**4*ALPHAL*TAUU + TU**4*ALPHAU
           ELSE
            QGAS = TU**4*ALPHAU*TAUL + TL**4*ALPHAL
          ENDIF
          QGT = SIGMA*QGAS
          IF(COSANGT.GE.0.0D0)THEN
            JJ = 1
           ELSE 
            JJ = 2
          ENDIF

C*** CALCULATE FLUX ON THE TARGET FRONT.  CALCULATE FLUX ON THE TARGET BACK ONLY IF THE 
C    REAR OF THE TARGET IS INTERIOR TO THE ROOM.

          IF(JJ.EQ.1.OR.IXTARG(TRGBACK,ITARG).EQ.INT)THEN
            QWTSUM(JJ) = QWTSUM(JJ) + QWT*AWALL
            QGASSUM(JJ) = QGASSUM(JJ) + QGT*AWALL
            AWALLSUM(JJ) = AWALLSUM(JJ) + AWALL
          ENDIF
  220   CONTINUE
        DO 225 I = 1, 2
          IF(AWALLSUM(I).EQ.0.0D0)AWALLSUM(I) = 1.0D0
          QTWFLUX(ITARG,I) = QWTSUM(I)/AWALLSUM(I)
          QTGFLUX(ITARG,I) = QGASSUM(I)/AWALLSUM(I)
  225   CONTINUE        

C*** IF THE TARGET REAR WAS EXTERIOR THEN CALCULATE THE FLUX ASSUMING AMBIENT OUTSIDE CONDITIONS

        IF(IXTARG(TRGBACK,ITARG).EQ.EXT)THEN
          QTGFLUX(ITARG,2) = SIGMA*TAMB(IROOM)**4
        ENDIF
      ENDIF

C*** COMPUTE CONVECTIVE FLUX

C  ASSUME TARGET IS A 'FLOOR', 'CEILING' OR 'WALL' DEPENDING ON HOW MUCH THE TARGET IS TILTED.  

      ZZNORM = XXTARG(TRGNORMZ,ITARG)
      IF(ZZNORM.LE.1.0D0.AND.ZZNORM.GE.COS45)THEN
        IW = 2
        IWB = 1
       ELSEIF(ZZNORM.GE.-1.0D0.AND.ZZNORM.LE.-COS45)THEN
        IW = 1
        IWB = 2
       ELSE
        IW = 3
        IWB = 3
      ENDIF
      ILAY = IXTARG(TRGLAYER,ITARG)
      TG = ZZTEMP(IROOM,ILAY)
      IF(IXTARG(TRGBACK,ITARG).EQ.INT)THEN
         TGB = TG
        ELSE
         TGB = TAMB(IROOM)
      ENDIF
      TTARGB = TTARG(2)
      DTTARG = 1.0D-7*TTARG(1)
      DTTARGB = 1.0D-7*TTARG(2)

      TEMIS = XXTARG(TRGEMIS,ITARG)

C*** CONNECTION FOR THE FRONT

      CALL CONVEC(IW,TG,TTARG(1),Q1)
      CALL CONVEC(IW,TG,TTARG(1)+DTTARG,Q2)
      QTCFLUX(ITARG,1) = Q1
      DQDTARG = (Q2-Q1)/DTTARG

      FLUX(1) = TEMIS*(QTFFLUX(ITARG,1) + QTWFLUX(ITARG,1) +
     .       QTGFLUX(ITARG,1)) + QTCFLUX(ITARG,1) - 
     .       TEMIS*SIGMA*TTARG(1)**4
      DFLUX(1) = -4.0D0*TEMIS*SIGMA*TTARG(1)**3 + DQDTARG

C*** CONNECTION FOR THE BACK

      CALL CONVEC(IWB,TGB,TTARGB,Q1B)
      CALL CONVEC(IWB,TGB,TTARGB+DTTARGB,Q2B)
      QTCFLUX(ITARG,2) = Q1B
      DQDTARGB = (Q2B-Q1B)/DTTARGB

      FLUX(2) = TEMIS*(QTFFLUX(ITARG,2) + QTWFLUX(ITARG,2) +
     .       QTGFLUX(ITARG,2)) + QTCFLUX(ITARG,2) - 
     .       TEMIS*SIGMA*TTARGB**4
      DFLUX(2) = -4.0D0*TEMIS*SIGMA*TTARGB**3 + DQDTARGB

      RETURN
      END
  

      SUBROUTINE GETYLYU(YO,Y,YT,S,YL,YU)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GETYLYU
C
C     Source File: TARGFLUX.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: YO
C                Y
C                YT
C                S
C                YL
C                YU
C
C     Revision History:
C        Created:  5/5/1995 at 14:00 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"

      IF(YO.LE.Y)THEN
         IF(YT.LE.Y)THEN
            YL = 1.0D0
           ELSE
            YL = (Y-YO)/(YT-YO)
         ENDIF
        ELSE
         IF(YT.GE.Y)THEN
            YL = 0.0D0
           ELSE
            YL = (Y-YT)/(YO-YT)
         ENDIF
      ENDIF
      YL = YL*S
      YU = S - YL
      RETURN
      END

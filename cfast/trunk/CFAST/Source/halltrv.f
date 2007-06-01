      SUBROUTINE HALLTRV(IROOM,XLOC,ZLOC,HALLTEMP,HALLRHO,HALLVEL)
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      IF(IZHALL(IROOM,IHMODE).EQ.IHDURING)THEN
        D0 = ZZHALL(IROOM,IHDEPTH)
        CJETHEIGHT = HR(IROOM) - D0

C*** location is in hall ceiling jet

        IF(ZLOC.GE.CJETHEIGHT.AND.XLOC.LE.ZZHALL(IROOM,IHDIST))THEN
          C1 = 1.0D0
          IHALF = IZHALL(IROOM,IHHALFFLAG)
          HHALF = ZZHALL(IROOM,IHHALF)
          DT0 = ZZHALL(IROOM,IHTEMP)

C*** check to see if the user specified a HHALF value on the command line.
C    if not (ie if IHALF==0) then calculate it 
C    using the correlations.

          IF(IHALF.EQ.0)THEN
c***  hhalf = -log10(2)/.018
            HHALF = 16.70D0
            ZZHALL(IROOM,IHHALF) = HHALF
          ENDIF

C*** if HHALF < 0.0 then assume that the temperature does not decay
C    (ie flow is adiabatic)

          IF(HHALF.GT.0.0D0)THEN
            FACT = 0.5D0**(XLOC/HHALF)
           ELSE
            FACT = 1.0D0
          ENDIF

          HALLTEMP = ZZTEMP(IROOM,LOWER) + DT0*FACT
          HALLRHO = ZZPABS(IROOM)/(RGAS*HALLTEMP)
          HALLVEL = ZZHALL(IROOM,IHVEL)
         ELSE
          HALLTEMP = ZZTEMP(IROOM,LOWER)
          HALLRHO = ZZRHO(IROOM,LOWER)
          HALLVEL = 0.10D0
        ENDIF
       ELSE

C*** hall jet is not flowing (either has not started or has finished)
C    so, use regular layer temperatures and densities

        IF(ZLOC.GT.ZZHLAY(IROOM,LOWER))THEN
          HALLTEMP = ZZTEMP(IROOM,UPPER)
          HALLRHO = ZZRHO(IROOM,UPPER)
         ELSE
          HALLTEMP = ZZTEMP(IROOM,LOWER)
          HALLRHO = ZZRHO(IROOM,LOWER)
        ENDIF
        HALLVEL = 0.10D0
      ENDIF

      RETURN
      END

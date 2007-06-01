      SUBROUTINE PYROLS(TIME,IROOM,BFIRET,AFIRET,HFIRET,QFIRET,HCOMBT,
     .                  CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,ZMFIRE)
C
C*RB
C     Routine:  PYROLS
C
C     Function: Calculate pyrolysis rate of the fuel for the specified
C               main fire
C     Inputs:   TIME    - current time (s)
C               IROOM   - room number containing fire
C     Outputs:  BFIRET  - current pyrolysis rate (kg/s)
C               AFIRET  - current area of the fire (m^2)
C               HFIRET  - current height of the fire (m)
C               QFIRET  - current heat release rate of the fire (W)
C               HCRATT  - current hydrogen/carbon ratio in fuel (kg/kg)
C               CCO2T   - current carbon/CO2 production ratio (kg/kg)
C               COCO2T  - current CO/CO2 production ratio (kg/kg)
C               OCRATT  - current Oxygen/Carbon ratio in fuel (kg/kg)
C               HCOMBT  - current heat of combustion (J/kg)
C               CLFRAT  - current HCl production rate (kg/kg burned)
C               CNFRAT  - current HCN production rate (kg/kg burned)
C               ZMFIRE  - current species production rates (kg/s)
C     Commons:  
C      PASSED:  Afired   Bfired   Cco2     Coco2    Hcratio  Hfired
C               Hocbmb   Lfmax    Mprodr   Ocrati   Qfired   Tfired  
C        USED:  Activs  
C
C     Revision History
C     RDP   11/11/91  modified to use new "interpolator" subroutine
C     WWJ   3/3/90    changed the procedure call to pass variables that
C                     are changed
C     gpf 10/14/93 added detection/suppression

      include "precis.fi"
      include "cfast.fi"

      DIMENSION ZMFIRE(*)
 
      ID = IQUENCH(IROOM)
      XXTIME = TIME
      IF(ID.EQ.0)THEN
C
C*** IF A SPRINKLER IS NOT ACTIVE THEN INTERPOLATE AT CURRENT TIME
C
        IFACT = 0
       ELSE
C
C*** IF A SPRINKLER IS ACTIVE THEN INTERPOLATE AT CURRENT TIME
C    AND WHEN SPRINKLER FIRST ACTIVATED.  MAKE SURE THAT SPECIFIED
C    HEAT RELEASE RATE IS THE SMALLER OF RATE AT CURRENT TIME
C    AND RATE AT SPRINKLER ACTIVATION TIME * EXP( ...) 
C
        TDRATE = XDTECT(ID,DRATE)
        XXTIMEF = XDTECT(ID,DTACT)
        CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIME,1,QT)
        CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIMEF,1,QTF)
        IFACT = 1
        TFACT = EXP(-(XXTIME-XXTIMEF)/TDRATE)
        IF(QT.LT.TFACT*QTF)THEN
C
C*** CURRENT TIME HEAT RELEASE RATE IS SMALLER THAN SPRINKLERD VALUE
C    SO USE CURRENT TIME AND RESET IFACT TO 0 SO RATES ARE NOT 
C    DECREASED
C
           IFACT = 0
          ELSE
           XXTIME = XXTIMEF
        ENDIF
      ENDIF
C
C     INTERPOLATE THE FIRE OUTPUTS
C
      CALL INTERP(TFIRED,BFIRED,LFMAX,XXTIME,1,BFIRET)
      CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIME,1,QFIRET)
      CALL INTERP(TFIRED,HCLF,LFMAX,XXTIME,1,CLFRAT)
      CALL INTERP(TFIRED,HCNF,LFMAX,XXTIME,1,CNFRAT)
 
C*** ATTENUATE MASS AND ENERGY RELEASE RATES IF THERE IS AN ACTIVE SPRINKLER
C    IN THIS ROOM
 
      IF(ID.NE.0.AND.IFACT.EQ.1)THEN
        BFIRET = BFIRET*TFACT
        QFIRET = QFIRET*TFACT
        CLFRAT = CLFRAT*TFACT
        CNFRAT = CNFRAT*TFACT
      ENDIF
      CALL INTERP(TFIRED,HOCBMB,LFMAX,XXTIME,1,HCOMBT)
      CALL INTERP(TFIRED,AFIRED,LFMAX,XXTIME,1,AFIRET)
      CALL INTERP(TFIRED,HFIRED,LFMAX,XXTIME,1,HFIRET)
      CALL INTERP(TFIRED,HCRATIO,LFMAX,XXTIME,1,HCRATT)
      XLIM = 0.3333D0
      HCRATT = MIN(XLIM,HCRATT)
      CALL INTERP(TFIRED,CCO2,LFMAX,XXTIME,1,CCO2T)
      CALL INTERP(TFIRED,COCO2,LFMAX,XXTIME,1,COCO2T)
      CALL INTERP(TFIRED,OCRATI,LFMAX,XXTIME,1,OCRATT)
      DO 10 IPROD = 1, NS
        IF (ACTIVS(IPROD)) THEN
          CALL INTERP(TFIRED,MPRODR(1,IPROD),LFMAX,XXTIME,1,XPROD)
          ZMFIRE(IPROD) = XPROD * BFIRET
        END IF
   10 CONTINUE
      RETURN
      END

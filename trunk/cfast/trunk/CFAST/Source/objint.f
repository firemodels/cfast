      SUBROUTINE OBJINT (OBJN, TIME, IROOM, OMASST, OAREAT, 
     . OHIGHT, OQDOTT, OBJHCT, CCO2T, COCO2T, HCRATT, ZMFIRE, OCRATT,
     . CLFRAT, CNFRAT,crfrat,UPDATE)

C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OBJINT
C
C     Source File: OBJINT.SOR
C
C     Functional Class:  
C
C     Description:  Returns  
C
C     Arguments: OBJN   The object pointer number, 
C                TIME   Current simulation time (s)
C                IROOM  Room object is in
C                FLUX   The normal radiative flux to the object
C                SURFT  Surfice temp of object
C                OMASST Pyrolysis rate of object (returned)
C                OAREAT Area of pyrolysis of object (returned)
C                OHIGHT Height of fire (returned)
C                OQDOTT Heat release rate of object
C                OBJHCT Object heat of combustion
C                CCO2T  Carbon to CO2 ratio
C                COCO2T CO to CO2 ration
C                HCRATT Hydrogen to Carbon ratio of fuel
C                ZMFIRE 
C                OCRATT Oxygen to Carbon ration of fuel
C                CLFRAT HCl production rate
C                CNFRAT HCN production rate
C                crFRAT trace gase production rate
C                UPDATE Update varible
C
C     Revision History:
C        Modified  8/21/1990  calculate the pyrolysis rate, ... 
C                             for other objects at this time, this 
C                             code is identical to PYROLS, but the
C                             intent is to be able to change it.
C                  10/14/1993 added detection/suppression by GPF
C                  8/15/1995  added code to handle type three fires.
C                               Also fixed objects so they can properly
C                             ignite by conditions and added header
C                             by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------


C     PYROLYSIS RATE OF THE FUEL - HCRATT IS IN COMMON (PARAMS.INC) SINCE
C     IT IS USED IN SEVERAL PLACES

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fireptrs.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"

      DIMENSION ZMFIRE(NS)
      INTEGER OBJN, UPDATE

      ONETHRD = 0.3333D0
      IF (.NOT.OBJON(OBJN).OR.OBJSET(OBJN).GT.0) THEN
        XX0 = 0.0D0
        OMASST = XX0
        OAREAT = XX0
        OHIGHT = XX0
        OQDOTT = XX0
        HCRATT = XX0
        CCO2T = XX0
        COCO2T = XX0
        OCRATT = XX0
        OBJHCT = 5.0D7
        DO 14 J = 1, NS
   14     ZMFIRE(J) = XX0
        CLFRAT = XX0
        CNFRAT = XX0
        IF (FSMTYPE.GT.0) THEN
          DYPDT = XX0
          DXPDT = XX0
          DYBDT = XX0
          DXBDT = XX0
          DQDT  = XX0
        END IF
        RETURN
      ENDIF

      LOBJLFM = OBJLFM(OBJN)
      XXTIME = TIME - OBJCRI(1,OBJN)
      
	ID = IQUENCH(IROOM)
      
	IF(ID.EQ.0)THEN

!	IF A SPRINKLER IS NOT ACTIVE THEN INTERPOLATE AT CURRENT TIME

	    IFACT = 0
	ELSE
C
C*** IF A SPRINKLER IS ACTIVE THEN INTERPOLATE AT CURRENT TIME
C    AND WHEN SPRINKLER FIRST ACTIVATED.  MAKE SURE THAT SPECIFIED
C    HEAT RELEASE RATE IS THE SMALLER OF RATE AT CURRENT TIME
C    AND RATE AT SPRINKLER ACTIVATION TIME * EXP( ...) 
C
	    TDRATE = XDTECT(ID,DRATE)
	    XXTIMEF = XDTECT(ID,DTACT) - OBJCRI(1,OBJN)
	    CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIME,1,
     .                QT)
	    CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIMEF,1,
     .                QTF)
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
      
	CALL INTERP(OTIME(1,OBJN),OMASS(1,OBJN),LOBJLFM,XXTIME,1,
     .            OMASST)
	CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIME,1,
     .  			 OQDOTT)
	CALL INTERP(OTIME(1,OBJN),OMPRODR(1,6,OBJN),LOBJLFM,XXTIME,1,
     +  			 CLFRAT)
	CALL INTERP(OTIME(1,OBJN),OMPRODR(1,5,OBJN),LOBJLFM,XXTIME,1,
     +					 CNFRAT)
      call interp(otime(1,objn),omprodr(1,11,objn),lobjlfm,xxtime,1,
     +                   crfrat)

C*** ATTENUATE MASS AND ENERGY RELEASE RATES IF THERE IS AN ACTIVE SPRINKLER
C    IN THIS ROOM

	IF(ID.NE.0.AND.IFACT.EQ.1)THEN
	    OMASST = OMASST*TFACT
	    OQDOTT = OQDOTT*TFACT
	    CLFRAT = CLFRAT*TFACT
	    CNFRAT = CNFRAT*TFACT
	ENDIF
	CALL INTERP(OTIME(1,OBJN),OAREA(1,OBJN),LOBJLFM,XXTIME,1,
     .        OAREAT)
	CALL INTERP(OTIME(1,OBJN),OHIGH(1,OBJN),LOBJLFM,XXTIME,1,
     .        OHIGHT)

	CALL INTERP(OTIME(1,OBJN),OHCR(1,OBJN),LOBJLFM,XXTIME,1,HCRATT)
	HCRATT = MIN (ONETHRD, HCRATT)
	CALL INTERP(OTIME(1,OBJN),OOD(1,OBJN),LOBJLFM,XXTIME,1,CCO2T)
	CALL INTERP(OTIME(1,OBJN),OCO(1,OBJN),LOBJLFM,XXTIME,1,COCO2T)
	CALL INTERP(OTIME(1,OBJN),OOC(1,OBJN),LOBJLFM,XXTIME,1,OCRATT)
	CALL INTERP(OTIME(1,OBJN),OBJHC(1,OBJN),LOBJLFM,XXTIME,1,OBJHCT)
	DO 15 J = 1, NS
	    CALL INTERP(OTIME(1,OBJN),OMPRODR(1,J,OBJN),LOBJLFM,
     .  		XXTIME,1,XPROD)
	    ZMFIRE(J) = XPROD * OMASST
   15	CONTINUE

      RETURN
      END

      SUBROUTINE UPDTECT(
     I                   IMODE,TCUR,DSTEP,NDTECT,
     I                   ZZHLAY,ZZTEMP,
     .                   XDTECT,IXDTECT,
     O                   IQUENCH,IDSET,IFDTECT,TDTECT
     .                   )
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDTECT
C
C     Source File: UPDTECT.SOR
C
C     Description:  This routine updates the temperature of each detector 
C           link.  It also determine whether the detector has activated 
C           in the time interval (tcur,tcur+dstep).  If this has occured
C           then a quenching algorithm will be invoked if the appropriate
C           option has been set.
C
C     Arguments: TCUR    current time
C                DSTEP   time step size (to next time)
C                NDTECT  number of detectors
C                XDTECT  2-d array containing floating point detector 
C                        data structures 
C                IXDTECT 2-d array containing integer detectir data 
C                        structures
C                IQUENCH if the j=IQUENCH(i) is non-zero then the 
C                        j'th sprinkler in the i'th room is quenching 
C                        the fire
C                IDSET   room where activated detector resides
C
C     Revision History:
C        Created: 9/3/93 by GPF:
C        MODIFIED: 4/24/95 by GPF:
C                removed references to ISET to eliminate FLINT complaints
C        MODIFIED: 2/29/96 by GPF:
C                fixed reference to TLINKO and VELO .  Added minimum ceiling jet
C                velocity and minimum temperature to detector link temperature
C                calculation.  0.1 m/s minimum c.j. velocity was taken from DETACT
C        MODIFIED: 10/9/97 by GPF:
C                return sensor number instead of "1" in IFDTECT 
C                if a sensor has activated
C                
C
      include "precis.fi"
      include "dsize.fi"
      include "cparams.fi"

      DIMENSION ZZTEMP(NR,2), ZZHLAY(NR,2)

      DIMENSION XDTECT(MXDTECT,*), IXDTECT(MXDTECT,*), IQUENCH(*)
C
      IDSET = 0
      IFDTECT = 0
      TDTECT = TCUR+2*DSTEP
      CJETMIN = 0.10D0
      DO 10 I = 1, NDTECT
        IROOM = IXDTECT(I,DROOM)
        TLINKO = XDTECT(I,DTEMP)

        ZDETECT = XDTECT(I,DZLOC)
        IF(ZDETECT.GT.ZZHLAY(IROOM,LOWER))THEN
          TLAY = ZZTEMP(IROOM,UPPER)
         ELSE
          TLAY = ZZTEMP(IROOM,LOWER)
        ENDIF

        TJET = MAX(XDTECT(I,DTJET),TLAY)
        TJETO = MAX(XDTECT(I,DTJETO),TLAY)
        VEL = MAX(XDTECT(I,DVEL),CJETMIN)
        VELO = MAX(XDTECT(I,DVELO),CJETMIN)

        RTI = XDTECT(I,DRTI)
        TRIG = XDTECT(I,DTRIG)
        IF(IXDTECT(I,DTYPE).EQ.SMOKED)THEN
          TLINK = TJET
         ELSEIF(IXDTECT(I,DTYPE).EQ.HEATD)THEN
          BN = SQRT(VELO)/RTI
          AN = BN*TJETO
          BNP1 = SQRT(VEL)/RTI
          ANP1 = BNP1*TJET
          DENOM = 1.0D0 + DSTEP*BNP1*.5D0
          FACT1 = (1.0D0 - DSTEP*BN*.50D0)/DENOM
          FACT2 = DSTEP/DENOM
          TLINK = FACT1*TLINKO + FACT2*(AN+ANP1)*0.5D0
         ELSE

C*** WHEN SOOT IS CALCULATED THEN SET TLINK TO SOOT CONCENTRATION.
C    SET IT TO ZERO FOR NOW.

           TLINK = 0.0D0
        ENDIF
        IF (IMODE.GT.0) THEN
          XDTECT(I,DTEMPO) = TLINKO
          XDTECT(I,DTEMP) = TLINK
        ENDIF
C*** DETERMINE IF DETECTOR HAS ACTIVATED IN THIS TIME INTERVAL (and not earlier)

        IF(TLINKO.LT.TRIG.AND.TRIG.LE.TLINK.AND.
     .                        IXDTECT(I,DACT).EQ.0)THEN
          DELTA = (TRIG-TLINKO)/(TLINK-TLINKO)
          TMP = TCUR+DSTEP*DELTA
          TDTECT = MIN(TMP,TDTECT)
          IFDTECT = I
          IF (IMODE.GT.0) THEN
            XDTECT(I,DTACT)= TCUR+DSTEP*DELTA
            IXDTECT(I,DACT) = 1

C*** DETERMINE IF THIS IS THE FIRST DETECTOR TO HAVE ACTIVATED IN THIS ROOM

            IDOLD = IQUENCH(IROOM)
            IQU = 0
            IF(IDOLD.EQ.0)THEN
              IQU = I
             ELSE
              IF(XDTECT(I,DTACT).LT.XDTECT(IDOLD,DTACT))THEN

C*** THIS CAN ONLY HAPPEN IF TWO DETECTORS HAVE ACTIVATED IN THE SAME
C    ROOM IN THE SAME (POSSIBLY VERY SHORT) TIME INTERVAL

                IQU = I
              ENDIF
            ENDIF
C*** IF THIS DETECTOR HAS ACTIVATED BEFORE ALL OTHERS IN THIS ROOM
C    AND THE QUENCHING FLAG WAS TURNED ON THEN LET THE SPRINKLER
C    QUENCH THE FIRE

            IF(IQU.NE.0.AND.IXDTECT(I,DQUENCH).EQ.1)THEN
              IQUENCH(IROOM)=IQU
              IDSET = IROOM
            ENDIF
          ENDIF
        ENDIF
        XDTECT(I,DTJETO) = TJET
        XDTECT(I,DVELO) = VEL
   10 CONTINUE
      RETURN
      END

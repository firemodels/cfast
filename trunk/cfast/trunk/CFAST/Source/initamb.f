      SUBROUTINE INITAMB(YINTER,IFLAG)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITAMB
C
C     Source File: INITAMB.SOR
C
C     Functional Class:  
C
C     Description:  
C     THIS ROUTINE COMPUTES INITIALIZATIONS FOR VARIALBES
C     RELATED TO AMBIENT CONDITIONS.  WHEN IFLAG=1 THE ARRAY
C     YINTER IS USED TO COMPUTE UPPER LAYER VOLUMES.  OTHERWISE,
C     UPPER LAYER VOLUMES ARE NOT COMPUTED.  IF IFLAG IS SET TO 1
C     THEN YINTER MUST BE A FLOATING POINT ARRAY OF AT LEAST SIZE NR
C     (NR = NUMBER OF ROOMS) IN THE CALLING ROUTINE.
C
C     Arguments: YINTER
C                IFLAG
C
C     Revision History:
C     July 26, 1990 changed mapping to eliminate reference to outside room
C     May 15, 1991  fix the minimum pressure so that all offsets are pref-pofset
C     June 14, 1992 added initialization of hvac duct temperatures and
C                   hvac internal node pressures
C     gpf 10/14/93  initialized variables involved in the 
C                   detection/suppression
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/1994 by GPF:
C                  initialize some target data structures
C        Modified: 4/26/95 by GPF
C                  Remove usage of obsolete variables.  Add definition of target temperatures
C                  into P array for implicit targets.  Changed definition of POFFSET to reduce
C                  the size of pressure variables that DASSL solves for.
C        Modified: 6/30/95 by GPF:
C                  If oxygen is being solved for by DASSL, 
C                  initialize solver array.
C        Modified: 5/28/96 by GPF:
C                  Fixed subscript error in WINDC array (changed I -> N)
C        Modified: 7/22/96 by GPF:
C                  initalized IZHALL(.,IHXY) for the hall option.
C                  This value is 1 if the room depth, DR, is the
C                  longer than room breadth, BR, and 2 otherwise.
C        Modified: 2/6/97 by GPF:
C                  Fixed wind.  Wind now is a property of a vent connected to the outside
C                  rather than a property of a room.  (before the fix you could not have 
C                  wind effecting more than one vent in the same room.
C                  Also fixed initializations.  Fictional flows in tall buildings occurred
C                  due to the inconsistent way that we initialized pressures and 
C                  temperatures.  Inside the building we assume that temperature
C                  and density are constant in a layer.  Outside the building
C                  we did not.  This inconsitency in assumptions results in 
C                  fictional flows.
C        Modified:  10/9/1997 by gpf
C                   turn off a sprinkler if its spray density is zero.
C        Modified:  12/3/1997 by gpf
C                   defined ETA(N) and ERA(N)
!        Modified:  1/25/5 - wwj; removed calculations using atmosp; lapse rate inside and out are assumed to be the same
!					  With chage, the TAMB line now only has the original three entries (ambient temperature, ambient pressure an
!					  ambient station elevation. The same is true for EAMB
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "fltarget.fi"
      include "opt.fi"
C
      DIMENSION YINTER(*)
C
C     INITIAL CONDITIONS FOR THE VARIABLES FOR WHICH WE KEEP TIME HISTORIES
C
      XX0 = 0.0D0
      XX2 = 2.0D0

C*** simplify and make initial pressure calculations consistent.  Inside pressures
C    were calculated using rho*g*h .  But outside pressures were calculated using
C    ATMOSP.  Fictional flows resulted making  SNSQE work a log harder to get
C    an initial solution.  The initial temperature values calculated by ATMOSP
C    at the top of the empire state building (about 400 M above base) is only
C    about 0.2 K different that at the base.  

!      IF(SAL2.GT.XX0.AND.SAL.NE.SAL2)PA = ATMOSP(TA,SAL,PA,SAL2)
      DO 10 I = 1, NM1
        PAMB(I) = -RA*G*HFLR(I)
        TAMB(I) = TA
        RAMB(I) = RA
        EPA(I) = -EXRA*G*HFLR(I)
        ETA(I) = EXTA
        ERA(I) = EXRA
   10 CONTINUE
      ETA(N) = EXTA
      ERA(N) = EXRA
      EPA(N) = XX0

C*** if the user enters a SAL2 value on the TAMB input line then
C    ATMOSP calculates a new base pressure.  This could be used 
C    for example, to calculate an ambient pressure in Denver 
C    (1609 M above sea level) given a base pressure of 101300 Pa
C    in Ocean City MD (sea level).

!      IF(SAL2.GT.XX0.AND.SAL.NE.SAL2)THEN
!        PA = ATMOSP(TA,SAL,PA,SAL2)
!        EXPA = PA
!        POFSET = PA
!      ENDIF
C
C     normalize pressures so that the smallest pressure is zero
C
      XXPMIN = PAMB(1)
      DO 30 I = 1, NM1
        XXPMIN = MIN(XXPMIN,PAMB(I),EPA(I))
   30 CONTINUE
      DO 50 I = 1, NM1
        EPA(I) = EPA(I) - XXPMIN
        PAMB(I) = PAMB(I) - XXPMIN
   50 CONTINUE
      POFSET = POFSET + XXPMIN
      PA = PA + XXPMIN - POFSET
      EXPA = EXPA + XXPMIN - POFSET

C     COPY ALL OF THE VARIABLES FROM THE INITIAL VALUES INTO THE DATA ARRAYS

      CALL DATACOPY(DUMMY,CONSTVAR)

C    DEFINE THE P ARRAY, THE SOLUTION TO THE ODE

      DO 70 I = 1, NM1
        P(I) = PAMB(I)
        P(I+NOFTU) = TAMB(I)

C     CHECK FOR A SPECIAL SETTING OF THE INTERFACE HEIGHT

        IF (IFLAG.EQ.1) THEN
          IF (YINTER(I).LT.0.D0) THEN
            P(I+NOFVU) = ZZVMIN(I)
          ELSE
            P(I+NOFVU) = MIN(ZZVMAX(I),MAX(ZZVMIN(I),YINTER(I)*AR(I)))
          END IF
          YINTER(I) = XX0
        END IF
        IF(IZSHAFT(I).EQ.1)P(I+NOFVU) = ZZVMAX(I)
        P(I+NOFTL) = TAMB(I)
   70 CONTINUE

C     DEFINE HVAC PRESSURES AND TEMPERATURES.  THESE VALUES ARE LATER REFINED BY 
C     SNSQE SO THAT EACH HVAC NODE CONSERVES MASS AND ENERGY

      DO 75 I = 1, NHVPVAR
         P(I+NOFPMV) = XX0
   75 CONTINUE
      DO 76 I = 1, NHVTVAR
         P(I+NOFTMV) = TAMB(1)
   76 CONTINUE

C     DEFINE INTERIOR SURFACE WALL TEMPERATURES

      II =NOFWT 
      DO 90 I = 1, NM1
        DO 80 IWALL = 1, NWAL
          IF (SWITCH(IWALL,I)) THEN
            II = II + 1
            P(II) = TAMB(I)
          END IF
   80   CONTINUE
   90 CONTINUE

c*** establish default values for detector data

      DO 13 I = 1, NDTECT
         IROOM=IXDTECT(I,DROOM)
         IF(XDTECT(I,DXLOC).LT.0.0D0)XDTECT(I,DXLOC)=bR(IROOM)*.5D0
         IF(XDTECT(I,DYLOC).LT.0.0D0)XDTECT(I,DYLOC)=dR(IROOM)*.5D0
         IF(XDTECT(I,DZLOC).LT.0.0D0)THEN
            XDTECT(I,DZLOC)=HRP(IROOM)+XDTECT(I,DZLOC)
         ENDIF
         TDSPRAY = XDTECT(I,DSPRAY)

C*** IF TDSPRAY>0 THEN INTERPRET IT AS A SPRAY DENSITY AND CONVERT
C        TO A CHARACTERISTIC QUENCHING TIME
C    IF TDSPRAY < 0 THEN INTERPRET ABS(TDSPRAY) AS THE TIME
C        REQUIRED TO REDUCE THE FIRE SIZE BY 50 PER CENT
C    IF TDSPRAY = 0 THEN TURN THE SPRINKLER OFF


         IF(TDSPRAY.GT.0.0D0)THEN
           TDRATE = 3.0D0/TDSPRAY**1.8D0
          ELSEIF(TDSPRAY.LT.0.0D0)THEN
           TDRATE = ABS(TDSPRAY)/LOG(XX2)
           TDSPRAY = (3.0D0/TDRATE)**(1.0D0/1.8D0)
          ELSE
           TDSPRAY = 0.0D0
           TDRATE = 1.0D10
           IXDTECT(I,DQUENCH) = 0
         ENDIF

C*** SET INITIAL CEILING JET AND DETECTOR LINK TEMPERATURES TO AMBIENT

         XDTECT(I,DSPRAY) = TDSPRAY
         XDTECT(I,DTHALF) = TDRATE*LOG(XX2)
         XDTECT(I,DRATE) = TDRATE
         XDTECT(I,DTEMP) = TAMB(IROOM)
         XDTECT(I,DTEMPO) = TAMB(IROOM)
         XDTECT(I,DTJET) = TAMB(IROOM)
         XDTECT(I,DTJETO) = TAMB(IROOM)
   13 CONTINUE

      CALL SORTBRM(XDTECT,MXDTECT,IXDTECT,MXDTECT,
     .             NDTECT,DTXCOL,DTICOL,DROOM,NR,NM1,IDTPNT)

C     P'S FOR PRESSURE, VOLUME AND TEMPERATURE ARE DEFINED
C     WE CAN NOW COPY THESE VALUES TO THE ENVIRONMENT VARIABLES

      CALL DATACOPY (P, ODEVARA)

C*** INITIALIZE TARGET TEMPERATURES

      DO 140 ITARG = 1, NTARG
         IROOM = IXTARG(TRGROOM,ITARG)
         IF(IXTARG(TRGMETH,ITARG).EQ.MPLICIT)THEN
           IEQ = IZTARG(ITARG)
           P(NOFTT+IEQ) = TAMB(IROOM)
         ENDIF  
         DO 150 I=TRGTEMPF,TRGTEMPB
            XXTARG(I,ITARG)=TAMB(IROOM)
  150    CONTINUE

C*** SCALE NORMAL VECTORS TO HAVE LENGTH 1

         SCALE = 1.0D0/DNRM2(3,XXTARG(TRGNORMX,ITARG),1)
         CALL DSCAL(3,SCALE,XXTARG(TRGNORMX,ITARG),1)
  140 CONTINUE 

c*** initialize solver oxygen values if required.   (must be initialized
c    after zzmass is defined)

      IF(OPTION(FOXYGEN).EQ.ON)THEN
        DO 160 IROOM = 1, NM1
           P(IROOM+NOFOXYU)=0.23D0*ZZMASS(IROOM,UPPER)
           P(IROOM+NOFOXYL)=0.23D0*ZZMASS(IROOM,LOWER)
  160   CONTINUE
      ENDIF

c*** define IHXY in IZHALL (dimension that is longest)

      DO 200 I = 1, NM1
        IF(IZHALL(I,IHROOM).EQ.1)THEN
          IF(DR(I).GT.BR(I))THEN
            IZHALL(I,IHXY) = 1
           ELSE
            IZHALL(I,IHXY) = 2
          ENDIF
        ENDIF
  200 CONTINUE

      RETURN
      END

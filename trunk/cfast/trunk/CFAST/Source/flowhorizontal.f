      SUBROUTINE HFLOW(TSEC,EPSP,NPROD,UFLW)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HFLOW
C
C     Description:  Physical interface routine to calculate flow through all unforced vertical vents (horizontal flow).
!     It returns rates of mass and energy flows into the layers from all vents in the building.
C
C     Arguments: TSEC  current simulation time (s)
C                EPSP  pressure error tolerance
C                NPROD
C                UFLW
C
C     Revision History:
C            4/24/95  remove declaration of yceil, to eliminate FLINT
C                     complaint
C            7/13/95  reduced the number of vent calcuations performed during
C                     a Jacobian calculation
C            2/5/96   changed FMODJAC references from 2 to ON
C            7/22/96  update hall info for vents connected to a hall
C            2/14/97  made memory requirements smaller by changing
C                     several o(n**2) data structures to o(n)
C                     Also, associated added wind pressure rise calculation
C                     to vents instead of rooms (now we can have wind come in
C                     one window of a room and go out another.
C           5/11/98 by GPF:
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
C			5/7/03 by wwj : move initialization of reporting structure (vss ...) from vent to here (hflow)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "flwptrs.fi"
      include "vntslb.fi"
      include "opt.fi"
      include "vents.fi"
C
      DIMENSION CONL(MXPRD,2), CONU(MXPRD,2), PMIX(MXPRD)
      DIMENSION UFLW(NR,MXPRD+2,2)
      DIMENSION UFLW3(2,MXPRD+2,2), UFLW2(2,MXPRD+2,2)
      DIMENSION YFLOR(2), YLAY(2), PFLOR(2)
      DIMENSION DENL(2), DENU(2), TU(2), TL(2)
      DIMENSION RSLAB(MXSLAB), TSLAB(MXSLAB), YSLAB(MXSLAB),
     +    XMSLAB(MXSLAB), QSLAB(MXSLAB), CSLAB(MXSLAB,MXPRD),
     +    PSLAB(MXSLAB,MXPRD)
      DIMENSION VNTOPN(NV)
      DIMENSION UFLW0(NR,NS+2,2)
      LOGICAL VENTFLG(MXVENT), ROOMFLG(NR), anyvents
      SAVE UFLW0
	real*8 factor2, qchfraction, height, width

C    TEMPORARY DECLARATION

      NIRM = NM1
C
      XX0 = 0.0D0
      DO 20 IFROM = 1, NIRM
        DO 10 IPROD = 1, NPROD + 2
          UFLW(IFROM,IPROD,LOWER) = XX0
          UFLW(IFROM,IPROD,UPPER) = XX0
   10   CONTINUE
   20 CONTINUE
      IF (OPTION(FHFLOW)/=ON) RETURN

      CALL VENTFLAG(VENTFLG,ROOMFLG,ANYVENTS)
      IF(ANYVENTS)THEN
      DO 80 I = 1, NVENTS
        IF(.NOT.VENTFLG(I))GO TO 80
        IROOM1 = IZVENT(I,1)
        IROOM2 = IZVENT(I,2)
        IK = IZVENT(I,3)

C       SETUP DATA STRUCTURES FOR FROM ROOM

        CALL GETVAR(I,IROOM1,IROOM2,NPROD,YFLOR(1),YLAY(1),PFLOR(1),
     +      DENL(1),DENU(1),CONL(1,1),CONU(1,1),TL(1),TU(1))

C       SETUP DATA STRUCTURES FOR TO ROOM

        CALL GETVAR(I,IROOM2,IROOM1,NPROD,YFLOR(2),YLAY(2),PFLOR(2),
     +      DENL(2),DENU(2),CONL(1,2),CONU(1,2),TL(2),TU(2))

C       convert vent dimensions to absolute dimensions

        YVBOT = ZZVENT(I,1) + YFLOR(1)
        YVTOP = ZZVENT(I,2) + YFLOR(1)
        YLAY(1) = YLAY(1) + YFLOR(1)
        YLAY(2) = YLAY(2) + YFLOR(2)

C       USE NEW INTERPOLATOR TO FIND VENT OPENING FRACTION

        IM = MIN(IROOM1,IROOM2)
        IX = MAX(IROOM1,IROOM2)
	  factor2 = qchfraction (qcvh, ijk(im,ix,ik),tsec)
        HEIGHT = ZZVENT(I,2) - ZZVENT(I,1)
        WIDTH = ZZVENT(I,3)
	  avent = factor2 * height * width

C*** augment floor pressure in the second room by the pressure induced by wind.
C***  (note this augmentation will be different for each vent)

        PFLOR(2) = PFLOR(2) + ZZVENT(I,6)
        IF (AVENT>=1.D-10) THEN
          CALL VENT(YFLOR,YLAY,TU,TL,DENL,DENU,PFLOR,YVTOP,YVBOT,AVENT,
     +        CP,CONL,CONU,NPROD,MXPRD,MXSLAB,EPSP,CSLAB,PSLAB,QSLAB,
     +        VSS(1,I),VSA(1,I),VAS(1,I),VAA(1,I),DIRS12,DPV1M2,RSLAB,
     +        TSLAB,YSLAB,YVELEV,XMSLAB,NSLAB,NNEUT,VENTVEL)

C*** UPDATE HALL INFO FOR VENTS CONNECTED FROM FIRE ROOM TO HALL

          IF(UPDATEHALL)THEN
            VENTHEIGHT = YVTOP - YVBOT
            IF(IZVENT(I,4)==1)THEN
              VLAYERDEPTH = YVTOP - YLAY(2)
              IF(VLAYERDEPTH>VENTHEIGHT)VLAYERDEPTH = VENTHEIGHT
              CALL SETHALL(1,I,IROOM1,TSEC,WIDTH,
     .              TSLAB(NSLAB),-VENTVEL,VLAYERDEPTH)
            ENDIF
            IF(IZVENT(I,5)==1)THEN
              VLAYERDEPTH = YVTOP - YLAY(1)
              IF(VLAYERDEPTH>VENTHEIGHT)VLAYERDEPTH = VENTHEIGHT
              CALL SETHALL(1,I,IROOM2,TSEC,WIDTH,
     .               TSLAB(NSLAB),VENTVEL,VLAYERDEPTH)
            ENDIF
          ENDIF

C         COPY NUMBER OF NEUTRAL PLANES TO CFAST DATA STRUCTURES

          NEUTRAL(IROOM1,IROOM2) = NNEUT
          NEUTRAL(IROOM2,IROOM1) = NNEUT

C     COPY FLOWS INTO the CFAST DATA STRUCTure
C	This data structure is for reporting purposes only;

          IIJK = IJK(IROOM1,IROOM2,IK)
          SS1(IIJK) = VSS(1,I)
          SS2(IIJK) = VSS(2,I)
          SA1(IIJK) = VSA(1,I)
          SA2(IIJK) = VSA(2,I)
          AS1(IIJK) = VAS(1,I)
          AS2(IIJK) = VAS(2,I)
          AA1(IIJK) = VAA(1,I)
          AA2(IIJK) = VAA(2,I)

          CALL FLOGO1(DIRS12,YSLAB,XMSLAB,NSLAB,YLAY,QSLAB,PSLAB,MXPRD,
     +        NPROD,MXSLAB,UFLW2)

C         CALCULATE ENTRAINMENT TYPE MIXING AT THE VENTS

          IF (OPTION(FENTRAIN)==ON) THEN
			CALL ENTRAIN(DIRS12,YSLAB,XMSLAB,NSLAB,TU,TL,CP,YLAY,CONL,
     +          CONU,PMIX,MXPRD,NPROD,YVBOT,YVTOP,UFLW3,VSAS(1,I),
     +          VASA(1,I))
            SAU1(IIJK) = VSAS(2,I)
            SAU2(IIJK) = VSAS(1,I)
            ASL1(IIJK) = VASA(2,I)
            ASL2(IIJK) = VASA(1,I)
          ELSE
            SAU1(IIJK) = XX0
            SAU2(IIJK) = XX0
            ASL1(IIJK) = XX0
            ASL2(IIJK) = XX0
          endif

C         SUM FLOWS FROM BOTH ROOMS FOR EACH LAYER AND TYPE OF PRODUCT
C         (BUT ONLY IF THE ROOM IS AN INSIDE ROOM)

          IF (IROOM1>=1.AND.IROOM1<=NIRM) THEN
            DO 40 IPROD = 1, NPROD + 2
              UFLW(IROOM1,IPROD,LOWER) = UFLW(IROOM1,IPROD,LOWER) +
     +            UFLW2(1,IPROD,L)
              UFLW(IROOM1,IPROD,UPPER) = UFLW(IROOM1,IPROD,UPPER) +
     +            UFLW2(1,IPROD,U)
   40       CONTINUE
            IF (OPTION(FENTRAIN)==ON) THEN
              DO 50 IPROD = 1, NPROD + 2
                UFLW(IROOM1,IPROD,LOWER) = UFLW(IROOM1,IPROD,LOWER) +
     +              UFLW3(1,IPROD,L)
                UFLW(IROOM1,IPROD,UPPER) = UFLW(IROOM1,IPROD,UPPER) +
     +              UFLW3(1,IPROD,U)
   50         CONTINUE
            endif
          endif
          IF (IROOM2>=1.AND.IROOM2<=NIRM) THEN
            DO 60 IPROD = 1, NPROD + 2
              UFLW(IROOM2,IPROD,LOWER) = UFLW(IROOM2,IPROD,LOWER) +
     +            UFLW2(2,IPROD,L)
              UFLW(IROOM2,IPROD,UPPER) = UFLW(IROOM2,IPROD,UPPER) +
     +            UFLW2(2,IPROD,U)
   60       CONTINUE
            IF (OPTION(FENTRAIN)==ON) THEN
              DO 70 IPROD = 1, NPROD + 2
                UFLW(IROOM2,IPROD,LOWER) = UFLW(IROOM2,IPROD,LOWER) +
     +              UFLW3(2,IPROD,L)
                UFLW(IROOM2,IPROD,UPPER) = UFLW(IROOM2,IPROD,UPPER) +
     +              UFLW3(2,IPROD,U)
   70         CONTINUE
            endif
          endif
        endif
   80 CONTINUE
      endif

      IF(OPTION(FMODJAC)==ON)THEN
        IF(JACCOL==0)THEN

C*** we need to save the solution for later jacobian calculations

          DO 140 IROOM = 1, NM1
            DO 150 IPROD = 1, NPROD + 2
              UFLW0(IROOM,IPROD,LOWER) = UFLW(IROOM,IPROD,LOWER)
              UFLW0(IROOM,IPROD,UPPER) = UFLW(IROOM,IPROD,UPPER)
  150       CONTINUE
  140     CONTINUE
         ELSEIF(JACCOL>0)THEN

C*** we are computing a jacobian, so get previously save solution for rooms
C    that are not affected by perturbed solution variable

          DO 160 IROOM = 1, NM1
            IF(.NOT.ROOMFLG(IROOM))THEN
              DO 170 IPROD = 1, NPROD + 2
                UFLW(IROOM,IPROD,LOWER) = UFLW0(IROOM,IPROD,LOWER)
                UFLW(IROOM,IPROD,UPPER) = UFLW0(IROOM,IPROD,UPPER)
  170         CONTINUE
            ENDIF
  160     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
 
      SUBROUTINE ENTRAIN(DIRS12,YSLAB,XMSLAB,NSLAB,TU,TL,CP,YLAY,CONL,
     +    CONU,PMIX,MXPRD,NPROD,YVBOT,YVTOP,UFLW3,VSAS,VASA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     ENTRAIN
C
C     Source File: ENTRAIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: DIRS12
C    INPUT
C    -----
C   DIRS12 - A MEASURE OF THE DIRECTION OF THE ROOM 1 TO ROOM
C                       2 FLOW IN EACH SLAB
C   YSLAB  - SLAB HEIGHTS IN ROOMS 1,2 ABOVE DATUM ELEVATION [M]
C   XMSLAB - MAGNITUDE OF THE MASS FLOW RATE IN SLABS [KG/S]
C   NSLAB  - NUMBER OF SLABS BETWEEN BOTTOM AND TOP OF VENT
C   TU     - UPPER LAYER TEMPERATURE IN EACH ROOM [K]
C   TL     - LOWER LAYER TEMPERATURE IN EACH ROOM [K]
C   YLAY   - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C
C   OUTPUT
C   ------
C   UFLW3(I,1,J), I=1 OR 2, J=1 OR 2 - MASS FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE TO ENTRAINMENT
C   UFLW3(I,2,J), I=1 OR 2, J=1 OR 2 - ENTHALPY FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I ENTRAINMENT
C   UFLW3(I,2+K,J), I=1 OR 2, K=1 TO NPROD, J=1 OR 2 - PRODUCT K FLOW
C            RATE TO UPPER (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE
C            ENTRAINMENT
C
C        Created:  
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "flwptrs.fi"
      INTEGER DIRS12(10)
      DIMENSION YSLAB(10), XMSLAB(10), PMIX(MXPRD)
      DIMENSION TU(2), TL(2), YLAY(2)
      DIMENSION CONL(MXPRD,2), CONU(MXPRD,2)
      DIMENSION UFLW3(2,MXPRD+2,2)
      DIMENSION VSAS(2), VASA(2)
      PARAMETER (XX0 = 0.0D0)
C
C*** INITIALIZE OUTPUTS
C
      DO 20 I = 1, 2
        DO 10 IPROD = 1, NPROD + 2
          UFLW3(I,IPROD,L) = XX0
          UFLW3(I,IPROD,U) = XX0
   10   CONTINUE
        VSAS(I) = XX0
        VASA(I) = XX0
   20 CONTINUE
C    
C
      DO 60 N = 1, NSLAB
C
C*** ELIMINATE CASES WHERE ENTRAINMENT DOES NOT OCCUR
C    I.E. A SLAB WHICH IS ADJACENT TO THE UPPER LAYER ON BOTH SIDES
C      OR A SLAB WHICH IS ADJACENT TO THE LOWER LAYER ON BOTH SIDES
C
        IF (YSLAB(N)<YLAY(1).OR.YSLAB(N)<YLAY(2)) THEN
          IF (YSLAB(N)>=YLAY(1).OR.YSLAB(N)>=YLAY(2)) THEN
C
C*** SLABS WITH NO FLOW CAUSE NO ENTRAINMENT
C
            IF (XMSLAB(N)/=XX0) THEN
C
C*** DETERMINE WHAT ROOM FLOW IS COMING FROM
C
              IF (DIRS12(N)==1) THEN
                IFROM = 1
                ITO = 2
              ELSE IF (DIRS12(N)==0) THEN
C
C*** NO FLOW IN THIS SLAB SO WE CAN SKIP IT
C    (WE SHOULD NEVER GET HERE)
C
                GO TO 60
              ELSE IF (DIRS12(N)==-1) THEN
                IFROM = 2
                ITO = 1
              endif
C
C***  DETERMINE TEMPERATURE AND PRODUCT CONCENTRATIONS
C     OF ENTRAINED FLOW
C
              IF (YSLAB(N)<YLAY(ITO)) THEN
                TMIX = TL(ITO)
                DO 30 IPROD = 1, NPROD
                  PMIX(IPROD) = CONL(IPROD,ITO)
   30           CONTINUE
              ELSE
                TMIX = TU(ITO)
                DO 40 IPROD = 1, NPROD
                  PMIX(IPROD) = CONU(IPROD,ITO)
   40           CONTINUE
              endif
C         
C*** COMPUTE THE SIZE OF THE ENTRAINED MASS FLOW
C
              IF (YSLAB(N)>=YLAY(IFROM)) THEN
C
C*** INTO UPPER
C
                IF (TU(IFROM)>TL(ITO).AND.XMSLAB(N)/=XX0) THEN
                  ZD = MAX(XX0,YLAY(ITO)-MAX(YVBOT,YLAY(IFROM)))
                  CALL ENTRFL(TU(IFROM),TL(ITO),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,U))
                  UFLW3(ITO,M,L) = -UFLW3(ITO,M,U)
                  VSAS(ITO) = UFLW3(ITO,M,U)
                endif
              ELSE
C
C*** INTO LOWER
C
                IF (TL(IFROM)<TU(ITO).AND.XMSLAB(N)/=XX0) THEN
C               ZD = MAX(XX0,YLAY(IFROM)-MAX(YVBOT,YLAY(ITO)))

C*** need to re-work distance zd for both into upper and into
C         upper case.  the above doesn't work for all cases

                  ZD = MIN(YVTOP,YLAY(IFROM)) - MAX(YLAY(ITO),YVBOT)
                  CALL ENTRFL(TU(ITO),TL(IFROM),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,L))
C*** The following factor (0.25 as of 10/1/93) now multiplies the lower layer
C*** entrainment to try to approximate the reduced Kelvin-Helmholz type mixing.
C*** This observation arises from the problems encountered in the test
C*** case temper1.dat.  This needs to be researched carefully!

                  UFLW3(ITO,M,L) = UFLW3(ITO,M,L) * 0.25D0
                  VASA(ITO) = UFLW3(ITO,M,L)
                  UFLW3(ITO,M,U) = -UFLW3(ITO,M,L)
                endif
              endif
C
C*** COMPUTE ENTHALPY AND PRODUCT FLOW RATES OF ENTRAINED FLOW
C    FROM THE MASS FLOW RATE
C
              UFLW3(ITO,Q,L) = CP * UFLW3(ITO,M,L) * TMIX
              UFLW3(ITO,Q,U) = CP * UFLW3(ITO,M,U) * TMIX
              DO 50 IPROD = 3, 2 + NPROD
                 UFLW3(ITO,IPROD,L) = UFLW3(ITO,M,L) * PMIX(IPROD-2)
                 UFLW3(ITO,IPROD,U) = UFLW3(ITO,M,U) * PMIX(IPROD-2)
   50         CONTINUE
            endif
          endif
        endif
   60 CONTINUE
      RETURN
      END
     
      
      SUBROUTINE ENTRFL(TU,TL,FMD,Z,FMZ)
C
C     For the reference for this correlation, see the comments
C     in the routine "firplm."  The offset for the formulation of
C     an equivalent door jet is provided by requiring the plume
C     be long enough to be the appropriate plume for the fire of size
C     QJ.  Note that McCaffrey's units are kilojoules.  Also we assume
C     that the plume is round as in McCaffrey's plume.  This should
C     be modified to account for the flat plume verus the round
C     plume in the theory.
C
C     update history
C
C     July 24, 1990 modified coefiscients so that McCaffrey correlation is
C                   continuous.  the coeff's are calculated the first time
C                   this routine is called.  gpf

      use cparams
      include "precis.fi"
      include "cfast.fi"
      LOGICAL FIRSTC
      SAVE FIRSTC, A1, A2, A3, E1, E2, E3, F1, F2
      DATA FIRSTC /.TRUE./
C
C*** DEFINE ASSIGNMENT STATEMENT SUBROUTINES TO COMPUTE THREE PARTS
C    OF CORRELATION
C
      FM1(ZQ) = ZQ ** .566D0
      FM2(ZQ) = ZQ ** .909D0
      FM3(ZQ) = ZQ ** 1.895D0
C
C*** FIRST TIME IN FIRPLM CALCULATE COEFF'S 
C    TO INSURE THAT MCCAFFREY CORRELATION IS CONTINUOUS.
C    THAT IS, FOR A1 = .011, COMPUTE A2, A3 SUCH THAT
C  
C     A1*ZQ**.566 = A2*ZQ**.909  FOR ZQ = .08
C     A2*ZQ**.909 = A3*ZQ**1.895 FOR ZQ = .2
C
      IF (FIRSTC) THEN
C
C*** RESET FLAG SO THIS CODE DOESN'T GET EXECUTED NEXT TIME IN 
C    THIS ROUTINE
C
        FIRSTC = .FALSE.
C
C*** BREAKPOINTS FOR "FORWARD" CORRELATION
C
        T1 = .08D0
        T2 = .20D0
C
C*** COEF'S FOR "FORWARD" CORRELATION
C
        A1 = .011D0
        A2 = A1 * FM1(T1) / FM2(T1)
        A3 = A2 * FM2(T2) / FM3(T2)
C
C*** EXPONENTS FOR "INVERSE" CORRELATION
C
        E1 = 1.0D0 / .566D0
        E2 = 1.0D0 / .909D0
        E3 = 1.0D0 / 1.895D0
C
C*** BREAKPOINTS FOR "INVERSE" CORRELATION
C
        F1 = A1 * FM1(T1)
        F2 = A2 * FM2(T2)
      endif
C
      XQJ = CP * (TU-TL) * 0.001D0
      QJ = XQJ * FMD
      FMDQJ = 1.D0 / XQJ
      IF (FMDQJ>=0.0D0.AND.FMDQJ<=F1) THEN
        Z0DQ = (FMDQJ/A1) ** E1
      ELSE IF (FMDQJ>F1.AND.FMDQJ<=F2) THEN
        Z0DQ = (FMDQJ/A2) ** E2
      ELSE
        Z0DQ = (FMDQJ/A3) ** E3
      endif
C
      ZDQ = Z / QJ ** 0.4D0 + Z0DQ
      IF (ZDQ>0.2D0) THEN
        FMZ = A3 * FM3(ZDQ) * QJ
      ELSE IF (ZDQ>0.08D0) THEN
        FMZ = A2 * FM2(ZDQ) * QJ
      ELSE
        FMZ = A1 * FM1(ZDQ) * QJ
      endif
C
      XX0 = 0.0D0
      FMZ = MAX(XX0,FMZ-FMD)
      RETURN
      END

      subroutine ventflag(ventflg,roomflg,anyvents)

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "flwptrs.fi"
      include "vntslb.fi"
      include "opt.fi"
      include "vents.fi"

      LOGICAL VENTFLG(MXVENT), ROOMFLG(NR), anyvents

C*** TURN ALL VENTS ON
      anyvents = .true.
      DO 100 I = 1, NVENTS
        VENTFLG(I) = .TRUE.
  100 CONTINUE

C*** If the 2nd modified jacobian option is on and a Jacobian is being computed
C    (JACCOL>0) then compute vent flows only for vents that that are connected
C    to rooms whose pressure, layer height, layer temperature,  or oxygen level
C    is being perturbed.

      IF(OPTION(FMODJAC)==ON)THEN
        IF(JACCOL>0)THEN

C*** we are computing a Jacobian

          IEQTYP = IZEQMAP(JACCOL,1)
          IROOM = IZEQMAP(JACCOL,2)
          anyvents = .false.
          DO 110 I = 1, NVENTS
            VENTFLG(I) = .FALSE.
  110     CONTINUE
          DO 120 I = 1, NM1
            ROOMFLG(I) = .FALSE.
  120     CONTINUE
          IF(IEQTYP==EQP.OR.IEQTYP==EQTU.OR.IEQTYP==EQVU.OR.
     .       IEQTYP==EQTL.OR.IEQTYP==EQOXYL.OR.IEQTYP==EQOXYU)THEN

C*** determine all rooms connected to perturbed rooms

            DO 130 I = 1, NVENTS
              IROOM1 = IZVENT(I,1)
              IROOM2 = IZVENT(I,2)
              IF(IROOM==IROOM1.OR.IROOM==IROOM2)THEN
                ROOMFLG(IROOM1) = .TRUE.
                ROOMFLG(IROOM2) = .TRUE.
              ENDIF
  130       CONTINUE
            ROOMFLG(NM1+1) = .FALSE.

C*** determine all vents connected to the above rooms

            DO 180 I = 1, NVENTS
              IROOM1 = IZVENT(I,1)
              IROOM2 = IZVENT(I,2)
              IF(ROOMFLG(IROOM1).OR.ROOMFLG(IROOM2))then
                VENTFLG(I) = .TRUE.
                anyvents = .true.
              endif
  180       CONTINUE
          ENDIF
        ENDIF
      ENDIF

      return
      end

      SUBROUTINE VENT(YFLOR,YLAY,TU,TL,DENL,DENU,PFLOR,YVTOP,YVBOT,
     +    AVENT,CP,CONL,CONU,NPROD,MXPRD,MXSLAB,EPSP,CSLAB,PSLAB,QSLAB,
     +    VSS,VSA,VAS,VAA,DIRS12,DPV1M2,RSLAB,TSLAB,YSLAB,YVELEV,XMSLAB,
     +    NSLAB,NNEUT,VENTVEL)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     VENT
C
C     Source File: VENT.SOR
C
C     Functional Class:  
C
C     Description:  Calculation of the flow of mass, enthalpy, oxygen
C           and other products of combustion through a vertical,
C           constant-width vent in a wall segment common to two rooms.
C           The subroutine uses input data describing the two-layer
C           environment in each of the two rooms and other input data
C           calculated in subroutine comwl1.
C
C   INPUT
C   -----
C   YFLOR - HEIGHT OF FLOOR ABOVE DATUM ELEVATION [M]
C   YLAY  - HEIGHT OF LAYER ABOVE DATUM ELEVATION [M]
C   TU    - UPPER LAYER TEMPERATURE [K]
C   TL    - LOWER LAYER TEMPERATURE [K]
C   DENL  - LOWER LAYER DENSITY [KG/M**3]
C   DENU  - UPPER LAYER DENSITY [KG/M**3]
C   PFLOR - PRESSURE AT FLOOR ABOVE DATUM PRESSURE
C                      [KG/(M*S**2) = PASCAL]
C   YVTOP - ELEVATION OF TOP OF VENT ABOVE DATUM ELEVATION [M]
C   YVBOT - ELEVATION OF BOTTOM OF VENT ABOVE DATUM ELEVATION [M]
C   AVENT - AREA OF THE VENT [M**2]
C   DP1M2 - PRESSURE IN ROOM 1 - PRESSURE IN ROOM 2 AT
C                      ELEVATIONS YELEV [KG/(M*S**2) = PASCAL]
C   CP    - SPECIFIC HEAT [W*S/(KG*K)]
C   CONL  - CONCENTRATION OF EACH PRODUCT IN LOWER LAYER
C                      [UNIT OF PRODUCT/(KG LAYER)]
C   CONU  - CONCENTRATION OF EACH PRODUCT IN UPPER LAYER
C                      [UNIT OF PRODUCT/(KG LAYER)]
C   NPROD - NUMBER OF PRODUCTS IN CURRENT SCENARIO
C   MXPRD - MAXIMUM NUMBER OF PRODUCTS CURRENTLY AVAILABLE
C   MXSLAB- MAXIMUM NUMBER OF SLABS CURRENTLY AVAILABLE
C   EPSP  - ERROR TOLERANCE FOR PRESSURES AT FLOOR
C
C   OUTPUT
C   ------
C   CSLAB  - CONCENTRATION OF OTHER PRODUCTS IN EACH SLAB
C                       [UNIT PRODUCT/(KG SLAB)]
C   PSLAB  - AMOUNT OF OTHER PRODUCTS IN EACH SLAB [UNIT OF
C                       PRODUCT/S]
C   QSLAB  - ENTHALPY FLOW RATE IN EACH SLAB [W]
C   DIRS12 - A MEASURE OF THE DIRECTION OF THE ROOM 1 TO ROOM
C                       2 FLOW IN EACH SLAB
C   RSLAB  - DENSITY OF THE FLOW IN EACH SLAB [KG/M**3]
C   TSLAB  - ABSOLUTE TEMPERATURE OF THE FLOW IN EACH SLAB [K]
C   YSLAB  - ELEVATIONS ABOVE THE DATUM ELEVATION OF THE
C                       CENTROIDS OF MOMENTUM OF EACH SLAB [M]
C   YVELEV - ELEVATIONS ABOVE THE DATUM ELEVATIONS OF VENT
C                       BOUNDARIES, LAYERS, AND NEUTRAL PLANES [M]
C   XMSLAB - MAGNITUDE OF THE MASS FLOW RATE IN SLABS [KG/S]
C   NVELEV - NUMBER OF UNIQUE ELEVATIONS DELINEATING SLABS
C   NSLAB  - NUMBER OF SLABS BETWEEN BOTTOM AND TOP OF THE VENT
C     Revision History:
C        Created:  
C        Modified by GPF 7/22/96
C                  added vent velocity calculation
C		Modified by wwj 5/7/03 - move initiazation to hflow
C
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      DIMENSION YFLOR(*), YLAY(*), TU(*), TL(*), DENL(*), DENU(*)
      DIMENSION PFLOR(*)
      DIMENSION PSLAB(MXSLAB,*)
      DIMENSION CSLAB(MXSLAB,*), CONL(MXPRD,2), CONU(MXPRD,2)
      DIMENSION YELEV(10), DP1M2(10), DPV1M2(10)
      DIMENSION XMSLAB(*), YN(10)
      DIMENSION RSLAB(*), TSLAB(*), YSLAB(*), YVELEV(*)
      DIMENSION QSLAB(*)
      DIMENSION VSS(2), VSA(2), VAS(2), VAA(2)
      INTEGER DIRS12(*)

      VENTVEL = 0.0D0
C*** CREATE INITIAL ELEVATION HEIGHT ARRAY (IGNORING NEUTRAL PLANES)

      CALL GETELEV(YVBOT,YVTOP,YLAY,YELEV,NELEV)

C*** FIND PRESSURE DROPS AT ABOVE ELEVATIONS

      CALL DELP(YELEV,NELEV,YFLOR,YLAY,DENL,DENU,PFLOR,EPSP,DP1M2)

C*** FIND NEUTRAL PLANES

      NVELEV = 1
      NNEUT = 0
      XX0 = 0.0D0
      DO 10 I = 1, NELEV - 1
        YVELEV(NVELEV) = YELEV(I)
        DPV1M2(NVELEV) = DP1M2(I)
        NVELEV = NVELEV + 1

C     A NEUTRAL PLANE LIES BETWEEN TWO ELEVATIONS HAVING 
C     OPPOSITE SIGNED PRESSURE DROPS

        IF (DP1M2(I)*DP1M2(I+1)<0.0D0) THEN
          NNEUT = NNEUT + 1
          DPP = DP1M2(I) - DP1M2(I+1)
          YN(NNEUT) = (YELEV(I+1)*DP1M2(I)-YELEV(I)*DP1M2(I+1)) / DPP
C     FAIL SAFE IN CASE INTERPOLATION CALCULATION FAILS

          IF (YN(NNEUT)<YELEV(I).OR.YN(NNEUT)>YELEV(I+1)) THEN
            YN(NNEUT) = (YELEV(I)+YELEV(I+1)) / 2.0D0
          endif
          YVELEV(NVELEV) = YN(NNEUT)
          DPV1M2(NVELEV) = 0.0D0
          NVELEV = NVELEV + 1
        endif
   10 CONTINUE
      YVELEV(NVELEV) = YELEV(NELEV)
      DPV1M2(NVELEV) = DP1M2(NELEV)
      NSLAB = NVELEV - 1
      DO 20 I = 1, NSLAB
        YSLAB(I) = (YVELEV(I)+YVELEV(I+1)) / 2.0D0
   20 CONTINUE

C     INITIALIZE CFAST DATA STRUCTURES FOR FLOW STORAGE

      DO 70 N = 1, NSLAB

C     DETERMINE WHETHER TEMPERATURE AND DENSITY PROPERTIES SHOULD COME FROM ROOM 1 OR ROOM 2

        PTEST = DPV1M2(N+1) + DPV1M2(N)
        IF (PTEST>0.0D0) THEN
          JROOM = 1
          DIRS12(N) = 1
        ELSE IF (PTEST<0.0D0) THEN
          DIRS12(N) = -1
          JROOM = 2
        ELSE
          DIRS12(N) = 0
          JROOM = 1
        endif

C    DETERMINE WHETHER TEMPERATURE AND DENSITY PROPERTIES
C    SHOULD COME FROM UPPER OR LOWER LAYER

        IF (YSLAB(N)<=YLAY(JROOM)) THEN
          TSLAB(N) = TL(JROOM)
          RSLAB(N) = DENL(JROOM)
          DO 30 IPROD = 1, NPROD
            CSLAB(N,IPROD) = CONL(IPROD,JROOM)
   30     CONTINUE
        ELSE
          TSLAB(N) = TU(JROOM)
          RSLAB(N) = DENU(JROOM)
          DO 40 IPROD = 1, NPROD
            CSLAB(N,IPROD) = CONU(IPROD,JROOM)
   40     CONTINUE
        endif

C    FOR NONZERO-FLOW SLABS DETERMINE XMSLAB(N) AND YSLAB(N)

        XMSLAB(N) = 0.0D0
        QSLAB(N) = 0.D0
        DO 50 IPROD = 1, NPROD
          PSLAB(N,IPROD) = 0.0D0
   50   CONTINUE
        P1 = ABS(DPV1M2(N))
        P2 = ABS(DPV1M2(N+1))
        P1RT = SQRT(P1)
        P2RT = SQRT(P2)

C    IF BOTH CROSS PRESSURES ARE 0 THEN THEN THERE IS NO FLOW
        IF (P1>XX0.OR.P2>XX0) THEN
          R1 = MAX(RSLAB(N),XX0)
          Y2 = YVELEV(N+1)
          Y1 = YVELEV(N)
          CVENT = .70D0

          AREA = AVENT * (Y2-Y1) / (YVTOP-YVBOT)
          R1M8 = 8.0D0*R1
          XMSLAB(N) = CVENT * SQRT(R1M8) * AREA * (P2+P1RT*P2RT+P1) / (
     +        P2RT+P1RT) / 3.0D0
          VENTVEL = 0.0D0
          IF(N==NSLAB)THEN
            IF(AREA/=0.0D0.AND.R1/=0.0D0)THEN
              VENTVEL = XMSLAB(N)/(AREA*R1)
	      IF(DIRS12(N)<0)VENTVEL = -VENTVEL
            ENDIF
          ENDIF
          QSLAB(N) = CP * XMSLAB(N) * TSLAB(N)
          SUM = 0.0D0
          DO 60 IPROD = 1, NPROD
            PSLAB(N,IPROD) = CSLAB(N,IPROD) * XMSLAB(N)
            SUM = SUM + PSLAB(N,IPROD)
   60     CONTINUE
        endif

C    CONSTRUCT CFAST DATA STRUCTURES SS, SA, AS, AA

        YS = YSLAB(N)
        IF (YS>MAX(YLAY(1),YLAY(2))) THEN
          IF (DIRS12(N)>0) THEN
            VSS(1) = XMSLAB(N)
          ELSE
            VSS(2) = XMSLAB(N)
          endif
        ELSE IF (YS<MIN(YLAY(1),YLAY(2))) THEN
          IF (DIRS12(N)>0) THEN
            VAA(1) = XMSLAB(N)
          ELSE
            VAA(2) = XMSLAB(N)
          endif
        ELSE IF (YS>YLAY(1)) THEN
          IF (DIRS12(N)>0) THEN
            VSA(1) = XMSLAB(N)
          ELSE
            VAS(2) = XMSLAB(N)
          endif
        ELSE IF (YS>YLAY(2)) THEN
          IF (DIRS12(N)>0) THEN
            VAS(1) = XMSLAB(N)
          ELSE
            VSA(2) = XMSLAB(N)
          endif
        endif
   70 CONTINUE
      RETURN
      END

      SUBROUTINE GETELEV(YVBOT,YVTOP,YLAY,YELEV,NELEV)
      include "precis.fi"
      DIMENSION YELEV(*), YLAY(*)
      YMIN = MIN(YLAY(1),YLAY(2))
      YMAX = MAX(YLAY(1),YLAY(2))
      IF (YMAX>=YVTOP.AND.(YMIN>=YVTOP.OR.YMIN<=YVBOT)) THEN
        NELEV = 2
        YELEV(1) = YVBOT
        YELEV(2) = YVTOP
      ELSE IF (YMAX<=YVBOT) THEN
        NELEV = 2
        YELEV(1) = YVBOT
        YELEV(2) = YVTOP
      ELSE
        IF (YMAX>=YVTOP.AND.YMIN>YVBOT) THEN
          NELEV = 3
          YELEV(1) = YVBOT
          YELEV(2) = YMIN
          YELEV(3) = YVTOP
        ELSE IF (YMIN<=YVBOT.AND.YMAX<YVTOP) THEN
          NELEV = 3
          YELEV(1) = YVBOT
          YELEV(2) = YMAX
          YELEV(3) = YVTOP
        ELSE
          NELEV = 4
          YELEV(1) = YVBOT
          YELEV(2) = YMIN
          YELEV(3) = YMAX
          YELEV(4) = YVTOP
        endif
      endif
      RETURN
      END

      SUBROUTINE GETVAR(IVENT,IROOM,IROOM2,NPROD,YFLOR,YLAY,PFLOR,
     +                  DENL,DENU,CONL,CONU,TL,TU)
      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "vents.fi"
C*BEG
C
C***  GETVAR  SPECIFIC - ROUTINE TO INTERFACE BETWEEN CCFM.VENTS GLOBAL
C             DATA STRUCTURES AND VENT (BOTH NATURAL AND FORCED) DATA
C             STRUCTURES.
C
C***  SUBROUTINE ARGUMENTS
C
C  INPUT
C  -----
C  IVENT - VENT NUMBER
C  IROOM - ROOM NUMBER
C
C  OUTPUT
C  ------
C  YFLOR   HEIGHT OF FLOOR ABOVE DATUM ELEVATION [M]
C  YLAY    HEIGHT OF LAYER ABOVE DATUM ELEVATION [M]
C  PFLOR   PRESSURE AT FLOOR RELATIVE TO AMBIENT [P]
C  DENL    DENSITY OF LOWER LAYER [KG/M**3]
C  DENU    DENSITY OF UPPER LAYER [KG/M**3]
C  CONL    CONCENTRATION OF LOWER LAYER FOR EACH PRODUCT
C          [UNIT OF PRODUCT/KG OF LAYER]
C  CONU    CONCENTRATION OF UPPER LAYER FOR EACH PRODUCT
C          [UNIT OF PRODUCT/KG OF LAYER]
C  TL      TEMPERATURE OF LOWER LAYER [K]
C  TU      TEMPERATURE OF UPPER LAYER [K]
C
C     Revision History:
C        Created:  
C        Modified: 07/22/1996 by GPF:
C                  Added logic to use lower layer if hall flow has not
C                  reached vent yet.
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C*END
      DIMENSION CONL(MXPRD), CONU(MXPRD)
      LOGICAL HALLFLAG
      INTEGER UP

      XX0 = 0.0D0
      HALLFLAG = .FALSE.

C*** for rooms that are halls only use upper layer properties
C    if the ceiling jet is beyond the vent

      UP = UPPER
C
      IF (IROOM<N) THEN
        YFLOR = ZZYFLOR(IROOM)
        PFLOR = ZZRELP(IROOM)
        YLAY = ZZHLAY(IROOM,LOWER)

C*** this is a hall, the vent number is defined and flow is occuring

        IF(IZHALL(IROOM,IHROOM)==1.AND.IVENT/=0.AND.
     .                             IZHALL(IROOM,IHMODE)==IHDURING)THEN
          VENTDIST = ZZVENTDIST(IROOM,IVENT)
          IF(VENTDIST>XX0)THEN
            TIME0 = ZZHALL(IROOM,IHTIME0)
            VEL = ZZHALL(IROOM,IHVEL)
            CJETDIST = VEL*(STIME-TIME0)
            IF(CJETDIST<VENTDIST)THEN
              UP = LOWER
             ELSE
              UP = UPPER
              HALLFLAG = .TRUE.
            ENDIF
           ELSE
            UP = LOWER
          ENDIF
        ENDIF

        DENU = ZZRHO(IROOM,UP)
        DENL = ZZRHO(IROOM,LOWER)
        DO 10 IPROD = 1, NPROD
          IP = IZPMAP(IPROD+2) - 2
          CONL(IPROD) = ZZCSPEC(IROOM,LOWER,IP)
          CONU(IPROD) = ZZCSPEC(IROOM,UP,IP)
   10   CONTINUE
        TU = ZZTEMP(IROOM,UP)
        TL = ZZTEMP(IROOM,LOWER)
        ZLOC = HR(IROOM) - ZZHALL(IROOM,IHDEPTH)/2.0D0
        IF(HALLFLAG)THEN
          CALL HALLTRV(IROOM,CJETDIST,ZLOC,TU,RHOU,HALLVEL)
        ENDIF
      ELSE
        YFLOR = ZZYFLOR(IROOM2)
        PFLOR = EPA(IROOM2)
        YLAY = ZZHLAY(IROOM,LOWER)
        DENU = ERA(IROOM2)
        DENL = ERA(IROOM2)
        DO 20 IPROD = 1, NPROD
          IP = IZPMAP(IPROD+2) - 2
          CONL(IPROD) = ZZCSPEC(IROOM,LOWER,IP)
          CONU(IPROD) = ZZCSPEC(IROOM,UP,IP)
   20   CONTINUE
        TU = ETA(IROOM2)
        TL = ETA(IROOM2)
      endif
      RETURN
      END

      SUBROUTINE FLOGO1(DIRS12,YSLAB,XMSLAB,NSLAB,YLAY,QSLAB,PSLAB,
     +    MXPRD,NPROD,MXSLAB,UFLW2)
      include "precis.fi"
C*BEG
C***  FLOGO1  GENERIC - DEPOSITION OF MASS, ENTHALPY, OXYGEN, AND OTHER
C             PRODUCT-OF-COMBUSTION FLOWS PASSING BETWEEN TWO ROOMS
C             THROUGH A VERTICAL, CONSTANT-WIDTH VENT.  THIS VERSION
C             IMPLEMENTS THE CFAST RULES FOR FLOW DEPOSTION. (UPPER
C             LAYER TO UPPER LAYER AND LOWER LAYER TO LOWER LAYER)
C
C*** SUBROUTINE ARGUMENTS
C
C    INPUT
C    -----
C   DIRS12 - A MEASURE OF THE DIRECTION OF THE ROOM 1 TO ROOM
C                       2 FLOW IN EACH SLAB
C   YSLAB - SLAB HEIGHTS IN ROOMS 1,2 ABOVE DATUM ELEVATION [M]
C   XMSLAB - MASS FLOW RATE IN SLABS [KG/S]
C   NSLAB  - NUMBER OF SLABS BETWEEN BOTTOM AND TOP OF VENT
C   YLAY   - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C   QSLAB  - ENTHALPY FLOW RATE IN EACH SLAB [W]
C   PSLAB  - FLOW RATE OF PRODUCT IN EACH SLAB [(UNIT OF PRODUCT/S]
C   MXPRD  - MAXIMUM NUMBER OF PRODUCTS CURRENTLY AVAILABLE.
C   NPROD  - NUMBER OF PRODUCTS
C   MXSLAB - MAXIMUM NUMBER OF SLABS CURRENTLY AVAILABLE.
C
C   OUTPUT
C   ------
C   UFLW2(I,1,J), I=1 OR 2, J=1 OR 2 - MASS FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE TO ALL SLAB
C            FLOWS OF VENT [KG/S]
C   UFLW2(I,2,J), I=1 OR 2, J=1 OR 2 - ENTHALPY FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE TO ALL SLAB
C            FLOWS OF VENT [W]
C   UFLW2(I,3,J), I=1 OR 2, J=1 OR 2 - OXYGEN FLOW RATE TO UPPER (J=2)
C            OR LOWER (J=1) LAYER OF ROOM I DUE TO ALL SLAB FLOWS OF
C            VENT [(KG OXYGEN)/S]
C   UFLW2(I,3+K,J), I=1 OR 2, K=2 TO NPROD, J=1 OR 2 - PRODUCT K FLOW
C            RATE TO UPPER (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE
C            TO ALL SLAB FLOWS OF VENT [(UNIT PRODUCT K)/S]
C*END
      include "flwptrs.fi"
      INTEGER DIRS12(*)
      DIMENSION YSLAB(*), XMSLAB(*), QSLAB(*), YLAY(*), 
     +    PSLAB(MXSLAB,*)
      DIMENSION UFLW2(2,MXPRD+2,2), FF(2)
C
C*** INITIALIZE OUTPUTS
C
      DO 20 I = 1, 2
        DO 10 IPROD = 1, NPROD + 2
          UFLW2(I,IPROD,L) = 0.0D0
          UFLW2(I,IPROD,U) = 0.0D0
   10   CONTINUE
   20 CONTINUE
C
C*** PUT EACH SLAB FLOW INTO APPROPRIATE LAYER OF ROOM ITO
C    AND TAKE SLAB FLOW OUT OF APPROPRIATE LAYER OF ROOM IFROM
C
      DO 70 N = 1, NSLAB
C
C*** DETERMINE WHERE ROOM FLOW IS COMING FROM
C
        IF (DIRS12(N)==1) THEN
          IFROM = 1
          ITO = 2
        ELSE IF (DIRS12(N)==-1) THEN
          IFROM = 2
          ITO = 1
        ELSE
C
C*** NO FLOW IN THIS SLAB SO WE CAN SKIP IT
C
          GO TO 70
        endif
C
C*** LOWER TO LOWER OR UPPER TO UPPER
C
        IF (YSLAB(N)>=YLAY(IFROM)) THEN
          FU = 1.0D0
        ELSE
          FU = 0.0D0
        endif
        FL = 1.0D0 - FU
        FF(L) = FL
        FF(U) = FU
C
C*** PUT FLOW INTO DESTINATION ROOM
C
        XMTERM = XMSLAB(N)
        QTERM = QSLAB(N)
        DO 40 ILAY = 1, 2
          UFLW2(ITO,M,ILAY) = UFLW2(ITO,M,ILAY) + FF(ILAY) * XMTERM
          UFLW2(ITO,Q,ILAY) = UFLW2(ITO,Q,ILAY) + FF(ILAY) * QTERM
          DO 30 IPROD = 1, NPROD
            UFLW2(ITO,2+IPROD,ILAY) = UFLW2(ITO,2+IPROD,ILAY) + 
     +          FF(ILAY) * PSLAB(N,IPROD)
   30     CONTINUE
   40   CONTINUE
C
C*** TAKE IT OUT OF THE ORIGIN ROOM
C
        IF (YSLAB(N)>=YLAY(IFROM)) THEN
          UFLW2(IFROM,M,U) = UFLW2(IFROM,M,U) - XMTERM
          UFLW2(IFROM,Q,U) = UFLW2(IFROM,Q,U) - QTERM
          DO 50 IPROD = 1, NPROD
            UFLW2(IFROM,2+IPROD,U) = UFLW2(IFROM,2+IPROD,U) - 
     +          PSLAB(N,IPROD)
   50     CONTINUE
        ELSE
          UFLW2(IFROM,M,L) = UFLW2(IFROM,M,L) - XMTERM
          UFLW2(IFROM,Q,L) = UFLW2(IFROM,Q,L) - QTERM
          DO 60 IPROD = 1, NPROD
            UFLW2(IFROM,2+IPROD,L) = UFLW2(IFROM,2+IPROD,L) - 
     +          PSLAB(N,IPROD)
   60     CONTINUE
        endif
   70 CONTINUE
      RETURN
      END

      SUBROUTINE DELP(Y,NELEV,YFLOR,YLAY,DENL,DENU,PFLOR,EPSP,DP)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DELP
C
C     Source File: DELP.SOR
C
C     Functional Class:  
C
C     Description:  calculation of the absolute hydrostatic
C           pressures at a specified elevation in each of two adjacent
C           rooms and the pressure difference.  the basic calculation
C           involves a determination and differencing of hydrostatic
C           pressures above a specified datum pressure.
C
C     Arguments: 
C   INPUT
C   -----
C   Y     - VECTOR OF HEIGHTS ABOVE DATUM ELEVATION WHERE
C           PRESSURE DIFFERENCE IS TO BE CALCULATED [M]
C   NELEV - NUMBER OF HEIGHTS TO BE CALCULATED
C   YFLOR - HEIGHT OF FLOOR IN EACH ROOM ABOVE DATUM ELEVATION
C           [M]
C   YLAY  - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C   DENL  - LOWER LAYER DENSITY IN EACH ROOM [KG/M**3]
C   DENU  - UPPER LAYER DENSITY IN EACH ROOM [KG/M**3]
C   PFLOR - PRESSURE AT BASE OF EACH ROOM ABOVE DATUM PRESSURE
C            [KG/(M*S**2) = PASCAL]
C
C   OUTPUT
C   ------
C   DP    - CHANGE IN PRESSURE BETWEEN TWO ROOMS [KG/(M*S**2) = PASCAL]
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      DIMENSION PROOM(2), Y(*)
      DIMENSION DENL(*), DENU(*), YFLOR(*), YLAY(*), PFLOR(*), DP(*)
      DIMENSION GDENL(2), GDENU(2), YGDEN(2)
      PARAMETER (G = 9.80D0)
      PARAMETER (X1 = 1.0D0)

      DO 10 IROOM = 1, 2
        YGDEN(IROOM) = -(YLAY(IROOM)-YFLOR(IROOM)) * DENL(IROOM) * G
        GDENL(IROOM) = -DENL(IROOM) * G
        GDENU(IROOM) = -DENU(IROOM) * G
   10 CONTINUE

C
      DO 30 I = 1, NELEV
        DO 20 IROOM = 1, 2
          IF (YFLOR(IROOM)<=Y(I).AND.Y(I)<=YLAY(IROOM)) THEN
C
C*** THE HEIGHT, Y, IS IN THE LOWER LAYER
C
            PROOM(IROOM) = (Y(I)-YFLOR(IROOM)) * GDENL(IROOM)
C
          ELSE IF (Y(I)>YLAY(IROOM)) THEN
C
C*** THE HEIGHT, Y, IS IN THE UPPER LAYER
C
            PROOM(IROOM) = YGDEN(IROOM) + GDENU(IROOM) * (Y(I)-
     +          YLAY(IROOM))
          ELSE
            PROOM(IROOM) = 0.0D0
          endif
   20   CONTINUE
C
C*** CHANGE IN PRESSURE IS DIFFERENCE IN PRESSURES IN TWO ROOMS
C
        DP1 = PFLOR(1) + PROOM(1)
        DP2 = PFLOR(2) + PROOM(2)
C
C*** TEST OF DELP FUDGE
C
        EPSCUT = 10.0D0 * EPSP * MAX(X1,ABS(DP1),ABS(DP2))
C
C
        DPOLD = DP1 - DP2
C
C*** TEST FOR UNDERFLOW
C
        IF (ABS(DPOLD/EPSCUT)<=130.0D0) THEN
            ZZ = 1.D0 - EXP(-ABS(DPOLD/EPSCUT))
            DP(I) = ZZ * DPOLD
          ELSE
            DP(I) = DPOLD
          endif
   30   CONTINUE
        RETURN
        END

!	The following functions are to implement the open/close function for vents.
!	This is done with a really simple, linear interpolation
!	The arrays to hold the open/close information are qcvh (4,mxvents), qcvv(4,nr), qcvm(4,mfan),
!         and qcvi(4,mfan). 

!	h is for horizontal flow, v for vertical flow, m for mechanical ventilation and i for filtering at mechanical vents

!   The qcv{x} arrays are of the form
!		(1,...) Is start of time to change
!		(2,...) Is the initial fraction (set in HVENT, VVENT and MVENT)
!		(3,...) Is the time to complete the change, Time+Decay_time, and
!		(4,...) Is the final fraction

!	The open/close function is done in the physical/mode interface, HFLOW, VFLOW and HVFAN

	real*8 function qchfraction (points, index, time)

!	This is the open/close function for buoyancy driven horizontal flow

	real*8 points(4,*), time, dt, dy, dydt, mintime
	data mintime/1.0e-6/

	if (time<points(1,index)) then
		qchfraction = points(2,index)
	else if (time>points(3,index)) then
		qchfraction = points(4,index)
	else
	    dt = max(points(3,index) - points(1,index),mintime)
	    deltat = max(time - points(1,index),mintime)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qchfraction = points(2,index) + dydt * deltat
	endif
	return
	end function qchfraction

	real*8 function qcvfraction (points, index, time)

!	This is the open/close function for buoyancy driven vertical flow

	real*8 points(4,*), time, dt, dy, dydt, mintime
	data mintime/1.0e-6/

	if (time<points(1,index)) then
		qcvfraction = points(2,index)
	else if (time>points(3,index)) then
		qcvfraction = points(4,index)
	else
	    dt = max(points(3,index) - points(1,index),mintime)
	    deltat = max(time - points(1,index),mintime)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcvfraction = points(2,index) + dydt * deltat
	endif
	return
	end function qcvfraction

	real*8 function qcffraction (points, index, time)

!	This is the open/close function for mechanical ventilation

	real*8 points(4,*), time, dt, dy, dydt, mintime
	data mintime/1.0d-6/

	if (time<points(1,index)) then
		qcffraction = points(2,index)
	else if (time>points(3,index)) then
		qcffraction = points(4,index)
	else
	    dt = max(points(3,index) - points(1,index), mintime)
	    deltat = max(time - points(1,index), mintime)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcffraction = points(2,index) + dydt * deltat
	endif
	return
	end function qcffraction

	real*8 function qcifraction (points, index, time)

!	This is the open/close function for filtering

	real*8 points(4,*), time, dt, dy, dydt, mintime
	data mintime/1.0d-6/

	if (time<points(1,index)) then
		qcifraction = points(2,index)
	else if (time>points(3,index)) then
		qcifraction = points(4,index)
	else
	    dt = max(points(3,index) - points(1,index),mintime)
	    deltat = max(time - points(1,index), mintime)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcifraction = points(2,index) + dydt * deltat
	endif
	return
	end function qcifraction
	
	subroutine getventinfo(i,ifrom, ito, iface, vwidth, vbottom, vtop, 
     *    voffset, vred, vgreen, vblue)

!       This is a routine to get the shape data for horizontal flow vents

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "vents.fi"
      
      ifrom =IZVENT(i,1)
      ito = IZVENT(i,2)
      iface = izvent(i,6)
      vwidth = zzvent(i,3)
      voffset = zzvent(i,4)
      vbottom = zzvent(i,1)
      vtop = zzvent(i,2)
      vred = 1.0d0
      vgreen = 0.0d0
      vblue = 1.0d0

      RETURN
      END

      integer function rev_flowhorizontal
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_flowhorizontal = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_flowhorizontal
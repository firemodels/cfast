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
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "prods.fi"
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
	double precision factor2, qchfraction, height, width

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
      IF (OPTION(FHFLOW).NE.ON) RETURN

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

C     INITIALIZE CFAST DATA STRUCTURES FOR FLOW STORAGE
c	This code was moved from vent hflow 5/7/2003 so that initialization always occurs.

        DO 21 Ilocal = 1, 2
          VSS(Ilocal,I) = XX0
          VAA(Ilocal,I) = XX0
          VSA(Ilocal,I) = XX0
          VAS(Ilocal,I) = XX0
21      CONTINUE

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
        IF (AVENT.GE.1.D-10) THEN
          CALL VENT(YFLOR,YLAY,TU,TL,DENL,DENU,PFLOR,YVTOP,YVBOT,AVENT,
     +        CP,CONL,CONU,NPROD,MXPRD,MXSLAB,EPSP,CSLAB,PSLAB,QSLAB,
     +        VSS(1,I),VSA(1,I),VAS(1,I),VAA(1,I),DIRS12,DPV1M2,RSLAB,
     +        TSLAB,YSLAB,YVELEV,XMSLAB,NSLAB,NNEUT,VENTVEL)

C*** UPDATE HALL INFO FOR VENTS CONNECTED FROM FIRE ROOM TO HALL

          IF(UPDATEHALL)THEN
            VENTHEIGHT = YVTOP - YVBOT
            IF(IZVENT(I,4).EQ.1)THEN
              VLAYERDEPTH = YVTOP - YLAY(2)
              IF(VLAYERDEPTH.GT.VENTHEIGHT)VLAYERDEPTH = VENTHEIGHT
              CALL SETHALL(1,I,IROOM1,TSEC,WIDTH,
     .              TSLAB(NSLAB),-VENTVEL,VLAYERDEPTH)
            ENDIF
            IF(IZVENT(I,5).EQ.1)THEN
              VLAYERDEPTH = YVTOP - YLAY(1)
              IF(VLAYERDEPTH.GT.VENTHEIGHT)VLAYERDEPTH = VENTHEIGHT
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

          IF (OPTION(FENTRAIN).EQ.ON) THEN
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
          END IF

C         SUM FLOWS FROM BOTH ROOMS FOR EACH LAYER AND TYPE OF PRODUCT
C         (BUT ONLY IF THE ROOM IS AN INSIDE ROOM)

          IF (IROOM1.GE.1.AND.IROOM1.LE.NIRM) THEN
            DO 40 IPROD = 1, NPROD + 2
              UFLW(IROOM1,IPROD,LOWER) = UFLW(IROOM1,IPROD,LOWER) +
     +            UFLW2(1,IPROD,L)
              UFLW(IROOM1,IPROD,UPPER) = UFLW(IROOM1,IPROD,UPPER) +
     +            UFLW2(1,IPROD,U)
   40       CONTINUE
            IF (OPTION(FENTRAIN).EQ.ON) THEN
              DO 50 IPROD = 1, NPROD + 2
                UFLW(IROOM1,IPROD,LOWER) = UFLW(IROOM1,IPROD,LOWER) +
     +              UFLW3(1,IPROD,L)
                UFLW(IROOM1,IPROD,UPPER) = UFLW(IROOM1,IPROD,UPPER) +
     +              UFLW3(1,IPROD,U)
   50         CONTINUE
            END IF
          END IF
          IF (IROOM2.GE.1.AND.IROOM2.LE.NIRM) THEN
            DO 60 IPROD = 1, NPROD + 2
              UFLW(IROOM2,IPROD,LOWER) = UFLW(IROOM2,IPROD,LOWER) +
     +            UFLW2(2,IPROD,L)
              UFLW(IROOM2,IPROD,UPPER) = UFLW(IROOM2,IPROD,UPPER) +
     +            UFLW2(2,IPROD,U)
   60       CONTINUE
            IF (OPTION(FENTRAIN).EQ.ON) THEN
              DO 70 IPROD = 1, NPROD + 2
                UFLW(IROOM2,IPROD,LOWER) = UFLW(IROOM2,IPROD,LOWER) +
     +              UFLW3(2,IPROD,L)
                UFLW(IROOM2,IPROD,UPPER) = UFLW(IROOM2,IPROD,UPPER) +
     +              UFLW3(2,IPROD,U)
   70         CONTINUE
            END IF
          END IF
        END IF
   80 CONTINUE
      endif

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.EQ.0)THEN

C*** we need to save the solution for later jacobian calculations

          DO 140 IROOM = 1, NM1
            DO 150 IPROD = 1, NPROD + 2
              UFLW0(IROOM,IPROD,LOWER) = UFLW(IROOM,IPROD,LOWER)
              UFLW0(IROOM,IPROD,UPPER) = UFLW(IROOM,IPROD,UPPER)
  150       CONTINUE
  140     CONTINUE
         ELSEIF(JACCOL.GT.0)THEN

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

      subroutine ventflag(ventflg,roomflg,anyvents)

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "prods.fi"
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

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.GT.0)THEN

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
          IF(IEQTYP.EQ.EQP.OR.IEQTYP.EQ.EQTU.OR.IEQTYP.EQ.EQVU.OR.
     .       IEQTYP.EQ.EQTL.OR.IEQTYP.EQ.EQOXYL.OR.IEQTYP.EQ.EQOXYU)THEN

C*** determine all rooms connected to perturbed rooms

            DO 130 I = 1, NVENTS
              IROOM1 = IZVENT(I,1)
              IROOM2 = IZVENT(I,2)
              IF(IROOM.EQ.IROOM1.OR.IROOM.EQ.IROOM2)THEN
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


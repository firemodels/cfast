      SUBROUTINE CVHEAT(FLWCV,FLXCV)
C*RB
C
C     Routine:    CFCNVC
C     Function:   Interface between RESID and CONVEC.  Loops over rooms
C                 setting up varibles.  Passes to CONVEC if Ceiling jet for
C                 a surface is off, otherwise sets FLXCV to 0.0 and then
C                 solves for FLWCV
C     Outputs:    FLWCV       Net enthalphy into each layer 
C                 FLXCV       Net heat flux onto surface
C     Revision History:
C     PAR   11/??/91 Created.
C     GPF   2/5/93  added partial derivative calculations for use
C                   with the reduced Jacobian option
C     GPF   7/13/95
C               Reduced the number of convection calculations performed during
C               a Jacobian calculation (OPTION(FMODJAC)==2)
C     GPF 2/5/96   removed reduced jacobian option added on 2/5/93.
C*RE

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "wdervs.fi"
      include "opt.fi"
      include "wnodes.fi"

      DIMENSION FLWCV(NR,2), FLXCV(NR,NWAL)
      DIMENSION FLWCV0(NR,2), FLXCV0(NR,NWAL)
      INTEGER CJETOPT
      LOGICAL ROOMFLG(NR), WALLFLG(4*NR)
      SAVE FLWCV0, FLXCV0

      XX0 = 0.0D0
      DO 20 I = 1, NM1
        FLWCV(I,UPPER) = XX0
        FLWCV(I,LOWER) = XX0
        DO 10 J = 1, NWAL
          FLXCV(I,J) = XX0
   10   CONTINUE
   20 CONTINUE
      IF (OPTION(FCONVEC).NE.ON) RETURN

      CJETOPT = OPTION(FCJET)

      DO 100 I = 1, NM1
        ROOMFLG(I) = .TRUE.
  100 CONTINUE
      DO 110 I = 1, NWALLS
        WALLFLG(I) = .TRUE.
  110 CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.GT.0)THEN

C*** If 2nd modified jacobian is active and dassl is computing a jacobian then
C    only compute convection heat transfer in the room where the dassl 
C    solution variable has been perturbed

          DO 120 I = 1, NM1
            ROOMFLG(I) = .FALSE.
  120     CONTINUE
          DO 130 I = 1, NWALLS
            WALLFLG(I) = .FALSE.
  130     CONTINUE

          IEQTYP = IZEQMAP(JACCOL,1)
          IROOM = IZEQMAP(JACCOL,2)
          IF(IEQTYP.EQ.EQTU.OR.IEQTYP.EQ.EQVU.OR.IEQTYP.EQ.EQTL.OR.
     .       IEQTYP.EQ.EQWT)THEN
            IF(IEQTYP.EQ.EQWT)IROOM = IZWALL(IROOM,1)
            DO 140 IWALL = 1, 4
              ROOMFLG(IROOM) = .TRUE.
              IF(SWITCH(IWALL,IROOM))THEN
                IW = IZWMAP2(IWALL,IROOM) - NOFWT
                WALLFLG(IW) = .TRUE.
              ENDIF
  140       CONTINUE
          ENDIF
        ENDIF
      ENDIF

      DO 30 IW = 1, NWALLS
        IF(.NOT.WALLFLG(IW))GO TO 30
        I = IZWALL(IW,1)
        IWALL = IZWALL(IW,2)
        NRMFIRE = IFRPNT(I,1)
          IF(MOD(IWALL,2).EQ.1)THEN
            ILAY = UPPER
           ELSE
            ILAY = LOWER
          ENDIF

C*** CEILING JET HEAT TRANSFER IS NOT ACTIVE IF CJETOPT=2.  USE
C    NORMAL (CALL CONVEC) INSTEAD

            IF (CJETOPT.NE.2.AND.CJETON(IWALL).AND.NRMFIRE.NE.0) THEN
              FLXCV(I,IWALL) = XX0
             ELSE
              CALL CONVEC(IWALL,ZZTEMP(I,ILAY),ZZWTEMP(I,IWALL,1),
     .                    FLXCV(I,IWALL))
            END IF
            FLWCV(I,ILAY) = FLWCV(I,ILAY) - 
     .                            ZZWAREA(I,IWALL)*FLXCV(I,IWALL)

   30 CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN
         IF(JACCOL.EQ.0)THEN

C*** save the flux and flow vectors when we are about to compute a jacobian

           DO 150 IROOM = 1, NM1
             FLWCV0(IROOM,1) = FLWCV(IROOM,1)
             FLWCV0(IROOM,2) = FLWCV(IROOM,2)
             DO 160 IWALL = 1, 4
               FLXCV0(IROOM,IWALL) = FLXCV(IROOM,IWALL)
  160        CONTINUE
  150      CONTINUE
          ELSEIF(JACCOL.GT.0)THEN

C*** we are computing the JACCOL'th column of the jacobian.  If the solution
C    hasn't changed then get it from the vectors saved above.

           DO 170 IROOM = 1, NM1
             IF(ROOMFLG(IROOM))GO TO 170
             FLWCV(IROOM,1) = FLWCV0(IROOM,1)
             FLWCV(IROOM,2) = FLWCV0(IROOM,2)
             DO 180 IWALL = 1, 4
               FLXCV(IROOM,IWALL) = FLXCV0(IROOM,IWALL)
  180        CONTINUE
  170      CONTINUE
         ENDIF
      ENDIF      

      RETURN
      END

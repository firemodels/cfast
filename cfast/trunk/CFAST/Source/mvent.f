      SUBROUTINE MVENT (tsec, HVPSOLV, HVTSOLV, TPRIME,
     .                  FLWMV, DELTPMV, DELTTMV, PRPRIME, NPROD,IERROR,
     .                  HVACFLG, filtered)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MVENT
C
C     Functional Class:  Physical interface routine
C
C     Description:  Physical interface routine for the HVAC model
C
C     Arguments: HVPSOLV
C                HVTSOLV
C                TPRIME
C                FLWMV
C                DELTPMV
C                DELTTMV
C                PRPRIME
C                NPROD
C                IERROR   Returns error codes
C
C     Revision History:
C        June 14, 1992 Made modifications to allow HVAC node
C                  pressures and HVAC duct temperatures to be
C                  be solved by DASSL instead of by time splitting.
C        July 13. 1995 Reduced the number of calcuations performed when
C                   computing the Jacobian.
C        Modified: 6/14/1992 at 10:00 by GPF:
C                  Made modifications to allow HVAC node pressures and HVAC
C                  duct temperatures to be solved by DASSL instead of by time
C                  splitting.
C        Modified: 7/13/1995 at 10:03 by GPF:
C                  Reduced the number of calculations performed when computing
C                  the Jacobian
C        Modified: 9/5/1995 at 10:03 by PAR:
C                  Added support for IERROR and returning stops to main
C        Modified: 2/5/96 by GPF:
C                  changed FMODJAC comparisons from 2 to ON
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
!        04/19/2007 calculate a filtered amount to remove in RESID
C

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "flwptrs.fi"
      include "opt.fi"

      DIMENSION FLWMV(NR,NS+2,2)
      DIMENSION HVPSOLV(*), HVTSOLV(*), DELTPMV(*), DELTTMV(*)
      DIMENSION TPRIME(*), PRPRIME(*)
      DIMENSION FLWMV0(NR,NS+2,2),DELTPMV0(MNODE),DELTTMV0(MBR)
      LOGICAL FIRST, DOIT, HVACFLG
	double precision filter, qcifraction, tsec, xx0, xx1, filterm1,
     . filtered(nr,ns+2,2)
      SAVE FIRST,FLWMV0,DELTPMV0,DELTTMV0

      DATA FIRST/.TRUE./

C     Initialize convection coefficient for hvac ducts. DUCTCV is read in
C     from solver.ini file by INITSLV.  CHV should eventually be defined
C     elsewhere.

      HVACFLG = .FALSE.
      IF (.NOT.MVCALC.OR.OPTION(FMVENT).NE.ON.OR.
     .    (NHVPVAR.EQ.0.AND.NHVTVAR.EQ.0)) RETURN
      HVACFLG = .TRUE.
      XX0 = 0.0D0
	xx1 = 1.0d0      
      IF (FIRST) THEN
         FIRST = .FALSE.
         DO 1 I = 1, NBR
         CHV(I) = DUCTCV
    1    CONTINUE
         do i = 1, n
            do j = 1, ns+2
                flwmv0(i,j,upper) = xx0
                flwmv0(i,j,lower) = xx0
            end do
         end do
      ENDIF

      DO 15 I = 1, N
         DO 10 J = 1, NS+2
            FLWMV(I,J,UPPER) = XX0
            FLWMV(I,J,LOWER) = XX0
            filtered(i,j,upper) = xx0
            filtered(i,j,lower) = xx0
10       CONTINUE
   15 CONTINUE
      DO 30 I = 1, NHVPVAR
         DELTPMV(I) = HVPSOLV(I)
   30 CONTINUE
      DO 31 I = 1, NHVTVAR
         DELTTMV(I) = HVTSOLV(I)
   31 CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN

C*** determine where the hvac calculation should be done.  initially assume
C    that it should be.

        DOIT = .TRUE.
        IF(JACCOL.GT.0)THEN
          IEQTYP = IZEQMAP(JACCOL,1)
          IF(IEQTYP.EQ.EQPMV.OR.IEQTYP.EQ.EQTMV)THEN

C*** if we're computing a jacobian and a hvac pressure or temperature is being perturbed
C    then doit.

            DOIT = .TRUE.
           ELSEIF(IEQTYP.EQ.EQTT.OR.IEQTYP.EQ.EQWT)THEN

C*** if we're computing a jacobian and a wall or target temperature is being perturbed
C    then don't doit

            DOIT = .FALSE.
           ELSE

C***  if we're computing a jacobian and anything else is being perturbed then doit.
C     if there are no hvac connections in the room where the variable is being perturbed
C     then we shouldn't have to do the hvac computation but that isn't working now.

            IROOM = IZEQMAP(JACCOL,2)
            IF(.NOT.IZHVAC(IROOM))DOIT = .FALSE.
            DOIT = .TRUE.
          ENDIF
        ENDIF

C*** If we're not going to do the hvac computation then get the answers from the
C    previously saved vectors

        IF(.NOT.DOIT)THEN
          DO 150 I = 1, NHVPVAR
            DELTPMV(I) = DELTPMV0(I)
  150     CONTINUE
          DO 160 I = 1, NHVTVAR
            DELTTMV(I) = DELTTMV0(I)
  160     CONTINUE
          DO 170 I = 1, N
            FLWMV(I,M,UPPER) = FLWMV0(I,M,UPPER)
            FLWMV(I,M,LOWER) = FLWMV0(I,M,LOWER)
            FLWMV(I,Q,UPPER) = FLWMV0(I,Q,UPPER)
            FLWMV(I,Q,LOWER) = FLWMV0(I,Q,LOWER)
            DO 180 J = 1, NS
              IF(ACTIVS(J))THEN
                FLWMV(I,J+2,UPPER) = FLWMV0(I,J+2,UPPER)
                FLWMV(I,J+2,LOWER) = FLWMV0(I,J+2,LOWER)
              ENDIF
  180       CONTINUE
  170     CONTINUE
          RETURN
        ENDIF
      ENDIF

      CALL HVFREX (tsec, HVPSOLV,HVTSOLV)
      CALL HVMFLO (tsec, DELTPMV,IERROR)
      IF (IERROR.NE.0) RETURN
      CALL HVSFLO (TPRIME,DELTTMV)
      CALL HVTOEX (tsec, PRPRIME,NPROD)
      DO 20 II = 1, NEXT
            I = HVNODE(1,II)
            J = HVNODE(2,II)
            ISYS = IZHVSYS(J)
            IF(I.LT.1.OR.I.GT.NM1) GO TO 20
            FLWMV(I,M,UPPER) = FLWMV(I,M,UPPER) + HVEFLO(UPPER,II)
            FLWMV(I,M,LOWER) = FLWMV(I,M,LOWER) + HVEFLO(LOWER,II)
            FLWMV(I,Q,UPPER) = FLWMV(I,Q,UPPER) +
     .                         CP*HVEXTT(II,UPPER)*HVEFLO(UPPER,II)
            FLWMV(I,Q,LOWER) = FLWMV(I,Q,LOWER) +
     .                         CP*HVEXTT(II,LOWER)*HVEFLO(LOWER,II)
            DO 40 K = 1, NS
               IF (ACTIVS(K)) THEN
                  FLWMV(I,2+K,LOWER) = FLWMV(I,2+K,LOWER) +
     .                     HVEXCN(II,K,LOWER)*HVEFLO(LOWER,II)
                  FLWMV(I,2+K,UPPER) = FLWMV(I,2+K,UPPER) +
     .                     HVEXCN(II,K,UPPER)*HVEFLO(UPPER,II)
               ENDIF
   40       CONTINUE
!	Filter 9 and 11, (2+k)) = 11 and 13, smoke and radiological fraction
!     Note that filtering is always negative. Same as agglomeration and settling
	      filter = qcifraction(qcvf,isys,tsec)
            filtered(i,13,upper) = max(xx0,filter*flwmv(i,13,upper))
            filtered(i,13,lower) = max(xx0,filter*flwmv(i,13,lower))
            filtered(i,11,upper) = max(xx0,filter*flwmv(i,11,upper))
            filtered(i,11,lower) = max(xx0,filter*flwmv(i,11,lower))
20    CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.EQ.0)THEN

C*** save information for a later Jacobian calculation

          DO 50 I = 1, NHVPVAR
            DELTPMV0(I) = DELTPMV(I)
   50     CONTINUE
          DO 60 I = 1, NHVTVAR
            DELTTMV0(I) = DELTTMV(I)
   60     CONTINUE
          DO 70 I = 1, N
            FLWMV0(I,M,UPPER) = FLWMV(I,M,UPPER)
            FLWMV0(I,M,LOWER) = FLWMV(I,M,LOWER)
            FLWMV0(I,Q,UPPER) = FLWMV(I,Q,UPPER)
            FLWMV0(I,Q,LOWER) = FLWMV(I,Q,LOWER)
            DO 80 J = 1, NS
              IF(ACTIVS(J))THEN
                FLWMV0(I,J+2,UPPER) = FLWMV(I,J+2,UPPER)
                FLWMV0(I,J+2,LOWER) = FLWMV(I,J+2,LOWER)
              ENDIF
   80       CONTINUE
   70     CONTINUE
        ENDIF
      ENDIF

      RETURN
      END

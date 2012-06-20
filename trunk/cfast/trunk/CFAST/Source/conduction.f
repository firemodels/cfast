      SUBROUTINE CNHEAT(UPDATE,DT,FLXTOT,DELTA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CNHEAT
C
C     Source File: CNHEAT.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Interface between RESID and the conduction calculation.
C                   For each active wall surface in each routine this
C                   routine calculates the residual function 
C                   q'' + K dT/dx, which when zero is simply Fourier's
C                   law of heat conduction.
C
C     Arguments: UPDATE  We don't keep solution unless UPDATE is 1 or 2
C                        If UPDATE is 2 then we don't calculate DELTA or
C                        use FLXTOT
C                DT Time step interval from last valid solution point
C                FLXTOT  Total flux striking walls
C                DELTA   The residual of q'' + K DT/dx
C
C     Revision History:
C        Created:  by GPF
C        Modified: 2/5/93 BY GPF
C                  added partial derivative calculations for the
C                  reduced Jacobian option
C        Modified: 2/25/94 BY GPF
C                  added external temperature to CNDUCT call for
C                  the room to room heat transfer option
C        Modified by GPF 10/10/94
C               Added fourth boundary condition option, flux on both
C               sides of wall.  This is needed when we do a full 
C               numerical conduction calculation of targets.  Currently,
C               we assume that targets are thin surfaces, ie we calculate
C               a temperature that results in a net flux of zero
C               striking the target.
C        Modified by GPF 4/26/95
C               Removed ETA from CNDUCT's argument list
C        Modified by GPF 7/13/95
C               Reduced the number of conduction calculations performed during
C               a Jacobian calculation (OPTION(FMODJAC)==2)
C        Modified by GPF 2/5/96   removed reduced jacobian option added on 2/5/93.
C        Modified by GPF 2/5/99   added wall to wall heat transfer
C
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      use cparams
      use dsize
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "wdervs.fi"
      include "opt.fi"

      DIMENSION TGRAD(2), DELTA(*)
      DIMENSION FLXTOT(NR,NWAL), IREVWC(NWAL)
      DIMENSION VTGRAD0(4*NR), VTGRAD(4*NR)
      INTEGER UPDATE
      SAVE VTGRAD0
      DATA IREVWC/2,1,3,4/

C
      SIGMA = 5.67D-8

C*** SOLVE CONDUCTION PROBLEM FOR ALL WALLS

      IBEG = 1
      IEND = NWALLS

C*** If the reduced jacobian option is on and dassl is computing a jacobian
C    then solve a conduction problem only if dassl is varying a wall temperature

      IF(OPTION(FMODJAC)==ON.AND.JACCOL>0)THEN
        IF(IZEQMAP(JACCOL,1)==EQWT)THEN

C  a wall temperatuer is being perturbed so solve that walls conduction problem

          IBEG = JACCOL - NOFWT
          IEND = IBEG
         ELSE

C   some other variable is being perturbed, so don't solve any 
C   conduction problems

          IBEG = 1
          IEND = 0
        ENDIF
      ENDIF

      DO 10 IW = IBEG, IEND
            IROOM = IZWALL(IW,1)
            IWALL = IZWALL(IW,2)
            ICOND = NOFWT + IW

C*** use exterior wall temperature from last time step to ...

            TWINT = ZZWTEMP(IROOM,IWALL,1)
            TWEXT = ZZWTEMP(IROOM,IWALL,2)
            TGAS = ETA(IROOM)
            IWEQ = IZWMAP2(IWALL,IROOM) - NOFWT
            IWB = IZWALL(IWEQ,5)

C*** compute flux seen by exterior of wall

            IF (IWB==3) THEN

c*** back wall is connected to the outside

              CALL CONVEC(IREVWC(IWALL),TGAS,TWEXT,WFLUXOUT)
              WFLUXOUT = WFLUXOUT + SIGMA * (TGAS**4-TWEXT**4)
              WFLUXSAVE = WFLUXOUT
              IF(IZHEAT(IROOM)/=0.AND.IWALL/=1.AND.IWALL/=2)THEN

c*** back wall is connected to rooms defined by izhtfrac with fractions
c         defined by zzhtfrac.  If izheat(iroom) is not zero then
c         nwroom better not be zero!  nwroom should always be zero
c         for iwall=3 and iwall=4

                WFLUXOUT = 0.0D0
                NWROOM = IZHTFRAC(IROOM,0)
                DO 50 JJ = 1, NWROOM
                  J = IZHTFRAC(IROOM,JJ)
                  FRAC = ZZHTFRAC(IROOM,J)
                  IF(IWALL==3)THEN
                    YB = ZZHLAY(IROOM,LOWER)
                    YT = ZZYCEIL(IROOM)
                   ELSEIF(IWALL==4)THEN
                    YB = 0.0D0
                    YT = ZZHLAY(IROOM,LOWER)
                  ENDIF
                  DFLOR = ZZYFLOR(J) - ZZYFLOR(IROOM)
                  YY = ZZHLAY(J,LOWER) + DFLOR
                  IF(J/=NM1+1)THEN
                    IF(YY>YT)THEN
                      FU = 0.0D0
                     ELSEIF(YY<YB)THEN
                      FU = 1.0D0
                     ELSE
                      IF(YB/=YT)THEN
                        FU = (YT-YY)/(YT-YB)
                       ELSE
                        FU = 0.0D0
                      ENDIF
                    ENDIF
                    FLUXU = FU*FLXTOT(J,3)
                    FLUXL = (1.0D0-FU)*FLXTOT(J,4)
                   ELSE
                    FLUXU = WFLUXSAVE
                    FLUXL = 0.0D0
                  ENDIF
                  WFLUXOUT = WFLUXOUT + FRAC*(FLUXU + FLUXL)
   50           CONTINUE
              ENDIF
            endif
            CALL CNDUCT(UPDATE,TWINT,TWEXT,DT,
     +          FKW(1,IWALL,IROOM),CW(1,IWALL,IROOM),RW(1,IWALL,IROOM),
     +          TWJ(1,IROOM,IWALL),WALLDX(1,IROOM,IWALL),
     +          NUMNODE(1,IWALL,IROOM),NSLB(IWALL,IROOM),
     +          WFLUXIN,WFLUXOUT,IWB,TGRAD,TDERV)

C*** store wall gradient

            VTGRAD(IW) = TGRAD(2)

C*** COMPUTE PARTIAL OF WALL TEMPERATURE EQUATION WITH
C    RESPECT TO THE WALL TEMPERATURE.  WE ASSUME THAT
C    THE PARTIALS OF CONVECTIVE HEAT FLUX AND RADIATIVE HEAT
C    FLUX WITH RESPECT TO WALL TEMPERATURE HAVE ALREADY
C    BEEN COMPUTED.  (IF THEY WERE NOT THEN WE WOULDN'T KNOW
C    HEAT FLUX STRIKING THE WALL!  

   10 CONTINUE

C*** save wall gradients during base call to resid (cnduct)

      IF(OPTION(FMODJAC)==ON)THEN

c*** store wall gradient for later use

        IF(JACCOL==0)THEN
          DO 20 IW = 1, NWALLS
            VTGRAD0(IW) = VTGRAD(IW)
   20     CONTINUE
         ELSEIF(JACCOL>0)THEN

c*** use saved wall temperature gradient except for conduction problem
c    corresponding to the perturbed wall temperature

          IF(IZEQMAP(JACCOL,1)==EQWT)THEN
            IEQ = JACCOL - NOFWT
           ELSE
            IEQ = 0
          ENDIF
          DO 30 IW = 1, NWALLS
            IF(IW/=IEQ)VTGRAD(IW) = VTGRAD0(IW)
   30     CONTINUE
        ENDIF
      ENDIF                 

C*** DASSL WILL TRY TO FORCE DELTA TO BE ZERO, SO THAT FOURIER'S
C    LAW, Q = -K DT/DX, IS SATISFIED AT THE WALL SURFACE 

      IF(UPDATE/=2)THEN
        DO 40 IW = 1, NWALLS
          ICOND = NOFWT + IW
          IROOM = IZWALL(IW,1)
          IWALL = IZWALL(IW,2)
          DELTA(ICOND) = FLXTOT(IROOM,IWALL) + VTGRAD(IW) * 
     +    FKW(1,IWALL,IROOM)
   40   CONTINUE
      ENDIF

      RETURN
      END

      SUBROUTINE CNDUCT(UPDATE,TEMPIN,TEMPOUT,DT,WK,WSPEC,WRHO,WTEMP,
     +    WALLDX,NUMNODE,NSLAB,
     +    WFLUXIN,WFLUXOUT,IWBOUND,TGRAD,TDERV)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CNDUCT
C
C     Source File: CNDUCT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: UPDATE   We don't keep solution unless UPDATE is 1 OR 2
C                TEMPIN   Temperature at interior wall
C                TEMPOUT  Temperature at exterior wall, not used now
C                DT       Time step interval from last valid solution point
C                WK       Wall thermal conductivity
C                WSPEC    Wall specific heat
C                WRHO     Wall density
C                WTEMP    Wall temperature profile
C                WALLDX   Wall position points
C                NUMNODE  Number of nodes in each slab
C                NSLAB    Number of slabs
C                WFLUXIN  Flux striking interior wall
C                WFLUXOUT Flux striking exterior wall
C                IWBOUND  Type of boundary condition for exterior wall
C                         (1=constant temperature, 2=insulated,
C                          3=flux based on ambient temperature on outside wall
C                          4=flux on both interior and exterior walls)
C                TGRAD    Temperature gradient
C                TDERV    Partial of Temperature gradient with
C                          respect to wall surface temperature.  This
C                          number is used to calculate wall Jacobian elements.
C
C     Revision History:
C        Created:  gpf
C        Modified by GPF 2/5/93
C               Added partial derivative calculations for use with
C               the reduced Jacobian option.  
C        Modified by GPF 2/25/94
C               Activated the TEMPOUT parameter for the room to room 
C               heat transfer option
C        Modified by GPF 10/10/94
C               Added fourth boundary condition option, flux on both
C               sides of wall.  This is needed when we do a full 
C               numerical conduction calculation of targets.  Currently,
C               we assume that targets are thin surfaces, ie we calculate
C               a temperature that results in a net flux of zero
C               striking the target.
C        Modified by GPF 4/26/95
C               Removed TAMB from argument list
C        Modified by GPF 6/28/95
C               increased local array size to 2*30=60 points.
C                
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
C*** DECLARE LOCAL VARIABLES
C
      PARAMETER (NN = 60)
      DIMENSION A(NN), B(NN), C(NN), TNEW(NN), TDERIV(NN), DDIF(3), 
     . TGRAD(2), WK(*), WSPEC(*), WRHO(*), WTEMP(*), WALLDX(*), 
     . NUMNODE(*)
      INTEGER UPDATE

      NX = NUMNODE(1)

C*** construct right hand side (rhs) of tri-diagonal system for interior 
C    nodes.  rhs at boundary and slab break points are defined farther down.

        
      DO 10 I = 2, NX - 1
        TNEW(I) = WTEMP(I)
   10 CONTINUE

C*** set up tri-diagonal coefficient matrix

C*** setup first row
      IF(IWBOUND/=4)THEN
         A(1) = 1.0D0
         B(1) = 0.0D0
         C(1) = 0.0D0
         TNEW(1) = TEMPIN
        ELSE
         A(1) = 1.0D0
         B(1) = 0.0D0
         C(1) = -1.0D0
         TNEW(1) = WALLDX(1) * WFLUXIN / WK(1)
      ENDIF

C*** do interior points for each slab

      IEND = 0
      DO 30 ISLAB = 1, NSLAB
        NINTX = NUMNODE(1+ISLAB)
        XKRHOC = WK(ISLAB) / (WSPEC(ISLAB)*WRHO(ISLAB))
        S = 2.0D0 * DT * XKRHOC
        IBEG = IEND + 2
        IEND = IBEG + NINTX - 1
        DO 20 I = IBEG, IEND
          HI = WALLDX(I)
          HIM1 = WALLDX(I-1)
          A(I) = 1.0D0 + S / (HI*HIM1)
          B(I) = -S / (HIM1*(HI+HIM1))
          C(I) = -S / (HI*(HI+HIM1))
   20   CONTINUE
   30 CONTINUE

C*** do break points between each slab

      IBREAK = 1
      DO 40 ISLAB = 2, NSLAB
        NINTX = NUMNODE(ISLAB)
        IBREAK = IBREAK + NINTX + 1
        B(IBREAK) = WSPEC(ISLAB-1) * WRHO(ISLAB-1) / WALLDX(IBREAK-1)
        C(IBREAK) = WSPEC(ISLAB) * WRHO(ISLAB) / WALLDX(IBREAK)
        A(IBREAK) = -(B(IBREAK)+C(IBREAK))
        TNEW(IBREAK) = 0.0D0
   40 CONTINUE

C*** setup last row, note: last row depends on form of boundary condition

      IF (IWBOUND==1) THEN

C*** constant temperature boundary condition
C    (if we ever solve for both interior and exterior wall temperatures
C     then use change TNEW(NX) = tamb to TNEW(NX) = tempout)

        A(NX) = 1.0D0
        B(NX) = 0.0D0
        C(NX) = 0.0D0
        TNEW(NX) = TEMPOUT
      ELSE IF (IWBOUND==2) THEN

C*** insulated boundary condition

        A(NX) = 1.0D0
        B(NX) = -1.0D0
        C(NX) = 0.0D0
        TNEW(NX) = 0.0D0
      ELSE IF (IWBOUND==3.OR.IWBOUND==4) THEN

C*** flux boundary condition (using lagged temperatures

        A(NX) = 1.0D0
        B(NX) = -1.0D0
        C(NX) = 0.0D0
        TNEW(NX) = WALLDX(NX-1) * WFLUXOUT / WK(NSLAB)
      endif
C     
C*** NOW PERFORM AN L-U FACTORIZATION OF THIS MATRIX (see atkinson p.455)
C    NOTE: MATRIX IS DIAGONALLY DOMINANT SO WE DON'T HAVE TO PIVOT

C*** note we do the following in case a(1) is not 1

      C(1) = C(1) / A(1)
      DO 50 I = 2, NX - 1
        A(I) = A(I) - B(I) * C(I-1)
        C(I) = C(I) / A(I)
   50 CONTINUE
      A(NX) = A(NX) - B(NX) * C(NX-1)
      DO 61 I = 1, NX
         TDERIV(I) = 0.0D0
   61 CONTINUE
      TDERIV(1) = 1.0D0

C*** NOW CONSTRUCT GUESS AT NEW TEMPERATURE PROFILE

C*** FORWARD SUBSTITION

      TNEW(1) = TNEW(1) / A(1)
      TDERIV(1) = TDERIV(1)/A(1)
      DO 60 I = 2, NX
        TNEW(I) = (TNEW(I)-B(I)*TNEW(I-1)) / A(I)
        TDERIV(I) = (TDERIV(I)-B(I)*TDERIV(I-1)) / A(I)
   60 CONTINUE

C*** BACKWARD SUBSTITION

      DO 70 I = NX - 1, 1, -1
        TNEW(I) = TNEW(I) - C(I) * TNEW(I+1)
        TDERIV(I) = TDERIV(I) - C(I) * TDERIV(I+1)
   70 CONTINUE

C***  WE DON'T KEEP solution UNLESS UPDATE IS 1 OR 2

      IF (UPDATE/=0) THEN
        DO 80 I = 1, NX
          WTEMP(I) = TNEW(I)
   80   CONTINUE
      endif

C*** estimate temperature gradient at wall surface by constructing a 
C    quadratic polynomial that interpolates first three data points in 
C    the temperature profile.  we will use divided differences.

C*** first divided difference

      DDIF(1) = (TNEW(2)-TNEW(1)) / WALLDX(1)
      DDIF(2) = (TNEW(3)-TNEW(2)) / WALLDX(2)

C*** second divided difference

      DDIF(2) = (DDIF(2)-DDIF(1)) / (WALLDX(1)+WALLDX(2))

      TGRAD(1) = (DDIF(1)-DDIF(2)*WALLDX(1))
      TGRAD(2) = (TNEW(2)-TNEW(1)) / WALLDX(1)
      TDERV = TDERIV(2)
      RETURN
      END
      integer function rev_conduction
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_conduction = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_conduction
      
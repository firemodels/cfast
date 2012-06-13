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

      use cparams
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
	real*8 filter, qcifraction, tsec, xx0, xx1, filterm1,
     . filtered(nr,ns+2,2)
      SAVE FIRST,FLWMV0,DELTPMV0,DELTTMV0

      DATA FIRST/.TRUE./

C     Initialize convection coefficient for hvac ducts. DUCTCV is read in
C     from solver.ini file by INITSLV.  CHV should eventually be defined
C     elsewhere.

      HVACFLG = .FALSE.
      IF (.NOT.MVCALC.OR.OPTION(FMVENT)/=ON.OR.
     .    (NHVPVAR==0.AND.NHVTVAR==0)) RETURN
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

      IF(OPTION(FMODJAC)==ON)THEN

C*** determine where the hvac calculation should be done.  initially assume
C    that it should be.

        DOIT = .TRUE.
        IF(JACCOL>0)THEN
          IEQTYP = IZEQMAP(JACCOL,1)
          IF(IEQTYP==EQPMV.OR.IEQTYP==EQTMV)THEN

C*** if we're computing a jacobian and a hvac pressure or temperature is being perturbed
C    then doit.

            DOIT = .TRUE.
           ELSEIF(IEQTYP==EQTT.OR.IEQTYP==EQWT)THEN

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
      IF (IERROR/=0) RETURN
      CALL HVSFLO (TPRIME,DELTTMV)
      CALL HVTOEX (tsec, PRPRIME,NPROD)
      DO 20 II = 1, NEXT
            I = HVNODE(1,II)
            J = HVNODE(2,II)
            ISYS = IZHVSYS(J)
            IF(I<1.OR.I>NM1) GO TO 20
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

      IF(OPTION(FMODJAC)==ON)THEN
        IF(JACCOL==0)THEN

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

      SUBROUTINE HVMFLO (tsec, DELTPMV,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVMFLO
C
C     Source File: HVMFLO.SOR
C
C     Functional Class:  
C
C     Description:  Routine for mass flow solution & calculation
C
C     Arguments: DELTPMV
C                IERROR   Returns error codes
C
C     Revision History:
C     May 15, 1991 set the convergence criterion to 0.01 rather than 0.00001
C                  also set dpz to zero.  The rational is that the static
C                  pressure differences should not matter in the flow.
C     June 14, 1992 essentially re-written. Formerly this routine used
C                   a newton like procedure to compute mass flow in each
C                   portion of the hvac network.  Now this routine computes
C                   mass conservation error.  The newton iteration is performed
C                   by dassl to reduce the the above error to nearly zero
C        Modified: 9/5/1995 at 9:53 by PAR:
C                  Added support of IERROR and returning stops to main
C        Modified: 2/5/96 by GPF:
C                  work around fix to HVAC density initialization using
C                  ambient pressure rather than average duct pressure
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"

      DIMENSION DELTPMV(*)
	real*8 tsec

C     CALCULATE AVERAGE TEMPERATURES AND DENSITIES FOR EACH BRANCH
C 
      XX1 = 1.0D0
      XX2 = 2.0D0
      XX0 = 0.0D0

      XX1O2 = XX1 / XX2
      DO 9 IB = 1, NBR
C         PAV = (HVP(NA(IB)) + HVP(NE(IB)))*XX1O2 + POFSET
C     fix 1/30/96 to solve pressure initialization in the beginning
         PAV = POFSET
         ROHB(IB) = PAV/(HVRGAS*TBR(IB))
         BFLO(IB) = XX1
    9 CONTINUE

C     START THE ITERATION CYCLE

      NITER = 2
      DO 1 ITER = 1, NITER

C     INITIALIZE CONDUCTANCE

      DO 11 IB = 1, NBR
         CE(IB)=XX0
   11 CONTINUE

!     CONVERT FROM PRESSURE TO MASS FLOW RATE COEFFICIENTS

      DO 40 IB = 1, NBR
         IF (CE(IB)/=XX0) THEN
            XTEMP = XX1 / SQRT(ABS(CE(IB)))
            CE (IB) = SIGN(XTEMP, CE(IB))
         ENDIF
   40 CONTINUE

C     CALCULATE HYDROSTATIC PRESSURE DIFFERENCE TERMS

      DO 70 I = 1, NNODE
      DO 70 J = 1, NCNODE(I)
         DPZ(I,J) = ROHB(ICMV(I,J))*HVGRAV*(HVGHT(MVINTNODE(I,J)) 
     +   - HVGHT(I))
   70 CONTINUE

C     Find mass flow for each branch and mass residual at each node

      DO 30 I = 1, NNODE
         F = 0.0D0
            DO 25 J = 1, NCNODE(I)
               DP = HVP(MVINTNODE(I,J)) - HVP(I) + DPZ(I,J)
               IF (NF(ICMV(I,J))==0) THEN

C        RESISTIVE BRANCH CONNECTION 

                  HVFLOW(I,J) = SIGN(CE(ICMV(I,J))*SQRT(ABS(DP)), DP)
                  BFLO(ICMV(I,J)) = ABS(HVFLOW(I,J))
                 ELSE

C        FAN BRANCH CONNECTION

                  K = NF(ICMV(I,J))
                  IF(NE(ICMV(I,J)) /= I)THEN
C*** FLOW IS AT FAN INLET
                     HVFLOW(I,J) = -HVFAN(tsec,I,J,K,DP)
                    ELSE
C*** FLOW IS AT FAN EXIT
                     DP = -DP
                     HVFLOW(I,J) = HVFAN(tsec,I,J,K,DP)
                  ENDIF
               ENDIF
               F = F + HVFLOW(I,J)
               II = IZHVIE(MVINTNODE(I,J))
               IF(II/=0)HVFLOW(MVINTNODE(I,J),1) = -HVFLOW(I,J)
   25       CONTINUE
         ii = IZHVMAPE(I)
         IF(II>0) DELTPMV(ii) = F
   30 CONTINUE
    1 CONTINUE

      RETURN
      END

      SUBROUTINE HVSFLO (TPRIME, DELTTMV)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVSFLO
C
C     Source File: HVSFLO.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: TPRIME
C                DELTTMV
C
C     Revision History:
C
C     June 14, 1992 This routine now only computes the 
C                   differential equation for temperature.  this
C                   equation is now solved by dassl.  The de's
C                   for smoke (species) are now performed in hvtoex.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "opt.fi"

      DIMENSION TPRIME(*), DELTTMV(*)

      XX0 = 0.0D0
      DO 41 IB = 1, NBR
         DELTTMV(IB) = ROHB(IB)*HVDVOL(IB)*TPRIME(IB)/GAMMA
   41 CONTINUE

      DO 30 I = 1, NNODE

C     CALCULATE TEMPERATURES & SMOKE FLOWS IN FOLLOWING LOOP AT THE
C     CONNECTING NODES

         HVTA = XX0
         FLOWIN = XX0
         DO 20 J = 1, NCNODE(I)
            IF (HVFLOW(I,J)>XX0) THEN
               FLOWIN = FLOWIN + HVFLOW(I,J)
               IB = ICMV(I,J)
               HVTEMP = HVFLOW(I,J)
               HVTA = HVTA + HVTEMP*TBR(IB)
            ENDIF
   20    CONTINUE
         IF (FLOWIN>XX0) THEN
            HVTA = HVTA / FLOWIN
         ELSE
            DO 21 II = 1, NEXT
               IF (HVNODE(2,II)==I) THEN
                  HVTA = HVEXTT(II,UPPER)
                  GO TO 22
               ENDIF
   21       CONTINUE

C           THIS IS A BAD SITUATION.  WE HAVE NO FLOW, YET MUST CALCULATE
C           THE INFLOW CONCENTRATIONS.  IF THERE IS FLOW, THEN THIS
C           SECTION WILL BE SKIPPED.

            HVTA = TBR(1)
   22       CONTINUE
         ENDIF
C
C     NOW CALCULATE THE RESULTING TEMPERATURE AND CONCENTRATIONS
C     IN THE DUCTS AND FANS
C
         DO 40 J = 1, NCNODE(I)
         IF (HVFLOW(I,J)<XX0) THEN
            IB = ICMV (I,J)
            DELTTMV(IB) = DELTTMV(IB) - (HVTA-TBR(IB))*ABS(HVFLOW(I,J))
            IF(OPTION(FHVLOSS)==ON)THEN
               DELTTMV(IB) = DELTTMV(IB) + 
     .                       CHV(IB)*(TBR(IB)-TAMB(1))*HVDARA(IB)
            ENDIF
         ENDIF
         II = IZHVIE(MVINTNODE(I,J))
         IF(II/=0.AND.HVFLOW(I,J)>XX0) THEN
            IB = ICMV(I,J)
            HVTA = HVEXTT(II,UPPER)
            DELTTMV(IB) = DELTTMV(IB) - (HVTA-TBR(IB))*HVFLOW(I,J)
            IF(OPTION(FHVLOSS)==ON)THEN
               DELTTMV(IB) = DELTTMV(IB) + 
     .                       CHV(IB)*(TBR(IB)-TAMB(1))*HVDARA(IB)
            ENDIF
         ENDIF
   40    CONTINUE
   30 CONTINUE
      RETURN
      END

      real*8 FUNCTION HVFAN(tsec,II,JJ,K,DP)

C     Description:  This function returns the mass flow rate through a fan
C           This function has been modified to prevent negative flow.  
C           A MAX function was inserted just after the calculation off, the 
C           flow to do this.  If the flow is allowed to be negative 
C           (flow reversal) then this statement must be removed.
!           Also, there is now a flow restriction on the fan, using qcmfraction
C
C     Arguments: II     node number
C                JJ     jj'th connection to node II
C                K      fan number
C                DP     head pressure across the fan
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      LOGICAL firstc
	real*8 hvfanl, openfraction, qcffraction, tsec,
     . minimumopen
      DATA firstc/.true./
      SAVE firstc
      PARAMETER (XX0 = 0.0D0)

      ROH = ROHB(ICMV(II,JJ))

	if (firstc) then
		minimumopen = sqrt(d1mach(1))
		firstc = .false.
	endif

! The hyperbolic tangent allows for smooth transition from full flow to no flow
! within the fan cuttoff pressure range
	F = 0.5 - tanh(8.0/(hmax(k)-hmin(k))*(dp-hmin(k))-4.0)/2.0
	F = MAX(minimumopen, F)
	HVFANl = F * qmax(k) * ROH
      openfraction = max (minimumopen, qcffraction (qcvm, k, tsec))
	hvfan = hvfanl * openfraction
	RETURN

      END

      SUBROUTINE HVFREX (tsec, HVPSOLV, HVTSOLV)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVFREX
C
C     Source File: HVFREX.SOR
C
C     Functional Class:  
C
C     Description:  UPDATE ARRAYS AND ASSIGN COMPARTMENT PRESSURES, TEMPERATURES AND
C                   CONCENTRATIONS TO EXTERIOR NODES
C
C     Arguments: HVPSOLV
C                HVTSOLV
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 05/15/1991 at 10:15 by WWJ:
C                  Add a test for the outside.  This is done differently
C                  with cfast than was done for fast
C        Modified: 06/14/1992 at 10:16 by GPF:
C                  Compare effective temperature using harmonic
C                  rather than arithmetic averages.  define branch
C                  temperatures and node pressures using values
C                  supplied by the dae solver dassl
C        Modified: 11/30/1992 at 10:32 by PAR:
C                  Changed the calculation of HVCVEN to reflect the 
C                  concentration of the layer the flow is actually coming out of
C        Modified: 11/30/1992 at 10:33 by PAR:
C                  Changed the calculation of the HVFRAC to make sure the vent 
C                  area is in the compartment
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      DIMENSION HVPSOLV(*), HVTSOLV(*)
	real*8 tsec, xx1, xx0

!
!    INITIALIZE THE PARAMETERS USED BY THE MECHANICAL VENTILATION ROUTINES

      XX0 = 0.0D0
      XX1 = 1.0D0
      XXTENTH = 0.1D0
      XX0P5 = 0.5D0

      DO 10 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        Z = ZZHLAY(I,LOWER)
        IF (HVORIEN(II)==1) THEN
C     WE HAVE AN OPENING WHICH IS ORIENTED VERTICALLY - USE A SMOOTH CROSSOVER
C     FIRST, CALCULATE THE SCALING LENGTH OF THE DUCT
           XXL = SQRT(AREXT(II))
        ELSE
           XXL = SQRT(AREXT(II)) * XXTENTH
        ENDIF
C     THEN THE BOTTOM OF THE VENT (ABOVE THE FLOOR)
        XXLL = MAX(XX0,MIN((HVELXT(II) - XX0P5 * XXL),(HR(I)-XXL)))
C     THESE ARE THE RELATIVE FRACTION OF THE UPPER AND LOWER 
C     LAYER THAT THE DUCT "SEES" THESE PARAMETERS GO FROM 0 TO 1
        FRACTION = MAX(XX0,MIN(XX1,MAX(XX0,(Z-XXLL)/XXL)))
        HVFRAC(UPPER,II) = MIN(XX1,MAX(XX1-FRACTION,XX0))
        HVFRAC(LOWER,II) = MIN(XX1,MAX(FRACTION,XX0))
   10 CONTINUE

C     THIS IS THE ACTUAL DUCT INITIALIZATION

      DO 30 II = 1, NEXT
        I = HVNODE(1,II)
        J = HVNODE(2,II)
        IF (I<N) THEN
          Z = ZZHLAY(I,LOWER)
          ZL = MIN(Z,HVELXT(II))
          ZU = MIN(XX0,HVELXT(II)-ZL)
          RU = ZZRHO(I,UPPER)
          RL = ZZRHO(I,LOWER)
          HVP(J) = ZZRELP(I) - (RU*ZU+RL*ZL) * HVGRAV
          HVEXTT(II,UPPER) = ZZTEMP(I,UPPER)
          HVEXTT(II,LOWER) = ZZTEMP(I,LOWER)
        ELSE
          HVEXTT(II,UPPER) = EXTA
          HVEXTT(II,LOWER) = EXTA
          HVP(J) =  EXPA - EXRA * HVGRAV * HVELXT(II)
        endif
        DO 20 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            IF (I<N) THEN
               HVEXCN(II,LSP,UPPER) = ZZCSPEC(I,UPPER,LSP)
               HVEXCN(II,LSP,LOWER) = ZZCSPEC(I,LOWER,LSP)
            ELSE
               XXRHO = O2N2(LSP) * EXRA
               HVEXCN(II,LSP,UPPER) = XXRHO
               HVEXCN(II,LSP,LOWER) = XXRHO
            endif
          endif
   20   CONTINUE
   30 CONTINUE
      DO 40 I = 1, NHVPVAR
         II = IZHVMAPI(I)
         HVP(II) = HVPSOLV(I)
   40 CONTINUE 
      DO 41 IB = 1, NHVTVAR
         TBR(IB) = HVTSOLV(IB)
   41 CONTINUE
      RETURN
      END

      SUBROUTINE HVTOEX(tsec,PRPRIME,NPROD)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HVTOEX
C
C     Source File: HVTOEX.SOR
C
C     Functional Class:  
C
C     Description:  ASSIGN RESULTS OF HVAC SIMULATION TO THE TRANSFER
C                   VARIABLES (HVEXTT, HVEXCN)
C
C     Arguments: PRPRIME
C                NPROD
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:24 by GPF:
C                  Added computation of species de's.  these
C                  equations are time split just like the gas layer species 
C                  equations.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C
      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      DIMENSION PRPRIME(*)
	real*8 tsec, xx1, xx0

C    SUM PRODUCT FLOWS ENTERING SYSTEM

      XX0 = 0.0D0
	xx1 = 1.0d0
      NHVPR = NLSPCT*NHVSYS
      IF(NPROD/=0)THEN
         DO 3 I = 1, NHVPR
            PRPRIME(I) = xx0
    3    CONTINUE
      ENDIF
      IF(NS>0)THEN
         DO 1 ISYS = 1, NHVSYS
            HVMFSYS(ISYS) = XX0
            DO 2 K = 1, NS
               DHVPRSYS(ISYS,K) = XX0
    2       CONTINUE
    1    CONTINUE
      ENDIF

C     FLOW INTO THE ISYS SYSTEM

      DO 20 II = 1, NEXT
        J = HVNODE(2,II)
        IB = ICMV(J,1)
        HVEFLO(UPPER,II) = HVFLOW(J,1) * HVFRAC(UPPER,II)
        HVEFLO(LOWER,II) = HVFLOW(J,1) * HVFRAC(LOWER,II)
        ISYS = IZHVSYS(J)
        IF (HVFLOW(J,1)<XX0) THEN
          HVMFSYS(ISYS) = HVMFSYS(ISYS) + HVFLOW(J,1)
          IF(NPROD/=0)THEN
             DO 10 K = 1, ns
               IF (ACTIVS(K)) DHVPRSYS(ISYS,K) = DHVPRSYS(ISYS,K) + 
     .                      ABS(HVEFLO(UPPER,II))*HVEXCN(II,K,UPPER) +
     .                      ABS(HVEFLO(LOWER,II))*HVEXCN(II,K,LOWER)
   10        CONTINUE
          ENDIF
        endif
   20 CONTINUE

C     FLOW OUT OF THE ISYS SYSTEM

      IF(NPROD/=0)THEN
         DO 24 K = 1, MIN(NS,9)
            IF(ACTIVS(K))THEN
               DO 25 ISYS = 1, NHVSYS
                  IF (ZZHVM(ISYS)/=XX0)THEN
                     DHVPRSYS(ISYS,K) = DHVPRSYS(ISYS,K) -
     .                   ABS(HVMFSYS(ISYS))*ZZHVPR(ISYS,K)/ZZHVM(ISYS)
                  ENDIF
   25          CONTINUE
            ENDIF
   24    CONTINUE
!	Do a special case for the non-reacting gas(es)
		  k = 11
            IF(ACTIVS(K))THEN
               DO 255 ISYS = 1, NHVSYS
                  IF (ZZHVM(ISYS)/=XX0)THEN
                     DHVPRSYS(ISYS,k) = DHVPRSYS(ISYS,k) -
     .                   ABS(HVMFSYS(ISYS))*ZZHVPR(ISYS,k)/ZZHVM(ISYS)
                  ENDIF
  255          CONTINUE
            ENDIF
 
C     PACK THE SPECIES CHANGE FOR DASSL (ACTUALLY RESID)

         ISOF = 0
         DO 26 K = 1, MIN(NS,9)
            IF(ACTIVS(K))THEN
               DO 27 ISYS = 1, NHVSYS
                  ISOF = ISOF + 1
                  IF (ZZHVM(ISYS)/=XX0)THEN
                     PRPRIME(ISOF) = DHVPRSYS(ISYS,K)
                   ELSE
	               PRPRIME(ISOF) = XX0
                   ENDIF
   27          CONTINUE
            ENDIF
   26    CONTINUE
!	Do a special case for the non-reacting gas(es)
            k = 11
            IF(ACTIVS(K))THEN
               DO 277 ISYS = 1, NHVSYS
                  ISOF = ISOF + 1
                  IF (ZZHVM(ISYS)/=XX0)THEN
                     PRPRIME(ISOF) = DHVPRSYS(ISYS,k)
                   ELSE
	               PRPRIME(ISOF) = XX0
                   ENDIF
  277          CONTINUE
            ENDIF
      ENDIF           

C    DEFINE FLOWS OR TEMPERATURE LEAVING SYSTEM

      DO 30 II = 1, NEXT
         J = HVNODE(2,II)
         ISYS = IZHVSYS(J)
C        WE ALLOW ONLY ONE CONNECTION FROM A NODE TO AN EXTERNAL DUCT
         IB = ICMV(J,1)
         IF (HVFLOW(J,1)>XX0) THEN
            HVEXTT(II,UPPER) = TBR(IB)
            HVEXTT(II,LOWER) = TBR(IB)
            DO 40 K = 1, NS
               IF (ACTIVS(K))THEN
! Case 1 - finite volume and finite mass in the ISYS mechanical ventilation system
                 IF (ZZHVM(ISYS)/=XX0) THEN
                    HVEXCN(II,K,UPPER) = ZZHVPR(ISYS,K)/ZZHVM(ISYS)
                    HVEXCN(II,K,LOWER) = HVEXCN(II,K,UPPER)
! Case 2 - zero volume (no duct). Flow through the system is mdot(product)/mdot(total mass) - see keywordcases to change this
                 ELSEIF(HVMFSYS(ISYS)/=XX0) THEN
                    HVEXCN(II,K,UPPER) = 
     .                          -(DHVPRSYS(ISYS,K)/HVMFSYS(ISYS))
                    HVEXCN(II,K,LOWER) = HVEXCN(II,K,UPPER)
                 ELSE
                    HVEXCN(II,K,UPPER) = XX0
                    HVEXCN(II,K,LOWER) = XX0
                 ENDIF
               ENDIF
   40       CONTINUE
         endif
   30 CONTINUE
      RETURN
      END
      integer function rev_flowfan
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_flowfan = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_flowfan
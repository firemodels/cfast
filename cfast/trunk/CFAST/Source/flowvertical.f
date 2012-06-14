
        
      SUBROUTINE VFLOW (TSEC,FLWVF,VFLOWFLG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     VFLOW
C
C     Source File: VFLOW.SOR
C
C     Functional Class:
C
C     Description:  Interface between CFAST and the vertical vent physical
C                   routines.
C
C     Arguments: FLWVF     (O) change in mass and energy for each layer of each
C                              compartment
C
C     Revision History:
C        Created:  2/8/1992 at 14:43 by PAR
C        Modified: 5/7/1992 at 14:45 by WWJ:
C                  add selection rules for the "from" compartment Y is the
C                  mass fraction
C        Modified: 11/8/1992 at 14:47 by PAR:
C                  fixed incorrect assignments and changed flow rules for "to"
C                  layer
C        Modified: 2/5/1996 by gpf:
C                  fixed initializations to FL and FU
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
C
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use cfast_main
      use params 
      use vents
      include "precis.fi"
      include "cenviro.fi"
      include "flwptrs.fi"
      include "vntslb.fi"
      include "opt.fi"
C
      DIMENSION FLWVF(NR,NS+2,2)
      DIMENSION XMVENT(2), ILAY(2), TMVENT(2)
      INTEGER TOPRM, BOTRM
      LOGICAL VFLOWFLG
	real*8 area, tsec
      DATA TOPRM /1/, BOTRM /2/

C     THE SELECTION RULES ARE NOW IMPLEMENTED HERE.  THE CROSOVER
C     IS THE RELATIVE FRACTION OF THE VOLUME CLOESEST TO THE HOLE
C     FROM WHICH THE MASS WILL COME

      VFLOWFLG = .FALSE.
      IF (OPTION(FVFLOW)/=ON) RETURN
      IF (NVVENT==0) RETURN
      VFLOWFLG = .TRUE.
      CROSOVER = 0.5D0
      OCO = 1.0D0 / CROSOVER
      EPSCUT = 0.0001D0
      XX0 = 0.0D0
      XX1 = 1.0D0
      DO 20 I = 1, N
         DO 10 J = 1, NS + 2
            FLWVF(I,J,UPPER) = XX0
            FLWVF(I,J,LOWER) = XX0
   10    CONTINUE
         DO 20 J = 1, N
            VMFLO(I,J,UPPER) = XX0
            VMFLO(I,J,LOWER) = XX0
   20 CONTINUE
C
      DO 60 I = 1, NVVENT
         ITOP = IVVENT(I,TOPRM)
         IBOT = IVVENT(I,BOTRM)
	   area = qcvfraction(qcvv, i, tsec) * vvarea(itop,ibot)
         CALL VENTCF (ITOP, IBOT, area, VSHAPE(ITOP,IBOT),
     +      EPSCUT, XMVENT, TMVENT, ILAY)
         DO 30 IFLOW = 1, 2

C     DETERMINE ROOM WHERE FLOW COMES AND GOES

            IF (IFLOW==1) THEN
               IFRM = IBOT
               ITO = ITOP
            ELSE
               IFRM = ITOP
               ITO = IBOT
            endif
C
C     DETERMINE MASS AND ENTHALPY FRACTIONS - FIRST "FROM," THEN "TO"
C
            IF (IFRM<=NM1) THEN
               IF (IFRM==IBOT) THEN
                    VOLUP = VOLFRU(IFRM) * OCO
                    VOLUP = MIN(VOLUP, XX1)
                    VOLLOW = MAX(XX1 - VOLUP, XX0)
               ELSE
                    VOLLOW = VOLFRL(IFRM) * OCO
                    VOLLOW = MIN(VOLLOW,XX1)
                    VOLUP = MAX(XX1 - VOLLOW, XX0)
               endif
               XXMU = VOLUP  * XMVENT(IFLOW)
               XXML = VOLLOW * XMVENT(IFLOW)
               XXQU = CP * XXMU * ZZTEMP(IFRM,UPPER)
               XXQL = CP * XXML * ZZTEMP(IFRM,LOWER)
               XXTMP = VOLUP*ZZTEMP(IFRM,UPPER)
     .                        + VOLLOW*ZZTEMP(IFRM,LOWER)
               XXTQ = XXQU + XXQL
            ELSE
               XXMU = XX0
               XXML = XMVENT(IFLOW)
               XXQU = XX0
               XXQL = CP * XXML * ETA(IFRM)
               XXTMP = ZZTEMP(IFRM,LOWER)
               XXTQ = XXQU + XXQL
            ENDIF

            FL = XX0
            IF (XXTMP<=ZZTEMP(ITO,LOWER)) FL = XX1
            FU = XX1 - FL
            FUMU = FU * XMVENT(IFLOW)
            FUML = FL * XMVENT(IFLOW)
            FUQU = FU * XXTQ
            FUQL = FL * XXTQ

C
C     DEPOSIT MASS AND ENTHALPY INTO "TO" ROOM VARIBLES (NOT OUTSIDE)
C
          IF (ITO<=NM1) THEN
             FLWVF(ITO,M,UPPER) = FLWVF(ITO,M,UPPER) + FUMU
             FLWVF(ITO,M,LOWER) = FLWVF(ITO,M,LOWER) + FUML
             FLWVF(ITO,Q,UPPER) = FLWVF(ITO,Q,UPPER) + FUQU
             FLWVF(ITO,Q,LOWER) = FLWVF(ITO,Q,LOWER) + FUQL
          endif
          VMFLO(IFRM,ITO,UPPER) = FUMU + VMFLO(IFRM,ITO,UPPER)
          VMFLO(IFRM,ITO,LOWER) = FUML + VMFLO(IFRM,ITO,LOWER)
C
C     EXTRACT MASS AND ENTHALPY FROM "FROM" ROOM (NOT FROM OUTSIDE)
C
          IF (IFRM<=NM1) THEN
             FLWVF(IFRM,M,UPPER) = FLWVF(IFRM,M,UPPER) - XXMU
             FLWVF(IFRM,M,LOWER) = FLWVF(IFRM,M,LOWER) - XXML
             FLWVF(IFRM,Q,UPPER) = FLWVF(IFRM,Q,UPPER) - XXQU
             FLWVF(IFRM,Q,LOWER) = FLWVF(IFRM,Q,LOWER) - XXQL
          endif
          VMFLO(ITO,IFRM,UPPER) = VMFLO(ITO,IFRM,UPPER) - XXMU
          VMFLO(ITO,IFRM,LOWER) = VMFLO(ITO,IFRM,LOWER) - XXML
C     SPECIES TRANSFER FOR VERTICAL VENTS
          DO 50 LSP = 1, NS
             IF (ACTIVS(LSP)) THEN
                INDEX = PP+LSP-1
                XXMIXL = ZZCSPEC(IFRM,LOWER,LSP) * XXML
                XXMIXU = ZZCSPEC(IFRM,UPPER,LSP) * XXMU
C
C     DEPOSIT MASS AND ENTHALPHY INTO "TO" ROOM VARIABLES (NOT OUTSIDE)
C
                IF (ITO<=NM1) THEN
                   PMTOUP = (XXMIXU + XXMIXL) * FU
                   PMTOLP = (XXMIXU + XXMIXL) * FL
                   FLWVF(ITO,INDEX,UPPER) = FLWVF(ITO,INDEX,UPPER) +
     .                                      PMTOUP
                   FLWVF(ITO,INDEX,LOWER) = FLWVF(ITO,INDEX,LOWER) +
     .                                      PMTOLP
                endif
C
C     EXTRACT MASS AND ENTHALPY FROM "FROM" ROOM (NOT FROM THE OUTSIDE)
C
                IF (IFRM<=NM1) THEN
                   FLWVF(IFRM,INDEX,UPPER) = FLWVF(IFRM,INDEX,UPPER) -
     .                                       XXMIXU
                   FLWVF(IFRM,INDEX,LOWER) = FLWVF(IFRM,INDEX,LOWER) -
     .                                       XXMIXL
                endif
             endif
   50     CONTINUE
   30    CONTINUE
   60 CONTINUE
C
      RETURN
      END

      SUBROUTINE VENTCF(ITOP,IBOT,AVENT,NSHAPE,EPSP,XMVENT,TMVENT,ILAY)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     VENTCF
C
C     Source File: VENTCF.SOR
C
C     Functional Class:  
C
C     Description:  This routine calculates the flow of mass, enthalpy, 
C          and products of combustion through a horizontal vent joining 
C          an upper space 1 to a lower space 2. the subroutine uses input
C          data describing the two-layer environment of inside rooms and 
C          the uniform environment in outside spaces.
C
C     Arguments:
C     INPUT
C     -----
C     ITOP        - TOP ROOM NUMBER (physically with respect to the second compartment)
C     IBOT        - BOTTOM ROOM NUMBER
C     AVENT       - AREA OF THE VENT [M**2]                
C     NSHAPE      - NUMBER CHARACTERIZING VENT SHAPE: 1 = CIRCLE, 2 = SQUARE
C     EPSP        - ERROR TOLERANCE FOR DPREF [DIMENSIONLESS]
C
C     OUTPUT
C     ------
C     XMVENT(I)   I = 1, MASS FLOW FROM ROOM IBOT TO ROOM ITOP
C                 I = 2, MASS FLOW FROM ROOM ITOP TO ROOM IBOT
C
C     TMVENT(I)   I = 1, TEMPERATURE IN LAYER NEXT TO VENT IN TOP ROOM
C                 I = 2, TEMPERATURE IN LAYER NEXT TO VENT IN BOTTOM ROOM
C
C     ILAY(I)     I = 1, LAYER INDEX NEXT TO VENT IN TOP ROOM
C                 I = 2, LAYER INDEX NEXT TO VENT IN BOTTOM ROOM
C
C
C     Revision History:
C     May 15, 1991 the pressure was calculated incorrectly change pref->pofset
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use cfast_main
      include "precis.fi"
      include "cenviro.fi"
C
C
C     Procedure arguments:
C
      DIMENSION XMVENT(2), TMVENT(2), ILAY(2), PABS(2), DEN(2), 
     +    RELP(2)
      DIMENSION DENVNT(2), DP(2), VST(2), VVENT(2), IROOM(2)
C
      LOGICAL FIRSTC
      INTEGER L, U, Q, M
      PARAMETER (L = 2,U = 1,Q = 2,M = 1)
C
      DATA FIRSTC /.TRUE./
      SAVE FIRSTC, GAMCUT, GAMMAX, PI, XXTWO, XXONE, XXZERO
C
C     Initialization code - executed the first time ventcf is called.
C
      IF (FIRSTC) THEN
        XXZERO = 0.0D0
        XXONE = 1.0D0
        XXTWO = 2.0D0
        FIRSTC = .FALSE.
        GAMCUT = (XXTWO/(GAMMA+XXONE)) ** (GAMMA/(GAMMA-XXONE))
        ZZZ = GAMMA * ((XXTWO/(GAMMA+XXONE))**((GAMMA+XXONE)/(GAMMA-
     +      XXONE)))
        GAMMAX = SQRT(ZZZ)
        PI = 4.0D0 * ATAN(XXONE)
      endif
C
C     calculate the PABS(I), DELP, the other properties
C     adjacent to the two sides of the vent, and DELDEN.
C
      DP(1) = XXZERO
      DP(2) = XXZERO
      IF (IBOT<=NM1) THEN
        DP(2) = -G * (ZZRHO(IBOT,L)*ZZHLAY(IBOT,L)+ZZRHO(IBOT,U)*
     +      ZZHLAY(IBOT,U))
        RELP(2) = ZZRELP(IBOT)
      ELSE
        RELP(2) = EPA(ITOP)
      endif
C
      IF (ITOP<=NM1) THEN
        RELP(1) = ZZRELP(ITOP)
      ELSE
        DP(1) = -G * HRP(IBOT) * ERA(IBOT)
        RELP(1) = EPA(IBOT)
      endif
      PABS(1) = RELP(1) + DP(1) + POFSET
      PABS(2) = RELP(2) + DP(2) + POFSET
C
C     DELP is pressure immediately below the vent less pressure
C     immediately above the vent.
C
      DELP = RELP(2) + DP(2) - (RELP(1)+DP(1))
C
C     ILAY(1) contains layer index in top room that is adjacent to vent
C     ILAY(2) contains layer index in bottom room that is adjacent to vent
C
      IF (ZZVOL(ITOP,L)<=XXTWO*ZZVMIN(ITOP)) THEN
        ILAY(1) = U
      ELSE
        ILAY(1) = L
      endif
      IF (ZZVOL(IBOT,U)<=XXTWO*ZZVMIN(IBOT)) THEN
        ILAY(2) = L
      ELSE
        ILAY(2) = U
      endif
C
C     DELDEN is density immediately above the vent less density
C     immediately below the vent
C
      IF (ITOP<=NM1) THEN
        DEN(1) = ZZRHO(ITOP,ILAY(1))
      ELSE
        DEN(1) = ERA(IBOT)
      endif
      IF (IBOT<=NM1) THEN
        DEN(2) = ZZRHO(IBOT,ILAY(2))
      ELSE
        DEN(2) = ERA(ITOP)
      endif
      DELDEN = DEN(1) - DEN(2)
C
C     calculate VST(I), the "standard" volume rate of flow
C     through the vent into space I
C
      IF (DELP>=XXZERO) THEN
        RHO = DEN(2)
        EPS = DELP / PABS(2)
      ELSE
        RHO = DEN(1)
        EPS = -DELP / PABS(1)
      endif
      X = XXONE - EPS
      COEF = 0.68D0 + 0.17D0 * EPS
      EPSCUT = EPSP * MAX (XXONE, RELP(1), RELP(2))
      EPSCUT = SQRT(EPSCUT)
      SRDELP = SQRT(ABS(DELP))
      FNOISE = XXONE
      IF ((SRDELP/EPSCUT)<=130.D0) FNOISE = XXONE - 
     +    EXP(-SRDELP/EPSCUT)
      IF (EPS<=0.1D-5) THEN
        W = XXONE - 0.75D0 * EPS / GAMMA
      ELSE
        IF (EPS<GAMCUT) THEN
          GG = X ** (XXONE/GAMMA)
          FF = SQRT((XXTWO*GAMMA/(GAMMA-XXONE))*GG*GG*(XXONE-X/GG))
        ELSE
          FF = GAMMAX
        endif
        W = FF / SQRT(EPS+EPS)
      endif
      RHO2 = 2.0D0/RHO
      V = FNOISE * COEF * W * SQRT(RHO2) * AVENT * SRDELP
C
C     calculate VST for DELP > 0, DELP < 0 and DELP = 0
C
      IF (DELP>XXZERO) THEN
        VST(1) = V
        VST(2) = XXZERO
      ELSE IF (DELP<XXZERO) THEN
        VST(1) = XXZERO
        VST(2) = V
      ELSE
        VST(1) = XXZERO
        VST(2) = XXZERO
      endif
C
C     calculate VEX, the exchange volume rate of flow through the vent
C
      IF (DELDEN>XXZERO.AND.AVENT/=XXZERO) THEN
C
C     unstable configuration, calculate nonzero VEX
C
        IF (NSHAPE==1) THEN
          CSHAPE = 0.754D0
          D = XXTWO * SQRT(AVENT/PI)
        ELSE
          CSHAPE = 0.942D0
          D = SQRT(AVENT)
        endif
        DELPFD = CSHAPE ** 2 * G * DELDEN * D ** 5 / (XXTWO*AVENT**2)
        DPDDPF = ABS(DELP/DELPFD)
        VEXMAX = 0.1D0 * SQRT(XXTWO*G*DELDEN*SQRT(AVENT**5)/(DEN(1)+
     +      DEN(2)))
        VEX = MAX(VEXMAX*(XXONE-DPDDPF),XXZERO)
      ELSE
C
C     Stable configuration, set VEX = 0
C
        VEX = XXZERO
      endif
C
C     calculate VVENT(I), the volume rate of flow through the vent into space i
C
      DO 10 I = 1, 2
        VVENT(I) = VST(I) + VEX
   10 CONTINUE
C
C     calculate the vent flow properties
C
      DENVNT(1) = DEN(2)
      DENVNT(2) = DEN(1)
C
C     calculate the vent mass flow rates
C
      IROOM(1) = IBOT
      IROOM(2) = ITOP
      DO 20 I = 1, 2
        XMVENT(I) = DENVNT(I) * VVENT(I)
        IF (IROOM(I)<=NM1) THEN
C
C        IROOM(I) is an inside room so use the environment variable
C        ZZTEMP for temperature 
C
          TMVENT(I) = ZZTEMP(IROOM(I),ILAY(3-I))
        ELSE
C
C        IROOM(I) is an outside room so use ETA(IROOM(3-I) for temperature
C
          TMVENT(I) = ETA(IROOM(3-I))
        endif
   20 CONTINUE
      RETURN
      END

        subroutine getvventinfo(iinvvent,itop,ibot,harea,hshape,hface)

!       This is a routine to get the shape data for vertical flow (horizontal) vents

      use cfast_main 
      use vents
      include "precis.fi"

	integer itop, ibot, hshape, hface, iinvvent
	real*8 harea

	itop = ivvent(iinvvent,1)
	ibot = ivvent(iinvvent,2)
	harea = vvarea(itop,ibot)
	hshape = vshape(itop,ibot)
	if (itop>nm1) then
		hface = 6
	else
		hface = 5
	endif

      RETURN
      END

      integer function rev_flowvertical
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_flowvertical = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_flowvertical
      SUBROUTINE INITMM
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITMM
C
C     Source File: INITMM.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine initializes the main memory - must be used by 
C                   all modules that will run the model kernel
C
C     Arguments: none
C
C     Revision History:
C        Created:  06/20/1990 at 9:57 by WWJ
C        Modified: 05/15/1991 at 10:27 by WWJ:
C                  initialize vertical flow data
C        Modified: 11/30/1992 at 10:56 by RDP:
C                  Set MINMAS to 0.0 (with change in TOXIC to eliminate its
C                  need)
C        Modified: 3/12/1993 at 9:36 by RDP:
C                  Moved object initialization to new routine INITOB
C        gpf 10/14/93 initialized variables involved in the 
C                     detection/suppression option
C        gpf 2/25/94 initialized variables involved in the
C                    room to room heat transfer option
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/1994 by gpf:
C                  initialize ntarg, a target counter
C        Modified: 4/26/95
C                  give default values for target data structures, corrected
C                  value of sigma
C        Modified: 7/13/95
C                  defined JACCOL, NEQOFF for use in the new Jacobian speedup
C                  option.
C        Modified: 9/12/96
C                  initialized IXDTECT(I,DQUENCH) to 0 rather than 1 .  That is,
C                  sprinklers will now not go off by default.  
C        Modified: 7/22/96
C                  Initialized variables for use by hall option
C        Modified: 11/25/96
C                  Initialized variables used by non-rectangular room option
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C        Modified: 7/18/97 by par
C                  Initialized GUISELCT for gui graphics output
C        Modified: 10/21/97 by gpf
C                  Changed default fire size to 0 to be consistent with FAST
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "cfin.fi"
      include "params.fi"
      include "thermp.fi"
      include "fltarget.fi"
      include "sizes.fi"
      include "vents.fi"

C     SET SOME INITIALIZATION - SIMPLE CONTROL STUFF

      EXSET = .FALSE.
	debugging = .false.
      XX0 = 0.0D0
	xx1 = 1.0d0
	xm1 = -1.0d0

C     INITIALIZE THE COMMON BLOCK

      DO 10 I = 1, NS
        O2N2(I) = XX0
        ALLOWED(I) = .FALSE.
        ACTIVS(I) = .TRUE.
   10 CONTINUE
      DO 30 I = 1, NR
        DO 20 J = 1, NWAL
          CNAME(J,I) = 'OFF'
          SWITCH(J,I) = .FALSE.
   20   CONTINUE
   30 CONTINUE
      DO 40 I = 1, NR
        SWITCH(1,I) = .TRUE.
        CNAME(1,I) = 'DEFAULT'
   40 CONTINUE
      NCONFG = 0
      NDUMPR = 0
      NLSPCT = 0
      NRESTR = 0
      NUMTHRM = 0
      MAPLTW(1) = 1
      MAPLTW(2) = 2
      MAPLTW(3) = 1
      MAPLTW(4) = 2
      HCLDEP = 0
      SMKAGL = 0
      N = 0
      DO 50 I = 1, NWAL + 1
        CJETON(I) = .FALSE.
   50 CONTINUE

C     INITIALIZE THE FLOW VARIABLES

      DO 80 I = 1, NR
        IZSHAFT(I) = 0
        HEATUP(I) = XX0
        HEATLP(I) = XX0
        HEATVF(I) = XX0
        DO 70 J = 1, NR
C     DO VERTICAL VENTS (VVENT,...)
          VSHAPE(I,J) = 0
          NWV(I,J) = 0
          VVAREA(I,J) = XX0
C     DO HORIZONTAL VENTS (HVENT,...)
          NW(I,J) = 0
          NEUTRAL(I,J) = 0
   70   CONTINUE
   80 CONTINUE

      DO 60 IVENT = 1, MXVENTS
        SS1(IVENT) = XX0
        SS2(IVENT) = XX0
        SA1(IVENT) = XX0
        SA2(IVENT) = XX0
        AS1(IVENT) = XX0
        AS2(IVENT) = XX0
        AA1(IVENT) = XX0
        AA2(IVENT) = XX0
        SAU1(IVENT) = XX0
        SAU2(IVENT) = XX0
        ASL1(IVENT) = XX0
        ASL2(IVENT) = XX0
   60 CONTINUE
   
      do i = 1, mext
        hveflot(upper,i) = xx0
        hveflot(lower,i) = xx0
        tracet(upper,i) = xx0
        tracet(lower,i) = xx0
      end do

C     INITIALIZE THE FORCING FUNCTIONS

      DO 100 I = 1, NR
        EMP(I) = XX0
        EMS(I) = XX0
        EME(I) = XX0
        APS(I) = XX0
        DO 90 K = UPPER, LOWER
          QR(K,I) = XX0
          QC(K,I) = XX0
          QFC(K,I) = XX0
   90   CONTINUE
  100 CONTINUE
      DO 110 I = 1, MXFIRE
        QFR(I) = XX0
  110 CONTINUE
      do i = 1, maxteq
        p(i) = xx0
      end do

C     DEFINE THE OUTSIDE WORLD AS INFINITY

      XLRG = 1.D+5
      DO 150 I = 1, NR
        DR(I) = XLRG
        BR(I) = XLRG
        HR(I) = XLRG
        HRP(I) = XLRG
        HRL(I) = XX0
        HFLR(I) = XX0
	  CXABS(I) = XX0
	  CYABS(I) = XX0
        AR(I) = BR(I) * DR(I)
        VR(I) = HR(I) * AR(I)
        DO 120 J = 1, NWAL
          EPW(J,I) = XX0
          QSRADW(J,I) = XX0
          QSCNV(J,I) = XX0
  120   CONTINUE
        DO 140 J = 1, NR
          NW(I,J) = 0
  140   CONTINUE
  150 CONTINUE
      DO 130 IVENT = 1, MXVENTS
        BW(IVENT) = XX0
        HH(IVENT) = XX0
        HL(IVENT) = XX0
        HHP(IVENT) = XX0
        HLP(IVENT) = XX0
	  VFACE(IVENT) = 1
  130 CONTINUE

C     SET THE TIME STEP AND INNER STEP DIVISION FOR TIME SPLITTING
C     WE DO NOT LET THE USER CHOOSE THESE

      DELTAT = 1.0D0
      MAXINR = 5

C     DEFINE ALL THE "UNIVERSAL CONSTANTS

      SIGM = 5.67D-8
      CP = 1012.0D0
      GAMMA = 1.40D0
      RGAS = (GAMMA-1.0D0) / GAMMA * CP
      MINMAS = 0.0D0
      G = 9.80D0
      STIME = XX0
      TREF = 288.D0
      LIMO2 = 0.10D0
      GMWF = 16.0D0
      HCOMBA = 50000000.0D0
      PREF = 1.013D+5
      PA = PREF
      POFSET = PREF
      SAL = XX0
      SAL2 = -1.0D0
      TE = TREF
      TA = TREF
      TGIGNT = TE + 200.D0
      EXTA = TA
      EXPA = PA
      EXSAL = SAL
      WINDV = XX0
      WINDRF = 10.D0
      WINDPW = 0.16D0
	do i = 0, mxfire
	  objmaspy(i) = xx0
	  radio(i) = xx0
  	  radconsplit(i) = 0.15d0
  	end do
  	tradio = xx0
      QRADRL = 0.15D0

C     NORMAL AIR

      O2N2(1) = 0.77D0
      O2N2(2) = 0.23D0

C     A SPECIFIED FIRE IN THE CENTER OF THE ROOM

      LFBT = 2
      LFBO = 0
      LFMAX = 1
	heatfl = .false.
	heatfq = 0.0
	heatfp(1) = xm1
	heatfp(2) = xm1
	heatfp(3) = xm1

C     SET TO -1 AS A FLAG FOR NPUTP INITIALIZATION - ANY VALUE NOT SET
C     WILL BE SET TO THE DEFAULT WHICH IS THE CENTER OF THE RESPECTIVE WALL

      FPOS(1) = xm1
      FPOS(2) = xm1
      FPOS(3) = xm1

C     SET UP DEFAULT VALUES FOR THE CHEMISTRY

      DO 190 I = 1, NV

C     DEFINE THE VENTS AS BEING OPEN

        DO 180 IVENT=1, MXVENTS
          QCVENT(IVENT,I) = 1.0D0
  180   CONTINUE
        TFIRED(I) = 86400.D0
        HFIRED(I) = XX0
        AFIRED(I) = XX0
        BFIRED(I) = 0.000D0
        QFIRED(I) = BFIRED(I) * HCOMBA
        HCRATIO(I) = 0.3333333D0
        HOCBMB(I) = HCOMBA
        COCO2(I) = XX0
        CCO2(I) = XX0
  190 CONTINUE

!	Start with vents open: h for hvent, v for vvent, and m for mvent

	do 191 i = 1,mxvents
	qcvh(1,i) = xx0
	qcvh(2,i) = xx1
	qcvh(3,i) = xx0
  191 qcvh(4,j) = xx1

	do 193 i = 1, nr
	qcvv(1,i) = xx0
	qcvv(2,i) = xx1
	qcvv(3,i) = xx0
  193 qcvv(4,i) = xx1

!	Note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering
!	Since there is not "thing" associated with a filter, there is no (as of 11/21/2006) 
!		way to have an intial value other than 0 (no filtering).
	do 194 i = 1, mfan
	qcvf(1,i) = xx0
	qcvf(2,i) = xx0
	qcvf(3,i) = xx0
	qcvf(4,i) = xx0
	qcvm(1,i) = xx0
	qcvm(2,i) = xx1
	qcvm(3,i) = xx0
  194 qcvm(4,i) = XX1
  192 continue

      HCRATT = HCRATIO(1)

C     TURN HVAC OFF INITIALLY

      NNODE = 0
      NFT = 0
      NFAN = 0
	nfilter = 0
      NBR = 0
      NEXT = 0
      HVGRAV = G
      HVRGAS = RGAS
      MVCALC = .FALSE.
      DO 200 I = 1, MNODE
        HVGHT(I) = XX0
  200 CONTINUE

C*** INITIALIZE DETECTORS

      DO 210 I = 1, MXDTECT
        XDTECT(I,DRTI) = 50.0D0
        XDTECT(I,DSPRAY) = -300.D0
        XDTECT(I,DXLOC) = -1.0D0
        XDTECT(I,DYLOC) = -1.0D0
        XDTECT(I,DZLOC) = -3.0D0/39.37D0
        XDTECT(I,DTRIG) = 330.3722D0
        XDTECT(I,DVEL) = 0.D0
        XDTECT(I,DVELO) = 0.D0
        XDTECT(I,DTACT) = 99999.D0
        IXDTECT(I,DTYPE) = 2
        IXDTECT(I,DROOM) = 1
        IXDTECT(I,DQUENCH) = 0
        IXDTECT(I,DACT) = 0
  210 CONTINUE
      NDTECT = 0
      DO 220 I = 1, NR
         IQUENCH(I) = 0
  220 CONTINUE

C*** initialize room to room heat transfer data structures

      NSWAL = 0

C*** initialize target counter

      NTARG = 0

      DO 230 ITARG = 1, MXTARG
        IXTARG(TRGMETH,ITARG) = XPLICIT
        IXTARG(TRGEQ,ITARG) = PDE
        IXTARG(TRGBACK,ITARG) = INT
        CXTARG(ITARG) = 'DEFAULT'
  230 CONTINUE

C*** initialize JACCOL  

      JACCOL = -2
      NEQOFF = 10

C*** initialize hall start time

      DO 240 I = 1, NR
        ZZHALL(I,IHTIME0) = -1.0D0
        ZZHALL(I,IHVEL) = -1.0D0
        ZZHALL(I,IHDEPTH) = -1.0D0
        ZZHALL(I,IHMAXLEN) = -1.0D0
        ZZHALL(I,IHHALF) = -1.0D0
        ZZHALL(I,IHTEMP) = 0.0D0
        ZZHALL(I,IHORG) = -1.0D0
        IZHALL(I,IHDEPTHFLAG) = 0
        IZHALL(I,IHHALFFLAG) = 0
        IZHALL(I,IHMODE) = IHAFTER
        IZHALL(I,IHROOM) = 0
        IZHALL(I,IHVELFLAG) = 0
        IZHALL(I,IHVENTNUM) = 0
        IZHALL(I,IHXY) = 0
        DO 250 IVENT = 1, MXVENT
          ZZVENTDIST(I,IVENT) = -1.
  250   CONTINUE
  240 CONTINUE
      UPDATEHALL = .FALSE.

      DO 260 I = 1, NR
      DO 260 J = 1, NR
      DO 260 K = 1, 4
        IJK(I,J,K) = 0
  260 CONTINUE
      NVENTIJK = 0

      DO 270 I = 1, NR
        IZRVOL(I) = 0
        DO 280 J = 1, MXPTS
          ZZRVOL(J,I)=0.0D0
          ZZRAREA(J,I)=0.0D0
          ZZRHGT(J,I)=0.0D0
  280   CONTINUE
  270 CONTINUE

C*** initialzie time step checking

      ZZDTCRIT = 1.0D-09
      IZDTNUM = 0
      IZDTMAX = 100
      IZDTFLAG = .TRUE.

C***  initialize GUISELCT

      DO 290 I = 1, MAXCOL
        GUISELCT(I) = 0
  290 CONTINUE

C*** initialize inter-compartment heat transfer fractions

      DO 300 I = 1, NR
        DO 310 J = 1, NR
          ZZHTFRAC(I,J) = 0.0D0
  310   CONTINUE
  300 CONTINUE

      do 320 j = 0, nr
        izheat(j) = 0
        do 330 i = 1, nr
          izhtfrac(i,j) = 0
  330   continue
  320 continue
  
      do lsp = 1, ns
        do j = upper, lower
            do i = 1, nr
            zzgspec(i,j,lsp) = xx0
            zzcspec(i,j,lsp) = xx0            
            end do
        end do
      end do

      RETURN
      END

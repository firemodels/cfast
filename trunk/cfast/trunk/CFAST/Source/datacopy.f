      SUBROUTINE DATACOPY(PDIF,IFLAG)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DATACOPY
C
C     Source File: DATACOPY.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Calculate environment variables from the solver vector
C
C     Arguments: PDIF   Solver vector
C                IFLAG  Action flag:
C
C     IFLAG = CONSTVAR ==> Constant data (data that does not change 
C                          with time)
C     IFLAG = ODEVARA  ==> ODE variables: pressure, temperature and upper 
C                          layer volume
C     IFLAG = ODEVARB  ==> Species data and wall temperature profile.  
C                          Use pold and pdold to estimate species
C     IFLAG = ODEVARC  ==> Species data and wall temperature profile.
C                          Use pdif array for species
C
C     Revision History:
C        Created:  06/21/1990 by WWJ
C        Modified: 07/26/1990 by WWJ:
C                  Changed mapping to eliminate outside room fixed index in 
C                  wall species do loop
C        Modified: 03/23/1991 by WWJ:
C                  Fixed initialization error
C        Modified: 06/14/1992 by GPF:
C                  Add HVAC environment varibles for species and mass
C        Modified: 11/28/1992 by PAR:
C                  Changed th calculation of ZZCSPEC so that it is always 
C                  calculated
C        Modified: 11/28/1992 by PAR:
C                  Changed the manner in which VOLFRAC is calculated for
C                  VFLOW
C        Modified: 12/22/1992 by RDP:
C                  Added calculation of a "mass fraction" for CT for flow 
C                  calculation
C        Modified: 2/5/1993 by GPF and PAR:
C                  Added wall area environment variables and pointers
C                  into the Jacobian matrix in support of the reduced
C                  Jacobian option
C        Modified: 3/19/1993 by RDP:
C                  Added flag value ODEVARC to use pdif array directly for 
C                  species data.
C        Modified: 2/25/1994 by GPF:
C                  defined data structures for the room to room heat transfer
C                  option
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/1994 by GPF:
C                  define data structures used by target calculation
C        Modified: 4/25/95 by GPF:
C                  Remove ZZ variables no longer used (ZZxMIN, ZZxMAX where x=P,G,W).
C                  Added definition of iztarg,  For implicit targets defined temperature
C                  in xxtarg every iteration
C        Modified: 6/30/95 by GPF:
C                  if dassl oxygen option is active (OPTION(FOXYGEN).eq.ON) then
C                  get oxygen mass and concentration info from DASSL solver array
C                  rather than portion of array solved explicitly 
C        Modified: 7/13/95 by GPF:
C                  defined IZEQMAP and IZHVAC, two mapping arrays used for the new
C                  Jacobian speed up option
C        Modified: 8/15/95 by PAR:
C	             added setting the enviromental varibles for a flame spread
C	             type object.
C        Modified: 2/5/96 by GPF:  removed reduced jacobian option added on 2/5/93.
C        Modified: 2/9/96 by GPF:  fixed problems with flow withdrawel from a layer
C                                  when the layer is very small.
C        Modified: 3/19/96 by GPF:  fixed defn of lower layer product concentrations
C                                   when using shaft option
C        Modified: 7/22/96 by GPF: added data structure definitions for use by
C                                  the hall option
C        Modified:11/25/96 by GPF: added non-rectangular room option
C        Modified:2/14/97  by GPF: made memory requirements smaller by changing
C                                  several o(n**2) data structures to o(n)
C                                  Also, associated added wind pressure rise calculation
C                                  to vents instead of rooms (now we can have wind come in
C                                  one window of a room and go out another.
C        Modified:10/23/97 by GPF: defined ZZDISC array.  This array contains
C                                  discontinuity time points (now where vents
C                                  change opening areas.
!	   Modified: 8/05 by wwj; extend compartment pairwise connections to 25
C        
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "vents.fi"
      include "wnodes.fi"
      include "dervs.fi"
      include "params.fi"
      include "wdervs.fi"
      include "opt.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"

C     ORDER OF VARIABLES IS DEFINED IN THE ROUTINE OFFSET

      DIMENSION PDIF(*)
      INTEGER FRMASK(mxccv)

      XX0 = 0.0D0
      XX2 = 2.0D0
      XX1 = 1.0D0
      VMINFRAC = 1.0D-4
      IF (IFLAG.EQ.CONSTVAR) THEN
        DO 10 IROOM = 1, N
          ZZVMIN(IROOM) = MIN(VMINFRAC * VR(IROOM), XX1)
          ZZVMAX(IROOM) = VR(IROOM) - ZZVMIN(IROOM)
   10   CONTINUE
        DO 20 IROOM = 1, NM1
          ZZYFLOR(IROOM) = HFLR(IROOM)
          ZZYCEIL(IROOM) = HRP(IROOM)

C*** DEFINE WALL CENTERS

          XX = bR(IROOM)
          XX2 = XX/2.0D0
          YY = dR(IROOM)
          YY2 = YY/2.0D0
          ZZ = HRP(IROOM)
          ZZWCEN(IROOM,1,1) = XX2
          ZZWCEN(IROOM,1,2) = YY2
          ZZWCEN(IROOM,1,3) = ZZ

          ZZWCEN(IROOM,2,1) = XX2
          ZZWCEN(IROOM,2,2) = YY

          ZZWCEN(IROOM,3,1) = XX
          ZZWCEN(IROOM,3,2) = YY2

          ZZWCEN(IROOM,4,1) = XX2
          ZZWCEN(IROOM,4,2) = 0.0D0

          ZZWCEN(IROOM,5,1) = 0.0D0
          ZZWCEN(IROOM,5,2) = YY2

          ZZWCEN(IROOM,6,1) = XX2
          ZZWCEN(IROOM,6,2) = YY

          ZZWCEN(IROOM,7,1) = XX
          ZZWCEN(IROOM,7,2) = YY2

          ZZWCEN(IROOM,8,1) = XX2
          ZZWCEN(IROOM,8,2) = 0.0D0

          ZZWCEN(IROOM,9,1) = 0.0D0
          ZZWCEN(IROOM,9,2) = YY2

          ZZWCEN(IROOM,10,1) = XX2
          ZZWCEN(IROOM,10,2) = YY2
          ZZWCEN(IROOM,10,3) = 0.0D0

   20   CONTINUE
        ZZYFLOR(N) = 0.0D0
        ZZYCEIL(N) = 100000.D0
        ZZVOL(N,UPPER) = 0.0D0
        ZZVOL(N,LOWER) = 100000.0D0
        ZZHLAY(N,UPPER) = 0.0D0
        ZZHLAY(N,LOWER) = 100000.0D0
        ZZRELP(N) = XX0
        ZZPABS(N) = POFSET
        ZZTEMP(N,UPPER) = 300.0D0
        ZZTEMP(N,LOWER) = 300.0D0
        DO 30 LSP = 3, NS
          ZZCSPEC(N,UPPER,LSP) = XX0
          ZZCSPEC(N,LOWER,LSP) = XX0
   30   CONTINUE
        ZZCSPEC(N,UPPER,1) = 0.770D0
        ZZCSPEC(N,LOWER,1) = 0.770D0
        ZZCSPEC(N,UPPER,2) = 0.230D0
        ZZCSPEC(N,LOWER,2) = 0.230D0
        DO 40 LAYER = UPPER, LOWER
          ZZRHO(N,LAYER) = ZZPABS(N) / RGAS / ZZTEMP(N,LAYER)
          ZZMASS(N,LAYER) = ZZRHO(N,LAYER) * ZZVOL(N,LAYER)
   40   CONTINUE

C*** DEFINE VENT DATA STRUCTURES

	  do 41 i = 1, mxccv
   41   frmask(i) = 2 ** i
        NVENTS = 0
        DO 70 I = 1, NM1
          DO 60 J = I + 1, N
            IF (NW(I,J).NE.0) THEN
              DO 50 K = 1, mxccv
                IF (IAND(FRMASK(K),NW(I,J)).NE.0) THEN
                  NVENTS = NVENTS + 1
                  IIJK = IJK(I,J,K)
                  ZZVENT(NVENTS,1) = HL(IIJK)
                  ZZVENT(NVENTS,2) = HH(IIJK)
                  ZZVENT(NVENTS,3) = BW(IIJK)
                  ZZVENT(NVENTS,4) = HALLDIST(IIJK,1)
                  ZZVENT(NVENTS,5) = HALLDIST(IIJK,2)
                  IZVENT(NVENTS,1) = I
                  IZVENT(NVENTS,2) = J
                  IZVENT(NVENTS,3) = K

C*** is "from" room a hall?

                  IF(IZHALL(I,IHROOM).EQ.1)THEN
                    IZVENT(NVENTS,4) = 1
                   ELSE
                    IZVENT(NVENTS,4) = 0
                  ENDIF

C*** is "to" room a hall?

                  IF(IZHALL(J,IHROOM).EQ.1)THEN
                    IZVENT(NVENTS,5) = 1
                   ELSE
                    IZVENT(NVENTS,5) = 0
                  ENDIF

!	add face (vface) to the data structure

				izvent(nvents,6) = vface(iijk)

C*** compute pressure rise due to wind.  This value is only defined for outside rooms

                  WCOS = WINDC(IIJK)
                  IF(J.EQ.N.AND.WCOS.NE.XX0)THEN

C*** compute wind velocity and pressure rise at the average vent height

                    HAVG = (ZZVENT(NVENTS,1)+ZZVENT(NVENTS,2))/2.0D0 
                    HAVG = HAVG + ZZYFLOR(I) 

C*** we used to compute wind speed at the floor.  If we use this height
C    then we get identical answers to previous CFAST calculations
C                   HAVG = ZZYFLOR(I)

                    IF(WINDRF.NE.XX0)THEN
                      WINDVNEW = WINDV*(HAVG/WINDRF)**WINDPW
                     ELSE
                      WINDVNEW = WINDV
                    ENDIF
                    WINDDP = WCOS*EXRA*WINDVNEW**2/2.0D0
                    ZZVENT(NVENTS,6) = WINDDP
                   ELSE
                    ZZVENT(NVENTS,6) = XX0
                  ENDIF

                END IF
   50         CONTINUE
            END IF
   60     CONTINUE
   70   CONTINUE

C*** define vents for vertical flow

        NVVENT = 0
        DO 90 I = 1, N
          DO 80 J = 1, N
            IF (NWV(I,J).NE.0) THEN
              NVVENT = NVVENT + 1
              IVVENT(NVVENT,1) = I
              IVVENT(NVVENT,2) = J
			qcvv(1,nvvent) = qcvpp(1,i,j)
			qcvv(2,nvvent) = qcvpp(2,i,j)
			qcvv(3,nvvent) = qcvpp(3,i,j)
			qcvv(4,nvvent) = qcvpp(4,i,j)
            END IF
   80     CONTINUE
   90   CONTINUE
C*** define discontinuity array.  first we look at vent openings

        XDELT = NSMAX / DELTAT
        ITSTOP = XDELT + 1
        TSTOP = ITSTOP - 1

        ZZDISC(0) = 0.0D0
	  zzdisc(1) = tstop
        III = 1
!	Add each of the change arrays to the discontinuity list
	  do  i = 1, nvents
		 iii = iii + 1
		 zzdisc(iii) = qcvh(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvh(3,i)
        enddo
	  do  i = 1, nvvent
		 iii = iii + 1
		 zzdisc(iii) = qcvv(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvv(3,i)
	  enddo
	  do  i = 1, nfan
		 iii = iii + 1
		 zzdisc(iii) = qcvm(1,i)
		 iii = iii + 1
		 zzdisc(iii) = qcvm(3,i)
	  enddo
        IZNDISC = III

!	Put the discontinuity array into order
	  call shellsort (zzdisc(0), izndisc+1)

C*** DEFINE IZWMAP FOR JAC AND OTHER CONSTANTS FOR THE CUSTOM LINEAR
C***   ALGEBRA ROUTINES THAT ARE CALLED IN DASSL

        ICOL = 0
        IEQ = NOFWT
c     set izwmap2 for the outside room first
        DO 112 IWALL = 1,4
          IZWMAP2(IWALL,NM1+1) = 0
  112   CONTINUE
        DO 110 IROOM = 1, NM1
          ICNT = 0
          IZNWALL(IROOM) = 0
          DO 100 IWALL = 1, 4
            IF (SWITCH(IWALL,IROOM)) THEN
              IEQ = IEQ + 1
              IZWMAP2(IWALL,IROOM) = IEQ
              ICNT = ICNT + 1
              ICOL = ICOL + 1
              IZNWALL(IROOM) = IZNWALL(IROOM) + 1

c*** define izwall, to describe ceiling-floor connections
c    first assume that walls are connected to the outside

              II = IEQ - NOFWT
              IZWALL(II,1) = IROOM
              IZWALL(II,2) = IWALL
              IZWALL(II,3) = NM1 + 1
              IF(IWALL.EQ.1.OR.IWALL.EQ.2)THEN
                IWFAR = 3 - IWALL
               ELSE
                IWFAR = IWALL
              ENDIF
              IZWALL(II,4) = IWFAR
              IZWALL(II,5) = IWBOUND

            ELSE
              IZWMAP2(IWALL,IROOM) = 0
            END IF
  100     CONTINUE
          IZWMAP(1,IROOM) = ICOL - ICNT + 1
          IZWMAP(2,IROOM) = ICNT
  110   CONTINUE

c*** update izwall for ceiling/floors that are connected 

        DO 115 I = 1, NSWAL
          IFROMR = IZSWAL(I,1)
          IFROMW = IZSWAL(I,2)
          ITOR = IZSWAL(I,3)
          ITOW = IZSWAL(I,4)
          IEQFROM = IZWMAP2(IFROMW,IFROMR) - NOFWT
          IEQTO = IZWMAP2(ITOW,ITOR) - NOFWT

          IZWALL(IEQFROM,3) = ITOR
          IZWALL(IEQFROM,4) = ITOW
          IZWALL(IEQFROM,5) = 1

          IZWALL(IEQTO,3) = IFROMR
          IZWALL(IEQTO,4) = IFROMW
          IZWALL(IEQTO,5) = 1

  115   CONTINUE 

        JACN1 = NOFPMV - NOFP
        JACN2 = NOFWT - NOFPMV
        JACN3 = NOFPRD - NOFWT
        JACDIM = JACN1 + JACN2 + JACN3

c*** define maps for dassl eqs <--> target data structures

        IEQ = 0
        DO 300 ITARG = 1, NTARG
           IF(IXTARG(TRGMETH,ITARG).EQ.MPLICIT)THEN
             IEQ = IEQ + 1
             IZTARG(ITARG) = IEQ
            ELSE
             IZTARG(ITARG) = 0
           ENDIF
  300   CONTINUE

C*** associate an equation type (ie pressure, temperature etc as defined by offsets)
C    with each dassl equation

        IEQ = 0
        DO 320 ITYPE = 1, NEQOFF
          IBEG = NOFSETS(ITYPE)
          IEND = NOFSETS(ITYPE+1)-1
          DO 330 I = IBEG, IEND
            IEQ = IEQ + 1
            IROOM = I + 1 - IBEG
            IZEQMAP(IEQ,1) = ITYPE
            IZEQMAP(IEQ,2) = IROOM
  330     CONTINUE
  320   CONTINUE

C*** indicate which rooms are connected to an hvac system

         DO 340 I = 1, NM1
           IZHVAC(I) = .FALSE.
  340    CONTINUE
         DO 350 II = 1, NEXT
            I = HVNODE(1,II)
            IZHVAC(I) = .TRUE.
  350    CONTINUE

      ELSE IF (IFLAG.EQ.ODEVARA) THEN
        DO 130 IROOM = 1, NM1
          ZZVOL(IROOM,UPPER) = MAX(PDIF(IROOM+NOFVU),ZZVMIN(IROOM))
          ZZVOL(IROOM,UPPER) = MIN(ZZVOL(IROOM,UPPER),ZZVMAX(IROOM))
          ZZVOL(IROOM,LOWER) = 
     +        MAX(VR(IROOM)-ZZVOL(IROOM,UPPER),ZZVMIN(IROOM))
          ZZVOL(IROOM,LOWER) = MIN(ZZVOL(IROOM,LOWER),ZZVMAX(IROOM))

C*** prevent flow from being withdrawn from a layer if the layer
C    is at the minium size

		VOLFRU(IROOM) = (ZZVOL(IROOM,UPPER)-VMINFRAC*VR(IROOM))/ 
     .                    VR(IROOM)*(1.0D0-2.0D0*VMINFRAC)
          VOLFRU(IROOM) = MAX(MIN(VOLFRU(IROOM),XX1),XX0)
          VOLFRL(IROOM) = 1.0D0 - VOLFRU(IROOM)
          VOLFRL(IROOM) = MAX(MIN(VOLFRL(IROOM),XX1),XX0)

c*** calculate layer height for non-rectangular rooms

          NPTS = IZRVOL(IROOM)
          IF(NPTS.EQ.0)THEN
            ZZHLAY(IROOM,UPPER) = ZZVOL(IROOM,UPPER) / AR(IROOM)
            ZZHLAY(IROOM,LOWER) = ZZVOL(IROOM,LOWER) / AR(IROOM)
           ELSE
            CALL INTERP(ZZRVOL(1,IROOM),ZZRHGT(1,IROOM),NPTS,
     .                  ZZVOL(IROOM,LOWER),1,ZZHLAY(IROOM,LOWER))
            ZZHLAY(IROOM,UPPER) = HR(IROOM) - ZZHLAY(IROOM,LOWER)            
          ENDIF

          ZZRELP(IROOM) = PDIF(IROOM)
          ZZPABS(IROOM) = PDIF(IROOM) + POFSET
          ZZTEMP(IROOM,UPPER) = PDIF(IROOM+NOFTU)
          ZZTEMP(IROOM,LOWER) = PDIF(IROOM+NOFTL)

C*** There is a problem with how flow is being withdrawn from layers
C    when the layers are small and the flow is large (for example with
C    ceiling vents.  As a result, DASSL, can predict a negative TEMPERATURE
C    (because the rhs of the temperature equation is wrong).  The following
C    code causes the temperature of the opposite layer to be used in these
C    situations.

          IF(ZZTEMP(IROOM,UPPER).LT.0.0D0)THEN
            ZZTEMP(IROOM,UPPER)=ZZTEMP(IROOM,LOWER)
          ENDIF
          IF(ZZTEMP(IROOM,LOWER).LT.0.0D0)THEN
            ZZTEMP(IROOM,LOWER)=ZZTEMP(IROOM,UPPER)
          ENDIF

          IF(IZSHAFT(IROOM).EQ.1)THEN
             ZZTEMP(IROOM,LOWER) = ZZTEMP(IROOM,UPPER)
          ENDIF

C*** COMPUTE AREA OF 10 WALL SEGMENTS

          XX = bR(IROOM)
          YY = dR(IROOM)
          ZZU = ZZHLAY(IROOM,UPPER)
          ZZL = ZZHLAY(IROOM,LOWER)
          ZZWAREA2(IROOM,1) = AR(IROOM)
          ZZWAREA2(IROOM,2) = ZZU*XX
          ZZWAREA2(IROOM,3) = ZZU*YY
          ZZWAREA2(IROOM,4) = ZZU*XX
          ZZWAREA2(IROOM,5) = ZZU*YY
          ZZWAREA2(IROOM,6) = ZZL*XX
          ZZWAREA2(IROOM,7) = ZZL*YY
          ZZWAREA2(IROOM,8) = ZZL*XX
          ZZWAREA2(IROOM,9) = ZZL*YY
          ZZWAREA2(IROOM,10) = AR(IROOM)

C*** COMPUTE AREA OF 4 WALL SEGMENTS

          ZZWAREA(IROOM,1) = AR(IROOM)
          ZZWAREA(IROOM,2) = AR(IROOM)
          ZZWAREA(IROOM,3) = (YY + XX)*ZZU * XX2
          ZZWAREA(IROOM,4) = MAX(XX0,(YY+XX)*ZZL*XX2)

C*** DEFINE Z WALL CENTERS (THE Z COORDINATE CHANGES WITH TIME)
C    (OTHER COORDINATES ARE STATIC AND ARE DEFINED EARLIER)

          DO 116 I = 1, 4
             YLAY = ZZHLAY(IROOM,LOWER)
             ZZWCEN(IROOM,I+1,3) =  (ZZYCEIL(IROOM)+YLAY)/2.0D0
             ZZWCEN(IROOM,I+5,3) = YLAY/2.0D0
  116     CONTINUE

          DO 120 LAYER = UPPER, LOWER
            ZZRHO(IROOM,LAYER) = ZZPABS(IROOM) / RGAS / 
     +          ZZTEMP(IROOM,LAYER)
            ZZMASS(IROOM,LAYER) = ZZRHO(IROOM,LAYER) * 
     +          ZZVOL(IROOM,LAYER)
  120     CONTINUE
  130   CONTINUE

C*** RECORD WHICH LAYER TARGET IS IN

        DO 135 ITARG = 1, NTARG
           IROOM = IXTARG(TRGROOM,ITARG)
           YLAY = ZZHLAY(IROOM,LOWER)
           YTARG = XXTARG(TRGCENZ,ITARG)
           IF(YTARG.GE.YLAY)THEN
              IXTARG(TRGLAYER,ITARG) = UPPER
             ELSE
              IXTARG(TRGLAYER,ITARG) = LOWER
           ENDIF
  135   CONTINUE

c*** stuff dassl estimate of target temperature's solved implicitly
c    (ie solved by dassl)

        DO 310 ITARG = 1, NTARG
          IF(IXTARG(TRGMETH,ITARG).NE.MPLICIT)GO TO 310
          IEQ = IZTARG(ITARG)
          XXTARG(TRGTEMPF,ITARG) = P(IEQ+NOFTT)
  310   CONTINUE

C*** define surface wall temperatures (interior=1,exterior=2)

      ELSE IF (IFLAG.EQ.ODEVARB.OR.IFLAG.EQ.ODEVARC) THEN
        ISOF = NOFWT
        DO 150 IROOM = 1, NM1
          DO 140 IWALL = 1, NWAL
            IWALLEQ = IZWMAP2(IWALL,IROOM)
            IF(IWALLEQ.NE.0)THEN
              IEQFROM = IWALLEQ - NOFWT
              IFROMR = IZWALL(IEQFROM,1)
              IFROMW = IZWALL(IEQFROM,2)
              ITOR = IZWALL(IEQFROM,3)
              ITOW = IZWALL(IEQFROM,4)
              ZZWTEMP(IROOM,IWALL,1) = PDIF(IWALLEQ)
              IWALLEQ2 = IZWMAP2(ITOW,ITOR)
              IINODE = NUMNODE(1,IWALL,IROOM)
              IF(IWALLEQ2.EQ.0)THEN
                ZZWTEMP(IROOM,IWALL,2) = TWJ(IINODE,IROOM,IWALL)
               ELSE
                ZZWTEMP(IROOM,IWALL,2) = PDIF(IWALLEQ2)
              ENDIF
             ELSE

c*** if we're not solving for the wall temperature then set it
c    to the layer temperature that it is adjacent too.  Note,
c    zzwtemp(iroom,iwall,2) is only referenced if the iwall'th
c    wall in room iroom is being solved with the heat equation
c
              IF(IWALL.EQ.1.OR.IWALL.EQ.3)THEN
                ILAY = UPPER
               ELSE
                ILAY = LOWER
              ENDIF
              ZZWTEMP(IROOM,IWALL,1) = ZZTEMP(IROOM,ILAY)
            ENDIF
  140     CONTINUE
  150   CONTINUE

C*** DEFINE SPECIES amounts

        ISOF = NOFPRD
        DO 170 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            DO 160 IROOM = 1, NM1
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPGAS = PDIF(ISOF)
              END IF
              ZZGSPEC(IROOM,UPPER,LSP) = MAX(PPGAS,XX0)

              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPGAS = PDIF(ISOF)
              END IF
              ZZGSPEC(IROOM,LOWER,LSP) = MAX(PPGAS,XX0)
  160       CONTINUE
          END IF
  170   CONTINUE

C     DEFINE SPECIES mass fractions: normalize to total product mass 
C     rather than total mass (this is equivalent to what was begin done 
C     in chemie)

        DO 200 IROOM = 1, NM1
          TOTL = XX0
          TOTU = XX0
          DO 180 LSP = 1, MIN(9,NS)
            IF (ACTIVS(LSP)) THEN
              TOTU = TOTU + ZZGSPEC(IROOM,UPPER,LSP)
              TOTL = TOTL + ZZGSPEC(IROOM,LOWER,LSP)
            END IF
  180     CONTINUE
          RTOTL = 1.0D0
          RTOTU = 1.0D0
          IF (TOTL.GT.XX0) RTOTL = 1.0D0 / TOTL
          IF (TOTU.GT.XX0) RTOTU = 1.0D0 / TOTU
          DO 190 LSP = 1, NS
            IF (ACTIVS(LSP)) THEN
              ZZCSPEC(IROOM,UPPER,LSP) = ZZGSPEC(IROOM,UPPER,LSP) * 
     +            RTOTU
              ZZCSPEC(IROOM,LOWER,LSP) = ZZGSPEC(IROOM,LOWER,LSP) * 
     +            RTOTL
              IF(IZSHAFT(IROOM).EQ.1)THEN
                ZZCSPEC(IROOM,LOWER,LSP) = ZZCSPEC(IROOM,UPPER,LSP)
              ENDIF
            END IF
  190     CONTINUE

c*** if oxygen is a dassl variable then use dassl solve array to define
c    zzgspec and zzcspec values for oxygen.
C    make sure oxygen never goes negative

          IF(OPTION(FOXYGEN).EQ.ON)THEN
             OXYL = MAX(P(IROOM+NOFOXYL),XX0)
             OXYU = MAX(P(IROOM+NOFOXYU),XX0)
             ZZGSPEC(IROOM,LOWER,2) = OXYL
             ZZGSPEC(IROOM,UPPER,2) = OXYU
             ZZCSPEC(IROOM,LOWER,2) = OXYL/
     .                                ZZMASS(IROOM,LOWER)
             ZZCSPEC(IROOM,UPPER,2) = OXYU/
     .                                ZZMASS(IROOM,UPPER)
             IF(IZSHAFT(IROOM).EQ.1)THEN
               ZZCSPEC(IROOM,LOWER,2) = ZZCSPEC(IROOM,UPPER,2)
             ENDIF
          ENDIF
  200   CONTINUE

C     define hcl absorption

        IF (ACTIVS(6)) THEN
          ISOF = NOFHCL
          DO 220 IROOM = 1, NM1
            DO 210 LSP = 1, NWAL
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPWGAS = POLD(ISOF) + DT * PDOLD(ISOF)
              ELSE
                PPWGAS = PDIF(ISOF)
              END IF
              ZZWSPEC(IROOM,LSP) = PPWGAS
  210       CONTINUE
  220     CONTINUE
        END IF
      END IF

C     copy hvac product values for each hvac system

      IF (NHVSYS.NE.0.AND.NS.NE.0) THEN
        ISOF = NOFHVPR
        DO 230 ISYS = 1, NHVSYS
          ZZHVM(ISYS) = XX0
  230   CONTINUE
        DO 250 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            DO 240 ISYS = 1, NHVSYS
              ISOF = ISOF + 1
              IF (IFLAG.EQ.ODEVARB) THEN
                PPHV = MAX(XX0,POLD(ISOF)+DT*PDOLD(ISOF))
              ELSE
                PPHV = MAX(XX0,PDIF(ISOF))
              END IF
              ZZHVPR(ISYS,LSP) = PPHV
              ZZHVM(ISYS) = ZZHVM(ISYS) + ZZHVPR(ISYS,LSP)
  240       CONTINUE
          END IF
  250   CONTINUE
      END IF
      RETURN
      END

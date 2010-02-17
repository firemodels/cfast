      SUBROUTINE TRHEAT(UPDATE,METHOD,DT,XPSOLVE,DELTA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TRHEAT
C
C     Source File: TRHEAT.SOR
C
C     Functional Class:  
C
C     Description:  Compute dassl residuals associated with targets
C
C     Arguments: UPDATE   variable indicating whether temperature profile
C                         should be updated
C                METHOD   one of steady, mplicit or xplicit (note: these
C                         are parameter values, not mis-pelld)
C                DT       time step
C                XPSOLVE  dassl derivative estimate
C                DELTA    dassl residual array
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"
      include "cenviro.fi"
      DIMENSION TMP(TRGTNUM), WALLDX(TRGTNUM), NMNODE(2), DELTA(*)
      DIMENSION XPSOLVE(*), TGRAD(2)
      LOGICAL FIRST
      INTEGER UPDATE
      DIMENSION WK(1),WSPEC(1),WRHO(1)
      SAVE FIRST,TMP
      DATA FIRST/.TRUE./
      
      IF(METHOD.EQ.STEADY)RETURN

C*** INITIALIZE NON-DIMENSIONAL TARGET NODE LOCATIONS THE FIRST TIME TRHEAT IS CALLED

      IF(FIRST)THEN
        FIRST = .FALSE.
        NNN = TRGTNUM - 1
        TMP(1) = 1.0D0
        TMP(NNN) = 1.0D0
        DO 10 I = 2, NNN/2 
          TMP(I) = TMP(I-1)*1.50D0
          TMP(NNN+1-I) = TMP(I)
   10   CONTINUE
        IF(MOD(NNN,2).EQ.1)TMP(NNN/2+1)=TMP(NNN/2)*1.50D0
        SUM = 0.0D0
        DO 20 I = 1, NNN
          SUM = SUM + TMP(I)
   20   CONTINUE
        DO 30 I = 1, NNN
          TMP(I) = TMP(I)/SUM
   30   CONTINUE
      ENDIF

C*** CACLULATE NET FLUX STRIKING EACH SIDE OF TARGET

      CALL TARGET(METHOD)

C*** FOR EACH TARGET CALCULATE ODE OR PDE RESIDUAL AND UPDATE TARGET
C    TEMPERATURE (IF UPDATE=1 OR 1)

      DO 40 ITARG = 1, NTARG
         IF(IXTARG(TRGMETH,ITARG).NE.METHOD)GO TO 40
         WFLUXIN = XXTARG(TRGNFLUXF,ITARG)
         WFLUXOUT = XXTARG(TRGNFLUXB,ITARG)
         WSPEC(1) = XXTARG(TRGCP,ITARG)
         WRHO(1) =  XXTARG(TRGRHO,ITARG)
         WK(1) =  XXTARG(TRGK,ITARG)
         XL = XXTARG(TRGL,ITARG)
         IIMETH = IXTARG(TRGMETH,ITARG)
         IIEQ = IXTARG(TRGEQ,ITARG)

C*** COMPUTE THE PDE RESIDUAL 

         IF(IIEQ.EQ.PDE.OR.IIEQ.EQ.CYLPDE)THEN
           IF(IIMETH.EQ.MPLICIT)THEN
             TEMPIN = XXTARG(TRGTEMPF,ITARG)
             IWBOUND = 3
            ELSE
             IWBOUND = 4
           ENDIF
           NMNODE(1) = TRGTNUM
           NMNODE(2) = TRGTNUM - 2
           NSLAB = 1
           IF(IIEQ.EQ.PDE)THEN
             DO 60 I = 1, TRGTNUM - 1
               WALLDX(I) = XL*TMP(I)
   60        CONTINUE
   
             CALL CNDUCT(UPDATE,TEMPIN,TEMPOUT,DT,WK,WSPEC,WRHO,
     +        XXTARG(TRGTEMPF,ITARG),WALLDX,NMNODE,NSLAB,
     +        WFLUXIN,WFLUXOUT,IWBOUND,TGRAD,TDERV)
             IF(IIMETH.EQ.MPLICIT)THEN
               IEQ = IZTARG(ITARG)
               DELTA(NOFTT+IEQ) = XXTARG(TRGNFLUXF,ITARG)+WK(1)*TGRAD(1)
             ENDIF
           ELSE IF(IIEQ.EQ.CYLPDE)THEN
             CALL CYLCNDUCT(XXTARG(TRGTEMPF,ITARG),NMNODE(1),
     +                      WFLUXIN,DT,WK(1),WRHO(1),WSPEC(1),XL)
           ENDIF

C*** COMPUTE THE ODE RESIDUAL

          ELSEIF(IIEQ.EQ.ODE)THEN
            DDTEMP = (WFLUXIN+WFLUXOUT)/(WSPEC(1)*WRHO(1)*XL)
            IF(IIMETH.EQ.MPLICIT)THEN
              IEQ = IZTARG(ITARG)
              DELTA(NOFTT+IEQ) = DDTEMP - XPSOLVE(NOFTT+IEQ) 
             ELSEIF(IIMETH.EQ.XPLICIT)THEN
              IF(UPDATE.NE.0)THEN
                TTOLD = XXTARG(TRGTEMPF,ITARG)
                TTNEW = TTOLD + DT*DDTEMP
                XXTARG(TRGTEMPF,ITARG) = TTNEW
              ENDIF
            ENDIF

C*** ERROR, THE EQUATION TYPE CAN HAS TO BE EITHER PDE OR ODE
C    IF THE METHOD IS NOT STEADY

          ELSE

         ENDIF
            
   40 CONTINUE
      RETURN
      END

      SUBROUTINE TARGET(METHOD)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TARGET
C
C     Source File: TARGET.SOR
C
C     Functional Class:  
C
C     Description:  
C         Routine to calculate total flux striking a target.
C         This flux is used to calculate a target temperature,
C         assuming that the sum of incoming and outgoing flux is zero,
C         ie, assuming that the target is at steady state.
C
C     Arguments: METHOD
C
C     Revision History:
C     Created by GPF 9/26/94
C        Modified: 4/26/1995 gpf:
C                  expanded this routine to compute transient in addition to
C                  steady state target temperatures
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fltarget.fi"

      LOGICAL FIRST
      DIMENSION FLUX(2), DFLUX(2), TTARG(2)
      SAVE FIRST, SIGMA

      DATA FIRST/.TRUE./

      IF(FIRST)THEN
         FIRST = .FALSE.
         SIGMA = 5.67D-8
      ENDIF

C     CALCULATE FLUX TO USER SPECIFIED TARGETS, ASSUMING TARGET IS AT THERMAL EQUILIBRIUM
      IF (NTARG.GT.NM1) THEN
        DO 10 ITARG = 1, NTARG
          METHTARG = IXTARG(TRGMETH,ITARG)
          IF(METHOD.NE.METHTARG)GO TO 10
          IROOM = IXTARG(TRGROOM,ITARG)
          IF(METHTARG.EQ.STEADY)THEN
             NITER = 10
            ELSE
             NITER = 1
          ENDIF
          TTARG(1) = XXTARG(TRGTEMPF,ITARG)
          TTARG(2) = XXTARG(TRGTEMPB,ITARG)
          DO 20 ITER = 1, NITER
            CALL TARGFLUX(ITER,ITARG,TTARG,FLUX,DFLUX)
            IF(DFLUX(1).NE.0.0D0.AND.METHTARG.EQ.STEADY)THEN
               DDIF = FLUX(1)/DFLUX(1)
               TTARG(1) = TTARG(1) - DDIF
               IF(ABS(DDIF).LE.1.0D-5*TTARG(1))GO TO 30
            ENDIF
   20     CONTINUE
   30     CONTINUE
          IF(METHTARG.EQ.STEADY)THEN
            XXTARG(TRGTEMPF,ITARG) = TTARG(1)
            XXTARG(TRGTEMPB,ITARG) = TTARG(2)
          ENDIF
          XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *        + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
          XXTARG(TRGTFLUXB,ITARG) = QTWFLUX(ITARG,2) + QTFFLUX(ITARG,2)
     *        + QTCFLUX(ITARG,2) + QTGFLUX(ITARG,2)
          CALL TARGFLUX(NITER+1,ITARG,TTARG,FLUX,DFLUX)
          XXTARG(TRGNFLUXF,ITARG) = FLUX(1)
          XXTARG(TRGNFLUXB,ITARG) = FLUX(2)
   10   CONTINUE
      END IF

C     CALCULATE FLUX TO FLOOR TARGETS FOR THE PRE-EXISTING DATA STRUCTURE, 
C     ONTARGET, AND A FLASHOVER INDICATOR ON THE FLOOR
      IF(METHOD.EQ.STEADY)THEN
      DO 40 IROOM = 1, NM1
        ITARG = NTARG - NM1 + IROOM

C     AMBIENT TARGET

        TTARG(1) = TAMB(IROOM)
        TTARG(2) = ETA(IROOM)
        XXTARG(TRGTEMPF,ITARG) = TTARG(1)
        CALL TARGFLUX(1,ITARG,TTARG,FLUX,DFLUX)
        XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
        ONTARGET(IROOM) = XXTARG(TRGTFLUXF,ITARG)-SIGMA*TTARG(1)**4

C     FLASHOVER INDICATOR

        TTARG(1) = ZZWTEMP(IROOM,2,1)
        XXTARG(TRGTEMPF,ITARG) = TTARG(1)
        CALL TARGFLUX(1,ITARG,TTARG,FLUX,DFLUX)
        XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
        XXTARG(TRGNFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1) - SIGMA*TTARG(1)**4
   40 CONTINUE
      ENDIF

      RETURN
      END

      SUBROUTINE TARGFLUX(ITER,ITARG,TTARG,FLUX,DFLUX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TARGFLUX
C
C     Source File: TARGFLUX.SOR
C
C     Functional Class:  
C
C     Description:  Routine to calculate flux (and later, temperature)
C                   of a target.
C
C     Arguments: ITER   iteration number
C                ITARG  targetnumber
C                TTARG  front and back target input temperature
C                FLUX   front and back output flux
C                DFLUX  front and back output flux derivative
C
C     Revision History:
C     Created by GPF 9/26/94
C        Modified: 4/26/1995 gpf:
C                  expanded this routine to handle fluxes for both the transient and
C                  steady state target temperatures
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fltarget.fi"
      include "objects2.fi"

      DIMENSION MAP10(10), SVECT(3), FLUX(2), DFLUX(2)
      DIMENSION TTARG(2),QWTSUM(2),AWALLSUM(2),QGASSUM(2)
      LOGICAL FIRST
      SAVE FIRST, PI, SIGMA, COS45

      DATA FIRST/.TRUE./
      DATA MAP10/1,3,3,3,3,4,4,4,4,2/

      IF(FIRST)THEN
         FIRST = .FALSE.
         XX1 = 1.0D0
         PI = 4.0D0*ATAN(XX1)
         SIGMA = 5.67D-8
         COS45 = SQRT(XX1+XX1)/2.0D0
      ENDIF

      ABSU = 0.50D0
      ABSL = 0.01D0
      IROOM = IXTARG(TRGROOM,ITARG)

C*** TERMS THAT DO NOT DEPEND UPON THE TARGET TEMPERATURE ONLY NEED TO BE CALCULATED ONCE

      IF(ITER.EQ.1)THEN

C*** INITIALIZE FLUX COUNTERS: TOTAL, FIRE, WALL, GAS 

        DO 10 I = 1, 2
          QTFFLUX(ITARG,I) = 0.0D0
          QTGFLUX(ITARG,I) = 0.0D0
          QTWFLUX(ITARG,I) = 0.0D0
   10   CONTINUE

        NFIRERM = IFRPNT(IROOM,1)
        ISTART = IFRPNT(IROOM,2)

C*** COMPUTE RADIATIVE FLUX FROM FIRE

        DO 210 IFIRE = ISTART, ISTART + NFIRERM - 1
          SVECT(1) = XXTARG(TRGCENX,ITARG) - XFIRE(IFIRE,1)
          SVECT(2) = XXTARG(TRGCENY,ITARG) - XFIRE(IFIRE,2)
          SVECT(3) = XXTARG(TRGCENZ,ITARG) - XFIRE(IFIRE,3)
          COSANG = 0.0D0
          S = max(DNRM2(3,SVECT,1),objcl(ifire))
          IF(S.NE.0.0D0)THEN
            COSANG = -DDOT(3,SVECT,1,XXTARG(TRGNORMX,ITARG),1)/S
          ENDIF
          ZFIRE = XFIRE(IFIRE,3)
          ZTARG = XXTARG(TRGCENZ,ITARG)
          ZLAY = ZZHLAY(IROOM,LOWER)

C*** COMPUTE PORTION OF PATH IN LOWER AND UPPER LAYERS

          CALL GETYLYU(ZFIRE,ZLAY,ZTARG,S,ZL,ZU)
          ABSL = ABSORB(IROOM, LOWER)
          ABSU = ABSORB(IROOM, UPPER)
          TAUL = EXP(-ABSL*ZL)
          TAUU = EXP(-ABSU*ZU)
          QFIRE = XFIRE(IFIRE,8)
          IF(S.NE.0.0D0)THEN
            QFT = QFIRE*ABS(COSANG)*TAUU*TAUL/(4.0D0*PI*S**2)
           ELSE
            QFT = 0.0D0
          ENDIF

C*** DECIDE WHETHER FLUX IS HITTING FRONT OR BACK OF TARGET
C    IF IT'S HITTING THE BACK TARGET ONLY ADD CONTRIBUTION IF THE TARGET
C    IS INTERIOR TO THE ROOM

          IF(COSANG.GE.0.0D0)THEN
             QTFFLUX(ITARG,1) = QTFFLUX(ITARG,1) + QFT
            ELSE
             IF(IXTARG(TRGBACK,ITARG).EQ.INT)THEN
               QTFFLUX(ITARG,2) = QTFFLUX(ITARG,2) + QFT
             ENDIF
          ENDIF

  210   CONTINUE

!*** COMPUTE RADIATIVE FLUX FROM WALLS AND GAS

        DO 215 I = 1, 2
          AWALLSUM(I) = 0.0D0   
          QWTSUM(I) = 0.0D0
          QGASSUM(I) = 0.0D0
  215   CONTINUE
        DO 220 IWALL = 1, 10
          QOUT = RDQOUT(MAP10(IWALL),IROOM)
          SVECT(1) = XXTARG(TRGCENX,ITARG) - ZZWCEN(IROOM,IWALL,1)
          SVECT(2) = XXTARG(TRGCENY,ITARG) - ZZWCEN(IROOM,IWALL,2)
          SVECT(3) = XXTARG(TRGCENZ,ITARG) - ZZWCEN(IROOM,IWALL,3)
          COSANGT = 0.0D0
          S = DNRM2(3,SVECT,1)
          IF(S.NE.0.0D0)THEN
            COSANGT = -DDOT(3,SVECT,1,XXTARG(TRGNORMX,ITARG),1)/S
          ENDIF
          ZWALL = ZZWCEN(IROOM,IWALL,3)
          ZTARG = XXTARG(TRGCENZ,ITARG)
          ZLAY = ZZHLAY(IROOM,LOWER)
          TL = ZZTEMP(IROOM,LOWER)
          TU = ZZTEMP(IROOM,UPPER)

C*** COMPUTE PATH LENGH IN LOWER (ZL) AND UPPER (ZU) LAYER

          CALL GETYLYU(ZWALL,ZLAY,ZTARG,S,ZL,ZU)

C*** FIND FRACTIONS TRANSMITTED AND ABSORBED IN LOWER AND UPPER LAYER

          TAUL = EXP(-ABSL*ZL)
          ALPHAL = 1.0D0 - TAUL
          TAUU = EXP(-ABSU*ZU)
          ALPHAU = 1.0D0 - TAUU

          AWALL = ZZWAREA2(IROOM,IWALL)
          QWT = QOUT*TAUL*TAUU
          IF(IWALL.LE.5)THEN
            QGAS = TL**4*ALPHAL*TAUU + TU**4*ALPHAU
           ELSE
            QGAS = TU**4*ALPHAU*TAUL + TL**4*ALPHAL
          ENDIF
          QGT = SIGMA*QGAS
          IF(COSANGT.GE.0.0D0)THEN
            JJ = 1
           ELSE 
            JJ = 2
          ENDIF

C*** CALCULATE FLUX ON THE TARGET FRONT.  CALCULATE FLUX ON THE TARGET BACK ONLY IF THE 
C    REAR OF THE TARGET IS INTERIOR TO THE ROOM.

          IF(JJ.EQ.1.OR.IXTARG(TRGBACK,ITARG).EQ.INT)THEN
            QWTSUM(JJ) = QWTSUM(JJ) + QWT*AWALL
            QGASSUM(JJ) = QGASSUM(JJ) + QGT*AWALL
            AWALLSUM(JJ) = AWALLSUM(JJ) + AWALL
          ENDIF
  220   CONTINUE
        DO 225 I = 1, 2
          IF(AWALLSUM(I).EQ.0.0D0)AWALLSUM(I) = 1.0D0
          QTWFLUX(ITARG,I) = QWTSUM(I)/AWALLSUM(I)
          QTGFLUX(ITARG,I) = QGASSUM(I)/AWALLSUM(I)
  225   CONTINUE        

C*** IF THE TARGET REAR WAS EXTERIOR THEN CALCULATE THE FLUX ASSUMING AMBIENT OUTSIDE CONDITIONS

        IF(IXTARG(TRGBACK,ITARG).EQ.EXT.or.qtgflux(itarg,2).eq.0.0)THEN
          QTGFLUX(ITARG,2) = SIGMA*TAMB(IROOM)**4
        ENDIF
      ENDIF

C*** COMPUTE CONVECTIVE FLUX

C  ASSUME TARGET IS A 'FLOOR', 'CEILING' OR 'WALL' DEPENDING ON HOW MUCH THE TARGET IS TILTED.  

      ZZNORM = XXTARG(TRGNORMZ,ITARG)
      IF(ZZNORM.LE.1.0D0.AND.ZZNORM.GE.COS45)THEN
        IW = 2
        IWB = 1
       ELSEIF(ZZNORM.GE.-1.0D0.AND.ZZNORM.LE.-COS45)THEN
        IW = 1
        IWB = 2
       ELSE
        IW = 3
        IWB = 3
      ENDIF
      IRTARG = IXTARG(TRGROOM,ITARG)
      XTARG = XXTARG(TRGCENX,ITARG)
      YTARG = XXTARG(TRGCENY,ITARG)
      ZTARG = XXTARG(TRGCENZ,ITARG)
      CALL GETTGAS(IRTARG,XTARG,YTARG,ZTARG,TG)
      TGTARG(ITARG) = TG
      IF(IXTARG(TRGBACK,ITARG).EQ.INT)THEN
         TGB = TG
        ELSE
         TGB = TAMB(IROOM)
      ENDIF
      TTARGB = TTARG(2)
      DTTARG = 1.0D-7*TTARG(1)
      DTTARGB = 1.0D-7*TTARG(2)

      TEMIS = XXTARG(TRGEMIS,ITARG)

C*** CONVECTION FOR THE FRONT

      CALL CONVEC(IW,TG,TTARG(1),Q1)
      CALL CONVEC(IW,TG,TTARG(1)+DTTARG,Q2)
      QTCFLUX(ITARG,1) = Q1
      DQDTARG = (Q2-Q1)/DTTARG

      FLUX(1) = TEMIS*(QTFFLUX(ITARG,1) + QTWFLUX(ITARG,1) +
     .       QTGFLUX(ITARG,1)) + QTCFLUX(ITARG,1) - 
     .       TEMIS*SIGMA*TTARG(1)**4
      DFLUX(1) = -4.0D0*TEMIS*SIGMA*TTARG(1)**3 + DQDTARG
      
      ! This is for "gauge" heat flux output ... it assumes an ambient temperature target
      GTFLUX(ITARG,2) = QTFFLUX(ITARG,1)
      GTFLUX(ITARG,3) = QTWFLUX(ITARG,1)
      GTFLUX(ITARG,4) = QTGFLUX(ITARG,1)
      CALL CONVEC(IW,TG,TAMB(IROOM),Q1G)
      GTFLUX(ITARG,5) = Q1G
      GTFLUX(ITARG,1) = GTFLUX(ITARG,2) + GTFLUX(ITARG,3) + 
     *                  GTFLUX(ITARG,4) + GTFLUX(ITARG,5) -
     *                  SIGMA*TAMB(IROOM)**4

C*** CONVECTION FOR THE BACK

      CALL CONVEC(IWB,TGB,TTARGB,Q1B)
      CALL CONVEC(IWB,TGB,TTARGB+DTTARGB,Q2B)
      QTCFLUX(ITARG,2) = Q1B
      DQDTARGB = (Q2B-Q1B)/DTTARGB

      FLUX(2) = TEMIS*(QTFFLUX(ITARG,2) + QTWFLUX(ITARG,2) +
     .       QTGFLUX(ITARG,2)) + QTCFLUX(ITARG,2) - 
     .       TEMIS*SIGMA*TTARGB**4
      DFLUX(2) = -4.0D0*TEMIS*SIGMA*TTARGB**3 + DQDTARGB

      RETURN
      END
      subroutine gettgas(irtarg,xtarg,ytarg,ztarg,tg)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fltarget.fi"
      include "objects2.fi"

      logical first
      save first, pi, four

      data first/.true./

      if(first)then
         first = .false.
         xx1 = 1.0d0
         four = 4.0d0
         pi = four*atan(xx1)
      endif
      
      ! default is the appropriate layer temperature
      if (ztarg.ge.zzhlay(irtarg,lower)) then
        tg = zztemp(irtarg,upper)
      else
        tg = zztemp(irtarg,lower)
      end if
      
      ! if there is a fire in the room and the target is DIRECTLY above the fire, use plume temperature
      do i = 1,nfire
        if (ifroom(i).eq.irtarg) then
            if (xtarg.eq.xfire(i,1).and.ytarg.eq.xfire(i,2).and.
     *          ztarg.gt.xfire(i,3)) then
                qdot = fqf(i)
                xrad = radconsplit(i)
                dfire = dsqrt(farea(i)*four/pi)
                tu = zztemp(irtarg,upper)
                tl = zztemp(irtarg,lower)
                zfire = xfire(i,3)
                zlayer = zzhlay(irtarg,lower)
                z = ztarg
                call plumetemp (qdot, xrad, dfire, tu, tl, zfire, 
     *                          zlayer, z, tplume)
                tg = tplume
            end if
        end if
      end do 
      end subroutine gettgas 

      SUBROUTINE GETYLYU(YO,Y,YT,S,YL,YU)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GETYLYU
C
C     Source File: TARGFLUX.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: YO
C                Y
C                YT
C                S
C                YL
C                YU
C
C     Revision History:
C        Created:  5/5/1995 at 14:00 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"

      IF(YO.LE.Y)THEN
         IF(YT.LE.Y)THEN
            YL = 1.0D0
           ELSE
            YL = (Y-YO)/(YT-YO)
         ENDIF
        ELSE
         IF(YT.GE.Y)THEN
            YL = 0.0D0
           ELSE
            YL = (Y-YT)/(YO-YT)
         ENDIF
      ENDIF
      YL = YL*S
      YU = S - YL
      RETURN
      END

      SUBROUTINE UPDTECT(
     I                   IMODE,TCUR,DSTEP,NDTECT,
     I                   ZZHLAY,ZZTEMP,
     .                   XDTECT,IXDTECT,
     O                   IQUENCH,IDSET,IFDTECT,TDTECT
     .                   )
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDTECT
C
C     Source File: UPDTECT.SOR
C
C     Description:  This routine updates the temperature of each detector 
C           link.  It also determine whether the detector has activated 
C           in the time interval (tcur,tcur+dstep).  If this has occured
C           then a quenching algorithm will be invoked if the appropriate
C           option has been set.
C
C     Arguments: TCUR    current time
C                DSTEP   time step size (to next time)
C                NDTECT  number of detectors
C                XDTECT  2-d array containing floating point detector 
C                        data structures 
C                IXDTECT 2-d array containing integer detectir data 
C                        structures
C                IQUENCH if the j=IQUENCH(i) is non-zero then the 
C                        j'th sprinkler in the i'th room is quenching 
C                        the fire
C                IDSET   room where activated detector resides
C
C     Revision History:
C        Created: 9/3/93 by GPF:
C        MODIFIED: 4/24/95 by GPF:
C                removed references to ISET to eliminate FLINT complaints
C        MODIFIED: 2/29/96 by GPF:
C                fixed reference to TLINKO and VELO .  Added minimum ceiling jet
C                velocity and minimum temperature to detector link temperature
C                calculation.  0.1 m/s minimum c.j. velocity was taken from DETACT
C        MODIFIED: 10/9/97 by GPF:
C                return sensor number instead of "1" in IFDTECT 
C                if a sensor has activated
C                
C
      include "precis.fi"
      include "dsize.fi"
      include "cparams.fi"

      DIMENSION ZZTEMP(NR,2), ZZHLAY(NR,2)

      DIMENSION XDTECT(MXDTECT,*), IXDTECT(MXDTECT,*), IQUENCH(*)
C
      IDSET = 0
      IFDTECT = 0
      TDTECT = TCUR+2*DSTEP
      CJETMIN = 0.10D0
      DO 10 I = 1, NDTECT
        IROOM = IXDTECT(I,DROOM)
        TLINKO = XDTECT(I,DTEMP)

        ZDETECT = XDTECT(I,DZLOC)
        IF(ZDETECT.GT.ZZHLAY(IROOM,LOWER))THEN
          TLAY = ZZTEMP(IROOM,UPPER)
         ELSE
          TLAY = ZZTEMP(IROOM,LOWER)
        ENDIF

        TJET = MAX(XDTECT(I,DTJET),TLAY)
        TJETO = MAX(XDTECT(I,DTJETO),TLAY)
        VEL = MAX(XDTECT(I,DVEL),CJETMIN)
        VELO = MAX(XDTECT(I,DVELO),CJETMIN)

        RTI = XDTECT(I,DRTI)
        TRIG = XDTECT(I,DTRIG)
        IF(IXDTECT(I,DTYPE).EQ.SMOKED)THEN
          TLINK = TJET
         ELSEIF(IXDTECT(I,DTYPE).EQ.HEATD)THEN
          BN = SQRT(VELO)/RTI
          AN = BN*TJETO
          BNP1 = SQRT(VEL)/RTI
          ANP1 = BNP1*TJET
          DENOM = 1.0D0 + DSTEP*BNP1*.5D0
          FACT1 = (1.0D0 - DSTEP*BN*.50D0)/DENOM
          FACT2 = DSTEP/DENOM
          TLINK = FACT1*TLINKO + FACT2*(AN+ANP1)*0.5D0
         ELSE

C*** WHEN SOOT IS CALCULATED THEN SET TLINK TO SOOT CONCENTRATION.
C    SET IT TO ZERO FOR NOW.

           TLINK = 0.0D0
        ENDIF
        IF (IMODE.GT.0) THEN
          XDTECT(I,DTEMPO) = TLINKO
          XDTECT(I,DTEMP) = TLINK
        ENDIF
C*** DETERMINE IF DETECTOR HAS ACTIVATED IN THIS TIME INTERVAL (and not earlier)

        IF(TLINKO.LT.TRIG.AND.TRIG.LE.TLINK.AND.
     .                        IXDTECT(I,DACT).EQ.0)THEN
          DELTA = (TRIG-TLINKO)/(TLINK-TLINKO)
          TMP = TCUR+DSTEP*DELTA
          TDTECT = MIN(TMP,TDTECT)
          IFDTECT = I
          IF (IMODE.GT.0) THEN
            XDTECT(I,DTACT)= TCUR+DSTEP*DELTA
            IXDTECT(I,DACT) = 1

C*** DETERMINE IF THIS IS THE FIRST DETECTOR TO HAVE ACTIVATED IN THIS ROOM

            IDOLD = IQUENCH(IROOM)
            IQU = 0
            IF(IDOLD.EQ.0)THEN
              IQU = I
             ELSE
              IF(XDTECT(I,DTACT).LT.XDTECT(IDOLD,DTACT))THEN

C*** THIS CAN ONLY HAPPEN IF TWO DETECTORS HAVE ACTIVATED IN THE SAME
C    ROOM IN THE SAME (POSSIBLY VERY SHORT) TIME INTERVAL

                IQU = I
              ENDIF
            ENDIF
C*** IF THIS DETECTOR HAS ACTIVATED BEFORE ALL OTHERS IN THIS ROOM
C    AND THE QUENCHING FLAG WAS TURNED ON THEN LET THE SPRINKLER
C    QUENCH THE FIRE

            IF(IQU.NE.0.AND.IXDTECT(I,DQUENCH).EQ.1)THEN
              IQUENCH(IROOM)=IQU
              IDSET = IROOM
            ENDIF
          ENDIF
        ENDIF
        XDTECT(I,DTJETO) = TJET
        XDTECT(I,DVELO) = VEL
   10 CONTINUE
      RETURN
      END

      integer function rev_target
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_target = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_target
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
      DIMENSION FLWVF(NR,NS+2,2)
      DIMENSION XMVENT(2), ILAY(2), TMVENT(2)
      INTEGER TOPRM, BOTRM
      LOGICAL VFLOWFLG
	double precision area, tsec
      DATA TOPRM /1/, BOTRM /2/

C     THE SELECTION RULES ARE NOW IMPLEMENTED HERE.  THE CROSOVER
C     IS THE RELATIVE FRACTION OF THE VOLUME CLOESEST TO THE HOLE
C     FROM WHICH THE MASS WILL COME

      VFLOWFLG = .FALSE.
      IF (OPTION(FVFLOW).NE.ON) RETURN
      IF (NVVENT.EQ.0) RETURN
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

            IF (IFLOW.EQ.1) THEN
               IFRM = IBOT
               ITO = ITOP
            ELSE
               IFRM = ITOP
               ITO = IBOT
            END IF
C
C     DETERMINE MASS AND ENTHALPY FRACTIONS - FIRST "FROM," THEN "TO"
C
            IF (IFRM.LE.NM1) THEN
               IF (IFRM.EQ.IBOT) THEN
                    VOLUP = VOLFRU(IFRM) * OCO
                    VOLUP = MIN(VOLUP, XX1)
                    VOLLOW = MAX(XX1 - VOLUP, XX0)
               ELSE
                    VOLLOW = VOLFRL(IFRM) * OCO
                    VOLLOW = MIN(VOLLOW,XX1)
                    VOLUP = MAX(XX1 - VOLLOW, XX0)
               END IF
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
            IF (XXTMP.LE.ZZTEMP(ITO,LOWER)) FL = XX1
            FU = XX1 - FL
            FUMU = FU * XMVENT(IFLOW)
            FUML = FL * XMVENT(IFLOW)
            FUQU = FU * XXTQ
            FUQL = FL * XXTQ

C
C     DEPOSIT MASS AND ENTHALPY INTO "TO" ROOM VARIBLES (NOT OUTSIDE)
C
          IF (ITO.LE.NM1) THEN
             FLWVF(ITO,M,UPPER) = FLWVF(ITO,M,UPPER) + FUMU
             FLWVF(ITO,M,LOWER) = FLWVF(ITO,M,LOWER) + FUML
             FLWVF(ITO,Q,UPPER) = FLWVF(ITO,Q,UPPER) + FUQU
             FLWVF(ITO,Q,LOWER) = FLWVF(ITO,Q,LOWER) + FUQL
          END IF
          VMFLO(IFRM,ITO,UPPER) = FUMU + VMFLO(IFRM,ITO,UPPER)
          VMFLO(IFRM,ITO,LOWER) = FUML + VMFLO(IFRM,ITO,LOWER)
C
C     EXTRACT MASS AND ENTHALPY FROM "FROM" ROOM (NOT FROM OUTSIDE)
C
          IF (IFRM.LE.NM1) THEN
             FLWVF(IFRM,M,UPPER) = FLWVF(IFRM,M,UPPER) - XXMU
             FLWVF(IFRM,M,LOWER) = FLWVF(IFRM,M,LOWER) - XXML
             FLWVF(IFRM,Q,UPPER) = FLWVF(IFRM,Q,UPPER) - XXQU
             FLWVF(IFRM,Q,LOWER) = FLWVF(IFRM,Q,LOWER) - XXQL
          END IF
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
                IF (ITO.LE.NM1) THEN
                   PMTOUP = (XXMIXU + XXMIXL) * FU
                   PMTOLP = (XXMIXU + XXMIXL) * FL
                   FLWVF(ITO,INDEX,UPPER) = FLWVF(ITO,INDEX,UPPER) +
     .                                      PMTOUP
                   FLWVF(ITO,INDEX,LOWER) = FLWVF(ITO,INDEX,LOWER) +
     .                                      PMTOLP
                END IF
C
C     EXTRACT MASS AND ENTHALPY FROM "FROM" ROOM (NOT FROM THE OUTSIDE)
C
                IF (IFRM.LE.NM1) THEN
                   FLWVF(IFRM,INDEX,UPPER) = FLWVF(IFRM,INDEX,UPPER) -
     .                                       XXMIXU
                   FLWVF(IFRM,INDEX,LOWER) = FLWVF(IFRM,INDEX,LOWER) -
     .                                       XXMIXL
                END IF
             END IF
   50     CONTINUE
   30    CONTINUE
   60 CONTINUE
C
      RETURN
      END

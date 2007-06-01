      SUBROUTINE HCL (FLWHCL, FLXHCL,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCL
C
C     Source File: HCL.SOR
C
C     Functional Class:  Physical interface routine
C
C     Description:       Physical Interface routine to do HCl deposition
C                        on wall surfaces.
C
C     Arguments: FLWHCL  Mass and energy flows into layers due to HCl
C                        deposition.  Standard source routine data 
C                        structure.
C                FLXHCL  HCl surface concentration flux.
C                IERROR  Returns error codes
C
C     Commons:
C        USED:  Activs   Ar       Br       Dr       Hr       Hwj     
C               Hwjdot   Mass     N        Nm1      Qscnv    Switch  
C               Twj      Zzhlay   Zzrho    Zztemp   Zzvol   
C
C     Revision History:
C        Created:  9/5/1995 at 9:32 by PAR
C        Modified: 9/5/1995 at 9:35 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "opt.fi"
      DIMENSION FLWHCL(NR,NS+2,2), FLXHCL(NR,4)

C     INITIALIZE SUMMATIONS AND LOCAL DATA

      X0 = 0.0D0

C*** only zero out mass (lsp=1) and hcl (lsp=2+6) entries of flwhcl

      DO 10 IROOM = 1, N
      DO 11 J = 1, NS+2
        FLWHCL(IROOM,J,UPPER) = X0
   11   FLWHCL(IROOM,J,LOWER) = X0
        FLXHCL(IROOM,1) = X0
        FLXHCL(IROOM,2) = X0
        FLXHCL(IROOM,3) = X0
        FLXHCL(IROOM,4) = X0
   10 CONTINUE
      IF (OPTION(FHCL).EQ.OFF) RETURN

C     CALCULATE THE HCL "ADDED" TO THE LAYERS FROM EACH SURFACE

      IF (ACTIVS(6)) THEN
        DO 30 IROOM = 1, NM1
          DO 20 IWALL = 1, 4
            IF (SWITCH(IWALL,IROOM)) THEN
              IF (IWALL.EQ.1) THEN
                ARW = AR(IROOM)
                LAYER = UPPER
              ELSE IF (IWALL.EQ.2) THEN
                ARW = AR(IROOM)
                LAYER = LOWER
              ELSE IF (IWALL.EQ.3) THEN
                ARW = (BR(IROOM)+DR(IROOM)) * ZZHLAY(IROOM,UPPER) * 
     +              2.0D0
                LAYER = UPPER
              ELSE IF (IWALL.EQ.4) THEN
                ARW = (BR(IROOM)+DR(IROOM)) * (HR(IROOM)-
     +              ZZHLAY(IROOM,UPPER)) * 2.0D0
                ARW = MAX(X0,ARW)
                LAYER = LOWER
              END IF
C              Hclg = Mass(Layer,iroom,6) / Zzvol(iroom,Layer)
C              H2o = Mass(Layer,iroom,8) / Zzvol(iroom,Layer)

C*** use environment variables

              HCLG = ZZCSPEC(IROOM,LAYER,6)
              H2O = ZZCSPEC(IROOM,LAYER,8)
              RHO = ZZRHO(IROOM,LAYER)
              TG = ZZTEMP(IROOM,LAYER)
              HCLW = ZZWSPEC(IROOM,IWALL)
              FLUX = QSCNV(IWALL,IROOM)
              TW = TWJ(1,IROOM,IWALL)
              CALL HCLTRAN(IROOM,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,FLUX,TW,
     +            HWDOT,HNET,IERROR)
              IF (IERROR.NE.0) RETURN

C             SUM UP THE FLOWS AND FLUXES FOR THE SOURCE ROUTINE

              FLWHCL(IROOM,1,LAYER) = FLWHCL(IROOM,1,LAYER) + HNET
              FLWHCL(IROOM,2+6,LAYER) = FLWHCL(IROOM,2+6,LAYER) + HNET
              FLXHCL(IROOM,IWALL) = HWDOT

            END IF
   20     CONTINUE
   30   CONTINUE
      END IF
      RETURN
      END

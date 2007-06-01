      SUBROUTINE CJET(FLWCJT,FLXCJT)
C
C     Routine:     CJET
C
C     Description:  Interface between RESID and CEILHT.  Loops over
C                 rooms setting up varibles to pass.  Calls CEILHT
C                 only when fires are in a room otherwise sets zeros
C                 for FLXCJT.  Then uses FLXCJT to figure FLWCJT.
C
C     Arguments: FLWCJT  Net enthalphy into each layer
C                FLXCJT  Net enthalphy flux onto surface
C
C     Revision History:
C     PAR 11/91    Created.
C     gpf 2/5/93   calculated partial derivatives needed for
C                  reduced Jacobian calculation
C     gpf 2/7/93   The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.
C     gpf 10/14/93 Added detector/sprinkler option.  changed code so
C                  jacobian elements are not computed if wall is not active.
C    
C     GPF 4/24/95    removed references to qwall, since it was not
C                    being used
C     GPF 2/5/96   removed reduced jacobian option added on 2/5/93.
C     GPF 7/22/96  handle ceiling jets in active halls.  a hall is active
C                  if the ceiling jet has started but not reached the end of 
C                  the hall.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "opt.fi"
      include "wdervs.fi"
C
      DIMENSION FLWCJT(NR,2), FLXCJT(NR,NWAL)
      INTEGER CJETOPT
      DIMENSION DUMMY(100)
C
      XX0 = 0.0D0
      DO 10 I = 1, NM1
        FLXCJT(I,1) = XX0
        FLXCJT(I,2) = XX0
        FLXCJT(I,3) = XX0
        FLXCJT(I,4) = XX0
        FLWCJT(I,1) = XX0
        FLWCJT(I,2) = XX0
   10 CONTINUE
      DO 15 ID = 1, NDTECT
         IROOM = IXDTECT(ID,DROOM)
         XDTECT(ID,DVEL) = 0.0D0
         ZLOC = XDTECT(ID,DZLOC)
         IF(ZLOC.GT.ZZHLAY(IROOM,LOWER))THEN
           XDTECT(ID,DTJET) = ZZTEMP(IROOM,UPPER)
          ELSE
           XDTECT(ID,DTJET) = ZZTEMP(IROOM,LOWER)
         ENDIF
   15 CONTINUE
      IF (OPTION(FCJET).EQ.OFF) RETURN
      CJETOPT = OPTION(FCJET)
C
      DO 30 I = 1, NM1
        NRMFIRE = IFRPNT(I,1)
        ID = IDTPNT(I,2)
        ND = IDTPNT(I,1)

C*** handle ceiling jets that are not in active halls

        IF (CJETON(NWAL+1).AND.NRMFIRE.GT.0.AND.
     .                IZHALL(I,IHMODE).NE.IHDURING) THEN
          DO 20 IFIRE = 1, NRMFIRE
            IFPNT = IFRPNT(I,2) + IFIRE - 1
            IF (SWITCH(1,I)) THEN
              TCEIL = TWJ(1,I,1)
            ELSE
              TCEIL = ZZTEMP(I,UPPER)
            END IF
            IF (SWITCH(3,I)) THEN
              TUWALL = TWJ(1,I,3)
            ELSE
              TUWALL = ZZTEMP(I,UPPER)
            END IF
            CALL CEILHT(XFIRE(IFPNT,4),XFIRE(IFPNT,7),TCEIL,
     +          ZZTEMP(I,LOWER),ZZTEMP(I,UPPER),TUWALL,bR(I),dR(I),
     +          HR(I),XFIRE(IFPNT,1),XFIRE(IFPNT,2),XFIRE(IFPNT,3),
     +          ZZHLAY(I,LOWER),ZZRHO(I,LOWER),ZZRHO(I,UPPER),CJETOPT,
     +          XDTECT(ID,DXLOC),XDTECT(ID,DYLOC),XDTECT(ID,DZLOC),
     +          ND,QCEIL,QFCLGA,QFWLA,QFWUA,
     +          XDTECT(ID,DTJET),XDTECT(ID,DVEL),FTMAX,FVMAX,FDMAX)
            FLXCJT(I,1) = FLXCJT(I,1) + QFCLGA
            FLXCJT(I,3) = FLXCJT(I,3) + QFWUA
            FLXCJT(I,4) = FLXCJT(I,4) + QFWLA
   20     CONTINUE
        ENDIF

C*** handle ceiling jets that are in active halls

        IF(IZHALL(I,IHMODE).EQ.IHDURING)CALL HALLHT(I,ID,ND)
               
        DO 50 IWALL = 1, 4
          IF(MOD(IWALL,2).EQ.1)THEN
            ILAY = UPPER
           ELSE
            ILAY = LOWER
          ENDIF
 
C     if (.not.(ceiling jet in fire room)) then flux to IWALL = 0.
 
          IF (.NOT.(SWITCH(IWALL,I).AND.CJETON(IWALL)
     .        .AND.NRMFIRE.GT.0)) THEN
             FLXCJT(I,IWALL) = XX0
          END IF
          FLWCJT(I,ILAY) = FLWCJT(I,ILAY) - 
     .                        ZZWAREA(I,IWALL)*FLXCJT(I,IWALL)
   50   CONTINUE
   30 CONTINUE
      RETURN
      END

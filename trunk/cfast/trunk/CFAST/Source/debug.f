      SUBROUTINE SETDBUG

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETDBUG
C
C     Source File: DEBUG.SOR
C
C     Functional Class:  DEBUG
C
C     Description:  
C
C     Arguments: IOFILI
C
C     Revision History:
C        Created:  12/4/1992 at 10:43 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cparams.fi"
      include "cfin.fi"
      include "opt.fi"
      include "cshell.fi"

      INTEGER FUNIT
      CHARACTER DBUGFIL*8

      CHARACTER KEYWORD*7, DBUGKY(MXDEBUG)*7, DUMMY*1, LY*2
      DATA DBUGKY/'MASSFLW',
     .            'HVACFLW',
     .            'HORZFLW',
     .            'VERTFLW',
     .            'MVNTFLW',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'ERRVCTR',
     .            'PRNTJAC',
     .            'PRNDPDT',
     .            'XXXXXXX'/

      CLOSE (IOFILI)
      OPEN (UNIT=IOFILI,FILE=LBUF)
      READ(IOFILI,*,END=100) DUMMY
   10 CONTINUE
      READ(IOFILI,*,END=100,err=10) KEYWORD, IROOM, ILAYER
      DO 20 I = 1, 15
         IF (KEYWORD.EQ.DBUGKY(I)) THEN
            IF (ILAYER.GT.0.AND.ILAYER.LE.2.AND.
     .                      IROOM.GT.0.AND.IROOM.LT.NR) THEN
               IOUNIT = FUNIT(70)
               IF (ILAYER.EQ.UPPER) THEN
                  LY = 'UP'
               ELSE
                  LY = 'LW'
               END IF
               IF (I.NE.2) THEN
                  WRITE(DBUGFIL,'(A4,A2,I2.2)')DBUGKY(I)
     .                                              ,LY,IROOM
               ELSE
                  WRITE(DBUGFIL,'(A4,I2.2,I2.2)')DBUGKY(I)
     .                                              ,ILAYER,IROOM
               END IF
               CALL OPNOTPT(DBUGFIL,IOUNIT)
               DBUGSW(I,ILAYER,IROOM) = IOUNIT
            END IF
         END IF
   20 CONTINUE
      IF (KEYWORD.EQ.DBUGKY(D_JAC)) THEN
         CALL OPNDBG(IROOM,ILAYER)
      ELSE IF (KEYWORD.EQ.DBUGKY(16)) THEN
         IOUNIT = FUNIT(70)
         CALL OPNOTPT('ERRVECTR',IOUNIT)
         DBUGSW(16,1,1) = IOUNIT
      ELSE
      END IF
      GO TO 10
  100 CONTINUE
      CLOSE (IOFILI)
      RETURN
      END

      SUBROUTINE OUTJAC (TSEC, WM, NEQS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTJAC
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: prints out the magnitude of the jacobian matrix
C
C     Arguments: TSEC
C                WM
C                NEQS
C
C     Revision History:
C        Created:  12/2/1992 at 11:30 by PAR
C        Modified 2/5/93 by GPF
C                        Implemented Jacobian printout for both
C                        full and reduced Jacobian options
C        Modified 2/5/96 by GPF   removed reduced jacobian option added on 2/5/93.
C        Modified 7/4/97 by WWJ add call to display routines
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wdervs.fi"
      include "opt.fi"

      DIMENSION WM(JACDIM,*), BUF(MAXEQ)
      CHARACTER*2 ENTRY(MAXEQ)
      LOGICAL FIRSTC
      INTEGER IOFFST(8)
      CHARACTER LBLS(8)*3
      CHARACTER HDER*256
      CHARACTER*2 DDIAG
      DATA FIRSTC/.TRUE./
      DATA LBLS/'P  ','PMV','TMV','TU ','VU ','TL ','WT ','PRD'/
      SAVE IOFFST, HDER, IOUNIT

C     NORMAL PROCESSING OF THE DEBUG OUTPUT

      IF (DBUGSW(D_JAC,D_PRN,1).LE.0) RETURN
      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IOFFST(1) = NOFP
         IOFFST(2) = NOFPMV
         IOFFST(3) = NOFTMV
         IOFFST(4) = NOFTU
         IOFFST(5) = NOFVU
         IOFFST(6) = NOFTL
         IOFFST(7) = NOFWT
         IOFFST(8) = NOFPRD
         HDER = '  '
         ITMP2 = 0
         DO 111 I = 1, 7
            IF (IOFFST(I)-IOFFST(I+1).EQ.0) GO TO 111
            ITMP2 = ITMP2 + 1
            ITMP = IOFFST(I)*2 + 7 + ITMP2*2
            HDER(ITMP:(ITMP+2)) = LBLS(I)
  111    CONTINUE
         IOUNIT = DBUGSW(D_JAC,D_PRN,1)
      END IF
      XX0 = 0.0D0
      WRITE(IOUNIT,*)' '
      WRITE(IOUNIT,*)'JACOBIAN',NUMJAC + TOTJAC,' TIME ',TSEC
      WRITE(IOUNIT,*)' '
      WRITE(IOUNIT,'(A256)')HDER
      IRDX = 1
      DO 10 I = 1, NEQS
         IF (I.GT.IOFFST(IRDX))THEN
            IRDX = IRDX + 1
  101       CONTINUE   
            IF (I.GT.IOFFST(IRDX)) THEN
               IRDX = IRDX + 1
               GO TO 101
            END IF
            ITCOL = NEQS + 8
            DO 103 K = 1, ITCOL + 2
               ENTRY(K) = '--'
  103       CONTINUE
            WRITE(IOUNIT,*)(ENTRY(K),K=1,ITCOL)
         END IF
         ENTRY(1) = LBLS(IRDX-1)(1:2)
         ICDX = 1
         ITCOL = 1
         WMII = WM(I,I)
         IF(WMII.NE.XX0)THEN
            IITMP = LOG(ABS(WMII))
           ELSE
            IITMP = 11
         ENDIF
         IF (IITMP.LT.VERYSM) THEN
            DDIAG = ' .'
         ELSE IF (IITMP.GT.VERYBG) THEN
            DDIAG = ' Û'
         ELSE
            WRITE(DDIAG,'(I2)')IITMP
         END IF

         DO 20 J = 1, NEQS
            ITCOL = ITCOL + 1
            IF (J.GT.IOFFST(ICDX)) THEN
               ICDX = ICDX + 1
  102          CONTINUE   
               IF (J.GT.IOFFST(ICDX)) THEN
                  ICDX = ICDX + 1
                  GO TO 102
               END IF
               ENTRY(ITCOL) = ' |'
               ITCOL = ITCOL + 1
            END IF
            WMIJ = BUF(J)
            IF (WMIJ.NE.XX0.AND.WMII.NE.XX0) THEN
               TMP1 = ABS(WMIJ/WMII)
               TMP = LOG(TMP1)
            ELSE IF (WMII.EQ.XX0) THEN
               TMP = 11
            ELSE
               TMP = -11
            ENDIF
            ITMP = INT(TMP + 0.5D0)

            IF (WMIJ.EQ.0.0D0) THEN
               ENTRY(ITCOL) = '  '
            ELSE IF (ITMP.LT.VERYSM) THEN
               ENTRY(ITCOL) = ' .'
            ELSE IF (ITMP.GT.VERYBG) THEN
               ENTRY(ITCOL) = ' Û'
            ELSE
               WRITE(ENTRY(ITCOL),'(I2)')ITMP
            END IF
   20    CONTINUE
         WRITE(IOUNIT,*)DDIAG,':',(ENTRY(K),K=1,ITCOL)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE OUTJCNT (T)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTJCNT
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: Print out numerical performance data; resid counts,
C                  jac counts, cpu times etc.
C
C     Arguments: T
C
C     Revision History:
C        Created:  12/2/1992 at 15:10 by PAR
C        Modified: 2/13/1993 by GPF
C                  Added Newton iteration counts and  overhead CPU times
C                  to output report
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
      include "wdervs.fi"

      LOGICAL FIRSTC
      DATA FIRSTC/.TRUE./
      SAVE IOUNIT

      IF (DBUGSW(D_JAC,D_CNT,1).LE.0) RETURN
      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IOUNIT = DBUGSW(D_JAC,D_CNT,1)
         WRITE(IOUNIT,1002)
 1002    FORMAT(15X,'STEPS',4X,'JACOBIANS',5X,'RESIDS',4X,
     .   'NEWT ITERS',9X,'CPU',14X,'OVER HEAD'/
     .   4X,'TIME',4X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM',
     .             2X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM',
     .             4X,'CUR',8X,'CUM',6X,'CUR',7X,'CUM')
      END IF
      TOTJAC = TOTJAC + NUMJAC
      TOTSTEP = TOTSTEP + NUMSTEP
      TOTRESD = TOTRESD + NUMRESD
      NUMITR = NUMRESD - NUMJAC*JACDIM
      TOTITR = TOTITR + NUMITR
      WRITE(IOUNIT,1001)T,NUMSTEP,TOTSTEP,NUMJAC,TOTJAC,
     .                  NUMRESD,TOTRESD,NUMITR,TOTITR,
     .                  PRTTIME,TOTTIME,OVTIME,TOVTIME
 1001 FORMAT(1X,1PE9.2,4(1X,I4,1X,I6),
     .       1X,1PE9.2,1X,1PE9.2,1X,1PE9.2,1X,1PE9.2)
      RETURN
      END

      SUBROUTINE OPNDBG (JACCNT, JACPRN)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPNDBG
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: opens a file on unit iounit
C
C     Arguments: JACCNT
C                JACPRN
C
C     Revision History:
C        Created:  12/2/1992 at 15:21 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"

      INTEGER FUNIT
      LOGICAL FIRSTC
      CHARACTER*6 CNTFIL, PRNFIL

      SAVE FIRSTC
      DATA FIRSTC/.TRUE./,CNTFIL/'JACCNT'/,PRNFIL/'JACPRN'/

      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IF (JACCNT.GT.0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(CNTFIL,IOUNIT)
            DBUGSW(D_JAC,D_CNT,1) = IOUNIT
         END IF
         IF (JACPRN.GT.0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(PRNFIL,IOUNIT)
            DBUGSW(D_JAC,D_PRN,1) = IOUNIT
         END IF
      END IF
      RETURN
      END

      BLOCKDATA INITDBUG

      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"

      DATA NUMJAC/0/, NUMSTEP/0/, NUMRESD/0/, NUMITR/0/
      DATA TOTJAC/0/, TOTSTEP/0/, TOTRESD/0/, TOTITR/0/
      END

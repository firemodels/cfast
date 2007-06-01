      SUBROUTINE DREADIN(OUTPUT,IOUNIT,IERR,IVERS0)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DREADIN
C
C     Source File: DREADIN.SOR
C
C     Functional Class:  
C
C     Description:  Read routine for compacted history files
C
C     Arguments: OUTPUT   Starting location for I/O
C                LENF     Length of floating point section in words
C                IOUNIT   Logical unit assigned for read
C                IERR     Status of read (zero if OK)
C                IVERS0   Version number of history file
C
C     Revision History:
C        Created:  9/14/1993 at 10:05 by RDP
C        Modified: 9/5/1995 at 9:26 by PAR:
C                  Added support for IERROR and returning error codes to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
 
      PARAMETER (MXDMP = 36000)
      LOGICAL CNVRT
      CHARACTER HEADER*6, RHEADER(2)*6
      INTEGER OUTPUT(*), INPUT(MXDMP), FLOP
      DATA RHEADER /'$$CFL$', '$$CFH$'/
C
      IERR = 0
      READ (IOUNIT,END=30,IOSTAT=IOS) HEADER, IVERS0
      IF (HEADER.EQ.RHEADER(1)) THEN
#ifdef pp_ibmpc
        CNVRT = .FALSE.
#else
        CNVRT = .TRUE.
#endif
      ELSE IF (HEADER.EQ.RHEADER(2)) THEN
#ifdef pp_ibmpc
        CNVRT = .TRUE.
#else
        CNVRT = .FALSE.
#endif
      ELSE
        IERR = 9999
        RETURN
      END IF
	IF (CNVRT) IVERS0 = FLOP(IVERS0)
      IF (CNVRT) THEN
        READ (IOUNIT,END=30,IOSTAT=IOS) INPUT(1), (INPUT(I),I = 2,
     +      FLOP(INPUT(1)))
        INPUT(1) = FLOP(INPUT(1))
      ELSE
        READ (IOUNIT,END=30,IOSTAT=IOS) INPUT(1), (INPUT(I),I = 2,
     +      INPUT(1))
      END IF
      IF (INPUT(1).GT.MXDMP) THEN
#ifndef pp_dll
         CALL XERROR('DREADIN - overwrite input buffer; fatal error',
     .               0,1,1)
#endif
         IERR = 7
         RETURN
      END IF
      IF (CNVRT) THEN
        DO 10 I = 2, INPUT(1)
          INPUT(I) = FLOP(INPUT(I))
   10   CONTINUE
      END IF
      CALL UNPACK(INPUT,OUTPUT)
#ifdef pp_double
      IF (CNVRT) THEN
        CALL LENOCO(((IVERS0-1800)/10),ITOT,IFLT,IINT)
        DO 20 I = 1, IFLT / 2
          ITEMP = OUTPUT(2*I-1)
          OUTPUT(2*I-1) = OUTPUT(2*I)
          OUTPUT(2*I) = ITEMP
   20   CONTINUE
      END IF
#endif
   30 IF (IOS.NE.0) THEN
        IERR = IOS
      ELSE
        IERR = 0
      END IF
      RETURN
      END
 
      SUBROUTINE UNPACK(INPUT,OUTPUT)
 
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UNPACK
C
C     Source File: DREADIN.SOR
C
C     Functional Class:  
C     
C     Description:  This routine is to uncrunch the packed binary history file.
C                   The length of the record is contained in the first word, 
C                   and does NOT include the first word itself.  
C                   See WRITEOT for the reference information.
C
C     Arguments: INPUT    Packed array
C                OUTPUT   Unpack array returned
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER OUTPUT(*), INPUT(*)
      INTEGER I, INTGR, CNTR, MRKR, INLEN, INIDX, OUTIDX
 
      MRKR = 106
      INIDX = 1
      OUTIDX = 0
      INLEN = INPUT(1)
   10 IF (.TRUE.) THEN
        INIDX = INIDX + 1
        IF (INIDX.GT.INLEN) GO TO 30
        INTGR = INPUT(INIDX)
        IF (INTGR.EQ.MRKR) THEN
          INIDX = INIDX + 1
          IF (INIDX.GT.INLEN) GO TO 30
          INTGR = INPUT(INIDX)
          IF (INTGR.EQ.MRKR) THEN
            OUTIDX = OUTIDX + 1
            OUTPUT(OUTIDX) = INTGR
          ELSE
            INIDX = INIDX + 1
            IF (INIDX.GT.INLEN) GO TO 30
            CNTR = INPUT(INIDX)
            DO 20, I = 1, CNTR
              OUTIDX = OUTIDX + 1
              OUTPUT(OUTIDX) = INTGR
   20       CONTINUE
          END IF
        ELSE
          OUTIDX = OUTIDX + 1
          OUTPUT(OUTIDX) = INTGR
        END IF
        GO TO 10
      END IF
   30 RETURN
      END

      INTEGER FUNCTION FLOP(INUM)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FLOP
C
C     Source File: FCONV.FOR
C
C     Functional Class:  UTILITY
C
C     Description:  Flip bytes in an integer word PC - IRIS or back
C
C     Arguments: INUM
C
C     Revision History:
C        Created:  8/6/1993 at 14:08 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER*1 B(4), C(4)
      INTEGER I, J
      EQUIVALENCE (B,I), (C,J)
      I = INUM
      C(1) = B(4)
      C(2) = B(3)
      C(3) = B(2)
      C(4) = B(1)
      FLOP = J
      RETURN
      END

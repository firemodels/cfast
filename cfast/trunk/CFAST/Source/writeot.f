      SUBROUTINE WRITEOT(INPUT,LEN,IOUNIT,IERR,IVERS0)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WRITEOT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  Write compacted history file
C
C     Arguments: INPUT
C                LEN
C                IOUNIT
C                IERR
C                IVERS0
C
C     Revision History:
C        Created:  PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
 
      PARAMETER (MXDMP = 36000)
      INTEGER INPUT(LEN), PARRAY(MXDMP)
      CHARACTER HEADER*6
      DATA HEADER /'$$CFL$'/
 
      IERR = 0
      CALL PACKOT(LEN,MXDMP,INPUT,PARRAY)
      WRITE (IOUNIT,IOSTAT=IOS) HEADER, IVERS0
      WRITE (IOUNIT,IOSTAT=IOS) PARRAY(1), (PARRAY(I),I = 2,PARRAY(1))

      IF (IOS.NE.0) THEN
        IERR = IOS
      ELSE
        IERR = 0
      END IF
      RETURN
      END

      SUBROUTINE PACKOT(ITIN,MXDMP,DOIT,RETBUF)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     PACKOT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  Pack the output array
C
C     Arguments: ITIN
C                MXDMP
C                DOIT
C                RETBUF
C
C     Revision History:
C        Created:  8/31/1993 at 16:51 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C     This is the pack routine.  It crunches the binary common block to reduce
C     The amount of storage required to hold a single history record.
C     The length of the record which is written is contained in the first word
C     of the record and does NOT include itself.
C     The original implementation of this work was published by 
C     Andrew Binstock in The C Gazette, December 1987.  It is one of a large
C     class of compression schemes.  This particular scheme is fast, and is
C     best at compressing strings of the same character.  Since much of
C     the history file in CFAST stays the same, 0 for example, this works
C     fairly well.
 
      INTEGER IC, ITIN, IDX, RIDX, MXDMP
      INTEGER DOIT(ITIN), RETBUF(MXDMP)
      INTEGER LC, COUNT, MRKR
      CHARACTER MSG*80
 
      IDX = 1
      RIDX = 1
      MRKR = 106
      IC = DOIT(IDX)
      IDX = IDX + 1
 
C     CHECKING TO MAKE SURE THE FIRST NUMBERS ARE NOT THE MARKER
 
   10 IF (IC.EQ.MRKR) THEN
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        IC = DOIT(IDX)
        IDX = IDX + 1
        GO TO 10
      END IF
 
      LC = IC
      COUNT = 1
 
C     MAIN LOOP
 
   20 IF (IDX.LE.ITIN) THEN
        IC = DOIT(IDX)
        IDX = IDX + 1
 
C     IF NEXT NUMBER = MARKER THEN STORE WHAT YOU HAVE
 
   30   IF (IC.EQ.MRKR) THEN
          IF (COUNT.GT.3) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
          ELSE
            IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            DO 40, I = 1, COUNT
              RIDX = RIDX + 1
              RETBUF(RIDX) = LC
   40       CONTINUE
          END IF
          COUNT = 0
          RIDX = RIDX + 1
          RETBUF(RIDX) = MRKR
          RIDX = RIDX + 1
          RETBUF(RIDX) = MRKR
          IF (IDX.GT.ITIN) GO TO 60
          IC = DOIT(IDX)
          IDX = IDX + 1
          LC = IC
          GO TO 30
        END IF
        IF (IC.EQ.LC) THEN
          COUNT = COUNT + 1
          IF (COUNT.EQ.(2**30)) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
            COUNT = 0
          END IF
        ELSE
          IF (COUNT.GT.3) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
            LC = IC
            COUNT = 1
          ELSE
            IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            DO 50, I = 1, COUNT
              RIDX = RIDX + 1
              RETBUF(RIDX) = LC
   50       CONTINUE
            LC = IC
            COUNT = 1
          END IF
        END IF
        GO TO 20
      END IF
   60 IF (COUNT.GT.3) THEN
        IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
        CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
        LC = IC
        COUNT = 1
      ELSE
        IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
        DO 70, I = 1, COUNT
          RIDX = RIDX + 1
          RETBUF(RIDX) = LC
   70   CONTINUE
      END IF
      RETBUF(1) = RIDX
      RETURN
      END

      SUBROUTINE OPUT(IC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPUT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IC
C                COUNT
C                ITIN
C                MXDMP
C                RIDX
C                RETBUF
C
C     Revision History:
C        Created:  8/31/1993 at 16:51 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER IC, COUNT, MRKR, ITIN, RIDX
      INTEGER RETBUF(ITIN)
      MRKR = 106
      RIDX = RIDX + 1
      RETBUF(RIDX) = MRKR
      RIDX = RIDX + 1
      RETBUF(RIDX) = IC
      RIDX = RIDX + 1
      RETBUF(RIDX) = COUNT
      RETURN
      END

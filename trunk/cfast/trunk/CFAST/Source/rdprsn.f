      SUBROUTINE RDPRSN (NREQ,NRET,PRSNID,HAZARD)
C
C   READ IN DATA FOR A SINGLE PERSON IN TENAB FORMAT
C
      include "cparams.fi"

      INTEGER PRSNID(*),HAZARD(*)
      CHARACTER*120 IN
      INTEGER COUNT,START,TYPE
      LOGICAL VALID,FIRST

  10  FIRST=.TRUE.
      IOLD=NRET
      CALL MESSNRF ('Enter person number ',20)
      CALL READASTU (IN,COUNT,START,128,VALID)
      CALL READCV1 (IN,COUNT,START,IX,XI,TYPE,VALID)
      IF (.NOT.VALID) THEN
         CALL CKDUP (PRSNID,HAZARD,NRET)
         RETURN
      END IF
      IF (TYPE.EQ.1) THEN
         IPERSN=ABS(IFIX(XI))
      ELSE IF (TYPE.EQ.2) THEN
         IPERSN=ABS(IX)
      ELSE IF (TYPE.EQ.3) THEN
         IPERSN=0
      END IF
      IF (IPERSN.EQ.0) THEN
         CALL CKDUP (PRSNID,HAZARD,NRET)
         RETURN
      END IF
  99  IF (.NOT.FIRST) THEN
 9992   CALL MESS ('Tenab Variable List ',20)
        CALL MESSNRF ('1. Fractional Effective Dose Due to Gases ',42)
        CALL MESS ('- Bukowski ',11)
        CALL MESSNRF ('2. Fractional Effective Dose Due to Gases ',42)
        CALL MESS ('- Purser ',9)
        CALL MESSNRF ('3. Fractional Effective Dose Due to CO2 ',40)
        CALL MESS ('- Purser ',9)
        CALL MESS ('4. Temperature - Deg C ',23)
        CALL MESSNRF ('5. Fractional Effective Dose Due to ',36)
        CALL MESS ('Convective Heat ',16)
        CALL MESS ('6. CT (G-MIN/M3) ',17)
        CALL MESS ('7. Flux (KW-MIN/M2) ',20)
        CALL MESS ('8. Derksen Curve ',17)
        CALL MESSNRF ('Select a measure(s) ',20)
        CALL READASTU (IN,COUNT,START,128,VALID)
        IF (.NOT.VALID) THEN
          CALL MESS ('Bad Input Line - Try Again ',27)
          GO TO 9992
        END IF
      END IF
      DO 4 I=1,NREQ-1
      CALL READCV1 (IN,COUNT,START,IX,XI,TYPE,VALID)
      IF (.NOT.VALID) GO TO 4
      NRET=NRET+1
      PRSNID(NRET)=IPERSN
      IF (TYPE.EQ.1) THEN
        HAZARD(NRET)=ABS(IFIX(XI))
      ELSE IF (TYPE.EQ.2) THEN
        HAZARD(NRET)=ABS(IX)
      ELSE
        NRET=NRET-1
      END IF
      IF (NRET+1.GT.NVM) RETURN
  4   CONTINUE
      IF (NRET-IOLD.NE.0) GO TO 10
      FIRST=.FALSE.
      GO TO 99
      END

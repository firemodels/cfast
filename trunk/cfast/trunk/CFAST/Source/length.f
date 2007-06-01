      INTEGER FUNCTION LENGTH(STRING)
      CHARACTER STRING*(*)
      IF (LEN(STRING).NE.0) THEN
        DO 10 I=LEN(STRING),1,-1
        IF (STRING(I:I).NE.' ') THEN
          LENGTH=I
          RETURN
        END IF
10      CONTINUE
      END IF
      LENGTH=0
      RETURN
      END

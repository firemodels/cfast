      SUBROUTINE GETELEV(YVBOT,YVTOP,YLAY,YELEV,NELEV)
      include "precis.fi"
      DIMENSION YELEV(*), YLAY(*)
      YMIN = MIN(YLAY(1),YLAY(2))
      YMAX = MAX(YLAY(1),YLAY(2))
      IF (YMAX.GE.YVTOP.AND.(YMIN.GE.YVTOP.OR.YMIN.LE.YVBOT)) THEN
        NELEV = 2
        YELEV(1) = YVBOT
        YELEV(2) = YVTOP
      ELSE IF (YMAX.LE.YVBOT) THEN
        NELEV = 2
        YELEV(1) = YVBOT
        YELEV(2) = YVTOP
      ELSE
        IF (YMAX.GE.YVTOP.AND.YMIN.GT.YVBOT) THEN
          NELEV = 3
          YELEV(1) = YVBOT
          YELEV(2) = YMIN
          YELEV(3) = YVTOP
        ELSE IF (YMIN.LE.YVBOT.AND.YMAX.LT.YVTOP) THEN
          NELEV = 3
          YELEV(1) = YVBOT
          YELEV(2) = YMAX
          YELEV(3) = YVTOP
        ELSE
          NELEV = 4
          YELEV(1) = YVBOT
          YELEV(2) = YMIN
          YELEV(3) = YMAX
          YELEV(4) = YVTOP
        END IF
      END IF
      RETURN
      END

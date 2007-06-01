#ifdef pp_double
      DOUBLE PRECISION FUNCTION RDPARFIG(X,Y,Z)
#else
      FUNCTION RDPARFIG(X,Y,Z)
#endif

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDPARFIG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  This routine calculates the configuration factor 
C          between two paralell plates a distance z a part.  Each 
C          plate has a dimension of x by y.  the units of x, y and z 
C          are un-important except that they must be consistent.
C
C     Arguments: X
C                Y
C                Z
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"

      SAVE IFIRST, PI, XX0, XX1, XXH
      DATA IFIRST /0/
      IF (IFIRST.EQ.0) THEN
        XX1 = 1.0D0
        XXH = 0.5D0
        XX0 = 0.0D0
        PI = 4.0D0 * ATAN(XX1)
        IFIRST = 1
      END IF
      RDPARFIG = XX0
      IF (Z.EQ.XX0.OR.X.EQ.XX0.OR.Y.EQ.XX0) RETURN
      XX = X / Z
      YY = Y / Z
      F1 = XXH*LOG((XX1+XX**2)*(XX1+YY**2)/(XX1+XX**2+YY**2))
      YSQ = SQRT(XX1+YY**2)
      F2 = XX * YSQ * ATAN(XX/YSQ)
      XSQ = SQRT(XX1+XX**2)
      F3 = YY * XSQ * ATAN(YY/XSQ)
      F4 = XX * ATAN(XX)
      F5 = YY * ATAN(YY)
      RDPARFIG = 2.0D0 * (F1+F2+F3-F4-F5) / (PI*XX*YY)
      RETURN
#ifdef pp_gui
        ENTRY INRDPAR
        IFIRST = 0
        RETURN
#endif
      END
#ifdef pp_double
      DOUBLE PRECISION FUNCTION RDPRPFIG(X,Y,Z)
#else
      FUNCTION RDPRPFIG(X,Y,Z)
#endif
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDPRPFIG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  this routine calculates the configuration
C          factor between two perpindular plates with a common edge.
C
C     Arguments: X
C                Y
C                Z
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
C    
      LOGICAL FIRST
      SAVE FIRST, PI
      DATA FIRST /.TRUE./
      XX1 = 1.0D0
      XX0 = 0.0D0
      IF (FIRST) THEN
        PI = 4.0D0 * ATAN(XX1)
        FIRST = .FALSE.
      END IF
      RDPRPFIG = XX0
      IF (Y.EQ.XX0.OR.X.EQ.XX0.OR.Z.EQ.XX0) RETURN
      H = X / Y
      W = Z / Y
      F1 = W * ATAN(XX1/W)
      F2 = H * ATAN(XX1/H)
C      
      HWSUM = H ** 2 + W ** 2
      HWNORM = SQRT(HWSUM)
      RHWNORM = 1.0D0/HWNORM
      F3 = HWNORM * ATAN(RHWNORM)
 
      WSUM1 = XX1 + W ** 2
      HSUM1 = XX1 + H ** 2
      HWSUM2 = XX1 + HWSUM
      F4A = WSUM1 * HSUM1 / HWSUM2
      F4B = (W**2*HWSUM2/WSUM1/HWSUM)
      F4C = (H**2*HWSUM2/HSUM1/HWSUM)
      F4 = 0.25D0*(LOG(F4A)+LOG(F4B)*W**2+LOG(F4C)*H**2) 
      RDPRPFIG = (F1+F2-F3+F4) / PI / W
      RETURN
#ifdef pp_gui
        ENTRY INRDPRP
        FIRST = .TRUE.
        RETURN
#endif
      END
      SUBROUTINE RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,
     +    ZFIRE,FIRANG)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDFANG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: MXFIRE
C                XROOM
C                YROOM
C                ZROOM
C                HLAY
C                NFIRE
C                XFIRE
C                YFIRE
C                ZFIRE
C                FIRANG
C
C     Revision History:
C                Modified by gpf 6/28/95:
C                   Changed constant in solid angle identity from 1 to 4 pi 
C                   since the underlying solid angle calculation is not 
C                   normalized to one any more.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
 
      DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
      DIMENSION FIRANG(MXFIRE,*)
      LOGICAL FIRST
      SAVE FIRST, FOURPI
      DATA FIRST/.TRUE./

      IF(FIRST)THEN
         FIRST = .FALSE.
         XX1 = 1.0D0
         PI = 4.0D0*ATAN(XX1)
         FOURPI = 4.0D0*PI
      ENDIF

      DO 10 I = 1, NFIRE
        ARG1 = -XFIRE(I)
        ARG2 = XROOM - XFIRE(I)
        ARG3 = -YFIRE(I)
        ARG4 = YROOM - YFIRE(I)
        F1 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZROOM-ZFIRE(I))
        FD = RDSANG(ARG1,ARG2,ARG3,ARG4,HLAY-ZFIRE(I))
        F4 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZFIRE(I))
        FIRANG(I,1) = F1
        FIRANG(I,4) = F4
        IF (ZFIRE(I).LT.HLAY) THEN
          FIRANG(I,2) = FD - F1
          FIRANG(I,3) = FOURPI - FD - F4
        ELSE
          FIRANG(I,2) = FOURPI - FD - F1
          FIRANG(I,3) = FD - F4
        END IF
   10 CONTINUE
      RETURN
#ifdef pp_gui
        ENTRY INRDFANG
        FIRST = .TRUE.
        RETURN
#endif
      END
#ifdef pp_double
      DOUBLE PRECISION FUNCTION RDSANG(X1,X2,Y1,Y2,R)
#else
      FUNCTION RDSANG(X1,X2,Y1,Y2,R)
#endif
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDSANG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X1
C                X2
C                Y1
C                Y2
C                R
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"

      F1 = SIGN(RDSANG1(ABS(X2),ABS(Y2),R),X2*Y2)
      F2 = SIGN(RDSANG1(ABS(X1),ABS(Y2),R),X1*Y2)
      F3 = SIGN(RDSANG1(ABS(X2),ABS(Y1),R),X2*Y1)
      F4 = SIGN(RDSANG1(ABS(X1),ABS(Y1),R),X1*Y1)
      RDSANG = F1 - F2 - F3 + F4
      RETURN
      END
#ifdef pp_double
      DOUBLE PRECISION FUNCTION RDSANG1(X,Y,R)
#else
      FUNCTION RDSANG1(X,Y,R)
#endif
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDSANG1
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X
C                Y
C                R
C
C     Revision History:
C           gpf 5/24/95  Eliminated a division by 4*pi.  This division was done
C                        elsewhere resulting in a double division by 4*pi.
C                        Now this routine computes a solid angle (maximum
C                        esult 4*pi) rather than a configuration factor
C                        (maximum result 1).
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
C
      LOGICAL FIRST
      SAVE FIRST, PI, PIO2
C     
      DATA FIRST /.TRUE./
C     
      XX0 = 0.0D0
      XX1 = 1.0D0
      IF (FIRST) THEN
        FIRST = .FALSE.
        PI = 4.0D0 * ATAN(XX1)
        PIO2 = PI / 2.0D0
      END IF
      IF (X.LE.XX0.OR.Y.LE.XX0) THEN
        RDSANG1 = XX0
      ELSE
        XR = X * X + R * R
        XYR = X * X + Y * Y + R * R
        XY = X * X + Y * Y
        YR = Y * Y + R * R
        F1 = MIN(XX1, Y * SQRT(XYR/XY/YR))
        F2 = MIN(XX1, X * SQRT(XYR/XY/XR))
        RDSANG1 = (ASIN(F1)+ASIN(F2)-PIO2)
      END IF
      RETURN
#ifdef pp_gui
        ENTRY INRDSANG1
        FIRST = .TRUE.
        RETURN
#endif
      END

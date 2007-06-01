      SUBROUTINE INITTARG (IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITTARG
C
C     Source File: INITTARG.SOR
C
C     Functional Class:  
C
C     Description:  Initialize target data structures
C
C     Arguments: IERROR  Returns error codes
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C       Modified:  8/15/1995 at 14:00 by PAR
C	             Changed intialization of wall targets so that the X,
C                  Y given in the keyword are using the same origin as 
C                  the general room geometry.
C        Modified: 9/5/1995 at 9:55 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"
      include "thermp.fi"
      include "cshell.fi"
      CHARACTER*133 MESSG
      INTEGER MAP6(6)
      DATA MAP6/1,3,3,3,3,2/

      IFAIL = 0
      DO 210 ITARG = 1, NTARG

C*** ROOM NUMBER MUST BE BETWEEN 1 AND NM1

        IROOM = IXTARG(TRGROOM,ITARG)
        IF(IROOM.LT.1.OR.IROOM.GT.NM1)THEN
		 write(logerr,5000) iroom
		 ierror = 213
		 return
        ENDIF
        IWALL = IXTARG(TRGWALL,ITARG)
        XLOC = XXTARG(TRGCENX,ITARG)
        YLOC = XXTARG(TRGCENY,ITARG)
        ZLOC = XXTARG(TRGCENZ,ITARG)
        XXNORM = XXTARG(TRGNORMX,ITARG)
        YYNORM = XXTARG(TRGNORMY,ITARG)
        ZZNORM = XXTARG(TRGNORMZ,ITARG)
        XSIZE = BR(IROOM)
        YSIZE = DR(IROOM)
        ZSIZE = HRP(IROOM)
        IF(IWALL.NE.0)THEN
          XXNORM = 0.0D0
          YYNORM = 0.0D0
          ZZNORM = 0.0D0
        ENDIF
        IF(IWALL.EQ.1)THEN
          ZZNORM = -1.0D0
          XX = XLOC
          YY = YLOC
          ZZ = ZSIZE
         ELSEIF(IWALL.EQ.2)THEN
          YYNORM = -1.0D0
C          XX = XSIZE - XLOC
          XX = XSIZE
          YY = YSIZE
          ZZ = YLOC
         ELSEIF(IWALL.EQ.3)THEN
          XXNORM = -1.0D0
          XX = XSIZE
          YY = XLOC
          ZZ = YLOC
         ELSEIF(IWALL.EQ.4)THEN
          YYNORM = 1.0D0
          XX = XLOC
          YY = 0.0D0
          ZZ = YLOC
         ELSEIF(IWALL.EQ.5)THEN
          XXNORM = 1.0D0
          XX = 0.0D0
C          YY = YSIZE - XLOC
          YY = YSIZE
          ZZ = YLOC
         ELSEIF(IWALL.EQ.6)THEN
          ZZNORM = 1.0D0
          XX = XLOC
C          YY = YSIZE - YLOC
          YY = YSIZE
          ZZ = 0.0D0
        ENDIF
        IF(IWALL.NE.0)THEN
          XXTARG(TRGCENX,ITARG) = XX
          XXTARG(TRGCENY,ITARG) = YY
          XXTARG(TRGCENZ,ITARG) = ZZ
          XXTARG(TRGNORMX,ITARG) = XXNORM
          XXTARG(TRGNORMY,ITARG) = YYNORM
          XXTARG(TRGNORMZ,ITARG) = ZZNORM
          XLOC = XX
          YLOC = YY
          ZLOC = ZZ
          IWALL2 = MAP6(IWALL)
          IF(SWITCH(IWALL2,IROOM))THEN
            CXTARG(ITARG) = CNAME(IWALL2,IROOM)
           ELSE
            CXTARG(ITARG) = ' '
          ENDIF
        ENDIF

C***    CENTER COORDINATES NEED TO BE WITHIN ROOM

        IF(XLOC.LT.0.0D0.OR.XLOC.GT.XSIZE.OR.
     .     YLOC.LT.0.0D0.OR.YLOC.GT.YSIZE.OR.
     .     ZLOC.LT.0.0D0.OR.ZLOC.GT.ZSIZE)THEN
		  write(logerr,5001) iroom,xloc,yloc,zloc
		  ierror = 214
		  return
        ENDIF
  210 CONTINUE

C*** put a target in the center of the floor of each room

      DO 216 IROOM = 1, NM1
         NTARG = NTARG + 1
         IXTARG(TRGROOM,NTARG) = IROOM
         IXTARG(TRGMETH,NTARG) = STEADY
         IXTARG(TRGBACK,ITARG) = EXT
         XX = BR(IROOM)*0.50D0
         YY = DR(IROOM)*0.50D0
         ZZ = 0.D0
         XXTARG(TRGCENX,NTARG) = XX
         XXTARG(TRGCENY,NTARG) = YY
         XXTARG(TRGCENZ,NTARG) = ZZ
         XXTARG(TRGNORMX,NTARG) = 0.0D0
         XXTARG(TRGNORMY,NTARG) = 0.0D0
         XXTARG(TRGNORMZ,NTARG) = 1.0D0
         IF(SWITCH(2,IROOM))THEN
           CXTARG(NTARG) = CNAME(2,IROOM)
          ELSE
           CXTARG(NTARG) = ' '
         ENDIF
  216 CONTINUE
      
      RETURN
 5000	format("Target assigned to non-existent compartment",i3)
 5001 format("Target located outside of compartment",i3,1x,3f10.3)
      END

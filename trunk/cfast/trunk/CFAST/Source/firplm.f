      SUBROUTINE FIRPLM(QJL,ZZ,XEMP,XEMS,XEME,XFX,XFY)
C*RB
C     Routine:   FIRPLM
C     Function:  Calculates plume entrainment for a fire from 
C                McCaffrey's correlation
C     Inputs:    QJL    fire size (W)
C                ZZ      plume height (m)
C                XEMP  mass loss rate of the fire (kg/s)
C                XFX   position of the fire in x direction (m)
C                XFY   position of the fire in y direction (m)
C     Outputs:   XEMS  total mass transfer rate at height z (kg/s)
C                XEME  net entrainment rate at height z (kg/s)
C     Algorithm: "Momentum Implications for Buoyant Diffusion Flames"
C                 Combustion and Flame 52, 149 (1983)
C     Revision History:
C     WWJ   3/9/87   modified to allow zero heat release rate
C     GPF   7/24/90  modified coefficients so that correlation is
C                    continuous.
C     RDP   10/15/91 modified so fire position in x&y determines
C                    entrainment type.  x=0 or y=0 means center wall,
C                    x=0 and y=0 means corner.
C*RE
      include "precis.fi"
      LOGICAL FIRST
      SAVE FIRST, A1, A2, A3, T1, T2
      DATA FIRST /.TRUE./
C
C*** DEFINE ASSIGNMENT STATEMENT SUBROUTINES TO COMPUTE THREE PARTS OF CORRELATION
C
      FM1(ZQ) = ZQ ** .566D0
      FM2(ZQ) = ZQ ** .909D0
      FM3(ZQ) = ZQ ** 1.895D0
C
C*** FIRST TIME IN FIRPLM CALCULATE COEFF'S TO INSURE THAT MCCAFFREY 
C    CORRELATION IS CONTINUOUS.  
C    THAT IS, FOR A1 = .011, COMPUTE A2, A3 SUCH THAT
C  
C     A1*ZQ**.566 = A2*ZQ**.909  FOR ZQ = .08
C     A2*ZQ**.909 = A3*ZQ**1.895 FOR ZQ = .2
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        T1 = .08D0
        T2 = .20D0
        A1 = .011D0
        A2 = A1 * FM1(T1) / FM2(T1)
        A3 = A2 * FM2(T2) / FM3(T2)
      END IF
      X0 = 0.0D0
C
C     DETERMINE WHICH ENTRAINMENT TO USE BY FIRE POSITION.  IF WE'RE ON
C     THE WALL OR IN THE CORNER, ENTRAINMENT IS MODIFIED.
C
      XF = 1.0D0
      IF (XFX.EQ.X0.OR.XFY.EQ.X0) XF = 2.0D0
      IF (XFX.EQ.X0.AND.XFY.EQ.X0) XF = 4.0D0
      QJ = 0.001D0 * QJL
      IF (ZZ.GT.0.D0.AND.QJ.GT.0.0D0) THEN
        ZDQ = ZZ / (XF*QJ) ** 0.4D0
        IF (ZDQ.GT.T2) THEN
          XEMS = A3 * FM3(ZDQ) * QJ
        ELSE IF (ZDQ.GT.T1) THEN
          XEMS = A2 * FM2(ZDQ) * QJ
        ELSE
          XEMS = A1 * FM1(ZDQ) * QJ
        END IF
        XEMS = MAX(XEMP,XEMS/XF)
        XEME = MAX(XEMS-XEMP,X0)
      ELSE
        XEMS = XEMP
        XEME = 0.0D0
      END IF
      RETURN
      END

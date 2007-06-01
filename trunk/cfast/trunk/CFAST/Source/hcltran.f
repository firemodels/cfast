      SUBROUTINE HCLTRAN(ICOMP,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,FLUX,TW,
     +    HWDOT,HNET,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCLTRAN
C
C     Source File: HCLTRAN.SOR
C
C     Functional Class:  
C
C     Description: routine to calculate the hydrogen chloride balance 
C                  in the gas and on the wall surface.
C
C     Arguments: ICOMP   Compartment number (input)
C                IWALL   Wall surface number (input)
C                ARW     Area of the wall surface (m^2) (input)
C                HCLG    Current HCL gas concentration (kg/m^3) (input)
C                H2O     Current H2O gas concentration (kg/m^3) (input)
C                RHO     Current gas density (kg/m^3) (input)
C                TG      Gas layer temperature (K) (input)
C                HCLW    Current HCL wall density (kg/m^2) (input)
C                FLUX    Current convective heat flux on wall (W/m^2) (input)
C                TW      Corresponding wall temperature (K) (input)
C                HWDOT   Time derivative of the HCl wall concentration (output)
C                HNET    Time derivative of the HCL gas concentration (output)
C                IERROR  Returns error codes (output)
C
C     Commons:
C        USED:  Cp       Hclbf   
C
C     Revision History:
C        Created:  10/29/1989 at 9:36 by WWJ:
C        Modified: 10/29/1989 at 9:46 by WWJ:
C                  fix the coefficient for deposition, 
C                  optimize the numerics
C        Modified: 2/27/1990 at 9:46 by WWJ:
C                  fix the constants, and add wall absorption 
C                  coefficents to the thermal database
C        Modified: 9/5/1995 at 9:46 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"

      XX0 = 0.0D0
      HWDOT = XX0
      HNET = XX0
      IF ((HCLG.EQ.0.).AND.(HCLW.EQ.0.)) RETURN
C
C     NOTE THAT WE CALCULATE DENSITY ON THE FLY, SINCE PPMDV IS NOT UPDATED
C     OFTEN ENOUGH
C
      XHCLF = HCLG * TG * 2.25D-3
      HCLP = XHCLF * 1.0D6
      TWC = TW - 273.0D0
C
C     SPECIFIC VALUES FOR PAINTED GYPSUM - B1 AND B2 ARE FOR GAS PHASE
C     REACTIONS, AND B3 AND B4 ARE FOR THE WALL ITSELF
C
      B1 = HCLBF(1,IWALL,ICOMP)
      B2 = HCLBF(2,IWALL,ICOMP)
      B3 = HCLBF(3,IWALL,ICOMP)
      B4 = HCLBF(4,IWALL,ICOMP)
      B5 = HCLBF(5,IWALL,ICOMP)
      B6 = HCLBF(6,IWALL,ICOMP)
      B7 = HCLBF(7,IWALL,ICOMP)

      IF (B1.LE.0) RETURN

C     CALCULATE HCL GAS-SURFACE PARTITION COEFFICIENT
C     H2OS IS THE SATURATION CONCENTRATION OF WATER.

      IF (TWC.LE.40.D0) THEN
        IF (HCLP.GT.10.D0) THEN
          H2OS = (1.8204D0-0.18890D0*LOG(HCLP)+0.06466D0*TWC+1.650D-3*
     +        TWC**2+7.408D-5*TWC**3) / TW
        ELSE
          XTEMP = 17.64262D0 - 5164.1D0 / TW
          EXPTW = EXP(XTEMP)
          BCOEF = (7.696D-5+3.5920D-6*TWC+9.166D-8*TWC**2+4.116D-9*TWC
     +        **3) / TW - 1.D-7 * EXPTW
          H2OS = 0.018D0 * EXPTW + 1.8D4 * BCOEF * HCLP
        END IF
      ELSE IF ((TWC.GT.40.0D0).AND.(TWC.LE.60.0D0)) THEN
        H2OS = (7.044D0-2.2416D3*XHCLF-3.874D-3*TWC**2+2.328D-4*TWC**3
     +      +2.376D6*XHCLF**2-5.527D8*XHCLF**3+4.918D10*XHCLF**4-
     +      1.359D12*XHCLF**5-1.4033D2*TWC*XHCLF+2.431D4*TWC*XHCLF**2-
     +      1.6023D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.60.0D0).AND.(TWC.LE.80.0D0)) THEN
        H2OS = (107.46D0-4.129D0*TWC+5.096D-2*TWC**2-3.1915D8*XHCLF**3
     +      +1.0408D10*XHCLF**4-2.2793D11*XHCLF**5-5.8194D0*TWC**2*
     +      XHCLF+7.6883D4*TWC*XHCLF**2-7.4363D2*TWC**2*XHCLF**2+
     +      .059067D0*TWC**3*XHCLF+1.8132D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.80.0D0).AND.(TWC.LE.95.0D0)) THEN
        H2OS = (2.583D2-8.0386D0*TWC+1.739D5*XHCLF+7.608D-2*TWC**2-
     +      1.5492D7*XHCLF**2+3.956D9*XHCLF**3-2.065D11*XHCLF**4+
     +      1.3747D13*XHCLF**5-4.086D3*TWC*XHCLF+24.06D0*TWC**2*XHCLF+
     +      1.3558D5*TWC*XHCLF**2-3.076D7*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.95.0D0).AND.(TWC.LE.110.0D0)) THEN
        H2OS = (6.431D2-16.374D0*TWC+2.822D5*XHCLF+0.12117D0*TWC**2-
     +      8.224D7*XHCLF**2-7.387D6*XHCLF**3-5.247D3*TWC*XHCLF+
     +      24.30D0*TWC**2*XHCLF+1.5465D6*TWC*XHCLF**2-7.250D3*TWC**2*
     +      XHCLF**2) / TW
      ELSE IF (TWC.GT.110.0D0) THEN
        XTEMP = 18.3036D0 - 3816.44D0 / (TW-46.13D0)
        H2OS = 0.2885D0 * EXP(XTEMP) / TW
      ELSE
C        STOP 'Error in hcltran - H2O out of range'
         CALL XERROR('HCLTRAN - H2O out of range',0,1,1)
         IERROR = 12
         RETURN
      END IF
C
C     CALCULATE THE COEFFICIENTS
C
C     RK IS THE CONSTANT "kc" WHICH IS THE DEPOSITION COEFFICIENT (M/S)
C     RKE IS THE EQUILIBRIUM COEFFIENT BETWEEN THE GAS AND SOLID PHASE
C
      IF (TW.GE.TG) THEN
        RK = 8.33D-3
      ELSE
        X001 = .001D0
        RK = ABS(FLUX/(MAX(X001,TG-TW)*RHO*CP))
      END IF
      IF (H2OS.GT.H2O) THEN
        XTEMP = 1500.0D0 / TW
        EXPTW = EXP(XTEMP)
        RKE = B1 * EXPTW / (1.0D0+B2*EXPTW*HCLG) * (1.0D0+B5*H2O**B6/(
     +      (H2OS-H2O)**B7))
      ELSE
        RKE = 1.0D4
      END IF
C
C     CALCULATE THE DERIVATIVES
C
      HCLCOF = RK * (HCLG-HCLW/(RKE+1.0D-20))
      HNET = -HCLCOF * ARW
      XTEMP = -B4 / (8.31D0*TW)
      HWDOT = HCLCOF - B3 * EXP(XTEMP) * HCLW
      RETURN
      END

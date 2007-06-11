      SUBROUTINE CHEMIE(QQSPRAY,PYROL,ENTRAIN,SOURCE,LAYER,HCOMBT,CCO2T,
     + COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,QPYROL,NETFUEL,XMASS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CHEMIE
C
C     Source File: CHEMIE.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Do the combustion chemistry - for plumes in both the
C                   upper and lower layers.  Note that the kinetics scheme
C                   is implemented here.  However, applying it to the
C                   various pieces, namely the lower layer plume, the 
C                   upper layer plume, and the door jet fires, is 
C                   somewhat complex.
C                     
C                   Care should be exercised in making changes either 
C                   here or in the source interface routine.
C
C     Arguments: QQSPRAY  heat release rate at sprinkler activation time
C                PYROL   pyrolysis rate of the fuel (kg/s)
C                ENTRAIN plume entrainment rate (kg/s)
C                SOURCE  room number from which the mass flux comes
C                LAYER   layer mass is coming from (1=lower, 2=upper)
C                HCOMBT  current heat of combustion (J/kg)
C                CCO2T   current carbon/CO2 production ratio (kg/kg)
C                COCO2T  current CO/CO2 production ratio (kg/kg)
C                HCRATT  current Hydrogen/Carbon ratio in fuel (kg/kg)
C                OCRATT  current Oxygen/Carbon ratio in fuel (kg/kg)
C                CLFRAT  current HCl production rate (kg/kg pyrolized )
C                CNFRAT  current HCN production rate (kg/kg pyrolized)
!                crfrat  current trace species production (kg/kg pyrolized)
C                QPYROL  net heat release rate constrained by available
C                        oxygen (W)
C                NETFUEL net burning rate of fuel constrained by
C                        available oxygen (kg/s)
C                XMASS   net rate of production of species into layers
C                        in the room containing the fire (kg/s)
C
C     Revision History:
!     WWJ     03/07   added trace species tracking
C	WWJ	  03/04   Fixed oxygen and hydrogen accounting. The way o/c was being used was as a mass ratio
C					   of oxygen to fuel, but the manual specifies mass of oxygen to mass of carbon
C     RDP   4/8/94    moved summing of species production rates to calling
C                     routine so CHEMIE can be called multiple times.
C                     Removed EQUIVALENCE to XMASS, now an argument.
C     GPF   10/14/93  added sprinkler attenuation
C     RDP   12/18/92  modified oxygen limit to TANH for smooth cutoff
C                     independent of the limit
C     RDP   11/10/91  modified so NETMAS returns just amount for this
C                     fire.  Accumulation must be done by calling 
C                     routine.  Note new definition of NETMAS.
C                     Standardized calling sequence.
C     WWJ   8/6/90    remove entrainment calculation. Done by caller
C           3/3/90    procedure call modified to pass all changed
C                     arguments
C           2/1/90    more complete combustion scheme, account for H, O,
C                     HCL, and HCN
C           9/28/89   correct coefficients - FACTOR and NETCO2
C           11/14/88  smooth falloff of kinetics near oxygen limit
C           11/25/87  generalize for upper layer, lower layer, and plume
C*RE
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      DIMENSION XMASS(NS)
      INTEGER SOURCE
      DOUBLE PRECISION NETFUEL, NETFUL
      DOUBLE PRECISION NETH2O, NETCO2, NETCO, NETC, NETO2, NETCL, NETCN
	double precision mdotnet, mdotnetactual
      LOGICAL FIRST
      SAVE FIRST, O2F, O2FI, O2RANGE, XX0, XXFOUR, XXHALF, XXRANGE
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
        O2F = 1.31D+7
        O2FI = 1.0D0 / O2F
        O2RANGE = 0.01D0
        FIRST = .FALSE.
        XX0 = 0.0D0
	  XX1 = 1.0D0
        XXFOUR = 4.0D0
        XXHALF = 0.5D0
        XXRANGE = 8.0D0 / O2RANGE
      END IF

C     CALCULATE THE ACTUAL BURNING RATE CONSTRAINED BY AVAILABLE O2.
 
C     NOTE THE SCALING IN THE TANH FUNCTION.  TANH APPROACHES Ò2 AT
C     ABOUT Ò4. THE FUNCTION INSIDE THE TANH SCALES THE ORDINATE TO
C     ÒO2RANGE.  THE REMAINDER OF THE FUNCTION SCALES THE ABSCISSA 
C     TO 0-1 FOR O2INDEX.
 
      O2FRAC = ZZCSPEC(SOURCE,LAYER,2)
      O2ENTR = ENTRAIN * O2FRAC
      O2INDEX = TANH(XXRANGE*(O2FRAC-LIMO2)-XXFOUR) * XXHALF + XXHALF
      O2MASS = O2ENTR * O2INDEX
      QPYROL = MAX(XX0,MIN(PYROL*HCOMBT,O2MASS*O2F))

C     THIS IS THE KINETICS SCHEME AS DRIVEN BY DIFFUSION

C     FIRST CONVERT CHLORINE AND CYANIDE PRODUCTION TO CARBON BASED 
C     RATIOS

      FACT = (1.0D0+HCRATT+OCRATT) / (1.0D0-CLFRAT-CNFRAT)
      CLCRAT = CLFRAT * FACT
      CNCRAT = CNFRAT * FACT

      FCRATT = (1.0D0+OCRATT+HCRATT+CLCRAT+CNCRAT)

	mdotnet = pyrol
	mdotnetactual = min (mdotnet, o2mass*o2f/hcombt)
      qpyrol = mdotnetactual * hcombt

!	Here we do a reduction for sprinklers if activation has occurred. Otherwise we just save the current value of the HRR

      IF (IDSET.EQ.SOURCE) THEN

C    IF IDSET=SOURCE THEN SAVE VALUE OF FIRE FOR LATER QUENCHING

        QQSPRAY = QPYROL
      ELSE IF (IDSET.EQ.0) THEN

!	A sprinkler reduces the HRR from a fire. The reduction factor is determined by the sprinkler characteristics.
!	This factor is applied to the fire based on HRR at activation.
!	However, the HRR might be reduced for other reasons, so the arithmetic Min function is used.
!	The value of QQSPRAY is the value at activation. TFACT is then a reduction based on time since activation

        ID = IQUENCH(SOURCE)
        IF (ID.NE.0) THEN
          TDRATE = XDTECT(ID,DRATE)
          TIMEF = XDTECT(ID,DTACT)
          TFACT = EXP(-(STIME-TIMEF)/TDRATE)
          IF (QQSPRAY.GT.0.0D0) QPYROL = MIN(QPYROL,TFACT*QQSPRAY)
        END IF
      END IF

      NETFUEL = mdotnetactual
      NETO2 = -QPYROL * O2FI 

C     NOW DO THE "KINETICS SCHEME"

      NETH2O = 9.0D0 * NETFUEL * HCRATT / FCRATT
      FACTOR1 = 1.0D0 + HCOMBT * O2FI - OCRATT / FCRATT
      FACTOR2 = (CLCRAT+CNCRAT+9.D0*HCRATT) / FCRATT
      NETCO2 = (FACTOR1-FACTOR2) * NETFUEL / (1.D0+COCO2T+CCO2T)

      XMASS(2) = NETO2
      XMASS(3) = NETCO2
      XMASS(4) = NETCO2 * COCO2T
      XMASS(5) = CNFRAT * NETFUEL
      XMASS(6) = CLFRAT * NETFUEL
      XMASS(7) = -netfuel
      XMASS(8) = 9.0D0 * NETFUEL * HCRATT / FCRATT
      XMASS(9) = NETCO2 * CCO2T

      RETURN
      END

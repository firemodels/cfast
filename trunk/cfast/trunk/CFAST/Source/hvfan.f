      DOUBLE PRECISION FUNCTION HVFAN(tsec,II,JJ,K,DP)

C     Description:  This function returns the mass flow rate through a fan
C           This function has been modified to prevent negative flow.  
C           A MAX function was inserted just after the calculation off, the 
C           flow to do this.  If the flow is allowed to be negative 
C           (flow reversal) then this statement must be removed.
!           Also, there is now a flow restriction on the fan, using qcmfraction
C
C     Arguments: II     node number
C                JJ     jj'th connection to node II
C                K      fan number
C                DP     head pressure across the fan
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      LOGICAL firstc
	double precision hvfanl, openfraction, qcffraction, tsec,
     . minimumopen
      DATA firstc/.true./
      SAVE firstc
      PARAMETER (XX0 = 0.0D0)

      ROH = ROHB(ICMV(II,JJ))

	if (firstc) then
		minimumopen = sqrt(d1mach(1))
		firstc = .false.
	endif

! The hyperbolic tangent allows for smooth transition from full flow to no flow
! within the fan cuttoff pressure range
	F = 0.5 - tanh(8.0/(hmax(k)-hmin(k))*(dp-hmin(k))-4.0)/2.0
	F = MAX(XX0, F)
	HVFANl = F * qmax(k) * ROH
      openfraction = max (minimumopen, qcffraction (qcvm, k, tsec))
	hvfan = hvfanl * openfraction
	RETURN

      END

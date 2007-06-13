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
      LOGICAL ERRORL, ERRORH, firstc
	double precision hvfanl, openfraction, qcffraction, tsec,
     . minimumflow
      DATA ERRORL, ERRORH /2 * .TRUE./, firstc/.true./
      SAVE ERRORL, ERRORH, firstc
      PARAMETER (XX0 = 0.0D0)

      ROH = ROHB(ICMV(II,JJ))

	if (firstc) then
		minimumflow = sqrt(d1mach(1))
		firstc = .false.
	endif

      IF (DP.LT.HMIN(K)) THEN
         IF (ERRORL) THEN
            WRITE (LOGERR,5000) DP,tsec
            ERRORL = .FALSE.
         END IF
! changed from   HVFAN = QMIN(K) to:
         HVFANl = QMIN(K)*ROH
      ELSE IF (DP.GT.HMAX(K)) THEN
         IF (ERRORH) THEN
            WRITE (LOGERR,5000) DP,tsec
            ERRORH = .FALSE.
         END IF
! changed from  HVFAN = QMAX(K) to:
         HVFANl = QMAX(K) * ROH
      ELSE
         call interp(dpmm,qfmm,2,dp,1,f)
! prevent negative flow
         F = MAX(XX0, F)
         HVFANl = F * ROH
      END IF

	openfraction = max (minimumflow, qcffraction (qcvm, k, tsec))
	hvfan = hvfanl * openfraction

      RETURN

 5000 FORMAT (5('*'),' Flow outside fan curve, DP = ',1PG12.3,
     . ' at T = ',f10.3)
      END

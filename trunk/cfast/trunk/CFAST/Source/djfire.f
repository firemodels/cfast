      SUBROUTINE DJFIRE(ITO,TJET,XXNETFL,SAS,HCOMBT,QPYROL,XNTMS,
     .                  DJFLOWFLG)
C*RB
C     Routine:  DJFIRE
C
C     Function: Calculate heat and combustion chemistry for a door jet
C               fire.
C
C     Inputs:   ITO     Room number door jet is flowing into
C               TJET    Temperature of the door jet gas
C               XXNETFL Net fuel available to be burned
C               SAS     Mass flow rate of entrained air in door jet
C               HCOMBT  Heat of combustion of unburned fuel
C     Outputs:  QPYROL  Total heat released by door jet fire
C               XNTMS   Net change in mass of species in door jet
C     Commons:
C        USED:  Activs   Tgignt   Zzgspec  Zzmass
C
C     Revision History:
C     gpf 5/11/98
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
C     RDP   4/8/94    moved summing of species production rates from CHEMIE
C                     to calling routine.
C     gpf 10/14/93   In DOFIRE, the routine CHEMIE requires info about
C                    fire size for sprinkler option.  Though a door jet
C                    fire is not attenuated by sprinklers, the call
C                    to CHEMIE had to made consistent with call in
C                    dofire.
C*RE
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
C
      DIMENSION XNTMS(2,NS), XMASS(NS)
      LOGICAL DJFLOWFLG
C
      X0 = 0.0D0
      QPYROL = X0
      DJFLOWFLG = .FALSE.
C
C     WE ONLY WNAT TO DO THE DOOR JET CALCULATION IF THERE IS FUEL,
C     OXYGEN, AND SUFFICIENT TEMPERATURE IN THE DOOR JET
C
      IF (XXNETFL.GT.X0.AND.SAS.GT.X0.AND.TJET.GE.TGIGNT) THEN
C
C     DO COMBUSTION CHEMISTRY ASSUMING COMPLETE COMVERSION TO CO2 & H2O.
C     ALTHOUGH THE REAL CHEMISTRY IS MORE COMPLEX, FOR NOW WE DON'T KNOW
C     HOW TO HANDLE IT.
C
        DUMMY = -1.0D0
        DJFLOWFLG = .TRUE.
        DO 10 i = 1, NS
          XMASS(i) = X0
   10   CONTINUE
        CALL CHEMIE(DUMMY,XXNETFL,SAS,ITO,LOWER,HCOMBT,X0,X0,X0,X0,X0,
     +      X0,QPYROL,XXNETFUE,XMASS)
        DO 20 I = 1, NS
          XNTMS(UPPER,I) = XMASS(I)
          XNTMS(LOWER,I) = X0
   20   CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE HALLHT(IROOM,IDSTART,ND)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HEATHT
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the velocity and temperature
C                   of the ceiling jet at each detector location in
C                   a corridor.
C
C     Arguments: IROOM - room number of corridor
C                IDSTART - index of first detector in corridor IROOM
C                ND - number of detectors in room IROOM
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      use cenviro
      use cparams
      use dsize
      include "precis.fi"
      include "cfast.fi"

      DO 10 ID = IDSTART, IDSTART + ND - 1
        XX = XDTECT(ID,DXLOC)
        YY = XDTECT(ID,DYLOC)
        ZZ = XDTECT(ID,DZLOC)
        IF(IZHALL(IROOM,IHXY)==1)THEN
          XLEN = XX
         ELSE
          XLEN = YY
        ENDIF
        CALL HALLTRV(IROOM,XLEN,ZZ,TEMP,RHO,VEL)
        XDTECT(ID,DTJET) = TEMP
        XDTECT(ID,DVEL) = VEL
   10 CONTINUE
      RETURN
      END

      SUBROUTINE HALLTRV(IROOM,XLOC,ZLOC,HALLTEMP,HALLRHO,HALLVEL)
      
      use cenviro
      use cparams
      use dsize
      include "precis.fi"
      include "cfast.fi"

      IF(IZHALL(IROOM,IHMODE)==IHDURING)THEN
        D0 = ZZHALL(IROOM,IHDEPTH)
        CJETHEIGHT = HR(IROOM) - D0

C*** location is in hall ceiling jet

        IF(ZLOC>=CJETHEIGHT.AND.XLOC<=ZZHALL(IROOM,IHDIST))THEN
          C1 = 1.0D0
          IHALF = IZHALL(IROOM,IHHALFFLAG)
          HHALF = ZZHALL(IROOM,IHHALF)
          DT0 = ZZHALL(IROOM,IHTEMP)

C*** check to see if the user specified a HHALF value on the command line.
C    if not (ie if IHALF==0) then calculate it 
C    using the correlations.

          IF(IHALF==0)THEN
c***  hhalf = -log10(2)/.018
            HHALF = 16.70D0
            ZZHALL(IROOM,IHHALF) = HHALF
          ENDIF

C*** if HHALF < 0.0 then assume that the temperature does not decay
C    (ie flow is adiabatic)

          IF(HHALF>0.0D0)THEN
            FACT = 0.5D0**(XLOC/HHALF)
           ELSE
            FACT = 1.0D0
          ENDIF

          HALLTEMP = ZZTEMP(IROOM,LOWER) + DT0*FACT
          HALLRHO = ZZPABS(IROOM)/(RGAS*HALLTEMP)
          HALLVEL = ZZHALL(IROOM,IHVEL)
         ELSE
          HALLTEMP = ZZTEMP(IROOM,LOWER)
          HALLRHO = ZZRHO(IROOM,LOWER)
          HALLVEL = 0.10D0
        ENDIF
       ELSE

C*** hall jet is not flowing (either has not started or has finished)
C    so, use regular layer temperatures and densities

        IF(ZLOC>ZZHLAY(IROOM,LOWER))THEN
          HALLTEMP = ZZTEMP(IROOM,UPPER)
          HALLRHO = ZZRHO(IROOM,UPPER)
         ELSE
          HALLTEMP = ZZTEMP(IROOM,LOWER)
          HALLRHO = ZZRHO(IROOM,LOWER)
        ENDIF
        HALLVEL = 0.10D0
      ENDIF

      RETURN
      END

      SUBROUTINE SETHALL(ITYPE,INUM,IHALL,TSEC,WIDTH,
     .                   HTEMP,HVEL,HDEPTH)
      
      use cenviro
      use cparams
      use dervs
      use dsize
      include "precis.fi"
      include "cfast.fi"
      include "vents.fi"

      XX0 = 0.0D0
      HHTEMP = HTEMP - ZZTEMP(IHALL,LOWER)

C*** This routine is only executed if 1) hall flow has not started
C    yet or 2)  hall flow has started and it is coming from the
C    IVENT'th vent

C     gpfrn 4/21/98: change the flow coefficient, based on latest les3d results

      IF(IZHALL(IHALL,IHVENTNUM)/=0.AND.
     .             IZHALL(IHALL,IHVENTNUM)/=INUM)RETURN
      ROOMWIDTH = MIN(BR(IHALL),DR(IHALL))
      ROOMLENGTH = MAX(BR(IHALL),DR(IHALL))
      VENTWIDTH = MIN(WIDTH,ROOMWIDTH)
      OTHIRD = 1.0D0/3.0D0
      FRACTION = (VENTWIDTH/ROOMWIDTH)**OTHIRD
      HALLDEPTH = HDEPTH*FRACTION**2
      HALLVEL = HVEL*FRACTION

C*** hall flow has not started yet

      IF(IZHALL(IHALL,IHVENTNUM)==0)THEN

C*** flow is going into the hall room and flow is below the soffit

        IF(HALLVEL>XX0.AND.HALLDEPTH>XX0)THEN
          IZHALL(IHALL,IHVENTNUM) = INUM
          IZHALL(IHALL,IHMODE) = IHDURING
          ZZHALL(IHALL,IHTIME0) = TSEC
          ZZHALL(IHALL,IHTEMP) = HHTEMP
          ZZHALL(IHALL,IHDIST) = 0.0D0
          IF(IZHALL(IHALL,IHVELFLAG)==0)ZZHALL(IHALL,IHVEL) = HALLVEL
          IF(IZHALL(IHALL,IHDEPTHFLAG)==0)THEN
            ZZHALL(IHALL,IHDEPTH) = HALLDEPTH
          ENDIF
          VENTDIST0 = -1.

C*** corridor flow coming from a vent 

          IF(ITYPE==1)THEN
            IF(IZVENT(INUM,1)==IHALL)THEN
              VENTDIST0 = ZZVENT(INUM,4)
             ELSEIF(IZVENT(INUM,2)==IHALL)THEN
              VENTDIST0 = ZZVENT(INUM,5)
            ENDIF
          ENDIF

C*** corridor flow coming from the main fire.
C    this is a restriction, but lets get it right for the main
C    fire before we worry about objects

          IF(ITYPE==2)THEN
            IF(IZHALL(IHALL,IHXY)==1)THEN
              VENTDIST0 = XFIRE(1,1)
             ELSE
              VENTDIST0 = XFIRE(1,2)
            ENDIF
          ENDIF
          ZZHALL(IHALL,IHORG) = VENTDIST0

          VENTDIST = -1.0D0
          VENTDISTMAX = VENTDIST 

C*** compute distances relative to vent where flow is coming from.
C    also compute the maximum distance

          DO 10 I = 1, NVENTS
            IF(IZVENT(I,1)==IHALL)THEN

C*** if distances are not defined for the origin or destination vent
C    then assume that the vent at the "far" end of the corridor

              IF(ZZVENT(I,4)>0.0D0.AND.VENTDIST0>=0.0D0)THEN
                VENTDIST = ABS(ZZVENT(I,4) - VENTDIST0)
               ELSE
                VENTDIST = ROOMLENGTH - VENTDIST0
              ENDIF
              ZZVENTDIST(IHALL,I) = VENTDIST
             ELSEIF(IZVENT(I,2)==IHALL)THEN

C*** if distances are not defined for the origin or destination vent
C    then assume that the vent at the "far" end of the corridor

              IF(ZZVENT(I,5)>0.0D0.AND.VENTDIST0>=0.0D0)THEN
                VENTDIST = ABS(ZZVENT(I,5) - VENTDIST0)
               ELSE
                VENTDIST = ROOMLENGTH - VENTDIST0
              ENDIF
              ZZVENTDIST(IHALL,I) = VENTDIST
             ELSE
              VENTDIST = -1.0D0
              ZZVENTDIST(IHALL,I) = VENTDIST
            ENDIF
   10     CONTINUE

C***      let the maximum distance that flow in a corridor can flow be
C         the width of the room, ie:

          ZZHALL(IHALL,IHMAXLEN) = ROOMLENGTH - VENTDIST0

          RETURN
        ENDIF
        RETURN
      ENDIF

C*** hall flow is coming from a vent or a fire

      IF(IZHALL(IHALL,IHVENTNUM)==INUM)THEN
        THALL0 = ZZHALL(IHALL,IHTIME0)
        F1 = (TOLD - THALL0)/(STIME-THALL0)
        F2 = (STIME - TOLD)/(STIME-THALL0)
        IF(IZHALL(IHALL,IHVELFLAG)==0)THEN
          ZZHALL(IHALL,IHVEL) = ZZHALL(IHALL,IHVEL)*F1 + ABS(HALLVEL)*F2
        ENDIF
        IF(IZHALL(IHALL,IHDEPTHFLAG)==0)THEN
          ZZHALL(IHALL,IHDEPTH) = ZZHALL(IHALL,IHDEPTH)*F1 + 
     .                            HALLDEPTH*F2
        ENDIF
        ZZHALL(IHALL,IHTEMP) = ZZHALL(IHALL,IHTEMP)*F1 + HHTEMP*F2
        VENTDISTMAX = ZZHALL(IHALL,IHMAXLEN)
        VENTDISTMIN = ROOMLENGTH - VENTDISTMAX
        CJETDIST = ZZHALL(IHALL,IHDIST) + DT*ZZHALL(IHALL,IHVEL)


C*** If ceiling jet has reached the end of the hall then 
C    indicate this fact in IZHALL  

        IF(CJETDIST>=VENTDISTMAX)THEN
          IZHALL(IHALL,IHMODE) = IHAFTER
          CJETDIST = VENTDISTMAX
        ENDIF
        ZZHALL(IHALL,IHDIST) = CJETDIST

      ENDIF
      RETURN
      END
       integer function rev_flowhall
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_flowhall = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_flowhall
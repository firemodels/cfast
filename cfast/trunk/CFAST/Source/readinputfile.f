      SUBROUTINE readinputfile (IERROR)

!	Read the input file and set up the data for processing

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "params.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "objects1.fi"
      include "thermp.fi"
      include "iofiles77.fi"

!	Pretty pedestrian to use these names, but...
	integer numr, numc
      LOGICAL LACTIVS(NS), EXISTS
      CHARACTER*133 MESSG
      CHARACTER AVERSION*5
      DIMENSION YINTER(NR)
      EQUIVALENCE (YINTER,QFR)
      DIMENSION TEMPAREA(MXPTS), TEMPHGT(MXPTS)

!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output (header) - note this is rewound each time the plot data is written)
!     14 smokeview output (plot data)
!     15 spreadsheet output (normal)
!     16 spreadsheet output (flow field)
!     17 spreadsheet output (species)
!     18 spreadsheet output (walls and targets)
!
!     switch (1) = CEILING PROPERTIES ARE DEFINED
!            (2) = FLOOR PROPERTIES ARE DEFINED
!            (3) = SIDE WALL PROPERTIES ARE DEFINED FOR UPPER WALLS
!            (4) = SIDE WALL PROPERTIES ARE DEFINED FOR LOWER WALLS

      IFAIL = 0
      XX0 = 0.0D0

!	Deal with opening the data file and assuring ourselves that it is compatible

      CLOSE (IOFILI)
      OPEN (UNIT=IOFILI,FILE=inputfile,ERR=130,STATUS='OLD',iostat=ios)
	call readcsvformat(iofili,rarray,carray,nrow,ncol,1,numr,numc,
     . ierror)
	if (ierror.gt.0) then
		 write(logerr,5003)
		 return
	endif

	close (iofili)

!	aversion is the header name, ivers is the major version number read in, iversion is the major version number
!	from the internal version data. These need to be compatible

	aversion = carray(1,1)
	ivers = rarray(1,2)
	iversion = version / 100
	if (version.ge.1000) iversion = version / 1000
	if (aversion.eq.heading.and.ivers.eq.iversion) then
		 write (logerr,5001) ivers
	else
		 write (logerr,5002) aversion,heading,ivers,iversion
		 ierror = 206
		 return
      END IF
	title = carray(1,3)

!     READ IN ALL SPECIFICATIONS

      DO 10 I = 1, NR
        YINTER(I) = -1.0D0
   10 CONTINUE
      
	call keywordcases (numr, numc, ierror)

!	wait until the input file is parsed before dieing on temperature
	if (exta.gt.373.15.or.exta.lt.223.15d0) then
		write(logerr,5022) exta
		ierror = 218
	endif
	if (ta.gt.373.15.or.ta.lt.223.15d0) then
		write(logerr,5022) ta
		ierror = 218
	endif

      IF (IERROR.NE.0) RETURN

!	We now know what output is going to be generated, so create the files

	call openoutputfiles

C     WE COME HERE FOR ANY CALL TO the keywordcases statement

      RA = PA / TA / RGAS
      EXRA = EXPA / EXTA / RGAS

!	Get the mainfire and set the species

	if (objnin(0).ne.' ') call inputmainfire (iofili,ierror)
	if (ierror.ne.0) return
!!!!!!!!!!!!!!!!!!!!!diagnostic
	if (debugging) call printfireparameters
	
      NLSPCT = 0
!	Note that we cannot reset the CT parameter
      DO 90 I = 1, NS-1
		 NLSPCT = NLSPCT + 1
          DO 80 J = 1, LFMAX
            MPRODR(J,I) = 0.0D0
   80     CONTINUE
   90 CONTINUE

      IF(LFBT.LT.0.OR.LFBT.GT.2) THEN
	   write(logerr,5101) lfbt
		IERROR = 201
         RETURN
      END IF

C     TURN ON THE SUBSIDIARY EQUATIONS IF THEY ARE NEEEDED - this is always true

         IF (ACTIVS(6)) HCLDEP = 1

C **** NOTE THAT SMOKE AGGLOMERATION IS NOT DONE, SO THE FOLLOWING
C **** FACTOR IS NEVER SET - THIS WILL PROVIDE FOR A SET OF SUBSIDARY
C **** EQUATION TO TRACK SMOKE NUMBER DENSITY AS WELL AS MASS DENSITY
C        IF (ACTIVS(9)) SMKAGL = 1

         NM1 = N - 1
         IF (NUMOBJL.GT.0) THEN
			do 19 i = 1, numobjl
			   CALL inputobject (objnin(i), i, iofili, ierror)
			   if (debugging) call printobjectparameters(i)
               IF (IERROR.NE.0) RETURN
   19			continue
         END IF

!	Initialize the targets
         CALL INITTARG (IERROR)
          IF (IERROR.NE.0) RETURN
!     NOW CALCULATE THE OFFSETS - the order is important
         CALL OFFSET (IERROR)
		 IF (IERROR.NE.0) RETURN

C     FLOOR PLAN DEPENDENT PARAMETERS

         NM1 = N - 1
         DO 20 I = 1, NM1
            HRL(I) = HFLR(I)
            HRP(I) = HR(I) + HFLR(I)
   20    CONTINUE

C     CHECK AND/OR SET FPOS

         IF (LFBO.GT.0) THEN
            IF ((FPOS(1).LT.XX0).OR.(FPOS(1).GT.BR(LFBO))) THEN
               FPOS(1) = BR(LFBO) / 2.0D0
               IF (LOGERR.GT.0) WRITE (LOGERR,5000) FPOS(1)
            END IF
            IF ((FPOS(2).LT.XX0).OR.(FPOS(2).GT.DR(LFBO))) THEN
               FPOS(2) = DR(LFBO) / 2.0D0
               IF (LOGERR.GT.0) WRITE (LOGERR,5010) FPOS(2)
            END IF
            IF ((FPOS(3).LT.XX0).OR.(FPOS(3).GT.HR(LFBO))) THEN
               FPOS(3) = 0.0D0
               IF (LOGERR.GT.0) WRITE (LOGERR,5020) FPOS(3)
            END IF
         ENDIF

C     CHECK AND/OR SET heatf position

         IF (heatfl) THEN
            IF ((heatfp(1).LT.XX0).OR.(heatfp(1).GT.BR(heatfr))) THEN
               heatfp(1) = BR(heatfr) / 2.0D0
            END IF
            IF ((heatfp(2).LT.XX0).OR.(heatfp(2).GT.DR(heatfr))) THEN
               heatfp(2) = DR(heatfr) / 2.0D0
            END IF
            IF ((heatfp(3).LT.XX0).OR.(heatfp(3).GT.HR(heatfr))) THEN
               heatfp(3) = 0.0D0
            END IF
		   write(logerr,5021) heatfr,heatfp
         ENDIF

C     CHECK AND/OR SET POSITION OF OBJECTS

         DO 1300 I = 1, NUMOBJL
            IF((OBJPOS(1,I).LT.XX0).OR.
     .                 (OBJPOS(1,I).GT.BR(OBJRM(I)))) THEN
               OBJPOS(1,I) = BR(OBJRM(I)) / 2.0D0
               IF (LOGERR.GT.0) WRITE (LOGERR,5080) I, OBJPOS(1,I)
            END IF
            IF((OBJPOS(2,I).LT.XX0).OR.
     .                 (OBJPOS(2,I).GT.DR(OBJRM(I)))) THEN
               OBJPOS(2,I) = DR(OBJRM(I)) / 2.0D0
               IF (LOGERR.GT.0) WRITE (LOGERR,5090) I, OBJPOS(2,I)
            END IF
            IF((OBJPOS(3,I).LT.XX0).OR.
     .                 (OBJPOS(3,I).GT.HR(OBJRM(I)))) THEN
               OBJPOS(3,I) = XX0
               IF (LOGERR.GT.0) WRITE (LOGERR,5100) I, OBJPOS(3,I)
            END IF
 1300    CONTINUE
C    MAKE SURE HORIZONTAL VENT SPECIFICATIONS ARE CORRECT -  WE HAVE TO DO THIS
C    HERE RATHER THAN RIGHT AFTER NPUTQ BECAUSE HRL AND HRP WERE JUST DEFINED
C    ABOVE

         DO 40 ITOP = 1, NM1
           IF (NWV(ITOP,ITOP).NE.0) THEN
            IF (LOGERR.GT.0) WRITE (LOGERR,*) 
     +        ' A ROOM CAN NOT BE CONNECTED TO ITSELF'
            NWV(ITOP,ITOP) = 0
           END IF
           DO 30 IBOT = 1, ITOP - 1
             IF (NWV(ITOP,IBOT).NE.0.OR.NWV(IBOT,ITOP).NE.0) THEN

C    SEE WHICH ROOM IS ON TOP (IF ANY) - THIS IS LIKE A BUBBLE SORT

              DEPS1 = HRL(ITOP) - HRP(IBOT)
              DEPS2 = HRL(IBOT) - HRP(ITOP)
              IF (NWV(ITOP,IBOT).NE.1.OR.ABS(DEPS1).GE.VFMAXDZ) THEN
               IF (NWV(IBOT,ITOP).NE.1.OR.ABS(DEPS2).GE.VFMAXDZ) THEN
                IF (NWV(ITOP,IBOT).EQ.1.AND.ABS(DEPS2).LT.VFMAXDZ) THEN
                  IF (NWV(IBOT,ITOP).NE.0) THEN
                     WRITE (LOGERR,*) 'Vent ', IBOT, ITOP, 
     +                       ' is being redefined'
                  END IF
                  NWV(ITOP,IBOT) = 0
                  NWV(IBOT,ITOP) = 1
                  VVAREA(IBOT,ITOP) = VVAREA(ITOP,IBOT)
                  VSHAPE(IBOT,ITOP) = VSHAPE(ITOP,IBOT)
                  GO TO 30
                END IF
                IF (NWV(IBOT,ITOP).EQ.1.AND.ABS(DEPS1).LT.VFMAXDZ) THEN
                  IF (NWV(ITOP,IBOT).NE.0) THEN
                      WRITE (LOGERR,*) 'Vent ', ITOP, IBOT, 
     +                    ' is being redefined'
                  END IF
                  NWV(ITOP,IBOT) = 1
                  NWV(IBOT,ITOP) = 0
                  VVAREA(ITOP,IBOT) = VVAREA(IBOT,ITOP)
                  VSHAPE(ITOP,IBOT) = VSHAPE(IBOT,ITOP)
                  GO TO 30
                END IF
                NWV(ITOP,IBOT) = 0
                NWV(IBOT,ITOP) = 0
               END IF
              END IF
             END IF
   30      CONTINUE
   40    CONTINUE

         DO 50 I = 1, NM1
            AR(I) = BR(I) * DR(I)
            VR(I) = AR(I) * HR(I)
   50    CONTINUE


C*** CHECK ROOM TO ROOM HEAT TRANSFER PARAMETERS (CFCON COMMAND)
         NSWALL2 = NSWAL
         II = 0
         DO 200 I = 1, NSWAL
            IROOM1 = IZSWAL(I,1)
            IROOM2 = IZSWAL(I,3)

C*** ROOM NUMBERS MUST BE BETWEEN 1 AND NM1

            IF(IROOM1.LT.1.OR.IROOM2.LT.1.OR.
     .          IROOM1.GT.NM1+1.OR.IROOM2.GT.NM1+1)THEN
              IFAIL = 39
              WRITE (MESSG,201)IROOM1,IROOM2 
  201         FORMAT(' INVALID CFCON SPECIFICATION:',
     +         ' ONE OR BOTH OF ROOMS ',2I3, ' IS OUT OF BOUNDS')
              CALL XERROR(MESSG,0,1,1)
            ENDIF

C*** If room is connected to the outside then ignore it
C
            IF(IROOM1.EQ.NM1+1.OR.IROOM2.EQ.NM1+1)THEN
              NSWALL2 = NSWALL2 - 1
              GO TO 200
             ELSE
              II = II + 1
              IF(I.NE.II)THEN
                IZSWAL(II,1) = IZSWAL(I,1)
                IZSWAL(II,2) = IZSWAL(I,2)
                IZSWAL(II,3) = IZSWAL(I,3)
                IZSWAL(II,4) = IZSWAL(I,4)
              ENDIF
            ENDIF

C*** FLOOR OF ONE ROOM MUST BE ADJACENT TO CEILING OF THE OTHER

            DWALL1 = ABS(HRL(IROOM1) - HRP(IROOM2))
            DWALL2 = ABS(HRL(IROOM2) - HRP(IROOM1))
            IF(DWALL1.LT.VFMAXDZ.OR.DWALL2.LE.VFMAXDZ)THEN
               IF(DWALL1.LT.VFMAXDZ)THEN
                 IZSWAL(II,2) = 2
                 IZSWAL(II,4) = 1
                ELSE
                 IZSWAL(II,2) = 1
                 IZSWAL(II,4) = 2
               ENDIF
              ELSE
               IFAIL = 40
               WRITE (MESSG,202)IROOM1,IROOM2 
  202          FORMAT(' INVALID CFCON SPECIFICATION:'
     +                ' CEILING AND FLOOR OF ROOMS',
     +                2I3, ' ARE NOT CONNECTETD')
            ENDIF

C*** WALLS MUST BE TURNED ON, IE SWITCH MUST BE SET
C    FOR THE CEILING IN THE LOWER ROOM AND THE FLOOR OF
C    THE UPPER ROOM

            IWALL1 = IZSWAL(II,2)
            IWALL2 = IZSWAL(II,4)
            IF(.NOT.SWITCH(IWALL1,IROOM1).OR.
     .         .NOT.SWITCH(IWALL2,IROOM2))THEN
               WRITE (MESSG,203)
  203          FORMAT(' INVALID CFCON SPECIFICATION:')
               CALL XERROR(MESSG,0,1,1)
               IF(.NOT.SWITCH(IWALL1,IROOM1))THEN
                  WRITE(MESSG,204)IWALL1, IROOM1
  204             FORMAT(' WALL ',I2,' OF ROOM ',I2,' IS NOT TURNED ON')
                  CALL XERROR(MESSG,0,1,1)
               ENDIF
               IF(.NOT.SWITCH(IWALL2,IROOM2))THEN
                  WRITE(MESSG,204)IWALL2,IROOM2
                  CALL XERROR(MESSG,0,1,1)
               ENDIF
               IFAIL = 41
            ENDIF
  200    CONTINUE
         NSWAL = NSWALL2


C*** CHECK SHAFTS

         DO 205 IROOM = NM1 + 1, NR
            IF(IZSHAFT(IROOM).NE.0)THEN
               CALL XERROR(' INVALID SHAFT SPECIFICATION:',0,1,1)
               IFAIL = 42
               WRITE (MESSG,206)IROOM,NM1
  206          FORMAT(' Room ',I3,' must be less than or equal to ',I3)
               CALL XERROR(MESSG,0,1,1)
            ENDIF
  205    CONTINUE

C    INITIALIZE VARIABLES THAT WILL CHANGE WHEN AMBIENT CONDITIONS CHANGE

         CALL INITAMB(YINTER,1)

C     INITIALIZE THE MECHANICAL VENTILATION

         CALL HVINIT (IERROR)
         IF (IERROR.NE.0) RETURN

C*** CHECK DETECTOR STUFF

         DO 65 I = 1, NDTECT
            IROOM = IXDTECT(I,DROOM)
            IF(IROOM.LT.1.OR.IROOM.GT.NM1)THEN
              WRITE (MESSG,104)IROOM 
  104         FORMAT('Invalid DETECTOR specification: Room ',
     +              I3, ' is not a valid')
              IFAIL = 43
              CALL XERROR(MESSG,0,1,1)
            ENDIF
            RTI = XDTECT(I,DRTI)
            IF(RTI.LE.0.0D0.AND.IXDTECT(I,DTYPE).NE.SMOKED)THEN
              WRITE (MESSG,101)RTI 
  101         FORMAT('Invalid DETECTOR specification - RTI= ',
     +              E11.4, ' is not a valid.')
              IFAIL = 44
            ENDIF
            XLOC = XDTECT(I,DXLOC)
            YLOC = XDTECT(I,DYLOC)
            ZLOC = XDTECT(I,DZLOC)
            IF(XLOC.LT.0.0D0.OR.XLOC.GT.bR(IROOM).OR.
     .         YLOC.LT.0.0D0.OR.YLOC.GT.dR(IROOM).OR.
     .         ZLOC.LT.0.0D0.OR.ZLOC.GT.HRP(IROOM))THEN
               WRITE(MESSG,102)XLOC,YLOC,ZLOC
  102          FORMAT('Invalid DETECTOR specification - x,y,z,location',
     +               'x,y,z=',3E11.4,' is out of bounds')
               IFAIL = 45
            ENDIF
            IDTYPE = IXDTECT(I,DTYPE)
            IF(IDTYPE.LT.1.OR.IDTYPE.GT.3)THEN
              WRITE(MESSG,103)IDTYPE
  103         FORMAT('Invalid DETECTOR specification - TYPE= ',
     +              I2,' is not a valid')
              IFAIL = 46
            ENDIF
   65    CONTINUE

C     FIRE TYPE AND PARAMETERS: COME HERE DIRECTLY IF THIS IS A RESTART

      IFIRED = 1
      TFMAXT = 0.0D0
      DO 60 I = 1, LFMAX
        TFMAXT = MAX(TFMAXT,TFIRED(I))
   60 CONTINUE

C     CHECK GUISELCT MAKE SURE IT HAS ONLY LEGAL ROOMS

      IGUISLCT = 0
      DO 140 I = 1, MAXCOL
        IF (GUISELCT(I).LE.0.OR.GUISELCT(I).GT.NM1) THEN
          GUISELCT(I) = 0
        ELSE
          IGUISLCT = IGUISLCT + 1
        END IF
  140 CONTINUE
      DO 150 I = MAXCOL-1, 1, -1
        IF (GUISELCT(I).EQ.0) THEN
          DO 155 J = I+1,MAXCOL
            GUISELCT(J-1) = GUISELCT(J)
  155     CONTINUE
          GUISELCT(MAXCOL) = 0
        END IF
  150 CONTINUE
      IF (GUISELCT(1).EQ.0) THEN
        IGUISLCT = MIN(MAXCOL,NM1)
        DO 157 I = 1, IGUISLCT
          GUISELCT(I) = I
  157   CONTINUE
      END IF

C*** check room area specs and convert to volume

      DO 300 I = 1, NM1
        NPTS = IZRVOL(I)
        IF(NPTS.NE.0)THEN

c*** force first elevation to be at the floor; add a data point if necessary (same area as first entered data point)

          IF(ZZRHGT(1,I).NE.0.0D0)THEN
            TEMPAREA(1) = ZZRAREA(1,I)
            TEMPHGT(1) = 0.0D0
            IOFF = 1
           ELSE
            IOFF = 0
          ENDIF

c*** copy data to temporary arrays

          DO 310 J = 1, NPTS
            TEMPAREA(J+IOFF) = ZZRAREA(J,I)
            TEMPHGT(J+IOFF) = ZZRHGT(J,I)
  310     CONTINUE

c*** force last elevation to be at the ceiling (as defined by hr(i)

          IF(HR(I).NE.ZZRHGT(NPTS,I))THEN
              IOFF2 = 1
              TEMPAREA(NPTS+IOFF+IOFF2) = ZZRAREA(NPTS,I)
              TEMPHGT(NPTS+IOFF+IOFF2) = HR(I)
             ELSE
              IOFF2 = 0
          ENDIF

          NPTS = NPTS + IOFF + IOFF2
          IZRVOL(I) = NPTS

c*** copy temporary arrays to zzrhgt and zzrarea; define volume by integrating areas

          ZZRHGT(1,I) = 0.0D0
          ZZRVOL(1,I) = 0.0D0
          ZZRAREA(1,I) = TEMPAREA(1)
          J = 1
          DO 320 J = 2, NPTS
            ZZRHGT(J,I) = TEMPHGT(J)
            ZZRAREA(J,I) = TEMPAREA(J)
            DAREA = (ZZRAREA(J,I)+ZZRAREA(J-1,I))/2.0D0
            DHEIGHT = ZZRHGT(J,I) - ZZRHGT(J-1,I)
            ZZRVOL(J,I) = ZZRVOL(J-1,I) + DAREA*DHEIGHT
  320     CONTINUE

C*** re-define volume, area, breadth and depth arrays 
C        (vr, ar, br and dr ) according to room area - height
C        data read in.  HR remains the same, VR is defined
C        by integrating areas specified on the ROOMAREA command,
C        AR is then VR/HR, BR and DR are defined so that
C        BR*DR=AR and BR/DR remain the same as entered on
C        the WIDTH and DEPTH  commands.

          VR(I) = ZZRVOL(NPTS,I)
          AR(I) = VR(I)/HR(I)
          XX = bR(I)/dR(I)
          BR(I) = SQRT(AR(I)*XX)
          DR(I) = SQRT(AR(I)/XX)
        ENDIF
  300 CONTINUE


C*** room to room heat transfer 

C  The array IZHEAT may have one of three values, 0, 1, 2.  
C  0 = no room to room heat transfer
C  1 = fractions are determined by what rooms are connected by vents
C      For example, if room 1 is connected to rooms 2, 3, 4 and the outside
C      by vents then the first row of ZZHTFRAC will have the values
C      0. .25 .25 .25 .25

c***  force all rooms to transfer heat between connected rooms

      IF(IZHEAT(0).EQ.1)THEN
        DO 400 I = 1, NM1
          IZHEAT(I) = 1
  400   CONTINUE
      ENDIF
      DO 410 I = 1, NM1

c*** force heat transfer between rooms connected by vents.

        IF(IZHEAT(I).EQ.1)THEN
          DO 420 J = 1, NM1+1
            NVENTIJ = 0
            DO 430 K = 1, 4
              NVENTIJ = NVENTIJ + IJK(I,J,K)
  430       CONTINUE
            IF(NVENTIJ.NE.0)ZZHTFRAC(I,J) = 1.0D0

C*** if the back wall is not active then don't consider its contribution

				IF(J.LE.NM1.AND..NOT.SWITCH(3,J))ZZHTFRAC(I,J) = 0.0D0
  420     CONTINUE
        ENDIF

c*** normalize ZZHTFRAC fraction matrix so that rows sum to one

        IF(IZHEAT(I).NE.0)THEN
          SUM = 0.0D0
          DO 440 J = 1, NM1+1
            SUM = SUM + ZZHTFRAC(I,J)
  440     CONTINUE
          IF(SUM.LT.1.D-5)THEN
            DO 450 J = 1, NM1
              ZZHTFRAC(I,J) = 0.0D0
  450       CONTINUE
            ZZHTFRAC(I,NM1+1) = 1.0D0
           ELSE
            DO 460 J = 1, NM1+1
              ZZHTFRAC(I,J) = ZZHTFRAC(I,J)/SUM
  460       CONTINUE
          ENDIF
          JJ = 0
          DO 480 J = 1, NM1
            IF(ZZHTFRAC(I,J).NE.0.0D0)THEN
              IZHTFRAC(I,0) = IZHTFRAC(I,0) + 1
              JJ = JJ + 1
              IZHTFRAC(I,JJ) = J
            ENDIF
  480     CONTINUE
        ENDIF
  410 CONTINUE


C     GET GRAPHICS DISPLAY INFORMATION - NOT WITH THE PREDEFINED DATA

      IF(IFAIL.GT.0) THEN
        CALL XERROR('Input error in readinputfile',0,1,1)
        IERROR = IFAIL
        RETURN
      END IF
      CLOSE (IOFILI)
      RETURN

C     ERROR

  130 IF (LOGERR.GT.0) WRITE (LOGERR,5050) MOD(IOS,256)
	ierror = 99
      RETURN

 5000 FORMAT ('Setting X cood. of fire position to default ',F12.5)
 5001 format ('Opening a version ',i2,' file in normal mode')
 5002 format ('Not a compatible version ',2a8,2x,2i10)
 5003 format ('Too many lines in the main data file')
 5010 FORMAT ('Setting Y cood. of fire position to default ',F12.5)
 5020 FORMAT ('Setting Z cood. of fire position to default ',F12.5)
 5021 FORMAT ('The constant heat source (heatf) is in compartment ',i3,
     .        ' at ',3f12.5)
 5022 format (
     . 'Initial temperature outside of allowable range (-50 to +100)',
     . f5.2)

C     READ FORMAT LIST

 5030 FORMAT (A5,2X,I3,128A1)
 5050 FORMAT (' Error opening the input file = ',I6)

C     OUTPUT FOR OBJECTS

 5080 FORMAT (' Object no. ',I3,' X cood. set to ',F12.5)
 5090 FORMAT (' Object no. ',I3,' Y cood. set to ',F12.5)
 5100 FORMAT (' Object no. ',I3,' Z cood. set to ',F12.5)
 5101 FORMAT ('Not an allowed fire type ',i3)

      END

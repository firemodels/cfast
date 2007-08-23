      SUBROUTINE RESULT(TIME,ISW)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RESULT
C
C     Source File: RESULT.SOR
C
C     Functional Class:
C
C     Description:  Output the results of the simulation at the current time
!                RSLTLAY is the basic environment
!                RSLTFIR information on fires
!                RSLTTAR targets and walls - temperature, radiation and convective flux
!                RSLTSPRINK sprinkler and detector information
!                RSLTHALL track the nose of the gravity wave
!                RSLTSP species
C
C     Arguments: TIME  Current time (s)
C                PSW   What to output:  N=Normal printout, S=Species,
C                      F=vent Flows, P=Wall temp profiles, T=Tenability,
C                      W=Wall and target fluxes, C=compact printout,
C                      O=original format.
C                      These may be combined in a single call
C                ISW   1 if called from CFAST, 0 otherwise (only effects
C                      printout of object names -- only CFAST knows actual
C                      names, others just do it by numbers
C
C     Revision History:
C        Recreated:  7/7/1993 at 14:57 by RDP
C        Modified: 11/15/95 by GPF
C                  Added R option, to print results in colums to be used by
C                  CFAST-WEB or other graphical analysis tools.
C        Modified: 6/6/96 by GPF
C                  Added RSLTSPRINK to report results at sprinkler/detector
C                  locations
C        Modifed: 7/22/96 by GPF
C                  Added RESLTHALL to report results for halls
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

C*RE
      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
      IF (outputformat.gt.1) THEN
        WRITE (IOFILO,5000) TIME
        CALL RSLTLAY
        CALL RSLTFIR(ISW)
        CALL RSLTTAR(ISW,1)
        CALL RSLTSPRINK
        CALL RSLTHALL(TIME)
        CALL RSLTSP
        if(trace) then
            CALL RSLTFLWt (time)
        else
            call rsltflw (time)
        endif
!        IF (INDEX(PSW,'T').NE.0) CALL RSLTTEN
      ELSE IF (outputformat.eq.1) THEN
        WRITE (IOFILO,5000) TIME
        CALL RSLTCMP (iofilo)
      END IF

 5000 FORMAT (//,' Time = ',F8.1,' seconds.')
      END

      SUBROUTINE RSLTLAY

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTLAY
C
C     Source File: RESULT.SOR
C
C     Functional Class:
C
C     Description:  Output the 2 layer environment at the current time
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/12/1993 at 12:51 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"
      WRITE (IOFILO,5000)
      WRITE (IOFILO,5010)
      WRITE (IOFILO,5020)
      WRITE (IOFILO,5030)
      WRITE (IOFILO,5040)
      DO 10 I = 1, NM1
        ITARG = NTARG - NM1 + I
        IZZVOL = ZZVOL(I,UPPER)/VR(I)*100.D0+0.5D0
	IF (IZSHAFT(I).EQ.1) THEN
        	WRITE (IOFILO,5071) compartmentnames(I), 
     +		ZZTEMP(I,UPPER)-273.15,
     +		ZZVOL(I,UPPER),
     +      	ZZABSB(UPPER,I),ZZRELP(I)-PAMB(I),
     +      	ONTARGET(I), XXTARG(TRGNFLUXF,ITARG)
        ELSE
        	WRITE (IOFILO,5070) compartmentnames(i), 
     +		ZZTEMP(I,UPPER)-273.15, ZZTEMP(I,LOWER)-273.15,
     +      	ZZHLAY(I,LOWER), ZZVOL(I,UPPER),
     +      	IZZVOL, ZZABSB(UPPER,I),ZZABSB(LOWER,I),ZZRELP(I)-PAMB(I),
     +      	ONTARGET(I), XXTARG(TRGNFLUXF,ITARG)
        ENDIF
   10 CONTINUE
      RETURN

 5000 FORMAT (' ')
 5010 FORMAT (' Compartment',T16,'Upper',T26,'Lower',T36,'Inter.',
     +        T46,'Upper',T62,'Upper',T73,'Lower',T83,'Pressure',
     +        T95,'Ambient',T106,'Floor')
 5020 FORMAT (T16,'Temp.',T26,'Temp.',T36,'Height',T46,'Vol.',
     +        T62,'Absorb',T73,'Absorb',T95,'Target',T106,'Target')
 5030 FORMAT (T17,'(C)',T26,'(C)',T36,'(m)',T46,'(m^3)',T62,'(m^-1)',
     +        T73,'(m^-1)',T85,'(Pa)',T95,'(W/m^2)',T106,'(W/m^2)')
 5040 FORMAT (' ',113('-'))
 5070 FORMAT (1x,a13,1P2G10.4,1PG10.4,1X,1pg8.2,'(',I3,'%) ',
     +        1PG10.3,1X,1PG10.3,1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
 5071 FORMAT (1x,A13,1PG10.4,10(' '),10(' '),1X,1pg8.2,7(' '),
     +        1PG10.3,1X,10(' '),1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
      END
      SUBROUTINE RSLTFIR(ISW)

C
C     Routine:     RSLTFIR
C
C     Source File: RESULT.SOR
C
C     Description:  Output the fire environment at the current time
C
C     Arguments: ISW    Print switch for object fire printout
C
C     Revision History:
C        Created:  7/12/1993 at 12:51 by RDP
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "objects1.fi"
      include "objects2.fi"

      EXTERNAL LENGTH
      XX0 = 0.0D0
      WRITE (IOFILO,5000)
      IF (LFMAX.GT.0.AND.LFBT.GT.0.AND.LFBO.GT.0) THEN
        CALL FLAMHGT (FROOM(0),FQF(0),FAREA(0),FHEIGHT)
        WRITE (IOFILO,5010) 'Main', FEMS(0), FEMP(0), FQF(0),
     +      FHEIGHT, FQFC(0), FQF(0) - FQFC(0)
      END IF
      IF (NUMOBJL.NE.0) THEN
        DO 10 I = 1, NUMOBJL
          CALL FLAMHGT (FROOM(I),FQF(I),FAREA(I),FHEIGHT)
          IF (ISW.NE.0) THEN
            IF (OBJPNT(I).NE.0) THEN
              J = OBJPNT(I)
              WRITE (IOFILO,5010) OBJNIN(J)(1:LENGTH(OBJNIN(J))),
     +            FEMS(I), FEMP(I), FQF(I),
     +            FHEIGHT,FQFC(I),FQF(I)-FQFC(I),objmaspy(i),radio(i)
            END IF
          ELSE
            WRITE (IOFILO,5020) I, FEMS(I), FEMP(I), FQF(I),
     +          FHEIGHT,FQFC(I),FQF(I)-FQFC(I),objmaspy(i),radio(i)
          END IF
   10   CONTINUE
      END IF
      WRITE (IOFILO,'(A)') ' '
      DO 30 IR = 1, NM1
        XEMS = XX0
        XEMP = XX0
        XQF = XX0
        XQUPR = XX0
        XQLOW = XX0
        DO 20 I = 0, NUMOBJL
          IF (IR.EQ.FROOM(I)) THEN
            XEMS = XEMS + FEMS(I)
            XEMP = XEMP + FEMP(I)
            XQF = XQF + FQF(I)
            XQUPR = XQUPR + FQUPR(I)
            XQLOW = XQLOW + FQLOW(I)
          END IF
   20   CONTINUE
        XQF = XQF + FQDJ(IR)
        IF (XEMS+XEMP+XQF+XQUPR+XQLOW+FQDJ(IR).NE.XX0)
     +      WRITE (IOFILO,5030) compartmentnames(IR), XEMS, XEMP, XQF,
     +      XQUPR, XQLOW,
     +      FQDJ(IR)
   30 CONTINUE
      IF (FQDJ(N).NE.XX0) WRITE (IOFILO,5040) FQDJ(N)
      RETURN
 5000 FORMAT (//,' Fires',/,
     +    '0Compartment    Fire      Plume     Pyrol     ',
     +    'Fire      Flame     Fire in   Fire in   Vent      ',
     +    'Convec.   Radiat.   Pyrolysate  Trace',/,
     +    '                          Flow      Rate      ',
     +    'Size      Height    Upper     Lower     Fire',/,
     +    '                          (kg/s)    (kg/s)    ',
     +    '(W)       (m)       (W)       (W)       (W)       ',
     +    '  (W)       (W)       (kg)      (kg)' ,/,
     +    ' ',138('-'))
 5010 FORMAT (' ',14X,A8,2X,1P4G10.3,30X,1P3G10.3,2x,g10.3)
 5020 FORMAT (' ',13X,'Object ',I2,2X,1P4G10.3,30X,1P3G10.3,2x,g10.3)
 5030 FORMAT (' ',a14,10X,1P3G10.3,10X,1P3G10.3)
 5040 FORMAT ('  Outside',76X,1PG10.3)
      END
      SUBROUTINE RSLTSP

!
!     Routine:     RSLTSP
!
!     Functional Class:
!
!     Description:  Output the layer and wall species at the current time
!
!     Arguments: none
!
!     Revision History:
!        Created:  7/12/1993 at 12:51 by RDP
!        Modified May, 2007 to output trace species
!

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      LOGICAL SWL(4)
      INTEGER IWPTR(4)
      CHARACTER STYPE(NS)*10, SUNITS(NS)*11, CIOUT*255, CJOUT*255,
     +    LNAMES(2)*5, WTYPE(4)*10
      EXTERNAL LENGTH
      DATA LNAMES /'Upper', 'Lower'/
      DATA IWPTR /1, 3, 4, 2/
      DATA WTYPE /'HCl c', 'HCl f', 'HCl uw', 'HCl lw'/
      DATA SUNITS /'(%)', '(%)', '(%)', '(ppm)', '(ppm)', '(ppm)',
     +    '(%)', '(%)', '(1/m)', '(g-min/m3)', ' kg '/
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O',
     +    'OD', 'CT', ' TS'/
      IF (NLSPCT.NE.0) THEN
        DO 20 I = 1, NWAL
          SWL(I) = .FALSE.
          DO 10 J = 1, NM1
            SWL(I) = SWL(I) .OR. SWITCH(I,J)
   10     CONTINUE
   20   CONTINUE

        DO 80 LAYER = UPPER, LOWER
          WRITE (IOFILO,5050) LNAMES(LAYER)
          CIOUT = 'Compartment'
          CJOUT = ' '
          IC = 16
          DO 30 LSP = 1, NS
            IF (ACTIVS(LSP)) THEN
              WRITE (CIOUT(IC:IC+9),5000) STYPE(LSP)
              WRITE (CJOUT(IC:IC+9),5000) SUNITS(LSP)
              IC = IC + 11
            END IF
   30     CONTINUE
          IF (ACTIVS(6)) THEN
            DO 40 IW = 1, 4
              IF (SWL(IWPTR(IW))) THEN
                WRITE (CIOUT(IC:IC+9),5000) WTYPE(IW)
                WRITE (CJOUT(IC:IC+9),5000) '(mg/m^2)  '
                IC = IC + 10
              END IF
   40       CONTINUE
          END IF
          WRITE (IOFILO,5020) CIOUT(1:LENGTH(CIOUT))
          WRITE (IOFILO,5020) CJOUT(1:LENGTH(CJOUT))
          WRITE (IOFILO,5030) ('-',I = 1,IC)
          WRITE (CIOUT,5010)
          DO 70 I = 1, NM1
            WRITE (CIOUT,5060) compartmentnames(i)
            IC = 14
            DO 50 LSP = 1, NS
              WRITE (CIOUT(IC:IC+9),5040) TOXICT(I,LAYER,LSP)
              IC = IC + 11
   50       CONTINUE
            IF (ACTIVS(6)) THEN
              DO 60 IW = 1, 4
                IF (SWL(IWPTR(IW))) THEN
                  WRITE (CIOUT(IC:IC+9),5040) ZZWSPEC(I,IWPTR(IW))
                  IC = IC + 10
                END IF
   60         CONTINUE
            END IF
            WRITE (IOFILO,5020) CIOUT(1:LENGTH(CIOUT))
   70     CONTINUE
   80   CONTINUE
      END IF
      RETURN
 5000 FORMAT (A10)
 5010 FORMAT (' ')
 5020 FORMAT (' ',A)
 5030 FORMAT (' ',255A1)
 5040 FORMAT (1PG10.3)
 5050 FORMAT (//,' ',A5,' Layer Species',/)
 5060 FORMAT (A13)
      END

      SUBROUTINE RSLTFLW (time)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTFLW - normal flow field (see rsltflwt for total mass)
C
C     Source File: RESULT
C
C     Functional Class:
C
C     Description:  Output the vent flow at the current time
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/12/1993 at 12:51 by RDP
C        Modified: 2/10/97 by gpf
C                  use o(n) vent datastructures instead of o(n**2)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"
      CHARACTER CIOUT*8, CJOUT*12, OUTBUF*90
      DIMENSION FLOW(4)
      LOGICAL FIRST
      XX0 = 0.0D0
      WRITE (IOFILO,5000)

      DO 70 IRM = 1, N
        I = IRM
        FIRST = .TRUE.
        WRITE (CIOUT,'(A8)') compartmentnames(IRM)
        IF (IRM.EQ.N) CIOUT = 'Outside'

C     HORIZONTAL FLOW NATURAL VENTS

        DO 20 J = 1, N
          DO 10 K = 1, mxccv
            IIJK = IJK(I,J,K)
            IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
              IF (J.EQ.N) THEN
                WRITE (CJOUT,'(A1,1X,A7,A2,I1)') 'H', 'Outside', ' #', K
              ELSE
                WRITE (CJOUT,'(A1,1X,A4,I3,A2,I1)') 'H', 'Comp', J,
     +              ' #', K
              END IF
              IF(I.LT.J)THEN
                SUM1 = SS2(IIJK) + SA2(IIJK)
                SUM2 = SS1(IIJK) + SA1(IIJK)
                SUM3 = AA2(IIJK) + AS2(IIJK)
                SUM4 = AA1(IIJK) + AS1(IIJK)
               ELSE
                SUM1 = SS1(IIJK) + SA1(IIJK)
                SUM2 = SS2(IIJK) + SA2(IIJK)
                SUM3 = AA1(IIJK) + AS1(IIJK)
                SUM4 = AA2(IIJK) + AS2(IIJK)
              ENDIF
              IF (I.EQ.N) THEN
                CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,XX0,XX0)
              ELSE
                IF(I.LT.J)THEN
                  SUM5 = SAU2(IIJK)
                  SUM6 = ASL2(IIJK)
                 ELSE
                  SUM5 = SAU1(IIJK)
                  SUM6 = ASL1(IIJK)
                ENDIF
                CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,SUM5,SUM6)
              END IF
              IF (FIRST) THEN
                IF (I.NE.1) WRITE (IOFILO,5010)
                WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                FIRST = .FALSE.
              ELSE
                WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE

C     VERTICAL FLOW NATURAL VENTS

        DO 40 J = 1, N
          IF (NWV(I,J).NE.0.OR.NWV(J,I).NE.0) THEN
            IF (J.EQ.N) THEN
              WRITE (CJOUT,'(A1,1X,A7)') 'V', 'Outside'
            ELSE
              WRITE (CJOUT,'(A1,1X,A4,I3)') 'V', 'Comp', J
            END IF
            DO 30 II = 1, 4
              FLOW(II) = XX0
   30       CONTINUE
            IF (VMFLO(J,I,UPPER).GE.XX0) FLOW(1) = VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,UPPER).LT.XX0) FLOW(2) = -VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,LOWER).GE.XX0) FLOW(3) = VMFLO(J,I,LOWER)
            IF (VMFLO(J,I,LOWER).LT.XX0) FLOW(4) = -VMFLO(J,I,LOWER)
            CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),XX0,XX0)
            IF (FIRST) THEN
              IF (I.NE.1) WRITE (IOFILO,5010)
              WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
              FIRST = .FALSE.
            ELSE
              WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
            END IF
          END IF
   40   CONTINUE

C     MECHANICAL VENTS

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
              WRITE (CJOUT,'(A1,1X,A4,I3)') 'M', 'Node', INODE
              DO 50 IIi = 1, 4
                FLOW(IIi) = XX0
   50         CONTINUE
              IF (HVEFLO(UPPER,I).GE.XX0) FLOW(1) = HVEFLO(UPPER,I)
              IF (HVEFLO(UPPER,I).LT.XX0) FLOW(2) = -HVEFLO(UPPER,I)
              IF (HVEFLO(LOWER,I).GE.XX0) FLOW(3) = HVEFLO(LOWER,I)
              IF (HVEFLO(LOWER,I).LT.XX0) FLOW(4) = -HVEFLO(LOWER,I)
              CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),XX0,XX0
     +            )
              IF (FIRST) THEN
                IF (I.NE.1) WRITE (IOFILO,5010)
                WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                FIRST = .FALSE.
              ELSE
                WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
              END IF
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

 5000 FORMAT (//,' Flow Through Vents (kg/s)',/,
     +    '0To             Through        ',
     +    '      Upper Layer           ','    Lower Layer           ',
     +    'Mixing       Mixing',/,' Compartment    Vent             ',
     +   2('Inflow       Outflow      '),'To Upper     To Lower',/,' ',
     +    104('-'))
 5010 FORMAT (' ')
 5020 FORMAT (' ',A7,8X,A12,1X,A)
      END

      SUBROUTINE RSLTFLWt (time)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTFLWt - total mass through vents
C
C     Source File: RESULT
C
C     Functional Class:
C
C     Description:  Output the vent flow at the current time
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/12/1993 at 12:51 by RDP
C        Modified: 2/10/97 by gpf
C                  use o(n) vent datastructures instead of o(n**2)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"
      CHARACTER CIOUT*8, CJOUT*12, OUTBUF*90
      DIMENSION FLOW(6)
      LOGICAL FIRST
      XX0 = 0.0D0
      WRITE (IOFILO,5000)

      DO 70 IRM = 1, N
        I = IRM
        FIRST = .TRUE.
        WRITE (CIOUT,'(A8)') compartmentnames(IRM)
        IF (IRM.EQ.N) CIOUT = 'Outside'

C     HORIZONTAL FLOW NATURAL VENTS


C     VERTICAL FLOW NATURAL VENTS


C     MECHANICAL VENTS

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
              WRITE (CJOUT,'(A1,1X,A4,I3)') 'M', 'Node', INODE
              DO 50 IIi = 1, 4
                FLOW(IIi) = XX0
   50         CONTINUE
              IF (HVEFLOt(UPPER,I).GE.XX0) FLOW(1) = HVEFLOt(UPPER,I)
              IF (HVEFLOt(UPPER,I).LT.XX0) FLOW(2) = -HVEFLOt(UPPER,I)
              IF (HVEFLOt(LOWER,I).GE.XX0) FLOW(3) = HVEFLOt(LOWER,I)
              IF (HVEFLOt(LOWER,I).LT.XX0) FLOW(4) = -HVEFLOt(LOWER,I)
              flow(5) = (abs(tracet(upper,i))+ abs(tracet(lower,i)))
     +                / tradio
              CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),
     +                    flow(5),xx0)
              IF (FIRST) THEN
                IF (I.NE.1) WRITE (IOFILO,5010)
                WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                FIRST = .FALSE.
              ELSE
                WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
              END IF
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

 5000 FORMAT (//,' Total mass flow through vents (kg)',/,
     +    '0To             Through        ',
     +    '      Upper Layer           ','    Lower Layer           ',
     +    'Trace Species',/,' Compartment    Vent             ',
     +   2('Inflow       Outflow      '),'Relative to Total Release',/,
     +    ' ', 104('-'))
 5010 FORMAT (' ')
 5020 FORMAT (' ',A7,8X,A12,1X,A)
      END
      
      SUBROUTINE RSLTCMP (iounit)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTCMP
C
C     Description:  Output a compressed output for 80 column screens
C
C     Arguments: none
C
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "objects1.fi"
      include "objects2.fi"
      XX0 = 0.0D0
      WRITE (iounit,5000)
      WRITE (iounit,5010)
      DO 20 IR = 1, NM1
        XEMP = XX0
        XQF = XX0
        DO 10 I = 0, NUMOBJL
          IF (IR.EQ.FROOM(I)) THEN
            XEMP = XEMP + FEMP(I)
            XQF = XQF + FQF(I)
          END IF
   10   CONTINUE
        XQF = XQF + FQDJ(IR)
        IF (IZSHAFT(IR).EQ.1) THEN
        WRITE (iounit,5031) IR, ZZTEMP(IR,UPPER)-273.15,
     +      XEMP, XQF, ZZRELP(IR) - PAMB(IR),
     +      ONTARGET(IR)
        ELSE
        WRITE (iounit,5030) IR, ZZTEMP(IR,UPPER)-273.15, 
     +      ZZTEMP(IR,LOWER)-273.15,
     +      ZZHLAY(IR,LOWER), XEMP, XQF, ZZRELP(IR) - PAMB(IR),
     +      ONTARGET(IR)
        ENDIF
   20 CONTINUE
      WRITE (iounit,5020) FQDJ(N)
      RETURN

 5000 FORMAT (' ')
 5010 FORMAT (
     +    ' Compartment   Upper   Lower   Inter.  Pyrol     ',
     +    'Fire      Pressure  Ambient',/,
     +    '               Temp.   Temp.   Height  Rate      ',
     +    'Size                Target',/,
     +    '               (C)     (C)     (m)     (kg/s)    ',
     +    '(W)       (Pa)      (W/m^2)',/,' ',77('-'))
 5020 FORMAT ('  Outside',39X,1PG10.3)
 5030 FORMAT (I5,7X,2F8.1,2X,1PG8.2,1P4G10.3)
 5031 FORMAT (I5,7X,F8.1,8(' '),2X,8(' '),1P4G10.3)
      END

      SUBROUTINE RSLTTAR(ISW,ITPRT)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTTAR
C
C     Source File: RESULT.SOR
C
C     Functional Class:
C
C     Description:  Output the temperatures and fluxes on surfaces and targets
C                   at the current time
C
C     Arguments: ISW   1 if called from CFAST, 0 otherwise (effects
C                      printout of object names -- only CFAST knows actual
C                      names, others just do it by numbers
C                ITPRT 1 if target printout specifically called for,
C                      0 otherwise
C
C     Revision History:
C        Created:  10/10/94 by gpf
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"

      INTEGER IWPTR(4)
      EXTERNAL LENGTH
      DATA IWPTR /1, 3, 4, 2/
      XX0 = 0.0D0
      X100 = 100.0D0
      IF ((ITPRT.EQ.0.AND.NTARG.LE.NM1).OR.NTARG.EQ.0) RETURN
      WRITE (IOFILO,5000)
      DO 10 I=1,NM1
        ITARG = NTARG-NM1+I
        RTOTAL = XXTARG(TRGTFLUXF,ITARG)
        FTOTAL = QTFFLUX(ITARG,1)
        WTOTAL = QTWFLUX(ITARG,1)
        GTOTAL = QTGFLUX(ITARG,1)
        CTOTAL = QTCFLUX(ITARG,1)
        IF (RTOTAL.NE.XX0) THEN
          WRITE (iofilo,5010) compartmentnames(I),
     +        ((TWJ(1,I,IWPTR(IW))-273.15),IW=1,4),
     +        TWJ(1,I,2)-273.15,RTOTAL,FTOTAL/RTOTAL*X100,
     +        WTOTAL/RTOTAL*X100,
     +        GTOTAL/RTOTAL*X100,CTOTAL/RTOTAL*X100
        ELSE
          WRITE (iofilo,5010) compartmentnames(I),
     +        (TWJ(1,I,IWPTR(IW))-273.15,IW=1,4),
     +        TWJ(1,I,2)-273.15
        END IF
        IF (NTARG.GT.NM1) THEN
          DO 20 ITARG = 1, NTARG-NM1
            IF (IXTARG(TRGROOM,ITARG).EQ.I) THEN
              TTTEMP = XXTARG(TRGTEMPF,ITARG)
              RTOTAL = XXTARG(TRGTFLUXF,ITARG)
              FTOTAL = QTFFLUX(ITARG,1)
              WTOTAL = QTWFLUX(ITARG,1)
              GTOTAL = QTGFLUX(ITARG,1)
              CTOTAL = QTCFLUX(ITARG,1)
              IF (RTOTAL.NE.XX0) THEN
                WRITE(IOFILO,5030)ITARG,TTTEMP-273.15,RTOTAL,
     +              FTOTAL/RTOTAL*X100,
     +              WTOTAL/RTOTAL*X100,GTOTAL/RTOTAL*X100,
     +              CTOTAL/RTOTAL*X100
              ELSE
                WRITE(IOFILO,5030)ITARG,TTTEMP-273.15
              END IF
            END IF
   20     CONTINUE
        END IF
   10 CONTINUE
      RETURN
5000  FORMAT (//,' Surfaces and Targets',/,
     +    '0Compartment    Ceiling   Up wall   Low wall  '
     +    'Floor    Target    Target   Flux To      Fire      '
     +    'Surface   Gas',/,
     +    '                Temp.     Temp.     Temp.     ',
     +    'Temp.              Temp.    Target       Rad.      ',
     +    'Rad.      Rad.      Convect.',/,
     +    '                (C)       (C)       (C)       ',
     +    '(C)                (C)      (W/m^2)      (%)       ',
     +    '(%)       (%)       (%)',/,1X,124('-'))
5010  FORMAT (1x,a14,1P4G10.4,1X,'Floor',2X,1PG10.3,1X,1PG10.4,0PF7.1,
     +    3(3X,F7.1))
5030  FORMAT (55X,I4,4X,1PG10.3,1X,1PG10.4,0PF7.1,3(3X,F7.1))
      END
      SUBROUTINE RSLTSPRINK

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTSPRINK
C
C     Source File: RESULT.SOR
C
C     Functional Class:
C
C     Description:  Output the conditions of and at a sprinkler location
C                   (temperature, velocities etc) at the current time
C
C     Revision History:
C        Created:  5/24/96
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      CHARACTER*5 CTYPE
      CHARACTER*3 CACT

      IF(NDTECT.EQ.0)RETURN
      write(iofilo,5000)
 5000 format(//' Sensors',/,
     .  '0                             Sensor    ',
     .  '                       Smoke'/
     .  ' Number  Compartment   Type   Temp (C)   Activated',
     .  '       Temp (C)   Vel (M/S)'/
     .  ' ---------------------------------',
     .  '-------------------------------------------')
      CJETMIN = 0.10D0
      DO 10 I = 1, NDTECT
        IROOM = IXDTECT(I,DROOM)

        ZDETECT = XDTECT(I,DZLOC)
        IF(ZDETECT.GT.ZZHLAY(IROOM,LOWER))THEN
          TLAY = ZZTEMP(IROOM,UPPER)
         ELSE
          TLAY = ZZTEMP(IROOM,LOWER)
        ENDIF

        TJET = MAX(XDTECT(I,DTJET),TLAY)-273.15
        VEL = MAX(XDTECT(I,DVEL),CJETMIN)
        TLINK =  XDTECT(I,DTEMP)-273.15

        ITYPE = IXDTECT(I,DTYPE)
        IF(ITYPE.EQ.SMOKED)THEN
          CTYPE = 'SMOKE'
         ELSEIF(ITYPE.EQ.HEATD)THEN
          CTYPE = 'HEAT'
         ELSE
          CTYPE = 'OTHER'
        ENDIF
        CACT = 'NO'
        IF(IXDTECT(I,DACT).EQ.1)CACT = 'YES'
        WRITE(IOFILO,5010)I,IROOM,CTYPE,TLINK,CACT,TJET,VEL
 5010   FORMAT(T2,I2,T10,I3,T24,A5,T31,1PE10.3,T42,A3,
     .        T58,1PE10.3,T69,1PE10.3)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE RSLTHALL(TIME)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RSLTHALL
C
C     Source File: RESULT.SOR
C
C     Functional Class:
C
C     Description:  Output the conditions for each hall
C
C     Revision History:
C        Created:  7/24/96
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"


      NHALLS = 0
      DO 10 I = 1, NM1
        IF(IZHALL(I,IHROOM).EQ.1)NHALLS = NHALLS + 1
   10 CONTINUE
      IF(NHALLS.EQ.0)RETURN
      WRITE(IOFILO,5000)
 5000 FORMAT (//,' Hall Flow',//
     .' Compartment  Start Time     Velocity       ',
     .'Depth        Distance'/
     .'                 (s)          (m/s)          ',
     .'(m)            (m)'/
     .'---------------------------------------------',
     .'--------------------')

      DO 20 I = 1, NM1
        IF(IZHALL(I,IHROOM).EQ.0)GO TO 20
        TSTART = ZZHALL(I,IHTIME0)
        VEL = ZZHALL(I,IHVEL)
        DEPTH = ZZHALL(I,IHDEPTH)
        DIST = ZZHALL(I,IHDIST)
        IF(DIST.GT.ZZHALL(I,IHMAXLEN))DIST = ZZHALL(I,IHMAXLEN)
        WRITE(IOFILO,30)I,TSTART,VEL,DEPTH,DIST
   30   FORMAT(4x,I2,7x,1PG10.3,5x,1PG10.3,3x,1PG10.3,5x,1PG10.3)
   20 CONTINUE

      RETURN
      END

      SUBROUTINE outinitial(ISW)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     outinitial
C
C     Source File: result.f
C
C     Description:  Output initial test case description
C
C     Arguments: ISW   Print switch for restart option, 1=Print
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "iofiles77.fi"

      CHARACTER CHKSUM*8
      EXTERNAL LENGTH
      integer imajor, iminor, iminorrev

      call splitversion(version,imajor,iminor,iminorrev)

      IF (.not.HEADER) THEN
        if (iminorrev.ge.10) then
            WRITE (iofilo,10) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3), MPSDAT(1), MPSDAT(2), MPSDAT(3)
        else
            WRITE (iofilo,20) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3), MPSDAT(1), MPSDAT(2), MPSDAT(3)
        end if
      end if

      WRITE (IOFILO,5000) trim(inputfile), trim(title)
      IF (outputformat.gt.1) THEN
        CALL OUTOVER
        CALL OUTAMB
        CALL OUTCOMP
        CALL OUTVENT
        CALL OUTTHE
        CALL OUTTARG (1)
        CALL OUTFIRE
        CALL OUTOBJ
      END IF

      RETURN
C
 5000 FORMAT (' Data file is ',A,'    Title is ',A)
 10   FORMAT (' CFAST Version ',i1,'.',i1,'.',I2,
     +        ' built ',I4.4,'/',I2.2,
     +        '/',I2.2,', run ',I4.4,'/',I2.2,'/',I2.2,/)
 20   FORMAT (' CFAST Version ',i1,'.',i1,'.',I1,
     +        ' built ',I4.4,'/',I2.2,
     +        '/',I2.2,', run ',I4.4,'/',I2.2,'/',I2.2,/)
      END

      SUBROUTINE OUTOVER

!
!     Routine:     OUTOVER
!
!     Source File: NPUTO.SOR
!
!     Functional Class:  
!
!     Description:  Output initial test case overview
!
!     Arguments: none
!
!     Revision History:
!        Created:  7/2/1993 at 14:50 by RDP
!

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"
      CHARACTER CHJET(4)*7, CJBUF*51
      DATA CHJET /'off', 'ceiling', 'wall', 'all'/

      WRITE (IOFILO,5000) 
      WRITE (IOFILO,5010) NM1, NVENTS, NVVENT, NEXT
      WRITE (IOFILO,5020) NSMAX, LPRINT, LDIAGO, ldiago, lcopyss
      IF (CJETON(5)) THEN
        IF (CJETON(1)) THEN
          IF (CJETON(3)) THEN
            JPOS = 3
          ELSE
            JPOS = 1
          END IF
        ELSE IF (CJETON(3)) THEN
          JPOS = 2
        END IF
        WRITE (CJBUF,'(''on for '',A7)') CHJET(JPOS+1)
      ELSE
        WRITE (CJBUF,'(''off for all surfaces.'')')
      END IF
      WRITE (IOFILO,5030) CJBUF
      IF (NDUMPR.GT.0.AND.DUMPF.NE.' ') WRITE (IOFILO,5040) DUMPF
      RETURN
C
 5000 FORMAT (//,' OVERVIEW',/)
 5010 FORMAT ('0Compartments    Doors, ...    Ceil. Vents, ...    ',
     +    'MV Connects',/,'0',I4,12X,I4,10X,I4,17X,I4)
 5020 FORMAT ('0Simulation    ',' Output     ','History     ',
     + 'Smokeview',2x'Spreadsheet',/,'    Time       ',4('Interval    '),
     + /,3x,' (s)          ',4('(s)         '),//,' ',
     + I6,6X,4(I6,6X))
 5030 FORMAT ('0Ceiling jet is ',A)
 5040 FORMAT (' History file is ',A)
      END

      SUBROUTINE OUTAMB

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTAMB
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case ambient conditions
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/2/1993 at 14:50 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

      WRITE (IOFILO,5000) TA-273.15, PA + POFSET, EXTA-273.15, 
     +    EXPA + POFSET, SAL, 
     +    WINDV, WINDRF, WINDPW
      RETURN

 5000 FORMAT (//,' AMBIENT CONDITIONS',//,' Interior       ',
     +    'Interior       ','Exterior       ','Exterior       ',
     +    'Station        ','Wind           ','Wind           ',
     +    'Wind           ',/,' ','Temperature    ','Pressure       ',
     +    'Temperature    ','Pressure       ','Elevation      ',
     +    'Speed          ','Ref. Height    ','Power',/,' ',
     +    '(C)            ','(Pa)           ','(C)             ',
     +    '(Pa)          ','(m)            ','(m/s)           ','(m)',
     +    //,' ',2(F7.0,8X,F9.0,6X),F7.2,6X,2(F7.1,8X),F7.2)
      END

      SUBROUTINE OUTCOMP

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTCOMP
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case geometry
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/2/1993 at 14:50 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

      WRITE (IOFILO,5000)
      DO 10 I = 1, NM1
        WRITE (IOFILO,5010) I, compartmentnames(i), BR(I), DR(I),
     +      HR(I), AR(I), VR(I), HRP(I), HFLR(I)
   10 CONTINUE
      RETURN
 5000 FORMAT (//,' COMPARTMENTS',//,' Compartment  ',
     +    'Name           ',
     +    'Width     ',
     +    'Depth     ','Height    ','Area      ','Volume    ',
     +    'Ceiling   ','Floor     ',/,' ',78X,'Height    ','Height    ',
     +    /,' ',29X,3('(m)',7X),'(m^2)     ','(m^3)      ',2('(m)',7X),/,
     +    ' ',96('-'))
 5010 FORMAT (' ',I5,8x,a13,7(F7.2,3X))
      END

      SUBROUTINE OUTVENT

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTVENT
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case vent connections
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/2/1993 at 14:50 by RDP
C       Modified:  2/8/1997 by gpf changed write statments to use new o(n) vent
C                  flow variables
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"
      CHARACTER CIOUT*8, CJOUT*14, CSOUT*6
      LOGICAL FIRST

C     HORIZONTAL FLOW VENTS

      IF (NVENTS.EQ.0) THEN
        WRITE (IOFILO,5000)
      ELSE
        WRITE (IOFILO,5010)
        DO 30 I = 1, NM1
          DO 20 J = I + 1, N
            DO 10 K = 1, 4
              WRITE (CJOUT,'(a14)') compartmentnames(J)
              IF (J.EQ.N) CJOUT = ' Outside'
              IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
                IIJK = IJK(I,J,K)
                WRITE (IOFILO,5020) compartmentnames(I), CJOUT, K,
     +              BW(IIJK), HL(IIJK), 
     +              HH(IIJK), HLP(IIJK), HHP(IIJK), (HHP(IIJK)-
     +              HLP(IIJK)) * BW(IIJK)
              END IF
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
      END IF

C     VERTICAL FLOW VENTS

      IF (NVVENT.EQ.0) THEN
        WRITE (IOFILO,5030)
      ELSE
        WRITE (IOFILO,5040)
        DO 50 I = 1, N
          DO 40 J = 1, N
            IF (NWV(I,J).NE.0) THEN
              WRITE (CIOUT,'(I5,3X)') I
              IF (I.EQ.N) CIOUT = ' Outside'
              WRITE (CJOUT,'(I5,3X)') J
              IF (J.EQ.N) CJOUT = ' Outside'
              CSOUT = 'Round'
              IF (VSHAPE(I,J).EQ.2) CSOUT = 'Square'
              IF (J.LT.N) THEN
                HRX = HR(J)
                HRPX = HRP(J)
              ELSE
                HRX = HRL(I)
                HRPX = HRL(I)
              END IF
              WRITE (IOFILO,5050) CIOUT, CJOUT, CSOUT, VVAREA(I,J), HRX,
     +            HRPX
            END IF
   40     CONTINUE
   50   CONTINUE
      END IF

C     MECHANICAL VENTS

      IF (NNODE.EQ.0.AND.NEXT.EQ.0) THEN
        WRITE (IOFILO,5060)
      ELSE

C     FANS

        WRITE (IOFILO,5120)
        DO 100 ISYS = 1, NHVSYS
          FIRST = .TRUE.
          DO 90 IBR = 1, NBR
            IF (IZHVBSYS(IBR).EQ.ISYS) THEN
              IF (NF(IBR).NE.0) THEN
                CALL CHKEXT(NA(IBR),IRM,IEXT)
                IF (IRM.GE.1.AND.IRM.LE.N) THEN
                  WRITE (CIOUT,'(A4,I3)') 'Comp', IRM
                  IF (IRM.EQ.N) CIOUT = 'Outside'
                  WRITE (CJOUT,'(A4,I3)') 'Node', NA(IBR)
                  IF (FIRST) THEN
                    WRITE (IOFILO,5100) ISYS, CIOUT, HVELXT(IEXT), 
     +                  CJOUT, HVGHT(NA(IBR)), AREXT(IEXT)
                    FIRST = .FALSE.
                  ELSE
                    WRITE (IOFILO,5110) CIOUT, HVELXT(IEXT), CJOUT, 
     +                  HVGHT(NA(IBR)), AREXT(IEXT)
                  END IF
                END IF
                IF (FIRST) THEN
                  WRITE (IOFILO,5130) ISYS, 'Node', NA(IBR), 
     +                HVGHT(NA(IBR)), 'Node', NE(IBR), HVGHT(NE(IBR)), 
     +                NF(IBR), HMIN(NF(IBR)), HMAX(NF(IBR)), (
     +                HVBCO(NF(IBR),J),J = 1,NFC(NF(IBR)))
                  FIRST = .FALSE.
                ELSE
                  WRITE (IOFILO,5140) 'Node', NA(IBR), HVGHT(NA(IBR)), 
     +                'Node', NE(IBR), HVGHT(NE(IBR)), NF(IBR), 
     +                HMIN(NF(IBR)), HMAX(NF(IBR)), (HVBCO(NF(IBR),J),J
     +                = 1,NFC(NF(IBR)))
                END IF
                CALL CHKEXT(NE(IBR),IRM,IEXT)
                IF (IRM.GE.1.AND.IRM.LE.N) THEN
                  WRITE (CIOUT,'(A4,I3)') 'Node', NE(IBR)
                  WRITE (CJOUT,'(A4,I3)') 'Comp', IRM
                  IF (IRM.EQ.N) CJOUT = 'Outside'
                  IF (FIRST) THEN
                    WRITE (IOFILO,5100) ISYS, CIOUT, HVGHT(NE(IBR)), 
     +                  CJOUT, HVELXT(IEXT), AREXT(IEXT)
                    FIRST = .FALSE.
                  ELSE
                    WRITE (IOFILO,5110) CIOUT, HVGHT(NE(IBR)), CJOUT, 
     +                  HVELXT(IEXT), AREXT(IEXT)
                  END IF
                END IF
              END IF
            END IF
   90     CONTINUE
  100   CONTINUE
      END IF
      RETURN

 5000 FORMAT (//,' VENT CONNECTIONS',//,' There are no horizontal',
     +    ' natural flow connections')
 5010 FORMAT (//,' VENT CONNECTIONS',//,' Horizontal Natural Flow',
     +    ' Connections (Doors, Windows, ...)',//,' From           ',
     +    'To             ','Vent      ','Width     ','Sill      ',
     +    'Soffit    ','Abs.      ','Abs.      ','Area',/,' ',
     +    'Compartment    ','Compartment    ','Number    ',10X,
     +    'Height    ','Height    ','Sill      ','Soffit',/,' ',40X,5(
     +    '(m)       '),1('(m^2)',5X),/,' ',100('-'))
 5020 FORMAT (' ',a14,1X,A14,I3,5X,6(F7.2,3X))
 5030 FORMAT (//,' There are no vertical natural flow connections')
 5040 FORMAT (//,' Vertical Natural Flow Connections (Ceiling, ...)',//,
     +    ' Top            Bottom         Shape     Area      ',
     +    'Relative  Absolute',/,' ',
     +    'Compartment    Compartment                        ',
     +    'Height    Height',/,' ',40X,'(m^2)     ',2('(m)       '),/,
     +    ' ',70('-'))
 5050 FORMAT (' ',A8,7X,A8,7X,A6,2X,3(F7.2,3X))
 5060 FORMAT (//,' There are no mechanical flow connections')
 5070 FORMAT (//' Mechanical Flow Connections (Fans, Ducts, ...)',//,
     +    ' Connections and Ducts',//,' System    ',
     +    'From           From      To             To        Length',
     +    '    Area      Rough',/,' ',
     +    '                         Elev.                    Elev.',/,
     +    ' ',25X,'(m)                      (m)       (m)   ',
     +    '    (m^2)     (mm)',/,' ',86('-'))
 5080 FORMAT (' ',I4,6X,A4,I3,5X,F7.2,6X,A4,I3,5X,4(F7.2,3X))
 5090 FORMAT (' ',10X,A4,I3,5X,F7.2,6X,A4,I3,5X,4(F7.2,3X))
 5100 FORMAT (' ',I4,6X,A7,5X,F7.2,6X,A7,5X,F7.2,13X,F7.2)
 5110 FORMAT (' ',10X,A7,5X,F7.2,6X,A7,5X,F7.2,13X,F7.2)
 5120 FORMAT (//,' Fans',//,' System    ',
     +    'From           From      To             To        Fan   ',
     +    '    Minimum   Maximum    Fan Curve',/,' ',
     +    '                         Elev.                    Elev.',
     +    '     Number',/,' ',25X,
     +    '(m)                      (m)             ',
     +    '    (Pa)      (Pa)',/,' ',100('-'))
 5130 FORMAT (' ',I4,6X,A4,I3,5X,F7.2,6X,A4,I3,5X,F7.2,6X,I3,6X,2(F7.2,3
     +    X),1P5G10.2)
 5140 FORMAT (' ',10X,A4,I3,5X,F7.2,6X,A4,I3,5X,F7.2,6X,I3,6X,2(F7.2,3X
     +    ),1P5G10.2)
 5150 FORMAT (' ')
      END

      SUBROUTINE CHKEXT(IND,IRM,IEXT)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CHKEXT
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Check if an HVAC node is a connection to an external room
C
C     Arguments: IND   Node number to check
C                IRM   Room number if node is an external connection
C                IEXT  External node number is node is an external connection
C
C     Revision History:
C        Created:  7/2/1993 at 14:50 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      DO 10 I = 1, NEXT
        IF (HVNODE(2,I).EQ.IND) THEN
          IEXT = I
          IRM = HVNODE(1,I)
          RETURN
        END IF
   10 CONTINUE
      IRM = 0
      IEXT = 0
      RETURN
      END

      SUBROUTINE OUTTHE

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTTHE
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case thermal properties
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/2/1993 at 14:50 by RDP
C        Modified: 5/2/1995 at 15:37 by RWP:
C                  Print loop modification for new internal data structure
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "thermp.fi"
      CHARACTER WALL(4)*7
      DATA WALL /'ceiling', 'floor', 'wall', 'wall'/

C     CHECK TO SEE IF ANY HEAT TRANSFER IS ON

      DO 20 I = 1, NM1
        DO 10 J = 1, NWAL
          IF (SWITCH(J,I).AND.CNAME(J,I).NE.' ') GO TO 30
   10   CONTINUE
   20 CONTINUE
      WRITE (IOFILO,5000)
      RETURN

C     SOME SURFACES ARE ON, DO THE PRINTOUT OF THE SURFACES

   30 WRITE (IOFILO,5010)
      DO 40 I = 1, NM1
        WRITE (IOFILO,5020) compartmentnames(I), CNAME(1,I), CNAME(3,I),
     +                      CNAME(2,I)
   40 CONTINUE

C     PRINT OUT THE PROPERTIES OF THE MATERIALS USED

   60 WRITE (IOFILO,5030) THRMFILE
      DO 80 I = 1, MAXCT
         WRITE (IOFILO,5040) NLIST(I), LFKW(1,I), LCW(1,I), LRW(1,I), 
     +      LFLW(1,I), LEPW(I), (LHCLBF(K,I),K = 1,5) 
         DO 70 J = 2, LNSLB(I)
            WRITE (IOFILO,5050) LFKW(J,I), LCW(J,I), LRW(J,I), 
     +          LFLW(J,I)
   70    CONTINUE
   80 CONTINUE
      WRITE (IOFILO,5060)
 5000 FORMAT (//,' Heat transfer for all surfaces is turned off')
 5010 FORMAT (//,' THERMAL PROPERTIES',//,' ',
     +    'Compartment    Ceiling      Wall         Floor',/,' ',70('-')
     +    )
 5020 FORMAT (' ',a13,3(A10,3X))
 5030 FORMAT (//,' Thermal data base used: ',A20,//,' Name',4X,
     +    'Conductivity',1X,'Specific heat',5X,'Density',5X,'Thickness',
     +    3X,'Emissivity',16X,'HCL B''s (1->5)')
 5040 FORMAT (' ',A8,1P5G13.3,5E10.2)
 5050 FORMAT (' ',8X,1P4G13.3)
 5060 FORMAT (' ')

      END

      SUBROUTINE OUTFIRE

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTFIRE
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case main fire specification
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/7/1993 at 14:48 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      CHARACTER CBUF*255, STYPE(NS)*5, FTYPE(0:4)*13
      EXTERNAL LENGTH
      DATA FTYPE /'Undefined', 'Unconstrained', 'Constrained', 
     +    'Pool Fire', 'Furniture'/
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O',
     +    'OD', 'CT', 'TS'/

      WRITE (IOFILO,5020)
      IF (LFMAX.GT.0.AND.LFBT.GT.0.AND.LFBO.GT.0) THEN
        WRITE (IOFILO,5030) 'Main Fire'
        WRITE (IOFILO,5040) LFBO, FTYPE(LFBT), FPOS, RELHUM * 100., 
     +      LIMO2 * 100., TE
        WRITE (CBUF,5050)
        IF (LFBT.EQ.1) THEN
          IS = 51
        ELSE
          WRITE (CBUF(51:110),5060)
          IS = 111
        END IF
        DO 10 LSP = 1, NS
          IF (ACTIVS(LSP).AND.ALLOWED(LSP)) THEN
            CBUF(IS:IS+9) = STYPE(LSP)
            IS = IS + 10
          END IF
   10   CONTINUE
        WRITE (IOFILO,'(3X,A)') CBUF(1:LENGTH(CBUF))
        WRITE (IOFILO,5000) ('(kg/kg)',I = 1,(IS-51)/10)
        WRITE (IOFILO,5010) ('-',I = 1,IS-1)
        DO 30 I = 1, LFMAX
          WRITE (CBUF,5070) TFIRED(I), BFIRED(I), HOCBMB(I), QFIRED(I),
     +        HFIRED(I)
          IF (LFBT.EQ.1) THEN
            IS = 41
          ELSE
            WRITE (CBUF(51:110),5080) CCO2(I), COCO2(I), HCRATIO(I), 
     +          OCRATI(I), HCNF(I), HCLF(I)
            IS = 111
          END IF
          DO 20 LSP = 1, NS
            IF (ACTIVS(LSP).AND.ALLOWED(LSP)) THEN
              WRITE (CBUF(IS:),5080) MPRODR(I,LSP)
              IS = IS + 10
            END IF
   20     CONTINUE
          WRITE (IOFILO,'(1X,A)') CBUF(1:LENGTH(CBUF))
   30   CONTINUE
      END IF
      RETURN
 5000 FORMAT ('   (s)       (kg/s)    (J/kg)    (W)       (m)       ',15
     +    (A7,3X))
 5010 FORMAT (' ',255A1)
 5020 FORMAT (//,' FIRES')
 5030 FORMAT ('0Name: ',A,//,' Compartment    Fire Type    ',
     +    '   Position (x,y,z)     Relative    Lower O2',
     +    '    Pyrolysis',/,' ',52X,
     +    'Humidity    Limit       Temperature')
 5040 FORMAT (I5,11X,A13,3(F7.2),F7.1,6X,F7.2,5X,F7.0,//)
 5050 FORMAT ('Time      Fmass     Hcomb     Fqdot     Fhigh     ')
 5060 FORMAT ('C/CO2     CO/CO2    H/C       O/C       HCN       ',
     +    'HCL       ')
 5070 FORMAT (F7.0,3X,1P4G10.2)
 5080 FORMAT (1P10G10.2)
      END

      SUBROUTINE OUTOBJ

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTOBJ
C
C     Source File: NPUTO.SOR
C
C     Functional Class:  
C
C     Description:  Output initial test case object fire specification
C
C     Arguments: none
C
C     Revision History:
C        Created:  7/7/1993 at 14:48 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"
      include "objects1.fi"
      include "objects2.fi"
      CHARACTER CBUF*255, STYPE(NS)*5, FTYPE(0:4)*13
      EXTERNAL LENGTH
      DATA FTYPE /'Undefined', 'Unconstrained', 'Constrained', 
     +    'Pool Fire', 'Furniture'/
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O',
     +    'OD', 'CT', 'TS'/

      IF (NUMOBJL.GT.0) THEN
        DO 40 IO = 1, MXOIN
          IF (OBJPNT(IO).NE.0) THEN
            J = OBJPNT(IO)
            NNV = OBJLFM(J)
            WRITE (IOFILO,5020) OBJNIN(J)(1:LENGTH(OBJNIN(J))), J
            WRITE (IOFILO,5030) compartmentnames(OBJRM(J)),
     +		  FTYPE(OBJTYP(J)), 
     +          OBJPOS(1,J), OBJPOS(2,J), OBJPOS(3,J), RELHUM * 100., 
     +          LIMO2 * 100., OBJVT(J)
            WRITE (CBUF,5040)
            WRITE (CBUF(51:132),5050)
            IS = 133
            WRITE (IOFILO,'(3X,A)') CBUF(1:LENGTH(CBUF))
            WRITE (IOFILO,5000) ('(kg/kg)',I = 1,(IS-51)/10)
            WRITE (IOFILO,5010) ('-',I = 1,IS-1)
            DO 30 I = 1, NNV
              WRITE (CBUF,5060) OTIME(I,J), OMASS(I,J), OBJHC(I,J), 
     +            OQDOT(I,J), OHIGH(I,J)
              WRITE (CBUF(51:132),5070) OOD(I,J), OCO(I,J), OHCR(I,J), 
     +            OOC(I,J), OMPRODR(I,6,J), OMPRODR(I,5,J),
     +            omprodr(i,10,j),omprodr(i,11,j)
              IS = 111
              DO 20 LSP = 1, NS
                IF (ACTIVS(LSP).AND.ALLOWED(LSP)) THEN
                  WRITE (CBUF(IS:),5070) OMPRODR(I,LSP,J)
                  IS = IS + 10
                END IF
   20         CONTINUE
              WRITE (IOFILO,'(1X,A)') CBUF(1:LENGTH(CBUF))
   30       CONTINUE
          END IF
   40   CONTINUE
      END IF
      RETURN
 5000 FORMAT ('   (s)       (kg/s)    (J/kg)    (W)       (m)       ',15
     +    (A7,3X))
 5010 FORMAT (' ',255A1)
 5020 FORMAT (//,' Name: ',A,'   Referenced as object #',I3,//,
     +    ' Compartment    Fire Type    ',
     +    '   Position (x,y,z)     Relative    Lower O2',
     +    '    Pyrolysis',/,' ',52X,
     +    'Humidity    Limit       Temperature')
 5030 FORMAT (1x,a14,1x,A13,3(F7.2),F7.1,6X,F7.2,5X,F7.0,//)
 5040 FORMAT ('Time      Fmass     Hcomb     Fqdot     Fhigh     ')
 5050 FORMAT ('C/CO2     CO/CO2    H/C       O/C       HCN       ',
     +    'HCL         CT        TS')
 5060 FORMAT (F7.0,3X,1P4G10.2)
 5070 FORMAT (1P10G10.2,2x,2g10.2)
      END

      CHARACTER*8 FUNCTION CHKSUM(FILE)
      CHARACTER*(*) FILE
      CHKSUM = '00000000'
      RETURN
      END

      SUBROUTINE OUTTARG(ISW)

C
C     Routine:     OUTTARG
C
C     Description:  Output initial test case target specifications
C
C     Arguments: none
C

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "fltarget.fi"
      CHARACTER CBUF*255

      IF(NTARG.NE.0)WRITE(IOFILO,5000)
 5000 FORMAT(//,' TARGETS'//
     .         ' Target',T9,'Compartment',T24,'Position (x, y, z)',
     .         T51,'Direction (x, y, z)',T76,'Material'/
     .         1X,82('-'))

      DO 10 ITARG = 1, NTARG
        IF (ITARG.LT.NTARG-NM1+1) THEN
          CBUF = CXTARG(ITARG)
        ELSE IF (ITARG.GE.NTARG-NM1+1.AND.ISW.NE.1) THEN
          WRITE (CBUF,5004) ITARG-(NTARG-NM1)
        ELSE 
          WRITE (CBUF,5005) CXTARG(ITARG),ITARG-(NTARG-NM1)
        END IF
5004  FORMAT ('Floor, compartment ',I2)
5005  FORMAT (A8,'  Floor, compartment ',I2)
!      IF (ITARG.EQ.NTARG-NM1+1) WRITE (IOFILO,5006)
5006  FORMAT (' ')
        WRITE(IOFILO,5010)ITARG,compartmentnames(IXTARG(TRGROOM,ITARG)),
     .      (XXTARG(TRGCENX+J,ITARG),J=0,2),
     .      (XXTARG(TRGNORMX+J,ITARG),J=0,2),
     .      CBUF(1:8)
 5010    FORMAT(' ',I5,T11,a14,T21,6(F7.2,2X),T76,A8)
   10 CONTINUE
      RETURN
      END

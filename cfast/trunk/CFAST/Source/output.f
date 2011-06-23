      SUBROUTINE DISCLAIM (NAME)

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
      CHARACTER NAME*(*), OBUF*14

C     December 15, 1989 - created to specify version, model name and run date
C     Modified 2-11-93 - Arithmetic on PC was giving wrong version numbers.
C                       ( .9999999999 instead 1.00000). Changed calculations
C                        to use integer arithmetic with MOD functions. This
C                        was probably a compiler bug.
C
C     FIRST THE DATE
     
      WRITE(OBUF,2) RUNDAT(1),RUNDAT(2),RUNDAT(3)

C     NOW THE VERSION

	call splitversion (version,majver,minver,minrev)
      IF (MINREV.GT.10) THEN
        write (aminrev,'(a2)') minrev
      else if (minrev.gt.0) then
        write (aminrev,'(a1)') minrev
      else
        aminrev = '  '
      end if

      WRITE (IOFILO,1) NAME, MAJVER,MINVER,AMINREV, OBUF
      RETURN

1     FORMAT(' **   ',A8,'  Version',3X,2(I1,'.'),A2,2X,A14,4X,'**',/,
     .       ' ** ',46X,' **',/,
     .       ' **             A contribution of the              **',/,
     .       ' ** National Institute of Standards and Technology **',/,
     .       ' **             Gaithersburg, MD  20899            **',/,
     .       ' **             Not subject to Copyright           **',/)
2     FORMAT('Run ',I4.4,'/',I2.2,'/',I2.2)

      END

	SUBROUTINE VERSIONOUT (IV)

!	A routine to put the header information in the output file. 
!	We assume the file is open

      include "precis.fi"
      include "cfast.fi"
      include "cfin.fi"
      include "cshell.fi"

!>>>>> IV should be the same as version
      call splitversion(version,imajor,iminor,iminorrev)
	
      if (iminorrev.ge.10) then
        WRITE (logerr,10) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3)
      else
        WRITE (logerr,20) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3)
      end if
	RETURN

10    FORMAT ('Version ',i1,'.',i1,'.',I2,' Created ',I4.4,'/',I2.2,
     +        '/',I2.2)
20    FORMAT ('Version ',i1,'.',i1,'.',I1,' Created ',I4.4,'/',I2.2,
     +        '/',I2.2)
	END

	subroutine splitversion(version,imajor,iminor,iminorrev)
	integer version,imajor,iminor,iminorrev
	if (version.ge.1000) then
	  imajor = version / 1000
	  iminor = mod(version,1000) / 100
	  iminorrev = mod(version,100)
	else
	  imajor = version / 100
	  iminor = mod(version,100) / 10
	  iminorrev = mod(version,10)
	end if
	return
	end

	subroutine printfireparameters

      include "cfast.fi"

	write(*,5) gmwf,hcomba,te,tgignt,qradrl,limo2
5	format('gmwf,h,te,tg,q,o2 ',f8.3,1pg12.3,0p4f8.2)

	write (*,1) lfbo,lfbt,lfmax,fpos(1),
     + fpos(2),fpos(3),fplume(0)
1	format('room,type,int,pos,plume ',3i3,3f5.1,i3)

	write(*,2) 'time ',(tfired(i),i=1,lfmax)
2	format(a5,11f10.0)
	write(*,2) 'qfir ',(qfired(i),i=1,lfmax)
	write(*,3) 'bfir ',(bfired(i),i=1,lfmax)
3	format(a5,11f10.7)
	write(*,2) 'hfir ',(hfired(i),i=1,lfmax)
	write(*,2) 'afir ',(afired(i),i=1,lfmax)
      write(*,3) 'ocra ',(OCRATI(I),i=1,lfmax)
	write(*,3) 'hcra ',(hcratio(i),i=1,lfmax)
	write(*,3) 'coc2 ',(COCO2(I),i=1,lfmax)
      write(*,3) 'cco2 ',(CCO2(I),i=1,lfmax)
      write(*,3) 'hcnf ',(HCNF(I),i=1,lfmax)
      write(*,3) 'hclf ',(HCLF(I),i=1,lfmax)
	write(*,3) 'mpr5 ',(mprodr(i,5),i=1,lfmax)
	write(*,3) 'mpr6 ',(mprodr(i,6),i=1,lfmax)
	write(*,3) 'mpr7 ',(mprodr(i,7),i=1,lfmax)
	write(*,3) 'mpr10',(mprodr(i,10),i=1,lfmax)
	write(*,3) 'mpr11',(mprodr(i,11),i=1,lfmax)
	write(*,2) 'hocb ',(hocbmb(i),i=1,lfmax)
	write(*,*)

	return
	end

	subroutine printobjectparameters (iobj)

      include "cfast.fi"
      include "objects2.fi"
      include "fltarget.fi"

	write(*,5) OBJGMW(IOBJ), objvt(iobj),objmas(iobj)
5	format('gmwf,te,m ',3f8.3)
	write(*,6) objrm(iobj),objtyp(iobj),iobj
6	format ('compartment,type,# ',3i3)
	write(*,7) OBJLFM(IOBJ),(objxyz(j,iobj),j=1,3)
7	format('int,dimensions ',i3,3f8.2)
	write(*,11) objlfm(iobj),(objpos(j,iobj),j=1,3)
11    format('int,position   ',i3,3f8.2)
	write(*,12) objcl(iobj)
12    format('Characteristic volume = ',f10.3)
	write(*,*) 'targ#,targname ',obtarg(iobj),cxtarg(obtarg(iobj))
	write(*,4) nlspct,activs
4	format('nlspct,activs ',i3,11l5)
	write(*,8) (otime(i,iobj),i=1,OBJLFM(IOBJ))
8	format('time ',11f10.0)
	write(*,1) (oqdot(i,iobj),i=1,OBJLFM(IOBJ))
1	format('qdot ',11f10.0)
	write(*,3) (omass(i,iobj),i=1,OBJLFM(IOBJ))
3	format('mdot ',11f10.7)
	write(*,9) 'high ',(ohigh(i,iobj),i=1,OBJLFM(IOBJ))
9	format(a5,11f10.3)
	write(*,2) 'area ',(oarea(i,iobj),i=1,OBJLFM(IOBJ))
2	format(a5,11f10.0)
      write(*,9) 'ocra ',(ooc(I,iobj),i=1,OBJLFM(IOBJ))
	write(*,9) 'hcr  ',(ohcr(i,iobj),i=1,OBJLFM(IOBJ))
	write(*,9) 'coc2 ',(OCO(I,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'cco2 ',(ood(I,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'hcnf ',(omprodr(i,5,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'hclf ',(omprodr(i,6,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'ct   ',(omprodr(i,10,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'fC   ',(omprodr(i,11,iobj),i=1,OBJLFM(IOBJ))
	write(*,10) 'hocb ',(objhc(i,iobj),i=1,OBJLFM(IOBJ))
10	format(a5,11f10.0)
	write(*,*)

	return
	end

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
     +      	IZZVOL, ZZABSB(UPPER,I),ZZABSB(LOWER,I),
     +      	ZZRELP(I)-PAMB(I),ONTARGET(I), XXTARG(TRGNFLUXF,ITARG)
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
        CALL FLAMHGT (FQF(0),FAREA(0),FHEIGHT)
        WRITE (IOFILO,5010) 'Main', FEMS(0), FEMP(0), FQF(0),
     +      FHEIGHT, FQFC(0), FQF(0) - FQFC(0)
      END IF
      IF (NUMOBJL.NE.0) THEN
        DO 10 I = 1, NUMOBJL
          CALL FLAMHGT (FQF(I),FAREA(I),FHEIGHT)
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
            if (LAYER.EQ.UPPER.OR.IZSHAFT(I).EQ.0) THEN
              DO 50 LSP = 1, NS
                WRITE (CIOUT(IC:IC+9),5040) TOXICT(I,LAYER,LSP)
                IC = IC + 11
   50         CONTINUE
              IF (ACTIVS(6)) THEN
                DO 60 IW = 1, 4
                  IF (SWL(IWPTR(IW))) THEN
                    WRITE (CIOUT(IC:IC+9),5040) ZZWSPEC(I,IWPTR(IW))
                    IC = IC + 10
                  END IF
   60           CONTINUE
              END IF
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
      include "vents.fi"
      CHARACTER CIOUT*8, CJOUT*12, OUTBUF*132
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
                CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,XX0,XX0,xx0,xx0)
              ELSE
                IF(I.LT.J)THEN
                  SUM5 = SAU2(IIJK)
                  SUM6 = ASL2(IIJK)
                 ELSE
                  SUM5 = SAU1(IIJK)
                  SUM6 = ASL1(IIJK)
                ENDIF
                CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,SUM5,SUM6,
     .                      xx0,xx0)
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
            CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),XX0,XX0,
     .                  xx0,xx0)
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
              DO 50 IIi = 1, 6
                FLOW(IIi) = XX0
   50         CONTINUE
              IF (HVEFLO(UPPER,I).GE.XX0) FLOW(1) = HVEFLO(UPPER,I)
              IF (HVEFLO(UPPER,I).LT.XX0) FLOW(2) = -HVEFLO(UPPER,I)
              IF (HVEFLO(LOWER,I).GE.XX0) FLOW(3) = HVEFLO(LOWER,I)
              IF (HVEFLO(LOWER,I).LT.XX0) FLOW(4) = -HVEFLO(LOWER,I)
              flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
              flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
              CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),
     .                    XX0,XX0,flow(5),flow(6))
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
     +   '0To             Through        ',
     +   '      Upper Layer           ','    Lower Layer           ',
     +   'Mixing       Mixing','       Trace Species (kg)'/,
     .   ' Compartment    Vent             ',
     +   2('Inflow       Outflow      '),'To Upper     To Lower',
     +   '     Vented     Filtered',/,134('-'))
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
      include "vents.fi"
      CHARACTER CIOUT*14, CJOUT*12, OUTBUF*132
      DIMENSION FLOW(6)
      LOGICAL FIRST
      XX0 = 0.0D0
      WRITE (IOFILO,5000)

      DO 70 IRM = 1, N
        I = IRM
        FIRST = .TRUE.
        WRITE (CIOUT,'(A14)') compartmentnames(IRM)
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
              flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
              flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
              CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),
     +                    flow(5),flow(6),xx0,xx0)
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
     +    '   Trace Species',/,' Compartment    Vent             ',
     +   2('Inflow       Outflow      '),' Vented ', '   Filtered',/,
     +    ' ', 104('-'))
 5010 FORMAT (' ')
 5020 FORMAT (' ',A14,1X,A12,1X,A)
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
        if (validate) then
          RTOTAL = GTFLUX(ITARG,1)
          FTOTAL = GTFLUX(ITARG,2)
          WTOTAL = GTFLUX(ITARG,3)
          GTOTAL = GTFLUX(ITARG,4)
          CTOTAL = GTFLUX(itarg,5)
        else
          RTOTAL = XXTARG(TRGTFLUXF,ITARG)
          FTOTAL = QTFFLUX(ITARG,1)
          WTOTAL = QTWFLUX(ITARG,1)
          GTOTAL = QTGFLUX(ITARG,1)
          CTOTAL = QTCFLUX(ITARG,1)
        end if
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
              TG = TGTARG(ITARG)
              TTTEMP = XXTARG(TRGTEMPF,ITARG)
              ITCTEMP = (TRGTEMPF+TRGTEMPB)/2
              if (IXTARG(TRGEQ,ITARG).EQ.CYLPDE) 
     +            ITCTEMP = TRGTEMPF+ xxtarg(trginterior,itarg)*
     +              (TRGTEMPB-TRGTEMPF)
              TCTEMP = XXTARG(ITCTEMP,ITARG)
              IF (IXTARG(TRGEQ,ITARG).EQ.ODE) TCTEMP = TTTEMP
              IF (IXTARG(TRGMETH,ITARG).EQ.STEADY) TCTEMP = TTTEMP
              if (validate) then
                RTOTAL = GTFLUX(ITARG,1)
                FTOTAL = GTFLUX(ITARG,2)
                WTOTAL = GTFLUX(ITARG,3)
                GTOTAL = GTFLUX(ITARG,4)
                CTOTAL = GTFLUX(itarg,5)
              else
                RTOTAL = XXTARG(TRGTFLUXF,ITARG)
                FTOTAL = QTFFLUX(ITARG,1)
                WTOTAL = QTWFLUX(ITARG,1)
                GTOTAL = QTGFLUX(ITARG,1)
                CTOTAL = QTCFLUX(ITARG,1)
              end if
              IF (RTOTAL.NE.XX0) THEN
                WRITE(IOFILO,5030)ITARG,TG-273.15,TTTEMP-273.15,
     +              TCTEMP-273.15,
     +              RTOTAL,FTOTAL/RTOTAL*X100,
     +              WTOTAL/RTOTAL*X100,GTOTAL/RTOTAL*X100,
     +              CTOTAL/RTOTAL*X100
              ELSE
                WRITE(IOFILO,5030)ITARG,TG-273.15,TTTEMP-273.15,
     +              TCTEMP-273.15
              END IF
            END IF
   20     CONTINUE
        END IF
   10 CONTINUE
      RETURN
5000  FORMAT (//,' Surfaces and Targets',/,
     +  '0Compartment    Ceiling   Up wall   Low wall  '
     +  'Floor    Target    Gas       Surface   Center   Flux To',
     +  '      Fire      Surface   Gas',/,
     +  '                Temp.     Temp.     Temp.     ',
     +  'Temp.              Temp.     Temp.     Temp.    Target       ',
     +  'Rad.      Rad.      Rad.      Convect.',/,
     +  '                (C)       (C)       (C)       ',
     +  '(C)                (C)       (C)       (C)      (W/m^2)      ',
     +  '(%)       (%)       (%)       (%)',/,1X,144('-'))
5010  FORMAT (1x,a14,1P4G10.3,1X,'Floor',12X,1PG10.3,11X,1PG10.4,0PF7.1,
     +    3(3X,F7.1))
5030  FORMAT (55X,I4,4X,1P3G10.3,1X,1PG10.4,0PF7.1,3(3X,F7.1))
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

      use iofiles
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cshell.fi"

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

      SUBROUTINE FLWOUT(OUTBUF,FLOW1,FLOW2,FLOW3,FLOW4,FLOW5,FLOW6,
     . flow7,flow8)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FLWOUT
C
C     Functional Class:  I/O
C
C     Description:  Stuff the flow output after blanking appropriate zeros
C
C     Arguments: 
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "solvprm.fi"
      DIMENSION FLOW(8)
      CHARACTER OUTBUF*(*)

      OUTBUF = ' '
      FLOW(1) = FLOW1
      FLOW(2) = FLOW2
      FLOW(3) = FLOW3
      FLOW(4) = FLOW4
      FLOW(5) = FLOW5
      FLOW(6) = FLOW6
      flow(7) = flow7
      flow(8) = flow8
      X1000 = 1000.0D0
      X100 = 100.0D0
      X10 = 10.0D0
      X1 = 1.0D0
      X01 = 0.1D0
      DO 10 I = 1, 8
        IF (FLOW(I).GE.X1000) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        ELSE IF (FLOW(I).GE.X100) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5010) FLOW(I)
        ELSE IF (FLOW(I).GE.X10) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5020) FLOW(I)
        ELSE IF (FLOW(I).GE.X1) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5030) FLOW(I)
        ELSE IF (FLOW(I).GE.X01) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5040) FLOW(I)
        ELSE
          WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        END IF
        IF (FLOW(I).LE.ATOL) OUTBUF(13*(I-1)+1:13*I) = ' '
   10 CONTINUE
      RETURN
 5000 FORMAT (2X,1PG11.3)
 5010 FORMAT (F6.0,7X)
 5020 FORMAT (F7.1,6X)
 5030 FORMAT (F8.2,5X)
 5040 FORMAT (F9.3,4X)
      END


      subroutine getabstarget(targetnumber, positionvector)

!	Routine to get the absolute position of a target in the computational space

!	This is the protocol between cfast and smokeview

      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"

      double precision positionvector(*)
      integer targetnumber

      do i = 1, 6
      positionvector (i) = xxtarg(i,targetnumber)
      end do

      positionvector(1) = positionvector(1) + 
     .                    cxabs(ixtarg(trgroom,targetnumber))
      positionvector(2) = positionvector(2) + 
     .                    cyabs(ixtarg(trgroom,targetnumber))
      positionvector(3) = positionvector(3) + 
     .                    hrl(ixtarg(trgroom,targetnumber))

      return

      end subroutine getabstarget

      SUBROUTINE SETDBUG

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETDBUG
C
C     Source File: DEBUG.SOR
C
C     Functional Class:  DEBUG
C
C     Description:  
C
C     Arguments: IOFILI
C
C     Revision History:
C        Created:  12/4/1992 at 10:43 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cparams.fi"
      include "cfin.fi"
      include "opt.fi"
      include "cshell.fi"

      INTEGER FUNIT
      CHARACTER DBUGFIL*8

      CHARACTER KEYWORD*7, DBUGKY(MXDEBUG)*7, DUMMY*1, LY*2
      DATA DBUGKY/'MASSFLW',
     .            'HVACFLW',
     .            'HORZFLW',
     .            'VERTFLW',
     .            'MVNTFLW',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'XXXXXXX',
     .            'ERRVCTR',
     .            'PRNTJAC',
     .            'PRNDPDT',
     .            'XXXXXXX'/

      CLOSE (IOFILI)
      OPEN (UNIT=IOFILI,FILE=LBUF)
      READ(IOFILI,*,END=100) DUMMY
   10 CONTINUE
      READ(IOFILI,*,END=100,err=10) KEYWORD, IROOM, ILAYER
      DO 20 I = 1, 15
         IF (KEYWORD.EQ.DBUGKY(I)) THEN
            IF (ILAYER.GT.0.AND.ILAYER.LE.2.AND.
     .                      IROOM.GT.0.AND.IROOM.LT.NR) THEN
               IOUNIT = FUNIT(70)
               IF (ILAYER.EQ.UPPER) THEN
                  LY = 'UP'
               ELSE
                  LY = 'LW'
               END IF
               IF (I.NE.2) THEN
                  WRITE(DBUGFIL,'(A4,A2,I2.2)')DBUGKY(I)
     .                                              ,LY,IROOM
               ELSE
                  WRITE(DBUGFIL,'(A4,I2.2,I2.2)')DBUGKY(I)
     .                                              ,ILAYER,IROOM
               END IF
               CALL OPNOTPT(DBUGFIL,IOUNIT)
               DBUGSW(I,ILAYER,IROOM) = IOUNIT
            END IF
         END IF
   20 CONTINUE
      IF (KEYWORD.EQ.DBUGKY(D_JAC)) THEN
         CALL OPNDBG(IROOM,ILAYER)
      ELSE IF (KEYWORD.EQ.DBUGKY(16)) THEN
         IOUNIT = FUNIT(70)
         CALL OPNOTPT('ERRVECTR',IOUNIT)
         DBUGSW(16,1,1) = IOUNIT
      ELSE
      END IF
      GO TO 10
  100 CONTINUE
      CLOSE (IOFILI)
      RETURN
      END

      SUBROUTINE OUTJAC (TSEC, WM, NEQS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTJAC
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: prints out the magnitude of the jacobian matrix
C
C     Arguments: TSEC
C                WM
C                NEQS
C
C     Revision History:
C        Created:  12/2/1992 at 11:30 by PAR
C        Modified 2/5/93 by GPF
C                        Implemented Jacobian printout for both
C                        full and reduced Jacobian options
C        Modified 2/5/96 by GPF   removed reduced jacobian option added on 2/5/93.
C        Modified 7/4/97 by WWJ add call to display routines
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wdervs.fi"
      include "opt.fi"

      DIMENSION WM(JACDIM,*), BUF(MAXEQ)
      CHARACTER*2 ENTRY(MAXEQ)
      LOGICAL FIRSTC
      INTEGER IOFFST(8)
      CHARACTER LBLS(8)*3
      CHARACTER HDER*256
      CHARACTER*2 DDIAG
      DATA FIRSTC/.TRUE./
      DATA LBLS/'P  ','PMV','TMV','TU ','VU ','TL ','WT ','PRD'/
      SAVE IOFFST, HDER, IOUNIT

C     NORMAL PROCESSING OF THE DEBUG OUTPUT

      IF (DBUGSW(D_JAC,D_PRN,1).LE.0) RETURN
      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IOFFST(1) = NOFP
         IOFFST(2) = NOFPMV
         IOFFST(3) = NOFTMV
         IOFFST(4) = NOFTU
         IOFFST(5) = NOFVU
         IOFFST(6) = NOFTL
         IOFFST(7) = NOFWT
         IOFFST(8) = NOFPRD
         HDER = '  '
         ITMP2 = 0
         DO 111 I = 1, 7
            IF (IOFFST(I)-IOFFST(I+1).EQ.0) GO TO 111
            ITMP2 = ITMP2 + 1
            ITMP = IOFFST(I)*2 + 7 + ITMP2*2
            HDER(ITMP:(ITMP+2)) = LBLS(I)
  111    CONTINUE
         IOUNIT = DBUGSW(D_JAC,D_PRN,1)
      END IF
      XX0 = 0.0D0
      WRITE(IOUNIT,*)' '
      WRITE(IOUNIT,*)'JACOBIAN',NUMJAC + TOTJAC,' TIME ',TSEC
      WRITE(IOUNIT,*)' '
      WRITE(IOUNIT,'(A256)')HDER
      IRDX = 1
      DO 10 I = 1, NEQS
         IF (I.GT.IOFFST(IRDX))THEN
            IRDX = IRDX + 1
  101       CONTINUE   
            IF (I.GT.IOFFST(IRDX)) THEN
               IRDX = IRDX + 1
               GO TO 101
            END IF
            ITCOL = NEQS + 8
            DO 103 K = 1, ITCOL + 2
               ENTRY(K) = '--'
  103       CONTINUE
            WRITE(IOUNIT,*)(ENTRY(K),K=1,ITCOL)
         END IF
         ENTRY(1) = LBLS(IRDX-1)(1:2)
         ICDX = 1
         ITCOL = 1
         WMII = WM(I,I)
         IF(WMII.NE.XX0)THEN
            IITMP = LOG(ABS(WMII))
           ELSE
            IITMP = 11
         ENDIF
         IF (IITMP.LT.VERYSM) THEN
            DDIAG = ' .'
         ELSE IF (IITMP.GT.VERYBG) THEN
            DDIAG = ' Û'
         ELSE
            WRITE(DDIAG,'(I2)')IITMP
         END IF

         DO 20 J = 1, NEQS
            ITCOL = ITCOL + 1
            IF (J.GT.IOFFST(ICDX)) THEN
               ICDX = ICDX + 1
  102          CONTINUE   
               IF (J.GT.IOFFST(ICDX)) THEN
                  ICDX = ICDX + 1
                  GO TO 102
               END IF
               ENTRY(ITCOL) = ' |'
               ITCOL = ITCOL + 1
            END IF
            WMIJ = BUF(J)
            IF (WMIJ.NE.XX0.AND.WMII.NE.XX0) THEN
               TMP1 = ABS(WMIJ/WMII)
               TMP = LOG(TMP1)
            ELSE IF (WMII.EQ.XX0) THEN
               TMP = 11
            ELSE
               TMP = -11
            ENDIF
            ITMP = INT(TMP + 0.5D0)

            IF (WMIJ.EQ.0.0D0) THEN
               ENTRY(ITCOL) = '  '
            ELSE IF (ITMP.LT.VERYSM) THEN
               ENTRY(ITCOL) = ' .'
            ELSE IF (ITMP.GT.VERYBG) THEN
               ENTRY(ITCOL) = ' Û'
            ELSE
               WRITE(ENTRY(ITCOL),'(I2)')ITMP
            END IF
   20    CONTINUE
         WRITE(IOUNIT,*)DDIAG,':',(ENTRY(K),K=1,ITCOL)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE OUTJCNT (T)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OUTJCNT
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: Print out numerical performance data; resid counts,
C                  jac counts, cpu times etc.
C
C     Arguments: T
C
C     Revision History:
C        Created:  12/2/1992 at 15:10 by PAR
C        Modified: 2/13/1993 by GPF
C                  Added Newton iteration counts and  overhead CPU times
C                  to output report
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
      include "wdervs.fi"

      LOGICAL FIRSTC
      DATA FIRSTC/.TRUE./
      SAVE IOUNIT

      IF (DBUGSW(D_JAC,D_CNT,1).LE.0) RETURN
      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IOUNIT = DBUGSW(D_JAC,D_CNT,1)
         WRITE(IOUNIT,1002)
 1002    FORMAT(15X,'STEPS',4X,'JACOBIANS',5X,'RESIDS',4X,
     .   'NEWT ITERS',9X,'CPU',14X,'OVER HEAD'/
     .   4X,'TIME',4X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM',
     .             2X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM',
     .             4X,'CUR',8X,'CUM',6X,'CUR',7X,'CUM')
      END IF
      TOTJAC = TOTJAC + NUMJAC
      TOTSTEP = TOTSTEP + NUMSTEP
      TOTRESD = TOTRESD + NUMRESD
      NUMITR = NUMRESD - NUMJAC*JACDIM
      TOTITR = TOTITR + NUMITR
      WRITE(IOUNIT,1001)T,NUMSTEP,TOTSTEP,NUMJAC,TOTJAC,
     .                  NUMRESD,TOTRESD,NUMITR,TOTITR,
     .                  PRTTIME,TOTTIME,OVTIME,TOVTIME
 1001 FORMAT(1X,1PE9.2,4(1X,I4,1X,I6),
     .       1X,1PE9.2,1X,1PE9.2,1X,1PE9.2,1X,1PE9.2)
      RETURN
      END

      SUBROUTINE OPNDBG (JACCNT, JACPRN)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPNDBG
C
C     Source File: JAC.SOR
C
C     Functional Class: DEBUG
C
C     Description: opens a file on unit iounit
C
C     Arguments: JACCNT
C                JACPRN
C
C     Revision History:
C        Created:  12/2/1992 at 15:21 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"

      INTEGER FUNIT
      LOGICAL FIRSTC
      CHARACTER*6 CNTFIL, PRNFIL

      SAVE FIRSTC
      DATA FIRSTC/.TRUE./,CNTFIL/'JACCNT'/,PRNFIL/'JACPRN'/

      IF (FIRSTC) THEN
         FIRSTC = .FALSE.
         IF (JACCNT.GT.0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(CNTFIL,IOUNIT)
            DBUGSW(D_JAC,D_CNT,1) = IOUNIT
         END IF
         IF (JACPRN.GT.0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(PRNFIL,IOUNIT)
            DBUGSW(D_JAC,D_PRN,1) = IOUNIT
         END IF
      END IF
      RETURN
      END

      BLOCKDATA INITDBUG

      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"

      DATA NUMJAC/0/, NUMSTEP/0/, NUMRESD/0/, NUMITR/0/
      DATA TOTJAC/0/, TOTSTEP/0/, TOTRESD/0/, TOTITR/0/
      END

      SUBROUTINE FND_COMP(IOUNIT,ICOMP)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FND_COMP
C
C     Source File: SOLVE.f
C
C     Functional Class:
C
C     Description:
C
C     Arguments: IOUNIT
C                ICOMP
C
C     Revision History:
C        Modified: 3/4/97 by GPF:
C                  Change the messages so that they "read" better
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cfin.fi"
      include "cenviro.fi"
      include "opt.fi"

      WRITE(LBUF,*)'Solution component with the greatest error is'
      CALL XERROR(LBUF,0,1,0)
      IF (ICOMP.LE.NOFP+NM1) THEN
         WRITE(LBUF,'(A18,I2)')' pressure in room ',icomp
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTU) THEN
         WRITE(LBUF,'(A18,I2)')' either HVAC or FSM ',icomp-nm1
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFVU) THEN
         WRITE(LBUF,'(A27,I2)')' upper layer temp in room ',
     .            icomp-noftu
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTL) THEN
         WRITE(LBUF,'(A26,I2)')' upper layer vol in room ',
     .            icomp-nofvu
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFTL+NM1) THEN
         WRITE(LBUF,'(A27,I2)')' lower layer temp in room ',
     .            icomp-noftl
         CALL XERROR(LBUF,0,1,0)
      ELSE IF (ICOMP.LE.NOFWT) THEN
         IF (OPTION(FOXYGEN).EQ.ON) THEN
            WRITE(LBUF,'(A18,I2)')' oxygen component ',icomp-nofoxyl
            CALL XERROR(LBUF,0,1,0)
         ELSE
            WRITE(LBUF,'(A15,I2)')' target number ',
     .             icomp-noftt
            CALL XERROR(LBUF,0,1,0)
         ENDIF
      ELSE IF (ICOMP.LE.NOFPRD) THEN
         ITMP = ICOMP - NOFWT
         IRM = IZWALL(ITMP,1)
         IW = IZWALL(ITMP,2)
         IF (IW.EQ.1) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)')
     .      ' wall temp in room ',IRM,' ceiling '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.2) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)')
     .      ' wall temp in room ',IRM,' floor   '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.3) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)')
     .      ' wall temp in room ',IRM,' upper wall '
            CALL XERROR(LBUF,0,1,0)
         ELSE IF(IW.EQ.4) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)')
     .      ' wall temp in room ',IRM,' lower wall '
            CALL XERROR(LBUF,0,1,0)
         END IF
      END IF

      RETURN
      END
      
            SUBROUTINE DEBUGPR(IKEY,T,DT,IEQMAX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DEBUGPR
C
C     Source File: SOLVE.SOR
C
C     Functional Class:
C
C     Description:
C
C     Arguments: IKEY
C                T
C                DT
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by PAR
C        Modified: 6/30/95 by GPF
C                  added debug print for oxygen concentrations and fire size
C                  when returning from dassl
C        Modified: 1/24/1996 at 14:38 by RWP:
C                  Call XERROR to display messages
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "opt.fi"
      include "params.fi"
      include "objects2.fi"
      include "fltarget.fi"

      INTEGER*2 CH, HIT
      CHARACTER SPNAME(NS)*5, CCC*3
      INTEGER BMAP(MBR)
      LOGICAL FIRSTC
      DATA SPNAME /'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%',
     +    '  TUH', ' H2O%', '   OD', '   CT', 'TS'/
      DATA FIRSTC /.TRUE./
      SAVE BMAP

C     DEBUG PRINTING

      IF (FIRSTC) THEN
        FIRSTC = .FALSE.
        DO 30 I = 1, NBR
          DO 10 J = 1, NCNODE(NA(I))
            IF (I.EQ.ICMV(NA(I),J)) THEN
              BMAP(I) = J
              GO TO 20
            END IF
   10     CONTINUE
   20     CONTINUE
   30   CONTINUE
      END IF

      IF (IKEY.EQ.1) THEN
        WRITE (*,*) 'Pause at time = ', T,
     +      ',  Press any key to continue'
   40   CALL GRABKY(CH,HIT)
        IF (HIT.EQ.0) GO TO 40
        WRITE (*,*) 'Continuing'
        WRITE (*,*)
      ELSE IF (IKEY.EQ.2) THEN
        WRITE (IOFILO,5000) T, DT
        DO 60 I = 1, NM1
          WRITE (*,5010) I
          WRITE (*,5020) '   Upper temp(K)', ZZTEMP(I,UPPER)
          WRITE (*,5020) '   Lower temp(K)', ZZTEMP(I,LOWER)
          WRITE (*,5020) ' Interface ht(m)', ZZHLAY(I,LOWER)
          WRITE (*,5020) '   Pressure (pa)', ZZRELP(I)
          IF (NLSPCT.GT.0) WRITE (*,*) ' SPECIES MASS FRACTIONS ',
     +        ' UPPER           LOWER'
          DO 50 IPROD = 1, NS
            IF (ACTIVS(IPROD)) THEN
              WRITE (*,5030) SPNAME(IPROD), (ZZCSPEC(I,IL,IPROD),IL
     +            = UPPER,LOWER)
            END IF
   50     CONTINUE
          IF (NWALLS.NE.0) WRITE (*,*) ' WALL TEMPERATURES'
          IF (SWITCH(1,I)) THEN
            WRITE (*,5040) ZZWTEMP(I,1,1)
          END IF
          IF (SWITCH(3,I)) THEN
            WRITE (*,5060) ZZWTEMP(I,3,1)
          END IF
          IF (SWITCH(4,I)) THEN
            WRITE (IOFILO,5070) ZZWTEMP(I,4,1)
          END IF
          IF (SWITCH(2,I)) THEN
            WRITE (IOFILO,5050) ZZWTEMP(I,2,1)
          END IF
   60   CONTINUE
        WRITE (*,*) ' '
        WRITE (*,*) 'HVAC PRINT BY SYSTEMS'
        DO 90 ISYS = 1, NHVSYS
          WRITE (*,*) 'FOR SYSTEM ', ISYS
          WRITE (*,*) 'MASS FLOW OF SYSTEM ', HVMFSYS(ISYS)
          WRITE (*,*) 'MASS OF GAS IN SYSTEM ', ZZHVM(ISYS)
          DO 70 IPROD = 1, NS
            WRITE (*,*) 'MASS OF ', SPNAME(IPROD), ' ',
     +          ZZHVPR(ISYS,IPROD)
   70     CONTINUE
          DO 80 IDT = 1, NBR
            IF (IZHVBSYS(IDT).EQ.ISYS) THEN
              WRITE (*,5080) NA(IDT), HVP(NA(IDT)), NE(IDT),
     +            HVP(NE(IDT)), HVFLOW(NA(IDT),BMAP(IDT)), TBR(IDT)
            END IF
   80     CONTINUE
   90   CONTINUE
        IF (NDTECT.NE.0)THEN
           WRITE(*,*)'DETECTOR INFO'
           WRITE(*,100)
  100      FORMAT('  N ',3X,'D TEMP',6X,'J TEMP',6X,' ACT')
           DO 101 I = 1, NDTECT
              IROOM = IXDTECT(I,DROOM)
              IF (IQUENCH(IROOM).EQ.I)THEN
                 CCC='***'
              ELSE
                 CCC = '   '
              ENDIF
              WRITE(*,102)I,XDTECT(I,DTEMP),XDTECT(I,DTJET),
     .             XDTECT(I,DVEL),XDTECT(I,DTACT),CCC
  102         FORMAT(1X,I2,1X,4(E11.4,1X),A3)
  101      CONTINUE
        ENDIF
        WRITE (*,*) ' '
      ELSE IF (IKEY.EQ.3) THEN
        WRITE (*,5090) T, DT
        CALL FND_COMP(IOFILO,IEQMAX)
        WRITE(*,6030)
        DO 201 IROOM = 1, NM1
           WRITE(*,6000)IROOM,ZZRELP(IROOM),ZZHLAY(IROOM,LOWER),
     .                       ZZTEMP(IROOM,LOWER),ZZTEMP(IROOM,UPPER),
     .                   ZZCSPEC(IROOM,LOWER,2),ZZCSPEC(IROOM,UPPER,2)
  201   CONTINUE
        IF(NHVPVAR.GT.0)WRITE(*,6010)(P(NOFPMV+I),I=1,NHVPVAR)
        IF(NHVTVAR.GT.0)WRITE(*,6020)(P(NOFTMV+I),I=1,NHVTVAR)
        IF(NNODE.GT.0)WRITE(*,6040)
        DO 210 I = 1, NNODE
          DO 220 J = 1, NCNODE(I)
             DP = HVP(MVINTNODE(I,J)) - HVP(I) + DPZ(I,J)
             WRITE(*,6050) I,MVINTNODE(I,J),DP,HVP(I),
     .          HVP(MVINTNODE(I,J)), HVGHT(I)
  220     CONTINUE
  210   CONTINUE
        WRITE(*,6070)
        DO 230 IROOM = 1, NM1
           XQF = 0.
           DO 202 IOBJ = 0, NUMOBJL
             IF (IROOM.EQ.FROOM(IOBJ))XQF = XQF + FQF(IOBJ)
  202      CONTINUE
           XQF = XQF + FQDJ(IROOM)
          WRITE(*,6060)IROOM,ZZWTEMP(IROOM,1,1),ZZWTEMP(IROOM,3,1),
     .                            ZZWTEMP(IROOM,4,1),ZZWTEMP(IROOM,2,1),
     .                            XQF
  230   CONTINUE
        IF(NUMOBJL.GT.0)THEN
          WRITE(*,6080)
          DO 240 IOBJ = 1, NUMOBJL
           WRITE(*,6085)IOBJ,XFIRE(IOBJ,10),XFIRE(IOBJ,11)
  240     CONTINUE
        ENDIF
        IF(NTARG.GT.0)THEN
          WRITE(*,6090)
          DO 250 ITARG = 1, NTARG
            WRITE(*,6095)ITARG,XXTARG(TRGTEMPF,ITARG)
  250     CONTINUE
        ENDIF
      END IF
      RETURN

 5000 FORMAT (' T = ',1PG12.4,' DT = ',1PG12.4)
 5010 FORMAT (' For room ',I3,' at time      T ')
 5020 FORMAT (A16,5X,E14.7,3X,E14.7)
 5030 FORMAT (15X,A5,1X,2(E14.7,3X))
 5040 FORMAT ('  Ceiling temp(K) ',F12.2)
 5050 FORMAT ('  Floor   temp(K) ',F12.2)
 5060 FORMAT ('  Up wall temp(K) ',F12.2)
 5070 FORMAT (' Low wall temp(K) ',E12.2)
 5080 FORMAT (' from ',I2,' pressure ',E10.3,' to ',I2,' pressure ',
     + G10.3,' mass flow is ',G10.3,' temp ',G10.3)
 5090 FORMAT (' Returned from dassl at T = ',1PG14.6,',  dt = ',1PG12.4)
 5095 FORMAT (' Solution Component with the most error: ',I3)
 6000 FORMAT(1X,I3,1X,6E13.6)
 6010 FORMAT(' HVAC   PRESSURES:',4E13.6)
 6020 FORMAT(' HVAC TEMPERATUES:',4E13.6)
 6030 FORMAT(T2,'ROOM',T9,'PRESSURE',T20,'LAYER HEIGHT',T35,'L. TEMP',
     .       T48,'U. TEMP',T62,'L. Oxy',T75,'U. Oxy')
 6040 FORMAT(T3,'NODES',T12,'DELTA P',T23,'P AT FIRST NODE',
     .       T39,'P AT 2ND NODE',T57,'HEIGHT')
 6050 FORMAT(1X,2I3,1X,4(E13.6,2x))
 6060 FORMAT(1X,I3,1X,5E13.6)
 6070 FORMAT(T2,'ROOM',T11,'Ceiling',T21,'Upper Wall',
     .      T36,'Lower Wall',T49,'Floor',T61,'Fire Size')
 6080 FORMAT(T2,'Object',T11,'Heat in lower ',T26,'Heat in upper')
 6085 FORMAT(1X,I2,4X,2E13.6)
 6090 FORMAT(T2,'Target',T11,'Temp')
 6095 FORMAT(1X,I2,4X,E13.6)

      END
      
      SUBROUTINE DUMPER(ISTEP,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DUMPER
C
C     Source File: DUMPER.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Saves the data files in packed binary format
C
C     Arguments: ISTEP  Current time step
C                IERROR Returns error code
C
C     Revision History:
C        8/5/1993  by RDP, change file opens to remove machine dependence of
C                          history file
C        3/11/1987 by WWJ, change from LOCATE to LENOCO to find COMMON size
C        2/22/1989 by WWJ, increase speed of write with implied array write
C        7/26/1990 by WWJ, changed file specifications to ASCII from ASCIIZ
C        4/28/1995 by GPF, removed unused FORMAT, 5010
C        Modified: 9/5/1995 at 9:29 by PAR:
C                  Added support for IERROR and returning errors and stops to 
C                  main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use iofiles
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"

      INTEGER IOUNIT, ITOT, OUTPUT(4096), ld
      LOGICAL FIRSTC, EXISTS
      EQUIVALENCE (GAMMA,OUTPUT)
      DATA IOUNIT /11/, FIRSTC /.TRUE./
      SAVE ITOT, FIRSTC

      IF (NDUMPR.EQ.0) stop 106
 
      TERMXX = ITMSTP - 1
      ITERMXX = ITMSTP - 1
      CALL LENOCO(version/10,ITOT,IFLT,IINT)
      CALL WRITEOT(OUTPUT,ITOT,IOUNIT,IERR,version)
      IF (IERR.EQ.0) THEN
        if (debugging) WRITE (LOGERR,5020) ISTEP, ITOT * 4
        RETURN
      END IF
	 
C     ERROR PROCESSING
 
   10 WRITE (LOGERR,5030) MOD(ierr,256), historyfile
      STOP

 5020 FORMAT ('Write to the history file at',I5,I7)
 5030 FORMAT ('From dumper, error in accessing history file, error = ',I5,
     +    /,' File = ',A256)
	END

      SUBROUTINE LENOCO(IV,ITOT,IFLT,IINT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     LENOCO
C
C     Source File: LENOCO.SOR
C
C     Functional Class:  CFAST
C
C     Description:  To calculation the length of the numeric common block
C
C     Arguments: IV     CFAST reduced version number (VERSION -1800) / 10
C                ITOT   Total length of the common block in storage units
C                IFLT   Length of the floating portion in numeric storage units
C                IINT   Length of the integer portion in numeric storage units
C
C     Revision History:
C        2/3/1990:  add a pseudo entry for 18.5
C        3/3/1990:  change common block size for extra hcl constants
C                   delete fuel ratios from unlabelled common
C        4/26/1990:  change the numbering system to start back at ten for CFA
C        9/10/1990:  add 8*mxoin for the object stuff
C        2/8/1992:   fixed count, at least for single precision for 1.4
C        2/20/1992:  add count for 1.5, for both double and single precision
C        9/13/1993:  added access to integer and floating counts
C        3/4/1994:   added count for detectors
C        6/30/1995:  added count for oxygen offsets
C        8/17/1995:  added count for fsm offsets and pointers and data
C                    structures for fsm.  PAR
C        7/22/1996:  added space to total for variables used
C                    by hall option
C        2/10/1997   take away space resulting from vent reduction
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"

      IINT = (LOC(ITERMXX) - LOC(NEUTRAL))/4 + 1
      IFLT = (LOC(TERMXX) - LOC(GAMMA))/4

      IFLT = IFLT + 2
      ITOT = IINT + IFLT

      RETURN
      END

      SUBROUTINE WRITEOT(INPUT,LEN,IOUNIT,IERR,IVERS0)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WRITEOT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  Write compacted history file
C
C     Arguments: INPUT
C                LEN
C                IOUNIT
C                IERR
C                IVERS0
C
C     Revision History:
C        Created:  PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
 
      PARAMETER (MXDMP = 36000)
      INTEGER INPUT(LEN), PARRAY(MXDMP)
      CHARACTER HEADER*6
      DATA HEADER /'$$CFL$'/
 
      IERR = 0
      CALL PACKOT(LEN,MXDMP,INPUT,PARRAY)
      WRITE (IOUNIT,IOSTAT=IOS) HEADER, IVERS0
      WRITE (IOUNIT,IOSTAT=IOS) PARRAY(1), (PARRAY(I),I = 2,PARRAY(1))

      IF (IOS.NE.0) THEN
        IERR = IOS
      ELSE
        IERR = 0
      END IF
      RETURN
      END

      SUBROUTINE PACKOT(ITIN,MXDMP,DOIT,RETBUF)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     PACKOT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  Pack the output array
C
C     Arguments: ITIN
C                MXDMP
C                DOIT
C                RETBUF
C
C     Revision History:
C        Created:  8/31/1993 at 16:51 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C     This is the pack routine.  It crunches the binary common block to reduce
C     The amount of storage required to hold a single history record.
C     The length of the record which is written is contained in the first word
C     of the record and does NOT include itself.
C     The original implementation of this work was published by 
C     Andrew Binstock in The C Gazette, December 1987.  It is one of a large
C     class of compression schemes.  This particular scheme is fast, and is
C     best at compressing strings of the same character.  Since much of
C     the history file in CFAST stays the same, 0 for example, this works
C     fairly well.
 
      INTEGER IC, ITIN, IDX, RIDX, MXDMP
      INTEGER DOIT(ITIN), RETBUF(MXDMP)
      INTEGER LC, COUNT, MRKR
      CHARACTER MSG*80
 
      IDX = 1
      RIDX = 1
      MRKR = 106
      IC = DOIT(IDX)
      IDX = IDX + 1
 
C     CHECKING TO MAKE SURE THE FIRST NUMBERS ARE NOT THE MARKER
 
   10 IF (IC.EQ.MRKR) THEN
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        IC = DOIT(IDX)
        IDX = IDX + 1
        GO TO 10
      END IF
 
      LC = IC
      COUNT = 1
 
C     MAIN LOOP
 
   20 IF (IDX.LE.ITIN) THEN
        IC = DOIT(IDX)
        IDX = IDX + 1
 
C     IF NEXT NUMBER = MARKER THEN STORE WHAT YOU HAVE
 
   30   IF (IC.EQ.MRKR) THEN
          IF (COUNT.GT.3) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
          ELSE
            IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            DO 40, I = 1, COUNT
              RIDX = RIDX + 1
              RETBUF(RIDX) = LC
   40       CONTINUE
          END IF
          COUNT = 0
          RIDX = RIDX + 1
          RETBUF(RIDX) = MRKR
          RIDX = RIDX + 1
          RETBUF(RIDX) = MRKR
          IF (IDX.GT.ITIN) GO TO 60
          IC = DOIT(IDX)
          IDX = IDX + 1
          LC = IC
          GO TO 30
        END IF
        IF (IC.EQ.LC) THEN
          COUNT = COUNT + 1
          IF (COUNT.EQ.(2**30)) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
            COUNT = 0
          END IF
        ELSE
          IF (COUNT.GT.3) THEN
            IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
            LC = IC
            COUNT = 1
          ELSE
            IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
            DO 50, I = 1, COUNT
              RIDX = RIDX + 1
              RETBUF(RIDX) = LC
   50       CONTINUE
            LC = IC
            COUNT = 1
          END IF
        END IF
        GO TO 20
      END IF
   60 IF (COUNT.GT.3) THEN
        IF ((RIDX+5).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
        CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
        LC = IC
        COUNT = 1
      ELSE
        IF ((RIDX+COUNT+2).GE.MXDMP) THEN
              WRITE (MSG,*) 
     .  'PACKOT - Overwrite, input and index = ', ITIN, IDX
              CALL XERROR(MSG,0,1,1)
              IERR = 19
              RETURN
            END IF
        DO 70, I = 1, COUNT
          RIDX = RIDX + 1
          RETBUF(RIDX) = LC
   70   CONTINUE
      END IF
      RETBUF(1) = RIDX
      RETURN
      END

      SUBROUTINE OPUT(IC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPUT
C
C     Source File: WRITEOT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IC
C                COUNT
C                ITIN
C                MXDMP
C                RIDX
C                RETBUF
C
C     Revision History:
C        Created:  8/31/1993 at 16:51 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER IC, COUNT, MRKR, ITIN, RIDX
      INTEGER RETBUF(ITIN)
      MRKR = 106
      RIDX = RIDX + 1
      RETBUF(RIDX) = MRKR
      RIDX = RIDX + 1
      RETBUF(RIDX) = IC
      RIDX = RIDX + 1
      RETBUF(RIDX) = COUNT
      RETURN
      END

	subroutine StatusOutput (T, dT, errorcode)
	
!  Write the status information to the "statusfile"

      include "precis.fi"
      include "cfast.fi"

	integer errocode	
	double precision T, dT

	rewind (12)
	write(12,5001) t, dt
     	call rsltcmp (12)
	errorcode = 0
	return
	
5001	FORMAT('Status at T = ',1PG11.2, ' DT = ',G11.3)
	end

      SUBROUTINE WRITEINI(FILE)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WRITEINI
C
C     Source File: WRITEINI
C
C     Functional Class:  
C
C     Description:  this routine creates a solver.ini file for the current
C                   version of CFAST.  It is created using:
C                   cfast -s filename
C                   where filename is the name of the file to contain
C                   the solver.ini options .  The default name is 
C                   SOLVE.INI (so as to not overwrite SOLVER.INI if
C                   it is present)
C
C     Arguments: 
C
C     Revision History:
C        Created:  10/21/97 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "params.fi"

      CHARACTER*(*) FILE
      INTEGER FUNIT

      NNNOPT = 21

      IUNIT = FUNIT(70)
      OPEN(UNIT=IUNIT,FILE=FILE)

      WRITE(IUNIT,10)
   10 FORMAT(' ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL,',
     .       ' REL OTHER TOL')
      WRITE (IUNIT,11) APTOL, RPTOL, ATOL, RTOL
   11 FORMAT(1X,5(1PG11.4,1X))

      WRITE(IUNIT,20)
   20 FORMAT(' ABS WALL TOL, REL WALL TOL, INITIALIZATION TOLERANCE')
      WRITE (IUNIT,11) AWTOL, RWTOL, ALGTOL

      WRITE(IUNIT,30)
   30 FORMAT(' ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, ',
     .       ' REL HVAC TEMP')
      WRITE (IUNIT,11) AHVPTOL, RHVPTOL, AHVTTOL, RHVTTOL

      WRITE(IUNIT,40)
   40 FORMAT(' NUMBER OF PHYSICAL OPTION FLAGS')
      WRITE (IUNIT,*) NNNOPT

      WRITE(IUNIT,50)
   50 FORMAT(' FIRE,      HFLOW,  ENTRAIN, VFLOW,       CJET')
      WRITE (IUNIT,*) (OPTION(J),J = 1,5)

      WRITE(IUNIT,60)
   60 FORMAT(' DOOR-FIRE, CONVEC, RAD,     CONDUCT, DEBUG PRINT  ')
      WRITE (IUNIT,*) (OPTION(J),J = 6,10)

      WRITE(IUNIT,70)
   70 FORMAT(' EXACT ODE, HCL,   MFLOW,    KEYBOARD, ',
     .        'TYPE OF INITIALIZATION')
      WRITE (IUNIT,*) (OPTION(J),J = 11,15)

      WRITE(IUNIT,80)
   80 FORMAT(' MV HEAT LOSS, USE MODIFIED JACOBIAN, DASSL DEBUG, ',
     .       'OXYGEN SOLVE    DETECTORS')
      WRITE (IUNIT,*) (OPTION(J),J = 16,20)

      WRITE(IUNIT,90)
   90 FORMAT(' OBJECT BACKTRACKING')
      WRITE (IUNIT,*) (OPTION(J),J = 21,21)

      WRITE(IUNIT,100)
  100 FORMAT(' NUMBER OF WALL NODES, FRACTIONS FOR FIRST, ',
     .       'MIDDLE AND LAST WALL SLAB')
      WRITE (IUNIT,101) NWPTS, (WSPLIT(I),I=1,3)
  101 FORMAT(1X,I3,1X,3(1PG11.4,1X))

      WRITE(IUNIT,110)
  110 FORMAT(' BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE,',
     .       '   2=INSULATED 3=FLUX)')
      WRITE (IUNIT,*) IWBOUND

      WRITE(IUNIT,120)
  120 FORMAT(' MAXIMUM STEP SIZE,  MAX FIRST STEP - ',
     .       ' IF EITHER <0 THEN SOLVER DECIDES')
      WRITE (IUNIT,11) STPMAX, DASSLFTS

      WRITE(IUNIT,130)
  130 FORMAT(' HVAC CONVECTION COEFFICIENT')
      WRITE(IUNIT,11) DUCTCV

      WRITE(IUNIT,140)
  140 FORMAT(' JAC CHECK (>0 CHECK JACOBIAN), JACOBIAN CUTOFF, ',
     .       '  SNSQE PRINT (1=ON)')
      WRITE(IUNIT,141) JACCHK, CUTJAC, IPRTALG
  141 FORMAT(1X,I3,1X,1PG11.4,I3)

      IF(1.EQ.1)STOP
      RETURN
      END

	subroutine openoutputfiles

!	Now that we know what output is needed, open the appropriate files
!	Note that the sign of lprint determines whether we write to the console or  file
!	Unit numbers defined here and readinputfiles

!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!	 4 is for the indicator that the model is running (kernelisrunning)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output (header) - note this is rewound each time the plot data is written)
!     14 smokeview output (plot data)
!     15 spreadsheet output (normal)
!     16 spreadsheet output (flow field)
!     17 spreadsheet output (species)
!     18 spreadsheet output (walls and targets)

!!!! Note that we assume that the default carraigecontrol for formatted files is of type LIST (no fortran controls)

      use iofiles
      include "cfast.fi"
      include "cshell.fi"

! first the file for "printed" output
	if (lprint.lt.0) then
		 open (unit=iofilo,file=outputfile,status='new',
     .		  carriagecontrol='fortran')
		 lprint = abs(lprint)
		 WRITE (LOGERR,5002) trim(outputfile)
		 if (outputformat.eq.0) outputformat = 2
	else
		 OPEN (UNIT=IOFILO,FILE='CON',CARRIAGECONTROL='FORTRAN')
		 write (logerr,5004)
		 if (outputformat.eq.0) outputformat = 1
	END IF

! next the history file
      IF (ldiago.gt.0) THEN
	   write(logerr,5001) trim(historyfile)
         OPEN (UNIT=11,FILE=historyfile,ERR=10,
     +		IOSTAT=IOS,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      END IF 

! Next create the status file
	open (12,file=statusfile,ACCESS='APPEND',ERR=81,iostat=ios)

! Now the smokeview files
	if (ldiagp.gt.0) then
		 write(logerr,5003) trim(smvhead),trim(smvdata)
	    OPEN (UNIT=13,FILE=smvhead,form='formatted',err=11,
     .          iostat=ios)
		 OPEN (UNIT=14,FILE=smvdata,FORM="UNFORMATTED",err=11,
     .          iostat=ios)
	endif
 
! Next the spread sheet files
	if (lcopyss.gt.0) then
		 write(logerr,5005) trim(ssnormal),trim(ssflow),
     .                       trim(ssspecies),trim(sswall)
		 open (unit=15, file=ssnormal,form='formatted')
		 open (unit=16, file=ssflow,form='formatted')
		 open (unit=17, file=ssspecies,form='formatted')
		 open (unit=18, file=sswall,form='formatted')
	endif

! And finally we create a file to indicate that the model is running.

	open (unit=4, file=kernelisrunning, dispose='DELETE')

	return

C ERROR PROCESSING
 
!	History file
   10 WRITE (LOGERR,5030) MOD(IOS,256), trim(historyfile)
      STOP 105
!	Smokeview file
   11 write(logerr,5040) mod(ios,256),trim(smvhead),trim(smvdata)
	stop 105
!	This one comes from writing to the status file
   81 write(logerr,*) 'Fatal error writing to the status file ',ios
	STOP 106

 5001 format ('Open the history file ',a)
 5002 format ('Open the output file ',a)
 5003 format ('Open the smokeview files - ',a,2x,a)
 5004 format ('Send output to the consol')
 5005 format ('Open the spreadsheet files - ',4(a,2x))
 5030 FORMAT ('Error ',i4,' while accessing history, file = ',A)
 5040 FORMAT ('Error ',i4,' while processing smokeview files -'
     . ,i3,2x,a,2x,a)

	end
	
	subroutine deleteoutputfiles (outputfile)

      use IFPORT

      character (*) outputfile
      logical exists, doesthefileexist
      integer (2) filecount

      if (doesthefileexist(outputfile)) then
	  filecount = delfilesqq(outputfile)
	  if (filecount.lt.1) stop 104
      endif

      return
      end

      integer function rev_output
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_output = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_output
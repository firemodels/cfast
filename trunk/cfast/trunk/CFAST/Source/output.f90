    subroutine disclaim (name)

    use cshell
    implicit none
    character name*(*), obuf*14, aminrev*2
    integer majver, minver, minrev

    ! first the date
    write(obuf,2) rundat(1),rundat(2),rundat(3)

    ! now the version
    call splitversion (version,majver,minver,minrev)
    if (minrev>10) then
        write (aminrev,'(a2)') minrev
    else if (minrev>0) then
        write (aminrev,'(a1)') minrev
    else
        aminrev = '  '
    endif

    write (iofilo,1) name, majver,minver,aminrev, obuf
    return

1   format (' **   ',A8,'  Version',3X,2(I1,'.'),A2,2X,A14,4X,'**',/, ' ** ',46X,' **',/, &
    ' **             A contribution of the              **',/, &
    ' ** National Institute of Standards and Technology **',/, &
    ' **             Gaithersburg, MD  20899            **',/, &
    ' **             Not subject to Copyright           **',/)
2   format('Run ',I4.4,'/',I2.2,'/',I2.2)

    end subroutine disclaim

    subroutine versionout (iunit)

    !	a routine to put the header information in the output file. 
    !	we assume the file is open

    use cfast_main
    use cshell
    implicit none
    integer rev_cfast, imajor, iminor, iminorrev, iunit

    call splitversion(version,imajor,iminor,iminorrev)

    if (iminorrev>=10) then
        if (iunit==0) then
            write (*,10) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), rev_cfast()
        else
            write (iunit,10) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), rev_cfast()
        endif
    else
        if (iunit==0) then
            write (*,20) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), rev_cfast()
        else
            write (iunit,20) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), rev_cfast()
        endif
    endif
    return

10  format ('Version ',i1,'.',i1,'.',I2,', Created ',I4.4,'/',I2.2,'/',I2.2,', Revision ',i5)
20  format ('Version  ',i1,'.',i1,'.',I1,', Created ',I4.4,'/',I2.2,'/',I2.2,', Revision ',i5)
    end subroutine versionout

    subroutine splitversion(version,imajor,iminor,iminorrev)
    integer version,imajor,iminor,iminorrev
    if (version>=1000) then
        imajor = version / 1000
        iminor = mod(version,1000) / 100
        iminorrev = mod(version,100)
    else
        imajor = version / 100
        iminor = mod(version,100) / 10
        iminorrev = mod(version,10)
    endif
    return
    end

    subroutine printfireparameters

    use cfast_main

    write(*,5) gmwf,hcomba,te,tgignt,qradrl,limo2
5   format('gmwf,h,te,tg,q,o2 ',f8.3,1pg12.3,0p4f8.2)

    write (*,1) lfbo,lfbt,lfmax,fpos(1),fpos(2),fpos(3),fplume(0)
1   format('room,type,int,pos,plume ',3i3,3f5.1,i3)

    write(*,2) 'time ',(tfired(i),i=1,lfmax)
2   format(a5,11f10.0)
    write(*,2) 'qfir ',(qfired(i),i=1,lfmax)
    write(*,3) 'bfir ',(bfired(i),i=1,lfmax)
3   format(a5,11f10.7)
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

    use cfast_main
    use fltarget
    use objects2

    write(*,5) OBJGMW(IOBJ), objvt(iobj),objmas(iobj)
5   format('gmwf,te,m ',3f8.3)
    write(*,6) objrm(iobj),objtyp(iobj),iobj
6   format ('compartment,type,# ',3i3)
    write(*,7) OBJLFM(IOBJ),(objxyz(j,iobj),j=1,3)
7   format('int,dimensions ',i3,3f8.2)
    write(*,11) objlfm(iobj),(objpos(j,iobj),j=1,3)
11  format('int,position   ',i3,3f8.2)
    write(*,12) objclen(iobj)
12  format('Characteristic volume = ',f10.3)
    write(*,*) 'targ#,targname ',obtarg(iobj),cxtarg(obtarg(iobj))
    write(*,4) nlspct,activs
4   format('nlspct,activs ',i3,11l5)
    write(*,8) (otime(i,iobj),i=1,OBJLFM(IOBJ))
8   format('time ',11f10.0)
    write(*,1) (oqdot(i,iobj),i=1,OBJLFM(IOBJ))
1   format('qdot ',11f10.0)
    write(*,3) (omass(i,iobj),i=1,OBJLFM(IOBJ))
3   format('mdot ',11f10.7)
    write(*,9) 'high ',(ohigh(i,iobj),i=1,OBJLFM(IOBJ))
9   format(a5,11f10.3)
    write(*,2) 'area ',(oarea(i,iobj),i=1,OBJLFM(IOBJ))
2   format(a5,11f10.0)
    write(*,9) 'ocra ',(ooc(I,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'hcr  ',(ohcr(i,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'coc2 ',(OCO(I,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'cco2 ',(ood(I,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'hcnf ',(omprodr(i,5,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'hclf ',(omprodr(i,6,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'ct   ',(omprodr(i,10,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'fC   ',(omprodr(i,11,iobj),i=1,OBJLFM(IOBJ))
    write(*,10) 'hocb ',(objhc(i,iobj),i=1,OBJLFM(IOBJ))
10  format(a5,11f10.0)
    write(*,*)

    return
    end

    SUBROUTINE RESULT(TIME,ISW)

    !     Description:  Output the results of the simulation at the current time
    !                RSLTLAY is the basic environment
    !                RSLTFIR information on fires
    !                RSLTTAR targets and walls - temperature, radiation and convective flux
    !                RSLTSPRINK sprinkler and detector information
    !                RSLTHALL track the nose of the gravity wave
    !                RSLTSP species

    !     Arguments: TIME  Current time (s)
    !                ISW   1 if called from CFAST, 0 otherwise (only effects
    !                      printout of object names -- only CFAST knows actual
    !                      names, others just do it by numbers

    use cshell
    implicit none

    integer isw
    real*8 :: time

    IF (outputformat>1) THEN
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
    ELSE IF (outputformat==1) THEN
        WRITE (IOFILO,5000) TIME
        CALL RSLTCMP (iofilo)
    endif

5000 FORMAT (//,' Time = ',F8.1,' seconds.')
    END

    SUBROUTINE RSLTLAY

    !     Description:  Output the 2 layer environment at the current time

    use cfast_main
    use cenviro
    use cshell
    use fltarget
    implicit none

    integer :: i, itarg, izzvol

    WRITE (IOFILO,5000)
    WRITE (IOFILO,5010)
    WRITE (IOFILO,5020)
    WRITE (IOFILO,5030)
    WRITE (IOFILO,5040)
    DO I = 1, NM1
        ITARG = NTARG - NM1 + I
        IZZVOL = ZZVOL(I,UPPER)/VR(I)*100.D0+0.5D0
        IF (IZSHAFT(I)==1) THEN
            WRITE (IOFILO,5071) compartmentnames(I), ZZTEMP(I,UPPER)-273.15, ZZVOL(I,UPPER), &
            ZZABSB(UPPER,I),ZZRELP(I)-PAMB(I),ONTARGET(I), XXTARG(TRGNFLUXF,ITARG)
        ELSE
            WRITE (IOFILO,5070) compartmentnames(i), ZZTEMP(I,UPPER)-273.15, ZZTEMP(I,LOWER)-273.15, &
            ZZHLAY(I,LOWER), ZZVOL(I,UPPER), IZZVOL, ZZABSB(UPPER,I),ZZABSB(LOWER,I), ZZRELP(I)-PAMB(I),ONTARGET(I), XXTARG(TRGNFLUXF,ITARG)
        ENDIF
    end do
    RETURN

5000 FORMAT (' ')
5010 FORMAT (' Compartment',T16,'Upper',T26,'Lower',T36,'Inter.',T46,'Upper',T62,'Upper',T73,'Lower',T83,'Pressure',T95,'Ambient',T106,'Floor')
5020 FORMAT (T16,'Temp.',T26,'Temp.',T36,'Height',T46,'Vol.',T62,'Absorb',T73,'Absorb',T95,'Target',T106,'Target')
5030 FORMAT (T17,'(C)',T26,'(C)',T36,'(m)',T46,'(m^3)',T62,'(m^-1)',T73,'(m^-1)',T85,'(Pa)',T95,'(W/m^2)',T106,'(W/m^2)')
5040 FORMAT (' ',113('-'))
5070 FORMAT (1x,a13,1P2G10.4,1PG10.4,1X,1pg8.2,'(',I3,'%) ',1PG10.3,1X,1PG10.3,1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
5071 FORMAT (1x,A13,1PG10.4,10(' '),10(' '),1X,1pg8.2,7(' '),1PG10.3,1X,10(' '),1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
    END
    SUBROUTINE RSLTFIR(ISW)

    !     Description:  Output the fire environment at the current time

    !     Arguments: ISW    Print switch for object fire printout

    use cfast_main
    use cshell
    use objects1
    implicit none

    integer length, i, isw, ir, j
    real*8 xx0, fheight, xems, xemp, xqf, xqupr, xqlow

    EXTERNAL LENGTH
    XX0 = 0.0D0
    WRITE (IOFILO,5000)
    IF (LFMAX>0.AND.LFBT>0.AND.LFBO>0) THEN
        CALL FLAMHGT (FQF(0),FAREA(0),FHEIGHT)
        WRITE (IOFILO,5010) 'Main', FEMS(0), FEMP(0), FQF(0), FHEIGHT, FQFC(0), FQF(0) - FQFC(0)
    endif
    IF (NUMOBJL/=0) THEN
        DO I = 1, NUMOBJL
            CALL FLAMHGT (FQF(I),FAREA(I),FHEIGHT)
            IF (ISW/=0) THEN
                IF (OBJPNT(I)/=0) THEN
                    J = OBJPNT(I)
                    WRITE (IOFILO,5010) OBJNIN(J)(1:LENGTH(OBJNIN(J))), FEMS(I), FEMP(I), FQF(I), FHEIGHT,FQFC(I),FQF(I)-FQFC(I),objmaspy(i),radio(i)
                endif
            ELSE
                WRITE (IOFILO,5020) I, FEMS(I), FEMP(I), FQF(I), FHEIGHT,FQFC(I),FQF(I)-FQFC(I),objmaspy(i),radio(i)
            endif
        end do
    endif
    WRITE (IOFILO,'(A)') ' '
    DO IR = 1, NM1
        XEMS = XX0
        XEMP = XX0
        XQF = XX0
        XQUPR = XX0
        XQLOW = XX0
        DO I = 0, NUMOBJL
            IF (IR==FROOM(I)) THEN
                XEMS = XEMS + FEMS(I)
                XEMP = XEMP + FEMP(I)
                XQF = XQF + FQF(I)
                XQUPR = XQUPR + FQUPR(I)
                XQLOW = XQLOW + FQLOW(I)
            endif
        end do
        XQF = XQF + FQDJ(IR)
        IF (XEMS+XEMP+XQF+XQUPR+XQLOW+FQDJ(IR)/=XX0) &
        WRITE (IOFILO,5030) compartmentnames(IR), XEMS, XEMP, XQF, XQUPR, XQLOW, FQDJ(IR)
    end do
    IF (FQDJ(N)/=XX0) WRITE (IOFILO,5040) FQDJ(N)
    RETURN
5000 FORMAT (//,' Fires',/,'0Compartment    Fire      Plume     Pyrol     Fire      Flame     Fire in   Fire in   Vent      Convec.   Radiat.   Pyrolysate  Trace',/, &
    '                          Flow      Rate      Size      Height    Upper     Lower     Fire',/, &
    '                          (kg/s)    (kg/s)    (W)       (m)       (W)       (W)       (W)         (W)       (W)       (kg)      (kg)' ,/,' ',138('-'))
5010 FORMAT (' ',14X,A8,2X,1P4G10.3,30X,1P3G10.3,2x,g10.3)
5020 FORMAT (' ',13X,'Object ',I2,2X,1P4G10.3,30X,1P3G10.3,2x,g10.3)
5030 FORMAT (' ',a14,10X,1P3G10.3,10X,1P3G10.3)
5040 FORMAT ('  Outside',76X,1PG10.3)
    END
    SUBROUTINE RSLTSP

    !     Description:  Output the layer and wall species at the current time

    use cfast_main
    use cenviro
    use cshell
    implicit none

    LOGICAL SWL(4)
    INTEGER IWPTR(4)
    CHARACTER STYPE(NS)*10, SUNITS(NS)*11, CIOUT*255, CJOUT*255,LNAMES(2)*5, WTYPE(4)*10
    EXTERNAL LENGTH
    integer length, i, j, layer, ic, lsp, iw

    DATA LNAMES /'Upper', 'Lower'/
    DATA IWPTR /1, 3, 4, 2/
    DATA WTYPE /'HCl c', 'HCl f', 'HCl uw', 'HCl lw'/
    DATA SUNITS /'(%)', '(%)', '(%)', '(ppm)', '(ppm)', '(ppm)','(%)', '(%)', '(1/m)', '(g-min/m3)', ' kg '/
    DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'CT', ' TS'/
    IF (NLSPCT/=0) THEN
        DO I = 1, NWAL
            SWL(I) = .FALSE.
            DO J = 1, NM1
                SWL(I) = SWL(I) .OR. SWITCH(I,J)
            end do
        end do

        DO LAYER = UPPER, LOWER
            WRITE (IOFILO,5050) LNAMES(LAYER)
            CIOUT = 'Compartment'
            CJOUT = ' '
            IC = 16
            DO LSP = 1, NS
                IF (ACTIVS(LSP)) THEN
                    WRITE (CIOUT(IC:IC+9),5000) STYPE(LSP)
                    WRITE (CJOUT(IC:IC+9),5000) SUNITS(LSP)
                    IC = IC + 11
                endif
            end do
            IF (ACTIVS(6)) THEN
                DO IW = 1, 4
                    IF (SWL(IWPTR(IW))) THEN
                        WRITE (CIOUT(IC:IC+9),5000) WTYPE(IW)
                        WRITE (CJOUT(IC:IC+9),5000) '(mg/m^2)  '
                        IC = IC + 10
                    endif
                end do
            endif
            WRITE (IOFILO,5020) CIOUT(1:LENGTH(CIOUT))
            WRITE (IOFILO,5020) CJOUT(1:LENGTH(CJOUT))
            WRITE (IOFILO,5030) ('-',I = 1,IC)
            WRITE (CIOUT,5010)
            DO I = 1, NM1
                WRITE (CIOUT,5060) compartmentnames(i)
                IC = 14
                if (LAYER==UPPER.OR.IZSHAFT(I)==0) THEN
                    DO LSP = 1, NS
                        WRITE (CIOUT(IC:IC+9),5040) TOXICT(I,LAYER,LSP)
                        IC = IC + 11
                    end do
                    IF (ACTIVS(6)) THEN
                        DO IW = 1, 4
                            IF (SWL(IWPTR(IW))) THEN
                                WRITE (CIOUT(IC:IC+9),5040) ZZWSPEC(I,IWPTR(IW))
                                IC = IC + 10
                            endif
                        end do
                    endif
                endif
                WRITE (IOFILO,5020) CIOUT(1:LENGTH(CIOUT))
            end do
        end do
    endif
    RETURN
5000 format (a10)
5010 format (' ')
5020 format (' ',a)
5030 format (' ',255a1)
5040 format (1pg10.3)
5050 format (//,' ',a5,' Layer Species',/)
5060 format (a13)
    end subroutine rsltsp

    SUBROUTINE RSLTFLW (time)

    !     Description:  Output the vent flow at the current time

    use cfast_main
    use cshell
    use vents
    implicit none

    integer :: irm, i, j, k, iijk, ii, inode, iii
    real*8 :: xx0, sum1, sum2, sum3, sum4, sum5, sum6, flow, time

    CHARACTER CIOUT*8, CJOUT*12, OUTBUF*132
    DIMENSION FLOW(6)
    LOGICAL FIRST
    XX0 = 0.0D0
    WRITE (IOFILO,5000)

    DO IRM = 1, N
        I = IRM
        FIRST = .TRUE.
        WRITE (CIOUT,'(A8)') compartmentnames(IRM)
        IF (IRM==N) CIOUT = 'Outside'

        !     HORIZONTAL FLOW NATURAL VENTS

        DO J = 1, N
            DO K = 1, mxccv
                IIJK = IJK(I,J,K)
                IF (IAND(1,ISHFT(NW(I,J),-K))/=0) THEN
                    IF (J==N) THEN
                        WRITE (CJOUT,'(A1,1X,A7,A2,I1)') 'H', 'Outside', ' #', K
                    ELSE
                        WRITE (CJOUT,'(A1,1X,A4,I3,A2,I1)') 'H', 'Comp', J,' #', K
                    endif
                    IF(I<J)THEN
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
                    IF (I==N) THEN
                        CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,XX0,XX0,xx0,xx0)
                    ELSE
                        IF(I<J)THEN
                            SUM5 = SAU2(IIJK)
                            SUM6 = ASL2(IIJK)
                        ELSE
                            SUM5 = SAU1(IIJK)
                            SUM6 = ASL1(IIJK)
                        ENDIF
                        CALL FLWOUT(OUTBUF,SUM1,SUM2,SUM3,SUM4,SUM5,SUM6,xx0,xx0)
                    endif
                    IF (FIRST) THEN
                        IF (I/=1) WRITE (IOFILO,5010)
                        WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                        FIRST = .FALSE.
                    ELSE
                        WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
                    endif
                endif
            end do
        end do

        !     VERTICAL FLOW NATURAL VENTS

        DO J = 1, N
            IF (NWV(I,J)/=0.OR.NWV(J,I)/=0) THEN
                IF (J==N) THEN
                    WRITE (CJOUT,'(A1,1X,A7)') 'V', 'Outside'
                ELSE
                    WRITE (CJOUT,'(A1,1X,A4,I3)') 'V', 'Comp', J
                endif
                DO II = 1, 4
                    FLOW(II) = XX0
                end do
                IF (VMFLO(J,I,UPPER)>=XX0) FLOW(1) = VMFLO(J,I,UPPER)
                IF (VMFLO(J,I,UPPER)<XX0) FLOW(2) = -VMFLO(J,I,UPPER)
                IF (VMFLO(J,I,LOWER)>=XX0) FLOW(3) = VMFLO(J,I,LOWER)
                IF (VMFLO(J,I,LOWER)<XX0) FLOW(4) = -VMFLO(J,I,LOWER)
                CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),XX0,XX0,xx0,xx0)
                IF (FIRST) THEN
                    IF (I/=1) WRITE (IOFILO,5010)
                    WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                    FIRST = .FALSE.
                ELSE
                    WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
                endif
            endif
        end do

        !     MECHANICAL VENTS

        IF (NNODE/=0.AND.NEXT/=0) THEN
            DO I = 1, NEXT
                II = HVNODE(1,I)
                IF (II==IRM) THEN
                    INODE = HVNODE(2,I)
                    WRITE (CJOUT,'(A1,1X,A4,I3)') 'M', 'Node', INODE
                    DO IIi = 1, 6
                        FLOW(IIi) = XX0
                    end do
                    IF (HVEFLO(UPPER,I)>=XX0) FLOW(1) = HVEFLO(UPPER,I)
                    IF (HVEFLO(UPPER,I)<XX0) FLOW(2) = -HVEFLO(UPPER,I)
                    IF (HVEFLO(LOWER,I)>=XX0) FLOW(3) = HVEFLO(LOWER,I)
                    IF (HVEFLO(LOWER,I)<XX0) FLOW(4) = -HVEFLO(LOWER,I)
                    flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
                    flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
                    CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),XX0,XX0,flow(5),flow(6))
                    IF (FIRST) THEN
                        IF (I/=1) WRITE (IOFILO,5010)
                        WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                        FIRST = .FALSE.
                    ELSE
                        WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
                    endif
                endif
            end do
        endif
    end do

5000 format (//,' Flow Through Vents (kg/s)',/, &
    '0To             Through              Upper Layer               Lower Layer           Mixing       Mixing       Trace Species (kg)'/, &
    ' Compartment    Vent                 Inflow       Outflow      Inflow       Outflow  To Upper     To Lower     Vented     Filtered',/,134('-'))
5010 format (' ')
5020 format (' ',a7,8x,a12,1x,a)
    end subroutine rsltflw

    SUBROUTINE RSLTFLWt (time)

    !     Description:  Output the vent flow at the current time

    use cfast_main
    use cshell
    use vents
    implicit none

    integer irm, i, ii, iii, inode
    real*8 xx0, flow, time

    CHARACTER CIOUT*14, CJOUT*12, OUTBUF*132
    DIMENSION FLOW(6)
    LOGICAL FIRST
    XX0 = 0.0D0
    WRITE (IOFILO,5000)

    DO IRM = 1, N
        I = IRM
        FIRST = .TRUE.
        WRITE (CIOUT,'(A14)') compartmentnames(IRM)
        IF (IRM==N) CIOUT = 'Outside'

        !     HORIZONTAL FLOW NATURAL VENTS


        !     VERTICAL FLOW NATURAL VENTS


        !     MECHANICAL VENTS

        IF (NNODE/=0.AND.NEXT/=0) THEN
            DO I = 1, NEXT
                II = HVNODE(1,I)
                IF (II==IRM) THEN
                    INODE = HVNODE(2,I)
                    WRITE (CJOUT,'(A1,1X,A4,I3)') 'M', 'Node', INODE
                    DO IIi = 1, 4
                        FLOW(IIi) = XX0
                    end do
                    IF (HVEFLOt(UPPER,I)>=XX0) FLOW(1) = HVEFLOt(UPPER,I)
                    IF (HVEFLOt(UPPER,I)<XX0) FLOW(2) = -HVEFLOt(UPPER,I)
                    IF (HVEFLOt(LOWER,I)>=XX0) FLOW(3) = HVEFLOt(LOWER,I)
                    IF (HVEFLOt(LOWER,I)<XX0) FLOW(4) = -HVEFLOt(LOWER,I)
                    flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
                    flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
                    CALL FLWOUT(OUTBUF,FLOW(1),FLOW(2),FLOW(3),FLOW(4),flow(5),flow(6),xx0,xx0)
                    IF (FIRST) THEN
                        IF (I/=1) WRITE (IOFILO,5010)
                        WRITE (IOFILO,5020) CIOUT, CJOUT, OUTBUF
                        FIRST = .FALSE.
                    ELSE
                        WRITE (IOFILO,5020) ' ', CJOUT, OUTBUF
                    endif
                endif
            end do
        endif
    end do

5000 FORMAT (//,' Total mass flow through vents (kg)',/, &
    '0To             Through        ','      Upper Layer           ','    Lower Layer           ','   Trace Species',/, &
    ' Compartment    Vent             ',2('Inflow       Outflow      '),' Vented ', '   Filtered',/,' ', 104('-'))
5010 FORMAT (' ')
5020 FORMAT (' ',A14,1X,A12,1X,A)
    END

    SUBROUTINE RSLTCMP (iounit)

    !     Description:  Output a compressed output for 80 column screens

    use cenviro
    use cfast_main
    use objects2
    implicit none

    integer i, iounit, ir
    real*8 xx0, xemp, xqf

    XX0 = 0.0D0
    WRITE (iounit,5000)
    WRITE (iounit,5010)
    DO IR = 1, NM1
        XEMP = XX0
        XQF = XX0
        DO I = 0, NUMOBJL
            IF (IR==FROOM(I)) THEN
                XEMP = XEMP + FEMP(I)
                XQF = XQF + FQF(I)
            endif
        end do
        XQF = XQF + FQDJ(IR)
        IF (IZSHAFT(IR)==1) THEN
            WRITE (iounit,5031) IR, ZZTEMP(IR,UPPER)-273.15, XEMP, XQF, ZZRELP(IR) - PAMB(IR), ONTARGET(IR)
        ELSE
            WRITE (iounit,5030) IR, ZZTEMP(IR,UPPER)-273.15, ZZTEMP(IR,LOWER)-273.15, ZZHLAY(IR,LOWER), XEMP, XQF, ZZRELP(IR) - PAMB(IR),ONTARGET(IR)
        ENDIF
    end do
    WRITE (iounit,5020) FQDJ(N)
    RETURN

5000 FORMAT (' ')
5010 FORMAT (' Compartment   Upper   Lower   Inter.  Pyrol     Fire      Pressure  Ambient',/, &
    '               Temp.   Temp.   Height  Rate      Size                Target',/, &
    '               (C)     (C)     (m)     (kg/s)    (W)       (Pa)      (W/m^2)',/,' ',77('-'))
5020 FORMAT ('  Outside',39X,1PG10.3)
5030 FORMAT (I5,7X,2F8.1,2X,1PG8.2,1P4G10.3)
5031 FORMAT (I5,7X,F8.1,8(' '),2X,8(' '),1P4G10.3)
    END

    SUBROUTINE RSLTTAR(ISW,ITPRT)

    !     Description:  Output the temperatures and fluxes on surfaces and targets at the current time

    !     Arguments: ISW   1 if called from CFAST, 0 otherwise (effects printout of object names -- only CFAST knows actual names, others just do it by numbers
    !                ITPRT 1 if target printout specifically called for, 0 otherwise

    use cenviro
    use cfast_main
    use cshell
    use fltarget
    implicit none

    integer length, itprt, i, iw, itarg, itctemp, isw
    real*8 xx0, x100, ctotal, rtotal, ftotal, wtotal, gtotal, tg, tttemp, tctemp

    INTEGER IWPTR(4)
    EXTERNAL LENGTH
    DATA IWPTR /1, 3, 4, 2/
    XX0 = 0.0D0
    X100 = 100.0D0
    IF ((ITPRT==0.AND.NTARG<=NM1).OR.NTARG==0) RETURN
    WRITE (IOFILO,5000)
    DO I=1,NM1
        ITARG = NTARG-NM1+I
        if (validate.or.netheatflux) then
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
        endif
        IF (RTOTAL/=XX0) THEN
            WRITE (iofilo,5010) compartmentnames(I),((TWJ(1,I,IWPTR(IW))-273.15),IW=1,4),TWJ(1,I,2)-273.15, &
            RTOTAL,FTOTAL/RTOTAL*X100,WTOTAL/RTOTAL*X100,GTOTAL/RTOTAL*X100,CTOTAL/RTOTAL*X100
        ELSE
            WRITE (iofilo,5010) compartmentnames(I),(TWJ(1,I,IWPTR(IW))-273.15,IW=1,4),TWJ(1,I,2)-273.15
        endif
        IF (NTARG>NM1) THEN
            DO ITARG = 1, NTARG-NM1
                IF (IXTARG(TRGROOM,ITARG)==I) THEN
                    TG = TGTARG(ITARG)
                    TTTEMP = XXTARG(TRGTEMPF,ITARG)
                    ITCTEMP = (TRGTEMPF+TRGTEMPB)/2
                    if (IXTARG(TRGEQ,ITARG)==CYLPDE) ITCTEMP = TRGTEMPF+ xxtarg(trginterior,itarg)*(TRGTEMPB-TRGTEMPF)
                    TCTEMP = XXTARG(ITCTEMP,ITARG)
                    IF (IXTARG(TRGEQ,ITARG)==ODE) TCTEMP = TTTEMP
                    IF (IXTARG(TRGMETH,ITARG)==STEADY) TCTEMP = TTTEMP
                    if (validate.or.netheatflux) then
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
                    endif
                    IF (RTOTAL/=XX0) THEN
                        WRITE(IOFILO,5030)ITARG,TG-273.15,TTTEMP-273.15, TCTEMP-273.15, &
                        RTOTAL,FTOTAL/RTOTAL*X100,WTOTAL/RTOTAL*X100,GTOTAL/RTOTAL*X100,CTOTAL/RTOTAL*X100
                    ELSE
                        WRITE(IOFILO,5030)ITARG,TG-273.15,TTTEMP-273.15,TCTEMP-273.15
                    endif
                endif
            end do
        endif
    end do
    RETURN
5000 FORMAT (//,' Surfaces and Targets',/, &
    '0Compartment    Ceiling   Up wall   Low wall  Floor    Target    Gas       Surface   Center   Flux To      Fire      Surface   Gas',/, &
    '                Temp.     Temp.     Temp.     Temp.              Temp.     Temp.     Temp.    Target       Rad.      Rad.      Rad.      Convect.',/, &
    '                (C)       (C)       (C)       (C)                (C)       (C)       (C)      (W/m^2)      (%)       (%)       (%)       (%)',/,1X,144('-'))
5010 FORMAT (1x,a14,1P4G10.3,1X,'Floor',12X,1PG10.3,11X,1PG10.4,0PF7.1,3(3X,F7.1))
5030 FORMAT (55X,I4,4X,1P3G10.3,1X,1PG10.4,0PF7.1,3(3X,F7.1))
    END
    SUBROUTINE RSLTSPRINK

    !     Description:  Output the conditions of and at a sprinkler location (temperature, velocities etc) at the current time

    use cenviro
    use cfast_main
    use cshell
    implicit none

    integer i, iroom, itype
    real*8 ctotal, tctemp, cjetmin, zdetect, tlay, tjet, vel, tlink

    CHARACTER*5 CTYPE
    CHARACTER*3 CACT

    IF(NDTECT==0)RETURN
    write(iofilo,5000)
5000 format(//' Sensors',/, &
    '0                             Sensor                           Smoke',/, &
    ' Number  Compartment   Type   Temp (C)   Activated       Temp (C)   Vel (M/S)',/, &
    ' ----------------------------------------------------------------------------')
    CJETMIN = 0.10D0
    DO I = 1, NDTECT
        IROOM = IXDTECT(I,DROOM)

        ZDETECT = XDTECT(I,DZLOC)
        IF(ZDETECT>ZZHLAY(IROOM,LOWER))THEN
            TLAY = ZZTEMP(IROOM,UPPER)
        ELSE
            TLAY = ZZTEMP(IROOM,LOWER)
        ENDIF

        TJET = MAX(XDTECT(I,DTJET),TLAY)-273.15
        VEL = MAX(XDTECT(I,DVEL),CJETMIN)
        TLINK =  XDTECT(I,DTEMP)-273.15

        ITYPE = IXDTECT(I,DTYPE)
        IF(ITYPE==SMOKED)THEN
            CTYPE = 'SMOKE'
        ELSEIF(ITYPE==HEATD)THEN
            CTYPE = 'HEAT'
        ELSE
            CTYPE = 'OTHER'
        ENDIF
        CACT = 'NO'
        IF(IXDTECT(I,DACT)==1)CACT = 'YES'
        WRITE(IOFILO,5010)I,IROOM,CTYPE,TLINK,CACT,TJET,VEL
5010    FORMAT(T2,I2,T10,I3,T24,A5,T31,1PE10.3,T42,A3,T58,1PE10.3,T69,1PE10.3)
    end do
    RETURN
    END
    SUBROUTINE RSLTHALL(TIME)

    !     Description:  Output the conditions for each hall

    use cenviro
    use cfast_main
    use cshell
    implicit none

    integer :: nhalls, i
    real*8 :: tstart, vel, depth, dist, time


    nhalls = 0
    do i = 1, nm1
        if(izhall(i,ihroom)==1)nhalls = nhalls + 1
    end do
    if(nhalls==0)return
    write(iofilo,5000)
5000 FORMAT (//,' Hall Flow',// &
    ' Compartment  Start Time     Velocity       Depth        Distance',/, &
    '                 (s)          (m/s)          (m)            (m)'/ &
    '-----------------------------------------------------------------')

    DO 20 I = 1, NM1
        IF(IZHALL(I,IHROOM)==0)GO TO 20
        TSTART = ZZHALL(I,IHTIME0)
        VEL = ZZHALL(I,IHVEL)
        DEPTH = ZZHALL(I,IHDEPTH)
        DIST = ZZHALL(I,IHDIST)
        IF(DIST>ZZHALL(I,IHMAXLEN))DIST = ZZHALL(I,IHMAXLEN)
        WRITE(IOFILO,30)I,TSTART,VEL,DEPTH,DIST
30      FORMAT(4x,I2,7x,1PG10.3,5x,1PG10.3,3x,1PG10.3,5x,1PG10.3)
20  CONTINUE

    RETURN
    END

    SUBROUTINE outinitial(ISW)

    !     Description:  Output initial test case description
    !     Arguments: ISW   Print switch for restart option, 1=Print

    use cfast_main
    use cshell
    use iofiles
    implicit none

    CHARACTER CHKSUM*8
    EXTERNAL LENGTH
    integer imajor, iminor, iminorrev, isw, length

    call splitversion(version,imajor,iminor,iminorrev)

    IF (.not.HEADER) THEN
        if (iminorrev>=10) then
            WRITE (iofilo,10) imajor, iminor, iminorrev, CRDATE(1), CRDATE(2), CRDATE(3), MPSDAT(1), MPSDAT(2), MPSDAT(3)
        else
            WRITE (iofilo,20) imajor, iminor, iminorrev, CRDATE(1), CRDATE(2), CRDATE(3), MPSDAT(1), MPSDAT(2), MPSDAT(3)
        endif
    endif

    WRITE (IOFILO,5000) trim(inputfile), trim(title)
    IF (outputformat>1) THEN
        CALL OUTOVER
        CALL OUTAMB
        CALL OUTCOMP
        CALL OUTVENT
        CALL OUTTHE
        CALL OUTTARG (1)
        CALL OUTFIRE
    endif

    RETURN

5000 FORMAT (' Data file is ',A,'    Title is ',A)
10  FORMAT (' CFAST Version ',i1,'.',i1,'.',I2,' built ',I4.4,'/',I2.2,'/',I2.2,', run ',I4.4,'/',I2.2,'/',I2.2,/)
20  FORMAT (' CFAST Version ',i1,'.',i1,'.',I1,' built ',I4.4,'/',I2.2,'/',I2.2,', run ',I4.4,'/',I2.2,'/',I2.2,/)
    END

    SUBROUTINE OUTOVER

    !     Description:  Output initial test case overview

    use cfast_main
    use cshell
    use vents
    implicit none

    CHARACTER CHJET(4)*7, CJBUF*51
    DATA CHJET /'off', 'ceiling', 'wall', 'all'/
    integer :: jpos

    WRITE (IOFILO,5000) 
    WRITE (IOFILO,5010) NM1, NVENTS, NVVENT, NEXT
    WRITE (IOFILO,5020) NSMAX, LPRINT, LDIAGO, ldiago, lcopyss
    IF (CJETON(5)) THEN
        IF (CJETON(1)) THEN
            IF (CJETON(3)) THEN
                JPOS = 3
            ELSE
                JPOS = 1
            endif
        ELSE IF (CJETON(3)) THEN
            JPOS = 2
        endif
        WRITE (CJBUF,'(''on for '',A7)') CHJET(JPOS+1)
    ELSE
        WRITE (CJBUF,'(''off for all surfaces.'')')
    endif
    WRITE (IOFILO,5030) CJBUF
    IF (NDUMPR>0.AND.DUMPF/=' ') WRITE (IOFILO,5040) DUMPF
    RETURN

5000 FORMAT (//,' OVERVIEW',/)
5010 FORMAT ('0Compartments    Doors, ...    Ceil. Vents, ...    MV Connects',/,'0',I4,12X,I4,10X,I4,17X,I4)
5020 FORMAT ('0Simulation    ',' Output     ','History     Smokeview',2x'Spreadsheet',/, &
    '    Time       ',4('Interval    '),/,3x,' (s)          ',4('(s)         '),//,' ',I6,6X,4(I6,6X))
5030 FORMAT ('0Ceiling jet is ',A)
5040 FORMAT (' History file is ',A)
    END

    SUBROUTINE OUTAMB

    !     Description:  Output initial test case ambient conditions

    use cfast_main
    use cshell
    use params
    implicit none

    WRITE (IOFILO,5000) TA-273.15, PA + POFSET, EXTA-273.15, EXPA + POFSET, SAL, WINDV, WINDRF, WINDPW
    RETURN

5000 FORMAT (//,' AMBIENT CONDITIONS',//, &
    ' Interior       Interior       Exterior       Exterior       Station        Wind           Wind           Wind           ',/, &
    ' Temperature    Pressure       Temperature    Pressure       Elevation      Speed          Ref. Height    Power',/,' ', &
    '  (C)            (Pa)           (C)            (Pa)           (m)            (m/s)          (m)', &
    //,' ',2(F7.0,8X,F9.0,6X),F7.2,6X,2(F7.1,8X),F7.2)
    END

    SUBROUTINE OUTCOMP

    !     Description:  Output initial test case geometry

    use cfast_main
    use cshell
    implicit none

    integer i

    WRITE (IOFILO,5000)
    DO I = 1, NM1
        WRITE (IOFILO,5010) I, compartmentnames(i), BR(I), DR(I), HR(I), AR(I), VR(I), HRP(I), HFLR(I)
    end do
    RETURN
5000 FORMAT (//,' COMPARTMENTS',//, &
    ' Compartment  Name           Width     Depth     Height    Area      Volume    Ceiling   Floor     ',/, &
    '                                                                               Height    Height    ',/, & 
    ' ',29X,3('(m)',7X),'(m^2)     ','(m^3)      ',2('(m)',7X),/,' ',96('-'))
5010 FORMAT (' ',I5,8x,a13,7(F7.2,3X))
    END

    SUBROUTINE OUTVENT

    !     Description:  Output initial test case vent connections

    use cfast_main
    use cshell
    use params
    use vents
    implicit none

    integer :: i,j,k,iijk, isys, ibr, irm, iext
    real*8 hrx, hrpx

    CHARACTER CIOUT*8, CJOUT*14, CSOUT*6
    LOGICAL FIRST

    !     HORIZONTAL FLOW VENTS
    IF (NVENTS==0) THEN
        WRITE (IOFILO,5000)
    ELSE
        WRITE (IOFILO,5010)
        DO I = 1, NM1
            DO J = I + 1, N
                DO K = 1, 4
                    WRITE (CJOUT,'(a14)') compartmentnames(J)
                    IF (J==N) CJOUT = ' Outside'
                    IF (IAND(1,ISHFT(NW(I,J),-K))/=0) THEN
                        IIJK = IJK(I,J,K)
                        WRITE (IOFILO,5020) compartmentnames(I), CJOUT, K, BW(IIJK), HL(IIJK),HH(IIJK), HLP(IIJK), HHP(IIJK), (HHP(IIJK)-HLP(IIJK)) * BW(IIJK)
                    endif
                end do
            end do
        end do
    endif

    !     VERTICAL FLOW VENTS
    IF (NVVENT==0) THEN
        WRITE (IOFILO,5030)
    ELSE
        WRITE (IOFILO,5040)
        DO I = 1, N
            DO J = 1, N
                IF (NWV(I,J)/=0) THEN
                    WRITE (CIOUT,'(I5,3X)') I
                    IF (I==N) CIOUT = ' Outside'
                    WRITE (CJOUT,'(I5,3X)') J
                    IF (J==N) CJOUT = ' Outside'
                    CSOUT = 'Round'
                    IF (VSHAPE(I,J)==2) CSOUT = 'Square'
                    IF (J<N) THEN
                        HRX = HR(J)
                        HRPX = HRP(J)
                    ELSE
                        HRX = HRL(I)
                        HRPX = HRL(I)
                    endif
                    WRITE (IOFILO,5050) CIOUT, CJOUT, CSOUT, VVAREA(I,J), HRX,HRPX
                endif
            end do
        end do
    endif

    !     MECHANICAL VENTS
    IF (NNODE==0.AND.NEXT==0) THEN
        WRITE (IOFILO,5060)
    ELSE

        !     FANS

        WRITE (IOFILO,5120)
        DO ISYS = 1, NHVSYS
            FIRST = .TRUE.
            DO IBR = 1, NBR
                IF (IZHVBSYS(IBR)==ISYS) THEN
                    IF (NF(IBR)/=0) THEN
                        CALL CHKEXT(NA(IBR),IRM,IEXT)
                        IF (IRM>=1.AND.IRM<=N) THEN
                            WRITE (CIOUT,'(A4,I3)') 'Comp', IRM
                            IF (IRM==N) CIOUT = 'Outside'
                            WRITE (CJOUT,'(A4,I3)') 'Node', NA(IBR)
                            IF (FIRST) THEN
                                WRITE (IOFILO,5100) ISYS, CIOUT, HVELXT(IEXT), CJOUT, HVGHT(NA(IBR)), AREXT(IEXT)
                                FIRST = .FALSE.
                            ELSE
                                WRITE (IOFILO,5110) CIOUT, HVELXT(IEXT), CJOUT, HVGHT(NA(IBR)), AREXT(IEXT)
                            endif
                        endif
                        IF (FIRST) THEN
                            WRITE (IOFILO,5130) ISYS, 'Node', NA(IBR), HVGHT(NA(IBR)), 'Node', NE(IBR), HVGHT(NE(IBR)), &
                            NF(IBR), HMIN(NF(IBR)), HMAX(NF(IBR)), (HVBCO(NF(IBR),J),J = 1,NFC(NF(IBR)))
                            FIRST = .FALSE.
                        ELSE
                            WRITE (IOFILO,5140) 'Node', NA(IBR), HVGHT(NA(IBR)), 'Node', NE(IBR), HVGHT(NE(IBR)), NF(IBR), &
                            HMIN(NF(IBR)), HMAX(NF(IBR)), (HVBCO(NF(IBR),J),J= 1,NFC(NF(IBR)))
                        endif
                        CALL CHKEXT(NE(IBR),IRM,IEXT)
                        IF (IRM>=1.AND.IRM<=N) THEN
                            WRITE (CIOUT,'(A4,I3)') 'Node', NE(IBR)
                            WRITE (CJOUT,'(A4,I3)') 'Comp', IRM
                            IF (IRM==N) CJOUT = 'Outside'
                            IF (FIRST) THEN
                                WRITE (IOFILO,5100) ISYS, CIOUT, HVGHT(NE(IBR)), CJOUT, HVELXT(IEXT), AREXT(IEXT)
                                FIRST = .FALSE.
                            ELSE
                                WRITE (IOFILO,5110) CIOUT, HVGHT(NE(IBR)), CJOUT, HVELXT(IEXT), AREXT(IEXT)
                            endif
                        endif
                    endif
                endif
            end do
        end do
    endif
    RETURN

5000 FORMAT (//,' VENT CONNECTIONS',//,' There are no horizontal natural flow connections')
5010 FORMAT (//,' VENT CONNECTIONS',//,' Horizontal Natural Flow Connections (Doors, Windows, ...)',//, &
    ' From           ','To             ','Vent      ','Width     ','Sill      ','Soffit    ','Abs.      ','Abs.      ','Area',/, & 
    ' ','Compartment    ','Compartment    ','Number    ',10X,'Height    ','Height    ','Sill      ','Soffit',/,' ',40X,5('(m)       '),1('(m^2)',5X),/,' ',100('-'))
5020 FORMAT (' ',a14,1X,A14,I3,5X,6(F7.2,3X))
5030 FORMAT (//,' There are no vertical natural flow connections')
5040 FORMAT (//,' Vertical Natural Flow Connections (Ceiling, ...)',//,' Top            Bottom         Shape     Area      ','Relative  Absolute',/,' ', &
    'Compartment    Compartment                        Height    Height',/,' ',40X,'(m^2)     ',2('(m)       '),/,' ',70('-'))
5050 FORMAT (' ',A8,7X,A8,7X,A6,2X,3(F7.2,3X))
5060 FORMAT (//,' There are no mechanical flow connections')
5070 FORMAT (//' Mechanical Flow Connections (Fans, Ducts, ...)',//, 'Connections and Ducts',//, &
    ' System    From           From      To             To        Length    Area      Rough',/,' ', &
    '                         Elev.                    Elev.',/,' ',25X,'(m)                      (m)       (m)   ','    (m^2)     (mm)',/,' ',86('-'))
5080 FORMAT (' ',I4,6X,A4,I3,5X,F7.2,6X,A4,I3,5X,4(F7.2,3X))
5090 FORMAT (' ',10X,A4,I3,5X,F7.2,6X,A4,I3,5X,4(F7.2,3X))
5100 FORMAT (' ',I4,6X,A7,5X,F7.2,6X,A7,5X,F7.2,13X,F7.2)
5110 FORMAT (' ',10X,A7,5X,F7.2,6X,A7,5X,F7.2,13X,F7.2)
5120 FORMAT (//,' Fans',//,' System    From           From      To             To        Fan       Minimum   Maximum    Fan Curve',/, &
    ' ','                         Elev.                    Elev.','     Number',/,' ',25X, &
    '(m)                      (m)             ','    (Pa)      (Pa)',/,' ',100('-'))
5130 FORMAT (' ',I4,6X,A4,I3,5X,F7.2,6X,A4,I3,5X,F7.2,6X,I3,6X,2(F7.2,3X),1P5G10.2)
5140 FORMAT (' ',10X,A4,I3,5X,F7.2,6X,A4,I3,5X,F7.2,6X,I3,6X,2(F7.2,3X),1P5G10.2)
5150 FORMAT (' ')
    END

    SUBROUTINE CHKEXT(IND,IRM,IEXT)

    !     Description:  Check if an HVAC node is a connection to an external room

    !     Arguments: IND   Node number to check
    !                IRM   Room number if node is an external connection
    !                IEXT  External node number is node is an external connection

    use cfast_main
    implicit none

    integer :: i, ind, iext, irm

    DO 10 I = 1, NEXT
    IF (HVNODE(2,I)==IND) THEN
    IEXT = I
    IRM = HVNODE(1,I)
    RETURN
    endif
10  CONTINUE
    IRM = 0
    IEXT = 0
    RETURN
    END

    SUBROUTINE OUTTHE

    !     Description:  Output initial test case thermal properties

    use cfast_main
    use cshell
    use thermp
    implicit none

    integer i, j, k

    CHARACTER WALL(4)*7
    DATA WALL /'ceiling', 'floor', 'wall', 'wall'/

    ! check to see if any heat transfer is on

    DO 20 I = 1, NM1
    DO 10 J = 1, NWAL
    IF (SWITCH(J,I).AND.CNAME(J,I)/=' ') GO TO 30
10  CONTINUE
20  CONTINUE
    WRITE (IOFILO,5000)
    RETURN

    ! some surfaces are on, do the printout of the surfaces

30  WRITE (IOFILO,5010)
    DO 40 I = 1, NM1
    WRITE (IOFILO,5020) compartmentnames(I), CNAME(1,I), CNAME(3,I),CNAME(2,I)
40  CONTINUE

    !     PRINT OUT THE PROPERTIES OF THE MATERIALS USED

60  WRITE (IOFILO,5030) THRMFILE
    DO 80 I = 1, MAXCT
    WRITE (IOFILO,5040) NLIST(I), LFKW(1,I), LCW(1,I), LRW(1,I), LFLW(1,I), LEPW(I), (LHCLBF(K,I),K = 1,5) 
    DO 70 J = 2, LNSLB(I)
    WRITE (IOFILO,5050) LFKW(J,I), LCW(J,I), LRW(J,I), LFLW(J,I)
70  CONTINUE
80  CONTINUE
    WRITE (IOFILO,5060)
5000 FORMAT (//,' Heat transfer for all surfaces is turned off')
5010 FORMAT (//,' THERMAL PROPERTIES',//,' ','Compartment    Ceiling      Wall         Floor',/,' ',70('-'))
5020 FORMAT (' ',a13,3(A10,3X))
5030 FORMAT (//,' Thermal data base used: ',A20,//,' Name',4X,'Conductivity',1X,'Specific heat',5X,'Density',5X,'Thickness',3X,'Emissivity',16X,'HCL B''s (1->5)')
5040 FORMAT (' ',A8,1P5G13.3,5E10.2)
5050 FORMAT (' ',8X,1P4G13.3)
5060 FORMAT (' ')

    END

    SUBROUTINE OUTFIRE

    !     routine: outfire
    !     purpose: This routine outputs the fire specification for all the object fires
    !     Arguments: none

    use cfast_main
    use cshell
    use objects1
    use objects2
    use params
    implicit none

    integer io, i, j, nnv, length, is
    real*8 y_hcn, y_hcl

    character cbuf*255, stype(ns)*5, ftype(0:4)*13
    external length
    data ftype /'Undefined', 'Unconstrained', 'Constrained','Pool Fire', 'Furniture'/
    data stype /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O', 'OD', 'CT', 'TS'/

    if (numobjl>0) then
    do io = 1, mxoin
    if (objpnt(io)/=0) then
    j = objpnt(io)
    nnv = objlfm(j)
    write (iofilo,5020) objnin(j)(1:length(objnin(j))), j
    write (iofilo,5030) compartmentnames(objrm(j)),ftype(objtyp(j)),objpos(1,j), objpos(2,j), objpos(3,j), relhum * 100., limo2 * 100.,radconsplit(j)
    write (iofilo,5031) obj_c(j), obj_h(j), obj_o(j), obj_n(j), obj_cl(j)
    write (cbuf,5040)
    write (cbuf(51:132),5050)
    is = 113
    write (iofilo,'(3x,a)') cbuf(1:length(cbuf))
    write (iofilo,5000) ('(kg/kg)',i = 1,(is-51)/10)
    write (iofilo,5010) ('-',i = 1,is-1)
    do i = 1, nnv
    write (cbuf,5060) otime(i,j), omass(i,j), objhc(i,j), oqdot(i,j), ohigh(i,j)
    y_HCN = obj_n(j)*0.027028d0/objgmw(j)
    y_HCl = obj_cl(j)*0.036458d0/objgmw(j)
    write (cbuf(51:132),5070) ood(i,j), oco(i,j), y_HCN, y_HCl,omprodr(i,10,j),omprodr(i,11,j)
    write (iofilo,'(1x,a)') cbuf(1:length(cbuf))
    end do
    endif
    end do
    endif
    return
5000 FORMAT ('   (s)       (kg/s)    (J/kg)    (W)       (m)       ',15(A7,3X))
5010 FORMAT (' ',255A1)
5020 FORMAT (//,' Name: ',A,'   Referenced as object #',I3,//,' Compartment    Fire Type    ','   Position (x,y,z)     Relative    Lower O2    Radiative',/,' ',52X,'Humidity    Limit       Fraction')
5030 format (1x,a14,1x,A13,3(F7.2),F7.1,6X,F7.2,5X,F7.2//)
5031 format (' Chemical formula of the fuel',/,3x,'Carbon    Hydrogen  Oxygen    Nitrogen  Chlorine',/,1x,5(f7.3,3x),//)
5040 format ('Time      Fmdot     Hcomb     Fqdot     Fheight   ')
5050 format ('Soot      CO        HCN       HCl       CT        TS')
5060 format (F7.0,3X,1P4G10.2)
5070 format (1P10G10.2,2x,2g10.2)
    end subroutine outfire

    CHARACTER*8 FUNCTION CHKSUM(FILE)
    implicit none
    CHARACTER*(*) FILE
    CHKSUM = '00000000'
    RETURN
    END

    SUBROUTINE OUTTARG(ISW)

    !      Description:  Output initial test case target specifications

    use cfast_main
    use cshell
    use fltarget
    implicit none

    integer :: itarg, isw, j

    CHARACTER CBUF*255

    IF(NTARG/=0)WRITE(IOFILO,5000)
5000 FORMAT(//,' TARGETS',//,' Target',T9,'Compartment',T24,'Position (x, y, z)',T51,'Direction (x, y, z)',T76,'Material',/,1X,82('-'))

    DO 10 ITARG = 1, NTARG
        IF (ITARG<NTARG-NM1+1) THEN
            CBUF = CXTARG(ITARG)
        ELSE IF (ITARG>=NTARG-NM1+1.AND.ISW/=1) THEN
            WRITE (CBUF,5004) ITARG-(NTARG-NM1)
        ELSE 
            WRITE (CBUF,5005) CXTARG(ITARG),ITARG-(NTARG-NM1)
        endif
5004    FORMAT ('Floor, compartment ',I2)
5005    FORMAT (A8,'  Floor, compartment ',I2)
        !      IF (ITARG==NTARG-NM1+1) WRITE (IOFILO,5006)
5006    FORMAT (' ')
        WRITE(IOFILO,5010)ITARG,compartmentnames(IXTARG(TRGROOM,ITARG)),(XXTARG(TRGCENX+J,ITARG),J=0,2),(XXTARG(TRGNORMX+J,ITARG),J=0,2),CBUF(1:8)
5010    FORMAT(' ',I5,T11,a14,T21,6(F7.2,2X),T76,A8)
10  CONTINUE
    RETURN
    END

    SUBROUTINE FLWOUT(OUTBUF,FLOW1,FLOW2,FLOW3,FLOW4,FLOW5,FLOW6,flow7,flow8)

    !     Description:  Stuff the flow output after blanking appropriate zeros

    use cparams
    use solver_parameters
    implicit none

    integer :: i
    real*8 FLOW(8), flow1, flow2, flow3, flow4, flow5, flow6, flow7, flow8, x1000,x100,x10,x1,x01
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
        IF (FLOW(I)>=X1000) THEN
            WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        ELSE IF (FLOW(I)>=X100) THEN
            WRITE (OUTBUF(13*(I-1)+1:13*I),5010) FLOW(I)
        ELSE IF (FLOW(I)>=X10) THEN
            WRITE (OUTBUF(13*(I-1)+1:13*I),5020) FLOW(I)
        ELSE IF (FLOW(I)>=X1) THEN
            WRITE (OUTBUF(13*(I-1)+1:13*I),5030) FLOW(I)
        ELSE IF (FLOW(I)>=X01) THEN
            WRITE (OUTBUF(13*(I-1)+1:13*I),5040) FLOW(I)
        ELSE
            WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        endif
        IF (FLOW(I)<=ATOL) OUTBUF(13*(I-1)+1:13*I) = ' '
10  CONTINUE
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

    use cfast_main
    use fltarget
    implicit none

    real*8 positionvector(*)
    integer targetnumber, i

    do i = 1, 6
        positionvector (i) = xxtarg(i,targetnumber)
    end do

    positionvector(1) = positionvector(1) + cxabs(ixtarg(trgroom,targetnumber))
    positionvector(2) = positionvector(2) + cyabs(ixtarg(trgroom,targetnumber))
    positionvector(3) = positionvector(3) + hrl(ixtarg(trgroom,targetnumber))

    return

    end subroutine getabstarget

    SUBROUTINE SETDBUG

    use cfin
    use cparams
    use cshell
    use opt
    include "precis.fi"

    INTEGER FUNIT
    CHARACTER DBUGFIL*8

    CHARACTER KEYWORD*7, DBUGKY(MXDEBUG)*7, DUMMY*1, LY*2
    DATA DBUGKY/'MASSFLW','HVACFLW','HORZFLW','VERTFLW','MVNTFLW','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX', &
    'ERRVCTR','PRNTJAC','PRNDPDT','XXXXXXX'/

    CLOSE (IOFILI)
    OPEN (UNIT=IOFILI,FILE=LBUF)
    READ(IOFILI,*,END=100) DUMMY
10  CONTINUE
    READ(IOFILI,*,END=100,err=10) KEYWORD, IROOM, ILAYER
    DO 20 I = 1, 15
        IF (KEYWORD==DBUGKY(I)) THEN
            IF (ILAYER>0.AND.ILAYER<=2.AND.IROOM>0.AND.IROOM<NR) THEN
                IOUNIT = FUNIT(70)
                IF (ILAYER==UPPER) THEN
                    LY = 'UP'
                ELSE
                    LY = 'LW'
                endif
                IF (I/=2) THEN
                    WRITE(DBUGFIL,'(A4,A2,I2.2)')DBUGKY(I),LY,IROOM
                ELSE
                    WRITE(DBUGFIL,'(A4,I2.2,I2.2)')DBUGKY(I),ILAYER,IROOM
                endif
                CALL OPNOTPT(DBUGFIL,IOUNIT)
                DBUGSW(I,ILAYER,IROOM) = IOUNIT
            endif
        endif
20  CONTINUE
    IF (KEYWORD==DBUGKY(D_JAC)) THEN
        CALL OPNDBG(IROOM,ILAYER)
    ELSE IF (KEYWORD==DBUGKY(16)) THEN
        IOUNIT = FUNIT(70)
        CALL OPNOTPT('ERRVECTR',IOUNIT)
        DBUGSW(16,1,1) = IOUNIT
    ELSE
    endif
    GO TO 10
100 CONTINUE
    CLOSE (IOFILI)
    RETURN
    END

    SUBROUTINE OUTJAC (TSEC, WM, NEQS)

    !     Description: prints out the magnitude of the jacobian matrix

    use cfast_main
    use cenviro
    use opt
    use wdervs
    include "precis.fi"

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

    !     NORMAL PROCESSING OF THE DEBUG OUTPUT

    IF (DBUGSW(D_JAC,D_PRN,1)<=0) RETURN
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
            IF (IOFFST(I)-IOFFST(I+1)==0) GO TO 111
            ITMP2 = ITMP2 + 1
            ITMP = IOFFST(I)*2 + 7 + ITMP2*2
            HDER(ITMP:(ITMP+2)) = LBLS(I)
111     CONTINUE
        IOUNIT = DBUGSW(D_JAC,D_PRN,1)
    endif
    XX0 = 0.0D0
    WRITE(IOUNIT,*)' '
    WRITE(IOUNIT,*)'JACOBIAN',NUMJAC + TOTJAC,' TIME ',TSEC
    WRITE(IOUNIT,*)' '
    WRITE(IOUNIT,'(A256)')HDER
    IRDX = 1
    DO 10 I = 1, NEQS
        IF (I>IOFFST(IRDX))THEN
            IRDX = IRDX + 1
101         CONTINUE   
            IF (I>IOFFST(IRDX)) THEN
                IRDX = IRDX + 1
                GO TO 101
            endif
            ITCOL = NEQS + 8
            DO 103 K = 1, ITCOL + 2
                ENTRY(K) = '--'
103         CONTINUE
            WRITE(IOUNIT,*)(ENTRY(K),K=1,ITCOL)
        endif
        ENTRY(1) = LBLS(IRDX-1)(1:2)
        ICDX = 1
        ITCOL = 1
        WMII = WM(I,I)
        IF(WMII/=XX0)THEN
            IITMP = LOG(ABS(WMII))
        ELSE
            IITMP = 11
        ENDIF
        IF (IITMP<VERYSM) THEN
            DDIAG = ' .'
        ELSE IF (IITMP>VERYBG) THEN
            DDIAG = ' Û'
        ELSE
            WRITE(DDIAG,'(I2)')IITMP
        endif

        DO 20 J = 1, NEQS
            ITCOL = ITCOL + 1
            IF (J>IOFFST(ICDX)) THEN
                ICDX = ICDX + 1
102             CONTINUE   
                IF (J>IOFFST(ICDX)) THEN
                    ICDX = ICDX + 1
                    GO TO 102
                endif
                ENTRY(ITCOL) = ' |'
                ITCOL = ITCOL + 1
            endif
            WMIJ = BUF(J)
            IF (WMIJ/=XX0.AND.WMII/=XX0) THEN
                TMP1 = ABS(WMIJ/WMII)
                TMP = LOG(TMP1)
            ELSE IF (WMII==XX0) THEN
                TMP = 11
            ELSE
                TMP = -11
            ENDIF
            ITMP = INT(TMP + 0.5D0)

            IF (WMIJ==0.0D0) THEN
                ENTRY(ITCOL) = '  '
            ELSE IF (ITMP<VERYSM) THEN
                ENTRY(ITCOL) = ' .'
            ELSE IF (ITMP>VERYBG) THEN
                ENTRY(ITCOL) = ' Û'
            ELSE
                WRITE(ENTRY(ITCOL),'(I2)')ITMP
            endif
20      CONTINUE
        WRITE(IOUNIT,*)DDIAG,':',(ENTRY(K),K=1,ITCOL)
10  CONTINUE
    RETURN
    END

    SUBROUTINE OUTJCNT (T)

    !     Description: Print out numerical performance data; resid counts,
    !                  jac counts, cpu times etc.

    use cparams
    use opt
    use wdervs
    include "precis.fi"

    LOGICAL FIRSTC
    DATA FIRSTC/.TRUE./
    SAVE IOUNIT

    IF (DBUGSW(D_JAC,D_CNT,1)<=0) RETURN
    IF (FIRSTC) THEN
        FIRSTC = .FALSE.
        IOUNIT = DBUGSW(D_JAC,D_CNT,1)
        WRITE(IOUNIT,1002)
1002    FORMAT(15X,'STEPS',4X,'JACOBIANS',5X,'RESIDS',4X,'NEWT ITERS',9X,'CPU',14X,'OVER HEAD',/,4X,'TIME',4X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM', &
2       X,'CUR',4X,'CUM',2X,'CUR',4X,'CUM',4X,'CUR',8X,'CUM',6X,'CUR',7X,'CUM')
    endif
    TOTJAC = TOTJAC + NUMJAC
    TOTSTEP = TOTSTEP + NUMSTEP
    TOTRESD = TOTRESD + NUMRESD
    NUMITR = NUMRESD - NUMJAC*JACDIM
    TOTITR = TOTITR + NUMITR
    WRITE(IOUNIT,1001)T,NUMSTEP,TOTSTEP,NUMJAC,TOTJAC,NUMRESD,TOTRESD,NUMITR,TOTITR,PRTTIME,TOTTIME,OVTIME,TOVTIME
1001 FORMAT(1X,1PE9.2,4(1X,I4,1X,I6),1X,1PE9.2,1X,1PE9.2,1X,1PE9.2,1X,1PE9.2)
    RETURN
    END

    SUBROUTINE OPNDBG (JACCNT, JACPRN)

    !     Description: opens a file on unit iounit

    !     Arguments: JACCNT
    !                JACPRN

    use cparams
    use opt
    include "precis.fi"

    INTEGER FUNIT
    LOGICAL FIRSTC
    CHARACTER*6 CNTFIL, PRNFIL

    SAVE FIRSTC
    DATA FIRSTC/.TRUE./,CNTFIL/'JACCNT'/,PRNFIL/'JACPRN'/

    IF (FIRSTC) THEN
        FIRSTC = .FALSE.
        IF (JACCNT>0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(CNTFIL,IOUNIT)
            DBUGSW(D_JAC,D_CNT,1) = IOUNIT
        endif
        IF (JACPRN>0) THEN
            IOUNIT = FUNIT(70)
            CALL OPNOTPT(PRNFIL,IOUNIT)
            DBUGSW(D_JAC,D_PRN,1) = IOUNIT
        endif
    endif
    RETURN
    END

    SUBROUTINE FND_COMP(IOUNIT,ICOMP)

    !     Arguments: IOUNIT
    !                ICOMP

    use cfast_main
    use cenviro
    use cfin
    use opt
    include "precis.fi"

    WRITE(LBUF,*)'Solution component with the greatest error is'
    CALL XERROR(LBUF,0,1,0)
    IF (ICOMP<=NOFP+NM1) THEN
        WRITE(LBUF,'(A18,I2)')' pressure in room ',icomp
        CALL XERROR(LBUF,0,1,0)
    ELSE IF (ICOMP<=NOFTU) THEN
        WRITE(LBUF,'(A18,I2)')' either HVAC or FSM ',icomp-nm1
        CALL XERROR(LBUF,0,1,0)
    ELSE IF (ICOMP<=NOFVU) THEN
        WRITE(LBUF,'(A27,I2)')' upper layer temp in room ',icomp-noftu
        CALL XERROR(LBUF,0,1,0)
    ELSE IF (ICOMP<=NOFTL) THEN
        WRITE(LBUF,'(A26,I2)')' upper layer vol in room ',icomp-nofvu
        CALL XERROR(LBUF,0,1,0)
    ELSE IF (ICOMP<=NOFTL+NM1) THEN
        WRITE(LBUF,'(A27,I2)')' lower layer temp in room ',icomp-noftl
        CALL XERROR(LBUF,0,1,0)
    ELSE IF (ICOMP<=NOFWT) THEN
        IF (OPTION(FOXYGEN)==ON) THEN
            WRITE(LBUF,'(A18,I2)')' oxygen component ',icomp-nofoxyl
            CALL XERROR(LBUF,0,1,0)
        ELSE
            WRITE(LBUF,'(A15,I2)')' target number ',icomp-noftt
            CALL XERROR(LBUF,0,1,0)
        ENDIF
    ELSE IF (ICOMP<=NOFPRD) THEN
        ITMP = ICOMP - NOFWT
        IRM = IZWALL(ITMP,1)
        IW = IZWALL(ITMP,2)
        IF (IW==1) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)') ' wall temp in room ',IRM,' ceiling '
            CALL XERROR(LBUF,0,1,0)
        ELSE IF(IW==2) THEN
            WRITE(LBUF,'(A18,I2,A9,I1)') ' wall temp in room ',IRM,' floor   '
            CALL XERROR(LBUF,0,1,0)
        ELSE IF(IW==3) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)') ' wall temp in room ',IRM,' upper wall '
            CALL XERROR(LBUF,0,1,0)
        ELSE IF(IW==4) THEN
            WRITE(LBUF,'(A18,I2,A12,I1)') ' wall temp in room ',IRM,' lower wall '
            CALL XERROR(LBUF,0,1,0)
        endif
    endif

    RETURN
    END

    SUBROUTINE DEBUGPR(IKEY,T,DT,IEQMAX)

    use cenviro
    use cfast_main
    use cshell
    use params
    use wnodes
    include "precis.fi"

    INTEGER*2 CH, HIT
    CHARACTER SPNAME(NS)*5, CCC*3
    INTEGER BMAP(MBR)
    LOGICAL FIRSTC
    DATA SPNAME /'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%','  TUH', ' H2O%', '   OD', '   CT', 'TS'/
    DATA FIRSTC /.TRUE./
    SAVE BMAP

    !     DEBUG PRINTING

    IF (FIRSTC) THEN
        FIRSTC = .FALSE.
        DO 30 I = 1, NBR
            DO 10 J = 1, NCNODE(NA(I))
                IF (I==ICMV(NA(I),J)) THEN
                    BMAP(I) = J
                    GO TO 20
                endif
10          CONTINUE
20          CONTINUE
30      CONTINUE
    endif

    IF (IKEY==1) THEN
        WRITE (*,*) 'Pause at time = ', T,',  Press any key to continue'
40      CALL GRABKY(CH,HIT)
        IF (HIT==0) GO TO 40
        WRITE (*,*) 'Continuing'
        WRITE (*,*)
    ELSE IF (IKEY==2) THEN
        WRITE (IOFILO,5000) T, DT
        DO 60 I = 1, NM1
            WRITE (*,5010) I
            WRITE (*,5020) '   Upper temp(K)', ZZTEMP(I,UPPER)
            WRITE (*,5020) '   Lower temp(K)', ZZTEMP(I,LOWER)
            WRITE (*,5020) ' Interface ht(m)', ZZHLAY(I,LOWER)
            WRITE (*,5020) '   Pressure (pa)', ZZRELP(I)
            IF (NLSPCT>0) WRITE (*,*) ' SPECIES MASS FRACTIONS ',' UPPER           LOWER'
            DO 50 IPROD = 1, NS
                IF (ACTIVS(IPROD)) THEN
                    WRITE (*,5030) SPNAME(IPROD), (ZZCSPEC(I,IL,IPROD),IL= UPPER,LOWER)
                endif
50          CONTINUE
            IF (NWALLS/=0) WRITE (*,*) ' WALL TEMPERATURES'
            IF (SWITCH(1,I)) THEN
                WRITE (*,5040) ZZWTEMP(I,1,1)
            endif
            IF (SWITCH(3,I)) THEN
                WRITE (*,5060) ZZWTEMP(I,3,1)
            endif
            IF (SWITCH(4,I)) THEN
                WRITE (IOFILO,5070) ZZWTEMP(I,4,1)
            endif
            IF (SWITCH(2,I)) THEN
                WRITE (IOFILO,5050) ZZWTEMP(I,2,1)
            endif
60      CONTINUE
        WRITE (*,*) ' '
        WRITE (*,*) 'HVAC PRINT BY SYSTEMS'
        DO 90 ISYS = 1, NHVSYS
            WRITE (*,*) 'FOR SYSTEM ', ISYS
            WRITE (*,*) 'MASS FLOW OF SYSTEM ', HVMFSYS(ISYS)
            WRITE (*,*) 'MASS OF GAS IN SYSTEM ', ZZHVM(ISYS)
            DO 70 IPROD = 1, NS
                WRITE (*,*) 'MASS OF ', SPNAME(IPROD), ' ',ZZHVPR(ISYS,IPROD)
70          CONTINUE
            DO 80 IDT = 1, NBR
                IF (IZHVBSYS(IDT)==ISYS) THEN
                    WRITE (*,5080) NA(IDT), HVP(NA(IDT)), NE(IDT),HVP(NE(IDT)), HVFLOW(NA(IDT),BMAP(IDT)), TBR(IDT)
                endif
80          CONTINUE
90      CONTINUE
        IF (NDTECT/=0)THEN
            WRITE(*,*)'DETECTOR INFO'
            WRITE(*,100)
100         FORMAT('  N ',3X,'D TEMP',6X,'J TEMP',6X,' ACT')
            DO 101 I = 1, NDTECT
                IROOM = IXDTECT(I,DROOM)
                IF (IQUENCH(IROOM)==I)THEN
                    CCC='***'
                ELSE
                    CCC = '   '
                ENDIF
                WRITE(*,102)I,XDTECT(I,DTEMP),XDTECT(I,DTJET),XDTECT(I,DVEL),XDTECT(I,DTACT),CCC
102             FORMAT(1X,I2,1X,4(E11.4,1X),A3)
101         CONTINUE
        ENDIF
        WRITE (*,*) ' '
    ELSE IF (IKEY==3) THEN
        WRITE (*,5090) T, DT
        CALL FND_COMP(IOFILO,IEQMAX)
        WRITE(*,6030)
        DO 201 IROOM = 1, NM1
            WRITE(*,6000)IROOM,ZZRELP(IROOM),ZZHLAY(IROOM,LOWER),ZZTEMP(IROOM,LOWER),ZZTEMP(IROOM,UPPER),ZZCSPEC(IROOM,LOWER,2),ZZCSPEC(IROOM,UPPER,2)
201     CONTINUE
        IF(NHVPVAR>0)WRITE(*,6010)(P(NOFPMV+I),I=1,NHVPVAR)
        IF(NHVTVAR>0)WRITE(*,6020)(P(NOFTMV+I),I=1,NHVTVAR)
        IF(NNODE>0)WRITE(*,6040)
        DO 210 I = 1, NNODE
            DO 220 J = 1, NCNODE(I)
                DP = HVP(MVINTNODE(I,J)) - HVP(I) + DPZ(I,J)
                WRITE(*,6050) I,MVINTNODE(I,J),DP,HVP(I),HVP(MVINTNODE(I,J)), HVGHT(I)
220         CONTINUE
210     CONTINUE
        WRITE(*,6070)
        DO 230 IROOM = 1, NM1
            XQF = 0.
            DO 202 IOBJ = 0, NUMOBJL
                IF (IROOM==FROOM(IOBJ))XQF = XQF + FQF(IOBJ)
202         CONTINUE
            XQF = XQF + FQDJ(IROOM)
            WRITE(*,6060)IROOM,ZZWTEMP(IROOM,1,1),ZZWTEMP(IROOM,3,1),ZZWTEMP(IROOM,4,1),ZZWTEMP(IROOM,2,1),XQF
230     CONTINUE
        IF(NUMOBJL>0)THEN
            WRITE(*,6080)
            DO 240 IOBJ = 1, NUMOBJL
                WRITE(*,6085)IOBJ,XFIRE(IOBJ,10),XFIRE(IOBJ,11)
240         CONTINUE
        ENDIF
        IF(NTARG>0)THEN
            WRITE(*,6090)
            DO 250 ITARG = 1, NTARG
                WRITE(*,6095)ITARG,XXTARG(TRGTEMPF,ITARG)
250         CONTINUE
        ENDIF
    endif
    RETURN

5000 FORMAT (' T = ',1PG12.4,' DT = ',1PG12.4)
5010 FORMAT (' For room ',I3,' at time      T ')
5020 FORMAT (A16,5X,E14.7,3X,E14.7)
5030 FORMAT (15X,A5,1X,2(E14.7,3X))
5040 FORMAT ('  Ceiling temp(K) ',F12.2)
5050 FORMAT ('  Floor   temp(K) ',F12.2)
5060 FORMAT ('  Up wall temp(K) ',F12.2)
5070 FORMAT (' Low wall temp(K) ',E12.2)
5080 FORMAT (' from ',I2,' pressure ',E10.3,' to ',I2,' pressure ',G10.3,' mass flow is ',G10.3,' temp ',G10.3)
5090 FORMAT (' Returned from dassl at T = ',1PG14.6,',  dt = ',1PG12.4)
5095 FORMAT (' Solution Component with the most error: ',I3)
6000 FORMAT(1X,I3,1X,6E13.6)
6010 FORMAT(' HVAC   PRESSURES:',4E13.6)
6020 FORMAT(' HVAC TEMPERATUES:',4E13.6)
6030 FORMAT(T2,'ROOM',T9,'PRESSURE',T20,'LAYER HEIGHT',T35,'L. TEMP',T48,'U. TEMP',T62,'L. Oxy',T75,'U. Oxy')
6040 FORMAT(T3,'NODES',T12,'DELTA P',T23,'P AT FIRST NODE',T39,'P AT 2ND NODE',T57,'HEIGHT')
6050 FORMAT(1X,2I3,1X,4(E13.6,2x))
6060 FORMAT(1X,I3,1X,5E13.6)
6070 FORMAT(T2,'ROOM',T11,'Ceiling',T21,'Upper Wall',T36,'Lower Wall',T49,'Floor',T61,'Fire Size')
6080 FORMAT(T2,'Object',T11,'Heat in lower ',T26,'Heat in upper')
6085 FORMAT(1X,I2,4X,2E13.6)
6090 FORMAT(T2,'Target',T11,'Temp')
6095 FORMAT(1X,I2,4X,E13.6)

    END

    SUBROUTINE DUMPER(ISTEP,IERROR)

    !     Description:  Saves the data files in packed binary format
    !
    !     Arguments: ISTEP  Current time step
    !                IERROR Returns error code

    use cfast_main
    use cshell
    use iofiles
    include "precis.fi"

    INTEGER IOUNIT, ITOT
    DATA IOUNIT /11/, FIRSTC /.TRUE./
    SAVE ITOT, FIRSTC

    IF (NDUMPR==0) stop 106

    TERMXX = ITMSTP - 1
    ITERMXX = ITMSTP - 1
    CALL LENOCO(version/10,ITOT,IFLT,IINT)
    CALL WRITEOT(dmpoutput,ITOT,IOUNIT,IERR,version)
    IF (IERR==0) THEN
        if (debugging) WRITE (LOGERR,5020) ISTEP, ITOT * 4
        RETURN
    endif

    !error processing
    WRITE (LOGERR,5030) MOD(ierr,256), historyfile
    STOP

5020 FORMAT ('Write to the history file at',I5,I7)
5030 FORMAT ('From dumper, error in accessing history file, error = ',I5,/,' File = ',A256)
    END

    SUBROUTINE LENOCO(IV,ITOT,IFLT,IINT)

    !     Description:  To calculation the length of the numeric common block

    !     Arguments: IV     CFAST reduced version number (VERSION -1800) / 10
    !                ITOT   Total length of the common block in storage units
    !                IFLT   Length of the floating portion in numeric storage units
    !                IINT   Length of the integer portion in numeric storage units

    use cfast_main
    include "precis.fi"

    IINT = (LOC(ITERMXX) - LOC(NEUTRAL))/4 + 1
    IFLT = (LOC(TERMXX) - LOC(GAMMA))/4

    IFLT = IFLT + 2
    ITOT = IINT + IFLT

    RETURN
    END

    SUBROUTINE WRITEOT(INPUT,LEN,IOUNIT,IERR,IVERS0)


    !     Description:  Write compacted history file
    !     Arguments: INPUT
    !                LEN
    !                IOUNIT
    !                IERR
    !                IVERS0

    PARAMETER (MXDMP = 36000)
    INTEGER INPUT(LEN), PARRAY(MXDMP)
    CHARACTER HEADER*6
    DATA HEADER /'$$CFL$'/

    IERR = 0
    CALL PACKOT(LEN,MXDMP,INPUT,PARRAY)
    WRITE (IOUNIT,IOSTAT=IOS) HEADER, IVERS0
    WRITE (IOUNIT,IOSTAT=IOS) PARRAY(1), (PARRAY(I),I = 2,PARRAY(1))

    IF (IOS/=0) THEN
        IERR = IOS
    ELSE
        IERR = 0
    endif
    RETURN
    END

    SUBROUTINE PACKOT(ITIN,MXDMP,DOIT,RETBUF)

    !     This is the pack routine.  It crunches the binary common block to reduce
    !     The amount of storage required to hold a single history record.
    !     The length of the record which is written is contained in the first word
    !     of the record and does NOT include itself.
    !     The original implementation of this work was published by 
    !     Andrew Binstock in The C Gazette, December 1987.  It is one of a large
    !     class of compression schemes.  This particular scheme is fast, and is
    !     best at compressing strings of the same character.  Since much of
    !     the history file in CFAST stays the same, 0 for example, this works
    !     fairly well.

    INTEGER IC, ITIN, IDX, RIDX, MXDMP
    INTEGER DOIT(ITIN), RETBUF(MXDMP)
    INTEGER LC, COUNT, MRKR
    CHARACTER MSG*80

    IDX = 1
    RIDX = 1
    MRKR = 106
    IC = DOIT(IDX)
    IDX = IDX + 1

    !     CHECKING TO MAKE SURE THE FIRST NUMBERS ARE NOT THE MARKER

10  IF (IC==MRKR) THEN
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        RIDX = RIDX + 1
        RETBUF(RIDX) = MRKR
        IC = DOIT(IDX)
        IDX = IDX + 1
        GO TO 10
    endif

    LC = IC
    COUNT = 1

    !     MAIN LOOP

20  IF (IDX<=ITIN) THEN
        IC = DOIT(IDX)
        IDX = IDX + 1

        !     IF NEXT NUMBER = MARKER THEN STORE WHAT YOU HAVE

30      IF (IC==MRKR) THEN
            IF (COUNT>3) THEN
                IF ((RIDX+5)>=MXDMP) THEN
                    WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
                    CALL XERROR(MSG,0,1,1)
                    IERR = 19
                    RETURN
                endif
                CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
            ELSE
                IF ((RIDX+COUNT+2)>=MXDMP) THEN
                    WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
                    CALL XERROR(MSG,0,1,1)
                    IERR = 19
                    RETURN
                endif
                DO 40, I = 1, COUNT
                    RIDX = RIDX + 1
                    RETBUF(RIDX) = LC
40              CONTINUE
            endif
            COUNT = 0
            RIDX = RIDX + 1
            RETBUF(RIDX) = MRKR
            RIDX = RIDX + 1
            RETBUF(RIDX) = MRKR
            IF (IDX>ITIN) GO TO 60
            IC = DOIT(IDX)
            IDX = IDX + 1
            LC = IC
            GO TO 30
        endif
        IF (IC==LC) THEN
            COUNT = COUNT + 1
            IF (COUNT==(2**30)) THEN
                IF ((RIDX+5)>=MXDMP) THEN
                    WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
                    CALL XERROR(MSG,0,1,1)
                    IERR = 19
                    RETURN
                endif
                CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
                COUNT = 0
            endif
        ELSE
            IF (COUNT>3) THEN
                IF ((RIDX+5)>=MXDMP) THEN
                    WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
                    CALL XERROR(MSG,0,1,1)
                    IERR = 19
                    RETURN
                endif
                CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
                LC = IC
                COUNT = 1
            ELSE
                IF ((RIDX+COUNT+2)>=MXDMP) THEN
                    WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
                    CALL XERROR(MSG,0,1,1)
                    IERR = 19
                    RETURN
                endif
                DO 50, I = 1, COUNT
                    RIDX = RIDX + 1
                    RETBUF(RIDX) = LC
50              CONTINUE
                LC = IC
                COUNT = 1
            endif
        endif
        GO TO 20
    endif
60  IF (COUNT>3) THEN
        IF ((RIDX+5)>=MXDMP) THEN
            WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
            CALL XERROR(MSG,0,1,1)
            IERR = 19
            RETURN
        endif
        CALL OPUT(LC,COUNT,ITIN,MXDMP,RIDX,RETBUF)
        LC = IC
        COUNT = 1
    ELSE
        IF ((RIDX+COUNT+2)>=MXDMP) THEN
            WRITE (MSG,*) 'PACKOT - Overwrite, input and index = ', ITIN, IDX
            CALL XERROR(MSG,0,1,1)
            IERR = 19
            RETURN
        endif
        DO 70, I = 1, COUNT
            RIDX = RIDX + 1
            RETBUF(RIDX) = LC
70      CONTINUE
    endif
    RETBUF(1) = RIDX
    RETURN
    END

    SUBROUTINE OPUT(IC,COUNT,ITIN,MXDMP,RIDX,RETBUF)

    !     Arguments: IC
    !                COUNT
    !                ITIN
    !                MXDMP
    !                RIDX
    !                RETBUF
    !

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

    implicit none
    integer errorcode	
    real*8 T, dT

    rewind (12)
    write(12,5001) t, dt
    call rsltcmp (12)
    errorcode = 0
    return

5001 FORMAT('Status at T = ',1PG11.2, ' DT = ',G11.3)
    end

    SUBROUTINE WRITEINI(FILE)


    !     Description:  this routine creates a solver.ini file for the current
    !                   version of CFAST.  It is created using:
    !                   cfast -s filename
    !                   where filename is the name of the file to contain
    !                   the solver.ini options .  The default name is 
    !                   SOLVE.INI (so as to not overwrite SOLVER.INI if
    !                   it is present)

    use cparams
    use opt
    use params
    use solver_parameters
    use wnodes
    include "precis.fi"

    CHARACTER*(*) FILE
    INTEGER FUNIT

    NNNOPT = 21

    IUNIT = FUNIT(70)
    OPEN(UNIT=IUNIT,FILE=FILE)

    WRITE(IUNIT,10)
10  FORMAT(' ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL,',' REL OTHER TOL')
    WRITE (IUNIT,11) APTOL, RPTOL, ATOL, RTOL
11  FORMAT(1X,5(1PG11.4,1X))

    WRITE(IUNIT,20)
20  FORMAT(' ABS WALL TOL, REL WALL TOL, INITIALIZATION TOLERANCE')
    WRITE (IUNIT,11) AWTOL, RWTOL, ALGTOL

    WRITE(IUNIT,30)
30  FORMAT(' ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, ',' REL HVAC TEMP')
    WRITE (IUNIT,11) AHVPTOL, RHVPTOL, AHVTTOL, RHVTTOL

    WRITE(IUNIT,40)
40  FORMAT(' NUMBER OF PHYSICAL OPTION FLAGS')
    WRITE (IUNIT,*) NNNOPT

    WRITE(IUNIT,50)
50  FORMAT(' FIRE,      HFLOW,  ENTRAIN, VFLOW,       CJET')
    WRITE (IUNIT,*) (OPTION(J),J = 1,5)

    WRITE(IUNIT,60)
60  FORMAT(' DOOR-FIRE, CONVEC, RAD,     CONDUCT, DEBUG PRINT  ')
    WRITE (IUNIT,*) (OPTION(J),J = 6,10)

    WRITE(IUNIT,70)
70  FORMAT(' EXACT ODE, HCL,   MFLOW,    KEYBOARD, ','TYPE OF INITIALIZATION')
    WRITE (IUNIT,*) (OPTION(J),J = 11,15)

    WRITE(IUNIT,80)
80  FORMAT(' MV HEAT LOSS, USE MODIFIED JACOBIAN, DASSL DEBUG, ','OXYGEN SOLVE    DETECTORS')
    WRITE (IUNIT,*) (OPTION(J),J = 16,20)

    WRITE(IUNIT,90)
90  FORMAT(' OBJECT BACKTRACKING')
    WRITE (IUNIT,*) (OPTION(J),J = 21,21)

    WRITE(IUNIT,100)
100 FORMAT(' NUMBER OF WALL NODES, FRACTIONS FOR FIRST, ','MIDDLE AND LAST WALL SLAB')
    WRITE (IUNIT,101) NWPTS, (WSPLIT(I),I=1,3)
101 FORMAT(1X,I3,1X,3(1PG11.4,1X))

    WRITE(IUNIT,110)
110 FORMAT(' BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE,','   2=INSULATED 3=FLUX)')
    WRITE (IUNIT,*) IWBOUND

    WRITE(IUNIT,120)
120 FORMAT(' MAXIMUM STEP SIZE,  MAX FIRST STEP - ',' IF EITHER <0 THEN SOLVER DECIDES')
    WRITE (IUNIT,11) STPMAX, DASSLFTS

    WRITE(IUNIT,130)
130 FORMAT(' HVAC CONVECTION COEFFICIENT')
    WRITE(IUNIT,11) DUCTCV

    WRITE(IUNIT,140)
140 FORMAT(' JAC CHECK (>0 CHECK JACOBIAN), JACOBIAN CUTOFF, ','  SNSQE PRINT (1=ON)')
    WRITE(IUNIT,141) JACCHK, CUTJAC, IPRTALG
141 FORMAT(1X,I3,1X,1PG11.4,I3)

    IF(1==1)STOP
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
    !     15 smokeview spreadsheet output
    !     21 spreadsheet output (normal)
    !     22 spreadsheet output (flow field)
    !     23 spreadsheet output (species)
    !     24 spreadsheet output (walls and targets)

    !!!! Note that we assume that the default carraigecontrol for formatted files is of type LIST (no fortran controls)

    use cfast_main
    use cshell
    use iofiles

    ! first the file for "printed" output
    if (lprint<0) then
        open (unit=iofilo,file=outputfile,status='new',carriagecontrol='fortran')
        lprint = abs(lprint)
        WRITE (LOGERR,5002) trim(outputfile)
        if (outputformat==0) outputformat = 2
    else
        OPEN (UNIT=IOFILO,FILE='CON',CARRIAGECONTROL='FORTRAN')
        write (logerr,5004)
        if (outputformat==0) outputformat = 1
    endif

    ! next the history file
    IF (ldiago>0) THEN
        write(logerr,5001) trim(historyfile)
        OPEN (UNIT=11,FILE=historyfile,ERR=10,IOSTAT=IOS,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
    endif 

    ! Next create the status file
    open (12,file=statusfile,ACCESS='APPEND',ERR=81,iostat=ios)

    ! Now the smokeview files
    if (ldiagp>0) then
        write(logerr,5003) trim(smvhead),trim(smvdata)
        OPEN (UNIT=13,FILE=smvhead,form='formatted',err=11,iostat=ios)
        OPEN (UNIT=14,FILE=smvdata,FORM="UNFORMATTED",err=11,iostat=ios)
        open (unit=15, file=smvcsv,form='formatted')
    endif

    ! Next the spread sheet files
    if (lcopyss>0) then
        write(logerr,5005) trim(ssnormal),trim(ssflow),trim(ssspecies),trim(sswall)
        open (unit=21, file=ssnormal,form='formatted')
        open (unit=22, file=ssflow,form='formatted')
        open (unit=23, file=ssspecies,form='formatted')
        open (unit=24, file=sswall,form='formatted')
    endif

    ! And finally we create a file to indicate that the model is running.

    open (unit=4, file=kernelisrunning, dispose='DELETE')

    return

    ! error processing

    !	History file
10  WRITE (LOGERR,5030) MOD(IOS,256), trim(historyfile)
    STOP 105
    !	Smokeview file
11  write(logerr,5040) mod(ios,256),trim(smvhead),trim(smvdata)
    stop 105
    !	This one comes from writing to the status file
81  write(logerr,*) 'Fatal error writing to the status file ',ios
    STOP 106

5001 format ('Open the history file ',a)
5002 format ('Open the output file ',a)
5003 format ('Open the smokeview files - ',a,2x,a)
5004 format ('Send output to the consol')
5005 format ('Open the spreadsheet files - ',4(a,2x))
5030 FORMAT ('Error ',i4,' while accessing history, file = ',A)
5040 FORMAT ('Error ',i4,' while processing smokeview files -',i3,2x,a,2x,a)

    end

    subroutine deleteoutputfiles (outputfile)

    use IFPORT

    character (*) outputfile
    logical exists, doesthefileexist
    integer (2) filecount

    if (doesthefileexist(outputfile)) then
        filecount = delfilesqq(outputfile)
        if (filecount<1) stop 104
    endif

    return
    end

    integer function rev_output

    INTEGER :: MODULE_REV
    CHARACTER(255) :: MODULE_DATE 
    CHARACTER(255), PARAMETER :: mainrev='$Revision: 461 $'
    CHARACTER(255), PARAMETER :: maindate='$Date: 2012-06-28 16:38:31 -0400 (Thu, 28 Jun 2012) $'

    WRITE(module_date,'(A)') mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
    READ (MODULE_DATE,'(I5)') MODULE_REV
    rev_output = module_rev
    WRITE(MODULE_DATE,'(A)') maindate
    return
    end function rev_output
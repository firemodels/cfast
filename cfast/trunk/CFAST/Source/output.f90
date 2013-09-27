
! --------------------------- disclaim -------------------------------------------

    subroutine disclaim (name)

    use cshell
    implicit none
    
    character(*), intent(in) :: name
    character obuf*14, aminrev*2
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

! --------------------------- versionout -------------------------------------------

    subroutine versionout (iunit)

    !	a routine to put the header information in the output file. 
    !	we assume the file is open

    use cfast_main
    use cshell
    implicit none
    
    integer, intent(in) :: iunit
    integer rev_cfast, imajor, iminor, iminorrev

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
    write (iunit,30) validate
    return

10  format ('Version ',i1,'.',i1,'.',I2,', Created ',I4.4,'/',I2.2,'/',I2.2,', Revision ',i5)
20  format ('Version  ',i1,'.',i1,'.',I1,', Created ',I4.4,'/',I2.2,'/',I2.2,', Revision ',i5)
30  format ('CFAST run with validation option = ',i2)    
    end subroutine versionout

! --------------------------- splitversion -------------------------------------------

    subroutine splitversion (version,imajor,iminor,iminorrev)
    
    implicit none
    
    integer, intent(in) :: version
    integer, intent(out) :: imajor,iminor,iminorrev
    
    if (version>=1000) then
        imajor = version/1000
        iminor = mod(version,1000)/100
        iminorrev = mod(version,100)
    else
        imajor = version/100
        iminor = mod(version,100)/10
        iminorrev = mod(version,10)
    endif
    return
    end subroutine splitversion

! --------------------------- printobjectparameters -------------------------------------------

    subroutine printobjectparameters (iobj)

    use cfast_main
    use fltarget
    use objects2
    implicit none
    
    integer, intent(in) :: iobj
    integer :: j,i

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
    write(*,9) 'hclf ',(omprodr(i,6,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'ct   ',(omprodr(i,10,iobj),i=1,OBJLFM(IOBJ))
    write(*,9) 'fC   ',(omprodr(i,11,iobj),i=1,OBJLFM(IOBJ))
    write(*,10) 'hocb ',(objhc(i,iobj),i=1,OBJLFM(IOBJ))
10  format(a5,11f10.0)
    write(*,*)

    return
    end subroutine printobjectparameters

! --------------------------- result -------------------------------------------

    subroutine result(time,isw)

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

    use precision_parameters
    use cshell
    implicit none

    integer, intent(in) :: isw
    real(eb), intent(in) :: time

    if (outputformat>1) then
        write (iofilo,5000) time
        call rsltlay
        call rsltfir(isw)
        call rslttar(1)
        call rsltsprink
        call rslthall()
        call rsltsp
        if(trace) then
            call rsltflwt ()
        else
            call rsltflw ()
        endif
    else if (outputformat==1) then
        write (iofilo,5000) time
        call rsltcmp (iofilo)
    endif

5000 format (//,' Time = ',f8.1,' seconds.')
    end subroutine result

! --------------------------- rsltlay -------------------------------------------

    subroutine rsltlay

    !     Description:  Output the 2 layer environment at the current time

    use precision_parameters
    use cfast_main
    use cenviro
    use cshell
    use fltarget
    implicit none

    integer :: i, itarg, izzvol

    write (iofilo,5000)
    write (iofilo,5010)
    write (iofilo,5020)
    write (iofilo,5030)
    write (iofilo,5040)
    do i = 1, nm1
        itarg = ntarg - nm1 + i
        izzvol = zzvol(i,upper)/vr(i)*100.0_eb+0.5_eb
        if (izshaft(i)==1) then
            write (iofilo,5071) compartmentnames(i), zztemp(i,upper)-kelvin_c_offset, zzvol(i,upper), &
            zzabsb(upper,i),zzrelp(i)-interior_rel_pressure(i),ontarget(i), xxtarg(trgnfluxf,itarg)
        else
            write (iofilo,5070) compartmentnames(i), zztemp(i,upper)-kelvin_c_offset, zztemp(i,lower)-kelvin_c_offset, &
            zzhlay(i,lower), zzvol(i,upper), izzvol, zzabsb(upper,i),zzabsb(lower,i), zzrelp(i)-interior_rel_pressure(i),ontarget(i), xxtarg(trgnfluxf,itarg)
        endif
    end do
    return

5000 format (' ')
5010 format (' Compartment',T16,'Upper',T26,'Lower',T36,'Inter.',T46,'Upper',T62,'Upper',T73,'Lower',T83,'Pressure',T95,'Ambient',T106,'Floor')
5020 format (T16,'Temp.',T26,'Temp.',T36,'Height',T46,'Vol.',T62,'Absorb',T73,'Absorb',T95,'Target',T106,'Target')
5030 FORMAT (T17,'(C)',T26,'(C)',T36,'(m)',T46,'(m^3)',T62,'(m^-1)',T73,'(m^-1)',T85,'(Pa)',T95,'(W/m^2)',T106,'(W/m^2)')
5040 format (' ',113('-'))
5070 format (1x,a13,1P2G11.4,1PG11.4,1X,1pg9.2,'(',I3,'%) ',1PG10.3,1X,1PG10.3,1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
5071 format (1x,A13,1PG11.4,10(' '),10(' '),1X,1pg9.2,7(' '),1PG10.3,1X,10(' '),1x,1PG10.3,1X,1PG10.3,1X,1PG10.3)
    end subroutine rsltlay

! --------------------------- rsltfir -------------------------------------------

    subroutine rsltfir (isw)

    !     Description:  Output the fire environment at the current time

    !     Arguments: ISW    Print switch for object fire printout

    use precision_parameters
    use cfast_main
    use cshell
    use objects1
    implicit none

    integer, intent(in) :: isw
    
    integer length, i, ir, j
    real(eb) :: fheight, xems, xemp, xqf, xqupr, xqlow

    external length
    write (iofilo,5000)
    if (lfmax>0.and.lfbt>0.and.lfbo>0) then
        call flamhgt (fqf(0),farea(0),fheight)
        write (iofilo,5010) 'Main', fems(0), femp(0), fqf(0), fheight, fqfc(0), fqf(0) - fqfc(0)
    endif
    if (numobjl/=0) then
        do i = 1, numobjl
            call flamhgt (fqf(i),farea(i),fheight)
            if (isw/=0) then
                if (objpnt(i)/=0) then
                    j = objpnt(i)
                    write (iofilo,5010) objnin(j)(1:len_trim(objnin(j))), fems(i), femp(i), fqf(i), fheight,fqfc(i),fqf(i)-fqfc(i),objmaspy(i),radio(i)
                endif
            else
                write (iofilo,5020) i, fems(i), femp(i), fqf(i), fheight,fqfc(i),fqf(i)-fqfc(i),objmaspy(i),radio(i)
            endif
        end do
    endif
    write (iofilo,'(a)') ' '
    do ir = 1, nm1
        xems = 0.0_eb
        xemp = 0.0_eb
        xqf = 0.0_eb
        xqupr = 0.0_eb
        xqlow = 0.0_eb
        do i = 0, numobjl
            if (ir==froom(i)) then
                xems = xems + fems(i)
                xemp = xemp + femp(i)
                xqf = xqf + fqf(i)
                xqupr = xqupr + fqupr(i)
                xqlow = xqlow + fqlow(i)
            endif
        end do
        xqf = xqf + fqdj(ir)
        if (xems+xemp+xqf+xqupr+xqlow+fqdj(ir)/=0.0_eb) write (iofilo,5030) compartmentnames(ir), xems, xemp, xqf, xqupr, xqlow, fqdj(ir)
    end do
    if (fqdj(n)/=0.0_eb) write (iofilo,5040) fqdj(n)
    return
5000 format (//,' Fires',/,'0Compartment    Fire      Plume     Pyrol     Fire      Flame     Fire in   Fire in   Vent      Convec.   Radiat.   Pyrolysate  Trace',/, &
    '                          Flow      Rate      Size      Height    Upper     Lower     Fire',/, &
    '                          (kg/s)    (kg/s)    (W)       (m)       (W)       (W)       (W)         (W)       (W)       (kg)      (kg)' ,/,' ',138('-'))
5010 format (' ',14x,a8,2x,1p4g10.3,30x,1p3g10.3,2x,g10.3)
5020 format (' ',13x,'Object ',i2,2x,1p4g10.3,30x,1p3g10.3,2x,g10.3)
5030 format (' ',a14,10x,1p3g10.3,10x,1p3g10.3)
5040 format ('  Outside',76x,1pg10.3)
    end subroutine rsltfir

! --------------------------- rsltsp -------------------------------------------

    subroutine rsltsp

    !     Description:  Output the layer and wall species at the current time

    use cfast_main
    use cenviro
    use cshell
    implicit none

    logical :: swl(4)
    integer :: iwptr(4)
    character :: stype(ns)*10, sunits(ns)*11, ciout*255, cjout*255,lnames(2)*5, wtype(4)*10
    external length
    integer :: length, i, j, layer, ic, lsp, iw

    data lnames /'Upper', 'Lower'/
    data iwptr /1, 3, 4, 2/
    data wtype /'HCl c', 'HCl f', 'HCl uw', 'HCl lw'/
    data sunits /'(%)', '(%)', '(%)', '(ppm)', '(ppm)', '(ppm)','(%)', '(%)', '(1/m)', '(g-min/m3)', ' kg '/
    data stype /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'CT', ' TS'/
    if (nlspct/=0) then
        do i = 1, nwal
            swl(i) = .false.
            do j = 1, nm1
                swl(i) = swl(i) .or. switch(i,j)
            end do
        end do

        do layer = upper, lower
            write (iofilo,5050) lnames(layer)
            ciout = 'compartment'
            cjout = ' '
            ic = 16
            do lsp = 1, ns
                if (activs(lsp)) then
                    write (ciout(ic:ic+9),5000) stype(lsp)
                    write (cjout(ic:ic+9),5000) sunits(lsp)
                    ic = ic + 11
                endif
            end do
            if (activs(6)) then
                do iw = 1, 4
                    if (swl(iwptr(iw))) then
                        write (ciout(ic:ic+9),5000) wtype(iw)
                        write (cjout(ic:ic+9),5000) '(mg/m^2)  '
                        ic = ic + 10
                    endif
                end do
            endif
            write (iofilo,5020) ciout(1:length(ciout))
            write (iofilo,5020) cjout(1:length(cjout))
            write (iofilo,5030) ('-',i = 1,ic)
            write (ciout,5010)
            do i = 1, nm1
                write (ciout,5060) compartmentnames(i)
                ic = 14
                if (layer==upper.or.izshaft(i)==0) then
                    do lsp = 1, ns
                        write (ciout(ic:ic+9),5040) toxict(i,layer,lsp)
                        ic = ic + 11
                    end do
                    if (activs(6)) then
                        do iw = 1, 4
                            if (swl(iwptr(iw))) then
                                write (ciout(ic:ic+9),5040) zzwspec(i,iwptr(iw))
                                ic = ic + 10
                            endif
                        end do
                    endif
                endif
                write (iofilo,5020) ciout(1:length(ciout))
            end do
        end do
    endif
    return
5000 format (a10)
5010 format (' ')
5020 format (' ',a)
5030 format (' ',255a1)
5040 format (1pg10.3)
5050 format (//,' ',a5,' Layer Species',/)
5060 format (a13)
    end subroutine rsltsp

! --------------------------- rsltflw -------------------------------------------

    subroutine rsltflw ()

    !     Description:  Output the vent flow at the current time

    use precision_parameters
    use cfast_main
    use cshell
    use vents
    implicit none
    
    integer :: irm, i, j, k, iijk, ii, inode, iii
    real(eb) :: sum1, sum2, sum3, sum4, sum5, sum6, flow

    character ciout*8, cjout*12, outbuf*132
    dimension flow(6)
    logical first
    
    write (iofilo,5000)

    do irm = 1, n
        i = irm
        first = .true.
        write (ciout,'(a8)') compartmentnames(irm)
        if (irm==n) ciout = 'Outside'

        !     horizontal flow natural vents
        do j = 1, n
            do k = 1, mxccv
                iijk = ijk(i,j,k)
                if (iand(1,ishft(nw(i,j),-k))/=0) then
                    if (j==n) then
                        write (cjout,'(a1,1x,a7,a2,i1)') 'H', 'Outside', ' #', k
                    else
                        write (cjout,'(a1,1x,a4,i3,a2,i1)') 'H', 'Comp', J,' #', k
                    endif
                    if(i<j)then
                        sum1 = ss2(iijk) + sa2(iijk)
                        sum2 = ss1(iijk) + sa1(iijk)
                        sum3 = aa2(iijk) + as2(iijk)
                        sum4 = aa1(iijk) + as1(iijk)
                    else
                        sum1 = ss1(iijk) + sa1(iijk)
                        sum2 = ss2(iijk) + sa2(iijk)
                        sum3 = aa1(iijk) + as1(iijk)
                        sum4 = aa2(iijk) + as2(iijk)
                    endif
                    if (i==n) then
                        call flwout(outbuf,sum1,sum2,sum3,sum4,0.0_eb,0.0_eb,0.0_eb,0.0_eb)
                    else
                        if(i<j)then
                            sum5 = sau2(iijk)
                            sum6 = asl2(iijk)
                        else
                            sum5 = sau1(iijk)
                            sum6 = asl1(iijk)
                        endif
                        call flwout(outbuf,sum1,sum2,sum3,sum4,sum5,sum6,0.0_eb,0.0_eb)
                    endif
                    if (first) then
                        if (i/=1) write (iofilo,5010)
                        write (iofilo,5020) ciout, cjout, outbuf
                        first = .false.
                    else
                        write (iofilo,5020) ' ', cjout, outbuf
                    endif
                endif
            end do
        end do

        !     vertical flow natural vents
        do j = 1, n
            if (nwv(i,j)/=0.or.nwv(j,i)/=0) then
                if (j==n) then
                    write (cjout,'(a1,1x,a7)') 'V', 'Outside'
                else
                    write (cjout,'(a1,1x,a4,i3)') 'V', 'Comp', j
                endif
                do ii = 1, 4
                    flow(ii) = 0.0_eb
                end do
                if (vmflo(j,i,upper)>=0.0_eb) flow(1) = vmflo(j,i,upper)
                if (vmflo(j,i,upper)<0.0_eb) flow(2) = -vmflo(j,i,upper)
                if (vmflo(j,i,lower)>=0.0_eb) flow(3) = vmflo(j,i,lower)
                if (vmflo(j,i,lower)<0.0_eb) flow(4) = -vmflo(j,i,lower)
                call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),0.0_eb,0.0_eb,0.0_eb,0.0_eb)
                if (first) then
                    if (i/=1) write (iofilo,5010)
                    write (iofilo,5020) ciout, cjout, outbuf
                    first = .false.
                else
                    write (iofilo,5020) ' ', cjout, outbuf
                endif
            endif
        end do

        !     mechanical vents
        if (nnode/=0.and.next/=0) then
            do i = 1, next
                ii = hvnode(1,i)
                if (ii==irm) then
                    inode = hvnode(2,i)
                    write (cjout,'(a1,1x,a4,i3)') 'M', 'Node', inode
                    do iii = 1, 6
                        flow(iii) = 0.0_eb
                    end do
                    if (hveflo(upper,i)>=0.0_eb) flow(1) = hveflo(upper,i)
                    if (hveflo(upper,i)<0.0_eb) flow(2) = -hveflo(upper,i)
                    if (hveflo(lower,i)>=0.0_eb) flow(3) = hveflo(lower,i)
                    if (hveflo(lower,i)<0.0_eb) flow(4) = -hveflo(lower,i)
                    flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
                    flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
                    call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),0.0_eb,0.0_eb,flow(5),flow(6))
                    if (first) then
                        if (i/=1) write (iofilo,5010)
                        write (iofilo,5020) ciout, cjout, outbuf
                        first = .false.
                    else
                        write (iofilo,5020) ' ', cjout, outbuf
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

! --------------------------- rsltflwt -------------------------------------------

    subroutine rsltflwt ()

    !     Description:  Output the vent flow at the current time

    use precision_parameters
    use cfast_main
    use cshell
    use vents
    implicit none

    integer :: irm, i, ii, iii, inode
    real(eb) :: flow(6)

    character :: ciout*14, cjout*12, outbuf*132
    logical first
    
    write (iofilo,5000)

    do irm = 1, n
        i = irm
        first = .true.
        write (ciout,'(a14)') compartmentnames(irm)
        if (irm==n) ciout = 'Outside'

        ! horizontal flow natural vents


        ! vertical flow natural vents


        ! mechanical vents
        if (nnode/=0.and.next/=0) then
            do i = 1, next
                ii = hvnode(1,i)
                if (ii==irm) then
                    inode = hvnode(2,i)
                    write (cjout,'(a1,1x,a4,i3)') 'M', 'Node', INODE
                    do iii = 1, 4
                        flow(iii) = 0.0_eb
                    end do
                    if (hveflot(upper,i)>=0.0_eb) flow(1) = hveflot(upper,i)
                    if (hveflot(upper,i)<0.0_eb) flow(2) = -hveflot(upper,i)
                    if (hveflot(lower,i)>=0.0_eb) flow(3) = hveflot(lower,i)
                    if (hveflot(lower,i)<0.0_eb) flow(4) = -hveflot(lower,i)
                    flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
                    flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))
                    call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),flow(5),flow(6),0.0_eb,0.0_eb)
                    if (first) then
                        if (i/=1) write (iofilo,5010)
                        write (iofilo,5020) ciout, cjout, outbuf
                        first = .false.
                    else
                        write (iofilo,5020) ' ', cjout, outbuf
                    endif
                endif
            end do
        endif
    end do

5000 format (//,' Total mass flow through vents (kg)',/, &
    '0To             Through        ','      Upper Layer           ','    Lower Layer           ','   Trace Species',/, &
    ' Compartment    Vent             ',2('Inflow       Outflow      '),' Vented ', '   Filtered',/,' ', 104('-'))
5010 format (' ')
5020 format (' ',a14,1x,a12,1x,a)
    end subroutine rsltflwt

! --------------------------- rsltcmp -------------------------------------------

    subroutine rsltcmp (iounit)

    !     Description:  Output a compressed output for 80 column screens

    use precision_parameters
    use cenviro
    use cfast_main
    use objects2
    implicit none

    integer, intent(in) :: iounit
    
    integer :: i, ir
    real(eb) :: xemp, xqf

    write (iounit,5000)
    write (iounit,5010)
    do ir = 1, nm1
        xemp = 0.0_eb
        xqf = 0.0_eb
        do i = 0, numobjl
            if (ir==froom(i)) then
                xemp = xemp + femp(i)
                xqf = xqf + fqf(i)
            endif
        end do
        xqf = xqf + fqdj(ir)
        if (izshaft(ir)==1) then
            write (iounit,5031) ir, zztemp(ir,upper)-kelvin_c_offset, xemp, xqf, zzrelp(ir) - interior_rel_pressure(ir), ontarget(ir)
        else
            write (iounit,5030) ir, zztemp(ir,upper)-kelvin_c_offset, zztemp(ir,lower)-kelvin_c_offset, zzhlay(ir,lower), xemp, xqf, zzrelp(ir) - interior_rel_pressure(ir),ontarget(ir)
        endif
    end do
    write (iounit,5020) fqdj(n)
    return

5000 format (' ')
5010 format (' Compartment   Upper   Lower   Inter.  Pyrol     Fire      Pressure  Ambient',/, &
    '               Temp.   Temp.   Height  Rate      Size                Target',/, &
    '               (C)     (C)     (m)     (kg/s)    (W)       (Pa)      (W/m^2)',/,' ',77('-'))
5020 format ('  Outside',39x,1pg10.3)
5030 format (i5,7x,2f8.1,2x,1pg9.2,1p4g10.3)
5031 format (i5,7x,f8.1,8(' '),2x,8(' '),1p4g10.3)
    end subroutine rsltcmp

! --------------------------- rslttar -------------------------------------------

    subroutine rslttar (itprt)

    !     description:  output the temperatures and fluxes on surfaces and targets at the current time
    !                itprt 1 if target printout specifically called for, 0 otherwise

    use precision_parameters
    use targptrs
    use cenviro
    use cfast_main
    use cshell
    use fltarget
    implicit none

    integer, intent(in) :: itprt
    
    integer :: length, i, iw, itarg, itctemp
    real(eb) :: ctotal, total, ftotal, wtotal, gtotal, tg, tttemp, tctemp

    integer :: iwptr(4)
    
    external length
    data iwptr /1, 3, 4, 2/

    if ((itprt==0.and.ntarg<=nm1).or.ntarg==0) return
    write (iofilo,5000)
    do i=1,nm1
        itarg = ntarg-nm1+i
        if (validate.or.netheatflux) then
            total = gtflux(itarg,t_total)
            ftotal = gtflux(itarg,t_ftotal)
            wtotal = gtflux(itarg,t_wtotal)
            gtotal = gtflux(itarg,t_gtotal)
            ctotal = gtflux(itarg,t_ctotal)
        else
            total = xxtarg(trgtfluxf,itarg)
            ftotal = qtfflux(itarg,1)
            wtotal = qtwflux(itarg,1)
            gtotal = qtgflux(itarg,1)
            ctotal = qtcflux(itarg,1)
        endif
        if (total<=1.0e-10_eb) total = 0.0_eb
        if (ftotal<=1.0e-10_eb) ftotal = 0.0_eb
        if (wtotal<=1.0e-10_eb) wtotal = 0.0_eb
        if (gtotal<=1.0e-10_eb) gtotal = 0.0_eb
        if (ctotal<=1.0e-10_eb) ctotal = 0.0_eb
        if (total/=0.0_eb) then
            write (iofilo,5010) compartmentnames(i),((twj(1,i,iwptr(iw))-kelvin_c_offset),iw=1,4),twj(1,i,2)-kelvin_c_offset, &
            total,ftotal,wtotal,gtotal,ctotal
        else
            write (iofilo,5010) compartmentnames(i),(twj(1,i,iwptr(iw))-kelvin_c_offset,iw=1,4),twj(1,i,2)-kelvin_c_offset
        endif
        if (ntarg>nm1) then
            do itarg = 1, ntarg-nm1
                if (ixtarg(trgroom,itarg)==i) then
                    tg = tgtarg(itarg)
                    tttemp = xxtarg(trgtempf,itarg)
                    itctemp = (trgtempf+trgtempb)/2
                    if (ixtarg(trgeq,itarg)==cylpde) itctemp = trgtempf+ xxtarg(trginterior,itarg)*(trgtempb-trgtempf)
                    tctemp = xxtarg(itctemp,itarg)
                    if (ixtarg(trgeq,itarg)==ode) tctemp = tttemp
                    if (ixtarg(trgmeth,itarg)==steady) tctemp = tttemp
                    if (validate.or.netheatflux) then
                        total = gtflux(itarg,t_total)
                        ftotal = gtflux(itarg,t_ftotal)
                        wtotal = gtflux(itarg,t_wtotal)
                        gtotal = gtflux(itarg,t_gtotal)
                        ctotal = gtflux(itarg,t_ctotal)
                    else
                        total = xxtarg(trgtfluxf,itarg)
                        ftotal = qtfflux(itarg,1)
                        wtotal = qtwflux(itarg,1)
                        gtotal = qtgflux(itarg,1)
                        ctotal = qtcflux(itarg,1)
                    endif
                    if (total<=1.0e-10_eb) total = 0.0_eb
                    if (ftotal<=1.0e-10_eb) ftotal = 0.0_eb
                    if (wtotal<=1.0e-10_eb) wtotal = 0.0_eb
                    if (gtotal<=1.0e-10_eb) gtotal = 0.0_eb
                    if (ctotal<=1.0e-10_eb) ctotal = 0.0_eb
                    if (total/=0.0_eb) then
                        write(iofilo,5030)itarg,tg-kelvin_c_offset,tttemp-kelvin_c_offset, tctemp-kelvin_c_offset, &
                        total,ftotal,wtotal,gtotal,ctotal
                    else
                        write(iofilo,5030)itarg,tg-kelvin_c_offset,tttemp-kelvin_c_offset,tctemp-kelvin_c_offset
                    endif
                endif
            end do
        endif
    end do
    return
5000 format (//,' Surfaces and Targets',/, &
    '0Compartment    Ceiling   Up wall   Low wall  Floor    Target    Gas       Surface   Center   Flux To      Fire         Surface      Gas',/, &
    '                Temp.     Temp.     Temp.     Temp.              Temp.     Temp.     Temp.    Target       Rad.         Rad.         Rad.         Convect.',/, &
    '                (C)       (C)       (C)       (C)                (C)       (C)       (C)      (W/m^2)      (W/m^2)      (W/m^2)      (W/m^2)      (W/m^2)      ',/,1x,144('-'))
5010 format (1x,a14,1p4g10.3,1x,'Floor',12x,1pg10.3,11x,5(1pg10.3,3x))
5030 format (55x,i4,4x,1p3g10.3,1x,5(1pg10.3,3x))
    end subroutine rslttar

! --------------------------- rsltsprink -------------------------------------------

    subroutine rsltsprink

    !     Description:  Output the conditions of and at a sprinkler location (temperature, velocities etc) at the current time

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    implicit none

    integer :: i, iroom, itype
    real(eb) :: cjetmin, zdetect, tlay, tjet, vel, tlink

    character(5) :: ctype
    character(3) :: cact

    if(ndtect==0)return
    write(iofilo,5000)
5000 format(//' Sensors',/, &
    '0                             Sensor                           Smoke',/, &
    ' Number  Compartment   Type   Temp (C)   Activated       Temp (C)   Vel (M/S)',/, &
    ' ----------------------------------------------------------------------------')
    cjetmin = 0.10_eb
    do i = 1, ndtect
        iroom = ixdtect(i,droom)

        zdetect = xdtect(i,dzloc)
        if(zdetect>zzhlay(iroom,lower))then
            tlay = zztemp(iroom,upper)
        else
            tlay = zztemp(iroom,lower)
        endif

        tjet = max(xdtect(i,dtjet),tlay)-kelvin_c_offset
        vel = max(xdtect(i,dvel),cjetmin)
        tlink =  xdtect(i,dtemp)-kelvin_c_offset

        itype = ixdtect(i,dtype)
        if(itype==smoked)then
            ctype = 'SMOKE'
        elseif(itype==heatd)then
            ctype = 'HEAT'
        else
            ctype = 'OTHER'
        endif
        cact = 'NO'
        if(ixdtect(i,dact)==1) cact = 'YES'
        write(iofilo,5010)i,iroom,ctype,tlink,cact,tjet,vel
5010    format(t2,i2,t10,i3,t24,a5,t31,1pe10.3,t42,a3,t58,1pe10.3,t69,1pe10.3)
    end do
    return
    end subroutine rsltsprink

! --------------------------- rslthall -------------------------------------------

    subroutine rslthall ()

    !     Description:  Output the conditions for each hall

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    implicit none

    integer :: nhalls, i
    real(eb) :: tstart, vel, depth, dist


    nhalls = 0
    do i = 1, nm1
        if(izhall(i,ihroom)==1)nhalls = nhalls + 1
    end do
    if(nhalls==0)return
    write(iofilo,5000)
5000 format (//,' Hall Flow',// &
    ' Compartment  Start Time     Velocity       Depth        Distance',/, &
    '                 (s)          (m/s)          (m)            (m)'/ &
    '-----------------------------------------------------------------')

    do i = 1, nm1
        if(izhall(i,ihroom)/=0) then
            tstart = zzhall(i,ihtime0)
            vel = zzhall(i,ihvel)
            depth = zzhall(i,ihdepth)
            dist = zzhall(i,ihdist)
            if(dist>zzhall(i,ihmaxlen))dist = zzhall(i,ihmaxlen)
            write(iofilo,30)i,tstart,vel,depth,dist
30          format(4x,i2,7x,1pg10.3,5x,1pg10.3,3x,1pg10.3,5x,1pg10.3)
        end if
    end do

    return
    end subroutine rslthall

! --------------------------- outinitial -------------------------------------------

    subroutine outinitial

    !     Description:  Output initial test case description

    use cfast_main
    use cshell
    use iofiles
    implicit none

    external length
    integer imajor, iminor, iminorrev, length

    call splitversion(version,imajor,iminor,iminorrev)

    if (.not.header) then
        if (iminorrev>=10) then
            write (iofilo,10) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), mpsdat(1), mpsdat(2), mpsdat(3)
        else
            write (iofilo,20) imajor, iminor, iminorrev, crdate(1), crdate(2), crdate(3), mpsdat(1), mpsdat(2), mpsdat(3)
        endif
    endif

    write (iofilo,5000) trim(inputfile), trim(title)
    if (outputformat>1) then
        call outover
        call outamb
        call outcomp
        call outvent
        call outthe
        call outtarg (1)
        call outfire
    endif

    return

5000 format (' Data file is ',a,'    Title is ',a)
10  format (' CFAST Version ',i1,'.',i1,'.',i2,' built ',i4.4,'/',i2.2,'/',i2.2,', run ',i4.4,'/',i2.2,'/',i2.2,/)
20  format (' CFAST Version ',i1,'.',i1,'.',i1,' built ',i4.4,'/',i2.2,'/',i2.2,', run ',i4.4,'/',i2.2,'/',i2.2,/)
    end subroutine outinitial

! --------------------------- outover -------------------------------------------

    subroutine outover

    !     description:  output initial test case overview

    use cfast_main
    use cshell
    use vents
    implicit none

    character chjet(4)*7, cjbuf*51
    data chjet /'off', 'ceiling', 'wall', 'all'/
    integer :: jpos

    write (iofilo,5000) 
    write (iofilo,5010) nm1, nvents, nvvent, next
    write (iofilo,5020) nsmax, lprint, ldiagp, lcopyss
    if (cjeton(5)) then
        if (cjeton(1)) then
            if (cjeton(3)) then
                jpos = 3
            else
                jpos = 1
            endif
        else if (cjeton(3)) then
            jpos = 2
        endif
        write (cjbuf,'(''on for '',a7)') chjet(jpos+1)
    else
        write (cjbuf,'(''off for all surfaces.'')')
    endif
    write (iofilo,5030) cjbuf
    return

5000 format (//,' OVERVIEW',/)
5010 FORMAT ('0Compartments    Doors, ...    Ceil. Vents, ...    MV Connects',/,'0',i4,12x,i4,10x,i4,17x,i4)
5020 format ('0Simulation     Output         Smokeview      Spreadsheet',/, &
             ' Time           Interval       Interval       Interval',/, & 
             ' (s)            (s)            (s)            (s)',/,' ',i6,6x,3(i6,9x))
5030 format ('0Ceiling jet is ',a)
    end subroutine outover

! --------------------------- outamb -------------------------------------------

    subroutine outamb

    !     Description:  Output initial test case ambient conditions

    use cfast_main
    use cshell
    use cenviro
    use params
    implicit none

    write (iofilo,5000) interior_temperature-kelvin_c_offset, interior_abs_pressure + pofset, exterior_temperature-kelvin_c_offset, exterior_abs_pressure + pofset, windv, windrf, windpw
    return

5000 format (//,' AMBIENT CONDITIONS',//, &
    ' Interior       Interior       Exterior       Exterior       Wind           Wind           Wind           ',/, &
    ' Temperature    Pressure       Temperature    Pressure       Speed          Ref. Height    Power',/,' ', &
    '  (C)            (Pa)           (C)            (Pa)          (m/s)          (m)', &
    //,' ',2(f7.0,8x,f9.0,6x),2(f7.1,8x),f7.2)
     
    end subroutine outamb

! --------------------------- outcomp -------------------------------------------

    subroutine outcomp

    !     Description:  Output initial test case geometry

    use cfast_main
    use cshell
    implicit none

    integer i

    write (iofilo,5000)
    do i = 1, nm1
        write (iofilo,5010) i, trim(compartmentnames(i)), br(i), dr(i), hr(i), hrp(i), hflr(i)
    end do
    return
5000 format (//,' COMPARTMENTS',//, &
    ' Compartment  Name                Width        Depth        Height       Ceiling      Floor     ',/, &
    '                                                                         Height       Height    ',/, & 
    ' '33x,5('(m)',10x),/,' ',96('-'))
5010 format (' ',i5,8x,a13,5(f12.2,1x))
    end subroutine outcomp

! --------------------------- outvent -------------------------------------------

    subroutine outvent

    !     Description:  Output initial test case vent connections

    use precision_parameters
    use cfast_main
    use cshell
    use params
    use vents
    implicit none

    integer :: i,j,k,iijk, isys, ibr, irm, iext
    real(eb) :: hrx, hrpx
    character :: ciout*8, cjout*14, csout*6
    logical :: first

    !     horizontal flow vents
    if (nvents==0) then
        write (iofilo,5000)
    else
        write (iofilo,5010)
        do i = 1, nm1
            do j = i + 1, n
                do k = 1, 4
                    write (cjout,'(a14)') compartmentnames(j)
                    if (j==n) cjout = ' Outside'
                    if (iand(1,ishft(nw(i,j),-k))/=0) then
                        iijk = ijk(i,j,k)
                        write (iofilo,5020) compartmentnames(i), cjout, k, bw(iijk), hl(iijk),hh(iijk), hlp(iijk), hhp(iijk)
                    endif
                end do
            end do
        end do
    endif

    !     vertical flow vents
    if (nvvent==0) then
        write (iofilo,5030)
    else
        write (iofilo,5040)
        do i = 1, n
            do j = 1, n
                if (nwv(i,j)/=0) then
                    write (ciout,'(i5,3x)') i
                    if (i==n) ciout = ' Outside'
                    write (cjout,'(i5,3x)') j
                    if (j==n) cjout = ' Outside'
                    csout = 'Round'
                    if (vshape(i,j)==2) csout = 'Square'
                    if (j<n) then
                        hrx = hr(j)
                        hrpx = hrp(j)
                    else
                        hrx = hrl(i)
                        hrpx = hrl(i)
                    endif
                    write (iofilo,5050) ciout, cjout, csout, vvarea(i,j), hrx,hrpx
                endif
            end do
        end do
    endif

    !     mechanical vents
    if (nnode==0.and.next==0) then
        write (iofilo,5060)
    else

        !     fans
        write (iofilo,5120)
        do isys = 1, nhvsys
            first = .true.
            do ibr = 1, nbr
                if (izhvbsys(ibr)==isys) then
                    if (nf(ibr)/=0) then
                        call chkext(na(ibr),irm,iext)
                        if (irm>=1.and.irm<=n) then
                            write (ciout,'(a4,i3)') 'Comp', irm
                            if (irm==n) ciout = 'Outside'
                            write (cjout,'(a4,i3)') 'Node', na(ibr)
                            if (first) then
                                write (iofilo,5100) isys, ciout, hvelxt(iext), cjout, hvght(na(ibr)), arext(iext)
                                first = .false.
                            else
                                write (iofilo,5110) ciout, hvelxt(iext), cjout, hvght(na(ibr)), arext(iext)
                            endif
                        endif
                        if (first) then
                            write (iofilo,5130) isys, 'Node', na(ibr), hvght(na(ibr)), 'Node', ne(ibr), hvght(ne(ibr)), &
                            nf(ibr), hmin(nf(ibr)), hmax(nf(ibr)), (hvbco(nf(ibr),j),j = 1,nfc(nf(ibr)))
                            first = .false.
                        else
                            write (iofilo,5140) 'Node', na(ibr), hvght(na(ibr)), 'Node', ne(ibr), hvght(ne(ibr)), nf(ibr), &
                            hmin(nf(ibr)), hmax(nf(ibr)), (hvbco(nf(ibr),j),j= 1,nfc(nf(ibr)))
                        endif
                        call chkext(ne(ibr),irm,iext)
                        if (irm>=1.and.irm<=n) then
                            write (ciout,'(a4,i3)') 'Node', ne(ibr)
                            write (cjout,'(a4,i3)') 'Comp', irm
                            if (irm==n) cjout = 'Outside'
                            if (first) then
                                write (iofilo,5100) isys, ciout, hvght(ne(ibr)), cjout, hvelxt(iext), arext(iext)
                                first = .false.
                            else
                                write (iofilo,5110) ciout, hvght(ne(ibr)), cjout, hvelxt(iext), arext(iext)
                            endif
                        endif
                    endif
                endif
            end do
        end do
    endif
    return

5000 format (//,' VENT CONNECTIONS',//,' There are no horizontal natural flow connections')
5010 format (//,' VENT CONNECTIONS',//,' Horizontal Natural Flow Connections (Doors, Windows, ...)',//, &
    ' From           To             Vent       Width       Sill        Soffit      Abs.        Abs.      ',/, & 
    ' Compartment    Compartment    Number                 Height      Height      Sill        Soffit',/, &
    ' ',41X,5('(m)         '),/,' ',100('-'))
5020 format (' ',a14,1X,A14,I3,5X,5(F9.2,3X))
5030 format (//,' There are no vertical natural flow connections')
5040 format (//,' Vertical Natural Flow Connections (Ceiling, ...)',//,' Top            Bottom         Shape     Area      ','Relative  Absolute',/,' ', &
    'Compartment    Compartment                        Height    Height',/,' ',40X,'(m^2)     ',2('(m)       '),/,' ',70('-'))
5050 format (' ',a8,7x,a8,7x,a6,2x,3(f7.2,3x))
5060 formaT (//,' There are no mechanical flow connections')
5070 format (//' Mechanical Flow Connections (Fans, Ducts, ...)',//, 'Connections and Ducts',//, &
    ' System    From           From      To             To        Length    Area      Rough',/,' ', &
    '                         Elev.                    Elev.',/,' ',25x,'(m)                      (m)       (m)   ','    (m^2)     (mm)',/,' ',86('-'))
5080 format (' ',i4,6x,a4,i3,5x,f7.2,6x,a4,i3,5x,4(f7.2,3x))
5090 format (' ',10x,a4,i3,5x,f7.2,6x,a4,i3,5x,4(f7.2,3x))
5100 format (' ',i4,6x,a7,5x,f7.2,6x,a7,5x,f7.2,13x,f7.2)
5110 format (' ',10x,a7,5x,f7.2,6x,a7,5x,f7.2,13x,f7.2)
5120 format (//,' Fans',//,' System    From           From      To             To        Fan       Minimum   Maximum    Fan Curve',/, &
    ' ','                         Elev.                    Elev.','     Number',/,' ',25x, &
    '(m)                      (m)             ','    (Pa)      (Pa)',/,' ',100('-'))
5130 format (' ',i4,6x,a4,i3,5x,f7.2,6x,a4,i3,5x,f7.2,6x,i3,6x,2(f7.2,3x),1p5g10.2)
5140 format (' ',10x,a4,i3,5x,f7.2,6x,a4,i3,5x,f7.2,6x,i3,6x,2(f7.2,3x),1p5g10.2)
5150 format (' ')
     
    end  subroutine outvent

! --------------------------- chkext -------------------------------------------

    subroutine chkext (ind,irm,iext)

    !     description:  check if an hvac node is a connection to an external room
    !     arguments: ind   node number to check
    !                irm   room number if node is an external connection
    !                iext  external node number is node is an external connection

    use cfast_main
    implicit none

    integer, intent(in) :: ind
    integer, intent(out) :: irm, iext
    
    integer :: i

    do i = 1, next
        if (hvnode(2,i)==ind) then
            iext = i
            irm = hvnode(1,i)
            return
        endif
    end do
    irm = 0
    iext = 0
    return
    end subroutine chkext

! --------------------------- outthe -------------------------------------------

    subroutine outthe

    !     description:  output initial test case thermal properties

    use cfast_main
    use cshell
    use thermp
    implicit none

    integer i, j, k

    ! check to see if any heat transfer is on
    do i = 1, nm1
        do j = 1, nwal
            if (switch(j,i).and.cname(j,i)/=' ') go to 30
        end do
    end do
    write (iofilo,5000)
    return

    ! some surfaces are on, do the printout of the surfaces
30  write (iofilo,5010)
    do  i = 1, nm1
        write (iofilo,5020) compartmentnames(i), cname(1,i), cname(3,i),cname(2,i)
    end do

    !     print out the properties of the materials used
60  write (iofilo,5030) thrmfile
    do i = 1, maxct
        write (iofilo,5040) nlist(i), lfkw(1,i), lcw(1,i), lrw(1,i), lflw(1,i), lepw(i), (lhclbf(k,i),k = 1,5) 
        do j = 2, lnslb(i)
            write (iofilo,5050) lfkw(j,i), lcw(j,i), lrw(j,i), lflw(j,i)
        end do
    end do
    write (iofilo,5060)
    return

5000 FORMAT (//,' Heat transfer for all surfaces is turned off')
5010 FORMAT (//,' THERMAL PROPERTIES',//,' ','Compartment    Ceiling      Wall         Floor',/,' ',70('-'))
5020 FORMAT (' ',a13,3(A10,3X))
5030 FORMAT (//,' Thermal data base used: ',A20,//,' Name',4X,'Conductivity',1X,'Specific heat',5X,'Density',5X,'Thickness',3X,'Emissivity',16X,'HCL B''s (1->5)')
5040 FORMAT (' ',A8,1P5G13.3,5E10.2)
5050 FORMAT (' ',8X,1P4G13.3)
5060 FORMAT (' ')

    end subroutine outthe

! --------------------------- outfire -------------------------------------------

    subroutine outfire

    !     routine: outfire
    !     purpose: This routine outputs the fire specification for all the object fires
    !     Arguments: none

    use precision_parameters
    use cfast_main
    use cshell
    use objects1
    use objects2
    use params
    implicit none

    integer :: io, i, j, nnv, length, is
    real(eb) :: y_hcn, y_hcl

    character cbuf*255, ftype(0:4)*13
    external length
    data ftype /'Undefined', 'Unconstrained', 'Constrained','Pool Fire', 'Furniture'/

    if (numobjl>0) then
        do io = 1, mxoin
            if (objpnt(io)/=0) then
                j = objpnt(io)
                nnv = objlfm(j)
                write (iofilo,5020) objnin(j)(1:length(objnin(j))), j
                write (iofilo,5030) compartmentnames(objrm(j)),ftype(objtyp(j)),objpos(1,j), objpos(2,j), objpos(3,j), relhum*100., limo2*100.,radconsplit(j)
                write (iofilo,5031) obj_c(j), obj_h(j), obj_o(j), obj_n(j), obj_cl(j)
                write (cbuf,5040)
                write (cbuf(51:132),5050)
                is = 113
                write (iofilo,'(3x,a)') cbuf(1:length(cbuf))
                write (iofilo,5000) ('(kg/kg)',i = 1,(is-51)/10)
                write (iofilo,5010) ('-',i = 1,is-1)
                do i = 1, nnv
                    write (cbuf,5060) otime(i,j), omass(i,j), objhc(i,j), oqdot(i,j), ohigh(i,j)
                    y_HCN = obj_n(j)*0.027028_eb/objgmw(j)
                    y_HCl = obj_cl(j)*0.036458_eb/objgmw(j)
                    write (cbuf(51:132),5070) ood(i,j), oco(i,j), y_HCN, y_HCl,omprodr(i,10,j),omprodr(i,11,j)
                    write (iofilo,'(1x,a)') cbuf(1:length(cbuf))
                end do
            endif
        end do
    endif
    return
5000 format ('   (s)       (kg/s)    (J/kg)    (W)       (m)       ',15(A7,3X))
5010 format (' ',255a1)
5020 format (//,' Name: ',A,'   Referenced as object #',i3,//,' Compartment    Fire Type    ','   Position (x,y,z)     Relative    Lower O2    Radiative',/,' ',52x,'Humidity    Limit       Fraction')
5030 format (1x,a14,1x,A13,3(F7.2),F7.1,6X,F7.2,5X,F7.2//)
5031 format (' Chemical formula of the fuel',/,3x,'Carbon    Hydrogen  Oxygen    Nitrogen  Chlorine',/,1x,5(f7.3,3x),//)
5040 format ('Time      Fmdot     Hcomb     Fqdot     Fheight   ')
5050 format ('Soot      CO        HCN       HCl       CT        TS')
5060 format (F7.0,3X,1P4G10.2)
5070 format (1P10G10.2,2x,2g10.2)
    end subroutine outfire

! --------------------------- chksum -------------------------------------------

    character(8) function chksum()
    
    implicit none
    
    chksum = '00000000'
    return
    end function chksum

! --------------------------- chksum -------------------------------------------

    subroutine outtarg (isw)

    !      description:  output initial test case target specifications

    use cfast_main
    use cshell
    use fltarget
    implicit none

    integer, intent(in) :: isw
    
    integer :: itarg, j

    character cbuf*255

    if(ntarg/=0) write(iofilo,5000)
5000 format(//,' TARGETS',//,' Target',T9,'Compartment',T24,'Position (x, y, z)',T51,'Direction (x, y, z)',T76,'Material',/,1X,82('-'))

    do itarg = 1, ntarg
        if (itarg<ntarg-nm1+1) then
            cbuf = cxtarg(itarg)
        else if (itarg>=ntarg-nm1+1.and.isw/=1) then
            write (cbuf,5004) itarg-(ntarg-nm1)
        else 
            writE (CBUF,5005) CXTARG(ITARG),ITARG-(NTARG-NM1)
        endif
5004    format ('Floor, compartment ',I2)
5005    format (A8,'  Floor, compartment ',I2)
5006    format (' ')
        write(iofilo,5010) itarg,compartmentnames(ixtarg(trgroom,itarg)),(xxtarg(trgcenx+j,itarg),j=0,2),(xxtarg(trgnormx+j,itarg),j=0,2),cbuf(1:8)
5010    format(' ',i5,t11,a14,t21,6(f7.2,2x),t76,a8)
    end do
    return
    end subroutine outtarg

! --------------------------- flwout -------------------------------------------

    subroutine flwout (outbuf,flow1,flow2,flow3,flow4,flow5,flow6,flow7,flow8)

    !     description:  stuff the flow output after blanking appropriate zeros

    use precision_parameters
    use cparams
    use solver_parameters
    implicit none

    
    
    real(eb), intent(in) :: flow1, flow2, flow3, flow4, flow5, flow6, flow7, flow8
    character, intent(out) :: outbuf*(*)
    
    real :: flow(8),  x1000,x100,x10,x1,x01

    integer :: i

    outbuf = ' '
    flow(1) = flow1
    flow(2) = flow2
    flow(3) = flow3
    flow(4) = flow4
    flow(5) = flow5
    flow(6) = flow6
    flow(7) = flow7
    flow(8) = flow8
    x1000 = 1000.0_eb
    x100 = 100.0_eb
    x10 = 10.0_eb
    x1 = 1.0_eb
    x01 = 0.1_eb
    do i = 1, 8
        if (flow(i)>=x1000) then
            write (outbuf(13*(i-1)+1:13*i),5000) flow(i)
        else if (flow(i)>=x100) then
            write (outbuf(13*(i-1)+1:13*i),5010) flow(i)
        else if (flow(i)>=x10) then
            write (outbuf(13*(i-1)+1:13*i),5020) flow(i)
        else if (flow(i)>=x1) then
            write (outbuf(13*(i-1)+1:13*i),5030) flow(i)
        else if (flow(i)>=x01) then
            write (outbuf(13*(i-1)+1:13*i),5040) flow(i)
        else
            write (outbuf(13*(i-1)+1:13*i),5000) flow(i)
        endif
        if (flow(i)<=atol) outbuf(13*(i-1)+1:13*i) = ' '
    end do
    return

5000 format (2x,1pg11.3)
5010 format (f6.0,7x)
5020 format (f7.1,6x)
5030 format (f8.2,5x)
5040 format (f9.3,4x)
    end subroutine flwout

! --------------------------- getabstarget -------------------------------------------

    subroutine getabstarget(targetnumber, positionvector)

    !	Routine to get the absolute position of a target in the computational space

    !	This is the protocol between cfast and smokeview

    use precision_parameters
    use cfast_main
    use fltarget
    implicit none

    integer, intent(in) :: targetnumber
    real(eb), intent(out) :: positionvector(*)
    integer :: i

    do i = 1, 6
        positionvector (i) = xxtarg(i,targetnumber)
    end do

    positionvector(1) = positionvector(1) + cxabs(ixtarg(trgroom,targetnumber))
    positionvector(2) = positionvector(2) + cyabs(ixtarg(trgroom,targetnumber))
    positionvector(3) = positionvector(3) + hrl(ixtarg(trgroom,targetnumber))

    return

    end subroutine getabstarget

! --------------------------- setdbug -------------------------------------------

    subroutine setdbug

    use cfin
    use cparams
    use cshell
    use opt
    
    implicit none

    integer funit, iroom, ilayer, i, iounit
    character dbugfil*8

    character keyword*7, dbugky(mxdebug)*7, dummy*1, ly*2
    data dbugky/'MASSFLW','HVACFLW','HORZFLW','VERTFLW','MVNTFLW','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX','XXXXXXX', &
    'ERRVCTR','PRNTJAC','PRNDPDT','XXXXXXX'/

    close (iofili)
    open (unit=iofili,file=lbuf)
    read(iofili,*,end=100) dummy
10  continue
    read(iofili,*,end=100,err=10) keyword, iroom, ilayer
    do i = 1, 15
        if (keyword==dbugky(i)) then
            if (ilayer>0.and.ilayer<=2.and.iroom>0.and.iroom<nr) then
                iounit = funit(70)
                if (ilayer==upper) then
                    ly = 'up'
                else
                    ly = 'LW'
                endif
                if (i/=2) then
                    write(dbugfil,'(a4,a2,i2.2)')dbugky(i),ly,iroom
                else
                    write(dbugfil,'(a4,i2.2,i2.2)')dbugky(i),ilayer,iroom
                endif
                call opnotpt(dbugfil,iounit)
                dbugsw(i,ilayer,iroom) = iounit
            endif
        endif
    end do
    if (keyword==dbugky(d_jac)) then
        call opndbg(iroom,ilayer)
    else if (keyword==dbugky(16)) then
        iounit = funit(70)
        call opnotpt('ERRVECTR',iounit)
        dbugsw(16,1,1) = iounit
    else
    endif
    go to 10
100 continue
    close (iofili)
    return
    end subroutine setdbug

! --------------------------- outjac -------------------------------------------

    subroutine outjac (tsec, wm, neqs)

    !     description: prints out the magnitude of the jacobian matrix

    use precision_parameters
    use cfast_main
    use cenviro
    use opt
    use wdervs
    
    implicit none

    real(eb), intent(in) :: wm(jacdim,*), tsec
    integer, intent(in) :: neqs
    
    real(eb) :: buf(maxeq), wmii, wmij, tmp, tmp1
    integer :: ioffst(8), itmp, itmp2, i, j, k, iounit, irdx, itcol, icdx, iitmp
    logical :: firstc
    character :: entry(maxeq)*2, lbls(8)*3, hder*256, ddiag*2

    data firstc/.true./
    data lbls/'p  ','pmv','tmv','tu ','vu ','tl ','wt ','prd'/
    save ioffst, hder, iounit

    !     normal processing of the debug output

    if (dbugsw(d_jac,d_prn,1)<=0) return
    if (firstc) then
        firstc = .false.
        ioffst(1) = nofp
        ioffst(2) = nofpmv
        ioffst(3) = noftmv
        ioffst(4) = noftu
        ioffst(5) = nofvu
        ioffst(6) = noftl
        ioffst(7) = nofwt
        ioffst(8) = nofprd
        hder = '  '
        itmp2 = 0
        do i = 1, 7
            if (ioffst(i)-ioffst(i+1)==0) cycle
            itmp2 = itmp2 + 1
            itmp = ioffst(i)*2 + 7 + itmp2*2
            hder(itmp:(itmp+2)) = lbls(i)
        end do
        iounit = dbugsw(d_jac,d_prn,1)
    endif
    write(iounit,*)' '
    write(iounit,*)'jacobian',numjac + totjac,' time ',tsec
    write(iounit,*)' '
    write(iounit,'(a256)')hder
    irdx = 1
    do i = 1, neqs
        if (i>ioffst(irdx))then
            irdx = irdx + 1
101         continue   
            if (i>ioffst(irdx)) then
                irdx = irdx + 1
                go to 101
            endif
            itcol = neqs + 8
            do 103 k = 1, itcol + 2
                entry(k) = '--'
103         continue
            write(iounit,*)(entry(k),k=1,itcol)
        endif
        entry(1) = lbls(irdx-1)(1:2)
        icdx = 1
        itcol = 1
        wmii = wm(i,i)
        if(wmii/=0.0_eb)then
            iitmp = log(abs(wmii))
        else
            iitmp = 11
        endif
        if (iitmp<verysm) then
            ddiag = ' .'
        else if (iitmp>verybg) then
            ddiag = ' û'
        else
            write(ddiag,'(i2)')iitmp
        endif

        do j = 1, neqs
            itcol = itcol + 1
            if (j>ioffst(icdx)) then
                icdx = icdx + 1
102             continue   
                if (j>ioffst(icdx)) then
                    icdx = icdx + 1
                    go to 102
                endif
                entry(itcol) = ' |'
                itcol = itcol + 1
            endif
            wmij = buf(j)
            if (wmij/=0.0_eb.and.wmii/=0.0_eb) then
                tmp1 = abs(wmij/wmii)
                tmp = log(tmp1)
            else if (wmii==0.0_eb) then
                tmp = 11
            else
                tmp = -11
            endif
            itmp = int(tmp + 0.5_eb)

            if (wmij==0.0_eb) then
                entry(itcol) = '  '
            else if (itmp<verysm) then
                entry(itcol) = ' .'
            else if (itmp>verybg) then
                entry(itcol) = ' û'
            else
                write(entry(itcol),'(i2)')itmp
            endif
        end do
        write(iounit,*)ddiag,':',(entry(k),k=1,itcol)
    end do
    return
    end subroutine outjac

! --------------------------- outjcmt -------------------------------------------

    subroutine outjcnt (t)

    !     description: print out numerical performance data; resid counts, jac counts, cpu times etc.

    use precision_parameters
    use cparams
    use opt
    use wdervs
    
    implicit none

    real(eb), intent(in) :: t
    
    integer :: iounit
    logical :: firstc = .true.
    save iounit

    if (dbugsw(d_jac,d_cnt,1)<=0) return
    if (firstc) then
        firstc = .false.
        iounit = dbugsw(d_jac,d_cnt,1)
        write(iounit,1002)
1002    FORMAT(15x,'STEPS',4x,'JACOBIANS',5x,'RESIDS',4x,'NEWT ITERS',9x,'CPU',14x,'OVER HEAD',/,4x,'TIME',4x,'CUR',4x,'CUM',2x,'CUR',4x,'CUM', &
            1x,'CUR',4x,'CUM',2x,'CUR',4x,'CUM',4x,'CUR',8x,'CUM',6x,'CUR',7x,'CUM')
    endif
    totjac = totjac + numjac
    totstep = totstep + numstep
    totresd = totresd + numresd
    numitr = numresd - numjac*jacdim
    totitr = totitr + numitr
    write(iounit,1001)t,numstep,totstep,numjac,totjac,numresd,totresd,numitr,totitr,prttime,tottime,ovtime,tovtime
1001 format(1x,1pe9.2,4(1x,i4,1x,i6),1x,1pe9.2,1x,1pe9.2,1x,1pe9.2,1x,1pe9.2)
    return
    end subroutine outjcnt

! --------------------------- opndbg -------------------------------------------

    subroutine opndbg (jaccnt, jacprn)

    !     Description: opens a file on unit iounit

    use cparams
    use opt
    
    implicit none

    integer, intent(in) :: jaccnt, jacprn
    
    integer funit, iounit
    logical :: firstc = .true.
    character :: cntfil*6 = 'JACCNT', prnfil*6 = 'JACPRN'

    save firstc

    if (firstc) then
        firstc = .false.
        if (jaccnt>0) then
            iounit = funit(70)
            call opnotpt(cntfil,iounit)
            dbugsw(d_jac,d_cnt,1) = iounit
        endif
        if (jacprn>0) then
            iounit = funit(70)
            call opnotpt(prnfil,iounit)
            dbugsw(d_jac,d_prn,1) = iounit
        endif
    endif
    return
    end  subroutine opndbg

! --------------------------- fnd_comp -------------------------------------------

    subroutine fnd_comp (icomp)

    use cfast_main
    use wallptrs
    use cenviro
    use cfin
    use opt
    implicit none
 
    integer, intent(in) :: icomp
    
    integer :: itmp, irm, iw

    write(lbuf,*)'Solution component with the greatest error is'
    call xerror(lbuf,0,1,0)
    if (icomp<=nofp+nm1) then
        write(lbuf,'(a18,i2)')' pressure in room ',icomp
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftu) then
        write(lbuf,'(a18,i2)')' either hvac or fsm ',icomp-nm1
        call xerror(lbuf,0,1,0)
    else if (icomp<=nofvu) then
        write(lbuf,'(a27,i2)')' upper layer temp in room ',icomp-noftu
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftl) then
        write(lbuf,'(a26,i2)')' upper layer vol in room ',icomp-nofvu
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftl+nm1) then
        write(lbuf,'(a27,i2)')' lower layer temp in room ',icomp-noftl
        call xerror(lbuf,0,1,0)
    else if (icomp<=nofwt) then
        if (option(foxygen)==on) then
            write(lbuf,'(a18,i2)')' oxygen component ',icomp-nofoxyl
            call xerror(lbuf,0,1,0)
        else
            write(lbuf,'(a15,i2)')' target number ',icomp-noftt
            call xerror(lbuf,0,1,0)
        endif
    else if (icomp<=nofprd) then
        itmp = icomp - nofwt
        irm = izwall(itmp,w_from_room)
        iw = izwall(itmp,w_from_wall)
        if (iw==1) then
            write(lbuf,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' ceiling '
            call xerror(lbuf,0,1,0)
        else if(iw==2) then
            write(lbuf,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' floor   '
            call xerror(lbuf,0,1,0)
        else if(iw==3) then
            write(lbuf,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' upper wall '
            call xerror(lbuf,0,1,0)
        else if(iw==4) then
            write(lbuf,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' lower wall '
            call xerror(lbuf,0,1,0)
        endif
    endif

    return
    end subroutine fnd_comp

! --------------------------- debugpr -------------------------------------------

    subroutine debugpr (ikey,t,dt,ieqmax)

    use precision_parameters
    use fireptrs
    use cenviro
    use cfast_main
    use cshell
    use params
    use wnodes
    implicit none

    integer, intent(in) :: ikey, ieqmax
    real(eb), intent(in) :: t, dt
    
    real(eb) :: xqf, dp
    integer :: bmap(mbr), i, j, iprod, il, isys, idt, iroom, iobj, itarg
    integer(2) :: ch, hit
    character(5) :: spname(ns) = (/'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%','  TUH', ' H2O%', '   OD', '   CT', '   TS'/), ccc*3
    logical :: firstc = .true.
    save bmap

    !     debug printing
    if (firstc) then
        firstc = .false.
        do i = 1, nbr
            do j = 1, ncnode(na(i))
                if (i==icmv(na(i),j)) then
                    bmap(i) = j
                    exit
                endif
        end do
    end do
    endif

    if (ikey==1) then
        write (*,*) 'Pause at time = ', T,',  Press any key to continue'
40      call grabky(ch,hit)
        if (hit==0) go to 40
        write (*,*) 'Continuing'
        write (*,*)
    else if (ikey==2) then
        write (iofilo,5000) t, dt
        do i = 1, nm1
            write (*,5010) i
            write (*,5020) '   Upper temp(K)', zztemp(i,upper)
            write (*,5020) '   Lower temp(K)', zztemp(i,lower)
            write (*,5020) ' Interface ht(m)', zzhlay(i,lower)
            write (*,5020) '   Pressure (pa)', zzrelp(i)
            if (nlspct>0) write (*,*) ' Species mass fractions ',' Upper           Lower'
            do iprod = 1, ns
                if (activs(iprod)) then
                    write (*,5030) spname(iprod), (zzcspec(i,il,iprod),il= upper,lower)
                endif
            end do
            if (nwalls/=0) write (*,*) ' Wall temperatures'
            if (switch(1,i)) then
                write (*,5040) zzwtemp(i,1,1)
            endif
            if (switch(3,i)) then
                write (*,5060) zzwtemp(i,3,1)
            endif
            if (switch(4,i)) then
                write (iofilo,5070) zzwtemp(i,4,1)
            endif
            if (switch(2,i)) then
                write (iofilo,5050) zzwtemp(i,2,1)
            endif
        end do
        write (*,*) ' '
        write (*,*) 'Hvac print by systems'
        do isys = 1, nhvsys
            write (*,*) 'For system ', isys
            write (*,*) 'Mass flow of system ', hvmfsys(isys)
            write (*,*) 'Mass of gas in system ', zzhvm(isys)
            do iprod = 1, ns
                write (*,*) 'Mass of ', spname(iprod), ' ',zzhvpr(isys,iprod)
            end do
            do idt = 1, nbr
                if (izhvbsys(idt)==isys) then
                    write (*,5080) na(idt), hvp(na(idt)), ne(idt),hvp(ne(idt)), hvflow(na(idt),bmap(idt)), tbr(idt)
                endif
            end do
        end do
        if (ndtect/=0)then
            write(*,*)'Detector info'
            write(*,100)
100         format('  N ',3X,'D temp',6X,'J temp',6X,' Act')
            do i = 1, ndtect
                iroom = ixdtect(i,droom)
                if (iquench(iroom)==i)then
                    ccc='***'
                else
                    ccc = '   '
                endif
                write(*,102)i,xdtect(i,dtemp),xdtect(i,dtjet),xdtect(i,dvel),xdtect(i,dtact),ccc
102             format(1x,i2,1x,4(e11.4,1x),a3)
            end do
        endif
        write (*,*) ' '
    else if (ikey==3) then
        write (*,5090) t, dt
        call fnd_comp(ieqmax)
        write(*,6030)
        do iroom = 1, nm1
            write(*,6000)iroom,zzrelp(iroom),zzhlay(iroom,lower),zztemp(iroom,lower),zztemp(iroom,upper),zzcspec(iroom,lower,2),zzcspec(iroom,upper,2)
        end do
        if(nhvpvar>0)write(*,6010)(p(nofpmv+i),i=1,nhvpvar)
        if(nhvtvar>0)write(*,6020)(p(noftmv+i),i=1,nhvtvar)
        if(nnode>0)write(*,6040)
        do i = 1, nnode
            do j = 1, ncnode(i)
                dp = hvp(mvintnode(i,j)) - hvp(i) + dpz(i,j)
                write(*,6050) i,mvintnode(i,j),dp,hvp(i),hvp(mvintnode(i,j)), hvght(i)
            end do
        end do
        write(*,6070)
        do iroom = 1, nm1
            xqf = 0.
            do iobj = 0, numobjl
                if (iroom==froom(iobj))xqf = xqf + fqf(iobj)
            end do
            xqf = xqf + fqdj(iroom)
            write(*,6060)iroom,zzwtemp(iroom,1,1),zzwtemp(iroom,3,1),zzwtemp(iroom,4,1),zzwtemp(iroom,2,1),xqf
        end do
        if(numobjl>0)then
            write(*,6080)
            do iobj = 1, numobjl
                write(*,6085)iobj,xfire(iobj,f_heatlp),xfire(iobj,f_heatup)
            end do
        endif
        if(ntarg>0)then
            write(*,6090)
            do itarg = 1, ntarg
                write(*,6095)itarg,xxtarg(trgtempf,itarg)
            end do
        endif
    endif
    return

5000 format (' T = ',1pg12.4,' DT = ',1pg12.4)
5010 format (' For room ',i3,' at time      T ')
5020 format (a16,5x,e14.7,3x,e14.7)
5030 format (15x,a5,1x,2(e14.7,3x))
5040 format ('  Ceiling temp(K) ',f12.2)
5050 format ('  Floor   temp(K) ',f12.2)
5060 format ('  Up wall temp(K) ',f12.2)
5070 format (' Low wall temp(K) ',e12.2)
5080 format (' from ',I2,' pressure ',e10.3,' to ',i2,' pressure ',g10.3,' mass flow is ',g10.3,' temp ',g10.3)
5090 format (' Returned from dassl at T = ',1pg14.6,',  dt = ',1pg12.4)
5095 formaT (' Solution Component with the most error: ',i3)
6000 format (1x,i3,1x,6e13.6)
6010 format (' HVAC pressures:',4e13.6)
6020 format (' HVAC temperatues:',4E13.6)
6030 format (t2,'Room',t9,'Pressure',t20,'Layer height',t35,'L. temp',t48,'U. temp',t62,'L. oxy',t75,'U. oxy')
6040 format (t3,'Nodes',t12,'Delta p',t23,'P at first node',t39,'P at 2nd node',t57,'Height')
6050 format (1x,2i3,1x,4(e13.6,2x))
6060 format (1x,i3,1x,5e13.6)
6070 format (t2,'Room',t11,'Ceiling',t21,'Upper Wall',t36,'Lower Wall',t49,'Floor',t61,'Fire Size')
6080 format (t2,'Object',t11,'Heat in lower ',t26,'Heat in upper')
6085 format (1x,i2,4x,2e13.6)
6090 format(t2,'Target',t11,'Temp')
6095 format(1x,i2,4x,e13.6)

    end subroutine debugpr

! --------------------------- oput -------------------------------------------

    subroutine oput (ic,count,itin,ridx,retbuf)

    implicit none
    
    integer, intent(in) :: ic, count, itin
    integer, intent(out) :: ridx, retbuf(itin)
    integer :: mrkr
    
    mrkr = 106
    ridx = ridx + 1
    retbuf(ridx) = mrkr
    ridx = ridx + 1
    retbuf(ridx) = ic
    ridx = ridx + 1
    retbuf(ridx) = count
    return
    end  subroutine oput 

    subroutine statusoutput (T, dT, errorcode)

    !  Write the status information to the "statusfile"

    use precision_parameters
    implicit none
    
    integer, intent(out) :: errorcode
    real(eb), intent(in) :: T, dT

    rewind (12)
    write(12,5001) t, dt
    call rsltcmp (12)
    errorcode = 0
    return

5001 FORMAT('Status at T = ',1PG11.2, ' DT = ',G11.3)
    end subroutine statusoutput

! --------------------------- writeini -------------------------------------------

    subroutine writeini(file)

    !     description:  this routine creates a solver.ini file for the current
    !                   version of cfast.  it is created using:
    !                   cfast -s filename
    !                   where filename is the name of the file to contain
    !                   the solver.ini options .  the default name is 
    !                   solve.ini (so as to not overwrite solver.ini if
    !                   it is present)

    use cparams
    use opt
    use params
    use solver_parameters
    use wnodes
    implicit none

    character(*), intent(in) :: file
    
    integer :: funit, nnnopt, i, j, iunit

    nnnopt = 21

    iunit = funit(70)
    open(unit=iunit,file=file)

    write(iunit,'(a)') ' ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL, REL OTHER TOL'
    write (iunit,11) aptol, rptol, atol, rtol
11  format(1x,5(1pg11.4,1x))

    write(iunit,'(a)') ' ABS WALL TOL, REL WALL TOL, INITIALIZATION TOLERANCE'
    write (iunit,11) awtol, rwtol, algtol

    write(iunit,'(a)') ' ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, REL HVAC TEMP'
    write (iunit,11) ahvptol, rhvptol, ahvttol, rhvttol

    write(iunit,'(a)') ' NUMBER OF PHYSICAL OPTION FLAGS'
    write (iunit,*) nnnopt

    write(iunit,'(a)') ' FIRE,      HFLOW,  ENTRAIN, VFLOW,       CJET'
    write (iunit,*) (option(j),j = 1,5)

    write(iunit,'(a)') ' DOOR-FIRE, CONVEC, RAD,     CONDUCT, DEBUG PRINT  '
    write (iunit,*) (option(j),j = 6,10)

    write(iunit,'(a)') ' EXACT ODE, HCL,   MFLOW,    KEYBOARD, TYPE OF INITIALIZATION'
    write (iunit,*) (option(j),j = 11,15)

    write(iunit,'(a)') ' MV HEAT LOSS, USE MODIFIED JACOBIAN, DASSL DEBUG, OXYGEN SOLVE    DETECTORS'
    write (iunit,*) (option(j),j = 16,20)

    write(iunit,'(a)') ' OBJECT BACKTRACKING'
    write (iunit,*) (option(j),j = 21,21)

    write(iunit,'(a)') ' NUMBER OF WALL NODES, FRACTIONS FOR FIRST, MIDDLE AND LAST WALL SLAB'
    write (iunit,'(1x,i3,1x,3(1pg11.4,1x))') nwpts, (wsplit(i),i=1,3)

    write(iunit,'(a)') ' BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE,   2=INSULATED 3=FLUX)'
    write (iunit,*) iwbound

    write(iunit,'(a)') ' MAXIMUM STEP SIZE,  MAX FIRST STEP -  IF EITHER <0 THEN SOLVER DECIDES'
    write (iunit,11) stpmax, dasslfts

    write(iunit,'(a)') ' HVAC CONVECTION COEFFICIENT'
    write(iunit,11) ductcv

    write(iunit,'(a)') ' JAC CHECK (>0 CHECK JACOBIAN), JACOBIAN CUTOFF,   SNSQE PRINT (1=ON)'
    write(iunit,'(1x,i3,1x,1pg11.4,i3)') jacchk, cutjac, iprtalg

    if (1==1) stop
    return
    end subroutine writeini

! --------------------------- openoutputfiles -------------------------------------------

    subroutine openoutputfiles

    !	Now that we know what output is needed, open the appropriate files
    !	Note that the sign of lprint determines whether we write to the console or  file
    !	Unit numbers defined here and readinputfiles

    !	Unit numbers defined in readop, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
    !      3 is for the log file  (LOGERR)
    !	   4 is for the indicator that the model is running (kernelisrunning)
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

    !!!! Note that we assume that the default carriage control for formatted files is of type LIST (no fortran controls)

    use cfast_main
    use cshell
    use iofiles
    
    implicit none
    
    integer :: ios

    ! first the file for "printed" output
    if (lprint<0) then
        open (unit=iofilo,file=outputfile,status='new',carriagecontrol='fortran')
        lprint = abs(lprint)
        write (logerr,5002) trim(outputfile)
        if (outputformat==0) outputformat = 2
    else
        open (unit=iofilo,file='con',carriagecontrol='fortran')
        write (logerr,5004)
        if (outputformat==0) outputformat = 1
    endif

    ! next the history file
    if (ldiago>0) then
        write(logerr,5001) trim(historyfile)
        open (unit=11,file=historyfile,err=10,iostat=ios,form='unformatted',access='sequential')
    endif 

    ! next create the status file
    open (12,file=statusfile,access='append',err=81,iostat=ios)

    ! now the smokeview files
    if (ldiagp>0) then
        write(logerr,5003) trim(smvhead),trim(smvdata)
        open (unit=13,file=smvhead,form='formatted',err=11,iostat=ios)
        open (unit=14,file=smvdata,form="unformatted",err=11,iostat=ios)
        open (unit=15, file=smvcsv,form='formatted')
    endif

    ! next the spread sheet files
    if (lcopyss>0) then
        write(logerr,5005) trim(ssnormal),trim(ssflow),trim(ssspecies),trim(sswall)
        open (unit=21, file=ssnormal,form='formatted')
        open (unit=22, file=ssflow,form='formatted')
        open (unit=23, file=ssspecies,form='formatted')
        open (unit=24, file=sswall,form='formatted')
    endif

    ! and finally we create a file to indicate that the model is running.

    open (unit=4, file=kernelisrunning, dispose='delete')

    return

    ! error processing

    !	history file
10  write (logerr,5030) mod(ios,256), trim(historyfile)
    stop 105
    !	smokeview file
11  write(logerr,5040) mod(ios,256),trim(smvhead),trim(smvdata)
    stop 105
    !	this one comes from writing to the status file
81  write(logerr,*) '***Fatal error writing to the status file ',ios
    stop 106

5001 format ('Open the history file ',a)
5002 format ('Open the output file ',a)
5003 format ('Open the smokeview files - ',a,2x,a)
5004 format ('Send output to the consol')
5005 format ('Open the spreadsheet files - ',4(a,2x))
5030 FORMAT ('***Error ',i4,' while accessing history, file = ',A)
5040 FORMAT ('***Error ',i4,' while processing smokeview files -',i3,2x,a,2x,a)

    end subroutine openoutputfiles

! --------------------------- deleteoutputfiles -------------------------------------------

    subroutine deleteoutputfiles (outputfile)

    use ifport
    implicit none

    character(*), intent(in) :: outputfile
    
    logical :: doesthefileexist
    integer(2) :: filecount

    if (doesthefileexist(outputfile)) then
        filecount = delfilesqq(outputfile)
        if (filecount<1) stop 104
    endif

    return
    end subroutine deleteoutputfiles 

! --------------------------- rev_output -------------------------------------------

    integer function rev_output ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_output = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_output

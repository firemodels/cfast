 module convection_routines
    use precision_parameters
    use fireptrs
    use wallptrs
    use cparams
    use cenviro
    use cfast_main
    use wnodes
    use opt
    
    private

   public convection, convective_flux

   contains
   
   subroutine convection (flwcv,flxcv)

    !     routine:    convection
    !     function:   interface between calculate_residuals and convective_flux.  loops over rooms
    !                 setting up varibles.  passes to convective_flux if ceiling jet for
    !                 a surface is off, otherwise sets flxcv to 0.0 and then
    !                 solves for flwcv
    !     outputs:    flwcv       net enthalphy into each layer 
    !                 flxcv       net heat flux onto surface


    real(eb), intent(out) :: flwcv(nr,2), flxcv(nr,nwal)
    real(eb) :: flwcv0(nr,2), flxcv0(nr,nwal), qconv, qconv_avg
    
    integer i, j, ieqtyp, iroom, iwall, iw, nrmfire, ilay, ifire
    logical roomflg(nr), wallflg(4*nr)
    save flwcv0, flxcv0

    do i = 1, nm1
        flwcv(i,upper) = 0.0_eb
        flwcv(i,lower) = 0.0_eb
        do j = 1, nwal
            flxcv(i,j) = 0.0_eb
        end do
    end do
    if (option(fconvec)/=on) return

    do i = 1, nm1
        roomflg(i) = .true.
    end do
    do i = 1, nwalls
        wallflg(i) = .true.
    end do

    if(option(fmodjac)==on)then
        if(jaccol>0)then

            ! if 2nd modified jacobian is active and dassl is computing a jacobian then
            ! only compute convection heat transfer in the room where the dassl 
            ! solution variable has been perturbed
            do i = 1, nm1
                roomflg(i) = .false.
            end do
            do i = 1, nwalls
                wallflg(i) = .false.
            end do

            ieqtyp = izeqmap(jaccol,1)
            iroom = izeqmap(jaccol,2)
            if(ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.ieqtyp==eqwt)then
                if(ieqtyp==eqwt)iroom = izwall(iroom,w_from_room)
                do iwall = 1, 4
                    roomflg(iroom) = .true.
                    if(surface_on_switch(iwall,iroom))then
                        iw = izwmap2(iwall,iroom) - nofwt
                        wallflg(iw) = .true.
                    endif
                end do
            endif
        endif
    endif

    ! calculate convection for all surfaces in all rooms
    do iw = 1, nwalls
        if(wallflg(iw)) then
            i = izwall(iw,w_from_room)
            iwall = izwall(iw,w_from_wall)
            nrmfire = ifrpnt(i,1)
            if(mod(iwall,2)==1)then
                ilay = upper
            else
                ilay = lower
            endif
            ! assume no fires in this room.  just use regular convection
            call convective_flux(iwall,zztemp(i,ilay),zzwtemp(i,iwall,1),flxcv(i,iwall))
            ! if there's a fire, we may need to modify the convection to account for the ceiling jet
            if (iwall==1.and.nrmfire>0) then
                qconv = 0.0_eb
                do ifire = 1, nrmfire
                    qconv = max(qconv,xfire(ifrpnt(i,2)+ifire-1,f_qfc))
                end do
                qconv_avg = 0.27_eb*qconv/((room_width(i)*room_depth(i))**0.68_eb*room_height(i)**0.64_eb)
                if (qconv_avg>flxcv(i,iwall)) flxcv(i,iwall) = qconv_avg
            end if
            flwcv(i,ilay) = flwcv(i,ilay) - zzwarea(i,iwall)*flxcv(i,iwall)
        endif
    end do

    if (option(fmodjac)==on) then
        if (jaccol==0) then

            ! save the flux and flow vectors when we are about to compute a jacobian
            do iroom = 1, nm1
                flwcv0(iroom,1) = flwcv(iroom,1)
                flwcv0(iroom,2) = flwcv(iroom,2)
                do iwall = 1, 4
                    flxcv0(iroom,iwall) = flxcv(iroom,iwall)
                end do
            end do
        elseif (jaccol>0) then

            ! we are computing the jaccol'th column of the jacobian.  if the solution hasn't 
            ! changed then get it from the vectors saved above.
            do iroom = 1, nm1
                if(.not.roomflg(iroom)) then
                    flwcv(iroom,1) = flwcv0(iroom,1)
                    flwcv(iroom,2) = flwcv0(iroom,2)
                    do iwall = 1, 4
                        flxcv(iroom,iwall) = flxcv0(iroom,iwall)
                    end do
                endif
            end do
        endif
    endif      
    return
    end subroutine convection

! --------------------------- convective_flux -------------------------------------------

    subroutine convective_flux (iw,tg,tw,qdinl)

    !     routine: convective_flux
    !     purpose: calculate convective heat transfer for a wall segment. 
    !     arguments:  iw     wall number, standand cfast numbering convention
    !                 tg     temperature of gas layer adjacent to wall surface
    !                 tw     wall surface temperature
    !                 qdinl  convective flux into wall surface iw

    integer, intent(in) :: iw
    real(eb), intent(in) :: tg, tw
    real(eb), intent(out) :: qdinl
    
    real(eb) :: h
    
    if (iw<=2) then
        h = 1.52_eb*abs(tg - tw)**onethird
    else
        h = 1.31_eb*abs(tg - tw)**onethird
    end if
    
    if(nfurn.gt.0)then
       qdinl = 0.0_eb
    else
      qdinl = h * (tg - tw)
    endif
    return
    end subroutine convective_flux
    
 end module convection_routines
    
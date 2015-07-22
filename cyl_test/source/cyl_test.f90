      program cyl_test
      implicit none
      integer, parameter :: nx=50
      double precision :: temp_out,WFLUXIN,DT,WRHO,DIAM
      double precision :: wk, wspec, x
      double precision :: temp_amb, temp_in
      double precision, dimension(nx) :: WTEMP
      
      integer :: i,j
      double precision :: t, temp_cable, temp_shroud
      double precision :: rad,tempx
      
      DT=0.1D0
      temp_in=24.0D0+273.0D0
      temp_amb=temp_in
      do i = 1, nx
        wtemp(i)=temp_in
      end do
      wrho=2538.0D0
      wk=0.2D0
      wspec=1500.0
      diam=0.0163D0
      rad=diam/2.0d0
      X=0.0015
      open(unit=12,file="test.csv")
      do i = 1, 18001
        t = (i-1)*dt
        temp_cable = wtemp(50)
        call get_flux(t,temp_cable,temp_shroud,wfluxin)
        call cylindrical_conductive_flux(wtemp,nx,wfluxin,dt,wk,wrho,wspec,diam)
        if(mod(i,100).eq.1)then
          call get_cylinder_temperature(X,WTEMP,50,rad,TEMPX)
          write(12,10)t,tempx-273.0D0,temp_shroud-273.0D0,(wtemp(j)-273,j=50,40,-1)
   10     format(14(e11.4,","))          
        endif
      end do
      end program cyl_test

! --------------------------- get_flux -------------------------------------------

    subroutine get_flux(t,temp_cable,temp_shroud,flux_out)
    
    use precision_parameters
    implicit none

    real(eb), intent(in) :: t,temp_cable
    real(eb), intent(out) :: flux_out,temp_shroud

    real(eb) :: factor, factor2, temp_gas

    if(t>=0.0_eb.and.t<=70.0_eb)then
        factor = (t-0.0_eb)/70.0_eb
        factor2 = ((t-0.0_eb)*210.0_eb + (70.0_eb-t)*24.0_eb)/70.0_eb
        ! else if(t>70.0.and.t<=820.0)then
    else if(t>70.0_eb)then
        factor = 1.0_eb
        factor2 = 210.0_eb
        ! else if(t>820.0.and.t<=1240.0)then
        !   factor = ((t-820.0)*0.62 + (1240.0-t)*1.0)/(1240.0-820.0)
        !   factor2 = ((t-820.0)*150.0 + (1240.0-t)*210.0)/(1240.0-820.0)
        ! else if(t>1240.0)then
        !   factor = 0.62
        !   factor2 = 150.0
    else
        factor = 0.0_eb
        factor2 = 24.0_eb
    endif

    temp_shroud = kelvin_c_offset + 24.0_eb*(1.0_eb-factor)+480.0_eb*factor
    temp_gas = factor2 + kelvin_c_offset
    flux_out = 0.95_eb*sigma*(temp_shroud**4-temp_cable**4)
    ! flux_out = flux_out + 10*(temp_shroud - temp_cable)

    end subroutine get_flux      


  

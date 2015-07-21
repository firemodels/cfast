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
        call get_flux(t,temp_cable,temp_amb,temp_shroud,wfluxin)
        call cylindrical_conductive_flux(wtemp,nx,wfluxin,dt,wk,wrho,wspec,diam)
        if(mod(i,100).eq.1)then
          call get_cylinder_temperature(X,WTEMP,50,rad,TEMPX)
          write(12,10)t,tempx-273.0D0,temp_shroud-273.0D0,(wtemp(j)-273,j=50,40,-1)
   10     format(14(e11.4,","))          
        endif
      end do
      end program cyl_test
  

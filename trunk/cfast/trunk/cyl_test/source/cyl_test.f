      program cyl_test
      implicit none
      integer, parameter :: nx=50
      double precision :: temp_out,WFLUXIN,DT,WRHO,DIAM
      double precision :: temp_amb, temp_in
      double precision, dimension(nx) :: WTEMP
      
      integer :: i,j
      double precision :: t, temp_cable, temp_shroud
      
      DT=0.1D0
      temp_in=24.0D0+273.0D0
      temp_amb=temp_in
      do i = 1, nx
        wtemp(i)=temp_in
      end do
      wrho=2538.0D0
      diam=0.0163D0
      open(unit=12,file="test.csv")
      do i = 1, 18001
        t = (i-1)*dt
        temp_cable = wtemp(50)
        call get_flux(t,temp_cable,temp_amb,temp_shroud,wfluxin)
        call CYLCNDUCT(wtemp,nx,wfluxin,dt,wrho,diam)
        if(mod(i,100).eq.1)then
          write(12,10)t,wfluxin,temp_shroud-273.0D0,
     x                (wtemp(j)-273.0,j=50,1,-1)
   10     format(54(e11.4,","))          
        endif
      end do
      end program cyl_test
  

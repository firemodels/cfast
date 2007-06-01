	subroutine SpreadSheetFluxHeader

! This routine spools the headers for the surface temperature and flux results.

! Format

!blank     c1     c1      c1    c1      c1   c1    c1      c1   c1         c2     c2      c2    c2       c2   c2   c2     c2   c2       ....
!time   ceiling	u-wall  l-wall floor  flux  fire surface gas convect   ceiling u-wall  l-wall floor  flux  fire surface gas convect    ....
       

!.....  target number
!.....  compartment name, flux, fire, surface, gas, convect


!.....  sensor number
!.....  compartment name, type, sensor temperature, activated, smoke temperature, smoke velocity


      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"

      parameter (maxoutput=512)
	integer position
      character heading*30(maxoutput,2), clabels*30(9), tlables*30(6),
     . tnum*2, ostring*30, slables*30(5), xtype*5, blank

      data clabels/"ceiling","upper wall","lower wall","floor",
     . "flux to target","total fire rad.","surface rad.","gas rad.",
     . "convective flux"/, blank/''/

      data tlables/"compartment","total flux","fire flux",
     . "surface flux","gas flux","convective flux"/

	data slables/"compartment","sensor temp.", "activated",
     . "smoke temp.","smoke velocity"/

	heading(1,1) = 'WALL'
	heading(1,2) = 'Time '
	position = 1

!	Compartment surfaces and the floor target

      do 10 i = 1, nm1
	do 20 j = 1, 9
	position = position + 1
	heading(position,1) = compartmentnames(i)
	heading(position,2) = clabels(j)
   20 continue
   10 continue

!	All the additional targets

      do 40 i = 1, nm1
      IF (NTARG.GT.NM1) THEN
        DO 30 ITARG = 1, NTARG-NM1
          IF (IXTARG(TRGROOM,ITARG).EQ.I) THEN
			  write(tnum,"(i2)") itarg
			  do 31 j = 1, 6
			  heading(position+j,1) = "Target "//tnum
   31			  heading(position+j,2) = tlables(j)
			  heading(position+1,2) = compartmentnames(i)
			  position = position + 6
          END IF
   30   CONTINUE
      END IF
   40 continue

!	Hall flow needs to go here

!	Detectors

	do 50 i = 1, ndtect
		 IROOM = IXDTECT(I,DROOM)
          ITYPE = IXDTECT(I,DTYPE)
		 write(tnum,"(i2)") i
		 if (itype.eq.smoked) then
			  xtype = 'Smoke'
		 elseif (itype.eq.heatd) then
		 	  xtype = 'Heat'
		 else
			  xtype = 'Other'
		 endif
		 do 51 j = 1, 5
		 heading(position+j,1) = 
     .         "Sensor "//tnum//' is a '//xtype//' detector'
   51		 heading(position+j,2) = slables(j)
		 heading(position+1,2) = compartmentnames(iroom)
		 position = position + 5
   50 continue

   	write(18,"(512(a,','))") (trim(heading(j,1)),j=1,position)
	write(18,"(512(a,','))") (trim(heading(j,2)),j=1,position)
	write(18,"(512(a,','))") (blank,j=1,position)

      return
      end




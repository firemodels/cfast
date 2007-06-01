      subroutine positionobject (xyz,index,opoint,rpoint,criterion,
     . defaultposition,minimumseparation,errorcode)

	integer index, defaultposition, opoint,rpoint, errorcode
	double precision xyz(3,0:*), xx0/0.d0/, minimumseparation,
     . criterion(*)

!       Position an object in a compartment
!		xyz is objposition (objpos)
!		index is 1, 2 or 3 for x, y or z
!		opoint is the object pointer
!		rpoint is the compartment
!		criterion is the maximum extent
!		defaultposition is to set to zero (base)(2) or midpoint(1)
!		minimumseparation is the closest the object can be to a wall

      IF((xyz(index,opoint).lt.xx0).or.
     .   (xyz(index,opoint).gt.criterion(rpoint))) THEN
		select case (defaultposition)
			case (1) 
				xyz(index,opoint) = criterion(rpoint)/2.0d0
			case (2) 
				xyz(index,opoint) = minimumseparation
			case default
				errorcode = 222
		end select
	else if (xyz(index,opoint).eq.xx0) then
		xyz(index,opoint) = minimumseparation
	else if (xyz(index,opoint).eq.criterion(rpoint)) then
		xyz(index,opoint) = criterion(rpoint)-minimumseparation
	END IF

	return

      end subroutine positionobject

!	The following functions are to implement the open/close function for vents.
!	This is done with a really simple, linear interpolation
!	The arrays to hold the open/close information are qcvh (4,mxvents), qcvv(4,nr), qcvm(4,mfan),
!         and qcvi(4,mfan). 

!	h is for horizontal flow, v for vertical flow, m for mechanical ventilation and i for filtering at mechanical vents

!   The qcv{x} arrays are of the form
!		(1,...) Is start of time to change
!		(2,...) Is the initial fraction (set in HVENT, VVENT and MVENT)
!		(3,...) Is the time to complete the change, Time+Decay_time, and
!		(4,...) Is the final fraction

!	The open/close function is done in the physical/mode interface, HFLOW, VFLOW and HVFAN

	double precision function qchfraction (points, index, time)

!	This is the open/close function for buoyancy driven horizontal flow

	double precision points(4,*), time, dt, dy, dydt

	if (time.lt.points(1,index)) then
		qchfraction = points(2,index)
		return
	else if (time.gt.points(3,index)) then
		qchfraction = points(4,index)
		return
	else
	   dt = max(points(3,index) - points(1,index),1.0d-3)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qchfraction = points(2,index) + dydt
		return
	endif
	end function qchfraction

	double precision function qcvfraction (points, index, time)

!	This is the open/close function for buoyancy driven vertical flow

	double precision points(4,*), time, dt, dy, dydt

	if (time.lt.points(1,index)) then
		qcvfraction = points(2,index)
		return
	else if (time.gt.points(3,index)) then
		qcvfraction = points(4,index)
		return
	else
	   dt = max(points(3,index) - points(1,index),1.0d-3)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcvfraction = points(2,index) + dydt
		return
	endif
	end function qcvfraction

	double precision function qcffraction (points, index, time)

!	This is the open/close function for mechanical ventilation

	double precision points(4,*), time, dt, dy, dydt

	if (time.lt.points(1,index)) then
		qcffraction = points(2,index)
		return
	else if (time.gt.points(3,index)) then
		qcffraction = points(4,index)
		return
	else
	   dt = max(points(3,index) - points(1,index),1.0d-3)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcffraction = points(2,index) + dydt
		return
	endif
	end function qcffraction

	double precision function qcifraction (points, index, time)

!	This is the open/close function for filtering

	double precision points(4,*), time, dt, dy, dydt

	if (time.lt.points(1,index)) then
		qcifraction = points(2,index)
		return
	else if (time.gt.points(3,index)) then
		qcifraction = points(4,index)
		return
	else
	    dt = max(points(3,index) - points(1,index),1.0d-3)
		dy = points(4,index) - points(2,index)
		dydt = dy / dt
		qcifraction = points(2,index) + dydt
		return
	endif
	end function qcifraction

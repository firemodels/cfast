      SUBROUTINE SpreadSheetFlowHeader

!	This is the header information for the flow spreadsheet and is called once
!	The logic is identical to SpreadSheetFlow so the output should be parallel

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

	parameter (maxoutput = 512)
	character*30 heading(maxoutput)
      CHARACTER ostring*30, CNUM*3, CNUM2*3
	integer position

	do 5 i = 1, maxoutput
    5 heading(i) = ' '

	position = 1

	ostring = 'Time'
	call SSFlowAtoH (position, maxoutput, heading, ostring)

!	Do the output by compartments

      DO 70 IRM = 1, N

C     Natural flow through vertical vents (horizontal flow)

      DO 20 J = 1, N
          DO 10 K = 1, mxccv
            I = IRM
            IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
              IIJK = IJK(I,J,K)
              IF (J.EQ.N) THEN
                 WRITE (CNUM,'(I2)') I
                 ostring = 'Inflow from outside to'//CNUM
				  call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Outflow to outside from'//CNUM
	           call SSFlowAtoH (position, maxoutput, heading, ostring)
              ELSE
                 WRITE (CNUM,'(I2)') I
                 WRITE (CNUM2,'(I2)') J
!	We show only net flow in the spreadsheets
                 ostring = 'Inflow from '//cnum2//' to'//CNUM
				  call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Outflow to '//cnum2//' from'//CNUM
	           call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Mixing to Upper'//CNUM//' ('//CNUM2//')'
			     call SSFlowAtoH (position, maxoutput, heading, ostring)
                 ostring = 'Mixing to Lower'//CNUM//' ('//CNUM2//')'
			     call SSFlowAtoH (position, maxoutput, heading, ostring)
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE

!	Natural flow through horizontal vents (vertical flow)

        DO 40 J = 1, N
          IF (NWV(I,J).NE.0.OR.NWV(J,I).NE.0) THEN
            WRITE (CNUM,'(I2)') I
            WRITE (CNUM2,'(I2)') J
            IF (J.EQ.N) CNUM2 = 'Out'
			if (i.eq.n) cnum = 'Out'
!	We show only net flow in the spreadsheets
		   ostring = 'V Outflow from '//cnum//' to '//cnum2
			call SSFlowAtoH (position, maxoutput, heading, ostring)
		   ostring = 'V Inflow from '//cnum//' to '//cnum2
			call SSFlowAtoH (position, maxoutput, heading, ostring)
          END IF
   40   CONTINUE

!	Mechanical ventilation

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
	        WRITE (CNUM,'(I2)') II
              IF (II.EQ.N) CNUM = 'Out'
              WRITE (CNUM2,'(I2)') INODE
			  ostring = "MV Inflow to "//cnum
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "MV Outflow from "//cnum
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "Trace Species through node "//cnum2
			  call SSFlowAtoH (position, maxoutput, heading, ostring)
			  ostring = "Trace captured at node "//cnum2
			  call SSFlowAtoH (position, maxoutput, heading, ostring)			  
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

	write(16,80) 'FLOW'
	write(16,80) (trim(heading(i)),i=1,position)
	write(16,80) ' '
   80	format(512a)
	return

	end

	subroutine SSFlowAtoH (position, maxoutput, heading, ostring)

	character heading*30(maxoutput), ostring*30
	integer position, length

	if (position.gt.maxoutput) return

	position = position + 1
	heading(position) = trim (ostring) // ','
	return
	
	END


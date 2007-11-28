      SUBROUTINE SpreadSheetFlow (Time, errorcode)

!	Routine to output the flow data to the flow spreadsheet {project}.f.csv

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "sizes.fi"
      include "vents.fi"

	parameter (maxoutput = 512)
	double precision time, outarray(maxoutput),sum1,sum2,sum3,sum4,
     . sum5,sum6, flow(6), sumin, sumout
	logical firstc/.true./
	integer position, errorcode
	save firstc

	if (firstc) then
		 call SpreadSheetFlowHeader
		 firstc = .false.
	endif
		 
      XX0 = 0.0D0
	position = 0

!	First the time

      CALL SSaddtolist (position,TIME,outarray)

      DO 70 IRM = 1, N

!	Next the horizontal flow through vertical vents

      DO 20 J = 1, N
          DO 10 K = 1, mxccv
            I = IRM
            IF (IAND(1,ISHFT(NW(I,J),-K)).NE.0) THEN
               IIJK = IJK(I,J,K)
               IF (I.LT.J)THEN
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
              IF (J.EQ.N) THEN
				  sumin = sum1 + sum3
				  sumout = sum2 + sum4
                 CALL SSAddtolist (position,SUMin,outarray)
                 CALL SSAddtolist (position,SUMout,outarray)
              ELSE
                 IF (I.LT.J)THEN
                    SUM5 = SAU2(IIJK)
                    SUM6 = ASL2(IIJK)
                 ELSE
                    SUM5 = SAU1(IIJK)
                    SUM6 = ASL1(IIJK)
                 ENDIF
!	We show only net flow in the spreadsheets
				  sumin = sum1 + sum3
				  sumout = sum2 + sum4
                 CALL SSAddtolist (position,SUMin,outarray)
                 CALL SSAddtolist (position,SUMout,outarray)
                 CALL SSAddtolist (position,SUM5,outarray)
                 CALL SSAddtolist (position,SUM6,outarray)
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE

!	Next natural flow through horizontal vents (vertical flow)

        DO 40 J = 1, N
          IF (NWV(I,J).NE.0.OR.NWV(J,I).NE.0) THEN
            DO 30 II = 1, 6
              FLOW(II) = XX0
   30       CONTINUE
            IF (VMFLO(J,I,UPPER).GE.XX0) FLOW(1) = VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,UPPER).LT.XX0) FLOW(2) = -VMFLO(J,I,UPPER)
            IF (VMFLO(J,I,LOWER).GE.XX0) FLOW(3) = VMFLO(J,I,LOWER)
            IF (VMFLO(J,I,LOWER).LT.XX0) FLOW(4) = -VMFLO(J,I,LOWER)
!	We show only net flow in the spreadsheets
		    sumin = flow(1) + flow(3)
			sumout = flow(2) + flow(4)
            CALL SSAddtolist (position,sumin,outarray)
            CALL SSAddtolist (position,sumout,outarray)
          END IF
   40   CONTINUE

!	Finally, mechanical ventilation

        IF (NNODE.NE.0.AND.NEXT.NE.0) THEN
          DO 60 I = 1, NEXT
            II = HVNODE(1,I)
            IF (II.EQ.IRM) THEN
              INODE = HVNODE(2,I)
              DO 50 III = 1, 6
                FLOW(III) = XX0
   50         CONTINUE
              IF (HVEFLO(UPPER,I).GE.XX0) FLOW(1) = HVEFLO(UPPER,I)
              IF (HVEFLO(UPPER,I).LT.XX0) FLOW(2) = -HVEFLO(UPPER,I)
              IF (HVEFLO(LOWER,I).GE.XX0) FLOW(3) = HVEFLO(LOWER,I)
              IF (HVEFLO(LOWER,I).LT.XX0) FLOW(4) = -HVEFLO(LOWER,I)
			  sumin = flow(1) + flow(3)
			  sumout = flow(2) + flow(4)
              flow(5) = abs(tracet(upper,i)) + abs(tracet(lower,i))
              flow(6) = abs(traces(upper,i)) + abs(traces(lower,i))			  
			  call SSAddtolist (position, sumin, outarray)
			  call SSAddtolist (position, sumout, outarray)
			  call SSAddtolist (position, flow(5), outarray)
			  call SSAddtolist (position, flow(6), outarray)
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE

	call ssprintresults(16, position, outarray)
	return

	END


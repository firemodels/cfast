      SUBROUTINE SpreadSheetNormal (time, errorcode)

! This routine writes to the {project}.n.csv file, the compartment information and the fires

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "iofiles77.fi"

	parameter (maxhead = 1+7*nr+5+7*mxfire)
	character*16 headline(3,maxhead)
	character*16 compartmentlabel(7)
	character*16 firelabel(7)
	double precision time, outarray(maxhead)
	logical firstc
	integer position

	data compartmentlabel/'Upper Layer Temp','Lower Layer Temp',
     . 'Layer Height','Volume','Pressure','Ambient Target',
     . 'Floor Target'/
	data firelabel/'Plume','Pyrolysis','Fire Size','Flame Height',
     . 'Convective flow','Pyrolosate','Total Trace'/
	data firstc/.true./
	save firstc

! Headers

!! Note: this more properly belongs in a separate routine as is done with the flow and species spreadsheets, but this one
!! is sufficiently short to make that more work than this simple approach

	If (firstc) then

	headline(1,1)= 'NORMAL'
	headline(2,1) = 'Time'
	headline(3,1) = ' '

	do 10 j = 1, nm1
	do 10 i = 1, 7
	headline(2,i+1+(j-1)*7) = compartmentlabel(i)
	headline(3,i+1+(j-1)*7) = ' '
   10	headline(1,i+1+(j-1)*7) = compartmentnames(j)

	nfires = 0
	if (lfbo.gt.0) then
		 do 20 i = 1, 7
		 headline(3,i+7*nm1+1) = firelabel(i)
		 headline(2,i+7*nm1+1) = compartmentnames(froom(0))
   20		 headline(1,i+7*nm1+1) = 'Mainfire'
		 nfires = 1
	endif

	do 30 j = 1, numobjl
	do 30 i = 1, 7
	headline(3,i+7*nm1+1+5*nfires+5*(j-1)) = firelabel(i)
	headline(2,i+7*nm1+1+5*nfires+5*(j-1)) = compartmentnames(froom(j))
   30 headline(1,i+7*nm1+1+5*nfires+5*(j-1)) = objnin(j)

	nfires = nfires + numobjl

	nheadings = 1+7*nm1+7*nfires
	if(nheadings.gt.maxhead.or.nheadings.gt.16000) then
		 errorcode = 211
		 return
	endif

	write(15,"(1024(a,','))") (trim(headline(1,i)),i=1,nheadings)
	write(15,"(1024(a,','))") (trim(headline(2,i)),i=1,nheadings)
	write(15,"(1024(a,','))") (trim(headline(3,i)),i=1,nheadings)
	firstc = .false.

	endif

! End of header information
 
	position = 0

      CALL SSaddtolist (position,TIME,outarray)

! Compartment information

        DO 100 I = 1, NM1
        ITARG = NTARG - NM1 + I
        IZZVOL = ZZVOL(I,UPPER)/VR(I)*100.D0+0.5D0
        CALL SSaddtolist (position,ZZTEMP(I,UPPER)-273.15,outarray)
        CALL SSaddtolist (position,ZZTEMP(I,LOWER)-273.15,outarray)
        CALL SSaddtolist (position,ZZHLAY(I,LOWER),outarray)
        CALL SSaddtolist (position,ZZVOL(I,UPPER),outarray)
        CALL SSaddtolist (position,ZZRELP(I) - PAMB(I),outarray)
        CALL SSaddtolist (position,ONTARGET(I),outarray)
        CALL SSaddtolist (position,XXTARG(TRGNFLUXF,ITARG),outarray)
  100 CONTINUE

! Fires

      XX0 = 0.0D0
      IF (LFMAX.GT.0.AND.LFBT.GT.0.AND.LFBO.GT.0) THEN
        CALL FLAMHGT (FROOM(0),FQF(0),FAREA(0),FHEIGHT)
        CALL SSaddtolist (position,FEMS(0),outarray)
        CALL SSaddtolist (position,FEMP(0),outarray)
        CALL SSaddtolist (position,FQF(0),outarray)
        CALL SSaddtolist (position,FHEIGHT,outarray)
        CALL SSaddtolist (position,FQFC(0),outarray)
        CALL SSaddtolist (position,objmaspy(0),outarray)
        CALL SSaddtolist (position,radio(0),outarray)
      END IF

      IF (NUMOBJL.NE.0) THEN
        DO 200 I = 1, NUMOBJL
          CALL FLAMHGT (FROOM(I),FQF(I),FAREA(I),FHEIGHT)
          CALL SSaddtolist (position,FEMS(I),outarray)
          CALL SSaddtolist (position,FEMP(I),outarray)
          CALL SSaddtolist (position,FQF(I),outarray)
          CALL SSaddtolist (position,FHEIGHT,outarray)
          CALL SSaddtolist (position,FQFC(I),outarray)
          CALL SSaddtolist (position,objmaspy(i),outarray)
          CALL SSaddtolist (position,radio(i),outarray)          
  200   CONTINUE
      END IF

      CALL SSprintresults (15, position, outarray)

      RETURN
      END

      SUBROUTINE SSaddtolist (ic, VALUE, array)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "cfin.fi"

	double precision array(*), value

      IC = IC + 1
!	We are imposing an arbitrary limit of 32000 columns
	if (ic.gt.32000) return
      array(IC) = VALUE
      RETURN
      
      ENTRY SSprintresults (iounit,ic,array)
      
      WRITE (iounit,"(1024(e12.6,','))" ) (array(i),i=1,ic)
      RETURN
      END


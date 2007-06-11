      SUBROUTINE SpreadSheetSpecies (time, errorcode)

!	Write out the species to the spread sheet file

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"

	parameter (maxhead = 1+18*nr)
	character*16 heading(3,maxhead)
	double precision time, outarray(maxhead)
	integer position

      CHARACTER STYPE(NS)*10, LNAMES(2)*5
	integer layer
      DATA LNAMES /'Upper', 'Lower'/
      DATA STYPE /'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O',
     +    'OD', 'CT', 'TS'/
      logical tooutput(11)/.false.,5*.true.,.false.,4*.true./,
     .  firstc/.true./
	
	save outarray, firstc

! If there are no species, then don't do the output
	if (nlspct.eq.0) return

! Set up the headings
	if (firstc) then
! Count species	nls is then the offset
		 nls = 0
		 do 9 i = 1, ns
			  if(tooutput(i)) nls = nls + 1
    9		 continue
		 maxsize = 2 * nls * nm1 + 1
		 if (maxsize.gt.maxhead) then
! We can only output to the maximum array size; this is not deemed to be a fatal error!
			  write(logerr,11) maxsize
			  maxsize = min (maxsize, maxhead)
		 endif

		 heading(1,1)='SPECIES'
		 heading(2,1) = 'Time'
		 heading(3,1) = ' '

		 do 10 i = 1, nm1
		 do 10 j = UPPER, LOWER
		 ins = 0
		 do 10 lsp = 1, NS
		 if(tooutput(lsp)) then
			  ins = ins + 1
			  IJKs = (ins-1)*2+(i-1)*2*nls+j+1
			  heading(3,ijks) = stype(lsp)
			  heading(2,ijks) = lnames(j)
   			  heading(1,ijks) = compartmentnames(i)
		 endif
   10		 continue
		 write(17,"(1024(a,','))") (trim(heading(1,i)),i=1,maxsize)
		 write(17,"(1024(a,','))") (trim(heading(2,i)),i=1,maxsize)
		 write(17,"(1024(a,','))") (trim(heading(3,i)),i=1,maxsize)
		 firstc = .false.
	endif

!	From now on, just the data, please

	position = 0

      CALL SSaddtolist (position,TIME,outarray)

      DO 70 I = 1, NM1
        DO 50 LSP = 1, NS
	    DO 80 LAYER = UPPER, LOWER
            IF (tooutput(LSP)) THEN
	        CALL SSaddtolist (position,TOXICT(I,LAYER,LSP),outarray)
! We can only output to the maximum array size; this is not deemed to be a fatal error!
			  if (position.ge.maxhead) go to 90
            END IF
   80	    CONTINUE
   50   CONTINUE
   70 CONTINUE

   90	call SSprintresults (17,position, outarray)

      RETURN

   11	format('Exceeded size of output files in species spread sheet')
      END


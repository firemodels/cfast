	logical function countargs (label,tocount,lcarray,numc,nret)

!	Count the number of arguements on the input line. 
!	Should be tocount. If not, then return an error (logical false)

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"

	integer tocount, i,numc,nret
	character lcarray*128(numc), label*5

	countargs = .false.
	nret = 0.

	do 1 i = 1, tocount
	if (lcarray(i).eq.' ') then
		 if (i.eq.1.) then
			  write(logerr,5003) label, tocount
		 else if (i.eq.2) then
			  write(logerr,5004) label, tocount
		 else
			  write(logerr,5000) label, tocount, nret
		 endif
		 return
	endif
	nret = nret + 1
    1 continue

	countargs = .true.

	do 2 i = tocount+1, numc
	if (lcarray(i).ne.' ') nret = nret + 1
2	continue	

	if(nret.eq.tocount) then
		 write(logerr,5001) label, (lcarray(i),i=1,nret)
	else
		 write(logerr,5002) label, (lcarray(i),i=1,nret)
	endif

	return

 5000	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there were only ',i2,' entries')
 5001 FORMAT('Key word ',a5,' parameter(s) = ',20a10)
 5002 FORMAT('Key word (ext) ',a5,' parameter(s) = ',128a10)
 5003	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there were no entries')
 5004	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there was only 1 entry')
	end


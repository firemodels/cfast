	subroutine gettpp (name, tp, errorcode)

      include "cfast.fi"
      include "thermp.fi"

	character name*(*), missingtpp*64
	integer tp, errorcode

	errorcode = 0
	do 1 i = 1, maxct
	tp = i
	if (name.eq.nlist(i)) return
 1	continue
	missingtpp = name
	errorcode = 205
	write(3,2) missingtpp
 2    format('Missing tpp = ',a)
	return
	end

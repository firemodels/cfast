	SUBROUTINE REMAPFIRES (NFIRES, FLOCAL, FXLOCAL, FYLOCAL, 
     . FZLOCAL, FQLOCAL, FHLOCAL)

C	This routine is to combine the main fire (in lfbo) and any objects into a single list
C	There does not have to be a main fire nor any objects, so NFIRES may be zero

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "smkview.fi"
      include "objects1.fi"
      include "objects2.fi"

C	First, the mainfire if there is one

	IF (LFBO.GT.0) THEN
		nfires = 1
		FLOCAL(1) = FROOM(0)
		FXLOCAL(1) = fopos(1,0)
		FYLOCAL(1) = fopos(2,0)
		FZLOCAL(1) = fopos(3,0)
		CALL FLAMHGT (FROOM(0),FQF(0),FAREA(0),FHEIGHT)
		FQLOCAL(1) = FQF(0)
		FHLOCAL(1) = FHEIGHT
	ELSE
		NFIRES = 0
	ENDIF
	
C	Now the other objects

	DO I = 1, NUMOBJL
		NFIRES = NFIRES + 1
		FXLOCAL(NFIRES) = fopos(1,i)
		FYLOCAL(NFIRES) = fopos(2,i)
		FZLOCAL(NFIRES) = fopos(3,i)
          CALL FLAMHGT (FROOM(I),fqf(i),FAREA(I),FHEIGHT)
		FQLOCAL(NFIRES) = fqf(i)
		FHLOCAL(NFIRES) = FHEIGHT
		flocal(nfires) = froom(i)
	END DO
	RETURN
	END

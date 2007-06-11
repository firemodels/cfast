      SUBROUTINE keywordcases(xnumr,xnumc,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     NPUTQ
C
C     Source File: NPUTQ.SOR
C
C     Functional Class:  INPUT
C
C     Description:  Handles CFAST datafile keywords
C
C     Arguments: ISRSTR
C                IERROR  Returns error codes
C
C     Revision History:
C	Modified: 10/20/04 : assume default values for hvac expansion coefficient and areas
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "sizes.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "thermp.fi"
      include "params.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "solvprm.fi"
      include "fltarget.fi"
      include "opt.fi"
      include "vents.fi"
      include "iofiles77.fi"

     
      PARAMETER (MAXIN = 36)
      LOGICAL LFUPDAT, eof, countargs
      INTEGER OBPNT,compartment,lrowcount,xnumr,xnumc,nx, i1, i2,
     + fannumber, iecfrom, iecto, mid
      DOUBLE PRECISION NTER(NR), initialopening, lrarray(ncol),
     + inter(nr), minpres, maxpres, heightfrom, heightto, areafrom, 
     + areato
      CHARACTER ORIENTYP*1, MESSG*133, lcarray*128(ncol), cjtype*1,
     + LABEL*5, C(MAXIN)*5, TCNAME*64, METHOD*8, EQTYPE*3, venttype,
     + orientypefrom*1, orientypeto*1, compfrom*128, compto*128

      EQUIVALENCE (INTER,QFR)

      DATA C /'TIMES', 'TAMB',  'LIMO2', 'HVENT', 
     +        'VVENT', 'MVOPN', 'WIND',  'EAMB', 
     +        'INTER', 'THRMF', 'MVDCT', 'MVFAN', 
     +        'INELV', 'OBJEC', 'CJET',  'OBJFL', 
     +        'STPMA' ,'DETEC', 'CFCON', 'ONEZ',
     +        'TARGE', 'TARG',  'HALL',  'ROOMA', 
     +        'ROOMH', 'DTCHE', 'SETP',  'HHEAT',
     +        'VHEAT', 'MAINF', 'COMPA', 'MVENT',
     +        'EVENT', 'HEATF', 'DJIGN', 'CHEMI'/

!	Start with a clean slate
      XX0 = 0.0D0
      XX1 = 1.0D0
      XXM1= -1.0D0
      LFUPDAT=.FALSE.
      IFLGSETP = 0
      SETPFILE = ' '
      DO 20 I = 1, NR
        DO 10 J = 1, 4
          CNAME(J,I) = 'OFF'
          SWITCH(J,I) = .FALSE.
   10   CONTINUE
	compartment = 0
	IERROR = 0
	lrowcount = 1

   20 CONTINUE

  810 CONTINUE

!	Start with the second row
   40 lrowcount = lrowcount + 1
!	If we reach the end of the file, then we are done
	if (lrowcount.gt.xnumr) return

!	Copy a single row into local arrays for processing in readin; start with column two, assuming that the key word is the first entry!

	label = carray(lrowcount,1)
	if (label.eq.' ') go to 40

	do 41 i = 2, xnumc
	lcarray(i-1) = carray(lrowcount,i)
   41 lrarray(i-1) = rarray(lrowcount,i)

      DO 50 I = 1, MAXIN
	  lsp = i
        IF (C(LSP).EQ.LABEL) THEN
            GO TO (150,190,210,550,
     +             560,670,620,200,
     +             630,230, 42,710,
     +              40,770,780,225,
     +			 805,815, 42,835,
     +             910, 42,940,950,
     +             960,970,990,1010,
     +             825,240,470,660,
     +			 552,1240,250,260), I
        END IF
   50 CONTINUE

!	Not a recognized key word
   42	write(logerr, 5051) label
	go to 40

!	Start the case statement for key words


!	TIMES total_simulation, print interval, history interval, smokeview interval, spreadsheet interval

  150	if (.not.countargs(label,5,lcarray, xnumc-1, nret)) then
		 ierror = 1
		 return
	endif
      NSMAX =  lrarray(1)
      LPRINT = lrarray(2)
      LDIAGO = lrarray(3)
	if (ldiago.gt.0) ndumpr = 1
      LDIAGP = lrarray(4)
      lcopyss =  lrarray(5)
      GO TO 810

!	TAMB REFERENCE AMBIENT TEMPERATURE (C), REFERENCE AMBIENT PRESSURE, REFERENCE PRESSURE, relative humidity

  190	if (.not.countargs(label,4,lcarray, xnumc-1, nret)) then
		 ierror = 2
		 return
	endif
	TA = lrarray(1)
      PA = lrarray(2)
      SAL = lrarray(3)
	relhum = lrarray(4) * 0.01d0
      IF (.NOT.EXSET) THEN
        EXTA = TA
        EXPA = PA
        EXRA = RA
        EXSAL = SAL
      END IF
      GO TO 810

!	EAMB REFERENCE EXTERNAL AMBIENT TEMPERATURE (C), REFERENCE EXTERNAL AMBIENT PRESSURE, REFERENCE EXTERNAL AMBIENT HEIGHT

  200	if (.not.countargs(label,3,lcarray, xnumc-1, nret)) then
		 ierror = 3
		 return
	endif
      EXTA = lrarray(1)
      EXPA = lrarray(2)
      EXSAL = lrarray(3)
      EXSET = .TRUE.
      GO TO 810

!	Limiting oxygen index

  210	if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 4
		 return
	endif
	limo2 = lrarray(1) * 0.01d0
	go to 810
	  
C     Rename the THERMAL or object DATA FILE

  225	if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 5
		 return
	endif
	ofile = lcarray(1)
      GO TO 810

  230	if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 6
		 return
	endif
	THRMFILE = lcarray(1)
      GO TO 810

!	Mainfire (MAINF)

  240	if (.not.countargs(label,5,lcarray, xnumc-1, nret)) then
		 ierror = 7
		 return
	endif
	objnin(0) = 'mainfire'
	lfbo =      lrarray(1)
	if(lfbo.lt.0.or.lfbo.gt.n-1) then
		 ierror = 64
		 return
	endif
!	Only constrained fires
	lfbt =      2
	fpos(1) =   lrarray(2)
	fpos(2) =   lrarray(3)
	fpos(3) =   lrarray(4)
	fplume(0) = lrarray(5)
	go to 810

!	Set the gaseous ignition temperature - this is a global parameter DJIGN

  250 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		return
		ierror = 4
	endif
	tgignt = lrarray(2)
	go to 810

!	Set global parameters - CHEMI is redundant with DJIGN (250) and LIMO2 (210)

  260	if (.not.countargs(label,2,lcarray,xnumc-1, nret)) then
		return
		ierror = 4
	endif
	limo2 = lrarray(1) * 0.01d0
	tgignt = lrarray(2)
	go to 810
	
!	COMPA	name(c), width(f), depth(f), height(f), absolute position (f) (3),
!           ceiling_material(c), floor_material(c), wall_material (c)

  470	if (.not.countargs(label, 10, lcarray, xnumc-1, nret)) then
		 ierror = 8
		 return
	endif
	
	compartment = compartment + 1
	if (compartment.gt.nr) then
		 write (logerr, 5062) compartment
		 ierror = 9
		 return
	endif

!	Name
	compartmentnames(compartment) = lcarray(1)
	
!	Size
	br(compartment) = lrarray(2)
	dr(compartment) = lrarray(3)
	hr(compartment) = lrarray(4)
	cxabs(compartment) = lrarray(5)
	cyabs(compartment) = lrarray(6)
	hflr(compartment) = lrarray(7)
	
!	Ceiling
      tcname = lcarray(8)
      IF (TCNAME.NE.'OFF') THEN
		 SWITCH(1,compartment) = .TRUE.
		 CNAME(1,compartment) = TCNAME
!		 Keep track of the total number of thermal properties used
		 NUMTHRM = NUMTHRM + 1
      END IF

!	Floor
      tcname = lcarray(9)
      IF (TCNAME.NE.'OFF') THEN
		 SWITCH(2,compartment) = .TRUE.
		 CNAME(2,compartment) = TCNAME
!		 Keep track of the total number of thermal properties used
		 NUMTHRM = NUMTHRM + 1
      END IF

!	Walls
      tcname = lcarray(10)
      IF (TCNAME.NE.'OFF') THEN
		 SWITCH(3,compartment) = .TRUE.
		 CNAME(3,compartment) = TCNAME
		 SWITCH(4,compartment) = .TRUE.
          CNAME(4,compartment) = TCNAME
!		 Keep track of the total number of thermal properties used
		 NUMTHRM = NUMTHRM + 1
      END IF

!	Reset this each time in case this is the last entry
	n = compartment+1
!	Throwaway index
	nx = compartment

	write (logerr,5063) compartment, compartmentnames(nx), br(nx),
     + dr(nx), hr(nx),cxabs(nx),cyabs(nx),hflr(nx),(switch(i,nx),i=1,4),
     + (cname(i,nx),i=1,4)
  	GO TO 810

!	HVENT 1st, 2nd, which_vent, width, soffit, sill, wind_coef, hall_1, hall_2, face, opening_fraction
!		 BW = width, HH = soffit, HL = sill, 
!		 HHP = ABSOLUTE HEIGHT OF THE soffit,HLP = ABSOLUTE HEIGHT OF THE sill, HFLR = ABSOLUTE HEIGHT OF THE FLOOR (not set here)
!		 WINDC = A WIND COEFFICIENT WHICH VARIES FROM -1 TO +1 AND IS DIMENSIONLESS
!		 Compartment offset for the HALL command (2 of these)
!		 VFACE = THE RELATIVE FACE OF THE VENT: 1-4 FOR X PLANE (-), Y PLANE (+), X PLANE (+), Y PLANE (-)
!		 Initial open fraction

  550 if (.not.countargs(label, 11, lcarray, xnumc-1, nret)) then
		 ierror = 10
		 return
	endif

!	We do not keep track of the number of vents. They are counted in the DATACOPY initialization (NVENTS)

      I = lrarray(1)
      J = lrarray(2)
      K = lrarray(3)
      IMIN = MIN(I,J)
      JMAX = MAX(I,J)
      IF (IMIN.GT.NR-1.OR.JMAX.GT.NR.OR.IMIN.EQ.JMAX) THEN
        WRITE (LOGERR,5070) I, J
	  ierror = 78
        return
      END IF
      IF (K.GT.mxccv) THEN
        WRITE (logerr,5080) I, J, K, NW(I,J)
	  ierror = 78
	  return
	END IF
      NVENTIJK = NVENTIJK + 1
	if (nventijk.gt.mxvents) then
	  write(logerr,5081) i,j,k
	  ierror = 78
	  return
	endif
	IJK(I,J,K) = NVENTIJK
      IJK(J,I,K) = IJK(I,J,K)
      IIJK = IJK(I,J,K)
      JIK = IIJK
      KOFFST = 2 ** K
      IF (IAND(KOFFST,NW(I,J)).NE.0) THEN
        WRITE (IOFILO,5090) I, J, K
      END IF
      NW(I,J) = IOR(NW(I,J),KOFFST)
      BW(IIJK) = lrarray(4)
      HH(IIJK) = lrarray(5)
      HL(IIJK) = lrarray(6)
      WINDC(IIJK) = lrarray(7)
      HALLDIST(IIJK,1) = lrarray(8)
      HALLDIST(IIJK,2) = lrarray(9)
	VFACE(IIJK) = lrarray(10)
	initialopening = lrarray(11)

! version 5 and earlier
!	do 551 l = 1, nv
!  551	qcvent(iijk,l) = initialopening
	
	qcvh(2,iijk) = initialopening
	qcvh(4,iijk) = initialopening

      HHP(IIJK) = HH(IIJK) + HFLR(I)
      HLP(IIJK) = HL(IIJK) + HFLR(I)

C     CONNECTIONS ARE BIDIRECTIONAL

      NW(J,I) = NW(I,J)
      HH(JIK) = MIN(HR(J),MAX(XX0,HHP(JIK)-HFLR(J)))
      HL(JIK) = MIN(HH(JIK),MAX(XX0,HLP(JIK)-HFLR(J)))

C     ASSURE OURSELVES THAT THE CONNECTIONS ARE SYMMETRICAL

      HHP(JIK) = HH(JIK) + HFLR(J)
      HLP(JIK) = HL(JIK) + HFLR(J)
      HH(IIJK) = MIN(HR(I),MAX(XX0,HHP(IIJK)-HFLR(I)))
      HL(IIJK) = MIN(HH(IIJK),MAX(XX0,HLP(IIJK)-HFLR(I)))
      GO TO 810

!	EVENT - H First_Compartment     Second_Compartment	 Vent_Number Time Final_Fraction decay_time
!	EVENT - V First_Compartment     Second_Compartment	 Not_Used	 Time Final_Fraction decay_time
!	EVENT - M Not_Used				  Not_used				 M_ID        Time Final_Fraction decay_time
!	EVENT - F Not_Used				  Not_used				 M_ID        Time Final_Fraction decay_time

  552	if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 11
		 return
	endif

!	Sort by event type, h, v or m
	venttype = lcarray(1)
		
	select case (venttype)
	case ('H')
		 if (.not.countargs(label,7,lcarray, xnumc-1, nret)) then
			  ierror = 11
			  return
		 endif
		 i = lrarray(2)
		 j = lrarray(3)
		 k = lrarray(4)
		 iijk = ijk(i,j,k)
		 qcvh(1,iijk) = lrarray(5)
		 qcvh(3,iijk) = lrarray(5) + lrarray(7)
		 qcvh(4,iijk) = lrarray(6)		 
	case ('V')
		 if (.not.countargs(label,7,lcarray, xnumc-1, nret)) then
			  ierror = 11
			  return
		 endif
!	Sort these out in datacopy; we duplicate here so that readinputfile does not have to sort these as well
		 itop = lrarray(2)
		 ibot = lrarray(3)
		 qcvpp(1,itop,ibot) = lrarray(5)
		 qcvpp(3,itop,ibot) = lrarray(5) + lrarray(7)
		 qcvpp(4,itop,ibot) = lrarray(6)
		 qcvpp(1,ibot,itop) = lrarray(5)
		 qcvpp(3,ibot,itop) = lrarray(5) + lrarray(7)
		 qcvpp(4,ibot,itop) = lrarray(6)
	case ('M')
		 if (.not.countargs(label,7,lcarray, xnumc-1, nret)) then
			  ierror = 11
			  return
		 endif
		 fannumber = lrarray(4)
		 qcvm(1,fannumber) = lrarray(5)
		 qcvm(3,fannumber) = lrarray(5) + lrarray(7)
		 qcvm(4,fannumber) = lrarray(6)
	case ('F')
		 if (.not.countargs(label,7,lcarray, xnumc-1, nret)) then
			  ierror = 11
			  return
		 endif
		 fannumber = lrarray(4)
		 if (fannumber.gt.nfan) then
			ierror = 82
			write(logerr,5196) fannumber
			return
		 endif
		 nfilter = nfilter + 1
		 qcvf(1,fannumber) = lrarray(5)
		 qcvf(3,fannumber) = lrarray(5) + lrarray(7)
		 qcvf(4,fannumber) = lrarray(6)
	case default
		 ierror = 71
		 return
	end select  
	go to 810

!     VVENT - from_compartment to_compartment area shape initial_fraction

  560 if (.not.countargs(label,5,lcarray, xnumc-1, nret)) then
		 ierror = 23
		 return
	endif
      I = lrarray(1)
      J = lrarray(2)
!     Check for outside of compartment space; self pointers are covered in readinputfile
      IF (I.GT.NR.OR.J.GT.NR) THEN
        WRITE (LOGERR,5070) I, J
	  ierror = 79
	  return
      END IF

!	readinputfile will verify the orientation (i is on top of j)

      NWV(I,J) = 1
      VVAREA(I,J) = lrarray(3)
!	Check the shape parameter. The default (1) is a circle)
	if (lrarray(4).lt.1.or.lrarray(4).gt.2) then
		 vshape(i,j) = 1
	else
	    VSHAPE(I,J) = lrarray(4)
	endif
	qcvpp(2,i,j) = lrarray(5)
	qcvpp(2,j,i) = lrarray(5)
	qcvpp(4,i,j) = lrarray(5)
	qcvpp(4,j,i) = lrarray(5)
      GO TO 810

!	WIND - VELOCITY AT REFERENCE HEIGHT and EXPONENTIAL LAPSE RATE 

  620 if (.not.countargs(label,3,lcarray, xnumc-1, nret)) then
		 ierror = 24
		 return
	endif
      WINDV = lrarray(1)
      WINDRF = lrarray(2)
      WINDPW = lrarray(3)
      GO TO 810

C     INTER - SET THE INITIAL INTERFACE HEIGHT ONLY IF IT DIFFERENT THAN THE DEFAULT
!	This key word takes arguements in pairs - compartment, height

  630 if (.not.countargs(label,2,lcarray, xnumc-1, nret)) then
		 ierror = 25
		 return
	endif
      IF ((NRET/2)*2.NE.NRET) THEN
!	There have to be pairs of numbers
		 WRITE (IOFILO,5130) NRET
		 ierror = 73
		 return
      END IF
      DO 640 I = 1, NRET - 1, 2
	  j = lrarray(i)
        IF (j.gt.n.or.j.lt.1) THEN
          WRITE (IOFILO,5140) I, j
		 ierror = 26
		 return
        ELSE
		 xxlocal = lrarray(i+1)
		 if(xxlocal.lt.xx0.or.xxlocal.gt.hr(j)) then
			  ierror = 72
		 return
		 endif
		 INTER(j) = xxlocal
        END IF
  640 CONTINUE
      GO TO 810

!	MVENT - simplified mechanical ventilation

!      (1) From_Compartment, (2) To_Compartment, (3) ID_Number
!      (4-6) From_Opening_Orientation From_Center_Height From_Opening_Area 
!      (7-9) To_Opening_Orientation To_Center_Height To_Opening_Area 
!      (10-12) Flow Flow_Begin_Dropoff_Pressure Zero_Flow_Pressure
!	 (13) Initial fraction of the fan speed

  660 if (.not.countargs(label, 13, lcarray, xnumc-1, nret)) then 
		 ierror = 12
		 return
	endif

!!!!!!!! This is only the case for a vent which has not been previously defined

      mid = lrarray(3)
      iecfrom = lrarray(1)
      iecto = lrarray(2)
	if (iecfrom.gt.n.or.iecto.gt.n) then
		 write(logerr,5191) iecfrom, iecto
		 ierror = 67
		 return
	endif

      orientypefrom = lcarray(4)
      HEIGHTfrom = lrarray(5)
      AREAfrom = lrarray(6)
      orientypeto = lcarray(7)
      heightto = lrarray(8)
      areato = lrarray(9)
      minpres = lrarray(11)
      maxpres = lrarray(12)
	fanfraction = lrarray(13)

!     We start with two new nodes for the openings into the compartments for connections to the fan

! 	first compartment/node opening
      next = next + 1
      nnode = nnode + 1
      IF (next.GT.mext.or.nnode.gt.mnode) THEN
		 WRITE (logerr,5192) next,nnode
		 ierror = 68
		 return
      END IF
      IF (orientypefrom.EQ.'V') THEN
        HVORIEN(next) = 1
      ELSE
        HVORIEN(next) = 2
      END IF
      HVNODE(1,next) = IECfrom
      HVNODE(2,next) = nnode
      HVELXT(next) = HEIGHTfrom
      AREXT(next) = AREAfrom

! 	second compartment/node opening
      next = next + 1
      nnode = nnode + 1
      IF (next.gt.mext.or.nnode.gt.mnode) THEN
		 WRITE (logerr,5192) next,nnode
		 ierror = 68
		 return
      END IF
      IF (orientypeto.EQ.'V') THEN
        HVORIEN(next) = 1
      ELSE
        HVORIEN(next) = 2
      END IF
      HVNODE(1,next) = iecto
      HVNODE(2,next) = nnode
      hvelxt(next) = heightto
      arext(next) = areato

!     now connect nodes 1 and 2 with a fan

      IF (minpres.gt.maxpres) THEN
        WRITE (logerr,5194) minpres,maxpres
	  ierror = 70
        return
      END IF

      NFAN = NFAN + 1
	if (mid.ne.nfan) then
		write(logerr,5193) mid,nfan
		ierror = 68
		return
	endif

      NBR = NBR + 1
      IF (NFAN.GT.MFAN.or.nbr.gt.mbr) THEN
		 WRITE (IOFILO,5195) MFAN
		 ierror = 70
		 return
      END IF
  
      NF(nbr) = nfan
      NFC(nfan) = 1
      NA(nbr) = hvnode(2,next-1)
      NE(nbr) = hvnode(2,next)
      HVDVOL(nbr) = xx0
      HMIN(nfan) = minpres
      HMAX(nfan) = maxpres
      HVBCO(nfan,1) = lrarray(10)

!     simple duct to connect the two nodes/fan - this is artificial since we do not worry about the species in the system

      NDT = NDT + 1
! To change from the zero volume calculation to a finite volume, use 1.0d1 (10 meter duct)
! The effect is in hvfrex. Case 1 is the finite volume and case 2, the zero volume calculation for flow through the external nodes
      DL(NDT) = XX0 ! 1.0d1
      DE(NDT) = lrarray(6)
      IBRD(NDT) = NBR

!	Finally, we set the initial fraction opening
	qcvm(2,mid) = lrarray(13)
	qcvm(4,mid) = lrarray(13)


!	write(logerr,9997) next-1,hvorien(next-1),hvnode(1,next-1),
!     . hvnode(2,next-1),hvelxt(next-1),arext(next-1),
!     . hvght(hvnode(2,next-1))
!	write(logerr,9997) next,hvorien(next),hvnode(1,next),
!     . hvnode(2,next),hvelxt(next),arext(next),hvght(hvnode(2,next))
!	write(logerr,999) nbr,nf(nbr),na(nbr),ne(nbr)
!	write(logerr,5272) nfan,na(nbr),ne(nbr),minpres,maxpres,nfc(nfan)
	go to 810
  
C     MVOPN Compartment_Number Duct_Work_Node Orientation Height Area

  670 if (.not.countargs(label,5,lcarray, xnumc-1, nret)) then
		 ierror = 63
		 return
	endif
	iec = lrarray(1)
	iic = lrarray(2)
	IF (iec.GT.NR.OR.iic.GT.MNODE) THEN
		 WRITE (IOFILO,5180) iec,iic
		 ierror = 27
		 GO TO 810
      END IF
      orientyp = lcarray(3)
      HEIGHT = lrarray(4)
      AREA = lrarray(5)
      ISET = NEXT
C     HVNODE IS THE PAIRWISE NODE CONNECTIONS FROM THE COMPARTMENTS TO THE HVAC
      DO 680 I = 1, NEXT
        IF (HVNODE(1,I).EQ.IEC.AND.HVNODE(2,I).EQ.IIC) THEN
          ISET = I
          WRITE (logerr,5200) HVNODE(1,I), HVNODE(2,I)
          GO TO 690
        END IF
  680 CONTINUE
      IF (NEXT.GE.MEXT) THEN
        WRITE (IOFILO,5210) MEXT
        GO TO 810
      ELSE
        NEXT = NEXT + 1
        ISET = NEXT
      END IF
  690 IF (ORIENTYP.EQ.'V') THEN
        HVORIEN(ISET) = 1
      ELSE
        HVORIEN(ISET) = 2
      END IF
      HVNODE(1,ISET) = IEC
      HVNODE(2,ISET) = IIC
      HVELXT(ISET) = HEIGHT
      AREXT(ISET) = AREA
      HVGHT(HVNODE(2,ISET)) = HVELXT(ISET)

      GO TO 810

!	MVFAN First_Node Second_Node Minimum_Pressure Maximum_Pressure 1_to_5_Coefficients

  710 if (.not.countargs(label,5,lcarray, xnumc-1, nret)) then
		 ierror = 29
		 return
	endif

C     MUST HAVE AT LEAST FROM/TO HMIN/HMAX (PRESSURE), AND AT LEAST ONE COEFFICI

	minpres = lrarray(3)
	maxpres = lrarray(4)
	IF (minpres.gt.maxpres) THEN
        WRITE (logerr,5270) minpres,maxpres
	  ierror = 30
        return
      END IF

C*** IS THIS FAN ALREADY DEFINED?

	i1 = lrarray(1)
	i2 = lrarray(2)
      DO 715 I = 1, NBR
         IFAN = NF(I)
         IF(IFAN.EQ.0)GO TO 715
         IF(NA(I).EQ.i1.AND.NE(I).EQ.i2) THEN
            ISET = I
            WRITE(logerr,5271) i1, i2
            GO TO 716
         ENDIF
  715 CONTINUE

C*** NO, SO UPDATE COUNTERS

      NFAN = NFAN + 1
      NBR = NBR + 1
      ISET = NBR
      IFAN = NFAN

      IF (NFAN.GT.MFAN) THEN
		 WRITE (IOFILO,5260) MFAN
		 ierror = 31
		 return
      END IF

  716 CONTINUE
      NFC(IFAN) = MIN(MFCOE,MAX(NRET-4,0))
      NF(ISET) = IFAN
      NA(ISET) = i1
      NE(ISET) = i2
      HVDVOL(ISET) = XX0
      HMIN(IFAN) = minpres
      HMAX(IFAN) = maxpres
      DO 720 J = 1, NFC(IFAN)
        HVBCO(IFAN,J) = lrarray(J+4)
  720 CONTINUE

      GO TO 810

!	OBJECT NAME ROOM POS(3) PLUME IGNITION_TYPE IGNITION_CRITERION NORMAL(3)

  770 if (.not.countargs(label,11,lcarray, xnumc-1, nret)) then
		 ierror = 32
		 return
	endif
	IF (NUMOBJL.GE.MXOIN) THEN
        WRITE(LOGERR,5300)
        GO TO 810
      END IF
      TCNAME = lcarray(1)
      IROOM = lrarray(2)
      IF (IROOM.LT.1.OR.IROOM.GT.N-1) THEN
		 WRITE(LOGERR,5320)IROOM
		 ierror = 33
		 return
      END IF
      OBPNT = NUMOBJL + 1
      NUMOBJL = OBPNT

!	Only constrained fires
	objtyp(numobjl) = 2
	if (objtyp(numobjl).gt.2) then
		 write(logerr,5321) objtyp(numobjl)
		 ierror = 63
		 return
	endif

      OBJPOS(1,OBPNT) = lrarray(3)
      OBJPOS(2,OBPNT) = lrarray(4)
      OBJPOS(3,OBPNT) = lrarray(5)
	if (objpos(1,obpnt).gt.br(iroom).or.
     .    objpos(2,obpnt).gt.dr(iroom).or.
     .    objpos(3,obpnt).gt.hr(iroom)) then
		write(logerr,5323) obpnt
		ierror = 82
		return
	endif

	fplume(numobjl) = lrarray(6)
      OBJIGN(OBPNT) =   lrarray(7)
      TMPCOND =         lrarray(8)
      OBJORT(1,OBPNT) = lrarray(9)
      OBJORT(2,OBPNT) = lrarray(10)
      OBJORT(3,OBPNT) = lrarray(11)
!	Enforce sanity; normal pointing vector must be non-zero (blas routine)
	if (dnrm2(3,objort(1,obpnt),1).le.0.0) then
		 write(logerr,5322)
		 ierror = 216
		 return
	endif
      OBJRM(obpnt) = IROOM
      OBJNIN(obpnt) = TCNAME
      OBJLD(obpnt) = .TRUE.
      OBJON(OBPNT) = .FALSE.
!	This is redudant but needed to be compatible with the object database format
	objpnt(obpnt) = obpnt

!!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
!!!!! The critiria for temperature and flux are stored backupwards - this is historical
!!!!! See corresponding code in updobj
      IF (TMPCOND.GT.0.0D0) THEN
        IF (OBJIGN(OBPNT).EQ.1) THEN
          OBJCRI(1,OBPNT) = TMPCOND
          OBJCRI(2,OBPNT) = 1.0D30
          OBJCRI(3,OBPNT) = 1.0D30
        ELSE IF (OBJIGN(OBPNT).EQ.2) THEN
          OBJCRI(1,OBPNT) = 1.0D30
          OBJCRI(2,OBPNT) = 1.0D30
          OBJCRI(3,OBPNT) = TMPCOND
        ELSE IF (OBJIGN(OBPNT).EQ.3) THEN
          OBJCRI(1,OBPNT) = 1.0D30
          OBJCRI(2,OBPNT) = TMPCOND
          OBJCRI(3,OBPNT) = 1.0D30
        ELSE
          write(logerr,5358) objign(obpnt)
		IERROR = 13
		RETURN
        END IF
	ELSE
        OBJON(OBPNT) = .TRUE.
      END IF
      IF (OPTION(FBTOBJ).EQ.OFF.AND.OBJIGN(OBPNT).NE.1.) THEN
        IF (STPMAX.GT.0.0D0) THEN
		 STPMAX = MIN(STPMAX,1.D0)
        ELSE
          STPMAX = 1.D0
        END IF
      END IF
      GO TO 810

C     CEILING JET (CJET)- walls, ceiling, all or off

  780 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 34
		 return
	endif
      DO 790 I = 1, 5
        CJETON(I) = .FALSE.
  790 CONTINUE
      cjtype = lcarray(1)(1:1)
      IF (cjtype.NE.' ') THEN
		 IF (cjtype.EQ.'C') THEN
			  CJETON(1) = .TRUE.
			  CJETON(5) = .TRUE.
		 ELSE IF (cjtype.EQ.'W') THEN
			  CJETON(3) = .TRUE.
			  CJETON(4) = .TRUE.
			  CJETON(5) = .TRUE.
		 ELSE IF (cjtype.EQ.'A') THEN
			  CJETON(1) = .TRUE.
			  CJETON(3) = .TRUE.
			  CJETON(4) = .TRUE.
			  CJETON(5) = .TRUE.
	    ENDIF
	ENDIF
	write(logerr,5341) cjeton
      GO TO 810

!	STPMAX # - set the maximum time step to #

  805 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 35
		 return
	endif
      STPMAX = lrarray(1)
      GO TO 810

!	DETECT Type Compartment Activation_Temperature Width Depth Height RTI Suppression Spray_Density

  815 if (.not.countargs(label,9,lcarray, xnumc-1, nret)) then
		 ierror = 34
		 return
	endif

      NDTECT = NDTECT + 1
	if (ndtect.gt.mxdtect) then
		write (logerr, 5338)
		ierror = 81
		return
	endif

      i1 = lrarray(1)
	i2 = lrarray(2)
!	Force to heat detector if out of range
	IF (i1.GT.3) i1 = HEATD
	IXDTECT(NDTECT,DTYPE) = i1
	IROOM = i2
	IXDTECT(NDTECT,DROOM) = IROOM
	IF(IROOM.LT.1.OR.IROOM.GT.NR)THEN
	 	WRITE (logerr,5342) i2
	 	IERROR = 35
	 	RETURN
	ENDIF

      XDTECT(NDTECT,DTRIG) = lrarray(3)
      XDTECT(NDTECT,DXLOC) = lrarray(4)
      XDTECT(NDTECT,DYLOC) = lrarray(5)
      XDTECT(NDTECT,DZLOC) = lrarray(6)
      XDTECT(NDTECT,DRTI) =  lrarray(7)
      IXDTECT(NDTECT,DQUENCH) = lrarray(8)
      XDTECT(NDTECT,DSPRAY) = lrarray(9)*1000.D0
!	If spray density is zero, then turn off the sprinkler
      IF(XDTECT(NDTECT,DSPRAY).EQ.0.0D0)THEN
        IXDTECT(NDTECT,DQUENCH) = 0
      ENDIF
      IF(OPTION(FBTDTECT).EQ.OFF.AND.IXDTECT(NDTECT,DQUENCH).GT.0)THEN
        IF (STPMAX.GT.0) THEN
          STPMAX = MIN(STPMAX,1.D0)
        ELSE
          STPMAX = 1.D0
        END IF
      END IF
	if (compartmentnames(i2).eq.' ') then
		 write(logerr,5344) i2
		 ierror = 36
		 return
	else
		 write(logerr, 5343) i1,compartmentnames(i2)
	endif

	if (debugging) then
		write(*,5400) (xdtect(ndtect,i),i=1,dtxcol)
		write(*,5401) (ixdtect(ndtect,i),i=1,dticol)
		write(*,*)
	endif

	if(xdtect(ndtect,dxloc).gt.br(i2).or.
     .   xdtect(ndtect,dyloc).gt.dr(i2).or.
     .   xdtect(ndtect,dzloc).gt.hr(i2)) then
		write(logerr,5339) ndtect,compartmentnames(i2)
		ierror = 80
		return
	endif

      GO TO 810

!     VHEAT top_compartment bottom_compartment

  825 if (.not.countargs(label,2,lcarray, xnumc-1, nret)) then
		 ierror = 37
		 return
	endif

      i1 = lrarray(1)
      i2 = lrarray(2)
	if (i1.lt.1.or.i2.lt.1.or.i1.gt.n.or.i2.gt.n) then
		 write(logerr,5345) i1, i2
		 ierror = 38
		 return
	endif

      NSWAL = NSWAL + 1
      IZSWAL(NSWAL,1) = i1
      IZSWAL(NSWAL,2) = 1
      IZSWAL(NSWAL,3) = i2
      IZSWAL(NSWAL,4) = 3
      GO TO 810

!	ONEZ compartment number - This turns the compartment into a single zone

  835 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 39
		 return
	endif

      iroom = lrarray(1)
      IF(iroom.LT.1.OR.iroom.GT.n)THEN
		 write(logerr, 5001) i1
		 ierror = 40
		 return
	endif
      IZSHAFT(IROOM) = 1
      GO TO 810

!	TARGET - Compartment position(3) normal(3) Material Method Equation_Type

  910 if (.not.countargs(label,10,lcarray, xnumc-1, nret)) then
		 ierror = 41
		 return
	endif

      IF(NTARG+1.GT.MXTARG)THEN
		 write(logerr,5002) 
		 ierror = 42
		 return
	else
		 NTARG = NTARG + 1
      ENDIF
!	The target can exist, now for the compartment
      IROOM = lrarray(1)
      IF(IROOM.LT.1.OR.IROOM.GT.N)THEN
		 write(logerr,5003) iroom
		 ierror = 43
		 return
	endif

!	position and normal
      IXTARG(TRGROOM,NTARG)=IROOM
      DO 911 I = 0, 2
        XXTARG(TRGCENX+I,NTARG) = lrarray(2+I)
        XXTARG(TRGNORMX+I,NTARG) = lrarray(5+I)
  911 CONTINUE

!	material type
	tcname = lcarray(8)
	IF(TCNAME.EQ.' ')TCNAME='DEFAULT'
      CXTARG(NTARG) = TCNAME
      IXTARG(TRGWALL,NTARG) = 0

!	solution method
      METHOD = ' '
      method = lcarray(9)
      CALL UPPERALL(METHOD,METHOD)
      IF(METHOD.NE.' ')THEN
		 IF(METHOD(1:3).EQ.'STE') THEN
			  IXTARG(TRGMETH,NTARG) = STEADY
			  METHOD = ' '
		 ELSEIF (METHOD(1:3).EQ.'IMP') THEN
			  IXTARG(TRGMETH,NTARG) = MPLICIT
		 ELSEIF (METHOD(1:3).EQ.'EXP') THEN
			  IXTARG(TRGMETH,NTARG) = XPLICIT
		 ELSE
		     WRITE(logerr,912) METHOD
			  ierror = 44
			  return
		 ENDIF
      ENDIF

!	equation type
      EQTYPE = ' '
      eqtype = lcarray(10)
      CALL UPPERALL(EQTYPE,EQTYPE)
      IF(EQTYPE.NE.' '.AND.METHOD.NE.' ')THEN
		 IF (EQTYPE(1:3).EQ.'ODE') THEN
			  IXTARG(TRGEQ,NTARG) = ODE
		 ELSEIF (EQTYPE(1:3).EQ.'PDE') THEN
	        IXTARG(TRGEQ,NTARG) = PDE
		 ELSE
	        WRITE(logerr,913) EQTYPE
			  ierror = 45
			  return
		 ENDIF
      ENDIF
      GO TO 810

!	HALL Compartment Velocity Depth Decay_Distance

  940 if (.not.countargs(label,4,lcarray, xnumc-1, nret)) then
		 ierror = 46
		 return
	endif

      IROOM = lrarray(1)

!	check that specified room is valid
      IF(IROOM.LT.0.OR.IROOM.GT.N)THEN
		 write(logerr,5346) iroom
		 ierror = 63
		 return
      ENDIF

      IZHALL(IROOM,IHROOM) = 1
      IZHALL(IROOM,IHVELFLAG) = 0
      IZHALL(IROOM,IHDEPTHFLAG) = 0
      IZHALL(IROOM,IHVENTNUM) = 0
      ZZHALL(IROOM,IHTIME0) = -1.0D0
      ZZHALL(IROOM,IHVEL) = 0.0D0
      ZZHALL(IROOM,IHDEPTH) = -1.0D0
      ZZHALL(IROOM,IHHALF) = -1.0D0

!     CORRIDOR velocity; not set if negative
	if(lrarray(2).ge.0) then
		 ZZHALL(IROOM,IHVEL) = lrarray(2)
		 IZHALL(IROOM,IHVELFLAG) = 1
	endif

!	CORRIDOR layer depth; not set if negative
      IF (lrarray(3).ge.0) THEN
		 ZZHALL(IROOM,IHDEPTH) = lrarray(3)
		 IZHALL(IROOM,IHDEPTHFLAG) = 1
      ENDIF

!	CORRIDOR temperature decay distance (temperature decays by 0.50); if negative, not set
	if (lrarray(4).ge.0) then
		 ZZHALL(IROOM,IHHALF) = lrarray(4)
		 IZHALL(IROOM,IHHALFFLAG) = 1
		 IZHALL(IROOM,IHMODE) = IHBEFORE
      ENDIF
      GO TO 810

!	ROOMA Compartment Number_of_Area_Values Area_Values
!	This provides for variable compartment floor areas; this should be accompanied by the roomh command

  950 if (.not.countargs(label,2,lcarray, xnumc-1, nret)) then
		 ierror = 47
		 return
	endif

      IROOM = lrarray(1)

!	make sure the room number is valid
      IF(IROOM.LT.1.OR.IROOM.GT.N)THEN
		 write(logerr,5347) iroom
		 ierror = 48
		 return
      ENDIF

!	make sure the number of points is valid
      NPTS = lrarray(2)
      IF(NPTS.GT.MXPTS.OR.NPTS.LE.0.or.npts.ne.nret-2) THEN
		 write (logerr,5347) npts
		 ierror = 49
		 return
      ENDIF
      IF(IZRVOL(IROOM).NE.0) NPTS = MIN(IZRVOL(IROOM),NPTS)
      IZRVOL(IROOM) = NPTS

!	make sure all data is positive 
      DO 954 I = 1, npts
        IF(lrarray(i+2).LT.0.0D0)THEN
			write(logerr,5348) lrarray(i+2)
 			ierror = 50
			return
        ENDIF
  954 CONTINUE

!	put the data in its place
      DO 951 I = 1, NPTS
        ZZRAREA(I,IROOM) = lrarray(i+2)
  951 CONTINUE
	write(logerr,5351) iroom, (zzrarea(iroom,i),i=1,npts)
      GO TO 810

!	ROOMH Compartment Number_of_Height_Values Height_Values
!	This companion to ROOMA, provides for variable compartment floor areas; this should be accompanied by the ROOMA command

  960 if (.not.countargs(label,2,lcarray, xnumc-1, nret)) then
		 ierror = 51
		 return
	endif

      IROOM = lrarray(1)

!	make sure the room number is valid
      IF(IROOM.LT.1.OR.IROOM.GT.N)THEN
		 write(logerr,5349) iroom
		 ierror = 52
		 return
      ENDIF

!	make sure the number of points is valid
      NPTS = lrarray(2)
      IF(NPTS.GT.MXPTS.OR.NPTS.LT.0.or.npts.ne.nret-2)THEN
		 write(logerr,5350) npts
		 ierror = 53
		 return
	endif
      IF(IZRVOL(IROOM).NE.0)NPTS = MIN(IZRVOL(IROOM),NPTS)
      IZRVOL(IROOM) = NPTS

!	make sure all data is positive 
      DO 955 I = 1, npts
        IF(lrarray(i+2).LT.0.0D0)THEN
			write(logerr,5348) lrarray(i+2)
 			ierror = 54
			return
        ENDIF
  955 CONTINUE

!	put the data in its place
      DO 961 I = 1, NPTS
        ZZRHGT(I,IROOM) = lrarray(i+2)
  961 CONTINUE
	write(logerr,5352) iroom, (zzrhgt(iroom,i),i=1,npts)
      GO TO 810

!	DTCHE Minimum_Time_Step Maximum_Iteration_Count

  970 if (.not.countargs(label,2,lcarray, xnumc-1, nret)) then
		 ierror = 55
		 return
	endif

      ZZDTCRIT = ABS(lrarray(1))
      IZDTMAX = ABS(lrarray(2))
!	a negative turns off the check
      IF(LRARRAY(2).LE.0)IZDTFLAG = .FALSE.
	go to 810

!	SETP file_name

  990 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 56
		 return
	endif

      IF (IFLGSETP.GT.0) THEN
		 ierror = 57
		 WRITE (LOGERR,5353) SETPFILE
		 return
	else
		 IFLGSETP = 1
		 setpfile = lcarray(1)
		 WRITE (LOGERR,5340) SETPFILE
      END IF
      GOTO 810

!	Horizontal heat flow

!	HHEAT First_Compartment Number_of_Parts N pairs of {Second_Compartment, Fraction}

!	There are two forms of the command
!		 The first (single entry of the room number) - all connections based on horizontal flow
!		 The second is the compartment number followed by N pairs of compartments to which the heat will flow and the 
!		 fraction of the vertical surface of the compartment that loses heat

 1010 if (.not.countargs(label,1,lcarray, xnumc-1, nret)) then
		 ierror = 58
		 return
	endif

      NTO = 0
      IFROM = lrarray(1)

	if (nret.eq.1) then
		 IZHEAT(IFROM) = 1
		 go to 810
	else
		 NTO = lrarray(2)
		 IF(NTO.LT.1.OR.NTO.GT.N)THEN
			  write(logerr,5354) nto
			  ierror = 59
			  RETURN
		 ENDIF
		 IZHEAT(IFROM) = 2
		 IZHEAT(IFROM) = 2
      ENDIF

	if (2*nto.ne.(nret-2)) then
		 write(logerr,	5355) ifrom, nto
		 ierror = 60
		 return
	endif

      DO 1011 I = 1, NTO
		 i1 = 2*I+1
	    i2 = 2*I+2
		 ITO = lrarray(i1)
		 FRAC = lrarray(i2)
		 IF(ITO.LT.1.OR.ITO.EQ.IFROM.OR.ITO.GT.N)THEN
			  write(logerr, 5356) ifrom,ito
			  IERROR = 61
			  RETURN
		 ENDIF
		 if(frac.lt.0.0d0.or.frac.gt.1.0d0)then
			  write(logerr, 5357) ifrom,ito,frac
			  ierror = 62
			  return
		 endif
		 ZZHTFRAC(IFROM,ITO) = FRAC
 1011 CONTINUE
      GO TO 810

!	HEATF Special fire - heat source only; no mass

 1240	if (.not.countargs(label,6,lcarray, xnumc-1, nret)) then
		 ierror = 65
		 return
	endif
	heatfr = lrarray(1)
	if(heatfr.lt.1.or.heatfr.gt.n-1) then
		 ierror = 66
		 return
	endif
	heatfl = .true.
	heatfp(1) = lrarray(2)
	heatfp(2) = lrarray(3)
	heatfp(3) = lrarray(4)
	heatfplume =  lrarray(5)
!	This is a constant heat source only
	heatfq = lrarray(6)
	go to 810


  912 FORMAT ('Invalid TARGET METHOD:',A8,'. Valid choices are: ',
     +         'STEADY, IMPLICIT OR EXPLICIT')
  913 FORMAT('Invalid equation type specified in TARGET:',A3,
     +          ' Valid choices are:ODE OR PDE')
 5000 FORMAT ('Keyword ',A5)
 5001	FORMAT ('ONEZ requires a defined compartment ',i3)
 5002 FORMAT ('Too many targets are being defined')
 5003 FORMAT ('The compartment specified by TARGET does not exist',i3)
 5030 FORMAT ('Thermal count does not match compartment count ',2I5)
 5051 FORMAT ('The key word ',a5,' is not recognized')
 5060 FORMAT ('THERE MUST BE SIX PARAMETERS TO SPECIFY A VENT',I5)
 5061 FORMAT ('NEED SIX PARAMETERS TO SPECIFY THE MAINFIRE',I5)
 5062 FORMAT ('Compartment number outside of allowable range',i5)
 5063 FORMAT ('Compartment ',i3,1x,a8,1x,6f6.1,4l,1x,4a10)
 5070 FORMAT ('VENT parameter(s) outside of allowable range',2I4)
 5080 FORMAT ('Too many pairwise horizontal connections',4I5)
 5081 format ('Too many horizontal connections ',3i5)
 5090 FORMAT ('The connection',3I3,' is being redefined')
 5100 FORMAT (' There must be at least 3 parameters to specify ',
     +        ' vertical flow (VVENT).',I4)
 5120 FORMAT ('NOT ENOUGH DATA FOR WIND INPUT ROUTINE ???')
 5130 FORMAT ('THE INTERFACE HEIGHT MUST BE SPECIFIED IN PAIRS',I4)
 5140 FORMAT ('Specification for interface height is outside of',
     +        ' allowable range',2I4)
 5170 FORMAT ('MVOPN must define both sides of a duct opening')
 5180 FORMAT ('Specified node number too large for this system',2I2)
 5191 format ('Compartments specified in MVENT have not been defined ',
     .        2i3)
 5192 format ('Exceeded maximum number of nodes/openings in MVENT ',2i3)
 5193 format ('MVENT(MID) is not consistent ',2i3)
 5194 format ('Pressure for zero flow must exceed the lower limit',
     .        f10.2)
 5195 format ('Too many fan systems',i3)
 5196 format ('Fan (MID) has not been defined for this filter ',i3)
 5200 FORMAT ('Redefinition for node ',2I3)
 5210 FORMAT ('Exceed max external connections',I3)
 5220 FORMAT ('Only ',I2,' specified for an mv duct, 5 required')
 5230 FORMAT ('Exceeded maximum number of mv ducts =',I2)
 5250 FORMAT ('Fan data in wrong format (>4)')
 5260 FORMAT ('Exceeded allowed number of fans',I3)
 5270 FORMAT ('Fan curve has incorrect specification',1P2G12.3)
 5271 FORMAT ('Fan between nodes:',i3,' and ',i3,' is being redefined')
 5272	FORMAT ('Define fan ',i2,' from ',i3,' to ',i3,
     +		  ' over the pressure range ',2f6.1,' with ',i2,
     +		  ' coefficients')
 5290 FORMAT ('Too many internal nodes specified')
 5300 FORMAT ('Too many objects defined in datafile')
 5310 FORMAT ('Incorrect number of parameters for OBJECT')
 5320 FORMAT ('Object specification error, room ',I4,' out of range')
 5321 FORMAT ('Object specification error, not an allowed fire type',i3)
 5322 FORMAT ('Object normal vector must be non-zero')
 5323 format ('Object ',i3,' is outside its compartment')
 5338 format ('Exceed allowed number of detectors')
 5339 format ('Detector ',i3,' is outside of compartment ',a)
 5340 FORMAT ('Set point file name is - ',A)
 5341	FORMAT ('Ceiling jet calculation has been set: ',5l2)
 5342	FORMAT ('Invalid DETECTOR specification - room ',i3)
 5343	FORMAT ('A type ',i3,' detector has been placed in ',a128)
 5344 FORMAT ('A referenced compartment is not yet defined ',i3)
 5345	FORMAT ('VHEAT has specified a non-existent compartment')
 5346 FORMAT ('HALL has specified a non-existent compartment',i3)
 5347 FORMAT ('Compartment specified by ROOMA does not exist ',i3)
 5348 FORMAT ('Data on the ROOMA (or H) line must be positive ',1pg12.3)
 5349 FORMAT ('Compartment specified by ROOMH is not defined ',i3)
 5350 FORMAT ('ROOMH error on data line ',i3)
 5351 FORMAT ('Compartment',i3,' has been redefined as a variable space'
     +        '- area: ',20f8.1)
 5352 FORMAT ('Compartment',i3,' has been redefined as a variable space'
     +        '- height: ',20f8.1)
 5353 FORMAT ('Trying to reset the point file name is - ',A)
 5354 FORMAT ('HHEAT to compartment out of bounds or not defined - ',i3)
 5355 FORMAT ('HHEAT fraction pairs is not consistent ',2i3)
 5356 FORMAT ('HHEAT specification error in compartment pairs: ',2i3)
 5357 FORMAT ('Error in fraction for HHEAT:',2i3,f5.3)
 5358 FORMAT ('Not a valid ignition criterion ',I5)
 5400 format ('xdtect = ',15f8.1)
 5401 format ('ixdtect = ',4i5)

      END

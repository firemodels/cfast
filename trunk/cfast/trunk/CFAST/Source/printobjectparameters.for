	subroutine printobjectparameters (iobj)

      include "cfast.fi"
      include "objects2.fi"
      include "fltarget.fi"

	write(*,5) OBJGMW(IOBJ), objvt(iobj),objmas(iobj)
5	format('gmwf,te,m ',3f8.3)
	write(*,6) objrm(iobj),objtyp(iobj),iobj
6	format ('compartment,type,# ',3i3)
	write(*,7) OBJLFM(IOBJ),(objxyz(j,iobj),j=1,3)
7	format('int,dimensions ',i3,3f8.2)
	write(*,11) objlfm(iobj),(objpos(j,iobj),j=1,3)
11    format('int,position   ',i3,3f8.2)
	write(*,12) objcl(iobj)
12    format('Characteristic volume = ',f10.3)
	write(*,*) 'targ#,targname ',obtarg(iobj),cxtarg(obtarg(iobj))
	write(*,4) nlspct,activs
4	format('nlspct,activs ',i3,10l5)

	write(*,8) (otime(i,iobj),i=1,OBJLFM(IOBJ))
8	format('time ',11f10.0)
	write(*,1) (oqdot(i,iobj),i=1,OBJLFM(IOBJ))
1	format('qdot ',11f10.0)
	write(*,3) (omass(i,iobj),i=1,OBJLFM(IOBJ))
3	format('mdot ',11f10.7)
	write(*,9) 'high ',(ohigh(i,iobj),i=1,OBJLFM(IOBJ))
9	format(a5,11f10.3)
	write(*,2) 'area ',(oarea(i,iobj),i=1,OBJLFM(IOBJ))
2	format(a5,11f10.0)
      write(*,9) 'ocra ',(ooc(I,iobj),i=1,OBJLFM(IOBJ))
	write(*,9) 'hcr  ',(ohcr(i,iobj),i=1,OBJLFM(IOBJ))
	write(*,9) 'coc2 ',(OCO(I,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'cco2 ',(ood(I,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'hcnf ',(omprodr(i,5,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'hclf ',(omprodr(i,6,iobj),i=1,OBJLFM(IOBJ))
      write(*,9) 'ct   ',(omprodr(i,10,iobj),i=1,OBJLFM(IOBJ))
	write(*,10) 'hocb ',(objhc(i,iobj),i=1,OBJLFM(IOBJ))
10	format(a5,11f10.0)
	write(*,*)

	return
	end

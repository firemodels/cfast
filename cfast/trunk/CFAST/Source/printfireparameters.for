	subroutine printfireparameters

      include "cfast.fi"

	write(*,5) gmwf,hcomba,te,tgignt,qradrl,limo2
5	format('gmwf,h,te,tg,q,o2 ',f8.3,1pg12.3,0p4f8.2)

	write (*,1) lfbo,lfbt,lfmax,fpos(1),
     + fpos(2),fpos(3),fplume(0)
1	format('room,type,int,pos,plume ',3i3,3f5.1,i3)

	write(*,4) nlspct,activs
4	format('nlspct,activs ',i3,10l5)

	write(*,2) 'time ',(tfired(i),i=1,lfmax)
2	format(a5,11f10.0)
	write(*,2) 'qfir ',(qfired(i),i=1,lfmax)
	write(*,3) 'bfir ',(bfired(i),i=1,lfmax)
3	format(a5,11f10.7)
	write(*,2) 'hfir ',(hfired(i),i=1,lfmax)
	write(*,2) 'afir ',(afired(i),i=1,lfmax)
      write(*,3) 'ocra ',(OCRATI(I),i=1,lfmax)
	write(*,3) 'hcra ',(hcratio(i),i=1,lfmax)
	write(*,3) 'coc2 ',(COCO2(I),i=1,lfmax)
      write(*,3) 'cco2 ',(CCO2(I),i=1,lfmax)
      write(*,3) 'hcnf ',(HCNF(I),i=1,lfmax)
      write(*,3) 'hclf ',(HCLF(I),i=1,lfmax)
	write(*,3) 'mpr5 ',(mprodr(i,5),i=1,lfmax)
	write(*,3) 'mpr6 ',(mprodr(i,6),i=1,lfmax)
	write(*,3) 'mpr10',(mprodr(i,10),i=1,lfmax)
	write(*,2) 'hocb ',(hocbmb(i),i=1,lfmax)
	write(*,*)

	return
	end

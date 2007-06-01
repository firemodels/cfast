	 subroutine shellsort (ra, n)

	 implicit none
	 integer n, j, i, inc
	 double precision ra(n), rra
	 
 	 inc = 1
1	 inc = 3*inc+1
	 if (inc.le.n) go to 1
2	 continue
		  inc = inc / 3
		  do i = inc+1, n
				rra = ra(i)
				j = i
3				if(ra(j-inc).gt.rra) then
					 ra(j) = ra(j-inc)
					 j = j - inc
					 if(j.le.inc) go to 4
				go to 3
				endif
4				ra(j) = rra
		  enddo
	 if(inc.gt.1) go to 2
	 return
	 end

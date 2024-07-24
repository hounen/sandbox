	subroutine pascal
	integer c,r
	common /ncr/c(20,20)
	nmax=20
	do 1 n=1,nmax
		c(n,1)=n
		c(n,n)=1
1	continue
	do 2 n=3,nmax
		mn1=n-1
		do 3 r=2,mn1
			c(n,r)=c(n-1,r-1)+c(n-1,r)
3		continue
2	continue
	return
	end

	subroutine insatu
	common /ncr/k(20,20)
	do 1 i=1,20
		write(6,601) (k(i,j),j=1,i)
601		format(' ',i3,i4,i5,i5,10i7,i6,i5,i5,i4,i3,i2)
1	continue
	return
	end

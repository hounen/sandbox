			program poten
			dimension u(100)
			character*1 ix2(78)
			dt =0.1d0
			dr =1.0d0
			wm =1.0d0
			write(*,*)'初期速度Vx入力してください'
			write(*,*)'典型的な値は2.0です'
			x1 =49.0d0
			read(*,*)vx1
			do 100 j=1,100
			   u(j)=(j-50.d0)*(j-50.d0)
			   u(j)=u(j)/500.d0
100   continue
			do 200 j=40,60
			   u(j)=350.d0-3.d0*(j-50)*(j-50)
		     u(j)=u(j)/250.d0
200   continue
			do 300 i=1,1000
			   do 210 l=1,70
210         ix2(l)=' '
			   j=ifix(x1)+1
			   vx2=vx1-dt*(u(j+1)-u(j))/(dr*wm)
			   x2=x1+dt*vx2
			if (mod(i-1,25).ne.0)goto 270
			do 250 k=1,20
			   j=ifix(x2)+1
			   yj=(u(j+1)+u(j))*0.5d0*3.5d0
			   iyj=20-yj
			   if(k.eq.iyj)then
			     xj=x2*0.7d0
			     ixj=ifix(xj)
			     ix2(ixj)='*'
			     write(*,1000)'+',(ix2(l),l=1,78)
			   else
			     write(*,*)'+'
			   endif
250   continue
			read(*,*)
270   continue
			   x1=x2
			   vx1=vx2
300   continue
1000  format(78a1)
			stop
			end


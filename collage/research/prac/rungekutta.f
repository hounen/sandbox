			implicit real*8 (a-h,o-z)
			dimension y(3)
			external func
			data x1,y1,ydi/0.d0,0.d0,1.d0/
			do 1 ih=1,3
				h=0.1**ih
				do 2 i=1,3
					xf=i*2
					call rk2(xi,yi,ydi,h,func,xf,yf,ydf)
2				 y(i)=yf

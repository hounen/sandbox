      function bessel_fuc(x,xnu)
      double precision x,xnu,bessel_fuc
      double precision ri,rk,rip,rkp
      call bessik(x,xnu,ri,rk,rip,rkp)
      bessel_fuc=ri
      return
      end
      
      subroutine bessik(x,xnu,ri,rk,rip,rkp)
      integer maxit
      double precision ri,rip,rk,rkp,x,xnu,xmin
      double precision eps,fpmin,pi
      parameter (eps=1.d-10,fpmin=1.d-30,maxit=10000,xmin=2.d0,
     & pi=3.141592653589793d0)
      integer i,l,nl
      double precision a,a1,b,c,d,del,del1,delh,dels,e,f,fact,
     & fact2,ff,gam1,gam2,gammi,gampl,h,p,pimu,q,q1,q2,
     & qnew,ril,ril1,rimu,rip1,ripl,ritemp,rk1,rkmu,rkmup,
     & rktemp,s,sum,sum1,x2,xi,xi2,xmu,xmu2
      if(x.le.0.d0.or.xnu.lt.0.d0) then
        write(*,*) 'bad arguments in bessik'
        stop
      endif
      nl=int(xnu+0.5d0)
      xmu=xnu-nl
      xmu2=xmu*xmu
      xi=1.d0/x
      xi2=2.d0*xi
      h=xnu*xi
      if(h.lt.fpmin) h=fpmin
      b=xi2*xnu
      d=0.d0
      c=h
      do i=1,maxit
        b=b+xi2
        d=1.d0/(b+d)
        c=b+1.d0/c
        del=c*d
        h=del*h
        if(abs(del-1.d0).lt.eps) goto 1
      enddo
      write(*,*) 'x too large in bessik; try asymptotic expansion'
      stop
    1 continue
      ril=fpmin
      ripl=h*ril
      ril1=ril
      rip1=ripl
      fact=xnu*xi
      do l=nl,1,-1
        ritemp=fact*ril+ripl
        fact=fact-xi
        ripl=fact*ritemp+ril
        ril=ritemp
      enddo
      f=ripl/ril
      if(x.lt.xmin) then
        x2=.5d0*x
        pimu=pi*xmu
        if(abs(pimu).lt.eps)then
          fact=1.d0
        else
          fact=pimu/sin(pimu)
        endif
        d=-log(x2)
        e=xmu*d
        if(abs(e).lt.eps)then
          fact2=1.d0
        else
          fact2=sinh(e)/e
        endif
        call beschb(xmu,gam1,gam2,gampl,gammi)
        ff=fact*(gam1*cosh(e)+gam2*fact2*d) 
        sum=ff
        e=exp(e)
        p=0.5d0*e/gampl
        q=0.5d0/(e*gammi)
        c=1.d0
        d=x2*x2
        sum1=p
        do i=1,maxit
          ff=(i*ff+p+q)/(i*i-xmu2)
          c=c*d/i
          p=p/(i-xmu)
          q=q/(i+xmu)
          del=c*ff
          sum=sum+del
          del1=c*(p-i*ff)
          sum1=sum1+del1
          if(abs(del).lt.abs(sum)*eps) goto 2
        enddo
        write(*,*) 'bessk series failed to converge'
        stop
    2   continue
        rkmu=sum
        rk1=sum1*xi2
      else
        b=2.d0*(1.d0+x)
        d=1.d0/b
        delh=d
        h=delh
        q1=0.d0
        q2=1.d0
        a1=.25d0-xmu2
        c=a1
        q=c
        a=-a1
        s=1.d0+q*delh
        do i=2,maxit
          a=a-2*(i-1)
          c=-a*c/i
          qnew=(q1-b*q2)/a
          q1=q2
          q2=qnew
          q=q+c*qnew
          b=b+2.d0
          d=1.d0/(b+a*d)
          delh=(b*d-1.d0)*delh
          h=h+delh
          dels=q*delh
          s=s+dels
          if(abs(dels/s).lt.eps) goto 3
        enddo
        write(*,*) 'bessik: failure to converge in cf2'
        stop
    3   continue
        h=a1*h
        rkmu=sqrt(pi/(2.d0*x))*exp(-x)/s
        rk1=rkmu*(xmu+x+.5d0-h)*xi
      endif
      rkmup=xmu*xi*rkmu-rk1
      rimu=xi/(f*rkmu-rkmup)
      ri=(rimu*ril1)/ril
      rip=(rimu*rip1)/ril
      do i=1,nl
        rktemp=(xmu+i)*xi2*rk1+rkmu
        rkmu=rk1
        rk1=rktemp
      enddo
      rk=rkmu
      rkp=xnu*xi*rkmu-rk1
      return
      end

      subroutine beschb(x,gam1,gam2,gampl,gammi)
      integer nuse1,nuse2
      double precision gam1,gam2,gammi,gampl,x
      parameter (nuse1=5,nuse2=5)
      double precision xx,c1(7),c2(8),chebev
      save c1,c2
      data c1/-1.142022680371168d0,6.5165112670737d-3,
     &     3.087090173086d-4,-3.4706269649d-6,6.9437664d-9,
     &     3.67795d-11,-1.356d-13/
      data c2/1.843740587300905d0,-7.68528408447867d-2,
     &     1.2719271366546d-3,-4.9717367042d-6,-3.31261198d-8,
     &     2.423096d-10,-1.702d-13,-1.49d-15/
      xx=8.d0*x*x-1.d0
      gam1=chebev(-1.d0,1.d0,c1,nuse1,xx)
      gam2=chebev(-1.d0,1.d0,c2,nuse2,xx)
      gampl=gam2-x*gam1
      gammi=gam2+x*gam1
      return
      end

      function chebev(a,b,c,m,x)
      integer m
      double precision chebev,a,b,x,c(m)
      integer j
      double precision d,dd,sv,y,y2
      if((x-a)*(x-b).gt.0.d0) pause 'x not in range in chebev'
      d=0.d0
      dd=0.d0
      y=(2.d0*x-a-b)/(b-a)
      y2=2.d0*y
      do j=m,2,-1
         sv=d
         d=y2*d-dd+c(j)
         dd=sv
      enddo
      chebev=y*d-dd+0.5d0*c(1)
      return
      end

      
      
      
      








      

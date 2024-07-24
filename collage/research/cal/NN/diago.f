c === diago.f =======================================================

      subroutine eigen(n,zg,zx,iprint)

c -------------------------------------------------------------------
      include 'li_inc.f'
      real*8 toler,anorm,eps
      dimension zg(nd,nd),zx(nd,nd),zwk(nd+1,nd/10+5)
c ---<diagonalization of complex matrix>-----------------------------
c     diagonalization of symmetric complex n-th matrix zg(i,j)
c                                  vector of j-th base zx(i,j)
c                                  zg(nd,nd),zx(nd,nd) nd >= n
c                                  zwk(nw,5) work nw >= nd+1
c     Output Level
c     iprint = 0 no print of eigen-values & vectors
c              1    print of eigen-values & vectors
c     nw=nd+1
c     toler is the relative precision of the machine

      toler=1.5d-60
      n1=n-1

      if(n1) 21,24,23
   24 zx(1,1)=(1.d0,0.d0)
      return

   23 do 1 i=1,n
      zx(i,i)=(1.d0,0.d0)
      if(i.eq.n) go to 22
      i1=i+1
      do 1  j=i1, n
      zx(i,j)=(0.d0,0.d0)
    1 zx(j,i)=(0.d0,0.d0)
   22 if(n.eq.2) go to 200

      zabsb=(0.d0,0.d0)
      n2=n-2

      do 2 k=1, n2
      zwk(k,1)=zg(k,k)
      k1=k+1
      zsigma=(0.d0,0.d0)
      do 3 i=k1, n
    3 zsigma=zsigma+zg(i,k)**2

      zabsb=sqrt(zsigma)
      zalpha=zg(k1,k)
      zbeta=zabsb
c      if(real(zalpha).ge.0.d0)zbeta=-zbeta
      if(dble(zalpha).ge.0.d0)zbeta=-zbeta
      zwk(k1,2)=zbeta
      if(abs(zsigma).le.abs(toler*zg(k,k))) go to 2
      zgamma=1.d0/(zsigma-zalpha*zbeta)
      zg(k1,k)=zalpha-zbeta

      do 6 i=k1, n
      zsum=(0.0d0,0.0d0)
      do 7 j=k1,i
    7 zsum=zsum+zg(i,j)*zg(j,k)

      if(i.eq.n) go to 6
      i1=i+1

      do 8 j=i1, n
    8 zsum=zsum+zg(j,i)*zg(j,k)

    6 zwk(i,4)=zsum*zgamma
      zsum=(0.0d0,0.0d0)

      do 9 i=k1, n
    9 zsum=zsum+zg(i,k)*zwk(i,4)
      zt=0.5d0*zgamma*zsum

      do 10 i=k1,n
   10 zwk(i,4)=zwk(i,4)-zt*zg(i,k)

      do 11 i=k1, n
      do 11 j=k1, i
   11 zg(i,j)=zg(i,j)-zg(i,k)*zwk(j,4)-zwk(i,4)*zg(j,k)

      do 12 i=2,n
      zsum=(0.0d0,0.0d0)

      do 13 j=k1,n
   13 zsum=zsum+zx(i,j)*zg(j,k)
   12 zwk(i,4)=zgamma*zsum

      do 14 i=2,n
      do 14 j=k1,n
   14 zx(i,j)=zx(i,j)-zwk(i,4)*zg(j,k)

    2 continue

  200 continue

      zwk(n1,1)=zg(n1,n1)
      zwk(n,1)=zg(n,n)
      zwk(n,2)=zg(n,n1)

      anorm=0.d0
      zta = (0.d0,0.d0)
      zwk(n+1,2) = (0.d0,0.d0)
      do 110 k=1,n
      zt=abs(zwk(k,1))+zta
      ta=abs(zwk(k+1,2))
      zt=zt+ta
      if(dble(zt).gt.anorm) anorm=dble(zt)
  110 continue
      eps = anorm*toler

      zamu=(0.d0,0.d0)
      zwk(1,2)=(0.d0,0.d0)
      m=n
   15 if(m.eq.0) go to 21
      i = m-1
      m1= i
      k = i
      if(abs(zwk(k+1,2)).gt.eps) go to 16
      zamu=(0.d0,0.d0)
      m = k
      go to 15
   16 i=i-1
      if(abs(zwk(i+1,2)).le.eps.or.i.eq.0) go to 18
      k=i
      go to 16
   18 zramda=(0.0d0,0.0d0)
      if(abs(zwk(m,1)-zamu).lt.0.5d0*abs(zwk(m,1)).or.m1.eq.k)
     &  zramda=zwk(m,1)+0.5d0*zwk(m1+1,2)
      zamu = zwk(m,1)
      zwk(k,1) = zwk(k,1) - zramda
      zbeta = zwk(k+1,2)

      do 19 j=k, m1
      za0=zwk(j,1)
      za1=zwk(j+1,1)-zramda
      zb0=zwk(j+1,2)
      zt=sqrt(za0**2+zbeta**2)
      zcosine=za0/zt
      zwk(j,4)=zcosine
      zsine=zbeta/zt
      zwk(j,5)=zsine
      zwk(j,1)=zcosine*za0+zsine*zbeta
      zwk(j+1,1)=-zsine*zb0+zcosine*za1
      zwk(j+1,2)=zcosine*zb0+zsine*za1
      zbeta=zwk(j+2,2)
      zwk(j+2,2)=zcosine*zbeta
   19 zwk(j+2,3)=zsine*zbeta
      zwk(k,2)  =(0.d0,0.d0)
      zwk(k+1,3)=(0.d0,0.d0)

      do 20 j=k,m1
      zsine=zwk(j,5)
      zcosine=zwk(j,4)
      za0=zwk(j,1)
      zb0=zwk(j+1,2)
      zwk(j,2)=zwk(j,2)*zcosine+zwk(j+1,3)*zsine
      zwk(j,1)=za0*zcosine+zb0*zsine+zramda
      zwk(j+1,2)=-za0*zsine+zb0*zcosine
      zwk(j+1,1)=zwk(j+1,1)*zcosine
      do 20 i=1,n
      zx0=zx(i,j)
      zx1=zx(i,j+1)
      zx(i,j)=zx0*zcosine+zx1*zsine
   20 zx(i,j+1)=-zx0*zsine+zx1*zcosine
      zwk(m,1)=zwk(m,1)+zramda
      go to 15
   21 do 120 m=1,n
  120 zg(m,m)=zwk(m,1)
      if(iprint.eq.0) return    !Output Level

      do 170 i=1,n,4
      i6=min0(i+3,n)
      write(6,601) (zg(j,j),j=i,i6)
      do 160 j=1,n
  160 write(6,602) j,(zx(j,j9),j9=i,i6)
  170 continue

c --------------------------------------------------------------------
  601 format(1h0,4x,4(2x,'(',e13.6,e15.6,')'))
  602 format(1x,'(',i3,')',f13.6,7f16.6)
      return
      end
c ===================================================================

      subroutine eignorm(n,ss,rma,rmb)

c -------------------------------------------------------------------
      include 'li_inc.f'
      dimension b(nd,2),rma(nd,nd),rmb(nd,nd)
c diagonalization of real norm matrix ---
c     n : dimension   ,obtained j-th eigenvalue :rma(i,i)
c     ss: ?           ,obtained j-th eigenvector:rmb(i,j)

      if(n.eq.1) go to 1034
      ind=0
      sum=0.0d0
      do 11 i=1,n
      do 11 j=1,n
      if(i-j) 10,11,10
   10 sum=sum+rma(i,j)**2
   11 continue

      sum=dsqrt(sum)
      sigma=n
      fnorm=sum/sigma
      e=fnorm*ss

      do 22 i=1,n
      do 22 j=1,n
      if(i-j) 21,20,21
   21 rmb(i,j)=0.0d0
      go to 22
   20 rmb(i,j)=1.0d0
   22 continue

  107 j=2
  108 i=1
  109 if(dabs(rma(i,j))-fnorm) 122,122,110
  110 ind=1
      r=-rma(i,j)
      q=0.5d0*(rma(i,i)-rma(j,j))

      if(q) 331,330,330
  330 signq=1.0d0
      go to 332

  331 signq=-1.0d0
  332 w=signq*r/dsqrt(r**2+q**2)

      sine=w/dsqrt(2.0d0*(1.0d0+dsqrt(1.0d0-w**2)))
c     sine=w/dsqrt(2.0d0*(1.0d0+dsqrt(q**2/(r**2+q**2))))
      cose=dsqrt(1.0d0-sine**2)

      do 200 k=1,n
      if(k-i) 201,200,201
  201 if(k-j) 202,200,202
  202 b(k,1)=rma(k,i)*cose-rma(k,j)*sine
      b(k,2)=rma(k,i)*sine+rma(k,j)*cose
  200 continue

      do 210 k=1,n
      if(k-i) 211,210,211
  211 if(k-j) 212,210,212
  212 rma(k,i)=b(k,1)
      rma(k,j)=b(k,2)
  210 continue

      do 220 k=1,n
      b(k,1)=rmb(k,i)*cose-rmb(k,j)*sine
  220 b(k,2)=rmb(k,i)*sine+rmb(k,j)*cose

      do 221 k=1,n
      rmb(k,i)=b(k,1)
  221 rmb(k,j)=b(k,2)

      b(1,1)=rma(i,i)*cose**2+rma(j,j)*sine**2-2.0d0*rma(i,j)*sine*cose
      b(2,2)=rma(i,i)*sine**2+rma(j,j)*cose**2+2.0d0*rma(i,j)*sine*cose
      b(2,1)=(rma(i,i)-rma(j,j))*sine*cose+rma(i,j)*(cose**2-sine**2)

      rma(i,i)=b(1,1)
      rma(j,j)=b(2,2)
      rma(j,i)=b(2,1)
      rma(i,j)=b(2,1)

      do 230 k=1,n
      if(k-i) 231,230,231
  231 if(k-j) 232,230,232
  232 rma(i,k)=rma(k,i)
      rma(j,k)=rma(k,j)
  230 continue

  122 if(i-j+1) 123,124,123
  123 i=i+1
      go to 109

  124 if(j-n) 125,126,125
  125 j=j+1
      go to 108

  126 if(ind-1) 128,127,128
  127 ind=0
      go to 107

  128 if(e-fnorm) 106,129,129
  106 fnorm=fnorm/sigma
      go to 107

  129 continue

      return

 1034 rmb(1,1)=1.0d0
      return

      end
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine eigj(n,g,x,iprint)
      include 'li_inc.f'
      dimension a(nd,5),g(nd,nd),x(nd,nd)

c     diagonalization of a real symmetric matrix by householder=s method
c     and the qr algolthm
      double precision sigma, sum,  norm,  t
      real*8 mu,lambda
      ind=0
c     toler is the relative precision of the machine
      toler=1.5d-13
c     preliminary accounting
      n1=n-1
      if(n1) 21,24,23
   24 x(1,1)=1.d00
      a(1,1)=g(1,1)
      go to 21
   23 if(ind.eq.3) go to 100
c    set x to identity
      do 1 i=1,n
      x(i,i)=1.d00
      if(i.eq.n) go to 22
      i1=i+1
      do 1  j=i1, n
      x(i,j)=0.0d00
    1 x(j,i)=0.0d00
   22 if(ind.eq.2) go to 100
      if(n.eq.2) go to 200
c     reduce g to tridiagonal form using n-2 orthogonal transforms
c     (i-2ww)=(i-gamma*uu)
c     g is replaced by u below the main diagonal
c     norm is the infinity norm of the reduced matrix.
      absb=0.0d00
      n2=n-2
      do 2 k=1, n2
      a(k,1)=g(k,k)
      k1=k+1
      sigma=0.0d0
      do 3 i=k1,n
    3 sigma=sigma+g(i,k)**2
      absb=dsqrt(sigma)
      alpha=g(k1,k)
      beta=absb
      if(alpha.ge.0.0d0)beta=-beta
      a(k1,2)=beta
      if(sigma.eq.0.0d0) go to 2
      gamma=1.0d00/(sigma-alpha*beta)
      g(k1,k)=alpha-beta
      do 6 i=k1, n
      sum=0.0d0
      do 7 j=k1,i
    7 sum=sum+g(i,j)*g(j,k)
      if(i.eq.n) go to 6
      i1=i+1
      do 8 j=i1, n
    8 sum=sum+g(j,i)*g(j,k)
    6 a(i,4)=sum*gamma
      sum=0.0d00
      do 9 i=k1,n
    9 sum=sum+g(i,k)*a(i,4)
      t=0.5d00*gamma*sum
      do 10 i=k1,n
   10 a(i,4)=a(i,4)-t*g(i,k)
      do 11 i=k1, n
      do 11 j=k1, i
   11 g(i,j)=g(i,j)-g(i,k)*a(j,4)-a(i,4)*g(j,k)
      do 12 i=2,n
      sum=0.0d0
      do 13 j=k1,n
   13 sum=sum+x(i,j)*g(j,k)
   12 a(i,4)=gamma*sum
      do 14 i=2,n
      do 14 j=k1,n
   14 x(i,j)=x(i,j)-a(i,4)*g(j,k)
    2 continue
  200 continue
      a(n1,1)=g(n1,n1)
      a(n,1)=g(n,n)
      a(n,2)=g(n,n1)
      if(ind.eq.1) return
c     calculation of the infinity norm of the reduced matrix.
  100 norm=0.0d00
      ta=0.0d00
      a(n+1,2)=0.0d00
      do 110 k=1,n
      t=dabs(a(k,1))+ta
      ta=dabs(a(k+1,2))
      t=t+ta
  110 norm=dmax1(norm, t)
      eps=norm*toler
c     infinity norm times relative machine precision
      mu=0.0d00
      a(1,2)=0.0d00
      m=n
   15 if(m.eq.0) go to 21
      i=m-1
      m1=i
      k=i
      if(dabs(a(k+1,2)).gt.eps) go to 16
      mu=0.0d00
      m=k
      go to 15
   16 i=i-1
      if(dabs(a(i+1,2)).le.eps.or.i.eq.0) go to 18
      k=i
      go to 16
   18 lambda=0.0d00
      if(dabs(a(m,1)-mu).lt.0.5d0*dabs(a(m,1)).or.m1.eq.k) lambda=a(m,1)
     1 +0.5d0*a(m1+1,2)
      mu=a(m,1)
      a(k,1)=a(k,1)-lambda
      beta=a(k+1,2)
c     transformation on the left
      do 19 j=k, m1
      a0=a(j,1)
      a1=a(j+1,1)-lambda
      b0=a(j+1,2)
      t=dsqrt(a0**2+beta**2)
      cosine=a0/t
      a(j,4)=cosine
      sine=beta/t
      a(j,5)= sine
      a(j,1)=cosine*a0+ sine*beta
      a(j+1,1)=- sine*b0+cosine*a1
      a(j+1,2)=cosine*b0+ sine*a1
      beta=a(j+2,2)
      a(j+2,2)=cosine*beta
   19 a(j+2,3)= sine*beta
      a(k,2)=0.0d00
      a(k+1,3)=0.0d00
c     tran sformation on the right
      do 20 j=k,m1
      sine=a(j,5)
      cosine=a(j,4)
      a0=a(j,1)
      b0=a(j+1,2)
      a(j,2)=a(j,2)*cosine+a(j+1,3)* sine
      a(j,1)=a0*cosine+b0* sine+lambda
      a(j+1,2)=-a0* sine+b0*cosine
      a(j+1,1)=a(j+1,1)*cosine
      do 20 i=1,n
      x0=x(i,j)
      x1=x(i,j+1)
      x(i,j)=x0*cosine+x1*sine
   20 x(i,j+1)=-x0*sine+x1*cosine
      a(m,1)=a(m,1)+lambda
      go to 15
   21 if(ind.ne.0) return
      do 120 m=1,n
  120 g(m,m)=a(m,1)
      if(iprint.eq.0) return
      do 170 i=1,n,7
      i6=min0(i+6,n)
      write(6,601) (g(j,j),j=i,i6)
      do 160 j=1,n
  160 write(6,602) j,(x (j,j9),j9=i,i6)
  170 continue
  601 format(1h0,8x,  7e16.6)
  602 format(7x,1h(,i2,1h),f12.6,6f16.6)
      return
      end


c==== diagonalization of real symmetric matrix =========================

      subroutine diag_dsym(mat,eig,nd,ntot)

c-----------------------------------------------------------------------
c this subroutine is the one for diagonalization of real symmetric
c matrix in double precision. the householder transformation and ql
c decomposition are used to diagonalize.
c
c mat  : input  <- matrix to be diagonalized
c        output -> eigenvector of matrix
c eig  : output -> eigenvalue of matrix
c nd   : input  <- dimension of matrix
c ntot : input  <- physical size of matrix and vector
c-----------------------------------------------------------------------
      integer nd,ntot
      double precision mat(ntot,ntot),eig(ntot)
c
c call subroutines
c
      double precision w(ntot)
      call tred2(mat,eig,w,nd,ntot) ! householder
      call tqli(eig,w,mat,nd,ntot)  ! ql method
      return
      end
c=======================================================================

c==== householder reduction ============================================

      subroutine tred2(a,d,e,n,np)

c-----------------------------------------------------------------------
      integer n,np
      double precision a(np,np),e(np),d(np)
c
c main part
c
      integer i,j,k,l
      double precision f,g,h,hh,scale
      do i=n,2,-1
        l=i-1
        h=0.d0
        scale=0.d0
        if(l.gt.1) then
          do k=1,l
            scale=scale+dabs(a(i,k))
          enddo
          if(scale.eq.0.d0) then
            e(i)=a(i,l)
          else
            do k=1,l
              a(i,k)=a(i,k)/scale
              h=h+a(i,k)**2
            enddo
            f=a(i,l)
            g=-dsign(dsqrt(h),f)
            e(i)=scale*g
            h=h-f*g
            a(i,l)=f-g
            f=0.d0
            do j=1,l
              a(j,i)=a(i,j)/h
              g=0.d0
              do k=1,j
                g=g+a(j,k)*a(i,k)
              enddo
              do k=j+1,l
                g=g+a(k,j)*a(i,k)
              enddo
              e(j)=g/h
              f=f+e(j)*a(i,j)
            enddo
            hh=f/(h+h)
            do j=1,l
              f=a(i,j)
              g=e(j)-hh*f
              e(j)=g
              do k=1,j
                a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
              enddo
            enddo
          endif
        else
          e(i)=a(i,l)
        endif
        d(i)=h
      enddo
      d(1)=0.d0
      e(1)=0.d0
      do i=1,n
        l=i-1
        if(d(i).ne.0.d0) then
          do j=1,l
            g=0.d0
            do k=1,l
              g=g+a(i,k)*a(k,j)
            enddo
            do k=1,l
              a(k,j)=a(k,j)-g*a(k,i)
            enddo
          enddo
        endif
        d(i)=a(i,i)
        a(i,i)=1.d0
        do j=1,l
          a(i,j)=0.d0
          a(j,i)=0.d0
        enddo
      enddo
      return
      end
c=======================================================================

c==== ql algorithm with implicit shifts ================================

      subroutine tqli(d,e,z,n,np)

c-----------------------------------------------------------------------
      integer n,np
      double precision e(np),d(np),z(np,np)
c
c main part
c
      integer i,iter,k,l,m
      double precision b,c,dd,f,g,p,r,s,pythag
      do i=2,n
        e(i-1)=e(i)
      enddo
      e(n)=0.d0
      do l=1,n
        iter=0
  1     do m=l,n-1
          dd=dabs(d(m))+dabs(d(m+1))
          if(dabs(e(m))+dd.eq.dd) goto 2
        enddo
        m=n
  2     if(m.ne.l) then
          if(iter.eq.30) goto 9999
          iter=iter+1
          g=(d(l+1)-d(l))/(2.d0*e(l))
          r=pythag(g,1.d0)
          g=d(m)-d(l)+e(l)/(g+dsign(r,g))
          s=1.d0
          c=1.d0
          p=0.d0
          do i=m-1,l,-1
            f=s*e(i)
            b=c*e(i)
            r=pythag(f,g)
            e(i+1)=r
            if(r.eq.0.d0) then
              d(i+1)=d(i+1)-p
              e(m)=0.d0
              goto 1
            endif
            s=f/r
            c=g/r
            g=d(i+1)-p
            r=(d(i)-g)*s+2.d0*c*b
            p=s*r
            d(i+1)=g+p
            g=c*r-b
            do k=1,n
              f=z(k,i+1)
              z(k,i+1)=s*z(k,i)+c*f
              z(k,i)=c*z(k,i)-s*f
            enddo
          enddo
          d(l)=d(l)-p
          e(l)=g
          e(m)=0.d0
          goto 1
        endif
      enddo
      return
9999  write(6,*) 'too many iterations in tqli!!'
      stop
      end
c=======================================================================

c=======================================================================

      function pythag(a,b)

c-----------------------------------------------------------------------
      double precision a,b,pythag
c
c main part
c
      double precision absa,absb
      absa=dabs(a)
      absb=dabs(b)
      if(absa.gt.absb) then
        pythag=absa*dsqrt(1.d0+(b/a)**2)
      else
        if(absb.eq.0.d0) then
          pythag=0.d0
        else
          pythag=absb*dsqrt(1.d0+(a/b)**2)
        endif
      endif
      return
      end
c=======================================================================

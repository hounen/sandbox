c==== diagonalization of complex symmertic matrix ======================

      subroutine diag_csym(mat,eig,nd,ntot)

c-----------------------------------------------------------------------
c this subroutine is the one for diagonalization of complex symmetric
c matrix in double precision. the householder transformation and ql
c decomposition are used to diagonalize.
c
c mat  : input  <- matrix to be diagonalize
c        output -> eigenvector of matrix
c eig  : output -> eigenvalue of matrix
c nd   : input  <- dimension of matrix
c ntot : input  <- physical size of matrix and vector
c-----------------------------------------------------------------------
      integer nd,ntot
      complex(kind(0d0)) mat(ntot,ntot),eig(ntot)
c
c call subroutines
c
      complex(kind(0d0)) w(ntot)
      call ctred2(mat,eig,w,nd,ntot) ! householder
      call ctqli(eig,w,mat,nd,ntot)  ! ql method
      return
      end
c=======================================================================

c==== householder reduction ============================================

      subroutine ctred2(a,d,e,n,np)

c-----------------------------------------------------------------------
      integer n,np
      complex(kind(0d0)) a(np,np),e(np),d(np)
c
c main part
c
      integer i,j,k,l
      complex(kind(0d0)) f,g,h,hh,scale
      do i=n,2,-1
        l=i-1
        h=0.d0
        scale=0.d0
        if(l.gt.1) then
          do k=1,l
            scale=scale+cdabs(a(i,k))
          enddo
          if(cdabs(scale).eq.0.d0) then
            e(i)=a(i,l)
          else
            do k=1,l
              a(i,k)=a(i,k)/scale
              h=h+a(i,k)**2
            enddo
            f=a(i,l)
            if(dble(f).ge.0.d0) then
              g=-cdsqrt(h)
            else
              g=cdsqrt(h)
            endif
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

      subroutine ctqli(d,e,z,n,np)

c-----------------------------------------------------------------------
      integer n,np
      complex(kind(0d0)) e(np),d(np),z(np,np)
c
c main part
c
      integer i,iter,k,l,m
      double precision dd
      complex(kind(0d0)) b,c,f,g,p,r,s,cpythag,one
      parameter(one=(1.d0,0.d0))
      do i=2,n
        e(i-1)=e(i)
      enddo
      e(n)=0.d0
      do l=1,n
        iter=0
  1     do m=l,n-1
          dd=cdabs(d(m))+cdabs(d(m+1))
          if(cdabs(e(m))+dd.eq.dd) goto 2
        enddo
        m=n
  2     if(m.ne.l) then
          if(iter.eq.1000) goto 9999
          iter=iter+1
          g=(d(l+1)-d(l))/(2.d0*e(l))
          r=cpythag(g,one)
          if(dble(r)*dble(g).ge.0.d0) then ! to be checked
            g=d(m)-d(l)+e(l)/(g+r)
          else
            g=d(m)-d(l)+e(l)/(g-r)
          endif
          s=1.d0
          c=1.d0
          p=0.d0
          do i=m-1,l,-1
            f=s*e(i)
            b=c*e(i)
            r=cpythag(f,g)
            e(i+1)=r
            if(cdabs(r).eq.0.d0) then
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
9999  write(6,*) 'too many iterations in ctqli!!'
      stop
      end
c=======================================================================

c=======================================================================

      function cpythag(a,b)

c-----------------------------------------------------------------------
      complex(kind(0d0)) a,b,cpythag
c
c main part
c
      double precision absa,absb
      absa=cdabs(a)
      absb=cdabs(b)
      if(absa.gt.absb) then
        cpythag=a*cdsqrt(1.d0+(b/a)**2)
      else
        if(absb.eq.0.d0) then
          cpythag=(0.d0,0.d0)
        else
          cpythag=b*cdsqrt(1.d0+(a/b)**2)
        endif
      endif
      return
      end
c=======================================================================

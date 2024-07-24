c==== set gaussian width ===============================================

      subroutine set_base(deg,bb)

c-----------------------------------------------------------------------
      include 'DIM_INC.f'
      zi=(0.d0,1.d0)
      th=deg/180.d0*pi
      zth2=cdexp(2.d0*th*zi)
      do i=1,nb
        za(i)=1.d0/(bb*db0*dgam**(i-1))**2/zth2
      enddo
      return
      end
c=======================================================================

c==== set factorial ====================================================

      subroutine set_fac

c-----------------------------------------------------------------------
      include 'DIM_INC.f'
      fac(0)=1.d0
      faclog(0)=0.d0
      do i=1,20
        fac(i)=fac(i-1)*i
      enddo

      faclog(0)=0.d0
      dfaclog(0)=0.d0
      do i=1,30
        xi=dble(i)
        faclog(i)=faclog(i-1)+dlog(xi)
        dfaclog(i)=dfaclog(i-1)+dlog(2.d0*xi+1.d0)
      enddo
      return
      end
c=======================================================================

c==== set matrix elements ==============================================

      subroutine set_mat

c-----------------------------------------------------------------------
      include 'DIM_INC.f'
      do i=1,nb
      do j=1,i
        zai=za(i)
        zaj=za(j)
        dnij=dnorm(ltot,zai,zaj)
        ztij=zkine(ltot,zai,zaj)
        zcij=zcent(ltot,zai,zaj)
        zclij=zcoul(ltot,zai,zaj)
        zcl0ij=zcoul0(ltot,zai,zaj)
        zpfij=zpseu(ltot,zai,zaj)
        dn(i,j)=dnij
        dn(j,i)=dn(i,j)
        dn0(i,j)=dnij
        dn0(j,i)=dn0(i,j)
        zh(i,j)=ztij+zcij+zclij+zpfij
        zh(j,i)=zh(i,j)
        zh0(i,j)=ztij+zcl0ij
        zh0(j,i)=zh0(i,j)
      enddo
      enddo
      return
      end
c=======================================================================

c==== norm =============================================================
      function dnorm(l,zai,zaj)
      include 'DIM_INC.f'
      zad=zai+zaj
      zpr=zai*zaj
      znorm=(2.d0*cdsqrt(zpr)/zad)**(l+1.5d0)
      dnorm=dble(znorm)
      return
      end
c=======================================================================

c==== kinetic ==========================================================
      function zkine(l,zai,zaj)
      include 'DIM_INC.f'
      zad=zai+zaj
      zpr=zai*zaj
      zrdm=5.d0*hbc*hbc/32.d0/dmass*(2*l+3.d0)*zpr/zad
      zkine=zrdm*(2.d0*cdsqrt(zpr)/zad)**(l+1.5d0)
      return
      end
c=======================================================================

c==== central ==========================================================
      function zcent(l,zai,zaj)
      include 'DIM_INC.f'
      v_c=-154.d0
      r_c=0.1102d0
      zad=zai+zaj+2.d0*r_c
      zpr=zai*zaj
      zcent=v_c*(2.d0*cdsqrt(zpr)/zad)**(l+1.5d0)
      return
      end

c==== coulomb ==========================================================
      function zcoul(l,zai,zaj)
      include 'DIM_INC.f'
      v_cl=16.d0*hbc*alpha*2.d0/dsqrt(pi)
      dkap=1.d0/0.4805d0
      zad=0.5d0*(zai+zaj)
      zpr=cdsqrt(zai*zaj)
      zco=1.d0/cdsqrt(1.d0+zad*dkap*dkap)
      zsum=(0.d0,0.d0)
      do ir=0,l
        facl=faclog(l)-faclog(ir)-faclog(l-ir)
        zsum=zsum+(-1.d0)**ir/(2.d0*ir+1.d0)*dexp(facl)
     &      *zco**(2*ir+1)*zpr**(l+1.5d0)/zad**(l+1)
      enddo
      zcoul=v_cl*zsum
      return
      end
c=======================================================================
      
c==== coulomb ==========================================================
      function zcoul0(l,zai,zaj)
      include 'DIM_INC.f'
      v_cl=16.d0*hbc*alpha*2.d0/dsqrt(pi)
      dkap=0.d0
      zad=0.5d0*(zai+zaj)
      zpr=cdsqrt(zai*zaj)
      zco=1.d0/cdsqrt(1.d0+zad*dkap*dkap)
      zsum=(0.d0,0.d0)
      do ir=0,l
        facl=faclog(l)-faclog(ir)-faclog(l-ir)
        zsum=zsum+(-1.d0)**ir/(2.d0*ir+1.d0)*dexp(facl)
     &      *zco**(2*ir+1)*zpr**(l+1.5d0)/zad**(l+1)
      enddo
      zcoul0=v_cl*zsum
      return
      end
c=======================================================================

c==== pseudo potential =================================================
      function zpseu(l,zai,zaj)
      include 'DIM_INC.f'
      dimension dnf(0:3,0:7,0:3)
      v_pf=1.d+6
      r_pf=1.d0/(0.9882d0)**2
      do nn=0,3
      do ll=0,7
      do kk=0,3
        ip=2*ll-nn+2*kk+3
        fcl=0.5d0*(faclog(nn)+dfaclog(ll+nn)-dfaclog(ll))
     &     -faclog(nn-kk)-faclog(kk)
        dnf(nn,ll,kk)
     &      =(-1.d0)**kk*dsqrt(2.d0**ip*r_pf**(ll+2*kk+1.5d0))
     &      *dexp(fcl)
      enddo
      enddo
      enddo
      zpseu=(0.d0,0.d0)

      itmp=8-l
      if(itmp.le.0) return
      if(itmp/2*2.eq.itmp) then
        nmax=itmp/2-1
      else
        nmax=itmp/2
      endif
      zsum=(0.d0,0.d0)
      do nn=0,nmax
        zsum1=(0.d0,0.d0)
        zsum2=(0.d0,0.d0)
        do kk=0,nn
          zsum1=zsum1
     &       +dnf(nn,l,kk)*zai**(0.5*l+0.75d0)
     &       /(zai+r_pf)**(l+kk+1.5d0)
          zsum2=zsum2
     &       +dnf(nn,l,kk)*zaj**(0.5*l+0.75d0)
     &       /(zaj+r_pf)**(l+kk+1.5d0)
        enddo
        zsum=zsum+zsum1*zsum2
      enddo
      zpseu=v_pf*zsum
      return
      end
c=======================================================================

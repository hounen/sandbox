      function znf(dl,za) 
      include 'li_inc.f'
      il=int(dl+0.01d0)!0.01d0はfunctionに読み込む時の誤差を考慮
      znf=2.d0*za**(dl+1.5d0)*2.d0**(dl+1.d0)/dsqrt(pi)/dfac(il)
      znf=cdsqrt(znf)
      return
      end
      
c norm
      function znorm(il1,il2,za1,za2)
      include 'li_inc.f'

      znorm=(0.d0,0.d0)
      if (il1.ne.il2) return

      dl1=dble(il1)
      dl2=dble(il2)

      zad=0.5d0*(za1+za2)
      zpr=za1*za2
      znorm=(zpr/zad**2.d0)**(0.5d0*(dl1+1.5d0))
      return
      end

c exp(-mu*r**2)

      function zinteg(dl1,dl2,za1,za2,n,dmu)
      include 'li_inc.f'
      zinteg=(0.d0,0.d0)
      if(n.ne.0) goto 1000
      zad=za1+za2+2.d0*dmu
      zpr=za1*za2
      zinteg=(2.d0*cdsqrt(zpr)/zad)**(0.5d0*(dl1+dl2+3.d0))
      return

1000  continue ! n is even.
      nn=int(dl1+dl2+0.01d0)+n+2
      nn=nn/2

      if(n/2*2.ne.n) goto 1001

      za=0.5d0*(za1+za2)+dmu
      zfac=znf(dl1,za1)*znf(dl2,za2)
      zinteg=zfac*dfac(nn-1)*sqrt(pi/za**(2*nn+1))/2.d0**(nn+1)
      return

1001  continue ! n is odd.
      zinteg=zfac*fac(nn)/2.d0/za**(nn+1)
      return
      end

c------wave function---------------------------------------------------
      function phi(rad,nz,zc,j,a0,gam,dl)
      include 'li_inc.f'
      zphi=(0.d0,0.d0)
       do k=1,nz
         zphi=zphi+zc(k,j)*rad**dl*dexp(-a0*gam**(k-1)*rad**2.d0/2.d0)
       enddo
         phi=(cdabs(zphi))**2.d0
      end

c Γ関数サブルーチン
      subroutine set_fac()
      include 'li_inc.f'
      fac(0)=1.d0!
      dfac(0)=1.d0!Γ(l+1.5)
      do i=1,20
        fac(i)=fac(i-1)*i
        dfac(i)=dfac(i-1)*(2*i+1)
      enddo
      return
      end

      

c hamiltonian      
      subroutine kine(zt,il1,il2,za1,za2)
      include 'li_inc.f'

      zt=(0.d0,0.d0)
      if (il1.ne.il2) return 

       dl1=dble(il1)
       dl2=dble(il2)

       dmh=hbc**2.d0/dmass
       zad=za1+za2
       zpr=za1*za2
       zt=dmh*(2.d0*dl1+3.d0)*zpr/zad*(2.d0*sqrt(zpr)/zad)**(dl1+1.5d0)

      return
      end
      
      subroutine cent(zc,il1,il2,is,it,za1,za2)
      include 'li_inc.f'
      dimension vc_nn(6,0:1,0:1),rc_nn(6,0:1,0:1)

      zc=(0.d0,0.d0)
      if (il1.ne.il2) return
       
      vc_nn(1,0,0)=  -5.3718485d0
      vc_nn(2,0,0)=  42.0351149d0
      vc_nn(3,0,0)=1313.1214374d0
      vc_nn(4,0,0)= 156.8939167d0
      vc_nn(5,0,0)=   1.6440785d0
      vc_nn(6,0,0)=   6.1188200d0
      rc_nn(1,0,0)=1.d0/0.1658676d0**2.d0
      rc_nn(2,0,0)=1.d0/0.2894831d0**2.d0
      rc_nn(3,0,0)=1.d0/0.5052249d0**2.d0
      rc_nn(4,0,0)=1.d0/0.8817517d0**2.d0
      rc_nn(5,0,0)=1.d0/1.5388910d0**2.d0
      rc_nn(6,0,0)=1.d0/2.6857737d0**2.d0

      vc_nn(1,1,0)=  91.4822846d0
      vc_nn(2,1,0)=2822.2369593d0
      vc_nn(3,1,0)=-467.7657278d0
      vc_nn(4,1,0)= -19.8229349d0
      vc_nn(5,1,0)= -16.3953104d0
      vc_nn(6,1,0)=  -3.5568691d0
      rc_nn(1,1,0)=1.d0/0.2844815d0**2.d0
      rc_nn(2,1,0)=1.d0/0.4299440d0**2.d0
      rc_nn(3,1,0)=1.d0/0.6497851d0**2.d0
      rc_nn(4,1,0)=1.d0/0.9820364d0**2.d0
      rc_nn(5,1,0)=1.d0/1.4841760d0**2.d0
      rc_nn(6,1,0)=1.d0/2.2430721d0**2.d0

      vc_nn(1,0,1)= 118.1762345d0
      vc_nn(2,0,1)=3754.5356911d0
      vc_nn(3,0,1)=-757.3370635d0
      vc_nn(4,0,1)= -68.1523604d0
      vc_nn(5,0,1)= -21.2313598d0
      vc_nn(6,0,1)=  -3.8928738d0
      rc_nn(1,0,1)=1.d0/0.2864738d0**2.d0
      rc_nn(2,0,1)=1.d0/0.4310967d0**2.d0
      rc_nn(3,0,1)=1.d0/0.6487308d0**2.d0
      rc_nn(4,0,1)=1.d0/0.9762350d0**2.d0
      rc_nn(5,0,1)=1.d0/1.4690757d0**2.d0
      rc_nn(6,0,1)=1.d0/2.2107213d0**2.d0

      vc_nn(1,1,1)= 133.3890061d0
      vc_nn(2,1,1)=1277.2251624d0
      vc_nn(3,1,1)=1481.3409438d0
      vc_nn(4,1,1)=-929.4184626d0
      vc_nn(5,1,1)= 474.6536455d0
      vc_nn(6,1,1)=-121.0755683d0
      rc_nn(1,1,1)=1.d0/0.3410784d0**2.d0
      rc_nn(2,1,1)=1.d0/0.4233829d0**2.d0
      rc_nn(3,1,1)=1.d0/0.5255481d0**2.d0
      rc_nn(4,1,1)=1.d0/0.6523664d0**2.d0
      rc_nn(5,1,1)=1.d0/0.8097869d0**2.d0
      rc_nn(6,1,1)=1.d0/1.0051941d0**2.d0

      dl1=dble(il1)
      dl2=dble(il2)

      do i=1,6
        zc=zc+vc_nn(i,is,it)*zinteg(dl1,dl2,za1,za2,0,rc_nn(i,is,it))
      enddo
      return
      end

      subroutine lsfrs(zcls,il1,il2,ij,is,it,za1,za2)
      include 'li_inc.f'
      dimension vls_nn(6,0:1,0:1),rls_nn(6,0:1,0:1)

      zcls=(0.d0,0.d0)

      if (il1.ne.il2) return
      if (is.eq.0) return
      if (il1+il2.eq.0) return

        vls_nn(1,1,0)=  4.7149736d0
        vls_nn(2,1,0)=-92.5679591d0
        vls_nn(3,1,0)=301.5818757d0
        vls_nn(4,1,0)=  1.7134339d0
        vls_nn(5,1,0)= -0.8889886d0
        vls_nn(6,1,0)=  0.0930875d0
        rls_nn(1,1,0)=1.d0/0.1975533d0**2.d0
        rls_nn(2,1,0)=1.d0/0.3369489d0**2.d0
        rls_nn(3,1,0)=1.d0/0.5747036d0**2.d0
        rls_nn(4,1,0)=1.d0/0.9802204d0**2.d0
        rls_nn(5,1,0)=1.d0/1.6718741d0**2.d0
        rls_nn(6,1,0)=1.d0/2.8515658d0**2.d0

        vls_nn(1,1,1)= -20.6687445d0
        vls_nn(2,1,1)= 406.4188719d0
        vls_nn(3,1,1)=-902.2225521d0
        vls_nn(4,1,1)= -18.4134186d0
        vls_nn(5,1,1)=   1.3631363d0
        vls_nn(6,1,1)=  -0.1837394d0
        rls_nn(1,1,1)=1.d0/0.2016024d0**2.d0
        rls_nn(2,1,1)=1.d0/0.3474255d0**2.d0
        rls_nn(3,1,1)=1.d0/0.5987251d0**2.d0
        rls_nn(4,1,1)=1.d0/1.0317948d0**2.d0
        rls_nn(5,1,1)=1.d0/1.7781124d0**2.d0
        rls_nn(6,1,1)=1.d0/3.0642562d0**2.d0

      dl1=dble(il1)
      dl2=dble(il2)

      zc_tot=(0.d0,0.d0)
      do i=1,6
          zc_tot=zc_tot+vls_nn(i,is,it)
     &           *zinteg(dl1,dl2,za1,za2,0,rls_nn(i,is,it))
      enddo

      dj=dble(ij)
      ds=dble(is)

      dls=dj*(dj+1.d0)-dl1*(dl1+1.d0)-ds*(ds+1.d0)
      zcls=0.5d0*dls*zc_tot
      return
      end

      subroutine tnfrs(ztn,il1,il2,ij,is,it,za1,za2)
      include 'li_inc.f'
      dimension vtn_nn(8,0:1,0:1),rtn_nn(8,0:1,0:1)

      ztn=(0.d0,0.d0)
      if (is.eq.0) return
      if (il1+il2.lt.2) return
      if (iabs(il1-il2).gt.2) return

        vtn_nn(1,1,0)=-8031.8094845d0
        vtn_nn(2,1,0)=-3397.2153441d0
        vtn_nn(3,1,0)=-2180.2316049d0
        vtn_nn(4,1,0)= -420.7351591d0
        vtn_nn(5,1,0)= -214.2512043d0
        vtn_nn(6,1,0)=  -17.9748552d0
        vtn_nn(7,1,0)=   -1.1514334d0
        vtn_nn(8,1,0)=   -0.0483353d0
        rtn_nn(1,1,0)=1.d0/0.1057489d0**2.d0
        rtn_nn(2,1,0)=1.d0/0.1728539d0**2.d0
        rtn_nn(3,1,0)=1.d0/0.2825416d0**2.d0
        rtn_nn(4,1,0)=1.d0/0.4618338d0**2.d0
        rtn_nn(5,1,0)=1.d0/0.7548994d0**2.d0
        rtn_nn(6,1,0)=1.d0/1.2339352d0**2.d0
        rtn_nn(7,1,0)=1.d0/2.0169525d0**2.d0
        rtn_nn(8,1,0)=1.d0/3.2968485d0**2.d0

        vtn_nn(1,1,1)=-486.2603768d0
        vtn_nn(2,1,1)= 318.9744930d0
        vtn_nn(3,1,1)= 661.1064012d0
        vtn_nn(4,1,1)=  26.1396422d0
        vtn_nn(5,1,1)=   3.8691045d0
        vtn_nn(6,1,1)=   0.2348827d0
        vtn_nn(7,1,1)=0.d0
        vtn_nn(8,1,1)=0.d0
        rtn_nn(1,1,1)=1.d0/0.1678926d0**2.d0
        rtn_nn(2,1,1)=1.d0/0.2829651d0**2.d0
        rtn_nn(3,1,1)=1.d0/0.4769077d0**2.d0
        rtn_nn(4,1,1)=1.d0/0.8037774d0**2.d0
        rtn_nn(5,1,1)=1.d0/1.3546817d0**2.d0
        rtn_nn(6,1,1)=1.d0/2.2831723d0**2.d0
        rtn_nn(7,1,1)=1.d0
        rtn_nn(8,1,1)=1.d0

      dl1=dble(il1)
      dl2=dble(il2)
      dj=dble(ij)
      ds=dble(is)

      zc_tot=(0.d0,0.d0)
      do i=1,8
        zc_tot=zc_tot
     &   +vtn_nn(i,is,it)*zinteg(dl1,dl2,za1,za2,2,rtn_nn(i,is,it))
      enddo

      ztn=(-1.d0)**(dl2+ds+dj)*2.d0*dsqrt(30*(2.d0*dl2+1.d0))
     &     *cgz_i(il2,2,il1)*sixj_d(dj,ds,dl1,2.d0,dl2,ds)*zc_tot 
      return
      end

      subroutine coulomb(zclb,il1,il2,za1,za2)
      include 'li_inc.f'

      zclb=(0.d0,0.d0)

      if (il1.ne.il2) return

      dkap=0.d0
      zad=0.5d0*(za1+za2)
      zpr=cdsqrt(za1*za2)
      zco=1.d0/cdsqrt(1.d0+zad*dkap*dkap)
      zsum=(0.d0,0.d0)
      do ir=0,il1
        facl=fac(il1)/fac(ir)/fac(il1-ir)
        zsum=zsum+(-1.d0)**ir/(2.d0*ir+1.d0)*facl
     &      *zco**(2*ir+1)*zpr**(il1+1.5d0)/zad**(il1+1)
      enddo
      zclb=zsum*hbc*al*2.d0/dsqrt(pi)
      return
      end

c error functionによってcoulomb作用に原子核の拡がりを考慮      

      subroutine pseudo(zpf,dl,za1,za2)
      include 'li_inc.f'
      zapf=0.8d0/1.4d0**2
      zpf=1.d6*znorm(dl,0.d0,za1,zapf)*znorm(0.d0,dl,zapf,za2)
      return
      end 

!c T-matrix part1      
!      subroutine cent_f1(tval,dk,dl)!中心力
!      include 'li_inc.f'
!      dimension vc(2),rc(2)
!      vc(1)=-96.3d0
!      rc(1)= 0.36d0
!      vc(2)= 77.0d0
!      rc(2)=  0.9d0 
!      tval=0.d0
!      do i=1,2
!        x=dk**2/2.d0/rc(i)
!c       tval=tval+vc(i)*pi**2.d0/2.d0/rc(i)/dk
!c    &             *dexp(-x)*bessel_fuc(x,dl+0.5d0)
!      tval=tval
!     &    +vc(i)/8.d0/pi/rc(i)/dk
!     &    *dexp(-x)*bessel_fuc(x,dl+0.5d0)
!      enddo
!      return
!      end
!      
!
!      subroutine cenl_f1(tval,dk,dl)!角運動量依存の中心力
!      include 'li_inc.f'
!      dimension vc(3),rc(3)
!      vc(1)= 34.0d0
!      rc(1)= 0.20d0
!      vc(2)=-85.0d0
!      rc(2)= 0.53d0
!      vc(3)= 51.0d0
!      rc(3)= 2.50d0
!      tval=0.d0
!      do i=1,3
!        x=dk**2/2.d0/rc(i)
!      tval=tval
!     &   +(-1)**dl*vc(i)*pi**2.d0/2.d0/rc(i)/dk
!     &   *dexp(-x)*bessel_fuc(x,dl+0.5d0)
!      enddo
!      return
!      end
!
!      subroutine lsfrs_f1(tval,dk,dl,dj)!ls力
!      include 'li_inc.f'
!      dimension vc(2),rc(2)
!      va=    -8.4d0
!      ra=    0.52d0
!      vc(1)= -10.d0
!      rc(1)=0.396d0
!      vc(2)= 10.0d0
!      rc(2)= 2.20d0
!      tval=0.d0
!      do i=1,2
!        x=dk**2/2.d0/rc(i)
!        tval=tval+(1.d0+0.3d0*(-1.d0)**(dl-1.d0))
!     &     *vc(i)*pi**2.d0/2.d0/rc(i)/dk*exp(-x)*bessel_fuc(x,dl+0.5d0)
!      enddo
!      x=dk**2/2.d0/ra
!      dls=dj*(dj+1)-dl*(dl+1)-0.75d0
!      tval=dls*(va*pi**2.d0/2.d0/ra/dk*exp(-x)
!     &         *bessel_fuc(x,dl+0.5d0)+tval)
!      return
!      end
!      
!cT-matrix part2
!      subroutine centmat(zmatc,dk,za,dl)!中心力
!      include 'li_inc.f'
!      dimension vc(2),rc(2)
!      vc(1)=-96.3d0
!      rc(1)= 0.36d0
!      vc(2)= 77.0d0
!      rc(2)=  0.9d0 
!      zmatc=0.d0
!      do i=1,2
!        zad=za+2.d0*rc(i)
!c        zapr=sqrt(2.d0/dfac(dl))*(sqrt(za)/zad)**(dl+1.5d0)
!        zapr=sqrt(2.d0**(dl+2)*sqrt(pi)/dfac(dl))
!     &      *(sqrt(za)/zad)**(dl+1.5d0)
!        zaexp=exp(-0.5d0*dk**2.d0/zad)
!        zmatc=zmatc+vc(i)*dk**dl*zapr*zaexp
!      enddo
!      return
!      end
!
!      subroutine cenlmat(zmatcl,dk,za,dl)!dl依存中心力
!      include 'li_inc.f'
!      dimension vc(3),rc(3)
!      vc(1)= 34.0d0
!      rc(1)= 0.20d0
!      vc(2)=-85.0d0
!      rc(2)= 0.53d0
!      vc(3)= 51.0d0
!      rc(3)= 2.50d0
!      zmatcl=0.d0
!      do i=1,3
!        zad=za+2.d0*rc(i)
!        zapr=sqrt(2.d0/dfac(dl))*(sqrt(za)/zad)**(dl+1.5d0)
!        zaexp=exp(-0.5d0*dk**2.d0/zad)
!        zmatcl=zmatcl+(-1.d0)**dl*vc(i)*(2.d0*pi)**1.5d0*
!     &                 dk**dl*zapr*zaexp
!      enddo
!      return
!      end
!      
!      subroutine lsmat(zmatls,dk,za,dl,dj)!ls力
!      include 'li_inc.f'
!      dimension vc(2),rc(2)
!      va=    -8.4d0
!      ra=    0.52d0
!      vc(1)= -10.d0
!      rc(1)=0.396d0
!      vc(2)= 10.0d0
!      rc(2)= 2.20d0
!      zmatls=0.d0
!      do i=1,2
!        zad=za+2.d0*rc(i)
!        zapr=sqrt(2.d0/dfac(dl))*(sqrt(za)/zad)**(dl+1.5d0)
!        zaexp=exp(-0.5d0*dk**2.d0/zad)
!        zmatls=zmatls+(1.d0+0.3d0*(-1.d0)**(dl-1.d0))
!     &       *vc(i)*(2.d0*pi)**1.5d0*zapr*zaexp
!      enddo
!      zad=za+2.d0*ra
!      zapr=sqrt(2.d0/dfac(dl))*(sqrt(za)/zad)**(dl+1.5d0)
!      zaexp=exp(-0.5d0*dk**2.d0/zad)
!      dls=dj*(dj+1)-dl*(dl+1)-0.75d0
!      zmatls=dls*(va*(2.d0*pi)**1.5d0*zapr*zaexp+zmatls)
!      return
!      end

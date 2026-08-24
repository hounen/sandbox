!     $Id: frdm.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                         !/*fr          1*/
      function frdm (ia,iz)                                                     !/*fr          2*/
!***********************************************************************        !/*fr          3*/
!     Returns the mass excess using the Finite-Range Droplet Model              !/*fr          4*/
!     for a SPHERICAL nucleus                                                   !/*fr          5*/
!     Ref: Moller,Nix,Myers, and Swiatecki, Atomic Data and Nuclear             !/*fr          6*/
!          Data Tables, Vol. 59, No. 2, March 1995.                             !/*fr          7*/
!***********************************************************************        !/*fr          8*/
#include <lcs/double.h>                                                         /*!fr          9*/
!***********************************************************************        !/*fr         10*/
! Parameters                                                                    !/*fr         11*/
!                                                                               !/*fr         12*/
! Pi                                                                            !/*fr         13*/
      parameter (pi=3.141592653589793238462643383279d0)                         !/*fr         14*/
! Hydrogen-atom mass excess M(H) - MeV                                          !/*fr         15*/
      parameter (xmh=7.289034d0)                                                !/*fr         16*/
! Neutron mass excess M(n) - MeV                                                !/*fr         17*/
      parameter (xmn=8.071431d0)                                                !/*fr         18*/
! Electronic charge squared e**2 - MeV-fm                                       !/*fr         19*/
      parameter (elc2=1.4399764d0)                                              !/*fr         20*/
! Electronic binding constant a(el) - MeV                                       !/*fr         21*/
      parameter (ael=1.433d-05)                                                 !/*fr         22*/
! Nuclear compressibility constant K - MeV                                      !/*fr         23*/
      parameter (xk=240.d0)                                                     !/*fr         24*/
! Proton RMS radius r(p) - fm                                                   !/*fr         25*/
      parameter (rp=0.80d0)                                                     !/*fr         26*/
! Nuclear radius constant r(0) - fm                                             !/*fr         27*/
      parameter (r0=1.16d0)                                                     !/*fr         28*/
! Range of Yukawa+exponential potential a - fm                                  !/*fr         29*/
      parameter (aa=0.68d0)                                                     !/*fr         30*/
! Range of Yukawa function for charge distribution a(den) - fm                  !/*fr         31*/
      parameter (aden=0.70d0)                                                   !/*fr         32*/
! Ave pairing-gap constant - r(mac) MeV                                         !/*fr         33*/
      parameter (rmac=4.80d0)                                                   !/*fr         34*/
! n-p interaction constant h - MeV                                              !/*fr         35*/
      parameter (h=6.6d0)                                                       !/*fr         36*/
! Wigner constant W - MeV                                                       !/*fr         37*/
      parameter (w=30.d0)                                                       !/*fr         38*/
! Symmetry energy comstant J - MeV                                              !/*fr         39*/
      parameter (xj=32.73d0)                                                    !/*fr         40*/
! Density-symmetry constant L - MeV                                             !/*fr         41*/
!     parameter (xl=0.d0)                                                       !/*fr         42*/
! Volume energy constant - a(1) MeV                                             !/*fr         43*/
      parameter (a1=16.247d0)                                                   !/*fr         44*/
! Surface energy constant a(2) - MeV                                            !/*fr         45*/
      parameter (a2=22.92d0)                                                    !/*fr         46*/
! Curvature energy constant a(3) - MeV                                          !/*fr         47*/
!     parameter (a3=0.d0)                                                       !/*fr         48*/
! Effective surface stiffness constant Q - MeV                                  !/*fr         49*/
      parameter (q=29.21d0)                                                     !/*fr         50*/
! A0 constant - a(0) MeV                                                        !/*fr         51*/
!     parameter (a0=0.d0)                                                       !/*fr         52*/
! Charge asymmetry constant c(a) - MeV                                          !/*fr         53*/
      parameter (ca=0.436d0)                                                    !/*fr         54*/
! Preexponential compressibility-term constant C - MeV                          !/*fr         55*/
      parameter (c=60.d0)                                                       !/*fr         56*/
! Exponential compressibility-term range constant                               !/*fr         57*/
      parameter (gamma=0.831d0)                                                 !/*fr         58*/
! Shape-dependent factors SPHERICAL nuclei - B(s),B(v),B(w),B(k),B(r)           !/*fr         59*/
      parameter (bs=1.d0, bv=1.d0, bw=1.d0, bk=1.d0, br=1.d0)                   !/*fr         60*/
!***********************************************************************        !/*fr         61*/
! Derivative parameters                                                         !/*fr         62*/
!                                                                               !/*fr         63*/
      parameter (c1=0.6d0*elc2/r0)                                              !/*fr         64*/
      parameter (c2=(1.d0/xj+18.d0/xk)*c1**2/336.d0)                            !/*fr         65*/
!     parameter (c4=1.25d0*(3.d0/(2.d0*pi))**(2.d0/3.d0)*c1)                    !/*fr         66*/
      parameter (c4=1.25d0*0.6108870d0*c1)                                      !/*fr         67*/
      parameter (c5=c1**2/(64.d0*q))                                            !/*fr         68*/
      parameter (f0=-0.125d0*(145.d0/48.d0)*rp**2*elc2/r0**3)                   !/*fr         69*/
#include <lcs/phprm0.h>                                                         /*!fr         70*/
#include <lcs/wapcom.h>                                                         /*!fr         71*/
      a=ia                                                                      !/*fr         72*/
      z=iz                                                                      !/*fr         73*/
      in=ia-iz                                                                  !/*fr         74*/
      ath=third(ia)                                                             !/*fr         75*/
      zth=third(iz)                                                             !/*fr         76*/
      xi=(a-2.d0*z)/a                                                           !/*fr         77*/
! B(n) SPHERICAL                                                                !/*fr         78*/
      x0=r0*ath/aa                                                              !/*fr         79*/
      txexp=exp(-2.d0*x0)                                                       !/*fr         80*/
      b1=1.d0-3.d0/x0**2+(1.d0+x0)*(2.d0+3.d0/x0+3.d0/x0**2)*txexp              !/*fr         81*/
      b2=1.d0-(1.d0+2.d0*x0-x0**2)*txexp                                        !/*fr         82*/
      y0=r0*ath/aden                                                            !/*fr         83*/
      tyexp=exp(-2.d0*y0)                                                       !/*fr         84*/
      b3=1.d0-5.d0*(1.d0-1.875d0/y0+2.625d0/y0**3-0.75d0*(1.d0+4.5d0/y0+        !/*fr         85*/
     & 7.d0/y0**2+3.5d0/y0**3)*tyexp)/y0**2                                     !/*fr         86*/
      b4=1.d0+5.d0*(-3.d0/y0**2+7.5d0/y0**3-15.75d0/y0**5+0.75d0*(2.d0          !/*fr         87*/
     & /y0+12.d0/y0**2+32.d0/y0**3+42.d0/y0**4+21.d0/y0**5)*tyexp)              !/*fr         88*/
!                                                                               !/*fr         89*/
      del=(xi+0.1875d0*c1*z*bv*bs/(q*b1*ath**2))/(1.d0+2.25d0*xj*bs**2/         !/*fr         90*/
     & (q*b1*ath))                                                              !/*fr         91*/
      eps=(c*exp(-gamma*ath)-2.d0*a2*b2/ath+c1*b4*z**2/(a*ath))/xk              !/*fr         92*/
!                                                                               !/*fr         93*/
      emac=xmh*z+xmn*(a-z)+(-a1+xj*del**2-0.5d0*xk*eps**2)*a+(a2*b1+2.25        !/*fr         94*/
     & d0*(xj*del*bs)**2/(q*b1))*ath**2+c1*b3*z**2/ath-c2*br*z**2*ath-c4        !/*fr         95*/
     & *z*zth/ath-c5*bw*bs*z**2/b1+f0*z**2/a-ca*(a*xi)+w*abs(xi)                !/*fr         96*/
      emac=emac-elbnde(iz)+delp(iz)+deln(in)                                    !/*fr         97*/
      modz=mod(iz,2)                                                            !/*fr         98*/
      modn=mod(in,2)                                                            !/*fr         99*/
! Del(n)                                                                        !/*fr        100*/
!     if (modn.eq.1) emac=emac+rmac*bs/exp(log(a-z)/3.d0)                       !/*fr        101*/
! Del(p)                                                                        !/*fr        102*/
!     if (modz.eq.1) emac=emac+rmac*bs/zth                                      !/*fr        103*/
! del(np)                                                                       !/*fr        104*/
      if (modn.eq.1.and.modz.eq.1) then                                         !/*fr        105*/
        emac=emac-h/(bs*ath**2)                                                 !/*fr        106*/
        if (iz.eq.in) emac=emac+w/a                                             !/*fr        107*/
      endif                                                                     !/*fr        108*/
!                                                                               !/*fr        109*/
      frdm=emac                                                                 !/*fr        110*/
      return                                                                    !/*fr        111*/
      end                                                                       !/*fr        112*/

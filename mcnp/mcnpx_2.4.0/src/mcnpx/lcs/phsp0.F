!     $Id: phsp0.F,v 1.5 2002/03/27 18:15:15 gwm Exp $                        !/*ph          1*/
      subroutine phsp0 (n,cp,xmp,kx,ky,kz,kq)                                   !/*ph          2*/
#include <lcs/double.h>                                                         /*!ph          3*/
!     ==================================================================        !/*ph          4*/
!     phsp0 calculates random momentum of "n" outgoing                          !/*ph          5*/
!     particles of masses xmp(i),i=1,n, conserving                              !/*ph          6*/
!     the initial 4-momentum cp.(= (0,0,0,e) in "phsp0")                        !/*ph          7*/
!     the outgoing momenta are returned in                                      !/*ph          8*/
!     kx,ky,kz(i),i=1,n.                                                        !/*ph          9*/
!     irand is the seed for the random number generator                         !/*ph         10*/
!     all units are in mev (mev/c).                                             !/*ph         11*/
!                                                                               !/*ph         12*/
! PHSP0  calls   SFLRAF  FLTRNF  SQRT  COS                                      !/*ph         13*/
! called by PHSBST                                                              !/*ph         14*/
!     ==================================================================        !/*ph         15*/
!                                                                               !/*ph         16*/
#ifdef CHEAP                                                                    /*!ph         17*/
      double precision kx, ky, kz, kq, kb                                       !/*ph         18*/
#else                                                                           /*!ph         19*/
      real kx, ky, kz, kq, kb                                                   !/*ph         20*/
#endif                                                                          /*!ph         21*/
      dimension cp(4), xmp(1), kx(1), ky(1), kz(1), kq(1)                       !/*ph         22*/
!                                                                               !/*ph         23*/
      pi=dp4*atan(dp1)                                                          !/*ph         24*/
!                                                                               !/*ph         25*/
!             calculate cos(theta(i))                                           !/*ph         26*/
      do 10 i=1,n                                                               !/*ph         27*/
      kx(i)=dp2*rang()-dp1                                                      !/*ph         28*/
   10 continue                                                                  !/*ph         29*/
!              calculate phi(i)                                                 !/*ph         30*/
      do 20 i=1,n                                                               !/*ph         31*/
      ky(i)=rang()*dp2*pi                                                       !/*ph         32*/
   20 continue                                                                  !/*ph         33*/
!           calculate r(i)  inside the unit sphere                              !/*ph         34*/
      do 30 i=1,n                                                               !/*ph         35*/
!               kz(i)=ranf(irand) ** dpth                                       !/*ph         36*/
!               max(r1,r2,r3) == ri ** (1/3) , but faster, inline               !/*ph         37*/
      r1=rang()                                                                 !/*ph         38*/
      r2=rang()                                                                 !/*ph         39*/
      r3=rang()                                                                 !/*ph         40*/
      kz(i)=max(r1,r2,r3)                                                       !/*ph         41*/
   30 continue                                                                  !/*ph         42*/
!           convert from polar to cartesian coordinates                         !/*ph         43*/
      tz=dp0                                                                    !/*ph         44*/
      ty=dp0                                                                    !/*ph         45*/
      tx=dp0                                                                    !/*ph         46*/
      do 40 i=1,n                                                               !/*ph         47*/
      rkz=kz(i)*kx(i)                                                           !/*ph         48*/
      sinth=sqrt(dp1-kx(i)**2)                                                  !/*ph         49*/
      rky=kz(i)*sinth*sin(ky(i))                                                !/*ph         50*/
      rkx=kz(i)*sinth*cos(ky(i))                                                !/*ph         51*/
      kz(i)=rkz                                                                 !/*ph         52*/
      ky(i)=rky                                                                 !/*ph         53*/
      kx(i)=rkx                                                                 !/*ph         54*/
!           prepare to zero the ensemble in space                               !/*ph         55*/
      tx=tx+rkx                                                                 !/*ph         56*/
      ty=ty+rky                                                                 !/*ph         57*/
      tz=tz+rkz                                                                 !/*ph         58*/
   40 continue                                                                  !/*ph         59*/
!                                                                               !/*ph         60*/
      rmul=real(n)                                                              !/*ph         61*/
      tx=tx/rmul                                                                !/*ph         62*/
      ty=ty/rmul                                                                !/*ph         63*/
      tz=tz/rmul                                                                !/*ph         64*/
      do 50 i=1,n                                                               !/*ph         65*/
      kx(i)=kx(i)-tx                                                            !/*ph         66*/
      ky(i)=ky(i)-ty                                                            !/*ph         67*/
      kz(i)=kz(i)-tz                                                            !/*ph         68*/
   50 continue                                                                  !/*ph         69*/
!                                                                               !/*ph         70*/
!                 pseudo-momenta now have zero average in all directions        !/*ph         71*/
!                                                                               !/*ph         72*/
!          setup to do newton method for proportionality constant               !/*ph         73*/
      kb=dp0                                                                    !/*ph         74*/
      do 60 i=1,n                                                               !/*ph         75*/
      kq(i)=kx(i)**2+ky(i)**2+kz(i)**2                                          !/*ph         76*/
      kb=kb+sqrt(kq(i))                                                         !/*ph         77*/
   60 continue                                                                  !/*ph         78*/
!                                                                               !/*ph         79*/
!           use ultra-relativisitic limit to get starting value (i.e.           !/*ph         80*/
!           largest possible c1)   sum((k(i)**2)**.5) = w0 = cp(4)              !/*ph         81*/
!                                                                               !/*ph         82*/
      c1=cp(4)/kb                                                               !/*ph         83*/
   70 rnum=dp0                                                                  !/*ph         84*/
      dnom=dp0                                                                  !/*ph         85*/
      c0=c1                                                                     !/*ph         86*/
      do 80 i=1,n                                                               !/*ph         87*/
      amq=xmp(i)**2                                                             !/*ph         88*/
      term=sqrt(amq+c0*c0*kq(i))                                                !/*ph         89*/
      rnum=rnum+term                                                            !/*ph         90*/
      dnom=dnom+c0*kq(i)/term                                                   !/*ph         91*/
   80 continue                                                                  !/*ph         92*/
      c1=c0-(rnum-cp(4))/dnom                                                   !/*ph         93*/
      rele=abs((c1-c0)/c0)                                                      !/*ph         94*/
      if (rele.gt.1.d-5) go to 70                                               !/*ph         95*/
!                                                                               !/*ph         96*/
!          now scale the zero-averaged random numbers into [mev/c]              !/*ph         97*/
      do 90 i=1,n                                                               !/*ph         98*/
      kx(i)=c1*kx(i)                                                            !/*ph         99*/
      ky(i)=c1*ky(i)                                                            !/*ph        100*/
      kz(i)=c1*kz(i)                                                            !/*ph        101*/
   90 continue                                                                  !/*ph        102*/
!                                                                               !/*ph        103*/
      return                                                                    !/*ph        104*/
      end                                                                       !/*ph        105*/

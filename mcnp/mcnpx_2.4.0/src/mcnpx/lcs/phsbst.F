!     $Id: phsbst.F,v 1.4 2002/03/27 18:15:14 gwm Exp $                       !/*ph          1*/
      subroutine phsbst (n,cp,xmp,kx,ky,kz,kq)                                  !/*ph          2*/
#include <lcs/double.h>                                                         /*!ph          3*/
!     ==================================================================        !/*ph          4*/
!     phsbst boosts 4-vector cp into the                                        !/*ph          5*/
!     center-of-mass frame, then sprays momenta with                            !/*ph          6*/
!     masses "xmp(i)" into pure phase-space, and then                           !/*ph          7*/
!     unboosts each of the four-vectors, "k(i)"                                 !/*ph          8*/
!                                                                               !/*ph          9*/
! PHSBST calls   SQRT  PHSP0  SQRT                                              !/*ph         10*/
! called by GAMMA                                                               !/*ph         11*/
!     ==================================================================        !/*ph         12*/
!                                                                               !/*ph         13*/
#ifdef CHEAP                                                                    /*!ph         14*/
      double precision kx, ky, kz                                               !/*ph         15*/
#else                                                                           /*!ph         16*/
      real kx, ky, kz, kq                                                       !/*ph         17*/
#endif                                                                          /*!ph         18*/
      dimension kx(1), ky(1), kz(1), kq(1), xmp(1), cp(4), pc(4)                !/*ph         19*/
!                                                                               !/*ph         20*/
!             calculate "beta"-vector                                           !/*ph         21*/
!                                                                               !/*ph         22*/
      b1=cp(1)/cp(4)                                                            !/*ph         23*/
      e1=sqrt(cp(4)**2-cp(1)**2)                                                !/*ph         24*/
      b2=cp(2)/e1                                                               !/*ph         25*/
      e2=sqrt(e1**2-cp(2)**2)                                                   !/*ph         26*/
      b3=cp(3)/e2                                                               !/*ph         27*/
      e3=sqrt(e2**2-cp(3)**2)                                                   !/*ph         28*/
      g1=dp1/sqrt(dp1-b1**2)                                                    !/*ph         29*/
      g2=dp1/sqrt(dp1-b2**2)                                                    !/*ph         30*/
      g3=dp1/sqrt(dp1-b3**2)                                                    !/*ph         31*/
!                                                                               !/*ph         32*/
!        set 4-vector the (0,0,0,e3) for in cm, then call "phsp0"               !/*ph         33*/
!                                                                               !/*ph         34*/
      pc(1)=dp0                                                                 !/*ph         35*/
      pc(2)=dp0                                                                 !/*ph         36*/
      pc(3)=dp0                                                                 !/*ph         37*/
      pc(4)=e3                                                                  !/*ph         38*/
!                                                                               !/*ph         39*/
!   check that output masses are ok                                             !/*ph         40*/
!                                                                               !/*ph         41*/
      rmtot=dp0                                                                 !/*ph         42*/
      do 10 i=1,n                                                               !/*ph         43*/
      rmtot=rmtot+xmp(i)                                                        !/*ph         44*/
   10 continue                                                                  !/*ph         45*/
!     if (rmtot .gt. e3) pause                                                  !/*ph         46*/
!                                                                               !/*ph         47*/
      call phsp0 (n,pc,xmp,kx,ky,kz,kq)                                         !/*ph         48*/
!                                                                               !/*ph         49*/
!            now we must boost all these back again                             !/*ph         50*/
!                                                                               !/*ph         51*/
      do 20 i=1,n                                                               !/*ph         52*/
      em3=sqrt(xmp(i)**2+kx(i)**2+ky(i)**2+kz(i)**2)                            !/*ph         53*/
      em2=g3*(em3+b3*kz(i))                                                     !/*ph         54*/
      kz(i)=g3*(kz(i)+b3*em3)                                                   !/*ph         55*/
      em1=g2*(em2+b2*ky(i))                                                     !/*ph         56*/
      ky(i)=g2*(ky(i)+b2*em2)                                                   !/*ph         57*/
      em0=g1*(em1+b1*kx(i))                                                     !/*ph         58*/
      kx(i)=g1*(kx(i)+b1*em1)                                                   !/*ph         59*/
   20 continue                                                                  !/*ph         60*/
!                                                                               !/*ph         61*/
      return                                                                    !/*ph         62*/
      end                                                                       !/*ph         63*/

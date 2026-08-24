!     $Id: panglk.F,v 1.5 2002/03/27 18:15:13 gwm Exp $                       !/*pa          1*/
      subroutine panglk (plab,ws4,wsp4,ans)                                     !/*pa          2*/
#include <lcs/double.h>                                                         /*!pa          3*/
!     ==================================================================        !/*pa          4*/
!                                                                               !/*pa          5*/
! PANGLK calls   SQRT  EXP  FLTRNF  ALOG                                        !/*pa          6*/
! called by GAMMA                                                               !/*pa          7*/
!     ==================================================================        !/*pa          8*/
!                                                                               !/*pa          9*/
#include <lcs/vegas.h>                                                          /*!pa         10*/
!     plab enters as the lab momentum [mev/c]                                   !/*pa         11*/
!     ecm = m1**2 + m2**2 + 2*m2*sqrt(m1**2+plab**2)                            !/*pa         12*/
!     rkcm = p1*m2/ecm  = cm momentum  [gev/c]                                  !/*pa         13*/
!      t = - ( 2*k*sin(thetacm3/2) )**2 = 4-momentum transfer  [mev/c]          !/*pa         14*/
!                                                                               !/*pa         15*/
      p1eff=plab                                                                !/*pa         16*/
      e1=sqrt(p1eff**2+ws4**2)                                                  !/*pa         17*/
      ecm=sqrt(ws4**2+wsp4**2+dp2*wsp4*e1)                                      !/*pa         18*/
!           convert plab from mev/c to gev/c                                    !/*pa         19*/
      p1eff=plab*1.d-3                                                          !/*pa         20*/
      rkcm=p1eff*(wsp4/ecm)                                                     !/*pa         21*/
!                                                                               !/*pa         22*/
!      based on fitting various energy (p1eff) (k+,p)                           !/*pa         23*/
!      cross sections to sums of exponential "decays"                           !/*pa         24*/
!         from data sets in cern-hera 75-01,                                    !/*pa         25*/
!         "compilation of k+,n cross-sections below 2 gev/c"                    !/*pa         26*/
!                       [ mrc 7 may 83 ]                                        !/*pa         27*/
!                                                                               !/*pa         28*/
      if (p1eff.le.0.4d0) go to 10                                              !/*pa         29*/
!                                                                               !/*pa         30*/
!     high energy, only single exponential                                      !/*pa         31*/
!                                                                               !/*pa         32*/
      bu=0.113d0*exp(2.1d0*p1eff)                                               !/*pa         33*/
      go to 20                                                                  !/*pa         34*/
!                                                                               !/*pa         35*/
!     low energy, two exponentials                                              !/*pa         36*/
!                                                                               !/*pa         37*/
   10 bs=5396.d0*exp(-12.d0*p1eff)                                              !/*pa         38*/
      bl=133.4d0*exp(-13.7d0*p1eff)                                             !/*pa         39*/
      b=7.48d-5*exp(33.3d0*p1eff)                                               !/*pa         40*/
!     frs is fraction of sigma in "short" exponential,                          !/*pa         41*/
!     form comes from integrating (exp(-bs*t) + b*exp(-bl*t) )                  !/*pa         42*/
      tkcm=dp2*rkcm**2                                                          !/*pa         43*/
      rns=(dp1-exp(-tkcm*bs))/bs                                                !/*pa         44*/
      rnl=(dp1-exp(-tkcm*bl))*b/bl                                              !/*pa         45*/
      frs=rns/(rns+rnl)                                                         !/*pa         46*/
!       i have no idea why this formula is here                                 !/*pa         47*/
!     frs = bl/(bl+bs*b)                                                        !/*pa         48*/
      r1=rang()                                                                 !/*pa         49*/
      bu=bs                                                                     !/*pa         50*/
      if (r1.gt.frs) bu=bl                                                      !/*pa         51*/
!                                                                               !/*pa         52*/
   20 continue                                                                  !/*pa         53*/
      r2=rang()                                                                 !/*pa         54*/
!       t4 is the 4-momentum transfer                                           !/*pa         55*/
      t4=-log(dp1-r2)/bu                                                        !/*pa         56*/
      t4=t4*dph/rkcm**2                                                         !/*pa         57*/
      ans=dp1-t4                                                                !/*pa         58*/
!     restrict ans to physical region                                           !/*pa         59*/
      if (abs(ans).gt.dp1) go to 20                                             !/*pa         60*/
      return                                                                    !/*pa         61*/
!                                                                               !/*pa         62*/
!     with new data from cern-hera 75-1                                         !/*pa         63*/
!         a compilation of k+ n cross-sections below 2 gev/c                    !/*pa         64*/
!         we describe dsigma/dt = c + a*exp(b*t), where c,a,b                   !/*pa         65*/
!         depend on p1 (if not k(+,-,0), p(n) )                                 !/*pa         66*/
!                                                                               !/*pa         67*/
!         then sigtot = 4*c*k**2 + a*(1.-exp(-4*b*k**2))/b                      !/*pa         68*/
!         if rand1 < 4*c*k**2/sigtot, then isotropic                            !/*pa         69*/
!         otherwise, -ln(rand2) has exp(-y) distribution                        !/*pa         70*/
!                                                                               !/*pa         71*/
!     c = ctab(p1eff)                                                           !/*pa         72*/
!     b = btab(p1eff)                                                           !/*pa         73*/
!     a = atab(p1eff)                                                           !/*pa         74*/
!     u = 4.*c*rkcm**2                                                          !/*pa         75*/
!     sigtot = u + a*(1.- exp(-4.*b*rkcm**2))/b                                 !/*pa         76*/
!     r1 = ranf(irand)                                                          !/*pa         77*/
!     if (r1 .lt. (u/sigtot)) go to 100                                         !/*pa         78*/
!  50 r2 = ranf(irand)                                                          !/*pa         79*/
!     x = 1. - log(r2)/2.d0/b/rkcm**2                                           !/*pa         80*/
!     if (abs(x) .gt. dp1) go to 50                                             !/*pa         81*/
!     ans = x                                                                   !/*pa         82*/
!     return                                                                    !/*pa         83*/
! 100 ans = 2. * ranf(irand) - 1.                                               !/*pa         84*/
!     return                                                                    !/*pa         85*/
!                                                                               !/*pa         86*/
      end                                                                       !/*pa         87*/

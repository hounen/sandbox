      function fmnmacro (iz, in)                                                !/*fm          1*/
! ======================================================================        !/*fm          2*/
!                                                                               !/*fm          3*/
!   This function calculates the Moller-Nix macroscopic mass excess             !/*fm          4*/
!   for (spherical) nuclei lying outside the tabulated range of Z and N.        !/*fm          5*/
!   The inputs are the proton and neutron numbers.                              !/*fm          6*/
!   Uses formula (62) from At. Data Nucl. Data Tables 59, 185 (1995).           !/*fm          7*/
!   Uses FRLDM, not FRDM.                                                       !/*fm          8*/
!                                                                               !/*fm          9*/
!   Called by: MOLLNIX                                                          !/*fm         10*/
!                                                                               !/*fm         11*/
!   Written by A. J. Sierk  LANL  T-2  November, 1997.                          !/*fm         12*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*fm         13*/
!                                                                               !/*fm         14*/
! ======================================================================        !/*fm         15*/
#include <cem/cdouble.h>                                                        /*!fm         16*/
#ifndef CEM                                                                     /*!fm         17*/
      fmnmacro=dp0                                                              !/*fm         18*/
#else                                                                           /*!fm         19*/
      logical nodd, podd                                                        !/*fm         20*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*fm         21*/
      common /pii/     pi                                                       !/*fm         22*/
      common /nsmnix/  nmin(93), nmax(93)                                       !/*fm         23*/
      data emp, emn, esq, ael, rp, r0, aype, ay, rmac, h, w, av, akv,           !/*fm         24*/
     &     as, aks, a0, ca                                                      !/*fm         25*/
     & /7.289034d0, 8.071431d0, 1.4399764d0, 1.433d-5, 0.80d0, 1.16d0,          !/*fm         26*/
     &  0.68d0, 0.70d0, 4.80d0, 6.6d0, 30.d0, 16.00126d0, 1.92240d0,            !/*fm         27*/
     & 21.18466d0, 2.345d0, 2.615d0, 0.10289d0/                                 !/*fm         28*/
      data d1, d2, d3/3.020833333d0,0.113541666667d0,0.00126240079d0/           !/*fm         29*/
      data zro, one, two, thr, fiv /0.d0, 1.d0, 2.d0, 3.d0, 5.d0/               !/*fm         30*/
! ======================================================================        !/*fm         31*/
      un = in                                                                   !/*fm         32*/
      z = iz                                                                    !/*fm         33*/
      a = z + un                                                                !/*fm         34*/
      ai = (un - z)/a                                                           !/*fm         35*/
      deln = zro                                                                !/*fm         36*/
      delnp = zro                                                               !/*fm         37*/
      nodd = .false.                                                            !/*fm         38*/
      podd = .false.                                                            !/*fm         39*/
      if (2*(in/2).ne.in) then                                                  !/*fm         40*/
        deln = rmac/un**thrd                                                    !/*fm         41*/
        nodd = .true.                                                           !/*fm         42*/
      endif                                                                     !/*fm         43*/
      delp = dp0                                                                !/*fm         44*/
      if (2*(iz/2).ne.iz) then                                                  !/*fm         45*/
        delp = rmac/z**thrd                                                     !/*fm         46*/
        podd = .true.                                                           !/*fm         47*/
      endif                                                                     !/*fm         48*/
      wt = zro                                                                  !/*fm         49*/
      if (nodd .and. podd) then                                                 !/*fm         50*/
        delnp = h/a**twthrd                                                     !/*fm         51*/
        if (in.eq.iz) wt = one/a                                                !/*fm         52*/
      endif                                                                     !/*fm         53*/
      c1 = thr*esq/(fiv*r0)                                                     !/*fm         54*/
      c4 = dp125m2*c1*(dp15m1/pi)**twthrd                                       !/*fm         55*/
      akf = (dp225m2*pi*z/a)**thrd/r0                                           !/*fm         56*/
      xx = akf*rp                                                               !/*fm         57*/
      fkprp = -dp125m3*esq*rp**2*(d1 - d2*xx**2 + d3*xx**4)/r0**3               !/*fm         58*/
      ys = r0*a**thrd/aype                                                      !/*fm         59*/
      yc = ys*aype/ay                                                           !/*fm         60*/
      bi = one - thr/ys**2 + (one + ys)*(two + thr*(one + one/ys)/ys)*          !/*fm         61*/
     &     exp(-two*ys)                                                         !/*fm         62*/
      b3 = one - fiv*(one - dp1875m3/yc + dp2625m3/yc**3 - dp75m2*(one+         !/*fm         63*/
     &     dp45m1/yc + dp7/yc**2 + dp35m1/yc**3)*exp(-two*yc))/yc**2            !/*fm         64*/
      fmnmacro = emp*z + emn*un - av*a*(one - akv*ai**2) + as*bi*               !/*fm         65*/
     &          (one - aks*ai**2)*a**twthrd + a0 + c1*b3*z**2/a**thrd -         !/*fm         66*/
     &          c4*z**(two*twthrd)/a**thrd + fkprp*z**2/a - ca*ai*a +           !/*fm         67*/
     &          w*(abs(ai) + wt) + delp + deln - delnp - ael*z**dp239m2         !/*fm         68*/
#endif                                                                          /*!fm         69*/
      return                                                                    !/*fm         70*/
      end                                                                       !/*fm         71*/

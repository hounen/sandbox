      function wim (iq, partin, ipatin)                                         !/*wi          1*/
! ======================================================================        !/*wi          2*/
!                                                                               !/*wi          3*/
!    Imaginary part of optical potential; protons for iq = 1, neutrons          !/*wi          4*/
!    for iq = 0.                                                                !/*wi          5*/
!                                                                               !/*wi          6*/
!   Called by CASCAD                                                            !/*wi          7*/
!                                                                               !/*wi          8*/
!   CEM95 written by S. G. Mashnik                                              !/*wi          9*/
!                                                                               !/*wi         10*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*wi         11*/
!   Edited by AJS, August, 1997.                                                !/*wi         12*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*wi         13*/
!                                                                               !/*wi         14*/
! ======================================================================        !/*wi         15*/
!                                                                               !/*wi         16*/
!  Definition of partin:                                                        !/*wi         17*/
!                       partin(1); x coordinate of particle                     !/*wi         18*/
!                       partin(2); y coordinate of particle                     !/*wi         19*/
!                       partin(3); z coordinate of particle                     !/*wi         20*/
!                       partin(8); kinetic energy of particle                   !/*wi         21*/
!                       partin(9); rest mass of particle                        !/*wi         22*/
!                                                                               !/*wi         23*/
!  Definition of ipatin:                                                        !/*wi         24*/
!                       ipatin(1); charge of particle                           !/*wi         25*/
!                       ipatin(5); zone number of nucleus where particle        !/*wi         26*/
!                                  is located.                                  !/*wi         27*/
!                                                                               !/*wi         28*/
! ======================================================================        !/*wi         29*/
#include <cem/cdouble.h>                                                        /*!wi         30*/
#ifndef CEM                                                                     /*!wi         31*/
      wim=dp0                                                                   !/*wi         32*/
#else                                                                           /*!wi         33*/
      dimension partin(9), ipatin(5)                                            !/*wi         34*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*wi         35*/
      common /mnuc/    emnucm, emnucg, emnucb, emneut, emprot                   !/*wi         36*/
      common /o/       anucl, znucl, eps, vpi, rsm(10), rbig(10),               !/*wi         37*/
     &                 rhop(10), rhon(10), af(10), tfp(10), tfn(10), n          !/*wi         38*/
      common /pii/     pi                                                       !/*wi         39*/
! ======================================================================        !/*wi         40*/
      twpi = dp2*pi                                                             !/*wi         41*/
      i1 = ipatin(5)                                                            !/*wi         42*/
      i2 = ipatin(1)                                                            !/*wi         43*/
      am = partin(9)                                                            !/*wi         44*/
      r = rsm(n)*sqrt(partin(1)**2 + partin(2)**2 + partin(3)**2)               !/*wi         45*/
      if (iq.le.0) then                                                         !/*wi         46*/
!   Becchetti & Greenlees neutron imaginary potential                           !/*wi         47*/
        r1 = dp126m2*anucl**thrd                                                !/*wi         48*/
        a1 = dp58m2                                                             !/*wi         49*/
        fac = dp1/(dp1 + exp((r - r1)/a1))                                      !/*wi         50*/
        tf = tfn(i1)                                                            !/*wi         51*/
        rho = rhon(1)*fac                                                       !/*wi         52*/
        am2 = emneut                                                            !/*wi         53*/
      else                                                                      !/*wi         54*/
!   Becchetti & Greenlees proton imaginary potential                            !/*wi         55*/
        r1 = dp132m2*anucl**thrd                                                !/*wi         56*/
        a1 = dp51m2 + dp7m1*(anucl - dp2*znucl)/anucl                           !/*wi         57*/
        fac = dp1/(dp1 + exp((r - r1)/a1))                                      !/*wi         58*/
        tf = tfp(i1)                                                            !/*wi         59*/
        rho = rhop(1)*fac                                                       !/*wi         60*/
        am2 = emprot                                                            !/*wi         61*/
      endif                                                                     !/*wi         62*/
      tp = tf*rang()**twthrd                                                    !/*wi         63*/
      t0gev = partin(8)                                                         !/*wi         64*/
!   eps is the average particle separation energy (distance of Fermi            !/*wi         65*/
!   surface below 0 energy).                                                    !/*wi         66*/
!   z is the asymptotic kinetic energy; y the momentum; pp the momentum         !/*wi         67*/
!   of the Fermi sea particle.                                                  !/*wi         68*/
      z = abs(t0gev - tf - eps)                                                 !/*wi         69*/
      y = sqrt(z*(z + dp2*am))                                                  !/*wi         70*/
      pp = sqrt(tp*(tp + dp2*am2))                                              !/*wi         71*/
!     pp = sqrt(tp*(tp + dp188m2))                                              !/*wi         72*/
      ct = dp1 - dp2*rang()                                                     !/*wi         73*/
      st = sqrt(dp1 - ct**2)                                                    !/*wi         74*/
      fi = twpi*rang()                                                          !/*wi         75*/
!   Randomly selected Fermi motion of particle in nucleus:                      !/*wi         76*/
      ppx = pp*st*cos(fi)                                                       !/*wi         77*/
      ppy = pp*st*sin(fi)                                                       !/*wi         78*/
      ppz = pp*ct                                                               !/*wi         79*/
      p0 = sqrt(t0gev*(t0gev + dp2*am))                                         !/*wi         80*/
      tt1 = am + am2                                                            !/*wi         81*/
!     tt1 = am + dp94m2                                                         !/*wi         82*/
      et = t0gev + tt1 + tp                                                     !/*wi         83*/
      vx = ppx/et                                                               !/*wi         84*/
      vy = ppy/et                                                               !/*wi         85*/
      vz = ppz/et + p0/et                                                       !/*wi         86*/
      u = et*sqrt(dp1 - vx**2 - vy**2 - vz**2)                                  !/*wi         87*/
      t = abs((u**2 - tt1**2)/(dp2*am))                                         !/*wi         88*/
!     t = abs((u**2 - tt1**2)/dp188m2)                                          !/*wi         89*/
      p = sqrt(t*(t + dp2*am))                                                  !/*wi         90*/
!  b = velocity of projectile in target nucleon rest frame                      !/*wi         91*/
      b = p/(t + am)                                                            !/*wi         92*/
      if (b.ne.dp0) then                                                        !/*wi         93*/
        if (i2.eq.iq) then                                                      !/*wi         94*/
          s = dp1063m2/b**2 - dp2992m2/b + dp429m1                              !/*wi         95*/
        else                                                                    !/*wi         96*/
          s = dp341m1/b**2 - dp822m1/b + dp822m1                                !/*wi         97*/
        endif                                                                   !/*wi         98*/
      else                                                                      !/*wi         99*/
        s = dp1000                                                              !/*wi        100*/
      endif                                                                     !/*wi        101*/
      bs = tf/t0gev                                                             !/*wi        102*/
      dz = dp1 - dp14m1*bs                                                      !/*wi        103*/
      if (bs.gt.dph) dz = dz + dp4m1*bs*((dp2 - dp1/bs)**dp25m1)                !/*wi        104*/
      wim = dp10*(y/(z + am))*dz*rho*s                                          !/*wi        105*/
#endif                                                                          /*!wi        106*/
      return                                                                    !/*wi        107*/
      end                                                                       !/*wi        108*/

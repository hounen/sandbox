      subroutine ctrans (p, h, am, c1, c2, c3)                                  !/*ct          1*/
#ifdef CEM                                                                      /*!ct          2*/
! ======================================================================        !/*ct          3*/
!***********************************************************************        !/*ct          4*/
!   Calculation of transition rates.                                   *        !/*ct          5*/
!***********************************************************************        !/*ct          6*/
!                                                                               !/*ct          7*/
!   CEM95 written by S. G. Mashnik                                              !/*ct          8*/
!                                                                               !/*ct          9*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*ct         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ct         11*/
!                                                                               !/*ct         12*/
! ======================================================================        !/*ct         13*/
#include <cem/cdouble.h>                                                        /*!ct         14*/
      common /bl1003/ u, a, z                                                   !/*ct         15*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*ct         16*/
! ======================================================================        !/*ct         17*/
      tf = dp45                                                                 !/*ct         18*/
      ak = dp1                                                                  !/*ct         19*/
      est = dp16m1*tf + u/(p + h)                                               !/*ct         20*/
      b1 = sqrt(dp2*est/emnucm)                                                 !/*ct         21*/
      b2 = b1**2                                                                !/*ct         22*/
      spp = dp1063m2/b2 - dp2992m2/b1 + dp429m1                                 !/*ct         23*/
      spn = dp341m1/b2 - dp822m1/b1 + dp822m1                                   !/*ct         24*/
      sf = (spp + spn)/dp2                                                      !/*ct         25*/
      b3 = tf/est                                                               !/*ct         26*/
      t = dp1 - dp14m1*b3                                                       !/*ct         27*/
      if (b3.gt.dph) t = t + dp4m1*b3*((dp2 - dp1/b3)**dp25m1)                  !/*ct         28*/
!   Average value of  (sigma * Vrel)  over excited states, and using            !/*ct         29*/
!   exclusion principle; divided by interaction volume.                         !/*ct         30*/
      svv = dp332m5*sf*t*sqrt(est)/(ak*((dp12m1 + dp1/(dp47m1*b1))**3))         !/*ct         31*/
!   (lambda +) from Eq. 18 of Appendix 4 of manual.                             !/*ct         32*/
      c1 = svv                                                                  !/*ct         33*/
      t1 = p + h                                                                !/*ct         34*/
      en = t1 + dp1                                                             !/*ct         35*/
      n = int(en + dp1m6)                                                       !/*ct         36*/
      ge = am*a*u                                                               !/*ct         37*/
      pp1 = p + dp1                                                             !/*ct         38*/
      hp1 = h + dp1                                                             !/*ct         39*/
      pp2 = p + dp2                                                             !/*ct         40*/
      hm1 = h - dp1                                                             !/*ct         41*/
      hm2 = h - dp2                                                             !/*ct         42*/
      hm3 = h - dp3                                                             !/*ct         43*/
      aph = dp25m2*(p*pp1 + h*hm3)                                              !/*ct         44*/
      aph1 = dp25m2*(pp1*pp2 + hp1*hm2)                                         !/*ct         45*/
      pn1 = ((ge - aph)/(ge - aph1))**n                                         !/*ct         46*/
!   (lambda -) from Eq. 18 of Appendix 4 of manual.                             !/*ct         47*/
      c2 = c1*pn1*p*h*en*(t1 - dp2)/(ge - aph)**2                               !/*ct         48*/
      c2 = max (dp0, c2)                                                        !/*ct         49*/
!   (lambda 0) from Eq. 18 of Appendix 4 of manual.                             !/*ct         50*/
      c3 = c1*pn1*en*(p*(p-dp1) + dp4*p*h + h*hm1)/(t1*(ge - aph))              !/*ct         51*/
#endif                                                                          /*!ct         52*/
      return                                                                    !/*ct         53*/
      end                                                                       !/*ct         54*/

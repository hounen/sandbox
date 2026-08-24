      function tkin (l, am)                                                     !/*tk          1*/
! ======================================================================        !/*tk          2*/
!***********************************************************************        !/*tk          3*/
!  Randomly select kinetic energy for evaporated particles in          *        !/*tk          4*/
!  equilibrium decay.                                                  *        !/*tk          5*/
!***********************************************************************        !/*tk          6*/
!                                                                               !/*tk          7*/
!   CEM95 written by S. G. Mashnik                                              !/*tk          8*/
!                                                                               !/*tk          9*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*tk         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*tk         11*/
!                                                                               !/*tk         12*/
! ======================================================================        !/*tk         13*/
#include <cem/cdouble.h>                                                        /*!tk         14*/
#ifndef CEM                                                                     /*!tk         15*/
      tkin=dp0                                                                  !/*tk         16*/
#else                                                                           /*!tk         17*/
      common /bl1009/ afj(7)                                                    !/*tk         18*/
      common /bl1011/  vj(6)                                                    !/*tk         19*/
      common /bl1015/  rj(7)                                                    !/*tk         20*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*tk         21*/
! ======================================================================        !/*tk         22*/
      rb = sqrt(dp4*am*afj(l)*rj(l))                                            !/*tk         23*/
      frk0 = dp15m1*sqrt(dp3)                                                   !/*tk         24*/
   10 b1 = rang()                                                               !/*tk         25*/
      rk = dp1 + (dp1/rb)*log(b1 + (dp1 - b1)*exp(-rb))                         !/*tk         26*/
      if (l.eq.1) then                                                          !/*tk         27*/
        beta = (dp212m2/afj(1)**twthrd - dp5m2)/                                !/*tk         28*/
     &          (dp76m2 + dp22m1/afj(1)**thrd)                                  !/*tk         29*/
        q1 = dp1 + beta/rj(1)                                                   !/*tk         30*/
        q2 = q1*sqrt(q1)                                                        !/*tk         31*/
        frk = frk0*rk*(q1 - rk**2)/q2                                           !/*tk         32*/
      else                                                                      !/*tk         33*/
        frk = frk0*rk*(dp1 - rk**2)                                             !/*tk         34*/
      endif                                                                     !/*tk         35*/
      b2 = rang()                                                               !/*tk         36*/
      if (b2.gt.frk) go to 10                                                   !/*tk         37*/
      tkin = rj(l)*(dp1 - rk**2) + vj(l)                                        !/*tk         38*/
#endif                                                                          /*!tk         39*/
      return                                                                    !/*tk         40*/
      end                                                                       !/*tk         41*/

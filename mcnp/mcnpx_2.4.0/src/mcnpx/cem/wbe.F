      function wbe (j, e)                                                       !/*wb          1*/
! ======================================================================        !/*wb          2*/
!                                                                               !/*wb          3*/
!   The integral of WBE (over excitation energy) gives the                      !/*wb          4*/
!   preequilibrium particle emission width for particle of type "j".            !/*wb          5*/
!   See Eqs. (14) & (24a) of Gudima, et al. (1983).                             !/*wb          6*/
!                                                                               !/*wb          7*/
!   Called from GAMAGU (via GU8).                                               !/*wb          8*/
!                                                                               !/*wb          9*/
!   CEM95 written by S. G. Mashnik                                              !/*wb         10*/
!                                                                               !/*wb         11*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*wb         12*/
!   Edited by AJS, August, 1997.                                                !/*wb         13*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*wb         14*/
!                                                                               !/*wb         15*/
! ======================================================================        !/*wb         16*/
#include <cem/cdouble.h>                                                        /*!wb         17*/
#ifndef CEM                                                                     /*!wb         18*/
      wbe=dp0                                                                   !/*wb         19*/
#else                                                                           /*!wb         20*/
      common /bl1003/  u, a, z                                                  !/*wb         21*/
      common /bl1005/  aj(6)                                                    !/*wb         22*/
      common /bl1009/  afj(7)                                                   !/*wb         23*/
      common /bl1011/  vj(6)                                                    !/*wb         24*/
      common /blac/    ac                                                       !/*wb         25*/
      common /blalj/   alj(6)                                                   !/*wb         26*/
      common /blbj/    bj(7)                                                    !/*wb         27*/
      common /blexn/   exn                                                      !/*wb         28*/
      common /blr0/    r0                                                       !/*wb         29*/
      common /br0j/    r0j(6), bn                                               !/*wb         30*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*wb         31*/
      common /gambet/  gb(6)                                                    !/*wb         32*/
! ======================================================================        !/*wb         33*/
      f1 = (e + bj(j))/u                                                        !/*wb         34*/
      f2 = (e - vj(j))/u                                                        !/*wb         35*/
      f3 = sqrt(u*aj(j))                                                        !/*wb         36*/
      if (j.gt.2) then                                                          !/*wb         37*/
        wbe = dp104m3*gb(j)*r0j(j)/(f3*r0*afj(j)**thrd)                         !/*wb         38*/
        if (f1.lt.dp0) then                                                     !/*wb         39*/
          fct = dp0                                                             !/*wb         40*/
        else                                                                    !/*wb         41*/
          fct = f1**(aj(j) - dp15m1)                                            !/*wb         42*/
        endif                                                                   !/*wb         43*/
        wbe = wbe*alj(j)*f2*fct                                                 !/*wb         44*/
        wbe = wbe*(dp1 - f1)**(exn - aj(j) - dp1)                               !/*wb         45*/
      else                                                                      !/*wb         46*/
!  Protons and neutrons:                                                        !/*wb         47*/
        if (j.eq.2) then                                                        !/*wb         48*/
          c1 = u*f2                                                             !/*wb         49*/
        elseif (j.lt.2) then                                                    !/*wb         50*/
          c1 = e + bn                                                           !/*wb         51*/
        endif                                                                   !/*wb         52*/
        wbe = dp234m6*alj(j)*r0j(j)*r0*r0*afj(j)**twthrd/                       !/*wb         53*/
     &        (ac*u*afj(j))                                                     !/*wb         54*/
        wbe = wbe*c1*(dp1 - f1)**(exn - dp2)                                    !/*wb         55*/
      endif                                                                     !/*wb         56*/
#endif                                                                          /*!wb         57*/
      return                                                                    !/*wb         58*/
      end                                                                       !/*wb         59*/

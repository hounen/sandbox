      function deltam (a, z, ish)                                               !/*de          1*/
! ======================================================================        !/*de          2*/
!                                                                               !/*de          3*/
!***********************************************************************        !/*de          4*/
!  Calculation of mass excess.                                         *        !/*de          5*/
!***********************************************************************        !/*de          6*/
!                                                                               !/*de          7*/
!   Called by: PRECOF                                                           !/*de          8*/
!                                                                               !/*de          9*/
!   Calls: MOLNIX SHELL                                                         !/*de         10*/
!                                                                               !/*de         11*/
!   CEM95 written by S. G. Mashnik                                              !/*de         12*/
!                                                                               !/*de         13*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*de         14*/
!   Modified by AJS, May, 1996 (adding exptl. or Moller-Nix masses).            !/*de         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*de         16*/
!                                                                               !/*de         17*/
! ======================================================================        !/*de         18*/
#include <cem/cdouble.h>                                                        /*!de         19*/
#ifndef CEM                                                                     /*!de         20*/
      deltam=dp0                                                                !/*de         21*/
#else                                                                           /*!de         22*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*de         23*/
! ======================================================================        !/*de         24*/
      iz = z + dp1m6                                                            !/*de         25*/
      n = a - z + dp1m6                                                         !/*de         26*/
      if (ish.eq.2) then                                                        !/*de         27*/
        frthrd = dp2*twthrd                                                     !/*de         28*/
        es = (p2_5835p1 - p4_4235p1*((a - dp2*z)/a)**2)*                        !/*de         29*/
     &       (((dp1 - p6_2025m1/a**twthrd)**2)*(a**twthrd))                     !/*de         30*/
        ec = p7_79m1*(z*(z - dp1))/a**thrd*                                     !/*de         31*/
     &       (dp1 - p1_5849/a**twthrd + p1_2273/a + p1_5772/a**frthrd)          !/*de         32*/
        ealfa = -p4_323m1*z**frthrd/(a**thrd)*                                  !/*de         33*/
     &          (dp1 + p4_9597m1/a - p5_7811m1/a**thrd -                        !/*de         34*/
     &           p1_4518m1/a**twthrd)                                           !/*de         35*/
        edob = ((p8_367*a + p3_1450p1*((a - dp2*z)**2/a)) -                     !/*de         36*/
     &          (p7_83m1*z + p1_7035p1*a))                                      !/*de         37*/
        deltam = es + ec + ealfa + edob                                         !/*de         38*/
        deltam = deltam + shell (a, z, ish)                                     !/*de         39*/
      elseif (ish.eq.4) then                                                    !/*de         40*/
        deltam = fmolnix (iz, n, 2)                                             !/*de         41*/
      endif                                                                     !/*de         42*/
#endif                                                                          /*!de         43*/
      return                                                                    !/*de         44*/
      end                                                                       !/*de         45*/

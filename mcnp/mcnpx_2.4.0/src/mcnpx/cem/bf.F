      function bf (a, z, e)                                                     !/*bf          1*/
! ======================================================================        !/*bf          2*/
!                                                                               !/*bf          3*/
!   Function to give fission barrier in MeV, given A and Z, and                 !/*bf          4*/
!   excitation energy at the saddle point, e.                                   !/*bf          5*/
!                                                                               !/*bf          6*/
!   CEM95 written by S. G. Mashnik                                              !/*bf          7*/
!                                                                               !/*bf          8*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*bf          9*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*bf         10*/
!                                                                               !/*bf         11*/
! ======================================================================        !/*bf         12*/
#include <cem/cdouble.h>                                                        /*!bf         13*/
#ifndef CEM                                                                     /*!bf         14*/
      bf=dp0                                                                    !/*bf         15*/
#else                                                                           /*!bf         16*/
      common /adbf/   amf, ijsp, nhump, r0m                                     !/*bf         17*/
      common /ajsbar/ ainit, zinit, eb(20,50,90), egs(20,50,90)                 !/*bf         18*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*bf         19*/
      common /jsp/    ws(51)                                                    !/*bf         20*/
      common /lsierk/ egs0, ln                                                  !/*bf         21*/
      common /menu1/  ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel            !/*bf         22*/
      common /nix/    yy2(51), xx1(51), xx2(51)                                 !/*bf         23*/
! ======================================================================        !/*bf         24*/
      iz = z+dp1m6                                                              !/*bf         25*/
      un = a - z                                                                !/*bf         26*/
      in = un+dp1m6                                                             !/*bf         27*/
      zsq = z*z                                                                 !/*bf         28*/
      xx = zsq/a                                                                !/*bf         29*/
      sym = ((un - z)/a)**2                                                     !/*bf         30*/
      egs0 = dp1m6                                                              !/*bf         31*/
      if (ib.eq.6) then                                                         !/*bf         32*/
!***********************************************************************        !/*bf         33*/
! Yukawa-plus-exponential macroscopic model of Krappe, Nix & Sierk(1979)        !/*bf         34*/
!***********************************************************************        !/*bf         35*/
        gamma = dp3                                                             !/*bf         36*/
        a2 = dp217m1                                                            !/*bf         37*/
        a3 = dp7322m4                                                           !/*bf         38*/
      elseif (ib.eq.7) then                                                     !/*bf         39*/
!***********************************************************************        !/*bf         40*/
!  Sierk [Phys. Rev. C 33, 2039 (1986)] global fit for macroscopic Bf. *        !/*bf         41*/
!  Yukawa-plus-exponential nuclear energy; diffuse-surface Coulomb     *        !/*bf         42*/
!  energy; diffuse-matter moments of inertia.                          *        !/*bf         43*/
!***********************************************************************        !/*bf         44*/
        call barfit (a, z, ln, bf0, egs0)                                       !/*bf         45*/
      endif                                                                     !/*bf         46*/
      if (ib.eq.6) then                                                         !/*bf         47*/
        a3rt = a**thrd                                                          !/*bf         48*/
        a3rt2 = a3rt**2                                                         !/*bf         49*/
        acor = dp1 - gamma*sym                                                  !/*bf         50*/
        sufnuc = a2*acor*a3rt2                                                  !/*bf         51*/
        coulmb = a3*zsq/a3rt                                                    !/*bf         52*/
        x = dph*coulmb/sufnuc                                                   !/*bf         53*/
        if (x.ge.dp1) then                                                      !/*bf         54*/
          bf = dp0                                                              !/*bf         55*/
          return                                                                !/*bf         56*/
        endif                                                                   !/*bf         57*/
        bx = subev (x, xx2, yy2, 51)                                            !/*bf         58*/
        bf0 = sufnuc*bx                                                         !/*bf         59*/
        bf0 = max(bf0, dp0)                                                     !/*bf         60*/
      endif                                                                     !/*bf         61*/
!                                                                               !/*bf         62*/
!     Bf0 = bf without shell and odd-even corrections to the g.s. mass          !/*bf         63*/
!                                                                               !/*bf         64*/
      daz = shell (a, z, ish)                                                   !/*bf         65*/
      bf = bf0 - daz                                                            !/*bf         66*/
      if (ib.eq.6) then                                                         !/*bf         67*/
!  Approximate rotational lowering of barrier.                                  !/*bf         68*/
        w = subev (x, xx1, ws, 51)                                              !/*bf         69*/
        fi = ln                                                                 !/*bf         70*/
        fjrb = dp9656m6*r0m*r0m*a**fvthrd                                       !/*bf         71*/
        dbfi = fi * (fi + dp1) * (w - dp1)/(dp2*fjrb*w)                         !/*bf         72*/
        bf = bf - dbfi                                                          !/*bf         73*/
      endif                                                                     !/*bf         74*/
      bf = max(bf, dp0)                                                         !/*bf         75*/
#endif                                                                          /*!bf         76*/
      return                                                                    !/*bf         77*/
      end                                                                       !/*bf         78*/

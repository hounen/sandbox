      function bf2 (a, z, e)                                                    !/*bf          1*/
! ======================================================================        !/*bf          2*/
!                                                                               !/*bf          3*/
!   Function to give fission barrier in MeV, given A, Z, L, and                 !/*bf          4*/
!   excitation energy at the saddle point, e. bf2 is for use in                 !/*bf          5*/
!   BARFIT when the a Z, A, L values lie outside the range of the fit.          !/*bf          6*/
!   It gives an approximate value using the Krappe-Nix-Sierk model.             !/*bf          7*/
!                                                                               !/*bf          8*/
!   CEM95 written by S. G. Mashnik                                              !/*bf          9*/
!                                                                               !/*bf         10*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*bf         11*/
!   bf2 created from bf by AJS, December, 1997                                  !/*bf         12*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*bf         13*/
!                                                                               !/*bf         14*/
! ======================================================================        !/*bf         15*/
#include <cem/cdouble.h>                                                        /*!bf         16*/
#ifndef CEM                                                                     /*!bf         17*/
      bf2=dp0                                                                   !/*bf         18*/
#else                                                                           /*!bf         19*/
      common /adbf/    amf, ijsp, nhump, r0m                                    !/*bf         20*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*bf         21*/
      common /jsp/     ws(51)                                                   !/*bf         22*/
      common /lsierk/  egs0, ln                                                 !/*bf         23*/
      common /nix/     yy2(51), xx1(51), xx2(51)                                !/*bf         24*/
! ======================================================================        !/*bf         25*/
      iz = int(z+dp1m6)                                                         !/*bf         26*/
      un = a - z                                                                !/*bf         27*/
      in = int(un+dp1m6)                                                        !/*bf         28*/
      zsq = z*z                                                                 !/*bf         29*/
      xx = zsq/a                                                                !/*bf         30*/
      sym = ((un - z)/a)**2                                                     !/*bf         31*/
      egs0 = dp1m6                                                              !/*bf         32*/
      gamma = dp3                                                               !/*bf         33*/
      a2 = dp217m1                                                              !/*bf         34*/
      a3 = dp7322m4                                                             !/*bf         35*/
      a3rt = a**thrd                                                            !/*bf         36*/
      a3rt2 = a3rt**2                                                           !/*bf         37*/
      acor = dp1 - gamma*sym                                                    !/*bf         38*/
      sufnuc = a2*acor*a3rt2                                                    !/*bf         39*/
      coulmb = a3*zsq/a3rt                                                      !/*bf         40*/
      x = dph*coulmb/sufnuc                                                     !/*bf         41*/
      if (x.ge.dp1) then                                                        !/*bf         42*/
        bf2 = dp0                                                               !/*bf         43*/
        return                                                                  !/*bf         44*/
      endif                                                                     !/*bf         45*/
      bx = subev (x, xx2, yy2, 51)                                              !/*bf         46*/
      bf0 = sufnuc*bx                                                           !/*bf         47*/
      bf0 = max(bf0, dp0)                                                       !/*bf         48*/
      daz = shell (a, z, 2)                                                     !/*bf         49*/
      bf2 = bf0 - daz                                                           !/*bf         50*/
!  Approximate rotational lowering of barrier.                                  !/*bf         51*/
      w = subev (x, xx1, ws, 51)                                                !/*bf         52*/
      fi = ln                                                                   !/*bf         53*/
      fjrb = dp9656m6*r0m*r0m*a**fvthrd                                         !/*bf         54*/
      dbfi = fi*(fi + dp1)*(w - dp1)/(dp2*fjrb*w)                               !/*bf         55*/
      egs0 = fi*(fi + dp1)/(dp2*fjrb)                                           !/*bf         56*/
      bf2 = bf2 - dbfi                                                          !/*bf         57*/
      bf2 = max(bf2, dp0)                                                       !/*bf         58*/
#endif                                                                          /*!bf         59*/
      return                                                                    !/*bf         60*/
      end                                                                       !/*bf         61*/

      function wopt (a, z, e1, r, i1)                                           !/*wo          1*/
! ======================================================================        !/*wo          2*/
!                                                                               !/*wo          3*/
!    Negative of the imaginary part of optical potential.                       !/*wo          4*/
!    Wopt is the negative of the W(r) defined in Becchetti & Greenlees.         !/*wo          5*/
!                                                                               !/*wo          6*/
!   Called by CASCAD                                                            !/*wo          7*/
!                                                                               !/*wo          8*/
!   CEM95 written by S. G. Mashnik                                              !/*wo          9*/
!                                                                               !/*wo         10*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*wo         11*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*wo         12*/
!                                                                               !/*wo         13*/
! ======================================================================        !/*wo         14*/
#include <cem/cdouble.h>                                                        /*!wo         15*/
#ifndef CEM                                                                     /*!wo         16*/
      wopt=dp0                                                                  !/*wo         17*/
#else                                                                           /*!wo         18*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*wo         19*/
! ======================================================================        !/*wo         20*/
      x = (a - dp2*z)/a                                                         !/*wo         21*/
      e = e1*dp1000                                                             !/*wo         22*/
      if (i1.le.0) then                                                         !/*wo         23*/
        if (e.le.dp25) then                                                     !/*wo         24*/
!  Imaginary part of Becchetti & Greenlees neutron optical potential.           !/*wo         25*/
!  [Phys. Rev. 182, 1190 (1969)] 0 < E < 40 MeV.                                !/*wo         26*/
          rm = dp126m2*a**thrd                                                  !/*wo         27*/
          am = dp58m2                                                           !/*wo         28*/
          wv = dp22m2*e - dp156m2                                               !/*wo         29*/
          wsf = dp13 - dp25m2*e - dp12*x                                        !/*wo         30*/
        else                                                                    !/*wo         31*/
!  Imaginary part of Marshak, Langford, Tamura, & Wong neutron optical          !/*wo         32*/
!  potential [Phys. Rev. C 2, 1862 (1970)]. 2 < E < 129 MeV.                    !/*wo         33*/
          rm = dp121m2*a**thrd                                                  !/*wo         34*/
          am = dp6448m4                                                         !/*wo         35*/
          wv = dp459m3 + dp111m3*e                                              !/*wo         36*/
          wsf = dp428m2 - dp414m4*e                                             !/*wo         37*/
        endif                                                                   !/*wo         38*/
      else                                                                      !/*wo         39*/
        if (e.le.dp25) then                                                     !/*wo         40*/
!  Imaginary part of Becchetti & Greenlees proton optical potential.            !/*wo         41*/
          rm = dp132m2*a**thrd                                                  !/*wo         42*/
          am = dp51m2 + dp7m1*x                                                 !/*wo         43*/
          wv = dp22m2*e - dp27m1                                                !/*wo         44*/
          wsf = dp118m1 - dp25m2*e + dp12*x                                     !/*wo         45*/
        else                                                                    !/*wo         46*/
!  Imaginary part of Menet, Gross, Malanify, & Zucker proton optical            !/*wo         47*/
!  potential [Phys. Rev. C 4, 1114 (1971)]  30 < E < 60 MeV.                    !/*wo         48*/
          rm = dp137m2*a**thrd                                                  !/*wo         49*/
          am = dp74m2 - dp8m3*e + x                                             !/*wo         50*/
          wv = dp12m1 + dp9m2*e                                                 !/*wo         51*/
          wsf = dp42m1 - dp5m2*e + dp155m1*x                                    !/*wo         52*/
        endif                                                                   !/*wo         53*/
      endif                                                                     !/*wo         54*/
      wv = max (wv, dp0)                                                        !/*wo         55*/
      wsf = max (wsf, dp0)                                                      !/*wo         56*/
      f = dp1/(dp1 + exp((r - rm)/am))                                          !/*wo         57*/
!  df/dr = (f - 1)/(am*f)                                                       !/*wo         58*/
      wopt = f*(wv + dp4*wsf*(dp1 - f))                                         !/*wo         59*/
#endif                                                                          /*!wo         60*/
      return                                                                    !/*wo         61*/
      end                                                                       !/*wo         62*/

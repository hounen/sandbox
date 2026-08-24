      function cosex (l, t, cm)                                                 !/*co          1*/
! ======================================================================        !/*co          2*/
!                                                                               !/*co          3*/
!     Random cosine of scattering angle, weighted by the angular                !/*co          4*/
!     distribution for charge exchange scattering.                              !/*co          5*/
!     l is hard wired to be 0!!                                                 !/*co          6*/
!                                                                               !/*co          7*/
!   Called by:                                                                  !/*co          8*/
!                                                                               !/*co          9*/
!   Calls: COSTA                                                                !/*co         10*/
!                                                                               !/*co         11*/
!   CEM95 written by S. G. Mashnik                                              !/*co         12*/
!                                                                               !/*co         13*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*co         14*/
!   Edited by AJS, July, 1997.                                                  !/*co         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*co         16*/
!                                                                               !/*co         17*/
! ======================================================================        !/*co         18*/
#include <cem/cdouble.h>                                                        /*!co         19*/
#ifndef CEM                                                                     /*!co         20*/
      cosex=dp0                                                                 !/*co         21*/
#else                                                                           /*!co         22*/
!                                                                               !/*co         23*/
      if (t.le.dp8m2) then                                                      !/*co         24*/
        cosex = costa (17,t)                                                    !/*co         25*/
      elseif (t.le.dp3m1) then                                                  !/*co         26*/
        cosex = costa (18,t)                                                    !/*co         27*/
      elseif (t.le.dp1) then                                                    !/*co         28*/
        cosex = costa (10,t)                                                    !/*co         29*/
      elseif (t.le.dp24m1) then                                                 !/*co         30*/
        cosex = costa (11,t)                                                    !/*co         31*/
      else                                                                      !/*co         32*/
        tmax = sqrt(t*(t + dp2*cm))                                             !/*co         33*/
        cosex = dp1 + (dp2*log(dp1 + rang()*                                    !/*co         34*/
     &                (exp(-dp75m1*tmax) - dp1)))/(dp75m1*tmax)                 !/*co         35*/
      endif                                                                     !/*co         36*/
#endif                                                                          /*!co         37*/
      return                                                                    !/*co         38*/
      end                                                                       !/*co         39*/

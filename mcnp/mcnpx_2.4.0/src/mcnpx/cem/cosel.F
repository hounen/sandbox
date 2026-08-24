      function cosel (l, mb, ksi, t, cm)                                        !/*co          1*/
! ======================================================================        !/*co          2*/
!                                                                               !/*co          3*/
!     Cosine of a random angle, weighted by the angular distribution            !/*co          4*/
!     for elastic scattering.                                                   !/*co          5*/
!     mb is baryon number; l is hardwired to be 0.                              !/*co          6*/
!                                                                               !/*co          7*/
!   Called by: ELEX                                                             !/*co          8*/
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
      cosel=dp0                                                                 !/*co         21*/
#else                                                                           /*!co         22*/
      if (mb.ge.2) then                                                         !/*co         23*/
!  N-N scattering                                                               !/*co         24*/
        if (ksi.ge.2) then                                                      !/*co         25*/
          if (t.gt.dp97m2) then                                                 !/*co         26*/
            if (t.le.dp28m1) then                                               !/*co         27*/
              cosel = (dp1 + costa (1,t))/dp2                                   !/*co         28*/
            elseif (t.le.dp10) then                                             !/*co         29*/
              cosel = (dp3 - costa (2,t))/dp4                                   !/*co         30*/
            else                                                                !/*co         31*/
              tmax = sqrt(t*(t + dp2*cm))                                       !/*co         32*/
              cosel = dp1 + (dp2*log(dp1 + rang()*                              !/*co         33*/
     &                      (exp(-dp87m1*tmax) - dp1)))/(dp87m1*tmax)           !/*co         34*/
            endif                                                               !/*co         35*/
          else                                                                  !/*co         36*/
            cosel = costa (3,t)                                                 !/*co         37*/
          endif                                                                 !/*co         38*/
        else                                                                    !/*co         39*/
          if (t.le.dp46m2) then                                                 !/*co         40*/
            cosel = dp1 - dp2*rang()                                            !/*co         41*/
          elseif (t.le.dp28m1) then                                             !/*co         42*/
            cosel = (dp1 + costa (1,t))/dp2                                     !/*co         43*/
          elseif (t.le.dp10) then                                               !/*co         44*/
            cosel = (dp3 - costa (2,t))/dp4                                     !/*co         45*/
          else                                                                  !/*co         46*/
            tmax = sqrt(t*(t + dp2*cm))                                         !/*co         47*/
            cosel = dp1 + (dp2*log(dp1 + rang()*                                !/*co         48*/
     &                    (exp(-dp87m1*tmax) - dp1)))/(dp87m1*tmax)             !/*co         49*/
          endif                                                                 !/*co         50*/
        endif                                                                   !/*co         51*/
      else                                                                      !/*co         52*/
!  pi-N scattering                                                              !/*co         53*/
        if (ksi.lt.2) then                                                      !/*co         54*/
          go to 20                                                              !/*co         55*/
        elseif (ksi.eq.2) then                                                  !/*co         56*/
          go to 30                                                              !/*co         57*/
        else                                                                    !/*co         58*/
          if (rang().le.dph) then                                               !/*co         59*/
            go to 20                                                            !/*co         60*/
          else                                                                  !/*co         61*/
            go to 30                                                            !/*co         62*/
          endif                                                                 !/*co         63*/
        endif                                                                   !/*co         64*/
   20   if (t.le.dp8m2) then                                                    !/*co         65*/
          cosel = costa (4,t)                                                   !/*co         66*/
        elseif (t.le.dp3m1) then                                                !/*co         67*/
          cosel = costa (5,t)                                                   !/*co         68*/
        elseif (t.le.dp1) then                                                  !/*co         69*/
          cosel = costa (6,t)                                                   !/*co         70*/
        elseif (t.le.dp24m1) then                                               !/*co         71*/
          cosel = costa (7,t)                                                   !/*co         72*/
        else                                                                    !/*co         73*/
          tmax = sqrt(t*(t + dp2*cm))                                           !/*co         74*/
          cosel = dp1 + (dp2*log(dp1 + rang()*                                  !/*co         75*/
     &                 (exp(-dp75m1*tmax) - dp1)))/(dp75m1*tmax)                !/*co         76*/
        endif                                                                   !/*co         77*/
        return                                                                  !/*co         78*/
   30   if (t.le.dp8m2) then                                                    !/*co         79*/
          cosel = costa (8,t)                                                   !/*co         80*/
        elseif (t.le.dp3m1) then                                                !/*co         81*/
          cosel = costa (9,t)                                                   !/*co         82*/
        elseif (t.le.dp1) then                                                  !/*co         83*/
          cosel = costa (10,t)                                                  !/*co         84*/
        elseif (t.le.dp24m1) then                                               !/*co         85*/
          cosel = costa (11,t)                                                  !/*co         86*/
        else                                                                    !/*co         87*/
          tmax = sqrt(t*(t + dp2*cm))                                           !/*co         88*/
          cosel = dp1 + (dp2*log(dp1 + rang()*                                  !/*co         89*/
     &                   (exp(-dp75m1*tmax) - dp1)))/(dp75m1*tmax)              !/*co         90*/
        endif                                                                   !/*co         91*/
      endif                                                                     !/*co         92*/
#endif                                                                          /*!co         93*/
      return                                                                    !/*co         94*/
      end                                                                       !/*co         95*/

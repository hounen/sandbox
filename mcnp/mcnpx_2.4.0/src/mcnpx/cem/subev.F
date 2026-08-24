      function subev (u, e, f, n)                                               !/*su          1*/
! ======================================================================        !/*su          2*/
!                                                                               !/*su          3*/
!    Quadratic interpolation of tabular values.                                 !/*su          4*/
!                                                                               !/*su          5*/
!    CEM95 written by S. G. Mashnik                                             !/*su          6*/
!                                                                               !/*su          7*/
!    Edited by A. J. Sierk   LANL  T-2,  February, 1996.                        !/*su          8*/
!    Changed to double by F. Gallmeier, ORNL CPED, October 1998.                !/*su          9*/
!                                                                               !/*su         10*/
! ======================================================================        !/*su         11*/
#include <cem/cdouble.h>                                                        /*!su         12*/
#ifndef CEM                                                                     /*!su         13*/
      subev=dp0                                                                 !/*su         14*/
#else                                                                           /*!su         15*/
      dimension e(n), f(n)                                                      !/*su         16*/
! ======================================================================        !/*su         17*/
      if (u.le.e(1)) then                                                       !/*su         18*/
        x1 = e(1)                                                               !/*su         19*/
        x2 = e(2)                                                               !/*su         20*/
        x3 = e(3)                                                               !/*su         21*/
        y1 = f(1)                                                               !/*su         22*/
        y2 = f(2)                                                               !/*su         23*/
        y3 = f(3)                                                               !/*su         24*/
      else                                                                      !/*su         25*/
        if (u.ge.e(n-1)) then                                                   !/*su         26*/
          if (u.gt.e(n)) then                                                   !/*su         27*/
            subev = f(n)                                                        !/*su         28*/
            return                                                              !/*su         29*/
          else                                                                  !/*su         30*/
            x1 = e(n-2)                                                         !/*su         31*/
            x2 = e(n-1)                                                         !/*su         32*/
            x3 = e(n)                                                           !/*su         33*/
            y1 = f(n-2)                                                         !/*su         34*/
            y2 = f(n-1)                                                         !/*su         35*/
            y3 = f(n)                                                           !/*su         36*/
          endif                                                                 !/*su         37*/
        else                                                                    !/*su         38*/
          do 10 j = 2,n                                                         !/*su         39*/
          if (u.lt.e(j)) then                                                   !/*su         40*/
            x1 = e(j-1)                                                         !/*su         41*/
            x2 = e(j)                                                           !/*su         42*/
            x3 = e(j+1)                                                         !/*su         43*/
            y1 = f(j-1)                                                         !/*su         44*/
            y2 = f(j)                                                           !/*su         45*/
            y3 = f(j+1)                                                         !/*su         46*/
            go to 20                                                            !/*su         47*/
          endif                                                                 !/*su         48*/
   10     continue                                                              !/*su         49*/
        endif                                                                   !/*su         50*/
      endif                                                                     !/*su         51*/
   20 subev = y1*(((u - x2)*(u - x3))/((x1 - x2)*(x1 - x3))) +                  !/*su         52*/
     &        y2*(((u - x1)*(u - x3))/((x2 - x1)*(x2 - x3))) +                  !/*su         53*/
     &        y3*(((u - x1)*(u - x2))/((x3 - x1)*(x3 - x2)))                    !/*su         54*/
#endif                                                                          /*!su         55*/
      return                                                                    !/*su         56*/
      end                                                                       !/*su         57*/

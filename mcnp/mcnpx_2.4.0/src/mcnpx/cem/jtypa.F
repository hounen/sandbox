      function jtypa (ith, mb, lamb)                                            !/*jt          1*/
#ifndef CEM                                                                     /*!jt          2*/
      jtypa = 0                                                                 !/*jt          3*/
#else                                                                           /*!jt          4*/
! ======================================================================        !/*jt          5*/
!                                                                               !/*jt          6*/
!     Determining type of angular distribution coefficients a(n,k).             !/*jt          7*/
!     mb = baryon number.                                                       !/*jt          8*/
!                                                                               !/*jt          9*/
!   Called by: DIRECT                                                           !/*jt         10*/
!                                                                               !/*jt         11*/
!   CEM95 written by S. G. Mashnik                                              !/*jt         12*/
!                                                                               !/*jt         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*jt         14*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*jt         15*/
!                                                                               !/*jt         16*/
! ======================================================================        !/*jt         17*/
#include <cem/cdouble.h>                                                        /*!jt         18*/
      if (ith.ne.0) then                                                        !/*jt         19*/
!   iks = 7 cross section:                                                      !/*jt         20*/
        if (mb.le.1) then                                                       !/*jt         21*/
!   pi + N:                                                                     !/*jt         22*/
          if (lamb.gt.1) then                                                   !/*jt         23*/
            jtypa=25                                                            !/*jt         24*/
          else                                                                  !/*jt         25*/
            jtypa=24                                                            !/*jt         26*/
          endif                                                                 !/*jt         27*/
        else                                                                    !/*jt         28*/
!   N + N                                                                       !/*jt         29*/
          if (lamb.le.1 .or. lamb.eq.3) then                                    !/*jt         30*/
            jtypa = 20                                                          !/*jt         31*/
          else                                                                  !/*jt         32*/
            jtypa = 21                                                          !/*jt         33*/
          endif                                                                 !/*jt         34*/
        endif                                                                   !/*jt         35*/
      else                                                                      !/*jt         36*/
!   iks = 4, 5, or 6 cross section:                                             !/*jt         37*/
        if (mb.le.1) then                                                       !/*jt         38*/
!   pi + N:                                                                     !/*jt         39*/
          if (lamb.le.1) then                                                   !/*jt         40*/
            jtypa = 26                                                          !/*jt         41*/
          else                                                                  !/*jt         42*/
            jtypa = 27                                                          !/*jt         43*/
          endif                                                                 !/*jt         44*/
        else                                                                    !/*jt         45*/
!   N + N                                                                       !/*jt         46*/
          if (lamb.le.1 .or. lamb.eq.3) then                                    !/*jt         47*/
            jtypa = 22                                                          !/*jt         48*/
          else                                                                  !/*jt         49*/
            jtypa = 23                                                          !/*jt         50*/
          endif                                                                 !/*jt         51*/
        endif                                                                   !/*jt         52*/
      endif                                                                     !/*jt         53*/
#endif                                                                          /*!jt         54*/
      return                                                                    !/*jt         55*/
      end                                                                       !/*jt         56*/

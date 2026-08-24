      function jtypb (ith, mb, lamb)                                            !/*jt          1*/
#ifndef CEM                                                                     /*!jt          2*/
      jtypb = 0                                                                 !/*jt          3*/
#else                                                                           /*!jt          4*/
! ======================================================================        !/*jt          5*/
!                                                                               !/*jt          6*/
!     Determining type of coefficients b(n,k);                                  !/*jt          7*/
!     for angular distribution calculation.                                     !/*jt          8*/
!                                                                               !/*jt          9*/
!     ith = 0 or 1;                                                             !/*jt         10*/
!     mb = baryon number of channel;                                            !/*jt         11*/
!     lamb = 1, 2, or 3.                                                        !/*jt         12*/
!                                                                               !/*jt         13*/
!   Called by: VMNSP                                                            !/*jt         14*/
!                                                                               !/*jt         15*/
!   CEM95 written by S. G. Mashnik                                              !/*jt         16*/
!                                                                               !/*jt         17*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*jt         18*/
!   Edited by AJS, July, 1997.                                                  !/*jt         19*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*jt         20*/
!                                                                               !/*jt         21*/
! ======================================================================        !/*jt         22*/
#include <cem/cdouble.h>                                                        /*!jt         23*/
      if (ith.ne.0) then                                                        !/*jt         24*/
!   One pion produced.                                                          !/*jt         25*/
        if (mb.le.1) then                                                       !/*jt         26*/
!   pi + N incident channel:                                                    !/*jt         27*/
          if (lamb.gt.1) then                                                   !/*jt         28*/
            jtypb = 6                                                           !/*jt         29*/
          else                                                                  !/*jt         30*/
            jtypb = 5                                                           !/*jt         31*/
          endif                                                                 !/*jt         32*/
        else                                                                    !/*jt         33*/
!   N + N incident channel:                                                     !/*jt         34*/
          if (lamb.le.1 .or. lamb.eq.3) then                                    !/*jt         35*/
            jtypb = 1                                                           !/*jt         36*/
          else                                                                  !/*jt         37*/
            jtypb = 2                                                           !/*jt         38*/
          endif                                                                 !/*jt         39*/
        endif                                                                   !/*jt         40*/
      else                                                                      !/*jt         41*/
!   All other pion production cross sections:                                   !/*jt         42*/
        if (mb.le.1) then                                                       !/*jt         43*/
!   pi + N incident channel:                                                    !/*jt         44*/
          if (lamb.le.1) then                                                   !/*jt         45*/
            jtypb = 7                                                           !/*jt         46*/
          else                                                                  !/*jt         47*/
            jtypb = 8                                                           !/*jt         48*/
          endif                                                                 !/*jt         49*/
        else                                                                    !/*jt         50*/
!   N + N incident channel:                                                     !/*jt         51*/
          if (lamb.le.1 .or. lamb.eq.3) then                                    !/*jt         52*/
            jtypb = 3                                                           !/*jt         53*/
          else                                                                  !/*jt         54*/
            jtypb = 4                                                           !/*jt         55*/
          endif                                                                 !/*jt         56*/
        endif                                                                   !/*jt         57*/
      endif                                                                     !/*jt         58*/
#endif                                                                          /*!jt         59*/
      return                                                                    !/*jt         60*/
      end                                                                       !/*jt         61*/

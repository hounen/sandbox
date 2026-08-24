      function s2 (sx, bs, rs)                                                  !/*s2          1*/
! ======================================================================        !/*s2          2*/
!                                                                               !/*s2          3*/
!   r**2 * Woods-Saxon function.                                                !/*s2          4*/
!   (For integrating density radially.)                                         !/*s2          5*/
!                                                                               !/*s2          6*/
!   CEM95 written by S. G. Mashnik                                              !/*s2          7*/
!                                                                               !/*s2          8*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*s2          9*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*s2         10*/
!                                                                               !/*s2         11*/
! ======================================================================        !/*s2         12*/
#include <cem/cdouble.h>                                                        /*!s2         13*/
#ifndef CEM                                                                     /*!s2         14*/
      s2=dp0                                                                    !/*s2         15*/
#else                                                                           /*!s2         16*/
      s2 = (sx**2)/(dp1 + exp((sx - rs)/bs))                                    !/*s2         17*/
#endif                                                                          /*!s2         18*/
      return                                                                    !/*s2         19*/
      end                                                                       !/*s2         20*/

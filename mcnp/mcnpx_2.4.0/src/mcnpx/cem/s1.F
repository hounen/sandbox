      function s1 (sx, bs, rs)                                                  !/*s1          1*/
! ======================================================================        !/*s1          2*/
!                                                                               !/*s1          3*/
!   r * Woods-Saxon function                                                    !/*s1          4*/
!                                                                               !/*s1          5*/
!   CEM95 written by S. G. Mashnik                                              !/*s1          6*/
!                                                                               !/*s1          7*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*s1          8*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*s1          9*/
!                                                                               !/*s1         10*/
! ======================================================================        !/*s1         11*/
#include <cem/cdouble.h>                                                        /*!s1         12*/
#ifndef CEM                                                                     /*!s1         13*/
      s1=dp0                                                                    !/*s1         14*/
#else                                                                           /*!s1         15*/
      s1 = sx/(dp1 + exp((sx - rs)/bs))                                         !/*s1         16*/
#endif                                                                          /*!s1         17*/
      return                                                                    !/*s1         18*/
      end                                                                       !/*s1         19*/

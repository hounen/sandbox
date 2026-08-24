      function gamagu (j)                                                       !/*ga          1*/
! ======================================================================        !/*ga          2*/
!                                                                               !/*ga          3*/
!     Finds the emission width for a preequilbrium particle of type "j"         !/*ga          4*/
!     by integrating Eq. (14) of Gudima, et al. over excitation energy.         !/*ga          5*/
!                                                                               !/*ga          6*/
!     Called by: PRECOF                                                         !/*ga          7*/
!                                                                               !/*ga          8*/
!     Calls: GU8                                                                !/*ga          9*/
!                                                                               !/*ga         10*/
!     CEM95 written by S. G. Mashnik                                            !/*ga         11*/
!                                                                               !/*ga         12*/
!     Edited by A. J. Sierk   LANL  T-2,  February, 1996.                       !/*ga         13*/
!     Edited by AJS, August, 1997.                                              !/*ga         14*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*ga         15*/
!                                                                               !/*ga         16*/
! ======================================================================        !/*ga         17*/
#include <cem/cdouble.h>                                                        /*!ga         18*/
#ifndef CEM                                                                     /*!ga         19*/
      gamagu=dp0                                                                !/*ga         20*/
#else                                                                           /*!ga         21*/
      common /bl1003/ u, a, z                                                   !/*ga         22*/
      common /bl1011/ vj(6)                                                     !/*ga         23*/
      common /blbj/   bj(7)                                                     !/*ga         24*/
      external wbe                                                              !/*ga         25*/
! ======================================================================        !/*ga         26*/
      a1 = vj(j)                                                                !/*ga         27*/
!     if (j.eq.1) a1 = dp0                                                      !/*ga         28*/
      b1 = u - bj(j)                                                            !/*ga         29*/
      call gu8 (a1, b1, j, wbe, y)                                              !/*ga         30*/
      gamagu = y                                                                !/*ga         31*/
#endif                                                                          /*!ga         32*/
      return                                                                    !/*ga         33*/
      end                                                                       !/*ga         34*/

      subroutine gu8 (a, b, j, wbe, y)                                          !/*gu          1*/
#ifdef CEM                                                                      /*!gu          2*/
! ======================================================================        !/*gu          3*/
!                                                                               !/*gu          4*/
!     8 point Gaussian quadrature of function wbe.                              !/*gu          5*/
!     y = Integral from a to b of wbe(j,e) de                                   !/*gu          6*/
!                                                                               !/*gu          7*/
!     Called by: GAMAGU                                                         !/*gu          8*/
!                                                                               !/*gu          9*/
!     Calls: WBE                                                                !/*gu         10*/
!                                                                               !/*gu         11*/
!     CEM95 written by S. G. Mashnik                                            !/*gu         12*/
!                                                                               !/*gu         13*/
!     Edited by A. J. Sierk   LANL  T-2,  February, 1996.                       !/*gu         14*/
!     Edited by fcg, ORNL CPED March, 1998                                      !/*gu         15*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*gu         16*/
!                                                                               !/*gu         17*/
! ======================================================================        !/*gu         18*/
#include <cem/cdouble.h>                                                        /*!gu         19*/
      common /cohelp/ w(8), fiks(8)                                             !/*gu         20*/
! ======================================================================        !/*gu         21*/
      y = dp0                                                                   !/*gu         22*/
      d1 = dph*(b - a)                                                          !/*gu         23*/
      d2 = dph*(b + a)                                                          !/*gu         24*/
        do 10 k = 1,8                                                           !/*gu         25*/
        e = d1*fiks(k) + d2                                                     !/*gu         26*/
        y = y + w(k)*wbe(j, e)                                                  !/*gu         27*/
   10   continue                                                                !/*gu         28*/
      y = y*d1                                                                  !/*gu         29*/
#endif                                                                          /*!gu         30*/
      return                                                                    !/*gu         31*/
      end                                                                       !/*gu         32*/

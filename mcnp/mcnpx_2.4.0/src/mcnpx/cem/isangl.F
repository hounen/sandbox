      subroutine isangl                                                         !/*is          1*/
#ifdef CEM                                                                      /*!is          2*/
! ======================================================================        !/*is          3*/
!                                                                               !/*is          4*/
!     Choose random isotropic distributed angle for emitted particle            !/*is          5*/
!                                                                               !/*is          6*/
!     CEM95 written by S. G. Mashnik                                            !/*is          7*/
!                                                                               !/*is          8*/
!     Edited by A. J. Sierk   LANL  T-2,  February, 1996.                       !/*is          9*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*is         10*/
!                                                                               !/*is         11*/
! ======================================================================        !/*is         12*/
#include <cem/cdouble.h>                                                        /*!is         13*/
      common /blangl/ angl(4)                                                   !/*is         14*/
      common /pii/    pi                                                        !/*is         15*/
! ======================================================================        !/*is         16*/
      angl(1) = dp1 - dp2*rang()                                                !/*is         17*/
      angl(4) = sqrt(dp1 - angl(1)**2)                                          !/*is         18*/
      f = dp2*pi*rang()                                                         !/*is         19*/
      angl(2) = sin(f)                                                          !/*is         20*/
      angl(3) = cos(f)                                                          !/*is         21*/
#endif                                                                          /*!is         22*/
      return                                                                    !/*is         23*/
      end                                                                       !/*is         24*/

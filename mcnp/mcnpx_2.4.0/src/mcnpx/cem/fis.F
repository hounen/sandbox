      function fis (sxfi, bs, rs)                                               !/*fi          1*/
! ======================================================================        !/*fi          2*/
!                                                                               !/*fi          3*/
!   Calculates r**2 * (the Coulomb potential) as a function of r.               !/*fi          4*/
!                                                                               !/*fi          5*/
!   CEM95 written by S. G. Mashnik                                              !/*fi          6*/
!                                                                               !/*fi          7*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*fi          8*/
!   Modified by A. J. Sierk  LANL  T-2  March, 1996.                            !/*fi          9*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*fi         10*/
!                                                                               !/*fi         11*/
! ======================================================================        !/*fi         12*/
#include <cem/cdouble.h>                                                        /*!fi         13*/
#ifndef CEM                                                                     /*!fi         14*/
      fis=dp0                                                                   !/*fi         15*/
#else                                                                           /*!fi         16*/
      external s1, s2                                                           !/*fi         17*/
      common /o/ anucl, znucl, eps, vpi, rsm(10), rbig(10), rhop(10),           !/*fi         18*/
     &           rhon(10), af(10), tfp(10), tfn(10), n                          !/*fi         19*/
! ======================================================================        !/*fi         20*/
      fis = (cfint1 (s2, sxfi, dp0, bs, rs)/sxfi +                              !/*fi         21*/
     &       cfint1 (s1, rsm(n), sxfi, bs, rs))*sxfi**2                         !/*fi         22*/
#endif                                                                          /*!fi         23*/
      return                                                                    !/*fi         24*/
      end                                                                       !/*fi         25*/

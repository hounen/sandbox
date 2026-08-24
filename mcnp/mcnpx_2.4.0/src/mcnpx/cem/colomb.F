      function colomb (l, radncl, am)                                           !/*co          1*/
! ======================================================================        !/*co          2*/
!                                                                               !/*co          3*/
!***********************************************************************        !/*co          4*/
!                                                                               !/*co          5*/
!  Calculation of Coulomb barrier in charged particle channel.                  !/*co          6*/
!                                                                               !/*co          7*/
!  Called by: PRECOF                                                            !/*co          8*/
!                                                                               !/*co          9*/
!***********************************************************************        !/*co         10*/
!                                                                               !/*co         11*/
!   CEM95 written by S. G. Mashnik                                              !/*co         12*/
!                                                                               !/*co         13*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*co         14*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*co         15*/
!                                                                               !/*co         16*/
! ======================================================================        !/*co         17*/
#include <cem/cdouble.h>                                                        /*!co         18*/
#ifndef CEM                                                                     /*!co         19*/
      colomb=dp0                                                                !/*co         20*/
#else                                                                           /*!co         21*/
      common /bl1003/ u, a, z                                                   !/*co         22*/
      common /bl1005/  aj(6)                                                    !/*co         23*/
      common /bl1006/  zj(6)                                                    !/*co         24*/
      common /bl1009/ afj(7)                                                    !/*co         25*/
      common /bl1010/ zfj(6)                                                    !/*co         26*/
      common /bl1017/  vk(6)                                                    !/*co         27*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*co         28*/
! ======================================================================        !/*co         29*/
      if (l.le.1) then                                                          !/*co         30*/
!   neutron channel:                                                            !/*co         31*/
        colomb = dp0                                                            !/*co         32*/
      else                                                                      !/*co         33*/
!   charged particle channels:                                                  !/*co         34*/
        temp1 = vk(l)*(dp1439m3/radncl)                                         !/*co         35*/
        colomb = temp1*zj(l)*zfj(l)/(aj(l)**thrd + afj(l)**thrd)                !/*co         36*/
!   Thermal lowering of Coulomb barrier???                                      !/*co         37*/
        colomb = colomb*(dp1 - u/(dp81*a*am))                                   !/*co         38*/
        colomb = max(colomb, dp0)                                               !/*co         39*/
      endif                                                                     !/*co         40*/
#endif                                                                          /*!co         41*/
      return                                                                    !/*co         42*/
      end                                                                       !/*co         43*/

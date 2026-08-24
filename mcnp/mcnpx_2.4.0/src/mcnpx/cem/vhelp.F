      subroutine vhelp                                                          !/*vh          1*/
#ifdef CEM                                                                      /*!vh          2*/
! ======================================================================        !/*vh          3*/
!                                                                               !/*vh          4*/
!   Auxiliary subroutine for nuclear data extraction.                           !/*vh          5*/
!                                                                               !/*vh          6*/
!    CEM95 written by S. G. Mashnik                                             !/*vh          7*/
!                                                                               !/*vh          8*/
!    Edited by A. J. Sierk   LANL  T-2,  February, 1996.                        !/*vh          9*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*vh         10*/
!                                                                               !/*vh         11*/
! ======================================================================        !/*vh         12*/
#include <cem/cdouble.h>                                                        /*!vh         13*/
      common /bl1003/ u, a, z                                                   !/*vh         14*/
      common /bl1016/ cc(6)                                                     !/*vh         15*/
      common /bl1017/ vk(6)                                                     !/*vh         16*/
      common /stmass/ z1(5), a1(5), c1(5), a2(5), c2(5)                         !/*vh         17*/
! ======================================================================        !/*vh         18*/
      cc(1) = dp0                                                               !/*vh         19*/
      vk(1) = dp0                                                               !/*vh         20*/
      cc(2) = subev (z, z1, c1, 5)                                              !/*vh         21*/
      vk(2) = subev (z, z1, a1, 5)                                              !/*vh         22*/
      cc(6) = subev (z, z1, c2, 5)                                              !/*vh         23*/
      vk(6) = subev (z, z1, a2, 5)                                              !/*vh         24*/
      cc(3) = cc(2)/dp2                                                         !/*vh         25*/
      vk(3) = vk(2) + dp6m2                                                     !/*vh         26*/
      cc(4) = cc(2)/dp3                                                         !/*vh         27*/
      vk(4) = vk(2) + dp12m2                                                    !/*vh         28*/
      cc(5) = cc(6)*dp4th                                                       !/*vh         29*/
      vk(5) = vk(6) - dp6m2                                                     !/*vh         30*/
#endif                                                                          /*!vh         31*/
      return                                                                    !/*vh         32*/
      end                                                                       !/*vh         33*/

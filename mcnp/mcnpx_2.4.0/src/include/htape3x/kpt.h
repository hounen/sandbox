!     $Id: kpt.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                          !/*kp          1*/
!                                                                               !/*kp          2*/
!   LAHET   FLUKA        NAME                                                   !/*kp          3*/
!   INDEX   INDEX                                                               !/*kp          4*/
!     1       1        proton                                                   !/*kp          5*/
!     2       8        neutron                                                  !/*kp          6*/
!     3       13       pion+                                                    !/*kp          7*/
!     4       23       pi0                                                      !/*kp          8*/
!     5       14       pion-                                                    !/*kp          9*/
!     6       10       muon+                                                    !/*kp         10*/
!     7       11       muon-                                                    !/*kp         11*/
!     8       -        deuteron                                                 !/*kp         12*/
!     9       -        triton                                                   !/*kp         13*/
!     10      -        He-3                                                     !/*kp         14*/
!     11      -        alpha                                                    !/*kp         15*/
!     12      7        photon                                                   !/*kp         16*/
!     13      15       kaon+                                                    !/*kp         17*/
!     14      12       kaon0long                                                !/*kp         18*/
!     15      19       kaon0short                                               !/*kp         19*/
!     16      16       kaon-                                                    !/*kp         20*/
!     17      2        antiproton                                               !/*kp         21*/
!     18      9        antineutron                                              !/*kp         22*/
!     19      3        electron                                                 !/*kp         23*/
!     20      4        positron                                                 !/*kp         24*/
!     21      5        neutrino                                                 !/*kp         25*/
!     22      6        antineutrino                                             !/*kp         26*/
!                                                                               !/*kp         27*/
!   ORIGIN                                                                      !/*kp         28*/
!   INDEX                                                                       !/*kp         29*/
!     0           cascade                                                       !/*kp         30*/
!     1           particle decay                                                !/*kp         31*/
!     2           preequilibrium                                                !/*kp         32*/
!     5           prefission evap                                               !/*kp         33*/
!     6           1st fragment evap                                             !/*kp         34*/
!     7           2nd fragment evap                                             !/*kp         35*/
!                                                                               !/*kp         36*/
      character kpart*9                                                         !/*kp         37*/
      parameter (lptx=lpt+22)                                                   !/*kp         38*/
      common /kpt/ kpart(lptx)                                                  !/*kp         39*/

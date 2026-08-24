!     $Id: index2.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*in          1*/
      subroutine index2 (ka,kb,ind)                                             !/*in          2*/
!                                                                     *         !/*in          3*/
!---------------------------------------------------------------------*         !/*in          4*/
!                                                                     *         !/*in          5*/
#include <lcs/double.h>                                                         /*!in          6*/
#include <fluka89/flprm1.h>                                                     /*!in          7*/
#include <fluka89/flprm2.h>                                                     /*!in          8*/
#include <lcs/inout.h>                                                          /*!in          9*/
      kp=ka*kb                                                                  !/*in         10*/
      ks=ka+kb                                                                  !/*in         11*/
      if (kp.eq.1) ind=1                                                        !/*in         12*/
      if (kp.eq.2) ind=2                                                        !/*in         13*/
      if (kp.eq.3) ind=3                                                        !/*in         14*/
      if (kp.eq.4.and.ks.eq.5) ind=4                                            !/*in         15*/
      if (kp.eq.5) ind=5                                                        !/*in         16*/
      if (kp.eq.6.and.ks.eq.7) ind=6                                            !/*in         17*/
      if (kp.eq.4.and.ks.eq.4) ind=7                                            !/*in         18*/
      if (kp.eq.6.and.ks.eq.5) ind=8                                            !/*in         19*/
      if (kp.eq.8) ind=9                                                        !/*in         20*/
      if (kp.eq.10) ind=10                                                      !/*in         21*/
      if (kp.eq.12.and.ks.eq.8) ind=11                                          !/*in         22*/
      if (kp.eq.9) ind=12                                                       !/*in         23*/
      if (kp.eq.12.and.ks.eq.7) ind=13                                          !/*in         24*/
      if (kp.eq.15) ind=14                                                      !/*in         25*/
      if (kp.eq.18) ind=15                                                      !/*in         26*/
      if (kp.eq.16) ind=16                                                      !/*in         27*/
      if (kp.eq.20) ind=17                                                      !/*in         28*/
      if (kp.eq.24) ind=18                                                      !/*in         29*/
      if (kp.eq.25) ind=19                                                      !/*in         30*/
      if (kp.eq.30) ind=20                                                      !/*in         31*/
      if (kp.eq.36) ind=21                                                      !/*in         32*/
      return                                                                    !/*in         33*/
      end                                                                       !/*in         34*/

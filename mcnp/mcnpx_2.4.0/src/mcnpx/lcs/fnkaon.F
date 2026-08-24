!     $Id: fnkaon.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                       !/*fn          1*/
      function fnkaon (plab)                                                    !/*fn          2*/
#include <lcs/double.h>                                                         /*!fn          3*/
!     ==================================================================        !/*fn          4*/
!     fnkaon calculates the fraction of pbar annihilations that                 !/*fn          5*/
!     go into a kaon branch, as function of pbar (lab) momentum                 !/*fn          6*/
!     plab is in [mev/c], not [gev/c]                                           !/*fn          7*/
!                                                                               !/*fn          8*/
! FNKAON called by GAMMA                                                        !/*fn          9*/
!     ==================================================================        !/*fn         10*/
!                                                                               !/*fn         11*/
!        linear coefficients are based on percentages of the                    !/*fn         12*/
!      total annihilation sigma to kaon sigma, which in turn                    !/*fn         13*/
!      derive from data in                                                      !/*fn         14*/
!       armenteros & french, high energy physics iv (1969) p 237                !/*fn         15*/
!       xuong & lynch, pr   128(1962) 1849                                      !/*fn         16*/
!       french et al, nuevo cimento, 52a(1967) 438                              !/*fn         17*/
!                                                                               !/*fn         18*/
      fnkaon=0.067d0+0.0253d0*plab*1.d-03                                       !/*fn         19*/
      return                                                                    !/*fn         20*/
      end                                                                       !/*fn         21*/

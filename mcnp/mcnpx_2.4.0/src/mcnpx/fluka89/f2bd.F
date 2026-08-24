!     $Id: f2bd.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                         !/*f2          1*/
      block data f2bd                                                           !/*f2          2*/
#include <fluka89/table1.h>                                                     /*!f2          3*/
#include <lcs/double.h>                                                         /*!f2          4*/
#include <lcs/lptS.h>                                                           /*!f2          5*/
#include <lcs/fltolh.h>                                                         /*!f2          6*/
#include <fluka89/inpdt2.h>                                                     /*!f2          7*/
#include <fluka89/remain.h>                                                     /*!f2          8*/
#include <fluka89/bamjcm.h>                                                     /*!f2          9*/
!----------------------------------------------------------------------*        !/*f2         10*/
!--- inpdat -----                                                               !/*f2         11*/
      data a1, b1, b2, b3, bet, as, b8, ame, diq /9*dp0/                        !/*f2         12*/
      data imps, imve /36*0,36*0/                                               !/*f2         13*/
      data ib08, ib10, ia08, ia10 /126*0,126*0,126*0,126*0/                     !/*f2         14*/
      data lt, le, isu /1,2*0/                                                  !/*f2         15*/
!--- remain -----                                                               !/*f2         16*/
      data rpxr, rpyr, rpzr, rer /4*dp0/                                        !/*f2         17*/
      data kr1r, kr2r /2*0/                                                     !/*f2         18*/
!--- bamjcm -----                                                               !/*f2         19*/
      data iv /kmxjcm*0/, kfr1 /kmxjcm*0/, kfr2 /kmxjcm*0/                      !/*f2         20*/
      data rpx /kmxjcm*dp0/, rpy /kmxjcm*dp0/, re /kmxjcm*dp0/                  !/*f2         21*/
!                                                                      *        !/*f2         22*/
! /fltolh/                                                             *        !/*f2         23*/
      data itypfl /1,8,13,23,14,10,11,4*0,7,15,12,19,16,2,9,3,4,5,6/            !/*f2         24*/
      data ityplh /1,17,19,20,21,22,12,2,18,6,7,14,3,5,13,16,2*0,15,3*0,        !/*f2         25*/
     & 4,7*0/                                                                   !/*f2         26*/
      data ilahet /0,16,18,19,20,21,11,1,17,5,6,13,2,4,12,15,99,99,14,99        !/*f2         27*/
     & ,99,99,3,13,14,125*100/                                                  !/*f2         28*/
      end                                                                       !/*f2         29*/

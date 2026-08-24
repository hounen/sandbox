!     $Id: cmc.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                          !/*cm          1*/
      subroutine cmc (cm7,cm8,cm9,cm10)                                         !/*cm          2*/
#include <lcs/double.h>                                                         /*!cm          3*/
!     ==================================================================        !/*cm          4*/
!     cmc is part of the center of mass transformation. the particles           !/*cm          5*/
!     after collision are brought back into the laboratory system               !/*cm          6*/
!                                                                               !/*cm          7*/
! CMC    calls   SQRT                                                           !/*cm          8*/
! called by AIOUT  GAMMA                                                        !/*cm          9*/
!     ==================================================================        !/*cm         10*/
!                                                                               !/*cm         11*/
#include <lcs/vegas.h>                                                          /*!cm         12*/
      cm2(8)=cm7*cm1(19)                                                        !/*cm         13*/
      cm2(4)=cm10*cm1(20)+cm2(8)                                                !/*cm         14*/
      cm2(5)=cm8                                                                !/*cm         15*/
      cm2(6)=cm9                                                                !/*cm         16*/
      cm2(8)=cm7*cm1(20)                                                        !/*cm         17*/
      cm2(3)=cm10*cm1(19)+cm2(8)                                                !/*cm         18*/
!                                                                               !/*cm         19*/
      cm2(9)=cm2(4)*cm1(10)+cm2(5)*cm1(11)+cm2(6)*cm1(12)                       !/*cm         20*/
      cm2(10)=cm2(4)*cm1(13)+cm2(5)*cm1(14)+cm2(6)*cm1(15)                      !/*cm         21*/
      cm2(11)=cm2(4)*cm1(16)+cm2(5)*cm1(17)+cm2(6)*cm1(18)                      !/*cm         22*/
!                                                                               !/*cm         23*/
      cm2(4)=cm2(9)*cm1(1)+cm2(10)*cm1(2)+cm2(11)*cm1(3)                        !/*cm         24*/
      cm2(5)=cm2(9)*cm1(4)+cm2(10)*cm1(5)+cm2(11)*cm1(6)                        !/*cm         25*/
      cm2(6)=cm2(9)*cm1(7)+cm2(10)*cm1(8)+cm2(11)*cm1(9)                        !/*cm         26*/
!                                                                               !/*cm         27*/
      cm2(8)=cm2(4)**2+cm2(5)**2+cm2(6)**2                                      !/*cm         28*/
      cm2(2)=sqrt(cm2(8))                                                       !/*cm         29*/
      return                                                                    !/*cm         30*/
      end                                                                       !/*cm         31*/

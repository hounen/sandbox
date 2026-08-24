!     $Id: idk.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                          !/*id          1*/
      subroutine idk                                                            !/*id          2*/
#include <lcs/double.h>                                                         /*!id          3*/
#include <lcs/btcom.h>                                                          /*!id          4*/
      univ=pnidk(6)*pnidk(6)                                                    !/*id          5*/
!     m(p1) squared  decay pion mass squared                                    !/*id          6*/
      pnidk(7)=(pnidk(1)*pnidk(1)+univ-sqnm)/(dp2*pnidk(1))                     !/*id          7*/
!     e(pi)prime  decay pion energy prime                                       !/*id          8*/
      pnidk(8)=sqrt(pnidk(7)*pnidk(7)-univ)                                     !/*id          9*/
!     decay pion momentum prime  p(d)                                           !/*id         10*/
      call polrn (pnidk(20),pnidk(21))                                          !/*id         11*/
!     cos theta, sin theta                                                      !/*id         12*/
      call azirn (pnidk(22),pnidk(23))                                          !/*id         13*/
!     cos phi, sin phi                                                          !/*id         14*/
      pnidk(9)=pnidk(22)*pnidk(21)*pnidk(8)                                     !/*id         15*/
!     decay pion x momentum component prime                                     !/*id         16*/
      pnidk(10)=pnidk(21)*pnidk(23)*pnidk(8)                                    !/*id         17*/
!     p(p1)prime y                                                              !/*id         18*/
      pnidk(11)=pnidk(20)*pnidk(8)                                              !/*id         19*/
!     p(p1)prime z                                                              !/*id         20*/
      univ=pnidk(9)*pnidk(2)+pnidk(10)*pnidk(3)+pnidk(11)*pnidk(4)              !/*id         21*/
!     p p1 prime dot p                                                          !/*id         22*/
      pnidk(12)=(pnidk(7)*pnidk(5)+univ)/pnidk(1)                               !/*id         23*/
!     decay pion energy  e(pi)                                                  !/*id         24*/
      pnidk(13)=pnidk(5)-pnidk(12)                                              !/*id         25*/
      univ=(((pnidk(5)/pnidk(1))-dp1)*univ)/(pnidk(2)*pnidk(2)+pnidk(3)         !/*id         26*/
     & *pnidk(3)+pnidk(4)*pnidk(4))                                             !/*id         27*/
!     (e/m-dp1)*p(p1)prime dot p/p squared                                      !/*id         28*/
      unive=pnidk(7)/pnidk(1)                                                   !/*id         29*/
!     e pi prime over mass                                                      !/*id         30*/
      do 10 i=2,4                                                               !/*id         31*/
      pnidk(i+12)=pnidk(i)*(univ+unive)+pnidk(i+7)                              !/*id         32*/
   10 pnidk(i+15)=pnidk(i)-pnidk(i+12)                                          !/*id         33*/
      return                                                                    !/*id         34*/
!     pion momentum components and nucleon momentum                             !/*id         35*/
!     components                                                                !/*id         36*/
      end                                                                       !/*id         37*/

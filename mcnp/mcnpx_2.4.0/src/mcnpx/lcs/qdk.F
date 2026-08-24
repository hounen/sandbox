!     $Id: qdk.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                          !/*qd          1*/
      subroutine qdk                                                            !/*qd          2*/
! QDK        calls...  SQRT  POLRN  AZIRN                                       !/*qd          3*/
!     QDK    called by...   QOU18                                               !/*qd          4*/
#include <lcs/double.h>                                                         /*!qd          5*/
#include <lcs/isobar.h>                                                         /*!qd          6*/
      univ_=pnidk_(6)*pnidk_(6)                                                 !/*qd          7*/
!     m(p1) squared  decay pion mass squared                                    !/*qd          8*/
      pnidk_(7)=(pnidk_(1)*pnidk_(1)+univ_-sqnm_)/(dp2*pnidk_(1))               !/*qd          9*/
!     e(pi)prime  decay pion energy prime                                       !/*qd         10*/
      pnidk_(8)=sqrt(pnidk_(7)*pnidk_(7)-univ_)                                 !/*qd         11*/
!     decay pion momentum prime  p(d)                                           !/*qd         12*/
      call polrn (pnidk_(20),pnidk_(21))                                        !/*qd         13*/
!     cos theta, sin theta                                                      !/*qd         14*/
      call azirn (pnidk_(22),pnidk_(23))                                        !/*qd         15*/
!     cos phi, sin phi                                                          !/*qd         16*/
      pnidk_(9)=pnidk_(22)*pnidk_(21)*pnidk_(8)                                 !/*qd         17*/
!     decay pion x momentum component prime                                     !/*qd         18*/
      pnidk_(10)=pnidk_(21)*pnidk_(23)*pnidk_(8)                                !/*qd         19*/
!     p(p1)prime y                                                              !/*qd         20*/
      pnidk_(11)=pnidk_(20)*pnidk_(8)                                           !/*qd         21*/
!     p(p1)prime z                                                              !/*qd         22*/
      univ_=pnidk_(9)*pnidk_(2)+pnidk_(10)*pnidk_(3)+pnidk_(11)*pnidk_(4        !/*qd         23*/
     & )                                                                        !/*qd         24*/
!     p p1 prime dot p                                                          !/*qd         25*/
      pnidk_(12)=(pnidk_(7)*pnidk_(5)+univ_)/pnidk_(1)                          !/*qd         26*/
!     decay pion energy  e(pi)                                                  !/*qd         27*/
      pnidk_(13)=pnidk_(5)-pnidk_(12)                                           !/*qd         28*/
      univ_=(((pnidk_(5)/pnidk_(1))-dp1)*univ_)/(pnidk_(2)*pnidk_(2)            !/*qd         29*/
     & +pnidk_(3)*pnidk_(3)+pnidk_(4)*pnidk_(4))                                !/*qd         30*/
!     (e/m-dp1)*p(p1)prime dot p/p squared                                      !/*qd         31*/
      unive_=pnidk_(7)/pnidk_(1)                                                !/*qd         32*/
!     e pi prime over mass                                                      !/*qd         33*/
      do 10 i=2,4                                                               !/*qd         34*/
      pnidk_(i+12)=pnidk_(i)*(univ_+unive_)+pnidk_(i+7)                         !/*qd         35*/
   10 pnidk_(i+15)=pnidk_(i)-pnidk_(i+12)                                       !/*qd         36*/
      return                                                                    !/*qd         37*/
!     pion momentum components and nucleon momentum                             !/*qd         38*/
!     components                                                                !/*qd         39*/
      end                                                                       !/*qd         40*/

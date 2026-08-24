!     $Id: cma.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                          !/*cm          1*/
      subroutine cma                                                            !/*cm          2*/
#include <lcs/double.h>                                                         /*!cm          3*/
!     ==================================================================        !/*cm          4*/
!      cma is part of the center of mass transformation as outlined             !/*cm          5*/
!      by metropolis, etc, in lams-2360. the other center of mass routin        !/*cm          6*/
!      are cmb, cmc, and elas.  cma brings the particle into the                !/*cm          7*/
!      center of mass system.                                                   !/*cm          8*/
!                                                                               !/*cm          9*/
! CMA    calls   SQRT                                                           !/*cm         10*/
! called by GAMMA                                                               !/*cm         11*/
!     ==================================================================        !/*cm         12*/
!                                                                               !/*cm         13*/
#include <lcs/vegas.h>                                                          /*!cm         14*/
      cm(15)=ws(4)*wsp(4)                                                       !/*cm         15*/
      cm(16)=gws(5)*wsp(5)                                                      !/*cm         16*/
!                          cm15 = m1*m2,   cm16 = e1*e2                         !/*cm         17*/
      gws3=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*cm         18*/
      wsp3=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                                    !/*cm         19*/
!                        gws3 = p1lab,    wsp3 = p2lab                          !/*cm         20*/
      cm(25)=(cm(16)-wsp(15)*gws3*wsp3)/cm(15)                                  !/*cm         21*/
!                             cm25 = q1.q2 / (m1*m2)                            !/*cm         22*/
      cm(1)=sqrt(cm(25)*cm(25)-.99999d0)                                        !/*cm         23*/
      cm(2)=gws(5)+wsp(5)                                                       !/*cm         24*/
!                          cm2 = e1+e2                                          !/*cm         25*/
      cm(16)=cm(2)*cm(2)                                                        !/*cm         26*/
      cm(17)=cm(25)*cm(15)                                                      !/*cm         27*/
!                         cm16 = (e1+2)**2,    cm17 = (q1.q2)**2                !/*cm         28*/
      cm(18)=ws(4)*ws(4)                                                        !/*cm         29*/
      cm(20)=cm(18)+cm(17)                                                      !/*cm         30*/
!               cm18 = m1**2,   cm20 = m1**2 + (q1.q2)**2                       !/*cm         31*/
      cm(19)=wsp(4)*wsp(4)                                                      !/*cm         32*/
      cm(21)=cm(19)+cm(17)                                                      !/*cm         33*/
!               cm19 = m2**2,   cm21 = m2**2 + (q1.q2)**2                       !/*cm         34*/
      cm(22)=cm(21)+cm(20)                                                      !/*cm         35*/
      cm(3)=sqrt(cm(22))                                                        !/*cm         36*/
!               cm22 = ecm**2,       cm3 = ecm                                  !/*cm         37*/
      cm(4)=sqrt(cm(16)-cm(22))                                                 !/*cm         38*/
!               cm4 = plab(tot) = sqrt( (e1+e2)**2 - ecm**2 )                   !/*cm         39*/
      cm(22)=cm(4)*cm(1)                                                        !/*cm         40*/
      cm(16)=cm(25)*(gws(5)-wsp(5))                                             !/*cm         41*/
      cm(17)=wsp(5)*cm(18)                                                      !/*cm         42*/
      cm(5)=(cm(16)+(gws(5)*cm(19)-cm(17))/cm(15))/cm(22)                       !/*cm         43*/
      cm(6)=-sqrt(1.00001d0-cm(5)*cm(5))                                        !/*cm         44*/
      cm(16)=cm(1)*cm(15)/cm(3)                                                 !/*cm         45*/
      cm(7)=cm(16)*cm(5)                                                        !/*cm         46*/
      cm(11)=-cm(7)                                                             !/*cm         47*/
      cm(9)=cm(16)*cm(6)                                                        !/*cm         48*/
      cm(13)=-cm(9)                                                             !/*cm         49*/
      cm(10)=cm(20)/cm(3)                                                       !/*cm         50*/
      cm(14)=cm(21)/cm(3)                                                       !/*cm         51*/
!            cm10 = ecm1,      cm14 = ecm2                                      !/*cm         52*/
      cm(25)=(wsp(15)*wsp3+gws3)/cm(4)                                          !/*cm         53*/
      cm(1)=-sqrt(1.00001d0-cm(25)*cm(25))                                      !/*cm         54*/
      cm(8)=dp0                                                                 !/*cm         55*/
      cm(12)=dp0                                                                !/*cm         56*/
      return                                                                    !/*cm         57*/
      end                                                                       !/*cm         58*/

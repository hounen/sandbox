!     $Id: cmb.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                          !/*cm          1*/
      subroutine cmb                                                            !/*cm          2*/
#include <lcs/double.h>                                                         /*!cm          3*/
!     ==================================================================        !/*cm          4*/
!      cmb is part of the center of mass transformation routine.                !/*cm          5*/
!      it sets up the 4 inverse matrices.                                       !/*cm          6*/
!                                                                               !/*cm          7*/
! CMB    calls   SQRT  ATAN%  FLTRNF  COS                                       !/*cm          8*/
! called by AIOUT  GAMMA                                                        !/*cm          9*/
!     ==================================================================        !/*cm         10*/
!                                                                               !/*cm         11*/
#include <lcs/vegas.h>                                                          /*!cm         12*/
      ws(8)=abs(gws(8))                                                         !/*cm         13*/
      ws8=ws(8)                                                                 !/*cm         14*/
      if (ws8+1.d-07-dp1) 10,80,80                                              !/*cm         15*/
   10 cm1(7)=gws(8)                                                             !/*cm         16*/
      cm1(9)=sqrt(dp1-gws(8)*gws(8))                                            !/*cm         17*/
      cm1(1)=gws(6)                                                             !/*cm         18*/
      cm1(5)=cm1(1)/cm1(9)                                                      !/*cm         19*/
      cm1(3)=-cm1(5)*cm1(7)                                                     !/*cm         20*/
      cm1(4)=gws(7)                                                             !/*cm         21*/
      cm1(2)=-cm1(4)/cm1(9)                                                     !/*cm         22*/
      cm1(6)=cm1(2)*cm1(7)                                                      !/*cm         23*/
      cm1(8)=dp0                                                                !/*cm         24*/
   20 continue                                                                  !/*cm         25*/
      if (ind(15).eq.1) go to 120                                               !/*cm         26*/
! if isobar decay phi and omega rotations are changed                           !/*cm         27*/
      if (ind(32).eq.1) go to 40                                                !/*cm         28*/
      tanfn=ws(6)*wsp(7)-ws(7)*wsp(6)                                           !/*cm         29*/
      tanfd=ws(8)*wsp(15)-wsp(8)                                                !/*cm         30*/
      tanfi=tanfn/tanfd                                                         !/*cm         31*/
      fi=atan(tanfi)                                                            !/*cm         32*/
      if (tanfn.le.dp0) go to 30                                                !/*cm         33*/
      fi=fi+dppi                                                                !/*cm         34*/
      if (tanfi.lt.dp0) fi=fi+dppi                                              !/*cm         35*/
      cm1(21)=fi                                                                !/*cm         36*/
      go to 50                                                                  !/*cm         37*/
   30 if (tanfi.lt.dp0) fi=fi+dppi                                              !/*cm         38*/
      cm1(21)=fi                                                                !/*cm         39*/
      go to 50                                                                  !/*cm         40*/
   40 continue                                                                  !/*cm         41*/
      rand=rang()                                                               !/*cm         42*/
      cm1(21)=rand*6.28318d0                                                    !/*cm         43*/
   50 cm1(14)=cos(cm1(21))                                                      !/*cm         44*/
      cm1(17)=sqrt(dp1-cm1(14)*cm1(14))                                         !/*cm         45*/
      if (cm1(21)-dppi) 70,60,60                                                !/*cm         46*/
   60 cm1(17)=-cm1(17)                                                          !/*cm         47*/
   70 cm1(13)=cm1(17)*cm(1)                                                     !/*cm         48*/
      cm1(12)=cm(1)                                                             !/*cm         49*/
      cm1(16)=-cm(1)*cm1(14)                                                    !/*cm         50*/
      cm1(10)=cm(25)                                                            !/*cm         51*/
      cm1(15)=-cm(25)*cm1(17)                                                   !/*cm         52*/
      cm1(18)=cm(25)*cm1(14)                                                    !/*cm         53*/
      cm1(11)=dp0                                                               !/*cm         54*/
      cm1(19)=cm(2)/cm(3)                                                       !/*cm         55*/
      cm1(20)=cm(4)/cm(3)                                                       !/*cm         56*/
      return                                                                    !/*cm         57*/
   80 if (gws(8)) 100,90,90                                                     !/*cm         58*/
   90 cm1(7)=dp1                                                                !/*cm         59*/
      cm1(5)=dp1                                                                !/*cm         60*/
      cm1(3)=-dp1                                                               !/*cm         61*/
      go to 110                                                                 !/*cm         62*/
  100 cm1(3)=dp1                                                                !/*cm         63*/
      cm1(5)=dp1                                                                !/*cm         64*/
      cm1(7)=-dp1                                                               !/*cm         65*/
  110 cm1(1)=dp0                                                                !/*cm         66*/
      cm1(2)=dp0                                                                !/*cm         67*/
      cm1(4)=dp0                                                                !/*cm         68*/
      cm1(6)=dp0                                                                !/*cm         69*/
      cm1(9)=dp0                                                                !/*cm         70*/
      cm1(8)=dp0                                                                !/*cm         71*/
      go to 20                                                                  !/*cm         72*/
  120 continue                                                                  !/*cm         73*/
      if (ws(26).eq.dp10) go to 130                                             !/*cm         74*/
      cm1(14)=ws(26)                                                            !/*cm         75*/
      cm1(17)=ws(27)                                                            !/*cm         76*/
      go to 70                                                                  !/*cm         77*/
  130 continue                                                                  !/*cm         78*/
      cm1(14)=dp1                                                               !/*cm         79*/
      cm1(17)=dp0                                                               !/*cm         80*/
      go to 70                                                                  !/*cm         81*/
      end                                                                       !/*cm         82*/

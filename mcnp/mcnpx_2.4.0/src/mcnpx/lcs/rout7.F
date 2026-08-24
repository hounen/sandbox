!     $Id: rout7.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout7                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      i3=0                                                                      !/*ro          5*/
      if (curr(1)-dp3) 20,50,10                                                 !/*ro          6*/
   10 if (curr(1)-dp5) 60,40,20                                                 !/*ro          7*/
!     proton, neutron not permitted                                             !/*ro          8*/
   20 i3=-1                                                                     !/*ro          9*/
   30 return                                                                    !/*ro         10*/
   40 it=7                                                                      !/*ro         11*/
      ifca=5                                                                    !/*ro         12*/
!     pi meson - (5)                                                            !/*ro         13*/
      absec=pmac(med)                                                           !/*ro         14*/
!     pim +pp abs.  tyor=pmapp(20021)                                           !/*ro         15*/
      go to 90                                                                  !/*ro         16*/
   50 it=10                                                                     !/*ro         17*/
      ifca=3                                                                    !/*ro         18*/
!     tyor=ppan(20004)  pip-nn abs. energy correction pimeson +                 !/*ro         19*/
      absec=ppan(med)                                                           !/*ro         20*/
      go to 100                                                                 !/*ro         21*/
   60 continue                                                                  !/*ro         22*/
      value1=rang()                                                             !/*ro         23*/
      if (value1-ppnna) 70,80,80                                                !/*ro         24*/
   70 it=8                                                                      !/*ro         25*/
      ifca=4                                                                    !/*ro         26*/
!     pnann(20015)=tyor  pin-nn abs  pimeson 0                                  !/*ro         27*/
      absec=-hvn(med)                                                           !/*ro         28*/
      go to 100                                                                 !/*ro         29*/
   80 it=9                                                                      !/*ro         30*/
      ifca=2                                                                    !/*ro         31*/
!     pnapp(20011)=tyor  pin+pp abs.  pimeson 0                                 !/*ro         32*/
      absec=-hvp(med)                                                           !/*ro         33*/
   90 strkp=-dp1                                                                !/*ro         34*/
      e(1)=wkrpn(med)*rcpmv+pm(1)                                               !/*ro         35*/
      go to 110                                                                 !/*ro         36*/
  100 strkp=-dp2                                                                !/*ro         37*/
      e(1)=wkrpn(med+3)*rcpmv+pm(1)                                             !/*ro         38*/
  110 continue                                                                  !/*ro         39*/
      p1oe1=sqrt(e(1)*e(1)-pm(1)*pm(1))                                         !/*ro         40*/
      if (inc.ne.0) go to 120                                                   !/*ro         41*/
      pxyz(1)=p1oe1*curr(7)                                                     !/*ro         42*/
      pxyz(5)=p1oe1*curr(8)                                                     !/*ro         43*/
      pxyz(9)=p1oe1*curr(9)                                                     !/*ro         44*/
      go to 130                                                                 !/*ro         45*/
  120 continue                                                                  !/*ro         46*/
      pxyz(1)=dp0                                                               !/*ro         47*/
      pxyz(5)=dp0                                                               !/*ro         48*/
      pxyz(9)=p1oe1                                                             !/*ro         49*/
  130 continue                                                                  !/*ro         50*/
      p1oe1=p1oe1/e(1)                                                          !/*ro         51*/
      go to 30                                                                  !/*ro         52*/
      end                                                                       !/*ro         53*/

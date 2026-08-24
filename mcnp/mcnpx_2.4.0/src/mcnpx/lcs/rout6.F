!     $Id: rout6.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout6                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      if (i3) 10,90,90                                                          !/*ro          5*/
   10 aabs=dp1                                                                  !/*ro          6*/
      med=clsm                                                                  !/*ro          7*/
      knot=not                                                                  !/*ro          8*/
      value1=rang()                                                             !/*ro          9*/
      if (isw(11)) 50,20,50                                                     !/*ro         10*/
   20 if (value1-ppmda) 60,40,40                                                !/*ro         11*/
!     prob. pim-deut abs.                                                       !/*ro         12*/
   30 return                                                                    !/*ro         13*/
   40 i3=1                                                                      !/*ro         14*/
      go to 30                                                                  !/*ro         15*/
   50 if (value1-pppda) 60,40,40                                                !/*ro         16*/
!     prob. pip-deut abs.                                                       !/*ro         17*/
   60 if (isw(11)) 80,70,80                                                     !/*ro         18*/
   70 it=13                                                                     !/*ro         19*/
      absec=pmac(med)                                                           !/*ro         20*/
      go to 90                                                                  !/*ro         21*/
   80 it=14                                                                     !/*ro         22*/
      absec=-hvn(med)                                                           !/*ro         23*/
   90 strkp=-dp1                                                                !/*ro         24*/
      i1=0                                                                      !/*ro         25*/
      i2=med                                                                    !/*ro         26*/
! subroutine bb coded inline                                                    !/*ro         27*/
      i=i2                                                                      !/*ro         28*/
!     collision cut-off energy (proton)                                         !/*ro         29*/
!     if(i1.eq.0) go to 810                                                     !/*ro         30*/
!     i=i+3                                                                     !/*ro         31*/
!     collision cut-off energy (neutron)                                        !/*ro         32*/
! BB coded inline                                                               !/*ro         33*/
      if (knot-6) 110,110,100                                                   !/*ro         34*/
  100 if (knot-12) 120,120,110                                                  !/*ro         35*/
  110 clcfe=cfepn(i)                                                            !/*ro         36*/
  120 e(1)=wkrpn(i)*rcpmv+pm(1)                                                 !/*ro         37*/
!     total energy particle 1                                                   !/*ro         38*/
      p1oe1=sqrt(e(1)*e(1)-pm(1)*pm(1))                                         !/*ro         39*/
      if (in1.eq.0) go to 130                                                   !/*ro         40*/
      pxyz(1)=p1oe1*curr(7)                                                     !/*ro         41*/
      pxyz(5)=p1oe1*curr(8)                                                     !/*ro         42*/
      pxyz(9)=p1oe1*curr(9)                                                     !/*ro         43*/
      go to 140                                                                 !/*ro         44*/
  130 continue                                                                  !/*ro         45*/
      pxyz(1)=dp0                                                               !/*ro         46*/
      pxyz(9)=p1oe1                                                             !/*ro         47*/
      pxyz(5)=dp0                                                               !/*ro         48*/
  140 continue                                                                  !/*ro         49*/
      p1oe1=p1oe1/e(1)                                                          !/*ro         50*/
      i3=0                                                                      !/*ro         51*/
      go to 30                                                                  !/*ro         52*/
      end                                                                       !/*ro         53*/

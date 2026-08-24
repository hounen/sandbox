!     $Id: rou18.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou18                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      go to (10,20,80,210,170,190), i3                                          !/*ro          5*/
   10 i=3                                                                       !/*ro          6*/
      col(15)=dp1                                                               !/*ro          7*/
      k=27                                                                      !/*ro          8*/
      go to 30                                                                  !/*ro          9*/
   20 i=3                                                                       !/*ro         10*/
      col(15)=dp4                                                               !/*ro         11*/
      k=15                                                                      !/*ro         12*/
   30 pnidk(1)=pm(i)                                                            !/*ro         13*/
      j=i                                                                       !/*ro         14*/
      do 40 l=2,4                                                               !/*ro         15*/
      pnidk(l)=pxyz(j)                                                          !/*ro         16*/
   40 j=j+4                                                                     !/*ro         17*/
      pnidk(5)=e(i)                                                             !/*ro         18*/
      pnidk(6)=pt(k-11)                                                         !/*ro         19*/
      call idk                                                                  !/*ro         20*/
      if (k-27) 60,50,60                                                        !/*ro         21*/
   50 pt(15)=pt(15)+((pnidk(12)-pnidk(6))/rcpmv)                                !/*ro         22*/
   60 pt(k)=pt(k)+((pnidk(13)-dncms)/rcpmv)                                     !/*ro         23*/
      i3=1                                                                      !/*ro         24*/
   70 iv=k                                                                      !/*ro         25*/
      return                                                                    !/*ro         26*/
   80 k=3                                                                       !/*ro         27*/
      col(15)=dp2                                                               !/*ro         28*/
      if (pt(2)-dp3) 170,90,90                                                  !/*ro         29*/
   90 if (pt(k)-25.d+02) 110,110,100                                            !/*ro         30*/
  100 i3=5                                                                      !/*ro         31*/
      go to 70                                                                  !/*ro         32*/
  110 if (pt(k)) 150,150,120                                                    !/*ro         33*/
  120 ccofe=eco(1)                                                              !/*ro         34*/
      if (pt(k-1)-dp4) 140,130,130                                              !/*ro         35*/
  130 ccofe=ccofe-ctofe+ctofen                                                  !/*ro         36*/
  140 if (pt(k)-ccofe) 150,150,160                                              !/*ro         37*/
  150 m=pt(k-1)                                                                 !/*ro         38*/
      pnbc(m)=pnbc(m)+dp1                                                       !/*ro         39*/
      pt(k)=dp0                                                                 !/*ro         40*/
      i3=3                                                                      !/*ro         41*/
      go to 70                                                                  !/*ro         42*/
  160 if (k-3) 170,170,210                                                      !/*ro         43*/
  170 col(15)=dp3                                                               !/*ro         44*/
      k=15                                                                      !/*ro         45*/
      if (pt(14)-dp2) 180,180,90                                                !/*ro         46*/
  180 i3=2                                                                      !/*ro         47*/
      go to 70                                                                  !/*ro         48*/
  190 l=14                                                                      !/*ro         49*/
      do 200 m=5,7                                                              !/*ro         50*/
      pt(m)=pnidk(l)                                                            !/*ro         51*/
      pt(m+12)=pnidk(l+3)                                                       !/*ro         52*/
  200 l=l+1                                                                     !/*ro         53*/
      pt(11)=pnidk(12)                                                          !/*ro         54*/
      pt(12)=pnidk(6)                                                           !/*ro         55*/
      i=4                                                                       !/*ro         56*/
      k=39                                                                      !/*ro         57*/
      col(15)=dp5                                                               !/*ro         58*/
      go to 30                                                                  !/*ro         59*/
  210 i1=3                                                                      !/*ro         60*/
  220 k=12*i1-33                                                                !/*ro         61*/
      if (i1-4) 230,240,250                                                     !/*ro         62*/
  230 i2=-1                                                                     !/*ro         63*/
      go to 280                                                                 !/*ro         64*/
  240 i2=0                                                                      !/*ro         65*/
      go to 280                                                                 !/*ro         66*/
  250 if (i1-5) 240,270,260                                                     !/*ro         67*/
  260 i3=4                                                                      !/*ro         68*/
      go to 70                                                                  !/*ro         69*/
  270 i2=1                                                                      !/*ro         70*/
  280 if (pt(k)) 290,300,290                                                    !/*ro         71*/
  290 call pstor                                                                !/*ro         72*/
  300 i1=i1+1                                                                   !/*ro         73*/
      go to 220                                                                 !/*ro         74*/
      end                                                                       !/*ro         75*/

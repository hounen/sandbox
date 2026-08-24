!     $Id: alpha.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                        !/*al          1*/
      subroutine alpha                                                          !/*al          2*/
#include <lcs/double.h>                                                         /*!al          3*/
#include <lcs/btcom.h>                                                          /*!al          4*/
      univ=rang()                                                               !/*al          5*/
      if (value3) 290,10,140                                                    !/*al          6*/
   10 if (univ-value1) 20,20,120                                                !/*al          7*/
   20 if (isw(11)) 40,30,40                                                     !/*al          8*/
   30 pt(2)=dp5                                                                 !/*al          9*/
      pt(26)=dp2                                                                !/*al         10*/
   40 pt(4)=pnms                                                                !/*al         11*/
      pm(4)=pnms                                                                !/*al         12*/
      univ=rang()                                                               !/*al         13*/
      if (univ-value2) 50,50,70                                                 !/*al         14*/
   50 pt(14)=dp4                                                                !/*al         15*/
   60 return                                                                    !/*al         16*/
   70 if (isw(11)) 110,80,110                                                   !/*al         17*/
   80 pt(26)=dp1                                                                !/*al         18*/
   90 pt(14)=dp5                                                                !/*al         19*/
  100 pt(16)=pnms                                                               !/*al         20*/
      go to 60                                                                  !/*al         21*/
  110 pt(26)=dp2                                                                !/*al         22*/
      go to 100                                                                 !/*al         23*/
  120 pt(2)=dp4                                                                 !/*al         24*/
      if (isw(11)) 100,130,100                                                  !/*al         25*/
  130 pt(14)=dp5                                                                !/*al         26*/
      go to 110                                                                 !/*al         27*/
  140 if (univ-value1) 150,150,190                                              !/*al         28*/
  150 pm(4)=pnms                                                                !/*al         29*/
      if (isw(11)) 160,180,160                                                  !/*al         30*/
  160 pt(2)=dp5                                                                 !/*al         31*/
  170 pt(16)=pnms                                                               !/*al         32*/
      pt(4)=pnms                                                                !/*al         33*/
      go to 60                                                                  !/*al         34*/
  180 pt(14)=dp5                                                                !/*al         35*/
      pt(26)=dp2                                                                !/*al         36*/
      go to 170                                                                 !/*al         37*/
  190 if (univ-value2) 200,200,240                                              !/*al         38*/
  200 pt(2)=dp4                                                                 !/*al         39*/
      univ=rang()                                                               !/*al         40*/
      if (univ-value3) 230,230,210                                              !/*al         41*/
  210 if (isw(11)) 50,220,50                                                    !/*al         42*/
  220 pt(26)=dp2                                                                !/*al         43*/
      go to 50                                                                  !/*al         44*/
  230 if (isw(11)) 110,90,110                                                   !/*al         45*/
  240 pm(4)=pnms                                                                !/*al         46*/
      pt(4)=pnms                                                                !/*al         47*/
      univ=rang()                                                               !/*al         48*/
      if (univ-6.6666667d-1) 250,250,270                                        !/*al         49*/
  250 if (isw(11)) 220,260,220                                                  !/*al         50*/
  260 pt(2)=dp5                                                                 !/*al         51*/
      go to 50                                                                  !/*al         52*/
  270 if (isw(11)) 90,280,90                                                    !/*al         53*/
  280 pt(26)=dp2                                                                !/*al         54*/
      go to 160                                                                 !/*al         55*/
  290 if (univ-value1) 300,300,330                                              !/*al         56*/
  300 pm(4)=pnms                                                                !/*al         57*/
      pt(4)=pnms                                                                !/*al         58*/
      univ=rang()                                                               !/*al         59*/
      if (value3+dp1) 320,310,320                                               !/*al         60*/
  310 if (univ-3.3333333d-1) 90,90,220                                          !/*al         61*/
  320 pt(2)=dp5                                                                 !/*al         62*/
      if (univ-3.3333333d-1) 110,110,50                                         !/*al         63*/
  330 if (univ-value2) 340,340,370                                              !/*al         64*/
  340 pt(2)=dp4                                                                 !/*al         65*/
      univ=rang()                                                               !/*al         66*/
      if (value3+dp1) 360,350,360                                               !/*al         67*/
  350 if (univ-6.6666667d-1) 50,50,110                                          !/*al         68*/
  360 if (univ-6.6666667d-1) 220,220,90                                         !/*al         69*/
  370 pm(4)=pnms                                                                !/*al         70*/
      pt(4)=pnms                                                                !/*al         71*/
      if (value3+dp1) 380,160,380                                               !/*al         72*/
  380 pt(14)=dp5                                                                !/*al         73*/
      go to 110                                                                 !/*al         74*/
      end                                                                       !/*al         75*/

!     $Id: alp28.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                        !/*al          1*/
      subroutine alp28                                                          !/*al          2*/
#include <lcs/double.h>                                                         /*!al          3*/
#include <lcs/btcom.h>                                                          /*!al          4*/
      r=rang()                                                                  !/*al          5*/
      if (isw(13)) 230,10,230                                                   !/*al          6*/
   10 if (r-6.d-1) 20,20,120                                                    !/*al          7*/
   20 pt(4)=pnms                                                                !/*al          8*/
      r=rang()                                                                  !/*al          9*/
      if (isw(4)) 30,90,30                                                      !/*al         10*/
   30 if (r-3.3333333d-1) 40,40,70                                              !/*al         11*/
   40 pt(26)=dp5                                                                !/*al         12*/
   50 pt(28)=pnms                                                               !/*al         13*/
   60 return                                                                    !/*al         14*/
   70 pt(26)=dp4                                                                !/*al         15*/
   80 pt(38)=dp2                                                                !/*al         16*/
      go to 60                                                                  !/*al         17*/
   90 pt(2)=dp5                                                                 !/*al         18*/
      pt(14)=dp2                                                                !/*al         19*/
      if (r-3.3333333d-1) 100,100,110                                           !/*al         20*/
  100 pt(28)=pnms                                                               !/*al         21*/
      go to 80                                                                  !/*al         22*/
  110 pt(26)=dp4                                                                !/*al         23*/
      go to 60                                                                  !/*al         24*/
  120 continue                                                                  !/*al         25*/
      r=rang()                                                                  !/*al         26*/
      if (isw(4)) 130,180,130                                                   !/*al         27*/
  130 if (r-6.6666667d-1) 140,140,160                                           !/*al         28*/
  140 pt(2)=dp4                                                                 !/*al         29*/
  150 continue                                                                  !/*al         30*/
      r=rang()                                                                  !/*al         31*/
      if (r-6.6666667d-1) 110,110,100                                           !/*al         32*/
  160 pt(14)=dp2                                                                !/*al         33*/
  170 pt(4)=pnms                                                                !/*al         34*/
      go to 150                                                                 !/*al         35*/
  180 if (r-6.6666667d-1) 190,190,220                                           !/*al         36*/
  190 pt(2)=dp4                                                                 !/*al         37*/
  200 pt(14)=dp2                                                                !/*al         38*/
  210 continue                                                                  !/*al         39*/
      r=rang()                                                                  !/*al         40*/
      if (r-6.6666667d-1) 70,70,40                                              !/*al         41*/
  220 pt(2)=dp5                                                                 !/*al         42*/
      pt(4)=pnms                                                                !/*al         43*/
      go to 210                                                                 !/*al         44*/
  230 if (r-value1) 240,240,270                                                 !/*al         45*/
  240 pt(4)=pnms                                                                !/*al         46*/
      if (isw(4)) 260,250,260                                                   !/*al         47*/
  250 pt(2)=dp5                                                                 !/*al         48*/
      pt(14)=dp2                                                                !/*al         49*/
      go to 50                                                                  !/*al         50*/
  260 pt(38)=dp2                                                                !/*al         51*/
      go to 40                                                                  !/*al         52*/
  270 continue                                                                  !/*al         53*/
      r=rang()                                                                  !/*al         54*/
      if (isw(4)) 280,310,280                                                   !/*al         55*/
  280 if (r-3.3333333d-1) 290,290,300                                           !/*al         56*/
  290 pt(4)=pnms                                                                !/*al         57*/
      go to 200                                                                 !/*al         58*/
  300 pt(2)=dp4                                                                 !/*al         59*/
      go to 210                                                                 !/*al         60*/
  310 if (r-3.3333333d-1) 320,320,330                                           !/*al         61*/
  320 pt(2)=dp5                                                                 !/*al         62*/
      go to 170                                                                 !/*al         63*/
  330 pt(14)=dp2                                                                !/*al         64*/
      go to 140                                                                 !/*al         65*/
      end                                                                       !/*al         66*/

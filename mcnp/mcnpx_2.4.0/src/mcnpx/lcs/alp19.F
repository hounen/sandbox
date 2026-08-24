!     $Id: alp19.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                        !/*al          1*/
      subroutine alp19                                                          !/*al          2*/
#include <lcs/double.h>                                                         /*!al          3*/
#include <lcs/btcom.h>                                                          /*!al          4*/
      univ=rang()                                                               !/*al          5*/
      pt(2)=dp1                                                                 !/*al          6*/
      pt(26)=dp1                                                                !/*al          7*/
      pt(14)=dp3                                                                !/*al          8*/
      pt(16)=poms                                                               !/*al          9*/
      if (isw(12)) 10,10,140                                                    !/*al         10*/
   10 if (univ-.25d0) 20,20,110                                                 !/*al         11*/
   20 if (isw(4)) 40,30,40                                                      !/*al         12*/
   30 pt(2)=dp2                                                                 !/*al         13*/
   40 continue                                                                  !/*al         14*/
      univ=rang()                                                               !/*al         15*/
      if (univ-6.6666667d-1) 50,50,90                                           !/*al         16*/
   50 pt(14)=dp4                                                                !/*al         17*/
   60 if (isw(4)) 80,70,80                                                      !/*al         18*/
   70 pt(26)=dp2                                                                !/*al         19*/
   80 go to 240                                                                 !/*al         20*/
   90 pt(16)=pnms                                                               !/*al         21*/
      if (isw(4)) 70,100,70                                                     !/*al         22*/
  100 pt(14)=dp5                                                                !/*al         23*/
      go to 80                                                                  !/*al         24*/
  110 if (isw(4)) 130,120,130                                                   !/*al         25*/
  120 pt(26)=dp2                                                                !/*al         26*/
      go to 90                                                                  !/*al         27*/
  130 pt(2)=dp2                                                                 !/*al         28*/
      pt(16)=pnms                                                               !/*al         29*/
      go to 80                                                                  !/*al         30*/
  140 if (univ-dph) 150,150,190                                                 !/*al         31*/
  150 if (isw(4)) 160,170,160                                                   !/*al         32*/
  160 pt(2)=dp2                                                                 !/*al         33*/
  170 continue                                                                  !/*al         34*/
      univ=rang()                                                               !/*al         35*/
      if (univ-3.3333333d-1) 90,90,180                                          !/*al         36*/
  180 pt(14)=dp4                                                                !/*al         37*/
      go to 60                                                                  !/*al         38*/
  190 if (isw(4)) 210,200,210                                                   !/*al         39*/
  200 pt(2)=dp2                                                                 !/*al         40*/
  210 continue                                                                  !/*al         41*/
      univ=rang()                                                               !/*al         42*/
      if (univ-6.6666667d-1) 220,220,230                                        !/*al         43*/
  220 pt(14)=dp4                                                                !/*al         44*/
      if (isw(4)) 70,80,70                                                      !/*al         45*/
  230 pt(16)=pnms                                                               !/*al         46*/
      if (isw(4)) 100,70,100                                                    !/*al         47*/
  240 return                                                                    !/*al         48*/
      end                                                                       !/*al         49*/

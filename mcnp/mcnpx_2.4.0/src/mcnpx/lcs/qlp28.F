!     $Id: qlp28.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*ql          1*/
      subroutine qlp28                                                          !/*ql          2*/
! QLP28      calls...  FLTRNF                                                   !/*ql          3*/
!     QLP28  called by...   QOU21                                               !/*ql          4*/
#include <lcs/double.h>                                                         /*!ql          5*/
#include <lcs/isobar.h>                                                         /*!ql          6*/
      r=rang()                                                                  !/*ql          7*/
      if (isw_(13)) 230,10,230                                                  !/*ql          8*/
   10 if (r-.6d0) 20,20,120                                                     !/*ql          9*/
   20 pt_(4)=pnms_                                                              !/*ql         10*/
      r=rang()                                                                  !/*ql         11*/
      if (isw_(4)) 30,90,30                                                     !/*ql         12*/
   30 if (r-.33333333d0) 40,40,70                                               !/*ql         13*/
   40 pt_(26)=dp5                                                               !/*ql         14*/
   50 pt_(28)=pnms_                                                             !/*ql         15*/
   60 return                                                                    !/*ql         16*/
   70 pt_(26)=dp4                                                               !/*ql         17*/
   80 pt_(38)=dp2                                                               !/*ql         18*/
      go to 60                                                                  !/*ql         19*/
   90 pt_(2)=dp5                                                                !/*ql         20*/
      pt_(14)=dp2                                                               !/*ql         21*/
      if (r-.33333333d0) 100,100,110                                            !/*ql         22*/
  100 pt_(28)=pnms_                                                             !/*ql         23*/
      go to 80                                                                  !/*ql         24*/
  110 pt_(26)=dp4                                                               !/*ql         25*/
      go to 60                                                                  !/*ql         26*/
  120 continue                                                                  !/*ql         27*/
      r=rang()                                                                  !/*ql         28*/
      if (isw_(4)) 130,180,130                                                  !/*ql         29*/
  130 if (r-.66666667d0) 140,140,160                                            !/*ql         30*/
  140 pt_(2)=dp4                                                                !/*ql         31*/
  150 continue                                                                  !/*ql         32*/
      r=rang()                                                                  !/*ql         33*/
      if (r-.66666667d0) 110,110,100                                            !/*ql         34*/
  160 pt_(14)=dp2                                                               !/*ql         35*/
  170 pt_(4)=pnms_                                                              !/*ql         36*/
      go to 150                                                                 !/*ql         37*/
  180 if (r-.66666667d0) 190,190,220                                            !/*ql         38*/
  190 pt_(2)=dp4                                                                !/*ql         39*/
  200 pt_(14)=dp2                                                               !/*ql         40*/
  210 continue                                                                  !/*ql         41*/
      r=rang()                                                                  !/*ql         42*/
      if (r-.66666667d0) 70,70,40                                               !/*ql         43*/
  220 pt_(2)=dp5                                                                !/*ql         44*/
      pt_(4)=pnms_                                                              !/*ql         45*/
      go to 210                                                                 !/*ql         46*/
  230 if (r-valu1_) 240,240,270                                                 !/*ql         47*/
  240 pt_(4)=pnms_                                                              !/*ql         48*/
      if (isw_(4)) 260,250,260                                                  !/*ql         49*/
  250 pt_(2)=dp5                                                                !/*ql         50*/
      pt_(14)=dp2                                                               !/*ql         51*/
      go to 50                                                                  !/*ql         52*/
  260 pt_(38)=dp2                                                               !/*ql         53*/
      go to 40                                                                  !/*ql         54*/
  270 continue                                                                  !/*ql         55*/
      r=rang()                                                                  !/*ql         56*/
      if (isw_(4)) 280,310,280                                                  !/*ql         57*/
  280 if (r-.33333333d0) 290,290,300                                            !/*ql         58*/
  290 pt_(4)=pnms_                                                              !/*ql         59*/
      go to 200                                                                 !/*ql         60*/
  300 pt_(2)=dp4                                                                !/*ql         61*/
      go to 210                                                                 !/*ql         62*/
  310 if (r-.33333333d0) 320,320,330                                            !/*ql         63*/
  320 pt_(2)=dp5                                                                !/*ql         64*/
      go to 170                                                                 !/*ql         65*/
  330 pt_(14)=dp2                                                               !/*ql         66*/
      go to 140                                                                 !/*ql         67*/
      end                                                                       !/*ql         68*/

!     $Id: qlpha.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*ql          1*/
      subroutine qlpha                                                          !/*ql          2*/
! QLPHA      calls...  FLTRNF                                                   !/*ql          3*/
!     QLPHA  called by...   QOU17                                               !/*ql          4*/
#include <lcs/double.h>                                                         /*!ql          5*/
#include <lcs/isobar.h>                                                         /*!ql          6*/
      univ_=rang()                                                              !/*ql          7*/
      if (valu3_) 290,10,140                                                    !/*ql          8*/
   10 if (univ_-valu1_) 20,20,120                                               !/*ql          9*/
   20 if (isw_(11)) 40,30,40                                                    !/*ql         10*/
   30 pt_(2)=dp5                                                                !/*ql         11*/
      pt_(26)=dp2                                                               !/*ql         12*/
   40 pt_(4)=pnms_                                                              !/*ql         13*/
      pm_(4)=pnms_                                                              !/*ql         14*/
      univ_=rang()                                                              !/*ql         15*/
      if (univ_-valu2_) 50,50,70                                                !/*ql         16*/
   50 pt_(14)=dp4                                                               !/*ql         17*/
   60 return                                                                    !/*ql         18*/
   70 if (isw_(11)) 110,80,110                                                  !/*ql         19*/
   80 pt_(26)=dp1                                                               !/*ql         20*/
   90 pt_(14)=dp5                                                               !/*ql         21*/
  100 pt_(16)=pnms_                                                             !/*ql         22*/
      go to 60                                                                  !/*ql         23*/
  110 pt_(26)=dp2                                                               !/*ql         24*/
      go to 100                                                                 !/*ql         25*/
  120 pt_(2)=dp4                                                                !/*ql         26*/
      if (isw_(11)) 100,130,100                                                 !/*ql         27*/
  130 pt_(14)=dp5                                                               !/*ql         28*/
      go to 110                                                                 !/*ql         29*/
  140 if (univ_-valu1_) 150,150,190                                             !/*ql         30*/
  150 pm_(4)=pnms_                                                              !/*ql         31*/
      if (isw_(11)) 160,180,160                                                 !/*ql         32*/
  160 pt_(2)=dp5                                                                !/*ql         33*/
  170 pt_(16)=pnms_                                                             !/*ql         34*/
      pt_(4)=pnms_                                                              !/*ql         35*/
      go to 60                                                                  !/*ql         36*/
  180 pt_(14)=dp5                                                               !/*ql         37*/
      pt_(26)=dp2                                                               !/*ql         38*/
      go to 170                                                                 !/*ql         39*/
  190 if (univ_-valu2_) 200,200,240                                             !/*ql         40*/
  200 pt_(2)=dp4                                                                !/*ql         41*/
      univ_=rang()                                                              !/*ql         42*/
      if (univ_-valu3_) 230,230,210                                             !/*ql         43*/
  210 if (isw_(11)) 50,220,50                                                   !/*ql         44*/
  220 pt_(26)=dp2                                                               !/*ql         45*/
      go to 50                                                                  !/*ql         46*/
  230 if (isw_(11)) 110,90,110                                                  !/*ql         47*/
  240 pm_(4)=pnms_                                                              !/*ql         48*/
      pt_(4)=pnms_                                                              !/*ql         49*/
      univ_=rang()                                                              !/*ql         50*/
      if (univ_-.66666667d0) 250,250,270                                        !/*ql         51*/
  250 if (isw_(11)) 220,260,220                                                 !/*ql         52*/
  260 pt_(2)=dp5                                                                !/*ql         53*/
      go to 50                                                                  !/*ql         54*/
  270 if (isw_(11)) 90,280,90                                                   !/*ql         55*/
  280 pt_(26)=dp2                                                               !/*ql         56*/
      go to 160                                                                 !/*ql         57*/
  290 if (univ_-valu1_) 300,300,330                                             !/*ql         58*/
  300 pm_(4)=pnms_                                                              !/*ql         59*/
      pt_(4)=pnms_                                                              !/*ql         60*/
      univ_=rang()                                                              !/*ql         61*/
      if (valu3_+dp1) 320,310,320                                               !/*ql         62*/
  310 if (univ_-.33333333d0) 90,90,220                                          !/*ql         63*/
  320 pt_(2)=dp5                                                                !/*ql         64*/
      if (univ_-.33333333d0) 110,110,50                                         !/*ql         65*/
  330 if (univ_-valu2_) 340,340,370                                             !/*ql         66*/
  340 pt_(2)=dp4                                                                !/*ql         67*/
      univ_=rang()                                                              !/*ql         68*/
      if (valu3_+dp1) 360,350,360                                               !/*ql         69*/
  350 if (univ_-.66666667d0) 50,50,110                                          !/*ql         70*/
  360 if (univ_-.66666667d0) 220,220,90                                         !/*ql         71*/
  370 pm_(4)=pnms_                                                              !/*ql         72*/
      pt_(4)=pnms_                                                              !/*ql         73*/
      if (valu3_+dp1) 380,160,380                                               !/*ql         74*/
  380 pt_(14)=dp5                                                               !/*ql         75*/
      go to 110                                                                 !/*ql         76*/
      end                                                                       !/*ql         77*/

!     $Id: qlp19.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*ql          1*/
      subroutine qlp19                                                          !/*ql          2*/
! QLP19      calls...  FLTRNF                                                   !/*ql          3*/
!     QLP19  called by...   QOU21                                               !/*ql          4*/
#include <lcs/double.h>                                                         /*!ql          5*/
#include <lcs/isobar.h>                                                         /*!ql          6*/
      univ_=rang()                                                              !/*ql          7*/
      pt_(2)=dp1                                                                !/*ql          8*/
      pt_(26)=dp1                                                               !/*ql          9*/
      pt_(14)=dp3                                                               !/*ql         10*/
      pt_(16)=poms_                                                             !/*ql         11*/
      if (isw_(12)) 10,10,140                                                   !/*ql         12*/
   10 if (univ_-.25d0) 20,20,110                                                !/*ql         13*/
   20 if (isw_(4)) 40,30,40                                                     !/*ql         14*/
   30 pt_(2)=dp2                                                                !/*ql         15*/
   40 continue                                                                  !/*ql         16*/
      univ_=rang()                                                              !/*ql         17*/
      if (univ_-.66666667d0) 50,50,90                                           !/*ql         18*/
   50 pt_(14)=dp4                                                               !/*ql         19*/
   60 if (isw_(4)) 80,70,80                                                     !/*ql         20*/
   70 pt_(26)=dp2                                                               !/*ql         21*/
   80 return                                                                    !/*ql         22*/
   90 pt_(16)=pnms_                                                             !/*ql         23*/
      if (isw_(4)) 70,100,70                                                    !/*ql         24*/
  100 pt_(14)=dp5                                                               !/*ql         25*/
      go to 80                                                                  !/*ql         26*/
  110 if (isw_(4)) 130,120,130                                                  !/*ql         27*/
  120 pt_(26)=dp2                                                               !/*ql         28*/
      go to 90                                                                  !/*ql         29*/
  130 pt_(2)=dp2                                                                !/*ql         30*/
      pt_(16)=pnms_                                                             !/*ql         31*/
      go to 80                                                                  !/*ql         32*/
  140 if (univ_-dph) 150,150,190                                                !/*ql         33*/
  150 if (isw_(4)) 160,170,160                                                  !/*ql         34*/
  160 pt_(2)=dp2                                                                !/*ql         35*/
  170 continue                                                                  !/*ql         36*/
      univ_=rang()                                                              !/*ql         37*/
      if (univ_-.33333333d0) 90,90,180                                          !/*ql         38*/
  180 pt_(14)=dp4                                                               !/*ql         39*/
      go to 60                                                                  !/*ql         40*/
  190 if (isw_(4)) 210,200,210                                                  !/*ql         41*/
  200 pt_(2)=dp2                                                                !/*ql         42*/
  210 continue                                                                  !/*ql         43*/
      univ_=rang()                                                              !/*ql         44*/
      if (univ_-.66666667d0) 220,220,230                                        !/*ql         45*/
  220 pt_(14)=dp4                                                               !/*ql         46*/
      if (isw_(4)) 70,80,70                                                     !/*ql         47*/
  230 pt_(16)=pnms_                                                             !/*ql         48*/
      if (isw_(4)) 240,70,240                                                   !/*ql         49*/
  240 go to 100                                                                 !/*ql         50*/
      end                                                                       !/*ql         51*/

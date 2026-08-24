!     $Id: rou14.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou14                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      if (i3) 310,270,10                                                        !/*ro          5*/
   10 if (i1) 20,80,80                                                          !/*ro          6*/
   20 i1=0                                                                      !/*ro          7*/
      value1=(e(3)-pm(3))/rcpmv                                                 !/*ro          8*/
      if (aabs) 30,40,30                                                        !/*ro          9*/
   30 value1=value1+absec                                                       !/*ro         10*/
   40 if (pt(2)-dp2) 70,50,80                                                   !/*ro         11*/
!     pt(2)=1=proton   pt(2)=2=neutron   pt(2)=3,4,5=pion                       !/*ro         12*/
   50 i3=2                                                                      !/*ro         13*/
   60 go to 320                                                                 !/*ro         14*/
   70 i3=1                                                                      !/*ro         15*/
      go to 60                                                                  !/*ro         16*/
   80 call pinst                                                                !/*ro         17*/
      if (i1) 90,100,90                                                         !/*ro         18*/
   90 i3=3                                                                      !/*ro         19*/
      go to 60                                                                  !/*ro         20*/
  100 i1=0                                                                      !/*ro         21*/
      m=pt(2)                                                                   !/*ro         22*/
      value2=value1                                                             !/*ro         23*/
      if (m-3) 110,150,150                                                      !/*ro         24*/
  110 if (eco(m)-value2) 140,120,130                                            !/*ro         25*/
  120 if (eco(m)) 90,90,130                                                     !/*ro         26*/
  130 pt(i1+3)=dp0                                                              !/*ro         27*/
      pnbc(m)=pnbc(m)+dp1                                                       !/*ro         28*/
      go to 230                                                                 !/*ro         29*/
  140 pt(i1+3)=value2                                                           !/*ro         30*/
      if (i1) 300,240,300                                                       !/*ro         31*/
  150 ccofe=clcfe                                                               !/*ro         32*/
      if (m-4) 160,170,170                                                      !/*ro         33*/
  160 if (strkp+dp2) 200,190,200                                                !/*ro         34*/
  170 if (strkp+dp2) 200,200,180                                                !/*ro         35*/
  180 ccofe=clcfe-ctofe+ctofen                                                  !/*ro         36*/
      go to 200                                                                 !/*ro         37*/
  190 ccofe=clcfe+ctofe-ctofen                                                  !/*ro         38*/
  200 if (value2-ccofe) 130,130,210                                             !/*ro         39*/
  210 if (strkp+dp2) 220,220,140                                                !/*ro         40*/
  220 pt(3)=value1-space(med+3)+space(med+9)                                    !/*ro         41*/
  230 if (i1) 260,240,260                                                       !/*ro         42*/
  240 m=pt(14)                                                                  !/*ro         43*/
      if (m-3) 250,90,90                                                        !/*ro         44*/
  250 value2=com                                                                !/*ro         45*/
      i1=12                                                                     !/*ro         46*/
      go to 110                                                                 !/*ro         47*/
  260 if (pt(3)) 300,270,300                                                    !/*ro         48*/
  270 call punp                                                                 !/*ro         49*/
      if (i1) 90,280,290                                                        !/*ro         50*/
!     -, =error  0=end of record  +=piscc(6607)                                 !/*ro         51*/
  280 i3=4                                                                      !/*ro         52*/
      go to 60                                                                  !/*ro         53*/
  290 i3=5                                                                      !/*ro         54*/
      go to 60                                                                  !/*ro         55*/
  300 call collm (-1)                                                           !/*ro         56*/
  310 if (ke.gt.0) go to 320                                                    !/*ro         57*/
      call stpr                                                                 !/*ro         58*/
      if (i1) 90,270,90                                                         !/*ro         59*/
  320 return                                                                    !/*ro         60*/
      end                                                                       !/*ro         61*/

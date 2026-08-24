!     $Id: rou19.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou19                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      pt(3)=pt(3)+((pt(11)-pt(12))/rcpmv)                                       !/*ro          5*/
!     collision allowed                                                         !/*ro          6*/
      k=3                                                                       !/*ro          7*/
   10 if (pt(k)-25.d+02) 30,30,20                                               !/*ro          8*/
   20 i3=1                                                                      !/*ro          9*/
      go to 90                                                                  !/*ro         10*/
   30 if (pt(k)) 70,70,40                                                       !/*ro         11*/
   40 ccofe=eco(1)                                                              !/*ro         12*/
      if (pt(k-1)-dp4) 60,50,50                                                 !/*ro         13*/
   50 ccofe=ccofe-ctofe+ctofen                                                  !/*ro         14*/
   60 if (pt(k)-ccofe) 70,70,170                                                !/*ro         15*/
   70 pt(k)=dp0                                                                 !/*ro         16*/
      if (pt(k-1)-dp3) 80,110,100                                               !/*ro         17*/
   80 i3=-1                                                                     !/*ro         18*/
   90 return                                                                    !/*ro         19*/
  100 if (pt(k-1)-dp5) 110,110,80                                               !/*ro         20*/
  110 m=pt(k-1)                                                                 !/*ro         21*/
      pnbc(m)=pnbc(m)+dp1                                                       !/*ro         22*/
      go to 140                                                                 !/*ro         23*/
  120 i2=2                                                                      !/*ro         24*/
  130 i1=(k/12)+3                                                               !/*ro         25*/
      call pstor                                                                !/*ro         26*/
  140 if (k-15) 150,160,190                                                     !/*ro         27*/
  150 k=15                                                                      !/*ro         28*/
      if (pt(15)) 160,160,120                                                   !/*ro         29*/
  160 k=27                                                                      !/*ro         30*/
      pt(27)=pt(27)+((pnidk(12)-pt(k+1))/rcpmv)                                 !/*ro         31*/
      go to 10                                                                  !/*ro         32*/
  170 if (k-15) 120,180,180                                                     !/*ro         33*/
  180 i2=0                                                                      !/*ro         34*/
      go to 130                                                                 !/*ro         35*/
  190 if (k-27) 80,200,210                                                      !/*ro         36*/
  200 if (pt(39)) 210,210,220                                                   !/*ro         37*/
  210 i3=0                                                                      !/*ro         38*/
      go to 90                                                                  !/*ro         39*/
  220 i2=1                                                                      !/*ro         40*/
      k=39                                                                      !/*ro         41*/
      go to 130                                                                 !/*ro         42*/
      end                                                                       !/*ro         43*/

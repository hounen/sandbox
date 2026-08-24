!     $Id: rout7a.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                       !/*ro          1*/
      subroutine rout7a                                                         !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
   10 i1=-1                                                                     !/*ro          5*/
      call spisom                                                               !/*ro          6*/
      go to (20,30,20,20,30), ifca                                              !/*ro          7*/
   20 value1=space(med+3)-7.d0                                                  !/*ro          8*/
      go to 40                                                                  !/*ro          9*/
   30 value1=space(med+9)-7.d0                                                  !/*ro         10*/
   40 if (value1) 50,70,70                                                      !/*ro         11*/
   50 i3=2                                                                      !/*ro         12*/
   60 return                                                                    !/*ro         13*/
   70 if ((value1*dp2*rcpmv)-e(2)) 10,10,80                                     !/*ro         14*/
   80 pm(3)=dncms                                                               !/*ro         15*/
      pm(2)=dp2*dncms                                                           !/*ro         16*/
      e(2)=pm(2)+e(2)                                                           !/*ro         17*/
      value1=ex                                                                 !/*ro         18*/
      if (med-2) 90,100,110                                                     !/*ro         19*/
   90 i3=3                                                                      !/*ro         20*/
      go to 60                                                                  !/*ro         21*/
  100 i3=4                                                                      !/*ro         22*/
      go to 60                                                                  !/*ro         23*/
  110 if (inc) 120,170,120                                                      !/*ro         24*/
  120 if (isw(1)) 140,130,140                                                   !/*ro         25*/
  130 i3=5                                                                      !/*ro         26*/
      go to 60                                                                  !/*ro         27*/
  140 if (isw(2)) 150,160,150                                                   !/*ro         28*/
  150 i3=6                                                                      !/*ro         29*/
      go to 60                                                                  !/*ro         30*/
  160 i3=9                                                                      !/*ro         31*/
      go to 60                                                                  !/*ro         32*/
  170 if (isw(1)) 190,180,190                                                   !/*ro         33*/
  180 i3=7                                                                      !/*ro         34*/
      go to 60                                                                  !/*ro         35*/
  190 if (isw(2)) 200,210,200                                                   !/*ro         36*/
  200 i3=1                                                                      !/*ro         37*/
      go to 60                                                                  !/*ro         38*/
  210 i3=8                                                                      !/*ro         39*/
      go to 60                                                                  !/*ro         40*/
      end                                                                       !/*ro         41*/

!     $Id: rou15.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou15 (t)                                                      !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(6426)                                                         !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      go to (10,80,20,100), i3                                                  !/*ro          6*/
   10 if (rlke.le.2.5d+03) go to 40                                             !/*ro          7*/
   20 i3=4                                                                      !/*ro          8*/
   30 go to 160                                                                 !/*ro          9*/
   40 if (ik-3) 70,50,60                                                        !/*ro         10*/
   50 i3=1                                                                      !/*ro         11*/
      go to 30                                                                  !/*ro         12*/
   60 i3=2                                                                      !/*ro         13*/
      go to 30                                                                  !/*ro         14*/
   70 call mud (snt,inpt)                                                       !/*ro         15*/
      call crdet (51,t(1),rlke)                                                 !/*ro         16*/
      i2=0                                                                      !/*ro         17*/
      cst=crdt(2)-abs(snt*(crdt(2)-crdt(1)))                                    !/*ro         18*/
   80 if (i2) 90,90,20                                                          !/*ro         19*/
   90 i3=3                                                                      !/*ro         20*/
      go to 30                                                                  !/*ro         21*/
  100 continue                                                                  !/*ro         22*/
      value1=rang()                                                             !/*ro         23*/
      if (value1-crdt(1)) 150,110,110                                           !/*ro         24*/
  110 value2=dp1                                                                !/*ro         25*/
!     scatt. forward                                                            !/*ro         26*/
      value1=rang()                                                             !/*ro         27*/
      if (value1-crdt(4)) 120,140,140                                           !/*ro         28*/
  120 continue                                                                  !/*ro         29*/
      value1=rang()                                                             !/*ro         30*/
!     to sample from uniform dist.                                              !/*ro         31*/
  130 cst=value1*value2                                                         !/*ro         32*/
      go to 90                                                                  !/*ro         33*/
  140 com=dp0                                                                   !/*ro         34*/
      call big7 (com,value1,i1)                                                 !/*ro         35*/
      go to 130                                                                 !/*ro         36*/
  150 value2=-dp1                                                               !/*ro         37*/
      value1=rang()                                                             !/*ro         38*/
      i1=i1+i2                                                                  !/*ro         39*/
      if (value1-crdt(2)) 120,140,140                                           !/*ro         40*/
  160 return                                                                    !/*ro         41*/
      end                                                                       !/*ro         42*/

!     $Id: store.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                        !/*st          1*/
      subroutine store                                                          !/*st          2*/
#include <lcs/double.h>                                                         /*!st          3*/
#include <lcs/btcom.h>                                                          /*!st          4*/
      do 10 i=1,21                                                              !/*st          5*/
   10 ce(i)=dp0                                                                 !/*st          6*/
      n=i1                                                                      !/*st          7*/
      if (i3) 190,20,190                                                        !/*st          8*/
   20 k=i2+6                                                                    !/*st          9*/
      l=i2                                                                      !/*st         10*/
      if (i5.ne.4) go to 50                                                     !/*st         11*/
      if (i1-5) 30,210,30                                                       !/*st         12*/
   30 mm=-6                                                                     !/*st         13*/
   40 l=k                                                                       !/*st         14*/
      n=n-1                                                                     !/*st         15*/
   50 do 100 i=1,n,2                                                            !/*st         16*/
      id=i                                                                      !/*st         17*/
      ce(i+1)=fmax(i)*1.d+30*space(k)                                           !/*st         18*/
      ce(i+2)=fmax(i+1)*1.d+30*space(l)                                         !/*st         19*/
      if (i5.lt.3) go to 100                                                    !/*st         20*/
      m=k                                                                       !/*st         21*/
      k=l                                                                       !/*st         22*/
      go to (100,100,90,70,60), i5                                              !/*st         23*/
   60 if (id-2) 100,100,80                                                      !/*st         24*/
   70 k=m+mm                                                                    !/*st         25*/
      mm=-mm                                                                    !/*st         26*/
      l=k                                                                       !/*st         27*/
      go to 100                                                                 !/*st         28*/
   80 k=i2                                                                      !/*st         29*/
   90 l=m                                                                       !/*st         30*/
  100 continue                                                                  !/*st         31*/
      if (i5.eq.4) go to 200                                                    !/*st         32*/
  110 sign=dp0                                                                  !/*st         33*/
      do 120 i=2,8                                                              !/*st         34*/
  120 sign=sign+ce(i)                                                           !/*st         35*/
      go to (130,130,140,230,140), i5                                           !/*st         36*/
  130 sign=sign*9.99999d-1                                                      !/*st         37*/
      return                                                                    !/*st         38*/
  140 if (i1-4) 150,150,130                                                     !/*st         39*/
  150 if (i3) 160,160,170                                                       !/*st         40*/
  160 space(i2+87)=sign                                                         !/*st         41*/
      go to 180                                                                 !/*st         42*/
  170 space(i2+106)=sign                                                        !/*st         43*/
  180 fmax(5)=dp0                                                               !/*st         44*/
      fmax(6)=dp0                                                               !/*st         45*/
      go to 130                                                                 !/*st         46*/
  190 k=i2                                                                      !/*st         47*/
      l=i2+6                                                                    !/*st         48*/
      if (i5.ne.4) go to 50                                                     !/*st         49*/
      mm=6                                                                      !/*st         50*/
      go to 40                                                                  !/*st         51*/
  200 ce(8)=fmax(7)*1.d+30*space(l)                                             !/*st         52*/
      go to 110                                                                 !/*st         53*/
  210 sub1=fmax(1)*1.d+30                                                       !/*st         54*/
      sub2=fmax(2)*1.d+30                                                       !/*st         55*/
      do 220 i=2,n,2                                                            !/*st         56*/
      ce(i)=sub1*space(k)                                                       !/*st         57*/
      ce(i+1)=sub2*space(k)                                                     !/*st         58*/
      k=l                                                                       !/*st         59*/
  220 l=k+6                                                                     !/*st         60*/
      ce(n+1)=space(k)*1.d+30*fmax(n)                                           !/*st         61*/
      go to 110                                                                 !/*st         62*/
  230 if (i1-5) 130,240,130                                                     !/*st         63*/
  240 space(i2+68)=sign                                                         !/*st         64*/
      fmax(6)=dp0                                                               !/*st         65*/
      fmax(7)=dp0                                                               !/*st         66*/
      go to 130                                                                 !/*st         67*/
      end                                                                       !/*st         68*/

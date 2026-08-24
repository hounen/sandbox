!     $Id: rou20.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou20 (t,b,r,w,g)                                              !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(115), b(80), r(239), w(60), g(55)                             !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      go to (10,50,70,80,190,200,270,60), i3                                    !/*ro          6*/
   10 pm(4)=dncms                                                               !/*ro          7*/
      call pinst                                                                !/*ro          8*/
      if (i1) 20,40,20                                                          !/*ro          9*/
   20 i3=1                                                                      !/*ro         10*/
   30 return                                                                    !/*ro         11*/
   40 i3=2                                                                      !/*ro         12*/
      go to 30                                                                  !/*ro         13*/
   50 isw(9)=0                                                                  !/*ro         14*/
   60 isw(10)=2                                                                 !/*ro         15*/
      i3=3                                                                      !/*ro         16*/
      go to 30                                                                  !/*ro         17*/
!     pi meson - single production                                              !/*ro         18*/
   70 pt(2)=dp2                                                                 !/*ro         19*/
      i3=4                                                                      !/*ro         20*/
      go to 30                                                                  !/*ro         21*/
!     collision particle pi-                                                    !/*ro         22*/
   80 pt(2)=dp2                                                                 !/*ro         23*/
      pt(14)=dp1                                                                !/*ro         24*/
   90 pm(3)=dncms                                                               !/*ro         25*/
      if (74.d1-rlke) 140,100,100                                               !/*ro         26*/
  100 if (3.d+02-rlke) 110,130,130                                              !/*ro         27*/
  110 value1=rlke-3.d+02                                                        !/*ro         28*/
      call crdet (5,t(1),value1)                                                !/*ro         29*/
!     (n-p)diff.crs.int.en.                                                     !/*ro         30*/
      i1=3                                                                      !/*ro         31*/
      i2=3                                                                      !/*ro         32*/
  120 i3=5                                                                      !/*ro         33*/
      go to 30                                                                  !/*ro         34*/
  130 call crdet (5,b(1),rlke)                                                  !/*ro         35*/
!     (n-p)diff.crs.low en.                                                     !/*ro         36*/
      i1=3                                                                      !/*ro         37*/
      i2=1                                                                      !/*ro         38*/
      go to 120                                                                 !/*ro         39*/
  140 if (35.d+02-rlke) 20,150,150                                              !/*ro         40*/
  150 if (it-17) 160,20,20                                                      !/*ro         41*/
  160 call dcintp (r(1))                                                        !/*ro         42*/
!     (n-p)diff.crs.high en.                                                    !/*ro         43*/
  170 if (i1) 20,180,180                                                        !/*ro         44*/
  180 i3=6                                                                      !/*ro         45*/
      go to 30                                                                  !/*ro         46*/
  190 pt(2)=dp1                                                                 !/*ro         47*/
      pt(14)=dp2                                                                !/*ro         48*/
      go to 90                                                                  !/*ro         49*/
  200 pt(2)=dp2                                                                 !/*ro         50*/
      pt(14)=dp2                                                                !/*ro         51*/
  210 pm(3)=dncms                                                               !/*ro         52*/
      if (5.d+02-rlke) 230,220,220                                              !/*ro         53*/
  220 i3=7                                                                      !/*ro         54*/
      go to 30                                                                  !/*ro         55*/
  230 if (1.d+03-rlke) 250,240,240                                              !/*ro         56*/
  240 call dcpr (w(1),12)                                                       !/*ro         57*/
      if (i2.eq.1) go to 20                                                     !/*ro         58*/
!     sample + mu in cst                                                        !/*ro         59*/
      snt=sqrt(dp1-cst*cst)                                                     !/*ro         60*/
!     p-p scattering                                                            !/*ro         61*/
      i3=8                                                                      !/*ro         62*/
      go to 30                                                                  !/*ro         63*/
!     -, scattering backward, mu less than 0                                    !/*ro         64*/
  250 if (35.d+02-rlke) 20,260,260                                              !/*ro         65*/
  260 call dcpr (g(1),11)                                                       !/*ro         66*/
      if (i2.eq.1) go to 20                                                     !/*ro         67*/
!     (p-p)diff.crs.sec.high en.                                                !/*ro         68*/
      go to 170                                                                 !/*ro         69*/
  270 pt(2)=dp1                                                                 !/*ro         70*/
      pt(14)=dp1                                                                !/*ro         71*/
      go to 210                                                                 !/*ro         72*/
!     no pion production possible                                               !/*ro         73*/
      end                                                                       !/*ro         74*/

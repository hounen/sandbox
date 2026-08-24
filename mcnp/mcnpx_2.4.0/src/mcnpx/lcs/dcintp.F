!     $Id: dcintp.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                       !/*dc          1*/
      subroutine dcintp (w)                                                     !/*dc          2*/
#include <lcs/double.h>                                                         /*!dc          3*/
      dimension w(250), z(24)                                                   !/*dc          4*/
#include <lcs/btcom.h>                                                          /*!dc          5*/
      z(1)=rlke-6.6d2                                                           !/*dc          6*/
      ie=int(z(1)/20.d0+5.d-06)                                                 !/*dc          7*/
      univ=(z(1)-real(ie)*2.d1)/2.d1                                            !/*dc          8*/
      ie=ie+1                                                                   !/*dc          9*/
      unive=(w(ie+1)-w(ie))*univ+w(ie)                                          !/*dc         10*/
! ratio                                                                         !/*dc         11*/
      i2=0                                                                      !/*dc         12*/
      k=1                                                                       !/*dc         13*/
      univ=rang()                                                               !/*dc         14*/
      if (univ.ge.unive) go to 180                                              !/*dc         15*/
! lt=backward                                                                   !/*dc         16*/
! n-p elastic scat.,rlke gt 660                                                 !/*dc         17*/
      lll=179                                                                   !/*dc         18*/
      if (w(180).gt.rlke) go to 20                                              !/*dc         19*/
      do 10 i=1,5                                                               !/*dc         20*/
      if (w(179+k).gt.rlke) go to 40                                            !/*dc         21*/
   10 k=k+12                                                                    !/*dc         22*/
   20 i1=-1                                                                     !/*dc         23*/
! error                                                                         !/*dc         24*/
   30 return                                                                    !/*dc         25*/
   40 m=12                                                                      !/*dc         26*/
   50 do 60 l=1,m                                                               !/*dc         27*/
      z(l+m)=w(lll+k)                                                           !/*dc         28*/
      z(l)=w(lll+k-m)                                                           !/*dc         29*/
   60 k=k+1                                                                     !/*dc         30*/
      univ=rang()                                                               !/*dc         31*/
      unive=(rlke-z(1))/(z(m+1)-z(1))                                           !/*dc         32*/
      do 70 i=2,m                                                               !/*dc         33*/
      p=(z(i+m)-z(i))*unive+z(i)                                                !/*dc         34*/
      if (p.ge.univ) go to 80                                                   !/*dc         35*/
   70 continue                                                                  !/*dc         36*/
      go to 20                                                                  !/*dc         37*/
   80 i1=i                                                                      !/*dc         38*/
      univ=rang()                                                               !/*dc         39*/
      flti=univ+real(i1-2)                                                      !/*dc         40*/
      if (m.le.9) go to 210                                                     !/*dc         41*/
      go to (20,90,90,100,110,120,120,130,140,150,160,170), i1                  !/*dc         42*/
   90 cst=1.d-2*flti-dp1                                                        !/*dc         43*/
      go to 30                                                                  !/*dc         44*/
  100 cst=2.d-2*univ-9.8d-1                                                     !/*dc         45*/
      go to 30                                                                  !/*dc         46*/
  110 cst=4.d-2*univ-9.6d-1                                                     !/*dc         47*/
      go to 30                                                                  !/*dc         48*/
  120 cst=6.d-2*flti-1.16d0                                                     !/*dc         49*/
      go to 30                                                                  !/*dc         50*/
  130 cst=8.d-2*univ-8.d-1                                                      !/*dc         51*/
      go to 30                                                                  !/*dc         52*/
  140 cst=1.d-1*univ-7.2d-1                                                     !/*dc         53*/
      go to 30                                                                  !/*dc         54*/
  150 cst=1.2d-1*univ-6.2d-1                                                    !/*dc         55*/
      go to 30                                                                  !/*dc         56*/
  160 cst=2.d-1*univ-5.d-1                                                      !/*dc         57*/
      go to 30                                                                  !/*dc         58*/
  170 cst=3.d-1*(univ-dp1)                                                      !/*dc         59*/
      go to 30                                                                  !/*dc         60*/
! forward                                                                       !/*dc         61*/
  180 lll=143                                                                   !/*dc         62*/
      if (w(144).gt.rlke) go to 20                                              !/*dc         63*/
      do 190 i=1,4                                                              !/*dc         64*/
      if (w(143+k).gt.rlke) go to 200                                           !/*dc         65*/
  190 k=k+9                                                                     !/*dc         66*/
      go to 20                                                                  !/*dc         67*/
  200 m=9                                                                       !/*dc         68*/
      go to 50                                                                  !/*dc         69*/
  210 go to (20,220,220,220,220,230,240,250,260), i1                            !/*dc         70*/
  220 cst=dp1-2.5d-2*flti                                                       !/*dc         71*/
      go to 30                                                                  !/*dc         72*/
  230 cst=8.5d-1+0.5d-1*univ                                                    !/*dc         73*/
      go to 30                                                                  !/*dc         74*/
  240 cst=7.d-1+1.5d-1*univ                                                     !/*dc         75*/
      go to 30                                                                  !/*dc         76*/
  250 cst=5.d-1+2.d-1*univ                                                      !/*dc         77*/
      go to 30                                                                  !/*dc         78*/
  260 cst=5.d-1*univ                                                            !/*dc         79*/
      go to 30                                                                  !/*dc         80*/
      end                                                                       !/*dc         81*/

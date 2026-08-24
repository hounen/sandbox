!     $Id: dcpr.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                         !/*dc          1*/
      subroutine dcpr (w,lk)                                                    !/*dc          2*/
#include <lcs/double.h>                                                         /*!dc          3*/
      dimension w(60), z(24)                                                    !/*dc          4*/
#include <lcs/btcom.h>                                                          /*!dc          5*/
! pdci has kk=12, pdch has kk=11                                                !/*dc          6*/
      i2=0                                                                      !/*dc          7*/
      kk=lk                                                                     !/*dc          8*/
      k=1                                                                       !/*dc          9*/
      do 10 i=1,5                                                               !/*dc         10*/
      if (w(k).ge.rlke) go to 40                                                !/*dc         11*/
   10 k=k+kk                                                                    !/*dc         12*/
   20 i2=1                                                                      !/*dc         13*/
! error return                                                                  !/*dc         14*/
   30 return                                                                    !/*dc         15*/
   40 do 50 l=1,kk                                                              !/*dc         16*/
      z(l+kk)=w(k)                                                              !/*dc         17*/
      z(l)=w(k-kk)                                                              !/*dc         18*/
   50 k=k+1                                                                     !/*dc         19*/
      sum=dp0                                                                   !/*dc         20*/
      r=rang()                                                                  !/*dc         21*/
      fract=(rlke-z(1))/(z(kk+1)-z(1))                                          !/*dc         22*/
      do 60 i=2,kk                                                              !/*dc         23*/
      sum=sum+z(i)+((z(i+kk)-z(i))*fract)                                       !/*dc         24*/
      if (r.lt.sum) go to 70                                                    !/*dc         25*/
   60 continue                                                                  !/*dc         26*/
      go to 20                                                                  !/*dc         27*/
! error                                                                         !/*dc         28*/
   70 continue                                                                  !/*dc         29*/
      r=rang()                                                                  !/*dc         30*/
      i1=i                                                                      !/*dc         31*/
      if (kk.gt.11) go to 90                                                    !/*dc         32*/
      if (i1.gt.2) go to 80                                                     !/*dc         33*/
      cst=4.d-1*r                                                               !/*dc         34*/
      go to 140                                                                 !/*dc         35*/
   80 i1=i1+1                                                                   !/*dc         36*/
   90 flti=real(i1-2)+r                                                         !/*dc         37*/
      go to (20,100,100,100,110,110,120,120,120,120,130,130), i1                !/*dc         38*/
  100 cst=2.d-1*(flti)                                                          !/*dc         39*/
      go to 140                                                                 !/*dc         40*/
  110 cst=3.d-1+1.d-1*(flti)                                                    !/*dc         41*/
      go to 140                                                                 !/*dc         42*/
  120 cst=6.d-1+4.d-2*(flti)                                                    !/*dc         43*/
      go to 140                                                                 !/*dc         44*/
  130 cst=7.8d-1+2.d-2*(flti)                                                   !/*dc         45*/
  140 continue                                                                  !/*dc         46*/
      r=rang()                                                                  !/*dc         47*/
      if (r.gt.5.d-1) go to 30                                                  !/*dc         48*/
      cst=-(cst)                                                                !/*dc         49*/
      go to 30                                                                  !/*dc         50*/
      end                                                                       !/*dc         51*/

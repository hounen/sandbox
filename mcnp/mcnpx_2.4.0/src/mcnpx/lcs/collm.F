!     $Id: collm.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                        !/*co          1*/
      subroutine collm (m)                                                      !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
#include <lcs/btcom.h>                                                          /*!co          4*/
      univ=e(2)+col(6)-col(11)                                                  !/*co          5*/
      unive=e(1)+col(11)                                                        !/*co          6*/
      univer=col(1)+col(6)                                                      !/*co          7*/
      k=16                                                                      !/*co          8*/
      do 10 i=1,9,4                                                             !/*co          9*/
      col(k)=(pxyz(i)*univ-pxyz(i+1)*unive)/univer                              !/*co         10*/
      col(k+3)=(pxyz(i)+pxyz(i+1))/col(1)                                       !/*co         11*/
!     vx                                                                        !/*co         12*/
   10 k=k+1                                                                     !/*co         13*/
      col(22)=(pxyz(10)*pxyz(5)-pxyz(9)*pxyz(6))/col(1)                         !/*co         14*/
!     qx                                                                        !/*co         15*/
      col(23)=(pxyz(2)*pxyz(9)-pxyz(10)*pxyz(1))/col(1)                         !/*co         16*/
!     qy                                                                        !/*co         17*/
      col(24)=(pxyz(6)*pxyz(1)-pxyz(5)*pxyz(2))/col(1)                          !/*co         18*/
      a=snt/col(14)                                                             !/*co         19*/
      b=a*col(10)                                                               !/*co         20*/
!     (-beta*cos phi*sin theta/alpha + cos theta)/p1p*p3p                       !/*co         21*/
      univ=col(10)*(cst-a*sopc*col(13))/col(12)                                 !/*co         22*/
      unive=b*sops/col(12)                                                      !/*co         23*/
!     p3p*sin phi*sin theta/p1p*alpha                                           !/*co         24*/
      univer=(sopc*b)+((e(3)+col(9))/(col(7)+dp1))                              !/*co         25*/
!     cos phi*sin theta*p3p/alpha  +  (e3+e3p)/(dp1+gamma)                      !/*co         26*/
      k=19                                                                      !/*co         27*/
      do 20 i=3,11,4                                                            !/*co         28*/
      pxyz(i)=col(k)*univer+col(k+3)*unive+col(k-3)*univ                        !/*co         29*/
   20 k=k+1                                                                     !/*co         30*/
      if (m) 30,40,30                                                           !/*co         31*/
   30 if (pt(15)) 40,60,40                                                      !/*co         32*/
   40 do 50 i=1,9,4                                                             !/*co         33*/
   50 pxyz(i+3)=pxyz(i)+pxyz(i+1)-pxyz(i+2)                                     !/*co         34*/
      if (m) 60,120,60                                                          !/*co         35*/
   60 if (pt(3)) 70,90,70                                                       !/*co         36*/
   70 pt(4)=pm(3)                                                               !/*co         37*/
      i1=3                                                                      !/*co         38*/
   80 i2=-1                                                                     !/*co         39*/
      call pstor                                                                !/*co         40*/
      if (i1-3) 90,90,110                                                       !/*co         41*/
   90 if (pt(15)) 100,110,100                                                   !/*co         42*/
  100 pt(16)=dncms                                                              !/*co         43*/
!     nucleon mass per cm                                                       !/*co         44*/
      i1=4                                                                      !/*co         45*/
      go to 80                                                                  !/*co         46*/
  110 pt(27)=dp0                                                                !/*co         47*/
      pt(39)=dp0                                                                !/*co         48*/
  120 return                                                                    !/*co         49*/
      end                                                                       !/*co         50*/

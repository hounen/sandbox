!     $Id: eint.F,v 1.4 2002/03/27 18:14:49 gwm Exp $                         !/*ei          1*/
      function eint (u,e,f,n,is)                                                !/*ei          2*/
!  B&P ROUTINE                                                                  !/*ei          3*/
!     linear interpolation  is=1                                                !/*ei          4*/
!     quadratic interpolation  is=2                                             !/*ei          5*/
#include <lcs/double.h>                                                         /*!ei          6*/
      dimension e(n), f(n)                                                      !/*ei          7*/
      if (is.le.0) eint=dp0                                                     !/*ei          8*/
      if (is.lt.0) return                                                       !/*ei          9*/
      if (u-e(1)) 10,10,20                                                      !/*ei         10*/
   10 x1=e(1)                                                                   !/*ei         11*/
      x2=e(2)                                                                   !/*ei         12*/
      x3=e(3)                                                                   !/*ei         13*/
      y1=f(1)                                                                   !/*ei         14*/
      y2=f(2)                                                                   !/*ei         15*/
      y3=f(3)                                                                   !/*ei         16*/
      go to 70                                                                  !/*ei         17*/
   20 if (u-e(n-1)) 40,30,30                                                    !/*ei         18*/
   30 x1=e(n-2)                                                                 !/*ei         19*/
      x2=e(n-1)                                                                 !/*ei         20*/
      x3=e(n)                                                                   !/*ei         21*/
      y1=f(n-2)                                                                 !/*ei         22*/
      y2=f(n-1)                                                                 !/*ei         23*/
      y3=f(n)                                                                   !/*ei         24*/
      go to 70                                                                  !/*ei         25*/
   40 n1=n-1                                                                    !/*ei         26*/
      do 60 j=2,n1                                                              !/*ei         27*/
      if (u-e(j)) 50,60,60                                                      !/*ei         28*/
   50 x1=e(j-1)                                                                 !/*ei         29*/
      x2=e(j)                                                                   !/*ei         30*/
      x3=e(j+1)                                                                 !/*ei         31*/
      y1=f(j-1)                                                                 !/*ei         32*/
      y2=f(j)                                                                   !/*ei         33*/
      y3=f(j+1)                                                                 !/*ei         34*/
      go to 70                                                                  !/*ei         35*/
   60 continue                                                                  !/*ei         36*/
   70 continue                                                                  !/*ei         37*/
      if (is.ne.2.or.n.eq.2) go to 80                                           !/*ei         38*/
      eint=y1*(((u-x2)*(u-x3))/((x1-x2)*(x1-x3)))+y2*(((u-x1)*(u-x3))/(         !/*ei         39*/
     & (x2-x1)*(x2-x3)))+y3*(((u-x1)*(u-x2))/((x3-x1)*(x3-x2)))                 !/*ei         40*/
   80 continue                                                                  !/*ei         41*/
      if (is.eq.1.or.n.eq.2) eint=y1+(u-x1)*(y2-y1)/(x2-x1)                     !/*ei         42*/
      return                                                                    !/*ei         43*/
      end                                                                       !/*ei         44*/

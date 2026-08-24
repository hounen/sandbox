!     $Id: binom1.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                       !/*bi          1*/
      function binom1 (m,l)                                                     !/*bi          2*/
#include <lcs/double.h>                                                         /*!bi          3*/
      parameter (ln=100)                                                        !/*bi          4*/
      dimension f(0:ln)                                                         !/*bi          5*/
      data f /dp0,ln*dp0/                                                       !/*bi          6*/
      if (m.lt.0) call expirx(1,'BINOM1','m.lt.0')                              !/*bi          7*/
      if (m.gt.ln) call expirx(1,'BINOM1','m.gt.ln')                            !/*bi          8*/
      n=min(l,m-l,7)                                                            !/*bi          9*/
      n=max(n,-1)                                                               !/*bi         10*/
      k=n+2                                                                     !/*bi         11*/
      x=m                                                                       !/*bi         12*/
      go to (10,20,30,40,50,60,70,80,100), k                                    !/*bi         13*/
   10 binom1=dp0                                                                !/*bi         14*/
      go to 90                                                                  !/*bi         15*/
   20 binom1=dp1                                                                !/*bi         16*/
      go to 90                                                                  !/*bi         17*/
   30 binom1=x                                                                  !/*bi         18*/
      go to 90                                                                  !/*bi         19*/
   40 binom1=dph*x*(x-dp1)                                                      !/*bi         20*/
      go to 90                                                                  !/*bi         21*/
   50 binom1=x*(x-dp1)*(x-dp2)/dp6                                              !/*bi         22*/
      go to 90                                                                  !/*bi         23*/
   60 binom1=x*(x-dp1)*(x-dp2)*(x-dp3)/dp24                                     !/*bi         24*/
      go to 90                                                                  !/*bi         25*/
   70 binom1=x*(x-dp1)*(x-dp2)*(x-dp3)*(x-dp4)/dp120                            !/*bi         26*/
      go to 90                                                                  !/*bi         27*/
   80 binom1=x*(x-dp1)*(x-dp2)*(x-dp3)*(x-dp4)*(x-dp5)/dp720                    !/*bi         28*/
   90 continue                                                                  !/*bi         29*/
      return                                                                    !/*bi         30*/
  100 continue                                                                  !/*bi         31*/
      if (f(ln).gt.dp0) go to 120                                               !/*bi         32*/
      do 110 i=2,ln                                                             !/*bi         33*/
      xi=i                                                                      !/*bi         34*/
      f(i)=f(i-1)+log(xi)                                                       !/*bi         35*/
  110 continue                                                                  !/*bi         36*/
  120 continue                                                                  !/*bi         37*/
      binom1=exp(f(m)-f(l)-f(m-l))                                              !/*bi         38*/
      go to 90                                                                  !/*bi         39*/
      end                                                                       !/*bi         40*/

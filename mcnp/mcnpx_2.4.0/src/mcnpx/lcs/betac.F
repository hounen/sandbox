!     $Id: betac.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                        !/*be          1*/
      function betac (m,n)                                                      !/*be          2*/
#include <lcs/double.h>                                                         /*!be          3*/
!                                                                               !/*be          4*/
!  BETAC samples the beta distribution given by                                 !/*be          5*/
!           p(v)=v**(m-1)(1-v)**(n-1)/beta(m,n)                                 !/*be          6*/
!  LA-9721-MS C75.                                                              !/*be          7*/
!                                                                               !/*be          8*/
      r1=dp1                                                                    !/*be          9*/
      do 10 i=1,m                                                               !/*be         10*/
      r1=r1*rang()                                                              !/*be         11*/
   10 continue                                                                  !/*be         12*/
      x=-log(r1)                                                                !/*be         13*/
      r2=dp1                                                                    !/*be         14*/
      do 20 i=1,n                                                               !/*be         15*/
      r2=r2*rang()                                                              !/*be         16*/
   20 continue                                                                  !/*be         17*/
      y=-log(r2)                                                                !/*be         18*/
      betac=x/(x+y)                                                             !/*be         19*/
      return                                                                    !/*be         20*/
      end                                                                       !/*be         21*/

!     $Id: boost.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                        !/*bo          1*/
      subroutine boost (e1,p1,e2,p2,beta,d)                                     !/*bo          2*/
! BOOST  calls   SQRT                                                           !/*bo          3*/
! called by ISLINK                                                              !/*bo          4*/
#include <lcs/double.h>                                                         /*!bo          5*/
      dimension p1(3), p2(3), d(3)                                              !/*bo          6*/
      gamma=dp1/sqrt(dp1-beta**2)                                               !/*bo          7*/
      betap=beta*(d(1)*p1(1)+d(2)*p1(2)+d(3)*p1(3))                             !/*bo          8*/
      fac=gamma*betap/(gamma+dp1)+e1                                            !/*bo          9*/
      do 10 i=1,3                                                               !/*bo         10*/
   10 p2(i)=p1(i)+beta*gamma*fac*d(i)                                           !/*bo         11*/
      e2=gamma*(e1+betap)                                                       !/*bo         12*/
      return                                                                    !/*bo         13*/
      end                                                                       !/*bo         14*/

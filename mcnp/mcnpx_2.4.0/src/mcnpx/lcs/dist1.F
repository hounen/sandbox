!     $Id: dist1.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                        !/*di          1*/
      function dist1 (a,b)                                                      !/*di          2*/
#include <lcs/double.h>                                                         /*!di          3*/
!                                                                               !/*di          4*/
! samples from the distribution                                                 !/*di          5*/
! P(x) : exp(ax)+b*exp(-ax) for -1<x<+1 and 0<=b<=1                             !/*di          6*/
!                                                                               !/*di          7*/
      xa=exp(a)                                                                 !/*di          8*/
      xai=dp1/xa                                                                !/*di          9*/
      r=rang()                                                                  !/*di         10*/
      if (b.eq.dp0) then                                                        !/*di         11*/
        y=r*(xa-xai)+xai                                                        !/*di         12*/
      else                                                                      !/*di         13*/
        arg=r*(dp1+b)*(xa-xai)-b*xa+xai                                         !/*di         14*/
        y=dph*(arg+sqrt(arg**2+dp4*b))                                          !/*di         15*/
      endif                                                                     !/*di         16*/
      dist1=log(y)/a                                                            !/*di         17*/
      return                                                                    !/*di         18*/
      end                                                                       !/*di         19*/

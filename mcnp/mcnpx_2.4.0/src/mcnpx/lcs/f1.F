!     $Id: f1.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                           !/*f1          1*/
      function f1 (a,x,n,p,q,r)                                                 !/*f1          2*/
#include <lcs/double.h>                                                         /*!f1          3*/
      fn=n                                                                      !/*f1          4*/
      temp=(p*a**2+q*a+r)*a/fn-(dp3*p*a**2+dp2*q*a+r)*x/(fn+dp1)+((dp3*p        !/*f1          5*/
     & *a+q)/(fn+dp2)-p*x/(fn+dp3))*x**2                                        !/*f1          6*/
      f1=temp                                                                   !/*f1          7*/
      return                                                                    !/*f1          8*/
      end                                                                       !/*f1          9*/

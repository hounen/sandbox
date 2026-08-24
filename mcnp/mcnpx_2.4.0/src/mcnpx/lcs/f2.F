!     $Id: f2.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                           !/*f2          1*/
      function f2 (a,x,n,flamda,fmu,fnu)                                        !/*f2          2*/
#include <lcs/double.h>                                                         /*!f2          3*/
      fn=n                                                                      !/*f2          4*/
      temp=(flamda*a**2+fmu*a+fnu)/fn-(dp2*flamda*a+fmu)*x/(fn+dp1)             !/*f2          5*/
     & +flamda*x*x/(fn+dp2)                                                     !/*f2          6*/
      f2=temp                                                                   !/*f2          7*/
      return                                                                    !/*f2          8*/
      end                                                                       !/*f2          9*/

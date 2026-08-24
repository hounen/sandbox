!     $Id: f3.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                           !/*f3          1*/
      function f3 (a,x,n,fmu,fnu)                                               !/*f3          2*/
#include <lcs/double.h>                                                         /*!f3          3*/
      fn=n                                                                      !/*f3          4*/
      temp=(fmu*a+fnu)/fn-fmu*x/(fn+dp1)                                        !/*f3          5*/
      f3=temp                                                                   !/*f3          6*/
      return                                                                    !/*f3          7*/
      end                                                                       !/*f3          8*/

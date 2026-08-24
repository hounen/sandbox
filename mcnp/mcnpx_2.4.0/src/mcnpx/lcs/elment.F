!     $Id: elment.F,v 1.4 2002/03/27 18:15:02 gwm Exp $                       !/*el          1*/
      function elment (a,ex,ne)                                                 !/*el          2*/
#include <lcs/double.h>                                                         /*!el          3*/
      data factor /135.d0/                                                      !/*el          4*/
      x=ex/ne                                                                   !/*el          5*/
      temp=factor/(x*a**3)                                                      !/*el          6*/
      if (ex.lt.dp2) then                                                       !/*el          7*/
        temp=0.267261d0*temp*x                                                  !/*el          8*/
      elseif (ex.lt.7.d0) then                                                  !/*el          9*/
        temp=temp*sqrt(x/7.d0)                                                  !/*el         10*/
      elseif (ex.ge.15.d0) then                                                 !/*el         11*/
        temp=temp*sqrt(15.d0/x)                                                 !/*el         12*/
      endif                                                                     !/*el         13*/
      elment=temp                                                               !/*el         14*/
      return                                                                    !/*el         15*/
      end                                                                       !/*el         16*/

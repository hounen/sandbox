!     $Id: azirn.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                        !/*az          1*/
      subroutine azirn (sin,cos)                                                !/*az          2*/
#include <lcs/double.h>                                                         /*!az          3*/
   10 continue                                                                  !/*az          4*/
      r1=dp2*rang()-dp1                                                         !/*az          5*/
      r1sq=r1*r1                                                                !/*az          6*/
      r2=rang()                                                                 !/*az          7*/
      r2sq=r2*r2                                                                !/*az          8*/
      rsq=r1sq+r2sq                                                             !/*az          9*/
      if (dp1-rsq) 10,20,20                                                     !/*az         10*/
   20 continue                                                                  !/*az         11*/
      rsq=dph*rsq                                                               !/*az         12*/
      cos=(rsq-r1sq)/rsq                                                        !/*az         13*/
      sin=r1*r2/rsq                                                             !/*az         14*/
      return                                                                    !/*az         15*/
      end                                                                       !/*az         16*/

!     $Id: gtiso.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                        !/*gt          1*/
      subroutine gtiso (u,v,w)                                                  !/*gt          2*/
#include <lcs/double.h>                                                         /*!gt          3*/
   10 continue                                                                  !/*gt          4*/
      z=rang()                                                                  !/*gt          5*/
      x=0.687368d0*(dp2*rang()-dp1)                                             !/*gt          6*/
      y=0.687368d0*(dp2*rang()-dp1)                                             !/*gt          7*/
      xsq=x*x                                                                   !/*gt          8*/
      ysq=y*y                                                                   !/*gt          9*/
      zsq=z*z                                                                   !/*gt         10*/
      d=xsq+ysq+zsq                                                             !/*gt         11*/
      if (d*d-z) 20,20,10                                                       !/*gt         12*/
   20 u=dp2*x*z/d                                                               !/*gt         13*/
      v=dp2*y*z/d                                                               !/*gt         14*/
      w=(zsq-xsq-ysq)/d                                                         !/*gt         15*/
      return                                                                    !/*gt         16*/
      end                                                                       !/*gt         17*/

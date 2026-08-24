!     $Id: polrn.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                        !/*po          1*/
      subroutine polrn (cs,si)                                                  !/*po          2*/
#include <lcs/double.h>                                                         /*!po          3*/
      parameter (rparm1=1.53960072d0)                                           !/*po          4*/
   10 continue                                                                  !/*po          5*/
      r1=rang()                                                                 !/*po          6*/
      r2=rang()                                                                 !/*po          7*/
      r1s=r1*r1                                                                 !/*po          8*/
      r2s=r2*r2                                                                 !/*po          9*/
      e=r1s+r2s                                                                 !/*po         10*/
      p=dp2*r1*r2/e                                                             !/*po         11*/
      if (e.gt.rparm1*p) go to 10                                               !/*po         12*/
      si=p                                                                      !/*po         13*/
      cs=(r2s-r1s)/e                                                            !/*po         14*/
      return                                                                    !/*po         15*/
      end                                                                       !/*po         16*/

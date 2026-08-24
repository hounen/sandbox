!     $Id: swapp.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                        !/*sw          1*/
      subroutine swapp                                                          !/*sw          2*/
#include <lcs/double.h>                                                         /*!sw          3*/
#include <lcs/banka.h>                                                          /*!sw          4*/
      i=kind(1)                                                                 !/*sw          5*/
      kind(1)=kind(2)                                                           !/*sw          6*/
      kind(2)=i                                                                 !/*sw          7*/
      d=ep(1)                                                                   !/*sw          8*/
      ep(1)=ep(2)                                                               !/*sw          9*/
      ep(2)=d                                                                   !/*sw         10*/
      d=alpha(1)                                                                !/*sw         11*/
      alpha(1)=alpha(2)                                                         !/*sw         12*/
      alpha(2)=d                                                                !/*sw         13*/
      d=beta(1)                                                                 !/*sw         14*/
      beta(1)=beta(2)                                                           !/*sw         15*/
      beta(2)=d                                                                 !/*sw         16*/
      d=gam(1)                                                                  !/*sw         17*/
      gam(1)=gam(2)                                                             !/*sw         18*/
      gam(2)=d                                                                  !/*sw         19*/
      d=wtfas(1)                                                                !/*sw         20*/
      wtfas(1)=wtfas(2)                                                         !/*sw         21*/
      wtfas(2)=d                                                                !/*sw         22*/
      return                                                                    !/*sw         23*/
      end                                                                       !/*sw         24*/

!     $Id: ieen.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                         !/*ie          1*/
      function ieen (e)                                                         !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= ieen                         b.colborn                               !/*ie          4*/
!  IEHN   called by...   HILONX                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
#include <lcs/evalc.h>                                                          /*!ie          8*/
      do 10 i=1,ieeval                                                          !/*ie          9*/
      if (e.lt.eeval(i)) go to 20                                               !/*ie         10*/
   10 continue                                                                  !/*ie         11*/
      if (e.eq.eeval(ieeval)) go to 30                                          !/*ie         12*/
      ieen=-1                                                                   !/*ie         13*/
      return                                                                    !/*ie         14*/
   20 ieen=i-1                                                                  !/*ie         15*/
      return                                                                    !/*ie         16*/
   30 ieen=ieeval-1                                                             !/*ie         17*/
      return                                                                    !/*ie         18*/
      end                                                                       !/*ie         19*/

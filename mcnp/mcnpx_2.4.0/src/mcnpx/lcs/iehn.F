!     $Id: iehn.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ie          1*/
      function iehn (e)                                                         !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= iehn                         b.colborn                               !/*ie          4*/
!  IEHN   called by...   HILONX                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
#include <lcs/hilonc.h>                                                         /*!ie          8*/
      do 10 i=1,iemxhn                                                          !/*ie          9*/
      if (e.lt.evalhn(i)) go to 20                                              !/*ie         10*/
   10 continue                                                                  !/*ie         11*/
      if (e.eq.evalhn(iemxhn)) go to 30                                         !/*ie         12*/
      iehn=-1                                                                   !/*ie         13*/
      return                                                                    !/*ie         14*/
   20 iehn=i-1                                                                  !/*ie         15*/
      return                                                                    !/*ie         16*/
   30 iehn=iemxhn-1                                                             !/*ie         17*/
      return                                                                    !/*ie         18*/
      end                                                                       !/*ie         19*/

!     $Id: ienn.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ie          1*/
      function ienn (e)                                                         !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= ienn                         b.colborn                               !/*ie          4*/
!  IENN   called by...   NASANX                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
#include <lcs/nasanc.h>                                                         /*!ie          8*/
      do 10 i=1,iemxnn                                                          !/*ie          9*/
      if (e.lt.evalnn(i)) go to 20                                              !/*ie         10*/
   10 continue                                                                  !/*ie         11*/
      if (e.eq.evalnn(iemxnn)) go to 30                                         !/*ie         12*/
      ienn=-1                                                                   !/*ie         13*/
      return                                                                    !/*ie         14*/
   20 ienn=i-1                                                                  !/*ie         15*/
      return                                                                    !/*ie         16*/
   30 ienn=iemxnn-1                                                             !/*ie         17*/
      return                                                                    !/*ie         18*/
      end                                                                       !/*ie         19*/

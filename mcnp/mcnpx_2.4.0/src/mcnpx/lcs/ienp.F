!     $Id: ienp.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ie          1*/
      function ienp (e)                                                         !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= ienp                         b.colborn                               !/*ie          4*/
!  IENP   called by...   NASAPX                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
#include <lcs/nasapc.h>                                                         /*!ie          8*/
      do 10 i=1,iemxnp                                                          !/*ie          9*/
      if (e.lt.evalnp(i)) go to 20                                              !/*ie         10*/
   10 continue                                                                  !/*ie         11*/
      if (e.eq.evalnp(iemxnp)) go to 30                                         !/*ie         12*/
      ienp=-1                                                                   !/*ie         13*/
      return                                                                    !/*ie         14*/
   20 ienp=i-1                                                                  !/*ie         15*/
      return                                                                    !/*ie         16*/
   30 ienp=iemxnp-1                                                             !/*ie         17*/
      return                                                                    !/*ie         18*/
      end                                                                       !/*ie         19*/

!     $Id: ianp.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ia          1*/
      function ianp (a)                                                         !/*ia          2*/
!--------------------------------------------------------                       !/*ia          3*/
! subname= ianp                         b.colborn                               !/*ia          4*/
!  IANP   called by...   NASAPX                                                 !/*ia          5*/
!--------------------------------------------------------                       !/*ia          6*/
#include <lcs/double.h>                                                         /*!ia          7*/
#include <lcs/nasapc.h>                                                         /*!ia          8*/
      do 10 i=1,iamxnp                                                          !/*ia          9*/
      if (a.lt.avalnp(i)) go to 20                                              !/*ia         10*/
   10 continue                                                                  !/*ia         11*/
      if (a.eq.avalnp(iamxnp)) go to 30                                         !/*ia         12*/
      ianp=-1                                                                   !/*ia         13*/
      return                                                                    !/*ia         14*/
   20 ianp=i-1                                                                  !/*ia         15*/
      return                                                                    !/*ia         16*/
   30 ianp=iamxnp-1                                                             !/*ia         17*/
      return                                                                    !/*ia         18*/
      end                                                                       !/*ia         19*/

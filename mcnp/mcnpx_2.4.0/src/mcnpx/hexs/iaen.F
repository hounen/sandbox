!     $Id: iaen.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                         !/*ia          1*/
      function iaen (a)                                                         !/*ia          2*/
!--------------------------------------------------------                       !/*ia          3*/
! subname= iaen                         b.colborn                               !/*ia          4*/
!  IAHN   called by...   HILONX                                                 !/*ia          5*/
!--------------------------------------------------------                       !/*ia          6*/
#include <lcs/double.h>                                                         /*!ia          7*/
#include <lcs/evalc.h>                                                          /*!ia          8*/
      do 10 i=1,iaeval                                                          !/*ia          9*/
      if (a.lt.aeval(i)) go to 20                                               !/*ia         10*/
   10 continue                                                                  !/*ia         11*/
      if (a.eq.aeval(iaeval)) go to 30                                          !/*ia         12*/
      iaen=-1                                                                   !/*ia         13*/
      return                                                                    !/*ia         14*/
   20 iaen=i-1                                                                  !/*ia         15*/
      return                                                                    !/*ia         16*/
   30 iaen=iaeval-1                                                             !/*ia         17*/
      return                                                                    !/*ia         18*/
      end                                                                       !/*ia         19*/

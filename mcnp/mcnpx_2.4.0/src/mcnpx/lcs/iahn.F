!     $Id: iahn.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ia          1*/
      function iahn (a)                                                         !/*ia          2*/
!--------------------------------------------------------                       !/*ia          3*/
! subname= iahn                         b.colborn                               !/*ia          4*/
!  IAHN   called by...   HILONX                                                 !/*ia          5*/
!--------------------------------------------------------                       !/*ia          6*/
#include <lcs/double.h>                                                         /*!ia          7*/
#include <lcs/hilonc.h>                                                         /*!ia          8*/
      do 10 i=1,iamxhn                                                          !/*ia          9*/
      if (a.lt.avalhn(i)) go to 20                                              !/*ia         10*/
   10 continue                                                                  !/*ia         11*/
      if (a.eq.avalhn(iamxhn)) go to 30                                         !/*ia         12*/
      iahn=-1                                                                   !/*ia         13*/
      return                                                                    !/*ia         14*/
   20 iahn=i-1                                                                  !/*ia         15*/
      return                                                                    !/*ia         16*/
   30 iahn=iamxhn-1                                                             !/*ia         17*/
      return                                                                    !/*ia         18*/
      end                                                                       !/*ia         19*/

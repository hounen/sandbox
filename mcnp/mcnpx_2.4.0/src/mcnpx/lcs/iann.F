!     $Id: iann.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                         !/*ia          1*/
      function iann (a)                                                         !/*ia          2*/
!--------------------------------------------------------                       !/*ia          3*/
! subname= iann                         b.colborn                               !/*ia          4*/
!  IANN   called by...   NASANX                                                 !/*ia          5*/
!--------------------------------------------------------                       !/*ia          6*/
#include <lcs/double.h>                                                         /*!ia          7*/
#include <lcs/nasanc.h>                                                         /*!ia          8*/
      do 10 i=1,iamxnn                                                          !/*ia          9*/
      if (a.lt.avalnn(i)) go to 20                                              !/*ia         10*/
   10 continue                                                                  !/*ia         11*/
      if (a.eq.avalnn(iamxnn)) go to 30                                         !/*ia         12*/
      iann=-1                                                                   !/*ia         13*/
      return                                                                    !/*ia         14*/
   20 iann=i-1                                                                  !/*ia         15*/
      return                                                                    !/*ia         16*/
   30 iann=iamxnn-1                                                             !/*ia         17*/
      return                                                                    !/*ia         18*/
      end                                                                       !/*ia         19*/

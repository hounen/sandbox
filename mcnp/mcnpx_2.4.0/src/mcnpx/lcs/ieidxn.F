!     $Id: ieidxn.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                       !/*ie          1*/
      function ieidxn (e)                                                       !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= ieidxn                       b.colborn                               !/*ie          4*/
!  IEIDXN called by...   NSGELS                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
      parameter (ieused=5)                                                      !/*ie          8*/
#include <lcs/elstic.h>                                                         /*!ie          9*/
      if (e.gt.esn(ieused+1)) go to 30                                          !/*ie         10*/
      do 10 i=1,ieused                                                          !/*ie         11*/
      if (e.le.esn(i)) go to 20                                                 !/*ie         12*/
   10 continue                                                                  !/*ie         13*/
      ieidxn=ieused                                                             !/*ie         14*/
      return                                                                    !/*ie         15*/
   20 ieidxn=i-1                                                                !/*ie         16*/
      return                                                                    !/*ie         17*/
   30 do 40 i=ieused+2,iemaxn                                                   !/*ie         18*/
      if (e.le.esn(i)) go to 20                                                 !/*ie         19*/
   40 continue                                                                  !/*ie         20*/
      ieidxn=iemaxn                                                             !/*ie         21*/
      return                                                                    !/*ie         22*/
      end                                                                       !/*ie         23*/

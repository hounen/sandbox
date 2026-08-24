!     $Id: ieidxp.F,v 1.4 2002/03/27 18:15:07 gwm Exp $                       !/*ie          1*/
      function ieidxp (e)                                                       !/*ie          2*/
!--------------------------------------------------------                       !/*ie          3*/
! subname= ieidxp                       b.colborn                               !/*ie          4*/
!  IEIDXP called by...   PSGELS                                                 !/*ie          5*/
!--------------------------------------------------------                       !/*ie          6*/
#include <lcs/double.h>                                                         /*!ie          7*/
      parameter (ieused=5)                                                      !/*ie          8*/
#include <lcs/elstic.h>                                                         /*!ie          9*/
      do 10 i=1,iemaxp                                                          !/*ie         10*/
      if (e.le.esp(i)) go to 20                                                 !/*ie         11*/
   10 continue                                                                  !/*ie         12*/
      ieidxp=iemaxp                                                             !/*ie         13*/
      return                                                                    !/*ie         14*/
   20 ieidxp=i-1                                                                !/*ie         15*/
      return                                                                    !/*ie         16*/
      end                                                                       !/*ie         17*/

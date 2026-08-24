!     $Id: wght4.F,v 1.4 2002/03/27 18:16:27 gwm Exp $                        !/*wg          1*/
      function wght4 (test)                                                     !/*wg          2*/
#include <htape3x/com32b.h>                                                     /*!wg          3*/
!------ compute weight for isopt=3 type distribution.                           !/*wg          4*/
#include <htape3x/wtpar4.h>                                                     /*!wg          5*/
      do 10 i=1,nring                                                           !/*wg          6*/
      if (test.le.sring2(i)) go to 20                                           !/*wg          7*/
   10 continue                                                                  !/*wg          8*/
      go to 30                                                                  !/*wg          9*/
   20 wght4=sprob(i)                                                            !/*wg         10*/
   30 return                                                                    !/*wg         11*/
      end                                                                       !/*wg         12*/

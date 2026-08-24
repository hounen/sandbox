!     $Id: jptx.F,v 1.4 2002/03/27 18:14:00 gwm Exp $                         !/*jp          1*/
      function jptx (iza)                                                       !/*jp          2*/
#include <htape3x/com32b.h>                                                     /*!jp          3*/
#include <htape3x/big.h>                                                        /*!jp          4*/
      iz=iza/1000                                                               !/*jp          5*/
      iref1=iref(iz)                                                            !/*jp          6*/
      iref2=iref(iz+1)-1                                                        !/*jp          7*/
      do 10 i=iref1,iref2                                                       !/*jp          8*/
      if (iza.eq.izao(i)) go to 20                                              !/*jp          9*/
   10 continue                                                                  !/*jp         10*/
      jptx=0                                                                    !/*jp         11*/
      return                                                                    !/*jp         12*/
   20 continue                                                                  !/*jp         13*/
      jptx=i                                                                    !/*jp         14*/
      return                                                                    !/*jp         15*/
      end                                                                       !/*jp         16*/

!     $Id: signex.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                       !/*si          1*/
      subroutine signex                                                         !/*si          2*/
#include <lcs/double.h>                                                         /*!si          3*/
#include <lcs/btcom.h>                                                          /*!si          4*/
      fi=dp0                                                                    !/*si          5*/
   10 continue                                                                  !/*si          6*/
      x=rang()                                                                  !/*si          7*/
      z=x                                                                       !/*si          8*/
   20 continue                                                                  !/*si          9*/
      y=rang()                                                                  !/*si         10*/
      if (z-y) 50,50,30                                                         !/*si         11*/
   30 continue                                                                  !/*si         12*/
      z=rang()                                                                  !/*si         13*/
      if (z-y) 20,40,40                                                         !/*si         14*/
   40 fi=fi+dp1                                                                 !/*si         15*/
      go to 10                                                                  !/*si         16*/
   50 univ=x+fi                                                                 !/*si         17*/
      ex=ex+univ/sign                                                           !/*si         18*/
      return                                                                    !/*si         19*/
!     ex=distance in sampling routine                                           !/*si         20*/
!     exponential random divided by sigma ci region i                           !/*si         21*/
      end                                                                       !/*si         22*/

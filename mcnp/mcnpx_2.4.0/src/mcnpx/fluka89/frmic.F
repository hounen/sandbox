!     $Id: frmic.F,v 1.4 2002/03/27 18:14:44 gwm Exp $                        !/*fr          1*/
      subroutine frmic (gpart)                                                  !/*fr          2*/
!                                                                     *         !/*fr          3*/
!---------------------------------------------------------------------*         !/*fr          4*/
!                                                                     *         !/*fr          5*/
#include <lcs/double.h>                                                         /*!fr          6*/
#include <fluka89/flprm1.h>                                                     /*!fr          7*/
#include <fluka89/flprm2.h>                                                     /*!fr          8*/
#include <lcs/inout.h>                                                          /*!fr          9*/
      dimension g(3)                                                            !/*fr         10*/
      do 10 i=1,3                                                               !/*fr         11*/
      g(i)=rang()                                                               !/*fr         12*/
   10 continue                                                                  !/*fr         13*/
      if (g(3).lt.g(2)) go to 40                                                !/*fr         14*/
      if (g(3).lt.g(1)) go to 30                                                !/*fr         15*/
      gpart=g(3)                                                                !/*fr         16*/
   20 return                                                                    !/*fr         17*/
   30 gpart=g(1)                                                                !/*fr         18*/
      go to 20                                                                  !/*fr         19*/
   40 if (g(2).lt.g(1)) go to 30                                                !/*fr         20*/
      gpart=g(2)                                                                !/*fr         21*/
      go to 20                                                                  !/*fr         22*/
      end                                                                       !/*fr         23*/

!     $Id: fermic.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                       !/*fe          1*/
      subroutine fermic (gpart)                                                 !/*fe          2*/
!                                                                     *         !/*fe          3*/
!---------------------------------------------------------------------*         !/*fe          4*/
!                                                                     *         !/*fe          5*/
#include <lcs/double.h>                                                         /*!fe          6*/
#include <fluka89/flprm1.h>                                                     /*!fe          7*/
#include <fluka89/flprm2.h>                                                     /*!fe          8*/
#include <lcs/inout.h>                                                          /*!fe          9*/
      dimension g(3)                                                            !/*fe         10*/
      do 10 i=1,3                                                               !/*fe         11*/
      g(i)=rang()                                                               !/*fe         12*/
   10 continue                                                                  !/*fe         13*/
      if (g(3).lt.g(2)) go to 40                                                !/*fe         14*/
      if (g(3).lt.g(1)) go to 30                                                !/*fe         15*/
      gpart=g(3)                                                                !/*fe         16*/
   20 return                                                                    !/*fe         17*/
   30 gpart=g(1)                                                                !/*fe         18*/
      go to 20                                                                  !/*fe         19*/
   40 if (g(2).lt.g(1)) go to 30                                                !/*fe         20*/
      gpart=g(2)                                                                !/*fe         21*/
      go to 20                                                                  !/*fe         22*/
      end                                                                       !/*fe         23*/

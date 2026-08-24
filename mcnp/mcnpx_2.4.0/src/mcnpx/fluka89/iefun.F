!     $Id: iefun.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                        !/*ie          1*/
      function iefun (pl,ire)                                                   !/*ie          2*/
!                                                                     *         !/*ie          3*/
!---------------------------------------------------------------------*         !/*ie          4*/
!                                                                     *         !/*ie          5*/
#include <lcs/double.h>                                                         /*!ie          6*/
#include <fluka89/flprm1.h>                                                     /*!ie          7*/
#include <fluka89/flprm2.h>                                                     /*!ie          8*/
#include <lcs/inout.h>                                                          /*!ie          9*/
#include <fluka89/run1.h>                                                       /*!ie         10*/
#include <fluka89/redver.h>                                                     /*!ie         11*/
#include <fluka89/reac1.h>                                                      /*!ie         12*/
      ipla=ieii(ire)+1+1                                                        !/*ie         13*/
      iple=ieii(ire+1)                                                          !/*ie         14*/
      if (pl.lt.0.d0) go to 30                                                  !/*ie         15*/
      do 10 i=ipla,iple                                                         !/*ie         16*/
      j=i-ipla+1                                                                !/*ie         17*/
      if (pl.le.plabf(i)) go to 60                                              !/*ie         18*/
   10 continue                                                                  !/*ie         19*/
      i=iple                                                                    !/*ie         20*/
      if (eftes.gt.40.d0) go to 20                                              !/*ie         21*/
      eftes=eftes+1.                                                            !/*ie         22*/
      write (io,80) pl,j                                                        !/*ie         23*/
   20 continue                                                                  !/*ie         24*/
      go to 70                                                                  !/*ie         25*/
   30 continue                                                                  !/*ie         26*/
      do 40 i=ipla,iple                                                         !/*ie         27*/
      j=i-ipla+1                                                                !/*ie         28*/
      if (-pl.le.umo(i)) go to 60                                               !/*ie         29*/
   40 continue                                                                  !/*ie         30*/
      i=iple                                                                    !/*ie         31*/
      if (eftes.gt.40.d0) go to 50                                              !/*ie         32*/
      eftes=eftes+1.d0                                                          !/*ie         33*/
      write (io,80) pl,i                                                        !/*ie         34*/
   50 continue                                                                  !/*ie         35*/
   60 continue                                                                  !/*ie         36*/
   70 continue                                                                  !/*ie         37*/
      iefun=i                                                                   !/*ie         38*/
      return                                                                    !/*ie         39*/
!                                                                               !/*ie         40*/
   80 format (14h plab or -ecm=,e10.4,27h is out of considered range,7h         !/*ie         41*/
     &iefun=,i5)                                                                !/*ie         42*/
      end                                                                       !/*ie         43*/

!     $Id: skipper.F,v 1.4 2002/03/27 18:16:26 gwm Exp $                      !/*sk          1*/
      subroutine skipper (*)                                                    !/*sk          2*/
#include <htape3x/com32b.h>                                                     /*!sk          3*/
#include <htape3x/label.h>                                                      /*!sk          4*/
#include <htape3x/contrl.h>                                                     /*!sk          5*/
      ibad=0                                                                    !/*sk          6*/
      if (mnuc.le.0) go to 20                                                   !/*sk          7*/
! residual nucleus records                                                      !/*sk          8*/
      do 10 i=1,mnuc                                                            !/*sk          9*/
      read (nhst,end=30,iostat=ibad,err=30) idmm                                !/*sk         10*/
      if (ibad.ne.0) go to 30                                                   !/*sk         11*/
   10 continue                                                                  !/*sk         12*/
   20 continue                                                                  !/*sk         13*/
! above-cutoff records                                                          !/*sk         14*/
      if (nabov.gt.0) read (nhst,end=30,iostat=ibad,err=30) idmm                !/*sk         15*/
      if (ibad.ne.0) go to 30                                                   !/*sk         16*/
! below-cutoff records                                                          !/*sk         17*/
      if (nbelo.gt.0) read (nhst,end=30,iostat=ibad,err=30) idmm                !/*sk         18*/
      if (ibad.ne.0) go to 30                                                   !/*sk         19*/
      return                                                                    !/*sk         20*/
   30 return 1                                                                  !/*sk         21*/
      end                                                                       !/*sk         22*/

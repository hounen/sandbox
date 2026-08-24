!     $Id: bnkeka.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*bn          1*/
      double precision function bnkeka (it,to,amss,sqamss)                      !/*bn          2*/
!                                                                     *         !/*bn          3*/
!---------------------------------------------------------------------*         !/*bn          4*/
!                                                                     *         !/*bn          5*/
#include <lcs/double.h>                                                         /*!bn          6*/
#include <fluka89/flprm1.h>                                                     /*!bn          7*/
#include <fluka89/flprm2.h>                                                     /*!bn          8*/
#include <lcs/inout.h>                                                          /*!bn          9*/
!*** cascade parameter calculation                                              !/*bn         10*/
      dimension a(2), b(2), c(2), d(2)                                          !/*bn         11*/
      data a /1.d0,1.3d0/                                                       !/*bn         12*/
      data b /.15d0,.225d0/                                                     !/*bn         13*/
      data c /1.35d0,2.28d0/                                                    !/*bn         14*/
      data d /.3d0,.4d0/                                                        !/*bn         15*/
      if (to-0.1d0) 10,10,20                                                    !/*bn         16*/
   10 continue                                                                  !/*bn         17*/
      bnkeka=sqamss*b(it)*0.1d0                                                 !/*bn         18*/
      return                                                                    !/*bn         19*/
   20 continue                                                                  !/*bn         20*/
      if (to-10.d0) 30,40,40                                                    !/*bn         21*/
   30 continue                                                                  !/*bn         22*/
      bnkeka=0.1d0*sqamss*(0.5d0+a(it)*(1.d0+log10(to))**2)*d(it)               !/*bn         23*/
      return                                                                    !/*bn         24*/
   40 continue                                                                  !/*bn         25*/
      bnkeka=sqamss*0.1d0*c(it)                                                 !/*bn         26*/
      return                                                                    !/*bn         27*/
      end                                                                       !/*bn         28*/

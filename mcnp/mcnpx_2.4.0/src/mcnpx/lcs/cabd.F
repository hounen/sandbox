!     $Id: cabd.F,v 1.4 2002/03/27 18:14:56 gwm Exp $                         !/*ca          1*/
      block data cabd                                                           !/*ca          2*/
#include <lcs/double.h>                                                         /*!ca          3*/
#ifndef OLDM                                                                    /*!ca          4*/
#include <lcs/phprm2.h>                                                         /*!ca          5*/
#endif                                                                          /*!ca          6*/
#include <lcs/cacom.h>                                                          /*!ca          7*/
#ifdef OLDM                                                                     /*!ca          8*/
      data exmass /8.3651d0,7.5831d0,13.7222d0,15.8383d0,15.8193d0,3.608        !/*ca          9*/
     & 4d0/                                                                     !/*ca         10*/
      data zmass9 /9.395730d+02,9.387910d+02,1.876138d+03,2.809462d+03,2        !/*ca         11*/
     & .809443d+03,3.728440d+03/                                                !/*ca         12*/
#else                                                                           /*!ca         13*/
      data exmass /xsm10,xsm11,xsm21,xsm31,xsm32,xsm42/                         !/*ca         14*/
      data zmass9 /wneut,wprot,wdeut,wtrit,w_he3,walph/                         !/*ca         15*/
#endif                                                                          /*!ca         16*/
      end                                                                       !/*ca         17*/

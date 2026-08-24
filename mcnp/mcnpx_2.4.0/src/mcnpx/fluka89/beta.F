!     $Id: beta.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                         !/*be          1*/
      double precision function beta (x1,x2,bet)                                !/*be          2*/
!                                                                     *         !/*be          3*/
!---------------------------------------------------------------------*         !/*be          4*/
!                                                                     *         !/*be          5*/
#include <lcs/double.h>                                                         /*!be          6*/
#include <fluka89/flprm1.h>                                                     /*!be          7*/
#include <fluka89/flprm2.h>                                                     /*!be          8*/
#include <lcs/inout.h>                                                          /*!be          9*/
      ax=0.d0                                                                   !/*be         10*/
      betx1=bet*x1                                                              !/*be         11*/
      if (betx1.lt.70.d0) ax=-1.d0/bet**2*(betx1+1.d0)*exp(-betx1)              !/*be         12*/
      ay=1.d0/bet**2*(bet*x2+1.d0)*exp(-bet*x2)                                 !/*be         13*/
      beta=ax+ay                                                                !/*be         14*/
      return                                                                    !/*be         15*/
      end                                                                       !/*be         16*/

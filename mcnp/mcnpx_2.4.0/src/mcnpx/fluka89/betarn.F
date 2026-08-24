!     $Id: betarn.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                       !/*be          1*/
      double precision function betarn (gam,eta)                                !/*be          2*/
!                                                                     *         !/*be          3*/
!---------------------------------------------------------------------*         !/*be          4*/
!                                                                     *         !/*be          5*/
#include <lcs/double.h>                                                         /*!be          6*/
#include <fluka89/flprm1.h>                                                     /*!be          7*/
#include <fluka89/flprm2.h>                                                     /*!be          8*/
#include <lcs/inout.h>                                                          /*!be          9*/
      y=gamrn(1.d0,gam)                                                         !/*be         10*/
      z=gamrn(1.d0,eta)                                                         !/*be         11*/
      betarn=y/(y+z)                                                            !/*be         12*/
      return                                                                    !/*be         13*/
      end                                                                       !/*be         14*/

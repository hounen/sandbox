!     $Id: drelab.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                       !/*dr          1*/
      subroutine drelab (x,y,z,cote,site,cops,sips)                             !/*dr          2*/
!                                                                     *         !/*dr          3*/
!---------------------------------------------------------------------*         !/*dr          4*/
!                                                                     *         !/*dr          5*/
#include <lcs/double.h>                                                         /*!dr          6*/
#include <fluka89/flprm1.h>                                                     /*!dr          7*/
#include <fluka89/flprm2.h>                                                     /*!dr          8*/
#include <lcs/inout.h>                                                          /*!dr          9*/
      x1=cops*x-sips*cote*y+sips*site*z                                         !/*dr         10*/
      x2=sips*x+cops*cote*y-cops*site*z                                         !/*dr         11*/
      x3=site*y+cote*z                                                          !/*dr         12*/
      x=x1                                                                      !/*dr         13*/
      y=x2                                                                      !/*dr         14*/
      z=x3                                                                      !/*dr         15*/
      return                                                                    !/*dr         16*/
      end                                                                       !/*dr         17*/

!     $Id: kpois.F,v 1.4 2002/03/27 18:14:46 gwm Exp $                        !/*kp          1*/
      function kpois (explam)                                                   !/*kp          2*/
!                                                                     *         !/*kp          3*/
!---------------------------------------------------------------------*         !/*kp          4*/
!                                                                     *         !/*kp          5*/
#include <lcs/double.h>                                                         /*!kp          6*/
#include <fluka89/flprm1.h>                                                     /*!kp          7*/
#include <fluka89/flprm2.h>                                                     /*!kp          8*/
#include <lcs/inout.h>                                                          /*!kp          9*/
      y=1.d0                                                                    !/*kp         10*/
      kpoiss=0                                                                  !/*kp         11*/
   10 continue                                                                  !/*kp         12*/
      y=y*rang()                                                                !/*kp         13*/
      if (y.lt.explam) go to 20                                                 !/*kp         14*/
      kpoiss=kpoiss+1                                                           !/*kp         15*/
      go to 10                                                                  !/*kp         16*/
   20 continue                                                                  !/*kp         17*/
      kpois=kpoiss                                                              !/*kp         18*/
      return                                                                    !/*kp         19*/
      end                                                                       !/*kp         20*/

!     $Id: evqini.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                       !/*ev          1*/
      subroutine evqini (what,sdum)                                             !/*ev          2*/
!                                                                     *         !/*ev          3*/
!---------------------------------------------------------------------*         !/*ev          4*/
!                                                                     *         !/*ev          5*/
#include <lcs/double.h>                                                         /*!ev          6*/
#include <fluka89/flprm1.h>                                                     /*!ev          7*/
#include <fluka89/flprm2.h>                                                     /*!ev          8*/
#include <lcs/inout.h>                                                          /*!ev          9*/
!*ca beam                                                                       !/*ev         10*/
!                                                                               !/*ev         11*/
#include <fluka89/inpdt2.h>                                                     /*!ev         12*/
      character*8 sdum                                                          !/*ev         13*/
      dimension what(6)                                                         !/*ev         14*/
#include <fluka89/print.h>                                                      /*!ev         15*/
      call datar3                                                               !/*ev         16*/
      call hadden                                                               !/*ev         17*/
      call rchanw                                                               !/*ev         18*/
      call chanwt                                                               !/*ev         19*/
      ipri=0                                                                    !/*ev         20*/
      init=0                                                                    !/*ev         21*/
      lt=0                                                                      !/*ev         22*/
!REP  if (ijbeam.eq.30) call aabini (sdum)                                      !/*ev         23*/
      return                                                                    !/*ev         24*/
      end                                                                       !/*ev         25*/

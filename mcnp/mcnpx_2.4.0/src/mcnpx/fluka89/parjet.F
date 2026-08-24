!     $Id: parjet.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*pa          1*/
      subroutine parjet (ihad,i)                                                !/*pa          2*/
!                                                                     *         !/*pa          3*/
!---------------------------------------------------------------------*         !/*pa          4*/
!                                                                     *         !/*pa          5*/
#include <lcs/double.h>                                                         /*!pa          6*/
#include <fluka89/flprm1.h>                                                     /*!pa          7*/
#include <fluka89/flprm2.h>                                                     /*!pa          8*/
#include <lcs/inout.h>                                                          /*!pa          9*/
#include <fluka89/finpr2.h>                                                     /*!pa         10*/
#include <fluka89/part1.h>                                                      /*!pa         11*/
!                                                                               !/*pa         12*/
#include <fluka89/inpdt1.h>                                                     /*!pa         13*/
      ihad=1                                                                    !/*pa         14*/
      nref(1)=i                                                                 !/*pa         15*/
      pxf(1)=0.d0                                                               !/*pa         16*/
      pyf(1)=0.d0                                                               !/*pa         17*/
      pzf(1)=0.d0                                                               !/*pa         18*/
      hep(1)=am(i)                                                              !/*pa         19*/
      amf(1)=am(i)                                                              !/*pa         20*/
      ichf(1)=ich(i)                                                            !/*pa         21*/
      ibarf(1)=ibar(i)                                                          !/*pa         22*/
      anf(1)=aname(i)                                                           !/*pa         23*/
      return                                                                    !/*pa         24*/
      end                                                                       !/*pa         25*/

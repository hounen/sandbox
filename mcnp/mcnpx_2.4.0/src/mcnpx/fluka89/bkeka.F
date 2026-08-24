!     $Id: bkeka.F,v 1.4 2002/03/27 18:14:40 gwm Exp $                        !/*bk          1*/
      double precision function bkeka (it,to,amss)                              !/*bk          2*/
!                                                                     *         !/*bk          3*/
!---------------------------------------------------------------------*         !/*bk          4*/
!                                                                     *         !/*bk          5*/
#include <lcs/double.h>                                                         /*!bk          6*/
#include <fluka89/flprm1.h>                                                     /*!bk          7*/
#include <fluka89/flprm2.h>                                                     /*!bk          8*/
#include <lcs/inout.h>                                                          /*!bk          9*/
!*** nuclear cascade nucleon emission parameters                                !/*bk         10*/
      dimension a(2), b(2)                                                      !/*bk         11*/
      data a /0.11d0,0.1d0/                                                     !/*bk         12*/
      data b /0.21d0,0.20d0/                                                    !/*bk         13*/
      if (to-10.d0) 10,20,20                                                    !/*bk         14*/
   10 continue                                                                  !/*bk         15*/
!     energyadjust                                                              !/*bk         16*/
      enadj=1.d0                                                                !/*bk         17*/
      bkeka=enadj*(1.d0-0.001d0*amss)*(a(it)+0.01d0*to)                         !/*bk         18*/
      return                                                                    !/*bk         19*/
   20 continue                                                                  !/*bk         20*/
      bkeka=b(it)*(1.d0-0.001d0*amss)                                           !/*bk         21*/
      return                                                                    !/*bk         22*/
      end                                                                       !/*bk         23*/

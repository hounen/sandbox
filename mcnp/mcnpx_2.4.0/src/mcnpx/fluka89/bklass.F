!     $Id: bklass.F,v 1.4 2002/03/27 18:14:40 gwm Exp $                       !/*bk          1*/
      subroutine bklass (i,j,k,i8,i10)                                          !/*bk          2*/
!                                                                     *         !/*bk          3*/
!---------------------------------------------------------------------*         !/*bk          4*/
!                                                                     *         !/*bk          5*/
#include <lcs/double.h>                                                         /*!bk          6*/
#include <fluka89/flprm1.h>                                                     /*!bk          7*/
#include <fluka89/flprm2.h>                                                     /*!bk          8*/
#include <lcs/inout.h>                                                          /*!bk          9*/
!                                                                               !/*bk         10*/
#include <fluka89/inpdt2.h>                                                     /*!bk         11*/
      if (i) 20,20,10                                                           !/*bk         12*/
   10 continue                                                                  !/*bk         13*/
      call index2 (j,k,ind)                                                     !/*bk         14*/
      i8=ib08(i,ind)                                                            !/*bk         15*/
      i10=ib10(i,ind)                                                           !/*bk         16*/
      if (i8.le.0) i8=i10                                                       !/*bk         17*/
      return                                                                    !/*bk         18*/
   20 continue                                                                  !/*bk         19*/
      ii=iabs(i)                                                                !/*bk         20*/
      jj=iabs(j)                                                                !/*bk         21*/
      kk=iabs(k)                                                                !/*bk         22*/
      call index2 (jj,kk,ind)                                                   !/*bk         23*/
      i8=ia08(ii,ind)                                                           !/*bk         24*/
      i10=ia10(ii,ind)                                                          !/*bk         25*/
      if (i8.le.0) i8=i10                                                       !/*bk         26*/
      return                                                                    !/*bk         27*/
      end                                                                       !/*bk         28*/

!     $Id: trahad.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                       !/*tr          1*/
      subroutine trahad (n,naux,gam,bgx,bgy,bgz)                                !/*tr          2*/
!                                                                     *         !/*tr          3*/
!---------------------------------------------------------------------*         !/*tr          4*/
!                                                                     *         !/*tr          5*/
#include <lcs/double.h>                                                         /*!tr          6*/
#include <fluka89/flprm1.h>                                                     /*!tr          7*/
#include <fluka89/flprm2.h>                                                     /*!tr          8*/
#include <lcs/inout.h>                                                          /*!tr          9*/
#include <fluka89/hadpar.h>                                                     /*!tr         10*/
#include <fluka89/auxpar.h>                                                     /*!tr         11*/
      do 10 i=1,n                                                               !/*tr         12*/
      j=naux+i-1                                                                !/*tr         13*/
      amh(j)=ama(i)                                                             !/*tr         14*/
      ichh(j)=icha(i)                                                           !/*tr         15*/
      ibarh(j)=ibara(i)                                                         !/*tr         16*/
      anh(j)=ana(i)                                                             !/*tr         17*/
      nreh(j)=nrea(i)                                                           !/*tr         18*/
      call altra (gam,bgx,bgy,bgz,pxa(i),pya(i),pza(i),hepa(i),ppa,pxh(j        !/*tr         19*/
     & ),pyh(j),pzh(j),heph(j))                                                 !/*tr         20*/
   10 continue                                                                  !/*tr         21*/
      return                                                                    !/*tr         22*/
      end                                                                       !/*tr         23*/

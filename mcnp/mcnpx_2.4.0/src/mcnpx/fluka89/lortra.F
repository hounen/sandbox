!     $Id: lortra.F,v 1.4 2002/03/27 18:14:46 gwm Exp $                       !/*lo          1*/
      subroutine lortra (n,naux,gam,bgx,bgy,bgz)                                !/*lo          2*/
!                                                                     *         !/*lo          3*/
!---------------------------------------------------------------------*         !/*lo          4*/
!                                                                     *         !/*lo          5*/
#include <lcs/double.h>                                                         /*!lo          6*/
#include <fluka89/flprm1.h>                                                     /*!lo          7*/
#include <fluka89/flprm2.h>                                                     /*!lo          8*/
#include <lcs/inout.h>                                                          /*!lo          9*/
#include <fluka89/finpr2.h>                                                     /*!lo         10*/
#include <fluka89/auxpar.h>                                                     /*!lo         11*/
      do 10 i=1,n                                                               !/*lo         12*/
      j=naux+i-1                                                                !/*lo         13*/
      ama(j)=amf(i)                                                             !/*lo         14*/
      icha(j)=ichf(i)                                                           !/*lo         15*/
      ibara(j)=ibarf(i)                                                         !/*lo         16*/
      ana(j)=anf(i)                                                             !/*lo         17*/
      nrea(j)=nref(i)                                                           !/*lo         18*/
      call altra (gam,bgx,bgy,bgz,pxf(i),pyf(i),pzf(i),hep(i),ppa,pxa(j)        !/*lo         19*/
     & ,pya(j),pza(j),hepa(j))                                                  !/*lo         20*/
   10 continue                                                                  !/*lo         21*/
      return                                                                    !/*lo         22*/
      end                                                                       !/*lo         23*/

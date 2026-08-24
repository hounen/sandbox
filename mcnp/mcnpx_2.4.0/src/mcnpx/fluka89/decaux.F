!     $Id: decaux.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*de          1*/
      subroutine decaux (nhad,numu)                                             !/*de          2*/
!                                                                     *         !/*de          3*/
!---------------------------------------------------------------------*         !/*de          4*/
!                                                                     *         !/*de          5*/
#include <lcs/double.h>                                                         /*!de          6*/
#include <fluka89/flprm1.h>                                                     /*!de          7*/
#include <fluka89/flprm2.h>                                                     /*!de          8*/
#include <lcs/inout.h>                                                          /*!de          9*/
#include <fluka89/finpr1.h>                                                     /*!de         10*/
#include <fluka89/auxpar.h>                                                     /*!de         11*/
      do 10 i=1,nhad                                                            !/*de         12*/
      pxf(i)=pxa(i)                                                             !/*de         13*/
      pyf(i)=pya(i)                                                             !/*de         14*/
      pzf(i)=pza(i)                                                             !/*de         15*/
      hef(i)=hepa(i)                                                            !/*de         16*/
      amf(i)=ama(i)                                                             !/*de         17*/
      ichf(i)=icha(i)                                                           !/*de         18*/
      ibarf(i)=ibara(i)                                                         !/*de         19*/
      anf(i)=ana(i)                                                             !/*de         20*/
      nref(i)=nrea(i)                                                           !/*de         21*/
   10 continue                                                                  !/*de         22*/
      call decayy (nhad,numu)                                                   !/*de         23*/
      do 20 i=1,nhad                                                            !/*de         24*/
      pxa(i)=pxf(i)                                                             !/*de         25*/
      pya(i)=pyf(i)                                                             !/*de         26*/
      pza(i)=pzf(i)                                                             !/*de         27*/
      hepa(i)=hef(i)                                                            !/*de         28*/
      ama(i)=amf(i)                                                             !/*de         29*/
      icha(i)=ichf(i)                                                           !/*de         30*/
      ibara(i)=ibarf(i)                                                         !/*de         31*/
      ana(i)=anf(i)                                                             !/*de         32*/
      nrea(i)=nref(i)                                                           !/*de         33*/
   20 continue                                                                  !/*de         34*/
      return                                                                    !/*de         35*/
      end                                                                       !/*de         36*/

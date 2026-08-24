!     $Id: altra.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                        !/*al          1*/
      subroutine altra (ga,bgx,bgy,bgz,pcx,pcy,pcz,ec,p,px,py,pz,e)             !/*al          2*/
!                                                                     *         !/*al          3*/
!---------------------------------------------------------------------*         !/*al          4*/
!                                                                     *         !/*al          5*/
#include <lcs/double.h>                                                         /*!al          6*/
#include <fluka89/flprm1.h>                                                     /*!al          7*/
#include <fluka89/flprm2.h>                                                     /*!al          8*/
#include <lcs/inout.h>                                                          /*!al          9*/
      ep=pcx*bgx+pcy*bgy+pcz*bgz                                                !/*al         10*/
      e=ga*ec+ep                                                                !/*al         11*/
      pe=ep/(ga+1.d0)+ec                                                        !/*al         12*/
      px=pcx+bgx*pe                                                             !/*al         13*/
      py=pcy+bgy*pe                                                             !/*al         14*/
      pz=pcz+bgz*pe                                                             !/*al         15*/
      px=min(dp1p30,max(-dp1p30,px))                                            !/*al         16*/
      py=min(dp1p30,max(-dp1p30,py))                                            !/*al         17*/
      pz=min(dp1p30,max(-dp1p30,pz))                                            !/*al         18*/
      p=sqrt(px*px+py*py+pz*pz)                                                 !/*al         19*/
      return                                                                    !/*al         20*/
      end                                                                       !/*al         21*/

!     $Id: altraf.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                       !/*al          1*/
      subroutine altraf (ga,bga,cx,cy,cz,cod,cof,sif,pc,ec,p,px,py,pz,e)        !/*al          2*/
!                                                                     *         !/*al          3*/
!---------------------------------------------------------------------*         !/*al          4*/
!                                                                     *         !/*al          5*/
#include <lcs/double.h>                                                         /*!al          6*/
#include <fluka89/flprm1.h>                                                     /*!al          7*/
#include <fluka89/flprm2.h>                                                     /*!al          8*/
#include <lcs/inout.h>                                                          /*!al          9*/
      bgx=bga*cx                                                                !/*al         10*/
      bgy=bga*cy                                                                !/*al         11*/
      bgz=bga*cz                                                                !/*al         12*/
      cod2=cod*cod                                                              !/*al         13*/
      if (cod2.gt.1.d0) cod2=1.d0                                               !/*al         14*/
      sid=sqrt(1.d0-cod2)*pc                                                    !/*al         15*/
      pcx=sid*cof                                                               !/*al         16*/
      pcy=sid*sif                                                               !/*al         17*/
      pcz=cod*pc                                                                !/*al         18*/
      ep=pcx*bgx+pcy*bgy+pcz*bgz                                                !/*al         19*/
      e=ga*ec+ep                                                                !/*al         20*/
      pe=ep/(ga+1.d0)+ec                                                        !/*al         21*/
      px=pcx+bgx*pe                                                             !/*al         22*/
      py=pcy+bgy*pe                                                             !/*al         23*/
      pz=pcz+bgz*pe                                                             !/*al         24*/
      p=sqrt(px*px+py*py+pz*pz)                                                 !/*al         25*/
      pm=1.d0/p                                                                 !/*al         26*/
      px=px*pm                                                                  !/*al         27*/
      py=py*pm                                                                  !/*al         28*/
      pz=pz*pm                                                                  !/*al         29*/
      return                                                                    !/*al         30*/
      end                                                                       !/*al         31*/

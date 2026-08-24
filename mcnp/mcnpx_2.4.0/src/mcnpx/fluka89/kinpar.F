!     $Id: kinpar.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*ki          1*/
      subroutine kinpar (pxyz,e,rcpmv)                                          !/*ki          2*/
!                                                                     *         !/*ki          3*/
!---------------------------------------------------------------------*         !/*ki          4*/
!                                                                     *         !/*ki          5*/
#include <lcs/double.h>                                                         /*!ki          6*/
#include <fluka89/flprm1.h>                                                     /*!ki          7*/
#include <fluka89/flprm2.h>                                                     /*!ki          8*/
#include <lcs/inout.h>                                                          /*!ki          9*/
#include <fluka89/kinpa.h>                                                      /*!ki         10*/
      dimension pxyz(10), e(2)                                                  !/*ki         11*/
      pxta=pxyz(2)                                                              !/*ki         12*/
      pyta=pxyz(6)                                                              !/*ki         13*/
      pzta=pxyz(10)                                                             !/*ki         14*/
      psx=pxyz(1)+pxta                                                          !/*ki         15*/
      psy=pxyz(5)+pyta                                                          !/*ki         16*/
      psz=pxyz(9)+pzta                                                          !/*ki         17*/
      pges2=psx*psx+psy*psy+psz*psz                                             !/*ki         18*/
      pges=sqrt(pges2)                                                          !/*ki         19*/
      pgesm=1.d0/pges                                                           !/*ki         20*/
      pta2=pxta*pxta+pyta*pyta+pzta*pzta                                        !/*ki         21*/
      ptam=1.d0/sqrt(pta2)                                                      !/*ki         22*/
      umo2=((e(1)+e(2))**2-pges2)/(rcpmv*rcpmv)                                 !/*ki         23*/
      umo2=umo2*0.000001d0                                                      !/*ki         24*/
      ccx=psx*pgesm                                                             !/*ki         25*/
      ccy=psy*pgesm                                                             !/*ki         26*/
      ccz=psz*pgesm                                                             !/*ki         27*/
      cxta=pxta*ptam                                                            !/*ki         28*/
      cyta=pyta*ptam                                                            !/*ki         29*/
      czta=pzta*ptam                                                            !/*ki         30*/
      ga=(e(1)+e(2))*0.001d0/(rcpmv*sqrt(umo2))                                 !/*ki         31*/
      bga=sqrt(ga*ga-1.d0)                                                      !/*ki         32*/
      return                                                                    !/*ki         33*/
      end                                                                       !/*ki         34*/

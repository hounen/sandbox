!     $Id: sigint.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*si          1*/
      subroutine sigint (ire,plab,n,ie,amt,amn,ecm,si,itar)                     !/*si          2*/
!                                                                     *         !/*si          3*/
!---------------------------------------------------------------------*         !/*si          4*/
!                                                                     *         !/*si          5*/
#include <lcs/double.h>                                                         /*!si          6*/
#include <fluka89/flprm1.h>                                                     /*!si          7*/
#include <fluka89/flprm2.h>                                                     /*!si          8*/
#include <lcs/inout.h>                                                          /*!si          9*/
#include <fluka89/ablts1.h>                                                     /*!si         10*/
#include <fluka89/redver.h>                                                     /*!si         11*/
#include <fluka89/reac1.h>                                                      /*!si         12*/
      ie=iefun(plab,ire)                                                        !/*si         13*/
      if (ie.le.ieii(ire)) ie=ie+1                                              !/*si         14*/
      amt=am(itar)                                                              !/*si         15*/
      amn=am(n)                                                                 !/*si         16*/
      amn2=amn*amn                                                              !/*si         17*/
      amt2=amt*amt                                                              !/*si         18*/
      ecm=sqrt(amn2+amt2+2.d0*amt*sqrt(amn2+plab**2))                           !/*si         19*/
      ecmo=umo(ie)                                                              !/*si         20*/
      ecm1=umo(ie-1)                                                            !/*si         21*/
      decm=ecmo-ecm1                                                            !/*si         22*/
      dec=ecmo-ecm                                                              !/*si         23*/
      iiki=ikii(ire)+1                                                          !/*si         24*/
      eklim=-thresh(iiki)                                                       !/*si         25*/
      wok=siin(ie)                                                              !/*si         26*/
      wdk=wok-siin(ie-1)                                                        !/*si         27*/
      if (ecm.gt.ecmo) wdk=0.d0                                                 !/*si         28*/
      ielim=iefun(eklim,ire)                                                    !/*si         29*/
      delim=umo(ielim)+eklim+1.d-16                                             !/*si         30*/
      dete=(ecm-(ecmo-eklim)*.5d0)*2.d0                                         !/*si         31*/
      if (delim*delim-dete*dete) 20,20,10                                       !/*si         32*/
   10 decc=delim                                                                !/*si         33*/
      go to 30                                                                  !/*si         34*/
   20 decc=decm                                                                 !/*si         35*/
   30 continue                                                                  !/*si         36*/
      wkk=wok-wdk*dec/(decc+1.d-9)                                              !/*si         37*/
      if (wkk.lt.0.d0) wkk=0.d0                                                 !/*si         38*/
      si=wkk+1.d-12                                                             !/*si         39*/
      if (-eklim.gt.ecm) si=1.d-14                                              !/*si         40*/
      return                                                                    !/*si         41*/
      end                                                                       !/*si         42*/

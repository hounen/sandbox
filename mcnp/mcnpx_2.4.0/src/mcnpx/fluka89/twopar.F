!     $Id: twopar.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                       !/*tw          1*/
      subroutine twopar (e1,e2,p1,p2,cod1,cod2,cof1,cof2,sif1,sif2,it1          !/*tw          2*/
     & ,it2,umoo,ecm,p,n,am1,am2)                                               !/*tw          3*/
!                                                                     *         !/*tw          4*/
!---------------------------------------------------------------------*         !/*tw          5*/
!                                                                     *         !/*tw          6*/
#include <lcs/double.h>                                                         /*!tw          7*/
#include <fluka89/flprm1.h>                                                     /*!tw          8*/
#include <fluka89/flprm2.h>                                                     /*!tw          9*/
#include <lcs/inout.h>                                                          /*!tw         10*/
#include <fluka89/gamrd1.h>                                                     /*!tw         11*/
#include <fluka89/ablts1.h>                                                     /*!tw         12*/
      ama=am1                                                                   !/*tw         13*/
      amb=am2                                                                   !/*tw         14*/
      ama2=ama*ama                                                              !/*tw         15*/
      amb2=amb*amb                                                              !/*tw         16*/
      s=umoo*umoo                                                               !/*tw         17*/
      e1=(s-amb2+ama2)/(2.d0*umoo)                                              !/*tw         18*/
      e2=(s-ama2+amb2)/(2.d0*umoo)                                              !/*tw         19*/
      if (e1.lt.ama*1.00001d0) e1=ama*1.00001d0                                 !/*tw         20*/
      amte=e1*e1-ama2                                                           !/*tw         21*/
      amte=amte+1.d-18                                                          !/*tw         22*/
      p1=sqrt(amte)                                                             !/*tw         23*/
      p2=p1                                                                     !/*tw         24*/
      call cosi (cof1,sif1)                                                     !/*tw         25*/
      cof2=-cof1                                                                !/*tw         26*/
      sif2=-sif1                                                                !/*tw         27*/
      call tchoic (tr,p,p1,ecm,e1,it1,it2,n,am1,am2)                            !/*tw         28*/
      cod1=(tr-ama2-am(n)*am(n)+2.d0*ecm*e1)/(2.d0*p*p1+1.d-18)                 !/*tw         29*/
      if (cod1.gt.0.9999999d0) cod1=0.9999999d0                                 !/*tw         30*/
      cod2=-cod1                                                                !/*tw         31*/
      return                                                                    !/*tw         32*/
      end                                                                       !/*tw         33*/

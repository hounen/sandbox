!     $Id: twopad.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                       !/*tw          1*/
      subroutine twopad (umo,ecm1,ecm2,pcm1,pcm2,cod1,cof1,sif1,cod2            !/*tw          2*/
     & ,cof2,sif2,am1,am2)                                                      !/*tw          3*/
!                                                                     *         !/*tw          4*/
!---------------------------------------------------------------------*         !/*tw          5*/
!                                                                               !/*tw          6*/
#include <lcs/double.h>                                                         /*!tw          7*/
#include <fluka89/flprm1.h>                                                     /*!tw          8*/
#include <fluka89/flprm2.h>                                                     /*!tw          9*/
#include <lcs/inout.h>                                                          /*!tw         10*/
#include <fluka89/gamrd1.h>                                                     /*!tw         11*/
      umo2=umo*umo                                                              !/*tw         12*/
      am11=am1*am1                                                              !/*tw         13*/
      am22=am2*am2                                                              !/*tw         14*/
      ecm1=(umo2+am11-am22)/(2.d0*umo)                                          !/*tw         15*/
      ecm2=umo-ecm1                                                             !/*tw         16*/
      wau=ecm1**2-am11                                                          !/*tw         17*/
      if (wau.lt.0.d0) redu=redu-5.d0                                           !/*tw         18*/
      wau=abs(wau)                                                              !/*tw         19*/
      pcm1=sqrt(wau)                                                            !/*tw         20*/
      pcm2=pcm1                                                                 !/*tw         21*/
      call cosi (sif1,cof1)                                                     !/*tw         22*/
      cod1=rang()                                                               !/*tw         23*/
      cod1=2.d0*cod1-1.d0                                                       !/*tw         24*/
!REP                                                                            !/*tw         25*/
!      q=sqrt(dp1-cod1**2)                                                      !/*tw         26*/
!      sif1=q*sif1                                                              !/*tw         27*/
!      cof1=q*cof1                                                              !/*tw         28*/
!REP                                                                            !/*tw         29*/
      cod2=-cod1                                                                !/*tw         30*/
      cof2=-cof1                                                                !/*tw         31*/
      sif2=-sif1                                                                !/*tw         32*/
      return                                                                    !/*tw         33*/
      end                                                                       !/*tw         34*/

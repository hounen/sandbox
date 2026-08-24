!     $Id: matinp.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                       !/*ma          1*/
      subroutine matinp (mxa,nel,mel,a9,z9,fmf,arg,rho)                         !/*ma          2*/
!     ==================================================================        !/*ma          3*/
#include <lcs/double.h>                                                         /*!ma          4*/
#include <lcs/lptS.h>                                                           /*!ma          5*/
#include <lcs/inout.h>                                                          /*!ma          6*/
#include <lcs/concom.h>                                                         /*!ma          7*/
      dimension nel(1), mel(1), a9(1), z9(1), fmf(1), arg(1), rho(1)            !/*ma          8*/
      parameter ( rclsel = 2.8179409183694872d-13 )                             !/*ma          9*/
      parameter ( el_mev = 0.51099906d+00 )                                     !/*ma         10*/
      parameter ( sqrtpi = 1.772453850905516027298167483341d+00 )               !/*ma         11*/
      parameter ( alpfsc = 7.2973530791728595d-03 )                             !/*ma         12*/
      parameter ( rossi = 0.49d0 )                                              !/*ma         13*/
      parameter ( cmcs_1 = sqrtpi*rclsel*el_mev*1.d+12 )                        !/*ma         14*/
      parameter ( cmcs_2 = 1.d0/(rossi*alpfsc**2) )                             !/*ma         15*/
      fac1=log(cmcs_2)                                                          !/*ma         16*/
      do 20 m=1,mxmat                                                           !/*ma         17*/
      med=mcell(m)                                                              !/*ma         18*/
      if (med.le.0) go to 20                                                    !/*ma         19*/
      nel1=nel(m)                                                               !/*ma         20*/
      sumarg=dp0                                                                !/*ma         21*/
      do 10 l=1,nel1                                                            !/*ma         22*/
      kmat2=mel(m)+l-1                                                          !/*ma         23*/
      z=z9(kmat2)                                                               !/*ma         24*/
      a=a9(kmat2)                                                               !/*ma         25*/
      qlzza=log(z*a)                                                            !/*ma         26*/
      sumarg=fmf(kmat2)*z*(z+dp1)*(fac1-qlzza/dp3)+sumarg                       !/*ma         27*/
   10 continue                                                                  !/*ma         28*/
      atdens=rho(med)                                                           !/*ma         29*/
      arg(m)=cmcs_1*sqrt(sumarg*atdens)                                         !/*ma         30*/
   20 continue                                                                  !/*ma         31*/
!                                                                               !/*ma         32*/
      end                                                                       !/*ma         33*/

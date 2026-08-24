!     $Id: tchoic.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*tc          1*/
      subroutine tchoic (t,p,pp,e,ee,i,ii,n,am1,am2)                            !/*tc          2*/
!                                                                     *         !/*tc          3*/
!---------------------------------------------------------------------*         !/*tc          4*/
!                                                                     *         !/*tc          5*/
#include <lcs/double.h>                                                         /*!tc          6*/
#include <fluka89/flprm1.h>                                                     /*!tc          7*/
#include <fluka89/flprm2.h>                                                     /*!tc          8*/
#include <lcs/inout.h>                                                          /*!tc          9*/
#include <fluka89/gamrd1.h>                                                     /*!tc         10*/
#include <fluka89/ablts1.h>                                                     /*!tc         11*/
#include <fluka89/slope1.h>                                                     /*!tc         12*/
      ama=am1                                                                   !/*tc         13*/
      amb=am2                                                                   !/*tc         14*/
      if (i.gt.30.and.ii.gt.30) go to 20                                        !/*tc         15*/
      iii=ii                                                                    !/*tc         16*/
      am3=am2                                                                   !/*tc         17*/
      if (i.le.30) go to 10                                                     !/*tc         18*/
      iii=i                                                                     !/*tc         19*/
      am3=am1                                                                   !/*tc         20*/
   10 continue                                                                  !/*tc         21*/
      go to 30                                                                  !/*tc         22*/
   20 continue                                                                  !/*tc         23*/
      iii=ii                                                                    !/*tc         24*/
      am3=am2                                                                   !/*tc         25*/
      if (ama.le.amb) go to 30                                                  !/*tc         26*/
      iii=i                                                                     !/*tc         27*/
      am3=am1                                                                   !/*tc         28*/
   30 continue                                                                  !/*tc         29*/
      ib=ibar(iii)                                                              !/*tc         30*/
      ama=am3                                                                   !/*tc         31*/
      k=(ama-0.75d0)/0.05d0                                                     !/*tc         32*/
      if (k-2.lt.0) k=1                                                         !/*tc         33*/
      if (k-26.ge.0) k=25                                                       !/*tc         34*/
      if (ib) 50,40,50                                                          !/*tc         35*/
   40 bm=bbm(k)                                                                 !/*tc         36*/
      go to 60                                                                  !/*tc         37*/
   50 bm=bbb(k)                                                                 !/*tc         38*/
   60 continue                                                                  !/*tc         39*/
      tmin=-2.d0*(e*ee-p*pp)+am(n)**2+am1**2                                    !/*tc         40*/
      tmax=-2.d0*(e*ee+p*pp)+am(n)**2+am1**2                                    !/*tc         41*/
      vb=rang()                                                                 !/*tc         42*/
      if (vb.lt.0.2d0) bm=bm*0.1d0*0.5d0                                        !/*tc         43*/
      tmi=bm*tmin                                                               !/*tc         44*/
      tma=bm*tmax                                                               !/*tc         45*/
      etma=0.d0                                                                 !/*tc         46*/
      if (abs(tma).gt.120.d0) go to 70                                          !/*tc         47*/
      etma=exp(tma)                                                             !/*tc         48*/
   70 continue                                                                  !/*tc         49*/
      an=(1.d0/bm)*(exp(tmi)-etma)                                              !/*tc         50*/
      r=rang()                                                                  !/*tc         51*/
      t=(1.d0/bm)*log(etma+r*an*bm)                                             !/*tc         52*/
      return                                                                    !/*tc         53*/
      end                                                                       !/*tc         54*/

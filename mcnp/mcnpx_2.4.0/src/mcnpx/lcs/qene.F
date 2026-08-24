!     $Id: qene.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                         !/*qe          1*/
      subroutine qene (z)                                                       !/*qe          2*/
! QENE       calls...  ERRORF  SQRT                                             !/*qe          3*/
!     QENE   called by...   QOU17  QOU21                                        !/*qe          4*/
#include <lcs/double.h>                                                         /*!qe          5*/
      dimension z(380)                                                          !/*qe          6*/
#include <lcs/isobar.h>                                                         /*!qe          7*/
      cd=com_*1.d2                                                              !/*qe          8*/
      i=int(cd+dp1)                                                             !/*qe          9*/
      az=z(i)*1.0d-13                                                           !/*qe         10*/
      if (i.eq.1) go to 140                                                     !/*qe         11*/
      bz=z(i+1)*1.0d-13                                                         !/*qe         12*/
      if (101-(i+1)) 60,10,20                                                   !/*qe         13*/
   10 cz=bz+5.d-1*(bz-az)                                                       !/*qe         14*/
      go to 30                                                                  !/*qe         15*/
   20 cz=z(i+2)*1.0d-13                                                         !/*qe         16*/
   30 xz=cd-real(i-1)                                                           !/*qe         17*/
      sca=cz-az                                                                 !/*qe         18*/
! f(2)-f(0)                                                                     !/*qe         19*/
   40 sba=bz-az                                                                 !/*qe         20*/
! f(1)-f(0)                                                                     !/*qe         21*/
      sqa=az*az                                                                 !/*qe         22*/
! f(0)**2                                                                       !/*qe         23*/
      sqac=sqa-cz*cz                                                            !/*qe         24*/
! f(0)**2-f(2)**2                                                               !/*qe         25*/
      sqba=bz*bz-sqa                                                            !/*qe         26*/
! f(1)**2-f(0)**2                                                               !/*qe         27*/
      rb=sqac+sqba+sqba                                                         !/*qe         28*/
! (asq-csq)+2(bsq-asq)                                                          !/*qe         29*/
      rc=az*cz*sca-sba*(2.d0*az*bz+xz*(bz-cz)*sca)                              !/*qe         30*/
      ra=sca-sba-sba                                                            !/*qe         31*/
! (c-a)-2(b-a)                                                                  !/*qe         32*/
      if (ra.ne.dp0) go to 50                                                   !/*qe         33*/
      com_=az+xz*sba                                                            !/*qe         34*/
      go to 70                                                                  !/*qe         35*/
   50 disc=rb*rb-dp4*ra*rc                                                      !/*qe         36*/
      if (disc) 60,80,80                                                        !/*qe         37*/
! b**2-4ac                                                                      !/*qe         38*/
   60 call errorf ('qene')                                                      !/*qe         39*/
   70 continue                                                                  !/*qe         40*/
      com_=com_*1.0d+13                                                         !/*qe         41*/
      return                                                                    !/*qe         42*/
   80 disc=sqrt(disc)                                                           !/*qe         43*/
      plus=(disc-rb)/(ra+ra)                                                    !/*qe         44*/
      aminus=(-rb-disc)/(ra+ra)                                                 !/*qe         45*/
      if (i.eq.1) go to 150                                                     !/*qe         46*/
   90 if (plus.gt.bz) go to 110                                                 !/*qe         47*/
      if (plus.lt.az) go to 110                                                 !/*qe         48*/
      if (aminus.gt.bz) go to 100                                               !/*qe         49*/
      if (aminus.ge.az) go to 130                                               !/*qe         50*/
  100 com_=plus                                                                 !/*qe         51*/
      go to 70                                                                  !/*qe         52*/
  110 continue                                                                  !/*qe         53*/
      if (aminus.gt.bz.or.aminus.lt.az) then                                    !/*qe         54*/
        if (abs(aminus-bz).le.1.d-03*abs(aminus)) go to 120                     !/*qe         55*/
        if (abs(aminus-az).le.1.d-03*abs(aminus)) go to 120                     !/*qe         56*/
        go to 60                                                                !/*qe         57*/
      endif                                                                     !/*qe         58*/
  120 com_=aminus                                                               !/*qe         59*/
      go to 70                                                                  !/*qe         60*/
  130 ra=xz*sba+az                                                              !/*qe         61*/
      rb=abs(ra-aminus)                                                         !/*qe         62*/
      rc=abs(ra-plus)                                                           !/*qe         63*/
      if (rb.gt.rc) go to 100                                                   !/*qe         64*/
      go to 120                                                                 !/*qe         65*/
  140 cz=z(i+1)*1.0d-13                                                         !/*qe         66*/
      sca=cz-az                                                                 !/*qe         67*/
      bz=az+sca*7.071067812d-1                                                  !/*qe         68*/
      xz=cd+cd                                                                  !/*qe         69*/
      go to 40                                                                  !/*qe         70*/
! (cz-az)(cz-az)=c,cz=mass for r=1,az=mass for r=0, c=const.for parabola        !/*qe         71*/
! (m-az)(m-az)=dph*c,determines mass,bz,for r=1/2                               !/*qe         72*/
  150 bz=cz                                                                     !/*qe         73*/
      xz=xz-cd                                                                  !/*qe         74*/
      sba=cz-az                                                                 !/*qe         75*/
      go to 90                                                                  !/*qe         76*/
      end                                                                       !/*qe         77*/

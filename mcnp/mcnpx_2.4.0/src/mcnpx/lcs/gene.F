!     $Id: gene.F,v 1.4 2002/03/27 18:15:05 gwm Exp $                         !/*ge          1*/
      subroutine gene (z)                                                       !/*ge          2*/
#include <lcs/double.h>                                                         /*!ge          3*/
      dimension z(101)                                                          !/*ge          4*/
#include <lcs/btcom.h>                                                          /*!ge          5*/
      cd=com*1.d2                                                               !/*ge          6*/
      i=int(cd+dp1)                                                             !/*ge          7*/
      az=z(i)*1.0d-13                                                           !/*ge          8*/
      if (i.eq.1) go to 140                                                     !/*ge          9*/
      bz=z(i+1)*1.0d-13                                                         !/*ge         10*/
      if (101-(i+1)) 60,10,20                                                   !/*ge         11*/
   10 cz=bz+5.d-1*(bz-az)                                                       !/*ge         12*/
      go to 30                                                                  !/*ge         13*/
   20 cz=z(i+2)*1.0d-13                                                         !/*ge         14*/
   30 xz=cd-real(i-1)                                                           !/*ge         15*/
      sca=cz-az                                                                 !/*ge         16*/
! f(2)-f(0)                                                                     !/*ge         17*/
   40 sba=bz-az                                                                 !/*ge         18*/
! f(1)-f(0)                                                                     !/*ge         19*/
      sqa=az*az                                                                 !/*ge         20*/
! f(0)**2                                                                       !/*ge         21*/
      sqac=sqa-cz*cz                                                            !/*ge         22*/
! f(0)**2-f(2)**2                                                               !/*ge         23*/
      sqba=bz*bz-sqa                                                            !/*ge         24*/
! f(1)**2-f(0)**2                                                               !/*ge         25*/
      rb=sqac+sqba+sqba                                                         !/*ge         26*/
! (asq-csq)+2(bsq-asq)                                                          !/*ge         27*/
      rc=az*cz*sca-sba*(2.d0*az*bz+xz*(bz-cz)*sca)                              !/*ge         28*/
      ra=sca-sba-sba                                                            !/*ge         29*/
! (c-a)-2(b-a)                                                                  !/*ge         30*/
      if (ra.ne.dp0) go to 50                                                   !/*ge         31*/
      com=az+xz*sba                                                             !/*ge         32*/
      go to 70                                                                  !/*ge         33*/
   50 disc=rb*rb-dp4*ra*rc                                                      !/*ge         34*/
      if (disc) 60,80,80                                                        !/*ge         35*/
! b**2-4ac                                                                      !/*ge         36*/
   60 call errorf ('gene')                                                      !/*ge         37*/
   70 continue                                                                  !/*ge         38*/
      com=com*1.0d+13                                                           !/*ge         39*/
      return                                                                    !/*ge         40*/
   80 disc=sqrt(disc)                                                           !/*ge         41*/
      plus=(disc-rb)/(ra+ra)                                                    !/*ge         42*/
      aminus=(-rb-disc)/(ra+ra)                                                 !/*ge         43*/
      if (i.eq.1) go to 150                                                     !/*ge         44*/
   90 if (plus.gt.bz) go to 110                                                 !/*ge         45*/
      if (plus.lt.az) go to 110                                                 !/*ge         46*/
      if (aminus.gt.bz) go to 100                                               !/*ge         47*/
      if (aminus.ge.az) go to 130                                               !/*ge         48*/
  100 com=plus                                                                  !/*ge         49*/
      go to 70                                                                  !/*ge         50*/
  110 continue                                                                  !/*ge         51*/
      if (aminus.gt.bz.or.aminus.lt.az) then                                    !/*ge         52*/
        if (abs(aminus-bz).le.1.d-03*abs(aminus)) go to 120                     !/*ge         53*/
        if (abs(aminus-az).le.1.d-03*abs(aminus)) go to 120                     !/*ge         54*/
        go to 60                                                                !/*ge         55*/
      endif                                                                     !/*ge         56*/
  120 com=aminus                                                                !/*ge         57*/
      go to 70                                                                  !/*ge         58*/
  130 ra=xz*sba+az                                                              !/*ge         59*/
      rb=abs(ra-aminus)                                                         !/*ge         60*/
      rc=abs(ra-plus)                                                           !/*ge         61*/
      if (rb.gt.rc) go to 100                                                   !/*ge         62*/
      go to 120                                                                 !/*ge         63*/
  140 cz=z(i+1)*1.0d-13                                                         !/*ge         64*/
      sca=cz-az                                                                 !/*ge         65*/
      bz=az+sca*7.071067812d-1                                                  !/*ge         66*/
      xz=cd+cd                                                                  !/*ge         67*/
      go to 40                                                                  !/*ge         68*/
! (cz-az)(cz-az)=c,cz=mass for r=1,az=mass for r=0, c=const.for parabola        !/*ge         69*/
! (m-az)(m-az)=dph*c,determines mass,bz,for r=1/2                               !/*ge         70*/
  150 bz=cz                                                                     !/*ge         71*/
      xz=xz-cd                                                                  !/*ge         72*/
      sba=cz-az                                                                 !/*ge         73*/
      go to 90                                                                  !/*ge         74*/
      end                                                                       !/*ge         75*/

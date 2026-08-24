!     $Id: threpd.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*th          1*/
      subroutine threpd (umo,ecm1,ecm2,ecm3,pcm1,pcm2,pcm3,cod1,cof1            !/*th          2*/
     & ,sif1,cod2,cof2,sif2,cod3,cof3,sif3,am1,am2,am3)                         !/*th          3*/
!                                                                     *         !/*th          4*/
!---------------------------------------------------------------------*         !/*th          5*/
!                                                                     *         !/*th          6*/
#include <lcs/double.h>                                                         /*!th          7*/
#include <fluka89/flprm1.h>                                                     /*!th          8*/
#include <fluka89/flprm2.h>                                                     /*!th          9*/
#include <lcs/inout.h>                                                          /*!th         10*/
#include <fluka89/gamrd1.h>                                                     /*!th         11*/
#include <fluka89/drei2.h>                                                      /*!th         12*/
      dimension f(5), xx(5)                                                     !/*th         13*/
! ..  save eps                                                                  !/*th         14*/
      data eps /1.d-16/                                                         !/*th         15*/
      umoo=umo+umo                                                              !/*th         16*/
      uumo=umo                                                                  !/*th         17*/
      aam1=am1                                                                  !/*th         18*/
      aam2=am2                                                                  !/*th         19*/
      aam3=am3                                                                  !/*th         20*/
      gu=(am2+am3)**2                                                           !/*th         21*/
      go=(umo-am1)**2                                                           !/*th         22*/
      ufak=1.0000000000001d0                                                    !/*th         23*/
      if (gu.gt.go) ufak=0.9999999999999d0                                      !/*th         24*/
      ofak=2.d0-ufak                                                            !/*th         25*/
      gu=gu*ufak                                                                !/*th         26*/
      go=go*ofak                                                                !/*th         27*/
      ds2=(go-gu)/99.d0                                                         !/*th         28*/
      am11=am1*am1                                                              !/*th         29*/
      am22=am2*am2                                                              !/*th         30*/
      am33=am3*am3                                                              !/*th         31*/
      umo2=umo*umo                                                              !/*th         32*/
      rho2=0.d0                                                                 !/*th         33*/
      s22=gu                                                                    !/*th         34*/
      do 10 i=1,100                                                             !/*th         35*/
      s21=s22                                                                   !/*th         36*/
      s22=gu+(i-1.d0)*ds2                                                       !/*th         37*/
      rho1=rho2                                                                 !/*th         38*/
      rho2=xlamb(s22,umo2,am11)*xlamb(s22,am22,am33)/(s22+eps)                  !/*th         39*/
      if (rho2.lt.rho1) go to 20                                                !/*th         40*/
   10 continue                                                                  !/*th         41*/
   20 s2sup=(s22-s21)*.5d0+s21                                                  !/*th         42*/
      suprho=xlamb(s2sup,umo2,am11)*xlamb(s2sup,am22,am33)/(s2sup+eps)          !/*th         43*/
      suprho=suprho*1.05d0                                                      !/*th         44*/
      xo=s21-ds2                                                                !/*th         45*/
      if (gu.lt.go.and.xo.lt.gu) xo=gu                                          !/*th         46*/
      if (gu.gt.go.and.xo.gt.gu) xo=gu                                          !/*th         47*/
      xx(1)=xo                                                                  !/*th         48*/
      xx(3)=s22                                                                 !/*th         49*/
      x1=(xo+s22)*0.5d0                                                         !/*th         50*/
      xx(2)=x1                                                                  !/*th         51*/
      f(3)=rho2                                                                 !/*th         52*/
      f(1)=xlamb(xo,umo2,am11)*xlamb(xo,am22,am33)/(xo+eps)                     !/*th         53*/
      f(2)=xlamb(x1,umo2,am11)*xlamb(x1,am22,am33)/(x1+eps)                     !/*th         54*/
      do 50 i=1,16                                                              !/*th         55*/
      x4=(xx(1)+xx(2))*0.5d0                                                    !/*th         56*/
      x5=(xx(2)+xx(3))*0.5d0                                                    !/*th         57*/
      f(4)=xlamb(x4,umo2,am11)*xlamb(x4,am22,am33)/(x4+eps)                     !/*th         58*/
      f(5)=xlamb(x5,umo2,am11)*xlamb(x5,am22,am33)/(x5+eps)                     !/*th         59*/
      xx(4)=x4                                                                  !/*th         60*/
      xx(5)=x5                                                                  !/*th         61*/
      do 30 ii=1,5                                                              !/*th         62*/
      ia=ii                                                                     !/*th         63*/
      do 30 iii=ia,5                                                            !/*th         64*/
      if (f(ii).ge.f(iii)) go to 30                                             !/*th         65*/
      fh=f(ii)                                                                  !/*th         66*/
      f(ii)=f(iii)                                                              !/*th         67*/
      f(iii)=fh                                                                 !/*th         68*/
      fh=xx(ii)                                                                 !/*th         69*/
      xx(ii)=xx(iii)                                                            !/*th         70*/
      xx(iii)=fh                                                                !/*th         71*/
   30 continue                                                                  !/*th         72*/
      suprho=f(1)                                                               !/*th         73*/
      s2sup=xx(1)                                                               !/*th         74*/
      do 40 ii=1,3                                                              !/*th         75*/
      ia=ii                                                                     !/*th         76*/
      do 40 iii=ia,3                                                            !/*th         77*/
      if (xx(ii).ge.xx(iii)) go to 40                                           !/*th         78*/
      fh=f(ii)                                                                  !/*th         79*/
      f(ii)=f(iii)                                                              !/*th         80*/
      f(iii)=fh                                                                 !/*th         81*/
      fh=xx(ii)                                                                 !/*th         82*/
      xx(ii)=xx(iii)                                                            !/*th         83*/
      xx(iii)=fh                                                                !/*th         84*/
   40 continue                                                                  !/*th         85*/
   50 continue                                                                  !/*th         86*/
      am23=(am2+am3)**2                                                         !/*th         87*/
      ith=0                                                                     !/*th         88*/
      redu=2.d0                                                                 !/*th         89*/
   60 continue                                                                  !/*th         90*/
      ith=ith+1                                                                 !/*th         91*/
      if (ith.gt.200) redu=-9.d0                                                !/*th         92*/
!REP  if (ith.gt.200) go to 90                                                  !/*th         93*/
      if (ith.gt.200) go to 70                                                  !/*th         94*/
      c=rang()                                                                  !/*th         95*/
      s2=am23+c*((umo-am1)**2-am23)                                             !/*th         96*/
      y=rang()                                                                  !/*th         97*/
      y=y*suprho                                                                !/*th         98*/
      row=xlamb(s2,umo2,am11)*xlamb(s2,am22,am33)/s2                            !/*th         99*/
      if (y.gt.row) go to 60                                                    !/*th        100*/
!REP                                                                            !/*th        101*/
!     s1=rang()                                                                 !/*th        102*/
!     s1=s1*row+am11+am22-(s2-umo2+am11)*(s2+am22-am33)/(2.d0*s2)-row*.5        !/*th        103*/
!    & d0                                                                       !/*th        104*/
!     s3=umo2+am11+am22+am33-s1-s2                                              !/*th        105*/
      ecm1=(umo2+am11-s2)/umoo                                                  !/*th        106*/
!     ecm2=(umo2+am22-s3)/umoo                                                  !/*th        107*/
!     ecm3=(umo2+am33-s1)/umoo                                                  !/*th        108*/
      pcm1=sqrt((ecm1+am1)*(ecm1-am1))                                          !/*th        109*/
!     pcm2=sqrt((ecm2+am2)*(ecm2-am2))                                          !/*th        110*/
!     pcm3=sqrt((ecm3+am3)*(ecm3-am3))                                          !/*th        111*/
!     call sfecfe (sfe,cfe)                                                     !/*th        112*/
!     if ((pcm1.le.1.d-3).or.(pcm2.le.1.d-3)) go to 70                          !/*th        113*/
!     costh=(ecm1*ecm2+0.5d0*(am11+am22-s1))/(pcm1*pcm2)                        !/*th        114*/
!     go to 80                                                                  !/*th        115*/
!  70 uw=rang()                                                                 !/*th        116*/
!     costh=(uw-.5d0)*2.d0                                                      !/*th        117*/
!  80 continue                                                                  !/*th        118*/
!     if (abs(costh).gt.0.9999999999999999d0) costh=sign(0.9999999999999        !/*th        119*/
!    & 999d0,costh)                                                             !/*th        120*/
!     if (redu.lt.1.d0) return                                                  !/*th        121*/
!     costh2=(pcm3*pcm3+pcm2*pcm2-pcm1*pcm1)/(2.d0*pcm2*pcm3)                   !/*th        122*/
!     if (abs(costh2).gt.0.9999999999999999d0) costh2=sign(0.99999999999        !/*th        123*/
!    & 99999d0,costh2)                                                          !/*th        124*/
!     sinth2=sqrt(1.d0-costh2*costh2)                                           !/*th        125*/
!     sinth1=costh2*sqrt(1.d0-costh*costh)-costh*sinth2                         !/*th        126*/
!     costh1=costh*costh2+sinth2*sqrt(1.d0-costh*costh)                         !/*th        127*/
!     cx11=-costh1                                                              !/*th        128*/
!     cy11=sinth1*cfe                                                           !/*th        129*/
!     cz11=sinth1*sfe                                                           !/*th        130*/
!     cx22=-costh2                                                              !/*th        131*/
!     cy22=-sinth2*cfe                                                          !/*th        132*/
!     cz22=-sinth2*sfe                                                          !/*th        133*/
!     call sfecfe (sif3,cof3)                                                   !/*th        134*/
!     cod3=rang()                                                               !/*th        135*/
!     cod3=2.d0*cod3-1.d0                                                       !/*th        136*/
!     sid3=sqrt(1.d0-cod3*cod3)                                                 !/*th        137*/
!     cod1=cx11*cod3+cz11*sid3                                                  !/*th        138*/
!     if ((1.d0-cod1*cod1).lt.1.d-14) write (itty,100) cod1,cof3,sid3           !/*th        139*/
!    & ,cx11,cz11                                                               !/*th        140*/
!     sid1=sqrt(1.d0-cod1*cod1)                                                 !/*th        141*/
!     cof1=(cx11*sid3*cof3-cy11*sif3-cz11*cod3*cof3)/sid1                       !/*th        142*/
!     sif1=(cx11*sid3*sif3+cy11*cof3-cz11*cod3*sif3)/sid1                       !/*th        143*/
!     cod2=cx22*cod3+cz22*sid3                                                  !/*th        144*/
!     sid2=sqrt(1.d0-cod2*cod2)                                                 !/*th        145*/
!     cof2=(cx22*sid3*cof3-cy22*sif3-cz22*cod3*cof3)/sid2                       !/*th        146*/
!     sif2=(cx22*sid3*sif3+cy22*cof3-cz22*cod3*sif3)/sid2                       !/*th        147*/
!  90 return                                                                    !/*th        148*/
!                                                                               !/*th        149*/
! 100 format (5f20.15)                                                          !/*th        150*/
      cod1=dp2*rang()-dp1                                                       !/*th        151*/
      sid1=sqrt(dp1-cod1**2)                                                    !/*th        152*/
      call sfecfe (sif1,cof1)                                                   !/*th        153*/
      bigmas=sqrt(umo**2+am1**2-dp2*umo*ecm1)                                   !/*th        154*/
      call twopad (bigmas,tki2,tki3,plr2,plr3,cd2,cf2,sf2,cd3,cf3,sf3           !/*th        155*/
     & ,am2,am3)                                                                !/*th        156*/
      gam=(umo-ecm1)/bigmas                                                     !/*th        157*/
      bgam=pcm1/bigmas                                                          !/*th        158*/
      txi=-cod1                                                                 !/*th        159*/
      tyi=-cof1*sid1                                                            !/*th        160*/
      tzi=-sif1*sid1                                                            !/*th        161*/
      call trafo (gam,bgam,txi,tyi,tzi,cd2,cf2,sf2,plr2,tki2,pcm2,cod2          !/*th        162*/
     & ,cof2,sif2,ecm2)                                                         !/*th        163*/
      sid2=sqrt(dp1-cod2**2)                                                    !/*th        164*/
      cof2=cof2/sid2                                                            !/*th        165*/
      sif2=sif2/sid2                                                            !/*th        166*/
      call trafo (gam,bgam,txi,tyi,tzi,cd3,cf3,sf3,plr3,tki3,pcm3,cod3          !/*th        167*/
     & ,cof3,sif3,ecm3)                                                         !/*th        168*/
      sid3=sqrt(dp1-cod3**2)                                                    !/*th        169*/
      cof3=cof3/sid3                                                            !/*th        170*/
      sif3=sif3/sid3                                                            !/*th        171*/
!REP                                                                            !/*th        172*/
   70 return                                                                    !/*th        173*/
      end                                                                       !/*th        174*/

!     $Id: difevt.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*di          1*/
      subroutine difevt (nhad,kproj,ktarg,pproj)                                !/*di          2*/
!                                                                     *         !/*di          3*/
!---------------------------------------------------------------------*         !/*di          4*/
!                                                                     *         !/*di          5*/
#include <lcs/double.h>                                                         /*!di          6*/
#include <fluka89/flprm1.h>                                                     /*!di          7*/
#include <fluka89/flprm2.h>                                                     /*!di          8*/
#include <lcs/inout.h>                                                          /*!di          9*/
#include <fluka89/auxpar.h>                                                     /*!di         10*/
#include <fluka89/cmsres.h>                                                     /*!di         11*/
#include <fluka89/finpr2.h>                                                     /*!di         12*/
#include <fluka89/hadpar.h>                                                     /*!di         13*/
!                                                                               !/*di         14*/
!=== inpdt2 ==========================================================*         !/*di         15*/
!                                                                               !/*di         16*/
!----------------------------------------------------------------------*        !/*di         17*/
!     include file: inpdt2 copy          created 29/3/90 by a. ferrari          !/*di         18*/
!     changes: none                                                             !/*di         19*/
!     included in the following subroutines or functions: not updated           !/*di         20*/
!                                                                               !/*di         21*/
!     t h i s   i s   a   c o p y   o f   i n p d a t   c r e a t e d           !/*di         22*/
!     t o   a v o i d   t o   d i r t y   t h e   b a m j e t                   !/*di         23*/
!     p a r a m e t e r s !!!!!!!!!!!!                                          !/*di         24*/
!                                                                               !/*di         25*/
!     description of the common block(s) and variable(s)                        !/*di         26*/
!                                                                               !/*di         27*/
!----------------------------------------------------------------------*        !/*di         28*/
!                                                                               !/*di         29*/
#include <fluka89/inpdt1.h>                                                     /*!di         30*/
#include <fluka89/part1.h>                                                      /*!di         31*/
      data unon /2.0d0/                                                         !/*di         32*/
      data unom /1.5d0/                                                         !/*di         33*/
#include <fluka89/print.h>                                                      /*!di         34*/
      dimension mquark(3,30)                                                    !/*di         35*/
      data mquark /1,1,2,-1,-1,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,-1        !/*di         36*/
     & ,-2,-2,0,0,0,0,0,0,0,0,0,1,-2,0,2,-1,0,1,-3,0,3,-1,0,1,2,3,-1,-2,        !/*di         37*/
     & -3,0,0,0,2,2,3,1,1,3,1,2,3,1,-1,0,2,-3,0,3,-2,0,2,-2,0,0,0,0,0,0,        !/*di         38*/
     & 0,0,0,0,0,0,0/                                                           !/*di         39*/
      ipri=0                                                                    !/*di         40*/
      asbamj=0.5d0                                                              !/*di         41*/
      b8bamj=0.4d0                                                              !/*di         42*/
      amproj=am(kproj)                                                          !/*di         43*/
      amtar=am(ktarg)                                                           !/*di         44*/
      eproj=sqrt(pproj**2+amproj**2)                                            !/*di         45*/
      umo=sqrt(amproj**2+amtar**2+2.d0*amtar*eproj)                             !/*di         46*/
      gamcm=(eproj+amtar)/umo                                                   !/*di         47*/
      bgcm=pproj/umo                                                            !/*di         48*/
      ibproj=ibar(kproj)                                                        !/*di         49*/
      ibtarg=ibar(ktarg)                                                        !/*di         50*/
      ipq1=mquark(1,kproj)                                                      !/*di         51*/
      ipq2=mquark(2,kproj)                                                      !/*di         52*/
      ipq3=mquark(3,kproj)                                                      !/*di         53*/
      iqt1=mquark(1,ktarg)                                                      !/*di         54*/
      iqt2=mquark(2,ktarg)                                                      !/*di         55*/
      iqt3=mquark(3,ktarg)                                                      !/*di         56*/
      iqp1=ipq1                                                                 !/*di         57*/
      iqp2=ipq2                                                                 !/*di         58*/
      iqp3=ipq3                                                                 !/*di         59*/
      unosea=3.d0                                                               !/*di         60*/
      if (kproj.gt.2) unosea=5.d0                                               !/*di         61*/
   10 continue                                                                  !/*di         62*/
      xsea=betarn(0.05d0,unosea)                                                !/*di         63*/
      xasea=betarn(0.05d0,unosea)                                               !/*di         64*/
      xpio=xsea+xasea                                                           !/*di         65*/
      if (xpio.ge.1.d0) go to 10                                                !/*di         66*/
      xhad=1.d0-xpio                                                            !/*di         67*/
      isam=2.d0*rang()+1.d0                                                     !/*di         68*/
      go to (20,80), isam                                                       !/*di         69*/
   20 continue                                                                  !/*di         70*/
      iidif=1                                                                   !/*di         71*/
      amch=sqrt(xpio)*umo                                                       !/*di         72*/
      if (amch.le.amtar+0.5d0) xpio=((amtar+0.5d0)/umo)**2                      !/*di         73*/
      if (amch.le.amtar+0.5d0) amch=amtar+0.5d0                                 !/*di         74*/
      eh1s=(umo**2+amproj**2-amch**2)/(2.d0*umo)                                !/*di         75*/
      if (eh1s.le.amproj) go to 10                                              !/*di         76*/
      eh2ex=umo-eh1s                                                            !/*di         77*/
      ph1s=sqrt(eh1s**2-amproj**2)                                              !/*di         78*/
      es=-2.d0/(b3bamj**2)*log(rang()*rang())                                   !/*di         79*/
      hps=sqrt(es*es+2.d0*es*am(1))                                             !/*di         80*/
      call sfecfe (sfe,cfe)                                                     !/*di         81*/
      ptxch1=hps*cfe                                                            !/*di         82*/
      ptych1=hps*sfe                                                            !/*di         83*/
      go to 40                                                                  !/*di         84*/
   30 continue                                                                  !/*di         85*/
      ptxch1=0.75d0*ptxch1                                                      !/*di         86*/
      ptych1=0.75d0*ptych1                                                      !/*di         87*/
   40 continue                                                                  !/*di         88*/
      px1=ptxch1                                                                !/*di         89*/
      py1=ptych1                                                                !/*di         90*/
      ach=ph1s**2-px1**2-py1**2                                                 !/*di         91*/
      if (ach.le.0.d0) go to 30                                                 !/*di         92*/
      pz1=sqrt(ach)                                                             !/*di         93*/
      gam1=eh2ex/amch                                                           !/*di         94*/
      bgx1=px1/amch                                                             !/*di         95*/
      bgy1=py1/amch                                                             !/*di         96*/
      bgz1=pz1/amch                                                             !/*di         97*/
      kpio=23                                                                   !/*di         98*/
      if (rang().le.0.5d0) kpio=26                                              !/*di         99*/
      ampio=am(kpio)                                                            !/*di        100*/
      epiol=(amch**2-ampio**2-amtar**2)/(2.d0*amtar)                            !/*di        101*/
      ppiol=sqrt(epiol**2-ampio**2)                                             !/*di        102*/
      call hadevt (nhad,kpio,ktarg,ppiol)                                       !/*di        103*/
      pxx1=0.d0                                                                 !/*di        104*/
      pyy1=0.d0                                                                 !/*di        105*/
      pzz1=0.d0                                                                 !/*di        106*/
      exxx=0.d0                                                                 !/*di        107*/
      do 50 i=1,nhad                                                            !/*di        108*/
      pxx1=pxx1+pxh(i)                                                          !/*di        109*/
      pyy1=pyy1+pyh(i)                                                          !/*di        110*/
      pzz1=pzz1+pzh(i)                                                          !/*di        111*/
      exxx=exxx+heph(i)                                                         !/*di        112*/
   50 continue                                                                  !/*di        113*/
      call trahad (nhad,1,gam1,-bgx1,-bgy1,-bgz1)                               !/*di        114*/
      pxx1=0.d0                                                                 !/*di        115*/
      pyy1=0.d0                                                                 !/*di        116*/
      pzz1=0.d0                                                                 !/*di        117*/
      exxx=0.d0                                                                 !/*di        118*/
      do 60 i=1,nhad                                                            !/*di        119*/
      pxx1=pxx1+pxh(i)                                                          !/*di        120*/
      pyy1=pyy1+pyh(i)                                                          !/*di        121*/
      pzz1=pzz1+pzh(i)                                                          !/*di        122*/
      exxx=exxx+heph(i)                                                         !/*di        123*/
   60 continue                                                                  !/*di        124*/
      nhad=nhad+1                                                               !/*di        125*/
      pxh(nhad)=px1                                                             !/*di        126*/
      pyh(nhad)=py1                                                             !/*di        127*/
      pzh(nhad)=pz1                                                             !/*di        128*/
      heph(nhad)=eh1s                                                           !/*di        129*/
      amh(nhad)=amproj                                                          !/*di        130*/
      ichh(nhad)=ich(kproj)                                                     !/*di        131*/
      ibarh(nhad)=ibar(kproj)                                                   !/*di        132*/
      anh(nhad)=aname(kproj)                                                    !/*di        133*/
      nreh(nhad)=kproj                                                          !/*di        134*/
      do 70 i=1,nhad                                                            !/*di        135*/
      pzz=pzh(i)                                                                !/*di        136*/
      hepp=heph(i)                                                              !/*di        137*/
      pzh(i)=gamcm*pzz+bgcm*hepp                                                !/*di        138*/
      heph(i)=gamcm*hepp+bgcm*pzz                                               !/*di        139*/
   70 continue                                                                  !/*di        140*/
      go to 140                                                                 !/*di        141*/
   80 continue                                                                  !/*di        142*/
      iidif=2                                                                   !/*di        143*/
      amch=sqrt(xpio)*umo                                                       !/*di        144*/
      if (amch.le.amproj+0.5d0) xpio=((amproj+0.5d0)/umo)**2                    !/*di        145*/
      if (amch.le.amproj+0.5d0) amch=amproj+0.5d0                               !/*di        146*/
      eh1s=(umo**2+amtar**2-amch**2)/(2.d0*umo)                                 !/*di        147*/
      if (eh1s.le.amtar) go to 10                                               !/*di        148*/
      eh2ex=umo-eh1s                                                            !/*di        149*/
      ph1s=sqrt(eh1s**2-amtar**2)                                               !/*di        150*/
      es=-2.d0/(b3bamj**2)*log(rang()*rang())                                   !/*di        151*/
      hps=sqrt(es*es+2.d0*es*am(1))                                             !/*di        152*/
      call sfecfe (sfe,cfe)                                                     !/*di        153*/
      ptxch1=hps*cfe                                                            !/*di        154*/
      ptych1=hps*sfe                                                            !/*di        155*/
      go to 100                                                                 !/*di        156*/
   90 continue                                                                  !/*di        157*/
      ptxch1=0.75d0*ptxch1                                                      !/*di        158*/
      ptych1=0.75d0*ptych1                                                      !/*di        159*/
  100 continue                                                                  !/*di        160*/
      px1=ptxch1                                                                !/*di        161*/
      py1=ptych1                                                                !/*di        162*/
      ach=ph1s**2-px1**2-py1**2                                                 !/*di        163*/
      if (ach.le.0.d0) go to 90                                                 !/*di        164*/
      pz1=sqrt(ach)                                                             !/*di        165*/
      gam1=eh2ex/amch                                                           !/*di        166*/
      bgx1=px1/amch                                                             !/*di        167*/
      bgy1=py1/amch                                                             !/*di        168*/
      bgz1=pz1/amch                                                             !/*di        169*/
      kpio=23                                                                   !/*di        170*/
!REP                                                                            !/*di        171*/
!        TEMPORARY fix: avoid possible infinite loop in HADEVT.                 !/*di        172*/
! ...    if (rang().le.0.5d0) kpio=26                                           !/*di        173*/
      if (kproj.ne.15.and.kproj.ne.16.and.kproj.ne.24.and.kproj.ne.25)          !/*di        174*/
     & then                                                                     !/*di        175*/
        if (rang().le.0.5d0) kpio=26                                            !/*di        176*/
      else                                                                      !/*di        177*/
        write (*,*) ' kpio=26 blocked; kproj=',kproj                            !/*di        178*/
      endif                                                                     !/*di        179*/
!REP                                                                            !/*di        180*/
      ampio=am(kpio)                                                            !/*di        181*/
      epiol=(amch**2-ampio**2-amproj**2)/(2.d0*amproj)                          !/*di        182*/
      ppiol=sqrt(epiol**2-ampio**2)                                             !/*di        183*/
      call hadevt (nhad,kpio,kproj,ppiol)                                       !/*di        184*/
      pxx1=0.d0                                                                 !/*di        185*/
      pyy1=0.d0                                                                 !/*di        186*/
      pzz1=0.d0                                                                 !/*di        187*/
      exxx=0.d0                                                                 !/*di        188*/
      do 110 i=1,nhad                                                           !/*di        189*/
      pxx1=pxx1+pxh(i)                                                          !/*di        190*/
      pyy1=pyy1+pyh(i)                                                          !/*di        191*/
      pzz1=pzz1+pzh(i)                                                          !/*di        192*/
      exxx=exxx+heph(i)                                                         !/*di        193*/
  110 continue                                                                  !/*di        194*/
      call trahad (nhad,1,gam1,-bgx1,-bgy1,-bgz1)                               !/*di        195*/
      pxx1=0.d0                                                                 !/*di        196*/
      pyy1=0.d0                                                                 !/*di        197*/
      pzz1=0.d0                                                                 !/*di        198*/
      exxx=0.d0                                                                 !/*di        199*/
      do 120 i=1,nhad                                                           !/*di        200*/
      pxx1=pxx1+pxh(i)                                                          !/*di        201*/
      pyy1=pyy1+pyh(i)                                                          !/*di        202*/
      pzh(i)=-pzh(i)                                                            !/*di        203*/
      pzz1=pzz1+pzh(i)                                                          !/*di        204*/
      exxx=exxx+heph(i)                                                         !/*di        205*/
  120 continue                                                                  !/*di        206*/
      nhad=nhad+1                                                               !/*di        207*/
      pxh(nhad)=px1                                                             !/*di        208*/
      pyh(nhad)=py1                                                             !/*di        209*/
      pzh(nhad)=-pz1                                                            !/*di        210*/
      heph(nhad)=eh1s                                                           !/*di        211*/
      amh(nhad)=amtar                                                           !/*di        212*/
      ichh(nhad)=ich(ktarg)                                                     !/*di        213*/
      ibarh(nhad)=ibar(ktarg)                                                   !/*di        214*/
      anh(nhad)=aname(ktarg)                                                    !/*di        215*/
      nreh(nhad)=ktarg                                                          !/*di        216*/
      do 130 i=1,nhad                                                           !/*di        217*/
      pzz=pzh(i)                                                                !/*di        218*/
      hepp=heph(i)                                                              !/*di        219*/
      pzh(i)=gamcm*pzz+bgcm*hepp                                                !/*di        220*/
      heph(i)=gamcm*hepp+bgcm*pzz                                               !/*di        221*/
  130 continue                                                                  !/*di        222*/
      go to 140                                                                 !/*di        223*/
  140 continue                                                                  !/*di        224*/
      puzz=0.d0                                                                 !/*di        225*/
      euzz=0.d0                                                                 !/*di        226*/
      puxx=0.d0                                                                 !/*di        227*/
      puyy=0.d0                                                                 !/*di        228*/
      icuu=0                                                                    !/*di        229*/
      ibuu=0                                                                    !/*di        230*/
      do 150 i=1,nhad                                                           !/*di        231*/
      puxx=puxx+pxh(i)                                                          !/*di        232*/
      puyy=puyy+pyh(i)                                                          !/*di        233*/
      puzz=puzz+pzh(i)                                                          !/*di        234*/
      euzz=euzz+heph(i)                                                         !/*di        235*/
      icuu=icuu+ichh(i)                                                         !/*di        236*/
      ibuu=ibuu+ibarh(i)                                                        !/*di        237*/
  150 continue                                                                  !/*di        238*/
      ichtot=ich(kproj)+ich(ktarg)                                              !/*di        239*/
      ibtot=ibar(kproj)+ibar(ktarg)                                             !/*di        240*/
      pchmin=1.d-10*pproj                                                       !/*di        241*/
      if ((abs(puxx).ge.pchmin).or.(abs(puyy).ge.pchmin).or.(abs(puzz           !/*di        242*/
     & -pproj).ge.pchmin).or.(abs(eproj+amtar-euzz).ge.1.d-10*euzz).or.         !/*di        243*/
     & (ichtot.ne.icuu).or.(ibtot.ne.ibuu)) then                                !/*di        244*/
        write (io,*) ' difevt: failure!!!: nhad, kproj, ktarg: ',nhad,          !/*di        245*/
     &   kproj,ktarg                                                            !/*di        246*/
        write (io,*) '                     ',                                   !/*di        247*/
     &  'ichtot, icuu, ibtot, ibuu: ',ichtot,icuu,ibtot,ibuu                    !/*di        248*/
        write (io,*) '                     ',                                   !/*di        249*/
     &  'pproj, puxx, puyy, puzz: ',pproj,puxx,puyy,puzz                        !/*di        250*/
        write (io,*) '                     eproj, euzz: ',eproj,euzz            !/*di        251*/
      endif                                                                     !/*di        252*/
      if (ipri.ne.1) go to 170                                                  !/*di        253*/
      do 160 i=1,nhad                                                           !/*di        254*/
      write (io,180) i,nreh(i),ichh(i),ibarh(i),anh(i),pxh(i),pyh(i),pzh        !/*di        255*/
     & (i),heph(i),amh(i)                                                       !/*di        256*/
  160 continue                                                                  !/*di        257*/
  170 continue                                                                  !/*di        258*/
      return                                                                    !/*di        259*/
!                                                                               !/*di        260*/
  180 format (4i5,a8,5f12.6)                                                    !/*di        261*/
      end                                                                       !/*di        262*/

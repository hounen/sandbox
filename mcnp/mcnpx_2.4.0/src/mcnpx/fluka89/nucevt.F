!     $Id: nucevt.F,v 1.4 2002/03/27 18:14:46 gwm Exp $                       !/*nu          1*/
      subroutine nucevt (nnhad,kproj,anuc,znuc,ppproj)                          !/*nu          2*/
!                                                                     *         !/*nu          3*/
!---------------------------------------------------------------------*         !/*nu          4*/
!                                                                     *         !/*nu          5*/
#include <lcs/double.h>                                                         /*!nu          6*/
#include <fluka89/flprm1.h>                                                     /*!nu          7*/
#include <fluka89/flprm2.h>                                                     /*!nu          8*/
#include <lcs/inout.h>                                                          /*!nu          9*/
!                                                                     *         !/*nu         10*/
#include <fluka89/balanc.h>                                                     /*!nu         11*/
#include <fluka89/hadpar.h>                                                     /*!nu         12*/
#include <fluka89/nucpar.h>                                                     /*!nu         13*/
#include <fluka89/part1.h>                                                      /*!nu         14*/
!                                                                     *         !/*nu         15*/
#include <fluka89/resnuc.h>                                                     /*!nu         16*/
#include <fluka89/print.h>                                                      /*!nu         17*/
      dimension xsea(20), xasea(20), ifs(20), ifas(20), xqt(20), xdqt(20        !/*nu         18*/
     & ), ifqt(3,20)                                                            !/*nu         19*/
      dimension mquark(3,30)                                                    !/*nu         20*/
      data mquark /1,1,2,-1,-1,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,-1        !/*nu         21*/
     & ,-2,-2,0,0,0,0,0,0,0,0,0,1,-2,0,2,-1,0,1,-3,0,3,-1,0,1,2,3,-1,-2,        !/*nu         22*/
     & -3,0,0,0,2,2,3,1,1,3,1,2,3,1,-1,0,2,-3,0,3,-2,0,2,-2,0,0,0,0,0,0,        !/*nu         23*/
     & 0,0,0,0,0,0,0/                                                           !/*nu         24*/
!REP                                                                            !/*nu         25*/
      data xsea /20*dp0/, xasea /20*dp0/                                        !/*nu         26*/
!REP                                                                            !/*nu         27*/
      pproj=ppproj                                                              !/*nu         28*/
      amproj=am(kproj)                                                          !/*nu         29*/
      eproj=sqrt(pproj**2+amproj**2)                                            !/*nu         30*/
      eeproj=eproj                                                              !/*nu         31*/
      if (pproj.lt.200.d0) then                                                 !/*nu         32*/
        xcutff=0.3d0/pproj                                                      !/*nu         33*/
      else                                                                      !/*nu         34*/
        umo=sqrt(2.d0*am(1)*eproj+amproj**2+am(1)**2)                           !/*nu         35*/
        xcutff=0.03d0/umo                                                       !/*nu         36*/
      endif                                                                     !/*nu         37*/
      ekproj=eproj-amproj                                                       !/*nu         38*/
      ibproj=ibar(kproj)                                                        !/*nu         39*/
      icproj=ich(kproj)                                                         !/*nu         40*/
      nnhad=0                                                                   !/*nu         41*/
      nevt=0                                                                    !/*nu         42*/
      sihn=30.d0                                                                !/*nu         43*/
      siha=40.d0*anuc**0.6666666666666667d0                                     !/*nu         44*/
      call nizl (kproj,anuc,ppproj,siha,zla)                                    !/*nu         45*/
      call nizl (kproj,1.d0,ppproj,sihn,zlp)                                    !/*nu         46*/
      anuav=anuc*sihn/siha                                                      !/*nu         47*/
      anusea=anuav-1.d0                                                         !/*nu         48*/
      if (anusea.lt.0.d0) anusea=0.d0                                           !/*nu         49*/
      if (anusea.eq.0.d0) go to 120                                             !/*nu         50*/
      explam=exp(-anusea)                                                       !/*nu         51*/
      nsea=nudist(anuav,ibproj,explam)-1                                        !/*nu         52*/
   10 continue                                                                  !/*nu         53*/
      if (nsea.eq.0) go to 120                                                  !/*nu         54*/
      if (init.eq.1) write (io,180) nnhad,kproj,anuc,pproj,amproj,eproj         !/*nu         55*/
     & ,anuav,anusea,nsea                                                       !/*nu         56*/
      nc3=0                                                                     !/*nu         57*/
   20 continue                                                                  !/*nu         58*/
      nc3=nc3+1                                                                 !/*nu         59*/
      if (nc3.gt.10) then                                                       !/*nu         60*/
        nsea=nsea-1                                                             !/*nu         61*/
        go to 10                                                                !/*nu         62*/
      endif                                                                     !/*nu         63*/
      xo=0.d0                                                                   !/*nu         64*/
      unosea=6.d0                                                               !/*nu         65*/
      if (kproj.gt.2) unosea=5.d0                                               !/*nu         66*/
      do 60 i=1,nsea                                                            !/*nu         67*/
      ai=i                                                                      !/*nu         68*/
      ncou=0                                                                    !/*nu         69*/
   30 continue                                                                  !/*nu         70*/
      ncou=ncou+1                                                               !/*nu         71*/
      if (ncou.ge.11) go to 40                                                  !/*nu         72*/
      xsea(i)=betarn(0.05d0,unosea)                                             !/*nu         73*/
      if (xsea(i).lt.xcutff) go to 30                                           !/*nu         74*/
   40 continue                                                                  !/*nu         75*/
      xo=xo+xsea(i)                                                             !/*nu         76*/
      ifs(i)=1.d0+rang()*2.d0                                                   !/*nu         77*/
      ifas(i)=-ifs(i)                                                           !/*nu         78*/
      ncou=0                                                                    !/*nu         79*/
      ncou=ncou+1                                                               !/*nu         80*/
      if (ncou.ge.11) go to 50                                                  !/*nu         81*/
      xasea(i)=betarn(0.05d0,unosea)                                            !/*nu         82*/
      if (xsea(i).lt.xcutff) go to 30                                           !/*nu         83*/
   50 continue                                                                  !/*nu         84*/
      xo=xo+xasea(i)                                                            !/*nu         85*/
   60 continue                                                                  !/*nu         86*/
      ixxyy=0                                                                   !/*nu         87*/
      if (xo.ge.1.d0-4.d0/pproj) ixxyy=1                                        !/*nu         88*/
      if (ixxyy.eq.1) nsea=nsea-1                                               !/*nu         89*/
      if (nsea.le.0) go to 120                                                  !/*nu         90*/
      if (ixxyy.eq.1) go to 20                                                  !/*nu         91*/
      do 70 i=1,nsea                                                            !/*nu         92*/
      xqt(i)=betarn(0.5d0,3.d0)                                                 !/*nu         93*/
      xdqt(i)=1.d0-xqt(i)                                                       !/*nu         94*/
      ib=1                                                                      !/*nu         95*/
      if (rang().gt.0.5d0) ib=8                                                 !/*nu         96*/
      ifqt(1,i)=mquark(1,ib)                                                    !/*nu         97*/
      ifqt(2,i)=mquark(2,ib)                                                    !/*nu         98*/
      ifqt(3,i)=mquark(3,ib)                                                    !/*nu         99*/
   70 continue                                                                  !/*nu        100*/
      do 110 i=1,nsea                                                           !/*nu        101*/
      em=ekproj*(xsea(i)+xasea(i))                                              !/*nu        102*/
      im=23                                                                     !/*nu        103*/
      if (rang().gt.0.5d0) im=26                                                !/*nu        104*/
      ekm=em-am(im)                                                             !/*nu        105*/
      if (ekm.lt.1.d0) go to 80                                                 !/*nu        106*/
      pm=sqrt(em**2-am(im)**2)                                                  !/*nu        107*/
      if (pm.gt.1.d0) go to 90                                                  !/*nu        108*/
   80 continue                                                                  !/*nu        109*/
      ii=i+1                                                                    !/*nu        110*/
      xsea(ii)=xsea(ii)+xsea(i)                                                 !/*nu        111*/
      xasea(ii)=xasea(ii)+xasea(i)                                              !/*nu        112*/
      go to 110                                                                 !/*nu        113*/
   90 continue                                                                  !/*nu        114*/
      ekproj=ekproj-em                                                          !/*nu        115*/
      if (ekproj.lt.3.99d0) then                                                !/*nu        116*/
        ekproj=ekproj+em                                                        !/*nu        117*/
        nsea=0                                                                  !/*nu        118*/
        go to 110                                                               !/*nu        119*/
      endif                                                                     !/*nu        120*/
      psea=psea+pm                                                              !/*nu        121*/
      zapu=znuc/anuc                                                            !/*nu        122*/
      if (rang().le.zapu) then                                                  !/*nu        123*/
        kt=1                                                                    !/*nu        124*/
        anow=anow-1.d0                                                          !/*nu        125*/
        znow=znow-1.d0                                                          !/*nu        126*/
        ktarp=ktarp+1                                                           !/*nu        127*/
      else                                                                      !/*nu        128*/
        kt=8                                                                    !/*nu        129*/
        anow=anow-1.d0                                                          !/*nu        130*/
        ktarn=ktarn+1                                                           !/*nu        131*/
      endif                                                                     !/*nu        132*/
      call ferevt (nhad,im,kt,pm,anuc,znuc)                                     !/*nu        133*/
      nevt=nevt+1                                                               !/*nu        134*/
      if (nnhad+nhad.gt.mxpnuc) then                                            !/*nu        135*/
        write (io,190) nnhad                                                    !/*nu        136*/
        stop                                                                    !/*nu        137*/
      endif                                                                     !/*nu        138*/
      do 100 jj=1,nhad                                                          !/*nu        139*/
      nnhad=nnhad+1                                                             !/*nu        140*/
      j=nnhad                                                                   !/*nu        141*/
      pxnu(j)=pxh(jj)                                                           !/*nu        142*/
      pynu(j)=pyh(jj)                                                           !/*nu        143*/
      pznu(j)=pzh(jj)                                                           !/*nu        144*/
      hepnu(j)=heph(jj)                                                         !/*nu        145*/
      amnu(j)=amh(jj)                                                           !/*nu        146*/
      ibarnu(j)=ibarh(jj)                                                       !/*nu        147*/
      ichnu(j)=ichh(jj)                                                         !/*nu        148*/
      nrenu(j)=nreh(jj)                                                         !/*nu        149*/
      annu(j)=anh(jj)                                                           !/*nu        150*/
  100 continue                                                                  !/*nu        151*/
  110 continue                                                                  !/*nu        152*/
      eproj=ekproj+amproj                                                       !/*nu        153*/
      pproj=sqrt(eproj**2-amproj**2)                                            !/*nu        154*/
  120 continue                                                                  !/*nu        155*/
      if (pproj.lt.4.d0) then                                                   !/*nu        156*/
        if ((kproj.eq.1.or.kproj.eq.8).and.ekproj.eq.0.d0) then                 !/*nu        157*/
        else                                                                    !/*nu        158*/
          nnhad=nnhad+1                                                         !/*nu        159*/
          j=nnhad                                                               !/*nu        160*/
          pxnu(j)=0.d0                                                          !/*nu        161*/
          pynu(j)=0.d0                                                          !/*nu        162*/
          pznu(j)=pproj                                                         !/*nu        163*/
          hepnu(j)=ekproj+amproj                                                !/*nu        164*/
          amnu(j)=amproj                                                        !/*nu        165*/
          ibarnu(j)=ibproj                                                      !/*nu        166*/
          ichnu(j)=icproj                                                       !/*nu        167*/
          nrenu(j)=kproj                                                        !/*nu        168*/
          annu(j)=anh(kproj)                                                    !/*nu        169*/
          psea=psea+pproj                                                       !/*nu        170*/
        endif                                                                   !/*nu        171*/
        go to 140                                                               !/*nu        172*/
      endif                                                                     !/*nu        173*/
      zapu=znuc/anuc                                                            !/*nu        174*/
      if (rang().le.zapu) then                                                  !/*nu        175*/
        kt=1                                                                    !/*nu        176*/
        anow=anow-1                                                             !/*nu        177*/
        znow=znow-1                                                             !/*nu        178*/
        ktarp=ktarp+1                                                           !/*nu        179*/
      else                                                                      !/*nu        180*/
        kt=8                                                                    !/*nu        181*/
        anow=anow-1                                                             !/*nu        182*/
        ktarn=ktarn+1                                                           !/*nu        183*/
      endif                                                                     !/*nu        184*/
      psea=psea+pproj                                                           !/*nu        185*/
      call ferevt (nhad,kproj,kt,pproj,anuc,znuc)                               !/*nu        186*/
      nevt=nevt+1                                                               !/*nu        187*/
      if (nnhad+nhad.gt.mxpnuc) then                                            !/*nu        188*/
        write (io,190) nnhad                                                    !/*nu        189*/
        stop                                                                    !/*nu        190*/
      endif                                                                     !/*nu        191*/
      do 130 jj=1,nhad                                                          !/*nu        192*/
      nnhad=nnhad+1                                                             !/*nu        193*/
      j=nnhad                                                                   !/*nu        194*/
      pxnu(j)=pxh(jj)                                                           !/*nu        195*/
      pynu(j)=pyh(jj)                                                           !/*nu        196*/
      pznu(j)=pzh(jj)                                                           !/*nu        197*/
      hepnu(j)=heph(jj)                                                         !/*nu        198*/
      amnu(j)=amh(jj)                                                           !/*nu        199*/
      ibarnu(j)=ibarh(jj)                                                       !/*nu        200*/
      ichnu(j)=ichh(jj)                                                         !/*nu        201*/
      nrenu(j)=nreh(jj)                                                         !/*nu        202*/
      annu(j)=anh(jj)                                                           !/*nu        203*/
  130 continue                                                                  !/*nu        204*/
  140 continue                                                                  !/*nu        205*/
      psea=psea-ppproj                                                          !/*nu        206*/
      etot=eeproj+am(1)*dfloat(ktarp)+am(8)*dfloat(ktarn)+efrm                  !/*nu        207*/
      ictot=ich(kproj)+ktarp                                                    !/*nu        208*/
      ibtot=ibar(kproj)+ktarp+ktarn                                             !/*nu        209*/
      emin=1.d-10*etot                                                          !/*nu        210*/
      pmin=1.d-10*ppproj                                                        !/*nu        211*/
      do 150 i=1,nnhad                                                          !/*nu        212*/
      pux=pux+pxnu(i)                                                           !/*nu        213*/
      puy=puy+pynu(i)                                                           !/*nu        214*/
      puz=puz+pznu(i)                                                           !/*nu        215*/
      euz=euz+hepnu(i)                                                          !/*nu        216*/
      icu=icu+ichnu(i)                                                          !/*nu        217*/
      ibu=ibu+ibarnu(i)                                                         !/*nu        218*/
  150 continue                                                                  !/*nu        219*/
      if (ictot.ne.icu.or.ibtot.ne.ibu) then                                    !/*nu        220*/
        write (io,*) ' nucevt-ferevt charges conservation failure:',            !/*nu        221*/
     &  ' icu,ibu,ibtot',icu,ibu,ibtot                                          !/*nu        222*/
!REP                                                                            !/*nu        223*/
!REP 1' icu,ichtot,ibu,ibtot',icu,ichtot,ibu,ibtot                              !/*nu        224*/
!REP                                                                            !/*nu        225*/
      endif                                                                     !/*nu        226*/
      if (abs(pux-pxfrm).gt.pmin) then                                          !/*nu        227*/
        write (io,*) ' nucevt-ferevt px conservation failure:',                 !/*nu        228*/
     & ' pux, pxfrm',pux,pxfrm                                                  !/*nu        229*/
      endif                                                                     !/*nu        230*/
      if (abs(puy-pyfrm).gt.pmin) then                                          !/*nu        231*/
        write (io,*) ' nucevt-ferevt py conservation failure:',                 !/*nu        232*/
     & ' puy, pyfrm',puy,pyfrm                                                  !/*nu        233*/
      endif                                                                     !/*nu        234*/
      if (abs(puz-ppproj-pzfrm-psea).gt.pmin) then                              !/*nu        235*/
        write (io,*) ' nucevt-ferevt pz conservation failure:',                 !/*nu        236*/
     & ' puz, ppproj+pzfrm+psea',puz,ppproj+pzfrm+psea                          !/*nu        237*/
      endif                                                                     !/*nu        238*/
      if (abs(etot-euz).gt.emin) then                                           !/*nu        239*/
        write (io,*) ' nucevt-ferevt e conservation failure:',                  !/*nu        240*/
     & ' euz, etot',euz,etot                                                    !/*nu        241*/
      endif                                                                     !/*nu        242*/
      if (init.ne.1) go to 170                                                  !/*nu        243*/
!REP                                                                            !/*nu        244*/
!REP  write (io,200) nnhad,kproj,anuc,ppproj,eproj,pux,puy,puz,euz0             !/*nu        245*/
!REP                                                                            !/*nu        246*/
      write (io,200) nnhad,kproj,anuc,ppproj,eproj,pux,puy,puz,icu,ibu          !/*nu        247*/
     & ,nnhad                                                                   !/*nu        248*/
      do 160 i=1,nnhad                                                          !/*nu        249*/
      write (io,210) i,nrenu(i),ichnu(i),ibarnu(i),annu(i),pxnu(i),pynu         !/*nu        250*/
     & (i),pznu(i),hepnu(i),amnu(i)                                             !/*nu        251*/
  160 continue                                                                  !/*nu        252*/
      init=0                                                                    !/*nu        253*/
  170 continue                                                                  !/*nu        254*/
      return                                                                    !/*nu        255*/
!                                                                               !/*nu        256*/
!REP                                                                            !/*nu        257*/
! 200 format (1x,2i5,7f12.6,3i5)                                                !/*nu        258*/
!REP                                                                            !/*nu        259*/
!                                                                               !/*nu        260*/
  180 format (1x,2i5,6f12.5,i10)                                                !/*nu        261*/
  190 format ('  nhad in nucevt too big change dimension','  nnhad=',i10        !/*nu        262*/
     & )                                                                        !/*nu        263*/
  200 format (1x,2i5,6f12.6,3i5)                                                !/*nu        264*/
  210 format (1x,4i5,a8,6f12.6)                                                 !/*nu        265*/
      end                                                                       !/*nu        266*/

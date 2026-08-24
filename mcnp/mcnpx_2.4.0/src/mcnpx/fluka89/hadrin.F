!     $Id: hadrin.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*ha          1*/
      subroutine hadrin (n,plab,elab,cx,cy,cz,itta)                             !/*ha          2*/
!                                                                     *         !/*ha          3*/
!---------------------------------------------------------------------*         !/*ha          4*/
!                                                                     *         !/*ha          5*/
#include <lcs/double.h>                                                         /*!ha          6*/
#include <fluka89/flprm1.h>                                                     /*!ha          7*/
#include <fluka89/flprm2.h>                                                     /*!ha          8*/
#include <lcs/inout.h>                                                          /*!ha          9*/
#include <fluka89/metlsp.h>                                                     /*!ha         10*/
#include <fluka89/fnlsp1.h>                                                     /*!ha         11*/
#include <fluka89/gamrd1.h>                                                     /*!ha         12*/
#include <fluka89/redver.h>                                                     /*!ha         13*/
#include <fluka89/reac1.h>                                                      /*!ha         14*/
#include <fluka89/ablts1.h>                                                     /*!ha         15*/
#include <fluka89/split.h>                                                      /*!ha         16*/
#include <fluka89/codsif.h>                                                     /*!ha         17*/
#include <fluka89/run1.h>                                                       /*!ha         18*/
      data nnn /0/                                                              !/*ha         19*/
      data umoda /0.d0/                                                         !/*ha         20*/
      dimension itprf(110)                                                      !/*ha         21*/
      data itprf /-1,-1,5*1,-1,-1,1,1,1,-1,-1,-1,-1,6*1,-1,-1,-1,85*1/          !/*ha         22*/
      lowp=0                                                                    !/*ha         23*/
      if (itprf(n).lt.0) go to 10                                               !/*ha         24*/
      write (io,280) n                                                          !/*ha         25*/
      ir=0                                                                      !/*ha         26*/
      return                                                                    !/*ha         27*/
   10 continue                                                                  !/*ha         28*/
      iatmpt=0                                                                  !/*ha         29*/
      if (abs(plab-5.d0).lt.4.99999d0) go to 20                                 !/*ha         30*/
      write (io,290) plab                                                       !/*ha         31*/
      ir=0                                                                      !/*ha         32*/
      return                                                                    !/*ha         33*/
   20 continue                                                                  !/*ha         34*/
      umodat=n*1.11111d0+itta*2.19291d0                                         !/*ha         35*/
      if (umodat.ne.umoda) call calumo (n,itta)                                 !/*ha         36*/
      umoda=umodat                                                              !/*ha         37*/
   30 iatmpt=0                                                                  !/*ha         38*/
      lowp=lowp+1                                                               !/*ha         39*/
   40 continue                                                                  !/*ha         40*/
      imach=0                                                                   !/*ha         41*/
      redu=2.d0                                                                 !/*ha         42*/
      iw1=0                                                                     !/*ha         43*/
      if (lowp.gt.20) go to 270                                                 !/*ha         44*/
      nnn=n                                                                     !/*ha         45*/
      if (nnn.eq.n) go to 50                                                    !/*ha         46*/
      runtes=0.d0                                                               !/*ha         47*/
      eftes=0.d0                                                                !/*ha         48*/
   50 continue                                                                  !/*ha         49*/
      is=1                                                                      !/*ha         50*/
      ir=0                                                                      !/*ha         51*/
      ist=1                                                                     !/*ha         52*/
      nstab=23                                                                  !/*ha         53*/
      ire=nure(n,1)                                                             !/*ha         54*/
      if (itta.gt.1) ire=nure(n,2)                                              !/*ha         55*/
      call sigint (ire,plab,n,ie,amt,amn,ecm,si,itta)                           !/*ha         56*/
      ianth=-1                                                                  !/*ha         57*/
      if (am(1).ne.0.9382796d0) ianth=1                                         !/*ha         58*/
      if (ianth.ge.0) si=1.d0                                                   !/*ha         59*/
      ecmmh=ecm                                                                 !/*ha         60*/
      if (si.lt.1.d-6) go to 270                                                !/*ha         61*/
      if (n.le.nstab) go to 60                                                  !/*ha         62*/
      runtes=runtes+1.d0                                                        !/*ha         63*/
      if (runtes.lt.20.d0) write (io,300) n                                     !/*ha         64*/
      if (ibar(n).eq.1) n=8                                                     !/*ha         65*/
      if (ibar(n).eq.-1) n=9                                                    !/*ha         66*/
   60 continue                                                                  !/*ha         67*/
      imach=imach+1                                                             !/*ha         68*/
      if (imach.gt.10) go to 270                                                !/*ha         69*/
      ecm=ecmmh                                                                 !/*ha         70*/
      amn2=amn**2                                                               !/*ha         71*/
      amt2=amt**2                                                               !/*ha         72*/
      ecmn=(ecm**2+amn2-amt2)/(2.d0*ecm)                                        !/*ha         73*/
      if (ecmn.le.amn) ecmn=amn                                                 !/*ha         74*/
      pcmn=sqrt(ecmn**2-amn2)                                                   !/*ha         75*/
      gam=(elab+amt)/ecm                                                        !/*ha         76*/
      bgam=plab/ecm                                                             !/*ha         77*/
      if (ianth.ge.0) ecm=2.1d0                                                 !/*ha         78*/
      ist=0                                                                     !/*ha         79*/
      vv=rang()                                                                 !/*ha         80*/
      iiei=ieii(ire)                                                            !/*ha         81*/
      idwk=ieii(ire+1)-iiei                                                     !/*ha         82*/
      iiwk=irii(ire)                                                            !/*ha         83*/
      iiki=ikii(ire)                                                            !/*ha         84*/
      hecm=ecm                                                                  !/*ha         85*/
      humo=2.d0*umo(iiei+idwk)-umo(iiei+idwk-1)                                 !/*ha         86*/
      if (humo.lt.ecm) ecm=humo                                                 !/*ha         87*/
      ecmo=umo(ie)                                                              !/*ha         88*/
      ecm1=umo(ie-1)                                                            !/*ha         89*/
      decm=ecmo-ecm1                                                            !/*ha         90*/
      dec=ecmo-ecm                                                              !/*ha         91*/
      ik=0                                                                      !/*ha         92*/
      vfw=vv                                                                    !/*ha         93*/
      wkk=0.d0                                                                  !/*ha         94*/
      wicor=0.d0                                                                !/*ha         95*/
   70 ik=ik+1                                                                   !/*ha         96*/
      vfwo=wkk                                                                  !/*ha         97*/
      iwk=iiwk+(ik-1)*idwk+ie-iiei                                              !/*ha         98*/
      wok=wk(iwk)                                                               !/*ha         99*/
      wdk=wok-wk(iwk-1)                                                         !/*ha        100*/
      if (plab.lt.plabf(iiei+2)) wdk=0.d0                                       !/*ha        101*/
      wico=wok*1.23459876d0+wdk*1.735218469d0                                   !/*ha        102*/
      if (wico.eq.wicor) go to 70                                               !/*ha        103*/
      if (umo(iiei+idwk).lt.hecm) wdk=0.d0                                      !/*ha        104*/
      wicor=wico                                                                !/*ha        105*/
      eklim=-thresh(iiki+ik)                                                    !/*ha        106*/
      ielim=iefun(eklim,ire)                                                    !/*ha        107*/
      delim=umo(ielim)+eklim+1.d-20                                             !/*ha        108*/
      dete=(ecm-(ecmo-eklim)*.5d0)*2.d0                                         !/*ha        109*/
      if (delim*delim-dete*dete) 90,90,80                                       !/*ha        110*/
   80 decc=delim                                                                !/*ha        111*/
      go to 100                                                                 !/*ha        112*/
   90 decc=decm                                                                 !/*ha        113*/
  100 continue                                                                  !/*ha        114*/
      wkk=wok-wdk*dec/(decc+1.d-20)                                             !/*ha        115*/
      if (vv.gt.wkk) go to 70                                                   !/*ha        116*/
      inrk=ikii(ire)+ik                                                         !/*ha        117*/
      ecm=hecm                                                                  !/*ha        118*/
      i1001=0                                                                   !/*ha        119*/
  110 continue                                                                  !/*ha        120*/
      it1=nrk(1,inrk)                                                           !/*ha        121*/
      am1=amga(it1)                                                             !/*ha        122*/
      it2=nrk(2,inrk)                                                           !/*ha        123*/
      am2=amga(it2)                                                             !/*ha        124*/
      ams=am1+am2                                                               !/*ha        125*/
      i1001=i1001+1                                                             !/*ha        126*/
      if (i1001.gt.50) go to 60                                                 !/*ha        127*/
      if (it2*ams.gt.it2*ecm) go to 110                                         !/*ha        128*/
      it11=it1                                                                  !/*ha        129*/
      it22=it2                                                                  !/*ha        130*/
      if (ianth.ge.0) ecm=elab+amt+0.00001d0                                    !/*ha        131*/
      am11=am1                                                                  !/*ha        132*/
      am22=am2                                                                  !/*ha        133*/
      if (it2.gt.0) go to 120                                                   !/*ha        134*/
      kz1=k1(it1)                                                               !/*ha        135*/
      ist=ist+1                                                                 !/*ha        136*/
      ieco=0                                                                    !/*ha        137*/
      eco=ecm                                                                   !/*ha        138*/
      gam=(elab+amt)/eco                                                        !/*ha        139*/
      bgam=plab/eco                                                             !/*ha        140*/
      cxs(1)=cx                                                                 !/*ha        141*/
      cys(1)=cy                                                                 !/*ha        142*/
      czs(1)=cz                                                                 !/*ha        143*/
      go to 170                                                                 !/*ha        144*/
  120 continue                                                                  !/*ha        145*/
      if (rang().lt.0.5d0) go to 130                                            !/*ha        146*/
      it1=it22                                                                  !/*ha        147*/
      it2=it11                                                                  !/*ha        148*/
      am1=am22                                                                  !/*ha        149*/
      am2=am11                                                                  !/*ha        150*/
  130 continue                                                                  !/*ha        151*/
      ibn=ibar(n)                                                               !/*ha        152*/
      ib1=ibar(it1)                                                             !/*ha        153*/
      it11=it1                                                                  !/*ha        154*/
      it22=it2                                                                  !/*ha        155*/
      am11=am1                                                                  !/*ha        156*/
      am22=am2                                                                  !/*ha        157*/
      if (ib1.eq.ibn) go to 140                                                 !/*ha        158*/
      it1=it22                                                                  !/*ha        159*/
      it2=it11                                                                  !/*ha        160*/
      am1=am22                                                                  !/*ha        161*/
      am2=am11                                                                  !/*ha        162*/
  140 continue                                                                  !/*ha        163*/
      call twopar (ecm1,ecm2,pcm1,pcm2,cod1,cod2,cof1,cof2,sif1,sif2,it1        !/*ha        164*/
     & ,it2,ecm,ecmn,pcmn,n,am1,am2)                                            !/*ha        165*/
      ist=ist+1                                                                 !/*ha        166*/
      its(ist)=it1                                                              !/*ha        167*/
      amm(ist)=am1                                                              !/*ha        168*/
      call trafo (gam,bgam,cx,cy,cz,cod1,cof1,sif1,pcm1,ecm1,pls(ist)           !/*ha        169*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*ha        170*/
      ist=ist+1                                                                 !/*ha        171*/
      its(ist)=it2                                                              !/*ha        172*/
      amm(ist)=am2                                                              !/*ha        173*/
      call trafo (gam,bgam,cx,cy,cz,cod2,cof2,sif2,pcm2,ecm2,pls(ist)           !/*ha        174*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*ha        175*/
  150 continue                                                                  !/*ha        176*/
      if (its(ist).gt.nstab) go to 160                                          !/*ha        177*/
      ir=ir+1                                                                   !/*ha        178*/
      if (redu.lt.0.d0) go to 30                                                !/*ha        179*/
      itr(ir)=its(ist)                                                          !/*ha        180*/
      plr(ir)=pls(ist)                                                          !/*ha        181*/
      cxr(ir)=cxs(ist)                                                          !/*ha        182*/
      cyr(ir)=cys(ist)                                                          !/*ha        183*/
      czr(ir)=czs(ist)                                                          !/*ha        184*/
      elr(ir)=els(ist)                                                          !/*ha        185*/
      ist=ist-1                                                                 !/*ha        186*/
      if (ist.ge.1) go to 150                                                   !/*ha        187*/
      go to 260                                                                 !/*ha        188*/
  160 continue                                                                  !/*ha        189*/
      it=its(ist)                                                               !/*ha        190*/
      eco=amm(ist)                                                              !/*ha        191*/
      gam=els(ist)/eco                                                          !/*ha        192*/
      bgam=pls(ist)/eco                                                         !/*ha        193*/
      ieco=0                                                                    !/*ha        194*/
      kz1=k1(it)                                                                !/*ha        195*/
  170 continue                                                                  !/*ha        196*/
      ieco=ieco+1                                                               !/*ha        197*/
      vv=rang()                                                                 !/*ha        198*/
      iik=kz1-1                                                                 !/*ha        199*/
  180 iik=iik+1                                                                 !/*ha        200*/
      if (vv.gt.wt(iik)) go to 180                                              !/*ha        201*/
      it1=nzk(iik,1)                                                            !/*ha        202*/
      i310=0                                                                    !/*ha        203*/
  190 continue                                                                  !/*ha        204*/
      i310=i310+1                                                               !/*ha        205*/
      am1=amga(it1)                                                             !/*ha        206*/
      it2=nzk(iik,2)                                                            !/*ha        207*/
      am2=amga(it2)                                                             !/*ha        208*/
      if (it2-1.lt.0) go to 240                                                 !/*ha        209*/
      it3=nzk(iik,3)                                                            !/*ha        210*/
      am3=amga(it3)                                                             !/*ha        211*/
      ams=am1+am2+am3                                                           !/*ha        212*/
      if (ieco.le.10) go to 200                                                 !/*ha        213*/
      iatmpt=iatmpt+1                                                           !/*ha        214*/
      if (iatmpt.gt.3) go to 270                                                !/*ha        215*/
      go to 40                                                                  !/*ha        216*/
  200 continue                                                                  !/*ha        217*/
      if (i310.gt.50) go to 170                                                 !/*ha        218*/
      if (ams.gt.eco) go to 190                                                 !/*ha        219*/
      if (redu.lt.0.d0) go to 30                                                !/*ha        220*/
      itwthc=0                                                                  !/*ha        221*/
      redu=2.d0                                                                 !/*ha        222*/
      if (it3.eq.0) go to 220                                                   !/*ha        223*/
  210 continue                                                                  !/*ha        224*/
      itwth=1                                                                   !/*ha        225*/
      call threpd (eco,ecm1,ecm2,ecm3,pcm1,pcm2,pcm3,cod1,cof1,sif1,cod2        !/*ha        226*/
     & ,cof2,sif2,cod3,cof3,sif3,am1,am2,am3)                                   !/*ha        227*/
      go to 230                                                                 !/*ha        228*/
  220 call twopad (eco,ecm1,ecm2,pcm1,pcm2,cod1,cof1,sif1,cod2,cof2,sif2        !/*ha        229*/
     & ,am1,am2)                                                                !/*ha        230*/
      itwth=-1                                                                  !/*ha        231*/
      it3=0                                                                     !/*ha        232*/
  230 continue                                                                  !/*ha        233*/
      itwthc=itwthc+1                                                           !/*ha        234*/
      if (redu.gt.0.d0) go to 240                                               !/*ha        235*/
      redu=2.d0                                                                 !/*ha        236*/
      if (itwthc.gt.100) go to 30                                               !/*ha        237*/
      if (itwth) 220,220,210                                                    !/*ha        238*/
  240 continue                                                                  !/*ha        239*/
      its(ist)=it1                                                              !/*ha        240*/
      if (it2-1.lt.0) go to 250                                                 !/*ha        241*/
      its(ist+1)=it2                                                            !/*ha        242*/
      its(ist+2)=it3                                                            !/*ha        243*/
      rx=cxs(ist)                                                               !/*ha        244*/
      ry=cys(ist)                                                               !/*ha        245*/
      rz=czs(ist)                                                               !/*ha        246*/
      amm(ist)=am1                                                              !/*ha        247*/
      call trafo (gam,bgam,rx,ry,rz,cod1,cof1,sif1,pcm1,ecm1,pls(ist)           !/*ha        248*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*ha        249*/
      ist=ist+1                                                                 !/*ha        250*/
      amm(ist)=am2                                                              !/*ha        251*/
      call trafo (gam,bgam,rx,ry,rz,cod2,cof2,sif2,pcm2,ecm2,pls(ist)           !/*ha        252*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*ha        253*/
      if (it3.le.0) go to 250                                                   !/*ha        254*/
      ist=ist+1                                                                 !/*ha        255*/
      amm(ist)=am3                                                              !/*ha        256*/
      call trafo (gam,bgam,rx,ry,rz,cod3,cof3,sif3,pcm3,ecm3,pls(ist)           !/*ha        257*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*ha        258*/
  250 continue                                                                  !/*ha        259*/
      go to 150                                                                 !/*ha        260*/
  260 continue                                                                  !/*ha        261*/
      return                                                                    !/*ha        262*/
  270 continue                                                                  !/*ha        263*/
      ir=1                                                                      !/*ha        264*/
      itr(1)=n                                                                  !/*ha        265*/
      cxr(1)=cx                                                                 !/*ha        266*/
      cyr(1)=cy                                                                 !/*ha        267*/
      czr(1)=cz                                                                 !/*ha        268*/
      elr(1)=elab                                                               !/*ha        269*/
      plr(1)=plab                                                               !/*ha        270*/
      return                                                                    !/*ha        271*/
!                                                                               !/*ha        272*/
  280 format (3(5h ****/),45h false use of the particle type index, valu        !/*ha        273*/
     &e ,i4,3(5h ****/))                                                        !/*ha        274*/
  290 format (3(5h ****/),64h projectile hadron momentum outside of the         !/*ha        275*/
     &allowed region, plab=,1e15.5/,3(5h ****/))                                !/*ha        276*/
  300 format (3h n=,i10,30h the proektile is a resonance )                      !/*ha        277*/
      end                                                                       !/*ha        278*/

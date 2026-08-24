!     $Id: nucrin.F,v 1.4 2002/03/27 18:14:46 gwm Exp $                       !/*nu          1*/
      subroutine nucrin (itttt,elab,cx,cy,cz,anuc,znuc,rhoo)                    !/*nu          2*/
!                                                                     *         !/*nu          3*/
!---------------------------------------------------------------------*         !/*nu          4*/
!                                                                     *         !/*nu          5*/
#include <lcs/double.h>                                                         /*!nu          6*/
#include <fluka89/flprm1.h>                                                     /*!nu          7*/
#include <fluka89/flprm2.h>                                                     /*!nu          8*/
#include <lcs/inout.h>                                                          /*!nu          9*/
#include <fluka89/balanc.h>                                                     /*!nu         10*/
#include <fluka89/finuc2.h>                                                     /*!nu         11*/
#include <fluka89/fnlsp2.h>                                                     /*!nu         12*/
!*ca nucdat                                                                     !/*nu         13*/
#include <fluka89/resnuc.h>                                                     /*!nu         14*/
#include <fluka89/hfrag.h>                                                      /*!nu         15*/
#include <fluka89/fermt.h>                                                      /*!nu         16*/
!*ca regim                                                                      !/*nu         17*/
#include <fluka89/perco.h>                                                      /*!nu         18*/
#include <fluka89/encote.h>                                                     /*!nu         19*/
!*ca probax                                                                     !/*nu         20*/
#include <fluka89/ablts1.h>                                                     /*!nu         21*/
!*ca nucthr                                                                     !/*nu         22*/
!REP  common /event/ ievent                                                     !/*nu         23*/
!REP tpnk needs to be SAVEd.                                                    !/*nu         24*/
!REP  common /tpnkc/ tpnk                                                       !/*nu         25*/
! ..  save tpnk                                                                 !/*nu         26*/
!REP  common /cddt97/ cddt                                                      !/*nu         27*/
! ..  save cddt                                                                 !/*nu         28*/
      data cddt /.3d0/                                                          !/*nu         29*/
      data cdddd /0.d0/                                                         !/*nu         30*/
      data ceet /0.5d0/                                                         !/*nu         31*/
      dimension thresr(30)                                                      !/*nu         32*/
      data thresr /1.9d0,0.d0,5*9.d0,1.9d0,0.d0,3*9.d0,1.08d0,1.08d0,1.4        !/*nu         33*/
     & 4d0,1.08d0,6*9.d0,1.08d0,1.44d0,1.08d0,5*9.d0/                           !/*nu         34*/
      dimension ixpi(30)                                                        !/*nu         35*/
      data ixpi /1,-1,5*2,1,-1,3*2,1,0,1,0,4,4,2,3*4,1,1,1,5*2/                 !/*nu         36*/
      dimension ins(30)                                                         !/*nu         37*/
!REP  data ieveno /0/                                                           !/*nu         38*/
      dimension amhh(15), iamc(15)                                              !/*nu         39*/
      data iamc /1,2,8,9,12,15,16,17,18,19,20,21,22,24,25/                      !/*nu         40*/
      data ins /13,13,5*32,14,14,3*32,10,10,12,11,7*32,15,15,5*32/              !/*nu         41*/
      dimension itprf(110)                                                      !/*nu         42*/
      data itprf /-1,-1,5*1,-1,-1,1,1,1,-1,-1,-1,-1,6*1,-1,-1,-1,85*1/          !/*nu         43*/
      data innuc /0/                                                            !/*nu         44*/
      irntot=0                                                                  !/*nu         45*/
      innuc=1                                                                   !/*nu         46*/
      itnucr=itttt                                                              !/*nu         47*/
      if (itprf(itttt).lt.0) go to 10                                           !/*nu         48*/
      write (io,630) itttt                                                      !/*nu         49*/
      np=0                                                                      !/*nu         50*/
      return                                                                    !/*nu         51*/
   10 continue                                                                  !/*nu         52*/
      sico=1.d0                                                                 !/*nu         53*/
      elph=0.d0                                                                 !/*nu         54*/
      plabco=1.d0                                                               !/*nu         55*/
      ibat=0                                                                    !/*nu         56*/
!REP  ieveno=ieveno+1                                                           !/*nu         57*/
!REP  ievent=ieveno                                                             !/*nu         58*/
      it=itttt                                                                  !/*nu         59*/
      nxpi=0                                                                    !/*nu         60*/
      inxpi=ixpi(it)                                                            !/*nu         61*/
      if (inxpi.eq.4) then                                                      !/*nu         62*/
        call hypero (it,itnucr,sico,plabco)                                     !/*nu         63*/
      endif                                                                     !/*nu         64*/
      tnkte=0.001d0                                                             !/*nu         65*/
      ethr=0.001d0                                                              !/*nu         66*/
      n=itttt                                                                   !/*nu         67*/
      if (itttt.eq.18) inxpi=-1                                                 !/*nu         68*/
      do 20 iam=1,15                                                            !/*nu         69*/
   20 amhh(iam)=am(iamc(iam))                                                   !/*nu         70*/
      jj=0                                                                      !/*nu         71*/
      cddt=0.5d0                                                                !/*nu         72*/
      ihaca=0                                                                   !/*nu         73*/
      in1=ins(n)                                                                !/*nu         74*/
      etest=elab                                                                !/*nu         75*/
      tlab=elab-am(it)                                                          !/*nu         76*/
      if (rang().lt.znuc/anuc) then                                             !/*nu         77*/
        itta=1                                                                  !/*nu         78*/
      else                                                                      !/*nu         79*/
        itta=8                                                                  !/*nu         80*/
      endif                                                                     !/*nu         81*/
      plab=sqrt(tlab*(elab+am(it)))                                             !/*nu         82*/
      if (abs(plab-5.d0).lt.4.99999d0) go to 30                                 !/*nu         83*/
      write (io,640) plab                                                       !/*nu         84*/
      np=0                                                                      !/*nu         85*/
      return                                                                    !/*nu         86*/
   30 continue                                                                  !/*nu         87*/
      if (jj.eq.1) go to 620                                                    !/*nu         88*/
      if (inxpi.le.0) go to 40                                                  !/*nu         89*/
      if (tlab.gt.ethr) go to 50                                                !/*nu         90*/
      tv=tlab                                                                   !/*nu         91*/
      irn=0                                                                     !/*nu         92*/
      return                                                                    !/*nu         93*/
   40 continue                                                                  !/*nu         94*/
      ethrr=ethr                                                                !/*nu         95*/
      if (inxpi.lt.0) ethrr=0.08d0                                              !/*nu         96*/
      aio=10.d0                                                                 !/*nu         97*/
      nxpi=1                                                                    !/*nu         98*/
      xpi=1.d0-aio*(tlab-ethrr)                                                 !/*nu         99*/
      vz=rang()                                                                 !/*nu        100*/
      if (vz.gt.xpi) nxpi=0                                                     !/*nu        101*/
   50 continue                                                                  !/*nu        102*/
      if (nxpi.le.0.or.ibar(it).ne.0) go to 60                                  !/*nu        103*/
      znow=znow-1                                                               !/*nu        104*/
      ktarp=ktarp+1                                                             !/*nu        105*/
      ktarn=ktarn-1                                                             !/*nu        106*/
      elpp=elab+am(1)-am(8)                                                     !/*nu        107*/
      tpk=0.d0                                                                  !/*nu        108*/
      tnk=0.d0                                                                  !/*nu        109*/
      tv=0.d0                                                                   !/*nu        110*/
      etest=elpp                                                                !/*nu        111*/
      tpnuc=znuc                                                                !/*nu        112*/
      tanuc=anuc                                                                !/*nu        113*/
      toeff=elpp                                                                !/*nu        114*/
      atar=anuc                                                                 !/*nu        115*/
      sqatar=sqrt(atar)                                                         !/*nu        116*/
      go to 140                                                                 !/*nu        117*/
   60 continue                                                                  !/*nu        118*/
      cdddd=cddt                                                                !/*nu        119*/
      inuc=anuc-znuc                                                            !/*nu        120*/
      if (itta.le.1) inuc=znuc                                                  !/*nu        121*/
      akmas=am(1)*znuc+am(8)*(anuc-znuc)                                        !/*nu        122*/
      umojan=sqrt(am(it)**2+akmas**2+2.d0*elab*akmas)                           !/*nu        123*/
      elabke=umojan-akmas                                                       !/*nu        124*/
      a=elab                                                                    !/*nu        125*/
      iforbi=0                                                                  !/*nu        126*/
      ifert=0                                                                   !/*nu        127*/
   70 continue                                                                  !/*nu        128*/
      irn=0                                                                     !/*nu        129*/
      tanuc=anuc                                                                !/*nu        130*/
      tpnuc=znuc                                                                !/*nu        131*/
      tnnuc=anuc-znuc                                                           !/*nu        132*/
      ddt=cddt*tlab                                                             !/*nu        133*/
      ammit=am(it)                                                              !/*nu        134*/
      ammta=am(itta)                                                            !/*nu        135*/
      ait2=ammit**2                                                             !/*nu        136*/
      ata2=ammta**2                                                             !/*nu        137*/
      i653=0                                                                    !/*nu        138*/
      ecmo=ait2+ata2+2.d0*ammta*elab                                            !/*nu        139*/
      emd=0.d0                                                                  !/*nu        140*/
   80 continue                                                                  !/*nu        141*/
      i653=i653+1                                                               !/*nu        142*/
      toeff=tlab                                                                !/*nu        143*/
      if (inxpi.le.0) then                                                      !/*nu        144*/
        ibl=ibar(itta)*ibar(it)                                                 !/*nu        145*/
        ibl=iabs(ibl)                                                           !/*nu        146*/
        icl=ich(it)                                                             !/*nu        147*/
        emd=am(it)*(((ibar(itta)-ibar(it))*ibl+(1-ibl)*iabs(icl)))              !/*nu        148*/
        thres=thresr(it)                                                        !/*nu        149*/
        ecms=sqrt(ecmo)                                                         !/*nu        150*/
        if (ecms.lt.thres) emd=0.d0                                             !/*nu        151*/
        if (nxpi.lt.1) emd=0.d0                                                 !/*nu        152*/
        if (ibar(it).ge.0) emd=0.d0                                             !/*nu        153*/
        eci=((sqrt(ecmo)+emd)**2-ait2-ata2)/(2.d0*ammta)                        !/*nu        154*/
        toeff=eci-ammit                                                         !/*nu        155*/
        if (inxpi.eq.0) then                                                    !/*nu        156*/
        endif                                                                   !/*nu        157*/
        if (toeff.lt.tlab) then                                                 !/*nu        158*/
          toeff=tlab                                                            !/*nu        159*/
        else                                                                    !/*nu        160*/
        endif                                                                   !/*nu        161*/
      endif                                                                     !/*nu        162*/
      t1=anuc                                                                   !/*nu        163*/
      t2=sqrt(t1)                                                               !/*nu        164*/
      atar=t1                                                                   !/*nu        165*/
      tpk0=bekeka(2,toeff,t1,t2)                                                !/*nu        166*/
      tnk0=bekeka(3,toeff,t1,t2)                                                !/*nu        167*/
      enadj=1.d0                                                                !/*nu        168*/
      tnk0=enadj*tnk0                                                           !/*nu        169*/
      tpk0=enadj*tpk0                                                           !/*nu        170*/
      eexo=bekeka(1,toeff,t1,t2)                                                !/*nu        171*/
      eex=beexi(1,tlab,t1)                                                      !/*nu        172*/
   90 continue                                                                  !/*nu        173*/
      dex=ceet*eexo                                                             !/*nu        174*/
      if (eexo.gt.eex) dex=ceet*eexo                                            !/*nu        175*/
      call gauss (tvv,eex,dex)                                                  !/*nu        176*/
      tv=tvv-tpk0-tnk0                                                          !/*nu        177*/
      tvh=tvv*0.35d0-tpk0-tnk0                                                  !/*nu        178*/
  100 if (tvh.gt.0.d0) go to 110                                                !/*nu        179*/
      call gauss (tv,eexo,dex)                                                  !/*nu        180*/
      if (tv) 100,100,120                                                       !/*nu        181*/
  110 continue                                                                  !/*nu        182*/
      if (tvv.ge.tlab) go to 90                                                 !/*nu        183*/
  120 continue                                                                  !/*nu        184*/
      t1=tpk0+tnk0                                                              !/*nu        185*/
      enadj=1.d0                                                                !/*nu        186*/
      t1=t1*enadj                                                               !/*nu        187*/
      t2=toeff-tv                                                               !/*nu        188*/
      itpn=0                                                                    !/*nu        189*/
      if (t2.lt.t1) go to 90                                                    !/*nu        190*/
  130 continue                                                                  !/*nu        191*/
      itpn=itpn+1                                                               !/*nu        192*/
      ddt=0.5d0*t1                                                              !/*nu        193*/
      call gauss (tpnk,t1,ddt)                                                  !/*nu        194*/
      if (itpn.gt.1) tpnk=t1+abs(tpnk-t1)*(stpn/atpn)                           !/*nu        195*/
      stpn=tpnk-t1+1.d-18                                                       !/*nu        196*/
      atpn=abs(stpn)                                                            !/*nu        197*/
      if (tpnk.gt.t2) go to 90                                                  !/*nu        198*/
      if (tpnk.lt.0.d0) go to 130                                               !/*nu        199*/
      tpp=tlab-t1                                                               !/*nu        200*/
      tpk=tpnk*tpk0/t1                                                          !/*nu        201*/
      tnk=tpnk*tnk0/t1                                                          !/*nu        202*/
      t3=am(it)                                                                 !/*nu        203*/
      elpp=elab-tpk-tnk-tv                                                      !/*nu        204*/
      if (inxpi.ne.0.or.nxpi.eq.0) go to 150                                    !/*nu        205*/
      if (ibar(itttt).lt.0) go to 150                                           !/*nu        206*/
  140 continue                                                                  !/*nu        207*/
      elpo=elpp                                                                 !/*nu        208*/
      if (ibar(it).gt.0) elpo=elpp-am(itttt)                                    !/*nu        209*/
      tpk=tpk+.3d0*elpo                                                         !/*nu        210*/
      tnk=tnk+.3d0*elpo                                                         !/*nu        211*/
      tv=tv+.4d0*elpo                                                           !/*nu        212*/
      elpp=0.000001d0                                                           !/*nu        213*/
      go to 430                                                                 !/*nu        214*/
  150 continue                                                                  !/*nu        215*/
      if (i653.lt.10) go to 160                                                 !/*nu        216*/
      if (elpp.gt.0.d0) go to 160                                               !/*nu        217*/
      a65=1.d0+elpp/(tpk+tnk+tv+1.d-18)                                         !/*nu        218*/
      tpk=tpk*a65                                                               !/*nu        219*/
      tnk=tnk*a65                                                               !/*nu        220*/
      tv=tv*a65                                                                 !/*nu        221*/
      elpp=0.d0                                                                 !/*nu        222*/
  160 continue                                                                  !/*nu        223*/
      if (elpp.lt.0.d0) go to 80                                                !/*nu        224*/
      if (elpp.gt.t3) go to 220                                                 !/*nu        225*/
      if (emd.le.0.d0) go to 210                                                !/*nu        226*/
      if (ibar(it).ge.0.and.elpp.lt.t3) go to 80                                !/*nu        227*/
      elph=elpp+am(itta)-1.d-20                                                 !/*nu        228*/
      t3=elph*.5d0                                                              !/*nu        229*/
!     elpp=t3                                                                   !/*nu        230*/
      elpp=elph                                                                 !/*nu        231*/
      if (t3.lt.0.14d0.and.inxpi.gt.0.and.nxpi.gt.0) go to 140                  !/*nu        232*/
      if (ibar(itttt).ge.0) go to 190                                           !/*nu        233*/
      etdd=0.d0                                                                 !/*nu        234*/
      etkor=tv+tpk+tnk+ammit                                                    !/*nu        235*/
      if (nxpi) 170,170,180                                                     !/*nu        236*/
  170 continue                                                                  !/*nu        237*/
      et=elab                                                                   !/*nu        238*/
      etest=et                                                                  !/*nu        239*/
      etrest=et-etkor                                                           !/*nu        240*/
      if (etrest.lt.0.d0) etdd=etrest                                           !/*nu        241*/
      tkor=1.d0+etdd/(tv+tnk+tpk+1.d-10)                                        !/*nu        242*/
      if (tkor.lt.1.d-6) go to 180                                              !/*nu        243*/
      tv=tv*tkor                                                                !/*nu        244*/
      tpk=tpk*tkor                                                              !/*nu        245*/
      tnk=tnk*tkor                                                              !/*nu        246*/
      elpp=et-tv-tpk-tnk                                                        !/*nu        247*/
      go to 220                                                                 !/*nu        248*/
  180 continue                                                                  !/*nu        249*/
      etdd=0.d0                                                                 !/*nu        250*/
      et=elab+ammta                                                             !/*nu        251*/
      etest=et                                                                  !/*nu        252*/
      etrest=et-etkor                                                           !/*nu        253*/
      if (etrest.lt.0.d0) etdd=etrest                                           !/*nu        254*/
      tkor=1.d0+etdd/(tv+tnk+tpk+1.d-10)                                        !/*nu        255*/
      if (tkor.lt.1.d-6) go to 80                                               !/*nu        256*/
      tv=tv*tkor                                                                !/*nu        257*/
      tpk=tpk*tkor                                                              !/*nu        258*/
      tnk=tnk*tkor                                                              !/*nu        259*/
      elpp=et-tv-tpk-tnk                                                        !/*nu        260*/
      t3=elph*.5d0                                                              !/*nu        261*/
  190 continue                                                                  !/*nu        262*/
      do 200 iam=1,15                                                           !/*nu        263*/
      jam=iamc(iam)                                                             !/*nu        264*/
  200 am(jam)=t3                                                                !/*nu        265*/
  210 continue                                                                  !/*nu        266*/
      elpp=t3                                                                   !/*nu        267*/
  220 continue                                                                  !/*nu        268*/
      plpp=sqrt(elpp**2-t3**2)                                                  !/*nu        269*/
      plabou=15.d0                                                              !/*nu        270*/
      if (plpp.lt.plabou) go to 230                                             !/*nu        271*/
      apl=plpp-plabou                                                           !/*nu        272*/
      elpp=sqrt(plabou**2+am(it)**2)                                            !/*nu        273*/
      tpk=tpk+apl*.3d0                                                          !/*nu        274*/
      tnk=tnk+apl*.3d0                                                          !/*nu        275*/
      tv=tv+apl*.4d0                                                            !/*nu        276*/
  230 continue                                                                  !/*nu        277*/
      iforbi=iforbi+1                                                           !/*nu        278*/
      if (iforbi.gt.10) go to 140                                               !/*nu        279*/
      tpnvk=tpk+tnk+tv                                                          !/*nu        280*/
      pplab=plab                                                                !/*nu        281*/
      eelab=elab                                                                !/*nu        282*/
      iperco=1                                                                  !/*nu        283*/
      elran=rang()                                                              !/*nu        284*/
      elanu3=anuc**(-0.7d0)                                                     !/*nu        285*/
      if (elran.gt.elanu3) go to 240                                            !/*nu        286*/
      if (abs(elpp-t3).gt.0.0001d0) go to 250                                   !/*nu        287*/
  240 continue                                                                  !/*nu        288*/
      pplab=plpp                                                                !/*nu        289*/
      eelab=elpp                                                                !/*nu        290*/
      iperco=-1                                                                 !/*nu        291*/
  250 continue                                                                  !/*nu        292*/
      ihaca=ihaca+1                                                             !/*nu        293*/
      itfrhd=it                                                                 !/*nu        294*/
      call ferhad (anuc,znuc,it,pplab,eelab,cx,cy,cz,itta)                      !/*nu        295*/
      if (ihaca.gt.30) then                                                     !/*nu        296*/
        go to 260                                                               !/*nu        297*/
      endif                                                                     !/*nu        298*/
      ihaca=ihaca+1                                                             !/*nu        299*/
      if (ir.eq.0) go to 220                                                    !/*nu        300*/
      if (ihaca.gt.1000) go to 260                                              !/*nu        301*/
      if (inxpi.lt.0.and.ir.eq.2.and.ibar(itr(1)).ne.0.and.nxpi.gt.0) go        !/*nu        302*/
     & to 220                                                                   !/*nu        303*/
      if (ir.lt.2.and.inxpi.lt.0) go to 220                                     !/*nu        304*/
  260 continue                                                                  !/*nu        305*/
      do 270 iam=1,15                                                           !/*nu        306*/
  270 am(iamc(iam))=amhh(iam)                                                   !/*nu        307*/
      if (ifert.lt.0) go to 70                                                  !/*nu        308*/
      ekikor=elpp-elate                                                         !/*nu        309*/
      if (-ekikor.gt.tpnvk) go to 70                                            !/*nu        310*/
      aa65=1.d0+ekikor/tpnvk                                                    !/*nu        311*/
      tpk=tpk*aa65                                                              !/*nu        312*/
      tnk=tnk*aa65                                                              !/*nu        313*/
      tv=tv*aa65                                                                !/*nu        314*/
      meson=0                                                                   !/*nu        315*/
      do 280 irz=1,ir                                                           !/*nu        316*/
      itrz=itr(irz)                                                             !/*nu        317*/
      ibarot=ibar(itrz)                                                         !/*nu        318*/
      if (ibarot.eq.0) meson=meson+1                                            !/*nu        319*/
      if (iabs(ibarot).lt.1.or.inxpi.ge.0) go to 280                            !/*nu        320*/
      el(irz)=sqrt(pl(irz)**2+am(itrz)**2)                                      !/*nu        321*/
  280 continue                                                                  !/*nu        322*/
      if (ir.ge.2) go to 320                                                    !/*nu        323*/
      tki=el(1)-am(itr(1))+1.d-20                                               !/*nu        324*/
      if (tki.le.0.d0) go to 320                                                !/*nu        325*/
      ade=0.12d0*(1.d0+0.003d0*atar)/tki                                        !/*nu        326*/
      dex=exp(-1.57d0*1.57d0/ade)                                               !/*nu        327*/
      an1=(1.d0-dex)*ade*.5d0                                                   !/*nu        328*/
      an2=dex*1.57d0                                                            !/*nu        329*/
      an=an1+an2                                                                !/*nu        330*/
      an1=an1/an                                                                !/*nu        331*/
      v=rang()                                                                  !/*nu        332*/
      if (v.gt.an1) go to 300                                                   !/*nu        333*/
  290 continue                                                                  !/*nu        334*/
      v=rang()                                                                  !/*nu        335*/
      de=sqrt(-ade*log(1.d0-v*(1.d0-dex)))                                      !/*nu        336*/
      if (de.gt.1.57d0) go to 290                                               !/*nu        337*/
      go to 310                                                                 !/*nu        338*/
  300 continue                                                                  !/*nu        339*/
      v=rang()                                                                  !/*nu        340*/
      de=-v                                                                     !/*nu        341*/
      de=atan2(sqrt(dp1-de**2),de)                                              !/*nu        342*/
  310 continue                                                                  !/*nu        343*/
      call cosi (sfe,cfe)                                                       !/*nu        344*/
      sid=sin(de)                                                               !/*nu        345*/
      cod=cos(de)                                                               !/*nu        346*/
      call ttrans (cxr(1),cyr(1),czr(1),cod,sid,sfe,cfe,ccxr,ccyr,cczr)         !/*nu        347*/
      cxr(1)=ccxr                                                               !/*nu        348*/
      cyr(1)=ccyr                                                               !/*nu        349*/
      czr(1)=cczr                                                               !/*nu        350*/
  320 continue                                                                  !/*nu        351*/
      etest=elab                                                                !/*nu        352*/
      if (ir.ge.2) then                                                         !/*nu        353*/
        tanuc=tanuc-1.d0                                                        !/*nu        354*/
        i1=itta/7                                                               !/*nu        355*/
        a1=dble(i1)                                                             !/*nu        356*/
        tpnuc=tpnuc-1.d0+a1                                                     !/*nu        357*/
        tnnuc=tnnuc-a1                                                          !/*nu        358*/
        anow=anow-1.d0                                                          !/*nu        359*/
        znow=znow-1.d0+a1                                                       !/*nu        360*/
        ktarp=ktarp+1-i1                                                        !/*nu        361*/
        ktarn=ktarn+i1                                                          !/*nu        362*/
        etest=etest+am(itta)                                                    !/*nu        363*/
      else                                                                      !/*nu        364*/
      endif                                                                     !/*nu        365*/
      irn=0                                                                     !/*nu        366*/
      irtvi=0                                                                   !/*nu        367*/
      irtvj=0                                                                   !/*nu        368*/
      ithinv=0                                                                  !/*nu        369*/
      ithinr=0                                                                  !/*nu        370*/
      itan=0                                                                    !/*nu        371*/
      do 420 i=1,ir                                                             !/*nu        372*/
      t1=el(i)                                                                  !/*nu        373*/
      i1=itr(i)                                                                 !/*nu        374*/
      if (t1.gt.ethr+am(i1)) go to 410                                          !/*nu        375*/
      tv=tv+t1                                                                  !/*nu        376*/
      itvt=0                                                                    !/*nu        377*/
      if (irtvi.gt.0) go to 360                                                 !/*nu        378*/
      if (i.gt.ir) go to 350                                                    !/*nu        379*/
      istv=0                                                                    !/*nu        380*/
      do 330 irt=i,ir                                                           !/*nu        381*/
      istv=istv+1                                                               !/*nu        382*/
      itrirt=itr(irt)                                                           !/*nu        383*/
      if (ithinr.eq.itrirt.or.ithinv.gt.0) go to 330                            !/*nu        384*/
      if (ibar(itrirt).ne.ibar(it).or.abs(am(itrirt)-am(it)).gt.0.01d0)         !/*nu        385*/
     & go to 330                                                                !/*nu        386*/
      itvt=1                                                                    !/*nu        387*/
      irtvi=irtvi+1                                                             !/*nu        388*/
      ithinv=itrirt                                                             !/*nu        389*/
      go to 340                                                                 !/*nu        390*/
  330 continue                                                                  !/*nu        391*/
  340 continue                                                                  !/*nu        392*/
      if (irtvi.gt.0) tv=tv-am(it)                                              !/*nu        393*/
      if (irtvi.gt.0) etest=etest-am(it)                                        !/*nu        394*/
  350 continue                                                                  !/*nu        395*/
  360 continue                                                                  !/*nu        396*/
      if (irtvj.gt.0) go to 400                                                 !/*nu        397*/
      if (i.gt.ir) go to 390                                                    !/*nu        398*/
      istv=0                                                                    !/*nu        399*/
      do 370 irt=i,ir                                                           !/*nu        400*/
      istv=istv+1                                                               !/*nu        401*/
      itrirt=itr(irt)                                                           !/*nu        402*/
      if (ithinv.eq.itrirt.or.ithinr.gt.0) go to 370                            !/*nu        403*/
      if (ibar(itrirt).ne.ibar(itta).or.abs(am(itrirt)-am(itta)).gt.0.01        !/*nu        404*/
     & d0) go to 370                                                            !/*nu        405*/
      irtvj=irtvj+1                                                             !/*nu        406*/
      ithinr=itrirt                                                             !/*nu        407*/
      go to 380                                                                 !/*nu        408*/
  370 continue                                                                  !/*nu        409*/
  380 continue                                                                  !/*nu        410*/
      if (itvt.gt.0) go to 390                                                  !/*nu        411*/
      if (irtvj.gt.0) tv=tv-am(itta)                                            !/*nu        412*/
  390 continue                                                                  !/*nu        413*/
  400 continue                                                                  !/*nu        414*/
      if (abs(abs(i1-4.5d0)-3.5d0).gt.0.1d0) go to 420                          !/*nu        415*/
      i12=i1/7                                                                  !/*nu        416*/
      a1=i12                                                                    !/*nu        417*/
      itan=itan+1                                                               !/*nu        418*/
      if (itan-1.gt.0) go to 420                                                !/*nu        419*/
      anow=anow+1.d0                                                            !/*nu        420*/
      znow=znow+1.d0-a1                                                         !/*nu        421*/
      ktarp=ktarp-1+i12                                                         !/*nu        422*/
      ktarn=ktarn-i12                                                           !/*nu        423*/
      tpnuc=tpnuc+1.d0-a1                                                       !/*nu        424*/
      tnnuc=tnnuc+a1                                                            !/*nu        425*/
      tanuc=tanuc+1.d0                                                          !/*nu        426*/
      go to 420                                                                 !/*nu        427*/
  410 continue                                                                  !/*nu        428*/
      irn=irn+1                                                                 !/*nu        429*/
      itrn(irn)=i1                                                              !/*nu        430*/
      cxrn(irn)=cxr(i)                                                          !/*nu        431*/
      cyrn(irn)=cyr(i)                                                          !/*nu        432*/
      czrn(irn)=czr(i)                                                          !/*nu        433*/
      elr(irn)=el(i)                                                            !/*nu        434*/
      plr(irn)=pl(i)                                                            !/*nu        435*/
      ibat=ibat+1                                                               !/*nu        436*/
      etest=etest-el(i)                                                         !/*nu        437*/
      enucr=enucr+elr(irn)                                                      !/*nu        438*/
      pxnucr=pxnucr+plr(irn)*cxrn(irn)                                          !/*nu        439*/
      pynucr=pynucr+plr(irn)*cyrn(irn)                                          !/*nu        440*/
      pznucr=pznucr+plr(irn)*czrn(irn)                                          !/*nu        441*/
      icnucr=icnucr+ich(itrn(irn))                                              !/*nu        442*/
      ibnucr=ibnucr+ibar(itrn(irn))                                             !/*nu        443*/
      irntot=irntot+1                                                           !/*nu        444*/
  420 continue                                                                  !/*nu        445*/
      go to 440                                                                 !/*nu        446*/
  430 continue                                                                  !/*nu        447*/
      irn=0                                                                     !/*nu        448*/
  440 continue                                                                  !/*nu        449*/
      iu=irn                                                                    !/*nu        450*/
      ibat=ibat-ibar(it)-ibar(itta)                                             !/*nu        451*/
      if (ibat+1) 450,460,470                                                   !/*nu        452*/
  450 if (ibar(it).eq.0) etest=etest-am(8)                                      !/*nu        453*/
  460 continue                                                                  !/*nu        454*/
      if (ibar(it).eq.0) etest=etest-am(itta)                                   !/*nu        455*/
  470 continue                                                                  !/*nu        456*/
      if ((tpk.le.tnkte).or.(tpnuc.lt.0.5d0)) tv=tv+tpk                         !/*nu        457*/
      if ((tnk.le.tnkte).or.(tnnuc.lt.0.5d0)) tv=tv+tnk                         !/*nu        458*/
  480 continue                                                                  !/*nu        459*/
      t2=am(8)                                                                  !/*nu        460*/
      if (irn.gt.58) go to 520                                                  !/*nu        461*/
      if (tnnuc.lt.0.5d0) go to 520                                             !/*nu        462*/
      if (tnk.le.tnkte) go to 520                                               !/*nu        463*/
      call rbkeka (2,toeff,atar,sqatar,tn,pn,dn)                                !/*nu        464*/
      tnk=tnk-tn                                                                !/*nu        465*/
      if (irn.gt.58) go to 490                                                  !/*nu        466*/
      if (tnnuc.lt.1.5d0) go to 490                                             !/*nu        467*/
      if (tnk.gt.tnkte) go to 500                                               !/*nu        468*/
  490 continue                                                                  !/*nu        469*/
      vt=rang()                                                                 !/*nu        470*/
      thk=tv+tnk                                                                !/*nu        471*/
      if (vt.gt.0.1d0.or.thk.lt.0.d0) tv=tv+tnk+tn                              !/*nu        472*/
      if (vt.gt.0.1d0) go to 520                                                !/*nu        473*/
      if (thk.lt.0.d0) go to 520                                                !/*nu        474*/
      tnk=0.d0                                                                  !/*nu        475*/
      tv=thk                                                                    !/*nu        476*/
  500 continue                                                                  !/*nu        477*/
      tnnuc=tnnuc-1.d0                                                          !/*nu        478*/
      tanuc=tanuc-1.d0                                                          !/*nu        479*/
      anow=anow-1.d0                                                            !/*nu        480*/
      ktarn=ktarn+1                                                             !/*nu        481*/
      if (tn.ge.ethr) go to 510                                                 !/*nu        482*/
      tv=tv+tn                                                                  !/*nu        483*/
      tnnuc=tnnuc+1.d0                                                          !/*nu        484*/
      tanuc=tanuc+1.d0                                                          !/*nu        485*/
      anow=anow+1.d0                                                            !/*nu        486*/
      ktarn=ktarn-1                                                             !/*nu        487*/
      go to 480                                                                 !/*nu        488*/
  510 continue                                                                  !/*nu        489*/
      irn=irn+1                                                                 !/*nu        490*/
      itrn(irn)=8                                                               !/*nu        491*/
      elr(irn)=tn+t2                                                            !/*nu        492*/
      etest=etest-tn                                                            !/*nu        493*/
      pn=sqrt((tn+t2)**2-t2**2)                                                 !/*nu        494*/
      plr(irn)=pn                                                               !/*nu        495*/
      call cosi (sfe,cfe)                                                       !/*nu        496*/
      sid=sin(dn)                                                               !/*nu        497*/
      cod=cos(dn)                                                               !/*nu        498*/
      call ttrans (cx,cy,cz,cod,sid,sfe,cfe,cxrn(irn),cyrn(irn),czrn(irn        !/*nu        499*/
     & ))                                                                       !/*nu        500*/
      enucr=enucr+elr(irn)                                                      !/*nu        501*/
      pxnucr=pxnucr+plr(irn)*cxrn(irn)                                          !/*nu        502*/
      pynucr=pynucr+plr(irn)*cyrn(irn)                                          !/*nu        503*/
      pznucr=pznucr+plr(irn)*czrn(irn)                                          !/*nu        504*/
      icnucr=icnucr+ich(itrn(irn))                                              !/*nu        505*/
      ibnucr=ibnucr+ibar(itrn(irn))                                             !/*nu        506*/
      irntot=irntot+1                                                           !/*nu        507*/
      go to 480                                                                 !/*nu        508*/
  520 continue                                                                  !/*nu        509*/
      if (irn.gt.59) go to 560                                                  !/*nu        510*/
      if (tpnuc.lt.0.5d0) go to 560                                             !/*nu        511*/
      if (tpk.le.tnkte) go to 560                                               !/*nu        512*/
      t2=am(1)                                                                  !/*nu        513*/
      call rbkeka (1,toeff,atar,sqatar,tn,pn,dn)                                !/*nu        514*/
      tpk=tpk-tn                                                                !/*nu        515*/
      if (irn.gt.59) go to 530                                                  !/*nu        516*/
      if (tpnuc.lt.1.5d0) go to 530                                             !/*nu        517*/
      if (tpk.gt.tnkte) go to 540                                               !/*nu        518*/
  530 continue                                                                  !/*nu        519*/
      vt=rang()                                                                 !/*nu        520*/
      thk=tv+tpk                                                                !/*nu        521*/
      if (vt.gt.0.1d0.or.thk.lt.0.d0) tv=tv+tpk+tn                              !/*nu        522*/
      if (vt.gt.0.1d0) go to 560                                                !/*nu        523*/
      if (thk.lt.0.d0) go to 560                                                !/*nu        524*/
      tpk=0.d0                                                                  !/*nu        525*/
      tv=thk                                                                    !/*nu        526*/
  540 continue                                                                  !/*nu        527*/
      tanuc=tanuc-1.d0                                                          !/*nu        528*/
      tpnuc=tpnuc-1.d0                                                          !/*nu        529*/
      anow=anow-1.d0                                                            !/*nu        530*/
      znow=znow-1.d0                                                            !/*nu        531*/
      ktarp=ktarp+1                                                             !/*nu        532*/
      if (tn.ge.ethr) go to 550                                                 !/*nu        533*/
      tv=tv+tn                                                                  !/*nu        534*/
      tanuc=tanuc+1.d0                                                          !/*nu        535*/
      tpnuc=tpnuc+1.d0                                                          !/*nu        536*/
      anow=anow+1.d0                                                            !/*nu        537*/
      znow=znow+1.d0                                                            !/*nu        538*/
      ktarp=ktarp-1                                                             !/*nu        539*/
      go to 520                                                                 !/*nu        540*/
  550 continue                                                                  !/*nu        541*/
      irn=irn+1                                                                 !/*nu        542*/
      itrn(irn)=1                                                               !/*nu        543*/
      elr(irn)=tn+t2                                                            !/*nu        544*/
      etest=etest-tn                                                            !/*nu        545*/
      pn=sqrt((tn+t2)**2-t2**2)                                                 !/*nu        546*/
      plr(irn)=pn                                                               !/*nu        547*/
      call cosi (sfe,cfe)                                                       !/*nu        548*/
      sid=sin(dn)                                                               !/*nu        549*/
      cod=cos(dn)                                                               !/*nu        550*/
      call ttrans (cx,cy,cz,cod,sid,sfe,cfe,cxrn(irn),cyrn(irn),czrn(irn        !/*nu        551*/
     & ))                                                                       !/*nu        552*/
      enucr=enucr+elr(irn)                                                      !/*nu        553*/
      pxnucr=pxnucr+plr(irn)*cxrn(irn)                                          !/*nu        554*/
      pynucr=pynucr+plr(irn)*cyrn(irn)                                          !/*nu        555*/
      pznucr=pznucr+plr(irn)*czrn(irn)                                          !/*nu        556*/
      icnucr=icnucr+ich(itrn(irn))                                              !/*nu        557*/
      ibnucr=ibnucr+ibar(itrn(irn))                                             !/*nu        558*/
      irntot=irntot+1                                                           !/*nu        559*/
      go to 520                                                                 !/*nu        560*/
  560 continue                                                                  !/*nu        561*/
      if (irn-1) 570,580,590                                                    !/*nu        562*/
  570 continue                                                                  !/*nu        563*/
      tv=elab-am(itttt)*ibar(itttt)                                             !/*nu        564*/
      etest=tv                                                                  !/*nu        565*/
      go to 610                                                                 !/*nu        566*/
  580 continue                                                                  !/*nu        567*/
      tv=elab-am(itttt)-elr(1)+am(itrn(1))                                      !/*nu        568*/
      if (tv.gt.0.d0) go to 610                                                 !/*nu        569*/
      tv=0.020d0+0.010d0*(1.d0-2.d0*rang())                                     !/*nu        570*/
      enucr=enucr-elr(1)                                                        !/*nu        571*/
      elr(1)=elab-tv                                                            !/*nu        572*/
      enucr=enucr+elr(1)                                                        !/*nu        573*/
      go to 610                                                                 !/*nu        574*/
  590 continue                                                                  !/*nu        575*/
      if (tv.gt.0.d0) go to 610                                                 !/*nu        576*/
      ttin=elab-am(itttt)*ibar(itttt)                                           !/*nu        577*/
      ttout=0.d0                                                                !/*nu        578*/
      do 600 ies=1,irn                                                          !/*nu        579*/
      ttout=ttout+elr(ies)-am(itrn(ies))*ibar(itrn(ies))                        !/*nu        580*/
  600 continue                                                                  !/*nu        581*/
      tv=ttin-ttout                                                             !/*nu        582*/
      if (tv.lt.0.d0) then                                                      !/*nu        583*/
        tv=0.010d0                                                              !/*nu        584*/
      endif                                                                     !/*nu        585*/
  610 continue                                                                  !/*nu        586*/
      etest=etest-tv                                                            !/*nu        587*/
      return                                                                    !/*nu        588*/
  620 continue                                                                  !/*nu        589*/
      zl=1.d0                                                                   !/*nu        590*/
      it=itnucr                                                                 !/*nu        591*/
      plabcr=plab*plabco                                                        !/*nu        592*/
      call nizl (it,anuc,plabcr,sinuc,zl)                                       !/*nu        593*/
      in1=ins(n)                                                                !/*nu        594*/
      sinuc=sinuc/(sico+1.d-14)                                                 !/*nu        595*/
      zl=zl*sico                                                                !/*nu        596*/
      return                                                                    !/*nu        597*/
!                                                                               !/*nu        598*/
  630 format (3(5h ****/),45h false use of the particle type index, valu        !/*nu        599*/
     &e ,i4,3(5h ****/))                                                        !/*nu        600*/
  640 format (3(5h ****/),64h projectile nucleus momentum outside of the        !/*nu        601*/
     & allowed region, plab,1e15.5/,3(5h ****/))                                !/*nu        602*/
      end                                                                       !/*nu        603*/

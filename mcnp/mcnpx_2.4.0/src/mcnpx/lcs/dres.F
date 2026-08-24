!     $Id: dres.F,v 1.4 2002/03/27 18:15:01 gwm Exp $                         !/*dr          1*/
      subroutine dres (m2,m3,wtfis,exma,ecv_,ex_,fiswt_,icc_,ifisct_            !/*dr          2*/
     & ,ipht_,mnuc_,nocas_,nph_,cos_,cosh_,ep_,hep_,np_,maxph,apr_,ecvr_        !/*dr          3*/
     & ,elv_,eph_,erec_,tph_,u_,uu_,v_,w_,zpr_,l12,l13)                         !/*dr          4*/
!     ==================================================================        !/*dr          5*/
!     dres     calls the following subroutines and functions                    !/*dr          6*/
!        DOST       ENERGY     ERROR      EXP        EXPRNF     FSINFO          !/*dr          7*/
!        GAMR       GTISO      QNRG       SQRT                                  !/*dr          8*/
!     ==================================================================        !/*dr          9*/
!***  npart0(j,1) is the particle counter for the j-th type, for any            !/*dr         10*/
!***  evaporation of particles from the original nucleus before fissioni        !/*dr         11*/
!***  a zero value is inserted in energy tables after evap. products            !/*dr         12*/
!***  from original nucleus                                                     !/*dr         13*/
!***  npart0(j,2) is the particle counter for the j-th type for                 !/*dr         14*/
!***  evaporation of the first fragment                                         !/*dr         15*/
!***  a zero value will be inserted between evaporation product                 !/*dr         16*/
!***  energies of first and second fragments                                    !/*dr         17*/
!***  consequently np_(j) will all be a value of 2 greater than true            !/*dr         18*/
!***  value since zero also inserted after original nucleus evaporation         !/*dr         19*/
#include <lcs/double.h>                                                         /*!dr         20*/
#ifndef OLDM                                                                    /*!dr         21*/
#include <lcs/phprm0.h>                                                         /*!dr         22*/
#endif                                                                          /*!dr         23*/
      logical fising                                                            !/*dr         24*/
#include <lcs/lptS.h>                                                           /*!dr         25*/
#include <lcs/cacom.h>                                                          /*!dr         26*/
#include <lcs/ercom.h>                                                          /*!dr         27*/
#include <lcs/inout.h>                                                          /*!dr         28*/
#include <lcs/orcm.h>                                                           /*!dr         29*/
      dimension apr_(3), zpr_(3), erec_(3), uu_(3), u_(3), v_(3), w_(3),        !/*dr         30*/
     & ecvr_(3), eph_(1), elv_(1), tph_(1)                                      !/*dr         31*/
      dimension ep_(l12,2), hep_(l13,4), cos_(3,l12,2), cosh_(3,l13,4),         !/*dr         32*/
     & np_(6)                                                                   !/*dr         33*/
      dimension exi(20), vp(3,20), erc(20), jjp(20)                             !/*dr         34*/
      dimension parthr(6), exma(6)                                              !/*dr         35*/
! ..  save ifirst, parthr                                                       !/*dr         36*/
      data ifirst /0/                                                           !/*dr         37*/
      data nwr1 /0/, ballim /0.1d0/                                             !/*dr         38*/
#ifdef OLDM                                                                     /*!dr         39*/
      data nwr2 /0/, um /931.20793d0/                                           !/*dr         40*/
#endif                                                                          /*!dr         41*/
      if (ifirst.eq.0) then                                                     !/*dr         42*/
        ifirst=1                                                                !/*dr         43*/
        do 10 i=1,6                                                             !/*dr         44*/
        fla9(i)=ia6(i)                                                          !/*dr         45*/
        flz9(i)=iz6(i)                                                          !/*dr         46*/
#ifdef OLDM                                                                     /*!dr         47*/
        zmass(i)=exmass(i)+um*fla9(i)                                           !/*dr         48*/
#else                                                                           /*!dr         49*/
        zmass(i)=zmass9(i)                                                      !/*dr         50*/
#endif                                                                          /*!dr         51*/
   10   exma(i)=exmass(i)                                                       !/*dr         52*/
        exbe=energy(8.d0,4.d0)-2.d0*exmass(6)                                   !/*dr         53*/
        parthr(1)=1.0d+30                                                       !/*dr         54*/
        parthr(2)=1.0d+30                                                       !/*dr         55*/
        parthr(3)=exmass(1)+exmass(2)-exmass(3)                                 !/*dr         56*/
        parthr(4)=exmass(1)+exmass(3)-exmass(4)                                 !/*dr         57*/
        parthr(5)=exmass(2)+exmass(3)-exmass(5)                                 !/*dr         58*/
        parthr(6)=exmass(2)+exmass(4)-exmass(6)                                 !/*dr         59*/
      endif                                                                     !/*dr         60*/
      fkey=dp0                                                                  !/*dr         61*/
#ifndef OLDM                                                                    /*!dr         62*/
      mnuc_=0                                                                   !/*dr         63*/
      apr_(1)=dp0                                                               !/*dr         64*/
      apr_(2)=dp0                                                               !/*dr         65*/
#endif                                                                          /*!dr         66*/
      do 20 i=1,6                                                               !/*dr         67*/
      np_(i)=0                                                                  !/*dr         68*/
   20 smom1(i)=dp0                                                              !/*dr         69*/
      ja=m2                                                                     !/*dr         70*/
      jz=m3                                                                     !/*dr         71*/
      uu=ex_                                                                    !/*dr         72*/
      nfiss=0                                                                   !/*dr         73*/
      fisinh=.false.                                                            !/*dr         74*/
      penbar=.false.                                                            !/*dr         75*/
      fising=.false.                                                            !/*dr         76*/
!.......................................................................        !/*dr         77*/
!///// start of loop /////                                                      !/*dr         78*/
   30 continue                                                                  !/*dr         79*/
      fja=ja                                                                    !/*dr         80*/
      fjz=jz                                                                    !/*dr         81*/
      ex2=energy(fja,fjz)                                                       !/*dr         82*/
#ifdef OLDM                                                                     /*!dr         83*/
      rnmass=ex2+um*ja                                                          !/*dr         84*/
#else                                                                           /*!dr         85*/
      rnmass=xmnuc(fja,fjz)                                                     !/*dr         86*/
#endif                                                                          /*!dr         87*/
      rnmass=rnmass+uu                                                          !/*dr         88*/
   40 continue                                                                  !/*dr         89*/
!.......................................................................        !/*dr         90*/
!///// Fermi breakup routines /////                                             !/*dr         91*/
      if (ja.gt.20.or.(ja.gt.13.and.uu.gt.44.0)) go to 140                      !/*dr         92*/
      if (ifbrk.eq.0.and.ja.ge.6) go to 140                                     !/*dr         93*/
      call moment (jz,ja,coslbr(1),coslbr(2),coslbr(3),uu,erec,ierr,exi         !/*dr         94*/
     & ,vp,erc,npt,jjp)                                                         !/*dr         95*/
      if (ierr) 70,50,140                                                       !/*dr         96*/
   50 continue                                                                  !/*dr         97*/
      uu=dp0                                                                    !/*dr         98*/
      erec=dp0                                                                  !/*dr         99*/
      nhev=0                                                                    !/*dr        100*/
      do 60 i=1,npt                                                             !/*dr        101*/
      ipt=0                                                                     !/*dr        102*/
      if (jjp(i).le.5) ipt=jjp(i)                                               !/*dr        103*/
      if (jjp(i).eq.7) ipt=6                                                    !/*dr        104*/
      if (ipt.gt.0) then                                                        !/*dr        105*/
        if (exi(i).ne.dp0) call expirx(1,'DRES','exi(i).ne.dp0')                !/*dr        106*/
        ja=ja-ia6(ipt)                                                          !/*dr        107*/
        jz=jz-iz6(ipt)                                                          !/*dr        108*/
        np_(ipt)=np_(ipt)+1                                                     !/*dr        109*/
        smom1(ipt)=smom1(ipt)+erc(i)                                            !/*dr        110*/
        if (ipt.le.2) then                                                      !/*dr        111*/
          ep_(np_(ipt),ipt)=erc(i)                                              !/*dr        112*/
          cos_(1,np_(ipt),ipt)=vp(1,i)                                          !/*dr        113*/
          cos_(2,np_(ipt),ipt)=vp(2,i)                                          !/*dr        114*/
          cos_(3,np_(ipt),ipt)=vp(3,i)                                          !/*dr        115*/
        else                                                                    !/*dr        116*/
          hep_(np_(ipt),ipt-2)=erc(i)                                           !/*dr        117*/
          cosh_(1,np_(ipt),ipt-2)=vp(1,i)                                       !/*dr        118*/
          cosh_(2,np_(ipt),ipt-2)=vp(2,i)                                       !/*dr        119*/
          cosh_(3,np_(ipt),ipt-2)=vp(3,i)                                       !/*dr        120*/
        endif                                                                   !/*dr        121*/
      else                                                                      !/*dr        122*/
        nhev=nhev+1                                                             !/*dr        123*/
        if (nhev.gt.1) call expirx(1,'DRES','nhev.gt.1')                        !/*dr        124*/
        uu=exi(i)                                                               !/*dr        125*/
        erec=erc(i)                                                             !/*dr        126*/
        coslbr(1)=vp(1,i)                                                       !/*dr        127*/
        coslbr(2)=vp(2,i)                                                       !/*dr        128*/
        coslbr(3)=vp(3,i)                                                       !/*dr        129*/
      endif                                                                     !/*dr        130*/
   60 continue                                                                  !/*dr        131*/
      go to 230                                                                 !/*dr        132*/
   70 continue                                                                  !/*dr        133*/
      do 80 j=1,6                                                               !/*dr        134*/
      if (ja.eq.ia6(j).and.jz.eq.iz6(j)) go to 90                               !/*dr        135*/
   80 continue                                                                  !/*dr        136*/
      go to 230                                                                 !/*dr        137*/
   90 continue                                                                  !/*dr        138*/
      eps=uu+erec                                                               !/*dr        139*/
      np_(j)=np_(j)+1                                                           !/*dr        140*/
      smom1(j)=smom1(j)+eps                                                     !/*dr        141*/
      if (j.gt.2) go to 110                                                     !/*dr        142*/
      ep_(np_(j),j)=eps                                                         !/*dr        143*/
      do 100 i=1,3                                                              !/*dr        144*/
  100 cos_(i,np_(j),j)=coslbr(i)                                                !/*dr        145*/
      go to 130                                                                 !/*dr        146*/
  110 kemiss=j-2                                                                !/*dr        147*/
      hep_(np_(j),kemiss)=eps                                                   !/*dr        148*/
      do 120 i=1,3                                                              !/*dr        149*/
  120 cosh_(i,np_(j),kemiss)=coslbr(i)                                          !/*dr        150*/
  130 erec=dp0                                                                  !/*dr        151*/
      uu=dp0                                                                    !/*dr        152*/
      ja=0                                                                      !/*dr        153*/
      jz=0                                                                      !/*dr        154*/
      go to 230                                                                 !/*dr        155*/
!.......................................................................        !/*dr        156*/
!///// calculate channel probabilities /////                                    !/*dr        157*/
  140 continue                                                                  !/*dr        158*/
      jrn=0                                                                     !/*dr        159*/
      gammaa=dp1+erec/rnmass                                                    !/*dr        160*/
      a=ja                                                                      !/*dr        161*/
      z=jz                                                                      !/*dr        162*/
      ex1=ex2                                                                   !/*dr        163*/
      xlhs=uu+erec+ex1                                                          !/*dr        164*/
      if (ja.eq.8.and.jz.eq.4) then                                             !/*dr        165*/
        jrn=1                                                                   !/*dr        166*/
        k=6                                                                     !/*dr        167*/
        q9(6)=-exbe                                                             !/*dr        168*/
        go to 160                                                               !/*dr        169*/
      endif                                                                     !/*dr        170*/
      call clect (a,z,ja,jz,uu,kfiss)                                           !/*dr        171*/
      if (kfiss.eq.1) go to 240                                                 !/*dr        172*/
      if (sigma.le.dp0) then                                                    !/*dr        173*/
        if (ievap.eq.0) go to 180                                               !/*dr        174*/
        if (ievap.ne.0) go to 200                                               !/*dr        175*/
      endif                                                                     !/*dr        176*/
!.......................................................................        !/*dr        177*/
!///// choose an emitted particle /////                                         !/*dr        178*/
      uran=rang()*sigma                                                         !/*dr        179*/
      sum=dp0                                                                   !/*dr        180*/
      do 150 j=1,6                                                              !/*dr        181*/
      k=j                                                                       !/*dr        182*/
      sum=r9(j)+sum                                                             !/*dr        183*/
      if (sum.ge.uran) go to 160                                                !/*dr        184*/
  150 continue                                                                  !/*dr        185*/
      call expirx(1,'DRES','failed to sample from r9(j)')                       !/*dr        186*/
  160 continue                                                                  !/*dr        187*/
      jemiss=k                                                                  !/*dr        188*/
      ar=a-fla9(jemiss)                                                         !/*dr        189*/
      zr=z-flz9(jemiss)                                                         !/*dr        190*/
      if (nobale.eq.0) then                                                     !/*dr        191*/
        ex2=q9(jemiss)-exmass(jemiss)+ex1                                       !/*dr        192*/
      else                                                                      !/*dr        193*/
        ex2=energy(ar,zr)                                                       !/*dr        194*/
      endif                                                                     !/*dr        195*/
!.......................................................................        !/*dr        196*/
!///// emit a particle /////                                                    !/*dr        197*/
  170 continue                                                                  !/*dr        198*/
      call emit (a,z,ja,jz,kfiss,jrn,cos_,cosh_,ep_,hep_,np_,l12,l13)           !/*dr        199*/
      if (kfiss.ne.0) go to 260                                                 !/*dr        200*/
      if (fkey.ne.dp0) fkey=dp0                                                 !/*dr        201*/
      go to 40                                                                  !/*dr        202*/
!.......................................................................        !/*dr        203*/
!///// normal emission blocked - RAL /////                                      !/*dr        204*/
  180 continue                                                                  !/*dr        205*/
!                                                                               !/*dr        206*/
!      check for proton barrier penetration                                     !/*dr        207*/
!      we allow for a barrier transmission probability                          !/*dr        208*/
!      of down to 10**-6.we use a gammow type relation.                         !/*dr        209*/
!                                                                               !/*dr        210*/
      if (argp.lt.dp0) go to 200                                                !/*dr        211*/
      mm=int(a-dp1)                                                             !/*dr        212*/
!**    coulomb barrier                                                          !/*dr        213*/
      vc=1.1076d0*(z-dp1)/rmass(mm)                                             !/*dr        214*/
      eta=argp/vc                                                               !/*dr        215*/
      if (eta.gt.dp1) go to 190                                                 !/*dr        216*/
      arge=0.60072d0*sqrt(z*rmass(mm)/eta)*(acos(eta)-eta*sqrt(dp1-eta          !/*dr        217*/
     & *eta))                                                                   !/*dr        218*/
      if (arge.gt.13.8d0) go to 200                                             !/*dr        219*/
!                                                                               !/*dr        220*/
!      we predict barrier penetration                                           !/*dr        221*/
!      pick ke uniformly in range 0.0 to argp                                   !/*dr        222*/
!                                                                               !/*dr        223*/
  190 continue                                                                  !/*dr        224*/
      eps=argp*rang()                                                           !/*dr        225*/
      penbar=.true.                                                             !/*dr        226*/
      jemiss=2                                                                  !/*dr        227*/
      call expirx(1,'DRES','eta.gt.dp1 .and. arge.le.13.8')                     !/*dr        228*/
      go to 170                                                                 !/*dr        229*/
!.......................................................................        !/*dr        230*/
!///// normal emission blocked - ORNL /////                                     !/*dr        231*/
  200 continue                                                                  !/*dr        232*/
!///// switch of FKEY - to be eliminated in future /////                        !/*dr        233*/
      if (fkey.ne.dp1) then                                                     !/*dr        234*/
        fkey=dp1                                                                !/*dr        235*/
        go to 140                                                               !/*dr        236*/
      endif                                                                     !/*dr        237*/
!.......................................................................        !/*dr        238*/
!///// no further evaporation /////                                             !/*dr        239*/
      if (ja.gt.5) go to 230                                                    !/*dr        240*/
      if (ja.eq.5) go to 40                                                     !/*dr        241*/
      if (ifbrk.ne.0) call expirx(1,'DRES','ifbrk.ne.0')                        !/*dr        242*/
      do 210 j=1,6                                                              !/*dr        243*/
      if (jz.eq.iz6(j).and.ja.eq.ia6(j)) go to 220                              !/*dr        244*/
  210 continue                                                                  !/*dr        245*/
      go to 40                                                                  !/*dr        246*/
!.......................................................................        !/*dr        247*/
!///// store - residual nuc is of emitted particle type /////                   !/*dr        248*/
  220 continue                                                                  !/*dr        249*/
      if (uu.gt.parthr(j)) go to 40                                             !/*dr        250*/
      jemiss=j                                                                  !/*dr        251*/
      eps=uu+erec                                                               !/*dr        252*/
      np_(jemiss)=np_(jemiss)+1                                                 !/*dr        253*/
      smom1(jemiss)=smom1(jemiss)+eps                                           !/*dr        254*/
      ja=0                                                                      !/*dr        255*/
      jz=0                                                                      !/*dr        256*/
      erec=dp0                                                                  !/*dr        257*/
      uu=dp0                                                                    !/*dr        258*/
      if (jemiss.le.2) then                                                     !/*dr        259*/
        ep_(np_(jemiss),jemiss)=eps                                             !/*dr        260*/
        cos_(1,np_(jemiss),jemiss)=coslbr(1)                                    !/*dr        261*/
        cos_(2,np_(jemiss),jemiss)=coslbr(2)                                    !/*dr        262*/
        cos_(3,np_(jemiss),jemiss)=coslbr(3)                                    !/*dr        263*/
      else                                                                      !/*dr        264*/
        kemiss=jemiss-2                                                         !/*dr        265*/
        hep_(np_(jemiss),kemiss)=eps                                            !/*dr        266*/
        cosh_(1,np_(jemiss),kemiss)=coslbr(1)                                   !/*dr        267*/
        cosh_(2,np_(jemiss),kemiss)=coslbr(2)                                   !/*dr        268*/
        cosh_(3,np_(jemiss),kemiss)=coslbr(3)                                   !/*dr        269*/
      endif                                                                     !/*dr        270*/
!.......................................................................        !/*dr        271*/
  230 continue                                                                  !/*dr        272*/
      if (ja.gt.0.and.(jz.le.1.or.(ja-jz).le.1.or.ja.le.5)) write (io,36        !/*dr        273*/
     & 0) nocas_,ja,jz,ex1,uu,q9(1),q9(2),fkey                                  !/*dr        274*/
      if (fisinh) then                                                          !/*dr        275*/
!.......................................................................        !/*dr        276*/
!///// fission termination /////                                                !/*dr        277*/
#ifdef OLDM                                                                     /*!dr        278*/
        zpr_(nfiss)=real(jz)                                                    !/*dr        279*/
        apr_(nfiss)=real(ja)                                                    !/*dr        280*/
        erec_(nfiss)=erec                                                       !/*dr        281*/
        uu_(nfiss)=uu                                                           !/*dr        282*/
        ecvr_(nfiss)=dp0                                                        !/*dr        283*/
        u_(nfiss)=coslbr(1)                                                     !/*dr        284*/
        v_(nfiss)=coslbr(2)                                                     !/*dr        285*/
        w_(nfiss)=coslbr(3)                                                     !/*dr        286*/
#else                                                                           /*!dr        287*/
        if (ja.gt.0) then                                                       !/*dr        288*/
          mnuc_=mnuc_+1                                                         !/*dr        289*/
          zpr_(mnuc_)=real(jz)                                                  !/*dr        290*/
          apr_(mnuc_)=real(ja)                                                  !/*dr        291*/
          erec_(mnuc_)=erec                                                     !/*dr        292*/
          uu_(mnuc_)=uu                                                         !/*dr        293*/
          ecvr_(mnuc_)=dp0                                                      !/*dr        294*/
          u_(mnuc_)=coslbr(1)                                                   !/*dr        295*/
          v_(mnuc_)=coslbr(2)                                                   !/*dr        296*/
          w_(mnuc_)=coslbr(3)                                                   !/*dr        297*/
        endif                                                                   !/*dr        298*/
#endif                                                                          /*!dr        299*/
        if (fising) go to 280                                                   !/*dr        300*/
        nph_=1                                                                  !/*dr        301*/
        eph_(1)=dp0                                                             !/*dr        302*/
        ecv_=dp0                                                                !/*dr        303*/
#ifdef OLDM                                                                     /*!dr        304*/
        if (apr_(1).gt.dp0.and.uu_(1).gt.dp0.and.ipht_.ne.0) call pht           !/*dr        305*/
#else                                                                           /*!dr        306*/
        if (mnuc_.ge.1.and.uu_(1).gt.dp0.and.ipht_.ne.0) call pht               !/*dr        307*/
#endif                                                                          /*!dr        308*/
     &  (apr_(1),zpr_(1),uu_(1),ecv_,eph_,elv_,tph_,nph_,maxph,icc_)            !/*dr        309*/
        nph_=nph_+1                                                             !/*dr        310*/
        eph_(nph_)=dp0                                                          !/*dr        311*/
        ecvr_(1)=ecv_                                                           !/*dr        312*/
        ecv_=dp0                                                                !/*dr        313*/
#ifdef OLDM                                                                     /*!dr        314*/
        if (apr_(2).gt.dp0.and.uu_(2).gt.dp0.and.ipht_.ne.0) call pht           !/*dr        315*/
#else                                                                           /*!dr        316*/
        if (mnuc_.eq.2.and.uu_(2).gt.dp0.and.ipht_.ne.0) call pht               !/*dr        317*/
#endif                                                                          /*!dr        318*/
     &  (apr_(2),zpr_(2),uu_(2),ecv_,eph_,elv_,tph_,nph_,maxph,icc_)            !/*dr        319*/
        nph_=nph_+1                                                             !/*dr        320*/
        eph_(nph_)=dp0                                                          !/*dr        321*/
        ecvr_(2)=ecv_                                                           !/*dr        322*/
#ifdef OLDM                                                                     /*!dr        323*/
        mnuc_=2                                                                 !/*dr        324*/
#endif                                                                          /*!dr        325*/
        ifisct_=ifisct_+1                                                       !/*dr        326*/
        fiswt_=fiswt_+wtfis                                                     !/*dr        327*/
      else                                                                      !/*dr        328*/
!.......................................................................        !/*dr        329*/
!///// non-fission termination /////                                            !/*dr        330*/
#ifdef OLDM                                                                     /*!dr        331*/
        zpr_(1)=real(jz)                                                        !/*dr        332*/
        apr_(1)=real(ja)                                                        !/*dr        333*/
        erec_(1)=erec                                                           !/*dr        334*/
        uu_(1)=uu                                                               !/*dr        335*/
        ecvr_(1)=dp0                                                            !/*dr        336*/
        u_(1)=coslbr(1)                                                         !/*dr        337*/
        v_(1)=coslbr(2)                                                         !/*dr        338*/
        w_(1)=coslbr(3)                                                         !/*dr        339*/
        nph_=0                                                                  !/*dr        340*/
        ecv_=dp0                                                                !/*dr        341*/
        if (apr_(1).gt.dp0.and.uu_(1).gt.dp0.and.ipht_.ne.0) call pht           !/*dr        342*/
     &  (apr_(1),zpr_(1),uu_(1),ecv_,eph_,elv_,tph_,nph_,maxph,icc_)            !/*dr        343*/
        ecvr_(1)=ecv_                                                           !/*dr        344*/
        mnuc_=1                                                                 !/*dr        345*/
#else                                                                           /*!dr        346*/
        if (ja.gt.0) then                                                       !/*dr        347*/
          zpr_(1)=real(jz)                                                      !/*dr        348*/
          apr_(1)=real(ja)                                                      !/*dr        349*/
          erec_(1)=erec                                                         !/*dr        350*/
          uu_(1)=uu                                                             !/*dr        351*/
          ecvr_(1)=dp0                                                          !/*dr        352*/
          u_(1)=coslbr(1)                                                       !/*dr        353*/
          v_(1)=coslbr(2)                                                       !/*dr        354*/
          w_(1)=coslbr(3)                                                       !/*dr        355*/
          nph_=0                                                                !/*dr        356*/
          ecv_=dp0                                                              !/*dr        357*/
          if (apr_(1).gt.dp0.and.uu_(1).gt.dp0.and.ipht_.ne.0) call pht         !/*dr        358*/
     &    (apr_(1),zpr_(1),uu_(1),ecv_,eph_,elv_,tph_,nph_,maxph,icc_)          !/*dr        359*/
          ecvr_(1)=ecv_                                                         !/*dr        360*/
          mnuc_=mnuc_+1                                                         !/*dr        361*/
        endif                                                                   !/*dr        362*/
#endif                                                                          /*!dr        363*/
      endif                                                                     !/*dr        364*/
!.......................................................................        !/*dr        365*/
      hevsum=smom1(3)+smom1(4)+smom1(5)+smom1(6)                                !/*dr        366*/
      return                                                                    !/*dr        367*/
!                                                                               !/*dr        368*/
!***********************************************************************        !/*dr        369*/
!                                                                               !/*dr        370*/
!          fission                                                              !/*dr        371*/
!                                                                               !/*dr        372*/
!***********************************************************************        !/*dr        373*/
!                                                                               !/*dr        374*/
!.......................................................................        !/*dr        375*/
!///// ORNL fission routine /////                                               !/*dr        376*/
  240 continue                                                                  !/*dr        377*/
      kfiss=0                                                                   !/*dr        378*/
      af=a                                                                      !/*dr        379*/
      zf=z                                                                      !/*dr        380*/
      ef=uu                                                                     !/*dr        381*/
      rfmass=ex1                                                                !/*dr        382*/
      ernf=erec                                                                 !/*dr        383*/
      do 250 i=1,3                                                              !/*dr        384*/
      coslf0(i)=coslbr(i)                                                       !/*dr        385*/
  250 continue                                                                  !/*dr        386*/
      fising=.true.                                                             !/*dr        387*/
      call fsinfo                                                               !/*dr        388*/
      if (ncall.eq.1) then                                                      !/*dr        389*/
!---- fission failed--------                                                    !/*dr        390*/
        fising=.false.                                                          !/*dr        391*/
        fisinh=.false.                                                          !/*dr        392*/
        nfiss=0                                                                 !/*dr        393*/
        kfiss=1                                                                 !/*dr        394*/
        go to 140                                                               !/*dr        395*/
      endif                                                                     !/*dr        396*/
      xrhs=r1mass+fisu(1)+fiske(1)+r2mass+fisu(2)+fiske(2)                      !/*dr        397*/
      if (abs(xlhs-xrhs).ge.ballim) then                                        !/*dr        398*/
        nwr1=nwr1+1                                                             !/*dr        399*/
        if (nwr1.le.10) write (io,370) xlhs,xrhs,ja,jz,jemiss                   !/*dr        400*/
      endif                                                                     !/*dr        401*/
      go to 280                                                                 !/*dr        402*/
!.......................................................................        !/*dr        403*/
!///// RAL fission routine /////                                                !/*dr        404*/
  260 continue                                                                  !/*dr        405*/
      do 270 i=1,3                                                              !/*dr        406*/
      coslf0(i)=coslbr(i)                                                       !/*dr        407*/
  270 continue                                                                  !/*dr        408*/
      fising=.true.                                                             !/*dr        409*/
      call fissed (ja,jz,uu)                                                    !/*dr        410*/
      if (.not.fisinh) then                                                     !/*dr        411*/
!---- fission failed--------                                                    !/*dr        412*/
        fising=.false.                                                          !/*dr        413*/
        nfiss=0                                                                 !/*dr        414*/
        penbar=.false.                                                          !/*dr        415*/
        go to 30                                                                !/*dr        416*/
      endif                                                                     !/*dr        417*/
!.......................................................................        !/*dr        418*/
!///// common fission code /////                                                !/*dr        419*/
  280 continue                                                                  !/*dr        420*/
!******* pick up the appropriate fragment                                       !/*dr        421*/
      nfiss=nfiss+1                                                             !/*dr        422*/
      ja=afis(nfiss)                                                            !/*dr        423*/
      jz=zfis(nfiss)                                                            !/*dr        424*/
      uu=fisu(nfiss)                                                            !/*dr        425*/
      erec=fiske(nfiss)                                                         !/*dr        426*/
      fkey=dp0                                                                  !/*dr        427*/
      penbar=.false.                                                            !/*dr        428*/
      if (nfiss.eq.2) then                                                      !/*dr        429*/
        fising=.false.                                                          !/*dr        430*/
        go to 310                                                               !/*dr        431*/
      endif                                                                     !/*dr        432*/
      hevs0=smom1(3)+smom1(5)+smom1(6)+smom1(4)                                 !/*dr        433*/
      do 290 i=1,6                                                              !/*dr        434*/
      numpar=np_(i)+1                                                           !/*dr        435*/
      np_(i)=numpar                                                             !/*dr        436*/
  290 continue                                                                  !/*dr        437*/
      do 300 j=1,3                                                              !/*dr        438*/
  300 coslbr(j)=coslf1(j)                                                       !/*dr        439*/
      go to 340                                                                 !/*dr        440*/
  310 continue                                                                  !/*dr        441*/
      hevs1=smom1(3)+smom1(5)+smom1(6)+smom1(4)                                 !/*dr        442*/
      do 320 i=1,6                                                              !/*dr        443*/
      numpar=np_(i)+1                                                           !/*dr        444*/
      np_(i)=numpar                                                             !/*dr        445*/
  320 continue                                                                  !/*dr        446*/
      do 330 j=1,3                                                              !/*dr        447*/
  330 coslbr(j)=coslf2(j)                                                       !/*dr        448*/
  340 continue                                                                  !/*dr        449*/
      do 350 i=1,6                                                              !/*dr        450*/
      numpar=np_(i)                                                             !/*dr        451*/
      if (i.lt.3) ep_(numpar,i)=dp0                                             !/*dr        452*/
      if (i.gt.2) hep_(numpar,i-2)=dp0                                          !/*dr        453*/
      do 350 j=1,3                                                              !/*dr        454*/
      if (i.lt.3) cos_(j,numpar,i)=dp0                                          !/*dr        455*/
      if (i.gt.2) cosh_(j,numpar,i-2)=dp0                                       !/*dr        456*/
  350 continue                                                                  !/*dr        457*/
      go to 30                                                                  !/*dr        458*/
!.......................................................................        !/*dr        459*/
!                                                                               !/*dr        460*/
  360 format (/5x,'nocas=',i7,' a=',i2,' z=',i2,' exm=',g11.4,' uu=',g11        !/*dr        461*/
     & .4,' q1(n)=',g11.4,' q9(p)=',g11.4,' fkey=',f5.1)                        !/*dr        462*/
  370 format (' ***fission xlhs=',g10.3,' xrhs=',g10.3,' ja=',i3,' jz='         !/*dr        463*/
     & ,i3,' jemiss=',i2)                                                       !/*dr        464*/
      end                                                                       !/*dr        465*/

!     $Id: eventq.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                       !/*ev          1*/
      subroutine eventq (ij,poo,eke,txi,tyi,tzi,we)                             !/*ev          2*/
!                                                                     *         !/*ev          3*/
!---------------------------------------------------------------------*         !/*ev          4*/
!                                                                     *         !/*ev          5*/
#include <lcs/double.h>                                                         /*!ev          6*/
!     input variables:                                                          !/*ev          7*/
!        ij   = type of the interacting particle                                !/*ev          8*/
!        poo  = its momentum in laboratory in gev/c                             !/*ev          9*/
!        eke  = its kinetic energy in laboratory in gev                         !/*ev         10*/
!        txi  = its direction cosine with respect to x-axis                     !/*ev         11*/
!        tyi  = its direction cosine with respect to y-axis                     !/*ev         12*/
!        tzi  = its direction cosine with respect to z-axis                     !/*ev         13*/
!        we   = its weight                                                      !/*ev         14*/
!        imat = number of the medium defines the target nucleus                 !/*ev         15*/
!                                                                               !/*ev         16*/
!**********************************************************************         !/*ev         17*/
#include <fluka89/flprm1.h>                                                     /*!ev         18*/
#include <fluka89/flprm2.h>                                                     /*!ev         19*/
#include <lcs/inout.h>                                                          /*!ev         20*/
#include <fluka89/balanc.h>                                                     /*!ev         21*/
!*ca beam                                                                       !/*ev         22*/
#include <fluka89/countq.h>                                                     /*!ev         23*/
#include <fluka89/fnlsp3.h>                                                     /*!ev         24*/
#include <fluka89/finuc1.h>                                                     /*!ev         25*/
#include <fluka89/hadpar.h>                                                     /*!ev         26*/
#include <fluka89/mapa.h>                                                       /*!ev         27*/
#include <fluka89/nucdat.h>                                                     /*!ev         28*/
#include <fluka89/nucpar.h>                                                     /*!ev         29*/
#include <fluka89/parevt.h>                                                     /*!ev         30*/
#include <fluka89/part1.h>                                                      /*!ev         31*/
#include <fluka89/resnuc.h>                                                     /*!ev         32*/
      logical lnucri                                                            !/*ev         33*/
      dimension ipa(4), kpa(26), soppp(2)                                       !/*ev         34*/
      data ipa /1,8,13,23/                                                      !/*ev         35*/
      data kpa /1,1,3,3,3,3,3,2,2,3,3,3,3,3,3,3,2,2,3,1,1,2,3,3,3,3/            !/*ev         36*/
      data idi /mxp/                                                            !/*ev         37*/
      data ipri /0/                                                             !/*ev         38*/
!     if (ij .eq. 30) then                                                      !/*ev         39*/
!        call eventa(ij,poo,eke,txi,tyi,tzi,we,imat)                            !/*ev         40*/
!        return                                                                 !/*ev         41*/
!     end if                                                                    !/*ev         42*/
!REP  lresmpl = .false.                                                         !/*ev         43*/
      euz=0.d0                                                                  !/*ev         44*/
      pux=0.d0                                                                  !/*ev         45*/
      puy=0.d0                                                                  !/*ev         46*/
      puz=0.d0                                                                  !/*ev         47*/
      icu=0                                                                     !/*ev         48*/
      ibu=0                                                                     !/*ev         49*/
      eintr=0.d0                                                                !/*ev         50*/
      pxintr=0.d0                                                               !/*ev         51*/
      pyintr=0.d0                                                               !/*ev         52*/
      pzintr=0.d0                                                               !/*ev         53*/
      icintr=0                                                                  !/*ev         54*/
      ibintr=0                                                                  !/*ev         55*/
      enucr=0.d0                                                                !/*ev         56*/
      pxnucr=0.d0                                                               !/*ev         57*/
      pynucr=0.d0                                                               !/*ev         58*/
      pznucr=0.d0                                                               !/*ev         59*/
      icnucr=0                                                                  !/*ev         60*/
      ibnucr=0                                                                  !/*ev         61*/
      efrm=0.d0                                                                 !/*ev         62*/
      pxfrm=0.d0                                                                !/*ev         63*/
      pyfrm=0.d0                                                                !/*ev         64*/
      pzfrm=0.d0                                                                !/*ev         65*/
      psea=0.d0                                                                 !/*ev         66*/
      ktarp=0                                                                   !/*ev         67*/
      ktarn=0                                                                   !/*ev         68*/
! ... ichtar = nint(ztar(imat))                                                 !/*ev         69*/
! ... ichtot = nint(ztar(imat)) + ich(ij)                                       !/*ev         70*/
! ... ibtar  = int(amss(imat))                                                  !/*ev         71*/
! ... bbtar  = amss(imat) - dfloat (ibtar)                                      !/*ev         72*/
! ... if ( rang() .lt. bbtar ) ibtar = ibtar + 1                                !/*ev         73*/
! ... ibtot  = ibtar + ibar(ij)                                                 !/*ev         74*/
! ... anow   = dfloat (ibtar)                                                   !/*ev         75*/
! ... znow   = dfloat (ichtar)                                                  !/*ev         76*/
      ibtar=nint(anow)                                                          !/*ev         77*/
      ichtar=nint(znow)                                                         !/*ev         78*/
      ibtot=ibtar+ibar(ij)                                                      !/*ev         79*/
      ichtot=ichtar+ich(ij)                                                     !/*ev         80*/
      bbtar=anow                                                                !/*ev         81*/
      zztar=znow                                                                !/*ev         82*/
      if (ibtar.eq.1) then                                                      !/*ev         83*/
        etot=am(1)+eke+am(ij)                                                   !/*ev         84*/
      else                                                                      !/*ev         85*/
        etot=bbtar*amuc12+eke+am(ij)                                            !/*ev         86*/
      endif                                                                     !/*ev         87*/
      ptot=sqrt((eke+dp2*am(ij))*eke)                                           !/*ev         88*/
      poo=ptot                                                                  !/*ev         89*/
      pxtot=ptot*txi                                                            !/*ev         90*/
      pytot=ptot*tyi                                                            !/*ev         91*/
      pztot=ptot*tzi                                                            !/*ev         92*/
      eotest=zztar*am(1)+(bbtar-zztar)*am(8)+eke+am(ij)                         !/*ev         93*/
      dsopp=0.d0                                                                !/*ev         94*/
      nnhad=0                                                                   !/*ev         95*/
      if (ij.ne.12.and.ij.ne.19) go to 10                                       !/*ev         96*/
      ij=24                                                                     !/*ev         97*/
      if (rang().gt.0.5d0) ij=25                                                !/*ev         98*/
   10 continue                                                                  !/*ev         99*/
      if (poo.ge.5.d0) go to 50                                                 !/*ev        100*/
   20 continue                                                                  !/*ev        101*/
! low-energy (NUCRIN) section                                                   !/*ev        102*/
      lnucri=.true.                                                             !/*ev        103*/
      np=0                                                                      !/*ev        104*/
      elab=eke+am(ij)                                                           !/*ev        105*/
      if (ij.gt.25) then                                                        !/*ev        106*/
        if (ij.eq.26) then                                                      !/*ev        107*/
          ij=23                                                                 !/*ev        108*/
        else                                                                    !/*ev        109*/
          write (io,*) ' *** ij > 25 in eventq, elab = ',elab,' ***'            !/*ev        110*/
          if (elab.lt.0.140d0) then                                             !/*ev        111*/
            write (io,*) 'eventq: energy too low to make a pion,',              !/*ev        112*/
     &      ' particle ignored!! ',ij,elab                                      !/*ev        113*/
            np=0                                                                !/*ev        114*/
            return                                                              !/*ev        115*/
          endif                                                                 !/*ev        116*/
          ij=23                                                                 !/*ev        117*/
          poo=sqrt((elab+am(ij))*(elab-am(ij)))                                 !/*ev        118*/
          ptot=poo                                                              !/*ev        119*/
          pxtot=ptot*txi                                                        !/*ev        120*/
          pytot=ptot*tyi                                                        !/*ev        121*/
          pztot=ptot*tzi                                                        !/*ev        122*/
        endif                                                                   !/*ev        123*/
      endif                                                                     !/*ev        124*/
      kproj=ij                                                                  !/*ev        125*/
      if (ij.eq.17) then                                                        !/*ev        126*/
        kproj=8                                                                 !/*ev        127*/
        poo=sqrt((elab+am(kproj))*(elab-am(kproj)))                             !/*ev        128*/
        ptot=poo                                                                !/*ev        129*/
        pxtot=ptot*txi                                                          !/*ev        130*/
        pytot=ptot*tyi                                                          !/*ev        131*/
        pztot=ptot*tzi                                                          !/*ev        132*/
      elseif (ij.eq.18) then                                                    !/*ev        133*/
        kproj=9                                                                 !/*ev        134*/
        poo=sqrt((elab+am(kproj))*(elab-am(kproj)))                             !/*ev        135*/
        ptot=poo                                                                !/*ev        136*/
        pxtot=ptot*txi                                                          !/*ev        137*/
        pytot=ptot*tyi                                                          !/*ev        138*/
        pztot=ptot*tzi                                                          !/*ev        139*/
      endif                                                                     !/*ev        140*/
      if (ibtar.eq.1) then                                                      !/*ev        141*/
! low-energy hydrogen section                                                   !/*ev        142*/
        tv=0.d0                                                                 !/*ev        143*/
        np=0                                                                    !/*ev        144*/
        itta=1                                                                  !/*ev        145*/
        call hadrin (kproj,poo,elab,txi,tyi,tzi,itta)                           !/*ev        146*/
        do 30 j=1,ih                                                            !/*ev        147*/
        np=np+1                                                                 !/*ev        148*/
        tki(np)=elh(j)-am(ith(j))                                               !/*ev        149*/
        if (tki(np).gt.0.d0) then                                               !/*ev        150*/
          kpart(np)=ith(j)                                                      !/*ev        151*/
          cxr(np)=cxh(j)                                                        !/*ev        152*/
          cyr(np)=cyh(j)                                                        !/*ev        153*/
          czr(np)=czh(j)                                                        !/*ev        154*/
          plr(np)=plh(j)                                                        !/*ev        155*/
          wei(np)=we                                                            !/*ev        156*/
          enucr=enucr+elh(j)                                                    !/*ev        157*/
          pxnucr=pxnucr+plr(np)*cxr(np)                                         !/*ev        158*/
          pynucr=pynucr+plr(np)*cyr(np)                                         !/*ev        159*/
          pznucr=pznucr+plr(np)*czr(np)                                         !/*ev        160*/
          icnucr=icnucr+ich(kpart(np))                                          !/*ev        161*/
          ibnucr=ibnucr+ibar(kpart(np))                                         !/*ev        162*/
        else                                                                    !/*ev        163*/
          np=np-1                                                               !/*ev        164*/
          go to 30                                                              !/*ev        165*/
        endif                                                                   !/*ev        166*/
   30   continue                                                                !/*ev        167*/
        ktarp=1                                                                 !/*ev        168*/
        anow=0.d0                                                               !/*ev        169*/
        znow=0.d0                                                               !/*ev        170*/
        go to 130                                                               !/*ev        171*/
      endif                                                                     !/*ev        172*/
! low-energy non-hydrogen section                                               !/*ev        173*/
      call nucrin (kproj,elab,txi,tyi,tzi,bbtar,zztar,1.)                       !/*ev        174*/
      if (np.eq.0) then                                                         !/*ev        175*/
        return                                                                  !/*ev        176*/
      endif                                                                     !/*ev        177*/
      if (np.gt.idi) go to 150                                                  !/*ev        178*/
      do 40 i=1,np                                                              !/*ev        179*/
      i1=kpart(i)                                                               !/*ev        180*/
      wei(i)=we                                                                 !/*ev        181*/
      tki(i)=tki(i)-am(i1)                                                      !/*ev        182*/
      if (tki(i).ge.0.d0) go to 40                                              !/*ev        183*/
      tki(i)=0.d0                                                               !/*ev        184*/
      plr(i)=0.d0                                                               !/*ev        185*/
   40 continue                                                                  !/*ev        186*/
      go to 130                                                                 !/*ev        187*/
   50 continue                                                                  !/*ev        188*/
! high-energy (NUCRIN) section                                                  !/*ev        189*/
      np=0                                                                      !/*ev        190*/
      lnucri=.false.                                                            !/*ev        191*/
      if (ibtar.eq.1) then                                                      !/*ev        192*/
! high-energy hydrogen section                                                  !/*ev        193*/
        nhad=0                                                                  !/*ev        194*/
        tv=0.d0                                                                 !/*ev        195*/
        kproj=ij                                                                !/*ev        196*/
        ktarg=1                                                                 !/*ev        197*/
!REP                                                                            !/*ev        198*/
!REP trap because DIFFFR is undefined in original coding!                       !/*ev        199*/
!REP    if (ldiffr(kproj).and.rang().lt.difffr.and.poo.gt.pthdff)               !/*ev        200*/
!       call errorf ('eventq.1')                                                !/*ev        201*/
!REP                                                                            !/*ev        202*/
        if (ldiffr(kproj).and.rang().lt.frdiff.and.poo.gt.pthdff) then          !/*ev        203*/
          call difevt (nhad,kproj,ktarg,poo)                                    !/*ev        204*/
        else                                                                    !/*ev        205*/
          call hadevt (nhad,kproj,ktarg,poo)                                    !/*ev        206*/
        endif                                                                   !/*ev        207*/
        do 60 j=1,nhad                                                          !/*ev        208*/
        np=np+1                                                                 !/*ev        209*/
        cfe=1.d0                                                                !/*ev        210*/
        sfe=0.d0                                                                !/*ev        211*/
        kpart(np)=nreh(j)                                                       !/*ev        212*/
        plr(np)=sqrt(pxh(j)**2+pyh(j)**2+pzh(j)**2)                             !/*ev        213*/
        pth=sqrt(pxh(j)**2+pyh(j)**2)                                           !/*ev        214*/
        cde=pzh(j)/plr(np)                                                      !/*ev        215*/
        sde=pth/plr(np)                                                         !/*ev        216*/
        if (pth.ge.1.0d-12) then                                                !/*ev        217*/
          cfe=pxh(j)/pth                                                        !/*ev        218*/
          sfe=pyh(j)/pth                                                        !/*ev        219*/
        endif                                                                   !/*ev        220*/
        call ttrans (txi,tyi,tzi,cde,sde,sfe,cfe,cxr(np),cyr(np),czr(np)        !/*ev        221*/
     &  )                                                                       !/*ev        222*/
        tki(np)=heph(j)-amh(j)                                                  !/*ev        223*/
        wei(np)=we                                                              !/*ev        224*/
        if (tki(np).gt.0.d0) then                                               !/*ev        225*/
          euz=euz+heph(j)                                                       !/*ev        226*/
          pux=pux+plr(np)*cxr(np)                                               !/*ev        227*/
          puy=puy+plr(np)*cyr(np)                                               !/*ev        228*/
          puz=puz+plr(np)*czr(np)                                               !/*ev        229*/
          icu=icu+ich(kpart(np))                                                !/*ev        230*/
          ibu=ibu+ibar(kpart(np))                                               !/*ev        231*/
        else                                                                    !/*ev        232*/
          np=np-1                                                               !/*ev        233*/
        endif                                                                   !/*ev        234*/
   60   continue                                                                !/*ev        235*/
        anow=0.d0                                                               !/*ev        236*/
        znow=0.d0                                                               !/*ev        237*/
        ktarp=1                                                                 !/*ev        238*/
        go to 130                                                               !/*ev        239*/
      endif                                                                     !/*ev        240*/
! high-energy non-hydrogen section                                              !/*ev        241*/
      ekin=eke                                                                  !/*ev        242*/
      if (.not.linctv) then                                                     !/*ev        243*/
        pla=sqrt((ekin+2.d0*am(ij))*ekin)                                       !/*ev        244*/
        go to 110                                                               !/*ev        245*/
      endif                                                                     !/*ev        246*/
! begin INTR section                                                            !/*ev        247*/
      sqrams=sqrt(bbtar)                                                        !/*ev        248*/
      ijj=kpa(ij)                                                               !/*ev        249*/
      tv=eke*ainel(ijj,5,eke,bbtar,sqrams)                                      !/*ev        250*/
      ekin=ekin-tv                                                              !/*ev        251*/
      soppp(1)=sopp(1)                                                          !/*ev        252*/
      soppp(2)=sopp(2)                                                          !/*ev        253*/
      ilo=1                                                                     !/*ev        254*/
   70 continue                                                                  !/*ev        255*/
      ito=ilo+5                                                                 !/*ev        256*/
      soppp(ilo)=soppp(ilo)+ainel(ijj,ito,eke,bbtar,sqrams)*eke                 !/*ev        257*/
   80 continue                                                                  !/*ev        258*/
      call rakeka (ilo,eke,bbtar,tkin,pla,dee)                                  !/*ev        259*/
      if (soppp(ilo).lt.tkin) go to 90                                          !/*ev        260*/
      soppp(ilo)=soppp(ilo)-tkin                                                !/*ev        261*/
      ekin=ekin-tkin                                                            !/*ev        262*/
      np=np+1                                                                   !/*ev        263*/
      if (np.gt.idi) go to 150                                                  !/*ev        264*/
      iilo=ipa(ilo)                                                             !/*ev        265*/
      kpart(np)=iilo                                                            !/*ev        266*/
      wei(np)=we                                                                !/*ev        267*/
      tki(np)=tkin                                                              !/*ev        268*/
      plr(np)=pla                                                               !/*ev        269*/
      call sfecfe (sfe,cfe)                                                     !/*ev        270*/
      call trans (txi,tyi,tzi,dee,sfe,cfe,cxr(np),cyr(np),czr(np))              !/*ev        271*/
      go to 80                                                                  !/*ev        272*/
   90 continue                                                                  !/*ev        273*/
      ilo=ilo+1                                                                 !/*ev        274*/
      if (ilo.lt.3) go to 70                                                    !/*ev        275*/
      pla=sqrt((ekin+2.d0*am(ij))*ekin)                                         !/*ev        276*/
      if (pla.lt.5.d0) go to 20                                                 !/*ev        277*/
      do 100 j=1,np                                                             !/*ev        278*/
      eintr=eintr+tki(j)+am(kpart(j))                                           !/*ev        279*/
      pxintr=pxintr+plr(j)*cxr(j)                                               !/*ev        280*/
      pyintr=pyintr+plr(j)*cyr(j)                                               !/*ev        281*/
      pzintr=pzintr+plr(j)*czr(j)                                               !/*ev        282*/
      icintr=icintr+ich(kpart(j))                                               !/*ev        283*/
      ibintr=ibintr+ibar(kpart(j))                                              !/*ev        284*/
      if (ich(kpart(j)).eq.1) then                                              !/*ev        285*/
        znow=znow-1.d0                                                          !/*ev        286*/
      else                                                                      !/*ev        287*/
      endif                                                                     !/*ev        288*/
      anow=anow-1.d0                                                            !/*ev        289*/
  100 continue                                                                  !/*ev        290*/
      dsopp=soppp(1)+soppp(2)-sopp(1)-sopp(2)                                   !/*ev        291*/
      sopp(1)=soppp(1)                                                          !/*ev        292*/
      sopp(2)=soppp(2)                                                          !/*ev        293*/
! end INTR section                                                              !/*ev        294*/
  110 continue                                                                  !/*ev        295*/
      nnhad=0                                                                   !/*ev        296*/
      call nucevt (nnhad,ij,bbtar,zztar,pla)                                    !/*ev        297*/
      do 120 i=1,nnhad                                                          !/*ev        298*/
      np=np+1                                                                   !/*ev        299*/
      if (np.gt.idi) go to 150                                                  !/*ev        300*/
      kpart(np)=nrenu(i)                                                        !/*ev        301*/
      ptnu=pxnu(i)**2+pynu(i)**2                                                !/*ev        302*/
      plr(np)=sqrt(ptnu+pznu(i)**2)                                             !/*ev        303*/
      ptnu=sqrt(ptnu)                                                           !/*ev        304*/
      cde=pznu(i)/plr(np)                                                       !/*ev        305*/
      sde=ptnu/plr(np)                                                          !/*ev        306*/
      if (sde.lt.anglgb) then                                                   !/*ev        307*/
        cfe=1.d0                                                                !/*ev        308*/
        sfe=0.d0                                                                !/*ev        309*/
      else                                                                      !/*ev        310*/
        cfe=pxnu(i)/ptnu                                                        !/*ev        311*/
        sfe=pynu(i)/ptnu                                                        !/*ev        312*/
      endif                                                                     !/*ev        313*/
      call ttrans (txi,tyi,tzi,cde,sde,sfe,cfe,cxr(np),cyr(np),czr(np))         !/*ev        314*/
      tki(np)=hepnu(i)-amnu(i)                                                  !/*ev        315*/
      wei(np)=we                                                                !/*ev        316*/
      if (tki(np).gt.0.d0) go to 120                                            !/*ev        317*/
      np=np-1                                                                   !/*ev        318*/
      euz=euz-hepnu(i)                                                          !/*ev        319*/
      pux=pux-pxnu(i)                                                           !/*ev        320*/
      puy=puy-pynu(i)                                                           !/*ev        321*/
      puz=puz-pznu(i)                                                           !/*ev        322*/
      icu=icu-ichnu(i)                                                          !/*ev        323*/
      ibu=ibu-ibarnu(i)                                                         !/*ev        324*/
  120 continue                                                                  !/*ev        325*/
      ptnu=pux**2+puy**2                                                        !/*ev        326*/
      putot=sqrt(ptnu+puz**2)                                                   !/*ev        327*/
      if (putot.gt.0.d0) then                                                   !/*ev        328*/
        ptnu=sqrt(ptnu)                                                         !/*ev        329*/
        cde=puz/putot                                                           !/*ev        330*/
        sde=ptnu/putot                                                          !/*ev        331*/
        if (sde.lt.anglgb) then                                                 !/*ev        332*/
          cfe=1.d0                                                              !/*ev        333*/
          sfe=0.d0                                                              !/*ev        334*/
        else                                                                    !/*ev        335*/
          cfe=pux/ptnu                                                          !/*ev        336*/
          sfe=puy/ptnu                                                          !/*ev        337*/
        endif                                                                   !/*ev        338*/
        call ttrans (txi,tyi,tzi,cde,sde,sfe,cfe,uxi,uyi,uzi)                   !/*ev        339*/
        pux=putot*uxi                                                           !/*ev        340*/
        puy=putot*uyi                                                           !/*ev        341*/
        puz=putot*uzi                                                           !/*ev        342*/
      endif                                                                     !/*ev        343*/
! end high-energy non-hydrogen section                                          !/*ev        344*/
  130 continue                                                                  !/*ev        345*/
      eres=etot-euz-enucr-eintr-tv                                              !/*ev        346*/
      pxres=pxtot-pux-pxnucr-pxintr                                             !/*ev        347*/
      pyres=pytot-puy-pynucr-pyintr                                             !/*ev        348*/
      pzres=pztot-puz-pznucr-pzintr                                             !/*ev        349*/
!                                                                               !/*ev        350*/
! ... Earlier code had an error trap here                                       !/*ev        351*/
! ... because ICHTOT,IBTOT,IMAT were undefined.                                 !/*ev        352*/
!                                                                               !/*ev        353*/
      icres=ichtot-icu-icnucr-icintr                                            !/*ev        354*/
      ibres=ibtot-ibu-ibnucr-ibintr                                             !/*ev        355*/
      eotest=eotest-euz-enucr-eintr-tv-dfloat(icres)*am(1)-dfloat(ibres         !/*ev        356*/
     & -icres)*am(8)+efrm                                                       !/*ev        357*/
      if (ipri.ne.1) return                                                     !/*ev        358*/
      write (io,160) np,nnhad,ij,poo,eke,txi,tyi,tzi,we,sopp                    !/*ev        359*/
      do 140 i=1,np                                                             !/*ev        360*/
      write (io,170) i,kpart(i),cxr(i),cyr(i),czr(i),tki(i),plr(i),wei(i        !/*ev        361*/
     & )                                                                        !/*ev        362*/
  140 continue                                                                  !/*ev        363*/
      return                                                                    !/*ev        364*/
  150 continue                                                                  !/*ev        365*/
      write (io,180)                                                            !/*ev        366*/
      stop                                                                      !/*ev        367*/
!                                                                               !/*ev        368*/
  160 format (3i7,8f12.6)                                                       !/*ev        369*/
  170 format (2i5,6f12.6)                                                       !/*ev        370*/
  180 format (1x,'overflow in eventq - increase the size of the',               !/*ev        371*/
     & ' common block finuc.')                                                  !/*ev        372*/
      end                                                                       !/*ev        373*/

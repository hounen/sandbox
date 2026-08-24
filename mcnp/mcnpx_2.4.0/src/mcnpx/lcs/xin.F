!     $Id: xin.F,v 1.6 2002/03/27 18:15:26 gwm Exp $                          !/*xi          1*/
      subroutine xin (nel,mel,iazdex,fmf,z9,a9,rho,sigg,hsigg,sigmx,pcap        !/*xi          2*/
     & ,esxln,esxlp,esxmp)                                                      !/*xi          3*/
!.......................................................................        !/*xi          4*/
!     ityp    particle      sigg index                                          !/*xi          5*/
!      1      proton             1                                              !/*xi          6*/
!      2      neutron            1                                              !/*xi          7*/
!      3      pi+                1                                              !/*xi          8*/
!      4      pi0                1                                              !/*xi          9*/
!      5      pi-                1                                              !/*xi         10*/
!      6      mu+                                                               !/*xi         11*/
!      7      mu-                                                               !/*xi         12*/
!      8      d                  3                                              !/*xi         13*/
!      9      t                  4                                              !/*xi         14*/
!     10      he-3               5                                              !/*xi         15*/
!     11      alpha              6                                              !/*xi         16*/
!     12      photon                                                            !/*xi         17*/
!     13      k+                                                                !/*xi         18*/
!     14      k0 long                                                           !/*xi         19*/
!     15      k0 short                                                          !/*xi         20*/
!     16      k-                                                                !/*xi         21*/
!     17      p-bar              2                                              !/*xi         22*/
!     18      n-bar              2                                              !/*xi         23*/
!.......................................................................        !/*xi         24*/
#include <lcs/double.h>                                                         /*!xi         25*/
#include <lcs/lptS.h>                                                           /*!xi         26*/
#include <lcs/tpcrs.h>                                                          /*!xi         27*/
#include <lcs/inout.h>                                                          /*!xi         28*/
#include <lcs/rucom.h>                                                          /*!xi         29*/
#include <lcs/concom.h>                                                         /*!xi         30*/
#include <lcs/ercom.h>                                                          /*!xi         31*/
#include <lcs/charla.h>                                                         /*!xi         32*/
#ifndef OLDM                                                                    /*!xi         33*/
#include <lcs/wapcom.h>                                                         /*!xi         34*/
#include <lcs/xsmax.h>                                                          /*!xi         35*/
#endif                                                                          /*!xi         36*/
      external  e0bd, bdnopt, countr, inbd                                      !/*xi         37*/
!X nel(lnel+m)                 : # of isotopes in mtl m                         !/*xi         38*/
!X mel(lmel+m)                 : pointer for isotopes in mtl m                  !/*xi         39*/
!X iazdex(liax+mel(mel+m)+l-1) : isotope index for ISABEL model                 !/*xi         40*/
!X fmf(lfmf+mel(lmel+m)+l-1)   : atom fraction for isotope l in mtl m           !/*xi         41*/
!X z9(lz9+mel(mel+m)+l-1)      : charge number for isotope l in mtl m           !/*xi         42*/
!X a9(la9+mel(lmel+m)+l-1)     : mass number for isotope l in mtl m             !/*xi         43*/
!X sigg(lsgg+i,mel(lmel+m)+l-1): index i=1,6 for isotope l in mtl m             !/*xi         44*/
!X hsigg(lhsg+i,m)             : H-xsex for part i in mtl m                     !/*xi         45*/
!X sigmx(lsgx+i,m)             : transport xsex for part i in mtl m             !/*xi         46*/
!X rho(lrho+i)              : atom density for cell i                           !/*xi         47*/
!X pcap(lpcp+mel(lmel+m)+l-1) : pi- capt prob for isotope l in mtl m            !/*xi         48*/
      parameter (iemaxn=31, iemaxp=26)                                          !/*xi         49*/
      dimension nel(1), mel(1), iazdex(1), fmf(1), z9(1), a9(1), rho(1),        !/*xi         50*/
     & sigg(6,1), hsigg(lpt,1), sigmx(lpt,1), pcap(1), esxln(iemaxn,1),         !/*xi         51*/
     & esxlp(iemaxp,1), esxmp(1)                                                !/*xi         52*/
      parameter (liso=500, liso5=liso-5)                                        !/*xi         53*/
      dimension zrev(liso), arev(liso), sigtrg(6,liso)                          !/*xi         54*/
      dimension tot(6)                                                          !/*xi         55*/
      data arev /dp1,dp2,2*dp3,dp4,liso5*dp0/                                   !/*xi         56*/
      data zrev /3*dp1,dp2,dp2,liso5*dp0/                                       !/*xi         57*/
      data nainp /5/                                                            !/*xi         58*/
!X Get new mass data                                                            !/*xi         59*/
!     call getdat                                                               !/*xi         60*/
      if (ibertp.ne.0) then                                                     !/*xi         61*/
         call openlcsdata (nbertp, "xin", "bertin", 1)                          !/*xi         62*/
         endif                                                                  !/*xi         63*/
!                                                                               !/*xi         64*/
! Note to reviewers... I believe that nbertp is set to 28 in inbd.F             !/*xi         65*/
! I don't see how it's value can be changed anywhere. Is this if                !/*xi         66*/
! statement some sort of mistaken attempt at an error check for                 !/*xi         67*/
! correct openning of bertin or something?                                      !/*xi         68*/
!                                                                               !/*xi         69*/
      if (nbertp.gt.0) then                                                     !/*xi         70*/
        rewind nbertp                                                           !/*xi         71*/
        call insig (nbertp,geosig)                                              !/*xi         72*/
        call gthsig (emax,ehin,ehipi,emin)                                      !/*xi         73*/
        if (nbertp.le.0) return                                                 !/*xi         74*/
        call inevap (nbertp)                                                    !/*xi         75*/
        if (ievap.ne.1) then                                                    !/*xi         76*/
          read (nbertp) idmm                                                    !/*xi         77*/
          read (nbertp) idmm                                                    !/*xi         78*/
          read (nbertp) idmm                                                    !/*xi         79*/
        else                                                                    !/*xi         80*/
          call inputf (nbertp)                                                  !/*xi         81*/
        endif                                                                   !/*xi         82*/
        dumm=rdtab(nbertp)                                                      !/*xi         83*/
        call redmas (0,io,nbertp)                                               !/*xi         84*/
#ifndef OLDM                                                                    /*!xi         85*/
        read (nbertp) wapz                                                      !/*xi         86*/
        read (nbertp) elbnde                                                    !/*xi         87*/
        read (nbertp) deln                                                      !/*xi         88*/
        read (nbertp) third                                                     !/*xi         89*/
        read (nbertp) iz0                                                       !/*xi         90*/
        read (nbertp) maxz                                                      !/*xi         91*/
        read (nbertp) maxn                                                      !/*xi         92*/
        read (nbertp) izmin                                                     !/*xi         93*/
        read (nbertp) izmax                                                     !/*xi         94*/
        read (nbertp) cwaps                                                     !/*xi         95*/
        read (nbertp) xmaxa                                                     !/*xi         96*/
        read (nbertp) xmaxs                                                     !/*xi         97*/
        read (nbertp) xmaxt                                                     !/*xi         98*/
#endif                                                                          /*!xi         99*/
        rewind nbertp                                                           !/*xi        100*/
      endif                                                                     !/*xi        101*/
!                                                                               !/*xi        102*/
#ifndef OLDM                                                                    /*!xi        103*/
!     Get B&P data                                                              !/*xi        104*/
      lu=15                                                                     !/*xi        105*/
      call sigtap(-1,lu,io)                                                     !/*xi        106*/
#endif                                                                          /*!xi        107*/
!X Get PHT data                                                                 !/*xi        108*/
      call binin (kdum,ldum)                                                    !/*xi        109*/
!X Initialize FLUKA (incl data reads)                                           !/*xi        110*/
#ifdef LCS                                                                      /*!xi        111*/
      call flukin (in,io,itty)                                                  !/*xi        112*/
#endif                                                                          /*!xi        113*/
!X Set up preequillibrium parameters                                            !/*xi        114*/
      call defcmg                                                               !/*xi        115*/
      if (nbertp.gt.0) go to 30                                                 !/*xi        116*/
      do 10 itp=1,5                                                             !/*xi        117*/
   10 hsigmx(itp)=dp0                                                           !/*xi        118*/
      do 20 i=1,260                                                             !/*xi        119*/
   20 geosig(i)=dp0                                                             !/*xi        120*/
   30 continue                                                                  !/*xi        121*/
      if (iexisa.eq.0.or.ibertp.eq.0) go to 50                                  !/*xi        122*/
!X Get ISABEL cross sections as needed                                          !/*xi        123*/
      call inisbl (ichoic)                                                      !/*xi        124*/
      do 40 i=1,5                                                               !/*xi        125*/
   40 call stora (i,arev,zrev,sigtrg)                                           !/*xi        126*/
   50 continue                                                                  !/*xi        127*/
!X Zero decay constants in NON-transport case                                   !/*xi        128*/
      if (n1col.lt.0) then                                                      !/*xi        129*/
        do 60 i=1,lpt                                                           !/*xi        130*/
        dkcons(i)=dp0                                                           !/*xi        131*/
   60   continue                                                                !/*xi        132*/
      endif                                                                     !/*xi        133*/
!----- Begin material loop                                                      !/*xi        134*/
      do 200 m=1,mxmat                                                          !/*xi        135*/
      if (mcell(m).le.0) go to 200                                              !/*xi        136*/
!X nel1=nel(lnel+m)                                                             !/*xi        137*/
      nel1=nel(m)                                                               !/*xi        138*/
!-----                                                                          !/*xi        139*/
      do 90 l=1,nel1                                                            !/*xi        140*/
!X kmat2=mel(lmel+m)+l-1                                                        !/*xi        141*/
!X ap=a9(la9+kmat2)                                                             !/*xi        142*/
!X zp=z9(lz9+kmat2)                                                             !/*xi        143*/
      kmat2=mel(m)+l-1                                                          !/*xi        144*/
      ap=a9(kmat2)                                                              !/*xi        145*/
      zp=z9(kmat2)                                                              !/*xi        146*/
      if (ap.eq.dp1.and.zp.eq.dp1) go to 90                                     !/*xi        147*/
      do 70 i=1,nainp                                                           !/*xi        148*/
      if (zp.eq.zrev(i).and.ap.eq.arev(i)) go to 80                             !/*xi        149*/
   70 continue                                                                  !/*xi        150*/
      nainp=nainp+1                                                             !/*xi        151*/
      i=nainp                                                                   !/*xi        152*/
      zrev(i)=zp                                                                !/*xi        153*/
      arev(i)=ap                                                                !/*xi        154*/
      if (iexisa.ne.0.and.ibertp.ne.0) call stora (i,arev,zrev,sigtrg)          !/*xi        155*/
   80 continue                                                                  !/*xi        156*/
      iazdex(kmat2)=i                                                           !/*xi        157*/
   90 continue                                                                  !/*xi        158*/
!-----                                                                          !/*xi        159*/
      do 100 k=1,7                                                              !/*xi        160*/
!X sigmx(lsgx+k,m)=dp0                                                          !/*xi        161*/
      sigmx(k,m)=dp0                                                            !/*xi        162*/
  100 continue                                                                  !/*xi        163*/
      do 110 k=1,6                                                              !/*xi        164*/
      tot(k)=dp0                                                                !/*xi        165*/
  110 continue                                                                  !/*xi        166*/
!-----                                                                          !/*xi        167*/
      do 190 l=1,nel1                                                           !/*xi        168*/
      kmat2=mel(m)+l-1                                                          !/*xi        169*/
      ap=a9(kmat2)                                                              !/*xi        170*/
      zp=z9(kmat2)                                                              !/*xi        171*/
      if (ap.eq.dp1.and.zp.eq.dp1) go to 130                                    !/*xi        172*/
      na=a9(kmat2)                                                              !/*xi        173*/
      if (iexisa.gt.0) kk=iazdex(kmat2)                                         !/*xi        174*/
      if (iexisa.lt.2) then                                                     !/*xi        175*/
!X sigg(lsgg+1,kmat2)=fmf(lfmf+kmat2)*geosig(na)                                !/*xi        176*/
        sigg(1,kmat2)=fmf(kmat2)*geosig(na)                                     !/*xi        177*/
      else                                                                      !/*xi        178*/
        sigg(1,kmat2)=fmf(kmat2)*sigtrg(1,kk)                                   !/*xi        179*/
      endif                                                                     !/*xi        180*/
      tot(1)=tot(1)+sigg(1,kmat2)                                               !/*xi        181*/
      if (iexisa.eq.0) go to 190                                                !/*xi        182*/
      do 120 k=2,6                                                              !/*xi        183*/
      sigg(k,kmat2)=fmf(kmat2)*sigtrg(k,kk)                                     !/*xi        184*/
      tot(k)=tot(k)+sigg(k,kmat2)                                               !/*xi        185*/
  120 continue                                                                  !/*xi        186*/
      go to 190                                                                 !/*xi        187*/
  130 continue                                                                  !/*xi        188*/
      if (iexisa.lt.2) then                                                     !/*xi        189*/
        do 140 it=1,5                                                           !/*xi        190*/
  140   hsigg(it,m)=fmf(kmat2)*hsigmx(it)*1.d+24                                !/*xi        191*/
      else                                                                      !/*xi        192*/
        do 150 it=1,5                                                           !/*xi        193*/
  150   hsigg(it,m)=fmf(kmat2)*sigtrg(1,1)                                      !/*xi        194*/
      endif                                                                     !/*xi        195*/
      if (iexisa.eq.0) go to 180                                                !/*xi        196*/
      do 160 k=3,6                                                              !/*xi        197*/
  160 hsigg(k+5,m)=fmf(kmat2)*sigtrg(k,1)                                       !/*xi        198*/
      do 170 k=13,16                                                            !/*xi        199*/
  170 hsigg(k,m)=hsigg(1,m)                                                     !/*xi        200*/
      hsigg(17,m)=fmf(kmat2)*sigtrg(2,1)                                        !/*xi        201*/
      hsigg(18,m)=hsigg(17,m)                                                   !/*xi        202*/
  180 continue                                                                  !/*xi        203*/
  190 continue                                                                  !/*xi        204*/
!-----                                                                          !/*xi        205*/
      sigmx(1,m)=tot(1)+hsigg(1,m)                                              !/*xi        206*/
      sigmx(2,m)=tot(1)+hsigg(2,m)                                              !/*xi        207*/
      sigmx(3,m)=tot(1)+hsigg(3,m)                                              !/*xi        208*/
      sigmx(4,m)=tot(1)                                                         !/*xi        209*/
      sigmx(5,m)=tot(1)+hsigg(5,m)                                              !/*xi        210*/
      sigmx(6,m)=dp0                                                            !/*xi        211*/
      sigmx(7,m)=dp0                                                            !/*xi        212*/
      sigmx(8,m)=tot(3)+hsigg(8,m)                                              !/*xi        213*/
      sigmx(9,m)=tot(4)+hsigg(9,m)                                              !/*xi        214*/
      sigmx(10,m)=tot(5)+hsigg(10,m)                                            !/*xi        215*/
      sigmx(11,m)=tot(6)+hsigg(11,m)                                            !/*xi        216*/
      sigmx(12,m)=dp0                                                           !/*xi        217*/
      sigmx(13,m)=tot(1)+hsigg(13,m)                                            !/*xi        218*/
      sigmx(14,m)=tot(1)+hsigg(13,m)                                            !/*xi        219*/
      sigmx(15,m)=tot(1)+hsigg(13,m)                                            !/*xi        220*/
      sigmx(16,m)=tot(1)+hsigg(13,m)                                            !/*xi        221*/
      sigmx(17,m)=tot(2)+hsigg(17,m)                                            !/*xi        222*/
      sigmx(18,m)=tot(2)+hsigg(18,m)                                            !/*xi        223*/
  200 continue                                                                  !/*xi        224*/
!----- End material loop                                                        !/*xi        225*/
      if (ielas.ge.1) call esneut (mxmat,nel(1),mel(1),a9(1),fmf(1)             !/*xi        226*/
     & ,esxln(1,1))                                                             !/*xi        227*/
      if (ielas.ge.2) call esprot (mxmat,nel(1),mel(1),a9(1),fmf(1)             !/*xi        228*/
     & ,esxlp(1,1),esxmp(1))                                                    !/*xi        229*/
#ifdef LCS_PRINT                                                                /*!xi        230*/
      write (io,280)                                                            !/*xi        231*/
  280 format ('1LAHET cross section processing',73x,'print table 44')           !/*xi        232*/
#endif                                                                          /*!xi        233*/
      do 230 m=1,mxmat                                                          !/*xi        234*/
      med=mcell(m)                                                              !/*xi        235*/
      if (med.le.0) go to 230                                                   !/*xi        236*/
      nel1=nel(m)                                                               !/*xi        237*/
      d=rho(med)                                                                !/*xi        238*/
#ifdef LCS_PRINT                                                                /*!xi        239*/
      write (io,300) nel1,m,(d*sigg(1,mel(m)+l-1),l=1,nel1)                     !/*xi        240*/
  300 format (/'0',20x,'*geometric*cross sections for nuclides 1-',i2,          !/*xi        241*/
     & ' in medium ',i2,' in reciprocal centimeters'/('0',10e12.4/))            !/*xi        242*/
#endif                                                                          /*!xi        243*/
      if (iexisa.eq.0) go to 210                                                !/*xi        244*/
#ifdef LCS_PRINT                                                                /*!xi        245*/
      write (io,310) nel1,m,(d*sigg(2,mel(m)+l-1),l=1,nel1)                     !/*xi        246*/
  310 format (/'0',7x,13hanti-nucleon ,                                         !/*xi        247*/
     & '*geometric*cross sections for nuclides 1-',i2,' in medium ',i2,         !/*xi        248*/
     & ' in reciprocal centimeters'/('0',10e12.4/))                             !/*xi        249*/
      write (io,320) nel1,m,(d*sigg(3,mel(m)+l-1),l=1,nel1)                     !/*xi        250*/
  320 format (/'0',11x,9hdeuteron ,                                             !/*xi        251*/
     & '*geometric*cross sections for nuclides 1-',i2,' in medium ',i2,         !/*xi        252*/
     & ' in reciprocal centimeters'/('0',10e12.4/))                             !/*xi        253*/
      write (io,330) nel1,m,(d*sigg(4,mel(m)+l-1),l=1,nel1)                     !/*xi        254*/
  330 format (/'0',13x,7htriton ,                                               !/*xi        255*/
     & '*geometric*cross sections for nuclides 1-',i2,' in medium ',i2,         !/*xi        256*/
     & ' in reciprocal centimeters'/('0',10e12.4/))                             !/*xi        257*/
      write (io,340) nel1,m,(d*sigg(5,mel(m)+l-1),l=1,nel1)                     !/*xi        258*/
  340 format (/'0',15x,5hhe-3 ,                                                 !/*xi        259*/
     & '*geometric*cross sections for nuclides 1-',i2,' in medium ',i2,         !/*xi        260*/
     & ' in reciprocal centimeters'/('0',10e12.4/))                             !/*xi        261*/
      write (io,350) nel1,m,(d*sigg(6,mel(m)+l-1),l=1,nel1)                     !/*xi        262*/
  350 format (/'0',14x,6halpha ,                                                !/*xi        263*/
     & '*geometric*cross sections for nuclides 1-',i2,' in medium ',i2,         !/*xi        264*/
     & ' in reciprocal centimeters'/('0',10e12.4/))                             !/*xi        265*/
#endif                                                                          /*!xi        266*/
  210 continue                                                                  !/*xi        267*/
#ifdef LCS_PRINT                                                                /*!xi        268*/
      write (io,360) m,(d*hsigg(i,m),i=1,5)                                     !/*xi        269*/
  360 format ('0',10x,73hmaximum hydrogen cross sections for particle ty        !/*xi        270*/
     &pes 0 through 4 in medium ,i2,' in reciprocal centimeters'/'0',27x        !/*xi        271*/
     & ,5e15.4/)                                                                !/*xi        272*/
#endif                                                                          /*!xi        273*/
      if (iexisa.eq.0) go to 220                                                !/*xi        274*/
#ifdef LCS_PRINT                                                                /*!xi        275*/
      write (io,370) m,(d*hsigg(i,m),i=8,11)                                    !/*xi        276*/
  370 format ('0',10x,74hmaximum hydrogen cross sections for particle ty        !/*xi        277*/
     &pes 7 through 10 in medium ,i2,' in reciprocal centimeters'/'0',27        !/*xi        278*/
     & x,5e15.4/)                                                               !/*xi        279*/
      write (io,380) m,(d*hsigg(i,m),i=13,18)                                   !/*xi        280*/
  380 format ('0',10x,75hmaximum hydrogen cross sections for particle ty        !/*xi        281*/
     &pes 12 through 17 in medium ,i2,' in reciprocal centimeters'/'0',2        !/*xi        282*/
     & 7x,6e15.4/)                                                              !/*xi        283*/
#endif                                                                          /*!xi        284*/
  220 continue                                                                  !/*xi        285*/
#ifdef LCS_PRINT                                                                /*!xi        286*/
      write (io,390) m,(d*sigmx(i,m),i=1,7)                                     !/*xi        287*/
  390 format ('0',10x,103htransport cross sections(the sum of *geometric        !/*xi        288*/
     &* xsects+max hydrogen xsect+max decay xsect) for particle/30x,28ht        !/*xi        289*/
     &ypes 0 through 6 in medium ,i2,' in reciprocal centimeters'/'0',20        !/*xi        290*/
     & x,7e12.4/)                                                               !/*xi        291*/
#endif                                                                          /*!xi        292*/
      if (iexisa.eq.0) go to 230                                                !/*xi        293*/
#ifdef LCS_PRINT                                                                /*!xi        294*/
      write (io,400) m,(d*sigmx(i,m),i=8,11)                                    !/*xi        295*/
  400 format ('0',10x,103htransport cross sections(the sum of *geometric        !/*xi        296*/
     &* xsects+max hydrogen xsect+max decay xsect) for particle/30x,29ht        !/*xi        297*/
     &ypes 7 through 10 in medium ,i2,' in reciprocal centimeters'/'0',2        !/*xi        298*/
     & 0x,7e12.4/)                                                              !/*xi        299*/
      write (io,410) m,(d*sigmx(i,m),i=13,18)                                   !/*xi        300*/
  410 format ('0',10x,103htransport cross sections(the sum of *geometric        !/*xi        301*/
     &* xsects+max hydrogen xsect+max decay xsect) for particle/30x,30ht        !/*xi        302*/
     &ypes 12 through 17 in medium ,i2,' in reciprocal centimeters'/'0',        !/*xi        303*/
     & 20x,7e12.4/)                                                             !/*xi        304*/
#endif                                                                          /*!xi        305*/
  230 continue                                                                  !/*xi        306*/
      do 270 m=1,mxmat                                                          !/*xi        307*/
      if (mcell(m).le.0) go to 270                                              !/*xi        308*/
!X nel1=nel(lnel+m)                                                             !/*xi        309*/
      nel1=nel(m)                                                               !/*xi        310*/
      do 240 l=1,nel1                                                           !/*xi        311*/
!X kmat2=mel(lmel+m)+l-1                                                        !/*xi        312*/
!X pcap(lpcp+kmat2)=fmf(lfmf+kmat2)*z9(lz9+kmat2)                               !/*xi        313*/
      pcap(mel(m)+l-1)=fmf(mel(m)+l-1)*z9(mel(m)+l-1)                           !/*xi        314*/
  240 continue                                                                  !/*xi        315*/
      pcaps=dp0                                                                 !/*xi        316*/
      do 250 l=1,nel1                                                           !/*xi        317*/
  250 pcaps=pcaps+pcap(mel(m)+l-1)                                              !/*xi        318*/
      do 260 l=1,nel1                                                           !/*xi        319*/
      pcap(mel(m)+l-1)=pcap(mel(m)+l-1)/pcaps                                   !/*xi        320*/
  260 continue                                                                  !/*xi        321*/
  270 continue                                                                  !/*xi        322*/
!                                                                               !/*xi        323*/
      if (modexs.ne.0) then                                                     !/*xi        324*/
         do 420 i=1,lpt                                                         !/*xi        325*/
  420       lmxs(i)=1                                                           !/*xi        326*/
      endif                                                                     !/*xi        327*/
!                                                                               !/*xi        328*/
      return                                                                    !/*xi        329*/
      end                                                                       !/*xi        330*/

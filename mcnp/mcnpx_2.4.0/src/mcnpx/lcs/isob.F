!     $Id: isob.F,v 1.5 2002/03/27 18:15:09 gwm Exp $                         !/*is          1*/
      subroutine isob                                                           !/*is          2*/
! ISOB       calls...  SQRT  QRDET  FLTRNF  QOU17  QOLLM  QOU18  QOU21          !/*is          3*/
!                      QOU19                                                    !/*is          4*/
!     ISOB   called by...   PCOL                                                !/*is          5*/
#include <lcs/double.h>                                                         /*!is          6*/
      dimension calcin(3)                                                       !/*is          7*/
#include <lcs/isobar.h>                                                         /*!is          8*/
#include <lcs/tpcrs.h>                                                          /*!is          9*/
#include <lcs/inout.h>                                                          /*!is         10*/
      data nter /0/                                                             !/*is         11*/
      if (nter.ne.0) go to 10                                                   !/*is         12*/
      nter=1                                                                    !/*is         13*/
      ik_=0                                                                     !/*is         14*/
      cases_=dp1                                                                !/*is         15*/
      ln_=2                                                                     !/*is         16*/
      pnms_=.708d+13                                                            !/*is         17*/
      sqnm_=2.2638564d+27                                                       !/*is         18*/
      dncms_=4.758d+13                                                          !/*is         19*/
      poms_=.684d+13                                                            !/*is         20*/
      rcpmv_=.50613d+11                                                         !/*is         21*/
   10 continue                                                                  !/*is         22*/
      loop=0                                                                    !/*is         23*/
      pgcnt_=dp0                                                                !/*is         24*/
      pacnt_=dp0                                                                !/*is         25*/
      pecnt=dp0                                                                 !/*is         26*/
      inpt_=0                                                                   !/*is         27*/
      do 20 i=1,48                                                              !/*is         28*/
   20 pt_(i)=dp0                                                                !/*is         29*/
      nof=0                                                                     !/*is         30*/
      begru_=dp0                                                                !/*is         31*/
      nor_=0                                                                    !/*is         32*/
!     nor_=number of record                                                     !/*is         33*/
      nof=nof+1                                                                 !/*is         34*/
!     einc_=inc.part.en.                                                        !/*is         35*/
!     cases_=no.of inc.part.                                                    !/*is         36*/
!     andit_=ang.dist.                                                          !/*is         37*/
!     prtin_=inc.part.                                                          !/*is         38*/
!     trsym_=struck part.                                                       !/*is         39*/
!     randi=input rand no.                                                      !/*is         40*/
!     ip_=type of reaction(-1=s.p.,0=d.p.,1=choice)                             !/*is         41*/
!     ik_=0,struck part.at rest--otherwise assume energy                        !/*is         42*/
      kase=0                                                                    !/*is         43*/
! sentinel for getting cross sections when struck particle has energy           !/*is         44*/
      do 30 i=1,3                                                               !/*is         45*/
   30 calcin(i)=dp0                                                             !/*is         46*/
      strkp_=-dp1                                                               !/*is         47*/
      go to (40,40,50,60,50), no_                                               !/*is         48*/
   40 pm_(1)=dncms_                                                             !/*is         49*/
      go to 100                                                                 !/*is         50*/
   50 pm_(1)=pnms_                                                              !/*is         51*/
      go to 70                                                                  !/*is         52*/
   60 pm_(1)=poms_                                                              !/*is         53*/
   70 if (no_-4) 80,90,90                                                       !/*is         54*/
   80 isw_(11)=1                                                                !/*is         55*/
      go to 100                                                                 !/*is         56*/
   90 isw_(11)=0                                                                !/*is         57*/
  100 pm_(2)=dncms_                                                             !/*is         58*/
      ka=1                                                                      !/*is         59*/
      e_(1)=einc_*rcpmv_+pm_(1)                                                 !/*is         60*/
      pxyz_(1)=dp0                                                              !/*is         61*/
      pxyz_(5)=dp0                                                              !/*is         62*/
      pxyz_(9)=sqrt(e_(1)*e_(1)-pm_(1)*pm_(1))                                  !/*is         63*/
      rlke_=einc_                                                               !/*is         64*/
      if (ik_) 130,110,130                                                      !/*is         65*/
  110 ps_=dp0                                                                   !/*is         66*/
      pxyz_(2)=dp0                                                              !/*is         67*/
      pxyz_(6)=dp0                                                              !/*is         68*/
      pxyz_(10)=dp0                                                             !/*is         69*/
      e_(2)=dncms_                                                              !/*is         70*/
      go to 130                                                                 !/*is         71*/
  120 continue                                                                  !/*is         72*/
  130 if (no_-3) 140,150,150                                                    !/*is         73*/
  140 if (rlke_-35.d+02) 220,220,180                                            !/*is         74*/
  150 if (rlke_-18.d1) 180,180,160                                              !/*is         75*/
  160 if (rlke_-25.d+02) 170,170,180                                            !/*is         76*/
  170 if (kase) 880,220,880                                                     !/*is         77*/
  180 if (ik_) 190,200,190                                                      !/*is         78*/
  190 if (kase) 210,200,210                                                     !/*is         79*/
  200 ne=3                                                                      !/*is         80*/
!     rlke_ in error                                                            !/*is         81*/
      go to 960                                                                 !/*is         82*/
  210 loop=loop+1                                                               !/*is         83*/
      if (loop-1000) 120,120,200                                                !/*is         84*/
  220 if (ip_) 230,450,450                                                      !/*is         85*/
  230 if (no_-3) 450,360,360                                                    !/*is         86*/
  240 lk=2                                                                      !/*is         87*/
      i3_=1                                                                     !/*is         88*/
  250 call qrdet (1,pspcl(1),calcin(1))                                         !/*is         89*/
!     argument=pspcl, p-p, n-n, s.p.                                            !/*is         90*/
      calcin(1)=crdt_(1)                                                        !/*is         91*/
      if (lk-6) 260,310,310                                                     !/*is         92*/
  260 call qrdet (1,pec(1),rlke_)                                               !/*is         93*/
!     elastic cross section                                                     !/*is         94*/
  270 calcin(3)=crdt_(1)+calcin(1)+calcin(2)                                    !/*is         95*/
!     total                                                                     !/*is         96*/
!     p-p, n-n, elastic                                                         !/*is         97*/
      go to 600                                                                 !/*is         98*/
  280 lk=3                                                                      !/*is         99*/
      i3_=2                                                                     !/*is        100*/
  290 call qrdet (1,spcln(1),calcin(1))                                         !/*is        101*/
!     argument=nspcl, n-p, p-n, s.p.                                            !/*is        102*/
      calcin(1)=crdt_(1)                                                        !/*is        103*/
      if (lk-6) 300,310,310                                                     !/*is        104*/
  300 call qrdet (1,ecn(1),rlke_)                                               !/*is        105*/
      go to 270                                                                 !/*is        106*/
!     p-n, n-p, elastic                                                         !/*is        107*/
  310 cratio=calcin(1)/(calcin(1)+calcin(2))                                    !/*is        108*/
      valu1_=rang()                                                             !/*is        109*/
      if (valu1_-cratio) 320,320,340                                            !/*is        110*/
  320 if (ka-no_) 350,330,350                                                   !/*is        111*/
  330 i3_=1                                                                     !/*is        112*/
      go to 260                                                                 !/*is        113*/
  340 if (ka-no_) 300,260,300                                                   !/*is        114*/
  350 i3_=2                                                                     !/*is        115*/
      go to 300                                                                 !/*is        116*/
  360 isw_(10)=0                                                                !/*is        117*/
      isw_(9)=0                                                                 !/*is        118*/
!     9 set for pi0,10 set for pi+-n,pi--p,pi0-p, and pi0-n                     !/*is        119*/
      calcin(1)=rlke_-1.8d+02                                                   !/*is        120*/
      lk=1                                                                      !/*is        121*/
!     pion                                                                      !/*is        122*/
      i3_=0                                                                     !/*is        123*/
      if (no_-4) 370,410,440                                                    !/*is        124*/
  370 if (ka.gt.1) go to 390                                                    !/*is        125*/
  380 call qrdet (1,ppscl(1),calcin(1))                                         !/*is        126*/
      calcin(1)=crdt_(1)                                                        !/*is        127*/
      call qrdet (1,ppec(1),rlke_)                                              !/*is        128*/
      go to 270                                                                 !/*is        129*/
!     pi(plus)-p, s.p. or pi(minus)-n                                           !/*is        130*/
  390 call qrdet (1,pmscl(1),calcin(1))                                         !/*is        131*/
      calcin(1)=crdt_(1)                                                        !/*is        132*/
      call qrdet (1,pmec(1),rlke_)                                              !/*is        133*/
  400 calcin(3)=crdt_(1)                                                        !/*is        134*/
      isw_(10)=2                                                                !/*is        135*/
      call qrdet (1,pmxc(1),rlke_)                                              !/*is        136*/
      calcin(3)=calcin(3)+crdt_(1)+calcin(1)+calcin(2)                          !/*is        137*/
!     pi(plus)-n, s.p) or pi(minus)-p                                           !/*is        138*/
      go to 600                                                                 !/*is        139*/
  410 isw_(9)=2                                                                 !/*is        140*/
      go to (420,430), ka                                                       !/*is        141*/
  420 call qrdet (1,pnscl(1),calcin(1))                                         !/*is        142*/
      calcin(1)=crdt_(1)                                                        !/*is        143*/
      call qrdet (1,pnec(1),rlke_)                                              !/*is        144*/
      go to 400                                                                 !/*is        145*/
!     pi(0)-p, s.p.                                                             !/*is        146*/
  430 call qrdet (1,pnnsl(1),calcin(1))                                         !/*is        147*/
      calcin(1)=crdt_(1)                                                        !/*is        148*/
      call qrdet (1,pnnec(1),rlke_)                                             !/*is        149*/
      go to 400                                                                 !/*is        150*/
!     pi(0)-n, s.p.                                                             !/*is        151*/
  440 if (ka-1) 390,390,380                                                     !/*is        152*/
!     pi(-)-p, pi(-)-n, s.p.                                                    !/*is        153*/
  450 if (rlke_-92.d1) 460,460,510                                              !/*is        154*/
  460 if (ip_) 470,180,470                                                      !/*is        155*/
  470 if (rlke_-36.d1) 180,180,480                                              !/*is        156*/
  480 if (kase) 880,490,880                                                     !/*is        157*/
  490 calcin(1)=rlke_-360.0                                                     !/*is        158*/
      if (no_.gt.1) go to 500                                                   !/*is        159*/
      isw_(4)=1                                                                 !/*is        160*/
      if (ka-1) 540,540,570                                                     !/*is        161*/
  500 isw_(4)=0                                                                 !/*is        162*/
      if (ka-1) 570,570,540                                                     !/*is        163*/
  510 if (kase) 880,520,880                                                     !/*is        164*/
  520 calcin(2)=rlke_-9.2d+02                                                   !/*is        165*/
      calcin(1)=rlke_-360.0                                                     !/*is        166*/
      if (no_.gt.1) go to 590                                                   !/*is        167*/
      isw_(4)=1                                                                 !/*is        168*/
      if (ka.gt.1) go to 560                                                    !/*is        169*/
  530 call qrdet (1,pdpcl(1),calcin(2))                                         !/*is        170*/
!     argument=pdpcl, p-p, n-n, d.p.                                            !/*is        171*/
      calcin(2)=crdt_(1)                                                        !/*is        172*/
!     n-n, or p-p, d.p.                                                         !/*is        173*/
  540 if (ip_) 240,550,550                                                      !/*is        174*/
  550 lk=4+2*(ip_)                                                              !/*is        175*/
      i3_=3                                                                     !/*is        176*/
      go to 250                                                                 !/*is        177*/
  560 call qrdet (1,dpcln(1),calcin(2))                                         !/*is        178*/
!     argument=ndpcl, n-p, p-n, d.p.                                            !/*is        179*/
      calcin(2)=crdt_(1)                                                        !/*is        180*/
  570 if (ip_) 280,580,580                                                      !/*is        181*/
  580 lk=5+ip_                                                                  !/*is        182*/
      i3_=4                                                                     !/*is        183*/
      go to 290                                                                 !/*is        184*/
!     n-p, or p-n, d.p.                                                         !/*is        185*/
  590 isw_(4)=0                                                                 !/*is        186*/
      if (ka-1) 560,560,530                                                     !/*is        187*/
  600 continue                                                                  !/*is        188*/
      kase=1                                                                    !/*is        189*/
      if (ik_) 120,610,120                                                      !/*is        190*/
  610 if (no_-2) 900,900,630                                                    !/*is        191*/
  620 i3_=0                                                                     !/*is        192*/
  630 call qou17 (fripn(1),pnmi(1),fmxsp(1),pcfsl(1),pnfsl(1))                  !/*is        193*/
      if (i3_) 1000,680,640                                                     !/*is        194*/
  640 if (col_(15)-dp1) 680,740,650                                             !/*is        195*/
  650 if (col_(15)-dp3) 720,710,660                                             !/*is        196*/
  660 if (col_(15)-dp5) 730,930,670                                             !/*is        197*/
  670 ne=11                                                                     !/*is        198*/
      go to 960                                                                 !/*is        199*/
  680 call qollm                                                                !/*is        200*/
      if (pt_(38)) 700,690,700                                                  !/*is        201*/
  690 i3_=1                                                                     !/*is        202*/
      go to 750                                                                 !/*is        203*/
  700 i3_=2                                                                     !/*is        204*/
      go to 750                                                                 !/*is        205*/
  710 i3_=4                                                                     !/*is        206*/
      go to 750                                                                 !/*is        207*/
  720 i3_=5                                                                     !/*is        208*/
      go to 750                                                                 !/*is        209*/
  730 i3_=6                                                                     !/*is        210*/
      go to 750                                                                 !/*is        211*/
  740 i3_=3                                                                     !/*is        212*/
  750 call qou18                                                                !/*is        213*/
      i3_=i3_                                                                   !/*is        214*/
      go to (640,760,640,770), i3_                                              !/*is        215*/
  760 ne=12                                                                     !/*is        216*/
      go to 960                                                                 !/*is        217*/
  770 pm_(4)=dncms_                                                             !/*is        218*/
      k=ip_                                                                     !/*is        219*/
      j=2                                                                       !/*is        220*/
      if (ip_) 780,810,790                                                      !/*is        221*/
  780 nwrit=22                                                                  !/*is        222*/
      mm=26                                                                     !/*is        223*/
      go to 820                                                                 !/*is        224*/
  790 if (pt_(38)) 800,780,800                                                  !/*is        225*/
  800 k=k+1                                                                     !/*is        226*/
  810 nwrit=29                                                                  !/*is        227*/
      mm=38                                                                     !/*is        228*/
  820 out_(1)=k                                                                 !/*is        229*/
      do 850 i=2,mm,12                                                          !/*is        230*/
      m=i                                                                       !/*is        231*/
      k=m+3                                                                     !/*is        232*/
      do 840 n=1,2                                                              !/*is        233*/
      do 830 l=m,k                                                              !/*is        234*/
      out_(j)=pt_(l)                                                            !/*is        235*/
  830 j=j+1                                                                     !/*is        236*/
      m=i+6                                                                     !/*is        237*/
  840 k=m+2                                                                     !/*is        238*/
  850 continue                                                                  !/*is        239*/
      nor_=nor_+1                                                               !/*is        240*/
      nwds=nwrit+4                                                              !/*is        241*/
!     no_.of words in record                                                    !/*is        242*/
      do 860 i=1,48                                                             !/*is        243*/
  860 pt_(i)=dp0                                                                !/*is        244*/
      pgcnt_=dp0                                                                !/*is        245*/
      pacnt_=dp0                                                                !/*is        246*/
      loop=0                                                                    !/*is        247*/
      begru_=begru_+dp1                                                         !/*is        248*/
      if (cases_-begru_) 960,990,870                                            !/*is        249*/
  870 if (ik_) 120,880,120                                                      !/*is        250*/
  880 go to (620,890,910,920,950,1010), lk                                      !/*is        251*/
  890 i3_=1                                                                     !/*is        252*/
  900 call qou21 (frinn(1),dmin(1),fmxsn(1),fmxdn(1),fsln(1))                   !/*is        253*/
      go to 630                                                                 !/*is        254*/
  910 i3_=2                                                                     !/*is        255*/
      go to 900                                                                 !/*is        256*/
  920 i3_=3                                                                     !/*is        257*/
      go to 900                                                                 !/*is        258*/
  930 call qou19                                                                !/*is        259*/
      if (i3_) 940,770,770                                                      !/*is        260*/
  940 ne=13                                                                     !/*is        261*/
      go to 960                                                                 !/*is        262*/
  950 i3_=4                                                                     !/*is        263*/
      go to 900                                                                 !/*is        264*/
  960 write (io,1050) ne,nor_,no_,nof,begru_,strkp_,rlke_                       !/*is        265*/
      if (ne-3) 990,970,970                                                     !/*is        266*/
  970 if (calcin(1)) 980,990,980                                                !/*is        267*/
  980 continue                                                                  !/*is        268*/
      nor_=nor_+1                                                               !/*is        269*/
  990 continue                                                                  !/*is        270*/
      return                                                                    !/*is        271*/
 1000 ne=10                                                                     !/*is        272*/
      go to 960                                                                 !/*is        273*/
 1010 continue                                                                  !/*is        274*/
      valu1_=rang()                                                             !/*is        275*/
      if (valu1_-cratio) 1020,1020,1030                                         !/*is        276*/
 1020 if (ka-no_) 910,890,910                                                   !/*is        277*/
 1030 if (ka-no_) 1040,920,1040                                                 !/*is        278*/
 1040 go to 950                                                                 !/*is        279*/
!                                                                               !/*is        280*/
 1050 format (1h1,' error at 135'/1h0,'  ne     nor_    no_     ',              !/*is        281*/
     & 'nof     begru_         strkp_          rlke_'/4i7,3e13.4)               !/*is        282*/
      end                                                                       !/*is        283*/

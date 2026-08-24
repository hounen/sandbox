!     $Id: bert.F,v 1.5 2002/03/27 18:14:54 gwm Exp $                         !/*be          1*/
      subroutine bert (finput,nopart,kind,eray,aray,bray,gray)                  !/*be          2*/
#include <lcs/double.h>                                                         /*!be          3*/
      external btbd                                                             !/*be          4*/
#include <lcs/inout.h>                                                          /*!be          5*/
#include <lcs/l05S.h>                                                           /*!be          6*/
#include <lcs/lptS.h>                                                           /*!be          7*/
      dimension finput(8), kind(1), eray(1), aray(1), bray(1), gray(1)          !/*be          8*/
!     a.c.3526(3410-44) cascade calculation                                     !/*be          9*/
      dimension icc(12)                                                         !/*be         10*/
#include <lcs/tpcrs.h>                                                          /*!be         11*/
#include <lcs/btcom.h>                                                          /*!be         12*/
      amasno=finput(1)                                                          !/*be         13*/
      zee=finput(2)                                                             !/*be         14*/
      einc=finput(3)                                                            !/*be         15*/
      ctofe=finput(4)                                                           !/*be         16*/
      casesn=finput(5)                                                          !/*be         17*/
      andit=finput(6)                                                           !/*be         18*/
      ctofen=finput(8)                                                          !/*be         19*/
      prtin=finput(7)                                                           !/*be         20*/
      ke=0                                                                      !/*be         21*/
!     p0ac(19),ppac(19)                                                         !/*be         22*/
!     sf is a scale factor subject to change                                    !/*be         23*/
!     andit                                                                     !/*be         24*/
!     isobar angular distribution  0,50 percent isotropic 50                    !/*be         25*/
!     percent forward-backward-1,all isotropic-2,all forward-backward           !/*be         26*/
!     =0,all of word in crdet to be considered.  not 0, only part. inpt         !/*be         27*/
!     escaping particle storage     esps                                        !/*be         28*/
!     number of forbidden collisions for neutrons     fcn                       !/*be         29*/
!     number of forbidden collisions for protons   fcp                          !/*be         30*/
!     particle with velocity less than criterion   plvc                         !/*be         31*/
!     particle with velocity greater than criterion   pgvc                      !/*be         32*/
      i18=0                                                                     !/*be         33*/
      i19=0                                                                     !/*be         34*/
      do 10 i=1,60                                                              !/*be         35*/
   10 ipec(i)=0                                                                 !/*be         36*/
      do 20 i=1,553                                                             !/*be         37*/
   20 pt(i)=dp0                                                                 !/*be         38*/
      do 30 i=1,1883                                                            !/*be         39*/
   30 esps(i)=dp0                                                               !/*be         40*/
      do 40 i=1,10                                                              !/*be         41*/
   40 count(i)=dp0                                                              !/*be         42*/
      do 50 i=1,12                                                              !/*be         43*/
   50 icc(i)=0                                                                  !/*be         44*/
      do 60 i=1,11                                                              !/*be         45*/
   60 curr(i)=0                                                                 !/*be         46*/
      space(13)=einc                                                            !/*be         47*/
      no=amasno                                                                 !/*be         48*/
      nmas=1+(no-1)*10                                                          !/*be         49*/
      do 70 i=1,10                                                              !/*be         50*/
      out(i)=crsc(nmas)                                                         !/*be         51*/
   70 nmas=nmas+1                                                               !/*be         52*/
      call rout1                                                                !/*be         53*/
      if (prtin.gt.dp4) go to 1960                                              !/*be         54*/
      no=prtin+dp1                                                              !/*be         55*/
      value1=einc+space(4)                                                      !/*be         56*/
      go to (1970,1970,90,80,90), no                                            !/*be         57*/
   80 ne=3                                                                      !/*be         58*/
!     incident particle is pi 0, ,therefore in error                            !/*be         59*/
      go to 1960                                                                !/*be         60*/
   90 call rout2 (ppac(1))                                                      !/*be         61*/
      if (i1.eq.0) then                                                         !/*be         62*/
        ne=4                                                                    !/*be         63*/
!        value1 gt 2500                                                         !/*be         64*/
        go to 1960                                                              !/*be         65*/
      endif                                                                     !/*be         66*/
      if (no.lt.4) then                                                         !/*be         67*/
        iv=2                                                                    !/*be         68*/
      elseif (no.eq.4) then                                                     !/*be         69*/
        ne=5                                                                    !/*be         70*/
        go to 1960                                                              !/*be         71*/
      else                                                                      !/*be         72*/
        iv=0                                                                    !/*be         73*/
      endif                                                                     !/*be         74*/
      call xyi (1,14,30)                                                        !/*be         75*/
      ip=1                                                                      !/*be         76*/
  100 call rout3                                                                !/*be         77*/
      if (begru.eq.dp0) go to 1900                                              !/*be         78*/
!     if begru=0, last run completed--bg6a                                      !/*be         79*/
      kk=i1                                                                     !/*be         80*/
!     xinc=xi(1)                                                                !/*be         81*/
!     xinc=x-coordinate inc.part.                                               !/*be         82*/
      call rout4                                                                !/*be         83*/
      if (i1.lt.0) go to 2110                                                   !/*be         84*/
      i1=kk                                                                     !/*be         85*/
  110 if (in1.ne.0) go to 1780                                                  !/*be         86*/
      if (ex.gt.d(2)) go to 550                                                 !/*be         87*/
  120 curr(2)=out(13)                                                           !/*be         88*/
      wkrpn(3)=curr(2)                                                          !/*be         89*/
!     k.e. with respect toprotons rg.3                                          !/*be         90*/
      wkrpn(6)=out(16)                                                          !/*be         91*/
!     k.e. with respect toneutrons rg.3                                         !/*be         92*/
      ifca=0                                                                    !/*be         93*/
  130 call bg6ca (3,3)                                                          !/*be         94*/
  140 ifcc=0                                                                    !/*be         95*/
      call abran (6)                                                            !/*be         96*/
      not=not                                                                   !/*be         97*/
      knot=not                                                                  !/*be         98*/
      if (not.eq.4) go to 330                                                   !/*be         99*/
      call bg6c (isw(11))                                                       !/*be        100*/
      value1=rlke                                                               !/*be        101*/
      if (in1.ne.0) go to 1560                                                  !/*be        102*/
      if (not-4) 170,330,150                                                    !/*be        103*/
  150 any=s(not-4)                                                              !/*be        104*/
  160 if (not-5) 180,510,540                                                    !/*be        105*/
  170 any=space(not+13)                                                         !/*be        106*/
!     it=1-6  pipps(20051),bg129(21011),pimpd(21004),pipnd(20644)               !/*be        107*/
  180 call rout5 (ppec(1),pmec(1),pmxc(1))                                      !/*be        108*/
!     ppec(126),pmec(126),pmxc(126)                                             !/*be        109*/
  190 if (clsm-dp2) 720,610,200                                                 !/*be        110*/
!     (pim-p)exchange scattering crs.                                           !/*be        111*/
  200 if (value1.gt.value2) go to 290                                           !/*be        112*/
  210 if (isw(1).ne.0) go to 250                                                !/*be        113*/
      ifc=ifcc+1                                                                !/*be        114*/
      if (in1.ne.0) go to 1570                                                  !/*be        115*/
  220 c(3)=dp0                                                                  !/*be        116*/
  230 c(1)=curr(4)                                                              !/*be        117*/
      c(2)=curr(5)                                                              !/*be        118*/
      c(3)=c(3)+curr(6)+ex+d(1)                                                 !/*be        119*/
  240 it=it                                                                     !/*be        120*/
      go to (740,950,960,1020,1030,1100,1110,1140,1150,1110,1160,1410,14        !/*be        121*/
     & 20,1150,1430,1440,1450,1460,1540,1510,1520,1530,1850,1860,1870,18        !/*be        122*/
     & 80,1110,1890), it                                                        !/*be        123*/
  250 if (isw(2).ne.0) go to 270                                                !/*be        124*/
      ifc=2+ifcc                                                                !/*be        125*/
      if (in1.ne.0) go to 1580                                                  !/*be        126*/
  260 c(3)=d(2)+d(3)                                                            !/*be        127*/
      go to 230                                                                 !/*be        128*/
  270 ifc=3+ifcc                                                                !/*be        129*/
      if (in1.ne.0) go to 430                                                   !/*be        130*/
  280 c(3)=d(2)+d(3)+d(4)+d(5)                                                  !/*be        131*/
      go to 230                                                                 !/*be        132*/
!     ifc(1-3),bg6c(1502),bg6f(3243),bg6k(4055) collision                       !/*be        133*/
  290 call signex                                                               !/*be        134*/
  300 if (isw(1).eq.0) go to 110                                                !/*be        135*/
      if (in1.ne.0) go to 1590                                                  !/*be        136*/
      if (ex.le.d(6)) go to 120                                                 !/*be        137*/
      if (isw(2).ne.0) go to 320                                                !/*be        138*/
  310 ipec(7)=ipec(7)+1                                                         !/*be        139*/
!     no. of escaped particles on region 2                                      !/*be        140*/
      go to 100                                                                 !/*be        141*/
  320 ipec(11)=ipec(11)+1                                                       !/*be        142*/
!     no. of escaped partices on region 1                                       !/*be        143*/
      go to 100                                                                 !/*be        144*/
  330 i3=-1                                                                     !/*be        145*/
  340 call rout6                                                                !/*be        146*/
      if (i3) 350,360,380                                                       !/*be        147*/
  350 ne=22                                                                     !/*be        148*/
!     impossible for 6--7,curr(1) is lt 3 or gt 5                               !/*be        149*/
      go to 1960                                                                !/*be        150*/
  360 call rout6a                                                               !/*be        151*/
      if (clsm-dp2) 730,480,1760                                                !/*be        152*/
  370 ifca=1                                                                    !/*be        153*/
      if (isw(1).eq.0) go to 220                                                !/*be        154*/
      if (isw(2)) 280,260,280                                                   !/*be        155*/
!     non-deuteron absorption                                                   !/*be        156*/
  380 call rout7                                                                !/*be        157*/
      if (i3.lt.0) go to 350                                                    !/*be        158*/
  390 call rout7a                                                               !/*be        159*/
      i3=i3                                                                     !/*be        160*/
      go to (430,400,500,410,220,280,470,450,260), i3                           !/*be        161*/
  400 ne=6                                                                      !/*be        162*/
!     pwd or nwd less than 7.d0                                                 !/*be        163*/
      go to 1960                                                                !/*be        164*/
  410 if (inc.ne.0) go to 490                                                   !/*be        165*/
      if (isw(3).eq.0) go to 460                                                !/*be        166*/
  420 value1=ex+d(4)                                                            !/*be        167*/
      go to 440                                                                 !/*be        168*/
  430 value1=ex+d(4)+d(5)                                                       !/*be        169*/
  440 if (curr(10).lt.dp2) go to 470                                            !/*be        170*/
  450 value1=value1+d(3)                                                        !/*be        171*/
  460 if (curr(10).gt.dp2) value1=value1+d(2)                                   !/*be        172*/
  470 c(1)=value1*curr(7)+curr(4)                                               !/*be        173*/
      c(2)=value1*curr(8)+curr(5)                                               !/*be        174*/
      c(3)=value1*curr(9)+curr(6)                                               !/*be        175*/
      go to 240                                                                 !/*be        176*/
  480 if (in1.ne.0) go to 1650                                                  !/*be        177*/
      ifca=7                                                                    !/*be        178*/
  490 c(3)=d(2)                                                                 !/*be        179*/
      if (isw(3).ne.0) c(3)=c(3)+d(3)+d(4)                                      !/*be        180*/
      go to 230                                                                 !/*be        181*/
  500 value1=ex                                                                 !/*be        182*/
      if (inc) 260,440,260                                                      !/*be        183*/
  510 if (rlke.gt.2500.d0) then                                                 !/*be        184*/
        ne=26                                                                   !/*be        185*/
!        rlke gt 2500                                                           !/*be        186*/
        go to 1960                                                              !/*be        187*/
      endif                                                                     !/*be        188*/
      if (rlke.gt.180.d0) go to 530                                             !/*be        189*/
  520 call signex                                                               !/*be        190*/
      if (clsm-dp2) 690,630,300                                                 !/*be        191*/
  530 value1=value1-180.d0                                                      !/*be        192*/
      call crjab (1,ppscl(1))                                                   !/*be        193*/
!     ppscl(117)                                                                !/*be        194*/
!     (pip-p)single prod. crs. low energy                                       !/*be        195*/
      go to 190                                                                 !/*be        196*/
  540 if (rlke.gt.2500.d0) then                                                 !/*be        197*/
        ne=27                                                                   !/*be        198*/
!        rlke gt 2500                                                           !/*be        199*/
        go to 1960                                                              !/*be        200*/
      endif                                                                     !/*be        201*/
      if (rlke.le.180.d0) go to 520                                             !/*be        202*/
      value1=value1-180.d0                                                      !/*be        203*/
      call crjab (1,pmscl(1))                                                   !/*be        204*/
!     pmscl(117)                                                                !/*be        205*/
!     (pim-p)single prod. crs. low energy                                       !/*be        206*/
      go to 190                                                                 !/*be        207*/
  550 if (d(3).eq.dp0) then                                                     !/*be        208*/
        ipec(2)=ipec(2)+1                                                       !/*be        209*/
!        no. of particles incident on region 3 escaping                         !/*be        210*/
        go to 100                                                               !/*be        211*/
      endif                                                                     !/*be        212*/
      isw(1)=1                                                                  !/*be        213*/
      call spac32 (31)                                                          !/*be        214*/
  560 if (in1.ne.0) go to 1600                                                  !/*be        215*/
      if (ex-d(3)) 580,580,660                                                  !/*be        216*/
  570 if (in1.ne.0) go to 590                                                   !/*be        217*/
  580 curr(2)=out(14)                                                           !/*be        218*/
      wkrpn(2)=curr(2)                                                          !/*be        219*/
      wkrpn(5)=out(17)                                                          !/*be        220*/
!     k.e. for protons and neutrons region 2                                    !/*be        221*/
  590 call bg6ca (2,2)                                                          !/*be        222*/
      go to 140                                                                 !/*be        223*/
  600 iv=-1                                                                     !/*be        224*/
      go to 620                                                                 !/*be        225*/
  610 iv=0                                                                      !/*be        226*/
  620 call rout8                                                                !/*be        227*/
      i3=i3                                                                     !/*be        228*/
      go to (630,1610,230,420), i3                                              !/*be        229*/
  630 if (isw(3).eq.0) go to 560                                                !/*be        230*/
  640 if (ex.le.d(5)) go to 570                                                 !/*be        231*/
      if (in1.ne.0) go to 1620                                                  !/*be        232*/
      call spac32 (32)                                                          !/*be        233*/
  650 if (ex-d(6)) 120,120,320                                                  !/*be        234*/
  660 if (d(4).ne.dp0) go to 680                                                !/*be        235*/
      call spac32 (32)                                                          !/*be        236*/
  670 if (ex-d(6)) 120,120,310                                                  !/*be        237*/
  680 isw(2)=1                                                                  !/*be        238*/
      isw(3)=1                                                                  !/*be        239*/
      call spac32 (30)                                                          !/*be        240*/
  690 if (in1.ne.0) go to 1630                                                  !/*be        241*/
  700 call rou10                                                                !/*be        242*/
      if (i3.lt.0) go to 640                                                    !/*be        243*/
  710 call bg6ca (1,1)                                                          !/*be        244*/
      go to 140                                                                 !/*be        245*/
  720 if (value1.le.value2) go to 2490                                          !/*be        246*/
      call signex                                                               !/*be        247*/
      if (in1) 1630,700,1630                                                    !/*be        248*/
  730 if (in1.eq.0) then                                                        !/*be        249*/
        ifca=6                                                                  !/*be        250*/
        go to 260                                                               !/*be        251*/
      endif                                                                     !/*be        252*/
      ifca=9*abs(i6-2)+13*(i6-1)*(3-i6)                                         !/*be        253*/
      go to 1640                                                                !/*be        254*/
  740 i3=1                                                                      !/*be        255*/
      go to 760                                                                 !/*be        256*/
  750 i3=2                                                                      !/*be        257*/
  760 call rou11 (ppdc(1))                                                      !/*be        258*/
!     ppdcl(378)                                                                !/*be        259*/
      i3=i3                                                                     !/*be        260*/
      go to (910,990,1080,770), i3                                              !/*be        261*/
  770 cst=crdt(2)-abs(snt*(crdt(2)-crdt(1)))                                    !/*be        262*/
  780 snt=sqrt(dp1-cst*cst)                                                     !/*be        263*/
  790 call rou12                                                                !/*be        264*/
      if (i3) 800,810,850                                                       !/*be        265*/
  800 ne=7                                                                      !/*be        266*/
!     com is less than -5.0d-6                                                  !/*be        267*/
      go to 1960                                                                !/*be        268*/
  810 if (efrn.lt.value1) go to 880                                             !/*be        269*/
      fcn=fcn+dp1                                                               !/*be        270*/
  820 iv=-1                                                                     !/*be        271*/
  830 i1=0                                                                      !/*be        272*/
      call rou13                                                                !/*be        273*/
      if (i3) 390,840,360                                                       !/*be        274*/
  840 ifc=ifc                                                                   !/*be        275*/
      go to (110,670,650,2130,2320,2360,560,640,690,2290,2350,2340,1780,        !/*be        276*/
     & 1590,1590,1660,1670,1680,1600,640,1780,1590,1590,1630,1780,1590,1        !/*be        277*/
     & 590,1600,1690,1630), ifc                                                 !/*be        278*/
  850 if (efrp.lt.value1) go to 880                                             !/*be        279*/
      fcp=fcp+dp1                                                               !/*be        280*/
      go to 820                                                                 !/*be        281*/
  860 i3=0                                                                      !/*be        282*/
      go to 890                                                                 !/*be        283*/
  870 i3=-1                                                                     !/*be        284*/
      go to 890                                                                 !/*be        285*/
  880 i3=1                                                                      !/*be        286*/
  890 call rou14                                                                !/*be        287*/
      i3=i3                                                                     !/*be        288*/
      go to (850,810,900,2510,1700), i3                                         !/*be        289*/
  900 ne=8                                                                      !/*be        290*/
!     pinst(med-)  punp(pgvc or plvc value-)  stpr(pgvc                         !/*be        291*/
!     gt 40  or plvc gt 80                                                      !/*be        292*/
      go to 1960                                                                !/*be        293*/
  910 i3=1                                                                      !/*be        294*/
      go to 930                                                                 !/*be        295*/
  920 i3=4                                                                      !/*be        296*/
  930 call rou15 (ppdc(1))                                                      !/*be        297*/
!     hppdci(45),ppdci(170)                                                     !/*be        298*/
      i3=i3                                                                     !/*be        299*/
      go to (970,1060,780,940,980,1070), i3                                     !/*be        300*/
  940 ne=9                                                                      !/*be        301*/
!     snn(rlke gte 1000)  dcintp(rlke gte crs.sect.values)                      !/*be        302*/
      go to 1960                                                                !/*be        303*/
  950 pt(2)=dp5                                                                 !/*be        304*/
      ik=it                                                                     !/*be        305*/
!     bg129  (pim-n)                                                            !/*be        306*/
      pt(14)=dp2                                                                !/*be        307*/
      go to 750                                                                 !/*be        308*/
  960 pt(2)=dp5                                                                 !/*be        309*/
!     pipnx  dir. scat.                                                         !/*be        310*/
      pt(14)=dp1                                                                !/*be        311*/
      ik=it                                                                     !/*be        312*/
      go to 750                                                                 !/*be        313*/
  970 i3=1                                                                      !/*be        314*/
      go to 1000                                                                !/*be        315*/
  980 i3=2                                                                      !/*be        316*/
      go to 1000                                                                !/*be        317*/
  990 i3=3                                                                      !/*be        318*/
 1000 call rou16 (pmdd(1))                                                      !/*be        319*/
!     hpmddi(45),pmddi(170),pmddl(378)                                          !/*be        320*/
 1010 if (i3.ne.0) go to 780                                                    !/*be        321*/
      i3=3                                                                      !/*be        322*/
      go to 930                                                                 !/*be        323*/
 1020 pt(2)=dp3                                                                 !/*be        324*/
      pt(14)=dp2                                                                !/*be        325*/
      ik=3                                                                      !/*be        326*/
      go to 750                                                                 !/*be        327*/
 1030 pt(14)=dp2                                                                !/*be        328*/
 1040 ik=it                                                                     !/*be        329*/
 1050 pt(2)=dp4                                                                 !/*be        330*/
      pm(3)=poms                                                                !/*be        331*/
!     pi 0 mass/cm                                                              !/*be        332*/
      i3=3                                                                      !/*be        333*/
      go to 760                                                                 !/*be        334*/
 1060 i3=1                                                                      !/*be        335*/
      go to 1090                                                                !/*be        336*/
 1070 i3=2                                                                      !/*be        337*/
      go to 1090                                                                !/*be        338*/
 1080 i3=3                                                                      !/*be        339*/
 1090 if (ik.eq.23) then                                                        !/*be        340*/
        call rou16 (pndd(1))                                                    !/*be        341*/
!        hpnddi(45),pnddi(170),pnddl(378)                                       !/*be        342*/
!        (pin-p)drct. cross section int. en.                                    !/*be        343*/
!        (pin-p)drct diff. crs. sec. low energy                                 !/*be        344*/
      else                                                                      !/*be        345*/
        call rou16 (pmdx(1))                                                    !/*be        346*/
!        hpmdxi(45),pmdxi(170),pmdxl(378)                                       !/*be        347*/
!        (pim-p)xch.                                                            !/*be        348*/
      endif                                                                     !/*be        349*/
      go to 1010                                                                !/*be        350*/
 1100 pt(14)=dp1                                                                !/*be        351*/
      go to 1040                                                                !/*be        352*/
 1110 pt(2)=dp1                                                                 !/*be        353*/
!     pim+(pp)  abs                                                             !/*be        354*/
!     it=10,pip+(nn)  abs                                                       !/*be        355*/
 1120 pt(14)=dp2                                                                !/*be        356*/
 1130 continue                                                                  !/*be        357*/
      call polrn (cst,snt)                                                      !/*be        358*/
      go to 790                                                                 !/*be        359*/
 1140 pt(2)=dp2                                                                 !/*be        360*/
!     pin+(nn)  abs                                                             !/*be        361*/
      go to 1120                                                                !/*be        362*/
 1150 pt(2)=dp1                                                                 !/*be        363*/
!     pin+(pp)  abs    als0 pi+ abs                                             !/*be        364*/
      pt(14)=dp1                                                                !/*be        365*/
      go to 1130                                                                !/*be        366*/
 1160 isw(9)=0                                                                  !/*be        367*/
      isw(10)=0                                                                 !/*be        368*/
      i3=0                                                                      !/*be        369*/
      go to 1180                                                                !/*be        370*/
 1170 i3=-1                                                                     !/*be        371*/
 1180 call rou17 (fripn(1),pnmi(1),fmxsp(1),pcfsl(1),pnfsl(1))                  !/*be        372*/
!     fripn(117),pnmi(101),fmxsp(117),pcfsl(234),pnfsl(234)                     !/*be        373*/
      if (i3) 1190,1290,1200                                                    !/*be        374*/
 1190 ne=10                                                                     !/*be        375*/
!     coll(com lt -5.0d-6)  ecpl(error in curr ,strkp,pt(26),                   !/*be        376*/
!     pt(2),pt(14) or pt(37))  isw10=0                                          !/*be        377*/
      go to 1960                                                                !/*be        378*/
 1200 k=3                                                                       !/*be        379*/
 1210 if (pt(k-1).ne.dp1) go to 1280                                            !/*be        380*/
      if (pt(k).gt.max(efrp,dp0)) go to 1230                                    !/*be        381*/
      fcp=fcp+dp1                                                               !/*be        382*/
!     no. forbidden collisions involving protons                                !/*be        383*/
 1220 pm(4)=dncms                                                               !/*be        384*/
      iv=0                                                                      !/*be        385*/
      go to 830                                                                 !/*be        386*/
 1230 m=pt(k-1)                                                                 !/*be        387*/
      if (pt(k).gt.eco(m)) go to 1240                                           !/*be        388*/
      pt(k)=dp0                                                                 !/*be        389*/
      pnbc(m)=pnbc(m)+dp1                                                       !/*be        390*/
 1240 if (col(15)-dp1) 1290,1330,1250                                           !/*be        391*/
 1250 if (col(15)-dp3) 1310,1300,1260                                           !/*be        392*/
 1260 if (col(15)-dp5) 1320,1370,1270                                           !/*be        393*/
 1270 ne=11                                                                     !/*be        394*/
!     col(15) gt 5                                                              !/*be        395*/
      go to 1960                                                                !/*be        396*/
 1280 if (pt(k-1).ne.dp2) go to 1400                                            !/*be        397*/
      if (pt(k).gt.max(efrn,dp0)) go to 1230                                    !/*be        398*/
      fcn=fcn+dp1                                                               !/*be        399*/
      go to 1220                                                                !/*be        400*/
 1290 call collm (0)                                                            !/*be        401*/
      if (pt(38).eq.dp0) then                                                   !/*be        402*/
        i3=1                                                                    !/*be        403*/
        go to 1340                                                              !/*be        404*/
      endif                                                                     !/*be        405*/
      i3=2                                                                      !/*be        406*/
      go to 1340                                                                !/*be        407*/
 1300 i3=4                                                                      !/*be        408*/
      go to 1340                                                                !/*be        409*/
 1310 i3=5                                                                      !/*be        410*/
      go to 1340                                                                !/*be        411*/
 1320 i3=6                                                                      !/*be        412*/
      go to 1340                                                                !/*be        413*/
 1330 i3=3                                                                      !/*be        414*/
 1340 call rou18                                                                !/*be        415*/
      i3=i3                                                                     !/*be        416*/
      k=iv                                                                      !/*be        417*/
      go to (1210,1350,1240,1480,1360), i3                                      !/*be        418*/
 1350 ne=12                                                                     !/*be        419*/
!     pt(k-1) lte 2                                                             !/*be        420*/
      go to 1960                                                                !/*be        421*/
 1360 i18=i18+1                                                                 !/*be        422*/
      go to 1220                                                                !/*be        423*/
 1370 call rou19                                                                !/*be        424*/
      if (i3) 1380,1480,1390                                                    !/*be        425*/
 1380 ne=13                                                                     !/*be        426*/
!     pt(k-1)lt3,gt6--k lt 27                                                   !/*be        427*/
      go to 1960                                                                !/*be        428*/
 1390 i19=i19+1                                                                 !/*be        429*/
      go to 1220                                                                !/*be        430*/
 1400 if (col(15).lt.dp1) go to 1290                                            !/*be        431*/
      ne=14                                                                     !/*be        432*/
!     col(15) gte 1.d0                                                          !/*be        433*/
      go to 1960                                                                !/*be        434*/
 1410 i3=2                                                                      !/*be        435*/
      go to 1490                                                                !/*be        436*/
 1420 i3=3                                                                      !/*be        437*/
      go to 1490                                                                !/*be        438*/
 1430 i3=4                                                                      !/*be        439*/
      go to 1490                                                                !/*be        440*/
 1440 i3=5                                                                      !/*be        441*/
      go to 1490                                                                !/*be        442*/
 1450 i3=6                                                                      !/*be        443*/
      go to 1490                                                                !/*be        444*/
 1460 i3=7                                                                      !/*be        445*/
      go to 1490                                                                !/*be        446*/
 1470 i3=8                                                                      !/*be        447*/
      go to 1490                                                                !/*be        448*/
 1480 i3=1                                                                      !/*be        449*/
 1490 call rou20 (dcin(1),dcln(1),dchn(1),pdci(1),pdch(1))                      !/*be        450*/
!     dcin(115),dcln(80),dchn(64),pdci(52),pdch(64)                             !/*be        451*/
      i3=i3                                                                     !/*be        452*/
      go to (1500,870,1170,1120,920,780,1130,790), i3                           !/*be        453*/
 1500 ne=15                                                                     !/*be        454*/
!     pinst--med-  rlke gt 3500                                                 !/*be        455*/
      go to 1960                                                                !/*be        456*/
 1510 i3=2                                                                      !/*be        457*/
      go to 1550                                                                !/*be        458*/
 1520 i3=3                                                                      !/*be        459*/
      go to 1550                                                                !/*be        460*/
 1530 i3=4                                                                      !/*be        461*/
      go to 1550                                                                !/*be        462*/
 1540 i3=1                                                                      !/*be        463*/
 1550 call rou21 (frinn(1),dmin(1),fmxsn(1),fmxdn(1),fsln(1))                   !/*be        464*/
!     frinn(161),dmin(101),fmxsn(161),fmxdn(130),fsln(176)                      !/*be        465*/
      go to 1180                                                                !/*be        466*/
 1560 iv=2                                                                      !/*be        467*/
      go to 1790                                                                !/*be        468*/
 1570 iv=3                                                                      !/*be        469*/
      go to 1790                                                                !/*be        470*/
 1580 iv=4                                                                      !/*be        471*/
      go to 1790                                                                !/*be        472*/
 1590 iv=5                                                                      !/*be        473*/
      go to 1790                                                                !/*be        474*/
 1600 iv=6                                                                      !/*be        475*/
      go to 1790                                                                !/*be        476*/
 1610 iv=7                                                                      !/*be        477*/
      go to 1790                                                                !/*be        478*/
 1620 iv=8                                                                      !/*be        479*/
      go to 1790                                                                !/*be        480*/
 1630 iv=9                                                                      !/*be        481*/
      go to 1790                                                                !/*be        482*/
 1640 iv=10                                                                     !/*be        483*/
      go to 1790                                                                !/*be        484*/
 1650 iv=11                                                                     !/*be        485*/
      go to 1790                                                                !/*be        486*/
 1660 iv=12                                                                     !/*be        487*/
      go to 1790                                                                !/*be        488*/
 1670 iv=13                                                                     !/*be        489*/
      go to 1790                                                                !/*be        490*/
 1680 iv=14                                                                     !/*be        491*/
      go to 1790                                                                !/*be        492*/
 1690 iv=15                                                                     !/*be        493*/
      go to 1790                                                                !/*be        494*/
 1700 iv=16                                                                     !/*be        495*/
      go to 1790                                                                !/*be        496*/
 1710 iv=17                                                                     !/*be        497*/
      go to 1790                                                                !/*be        498*/
 1720 iv=18                                                                     !/*be        499*/
      go to 1790                                                                !/*be        500*/
 1730 iv=19                                                                     !/*be        501*/
      go to 1790                                                                !/*be        502*/
 1740 iv=20                                                                     !/*be        503*/
      go to 1790                                                                !/*be        504*/
 1750 iv=21                                                                     !/*be        505*/
      go to 1790                                                                !/*be        506*/
 1760 iv=22                                                                     !/*be        507*/
      go to 1790                                                                !/*be        508*/
 1770 iv=23                                                                     !/*be        509*/
      go to 1790                                                                !/*be        510*/
 1780 iv=1                                                                      !/*be        511*/
 1790 call rou22 (ppac(1),poac(1),pnec(1),pmxc(1),pnnec(1))                     !/*be        512*/
!     ppac(19),poac(19),pnec(126),pmxc(126),pnnec(126)                          !/*be        513*/
      iv=iv                                                                     !/*be        514*/
      if (i1.lt.0) go to 2110                                                   !/*be        515*/
      go to (1840,1800,640,860,2160,470,460,440,2440,2210,2190,2380,590,        !/*be        516*/
     & 160,710,370,430,420,2170,130,1830,2220,1810), iv                         !/*be        517*/
 1800 ne=16                                                                     !/*be        518*/
!     com gt 3500, ccpes, esps(1) gte 30.0, com gt 2500                         !/*be        519*/
      go to 1960                                                                !/*be        520*/
 1810 ne=21                                                                     !/*be        521*/
!     iv gt 22                                                                  !/*be        522*/
      go to 1960                                                                !/*be        523*/
 1820 aabs=dp1                                                                  !/*be        524*/
      value1=rang()                                                             !/*be        525*/
      if (value1.ge.ppnda) go to 380                                            !/*be        526*/
!     prob. pin-deut abs                                                        !/*be        527*/
      it=27                                                                     !/*be        528*/
!     bg117(20040) pi0 abs                                                      !/*be        529*/
      med=med                                                                   !/*be        530*/
      absec=-hvp(med)                                                           !/*be        531*/
      i3=1                                                                      !/*be        532*/
      go to 340                                                                 !/*be        533*/
 1830 if (not.lt.6) go to 1770                                                  !/*be        534*/
      if (rlke.gt.2500.d0) then                                                 !/*be        535*/
        ne=25                                                                   !/*be        536*/
!        rlke gt 2500                                                           !/*be        537*/
        go to 1960                                                              !/*be        538*/
      endif                                                                     !/*be        539*/
      if (rlke.le.180.d0) then                                                  !/*be        540*/
        if (clsm-dp2) 2500,2470,2240                                            !/*be        541*/
      endif                                                                     !/*be        542*/
      value1=rlke-180.d0                                                        !/*be        543*/
      if (not.le.6) then                                                        !/*be        544*/
        call crjab (1,pnscl(1))                                                 !/*be        545*/
!        pnscl(117)                                                             !/*be        546*/
!        (pin-p)single production crs. low en.                                  !/*be        547*/
      else                                                                      !/*be        548*/
        call crjab (1,pnnsl(1))                                                 !/*be        549*/
!        pnnsl(117)                                                             !/*be        550*/
!        (pin-n)single prod. crs. low en.                                       !/*be        551*/
      endif                                                                     !/*be        552*/
      go to 2220                                                                !/*be        553*/
 1840 isw(11)=0                                                                 !/*be        554*/
      go to 1710                                                                !/*be        555*/
 1850 pt(14)=dp1                                                                !/*be        556*/
      go to 1040                                                                !/*be        557*/
 1860 pt(2)=dp3                                                                 !/*be        558*/
      pt(14)=dp2                                                                !/*be        559*/
      ik=it                                                                     !/*be        560*/
      go to 750                                                                 !/*be        561*/
 1870 pt(14)=dp2                                                                !/*be        562*/
      ik=23                                                                     !/*be        563*/
      go to 1050                                                                !/*be        564*/
 1880 pt(2)=dp5                                                                 !/*be        565*/
      i3=4                                                                      !/*be        566*/
      go to 760                                                                 !/*be        567*/
 1890 isw(9)=2                                                                  !/*be        568*/
      go to 1470                                                                !/*be        569*/
 1900 itote=ipec(2)+ipec(7)+ipec(11)                                            !/*be        570*/
      if (itote-1) 1940,1920,1910                                               !/*be        571*/
 1910 call errorf ('bert.2')                                                    !/*be        572*/
 1920 nopart=-1                                                                 !/*be        573*/
 1930 continue                                                                  !/*be        574*/
      return                                                                    !/*be        575*/
 1940 nopart=esps(1)                                                            !/*be        576*/
      if (nopart.gt.l05) then                                                   !/*be        577*/
        write (io,2550) nopart                                                  !/*be        578*/
        nopart=l05                                                              !/*be        579*/
      endif                                                                     !/*be        580*/
      do 1950 ndex=1,nopart                                                     !/*be        581*/
      klmn=8*(ndex-1)+1                                                         !/*be        582*/
      kind(ndex)=esps(klmn+1)-dp1                                               !/*be        583*/
      eray(ndex)=esps(klmn+2)                                                   !/*be        584*/
      aray(ndex)=esps(klmn+3)                                                   !/*be        585*/
      bray(ndex)=esps(klmn+4)                                                   !/*be        586*/
 1950 gray(ndex)=esps(klmn+5)                                                   !/*be        587*/
      go to 1930                                                                !/*be        588*/
 1960 write (io,2560) ne                                                        !/*be        589*/
      call errorf ('bert.3')                                                    !/*be        590*/
 1970 value2=einc+space(12)                                                     !/*be        591*/
      if (value1.gt.160.d0) go to 1990                                          !/*be        592*/
      space(33)=1.4d-24                                                         !/*be        593*/
!     no production possible                                                    !/*be        594*/
      fmax(2)=1.4d-24                                                           !/*be        595*/
      space(34)=0.46d-24                                                        !/*be        596*/
      fmax(1)=0.46d-24                                                          !/*be        597*/
      do 1980 i=9,12                                                            !/*be        598*/
 1980 s(i)=dp0                                                                  !/*be        599*/
!     einc+50.d0 is less than 160.d0                                            !/*be        600*/
      go to 2090                                                                !/*be        601*/
 1990 call bovera (value2,dncms,ans)                                            !/*be        602*/
!     nucleon mass=constant  ans=p1/e1                                          !/*be        603*/
      if (value1.gt.560.d0) go to 2040                                          !/*be        604*/
      s(11)=dp0                                                                 !/*be        605*/
      s(12)=dp0                                                                 !/*be        606*/
!     single production possible--s(11),s(12) double prod.                      !/*be        607*/
      if (value1.lt.400.d0) go to 2000                                          !/*be        608*/
      s(9)=22.6d-27*ans                                                         !/*be        609*/
      s(10)=14.0d-27*ans                                                        !/*be        610*/
      space(44)=56.0d-27                                                        !/*be        611*/
      space(45)=27.0d-27*ans                                                    !/*be        612*/
      go to 2030                                                                !/*be        613*/
 2000 if (value1.lt.300.d0) go to 2010                                          !/*be        614*/
      s(9)=20.0d-27*ans                                                         !/*be        615*/
      s(10)=14.0d-27*ans                                                        !/*be        616*/
      space(44)=0.106d-24                                                       !/*be        617*/
      space(45)=36.0d-27*ans                                                    !/*be        618*/
      go to 2030                                                                !/*be        619*/
 2010 if (value1.lt.200.d0) go to 2020                                          !/*be        620*/
      s(9)=11.4d-27*ans                                                         !/*be        621*/
      s(10)=11.2d-27*ans                                                        !/*be        622*/
      space(44)=0.313d-24                                                       !/*be        623*/
      space(45)=0.103d-24                                                       !/*be        624*/
      go to 2030                                                                !/*be        625*/
 2020 s(9)=1.95d-27*ans                                                         !/*be        626*/
      s(10)=1.7d-27*ans                                                         !/*be        627*/
      space(44)=0.52d-24                                                        !/*be        628*/
      space(45)=0.176d-24                                                       !/*be        629*/
 2030 space(33)=space(44)                                                       !/*be        630*/
      space(34)=space(45)                                                       !/*be        631*/
      go to 2090                                                                !/*be        632*/
 2040 if (value1.le.3600.d0) go to 2050                                         !/*be        633*/
      ne=17                                                                     !/*be        634*/
!     value1 is greater than 3600                                               !/*be        635*/
      go to 1960                                                                !/*be        636*/
 2050 s(9)=22.6d-27*ans                                                         !/*be        637*/
!     double production possible                                                !/*be        638*/
      s(10)=14.0d-27*ans                                                        !/*be        639*/
      if (value1.ge.800.d0) go to 2060                                          !/*be        640*/
      s(11)=1.9d-27*ans                                                         !/*be        641*/
      s(12)=9.0d-27*ans                                                         !/*be        642*/
      space(46)=38.4d-27*ans                                                    !/*be        643*/
      space(47)=27.2d-27*ans                                                    !/*be        644*/
      go to 2080                                                                !/*be        645*/
 2060 if (value1.ge.1680.d0) go to 2070                                         !/*be        646*/
      s(11)=10.8d-27*ans                                                        !/*be        647*/
      s(12)=17.4d-27*ans                                                        !/*be        648*/
      space(46)=33.0d-27*ans                                                    !/*be        649*/
      space(47)=27.2d-27*ans                                                    !/*be        650*/
      go to 2080                                                                !/*be        651*/
 2070 space(46)=25.0d-27*ans                                                    !/*be        652*/
      space(47)=26.5d-27*ans                                                    !/*be        653*/
      s(10)=13.6d-27*ans                                                        !/*be        654*/
      s(11)=18.0d-27*ans                                                        !/*be        655*/
      s(12)=23.6d-27*ans                                                        !/*be        656*/
 2080 space(33)=space(46)                                                       !/*be        657*/
      space(34)=space(47)                                                       !/*be        658*/
 2090 if (no.ge.2) then                                                         !/*be        659*/
        iv=-1                                                                   !/*be        660*/
      else                                                                      !/*be        661*/
        iv=1                                                                    !/*be        662*/
      endif                                                                     !/*be        663*/
      call xyi (9,33,41)                                                        !/*be        664*/
      ip=2                                                                      !/*be        665*/
 2100 if (no.ge.2) then                                                         !/*be        666*/
        isw(4)=0                                                                !/*be        667*/
      else                                                                      !/*be        668*/
        isw(4)=1                                                                !/*be        669*/
      endif                                                                     !/*be        670*/
      call undis                                                                !/*be        671*/
      if (begru.eq.dp0) go to 1900                                              !/*be        672*/
      aabs=dp0                                                                  !/*be        673*/
!     xinc=xi(1)                                                                !/*be        674*/
!     xinc=x-coordinate inc.part.                                               !/*be        675*/
      inc=1                                                                     !/*be        676*/
!     0 if particle cascade                                                     !/*be        677*/
      curr(1)=no                                                                !/*be        678*/
      curr(3)=dncms                                                             !/*be        679*/
!     nucleon mass/cm                                                           !/*be        680*/
      call geo                                                                  !/*be        681*/
      if (i1.ge.0) go to 2120                                                   !/*be        682*/
 2110 ne=20                                                                     !/*be        683*/
!     error in geom                                                             !/*be        684*/
      go to 1960                                                                !/*be        685*/
 2120 call partin                                                               !/*be        686*/
      call spac32 (43)                                                          !/*be        687*/
 2130 if (ex.gt.d(2)) go to 2280                                                !/*be        688*/
 2140 wkrpn(3)=out(13)                                                          !/*be        689*/
      wkrpn(6)=out(16)                                                          !/*be        690*/
      curr(2)=wkrpn(6)                                                          !/*be        691*/
      if (isw(4).ne.0) then                                                     !/*be        692*/
        curr(2)=wkrpn(3)                                                        !/*be        693*/
!        k.e.with respect to neutrons(protons), rg.3                            !/*be        694*/
      endif                                                                     !/*be        695*/
      call bg6ca (3,0)                                                          !/*be        696*/
      ifca=3                                                                    !/*be        697*/
 2150 ifcc=3                                                                    !/*be        698*/
 2160 ka=6                                                                      !/*be        699*/
 2170 call abran (ka)                                                           !/*be        700*/
      knot=not+6                                                                !/*be        701*/
      if (in1.gt.0) knot=knot+6                                                 !/*be        702*/
      if (knot.eq.17) go to 1820                                                !/*be        703*/
      call bg6c (isw(4))                                                        !/*be        704*/
      if (rlke.le.dp0) then                                                     !/*be        705*/
        ne=24                                                                   !/*be        706*/
!        rlke lte 0                                                             !/*be        707*/
        go to 1960                                                              !/*be        708*/
      endif                                                                     !/*be        709*/
      value1=rlke                                                               !/*be        710*/
      if (in1.ne.0) go to 1720                                                  !/*be        711*/
      if (not.ge.5) go to 2440                                                  !/*be        712*/
      if (not-2) 2180,2200,2380                                                 !/*be        713*/
 2180 any=space(33)                                                             !/*be        714*/
 2190 call crjab (1,ecn(1))                                                     !/*be        715*/
!     ecn(176)                                                                  !/*be        716*/
!     (n-p)elastic crs. scattering                                              !/*be        717*/
      go to 2220                                                                !/*be        718*/
 2200 any=space(34)                                                             !/*be        719*/
 2210 call crjab (1,pec(1))                                                     !/*be        720*/
!     pec(176)                                                                  !/*be        721*/
!     (p-p)elastic scat. crs.                                                   !/*be        722*/
 2220 if (clsm-dp2) 2480,2460,2230                                              !/*be        723*/
 2230 if (value1.le.value2) go to 210                                           !/*be        724*/
 2240 call signex                                                               !/*be        725*/
      if (isw(1).ne.0) go to 2250                                               !/*be        726*/
      if (in1.eq.0) go to 2130                                                  !/*be        727*/
      if (curr(1)-dp2) 1780,1780,1730                                           !/*be        728*/
 2250 if (in1.ne.0) go to 1590                                                  !/*be        729*/
      if (ex.le.d(6)) go to 2140                                                !/*be        730*/
      if (isw(2).ne.0) go to 2270                                               !/*be        731*/
 2260 ipec(7)=ipec(7)+1                                                         !/*be        732*/
!     no. of escaped particles on rg.2                                          !/*be        733*/
      go to 2100                                                                !/*be        734*/
 2270 ipec(11)=ipec(11)+1                                                       !/*be        735*/
      go to 2100                                                                !/*be        736*/
!     no. of particles escaped on rg.1                                          !/*be        737*/
 2280 if (d(3).eq.dp0) then                                                     !/*be        738*/
        ipec(2)=ipec(2)+1                                                       !/*be        739*/
        go to 2100                                                              !/*be        740*/
      endif                                                                     !/*be        741*/
!     no. of particles incident on rg.3 escaping                                !/*be        742*/
      isw(1)=1                                                                  !/*be        743*/
      call spac32 (42)                                                          !/*be        744*/
 2290 if (ex.gt.d(3)) go to 2310                                                !/*be        745*/
 2300 wkrpn(2)=out(14)                                                          !/*be        746*/
      wkrpn(5)=out(17)                                                          !/*be        747*/
      curr(2)=wkrpn(5)                                                          !/*be        748*/
      if (isw(4).ne.0) then                                                     !/*be        749*/
        curr(2)=wkrpn(2)                                                        !/*be        750*/
!        k.e.with respect to neutrons(protons) rg.2                             !/*be        751*/
      endif                                                                     !/*be        752*/
      call bg6ca (2,0)                                                          !/*be        753*/
      go to 2150                                                                !/*be        754*/
 2310 if (d(4).ne.dp0) go to 2330                                               !/*be        755*/
      call spac32 (43)                                                          !/*be        756*/
 2320 if (ex-d(6)) 2140,2140,2260                                               !/*be        757*/
 2330 isw(2)=1                                                                  !/*be        758*/
      isw(3)=1                                                                  !/*be        759*/
      call spac32 (41)                                                          !/*be        760*/
 2340 if (ex.le.d(4)) go to 2370                                                !/*be        761*/
      call spac32 (42)                                                          !/*be        762*/
 2350 if (ex.le.d(5)) go to 2300                                                !/*be        763*/
      call spac32 (43)                                                          !/*be        764*/
 2360 if (ex-d(6)) 2140,2140,2270                                               !/*be        765*/
 2370 wkrpn(1)=out(15)                                                          !/*be        766*/
      wkrpn(4)=out(18)                                                          !/*be        767*/
      curr(2)=wkrpn(4)                                                          !/*be        768*/
      if (isw(4).ne.0) curr(2)=wkrpn(1)                                         !/*be        769*/
!     k.e. with respect to neutrons(protons) rg.1                               !/*be        770*/
      call bg6ca (1,0)                                                          !/*be        771*/
      go to 2150                                                                !/*be        772*/
 2380 if (rlke.le.3500.d0) go to 2400                                           !/*be        773*/
 2390 ne=23                                                                     !/*be        774*/
!     rlke gt 3500                                                              !/*be        775*/
      go to 1960                                                                !/*be        776*/
 2400 if (rlke.le.360.d0) go to 2450                                            !/*be        777*/
      value1=rlke-360.d0                                                        !/*be        778*/
      if (in1.eq.0) any=s(knot)                                                 !/*be        779*/
      if (not-4) 2420,2430,2410                                                 !/*be        780*/
 2410 ne=18                                                                     !/*be        781*/
!     not=5                                                                     !/*be        782*/
      go to 1960                                                                !/*be        783*/
 2420 call crjab (1,pspcl(1))                                                   !/*be        784*/
!     pspcl(158)                                                                !/*be        785*/
!     (p-p) sing. prod. crs. low energy                                         !/*be        786*/
      go to 2220                                                                !/*be        787*/
 2430 call crjab (1,spcln(1))                                                   !/*be        788*/
!     spcln(158)                                                                !/*be        789*/
!     (n-p) single prod. crs. low energy                                        !/*be        790*/
      go to 2220                                                                !/*be        791*/
 2440 if (rlke.gt.3500.d0) go to 2390                                           !/*be        792*/
      if (rlke.le.920.d0) go to 2450                                            !/*be        793*/
      value1=rlke-920.d0                                                        !/*be        794*/
      if (not.gt.6) then                                                        !/*be        795*/
        ne=19                                                                   !/*be        796*/
!        not gt 6                                                               !/*be        797*/
        go to 1960                                                              !/*be        798*/
      elseif (not.lt.6) then                                                    !/*be        799*/
        if (in1.eq.0) any=s(11)                                                 !/*be        800*/
        call crjab (1,pdpcl(1))                                                 !/*be        801*/
!        pdpcl(130)                                                             !/*be        802*/
!        (p-p) double production crs. low energy                                !/*be        803*/
      else                                                                      !/*be        804*/
        if (in1.eq.0) any=s(12)                                                 !/*be        805*/
        call crjab (1,dpcln(1))                                                 !/*be        806*/
!        dpcln(130)                                                             !/*be        807*/
!        (n-p) double production crs. low energy                                !/*be        808*/
      endif                                                                     !/*be        809*/
      go to 2220                                                                !/*be        810*/
 2450 if (clsm-dp2) 2500,2470,2240                                              !/*be        811*/
 2460 if (value1.le.value2) go to 600                                           !/*be        812*/
 2470 call signex                                                               !/*be        813*/
      if (in1.ne.0) go to 1740                                                  !/*be        814*/
      if (isw(3)) 2350,2290,2350                                                !/*be        815*/
 2480 if (value1.gt.value2) go to 2500                                          !/*be        816*/
 2490 ifc=9+ifcc                                                                !/*be        817*/
      if (in1) 1640,260,1640                                                    !/*be        818*/
 2500 call signex                                                               !/*be        819*/
      if (in1) 1750,2340,1750                                                   !/*be        820*/
 2510 if (esps(1).eq.dp0) then                                                  !/*be        821*/
        nwds=1                                                                  !/*be        822*/
        go to 2540                                                              !/*be        823*/
      endif                                                                     !/*be        824*/
      nwds=esps(1)*8.d0+1.5d0                                                   !/*be        825*/
!     total no. of words(escaping particles)                                    !/*be        826*/
      if (count(6).ge.dp0) go to 2540                                           !/*be        827*/
! minus,record not representative,skip                                          !/*be        828*/
      do 2520 i=1,nwds                                                          !/*be        829*/
 2520 esps(i)=dp0                                                               !/*be        830*/
      do 2530 i=1,5                                                             !/*be        831*/
 2530 count(i)=dp0                                                              !/*be        832*/
 2540 nor=nor+1                                                                 !/*be        833*/
      in1=0                                                                     !/*be        834*/
      if (ip-1) 100,100,2100                                                    !/*be        835*/
!                                                                               !/*be        836*/
 2550 format ('0nopart has exceeded the maximum permitted in ',                 !/*be        837*/
     & 'subroutine BERT. NOPART will be set to ',i3//)                          !/*be        838*/
 2560 format (1h1,5hne = ,i5)                                                   !/*be        839*/
      end                                                                       !/*be        840*/

!     $Id: rou22.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou22 (v,w,x,y,z)                                              !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension v(19), w(19), x(126), y(126), z(126)                            !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      i5=int(curr(1))                                                           !/*ro          6*/
      go to (260,990,330,340,360,970,510,1030,1090,450,1180,430,390,490,        !/*ro          7*/
     & 1330,10,680,580,250,520,420,1130,1450), iv                               !/*ro          8*/
   10 do 20 i=1,3                                                               !/*ro          9*/
      xi(i)=curr(i+3)                                                           !/*ro         10*/
   20 dcos(i)=curr(i+6)                                                         !/*ro         11*/
      in1=-1                                                                    !/*ro         12*/
      med=int(curr(10))                                                         !/*ro         13*/
      i5=int(curr(1))                                                           !/*ro         14*/
      call geo                                                                  !/*ro         15*/
      if (i1) 60,30,30                                                          !/*ro         16*/
   30 if (curr(1)-dp2) 70,660,40                                                !/*ro         17*/
   40 if (curr(1)-dp4) 670,680,50                                               !/*ro         18*/
   50 iv=1                                                                      !/*ro         19*/
   60 return                                                                    !/*ro         20*/
   70 isw(4)=1                                                                  !/*ro         21*/
!     proton                                                                    !/*ro         22*/
   80 aabs=dp0                                                                  !/*ro         23*/
      i4=1                                                                      !/*ro         24*/
      i2=med                                                                    !/*ro         25*/
      if (i2-1) 110,90,210                                                      !/*ro         26*/
   90 i4=6                                                                      !/*ro         27*/
      go to 210                                                                 !/*ro         28*/
  100 isw(6)=1                                                                  !/*ro         29*/
      isw(5)=1                                                                  !/*ro         30*/
      i3=0                                                                      !/*ro         31*/
      if (com-36.d+02) 120,120,110                                              !/*ro         32*/
  110 iv=2                                                                      !/*ro         33*/
      go to 60                                                                  !/*ro         34*/
!     com=greatest energy inside nucleus for nucleons only                      !/*ro         35*/
  120 if (com-56.d1) 130,130,140                                                !/*ro         36*/
  130 if (com-16.d1) 200,200,190                                                !/*ro         37*/
  140 call dfmax                                                                !/*ro         38*/
!     dfmax fills out fmax(1-6)                                                 !/*ro         39*/
      i1=6                                                                      !/*ro         40*/
  150 if (curr(1)-dp2) 170,160,160                                              !/*ro         41*/
  160 i3=1                                                                      !/*ro         42*/
  170 call store                                                                !/*ro         43*/
      ex=dp0                                                                    !/*ro         44*/
      call signex                                                               !/*ro         45*/
      go to (250,360,420,470,390,570,850,970,1090,180,1330), i4                 !/*ro         46*/
  180 iv=3                                                                      !/*ro         47*/
      go to 60                                                                  !/*ro         48*/
  190 isw(6)=0                                                                  !/*ro         49*/
      call pfmax                                                                !/*ro         50*/
!     pfmax fills out fmax(1-4)                                                 !/*ro         51*/
      i1=4                                                                      !/*ro         52*/
      go to 150                                                                 !/*ro         53*/
  200 isw(5)=0                                                                  !/*ro         54*/
      isw(6)=0                                                                  !/*ro         55*/
      call nn                                                                   !/*ro         56*/
!     nn fills out fmax(1-2),fmax(3-6)=0                                        !/*ro         57*/
      i1=2                                                                      !/*ro         58*/
      go to 150                                                                 !/*ro         59*/
  210 isw(1)=0                                                                  !/*ro         60*/
      isw(2)=0                                                                  !/*ro         61*/
      isw(3)=0                                                                  !/*ro         62*/
      m=med+int(15.d0-6.d0*curr(1))                                             !/*ro         63*/
      a=curr(2)-space(m)                                                        !/*ro         64*/
  220 do 230 i=1,3                                                              !/*ro         65*/
      wkrpn(i)=a+space(i+9)                                                     !/*ro         66*/
  230 wkrpn(i+3)=a+space(i+3)                                                   !/*ro         67*/
  240 m=4-3*isw(4)                                                              !/*ro         68*/
      com=wkrpn(m)                                                              !/*ro         69*/
      go to 100                                                                 !/*ro         70*/
  250 if (i2.lt.3) go to 1200                                                   !/*ro         71*/
  260 if (ex-d(2)) 300,300,270                                                  !/*ro         72*/
!     entry point for cascade charged and neutral pions after crjab             !/*ro         73*/
!     rejection,prod.reaction lt 180 fermi rejection for non-absorption         !/*ro         74*/
!     reaction also all cascade nuclear rejections including crjab or           !/*ro         75*/
!     rlke lt prod.threshold                                                    !/*ro         76*/
  270 if (d(3)) 550,280,550                                                     !/*ro         77*/
  280 call ccpes                                                                !/*ro         78*/
      if (i1) 290,290,110                                                       !/*ro         79*/
!     go to punp                                                                !/*ro         80*/
  290 iv=4                                                                      !/*ro         81*/
      go to 60                                                                  !/*ro         82*/
  300 if (in1) 310,310,1350                                                     !/*ro         83*/
  310 call bg6ca (3,0)                                                          !/*ro         84*/
      ifcc=12                                                                   !/*ro         85*/
  320 med=clsm                                                                  !/*ro         86*/
      iv=5                                                                      !/*ro         87*/
      go to 60                                                                  !/*ro         88*/
  330 value1=ex                                                                 !/*ro         89*/
      iv=6                                                                      !/*ro         90*/
      go to 60                                                                  !/*ro         91*/
  340 value1=ex+d(3)                                                            !/*ro         92*/
  350 iv=7                                                                      !/*ro         93*/
      go to 60                                                                  !/*ro         94*/
!     e.p.for rejection following crjab or rlke lt 180 in prod.reactions        !/*ro         95*/
!     and for fermi rejection in scattering and production reactions            !/*ro         96*/
!     for cascade charged and neutral pions                                     !/*ro         97*/
  360 if (ex-d(6)) 300,300,280                                                  !/*ro         98*/
  370 isw(1)=1                                                                  !/*ro         99*/
  380 if (i5.ge.4) go to 970                                                    !/*ro        100*/
!     e.p.for all rejections,crjab,production,and fermi for cascade             !/*ro        101*/
!     nucleons                                                                  !/*ro        102*/
  390 if (ex-d(3)) 500,500,400                                                  !/*ro        103*/
  400 i2=3                                                                      !/*ro        104*/
      i4=2                                                                      !/*ro        105*/
      i3=1                                                                      !/*ro        106*/
      i1=2                                                                      !/*ro        107*/
      if (d(4)) 410,240,410                                                     !/*ro        108*/
  410 isw(2)=1                                                                  !/*ro        109*/
      isw(3)=1                                                                  !/*ro        110*/
      i4=3                                                                      !/*ro        111*/
      i2=1                                                                      !/*ro        112*/
      go to 240                                                                 !/*ro        113*/
  420 if (i5.ge.4) go to 1090                                                   !/*ro        114*/
  430 if (ex-d(4)) 440,440,460                                                  !/*ro        115*/
  440 call bg6ca (1,0)                                                          !/*ro        116*/
      ifcc=7                                                                    !/*ro        117*/
      go to 320                                                                 !/*ro        118*/
  450 value1=ex                                                                 !/*ro        119*/
      iv=8                                                                      !/*ro        120*/
      go to 60                                                                  !/*ro        121*/
  460 i4=4                                                                      !/*ro        122*/
      i1=2                                                                      !/*ro        123*/
      i2=2                                                                      !/*ro        124*/
      i3=1                                                                      !/*ro        125*/
      go to 240                                                                 !/*ro        126*/
!     e.p.for cascade nucleons and pi0,after crjab and prod.threshold           !/*ro        127*/
!     rejections                                                                !/*ro        128*/
  470 if (i5-2) 490,490,480                                                     !/*ro        129*/
  480 if (i5-4) 110,1330,110                                                    !/*ro        130*/
!     e.p.for cascade nucleons after all fermi rejections                       !/*ro        131*/
  490 if (ex-d(5)) 500,500,530                                                  !/*ro        132*/
  500 call bg6ca (2,0)                                                          !/*ro        133*/
      ifcc=10                                                                   !/*ro        134*/
      go to 320                                                                 !/*ro        135*/
  510 value1=ex                                                                 !/*ro        136*/
      go to 350                                                                 !/*ro        137*/
  520 if (isw(3)) 470,380,470                                                   !/*ro        138*/
  530 i4=2                                                                      !/*ro        139*/
      i2=3                                                                      !/*ro        140*/
  540 i3=1                                                                      !/*ro        141*/
      i1=2                                                                      !/*ro        142*/
      go to 240                                                                 !/*ro        143*/
  550 isw(1)=1                                                                  !/*ro        144*/
      if (curr(1)-dp3) 560,870,1280                                             !/*ro        145*/
  560 i4=5                                                                      !/*ro        146*/
      i2=2                                                                      !/*ro        147*/
      go to 540                                                                 !/*ro        148*/
!     d.p.                                                                      !/*ro        149*/
  570 isw(1)=1                                                                  !/*ro        150*/
      isw(2)=1                                                                  !/*ro        151*/
      isw(3)=1                                                                  !/*ro        152*/
      go to 250                                                                 !/*ro        153*/
  580 any=fmax(not)                                                             !/*ro        154*/
      if (not-5) 610,590,600                                                    !/*ro        155*/
  590 iv=9                                                                      !/*ro        156*/
      go to 60                                                                  !/*ro        157*/
  600 if (i5-4) 590,620,590                                                     !/*ro        158*/
  610 if (knot-15) 620,1440,1440                                                !/*ro        159*/
  620 if (i5.ge.4) go to 1390                                                   !/*ro        160*/
      if (not-2) 630,640,650                                                    !/*ro        161*/
  630 iv=10                                                                     !/*ro        162*/
      go to 60                                                                  !/*ro        163*/
  640 iv=11                                                                     !/*ro        164*/
      go to 60                                                                  !/*ro        165*/
  650 iv=12                                                                     !/*ro        166*/
      go to 60                                                                  !/*ro        167*/
  660 isw(4)=0                                                                  !/*ro        168*/
!     neutron                                                                   !/*ro        169*/
      go to 80                                                                  !/*ro        170*/
  670 isw(11)=1                                                                 !/*ro        171*/
!     curr(1)=3=pi+ meson(11575)--pi 0=4(15100)--pi -(14646)                    !/*ro        172*/
  680 in1=1                                                                     !/*ro        173*/
      isw(1)=0                                                                  !/*ro        174*/
      isw(2)=0                                                                  !/*ro        175*/
      isw(3)=0                                                                  !/*ro        176*/
      isw(5)=1                                                                  !/*ro        177*/
      isw(6)=0                                                                  !/*ro        178*/
      isw(7)=0                                                                  !/*ro        179*/
      isw(8)=1                                                                  !/*ro        180*/
      i6=i5-2                                                                   !/*ro        181*/
      i2=med                                                                    !/*ro        182*/
      com=curr(2)-space(med+9)                                                  !/*ro        183*/
      if (med-2) 700,690,710                                                    !/*ro        184*/
!     e.p.for pi0 after fermi rejection                                         !/*ro        185*/
  690 isw(8)=0                                                                  !/*ro        186*/
  700 isw(7)=1                                                                  !/*ro        187*/
  710 do 720 i=1,3                                                              !/*ro        188*/
      wkrpn(i)=com+space(i+9)                                                   !/*ro        189*/
  720 wkrpn(i+3)=com+space(i+3)                                                 !/*ro        190*/
      com=com+space(4)                                                          !/*ro        191*/
      if (com-26.d+02) 730,730,110                                              !/*ro        192*/
  730 if (com-1.d+02) 740,740,750                                               !/*ro        193*/
  740 lg=4                                                                      !/*ro        194*/
      go to 1230                                                                !/*ro        195*/
  750 lg=6                                                                      !/*ro        196*/
      call spcn                                                                 !/*ro        197*/
      if (value1) 760,760,780                                                   !/*ro        198*/
  760 com=curr(2)-space(med+9)                                                  !/*ro        199*/
      if (com-36.d1) 770,780,780                                                !/*ro        200*/
  770 if (i6.eq.2) go to 1240                                                   !/*ro        201*/
      call crdet (1,v(1),com)                                                   !/*ro        202*/
      fmax(4)=crdt(1)                                                           !/*ro        203*/
  780 if (i6.eq.2) go to 1250                                                   !/*ro        204*/
      i1=6                                                                      !/*ro        205*/
  790 i4=7                                                                      !/*ro        206*/
      i2=1                                                                      !/*ro        207*/
      i3=0                                                                      !/*ro        208*/
      if (i6.eq.2) go to 800                                                    !/*ro        209*/
      if (isw(11)) 800,840,800                                                  !/*ro        210*/
!     pi+                                                                       !/*ro        211*/
  800 if (isw(7)) 810,830,810                                                   !/*ro        212*/
!     pi-                                                                       !/*ro        213*/
  810 if (isw(8)) 170,820,170                                                   !/*ro        214*/
  820 i2=2                                                                      !/*ro        215*/
      go to 170                                                                 !/*ro        216*/
  830 i2=3                                                                      !/*ro        217*/
      go to 170                                                                 !/*ro        218*/
  840 i3=1                                                                      !/*ro        219*/
      go to 800                                                                 !/*ro        220*/
  850 if (curr(1)-dp2) 860,860,250                                              !/*ro        221*/
  860 a=curr(2)-space(med+9)                                                    !/*ro        222*/
      go to 220                                                                 !/*ro        223*/
  870 i4=8                                                                      !/*ro        224*/
  880 i2=2                                                                      !/*ro        225*/
  890 i3=0                                                                      !/*ro        226*/
      i1=lg                                                                     !/*ro        227*/
      if (curr(1)-dp4) 900,960,900                                              !/*ro        228*/
  900 if (isw(11)) 920,910,920                                                  !/*ro        229*/
  910 i3=1                                                                      !/*ro        230*/
  920 if (curr(1)-dp3) 930,940,940                                              !/*ro        231*/
  930 iv=23                                                                     !/*ro        232*/
      go to 60                                                                  !/*ro        233*/
  940 if (lg-4) 930,170,950                                                     !/*ro        234*/
  950 m=5-abs(i5-4)                                                             !/*ro        235*/
      univer=fmax(m)                                                            !/*ro        236*/
      call spcn                                                                 !/*ro        237*/
      fmax(m)=univer                                                            !/*ro        238*/
      go to 170                                                                 !/*ro        239*/
  960 i1=i1+1                                                                   !/*ro        240*/
      go to 940                                                                 !/*ro        241*/
!     e.p.for charged and neutral pions after crjab rejection also,after        !/*ro        242*/
!     rlke.le.180 in prod.reactions and after fermi rejection in scatter        !/*ro        243*/
!     ing and prod.reactions                                                    !/*ro        244*/
  970 if (ex-d(3)) 980,980,1060                                                 !/*ro        245*/
  980 if (i6.eq.2) go to 1300                                                   !/*ro        246*/
      iv=13                                                                     !/*ro        247*/
      go to 60                                                                  !/*ro        248*/
  990 any=fmax(not)                                                             !/*ro        249*/
      if (curr(1)-dp3) 1000,1020,1020                                           !/*ro        250*/
 1000 ifc=12                                                                    !/*ro        251*/
 1010 iv=14                                                                     !/*ro        252*/
      go to 60                                                                  !/*ro        253*/
 1020 ifcc=(clsm-dp2)*((clsm*5.5d0)-8.5d0)+12.05d0                              !/*ro        254*/
      go to 1010                                                                !/*ro        255*/
 1030 if (i4-10) 1040,1120,1040                                                 !/*ro        256*/
!     e.p.for escape prior to choosing reactions--cascade charged pion          !/*ro        257*/
 1040 if (clsm-dp2) 1050,1120,1050                                              !/*ro        258*/
 1050 i4=2                                                                      !/*ro        259*/
      go to 880                                                                 !/*ro        260*/
 1060 i4=2                                                                      !/*ro        261*/
!     e.p.when cascade particle escapes from region 2                           !/*ro        262*/
      i2=3                                                                      !/*ro        263*/
      if (d(4)) 1080,1070,1080                                                  !/*ro        264*/
 1070 if (i6-2) 890,1340,890                                                    !/*ro        265*/
 1080 isw(2)=1                                                                  !/*ro        266*/
      isw(3)=1                                                                  !/*ro        267*/
      i4=9                                                                      !/*ro        268*/
      i2=1                                                                      !/*ro        269*/
      go to 1070                                                                !/*ro        270*/
!     e.p.after all rejections except fermi in abs.reactions for                !/*ro        271*/
!     charged and neutral cc pions--escape from region 1 prior to               !/*ro        272*/
!     choosing reaction for cc charged pion                                     !/*ro        273*/
 1090 if (ex-d(4)) 1100,1100,1110                                               !/*ro        274*/
 1100 if (i6.eq.2) go to 1360                                                   !/*ro        275*/
      iv=15                                                                     !/*ro        276*/
      go to 60                                                                  !/*ro        277*/
 1110 i4=10                                                                     !/*ro        278*/
      if (i6-2) 880,1320,880                                                    !/*ro        279*/
 1120 i4=2                                                                      !/*ro        280*/
      i2=3                                                                      !/*ro        281*/
      go to 890                                                                 !/*ro        282*/
 1130 if (in1) 1150,1140,1150                                                   !/*ro        283*/
 1140 iv=16                                                                     !/*ro        284*/
      go to 60                                                                  !/*ro        285*/
 1150 ifca=8*abs(i6-2)-11*(i6-1)*(i6-3)                                         !/*ro        286*/
      if (isw(1)) 1160,330,1160                                                 !/*ro        287*/
 1160 if (isw(2)) 1170,340,1170                                                 !/*ro        288*/
 1170 iv=17                                                                     !/*ro        289*/
      go to 60                                                                  !/*ro        290*/
 1180 ifca=10*abs(i6-2)+12*(i6-1)*(3-i6)                                        !/*ro        291*/
      if (isw(3)) 1190,510,1190                                                 !/*ro        292*/
 1190 iv=18                                                                     !/*ro        293*/
      go to 60                                                                  !/*ro        294*/
 1200 if (curr(1)-dp3) 1210,1220,1220                                           !/*ro        295*/
 1210 if (med-1) 420,420,370                                                    !/*ro        296*/
 1220 isw(1)=1                                                                  !/*ro        297*/
      if (med.ge.2) go to 970                                                   !/*ro        298*/
      isw(2)=1                                                                  !/*ro        299*/
      isw(3)=1                                                                  !/*ro        300*/
      go to 1090                                                                !/*ro        301*/
 1230 isw(5)=0                                                                  !/*ro        302*/
      if (i6.eq.2) go to 1380                                                   !/*ro        303*/
      call bovera (curr(2),pnms,ans)                                            !/*ro        304*/
      fmax(1)=.20d-24*ans                                                       !/*ro        305*/
!     (pi+p)scattering                                                          !/*ro        306*/
      fmax(2)=23.0d-27*ans                                                      !/*ro        307*/
!     (pim+p)scattering                                                         !/*ro        308*/
      fmax(3)=45.1d-27*ans                                                      !/*ro        309*/
!     (pim+p)exchange                                                           !/*ro        310*/
      com=curr(2)-space(med+9)                                                  !/*ro        311*/
!     (k.e.of pions outside nucleus                                             !/*ro        312*/
      call crdet (1,v(1),com)                                                   !/*ro        313*/
      fmax(4)=crdt(1)                                                           !/*ro        314*/
!     c(pip+p)abs.                                                              !/*ro        315*/
      i1=4                                                                      !/*ro        316*/
      go to 790                                                                 !/*ro        317*/
 1240 call crdet (1,w(1),com)                                                   !/*ro        318*/
      fmax(5)=crdt(1)                                                           !/*ro        319*/
 1250 i1=7                                                                      !/*ro        320*/
      go to 790                                                                 !/*ro        321*/
 1260 call bg6ca (3,4)                                                          !/*ro        322*/
      ifcc=24                                                                   !/*ro        323*/
 1270 ka=7                                                                      !/*ro        324*/
      med=clsm                                                                  !/*ro        325*/
      iv=19                                                                     !/*ro        326*/
      go to 60                                                                  !/*ro        327*/
 1280 i4=8                                                                      !/*ro        328*/
 1290 i2=2                                                                      !/*ro        329*/
      go to 890                                                                 !/*ro        330*/
 1300 call bg6ca (2,3)                                                          !/*ro        331*/
 1310 ifcc=21                                                                   !/*ro        332*/
      go to 1270                                                                !/*ro        333*/
 1320 i4=11                                                                     !/*ro        334*/
      go to 1290                                                                !/*ro        335*/
 1330 if (ex-d(5)) 1300,1300,1370                                               !/*ro        336*/
 1340 i1=9-i5                                                                   !/*ro        337*/
      go to 890                                                                 !/*ro        338*/
 1350 if (i6.eq.2) go to 1260                                                   !/*ro        339*/
      iv=20                                                                     !/*ro        340*/
      go to 60                                                                  !/*ro        341*/
 1360 call bg6ca (1,2)                                                          !/*ro        342*/
      go to 1310                                                                !/*ro        343*/
 1370 i1=5                                                                      !/*ro        344*/
      go to 1120                                                                !/*ro        345*/
 1380 call bovera (curr(2),poms,ans)                                            !/*ro        346*/
      fmax(1)=89.2d-27*ans                                                      !/*ro        347*/
!     (pi0+p)elast.scat.                                                        !/*ro        348*/
      fmax(2)=45.1d-27*ans                                                      !/*ro        349*/
!     (pi0+p)ex.scat.                                                           !/*ro        350*/
      fmax(3)=fmax(1)                                                           !/*ro        351*/
      space(48)=fmax(1)                                                         !/*ro        352*/
      fmax(4)=fmax(2)                                                           !/*ro        353*/
      space(49)=fmax(2)                                                         !/*ro        354*/
      com=curr(2)-space(med+9)                                                  !/*ro        355*/
      call crdet (1,w(1),com)                                                   !/*ro        356*/
!     (pin-p)abs. crs.sec.                                                      !/*ro        357*/
      fmax(5)=crdt(1)                                                           !/*ro        358*/
!     (pi0+p)abs.                                                               !/*ro        359*/
      space(50)=fmax(5)                                                         !/*ro        360*/
      i1=5                                                                      !/*ro        361*/
      go to 790                                                                 !/*ro        362*/
 1390 if (not-2) 1410,1430,1400                                                 !/*ro        363*/
 1400 iv=21                                                                     !/*ro        364*/
      go to 60                                                                  !/*ro        365*/
 1410 call crjab (1,x(1))                                                       !/*ro        366*/
!     (pin-p)direct scat.crs.                                                   !/*ro        367*/
 1420 iv=22                                                                     !/*ro        368*/
      go to 60                                                                  !/*ro        369*/
 1430 call crjab (1,y(1))                                                       !/*ro        370*/
!     (pim-p)xch.scat.crs.                                                      !/*ro        371*/
      go to 1420                                                                !/*ro        372*/
 1440 if (knot-16) 1450,1430,1430                                               !/*ro        373*/
 1450 call crjab (1,z(1))                                                       !/*ro        374*/
!     (pin-n)drct.scat.crs.                                                     !/*ro        375*/
      go to 1420                                                                !/*ro        376*/
      end                                                                       !/*ro        377*/

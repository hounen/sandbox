!     $Id: disint.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                       !/*di          1*/
      subroutine disint (ij,poo,eke,txi,tyi,tzi,we,cxr,cyr,czr,tki,kpart        !/*di          2*/
     & ,wei,np)                                                                 !/*di          3*/
!     a.g.                                                                      !/*di          4*/
!     modification: decay particles taken through call                          !/*di          5*/
!********************************************************************           !/*di          6*/
!     version may 83 by              pertti aarnio                              !/*di          7*/
!                                    helsinki university of                     !/*di          8*/
!                                    technology, finland                        !/*di          9*/
!     last change 08. december 83 by pertti aarnio                              !/*di         10*/
!                                                                               !/*di         11*/
!     this is a subroutine of fluka82 for stable particle decay                 !/*di         12*/
!                                                                               !/*di         13*/
!     input variables:                                                          !/*di         14*/
!        ij   = type of the decaying particle                                   !/*di         15*/
!        poo  = its momentum in laboratory in gev/c                             !/*di         16*/
!        eke  = its kinetic energy in laboratory in gev                         !/*di         17*/
!        txi  = its direction cosine with respect to x-axis                     !/*di         18*/
!        tyi  = its direction cosine with respect to y-axis                     !/*di         19*/
!        tzi  = its direction cosine with respect to z-axis                     !/*di         20*/
!        we   = its weight                                                      !/*di         21*/
!********************************************************************           !/*di         22*/
#include <lcs/double.h>                                                         /*!di         23*/
#include <fluka89/flprm1.h>                                                     /*!di         24*/
#include <fluka89/flprm2.h>                                                     /*!di         25*/
#include <lcs/inout.h>                                                          /*!di         26*/
#include <fluka89/paprop.h>                                                     /*!di         27*/
      parameter (mxm=10)                                                        !/*di         28*/
      dimension cxr(mxm), cyr(mxm), czr(mxm), tki(mxm), plr(mxm), wei           !/*di         29*/
     & (mxm), kpart(mxm)                                                        !/*di         30*/
!     modification for lahet interface.                                         !/*di         31*/
!     a.g.                                                                      !/*di         32*/
!     common changed to dimension to form seperate stack for decay              !/*di         33*/
!     products.                                                                 !/*di         34*/
!REP                                                                            !/*di         35*/
      dimension cv0(3), cv1(3)                                                  !/*di         36*/
!REP                                                                            !/*di         37*/
!                                                                               !/*di         38*/
! the following has been modified by a. ferrari                                 !/*di         39*/
!                                                                               !/*di         40*/
      data idi /mxm/                                                            !/*di         41*/
!      data idi/350/                                                            !/*di         42*/
      go to (10,20,30,40,50,60,70,80,90,100,110,120,180,190,200,260,320,        !/*di         43*/
     & 340,360,380,390,410,420,440,450,460,470,480,490,500), ij                 !/*di         44*/
!REP                                                                            !/*di         45*/
!     return                                                                    !/*di         46*/
!     go to 580                                                                 !/*di         47*/
!REP                                                                            !/*di         48*/
!                                                                               !/*di         49*/
!********************************************************************           !/*di         50*/
!                                                                               !/*di         51*/
!     select the decay channel for different primaries                          !/*di         52*/
!                                                                               !/*di         53*/
!********************************************************************           !/*di         54*/
!                                                                               !/*di         55*/
!*****proton                                                                    !/*di         56*/
!                                                                               !/*di         57*/
   10 continue                                                                  !/*di         58*/
      go to 510                                                                 !/*di         59*/
!                                                                               !/*di         60*/
!**** anti proton                                                               !/*di         61*/
!                                                                               !/*di         62*/
   20 continue                                                                  !/*di         63*/
      go to 510                                                                 !/*di         64*/
!                                                                               !/*di         65*/
!*****electron                                                                  !/*di         66*/
!                                                                               !/*di         67*/
   30 continue                                                                  !/*di         68*/
      go to 510                                                                 !/*di         69*/
!                                                                               !/*di         70*/
!*****positron                                                                  !/*di         71*/
!                                                                               !/*di         72*/
   40 continue                                                                  !/*di         73*/
      go to 510                                                                 !/*di         74*/
!                                                                               !/*di         75*/
!*****neutrino                                                                  !/*di         76*/
!                                                                               !/*di         77*/
   50 continue                                                                  !/*di         78*/
      go to 510                                                                 !/*di         79*/
!                                                                               !/*di         80*/
!*****anti neutrino                                                             !/*di         81*/
!                                                                               !/*di         82*/
   60 continue                                                                  !/*di         83*/
      go to 510                                                                 !/*di         84*/
!                                                                               !/*di         85*/
!*****photon                                                                    !/*di         86*/
!                                                                               !/*di         87*/
   70 continue                                                                  !/*di         88*/
      go to 510                                                                 !/*di         89*/
!                                                                               !/*di         90*/
!*****neutron                                                                   !/*di         91*/
!                                                                               !/*di         92*/
   80 continue                                                                  !/*di         93*/
      kpart(1)=1                                                                !/*di         94*/
      kpart(2)=3                                                                !/*di         95*/
      kpart(3)=6                                                                !/*di         96*/
      go to 530                                                                 !/*di         97*/
!                                                                               !/*di         98*/
!*****anti neutron                                                              !/*di         99*/
!                                                                               !/*di        100*/
   90 continue                                                                  !/*di        101*/
      kpart(1)=2                                                                !/*di        102*/
      kpart(2)=4                                                                !/*di        103*/
      kpart(3)=5                                                                !/*di        104*/
      go to 530                                                                 !/*di        105*/
!                                                                               !/*di        106*/
!*****positive muon                                                             !/*di        107*/
!                                                                               !/*di        108*/
  100 continue                                                                  !/*di        109*/
      kpart(1)=4                                                                !/*di        110*/
      kpart(2)=5                                                                !/*di        111*/
      kpart(3)=6                                                                !/*di        112*/
      go to 530                                                                 !/*di        113*/
!                                                                               !/*di        114*/
!*****negative muon                                                             !/*di        115*/
!                                                                               !/*di        116*/
  110 continue                                                                  !/*di        117*/
      kpart(1)=3                                                                !/*di        118*/
      kpart(2)=5                                                                !/*di        119*/
      kpart(3)=6                                                                !/*di        120*/
      go to 530                                                                 !/*di        121*/
!                                                                               !/*di        122*/
!*****kaon-long                                                                 !/*di        123*/
!                                                                               !/*di        124*/
  120 continue                                                                  !/*di        125*/
      cha=rang()                                                                !/*di        126*/
      if (cha.lt.0.215d0) go to 130                                             !/*di        127*/
      if (cha.lt.0.3389d0) go to 140                                            !/*di        128*/
      if (cha.lt.0.4744d0) go to 150                                            !/*di        129*/
      if (cha.lt.0.6099d0) go to 160                                            !/*di        130*/
      if (cha.lt.0.8034d0) go to 170                                            !/*di        131*/
      kpart(1)=14                                                               !/*di        132*/
      kpart(2)=4                                                                !/*di        133*/
      kpart(3)=5                                                                !/*di        134*/
      go to 530                                                                 !/*di        135*/
  130 continue                                                                  !/*di        136*/
      kpart(1)=23                                                               !/*di        137*/
      kpart(2)=23                                                               !/*di        138*/
      kpart(3)=23                                                               !/*di        139*/
      go to 530                                                                 !/*di        140*/
  140 continue                                                                  !/*di        141*/
      kpart(1)=13                                                               !/*di        142*/
      kpart(2)=14                                                               !/*di        143*/
      kpart(3)=23                                                               !/*di        144*/
      go to 530                                                                 !/*di        145*/
  150 continue                                                                  !/*di        146*/
      kpart(1)=13                                                               !/*di        147*/
      kpart(2)=11                                                               !/*di        148*/
      kpart(3)=5                                                                !/*di        149*/
      go to 530                                                                 !/*di        150*/
  160 continue                                                                  !/*di        151*/
      kpart(1)=14                                                               !/*di        152*/
      kpart(2)=10                                                               !/*di        153*/
      kpart(3)=5                                                                !/*di        154*/
      go to 530                                                                 !/*di        155*/
  170 continue                                                                  !/*di        156*/
      kpart(1)=13                                                               !/*di        157*/
      kpart(2)=3                                                                !/*di        158*/
      kpart(3)=5                                                                !/*di        159*/
      go to 530                                                                 !/*di        160*/
!                                                                               !/*di        161*/
!*****positive pion                                                             !/*di        162*/
!                                                                               !/*di        163*/
  180 continue                                                                  !/*di        164*/
      kpart(1)=10                                                               !/*di        165*/
      kpart(2)=5                                                                !/*di        166*/
      go to 520                                                                 !/*di        167*/
!                                                                               !/*di        168*/
!*****negative pion                                                             !/*di        169*/
!                                                                               !/*di        170*/
  190 continue                                                                  !/*di        171*/
      kpart(1)=11                                                               !/*di        172*/
      kpart(2)=6                                                                !/*di        173*/
      go to 520                                                                 !/*di        174*/
!                                                                               !/*di        175*/
!*****positive kaon                                                             !/*di        176*/
!                                                                               !/*di        177*/
  200 continue                                                                  !/*di        178*/
      cha=rang()                                                                !/*di        179*/
      if (cha.lt.0.635d0) go to 210                                             !/*di        180*/
      if (cha.lt.0.8466d0) go to 220                                            !/*di        181*/
      if (cha.lt.0.9025d0) go to 230                                            !/*di        182*/
      if (cha.lt.0.9198d0) go to 240                                            !/*di        183*/
      if (cha.lt.0.9518d0) go to 250                                            !/*di        184*/
      kpart(1)=23                                                               !/*di        185*/
      kpart(2)=4                                                                !/*di        186*/
      kpart(3)=5                                                                !/*di        187*/
      go to 530                                                                 !/*di        188*/
  210 continue                                                                  !/*di        189*/
      kpart(1)=10                                                               !/*di        190*/
      kpart(2)=5                                                                !/*di        191*/
      go to 520                                                                 !/*di        192*/
  220 continue                                                                  !/*di        193*/
      kpart(1)=13                                                               !/*di        194*/
      kpart(2)=23                                                               !/*di        195*/
      go to 520                                                                 !/*di        196*/
  230 continue                                                                  !/*di        197*/
      kpart(1)=13                                                               !/*di        198*/
      kpart(2)=13                                                               !/*di        199*/
      kpart(3)=14                                                               !/*di        200*/
      go to 530                                                                 !/*di        201*/
  240 continue                                                                  !/*di        202*/
      kpart(1)=13                                                               !/*di        203*/
      kpart(2)=23                                                               !/*di        204*/
      kpart(3)=23                                                               !/*di        205*/
      go to 530                                                                 !/*di        206*/
  250 continue                                                                  !/*di        207*/
      kpart(1)=23                                                               !/*di        208*/
      kpart(2)=10                                                               !/*di        209*/
      kpart(3)=5                                                                !/*di        210*/
      go to 530                                                                 !/*di        211*/
!                                                                               !/*di        212*/
!*****negative kaon                                                             !/*di        213*/
!                                                                               !/*di        214*/
  260 continue                                                                  !/*di        215*/
      cha=rang()                                                                !/*di        216*/
      if (cha.lt.0.635d0) go to 270                                             !/*di        217*/
      if (cha.lt.0.8466d0) go to 280                                            !/*di        218*/
      if (cha.lt.0.9025d0) go to 290                                            !/*di        219*/
      if (cha.lt.0.9198d0) go to 300                                            !/*di        220*/
      if (cha.lt.0.9518d0) go to 310                                            !/*di        221*/
      kpart(1)=23                                                               !/*di        222*/
      kpart(2)=3                                                                !/*di        223*/
      kpart(3)=6                                                                !/*di        224*/
      go to 530                                                                 !/*di        225*/
  270 continue                                                                  !/*di        226*/
      kpart(1)=11                                                               !/*di        227*/
      kpart(2)=6                                                                !/*di        228*/
      go to 520                                                                 !/*di        229*/
  280 continue                                                                  !/*di        230*/
      kpart(1)=14                                                               !/*di        231*/
      kpart(2)=23                                                               !/*di        232*/
      go to 520                                                                 !/*di        233*/
  290 continue                                                                  !/*di        234*/
      kpart(1)=14                                                               !/*di        235*/
      kpart(2)=14                                                               !/*di        236*/
      kpart(3)=13                                                               !/*di        237*/
      go to 530                                                                 !/*di        238*/
  300 continue                                                                  !/*di        239*/
      kpart(1)=14                                                               !/*di        240*/
      kpart(2)=23                                                               !/*di        241*/
      kpart(3)=23                                                               !/*di        242*/
      go to 530                                                                 !/*di        243*/
  310 continue                                                                  !/*di        244*/
      kpart(1)=23                                                               !/*di        245*/
      kpart(2)=11                                                               !/*di        246*/
      kpart(3)=6                                                                !/*di        247*/
      go to 530                                                                 !/*di        248*/
!                                                                               !/*di        249*/
!*****lambda                                                                    !/*di        250*/
!                                                                               !/*di        251*/
  320 continue                                                                  !/*di        252*/
      cha=rang()                                                                !/*di        253*/
      if (cha.lt.0.642d0) go to 330                                             !/*di        254*/
      kpart(1)=8                                                                !/*di        255*/
      kpart(2)=23                                                               !/*di        256*/
      go to 520                                                                 !/*di        257*/
  330 continue                                                                  !/*di        258*/
      kpart(1)=1                                                                !/*di        259*/
      kpart(2)=14                                                               !/*di        260*/
      go to 520                                                                 !/*di        261*/
!                                                                               !/*di        262*/
!*****anti lambda                                                               !/*di        263*/
!                                                                               !/*di        264*/
  340 continue                                                                  !/*di        265*/
      cha=rang()                                                                !/*di        266*/
      if (cha.lt.0.642d0) go to 350                                             !/*di        267*/
      kpart(1)=9                                                                !/*di        268*/
      kpart(2)=23                                                               !/*di        269*/
      go to 520                                                                 !/*di        270*/
  350 continue                                                                  !/*di        271*/
      kpart(1)=2                                                                !/*di        272*/
      kpart(2)=13                                                               !/*di        273*/
      go to 520                                                                 !/*di        274*/
!                                                                               !/*di        275*/
!*****kaon-short                                                                !/*di        276*/
!                                                                               !/*di        277*/
  360 continue                                                                  !/*di        278*/
      cha=rang()                                                                !/*di        279*/
      if (cha.lt.0.6861d0) go to 370                                            !/*di        280*/
      kpart(1)=23                                                               !/*di        281*/
      kpart(2)=23                                                               !/*di        282*/
      go to 520                                                                 !/*di        283*/
  370 continue                                                                  !/*di        284*/
      kpart(1)=13                                                               !/*di        285*/
      kpart(2)=14                                                               !/*di        286*/
      go to 520                                                                 !/*di        287*/
!                                                                               !/*di        288*/
!*****negative sigma                                                            !/*di        289*/
!                                                                               !/*di        290*/
  380 continue                                                                  !/*di        291*/
      kpart(1)=8                                                                !/*di        292*/
      kpart(2)=14                                                               !/*di        293*/
      go to 520                                                                 !/*di        294*/
!                                                                               !/*di        295*/
!*****positive sigma                                                            !/*di        296*/
!                                                                               !/*di        297*/
  390 continue                                                                  !/*di        298*/
      cha=rang()                                                                !/*di        299*/
      if (cha.lt.0.5164d0) go to 400                                            !/*di        300*/
      kpart(1)=8                                                                !/*di        301*/
      kpart(2)=13                                                               !/*di        302*/
      go to 520                                                                 !/*di        303*/
  400 continue                                                                  !/*di        304*/
      kpart(1)=1                                                                !/*di        305*/
      kpart(2)=23                                                               !/*di        306*/
      go to 520                                                                 !/*di        307*/
!                                                                               !/*di        308*/
!*****neutral sigma                                                             !/*di        309*/
!                                                                               !/*di        310*/
  410 continue                                                                  !/*di        311*/
      kpart(1)=17                                                               !/*di        312*/
      kpart(2)=7                                                                !/*di        313*/
      go to 520                                                                 !/*di        314*/
!                                                                               !/*di        315*/
!*****neutral pion                                                              !/*di        316*/
!                                                                               !/*di        317*/
  420 continue                                                                  !/*di        318*/
      cha=rang()                                                                !/*di        319*/
      if (cha.lt.0.98787d0) go to 430                                           !/*di        320*/
      kpart(1)=7                                                                !/*di        321*/
      kpart(2)=3                                                                !/*di        322*/
      kpart(3)=4                                                                !/*di        323*/
      go to 530                                                                 !/*di        324*/
  430 continue                                                                  !/*di        325*/
      kpart(1)=7                                                                !/*di        326*/
      kpart(2)=7                                                                !/*di        327*/
      go to 520                                                                 !/*di        328*/
!                                                                               !/*di        329*/
!*****neutral kaon                                                              !/*di        330*/
!                                                                               !/*di        331*/
  440 continue                                                                  !/*di        332*/
      np=1                                                                      !/*di        333*/
      kpart(1)=19                                                               !/*di        334*/
      if (rang().gt.0.5d0) kpart(1)=12                                          !/*di        335*/
      cxr(1)=txi                                                                !/*di        336*/
      cyr(1)=tyi                                                                !/*di        337*/
      czr(1)=tzi                                                                !/*di        338*/
      tki(1)=eke                                                                !/*di        339*/
      plr(1)=poo                                                                !/*di        340*/
      wei(1)=we                                                                 !/*di        341*/
!REP                                                                            !/*di        342*/
!     return                                                                    !/*di        343*/
      go to 580                                                                 !/*di        344*/
!REP                                                                            !/*di        345*/
!                                                                               !/*di        346*/
!*****neutral anti-kaon                                                         !/*di        347*/
!                                                                               !/*di        348*/
  450 continue                                                                  !/*di        349*/
      go to 440                                                                 !/*di        350*/
!                                                                               !/*di        351*/
!*****blank                                                                     !/*di        352*/
!AG***rename the cascade 0 (teta0 id=97)                                        !/*di        353*/
!                                                                               !/*di        354*/
  460 continue                                                                  !/*di        355*/
!REP  go to 510                                                                 !/*di        356*/
!AG   kpart(1)=17                                                               !/*di        357*/
!AG   kpart(2)=23                                                               !/*di        358*/
!AG   go to 520                                                                 !/*di        359*/
!                                                                               !/*di        360*/
!*****blank                                                                     !/*di        361*/
!AG***rename the cascade 0 (teta- id=98)                                        !/*di        362*/
!                                                                               !/*di        363*/
  470 continue                                                                  !/*di        364*/
!REP  go to 510                                                                 !/*di        365*/
!AG   kpart(1)=17                                                               !/*di        366*/
!AG   kpart(2)=14                                                               !/*di        367*/
!AG   go to 520                                                                 !/*di        368*/
!                                                                               !/*di        369*/
!*****blank                                                                     !/*di        370*/
!AG***rename the cascade - (teta+ id=99)                                        !/*di        371*/
!                                                                               !/*di        372*/
  480 continue                                                                  !/*di        373*/
!REP  go to 510                                                                 !/*di        374*/
!AG   kpart(1)=17                                                               !/*di        375*/
!AG   kpart(2)=13                                                               !/*di        376*/
!AG   go to 520                                                                 !/*di        377*/
!                                                                               !/*di        378*/
!*****blank                                                                     !/*di        379*/
!                                                                               !/*di        380*/
  490 continue                                                                  !/*di        381*/
!REP  go to 510                                                                 !/*di        382*/
!                                                                               !/*di        383*/
!*****blank                                                                     !/*di        384*/
!                                                                               !/*di        385*/
  500 continue                                                                  !/*di        386*/
!REP                                                                            !/*di        387*/
      write (itty,590) ij                                                       !/*di        388*/
!REP  go to 510                                                                 !/*di        389*/
!                                                                               !/*di        390*/
!********************************************************************           !/*di        391*/
!                                                                               !/*di        392*/
!     no decay - half life too long                                             !/*di        393*/
!                                                                               !/*di        394*/
!********************************************************************           !/*di        395*/
!                                                                               !/*di        396*/
  510 continue                                                                  !/*di        397*/
      np=1                                                                      !/*di        398*/
      kpart(1)=ij                                                               !/*di        399*/
      cxr(1)=txi                                                                !/*di        400*/
      cyr(1)=tyi                                                                !/*di        401*/
      czr(1)=tzi                                                                !/*di        402*/
      tki(1)=eke                                                                !/*di        403*/
      plr(1)=poo                                                                !/*di        404*/
      wei(1)=we                                                                 !/*di        405*/
!REP                                                                            !/*di        406*/
      go to 580                                                                 !/*di        407*/
!     return                                                                    !/*di        408*/
!REP                                                                            !/*di        409*/
!                                                                               !/*di        410*/
!********************************************************************           !/*di        411*/
!                                                                               !/*di        412*/
!     two particle decay                                                        !/*di        413*/
!                                                                               !/*di        414*/
!********************************************************************           !/*di        415*/
!                                                                               !/*di        416*/
  520 continue                                                                  !/*di        417*/
      np=2                                                                      !/*di        418*/
      call twopad (am(ij),tki(1),tki(2),plr(1),plr(2),cxr(1),cyr(1),czr(        !/*di        419*/
     & 1),cxr(2),cyr(2),czr(2),am(kpart(1)),am(kpart(2)))                       !/*di        420*/
      wei(1)=we                                                                 !/*di        421*/
      wei(2)=we                                                                 !/*di        422*/
      go to 540                                                                 !/*di        423*/
!                                                                               !/*di        424*/
!********************************************************************           !/*di        425*/
!                                                                               !/*di        426*/
!     three particle decay                                                      !/*di        427*/
!                                                                               !/*di        428*/
!********************************************************************           !/*di        429*/
!                                                                               !/*di        430*/
  530 continue                                                                  !/*di        431*/
      np=3                                                                      !/*di        432*/
      call threpd (am(ij),tki(1),tki(2),tki(3),plr(1),plr(2),plr(3),cxr(        !/*di        433*/
     & 1),cyr(1),czr(1),cxr(2),cyr(2),czr(2),cxr(3),cyr(3),czr(3),am            !/*di        434*/
     & (kpart(1)),am(kpart(2)),am(kpart(3)))                                    !/*di        435*/
      wei(1)=we                                                                 !/*di        436*/
      wei(2)=we                                                                 !/*di        437*/
      wei(3)=we                                                                 !/*di        438*/
!                                                                               !/*di        439*/
!********************************************************************           !/*di        440*/
!                                                                               !/*di        441*/
!     rotation of the direction of the decay products                           !/*di        442*/
!                                                                               !/*di        443*/
!********************************************************************           !/*di        444*/
!                                                                               !/*di        445*/
  540 continue                                                                  !/*di        446*/
!REP                                                                            !/*di        447*/
      residu=am(ij)                                                             !/*di        448*/
      resid1=residu+eke                                                         !/*di        449*/
      px0=dp0                                                                   !/*di        450*/
      py0=dp0                                                                   !/*di        451*/
      pz0=dp0                                                                   !/*di        452*/
      px1=poo*txi                                                               !/*di        453*/
      py1=poo*tyi                                                               !/*di        454*/
      pz1=poo*tzi                                                               !/*di        455*/
      if (np.le.0) go to 580                                                    !/*di        456*/
!     if (np.le.0) return                                                       !/*di        457*/
!REP                                                                            !/*di        458*/
      do 550 k=1,np                                                             !/*di        459*/
      cx=cxr(k)                                                                 !/*di        460*/
      cy=cyr(k)                                                                 !/*di        461*/
      cz=czr(k)                                                                 !/*di        462*/
      pcm=plr(k)                                                                !/*di        463*/
      ecm=tki(k)                                                                !/*di        464*/
!REP                                                                            !/*di        465*/
      px0=px0-plr(k)*cxr(k)                                                     !/*di        466*/
      py0=py0-plr(k)*cyr(k)*sqrt(dp1-cxr(k)**2)                                 !/*di        467*/
      pz0=pz0-plr(k)*czr(k)*sqrt(dp1-cxr(k)**2)                                 !/*di        468*/
      residu=residu-tki(k)                                                      !/*di        469*/
      cv0(k)=cxr(k)**2+(dp1-cxr(k)**2)*(cyr(k)**2+czr(k)**2)-dp1                !/*di        470*/
!REP                                                                            !/*di        471*/
      gam=1.d0+eke/am(ij)                                                       !/*di        472*/
      bgam=poo/am(ij)                                                           !/*di        473*/
      call trafo (gam,bgam,txi,tyi,tzi,cx,cy,cz,pcm,ecm,plr(k),cxr(k)           !/*di        474*/
     & ,cyr(k),czr(k),tki(k))                                                   !/*di        475*/
!REP                                                                            !/*di        476*/
      resid1=resid1-tki(k)                                                      !/*di        477*/
      px1=px1-plr(k)*cxr(k)                                                     !/*di        478*/
      py1=py1-plr(k)*cyr(k)                                                     !/*di        479*/
      pz1=pz1-plr(k)*czr(k)                                                     !/*di        480*/
      cv1(k)=cxr(k)**2+cyr(k)**2+czr(k)**2-dp1                                  !/*di        481*/
!REP                                                                            !/*di        482*/
      tki(k)=tki(k)-am(kpart(k))                                                !/*di        483*/
  550 continue                                                                  !/*di        484*/
!REP                                                                            !/*di        485*/
      p0=sqrt(px0**2+py0**2+pz0**2)                                             !/*di        486*/
      p1=sqrt(px1**2+py1**2+pz1**2)                                             !/*di        487*/
      if (abs(residu).ge.0.001d0.or.abs(resid1).ge.0.001d0.or.p0.ge.0.00        !/*di        488*/
     & 1d0.or.p1.ge.0.001d0) go to 570                                          !/*di        489*/
      do 560 k=1,np                                                             !/*di        490*/
      if (cv0(k).ge.0.001d0.or.cv1(k).ge.0.001d0) go to 570                     !/*di        491*/
  560 continue                                                                  !/*di        492*/
      go to 580                                                                 !/*di        493*/
  570 continue                                                                  !/*di        494*/
      nocas=igetd(1)                                                            !/*di        495*/
      write (io,600) nocas,residu,p0,px0,py0,pz0,resid1,p1,px1,py1,pz1          !/*di        496*/
     & ,ij,am(ij),eke,(kpart(k),am(kpart(k)),tki(k),k=1,np)                     !/*di        497*/
      write (io,610) (cv0(k),cv1(k),k=1,np)                                     !/*di        498*/
      write (itty,600) nocas,residu,p0,px0,py0,pz0,resid1,p1,px1,py1,pz1        !/*di        499*/
     & ,ij,am(ij),eke,(kpart(k),am(kpart(k)),tki(k),k=1,np)                     !/*di        500*/
      write (itty,610) (cv0(k),cv1(k),k=1,np)                                   !/*di        501*/
  580 continue                                                                  !/*di        502*/
!REP                                                                            !/*di        503*/
      return                                                                    !/*di        504*/
!REP                                                                            !/*di        505*/
!                                                                               !/*di        506*/
!REP                                                                            !/*di        507*/
!                                                                               !/*di        508*/
  590 format (' DISINT: ij = ',i4)                                              !/*di        509*/
  600 format (' energy defect in DISINT - NOCAS =',i8/1p5e12.4/1p5e12.4/        !/*di        510*/
     & 5x,i3,1p2e12.4/3(5x,i3,1p2e12.4))                                        !/*di        511*/
  610 format (5x,1p6e12.4)                                                      !/*di        512*/
      end                                                                       !/*di        513*/

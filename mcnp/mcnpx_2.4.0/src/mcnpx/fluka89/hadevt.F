!     $Id: hadevt.F,v 1.4 2002/03/27 18:14:44 gwm Exp $                       !/*ha          1*/
      subroutine hadevt (nhad,kproj,ktarg,pproj)                                !/*ha          2*/
!                                                                     *         !/*ha          3*/
!---------------------------------------------------------------------*         !/*ha          4*/
!                                                                     *         !/*ha          5*/
#include <lcs/double.h>                                                         /*!ha          6*/
#include <fluka89/flprm1.h>                                                     /*!ha          7*/
#include <fluka89/flprm2.h>                                                     /*!ha          8*/
#include <lcs/inout.h>                                                          /*!ha          9*/
#include <fluka89/auxpar.h>                                                     /*!ha         10*/
#include <fluka89/cmsres.h>                                                     /*!ha         11*/
#include <fluka89/finpr2.h>                                                     /*!ha         12*/
#include <fluka89/hadpar.h>                                                     /*!ha         13*/
!                                                                               !/*ha         14*/
#include <fluka89/inpdt1.h>                                                     /*!ha         15*/
#include <fluka89/part1.h>                                                      /*!ha         16*/
!REP  common /invt/ pnuc(3), inucvt                                             !/*ha         17*/
      data unon /2.0d0/                                                         !/*ha         18*/
      data unom /1.5d0/                                                         !/*ha         19*/
      data unoms /0.5d0/                                                        !/*ha         20*/
#include <fluka89/print.h>                                                      /*!ha         21*/
      logical lissu                                                             !/*ha         22*/
      dimension mquark(3,30)                                                    !/*ha         23*/
      data mquark /1,1,2,-1,-1,-2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,-1        !/*ha         24*/
     & ,-2,-2,0,0,0,0,0,0,0,0,0,1,-2,0,2,-1,0,1,-3,0,3,-1,0,1,2,3,-1,-2,        !/*ha         25*/
     & -3,0,0,0,2,2,3,1,1,3,1,2,3,1,-1,0,2,-3,0,3,-2,0,2,-2,0,0,0,0,0,0,        !/*ha         26*/
     & 0,0,0,0,0,0,0/                                                           !/*ha         27*/
      data ecrit/1.0d-12/, pcrit/1.0d-12/                                       !/*ha         28*/
   10 continue                                                                  !/*ha         29*/
      asbamj=0.5d0                                                              !/*ha         30*/
      b8bamj=0.4d0                                                              !/*ha         31*/
      amproj=am(kproj)                                                          !/*ha         32*/
      amtar=am(ktarg)                                                           !/*ha         33*/
      eproj=sqrt(pproj**2+amproj**2)                                            !/*ha         34*/
      umo=sqrt(amproj**2+amtar**2+2.d0*amtar*eproj)                             !/*ha         35*/
      gamcm=(eproj+amtar)/umo                                                   !/*ha         36*/
      bgcm=pproj/umo                                                            !/*ha         37*/
      ibproj=ibar(kproj)                                                        !/*ha         38*/
      ibtarg=ibar(ktarg)                                                        !/*ha         39*/
      iqp1=mquark(1,kproj)                                                      !/*ha         40*/
      iqp2=mquark(2,kproj)                                                      !/*ha         41*/
      iqp3=mquark(3,kproj)                                                      !/*ha         42*/
      iqt1=mquark(1,ktarg)                                                      !/*ha         43*/
      iqt2=mquark(2,ktarg)                                                      !/*ha         44*/
      iqt3=mquark(3,ktarg)                                                      !/*ha         45*/
      if (ibproj) 20,110,410                                                    !/*ha         46*/
   20 continue                                                                  !/*ha         47*/
      isam1=1.d0+3.d0*rang()                                                    !/*ha         48*/
      go to (30,40,50), isam1                                                   !/*ha         49*/
   30 continue                                                                  !/*ha         50*/
      ibf=iqp1                                                                  !/*ha         51*/
      iff1=iqp2                                                                 !/*ha         52*/
      iff2=iqp3                                                                 !/*ha         53*/
      go to 60                                                                  !/*ha         54*/
   40 continue                                                                  !/*ha         55*/
      ibf=iqp2                                                                  !/*ha         56*/
      iff1=iqp1                                                                 !/*ha         57*/
      iff2=iqp3                                                                 !/*ha         58*/
      go to 60                                                                  !/*ha         59*/
   50 continue                                                                  !/*ha         60*/
      ibf=iqp3                                                                  !/*ha         61*/
      iff1=iqp1                                                                 !/*ha         62*/
      iff2=iqp2                                                                 !/*ha         63*/
   60 continue                                                                  !/*ha         64*/
      isam2=1.d0+3.d0*rang()                                                    !/*ha         65*/
      go to (70,80,90), isam2                                                   !/*ha         66*/
   70 continue                                                                  !/*ha         67*/
      ibb=iqt1                                                                  !/*ha         68*/
      ifb1=iqt2                                                                 !/*ha         69*/
      ifb2=iqt3                                                                 !/*ha         70*/
      go to 100                                                                 !/*ha         71*/
   80 continue                                                                  !/*ha         72*/
      ibb=iqt2                                                                  !/*ha         73*/
      ifb1=iqt1                                                                 !/*ha         74*/
      ifb2=iqt3                                                                 !/*ha         75*/
      go to 100                                                                 !/*ha         76*/
   90 continue                                                                  !/*ha         77*/
      ibb=iqt3                                                                  !/*ha         78*/
      ifb1=iqt1                                                                 !/*ha         79*/
      ifb2=iqt2                                                                 !/*ha         80*/
  100 continue                                                                  !/*ha         81*/
      go to 500                                                                 !/*ha         82*/
  110 continue                                                                  !/*ha         83*/
      if (ibtarg) 300,260,120                                                   !/*ha         84*/
  120 continue                                                                  !/*ha         85*/
      isam3=1.d0+2.d0*rang()                                                    !/*ha         86*/
      go to (130,140), isam3                                                    !/*ha         87*/
  130 continue                                                                  !/*ha         88*/
      iff=iqp1                                                                  !/*ha         89*/
      ibf=iqp2                                                                  !/*ha         90*/
      go to 150                                                                 !/*ha         91*/
  140 continue                                                                  !/*ha         92*/
      iff=iqp2                                                                  !/*ha         93*/
      ibf=iqp1                                                                  !/*ha         94*/
  150 continue                                                                  !/*ha         95*/
      isam4=1.d0+3.d0*rang()                                                    !/*ha         96*/
      go to (160,190,220), isam4                                                !/*ha         97*/
  160 continue                                                                  !/*ha         98*/
      go to (170,180), isam3                                                    !/*ha         99*/
  170 continue                                                                  !/*ha        100*/
      ibb=iqt1                                                                  !/*ha        101*/
      ifb1=iqt2                                                                 !/*ha        102*/
      ifb2=iqt3                                                                 !/*ha        103*/
      go to 250                                                                 !/*ha        104*/
  180 continue                                                                  !/*ha        105*/
      ibb1=iqt2                                                                 !/*ha        106*/
      ibb2=iqt3                                                                 !/*ha        107*/
      ifb=iqt1                                                                  !/*ha        108*/
      go to 250                                                                 !/*ha        109*/
  190 continue                                                                  !/*ha        110*/
      go to (200,210), isam3                                                    !/*ha        111*/
  200 continue                                                                  !/*ha        112*/
      ibb=iqt2                                                                  !/*ha        113*/
      ifb1=iqt1                                                                 !/*ha        114*/
      ifb2=iqt3                                                                 !/*ha        115*/
      go to 250                                                                 !/*ha        116*/
  210 continue                                                                  !/*ha        117*/
      ibb1=iqt1                                                                 !/*ha        118*/
      ibb2=iqt3                                                                 !/*ha        119*/
      ifb=iqt2                                                                  !/*ha        120*/
      go to 250                                                                 !/*ha        121*/
  220 continue                                                                  !/*ha        122*/
      go to (230,240), isam3                                                    !/*ha        123*/
  230 continue                                                                  !/*ha        124*/
      ibb=iqt3                                                                  !/*ha        125*/
      ifb1=iqt1                                                                 !/*ha        126*/
      ifb2=iqt2                                                                 !/*ha        127*/
      go to 250                                                                 !/*ha        128*/
  240 continue                                                                  !/*ha        129*/
      ibb1=iqt1                                                                 !/*ha        130*/
      ibb2=iqt2                                                                 !/*ha        131*/
      ifb=iqt3                                                                  !/*ha        132*/
  250 continue                                                                  !/*ha        133*/
      go to 500                                                                 !/*ha        134*/
  260 continue                                                                  !/*ha        135*/
      isam3=1.d0+2.d0*rang()                                                    !/*ha        136*/
      go to (270,280), isam3                                                    !/*ha        137*/
  270 continue                                                                  !/*ha        138*/
      iff=iqp1                                                                  !/*ha        139*/
      ibf=iqp2                                                                  !/*ha        140*/
      ibb=iqt1                                                                  !/*ha        141*/
      ifb=iqt2                                                                  !/*ha        142*/
      go to 290                                                                 !/*ha        143*/
  280 continue                                                                  !/*ha        144*/
      iff=iqp2                                                                  !/*ha        145*/
      ibf=iqp1                                                                  !/*ha        146*/
      ibb=iqt2                                                                  !/*ha        147*/
      ifb=iqt1                                                                  !/*ha        148*/
  290 continue                                                                  !/*ha        149*/
      go to 500                                                                 !/*ha        150*/
  300 continue                                                                  !/*ha        151*/
      isam3=2                                                                   !/*ha        152*/
      iff=iqp1                                                                  !/*ha        153*/
      ibf=iqp2                                                                  !/*ha        154*/
      isam4=1.d0+3.d0*rang()                                                    !/*ha        155*/
      go to (310,340,370), isam4                                                !/*ha        156*/
  310 continue                                                                  !/*ha        157*/
      go to (320,330), isam3                                                    !/*ha        158*/
  320 continue                                                                  !/*ha        159*/
      ibb=iqt1                                                                  !/*ha        160*/
      ifb1=iqt2                                                                 !/*ha        161*/
      ifb2=iqt3                                                                 !/*ha        162*/
      go to 400                                                                 !/*ha        163*/
  330 continue                                                                  !/*ha        164*/
      ibb1=iqt2                                                                 !/*ha        165*/
      ibb2=iqt3                                                                 !/*ha        166*/
      ifb=iqt1                                                                  !/*ha        167*/
      go to 400                                                                 !/*ha        168*/
  340 continue                                                                  !/*ha        169*/
      go to (350,360), isam3                                                    !/*ha        170*/
  350 continue                                                                  !/*ha        171*/
      ibb=iqt2                                                                  !/*ha        172*/
      ifb1=iqt1                                                                 !/*ha        173*/
      ifb2=iqt3                                                                 !/*ha        174*/
      go to 400                                                                 !/*ha        175*/
  360 continue                                                                  !/*ha        176*/
      ibb1=iqt1                                                                 !/*ha        177*/
      ibb2=iqt3                                                                 !/*ha        178*/
      ifb=iqt2                                                                  !/*ha        179*/
      go to 400                                                                 !/*ha        180*/
  370 continue                                                                  !/*ha        181*/
      go to (380,390), isam3                                                    !/*ha        182*/
  380 continue                                                                  !/*ha        183*/
      ibb=iqt3                                                                  !/*ha        184*/
      ifb1=iqt1                                                                 !/*ha        185*/
      ifb2=iqt2                                                                 !/*ha        186*/
      go to 400                                                                 !/*ha        187*/
  390 continue                                                                  !/*ha        188*/
      ibb1=iqt1                                                                 !/*ha        189*/
      ibb2=iqt2                                                                 !/*ha        190*/
      ifb=iqt3                                                                  !/*ha        191*/
  400 continue                                                                  !/*ha        192*/
      go to 500                                                                 !/*ha        193*/
  410 continue                                                                  !/*ha        194*/
      isam5=1.d0+3.d0*rang()                                                    !/*ha        195*/
      go to (420,430,440), isam5                                                !/*ha        196*/
  420 continue                                                                  !/*ha        197*/
      ibf=iqp1                                                                  !/*ha        198*/
      iff1=iqp2                                                                 !/*ha        199*/
      iff2=iqp3                                                                 !/*ha        200*/
      go to 450                                                                 !/*ha        201*/
  430 continue                                                                  !/*ha        202*/
      ibf=iqp2                                                                  !/*ha        203*/
      iff1=iqp1                                                                 !/*ha        204*/
      iff2=iqp3                                                                 !/*ha        205*/
      go to 450                                                                 !/*ha        206*/
  440 continue                                                                  !/*ha        207*/
      ibf=iqp3                                                                  !/*ha        208*/
      iff1=iqp1                                                                 !/*ha        209*/
      iff2=iqp2                                                                 !/*ha        210*/
  450 continue                                                                  !/*ha        211*/
      isam6=1.d0+3.d0*rang()                                                    !/*ha        212*/
      go to (460,470,480), isam6                                                !/*ha        213*/
  460 continue                                                                  !/*ha        214*/
      ifb=iqt1                                                                  !/*ha        215*/
      ibb1=iqt2                                                                 !/*ha        216*/
      ibb2=iqt3                                                                 !/*ha        217*/
      go to 490                                                                 !/*ha        218*/
  470 continue                                                                  !/*ha        219*/
      ifb=iqt2                                                                  !/*ha        220*/
      ibb1=iqt1                                                                 !/*ha        221*/
      ibb2=iqt3                                                                 !/*ha        222*/
      go to 490                                                                 !/*ha        223*/
  480 continue                                                                  !/*ha        224*/
      ifb=iqt3                                                                  !/*ha        225*/
      ibb1=iqt1                                                                 !/*ha        226*/
      ibb2=iqt2                                                                 !/*ha        227*/
  490 continue                                                                  !/*ha        228*/
  500 continue                                                                  !/*ha        229*/
  510 continue                                                                  !/*ha        230*/
      if (ibproj) 520,540,530                                                   !/*ha        231*/
  520 continue                                                                  !/*ha        232*/
      uno=unon                                                                  !/*ha        233*/
      xp=betarn(0.5d0,uno)                                                      !/*ha        234*/
      xxp=1.d0-xp                                                               !/*ha        235*/
      uno=unon                                                                  !/*ha        236*/
      xt=betarn(0.5d0,uno)                                                      !/*ha        237*/
      xxt=1.d0-xt                                                               !/*ha        238*/
      go to 560                                                                 !/*ha        239*/
  530 continue                                                                  !/*ha        240*/
      uno=unon                                                                  !/*ha        241*/
      xp=betarn(0.5d0,uno)                                                      !/*ha        242*/
      xxp=1.d0-xp                                                               !/*ha        243*/
      uno=unon                                                                  !/*ha        244*/
      xt=betarn(0.5d0,uno)                                                      !/*ha        245*/
      xxt=1.d0-xt                                                               !/*ha        246*/
      go to 560                                                                 !/*ha        247*/
  540 continue                                                                  !/*ha        248*/
      uno=unom                                                                  !/*ha        249*/
      if (iff.eq.3.or.iff.eq.-3) uno=unoms                                      !/*ha        250*/
      xp=betarn(0.5d0,uno)                                                      !/*ha        251*/
      xxp=1.d0-xp                                                               !/*ha        252*/
      if (ibtarg.eq.0) go to 550                                                !/*ha        253*/
      uno=unon                                                                  !/*ha        254*/
  550 continue                                                                  !/*ha        255*/
      xt=betarn(0.5d0,uno)                                                      !/*ha        256*/
      xxt=1.d0-xt                                                               !/*ha        257*/
  560 continue                                                                  !/*ha        258*/
      if (ibproj) 570,770,690                                                   !/*ha        259*/
  570 continue                                                                  !/*ha        260*/
      iiff1=-iff1                                                               !/*ha        261*/
      iiff2=-iff2                                                               !/*ha        262*/
      if (iiff1.eq.ifb1) go to 580                                              !/*ha        263*/
      if (iiff1.eq.ifb2) go to 590                                              !/*ha        264*/
      if (iiff2.eq.ifb1) go to 600                                              !/*ha        265*/
      if (iiff2.eq.ifb2) go to 610                                              !/*ha        266*/
  580 continue                                                                  !/*ha        267*/
      iiff2=iabs(iff2)                                                          !/*ha        268*/
      ifps=imps(iiff2,ifb2)                                                     !/*ha        269*/
      ifv=imve(iiff2,ifb2)                                                      !/*ha        270*/
      go to 620                                                                 !/*ha        271*/
  590 continue                                                                  !/*ha        272*/
      iiff2=iabs(iff2)                                                          !/*ha        273*/
      ifps=imps(iiff2,ifb1)                                                     !/*ha        274*/
      ifv=imve(iiff2,ifb1)                                                      !/*ha        275*/
      go to 620                                                                 !/*ha        276*/
  600 continue                                                                  !/*ha        277*/
      iiff1=iabs(iff1)                                                          !/*ha        278*/
      ifps=imps(iiff1,ifb2)                                                     !/*ha        279*/
      ifv=imve(iiff1,ifb2)                                                      !/*ha        280*/
      go to 620                                                                 !/*ha        281*/
  610 continue                                                                  !/*ha        282*/
      iiff1=iabs(iiff1)                                                         !/*ha        283*/
      ifps=imps(iiff1,ifb1)                                                     !/*ha        284*/
      ifv=imve(iiff1,ifb1)                                                      !/*ha        285*/
  620 continue                                                                  !/*ha        286*/
      amfps=am(ifps)                                                            !/*ha        287*/
      amfv=am(ifv)                                                              !/*ha        288*/
      nnch1=0                                                                   !/*ha        289*/
      amff=2.3d0                                                                !/*ha        290*/
      xsq=sqrt(xxp*xxt)                                                         !/*ha        291*/
      amch1=umo*xsq                                                             !/*ha        292*/
      aaps=ifps                                                                 !/*ha        293*/
      aav=ifv                                                                   !/*ha        294*/
      if (amch1.lt.amfps) go to 510                                             !/*ha        295*/
      if (amch1.gt.amfv) go to 630                                              !/*ha        296*/
      amch1=amfps                                                               !/*ha        297*/
      nnch1=-1                                                                  !/*ha        298*/
      xsq=amfps/umo                                                             !/*ha        299*/
      xxp=xsq**2/xxt                                                            !/*ha        300*/
      xp=1.d0-xxp                                                               !/*ha        301*/
      go to 640                                                                 !/*ha        302*/
  630 continue                                                                  !/*ha        303*/
      if (amch1.gt.amff) go to 640                                              !/*ha        304*/
      amch1=amfv                                                                !/*ha        305*/
      nnch1=1                                                                   !/*ha        306*/
      xsq=amfv/umo                                                              !/*ha        307*/
      xxp=xsq**2/xxt                                                            !/*ha        308*/
      xp=1.d0-xxp                                                               !/*ha        309*/
      go to 640                                                                 !/*ha        310*/
  640 continue                                                                  !/*ha        311*/
      go to 650                                                                 !/*ha        312*/
  650 continue                                                                  !/*ha        313*/
      iibf=iabs(ibf)                                                            !/*ha        314*/
      ibps=imps(iibf,ibb)                                                       !/*ha        315*/
      ibv=imve(iibf,ibb)                                                        !/*ha        316*/
      ambps=am(ibps)                                                            !/*ha        317*/
      ambv=am(ibv)                                                              !/*ha        318*/
      nnch2=0                                                                   !/*ha        319*/
      ambb=ambv+0.3d0                                                           !/*ha        320*/
      aaps=ibps                                                                 !/*ha        321*/
      aav=ibv                                                                   !/*ha        322*/
      xxsq=sqrt(xp*xt)                                                          !/*ha        323*/
      amch2=umo*xxsq                                                            !/*ha        324*/
      if (amch2.lt.ambps) go to 510                                             !/*ha        325*/
      if (amch2.gt.ambv) go to 660                                              !/*ha        326*/
      amch2=ambps                                                               !/*ha        327*/
      nnch2=-1                                                                  !/*ha        328*/
      xxsq=ambps/umo                                                            !/*ha        329*/
      go to 670                                                                 !/*ha        330*/
  660 continue                                                                  !/*ha        331*/
      if (amch2.gt.ambb) go to 680                                              !/*ha        332*/
      amch2=ambv                                                                !/*ha        333*/
      nnch2=1                                                                   !/*ha        334*/
      xxsq=ambv/umo                                                             !/*ha        335*/
      go to 670                                                                 !/*ha        336*/
  670 continue                                                                  !/*ha        337*/
      xxsq2=xxsq*xxsq                                                           !/*ha        338*/
      if (nnch1.ne.0) then                                                      !/*ha        339*/
        xsq2=xsq*xsq                                                            !/*ha        340*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        341*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        342*/
        ssum=sqrt(help+1.d0+4.d0*xxsq2)                                         !/*ha        343*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        344*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        345*/
        if (xp.gt.xt) then                                                      !/*ha        346*/
          xp=alpha                                                              !/*ha        347*/
          xt=beta                                                               !/*ha        348*/
        else                                                                    !/*ha        349*/
          xt=alpha                                                              !/*ha        350*/
          xp=beta                                                               !/*ha        351*/
        endif                                                                   !/*ha        352*/
        xxp=1.d0-xp                                                             !/*ha        353*/
        xxt=1.d0-xt                                                             !/*ha        354*/
      else                                                                      !/*ha        355*/
        ddiff=xp-xt                                                             !/*ha        356*/
        ssum=sqrt(4.d0*xxsq2+ddiff**2)                                          !/*ha        357*/
        xp=(ssum+ddiff)*0.5d0                                                   !/*ha        358*/
        xt=(ssum-ddiff)*0.5d0                                                   !/*ha        359*/
        xxp=1.d0-xp                                                             !/*ha        360*/
        xxt=1.d0-xt                                                             !/*ha        361*/
        xsq=sqrt(xxp*xxt)                                                       !/*ha        362*/
        amch1=xsq*umo                                                           !/*ha        363*/
      endif                                                                     !/*ha        364*/
  680 continue                                                                  !/*ha        365*/
      pch1=umo*(xxp-xxt)*.5d0                                                   !/*ha        366*/
      ech1=umo*(xxp+xxt)*.5d0                                                   !/*ha        367*/
      gamch1=ech1/amch1                                                         !/*ha        368*/
      bgch1=pch1/amch1                                                          !/*ha        369*/
      pch2=umo*(xp-xt)*.5d0                                                     !/*ha        370*/
      ech2=umo*(xp+xt)*.5d0                                                     !/*ha        371*/
      gamch2=ech2/amch2                                                         !/*ha        372*/
      bgch2=pch2/amch2                                                          !/*ha        373*/
      go to 1300                                                                !/*ha        374*/
  690 continue                                                                  !/*ha        375*/
      go to 740                                                                 !/*ha        376*/
  700 continue                                                                  !/*ha        377*/
      call bklass (ifb,iff1,iff2,if8,if10)                                      !/*ha        378*/
      amf8=am(if8)                                                              !/*ha        379*/
      amf10=am(if10)                                                            !/*ha        380*/
      amff=amf10+0.3d0                                                          !/*ha        381*/
      nnch1=0                                                                   !/*ha        382*/
      xsq=sqrt(abs(xxp*xt))                                                     !/*ha        383*/
      amch1=umo*xsq                                                             !/*ha        384*/
      aab8=ib8                                                                  !/*ha        385*/
      aab10=ibio                                                                !/*ha        386*/
      if (amch1.lt.amf8) go to 510                                              !/*ha        387*/
      if (amch1.gt.amf10) go to 710                                             !/*ha        388*/
      amch1=amf8                                                                !/*ha        389*/
      nnch1=-1                                                                  !/*ha        390*/
      xsq=amf8/umo                                                              !/*ha        391*/
      go to 720                                                                 !/*ha        392*/
  710 continue                                                                  !/*ha        393*/
      if (amch1.gt.amff) go to 730                                              !/*ha        394*/
      amch1=amf10                                                               !/*ha        395*/
      nnch1=1                                                                   !/*ha        396*/
      xsq=amf10/umo                                                             !/*ha        397*/
      go to 720                                                                 !/*ha        398*/
  720 continue                                                                  !/*ha        399*/
      xsq2=xsq*xsq                                                              !/*ha        400*/
      if (nnch2.ne.0) then                                                      !/*ha        401*/
        xxsq2=xxsq*xxsq                                                         !/*ha        402*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        403*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        404*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        405*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        406*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        407*/
        if (xxp.gt.xt) then                                                     !/*ha        408*/
          xxp=alpha                                                             !/*ha        409*/
          xt=beta                                                               !/*ha        410*/
        else                                                                    !/*ha        411*/
          xt=alpha                                                              !/*ha        412*/
          xxp=beta                                                              !/*ha        413*/
        endif                                                                   !/*ha        414*/
        xp=1.d0-xxp                                                             !/*ha        415*/
        xxt=1.d0-xt                                                             !/*ha        416*/
      else                                                                      !/*ha        417*/
        ddiff=xxp-xt                                                            !/*ha        418*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        419*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        420*/
        xt=(ssum-ddiff)*0.5d0                                                   !/*ha        421*/
        xp=1.d0-xxp                                                             !/*ha        422*/
        xxt=1.d0-xt                                                             !/*ha        423*/
        xxsq=sqrt(xp*xxt)                                                       !/*ha        424*/
        amch2=xxsq*umo                                                          !/*ha        425*/
      endif                                                                     !/*ha        426*/
  730 continue                                                                  !/*ha        427*/
      pch1=umo*(xxp-xt)*.5d0                                                    !/*ha        428*/
      ech1=umo*(xxp+xt)*.5d0                                                    !/*ha        429*/
      gamch1=ech1/amch1                                                         !/*ha        430*/
      bgch1=pch1/amch1                                                          !/*ha        431*/
      pch2=umo*(xp-xxt)*.5d0                                                    !/*ha        432*/
      ech2=umo*(xp+xxt)*.5d0                                                    !/*ha        433*/
      gamch2=ech2/amch2                                                         !/*ha        434*/
      bgch2=pch2/amch2                                                          !/*ha        435*/
      go to 1300                                                                !/*ha        436*/
  740 continue                                                                  !/*ha        437*/
      call bklass (ibf,ibb1,ibb2,ib8,ibio)                                      !/*ha        438*/
      nnch2=0                                                                   !/*ha        439*/
      amb8=am(ib8)                                                              !/*ha        440*/
      amb10=am(ibio)                                                            !/*ha        441*/
      ambb=amb10+0.3d0                                                          !/*ha        442*/
      xxsq=sqrt(xp*xxt)                                                         !/*ha        443*/
      amch2=umo*xxsq                                                            !/*ha        444*/
      aab8=ib8                                                                  !/*ha        445*/
      aab10=ibio                                                                !/*ha        446*/
      if (amch2.lt.amb8) go to 510                                              !/*ha        447*/
      if (amch2.gt.amb10) go to 750                                             !/*ha        448*/
      amch2=amb8                                                                !/*ha        449*/
      nnch2=-1                                                                  !/*ha        450*/
      xxsq=amb8/umo                                                             !/*ha        451*/
      xp=xxsq**2/xxt                                                            !/*ha        452*/
      xxp=1.d0-xp                                                               !/*ha        453*/
      go to 760                                                                 !/*ha        454*/
  750 continue                                                                  !/*ha        455*/
      if (amch2.gt.ambb) go to 760                                              !/*ha        456*/
      amch2=amb10                                                               !/*ha        457*/
      nnch2=1                                                                   !/*ha        458*/
      xxsq=amb10/umo                                                            !/*ha        459*/
      xp=xxsq**2/xxt                                                            !/*ha        460*/
      xxp=1.d0-xp                                                               !/*ha        461*/
  760 continue                                                                  !/*ha        462*/
      go to 700                                                                 !/*ha        463*/
  770 continue                                                                  !/*ha        464*/
      if (ibtarg) 1140,950,780                                                  !/*ha        465*/
  780 continue                                                                  !/*ha        466*/
      go to (790,870), isam3                                                    !/*ha        467*/
  790 continue                                                                  !/*ha        468*/
      go to 840                                                                 !/*ha        469*/
  800 continue                                                                  !/*ha        470*/
      call bklass (iff,ifb1,ifb2,if8,ifio)                                      !/*ha        471*/
      amf8=am(if8)                                                              !/*ha        472*/
      amf10=am(ifio)                                                            !/*ha        473*/
      nnch1=0                                                                   !/*ha        474*/
      amff=amf10+0.3d0                                                          !/*ha        475*/
      xsq=sqrt(xxp*xxt)                                                         !/*ha        476*/
      amch1=umo*xsq                                                             !/*ha        477*/
      aa8=if8                                                                   !/*ha        478*/
      aa10=ifio                                                                 !/*ha        479*/
      if (amch1.lt.amf8) go to 510                                              !/*ha        480*/
      if (amch1.gt.amf10) go to 810                                             !/*ha        481*/
      amch1=amf8                                                                !/*ha        482*/
      nnch1=-1                                                                  !/*ha        483*/
      xsq=amf8/umo                                                              !/*ha        484*/
      go to 820                                                                 !/*ha        485*/
  810 continue                                                                  !/*ha        486*/
      if (amch1.gt.amff) go to 830                                              !/*ha        487*/
      amch1=amf10                                                               !/*ha        488*/
      nnch1=1                                                                   !/*ha        489*/
      xsq=amf10/umo                                                             !/*ha        490*/
      go to 820                                                                 !/*ha        491*/
  820 continue                                                                  !/*ha        492*/
      xsq2=xsq*xsq                                                              !/*ha        493*/
      if (nnch2.ne.0) then                                                      !/*ha        494*/
        xxsq2=xxsq*xxsq                                                         !/*ha        495*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        496*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        497*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        498*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        499*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        500*/
        if (xxp.gt.xxt) then                                                    !/*ha        501*/
          xxp=alpha                                                             !/*ha        502*/
          xxt=beta                                                              !/*ha        503*/
        else                                                                    !/*ha        504*/
          xxt=alpha                                                             !/*ha        505*/
          xxp=beta                                                              !/*ha        506*/
        endif                                                                   !/*ha        507*/
        xp=1.d0-xxp                                                             !/*ha        508*/
        xt=1.d0-xxt                                                             !/*ha        509*/
      else                                                                      !/*ha        510*/
        ddiff=xxp-xxt                                                           !/*ha        511*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        512*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        513*/
        xxt=(ssum-ddiff)*0.5d0                                                  !/*ha        514*/
        xp=1.d0-xxp                                                             !/*ha        515*/
        xt=1.d0-xxt                                                             !/*ha        516*/
        xxsq=sqrt(xp*xt)                                                        !/*ha        517*/
        amch2=xxsq*umo                                                          !/*ha        518*/
      endif                                                                     !/*ha        519*/
  830 continue                                                                  !/*ha        520*/
      pch1=umo*(xxp-xxt)*.5d0                                                   !/*ha        521*/
      ech1=umo*(xxp+xxt)*.5d0                                                   !/*ha        522*/
      gamch1=ech1/amch1                                                         !/*ha        523*/
      bgch1=pch1/amch1                                                          !/*ha        524*/
      pch2=umo*(xp-xt)*.5d0                                                     !/*ha        525*/
      ech2=umo*(xp+xt)*.5d0                                                     !/*ha        526*/
      gamch2=ech2/amch2                                                         !/*ha        527*/
      bgch2=pch2/amch2                                                          !/*ha        528*/
      go to 1300                                                                !/*ha        529*/
  840 continue                                                                  !/*ha        530*/
      iibf=iabs(ibf)                                                            !/*ha        531*/
      ibps=imps(iibf,ibb)                                                       !/*ha        532*/
      ibv=imve(iibf,ibb)                                                        !/*ha        533*/
      ambps=am(ibps)                                                            !/*ha        534*/
      ambv=am(ibv)                                                              !/*ha        535*/
      nnch2=0                                                                   !/*ha        536*/
      ambb=ambv+0.3d0                                                           !/*ha        537*/
      xxsq=sqrt(xp*xt)                                                          !/*ha        538*/
      amch2=umo*xxsq                                                            !/*ha        539*/
      aaps=ibps                                                                 !/*ha        540*/
      aav=ibv                                                                   !/*ha        541*/
      if (amch2.lt.ambps) go to 510                                             !/*ha        542*/
      if (amch2.gt.ambv) go to 850                                              !/*ha        543*/
      amch2=ambps                                                               !/*ha        544*/
      nnch2=-1                                                                  !/*ha        545*/
      xxsq=ambps/umo                                                            !/*ha        546*/
      xt=xxsq**2/xp                                                             !/*ha        547*/
      xxt=1.d0-xt                                                               !/*ha        548*/
      go to 860                                                                 !/*ha        549*/
  850 continue                                                                  !/*ha        550*/
      if (amch2.gt.ambb) go to 860                                              !/*ha        551*/
      amch2=ambv                                                                !/*ha        552*/
      nnch2=1                                                                   !/*ha        553*/
      xxsq=ambv/umo                                                             !/*ha        554*/
      xt=xxsq**2/xp                                                             !/*ha        555*/
      xxt=1.d0-xt                                                               !/*ha        556*/
  860 continue                                                                  !/*ha        557*/
      go to 800                                                                 !/*ha        558*/
  870 continue                                                                  !/*ha        559*/
      go to 920                                                                 !/*ha        560*/
  880 continue                                                                  !/*ha        561*/
      iiff=iabs(iff)                                                            !/*ha        562*/
      ifps=imps(iiff,ifb)                                                       !/*ha        563*/
      ifv=imve(iiff,ifb)                                                        !/*ha        564*/
      amfps=am(ifps)                                                            !/*ha        565*/
      amfv=am(ifv)                                                              !/*ha        566*/
      nnch1=0                                                                   !/*ha        567*/
      amff=amfv+0.3d0                                                           !/*ha        568*/
      xsq=sqrt(xxp*xt)                                                          !/*ha        569*/
      amch1=umo*xsq                                                             !/*ha        570*/
      aaps=ifps                                                                 !/*ha        571*/
      aav=ifv                                                                   !/*ha        572*/
      if (amch1.lt.amfps) go to 510                                             !/*ha        573*/
      if (amch1.gt.amfv) go to 890                                              !/*ha        574*/
      amch1=amfps                                                               !/*ha        575*/
      nnch1=-1                                                                  !/*ha        576*/
      xsq=amfps/umo                                                             !/*ha        577*/
      go to 900                                                                 !/*ha        578*/
  890 continue                                                                  !/*ha        579*/
      if (amch1.gt.amff) go to 910                                              !/*ha        580*/
      amch1=amfv                                                                !/*ha        581*/
      nnch1=1                                                                   !/*ha        582*/
      xsq=amfv/umo                                                              !/*ha        583*/
      go to 900                                                                 !/*ha        584*/
  900 continue                                                                  !/*ha        585*/
      xsq2=xsq*xsq                                                              !/*ha        586*/
      if (nnch2.ne.0) then                                                      !/*ha        587*/
        xxsq2=xxsq*xxsq                                                         !/*ha        588*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        589*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        590*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        591*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        592*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        593*/
        if (xxp.gt.xt) then                                                     !/*ha        594*/
          xxp=alpha                                                             !/*ha        595*/
          xt=beta                                                               !/*ha        596*/
        else                                                                    !/*ha        597*/
          xt=alpha                                                              !/*ha        598*/
          xxp=beta                                                              !/*ha        599*/
        endif                                                                   !/*ha        600*/
        xp=1.d0-xxp                                                             !/*ha        601*/
        xxt=1.d0-xt                                                             !/*ha        602*/
      else                                                                      !/*ha        603*/
        ddiff=xxp-xt                                                            !/*ha        604*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        605*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        606*/
        xt=(ssum-ddiff)*0.5d0                                                   !/*ha        607*/
        xp=1.d0-xxp                                                             !/*ha        608*/
        xxt=1.d0-xt                                                             !/*ha        609*/
        xxsq=sqrt(xp*xxt)                                                       !/*ha        610*/
        amch2=xxsq*umo                                                          !/*ha        611*/
      endif                                                                     !/*ha        612*/
  910 continue                                                                  !/*ha        613*/
      pch1=umo*(xxp-xt)*.5d0                                                    !/*ha        614*/
      ech1=umo*(xxp+xt)*.5d0                                                    !/*ha        615*/
      gamch1=ech1/amch1                                                         !/*ha        616*/
      bgch1=pch1/amch1                                                          !/*ha        617*/
      pch2=umo*(xp-xxt)*.5d0                                                    !/*ha        618*/
      ech2=umo*(xp+xxt)*.5d0                                                    !/*ha        619*/
      gamch2=ech2/amch2                                                         !/*ha        620*/
      bgch2=pch2/amch2                                                          !/*ha        621*/
      go to 1300                                                                !/*ha        622*/
  920 continue                                                                  !/*ha        623*/
      call bklass (ibf,ibb1,ibb2,ib8,ibio)                                      !/*ha        624*/
      amb8=am(ib8)                                                              !/*ha        625*/
      amb10=am(ibio)                                                            !/*ha        626*/
      nnch2=0                                                                   !/*ha        627*/
      ambb=amb10+0.3d0                                                          !/*ha        628*/
      xxsq=sqrt(xp*xxt)                                                         !/*ha        629*/
      amch2=umo*xxsq                                                            !/*ha        630*/
      aa8=ib8                                                                   !/*ha        631*/
      aa10=ibio                                                                 !/*ha        632*/
      if (amch2.lt.amb8) go to 510                                              !/*ha        633*/
      if (amch2.gt.amb10) go to 930                                             !/*ha        634*/
      amch2=amb8                                                                !/*ha        635*/
      nnch2=-1                                                                  !/*ha        636*/
      xxsq=amb8/umo                                                             !/*ha        637*/
      xxt=xxsq**2/xp                                                            !/*ha        638*/
      xt=1.d0-xxt                                                               !/*ha        639*/
      go to 940                                                                 !/*ha        640*/
  930 continue                                                                  !/*ha        641*/
      if (amch2.gt.ambb) go to 940                                              !/*ha        642*/
      amch2=amb10                                                               !/*ha        643*/
      nnch2=1                                                                   !/*ha        644*/
      xxsq=amb10/umo                                                            !/*ha        645*/
      xxt=xxsq**2/xp                                                            !/*ha        646*/
      xt=1.d0-xxt                                                               !/*ha        647*/
      go to 940                                                                 !/*ha        648*/
  940 continue                                                                  !/*ha        649*/
      go to 880                                                                 !/*ha        650*/
  950 continue                                                                  !/*ha        651*/
      if (rang().le.0.5d0) go to 960                                            !/*ha        652*/
      xt=xxt                                                                    !/*ha        653*/
      xxt=1.d0-xt                                                               !/*ha        654*/
  960 continue                                                                  !/*ha        655*/
      go to (970,1050), isam3                                                   !/*ha        656*/
  970 continue                                                                  !/*ha        657*/
      go to 1020                                                                !/*ha        658*/
  980 continue                                                                  !/*ha        659*/
      iifb=iabs(ifb)                                                            !/*ha        660*/
      ifps=imps(iifb,iff)                                                       !/*ha        661*/
      ifv=imve(iifb,iff)                                                        !/*ha        662*/
      amfps=am(ifps)                                                            !/*ha        663*/
      amfv=am(ifv)                                                              !/*ha        664*/
      nnch1=0                                                                   !/*ha        665*/
      amff=amfv+0.3d0                                                           !/*ha        666*/
      xsq=sqrt(xxp*xxt)                                                         !/*ha        667*/
      amch1=umo*xsq                                                             !/*ha        668*/
      aaps=ifps                                                                 !/*ha        669*/
      aav=ifv                                                                   !/*ha        670*/
      if (amch1.lt.amfps) go to 510                                             !/*ha        671*/
      if (amch1.gt.amfv) go to 990                                              !/*ha        672*/
      amch1=amfps                                                               !/*ha        673*/
      nnch1=-1                                                                  !/*ha        674*/
      xsq=amfps/umo                                                             !/*ha        675*/
      go to 1000                                                                !/*ha        676*/
  990 continue                                                                  !/*ha        677*/
      if (amch1.gt.amff) go to 1010                                             !/*ha        678*/
      amch1=amfv                                                                !/*ha        679*/
      nnch1=1                                                                   !/*ha        680*/
      xsq=amfv/umo                                                              !/*ha        681*/
      go to 1000                                                                !/*ha        682*/
 1000 continue                                                                  !/*ha        683*/
      xsq2=xsq*xsq                                                              !/*ha        684*/
      if (nnch2.ne.0) then                                                      !/*ha        685*/
        xxsq2=xxsq*xxsq                                                         !/*ha        686*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        687*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        688*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        689*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        690*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        691*/
        if (xxp.gt.xxt) then                                                    !/*ha        692*/
          xxp=alpha                                                             !/*ha        693*/
          xxt=beta                                                              !/*ha        694*/
        else                                                                    !/*ha        695*/
          xxt=alpha                                                             !/*ha        696*/
          xxp=beta                                                              !/*ha        697*/
        endif                                                                   !/*ha        698*/
        xp=1.d0-xxp                                                             !/*ha        699*/
        xt=1.d0-xxt                                                             !/*ha        700*/
      else                                                                      !/*ha        701*/
        ddiff=xxp-xxt                                                           !/*ha        702*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        703*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        704*/
        xxt=(ssum-ddiff)*0.5d0                                                  !/*ha        705*/
        xp=1.d0-xxp                                                             !/*ha        706*/
        xt=1.d0-xxt                                                             !/*ha        707*/
        xxsq=sqrt(xp*xt)                                                        !/*ha        708*/
        amch2=xxsq*umo                                                          !/*ha        709*/
      endif                                                                     !/*ha        710*/
 1010 continue                                                                  !/*ha        711*/
      pch1=umo*(xxp-xxt)*.5d0                                                   !/*ha        712*/
      ech1=umo*(xxp+xxt)*.5d0                                                   !/*ha        713*/
      gamch1=ech1/amch1                                                         !/*ha        714*/
      bgch1=pch1/amch1                                                          !/*ha        715*/
      pch2=umo*(xp-xt)*.5d0                                                     !/*ha        716*/
      ech2=umo*(xp+xt)*.5d0                                                     !/*ha        717*/
      gamch2=ech2/amch2                                                         !/*ha        718*/
      bgch2=pch2/amch2                                                          !/*ha        719*/
      go to 1130                                                                !/*ha        720*/
 1020 continue                                                                  !/*ha        721*/
      iibf=iabs(ibf)                                                            !/*ha        722*/
      ibps=imps(iibf,ibb)                                                       !/*ha        723*/
      ibv=imve(iibf,ibb)                                                        !/*ha        724*/
      ambps=am(ibps)                                                            !/*ha        725*/
      ambv=am(ibv)                                                              !/*ha        726*/
      nnch2=0                                                                   !/*ha        727*/
      ambb=ambv+0.3d0                                                           !/*ha        728*/
      xxsq=sqrt(xp*xt)                                                          !/*ha        729*/
      amch2=umo*xxsq                                                            !/*ha        730*/
      aaps=ibps                                                                 !/*ha        731*/
      aav=ibv                                                                   !/*ha        732*/
      if (amch2.lt.ambps) go to 510                                             !/*ha        733*/
      if (amch2.gt.ambv) go to 1030                                             !/*ha        734*/
      amch2=ambps                                                               !/*ha        735*/
      nnch2=-1                                                                  !/*ha        736*/
      xxsq=ambps/umo                                                            !/*ha        737*/
      xt=xxsq**2/xp                                                             !/*ha        738*/
      xxt=1.d0-xt                                                               !/*ha        739*/
      go to 1040                                                                !/*ha        740*/
 1030 continue                                                                  !/*ha        741*/
      if (amch2.gt.ambb) go to 1040                                             !/*ha        742*/
      amch2=ambv                                                                !/*ha        743*/
      nnch2=1                                                                   !/*ha        744*/
      xxsq=ambv/umo                                                             !/*ha        745*/
      xt=xxsq**2/xp                                                             !/*ha        746*/
      xxt=1.d0-xt                                                               !/*ha        747*/
 1040 continue                                                                  !/*ha        748*/
      go to 980                                                                 !/*ha        749*/
 1050 continue                                                                  !/*ha        750*/
      go to 1100                                                                !/*ha        751*/
 1060 continue                                                                  !/*ha        752*/
      iiff=iabs(iff)                                                            !/*ha        753*/
      ifps=imps(iiff,ifb)                                                       !/*ha        754*/
      ifv=imve(iiff,ifb)                                                        !/*ha        755*/
      amfps=am(ifps)                                                            !/*ha        756*/
      amfv=am(ifv)                                                              !/*ha        757*/
      amff=amfv+0.3d0                                                           !/*ha        758*/
      xsq=sqrt(xxp*xt)                                                          !/*ha        759*/
      amch1=umo*xsq                                                             !/*ha        760*/
      aaps=ifps                                                                 !/*ha        761*/
      aav=ifv                                                                   !/*ha        762*/
      if (amch1.lt.amfps) go to 510                                             !/*ha        763*/
      if (amch1.gt.amfv) go to 1070                                             !/*ha        764*/
      amch1=amfps                                                               !/*ha        765*/
      nnch1=-1                                                                  !/*ha        766*/
      xsq=amfps/umo                                                             !/*ha        767*/
      go to 1080                                                                !/*ha        768*/
 1070 continue                                                                  !/*ha        769*/
      if (amch1.gt.amff) go to 1090                                             !/*ha        770*/
      amch1=amfv                                                                !/*ha        771*/
      nnch1=1                                                                   !/*ha        772*/
      xsq=amfv/umo                                                              !/*ha        773*/
      go to 1080                                                                !/*ha        774*/
 1080 continue                                                                  !/*ha        775*/
      xsq2=xsq*xsq                                                              !/*ha        776*/
      if (nnch2.ne.0) then                                                      !/*ha        777*/
        xxsq2=xxsq*xxsq                                                         !/*ha        778*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        779*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        780*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        781*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        782*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        783*/
        if (xxp.gt.xt) then                                                     !/*ha        784*/
          xxp=alpha                                                             !/*ha        785*/
          xt=beta                                                               !/*ha        786*/
        else                                                                    !/*ha        787*/
          xt=alpha                                                              !/*ha        788*/
          xxp=beta                                                              !/*ha        789*/
        endif                                                                   !/*ha        790*/
        xp=1.d0-xxp                                                             !/*ha        791*/
        xxt=1.d0-xt                                                             !/*ha        792*/
      else                                                                      !/*ha        793*/
        ddiff=xxp-xt                                                            !/*ha        794*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        795*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        796*/
        xt=(ssum-ddiff)*0.5d0                                                   !/*ha        797*/
        xp=1.d0-xxp                                                             !/*ha        798*/
        xxt=1.d0-xt                                                             !/*ha        799*/
        xxsq=sqrt(xp*xxt)                                                       !/*ha        800*/
        amch2=xxsq*umo                                                          !/*ha        801*/
      endif                                                                     !/*ha        802*/
 1090 continue                                                                  !/*ha        803*/
      pch1=umo*(xxp-xt)*.5d0                                                    !/*ha        804*/
      ech1=umo*(xxp+xt)*.5d0                                                    !/*ha        805*/
      gamch1=ech1/amch1                                                         !/*ha        806*/
      bgch1=pch1/amch1                                                          !/*ha        807*/
      pch2=umo*(xp-xxt)*.5d0                                                    !/*ha        808*/
      ech2=umo*(xp+xxt)*.5d0                                                    !/*ha        809*/
      gamch2=ech2/amch2                                                         !/*ha        810*/
      bgch2=pch2/amch2                                                          !/*ha        811*/
      go to 1130                                                                !/*ha        812*/
 1100 continue                                                                  !/*ha        813*/
      iibb=iabs(ibb)                                                            !/*ha        814*/
      ibps=imps(iibb,ibf)                                                       !/*ha        815*/
      ibv=imve(iibb,ibf)                                                        !/*ha        816*/
      ambps=am(ibps)                                                            !/*ha        817*/
      ambv=am(ibv)                                                              !/*ha        818*/
      nnch2=0                                                                   !/*ha        819*/
      ambb=ambv+0.3d0                                                           !/*ha        820*/
      xxsq=sqrt(xp*xxt)                                                         !/*ha        821*/
      amch2=umo*xxsq                                                            !/*ha        822*/
      aaps=ibps                                                                 !/*ha        823*/
      aav=ibv                                                                   !/*ha        824*/
      if (amch2.lt.ambps) go to 510                                             !/*ha        825*/
      if (amch2.gt.ambv) go to 1110                                             !/*ha        826*/
      amch2=ambps                                                               !/*ha        827*/
      nnch2=-1                                                                  !/*ha        828*/
      xxsq=ambps/umo                                                            !/*ha        829*/
      xxt=xxsq**2/xp                                                            !/*ha        830*/
      xt=1.d0-xxt                                                               !/*ha        831*/
      go to 1120                                                                !/*ha        832*/
 1110 continue                                                                  !/*ha        833*/
      if (amch2.gt.ambb) go to 1120                                             !/*ha        834*/
      amch2=ambv                                                                !/*ha        835*/
      nnch2=1                                                                   !/*ha        836*/
      xxsq=ambv/umo                                                             !/*ha        837*/
      xxt=xxsq**2/xp                                                            !/*ha        838*/
      xt=1.d0-xxt                                                               !/*ha        839*/
      go to 1120                                                                !/*ha        840*/
 1120 continue                                                                  !/*ha        841*/
      go to 1060                                                                !/*ha        842*/
 1130 continue                                                                  !/*ha        843*/
      go to 1300                                                                !/*ha        844*/
 1140 continue                                                                  !/*ha        845*/
      go to (1230,1150), isam3                                                  !/*ha        846*/
 1150 continue                                                                  !/*ha        847*/
      go to 1200                                                                !/*ha        848*/
 1160 continue                                                                  !/*ha        849*/
      call bklass (ibf,ibb1,ibb2,if8,ifio)                                      !/*ha        850*/
      amf8=am(if8)                                                              !/*ha        851*/
      amf10=am(ifio)                                                            !/*ha        852*/
      nnch1=0                                                                   !/*ha        853*/
      amff=amf10+0.3d0                                                          !/*ha        854*/
      xsq=sqrt(xxp*xxt)                                                         !/*ha        855*/
      amch1=umo*xsq                                                             !/*ha        856*/
      aa8=if8                                                                   !/*ha        857*/
      aa10=ifio                                                                 !/*ha        858*/
      if (amch1.lt.amf8) go to 510                                              !/*ha        859*/
      if (amch1.gt.amf10) go to 1170                                            !/*ha        860*/
      amch1=amf8                                                                !/*ha        861*/
      nnch1=-1                                                                  !/*ha        862*/
      xsq=amf8/umo                                                              !/*ha        863*/
      go to 1180                                                                !/*ha        864*/
 1170 continue                                                                  !/*ha        865*/
      if (amch1.gt.amff) go to 1190                                             !/*ha        866*/
      amch1=amf10                                                               !/*ha        867*/
      nnch1=1                                                                   !/*ha        868*/
      xsq=amf10/umo                                                             !/*ha        869*/
      go to 1180                                                                !/*ha        870*/
 1180 continue                                                                  !/*ha        871*/
      xsq2=xsq*xsq                                                              !/*ha        872*/
      if (nnch2.ne.0) then                                                      !/*ha        873*/
        xxsq2=xxsq*xxsq                                                         !/*ha        874*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        875*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        876*/
        ssum=sqrt(help+1.d0+4.d0*xsq2)                                          !/*ha        877*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        878*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        879*/
        if (xxp.gt.xxt) then                                                    !/*ha        880*/
          xxp=alpha                                                             !/*ha        881*/
          xxt=beta                                                              !/*ha        882*/
        else                                                                    !/*ha        883*/
          xxt=alpha                                                             !/*ha        884*/
          xxp=beta                                                              !/*ha        885*/
        endif                                                                   !/*ha        886*/
        xp=1.d0-xxp                                                             !/*ha        887*/
        xt=1.d0-xxt                                                             !/*ha        888*/
      else                                                                      !/*ha        889*/
        ddiff=xxp-xxt                                                           !/*ha        890*/
        ssum=sqrt(4.d0*xsq2+ddiff**2)                                           !/*ha        891*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha        892*/
        xxt=(ssum-ddiff)*0.5d0                                                  !/*ha        893*/
        xp=1.d0-xxp                                                             !/*ha        894*/
        xt=1.d0-xxt                                                             !/*ha        895*/
        xxsq=sqrt(xp*xt)                                                        !/*ha        896*/
        amch2=xxsq*umo                                                          !/*ha        897*/
      endif                                                                     !/*ha        898*/
 1190 continue                                                                  !/*ha        899*/
      pch1=umo*(xxp-xxt)*.5d0                                                   !/*ha        900*/
      ech1=umo*(xxp+xxt)*.5d0                                                   !/*ha        901*/
      gamch1=ech1/amch1                                                         !/*ha        902*/
      bgch1=pch1/amch1                                                          !/*ha        903*/
      pch2=umo*(xp-xt)*.5d0                                                     !/*ha        904*/
      ech2=umo*(xp+xt)*.5d0                                                     !/*ha        905*/
      gamch2=ech2/amch2                                                         !/*ha        906*/
      bgch2=pch2/amch2                                                          !/*ha        907*/
      go to 1300                                                                !/*ha        908*/
 1200 continue                                                                  !/*ha        909*/
      iifb=iabs(ifb)                                                            !/*ha        910*/
      ibps=imps(iifb,iff)                                                       !/*ha        911*/
      ibv=imve(iifb,iff)                                                        !/*ha        912*/
      ambps=am(ibps)                                                            !/*ha        913*/
      ambv=am(ibv)                                                              !/*ha        914*/
      nnch2=0                                                                   !/*ha        915*/
      ambb=ambv+0.3d0                                                           !/*ha        916*/
      xxsq=sqrt(xp*xt)                                                          !/*ha        917*/
      amch2=umo*xxsq                                                            !/*ha        918*/
      aaps=ibps                                                                 !/*ha        919*/
      aav=ibv                                                                   !/*ha        920*/
      if (amch2.lt.ambps) go to 510                                             !/*ha        921*/
      if (amch2.gt.ambv) go to 1210                                             !/*ha        922*/
      amch2=ambps                                                               !/*ha        923*/
      nnch2=-1                                                                  !/*ha        924*/
      xxsq=ambps/umo                                                            !/*ha        925*/
      xt=xxsq**2/xp                                                             !/*ha        926*/
      xxt=1.d0-xt                                                               !/*ha        927*/
      go to 1220                                                                !/*ha        928*/
 1210 continue                                                                  !/*ha        929*/
      if (amch2.gt.ambb) go to 1220                                             !/*ha        930*/
      amch2=ambv                                                                !/*ha        931*/
      nnch2=1                                                                   !/*ha        932*/
      xxsq=ambv/umo                                                             !/*ha        933*/
      xt=xxsq**2/xp                                                             !/*ha        934*/
      xxt=1.d0-xt                                                               !/*ha        935*/
 1220 continue                                                                  !/*ha        936*/
      go to 1160                                                                !/*ha        937*/
 1230 continue                                                                  !/*ha        938*/
      iibb=iabs(ibb)                                                            !/*ha        939*/
      ifps=imps(iibb,ibf)                                                       !/*ha        940*/
      ifv=imve(iibb,ibf)                                                        !/*ha        941*/
      amfps=am(ifps)                                                            !/*ha        942*/
      amfv=am(ifv)                                                              !/*ha        943*/
      nnch1=0                                                                   !/*ha        944*/
      amff=amfv+0.3d0                                                           !/*ha        945*/
      xsq=sqrt(xp*xt)                                                           !/*ha        946*/
      amch1=umo*xsq                                                             !/*ha        947*/
      aaps=ifps                                                                 !/*ha        948*/
      aav=ifv                                                                   !/*ha        949*/
      if (amch1.lt.amfps) go to 510                                             !/*ha        950*/
      if (amch1.gt.amfv) go to 1240                                             !/*ha        951*/
      amch1=amfps                                                               !/*ha        952*/
      nnch1=-1                                                                  !/*ha        953*/
      xsq=amfps/umo                                                             !/*ha        954*/
      xp=xsq**2/xt                                                              !/*ha        955*/
      xxp=1.d0-xp                                                               !/*ha        956*/
      go to 1250                                                                !/*ha        957*/
 1240 continue                                                                  !/*ha        958*/
      if (amch1.gt.amff) go to 1250                                             !/*ha        959*/
      amch1=amfv                                                                !/*ha        960*/
      nnch1=1                                                                   !/*ha        961*/
      xsq=amfv/umo                                                              !/*ha        962*/
      xp=xsq**2/xt                                                              !/*ha        963*/
      xxp=1.d0-xp                                                               !/*ha        964*/
      go to 1250                                                                !/*ha        965*/
 1250 continue                                                                  !/*ha        966*/
      go to 1260                                                                !/*ha        967*/
 1260 continue                                                                  !/*ha        968*/
      call bklass (iff,ifb1,ifb2,ib8,ibio)                                      !/*ha        969*/
      amb8=am(ib8)                                                              !/*ha        970*/
      amb10=am(ibio)                                                            !/*ha        971*/
      nnch2=0                                                                   !/*ha        972*/
      ambb=amb10+0.3d0                                                          !/*ha        973*/
      xxsq=sqrt(xxp*xxt)                                                        !/*ha        974*/
      amch2=umo*xxsq                                                            !/*ha        975*/
      aa8=ib8                                                                   !/*ha        976*/
      aa10=ibio                                                                 !/*ha        977*/
      if (amch2.lt.amb8) go to 510                                              !/*ha        978*/
      if (amch2.gt.amb10) go to 1270                                            !/*ha        979*/
      amch2=amb8                                                                !/*ha        980*/
      nnch2=-1                                                                  !/*ha        981*/
      xxsq=amb8/umo                                                             !/*ha        982*/
      go to 1280                                                                !/*ha        983*/
 1270 continue                                                                  !/*ha        984*/
      if (amch2.gt.ambb) go to 1290                                             !/*ha        985*/
      amch2=amb10                                                               !/*ha        986*/
      nnch2=1                                                                   !/*ha        987*/
      xxsq=amb10/umo                                                            !/*ha        988*/
      go to 1280                                                                !/*ha        989*/
 1280 continue                                                                  !/*ha        990*/
      xxsq2=xxsq*xxsq                                                           !/*ha        991*/
      if (nnch1.ne.0) then                                                      !/*ha        992*/
        xsq2=xsq*xsq                                                            !/*ha        993*/
        help=xsq2*(xsq2-2.d0)+xxsq2*(xxsq2-2.d0)-2.d0*xsq2*xxsq2                !/*ha        994*/
        ddiff=sqrt(help+1.d0)                                                   !/*ha        995*/
        ssum=sqrt(help+1.d0+4.d0*xxsq2)                                         !/*ha        996*/
        alpha=(ssum+ddiff)*0.5d0                                                !/*ha        997*/
        beta=(ssum-ddiff)*0.5d0                                                 !/*ha        998*/
        if (xxp.gt.xxt) then                                                    !/*ha        999*/
          xxp=alpha                                                             !/*ha       1000*/
          xxt=beta                                                              !/*ha       1001*/
        else                                                                    !/*ha       1002*/
          xxt=alpha                                                             !/*ha       1003*/
          xxp=beta                                                              !/*ha       1004*/
        endif                                                                   !/*ha       1005*/
        xp=1.d0-xxp                                                             !/*ha       1006*/
        xt=1.d0-xxt                                                             !/*ha       1007*/
      else                                                                      !/*ha       1008*/
        ddiff=xxp-xxt                                                           !/*ha       1009*/
        ssum=sqrt(4.d0*xxsq2+ddiff**2)                                          !/*ha       1010*/
        xxp=(ssum+ddiff)*0.5d0                                                  !/*ha       1011*/
        xxt=(ssum-ddiff)*0.5d0                                                  !/*ha       1012*/
        xp=1.d0-xxp                                                             !/*ha       1013*/
        xt=1.d0-xxt                                                             !/*ha       1014*/
        xsq=sqrt(xp*xt)                                                         !/*ha       1015*/
        amch1=xsq*umo                                                           !/*ha       1016*/
      endif                                                                     !/*ha       1017*/
 1290 continue                                                                  !/*ha       1018*/
      pch1=umo*(xp-xt)*.5d0                                                     !/*ha       1019*/
      ech1=umo*(xp+xt)*.5d0                                                     !/*ha       1020*/
      gamch1=ech1/amch1                                                         !/*ha       1021*/
      bgch1=pch1/amch1                                                          !/*ha       1022*/
      pch2=umo*(xxp-xxt)*.5d0                                                   !/*ha       1023*/
      ech2=umo*(xxp+xxt)*.5d0                                                   !/*ha       1024*/
      gamch2=ech2/amch2                                                         !/*ha       1025*/
      bgch2=pch2/amch2                                                          !/*ha       1026*/
      go to 1300                                                                !/*ha       1027*/
 1300 continue                                                                  !/*ha       1028*/
      if (ibproj) 1310,1370,1770                                                !/*ha       1029*/
 1310 continue                                                                  !/*ha       1030*/
      if (nnch1) 1320,1340,1330                                                 !/*ha       1031*/
 1320 continue                                                                  !/*ha       1032*/
      ich1=ifps                                                                 !/*ha       1033*/
      go to 1350                                                                !/*ha       1034*/
 1330 continue                                                                  !/*ha       1035*/
      ich1=ifv                                                                  !/*ha       1036*/
      go to 1350                                                                !/*ha       1037*/
 1340 continue                                                                  !/*ha       1038*/
      iaiff1=iabs(iff1)+6                                                       !/*ha       1039*/
      iaiff2=iabs(iff2)+6                                                       !/*ha       1040*/
      call bamjet (ihad,ifb1,ifb2,iaiff1,iaiff2,amch1,5)                        !/*ha       1041*/
      go to 1360                                                                !/*ha       1042*/
 1350 continue                                                                  !/*ha       1043*/
      call parjet (ihad,ich1)                                                   !/*ha       1044*/
 1360 continue                                                                  !/*ha       1045*/
      go to 1830                                                                !/*ha       1046*/
 1370 continue                                                                  !/*ha       1047*/
      if (ibtarg) 1640,1510,1380                                                !/*ha       1048*/
 1380 continue                                                                  !/*ha       1049*/
      go to (1390,1450), isam3                                                  !/*ha       1050*/
 1390 continue                                                                  !/*ha       1051*/
      if (nnch1) 1400,1420,1410                                                 !/*ha       1052*/
 1400 continue                                                                  !/*ha       1053*/
      ich1=if8                                                                  !/*ha       1054*/
      go to 1430                                                                !/*ha       1055*/
 1410 continue                                                                  !/*ha       1056*/
      ich1=ifio                                                                 !/*ha       1057*/
      go to 1430                                                                !/*ha       1058*/
 1420 continue                                                                  !/*ha       1059*/
      call bamjet (ihad,iff,ifb1,ifb2,iff,amch1,4)                              !/*ha       1060*/
      go to 1440                                                                !/*ha       1061*/
 1430 continue                                                                  !/*ha       1062*/
      call parjet (ihad,ich1)                                                   !/*ha       1063*/
 1440 continue                                                                  !/*ha       1064*/
      go to 1830                                                                !/*ha       1065*/
 1450 continue                                                                  !/*ha       1066*/
      if (nnch1) 1460,1480,1470                                                 !/*ha       1067*/
 1460 continue                                                                  !/*ha       1068*/
      ich1=ifps                                                                 !/*ha       1069*/
      go to 1490                                                                !/*ha       1070*/
 1470 continue                                                                  !/*ha       1071*/
      ich1=ifv                                                                  !/*ha       1072*/
      go to 1490                                                                !/*ha       1073*/
 1480 continue                                                                  !/*ha       1074*/
      iaiff=iabs(iff)+6                                                         !/*ha       1075*/
      call bamjet (ihad,ifb,iaiff,ifb,ifb,amch1,3)                              !/*ha       1076*/
      go to 1500                                                                !/*ha       1077*/
 1490 continue                                                                  !/*ha       1078*/
      call parjet (ihad,ich1)                                                   !/*ha       1079*/
 1500 continue                                                                  !/*ha       1080*/
      go to 1830                                                                !/*ha       1081*/
 1510 continue                                                                  !/*ha       1082*/
      go to (1520,1580), isam3                                                  !/*ha       1083*/
 1520 continue                                                                  !/*ha       1084*/
      if (nnch1) 1530,1550,1540                                                 !/*ha       1085*/
 1530 continue                                                                  !/*ha       1086*/
      ich1=ifps                                                                 !/*ha       1087*/
      go to 1560                                                                !/*ha       1088*/
 1540 continue                                                                  !/*ha       1089*/
      ich1=ifv                                                                  !/*ha       1090*/
      go to 1560                                                                !/*ha       1091*/
 1550 continue                                                                  !/*ha       1092*/
      iaifb=iabs(ifb)+6                                                         !/*ha       1093*/
      call bamjet (ihad,iff,iaifb,ifb2,iff,amch1,3)                             !/*ha       1094*/
      go to 1570                                                                !/*ha       1095*/
 1560 continue                                                                  !/*ha       1096*/
      call parjet (ihad,ich1)                                                   !/*ha       1097*/
 1570 continue                                                                  !/*ha       1098*/
      go to 1830                                                                !/*ha       1099*/
 1580 continue                                                                  !/*ha       1100*/
      if (nnch1) 1590,1610,1600                                                 !/*ha       1101*/
 1590 continue                                                                  !/*ha       1102*/
      ich1=ifps                                                                 !/*ha       1103*/
      go to 1620                                                                !/*ha       1104*/
 1600 continue                                                                  !/*ha       1105*/
      ich1=ifv                                                                  !/*ha       1106*/
      go to 1620                                                                !/*ha       1107*/
 1610 continue                                                                  !/*ha       1108*/
      iaiff=iabs(iff)+6                                                         !/*ha       1109*/
      call bamjet (ihad,ifb,iaiff,ifb,ifb,amch1,3)                              !/*ha       1110*/
      go to 1630                                                                !/*ha       1111*/
 1620 continue                                                                  !/*ha       1112*/
      call parjet (ihad,ich1)                                                   !/*ha       1113*/
 1630 continue                                                                  !/*ha       1114*/
      go to 1830                                                                !/*ha       1115*/
 1640 continue                                                                  !/*ha       1116*/
      go to (1710,1650), isam3                                                  !/*ha       1117*/
 1650 continue                                                                  !/*ha       1118*/
      if (nnch1) 1660,1680,1670                                                 !/*ha       1119*/
 1660 continue                                                                  !/*ha       1120*/
      ich1=if8                                                                  !/*ha       1121*/
      go to 1690                                                                !/*ha       1122*/
 1670 continue                                                                  !/*ha       1123*/
      ich1=ifio                                                                 !/*ha       1124*/
      go to 1690                                                                !/*ha       1125*/
 1680 continue                                                                  !/*ha       1126*/
      iaibf=iabs(ibf)+6                                                         !/*ha       1127*/
      iaibb1=iabs(ibb1)+6                                                       !/*ha       1128*/
      iaibb2=iabs(ibb2)+6                                                       !/*ha       1129*/
      call bamjet (ihad,iaibf,iaibb1,iaibb2,ibb,amch1,4)                        !/*ha       1130*/
      go to 1700                                                                !/*ha       1131*/
 1690 continue                                                                  !/*ha       1132*/
      call parjet (ihad,ich1)                                                   !/*ha       1133*/
 1700 continue                                                                  !/*ha       1134*/
      go to 1830                                                                !/*ha       1135*/
 1710 continue                                                                  !/*ha       1136*/
      if (nnch2) 1720,1740,1730                                                 !/*ha       1137*/
 1720 continue                                                                  !/*ha       1138*/
      ich1=ib8                                                                  !/*ha       1139*/
      go to 1750                                                                !/*ha       1140*/
 1730 continue                                                                  !/*ha       1141*/
      ich1=ibio                                                                 !/*ha       1142*/
      go to 1750                                                                !/*ha       1143*/
 1740 continue                                                                  !/*ha       1144*/
      iaiff=iabs(iff)+6                                                         !/*ha       1145*/
      iaifb1=iabs(ifb1)+6                                                       !/*ha       1146*/
      iaifb2=iabs(ifb2)+6                                                       !/*ha       1147*/
      call bamjet (ihad,iaiff,iaifb1,iaifb2,ifb,amch2,4)                        !/*ha       1148*/
      go to 1760                                                                !/*ha       1149*/
 1750 continue                                                                  !/*ha       1150*/
      call parjet (ihad,ich1)                                                   !/*ha       1151*/
 1760 continue                                                                  !/*ha       1152*/
      go to 1830                                                                !/*ha       1153*/
 1770 continue                                                                  !/*ha       1154*/
      if (nnch1) 1780,1800,1790                                                 !/*ha       1155*/
 1780 continue                                                                  !/*ha       1156*/
      ich1=if8                                                                  !/*ha       1157*/
      go to 1810                                                                !/*ha       1158*/
 1790 continue                                                                  !/*ha       1159*/
      ich1=if10                                                                 !/*ha       1160*/
      go to 1810                                                                !/*ha       1161*/
 1800 continue                                                                  !/*ha       1162*/
      call bamjet (ihad,ifb,iff1,iff2,ifb,amch1,4)                              !/*ha       1163*/
      go to 1820                                                                !/*ha       1164*/
 1810 continue                                                                  !/*ha       1165*/
      call parjet (ihad,ich1)                                                   !/*ha       1166*/
 1820 continue                                                                  !/*ha       1167*/
 1830 continue                                                                  !/*ha       1168*/
      if (ibproj) 1840,1850,1880                                                !/*ha       1169*/
 1840 continue                                                                  !/*ha       1170*/
      go to 1890                                                                !/*ha       1171*/
 1850 continue                                                                  !/*ha       1172*/
      go to (1860,1870), isam3                                                  !/*ha       1173*/
 1860 continue                                                                  !/*ha       1174*/
      go to 1910                                                                !/*ha       1175*/
 1870 continue                                                                  !/*ha       1176*/
      go to 1890                                                                !/*ha       1177*/
 1880 continue                                                                  !/*ha       1178*/
      go to 1890                                                                !/*ha       1179*/
 1890 continue                                                                  !/*ha       1180*/
      do 1900 i=1,ihad                                                          !/*ha       1181*/
      pzf(i)=-pzf(i)                                                            !/*ha       1182*/
 1900 continue                                                                  !/*ha       1183*/
 1910 continue                                                                  !/*ha       1184*/
      iiihad=ihad                                                               !/*ha       1185*/
      es=-2.d0/(b3bamj**2)*log(rang()*rang())                                   !/*ha       1186*/
      hps=sqrt(es*es+2.d0*es*am(1))                                             !/*ha       1187*/
      call sfecfe (sfe,cfe)                                                     !/*ha       1188*/
      ptxch1=hps*cfe                                                            !/*ha       1189*/
      ptych1=hps*sfe                                                            !/*ha       1190*/
      go to 1930                                                                !/*ha       1191*/
 1920 continue                                                                  !/*ha       1192*/
      ptxch1=0.75d0*ptxch1                                                      !/*ha       1193*/
      ptych1=0.75d0*ptych1                                                      !/*ha       1194*/
 1930 continue                                                                  !/*ha       1195*/
      ihad=iiihad                                                               !/*ha       1196*/
      ptxch2=-ptxch1                                                            !/*ha       1197*/
      ptych2=-ptych1                                                            !/*ha       1198*/
      bgch1x=ptxch1/amch1                                                       !/*ha       1199*/
      bgch1y=ptych1/amch1                                                       !/*ha       1200*/
      ach1=bgch1**2-(ptxch1**2+ptych1**2)/amch1**2                              !/*ha       1201*/
      if (ach1.le.0.d0) go to 1920                                              !/*ha       1202*/
      bgch1z=sqrt(ach1)                                                         !/*ha       1203*/
      bgch1z=sign(bgch1z,bgch1)                                                 !/*ha       1204*/
      call lortra (ihad,1,gamch1,bgch1x,bgch1y,bgch1z)                          !/*ha       1205*/
      ihad=iiihad                                                               !/*ha       1206*/
      naux=ihad                                                                 !/*ha       1207*/
      if (ibproj) 1940,2000,2400                                                !/*ha       1208*/
 1940 continue                                                                  !/*ha       1209*/
      if (nnch2) 1950,1970,1960                                                 !/*ha       1210*/
 1950 continue                                                                  !/*ha       1211*/
      ich2=ibps                                                                 !/*ha       1212*/
      go to 1980                                                                !/*ha       1213*/
 1960 continue                                                                  !/*ha       1214*/
      ich2=ibv                                                                  !/*ha       1215*/
      go to 1980                                                                !/*ha       1216*/
 1970 continue                                                                  !/*ha       1217*/
      iaibf=iabs(ibf)+6                                                         !/*ha       1218*/
      call bamjet (ihad,ibb,iaibf,ibb,ibb,amch2,3)                              !/*ha       1219*/
      go to 1990                                                                !/*ha       1220*/
 1980 continue                                                                  !/*ha       1221*/
      call parjet (ihad,ich2)                                                   !/*ha       1222*/
 1990 continue                                                                  !/*ha       1223*/
      go to 2460                                                                !/*ha       1224*/
 2000 continue                                                                  !/*ha       1225*/
      if (ibtarg) 2270,2140,2010                                                !/*ha       1226*/
 2010 continue                                                                  !/*ha       1227*/
      go to (2020,2080), isam3                                                  !/*ha       1228*/
 2020 continue                                                                  !/*ha       1229*/
      if (nnch2) 2030,2050,2040                                                 !/*ha       1230*/
 2030 continue                                                                  !/*ha       1231*/
      ich2=ibps                                                                 !/*ha       1232*/
      go to 2060                                                                !/*ha       1233*/
 2040 continue                                                                  !/*ha       1234*/
      ich2=ibv                                                                  !/*ha       1235*/
      go to 2060                                                                !/*ha       1236*/
 2050 continue                                                                  !/*ha       1237*/
      iaibf=iabs(ibf)+6                                                         !/*ha       1238*/
      call bamjet (ihad,ibb,iaibf,ibb,ibb,amch2,3)                              !/*ha       1239*/
      go to 2070                                                                !/*ha       1240*/
 2060 continue                                                                  !/*ha       1241*/
      call parjet (ihad,ich2)                                                   !/*ha       1242*/
 2070 continue                                                                  !/*ha       1243*/
      go to 2460                                                                !/*ha       1244*/
 2080 continue                                                                  !/*ha       1245*/
      if (nnch2) 2090,2110,2100                                                 !/*ha       1246*/
 2090 continue                                                                  !/*ha       1247*/
      ich2=ib8                                                                  !/*ha       1248*/
      go to 2120                                                                !/*ha       1249*/
 2100 continue                                                                  !/*ha       1250*/
      ich2=ibio                                                                 !/*ha       1251*/
      go to 2120                                                                !/*ha       1252*/
 2110 continue                                                                  !/*ha       1253*/
      call bamjet (ihad,ibf,ibb1,ibb2,ibf,amch2,4)                              !/*ha       1254*/
      go to 2130                                                                !/*ha       1255*/
 2120 continue                                                                  !/*ha       1256*/
      call parjet (ihad,ich2)                                                   !/*ha       1257*/
 2130 continue                                                                  !/*ha       1258*/
      go to 2460                                                                !/*ha       1259*/
 2140 continue                                                                  !/*ha       1260*/
      go to (2150,2210), isam3                                                  !/*ha       1261*/
 2150 continue                                                                  !/*ha       1262*/
      if (nnch2) 2160,2180,2170                                                 !/*ha       1263*/
 2160 continue                                                                  !/*ha       1264*/
      ich2=ibps                                                                 !/*ha       1265*/
      go to 2190                                                                !/*ha       1266*/
 2170 continue                                                                  !/*ha       1267*/
      ich2=ibv                                                                  !/*ha       1268*/
      go to 2190                                                                !/*ha       1269*/
 2180 continue                                                                  !/*ha       1270*/
      iaibf=iabs(ibf)+6                                                         !/*ha       1271*/
      call bamjet (ihad,ibb,iaibf,ibb,ibb,amch2,3)                              !/*ha       1272*/
      go to 2200                                                                !/*ha       1273*/
 2190 continue                                                                  !/*ha       1274*/
      call parjet (ihad,ich2)                                                   !/*ha       1275*/
 2200 continue                                                                  !/*ha       1276*/
      go to 2460                                                                !/*ha       1277*/
 2210 continue                                                                  !/*ha       1278*/
      if (nnch2) 2220,2240,2230                                                 !/*ha       1279*/
 2220 continue                                                                  !/*ha       1280*/
      ich2=ibps                                                                 !/*ha       1281*/
      go to 2250                                                                !/*ha       1282*/
 2230 continue                                                                  !/*ha       1283*/
      ich2=ibv                                                                  !/*ha       1284*/
      go to 2250                                                                !/*ha       1285*/
 2240 continue                                                                  !/*ha       1286*/
      iaibb=iabs(ibb)+6                                                         !/*ha       1287*/
      call bamjet (ihad,ibf,iaibb,ibb2,ibf,amch2,3)                             !/*ha       1288*/
      go to 2260                                                                !/*ha       1289*/
 2250 continue                                                                  !/*ha       1290*/
      call parjet (ihad,ich2)                                                   !/*ha       1291*/
 2260 continue                                                                  !/*ha       1292*/
      go to 2460                                                                !/*ha       1293*/
 2270 continue                                                                  !/*ha       1294*/
      go to (2340,2280), isam3                                                  !/*ha       1295*/
 2280 continue                                                                  !/*ha       1296*/
      if (nnch2) 2290,2310,2300                                                 !/*ha       1297*/
 2290 continue                                                                  !/*ha       1298*/
      ich2=ibps                                                                 !/*ha       1299*/
      go to 2320                                                                !/*ha       1300*/
 2300 continue                                                                  !/*ha       1301*/
      ich2=ibv                                                                  !/*ha       1302*/
      go to 2320                                                                !/*ha       1303*/
 2310 continue                                                                  !/*ha       1304*/
      iaifb=iabs(ifb)+6                                                         !/*ha       1305*/
      call bamjet (ihad,iff,iaifb,ibb,ibb,amch2,3)                              !/*ha       1306*/
      go to 2330                                                                !/*ha       1307*/
 2320 continue                                                                  !/*ha       1308*/
      call parjet (ihad,ich2)                                                   !/*ha       1309*/
 2330 continue                                                                  !/*ha       1310*/
      go to 2460                                                                !/*ha       1311*/
 2340 continue                                                                  !/*ha       1312*/
      if (nnch1) 2350,2370,2360                                                 !/*ha       1313*/
 2350 continue                                                                  !/*ha       1314*/
      ich2=ifps                                                                 !/*ha       1315*/
      go to 2380                                                                !/*ha       1316*/
 2360 continue                                                                  !/*ha       1317*/
      ich2=ifv                                                                  !/*ha       1318*/
      go to 2380                                                                !/*ha       1319*/
 2370 continue                                                                  !/*ha       1320*/
      iaibb=iabs(ibb)+6                                                         !/*ha       1321*/
      call bamjet (ihad,ibf,iaibb,iaifb2,ibf,amch1,3)                           !/*ha       1322*/
      go to 2390                                                                !/*ha       1323*/
 2380 continue                                                                  !/*ha       1324*/
      call parjet (ihad,ich2)                                                   !/*ha       1325*/
 2390 continue                                                                  !/*ha       1326*/
      go to 2460                                                                !/*ha       1327*/
 2400 continue                                                                  !/*ha       1328*/
      if (nnch2) 2410,2430,2420                                                 !/*ha       1329*/
 2410 continue                                                                  !/*ha       1330*/
      ich2=ib8                                                                  !/*ha       1331*/
      go to 2440                                                                !/*ha       1332*/
 2420 continue                                                                  !/*ha       1333*/
      ich2=ibio                                                                 !/*ha       1334*/
      go to 2440                                                                !/*ha       1335*/
 2430 continue                                                                  !/*ha       1336*/
      call bamjet (ihad,ibf,ibb1,ibb2,ibf,amch2,4)                              !/*ha       1337*/
      go to 2450                                                                !/*ha       1338*/
 2440 continue                                                                  !/*ha       1339*/
      call parjet (ihad,ich2)                                                   !/*ha       1340*/
 2450 continue                                                                  !/*ha       1341*/
 2460 continue                                                                  !/*ha       1342*/
      if (ibproj) 2470,2480,2510                                                !/*ha       1343*/
 2470 continue                                                                  !/*ha       1344*/
      go to 2520                                                                !/*ha       1345*/
 2480 continue                                                                  !/*ha       1346*/
      go to (2490,2500), isam3                                                  !/*ha       1347*/
 2490 continue                                                                  !/*ha       1348*/
      go to 2520                                                                !/*ha       1349*/
 2500 continue                                                                  !/*ha       1350*/
      go to 2540                                                                !/*ha       1351*/
 2510 continue                                                                  !/*ha       1352*/
      go to 2540                                                                !/*ha       1353*/
 2520 continue                                                                  !/*ha       1354*/
      do 2530 i=1,ihad                                                          !/*ha       1355*/
      pzf(i)=-pzf(i)                                                            !/*ha       1356*/
 2530 continue                                                                  !/*ha       1357*/
 2540 continue                                                                  !/*ha       1358*/
      naux=naux+1                                                               !/*ha       1359*/
      bgch2x=ptxch2/amch2                                                       !/*ha       1360*/
      bgch2y=ptych2/amch2                                                       !/*ha       1361*/
      ach2=bgch2**2-(ptxch2**2+ptych2**2)/amch2**2                              !/*ha       1362*/
      if (ach2.le.0.d0) go to 1920                                              !/*ha       1363*/
      bgch2z=sqrt(ach2)                                                         !/*ha       1364*/
      bgch2z=sign(bgch2z,bgch2)                                                 !/*ha       1365*/
      call lortra (ihad,naux,gamch2,bgch2x,bgch2y,bgch2z)                       !/*ha       1366*/
      naux=ihad+naux-1                                                          !/*ha       1367*/
      do 2550 i=1,naux                                                          !/*ha       1368*/
      pxr(i)=pxa(i)                                                             !/*ha       1369*/
      pyr(i)=pya(i)                                                             !/*ha       1370*/
      pzr(i)=pza(i)                                                             !/*ha       1371*/
      amr(i)=ama(i)                                                             !/*ha       1372*/
      ichr(i)=icha(i)                                                           !/*ha       1373*/
      anr(i)=ana(i)                                                             !/*ha       1374*/
      ibarr(i)=ibara(i)                                                         !/*ha       1375*/
      nrer(i)=nrea(i)                                                           !/*ha       1376*/
      her(i)=hepa(i)                                                            !/*ha       1377*/
 2550 continue                                                                  !/*ha       1378*/
      nres=naux                                                                 !/*ha       1379*/
      call decaux (naux,5)                                                      !/*ha       1380*/
!REP  call decaux (naux,4)                                                      !/*ha       1381*/
      euz=0.d0                                                                  !/*ha       1382*/
      pux=0.d0                                                                  !/*ha       1383*/
      puy=0.d0                                                                  !/*ha       1384*/
      puz=0.d0                                                                  !/*ha       1385*/
      icu=0                                                                     !/*ha       1386*/
      ibu=0                                                                     !/*ha       1387*/
      issu=0                                                                    !/*ha       1388*/
      lissu=.true.                                                              !/*ha       1389*/
      do 2570 i=1,naux                                                          !/*ha       1390*/
      heph(i)=gamcm*hepa(i)+bgcm*pza(i)                                         !/*ha       1391*/
      pzh(i)=gamcm*pza(i)+bgcm*hepa(i)                                          !/*ha       1392*/
      pxh(i)=pxa(i)                                                             !/*ha       1393*/
      pyh(i)=pya(i)                                                             !/*ha       1394*/
      amh(i)=ama(i)                                                             !/*ha       1395*/
      ichh(i)=icha(i)                                                           !/*ha       1396*/
      anh(i)=ana(i)                                                             !/*ha       1397*/
      ibarh(i)=ibara(i)                                                         !/*ha       1398*/
      nreh(i)=nrea(i)                                                           !/*ha       1399*/
      euz=euz+heph(i)                                                           !/*ha       1400*/
      pux=pux+pxh(i)                                                            !/*ha       1401*/
      puy=puy+pyh(i)                                                            !/*ha       1402*/
      puz=puz+pzh(i)                                                            !/*ha       1403*/
      icu=icu+ichh(i)                                                           !/*ha       1404*/
      ibu=ibu+ibarh(i)                                                          !/*ha       1405*/
      do 2560 j=1,3                                                             !/*ha       1406*/
      if (nreh(i).gt.30) then                                                   !/*ha       1407*/
        lissu=.false.                                                           !/*ha       1408*/
      else                                                                      !/*ha       1409*/
        issu=issu+mquark(j,nreh(i))/3                                           !/*ha       1410*/
      endif                                                                     !/*ha       1411*/
 2560 continue                                                                  !/*ha       1412*/
 2570 continue                                                                  !/*ha       1413*/
      nhad=naux                                                                 !/*ha       1414*/
      ichtot=ich(kproj)+ich(ktarg)                                              !/*ha       1415*/
      ibtot=ibproj+ibtarg                                                       !/*ha       1416*/
      istot=0                                                                   !/*ha       1417*/
      do 2580 j=1,3                                                             !/*ha       1418*/
      istot=istot+mquark(j,kproj)/3+mquark(j,ktarg)/3                           !/*ha       1419*/
 2580 continue                                                                  !/*ha       1420*/
      if (icu.ne.ichtot) then                                                   !/*ha       1421*/
        call hadcnt (1,nout)                                                    !/*ha       1422*/
        if (nout.le.5) write (io,*) ' charge conservation failure: ',           !/*ha       1423*/
     &  ' icu,ichtot,ibu,ibtot,issu,istot',                                     !/*ha       1424*/
     &    icu,ichtot,ibu,ibtot,issu,istot                                       !/*ha       1425*/
      elseif (ibu.ne.ibtot) then                                                !/*ha       1426*/
        call hadcnt (2,nout)                                                    !/*ha       1427*/
        if (nout.le.5) write (io,*) ' baryon conservation failure: ',           !/*ha       1428*/
     &  ' icu,ichtot,ibu,ibtot,issu,istot',                                     !/*ha       1429*/
     &    icu,ichtot,ibu,ibtot,issu,istot                                       !/*ha       1430*/
      elseif (issu.ne.istot.and.lissu) then                                     !/*ha       1431*/
        call hadcnt (3,nout)                                                    !/*ha       1432*/
        if(nout.le.5) write (io,*) ' strangeness conservation failure:',        !/*ha       1433*/
     & ' icu,ichtot,ibu,ibtot,issu,istot',                                      !/*ha       1434*/
     &   icu,ichtot,ibu,ibtot,issu,istot                                        !/*ha       1435*/
!     else                                                                      !/*ha       1436*/
!       go to 2590                                                              !/*ha       1437*/
!     endif                                                                     !/*ha       1438*/
      elseif (abs(euz-amtar-eproj)/(amtar+eproj).gt.ecrit) then                 !/*ha       1439*/
        call hadcnt (4,nout)                                                    !/*ha       1440*/
      elseif (abs(pux)/pproj.gt.pcrit) then                                     !/*ha       1441*/
        call hadcnt (5,nout)                                                    !/*ha       1442*/
      elseif (abs(puy)/pproj.gt.pcrit) then                                     !/*ha       1443*/
        call hadcnt (5,nout)                                                    !/*ha       1444*/
      elseif (abs(puz-pproj)/pproj.gt.pcrit) then                               !/*ha       1445*/
        call hadcnt (5,nout)                                                    !/*ha       1446*/
      else                                                                      !/*ha       1447*/
        call hadcnt (-1,nout)                                                   !/*ha       1448*/
        go to 2590                                                              !/*ha       1449*/
      endif                                                                     !/*ha       1450*/
      go to 10                                                                  !/*ha       1451*/
 2590 continue                                                                  !/*ha       1452*/
      return                                                                    !/*ha       1453*/
      end                                                                       !/*ha       1454*/

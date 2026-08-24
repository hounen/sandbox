!     $Id: twopin.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                       !/*tw          1*/
      subroutine twopin (ecm,id1in,id2in,id1out,id2out,miso1,miso2,efd)         !/*tw          2*/
#include <lcs/double.h>                                                         /*!tw          3*/
!     ==================================================================        !/*tw          4*/
!     twopin determines the types (id1out,id2out) and masses                    !/*tw          5*/
!     (miso1,miso2) in an isobar-nucleon collision between isobar of            !/*tw          6*/
!     type id1in and mass mis1in and nucleon of type id2in, with                !/*tw          7*/
!     total c.m. energy ecm.  The choice of isobar types is based on            !/*tw          8*/
!     the assumption of the formation of two (3/2,3/2) isobars and              !/*tw          9*/
!     the conservation of isotopic spin in the collision.                       !/*tw         10*/
!     efd is the multiplying factor of efp-efn for energy fudge factor.         !/*tw         11*/
!                                                                               !/*tw         12*/
! TWOPIN calls   FLTRNF  SQRT  PTOTAL  PHASE                                    !/*tw         13*/
! called by GAMMA                                                               !/*tw         14*/
!     ==================================================================        !/*tw         15*/
!                                                                               !/*tw         16*/
#ifdef CHEAP                                                                    /*!tw         17*/
      double precision id1in, id2in, id1out, id2out, miso1, miso2, mimax        !/*tw         18*/
     & , mmax1, mmax2                                                           !/*tw         19*/
#else                                                                           /*!tw         20*/
      real id1in, id2in, id1out, id2out, miso1, miso2, mimax, mmax1,            !/*tw         21*/
     & mmax2                                                                    !/*tw         22*/
#endif                                                                          /*!tw         23*/
      dimension scaii(16)                                                       !/*tw         24*/
      data scaii /0.0d0,2.d0,4.d0,6.d0,12.d0,19.d0,29.d0,42.d0,58.d0,77         !/*tw         25*/
     & .d0,99.d0,129.d0,159.d0,179.d0,191.d0,195.d0/                            !/*tw         26*/
      data dimu, mimax /1077.811d0,1573.d0/                                     !/*tw         27*/
      miso1=dp0                                                                 !/*tw         28*/
      miso2=dp0                                                                 !/*tw         29*/
      if (ecm.le.dp2*dimu) return                                               !/*tw         30*/
      efd=dp0                                                                   !/*tw         31*/
!                                                                               !/*tw         32*/
! first decide what type of isobars result from the collision.                  !/*tw         33*/
!                                                                               !/*tw         34*/
      eta=rang()                                                                !/*tw         35*/
! for distribution functions 6/7,1/7 multiply ranf by 7.                        !/*tw         36*/
      eta7=eta*7.d0                                                             !/*tw         37*/
      if (id2in.eq.1) go to 70                                                  !/*tw         38*/
!                                                                               !/*tw         39*/
! isobar+neutron                                                                !/*tw         40*/
!                                                                               !/*tw         41*/
      if (id1in.ne.25) go to 10                                                 !/*tw         42*/
!                           n + i-                                              !/*tw         43*/
      id1out=25.d0                                                              !/*tw         44*/
      id2out=24.d0                                                              !/*tw         45*/
      go to 140                                                                 !/*tw         46*/
   10 if (id1in.ne.24) go to 30                                                 !/*tw         47*/
!                           n + i0                                              !/*tw         48*/
      if (eta7.le.dp1) go to 20                                                 !/*tw         49*/
      id1out=14.d0                                                              !/*tw         50*/
      id2out=25.d0                                                              !/*tw         51*/
      efd=dp1                                                                   !/*tw         52*/
      go to 140                                                                 !/*tw         53*/
   20 id1out=24.d0                                                              !/*tw         54*/
      id2out=24.d0                                                              !/*tw         55*/
      go to 150                                                                 !/*tw         56*/
   30 if (id1in.ne.14) go to 50                                                 !/*tw         57*/
!                           n + i+                                              !/*tw         58*/
      if (eta.le.0.1d0) go to 40                                                !/*tw         59*/
      id1out=13.d0                                                              !/*tw         60*/
      id2out=25.d0                                                              !/*tw         61*/
      go to 140                                                                 !/*tw         62*/
   40 id1out=14.d0                                                              !/*tw         63*/
      id2out=24.d0                                                              !/*tw         64*/
      go to 140                                                                 !/*tw         65*/
!                           n + i++                                             !/*tw         66*/
   50 if (eta7.lt.dp1) go to 60                                                 !/*tw         67*/
      id1out=13.d0                                                              !/*tw         68*/
      id2out=24.d0                                                              !/*tw         69*/
      go to 140                                                                 !/*tw         70*/
   60 id1out=14.d0                                                              !/*tw         71*/
      id2out=14.d0                                                              !/*tw         72*/
      efd=dp1                                                                   !/*tw         73*/
      go to 150                                                                 !/*tw         74*/
!                                                                               !/*tw         75*/
! isobar+proton                                                                 !/*tw         76*/
!                                                                               !/*tw         77*/
   70 if (id1in.ne.13) go to 80                                                 !/*tw         78*/
!                           p + i++                                             !/*tw         79*/
      id1out=13.d0                                                              !/*tw         80*/
      id2out=14.d0                                                              !/*tw         81*/
      go to 140                                                                 !/*tw         82*/
   80 if (id1in.ne.14) go to 100                                                !/*tw         83*/
!                           p + i+                                              !/*tw         84*/
      if (eta7.le.dp1) go to 90                                                 !/*tw         85*/
      id1out=13.d0                                                              !/*tw         86*/
      id2out=24.d0                                                              !/*tw         87*/
      efd=-dp1                                                                  !/*tw         88*/
      go to 140                                                                 !/*tw         89*/
   90 id1out=14.d0                                                              !/*tw         90*/
      id2out=14.d0                                                              !/*tw         91*/
      go to 150                                                                 !/*tw         92*/
  100 if (id1in.ne.24) go to 120                                                !/*tw         93*/
!                           p + i0                                              !/*tw         94*/
      if (eta.le.dph) go to 110                                                 !/*tw         95*/
      id1out=14.d0                                                              !/*tw         96*/
      id2out=24.d0                                                              !/*tw         97*/
      go to 140                                                                 !/*tw         98*/
  110 id1out=13.d0                                                              !/*tw         99*/
      id2out=25.d0                                                              !/*tw        100*/
      go to 140                                                                 !/*tw        101*/
  120 if (eta7.lt.dp1) go to 130                                                !/*tw        102*/
!                           p + i-                                              !/*tw        103*/
      id1out=14.d0                                                              !/*tw        104*/
      id2out=25.d0                                                              !/*tw        105*/
      go to 140                                                                 !/*tw        106*/
  130 id1out=24.d0                                                              !/*tw        107*/
      id2out=24.d0                                                              !/*tw        108*/
      efd=-dp1                                                                  !/*tw        109*/
      go to 150                                                                 !/*tw        110*/
!                                                                               !/*tw        111*/
! mix them up                                                                   !/*tw        112*/
!                                                                               !/*tw        113*/
  140 continue                                                                  !/*tw        114*/
      eta=rang()                                                                !/*tw        115*/
      if (eta.gt.dph) go to 150                                                 !/*tw        116*/
      idtem=id1out                                                              !/*tw        117*/
      id1out=id2out                                                             !/*tw        118*/
      id2out=idtem                                                              !/*tw        119*/
!                                                                               !/*tw        120*/
! now select the masses of the isobars                                          !/*tw        121*/
!                                                                               !/*tw        122*/
  150 continue                                                                  !/*tw        123*/
! pmax = max (phase*sig33**2), this occurs for m1=m2.                           !/*tw        124*/
      pmaxo=dp0                                                                 !/*tw        125*/
      do 160 i=2,16                                                             !/*tw        126*/
      ii=i-1                                                                    !/*tw        127*/
      ams=dimu+10*(i-2)                                                         !/*tw        128*/
      ecm2=ecm*ecm                                                              !/*tw        129*/
      ams2=ams*ams                                                              !/*tw        130*/
      scaii4=scaii(i)**4                                                        !/*tw        131*/
      pmaxx=ecm2*(ecm2-dp4*ams2)                                                !/*tw        132*/
      pmax=pmaxx*scaii4                                                         !/*tw        133*/
      if (pmax.le.pmaxo) go to 170                                              !/*tw        134*/
      pmaxo=pmax                                                                !/*tw        135*/
      pmaxxo=pmaxx                                                              !/*tw        136*/
  160 continue                                                                  !/*tw        137*/
  170 if (pmaxo.le.0) return                                                    !/*tw        138*/
      ams10=ams+dp10                                                            !/*tw        139*/
      ecmo2=dph*ecm                                                             !/*tw        140*/
      if (ams10.lt.ecmo2) go to 180                                             !/*tw        141*/
      ii1=ii-1                                                                  !/*tw        142*/
      dsca=scaii(ii)-scaii(ii1)                                                 !/*tw        143*/
      scaii4=(scaii(ii1)+0.1d0*(ecmo2-ams)*dsca)**4                             !/*tw        144*/
      pmaxo=pmaxxo*scaii4                                                       !/*tw        145*/
  180 pmax=0.125*sqrt(pmaxo)                                                    !/*tw        146*/
! maximal mass and range of masses for the first isobar                         !/*tw        147*/
      mmax1=ecm-dimu                                                            !/*tw        148*/
      rangm1=ecm-dp2*dimu                                                       !/*tw        149*/
!                                                                               !/*tw        150*/
!       for a given ecm select the isobar masses by the                         !/*tw        151*/
!       von neuman rejection method                                             !/*tw        152*/
  190 continue                                                                  !/*tw        153*/
      eta1=rang()                                                               !/*tw        154*/
      eta2=rang()                                                               !/*tw        155*/
      eta3=rang()                                                               !/*tw        156*/
      miso1=mmax1-rangm1*sqrt(eta1)                                             !/*tw        157*/
      if (miso1.gt.mimax) go to 190                                             !/*tw        158*/
      mmax2=ecm-miso1                                                           !/*tw        159*/
      rangm2=mmax2-dimu                                                         !/*tw        160*/
      if (rangm2.le.0) go to 190                                                !/*tw        161*/
      miso2=dimu+rangm2*eta2                                                    !/*tw        162*/
      if (miso2.gt.mimax) go to 190                                             !/*tw        163*/
      tcm1=miso1-dimu                                                           !/*tw        164*/
      tcm2=miso2-dimu                                                           !/*tw        165*/
      izero=0                                                                   !/*tw        166*/
      ione=1                                                                    !/*tw        167*/
      call ptotal (tcm1,ione,izero,izero,sig331,absor)                          !/*tw        168*/
      izero=0                                                                   !/*tw        169*/
      ione=1                                                                    !/*tw        170*/
      call ptotal (tcm2,ione,izero,izero,sig332,absor)                          !/*tw        171*/
      pm=sig331*sig332*phase(ecm,miso1,miso2)/pmax                              !/*tw        172*/
      if (eta3.gt.pm) go to 190                                                 !/*tw        173*/
      return                                                                    !/*tw        174*/
      end                                                                       !/*tw        175*/

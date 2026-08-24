!     $Id: pcol.F,v 1.5 2002/03/27 18:15:13 gwm Exp $                         !/*pc          1*/
      subroutine pcol (ityp,hsig,ec,nopart,kind,ep,alf,bet,gam,andt)            !/*pc          2*/
! PCOL       calls...  ERRORF  SQRT  FLTRNF  XSEC  MUD  CRDET  ROU16            !/*pc          3*/
!                      ISOB  ROU20  ESWH  ROU15  ROU11                          !/*pc          4*/
!     PCOL   called by...   HYD                                                 !/*pc          5*/
#include <lcs/double.h>                                                         /*!pc          6*/
#include <lcs/lptS.h>                                                           /*!pc          7*/
#include <lcs/isobar.h>                                                         /*!pc          8*/
#include <lcs/btcom.h>                                                          /*!pc          9*/
#include <lcs/tpcrs.h>                                                          /*!pc         10*/
#include <lcs/inout.h>                                                          /*!pc         11*/
      external h1bd                                                             !/*pc         12*/
      dimension kind(2), ep(2), alf(2), bet(2), gam(2)                          !/*pc         13*/
      dimension icc(12)                                                         !/*pc         14*/
      parameter (lim1=613, lim2=1883)                                           !/*pc         15*/
      dimension idumm(lim1), rdumm(lim2)                                        !/*pc         16*/
      data ib1 /0/                                                              !/*pc         17*/
      inpt=0                                                                    !/*pc         18*/
      ke=1                                                                      !/*pc         19*/
      if (ib1) 20,10,20                                                         !/*pc         20*/
   10 continue                                                                  !/*pc         21*/
      ib1=1                                                                     !/*pc         22*/
      andit_=andt                                                               !/*pc         23*/
!     poac(19),ppac(19)                                                         !/*pc         24*/
   20 continue                                                                  !/*pc         25*/
      do 30 i=1,lim1                                                            !/*pc         26*/
   30 idumm(i)=0                                                                !/*pc         27*/
      do 40 i=1,lim2                                                            !/*pc         28*/
   40 rdumm(i)=dp0                                                              !/*pc         29*/
      do 50 i=1,12                                                              !/*pc         30*/
   50 icc(i)=0                                                                  !/*pc         31*/
      pm(1)=dncms                                                               !/*pc         32*/
      go to (80,80,70,60,70,60,60), ityp                                        !/*pc         33*/
!           p  n  pi+ pi0 pi- mu+ mu-                                           !/*pc         34*/
   60 write (io,420) ityp                                                       !/*pc         35*/
      call errorf ('pcol.1')                                                    !/*pc         36*/
   70 pm(1)=pnms                                                                !/*pc         37*/
   80 inc=1                                                                     !/*pc         38*/
      clsm=dp1                                                                  !/*pc         39*/
      pm(2)=dncms                                                               !/*pc         40*/
      e(2)=dncms                                                                !/*pc         41*/
      e(1)=ec*rcpmv+pm(1)                                                       !/*pc         42*/
      pxyz(2)=dp0                                                               !/*pc         43*/
      pxyz(6)=dp0                                                               !/*pc         44*/
      pxyz(10)=dp0                                                              !/*pc         45*/
      pxyz(1)=dp0                                                               !/*pc         46*/
      pxyz(5)=dp0                                                               !/*pc         47*/
      pxyz(9)=sqrt(e(1)*e(1)-pm(1)*pm(1))                                       !/*pc         48*/
      p1oe1=pxyz(9)/e(1)                                                        !/*pc         49*/
!     calc's x,y,z mom coord's of inc. part.(pxyz(1-5-9))and p1oe1 = pz1        !/*pc         50*/
      rlke=ec                                                                   !/*pc         51*/
      value1=rang()                                                             !/*pc         52*/
      r=value1                                                                  !/*pc         53*/
      value1=rlke                                                               !/*pc         54*/
      itp=ityp-ityp/5                                                           !/*pc         55*/
      nopart=2                                                                  !/*pc         56*/
      if (itp-3) 140,170,90                                                     !/*pc         57*/
   90 continue                                                                  !/*pc         58*/
!    try (pi-,p) exchange.                                                      !/*pc         59*/
      r=r-xsecl(4,itp,ec)/hsig                                                  !/*pc         60*/
      if (r.gt.dp0) go to 170                                                   !/*pc         61*/
!  a (pi-,p) exchange event  has occurred                                       !/*pc         62*/
      it=5                                                                      !/*pc         63*/
      kind(1)=3                                                                 !/*pc         64*/
!  pi 0                                                                         !/*pc         65*/
      kind(2)=1                                                                 !/*pc         66*/
!  neutron                                                                      !/*pc         67*/
      pt(2)=dp4                                                                 !/*pc         68*/
      pt(14)=2.d0                                                               !/*pc         69*/
      ik=it                                                                     !/*pc         70*/
      pm(3)=poms                                                                !/*pc         71*/
      if (rlke-34.d1) 100,100,110                                               !/*pc         72*/
  100 call mud (snt,inpt)                                                       !/*pc         73*/
      call crdet (51,pmdx,rlke)                                                 !/*pc         74*/
!            dif pi--p exchg                                                    !/*pc         75*/
      go to 320                                                                 !/*pc         76*/
  110 i3=1                                                                      !/*pc         77*/
      if (rlke-1.d+03) 120,130,130                                              !/*pc         78*/
  120 i3=2                                                                      !/*pc         79*/
      value1=rlke-34.d1                                                         !/*pc         80*/
  130 call rou16 (pmdx)                                                         !/*pc         81*/
!  dif pi--p exchg 2500-1000, 1000-340,  340- 0                                 !/*pc         82*/
      go to 390                                                                 !/*pc         83*/
  140 continue                                                                  !/*pc         84*/
!    try nucleon - p  dble  production.                                         !/*pc         85*/
      if (ec.lt.eth(2,itp)) go to 170                                           !/*pc         86*/
      r=r-xsecl(2,itp,ec)/hsig                                                  !/*pc         87*/
      if (r.gt.dp0) go to 170                                                   !/*pc         88*/
!    dble production has occurred.                                              !/*pc         89*/
      nopart=4                                                                  !/*pc         90*/
      ip_=0                                                                     !/*pc         91*/
  150 no_=ityp                                                                  !/*pc         92*/
      einc_=ec                                                                  !/*pc         93*/
      call isob                                                                 !/*pc         94*/
      kno=2                                                                     !/*pc         95*/
      do 160 noo=1,nopart                                                       !/*pc         96*/
      kind(noo)=int(out_(kno))-1                                                !/*pc         97*/
      ep(noo)=out_(kno+1)                                                       !/*pc         98*/
      alf(noo)=out_(kno+4)                                                      !/*pc         99*/
      bet(noo)=out_(kno+5)                                                      !/*pc        100*/
      gam(noo)=out_(kno+6)                                                      !/*pc        101*/
  160 kno=kno+7                                                                 !/*pc        102*/
      go to 270                                                                 !/*pc        103*/
  170 continue                                                                  !/*pc        104*/
!  try sngl production.                                                         !/*pc        105*/
      if (ec.lt.eth(1,itp)) go to 180                                           !/*pc        106*/
      r=r-xsecl(1,itp,ec)/hsig                                                  !/*pc        107*/
      if (r.gt.dp0) go to 180                                                   !/*pc        108*/
!    nucleon - p ,pi+ -p, or pi- -p  sngl production has occurred.              !/*pc        109*/
      nopart=3                                                                  !/*pc        110*/
      ip_=-1                                                                    !/*pc        111*/
      go to 150                                                                 !/*pc        112*/
  180 continue                                                                  !/*pc        113*/
!  an elastic event has occurred.                                               !/*pc        114*/
      go to (200,210,290,190,340,190,190), ityp                                 !/*pc        115*/
  190 write (io,430) ityp                                                       !/*pc        116*/
      call errorf ('pcol.2')                                                    !/*pc        117*/
  200 i3=7                                                                      !/*pc        118*/
      it=18                                                                     !/*pc        119*/
      go to 220                                                                 !/*pc        120*/
  210 i3=4                                                                      !/*pc        121*/
      it=15                                                                     !/*pc        122*/
  220 call rou20 (dcin(1),dcln(1),dchn(1),pdci(1),pdch(1))                      !/*pc        123*/
! elas dif xsects-np 300-740,np 0-300, np 660-3500, pp 500-1000,pp 660-3        !/*pc        124*/
!     sets pt(2)=1.-prot,2.-neut,pt(14)=1.,pm(3)= dncms                         !/*pc        125*/
!     "calc"s cst(scat cos) for ec gt (500 for prot,740 for neuts)              !/*pc        126*/
      i3=i3                                                                     !/*pc        127*/
      igo=i3-4                                                                  !/*pc        128*/
      go to (280,230,240,250), igo                                              !/*pc        129*/
!      i3 =   5    6    7    8                                                  !/*pc        130*/
  230 snt=sqrt(dp1-cst*cst)                                                     !/*pc        131*/
      go to 250                                                                 !/*pc        132*/
  240 continue                                                                  !/*pc        133*/
      call polrn (cst,snt)                                                      !/*pc        134*/
  250 call eswh                                                                 !/*pc        135*/
      if (it.eq.5) go to 260                                                    !/*pc        136*/
      kind(1)=ityp-1                                                            !/*pc        137*/
      kind(2)=0                                                                 !/*pc        138*/
!    proton                                                                     !/*pc        139*/
  260 continue                                                                  !/*pc        140*/
      ep(1)=pt(3)                                                               !/*pc        141*/
      alf(1)=pt(8)                                                              !/*pc        142*/
      bet(1)=pt(9)                                                              !/*pc        143*/
      gam(1)=pt(10)                                                             !/*pc        144*/
      ep(2)=pt(15)                                                              !/*pc        145*/
      alf(2)=pt(20)                                                             !/*pc        146*/
      bet(2)=pt(21)                                                             !/*pc        147*/
      gam(2)=pt(22)                                                             !/*pc        148*/
  270 continue                                                                  !/*pc        149*/
      return                                                                    !/*pc        150*/
  280 i3=4                                                                      !/*pc        151*/
      call rou15 (ppdc(1))                                                      !/*pc        152*/
!      "calc"s cst(scat cos) for ec lt 740 mev                                  !/*pc        153*/
      go to 230                                                                 !/*pc        154*/
  290 i3=1                                                                      !/*pc        155*/
      it=1                                                                      !/*pc        156*/
!     pi+-p elas                                                                !/*pc        157*/
      call rou11 (ppdc(1))                                                      !/*pc        158*/
!  cs(248)  pi+-p  lt 340 mev.                                                  !/*pc        159*/
      i3=i3                                                                     !/*pc        160*/
      go to (330,300,300,320), i3                                               !/*pc        161*/
  300 write (io,440) i3                                                         !/*pc        162*/
  310 call errorf ('pcol.3')                                                    !/*pc        163*/
  320 cst=crdt(2)-abs(snt*(crdt(2)-crdt(1)))                                    !/*pc        164*/
      go to 230                                                                 !/*pc        165*/
  330 i3=1                                                                      !/*pc        166*/
      call rou15 (ppdc(1))                                                      !/*pc        167*/
!   pi+-p dif elas for1.-2.5 gev at 3401,for .34 -1. at 3231.                   !/*pc        168*/
      i3=i3                                                                     !/*pc        169*/
      go to 230                                                                 !/*pc        170*/
  340 it=3                                                                      !/*pc        171*/
!   pi--p elas                                                                  !/*pc        172*/
      i3=2                                                                      !/*pc        173*/
      pt(2)=5.d0                                                                !/*pc        174*/
      pt(14)=dp1                                                                !/*pc        175*/
      ik=it                                                                     !/*pc        176*/
      pm(3)=pnms                                                                !/*pc        177*/
      if (rlke-34.d1) 350,350,360                                               !/*pc        178*/
  350 call mud (snt,inpt)                                                       !/*pc        179*/
      call crdet (51,pmdd(1),rlke)                                              !/*pc        180*/
      go to 320                                                                 !/*pc        181*/
  360 i3=1                                                                      !/*pc        182*/
      if (rlke-1.d+03) 370,380,380                                              !/*pc        183*/
  370 i3=2                                                                      !/*pc        184*/
      value1=rlke-34.d1                                                         !/*pc        185*/
  380 call rou16 (pmdd(1))                                                      !/*pc        186*/
  390 i3=i3                                                                     !/*pc        187*/
      if (i3) 230,400,410                                                       !/*pc        188*/
  400 i3=3                                                                      !/*pc        189*/
      call rou15 (ppdc(1))                                                      !/*pc        190*/
      go to 230                                                                 !/*pc        191*/
  410 write (io,450) i3                                                         !/*pc        192*/
      go to 310                                                                 !/*pc        193*/
!                                                                               !/*pc        194*/
  420 format (1h1,7hityp = ,i2,19h at stmt 23 in pcol)                          !/*pc        195*/
  430 format (1h1,6hityp =,i2,17h stmt 601 in pcol)                             !/*pc        196*/
  440 format (1h1,3hi3=,i3,20h at stmt 806 in pcol)                             !/*pc        197*/
  450 format (1h1,3hi3=,i3,20h at stmt 970 in pcol)                             !/*pc        198*/
      end                                                                       !/*pc        199*/

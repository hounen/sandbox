!     $Id: pi.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                           !/*pi          1*/
      subroutine pi (po,itip,ener,pmom,a,b,g)                                   !/*pi          2*/
! PI         calls...  SQRT  FLTRNF  EXP  COS  AZIRN                            !/*pi          3*/
!     PI     called by...   NUCNUC  PINUC  STOR                                 !/*pi          4*/
#include <lcs/double.h>                                                         /*!pi          5*/
      dimension pm(12), ang(15)                                                 !/*pi          6*/
      dimension ratt(12)                                                        !/*pi          7*/
      dimension dndpip(3,12), dndpim(3,12)                                      !/*pi          8*/
      data dndpip /-5.3242445d-03,2.2346497d-02,4.5267868d-01,1.1044145e        !/*pi          9*/
     & -03,-3.2094955d-02,5.6748962d-01,8.9269876d-04,-2.8545380d-02,5.5        !/*pi         10*/
     & 277443d-01,5.0222874d-04,-2.0401001d-02,5.1032925d-01,2.0570680d-        !/*pi         11*/
     & 04,-1.1921406d-02,4.4970989d-01,6.6768611d-05,-5.8776736d-03,3.83        !/*pi         12*/
     & 99315d-01,2.2491440d-05,-2.9177666d-03,3.3454037d-01,5.6584831d-0        !/*pi         13*/
     & 6,-1.2033470d-03,2.9090124d-01,9.4826100d-07,-3.7099421d-04,2.541        !/*pi         14*/
     & 4348d-01,1.6872946d-07,-1.1652336d-04,2.3341179d-01,3.4642653d-08        !/*pi         15*/
     & ,-4.0553510d-05,2.2265625d-01,5.3050826d-09,-1.1835014d-05,2.1562        !/*pi         16*/
     & 809d-01/                                                                 !/*pi         17*/
      data dndpim /-3.7355423d-03,2.9491067d-02,2.0408440d-01,-1.3875589        !/*pi         18*/
     & d-04,-1.4313459d-03,2.7039909d-01,1.4499575d-04,-5.3753257d-03,2.        !/*pi         19*/
     & 8402138d-01,1.0721385d-04,-4.5639873d-03,2.7966785d-01,4.8756599e        !/*pi         20*/
     & -05,-2.8809309d-03,2.6756477d-01,1.6727019d-05,-1.4842749d-03,2.5        !/*pi         21*/
     & 234318d-01,5.7825819d-06,-7.5179338d-04,2.4009037d-01,1.4668331d-        !/*pi         22*/
     & 06,-3.1205267d-04,2.2888833d-01,2.4605833d-07,-9.6268952d-05,2.19        !/*pi         23*/
     & 35964d-01,4.3786713d-08,-3.0238181d-05,2.1398067d-01,8.9794412d-0        !/*pi         24*/
     & 9,-1.0516495d-05,2.1118736d-01,1.3733370d-09,-3.0666124d-06,2.093        !/*pi         25*/
     & 6286d-01/                                                                !/*pi         26*/
      data ang /dp0,dp0,.01745d0,.0349d0,.0698d0,.1396d0,.2094d0,.3491d0        !/*pi         27*/
     & ,.7845d0,6*dp0/                                                          !/*pi         28*/
      data pm /4.5d0,7.5d0,10.5d0,14.d0,22.d0,34.d0,50.d0,90.d0,170.d0,2        !/*pi         29*/
     & 90.d0,490.d0,1.d3/                                                       !/*pi         30*/
!                                                                               !/*pi         31*/
      a1=3.386d0                                                                !/*pi         32*/
      a2=4.146d0                                                                !/*pi         33*/
      a3=4.556d0                                                                !/*pi         34*/
      ta3=a3+a3                                                                 !/*pi         35*/
      a5=9.600d0                                                                !/*pi         36*/
      a6=4.823                                                                  !/*pi         37*/
      topi=6.28318d0                                                            !/*pi         38*/
      xp=0.7853982d0                                                            !/*pi         39*/
      xp2a3=xp*xp*a3                                                            !/*pi         40*/
      sxp=sqrt(xp)                                                              !/*pi         41*/
      spo=sqrt(po)                                                              !/*pi         42*/
      pos=po*po                                                                 !/*pi         43*/
!                                                                               !/*pi         44*/
      r1=rang()                                                                 !/*pi         45*/
      go to (10,10,20,20,30), itip                                              !/*pi         46*/
!                                                                               !/*pi         47*/
!     itip = 1 - proton                                                         !/*pi         48*/
!     itip = 2 - neutron                                                        !/*pi         49*/
!                                                                               !/*pi         50*/
   10 a4=7.141d0                                                                !/*pi         51*/
      rm=0.94d0                                                                 !/*pi         52*/
      go to 40                                                                  !/*pi         53*/
!                                                                               !/*pi         54*/
!     itip = 3 - pi plus                                                        !/*pi         55*/
!     itip = 4 - pi zero                                                        !/*pi         56*/
!                                                                               !/*pi         57*/
   20 a4=7.141d0                                                                !/*pi         58*/
      rm=0.139d0                                                                !/*pi         59*/
      go to 40                                                                  !/*pi         60*/
!                                                                               !/*pi         61*/
!     itip = 5 - pi minus                                                       !/*pi         62*/
!                                                                               !/*pi         63*/
   30 a4=1.853                                                                  !/*pi         64*/
      rm=0.139d0                                                                !/*pi         65*/
   40 continue                                                                  !/*pi         66*/
!                       sample uniformly from po                                !/*pi         67*/
!                                                                               !/*pi         68*/
      ps=po*r1                                                                  !/*pi         69*/
!                                                                               !/*pi         70*/
!                       find dndp(max) for ps                                   !/*pi         71*/
!                                                                               !/*pi         72*/
      if (po.lt.pm(6)) go to 60                                                 !/*pi         73*/
      if (po.lt.pm(9)) go to 50                                                 !/*pi         74*/
      inx=10                                                                    !/*pi         75*/
      if (po.lt.pm(10)) go to 90                                                !/*pi         76*/
      inx=11                                                                    !/*pi         77*/
      if (po.gt.pm(11)) inx=12                                                  !/*pi         78*/
      go to 90                                                                  !/*pi         79*/
   50 inx=7                                                                     !/*pi         80*/
      if (po.lt.pm(7)) go to 90                                                 !/*pi         81*/
      inx=8                                                                     !/*pi         82*/
      if (po.gt.pm(8)) inx=9                                                    !/*pi         83*/
      go to 90                                                                  !/*pi         84*/
   60 if (po.lt.pm(4)) go to 70                                                 !/*pi         85*/
      inx=5                                                                     !/*pi         86*/
      if (po.gt.pm(5)) inx=6                                                    !/*pi         87*/
      go to 90                                                                  !/*pi         88*/
   70 if (po.gt.pm(2)) go to 80                                                 !/*pi         89*/
      inx=1                                                                     !/*pi         90*/
      if (po.gt.pm(1)) inx=2                                                    !/*pi         91*/
      go to 90                                                                  !/*pi         92*/
   80 inx=3                                                                     !/*pi         93*/
      if (po.gt.pm(3)) inx=4                                                    !/*pi         94*/
   90 if (itip.gt.4) go to 100                                                  !/*pi         95*/
      dmax=dndpip(1,inx)*po*po+dndpip(2,inx)*po+dndpip(3,inx)                   !/*pi         96*/
      go to 110                                                                 !/*pi         97*/
  100 dmax=dndpim(1,inx)*po*po+dndpim(2,inx)*po+dndpim(3,inx)                   !/*pi         98*/
  110 if (ener.ne.dp0) ps=pmom                                                  !/*pi         99*/
      pss=ps*ps                                                                 !/*pi        100*/
      psa6=ps*a6                                                                !/*pi        101*/
      sps=sqrt(ps)                                                              !/*pi        102*/
      f1=a1*pss*exp(-a2*ps/spo)                                                 !/*pi        103*/
      f2=(a4*pss/po)*exp(-a5*pss/pos)                                           !/*pi        104*/
      d1=ta3*ps*spo                                                             !/*pi        105*/
      d2=psa6*psa6                                                              !/*pi        106*/
      e1=xp2a3*ps*spo                                                           !/*pi        107*/
      if (e1.gt.5.d1) e1=5.d1                                                   !/*pi        108*/
      e2=xp*psa6                                                                !/*pi        109*/
      if (e2.gt.5.d1) e2=5.d1                                                   !/*pi        110*/
      dpsmx=((topi*f1/d1)*(dp1-exp(-e1)))+((topi*f2/d2)*(dp1-exp(-e2)*          !/*pi        111*/
     & (dp1+e2)))                                                               !/*pi        112*/
      ratio=dpsmx/dmax                                                          !/*pi        113*/
      r2=rang()                                                                 !/*pi        114*/
      if (ener.ne.dp0) go to 120                                                !/*pi        115*/
      if (r2.le.ratio) go to 120                                                !/*pi        116*/
      r1=rang()                                                                 !/*pi        117*/
      ps=po*r1                                                                  !/*pi        118*/
      go to 110                                                                 !/*pi        119*/
  120 continue                                                                  !/*pi        120*/
!                  for current ps and r2, find angles.                          !/*pi        121*/
!                                                                               !/*pi        122*/
!     set angle intervals - coarse-  test for ratio and r2                      !/*pi        123*/
!      0 -  1 degrees (.00000 - .01745 radians)                                 !/*pi        124*/
!      1 -  2 degrees (.01745 - .03490 radians)                                 !/*pi        125*/
!      2 -  4 degrees (.03490 - .06980 radians)                                 !/*pi        126*/
!      4 -  8 degrees (.06980 - .13960 radians)                                 !/*pi        127*/
!      8 - 12 degrees (.13960 - .20940 radians)                                 !/*pi        128*/
!     12 - 20 degrees (.20940 - .34910 radians)                                 !/*pi        129*/
!     20 - 45 degrees (.34910 - .78540 radians)                                 !/*pi        130*/
      iset=0                                                                    !/*pi        131*/
      f3=topi*f1                                                                !/*pi        132*/
      f4=topi*f2                                                                !/*pi        133*/
      a7=dp1/d1                                                                 !/*pi        134*/
      a8=dp1/d2                                                                 !/*pi        135*/
      f3a7=f3*a7                                                                !/*pi        136*/
      f4a8=f4*a8                                                                !/*pi        137*/
      a7=a7+a7                                                                  !/*pi        138*/
      r2=rang()                                                                 !/*pi        139*/
      r3=r2                                                                     !/*pi        140*/
      exl=-dp1                                                                  !/*pi        141*/
      f6l=-dp1                                                                  !/*pi        142*/
      ratt(1)=dp0                                                               !/*pi        143*/
      angfrs=dp0                                                                !/*pi        144*/
      anglas=ang(9)                                                             !/*pi        145*/
  130 tu=(angfrs+anglas)*dph                                                    !/*pi        146*/
      txu=tu*tu/a7                                                              !/*pi        147*/
      if (txu.gt.5.d1) txu=5.d1                                                 !/*pi        148*/
      exu=-exp(-txu)                                                            !/*pi        149*/
      f6u=dp0                                                                   !/*pi        150*/
      if (psa6*tu.gt.5.d1) go to 140                                            !/*pi        151*/
      f6u=exp(-psa6*tu)*(-psa6*tu-dp1)                                          !/*pi        152*/
  140 f5=exu-exl                                                                !/*pi        153*/
      f6=f6u-f6l                                                                !/*pi        154*/
      rat=f3a7*f5+f4a8*f6                                                       !/*pi        155*/
      ratt(2)=rat/dpsmx                                                         !/*pi        156*/
      if (abs(anglas-tu)/anglas.lt.1.d-4) go to 160                             !/*pi        157*/
      if (r3.le.ratt(2)) go to 150                                              !/*pi        158*/
      angfrs=tu                                                                 !/*pi        159*/
      go to 130                                                                 !/*pi        160*/
  150 anglas=tu                                                                 !/*pi        161*/
      go to 130                                                                 !/*pi        162*/
  160 tht=tu                                                                    !/*pi        163*/
!                                                                               !/*pi        164*/
!     find angles -                                                             !/*pi        165*/
!                                                                               !/*pi        166*/
      g=cos(tht)                                                                !/*pi        167*/
      call azirn (csp,snp)                                                      !/*pi        168*/
      z=sqrt(dp1-g*g)                                                           !/*pi        169*/
      a=z*csp                                                                   !/*pi        170*/
      b=z*snp                                                                   !/*pi        171*/
!                                                                               !/*pi        172*/
      ener=sqrt(pss+rm*rm)-rm                                                   !/*pi        173*/
      if (ener.lt.dp0) ener=1.d-6                                               !/*pi        174*/
      pmom=ps                                                                   !/*pi        175*/
      return                                                                    !/*pi        176*/
      end                                                                       !/*pi        177*/

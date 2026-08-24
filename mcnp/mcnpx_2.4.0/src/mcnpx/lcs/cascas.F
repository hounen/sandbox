!     $Id: cascas.F,v 1.6 2002/03/27 18:14:56 gwm Exp $                       !/*ca          1*/
      subroutine cascas                                                         !/*ca          2*/
#include <lcs/double.h>                                                         /*!ca          3*/
!     ==================================================================        !/*ca          4*/
!                                                                               !/*ca          5*/
! CASCAS calls   SQRT  EXP  ALOG  CII  CIJ  FLTRNF  FINLII  FINLIJ              !/*ca          6*/
!                AITOTA  PTOTAL  PTOTKL  CEP  ISEDT                             !/*ca          7*/
! called by ISABEL                                                              !/*ca          8*/
!     ==================================================================        !/*ca          9*/
!                                                                               !/*ca         10*/
#include <lcs/inout.h>                                                          /*!ca         11*/
#include <lcs/vegas.h>                                                          /*!ca         12*/
!       ap-scattering and total cross-sections (from old sub "aptota")          !/*ca         13*/
!                                                                               !/*ca         14*/
      aptas(e)=exp(4.6051702d0-1.03651539d0*exp(.3808475d0*log(max(1.d-0        !/*ca         15*/
     & 3,1.d-03*e))))                                                           !/*ca         16*/
      aptat(e)=exp(4.5485063d0*exp(-.06010215d0*log(max(1.d-03,1.d-03*e)        !/*ca         17*/
     & )))                                                                      !/*ca         18*/
!              sig(e) charge-exchange                                           !/*ca         19*/
      aptac(e)=4.4d0/sqrt(max(1.d-03,1.d-03*e))                                 !/*ca         20*/
!                                                                               !/*ca         21*/
!     rtar2 is the squered radius of the target                                 !/*ca         22*/
!     pi is multiplied by 10 since the cross sections have to be                !/*ca         23*/
!     converted from mb. to fm**2                                               !/*ca         24*/
      pi=31.4159d0                                                              !/*ca         25*/
!     set default to no interaction                                             !/*ca         26*/
      ind(4)=1                                                                  !/*ca         27*/
!     dns2,dns3 are the squere and cube of particle density                     !/*ca         28*/
!     stand. dev.   dnf=(2*pi)**(-3/2)                                          !/*ca         29*/
      dnf=0.0634936d0                                                           !/*ca         30*/
      dns2=dp1                                                                  !/*ca         31*/
      dns3=dp1                                                                  !/*ca         32*/
      ind(32)=2                                                                 !/*ca         33*/
      if (ws(3).eq.13..or.ws(3).eq.25.) ind(12)=1                               !/*ca         34*/
      if (ind(1).eq.1) return                                                   !/*ca         35*/
      iws2=ws(2)/dp5+1.d-03                                                     !/*ca         36*/
      if (iws2.eq.1.and.ws(31).gt.dp0) return                                   !/*ca         37*/
!---- particles below cutoff do not cas-cas (y. yariv 12/31/86)                 !/*ca         38*/
      if (ws(31).eq.5.d+02) return                                              !/*ca         39*/
!----                                                                           !/*ca         40*/
      i7=icsb(7)                                                                !/*ca         41*/
      if (ws(3).gt.dp2.or.ws(3).le.-dp1) i7=-1                                  !/*ca         42*/
      x1=ws(10)                                                                 !/*ca         43*/
      y1=ws(11)                                                                 !/*ca         44*/
      z1=ws(12)                                                                 !/*ca         45*/
      p1=sqrt(ws(5)**2-ws(4)**2)                                                !/*ca         46*/
!                                                                               !/*ca         47*/
!     certain criteria must not be satisfied for                                !/*ca         48*/
!     a real cas-cas interaction:                                               !/*ca         49*/
!     st(i*i81+1)   .le. 0          ie. particle is dead                        !/*ca         50*/
!     ((st(i*i81+2)   .lt. 0).and.    ie. particle is virgin,                   !/*ca         51*/
!     (st(i*i81+i81) .gt. 0))            unreacted projectile                   !/*ca         52*/
!     ((st(i*i81+3)   .lt. 1.).or.    ie. particle is not                       !/*ca         53*/
!     (st(i*i81+3)   .gt. 2.))           proton or neutron                      !/*ca         54*/
!     st(i*i81)*0.2d0 .ge. 11         ie. particle is thermalized               !/*ca         55*/
   10 i7=i7+1                                                                   !/*ca         56*/
      i6=i7*i81                                                                 !/*ca         57*/
      if (st(i6+1)) 10,350,20                                                   !/*ca         58*/
   20 if (st(i6+3).le.-dp1) go to 10                                            !/*ca         59*/
      if (st(i6+3).gt.dp2) go to 10                                             !/*ca         60*/
      if (st(i6+2).lt.6.d0.and.st(i6+i81).gt.dp0) go to 10                      !/*ca         61*/
      if (st(i6+i81).eq.5.d+02) go to 10                                        !/*ca         62*/
      j2=0.2d0*st(i6+2)+1.d-03                                                  !/*ca         63*/
      if (j2.ge.11) go to 10                                                    !/*ca         64*/
      icsb(17)=i7                                                               !/*ca         65*/
      k=iws2*5                                                                  !/*ca         66*/
      mk=i81*icsb(7)+11+k                                                       !/*ca         67*/
      j=j2*5                                                                    !/*ca         68*/
      mj=i6+11+j                                                                !/*ca         69*/
!                                                                               !/*ca         70*/
!     the last interaction of each particle must                                !/*ca         71*/
!     not have been with each other                                             !/*ca         72*/
!                                                                               !/*ca         73*/
      if (st(mj+1).ne.st(mk+1)) go to 30                                        !/*ca         74*/
      if (st(mj+2).ne.st(mk+2)) go to 30                                        !/*ca         75*/
      if (st(mj+3).ne.st(mk+3)) go to 30                                        !/*ca         76*/
      if (st(mj+4).ne.st(mk+4)) go to 30                                        !/*ca         77*/
      go to 10                                                                  !/*ca         78*/
   30 x2=st(i6+10)                                                              !/*ca         79*/
      y2=st(i6+11)                                                              !/*ca         80*/
      z2=st(i6+12)                                                              !/*ca         81*/
!-----------------------------------------------------------------------        !/*ca         82*/
! Third attempt to fix CASCAS.                                                  !/*ca         83*/
      if (ws(14).gt.csa(9)) istept=0                                            !/*ca         84*/
!-----------------------------------------------------------------------        !/*ca         85*/
!                                                                               !/*ca         86*/
!  for particles with "non invariant" distance > rcas                           !/*ca         87*/
!   no cascade-cascade interaction                                              !/*ca         88*/
!                                                                               !/*ca         89*/
      ss=(x1-x2)**2+(y1-y2)**2+(z1-z2)**2                                       !/*ca         90*/
      if (ss.gt.rcas) go to 10                                                  !/*ca         91*/
!                                                                               !/*ca         92*/
      wsp(15)=ws(6)*st(i6+6)+ws(7)*st(i6+7)+ws(8)*st(i6+8)                      !/*ca         93*/
      e2=st(i6+5)                                                               !/*ca         94*/
      am2=st(i6+4)                                                              !/*ca         95*/
      p2=sqrt(e2*e2-am2*am2)                                                    !/*ca         96*/
!-----factors of contraction in partner f.o.r. [y.y. 9/5/84]                    !/*ca         97*/
      gmx=dp1-(p2*st(i6+6)/e2)**2                                               !/*ca         98*/
      gmy=dp1-(p2*st(i6+7)/e2)**2                                               !/*ca         99*/
      gmz=dp1-(p2*st(i6+8)/e2)**2                                               !/*ca        100*/
!  ss is a properly lorentz-contracted distance                                 !/*ca        101*/
      ss=(x1-x2)**2/gmx+(y1-y2)**2/gmy+(z1-z2)**2/gmz                           !/*ca        102*/
!  calculate the relative energy                                                !/*ca        103*/
!       err = 4p1.4p2, an invariant                                             !/*ca        104*/
      err=ws(5)*e2-p1*p2*wsp(15)                                                !/*ca        105*/
      er=(err/am2-ws(4))                                                        !/*ca        106*/
      etep(1)=er                                                                !/*ca        107*/
      etep(2)=er                                                                !/*ca        108*/
!  calculate density and interaction probability                                !/*ca        109*/
      dens=dnf/dns3*exp(-ss/dp2/dns2)                                           !/*ca        110*/
      wsp(20)=dens/p1/e2                                                        !/*ca        111*/
      wsp(21)=am2*am2*ws(4)*ws(4)                                               !/*ca        112*/
!        err = 4p1.4p2 = e1-prime.m2 in system with m2 at rest                  !/*ca        113*/
!            then wsp21 = rho*m2*p1-prime/e2/p1                                 !/*ca        114*/
!                       = rho/gamma * (p1-prime/p1)                             !/*ca        115*/
!                             = rel. contraction                                !/*ca        116*/
!                                     = phase-space factor                      !/*ca        117*/
      wsp(21)=wsp(20)*sqrt(err*err-wsp(21))                                     !/*ca        118*/
      if (ws(3).ge.dp0) go to 40                                                !/*ca        119*/
!  get the cross section for ap                                                 !/*ca        120*/
      apscat=aptas(er)                                                          !/*ca        121*/
      apchx=aptac(er)                                                           !/*ca        122*/
      aptot=aptat(er)                                                           !/*ca        123*/
      c=aptot                                                                   !/*ca        124*/
      go to 60                                                                  !/*ca        125*/
   40 if (ws(3).gt.dp2) go to 140                                               !/*ca        126*/
      if (er.lt.3.d1) go to 10                                                  !/*ca        127*/
      if (ws(3).ne.st(i6+3)) go to 50                                           !/*ca        128*/
      c=cii(er)                                                                 !/*ca        129*/
      go to 60                                                                  !/*ca        130*/
   50 continue                                                                  !/*ca        131*/
      c=cij(er)                                                                 !/*ca        132*/
   60 wsp(20)=c*wsp(20)                                                         !/*ca        133*/
      wsp(21)=c*wsp(21)                                                         !/*ca        134*/
!         wsp21 = inverse mean-free-path                                        !/*ca        135*/
      selpt1=ntdel*ws(20)*wsp(21)                                               !/*ca        136*/
!       selpt1/wsp21*rand = fract. of mfp to move                               !/*ca        137*/
!                         <= 3 old intervals                                    !/*ca        138*/
      rand=rang()                                                               !/*ca        139*/
      if (rand.ge.selpt1) go to 10                                              !/*ca        140*/
   70 go to 80                                                                  !/*ca        141*/
!  30 j=j-5                                                                     !/*ca        142*/
!     if(j-5) 24,23,22                                                          !/*ca        143*/
!  23 if(st(i6+i81).gt.dp0) go to 24                                            !/*ca        144*/
!  22 mj=i6+11+j                                                                !/*ca        145*/
!     if(st(mk+1).ne.st(mj+1)) go to 30                                         !/*ca        146*/
!  26 if(st(mk+2).ne.st(mj+2)) go to 30                                         !/*ca        147*/
!     if(st(mk+3).ne.st(mj+3)) go to 30                                         !/*ca        148*/
!     if(st(mk+4).ne.st(mj+4)) go to 30                                         !/*ca        149*/
!     ss=(x1-st(mj+2))**2+(y1-st(mj+3))**2+(z1-st(mj+4))**2                     !/*ca        150*/
!     if(ss.lt.ri) go to 7                                                      !/*ca        151*/
!     go to 30                                                                  !/*ca        152*/
!     go to 7                                                                   !/*ca        153*/
!  24 k=k-5                                                                     !/*ca        154*/
!     if(k-5) 50,43,42                                                          !/*ca        155*/
!  43 if(ws(31).gt.dp0) go to 50                                                !/*ca        156*/
!  42 mk=mk-5                                                                   !/*ca        157*/
!     j=j2*5+5                                                                  !/*ca        158*/
!     go to 30                                                                  !/*ca        159*/
   80 do 90 i=1,8                                                               !/*ca        160*/
   90 wsp(i)=st(i6+i)                                                           !/*ca        161*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*ca        162*/
      wsp(24)=st(i6+16)                                                         !/*ca        163*/
      wsp(31)=st(i6+i81)                                                        !/*ca        164*/
      if (ws(3).gt.dp10) go to 250                                              !/*ca        165*/
      if (ws(3).le.-dp1) go to 120                                              !/*ca        166*/
      if (ws(3).gt.dp2) go to 240                                               !/*ca        167*/
!                                                                               !/*ca        168*/
      ind(28)=0                                                                 !/*ca        169*/
      if (ws(3).ne.st(i6+3)) go to 100                                          !/*ca        170*/
      finel=finlii(er)                                                          !/*ca        171*/
      go to 110                                                                 !/*ca        172*/
  100 continue                                                                  !/*ca        173*/
      finel=finlij(er)                                                          !/*ca        174*/
  110 continue                                                                  !/*ca        175*/
      ranfe=rang()                                                              !/*ca        176*/
      if (ranfe.lt.finel) ind(28)=1                                             !/*ca        177*/
      go to 250                                                                 !/*ca        178*/
  120 ind(28)=0                                                                 !/*ca        179*/
      if (abs(ws(3)).ne.abs(wsp(3))) apchx=dp0                                  !/*ca        180*/
      rand1=rang()                                                              !/*ca        181*/
      fsc=(apscat+apchx)/aptot                                                  !/*ca        182*/
      if (rand1.ge.fsc) go to 130                                               !/*ca        183*/
!            now either elastic or charge-exchange                              !/*ca        184*/
      if (apchx.eq.dp0) go to 250                                               !/*ca        185*/
!             either nbar+n or pbar+p                                           !/*ca        186*/
      rand1=rang()                                                              !/*ca        187*/
      fcx=apchx/(apchx+apscat)                                                  !/*ca        188*/
      if (rand1.ge.fcx) go to 250                                               !/*ca        189*/
!                    charge-exchange                                            !/*ca        190*/
      ind(29)=2                                                                 !/*ca        191*/
      go to 250                                                                 !/*ca        192*/
!                     annihilation                                              !/*ca        193*/
  130 ind(28)=2                                                                 !/*ca        194*/
      go to 250                                                                 !/*ca        195*/
!                                                                               !/*ca        196*/
!                 pion interactions                                             !/*ca        197*/
!                                                                               !/*ca        198*/
  140 e=sqrt(err+err+ws(4)*ws(4)+am2*am2)                                       !/*ca        199*/
      e10=err/am2                                                               !/*ca        200*/
      p10=sqrt(e10**2-ws(4)**2)                                                 !/*ca        201*/
      wsp(3)=st(i6+3)                                                           !/*ca        202*/
      if (ws(3).lt.dp10) go to 150                                              !/*ca        203*/
      wsp(4)=st(i6+4)                                                           !/*ca        204*/
      v=wsp(4)*1.d-03                                                           !/*ca        205*/
      u=e*1.d-03                                                                !/*ca        206*/
      call aitota (er,dp0,dp0,u,v,wsp)                                          !/*ca        207*/
      wsp(20)=aitot*wsp(20)                                                     !/*ca        208*/
      wsp(21)=aitot*wsp(21)                                                     !/*ca        209*/
      selpt1=ntdel*ws(20)*wsp(21)                                               !/*ca        210*/
      rand=rang()                                                               !/*ca        211*/
      if (rand.ge.selpt1) go to 10                                              !/*ca        212*/
      go to 70                                                                  !/*ca        213*/
  150 continue                                                                  !/*ca        214*/
      if ((e.gt.66.d1).and.(ws(3).lt.6.d0)) go to 10                            !/*ca        215*/
      if ((p10.gt.1.2d+03).and.(ws(3).gt.6.d0)) go to 10                        !/*ca        216*/
      ind9=0                                                                    !/*ca        217*/
      ind10=0                                                                   !/*ca        218*/
      ind11=0                                                                   !/*ca        219*/
      if (ws(3).gt.6.d0) go to 200                                              !/*ca        220*/
      if (ws(3)-dp4) 190,210,160                                                !/*ca        221*/
  160 if (wsp(3).eq.dp2) go to 180                                              !/*ca        222*/
  170 ind10=1                                                                   !/*ca        223*/
      go to 220                                                                 !/*ca        224*/
  180 ind9=1                                                                    !/*ca        225*/
      go to 220                                                                 !/*ca        226*/
  190 if (wsp(3).eq.dp2) go to 170                                              !/*ca        227*/
      go to 180                                                                 !/*ca        228*/
  200 if (ws(3)-8.d0) 190,210,160                                               !/*ca        229*/
  210 ind11=1                                                                   !/*ca        230*/
  220 ind(9)=ind9                                                               !/*ca        231*/
      ind(10)=ind10                                                             !/*ca        232*/
      ind(11)=ind11                                                             !/*ca        233*/
      typ=ws(3)-8.d0                                                            !/*ca        234*/
      if (ws(3).lt.6.d0) call ptotal (e,ind(9),ind(10),ind(11),scat             !/*ca        235*/
     & ,absor)                                                                  !/*ca        236*/
      if (ws(3).lt.6.d0) go to 230                                              !/*ca        237*/
      call ptotkl (p10,typ,scp,scn,abp,abn)                                     !/*ca        238*/
      scat=scp                                                                  !/*ca        239*/
      if (wsp(3).eq.dp2) scat=scn                                               !/*ca        240*/
  230 wsp(20)=scat*wsp(20)                                                      !/*ca        241*/
      wsp(21)=scat*wsp(21)                                                      !/*ca        242*/
      selpt1=ntdel*ws(20)*wsp(21)                                               !/*ca        243*/
      rand=rang()                                                               !/*ca        244*/
      if (rand.ge.selpt1) go to 10                                              !/*ca        245*/
      go to 70                                                                  !/*ca        246*/
  240 ind(16)=1                                                                 !/*ca        247*/
      if (e.lt.25.d1) go to 250                                                 !/*ca        248*/
      ind(16)=0                                                                 !/*ca        249*/
      i9=ind9                                                                   !/*ca        250*/
      i10=ind10                                                                 !/*ca        251*/
      i11=ind11                                                                 !/*ca        252*/
      tcm=e                                                                     !/*ca        253*/
!-----due to "ntdel" pushing particle can cause troubles.                       !/*ca        254*/
!-----leave it as it is (including ws(16)) when ntdel.ne.1                      !/*ca        255*/
!-----[yy 8/16/84]                                                              !/*ca        256*/
  250 wsp(13)=ws(16)                                                            !/*ca        257*/
      if (ntdel.ne.1) go to 270                                                 !/*ca        258*/
      ws(20)=rand/wsp(21)                                                       !/*ca        259*/
      ws(16)=ws(16)-ws(20)/sqrt(gws(5)*gws(5)-ws(4)*ws(4))/gws(5)               !/*ca        260*/
      wsp(13)=ws(16)                                                            !/*ca        261*/
!  gdc9 is the distance to the collision site in fermi.                         !/*ca        262*/
      gdc9=ws(20)*dp10                                                          !/*ca        263*/
      do 260 i=1,3                                                              !/*ca        264*/
      ws(i+9)=gdc9*gws(i+5)+gws(i+9)                                            !/*ca        265*/
  260 continue                                                                  !/*ca        266*/
  270 ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*ca        267*/
      ws(14)=sqrt(ws(25))                                                       !/*ca        268*/
      do 280 i=5,8                                                              !/*ca        269*/
  280 ws(i)=gws(i)                                                              !/*ca        270*/
      ws(24)=gws(2)                                                             !/*ca        271*/
!  bring the partner to the collision site (i.e. the position                   !/*ca        272*/
!  of the principal particle).                                                  !/*ca        273*/
!  this may change the region  - correct energy .                               !/*ca        274*/
!                                                                               !/*ca        275*/
      wsp(10)=ws(10)                                                            !/*ca        276*/
      wsp(11)=ws(11)                                                            !/*ca        277*/
      wsp(12)=ws(12)                                                            !/*ca        278*/
      wsp(14)=ws(14)                                                            !/*ca        279*/
      wsp(25)=ws(25)                                                            !/*ca        280*/
      iwsp3=wsp(3)                                                              !/*ca        281*/
      efo=wsp(24)                                                               !/*ca        282*/
      if (wsp(31).ne.dp0) go to 300                                             !/*ca        283*/
      efn=-csa(5)                                                               !/*ca        284*/
      if (istept.le.0) go to 290                                                !/*ca        285*/
      efn=ef(iwsp3,istept)                                                      !/*ca        286*/
      wsp(24)=efn                                                               !/*ca        287*/
  290 dele=efn-efo                                                              !/*ca        288*/
      go to 320                                                                 !/*ca        289*/
  300 efn=-csa(15)                                                              !/*ca        290*/
      if (istepp.le.0) go to 310                                                !/*ca        291*/
      efn=pef(iwsp3,istepp)                                                     !/*ca        292*/
      wsp(24)=efn                                                               !/*ca        293*/
  310 delep=efn-efo                                                             !/*ca        294*/
      call cep (delep,dele,wsp(4),wsp)                                          !/*ca        295*/
  320 wsp(5)=wsp(5)+dele                                                        !/*ca        296*/
      if (wsp(5).gt.wsp(4)) go to 330                                           !/*ca        297*/
      ind(16)=0                                                                 !/*ca        298*/
      ind(28)=0                                                                 !/*ca        299*/
      ind(9)=0                                                                  !/*ca        300*/
      ind(10)=0                                                                 !/*ca        301*/
      ind(11)=0                                                                 !/*ca        302*/
      i9=0                                                                      !/*ca        303*/
      i10=0                                                                     !/*ca        304*/
      i11=0                                                                     !/*ca        305*/
      go to 10                                                                  !/*ca        306*/
  330 wsp(9)=sqrt(wsp(5)**2-wsp(4)**2)                                          !/*ca        307*/
      ind(4)=0                                                                  !/*ca        308*/
!                                                                               !/*ca        309*/
!     bringing partner to collision site may have changed the number            !/*ca        310*/
!     of particles in target area. Correct it. [yy 24jul84]                     !/*ca        311*/
!     define wsp14 [yy 24 jul84], old posn of partner                           !/*ca        312*/
      wsp14=sqrt(x2*x2+y2*y2+z2*z2)                                             !/*ca        313*/
      i=wsp(3)                                                                  !/*ca        314*/
!-----------------------------------------------------------------------        !/*ca        315*/
! Second attempt to fix CASCAS. ISTEPT may be 1 at this point,                  !/*ca        316*/
! even though the incident particle is outside the target nucleus.              !/*ca        317*/
      if (istept.eq.0.or.ws(14).gt.csa(9)) go to 340                            !/*ca        318*/
!-----------------------------------------------------------------------        !/*ca        319*/
      if (wsp14.lt.csa(9)) return                                               !/*ca        320*/
      npmon(i)=npmon(i)+1                                                       !/*ca        321*/
      npmon(3)=npmon(3)+1                                                       !/*ca        322*/
#ifdef NPM                                                                      /*!ca        323*/
      write (io,360) npmon,(ind(iii),iii=1,35)                                  !/*ca        324*/
#endif                                                                          /*!ca        325*/
!-----------------------------------------------------------------------        !/*ca        326*/
! First attempt to fix CASCAS.                                                  !/*ca        327*/
      wsp(30)=-99.d0                                                            !/*ca        328*/
!-----------------------------------------------------------------------        !/*ca        329*/
      return                                                                    !/*ca        330*/
  340 if (wsp14.gt.csa(9)) return                                               !/*ca        331*/
      ind34=ind(34)                                                             !/*ca        332*/
      ind(34)=2                                                                 !/*ca        333*/
      istep0=istept                                                             !/*ca        334*/
      if (istept.eq.0) istept=1                                                 !/*ca        335*/
      call isedt (wsp)                                                          !/*ca        336*/
      istept=istep0                                                             !/*ca        337*/
      ind(34)=ind34                                                             !/*ca        338*/
!-----------------------------------------------------------------------        !/*ca        339*/
! First attempt to fix CASCAS.                                                  !/*ca        340*/
      wsp(30)=+99.d0                                                            !/*ca        341*/
!-----------------------------------------------------------------------        !/*ca        342*/
!                                                                               !/*ca        343*/
  350 continue                                                                  !/*ca        344*/
      return                                                                    !/*ca        345*/
!                                                                               !/*ca        346*/
  360 format ('cascas.502 +',3i5/35i3)                                          !/*ca        347*/
      end                                                                       !/*ca        348*/

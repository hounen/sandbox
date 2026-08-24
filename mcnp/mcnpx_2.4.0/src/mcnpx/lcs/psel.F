!     $Id: psel.F,v 1.5 2002/03/27 18:15:17 gwm Exp $                         !/*ps          1*/
      subroutine psel (gdc9,gdc10)                                              !/*ps          2*/
#include <lcs/double.h>                                                         /*!ps          3*/
!     ==================================================================        !/*ps          4*/
!     psel selects the momentum of the partner of collision.                    !/*ps          5*/
!                                                                               !/*ps          6*/
! PSEL   calls   SQRT  PLT  ISEDT  COUNT  GETWIG  FLTRNF  COS  REGB             !/*ps          7*/
!                REFL  PTOTAL  PTOTKL  IFRIST  NOPART  SAVE  CEP                !/*ps          8*/
!                WWSST  WSSTO  UPDECA                                           !/*ps          9*/
! called by ISABEL  RLAR                                                        !/*ps         10*/
!     ==================================================================        !/*ps         11*/
!                                                                               !/*ps         12*/
#include <lcs/inout.h>                                                          /*!ps         13*/
#include <lcs/vegas.h>                                                          /*!ps         14*/
      dimension p(4), pp(4), pt(4)                                              !/*ps         15*/
!                                                                               !/*ps         16*/
      bet=-voc                                                                  !/*ps         17*/
      iref=0                                                                    !/*ps         18*/
      ipisw=0                                                                   !/*ps         19*/
      ind21=ind(21)                                                             !/*ps         20*/
!  when a proton or a neutron is leaving target (projectile)                    !/*ps         21*/
!  its energy is compared to the cutoff (or the binding) energy.                !/*ps         22*/
!  if a "below cutoff" target (projectile) particle leaves                      !/*ps         23*/
!  the projectile (target) ind(20)=1,and the particle is                        !/*ps         24*/
!  "killed" in gamma. the same holds for a target (projectile)                  !/*ps         25*/
!  particle which is energetically forbidden to leave the target                !/*ps         26*/
!  (projectile).                                                                !/*ps         27*/
      must=0                                                                    !/*ps         28*/
      if ((ws(3).gt.6.d0).and.(ws(3).lt.dp10)) ind(16)=0                        !/*ps         29*/
      if (ind(32).eq.2) go to 600                                               !/*ps         30*/
!       not a cas-cas interaction                                               !/*ps         31*/
      iws3=ws(3)                                                                !/*ps         32*/
      if (ind(34).eq.0.and.ind(8).eq.0) go to 60                                !/*ps         33*/
!      particle leaves its system or region                                     !/*ps         34*/
      if (iws3.gt.2.and.iws3.lt.10) go to 60                                    !/*ps         35*/
!          nucleons or isobars need partner                                     !/*ps         36*/
      if (ws(31).eq.dp0) go to 10                                               !/*ps         37*/
!          particle is projectile-like                                          !/*ps         38*/
!          so transform its (cm) to target lab for partner choice               !/*ps         39*/
      pt(1)=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                       !/*ps         40*/
      pt(2)=ws(6)                                                               !/*ps         41*/
      pt(3)=ws(7)                                                               !/*ps         42*/
      pt(4)=ws(8)                                                               !/*ps         43*/
!     -----------------------------------                                       !/*ps         44*/
      call plt (pt,ws(5),pp,ee,bet,gama)                                        !/*ps         45*/
!     -----------------------------------                                       !/*ps         46*/
   10 if (iws3.gt.2.or.ind(34).eq.0) go to 60                                   !/*ps         47*/
!             if nucleon wants to leave (tar,proj,sys), check cutoffs           !/*ps         48*/
      if (ws(31).eq.dp0) go to 40                                               !/*ps         49*/
!-----------------------------------------------------------------------        !/*ps         50*/
! A "5.d+02 virgin" leaving the target must be deflowered.                      !/*ps         51*/
      if (ws(31).eq.5.d+02) then                                                !/*ps         52*/
        ws(31)=-dp1                                                             !/*ps         53*/
        icsb13=icsb(13)                                                         !/*ps         54*/
        st(icsb13+i81)=-dp1                                                     !/*ps         55*/
      endif                                                                     !/*ps         56*/
!-----------------------------------------------------------------------        !/*ps         57*/
      cutoff=csa(15)                                                            !/*ps         58*/
      if (ind(34).eq.2) cutoff=cutofa(iws3+9)                                   !/*ps         59*/
!-----------------------------------------------------------------------        !/*ps         60*/
      valqq=cutoff+pef(iws3,istepp)+ws(4)-ee                                    !/*ps         61*/
!-----------------------------------------------------------------------        !/*ps         62*/
      if (valqq.lt.0) go to 60                                                  !/*ps         63*/
!-----if a projectile particle falls below cutoff leaving the target            !/*ps         64*/
!-----or leaving the projectile within target volume it should enter            !/*ps         65*/
!-----the aa vector (y. yariv 12/31/86)                                         !/*ps         66*/
!-----------------------------------------------------------------------        !/*ps         67*/
!     if (ind(34).eq.2) go to 12                                                !/*ps         68*/
      if (ind(34).eq.2) go to 20                                                !/*ps         69*/
!-----------------------------------------------------------------------        !/*ps         70*/
      if (ind(34).eq.1.and.istept.gt.0) go to 20                                !/*ps         71*/
      go to 30                                                                  !/*ps         72*/
   20 ind34=ind(34)                                                             !/*ps         73*/
      ind(34)=2                                                                 !/*ps         74*/
!     -----------------------------------                                       !/*ps         75*/
      call isedt (ws)                                                           !/*ps         76*/
!     -----------------------------------                                       !/*ps         77*/
      ind(34)=ind34                                                             !/*ps         78*/
      if (kcan.ne.0) go to 810                                                  !/*ps         79*/
!                                                                               !/*ps         80*/
   30 expr=expr+ee-pef(iws3,istepp)-ws(4)                                       !/*ps         81*/
! put in nucleon                                                                !/*ps         82*/
      call countv (ws(3),2,2)                                                   !/*ps         83*/
      ws10=ws(10)-proj(5)                                                       !/*ps         84*/
      ws11=ws(11)-proj(6)                                                       !/*ps         85*/
      ws12=(ws(12)-proj(7))/gama                                                !/*ps         86*/
      pir(1)=pir(1)+pp(1)*pp(2)                                                 !/*ps         87*/
      pir(2)=pir(2)+pp(1)*pp(3)                                                 !/*ps         88*/
      pir(3)=pir(3)+pp(1)*pp(4)                                                 !/*ps         89*/
      pir(4)=pir(4)+pp(1)*(ws11*pp(4)-ws12*pp(3))                               !/*ps         90*/
      pir(5)=pir(5)+pp(1)*(ws12*pp(2)-ws10*pp(4))                               !/*ps         91*/
      pir(6)=pir(6)+pp(1)*(ws10*pp(3)-ws11*pp(2))                               !/*ps         92*/
      go to 50                                                                  !/*ps         93*/
   40 cutoff=csa(5)                                                             !/*ps         94*/
      if (ind(34).eq.1) cutoff=cutofa(iws3+2)                                   !/*ps         95*/
      if (ws(3).le.-dp1) cutoff=dp0                                             !/*ps         96*/
!-----------------------------------------------------------------------        !/*ps         97*/
      if ((cutoff+ef(iws3,istept)+ws(4)-ws(5)).lt.dp0) go to 60                 !/*ps         98*/
!-----------------------------------------------------------------------        !/*ps         99*/
   50 ind(20)=1                                                                 !/*ps        100*/
      if (ws(31).ne.dp0.or.ws(3).le.-dp1) go to 810                             !/*ps        101*/
!                                                                               !/*ps        102*/
!-----------------------------------------------------------------------        !/*ps        103*/
      ex=ex+ws(5)-ws(4)-ef(iws3,istept)                                         !/*ps        104*/
!-----------------------------------------------------------------------        !/*ps        105*/
! put in nucleon                                                                !/*ps        106*/
      call countv (ws(3),1,2)                                                   !/*ps        107*/
      pm=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                          !/*ps        108*/
      px=ws(6)*pm                                                               !/*ps        109*/
      py=ws(7)*pm                                                               !/*ps        110*/
      pz=ws(8)*pm                                                               !/*ps        111*/
      pit(1)=pit(1)+px                                                          !/*ps        112*/
      pit(2)=pit(2)+py                                                          !/*ps        113*/
      pit(3)=pit(3)+pz                                                          !/*ps        114*/
      pit(4)=pit(4)+ws(11)*pz-ws(12)*py                                         !/*ps        115*/
      pit(5)=pit(5)+ws(12)*px-ws(10)*pz                                         !/*ps        116*/
      pit(6)=pit(6)+ws(10)*py-ws(11)*px                                         !/*ps        117*/
      go to 810                                                                 !/*ps        118*/
!                                                                               !/*ps        119*/
!                everything needs a partner                                     !/*ps        120*/
!                                                                               !/*ps        121*/
   60 if (ind(30).eq.1) go to 70                                                !/*ps        122*/
!             interaction in target is possible                                 !/*ps        123*/
      wsp(18)=den_(istept)                                                      !/*ps        124*/
      ll=wsp(3)+0.1d0                                                           !/*ps        125*/
      wsp(24)=ef(ll,istept)                                                     !/*ps        126*/
!                                                                               !/*ps        127*/
!       particle is selected uniformly from the density at step "ll",           !/*ps        128*/
!       and fermi-momentum goes like den**1/3                                   !/*ps        129*/
!       gdc(10) is a random number between zero and one.                        !/*ps        130*/
!     wsp(9)=pfr(ll,istept)*gdc10**dpth                                         !/*ps        131*/
!                                                                               !/*ps        132*/
!     ---------------------------------------------------------                 !/*ps        133*/
      call getwig (pfr(ll,istept),pfw(ll,istept),wsp(9),gdc10)                  !/*ps        134*/
!     ---------------------------------------------------------                 !/*ps        135*/
      wsp(5)=sqrt(wsp(9)*wsp(9)+amass(ll)*amass(ll))                            !/*ps        136*/
      exth=amass(ll)+wsp(24)-wsp(5)                                             !/*ps        137*/
      fws3=ll                                                                   !/*ps        138*/
      if (ws(3).gt.-dp1) go to 70                                               !/*ps        139*/
      p2=rang()                                                                 !/*ps        140*/
      p2=dp2*p2-1                                                               !/*ps        141*/
      p4=sqrt(dp1-p2*p2)                                                        !/*ps        142*/
      p3=rang()                                                                 !/*ps        143*/
      p3=6.28318d0*p3                                                           !/*ps        144*/
      wsp29=cos(p3)                                                             !/*ps        145*/
      wsp28=sin(p3)                                                             !/*ps        146*/
      wsp(6)=p2                                                                 !/*ps        147*/
      wsp(7)=p4*wsp29                                                           !/*ps        148*/
      wsp(8)=p4*wsp28                                                           !/*ps        149*/
      wsp(15)=gws(6)*wsp(6)+gws(7)*wsp(7)+gws(8)*wsp(8)                         !/*ps        150*/
!                                                                               !/*ps        151*/
   70 if (ind(31).eq.1) go to 80                                                !/*ps        152*/
!            interaction in projectile is possible                              !/*ps        153*/
!                                                                               !/*ps        154*/
      l=wspr(3)                                                                 !/*ps        155*/
      wspr(18)=pdn(istepp)                                                      !/*ps        156*/
      wspr(24)=pef(l,istepp)                                                    !/*ps        157*/
      p(2)=rang()                                                               !/*ps        158*/
      p(2)=dp2*p(2)-dp1                                                         !/*ps        159*/
      p(4)=sqrt(dp1-p(2)*p(2))                                                  !/*ps        160*/
      rand=rang()                                                               !/*ps        161*/
      p(3)=6.28318d0*rand                                                       !/*ps        162*/
      wspr29=cos(p(3))                                                          !/*ps        163*/
      wspr28=sin(p(3))                                                          !/*ps        164*/
      p(3)=p(4)*wspr29                                                          !/*ps        165*/
      p(4)=p(4)*wspr28                                                          !/*ps        166*/
      rand=rang()                                                               !/*ps        167*/
!     p(1)=ppfr(l,istepp)*rand**dpth                                            !/*ps        168*/
!     ---------------------------------------------------------                 !/*ps        169*/
      call getwig (ppfr(l,istepp),ppfw(l,istepp),p(1),rand)                     !/*ps        170*/
!     ---------------------------------------------------------                 !/*ps        171*/
      e=sqrt(p(1)*p(1)+amass(l)*amass(l))                                       !/*ps        172*/
      eprh(1)=amass(l)+wspr(24)-e                                               !/*ps        173*/
      fwsp3=l                                                                   !/*ps        174*/
      eprh(2)=-p(2)*p(1)                                                        !/*ps        175*/
      eprh(3)=-p(3)*p(1)                                                        !/*ps        176*/
      eprh(4)=-p(4)*p(1)                                                        !/*ps        177*/
      wspr(5)=e                                                                 !/*ps        178*/
      wspr(9)=p(1)                                                              !/*ps        179*/
      wspr(6)=p(2)                                                              !/*ps        180*/
      wspr(7)=p(3)                                                              !/*ps        181*/
      wspr(8)=p(4)                                                              !/*ps        182*/
      wspr(28)=wspr(8)/sqrt(wspr(8)*wspr(8)+wspr(7)*wspr(7))                    !/*ps        183*/
      wspr(29)=wspr(7)/sqrt(wspr(8)*wspr(8)+wspr(7)*wspr(7))                    !/*ps        184*/
      wspr(15)=gws(6)*wspr(6)+gws(7)*wspr(7)+gws(8)*wspr(8)                     !/*ps        185*/
!                                                                               !/*ps        186*/
   80 if (ind(8).eq.0) go to 220                                                !/*ps        187*/
!     particle advance changes region                                           !/*ps        188*/
!                                                                               !/*ps        189*/
      ws(24)=dp0                                                                !/*ps        190*/
      efo=dp0                                                                   !/*ps        191*/
      efn=dp0                                                                   !/*ps        192*/
      gdc6=dp0                                                                  !/*ps        193*/
!                                                                               !/*ps        194*/
!     in psel if particle is pion then select partner and check if there        !/*ps        195*/
!     will be an interaction.  if there will be, then pion makes isobar.        !/*ps        196*/
!     otherwise pion advances in the nucleus.  statement 200 starts pion        !/*ps        197*/
!     interaction.                                                              !/*ps        198*/
!                                                                               !/*ps        199*/
      if (iws3.gt.2.and.iws3.lt.10) go to 220                                   !/*ps        200*/
!            determine charge of isobar, nucleon or anti-nucleon to             !/*ps        201*/
!            fix value of potential that it sits in.                            !/*ps        202*/
!                                                                               !/*ps        203*/
      iwsf=iws3                                                                 !/*ps        204*/
      if (iwsf.gt.10) iwsf=iwsf/10                                              !/*ps        205*/
      if (ws(31).ne.dp0) go to 120                                              !/*ps        206*/
      if (iwsf.ge.0) go to 100                                                  !/*ps        207*/
!                                                                               !/*ps        208*/
!     anti-nucleon in potential                                                 !/*ps        209*/
!                                                                               !/*ps        210*/
      iwsf=-iwsf                                                                !/*ps        211*/
      if (istept.eq.0) go to 90                                                 !/*ps        212*/
      efo=vap*ef(iwsf,istept)                                                   !/*ps        213*/
   90 efn=dp0                                                                   !/*ps        214*/
      if (istpt.gt.0) efn=vap*ef(iwsf,istpt)                                    !/*ps        215*/
      if (efn.gt.dp0) ws(24)=efn                                                !/*ps        216*/
      go to 140                                                                 !/*ps        217*/
!                                                                               !/*ps        218*/
!     +nucleon in potential                                                     !/*ps        219*/
!                                                                               !/*ps        220*/
  100 if (istept.eq.0) go to 110                                                !/*ps        221*/
      efo=ef(iwsf,istept)                                                       !/*ps        222*/
  110 efn=-csa(5)                                                               !/*ps        223*/
      if (istpt.gt.0) efn=ef(iwsf,istpt)                                        !/*ps        224*/
      if (efn.gt.dp0) ws(24)=efn                                                !/*ps        225*/
      go to 140                                                                 !/*ps        226*/
!                                                                               !/*ps        227*/
!     +nucleon in projectile potential                                          !/*ps        228*/
!                                                                               !/*ps        229*/
  120 if (istepp.eq.0) go to 130                                                !/*ps        230*/
      efo=pef(iwsf,istepp)                                                      !/*ps        231*/
  130 efn=-csa(15)                                                              !/*ps        232*/
      if (istpp.gt.0) efn=pef(iwsf,istpp)                                       !/*ps        233*/
      if (efn.gt.dp0) ws(24)=efn                                                !/*ps        234*/
!                                                                               !/*ps        235*/
  140 gdc6=efn-efo                                                              !/*ps        236*/
!                                                                               !/*ps        237*/
!     force the anti-nucleon to be refracted or reflected due to                !/*ps        238*/
!     the strong attraction irrespective of the model used                      !/*ps        239*/
!                                                                               !/*ps        240*/
      if (ws(3).le.-dp1.and.ind(34).ne.0) go to 190                             !/*ps        241*/
      go to (190,190,150,190,190,150), ind21                                    !/*ps        242*/
!                                                                               !/*ps        243*/
!     for step without refraction when particle is crossing change energ        !/*ps        244*/
!     but not directions.                                                       !/*ps        245*/
!                                                                               !/*ps        246*/
  150 if (ws(31).ne.dp0) go to 160                                              !/*ps        247*/
      ws(5)=ws(5)+gdc6                                                          !/*ps        248*/
      if (ws(5).gt.ws(4)) go to 220                                             !/*ps        249*/
      ws(5)=ws(5)-gdc6                                                          !/*ps        250*/
      go to 170                                                                 !/*ps        251*/
  160 ee=ee+gdc6                                                                !/*ps        252*/
      if (ee.lt.ws(4)) go to 170                                                !/*ps        253*/
      pp(1)=sqrt(ee*ee-ws(4)*ws(4))                                             !/*ps        254*/
!     ---------------------------------------------------------                 !/*ps        255*/
      call plt (pp,ee,pt,ws(5),voc,gama)                                        !/*ps        256*/
!     ---------------------------------------------------------                 !/*ps        257*/
!                                                                               !/*ps        258*/
!     there is no change in directions in the projectile frame,                 !/*ps        259*/
!     however there is a change in the target frame.                            !/*ps        260*/
!                                                                               !/*ps        261*/
      ws(6)=pt(2)                                                               !/*ps        262*/
      ws(7)=pt(3)                                                               !/*ps        263*/
      ws(8)=pt(4)                                                               !/*ps        264*/
      go to 220                                                                 !/*ps        265*/
!                                                                               !/*ps        266*/
!     if the energy of a particle (isobar) becomes negative when crossin        !/*ps        267*/
!     ind(22)=dp1  then isobar is forced to do something in the old regi        !/*ps        268*/
!                                                                               !/*ps        269*/
  170 if (ws(3).gt.dp10) go to 180                                              !/*ps        270*/
!                                                                               !/*ps        271*/
!     changes made to reflect a pion that can not cross in a no refracti        !/*ps        272*/
!     reflection option                                                         !/*ps        273*/
!                                                                               !/*ps        274*/
      ipisw=1                                                                   !/*ps        275*/
      ws(24)=gws(2)                                                             !/*ps        276*/
      go to 210                                                                 !/*ps        277*/
  180 ind(22)=1                                                                 !/*ps        278*/
      iws1=abs(ws(1))                                                           !/*ps        279*/
      if (mprin.gt.1) write (io,820) iws1                                       !/*ps        280*/
!       change made 2/2/71                                                      !/*ps        281*/
      ws(5)=gws(5)                                                              !/*ps        282*/
      beta=sqrt(gws(5)*gws(5)-ws(4)*ws(4))/gws(5)                               !/*ps        283*/
      ws(20)=0.1d0*sqrt((gws(10)-ws(10))**2+(gws(11)-ws(11))**2+(gws(12)        !/*ps        284*/
     & -ws(12))**2)                                                             !/*ps        285*/
      ws(16)=ws(16)-ws(20)/beta                                                 !/*ps        286*/
      gdc9=dp10*ws(20)                                                          !/*ps        287*/
      go to 810                                                                 !/*ps        288*/
!                                                                               !/*ps        289*/
  190 if (gdc6) 210,220,200                                                     !/*ps        290*/
!            ind(5)=1 when the change of potential is plus or zero.             !/*ps        291*/
  200 ind(5)=1                                                                  !/*ps        292*/
!                                                                               !/*ps        293*/
!     in case of reflection or refraction the particle is brought               !/*ps        294*/
!     by regb back to the region boundary. in the last case one must            !/*ps        295*/
!     be carefull to force the particle to cross into the new region.           !/*ps        296*/
!     subr refl reflects/refracts on region boundary                            !/*ps        297*/
!                                                                               !/*ps        298*/
!     -------------------------------------------                               !/*ps        299*/
  210 call regb (dist)                                                          !/*ps        300*/
      call refl (ipisw,gdc6,dp0,iref)                                           !/*ps        301*/
!     -------------------------------------------                               !/*ps        302*/
!                                                                               !/*ps        303*/
!     invokation of edit is mod received from yariv [mrc 19aug85 ]              !/*ps        304*/
!                                                                               !/*ps        305*/
!     --------------------------------------------                              !/*ps        306*/
      if (ind(34).eq.2) call isedt (ws)                                         !/*ps        307*/
!     --------------------------------------------                              !/*ps        308*/
      go to 810                                                                 !/*ps        309*/
!                                                                               !/*ps        310*/
!     statement 200 starts to calculate pion interaction to make isobar.        !/*ps        311*/
!     to make isobar, it is not sufficient to choose angle between the          !/*ps        312*/
!     two particles.  theta and phi have to be chosen.  see notes.              !/*ps        313*/
!                                                                               !/*ps        314*/
  220 continue                                                                  !/*ps        315*/
!     --------------------------------------------                              !/*ps        316*/
      if (ind(34).eq.2) call isedt (ws)                                         !/*ps        317*/
!     --------------------------------------------                              !/*ps        318*/
      if (kcan.ne.0) go to 810                                                  !/*ps        319*/
      if (iws3.lt.3.or.iws3.gt.9) go to 810                                     !/*ps        320*/
      if (ind(34).gt.0) go to 770                                               !/*ps        321*/
!----   interaction only if not leaving target or projectile                    !/*ps        322*/
      int=0                                                                     !/*ps        323*/
      inp=0                                                                     !/*ps        324*/
      ind9=0                                                                    !/*ps        325*/
      ind10=0                                                                   !/*ps        326*/
      must=0                                                                    !/*ps        327*/
      ind11=0                                                                   !/*ps        328*/
      indp9=0                                                                   !/*ps        329*/
      indp10=0                                                                  !/*ps        330*/
      indp11=0                                                                  !/*ps        331*/
      gws3=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*ps        332*/
      if (ind(30).eq.1) go to 330                                               !/*ps        333*/
!----   interaction in target is possible                                       !/*ps        334*/
      wsp(6)=rang()                                                             !/*ps        335*/
      wsp(6)=dp2*wsp(6)-dp1                                                     !/*ps        336*/
      wsp(8)=sqrt(dp1-wsp(6)*wsp(6))                                            !/*ps        337*/
      rand=rang()                                                               !/*ps        338*/
      wsp(7)=6.28318d0*rand                                                     !/*ps        339*/
      wsp(29)=cos(wsp(7))                                                       !/*ps        340*/
      wsp(28)=sin(wsp(7))                                                       !/*ps        341*/
      wsp(7)=wsp(8)*wsp(29)                                                     !/*ps        342*/
      wsp(8)=wsp(8)*wsp(28)                                                     !/*ps        343*/
      wsp(15)=gws(6)*wsp(6)+gws(7)*wsp(7)+gws(8)*wsp(8)                         !/*ps        344*/
      wsp3=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                                    !/*ps        345*/
!           knowing the angles of both particles, et is                         !/*ps        346*/
!           kinetic energy in the center of mass.                               !/*ps        347*/
      e10=(gws(5)*wsp(5)-wsp(15)*gws3*wsp3)/wsp(4)                              !/*ps        348*/
      et=(sqrt(ws(4)*ws(4)+wsp(19)+dp2*e10*wsp(4))-wsp(4)-ws(4))                !/*ps        349*/
      p10=sqrt(e10**2-ws(4)**2)                                                 !/*ps        350*/
      p10t=p10                                                                  !/*ps        351*/
!          present ptotal has cross sections up to 660 mev. if et               !/*ps        352*/
!          is .gt. 660 mev, choose another set of theta and phi                 !/*ps        353*/
      if ((et.lt.66.d1).and.(ws(3).lt.6.d0)) go to 230                          !/*ps        354*/
      if ((p10.lt.1.2d+03).and.(ws(3).gt.6.d0)) go to 230                       !/*ps        355*/
!           change made 11/10/70                                                !/*ps        356*/
      ind(30)=1                                                                 !/*ps        357*/
      go to 330                                                                 !/*ps        358*/
! ind(9)=1 for ii collisions.                                                   !/*ps        359*/
! ind(10)=1 for ij collisions.                                                  !/*ps        360*/
! ind(11)=1 for 0 collisions.                                                   !/*ps        361*/
  230 if (ws(3)-6.d0) 250,240,240                                               !/*ps        362*/
  240 if (ws(3)-8.d0) 290,300,260                                               !/*ps        363*/
  250 if (ws(3)-dp4) 290,300,260                                                !/*ps        364*/
  260 if (wsp(3).eq.dp2) go to 280                                              !/*ps        365*/
  270 ind10=1                                                                   !/*ps        366*/
      go to 310                                                                 !/*ps        367*/
  280 ind9=1                                                                    !/*ps        368*/
      go to 310                                                                 !/*ps        369*/
  290 if (wsp(3).eq.dp2) go to 270                                              !/*ps        370*/
      go to 280                                                                 !/*ps        371*/
  300 ind11=1                                                                   !/*ps        372*/
  310 ind(9)=ind9                                                               !/*ps        373*/
      ind(10)=ind10                                                             !/*ps        374*/
      ind(11)=ind11                                                             !/*ps        375*/
! ptotal calculates pion interaction cross sections.                            !/*ps        376*/
!     -----------------------------------------------------------------         !/*ps        377*/
      if (ws(3).lt.6.d0) call ptotal (et,ind(9),ind(10),ind(11),scat            !/*ps        378*/
     & ,absor)                                                                  !/*ps        379*/
!     -----------------------------------------------------------------         !/*ps        380*/
      if (ws(3).lt.6.d0) go to 320                                              !/*ps        381*/
      typ=ws(3)-8.d0                                                            !/*ps        382*/
!     -----------------------------------------------------------------         !/*ps        383*/
      call ptotkl (p10,typ,scp,scn,abp,abn)                                     !/*ps        384*/
!     -----------------------------------------------------------------         !/*ps        385*/
      scat=scp                                                                  !/*ps        386*/
      if (wsp(3).eq.2) scat=scn                                                 !/*ps        387*/
! probability of interaction is calculated just as in subr ainter.              !/*ps        388*/
  320 wsp18=wsp(18)                                                             !/*ps        389*/
      if (np.le.0) wsp18=wsp18*(sel(1)+sel(2))/csa(3)                           !/*ps        390*/
      wsp(20)=scat*wsp18/gws3/wsp(5)                                            !/*ps        391*/
      wsp(21)=ws(4)*ws(4)*wsp(19)                                               !/*ps        392*/
      selpt1=-gws3*wsp3*wsp(15)+gws(5)*wsp(5)                                   !/*ps        393*/
      wsp(21)=sqrt(selpt1*selpt1-wsp(21))*wsp(20)                               !/*ps        394*/
      selpt1=ws(20)*wsp(21)                                                     !/*ps        395*/
      wsp(20)=rang()                                                            !/*ps        396*/
      if (wsp(20).gt.selpt1) go to 330                                          !/*ps        397*/
      int=1                                                                     !/*ps        398*/
      fdist=wsp(20)/selpt1                                                      !/*ps        399*/
! now the same for the projectile                                               !/*ps        400*/
  330 if (ind(31).eq.1) go to 440                                               !/*ps        401*/
      wspr3=sqrt(wspr(5)*wspr(5)-wspr(4)*wspr(4))                               !/*ps        402*/
!-----transform the principal particle to projectile f.o.r.                     !/*ps        403*/
      e=gws(5)                                                                  !/*ps        404*/
      p(1)=gws3                                                                 !/*ps        405*/
      p(2)=gws(6)                                                               !/*ps        406*/
      p(3)=gws(7)                                                               !/*ps        407*/
      p(4)=gws(8)                                                               !/*ps        408*/
!     -----------------------------------------------------------------         !/*ps        409*/
      call plt (p,e,pt,ee,bet,gama)                                             !/*ps        410*/
!     -----------------------------------------------------------------         !/*ps        411*/
      wspr(15)=pt(2)*wspr(6)+pt(3)*wspr(7)+pt(4)*wspr(8)                        !/*ps        412*/
      e10=(ee*wspr(5)-wspr(15)*pt(1)*wspr3)/wspr(4)                             !/*ps        413*/
      ep=(sqrt(ws(4)*ws(4)+wspr(19)+dp2*e10*wspr(4))-wspr(4)-ws(4))             !/*ps        414*/
      p10=sqrt(e10**2-ws(4)**2)                                                 !/*ps        415*/
      p10p=p10                                                                  !/*ps        416*/
      if ((ep.le.66.d1).and.(ws(3).lt.6.d0)) go to 340                          !/*ps        417*/
      if ((p10.le.1.2d+03).and.(ws(3).gt.6.d0)) go to 340                       !/*ps        418*/
      ind(31)=1                                                                 !/*ps        419*/
      go to 440                                                                 !/*ps        420*/
  340 if (ws(3)-6.d0) 360,350,350                                               !/*ps        421*/
  350 if (ws(3)-8.d0) 400,410,370                                               !/*ps        422*/
  360 if (ws(3)-dp4) 400,410,370                                                !/*ps        423*/
  370 if (wspr(3).eq.dp2) go to 390                                             !/*ps        424*/
  380 indp10=1                                                                  !/*ps        425*/
      go to 420                                                                 !/*ps        426*/
  390 indp9=1                                                                   !/*ps        427*/
      go to 420                                                                 !/*ps        428*/
  400 if (wspr(3).eq.dp2) go to 380                                             !/*ps        429*/
      go to 390                                                                 !/*ps        430*/
  410 indp11=1                                                                  !/*ps        431*/
  420 ind(9)=indp9                                                              !/*ps        432*/
      ind(10)=indp10                                                            !/*ps        433*/
      ind(11)=indp11                                                            !/*ps        434*/
!     -----------------------------------------------------------------         !/*ps        435*/
      if (ws(3).lt.6.d0) call ptotal (ep,ind(9),ind(10),ind(11),scat            !/*ps        436*/
     & ,absor)                                                                  !/*ps        437*/
!     -----------------------------------------------------------------         !/*ps        438*/
      if (ws(3).lt.6.d0) go to 430                                              !/*ps        439*/
      typ=ws(3)-8.d0                                                            !/*ps        440*/
!     -----------------------------------------------------------------         !/*ps        441*/
      call ptotkl (p10,typ,scp,scn,abp,abn)                                     !/*ps        442*/
!     -----------------------------------------------------------------         !/*ps        443*/
      scat=scp                                                                  !/*ps        444*/
      if (wsp(3).eq.dp2) scat=scn                                               !/*ps        445*/
  430 wspr18=wspr(18)                                                           !/*ps        446*/
      if (np.le.0) wspr18=wspr18*(spr(1)+spr(2)+spr(3)+spr(4))/proj(3)          !/*ps        447*/
      wspr(20)=scat*wspr18/pt(1)/wspr(5)                                        !/*ps        448*/
      wspr(21)=ws(4)*ws(4)*wspr(19)                                             !/*ps        449*/
      selpt2=-gws3*wspr3*wspr(15)+gws(5)*wspr(5)                                !/*ps        450*/
      wspr(21)=sqrt(selpt2*selpt2-wspr(21))*wspr(20)                            !/*ps        451*/
      selpt2=wspr(31)*wspr(21)                                                  !/*ps        452*/
      wspr(20)=rang()                                                           !/*ps        453*/
      if (wspr(20).gt.selpt2) go to 440                                         !/*ps        454*/
      inp=1                                                                     !/*ps        455*/
      fdistp=wspr(20)/selpt2                                                    !/*ps        456*/
  440 if (int+inp-1) 760,450,550                                                !/*ps        457*/
  450 if (inp.eq.1) go to 500                                                   !/*ps        458*/
! change made 3/23/71 to put d restriction on pis                               !/*ps        459*/
  460 if (must.eq.1) go to 490                                                  !/*ps        460*/
      tdn=wsp(18)*csa(8)                                                        !/*ps        461*/
      if (wsp(3).eq.dp2) tdn=wsp(18)-tdn                                        !/*ps        462*/
      if (np) 490,470,480                                                       !/*ps        463*/
  470 ik=wsp(3)+1.d-02                                                          !/*ps        464*/
      tdn=wsp(18)*sel(ik)/csa(3)                                                !/*ps        465*/
      if (ifrist(wsp(3),icsb(13),tdn,1).gt.1) go to 760                         !/*ps        466*/
      go to 490                                                                 !/*ps        467*/
  480 if (ws(3).le.-dp1.and.(gws(5)-ws(4)-ws(24)).lt.dp0) go to 490             !/*ps        468*/
      if (nopart(wsp(3),tdn,gws,tnpar,nnt,dp1).gt.1) go to 760                  !/*ps        469*/
  490 ind(32)=1                                                                 !/*ps        470*/
      e=et                                                                      !/*ps        471*/
      p10=p10t                                                                  !/*ps        472*/
!          250. mev(k.e.c.m.)  is the cutoff for pi+n-->delta                   !/*ps        473*/
      if (e.lt.25.d1) go to 560                                                 !/*ps        474*/
      i9=ind9                                                                   !/*ps        475*/
      i10=ind10                                                                 !/*ps        476*/
      i11=ind11                                                                 !/*ps        477*/
      tcm=e                                                                     !/*ps        478*/
      go to 560                                                                 !/*ps        479*/
  500 if (must.eq.1) go to 530                                                  !/*ps        480*/
      tdn=wspr(18)*proj(8)                                                      !/*ps        481*/
      if (wspr(3).eq.dp2) tdn=wspr(18)-tdn                                      !/*ps        482*/
      if (np) 530,510,520                                                       !/*ps        483*/
  510 ik=wspr(3)+1.d-02                                                         !/*ps        484*/
      tdn=wspr(18)*(spr(ik)+spr(ik+2))/proj(3)                                  !/*ps        485*/
      if (ifrist(wspr(3),icsb(13),tdn,0).gt.1) go to 760                        !/*ps        486*/
      go to 530                                                                 !/*ps        487*/
  520 if (nopart(wspr(3),tdn,gws,pnpar,nnp,gama).gt.1) go to 760                !/*ps        488*/
  530 ind(32)=0                                                                 !/*ps        489*/
      e=ep                                                                      !/*ps        490*/
      fdist=fdistp                                                              !/*ps        491*/
!-----boost the new projectile isobar to lab f.o.r.                             !/*ps        492*/
      eee=wspr(5)                                                               !/*ps        493*/
      p(1)=wspr3                                                                !/*ps        494*/
      p(2)=wspr(6)                                                              !/*ps        495*/
      p(3)=wspr(7)                                                              !/*ps        496*/
      p(4)=wspr(8)                                                              !/*ps        497*/
!     -----------------------------------------------------------------         !/*ps        498*/
      call plt (p,eee,pt,ee,voc,gama)                                           !/*ps        499*/
!     -----------------------------------------------------------------         !/*ps        500*/
      wspr(5)=ee                                                                !/*ps        501*/
      wspr(6)=pt(2)                                                             !/*ps        502*/
      wspr(7)=pt(3)                                                             !/*ps        503*/
      wspr(8)=pt(4)                                                             !/*ps        504*/
      wspr(9)=pt(1)                                                             !/*ps        505*/
      wspr(15)=pt(2)*gws(6)+pt(3)*gws(7)+pt(4)*gws(8)                           !/*ps        506*/
!                                                                               !/*ps        507*/
      do 540 i=1,21                                                             !/*ps        508*/
  540 wsp(i)=wspr(i)                                                            !/*ps        509*/
      wsp(31)=-dp1                                                              !/*ps        510*/
      p10=p10p                                                                  !/*ps        511*/
      if (e.lt.25.d1) go to 560                                                 !/*ps        512*/
      i9=indp9                                                                  !/*ps        513*/
      i10=indp10                                                                !/*ps        514*/
      i11=indp11                                                                !/*ps        515*/
      tcm=e                                                                     !/*ps        516*/
      go to 560                                                                 !/*ps        517*/
  550 spt=selpt1/(selpt1+selpt2)                                                !/*ps        518*/
      rand=rang()                                                               !/*ps        519*/
      if (rand.le.spt) go to 460                                                !/*ps        520*/
      go to 500                                                                 !/*ps        521*/
  560 if (wsp(20).eq.dp0.and.wsp(21).eq.dp0) go to 570                          !/*ps        522*/
! ind(16)=1 when an isobar is made.                                             !/*ps        523*/
      ind(16)=1                                                                 !/*ps        524*/
      if (e.gt.2.5d+02.and.must.eq.0) ind(16)=0                                 !/*ps        525*/
!                                                                               !/*ps        526*/
!      fix to stop kaons from forming isobars (mrc 26 oct 82)                   !/*ps        527*/
!                                                                               !/*ps        528*/
      if ((ws(3).gt.6.d0).and.(ws(3).lt.dp10)) ind(16)=0                        !/*ps        529*/
!                                                                               !/*ps        530*/
      ws(20)=fdist*ws(20)                                                       !/*ps        531*/
      wsp(21)=ws(20)/(sqrt(gws(5)*gws(5)-ws(4)*ws(4))/gws(5))                   !/*ps        532*/
! ws(16) is the time remaining in time interval.                                !/*ps        533*/
  570 ws(16)=ws(16)-wsp(21)                                                     !/*ps        534*/
      wsp(13)=ws(16)                                                            !/*ps        535*/
! advance the pion to collision site.                                           !/*ps        536*/
      gdc9=ws(20)*dp10                                                          !/*ps        537*/
!dir$ ivdep                                                                     !/*ps        538*/
      do 580 i=1,3                                                              !/*ps        539*/
  580 ws(i+9)=gws(i+9)+gdc9*gws(i+5)                                            !/*ps        540*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*ps        541*/
      ws(14)=sqrt(ws(25))                                                       !/*ps        542*/
! give it back its original direction cosines.                                  !/*ps        543*/
      do 590 i=5,8                                                              !/*ps        544*/
      ws(i)=gws(i)                                                              !/*ps        545*/
  590 continue                                                                  !/*ps        546*/
      if (e.gt.2.5d+02.and.must.eq.0) go to 790                                 !/*ps        547*/
!      fix to stop kaons from forming isobars (mrc 26 oct 82)                   !/*ps        548*/
      if ((ws(3).gt.6.d0).and.(ws(3).lt.dp10)) go to 790                        !/*ps        549*/
!       subr save is called to save some pion characteristics in wws.           !/*ps        550*/
!     -----------------------------------------------------------------         !/*ps        551*/
  600 call save                                                                 !/*ps        552*/
!     -----------------------------------------------------------------         !/*ps        553*/
! identity of the isobar is calculated. i+ and i0 forget how they are           !/*ps        554*/
!  made.                                                                        !/*ps        555*/
      ws(3)=dp10*wsp(3)+ws(3)                                                   !/*ps        556*/
      ws(31)=wsp(31)                                                            !/*ps        557*/
      if (wsp(31).ne.dp0) go to 610                                             !/*ps        558*/
      efp=ef(1,istept)                                                          !/*ps        559*/
      efn=ef(2,istept)                                                          !/*ps        560*/
      go to 620                                                                 !/*ps        561*/
  610 efp=pef(1,istepp)                                                         !/*ps        562*/
      efn=pef(2,istepp)                                                         !/*ps        563*/
  620 if (ws(3)-15.d0) 640,630,640                                              !/*ps        564*/
  630 ws(3)=24.d0                                                               !/*ps        565*/
!                                                                               !/*ps        566*/
!     delta is the fudge factor when there is a difference in potential         !/*ps        567*/
!     after an interaction.  see notes.                                         !/*ps        568*/
!                                                                               !/*ps        569*/
      delta=efn-efp-csa(11)                                                     !/*ps        570*/
      go to 670                                                                 !/*ps        571*/
  640 if (ws(3)-23.d0) 660,650,660                                              !/*ps        572*/
  650 ws(3)=14.d0                                                               !/*ps        573*/
      delta=efp-efn+csa(11)                                                     !/*ps        574*/
      go to 670                                                                 !/*ps        575*/
  660 delta=dp0                                                                 !/*ps        576*/
  670 if (ws(3)-14.d0) 680,680,690                                              !/*ps        577*/
  680 ws(24)=efp                                                                !/*ps        578*/
      go to 700                                                                 !/*ps        579*/
  690 ws(24)=efn                                                                !/*ps        580*/
!                                                                               !/*ps        581*/
!     energy is adjusted to take care of change in potential.                   !/*ps        582*/
!     following changes made 10/12/70 to conserve energy and at least           !/*ps        583*/
!     the total momentum direction                                              !/*ps        584*/
!                                                                               !/*ps        585*/
  700 delt=delta                                                                !/*ps        586*/
      wsp4=wsp(4)                                                               !/*ps        587*/
!     -----------------------------------------------------------------         !/*ps        588*/
      if (wsp(31).ne.dp0) call cep (delta,delt,wsp4,wsp)                        !/*ps        589*/
!     -----------------------------------------------------------------         !/*ps        590*/
      ws3=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                         !/*ps        591*/
      wsp3=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                                    !/*ps        592*/
! combine two particles into the isobar. see notes.                             !/*ps        593*/
      pi6=dp0                                                                   !/*ps        594*/
      do 710 i=6,8                                                              !/*ps        595*/
      ws(i)=ws(i)*ws3+wsp(i)*wsp3                                               !/*ps        596*/
      pi6=pi6+ws(i)*ws(i)                                                       !/*ps        597*/
  710 continue                                                                  !/*ps        598*/
      ws3=sqrt(pi6)                                                             !/*ps        599*/
      do 720 i=6,8                                                              !/*ps        600*/
      ws(i)=ws(i)/ws3                                                           !/*ps        601*/
  720 continue                                                                  !/*ps        602*/
      ws(5)=ws(5)+wsp(5)                                                        !/*ps        603*/
      ws(4)=sqrt(ws(5)*ws(5)-ws3*ws3)                                           !/*ps        604*/
!           additional changes made 10/16/70.see notes                          !/*ps        605*/
      if ((delt+wsp(5)).eq.dp0) go to 730                                       !/*ps        606*/
      cprime=(ws(5)+delt)/ws(5)                                                 !/*ps        607*/
      ws(4)=cprime*ws(4)                                                        !/*ps        608*/
      ws(5)=ws(5)+delt                                                          !/*ps        609*/
!                                                                               !/*ps        610*/
!     change made 3/23/71 to only follow isobars whose mass is greater          !/*ps        611*/
!     than 1080 mev.force others to capture                                     !/*ps        612*/
  730 if (ws(4).lt.108.d1) ind(22)=1                                            !/*ps        613*/
      ind(3)=0                                                                  !/*ps        614*/
!          ws(15) has time of birth of isobar.                                  !/*ps        615*/
      ws(15)=ws(19)-ws(16)                                                      !/*ps        616*/
!          ws(9)=dp0 because isobar was not selected from the femi sea.         !/*ps        617*/
      ws(9)=dp0                                                                 !/*ps        618*/
!          the nucleon that combined with pion is stored in st.                 !/*ps        619*/
      if (ind(32).eq.2) go to 740                                               !/*ps        620*/
      icsb(15)=icsb(15)+1                                                       !/*ps        621*/
      if (icsb(15).ge.i391) go to 800                                           !/*ps        622*/
      icsb8=icsb(15)*i81                                                        !/*ps        623*/
      st(icsb8+1)=-(ws(1)+0.1d0)                                                !/*ps        624*/
      st(icsb8+2)=5.1d0                                                         !/*ps        625*/
      wsp2=5.1d0                                                                !/*ps        626*/
!     wsp2=5.+ws(2)-aint(ws(2))                                                 !/*ps        627*/
!     st(icsb8+2)=wsp2                                                          !/*ps        628*/
      st(icsb8+3)=wsp(3)                                                        !/*ps        629*/
      st(icsb8+4)=wsp(4)                                                        !/*ps        630*/
      st(icsb8+5)=wsp(5)                                                        !/*ps        631*/
      st(icsb8+6)=dp0                                                           !/*ps        632*/
      st(icsb8+7)=dp0                                                           !/*ps        633*/
      st(icsb8+8)=dp0                                                           !/*ps        634*/
      st(icsb8+9)=wsp(9)                                                        !/*ps        635*/
      st(icsb8+10)=ws(10)                                                       !/*ps        636*/
      st(icsb8+11)=ws(11)                                                       !/*ps        637*/
      st(icsb8+12)=ws(12)                                                       !/*ps        638*/
      st(icsb8+13)=wsp(13)                                                      !/*ps        639*/
      st(icsb8+14)=ws(14)                                                       !/*ps        640*/
      st(icsb8+15)=ws(25)                                                       !/*ps        641*/
      st(icsb8+16)=wsp(24)                                                      !/*ps        642*/
      st(icsb8+17)=ws(15)                                                       !/*ps        643*/
      st(icsb8+18)=ws(10)                                                       !/*ps        644*/
      st(icsb8+19)=ws(11)                                                       !/*ps        645*/
      st(icsb8+20)=ws(12)                                                       !/*ps        646*/
      st(icsb8+21)=dp0                                                          !/*ps        647*/
      st(icsb8+i81)=ws(31)                                                      !/*ps        648*/
      go to 750                                                                 !/*ps        649*/
! ---- update storage of partner after "cas-cas" interaction                    !/*ps        650*/
  740 i7=icsb(17)*i81                                                           !/*ps        651*/
      ig=st(i7+2)/dp5+1.d-03                                                    !/*ps        652*/
      igf=ig*5+16+i7                                                            !/*ps        653*/
      st(i7+1)=-ws(1)                                                           !/*ps        654*/
      wsp2=st(i7+2)+5.1d0                                                       !/*ps        655*/
      st(i7+2)=wsp2                                                             !/*ps        656*/
      st(i7+6)=dp0                                                              !/*ps        657*/
      st(i7+7)=dp0                                                              !/*ps        658*/
      st(i7+8)=dp0                                                              !/*ps        659*/
      st(igf+1)=ws(15)                                                          !/*ps        660*/
      st(i7+10)=ws(10)                                                          !/*ps        661*/
      st(igf+2)=ws(10)                                                          !/*ps        662*/
      st(i7+11)=ws(11)                                                          !/*ps        663*/
      st(igf+3)=ws(11)                                                          !/*ps        664*/
      st(i7+12)=ws(12)                                                          !/*ps        665*/
      st(igf+4)=ws(12)                                                          !/*ps        666*/
      st(igf+5)=dp0                                                             !/*ps        667*/
      icsb(1)=icsb(1)-1                                                         !/*ps        668*/
      if (icsb(17).gt.icsb(7)) icsb(2)=icsb(2)-1                                !/*ps        669*/
!                                                                               !/*ps        670*/
!     the pion that combined with nucleon is placed in the next                 !/*ps        671*/
!     free space in st so that isobar can take old place in st                  !/*ps        672*/
!     that pion occupied.  change made 2/2/71                                   !/*ps        673*/
!     ws(2)=ws(2)+.1                                                            !/*ps        674*/
!                                                                               !/*ps        675*/
  750 ipl=0                                                                     !/*ps        676*/
!     -------------------------------                                           !/*ps        677*/
      call wwsst (ipl)                                                          !/*ps        678*/
!     -------------------------------                                           !/*ps        679*/
!          ind(20)=1 because afte an isobar is made, it selects a part-         !/*ps        680*/
!          ner and finishes off the time interval the pion had started.         !/*ps        681*/
      ind(20)=1                                                                 !/*ps        682*/
      icsb13=icsb(13)                                                           !/*ps        683*/
      ws(2)=wsp2                                                                !/*ps        684*/
!          wssto is called to store isobar characteisics into st.               !/*ps        685*/
      call wssto (icsb13)                                                       !/*ps        686*/
      st(icsb13+17)=ws(15)                                                      !/*ps        687*/
      st(icsb13+18)=ws(10)                                                      !/*ps        688*/
      st(icsb13+19)=ws(11)                                                      !/*ps        689*/
      st(icsb13+20)=ws(12)                                                      !/*ps        690*/
      st(icsb13+21)=ws(5)                                                       !/*ps        691*/
!     -------------------------------                                           !/*ps        692*/
      call updeca                                                               !/*ps        693*/
!     -------------------------------                                           !/*ps        694*/
      ws(29)=wws(3)                                                             !/*ps        695*/
      st(icsb13+i81-4)=ws(26)                                                   !/*ps        696*/
      st(icsb13+i81-3)=ws(27)                                                   !/*ps        697*/
      st(icsb13+i81-2)=ws(28)                                                   !/*ps        698*/
      st(icsb13+i81-1)=ws(29)                                                   !/*ps        699*/
      go to 810                                                                 !/*ps        700*/
!                                                                               !/*ps        701*/
!     change made 6/22/71 to force a pi which can never escape to               !/*ps        702*/
!     form an isobar which is below cutoff                                      !/*ps        703*/
  760 continue                                                                  !/*ps        704*/
!        fix to keep kaons from forming isobars (mrc 26 oct 82)                 !/*ps        705*/
      if ((ws(3).gt.6.d0).and.(ws(3).lt.dp10)) go to 770                        !/*ps        706*/
      picut=ws(4)+vpion                                                         !/*ps        707*/
      csa11=cutofa(1)+cutofa(10)-csa(5)-csa(15)                                 !/*ps        708*/
      if (ws(3).ne.dp4) picut=csa11+picut                                       !/*ps        709*/
      if (gws(5).le.picut) go to 780                                            !/*ps        710*/
!          ind(4)=1 when there is no interaction, the next particle             !/*ps        711*/
!          from st is loaded into ws.                                           !/*ps        712*/
  770 ind(4)=1                                                                  !/*ps        713*/
      go to 810                                                                 !/*ps        714*/
!                                                                               !/*ps        715*/
  780 wsp(20)=dp0                                                               !/*ps        716*/
      wspr(20)=dp0                                                              !/*ps        717*/
      wsp(21)=dp0                                                               !/*ps        718*/
      wspr(21)=dp0                                                              !/*ps        719*/
      ws(20)=dp0                                                                !/*ps        720*/
      int=1-ind(30)                                                             !/*ps        721*/
      inp=1-ind(31)                                                             !/*ps        722*/
      if ((inp+int).eq.0) go to 770                                             !/*ps        723*/
      ind(16)=1                                                                 !/*ps        724*/
      ind(22)=1                                                                 !/*ps        725*/
      must=1                                                                    !/*ps        726*/
      go to 440                                                                 !/*ps        727*/
!           no isobar formation                                                 !/*ps        728*/
  790 ind(4)=0                                                                  !/*ps        729*/
      go to 810                                                                 !/*ps        730*/
!                                                                               !/*ps        731*/
  800 kcan=2                                                                    !/*ps        732*/
      lkcan='psel'                                                              !/*ps        733*/
      go to 810                                                                 !/*ps        734*/
!                                                                               !/*ps        735*/
  810 return                                                                    !/*ps        736*/
!                                                                               !/*ps        737*/
  820 format (' isobar mass became more than its total energy during',          !/*ps        738*/
     & ' crossing        ',z18,i10)                                             !/*ps        739*/
      end                                                                       !/*ps        740*/

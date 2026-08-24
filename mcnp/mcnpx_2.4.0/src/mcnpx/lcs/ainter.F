!     $Id: ainter.F,v 1.5 2002/03/27 18:14:52 gwm Exp $                       !/*ai          1*/
      subroutine ainter (gdc9,gdc10)                                            !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     ainter decides whether there will be an                                   !/*ai          5*/
!     an interaction. if there will be a collision, ainter                      !/*ai          6*/
!     decides where the collision will take place.                              !/*ai          7*/
!                                                                               !/*ai          8*/
! AINTER calls   SQRT  IFRIST  ALOG  EXP  CII  CIJ  FLTRNF  NOPART              !/*ai          9*/
!                PLT  FINLII  FINLIJ  AITOTA                                    !/*ai         10*/
! called by ISABEL  GAMMA                                                       !/*ai         11*/
!     ==================================================================        !/*ai         12*/
!                                                                               !/*ai         13*/
#include <lcs/vegas.h>                                                          /*!ai         14*/
      dimension p(4), pt(4)                                                     !/*ai         15*/
!                                                                               !/*ai         16*/
!       ap-scattering and total cross-sections (from old sub "aptota")          !/*ai         17*/
!                 sig(e) total                                                  !/*ai         18*/
      aptat(e)=exp(4.5485063d0*exp(-.06010215d0*log(max(1.d-3,1.d-3*e)))        !/*ai         19*/
     & )                                                                        !/*ai         20*/
!                    sig(e) elastic                                             !/*ai         21*/
      aptas(e)=exp(4.6051702d0-1.03651539d0*exp(.3808475d0*log(max(1.d-3        !/*ai         22*/
     & ,1.d-3*e))))                                                             !/*ai         23*/
!              sig(e) charge-exchange                                           !/*ai         24*/
      aptac(e)=4.4d0/sqrt(max(1.d-3,1.d-3*e))                                   !/*ai         25*/
!                                                                               !/*ai         26*/
      bet=-voc                                                                  !/*ai         27*/
      int=0                                                                     !/*ai         28*/
      inp=0                                                                     !/*ai         29*/
      etep(1)=dp0                                                               !/*ai         30*/
      etep(2)=dp0                                                               !/*ai         31*/
      if (ind(22).ne.0) go to 10                                                !/*ai         32*/
!                       isobar now above cutoff                                 !/*ai         33*/
      if (ind(8).gt.0.or.ind(34).gt.0) go to 150                                !/*ai         34*/
!                   interaction in same region, and particle in system          !/*ai         35*/
!  the interaction is inhibited if the particle just left                       !/*ai         36*/
!  the target                                                                   !/*ai         37*/
   10 if (ws(3).gt.dp10) go to 210                                              !/*ai         38*/
!                      isobars are handled later (stmt# 210)                    !/*ai         39*/
!  gws(5) is the total energy of the particle before crossing                   !/*ai         40*/
!  into a new region. gws(5) should be used because a particle                  !/*ai         41*/
!  is advanced to the boundary of the new region, and collision                 !/*ai         42*/
!  takes place in the old region.                                               !/*ai         43*/
      gdc4=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*ai         44*/
      if (ind(30).eq.1) go to 60                                                !/*ai         45*/
!                       interaction in target is possible (after selpt)         !/*ai         46*/
!       wsp18 = total density, csa8 = z/a, tdn= p(n) density                    !/*ai         47*/
      tdn=wsp(18)*csa(8)                                                        !/*ai         48*/
      if (wsp(3).eq.dp2) tdn=wsp(18)-tdn                                        !/*ai         49*/
      wsp18=wsp(18)                                                             !/*ai         50*/
!         if (np) nochecks, distance, holes                                     !/*ai         51*/
      if (np.gt.0) go to 20                                                     !/*ai         52*/
!           if no check on holes, then renormalize                              !/*ai         53*/
!           density by # potential partners (p+n) in target                     !/*ai         54*/
      wsp18=wsp18*(sel(1)+sel(2))/csa(3)                                        !/*ai         55*/
      if (np.lt.0) go to 20                                                     !/*ai         56*/
!          if distance restriction, "ifrist" checks it out                      !/*ai         57*/
      ik=wsp(3)+1.d-02                                                          !/*ai         58*/
      tdn=wsp(18)*sel(ik)/csa(3)                                                !/*ai         59*/
      if (ifrist(wsp(3),icsb(13),tdn,1).gt.1) go to 60                          !/*ai         60*/
!            selpt1 = e1*e1p-p1*p1p*cos(theta) =?= invariant mass**2            !/*ai         61*/
   20 selpt1=gws(5)*wsp(5)-sqrt(wsp(5)**2-wsp(4)**2)*wsp(15)*gdc4               !/*ai         62*/
!             et = kinetic energy of (pbar) on startionary target               !/*ai         63*/
      et=((selpt1/wsp(4))-ws(4))                                                !/*ai         64*/
      if (ws(3).ge.dp0) go to 30                                                !/*ai         65*/
!  get the cross section for ap(stmt.fcn replaces call to aptota)               !/*ai         66*/
      apscat=aptas(et)                                                          !/*ai         67*/
      aptot=aptat(et)                                                           !/*ai         68*/
      apchx=aptac(et)                                                           !/*ai         69*/
      c=aptot                                                                   !/*ai         70*/
      go to 40                                                                  !/*ai         71*/
!       if e < 5 mev, and no proj. nucleus, ultimately goesto stmt 150          !/*ai         72*/
   30 if (et.lt.dp5) go to 60                                                   !/*ai         73*/
      if (ws(3).eq.wsp(3)) c=cii(et)                                            !/*ai         74*/
      if (ws(3).ne.wsp(3)) c=cij(et)                                            !/*ai         75*/
!               "c" is the total (p+n) cross-section                            !/*ai         76*/
   40 wsp(20)=c*wsp18/gdc4/wsp(5)                                               !/*ai         77*/
      wsp(21)=ws(4)*ws(4)*wsp(19)                                               !/*ai         78*/
      wsp(21)=wsp(20)*sqrt(selpt1*selpt1-wsp(21))                               !/*ai         79*/
!  selpt1 contains the probability of interaction.                              !/*ai         80*/
!  if the rand. no. is less than selpt1 there will be a collision.              !/*ai         81*/
!  program than branches to statement 50 .                                      !/*ai         82*/
      selpt1=ws(20)*wsp(21)                                                     !/*ai         83*/
      randt=rang()                                                              !/*ai         84*/
      if (randt.ge.selpt1) go to 60                                             !/*ai         85*/
      fdistt=randt/selpt1                                                       !/*ai         86*/
      if (np.le.0) go to 50                                                     !/*ai         87*/
      if (ws(3).le.-dp1.and.(gws(5)-ws(4)-ws(24)).lt.dp0) go to 50              !/*ai         88*/
      if (nopart(wsp(3),tdn,gws,tnpar,nnt,dp1).gt.1) go to 60                   !/*ai         89*/
   50 int=1                                                                     !/*ai         90*/
!  now the same for the projectile                                              !/*ai         91*/
   60 if (ind(31).eq.1) go to 90                                                !/*ai         92*/
!        rxn in projectile is possible                                          !/*ai         93*/
      tdn=wspr(18)*proj(8)                                                      !/*ai         94*/
      if (wspr(3).eq.dp2) tdn=wspr(18)-tdn                                      !/*ai         95*/
      wspr18=wspr(18)                                                           !/*ai         96*/
      if (np.gt.0) go to 70                                                     !/*ai         97*/
      wspr18=wspr18*(spr(1)+spr(2)+spr(3)+spr(4))/proj(3)                       !/*ai         98*/
      if (np.lt.0) go to 70                                                     !/*ai         99*/
      ik=wspr(3)+1.d-02                                                         !/*ai        100*/
      tdn=wspr(18)*(spr(ik)+spr(ik+2))/proj(3)                                  !/*ai        101*/
      if (ifrist(wspr(3),icsb(13),tdn,0).gt.1) go to 90                         !/*ai        102*/
!-----transform the principal particle to projectile f.o.r                      !/*ai        103*/
   70 e=gws(5)                                                                  !/*ai        104*/
      p(1)=gdc4                                                                 !/*ai        105*/
      p(2)=gws(6)                                                               !/*ai        106*/
      p(3)=gws(7)                                                               !/*ai        107*/
      p(4)=gws(8)                                                               !/*ai        108*/
      call plt (p,e,pt,ee,bet,gama)                                             !/*ai        109*/
      gdc4p=sqrt(wspr(5)**2-wspr(4)**2)                                         !/*ai        110*/
      wspr(15)=pt(2)*wspr(6)+pt(3)*wspr(7)+pt(4)*wspr(8)                        !/*ai        111*/
      selpt2=ee*wspr(5)-gdc4p*pt(1)*wspr(15)                                    !/*ai        112*/
      ep=((selpt2/wspr(4))-ws(4))                                               !/*ai        113*/
      if (ep.lt.dp5) go to 90                                                   !/*ai        114*/
      if (ws(3).eq.wspr(3)) c=cii(ep)                                           !/*ai        115*/
      if (ws(3).ne.wspr(3)) c=cij(ep)                                           !/*ai        116*/
      wspr(20)=c*wspr18/pt(1)/wspr(5)                                           !/*ai        117*/
      wspr(21)=ws(4)*ws(4)*wspr(19)                                             !/*ai        118*/
      wspr(21)=wspr(20)*sqrt(selpt2*selpt2-wspr(21))                            !/*ai        119*/
      selpt2=wspr(31)*wspr(21)                                                  !/*ai        120*/
      randp=rang()                                                              !/*ai        121*/
      if (randp.ge.selpt2) go to 90                                             !/*ai        122*/
      fdistp=randp/selpt2                                                       !/*ai        123*/
      if (np.le.0) go to 80                                                     !/*ai        124*/
      if (nopart(wspr(3),tdn,gws,pnpar,nnp,gama).gt.1) go to 90                 !/*ai        125*/
   80 inp=1                                                                     !/*ai        126*/
   90 if (int+inp-1) 150,100,140                                                !/*ai        127*/
  100 if (inp.eq.1) go to 120                                                   !/*ai        128*/
  110 ind(32)=1                                                                 !/*ai        129*/
      e=et                                                                      !/*ai        130*/
      fdist=fdistt                                                              !/*ai        131*/
      etep(1)=e                                                                 !/*ai        132*/
      etep(2)=et                                                                !/*ai        133*/
      go to 160                                                                 !/*ai        134*/
!-----boost the projectile particle to lab. f.o.r                               !/*ai        135*/
  120 e=wspr(5)                                                                 !/*ai        136*/
      p(1)=gdc4p                                                                !/*ai        137*/
      p(2)=wspr(6)                                                              !/*ai        138*/
      p(3)=wspr(7)                                                              !/*ai        139*/
      p(4)=wspr(8)                                                              !/*ai        140*/
      call plt (p,e,pt,ee,voc,gama)                                             !/*ai        141*/
      wspr(5)=ee                                                                !/*ai        142*/
      wspr(6)=pt(2)                                                             !/*ai        143*/
      wspr(7)=pt(3)                                                             !/*ai        144*/
      wspr(8)=pt(4)                                                             !/*ai        145*/
      wspr(9)=pt(1)                                                             !/*ai        146*/
      wspr(15)=pt(2)*gws(6)+pt(3)*gws(7)+pt(4)*gws(8)                           !/*ai        147*/
      do 130 i=1,21                                                             !/*ai        148*/
  130 wsp(i)=wspr(i)                                                            !/*ai        149*/
      wsp(31)=-dp1                                                              !/*ai        150*/
      ind(32)=0                                                                 !/*ai        151*/
      e=ep                                                                      !/*ai        152*/
      fdist=fdistp                                                              !/*ai        153*/
      etep(2)=e                                                                 !/*ai        154*/
      etep(1)=ep                                                                !/*ai        155*/
      go to 160                                                                 !/*ai        156*/
  140 spt=selpt1/(selpt1+selpt2)                                                !/*ai        157*/
      randpt=rang()                                                             !/*ai        158*/
      if (randpt.le.spt) go to 110                                              !/*ai        159*/
      go to 120                                                                 !/*ai        160*/
!  ind(4)=1 when there will be no collision.                                    !/*ai        161*/
!  the next particle in st is loaded into ws.                                   !/*ai        162*/
  150 ind(4)=1                                                                  !/*ai        163*/
      return                                                                    !/*ai        164*/
!  check whether collision is allowed by dist. restriction                      !/*ai        165*/
!  and whether there is a particle at the collision site                        !/*ai        166*/
!  ws(20) contains the distance in unit of 10(-12)cm.                           !/*ai        167*/
!-----------------------------------------------------------------------        !/*ai        168*/
!  This modification made after consulation with Y. Yariv on                    !/*ai        169*/
!  12/30/86.  It corrects the transport of a virgin projectile particle         !/*ai        170*/
!  to the collision site while maintaining the correctness for others.          !/*ai        171*/
  160 continue                                                                  !/*ai        172*/
!  bring the particle to the collision site.                                    !/*ai        173*/
      do 170 i=10,12                                                            !/*ai        174*/
  170 ws(i)=gws(i)+fdist*(ws(i)-gws(i))                                         !/*ai        175*/
!  gdc9 is the distance to the collision site.                                  !/*ai        176*/
      gdc9=sqrt((ws(10)-gws(10))**2+(ws(11)-gws(11))**2+(ws(12)-gws(12))        !/*ai        177*/
     & **2)                                                                     !/*ai        178*/
      ws(20)=0.1d0*gdc9                                                         !/*ai        179*/
!  The following statement actually contains an error for virgin                !/*ai        180*/
!  projectile particles, since they were moved in a "frozen"                    !/*ai        181*/
!  state with the BETA of the projectile.                                       !/*ai        182*/
      ws(16)=ws(16)-ws(20)/(sqrt(gws(5)*gws(5)-ws(4)*ws(4))/gws(5))             !/*ai        183*/
      wsp(13)=ws(16)                                                            !/*ai        184*/
!-----------------------------------------------------------------------        !/*ai        185*/
!-----------------------------------------------------------------------        !/*ai        186*/
!  bring the particle to the collision site.                                    !/*ai        187*/
!-----virgin particles move in beam direction, others in the direction          !/*ai        188*/
!-----of their momentum (y. yariv 12/31/86)                                     !/*ai        189*/
!      do 170 i=6,8                                                             !/*ai        190*/
!  170 ws(i+4)=fdist*(gws(i)-ws(i))+gws(i+4)                                    !/*ai        191*/
!-----------------------------------------------------------------------        !/*ai        192*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*ai        193*/
      ws(14)=sqrt(ws(25))                                                       !/*ai        194*/
      do 180 i=5,8                                                              !/*ai        195*/
  180 ws(i)=gws(i)                                                              !/*ai        196*/
      ws(24)=gws(2)                                                             !/*ai        197*/
      ind(4)=0                                                                  !/*ai        198*/
!  ap annihilation                                                              !/*ai        199*/
      if (ws(3).ge.dp0) go to 200                                               !/*ai        200*/
!                       now an anti-particle                                    !/*ai        201*/
!                       but charge-exchange only on identical partners          !/*ai        202*/
      if (abs(ws(3)).ne.abs(wsp(3))) apchx=dp0                                  !/*ai        203*/
      rand=rang()                                                               !/*ai        204*/
      fsc=(apscat+apchx)/aptot                                                  !/*ai        205*/
      if (rand.ge.fsc) go to 190                                                !/*ai        206*/
!            now either elastic or charge-exchange                              !/*ai        207*/
      if (apchx.eq.dp0) return                                                  !/*ai        208*/
!             either nbar+n or pbar+p                                           !/*ai        209*/
      rand=rang()                                                               !/*ai        210*/
      fcx=apchx/(apchx+apscat)                                                  !/*ai        211*/
      if (rand.ge.fcx) return                                                   !/*ai        212*/
!                    charge-exchange                                            !/*ai        213*/
      ind(29)=2                                                                 !/*ai        214*/
      return                                                                    !/*ai        215*/
!                     annihilation                                              !/*ai        216*/
  190 ind(28)=2                                                                 !/*ai        217*/
      return                                                                    !/*ai        218*/
!  pi production                                                                !/*ai        219*/
  200 if (ws(3).gt.dp10) return                                                 !/*ai        220*/
      rand=rang()                                                               !/*ai        221*/
      if (ws(3).eq.wsp(3)) finel=finlii(e)                                      !/*ai        222*/
      if (ws(3).ne.wsp(3)) finel=finlij(e)                                      !/*ai        223*/
      if (rand.lt.finel) ind(28)=1                                              !/*ai        224*/
      return                                                                    !/*ai        225*/
!                                                                               !/*ai        226*/
!  start treating isobar interactions                                           !/*ai        227*/
!  ind(12)=1 when ws contains an i++ or i-.                                     !/*ai        228*/
!                                                                               !/*ai        229*/
  210 if (ws(3).eq.13.d0.or.ws(3).eq.25.d0) ind(12)=1                           !/*ai        230*/
      gws3=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*ai        231*/
!  v is the mass of the isobar in unit of bev.                                  !/*ai        232*/
      v=1.d-03*ws(4)                                                            !/*ai        233*/
      if (v.lt.1.11d0) v=1.11d0                                                 !/*ai        234*/
!  first the interactins with the projectile                                    !/*ai        235*/
      if (ind(31).eq.1) go to 280                                               !/*ai        236*/
!-----transform the principal particle to projectile f.o.r                      !/*ai        237*/
      e=gws(5)                                                                  !/*ai        238*/
      p(1)=gws3                                                                 !/*ai        239*/
      p(2)=gws(6)                                                               !/*ai        240*/
      p(3)=gws(7)                                                               !/*ai        241*/
      p(4)=gws(8)                                                               !/*ai        242*/
      call plt (p,e,pt,ee,bet,gama)                                             !/*ai        243*/
      wspr(15)=pt(2)*wspr(6)+pt(3)*wspr(7)+pt(4)*wspr(8)                        !/*ai        244*/
!-----                                                                          !/*ai        245*/
      wspr3=sqrt(wspr(5)*wspr(5)-wspr(4)*wspr(4))                               !/*ai        246*/
      selpt2=ee*wspr(5)-pt(1)*wspr3*wspr(15)                                    !/*ai        247*/
      wspr(16)=selpt2/wspr(4)                                                   !/*ai        248*/
!  e is kinetic energy of the isobar hitting a stationary nucleon               !/*ai        249*/
!  zeev fraenkels table only goes up to tlab=1000mev.                           !/*ai        250*/
!  if e is greater than 1000 mev, a line is printed, and anew                   !/*ai        251*/
!  angle of collision(wspr(15)) between isobar and nucleon is chosen.           !/*ai        252*/
      e=(wspr(16)-ws(4))                                                        !/*ai        253*/
!  aiso is the beta of the isobar in projectile f.o.r.                          !/*ai        254*/
      aiso=pt(1)/ee                                                             !/*ai        255*/
      wspr18=wspr(18)                                                           !/*ai        256*/
      if (np.le.0) wspr18=wspr18*(spr(1)+spr(2)+spr(3)+spr(4))/proj(3)          !/*ai        257*/
      u1=ws(4)*ws(4)+wspr(19)                                                   !/*ai        258*/
!  u is the total energy in the center of mass in unit of bev.                  !/*ai        259*/
      u=(sqrt(selpt2+selpt2+u1))*1.d-03                                         !/*ai        260*/
!  tables for isobar scattering have a minimum u and v                          !/*ai        261*/
!  of 2.1 and 1.11. if calculated u and v are less than these                   !/*ai        262*/
!  then u and v are set equal to 2.1 and 1.11.                                  !/*ai        263*/
      if (u.lt.2.1d0) u=2.1d0                                                   !/*ai        264*/
      call aitota (e,aiso,wspr18,u,v,wspr)                                      !/*ai        265*/
!  changes made to treat isobar decays independently from                       !/*ai        266*/
!  isobar-nucleon interactions                                                  !/*ai        267*/
      wspr(20)=wspr18/(gws3*wspr(5))                                            !/*ai        268*/
      pdecay=wspr18*aidec                                                       !/*ai        269*/
      wspr(21)=ws(4)*ws(4)*wspr(19)                                             !/*ai        270*/
      wspr(21)=wspr(20)*sqrt(selpt2*selpt2-wspr(21))                            !/*ai        271*/
      pcapt=aicap*wspr(21)                                                      !/*ai        272*/
      pscat=aiscat*wspr(21)                                                     !/*ai        273*/
      if (ind(22).eq.0.and.ind(24).eq.0) go to 220                              !/*ai        274*/
      if (ws(31).ne.dp0.or.ind(30).eq.1) go to 410                              !/*ai        275*/
      go to 280                                                                 !/*ai        276*/
!  check to see whether collision is allowed by distance restriction            !/*ai        277*/
!  isobars can always decay                                                     !/*ai        278*/
  220 tdn=wspr(18)*proj(8)                                                      !/*ai        279*/
      if (wspr(3).eq.dp2) tdn=wspr(18)-tdn                                      !/*ai        280*/
      fract=dp1                                                                 !/*ai        281*/
      if (np) 270,230,240                                                       !/*ai        282*/
  230 ik=wspr(3)+1.d-02                                                         !/*ai        283*/
      tdn=wspr(18)*(spr(ik)+spr(ik+2))/proj(3)                                  !/*ai        284*/
      if (ifrist(wspr(3),icsb(13),tdn,0).le.1) go to 260                        !/*ai        285*/
      go to 250                                                                 !/*ai        286*/
  240 if (nopart(wspr(3),tdn,gws,pnpar,nnp,gama).le.1) go to 260                !/*ai        287*/
!                                                                               !/*ai        288*/
!           found a hole, so no chance to scatter or capture                    !/*ai        289*/
!                                                                               !/*ai        290*/
  250 pcapt=dp0                                                                 !/*ai        291*/
      pscat=dp0                                                                 !/*ai        292*/
      go to 270                                                                 !/*ai        293*/
!                                                                               !/*ai        294*/
!          no hole, but suppress the t=1 fraction, absorb,scatter               !/*ai        295*/
!          on the t=0 pairs of nucleons only (when knp .gt.0)                   !/*ai        296*/
!          (knp.lt.0 ==> isospin independence, leave as was)                    !/*ai        297*/
!          change made  1 feb 84 [mrc]                                          !/*ai        298*/
!                                                                               !/*ai        299*/
  260 continue                                                                  !/*ai        300*/
      if (knp.gt.0) fract=0.75d0                                                !/*ai        301*/
  270 pcapt=pcapt*fract                                                         !/*ai        302*/
!                                                                               !/*ai        303*/
      randp=rang()                                                              !/*ai        304*/
      ptotal=wspr(31)*(pdecay+pcapt+pscat)                                      !/*ai        305*/
      if (randp.gt.ptotal) go to 280                                            !/*ai        306*/
      fdistp=randp/ptotal                                                       !/*ai        307*/
      inp=1                                                                     !/*ai        308*/
      aisctp=aiscat                                                             !/*ai        309*/
      aicapp=aicap                                                              !/*ai        310*/
      fscatp=fscat                                                              !/*ai        311*/
      aidecp=aidec                                                              !/*ai        312*/
      aitotp=aitot                                                              !/*ai        313*/
      pdecp=pdecay                                                              !/*ai        314*/
      pcapp=pcapt                                                               !/*ai        315*/
      pscatp=pscat                                                              !/*ai        316*/
      ptotap=ptotal                                                             !/*ai        317*/
  280 if (ind(30).eq.1) go to 340                                               !/*ai        318*/
      wsp3=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                                    !/*ai        319*/
      selpt1=gws(5)*wsp(5)-gws3*wsp3*wsp(15)                                    !/*ai        320*/
!                                                                               !/*ai        321*/
!        wsp16 = e1(lab) if partner were at rest (m2=e2)                        !/*ai        322*/
!                                                                               !/*ai        323*/
      wsp(16)=selpt1/wsp(4)                                                     !/*ai        324*/
!          "e" is the lab kinetic energy (t1)                                   !/*ai        325*/
      e=(wsp(16)-ws(4))                                                         !/*ai        326*/
      aiso=gws3/gws(5)                                                          !/*ai        327*/
      wsp18=wsp(18)                                                             !/*ai        328*/
      if (np.le.0) wsp18=wsp18*(sel(1)+sel(2))/csa(3)                           !/*ai        329*/
      u1=ws(4)*ws(4)+wsp(19)                                                    !/*ai        330*/
      u=(sqrt(selpt1+selpt1+u1))*1.d-03                                         !/*ai        331*/
      if (u.lt.2.1d0) u=2.1d0                                                   !/*ai        332*/
      call aitota (e,aiso,wsp18,u,v,wsp)                                        !/*ai        333*/
!            wsp20 = density(r)/p1/e2                                           !/*ai        334*/
      wsp(20)=wsp18/(gws3*wsp(5))                                               !/*ai        335*/
      pdecay=wsp18*aidec                                                        !/*ai        336*/
!           wsp21 = density(r) *(p1-prime * m2)/(p1 * e2)                       !/*ai        337*/
!           density in real lab-frame, not artificial e2=m2 one                 !/*ai        338*/
      wsp(21)=wsp(20)*sqrt(selpt1*selpt1-wsp(19)*ws(4)**2)                      !/*ai        339*/
!           all pxxxx = inverse m.f.p [fm**-1]                                  !/*ai        340*/
      pcapt=aicap*wsp(21)                                                       !/*ai        341*/
      pscat=aiscat*wsp(21)                                                      !/*ai        342*/
      if (ind(22).eq.1.or.ind(24).eq.1) go to 410                               !/*ai        343*/
      tdn=wsp(18)*csa(8)                                                        !/*ai        344*/
      if (wsp(3).eq.dp2) tdn=wsp(18)-tdn                                        !/*ai        345*/
      fract=dp1                                                                 !/*ai        346*/
      if (np) 330,290,300                                                       !/*ai        347*/
  290 ik=wsp(3)+1.d-02                                                          !/*ai        348*/
      tdn=wsp(18)*sel(ik)/csa(3)                                                !/*ai        349*/
      if (ifrist(wsp(3),icsb(13),tdn,1).le.1) go to 320                         !/*ai        350*/
      go to 310                                                                 !/*ai        351*/
  300 if (ws(3).le.-dp1.and.(gws(5)-ws(4)-ws(24)).lt.dp0) go to 330             !/*ai        352*/
      if (nopart(wsp(3),tdn,gws,tnpar,nnt,dp1).le.1) go to 320                  !/*ai        353*/
!                                                                               !/*ai        354*/
!         hit a hole, forbid scattering or capture                              !/*ai        355*/
!                                                                               !/*ai        356*/
  310 pcapt=dp0                                                                 !/*ai        357*/
      pscat=dp0                                                                 !/*ai        358*/
      go to 330                                                                 !/*ai        359*/
!                                                                               !/*ai        360*/
!         no hole, but for isospin dependent holes, suppress                    !/*ai        361*/
!         the t=1 component of the "no-hole"  capturing                         !/*ai        362*/
!         change made  1 feb 84 [mrc]                                           !/*ai        363*/
!                                                                               !/*ai        364*/
  320 continue                                                                  !/*ai        365*/
      if (knp.gt.0) fract=0.75d0                                                !/*ai        366*/
  330 pcapt=pcapt*fract                                                         !/*ai        367*/
!                                                                               !/*ai        368*/
      rand=rang()                                                               !/*ai        369*/
!                                                                               !/*ai        370*/
!        ws20 = distance travelled in time-step                                 !/*ai        371*/
!        sum pxxxx = 1/av.m.f.p.                                                !/*ai        372*/
!        ptotal = prob. of interaction in this time-step                        !/*ai        373*/
!                                                                               !/*ai        374*/
      ptotal=ws(20)*(pdecay+pcapt+pscat)                                        !/*ai        375*/
      ws20=ws(20)                                                               !/*ai        376*/
      if (rand.gt.ptotal) go to 340                                             !/*ai        377*/
      int=1                                                                     !/*ai        378*/
      fdist=rand/ptotal                                                         !/*ai        379*/
  340 if (int+inp-1) 150,350,380                                                !/*ai        380*/
  350 if (int.eq.1) go to 390                                                   !/*ai        381*/
  360 e=wspr(5)                                                                 !/*ai        382*/
      p(1)=wspr3                                                                !/*ai        383*/
      p(2)=wspr(6)                                                              !/*ai        384*/
      p(3)=wspr(7)                                                              !/*ai        385*/
      p(4)=wspr(8)                                                              !/*ai        386*/
      call plt (p,e,pt,ee,voc,gama)                                             !/*ai        387*/
      wspr(5)=ee                                                                !/*ai        388*/
      wspr(6)=pt(2)                                                             !/*ai        389*/
      wspr(7)=pt(3)                                                             !/*ai        390*/
      wspr(8)=pt(4)                                                             !/*ai        391*/
      wspr(9)=pt(1)                                                             !/*ai        392*/
      wspr(15)=pt(2)*gws(6)+pt(3)*gws(7)+pt(4)*gws(8)                           !/*ai        393*/
      do 370 i=1,21                                                             !/*ai        394*/
  370 wsp(i)=wspr(i)                                                            !/*ai        395*/
      wsp(31)=-dp1                                                              !/*ai        396*/
      ws20=wspr(31)                                                             !/*ai        397*/
      ind(32)=0                                                                 !/*ai        398*/
      aiscat=aisctp                                                             !/*ai        399*/
      aicap=aicapp                                                              !/*ai        400*/
      aidec=aidecp                                                              !/*ai        401*/
      aitot=aitotp                                                              !/*ai        402*/
      fscat=fscatp                                                              !/*ai        403*/
      pdecay=pdecp                                                              !/*ai        404*/
      pcapt=pcapp                                                               !/*ai        405*/
      pscat=pscatp                                                              !/*ai        406*/
      ptotal=ptotap                                                             !/*ai        407*/
      fdist=fdistp                                                              !/*ai        408*/
      go to 390                                                                 !/*ai        409*/
  380 ptp=ptotal/(ptotal+ptotap)                                                !/*ai        410*/
      randpt=rang()                                                             !/*ai        411*/
      if (randpt.gt.ptp) go to 360                                              !/*ai        412*/
!  there will be an isobar interaction.which type?                              !/*ai        413*/
  390 wsp(21)=ptotal/ws20                                                       !/*ai        414*/
      rant=rang()                                                               !/*ai        415*/
      if (rant.gt.(ws20*pdecay/ptotal)) go to 400                               !/*ai        416*/
!  its a decay                                                                  !/*ai        417*/
      aitot=aidec                                                               !/*ai        418*/
      aicap=dp0                                                                 !/*ai        419*/
      aiscat=dp0                                                                !/*ai        420*/
      go to 160                                                                 !/*ai        421*/
!  there will be an isobar-nucleon interaction                                  !/*ai        422*/
  400 aidec=dp0                                                                 !/*ai        423*/
      aitot=aicap+aiscat                                                        !/*ai        424*/
      go to 160                                                                 !/*ai        425*/
!  just want the capture and decay probabilities                                !/*ai        426*/
!-----------------------------------------------------------------------        !/*ai        427*/
  410 continue                                                                  !/*ai        428*/
      if (ind(31).eq.1) go to 430                                               !/*ai        429*/
      if (ws(31).eq.dp0.and.ind(30).eq.0) go to 430                             !/*ai        430*/
      do 420 i=1,21                                                             !/*ai        431*/
  420 wsp(i)=wspr(i)                                                            !/*ai        432*/
      wsp(31)=-dp1                                                              !/*ai        433*/
      if (ind(32).ne.2) ind(32)=0                                               !/*ai        434*/
      go to 440                                                                 !/*ai        435*/
  430 continue                                                                  !/*ai        436*/
      if (ind(32).ne.2) ind(32)=1                                               !/*ai        437*/
!-----------------------------------------------------------------------        !/*ai        438*/
  440 aiscat=dp0                                                                !/*ai        439*/
      if ((aidec+aicap).le.dp0) return                                          !/*ai        440*/
      aidec=pdecay/(pdecay+pcapt)                                               !/*ai        441*/
      aicap=dp1-aidec                                                           !/*ai        442*/
      aitot=dp1                                                                 !/*ai        443*/
      return                                                                    !/*ai        444*/
      end                                                                       !/*ai        445*/

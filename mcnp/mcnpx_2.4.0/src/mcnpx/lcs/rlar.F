!     $Id: rlar.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                         !/*rl          1*/
      subroutine rlar                                                           !/*rl          2*/
#include <lcs/double.h>                                                         /*!rl          3*/
!     ==================================================================        !/*rl          4*/
!     rlar takes care of the particle that has advanced out of                  !/*rl          5*/
!     the nucleus.                                                              !/*rl          6*/
!                                                                               !/*rl          7*/
! RLAR   calls   COUNT  SQRT  ISEDT  PLT  REGB  REFL  PSEL  AIOUT               !/*rl          8*/
! called by ISABEL                                                              !/*rl          9*/
!     ==================================================================        !/*rl         10*/
!                                                                               !/*rl         11*/
#include <lcs/vegas.h>                                                          /*!rl         12*/
      dimension pout(4), cutoff(2), poutt(4)                                    !/*rl         13*/
!                                                                               !/*rl         14*/
! ind(4)=1 when a particle has advanced out of the nucleus.                     !/*rl         15*/
!                                                                               !/*rl         16*/
      ind(4)=1                                                                  !/*rl         17*/
      ind19=ind(19)                                                             !/*rl         18*/
      ind(19)=0                                                                 !/*rl         19*/
!  "virgin" projectile particles leaving the target                             !/*rl         20*/
!  or without prospect to find a target partner can                             !/*rl         21*/
!  be only partners and should be neutralized in st.                            !/*rl         22*/
      if (ind(34).eq.2) go to 170                                               !/*rl         23*/
      ind21=ind(21)                                                             !/*rl         24*/
      bet=-voc                                                                  !/*rl         25*/
! part of change made 10/19/70 to force pi+ below cut-off to be                 !/*rl         26*/
! reflected                                                                     !/*rl         27*/
      ipisw=0                                                                   !/*rl         28*/
!  refra6 is the cutoff energy of the particle to see if it can                 !/*rl         29*/
!  get out.  it is the sum of binding energy plus the fermi potential           !/*rl         30*/
!  for neutron.  it is binding energy, fermi potential, and coulomb             !/*rl         31*/
!  barrier for proton. i++ has cutoff of (e-binding)+2*(e-coulomb).             !/*rl         32*/
!  i+,i- have cutoff of (e-binding)+(e-coulomb). i0 has the same                !/*rl         33*/
!  cutoff as neutrons. pi0 feels the pion potential. pi+,pi- feel               !/*rl         34*/
!  in addition the coulomb potential.                                           !/*rl         35*/
      ipi=0                                                                     !/*rl         36*/
      ika=0                                                                     !/*rl         37*/
      vp=dp0                                                                    !/*rl         38*/
      if (ind21.gt.3) vp=vpion                                                  !/*rl         39*/
      ipt=0                                                                     !/*rl         40*/
      if (ws(31).ne.dp0) ipt=9                                                  !/*rl         41*/
      cutoff(1)=cutofa(ipt+1)                                                   !/*rl         42*/
      cutoff(2)=cutofa(ipt+2)                                                   !/*rl         43*/
!                 csa11 = coulomb energy                                        !/*rl         44*/
      csa11=csa(11)                                                             !/*rl         45*/
      iws=ws(3)                                                                 !/*rl         46*/
      if (iws.gt.-1) go to 10                                                   !/*rl         47*/
      refra6=ws(24)                                                             !/*rl         48*/
!  force ap to refract/reflect irrespective of the model                        !/*rl         49*/
      if ((sel(1)+sel(2)).eq.dp0.and.ind(34).eq.3) then                         !/*rl         50*/
        if ((ws(5)-ws(4)-refra6).lt.dp0) go to 200                              !/*rl         51*/
        go to 50                                                                !/*rl         52*/
      endif                                                                     !/*rl         53*/
      if (ws(3).eq.-dp1.and.(ws(5)-ws(4)-refra6-csa11).lt.dp0) ipisw=1          !/*rl         54*/
      go to 160                                                                 !/*rl         55*/
   10 if (iws.gt.2.and.iws.lt.6) go to 30                                       !/*rl         56*/
      if ((iws.gt.6).and.(iws.lt.10)) go to 20                                  !/*rl         57*/
      if (iws.gt.10) iws=iws/10                                                 !/*rl         58*/
      refra6=cutoff(iws)+ws(24)                                                 !/*rl         59*/
      if (ws(3).eq.13.d0.or.ws(3).eq.25.d0) refra6=refra6+csa11                 !/*rl         60*/
      if (ws(3).gt.dp10) refra6=refra6+vp                                       !/*rl         61*/
      go to 40                                                                  !/*rl         62*/
   20 refra6=csa11                                                              !/*rl         63*/
      if (ws(3).eq.8.d0) refra6=dp0                                             !/*rl         64*/
      ika=1                                                                     !/*rl         65*/
      go to 40                                                                  !/*rl         66*/
   30 ipi=1                                                                     !/*rl         67*/
      refra6=vp+csa11                                                           !/*rl         68*/
      if (ws(3).ne.dp4) go to 40                                                !/*rl         69*/
      if (vp.eq.dp0) go to 170                                                  !/*rl         70*/
      refra6=vp                                                                 !/*rl         71*/
   40 go to (160,160,50,160,160,50), ind21                                      !/*rl         72*/
! statement 35 up to not including 30 is for models without refraction.         !/*rl         73*/
!  then only energy is checked.  for pion0 and pion- since they feel no         !/*rl         74*/
!  potential their energies are not checked at all.                             !/*rl         75*/
! changes made 3/2/71 so pions have a potential                                 !/*rl         76*/
! if particle cannot get out for all particles except pion+, let                !/*rl         77*/
!  particle fall below cutoff. ind(20)=dp1 for isobars below cutoff             !/*rl         78*/
!  ind(22)=dp1  pion+ is reflected if it cannot leave the nucleus.              !/*rl         79*/
   50 if (ipi.eq.1.and.istept.eq.0) go to 130                                   !/*rl         80*/
      if (ws(31).ne.dp0.or.istept.eq.0) go to 120                               !/*rl         81*/
      if ((ws(5)-ws(4)-refra6).lt.0) go to 60                                   !/*rl         82*/
!                                                                               !/*rl         83*/
!        above-cutoff branch                                                    !/*rl         84*/
!                                                                               !/*rl         85*/
      if (ipi.eq.1) go to 130                                                   !/*rl         86*/
      if ((ws(3).lt.dp10).and.(ws(3).gt.6.d0)) go to 140                        !/*rl         87*/
      ws(5)=ws(5)-csa(5)-ws(24)                                                 !/*rl         88*/
      go to 150                                                                 !/*rl         89*/
!                                                                               !/*rl         90*/
!     below cutoff branch                                                       !/*rl         91*/
!                                                                               !/*rl         92*/
! part of change made 10/19/70 to force pi+ below cut-off to be                 !/*rl         93*/
! reflected                                                                     !/*rl         94*/
!                                                                               !/*rl         95*/
   60 continue                                                                  !/*rl         96*/
      if (ind19.ne.0) then                                                      !/*rl         97*/
        kcan=1                                                                  !/*rl         98*/
        lkcan='rlar 2'                                                          !/*rl         99*/
        return                                                                  !/*rl        100*/
      endif                                                                     !/*rl        101*/
      if (ipi.eq.0) go to 70                                                    !/*rl        102*/
!         change made 3/2/71 so all pions below cutoff are reflected            !/*rl        103*/
      ipisw=1                                                                   !/*rl        104*/
      go to 160                                                                 !/*rl        105*/
!                                                                               !/*rl        106*/
!     ultimately, when killing off kaons below cutoff, put in the               !/*rl        107*/
!     20% branch of decays into 2pions                                          !/*rl        108*/
!                                                                               !/*rl        109*/
   70 continue                                                                  !/*rl        110*/
      ind(20)=1                                                                 !/*rl        111*/
      iws3=ws(3)                                                                !/*rl        112*/
      if (iws3.gt.10) go to 100                                                 !/*rl        113*/
      if (ws(31).ne.dp0) go to 80                                               !/*rl        114*/
      ex=ex+ws(5)-ws(4)-ef(iws3,istept)                                         !/*rl        115*/
! put in nucleon                                                                !/*rl        116*/
      call countv (ws(3),1,2)                                                   !/*rl        117*/
      pm=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                          !/*rl        118*/
      px=ws(6)*pm                                                               !/*rl        119*/
      py=ws(7)*pm                                                               !/*rl        120*/
      pz=ws(8)*pm                                                               !/*rl        121*/
      pit(1)=pit(1)+px                                                          !/*rl        122*/
      pit(2)=pit(2)+py                                                          !/*rl        123*/
      pit(3)=pit(3)+pz                                                          !/*rl        124*/
      pit(4)=pit(4)+ws(11)*pz-ws(12)*py                                         !/*rl        125*/
      pit(5)=pit(5)+ws(12)*px-ws(10)*pz                                         !/*rl        126*/
      pit(6)=pit(6)+ws(10)*py-ws(11)*px                                         !/*rl        127*/
      return                                                                    !/*rl        128*/
   80 expr=expr+eoutt-ws(4)-pef(iws3,istepp)                                    !/*rl        129*/
! put in nucleon                                                                !/*rl        130*/
      call countv (ws(3),2,2)                                                   !/*rl        131*/
!      particle below cutoff in projectile must                                 !/*rl        132*/
!     eventually leave the target.  This corrects the                           !/*rl        133*/
!     miscounting    [yy 24jul84]                                               !/*rl        134*/
      if (istept.eq.0) go to 90                                                 !/*rl        135*/
      ind34=ind(34)                                                             !/*rl        136*/
      ind(34)=2                                                                 !/*rl        137*/
      call isedt (ws)                                                           !/*rl        138*/
      ind(34)=ind34                                                             !/*rl        139*/
!                                                                               !/*rl        140*/
   90 ws10=ws(10)-proj(5)                                                       !/*rl        141*/
      ws11=ws(11)-proj(6)                                                       !/*rl        142*/
      ws12=(ws(12)-proj(7))/gama                                                !/*rl        143*/
      pir(1)=pir(1)+poutt(1)*poutt(2)                                           !/*rl        144*/
      pir(2)=pir(2)+poutt(1)*poutt(3)                                           !/*rl        145*/
      pir(3)=pir(3)+poutt(1)*poutt(4)                                           !/*rl        146*/
      pir(4)=pir(4)+poutt(1)*(ws11*poutt(4)-ws12*poutt(3))                      !/*rl        147*/
      pir(5)=pir(5)+poutt(1)*(ws12*poutt(2)-ws10*poutt(4))                      !/*rl        148*/
      pir(6)=pir(6)+poutt(1)*(ws10*poutt(3)-ws11*poutt(2))                      !/*rl        149*/
      return                                                                    !/*rl        150*/
  100 ind(22)=1                                                                 !/*rl        151*/
      if (ws(3).eq.13.d0.or.ws(3).eq.25.d0) ind(12)=1                           !/*rl        152*/
      do 110 i=1,8                                                              !/*rl        153*/
      ws(i+4)=gws(i+4)                                                          !/*rl        154*/
  110 continue                                                                  !/*rl        155*/
      ws(14)=gws(1)                                                             !/*rl        156*/
      ws(24)=gws(2)                                                             !/*rl        157*/
      ws(25)=gws(3)                                                             !/*rl        158*/
      return                                                                    !/*rl        159*/
  120 if (istepp.eq.0.or.ws(31).eq.dp0) go to 170                               !/*rl        160*/
      pout(1)=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                     !/*rl        161*/
      pout(2)=ws(6)                                                             !/*rl        162*/
      pout(3)=ws(7)                                                             !/*rl        163*/
      pout(4)=ws(8)                                                             !/*rl        164*/
!     ------------------------------------------                                !/*rl        165*/
      call plt (pout,ws(5),poutt,eoutt,bet,gama)                                !/*rl        166*/
!     ------------------------------------------                                !/*rl        167*/
      if ((eoutt-ws(4)-refra6).lt.dp0) go to 60                                 !/*rl        168*/
      if (ipi.eq.1) go to 130                                                   !/*rl        169*/
      eoutt=eoutt-csa(15)-ws(24)                                                !/*rl        170*/
      poutt(1)=sqrt(eoutt*eoutt-ws(4)*ws(4))                                    !/*rl        171*/
!     ------------------------------------------                                !/*rl        172*/
      call plt (poutt,eoutt,pout,ws(5),voc,gama)                                !/*rl        173*/
!     ------------------------------------------                                !/*rl        174*/
      go to 150                                                                 !/*rl        175*/
! statement 36 is for particles with enough energy to leave.                    !/*rl        176*/
! for pion+ its energy inside and outside the nucleus is the same.              !/*rl        177*/
!  so just let it out.                                                          !/*rl        178*/
! for other particles the binding energy and the fermi kinetic energy           !/*rl        179*/
!  have to be subtracted.                                                       !/*rl        180*/
! change made 3/2/71 so correct pion energy outside calc.                       !/*rl        181*/
! change made 3/23/71 to allow for attractive coulomb field on pi- i-           !/*rl        182*/
!                                                                               !/*rl        183*/
  130 if (ws(3).eq.dp5) ws(5)=ws(5)-csa11                                       !/*rl        184*/
      if (ws(3).eq.dp3) ws(5)=ws(5)+csa11                                       !/*rl        185*/
      ws(5)=ws(5)-vp                                                            !/*rl        186*/
      go to 170                                                                 !/*rl        187*/
  140 continue                                                                  !/*rl        188*/
      if (ws(3).eq.9.d0) ws(5)=ws(5)-csa11                                      !/*rl        189*/
      if (ws(3).eq.7.d0) ws(5)=ws(5)+csa11                                      !/*rl        190*/
      go to 170                                                                 !/*rl        191*/
! change made 3/23/71 to allow for attractive coulomb field on pi- i-           !/*rl        192*/
!                                                                               !/*rl        193*/
  150 if (ws(3).eq.25.d0) ws(5)=ws(5)-csa11                                     !/*rl        194*/
      if (ws(3).eq.13.d0) ws(5)=ws(5)+csa11                                     !/*rl        195*/
      if (ws(3).gt.dp10) ws(5)=ws(5)-vp                                         !/*rl        196*/
      go to 170                                                                 !/*rl        197*/
! in the following statements, the radial component of momentum is              !/*rl        198*/
!  calculated, and checked to see if the particle can get out.                  !/*rl        199*/
!                                                                               !/*rl        200*/
  160 delem=-refra6                                                             !/*rl        201*/
!     if (ws(3).eq.3) write(6,170) istept,istepp,refra6,ws                      !/*rl        202*/
!  in case of reflection or refraction the particle is brought                  !/*rl        203*/
!  by regb back to the surface.                                                 !/*rl        204*/
!                                                                               !/*rl        205*/
!     ------------------------------------------                                !/*rl        206*/
      call regb (dist)                                                          !/*rl        207*/
      call refl (ipisw,delem,csa11,iref)                                        !/*rl        208*/
!     ------------------------------------------                                !/*rl        209*/
      if (iref.eq.1) ind(34)=-3                                                 !/*rl        210*/
      if (kcan.eq.1.or.iref.eq.1) return                                        !/*rl        211*/
! statement 33 to 17 energy outside is calculated.                              !/*rl        212*/
!  33 if(ws(3).gt.dp10) go to 39                                                !/*rl        213*/
!  47 if(ws(3)-2.d0) 17,18,48                                                   !/*rl        214*/
!  48 if(ws(3)-dp4) 17,18,16                                                    !/*rl        215*/
!  39 if(ws(3)-14.d0) 34,17,32                                                  !/*rl        216*/
!  34 ws(5)=ws(5)+csa11+csa11                                                   !/*rl        217*/
!     go to 18                                                                  !/*rl        218*/
!  32 if(ws(3)-24.d0) 18,18,16                                                  !/*rl        219*/
! fori- refraction at the surface is influenced by the negative coulomb         !/*rl        220*/
!  barrier it feels.  to check if i- has enough to get out it only has          !/*rl        221*/
!  to have enough energy to overcome the nuclear potential.                     !/*rl        222*/
! change made 3/23/71 to allow for attractive coulomb field on pi- i-           !/*rl        223*/
!  16 ws(5)=ws(5)-csa11                                                         !/*rl        224*/
!     go to 18                                                                  !/*rl        225*/
!  17 ws(5)=ws(5)+csa11                                                         !/*rl        226*/
!                                                                               !/*rl        227*/
!-----------------------------------------------------------------------        !/*rl        228*/
! To trap "5.d+02" particles that get here without passing through PSEL.        !/*rl        229*/
  170 continue                                                                  !/*rl        230*/
      if (ws(31).eq.5.d+02) then                                                !/*rl        231*/
        call psel (gdc9,gdc10)                                                  !/*rl        232*/
        return                                                                  !/*rl        233*/
      endif                                                                     !/*rl        234*/
      icsb13=icsb(13)                                                           !/*rl        235*/
!-----------------------------------------------------------------------        !/*rl        236*/
      st(icsb13+1)=-ws(1)                                                       !/*rl        237*/
      if (ind(34).eq.3) go to 180                                               !/*rl        238*/
      ws(2)=dp0                                                                 !/*rl        239*/
      iws3=ws(3)                                                                !/*rl        240*/
      spr(iws3)=spr(iws3)-dp1                                                   !/*rl        241*/
      spr(2+iws3)=spr(2+iws3)+dp1                                               !/*rl        242*/
  180 icsb(1)=icsb(1)-1                                                         !/*rl        243*/
! ind(6) is set to one, if a particle is leaving the nucleus.                   !/*rl        244*/
! when a particle leaves, edit is called.                                       !/*rl        245*/
      ind(6)=1                                                                  !/*rl        246*/
      st(icsb13+13)=ws(19)-ws(16)                                               !/*rl        247*/
! if isobar leaves the nucleus, aiout is called to make isobar decay            !/*rl        248*/
!  at the surface.                                                              !/*rl        249*/
      if (ws(3).lt.10) go to 190                                                !/*rl        250*/
!     ------------------------------------------                                !/*rl        251*/
      call aiout                                                                !/*rl        252*/
!     ------------------------------------------                                !/*rl        253*/
      if (kcan.ne.0) return                                                     !/*rl        254*/
! if ind(18)=1, then decay is not allowed. then ind(22) is set to 1             !/*rl        255*/
!  so that another partner is chosen and the isobar can either scatter          !/*rl        256*/
!  or capture.                                                                  !/*rl        257*/
! change made 10/19/70 so that isobars unable to escape because they            !/*rl        258*/
! can not decay and isobars below cut-off are treated the same.they are         !/*rl        259*/
! taken back to where they were before being advanced and forced to             !/*rl        260*/
! do something there.                                                           !/*rl        261*/
      if (ind(18).eq.1) go to 100                                               !/*rl        262*/
      return                                                                    !/*rl        263*/
!     ------------------------------------------                                !/*rl        264*/
  190 call isedt (ws)                                                           !/*rl        265*/
!     ------------------------------------------                                !/*rl        266*/
      if (kcan.ne.0) return                                                     !/*rl        267*/
      return                                                                    !/*rl        268*/
  200 kcan=1                                                                    !/*rl        269*/
      lkcan='rlar 1'                                                            !/*rl        270*/
      return                                                                    !/*rl        271*/
!                                                                               !/*rl        272*/
      end                                                                       !/*rl        273*/

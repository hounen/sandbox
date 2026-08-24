!     $Id: pgamma.F,v 1.5 2002/03/27 18:15:13 gwm Exp $                       !/*pg          1*/
      subroutine pgamma                                                         !/*pg          2*/
#include <lcs/double.h>                                                         /*!pg          3*/
!     ==================================================================        !/*pg          4*/
!     gamma takes care of the interaction mechanism of a collision.             !/*pg          5*/
!     also, in programs without refraction if a particle cannot                 !/*pg          6*/
!     leave the nucleus, then it is treated as if it fell below                 !/*pg          7*/
!     the cutoff.                                                               !/*pg          8*/
!                                                                               !/*pg          9*/
! GAMMA  calls   CMA  ANGLEN  ELAS  CEP  PIH  SQRT  SETWIG  FLTRNF              !/*pg         10*/
!                RESTO  CMB  CMC  PLT  WWSST  DCNPAR  COUNT  ISEDT              !/*pg         11*/
!                UPDECA  SAVE  EXP  ALOG  FNKAON  PMKD PNCDG  PHSBST            !/*pg         12*/
!                SQRT  GETWIG  AINTER  DECAY  AIANGL  TWOPIN  ONEPIN            !/*pg         13*/
!                ANGLI1  BRANCH  PANGLE  KRANCH  PANGLK  PIN1PI                 !/*pg         14*/
! called by ISABEL                                                              !/*pg         15*/
!     ==================================================================        !/*pg         16*/
!                                                                               !/*pg         17*/
#include <lcs/inout.h>                                                          /*!pg         18*/
#include <lcs/vegas.h>                                                          /*!pg         19*/
      logical lkaon                                                             !/*pg         20*/
      dimension p1(4), p2(4), pt1(4), pt2(4), tecm(15), chrg(i20), xmpr         !/*pg         21*/
     & (i20), akx(i20), aky(i20), akz(i20), appt(4)                             !/*pg         22*/
      dimension aex(i20), apx(i20), akq(i20)                                    !/*pg         23*/
!                                                                               !/*pg         24*/
      data frkpkm, frkpk0, frk0k0, frk0km /4*0.25d0/                            !/*pg         25*/
!                                                                               !/*pg         26*/
      r60=i60                                                                   !/*pg         27*/
      bet=-voc                                                                  !/*pg         28*/
!-----------------------------------------------------------------------        !/*pg         29*/
! First attempt to fix CASCAS.                                                  !/*pg         30*/
      ind(35)=0                                                                 !/*pg         31*/
      ind(36)=0                                                                 !/*pg         32*/
      ist=0                                                                     !/*pg         33*/
      ipl=0                                                                     !/*pg         34*/
      if (wsp(30).eq.-99.d0) then                                               !/*pg         35*/
        wsp(30)=dp0                                                             !/*pg         36*/
        ind(35)=1                                                               !/*pg         37*/
      elseif (wsp(30).eq.+99.d0) then                                           !/*pg         38*/
        wsp(30)=dp0                                                             !/*pg         39*/
        ind(35)=2                                                               !/*pg         40*/
      endif                                                                     !/*pg         41*/
!-----------------------------------------------------------------------        !/*pg         42*/
      csa11=csa(11)                                                             !/*pg         43*/
      if (ws(3).eq.13.d0.or.ws(3).eq.25.d0) ind(12)=1                           !/*pg         44*/
      ipl=0                                                                     !/*pg         45*/
      notrys=0                                                                  !/*pg         46*/
      ind21=ind(21)                                                             !/*pg         47*/
      ws4=ws(4)                                                                 !/*pg         48*/
      ws24=ws(24)                                                               !/*pg         49*/
      iext=0                                                                    !/*pg         50*/
      kext=0                                                                    !/*pg         51*/
      iexq=0                                                                    !/*pg         52*/
      ws31=ws(31)                                                               !/*pg         53*/
      wsp31=wsp(31)                                                             !/*pg         54*/
      wsp4=wsp(4)                                                               !/*pg         55*/
      efp=dp0                                                                   !/*pg         56*/
      efn=dp0                                                                   !/*pg         57*/
      pefp=dp0                                                                  !/*pg         58*/
      pefn=dp0                                                                  !/*pg         59*/
      if (ws31.eq.dp0) go to 10                                                 !/*pg         60*/
!          particle is from projectile                                          !/*pg         61*/
      if (istepp.eq.0) go to 20                                                 !/*pg         62*/
      efp=pef(1,istepp)                                                         !/*pg         63*/
      efn=pef(2,istepp)                                                         !/*pg         64*/
      go to 20                                                                  !/*pg         65*/
!          particle is from target                                              !/*pg         66*/
   10 if (istept.eq.0) go to 20                                                 !/*pg         67*/
      efp=ef(1,istept)                                                          !/*pg         68*/
      efn=ef(2,istept)                                                          !/*pg         69*/
   20 if (wsp31.eq.dp0) go to 30                                                !/*pg         70*/
!         partner is from projectile                                            !/*pg         71*/
      if (istepp.eq.0) go to 40                                                 !/*pg         72*/
      pefp=pef(1,istepp)                                                        !/*pg         73*/
      pefn=pef(2,istepp)                                                        !/*pg         74*/
      go to 40                                                                  !/*pg         75*/
!         partner is from target                                                !/*pg         76*/
   30 if (istept.eq.0) go to 40                                                 !/*pg         77*/
      pefp=ef(1,istept)                                                         !/*pg         78*/
      pefn=ef(2,istept)                                                         !/*pg         79*/
!                                                                               !/*pg         80*/
! ind(22)=1 when isobar falls below cutoff or when isobars cannot get           !/*pg         81*/
!  out for model with no refraction or when the kinetic energy of the           !/*pg         82*/
!  isobar becomes negative.  in other words whenever one wants to force         !/*pg         83*/
!  the isobar to do something, that is capture or decay.                        !/*pg         84*/
!                                                                               !/*pg         85*/
   40 if (ind(22).eq.1) go to 60                                                !/*pg         86*/
      if (ind(34).gt.0) go to 50                                                !/*pg         87*/
      go to (60,60,50,60,60,50), ind21                                          !/*pg         88*/
   50 if (ind(20)-1) 60,950,950                                                 !/*pg         89*/
!                                                                               !/*pg         90*/
!  statement 21 takes care of particles below cutoff.                           !/*pg         91*/
! ind(1)=1 when particle is in interaction mode, that is in subr                !/*pg         92*/
! in ainter it is determined if a collision is probable or not.                 !/*pg         93*/
!                                                                               !/*pg         94*/
   60 ind(1)=1                                                                  !/*pg         95*/
!                                                                               !/*pg         96*/
!  statement 1100 and following are for ap interaction                          !/*pg         97*/
!                                                                               !/*pg         98*/
      if (ws(3).le.dp0) go to 1160                                              !/*pg         99*/
!                                                                               !/*pg        100*/
! statement 50 and the following are for isobar.                                !/*pg        101*/
!                                                                               !/*pg        102*/
      if (ws(3).ge.dp10) go to 1360                                             !/*pg        103*/
!                                                                               !/*pg        104*/
!       subrs cma,cmb,and cmc are center of mass transformations.               !/*pg        105*/
!                                                                               !/*pg        106*/
      call cma                                                                  !/*pg        107*/
!                                                                               !/*pg        108*/
!**** added for pi production                                                   !/*pg        109*/
!                                                                               !/*pg        110*/
      if (ind(28).eq.1) go to 1720                                              !/*pg        111*/
!                                                                               !/*pg        112*/
!***added for high energy pi-n interactions                                     !/*pg        113*/
!                                                                               !/*pg        114*/
      if ((ws(3).gt.dp2).and.(ws(3).lt.6.d0)) go to 1730                        !/*pg        115*/
!                                                                               !/*pg        116*/
!** added to include kaon scattering                                            !/*pg        117*/
      if ((ws(3).gt.6.d0).and.(ws(3).le.9.d0)) go to 1740                       !/*pg        118*/
!                                                                               !/*pg        119*/
! psel6 contains the kinetic energy in the center of mass.                      !/*pg        120*/
!                                                                               !/*pg        121*/
      psel6=(cm(3)-ws(4)-wsp(4))                                                !/*pg        122*/
!                                                                               !/*pg        123*/
! anglen is the angular distribution subr.                                      !/*pg        124*/
!                                                                               !/*pg        125*/
      call anglen (ws(3),wsp(3),psel6,ans)                                      !/*pg        126*/
!                                                                               !/*pg        127*/
! elas is the elastic scattering subr                                           !/*pg        128*/
!                                                                               !/*pg        129*/
   70 call elas (ans)                                                           !/*pg        130*/
!                                                                               !/*pg        131*/
! paul1 contains the total energy of the particle in the lab after              !/*pg        132*/
!  collision.                                                                   !/*pg        133*/
!                checks to see if p1,t2 are now t1,p2 or not; program           !/*pg        134*/
!                conserves # of proj,targ for various reasons;                  !/*pg        135*/
!                 decides which is which by which pair has lowest               !/*pg        136*/
!                energy. in ties, flip/flop is changed at stmt#350              !/*pg        137*/
!                                                                               !/*pg        138*/
      paul1=(cm(4)*cm(11)+cm(2)*cm(14))/cm(3)                                   !/*pg        139*/
      paul2=(cm(2)*cm(10)+cm(4)*cm(7))/cm(3)                                    !/*pg        140*/
      tws24=ws(24)                                                              !/*pg        141*/
      twsp24=wsp(24)                                                            !/*pg        142*/
      tws31=ws(31)                                                              !/*pg        143*/
      twsp31=wsp(31)                                                            !/*pg        144*/
      if (csa(1).ne.dp0) go to 130                                              !/*pg        145*/
      if (ws(3).gt.dp2.and.ws(3).lt.dp10) go to 130                             !/*pg        146*/
      if (wsp(3).gt.dp2.and.wsp(3).lt.dp10) go to 130                           !/*pg        147*/
      if (tws31.eq.dp0) go to 80                                                !/*pg        148*/
      if (twsp31.ne.dp0) go to 130                                              !/*pg        149*/
      iext=1                                                                    !/*pg        150*/
      if ((paul2-ws(4)).ge.(paul1-wsp(4))) go to 130                            !/*pg        151*/
      go to 90                                                                  !/*pg        152*/
   80 if (twsp31.eq.dp0) go to 130                                              !/*pg        153*/
      iext=2                                                                    !/*pg        154*/
      if ((paul1-wsp(4)).ge.(paul2-ws(4))) go to 130                            !/*pg        155*/
!                                                                               !/*pg        156*/
!  a pseudo-exchange of projectile and target particles                         !/*pg        157*/
!                                                                               !/*pg        158*/
   90 kext=1                                                                    !/*pg        159*/
      iext=3-iext                                                               !/*pg        160*/
      ws(31)=dp0                                                                !/*pg        161*/
      wsp(31)=dp0                                                               !/*pg        162*/
      if (tws31.eq.dp0) ws(31)=-dp1                                             !/*pg        163*/
      if (twsp31.eq.dp0) wsp(31)=-dp1                                           !/*pg        164*/
      ws(24)=twsp24                                                             !/*pg        165*/
      wsp(24)=tws24                                                             !/*pg        166*/
!                                                                               !/*pg        167*/
!  for different kinds of particles a fudge factor delta                        !/*pg        168*/
!  is calculated in order to conserve energy                                    !/*pg        169*/
!                                                                               !/*pg        170*/
      if (ws(3).eq.wsp(3)) go to 130                                            !/*pg        171*/
      kext=2                                                                    !/*pg        172*/
      do 100 ic=1,15                                                            !/*pg        173*/
  100 tecm(ic)=cm(ic)                                                           !/*pg        174*/
      del1=dp0                                                                  !/*pg        175*/
      del2=dp0                                                                  !/*pg        176*/
      delp=dp0                                                                  !/*pg        177*/
      ii1=ws(3)                                                                 !/*pg        178*/
      ii2=wsp(3)                                                                !/*pg        179*/
      if (ii1.gt.10) ii1=ii1/10                                                 !/*pg        180*/
      if (ii2.gt.10) ii2=ii2/10                                                 !/*pg        181*/
      if (istepp.gt.0) delp=pef(ii1,istepp)-pef(ii2,istepp)                     !/*pg        182*/
      if (istept.gt.0) del2=ef(ii2,istept)-ef(ii1,istept)                       !/*pg        183*/
      ws(24)=dp0                                                                !/*pg        184*/
      wsp(24)=dp0                                                               !/*pg        185*/
      if (ws(31).eq.dp0) go to 110                                              !/*pg        186*/
      if (istepp.gt.0) ws(24)=pef(ii1,istepp)                                   !/*pg        187*/
      if (istept.gt.0) wsp(24)=ef(ii2,istept)                                   !/*pg        188*/
      if (istepp.gt.0) call cep (delp,del1,wsp4,wsp)                            !/*pg        189*/
      delta=del1+del2                                                           !/*pg        190*/
      go to 120                                                                 !/*pg        191*/
  110 delp=-delp                                                                !/*pg        192*/
      if (istept.gt.0) ws(24)=ef(ii1,istept)                                    !/*pg        193*/
      if (istepp.gt.0) wsp(24)=pef(ii2,istepp)                                  !/*pg        194*/
      if (istepp.gt.0) call cep (delp,del1,ws4,ws)                              !/*pg        195*/
      delta=del1-del2                                                           !/*pg        196*/
  120 call pih (delta)                                                          !/*pg        197*/
      paul1=(cm(4)*cm(11)+cm(2)*cm(14))/cm(3)                                   !/*pg        198*/
      paul2=(cm(2)*cm(10)+cm(4)*cm(7))/cm(3)                                    !/*pg        199*/
!                                                                               !/*pg        200*/
! if paul1 is less than wsp(24),then this is a pauli forbidden                  !/*pg        201*/
! collision if not, do the same check on ws.  if that is also pauli             !/*pg        202*/
!  allowed, then finish the cm transformation via cmb and cmc.                  !/*pg        203*/
!                                                                               !/*pg        204*/
  130 iwsp3=wsp(3)                                                              !/*pg        205*/
      if (istept.eq.0.or.iwsp3.gt.2) go to 230                                  !/*pg        206*/
!                                                                               !/*pg        207*/
!      logic to check pauli principle with wigner function                      !/*pg        208*/
!                                                                               !/*pg        209*/
      if (iden.lt.3) go to 140                                                  !/*pg        210*/
      if (ipauli.eq.0.or.csa(3).le.dp2) go to 230                               !/*pg        211*/
      pf2=paul1**2-wsp(4)**2                                                    !/*pg        212*/
      pf2=max(pf2,dp0)                                                          !/*pg        213*/
      pf=sqrt(pf2)                                                              !/*pg        214*/
      call setwig (pfr(iwsp3,istept),pfw(iwsp3,istept),pf,r1,rhoq)              !/*pg        215*/
      rand=rang()                                                               !/*pg        216*/
      if (rand.gt.r1) go to 230                                                 !/*pg        217*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        218*/
      rat=sel(1)/csa(4)                                                         !/*pg        219*/
      if (iwsp3.eq.2) rat=sel(2)/(csa(3)-csa(4))                                !/*pg        220*/
      rand=rang()                                                               !/*pg        221*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        222*/
!          increment no. of partially unblocked events                          !/*pg        223*/
      go to 150                                                                 !/*pg        224*/
!                                                                               !/*pg        225*/
  140 continue                                                                  !/*pg        226*/
      rand=rang()                                                               !/*pg        227*/
      if (ipauli.eq.0.or.csa(3).le.dp2) go to 230                               !/*pg        228*/
      if (paul1.gt.(wsp(4)+ef(iwsp3,istept))) go to 230                         !/*pg        229*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        230*/
      rat=sel(1)/csa(4)                                                         !/*pg        231*/
      if (iwsp3.eq.2) rat=sel(2)/(csa(3)-csa(4))                                !/*pg        232*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        233*/
!          increment no. of partially unblocked events                          !/*pg        234*/
!          (partner in target = type 3)                                         !/*pg        235*/
  150 ipl=3                                                                     !/*pg        236*/
      nypl(3)=nypl(3)+1                                                         !/*pg        237*/
      if (ind(32).eq.2) nyplc(3)=nyplc(3)+1                                     !/*pg        238*/
      go to 230                                                                 !/*pg        239*/
!                                                                               !/*pg        240*/
!    ind18 = 1 ==> cp**2 < 0., unallowed decay from "pih"                       !/*pg        241*/
!                                                                               !/*pg        242*/
  160 if (ind(18).eq.1) go to 200                                               !/*pg        243*/
!                                                                               !/*pg        244*/
!     cumulate counters of pauli-blocking statistics                            !/*pg        245*/
!                                                                               !/*pg        246*/
      if (ind(29).ge.1) go to 170                                               !/*pg        247*/
      if (ind(28).eq.1) go to 170                                               !/*pg        248*/
      if (ind(27).eq.1) go to 170                                               !/*pg        249*/
      if (ind(13).eq.1.and.fscat.ne.1) go to 170                                !/*pg        250*/
      ident=ws(3)                                                               !/*pg        251*/
      intc=ws(2)                                                                !/*pg        252*/
      i31=ws31                                                                  !/*pg        253*/
      go to 180                                                                 !/*pg        254*/
  170 ident=wws(3)                                                              !/*pg        255*/
      intc=wws(2)                                                               !/*pg        256*/
      i31=wws(31)                                                               !/*pg        257*/
  180 ik=2                                                                      !/*pg        258*/
      if (ident.le.2) go to 190                                                 !/*pg        259*/
      ik=3                                                                      !/*pg        260*/
      if (ident.le.5) go to 190                                                 !/*pg        261*/
      ik=4                                                                      !/*pg        262*/
      if (ident.gt.9) go to 190                                                 !/*pg        263*/
      ik=5                                                                      !/*pg        264*/
  190 npl(1)=npl(1)+1                                                           !/*pg        265*/
      npl(ik)=npl(ik)+1                                                         !/*pg        266*/
      if (ind(32).eq.2) go to 200                                               !/*pg        267*/
!                                                                               !/*pg        268*/
!   ind32 .ne. 2 ==> not a cas-cas interaction                                  !/*pg        269*/
!                                                                               !/*pg        270*/
      if (i31.le.0.or.intc.gt.5) go to 200                                      !/*pg        271*/
      intpl(ik)=intpl(ik)+1                                                     !/*pg        272*/
      intpl(1)=intpl(1)+1                                                       !/*pg        273*/
      if (ind(3).eq.1) repeat=.true.                                            !/*pg        274*/
  200 if (ind(22).lt.1) go to 220                                               !/*pg        275*/
!                                                                               !/*pg        276*/
!  ind(22)=1 and a pauli forbidden collision, then this means one               !/*pg        277*/
!  tried to force the isobar to do something and that thing is                  !/*pg        278*/
!  pauli forbidden.  then pick another angle between the isobar and             !/*pg        279*/
!  its partner.                                                                 !/*pg        280*/
!                                                                               !/*pg        281*/
      if (ind(33).ne.0) go to 210                                               !/*pg        282*/
      wws(5)=wws(5)+dp10                                                        !/*pg        283*/
      gws(5)=gws(5)+dp10                                                        !/*pg        284*/
      if (mprin.gt.2) write (io,1940)                                           !/*pg        285*/
  210 wsp(3)=wwsp(3)                                                            !/*pg        286*/
      wsp(4)=wwsp(4)                                                            !/*pg        287*/
      wsp(5)=wwsp(5)                                                            !/*pg        288*/
      ws(3)=wws(3)                                                              !/*pg        289*/
      ws(31)=wws(31)                                                            !/*pg        290*/
      ws(4)=wws(4)                                                              !/*pg        291*/
      ws(5)=wws(5)                                                              !/*pg        292*/
      wsp(9)=wwsp(9)                                                            !/*pg        293*/
      wsp(24)=wwsp(24)                                                          !/*pg        294*/
      wsp(31)=wwsp(31)                                                          !/*pg        295*/
      ws(24)=wws(24)                                                            !/*pg        296*/
      rand=rang()                                                               !/*pg        297*/
      wsp(15)=dp2*rand-dp1                                                      !/*pg        298*/
      ind(13)=0                                                                 !/*pg        299*/
      ind(14)=0                                                                 !/*pg        300*/
      ind(15)=0                                                                 !/*pg        301*/
      ind(26)=0                                                                 !/*pg        302*/
      ind(18)=0                                                                 !/*pg        303*/
      notrys=notrys+1                                                           !/*pg        304*/
      if (notrys.lt.10) go to 1450                                              !/*pg        305*/
!                     force it to capture after 10 tries                        !/*pg        306*/
      if (ind(33).eq.0) go to 1470                                              !/*pg        307*/
!                                                                               !/*pg        308*/
!      mod added to stop a vicious infinite loop                                !/*pg        309*/
!      where ind33.eq.1, notrys>35000 (1600th cascade                           !/*pg        310*/
!      of a ne+u at 400 mev/n run) [1 mar 84 mrc]                               !/*pg        311*/
!                                                                               !/*pg        312*/
      if (notrys.gt.20) go to 1470                                              !/*pg        313*/
!                                                                               !/*pg        314*/
      aidec=dp0                                                                 !/*pg        315*/
      aiscat=dp0                                                                !/*pg        316*/
      aitot=dp1                                                                 !/*pg        317*/
      aicap=dp1                                                                 !/*pg        318*/
      rand=rang()                                                               !/*pg        319*/
      wsp(15)=dp2*rand-dp1                                                      !/*pg        320*/
      go to 1450                                                                !/*pg        321*/
!                    ind(20)=1 for unallowed collisions.                        !/*pg        322*/
  220 ind(20)=1                                                                 !/*pg        323*/
      ws(24)=ws24                                                               !/*pg        324*/
!-----------------------------------------------------------------------        !/*pg        325*/
! First attempt to fix CASCAS. When a partner is brought into the               !/*pg        326*/
! the target for a CASCAS interaction, but the interaction                      !/*pg        327*/
! is not allowed, then NPMON must be decremented.                               !/*pg        328*/
      if (ind(35).eq.1) then                                                    !/*pg        329*/
        i=wsp(3)                                                                !/*pg        330*/
        npmon(i)=npmon(i)-1                                                     !/*pg        331*/
        npmon(3)=npmon(3)-1                                                     !/*pg        332*/
#ifdef NPM                                                                      /*!pg        333*/
        write (io,1880) npmon,(ind(iii),iii=1,35)                               !/*pg        334*/
#endif                                                                          /*!pg        335*/
! Similarly, if a partner is removed from the target by a CASCAS                !/*pg        336*/
! interaction in the projectile and the interaction is subsequently             !/*pg        337*/
! disallowed, then NPMON must be incremented.                                   !/*pg        338*/
      elseif (ind(35).eq.2) then                                                !/*pg        339*/
        i=wsp(3)                                                                !/*pg        340*/
        npmon(i)=npmon(i)+1                                                     !/*pg        341*/
        npmon(3)=npmon(3)+1                                                     !/*pg        342*/
#ifdef NPM                                                                      /*!pg        343*/
        write (io,1890) npmon,(ind(iii),iii=1,35)                               !/*pg        344*/
#endif                                                                          /*!pg        345*/
      endif                                                                     !/*pg        346*/
!-----------------------------------------------------------------------        !/*pg        347*/
      ws(31)=ws31                                                               !/*pg        348*/
!                                                                               !/*pg        349*/
! for decay (ind(15)=1), capture(ind(14)=1), and for exchange                   !/*pg        350*/
!  scattering (ind(13)=1 and fscatt not equal to one), call subr resto          !/*pg        351*/
!  to restore old characteristics of isobar into ws.                            !/*pg        352*/
!***resto nucleon if pi production                                              !/*pg        353*/
!                                                                               !/*pg        354*/
!           ind28=1 ==>  n+n --> n+n                                            !/*pg        355*/
      if (ind(28).eq.1) call resto                                              !/*pg        356*/
!           ind27=1 ==>  pi+n --> i                                             !/*pg        357*/
      if (ind(27).eq.1) call resto                                              !/*pg        358*/
!           ind29=1 ==>  pi+n --> pi+n   <or> ka+n --> ka+n                     !/*pg        359*/
      if (ind(29).ge.1) call resto                                              !/*pg        360*/
      if (ind(15).eq.1) call resto                                              !/*pg        361*/
      if (ind(14).ge.1) call resto                                              !/*pg        362*/
      if ((ind(13).eq.1).and.(fscat.ne.dp1)) call resto                         !/*pg        363*/
      if (icsb(3).ne.0) go to 1050                                              !/*pg        364*/
      icsb(3)=icsb(3)+1                                                         !/*pg        365*/
      go to 1050                                                                !/*pg        366*/
!                                                                               !/*pg        367*/
!     check pauli-principle for nucleons in projectile nucleus                  !/*pg        368*/
!                                                                               !/*pg        369*/
  230 iws3=ws(3)                                                                !/*pg        370*/
      if (ws(3).gt.dp2.or.ws(3).lt.dp0) go to 260                               !/*pg        371*/
      if (istept.eq.0) go to 260                                                !/*pg        372*/
!           we have a nucleon inside the potential                              !/*pg        373*/
!                                                                               !/*pg        374*/
!      logic to check pauli principle with wigner function                      !/*pg        375*/
!                                                                               !/*pg        376*/
      if (iden.lt.3) go to 240                                                  !/*pg        377*/
      if (ipauli.eq.0.or.csa(3).le.dp2) go to 260                               !/*pg        378*/
      pf2=paul2**2-ws(4)**2                                                     !/*pg        379*/
      pf2=max(pf2,dp0)                                                          !/*pg        380*/
      pf=sqrt(pf2)                                                              !/*pg        381*/
      call setwig (pfr(iws3,istept),pfw(iws3,istept),pf,r1,rhoq)                !/*pg        382*/
      rand=rang()                                                               !/*pg        383*/
      if (rand.gt.r1) go to 260                                                 !/*pg        384*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        385*/
!           check if partially blocked                                          !/*pg        386*/
      rat=sel(1)/csa(4)                                                         !/*pg        387*/
      if (iws3.eq.2) rat=sel(2)/(csa(3)-csa(4))                                 !/*pg        388*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        389*/
!           count no. of partially unblocked events                             !/*pg        390*/
!           (particle in target = type 1)                                       !/*pg        391*/
      go to 250                                                                 !/*pg        392*/
!                                                                               !/*pg        393*/
  240 continue                                                                  !/*pg        394*/
      rand=rang()                                                               !/*pg        395*/
      if (ipauli.eq.0.or.csa(3).le.dp2) go to 260                               !/*pg        396*/
      if (paul2.ge.(ws(4)+ef(iws3,istept))) go to 260                           !/*pg        397*/
!           check if fully blocked                                              !/*pg        398*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        399*/
!           check if partially blocked                                          !/*pg        400*/
      rat=sel(1)/csa(4)                                                         !/*pg        401*/
      if (iws3.eq.2) rat=sel(2)/(csa(3)-csa(4))                                 !/*pg        402*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        403*/
!           count no. of partially unblocked events                             !/*pg        404*/
!           (particle in target = type 1)                                       !/*pg        405*/
  250 ipl=1                                                                     !/*pg        406*/
      nypl(1)=nypl(1)+1                                                         !/*pg        407*/
      if (ind(32).eq.2) nyplc(1)=nyplc(1)+1                                     !/*pg        408*/
!                                                                               !/*pg        409*/
  260 call cmb                                                                  !/*pg        410*/
      call cmc (cm(7),cm(8),cm(9),cm(10))                                       !/*pg        411*/
      e1=cm2(3)                                                                 !/*pg        412*/
      p1(2)=cm2(4)/cm2(2)                                                       !/*pg        413*/
      p1(3)=cm2(5)/cm2(2)                                                       !/*pg        414*/
      p1(4)=cm2(6)/cm2(2)                                                       !/*pg        415*/
      e1mws=e1*e1-ws(4)*ws(4)                                                   !/*pg        416*/
      if (e1mws.lt.dp0) then                                                    !/*pg        417*/
        kcan=1                                                                  !/*pg        418*/
        lkcan='gamma 4'                                                         !/*pg        419*/
        go to 1050                                                              !/*pg        420*/
      endif                                                                     !/*pg        421*/
      p1(1)=sqrt(e1mws)                                                         !/*pg        422*/
!            transform into proj. frame when interaction is there               !/*pg        423*/
      call plt (p1,e1,pt1,et1,bet,gama)                                         !/*pg        424*/
      if (istepp.eq.0.or.ws(3).gt.dp2) go to 290                                !/*pg        425*/
!                                                                               !/*pg        426*/
!          nucleon is now in proj. potential                                    !/*pg        427*/
!      logic to check pauli principle with wigner function                      !/*pg        428*/
!                                                                               !/*pg        429*/
      if (iden.lt.3) go to 270                                                  !/*pg        430*/
      if (ipauli.eq.0.or.proj(3).le.dp2) go to 290                              !/*pg        431*/
      pf=sqrt(et1**2-ws(4)**2)                                                  !/*pg        432*/
      call setwig (ppfr(iws3,istepp),ppfw(iws3,istepp),pf,r1,rhoq)              !/*pg        433*/
      rand=rang()                                                               !/*pg        434*/
      if (rand.gt.r1) go to 290                                                 !/*pg        435*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        436*/
!           check if partially blocked                                          !/*pg        437*/
      rat=(spr(1)+spr(3))/proj(4)                                               !/*pg        438*/
      if (iws3.eq.2) rat=(spr(2)+spr(4))/(proj(3)-proj(4))                      !/*pg        439*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        440*/
!           count no. of partially unblocked events                             !/*pg        441*/
!           (particle in target = type 1)                                       !/*pg        442*/
      go to 280                                                                 !/*pg        443*/
!                                                                               !/*pg        444*/
  270 continue                                                                  !/*pg        445*/
      rand=rang()                                                               !/*pg        446*/
      if (ipauli.eq.0.or.proj(3).le.dp2) go to 290                              !/*pg        447*/
      if (et1.ge.(ws(4)+pef(iws3,istepp))) go to 290                            !/*pg        448*/
!          check if blocking will occur                                         !/*pg        449*/
      if (ws(2).le.dp5.and.ws31.gt.dp0.and.ind(28).eq.1) go to 160              !/*pg        450*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        451*/
!           check if partially blocked                                          !/*pg        452*/
      rat=(spr(1)+spr(3))/proj(4)                                               !/*pg        453*/
      if (iws3.eq.2) rat=(spr(2)+spr(4))/(proj(3)-proj(4))                      !/*pg        454*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        455*/
!          increment no. of partially unblocked events                          !/*pg        456*/
!          (particle in projectile = type 2)                                    !/*pg        457*/
  280 ipl=2                                                                     !/*pg        458*/
      nypl(2)=nypl(2)+1                                                         !/*pg        459*/
      if (ind(32).eq.2) nyplc(2)=nyplc(2)+1                                     !/*pg        460*/
!                                                                               !/*pg        461*/
  290 call cmc (cm(11),cm(12),cm(13),cm(14))                                    !/*pg        462*/
      e2=cm2(3)                                                                 !/*pg        463*/
      p2(2)=cm2(4)/cm2(2)                                                       !/*pg        464*/
      p2(3)=cm2(5)/cm2(2)                                                       !/*pg        465*/
      p2(4)=cm2(6)/cm2(2)                                                       !/*pg        466*/
!                                                                               !/*pg        467*/
      e2mwsp=e2*e2-wsp(4)**2                                                    !/*pg        468*/
      if (e2mwsp.ge.dp0) go to 300                                              !/*pg        469*/
!     write(6,9341) e2,wsp(4),e2mwsp,ws(1)                                      !/*pg        470*/
!9341 format(" neg sqrt in gamma; e2,wsp4,e2smwsp,ws1 = ",3e15.5,f8.1)          !/*pg        471*/
!                                                                               !/*pg        472*/
!          fix added for rare cases when sub-threshold decays are not           !/*pg        473*/
!          properly handled in subr pih ( 11/jan/82, mrc)                       !/*pg        474*/
      ind(18)=1                                                                 !/*pg        475*/
      go to 160                                                                 !/*pg        476*/
!                                                                               !/*pg        477*/
  300 p2(1)=sqrt(e2mwsp)                                                        !/*pg        478*/
      iwsp3=wsp(3)                                                              !/*pg        479*/
      call plt (p2,e2,pt2,et2,bet,gama)                                         !/*pg        480*/
      if (istepp.eq.0.or.wsp(3).gt.dp2) go to 330                               !/*pg        481*/
!          nucleon is now in proj. potential                                    !/*pg        482*/
!      logic to check pauli principle with wigner function                      !/*pg        483*/
!                                                                               !/*pg        484*/
      if (iden.lt.3) go to 310                                                  !/*pg        485*/
      if (ipauli.eq.0.or.proj(3).le.dp2) go to 330                              !/*pg        486*/
      pf=sqrt(et2**2-wsp(4)**2)                                                 !/*pg        487*/
      call setwig (ppfr(iwsp3,istepp),ppfw(iwsp3,istepp),pf,r1,rhoq)            !/*pg        488*/
      rand=rang()                                                               !/*pg        489*/
      if (rand.gt.r1) go to 330                                                 !/*pg        490*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        491*/
!           check if partially blocked                                          !/*pg        492*/
      rat=(spr(1)+spr(3))/proj(4)                                               !/*pg        493*/
      if (iwsp3.eq.2) rat=(spr(2)+spr(4))/(proj(3)-proj(4))                     !/*pg        494*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        495*/
!           count no. of partially unblocked events                             !/*pg        496*/
!           (particle in target = type 1)                                       !/*pg        497*/
      go to 320                                                                 !/*pg        498*/
!                                                                               !/*pg        499*/
  310 continue                                                                  !/*pg        500*/
      rand=rang()                                                               !/*pg        501*/
      if (ipauli.eq.0.or.proj(3).le.dp2) go to 330                              !/*pg        502*/
      if (et2.ge.(wsp(4)+pef(iwsp3,istepp))) go to 330                          !/*pg        503*/
      if (ipauli.eq.2.or.ipl.gt.0) go to 160                                    !/*pg        504*/
      rat=(spr(1)+spr(3))/proj(4)                                               !/*pg        505*/
      if (iwsp3.eq.2) rat=(spr(2)+spr(4))/(proj(3)-proj(4))                     !/*pg        506*/
      if (rand.le.rat.and.ipauli.eq.1) go to 160                                !/*pg        507*/
!          increment no. of partially unblocked events                          !/*pg        508*/
!          (partner in projectile = type 4)                                     !/*pg        509*/
  320 ipl=4                                                                     !/*pg        510*/
      nypl(4)=nypl(4)+1                                                         !/*pg        511*/
      if (ind(32).eq.2) nyplc(4)=nyplc(4)+1                                     !/*pg        512*/
!                                                                               !/*pg        513*/
  330 if (iext.eq.0.or.iexq.gt.0) go to 380                                     !/*pg        514*/
      if (iext.eq.2) go to 340                                                  !/*pg        515*/
      if ((et1-ws(4)).le.(et2-wsp(4))) go to 380                                !/*pg        516*/
      go to 350                                                                 !/*pg        517*/
  340 if ((et1-ws(4)).gt.(et2-wsp(4))) go to 380                                !/*pg        518*/
  350 iexc=-iexc                                                                !/*pg        519*/
      if (iexc.gt.0) go to 380                                                  !/*pg        520*/
      if (kext.lt.2) go to 370                                                  !/*pg        521*/
      do 360 ic=1,15                                                            !/*pg        522*/
  360 cm(ic)=tecm(ic)                                                           !/*pg        523*/
      ws(24)=tws24                                                              !/*pg        524*/
      wsp(24)=twsp24                                                            !/*pg        525*/
  370 iexq=1                                                                    !/*pg        526*/
!-----------------------------------------------------------------------        !/*pg        527*/
! Should IPL be reset for another pass?                                         !/*pg        528*/
      ipl=0                                                                     !/*pg        529*/
!-----------------------------------------------------------------------        !/*pg        530*/
      if (kext.eq.0) go to 90                                                   !/*pg        531*/
      ws(31)=tws31                                                              !/*pg        532*/
      wsp(31)=twsp31                                                            !/*pg        533*/
!---------------isertion in wrong place in original from YY (12/86)             !/*pg        534*/
      kext=0                                                                    !/*pg        535*/
      go to 130                                                                 !/*pg        536*/
  380 if (ind(15).eq.0) go to 390                                               !/*pg        537*/
!           if isobar decay, set ws(2) to 5.whatever                            !/*pg        538*/
      j2=wws(2)/dp5+1.d-03                                                      !/*pg        539*/
      wsp(2)=dp5+abs(wws(2)-j2*dp5)                                             !/*pg        540*/
      ws(2)=wsp(2)                                                              !/*pg        541*/
      go to 420                                                                 !/*pg        542*/
  390 fwsp2=dp0                                                                 !/*pg        543*/
      if (ind(32).ne.2) go to 400                                               !/*pg        544*/
      j2=wsp(2)/dp5+1.d-03                                                      !/*pg        545*/
      fwsp2=wsp(2)-dp5*j2                                                       !/*pg        546*/
  400 if (ws(31).eq.tws31) go to 410                                            !/*pg        547*/
      j2=ws(2)/dp5+1.d-03                                                       !/*pg        548*/
      aws2=j2*dp5                                                               !/*pg        549*/
      fws2=ws(2)-aws2                                                           !/*pg        550*/
      ws(2)=aws2+fwsp2+0.1d0                                                    !/*pg        551*/
      wsp(2)=fws2+5.1d0                                                         !/*pg        552*/
      go to 420                                                                 !/*pg        553*/
  410 ws(2)=ws(2)+0.1d0                                                         !/*pg        554*/
      wsp(2)=fwsp2+5.1d0                                                        !/*pg        555*/
  420 ws(5)=e1                                                                  !/*pg        556*/
      ws(6)=p1(2)                                                               !/*pg        557*/
      ws(7)=p1(3)                                                               !/*pg        558*/
      ws(8)=p1(4)                                                               !/*pg        559*/
      wsp(5)=e2                                                                 !/*pg        560*/
      wsp(6)=p2(2)                                                              !/*pg        561*/
      wsp(7)=p2(3)                                                              !/*pg        562*/
      wsp(8)=p2(4)                                                              !/*pg        563*/
! for allowed interaction, ws(15), time of collision is calculated.             !/*pg        564*/
!                                                                               !/*pg        565*/
  430 ws(15)=ws(19)-ws(16)                                                      !/*pg        566*/
      ind(22)=0                                                                 !/*pg        567*/
      ind(24)=0                                                                 !/*pg        568*/
      ind(25)=0                                                                 !/*pg        569*/
      ind(3)=0                                                                  !/*pg        570*/
!                                                                               !/*pg        571*/
!    if isobar scattering(ind13=1), increment its scatter-count                 !/*pg        572*/
!    ditto isobar capture (ind14=1)                                             !/*pg        573*/
!                                                                               !/*pg        574*/
      if (ind(13).eq.1) ws(30)=ws(30)+dp1                                       !/*pg        575*/
      if (ind(14).eq.1) ws33=ws33+dp1                                           !/*pg        576*/
      if (ind(14).eq.2) ws34=ws34+dp1                                           !/*pg        577*/
!                                                                               !/*pg        578*/
!      store partner in pi production or ap annihilation                        !/*pg        579*/
!      ind28=1 if n+n->n* + pi, ind28=2 if p(n)bar annihilation                 !/*pg        580*/
!                                                                               !/*pg        581*/
      if (ind(28).ge.1) go to 460                                               !/*pg        582*/
!                                                                               !/*pg        583*/
!       store partner in pi-production                                          !/*pg        584*/
!          ind27=1 if pi+n-> n* + pi                                            !/*pg        585*/
!                                                                               !/*pg        586*/
      if (ind(27).eq.1) go to 460                                               !/*pg        587*/
!                                                                               !/*pg        588*/
!       store partner in chg.exchange                                           !/*pg        589*/
!          ind29=1 when pi1+n1 -> pi2+n2; ind29=2 when pbar+p->nbar+n           !/*pg        590*/
!                                                                               !/*pg        591*/
      if (ind(29).ge.1) go to 460                                               !/*pg        592*/
!                                                                               !/*pg        593*/
!   ind13=1 is isobar scattering, turned on in gamma, off in selpt              !/*pg        594*/
!                                                                               !/*pg        595*/
      if (ind(13).eq.1) go to 450                                               !/*pg        596*/
!                                                                               !/*pg        597*/
!   ind15=1 is isobar decay, turned on in gamma, off in selpt                   !/*pg        598*/
!                                                                               !/*pg        599*/
      if (ind(15).ne.1) go to 440                                               !/*pg        600*/
      call wwsst (ipl)                                                          !/*pg        601*/
      go to 500                                                                 !/*pg        602*/
!                                                                               !/*pg        603*/
!     ind14=1 is isobar capture, turned on in gamma, off in selpt               !/*pg        604*/
!                                                                               !/*pg        605*/
  440 if (ind(14).lt.1) go to 500                                               !/*pg        606*/
      go to 460                                                                 !/*pg        607*/
!                                                                               !/*pg        608*/
! if isobar ch.ex. scat. (isobar changes identity (ind26=1)) collision          !/*pg        609*/
!  occurs, then go to statement 460 which stores into next free space           !/*pg        610*/
!  in st the original collision partner.  then all the characteristics          !/*pg        611*/
!  of the original isobar is stored in next free space in st by                 !/*pg        612*/
!       subr wwsst so that the new particle in ws can have the space            !/*pg        613*/
!  of the original isobar. this switching of particle in st is also             !/*pg        614*/
!  done for isobar decay and capture.                                           !/*pg        615*/
!                                                                               !/*pg        616*/
  450 if (ind(26)-1) 490,460,490                                                !/*pg        617*/
!        this is an interaction where isobar changes identity                   !/*pg        618*/
  460 if (ind(32).lt.2) go to 470                                               !/*pg        619*/
!        this is a cas-cas interaction                                          !/*pg        620*/
      i7=icsb(17)*i81                                                           !/*pg        621*/
      ig=st(i7+2)/dp5+1.d-03                                                    !/*pg        622*/
      igf=ig*5+16+i7                                                            !/*pg        623*/
      st(i7+1)=-(ws(1)+0.1d0)                                                   !/*pg        624*/
      st(i7+2)=st(i7+2)+dp5                                                     !/*pg        625*/
      st(igf+1)=ws(15)                                                          !/*pg        626*/
      st(i7+10)=ws(10)                                                          !/*pg        627*/
      st(igf+2)=ws(10)                                                          !/*pg        628*/
      st(i7+11)=ws(11)                                                          !/*pg        629*/
      st(igf+3)=ws(11)                                                          !/*pg        630*/
      st(i7+12)=ws(12)                                                          !/*pg        631*/
      st(igf+4)=ws(12)                                                          !/*pg        632*/
      st(igf+5)=wwsp(5)                                                         !/*pg        633*/
      if (ind(28).ge.2) go to 480                                               !/*pg        634*/
!        this event is not a pbar or nbar annihilation                          !/*pg        635*/
      if (icsb(3).le.0) icsb(3)=1                                               !/*pg        636*/
      icsb(3)=icsb(3)+1                                                         !/*pg        637*/
      if (icsb(17).lt.icsb(7)) icsb(2)=icsb(2)+1                                !/*pg        638*/
      go to 480                                                                 !/*pg        639*/
!        interaction in target or projectile                                    !/*pg        640*/
  470 icsb(15)=icsb(15)+1                                                       !/*pg        641*/
      if (icsb(15).ge.i391) go to 1860                                          !/*pg        642*/
      icsb8=icsb(15)*i81                                                        !/*pg        643*/
      st(icsb8+1)=-(ws(1)+0.1d0)                                                !/*pg        644*/
      st(icsb8+2)=dp5                                                           !/*pg        645*/
      st(icsb8+3)=wwsp(3)                                                       !/*pg        646*/
      st(icsb8+4)=wwsp(4)                                                       !/*pg        647*/
      st(icsb8+5)=wwsp(5)                                                       !/*pg        648*/
      st(icsb8+6)=dp0                                                           !/*pg        649*/
      st(icsb8+7)=dp0                                                           !/*pg        650*/
      st(icsb8+8)=dp0                                                           !/*pg        651*/
      st(icsb8+9)=wwsp(9)                                                       !/*pg        652*/
      st(icsb8+10)=ws(10)                                                       !/*pg        653*/
      st(icsb8+18)=ws(10)                                                       !/*pg        654*/
      st(icsb8+11)=ws(11)                                                       !/*pg        655*/
      st(icsb8+19)=ws(11)                                                       !/*pg        656*/
      st(icsb8+12)=ws(12)                                                       !/*pg        657*/
      st(icsb8+20)=ws(12)                                                       !/*pg        658*/
      st(icsb8+17)=ws(15)                                                       !/*pg        659*/
      st(icsb8+13)=ws(19)-ws(16)                                                !/*pg        660*/
      st(icsb8+14)=ws(14)                                                       !/*pg        661*/
      st(icsb8+15)=ws(25)                                                       !/*pg        662*/
      st(icsb8+16)=wwsp(24)                                                     !/*pg        663*/
      st(icsb8+21)=dp0                                                          !/*pg        664*/
      st(icsb8+i81)=wwsp(31)                                                    !/*pg        665*/
  480 call wwsst (ipl)                                                          !/*pg        666*/
      go to 500                                                                 !/*pg        667*/
!                                                                               !/*pg        668*/
! for non-exchange isobar scattering wsp(9) has the fermi momentum              !/*pg        669*/
!  of the partner.                                                              !/*pg        670*/
!                                                                               !/*pg        671*/
  490 wsp(9)=wwsp(9)                                                            !/*pg        672*/
!                                                                               !/*pg        673*/
! icsb(1) is the number of particles to be considered in the next time          !/*pg        674*/
!  interval.                                                                    !/*pg        675*/
! icsb(2) is the number of particles still to be considered in the              !/*pg        676*/
!  current time interval.                                                       !/*pg        677*/
! icsb(3) is the number of particles to be followed in stp.                     !/*pg        678*/
!                                                                               !/*pg        679*/
  500 if (ind(28).lt.2) go to 510                                               !/*pg        680*/
!                              annihilation                                     !/*pg        681*/
      imult=mult-1                                                              !/*pg        682*/
      if (ind(32).eq.2) imult=imult-1                                           !/*pg        683*/
      if (icsb(3).le.0) icsb(3)=1                                               !/*pg        684*/
      icsb(3)=icsb(3)+imult                                                     !/*pg        685*/
      icsb(1)=icsb(1)+imult                                                     !/*pg        686*/
      icsb(2)=icsb(2)+imult                                                     !/*pg        687*/
      wsp(1)=ws(1)                                                              !/*pg        688*/
      go to 1080                                                                !/*pg        689*/
!                           not an annihilation                                 !/*pg        690*/
  510 if (ind(32).eq.2) go to 520                                               !/*pg        691*/
!                           not cas-cas int''n                                  !/*pg        692*/
      if (icsb(3).le.0) icsb(3)=1                                               !/*pg        693*/
!                        increment counts of particles to be                    !/*pg        694*/
!                        considered, because of decay                           !/*pg        695*/
      icsb(3)=icsb(3)+1                                                         !/*pg        696*/
      icsb(1)=icsb(1)+1                                                         !/*pg        697*/
      icsb(2)=icsb(2)+1                                                         !/*pg        698*/
!                                                                               !/*pg        699*/
! the following  statement only keep track of the type of struck                !/*pg        700*/
!  particle.  for decay there is no struck particle.  for scattering            !/*pg        701*/
!  the struck particle is in wsp.  for capture the struck particle              !/*pg        702*/
!  is counted in wwsst.                                                         !/*pg        703*/
! ws(22) and ws(23) record the number of struck protons and neutrons.           !/*pg        704*/
!                                                                               !/*pg        705*/
  520 wsp(1)=ws(1)                                                              !/*pg        706*/
      i=wsp(3)                                                                  !/*pg        707*/
      ibt=ws(3)                                                                 !/*pg        708*/
      lbt=ws31                                                                  !/*pg        709*/
!-----------------------------------------------------------------------        !/*pg        710*/
!    To clear up an inconsistency in the use of "500."                          !/*pg        711*/
      if (lbt.eq.500) lbt=-1                                                    !/*pg        712*/
!-----------------------------------------------------------------------        !/*pg        713*/
      t0=sqrt(wsp(5)**2-wsp(4)**2)                                              !/*pg        714*/
      t1=wsp(6)*t0                                                              !/*pg        715*/
      t2=wsp(7)*t0                                                              !/*pg        716*/
      t3=wsp(8)*t0                                                              !/*pg        717*/
      lin=ws(2)+1.d-2                                                           !/*pg        718*/
!***increment no.holes in pi production                                         !/*pg        719*/
      if (ind(28).eq.1) go to 530                                               !/*pg        720*/
      if (ind(27).eq.1) go to 530                                               !/*pg        721*/
      if (ind(29).ge.1) go to 530                                               !/*pg        722*/
      if (ind(13).ne.1) go to 540                                               !/*pg        723*/
      if (fscat.eq.dp1) go to 550                                               !/*pg        724*/
  530 i=wwsp(3)                                                                 !/*pg        725*/
      ibt=wws(3)                                                                !/*pg        726*/
!-----------------------------------------------------------------------        !/*pg        727*/
! IND(36)=1 when I and IBT are defined from WWSP and WWS;                       !/*pg        728*/
! i.e., when IND(27),IND(28),or IND(29) = 1.                                    !/*pg        729*/
      ind(36)=1                                                                 !/*pg        730*/
!-----------------------------------------------------------------------        !/*pg        731*/
      t0=sqrt(wwsp(5)**2-wwsp(4)**2)                                            !/*pg        732*/
      t1=wwsp(6)*t0                                                             !/*pg        733*/
      t2=wwsp(7)*t0                                                             !/*pg        734*/
      t3=wwsp(8)*t0                                                             !/*pg        735*/
      lin=wws(2)+1.d-2                                                          !/*pg        736*/
      go to 550                                                                 !/*pg        737*/
  540 if (ind(14).ge.1) go to 880                                               !/*pg        738*/
      if (ind(15).eq.1) go to 880                                               !/*pg        739*/
  550 if ((ibt.gt.2).or.(ibt.lt.0)) go to 590                                   !/*pg        740*/
      if (lin.gt.5.or.lbt.le.0) go to 560                                       !/*pg        741*/
      it=5                                                                      !/*pg        742*/
      entp=etep(1)                                                              !/*pg        743*/
      go to 580                                                                 !/*pg        744*/
  560 it=1                                                                      !/*pg        745*/
      if (wsp31.ne.dp0) go to 570                                               !/*pg        746*/
      if (ws31.ne.dp0) it=4                                                     !/*pg        747*/
      entp=etep(1)                                                              !/*pg        748*/
      go to 580                                                                 !/*pg        749*/
  570 it=2                                                                      !/*pg        750*/
      if (ws31.ne.dp0) it=3                                                     !/*pg        751*/
      entp=etep(2)                                                              !/*pg        752*/
  580 monin(it)=monin(it)+1                                                     !/*pg        753*/
      monin(6)=monin(6)+1                                                       !/*pg        754*/
      if (ind(32).eq.2) monin(7)=monin(7)+1                                     !/*pg        755*/
      se(it)=se(it)+entp                                                        !/*pg        756*/
      ses(it)=ses(it)+entp*entp                                                 !/*pg        757*/
      srs1=ws(10)*ws(10)+ws(11)*ws(11)                                          !/*pg        758*/
      srs(it)=srs(it)+srs1                                                      !/*pg        759*/
      sr(it)=sr(it)+sqrt(srs1)                                                  !/*pg        760*/
!-----------------------------------------------------------------------        !/*pg        761*/
! To deal with charge exchange in N+N->N+I                                      !/*pg        762*/
! 590 if (ind(32)-1) 620,600,591                                                !/*pg        763*/
  590 continue                                                                  !/*pg        764*/
      if (ind(36).eq.1) then                                                    !/*pg        765*/
        jws3=ws(3)                                                              !/*pg        766*/
        jwsp3=wsp(3)                                                            !/*pg        767*/
      else                                                                      !/*pg        768*/
        jws3=ibt                                                                !/*pg        769*/
        jwsp3=i                                                                 !/*pg        770*/
      endif                                                                     !/*pg        771*/
      if (ind(32)-1) 720,650,600                                                !/*pg        772*/
!-----------------------------------------------------------------------        !/*pg        773*/
!                           cas-cas interactions (for a1+a2)                    !/*pg        774*/
!-----some complications due to partially allowed pauli (y.yariv 12/27/8        !/*pg        775*/
  600 if (ipl.le.0) go to 870                                                   !/*pg        776*/
!-----------------------------------------------------------------------        !/*pg        777*/
! I am worried about leaving IST undefined for IND(32)=dp2                      !/*pg        778*/
! So define IST=0 and try to prevoke an error                                   !/*pg        779*/
      ist=0                                                                     !/*pg        780*/
      istt=0                                                                    !/*pg        781*/
      if (istept.eq.0) istt=2                                                   !/*pg        782*/
      go to (610,620,630,640), ipl                                              !/*pg        783*/
  610 sel(jws3)=sel(jws3)+dp1                                                   !/*pg        784*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*pg        785*/
      go to 870                                                                 !/*pg        786*/
! 594 spr(ibt+istt)=spr(ibt+istt)+dp1                                           !/*pg        787*/
  620 continue                                                                  !/*pg        788*/
      if (istept.ne.0) go to 870                                                !/*pg        789*/
      spr(jws3+istt)=spr(jws3+istt)+dp1                                         !/*pg        790*/
      call dcnpar (pnpar,nnp,jws3,isc)                                          !/*pg        791*/
      go to 870                                                                 !/*pg        792*/
  630 sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*pg        793*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*pg        794*/
      go to 870                                                                 !/*pg        795*/
! 598 spr(i+istt)=spr(i+istt)+dp1                                               !/*pg        796*/
  640 continue                                                                  !/*pg        797*/
      if (istept.ne.0) go to 870                                                !/*pg        798*/
      spr(jwsp3+istt)=spr(jwsp3+istt)+dp1                                       !/*pg        799*/
!-----------------------------------------------------------------------        !/*pg        800*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*pg        801*/
      go to 870                                                                 !/*pg        802*/
!                           target interactions (for a1+a2)                     !/*pg        803*/
  650 ws(i+21)=ws(i+21)+dp1                                                     !/*pg        804*/
      if (ipl.eq.0) go to 700                                                   !/*pg        805*/
!-----------------------------------------------------------------------        !/*pg        806*/
      go to (660,670,680,690), ipl                                              !/*pg        807*/
  660 if (i.eq.jws3) go to 710                                                  !/*pg        808*/
      sel(jws3)=sel(jws3)+dp1                                                   !/*pg        809*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*pg        810*/
      go to 700                                                                 !/*pg        811*/
  670 continue                                                                  !/*pg        812*/
      if (istept.ne.0) go to 700                                                !/*pg        813*/
      spr(jws3)=spr(jws3)+dp1                                                   !/*pg        814*/
      call dcnpar (dnpar,nnp,jws3,isc)                                          !/*pg        815*/
      go to 700                                                                 !/*pg        816*/
  680 sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*pg        817*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*pg        818*/
      go to 700                                                                 !/*pg        819*/
  690 continue                                                                  !/*pg        820*/
      if (istept.ne.0) go to 700                                                !/*pg        821*/
      spr(jwsp3)=spr(jwsp3)+dp1                                                 !/*pg        822*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*pg        823*/
!-----                                                                          !/*pg        824*/
  700 sel(i)=sel(i)-dp1                                                         !/*pg        825*/
      nnt=nnt+1                                                                 !/*pg        826*/
      tnpar(1,nnt)=ws(10)                                                       !/*pg        827*/
      tnpar(2,nnt)=ws(11)                                                       !/*pg        828*/
      tnpar(3,nnt)=ws(12)                                                       !/*pg        829*/
      tnpar(4,nnt)=i                                                            !/*pg        830*/
  710 ws(15)=1.d+02*i+ws(15)                                                    !/*pg        831*/
      ex=ex+exth                                                                !/*pg        832*/
! take out a nucleon                                                            !/*pg        833*/
      call countv (fws3,1,1)                                                    !/*pg        834*/
      po=sqrt(gws(5)*gws(5)-ws4*ws4)                                            !/*pg        835*/
      pxh=gws(6)*po-ws(6)*p1(1)-wsp(6)*p2(1)                                    !/*pg        836*/
      pyh=gws(7)*po-ws(7)*p1(1)-wsp(7)*p2(1)                                    !/*pg        837*/
      pzh=gws(8)*po-ws(8)*p1(1)-wsp(8)*p2(1)                                    !/*pg        838*/
!       pit is target recoil momen., pir is projectile                          !/*pg        839*/
      pit(1)=pit(1)+pxh                                                         !/*pg        840*/
      pit(2)=pit(2)+pyh                                                         !/*pg        841*/
      pit(3)=pit(3)+pzh                                                         !/*pg        842*/
      pit(4)=pit(4)+ws(11)*pzh-ws(12)*pyh                                       !/*pg        843*/
      pit(5)=pit(5)+ws(12)*pxh-ws(10)*pzh                                       !/*pg        844*/
      pit(6)=pit(6)+ws(10)*pyh-ws(11)*pxh                                       !/*pg        845*/
      if (lin.le.5.and.lbt.gt.0) go to 780                                      !/*pg        846*/
      go to 870                                                                 !/*pg        847*/
!                        projectile interactions ( for a1+a2)                   !/*pg        848*/
  720 ist=0                                                                     !/*pg        849*/
      ck=spr(i)/(spr(i+2)+spr(i))                                               !/*pg        850*/
      rand=rang()                                                               !/*pg        851*/
      if (rand.gt.ck) ist=2                                                     !/*pg        852*/
      expr=expr+eprh(1)                                                         !/*pg        853*/
! take out a nucleon                                                            !/*pg        854*/
      call countv (fwsp3,2,1)                                                   !/*pg        855*/
      ws10=ws(10)-proj(5)                                                       !/*pg        856*/
      ws11=ws(11)-proj(6)                                                       !/*pg        857*/
      ws12=(ws(12)-proj(7))/gama                                                !/*pg        858*/
      pir(1)=pir(1)+eprh(2)                                                     !/*pg        859*/
      pir(2)=pir(2)+eprh(3)                                                     !/*pg        860*/
      pir(3)=pir(3)+eprh(4)                                                     !/*pg        861*/
      tp1=eprh(2)                                                               !/*pg        862*/
      tp2=eprh(3)                                                               !/*pg        863*/
      tp3=eprh(4)                                                               !/*pg        864*/
      pir(4)=pir(4)+ws11*eprh(4)-ws12*eprh(3)                                   !/*pg        865*/
      pir(5)=pir(5)+ws12*eprh(2)-ws10*eprh(4)                                   !/*pg        866*/
      pir(6)=pir(6)+ws10*eprh(3)-ws11*eprh(2)                                   !/*pg        867*/
!-----some complications due to partially allowed pauli (y.yariv 12/27/8        !/*pg        868*/
      if (ipl.eq.0) go to 770                                                   !/*pg        869*/
!-----------------------------------------------------------------------        !/*pg        870*/
      go to (730,740,750,760), ipl                                              !/*pg        871*/
  730 sel(jws3)=sel(jws3)+dp1                                                   !/*pg        872*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*pg        873*/
      go to 770                                                                 !/*pg        874*/
! This is an odd attempt to keep the count straight.                            !/*pg        875*/
! However, an IST=0 particle is assumed to have                                 !/*pg        876*/
! been counted initially in the NPMON array. If is is scattered up into         !/*pg        877*/
! the target from IST=2, then NPMON will be incremented.                        !/*pg        878*/
! 624 if(i.eq.ibt) go to 630                                                    !/*pg        879*/
!     spr(ibt+ist)=spr(ibt+ist)+dp1                                             !/*pg        880*/
  740 spr(jws3+2)=spr(jws3+2)+dp1                                               !/*pg        881*/
      call dcnpar (pnpar,nnp,jws3,isc)                                          !/*pg        882*/
      go to 770                                                                 !/*pg        883*/
  750 sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*pg        884*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*pg        885*/
      go to 770                                                                 !/*pg        886*/
  760 continue                                                                  !/*pg        887*/
      if (ind(36).eq.1) then                                                    !/*pg        888*/
        spr(jwsp3+2)=spr(jwsp3+2)+dp1                                           !/*pg        889*/
      else                                                                      !/*pg        890*/
        spr(jwsp3+ist)=spr(jwsp3+ist)+dp1                                       !/*pg        891*/
      endif                                                                     !/*pg        892*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*pg        893*/
!-----------------------------------------------------------------------        !/*pg        894*/
!-----                                                                          !/*pg        895*/
  770 spr(i+ist)=spr(i+ist)-dp1                                                 !/*pg        896*/
      pnpar(4,nnp+1)=i                                                          !/*pg        897*/
      ws(15)=1.d+03+1.d+02*i+ws(15)                                             !/*pg        898*/
      wsp(i+21)=wsp(i+21)+dp1                                                   !/*pg        899*/
      go to 810                                                                 !/*pg        900*/
!-----------------------------------------------------------------------        !/*pg        901*/
! 640 if(ipl.eq.2) go to 650                                                    !/*pg        902*/
  780 continue                                                                  !/*pg        903*/
      if (ipl.eq.2) then                                                        !/*pg        904*/
        if (ind(32).ne.0.and.istept.ne.0) then                                  !/*pg        905*/
          go to 790                                                             !/*pg        906*/
        else                                                                    !/*pg        907*/
          go to 800                                                             !/*pg        908*/
        endif                                                                   !/*pg        909*/
      endif                                                                     !/*pg        910*/
  790 continue                                                                  !/*pg        911*/
!-----------------------------------------------------------------------        !/*pg        912*/
      spr(ibt)=spr(ibt)-dp1                                                     !/*pg        913*/
      pnpar(4,nnp+1)=ibt                                                        !/*pg        914*/
  800 ws(15)=2.d+03+(ibt+i-2)*1.d+02+ws(15)                                     !/*pg        915*/
      wsp(ibt+21)=wsp(ibt+21)+dp1                                               !/*pg        916*/
      expr=expr+eh(lbt)                                                         !/*pg        917*/
! take out a nucleon                                                            !/*pg        918*/
      type=abs(jpro(lbt))                                                       !/*pg        919*/
      call countv (type,2,1)                                                    !/*pg        920*/
      ws10=ws(10)-proj(5)                                                       !/*pg        921*/
      ws11=ws(11)-proj(6)                                                       !/*pg        922*/
      ws12=(ws(12)-proj(7))/gama                                                !/*pg        923*/
      pir(1)=pir(1)+ph(1,lbt)                                                   !/*pg        924*/
      pir(2)=pir(2)+ph(2,lbt)                                                   !/*pg        925*/
      pir(3)=pir(3)+ph(3,lbt)                                                   !/*pg        926*/
      tp1=ph(1,lbt)                                                             !/*pg        927*/
      tp2=ph(2,lbt)                                                             !/*pg        928*/
      tp3=ph(3,lbt)                                                             !/*pg        929*/
      pir(4)=pir(4)+ws11*ph(3,lbt)-ws12*ph(2,lbt)                               !/*pg        930*/
      pir(5)=pir(5)+ws12*ph(1,lbt)-ws10*ph(3,lbt)                               !/*pg        931*/
      pir(6)=pir(6)+ws10*ph(2,lbt)-ws11*ph(1,lbt)                               !/*pg        932*/
  810 if (lin.le.5.and.lbt.gt.0) go to 820                                      !/*pg        933*/
      if (ipl.eq.4) go to 840                                                   !/*pg        934*/
      go to 830                                                                 !/*pg        935*/
  820 if (ipl.eq.2) go to 840                                                   !/*pg        936*/
  830 nnp=nnp+1                                                                 !/*pg        937*/
      pnpar(1,nnp)=ws(10)                                                       !/*pg        938*/
      pnpar(2,nnp)=ws(11)                                                       !/*pg        939*/
      pnpar(3,nnp)=ws(12)                                                       !/*pg        940*/
  840 if (ind(32).eq.1) go to 870                                               !/*pg        941*/
      if (ist.eq.0) go to 850                                                   !/*pg        942*/
      if (istept.eq.0) go to 870                                                !/*pg        943*/
      npmon(i)=npmon(i)+1                                                       !/*pg        944*/
      npmon(3)=npmon(3)+1                                                       !/*pg        945*/
#ifdef NPM                                                                      /*!pg        946*/
      write (io,1900) npmon,(ind(iii),iii=1,35)                                 !/*pg        947*/
#endif                                                                          /*!pg        948*/
      go to 870                                                                 !/*pg        949*/
!-----------------------------------------------------------------------        !/*pg        950*/
! 680 if (istept.gt.0) go to 690                                                !/*pg        951*/
  850 if (istept.gt.0.or.(ind(32).eq.0.and.ipl.eq.4)) go to 860                 !/*pg        952*/
!-----------------------------------------------------------------------        !/*pg        953*/
      npmon(i)=npmon(i)-1                                                       !/*pg        954*/
      npmon(3)=npmon(3)-1                                                       !/*pg        955*/
#ifdef NPM                                                                      /*!pg        956*/
      write (io,1910) npmon,(ind(iii),iii=1,35)                                 !/*pg        957*/
#endif                                                                          /*!pg        958*/
  860 if (ipl.eq.2.or.ipl.eq.4) go to 870                                       !/*pg        959*/
      nrep=nrep+1                                                               !/*pg        960*/
      jrep(nrep)=i                                                              !/*pg        961*/
  870 if (ind(27).eq.1) go to 1080                                              !/*pg        962*/
! for capture the struck particle is counted in wwsst.                          !/*pg        963*/
! cutofa(3) and cutofa(4) are cutoff energies for protons and neutrons          !/*pg        964*/
!  below which the particles are not followed.                                  !/*pg        965*/
! isobar is checked for below or above just before the collision.               !/*pg        966*/
!  nucleons are checked after collision.  pions are not checked at all.         !/*pg        967*/
!-----------------------------------------------------------------------        !/*pg        968*/
! 710 if (istept*istepp.ne.0.and.ipl.le.2) go to 910                            !/*pg        969*/
  880 if (istept*istepp.ne.0.and.ipl.le.2) then                                 !/*pg        970*/
!        if (ind(14).eq.1 .and. ipl.eq.2)  go to 760                            !/*pg        971*/
        go to 1080                                                              !/*pg        972*/
      endif                                                                     !/*pg        973*/
!-----------------------------------------------------------------------        !/*pg        974*/
      i=wsp(3)                                                                  !/*pg        975*/
      if (i.gt.2) go to 1080                                                    !/*pg        976*/
      if (istept.eq.0.or.ipl.eq.4) go to 890                                    !/*pg        977*/
      if ((cutofa(i+2)+ef(i,istept)+wsp(4)-wsp(5)).lt.0) go to 1080             !/*pg        978*/
      ex=ex+wsp(5)-wsp(4)-ef(i,istept)                                          !/*pg        979*/
! put in nucleon                                                                !/*pg        980*/
      call countv (wsp(3),1,2)                                                  !/*pg        981*/
      if (wsp(31).ne.dp0) ex=ex+csa(5)                                          !/*pg        982*/
      px=wsp(6)*p2(1)                                                           !/*pg        983*/
      py=wsp(7)*p2(1)                                                           !/*pg        984*/
      pz=wsp(8)*p2(1)                                                           !/*pg        985*/
      pit(1)=pit(1)+px                                                          !/*pg        986*/
      pit(2)=pit(2)+py                                                          !/*pg        987*/
      pit(3)=pit(3)+pz                                                          !/*pg        988*/
      pit(4)=pit(4)+wsp(11)*pz-wsp(12)*py                                       !/*pg        989*/
      pit(5)=pit(5)+wsp(12)*px-wsp(10)*pz                                       !/*pg        990*/
      pit(6)=pit(6)+wsp(10)*py-wsp(11)*px                                       !/*pg        991*/
      go to 900                                                                 !/*pg        992*/
  890 if ((cutofa(i+9)+pef(i,istepp)+wsp(4)-et2).le.0) go to 1080               !/*pg        993*/
!-----do not kill a particle "lawfully" in fermi sea (y.yariv 12/28/86)         !/*pg        994*/
!-----make from it a virgin particle.                                           !/*pg        995*/
!-----------------------------------------------------------------------        !/*pg        996*/
      if (ipl.eq.4.and.istept.ne.0) then                                        !/*pg        997*/
        if (ind(32).ne.0) then                                                  !/*pg        998*/
          wsp(31)=5.d+02                                                        !/*pg        999*/
          iwsp3=wsp(3)                                                          !/*pg       1000*/
          wsp(24)=pef(iwsp3,istepp)                                             !/*pg       1001*/
          go to 1080                                                            !/*pg       1002*/
        elseif (ist.eq.2.or.ind(36).eq.1.or.ind(15).eq.1) then                  !/*pg       1003*/
          ind34=ind(34)                                                         !/*pg       1004*/
          ind(34)=2                                                             !/*pg       1005*/
          call isedt (wsp)                                                      !/*pg       1006*/
          ind(34)=ind34                                                         !/*pg       1007*/
        endif                                                                   !/*pg       1008*/
      endif                                                                     !/*pg       1009*/
      if (ind(36).eq.1.and.ipl.eq.4.and.ist.eq.0) then                          !/*pg       1010*/
        nrep=nrep+1                                                             !/*pg       1011*/
        jrep(nrep)=wwsp(3)                                                      !/*pg       1012*/
      endif                                                                     !/*pg       1013*/
!-----------------------------------------------------------------------        !/*pg       1014*/
! statement 730 is for partner below cutoff.  store wsp directly                !/*pg       1015*/
      expr=expr+et2-wsp(4)-pef(i,istepp)                                        !/*pg       1016*/
! put in nucleon                                                                !/*pg       1017*/
      call countv (wsp(3),2,2)                                                  !/*pg       1018*/
      if (wsp(31).eq.dp0) expr=expr+csa(15)                                     !/*pg       1019*/
      ws10=ws(10)-proj(5)                                                       !/*pg       1020*/
      ws11=ws(11)-proj(6)                                                       !/*pg       1021*/
      ws12=(ws(12)-proj(7))/gama                                                !/*pg       1022*/
      pir(1)=pir(1)+pt2(1)*pt2(2)                                               !/*pg       1023*/
      pir(2)=pir(2)+pt2(1)*pt2(3)                                               !/*pg       1024*/
      pir(3)=pir(3)+pt2(1)*pt2(4)                                               !/*pg       1025*/
      pir(4)=pir(4)+pt2(1)*(ws11*pt2(4)-ws12*pt2(3))                            !/*pg       1026*/
      pir(5)=pir(5)+pt2(1)*(ws12*pt2(2)-ws10*pt2(4))                            !/*pg       1027*/
      pir(6)=pir(6)+pt2(1)*(ws10*pt2(3)-ws11*pt2(2))                            !/*pg       1028*/
! statement 780 is for partner below cutoff.  store wsp directly                !/*pg       1029*/
! into st.  icsb(15) is the number of particles stored in st.                   !/*pg       1030*/
! if wsp falls below the cutoff, it is directly stored in st.                   !/*pg       1031*/
! when wsp(1) or ws(1) is set to minus, then that particle will not             !/*pg       1032*/
!  be followed again.                                                           !/*pg       1033*/
!                                                                               !/*pg       1034*/
  900 wsp(1)=-wsp(1)                                                            !/*pg       1035*/
      icsb(1)=icsb(1)-1                                                         !/*pg       1036*/
      icsb(2)=icsb(2)-1                                                         !/*pg       1037*/
      icsb(3)=icsb(3)-1                                                         !/*pg       1038*/
      if (ind(32).ne.2) go to 910                                               !/*pg       1039*/
      if (ind(14).eq.1) go to 930                                               !/*pg       1040*/
      if (ind(26).eq.1) go to 930                                               !/*pg       1041*/
      if (ind(27).eq.1) go to 1060                                              !/*pg       1042*/
      if (ind(28).eq.1) go to 930                                               !/*pg       1043*/
      if (ind(29).ge.1) go to 930                                               !/*pg       1044*/
      if (icsb(17).lt.icsb(7)) icsb(2)=icsb(2)+1                                !/*pg       1045*/
      go to 1140                                                                !/*pg       1046*/
  910 icsb(15)=icsb(15)+1                                                       !/*pg       1047*/
      if (icsb(15).ge.i391) go to 1860                                          !/*pg       1048*/
      wsp(13)=ws(16)                                                            !/*pg       1049*/
      icsb8=icsb(15)*i81                                                        !/*pg       1050*/
      do 920 ib=1,9                                                             !/*pg       1051*/
  920 st(icsb8+ib)=wsp(ib)                                                      !/*pg       1052*/
      st(icsb8+18)=ws(10)                                                       !/*pg       1053*/
      st(icsb8+19)=ws(11)                                                       !/*pg       1054*/
      st(icsb8+20)=ws(12)                                                       !/*pg       1055*/
      st(icsb8+21)=wsp(5)                                                       !/*pg       1056*/
      st(icsb8+10)=ws(10)                                                       !/*pg       1057*/
      st(icsb8+11)=ws(11)                                                       !/*pg       1058*/
      st(icsb8+12)=ws(12)                                                       !/*pg       1059*/
      st(icsb8+13)=wsp(13)                                                      !/*pg       1060*/
      st(icsb8+14)=ws(14)                                                       !/*pg       1061*/
      st(icsb8+17)=ws(15)                                                       !/*pg       1062*/
      st(icsb8+15)=ws(25)                                                       !/*pg       1063*/
      st(icsb8+16)=wsp(24)                                                      !/*pg       1064*/
      st(icsb8+i81)=wsp(31)                                                     !/*pg       1065*/
  930 continue                                                                  !/*pg       1066*/
! isobars forced below the cutoff in i-n collisions are not checked             !/*pg       1067*/
! for being below the cutoff in gamma.change made 1/28/71                       !/*pg       1068*/
      if (ws(3).ge.dp3) go to 1030                                              !/*pg       1069*/
      l=ws(3)                                                                   !/*pg       1070*/
      if (l.lt.0) go to 1030                                                    !/*pg       1071*/
      if (istept*istepp.ne.0.and.ipl.ne.1.and.ipl.ne.2) go to 1030              !/*pg       1072*/
      if (istept.eq.0.or.ipl.eq.2) go to 940                                    !/*pg       1073*/
      if ((cutofa(l+2)+ef(l,istept)+ws(4)-ws(5)).lt.0) go to 1030               !/*pg       1074*/
      ex=ex+ws(5)-ws(4)-ef(l,istept)                                            !/*pg       1075*/
! put in nucleon                                                                !/*pg       1076*/
      call countv (ws(3),1,2)                                                   !/*pg       1077*/
      if (ws(31).ne.dp0) ex=ex+csa(5)                                           !/*pg       1078*/
      px=ws(6)*p1(1)                                                            !/*pg       1079*/
      py=ws(7)*p1(1)                                                            !/*pg       1080*/
      pz=ws(8)*p1(1)                                                            !/*pg       1081*/
      pit(1)=pit(1)+px                                                          !/*pg       1082*/
      pit(2)=pit(2)+py                                                          !/*pg       1083*/
      pit(3)=pit(3)+pz                                                          !/*pg       1084*/
      pit(4)=pit(4)+ws(11)*pz-ws(12)*py                                         !/*pg       1085*/
      pit(5)=pit(5)+ws(12)*px-ws(10)*pz                                         !/*pg       1086*/
      pit(6)=pit(6)+ws(10)*py-ws(11)*px                                         !/*pg       1087*/
      go to 950                                                                 !/*pg       1088*/
  940 if (ws(2).le.dp5.and.ws31.gt.dp0.and.ipl.gt.0) ipl=5                      !/*pg       1089*/
      if ((cutofa(l+9)+pef(l,istepp)+ws(4)-et1).le.0) go to 1030                !/*pg       1090*/
!-----do not kill a particle "lawfully" in fermi sea (y.yariv 12/28/86)         !/*pg       1091*/
!-----make from it a virgin particle.                                           !/*pg       1092*/
!-----------------------------------------------------------------------        !/*pg       1093*/
      if (ipl.eq.2.and.istept.ne.0) then                                        !/*pg       1094*/
        if (ind(32).ne.0.and.ind(14).ne.1) then                                 !/*pg       1095*/
          ws(31)=5.d+02                                                         !/*pg       1096*/
          iws3=ws(3)                                                            !/*pg       1097*/
          ws(24)=pef(iws3,istepp)                                               !/*pg       1098*/
          go to 1030                                                            !/*pg       1099*/
        else                                                                    !/*pg       1100*/
          ind34=ind(34)                                                         !/*pg       1101*/
          ind(34)=2                                                             !/*pg       1102*/
          call isedt (ws)                                                       !/*pg       1103*/
          ind(34)=ind34                                                         !/*pg       1104*/
        endif                                                                   !/*pg       1105*/
      endif                                                                     !/*pg       1106*/
      if (ipl.eq.2.and.ind(32).eq.0.and.ist.eq.0) then                          !/*pg       1107*/
        if (ind(14).eq.1) then                                                  !/*pg       1108*/
          iwsp3=wwsp(3)                                                         !/*pg       1109*/
        else                                                                    !/*pg       1110*/
          iwsp3=wsp(3)                                                          !/*pg       1111*/
        endif                                                                   !/*pg       1112*/
        nrep=nrep+1                                                             !/*pg       1113*/
        jrep(nrep)=iwsp3                                                        !/*pg       1114*/
      endif                                                                     !/*pg       1115*/
!-----------------------------------------------------------------------        !/*pg       1116*/
! statement 780 is for storing the ws particle that fell below cutoff.          !/*pg       1117*/
      expr=expr+et1-ws(4)-pef(l,istepp)                                         !/*pg       1118*/
! put in nucleon                                                                !/*pg       1119*/
      call countv (ws(3),2,2)                                                   !/*pg       1120*/
      if (ws(31).eq.dp0) expr=expr+csa(15)                                      !/*pg       1121*/
      ws10=ws(10)-proj(5)                                                       !/*pg       1122*/
      ws11=ws(11)-proj(6)                                                       !/*pg       1123*/
      ws12=(ws(12)-proj(7))/gama                                                !/*pg       1124*/
      pir(1)=pir(1)+pt1(1)*pt1(2)                                               !/*pg       1125*/
      pir(2)=pir(2)+pt1(1)*pt1(3)                                               !/*pg       1126*/
      pir(3)=pir(3)+pt1(1)*pt1(4)                                               !/*pg       1127*/
      pir(4)=pir(4)+pt1(1)*(ws11*pt1(4)-ws12*pt1(3))                            !/*pg       1128*/
      pir(5)=pir(5)+pt1(1)*(ws12*pt1(2)-ws10*pt1(4))                            !/*pg       1129*/
      pir(6)=pir(6)+pt1(1)*(ws10*pt1(3)-ws11*pt1(2))                            !/*pg       1130*/
! statement 21 is for storing the ws particle that fell below cutoff.           !/*pg       1131*/
  950 iws2=5*int(ws(2)/dp5)                                                     !/*pg       1132*/
! change made 10/13/70 by g.d.h.card removed                                    !/*pg       1133*/
      icsb13=icsb(13)+iws2+16                                                   !/*pg       1134*/
! ind(20)=1 when particle still has to be followed  to finish off the           !/*pg       1135*/
!  time interval.                                                               !/*pg       1136*/
! ind(20)=1 when particle cannot leave the nucleus and then it is               !/*pg       1137*/
!  treated as if it fell below the cutoff.                                      !/*pg       1138*/
! if ind(20)=1 here a nucleon cannot leave the nucleus and treated as           !/*pg       1139*/
!  if it fell below the cutoff.  this is for model without refraction.          !/*pg       1140*/
!                                                                               !/*pg       1141*/
      if (ind(20).eq.1) go to 980                                               !/*pg       1142*/
      if (ind(14).lt.1) go to 960                                               !/*pg       1143*/
!                                                                               !/*pg       1144*/
!  next stmt is for below the cutoff when capture occurs.  5 have               !/*pg       1145*/
!  to be subtracted from the calculated icsb8 because the 5 items to            !/*pg       1146*/
!  be stored into st are the birth place and time and energy of this            !/*pg       1147*/
!  captured nucleon.                                                            !/*pg       1148*/
!  also for capture ws(2) is not increased by 5 because the                     !/*pg       1149*/
!  capture was its birth, not its first interaction.  this part                 !/*pg       1150*/
!  of the program is not used by pions or isobars.                              !/*pg       1151*/
!                                                                               !/*pg       1152*/
      icsb13=icsb13-5                                                           !/*pg       1153*/
  960 st(icsb13+1)=ws(15)                                                       !/*pg       1154*/
      st(icsb13+2)=ws(10)                                                       !/*pg       1155*/
      st(icsb13+3)=ws(11)                                                       !/*pg       1156*/
      st(icsb13+4)=ws(12)                                                       !/*pg       1157*/
      st(icsb13+5)=ws(5)                                                        !/*pg       1158*/
      if (ind(14).ge.1) go to 970                                               !/*pg       1159*/
      ws(2)=ws(2)+dp5                                                           !/*pg       1160*/
  970 icsb(3)=icsb(3)-1                                                         !/*pg       1161*/
  980 ws(1)=-ws(1)                                                              !/*pg       1162*/
      if (ipl.lt.5) go to 990                                                   !/*pg       1163*/
      ws(1)=-ws(1)                                                              !/*pg       1164*/
      ws(2)=ws(2)-dp5                                                           !/*pg       1165*/
  990 if (ind(34).gt.0.and.ind(1).eq.1) icsb(3)=icsb(3)-1                       !/*pg       1166*/
      icsb(1)=icsb(1)-1                                                         !/*pg       1167*/
      icsb13=icsb(13)                                                           !/*pg       1168*/
      do 1000 ib=1,14                                                           !/*pg       1169*/
 1000 st(icsb13+ib)=ws(ib)                                                      !/*pg       1170*/
      st(icsb13+15)=ws(25)                                                      !/*pg       1171*/
      st(icsb13+16)=ws(24)                                                      !/*pg       1172*/
      st(icsb13+i81)=ws(31)                                                     !/*pg       1173*/
      ind(20)=0                                                                 !/*pg       1174*/
!   for a particle killed after 12 interactions.                                !/*pg       1175*/
      if (ind(33).eq.0) go to 1020                                              !/*pg       1176*/
! if there are more interactions than the space given to a particle             !/*pg       1177*/
!  in st, then ind(19)=dp1  then start a new cascade.  the cascade              !/*pg       1178*/
!  no. will be increased by one, the total no. of cascades                      !/*pg       1179*/
!  to be calculated will remain the same.  in the output where                  !/*pg       1180*/
!  z,a,e, cascade no., random no., are printed, no cascade no. or               !/*pg       1181*/
!  ramdom no., will be printed for that unfinished cascade.                     !/*pg       1182*/
      if (ws(2)-r60) 1020,1010,1010                                             !/*pg       1183*/
 1010 jqint=i60/5-1                                                             !/*pg       1184*/
      write (io,1950) jqint,ws(1),ws(4),ws(5),ws(13),ws(14),ws(24)              !/*pg       1185*/
!     write(6,2468)istept,istepp,sel,spr,ind,icsb,ws,wsp,gws                    !/*pg       1186*/
!2468 format ("0 gamma     istept/istepp",2i5,"     sel/spr",6f5.0/             !/*pg       1187*/
!    &        " ind",40i3/" icsb",17i6/                                         !/*pg       1188*/
!    &  " ws"/4(1x,8f15.4/)," wsp"/4(1x,8f15.4/)," gws"/4(1x,8f15.4/))          !/*pg       1189*/
!     k81=(icsb(15)+1)*i81                                                      !/*pg       1190*/
!     write(6,1988)       (st(i),i=1,k81)                                       !/*pg       1191*/
!1988 format(40(1x,16f8.2/4(1x,5f8.2,1x,5f8.2,1x,5f8.2/),1x,5f8.2/))            !/*pg       1192*/
      ind(19)=1                                                                 !/*pg       1193*/
      go to 1050                                                                !/*pg       1194*/
 1020 if (icsb(3).gt.0) go to 1050                                              !/*pg       1195*/
      icsb(11)=0                                                                !/*pg       1196*/
! icsb(16) the number of particles stored into stp.                             !/*pg       1197*/
      icsb(16)=0                                                                !/*pg       1198*/
      icsb(10)=0                                                                !/*pg       1199*/
      ind(1)=0                                                                  !/*pg       1200*/
      go to 1050                                                                !/*pg       1201*/
!                                                                               !/*pg       1202*/
! statement 860 is for nucleons above the cutoff.  for decay, capture           !/*pg       1203*/
!  and exchange scattering all of ws have to be stored.  otherwise              !/*pg       1204*/
!  only time, place, and energy of interaction have to be stored.               !/*pg       1205*/
!                                                                               !/*pg       1206*/
 1030 if (ind(15).eq.1) go to 1060                                              !/*pg       1207*/
      if (ind(14).ge.1) go to 1060                                              !/*pg       1208*/
      if (ind(26).eq.1) go to 1060                                              !/*pg       1209*/
      if (ind(29).ge.1) go to 1060                                              !/*pg       1210*/
      if (ind(28).eq.1) go to 1060                                              !/*pg       1211*/
      iws2=ws(2)/5                                                              !/*pg       1212*/
      iws2=iws2*5                                                               !/*pg       1213*/
      icsb13=icsb(13)+iws2+14                                                   !/*pg       1214*/
      ic13=icsb(13)                                                             !/*pg       1215*/
      st(icsb13+4)=ws(10)                                                       !/*pg       1216*/
      st(icsb13+5)=ws(11)                                                       !/*pg       1217*/
      st(icsb13+6)=ws(12)                                                       !/*pg       1218*/
      st(icsb13+3)=ws(15)                                                       !/*pg       1219*/
      st(ic13+15)=ws(25)                                                        !/*pg       1220*/
      st(icsb13+7)=ws(5)                                                        !/*pg       1221*/
      ws(2)=ws(2)+dp5                                                           !/*pg       1222*/
      if (ws(3).lt.dp10) go to 1040                                             !/*pg       1223*/
      icsb13=icsb(13)+i81-4                                                     !/*pg       1224*/
      st(icsb13)=dp10                                                           !/*pg       1225*/
      ws(26)=dp10                                                               !/*pg       1226*/
 1040 ind(20)=1                                                                 !/*pg       1227*/
      if (ws(2)-r60) 1050,1010,1010                                             !/*pg       1228*/
!-----------------------------------------------------------------------        !/*pg       1229*/
 1050 continue                                                                  !/*pg       1230*/
!      common return point                                                      !/*pg       1231*/
      return                                                                    !/*pg       1232*/
!-----------------------------------------------------------------------        !/*pg       1233*/
 1060 icsb13=icsb(13)                                                           !/*pg       1234*/
      do 1070 ib=2,14                                                           !/*pg       1235*/
 1070 st(icsb13+ib)=ws(ib)                                                      !/*pg       1236*/
      st(icsb13+15)=ws(25)                                                      !/*pg       1237*/
      st(icsb13+16)=ws(24)                                                      !/*pg       1238*/
      st(icsb13+17)=ws(15)                                                      !/*pg       1239*/
      st(icsb13+18)=ws(10)                                                      !/*pg       1240*/
      st(icsb13+19)=ws(11)                                                      !/*pg       1241*/
      st(icsb13+20)=ws(12)                                                      !/*pg       1242*/
      st(icsb13+21)=ws(5)                                                       !/*pg       1243*/
      st(icsb13+i81)=ws(31)                                                     !/*pg       1244*/
      if (ws(3).lt.dp10) go to 1040                                             !/*pg       1245*/
      st(icsb13+i81-4)=dp10                                                     !/*pg       1246*/
      ws(26)=dp10                                                               !/*pg       1247*/
      if (ind(26).eq.1) go to 1040                                              !/*pg       1248*/
      call updeca                                                               !/*pg       1249*/
      ws(29)=dp10                                                               !/*pg       1250*/
      st(icsb13+i81-4)=ws(26)                                                   !/*pg       1251*/
      st(icsb13+i81-3)=ws(27)                                                   !/*pg       1252*/
      st(icsb13+i81-2)=ws(28)                                                   !/*pg       1253*/
      st(icsb13+i81-1)=ws(29)                                                   !/*pg       1254*/
!***above to give non-isotropic decay for inelastic isobars4/24/73              !/*pg       1255*/
      go to 1040                                                                !/*pg       1256*/
! statement 910 is for partner in wsp that stayed above the cutoff.             !/*pg       1257*/
!  then all characteristics of wsp are stored into stp so that it will          !/*pg       1258*/
!  be followed for the remaining time interval.                                 !/*pg       1259*/
!-----                                                                          !/*pg       1260*/
!       if wsp(13)=0 (e.g. forced isobar decay)                                 !/*pg       1261*/
!       set it to ws(16) [y.y.9/5/84]                                           !/*pg       1262*/
!                                                                               !/*pg       1263*/
 1080 if (wsp(13).le.dp0) wsp(13)=ws(16)                                        !/*pg       1264*/
!-----                                                                          !/*pg       1265*/
      if (ind(28).ge.2) go to 1090                                              !/*pg       1266*/
      if (ind(32).lt.2) go to 1110                                              !/*pg       1267*/
      if (ind(14).ge.1) go to 1110                                              !/*pg       1268*/
      if (ind(26).eq.1) go to 1110                                              !/*pg       1269*/
      if (ind(27).eq.1) go to 1110                                              !/*pg       1270*/
      if (ind(28).eq.1) go to 1110                                              !/*pg       1271*/
      if (ind(29).ge.1) go to 1110                                              !/*pg       1272*/
      go to 1140                                                                !/*pg       1273*/
 1090 imult=mult-1                                                              !/*pg       1274*/
      ws(15)=iwsp3*1.d+02+ws(15)                                                !/*pg       1275*/
      if (imult.eq.0) go to 1060                                                !/*pg       1276*/
      do 1100 imlt=1,imult                                                      !/*pg       1277*/
      icsb(16)=icsb(16)+1                                                       !/*pg       1278*/
      icsb8=icsb(9)*(icsb(16)-1)                                                !/*pg       1279*/
      stp(icsb8+1)=wsp(1)                                                       !/*pg       1280*/
      stp(icsb8+2)=ws(2)                                                        !/*pg       1281*/
      stp(icsb8+3)=chrg(imlt)                                                   !/*pg       1282*/
      stp(icsb8+4)=xmpr(imlt)                                                   !/*pg       1283*/
      stp(icsb8+5)=aex(imlt)                                                    !/*pg       1284*/
      stp(icsb8+6)=akx(imlt)                                                    !/*pg       1285*/
      stp(icsb8+7)=aky(imlt)                                                    !/*pg       1286*/
      stp(icsb8+8)=akz(imlt)                                                    !/*pg       1287*/
      stp(icsb8+9)=apx(imlt)                                                    !/*pg       1288*/
      stp(icsb8+10)=ws(10)                                                      !/*pg       1289*/
      stp(icsb8+11)=ws(11)                                                      !/*pg       1290*/
      stp(icsb8+12)=ws(12)                                                      !/*pg       1291*/
      stp(icsb8+13)=wsp(13)                                                     !/*pg       1292*/
      stp(icsb8+14)=ws(14)                                                      !/*pg       1293*/
      stp(icsb8+15)=ws(25)                                                      !/*pg       1294*/
      stp(icsb8+16)=vpion                                                       !/*pg       1295*/
      stp(icsb8+17)=ws(15)                                                      !/*pg       1296*/
      stp(icsb8+18)=ws(10)                                                      !/*pg       1297*/
      stp(icsb8+19)=ws(11)                                                      !/*pg       1298*/
      stp(icsb8+20)=ws(12)                                                      !/*pg       1299*/
      stp(icsb8+21)=aex(imlt)                                                   !/*pg       1300*/
 1100 stp(icsb8+22)=dp0                                                         !/*pg       1301*/
      go to 1060                                                                !/*pg       1302*/
 1110 icsb(16)=icsb(16)+1                                                       !/*pg       1303*/
      icsb8=icsb(9)*(icsb(16)-1)                                                !/*pg       1304*/
      do 1120 ib=1,9                                                            !/*pg       1305*/
 1120 stp(icsb8+ib)=wsp(ib)                                                     !/*pg       1306*/
      stp(icsb8+10)=ws(10)                                                      !/*pg       1307*/
      stp(icsb8+18)=ws(10)                                                      !/*pg       1308*/
      stp(icsb8+11)=ws(11)                                                      !/*pg       1309*/
      stp(icsb8+19)=ws(11)                                                      !/*pg       1310*/
      stp(icsb8+12)=ws(12)                                                      !/*pg       1311*/
      stp(icsb8+20)=ws(12)                                                      !/*pg       1312*/
      stp(icsb8+21)=wsp(5)                                                      !/*pg       1313*/
      stp(icsb8+13)=wsp(13)                                                     !/*pg       1314*/
      stp(icsb8+14)=ws(14)                                                      !/*pg       1315*/
      stp(icsb8+17)=ws(15)                                                      !/*pg       1316*/
      stp(icsb8+15)=ws(25)                                                      !/*pg       1317*/
      stp(icsb8+16)=wsp(24)                                                     !/*pg       1318*/
      stp(icsb8+22)=wsp(31)                                                     !/*pg       1319*/
 1130 if (ind(27).eq.1) go to 1060                                              !/*pg       1320*/
      go to 930                                                                 !/*pg       1321*/
 1140 i7=icsb(17)*i81                                                           !/*pg       1322*/
      ig=st(i7+2)/dp5+1.d-03                                                    !/*pg       1323*/
      igf=ig*5+16+i7                                                            !/*pg       1324*/
      wsp(2)=ig*dp5+wsp(2)                                                      !/*pg       1325*/
      do 1150 ib=1,12                                                           !/*pg       1326*/
 1150 st(i7+ib)=wsp(ib)                                                         !/*pg       1327*/
      st(i7+14)=ws(14)                                                          !/*pg       1328*/
      st(i7+15)=ws(25)                                                          !/*pg       1329*/
      st(i7+16)=wsp(24)                                                         !/*pg       1330*/
      st(igf+1)=ws(15)                                                          !/*pg       1331*/
      st(igf+2)=ws(10)                                                          !/*pg       1332*/
      st(igf+3)=ws(11)                                                          !/*pg       1333*/
      st(igf+4)=ws(12)                                                          !/*pg       1334*/
      st(igf+5)=wsp(5)                                                          !/*pg       1335*/
      st(i7+i81)=wsp(31)                                                        !/*pg       1336*/
      go to 1130                                                                !/*pg       1337*/
!  statement 1100 is for ap interaction.                                        !/*pg       1338*/
!  check is made whether ap scatters elastically                                !/*pg       1339*/
!  or annihilates.                                                              !/*pg       1340*/
!  if it scatters elastically it is followed like                               !/*pg       1341*/
!  nucleon. if it annihilates the resulting pion                                !/*pg       1342*/
!  multiplicity,charge and momentum distributions                               !/*pg       1343*/
!  are selected. one of the pions is loaded                                     !/*pg       1344*/
!  into ws and the rest into stp to be followed                                 !/*pg       1345*/
!  in the present time interval.                                                !/*pg       1346*/
!                                                                               !/*pg       1347*/
 1160 if (ind(28).ge.2) go to 1180                                              !/*pg       1348*/
!                                                                               !/*pg       1349*/
!  elastic scattering of ap                                                     !/*pg       1350*/
      call cma                                                                  !/*pg       1351*/
      delta=dp0                                                                 !/*pg       1352*/
      if (ind(29).lt.2) go to 1170                                              !/*pg       1353*/
!                                                                               !/*pg       1354*/
!      charge-exchange will occur, flip-flop the masses                         !/*pg       1355*/
!                                                                               !/*pg       1356*/
      delta=(vap+dp1)*(efn-efp)                                                 !/*pg       1357*/
      if (wsp(3).eq.dp2) delta=-delta                                           !/*pg       1358*/
      call save                                                                 !/*pg       1359*/
      wwsp(3)=wsp(3)                                                            !/*pg       1360*/
      wwsp(4)=wsp(4)                                                            !/*pg       1361*/
      wwsp(5)=wsp(5)                                                            !/*pg       1362*/
      wwsp(9)=wsp(9)                                                            !/*pg       1363*/
      wwsp(24)=wsp(24)                                                          !/*pg       1364*/
      wwsp(31)=wsp(31)                                                          !/*pg       1365*/
      ws(3)=-ws(3)-3.d0                                                         !/*pg       1366*/
      wsp(3)=-ws(3)                                                             !/*pg       1367*/
      iwsp3=wsp(3)                                                              !/*pg       1368*/
      ws(4)=amass(iwsp3)                                                        !/*pg       1369*/
      wsp(4)=ws(4)                                                              !/*pg       1370*/
      ws(24)=efp                                                                !/*pg       1371*/
      if (wsp(3).eq.dp2) ws(24)=efn                                             !/*pg       1372*/
      wsp(24)=ws(24)                                                            !/*pg       1373*/
      ws(24)=ws(24)*vap                                                         !/*pg       1374*/
!                                                                               !/*pg       1375*/
 1170 call pih (delta)                                                          !/*pg       1376*/
      if (ind(18).eq.1) go to 160                                               !/*pg       1377*/
!  calculate cosine of c.m.  elastic scattering angle                           !/*pg       1378*/
!  of ap-p (and tentatively ap-n).                                              !/*pg       1379*/
!  the d(sigma-el)/dt, where -t=2*pcm**2*(1-coscm)                              !/*pg       1380*/
!  (4-momentum transfer), is approximated by a*exp(a*t),                        !/*pg       1381*/
!  a = 12.94 + 39.03*exp(-2.075*plab), plab in gev/c.                           !/*pg       1382*/
!  (see phys. rev. c26,2138(1982))                                              !/*pg       1383*/
!        psel6 = cm momentum in em, pselg6 in gev/c                             !/*pg       1384*/
!        psel1 = p(beam) = p(cm) * ecm / m2                                     !/*pg       1385*/
!                                                                               !/*pg       1386*/
      psel6=sqrt(cm(10)**2-ws(4)**2)                                            !/*pg       1387*/
      pselg6=psel6*1.d-03                                                       !/*pg       1388*/
      psel1=pselg6*cm(3)/ws(4)                                                  !/*pg       1389*/
      twoa=dp2*(12.94d0+39.03d0*exp(-2.075d0*psel1))                            !/*pg       1390*/
      fbp2=twoa*pselg6*pselg6*dp2                                               !/*pg       1391*/
      fbp2=dp1-exp(-fbp2)                                                       !/*pg       1392*/
      rand=rang()                                                               !/*pg       1393*/
!      fbp2 in logrithm normalizes to unity on interval                         !/*pg       1394*/
!       t=(0,2*kcm**2), not (0,infinity)                                        !/*pg       1395*/
      ans=dp1+log(dp1-rand*fbp2)/twoa/pselg6**2                                 !/*pg       1396*/
      go to 70                                                                  !/*pg       1397*/
!                                                                               !/*pg       1398*/
!  annihilation of ap                                                           !/*pg       1399*/
!                                                                               !/*pg       1400*/
 1180 call save                                                                 !/*pg       1401*/
      wwsp(3)=wsp(3)                                                            !/*pg       1402*/
      wwsp(4)=wsp(4)                                                            !/*pg       1403*/
      wwsp(5)=wsp(5)                                                            !/*pg       1404*/
      wwsp(9)=wsp(9)                                                            !/*pg       1405*/
      wwsp(24)=wsp(24)                                                          !/*pg       1406*/
      wwsp(31)=wsp(31)                                                          !/*pg       1407*/
!  find the pion multiplicity and charge distribution                           !/*pg       1408*/
!                                                                               !/*pg       1409*/
!    following 3 lines included to define momentum                              !/*pg       1410*/
!                                                                               !/*pg       1411*/
      call cma                                                                  !/*pg       1412*/
      psel6=sqrt(cm(10)**2-ws(4)**2)                                            !/*pg       1413*/
      psel1=cm(3)*psel6/ws(4)                                                   !/*pg       1414*/
!                                                                               !/*pg       1415*/
      frack=fnkaon(psel1)                                                       !/*pg       1416*/
      rand=rang()                                                               !/*pg       1417*/
      lkaon=.false.                                                             !/*pg       1418*/
      if (rand.le.frack) lkaon=.true.                                           !/*pg       1419*/
!                                                                               !/*pg       1420*/
!       ind(28) = 3 so i can terminate pure pion cases                          !/*pg       1421*/
!       early on, and only track kaon branches, but keep cross                  !/*pg       1422*/
!       sections correctly calculated (i hope)  [mrc  2 nov 83]                 !/*pg       1423*/
!                                                                               !/*pg       1424*/
      if (lkaon) ind(28)=3                                                      !/*pg       1425*/
!                                                                               !/*pg       1426*/
!        old "kaskew" logic of ap18                                             !/*pg       1427*/
!                                                                               !/*pg       1428*/
!     if (.not.kabran) go to 970                                                !/*pg       1429*/
!     kabran = .false.                                                          !/*pg       1430*/
!     lkaon = .true.                                                            !/*pg       1431*/
!                                                                               !/*pg       1432*/
!      the loop between "pmkd" and the call to "phsp" is                        !/*pg       1433*/
!      to correct for cases where "pmkd" gets a tail of a                       !/*pg       1434*/
!      gaussian multiplicity that has more mass than total energy               !/*pg       1435*/
!         (mrc,21 oct 82)                                                       !/*pg       1436*/
!                                                                               !/*pg       1437*/
 1190 if (lkaon) call pmkd (mult,npm,wsp(3),ws(3),psel1)                        !/*pg       1438*/
      if (.not.lkaon) call pmcdg (mult,npm,wsp(3),ws(3),psel1)                  !/*pg       1439*/
!  assign charges to the pions                                                  !/*pg       1440*/
 1200 npmm=npm                                                                  !/*pg       1441*/
      npmp=npm                                                                  !/*pg       1442*/
      mullo=1                                                                   !/*pg       1443*/
      if (wsp(3)+ws(3)) 1220,1230,1210                                          !/*pg       1444*/
!                  pbar+n                                                       !/*pg       1445*/
 1210 npmp=npmp-1                                                               !/*pg       1446*/
      go to 1230                                                                !/*pg       1447*/
!                  nbar+p                                                       !/*pg       1448*/
 1220 npmm=npmm-1                                                               !/*pg       1449*/
!                  pbar+p or nbar+n                                             !/*pg       1450*/
 1230 if (.not.lkaon) go to 1280                                                !/*pg       1451*/
!                                                                               !/*pg       1452*/
!               the rare kaon-branch has occurred                               !/*pg       1453*/
!                                                                               !/*pg       1454*/
      fr=frkpkm                                                                 !/*pg       1455*/
      rand=rang()                                                               !/*pg       1456*/
      if (rand.gt.fr) go to 1240                                                !/*pg       1457*/
!                    branch is (k+,k-)                                          !/*pg       1458*/
      xmpr(1)=amass(7)                                                          !/*pg       1459*/
      xmpr(2)=amass(9)                                                          !/*pg       1460*/
      chrg(1)=7.d0                                                              !/*pg       1461*/
      chrg(2)=9.d0                                                              !/*pg       1462*/
      npmm=npmm-1                                                               !/*pg       1463*/
      npmp=npmp-1                                                               !/*pg       1464*/
      go to 1270                                                                !/*pg       1465*/
 1240 fr=fr+frkpk0                                                              !/*pg       1466*/
      if (rand.gt.fr) go to 1250                                                !/*pg       1467*/
!                    branch is (k+,k0)                                          !/*pg       1468*/
      xmpr(1)=amass(7)                                                          !/*pg       1469*/
      xmpr(2)=amass(8)                                                          !/*pg       1470*/
      chrg(1)=7.d0                                                              !/*pg       1471*/
      chrg(2)=8.d0                                                              !/*pg       1472*/
      npmp=npmp-1                                                               !/*pg       1473*/
      go to 1270                                                                !/*pg       1474*/
 1250 fr=fr+frk0k0                                                              !/*pg       1475*/
      if (rand.gt.fr) go to 1260                                                !/*pg       1476*/
!                    branch is (k0,k0)                                          !/*pg       1477*/
      xmpr(1)=amass(8)                                                          !/*pg       1478*/
      xmpr(2)=amass(8)                                                          !/*pg       1479*/
      chrg(1)=8.d0                                                              !/*pg       1480*/
      chrg(2)=8.d0                                                              !/*pg       1481*/
      go to 1270                                                                !/*pg       1482*/
!                    all that is left is (k0,k-)                                !/*pg       1483*/
 1260 xmpr(1)=amass(8)                                                          !/*pg       1484*/
      xmpr(2)=amass(9)                                                          !/*pg       1485*/
      chrg(1)=8.d0                                                              !/*pg       1486*/
      chrg(2)=9.d0                                                              !/*pg       1487*/
      npmm=npmm-1                                                               !/*pg       1488*/
!           now check that there is enough slack to                             !/*pg       1489*/
!        create pions to conserve total charge with the given                   !/*pg       1490*/
!        total multiplicity                                                     !/*pg       1491*/
!                                                                               !/*pg       1492*/
 1270 multi=mult-2                                                              !/*pg       1493*/
      if (npmm.lt.0) go to 1200                                                 !/*pg       1494*/
      if (npmp.lt.0) go to 1200                                                 !/*pg       1495*/
      mullo=3                                                                   !/*pg       1496*/
      if ((npmm+npmp).gt.multi) go to 1200                                      !/*pg       1497*/
      if (multi.gt.0) go to 1290                                                !/*pg       1498*/
      if (npmm.ne.0) go to 1200                                                 !/*pg       1499*/
      if (npmp.ne.0) go to 1200                                                 !/*pg       1500*/
      go to 1330                                                                !/*pg       1501*/
!                                                                               !/*pg       1502*/
 1280 multi=mult                                                                !/*pg       1503*/
 1290 do 1320 ipio=mullo,mult                                                   !/*pg       1504*/
      probm=real(npmm)/real(multi)                                              !/*pg       1505*/
      probp=real(npmp)/real(multi)+probm                                        !/*pg       1506*/
      multi=multi-1                                                             !/*pg       1507*/
      rand=rang()                                                               !/*pg       1508*/
      if (rand.lt.probm) go to 1300                                             !/*pg       1509*/
      if (rand.lt.probp) go to 1310                                             !/*pg       1510*/
!  the particle is pi0                                                          !/*pg       1511*/
      chrg(ipio)=dp4                                                            !/*pg       1512*/
      xmpr(ipio)=amass(4)                                                       !/*pg       1513*/
      go to 1320                                                                !/*pg       1514*/
!  the particle is pi-                                                          !/*pg       1515*/
 1300 chrg(ipio)=dp5                                                            !/*pg       1516*/
      xmpr(ipio)=amass(5)                                                       !/*pg       1517*/
      npmm=npmm-1                                                               !/*pg       1518*/
      npmm=max(npmm,0)                                                          !/*pg       1519*/
      go to 1320                                                                !/*pg       1520*/
!  the particle is pi+                                                          !/*pg       1521*/
 1310 chrg(ipio)=dp3                                                            !/*pg       1522*/
      xmpr(ipio)=amass(3)                                                       !/*pg       1523*/
      npmp=npmp-1                                                               !/*pg       1524*/
      npmp=max(npmp,0)                                                          !/*pg       1525*/
 1320 continue                                                                  !/*pg       1526*/
!                                                                               !/*pg       1527*/
!  decrease the number of nucleons in the target by 1                           !/*pg       1528*/
!                                                                               !/*pg       1529*/
 1330 iwsp3=wsp(3)                                                              !/*pg       1530*/
      npmon(iwsp3)=npmon(iwsp3)-1                                               !/*pg       1531*/
      npmon(3)=npmon(3)-1                                                       !/*pg       1532*/
#ifdef NPM                                                                      /*!pg       1533*/
      write (io,1920) npmon,(ind(iii),iii=1,35)                                 !/*pg       1534*/
#endif                                                                          /*!pg       1535*/
      ws(iwsp3+21)=ws(iwsp3+21)+dp1                                             !/*pg       1536*/
      sel(iwsp3)=sel(iwsp3)-dp1                                                 !/*pg       1537*/
      nnt=nnt+1                                                                 !/*pg       1538*/
      tnpar(1,nnt)=ws(10)                                                       !/*pg       1539*/
      tnpar(2,nnt)=ws(11)                                                       !/*pg       1540*/
      tnpar(3,nnt)=ws(12)                                                       !/*pg       1541*/
      tnpar(4,nnt)=iwsp3                                                        !/*pg       1542*/
!  assign momenta                                                               !/*pg       1543*/
      wsp(24)=ef(iwsp3,istept)                                                  !/*pg       1544*/
      coul=dp0                                                                  !/*pg       1545*/
      if (iwsp3.eq.2) coul=csa(11)                                              !/*pg       1546*/
      delta=mult*vpion-ws(24)-wsp(24)-csa(5)+coul                               !/*pg       1547*/
      ap1=ws(5)+wsp(5)                                                          !/*pg       1548*/
      ap2=ap1+delta                                                             !/*pg       1549*/
      ap3=ap2/ap1                                                               !/*pg       1550*/
      ap4=ws(4)*ws(4)                                                           !/*pg       1551*/
      ap5=ws(5)*ws(5)-ap4                                                       !/*pg       1552*/
      ap6=wsp(5)*wsp(5)-wsp(19)                                                 !/*pg       1553*/
      ap7=sqrt(ap5)                                                             !/*pg       1554*/
      ap8=sqrt(ap6)                                                             !/*pg       1555*/
      ex=ex+exth                                                                !/*pg       1556*/
! take out a nucleon                                                            !/*pg       1557*/
      call countv (fws3,1,1)                                                    !/*pg       1558*/
      pxh=-ap8*wsp(6)                                                           !/*pg       1559*/
      pyh=-ap8*wsp(7)                                                           !/*pg       1560*/
      pzh=-ap8*wsp(8)                                                           !/*pg       1561*/
      pit(1)=pit(1)+pxh                                                         !/*pg       1562*/
      pit(2)=pit(2)+pyh                                                         !/*pg       1563*/
      pit(3)=pit(3)+pzh                                                         !/*pg       1564*/
      pit(4)=pit(4)+ws(11)*pzh-ws(12)*pyh                                       !/*pg       1565*/
      pit(5)=pit(5)+ws(12)*pxh-ws(10)*pzh                                       !/*pg       1566*/
      pit(6)=pit(6)+ws(10)*pyh-ws(11)*pxh                                       !/*pg       1567*/
      ap9=dp2*ap7*ap8*wsp(15)                                                   !/*pg       1568*/
      papno=ap5+ap6+ap9                                                         !/*pg       1569*/
      aminv=sqrt(ap4+wsp(19)+dp2*ws(5)*wsp(5)-ap9)*ap3                          !/*pg       1570*/
      papnn=ap2*ap2-aminv*aminv                                                 !/*pg       1571*/
      pfact=sqrt(papnn/papno)                                                   !/*pg       1572*/
      appt(1)=(ap7*ws(6)+ap8*wsp(6))*pfact                                      !/*pg       1573*/
      appt(2)=(ap7*ws(7)+ap8*wsp(7))*pfact                                      !/*pg       1574*/
      appt(3)=(ap7*ws(8)+ap8*wsp(8))*pfact                                      !/*pg       1575*/
      appt(4)=ap2                                                               !/*pg       1576*/
      mulo=mult                                                                 !/*pg       1577*/
      call phsbst (mult,appt,xmpr,akx,aky,akz,akq)                              !/*pg       1578*/
      if (mult.eq.mulo) go to 1340                                              !/*pg       1579*/
      npmon(iwsp3)=npmon(iwsp3)+1                                               !/*pg       1580*/
      npmon(3)=npmon(3)+1                                                       !/*pg       1581*/
#ifdef NPM                                                                      /*!pg       1582*/
      write (io,1930) npmon,(ind(iii),iii=1,35)                                 !/*pg       1583*/
#endif                                                                          /*!pg       1584*/
      ws(iwsp3+21)=ws(iwsp3+21)-dp1                                             !/*pg       1585*/
      sel(iwsp3)=sel(iwsp3)+dp1                                                 !/*pg       1586*/
      nnt=nnt-1                                                                 !/*pg       1587*/
      pit(1)=pit(1)-pxh                                                         !/*pg       1588*/
      pit(2)=pit(2)-pyh                                                         !/*pg       1589*/
      pit(3)=pit(3)-pzh                                                         !/*pg       1590*/
      pit(4)=pit(4)-ws(11)*pzh-ws(12)*pyh                                       !/*pg       1591*/
      pit(5)=pit(5)-ws(12)*pxh-ws(10)*pzh                                       !/*pg       1592*/
      pit(6)=pit(6)-ws(10)*pyh-ws(11)*pxh                                       !/*pg       1593*/
      go to 1190                                                                !/*pg       1594*/
!                                                                               !/*pg       1595*/
!    continue here if sum of masses is okay (mrc,21 oct 82)                     !/*pg       1596*/
!                                                                               !/*pg       1597*/
 1340 do 1350 iep=1,mult                                                        !/*pg       1598*/
      apx2=akx(iep)**2+aky(iep)**2+akz(iep)**2                                  !/*pg       1599*/
      apxx=sqrt(apx2)                                                           !/*pg       1600*/
      aex(iep)=sqrt(apx2+xmpr(iep)**2)                                          !/*pg       1601*/
      apx(iep)=apxx                                                             !/*pg       1602*/
      akx(iep)=akx(iep)/apxx                                                    !/*pg       1603*/
      aky(iep)=aky(iep)/apxx                                                    !/*pg       1604*/
 1350 akz(iep)=akz(iep)/apxx                                                    !/*pg       1605*/
      ws(3)=chrg(mult)                                                          !/*pg       1606*/
      ws(4)=xmpr(mult)                                                          !/*pg       1607*/
      ws(5)=aex(mult)                                                           !/*pg       1608*/
      ws(6)=akx(mult)                                                           !/*pg       1609*/
      ws(7)=aky(mult)                                                           !/*pg       1610*/
      ws(8)=akz(mult)                                                           !/*pg       1611*/
      go to 430                                                                 !/*pg       1612*/
!                                                                               !/*pg       1613*/
! statement 50 starts isobar interaction.                                       !/*pg       1614*/
! for models without cutoff for isobars, it is not necessary to check           !/*pg       1615*/
!  for isobar cutoff or whether it can decay or capture.                        !/*pg       1616*/
! for models with cutoff, check if isobar is below cutoff.  this is             !/*pg       1617*/
!  done in statement 42.                                                        !/*pg       1618*/
! the following changes have been made so that an isobar below                  !/*pg       1619*/
! the cutoff can at least capture.further if it was below the                   !/*pg       1620*/
! cutoff originally there is no reason to check for it again.                   !/*pg       1621*/
! changes made 10/12/70.statements 50 to 12                                     !/*pg       1622*/
!                                                                               !/*pg       1623*/
 1360 if (ind(33).eq.0.and.ind(32).ne.2) go to 1460                             !/*pg       1624*/
      if (ind(22).eq.1) go to 1380                                              !/*pg       1625*/
!                                                                               !/*pg       1626*/
! change made 10/14/70 to allow isobars unable to escape to fall                !/*pg       1627*/
! below the cut-off irrespective of model                                       !/*pg       1628*/
!                                                                               !/*pg       1629*/
      go to (1440,1370,1370,1440,1370,1370), ind21                              !/*pg       1630*/
!                                                                               !/*pg       1631*/
! statement 35 is for isobars below cut-off.no check is made on the             !/*pg       1632*/
! decay cross section since even though it may be non-zero an isobar            !/*pg       1633*/
! may not be able to decay because the test (fcn en) checks                     !/*pg       1634*/
! the maximum k.e.a nucleon could have on decay,not the minimum.                !/*pg       1635*/
! ergo an isobar below cutoff should at least be allowed to capture.            !/*pg       1636*/
! therefore if it is an i-i type of                                             !/*pg       1637*/
!  collision, that is i++ and p, or i- and n, then cannot capture.              !/*pg       1638*/
!  then force change of identity of partner and re-select momentum.             !/*pg       1639*/
!  when isobar is below cutoff, no scattering occurs.                           !/*pg       1640*/
!  save characteristics                                                         !/*pg       1641*/
!                                                                               !/*pg       1642*/
 1370 if (ind(22).ne.1) go to 1440                                              !/*pg       1643*/
 1380 if (ind(12).ne.1) go to 1430                                              !/*pg       1644*/
      if (ws(3).ne.13.d0) go to 1390                                            !/*pg       1645*/
      if (wsp(3)-dp1) 1430,1400,1430                                            !/*pg       1646*/
 1390 if (wsp(3).ne.dp2) go to 1430                                             !/*pg       1647*/
 1400 wsp(3)=dp3-wsp(3)                                                         !/*pg       1648*/
      ll=wsp(3)+0.1d0                                                           !/*pg       1649*/
      if (sel(ll).le.dp0.or.istept.le.0) go to 1410                             !/*pg       1650*/
      wsp(4)=amass(ll)                                                          !/*pg       1651*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       1652*/
      wsp(24)=ef(ll,istept)                                                     !/*pg       1653*/
      gdc10=rang()                                                              !/*pg       1654*/
!     wsp(9)=pfr(ll,istept) *gdc10**dpth                                        !/*pg       1655*/
      call getwig (pfr(ll,istept),pfw(ll,istept),wsp(9),gdc10)                  !/*pg       1656*/
      wsp(5)=sqrt(wsp(9)*wsp(9)+wsp(4)*wsp(4))                                  !/*pg       1657*/
      exth=wsp(24)-wsp(5)+wsp(4)                                                !/*pg       1658*/
      fws3=wsp(3)                                                               !/*pg       1659*/
      ind(24)=1                                                                 !/*pg       1660*/
 1410 if ((spr(ll)+spr(ll+2)).le.dp0.or.istepp.le.0) go to 1420                 !/*pg       1661*/
      wspr(3)=wsp(3)                                                            !/*pg       1662*/
      wspr(4)=amass(ll)                                                         !/*pg       1663*/
      wspr(19)=wspr(4)*wspr(4)                                                  !/*pg       1664*/
      wspr(24)=pef(ll,istepp)                                                   !/*pg       1665*/
      gdc10=rang()                                                              !/*pg       1666*/
!     wspr(9)=ppfr(ll,istepp)*gdc10**dpth                                       !/*pg       1667*/
      call getwig (ppfr(ll,istepp),ppfw(ll,istepp),wspr(9),gdc10)               !/*pg       1668*/
      p1(1)=wspr(9)                                                             !/*pg       1669*/
      p1(2)=wsp(6)                                                              !/*pg       1670*/
      p1(3)=wsp(7)                                                              !/*pg       1671*/
      p1(4)=wsp(8)                                                              !/*pg       1672*/
      e1=sqrt(wspr(9)*wspr(9)+wspr(19))                                         !/*pg       1673*/
      call plt (p1,e1,pt1,wspr(5),voc,gama)                                     !/*pg       1674*/
      wspr(6)=pt1(2)                                                            !/*pg       1675*/
      wspr(7)=pt1(3)                                                            !/*pg       1676*/
      wspr(8)=pt1(4)                                                            !/*pg       1677*/
      wspr(9)=pt1(1)                                                            !/*pg       1678*/
      eprh(1)=wspr(24)-wspr(5)+wspr(4)                                          !/*pg       1679*/
      fwsp3=wspr(3)                                                             !/*pg       1680*/
      eprh(2)=-wspr(6)*wspr(9)                                                  !/*pg       1681*/
      eprh(3)=-wspr(7)*wspr(9)                                                  !/*pg       1682*/
      eprh(4)=-wspr(8)*wspr(9)                                                  !/*pg       1683*/
!                                                                               !/*pg       1684*/
! ind(24)=1 when isobar falls below cutoff and cannot decay or capture.         !/*pg       1685*/
!  then change partner and recalculate cross section by calling ainter.         !/*pg       1686*/
!                                                                               !/*pg       1687*/
      ind(24)=1                                                                 !/*pg       1688*/
 1420 if (ind(24).lt.1) go to 1460                                              !/*pg       1689*/
      call ainter (gdc9,gdc10)                                                  !/*pg       1690*/
!                                                                               !/*pg       1691*/
! following change made 2/2/71 .isobar no longer checked for below              !/*pg       1692*/
! cutoff in gamma                                                               !/*pg       1693*/
!                                                                               !/*pg       1694*/
 1430 aiscat=dp0                                                                !/*pg       1695*/
      aitot=aidec+aicap                                                         !/*pg       1696*/
      wwsp(3)=wsp(3)                                                            !/*pg       1697*/
      wwsp(4)=wsp(4)                                                            !/*pg       1698*/
      wwsp(5)=wsp(5)                                                            !/*pg       1699*/
      wws(3)=ws(3)                                                              !/*pg       1700*/
      wws(31)=ws(31)                                                            !/*pg       1701*/
      wws(4)=ws(4)                                                              !/*pg       1702*/
      wws(5)=ws(5)                                                              !/*pg       1703*/
      wwsp(9)=wsp(9)                                                            !/*pg       1704*/
      wwsp(24)=wsp(24)                                                          !/*pg       1705*/
      wwsp(31)=wsp(31)                                                          !/*pg       1706*/
      wws(24)=ws(24)                                                            !/*pg       1707*/
! statement 13 starts calculating probability for scattering, decay or          !/*pg       1708*/
!  capture.                                                                     !/*pg       1709*/
 1440 aiscat=aiscat/aitot                                                       !/*pg       1710*/
      aidec=aidec/aitot                                                         !/*pg       1711*/
      aicap=aicap/aitot                                                         !/*pg       1712*/
 1450 if (ind(33).eq.0.and.ind(32).ne.2) go to 1460                             !/*pg       1713*/
      gws3=sqrt(gws(5)*gws(5)-ws(4)*ws(4))                                      !/*pg       1714*/
      wsp3=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                                    !/*pg       1715*/
      if (wsp(4).eq.dp0) go to 1870                                             !/*pg       1716*/
      e=((gws(5)*wsp(5)-wsp(15)*gws3*wsp3)/wsp(4)-ws(4))                        !/*pg       1717*/
      u1=ws(4)*ws(4)+wsp(4)*wsp(4)                                              !/*pg       1718*/
      u2=dp2*(gws(5)*wsp(5)-wsp(15)*gws3*wsp3)                                  !/*pg       1719*/
      u=1.d-03*sqrt(u1+u2)                                                      !/*pg       1720*/
      gf=rang()                                                                 !/*pg       1721*/
      if (gf.lt.aiscat) go to 1490                                              !/*pg       1722*/
      if (gf.ge.(aiscat+aidec)) go to 1630                                      !/*pg       1723*/
!                                                                               !/*pg       1724*/
! statement 54 is for decay.  if ind(18)=1, not enough momentum                 !/*pg       1725*/
!  to be divided between two particles, then it is similar to                   !/*pg       1726*/
!  pauli forbidden collision, otherwise go to statement 79 because              !/*pg       1727*/
!       subr decay returns the angle between the two decay particles.           !/*pg       1728*/
!                                                                               !/*pg       1729*/
 1460 call save                                                                 !/*pg       1730*/
      call decay (ans)                                                          !/*pg       1731*/
      if (ind(18).eq.0) go to 1480                                              !/*pg       1732*/
      if (ind(33).ne.0) go to 160                                               !/*pg       1733*/
 1470 kcan=1                                                                    !/*pg       1734*/
      lkcan='gamma 1'                                                           !/*pg       1735*/
      if (mprin.gt.0) write (io,1960)                                           !/*pg       1736*/
      go to 1050                                                                !/*pg       1737*/
 1480 wsp(31)=ws(31)                                                            !/*pg       1738*/
      ws(31)=dp0                                                                !/*pg       1739*/
      go to 70                                                                  !/*pg       1740*/
! statement 51 is for scattering.                                               !/*pg       1741*/
 1490 ind(13)=1                                                                 !/*pg       1742*/
      if (u.lt.2.1d0) u=2.1d0                                                   !/*pg       1743*/
      v=ws(4)*1.d-03                                                            !/*pg       1744*/
      if (v.lt.1.11d0) v=1.11d0                                                 !/*pg       1745*/
      call cma                                                                  !/*pg       1746*/
      if (fscat.ne.dp1) go to 1510                                              !/*pg       1747*/
!                                                                               !/*pg       1748*/
! for non-exchange scattering, (fscat=1.0), find angle in c.m.                  !/*pg       1749*/
!  between isobar and nucleon and go to elas.                                   !/*pg       1750*/
!                                                                               !/*pg       1751*/
 1500 call aiangl (e,u,v,ans,ind(13),ind(14))                                   !/*pg       1752*/
      go to 70                                                                  !/*pg       1753*/
!                                                                               !/*pg       1754*/
! for possible exchange scattering, save old partner.  see zeev                 !/*pg       1755*/
!  fraenkel notes on branching ratios, which program calls fscat.               !/*pg       1756*/
!                                                                               !/*pg       1757*/
 1510 call save                                                                 !/*pg       1758*/
      wwsp(31)=wsp(31)                                                          !/*pg       1759*/
      wwsp(3)=wsp(3)                                                            !/*pg       1760*/
      wwsp(4)=wsp(4)                                                            !/*pg       1761*/
      wwsp(24)=wsp(24)                                                          !/*pg       1762*/
      wwsp(5)=wsp(5)                                                            !/*pg       1763*/
      wwsp(9)=wsp(9)                                                            !/*pg       1764*/
      wsp(9)=dp0                                                                !/*pg       1765*/
      delt1=dp0                                                                 !/*pg       1766*/
      delt2=dp0                                                                 !/*pg       1767*/
      delta=dp0                                                                 !/*pg       1768*/
      if (fscat.ne.0.33333d0) go to 1550                                        !/*pg       1769*/
      ind(26)=1                                                                 !/*pg       1770*/
      if (ws(3).ne.13.d0) go to 1530                                            !/*pg       1771*/
      ws(3)=14.d0                                                               !/*pg       1772*/
      ws(24)=efp                                                                !/*pg       1773*/
 1520 wsp(3)=dp1                                                                !/*pg       1774*/
      wsp(4)=amass(1)                                                           !/*pg       1775*/
      wsp(24)=pefp                                                              !/*pg       1776*/
      delta=pefp-pefn+csa11                                                     !/*pg       1777*/
      go to 1610                                                                !/*pg       1778*/
 1530 ws(3)=24.d0                                                               !/*pg       1779*/
      ws(24)=efn                                                                !/*pg       1780*/
 1540 wsp(3)=dp2                                                                !/*pg       1781*/
      wsp(4)=amass(2)                                                           !/*pg       1782*/
      wsp(24)=pefn                                                              !/*pg       1783*/
      delta=pefn-pefp-csa11                                                     !/*pg       1784*/
      go to 1610                                                                !/*pg       1785*/
 1550 continue                                                                  !/*pg       1786*/
      gf=rang()                                                                 !/*pg       1787*/
      if (fscat.ne.0.77778) go to 1570                                          !/*pg       1788*/
      if (gf*0.77778d0.lt.0.44444d0) go to 1620                                 !/*pg       1789*/
!                                                                               !/*pg       1790*/
! ind(26)=1 when exchange scattering occurs.  then delta, the                   !/*pg       1791*/
!  fudge factor used to conserve energy has to be used.                         !/*pg       1792*/
!                                                                               !/*pg       1793*/
      ind(26)=1                                                                 !/*pg       1794*/
      if (ws(3).ne.14.d0) go to 1560                                            !/*pg       1795*/
      ws(3)=13.d0                                                               !/*pg       1796*/
      ws(24)=efp                                                                !/*pg       1797*/
      go to 1540                                                                !/*pg       1798*/
 1560 ws(3)=25.d0                                                               !/*pg       1799*/
      ws(24)=efn                                                                !/*pg       1800*/
      go to 1520                                                                !/*pg       1801*/
 1570 if (gf*0.55555d0.ge.0.44444d0) go to 1620                                 !/*pg       1802*/
      ind(26)=1                                                                 !/*pg       1803*/
      delta=dp0                                                                 !/*pg       1804*/
      if (ws(3).ne.14.d0) go to 1580                                            !/*pg       1805*/
      ws(3)=24.d0                                                               !/*pg       1806*/
      wsp(3)=dp1                                                                !/*pg       1807*/
      wsp(4)=amass(1)                                                           !/*pg       1808*/
      wsp(24)=pefp                                                              !/*pg       1809*/
      ws(24)=efn                                                                !/*pg       1810*/
      delt1=efn-efp                                                             !/*pg       1811*/
      delt2=pefp-pefn                                                           !/*pg       1812*/
      go to 1590                                                                !/*pg       1813*/
 1580 ws(3)=14.d0                                                               !/*pg       1814*/
      wsp(3)=dp2                                                                !/*pg       1815*/
      wsp(4)=amass(2)                                                           !/*pg       1816*/
      wsp(24)=pefn                                                              !/*pg       1817*/
      ws(24)=efp                                                                !/*pg       1818*/
 1590 if (ws(31).ne.dp0) go to 1600                                             !/*pg       1819*/
      if (wsp(31).eq.dp0) go to 1620                                            !/*pg       1820*/
      call cep (delt1,delta,ws4,ws)                                             !/*pg       1821*/
      delta=delta+delt2                                                         !/*pg       1822*/
      go to 1620                                                                !/*pg       1823*/
 1600 if (wsp(31).ne.dp0) go to 1620                                            !/*pg       1824*/
      call cep (delt2,delta,wsp4,wsp)                                           !/*pg       1825*/
      delta=delta+delt1                                                         !/*pg       1826*/
      go to 1620                                                                !/*pg       1827*/
!                                                                               !/*pg       1828*/
! pih is called to divide momentum in c.m.  if o.k.  call aiangl                !/*pg       1829*/
!  to calculate angle between 2 particles.  if not o.k. this is                 !/*pg       1830*/
!  similar to having a pauli forbidden collision.                               !/*pg       1831*/
! changes made 10/12/70 by g.d.h.                                               !/*pg       1832*/
!                                                                               !/*pg       1833*/
 1610 wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       1834*/
      delt=delta                                                                !/*pg       1835*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       1836*/
 1620 call pih (delta)                                                          !/*pg       1837*/
      if (ind(18)-1) 1500,200,1500                                              !/*pg       1838*/
!                                                                               !/*pg       1839*/
! statement 1460 is for capture.  ind(14)=dp1  save old partner.  do c.m        !/*pg       1840*/
!  transformation.  if collision is ii, that is, i++ and p or                   !/*pg       1841*/
!  i- and n, capture cross section is zero and this part of program             !/*pg       1842*/
!  would not be reached.  delta is used to conserve energy if there is          !/*pg       1843*/
!  a change of type of potential.                                               !/*pg       1844*/
!                                                                               !/*pg       1845*/
 1630 call save                                                                 !/*pg       1846*/
      ind(14)=1                                                                 !/*pg       1847*/
      if (u.lt.2.1d0) u=2.1d0                                                   !/*pg       1848*/
      wwsp(3)=wsp(3)                                                            !/*pg       1849*/
      wwsp(4)=wsp(4)                                                            !/*pg       1850*/
      wwsp(5)=wsp(5)                                                            !/*pg       1851*/
      wwsp(31)=wsp(31)                                                          !/*pg       1852*/
      wwsp(24)=wsp(24)                                                          !/*pg       1853*/
      wwsp(9)=wsp(9)                                                            !/*pg       1854*/
      call cma                                                                  !/*pg       1855*/
      if (bdd.le.dp0.or.notrys.gt.5) go to 1650                                 !/*pg       1856*/
!        isobar+nucleon --> isobar+isobar                                       !/*pg       1857*/
      gf=rang()                                                                 !/*pg       1858*/
      if (gf.gt.bdd) go to 1650                                                 !/*pg       1859*/
      ecm=cm(3)                                                                 !/*pg       1860*/
      call twopin (ecm,wws(3),wwsp(3),ws(3),wsp(3),ws(4),wsp(4),efd)            !/*pg       1861*/
      if (ws(4).gt.dp0) go to 1640                                              !/*pg       1862*/
      ws(3)=wws(3)                                                              !/*pg       1863*/
      ws(4)=wws(4)                                                              !/*pg       1864*/
      wsp(3)=wwsp(3)                                                            !/*pg       1865*/
      wsp(4)=wwsp(4)                                                            !/*pg       1866*/
      go to 1650                                                                !/*pg       1867*/
 1640 ind(14)=2                                                                 !/*pg       1868*/
      ws(24)=efn                                                                !/*pg       1869*/
      if (ws(3).le.14.d0) ws(24)=efp                                            !/*pg       1870*/
      wsp(24)=pefn                                                              !/*pg       1871*/
      if (wsp(3).le.14.d0) wsp(24)=pefp                                         !/*pg       1872*/
      delta=efd*(efp-efn)                                                       !/*pg       1873*/
      if (ws(24).eq.wws(24)) delta=efd*(pefp-pefn)                              !/*pg       1874*/
      go to 1710                                                                !/*pg       1875*/
!        isobar+nucleon --> nucleon+nucleon                                     !/*pg       1876*/
 1650 if (ind(12).ne.1) go to 1670                                              !/*pg       1877*/
      if (ws(3).le.13.d0) go to 1660                                            !/*pg       1878*/
      ws(3)=dp2                                                                 !/*pg       1879*/
      wsp(3)=dp2                                                                !/*pg       1880*/
      wsp(24)=pefn                                                              !/*pg       1881*/
      delta=pefn-pefp-csa11                                                     !/*pg       1882*/
      go to 1690                                                                !/*pg       1883*/
 1660 ws(3)=dp1                                                                 !/*pg       1884*/
      wsp(3)=dp1                                                                !/*pg       1885*/
      wsp(24)=pefp                                                              !/*pg       1886*/
      delta=pefp-pefn+csa11                                                     !/*pg       1887*/
      go to 1690                                                                !/*pg       1888*/
 1670 delta=dp0                                                                 !/*pg       1889*/
      if (ws(3).eq.14.d0) go to 1680                                            !/*pg       1890*/
      ws(3)=dp2                                                                 !/*pg       1891*/
      go to 1700                                                                !/*pg       1892*/
 1680 ws(3)=dp1                                                                 !/*pg       1893*/
      go to 1700                                                                !/*pg       1894*/
 1690 i=wsp(3)                                                                  !/*pg       1895*/
      wsp(4)=amass(i)                                                           !/*pg       1896*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       1897*/
 1700 i=ws(3)                                                                   !/*pg       1898*/
      ws(4)=amass(i)                                                            !/*pg       1899*/
! pih is called to divide momentum.  if not possible, this is like              !/*pg       1900*/
!  making pauli forbidden collision.                                            !/*pg       1901*/
 1710 if (ind21.gt.3) delta=delta-vpion                                         !/*pg       1902*/
      delt=delta                                                                !/*pg       1903*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       1904*/
      call pih (delta)                                                          !/*pg       1905*/
      if (ind(18).eq.1) go to 200                                               !/*pg       1906*/
      v=wws(4)*1.d-03                                                           !/*pg       1907*/
      if (v.lt.1.11d0) v=1.11d0                                                 !/*pg       1908*/
!                                                                               !/*pg       1909*/
! aiangl is called to calculate angle between two nucleons.                     !/*pg       1910*/
!                                                                               !/*pg       1911*/
      call aiangl (e,u,v,ans,ind(13),ind(14))                                   !/*pg       1912*/
!                                                                               !/*pg       1913*/
! wsp(9) and ws(9) are set to zero, because the two capture particles           !/*pg       1914*/
!  did not come from fermi sea.  wwsp(9) has the fermi                          !/*pg       1915*/
!  momentum of collision partner.                                               !/*pg       1916*/
!                                                                               !/*pg       1917*/
      wsp(9)=dp0                                                                !/*pg       1918*/
      ws(9)=dp0                                                                 !/*pg       1919*/
      go to 70                                                                  !/*pg       1920*/
!***pion production in n-n collisions                                           !/*pg       1921*/
!                                                                               !/*pg       1922*/
 1720 call save                                                                 !/*pg       1923*/
      wwsp(3)=wsp(3)                                                            !/*pg       1924*/
      wwsp(4)=wsp(4)                                                            !/*pg       1925*/
      wwsp(5)=wsp(5)                                                            !/*pg       1926*/
      wwsp(24)=wsp(24)                                                          !/*pg       1927*/
      wwsp(31)=wsp(31)                                                          !/*pg       1928*/
      wwsp(9)=wsp(9)                                                            !/*pg       1929*/
      ecm=cm(3)                                                                 !/*pg       1930*/
      call onepin (ecm,wwsp(3),wws(3),ws(3),wsp(3),ws(4),wsp(4))                !/*pg       1931*/
      if (ws(31).ne.dp0) ws(31)=-dp1                                            !/*pg       1932*/
      delta=dp0                                                                 !/*pg       1933*/
      if (ws(3).eq.13.d0) delta=pefn-pefp-csa11                                 !/*pg       1934*/
      if (ws(3).eq.25.d0) delta=pefp-pefn+csa11                                 !/*pg       1935*/
      if (ind21.gt.3) delta=delta+vpion                                         !/*pg       1936*/
      delt=delta                                                                !/*pg       1937*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       1938*/
      i=wsp(3)                                                                  !/*pg       1939*/
      wsp(4)=amass(i)                                                           !/*pg       1940*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       1941*/
      call pih (delta)                                                          !/*pg       1942*/
      if (ind(18).eq.1) go to 220                                               !/*pg       1943*/
      call angli1 (ans)                                                         !/*pg       1944*/
      wsp(9)=dp0                                                                !/*pg       1945*/
      ws(9)=dp0                                                                 !/*pg       1946*/
      if (wsp(3).eq.dp1) wsp(24)=pefp                                           !/*pg       1947*/
      if (wsp(3).eq.dp2) wsp(24)=pefn                                           !/*pg       1948*/
      if (ws(3).le.14.d0) ws(24)=efp                                            !/*pg       1949*/
      if (ws(3).ge.24.d0) ws(24)=efn                                            !/*pg       1950*/
      go to 70                                                                  !/*pg       1951*/
!  begin high energy pi-n interactions                                          !/*pg       1952*/
 1730 call branch (tcm,i9,i10,i11,fin,finex)                                    !/*pg       1953*/
      fint=rang()                                                               !/*pg       1954*/
      if (fint.le.finex) go to 1790                                             !/*pg       1955*/
!  elastic scattering                                                           !/*pg       1956*/
      call pangle (tcm,i9,i10,i11,dp1,dp1,ans)                                  !/*pg       1957*/
      go to 70                                                                  !/*pg       1958*/
!                                                                               !/*pg       1959*/
!     get appropriate physics info for kaon+nucleon                             !/*pg       1960*/
!                                                                               !/*pg       1961*/
 1740 continue                                                                  !/*pg       1962*/
      call kranch (p10,ws(3),wsp(3),ws(4),wsp(4),finex)                         !/*pg       1963*/
      fint=rang()                                                               !/*pg       1964*/
      if (fint.le.finex) go to 1750                                             !/*pg       1965*/
!                                                                               !/*pg       1966*/
!         elastic scattering of kaon by nucleon (all the same)                  !/*pg       1967*/
!                                                                               !/*pg       1968*/
      call panglk (p10,ws(4),wsp(4),ans)                                        !/*pg       1969*/
      go to 70                                                                  !/*pg       1970*/
!                                                                               !/*pg       1971*/
!  exchange scattering                                                          !/*pg       1972*/
!        only   ka+ plus n --> k0  plus p                                       !/*pg       1973*/
!        and    ka0 plus p --> ka+ plus n                                       !/*pg       1974*/
!        are allowed to be possible [23 may 83, mrc] in "kranch"                !/*pg       1975*/
!                                                                               !/*pg       1976*/
 1750 call save                                                                 !/*pg       1977*/
      wwsp(3)=wsp(3)                                                            !/*pg       1978*/
      wwsp(4)=wsp(4)                                                            !/*pg       1979*/
      wwsp(5)=wsp(5)                                                            !/*pg       1980*/
      wwsp(24)=wsp(24)                                                          !/*pg       1981*/
      wwsp(31)=wsp(31)                                                          !/*pg       1982*/
      wwsp(9)=wsp(9)                                                            !/*pg       1983*/
      ws(31)=dp0                                                                !/*pg       1984*/
      ind(29)=1                                                                 !/*pg       1985*/
      delta=pefn-pefp-csa11                                                     !/*pg       1986*/
      if (ws(3)-8.d0) 1760,1770,70                                              !/*pg       1987*/
 1760 continue                                                                  !/*pg       1988*/
!        only   ka+ plus n --> k0  plus p                                       !/*pg       1989*/
      ws(3)=8.d0                                                                !/*pg       1990*/
      wsp(3)=dp1                                                                !/*pg       1991*/
      delta=-delta                                                              !/*pg       1992*/
      go to 1780                                                                !/*pg       1993*/
 1770 continue                                                                  !/*pg       1994*/
!        only   ka0 plus p --> ka+ plus n                                       !/*pg       1995*/
      ws(3)=7.d0                                                                !/*pg       1996*/
      wsp(3)=dp2                                                                !/*pg       1997*/
 1780 continue                                                                  !/*pg       1998*/
      i=ws(3)                                                                   !/*pg       1999*/
      j=wsp(3)                                                                  !/*pg       2000*/
      ws(4)=amass(i)                                                            !/*pg       2001*/
      wsp(4)=amass(j)                                                           !/*pg       2002*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       2003*/
      delt=delta                                                                !/*pg       2004*/
!                                                                               !/*pg       2005*/
!       cep determines change of energy in target frame,                        !/*pg       2006*/
!       if reaction occurred in the projectile                                  !/*pg       2007*/
!                                                                               !/*pg       2008*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       2009*/
!                                                                               !/*pg       2010*/
!        pih rescales the momentum-vectors to the new                           !/*pg       2011*/
!        (ws(5)+delta) total energy for consistency                             !/*pg       2012*/
!                                                                               !/*pg       2013*/
      call pih (delta)                                                          !/*pg       2014*/
      if (ind(18).eq.1) go to 220                                               !/*pg       2015*/
!                                                                               !/*pg       2016*/
!         ind18=0 if reaction is allowed by kinematics                          !/*pg       2017*/
!          in principle, "p10" should be recalculated, but                      !/*pg       2018*/
!          if the reaction was kinematically possible, then                     !/*pg       2019*/
!          p10 is not likely to have changed significantly                      !/*pg       2020*/
!                                                                               !/*pg       2021*/
      call panglk (p10,ws(4),wsp(4),ans)                                        !/*pg       2022*/
!                                                                               !/*pg       2023*/
!       store the appropriate value for the fermi-energy for                    !/*pg       2024*/
!       the partner                                                             !/*pg       2025*/
!                                                                               !/*pg       2026*/
      if (wsp(3).eq.dp1) wsp(24)=pefp                                           !/*pg       2027*/
      if (wsp(3).eq.dp2) wsp(24)=pefn                                           !/*pg       2028*/
      wsp(9)=dp0                                                                !/*pg       2029*/
      ws(9)=dp0                                                                 !/*pg       2030*/
      go to 70                                                                  !/*pg       2031*/
!                                                                               !/*pg       2032*/
!           continue with high-energy pi analysis                               !/*pg       2033*/
!                                                                               !/*pg       2034*/
 1790 continue                                                                  !/*pg       2035*/
      if (fint.le.fin) go to 1850                                               !/*pg       2036*/
!  exchange scattering                                                          !/*pg       2037*/
      call save                                                                 !/*pg       2038*/
      wwsp(3)=wsp(3)                                                            !/*pg       2039*/
      wwsp(4)=wsp(4)                                                            !/*pg       2040*/
      wwsp(5)=wsp(5)                                                            !/*pg       2041*/
      wwsp(24)=wsp(24)                                                          !/*pg       2042*/
      wwsp(31)=wsp(31)                                                          !/*pg       2043*/
      wwsp(9)=wsp(9)                                                            !/*pg       2044*/
      ws(31)=dp0                                                                !/*pg       2045*/
      ind(29)=1                                                                 !/*pg       2046*/
      delta=pefn-pefp-csa11                                                     !/*pg       2047*/
      if (ws(3)-dp4) 1800,1810,1830                                             !/*pg       2048*/
 1800 continue                                                                  !/*pg       2049*/
!  pi+ plus n to p0 plus p                                                      !/*pg       2050*/
      ws(3)=dp4                                                                 !/*pg       2051*/
      wsp(3)=dp1                                                                !/*pg       2052*/
      delta=-delta                                                              !/*pg       2053*/
      go to 1840                                                                !/*pg       2054*/
 1810 continue                                                                  !/*pg       2055*/
      if (wsp(3).eq.dp1) go to 1820                                             !/*pg       2056*/
!  p0 plus n to pi- plus p                                                      !/*pg       2057*/
      delta=-delta                                                              !/*pg       2058*/
      ws(3)=dp5                                                                 !/*pg       2059*/
      wsp(3)=dp1                                                                !/*pg       2060*/
      go to 1840                                                                !/*pg       2061*/
 1820 continue                                                                  !/*pg       2062*/
!  p0 plus p to pi+ plus n                                                      !/*pg       2063*/
      ws(3)=dp3                                                                 !/*pg       2064*/
      wsp(3)=dp2                                                                !/*pg       2065*/
      go to 1840                                                                !/*pg       2066*/
 1830 continue                                                                  !/*pg       2067*/
!  pi- plus p to p0 plus n                                                      !/*pg       2068*/
      ws(3)=dp4                                                                 !/*pg       2069*/
      wsp(3)=dp2                                                                !/*pg       2070*/
 1840 continue                                                                  !/*pg       2071*/
      i=ws(3)                                                                   !/*pg       2072*/
      j=wsp(3)                                                                  !/*pg       2073*/
      ws(4)=amass(i)                                                            !/*pg       2074*/
      wsp(4)=amass(j)                                                           !/*pg       2075*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       2076*/
      delt=delta                                                                !/*pg       2077*/
!                                                                               !/*pg       2078*/
!       cep determines change of energy in target frame,                        !/*pg       2079*/
!       if reaction occurred in the projectile                                  !/*pg       2080*/
!                                                                               !/*pg       2081*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       2082*/
!                                                                               !/*pg       2083*/
!        pih rescales the momentum-vectors to the new                           !/*pg       2084*/
!        (ws(5)+delta) total energy for consistency                             !/*pg       2085*/
!                                                                               !/*pg       2086*/
      call pih (delta)                                                          !/*pg       2087*/
      if (ind(18).eq.1) go to 220                                               !/*pg       2088*/
!                                                                               !/*pg       2089*/
!         ind18=0 if reaction is allowed by kinematics                          !/*pg       2090*/
!                                                                               !/*pg       2091*/
      call pangle (tcm,i9,i10,i11,dp1,dp2,ans)                                  !/*pg       2092*/
      if (wsp(3).eq.dp1) wsp(24)=pefp                                           !/*pg       2093*/
      if (wsp(3).eq.dp2) wsp(24)=pefn                                           !/*pg       2094*/
      wsp(9)=dp0                                                                !/*pg       2095*/
      ws(9)=dp0                                                                 !/*pg       2096*/
      go to 70                                                                  !/*pg       2097*/
 1850 continue                                                                  !/*pg       2098*/
!  pi plus n to isobar plus pi                                                  !/*pg       2099*/
      ind(27)=1                                                                 !/*pg       2100*/
      call save                                                                 !/*pg       2101*/
      wwsp(3)=wsp(3)                                                            !/*pg       2102*/
      wwsp(4)=wsp(4)                                                            !/*pg       2103*/
      wwsp(5)=wsp(5)                                                            !/*pg       2104*/
      wwsp(24)=wsp(24)                                                          !/*pg       2105*/
      wwsp(31)=wsp(31)                                                          !/*pg       2106*/
      wwsp(9)=wsp(9)                                                            !/*pg       2107*/
      delta=pefn-pefp-csa11                                                     !/*pg       2108*/
      ecm=cm(3)                                                                 !/*pg       2109*/
      call pin1pi (ecm,wws(3),wwsp(3),ws(3),wsp(3),ws(4),wsp(4),dfact)          !/*pg       2110*/
      delta=dfact*delta                                                         !/*pg       2111*/
      if (ind21.gt.3) delta=delta+vpion                                         !/*pg       2112*/
      i=wsp(3)                                                                  !/*pg       2113*/
      wsp(4)=amass(i)                                                           !/*pg       2114*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*pg       2115*/
      delt=delta                                                                !/*pg       2116*/
      if (wsp(31).ne.dp0) call cep (delt,delta,wsp4,wsp)                        !/*pg       2117*/
      call pih (delta)                                                          !/*pg       2118*/
      if (ind(18).eq.1) go to 220                                               !/*pg       2119*/
      call angli1 (ans)                                                         !/*pg       2120*/
      wsp(9)=dp0                                                                !/*pg       2121*/
      ws(9)=dp0                                                                 !/*pg       2122*/
      if (ws(3).le.14.d0) ws(24)=pefp                                           !/*pg       2123*/
      if (ws(3).ge.24.d0) ws(24)=pefn                                           !/*pg       2124*/
      wsp(24)=dp0                                                               !/*pg       2125*/
      call elas (ans)                                                           !/*pg       2126*/
      iws3=ws(3)                                                                !/*pg       2127*/
      iwsp3=wsp(3)                                                              !/*pg       2128*/
      go to 260                                                                 !/*pg       2129*/
 1860 kcan=2                                                                    !/*pg       2130*/
      lkcan='gamma 2'                                                           !/*pg       2131*/
      go to 1050                                                                !/*pg       2132*/
 1870 kcan=1                                                                    !/*pg       2133*/
      lkcan='gamma 3'                                                           !/*pg       2134*/
      go to 1050                                                                !/*pg       2135*/
!                                                                               !/*pg       2136*/
 1880 format ('gamma.322 -',3i5/35i3)                                           !/*pg       2137*/
 1890 format ('gamma.322 +',3i5/35i3)                                           !/*pg       2138*/
 1900 format ('gamma.798 +',3i5/35i3)                                           !/*pg       2139*/
 1910 format ('gamma.802 -',3i5/35i3)                                           !/*pg       2140*/
 1920 format ('gamma.1313 -',3i5/35i3)                                          !/*pg       2141*/
 1930 format ('gamma.1357 +',3i5/35i3)                                          !/*pg       2142*/
 1940 format (' isobar can not capture nor decay, 10 mev.',                     !/*pg       2143*/
     & ' added to its energy')                                                  !/*pg       2144*/
 1950 format ('0 gamma : # of interactions > ',i3,' ws1,4,5,13,14,24=',6        !/*pg       2145*/
     & f10.1)                                                                   !/*pg       2146*/
 1960 format ('0 isobar cannot capture nor decay')                              !/*pg       2147*/
      end                                                                       !/*pg       2148*/

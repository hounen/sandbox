!     $Id: isabel.F,v 1.5 2002/03/27 18:15:09 gwm Exp $                       !/*is          1*/
      subroutine isabel (dsel,proj3)                                            !/*is          2*/
! ISABEL calls   IGETD  AMFP  POE  SELPT  RLAR  DELTA  AJTEST  ISEDT            !/*is          3*/
!                SQRT  PSEL  AINTER  CASCAS  GAMMA  ISBD                        !/*is          4*/
! called by HCASC                                                               !/*is          5*/
#include <lcs/double.h>                                                         /*!is          6*/
      external isbd                                                             !/*is          7*/
      dimension aips(5), ains(5), sws(2), swsp(2)                               !/*is          8*/
#include <lcs/vegas.h>                                                          /*!is          9*/
      logical annih                                                             !/*is         10*/
#include <lcs/inout.h>                                                          /*!is         11*/
      dimension dsel(2)                                                         !/*is         12*/
      data aips /5*dp0/, ains /5*dp0/, sws /2*dp0/, swsp /2*dp0/                !/*is         13*/
!-----------------------------------------------------------------------        !/*is         14*/
      proj3=proj(3)                                                             !/*is         15*/
      nocas=igetd(1)                                                            !/*is         16*/
      if (nocas.le.1) then                                                      !/*is         17*/
        mprin=6                                                                 !/*is         18*/
      else                                                                      !/*is         19*/
        mprin=0                                                                 !/*is         20*/
      endif                                                                     !/*is         21*/
!     below coulomb barrier                                                     !/*is         22*/
      if (csa(1).eq.dp1.or.csa(1).eq.dp3) then                                  !/*is         23*/
        temp=csa(11)                                                            !/*is         24*/
        if (csa(1).ne.dp1) temp=temp+dp1                                        !/*is         25*/
        if ((csa(2)-ws(4)-temp).le.dp0) then                                    !/*is         26*/
          nprts=-1                                                              !/*is         27*/
          return                                                                !/*is         28*/
        endif                                                                   !/*is         29*/
      endif                                                                     !/*is         30*/
!-----------------------------------------------------------------------        !/*is         31*/
      hold1=bige                                                                !/*is         32*/
      hold2=bigp(3)                                                             !/*is         33*/
!       subr amfp calculates mean free path.                                    !/*is         34*/
!                                                                               !/*is         35*/
      proj(13)=-dp1                                                             !/*is         36*/
      repeat=.false.                                                            !/*is         37*/
      nrpcnt=0                                                                  !/*is         38*/
!     ---------------------------------                                         !/*is         39*/
!-----------------------------------------------------------------------        !/*is         40*/
!***** to fix up consistency with POE for low  energy pions                     !/*is         41*/
      if (csa(1).ge.dp3.and.csa(1).le.dp5) then                                 !/*is         42*/
        en=ws(5)+vpion+(csa(1)-dp4)*csa(11)                                     !/*is         43*/
      elseif (csa(1).eq.dp0) then                                               !/*is         44*/
        en=dph*(amass(1)+amass(2))/gama                                         !/*is         45*/
      else                                                                      !/*is         46*/
        en=ws(5)                                                                !/*is         47*/
      endif                                                                     !/*is         48*/
      csa(14)=dp0                                                               !/*is         49*/
      call amfp (en,den_(ieght))                                                !/*is         50*/
!-----------------------------------------------------------------------        !/*is         51*/
!     ---------------------------------                                         !/*is         52*/
   10 do 20 i=1,i32k                                                            !/*is         53*/
   20 st(i)=dp0                                                                 !/*is         54*/
      do 30 i=1,600                                                             !/*is         55*/
   30 stp(i)=dp0                                                                !/*is         56*/
!         reset annihilation flag                                               !/*is         57*/
      annih=.false.                                                             !/*is         58*/
!          reset- skewing flags                                                 !/*is         59*/
      if (kaskew.ne.0) kabran=.true.                                            !/*is         60*/
      nti=1                                                                     !/*is         61*/
      ind(21)=model-70                                                          !/*is         62*/
      kcan=0                                                                    !/*is         63*/
      lkcan='********'                                                          !/*is         64*/
      bige=hold1                                                                !/*is         65*/
      bigp(3)=hold2                                                             !/*is         66*/
      bigp(1)=dp0                                                               !/*is         67*/
      bigp(2)=dp0                                                               !/*is         68*/
!      sub poe calculates point of entry into the nucleus by the                !/*is         69*/
!  incident particle.                                                           !/*is         70*/
!                                                                               !/*is         71*/
!     ---------------------------------                                         !/*is         72*/
      call poe                                                                  !/*is         73*/
      if (ind(7).eq.1) go to 130                                                !/*is         74*/
!     ---------------------------------                                         !/*is         75*/
!                                                                               !/*is         76*/
!       subr selpt selects partner for collision and advances the               !/*is         77*/
!       particle to a new position.                                             !/*is         78*/
!                                                                               !/*is         79*/
!     ---------------------------------                                         !/*is         80*/
   40 call selpt (gdc9,gdc10)                                                   !/*is         81*/
!     ---------------------------------                                         !/*is         82*/
!                                                                               !/*is         83*/
      if (kcan.ne.0) go to 160                                                  !/*is         84*/
!                                                                               !/*is         85*/
! ind(22)=1 when either in gamma or psel isobar falls below cutoff or           !/*is         86*/
!  in rlar for model without refraction an isobar that cannot escape is         !/*is         87*/
!  treated as if it fell below cutoff.  ind(22)is only used for isobar          !/*is         88*/
!  models with cutoff energies for isobars.  when this isobar falls             !/*is         89*/
!  below the cutoff, immediately another partner is selected.  the              !/*is         90*/
!  probability for isobar scattering is set to zero, and the isobar can         !/*is         91*/
!  choose to decay or be captured.  if decay is kinematically not               !/*is         92*/
!  allowed, then check if capture is charge allowed(for example, i++            !/*is         93*/
!  cannot capture on a proton).  if not charge allowed, identity of             !/*is         94*/
!  partner is forced to be the allowed kind and reselect its momentum.          !/*is         95*/
!  then ainter is called again to recalculate probability for capture.          !/*is         96*/
!                                                                               !/*is         97*/
      if (ind(22)-1) 50,190,50                                                  !/*is         98*/
!                                                                               !/*is         99*/
!  if the particle is leaving the system (ind(34)=3) or a "virgin"              !/*is        100*/
!  projectile particle is either leaving the target volume or                   !/*is        101*/
!  can not find a "virgin" target partner there rlar is called.                 !/*is        102*/
!  a "virgin" has ind(34)=2 so it is not misused by rlar.                       !/*is        103*/
!                                                                               !/*is        104*/
   50 if (ind(34).lt.2) go to 190                                               !/*is        105*/
!-----------------------------------------------------------------------        !/*is        106*/
      if (ws(31).eq.5.d+02.and.ind(34).eq.2) go to 190                          !/*is        107*/
!-----------------------------------------------------------------------        !/*is        108*/
      if (ws(2).lt.5.1d0.and.ws(31).gt.0.1d0) go to 60                          !/*is        109*/
!-----------------------------------------------------------------------        !/*is        110*/
! The trouble with this is that a "virgin" going through RLAR                   !/*is        111*/
! is not put back into the projectile, but a "500" particle                     !/*is        112*/
! has been taken out of the projectile and must be put back.                    !/*is        113*/
!-----particles allowed to exist under the fermi sea a treated like             !/*is        114*/
!-----virgins (y. yariv 12/30/86)                                               !/*is        115*/
      if (ws(31).eq.5.d+02) go to 60                                            !/*is        116*/
!-----                                                                          !/*is        117*/
!-----------------------------------------------------------------------        !/*is        118*/
      if (ind(34).eq.2) go to 190                                               !/*is        119*/
      go to 70                                                                  !/*is        120*/
   60 ind(34)=2                                                                 !/*is        121*/
!                                                                               !/*is        122*/
!     ---------------------------------                                         !/*is        123*/
   70 call rlar                                                                 !/*is        124*/
!     ---------------------------------                                         !/*is        125*/
!                                                                               !/*is        126*/
      if (ind(34).lt.0) go to 40                                                !/*is        127*/
      if (kcan.ne.0) go to 160                                                  !/*is        128*/
      if (ind(22).ne.1) go to 80                                                !/*is        129*/
      if (ind(33).eq.0) go to 230                                               !/*is        130*/
      go to 40                                                                  !/*is        131*/
!                                                                               !/*is        132*/
! ind(21) corresponds to the various models for the isobar model.               !/*is        133*/
!                                                                               !/*is        134*/
   80 ind21=ind(21)                                                             !/*is        135*/
!                                                                               !/*is        136*/
! ind(21)=1 is equivalent to mdl=71 in sub ainput.                              !/*is        137*/
!                                                                               !/*is        138*/
      go to (100,100,90,100,100,90), ind21                                      !/*is        139*/
!                                                                               !/*is        140*/
! ind(20)=dp1  here it means in model without refraction nor reflection         !/*is        141*/
!  when particle does not have enough energy to get out, it is treated          !/*is        142*/
!  as if it fell below the cutoff.  the program branches to subr                !/*is        143*/
!  gamma which will store the particle into st as if it fell below              !/*is        144*/
!  cutoff.                                                                      !/*is        145*/
!                                                                               !/*is        146*/
   90 if (ind(20).eq.1) go to 230                                               !/*is        147*/
  100 if (ind(34).eq.2.and.ind(31).eq.0) go to 190                              !/*is        148*/
!                                                                               !/*is        149*/
! ind(17)=1 when isobar is allowed to escape. isobar is forced to decay         !/*is        150*/
!  at the surface. if decay is not allowed it is treated as if it falls         !/*is        151*/
!  below cutoff.                                                                !/*is        152*/
!                                                                               !/*is        153*/
      if (ind(17).eq.1) go to 120                                               !/*is        154*/
!                                                                               !/*is        155*/
!       subr delta and ajtest are bookkeeping routines.                         !/*is        156*/
!                                                                               !/*is        157*/
!     ---------------------------------                                         !/*is        158*/
  110 call delta                                                                !/*is        159*/
  120 call ajtest                                                               !/*is        160*/
!     ---------------------------------                                         !/*is        161*/
!                                                                               !/*is        162*/
      if (kcan.ne.0) go to 160                                                  !/*is        163*/
!                                                                               !/*is        164*/
! when icsb(1) is zero ,then there are no more active particles to              !/*is        165*/
!  follow for that cascade.  hence edit and count are called to do              !/*is        166*/
!  some partial editing and to store some information(st) on tape.              !/*is        167*/
!                                                                               !/*is        168*/
      if (icsb(1)) 40,130,40                                                    !/*is        169*/
!                                                                               !/*is        170*/
! ind(7)=1 when a cascade is finished.                                          !/*is        171*/
!                                                                               !/*is        172*/
  130 ind(7)=1                                                                  !/*is        173*/
!         if skewed "ka"s and no annihilation disregard cascade                 !/*is        174*/
      if (kabran.and..not.annih.and.ind(3).ne.1) go to 160                      !/*is        175*/
      ind(7)=1                                                                  !/*is        176*/
!     store cumulative values                                                   !/*is        177*/
      do 140 i=1,5                                                              !/*is        178*/
      aips(i)=aip(i)                                                            !/*is        179*/
      ains(i)=ain(i)                                                            !/*is        180*/
  140 continue                                                                  !/*is        181*/
      do 150 i=1,2                                                              !/*is        182*/
      sws(i)=ws(21+i)                                                           !/*is        183*/
      swsp(i)=wsp(21+i)                                                         !/*is        184*/
  150 continue                                                                  !/*is        185*/
      sws30=ws(30)                                                              !/*is        186*/
      call isedt (ws)                                                           !/*is        187*/
      if (kcan.ne.0) go to 160                                                  !/*is        188*/
!//////                                                                         !/*is        189*/
      if (repeat.and.nprts.lt.0.and.nrpcnt.lt.10) then                          !/*is        190*/
        nrpcnt=nrpcnt+1                                                         !/*is        191*/
        go to 160                                                               !/*is        192*/
      endif                                                                     !/*is        193*/
!//////                                                                         !/*is        194*/
      dsel(1)=sel(1)                                                            !/*is        195*/
      dsel(2)=sel(2)                                                            !/*is        196*/
      return                                                                    !/*is        197*/
!                                                                               !/*is        198*/
!   stmnt 230 is for cancelled cascades. if kcan.ne.0 the cascade               !/*is        199*/
!   is disregarded. for "skewed" calculations the cascade                       !/*is        200*/
!   cancelled due to "skewing" is counted as inelastic.                         !/*is        201*/
  160 continue                                                                  !/*is        202*/
      if (kcan.ne.0) kcas=kcas+1                                                !/*is        203*/
! restore cumulative values for cancelled cascades                              !/*is        204*/
      do 170 i=1,5                                                              !/*is        205*/
      aip(i)=aips(i)                                                            !/*is        206*/
      ain(i)=ains(i)                                                            !/*is        207*/
  170 continue                                                                  !/*is        208*/
      do 180 i=1,2                                                              !/*is        209*/
      ws(21+i)=sws(i)                                                           !/*is        210*/
      wsp(21+i)=swsp(i)                                                         !/*is        211*/
  180 continue                                                                  !/*is        212*/
      ws(30)=sws30                                                              !/*is        213*/
      if (kcan.eq.0) go to 10                                                   !/*is        214*/
!                                                                               !/*is        215*/
      poe1=nocas                                                                !/*is        216*/
      write (io,260) nocas,lkcan,kcan                                           !/*is        217*/
      bimp=sqrt(proj(5)**2+proj(6)**2)                                          !/*is        218*/
      if (kcan.eq.2) write (io,270) bimp,ws(13)*dp10                            !/*is        219*/
      if (kcan.eq.3) write (io,280)                                             !/*is        220*/
      kcan=0                                                                    !/*is        221*/
      if (lkcan.eq.'islink 1'.or.lkcan.eq.'isedt 4') repeat=.true.              !/*is        222*/
      if (mprin.le.6) go to 10                                                  !/*is        223*/
      write (io,290) istept,istepp,sel,spr,ind,icsb,ws,wsp,gws                  !/*is        224*/
      k81=(icsb(15)+1)*i81                                                      !/*is        225*/
      write (io,300) ((st(i+j-i81),j=1,31),st(i),i=i81,k81,i81)                 !/*is        226*/
      go to 10                                                                  !/*is        227*/
!                                                                               !/*is        228*/
!     subr psel selects momentum of partner of collision.  psel also            !/*is        229*/
!     refracts,reflects particle as it crosses regions in the nucleus.          !/*is        230*/
!     for isobar model, psel creates isobar whenever pion interacts             !/*is        231*/
!     with a nucleon.                                                           !/*is        232*/
!                                                                               !/*is        233*/
  190 call psel (gdc9,gdc10)                                                    !/*is        234*/
      if (kcan.ne.0) go to 160                                                  !/*is        235*/
!                                                                               !/*is        236*/
!      if ind33.eq.0 then nothing in fermi sea, but                             !/*is        237*/
!     maybe a cas-cas is possible                                               !/*is        238*/
!     isobars below cutoff are "destroyed" in gamma                             !/*is        239*/
!                                                                               !/*is        240*/
      if (ind(33).eq.0.and.ind(22).eq.0) go to 220                              !/*is        241*/
!                                                                               !/*is        242*/
! ind(16)=1 when an isobar has just been made.                                  !/*is        243*/
!                                                                               !/*is        244*/
      if (ind(16).eq.1) go to 250                                               !/*is        245*/
      if (ws(3).gt.dp10) go to 200                                              !/*is        246*/
      if (ws(3).gt.dp2) go to 210                                               !/*is        247*/
      if (ind(20).eq.1) go to 230                                               !/*is        248*/
!                                                                               !/*is        249*/
!       subr ainter calculates whether or not if a particle will                !/*is        250*/
!  interact if so, where the collision site is.                                 !/*is        251*/
!                                                                               !/*is        252*/
  200 call ainter (gdc9,gdc10)                                                  !/*is        253*/
!                                                                               !/*is        254*/
      if (ind(22).eq.1) go to 230                                               !/*is        255*/
!                                                                               !/*is        256*/
! when ind(4)=1, there is no interaction.                                       !/*is        257*/
!                                                                               !/*is        258*/
  210 if (ind(4).eq.0) go to 230                                                !/*is        259*/
!                                                                               !/*is        260*/
!   if ntdel <=0 no cas-cas interactions                                        !/*is        261*/
!                                                                               !/*is        262*/
  220 continue                                                                  !/*is        263*/
      if (ntdel.le.0) go to 110                                                 !/*is        264*/
!                                                                               !/*is        265*/
!   cas-cas enters every ntdel time interval                                    !/*is        266*/
!                                                                               !/*is        267*/
      if (mod(nti,ntdel).gt.0) go to 110                                        !/*is        268*/
!                                                                               !/*is        269*/
      call cascas                                                               !/*is        270*/
      if (kcan.ne.0) go to 160                                                  !/*is        271*/
!                                                                               !/*is        272*/
      if (ind(4).eq.1) go to 110                                                !/*is        273*/
      if (ind(16).eq.1) go to 190                                               !/*is        274*/
!                                                                               !/*is        275*/
!       subr gamma performs the actual collision.                               !/*is        276*/
!                                                                               !/*is        277*/
  230 call pgamma                                                               !/*is        278*/
      if (kcan.ne.0) go to 160                                                  !/*is        279*/
!                                                                               !/*is        280*/
! ind(26)=1 when isobar changes identity in exchange scattering.                !/*is        281*/
!                                                                               !/*is        282*/
      ind(26)=0                                                                 !/*is        283*/
!                                                                               !/*is        284*/
! changes made 10/14/70 to check for isobars created below cut-off in           !/*is        285*/
! gamma                                                                         !/*is        286*/
!  24 if(ind(19).eq.1)go to 5                                                   !/*is        287*/
!                                                                               !/*is        288*/
! ind(19)=1 when more than 12 interactions has been made by a particle.         !/*is        289*/
! this is checked in subr gamma.  ind(19)=1 also in subr                        !/*is        290*/
!  totcrs only cii or cij is needed.                                            !/*is        291*/
!                                                                               !/*is        292*/
      if (ind(19).ne.1) go to 240                                               !/*is        293*/
!                                                                               !/*is        294*/
!    this  handles ind19=1 by making                                            !/*is        295*/
!    the particle one that is leaving the system (ind34=3)                      !/*is        296*/
!                                                                               !/*is        297*/
      if (ws(1).lt.dp0) go to 120                                               !/*is        298*/
      ind(33)=0                                                                 !/*is        299*/
      ind(34)=3                                                                 !/*is        300*/
      ind(20)=0                                                                 !/*is        301*/
      go to 50                                                                  !/*is        302*/
!                                                                               !/*is        303*/
  240 if (ind(22).eq.1) go to 40                                                !/*is        304*/
!                                                                               !/*is        305*/
! ind(20)=1 when particle falls below cutoff.                                   !/*is        306*/
!                                                                               !/*is        307*/
  250 if (ind(20).ne.1) go to 120                                               !/*is        308*/
      ind(20)=0                                                                 !/*is        309*/
!                                                                               !/*is        310*/
!           following statement terminates at point of                          !/*is        311*/
!       annihilation those cascades that do not produce                         !/*is        312*/
!       kaons, keeping cross-sections correct. n.b.  the                        !/*is        313*/
!       number of cascades must be the correct number of                        !/*is        314*/
!       unskewed ones in csa(6), not the number of kaon-                        !/*is        315*/
!       producing cascades.                                                     !/*is        316*/
!                                                                               !/*is        317*/
      if (ind(28).ge.2) annih=.true.                                            !/*is        318*/
      if (kabran.and.(ind(28).eq.2)) go to 160                                  !/*is        319*/
!                                                                               !/*is        320*/
!-----if a particle below fermi sea interacted it is not anymore                !/*is        321*/
!-----below fermi sea and should not be frozen in projectile f.o.r.             !/*is        322*/
!-----when pushed in selpt (y. yariv 12/29/86)                                  !/*is        323*/
!                                                                               !/*is        324*/
      if (ws(31).eq.5.d+02) ws(31)=-dp1                                         !/*is        325*/
!                                                                               !/*is        326*/
      go to 40                                                                  !/*is        327*/
!                                                                               !/*is        328*/
  260 format (' cascade ',i7,' stopped at ',a8,' (kcan=',i2,')')                !/*is        329*/
  270 format ('    impact parameter = ',f10.2,' fm',' time = ',f10.2,           !/*is        330*/
     & ' fm/c')                                                                 !/*is        331*/
  280 format (' aa had more than 360 , or bb more than 400 particles')          !/*is        332*/
  290 format ('  main      istept/istepp',2i5,'     sel/spr',6f5.0/             !/*is        333*/
     & ' ind',40i3/' icsb',17i6/' ws'/4(1x,8f15.4/),' wsp'/                     !/*is        334*/
     & 4(1x,8f15.4/),' gws'/4(1x,8f15.4/))                                      !/*is        335*/
  300 format (20(1x,16f8.2/3(1x,5f8.2),f6.0/))                                  !/*is        336*/
      end                                                                       !/*is        337*/

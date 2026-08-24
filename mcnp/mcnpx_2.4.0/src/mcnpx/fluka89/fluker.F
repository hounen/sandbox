!     $Id: fluker.F,v 1.5 2002/03/27 18:14:43 gwm Exp $                       !/*fl          1*/
      subroutine fluker (ain,zin,ityp,npart,ec,kind,ep,alpha,beta,gam,ij        !/*fl          2*/
     & ,ex,geom_xs,lmxs_)                                                       !/*fl          3*/
!REP                                                                            !/*fl          4*/
!     returns IJ as FLUKA index of incident particle.                           !/*fl          5*/
!     returns EX (1000*TV) as excitation + recoil                               !/*fl          6*/
!     KIND still has KLUKA index; all secondaries are retained.                 !/*fl          7*/
!REP                                                                            !/*fl          8*/
!     fluka interface routine for lahet                                         !/*fl          9*/
!                                                                               !/*fl         10*/
!     calls the following fluka routines                                        !/*fl         11*/
!     nizl    - gets cross section for reaction                                 !/*fl         12*/
!     disint  - decays unstable particles                                       !/*fl         13*/
!     eventq  - the hadron shower generator                                     !/*fl         14*/
!                                                                               !/*fl         15*/
!     some modifications were needed to interface the codes. each               !/*fl         16*/
!     modification is marked   a.g.  to help you spot it.                       !/*fl         17*/
!                                                                               !/*fl         18*/
!     ain   is nuclear weight of struck nucleus                                 !/*fl         19*/
!     zin   is nuclear charge of struck nucleus                                 !/*fl         20*/
!     ityp  is lahet ityp of striking particle                                  !/*fl         21*/
!     np=npart is number of particles produced                                  !/*fl         22*/
!     ec    is incoming particle kinetic energy (mev)                           !/*fl         23*/
!     sigg  is geometric cross-section                                          !/*fl         24*/
!     apr   mass and charge of residual nucleus                                 !/*fl         25*/
!     kind(i),i=1,np  lahet typ of striking particle, in integer                !/*fl         26*/
!     ep(i)           kinetic energy (mev) of i-th particle                     !/*fl         27*/
!     alpha(i), beta(i),gam(i)   direction cosines                              !/*fl         28*/
!                                                                               !/*fl         29*/
!     fluka random number generator replaceb by rang from lahet                 !/*fl         30*/
!                                                                               !/*fl         31*/
!     the external statement for block datas added by a. ferrari on    *        !/*fl         32*/
!     27-5-89, to avoid to explicitly load all block datas during      *        !/*fl         33*/
!     linking!                                                         *        !/*fl         34*/
!----------------------------------------------------------------------*        !/*fl         35*/
!                                                                      *        !/*fl         36*/
!======================================================================*        !/*fl         37*/
!                                                                      *        !/*fl         38*/
!                                                                      *        !/*fl         39*/
!   fffff   l       u   u  k   k   aaa        v   v   aaa   x   x      *        !/*fl         40*/
!   f       l       u   u  k  k   a   a       v   v  a   a  x   x      *        !/*fl         41*/
!   f       l       u   u  k k    a   a       v   v  a   a   x x       *        !/*fl         42*/
!   ffff    l       u   u  kk     aaaaa  ===  v   v  aaaaa    x        *        !/*fl         43*/
!   f       l       u   u  k k    a   a       v   v  a   a   x x       *        !/*fl         44*/
!   f       l       u   u  k  k   a   a        v v   a   a  x   x      *        !/*fl         45*/
!   f       lllll    uuu   k   k  a   a         v    a   a  x   x      *        !/*fl         46*/
!                                                                      *        !/*fl         47*/
!         version of fluka90 for vax, vms operating system             *        !/*fl         48*/
!         by :                                                         *        !/*fl         49*/
!                    a. ferrari infn milan and cern - sps              *        !/*fl         50*/
!                    a. fasso`  cern - tis                             *        !/*fl         51*/
!                    p. sala    infn milan                             *        !/*fl         52*/
!                                                                      *        !/*fl         53*/
!         this version includes all the features of the fluka90        *        !/*fl         54*/
!           package together with a few further options, which,        *        !/*fl         55*/
!           are not yet included in the ibm-vm fluka90 release         *        !/*fl         56*/
!                                                                      *        !/*fl         57*/
!======================================================================*        !/*fl         58*/
!----------------------------------------------------------------------*        !/*fl         59*/
!     version (not an official one!!!!) may 1990                       *        !/*fl         60*/
!                            (c) copyright of the authors              *        !/*fl         61*/
!                                                                      *        !/*fl         62*/
!     authors: p.a. aarnio, a. fasso, a. ferrari,  h. moehring,        *        !/*fl         63*/
!              j. ranft, g.r. stevenson, j. m. zazula                  *        !/*fl         64*/
!                                                                      *        !/*fl         65*/
!     p.a. aarnio:       helsinki university of technology             *        !/*fl         66*/
!                        department of technical physics               *        !/*fl         67*/
!                        sf-02150 espoo                                *        !/*fl         68*/
!                        finland                                       *        !/*fl         69*/
!                                                                      *        !/*fl         70*/
!     a. fasso:          cern tis-rp                                   *        !/*fl         71*/
!                        ch-1217 geneva                                *        !/*fl         72*/
!                        switzerland                                   *        !/*fl         73*/
!                                                                      *        !/*fl         74*/
!     a. ferrari:        infn - milan  and cern sps                    *        !/*fl         75*/
!                        milan                                         *        !/*fl         76*/
!                        italy                                         *        !/*fl         77*/
!                                                                      *        !/*fl         78*/
!     h. moehring:       karl marx university                          *        !/*fl         79*/
!                        leipzig                                       *        !/*fl         80*/
!                        ddr                                           *        !/*fl         81*/
!                                                                      *        !/*fl         82*/
!     j. ranft:          karl marx university                          *        !/*fl         83*/
!                        leipzig                                       *        !/*fl         84*/
!                        ddr                                           *        !/*fl         85*/
!                                                                      *        !/*fl         86*/
!     g.r. stevenson:    cern tis-rp                                   *        !/*fl         87*/
!                        ch-1217 geneva                                *        !/*fl         88*/
!                        switzerland                                   *        !/*fl         89*/
!                                                                      *        !/*fl         90*/
!     j.m. zazula                                                      *        !/*fl         91*/
!                                                                      *        !/*fl         92*/
!                                                                      *        !/*fl         93*/
!     revision history:                                                *        !/*fl         94*/
!                                                                      *        !/*fl         95*/
!     rev. may    1981:   development version used only by the author  *        !/*fl         96*/
!                                                                      *        !/*fl         97*/
!     rev. august 1981:   version tested internally in rp-group        *        !/*fl         98*/
!                                                                      *        !/*fl         99*/
!     rev. august 1982:   version put on limited release together      *        !/*fl        100*/
!                         with the preliminary fluka82 manual.         *        !/*fl        101*/
!                                                                      *        !/*fl        102*/
!     rev. june   1983:   first release of the code and the manual     *        !/*fl        103*/
!                                                                      *        !/*fl        104*/
!     rev. dec.   1983:   known bugs are corrected, fermi-momentum     *        !/*fl        105*/
!                         for nucleons in nuclei added, eventq         *        !/*fl        106*/
!                         recommended and changed as the default       *        !/*fl        107*/
!                         cross sections extended to higher energies   *        !/*fl        108*/
!                                                                      *        !/*fl        109*/
!     rev. july   1984:   elastic cross section routine sigel          *        !/*fl        110*/
!                         track length, boundary crossing and collision*        !/*fl        111*/
!                         estimators added. batch statistics           *        !/*fl        112*/
!                         implemented.                                 *        !/*fl        113*/
!                                                                      *        !/*fl        114*/
!     rev. sept.  1984:   em-shower in kaskad created trouble in       *        !/*fl        115*/
!                         geometries involving certain vacuum          *        !/*fl        116*/
!                         configurations. that has been corrected.     *        !/*fl        117*/
!                                                                      *        !/*fl        118*/
!                         this revision contains some features not     *        !/*fl        119*/
!                         documented in the writeup.  they can be      *        !/*fl        120*/
!                         used by studying the input defined in the    *        !/*fl        121*/
!                         main program.                                *        !/*fl        122*/
!                                                                      *        !/*fl        123*/
!     rev. nov.   1985:  preliminary version of fluka86                *        !/*fl        124*/
!                        verified running on fortran77 compiler.       *        !/*fl        125*/
!                        known bugs corrected, only serious one        *        !/*fl        126*/
!                        the low energy photon cross section,          *        !/*fl        127*/
!                        faster ionisation energy deposition,          *        !/*fl        128*/
!                        matregs card added in cartesian geometry,     *        !/*fl        129*/
!                        egs-link added i.e. em-cascade energy         *        !/*fl        130*/
!                        deposition can now be treated by egs,         *        !/*fl        131*/
!                        simple calorimeter option added, leading      *        !/*fl        132*/
!                        particle biasing, possibility to input        *        !/*fl        133*/
!                        the radiation length manually.                *        !/*fl        134*/
!                        possibility for scoring the em-cascade        *        !/*fl        135*/
!                        energy only (particle type 211) added.        *        !/*fl        136*/
!                        energy deposited in black holes is also       *        !/*fl        137*/
!                        scored regionwise.                            *        !/*fl        138*/
!                                                                      *        !/*fl        139*/
!     rev. jan.   1986:  first release of fluka86                      *        !/*fl        140*/
!                        diffractive events included in the            *        !/*fl        141*/
!                        eventq-model.  first version of a             *        !/*fl        142*/
!                        new beam definition (emittance and            *        !/*fl        143*/
!                        beamvert cards) implemented.                  *        !/*fl        144*/
!                                                                      *        !/*fl        145*/
!     rev. jan.   1987:  first release of fluka87                      *        !/*fl        146*/
!                        new version of the eventq.                    *        !/*fl        147*/
!                        photoproduction of the hadrons.               *        !/*fl        148*/
!                        nucleus-nucleus collisions.                   *        !/*fl        149*/
!                        true compounds, magnetic fields,              *        !/*fl        150*/
!                        ionization energy fluctuations (landau        *        !/*fl        151*/
!                        & gaussian), boundary crossing, track-length, *        !/*fl        152*/
!                        collision estimators for electrons, photons   *        !/*fl        153*/
!                        and positrons also, particle ranges,          *        !/*fl        154*/
!                        modified low energy treatment.                *        !/*fl        155*/
!                                                                      *        !/*fl        156*/
!     rev. may 89 - may 90 implementation of the vms version: magnetic *        !/*fl        157*/
!                        field transport extended to electrons and po- *        !/*fl        158*/
!                        sitrons, new versions of the multiple scatte- *        !/*fl        159*/
!                        ring both for hadrons and leptons, general re-*        !/*fl        160*/
!                        vision of the event generator                 *        !/*fl        161*/
!                                                                      *        !/*fl        162*/
!     last change 11. may 90 by alfredo ferrari                        *        !/*fl        163*/
!----------------------------------------------------------------------*        !/*fl        164*/
!                                                                      *        !/*fl        165*/
!                                                                      *        !/*fl        166*/
!----------------------------------------------------------------------*        !/*fl        167*/
!                                                                      *        !/*fl        168*/
!     include the common blocks                                        *        !/*fl        169*/
!                                                                      *        !/*fl        170*/
!----------------------------------------------------------------------*        !/*fl        171*/
!                                                                      *        !/*fl        172*/
!                                                                      *        !/*fl        173*/
#include <lcs/double.h>                                                         /*!fl        174*/
      external blkdt1, blkdt2, blkdt3, blkdt4, blkdt5, blkdt6, blkdt7,          !/*fl        175*/
     & bdnopt, countr                                                           !/*fl        176*/
#include <lcs/lptS.h>                                                           /*!fl        177*/
#include <fluka89/flprm1.h>                                                     /*!fl        178*/
#include <fluka89/flprm2.h>                                                     /*!fl        179*/
#include <lcs/inout.h>                                                          /*!fl        180*/
#include <fluka89/blnkcm.h>                                                     /*!fl        181*/
#include <fluka89/countq.h>                                                     /*!fl        182*/
#include <fluka89/finuc1.h>                                                     /*!fl        183*/
!*ca geosel                                                                     !/*fl        184*/
#include <fluka89/mapa.h>                                                       /*!fl        185*/
#include <fluka89/paprop.h>                                                     /*!fl        186*/
#include <fluka89/parevt.h>                                                     /*!fl        187*/
#include <fluka89/resnuc.h>                                                     /*!fl        188*/
!*ca thr                                                                        !/*fl        189*/
#include <lcs/fltolh.h>                                                         /*!fl        190*/
      parameter (mxm=10)                                                        !/*fl        191*/
      dimension cxrd(mxm), cyrd(mxm), czrd(mxm), tkid(mxm),                     !/*fl        192*/
     & weid(mxm), kpartd(mxm)                                                   !/*fl        193*/
      dimension cxr2(mxm), cyr2(mxm), czr2(mxm), tki2(mxm),                     !/*fl        194*/
     & wei2(mxm), kpart2(mxm)                                                   !/*fl        195*/
!                                                                      *        !/*fl        196*/
!----------------------------------------------------------------------*        !/*fl        197*/
!                                                                      *        !/*fl        198*/
!     define the local variables                                       *        !/*fl        199*/
!                                                                      *        !/*fl        200*/
!----------------------------------------------------------------------*        !/*fl        201*/
!                                                                      *        !/*fl        202*/
      character*8 sdum                                                          !/*fl        203*/
      dimension what(6)                                                         !/*fl        204*/
!                                                                               !/*fl        205*/
!     comment the next 2 cards on ibm: activate them on vax                     !/*fl        206*/
!                                                                               !/*fl        207*/
!     external handler                                                          !/*fl        208*/
!     integer*4 handler                                                         !/*fl        209*/
!                                                                      *        !/*fl        210*/
!----------------------------------------------------------------------*        !/*fl        211*/
!                                                                      *        !/*fl        212*/
!                                                                               !/*fl        213*/
!     dimensions from lahet - begin                                             !/*fl        214*/
!                                                                               !/*fl        215*/
#include <lcs/l05S.h>                                                           /*!fl        216*/
      dimension alpha(l05), beta(l05), gam(l05), ep(l05), kind(l05)             !/*fl        217*/
!                                                                               !/*fl        218*/
!     dimensions from lahet - end                                               !/*fl        219*/
!                                                                               !/*fl        220*/
!     at this time we are using a temporary fix instead of correct              !/*fl        221*/
!     decay routines                                                            !/*fl        222*/
!     a.g. particles with indices above 6 will later be deleted.                !/*fl        223*/
!     there seems to be a bug in fluka that gives an index of 99                !/*fl        224*/
      data ientry /0/                                                           !/*fl        225*/
!                                                                               !/*fl        226*/
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        !/*fl        227*/
!                                                                               !/*fl        228*/
!   START OF CODING                                                             !/*fl        229*/
!                                                                               !/*fl        230*/
      if (ientry.gt.0) go to 10                                                 !/*fl        231*/
      ientry=1                                                                  !/*fl        232*/
!REP  nbatch=0                                                                  !/*fl        233*/
      jbatch=0                                                                  !/*fl        234*/
      den1=1.d0                                                                 !/*fl        235*/
      den2=0.d0                                                                 !/*fl        236*/
      den3=0.d0                                                                 !/*fl        237*/
      imdfx=0                                                                   !/*fl        238*/
      ieof=0                                                                    !/*fl        239*/
!REP  landa=.false.                                                             !/*fl        240*/
      call evqini (what,sdum)                                                   !/*fl        241*/
      write (io,60)                                                             !/*fl        242*/
      txi=0.                                                                    !/*fl        243*/
      tyi=0.                                                                    !/*fl        244*/
      tzi=1.                                                                    !/*fl        245*/
   10 continue                                                                  !/*fl        246*/
!                                                                               !/*fl        247*/
      eke=ec*.001d0                                                             !/*fl        248*/
! gev for fluka                                                                 !/*fl        249*/
      anow=ain                                                                  !/*fl        250*/
      iain=ain+.0001d0                                                          !/*fl        251*/
      znow=zin                                                                  !/*fl        252*/
      npart=0                                                                   !/*fl        253*/
      ij=itypfl(ityp)                                                           !/*fl        254*/
      poo=sqrt(eke*(eke+dp2*am(ij)))                                            !/*fl        255*/
      if (lmxs_.ne.0) then                                                      !/*fl        256*/
         call nizl (ij,anow,poo,si,zl)                                          !/*fl        257*/
         si_barns=0.001d0*si                                                    !/*fl        258*/
         if (si_barns.gt.geom_xs) then                                          !/*fl        259*/
            write (itty,*) ' ityp,a,p/c,si_barns,geom_xs ',ityp,anow,           !/*fl        260*/
     &       poo,si_barns,geom_xs                                               !/*fl        261*/
            write (io,*) ' ityp,a,p/c,si_barns,geom_xs ',ityp,anow,             !/*fl        262*/
     &       poo,si_barns,geom_xs                                               !/*fl        263*/
!           call errorf ('fluker.3')                                            !/*fl        264*/
         endif                                                                  !/*fl        265*/
         ratio=si_barns/geom_xs                                                 !/*fl        266*/
         if (rang().gt.ratio) then                                              !/*fl        267*/
!           Transparency                                                        !/*fl        268*/
            npart=-1                                                            !/*fl        269*/
            return                                                              !/*fl        270*/
         endif                                                                  !/*fl        271*/
      endif                                                                     !/*fl        272*/
      call eventq (ij,poo,eke,txi,tyi,tzi,we)                                   !/*fl        273*/
      if (np.le.0) return                                                       !/*fl        274*/
      if (np.gt.l05) then                                                       !/*fl        275*/
        write (io,*) ' l05 in fluka is too small. np = ',np                     !/*fl        276*/
        np=l05                                                                  !/*fl        277*/
      endif                                                                     !/*fl        278*/
      l=0                                                                       !/*fl        279*/
      do 50 i=1,np                                                              !/*fl        280*/
      if (kpart(i).gt.25) then                                                  !/*fl        281*/
        call errorf ('fluker.1')                                                !/*fl        282*/
      elseif ((kpart(i).ge.0.and.kpart(i).le.16).or.kpart(i).eq.19.or           !/*fl        283*/
     & .kpart(i).eq.23) then                                                    !/*fl        284*/
!*******************ADD TO LAHET STACK                                          !/*fl        285*/
        l=l+1                                                                   !/*fl        286*/
        if (l.gt.l05) call errorf ('fluker.2')                                  !/*fl        287*/
        alpha(l)=cxr(i)                                                         !/*fl        288*/
        beta(l)=cyr(i)                                                          !/*fl        289*/
        gam(l)=czr(i)                                                           !/*fl        290*/
        kind(l)=kpart(i)                                                        !/*fl        291*/
        ep(l)=1000.*tki(i)                                                      !/*fl        292*/
      else                                                                      !/*fl        293*/
        poo=sqrt(tki(i)*(dp2*am(kpart(i))+tki(i)))                              !/*fl        294*/
        call disint (kpart(i),poo,tki(i),cxr(i),cyr(i),czr(i),we,cxrd           !/*fl        295*/
     &  ,cyrd,czrd,tkid,kpartd,weid,npd)                                        !/*fl        296*/
        idcyflag=0                                                              !/*fl        297*/
! this flag signals secondary decays when idcyflag=1                            !/*fl        298*/
        do 20 idcy=1,npd                                                        !/*fl        299*/
        if ((kpartd(idcy).ge.0.and.kpartd(idcy).le.16).or.kpartd(idcy)          !/*fl        300*/
     &  .eq.19.or.kpartd(idcy).eq.23) then                                      !/*fl        301*/
!...................ADD TO LAHET STACK                                          !/*fl        302*/
          l=l+1                                                                 !/*fl        303*/
          alpha(l)=cxrd(idcy)                                                   !/*fl        304*/
          beta(l)=cyrd(idcy)                                                    !/*fl        305*/
          gam(l)=czrd(idcy)                                                     !/*fl        306*/
          kind(l)=kpartd(idcy)                                                  !/*fl        307*/
          ep(l)=1000.*tkid(idcy)                                                !/*fl        308*/
        else                                                                    !/*fl        309*/
          idcyflag=1                                                            !/*fl        310*/
        endif                                                                   !/*fl        311*/
   20   continue                                                                !/*fl        312*/
        if (idcyflag.eq.1) then                                                 !/*fl        313*/
          do 40 idcy=1,npd                                                      !/*fl        314*/
          if ((kpartd(idcy).ge.0.and.kpartd(idcy).le.16).or.kpartd(idcy)        !/*fl        315*/
     &    .eq.19.or.kpartd(idcy).eq.23) go to 40                                !/*fl        316*/
!   now decay unstable secondaries                                              !/*fl        317*/
          kparto=kpartd(idcy)                                                   !/*fl        318*/
          tkio=tkid(idcy)                                                       !/*fl        319*/
          poo=sqrt(tkio*(tkio+dp2*am(kparto)))                                  !/*fl        320*/
          cxro=cxrd(idcy)                                                       !/*fl        321*/
          cyro=cyrd(idcy)                                                       !/*fl        322*/
          czro=czrd(idcy)                                                       !/*fl        323*/
          call disint (kparto,poo,tkio,cxro,cyro,czro,we,cxr2,cyr2,czr2         !/*fl        324*/
     &    ,tki2,kpart2,wei2,np2)                                                !/*fl        325*/
          do 30 jdcy=1,np2                                                      !/*fl        326*/
!...................ADD TO LAHET STACK                                          !/*fl        327*/
          l=l+1                                                                 !/*fl        328*/
          alpha(l)=cxr2(jdcy)                                                   !/*fl        329*/
          beta(l)=cyr2(jdcy)                                                    !/*fl        330*/
          gam(l)=czr2(jdcy)                                                     !/*fl        331*/
          kind(l)=kpart2(jdcy)                                                  !/*fl        332*/
          ep(l)=1000.*tki2(jdcy)                                                !/*fl        333*/
   30     continue                                                              !/*fl        334*/
   40     continue                                                              !/*fl        335*/
        endif                                                                   !/*fl        336*/
      endif                                                                     !/*fl        337*/
   50 continue                                                                  !/*fl        338*/
!REP/LW                                                                         !/*fl        339*/
      npart=l                                                                   !/*fl        340*/
!REP/LW                                                                         !/*fl        341*/
      ex=1000.d0*tv                                                             !/*fl        342*/
!REP                                                                            !/*fl        343*/
      return                                                                    !/*fl        344*/
!                                                                               !/*fl        345*/
   60 format (//' flukavax-1.0/1990 a. ferrari - initialized'//)                !/*fl        346*/
      end                                                                       !/*fl        347*/

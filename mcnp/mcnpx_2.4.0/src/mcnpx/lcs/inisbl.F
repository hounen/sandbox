!     $Id: inisbl.F,v 1.5 2002/03/27 18:15:08 gwm Exp $                       !/*in          1*/
      subroutine inisbl (ichoic)                                                !/*in          2*/
! INISBL calls   AINPUT  PMCDGI                                                 !/*in          3*/
#include <lcs/double.h>                                                         /*!in          4*/
!                                                                               !/*in          5*/
!    there remains to use sel(1,2) to see what the remaining                    !/*in          6*/
!     z,a of the target is, and if dp1, to use "pit" for the proper             !/*in          7*/
!     "fermi" momentum if another partner is needed, instead of pulling         !/*in          8*/
!     a particle out of the fermi-sea.                                          !/*in          9*/
!                                                                               !/*in         10*/
!     to boost into the target rest-frame, selpt might be the place             !/*in         11*/
!     to do it                                                                  !/*in         12*/
!      ajtest is done for various place in the 20 subintervals                  !/*in         13*/
!     for each active particle, making it not a good place.                     !/*in         14*/
!                                                                               !/*in         15*/
!    version (is2) has been modified in its CFT dependent coding, to            !/*in         16*/
!    rename some CAL routines (nxt1p --> nxtp(,,1); num1p --> nump(,,1)         !/*in         17*/
!                                                                               !/*in         18*/
!    version (is1) has been extensively modified by Yariv to perform            !/*in         19*/
!    all calculations in units of MeV, rather than electron masses.             !/*in         20*/
!                                                                               !/*in         21*/
!   version (is) has been modified by yariv (sept/oct 1984)                     !/*in         22*/
!   to insure symmetry in c12+c12 collisions at zero impact                     !/*in         23*/
!   parameter, by correcting minor bugs in "cascas".  Further,                  !/*in         24*/
!   the various models that turn on refraction/reflection                       !/*in         25*/
!   have been corrected so that they work.                                      !/*in         26*/
!    Additionally, the code (ish/isc) has been changed                          !/*in         27*/
!   to be basically fortran77 correct:  files are                               !/*in         28*/
!   connected to logical unit numbers via "open" statements,                    !/*in         29*/
!   AND  in all cases, one reads in a file name ("fnameh"),                     !/*in         30*/
!        and generates the following output files                               !/*in         31*/
!             fnameh     history  info                 (binary)                 !/*in         32*/
!        "e"//fnameh     target evaporation info       (binary)                 !/*in         33*/
!        "f"//fnameh     projectile evaporation info   (binary)                 !/*in         34*/
!        "m"//fnameh     movie info (for Dan Strottman, LANL only)              !/*in         35*/
!        "a"//fnameh     ascii info output by ISABEL                            !/*in         36*/
!                                                                               !/*in         37*/
!     version (ap24) has been modified so that the historian                    !/*in         38*/
!     utility can assign filenames based on an input name (via apin),           !/*in         39*/
!     or fix the names in the program card for external users                   !/*in         40*/
!      internal: ap<name>, e<name>, f<name>, m<name>, <name>                    !/*in         41*/
!      external: apout   , evtar  , evpro  , movie  , histry                    !/*in         42*/
!      lun #   :   6     ,   1    ,   2    ,   3    ,   10                      !/*in         43*/
!                                                                               !/*in         44*/
!     version (ap23) has one minor change in subroutine                         !/*in         45*/
!     "begin", to write the correct amounts of xr, ro, and dx (or dxp)          !/*in         46*/
!     and to write the starting random seed (irand0) not irand.                 !/*in         47*/
!                                                                               !/*in         48*/
!      version (ap22) has four changes:  rather than do simple                  !/*in         49*/
!     summary output to "apout", "ap" is prefixed to the history                !/*in         50*/
!     file-name, so that the ctl-controller does not have to switch it.         !/*in         51*/
!     further, the arrays tnpar and pnpar have been expanded                    !/*in         52*/
!     to (7,250) from (4,250), to enable storage of the hole                    !/*in         53*/
!     momenta.  Then, when in ap22c, there is a "df,movies" line,               !/*in         54*/
!     the program will write a file "m_history-file-name" with                  !/*in         55*/
!     all pertinent information                                                 !/*in         56*/
!       third, the pion charge-multiplicity routine (pmcd) has                  !/*in         57*/
!       been replaced by (pmcdg) to do the correct multiplcity                  !/*in         58*/
!       as a function of energy, and does combinatorics to                      !/*in         59*/
!       determine the pi- multiplicity given the total mult.                    !/*in         60*/
!      most importantly, with the "df,correct" option in ap22c,                 !/*in         61*/
!      the program will use pp and pn cross-sections that fit the               !/*in         62*/
!      data(/fits) of arndt, up to 1 gev, and uses the old "bugg"               !/*in         63*/
!      prescriptions beyond that point                                          !/*in         64*/
!                                                                               !/*in         65*/
!     version (ap21) has three changes in it:  rlar__80 and 81 are              !/*in         66*/
!     reversed wrt ap20 to avoid subtracting the fermi binding-enrgy            !/*in         67*/
!     from kaons.  further, pion capture on t=1 pairs of nucleons               !/*in         68*/
!     is completely forbidden now:  isobars can capture on isospin-             !/*in         69*/
!     dependent non-holes only 75% of the time ( in ainter). finally,           !/*in         70*/
!     iden=3 has been altered, so that it is the same as iden=2 for             !/*in         71*/
!     purposes of calculating the radial density, but it generates              !/*in         72*/
!     a heuristic wigner-function momentum distribution by calculating          !/*in         73*/
!     p**2 = ( vf + m + csa(5) )**2 - m**2, having used the thomas-             !/*in         74*/
!     fermi local density approximation to calcuate vf (a la iden=2)            !/*in         75*/
!     in similar manner, the vf calculated with iden=4 now                      !/*in         76*/
!     has csa(5) subtracted from it, so it is treated as all                    !/*in         77*/
!     other iden values are.                                                    !/*in         78*/
!                                                                               !/*in         79*/
!    version (ap20) has eliminated the notion of "pi-skewing",                  !/*in         80*/
!    since it was of no ultimate usefulness.  in its place in common            !/*in         81*/
!    block "new", is instead the variable "gamiso".  this flags                 !/*in         82*/
!    the treatment of the isobar-decay width.  when zero, ginnocchio            !/*in         83*/
!    energy-dependent formula is used.  when non-zero, the number is            !/*in         84*/
!    presumed to be an energy-independent width (in [mev]) for the              !/*in         85*/
!    isobar.  (112 = cugnon, 90 = pre 1978 work with this code)                 !/*in         86*/
!           note added.  when doing nucleus+nucleus, in poenew,                 !/*in         87*/
!           at lattice of nucleons strikes the target, and the                  !/*in         88*/
!           distribution of nucleons in the proj. lattice is not                !/*in         89*/
!           the one so carefully allowed-for in "den1".                         !/*in         90*/
!           this will be rectified sometime (mrc, 14 nov 83)                    !/*in         91*/
!                                                                               !/*in         92*/
!    version (ap19) has again redefined "kaskew", this time                     !/*in         93*/
!    when set (sensible only for pbar+nucleus cases), it determines             !/*in         94*/
!    whether an annihilation will be terminated because it did not pro-         !/*in         95*/
!    duce any kaons.  zip19 will then automatically get the correct             !/*in         96*/
!    values for sigma(theta,e,etc).                                             !/*in         97*/
!       for input, besides kaskew=1, you must enter csa(6) = total              !/*in         98*/
!    number of cascades  ^^= (# kaons)/(.08=branching ratio)                    !/*in         99*/
!    this involved changing 3 or 4 "if"s involving ind(28)                      !/*in        100*/
!        piskew =1 sets a flag "pibran" to true, and if                         !/*in        101*/
!        a pion was not produced, the cascade is cancelled.  this               !/*in        102*/
!        is not truly unbiased, so never use it.                                !/*in        103*/
!                                                                               !/*in        104*/
!     version (ap18) has been modified to write "pip" to the                    !/*in        105*/
!     history file, enabling zipxx to calculate the correct reaction            !/*in        106*/
!     cross-sections, instead of copying half of den1 into zipxx to             !/*in        107*/
!     recalculate the appropriate radii.                                        !/*in        108*/
!                                                                               !/*in        109*/
!     version (ap17) has been modified (hopefully) to allow                     !/*in        110*/
!    more than 11 interactions/particle in a cascade by making the              !/*in        111*/
!    variable alterable in a parameter statement 11==> 60,81                    !/*in        112*/
!                                                13==> 70,91  etc.              !/*in        113*/
!    one must use the equally modified zip17 to have equal                      !/*in        114*/
!    sized blocks of "st" on input/output.                                      !/*in        115*/
!                                                                               !/*in        116*/
!      version (ap16) has been modified to symmetrically treat                  !/*in        117*/
!     nucleus+nucleus collisions with the "iden" parameter = 2 to 4             !/*in        118*/
!     i.e. one can input moments (or folded-yukawa) data for both               !/*in        119*/
!     target and projectile, unlike target-only in earlier versions             !/*in        120*/
!                                                                               !/*in        121*/
!      version (ap15) has also included in it the proper coding                 !/*in        122*/
!     (i hope) to treat kaon charge-exchange properly (i.e. exactly             !/*in        123*/
!     like pion chage-exchange is treated) in gamma.                            !/*in        124*/
!                                                                               !/*in        125*/
!     further, a new variable, "nopot", has been included in the input          !/*in        126*/
!     namelist, to enable one to turn off all potentials.  (1-nopot)            !/*in        127*/
!     multiplies the various fermi-energies.  one must also, in such            !/*in        128*/
!     a case, also set various "csa" elements to just above zero                !/*in        129*/
!     i.e. csa(5,15) = .01 == binding energy of target, proj.  this             !/*in        130*/
!     prolongs the life of "flying dutchmen" significantly, so run              !/*in        131*/
!     50 or so to determine proper time-limits necessary                        !/*in        132*/
!                                                                               !/*in        133*/
!     for kaon branching ratios, ainput set brkain to                           !/*in        134*/
!     either 1.,0., or brkatr(ue), and zipper then has to read                  !/*in        135*/
!     those values to determine how to renorm the sigmas it                     !/*in        136*/
!      calculates                                                               !/*in        137*/
!         (ap14) has modified the kaon production to make it like               !/*in        138*/
!         the pi-production at low energies:  apin has a variable,              !/*in        139*/
!         "kaskew" that is either 0 or dp1  if 1, kaon branch is always         !/*in        140*/
!         taken when the pbar annihilates.  run sufficient cascades             !/*in        141*/
!         with "kaskew" = 0 to get good statistics,  because                    !/*in        142*/
!         you must renormalize the cross-section by hand                        !/*in        143*/
!                                                                               !/*in        144*/
!   this version (ap14) has a modification similar to                           !/*in        145*/
!    "kaskew", for pion-production at low energies.  in the input               !/*in        146*/
!   file (apin), the variable "piskew" is set to 1.(0.=default).  when          !/*in        147*/
!   piskew is 1, at the beginning of each cascade, the internal                 !/*in        148*/
!   variable "pibran" is set to "true", and in "ainter", if pibran is           !/*in        149*/
!   true, the nucleon-nucleon reaction is always assumed to take                !/*in        150*/
!   the pi-production branch, at which time "pibran" is set false               !/*in        151*/
!   for the rest of the cascade.                                                !/*in        152*/
!      note well----  sufficient statistics must be run with piskew=0.          !/*in        153*/
!                     to get good statistics. normalization is auto-            !/*in        154*/
!                     matically taken care of, as cascade# (poe1) is            !/*in        155*/
!                     always incremented, before the unwanted                   !/*in        156*/
!                     cascades are cancelled                                    !/*in        157*/
!                                                                               !/*in        158*/
!         this version (ap13) has modified the kaon-reaction ("ptotkl")         !/*in        159*/
!         cross-sections to be more faithful to the data (cern-hera 75)         !/*in        160*/
!         it remains to modify "panglk" to generate the proper elastic          !/*in        161*/
!         scattering differential cross-sections  (24 apr 83 / mrc)             !/*in        162*/
!                                                                               !/*in        163*/
!            this version (ap10) is designed to include the logic               !/*in        164*/
!            to allow kaon-production in pbar annihilations.                    !/*in        165*/
!            there is a flag in the "apin" file, "brkain"                       !/*in        166*/
!            which you set to either 0.03 or 1.0 for normal or                  !/*in        167*/
!            exclusive branching to that channel.                               !/*in        168*/
!                                                                               !/*in        169*/
!           in this (ap9) and following versions will be logic to               !/*in        170*/
!          allow a folded-yukawa momentum distribution at each                  !/*in        171*/
!          potential-step, thereby properly checking the pauli-ex-              !/*in        172*/
!          clusion principle.                                                   !/*in        173*/
!                                                                               !/*in        174*/
!         from ap9 onwards, use is made of the "historian"                      !/*in        175*/
!         utility to keep the program compatible with ltss/ctss.  to            !/*in        176*/
!         use it, mass in (ap12h,ap12c,apin) from cfs.  if you are              !/*in        177*/
!         on ctss, trans each of them, and then fred ap12c--changing lin        !/*in        178*/
!         2 from "*df,cdc*" to "*df,cray". the historian will then auto-        !/*in        179*/
!         matically leave out the "level 2" statements, and do other            !/*in        180*/
!         little things that have to do with 60/64 bits in the random #         !/*in        181*/
!         generators.  then, assuming you are in a ctl enviornment,             !/*in        182*/
!         simply type   "hcl4 ap12"  (or "hcl5 ap12" if you want to use         !/*in        183*/
!         the ftn5 compiler on ltss.   on the cray, you type only               !/*in        184*/
!         "hcl ap12".)                                                          !/*in        185*/
!                                                                               !/*in        186*/
!       otherwise, you need to type                                             !/*in        187*/
!     historn (i=ap12h,n=ap12ol,c=0)                                            !/*in        188*/
!     historn (i=ap12c,p=ap12ol,f,c=ap12)                                       !/*in        189*/
!     ftn (i=ap12,nall [,go])                                                   !/*in        190*/
!                                                                               !/*in        191*/
!     ==================================================================        !/*in        192*/
!     inisbl                                                                    !/*in        193*/
!     purpose -                                                                 !/*in        194*/
!           to initialize the parameters for execution of isabel                !/*in        195*/
!           routines within hetc                                                !/*in        196*/
!                                                                               !/*in        197*/
!  allowed model values for hetc application:                                   !/*in        198*/
!    model=1: isobar with refr and refl, no cutoff for isobars                  !/*in        199*/
!    model=2: isobar with refr and refl, with cutoff for isobars                !/*in        200*/
!    model=3: isobar with no refr nor refl, with cutoff for isobars             !/*in        201*/
!    model=4: isobar with refr and refl, with 25 mev pion potential,            !/*in        202*/
!        no cutoff for isobars                                                  !/*in        203*/
!    model=5: isobar with refr and refl, with 25 mev pion potential,            !/*in        204*/
!        with cutoff for isobars                                                !/*in        205*/
!    model=6: isobar with no refr nor refl, with 25 mev pion potential,         !/*in        206*/
!        with cutoff for isobars                                                !/*in        207*/
!                                                                               !/*in        208*/
!  allowed iden values for hetc application:                                    !/*in        209*/
!    iden=0, myer prescription for (8) densities                                !/*in        210*/
!    iden=1, old (isobar) prescription for (8) densities                        !/*in        211*/
!    iden=2, folded-yukawa prescription for (16) densities                      !/*in        212*/
!     ==================================================================        !/*in        213*/
#include <lcs/inout.h>                                                          /*!in        214*/
#include <lcs/vegas.h>                                                          /*!in        215*/
      data imisd /0/                                                            !/*in        216*/
!                                                                               !/*in        217*/
!      these new variables [24jul84 yy] are designed to                         !/*in        218*/
!      properly cumulate things even when a cascade is                          !/*in        219*/
!      cancelled midway through an event                                        !/*in        220*/
!                                                                               !/*in        221*/
!-----------------------------------------------------------------------        !/*in        222*/
!      unfold ICHOIC                                                            !/*in        223*/
      is=sign(1,ichoic)                                                         !/*in        224*/
      ichoic=abs(ichoic)                                                        !/*in        225*/
      model=mod(ichoic,10)                                                      !/*in        226*/
      iden=mod(ichoic/10,10)                                                    !/*in        227*/
      ntdel=mod(ichoic/100,10)                                                  !/*in        228*/
      ipauli=mod(ichoic/1000,10)                                                !/*in        229*/
      ipauli=1+is*ipauli                                                        !/*in        230*/
#ifdef LCS_PRINT                                                                /*!in        231*/
      write (io,40) model,iden,ntdel,ipauli                                     !/*in        232*/
   40 format (10x,'ISABEL parameters:'/10x,'model=',i3,'  iden=',i3,            !/*in        233*/
     & '  ntdel=',i3,'  ipauli=',i3)                                            !/*in        234*/
#endif                                                                          /*!in        235*/
!-----------------------------------------------------------------------        !/*in        236*/
      if (model.eq.0.or.model.gt.6) model=3                                     !/*in        237*/
      model=model+70                                                            !/*in        238*/
      mprin=0                                                                   !/*in        239*/
!          evap. output files 0=no output, 1=output target,                     !/*in        240*/
!                                          2=output target + projectile         !/*in        241*/
!          pauli prin. 0=no, 1=partial, 2=full                                  !/*in        242*/
!         nopot=0 = normal.  nopot=1 ==> no potentials anywhere                 !/*in        243*/
!-----now boost the projectile particle to the lab frame                        !/*in        244*/
      nopot=0                                                                   !/*in        245*/
!              depth of pbar potential [mev]                                    !/*in        246*/
      vap=75.d0                                                                 !/*in        247*/
!           set to no-skewing of production cross-sections                      !/*in        248*/
      kaskew=0                                                                  !/*in        249*/
!          rearrangement flag -1=fast, no check on holes                        !/*in        250*/
!                              0=fast,      "    "   "                          !/*in        251*/
!                              1=slow,      "    "   "                          !/*in        252*/
      np=1                                                                      !/*in        253*/
!          type of distance restr (1=1.16fm, 2=3root(vol),3=3root(rho))         !/*in        254*/
      knp=1                                                                     !/*in        255*/
      gapl=1.1d0                                                                !/*in        256*/
!              depth of pion-potential [mev]                                    !/*in        257*/
      vpion=dp0                                                                 !/*in        258*/
      if (model.ge.74) vpion=25.d0                                              !/*in        259*/
!         isonsw = 0 ==> isobar ch-ex allowed, .eq.1, ==> not allowed           !/*in        260*/
      isonsw=0                                                                  !/*in        261*/
!         gamiso .eq. 0. ==> ginnocchi energy-dependent width,                  !/*in        262*/
!                .ne. 0. ==> [mev] constant width                               !/*in        263*/
      gamiso=dp0                                                                !/*in        264*/
!              # of time-steps per mean-free-time (mfp/v)                       !/*in        265*/
      timef=2.d1                                                                !/*in        266*/
!              # of time-steps between cas-cas interactions                     !/*in        267*/
!              maximum distance between cas-cas particles [fm]                  !/*in        268*/
      rcas=dp3                                                                  !/*in        269*/
!       density prescription 0=myer w-s, 1=isobar, 2=folded yukawa              !/*in        270*/
!               3=input density moments but th-fm wig fcn,                      !/*in        271*/
!               4=input moments for density and wign fcn                        !/*in        272*/
      ibrad=0                                                                   !/*in        273*/
      if (iden.gt.2) then                                                       !/*in        274*/
        ibrad=1                                                                 !/*in        275*/
        iden=iden-3                                                             !/*in        276*/
      endif                                                                     !/*in        277*/
!             radial deltas for iden=0,1                                        !/*in        278*/
!             note: dx(1) is subtracted from r*a**.3 to get max radius          !/*in        279*/
      dx(1)=-2.5d0                                                              !/*in        280*/
      dx(2)=dp1                                                                 !/*in        281*/
      do 10 i=3,7                                                               !/*in        282*/
   10 dx(i)=0.6d0                                                               !/*in        283*/
      dx(8)=dp1                                                                 !/*in        284*/
!            radial deltas for iden=2 (folded yukawa density)                   !/*in        285*/
!            these assure that the pbar potl steps are <=30 mev                 !/*in        286*/
!            if iden > 2, use same steps to get moments                         !/*in        287*/
!         note:  dxp(1) is subtracted from r*a**.3 to get max radius            !/*in        288*/
      dxp(1)=-3.5d0                                                             !/*in        289*/
      dxp(2)=dp1                                                                !/*in        290*/
      dxp(3)=.9d0                                                               !/*in        291*/
      dxp(4)=.6d0                                                               !/*in        292*/
      dxp(5)=.3d0                                                               !/*in        293*/
      dxp(6)=0.2d0                                                              !/*in        294*/
      dxp(7)=0.2d0                                                              !/*in        295*/
      dxp(8)=.3d0                                                               !/*in        296*/
      dxp(9)=0.3d0                                                              !/*in        297*/
      do 20 i=10,16                                                             !/*in        298*/
   20 dxp(i)=0.4d0                                                              !/*in        299*/
!            following form reproduces effects prior to 15/feb/82 (mrc)         !/*in        300*/
!            also set ieght =8 after stmt# 10                                   !/*in        301*/
!     data dxp /-3.5,8*2.0,1.25,5*0.7,1.25/                                     !/*in        302*/
!                                                                               !/*in        303*/
      poe1=dp0                                                                  !/*in        304*/
      do 30 i=1,11                                                              !/*in        305*/
   30 na(i)=0                                                                   !/*in        306*/
      kcas=0                                                                    !/*in        307*/
      kcan=0                                                                    !/*in        308*/
!                                                                               !/*in        309*/
!     ----------------------------                                              !/*in        310*/
      call ainput (model)                                                       !/*in        311*/
!     ----------------------------                                              !/*in        312*/
!                                                                               !/*in        313*/
!        pmcdgi initializes the cumulative distribution-                        !/*in        314*/
!        functions for pbar annihilation into pions                             !/*in        315*/
!                                                                               !/*in        316*/
!     ---------------------------------                                         !/*in        317*/
      call pmcdgi                                                               !/*in        318*/
!     ---------------------------------                                         !/*in        319*/
      return                                                                    !/*in        320*/
!                                                                               !/*in        321*/
      end                                                                       !/*in        322*/

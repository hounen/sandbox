!#ident GLOBAL5_cm.F from deck mgd                                              !/*mgd         1*/
      module global_data                                                        !/*mgd         2*/
!                                                                               !/*mgd         3*/
      use global_tables                                                         !/*mgd         4*/
      use global_static                                                         !/*mgd         5*/
      use thread                                                                !/*mgdk        1*/
      implicit none                                                             !/*mgd         6*/
      save                                                                      !/*mgd         7*/
      public                                                                    !/*mgd         8*/
!                                                                               !/*mgd         9*/
! **************** dynamically allocated variables *********************        !/*mgd        10*/
!                                                                               !/*mgd        11*/
!        fixed dynamically allocated common (fdac) - reals.                     !/*mgd        12*/
!                                                                               !/*mgd        13*/
      real(kindr), allocatable :: ara(:)                                        !/*mgdl        1*/
      real(kindr), allocatable :: asm(:,:)                                      !/*mgd        14*/
      real(kindr), allocatable :: asp(:)                                        !/*mgd        15*/
      real(kindr), allocatable :: awc(:)                                        !/*mgd        16*/
      real(kindr), allocatable :: awn(:)                                        !/*mgd        17*/
      real(kindr), allocatable :: cmg(:)                                        !/*mgd        18*/
      real(kindr), allocatable :: den(:)                                        !/*mgdl        2*/
      real(kindr), allocatable :: dptb(:,:)                                     !/*mgd        19*/
      real(kindr), allocatable :: drs(:,:)                                      !/*mgd        20*/
      real(kindr), allocatable :: dxcp(:,:,:)                                   !/*mgd        21*/
      real(kindr), allocatable :: eaa(:)                                        !/*mgd        22*/
      real(kindr), allocatable :: ear(:)                                        !/*mgd        23*/
      real(kindr), allocatable :: eba(:,:)                                      !/*mgd        24*/
      real(kindr), allocatable :: ebd(:,:)                                      !/*mgd        25*/
      real(kindr), allocatable :: ebl(:)                                        !/*mgd        26*/
      real(kindr), allocatable :: ebt(:,:)                                      !/*mgd        27*/
      real(kindr), allocatable :: ech(:,:,:)                                    !/*mgd        28*/
      real(kindr), allocatable :: edg(:)                                        !/*mgd        29*/
      real(kindr), allocatable :: eee(:,:)                                      !/*mgd        30*/
      real(kindr), allocatable :: eek(:)                                        !/*mgd        31*/
      real(kindr), allocatable :: egg(:,:)                                      !/*mgd        32*/
      real(kindr), allocatable :: elp(:,:)                                      !/*mgd        33*/
      real(kindr), allocatable :: esa(:)                                        !/*mgd        34*/
      real(kindr), allocatable :: ewwg(:)                                       !/*mgd        35*/
      real(kindr), allocatable :: fim(:,:)                                      !/*mgd        36*/
      real(kindr), allocatable :: flc(:)                                        !/*mgd        37*/
      real(kindr), allocatable :: fme(:)                                        !/*mgdl        3*/
      real(kindr), allocatable :: fmg(:)                                        !/*mgd        38*/
      real(kindr), allocatable :: for(:,:)                                      !/*mgd        39*/
      real(kindr), allocatable :: frc(:)                                        !/*mgd        40*/
      real(kindr), allocatable :: fst(:)                                        !/*mgd        41*/
      real(kindr), allocatable :: ftt(:)                                        !/*mgd        42*/
      real(kindr), allocatable :: gmg(:)                                        !/*mgd        43*/
      real(kindr), allocatable :: gvl(:)                                        !/*mgd        44*/
      real(kindr), allocatable :: gwt(:)                                        !/*mgd        45*/
      real(kindr), allocatable :: pbr(:)                                        !/*mgd        46*/
      real(kindr), allocatable :: pbt(:)                                        !/*mgd        47*/
      real(kindr), allocatable :: pkn(:)                                        !/*mgd        48*/
      real(kindr), allocatable :: pmg(:)                                        !/*mgd        49*/
      real(kindr), allocatable :: pnt(:)                                        !/*mgd        50*/
      real(kindr), allocatable :: pru(:)                                        !/*mgd        51*/
      real(kindr), allocatable :: pxr(:)                                        !/*mgd        52*/
      real(kindr), allocatable :: qav(:,:)                                      !/*mgd        53*/
      real(kindr), allocatable :: qax(:,:)                                      !/*mgd        54*/
      real(kindr), allocatable :: qcn(:)                                        !/*mgd        55*/
      real(kindr), allocatable :: rho(:)                                        !/*mgdl        4*/
      real(kindr), allocatable :: rptb(:)                                       !/*mgd        56*/
      real(kindr), allocatable :: scf(:)                                        !/*mgd        57*/
      real(kindr), allocatable :: smg(:)                                        !/*mgd        58*/
      real(kindr), allocatable :: spf(:,:)                                      !/*mgd        59*/
      real(kindr), allocatable :: sqq(:,:)                                      !/*mgd        60*/
      real(kindr), allocatable :: sso(:)                                        !/*mgd        61*/
      real(kindr), allocatable :: tbt(:)                                        !/*mgd        62*/
      real(kindr), allocatable :: tds(:)                                        !/*mgd        63*/
      real(kindr), allocatable :: tmp(:)                                        !/*mgd        64*/
      real(kindr), allocatable :: trf(:,:)                                      !/*mgd        65*/
      real(kindr), allocatable :: tth(:)                                        !/*mgd        66*/
      real(kindr), allocatable :: vcl(:,:,:)                                    !/*mgd        67*/
      real(kindr), allocatable :: vec(:,:)                                      !/*mgd        68*/
      real(kindr), allocatable :: vol(:)                                        !/*mgdl        5*/
      real(kindr), allocatable :: wgm(:)                                        !/*mgd        69*/
      real(kindr), allocatable :: wgma(:)                                       !/*mgd        70*/
      real(kindr), allocatable :: wwe(:)                                        !/*mgd        71*/
      real(kindr), allocatable :: wwf(:)                                        !/*mgd        72*/
      real(kindr), allocatable :: wwk(:)                                        !/*mgd        73*/
      real(kindr), allocatable :: xnm(:)                                        !/*mgd        74*/
      real(kindr), allocatable :: xse85(:,:)                                    !/*mgd        75*/
!                                                                               !/*mgd        76*/
!        mcnpx                                                                  !/*mgd        77*/
      real(kindr), allocatable :: aa9(:)                                        !/*mgd        78*/
      real(kindr), allocatable :: arg(:)                                        !/*mgd        79*/
      real(kindr), allocatable :: ava(:)                                        !/*mgd        80*/
      real(kindr), allocatable :: avz(:)                                        !/*mgd        81*/
      real(kindr), allocatable :: emaxt(:)                                      !/*mgd        82*/
#ifdef MESHTAL                                                                  /*!mgd        83*/
      real(kindr), allocatable :: emsh(:)                                       !/*mgd        84*/
#endif                                                                          /*!mgd        85*/
#ifdef SPABI                                                                    /*!mgd        86*/
      real(kindr), allocatable :: energies_and_params(:)                        !/*mgd        87*/
#endif                                                                          /*!mgd        88*/
#ifdef CEM                                                                      /*!mgd        89*/
      real(kindr), allocatable :: epnmin(:)                                     !/*mgd        90*/
#endif                                                                          /*!mgd        91*/
      real(kindr), allocatable :: esxln(:,:)                                    !/*mgd        92*/
      real(kindr), allocatable :: esxlp(:,:)                                    !/*mgd        93*/
      real(kindr), allocatable :: esxmp(:)                                      !/*mgd        94*/
      real(kindr), allocatable :: fmf(:)                                        !/*mgd        95*/
      real(kindr), allocatable :: hsigg(:,:)                                    !/*mgd        96*/
      real(kindr), allocatable :: pcap(:)                                       !/*mgd        97*/
      real(kindr), allocatable :: sigg(:,:)                                     !/*mgd        98*/
      real(kindr), allocatable :: sigmx(:,:)                                    !/*mgd        99*/
      real(kindr), allocatable :: xsiso(:,:)                                    !/*mgd       100*/
      real(kindr), allocatable :: zz9(:)                                        !/*mgd       101*/
!                                                                               !/*mgd       102*/
!        fixed dynamically allocated common (fdac) - integers.                  !/*mgd       103*/
!                                                                               !/*mgd       104*/
      integer(kindi), allocatable :: idna(:)                                    !/*mgd       105*/
      integer(kindi), allocatable :: idne(:)                                    !/*mgd       106*/
      integer(kindi), allocatable :: idns(:)                                    !/*mgd       107*/
      integer(kindi), allocatable :: idnt(:)                                    !/*mgd       108*/
      integer(kindi), allocatable :: ipan(:)                                    !/*mgd       109*/
      integer(kindi), allocatable :: iptal(:,:,:)                               !/*mgd       110*/
      integer(kindi), allocatable :: iptb(:,:)                                  !/*mgd       111*/
      integer(kindi), allocatable :: iss(:,:)                                   !/*mgd       112*/
      integer(kindi), allocatable :: itds(:)                                    !/*mgd       113*/
      integer(kindi), allocatable :: ixl(:,:)                                   !/*mgd       114*/
      integer(kindi), allocatable :: ixs(:,:,:)                                 !/*mgd       115*/
      integer(kindi), allocatable :: iza(:)                                     !/*mgd       116*/
      integer(kindi), allocatable :: izn(:)                                     !/*mgd       117*/
      integer(kindi), allocatable :: jasq(:)                                    !/*mgd       118*/
      integer(kindi), allocatable :: jasr(:,:)                                  !/*mgd       119*/
      integer(kindi), allocatable :: jasw(:)                                    !/*mgd       120*/
      integer(kindi), allocatable :: jmd(:)                                     !/*mgd       121*/
      integer(kindi), allocatable :: jptal(:,:)                                 !/*mgd       122*/
      integer(kindi), allocatable :: jscn(:)                                    !/*mgd       123*/
      integer(kindi), allocatable :: jsq(:)                                     !/*mgd       124*/
      integer(kindi), allocatable :: jss(:)                                     !/*mgd       125*/
      integer(kindi), allocatable :: jtf(:,:)                                   !/*mgd       126*/
      integer(kindi), allocatable :: jun(:)                                     !/*mgd       127*/
      integer(kindi), allocatable :: jvc(:)                                     !/*mgd       128*/
      integer(kindi), allocatable :: jxs(:,:)                                   !/*mgd       129*/
      integer(kindi), allocatable :: kcp(:)                                     !/*mgd       130*/
      integer(kindi), allocatable :: kmm(:)                                     !/*mgd       131*/
      integer(kindi), allocatable :: ksc(:)                                     !/*mgd       132*/
      integer(kindi), allocatable :: ksd(:,:)                                   !/*mgd       133*/
      integer(kindi), allocatable :: ksm(:)                                     !/*mgd       134*/
      integer(kindi), allocatable :: kst(:)                                     !/*mgd       135*/
      integer(kindi), allocatable :: ksu(:)                                     !/*mgd       136*/
      integer(kindi), allocatable :: ktp(:,:)                                   !/*mgd       137*/
      integer(kindi), pointer :: kxs(:)                                         !/*mgd       138*/
      integer(kindi), pointer :: laf(:,:)                                       !/*mgd       139*/
      integer(kindi), allocatable :: lat(:,:)                                   !/*mgd       140*/
      integer(kindi), allocatable :: lca(:)                                     !/*mgd       141*/
      integer(kindi), allocatable :: lft(:,:)                                   !/*mgd       142*/
      integer(kindi), allocatable :: lja(:)                                     !/*mgd       143*/
      integer(kindi), allocatable :: lme(:,:)                                   !/*mgd       144*/
      integer(kindi), allocatable :: lmn(:)                                     !/*mgd       145*/
      integer(kindi), allocatable :: lmt(:)                                     !/*mgd       146*/
      integer(kindi), allocatable :: locct(:,:)                                 !/*mgd       147*/
      integer(kindi), allocatable :: locph(:)                                   !/*mgd       148*/
      integer(kindi), allocatable :: locst(:,:)                                 !/*mgd       149*/
      integer(kindi), allocatable :: lsc(:)                                     !/*mgd       150*/
      integer(kindi), allocatable :: mat(:)                                     !/*mgd       151*/
      integer(kindi), allocatable :: mazp(:,:)                                  !/*mgd       152*/
      integer(kindi), pointer :: mazu(:)                                        !/*mgd       153*/
      integer(kindi), allocatable :: mbd(:)                                     !/*mgd       154*/
      integer(kindi), allocatable :: mbi(:)                                     !/*mgd       155*/
      integer(kindi), allocatable :: mfl(:,:)                                   !/*mgd       156*/
      integer(kindi), pointer :: ncl(:)                                         !/*mgd       157*/
      integer(kindi), allocatable :: ngmfl(:)                                   !/*mgd       158*/
      integer(kindi), allocatable :: nhtfl(:)                                   !/*mgd       159*/
      integer(kindi), allocatable :: nmt(:)                                     !/*mgd       160*/
      integer(kindi), allocatable :: npq(:)                                     !/*mgd       161*/
      integer(kindi), allocatable :: nptb(:)                                    !/*mgd       162*/
      integer(kindi), allocatable :: nsb(:)                                     !/*mgd       163*/
      integer(kindi), pointer :: nsfm(:)                                        !/*mgd       164*/
      integer(kindi), allocatable :: nslr(:,:)                                  !/*mgd       165*/
      integer(kindi), allocatable :: nty(:)                                     !/*mgd       166*/
      integer(kindi), allocatable :: nxs(:,:)                                   !/*mgd       167*/
!                                                                               !/*mgd       168*/
!        mcnpx                                                                  !/*mgd       169*/
      integer(kindi), allocatable :: iazdex(:)                                  !/*mgd       170*/
      integer(kindi), allocatable :: kpptb(:,:)                                 !/*mgd       171*/
      integer(kindi), allocatable :: mel(:)                                     !/*mgd       172*/
      integer(kindi), allocatable :: nel(:)                                     !/*mgd       173*/
#ifdef HISTP                                                                    /*!mgd       174*/
      integer(kindi), allocatable :: nhw(:)                                     !/*mgd       175*/
#endif                                                                          /*!mgd       176*/
#ifdef CEM                                                                      /*!mgd       177*/
      integer(kindi), allocatable :: ipnf(:)                                    !/*mgd       178*/
#endif                                                                          /*!mgd       179*/
!                                                                               !/*mgd       180*/
!        variable dynamically allocated common (vdac)                           !/*mgd       181*/
!        reals.                                                                 !/*mgd       182*/
      real(kindr), allocatable :: fso(:)                                        !/*mgd       186*/
      real(kindr), allocatable :: rkpl(:,:)                                     !/*mgd       188*/
      real(kindr), allocatable :: tfc(:,:,:)                                    !/*mgd       189*/
!        reals dimensioned by number of tasks.                                  !/*mgd       195*/
      real(kindr), allocatable :: ddm(:,:)                                      !/*mgd       196*/
      real(kindr), allocatable :: ddn(:,:)                                      !/*mgd       197*/
      real(kindr), allocatable :: dec(:,:)                                      !/*mgd       198*/
      real(kindr), allocatable :: dxc(:,:)                                      !/*mgd       199*/
      real(kindr), allocatable :: dxd(:,:,:)                                    !/*mgd       200*/
      real(kindr), allocatable :: febl(:,:)                                     !/*mgd       201*/
      real(kindr), allocatable :: flx(:)                                        !/*mgd       202*/
      real(kindr), allocatable :: wwfa(:)                                       !/*mgd       203*/
      real(kindr), allocatable :: swwfa(:)                                      !/*mgd       204*/
      real(kindr), allocatable :: pac(:,:,:)                                    !/*mgd       205*/
      real(kindr), allocatable :: pan(:,:,:)                                    !/*mgd       206*/
      real(kindr), allocatable :: pcc(:,:)                                      !/*mgd       207*/
      real(kindr), allocatable :: pwb(:,:,:)                                    !/*mgd       208*/
      real(kindr), allocatable :: sump(:)                                       !/*mgd       209*/
      real(kindr), allocatable :: wns(:,:)                                      !/*mgd       210*/
!                                                                               !/*mgd       211*/
!        variable dynamically allocated common (vdac)                           !/*mgd       212*/
!        integers.                                                              !/*mgd       213*/
      integer(kindi), allocatable :: jfq(:,:)                                   !/*mgd       214*/
      integer(kindi), allocatable :: npsw(:)                                    !/*mgd       215*/
      integer(kindi), allocatable :: nsl(:,:)                                   !/*mgd       216*/
      integer(kindi), allocatable :: ntbb(:,:)                                  !/*mgd       217*/
      integer(kindi), allocatable :: lfcl(:)                                    !/*mgd       218*/
!        integers dimensioned by number of tasks.                               !/*mgd       219*/
      integer(kindi), allocatable :: isef(:,:)                                  !/*mgd       220*/
      integer(kindi), allocatable :: maze(:)                                    !/*mgd       221*/
      integer(kindi), allocatable :: ndpf(:,:)                                  !/*mgd       222*/
      integer(kindi), allocatable :: ndr(:)                                     !/*mgd       223*/
      integer(kindi), allocatable :: laj(:)                                     !/*mgd       224*/
      integer(kindi), allocatable :: lcaj(:)                                    !/*mgd       225*/
      integer(kindi), allocatable :: lse(:)                                     !/*mgd       226*/
!                                                                               !/*mgd       227*/
!        ephemeral dynamically allocated common (edac).                         !/*mgd       228*/
!        unswept variables                                                      !/*mgd       229*/
      real(kindr), allocatable :: ptr(:)                                        !/*mgd       230*/
      real(kindr), allocatable :: pts(:)                                        !/*mgd       231*/
      real(kindr), allocatable :: rng(:,:)                                      !/*mgd       232*/
      real(kindr), allocatable :: scr(:)                                        !/*mgd       233*/
      integer(kindi), allocatable :: ixc(:,:)                                   !/*mgd       234*/
      integer(kindi), allocatable :: jcond(:)                                   !/*mgd       235*/
      integer(kindi), allocatable :: jemi(:)                                    !/*mgd       236*/
      integer(kindi), allocatable :: jmt(:)                                     !/*mgd       237*/
      integer(kindi), allocatable :: kdup(:,:)                                  !/*mgd       238*/
      integer(kindi), allocatable :: kmt(:,:)                                   !/*mgd       239*/
      integer(kindi), allocatable :: kxd(:)                                     !/*mgd       240*/
      integer(kindi), allocatable :: lbb(:)                                     !/*mgd       241*/
!        swept variables                                                        !/*mgd       242*/
      real(kindr), allocatable :: drc(:,:)                                      !/*mgd       243*/
      real(kindr), allocatable :: fdd(:,:)                                      !/*mgd       244*/
#ifdef MESHTAL                                                                  /*!mgd       245*/
      real(kindr), allocatable :: gdata(:)                                      !/*mgd       246*/
      real(kindr), allocatable :: gd1(:)                                        !/*mgd       247*/
      real(kindr), allocatable :: gd2(:)                                        !/*mgd       248*/
      real(kindr), allocatable :: gd3(:)                                        !/*mgd       249*/
#endif                                                                          /*!mgd       250*/
      real(kindr), allocatable :: pik(:)                                        !/*mgd       251*/
      real(kindr), allocatable :: ptb(:,:)                                      !/*mgd       252*/
      real(kindr), allocatable :: rtc(:,:)                                      !/*mgd       253*/
      real(kindr), allocatable :: tgp(:)                                        !/*mgd       254*/
      integer(kindi), allocatable :: iafg(:)                                    !/*mgd       255*/
      integer(kindi), allocatable :: ifl(:)                                     !/*mgd       256*/
      integer(kindi), allocatable :: ipac2(:)                                   !/*mgd       257*/
      integer(kindi), allocatable :: jfl(:)                                     !/*mgd       258*/
      integer(kindi), allocatable :: jft(:)                                     !/*mgd       259*/
      integer(kindi), allocatable :: jptb(:)                                    !/*mgd       260*/
      integer(kindi), allocatable :: ktc(:,:)                                   !/*mgd       261*/
      integer(kindi), allocatable :: nmcp(:,:)                                  !/*mgd       262*/
!                                                                               !/*mgd       263*/
!        dynamically allocated variables for tallies and tfc statistics.        !/*mgd       264*/
!                                                                               !/*mgd       265*/
      real(kindr), allocatable :: shsd(:,:)                                     !/*mgd       266*/
      real(kindr), allocatable :: stt(:,:)                                      !/*mgd       267*/
      real(kindr), pointer :: tal(:)                                            !/*mgd       268*/
      integer(kindi), allocatable :: nhsd(:,:)                                  !/*mgd       269*/
!                                                                               !/*mgd       270*/
!        dynamically allocated variables for bank and cross sections.           !/*mgd       271*/
!                                                                               !/*mgd       272*/
      real(kindr), allocatable :: gbnk(:)                                       !/*mgd       273*/
      real(kindr), allocatable :: exs(:)                                        !/*mgd       274*/
#ifdef XS64                                                                     /*!mgd       275*/
      real(kindr), pointer :: xss(:)                                            !/*mgd       276*/
#else                                                                           /*!mgd       277*/
      real, pointer :: xss(:)                                                   !/*mgd       278*/
#endif                                                                          /*!mgd       279*/
      integer(kindi), allocatable :: ibnk(:)                                    !/*mgd       280*/
!                                                                               !/*mgd       281*/
!        dynamically allocated common for imcn.                                 !/*mgd       282*/
!        before volume                                                          !/*mgd       283*/
      real(kindr), allocatable :: awt(:)                                        !/*mgd       284*/
      real(kindr), allocatable :: bbv(:)                                        !/*mgd       285*/
      real(kindr), allocatable :: prb(:)                                        !/*mgd       286*/
      real(kindr), allocatable :: rtp(:)                                        !/*mgd       287*/
      real(kindr), allocatable :: sfb(:)                                        !/*mgd       288*/
      integer(kindi), allocatable :: ipnt(:,:,:)                                !/*mgd       289*/
      integer(kindi), allocatable :: jtr(:)                                     !/*mgd       290*/
      integer(kindi), allocatable :: kaw(:)                                     !/*mgd       291*/
      integer(kindi), allocatable :: kdr(:)                                     !/*mgd       292*/
      integer(kindi), allocatable :: ktr(:)                                     !/*mgd       293*/
      integer(kindi), allocatable :: lxd(:,:)                                   !/*mgd       294*/
      integer(kindi), allocatable :: mfm(:)                                     !/*mgd       295*/
      integer(kindi), allocatable :: nlv(:)                                     !/*mgd       296*/
!        during volume                                                          !/*mgd       297*/
      real(kindr), allocatable :: aras(:,:)                                     !/*mgd       298*/
      real(kindr), allocatable :: atsa(:,:)                                     !/*mgd       299*/
      real(kindr), allocatable :: rscrn(:,:)                                    !/*mgd       300*/
      real(kindr), allocatable :: rsint(:,:)                                    !/*mgd       301*/
      real(kindr), allocatable :: scfq(:,:)                                     !/*mgd       302*/
      real(kindr), allocatable :: vols(:,:)                                     !/*mgd       303*/
      integer(kindi), allocatable :: iint(:)                                    !/*mgd       304*/
      integer(kindi), allocatable :: icrn(:,:)                                  !/*mgd       305*/
      integer(kindi), allocatable :: ljav(:)                                    !/*mgd       306*/
      integer(kindi), allocatable :: ljsv(:)                                    !/*mgd       307*/
      integer(kindi), allocatable :: lsat(:)                                    !/*mgd       308*/
!                                                                               !/*mgd       309*/
!        dynamically allocated common for plot.                                 !/*mgd       310*/
!                                                                               !/*mgd       311*/
      real(kindr), allocatable :: amx(:,:,:)                                    !/*mgd       312*/
      real(kindr), allocatable :: coe(:,:,:)                                    !/*mgd       313*/
      real(kindr), allocatable :: crs(:)                                        !/*mgd       314*/
      real(kindr), allocatable :: plb(:)                                        !/*mgd       315*/
      real(kindr), allocatable :: qmx(:,:,:,:)                                  !/*mgd       316*/
      real(kindr), pointer :: zst(:)                                            !/*mgd       317*/
      integer(kindi), allocatable :: jst(:,:)                                   !/*mgd       318*/
      integer(kindi), allocatable :: kcl(:,:)                                   !/*mgd       319*/
      integer(kindi), allocatable :: kfm(:)                                     !/*mgd       320*/
      integer(kindi), allocatable :: lcl(:)                                     !/*mgd       321*/
      integer(kindi), allocatable :: lsg(:)                                     !/*mgd       322*/
      integer(kindi), allocatable :: ncs(:)                                     !/*mgd       323*/
!                                                                               !/*mgd       324*/
!        dynamically allocated common for mcplot.                               !/*mgd       325*/
!                                                                               !/*mgd       326*/
      real(kindr), allocatable :: ab1(:)                                        !/*mgd       327*/
      real(kindr), allocatable :: ab2(:)                                        !/*mgd       328*/
      real(kindr), allocatable :: erb(:)                                        !/*mgd       329*/
      real(kindr), allocatable :: ord(:)                                        !/*mgd       330*/
      real(kindr), allocatable :: xcc(:)                                        !/*mgd       331*/
      real(kindr), allocatable :: ycc(:)                                        !/*mgd       332*/
      real, allocatable :: xrr(:)                                               !/*mgd       333*/
      real, allocatable :: yrr(:)                                               !/*mgd       334*/
      integer(kindi), allocatable :: kxspie(:)                                  !/*mgd       335*/
      integer(kindi), allocatable :: kxspen(:)                                  !/*mgd       336*/
      integer(kindi), allocatable :: kxspxs(:)                                  !/*mgd       337*/
      integer(kindi), allocatable :: kxspnx(:)                                  !/*mgd       338*/
!                                                                               !/*mgd       339*/
      contains                                                                  !/*mgd       340*/
      subroutine dynamic_allocate                                               !/*mgd       341*/
!        allocate and initialize dynamic variables.                             !/*mgd       342*/
      use allocate_memory                                                       !/*mgd       343*/
      integer(kindi) :: aloc,kc,ke,mt                                           !/*mgd       344*/
      external expire                                                           !/*mgd       345*/
!                                                                               !/*mgd       346*/
      mt=1                                                                      !/*mgd       347*/
#if defined(MULTT) || defined(MULTP)                                            /*!mgd       348*/
      mt=mtasks+1                                                               !/*mgd       349*/
#endif                                                                          /*!mgd       350*/
      ke=max(kpt1(3),kpt1(2)*(1-ides1))                                         !/*mgd       351*/
!                                                                               !/*mgd       352*/
!        allocate fdac real variables.                                          !/*mgd       353*/
      if(allocated(ara))deallocate (ara)                                        !/*mgdl        6*/
      allocate (ara(mxj1), stat=aloc)                                           !/*mgdl        7*/
      if(aloc.ne.0)go to 10                                                     !/*mgdl        8*/
      ara=0.                                                                    !/*mgdl        9*/
      if(allocated(asm))deallocate (asm)                                        !/*mgd       354*/
      allocate (asm(3,max(nwma1(1),nwma1(2),nwma1(3))+3), stat=aloc)            !/*mgd       355*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       356*/
      asm=0                                                                     !/*mgd       357*/
      if(allocated(asp))deallocate (asp)                                        !/*mgd       358*/
      allocate (asp(nee3*nmat1*(1-istrg3)), stat=aloc)                          !/*mgd       359*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       360*/
      asp=0                                                                     !/*mgd       361*/
      if(allocated(awc))deallocate (awc)                                        !/*mgd       362*/
      allocate (awc(mix1), stat=aloc)                                           !/*mgd       363*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       364*/
      awc=0                                                                     !/*mgd       365*/
      if(allocated(awn))deallocate (awn)                                        !/*mgd       366*/
      allocate (awn(mxe1), stat=aloc)                                           !/*mgd       367*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       368*/
      awn=0                                                                     !/*mgd       369*/
      if(allocated(cmg))deallocate (cmg)                                        !/*mgd       370*/
      allocate (cmg((1-abs(iplt1-1))*mxa1*igm1), stat=aloc)                     !/*mgd       371*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       372*/
      cmg=0                                                                     !/*mgd       373*/
      if(allocated(den))deallocate (den)                                        !/*mgdl       10*/
      allocate (den(mxa1), stat=aloc)                                           !/*mgdl       11*/
      if(aloc.ne.0)go to 10                                                     !/*mgdl       12*/
      den=0.                                                                    !/*mgdl       13*/
      if(allocated(dptb))deallocate (dptb)                                      !/*mgd       374*/
      allocate (dptb(3,npert1*mnnm1), stat=aloc)                                !/*mgd       375*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       376*/
      dptb=0                                                                    !/*mgd       377*/
      if(allocated(drs))deallocate (drs)                                        !/*mgd       378*/
      allocate (drs(mipt,nee_max*nmat1), stat=aloc)                             !/*mgd       379*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       380*/
      drs=0                                                                     !/*mgd       381*/
      if(allocated(dxcp))deallocate (dxcp)                                      !/*mgd       382*/
      allocate (dxcp(0:mxdx*min(1_kindi,nxnx1),mipt,mxa1), stat=aloc)           !/*mgd       383*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       384*/
      dxcp=0                                                                    !/*mgd       385*/
      if(allocated(eaa))deallocate (eaa)                                        !/*mgd       386*/
      allocate (eaa(msd1), stat=aloc)                                           !/*mgd       387*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       388*/
      eaa=0                                                                     !/*mgd       389*/
      if(allocated(ear))deallocate (ear)                                        !/*mgd       390*/
      allocate (ear(nee3*nmat1*(1-istrg3)), stat=aloc)                          !/*mgd       391*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       392*/
      ear=0                                                                     !/*mgd       393*/
      if(allocated(eba))deallocate (eba)                                        !/*mgd       394*/
      allocate (eba(mtop,nee3*nmat1), stat=aloc)                                !/*mgd       395*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       396*/
      eba=0                                                                     !/*mgd       397*/
      if(allocated(ebd))deallocate (ebd)                                        !/*mgd       398*/
      allocate (ebd(mtop,nee3*nmat1*mbbm1), stat=aloc)                          !/*mgd       399*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       400*/
      ebd=0                                                                     !/*mgd       401*/
      if(allocated(ebl))deallocate (ebl)                                        !/*mgd       402*/
      allocate (ebl(max(16_kindi,igm1)), stat=aloc)                             !/*mgd       403*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       404*/
      ebl=0                                                                     !/*mgd       405*/
      if(allocated(ebt))deallocate (ebt)                                        !/*mgd       406*/
      allocate (ebt(mtop,nee3*nmat1), stat=aloc)                                !/*mgd       407*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       408*/
      ebt=0                                                                     !/*mgd       409*/
      if(allocated(ech))deallocate (ech)                                        !/*mgd       410*/
      allocate (ech(mpng,mwng,(nee3/mstp+2)*nmat1*ke), stat=aloc)               !/*mgd       411*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       412*/
      ech=0                                                                     !/*mgd       413*/
      if(allocated(edg))deallocate (edg)                                        !/*mgd       414*/
      allocate (edg(nmat1*ke), stat=aloc)                                       !/*mgd       415*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       416*/
      edg=0                                                                     !/*mgd       417*/
      if(allocated(eee))deallocate (eee)                                        !/*mgd       418*/
      allocate (eee(mipt,nee_max), stat=aloc)                                   !/*mgd       419*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       420*/
      eee=0                                                                     !/*mgd       421*/
      if(allocated(eek))deallocate (eek)                                        !/*mgd       422*/
      allocate (eek(nmat1*ke), stat=aloc)                                       !/*mgd       423*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       424*/
      eek=0                                                                     !/*mgd       425*/
      if(allocated(egg))deallocate (egg)                                        !/*mgd       426*/
      allocate (egg(maxi,nee3*nmat1), stat=aloc)                                !/*mgd       427*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       428*/
      egg=0                                                                     !/*mgd       429*/
      if(allocated(elp))deallocate (elp)                                        !/*mgd       430*/
      allocate (elp(mipt,mxa1), stat=aloc)                                      !/*mgd       431*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       432*/
      elp=0                                                                     !/*mgd       433*/
      if(allocated(esa))deallocate (esa)                                        !/*mgd       434*/
      allocate (esa(0:mxe1), stat=aloc)                                         !/*mgd       435*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       436*/
      esa=0                                                                     !/*mgd       437*/
      if(allocated(ewwg))deallocate (ewwg)                                      !/*mgd       438*/
      allocate (ewwg(mgww1), stat=aloc)                                         !/*mgd       439*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       440*/
      ewwg=0                                                                    !/*mgd       441*/
      if(allocated(fim))deallocate (fim)                                        !/*mgd       442*/
      allocate (fim(mipt,mxa1), stat=aloc)                                      !/*mgdl       14*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       444*/
      fim=0                                                                     !/*mgd       445*/
      if(allocated(flc))deallocate (flc)                                        !/*mgd       446*/
      allocate (flc(nee3*nmat1*(1-istrg3)), stat=aloc)                          !/*mgd       447*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       448*/
      flc=0                                                                     !/*mgd       449*/
      if(allocated(fme))deallocate (fme)                                        !/*mgdl       15*/
      allocate (fme(mix1), stat=aloc)                                           !/*mgdl       16*/
      if(aloc.ne.0)go to 10                                                     !/*mgdl       17*/
      fme=0.                                                                    !/*mgdl       18*/
      if(allocated(fmg))deallocate (fmg)                                        !/*mgd       450*/
      allocate (fmg(max(0_kindi,mcal1-1)*npn1), stat=aloc)                      !/*mgd       451*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       452*/
      fmg=0                                                                     !/*mgd       453*/
      if(allocated(for))deallocate (for)                                        !/*mgd       454*/
      allocate (for(mipt,mxa1), stat=aloc)                                      !/*mgd       455*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       456*/
      for=0                                                                     !/*mgd       457*/
      if(allocated(frc))deallocate (frc)                                        !/*mgd       458*/
      allocate (frc(msd1), stat=aloc)                                           !/*mgd       459*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       460*/
      frc=0                                                                     !/*mgd       461*/
      if(allocated(fst))deallocate (fst)                                        !/*mgd       462*/
      allocate (fst(nee3*nmat1*mbbm1), stat=aloc)                               !/*mgd       463*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       464*/
      fst=0                                                                     !/*mgd       465*/
      if(allocated(ftt))deallocate (ftt)                                        !/*mgd       466*/
      allocate (ftt(nee3*nmat1*mbbm1), stat=aloc)                               !/*mgd       467*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       468*/
      ftt=0                                                                     !/*mgd       469*/
      if(allocated(gmg))deallocate (gmg)                                        !/*mgd       470*/
      allocate (gmg(max(0_kindi,min(1_kindi,mcal1-abs(iplt1-1)))                !/*mgd       471*/
     & *igm1*mxa1), stat=aloc)                                                  !/*mgd       472*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       473*/
      gmg=0                                                                     !/*mgd       474*/
      if(allocated(gvl))deallocate (gvl)                                        !/*mgd       475*/
      allocate (gvl(igm1), stat=aloc)                                           !/*mgd       476*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       477*/
      gvl=0                                                                     !/*mgd       478*/
      if(allocated(gwt))deallocate (gwt)                                        !/*mgd       479*/
      allocate (gwt(mxa1*kpt1(1)*kpt1(2)), stat=aloc)                           !/*mgd       480*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       481*/
      gwt=0                                                                     !/*mgd       482*/
      if(allocated(pbr))deallocate (pbr)                                        !/*mgd       483*/
      allocate (pbr(nee3*nmat1), stat=aloc)                                     !/*mgd       484*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       485*/
      pbr=0                                                                     !/*mgd       486*/
      if(allocated(pbt))deallocate (pbt)                                        !/*mgd       487*/
      allocate (pbt(nee3*nmat1), stat=aloc)                                     !/*mgd       488*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       489*/
      pbt=0                                                                     !/*mgd       490*/
      if(allocated(pkn))deallocate (pkn)                                        !/*mgd       491*/
      allocate (pkn(nee3*nmat1), stat=aloc)                                     !/*mgd       492*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       493*/
      pkn=0                                                                     !/*mgd       494*/
      if(allocated(pmg))deallocate (pmg)                                        !/*mgd       495*/
      allocate (pmg(max(0_kindi,mcal1-1)*igm1*npn1), stat=aloc)                 !/*mgd       496*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       497*/
      pmg=0                                                                     !/*mgd       498*/
      if(allocated(pnt))deallocate (pnt)                                        !/*mgd       499*/
      allocate (pnt(nmat1), stat=aloc)                                          !/*mgd       500*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       501*/
      pnt=0                                                                     !/*mgd       502*/
      if(allocated(pru))deallocate (pru)                                        !/*mgd       503*/
      allocate (pru(nee3*nmat1), stat=aloc)                                     !/*mgd       504*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       505*/
      pru=0                                                                     !/*mgd       506*/
      if(allocated(pxr))deallocate (pxr)                                        !/*mgd       507*/
      allocate (pxr(nee3*nmat1), stat=aloc)                                     !/*mgd       508*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       509*/
      pxr=0                                                                     !/*mgd       510*/
      if(allocated(qav))deallocate (qav)                                        !/*mgd       511*/
      allocate (qav(mipt,nee_max*nmat1), stat=aloc)                             !/*mgd       512*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       513*/
      qav=0                                                                     !/*mgd       514*/
      if(allocated(qax))deallocate (qax)                                        !/*mgd       515*/
      allocate (qax(mipt,mxa1), stat=aloc)                                      !/*mgd       516*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       517*/
      qax=0                                                                     !/*mgd       518*/
      if(allocated(qcn))deallocate (qcn)                                        !/*mgd       519*/
      allocate (qcn(nee3*nmat1*(1-istrg3)), stat=aloc)                          !/*mgd       520*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       521*/
      qcn=0                                                                     !/*mgd       522*/
      if(allocated(rho))deallocate (rho)                                        !/*mgdl       19*/
      allocate (rho(mxa1), stat=aloc)                                           !/*mgdl       20*/
      if(aloc.ne.0)go to 10                                                     !/*mgdl       21*/
      rho=0.                                                                    !/*mgdl       22*/
      if(allocated(rptb))deallocate (rptb)                                      !/*mgd       523*/
      allocate (rptb(ipert1), stat=aloc)                                        !/*mgd       524*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       525*/
      rptb=0                                                                    !/*mgd       526*/
      if(allocated(scf))deallocate (scf)                                        !/*mgd       527*/
      allocate (scf(nsc1+10), stat=aloc)                                        !/*mgd       528*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       529*/
      scf=0                                                                     !/*mgd       530*/
      if(allocated(smg))deallocate (smg)                                        !/*mgd       531*/
      allocate (smg(max(0_kindi,mcal1-1)*mxa1*igm1), stat=aloc)                 !/*mgd       532*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       533*/
      smg=0                                                                     !/*mgd       534*/
      if(allocated(spf))deallocate (spf)                                        !/*mgd       535*/
      if(dbcn(6).eq.0.)then                                                     !/*mgd       536*/
      allocate (spf(4,mxxs1/4+1), stat=aloc)                                    !/*mgd       537*/
      else                                                                      !/*mgd       538*/
      allocate (spf(mxxs1+4,mxxs1/4), stat=aloc)                                !/*mgd       539*/
      endif                                                                     !/*mgd       540*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       541*/
      spf=0                                                                     !/*mgd       542*/
      lspf=4                                                                    !/*mgd       543*/
      if(allocated(sqq))deallocate (sqq)                                        !/*mgd       544*/
      allocate (sqq(12,msd1), stat=aloc)                                        !/*mgd       545*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       546*/
      sqq=0                                                                     !/*mgd       547*/
      if(allocated(sso))deallocate (sso)                                        !/*mgd       548*/
      allocate (sso(msso1+1), stat=aloc)                                        !/*mgd       549*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       550*/
      sso=0                                                                     !/*mgd       551*/
      lsso=1                                                                    !/*mgd       552*/
      if(allocated(tbt))deallocate (tbt)                                        !/*mgd       553*/
      allocate (tbt(mxe1), stat=aloc)                                           !/*mgd       554*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       555*/
      tbt=0                                                                     !/*mgd       556*/
      if(allocated(tds))deallocate (tds)                                        !/*mgd       557*/
      allocate (tds(ltd1+1), stat=aloc)                                         !/*mgd       558*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       559*/
      tds=0.                                                                    !/*mgd       560*/
      ltds=1                                                                    !/*mgd       561*/
      if(allocated(tmp))deallocate (tmp)                                        !/*mgd       562*/
      allocate (tmp(mxa1*mxt1), stat=aloc)                                      !/*mgd       563*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       564*/
      tmp=0.                                                                    !/*mgd       565*/
      if(allocated(trf))deallocate (trf)                                        !/*mgd       566*/
      allocate (trf(17,0:mxtr1), stat=aloc)                                     !/*mgd       567*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       568*/
      trf=0.                                                                    !/*mgd       569*/
      if(allocated(tth))deallocate (tth)                                        !/*mgd       570*/
      allocate (tth(mxt1), stat=aloc)                                           !/*mgd       571*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       572*/
      tth=0                                                                     !/*mgd       573*/
      if(allocated(vcl))deallocate (vcl)                                        !/*mgd       574*/
      allocate (vcl(3,7,nlat1), stat=aloc)                                      !/*mgd       575*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       576*/
      vcl=0                                                                     !/*mgd       577*/
      if(allocated(vec))deallocate (vec)                                        !/*mgd       578*/
      allocate (vec(3,nvec1), stat=aloc)                                        !/*mgd       579*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       580*/
      vec=0                                                                     !/*mgd       581*/
      if(allocated(vol))deallocate (vol)                                        !/*mgdl       23*/
      allocate (vol(mxa1), stat=aloc)                                           !/*mgdl       24*/
      if(aloc.ne.0)go to 10                                                     !/*mgdl       25*/
      vol=0.                                                                    !/*mgdl       26*/
      if(allocated(wgm))deallocate (wgm)                                        !/*mgd       582*/
      allocate (wgm(nwgm1), stat=aloc)                                          !/*mgd       583*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       584*/
      wgm=0                                                                     !/*mgd       585*/
      if(allocated(wgma))deallocate (wgma)                                      !/*mgd       586*/
      allocate (wgma(nwgma1), stat=aloc)                                        !/*mgd       587*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       588*/
      wgma=0                                                                    !/*mgd       589*/
      if(allocated(wwe))deallocate (wwe)                                        !/*mgd       590*/
      allocate (wwe(mww1), stat=aloc)                                           !/*mgd       591*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       592*/
      wwe=0                                                                     !/*mgd       593*/
      if(allocated(wwf))deallocate (wwf)                                        !/*mgd       594*/
      allocate (wwf(nwwm1*mww1+3), stat=aloc)                                   !/*mgd       595*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       596*/
      wwf=0                                                                     !/*mgd       597*/
      if(allocated(wwk))deallocate (wwk)                                        !/*mgd       598*/
      allocate (wwk(nmat1*ke), stat=aloc)                                       !/*mgd       599*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       600*/
      wwk=0                                                                     !/*mgd       601*/
      if(allocated(xnm))deallocate (xnm)                                        !/*mgd       602*/
      allocate (xnm(nmat1*ke), stat=aloc)                                       !/*mgd       603*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       604*/
      xnm=0                                                                     !/*mgd       605*/
      if(allocated(xse85))deallocate (xse85)                                    !/*mgd       606*/
      allocate (xse85(10,nmat1*nee3), stat=aloc)                                !/*mgd       607*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       608*/
      xse85=0                                                                   !/*mgd       609*/
!                                                                               !/*mgd       610*/
!        mcnpx                                                                  !/*mgd       611*/
!        kc = 1 if any charged particles are run, otherwise kc = 0.             !/*mgd       612*/
      kc=0                                                                      !/*mgd       613*/
      if(nee_max.gt.0)kc=1                                                      !/*mgd       614*/
!                                                                               !/*mgd       615*/
      if(allocated(aa9))deallocate (aa9)                                        !/*mgd       616*/
      allocate (aa9(mxmel1), stat=aloc)                                         !/*mgd       617*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       618*/
      aa9=0                                                                     !/*mgd       619*/
      if(allocated(arg))deallocate (arg)                                        !/*mgd       620*/
      allocate (arg(nmat1), stat=aloc)                                          !/*mgd       621*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       622*/
      arg=0                                                                     !/*mgd       623*/
      if(allocated(ava))deallocate (ava)                                        !/*mgd       624*/
      allocate (ava(nmat1*kc), stat=aloc)                                       !/*mgd       625*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       626*/
      ava=0                                                                     !/*mgd       627*/
      if(allocated(avz))deallocate (avz)                                        !/*mgd       628*/
      allocate (avz(nmat1*kc), stat=aloc)                                       !/*mgd       629*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       630*/
      avz=0                                                                     !/*mgd       631*/
      if(allocated(emaxt))deallocate (emaxt)                                    !/*mgd       632*/
      allocate (emaxt(mxe1), stat=aloc)                                         !/*mgd       633*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       634*/
      emaxt=0                                                                   !/*mgd       635*/
#ifdef MESHTAL                                                                  /*!mgd       636*/
      if(allocated(emsh))deallocate (emsh)                                      !/*mgd       637*/
      allocate (emsh(nemsh1), stat=aloc)                                        !/*mgd       638*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       639*/
      emsh=0                                                                    !/*mgd       640*/
#endif                                                                          /*!mgd       641*/
#ifdef SPABI                                                                    /*!mgd       642*/
      if(allocated(energies_and_params))deallocate (energies_and_params)        !/*mgd       643*/
      allocate (energies_and_params(nspabi), stat=aloc)                         !/*mgd       644*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       645*/
      energies_and_params=0                                                     !/*mgd       646*/
#endif                                                                          /*!mgd       647*/
#ifdef CEM                                                                      /*!mgd       648*/
      if(allocated(epnmin))deallocate (epnmin)                                  !/*mgd       649*/
      allocate (epnmin(mxmel1), stat=aloc)                                      !/*mgd       650*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       651*/
      epnmin=0                                                                  !/*mgd       652*/
#endif                                                                          /*!mgd       653*/
      if(allocated(esxln))deallocate (esxln)                                    !/*mgd       654*/
      allocate (esxln(iemaxn,mxmel1), stat=aloc)                                !/*mgd       655*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       656*/
      esxln=0                                                                   !/*mgd       657*/
      if(allocated(esxlp))deallocate (esxlp)                                    !/*mgd       658*/
      allocate (esxlp(iemaxp,mxmel1), stat=aloc)                                !/*mgd       659*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       660*/
      esxlp=0                                                                   !/*mgd       661*/
      if(allocated(esxmp))deallocate (esxmp)                                    !/*mgd       662*/
      allocate (esxmp(mxmel1), stat=aloc)                                       !/*mgd       663*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       664*/
      esxmp=0                                                                   !/*mgd       665*/
      if(allocated(fmf))deallocate (fmf)                                        !/*mgd       666*/
      allocate (fmf(mxmel1), stat=aloc)                                         !/*mgd       667*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       668*/
      fmf=0                                                                     !/*mgd       669*/
      if(allocated(hsigg))deallocate (hsigg)                                    !/*mgd       670*/
      allocate (hsigg(mlpt,nmat1), stat=aloc)                                   !/*mgd       671*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       672*/
      hsigg=0                                                                   !/*mgd       673*/
      if(allocated(pcap))deallocate (pcap)                                      !/*mgd       674*/
      allocate (pcap(mxmel1), stat=aloc)                                        !/*mgd       675*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       676*/
      pcap=0                                                                    !/*mgd       677*/
      if(allocated(sigg))deallocate (sigg)                                      !/*mgd       678*/
      allocate (sigg(msigg,mxmel1), stat=aloc)                                  !/*mgd       679*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       680*/
      sigg=0                                                                    !/*mgd       681*/
      if(allocated(sigmx))deallocate (sigmx)                                    !/*mgd       682*/
      allocate (sigmx(mlpt,nmat1+1), stat=aloc)                                 !/*mgd       683*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       684*/
      sigmx=0                                                                   !/*mgd       685*/
      if(allocated(xsiso))deallocate (xsiso)                                    !/*mgd       686*/
      allocate (xsiso(mxso,mxmel1), stat=aloc)                                  !/*mgd       687*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       688*/
      xsiso=0                                                                   !/*mgd       689*/
      if(allocated(zz9))deallocate (zz9)                                        !/*mgd       690*/
      allocate (zz9(mxmel1), stat=aloc)                                         !/*mgd       691*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       692*/
      zz9=0                                                                     !/*mgd       693*/
!                                                                               !/*mgd       694*/
!        allocate fdac integer variables.                                       !/*mgd       695*/
      if(allocated(idna))deallocate (idna)                                      !/*mgd       696*/
      allocate (idna(mlja1+1), stat=aloc)                                       !/*mgd       697*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       698*/
      idna=0                                                                    !/*mgd       699*/
      if(allocated(idne))deallocate (idne)                                      !/*mgd       700*/
      allocate (idne(mxj1+100), stat=aloc)                                      !/*mgd       701*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       702*/
      idne=0                                                                    !/*mgd       703*/
      if(allocated(idns))deallocate (idns)                                      !/*mgd       704*/
      allocate (idns(mxj1), stat=aloc)                                          !/*mgd       705*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       706*/
      idns=0                                                                    !/*mgd       707*/
      if(allocated(idnt))deallocate (idnt)                                      !/*mgd       708*/
      allocate (idnt(mxj1), stat=aloc)                                          !/*mgd       709*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       710*/
      idnt=0                                                                    !/*mgd       711*/
      if(allocated(ipan))deallocate (ipan)                                      !/*mgd       712*/
      allocate (ipan(mxa1+1), stat=aloc)                                        !/*mgd       713*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       714*/
      ipan=0                                                                    !/*mgd       715*/
      if(allocated(iptal))deallocate (iptal)                                    !/*mgd       716*/
      allocate (iptal(8,6,ntal1), stat=aloc)                                    !/*mgd       717*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       718*/
      iptal=0                                                                   !/*mgd       719*/
      if(allocated(iptb))deallocate (iptb)                                      !/*mgd       720*/
      allocate (iptb(2+2*npkey,npert1), stat=aloc)                              !/*mgd       721*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       722*/
      iptb=0                                                                    !/*mgd       723*/
      if(allocated(iss))deallocate (iss)                                        !/*mgd       724*/
      allocate (iss(mxss,njsx1), stat=aloc)                                     !/*mgd       725*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       726*/
      iss=0                                                                     !/*mgd       727*/
      if(allocated(itds))deallocate (itds)                                      !/*mgd       728*/
      allocate (itds(lit1+1), stat=aloc)                                        !/*mgd       729*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       730*/
      itds=0                                                                    !/*mgd       731*/
      litd=1                                                                    !/*mgd       732*/
      if(allocated(ixl))deallocate (ixl)                                        !/*mgd       733*/
      allocate (ixl(3,mxe1), stat=aloc)                                         !/*mgd       734*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       735*/
      ixl=0                                                                     !/*mgd       736*/
      if(allocated(ixs))deallocate (ixs)                                        !/*mgd       737*/
      allocate (ixs(mixs,maxsec,mxe1), stat=aloc)                               !/*mgd       738*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       739*/
      ixs=0                                                                     !/*mgd       740*/
      if(allocated(iza))deallocate (iza)                                        !/*mgd       741*/
      allocate (iza(mix1), stat=aloc)                                           !/*mgd       742*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       743*/
      iza=0                                                                     !/*mgd       744*/
      if(allocated(izn))deallocate (izn)                                        !/*mgd       745*/
      allocate (izn(mix1), stat=aloc)                                           !/*mgd       746*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       747*/
      izn=0                                                                     !/*mgd       748*/
      if(allocated(jasq))deallocate (jasq)                                      !/*mgd       749*/
      allocate (jasq((njsw1+niwr1)*kjaq1), stat=aloc)                           !/*mgd       750*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       751*/
      jasq=0                                                                    !/*mgd       752*/
      if(allocated(jasr))deallocate (jasr)                                      !/*mgd       753*/
      allocate (jasr(mxss,njsr1+nilr1+1), stat=aloc)                            !/*mgd       754*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       755*/
      jasr=0                                                                    !/*mgd       756*/
      if(allocated(jasw))deallocate (jasw)                                      !/*mgd       757*/
      allocate (jasw(njsw1+niwr1), stat=aloc)                                   !/*mgd       758*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       759*/
      jasw=0                                                                    !/*mgd       760*/
      if(allocated(jmd))deallocate (jmd)                                        !/*mgd       761*/
      allocate (jmd(0:nmat1+1), stat=aloc)                                      !/*mgd       762*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       763*/
      jmd=0                                                                     !/*mgd       764*/
      if(allocated(jptal))deallocate (jptal)                                    !/*mgd       765*/
      allocate (jptal(18,ntal1), stat=aloc)                                     !/*mgd       766*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       767*/
      jptal=0                                                                   !/*mgd       768*/
      if(allocated(jscn))deallocate (jscn)                                      !/*mgd       769*/
      allocate (jscn(mssc1+1), stat=aloc)                                       !/*mgd       770*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       771*/
      jscn=0                                                                    !/*mgd       772*/
      ljsc=1                                                                    !/*mgd       773*/
      if(allocated(jsq))deallocate (jsq)                                        !/*mgd       774*/
      allocate (jsq(mjss1+nilw1), stat=aloc)                                    !/*mgd       775*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       776*/
      jsq=0                                                                     !/*mgd       777*/
      if(allocated(jss))deallocate (jss)                                        !/*mgd       778*/
      allocate (jss(mjss1+nilw1), stat=aloc)                                    !/*mgd       779*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       780*/
      jss=0                                                                     !/*mgd       781*/
      if(allocated(jtf))deallocate (jtf)                                        !/*mgd       782*/
      allocate (jtf(8,ntal1), stat=aloc)                                        !/*mgd       783*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       784*/
      jtf=0                                                                     !/*mgd       785*/
      if(allocated(jun))deallocate (jun)                                        !/*mgd       786*/
      allocate (jun(junf1*mxa1), stat=aloc)                                     !/*mgd       787*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       788*/
      jun=0                                                                     !/*mgd       789*/
      if(allocated(jvc))deallocate (jvc)                                        !/*mgd       790*/
      allocate (jvc(nvec1), stat=aloc)                                          !/*mgd       791*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       792*/
      jvc=0                                                                     !/*mgd       793*/
      if(allocated(jxs))deallocate (jxs)                                        !/*mgd       794*/
      allocate (jxs(32,mxe1), stat=aloc)                                        !/*mgd       795*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       796*/
      jxs=0                                                                     !/*mgd       797*/
      if(allocated(kcp))deallocate (kcp)                                        !/*mgd       798*/
      allocate (kcp(mkcp1), stat=aloc)                                          !/*mgd       799*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       800*/
      kcp=0                                                                     !/*mgd       801*/
      if(allocated(kmm))deallocate (kmm)                                        !/*mgd       802*/
      allocate (kmm(mix1), stat=aloc)                                           !/*mgd       803*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       804*/
      kmm=0                                                                     !/*mgd       805*/
      if(allocated(ksc))deallocate (ksc)                                        !/*mgd       806*/
      allocate (ksc(mxj1), stat=aloc)                                           !/*mgd       807*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       808*/
      ksc=0                                                                     !/*mgd       809*/
      if(allocated(ksd))deallocate (ksd)                                        !/*mgd       810*/
      allocate (ksd(21,msd1+3), stat=aloc)                                      !/*mgd       811*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       812*/
      ksd=0                                                                     !/*mgd       813*/
      if(allocated(ksm))deallocate (ksm)                                        !/*mgd       814*/
      allocate (ksm(mxj1), stat=aloc)                                           !/*mgd       815*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       816*/
      ksm=0                                                                     !/*mgd       817*/
      if(allocated(kst))deallocate (kst)                                        !/*mgd       818*/
      allocate (kst(mxj1), stat=aloc)                                           !/*mgd       819*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       820*/
      kst=0                                                                     !/*mgd       821*/
      if(allocated(ksu))deallocate (ksu)                                        !/*mgd       822*/
      allocate (ksu(mxj1), stat=aloc)                                           !/*mgd       823*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       824*/
      ksu=0                                                                     !/*mgd       825*/
      if(allocated(ktp))deallocate (ktp)                                        !/*mgd       826*/
      allocate (ktp(mipt,ntal1), stat=aloc)                                     !/*mgd       827*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       828*/
      ktp=0                                                                     !/*mgd       829*/
      kxs => chg_mem (kxs,mxe1,aloc)                                            !/*mgd       830*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       831*/
      kxs=0                                                                     !/*mgd       832*/
      if(dbcn(6).eq.0.)then                                                     !/*mgd       833*/
      laf => chg_mem (laf,3_kindi,mlaf1/3+1,aloc)                               !/*mgd       834*/
      else                                                                      !/*mgd       835*/
      laf => chg_mem (laf,mlaf1+3,mlaf1/3,aloc)                                 !/*mgd       836*/
      endif                                                                     !/*mgd       837*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       838*/
      laf=0                                                                     !/*mgd       839*/
      llaf=3                                                                    !/*mgd       840*/
      if(allocated(lat))deallocate (lat)                                        !/*mgd       841*/
      allocate (lat(2,junf1*mxa1), stat=aloc)                                   !/*mgd       842*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       843*/
      lat=0                                                                     !/*mgd       844*/
      if(allocated(lca))deallocate (lca)                                        !/*mgd       845*/
      allocate (lca(mxafs1+1), stat=aloc)                                       !/*mgd       846*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       847*/
      lca=0                                                                     !/*mgd       848*/
      if(allocated(lft))deallocate (lft)                                        !/*mgd       849*/
      allocate (lft(mkft,ntal1), stat=aloc)                                     !/*mgd       850*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       851*/
      lft=0                                                                     !/*mgd       852*/
      if(allocated(lja))deallocate (lja)                                        !/*mgd       853*/
      allocate (lja(mlja1+1), stat=aloc)                                        !/*mgd       854*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       855*/
      lja=0                                                                     !/*mgd       856*/
      if(allocated(lme))deallocate (lme)                                        !/*mgd       857*/
      allocate (lme(mipt,mix1), stat=aloc)                                      !/*mgd       858*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       859*/
      lme=0                                                                     !/*mgd       860*/
      if(allocated(lmn))deallocate (lmn)                                        !/*mgd       861*/
      allocate (lmn(mix1), stat=aloc)                                           !/*mgd       862*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       863*/
      lmn=0                                                                     !/*mgd       864*/
      if(allocated(lmt))deallocate (lmt)                                        !/*mgd       865*/
      allocate (lmt(mix1), stat=aloc)                                           !/*mgd       866*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       867*/
      lmt=0                                                                     !/*mgd       868*/
      if(allocated(locct))deallocate (locct)                                    !/*mgd       869*/
      allocate (locct(mipt,mxa1), stat=aloc)                                    !/*mgd       870*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       871*/
      locct=0                                                                   !/*mgd       872*/
      if(allocated(locph))deallocate (locph)                                    !/*mgd       873*/
      allocate (locph(mxa1), stat=aloc)                                         !/*mgd       874*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       875*/
      locph=0                                                                   !/*mgd       876*/
      if(allocated(locst))deallocate (locst)                                    !/*mgd       877*/
      allocate (locst(mipt,mxj1), stat=aloc)                                    !/*mgd       878*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       879*/
      locst=0                                                                   !/*mgd       880*/
      if(allocated(lsc))deallocate (lsc)                                        !/*mgd       881*/
      allocate (lsc(mxj1+1), stat=aloc)                                         !/*mgd       882*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       883*/
      lsc=0                                                                     !/*mgd       884*/
      if(allocated(mat))deallocate (mat)                                        !/*mgd       885*/
      allocate (mat(mxa1), stat=aloc)                                           !/*mgd       886*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       887*/
      mat=0                                                                     !/*mgd       888*/
      if(allocated(mazp))deallocate (mazp)                                      !/*mgd       889*/
      allocate (mazp(3,junf1*mxa1), stat=aloc)                                  !/*mgd       890*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       891*/
      mazp=0                                                                    !/*mgd       892*/
      mazu => chg_mem (mazu,nmzu1,aloc)                                         !/*mgd       893*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       894*/
      mazu=0                                                                    !/*mgd       895*/
      if(allocated(mbd))deallocate (mbd)                                        !/*mgd       896*/
      allocate (mbd(mxa1), stat=aloc)                                           !/*mgd       897*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       898*/
      mbd=0                                                                     !/*mgd       899*/
      if(allocated(mbi))deallocate (mbi)                                        !/*mgd       900*/
      allocate (mbi(nmat1*ke), stat=aloc)                                       !/*mgd       901*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       902*/
      mbi=0                                                                     !/*mgd       903*/
      if(allocated(mfl))deallocate (mfl)                                        !/*mgd       904*/
      allocate (mfl(3,junf1*mxa1), stat=aloc)                                   !/*mgd       905*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       906*/
      mfl=0                                                                     !/*mgd       907*/
!        I think the storage for ncl can be mxa1+1, not mxa1+2.                 !/*mgd       908*/
      ncl => chg_mem (ncl,mxa1+2,aloc)                                          !/*mgd       909*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       910*/
      ncl=0                                                                     !/*mgd       911*/
      if(allocated(ngmfl))deallocate (ngmfl)                                    !/*mgd       912*/
      allocate (ngmfl(mxe1), stat=aloc)                                         !/*mgd       913*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       914*/
      ngmfl=0                                                                   !/*mgd       915*/
      if(allocated(nhtfl))deallocate (nhtfl)                                    !/*mgd       916*/
      allocate (nhtfl(mxe1), stat=aloc)                                         !/*mgd       917*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       918*/
      nhtfl=0                                                                   !/*mgd       919*/
      if(allocated(nmt))deallocate (nmt)                                        !/*mgd       920*/
      allocate (nmt(0:nmat1), stat=aloc)                                        !/*mgd       921*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       922*/
      nmt=0                                                                     !/*mgd       923*/
      if(allocated(npq))deallocate (npq)                                        !/*mgd       924*/
      allocate (npq(0:nmat1), stat=aloc)                                        !/*mgd       925*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       926*/
      npq=0                                                                     !/*mgd       927*/
      if(allocated(nptb))deallocate (nptb)                                      !/*mgd       928*/
      allocate (nptb(npert1+1), stat=aloc)                                      !/*mgd       929*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       930*/
      nptb=0                                                                    !/*mgd       931*/
      if(allocated(nsb))deallocate (nsb)                                        !/*mgd       932*/
      allocate (nsb(nmat1*kc), stat=aloc)                                       !/*mgd       933*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       934*/
      nsb=0                                                                     !/*mgd       935*/
      nsfm => chg_mem (nsfm,mxj1,aloc)                                          !/*mgd       936*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       937*/
      nsfm=0                                                                    !/*mgd       938*/
      if(allocated(nslr))deallocate (nslr)                                      !/*mgd       939*/
      allocate (nslr(2+4*mipts1,njsw1+niwr1), stat=aloc)                        !/*mgd       940*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       941*/
      nslr=0                                                                    !/*mgd       942*/
      if(allocated(nty))deallocate (nty)                                        !/*mgd       943*/
      allocate (nty(mxe1), stat=aloc)                                           !/*mgd       944*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       945*/
      nty=0                                                                     !/*mgd       946*/
      if(allocated(nxs))deallocate (nxs)                                        !/*mgd       947*/
      allocate (nxs(16,mxe1), stat=aloc)                                        !/*mgd       948*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       949*/
      nxs=0                                                                     !/*mgd       950*/
!                                                                               !/*mgd       951*/
!        mcnpx                                                                  !/*mgd       952*/
      if(allocated(iazdex))deallocate (iazdex)                                  !/*mgd       953*/
      allocate (iazdex(mxmel1), stat=aloc)                                      !/*mgd       954*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       955*/
      iazdex=0                                                                  !/*mgd       956*/
      if(allocated(kpptb))deallocate (kpptb)                                    !/*mgd       957*/
      allocate (kpptb(mipt,npert), stat=aloc)                                   !/*mgd       958*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       959*/
      kpptb=0                                                                   !/*mgd       960*/
      if(allocated(mel))deallocate (mel)                                        !/*mgd       961*/
      allocate (mel(nmat1+1), stat=aloc)                                        !/*mgd       962*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       963*/
      mel=0                                                                     !/*mgd       964*/
      if(allocated(nel))deallocate (nel)                                        !/*mgd       965*/
      allocate (nel(nmat1), stat=aloc)                                          !/*mgd       966*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       967*/
      nel=0                                                                     !/*mgd       968*/
#ifdef HISTP                                                                    /*!mgd       969*/
      if(allocated(nhw))deallocate (nhw)                                        !/*mgd       970*/
      allocate (nhw(mxa), stat=aloc)                                            !/*mgd       971*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       972*/
      nhw=0                                                                     !/*mgd       973*/
#endif                                                                          /*!mgd       974*/
#ifdef CEM                                                                      /*!mgd       975*/
      if(allocated(ipnf))deallocate (ipnf)                                      !/*mgd       976*/
      allocate (ipnf(mxmel1), stat=aloc)                                        !/*mgd       977*/
      if(aloc.ne.0)go to 10                                                     !/*mgd       978*/
      ipnf=0                                                                    !/*mgd       979*/
#endif                                                                          /*!mgd       980*/
!                                                                               !/*mgd       981*/
!        for debug - make sure variables don't change on continue-runs.         !/*mgd       982*/
!     WRITE(*,*)nee3,nmat1,istrg3,mix1,mxe1,iplt1,mxa1,igm1,npert1,             !/*mgd       983*/
!    1 mnnm1,nxnx1,mipt,msd1,mtop,NINT(bbrem(1)),mpng,mwng,mstp,ke,maxi,        !/*mgd       984*/
!    2 mgww(4),mcal1,npn1,kpt1(1),kpt1(2),ipert1,nsc1,mxxs1,msso1,ltd1,         !/*mgd       985*/
!    3 nlat1,nvec1,nwgm1,mww1(4),nwwm1,nwgma1,nwma1(1),nwma1(2),                !/*mgd       986*/
!    4 nwma1(3),mlja1,mxj1,ntal1,npkey,mxss,njsx1,lit1,njsr1,nilr1,             !/*mgd       987*/
!    5 njsw1,niwr1,mssc1,mjss1,nilw1,junf1,mkcp1,msd1,mlaf1,mxafs1,             !/*mgd       988*/
!    6 mkft,nmzu1,iptra1,iptra2,icw1,indt1,kfl1,npikmt1,mrkp1,kjaq1,            !/*mgd       989*/
! #ifdef SPABI                                                                  !/*mgd       990*/
!      8 nspabi,                                                                !/*mgd       991*/
! #endif                                                                        !/*mgd       992*/
! #ifdef MESHTAL                                                                !/*mgd       993*/
!      8 nemsh1,ngdt,ngd1,ngd2,ngd3,                                            !/*mgd       994*/
! #endif                                                                        !/*mgd       995*/
!      9 mxmel1                                                                 !/*mgd       996*/
!                                                                               !/*mgd       997*/
!        allocate vdac real variables.                                          !/*mgd       998*/
      if(allocated(fso))deallocate (fso)                                        !/*mgd      1011*/
      allocate (fso(6*msrk1), stat=aloc)                                        !/*mgd      1012*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1013*/
      fso=0.                                                                    !/*mgd      1014*/
      if(allocated(rkpl))deallocate (rkpl)                                      !/*mgd      1019*/
      allocate (rkpl(mkpl,mrkp1), stat=aloc)                                    !/*mgd      1020*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1021*/
      rkpl=0.                                                                   !/*mgd      1022*/
      if(allocated(tfc))deallocate (tfc)                                        !/*mgd      1023*/
      allocate (tfc(6,20,ntal1*(npert1+1)), stat=aloc)                          !/*mgd      1024*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1025*/
      tfc=0.                                                                    !/*mgd      1026*/
!                                                                               !/*mgd      1041*/
!        allocate vdac real variables dimensioned by tasks.                     !/*mgd      1042*/
!        zero offset is to avoid write ddm(:,1:0) in vdac_write/read.           !/*mgd      1043*/
      if(allocated(ddm))deallocate (ddm)                                        !/*mgd      1044*/
      allocate (ddm(2,0:ntal1*(npert1+1)*mt), stat=aloc)                        !/*mgd      1045*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1046*/
      ddm=0.                                                                    !/*mgd      1047*/
      if(allocated(ddn))deallocate (ddn)                                        !/*mgd      1048*/
      allocate (ddn(24,0:ndnd1*mt), stat=aloc)                                  !/*mgd      1049*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1050*/
      ddn=0.                                                                    !/*mgd      1051*/
      if(allocated(dec))deallocate (dec)                                        !/*mgd      1052*/
      allocate (dec(3,0:mxa1*ndnd1*mt), stat=aloc)                              !/*mgd      1053*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1054*/
      dec=0.                                                                    !/*mgd      1055*/
      if(allocated(dxc))deallocate (dxc)                                        !/*mgd      1056*/
      allocate (dxc(3,0:mxa1*nxnx1*mt), stat=aloc)                              !/*mgd      1057*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1058*/
      dxc=0.                                                                    !/*mgd      1059*/
      if(allocated(dxd))deallocate (dxd)                                        !/*mgd      1060*/
      allocate (dxd(mipt,24,0:mxdx*min(1_kindi,nxnx1)*mt), stat=aloc)           !/*mgdk        2*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1062*/
      dxd=0.                                                                    !/*mgd      1063*/
      if(allocated(febl))deallocate (febl)                                      !/*mgd      1064*/
      allocate (febl(2,max(16_kindi,igm1)*mt), stat=aloc)                       !/*mgd      1065*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1066*/
      febl=0.                                                                   !/*mgd      1067*/
      if(allocated(flx))deallocate (flx)                                        !/*mgd      1068*/
      allocate (flx(0:min(1_kindi,icw1)*mxa1*igm1*mt), stat=aloc)               !/*mgd      1069*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1070*/
      flx=0.                                                                    !/*mgd      1071*/
      if(allocated(wwfa))deallocate (wwfa)                                      !/*mgd      1072*/
      allocate (wwfa(0:(mgww1+mipt)*nwwma1*mt), stat=aloc)                      !/*mgd      1073*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1074*/
      wwfa=0.                                                                   !/*mgd      1075*/
      if(allocated(swwfa))deallocate (swwfa)                                    !/*mgd      1076*/
      allocate (swwfa(0:(mgww1+mipt)*nwwma1*mt), stat=aloc)                     !/*mgd      1077*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1078*/
      swwfa=0.                                                                  !/*mgd      1079*/
!                                                                               !/*mgd      1080*/
!        pac, pan, pwb use mt+1 to account for junk storage.                    !/*mgd      1081*/
      if(allocated(pac))deallocate (pac)                                        !/*mgd      1082*/
      allocate (pac(mipt,10,mxa1*(mt+1)), stat=aloc)                            !/*mgd      1083*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1084*/
      pac=0.                                                                    !/*mgd      1085*/
      if(allocated(pan))deallocate (pan)                                        !/*mgd      1086*/
      allocate (pan(3,8,0:npn1*(mt+1)), stat=aloc)                              !/*mgd      1087*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1088*/
      pan=0.                                                                    !/*mgd      1089*/
      if(allocated(pwb))deallocate (pwb)                                        !/*mgd      1090*/
      allocate (pwb(mipt,items_pwb_total,mxa1*(mt+1)), stat=aloc)               !/*mgd      1091*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1092*/
      pwb=0.                                                                    !/*mgd      1093*/
!                                                                               !/*mgd      1104*/
      if(allocated(pcc))deallocate (pcc)                                        !/*mgd      1105*/
      allocate (pcc(3,0:mxa1*kpt1(2)*mt), stat=aloc)                            !/*mgd      1106*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1107*/
      pcc=0.                                                                    !/*mgd      1108*/
      if(allocated(sump))deallocate (sump)                                      !/*mgd      1109*/
      if(nsr.eq.71) then                                                        !/*mgd      1110*/
         allocate (sump(0:npert1*(mt+2)), stat=aloc)                            !/*mgd      1111*/
         if(aloc.ne.0)go to 10                                                  !/*mgd      1112*/
         sump=0.                                                                !/*mgd      1113*/
      endif                                                                     !/*mgd      1114*/
      if(allocated(wns))deallocate (wns)                                        !/*mgd      1115*/
      if(dbcn(6).eq.0.)then                                                     !/*mgd      1116*/
      allocate (wns(2,0:mxxs1/4*mt), stat=aloc)                                 !/*mgd      1117*/
      else                                                                      !/*mgd      1118*/
      allocate (wns(mxxs1,0:mxxs1/4*mt), stat=aloc)                             !/*mgd      1119*/
      endif                                                                     !/*mgd      1120*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1121*/
      wns=0.                                                                    !/*mgd      1122*/
!                                                                               !/*mgd      1123*/
!        allocate vdac integer variables.                                       !/*mgd      1124*/
      if(allocated(jfq))deallocate (jfq)                                        !/*mgd      1125*/
      allocate (jfq(8,0:ntal1), stat=aloc)                                      !/*mgd      1126*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1127*/
      jfq=0                                                                     !/*mgd      1128*/
      if(allocated(npsw))deallocate (npsw)                                      !/*mgd      1129*/
      allocate (npsw(njss1+nilw1), stat=aloc)                                   !/*mgd      1130*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1131*/
      npsw=0                                                                    !/*mgd      1132*/
      if(allocated(nsl))deallocate (nsl)                                        !/*mgd      1133*/
      allocate (nsl(2+4*mipt,njss1+nilw1), stat=aloc)                           !/*mgd      1134*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1135*/
      nsl=0                                                                     !/*mgd      1136*/
      if(allocated(ntbb))deallocate (ntbb)                                      !/*mgd      1137*/
      allocate (ntbb(5,ntal1), stat=aloc)                                       !/*mgd      1138*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1139*/
      ntbb=0                                                                    !/*mgd      1140*/
      if(allocated(lfcl))deallocate (lfcl)                                      !/*mgd      1141*/
      allocate (lfcl(mxa1+2), stat=aloc)                                        !/*mgd      1142*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1143*/
      lfcl=0                                                                    !/*mgd      1144*/
!                                                                               !/*mgd      1145*/
!        allocate vdac integer variables dimensioned by number of tasks.        !/*mgd      1146*/
!        zero offset is to avoid write ndr(1:0) in vdac_write/read.             !/*mgd      1147*/
      if(allocated(isef))deallocate (isef)                                      !/*mgd      1148*/
      allocate (isef(2,mxa1*mt), stat=aloc)                                     !/*mgd      1149*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1150*/
      isef=0                                                                    !/*mgd      1151*/
      if(allocated(maze))deallocate (maze)                                      !/*mgd      1152*/
      allocate (maze(0:3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3))*mt), stat=aloc)        !/*mgd      1153*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1154*/
      maze=0                                                                    !/*mgd      1155*/
      if(allocated(ndpf))deallocate (ndpf)                                      !/*mgd      1156*/
      allocate (ndpf(6,0:ndnd1*mt), stat=aloc)                                  !/*mgd      1157*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1158*/
      ndpf=0                                                                    !/*mgd      1159*/
      if(allocated(ndr))deallocate (ndr)                                        !/*mgd      1160*/
      allocate (ndr(0:mxe1*mt), stat=aloc)                                      !/*mgd      1161*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1162*/
      ndr=0                                                                     !/*mgd      1163*/
      if(allocated(laj))deallocate (laj)                                        !/*mgd      1164*/
      allocate (laj((mlaj1+mxa1)*mtasks), stat=aloc)                            !/*mgd      1165*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1166*/
      laj=0                                                                     !/*mgd      1167*/
      if(allocated(lcaj))deallocate (lcaj)                                      !/*mgd      1168*/
      allocate (lcaj((mlja1+1)*mtasks), stat=aloc)                              !/*mgd      1169*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1170*/
      lcaj=0                                                                    !/*mgd      1171*/
      if(allocated(lse))deallocate (lse)                                        !/*mgd      1172*/
      allocate (lse(mxa1*mtasks), stat=aloc)                                    !/*mgd      1173*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1174*/
      lse=0                                                                     !/*mgd      1175*/
!                                                                               !/*mgd      1176*/
!        allocate edac variables - unswept.                                     !/*mgd      1177*/
!        real.                                                                  !/*mgd      1178*/
#ifdef MESHTAL                                                                  /*!mgdk        3*/
      if(allocated(gd1))deallocate (gd1)                                        !/*mgdk        4*/
      allocate (gd1(ngd1), stat=aloc)                                           !/*mgdk        5*/
      if(aloc.ne.0)go to 10                                                     !/*mgdk        6*/
      gd1=0                                                                     !/*mgdk        7*/
      if(allocated(gd2))deallocate (gd2)                                        !/*mgdk        8*/
      allocate (gd2(ngd2), stat=aloc)                                           !/*mgdk        9*/
      if(aloc.ne.0)go to 10                                                     !/*mgdk       10*/
      gd2=0                                                                     !/*mgdk       11*/
      if(allocated(gd3))deallocate (gd3)                                        !/*mgdk       12*/
      allocate (gd3(ngd3), stat=aloc)                                           !/*mgdk       13*/
      if(aloc.ne.0)go to 10                                                     !/*mgdk       14*/
      gd3=0                                                                     !/*mgdk       15*/
#endif                                                                          /*!mgdk       16*/
      if(allocated(ptr))deallocate (ptr)                                        !/*mgd      1179*/
      allocate (ptr(iptra1), stat=aloc)                                         !/*mgd      1180*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1181*/
      ptr=0                                                                     !/*mgd      1182*/
      if(allocated(pts))deallocate (pts)                                        !/*mgd      1183*/
      allocate (pts(iptra2), stat=aloc)                                         !/*mgd      1184*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1185*/
      pts=0                                                                     !/*mgd      1186*/
      if(allocated(rng))deallocate (rng)                                        !/*mgd      1187*/
      allocate (rng(mipt,nee_max*nmat1), stat=aloc)                             !/*mgd      1188*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1189*/
      rng=0                                                                     !/*mgd      1190*/
      if(allocated(scr))deallocate (scr)                                        !/*mgd      1191*/
      allocate (scr(min(1_kindi,icw1)*igm1*mxa1), stat=aloc)                    !/*mgd      1192*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1193*/
      scr=0                                                                     !/*mgd      1194*/
!        integer.                                                               !/*mgd      1195*/
      if(allocated(ixc))deallocate (ixc)                                        !/*mgd      1196*/
      allocate (ixc(61,mxe1), stat=aloc)                                        !/*mgd      1197*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1198*/
      ixc=0                                                                     !/*mgd      1199*/
      if(allocated(jcond))deallocate (jcond)                                    !/*mgd      1200*/
      allocate (jcond(nmat1*kc), stat=aloc)                                     !/*mgd      1201*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1202*/
      jcond=0                                                                   !/*mgd      1203*/
      if(allocated(jemi))deallocate (jemi)                                      !/*mgd      1204*/
      allocate (jemi(nmat1*kc), stat=aloc)                                      !/*mgd      1205*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1206*/
      jemi=0                                                                    !/*mgd      1207*/
      if(allocated(jmt))deallocate (jmt)                                        !/*mgd      1208*/
      allocate (jmt(indt1), stat=aloc)                                          !/*mgd      1209*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1210*/
      jmt=0                                                                     !/*mgd      1211*/
      if(allocated(kdup))deallocate (kdup)                                      !/*mgd      1212*/
      allocate (kdup(mipt+2,max(ndup1(1),ndup1(2),ndup1(3))), stat=aloc)        !/*mgd      1213*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1214*/
      kdup=0                                                                    !/*mgd      1215*/
      if(allocated(kmt))deallocate (kmt)                                        !/*mgd      1216*/
      allocate (kmt(3,indt1), stat=aloc)                                        !/*mgd      1217*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1218*/
      kmt=0                                                                     !/*mgd      1219*/
      if(allocated(kxd))deallocate (kxd)                                        !/*mgd      1220*/
      allocate (kxd(mxe1), stat=aloc)                                           !/*mgd      1221*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1222*/
      kxd=0                                                                     !/*mgd      1223*/
      if(allocated(lbb))deallocate (lbb)                                        !/*mgd      1224*/
      allocate (lbb(mtasks), stat=aloc)                                         !/*mgd      1225*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1226*/
      lbb=0                                                                     !/*mgd      1227*/
!                                                                               !/*mgd      1228*/
!        allocate edac variables - swept.                                       !/*mgd      1229*/
!        real.                                                                  !/*mgd      1230*/
      if(allocated(drc))deallocate (drc)                                        !/*mgd      1231*/
#ifdef RADIOG                                                                   /*!mgd      1232*/
      allocate (drc(20,ndnd1*mtasks), stat=aloc)                                !/*mgd      1233*/
#else                                                                           /*!mgd      1234*/
      allocate (drc(18,ndnd1*mtasks), stat=aloc)                                !/*mgd      1235*/
#endif                                                                          /*!mgd      1236*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1237*/
      drc=0                                                                     !/*mgd      1238*/
      if(allocated(fdd))deallocate (fdd)                                        !/*mgd      1239*/
      allocate (fdd(2,msd1*mtasks), stat=aloc)                                  !/*mgd      1240*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1241*/
      fdd=0                                                                     !/*mgd      1242*/
      if(allocated(pik))deallocate (pik)                                        !/*mgd      1261*/
      allocate (pik(npikmt1*mtasks), stat=aloc)                                 !/*mgd      1262*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1263*/
      pik=0                                                                     !/*mgd      1264*/
      if(allocated(ptb))deallocate (ptb)                                        !/*mgd      1265*/
      allocate (ptb(5,npert1*mnnm1*mtasks), stat=aloc)                          !/*mgd      1266*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1267*/
      ptb=0                                                                     !/*mgd      1268*/
      if(allocated(rtc))deallocate (rtc)                                        !/*mgd      1269*/
      allocate (rtc(15,mxe1*mtasks), stat=aloc)                                 !/*mgd      1270*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1271*/
      rtc=0                                                                     !/*mgd      1272*/
      if(allocated(tgp))deallocate (tgp)                                        !/*mgd      1273*/
      allocate (tgp(mxe1*mtasks), stat=aloc)                                    !/*mgd      1274*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1275*/
      tgp=0                                                                     !/*mgd      1276*/
#ifdef MESHTAL                                                                  /*!mgdk       17*/
      if(allocated(gdata))deallocate (gdata)                                    !/*mgdk       18*/
      allocate (gdata(ngdt), stat=aloc)                                         !/*mgdk       19*/
      if(aloc.ne.0)go to 10                                                     !/*mgdk       20*/
      gdata=0                                                                   !/*mgdk       21*/
#endif                                                                          /*!mgdk       22*/
!        integer.                                                               !/*mgd      1277*/
      if(allocated(iafg))deallocate (iafg)                                      !/*mgd      1278*/
      allocate (iafg((mgww1+mipt)*nwwma1*mtasks), stat=aloc)                    !/*mgd      1279*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1280*/
      iafg=0                                                                    !/*mgd      1281*/
      if(allocated(ifl))deallocate (ifl)                                        !/*mgd      1282*/
      allocate (ifl(mxa1*min(1_kindi,max(0_kindi,kfl1-1))*mtasks),              !/*mgd      1283*/
     &  stat=aloc)                                                              !/*mgd      1284*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1285*/
      ifl=0                                                                     !/*mgd      1286*/
      if(allocated(ipac2))deallocate (ipac2)                                    !/*mgd      1287*/
      allocate (ipac2(mxa1*mtasks), stat=aloc)                                  !/*mgd      1288*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1289*/
      ipac2=0                                                                   !/*mgd      1290*/
      if(allocated(jfl))deallocate (jfl)                                        !/*mgd      1291*/
      allocate (jfl(mxj1*mod(kfl1,2_kindi)*mtasks), stat=aloc)                  !/*mgd      1292*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1293*/
      jfl=0                                                                     !/*mgd      1294*/
      if(allocated(jft))deallocate (jft)                                        !/*mgd      1295*/
      allocate (jft(ntal1*mtasks), stat=aloc)                                   !/*mgd      1296*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1297*/
      jft=0                                                                     !/*mgd      1298*/
      if(allocated(jptb))deallocate (jptb)                                      !/*mgd      1299*/
      allocate (jptb(npert1*mtasks), stat=aloc)                                 !/*mgd      1300*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1301*/
      jptb=0                                                                    !/*mgd      1302*/
      if(allocated(ktc))deallocate (ktc)                                        !/*mgd      1303*/
      allocate (ktc(2,mxe1*mtasks), stat=aloc)                                  !/*mgd      1304*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1305*/
      ktc=0                                                                     !/*mgd      1306*/
      if(allocated(nmcp))deallocate (nmcp)                                      !/*mgd      1307*/
      allocate (nmcp(4,mgww1*nwwma1*mtasks), stat=aloc)                         !/*mgd      1308*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1309*/
      nmcp=0                                                                    !/*mgd      1310*/
!                                                                               !/*mgd      1311*/
!        allocate tally variables.                                              !/*mgd      1312*/
      if(allocated(shsd))deallocate (shsd)                                      !/*mgd      1313*/
      allocate (shsd(nspt,ntal1*(npert1+1)*(mt+1)), stat=aloc)                  !/*mgd      1314*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1315*/
      shsd=0.                                                                   !/*mgd      1316*/
      if(allocated(stt))deallocate (stt)                                        !/*mgd      1317*/
      allocate (stt(ntp,ntal1*(npert1+1)*(mt+1)), stat=aloc)                    !/*mgd      1318*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1319*/
      stt=0.                                                                    !/*mgd      1320*/
      if(allocated(nhsd))deallocate (nhsd)                                      !/*mgd      1321*/
      allocate (nhsd(nsp12,ntal1*(npert1+1)*(mt+1)), stat=aloc)                 !/*mgd      1322*/
      if(aloc.ne.0)go to 10                                                     !/*mgd      1323*/
      nhsd=0                                                                    !/*mgd      1324*/
      return                                                                    !/*mgd      1325*/
   10 call expire(0,'dynamic_allocate','error getting memory')                  !/*mgd      1326*/
      end subroutine dynamic_allocate                                           !/*mgd      1327*/
      subroutine imcn_allocate (flag)                                           !/*mgd      1328*/
!        allocate and deallocate dynamic variables for imcn.                    !/*mgd      1329*/
      integer, intent (in) :: flag                                              !/*mgd      1330*/
      integer(kindi) :: aloc                                                    !/*mgd      1331*/
      external expire                                                           !/*mgd      1332*/
!                                                                               !/*mgd      1333*/
      if(flag.eq.0)go to 10                                                     !/*mgd      1334*/
!                                                                               !/*mgd      1335*/
!        allocate dynamic variables for imcn.                                   !/*mgd      1336*/
!        real.                                                                  !/*mgd      1337*/
      allocate (awt(naw1), stat=aloc)                                           !/*mgd      1338*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1339*/
      awt=0.                                                                    !/*mgd      1340*/
      allocate (bbv(mseb), stat=aloc)                                           !/*mgd      1341*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1342*/
      bbv=0.                                                                    !/*mgd      1343*/
      allocate (prb(mseb), stat=aloc)                                           !/*mgd      1344*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1345*/
      prb=0.                                                                    !/*mgd      1346*/
      allocate (rtp(lrt1), stat=aloc)                                           !/*mgd      1347*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1348*/
      rtp=0.                                                                    !/*mgd      1349*/
      allocate (sfb(mseb), stat=aloc)                                           !/*mgd      1350*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1351*/
      sfb=0.                                                                    !/*mgd      1352*/
!        integer.                                                               !/*mgd      1353*/
      allocate (ipnt(2,mktc,0:ntal1), stat=aloc)                                !/*mgd      1354*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1355*/
      ipnt=0                                                                    !/*mgd      1356*/
      allocate (jtr(mxj), stat=aloc)                                            !/*mgd      1357*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1358*/
      jtr=0                                                                     !/*mgd      1359*/
      allocate (kaw(naw1), stat=aloc)                                           !/*mgd      1360*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1361*/
      kaw=0                                                                     !/*mgd      1362*/
      allocate (kdr(mxe1), stat=aloc)                                           !/*mgd      1363*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1364*/
      kdr=0                                                                     !/*mgd      1365*/
      allocate (ktr(mxa*junf), stat=aloc)                                       !/*mgd      1366*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1367*/
      ktr=0                                                                     !/*mgd      1368*/
      allocate (lxd(mipt,nmat1), stat=aloc)                                     !/*mgd      1369*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1370*/
      lxd=0                                                                     !/*mgd      1371*/
      allocate (mfm(nmfm1), stat=aloc)                                          !/*mgd      1372*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1373*/
      mfm=0                                                                     !/*mgd      1374*/
      allocate (nlv(mxa), stat=aloc)                                            !/*mgd      1375*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1376*/
      nlv=0                                                                     !/*mgd      1377*/
      return                                                                    !/*mgd      1378*/
!                                                                               !/*mgd      1379*/
!        deallocate dynamic variables for imcn.                                 !/*mgd      1380*/
   10 if(.not.allocated(awt))return                                             !/*mgd      1381*/
      deallocate (awt)                                                          !/*mgd      1382*/
      deallocate (bbv)                                                          !/*mgd      1383*/
      deallocate (prb)                                                          !/*mgd      1384*/
      deallocate (rtp)                                                          !/*mgd      1385*/
      deallocate (sfb)                                                          !/*mgd      1386*/
      deallocate (ipnt)                                                         !/*mgd      1387*/
      deallocate (kaw)                                                          !/*mgd      1388*/
      deallocate (kdr)                                                          !/*mgd      1389*/
      deallocate (ktr)                                                          !/*mgd      1390*/
      deallocate (lxd)                                                          !/*mgd      1391*/
      deallocate (mfm)                                                          !/*mgd      1392*/
      deallocate (nlv)                                                          !/*mgd      1393*/
      return                                                                    !/*mgd      1394*/
   20 call expire(0,'imcn_allocate','error getting memory')                     !/*mgd      1395*/
      end subroutine imcn_allocate                                              !/*mgd      1396*/
      subroutine volume_allocate (flag)                                         !/*mgd      1397*/
!        allocate and deallocate dynamic variables for imcn volume.             !/*mgd      1398*/
      integer, intent (in) :: flag                                              !/*mgd      1399*/
      integer(kindi) :: aloc                                                    !/*mgd      1400*/
      external expire                                                           !/*mgd      1401*/
!                                                                               !/*mgd      1402*/
      if(flag.eq.0)go to 10                                                     !/*mgd      1403*/
!                                                                               !/*mgd      1404*/
!        allocate dynamic variables for imcn volume.                            !/*mgd      1405*/
!        real.                                                                  !/*mgd      1406*/
      allocate (aras(2,mxj), stat=aloc)                                         !/*mgd      1407*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1408*/
      aras=0.                                                                   !/*mgd      1409*/
      allocate (atsa(2,ktl1), stat=aloc)                                        !/*mgd      1410*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1411*/
      atsa=0.                                                                   !/*mgd      1412*/
      allocate (rscrn(2,4*mxj+5), stat=aloc)                                    !/*mgd      1413*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1414*/
      rscrn=0.                                                                  !/*mgd      1415*/
      allocate (rsint(2,4*mxj+5), stat=aloc)                                    !/*mgd      1416*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1417*/
      rsint=0.                                                                  !/*mgd      1418*/
      allocate (scfq(5,mxj), stat=aloc)                                         !/*mgd      1419*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1420*/
      scfq=0.                                                                   !/*mgd      1421*/
      allocate (vols(2,mxa), stat=aloc)                                         !/*mgd      1422*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1423*/
      vols=0.                                                                   !/*mgd      1424*/
!        integer.                                                               !/*mgd      1425*/
      allocate (iint(4*mxj+5), stat=aloc)                                       !/*mgd      1426*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1427*/
      iint=0                                                                    !/*mgd      1428*/
      allocate (icrn(3,4*mxj+5), stat=aloc)                                     !/*mgd      1429*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1430*/
      icrn=0                                                                    !/*mgd      1431*/
      allocate (ljav(max(mlja+2,mlgc+1_kindi)), stat=aloc)                      !/*mgd      1432*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1433*/
      ljav=0                                                                    !/*mgd      1434*/
      allocate (ljsv(mxj), stat=aloc)                                           !/*mgd      1435*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1436*/
      ljsv=0                                                                    !/*mgd      1437*/
      allocate (lsat(ntal1), stat=aloc)                                         !/*mgd      1438*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1439*/
      lsat=0                                                                    !/*mgd      1440*/
      return                                                                    !/*mgd      1441*/
!                                                                               !/*mgd      1442*/
!        deallocate dynamic variables for imcn volume.                          !/*mgd      1443*/
   10 if(.not.allocated (aras))return                                           !/*mgd      1444*/
      deallocate (aras)                                                         !/*mgd      1445*/
      deallocate (atsa)                                                         !/*mgd      1446*/
      deallocate (rscrn)                                                        !/*mgd      1447*/
      deallocate (rsint)                                                        !/*mgd      1448*/
      deallocate (scfq)                                                         !/*mgd      1449*/
      deallocate (vols)                                                         !/*mgd      1450*/
      deallocate (iint)                                                         !/*mgd      1451*/
      deallocate (icrn)                                                         !/*mgd      1452*/
      deallocate (ljav)                                                         !/*mgd      1453*/
      deallocate (ljsv)                                                         !/*mgd      1454*/
      deallocate (lsat)                                                         !/*mgd      1455*/
!        this variable is allocated before volume and used in volume.           !/*mgd      1456*/
      deallocate (jtr)                                                          !/*mgd      1457*/
      return                                                                    !/*mgd      1458*/
   20 call expire(0,'volume_allocate','error getting memory')                   !/*mgd      1459*/
      end subroutine volume_allocate                                            !/*mgd      1460*/
      subroutine plotg_allocate (flag)                                          !/*mgd      1461*/
!        allocate and deallocate dynamic variables for geometry plots.          !/*mgd      1462*/
      use allocate_memory                                                       !/*mgd      1463*/
      integer, intent (in) :: flag                                              !/*mgd      1464*/
      integer(kindi) :: aloc                                                    !/*mgd      1465*/
      external expire                                                           !/*mgd      1466*/
!                                                                               !/*mgd      1467*/
      if(flag.eq.0)go to 10                                                     !/*mgd      1468*/
!                                                                               !/*mgd      1469*/
!        allocate dynamic variables for geometry plotting.                      !/*mgd      1470*/
!        real.                                                                  !/*mgd      1471*/
      allocate (amx(4,4,mxj), stat=aloc)                                        !/*mgd      1472*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1473*/
      amx=0.                                                                    !/*mgd      1474*/
      allocate (coe(6,2,mxj+1), stat=aloc)                                      !/*mgd      1475*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1476*/
      coe=0.                                                                    !/*mgd      1477*/
      allocate (crs(4*mxj+24), stat=aloc)                                       !/*mgd      1478*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1479*/
      crs=0.                                                                    !/*mgd      1480*/
      allocate (plb(3*4*mxj+100), stat=aloc)                                    !/*mgd      1481*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1482*/
      plb=0.                                                                    !/*mgd      1483*/
      allocate (qmx(3,3,2,mxj), stat=aloc)                                      !/*mgd      1484*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1485*/
      qmx=0.                                                                    !/*mgd      1486*/
      zst => chg_mem (zst,10000_kindi,aloc)                                     !/*mgd      1487*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1488*/
      zst=0.                                                                    !/*mgd      1489*/
!        integer.                                                               !/*mgd      1490*/
      allocate (jst(2,10404), stat=aloc)                                        !/*mgd      1491*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1492*/
      jst=0                                                                     !/*mgd      1493*/
      allocate (kcl(102,102), stat=aloc)                                        !/*mgd      1494*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1495*/
      kcl=0                                                                     !/*mgd      1496*/
      allocate (kfm(mxj+1), stat=aloc)                                          !/*mgd      1497*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1498*/
      kfm=0                                                                     !/*mgd      1499*/
      allocate (lcl(max(mxa,nwwm)), stat=aloc)                                  !/*mgd      1500*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1501*/
      lcl=0                                                                     !/*mgd      1502*/
      allocate (lsg(4*mxj+24), stat=aloc)                                       !/*mgd      1503*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1504*/
      lsg=0                                                                     !/*mgd      1505*/
      allocate (ncs(mxj), stat=aloc)                                            !/*mgd      1506*/
      if(aloc.ne.0)go to 20                                                     !/*mgd      1507*/
      ncs=0                                                                     !/*mgd      1508*/
      return                                                                    !/*mgd      1509*/
!                                                                               !/*mgd      1510*/
!        deallocate dynamic variables for geometry plotting.                    !/*mgd      1511*/
   10 if(.not.allocated (amx))return                                            !/*mgd      1512*/
      deallocate (amx)                                                          !/*mgd      1513*/
      deallocate (coe)                                                          !/*mgd      1514*/
      deallocate (crs)                                                          !/*mgd      1515*/
      deallocate (plb)                                                          !/*mgd      1516*/
      deallocate (qmx)                                                          !/*mgd      1517*/
      deallocate (zst)                                                          !/*mgd      1518*/
      deallocate (jst)                                                          !/*mgd      1519*/
      deallocate (kcl)                                                          !/*mgd      1520*/
      deallocate (kfm)                                                          !/*mgd      1521*/
      deallocate (lcl)                                                          !/*mgd      1522*/
      deallocate (lsg)                                                          !/*mgd      1523*/
      deallocate (ncs)                                                          !/*mgd      1524*/
      return                                                                    !/*mgd      1525*/
   20 call expire(0,'plotg_allocate','error getting memory')                    !/*mgd      1526*/
      end subroutine plotg_allocate                                             !/*mgd      1527*/
      subroutine fdac_write (unit)                                              !/*mgd      1528*/
!        write fdac variables to the specified unit.                            !/*mgd      1529*/
      integer, intent (in) :: unit                                              !/*mgd      1530*/
!                                                                               !/*mgd      1531*/
!        reals.                                                                 !/*mgd      1532*/
      write (unit) ara,asm,asp,awc,awn,cmg,den,dptb,drs,dxcp,eaa,               !/*mgdl       27*/
     & ear,eba,ebd,ebl,ebt,ech,edg,eee,eek,egg,elp,esa,ewwg,fim,                !/*mgdl       28*/
     & flc,fme,fmg,for,frc,fst,ftt,gmg,gvl,gwt,pbr,pbt,pkn,pmg,                 !/*mgdl       29*/
     & pnt,pru,pxr,qav,qax,qcn,rho,rptb,scf,smg,spf,sqq,sso,tbt,                !/*mgdl       30*/
     & tds,tmp,trf,tth,vcl,vec,vol,wgm,wgma,wwe,wwf,wwk,xnm,xse85,              !/*mgdl       31*/
     & aa9,arg,ava,avz,emaxt,                                                   !/*mgd      1538*/
#ifdef MESHTAL                                                                  /*!mgd      1539*/
     & emsh,                                                                    !/*mgd      1540*/
#endif                                                                          /*!mgd      1541*/
#ifdef SPABI                                                                    /*!mgd      1542*/
     & energies_and_params,                                                     !/*mgd      1543*/
#endif                                                                          /*!mgd      1544*/
#ifdef CEM                                                                      /*!mgd      1545*/
     & epnmin,                                                                  !/*mgd      1546*/
#endif                                                                          /*!mgd      1547*/
     & esxln,esxlp,esxmp,fmf,hsigg,pcap,sigg,sigmx,xsiso,zz9                    !/*mgd      1548*/
!                                                                               !/*mgd      1549*/
!        integers.                                                              !/*mgd      1550*/
      write (unit) idna,idne,idns,idnt,ipan,iptal,iptb,iss,itds,ixl,            !/*mgd      1551*/
     & ixs,iza,izn,jasq,jasr,jasw,jmd,jptal,jscn,jsq,jss,jtf,jun,jvc,           !/*mgd      1552*/
     & jxs,kcp,kmm,ksc,ksd,ksm,kst,ksu,ktp,kxs,laf,lat,lca,lft,lja,             !/*mgd      1553*/
     & lme,lmn,lmt,locct,locph,locst,lsc,mat,mazp,mazu,mbd,mbi,mfl,             !/*mgd      1554*/
     & ncl,ngmfl,nhtfl,nmt,npq,nptb,nsb,nsfm,nslr,nty,nxs,                      !/*mgd      1555*/
#ifdef HISTP                                                                    /*!mgd      1556*/
     & nhw,                                                                     !/*mgd      1557*/
#endif                                                                          /*!mgd      1558*/
#ifdef CEM                                                                      /*!mgd      1559*/
     & ipnf,                                                                    !/*mgd      1560*/
#endif                                                                          /*!mgd      1561*/
     & iazdex,kpptb,mel,nel                                                     !/*mgd      1562*/
      return                                                                    !/*mgd      1563*/
      end subroutine fdac_write                                                 !/*mgd      1564*/
      subroutine fdac_read (unit)                                               !/*mgd      1565*/
!        read fdac variables to the specified unit.                             !/*mgd      1566*/
      integer, intent (in) :: unit                                              !/*mgd      1567*/
!                                                                               !/*mgd      1568*/
!        reals.                                                                 !/*mgd      1569*/
      read (unit) ara,asm,asp,awc,awn,cmg,den,dptb,drs,dxcp,eaa,                !/*mgdl       32*/
     & ear,eba,ebd,ebl,ebt,ech,edg,eee,eek,egg,elp,esa,ewwg,fim,                !/*mgdl       33*/
     & flc,fme,fmg,for,frc,fst,ftt,gmg,gvl,gwt,pbr,pbt,pkn,pmg,                 !/*mgdl       34*/
     & pnt,pru,pxr,qav,qax,qcn,rho,rptb,scf,smg,spf,sqq,sso,tbt,                !/*mgdl       35*/
     & tds,tmp,trf,tth,vcl,vec,vol,wgm,wgma,wwe,wwf,wwk,xnm,xse85,              !/*mgdl       36*/
     & aa9,arg,ava,avz,emaxt,                                                   !/*mgd      1575*/
#ifdef MESHTAL                                                                  /*!mgd      1576*/
     & emsh,                                                                    !/*mgd      1577*/
#endif                                                                          /*!mgd      1578*/
#ifdef SPABI                                                                    /*!mgd      1579*/
     & energies_and_params,                                                     !/*mgd      1580*/
#endif                                                                          /*!mgd      1581*/
#ifdef CEM                                                                      /*!mgd      1582*/
     & epnmin,                                                                  !/*mgd      1583*/
#endif                                                                          /*!mgd      1584*/
     & esxln,esxlp,esxmp,fmf,hsigg,pcap,sigg,sigmx,xsiso,zz9                    !/*mgd      1585*/
!                                                                               !/*mgd      1586*/
!        integers.                                                              !/*mgd      1587*/
      read (unit) idna,idne,idns,idnt,ipan,iptal,iptb,iss,itds,ixl,             !/*mgd      1588*/
     & ixs,iza,izn,jasq,jasr,jasw,jmd,jptal,jscn,jsq,jss,jtf,jun,jvc,           !/*mgd      1589*/
     & jxs,kcp,kmm,ksc,ksd,ksm,kst,ksu,ktp,kxs,laf,lat,lca,lft,lja,             !/*mgd      1590*/
     & lme,lmn,lmt,locct,locph,locst,lsc,mat,mazp,mazu,mbd,mbi,mfl,             !/*mgd      1591*/
     & ncl,ngmfl,nhtfl,nmt,npq,nptb,nsb,nsfm,nslr,nty,nxs,                      !/*mgd      1592*/
#ifdef HISTP                                                                    /*!mgd      1593*/
     & nhw,                                                                     !/*mgd      1594*/
#endif                                                                          /*!mgd      1595*/
#ifdef CEM                                                                      /*!mgd      1596*/
     & ipnf,                                                                    !/*mgd      1597*/
#endif                                                                          /*!mgd      1598*/
     & iazdex,kpptb,mel,nel                                                     !/*mgd      1599*/
      return                                                                    !/*mgd      1600*/
      end subroutine fdac_read                                                  !/*mgd      1601*/
      subroutine vdac_write (unit)                                              !/*mgd      1602*/
!        write vdac variables to the specified unit.                            !/*mgd      1603*/
      integer, intent (in) :: unit                                              !/*mgd      1604*/
!                                                                               !/*mgd      1605*/
!        reals.                                                                 !/*mgd      1606*/
      write (unit) fso,rkpl,tfc                                                 !/*mgdl       37*/
!                                                                               !/*mgd      1611*/
!        reals dimensioned by number of tasks.                                  !/*mgd      1612*/
      write (unit)                                                              !/*mgd      1613*/
     & ddm(:,0:ntal1*(npert1+1)),                                               !/*mgd      1614*/
     & ddn(:,0:ndnd1),                                                          !/*mgd      1615*/
     & dec(:,0:mxa1*ndnd1),                                                     !/*mgd      1616*/
     & dxc(:,0:mxa1*nxnx1),                                                     !/*mgd      1617*/
     & dxd(:,:,0:mxdx*min(1_kindi,nxnx1)),                                      !/*mgdk       23*/
     & febl(:,1:max(16_kindi,igm1)),                                            !/*mgd      1619*/
     & flx(0:min(1_kindi,icw1)*mxa1*igm1),                                      !/*mgd      1620*/
     & wwfa(0:(mgww1+mipt)*nwwma1),                                             !/*mgd      1621*/
     & swwfa(0:(mgww1+mipt)*nwwma1),                                            !/*mgd      1622*/
     & pac(:,:,1:2*mxa1),                                                       !/*mgd      1623*/
     & pan(:,:,0:2*npn1),                                                       !/*mgd      1624*/
     & pwb(:,:,1:2*mxa1),                                                       !/*mgd      1625*/
     & pcc(:,0:mxa1*kpt1(2)),                                                   !/*mgd      1626*/
     & wns(:,0:mxxs1/4)                                                         !/*mgd      1627*/
      if(nsr.eq.71) write (unit) sump(0:3*npert1)                               !/*mgd      1628*/
      if(mxf.ne.0) write (unit) shsd(:,1:ntal1*(npert1+1)),                     !/*mgd      1629*/
     & tal(mxf+1:nmxf*mxf)                                                      !/*mgd      1630*/
!                                                                               !/*mgd      1631*/
!        integers.                                                              !/*mgd      1632*/
      write (unit) jfq,npsw,nsl,ntbb,lfcl                                       !/*mgd      1633*/
!                                                                               !/*mgd      1634*/
!        integers dimensioned by number of tasks.                               !/*mgd      1635*/
!        write entire laj,lcaj,lse arrays to keep task information.             !/*mgd      1636*/
      write (unit)                                                              !/*mgd      1637*/
     & isef(:,1:mxa1),                                                          !/*mgd      1638*/
     & maze(0:3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3))),                               !/*mgd      1639*/
     & ndpf(:,0:ndnd1),                                                         !/*mgd      1640*/
     & ndr(0:mxe1),                                                             !/*mgd      1641*/
     & laj,lcaj,lse                                                             !/*mgd      1642*/
      if(mxf.ne.0) write (unit) nhsd(:,1:ntal1*(npert1+1))                      !/*mgd      1643*/
      return                                                                    !/*mgd      1644*/
      end subroutine vdac_write                                                 !/*mgd      1645*/
      subroutine vdac_read (unit)                                               !/*mgd      1646*/
!        read vdac variables from the specified unit.                           !/*mgd      1647*/
      integer, intent (in) :: unit                                              !/*mgd      1648*/
!                                                                               !/*mgd      1649*/
!        reals.                                                                 !/*mgd      1650*/
      read (unit) fso,rkpl,tfc                                                  !/*mgdl       38*/
!                                                                               !/*mgd      1655*/
!        reals dimensioned by number of tasks.                                  !/*mgd      1656*/
      read (unit)                                                               !/*mgd      1657*/
     & ddm(:,0:ntal1*(npert1+1)),                                               !/*mgd      1658*/
     & ddn(:,0:ndnd1),                                                          !/*mgd      1659*/
     & dec(:,0:mxa1*ndnd1),                                                     !/*mgd      1660*/
     & dxc(:,0:mxa1*nxnx1),                                                     !/*mgd      1661*/
     & dxd(:,:,0:mxdx*min(1_kindi,nxnx1)),                                      !/*mgdk       24*/
     & febl(:,1:max(16_kindi,igm1)),                                            !/*mgd      1663*/
     & flx(0:min(1_kindi,icw1)*mxa1*igm1),                                      !/*mgd      1664*/
     & wwfa(0:(mgww1+mipt)*nwwma1),                                             !/*mgd      1665*/
     & swwfa(0:(mgww1+mipt)*nwwma1),                                            !/*mgd      1666*/
     & pac(:,:,1:2*mxa1),                                                       !/*mgd      1667*/
     & pan(:,:,0:2*npn1),                                                       !/*mgd      1668*/
     & pwb(:,:,1:2*mxa1),                                                       !/*mgd      1669*/
     & pcc(:,0:mxa1*kpt1(2)),                                                   !/*mgd      1670*/
     & wns(:,0:mxxs1/4)                                                         !/*mgd      1671*/
      if(nsr.eq.71) read (unit) sump(0:3*npert1)                                !/*mgd      1672*/
      if(mxf.ne.0) read (unit) shsd(:,1:ntal1*(npert1+1)),                      !/*mgd      1673*/
     & tal(mxf+1:nmxf*mxf)                                                      !/*mgd      1674*/
!                                                                               !/*mgd      1675*/
!        integers.                                                              !/*mgd      1676*/
      read (unit) jfq,npsw,nsl,ntbb,lfcl                                        !/*mgd      1677*/
!                                                                               !/*mgd      1678*/
!        integers dimensioned by number of tasks.                               !/*mgd      1679*/
!        read entire laj,lcaj,lse arrays to keep task information.              !/*mgd      1680*/
      read (unit)                                                               !/*mgd      1681*/
     & isef(:,1:mxa1),                                                          !/*mgd      1682*/
     & maze(0:3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3))),                               !/*mgd      1683*/
     & ndpf(:,0:ndnd1),                                                         !/*mgd      1684*/
     & ndr(0:mxe1),                                                             !/*mgd      1685*/
     & laj,lcaj,lse                                                             !/*mgd      1686*/
      if(mxf.ne.0) read (unit) nhsd(:,1:ntal1*(npert1+1))                       !/*mgd      1687*/
      return                                                                    !/*mgd      1688*/
      end subroutine vdac_read                                                  !/*mgd      1689*/
#ifdef MULTP                                                                    /*!mgd      1690*/
      subroutine fdac_pack                                                      !/*mgd      1691*/
!        pack fdac variables into a message buffer.                             !/*mgd      1692*/
      use message_passing                                                       !/*mgdk       25*/
      integer(kindi) :: i                                                       !/*mgd      1693*/
!                                                                               !/*mgd      1694*/
!        pack fdac real variables.                                              !/*mgd      1695*/
      call mputd (ara, size(ara), i)                                            !/*mgdl       39*/
      call mputd (asm, size(asm), i)                                            !/*mgd      1696*/
      call mputd (asp, size(asp), i)                                            !/*mgd      1697*/
      call mputd (awc, size(awc), i)                                            !/*mgd      1698*/
      call mputd (awn, size(awn), i)                                            !/*mgd      1699*/
      call mputd (cmg, size(cmg), i)                                            !/*mgd      1700*/
      call mputd (den, size(den), i)                                            !/*mgdl       40*/
      call mputd (dptb, size(dptb), i)                                          !/*mgd      1701*/
      call mputd (drs, size(drs), i)                                            !/*mgd      1702*/
      call mputd (dxcp, size(dxcp), i)                                          !/*mgd      1703*/
      call mputd (eaa, size(eaa), i)                                            !/*mgd      1704*/
      call mputd (ear, size(ear), i)                                            !/*mgd      1705*/
      call mputd (eba, size(eba), i)                                            !/*mgd      1706*/
      call mputd (ebd, size(ebd), i)                                            !/*mgd      1707*/
      call mputd (ebl, size(ebl), i)                                            !/*mgd      1708*/
      call mputd (ebt, size(ebt), i)                                            !/*mgd      1709*/
      call mputd (ech, size(ech), i)                                            !/*mgd      1710*/
      call mputd (edg, size(edg), i)                                            !/*mgd      1711*/
      call mputd (eee, size(eee), i)                                            !/*mgd      1712*/
      call mputd (eek, size(eek), i)                                            !/*mgd      1713*/
      call mputd (egg, size(egg), i)                                            !/*mgd      1714*/
      call mputd (elp, size(elp), i)                                            !/*mgd      1715*/
      call mputd (esa, size(esa), i)                                            !/*mgd      1716*/
      call mputd (ewwg, size(ewwg), i)                                          !/*mgd      1717*/
      call mputd (fim, size(fim), i)                                            !/*mgd      1718*/
      call mputd (flc, size(flc), i)                                            !/*mgd      1719*/
      call mputd (fme, size(fme), i)                                            !/*mgdl       41*/
      call mputd (fmg, size(fmg), i)                                            !/*mgd      1720*/
      call mputd (for, size(for), i)                                            !/*mgd      1721*/
      call mputd (frc, size(frc), i)                                            !/*mgd      1722*/
      call mputd (fst, size(fst), i)                                            !/*mgd      1723*/
      call mputd (ftt, size(ftt), i)                                            !/*mgd      1724*/
      call mputd (gmg, size(gmg), i)                                            !/*mgd      1725*/
      call mputd (gvl, size(gvl), i)                                            !/*mgd      1726*/
      call mputd (gwt, size(gwt), i)                                            !/*mgd      1727*/
      call mputd (pbr, size(pbr), i)                                            !/*mgd      1728*/
      call mputd (pbt, size(pbt), i)                                            !/*mgd      1729*/
      call mputd (pkn, size(pkn), i)                                            !/*mgd      1730*/
      call mputd (pmg, size(pmg), i)                                            !/*mgd      1731*/
      call mputd (pnt, size(pnt), i)                                            !/*mgd      1732*/
      call mputd (pru, size(pru), i)                                            !/*mgd      1733*/
      call mputd (pxr, size(pxr), i)                                            !/*mgd      1734*/
      call mputd (qav, size(qav), i)                                            !/*mgd      1735*/
      call mputd (qax, size(qax), i)                                            !/*mgd      1736*/
      call mputd (qcn, size(qcn), i)                                            !/*mgd      1737*/
      call mputd (rho, size(rho), i)                                            !/*mgdl       42*/
      call mputd (rptb, size(rptb), i)                                          !/*mgd      1738*/
      call mputd (scf, size(scf), i)                                            !/*mgd      1739*/
      call mputd (smg, size(smg), i)                                            !/*mgd      1740*/
      call mputd (spf, size(spf), i)                                            !/*mgd      1741*/
      call mputd (sqq, size(sqq), i)                                            !/*mgd      1742*/
      call mputd (sso, size(sso), i)                                            !/*mgd      1743*/
      call mputd (tbt, size(tbt), i)                                            !/*mgd      1744*/
      call mputd (tds, size(tds), i)                                            !/*mgd      1745*/
      call mputd (tmp, size(tmp), i)                                            !/*mgd      1746*/
      call mputd (trf, size(trf), i)                                            !/*mgd      1747*/
      call mputd (tth, size(tth), i)                                            !/*mgd      1748*/
      call mputd (vcl, size(vcl), i)                                            !/*mgd      1749*/
      call mputd (vec, size(vec), i)                                            !/*mgd      1750*/
      call mputd (vol, size(vol), i)                                            !/*mgdl       43*/
      call mputd (wgm, size(wgm), i)                                            !/*mgd      1751*/
      call mputd (wgma, size(wgma), i)                                          !/*mgd      1752*/
      call mputd (wwe, size(wwe), i)                                            !/*mgd      1753*/
      call mputd (wwf, size(wwf), i)                                            !/*mgd      1754*/
      call mputd (wwk, size(wwk), i)                                            !/*mgd      1755*/
      call mputd (xnm, size(xnm), i)                                            !/*mgd      1756*/
      call mputd (xse85, size(xse85), i)                                        !/*mgd      1757*/
!                                                                               !/*mgdk       26*/
!        pack fdac high-energy variables.                                       !/*mgdk       27*/
      call mputd (aa9, size(aa9), i)                                            !/*mgdk       28*/
      call mputd (arg, size(arg), i)                                            !/*mgdk       29*/
      call mputd (ava, size(ava), i)                                            !/*mgdk       30*/
      call mputd (avz, size(avz), i)                                            !/*mgdk       31*/
      call mputd (emaxt, size(emaxt), i)                                        !/*mgdk       32*/
#ifdef MESHTAL                                                                  /*!mgdk       33*/
      call mputd (emsh, size(emsh), i)                                          !/*mgdk       34*/
#endif                                                                          /*!mgdk       35*/
#ifdef SPABI                                                                    /*!mgdk       36*/
      call mputd (energies_and_params, size(energies_and_params), i)            !/*mgdk       37*/
#endif                                                                          /*!mgdk       38*/
#ifdef CEM                                                                      /*!mgdk       39*/
      call mputd (epnmin, size(epnmin), i)                                      !/*mgdk       40*/
#endif                                                                          /*!mgdk       41*/
      call mputd (esxln, size(esxln), i)                                        !/*mgdk       42*/
      call mputd (esxlp, size(esxlp), i)                                        !/*mgdk       43*/
      call mputd (esxmp, size(esxmp), i)                                        !/*mgdk       44*/
      call mputd (fmf, size(fmf), i)                                            !/*mgdk       45*/
      call mputd (hsigg, size(hsigg), i)                                        !/*mgdk       46*/
      call mputd (pcap, size(pcap), i)                                          !/*mgdk       47*/
      call mputd (sigg, size(sigg), i)                                          !/*mgdk       48*/
      call mputd (sigmx, size(sigmx), i)                                        !/*mgdk       49*/
      call mputd (xsiso, size(xsiso), i)                                        !/*mgdk       50*/
      call mputd (zz9, size(zz9), i)                                            !/*mgdk       51*/
!                                                                               !/*mgd      1758*/
!        pack fdac integer variables.                                           !/*mgd      1759*/
      call mputi (idna, size(idna), i)                                          !/*mgd      1760*/
      call mputi (idne, size(idne), i)                                          !/*mgd      1761*/
      call mputi (idns, size(idns), i)                                          !/*mgd      1762*/
      call mputi (idnt, size(idnt), i)                                          !/*mgd      1763*/
      call mputi (ipan, size(ipan), i)                                          !/*mgd      1764*/
      call mputi (iptal, size(iptal), i)                                        !/*mgd      1765*/
      call mputi (iptb, size(iptb), i)                                          !/*mgd      1766*/
      call mputi (iss, size(iss), i)                                            !/*mgd      1767*/
      call mputi (itds, size(itds), i)                                          !/*mgd      1768*/
      call mputi (ixl, size(ixl), i)                                            !/*mgd      1769*/
      call mputi (ixs, size(ixs), i)                                            !/*mgd      1770*/
      call mputi (iza, size(iza), i)                                            !/*mgd      1771*/
      call mputi (izn, size(izn), i)                                            !/*mgd      1772*/
      call mputi (jasq, size(jasq), i)                                          !/*mgd      1773*/
      call mputi (jasr, size(jasr), i)                                          !/*mgd      1774*/
      call mputi (jasw, size(jasw), i)                                          !/*mgd      1775*/
      call mputi (jmd, size(jmd), i)                                            !/*mgd      1776*/
      call mputi (jptal, size(jptal), i)                                        !/*mgd      1777*/
      call mputi (jscn, size(jscn), i)                                          !/*mgd      1778*/
      call mputi (jsq, size(jsq), i)                                            !/*mgd      1779*/
      call mputi (jss, size(jss), i)                                            !/*mgd      1780*/
      call mputi (jtf, size(jtf), i)                                            !/*mgd      1781*/
      call mputi (jun, size(jun), i)                                            !/*mgd      1782*/
      call mputi (jvc, size(jvc), i)                                            !/*mgd      1783*/
      call mputi (jxs, size(jxs), i)                                            !/*mgd      1784*/
      call mputi (kcp, size(kcp), i)                                            !/*mgd      1785*/
      call mputi (kmm, size(kmm), i)                                            !/*mgd      1786*/
      call mputi (ksc, size(ksc), i)                                            !/*mgd      1787*/
      call mputi (ksd, size(ksd), i)                                            !/*mgd      1788*/
      call mputi (ksm, size(ksm), i)                                            !/*mgd      1789*/
      call mputi (kst, size(kst), i)                                            !/*mgd      1790*/
      call mputi (ksu, size(ksu), i)                                            !/*mgd      1791*/
      call mputi (ktp, size(ktp), i)                                            !/*mgd      1792*/
      call mputi (kxs, size(kxs), i)                                            !/*mgd      1793*/
      call mputi (laf, size(laf), i)                                            !/*mgd      1794*/
      call mputi (lat, size(lat), i)                                            !/*mgd      1795*/
      call mputi (lca, size(lca), i)                                            !/*mgd      1796*/
      call mputi (lft, size(lft), i)                                            !/*mgd      1797*/
      call mputi (lja, size(lja), i)                                            !/*mgd      1798*/
      call mputi (lme, size(lme), i)                                            !/*mgd      1799*/
      call mputi (lmn, size(lmn), i)                                            !/*mgd      1800*/
      call mputi (lmt, size(lmt), i)                                            !/*mgd      1801*/
      call mputi (locct, size(locct), i)                                        !/*mgd      1802*/
      call mputi (locph, size(locph), i)                                        !/*mgd      1803*/
      call mputi (locst, size(locst), i)                                        !/*mgd      1804*/
      call mputi (lsc, size(lsc), i)                                            !/*mgd      1805*/
      call mputi (mat, size(mat), i)                                            !/*mgd      1806*/
      call mputi (mazp, size(mazp), i)                                          !/*mgd      1807*/
      call mputi (mazu, size(mazu), i)                                          !/*mgd      1808*/
      call mputi (mbd, size(mbd), i)                                            !/*mgd      1809*/
      call mputi (mbi, size(mbi), i)                                            !/*mgd      1810*/
      call mputi (mfl, size(mfl), i)                                            !/*mgd      1811*/
      call mputi (ncl, size(ncl), i)                                            !/*mgd      1812*/
      call mputi (ngmfl, size(ngmfl), i)                                        !/*mgd      1813*/
      call mputi (nhtfl, size(nhtfl), i)                                        !/*mgd      1814*/
      call mputi (nmt, size(nmt), i)                                            !/*mgd      1815*/
      call mputi (npq, size(npq), i)                                            !/*mgd      1816*/
      call mputi (nptb, size(nptb), i)                                          !/*mgd      1817*/
      call mputi (nsb, size(nsb), i)                                            !/*mgd      1818*/
      call mputi (nsfm, size(nsfm), i)                                          !/*mgd      1819*/
      call mputi (nslr, size(nslr), i)                                          !/*mgd      1820*/
      call mputi (nty, size(nty), i)                                            !/*mgd      1821*/
      call mputi (nxs, size(nxs), i)                                            !/*mgd      1822*/
!                                                                               !/*mgdk       52*/
!        pack fdac high-energy variables.                                       !/*mgdk       53*/
      call mputi (iazdex, size(iazdex), i)                                      !/*mgdk       54*/
      call mputi (kpptb, size(kpptb), i)                                        !/*mgdk       55*/
      call mputi (mel, size(mel), i)                                            !/*mgdk       56*/
      call mputi (nel, size(nel), i)                                            !/*mgdk       57*/
#ifdef HISTP                                                                    /*!mgdk       58*/
      call mputi (nhw, size(nhw), i)                                            !/*mgdk       59*/
#endif                                                                          /*!mgdk       60*/
#ifdef CEM                                                                      /*!mgdk       61*/
      call mputi (ipnf, size(ipnf), i)                                          !/*mgdk       62*/
#endif                                                                          /*!mgdk       63*/
      return                                                                    !/*mgd      1823*/
      end subroutine fdac_pack                                                  !/*mgd      1824*/
      subroutine fdac_unpack                                                    !/*mgd      1825*/
!        unpack fdac variables into a message buffer.                           !/*mgd      1826*/
      use message_passing                                                       !/*mgdk       64*/
      integer(kindi) :: i                                                       !/*mgd      1827*/
!                                                                               !/*mgd      1828*/
!        unpack fdac real variables.                                            !/*mgd      1829*/
      call mgetd (ara, size(ara), i)                                            !/*mgdl       44*/
      call mgetd (asm, size(asm), i)                                            !/*mgd      1830*/
      call mgetd (asp, size(asp), i)                                            !/*mgd      1831*/
      call mgetd (awc, size(awc), i)                                            !/*mgd      1832*/
      call mgetd (awn, size(awn), i)                                            !/*mgd      1833*/
      call mgetd (cmg, size(cmg), i)                                            !/*mgd      1834*/
      call mgetd (den, size(den), i)                                            !/*mgdl       45*/
      call mgetd (dptb, size(dptb), i)                                          !/*mgd      1835*/
      call mgetd (drs, size(drs), i)                                            !/*mgd      1836*/
      call mgetd (dxcp, size(dxcp), i)                                          !/*mgd      1837*/
      call mgetd (eaa, size(eaa), i)                                            !/*mgd      1838*/
      call mgetd (ear, size(ear), i)                                            !/*mgd      1839*/
      call mgetd (eba, size(eba), i)                                            !/*mgd      1840*/
      call mgetd (ebd, size(ebd), i)                                            !/*mgd      1841*/
      call mgetd (ebl, size(ebl), i)                                            !/*mgd      1842*/
      call mgetd (ebt, size(ebt), i)                                            !/*mgd      1843*/
      call mgetd (ech, size(ech), i)                                            !/*mgd      1844*/
      call mgetd (edg, size(edg), i)                                            !/*mgd      1845*/
      call mgetd (eee, size(eee), i)                                            !/*mgd      1846*/
      call mgetd (eek, size(eek), i)                                            !/*mgd      1847*/
      call mgetd (egg, size(egg), i)                                            !/*mgd      1848*/
      call mgetd (elp, size(elp), i)                                            !/*mgd      1849*/
      call mgetd (esa, size(esa), i)                                            !/*mgd      1850*/
      call mgetd (ewwg, size(ewwg), i)                                          !/*mgd      1851*/
      call mgetd (fim, size(fim), i)                                            !/*mgd      1852*/
      call mgetd (flc, size(flc), i)                                            !/*mgd      1853*/
      call mgetd (fme, size(fme), i)                                            !/*mgdl       46*/
      call mgetd (fmg, size(fmg), i)                                            !/*mgd      1854*/
      call mgetd (for, size(for), i)                                            !/*mgd      1855*/
      call mgetd (frc, size(frc), i)                                            !/*mgd      1856*/
      call mgetd (fst, size(fst), i)                                            !/*mgd      1857*/
      call mgetd (ftt, size(ftt), i)                                            !/*mgd      1858*/
      call mgetd (gmg, size(gmg), i)                                            !/*mgd      1859*/
      call mgetd (gvl, size(gvl), i)                                            !/*mgd      1860*/
      call mgetd (gwt, size(gwt), i)                                            !/*mgd      1861*/
      call mgetd (pbr, size(pbr), i)                                            !/*mgd      1862*/
      call mgetd (pbt, size(pbt), i)                                            !/*mgd      1863*/
      call mgetd (pkn, size(pkn), i)                                            !/*mgd      1864*/
      call mgetd (pmg, size(pmg), i)                                            !/*mgd      1865*/
      call mgetd (pnt, size(pnt), i)                                            !/*mgd      1866*/
      call mgetd (pru, size(pru), i)                                            !/*mgd      1867*/
      call mgetd (pxr, size(pxr), i)                                            !/*mgd      1868*/
      call mgetd (qav, size(qav), i)                                            !/*mgd      1869*/
      call mgetd (qax, size(qax), i)                                            !/*mgd      1870*/
      call mgetd (qcn, size(qcn), i)                                            !/*mgd      1871*/
      call mgetd (rho, size(rho), i)                                            !/*mgdl       47*/
      call mgetd (rptb, size(rptb), i)                                          !/*mgd      1872*/
      call mgetd (scf, size(scf), i)                                            !/*mgd      1873*/
      call mgetd (smg, size(smg), i)                                            !/*mgd      1874*/
      call mgetd (spf, size(spf), i)                                            !/*mgd      1875*/
      call mgetd (sqq, size(sqq), i)                                            !/*mgd      1876*/
      call mgetd (sso, size(sso), i)                                            !/*mgd      1877*/
      call mgetd (tbt, size(tbt), i)                                            !/*mgd      1878*/
      call mgetd (tds, size(tds), i)                                            !/*mgd      1879*/
      call mgetd (tmp, size(tmp), i)                                            !/*mgd      1880*/
      call mgetd (trf, size(trf), i)                                            !/*mgd      1881*/
      call mgetd (tth, size(tth), i)                                            !/*mgd      1882*/
      call mgetd (vcl, size(vcl), i)                                            !/*mgd      1883*/
      call mgetd (vec, size(vec), i)                                            !/*mgd      1884*/
      call mgetd (vol, size(vol), i)                                            !/*mgdl       48*/
      call mgetd (wgm, size(wgm), i)                                            !/*mgd      1885*/
      call mgetd (wgma, size(wgma), i)                                          !/*mgd      1886*/
      call mgetd (wwe, size(wwe), i)                                            !/*mgd      1887*/
      call mgetd (wwf, size(wwf), i)                                            !/*mgd      1888*/
      call mgetd (wwk, size(wwk), i)                                            !/*mgd      1889*/
      call mgetd (xnm, size(xnm), i)                                            !/*mgd      1890*/
      call mgetd (xse85, size(xse85), i)                                        !/*mgd      1891*/
!                                                                               !/*mgdk       65*/
!        unpack fdac high-energy variables.                                     !/*mgdk       66*/
      call mgetd (aa9, size(aa9), i)                                            !/*mgdk       67*/
      call mgetd (arg, size(arg), i)                                            !/*mgdk       68*/
      call mgetd (ava, size(ava), i)                                            !/*mgdk       69*/
      call mgetd (avz, size(avz), i)                                            !/*mgdk       70*/
      call mgetd (emaxt, size(emaxt), i)                                        !/*mgdk       71*/
#ifdef MESHTAL                                                                  /*!mgdk       72*/
      call mgetd (emsh, size(emsh), i)                                          !/*mgdk       73*/
#endif                                                                          /*!mgdk       74*/
#ifdef SPABI                                                                    /*!mgdk       75*/
      call mgetd (energies_and_params, size(energies_and_params), i)            !/*mgdk       76*/
#endif                                                                          /*!mgdk       77*/
#ifdef CEM                                                                      /*!mgdk       78*/
      call mgetd (epnmin, size(epnmin), i)                                      !/*mgdk       79*/
#endif                                                                          /*!mgdk       80*/
      call mgetd (esxln, size(esxln), i)                                        !/*mgdk       81*/
      call mgetd (esxlp, size(esxlp), i)                                        !/*mgdk       82*/
      call mgetd (esxmp, size(esxmp), i)                                        !/*mgdk       83*/
      call mgetd (fmf, size(fmf), i)                                            !/*mgdk       84*/
      call mgetd (hsigg, size(hsigg), i)                                        !/*mgdk       85*/
      call mgetd (pcap, size(pcap), i)                                          !/*mgdk       86*/
      call mgetd (sigg, size(sigg), i)                                          !/*mgdk       87*/
      call mgetd (sigmx, size(sigmx), i)                                        !/*mgdk       88*/
      call mgetd (xsiso, size(xsiso), i)                                        !/*mgdk       89*/
      call mgetd (zz9, size(zz9), i)                                            !/*mgdk       90*/
!                                                                               !/*mgd      1892*/
!        unpack fdac integer variables.                                         !/*mgd      1893*/
      call mgeti (idna, size(idna), i)                                          !/*mgd      1894*/
      call mgeti (idne, size(idne), i)                                          !/*mgd      1895*/
      call mgeti (idns, size(idns), i)                                          !/*mgd      1896*/
      call mgeti (idnt, size(idnt), i)                                          !/*mgd      1897*/
      call mgeti (ipan, size(ipan), i)                                          !/*mgd      1898*/
      call mgeti (iptal, size(iptal), i)                                        !/*mgd      1899*/
      call mgeti (iptb, size(iptb), i)                                          !/*mgd      1900*/
      call mgeti (iss, size(iss), i)                                            !/*mgd      1901*/
      call mgeti (itds, size(itds), i)                                          !/*mgd      1902*/
      call mgeti (ixl, size(ixl), i)                                            !/*mgd      1903*/
      call mgeti (ixs, size(ixs), i)                                            !/*mgd      1904*/
      call mgeti (iza, size(iza), i)                                            !/*mgd      1905*/
      call mgeti (izn, size(izn), i)                                            !/*mgd      1906*/
      call mgeti (jasq, size(jasq), i)                                          !/*mgd      1907*/
      call mgeti (jasr, size(jasr), i)                                          !/*mgd      1908*/
      call mgeti (jasw, size(jasw), i)                                          !/*mgd      1909*/
      call mgeti (jmd, size(jmd), i)                                            !/*mgd      1910*/
      call mgeti (jptal, size(jptal), i)                                        !/*mgd      1911*/
      call mgeti (jscn, size(jscn), i)                                          !/*mgd      1912*/
      call mgeti (jsq, size(jsq), i)                                            !/*mgd      1913*/
      call mgeti (jss, size(jss), i)                                            !/*mgd      1914*/
      call mgeti (jtf, size(jtf), i)                                            !/*mgd      1915*/
      call mgeti (jun, size(jun), i)                                            !/*mgd      1916*/
      call mgeti (jvc, size(jvc), i)                                            !/*mgd      1917*/
      call mgeti (jxs, size(jxs), i)                                            !/*mgd      1918*/
      call mgeti (kcp, size(kcp), i)                                            !/*mgd      1919*/
      call mgeti (kmm, size(kmm), i)                                            !/*mgd      1920*/
      call mgeti (ksc, size(ksc), i)                                            !/*mgd      1921*/
      call mgeti (ksd, size(ksd), i)                                            !/*mgd      1922*/
      call mgeti (ksm, size(ksm), i)                                            !/*mgd      1923*/
      call mgeti (kst, size(kst), i)                                            !/*mgd      1924*/
      call mgeti (ksu, size(ksu), i)                                            !/*mgd      1925*/
      call mgeti (ktp, size(ktp), i)                                            !/*mgd      1926*/
      call mgeti (kxs, size(kxs), i)                                            !/*mgd      1927*/
      call mgeti (laf, size(laf), i)                                            !/*mgd      1928*/
      call mgeti (lat, size(lat), i)                                            !/*mgd      1929*/
      call mgeti (lca, size(lca), i)                                            !/*mgd      1930*/
      call mgeti (lft, size(lft), i)                                            !/*mgd      1931*/
      call mgeti (lja, size(lja), i)                                            !/*mgd      1932*/
      call mgeti (lme, size(lme), i)                                            !/*mgd      1933*/
      call mgeti (lmn, size(lmn), i)                                            !/*mgd      1934*/
      call mgeti (lmt, size(lmt), i)                                            !/*mgd      1935*/
      call mgeti (locct, size(locct), i)                                        !/*mgd      1936*/
      call mgeti (locph, size(locph), i)                                        !/*mgd      1937*/
      call mgeti (locst, size(locst), i)                                        !/*mgd      1938*/
      call mgeti (lsc, size(lsc), i)                                            !/*mgd      1939*/
      call mgeti (mat, size(mat), i)                                            !/*mgd      1940*/
      call mgeti (mazp, size(mazp), i)                                          !/*mgd      1941*/
      call mgeti (mazu, size(mazu), i)                                          !/*mgd      1942*/
      call mgeti (mbd, size(mbd), i)                                            !/*mgd      1943*/
      call mgeti (mbi, size(mbi), i)                                            !/*mgd      1944*/
      call mgeti (mfl, size(mfl), i)                                            !/*mgd      1945*/
      call mgeti (ncl, size(ncl), i)                                            !/*mgd      1946*/
      call mgeti (ngmfl, size(ngmfl), i)                                        !/*mgd      1947*/
      call mgeti (nhtfl, size(nhtfl), i)                                        !/*mgd      1948*/
      call mgeti (nmt, size(nmt), i)                                            !/*mgd      1949*/
      call mgeti (npq, size(npq), i)                                            !/*mgd      1950*/
      call mgeti (nptb, size(nptb), i)                                          !/*mgd      1951*/
      call mgeti (nsb, size(nsb), i)                                            !/*mgd      1952*/
      call mgeti (nsfm, size(nsfm), i)                                          !/*mgd      1953*/
      call mgeti (nslr, size(nslr), i)                                          !/*mgd      1954*/
      call mgeti (nty, size(nty), i)                                            !/*mgd      1955*/
      call mgeti (nxs, size(nxs), i)                                            !/*mgd      1956*/
!                                                                               !/*mgdk       91*/
!        unpack fdac high-energy variables.                                     !/*mgdk       92*/
      call mgeti (iazdex, size(iazdex), i)                                      !/*mgdk       93*/
      call mgeti (kpptb, size(kpptb), i)                                        !/*mgdk       94*/
      call mgeti (mel, size(mel), i)                                            !/*mgdk       95*/
      call mgeti (nel, size(nel), i)                                            !/*mgdk       96*/
#ifdef HISTP                                                                    /*!mgdk       97*/
      call mgeti (nhw, size(nhw), i)                                            !/*mgdk       98*/
#endif                                                                          /*!mgdk       99*/
#ifdef CEM                                                                      /*!mgdk      100*/
      call mgeti (ipnf, size(ipnf), i)                                          !/*mgdk      101*/
#endif                                                                          /*!mgdk      102*/
      return                                                                    !/*mgd      1957*/
      end subroutine fdac_unpack                                                !/*mgd      1958*/
      subroutine vdac_pack                                                      !/*mgd      1959*/
!        pack vdac variables into a message buffer.                             !/*mgd      1960*/
      use message_passing                                                       !/*mgdk      103*/
      integer(kindi) :: i                                                       !/*mgd      1961*/
!                                                                               !/*mgd      1962*/
!        pack vdac real variables.                                              !/*mgd      1963*/
      call mputd (fso, size(fso), i)                                            !/*mgdk      105*/
      call mputd (rkpl, size(rkpl), i)                                          !/*mgd      1966*/
      call mputd (tfc, size(tfc), i)                                            !/*mgd      1967*/
      call mputd (ddm, size(ddm), i)                                            !/*mgd      1973*/
      call mputd (ddn, size(ddn), i)                                            !/*mgd      1974*/
      call mputd (dec, size(dec), i)                                            !/*mgd      1975*/
      call mputd (dxc, size(dxc), i)                                            !/*mgd      1976*/
      call mputd (dxd, size(dxd), i)                                            !/*mgd      1977*/
      call mputd (febl, size(febl), i)                                          !/*mgd      1978*/
      call mputd (flx, size(flx), i)                                            !/*mgd      1979*/
      call mputd (wwfa, size(wwfa), i)                                          !/*mgd      1981*/
      call mputd (swwfa, size(swwfa), i)                                        !/*mgd      1982*/
      call mputd (pac, size(pac), i)                                            !/*mgd      1983*/
      call mputd (pan, size(pan), i)                                            !/*mgd      1984*/
      call mputd (pwb, size(pwb), i)                                            !/*mgd      1985*/
      call mputd (pcc, size(pcc), i)                                            !/*mgd      1986*/
      if(nsr.eq.71)call mputd (sump, size(sump), i)                             !/*mgd      1987*/
      call mputd (wns, size(wns), i)                                            !/*mgd      1988*/
!                                                                               !/*mgd      1991*/
!        pack vdac integer variables.                                           !/*mgd      1992*/
      call mputi (jfq, size(jfq), i)                                            !/*mgdk      107*/
      call mputi (npsw, size(npsw), i)                                          !/*mgdk      108*/
      call mputi (nsl, size(nsl), i)                                            !/*mgdk      109*/
      call mputi (ntbb, size(ntbb), i)                                          !/*mgdk      110*/
      call mputi (lfcl, size(lfcl), i)                                          !/*mgdk      111*/
      call mputi (isef, size(isef), i)                                          !/*mgdk      112*/
      call mputi (maze, size(maze), i)                                          !/*mgdk      113*/
      call mputi (ndpf, size(ndpf), i)                                          !/*mgdk      114*/
      call mputi (ndr, size(ndr), i)                                            !/*mgdk      115*/
      call mputi (laj, size(laj), i)                                            !/*mgdk      116*/
      call mputi (lcaj, size(lcaj), i)                                          !/*mgdk      117*/
      call mputi (lse, size(lse), i)                                            !/*mgdk      118*/
      return                                                                    !/*mgd      2005*/
      end subroutine vdac_pack                                                  !/*mgd      2006*/
      subroutine vdac_unpack                                                    !/*mgd      2007*/
!        unpack vdac variables into a message buffer.                           !/*mgd      2008*/
      use message_passing                                                       !/*mgdk      119*/
      integer(kindi) :: i                                                       !/*mgd      2009*/
!                                                                               !/*mgd      2010*/
!        unpack vdac real variables.                                            !/*mgd      2011*/
      call mgetd (fso, size(fso), i)                                            !/*mgdk      121*/
      call mgetd (rkpl, size(rkpl), i)                                          !/*mgd      2014*/
      call mgetd (tfc, size(tfc), i)                                            !/*mgd      2015*/
      call mgetd (ddm, size(ddm), i)                                            !/*mgd      2021*/
      call mgetd (ddn, size(ddn), i)                                            !/*mgd      2022*/
      call mgetd (dec, size(dec), i)                                            !/*mgd      2023*/
      call mgetd (dxc, size(dxc), i)                                            !/*mgd      2024*/
      call mgetd (dxd, size(dxd), i)                                            !/*mgd      2025*/
      call mgetd (febl, size(febl), i)                                          !/*mgd      2026*/
      call mgetd (flx, size(flx), i)                                            !/*mgd      2027*/
      call mgetd (wwfa, size(wwfa), i)                                          !/*mgd      2029*/
      call mgetd (swwfa, size(swwfa), i)                                        !/*mgd      2030*/
      call mgetd (pac, size(pac), i)                                            !/*mgd      2031*/
      call mgetd (pan, size(pan), i)                                            !/*mgd      2032*/
      call mgetd (pwb, size(pwb), i)                                            !/*mgd      2033*/
      call mgetd (pcc, size(pcc), i)                                            !/*mgd      2034*/
      if(nsr.eq.71)call mgetd (sump, size(sump), i)                             !/*mgd      2035*/
      call mgetd (wns, size(wns), i)                                            !/*mgd      2036*/
!                                                                               !/*mgd      2039*/
!        unpack vdac integer variables.                                         !/*mgd      2040*/
      call mgeti (jfq, size(jfq), i)                                            !/*mgdk      123*/
      call mgeti (npsw, size(npsw), i)                                          !/*mgdk      124*/
      call mgeti (nsl, size(nsl), i)                                            !/*mgdk      125*/
      call mgeti (ntbb, size(ntbb), i)                                          !/*mgdk      126*/
      call mgeti (lfcl, size(lfcl), i)                                          !/*mgdk      127*/
      call mgeti (isef, size(isef), i)                                          !/*mgdk      128*/
      call mgeti (maze, size(maze), i)                                          !/*mgdk      129*/
      call mgeti (ndpf, size(ndpf), i)                                          !/*mgdk      130*/
      call mgeti (ndr, size(ndr), i)                                            !/*mgdk      131*/
      call mgeti (laj, size(laj), i)                                            !/*mgdk      132*/
      call mgeti (lcaj, size(lcaj), i)                                          !/*mgdk      133*/
      call mgeti (lse, size(lse), i)                                            !/*mgdk      134*/
      return                                                                    !/*mgd      2053*/
      end subroutine vdac_unpack                                                !/*mgd      2054*/
#endif                                                                          /*!mgd      2055*/
      end module global_data                                                    !/*mgd      2056*/

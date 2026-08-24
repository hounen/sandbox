!#ident MCPLOT_mp.F from deck mmd                                               !/*mmd         1*/
      module mcplot_data                                                        !/*mmd         2*/
#ifdef MCPLOT                                                                   /*!mmd         3*/
!                                                                               !/*mmd         4*/
      use global_parameters                                                     !/*mmd         5*/
      implicit none                                                             !/*mmd         6*/
      save                                                                      !/*mmd         7*/
      public                                                                    !/*mmd         8*/
      integer, private :: k                                                     !/*mmd         9*/
!                                                                               !/*mmd        10*/
!***********************************************************************        !/*mmd        11*/
!        parameter variables and arrays.                                        !/*mmd        12*/
!                                                                               !/*mmd        13*/
      integer, parameter :: isbm=100                                            !/*mmd        14*/
      integer, parameter :: jsbm=100                                            !/*mmd        15*/
      integer, parameter :: mclevs=20                                           !/*mmd        16*/
      integer, parameter :: nkeys=60                                            !/*mmdl        1*/
!                                                                               !/*mmd        18*/
!***********************************************************************        !/*mmd        19*/
!        integer variables and arrays.                                          !/*mmd        20*/
!                                                                               !/*mmd        21*/
      integer(kindi) :: lab1                                                    !/*mmd        22*/
      integer(kindi) :: lab2                                                    !/*mmd        23*/
      integer(kindi) :: lerb                                                    !/*mmd        24*/
      integer(kindi) :: lmcc                                                    !/*mmd        25*/
      integer(kindi) :: lord                                                    !/*mmd        26*/
      integer(kindi) :: lxcc                                                    !/*mmd        27*/
      integer(kindi) :: lxie                                                    !/*mmd        28*/
      integer(kindi) :: lxen                                                    !/*mmd        29*/
      integer(kindi) :: lxnx                                                    !/*mmd        30*/
      integer(kindi) :: lxrr                                                    !/*mmd        31*/
      integer(kindi) :: lxxs                                                    !/*mmd        32*/
      integer(kindi) :: lycc                                                    !/*mmd        33*/
      integer(kindi) :: lyrr                                                    !/*mmd        34*/
!                                                                               !/*mmd        35*/
      integer(kindi) :: ibl(8,2)                                                !/*mmd        36*/
      integer(kindi) :: icut(2)                                                 !/*mmd        37*/
      integer(kindi) :: ifree(2) = (/ 7,8 /)                                    !/*mmd        38*/
      integer(kindi) :: ipct = 1                                                !/*mmd        39*/
      integer(kindi) :: iprpts = 0                                              !/*mmd        40*/
      integer(kindi) :: itfc = 0                                                !/*mmd        41*/
      integer(kindi) :: itik(2) = (/ 0,0 /)                                     !/*mmd        42*/
      integer(kindi) :: ititle(7) = (/ 0,0,0,0,0,0,0 /)                         !/*mmd        43*/
      integer(kindi) :: j3d = 0                                                 !/*mmd        44*/
      integer(kindi) :: jlim(2) = (/ 0,0 /)                                     !/*mmd        45*/
      integer(kindi) :: kbin(8,2) =                                             !/*mmd        46*/
     & reshape ( (/ (0, k=1,16) /), (/ 8,2 /) )                                 !/*mmd        47*/
      integer(kindi) :: koplot = 0                                              !/*mmd        48*/
      integer(kindi) :: kurv = 0                                                !/*mmd        49*/
      integer(kindi) :: kxsplt = 0                                              !/*mmd        50*/
      integer(kindi) :: kxspma = 0                                              !/*mmd        51*/
      integer(kindi) :: kxspmt = 1                                              !/*mmd        52*/
      integer(kindi) :: kxspar = 1                                              !/*mmd        53*/
      integer(kindi) :: kxsmat = 1                                              !/*mmd        54*/
      integer(kindi) :: kxspkm                                                  !/*mmd        55*/
      integer(kindi) :: kxsptp = 1                                              !/*mmd        56*/
      integer(kindi) :: lax = 0                                                 !/*mmd        57*/
      integer(kindi) :: legend = 1                                              !/*mmd        58*/
      integer(kindi) :: lnstyl                                                  !/*mmd        59*/
      integer(kindi) :: lpert = 0                                               !/*mmd        60*/
      integer(kindi) :: lput = 0                                                !/*mmd        61*/
      integer(kindi) :: mscal = 1                                               !/*mmd        62*/
      integer(kindi) :: mtal = 1                                                !/*mmd        63*/
      integer(kindi) :: nclev                                                   !/*mmd        64*/
      integer(kindi) :: noerbr = 0                                              !/*mmd        65*/
      integer(kindi) :: nfree = 1                                               !/*mmd        66*/
      integer(kindi) :: nonorm = 0                                              !/*mmd        67*/
      integer(kindi) :: npt(2)                                                  !/*mmd        68*/
!                                                                               !/*mmd        69*/
!        commands that are legal in a coplot situation.                         !/*mmd        70*/
      integer(kindi) :: legalc(nkeys) = (/                                      !/*mmd        71*/
     & 0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,1,0,1,             !/*mmd        72*/
     & 0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,0,0 /)           !/*mmdl        2*/
!                                                                               !/*mmd        74*/
!        commands that are legal in a runtime monitor situation.                !/*mmd        75*/
      integer(kindi) :: legalm(nkeys) = (/                                      !/*mmd        76*/
     & 1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,             !/*mmd        77*/
     & 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0 /)           !/*mmdl        3*/
!                                                                               !/*mmd        79*/
!        commands that are legal in xs plots.                                   !/*mmd        80*/
!        -2 = switch mode to tally plot.                                        !/*mmd        81*/
!        -1 = legal in tally plot only.                                         !/*mmd        82*/
!         0 = legal for either tally or xs plots.                               !/*mmd        83*/
!         1 = legal in xs plot only.                                            !/*mmd        84*/
!         2 = switch mode to xs plot.                                           !/*mmd        85*/
      integer(kindi) :: legalx(nkeys) = (/                                      !/*mmd        86*/
     & 0,0,0,0,0,-1,-1,-1,-1,0,-1,0,0,0,0,-1,-2,-2,0,0,0,0,0,0,0,0,             !/*mmd        87*/
     & 0,0,-1,-1,-1,-2,-2,0,0,0,0,0,0,0,-1,0,-1,-1,-1,0,0,0,-1,-1,-1,           !/*mmd        88*/
     & 0,0,-1,2,1,1,0,0,0 /)                                                    !/*mmdl        4*/
!                                                                               !/*mmd        90*/
!        cross section plot labels.                                             !/*mmd        91*/
      integer(kindi) :: jlbl(2,10) = reshape (                                  !/*mmd        92*/
     & (/ 1,7,1,7,19,19,8,10,11,16,21,28,21,28,44,49,50,54,31,43/),             !/*mmdl        5*/
     & (/ 2,10 /) )                                                             !/*mmd        94*/
      integer(kindi) :: klbl(54) = (/                                           !/*mmdl        6*/
     & -1, -2, -3, -4, -5, -6,  0,  1,  4,  2,                                  !/*mmd        96*/
     & -1, -2, -3, -4, -5, -6,  0,  0,  0,  0,                                  !/*mmd        97*/
     & -1, -2, -3, -4, -5, -6, -7,  0,  0,  0,                                  !/*mmd        98*/
     &  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,                                  !/*mmd        99*/
     & 11, 12, 13, -1, -2, -3, -4, -5,  0,  1,  2,  3,  4,  0  /)               !/*mmdl        7*/
      integer(kindi) :: kxspu(54) = (/                                          !/*mmdl        8*/
     &  1,  1,  1,  2,  1,  1,  1,  1,  1,  1,                                  !/*mmd       102*/
     &  1,  1,  1,  1,  1,  2,  9,  9,  1,  9,                                  !/*mmd       103*/
     &  1,  1,  3,  4,  1,  5,  6,  1,  9,  9,                                  !/*mmd       104*/
     & 10, 10, 10, 11, 12, 12, 13, 12, 11, 12,                                  !/*mmd       105*/
     & 14, 15, 12,  1,  1,  1,  2,  1,  1,  1,  1,  1,  2,  1  /)               !/*mmdl        9*/
!                                                                               !/*mmd       107*/
!***********************************************************************        !/*mmd       108*/
!        real variables and arrays.                                             !/*mmd       109*/
!                                                                               !/*mmd       110*/
      real(kindr) :: abhi(2)                                                    !/*mmd       111*/
      real(kindr) :: ablo(2)                                                    !/*mmd       112*/
      real(kindr) :: clev(mclevs)                                               !/*mmd       113*/
      real(kindr) :: contur(3) = (/ 5.,95.,10. /)                               !/*mmd       114*/
      real(kindr) :: cthick = .02                                               !/*mmd       115*/
      real(kindr) :: dissf(3)                                                   !/*mmd       116*/
      real(kindr) :: pht(2) = (/ 225.,35. /)                                    !/*mmd       117*/
      real(kindr) :: plim(4)                                                    !/*mmd       118*/
      real(kindr) :: scalf(2,3) =                                               !/*mmd       119*/
     & reshape ( (/ 1.,0.,1.,0.,1.,0. /), (/ 2,3 /) )                           !/*mmd       120*/
      real(kindr) :: tensn = 0.                                                 !/*mmd       121*/
      real(kindr) :: xlg                                                        !/*mmd       122*/
      real(kindr) :: xst                                                        !/*mmd       123*/
      real(kindr) :: xyzmn(3) = (/ 0.,0.,0. /)                                  !/*mmd       124*/
      real(kindr) :: xyzmx(3) = (/ 0.,0.,0. /)                                  !/*mmd       125*/
      real(kindr) :: ylg                                                        !/*mmd       126*/
      real(kindr) :: yst                                                        !/*mmd       127*/
      real(kindr) :: yval                                                       !/*mmd       128*/
!                                                                               !/*mmd       129*/
!***********************************************************************        !/*mmd       130*/
!        character variables and arrays.                                        !/*mmd       131*/
!                                                                               !/*mmd       132*/
      character*40 :: titles(7) = (/ (' ', k=1,7) /)                            !/*mmd       133*/
      character*10 :: xspttl = ' '                                              !/*mmd       134*/
!                                                                               !/*mmd       135*/
!        the command keywords of mcplot.                                        !/*mmd       136*/
      character*8 :: keys(nkeys) = (/                                           !/*mmd       137*/
     & 'term    ','file    ','&       ','coplot  ','end     ',                  !/*mmd       138*/
     & 'return  ','plot    ','freq    ','iptal   ','options ',                  !/*mmd       139*/
     & 'printal ','printpts','status  ','runtpe  ','dump    ',                  !/*mmd       140*/
     & 'wmctal  ','rmctal  ','tally   ','nonorm  ','factor  ',                  !/*mmd       141*/
     & 'reset   ','title   ','below   ','subtitle','xtitle  ',                  !/*mmd       142*/
     & 'ytitle  ','ztitle  ','label   ','free    ','fixed   ',                  !/*mmd       143*/
     & 'set     ','tfc     ','kcode   ','linlin  ','linlog  ',                  !/*mmd       144*/
     & 'loglin  ','loglog  ','xlims   ','ylims   ','scales  ',                  !/*mmd       145*/
     & 'hist    ','plinear ','spline  ','bar     ','noerrbar',                  !/*mmd       146*/
     & 'thick   ','thin    ','legend  ','contour ','3d      ',                  !/*mmd       147*/
     & 'view    ','?       ','help    ','pert    ','xs      ',                  !/*mmd       148*/
     & 'mt      ','par     ','pause   ','        ','        ' /)                !/*mmdl       10*/
!                                                                               !/*mmd       150*/
!        cross section plot labels.                                             !/*mmd       151*/
      character*40 :: hlbl(54) = (/                                             !/*mmdl       11*/
     & 'neutron total cross section             ',                              !/*mmd       153*/
     & 'neutron absorption cross section        ',                              !/*mmd       154*/
     & 'neutron elastic cross section           ',                              !/*mmd       155*/
     & 'neutron average heating numbers         ',                              !/*mmd       156*/
     & 'neutron photon production cross section ',                              !/*mmd       157*/
     & 'neutron fission cross section           ',                              !/*mmd       158*/
     & 'neutron reaction cross section          ',                              !/*mmd       159*/
     & 's(a,b) total cross section              ',                              !/*mmd       160*/
     & 's(a,b) inelastic scattering x-section   ',                              !/*mmd       161*/
     & 's(a,b) elastic scattering x-section     ',                              !/*mmd       162*/
     & 'incoherent photon cross section         ',                              !/*mmd       163*/
     & 'coherent photon cross section           ',                              !/*mmd       164*/
     & 'photoelectric photon cross section      ',                              !/*mmd       165*/
     & 'pair production photon cross section    ',                              !/*mmd       166*/
     & 'total photon cross section              ',                              !/*mmd       167*/
     & 'average photon heating numbers          ',                              !/*mmd       168*/
     & '                                        ',                              !/*mmd       169*/
     & '                                        ',                              !/*mmd       170*/
     & 'dosimetry cross section                 ',                              !/*mmd       171*/
     & '                                        ',                              !/*mmd       172*/
     & 'multigroup total cross section          ',                              !/*mmd       173*/
     & 'multigroup fission cross section        ',                              !/*mmd       174*/
     & 'multigroup nubar data                   ',                              !/*mmd       175*/
     & 'multigroup fission chi data             ',                              !/*mmd       176*/
     & 'multigroup absorption cross section     ',                              !/*mmd       177*/
     & 'multigroup stopping powers              ',                              !/*mmd       178*/
     & 'multigroup momentum transfers           ',                              !/*mmd       179*/
     & 'multigroup edit reaction                ',                              !/*mmd       180*/
     & '                                        ',                              !/*mmd       181*/
     & '                                        ',                              !/*mmd       182*/
     & 'de/dx electron collision stopping power ',                              !/*mmd       183*/
     & 'de/dx electron radiative stopping power ',                              !/*mmd       184*/
     & 'de/dx total electron stopping power     ',                              !/*mmd       185*/
     & 'electron range                          ',                              !/*mmd       186*/
     & 'electron radiation yield                ',                              !/*mmd       187*/
     & 'relativistic beta**2                    ',                              !/*mmd       188*/
     & 'stopping power density correction       ',                              !/*mmd       189*/
     & 'ratio of rad/col stopping powers        ',                              !/*mmd       190*/
     & 'drange                                  ',                              !/*mmd       191*/
     & 'dyield                                  ',                              !/*mmd       192*/
     & 'rng array values                        ',                              !/*mmd       193*/
     & 'qav array values                        ',                              !/*mmd       194*/
     & 'ear array values                        ',                              !/*mmdl       12*/
     & 'proton total cross section              ',                              !/*mmdl       13*/
     & 'proton absorption cross section         ',                              !/*mmdl       14*/
     & 'proton elastic cross section            ',                              !/*mmdl       15*/
     & 'proton average heating numbers          ',                              !/*mmdl       16*/
     & 'proton photon production                ',                              !/*mmdl       17*/
     & 'proton reaction cross section           ',                              !/*mmdl       18*/
     & 'photonuclear total cross section        ',                              !/*mmdl       19*/
     & 'photonuclear non-elastic cross section  ',                              !/*mmdl       20*/
     & 'photonuclear elastic cross section      ',                              !/*mmdl       21*/
     & 'photonuclear heating numbers            ',                              !/*mmdl       22*/
     & 'photonuclear reaction cross section     ' /)                            !/*mmdl       23*/
!                                                                               !/*mmd       196*/
!        jlbl=(/1,7,1,7,19,19,8,10,11,16,21,28,21,28,44,49,50,54,31,43/)        !/*mmdl       24*/
      character*40 :: hxspu(15) = (/                                            !/*mmd       198*/
     & 'cross section (barns)                   ',                              !/*mmd       199*/
     & 'heating numbers (mev/col)               ',                              !/*mmd       200*/
     & 'nubar                                   ',                              !/*mmd       201*/
     & 'chi (fission fraction)                  ',                              !/*mmd       202*/
     & 'stopping power                          ',                              !/*mmd       203*/
     & 'momentum transfer                       ',                              !/*mmd       204*/
     & '                                        ',                              !/*mmd       205*/
     & '                                        ',                              !/*mmd       206*/
     & '                                        ',                              !/*mmd       207*/
     & 'stopping power (mev cm2/gm)             ',                              !/*mmd       208*/
     & 'range (gm/cm2)                          ',                              !/*mmd       209*/
     & 'dimensionless                           ',                              !/*mmd       210*/
     & 'de/dx den corr (mev cm2/gm)             ',                              !/*mmd       211*/
     & 'rng (atom/barn)                         ',                              !/*mmd       212*/
     & 'qav (mev barn/atom)                     ' /)                            !/*mmd       213*/
#endif                                                                          /*!mmd       214*/
      end module mcplot_data                                                    !/*mmd       215*/

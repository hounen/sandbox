!#ident IMCN_jc.F from deck mid                                                 !/*mid         1*/
      module imcn_data                                                          !/*mid         2*/
!                                                                               !/*mid         3*/
      use global_parameters                                                     !/*mid         4*/
      implicit none                                                             !/*mid         5*/
      save                                                                      !/*mid         6*/
      public                                                                    !/*mid         7*/
      integer, private :: i                                                     !/*mid         8*/
      integer, parameter, private :: nx = 5*mxdx+3                              !/*mid         9*/
!                                                                               !/*mid        10*/
!***********************************************************************        !/*mid        11*/
!        parameter variables and arrays.                                        !/*mid        12*/
!                                                                               !/*mid        13*/
            integer, parameter :: nkcd=105                                      !/*midc        1*/
      integer, parameter :: ntalmx=100                                          !/*mid        15*/
      integer, parameter :: mopts=3+mipt                                        !/*mid        16*/
!                                                                               !/*mid        17*/
!***********************************************************************        !/*mid        18*/
!        integer variables and arrays.                                          !/*mid        19*/
!                                                                               !/*mid        20*/
      integer(kindi) :: ljtr                                                    !/*mid        21*/
      integer(kindi) :: lawt                                                    !/*mid        22*/
      integer(kindi) :: lbbv                                                    !/*mid        23*/
      integer(kindi) :: lprb                                                    !/*mid        24*/
      integer(kindi) :: lrtp                                                    !/*mid        25*/
      integer(kindi) :: lsfb                                                    !/*mid        26*/
      integer(kindi) :: lipn                                                    !/*mid        27*/
      integer(kindi) :: lkaw                                                    !/*mid        28*/
      integer(kindi) :: lkdr                                                    !/*mid        29*/
      integer(kindi) :: llxd                                                    !/*mid        30*/
      integer(kindi) :: lmfm                                                    !/*mid        31*/
      integer(kindi) :: ldup                                                    !/*mid        32*/
      integer(kindi) :: lktr                                                    !/*mid        33*/
      integer(kindi) :: lnlv                                                    !/*mid        34*/
      integer(kindi) :: lars                                                    !/*mid        35*/
      integer(kindi) :: lats                                                    !/*mid        36*/
      integer(kindi) :: lrsc                                                    !/*mid        37*/
      integer(kindi) :: lrsn                                                    !/*mid        38*/
      integer(kindi) :: lscq                                                    !/*mid        39*/
      integer(kindi) :: lvls                                                    !/*mid        40*/
      integer(kindi) :: liin                                                    !/*mid        41*/
      integer(kindi) :: licr                                                    !/*mid        42*/
      integer(kindi) :: llav                                                    !/*mid        43*/
      integer(kindi) :: llsv                                                    !/*mid        44*/
      integer(kindi) :: llsa                                                    !/*mid        45*/
!                                                                               !/*mid        46*/
      integer(kindi) :: ica                                                     !/*mid        47*/
      integer(kindi) :: icn                                                     !/*mid        48*/
      integer(kindi) :: icx                                                     !/*mid        49*/
      integer(kindi) :: ifip(mipt) = (/ (0, i=1,mipt) /)                        !/*midl        1*/
      integer(kindi) :: iitm                                                    !/*mid        51*/
      integer(kindi) :: ipl = 0                                                 !/*mid        52*/
      integer(kindi) :: irc                                                     !/*mid        53*/
      integer(kindi) :: irs                                                     !/*mid        54*/
      integer(kindi) :: jsd(4,33) =                                             !/*mid        55*/
     & reshape ( (/ (0, i=1,132) /), (/ 4,33 /) )                               !/*mid        56*/
      integer(kindi) :: jui                                                     !/*mid        57*/
      integer(kindi) :: kitm                                                    !/*mid        58*/
!        krq() initialized below with cnm().                                    !/*mid        59*/
      integer(kindi) :: krq(7,nkcd)                                             !/*mid        60*/
      integer(kindi) :: ktl(ntalmx,2) =                                         !/*mid        61*/
     & reshape ( (/ (0, i=1,ntalmx),(1, i=1,ntalmx) /), (/ ntalmx,2 /) )        !/*mid        62*/
      integer(kindi) :: ktt(ntalmx,mipt) =                                      !/*mid        63*/
     & reshape ( (/ (0, i=1,ntalmx*mipt) /), (/ ntalmx,mipt /) )                !/*mid        64*/
#ifdef RADIOG                                                                   /*!mid        65*/
      integer(kindi) :: lftnp                                                   !/*mid        66*/
#endif                                                                          /*!mid        67*/
      integer(kindi) :: licc                                                    !/*mid        68*/
      integer(kindi) :: likef = 0                                               !/*mid        69*/
      integer(kindi) :: lit = 0                                                 !/*mid        70*/
      integer(kindi) :: lrt = 0                                                 !/*mid        71*/
      integer(kindi) :: ltd = 0                                                 !/*mid        72*/
      integer(kindi) :: m1c                                                     !/*mid        73*/
      integer(kindi) :: m2c                                                     !/*mid        74*/
      integer(kindi) :: m3c                                                     !/*mid        75*/
      integer(kindi) :: m4c                                                     !/*mid        76*/
      integer(kindi) :: m5c                                                     !/*mid        77*/
      integer(kindi) :: m6c                                                     !/*mid        78*/
      integer(kindi) :: m7c                                                     !/*mid        79*/
      integer(kindi) :: m8c                                                     !/*mid        80*/
      integer(kindi) :: m9c                                                     !/*mid        81*/
      integer(kindi) :: m10c                                                    !/*mid        82*/
      integer(kindi) :: mipts                                                   !/*mid        83*/
      integer(kindi) :: mkcp = 0                                                !/*mid        84*/
      integer(kindi) :: mlaf = 0                                                !/*mid        85*/
      integer(kindi) :: mssc = 0                                                !/*mid        86*/
      integer(kindi) :: mxit = 0                                                !/*mid        87*/
      integer(kindi) :: naw = 0                                                 !/*mid        88*/
      integer(kindi) :: ncomp = 0                                               !/*mid        89*/
      integer(kindi) :: ncpar(mipt,nkcd) =                                      !/*mid        90*/
     & reshape ( (/ (0, i=1,mipt*nkcd) /), (/ mipt,nkcd /) )                    !/*mid        91*/
      integer(kindi) :: ncparf = 0                                              !/*mid        92*/
      integer(kindi) :: ncrn                                                    !/*mid        93*/
      integer(kindi) :: ndp(ntalmx) = (/ (0, i=1,ntalmx) /)                     !/*mid        94*/
      integer(kindi) :: ndup(3) = (/ 0,0,0 /)                                   !/*mid        95*/
      integer(kindi) :: nii                                                     !/*mid        96*/
      integer(kindi) :: nitm                                                    !/*mid        97*/
      integer(kindi) :: niwr = 0                                                !/*mid        98*/
      integer(kindi) :: njsw = 0                                                !/*mid        99*/
      integer(kindi) :: nmfm = 0                                                !/*mid       100*/
      integer(kindi) :: novol = 0                                               !/*mid       101*/
      integer(kindi) :: nqp(mipt)                                               !/*midl        2*/
      integer(kindi) :: nqw                                                     !/*mid       103*/
      integer(kindi) :: nsc = 0                                                 !/*mid       104*/
      integer(kindi) :: nsjv                                                    !/*mid       105*/
      integer(kindi) :: nsv                                                     !/*mid       106*/
      integer(kindi) :: ntl(0:ntalmx) = (/ (0, i=0,ntalmx) /)                   !/*mid       107*/
      integer(kindi) :: nwc                                                     !/*mid       108*/
      integer(kindi) :: nxsc = 0                                                !/*mid       109*/
!                                                                               !/*mid       110*/
!***********************************************************************        !/*mid       111*/
!        real variables and arrays.                                             !/*mid       112*/
!                                                                               !/*mid       113*/
      real(kindr) :: ajsh                                                       !/*mid       114*/
      real(kindr) :: bbb(4,4)                                                   !/*mid       115*/
      real(kindr) :: ritm                                                       !/*mid       116*/
      real(kindr) :: swtx = 0.d0                                                !/*mid       117*/
!                                                                               !/*mid       118*/
!        equiprobable-bin fission energy spectrum for skcode.                   !/*mid       119*/
      real(kindr) :: fes(33) = (/                                               !/*mid       120*/
     & 0.     ,  .162524, .266043, .358425, .445672, .530293, .613680,          !/*mid       121*/
     &  .696783, .780264, .864702, .950540,1.038286,1.128326,1.221111,          !/*mid       122*/
     & 1.317206,1.417070,1.521302,1.630646,1.745929,1.868073,1.998282,          !/*mid       123*/
     & 2.138046,2.289259,2.454356,2.636707,2.840830,3.073518,3.344965,          !/*mid       124*/
     & 3.672134,4.086420,4.656234,5.588725,9.0 /)                               !/*mid       125*/
!                                                                               !/*mid       126*/
!***********************************************************************        !/*mid       127*/
!        character variables and arrays.                                        !/*mid       128*/
!                                                                               !/*mid       129*/
      character*80 :: hitm                                                      !/*mid       130*/
      character*80 :: hlin                                                      !/*mid       131*/
      character*5 :: ich                                                        !/*mid       132*/
!                                                                               !/*mid       133*/
!        cnm(k):  name on input data card k.                                    !/*mid       134*/
!        krq(i,k):  values of attributes of input data card k.                  !/*mid       135*/
!        i=1   1 if the card is legal in a continue run.                        !/*mid       136*/
!        i=2   1 if the card requires particle designator.                      !/*mid       137*/
!        i=3   special card type numbers for tally cards.                       !/*mid       138*/
!        i=4   1 if j can be used as an input item.                             !/*mid       139*/
!        i=5   requires  -1=numbers  0=any  1=positive  2=integers              !/*mid       140*/
!        i=6   maximum number of entries allowed.  0=no limit.                  !/*mid       141*/
!              negative means variable, to be set in rdprob.                    !/*mid       142*/
!        i=7   2=surface parameters  1=cell parameters  0=something else        !/*mid       143*/
!                                                                               !/*mid       144*/
      character*5 :: cnm(nkcd)                                                  !/*mid       145*/
      data cnm( 1),(krq(i, 1),i=1,7)/'     ',0,0, 0,0, 0,   0,0/                !/*mid       146*/
      data cnm( 2),(krq(i, 2),i=1,7)/'     ',0,0, 0,1, 0,   0,0/                !/*mid       147*/
      data cnm( 3),(krq(i, 3),i=1,7)/'tr   ',0,0, 0,1,-1,  13,0/                !/*mid       148*/
      data cnm( 4),(krq(i, 4),i=1,7)/'vect ',0,0, 0,0, 0,   0,0/                !/*mid       149*/
      data cnm( 5),(krq(i, 5),i=1,7)/'mode ',0,0, 0,0, 0,   0,0/                !/*mid       150*/
      data cnm( 6),(krq(i, 6),i=1,7)/'imp  ',0,1, 0,1, 1,  -1,1/                !/*mid       151*/
      data cnm( 7),(krq(i, 7),i=1,7)/'vol  ',0,0, 0,1, 0,  -1,1/                !/*mid       152*/
      data cnm( 8),(krq(i, 8),i=1,7)/'area ',0,0, 0,1, 1,  -2,2/                !/*mid       153*/
      data cnm( 9),(krq(i, 9),i=1,7)/'pwt  ',0,0, 0,1,-1,  -1,1/                !/*mid       154*/
      data cnm(10),(krq(i,10),i=1,7)/'ext  ',0,1, 0,1, 0,  -1,1/                !/*mid       155*/
      data cnm(11),(krq(i,11),i=1,7)/'fcl  ',0,1, 0,1,-1,  -1,1/                !/*mid       156*/
      data cnm(12),(krq(i,12),i=1,7)/'wwn  ',0,1, 0,0,-1,  -1,1/                !/*mid       157*/
      data cnm(13),(krq(i,13),i=1,7)/'wwe  ',0,1, 0,0, 1,  99,0/                !/*mid       158*/
      data cnm(14),(krq(i,14),i=1,7)/'wwp  ',0,1, 0,1,-1,   7,0/                !/*mid       159*/
      data cnm(15),(krq(i,15),i=1,7)/'pd   ',0,0,15,1, 1,  -1,1/                !/*mid       160*/
      data cnm(16),(krq(i,16),i=1,7)/'dxc  ',0,1, 0,1, 1,  -1,1/                !/*mid       161*/
      data cnm(17),(krq(i,17),i=1,7)/'nonu ',0,0, 0,0, 2,  -1,1/                !/*mid       162*/
      data cnm(18),(krq(i,18),i=1,7)/'si   ',0,0, 0,1, 0,   0,0/                !/*mid       163*/
      data cnm(19),(krq(i,19),i=1,7)/'sp   ',0,0, 0,1, 0,   0,0/                !/*mid       164*/
      data cnm(20),(krq(i,20),i=1,7)/'sb   ',0,0, 0,1, 0,   0,0/                !/*mid       165*/
      data cnm(21),(krq(i,21),i=1,7)/'ds   ',0,0, 0,1, 0,   0,0/                !/*mid       166*/
      data cnm(22),(krq(i,22),i=1,7)/'sc   ',0,0, 0,0, 0,   0,0/                !/*mid       167*/
      data cnm(23),(krq(i,23),i=1,7)/'sdef ',0,0, 0,1, 0,   0,0/                !/*mid       168*/
      data cnm(24),(krq(i,24),i=1,7)/'f    ',0,1, 2,0, 0,   0,0/                !/*mid       169*/
      data cnm(25),(krq(i,25),i=1,7)/'fx   ',0,1, 2,0, 0,   0,0/                !/*mid       170*/
      data cnm(26),(krq(i,26),i=1,7)/'fy   ',0,1, 2,0, 0,   0,0/                !/*mid       171*/
      data cnm(27),(krq(i,27),i=1,7)/'fz   ',0,1, 2,0, 0,   0,0/                !/*mid       172*/
      data cnm(28),(krq(i,28),i=1,7)/'ft   ',0,0,22,1, 0,   0,0/                !/*mid       173*/
      data cnm(29),(krq(i,29),i=1,7)/'fc   ',0,0, 7,0, 0,   0,0/                !/*mid       174*/
      data cnm(30),(krq(i,30),i=1,7)/'e    ',0,0, 4,1, 0,   0,0/                !/*mid       175*/
      data cnm(31),(krq(i,31),i=1,7)/'t    ',0,0, 5,1, 0,   0,0/                !/*mid       176*/
      data cnm(32),(krq(i,32),i=1,7)/'c    ',0,0, 3,1, 0,   0,0/                !/*mid       177*/
      data cnm(33),(krq(i,33),i=1,7)/'fq   ',1,0,17,1, 0,   8,0/                !/*mid       178*/
      data cnm(34),(krq(i,34),i=1,7)/'fm   ',0,0,10,0, 0,   0,0/                !/*mid       179*/
      data cnm(35),(krq(i,35),i=1,7)/'de   ',0,0,18,0, 0,   0,0/                !/*mid       180*/
      data cnm(36),(krq(i,36),i=1,7)/'df   ',0,0,19,0, 0,   0,0/                !/*mid       181*/
      data cnm(37),(krq(i,37),i=1,7)/'em   ',0,0,13,1,-1,   0,0/                !/*mid       182*/
      data cnm(38),(krq(i,38),i=1,7)/'tm   ',0,0,14,1,-1,   0,0/                !/*mid       183*/
      data cnm(39),(krq(i,39),i=1,7)/'cm   ',0,0,12,1,-1,   0,0/                !/*mid       184*/
      data cnm(40),(krq(i,40),i=1,7)/'cf   ',0,0, 8,1, 2,  -1,0/                !/*mid       185*/
      data cnm(41),(krq(i,41),i=1,7)/'sf   ',0,0, 9,1,-1,  -2,0/                !/*mid       186*/
#ifdef RADIOG                                                                   /*!mid       187*/
      data cnm(42),(krq(i,42),i=1,7)/'fs   ',0,0, 6,0, 0,   0,0/                !/*mid       188*/
#else                                                                           /*!mid       189*/
      data cnm(42),(krq(i,42),i=1,7)/'fs   ',0,0, 6,0, 0,  -2,0/                !/*mid       190*/
#endif                                                                          /*!mid       191*/
      data cnm(43),(krq(i,43),i=1,7)/'sd   ',0,0,11,1, 0,   0,0/                !/*mid       192*/
      data cnm(44),(krq(i,44),i=1,7)/'fu   ',0,0,16,1, 0,   0,0/                !/*mid       193*/
      data cnm(45),(krq(i,45),i=1,7)/'tf   ',0,0,20,1, 2,   8,0/                !/*mid       194*/
      data cnm(46),(krq(i,46),i=1,7)/'dd   ',1,0,21,0,-1,   0,0/                !/*mid       195*/
      data cnm(47),(krq(i,47),i=1,7)/'dxt  ',0,1, 0,0,-1,  nx,0/                !/*mid       196*/
      data cnm(48),(krq(i,48),i=1,7)/'m    ',0,0, 0,0, 0,   0,0/                !/*mid       197*/
      data cnm(49),(krq(i,49),i=1,7)/'drxs ',0,0, 0,0, 0,   0,0/                !/*mid       198*/
      data cnm(50),(krq(i,50),i=1,7)/'totnu',0,0, 0,0, 0,   1,0/                !/*mid       199*/
      data cnm(51),(krq(i,51),i=1,7)/'awtab',0,0, 0,0, 1,   0,0/                !/*mid       200*/
      data cnm(52),(krq(i,52),i=1,7)/'xs   ',0,0, 0,0, 0,  11,0/                !/*mid       201*/
      data cnm(53),(krq(i,53),i=1,7)/'void ',0,0, 0,0, 0,   0,0/                !/*mid       202*/
      data cnm(54),(krq(i,54),i=1,7)/'phys ',0,1, 0,1,-1,  10,0/                !/*mid       203*/
      data cnm(55),(krq(i,55),i=1,7)/'esplt',0,1, 0,0,-1,  10,0/                !/*mid       204*/
      data cnm(56),(krq(i,56),i=1,7)/'tmp  ',0,0, 0,1, 1,  -1,1/                !/*mid       205*/
      data cnm(57),(krq(i,57),i=1,7)/'thtme',0,0, 0,0,-1,  99,0/                !/*mid       206*/
      data cnm(58),(krq(i,58),i=1,7)/'mt   ',0,0, 0,0, 0,   0,0/                !/*mid       207*/
      data cnm(59),(krq(i,59),i=1,7)/'cut  ',0,1, 0,1,-1,   5,0/                !/*mid       208*/
#ifdef RADIOG                                                                   /*!mid       209*/
      data cnm(60),(krq(i,60),i=1,7)/'nps  ',1,0, 0,0, 2,   2,0/                !/*mid       210*/
#else                                                                           /*!mid       211*/
      data cnm(60),(krq(i,60),i=1,7)/'nps  ',1,0, 0,0, 2,   1,0/                !/*mid       212*/
#endif                                                                          /*!mid       213*/
      data cnm(61),(krq(i,61),i=1,7)/'ctme ',1,0, 0,0, 1,   1,0/                !/*mid       214*/
      data cnm(62),(krq(i,62),i=1,7)/'idum ',1,0, 0,1, 2,mdum,0/                !/*mid       215*/
      data cnm(63),(krq(i,63),i=1,7)/'rdum ',1,0, 0,1,-1,mdum,0/                !/*mid       216*/
      data cnm(64),(krq(i,64),i=1,7)/'prdmp',1,0, 0,1,-1,   5,0/                !/*mid       217*/
      data cnm(65),(krq(i,65),i=1,7)/'lost ',1,0, 0,1, 2,   2,0/                !/*mid       218*/
      data cnm(66),(krq(i,66),i=1,7)/'dbcn ',1,0, 0,1,-1,  30,0/                !/*mid       219*/
      data cnm(67),(krq(i,67),i=1,7)/'files',0,0, 0,1, 0,  30,0/                !/*mid       220*/
      data cnm(68),(krq(i,68),i=1,7)/'print',1,0, 0,0, 2,   0,0/                !/*mid       221*/
      data cnm(69),(krq(i,69),i=1,7)/'kcode',1,0, 0,1, 0,   9,0/                !/*mid       222*/
      data cnm(70),(krq(i,70),i=1,7)/'ksrc ',0,0, 0,0,-1,   0,0/                !/*mid       223*/
      data cnm(71),(krq(i,71),i=1,7)/'wwg  ',0,0, 0,1,-1,   9,0/                !/*mid       224*/
      data cnm(72),(krq(i,72),i=1,7)/'wwge ',0,1, 0,0, 1,  15,0/                !/*mid       225*/
      data cnm(73),(krq(i,73),i=1,7)/'ssw  ',0,0, 0,0, 0,   0,0/                !/*mid       226*/
      data cnm(74),(krq(i,74),i=1,7)/'ssr  ',0,0, 0,0, 0,   0,0/                !/*mid       227*/
      data cnm(75),(krq(i,75),i=1,7)/'u    ',0,0, 0,1, 2,  -1,1/                !/*mid       228*/
      data cnm(76),(krq(i,76),i=1,7)/'trcl ',0,0, 0,1, 0,   0,1/                !/*mid       229*/
      data cnm(77),(krq(i,77),i=1,7)/'lat  ',0,0, 0,1, 2,  -1,1/                !/*mid       230*/
      data cnm(78),(krq(i,78),i=1,7)/'fill ',0,0, 0,1, 0,   0,1/                !/*mid       231*/
      data cnm(79),(krq(i,79),i=1,7)/'mgopt',0,0, 0,0, 0,   7,0/                !/*mid       232*/
      data cnm(80),(krq(i,80),i=1,7)/'pikmt',0,0, 0,0,-1,   0,0/                !/*mid       233*/
      data cnm(81),(krq(i,81),i=1,7)/'elpt ',0,1, 0,1, 1,  -1,1/                !/*mid       234*/
      data cnm(82),(krq(i,82),i=1,7)/'bbrem',0,0, 0,0, 1,   0,0/                !/*mid       235*/
      data cnm(83),(krq(i,83),i=1,7)/'mplot',1,0, 0,0, 0,   0,0/                !/*mid       236*/
      data cnm(84),(krq(i,84),i=1,7)/'ptrac',0,0, 0,0, 0,   0,0/                !/*mid       237*/
      data cnm(85),(krq(i,85),i=1,7)/'pert ',0,1, 0,0, 0,   0,0/                !/*mid       238*/
      data cnm(86),(krq(i,86),i=1,7)/'mesh ',1,0, 0,1, 0,   0,0/                !/*mid       239*/
      data cnm(87),(krq(i,87),i=1,7)/'zd   ',1,0, 0,1, 0,   0,0/                !/*mid       243*/
      data cnm(88),(krq(i,88),i=1,7)/'mpn  ',0,0, 0,0, 2,   0,0/                !/*mid       245*/
      data cnm(89),(krq(i,89),i=1,7)/'lca  ',0,0, 0,1, 2,   9,0/                !/*mid       246*/
      data cnm(90),(krq(i,90),i=1,7)/'lcb  ',0,0, 0,1,-1,   8,0/                !/*mid       247*/
      data cnm(91),(krq(i,91),i=1,7)/'lea  ',0,0, 0,1, 2,   8,0/                !/*mid       248*/
      data cnm(92),(krq(i,92),i=1,7)/'leb  ',0,0, 0,1,-1,   4,0/                !/*mid       249*/
      data cnm(93),(krq(i,93),i=1,7)/'histp',0,0, 0,0, 2,   0,0/                !/*mid       250*/
      data cnm(94),(krq(i,94),i=1,7)/'tmesh',0,0, 0,0, 0,   0,0/                !/*mid       251*/
      data cnm(95),(krq(i,95),i=1,7)/'notrn',0,0, 0,0, 0,   0,0/                !/*mid       252*/
      data cnm(96),(krq(i,96),i=1,7)/'talnp',1,0, 0,0, 2, 100,0/                !/*mid       253*/
      data cnm(97),(krq(i,97),i=1,7)/'pi   ',0,1, 2,0, 0,   0,0/                !/*mid       254*/
      data cnm(98),(krq(i,98),i=1,7)/'tir  ',0,1, 2,0, 0,   0,0/                !/*mid       255*/
      data cnm(99),(krq(i,99),i=1,7)/'tic  ',0,1, 2,0, 0,   0,0/                !/*mid       256*/
      data cnm(100),(krq(i,100),i=1,7)/'spabi',0,1, 0,0, 0,   0,0/              !/*mid       257*/
      data cnm(101),(krq(i,101),i=1,7)/'xtra ',1,0, 0,1, 0,   0,0/              !/*midc        2*/
      data cnm(102),(krq(i,102),i=1,7)/'xtra ',1,0, 0,1, 0,   0,0/              !/*midc        6*/
      data cnm(103),(krq(i,103),i=1,7)/'za   ',1,0, 0,1, 0,   0,0/              !/*midc        8*/
      data cnm(104),(krq(i,104),i=1,7)/'zb   ',1,0, 0,1, 0,   0,0/              !/*midc        9*/
      data cnm(105),(krq(i,105),i=1,7)/'zc   ',1,0, 0,1, 0,   0,0/              !/*midc       10*/
!                                                                               !/*mid       262*/
!        available options for the m card.                                      !/*mid       263*/
      character*5 :: hmopt(mopts) = (/                                          !/*mid       264*/
     & 'gas  ','estep','cond ','nlib ','plib ','elib ','pnlib',                 !/*mid       265*/
     & '     ','     ','     ','     ','hlib ','     ','     ',                 !/*mid       266*/
     & '     ','     ','     ','     ','     ','     ','     ',                 !/*mid       267*/
     & '     ','     ','     ','     ','     ','     ','     ',                 !/*mid       268*/
     & '     ','     ','     ','     ','     ','     ','     ',                 !/*mid       269*/
     & '     ','     ' /)                                                       !/*mid       270*/
!                                                                               !/*mid       271*/
!        ptrac card keywords and mnemonics for the filter keyword.              !/*mid       272*/
      character*7 :: hptr(nptr) = (/                                            !/*mid       273*/
     & 'buffer ','cell   ','event  ','file   ','filter ','max    ',             !/*mid       274*/
     & 'meph   ','nps    ','surface','tally  ','type   ','value  ',             !/*mid       275*/
     & 'write  ' /)                                                             !/*mid       276*/
      character*7 :: hpbl(24) = (/                                              !/*mid       277*/
     & 'x      ','y      ','z      ','u      ','v      ','w      ',             !/*mid       278*/
     & 'erg    ','wgt    ','tme    ','vel    ','imp1   ','imp2   ',             !/*mid       279*/
     & 'imp3   ','spare1 ','spare2 ','spare3 ','icl    ','jsu    ',             !/*mid       280*/
     & 'idx    ','ncp    ','lev    ','iii    ','jjj    ','kkk    ' /)           !/*mid       281*/
!                                                                               !/*mid       282*/
!        pert card keywords.                                                    !/*mid       283*/
      character*7 :: hptb(npkey) = (/                                           !/*mid       284*/
     & 'cell   ','mat    ','rho    ','rxn    ','erg    ','method ' /)           !/*mid       285*/
!                                                                               !/*mid       286*/
!        mesh card keywords.                                                    !/*mid       287*/
      character*7 :: hmsh(nmkey) = (/                                           !/*mid       288*/
     & 'ref    ','origin ','axs    ','vec    ','geom   ','imesh  ',             !/*mid       289*/
     & 'iints  ','jmesh  ','jints  ','kmesh  ','kints  ' /)                     !/*mid       290*/
!                                                                               !/*mid       291*/
      end module imcn_data                                                      !/*mid       292*/

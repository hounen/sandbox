!     $Id: ercom.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                        !/*er          1*/
      logical fisinh                                                            !/*er          2*/
      logical penbar                                                            !/*er          3*/
      common /ercom/ waps(250,20), yzero, bzero, yzere, bzere, yz0, bz0,        !/*er          4*/
     & tz0(4,7), eff, rho9(6), hevsum, alph(300), bet(300), pp0(1001),          !/*er          5*/
     & pp1(1001), pp2(1001), cam2(130), cam3(200), cam4(130), cam5(200),        !/*er          6*/
     & afis(10), zfis(10), fisu(10), fiske(10), amdiff, coslf0(3),              !/*er          7*/
     & rnmass, rfmass, coslf1(3), coslf2(3), ernff, amcf, amc1, amc2,           !/*er          8*/
     & fkey, sos(6), strun(6), zmass(6), q9(6), fla9(6), flkcou(6),             !/*er          9*/
     & ccoul(6), thresh(6), smalla(6), flz9(6), r9(6), s9(6), eye1(6),          !/*er         10*/
     & smom1(6), eps, eye0, gammaa, ar, zr, ex1, ex2, xlhs, xrhs, argp,         !/*er         11*/
     & sigma, nofis, ievap, ifbrk, nobale, jemiss,                              !/*er         12*/
#ifdef CEM                                                                      /*!er         13*/
     & icem,                                                                    !/*er         14*/
#endif                                                                          /*!er         15*/
     & fisinh, penbar                                                           !/*er         16*/

!     $Id: cmgcom.h,v 1.5 2002/03/27 18:14:13 gwm Exp $                       !/*cm          1*/
#ifdef CHEAP                                                                    /*!cm          2*/
      double precision lamda0, lamda1, lamda2, lamda3, mu0, mu1, mu2,           !/*cm          3*/
     & mu3, nu0, nu1, nu2, lambda, mu, nu, muprim, nuprim                       !/*cm          4*/
#else                                                                           /*!cm          5*/
      real lamda0, lamda1, lamda2, lamda3, mu0, mu1, mu2, mu3, nu0, nu1,        !/*cm          6*/
     & nu2, lambda, mu, nu, muprim, nuprim                                      !/*cm          7*/
#endif                                                                          /*!cm          8*/
!     Comment formerly required to avoid a DEC ALPHA preprocessor bug.          !/*cm          9*/
      common /cmgcom/ p0(6), p1(6), p2(6), lamda0(6), lamda1(6), lamda2(        !/*cm         10*/
     & 6), lamda3(6), mu0(6), mu1(6), mu2(6), mu3(6), nu0(6), nu1(6),           !/*cm         11*/
     & nu2(6), itt(6), ematch, lambda, mu, nu, muprim, nuprim, ecoul,           !/*cm         12*/
     & ezero, acrit, rad0, rcompa, rcompb, deltab, rfix1, rfix2, delta(6        !/*cm         13*/
     & ), p, q, r, emx                                                          !/*cm         14*/

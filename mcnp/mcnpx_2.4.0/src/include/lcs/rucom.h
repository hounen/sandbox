!     $Id: rucom.h,v 1.5 2002/03/27 18:14:18 gwm Exp $                        !/*ru          1*/
      parameter (l12=300)                                                       !/*ru          2*/
      parameter (l13=300)                                                       !/*ru          3*/
      parameter (maxph=200)                                                     !/*ru          4*/
      common /rucom/ apr, cosphi, costh, delsig, totm, totels, esminp,          !/*ru          5*/
     & siga, ex, geomxs, sinphi, sinth, zpr, epart(l12,2),                      !/*ru          6*/
     & hepart(l13,4), cosevp(3,l12,2), coshev(3,l13,4), exlow(5), ehin,         !/*ru          7*/
     & ehipi, aprr(3), zprr(3), erecr(3), uur(3), ur(3), vr(3), wr(3),          !/*ru          8*/
     & ecvr(3), flenb(6), fiswt, pmass(lpt), charge(lpt), geosig(260),          !/*ru          9*/
     & ecv, eph(maxph), elv(maxph), tph(maxph), exc0, ityp, lelem,              !/*ru         10*/
     & nocas, nopart, npart(6), nexite, negex, lowaz, negaz, nrnorm(4),         !/*ru         11*/
     & ihie, mnuc, nobalc, ifisct, ipht, irejpi, iexisa, instab(lpt),           !/*ru         12*/
     & nph, icc, ielas, iexp, isprd, ielast, ityp1                              !/*ru         13*/

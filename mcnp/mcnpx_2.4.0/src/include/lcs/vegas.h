!     $Id: vegas.h,v 1.4 2002/03/27 18:14:19 gwm Exp $                        !/*ve          1*/
!            60 = 5 * (11+1) interactions / particle                            !/*ve          2*/
!            81 = 60 + 21 = information length / particle                       !/*ve          3*/
!            391= no. of particles that "st" can hold                           !/*ve          4*/
!            32k = 81 * 391  particles in storage area                          !/*ve          5*/
!            20 = total pion multiplicity (for 10 gev)                          !/*ve          6*/
#ifdef BIG                                                                      /*!ve          7*/
      parameter (i60=160, i391=1500)                                            !/*ve          8*/
#else                                                                           /*!ve          9*/
      parameter (i60=80, i391=750)                                              !/*ve         10*/
#endif                                                                          /*!ve         11*/
      parameter (i81=21+i60, i32k=i81*i391, i20=20)                             !/*ve         12*/
      common /vegas/ st(i32k), amass(9), csa(15), ws(32), wsp(32), stp(6        !/*ve         13*/
     & 00), den_(16), pdn(16), expr, ef(2,17), pfr(2,17), pfw(2,17),            !/*ve         14*/
     & poe1, cutofa(11), pit(6), ex, gws(15), cm(25), ela(20), cm1(21),         !/*ve         15*/
     & cm2(14), wws(32), wwsp(32), aip(5), ain(5), ws34, bdd, aiscat,           !/*ve         16*/
     & aidec, aicap, aitot, fscat, pir(6), wspr(32), ws33, icsb(17), ind        !/*ve         17*/
     & (40), ieght                                                              !/*ve         18*/
      parameter (liso=500, liso5=liso-5)                                        !/*ve         19*/
      common /new/ xysq(250), proj(14), xpro(250), ypro(250), zpro(250),        !/*ve         20*/
     & epro(250), voc, ppro(250), pxpro(250), pypro(250), pzpro(250),           !/*ve         21*/
     & pef(2,17), eprh(4), ya(17), ppfr(2,17), ppfw(2,17), eh(250), ph(3        !/*ve         22*/
     & ,250), exth, gama, yp, vap, gamiso, vpion, timef, etep(2), se(5),        !/*ve         23*/
     & ses(5), sr(5), srs(5), fws3, fwsp3, xmasss(3,3), rcas, aa(360),          !/*ve         24*/
     & bb(3600), transp, tcm, p10, dx(8), dxp(16), xa(17), bige, bigp(3)        !/*ve         25*/
     & , xmp0, xmt0, xmp, r0, at, r0p, ap, tnpar(4,250), pnpar(4,250),          !/*ve         26*/
     & gapl, sel(2), spr(4)                                                     !/*ve         27*/
!     tcm is c.m.k.e. (in m(elec)), p10 is lab momentum, (in mev/c)             !/*ve         28*/
      logical kabran                                                            !/*ve         29*/
      logical repeat                                                            !/*ve         30*/
      common /new/ nuc, npmon(3), jpro(250), ipro(250), npout, kaskew,          !/*ve         31*/
     & kabran, nopot, kcan, monin(7), mprin, nrpcnt, repeat, ipauli,            !/*ve         32*/
     & isonsw, model, npl(5), intpl(5), nypl(5), nyplc(5), ixa(2), ixq(2        !/*ve         33*/
     & ), ntdel, nti, na(11), kcas, i9, i10, i11, iden, ibrad, jrep(10),        !/*ve         34*/
     & nrep, iexc, nnt, nnp, knp, np, istept, istepp, istpp, istpt,             !/*ve         35*/
     & nprts, hcsa(15,2,liso), hden(16,2,liso), hx(17,2,liso), hef(34,2         !/*ve         36*/
     & ,liso), hpfr(34,2,liso), hpfw(34,2,liso)                                 !/*ve         37*/
      character lkcan*8                                                         !/*ve         38*/
      common /chnew2/ lkcan                                                     !/*ve         39*/

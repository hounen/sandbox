!     $Id: concom.h,v 1.5 2002/06/14 17:05:41 jxh Exp $                         !/*co          1*/
      parameter (lring=100, lesrc=200, ltsrc=200, lcul=201)                     !/*co          2*/
      logical bfhist                                                            !/*co          3*/
      common /concom/ cpa, wtneut(4), wtnt1(4), wtnt2(4), sprlm1, sprlm2        !/*co          4*/
     & , wtpi, wtmu, fiswt1, fiswt2, swtm, wtsum(lpt,8), d_, dkcons(lpt)        !/*co          5*/
     & , edel(2), rms(lpt), rmsinv(lpt), rbar, sigr, es, xs, ys, zs,            !/*co          6*/
     & elow(2), erng(lcul,2), emax, oldwt, peiv(lpt), tip0, x_0, y_0,           !/*co          7*/
     & z_0, a_0, b_0, cuts, smu, e_0, rsmu, xcut, twit, tpeak, tsig, w_0        !/*co          8*/
     & , sring1(lring), sring2(lring), sprob(lring), esrc(lesrc), eprb          !/*co          9*/
     & (lesrc), tsrc(ltsrc), tprb(ltsrc), emin(lpt), vmass(lpt), sigdk,         !/*co         10*/
     & sigdk0, edkmin                                                           !/*co         11*/
      common /concom/ lhist, lnrec, nlrnge, neutnt(4), nhstin, ir0(2),          !/*co         12*/
     & icpt, kpart(lpt), krec(8), isrfsr, noskip, jcasc, nwrds, neutct,         !/*co         13*/
     & nlimit, lneut, bfhist, mxhist, iqhist, incol, ibertp, nhstp,             !/*co         14*/
     & nosum(lpt,8), ncolc(13), jip0, ichoic, lneutp, npidk, nobch,             !/*co         15*/
     & neutno(4), nseudo, nsav, nspred, nstrag, itrans(lpt), jbnk, nbnk,        !/*co         16*/
     & mbb, lsb, nbmx, nbmxh, mxmel, mxelm, lbb, nbov, nbhw, nbt(lpt),          !/*co         17*/
     & maxbch, maxcas, n1col, namax, nbertp, ncmosc, ncol, isopt, neprb,        !/*co         18*/
     & itopt, ietab, nring, ntprb, jbr_0, itip0, mxmat, nocsc, noact,           !/*co         19*/
     & modexs, lmxs(lpt), moddd, ntapes                                         !/*colk        1*/
      dimension ncolx(8)                                                        !/*co         21*/
      equivalence (ncolx(1),ncolc(6))                                           !/*co         22*/
      character*8 napart                                                        !/*co         23*/
      parameter (lpt2=lpt+2)                                                    !/*co         24*/
      common /aasch/ napart(lpt2)                                               !/*co         25*/

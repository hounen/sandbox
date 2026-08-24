!     $Id: label.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                        !/*la          1*/
      parameter (l02=500)                                                       !/*la          2*/
#include <htape3x/lpt.h>                                                        /*!la          3*/
      parameter (lmel=2000)                                                     !/*la          4*/
      parameter (lmt=50, lmt1=lmt+1)                                            !/*la          5*/
      real x, y, z, xc, yc, zc, oldwt, wt, e, ec, u, v, w, tc, tip, ex,         !/*la          6*/
     & tipa, ea, ua, va, wa, wta, tipb, eb, ub, vb, wb, wtb, aprr, zprr,        !/*la          7*/
     & erecr, uur, ecvr, ur, vr, wr, fmu                                        !/*la          8*/
      real geomxs                                                               !/*la          9*/
      real tca, tcb                                                             !/*la         10*/
      integer bold, blz                                                         !/*la         11*/
      common /label/ emin(lpt), emax, nhst, ncol, nocas, name, mat, nmed        !/*la         12*/
     & , bold, blz, tip, x, y, z, xc, yc, zc, oldwt, wt, e, ec, u, v, w,        !/*la         13*/
     & tc, lelem, nopart, nabov, nbelo, mnuc, ex, geomxs,                       !/*la         14*/
     &  maxbch, maxcas, mxmat,                                                  !/*la         15*/
     & kneutp, npidk, n1col, nobalc, nexite, nspred, nwsprd, nseudo,            !/*la         16*/
     & nbertp, ibertp, ibad10, ibad11, ibad, icpt, ipht                         !/*la         17*/
      common /label1/ zz(lmel), a(lmel), sigg(6,lmel), sigmx(lpt,lmt1),         !/*la         18*/
     & hsigg(lpt,lmt), ea(l02), eb(l02), tipa(l02), tipb(l02), ua(l02),         !/*la         19*/
     & ub(l02), va(l02), vb(l02), wa(l02), wb(l02), wta(l02), wtb(l02),         !/*la         20*/
     & tca(l02), tcb(l02), wtsum(lpt,8), aprr(3), zprr(3), erecr(3), uur        !/*la         21*/
     & (3), ur(3), vr(3), wr(3), ecvr(3), namea(l02), nel(lmt), mel(lmt)        !/*la         22*/
     & , jtipa(l02), jtipb(l02), nosum(lpt,8)                                   !/*la         23*/
      real dc, cx                                                               !/*la         24*/
      dimension dc(3), cx(3)                                                    !/*la         25*/
      equivalence (dc(1),u), (cx(1),xc), (fmu,ex)                               !/*la         26*/

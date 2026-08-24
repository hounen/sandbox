!     $Id: contrl.h,v 1.5 2002/03/27 18:14:11 gwm Exp $                       !/*co          1*/
      parameter (lpx=400, lpx1=lpx+1)                                           !/*co          2*/
      parameter (lps=100)                                                       !/*co          3*/
      logical lft, lfe, lfte, lbunch, ltesting, l_deg                           !/*co          4*/
      logical lsntim, lsnerg, lsntyp, lsnprm, lsfprm, ifeng                     !/*co          5*/
      common /contrl/ fseg(lps), cv(3), twopi, titl1(10), titl2(10),            !/*co          6*/
     & fnorm, fparm(lpx), dnparm(lpx1), cpa, lparm(lpx), iopt, itip(lpt)        !/*co          7*/
     & , ntyp1, nfprm, icase, ifirst, lowaz, ihiaz, iptll, iptls, iphld,        !/*co          8*/
     & nexlo, nocas0, ncp(lpt), ipcov, ndel, ifeng, lfll, kopt, jmat,           !/*co          9*/
     & lsize, lft, lfe, lfte, nseg, nparm1, nfprm1, nseg1, ifseg, iscrt,        !/*co         10*/
     & ntype, nbin, nparm, lsntim, lsnerg, lsntyp, lsnprm, lsfprm,              !/*co         11*/
     & nplot, kplot, jopt, lbunch, ltesting, l_deg, ipfsq, ixout, irs,          !/*co         12*/
     & imerge, itconv, irsp, imom, iyield, itmult, in, io, itty, jtty,          !/*co         13*/
     & nobt10, nobt11,ncol10,ncol11,nocs10,nocs11,nocasx                        !/*co         14*/

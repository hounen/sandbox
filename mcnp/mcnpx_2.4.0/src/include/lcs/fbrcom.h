!     $Id: fbrcom.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                       !/*fb          1*/
      parameter (nmx=3)                                                         !/*fb          2*/
      common /fbrcom/ atop, z(lmas), a(lmas), e(lstat), aj(lstat), wtl          !/*fb          3*/
     & (lstat), tiso(lstat), parity(lstat), unstab, xm(lmas), axm(lmas),        !/*fb          4*/
     & roh(lmas), xminv(lmas), v(22), rh(22), q1(mch5), wf(mch5), coulch        !/*fb          5*/
     & (mch1), coulck(mch1), q0(mch1), w0(mch1), fz(lchan), fa(lchan),          !/*fb          6*/
     & flz(lchan), fla(lchan), gam(lchan), wff(mch6), nklev(lstat), jns         !/*fb          7*/
     & (lmas1), ip(11,13), lout, lmass, iok1, itop, nr, nrm, irz, ira,          !/*fb          8*/
     & jz(lmas), ja(lmas), ns(lmas), ipdat(lmas), npdat(lmas), izaps            !/*fb          9*/
     & (mch1), itr(mch6), izap(lchan,mch2), il0(lstat), n1(mch5), klev1         !/*fb         10*/
     & (mch5), izap1(nmx,mch5)                                                  !/*fb         11*/

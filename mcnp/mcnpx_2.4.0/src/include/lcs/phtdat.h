!     $Id: phtdat.h,v 1.4 2002/03/27 18:14:17 gwm Exp $                       !/*ph          1*/
      parameter (len1=92000, len2=3000, len3=110000)                            !/*ph          2*/
      common /phtdat/ elo(len1), tlev(len1), pmode(len3), izao(len2),           !/*ph          3*/
     & ipt(len2), iref(105), isp(len1), idk(len1), lfin(len3)                   !/*ph          4*/

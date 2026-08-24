!     $Id: ctcon.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                        !/*ct          1*/
      parameter (mtprb=200)                                                     !/*ct          2*/
      common /ctcon0/ twit, tpeak, tsig, itopt                                  !/*ct          3*/
      common /ctcon1/ tsrc(mtprb), tprb(mtprb), ntprb                           !/*ct          4*/
      common /ctcon2/ twit1, twit2, aa, bb, itopt2                              !/*ct          5*/

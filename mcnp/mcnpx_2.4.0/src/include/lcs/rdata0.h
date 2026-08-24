!     $Id: rdata0.h,v 1.5 2002/03/27 18:14:17 gwm Exp $                       !/*rd          1*/
#ifdef CHEAP                                                                    /*!rd          2*/
      real*8 rani, ranj, rijk, rijk0, ranb, rans                                !/*rd          3*/
#endif                                                                          /*!rd          4*/
!     Comment formerly required to avoid a DEC ALPHA preprocessor bug.          !/*rd          5*/
      common /rdata/ rijk, rijk0, rani, ranj, ranb, rans, nrand                 !/*rd          6*/

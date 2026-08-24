!     $Id: charht.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                       !/*ch          1*/
      parameter (ndef=8)                                                        !/*ch          2*/
      character msub(ndef)*8, isub(ndef)*8                                      !/*ch          3*/
      character*8 int, outt, histp, histx, bertin, opt8a, opt15a, phtlib        !/*ch          4*/
      common /charht/ msub, int, outt, histp, histx, bertin, opt8a,             !/*ch          5*/
     & opt15a, phtlib                                                           !/*ch          6*/
      equivalence (isub,int)                                                    !/*ch          7*/

!     $Id: charst.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                       !/*ch          1*/
      parameter (ndef=3)                                                        !/*ch          2*/
      character msub(ndef)*8, isub(ndef)*8                                      !/*ch          3*/
      character*8 outst, histp, histin                                          !/*ch          4*/
      common /charst/ msub, outst, histp, histin                                !/*ch          5*/
      equivalence (isub(1),outst)                                               !/*ch          6*/

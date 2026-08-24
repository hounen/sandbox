!     $Id: charhc.h,v 1.4 2002/03/27 18:14:10 gwm Exp $                       !/*ch          1*/
      parameter (ndef=3)                                                        !/*ch          2*/
      character msub(ndef)*8, isub(ndef)*8                                      !/*ch          3*/
      character*8 hcnvout, bcdtp, bintp                                         !/*ch          4*/
      common /charhc/ msub, hcnvout, bcdtp, bintp                               !/*ch          5*/
      equivalence (isub,hcnvout)                                                !/*ch          6*/

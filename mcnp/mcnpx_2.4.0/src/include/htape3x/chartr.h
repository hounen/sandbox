!     $Id: chartr.h,v 1.4 2002/03/27 18:14:11 gwm Exp $                       !/*ch          1*/
      parameter (ndef=6)                                                        !/*ch          2*/
      character msub(ndef)*8, isub(ndef)*8                                      !/*ch          3*/
      character*8 otrx, filin, bcdlib, binlib, error1, error2                   !/*ch          4*/
      common /chartr/ msub, otrx, filin, bcdlib, binlib, error1, error2         !/*ch          5*/
      equivalence (isub,otrx)                                                   !/*ch          6*/

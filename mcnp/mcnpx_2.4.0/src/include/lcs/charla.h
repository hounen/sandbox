!     $Id: charla.h,v 1.8 2002/03/27 18:14:13 gwm Exp $                       !/*ch          1*/
      parameter (ndef=11)                                                       !/*ch          2*/
      character msub(ndef)*8, isub(ndef)*8                                      !/*ch          3*/
      character aid*80, idtm*19, probid*19                                      !/*ch          4*/
      character*8 inh, outh, histp, neutp, bertin, elstin, histin,              !/*ch          5*/
     & histina, histinb, histinc, phtlib, mdrp                                  !/*ch          6*/
      common /charla/ msub, inh, outh, histp, neutp, bertin, elstin,            !/*ch          7*/
     & histin, histina, histinb, histinc, phtlib, mdrp,                         !/*ch          8*/
     & aid, idtm, probid                                                        !/*ch          9*/
      equivalence (isub,inh)                                                    !/*ch         10*/

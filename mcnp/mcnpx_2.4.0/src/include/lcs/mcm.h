!     $Id: mcm.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                          !/*mc          1*/
      common /mcm/ amornt, amrnst, btot, btotpr, check7, check8, count0,        !/*mc          2*/
     & count1, count2, count3, count4, count5, ernprt, ernt, estart,            !/*mc          3*/
     & etot, etotpr, tarmat, zkz, resmas(5), avere(10), yield(10), fug(5        !/*mc          4*/
     & ), einct(10), anumbe(10), nmlteo, nttme                                  !/*mc          5*/
      dimension cutcm(5), cutl(5)                                               !/*mc          6*/
      equivalence (cutcm(1),resmas(1),cutl(1))                                  !/*mc          7*/

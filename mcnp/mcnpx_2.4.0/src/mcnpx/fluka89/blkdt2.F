!     $Id: blkdt2.F,v 1.4 2002/03/27 18:14:40 gwm Exp $                       !/*bl          1*/
      block data blkdt2                                                         !/*bl          2*/
!                                                                     *         !/*bl          3*/
!---------------------------------------------------------------------*         !/*bl          4*/
!                                                                     *         !/*bl          5*/
#include <lcs/double.h>                                                         /*!bl          6*/
#include <fluka89/flprm1.h>                                                     /*!bl          7*/
!                                                                     *         !/*bl          8*/
#include <fluka89/flprm2.h>                                                     /*!bl          9*/
!         character*8 edi, edi1, edi2      not used here! a.f.                  !/*bl         10*/
!REP  common /cddt97/ cddt                                                      !/*bl         11*/
!REP  save cddt                                                                 !/*bl         12*/
!REP  data cddt /.3d0/                                                          !/*bl         13*/
#include <fluka89/run2.h>                                                       /*!bl         14*/
      data runt /2*100.d0/                                                      !/*bl         15*/
!*ca idgb                                                                       !/*bl         16*/
!     data idgb, idg /0,0/                                                      !/*bl         17*/
#include <fluka89/encote.h>                                                     /*!bl         18*/
!*ca nucthr                                                                     !/*bl         19*/
      data tnkte /1.d-3/                                                        !/*bl         20*/
!REP  data ethr, tnkte /2*1.d-3/                                                !/*bl         21*/
!=                                               end*block.blkdt2      *        !/*bl         22*/
      end                                                                       !/*bl         23*/
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        !/*bl         24*/

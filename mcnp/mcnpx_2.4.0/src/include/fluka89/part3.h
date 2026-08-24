!     $Id: part3.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                        !/*pa          1*/
!***********************************************************************        !/*pa          2*/
!     note: see also part2 copy and part copy                                   !/*pa          3*/
!     changes: none                                                             !/*pa          4*/
!     included in the following subroutines or functions: not updated           !/*pa          5*/
!                                                                               !/*pa          6*/
!     description of the common block(s) and variable(s)                        !/*pa          7*/
!                                                                               !/*pa          8*/
!***********************************************************************        !/*pa          9*/
      parameter (idmax8=183)                                                    !/*pa         10*/
      character*8 anamc                                                         !/*pa         11*/
!     integer*2 ichc, ibarc, k1c, k2c                                           !/*pa         12*/
      common /part/ anamc(idmax8), amc(idmax8), gac(idmax8), tauc(idmax8        !/*pa         13*/
     & ), ichc(idmax8), ibarc(idmax8), k1c(idmax8), k2c(idmax8)                 !/*pa         14*/

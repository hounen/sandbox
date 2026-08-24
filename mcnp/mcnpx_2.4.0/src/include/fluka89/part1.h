!     $Id: part1.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                        !/*pa          1*/
!***********************************************************************        !/*pa          2*/
!     note: see also part2 copy and part3 copy                                  !/*pa          3*/
!     changes: none                                                             !/*pa          4*/
!     included in the following subroutines or functions: not updated           !/*pa          5*/
!                                                                               !/*pa          6*/
!     description of the common block(s) and variable(s)                        !/*pa          7*/
!                                                                               !/*pa          8*/
!***********************************************************************        !/*pa          9*/
      parameter (idmax8=183)                                                    !/*pa         10*/
      character*8 aname                                                         !/*pa         11*/
!     integer*2 ich, ibar, k1, k2                                               !/*pa         12*/
      common /part/ aname(idmax8), am(idmax8), ga(idmax8), tau(idmax8),         !/*pa         13*/
     & ich(idmax8), ibar(idmax8), k1(idmax8), k2(idmax8)                        !/*pa         14*/

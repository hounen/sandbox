!     $Id: decyc2.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*de          1*/
!***********************************************************************        !/*de          2*/
!     note: see also decayc copy                                                !/*de          3*/
!     changes: none                                                             !/*de          4*/
!     included in the following subroutines or functions: not updated           !/*de          5*/
!                                                                               !/*de          6*/
!     description of the common block(s) and variable(s)                        !/*de          7*/
!                                                                               !/*de          8*/
!***********************************************************************        !/*de          9*/
      parameter (idmax9=540)                                                    !/*de         10*/
      character*8 zknamc                                                        !/*de         11*/
      common /decayc/ zknamc(idmax9), nzkc(idmax9,3), wtc(idmax9)               !/*de         12*/

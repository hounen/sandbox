!     $Id: fnlsp2.h,v 1.4 2002/03/27 18:14:07 gwm Exp $                       !/*fn          1*/
!***********************************************************************        !/*fn          2*/
!     note: see also file finlsp copy                                           !/*fn          3*/
!     changes: none                                                             !/*fn          4*/
!     included in the following subroutines or functions: not updated           !/*fn          5*/
!                                                                               !/*fn          6*/
!     description of the common block(s) and variable(s)                        !/*fn          7*/
!                                                                               !/*fn          8*/
!***********************************************************************        !/*fn          9*/
      parameter (idmax7=100)                                                    !/*fn         10*/
      common /finlsp/ cxr(idmax7), cyr(idmax7), czr(idmax7), el(idmax7),        !/*fn         11*/
     & pl(idmax7), ir, itr(idmax7)                                              !/*fn         12*/

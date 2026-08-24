!     $Id: fnlsp3.h,v 1.4 2002/03/27 18:14:07 gwm Exp $                       !/*fn          1*/
!***********************************************************************        !/*fn          2*/
!     include file:  fnlsp3 copy           created 4/9/89 by a. ferrari         !/*fn          3*/
!     note: see also file finlsp copy and fnlsp2                                !/*fn          4*/
!     changes: none                                                             !/*fn          5*/
!     included in the following subroutines or functions:                       !/*fn          6*/
!                                                         eventq                !/*fn          7*/
!                                                                               !/*fn          8*/
!     description of the common block(s) and variable(s)                        !/*fn          9*/
!                                                                               !/*fn         10*/
!***********************************************************************        !/*fn         11*/
      parameter (idmax7=100)                                                    !/*fn         12*/
      common /finlsp/ cxh(idmax7), cyh(idmax7), czh(idmax7), elh(idmax7)        !/*fn         13*/
     & , plh(idmax7), ih, ith(idmax7)                                           !/*fn         14*/

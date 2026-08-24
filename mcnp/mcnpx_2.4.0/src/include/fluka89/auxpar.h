!     $Id: auxpar.h,v 1.4 2002/03/27 18:14:05 gwm Exp $                       !/*au          1*/
!***********************************************************************        !/*au          2*/
!     changes: none                                                             !/*au          3*/
!     included in the following subroutines or functions: not updated           !/*au          4*/
!                                                                               !/*au          5*/
!     description of the common block(s) and variable(s)                        !/*au          6*/
!                                                                               !/*au          7*/
!***********************************************************************        !/*au          8*/
      parameter (idmax6=1133)                                                   !/*au          9*/
      character*8 ana                                                           !/*au         10*/
      common /auxpar/ pxa(idmax6), pya(idmax6), pza(idmax6), hepa(idmax6        !/*au         11*/
     & ), ama(idmax6), icha(idmax6), ibara(idmax6), ana(idmax6), nrea           !/*au         12*/
     & (idmax6)                                                                 !/*au         13*/

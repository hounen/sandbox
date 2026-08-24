!     $Id: finpr1.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*fi          1*/
!***********************************************************************        !/*fi          2*/
!     note: do not forget file finpar copy                                      !/*fi          3*/
!     changes: none                                                             !/*fi          4*/
!     included in the following subroutines or functions: not updated           !/*fi          5*/
!                                                                               !/*fi          6*/
!     description of the common block(s) and variable(s)                        !/*fi          7*/
!                                                                               !/*fi          8*/
!***********************************************************************        !/*fi          9*/
      parameter (idmax3=1133)                                                   !/*fi         10*/
      character*8 anf                                                           !/*fi         11*/
      common /finpar/ pxf(idmax3), pyf(idmax3), pzf(idmax3), hef(idmax3)        !/*fi         12*/
     & , amf(idmax3), ichf(idmax3), ibarf(idmax3), anf(idmax3), nref            !/*fi         13*/
     & (idmax3)                                                                 !/*fi         14*/

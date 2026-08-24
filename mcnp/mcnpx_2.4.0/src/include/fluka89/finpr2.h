!     $Id: finpr2.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*fi          1*/
!***********************************************************************        !/*fi          2*/
!     note: do not forget file finpr2 copy                                      !/*fi          3*/
!     changes: none                                                             !/*fi          4*/
!     included in the following subroutines or functions: not updated           !/*fi          5*/
!                                                                               !/*fi          6*/
!     description of the common block(s) and variable(s)                        !/*fi          7*/
!                                                                               !/*fi          8*/
!***********************************************************************        !/*fi          9*/
      parameter (idmax1=1133)                                                   !/*fi         10*/
      character*8 anf                                                           !/*fi         11*/
      common /finpar/ pxf(idmax1), pyf(idmax1), pzf(idmax1), hep(idmax1)        !/*fi         12*/
     & , amf(idmax1), ichf(idmax1), ibarf(idmax1), anf(idmax1), nref            !/*fi         13*/
     & (idmax1)                                                                 !/*fi         14*/

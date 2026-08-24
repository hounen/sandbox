!     $Id: cmsres.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*cm          1*/
!***********************************************************************        !/*cm          2*/
!     changes: none                                                             !/*cm          3*/
!     included in the following subroutines or functions: not updated           !/*cm          4*/
!                                                                               !/*cm          5*/
!     description of the common block(s) and variable(s)                        !/*cm          6*/
!                                                                               !/*cm          7*/
!***********************************************************************        !/*cm          8*/
      parameter (idmax5=1133)                                                   !/*cm          9*/
      character*8 anr                                                           !/*cm         10*/
      common /cmsres/ pxr(idmax5), pyr(idmax5), pzr(idmax5), her(idmax5)        !/*cm         11*/
     & , amr(idmax5), ichr(idmax5), ibarr(idmax5), anr(idmax5), nrer            !/*cm         12*/
     & (idmax5), nres                                                           !/*cm         13*/

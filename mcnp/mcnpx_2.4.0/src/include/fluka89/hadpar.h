!     $Id: hadpar.h,v 1.4 2002/03/27 18:14:07 gwm Exp $                       !/*ha          1*/
!***********************************************************************        !/*ha          2*/
!     changes: none                                                             !/*ha          3*/
!     included in the following subroutines or functions: not updated           !/*ha          4*/
!                                                                               !/*ha          5*/
!     description of the common block(s) and variable(s)                        !/*ha          6*/
!                                                                               !/*ha          7*/
!***********************************************************************        !/*ha          8*/
      parameter (idmax4=1133)                                                   !/*ha          9*/
      character*8 anh                                                           !/*ha         10*/
      common /hadpar/ pxh(idmax4), pyh(idmax4), pzh(idmax4), heph(idmax4        !/*ha         11*/
     & ), amh(idmax4), ichh(idmax4), ibarh(idmax4), anh(idmax4), nreh           !/*ha         12*/
     & (idmax4)                                                                 !/*ha         13*/

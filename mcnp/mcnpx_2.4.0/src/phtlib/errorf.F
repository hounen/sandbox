!     $Id: errorf.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                       !/*er          1*/
      subroutine errorf (loc)                                                   !/*er          2*/
#include <htape3x/com32b.h>                                                     /*!er          3*/
      character *8 loc                                                          !/*er          4*/
#include <htape3x/lpt.h>                                                        /*!er          5*/
#include <htape3x/contrl.h>                                                     /*!er          6*/
      write (io,10) loc                                                         !/*er          7*/
      write (jtty,10) loc                                                       !/*er          8*/
      stop                                                                      !/*er          9*/
!                                                                               !/*er         10*/
   10 format (//'error exit in subroutine ',a8)                                 !/*er         11*/
      end                                                                       !/*er         12*/

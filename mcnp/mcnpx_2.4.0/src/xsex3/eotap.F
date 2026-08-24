!     $Id: eotap.F,v 1.4 2002/03/27 18:16:24 gwm Exp $                        !/*eo          1*/
      subroutine eotap                                                          !/*eo          2*/
#include <htape3x/com32b.h>                                                     /*!eo          3*/
#include <htape3x/label.h>                                                      /*!eo          4*/
#include <htape3x/contrl.h>                                                     /*!eo          5*/
      character *8 fname                                                        !/*eo          6*/
      backspace nhst                                                            !/*eo          7*/
      read (nhst) ncol,fname                                                    !/*eo          8*/
      call newfil (fname,nhst)                                                  !/*eo          9*/
      if (icase.le.1) then                                                      !/*eo         10*/
        write (io,10) fname                                                     !/*eo         11*/
        write (jtty,10) fname                                                   !/*eo         12*/
      endif                                                                     !/*eo         13*/
      return                                                                    !/*eo         14*/
!                                                                               !/*eo         15*/
   10 format (/5x,'new history tape assigned - ',a10)                           !/*eo         16*/
      end                                                                       !/*eo         17*/

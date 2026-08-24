!     $Id: errorf.F,v 1.4 2002/03/27 18:15:02 gwm Exp $                       !/*er          1*/
      subroutine errorf(mess)                                                   !/*er          2*/
#include <lcs/inout.h>                                                          /*!er          3*/
      character mess*58                                                         !/*er          4*/
      write(itty,10)mess                                                        !/*er          5*/
      write(io,10)mess                                                          !/*er          6*/
   10 format(' LCS error: ',a58)                                                !/*er          7*/
#ifdef UNICOS                                                                   /*!er          8*/
      call trbk                                                                 !/*er          9*/
#endif                                                                          /*!er         10*/
      stop                                                                      !/*er         11*/
      end                                                                       !/*er         12*/

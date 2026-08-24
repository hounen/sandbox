!     $Id: ipterr.F,v 1.2 2002/03/27 18:14:00 gwm Exp $                       !/*ip          1*/
      integer function ipterr (dum)                                             !/*ip          2*/
#include <htape3x/com32b.h>                                                     /*!ip          3*/
#include <htape3x/alloc.h>                                                      /*!ip          4*/
      ipterr=ierr                                                               !/*ip          5*/
      return                                                                    !/*ip          6*/
      end                                                                       !/*ip          7*/

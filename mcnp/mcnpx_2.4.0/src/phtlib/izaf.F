!     $Id: izaf.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                         !/*iz          1*/
      function izaf (iza,idm)                                                   !/*iz          2*/
#include <htape3x/com32b.h>                                                     /*!iz          3*/
      dimension ichang(19)                                                      !/*iz          4*/
      data ichang /-1,-1001,-1002,-1003,-2003,-2004,0,-1000,1000,-1000,         !/*iz          5*/
     & -2,-2002,0,0,0,2000,0,-1000,0/                                           !/*iz          6*/
      izaf=iza+ichang(idm)                                                      !/*iz          7*/
      return                                                                    !/*iz          8*/
      end                                                                       !/*iz          9*/

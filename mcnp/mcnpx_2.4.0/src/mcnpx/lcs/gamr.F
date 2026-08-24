!     $Id: gamr.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                         !/*ga          1*/
      function gamr (a,z,unew)                                                  !/*ga          2*/
#include <lcs/double.h>                                                         /*!ga          3*/
      add=dp0                                                                   !/*ga          4*/
      jz=z                                                                      !/*ga          5*/
      if ((jz/2)*2.ne.jz) add=.12d0                                             !/*ga          6*/
      if (a-z.le.153.d0) then                                                   !/*ga          7*/
        gamr=dp10**(0.14d0*(a-z)-0.276d0*z+5.460+add)                           !/*ga          8*/
      else                                                                      !/*ga          9*/
        gamr=dp10**(5.d-02*(a-z)-0.276d0*z+19.23d0+add)                         !/*ga         10*/
      endif                                                                     !/*ga         11*/
      return                                                                    !/*ga         12*/
      end                                                                       !/*ga         13*/

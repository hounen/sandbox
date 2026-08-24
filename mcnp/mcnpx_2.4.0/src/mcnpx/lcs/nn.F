!     $Id: nn.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                           !/*nn          1*/
      subroutine nn                                                             !/*nn          2*/
#include <lcs/double.h>                                                         /*!nn          3*/
#include <lcs/btcom.h>                                                          /*!nn          4*/
      fmax(1)=0.46d-24                                                          !/*nn          5*/
      fmax(2)=1.4d-24                                                           !/*nn          6*/
      do 10 i=3,6                                                               !/*nn          7*/
   10 fmax(i)=dp0                                                               !/*nn          8*/
      return                                                                    !/*nn          9*/
      end                                                                       !/*nn         10*/

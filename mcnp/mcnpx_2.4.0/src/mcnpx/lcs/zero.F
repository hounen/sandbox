!     $Id: zero.F,v 1.4 2002/03/27 18:15:27 gwm Exp $                         !/*ze          1*/
      subroutine zero                                                           !/*ze          2*/
#include <lcs/double.h>                                                         /*!ze          3*/
#include <lcs/btcom.h>                                                          /*!ze          4*/
      do 10 i=1,21                                                              !/*ze          5*/
   10 ce(i)=dp0                                                                 !/*ze          6*/
      return                                                                    !/*ze          7*/
      end                                                                       !/*ze          8*/

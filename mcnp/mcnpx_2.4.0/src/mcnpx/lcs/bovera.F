!     $Id: bovera.F,v 1.5 2002/03/27 18:14:56 gwm Exp $                       !/*bo          1*/
      subroutine bovera (v,ve,verr)                                             !/*bo          2*/
#include <lcs/double.h>                                                         /*!bo          3*/
#include <lcs/btcom.h>                                                          /*!bo          4*/
      verr=sqrt(dp1-((ve*ve)/((v*rcpmv+ve)**2)))                                !/*bo          5*/
      verr=(sqrt(verr*(.691d0+(verr*1.109d0))+.108d0))/verr                     !/*bo          6*/
      return                                                                    !/*bo          7*/
      end                                                                       !/*bo          8*/

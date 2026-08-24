!     $Id: xmnuc.F,v 1.5 2002/03/27 18:14:51 gwm Exp $                        !/*xm          1*/
      function xmnuc (a,z)                                                      !/*xm          2*/
#include <lcs/double.h>                                                         /*!xm          3*/
#include <lcs/phprm0.h>                                                         /*!xm          4*/
#include <lcs/wapcom.h>                                                         /*!xm          5*/
      character*1 c                                                             !/*xm          6*/
      xmnuc=amumev*a+xm_atm (a,z,c)-z*elmass+elbnde(int(z))                     !/*xm          7*/
      return                                                                    !/*xm          8*/
      end                                                                       !/*xm          9*/

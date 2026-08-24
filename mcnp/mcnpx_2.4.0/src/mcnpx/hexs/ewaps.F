!     $Id: ewaps.F,v 1.4 2002/03/27 18:14:49 gwm Exp $                        !/*ew          1*/
      function ewaps (a,z,c,iwaps)                                              !/*ew          2*/
#include <lcs/double.h>                                                         /*!ew          3*/
#include <lcs/phprm0.h>                                                         /*!ew          4*/
#include <lcs/wapcom.h>                                                         /*!ew          5*/
      character*1 c                                                             !/*ew          6*/
      ia=a                                                                      !/*ew          7*/
      iz=z                                                                      !/*ew          8*/
      in=ia-iz                                                                  !/*ew          9*/
      j=iz-iz0(ia)+lmid+1                                                       !/*ew         10*/
      ewaps=1.d+30                                                              !/*ew         11*/
      c=' '                                                                     !/*ew         12*/
      iwaps=0                                                                   !/*ew         13*/
      if (j.lt.1.or.j.gt.lwidth) return                                         !/*ew         14*/
      ewaps=wapz(j,ia)                                                          !/*ew         15*/
      c=cwaps(j,ia)                                                             !/*ew         16*/
      iwaps=1                                                                   !/*ew         17*/
      return                                                                    !/*ew         18*/
      end                                                                       !/*ew         19*/

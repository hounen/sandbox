!     $Id: fmax.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                         !/*fm          1*/
      function fmax (e)                                                         !/*fm          2*/
#include <lcs/double.h>                                                         /*!fm          3*/
#include <lcs/phtcom.h>                                                         /*!fm          4*/
      fact=dp1                                                                  !/*fm          5*/
      epp=e-(e-eminn)*1.0d-06                                                   !/*fm          6*/
      call conprb (e,e,iia)                                                     !/*fm          7*/
      t1=psjsum                                                                 !/*fm          8*/
      call conprb (e,epp,iia)                                                   !/*fm          9*/
      t2=psjsum                                                                 !/*fm         10*/
      if (t2.le.t1) fact=t1                                                     !/*fm         11*/
      finv=dp1/fact                                                             !/*fm         12*/
      fmax=fact                                                                 !/*fm         13*/
      return                                                                    !/*fm         14*/
      end                                                                       !/*fm         15*/

!     $Id: gaurn.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                        !/*ga          1*/
      function gaurn ()                                                         !/*ga          2*/
! The following generates a true gaussian deviate (mean=0, std=1)               !/*ga          3*/
! C&E III, C51, Rx3.                                                            !/*ga          4*/
#include <lcs/double.h>                                                         /*!ga          5*/
#include <lcs/rancom.h>                                                         /*!ga          6*/
      if (gauhold.ne.dp0) then                                                  !/*ga          7*/
        gaurn=gauhold                                                           !/*ga          8*/
        gauhold=dp0                                                             !/*ga          9*/
      else                                                                      !/*ga         10*/
   10   continue                                                                !/*ga         11*/
        r1=dp2*rang()-dp1                                                       !/*ga         12*/
        r2=dp2*rang()-dp1                                                       !/*ga         13*/
        rsq=r1**2+r2**2                                                         !/*ga         14*/
        if (rsq.ge.dp1) go to 10                                                !/*ga         15*/
        temp=sqrt(-dp2*log(rsq)/rsq)                                            !/*ga         16*/
        gaurn=r1*temp                                                           !/*ga         17*/
        gauhold=r2*temp                                                         !/*ga         18*/
      endif                                                                     !/*ga         19*/
      return                                                                    !/*ga         20*/
      end                                                                       !/*ga         21*/

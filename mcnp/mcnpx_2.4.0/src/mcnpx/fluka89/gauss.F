!     $Id: gauss.F,v 1.4 2002/03/27 18:14:44 gwm Exp $                        !/*ga          1*/
      subroutine gauss (x,a,s)                                                  !/*ga          2*/
!                                                                               !/*ga          3*/
!     The following generates a true gaussian deviate (mean=a, std=s)           !/*ga          4*/
!     C&E III, C51, Rx3.                                                        !/*ga          5*/
#include <lcs/double.h>                                                         /*!ga          6*/
#include <lcs/rancom.h>                                                         /*!ga          7*/
!                                                                               !/*ga          8*/
      if (gauhold.ne.dp0) then                                                  !/*ga          9*/
        grn=gauhold                                                             !/*ga         10*/
        gauhold=dp0                                                             !/*ga         11*/
      else                                                                      !/*ga         12*/
   10   r1=dp2*rang()-dp1                                                       !/*ga         13*/
        r2=dp2*rang()-dp1                                                       !/*ga         14*/
        rsq=r1**2+r2**2                                                         !/*ga         15*/
        if (rsq.ge.dp1) go to 10                                                !/*ga         16*/
        temp=sqrt(-dp2*log(rsq)/rsq)                                            !/*ga         17*/
        grn=r1*temp                                                             !/*ga         18*/
        gauhold=r2*temp                                                         !/*ga         19*/
      endif                                                                     !/*ga         20*/
!                                                                               !/*ga         21*/
      x=grn*s+a                                                                 !/*ga         22*/
      return                                                                    !/*ga         23*/
      end                                                                       !/*ga         24*/

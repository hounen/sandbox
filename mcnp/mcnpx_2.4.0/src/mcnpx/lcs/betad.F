!     $Id: betad.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                        !/*be          1*/
      function betad (n)                                                        !/*be          2*/
#include <lcs/double.h>                                                         /*!be          3*/
!                                                                               !/*be          4*/
!  BETAD samples the beta distribution given by                                 !/*be          5*/
!           p(v)=v**dph*(1-v)**(3*n/2-4)/beta(3/2,3*n/2-3)                      !/*be          6*/
!  for n .ge. 3.                                                                !/*be          7*/
!  For use in HETC, only the n=3 case is retained                               !/*be          8*/
!                                                                               !/*be          9*/
!     REFERENCES.......  MOMENT                                                 !/*be         10*/
!     EXTERNALS........  RANF      ALOG      MOD                                !/*be         11*/
!                                                                               !/*be         12*/
#include <lcs/rancom.h>                                                         /*!be         13*/
      if (bethold.ne.dp0) then                                                  !/*be         14*/
        tau2=bethold                                                            !/*be         15*/
        bethold=dp0                                                             !/*be         16*/
      else                                                                      !/*be         17*/
   10   continue                                                                !/*be         18*/
        r1=dp2*rang()-dp1                                                       !/*be         19*/
        sum=r1**2                                                               !/*be         20*/
        r2=dp2*rang()-dp1                                                       !/*be         21*/
        sum=sum+r2**2                                                           !/*be         22*/
        if (sum.ge.1) go to 10                                                  !/*be         23*/
        fac=-log(sum)/sum                                                       !/*be         24*/
        tau2=fac*r1**2                                                          !/*be         25*/
        bethold=fac*r2**2                                                       !/*be         26*/
      endif                                                                     !/*be         27*/
      x=tau2-log(rang())                                                        !/*be         28*/
      if (bethold.ne.0) then                                                    !/*be         29*/
        tau2=bethold                                                            !/*be         30*/
        bethold=dp0                                                             !/*be         31*/
      else                                                                      !/*be         32*/
   20   continue                                                                !/*be         33*/
        r1=dp2*rang()-dp1                                                       !/*be         34*/
        sum=r1**2                                                               !/*be         35*/
        r2=dp2*rang()-dp1                                                       !/*be         36*/
        sum=sum+r2**2                                                           !/*be         37*/
        if (sum.ge.1) go to 20                                                  !/*be         38*/
        fac=-log(sum)/sum                                                       !/*be         39*/
        tau2=fac*r1**2                                                          !/*be         40*/
        bethold=fac*r2**2                                                       !/*be         41*/
      endif                                                                     !/*be         42*/
      y=tau2-log(rang())                                                        !/*be         43*/
      betad=x/(x+y)                                                             !/*be         44*/
      return                                                                    !/*be         45*/
      end                                                                       !/*be         46*/

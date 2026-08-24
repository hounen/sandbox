!     $Id: drein1.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                       !/*dr          1*/
      subroutine drein1 (j,s,a,eyi1,eyi0)                                       !/*dr          2*/
#include <lcs/double.h>                                                         /*!dr          3*/
!                                                                               !/*dr          4*/
!     compute statistical theory emission integrals                             !/*dr          5*/
!     for s<dph use a series expansion.                                         !/*dr          6*/
!     for s>dph the explicit relationship                                       !/*dr          7*/
!     return for neutrons (and compute eyi0)                                    !/*dr          8*/
!     return 1 for all others.                                                  !/*dr          9*/
!                                                                               !/*dr         10*/
!     coeficients for series expansions                                         !/*dr         11*/
!                                                                               !/*dr         12*/
!     correction of c1 by r. e. prael                                           !/*dr         13*/
      dimension c0(7), c1(7)                                                    !/*dr         14*/
      data c0 /0.66666667d0,0.25d0,0.06666667d0,0.01388889d0,0.00238095d        !/*dr         15*/
     & 0,0.00034722d0,0.00004409d0/                                             !/*dr         16*/
      data c1 /0.53333333d0,0.16666667d0,0.03809524d0,0.00694444d0,0.001        !/*dr         17*/
     & 0582d0,0.00013889d0,0.0000160d0/                                         !/*dr         18*/
!                                                                               !/*dr         19*/
      exps=dp0                                                                  !/*dr         20*/
      if (s.lt.1.d+02) exps=exp(-s)                                             !/*dr         21*/
      if (s.lt.dph) go to 10                                                    !/*dr         22*/
!///// explicit relation                                                        !/*dr         23*/
      b=dph/a                                                                   !/*dr         24*/
      eyi1=b*b*(dp3+s*(s-dp3)+exps*(dph*s*s-dp3))                               !/*dr         25*/
      if (j.eq.1) eyi0=b*(s-dp1+exps)                                           !/*dr         26*/
      return                                                                    !/*dr         27*/
!///// small s series expansion                                                 !/*dr         28*/
   10 continue                                                                  !/*dr         29*/
!                                                                               !/*dr         30*/
!  eyi1=(1.0/(8*a*a))*(s**4/4)*(sum n=0 to 7:8*s**n/(n!*(n+2)*(n+4))            !/*dr         31*/
!                                                                               !/*dr         32*/
      eyi1=dp1                                                                  !/*dr         33*/
      b=dp1                                                                     !/*dr         34*/
      do 20 n=1,7                                                               !/*dr         35*/
      b=b*s                                                                     !/*dr         36*/
      c=b*c1(n)                                                                 !/*dr         37*/
      if (c.lt.1.0d-7) go to 30                                                 !/*dr         38*/
      eyi1=eyi1+c                                                               !/*dr         39*/
   20 continue                                                                  !/*dr         40*/
   30 continue                                                                  !/*dr         41*/
      b=s*s/a                                                                   !/*dr         42*/
      eyi1=eyi1*exps*b*b*0.03125d0                                              !/*dr         43*/
      if (j.gt.1) return                                                        !/*dr         44*/
! eyi0 (neutrons only)=(.5/a)*s**2/2*(sum n=0 to 7:2*s**n/(n!*(n+2))            !/*dr         45*/
      eyi0=dp1                                                                  !/*dr         46*/
      b=dp1                                                                     !/*dr         47*/
      do 40 n=1,7                                                               !/*dr         48*/
      b=b*s                                                                     !/*dr         49*/
      c=b*c0(n)                                                                 !/*dr         50*/
      if (c.lt.1.0d-7) go to 50                                                 !/*dr         51*/
      eyi0=eyi0+c                                                               !/*dr         52*/
   40 continue                                                                  !/*dr         53*/
   50 continue                                                                  !/*dr         54*/
      eyi0=exps*eyi0*s*s*0.25d0/a                                               !/*dr         55*/
      return                                                                    !/*dr         56*/
      end                                                                       !/*dr         57*/

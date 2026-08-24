!     $Id: pgs.F,v 1.4 2002/03/27 18:15:14 gwm Exp $                          !/*pg          1*/
      function pgs (e,emn)                                                      !/*pg          2*/
#include <lcs/double.h>                                                         /*!pg          3*/
#include <lcs/phtcom.h>                                                         /*!pg          4*/
!.....                                                                          !/*pg          5*/
!     PGS computes log base 10 of continuum probability                         !/*pg          6*/
!.....                                                                          !/*pg          7*/
      data fac /1.0d-06/                                                        !/*pg          8*/
      f(x)=6.+x*(6.+x*(3.+x))                                                   !/*pg          9*/
      if ((e-emn).gt.fac) then                                                  !/*pg         10*/
        s0=(e-min(e,exx))/tn                                                    !/*pg         11*/
        if (s0.gt.dp0) then                                                     !/*pg         12*/
          u=e-de                                                                !/*pg         13*/
          x1=dp2*sqrt(u*aa)                                                     !/*pg         14*/
!           c2ssq=dp2*sqrt(aa*ux)                                               !/*pg         15*/
          if (exx.gt.emn) then                                                  !/*pg         16*/
            x0=c2ssq                                                            !/*pg         17*/
          else                                                                  !/*pg         18*/
            x0=dp2*sqrt(aa*(emn-de))                                            !/*pg         19*/
          endif                                                                 !/*pg         20*/
          x12=x1**2                                                             !/*pg         21*/
          x02=x0**2                                                             !/*pg         22*/
          t1=3.0*x12*(x12-x02)+x02**2+x0*(6.*x12-4.*x02)+(12.*x02-6.*x12        !/*pg         23*/
     &    )-24.*(x0-1.)                                                         !/*pg         24*/
          t2=x12*(x12+2.*x1+6.)-24.*(x1-1)                                      !/*pg         25*/
          t2=t2*exp(x1-x0)                                                      !/*pg         26*/
          t3=x12**3*exp(-x0)*fint(x0,x1)                                        !/*pg         27*/
          p2=t3-t2+t1                                                           !/*pg         28*/
          p2=2.*p2*p2ux*x0*x02/(4.0*aa)**4                                      !/*pg         29*/
        else                                                                    !/*pg         30*/
          p2=dp0                                                                !/*pg         31*/
        endif                                                                   !/*pg         32*/
        if (exx.gt.emn) then                                                    !/*pg         33*/
          smax=(e-emn)/tn                                                       !/*pg         34*/
          if (smax.lt.0.01.and.abs(s0).lt.0.01) then                            !/*pg         35*/
            temp2=0.25*(exp(s0-smax)*smax**4-s0**4)                             !/*pg         36*/
          else                                                                  !/*pg         37*/
            temp2=f(s0)-f(smax)*exp(s0-smax)                                    !/*pg         38*/
          endif                                                                 !/*pg         39*/
          p1=p2ux*temp2*tn**4                                                   !/*pg         40*/
          if (s0.le.0) p1=p1*exp((e-exx)/tn)                                    !/*pg         41*/
        else                                                                    !/*pg         42*/
          p1=dp0                                                                !/*pg         43*/
        endif                                                                   !/*pg         44*/
        temp=p1+p2                                                              !/*pg         45*/
      else                                                                      !/*pg         46*/
        temp=dp0                                                                !/*pg         47*/
      endif                                                                     !/*pg         48*/
      if (temp.le.dp0) then                                                     !/*pg         49*/
        pgs=-1.0d+30                                                            !/*pg         50*/
      else                                                                      !/*pg         51*/
        pgs=log10(temp)                                                         !/*pg         52*/
      endif                                                                     !/*pg         53*/
      return                                                                    !/*pg         54*/
      end                                                                       !/*pg         55*/

!     $Id: skeme.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                        !/*sk          1*/
      function skeme (e,emn)                                                    !/*sk          2*/
#include <lcs/double.h>                                                         /*!sk          3*/
#include <lcs/phtcom.h>                                                         /*!sk          4*/
      smax=(e-emn)/tn                                                           !/*sk          5*/
   10 continue                                                                  !/*sk          6*/
      if (smax.lt.dp3) then                                                     !/*sk          7*/
   20   continue                                                                !/*sk          8*/
        x=rang()                                                                !/*sk          9*/
        r=exp(smax*(1-x))*x**3                                                  !/*sk         10*/
        if (rang().gt.r) go to 20                                               !/*sk         11*/
        s=smax*x                                                                !/*sk         12*/
      else                                                                      !/*sk         13*/
   30   continue                                                                !/*sk         14*/
        r1=rang()                                                               !/*sk         15*/
        r2=rang()                                                               !/*sk         16*/
        r3=rang()                                                               !/*sk         17*/
        r4=rang()                                                               !/*sk         18*/
        s=-log(r1*r2*r3*r4)                                                     !/*sk         19*/
        if (s.gt.smax) go to 30                                                 !/*sk         20*/
      endif                                                                     !/*sk         21*/
      e1=e-s*tn                                                                 !/*sk         22*/
      if (e1.gt.exx) then                                                       !/*sk         23*/
        u=e1-de                                                                 !/*sk         24*/
        rej=(ux/u)**3*exp(4.0*sqrt(aa*u)-6.0-dp2*(u+ux)/tn)                     !/*sk         25*/
        r1=rang()                                                               !/*sk         26*/
        if (r1**2.gt.rej) go to 10                                              !/*sk         27*/
      endif                                                                     !/*sk         28*/
      skeme=e1                                                                  !/*sk         29*/
      return                                                                    !/*sk         30*/
      end                                                                       !/*sk         31*/

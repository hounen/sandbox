!     $Id: drein2.F,v 1.4 2002/03/27 18:15:01 gwm Exp $                       !/*dr          1*/
      subroutine drein2 (s,a,eye2)                                              !/*dr          2*/
#include <lcs/double.h>                                                         /*!dr          3*/
!                                                                               !/*dr          4*/
!     compute statistical theory emission integrals                             !/*dr          5*/
!                                                                               !/*dr          6*/
!     compute third integral                                                    !/*dr          7*/
!                                                                               !/*dr          8*/
!     for s<dph use a series expansion.                                         !/*dr          9*/
!     for s>dph the explicit relationship                                       !/*dr         10*/
!                                                                               !/*dr         11*/
!     coeficients for series expansions                                         !/*dr         12*/
!                                                                               !/*dr         13*/
      dimension c2(7)                                                           !/*dr         14*/
      data c2 /0.45714286d0,0.125d0,0.02539683d0,0.00416667d0,0.0005772d        !/*dr         15*/
     & 0,0.00006944d0,0.0000074d0/                                              !/*dr         16*/
      exps=dp0                                                                  !/*dr         17*/
      if (s.lt.1.d+02) exps=exp(-s)                                             !/*dr         18*/
      if (s.lt.dph) go to 10                                                    !/*dr         19*/
!///// explicit relation                                                        !/*dr         20*/
      b=s*s                                                                     !/*dr         21*/
      eye2=0.25d0*(s*(15.d0-s*(6.d0-s))-15.d0+(15.d0+0.125d0*b*(b-12.d0)        !/*dr         22*/
     & )*exps)/(a*a*a)                                                          !/*dr         23*/
      return                                                                    !/*dr         24*/
   10 continue                                                                  !/*dr         25*/
!                                                                               !/*dr         26*/
!     series expansion                                                          !/*dr         27*/
!     eye2=(1/(32a**3))*s**6/6*(sum n=0 to 7:48*s**n/(n!(n+2)(n+4)(n+6))        !/*dr         28*/
!                                                                               !/*dr         29*/
      eye2=dp1                                                                  !/*dr         30*/
      b=dp1                                                                     !/*dr         31*/
      do 20 n=1,7                                                               !/*dr         32*/
      b=b*s                                                                     !/*dr         33*/
      c=b*c2(n)                                                                 !/*dr         34*/
      if (c.lt.1.0d-7) go to 30                                                 !/*dr         35*/
      eye2=eye2+c                                                               !/*dr         36*/
   20 continue                                                                  !/*dr         37*/
   30 continue                                                                  !/*dr         38*/
      b=0.25d0*s*s/a                                                            !/*dr         39*/
      eye2=eye2*b*b*b*exps*0.33333333d0                                         !/*dr         40*/
      return                                                                    !/*dr         41*/
      end                                                                       !/*dr         42*/

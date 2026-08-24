!     $Id: vl.F,v 1.4 2002/03/27 18:15:26 gwm Exp $                           !/*vl          1*/
      function vl (rk2,l)                                                       !/*vl          2*/
#include <lcs/double.h>                                                         /*!vl          3*/
!                                                                               !/*vl          4*/
! Function VL computes the angular momentum barrier factor for angular          !/*vl          5*/
! momentum L.                                                                   !/*vl          6*/
!                                                                               !/*vl          7*/
!    REFERENCES.......  LEVBRK                                                  !/*vl          8*/
!    EXTERNALS........  EXP                                                     !/*vl          9*/
!                                                                               !/*vl         10*/
      if (l.eq.0) then                                                          !/*vl         11*/
        vl=dp1                                                                  !/*vl         12*/
        go to 60                                                                !/*vl         13*/
      elseif (l.gt.5) then                                                      !/*vl         14*/
        fl=l                                                                    !/*vl         15*/
        arg=fl*(fl+dp1)/rk2                                                     !/*vl         16*/
        vl=exp(-arg)                                                            !/*vl         17*/
        go to 60                                                                !/*vl         18*/
      endif                                                                     !/*vl         19*/
      go to (10,20,30,40,50), l                                                 !/*vl         20*/
   10 continue                                                                  !/*vl         21*/
      vl=rk2/(rk2+1)                                                            !/*vl         22*/
      go to 60                                                                  !/*vl         23*/
   20 continue                                                                  !/*vl         24*/
      rk4=rk2**2                                                                !/*vl         25*/
      vl=rk4/(rk4+dp3*rk2+9.d0)                                                 !/*vl         26*/
      go to 60                                                                  !/*vl         27*/
   30 continue                                                                  !/*vl         28*/
      rk4=rk2*rk2                                                               !/*vl         29*/
      rk6=rk4*rk2                                                               !/*vl         30*/
      vl=rk6/(rk6+6.d0*rk4+45.d0*rk2+225.d0)                                    !/*vl         31*/
      go to 60                                                                  !/*vl         32*/
   40 continue                                                                  !/*vl         33*/
      rk4=rk2*rk2                                                               !/*vl         34*/
      y=(rk4-45.d0*rk2+105.d0)**2+rk2*(dp10*rk2-105.d0)**2                      !/*vl         35*/
      vl=rk4**2/y                                                               !/*vl         36*/
      go to 60                                                                  !/*vl         37*/
   50 continue                                                                  !/*vl         38*/
      rk4=rk2*rk2                                                               !/*vl         39*/
      y=rk2*(rk4-105.d0*rk2+945.d0)**2+(15.d0*rk4-42.d1*rk2+945.d0)**2          !/*vl         40*/
      vl=rk4*rk4*rk2/y                                                          !/*vl         41*/
   60 continue                                                                  !/*vl         42*/
      return                                                                    !/*vl         43*/
      end                                                                       !/*vl         44*/

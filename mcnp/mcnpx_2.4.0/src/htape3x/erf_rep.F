!     $Id: erf_rep.F,v 1.3 2002/03/27 18:13:58 gwm Exp $                      !/*er          1*/
      function erf_rep (x)                                                      !/*er          2*/
#include <htape3x/com32b.h>                                                     /*!er          3*/
!***library=slatec                                                              !/*er          4*/
!***author  fullerton, w., (lanl)                                               !/*er          5*/
!***purpose  computes the error function, ERF.                                  !/*er          6*/
!***routines called  csevl,erfc_rep                                             !/*er          7*/
!***end prologue  ERF                                                           !/*er          8*/
      dimension erfcs(13)                                                       !/*er          9*/
      data erfcs(1) /-.049046121234691808d0/                                    !/*er         10*/
      data erfcs(2) /-.14226120510371364d0/                                     !/*er         11*/
      data erfcs(3) /.010035582187599796d0/                                     !/*er         12*/
      data erfcs(4) /-.000576876469976748d0/                                    !/*er         13*/
      data erfcs(5) /.000027419931252196d0/                                     !/*er         14*/
      data erfcs(6) /-.000001104317550734d0/                                    !/*er         15*/
      data erfcs(7) /.000000038488755420d0/                                     !/*er         16*/
      data erfcs(8) /-.000000001180858253d0/                                    !/*er         17*/
      data erfcs(9) /.000000000032334215d0/                                     !/*er         18*/
      data erfcs(10) /-.000000000000799101d0/                                   !/*er         19*/
      data erfcs(11) /.000000000000017990d0/                                    !/*er         20*/
      data erfcs(12) /-.000000000000000371d0/                                   !/*er         21*/
      data erfcs(13) /.000000000000000007d0/                                    !/*er         22*/
      data sqrtpi /1.7724538509055160d0/                                        !/*er         23*/
      data nterf /11/                                                           !/*er         24*/
      data xbig /0.56573450083402577d+01/                                       !/*er         25*/
      data sqeps /0.11920928955078125d-06/                                      !/*er         26*/
!                                                                               !/*er         27*/
      y=abs(x)                                                                  !/*er         28*/
      if (y.gt.dp1) go to 10                                                    !/*er         29*/
!                                                                               !/*er         30*/
! ERF(x) = 1.0 - ERFC(x) for -1.0 .le. x .le. 1.0                               !/*er         31*/
!                                                                               !/*er         32*/
      if (y.le.sqeps) erf_rep=dp2*x/sqrtpi                                      !/*er         33*/
      if (y.gt.sqeps) erf_rep=x*(dp1+csevl(dp2*x**2-dp1,erfcs,nterf))           !/*er         34*/
      return                                                                    !/*er         35*/
!                                                                               !/*er         36*/
! ERF(x) = 1.0 - ERFC(x) for  abs(x) .gt. 1.0                                   !/*er         37*/
!                                                                               !/*er         38*/
   10 if (y.le.xbig) erf_rep=sign(dp1-erfc_rep(y),x)                            !/*er         39*/
      if (y.gt.xbig) erf_rep=sign(dp1,x)                                        !/*er         40*/
!                                                                               !/*er         41*/
      return                                                                    !/*er         42*/
      end                                                                       !/*er         43*/

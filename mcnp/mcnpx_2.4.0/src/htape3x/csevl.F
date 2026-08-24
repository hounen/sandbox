!     $Id: csevl.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                        !/*cs          1*/
      function csevl (x,cs,n)                                                   !/*cs          2*/
#include <htape3x/com32b.h>                                                     /*!cs          3*/
!***begin prologue  csevl                                                       !/*cs          4*/
!***date written   770401   (yymmdd)                                            !/*cs          5*/
!***revision date  861211   (yymmdd)                                            !/*cs          6*/
!***category no.  c3a2                                                          !/*cs          7*/
!***keywords  library=slatec(fnlib),                                            !/*cs          8*/
!             type=single precision(csevl-s dcsevl-d),chebyshev,                !/*cs          9*/
!             special functions                                                 !/*cs         10*/
!***author  fullerton, w., (lanl)                                               !/*cs         11*/
!***purpose  evaluate the n-term chebyshev series cs at x.                      !/*cs         12*/
!***description                                                                 !/*cs         13*/
!                                                                               !/*cs         14*/
! evaluate the n-term chebyshev series cs at x.  adapted from                   !/*cs         15*/
! r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973). also see fox             !/*cs         16*/
! and parker, chebyshev polynomials in numerical analysis, oxford press,        !/*cs         17*/
! page 56.                                                                      !/*cs         18*/
!                                                                               !/*cs         19*/
!       input arguments --                                                      !/*cs         20*/
! x    value at which the series is to be evaluated.                            !/*cs         21*/
! cs   array of n terms of a chebyshev series.  in eval-                        !/*cs         22*/
!      uating cs, only half the first coefficient is summed.                    !/*cs         23*/
! n    number of terms in array cs.                                             !/*cs         24*/
!***references  (none)                                                          !/*cs         25*/
!***routines called  error                                                      !/*cs         26*/
!***end prologue  csevl                                                         !/*cs         27*/
!                                                                               !/*cs         28*/
      dimension cs(1)                                                           !/*cs         29*/
!***first executable statement  csevl                                           !/*cs         30*/
      if (n.lt.1) call errorf ('csevl.1 ')                                      !/*cs         31*/
      if (n.gt.1000) call errorf ('csevl.2 ')                                   !/*cs         32*/
      if (x.lt.-dp1.or.x.gt.dp1) call errorf ('csevl.3 ')                       !/*cs         33*/
!                                                                               !/*cs         34*/
      b1=dp0                                                                    !/*cs         35*/
      b0=dp0                                                                    !/*cs         36*/
      twox=dp2*x                                                                !/*cs         37*/
      do 10 i=1,n                                                               !/*cs         38*/
      b2=b1                                                                     !/*cs         39*/
      b1=b0                                                                     !/*cs         40*/
      ni=n+1-i                                                                  !/*cs         41*/
      b0=twox*b1-b2+cs(ni)                                                      !/*cs         42*/
   10 continue                                                                  !/*cs         43*/
!                                                                               !/*cs         44*/
      csevl=dph*(b0-b2)                                                         !/*cs         45*/
!                                                                               !/*cs         46*/
      return                                                                    !/*cs         47*/
      end                                                                       !/*cs         48*/

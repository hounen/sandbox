      subroutine lpoly2 (x, ple, n)                                             !/*lp          1*/
#ifdef CEM                                                                      /*!lp          2*/
! ======================================================================        !/*lp          3*/
!                                                                               !/*lp          4*/
!  Calculates the first n Legendre polynomials, those of order 0 to n-1.        !/*lp          5*/
!                                                                               !/*lp          6*/
!  Written by A. J. Sierk,  LANL T-2, February, 1994.                           !/*lp          7*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*lp          8*/
!                                                                               !/*lp          9*/
! ======================================================================        !/*lp         10*/
#include <cem/cdouble.h>                                                        /*!lp         11*/
      dimension ple(n)                                                          !/*lp         12*/
! ======================================================================        !/*lp         13*/
      ple(1) = 1.d0                                                             !/*lp         14*/
      ple(2) = x                                                                !/*lp         15*/
      ple(3) = 0.5d0*(3.d0*x*x - 1.d0)                                          !/*lp         16*/
      ple(4) = 0.5d0*x*(5.d0*x*x - 3.d0)                                        !/*lp         17*/
      if (n.ge.5) then                                                          !/*lp         18*/
        do 10 i = 5,n                                                           !/*lp         19*/
        f=i-1                                                                   !/*lp         20*/
        ple(i) = ((2*i-3)*x*ple(i-1)-(i-2)*ple(i-2))/f                          !/*lp         21*/
   10   continue                                                                !/*lp         22*/
      endif                                                                     !/*lp         23*/
#endif                                                                          /*!lp         24*/
      return                                                                    !/*lp         25*/
      end                                                                       !/*lp         26*/

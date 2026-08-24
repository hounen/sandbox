      function pmom (j, t)                                                      !/*pm          1*/
! ======================================================================        !/*pm          2*/
!                                                                               !/*pm          3*/
!     Calculation of secondary particle random momentum, weighted               !/*pm          4*/
!     by the angular distribution and the momentum distribution.                !/*pm          5*/
!                                                                               !/*pm          6*/
!   Called by: VMNSP                                                            !/*pm          7*/
!                                                                               !/*pm          8*/
!   CEM95 written by S. G. Mashnik                                              !/*pm          9*/
!                                                                               !/*pm         10*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*pm         11*/
!   Edited by AJS, july, 1997.                                                  !/*pm         12*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*pm         13*/
!                                                                               !/*pm         14*/
! ======================================================================        !/*pm         15*/
#include <cem/cdouble.h>                                                        /*!pm         16*/
#ifndef CEM                                                                     /*!pm         17*/
      pmom=dp0                                                                  !/*pm         18*/
#else                                                                           /*!pm         19*/
      dimension bnkj(4,4,8), ckj(3,8), bnk(4,4)                                 !/*pm         20*/
      common /coefbc/ bnkj, ckj                                                 !/*pm         21*/
! ======================================================================        !/*pm         22*/
        do 10 k = 1,4                                                           !/*pm         23*/
        do 10 n = 1,4                                                           !/*pm         24*/
        bnk(n,k) = bnkj(n,k,j)                                                  !/*pm         25*/
   10   continue                                                                !/*pm         26*/
      r1 = rang()                                                               !/*pm         27*/
      s1 = dp0                                                                  !/*pm         28*/
      s2 = dp0                                                                  !/*pm         29*/
      s3 = dp0                                                                  !/*pm         30*/
        do 20 n = 1,4                                                           !/*pm         31*/
        do 20 k = 1,4                                                           !/*pm         32*/
        s1 = s1 + bnk(n,k)*(t**(k-1))*(r1**(n-1))                               !/*pm         33*/
   20   continue                                                                !/*pm         34*/
        do 30 n = 1,4                                                           !/*pm         35*/
        do 30 k = 1,4                                                           !/*pm         36*/
        s2 = s2 + bnk(n,k)*t**(k-1)                                             !/*pm         37*/
   30   continue                                                                !/*pm         38*/
        do 40 k = 1,3                                                           !/*pm         39*/
        s3 = s3 + ckj(k,j)*t**(k-1)                                             !/*pm         40*/
   40   continue                                                                !/*pm         41*/
      pmom = s3*sqrt(r1)*(s1 + (dp1 - s2)*r1**4)                                !/*pm         42*/
#endif                                                                          /*!pm         43*/
      return                                                                    !/*pm         44*/
      end                                                                       !/*pm         45*/

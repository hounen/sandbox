      function costa (j, t)                                                     !/*co          1*/
! ======================================================================        !/*co          2*/
!                                                                               !/*co          3*/
!     Cosine calculation for elastic and charge-exchange reactions.             !/*co          4*/
!                                                                               !/*co          5*/
!   Called by: ABSORP COSEL COSEX DIRECT ISOBAR                                 !/*co          6*/
!                                                                               !/*co          7*/
!   CEM95 written by S. G. Mashnik                                              !/*co          8*/
!                                                                               !/*co          9*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*co         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*co         11*/
!                                                                               !/*co         12*/
! ======================================================================        !/*co         13*/
#include <cem/cdouble.h>                                                        /*!co         14*/
#ifndef CEM                                                                     /*!co         15*/
      costa=dp0                                                                 !/*co         16*/
#else                                                                           /*!co         17*/
      dimension ankj(4,4,29), ank(4,4)                                          !/*co         18*/
      common /coefa/ ankj                                                       !/*co         19*/
! ======================================================================        !/*co         20*/
        do 10 k = 1,4                                                           !/*co         21*/
        do 10 n = 1,4                                                           !/*co         22*/
        ank(n,k) = ankj(n,k,j)                                                  !/*co         23*/
   10   continue                                                                !/*co         24*/
      s1 = dp0                                                                  !/*co         25*/
      r1 = rang()                                                               !/*co         26*/
      s2 = dp0                                                                  !/*co         27*/
        do 20 n = 1,4                                                           !/*co         28*/
        do 20 k = 1,4                                                           !/*co         29*/
        term = ank(n,k)*t**(k-1)                                                !/*co         30*/
        s1 = s1 + term*(r1**(n-1))                                              !/*co         31*/
        s2 = s2 + term                                                          !/*co         32*/
   20   continue                                                                !/*co         33*/
      cta = dp2*sqrt(r1)*(s1 + (dp1 - s2)*r1**4) - dp1                          !/*co         34*/
      temp1 = abs(cta)                                                          !/*co         35*/
      if (temp1.le.dp1) then                                                    !/*co         36*/
        costa = cta                                                             !/*co         37*/
      else                                                                      !/*co         38*/
        costa = sign(dp1, cta)                                                  !/*co         39*/
      endif                                                                     !/*co         40*/
#endif                                                                          /*!co         41*/
      return                                                                    !/*co         42*/
      end                                                                       !/*co         43*/

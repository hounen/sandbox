      subroutine kinema (pstar, v, p, ct, st, cfi, sfi, t, cm)                  !/*ki          1*/
#ifdef CEM                                                                      /*!ki          2*/
! ======================================================================        !/*ki          3*/
!                                                                               !/*ki          4*/
!     Kinematics routine.                                                       !/*ki          5*/
!     Lorentz transformation of the momentum pstar in the center                !/*ki          6*/
!     of mass system to p in the laboratory system                              !/*ki          7*/
!                                                                               !/*ki          8*/
!   Called by: PAULIP PRECOF                                                    !/*ki          9*/
!                                                                               !/*ki         10*/
!   CEM95 written by S. G. Mashnik                                              !/*ki         11*/
!                                                                               !/*ki         12*/
!   Edited by A. J. Sierk,  LANL T-2, February, 1996.                           !/*ki         13*/
!   Edited by AJS, October, 1997.                                               !/*ki         14*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ki         15*/
!                                                                               !/*ki         16*/
! ======================================================================        !/*ki         17*/
#include <cem/cdouble.h>                                                        /*!ki         18*/
      dimension pstar(3), v(3), p(3)                                            !/*ki         19*/
! ======================================================================        !/*ki         20*/
      spv = pstar(1)*v(1) + pstar(2)*v(2) + pstar(3)*v(3)                       !/*ki         21*/
      v2 = v(1)**2 + v(2)**2 + v(3)**2                                          !/*ki         22*/
      temp1 = sqrt(dp1 - v2)                                                    !/*ki         23*/
      temp2 = spv*(dp1/temp1 - dp1)/v2                                          !/*ki         24*/
      pmstar = sqrt(pstar(1)**2 + pstar(2)**2 + pstar(3)**2)                    !/*ki         25*/
      temp3 = sqrt(pmstar**2 + cm**2)                                           !/*ki         26*/
      temp1 = temp3/temp1                                                       !/*ki         27*/
      p(1) = pstar(1) + v(1)*temp2 + v(1)*temp1                                 !/*ki         28*/
      p(2) = pstar(2) + v(2)*temp2 + v(2)*temp1                                 !/*ki         29*/
      p(3) = pstar(3) + v(3)*temp2 + v(3)*temp1                                 !/*ki         30*/
      pms = p(1)**2 + p(2)**2 + p(3)**2                                         !/*ki         31*/
      pm = sqrt(pms)                                                            !/*ki         32*/
      ct = p(3)/pm                                                              !/*ki         33*/
      temp4 = dp1 - ct**2                                                       !/*ki         34*/
      if (temp4.le.dp0) then                                                    !/*ki         35*/
        ct = dp1                                                                !/*ki         36*/
        cfi = dp1                                                               !/*ki         37*/
        sfi = dp0                                                               !/*ki         38*/
        st = dp0                                                                !/*ki         39*/
      else                                                                      !/*ki         40*/
        st = sqrt(temp4)                                                        !/*ki         41*/
        temp3 = pm*st                                                           !/*ki         42*/
        cfi = p(1)/temp3                                                        !/*ki         43*/
        sfi = p(2)/temp3                                                        !/*ki         44*/
      endif                                                                     !/*ki         45*/
      t = sqrt(pms + cm**2) - cm                                                !/*ki         46*/
#endif                                                                          /*!ki         47*/
      return                                                                    !/*ki         48*/
      end                                                                       !/*ki         49*/

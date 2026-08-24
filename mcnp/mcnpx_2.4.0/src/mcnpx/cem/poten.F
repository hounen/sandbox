      function poten (i, n, ipatin, tfp, tfn, af, vpi, eps)                     !/*po          1*/
! ======================================================================        !/*po          2*/
!                                                                               !/*po          3*/
!     Calculation of particle potential in  nuclear zone number i.              !/*po          4*/
!                                                                               !/*po          5*/
!   Called by: CASCAD PINPN REFRAC                                              !/*po          6*/
!                                                                               !/*po          7*/
!   CEM95 written by S. G. Mashnik                                              !/*po          8*/
!                                                                               !/*po          9*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*po         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*po         11*/
!                                                                               !/*po         12*/
! ======================================================================        !/*po         13*/
!                                                                               !/*po         14*/
!  Definition of ipatin:                                                        !/*po         15*/
!                       ipatin(1); charge of particle                           !/*po         16*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*po         17*/
!                       ipatin(3); strangeness of particle                      !/*po         18*/
!                       ipatin(4); particle baryon number                       !/*po         19*/
!                       ipatin(5); zone number of nucleus where particle        !/*po         20*/
!                                  is located.                                  !/*po         21*/
!                                                                               !/*po         22*/
! ======================================================================        !/*po         23*/
#include <cem/cdouble.h>                                                        /*!po         24*/
#ifndef CEM                                                                     /*!po         25*/
      poten=dp0                                                                 !/*po         26*/
#else                                                                           /*!po         27*/
      dimension ipatin(5), tfp(10), tfn(10), af(10)                             !/*po         28*/
! ======================================================================        !/*po         29*/
      if (i-n.ge.2) then                                                        !/*po         30*/
        poten = dp0                                                             !/*po         31*/
      else                                                                      !/*po         32*/
        temp1 = ipatin(1)                                                       !/*po         33*/
        if (i-n.eq.1) then                                                      !/*po         34*/
!  i = n+1; nuclear part of potential energy vanishes                           !/*po         35*/
          poten = temp1*af(n+1)                                                 !/*po         36*/
        else                                                                    !/*po         37*/
          if (ipatin(3).ne.0) then                                              !/*po         38*/
!  Coulomb energy only                                                          !/*po         39*/
            poten = temp1*af(i)                                                 !/*po         40*/
          else                                                                  !/*po         41*/
            if (ipatin(4).eq.0) then                                            !/*po         42*/
!  Meson (pion binding + Coulomb energy):                                       !/*po         43*/
              temp2 = ipatin(2) - 1                                             !/*po         44*/
              poten = temp1*af(i) - vpi*temp2                                   !/*po         45*/
            else                                                                !/*po         46*/
!  Baryon (Coulomb + average particle sep. eng. + Fermi energy)                 !/*po         47*/
              temp3 = ipatin(4)                                                 !/*po         48*/
              poten = temp1*af(i) + tfp(i)*temp1 +                              !/*po         49*/
     &                (dp1 - temp1)*tfn(i) + temp3*eps                          !/*po         50*/
            endif                                                               !/*po         51*/
          endif                                                                 !/*po         52*/
        endif                                                                   !/*po         53*/
      endif                                                                     !/*po         54*/
#endif                                                                          /*!po         55*/
      return                                                                    !/*po         56*/
      end                                                                       !/*po         57*/

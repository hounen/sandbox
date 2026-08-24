      subroutine partn (partin, ipatin, partne, ipatne)                         !/*pa          1*/
#ifdef CEM                                                                      /*!pa          2*/
! ======================================================================        !/*pa          3*/
!                                                                               !/*pa          4*/
!     Partner selection.                                                        !/*pa          5*/
!     partin and ipatin refer to the cascade particle;                          !/*pa          6*/
!     partne and ipatne refer to the partner nuclear particle which is          !/*pa          7*/
!        potentially interacting with the cascade particle.                     !/*pa          8*/
!                                                                               !/*pa          9*/
!   Called by: ABSORP POINTE                                                    !/*pa         10*/
!                                                                               !/*pa         11*/
!   CEM95 written by S. G. Mashnik                                              !/*pa         12*/
!                                                                               !/*pa         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*pa         14*/
!   Edited by AJS, August, 1997.                                                !/*pa         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*pa         16*/
!                                                                               !/*pa         17*/
! ======================================================================        !/*pa         18*/
!                                                                               !/*pa         19*/
!  Definition of partin:                                                        !/*pa         20*/
!                       partin(1); Normalized x coordinate of particle          !/*pa         21*/
!                       partin(2); Normalized y coordinate of particle          !/*pa         22*/
!                       partin(3); Normalized z coordinate of particle          !/*pa         23*/
!                                                                               !/*pa         24*/
!  Definition of ipatin:                                                        !/*pa         25*/
!                       ipatin(1); charge of particle                           !/*pa         26*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*pa         27*/
!                       ipatin(3); strangeness of particle; always 0!           !/*pa         28*/
!                       ipatin(4); particle baryon number                       !/*pa         29*/
!                       ipatin(5); zone number of nucleus where particle        !/*pa         30*/
!                                  is located.                                  !/*pa         31*/
!                                                                               !/*pa         32*/
!  partne, ipatne are corresponding characteristics of the partner.             !/*pa         33*/
!                                                                               !/*pa         34*/
! ======================================================================        !/*pa         35*/
#include <cem/cdouble.h>                                                        /*!pa         36*/
      dimension tfp(10), tfn(10), partin(9), ipatin(5), partne(9),              !/*pa         37*/
     &          ipatne(5), rhop(10), rhon(10), rsm(10), rbig(10), af(10)        !/*pa         38*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*pa         39*/
      common /mnuc/    emnucm, emnucg, emnucb, emneut, emprot                   !/*pa         40*/
      common /o/       anucl, znucl, eps, vpi, rsm, rbig, rhop,                 !/*pa         41*/
     &                 rhon, af, tfp, tfn, n                                    !/*pa         42*/
      common /pii/     pi                                                       !/*pa         43*/
! ======================================================================        !/*pa         44*/
      twpi = dp2*pi                                                             !/*pa         45*/
      j1 = ipatin(5)                                                            !/*pa         46*/
      bpartn = (anucl - znucl)/anucl                                            !/*pa         47*/
      beta1 = rang()                                                            !/*pa         48*/
      beta2 = rang()**twthrd                                                    !/*pa         49*/
!   Choose whether interacting particle is a neutron or a proton:               !/*pa         50*/
      if (beta1.ge.bpartn) then                                                 !/*pa         51*/
!   Proton                                                                      !/*pa         52*/
        ipatne(1) = 1                                                           !/*pa         53*/
!   Kinetic energy randomly chosen from Fermi sea.                              !/*pa         54*/
        tnjr = tfp(j1)*beta2                                                    !/*pa         55*/
      else                                                                      !/*pa         56*/
!  Neutron                                                                      !/*pa         57*/
        ipatne(1) = 0                                                           !/*pa         58*/
!   Kinetic energy randomly chosen from Fermi sea.                              !/*pa         59*/
        tnjr = tfn(j1)*beta2                                                    !/*pa         60*/
      endif                                                                     !/*pa         61*/
!   The partner has a randomly selected direction of motion                     !/*pa         62*/
      b1 = rang()                                                               !/*pa         63*/
      partne(5) = dp1 - dp2*b1                                                  !/*pa         64*/
      b2 = rang()                                                               !/*pa         65*/
      phin = twpi*b2                                                            !/*pa         66*/
      partne(4) = sqrt(dp1 - partne(5)**2)                                      !/*pa         67*/
      partne(7) = cos(phin)                                                     !/*pa         68*/
      partne(6) = sin(phin)                                                     !/*pa         69*/
      partne(1) = partin(1)                                                     !/*pa         70*/
      partne(2) = partin(2)                                                     !/*pa         71*/
      partne(3) = partin(3)                                                     !/*pa         72*/
      partne(9) = emprot*ipatne(1) + emneut*(dp1-ipatne(1))                     !/*pa         73*/
      partne(8) = tnjr                                                          !/*pa         74*/
      ipatne(5) = ipatin(5)                                                     !/*pa         75*/
      ipatne(2) = 0                                                             !/*pa         76*/
      ipatne(3) = 0                                                             !/*pa         77*/
      ipatne(4) = 1                                                             !/*pa         78*/
#endif                                                                          /*!pa         79*/
      return                                                                    !/*pa         80*/
      end                                                                       !/*pa         81*/

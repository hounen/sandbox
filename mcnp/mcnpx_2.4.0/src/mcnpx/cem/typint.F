      subroutine typint (partin, ipatin, partne, ipatne, v, u, tin1,            !/*ty          1*/
     &                   sigp, sign, sigabs, mv, np, z, a)                      !/*ty          2*/
#ifdef CEM                                                                      /*!ty          3*/
! ======================================================================        !/*ty          4*/
!                                                                               !/*ty          5*/
!     Determine interaction type and calculate                                  !/*ty          6*/
!     secondary particle characteristics.                                       !/*ty          7*/
!                                                                               !/*ty          8*/
!   Called by: CASCAD                                                           !/*ty          9*/
!                                                                               !/*ty         10*/
!   Calls: ABSORP BINEL ELEX SIGMAT SLQEK                                       !/*ty         11*/
!                                                                               !/*ty         12*/
!   CEM95 written by S. G. Mashnik                                              !/*ty         13*/
!                                                                               !/*ty         14*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*ty         15*/
!   Edited by AJS, July-August, 1997.                                           !/*ty         16*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ty         17*/
!                                                                               !/*ty         18*/
! ======================================================================        !/*ty         19*/
!                                                                               !/*ty         20*/
!  Definition of ipatin (ipatne similar for 2nd particle):                      !/*ty         21*/
!                      ipatin(1); charge of particle                            !/*ty         22*/
!                      ipatin(2); (Always 0 in CEM97a![used for gammas])        !/*ty         23*/
!                      ipatin(3); strangeness of particle                       !/*ty         24*/
!                      ipatin(4); particle baryon number                        !/*ty         25*/
!                      ipatin(5); nuclear zone number                           !/*ty         26*/
!                                                                               !/*ty         27*/
! ======================================================================        !/*ty         28*/
#include <cem/cdouble.h>                                                        /*!ty         29*/
      dimension partin(9), ipatin(5), partne(9), ipatne(5), v(3)                !/*ty         30*/
      common /o/ anucl, znucl, eps, vpi, rsm(10), rbig(10),                     !/*ty         31*/
     &           rhop(10), rhon(10), af(10), tfp(10), tfn(10), n                !/*ty         32*/
! ======================================================================        !/*ty         33*/
   10 mtemp = ipatin(5)                                                         !/*ty         34*/
!   Betabs will be zero unless cascade particle is a pion.                      !/*ty         35*/
      betabs = rhop(mtemp)*sigabs/(rhop(mtemp)*sigp +                           !/*ty         36*/
     &         rhon(mtemp)*sign + rhop(mtemp)*sigabs)                           !/*ty         37*/
      b1 = rang()                                                               !/*ty         38*/
      if (b1.le.betabs) then                                                    !/*ty         39*/
        call absorp (partin, ipatin, partne, ipatne, mv, np, anucl,             !/*ty         40*/
     &               znucl, v)                                                  !/*ty         41*/
      else                                                                      !/*ty         42*/
        call slqek (l, ms, mb, ksi, me, ipatin(2), ipatin(3), ipatin(4),        !/*ty         43*/
     &            ipatin(1), ipatne(2), ipatne(3), ipatne(4), ipatne(1))        !/*ty         44*/
!  Ratio of sum of elastic cross section and (SCX for pion channel)             !/*ty         45*/
!                            to total                                           !/*ty         46*/
        betael = (sigmat (l, ms, mb, ksi, 1, tin1) +                            !/*ty         47*/
     &            sigmat (l, ms, mb, ksi, 2, tin1))/                            !/*ty         48*/
     &            sigmat (l, ms, mb, ksi, 0, tin1)                              !/*ty         49*/
        b1 = rang()                                                             !/*ty         50*/
        if (b1.le.betael) then                                                  !/*ty         51*/
!   Elastic and (for pion-N channel) SCX:                                       !/*ty         52*/
          call elex (v, u, tin1, partin, ipatin, partne, ipatne, mv, np,        !/*ty         53*/
     &               l, ms, mb, ksi, me)                                        !/*ty         54*/
        else                                                                    !/*ty         55*/
!   Inelastic reaction of some type (pion production):                          !/*ty         56*/
          call binel (partin, ipatin, partne, ipatne, l, ms, mb, ksi,           !/*ty         57*/
     &                me, v, u, tin1, mv, np, nin, znucl, anucl)                !/*ty         58*/
          if (nin.ne.0) go to 10                                                !/*ty         59*/
        endif                                                                   !/*ty         60*/
      endif                                                                     !/*ty         61*/
#endif                                                                          /*!ty         62*/
      return                                                                    !/*ty         63*/
      end                                                                       !/*ty         64*/

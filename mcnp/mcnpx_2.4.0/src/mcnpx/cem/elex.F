      subroutine elex(v, u, tin1, partin, ipatin, partne, ipatne,               !/*el          1*/
     &                mv, np, l, ms, mb, ksi, me)                               !/*el          2*/
#ifdef CEM                                                                      /*!el          3*/
! ======================================================================        !/*el          4*/
!                                                                               !/*el          5*/
!     Calculation of particle characteristics in elastic and                    !/*el          6*/
!     single charge exchange scattering.                                        !/*el          7*/
!                                                                               !/*el          8*/
!   Called by: TYPINT                                                           !/*el          9*/
!                                                                               !/*el         10*/
!   Calls: ABEL COSEL COSEX SIGMAT                                              !/*el         11*/
!                                                                               !/*el         12*/
!   CEM95 written by S. G. Mashnik                                              !/*el         13*/
!                                                                               !/*el         14*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*el         15*/
!   Edited by A. J. Sierk  LANL  T-2  December, 1997.                           !/*el         16*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*el         17*/
!                                                                               !/*el         18*/
! ======================================================================        !/*el         19*/
!                                                                               !/*el         20*/
!  np is the number of particles in the final state (always = 2, unless         !/*el         21*/
!  storage block "memory" is overfilled).                                       !/*el         22*/
!  mv is the index of the last storage cell in /memory/ filled prior to         !/*el         23*/
!  the call to ELEX.                                                            !/*el         24*/
!                                                                               !/*el         25*/
!  Definition of partin (partne similar for partner particle):                  !/*el         26*/
!                       partin(1); x coordinate of particle                     !/*el         27*/
!                       partin(2); y coordinate of particle                     !/*el         28*/
!                       partin(3); z coordinate of particle                     !/*el         29*/
!                       partin(4); sin(theta), direction of momentum            !/*el         30*/
!                       partin(5); cos(theta), direction of momentum            !/*el         31*/
!                       partin(6); sin(phi), direction of momentum              !/*el         32*/
!                       partin(7); cos(phi), direction of momentum              !/*el         33*/
!                       partin(8); kinetic energy of particle                   !/*el         34*/
!                       partin(9); rest mass of particle                        !/*el         35*/
!                                                                               !/*el         36*/
!  Definition of ipatin (ipatne similar for partner particle):                  !/*el         37*/
!                       ipatin(1); charge of particle                           !/*el         38*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*el         39*/
!                       ipatin(3); strangeness of particle                      !/*el         40*/
!                       ipatin(4); particle baryon number                       !/*el         41*/
!                       ipatin(5); zone number of nucleus where particle        !/*el         42*/
!                                  is located.                                  !/*el         43*/
!                                                                               !/*el         44*/
! ======================================================================        !/*el         45*/
#include <cem/cdouble.h>                                                        /*!el         46*/
      dimension pmemo(9,150), imemo(5,150), v(3), partin(9), ipatin(5),         !/*el         47*/
     &          partne(9), ipatne(5), pist(3), pnst(3)                          !/*el         48*/
      common /memory/ pmemo, imemo                                              !/*el         49*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*el         50*/
      common /mpi/    empich, empi0                                             !/*el         51*/
      common /pii/    pi                                                        !/*el         52*/
! ======================================================================        !/*el         53*/
      cmi = partin(9)                                                           !/*el         54*/
      cmn = partne(9)                                                           !/*el         55*/
!   Ratio of charge exchange to (elastic + SCX)                                 !/*el         56*/
!   (Will be zero for N-N channel, pi+ + p; pi- + n):                           !/*el         57*/
      betaex = sigmat (l, ms, mb, ksi, 2, tin1)/                                !/*el         58*/
     &        (sigmat (l, ms, mb, ksi, 1, tin1) +                               !/*el         59*/
     &         sigmat (l, ms, mb, ksi, 2, tin1))                                !/*el         60*/
      b1 = rang()                                                               !/*el         61*/
      if (b1.ge.betaex) then                                                    !/*el         62*/
!  Elastic scattering:                                                          !/*el         63*/
        ie = ipatin(1)                                                          !/*el         64*/
        ne = ipatne(1)                                                          !/*el         65*/
        ctsti = cosel (l, mb, ksi, tin1, partin(9))                             !/*el         66*/
      else                                                                      !/*el         67*/
!  Charge exchange scattering:                                                  !/*el         68*/
        if (ipatin(1).ne.0) then                                                !/*el         69*/
!   Charged pion --> neutral pion:                                              !/*el         70*/
          ie = 0                                                                !/*el         71*/
!   Charge on outgoing nucleon = total charge in system:                        !/*el         72*/
          ne = me                                                               !/*el         73*/
        else                                                                    !/*el         74*/
!   Neutral pion --> charged pion:                                              !/*el         75*/
!   p --> n; or n --> p:                                                        !/*el         76*/
          ne = 1 - ipatne(1)                                                    !/*el         77*/
          ie = me - ne                                                          !/*el         78*/
        endif                                                                   !/*el         79*/
        ctsti = cosex (l, tin1, partin(9))                                      !/*el         80*/
        cmi = empi0*(dp1 - abs(ie)) + empich*abs(ie)                            !/*el         81*/
        cmn = emneut*(dp1 - ne) + emprot*ne                                     !/*el         82*/
        if (u.le.cmi+cmn) then                                                  !/*ela         1*/
          cmi = partin(9)                                                       !/*ela         2*/
          cmn = partne(9)                                                       !/*ela         3*/
          ie = ipatin(1)                                                        !/*ela         4*/
          ne = ipatne(1)                                                        !/*ela         5*/
          ctsti = cosel (l, mb, ksi, tin1, partin(9))                           !/*ela         6*/
        endif                                                                   !/*ela         7*/
      endif                                                                     !/*el         83*/
!   Both options continue from here:                                            !/*el         84*/
      b2 = rang()                                                               !/*el         85*/
      fisti = dp2*pi*b2                                                         !/*el         86*/
      call abel (partin, v, u, pist, pnst, ctsti, fisti, cmi, cmn)              !/*el         87*/
      if (mv.gt.147) then                                                       !/*el         88*/
        np = 0                                                                  !/*el         89*/
        call expirx(1,'ELEX','memory exceeded in cascade.')                     !/*el         90*/
      else                                                                      !/*el         91*/
        pmemo(1,mv+3) = pist(1)                                                 !/*el         92*/
        pmemo(2,mv+3) = pist(2)                                                 !/*el         93*/
        pmemo(3,mv+3) = pist(3)                                                 !/*el         94*/
        pmemo(9,mv+3) = cmi                                                     !/*el         95*/
        imemo(1,mv+3) = ie                                                      !/*el         96*/
        imemo(2,mv+3) = 0                                                       !/*el         97*/
        imemo(3,mv+3) = ipatin(3)                                               !/*el         98*/
        imemo(4,mv+3) = ipatin(4)                                               !/*el         99*/
        pmemo(1,mv+1) = pnst(1)                                                 !/*el        100*/
        pmemo(2,mv+1) = pnst(2)                                                 !/*el        101*/
        pmemo(3,mv+1) = pnst(3)                                                 !/*el        102*/
        pmemo(9,mv+1) = cmn                                                     !/*el        103*/
        imemo(1,mv+1) = ne                                                      !/*el        104*/
        imemo(2,mv+1) = 0                                                       !/*el        105*/
        imemo(3,mv+1) = 0                                                       !/*el        106*/
        imemo(4,mv+1) = 1                                                       !/*el        107*/
        np = 2                                                                  !/*el        108*/
      endif                                                                     !/*el        109*/
#endif                                                                          /*!el        110*/
      return                                                                    !/*el        111*/
      end                                                                       !/*el        112*/

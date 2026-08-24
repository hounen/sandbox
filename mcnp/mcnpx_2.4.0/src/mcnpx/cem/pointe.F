      subroutine pointe (partin, ipatin, partne, ipatne, nabs, nout,            !/*po          1*/
     &                   irefl, v, u, tin1, sigp, sign, sigabs, t0gev)          !/*po          2*/
#ifdef CEM                                                                      /*!po          3*/
! ======================================================================        !/*po          4*/
!                                                                               !/*po          5*/
!     Determination of interaction point inside nucleus.                        !/*po          6*/
!                                                                               !/*po          7*/
!     Called by: CASCAD                                                         !/*po          8*/
!                                                                               !/*po          9*/
!     Calls: ABSORP GEOM PARTN REFRAC SIGMAT SLQEK TINVU                        !/*po         10*/
!                                                                               !/*po         11*/
!   CEM95 written by S. G. Mashnik                                              !/*po         12*/
!                                                                               !/*po         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*po         14*/
!   Edited by AJS, July-August, 1997                                            !/*po         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*po         16*/
!                                                                               !/*po         17*/
! ======================================================================        !/*po         18*/
!                                                                               !/*po         19*/
!  nout = 1 when cascade particle is exiting the nucleus.                       !/*po         20*/
!                                                                               !/*po         21*/
!  Definition of partin:                                                        !/*po         22*/
!                       partin(1); Normalized x coordinate of particle          !/*po         23*/
!                       partin(2); Normalized y coordinate of particle          !/*po         24*/
!                       partin(3); Normalized z coordinate of particle          !/*po         25*/
!                       partin(4); sin(theta), direction of momentum            !/*po         26*/
!                       partin(5); cos(theta), direction of momentum            !/*po         27*/
!                       partin(6); sin(phi), direction of momentum              !/*po         28*/
!                       partin(7); cos(phi), direction of momentum              !/*po         29*/
!                       partin(8); kinetic energy of particle                   !/*po         30*/
!                       partin(9); rest mass of particle                        !/*po         31*/
!                                                                               !/*po         32*/
!  Definition of ipatin:                                                        !/*po         33*/
!                       ipatin(5); zone number of nucleus where particle        !/*po         34*/
!                                  is located.                                  !/*po         35*/
!                                                                               !/*po         36*/
! ======================================================================        !/*po         37*/
#include <cem/cdouble.h>                                                        /*!po         38*/
      dimension partin(9), ipatin(5), partne(9), ipatne(5), v(3)                !/*po         39*/
      common /nuctyp/ aa, zz                                                    !/*po         40*/
      common /o/      anucl, znucl, eps, vpi, rsm(10), rbig(10),                !/*po         41*/
     &                rhop(10), rhon(10), af(10), tfp(10), tfn(10), n           !/*po         42*/
! ======================================================================        !/*po         43*/
      sk = dp3                                                                  !/*po         44*/
      temp1 = log(rang())                                                       !/*po         45*/
      ig = 0                                                                    !/*po         46*/
!   s is the distance along the current trajectory to the first                 !/*po         47*/
!   intersection of the trajectory with a boundary of the current               !/*po         48*/
!   nuclear zone, ipatin(5).                                                    !/*po         49*/
   10 s = geom (rbig, partin, ipatin)                                           !/*po         50*/
   20 continue                                                                  !/*po         51*/
      ig = ig + 1                                                               !/*po         52*/
      if (ig.gt.50 .and. ipatin(4).gt.0) then                                   !/*po         53*/
!   Absorb nucleon if it has been here for too long.                            !/*po         54*/
        nabs = 1                                                                !/*po         55*/
        return                                                                  !/*po         56*/
!       else                                                                    !/*po         57*/
!   If pion has repeated too much, restart the cascade.                         !/*po         58*/
!         partin(8) = dp1m6                                                     !/*po         59*/
!       endif                                                                   !/*po         60*/
      endif                                                                     !/*po         61*/
!  Randomly select a partner for interaction by isospin and Fermi motion        !/*po         62*/
      call partn (partin, ipatin, partne, ipatne)                               !/*po         63*/
!  Find tin1, the kinetic energy of the cascade particle in the frame           !/*po         64*/
!  in which the partner is at rest. v is the velocity of the center of          !/*po         65*/
!  mass of the 2 interacting particles with respect to the nuclear              !/*po         66*/
!  rest (lab) frame. U is the total energy in the 2-particle C-M frame.         !/*po         67*/
      call tinvu (partin(8), partin(9), partne(8), partne(9), partin(4),        !/*po         68*/
     &            partin(5), partin(6), partin(7), partne(4), partne(5),        !/*po         69*/
     &            partne(6), partne(7), v, u, tin1)                             !/*po         70*/
!  Determine the index ksin for cascade particle interacting with a             !/*po         71*/
!  neutron.                                                                     !/*po         72*/
      call slqek (l, ms, mb, ksin, me, ipatin(2), ipatin(3), ipatin(4),         !/*po         73*/
     &            ipatin(1), ipatne(2), ipatne(3), ipatne(4), 0)                !/*po         74*/
!  Find the total cross section of the cascade particle (pi+, pi0, pi-,         !/*po         75*/
!  p, or n) with a neutron in the nucleus.                                      !/*po         76*/
      sign = sigmat (l, ms, mb, ksin, 0, tin1)                                  !/*po         77*/
!  Determine the index ksip for cascade particle interacting with a             !/*po         78*/
!  proton.                                                                      !/*po         79*/
      call slqek (l, ms, mb, ksip, me, ipatin(2), ipatin(3), ipatin(4),         !/*po         80*/
     &            ipatin(1), ipatne(2), ipatne(3), ipatne(4), 1)                !/*po         81*/
!  Find the total cross section of the cascade particle (pi+, pi0, pi-,         !/*po         82*/
!  p, or n) with a proton in the nucleus.                                       !/*po         83*/
      sigp = sigmat (l, ms, mb, ksip, 0, tin1)                                  !/*po         84*/
!  Find absorption cross section of cascade particle on two nucleons.           !/*po         85*/
!  Only nonzero for pions!                                                      !/*po         86*/
      sigabs = sigmat (l, ms, mb, ksip, 3, partin(8))                           !/*po         87*/
      itemp = ipatin(5)                                                         !/*po         88*/
!  Particle mean free path (in units of rsm(n); sigma in microbarns;            !/*po         89*/
!  rho in fm^-3).                                                               !/*po         90*/
      plambi = dp10/((rhon(itemp)*sign + rhop(itemp)*sigp +                     !/*po         91*/
     &                rhop(itemp)*sigabs)*rsm(n))                               !/*po         92*/
!  -plambi*temp1 is the random distance [in units of rsm(n)] that               !/*po         93*/
!  the particle will traverse before its next interaction.                      !/*po         94*/
      delx = -plambi*temp1                                                      !/*po         95*/
      if (delx .gt. dp6*rhon(1)/rhon(n)) then                                   !/*po         96*/
!  The interaction distance is much greater than the nuclear size:              !/*po         97*/
        partin(8) = t0gev                                                       !/*po         98*/
        nout = 1                                                                !/*po         99*/
      else                                                                      !/*po        100*/
        temp2 = sk                                                              !/*po        101*/
        deltsi = plambi/temp2                                                   !/*po        102*/
        temp5 = min(s, deltsi)                                                  !/*po        103*/
        piks = temp1 + temp5/plambi                                             !/*po        104*/
        if (piks.gt.dp0) then                                                   !/*po        105*/
!   Interaction point is before intersection with zone boundary or              !/*po        106*/
!   lambda/3.  Advance particle along trajectory; interaction will              !/*po        107*/
!   occur at this point; still in the same zone. Then return to CASCAD.         !/*po        108*/
            partin(1) = partin(1) + delx*partin(4)*partin(7)                    !/*po        109*/
            partin(2) = partin(2) + delx*partin(4)*partin(6)                    !/*po        110*/
            partin(3) = partin(3) + delx*partin(5)                              !/*po        111*/
            partne(1) = partin(1)                                               !/*po        112*/
            partne(2) = partin(2)                                               !/*po        113*/
            partne(3) = partin(3)                                               !/*po        114*/
        else                                                                    !/*po        115*/
!   Projected interaction point is either beyond the boundary of                !/*po        116*/
!   the nuclear zone, or more than lambda/3.                                    !/*po        117*/
!   Magnitude of temp1 is reduced; as is s.                                     !/*po        118*/
          temp1 = piks                                                          !/*po        119*/
          s = s - temp5                                                         !/*po        120*/
!   Advance particle along trajectory either to intersection with               !/*po        121*/
!   zone boundary, or a distance of lambda/3.                                   !/*po        122*/
          partin(1) = partin(1) + temp5*partin(4)*partin(7)                     !/*po        123*/
          partin(2) = partin(2) + temp5*partin(4)*partin(6)                     !/*po        124*/
          partin(3) = partin(3) + temp5*partin(5)                               !/*po        125*/
!   If boundary not reached, go back to 20 with new value of temp1;             !/*po        126*/
!   distance to the boundary  i  is reduced by the amount moved.                !/*po        127*/
          if (s.ne.dp0) go to 20                                                !/*po        128*/
!   Particle moves to boundary of zone.                                         !/*po        129*/
          call refrac (partin, ipatin, nabs, nout, irefl)                       !/*po        130*/
          if (partin(8).le.dp1m5) then                                          !/*po        131*/
            if (ipatin(4).eq.1) then                                            !/*po        132*/
              nabs = 1                                                          !/*po        133*/
              return                                                            !/*po        134*/
            endif                                                               !/*po        135*/
          endif                                                                 !/*po        136*/
          if (ipatin(5).lt.n+1) then                                            !/*po        137*/
!   Particle is not outside nucleus.                                            !/*po        138*/
!   Nabs should always be zero!!                                                !/*po        139*/
!   Continue trajectory in new zone unless particle is absorbed.                !/*po        140*/
            if (nabs.le.0) go to 10                                             !/*po        141*/
          else                                                                  !/*po        142*/
!   Particle is in zone n+1; will exit nucleus at rsm(n+1) = rmax               !/*po        143*/
            temp3 = geom (rbig, partin, ipatin)                                 !/*po        144*/
            partin(1) = partin(1) + temp3*partin(4)*partin(7)                   !/*po        145*/
            partin(2) = partin(2) + temp3*partin(4)*partin(6)                   !/*po        146*/
            partin(3) = partin(3) + temp3*partin(5)                             !/*po        147*/
            call refrac (partin, ipatin, nabs, nout, irefl)                     !/*po        148*/
            nout = 1                                                            !/*po        149*/
          endif                                                                 !/*po        150*/
        endif                                                                   !/*po        151*/
      endif                                                                     !/*po        152*/
#endif                                                                          /*!po        153*/
      return                                                                    !/*po        154*/
      end                                                                       !/*po        155*/

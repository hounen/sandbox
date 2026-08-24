      subroutine abel (partin, v, u, pist, pnst, cti, fii, cmi, cmn)            !/*ab          1*/
#ifdef CEM                                                                      /*!ab          2*/
! ======================================================================        !/*ab          3*/
!                                                                               !/*ab          4*/
!     Calculates momenta of secondary particles                                 !/*ab          5*/
!     in center-of-mass system for absorption (called from ABSORP) and          !/*ab          6*/
!     elastic or pion charge-exchange scattering (called from ELEX).            !/*ab          7*/
!                                                                               !/*ab          8*/
!   Called by: ABSORP ELEX                                                      !/*ab          9*/
!                                                                               !/*ab         10*/
!   Calls: ROTOR                                                                !/*ab         11*/
!                                                                               !/*ab         12*/
!   CEM95 written by S. G. Mashnik                                              !/*ab         13*/
!                                                                               !/*ab         14*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*ab         15*/
!      and by F. Gallmeier, ORNL CPED, October 1998.                            !/*ab         16*/
!                                                                               !/*ab         17*/
! ======================================================================        !/*ab         18*/
!                                                                               !/*ab         19*/
!  v is the velocity of the center-of-momentum frame in the lab frame.          !/*ab         20*/
!     pi + 2N system for absorption; 2N system for elastic scattering;          !/*ab         21*/
!     pi + N system for SCX scattering.                                         !/*ab         22*/
!  u is the TOTAL energy available in the center-of-momentum frame.             !/*ab         23*/
!  cti is random value of cos(theta); fii is random value of phi.               !/*ab         24*/
!                                                                               !/*ab         25*/
!  partin refers to the incident pion for pion absorption.                      !/*ab         26*/
!                                                                               !/*ab         27*/
!  Definition of partin:                                                        !/*ab         28*/
!                       partin(1); x coordinate of particle                     !/*ab         29*/
!                       partin(2); y coordinate of particle                     !/*ab         30*/
!                       partin(3); z coordinate of particle                     !/*ab         31*/
!                       partin(4); sin(theta), direction of momentum            !/*ab         32*/
!                       partin(5); cos(theta), direction of momentum            !/*ab         33*/
!                       partin(6); sin(phi), direction of momentum              !/*ab         34*/
!                       partin(7); cos(phi), direction of momentum              !/*ab         35*/
!                       partin(8); (Lab) kinetic energy of particle             !/*ab         36*/
!                       partin(9); rest mass of particle                        !/*ab         37*/
!                                                                               !/*ab         38*/
! ======================================================================        !/*ab         39*/
#include <cem/cdouble.h>                                                        /*!ab         40*/
      dimension v(3), pi(3), pist(3), pnst(3), partin(9), pins(3)               !/*ab         41*/
! ======================================================================        !/*ab         42*/
      ei = (u**2 + cmi**2 - cmn**2)/(dp2*u)                                     !/*ab         43*/
!   |momentum| of each of the final state particles in the                      !/*ab         44*/
!     center of mass system:                                                    !/*ab         45*/
      pim = dp0                                                                 !/*aba         1*/
      if (ei.gt.cmi) pim = sqrt(ei**2 - cmi**2)                                 !/*aba         2*/
      v2 = v(1)**2 + v(2)**2 + v(3)**2                                          !/*ab         47*/
!   temp3 = cascade particle initial momentum in the lab frame.                 !/*ab         48*/
      temp3 = sqrt(partin(8)*(partin(8) + dp2*partin(9)))                       !/*ab         49*/
!   temp4 = transverse component of the cascade particle momentum in            !/*ab         50*/
!           the lab frame.                                                      !/*ab         51*/
      temp4 = temp3*partin(4)                                                   !/*ab         52*/
!   Dot product of the lab velocity of CM with the lab momentum of              !/*ab         53*/
!   cascade particle.                                                           !/*ab         54*/
      spv = temp4*partin(7)*v(1) + temp3*partin(5)*v(3) +                       !/*ab         55*/
     &      temp4*partin(6)*v(2)                                                !/*ab         56*/
!   fac = gamma (of CM frame with respect to lab!)                              !/*ab         57*/
      fac = dp1/sqrt(dp1 - v2)                                                  !/*ab         58*/
!   Temp1 = p (dot) v * (gamma - 1)/v**2                                        !/*ab         59*/
      temp1 = spv*(fac - dp1)/v2                                                !/*ab         60*/
!   Temp2 = Ein*gamma                                                           !/*ab         61*/
      temp2 = (partin(8) + partin(9))*fac                                       !/*ab         62*/
      fac2 = temp1 - temp2                                                      !/*ab         63*/
!   pins is the momentum vector of the particle transformed into the            !/*ab         64*/
!   CM system. (See Jackson, 2nd ed., p541 for Lor. trans. matrix)              !/*ab         65*/
      pins(1) = temp4*partin(7) + v(1)*fac2                                     !/*ab         66*/
      pins(2) = temp4*partin(6) + v(2)*fac2                                     !/*ab         67*/
      pins(3) = temp3*partin(5) + v(3)*fac2                                     !/*ab         68*/
      sti = sqrt(dp1 - cti**2)                                                  !/*ab         69*/
      cfi = cos(fii)                                                            !/*ab         70*/
      sfi = sin(fii)                                                            !/*ab         71*/
!   pins and v define a coordinate system in the CM frame,                      !/*ab         72*/
!   which is rotated w.r.t. the lab coordinate system.                          !/*ab         73*/
!   pi is (one) final state particle momentum (with random direction)           !/*ab         74*/
!   in this rotated CM frame; CM' (see ROTOR for definition of x' axes).        !/*ab         75*/
      pi(1) = pim*sti*cfi                                                       !/*ab         76*/
      pi(2) = pim*sti*sfi                                                       !/*ab         77*/
      pi(3) = pim*cti                                                           !/*ab         78*/
!   Rotor finds the components of the final state particle momentum in          !/*ab         79*/
!   the CM frame rotated to have its axes parallel to the corresponding         !/*ab         80*/
!   axes in the lab frame.                                                      !/*ab         81*/
      call rotor (pins, v, pi, pist)                                            !/*ab         82*/
      pnst(1) = -pist(1)                                                        !/*ab         83*/
      pnst(2) = -pist(2)                                                        !/*ab         84*/
      pnst(3) = -pist(3)                                                        !/*ab         85*/
#endif                                                                          /*!ab         86*/
      return                                                                    !/*ab         87*/
      end                                                                       !/*ab         88*/

      subroutine tinvu (tin, cmin, tn, cmn, sintin, costin, sinfin,             !/*ti          1*/
     &                  cosfin, sintn, costn, sinfn, cosfn, v, u, tin1)         !/*ti          2*/
#ifdef CEM                                                                      /*!ti          3*/
! ======================================================================        !/*ti          4*/
!                                                                               !/*ti          5*/
!     Calculation of tin1, v, & u                                               !/*ti          6*/
!     tin1 is kinetic energy of the cascade particle in a frame in              !/*ti          7*/
!         which the nuclear partner particle is at rest.                        !/*ti          8*/
!     u is the total energy available in the center-of-momentum frame.          !/*ti          9*/
!     v is the velocity of the center-of-momentum frame of the                  !/*ti         10*/
!         interacting particles with respect to the nucleus.                    !/*ti         11*/
!                                                                               !/*ti         12*/
!     The four variables ....in correspond to the direction of the              !/*ti         13*/
!     momentum of the cascade particle; those ....n to the direction            !/*ti         14*/
!     of the target particle (or two-particle state, for absorption)            !/*ti         15*/
!     in the lab.                                                               !/*ti         16*/
!                                                                               !/*ti         17*/
!     Called by: ABSORP POINTE                                                  !/*ti         18*/
!                                                                               !/*ti         19*/
!   CEM95 written by S. G. Mashnik                                              !/*ti         20*/
!                                                                               !/*ti         21*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*ti         22*/
!   Edited by AJS  July, 1997.                                                  !/*ti         23*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ti         24*/
!                                                                               !/*ti         25*/
! ======================================================================        !/*ti         26*/
#include <cem/cdouble.h>                                                        /*!ti         27*/
      dimension v(3)                                                            !/*ti         28*/
! ======================================================================        !/*ti         29*/
!   Momentum and total energy of cascade particle in lab frame:                 !/*ti         30*/
      pin = sqrt(tin*(tin + dp2*cmin))                                          !/*ti         31*/
      pinx = pin*sintin*cosfin                                                  !/*ti         32*/
      piny = pin*sintin*sinfin                                                  !/*ti         33*/
      pinz = pin*costin                                                         !/*ti         34*/
      ein = tin + cmin                                                          !/*ti         35*/
!   Momentum and total energy of Fermi sea particle (or of pair of              !/*ti         36*/
!   nucleons for pion absorption) in lab rframe:                                !/*ti         37*/
      pn = sqrt(tn*(tn + dp2*cmn))                                              !/*ti         38*/
      pnx = pn*sintn*cosfn                                                      !/*ti         39*/
      pny = pn*sintn*sinfn                                                      !/*ti         40*/
      pnz = pn*costn                                                            !/*ti         41*/
      en = tn + cmn                                                             !/*ti         42*/
!   Total energy in lab frame.                                                  !/*ti         43*/
      fac = ein + en                                                            !/*ti         44*/
!   Velocity of the center-of-momentum system of the 2 (or 3)                   !/*ti         45*/
!   particles in the lab frame.                                                 !/*ti         46*/
      v(1) = (pinx + pnx)/fac                                                   !/*ti         47*/
      v(2) = (piny + pny)/fac                                                   !/*ti         48*/
      v(3) = (pinz + pnz)/fac                                                   !/*ti         49*/
      v2 = v(1)**2 + v(2)**2 + v(3)**2                                          !/*ti         50*/
!   Lorentz transformation of total energy into C.M. frame.                     !/*ti         51*/
      u = fac*sqrt(dp1 - v2)                                                    !/*ti         52*/
!   KINETIC energy of projectile in target rest frame                           !/*ti         53*/
      tin1 = (u**2 - (cmin + cmn)**2)/(dp2*cmn)                                 !/*ti         54*/
#endif                                                                          /*!ti         55*/
      return                                                                    !/*ti         56*/
      end                                                                       !/*ti         57*/

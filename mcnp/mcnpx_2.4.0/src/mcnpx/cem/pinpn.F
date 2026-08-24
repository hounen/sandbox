      subroutine pinpn (partin, ipatin, nout, am0, p0, t3)                      !/*pi          1*/
#ifdef CEM                                                                      /*!pi          2*/
! ======================================================================        !/*pi          3*/
!                                                                               !/*pi          4*/
!     Calculation of entry point of particle into nucleus, initial              !/*pi          5*/
!     angular momentum of nucleus.                                              !/*pi          6*/
!                                                                               !/*pi          7*/
!     Called by: CASCADE                                                        !/*pi          8*/
!                                                                               !/*pi          9*/
!     Calls: POTEN                                                              !/*pi         10*/
!                                                                               !/*pi         11*/
!   CEM95 written by S. G. Mashnik                                              !/*pi         12*/
!                                                                               !/*pi         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*pi         14*/
!   Modified by A. J. Sierk,  LANL  T-2  March, 1996.                           !/*pi         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*pi         16*/
!                                                                               !/*pi         17*/
! ======================================================================        !/*pi         18*/
!                                                                               !/*pi         19*/
!  Definition of partin:                                                        !/*pi         20*/
!                       partin(1); Normalized x coordinate of particle          !/*pi         21*/
!                       partin(2); Normalized y coordinate of particle          !/*pi         22*/
!                       partin(3); Normalized z coordinate of particle          !/*pi         23*/
!                       partin(4); sin(theta), direction of momentum            !/*pi         24*/
!                       partin(5); cos(theta), direction of momentum            !/*pi         25*/
!                       partin(6); sin(phi), direction of momentum              !/*pi         26*/
!                       partin(7); cos(phi), direction of momentum              !/*pi         27*/
!                       partin(8); kinetic energy of particle                   !/*pi         28*/
!                       partin(9); rest mass of particle                        !/*pi         29*/
!                                                                               !/*pi         30*/
!  Definition of ipatin:                                                        !/*pi         31*/
!                       ipatin(1); charge of particle                           !/*pi         32*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*pi         33*/
!                       ipatin(3); strangeness of particle                      !/*pi         34*/
!                       ipatin(4); particle baryon number                       !/*pi         35*/
!                       ipatin(5); zone number of nucleus where particle        !/*pi         36*/
!                                  is located.                                  !/*pi         37*/
!                                                                               !/*pi         38*/
! ======================================================================        !/*pi         39*/
#include <cem/cdouble.h>                                                        /*!pi         40*/
      dimension rsm(10), rbig(10), rhop(10), rhon(10), tfp(10), tfn(10),        !/*pi         41*/
     &          af(10), partin(9), ipatin(5), am0(3)                            !/*pi         42*/
      common /begin/ cm0, t0gev, me0, mb0, ms0, l0                              !/*pi         43*/
      common /o/     anucl, znucl, eps, vpi, rsm, rbig, rhop, rhon,             !/*pi         44*/
     &               af, tfp, tfn, n                                            !/*pi         45*/
      common /pii/   pi                                                         !/*pi         46*/
! ======================================================================        !/*pi         47*/
      twpi = dp2*pi                                                             !/*pi         48*/
!   sin(theta) direction of momentum                                            !/*pi         49*/
      partin(4) = dp0                                                           !/*pi         50*/
!   cos(theta) direction of momentum                                            !/*pi         51*/
      partin(5) = dp1                                                           !/*pi         52*/
!   cos(phi) direction of momentum                                              !/*pi         53*/
      partin(6) = dp0                                                           !/*pi         54*/
!   sin(phi) direction of momentum                                              !/*pi         55*/
      partin(7) = dp1                                                           !/*pi         56*/
      partin(9) = cm0                                                           !/*pi         57*/
      ipatin(1) = me0                                                           !/*pi         58*/
!  l0 and ms0 are always 0 in CEM97a!!                                          !/*pi         59*/
      ipatin(2) = l0                                                            !/*pi         60*/
      ipatin(3) = ms0                                                           !/*pi         61*/
      ipatin(4) = mb0                                                           !/*pi         62*/
      ipatin(5) = n                                                             !/*pi         63*/
      nout = 0                                                                  !/*pi         64*/
      temp1 = rang()                                                            !/*pi         65*/
      temp2 = twpi*rang()                                                       !/*pi         66*/
      temp3 = rbig(n)*sqrt(temp1)                                               !/*pi         67*/
!  x, y, and z coordinates (in units of rsm(n) of point where particle          !/*pi         68*/
!  enters nucleus, measured with respect to the center of the nucleus           !/*pi         69*/
!  with the z-axis being parallel to the beam:                                  !/*pi         70*/
      partin(1) = temp3*cos(temp2)                                              !/*pi         71*/
      partin(2) = temp3*sin(temp2)                                              !/*pi         72*/
      partin(3) = -rbig(n)*sqrt(dp1 - temp1)                                    !/*pi         73*/
      p0 = sqrt(t0gev*(t0gev + dp2*cm0))                                        !/*pi         74*/
!  Cartesian components of angular momentum of incoming particle:               !/*pi         75*/
!  Seems to be the negative of  r x p  !!                                       !/*pi         76*/
      am0(1) = -p0*partin(2)                                                    !/*pi         77*/
      am0(2) =  p0*partin(1)                                                    !/*pi         78*/
      am0(3) =  dp0                                                             !/*pi         79*/
!  Incident kinetic energy:                                                     !/*pi         80*/
      t3 = t0gev                                                                !/*pi         81*/
!  Total kinetic energy of particle in outside zone of the potential            !/*pi         82*/
!   well (tin + pot):                                                           !/*pi         83*/
      partin(8) = t0gev + poten (n, n, ipatin, tfp, tfn, af, vpi, eps)          !/*pi         84*/
#endif                                                                          /*!pi         85*/
      return                                                                    !/*pi         86*/
      end                                                                       !/*pi         87*/

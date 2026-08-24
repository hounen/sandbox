      subroutine refrac (partin, ipatin, nabs, nout, irefl)                     !/*re          1*/
#ifdef CEM                                                                      /*!re          2*/
! ======================================================================        !/*re          3*/
!                                                                               !/*re          4*/
!     Calculation of energy and direction change of a particle                  !/*re          5*/
!     when entering another zone of the nucleus.                                !/*re          6*/
!                                                                               !/*re          7*/
!     NOTE: NO DIRECTION CHANGE CALCULATED!!!                                   !/*re          8*/
!                                                                               !/*re          9*/
!     Called by: POINTE                                                         !/*re         10*/
!                                                                               !/*re         11*/
!     Calls: POTEN                                                              !/*re         12*/
!                                                                               !/*re         13*/
!     CEM95 written by S. G. Mashnik                                            !/*re         14*/
!                                                                               !/*re         15*/
!     Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                  !/*re         16*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*re         17*/
!                                                                               !/*re         18*/
! ======================================================================        !/*re         19*/
!                                                                               !/*re         20*/
!  nout = 1 if particle is in a zone outside the nucleus.                       !/*re         21*/
!           [ipatin(5) > n+1]                                                   !/*re         22*/
!                                                                               !/*re         23*/
!  Definition of partin:                                                        !/*re         24*/
!                       partin(1); Normalized x coordinate of particle          !/*re         25*/
!                       partin(2); Normalized y coordinate of particle          !/*re         26*/
!                       partin(3); Normalized z coordinate of particle          !/*re         27*/
!                       partin(4); sin(theta), direction of momentum            !/*re         28*/
!                       partin(5); cos(theta), direction of momentum            !/*re         29*/
!                       partin(6); sin(phi), direction of momentum              !/*re         30*/
!                       partin(7); cos(phi), direction of momentum              !/*re         31*/
!                       partin(8); kinetic energy of particle                   !/*re         32*/
!                       partin(9); rest mass of particle                        !/*re         33*/
!                                                                               !/*re         34*/
!  Definition of ipatin:                                                        !/*re         35*/
!                       ipatin(5); zone number of nucleus where particle        !/*re         36*/
!                                  is located.                                  !/*re         37*/
!                                                                               !/*re         38*/
! ======================================================================        !/*re         39*/
#include <cem/cdouble.h>                                                        /*!re         40*/
      dimension partin(9), ipatin(5), tfp(10), tfn(10), af(10),                 !/*re         41*/
     &          rsm(10), rbig(10), rhop(10), rhon(10)                           !/*re         42*/
      common /o/ anucl, znucl, eps, vpi, rsm, rbig, rhop, rhon, af,             !/*re         43*/
     &           tfp, tfn, n                                                    !/*re         44*/
! ======================================================================        !/*re         45*/
      nabs = 0                                                                  !/*re         46*/
      nout = 0                                                                  !/*re         47*/
      rin = sqrt(partin(1)**2 + partin(2)**2 + partin(3)**2)                    !/*re         48*/
      ctinjb = (partin(4)*(partin(1)*partin(7) +                                !/*re         49*/
     &          partin(2)*partin(6)) +                                          !/*re         50*/
     &          partin(3)*partin(5))/rin                                        !/*re         51*/
!   ctinjb < 0 means inward going particle                                      !/*re         52*/
      if (ctinjb.lt.dp0) then                                                   !/*re         53*/
        jr1 = ipatin(5) - 1                                                     !/*re         54*/
      else                                                                      !/*re         55*/
        jr1 = ipatin(5) + 1                                                     !/*re         56*/
      endif                                                                     !/*re         57*/
      tinj1 = partin(8) + poten (jr1, n, ipatin, tfp, tfn, af, vpi, eps)        !/*re         58*/
     &        - poten (ipatin(5), n, ipatin, tfp, tfn, af, vpi, eps)            !/*re         59*/
      partin(8) = tinj1                                                         !/*re         60*/
      ipatin(5) = jr1                                                           !/*re         61*/
      if (jr1 - n .ge. 2) nout = 1                                              !/*re         62*/
#endif                                                                          /*!re         63*/
      return                                                                    !/*re         64*/
      end                                                                       !/*re         65*/

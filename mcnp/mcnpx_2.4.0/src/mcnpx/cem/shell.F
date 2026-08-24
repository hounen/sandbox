      function shell (a, z, ish)                                                !/*sh          1*/
! ======================================================================        !/*sh          2*/
!                                                                               !/*sh          3*/
!   Ground state shell and pairing corrections vs. Z and A.                     !/*sh          4*/
!                                                                               !/*sh          5*/
!   Called by: DELTAM PRECOF                                                    !/*sh          6*/
!                                                                               !/*sh          7*/
!   Calls: MOLNIX                                                               !/*sh          8*/
!                                                                               !/*sh          9*/
!   CEM95 written by S. G. Mashnik                                              !/*sh         10*/
!                                                                               !/*sh         11*/
!   Added ish to agrument list, so shell can be used either for ground          !/*sh         12*/
!   state shell and pairing correction to barrier heights, or for               !/*sh         13*/
!   shell correction to level densities.                                        !/*sh         14*/
!                                                                               !/*sh         15*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*sh         16*/
!   Modified by A. J. Sierk,  LANL  T-2  April-May, 1996.                       !/*sh         17*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*sh         18*/
!                                                                               !/*sh         19*/
! ======================================================================        !/*sh         20*/
#include <cem/cdouble.h>                                                        /*!sh         21*/
#ifndef CEM                                                                     /*!sh         22*/
      shell=dp0                                                                 !/*sh         23*/
#else                                                                           /*!sh         24*/
      common /bl1001/   t1y(130)                                                !/*sh         25*/
      common /bl1002/  t2xy(200)                                                !/*sh         26*/
      common /cam70/   sn70(155), sz70(102), pn70(155), pz70(102)               !/*sh         27*/
! ======================================================================        !/*sh         28*/
      iz = int(z + dp1m6)                                                       !/*sh         29*/
      un = a - z                                                                !/*sh         30*/
      in = int(un + dp1m6)                                                      !/*sh         31*/
      if (ish.eq.2) then                                                        !/*sh         32*/
!   Truran, Cameron & Hilf: [Proc. Int. Conf. on the Prop. of Nuclei far        !/*sh         33*/
!   from the Region of Beta-Stability, Leysin, 1, 275 (1970)]                   !/*sh         34*/
!   shell corrections.                                                          !/*sh         35*/
        if (in.lt.10 .or. in.gt.155 .or. iz.lt.10 .or. iz.gt.102) then          !/*sh         36*/
          shell = (t1y(iz) + t2xy(in))                                          !/*sh         37*/
        else                                                                    !/*sh         38*/
          shell = sz70(iz) + pz70(iz) + sn70(in) + pn70(in)                     !/*sh         39*/
        endif                                                                   !/*sh         40*/
      elseif (ish.eq.4) then                                                    !/*sh         41*/
!   Microscopic correction to ground state mass from the Finite Range           !/*sh         42*/
!   Liquid Drop Model of Moller, Nix, Myers, & Swiatecki, [Atomic Data          !/*sh         43*/
!   Nucl. Data Tables, 59, 185 (1995)].                                         !/*sh         44*/
        shell = fmolnix (iz, in, 1)                                             !/*sh         45*/
      endif                                                                     !/*sh         46*/
#endif                                                                          /*!sh         47*/
      return                                                                    !/*sh         48*/
      end                                                                       !/*sh         49*/

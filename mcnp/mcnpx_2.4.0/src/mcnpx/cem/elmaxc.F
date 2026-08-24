      subroutine elmaxc (zn, an, elmax)                                         !/*el          1*/
#ifdef CEM                                                                      /*!el          2*/
! ======================================================================        !/*el          3*/
!                                                                               !/*el          4*/
!   This subroutine evaluates a 2-dimensional fitting function                  !/*el          5*/
!   to arrive at an approximation to Lmax(Z,A), the value of                    !/*el          6*/
!   angular momentum in units of h-bar at which the fission                     !/*el          7*/
!   saddle point and the fission barrier vanishes.  Lmax is the                 !/*el          8*/
!   maximum amount of angular momentum which the nucleus can                    !/*el          9*/
!   sustain without fissioning.                                                 !/*el         10*/
!                                                                               !/*el         11*/
!   The 2-dimensional fit was made to 192 calculated values of                  !/*el         12*/
!   Lmax for values of Z from 10 to 110, with A values spanning                 !/*el         13*/
!   a range larger than any nuclei which might be formed in                     !/*el         14*/
!   a reaction.                                                                 !/*el         15*/
!                                                                               !/*el         16*/
!   The 192 calculated values of Lmax were calculated in the                    !/*el         17*/
!   three-quadratic-surface axially symmetric parametrization,                  !/*el         18*/
!   or in the triaxial Legendre Polynomial parametrization                      !/*el         19*/
!   using Yukawa-plus-exponential nuclear energy, diffuse                       !/*el         20*/
!   Coulomb energy, and diffuse-matter moments of inertia by                    !/*el         21*/
!   A. J. Sierk of Los Alamos National Laboratory, Group T-2.                   !/*el         22*/
!                                                                               !/*el         23*/
!   This subroutine written February, 1994.                                     !/*el         24*/
!   Updated constants and extended Z and A range of fit, May, 1996.             !/*el         25*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*el         26*/
!                                                                               !/*el         27*/
! ======================================================================        !/*el         28*/
#include <cem/cdouble.h>                                                        /*!el         29*/
      dimension b(40), pla(5), plz(8)                                           !/*el         30*/
      data npa, npz /5,8/                                                       !/*el         31*/
      data b                                                                    !/*el         32*/
     & /-3.30787006d+05, 8.34310803d+05,-4.11955431d+05, 9.82177374d+05,        !/*el         33*/
     &   3.40653902d+05, 8.39479538d+05,-2.13869250d+06, 1.11540268d+06,        !/*el         34*/
     &  -2.52645986d+06,-7.72016242d+05,-1.00538896d+06, 2.59234090d+06,        !/*el         35*/
     &  -1.49269622d+06, 3.06066515d+06, 7.02514732d+05, 7.97067135d+05,        !/*el         36*/
     &  -2.08607738d+06, 1.33444088d+06,-2.52998737d+06,-3.45973915d+05,        !/*el         37*/
     &  -4.15169782d+05, 1.10638461d+06,-7.71715934d+05, 1.45998634d+06,        !/*el         38*/
     &   6.29116062d+04, 1.07407080d+05,-3.04281993d+05, 2.18876998d+05,        !/*el         39*/
     &  -5.37873263d+05, 2.14561453d+04, 1.64345870d+03, 6.52424621d+03,        !/*el         40*/
     &   3.41057615d+03, 1.03029197d+05,-1.17188782d+04,-5.44933809d+03,        !/*el         41*/
     &   1.23028722d+04,-1.25511319d+04,-5.30918539d+03, 1.17305352d+03/        !/*el         42*/
! ======================================================================        !/*el         43*/
      xz = zn/100.d0                                                            !/*el         44*/
      xa = an/320.d0                                                            !/*el         45*/
      elmax = 0.d0                                                              !/*el         46*/
      call lpoly2 (xz, plz, npz)                                                !/*el         47*/
      call lpoly2 (xa, pla, npa)                                                !/*el         48*/
        do 20 iz = 1,npz                                                        !/*el         49*/
          do 10 ia = 1,npa                                                      !/*el         50*/
          ib = ia + npa*(iz-1)                                                  !/*el         51*/
          elmax = elmax + b(ib)*plz(iz)*pla(ia)                                 !/*el         52*/
   10     continue                                                              !/*el         53*/
   20   continue                                                                !/*el         54*/
#endif                                                                          /*!el         55*/
      return                                                                    !/*el         56*/
      end                                                                       !/*el         57*/

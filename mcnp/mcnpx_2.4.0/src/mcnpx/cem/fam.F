      function fam (a, z, e)                                                    !/*fa          1*/
! ======================================================================        !/*fa          2*/
!                                                                               !/*fa          3*/
!   Calculates a level density parameter, a                                     !/*fa          4*/
!                                                                               !/*fa          5*/
!   CEM95 written by S. G. Mashnik                                              !/*fa          6*/
!                                                                               !/*fa          7*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*fa          8*/
!   Modified to add ifam = 11, 12; May, 1996.                                   !/*fa          9*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*fa         10*/
!                                                                               !/*fa         11*/
! ======================================================================        !/*fa         12*/
#include <cem/cdouble.h>                                                        /*!fa         13*/
#ifndef CEM                                                                     /*!fa         14*/
      fam=dp0                                                                   !/*fa         15*/
#else                                                                           /*!fa         16*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*fa         17*/
      common /menu1/   ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel           !/*fa         18*/
      common /menu2/   aam, isha, ipair                                         !/*fa         19*/
! ======================================================================        !/*fa         20*/
      if (ifam.eq.9) then                                                       !/*fa         21*/
!   Third set of Iljinov, Mebel E. A. parameters: without collective            !/*fa         22*/
!   effects; Truran, Cameron & Hilf shell corrections; without A                !/*fa         23*/
!   dependence of ga.                                                           !/*fa         24*/
        al = 0.072d0                                                            !/*fa         25*/
        be = 0.257d0                                                            !/*fa         26*/
        ga = 0.059d0                                                            !/*fa         27*/
        afg = al + be/a**thrd                                                   !/*fa         28*/
!   Set of parameters derived by Sierk in May 1996, following method            !/*fa         29*/
!   of Iljinov, et al., using nearly the same data set; several                 !/*fa         30*/
!   errors in "J"s, Bn values, and a few apparent misprints in the              !/*fa         31*/
!   Tables 1 & 2 were corrected. Using Moller-Nix-Myers-Swiatecki (1992)        !/*fa         32*/
!   shell corrections; pairing = 11/sqrt(A); without collective effects;        !/*fa         33*/
!   without A dependence of ga. Gives nearly identical fit to ifam = 7,         !/*fa         34*/
!   above, but without the time-consuming calculation of shell                  !/*fa         35*/
!   corrections following Myers & Swiatecki.                                    !/*fa         36*/
      elseif (ifam.eq.11) then                                                  !/*fa         37*/
        al =  0.1450d0                                                          !/*fa         38*/
        be = -0.0694d0                                                          !/*fa         39*/
        ga =  0.0511d0                                                          !/*fa         40*/
        afg = al + be/a**thrd                                                   !/*fa         41*/
!   Set of parameters derived by Sierk in May 1996, following method            !/*fa         42*/
!   of Iljinov, et al., using nearly the same data set; several                 !/*fa         43*/
!   errors in "J"s, Bn values, and a few apparent misprints in the              !/*fa         44*/
!   Tables 1 & 2 were corrected. Using Moller-Nix-Myers-Swiatecki (1992)        !/*fa         45*/
!   shell corrections; Moller-Nix pairing gaps for the shifted Fermi            !/*fa         46*/
!   gas level density formula; without collective effects;                      !/*fa         47*/
!   without A dependence of ga. Gives not quite as good fit as                  !/*fa         48*/
!   ifam=11, above. [2nd version uses rho = 0.010 x exp(2 x sqrt(aE*))]         !/*fa         49*/
      elseif (ifam.eq.12) then                                                  !/*fa         50*/
        al =  0.1463d0                                                          !/*fa         51*/
        be = -0.0716d0                                                          !/*fa         52*/
        ga =  0.0542d0                                                          !/*fa         53*/
        afg = al + be/a**thrd                                                   !/*fa         54*/
      endif                                                                     !/*fa         55*/
      fe = dp1 - exp(-ga*e)                                                     !/*fa         56*/
      ish1 = isha                                                               !/*fa         57*/
!   If finding saddle point level density, do not include GS                    !/*fa         58*/
!   shell & pairing correction (e input as -1.0).                               !/*fa         59*/
      sh = dp0                                                                  !/*fa         60*/
      if (e.ne.-dp1) sh = shell (a, z, ish1)                                    !/*fa         61*/
      fam = afg*(dp1 + fe*sh/e)                                                 !/*fa         62*/
#endif                                                                          /*!fa         63*/
      return                                                                    !/*fa         64*/
      end                                                                       !/*fa         65*/

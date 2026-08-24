!     $Id: mscat3.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                       !/*ms          1*/
      subroutine mscat3 (lp,delt,arg_,e,theta)                                  !/*ms          2*/
!-----------------------------------------------------------------------        !/*ms          3*/
! lp:       LAHET particle type                                                 !/*ms          4*/
! delt:     step length (cm)                                                    !/*ms          5*/
! arg_:     MCS parameter in current medium                                     !/*ms          6*/
! e:        particle kinetic energy (MeV)                                       !/*ms          7*/
! theta:    MCS polar scattering angle (radians)                                !/*ms          8*/
! variables used from COMMON RUCOM:                                             !/*ms          9*/
! charge:   particle charge array                                               !/*ms         10*/
! pmass:    particle mass array                                                 !/*ms         11*/
!.......................................................................        !/*ms         12*/
#include <lcs/double.h>                                                         /*!ms         13*/
#include <lcs/lptS.h>                                                           /*!ms         14*/
#include <lcs/rucom.h>                                                          /*!ms         15*/
!=======================================================================        !/*ms         16*/
      xmas=pmass(lp)                                                            !/*ms         17*/
      sigth1=arg_*abs(charge(lp))/(e*(dp1+xmas/(e+xmas)))                       !/*ms         18*/
      sigth=dp2*sigth1*sqrt(dp2*delt)                                           !/*ms         19*/
      theta=sigth*sqrt(-log(rang()))                                            !/*ms         20*/
!-----------------------------------------------------------------------        !/*ms         21*/
      return                                                                    !/*ms         22*/
      end                                                                       !/*ms         23*/

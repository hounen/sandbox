!     $Id: kranch.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                       !/*kr          1*/
      subroutine kranch (p10,ws3,wsp3,ws4,wsp4,finex)                           !/*kr          2*/
#include <lcs/double.h>                                                         /*!kr          3*/
!     ==================================================================        !/*kr          4*/
!     program calculates the probability of a k+n --> k0,p                      !/*kr          5*/
!                                         and k0,p -->k+,n                      !/*kr          6*/
!     charge-exchange reaction                                                  !/*kr          7*/
!                                                                               !/*kr          8*/
! KRANCH calls   PTOTKL  EXP                                                    !/*kr          9*/
! called by GAMMA                                                               !/*kr         10*/
!     ==================================================================        !/*kr         11*/
!                                                                               !/*kr         12*/
      finex=dp0                                                                 !/*kr         13*/
      typ=ws3-8.d0                                                              !/*kr         14*/
      if (typ.gt.dp0) return                                                    !/*kr         15*/
      call ptotkl (p10,typ,scp,scn,abp,abn)                                     !/*kr         16*/
!                                                                               !/*kr         17*/
      if (typ) 10,20,30                                                         !/*kr         18*/
!       k+ incident                                                             !/*kr         19*/
   10 if (wsp3.eq.dp1) return                                                   !/*kr         20*/
!               cross-section fit to data of                                    !/*kr         21*/
!               giacomelli, np b42(1972) 437                                    !/*kr         22*/
!               slater,    prl  10(1961) 378                                    !/*kr         23*/
      arg=dph*((p10-9.d+02)/51.d1)**2                                           !/*kr         24*/
      sigex=6.6d0*exp(-arg)                                                     !/*kr         25*/
      finex=sigex/scn                                                           !/*kr         26*/
      if (finex.gt.dp1) finex=dp1                                               !/*kr         27*/
      return                                                                    !/*kr         28*/
!       k0 incident                                                             !/*kr         29*/
   20 if (wsp3.eq.dp2) return                                                   !/*kr         30*/
!               cross-section fit to data of                                    !/*kr         31*/
!               armitage, np b123(1977) 11                                      !/*kr         32*/
      arg=dph*((p10-8.d+02)/42.d1)**2                                           !/*kr         33*/
      sigex=8.d0*exp(-arg)                                                      !/*kr         34*/
      finex=sigex/scp                                                           !/*kr         35*/
      if (finex.gt.dp1) finex=dp1                                               !/*kr         36*/
   30 return                                                                    !/*kr         37*/
      end                                                                       !/*kr         38*/

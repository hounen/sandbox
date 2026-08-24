!     $Id: pih.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                          !/*pi          1*/
      subroutine pih (delta)                                                    !/*pi          2*/
#include <lcs/double.h>                                                         /*!pi          3*/
!     ==================================================================        !/*pi          4*/
!     pih divides the momentum of the isobar to the two decay particles.        !/*pi          5*/
!     pih is also called when isobar does exchange scattering                   !/*pi          6*/
!     when the delta does not equal to zero.  then the momentum of              !/*pi          7*/
!     the two scattered particles have to be recalculated.                      !/*pi          8*/
!     also in isobar capture when delta does not equal to zero,                 !/*pi          9*/
!     pih is called to recalculate the momentum of the two captured             !/*pi         10*/
!     particles.                                                                !/*pi         11*/
!                                                                               !/*pi         12*/
! PIH    calls   SQRT                                                           !/*pi         13*/
! called by DECAY  GAMMA                                                        !/*pi         14*/
!     ==================================================================        !/*pi         15*/
!                                                                               !/*pi         16*/
#include <lcs/vegas.h>                                                          /*!pi         17*/
      e=ws(4)*ws(4)                                                             !/*pi         18*/
!                                                                               !/*pi         19*/
! cm3 has the energy for decay,that is the mass plus delta(the fugde            !/*pi         20*/
!  factor).  cm(3) has to be divided between nucleon and pion requiring         !/*pi         21*/
!  the momentum of the two decay particles to be equal.  also see flow          !/*pi         22*/
!  chart for pih.                                                               !/*pi         23*/
! changes made 10/6/70 to conserve energy and momentum in decay,                !/*pi         24*/
! charge exchange scattering,and absorption when a difference in                !/*pi         25*/
! potential occurs.                                                             !/*pi         26*/
      cm3=cm(3)                                                                 !/*pi         27*/
! pi6 is the square of the momentum.  if pi6 is negative then decay is          !/*pi         28*/
!  not allowed.  then ind(18)=dp1                                               !/*pi         29*/
      pi6=(cm3*cm3+wsp(19)-e)/cm3/dp2                                           !/*pi         30*/
      pi6=pi6*pi6-wsp(19)                                                       !/*pi         31*/
      if (pi6.gt.dp0) go to 40                                                  !/*pi         32*/
   10 ind(18)=1                                                                 !/*pi         33*/
      go to 30                                                                  !/*pi         34*/
   20 pi6=sqrt(pi6)                                                             !/*pi         35*/
!  now pi6 is the momentum                                                      !/*pi         36*/
!  cm(7,8,9,10) are the x,y,z,component of momentum in                          !/*pi         37*/
!  c.m.and energy of particle in ws.  for decay of isobar px=p.                 !/*pi         38*/
!  cm(11,12,13,14) are similar quantities for particle in wsp.                  !/*pi         39*/
      cm(7)=cm(5)*pi6                                                           !/*pi         40*/
      cm(11)=-cm(7)                                                             !/*pi         41*/
      cm(9)=cm(6)*pi6                                                           !/*pi         42*/
      cm(13)=-cm(9)                                                             !/*pi         43*/
      cosy2=1-cm(5)*cm(5)-cm(6)*cm(6)                                           !/*pi         44*/
      cm(8)=dp0                                                                 !/*pi         45*/
      if (cosy2.gt.dp0) cm(8)=sqrt(cosy2)*pi6                                   !/*pi         46*/
      cm(12)=-cm(8)                                                             !/*pi         47*/
      pi6=pi6*pi6                                                               !/*pi         48*/
      cm(10)=sqrt(pi6+e)                                                        !/*pi         49*/
      cm(14)=sqrt(pi6+wsp(19))                                                  !/*pi         50*/
   30 return                                                                    !/*pi         51*/
   40 continue                                                                  !/*pi         52*/
      if (delta.eq.dp0) go to 20                                                !/*pi         53*/
! now lengthen or shorten the c.m.momentum vector.(see notes)                   !/*pi         54*/
      c=cm(2)+delta                                                             !/*pi         55*/
      cprime=cm(3)*c/cm(2)                                                      !/*pi         56*/
      psqc=(cprime*cprime+e-wsp(19))/(dp2*cprime)                               !/*pi         57*/
      psqc=psqc*psqc-e                                                          !/*pi         58*/
      pi6=psqc                                                                  !/*pi         59*/
      if (pi6.gt.dp0) go to 20                                                  !/*pi         60*/
      go to 10                                                                  !/*pi         61*/
      end                                                                       !/*pi         62*/

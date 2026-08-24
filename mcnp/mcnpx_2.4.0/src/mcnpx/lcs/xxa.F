!     $Id: xxa.F,v 1.4 2002/03/27 18:15:27 gwm Exp $                          !/*xx          1*/
      function xxa (at,zt,ap,zp,a2,z2,epsa,epsb)                                !/*xx          2*/
#include <lcs/double.h>                                                         /*!xx          3*/
!     ATAR: target A                                                            !/*xx          4*/
!     ZT: target Z                                                              !/*xx          5*/
!     AP: projectile A                                                          !/*xx          6*/
!     ZP: projectile Z                                                          !/*xx          7*/
!     A2: ejectile A                                                            !/*xx          8*/
!     Z2: ejectile Z                                                            !/*xx          9*/
!     EPSA: channel energy in incident channel                                  !/*xx         10*/
!     EPSB: channel energy in emission channel                                  !/*xx         11*/
!                                                                               !/*xx         12*/
!     xmb=dp1                                                                   !/*xx         13*/
!     if (za2.eq.1.) xmb=dph                                                    !/*xx         14*/
!     if (za2.eq.2004.) xmb=dp0                                                 !/*xx         15*/
!                                                                               !/*xx         16*/
      if (z2.eq.dp0) then                                                       !/*xx         17*/
        xmb=dph                                                                 !/*xx         18*/
      elseif (a2.eq.dp2) then                                                   !/*xx         19*/
        xmb=dp2                                                                 !/*xx         20*/
      else                                                                      !/*xx         21*/
        xmb=dp1                                                                 !/*xx         22*/
      endif                                                                     !/*xx         23*/
!                                                                               !/*xx         24*/
      if (ap.eq.dp1) then                                                       !/*xx         25*/
        xma=dp1                                                                 !/*xx         26*/
      else                                                                      !/*xx         27*/
        xma=dp0                                                                 !/*xx         28*/
      endif                                                                     !/*xx         29*/
!      determine ssa                                                            !/*xx         30*/
      ac=at+ap                                                                  !/*xx         31*/
      ab=at                                                                     !/*xx         32*/
      zc=zt+zp                                                                  !/*xx         33*/
      xnc=ac-zc                                                                 !/*xx         34*/
      zb=zt                                                                     !/*xx         35*/
      xnb=ab-zb                                                                 !/*xx         36*/
      ssa=sliq(ac,ab,zc,zb,xnc,xnb)                                             !/*xx         37*/
!      determine ssb                                                            !/*xx         38*/
      ab=ac-a2                                                                  !/*xx         39*/
      zb=zc-z2                                                                  !/*xx         40*/
      xnb=ab-zb                                                                 !/*xx         41*/
      ssb=sliq(ac,ab,zc,zb,xnc,xnb)                                             !/*xx         42*/
!                                                                               !/*xx         43*/
      epa=epsa+ssa                                                              !/*xx         44*/
      epb=epsb+ssb                                                              !/*xx         45*/
      xx1=min(epa,dp130)*epb/epa                                                !/*xx         46*/
      xx3=min(epa,dp41)*epb/epa                                                 !/*xx         47*/
      xxa=4.d-2*xx1+(1.8d-6)*(xx1**3)+(6.7d-7)*xma*xmb*(xx3**4)                 !/*xx         48*/
      return                                                                    !/*xx         49*/
      end                                                                       !/*xx         50*/

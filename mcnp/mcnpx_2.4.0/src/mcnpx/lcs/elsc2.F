!     $Id: elsc2.F,v 1.6 2002/03/27 18:15:02 gwm Exp $                        !/*el          1*/
      subroutine elsc2 (ecl,a1,z1)                                              !/*el          2*/
!     ==================================================================        !/*el          3*/
!     elsc2   calls the following subroutines and functions                     !/*el          4*/
!        AZIRN        ENERGY        PDEVNT        SIGELS        SQRT            !/*el          5*/
!     ==================================================================        !/*el          6*/
#include <lcs/double.h>                                                         /*!el          7*/
#ifndef OLDM                                                                    /*!el          8*/
#include <lcs/phprm0.h>                                                         /*!el          9*/
#endif                                                                          /*!el         10*/
#include <lcs/lptS.h>                                                           /*!el         11*/
#include <lcs/rucom.h>                                                          /*!el         12*/
#include <lcs/cacom.h>                                                          /*!el         13*/
#include <lcs/banka.h>                                                          /*!el         14*/
      external elbd                                                             !/*el         15*/
      parameter (ieused=5)                                                      !/*el         16*/
#ifdef OLDM                                                                     /*!el         17*/
!     CHECK: pi+ mass increased by electron mass                                !/*el         18*/
!     CHECK: pi- mass decreased by electron mass                                !/*el         19*/
      data amumev /931.20793d0/                                                 !/*el         20*/
#endif                                                                          /*!el         21*/
      ex=dp0                                                                    !/*el         22*/
      exc0=dp0                                                                  !/*el         23*/
      uu=dp0                                                                    !/*el         24*/
      apr=a1                                                                    !/*el         25*/
      zpr=z1                                                                    !/*el         26*/
      ielevt=1                                                                  !/*el         27*/
      nopart=1                                                                  !/*el         28*/
      if (a1.le.dp4) nopart=2                                                   !/*el         29*/
      do 10 i=1,6                                                               !/*el         30*/
   10 npart(i)=0                                                                !/*el         31*/
      kind(nopart)=ityp-1                                                       !/*el         32*/
      wtfas(nopart)=dp1                                                         !/*el         33*/
#ifdef OLDM                                                                     /*!el         34*/
      am2=apr*amumev                                                            !/*el         35*/
      if (nobalc.eq.0) am2=am2+energy(apr,zpr)                                  !/*el         36*/
#else                                                                           /*!el         37*/
      am2=xmnuc(apr,zpr)                                                        !/*el         38*/
#endif                                                                          /*!el         39*/
      am1=pmass(ityp)                                                           !/*el         40*/
!     Modified scattering section.                                              !/*el         41*/
      p_scat=sqrt(ecl*(ecl+dp2*am1))*am2/sqrt((am1+am2)**2+dp2*am2*ecl)         !/*el         42*/
!                                                                               !/*el         43*/
      call selcst (apr,p_scat,csthcm)                                           !/*el         44*/
#ifdef LCS                                                                      /*!el         45*/
      call revnt (6)                                                            !/*el         46*/
#endif                                                                          /*!el         47*/
      am1sq=am1**2                                                              !/*el         48*/
      call kinels (csthcm,am1,am1sq,am2,ecl,ep(nopart),erec,alpha(nopart        !/*el         49*/
     & ),beta(nopart),gam(nopart))                                              !/*el         50*/
!                                                                               !/*el         51*/
      p0=sqrt(ecl*(ecl+dp2*am1))                                                !/*el         52*/
      p1=sqrt(ep(nopart)*(ep(nopart)+dp2*am1))                                  !/*el         53*/
      p2x=-p1*alpha(nopart)                                                     !/*el         54*/
      p2y=-p1*beta(nopart)                                                      !/*el         55*/
      p2z=p0-p1*gam(nopart)                                                     !/*el         56*/
      p2=sqrt(p2x**2+p2y**2+p2z**2)                                             !/*el         57*/
      if (nopart.eq.1) go to 20                                                 !/*el         58*/
!                                                                               !/*el         59*/
      if (apr.eq.dp4.and.zpr.eq.dp2) then                                       !/*el         60*/
        kind(1)=10                                                              !/*el         61*/
      elseif (apr.eq.dp3.and.zpr.eq.dp2) then                                   !/*el         62*/
        kind(1)=9                                                               !/*el         63*/
      elseif (apr.eq.dp3.and.zpr.eq.dp1) then                                   !/*el         64*/
        kind(1)=8                                                               !/*el         65*/
      elseif (apr.eq.dp2.and.zpr.eq.dp1) then                                   !/*el         66*/
        kind(1)=7                                                               !/*el         67*/
      else                                                                      !/*el         68*/
        call expirx(1,'ELSC2','illegal particle type.')                         !/*el         69*/
        return                                                                  !/*el         70*/
      endif                                                                     !/*el         71*/
      wtfas(1)=dp1                                                              !/*el         72*/
      ep(1)=erec                                                                !/*el         73*/
      erec=dp0                                                                  !/*el         74*/
      apr=dp0                                                                   !/*el         75*/
      zpr=dp0                                                                   !/*el         76*/
      alpha(1)=p2x/p2                                                           !/*el         77*/
      beta(1)=p2y/p2                                                            !/*el         78*/
      gam(1)=p2z/p2                                                             !/*el         79*/
      mnuc=0                                                                    !/*el         80*/
      go to 30                                                                  !/*el         81*/
!                                                                               !/*el         82*/
   20 continue                                                                  !/*el         83*/
      aprr(1)=apr                                                               !/*el         84*/
      zprr(1)=zpr                                                               !/*el         85*/
      erecr(1)=erec                                                             !/*el         86*/
      uur(1)=dp0                                                                !/*el         87*/
      ecvr(1)=dp0                                                               !/*el         88*/
      ur(1)=p2x/p2                                                              !/*el         89*/
      vr(1)=p2y/p2                                                              !/*el         90*/
      wr(1)=p2z/p2                                                              !/*el         91*/
      mnuc=1                                                                    !/*el         92*/
   30 continue                                                                  !/*el         93*/
!                                                                               !/*el         94*/
      return                                                                    !/*el         95*/
      end                                                                       !/*el         96*/

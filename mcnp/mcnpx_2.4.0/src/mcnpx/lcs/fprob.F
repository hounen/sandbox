!     $Id: fprob.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                        !/*fp          1*/
      function fprob (z,a,u)                                                    !/*fp          2*/
#include <lcs/double.h>                                                         /*!fp          3*/
!                                                                               !/*fp          4*/
!   function to compute the fission probability.                                !/*fp          5*/
!     for z<90 uses ..........                                                  !/*fp          6*/
!     uses statistical model fits.                                              !/*fp          7*/
!                                                                               !/*fp          8*/
#include <lcs/ercom.h>                                                          /*!fp          9*/
      parameter (lnfis=18)                                                      !/*fp         10*/
      dimension slope(lnfis), anort(lnfis)                                      !/*fp         11*/
#ifdef CHEAP                                                                    /*!fp         12*/
      double precision i0, i1                                                   !/*fp         13*/
#else                                                                           /*!fp         14*/
      real i0, i1                                                               !/*fp         15*/
#endif                                                                          /*!fp         16*/
      data a1, a2, a3, const /0.2185024d0,16.70314d0,321.175d0,0.3518099        !/*fp         17*/
     & d0/                                                                      !/*fp         18*/
      data c1, c2, c3 /1.089257d0,0.01097896d0,31.08551d0/                      !/*fp         19*/
!     forz> 89 and <100 ........                                                !/*fp         20*/
!     use the systematics of vandenbosch & huizenga with a ball park            !/*fp         21*/
!     observation that fission probability drops for most nuclei at 6 m         !/*fp         22*/
      data slope /0.23d0,0.233d0,0.12225d0,0.14727d0,0.13559d0,0.15735d0        !/*fp         23*/
     & ,0.16597d0,0.17589d0,0.18018d0,0.19568d0,0.16313d0,0.17123d0,6*0.        !/*fp         24*/
     & 17d0/                                                                    !/*fp         25*/
!     data anort /221.6,226.9,229.75,234.04,238.88,241.34,243.04,245.52,        !/*fp         26*/
!    & 246.84,250.18,254.0,257.8/                                               !/*fp         27*/
      data anort /219.4d0,226.9d0,229.75d0,234.04d0,238.88d0,241.34d0,24        !/*fp         28*/
     & 3.04d0,245.52d0,246.84d0,250.18d0,254.0d0,257.8d0,261.3d0,264.8d0        !/*fp         29*/
     & ,268.3d0,271.8d0,275.3d0,278.8d0/                                        !/*fp         30*/
      fprob=dp0                                                                 !/*fp         31*/
      iz=int(z)                                                                 !/*fp         32*/
      if (iz.gt.88) then                                                        !/*fp         33*/
!                                                                               !/*fp         34*/
!     high z fission probability                                                !/*fp         35*/
!                                                                               !/*fp         36*/
        iz=iz-88                                                                !/*fp         37*/
        if (iz.gt.lnfis.or.u.lt.6.0) go to 20                                   !/*fp         38*/
        ja=int(a)                                                               !/*fp         39*/
        gamnf=slope(iz)*(real(ja)-anort(iz))                                    !/*fp         40*/
        gamnf=dp10**gamnf                                                       !/*fp         41*/
        go to 10                                                                !/*fp         42*/
      endif                                                                     !/*fp         43*/
      jn=int(a-z)                                                               !/*fp         44*/
      se=energy((a-dp1),z)-energy(a,z)+8.3674d0-cam2(iz)-cam3(jn-1)             !/*fp         45*/
      if ((2*(iz/2).ne.iz).or.(2*(jn/2).ne.jn)) se=se-cam4(iz)-cam5(jn-1        !/*fp         46*/
     & )                                                                        !/*fp         47*/
      if ((2*(iz/2).ne.iz).and.(2*(jn/2).ne.jn)) se=se-cam4(iz)-cam5(jn-        !/*fp         48*/
     & 1)                                                                       !/*fp         49*/
      x=z*z/a                                                                   !/*fp         50*/
      eff=x*(a1*x-a2)+a3+se                                                     !/*fp         51*/
      if (eff.gt.u) go to 20                                                    !/*fp         52*/
      an=0.125d0*(a-dp1)                                                        !/*fp         53*/
      an1=dph/an                                                                !/*fp         54*/
      an2=0.25d0*an1/an                                                         !/*fp         55*/
      af=x-c3                                                                   !/*fp         56*/
      af=an*(c1+c2*af*af)                                                       !/*fp         57*/
      a1thrd=a**0.33333333d0                                                    !/*fp         58*/
      s=dp2*sqrt(an*(u-se))                                                     !/*fp         59*/
      if (s.gt.dp10) then                                                       !/*fp         60*/
        i0=an1*(s-dp1)                                                          !/*fp         61*/
        i1=s*an2*((s+s-6.d0)+6.d0)                                              !/*fp         62*/
        gamnf=const*(((0.76d0*i1-5.d-02*i0)*a1thrd+1.93d0*i1)*a1thrd+1.6        !/*fp         63*/
     &  6d0*i0)                                                                 !/*fp         64*/
        s2=dp2*sqrt(af*(u-eff))                                                 !/*fp         65*/
        exps=dp0                                                                !/*fp         66*/
        if ((s-s2).gt.-150.d0) exps=exp(s-s2)                                   !/*fp         67*/
        gamnf=gamnf*exps*af/(s2-dp1)                                            !/*fp         68*/
      else                                                                      !/*fp         69*/
        e1=exp(s)                                                               !/*fp         70*/
        i0=((s-dp1)*e1+dp1)*an1                                                 !/*fp         71*/
        i1=((6.d0+s*(s+s-6.d0))*e1+s*s-6.d0)*an2                                !/*fp         72*/
        gamnf=const*(((0.76d0*i1-5.d-02*i0)*a1thrd+1.93d0*i1)*a1thrd+1.6        !/*fp         73*/
     &  6d0*i0)                                                                 !/*fp         74*/
        s=dp2*sqrt(af*(u-eff))                                                  !/*fp         75*/
        e2=((s-dp1)*exp(s)+dp1)/af                                              !/*fp         76*/
      endif                                                                     !/*fp         77*/
   10 continue                                                                  !/*fp         78*/
      fprob=dp1/(dp1+gamnf)                                                     !/*fp         79*/
   20 continue                                                                  !/*fp         80*/
      return                                                                    !/*fp         81*/
      end                                                                       !/*fp         82*/

!     $Id: setcmg.F,v 1.4 2002/03/27 18:15:23 gwm Exp $                       !/*se          1*/
      subroutine setcmg (ipt,a,z,f)                                             !/*se          2*/
#include <lcs/double.h>                                                         /*!se          3*/
#include <lcs/lptS.h>                                                           /*!se          4*/
#include <lcs/cacom.h>                                                          /*!se          5*/
#include <lcs/cmgcom.h>                                                         /*!se          6*/
      parameter (pi=3.141593d0, tenpi=1.d1*pi, rcomp=1.23d0)                    !/*se          7*/
      dimension flz(6), fla(6), rho0(6)                                         !/*se          8*/
      data flz /dp0,3*dp1,2*dp2/, fla /2*dp1,dp2,2*dp3,dp4/, rho0 /2*dp0        !/*se          9*/
     & ,4*1.2d0/                                                                !/*se         10*/
!                                                                               !/*se         11*/
      ia=int(a)                                                                 !/*se         12*/
      a3=rmass(ia)                                                              !/*se         13*/
      if (ipt.eq.1) then                                                        !/*se         14*/
        ta=a3**2                                                                !/*se         15*/
        lambda=lamda0(1)/a3+lamda1(1)+lamda2(1)*ta+lamda3(1)*ta**2              !/*se         16*/
        mu=mu0(1)*a3+mu1(1)*ta+mu2(1)*ta**2+mu3(1)                              !/*se         17*/
        nu=(nu0(1)*ta+nu1(1))*ta+nu2(1)                                         !/*se         18*/
        if (itt(1).eq.4) then                                                   !/*se         19*/
          p=p0(1)                                                               !/*se         20*/
          ecoul=2.4d0                                                           !/*se         21*/
          q=lambda-nu/ecoul**2-dp2*p*ecoul                                      !/*se         22*/
          r=mu+dp2*nu/ecoul+p*ecoul**2                                          !/*se         23*/
          t0=q**2-dp4*p*r                                                       !/*se         24*/
          t1=-dph*q/p                                                           !/*se         25*/
          if (t0.le.dp0) then                                                   !/*se         26*/
            ezero=max(t1,dp0)                                                   !/*se         27*/
          else                                                                  !/*se         28*/
            t2=dph*sqrt(t0)/p                                                   !/*se         29*/
            eps1=t1+t2                                                          !/*se         30*/
            eps2=t1-t2                                                          !/*se         31*/
            if (p.gt.dp0) then                                                  !/*se         32*/
              ezero=max(eps1,eps2)                                              !/*se         33*/
            elseif (p.lt.dp0) then                                              !/*se         34*/
              ezero=min(eps1,eps2)                                              !/*se         35*/
            else                                                                !/*se         36*/
              ezero=-r/q                                                        !/*se         37*/
            endif                                                               !/*se         38*/
          endif                                                                 !/*se         39*/
        elseif (itt(1).eq.1) then                                               !/*se         40*/
          p=dp0                                                                 !/*se         41*/
          q=dp0                                                                 !/*se         42*/
          r=dp0                                                                 !/*se         43*/
          ezero=max(-dph*(mu-sqrt(mu**2-dp4*nu*lambda))/lambda,dp0)             !/*se         44*/
          ecoul=ezero                                                           !/*se         45*/
        else                                                                    !/*se         46*/
          p=dp0                                                                 !/*se         47*/
          q=dp0                                                                 !/*se         48*/
          r=dp0                                                                 !/*se         49*/
          ecoul=dp0                                                             !/*se         50*/
          ezero=dp0                                                             !/*se         51*/
        endif                                                                   !/*se         52*/
      else                                                                      !/*se         53*/
        ecoul=1.44d0*z*flz(ipt)/(1.5d0*a3+rho0(ipt))                            !/*se         54*/
        lambda=lamda0(ipt)*a+lamda1(ipt)                                        !/*se         55*/
        ta=a**mu1(ipt)                                                          !/*se         56*/
        mu=mu0(ipt)*ta                                                          !/*se         57*/
        nu=ta*(nu0(ipt)+nu1(ipt)*ecoul+nu2(ipt)*ecoul**2)                       !/*se         58*/
        p=p0(ipt)+p1(ipt)/ecoul+p2(ipt)/ecoul**2                                !/*se         59*/
        q=lambda-nu/ecoul**2-dp2*p*ecoul                                        !/*se         60*/
        r=mu+dp2*nu/ecoul+p*ecoul**2                                            !/*se         61*/
        t0=q**2-dp4*p*r                                                         !/*se         62*/
        t1=-dph*q/p                                                             !/*se         63*/
        if (t0.le.dp0) then                                                     !/*se         64*/
          ezero=max(t1,dp0)                                                     !/*se         65*/
        else                                                                    !/*se         66*/
          t2=dph*sqrt(t0)/p                                                     !/*se         67*/
          eps1=t1+t2                                                            !/*se         68*/
          eps2=t1-t2                                                            !/*se         69*/
          if (p.gt.dp0) then                                                    !/*se         70*/
            ezero=max(eps1,eps2)                                                !/*se         71*/
          elseif (p.lt.dp0) then                                                !/*se         72*/
            ezero=min(eps1,eps2)                                                !/*se         73*/
          else                                                                  !/*se         74*/
            ezero=-r/q                                                          !/*se         75*/
          endif                                                                 !/*se         76*/
        endif                                                                   !/*se         77*/
        if (ipt.eq.6.and.itt(6).eq.2) then                                      !/*se         78*/
          lambda=lamda1(6)*exp(lamda0(6)*a/lamda1(6))                           !/*se         79*/
          nu=(lambda-dp2*p*ecoul-q)*ecoul**2                                    !/*se         80*/
          mu=r-p*ecoul**2-dp2*nu/ecoul                                          !/*se         81*/
        endif                                                                   !/*se         82*/
      endif                                                                     !/*se         83*/
      if (emx.le.dp0.or.(ipt.eq.6.and.itt(6).eq.5)) then                        !/*se         84*/
        muprim=dp0                                                              !/*se         85*/
        nuprim=dp0                                                              !/*se         86*/
        ematch=1.d5                                                             !/*se         87*/
      else                                                                      !/*se         88*/
        if (ipt.eq.6.and.itt(6).eq.3) then                                      !/*se         89*/
          muprim=tenpi*(rcomp*a3+rfix1*a3**2+delta(ipt))**2                     !/*se         90*/
        elseif (ipt.eq.6.and.itt(6).eq.4) then                                  !/*se         91*/
          muprim=tenpi*(rcomp*a3+rfix2*a+delta(ipt))**2                         !/*se         92*/
        else                                                                    !/*se         93*/
          muprim=tenpi*(rcomp*a3+delta(ipt))**2                                 !/*se         94*/
        endif                                                                   !/*se         95*/
        ematch=dph*(muprim-mu)/lambda                                           !/*se         96*/
        if (ematch.gt.emx.or.ematch.le.ecoul) then                              !/*se         97*/
          ematch=emx                                                            !/*se         98*/
          ta=p*ecoul**2+q*ecoul+r                                               !/*se         99*/
          tb=dp2*p*ecoul**2+q*ecoul                                             !/*se        100*/
          lambda=dph*(muprim-ta-tb)/(ematch-ecoul)                              !/*se        101*/
          mu=ta+tb-dp2*lambda*ecoul                                             !/*se        102*/
          nu=lambda*ecoul**2-tb*ecoul                                           !/*se        103*/
        endif                                                                   !/*se        104*/
        nuprim=lambda*ematch**2+(mu-muprim)*ematch+nu                           !/*se        105*/
      endif                                                                     !/*se        106*/
      ezero=max(ezero,dp0)                                                      !/*se        107*/
      ecoul=max(ecoul,ezero)                                                    !/*se        108*/
      if (f.ne.dp1) then                                                        !/*se        109*/
        p=p/f**2                                                                !/*se        110*/
        q=q/f                                                                   !/*se        111*/
        lambda=lambda/f                                                         !/*se        112*/
        nu=nu*f                                                                 !/*se        113*/
        nuprim=nuprim*f                                                         !/*se        114*/
        ezero=f*ezero                                                           !/*se        115*/
        ecoul=f*ecoul                                                           !/*se        116*/
        ematch=f*ematch                                                         !/*se        117*/
      endif                                                                     !/*se        118*/
      return                                                                    !/*se        119*/
      end                                                                       !/*se        120*/

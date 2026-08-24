!     $Id: cbreak.F,v 1.4 2002/03/27 18:14:56 gwm Exp $                       !/*cb          1*/
      subroutine cbreak (ipt,ex,n,kip,ilev,t,ki)                                !/*cb          2*/
#include <lcs/double.h>                                                         /*!cb          3*/
!///////////////////////////////////////////////////////////////////////        !/*cb          4*/
!  CBREAK implements the energy-dependent factors in the breakup channel        !/*cb          5*/
!  probabilites for an arbitrary excited nucleus and selects a breakup          !/*cb          6*/
!  channel.                                                                     !/*cb          7*/
!///////////////////////////////////////////////////////////////////////        !/*cb          8*/
#include <lcs/parms.h>                                                          /*!cb          9*/
#include <lcs/fbrcom.h>                                                         /*!cb         10*/
      dimension kip(lchan), ilev(lchan)                                         !/*cb         11*/
      kk=ipdat(ipt)                                                             !/*cb         12*/
      nct=npdat(ipt)                                                            !/*cb         13*/
      if (nct.eq.0) go to 80                                                    !/*cb         14*/
      if (nct.eq.1) then                                                        !/*cb         15*/
        if ((ex+q0(kk)).le.dp0) go to 80                                        !/*cb         16*/
        itr(1)=kk                                                               !/*cb         17*/
        kk=1                                                                    !/*cb         18*/
        ki=1                                                                    !/*cb         19*/
        kj=2                                                                    !/*cb         20*/
        wtt=dp1                                                                 !/*cb         21*/
        wff(1)=dp1                                                              !/*cb         22*/
        go to 60                                                                !/*cb         23*/
      endif                                                                     !/*cb         24*/
      kk=kk-1                                                                   !/*cb         25*/
      k=kk+nct                                                                  !/*cb         26*/
      kj=1                                                                      !/*cb         27*/
      wtt=dp0                                                                   !/*cb         28*/
   10 continue                                                                  !/*cb         29*/
      kk=kk+1                                                                   !/*cb         30*/
      if (kk.gt.k) go to 40                                                     !/*cb         31*/
      t0=ex+q0(kk)                                                              !/*cb         32*/
      if (t0.le.dp0) go to 10                                                   !/*cb         33*/
      ijpp=mod(izaps(kk),1000000)/10000                                         !/*cb         34*/
      n0k=2                                                                     !/*cb         35*/
      if (ijpp.gt.0) n0k=3                                                      !/*cb         36*/
      if (n0k.gt.2) texp=1.5d0*n0k-2.5d0                                        !/*cb         37*/
      itr(kj)=kk                                                                !/*cb         38*/
      w00=w0(kk)                                                                !/*cb         39*/
      coulx=coulch(kk)                                                          !/*cb         40*/
      if (coulx.eq.dp0) go to 30                                                !/*cb         41*/
!     ICMB=5                                                                    !/*cb         42*/
      if (n0k.gt.2) go to 20                                                    !/*cb         43*/
      couly=coulck(kk)                                                          !/*cb         44*/
      tem2=sqrt(t0)                                                             !/*cb         45*/
      eta=coulx/tem2                                                            !/*cb         46*/
      rhox=couly*tem2                                                           !/*cb         47*/
      w00=w00*max(dp1m10,clmb1(rhox,eta,ml))                                    !/*cb         48*/
      go to 30                                                                  !/*cb         49*/
   20 continue                                                                  !/*cb         50*/
!     ICMB=3                                                                    !/*cb         51*/
      tem2=coulx/t0                                                             !/*cb         52*/
      if (tem2.gt.100.d0) tem2=100.d0                                           !/*cb         53*/
      w00=w00*exp(-tem2)                                                        !/*cb         54*/
   30 continue                                                                  !/*cb         55*/
      if (n0k.eq.2) wtt=wtt+w00*sqrt(t0)                                        !/*cb         56*/
      if (n0k.ne.2) wtt=wtt+w00*exp(texp*log(t0))                               !/*cb         57*/
      wff(kj)=wtt                                                               !/*cb         58*/
      kj=kj+1                                                                   !/*cb         59*/
      go to 10                                                                  !/*cb         60*/
   40 continue                                                                  !/*cb         61*/
      if (kj.eq.1) go to 80                                                     !/*cb         62*/
      fran=wtt*rang()                                                           !/*cb         63*/
!                                                                               !/*cb         64*/
!     binary search                                                             !/*cb         65*/
!                                                                               !/*cb         66*/
      ki=kj-1                                                                   !/*cb         67*/
      lo=1                                                                      !/*cb         68*/
      lhi=ki                                                                    !/*cb         69*/
      if (fran.le.wff(1)) lhi=1                                                 !/*cb         70*/
   50 continue                                                                  !/*cb         71*/
      if ((lhi-lo).gt.1) then                                                   !/*cb         72*/
        ll=(lhi+lo)/2                                                           !/*cb         73*/
        if (fran.gt.wff(ll)) then                                               !/*cb         74*/
          lo=ll                                                                 !/*cb         75*/
        else                                                                    !/*cb         76*/
          lhi=ll                                                                !/*cb         77*/
        endif                                                                   !/*cb         78*/
        go to 50                                                                !/*cb         79*/
      endif                                                                     !/*cb         80*/
      kk=lhi                                                                    !/*cb         81*/
!                                                                               !/*cb         82*/
!     decay channel selected                                                    !/*cb         83*/
!                                                                               !/*cb         84*/
   60 continue                                                                  !/*cb         85*/
      i=itr(kk)                                                                 !/*cb         86*/
      q=q0(i)                                                                   !/*cb         87*/
      t=q+ex                                                                    !/*cb         88*/
      iizap=izaps(i)                                                            !/*cb         89*/
      jjix=iizap/1000000                                                        !/*cb         90*/
      jj=jjix/3+1                                                               !/*cb         91*/
      ix=mod(jjix,3)+1                                                          !/*cb         92*/
      kip(3)=mod(iizap,1000000)/10000                                           !/*cb         93*/
      n=2                                                                       !/*cb         94*/
      if (kip(3).gt.0) n=3                                                      !/*cb         95*/
      kip(2)=mod(iizap,10000)/100                                               !/*cb         96*/
      kip(1)=mod(iizap,100)                                                     !/*cb         97*/
      do 70 j=1,n                                                               !/*cb         98*/
      ilev(j)=1                                                                 !/*cb         99*/
   70 continue                                                                  !/*cb        100*/
      ilev(ix)=jj                                                               !/*cb        101*/
      iok1=1                                                                    !/*cb        102*/
      return                                                                    !/*cb        103*/
!                                                                               !/*cb        104*/
!       kj=1; res.nuc. does not have enough ex.en. to breakup                   !/*cb        105*/
!                                                                               !/*cb        106*/
   80 continue                                                                  !/*cb        107*/
      n=1                                                                       !/*cb        108*/
      ki=0                                                                      !/*cb        109*/
      kip(1)=ipt                                                                !/*cb        110*/
      ilev(1)=-1                                                                !/*cb        111*/
      t=dp0                                                                     !/*cb        112*/
      iok1=1                                                                    !/*cb        113*/
      return                                                                    !/*cb        114*/
      end                                                                       !/*cb        115*/

!     $Id: opt5.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                         !/*op          1*/
      subroutine opt5 (tlly,hold,tlsq,cov,ndim)                                 !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/srcwt.h>                                                      /*!op          4*/
#include <htape3x/label.h>                                                      /*!op          5*/
#include <htape3x/contrl.h>                                                     /*!op          6*/
      dimension tlly(1), tlsq(1), cov(2,ndim,1), hold(4,ndim,1)                 !/*op          7*/
      dimension nc4(8)                                                          !/*op          8*/
      dimension dela(lpt), delz(lpt)                                            !/*op          9*/
      data dela /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,2*-1.d0,4*dp0/                 !/*op         10*/
      data delz /dp1,dp0,dp1,dp0,-1.d0,dp1,-1.d0,2*dp1,2*dp2,dp0,dp1,           !/*op         11*/
     &2*dp0,2*-1.d0,dp0,-1.d0,dp1,2*dp0/                                        !/*op         12*/
      data nc4 /0,1,0,0,0,1,0,0/                                                !/*op         13*/
      if (ncol.ne.2.and.ncol.ne.6) go to 70                                     !/*op         14*/
   10 continue                                                                  !/*op         15*/
      if (lelem.eq.-1) go to 70                                                 !/*op         16*/
      if (ifeng.and.(name.ne.1)) go to 70                                       !/*op         17*/
      if (nparm.le.0) go to 30                                                  !/*op         18*/
      if (jmat.eq.0) icomp=bold                                                 !/*op         19*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         20*/
      do 20 ib=1,nparm                                                          !/*op         21*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         22*/
   20 continue                                                                  !/*op         23*/
      go to 70                                                                  !/*op         24*/
   30 continue                                                                  !/*op         25*/
      ib=1                                                                      !/*op         26*/
   40 continue                                                                  !/*op         27*/
      if (lbunch) ib=1                                                          !/*op         28*/
      apr1=dp0                                                                  !/*op         29*/
      apr2=dp0                                                                  !/*op         30*/
      if (mnuc.eq.1.or.mnuc.eq.2) apr2=aprr(1)                                  !/*op         31*/
      if (mnuc.eq.2) apr1=aprr(2)                                               !/*op         32*/
      aprx=apr2+apr1                                                            !/*op         33*/
      if (nabov.eq.0) go to 110                                                 !/*op         34*/
      do 100 i=1,nabov                                                          !/*op         35*/
      itp=tipa(i)+dp1                                                           !/*op         36*/
!     if (jtipa(i).eq.5.or.jtipa(i).eq.6.or.jtipa(i).eq.7) aprx=aprx+           !/*op         37*/
!    & dela(itp)                                                                !/*op         38*/
      if (jtipa(i).eq.2.or.jtipa(i).eq.5.or.jtipa(i).eq.6.or.jtipa(i)           !/*op         39*/
     & .eq.7) aprx=aprx+dela(itp)                                               !/*op         40*/
  100 continue                                                                  !/*op         41*/
  110 continue                                                                  !/*op         42*/
      if (nbelo.eq.0) go to 130                                                 !/*op         43*/
      do 120 i=1,nbelo                                                          !/*op         44*/
      itp=tipb(i)+dp1                                                           !/*op         45*/
!     if (jtipb(i).eq.5.or.jtipb(i).eq.6.or.jtipb(i).eq.7) aprx=aprx+           !/*op         46*/
!    & dela(itp)                                                                !/*op         47*/
      if (jtipb(i).eq.2.or.jtipb(i).eq.5.or.jtipb(i).eq.6.or.jtipb(i)           !/*op         48*/
     & .eq.7) aprx=aprx+dela(itp)                                               !/*op         49*/
  120 continue                                                                  !/*op         50*/
  130 continue                                                                  !/*op         51*/
      ia0=nint(aprx)                                                            !/*op         52*/
      call recels (ielstc)                                                      !/*op         53*/
      if (ielstc.eq.1) go to 70                                                 !/*op         54*/
      do 140 i=1,mnuc                                                           !/*op         55*/
      ia=aprr(i)                                                                !/*op         56*/
      if (ia.eq.0) go to 140                                                    !/*op         57*/
      hold(1,ia,ib)=hold(1,ia,ib)+wt                                            !/*op         58*/
      hold(2,ia,ib)=hold(2,ia,ib)+wt*uur(i)                                     !/*op         59*/
      hold(1,ndim,ib)=hold(1,ndim,ib)+wt                                        !/*op         60*/
      hold(2,ndim,ib)=hold(2,ndim,ib)+wt*uur(i)                                 !/*op         61*/
  140 continue                                                                  !/*op         62*/
      if (ia0.eq.0) go to 70                                                    !/*op         63*/
      hold(3,ia0,ib)=hold(3,ia0,ib)+wt                                          !/*op         64*/
      hold(4,ia0,ib)=hold(4,ia0,ib)+wt*ex                                       !/*op         65*/
      hold(3,ndim,ib)=hold(3,ndim,ib)+wt                                        !/*op         66*/
      hold(4,ndim,ib)=hold(4,ndim,ib)+wt*ex                                     !/*op         67*/
   70 continue                                                                  !/*op         68*/
      call srchn (nc4)                                                          !/*op         69*/
      if (ncol.le.0) go to 80                                                   !/*op         70*/
      if (nocas-nocas0) 80,10,80                                                !/*op         71*/
   80 continue                                                                  !/*op         72*/
      if (lbunch) nup=1                                                         !/*op         73*/
      if (.not.lbunch) nup=max(nparm,1)                                         !/*op         74*/
      fsq=fwti**2                                                               !/*op         75*/
      do 90 ib=1,nup                                                            !/*op         76*/
      do 90 i=1,ndim                                                            !/*op         77*/
      cov(1,i,ib)=cov(1,i,ib)+fsq*hold(1,i,ib)*hold(2,i,ib)                     !/*op         78*/
   90 cov(2,i,ib)=cov(2,i,ib)+fsq*hold(3,i,ib)*hold(4,i,ib)                     !/*op         79*/
      return                                                                    !/*op         80*/
!                                                                               !/*op         81*/
      end                                                                       !/*op         82*/

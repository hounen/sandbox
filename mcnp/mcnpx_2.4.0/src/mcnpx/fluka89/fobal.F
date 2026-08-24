!     $Id: fobal.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                        !/*fo          1*/
      subroutine fobal (ecl,a1,z1,nrj,negzpr,ij,exf)                            !/*fo          2*/
!     IJ      BPART             ILAHET                                          !/*fo          3*/
!                                                                               !/*fo          4*/
!      1     proton              0                                              !/*fo          5*/
!      2     aproton             16                                             !/*fo          6*/
!      3     electron            19                                             !/*fo          7*/
!      4     positron            20                                             !/*fo          8*/
!      5     neutrie             99                                             !/*fo          9*/
!      6     aneutrie            99                                             !/*fo         10*/
!      7     photon              11                                             !/*fo         11*/
!      8     neutron             1                                              !/*fo         12*/
!      9     aneutron            17                                             !/*fo         13*/
!     10     muon+               5                                              !/*fo         14*/
!     11     muon-               6                                              !/*fo         15*/
!     12     kaonlong            13                                             !/*fo         16*/
!     13     pion+               2                                              !/*fo         17*/
!     14     pion-               4                                              !/*fo         18*/
!     15     kaon+               12                                             !/*fo         19*/
!     16     kaon-               15                                             !/*fo         20*/
!     17     lambda              99                                             !/*fo         21*/
!     18     alambda             99                                             !/*fo         22*/
!     19     kaonshrt            14                                             !/*fo         23*/
!     20     sigma-              99                                             !/*fo         24*/
!     21     sigma+              99                                             !/*fo         25*/
!     22     sigmazer            99                                             !/*fo         26*/
!     23     pizero              3                                              !/*fo         27*/
!     24     kaonzero            13                                             !/*fo         28*/
#include <lcs/double.h>                                                         /*!fo         29*/
#include <lcs/lptS.h>                                                           /*!fo         30*/
#include <lcs/rucom.h>                                                          /*!fo         31*/
#include <lcs/cacom.h>                                                          /*!fo         32*/
#include <lcs/banka.h>                                                          /*!fo         33*/
#include <fluka89/paprop.h>                                                     /*!fo         34*/
#include <lcs/fltolh.h>                                                         /*!fo         35*/
!.......................................................................        !/*fo         36*/
      data bindng /7.d0/                                                        !/*fo         37*/
! no energy balancing                                                           !/*fo         38*/
!.......................................................................        !/*fo         39*/
! section for FLUKA model                                                       !/*fo         40*/
!.......................................................................        !/*fo         41*/
      zpr=z1+ichrge(ij)                                                         !/*fo         42*/
      if (ij.eq.1.or.ij.eq.8) then                                              !/*fo         43*/
        apr=a1+dp1                                                              !/*fo         44*/
      elseif (ij.eq.2.or.ij.eq.9) then                                          !/*fo         45*/
        apr=a1-dp1                                                              !/*fo         46*/
      else                                                                      !/*fo         47*/
        apr=a1                                                                  !/*fo         48*/
      endif                                                                     !/*fo         49*/
      a0=apr                                                                    !/*fo         50*/
      z0=zpr                                                                    !/*fo         51*/
      if (nopart.eq.0) go to 20                                                 !/*fo         52*/
      do 10 i=1,nopart                                                          !/*fo         53*/
!REP                                                                            !/*fo         54*/
      lk=kind(i)                                                                !/*fo         55*/
!REP                                                                            !/*fo         56*/
      if (lk.gt.25) call expirx(1,'FOBAL','lk.gt.25')                           !/*fo         57*/
      zpr=zpr-ichrge(lk)                                                        !/*fo         58*/
      if (lk.eq.1.or.lk.eq.8) then                                              !/*fo         59*/
        apr=apr-dp1                                                             !/*fo         60*/
      elseif (lk.eq.2.or.lk.eq.9) then                                          !/*fo         61*/
        apr=apr+dp1                                                             !/*fo         62*/
      endif                                                                     !/*fo         63*/
   10 continue                                                                  !/*fo         64*/
      if (a0.gt.dp1) then                                                       !/*fo         65*/
        bindng=(a0-z0)*energy(dp1,dp0)+z0*energy(dp1,dp1)-energy(a0,z0)         !/*fo         66*/
        bindng=bindng/a0                                                        !/*fo         67*/
      else                                                                      !/*fo         68*/
        bindng=dp0                                                              !/*fo         69*/
      endif                                                                     !/*fo         70*/
      ex=max(exf-(a0-apr)*bindng,dp0)                                           !/*fo         71*/
!.......................................................................        !/*fo         72*/
   20 continue                                                                  !/*fo         73*/
      if (zpr.lt.dp0.or.apr.lt.zpr) then                                        !/*fo         74*/
        negzpr=negzpr+1                                                         !/*fo         75*/
        negaz=negaz+1                                                           !/*fo         76*/
        nrj=1                                                                   !/*fo         77*/
      elseif ((apr.eq.zpr.or.zpr.eq.dp0).and.apr.gt.dp3) then                   !/*fo         78*/
        lowaz=lowaz+1                                                           !/*fo         79*/
        nrj=1                                                                   !/*fo         80*/
      else                                                                      !/*fo         81*/
        if (apr.le.dp1) nrnorm(4)=nrnorm(4)+1                                   !/*fo         82*/
!L    ==================================================================        !/*fo         83*/
!L        if (lelem.eq.-1) call revnt (3)                                       !/*fo         84*/
!L        if (lelem.ne.-1) call revnt (1)                                       !/*fo         85*/
!L    ==================================================================        !/*fo         86*/
        if (apr.gt.dp1) then                                                    !/*fo         87*/
          waz=931.162d0*apr+ex                                                  !/*fo         88*/
          call frecol (ecl,waz,ij,1,nrj)                                        !/*fo         89*/
          ex=max(ex-erec,dp0)                                                   !/*fo         90*/
        elseif (apr.eq.dp1) then                                                !/*fo         91*/
          waz=931.162d0*apr                                                     !/*fo         92*/
          call frecol (ecl,waz,ij,0,nrj)                                        !/*fo         93*/
          ex=dp0                                                                !/*fo         94*/
        else                                                                    !/*fo         95*/
          ex=dp0                                                                !/*fo         96*/
          erec=dp0                                                              !/*fo         97*/
        endif                                                                   !/*fo         98*/
      endif                                                                     !/*fo         99*/
      if (nrj.ne.0) go to 40                                                    !/*fo        100*/
      nprt=0                                                                    !/*fo        101*/
      do 30 i=1,nopart                                                          !/*fo        102*/
      kd=ilahet(kind(i))                                                        !/*fo        103*/
      if (kd.le.lpt) then                                                       !/*fo        104*/
        nprt=nprt+1                                                             !/*fo        105*/
        kind(nprt)=kd                                                           !/*fo        106*/
        alpha(nprt)=alpha(i)                                                    !/*fo        107*/
        beta(nprt)=beta(i)                                                      !/*fo        108*/
        gam(nprt)=gam(i)                                                        !/*fo        109*/
        ep(nprt)=ep(i)                                                          !/*fo        110*/
      endif                                                                     !/*fo        111*/
   30 continue                                                                  !/*fo        112*/
      nopart=nprt                                                               !/*fo        113*/
      if (apr.eq.dp1) then                                                      !/*fo        114*/
        nopart=nopart+1                                                         !/*fo        115*/
        ep(nopart)=erec                                                         !/*fo        116*/
        kind(nopart)=dp1-zpr                                                    !/*fo        117*/
        alpha(nopart)=coslbr(1)                                                 !/*fo        118*/
        beta(nopart)=coslbr(2)                                                  !/*fo        119*/
        gam(nopart)=coslbr(3)                                                   !/*fo        120*/
        ex=dp0                                                                  !/*fo        121*/
        erec=dp0                                                                !/*fo        122*/
        ex2=dp0                                                                 !/*fo        123*/
        apr=dp0                                                                 !/*fo        124*/
        zpr=dp0                                                                 !/*fo        125*/
      endif                                                                     !/*fo        126*/
   40 continue                                                                  !/*fo        127*/
      return                                                                    !/*fo        128*/
      end                                                                       !/*fo        129*/

!     $Id: fcasc.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                        !/*fc          1*/
      subroutine fcasc (ecl,a1,z1,geom_xs,lmxs_)                                !/*fc          2*/
!     ==================================================================        !/*fc          3*/
!     bcasc    calls the following subroutines and functions                    !/*fc          4*/
!        bert       energy     error      nobal      skale      rebal           !/*fc          5*/
!     ==================================================================        !/*fc          6*/
#include <lcs/double.h>                                                         /*!fc          7*/
      parameter (pi=3.141593d0, hbar=6.581d-22, clite=2.9979d+10, cmtofm        !/*fc          8*/
     & =1.0d-13, fsc=dp1/137.04d0, hbarc=hbar*clite, rcoul=1.70d0, conce        !/*fc          9*/
     & =fsc*hbarc/(rcoul*cmtofm), conar=dph*fsc*cmtofm*rcoul/hbarc,             !/*fc         10*/
     & cmtomb=1.0d+27, conem=dp1/(cmtomb*hbar*(pi*hbarc)**2), conit=pi          !/*fc         11*/
     & /hbar, conrad=hbarc/cmtofm, sqrt2=1.414214d0, amu=931.20793d0,           !/*fc         12*/
     & fmtomb=1.d-1, tenpi=dp10*pi)                                             !/*fc         13*/
#include <lcs/inout.h>                                                          /*!fc         14*/
#include <lcs/lptS.h>                                                           /*!fc         15*/
#include <lcs/rucom.h>                                                          /*!fc         16*/
#include <lcs/cacom.h>                                                          /*!fc         17*/
#include <lcs/banka.h>                                                          /*!fc         18*/
!REP - SOPP                                                                     !/*fc         19*/
#include <fluka89/countq.h>                                                     /*!fc         20*/
!REP - SOPP                                                                     !/*fc         21*/
      dimension iix(6)                                                          !/*fc         22*/
      data iix /1,0,7,8,9,10/, maxstg /10/                                      !/*fc         23*/
      data limit /100/                                                          !/*fc         24*/
      data amumev /931.20793d0/                                                 !/*fc         25*/
      nrj=0                                                                     !/*fc         26*/
      nnuc=-1                                                                   !/*fc         27*/
      negzpr=0                                                                  !/*fc         28*/
!     nrjpi=0                                                                   !/*fc         29*/
   10 continue                                                                  !/*fc         30*/
!     fluka interface routine for lahet                                         !/*fc         31*/
!REP - SOPP                                                                     !/*fc         32*/
      sopp(1)=dp0                                                               !/*fc         33*/
      sopp(2)=dp0                                                               !/*fc         34*/
!REP - SOPP                                                                     !/*fc         35*/
      call fluker (a1,z1,ityp,nopart,ecl,kind,ep,alpha,beta,gam,ij,exf          !/*fc         36*/
     & ,geom_xs,lmxs_)                                                          !/*fc         37*/
      if (nopart.eq.1) then                                                     !/*fc         38*/
        if (ityp.eq.1.and.kind(1).eq.8) then                                    !/*fc         39*/
          ep(1)=ep(1)-(energy(a1,z1+dp1)+energy(dp1,dp0)-energy(dp1,dp1)        !/*fc         40*/
     &    -energy(a1,z1))                                                       !/*fc         41*/
        elseif (ityp.eq.2.and.kind(2).eq.1) then                                !/*fc         42*/
          ep(1)=ep(1)-(energy(a1,z1-dp1)+energy(dp1,dp1)-energy(dp1,dp0)        !/*fc         43*/
     &    -energy(a1,z1))                                                       !/*fc         44*/
        endif                                                                   !/*fc         45*/
      endif                                                                     !/*fc         46*/
      if (nopart.ge.0) then                                                     !/*fc         47*/
        nrj=0                                                                   !/*fc         48*/
        call fobal (ecl,a1,z1,nrj,negzpr,ij,exf)                                !/*fc         49*/
        iprqa=ipreq                                                             !/*fc         50*/
        if (nrj.gt.0) go to 10                                                  !/*fc         51*/
        if (iprqa.ne.0) go to 20                                                !/*fc         52*/
        go to 40                                                                !/*fc         53*/
      endif                                                                     !/*fc         54*/
!.......................................................................        !/*fc         55*/
! no interaction - pseudocollision                                              !/*fc         56*/
      if (nrj.eq.1) go to 10                                                    !/*fc         57*/
      go to 40                                                                  !/*fc         58*/
!.......................................................................        !/*fc         59*/
! preequilibrium                                                                !/*fc         60*/
   20 continue                                                                  !/*fc         61*/
      if (apr.le.dp5) go to 40                                                  !/*fc         62*/
      if (ityp.gt.2.or.ityp.lt.1) go to 40                                      !/*fc         63*/
      if (ex.le.dp0) go to 40                                                   !/*fc         64*/
!                                                                               !/*fc         65*/
!     initialize emitting nucleus - preeq                                       !/*fc         66*/
!                                                                               !/*fc         67*/
      exc0=ex                                                                   !/*fc         68*/
      ja=apr                                                                    !/*fc         69*/
      jz=zpr                                                                    !/*fc         70*/
      ex0=energy(apr,zpr)                                                       !/*fc         71*/
      waz=ex0+amumev*apr                                                        !/*fc         72*/
      gamma9=dp1+erec/(waz+ex)                                                  !/*fc         73*/
      ipr0=3-ityp                                                               !/*fc         74*/
      nnout=a1+ia6(ipr0)-apr+1.d-02                                             !/*fc         75*/
      npiout=nopart-nnout                                                       !/*fc         76*/
      nstage=max(nnout,npiout)                                                  !/*fc         77*/
      if (nstage.eq.0) then                                                     !/*fc         78*/
        rnmas0=energy(a1,z1)+amumev*a1                                          !/*fc         79*/
        temp=sqrt((rnmas0+zmass9(ipr0))**2+dp2*rnmas0*ecl)                      !/*fc         80*/
        eps0=temp-rnmas0-zmass9(ipr0)                                           !/*fc         81*/
      endif                                                                     !/*fc         82*/
      np=ia6(ipr0)+1-nnout                                                      !/*fc         83*/
      if (nopart.eq.0.and.iprqa.eq.3) then                                      !/*fc         84*/
        nh=1                                                                    !/*fc         85*/
      else                                                                      !/*fc         86*/
        if (np.le.0) then                                                       !/*fc         87*/
          nh=2-np                                                               !/*fc         88*/
          np=1                                                                  !/*fc         89*/
        else                                                                    !/*fc         90*/
          nh=2                                                                  !/*fc         91*/
          np=np+1                                                               !/*fc         92*/
        endif                                                                   !/*fc         93*/
      endif                                                                     !/*fc         94*/
   30 continue                                                                  !/*fc         95*/
!                                                                               !/*fc         96*/
!     attempt emission                                                          !/*fc         97*/
!                                                                               !/*fc         98*/
      nstage=nstage+1                                                           !/*fc         99*/
      ipr=0                                                                     !/*fc        100*/
      call pclct (apr,zpr,ja,jz,ex,np,nh,a1,z1,ipr0,erec)                       !/*fc        101*/
      if (ipr.eq.0) return                                                      !/*fc        102*/
!                                                                               !/*fc        103*/
!         binning emitted particle                                              !/*fc        104*/
!                                                                               !/*fc        105*/
      nopart=nopart+1                                                           !/*fc        106*/
      i=nopart                                                                  !/*fc        107*/
      ep(i)=eps9                                                                !/*fc        108*/
      alpha(i)=coslbp(1)                                                        !/*fc        109*/
      beta(i)=coslbp(2)                                                         !/*fc        110*/
      gam(i)=coslbp(3)                                                          !/*fc        111*/
      kind(i)=iix(ipr)                                                          !/*fc        112*/
      if (nstage.lt.maxstg) go to 30                                            !/*fc        113*/
      return                                                                    !/*fc        114*/
!.......................................................................        !/*fc        115*/
   40 continue                                                                  !/*fc        116*/
      exc0=ex                                                                   !/*fc        117*/
      return                                                                    !/*fc        118*/
!                                                                               !/*fc        119*/
      end                                                                       !/*fc        120*/

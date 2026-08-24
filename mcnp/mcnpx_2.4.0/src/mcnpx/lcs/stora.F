!     $Id: stora.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                        !/*st          1*/
      subroutine stora (n,arev,zrev,sigtrg)                                     !/*st          2*/
! STORA calls ERPRNT  ENERGY  DEN1                                              !/*st          3*/
!     STORA called by XIN                                                       !/*st          4*/
#include <lcs/double.h>                                                         /*!st          5*/
!-----------------------------------------------------------------------        !/*st          6*/
!     Indexing: IPT = 1 for standard data, IPT = 2 for anti-nucleon             !/*st          7*/
!               N =  isotope number                                             !/*st          8*/
!     CSA array:                                                                !/*st          9*/
!          CSA(1): projectile indentifier (+1 for nucleon, -1 for               !/*st         10*/
!                anti-nucleon in STORA)                                         !/*st         11*/
!                    +/- 1 = proton (antiproton)                                !/*st         12*/
!                    +/- 2 = neutron (antineutron)                              !/*st         13*/
!                    3,4,5 = pion ( + , 0 , - )                                 !/*st         14*/
!                    7,8,9 = kaon ( + , 0 , - )                                 !/*st         15*/
!          CSA(2): incident energy / nucleon (relativistic                      !/*st         16*/
!               including rest mass)                                            !/*st         17*/
!          CSA(3): A of target                                                  !/*st         18*/
!          CSZ(4): Z of target                                                  !/*st         19*/
!          CSA(5): average binding energy of a nucleon in the target            !/*st         20*/
!          CSA(6): not used                                                     !/*st         21*/
!          CSA(7): Z-A of target                                                !/*st         22*/
!          CSA(8): Z/A of target                                                !/*st         23*/
!          CSA(9): nuclear radius R of target                                   !/*st         24*/
!          CSA(10): nuclear radius R squared of target                          !/*st         25*/
!          CSA(11) initailly: Coulomb barrier parameter for target              !/*st         26*/
!              (1.44Z/R) or target+projectile                                   !/*st         27*/
!          CSA(12): set to 1 when cascade is finished                           !/*st         28*/
!          CSA(13): ratio of antiproton to proton potential depth               !/*st         29*/
!          CSA(14): characteristic time of an incident particle                 !/*st         30*/
!          CSA(15): average binding energy of a nucleon in the projectil        !/*st         31*/
!-----------------------------------------------------------------------        !/*st         32*/
#include <lcs/vegas.h>                                                          /*!st         33*/
      dimension zrev(liso), arev(liso), sigtrg(6,liso)                          !/*st         34*/
      dimension efx(34), pfrx(34), pfwx(34)                                     !/*st         35*/
      equivalence (ef,efx), (pfr,pfrx), (pfw,pfwx)                              !/*st         36*/
      data pi /3.141592d0/                                                      !/*st         37*/
      atg=arev(n)                                                               !/*st         38*/
      zt=zrev(n)                                                                !/*st         39*/
      csa(3)=atg                                                                !/*st         40*/
      csa(4)=zt                                                                 !/*st         41*/
      at1=atg-dp1                                                               !/*st         42*/
      zt1=zt-dp1                                                                !/*st         43*/
      if(n.gt.liso)call erprnt(1,1,0,0,0,0,0,0,'17hSTORA.  n.gt.liso')          !/*st         44*/
      if (atg.le.dp3) then                                                      !/*st         45*/
        csa(5)=(zt*energy(dp1,dp1)+(atg-zt)*energy(dp1,dp0)-energy(atg          !/*st         46*/
     &  ,zt))/atg                                                               !/*st         47*/
      else                                                                      !/*st         48*/
        csa(5)=dph*(energy(dp1,dp1)+energy(at1,zt1)+energy(dp1,dp0)             !/*st         49*/
     &  +energy(at1,zt))-energy(atg,zt)                                         !/*st         50*/
      endif                                                                     !/*st         51*/
      do 70 ipt=1,2                                                             !/*st         52*/
      csa(1)=dp3-dp2*ipt                                                        !/*st         53*/
      if (iden.lt.2) go to 10                                                   !/*st         54*/
      dxp(1)=-1.5d0-ipt                                                         !/*st         55*/
      if (atg.le.dp1) dxp(1)=dp0                                                !/*st         56*/
      go to 20                                                                  !/*st         57*/
   10 continue                                                                  !/*st         58*/
      dx(1)=-1.5d0-ipt                                                          !/*st         59*/
      if (atg.le.dp1) dx(1)=dp0                                                 !/*st         60*/
   20 continue                                                                  !/*st         61*/
      call den1                                                                 !/*st         62*/
      do 30 i=3,13                                                              !/*st         63*/
   30 hcsa(i,ipt,n)=csa(i)                                                      !/*st         64*/
      inine=ieght+1                                                             !/*st         65*/
      inine2=2*inine                                                            !/*st         66*/
      do 40 i=1,inine2                                                          !/*st         67*/
      hef(i,ipt,n)=efx(i)                                                       !/*st         68*/
      hpfr(i,ipt,n)=pfrx(i)                                                     !/*st         69*/
      hpfw(i,ipt,n)=pfwx(i)                                                     !/*st         70*/
   40 continue                                                                  !/*st         71*/
      do 50 i=1,inine                                                           !/*st         72*/
   50 hx(i,ipt,n)=xa(i)                                                         !/*st         73*/
      do 60 i=1,ieght                                                           !/*st         74*/
   60 hden(i,ipt,n)=den_(i)                                                     !/*st         75*/
   70 continue                                                                  !/*st         76*/
      sigtrg(1,n)=1.d-02*pi*hx(1,1,n)**2                                        !/*st         77*/
      sigtrg(2,n)=1.d-02*pi*hx(1,2,n)**2                                        !/*st         78*/
      if (n.lt.2) go to 90                                                      !/*st         79*/
      nup=min(n,5)                                                              !/*st         80*/
      do 80 i=2,nup                                                             !/*st         81*/
      rr=hx(1,1,n)+hx(1,1,i)                                                    !/*st         82*/
      sigtrg(i+1,n)=1.d-02*pi*rr**2                                             !/*st         83*/
      if (n.le.5) then                                                          !/*st         84*/
        j=i                                                                     !/*st         85*/
        if (j.eq.2) j=1                                                         !/*st         86*/
        sigtrg(n+1,i-1)=sigtrg(j,n)                                             !/*st         87*/
      endif                                                                     !/*st         88*/
   80 continue                                                                  !/*st         89*/
   90 continue                                                                  !/*st         90*/
      return                                                                    !/*st         91*/
      end                                                                       !/*st         92*/

!     $Id: plev.F,v 1.5 2002/03/27 18:15:16 gwm Exp $                         !/*pl          1*/
      subroutine plev (e,izt,ia,iflag)                                          !/*pl          2*/
!                                                                               !/*pl          3*/
!     calculates level density model parameters                                 !/*pl          4*/
!                                                                               !/*pl          5*/
!         izt - nucleus charge number                                           !/*pl          6*/
!         ia  - nucleus mass number                                             !/*pl          7*/
!         is - shape control parameter                                          !/*pl          8*/
!              1 - spherical    2 - deformed                                    !/*pl          9*/
!                1, gilbert and cameron                                         !/*pl         10*/
!                2,some g+c parameters are modified                             !/*pl         11*/
!                3,fermi gas level density only, with                           !/*pl         12*/
!                  parameter modification allowed                               !/*pl         13*/
!                                                                               !/*pl         14*/
#include <lcs/double.h>                                                         /*!pl         15*/
#include <lcs/phtcom.h>                                                         /*!pl         16*/
#include <lcs/inout.h>                                                          /*!pl         17*/
      parameter (inn=150, izz=98)                                               !/*pl         18*/
      dimension pz(izz), pn(inn)                                                !/*pl         19*/
!                                                                               !/*pl         20*/
!     data tables of cook et. al.  aaec/tm392, supplimented by g+c.             !/*pl         21*/
!                                                                               !/*pl         22*/
      data pz /dp0,5.44d0,dp0,2.76d0,dp0,3.34d0,dp0,2.7d0,dp0,2.5d0,dp0,        !/*pl         23*/
     & 2.46d0,dp0,2.09d0,dp0,1.62d0,dp0,1.62d0,dp0,1.83d0,dp0,1.73d0,dp0        !/*pl         24*/
     & ,1.35d0,dp0,1.54d0,dp0,1.28d0,0.26d0,0.88d0,0.19d0,1.35d0,-.05d0,        !/*pl         25*/
     & 1.52d0,-.09d0,1.17d0,.04d0,1.24d0,0.29d0,1.09d0,.26d0,1.17d0,.23d        !/*pl         26*/
     & 0,1.15d0,-.08d0,1.35d0,0.34d0,1.05d0,.28d0,1.27d0,dp0,1.05d0,dp0,        !/*pl         27*/
     & 1.d0,.09d0,1.2d0,.2d0,1.4d0,.93d0,1.d0,-.2d0,1.19d0,.09d0,.97d0          !/*pl         28*/
     & ,dp0,.92d0,.11d0,.68d0,.05d0,.68d0,-.22d0,.79d0,.09d0,.69d0,.01d0        !/*pl         29*/
     & ,.72d0,dp0,.4d0,.16d0,.73d0,dp0,.46d0,.17d0,.89d0,dp0,.79d0,dp0,.        !/*pl         30*/
     & 89d0,dp0,.81d0,-.06d0,.69d0,-.2d0,.71d0,-.12d0,.72d0,dp0,.77d0/          !/*pl         31*/
      data (pn(i),i=1,125) /dp0,5.98d0,dp0,2.77d0,dp0,3.16d0,dp0,3.01d0         !/*pl         32*/
     & ,dp0,2.5d0,dp0,2.67d0,dp0,1.8d0,dp0,1.67d0,dp0,1.86d0,dp0,2.04d0         !/*pl         33*/
     & ,dp0,1.64d0,dp0,1.44d0,dp0,1.54d0,dp0,1.3d0,dp0,1.27d0,dp0,1.29d0        !/*pl         34*/
     & ,.08d0,1.41d0,-.08d0,1.5d0,-.05d0,2.24d0,-.47d0,1.43d0,-.15d0,1.4        !/*pl         35*/
     & 4d0,.06d0,1.56d0,.25d0,1.57d0,-.16d0,1.46d0,dp0,.93d0,.01d0,.62d0        !/*pl         36*/
     & ,-.5d0,1.42d0,.13d0,1.52d0,-.65d0,.8d0,-.08d0,1.29d0,-.47d0,1.25d        !/*pl         37*/
     & 0,-.44d0,.97d0,.08d0,1.65d0,-.11d0,1.26d0,-.46d0,1.06d0,0.22d0,1.        !/*pl         38*/
     & 55d0,-.07d0,1.37d0,0.1d0,1.2d0,-.27d0,.92d0,-.35d0,1.19d0,dp0,1.0        !/*pl         39*/
     & 5d0,-.25d0,1.61d0,-.21d0,.9d0,-.21d0,.74d0,-.38d0,.72d0,-.34d0,.9        !/*pl         40*/
     & 2d0,-.26d0,.94d0,.01d0,.65d0,-.36d0,.83d0,.11d0,.67d0,.05d0,1.d0,        !/*pl         41*/
     & .51d0,1.04d0,.33d0,.68d0,-.27d0,.81d0,.09d0,.75d0,.17d0,.86d0,.14        !/*pl         42*/
     & d0,1.1d0,-.22d0,.84d0,-.47d0,.48d0,.02d0,.88d0,.24d0,.52d0,.27d0,        !/*pl         43*/
     & .41d0,-.05/                                                              !/*pl         44*/
      data (pn(i),i=126,150) /0.38d0,.15d0,.67d0,dp0,.61d0,dp0,.78d0,dp0        !/*pl         45*/
     & ,.67d0,dp0,.67d0,dp0,.79d0,dp0,.6d0,.04d0,.64d0,-.06d0,.45d0,.05d        !/*pl         46*/
     & 0,.26d0,-.22d0,.39d0,dp0,.39d0/                                          !/*pl         47*/
!                                                                               !/*pl         48*/
!     calculate pairing energy and parameter 'aa'                               !/*pl         49*/
      if (izt.gt.izz) write (io,10) izt                                         !/*pl         50*/
      iz=min(izt,izz)                                                           !/*pl         51*/
      jn=ia-iz                                                                  !/*pl         52*/
      if (jn.gt.inn) write (io,20) jn                                           !/*pl         53*/
      jn=min(jn,inn)                                                            !/*pl         54*/
      am=ia                                                                     !/*pl         55*/
      de=pz(iz)+pn(jn)                                                          !/*pl         56*/
      aa=geta(e,iz,jn,ilvdn,is)                                                 !/*pl         57*/
!     calculate constant jn sigma correlation                                   !/*pl         58*/
      am3=am**.33333333                                                         !/*pl         59*/
      temp=1.58193/am3                                                          !/*pl         60*/
!     calculate energy boundary between two level density functions             !/*pl         61*/
      ux=2.5+150./am                                                            !/*pl         62*/
      exx=ux+de                                                                 !/*pl         63*/
!     calculate low energy temperature                                          !/*pl         64*/
      tn=sqrt(aa/ux)-1.5/ux                                                     !/*pl         65*/
      tn=dp1/tn                                                                 !/*pl         66*/
      c2ssq=dp2*sqrt(aa*ux)                                                     !/*pl         67*/
      p2ux=temp*aa*exp(c2ssq)/c2ssq**3                                          !/*pl         68*/
      if (iflag.eq.0) return                                                    !/*pl         69*/
      p1pe=p2ux*exp(-ux/tn)                                                     !/*pl         70*/
      e0=exx-tn*log(tn*p2ux)                                                    !/*pl         71*/
      return                                                                    !/*pl         72*/
!                                                                               !/*pl         73*/
   10 format (//5x,'z = ',i6,                                                   !/*pl         74*/
     & ' is not contained within internal level density tables.'/)              !/*pl         75*/
   20 format (/5x,'neutron number = ',i6,                                       !/*pl         76*/
     & ' is not contained within internal level density tables.'//)             !/*pl         77*/
      end                                                                       !/*pl         78*/

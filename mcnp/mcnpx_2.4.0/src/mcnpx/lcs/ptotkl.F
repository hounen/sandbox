!     $Id: ptotkl.F,v 1.5 2002/03/27 18:15:17 gwm Exp $                       !/*pt          1*/
      subroutine ptotkl (cp,typ,scp,scn,abp,abn)                                !/*pt          2*/
#include <lcs/double.h>                                                         /*!pt          3*/
!     ==================================================================        !/*pt          4*/
!     ptotkl, given p(beam), will return the total i-i or i-j                   !/*pt          5*/
!     ka-nucleon cross section for that energy and the absorption               !/*pt          6*/
!     cross section as well as the pi0-nucleon cross section.                   !/*pt          7*/
!     ind9=1 means an i-j collision, ind10=1 means an i-i collision,            !/*pt          8*/
!     and ind11=1 means an i-0 collisision                                      !/*pt          9*/
!                                                                               !/*pt         10*/
! PTOTKL called by AMFP  KRANCH  CASCAS  PSEL                                   !/*pt         11*/
!     ==================================================================        !/*pt         12*/
!                                                                               !/*pt         13*/
!            values, in 100 mev/c steps, are averages of numbers                !/*pt         14*/
!            taken from price, et al "compilation of k+n rxns",                 !/*pt         15*/
!            ucrl-20000, berkeley, sept 1969.                                   !/*pt         16*/
!                                                                               !/*pt         17*/
!            these are the sig(total) values, although at circa                 !/*pt         18*/
!            1 gev/c, the kp-->knpi rxn opens up and amounts                    !/*pt         19*/
!            to 1/2 of sig(tot) for the next gev/c                              !/*pt         20*/
!                                                                               !/*pt         21*/
!            k+n values (from k+ d data) exist from 770 mev/c,                  !/*pt         22*/
!             below that, s(k+ n) = s(k+ p) + 3 mb, and                         !/*pt         23*/
!             s(k0 n,p)= 0.5 * (s(k+ p) + s(k+ n))                              !/*pt         24*/
!                                                                               !/*pt         25*/
!     dimension scaii(12), scaij(12), scao(12)                                  !/*pt         26*/
!     sigma (k+,p),(k-,n)                                                       !/*pt         27*/
!     data scaii/8.,10.5,11.0,11.5,12.0,12.2,12.3,12.0,13.0,                    !/*pt         28*/
!    &     14.9,16.5,17.5 /                                                     !/*pt         29*/
!      sigma (k+,n),(k-,p)                                                      !/*pt         30*/
!     data scaij/11.,13.5,14.0,14.5,15.,15.2,15.3,15.0,                         !/*pt         31*/
!    &     16.3,17.7,19.5,20.5 /                                                !/*pt         32*/
!     data scao /9.5,12.0,12.5,13.0,13.5,13.7,13.8,13.5,                        !/*pt         33*/
!    &     14.5,16.4,18.0,19.0 /                                                !/*pt         34*/
! above tables are based on cern/hera 69-1 report                               !/*pt         35*/
! above tables are good for c.m.k.e. =660mev or lab.pi k.e.=990mev              !/*pt         36*/
!     or lab pi momentum = 1.12 gev/c                                           !/*pt         37*/
!                                                                               !/*pt         38*/
!      recent cross-sections based on particle properties data booklet          !/*pt         39*/
!            pp 130-135                                                         !/*pt         40*/
!                                                                               !/*pt         41*/
#include <lcs/inout.h>                                                          /*!pt         42*/
      dimension scpn(13), scpp(13), scmp(13), scmn(13), sc0p(13), sc0n(1        !/*pt         43*/
     & 3)                                                                       !/*pt         44*/
!                                                                               !/*pt         45*/
      data scpn /7*15.d0,16.d0,17.d0,19.d0,20.d0,21.d0,20.d0/                   !/*pt         46*/
      data scpp /7*12.d0,13.d0,15.d0,16.d0,17.d0,18.d0,17.d0/                   !/*pt         47*/
      data scmp /90.d0,70.d0,80.d0,70.d0,55.d0,45.d0,35.d0,45.d0,45.d0,5        !/*pt         48*/
     & 0.d0,45.d0,40.d0,35.d0/                                                  !/*pt         49*/
      data scmn /5*24.d0,26.d0,30.d0,30.d0,35.d0,40.d0,35.d0,33.d0,30.d0        !/*pt         50*/
     & /                                                                        !/*pt         51*/
      data sc0p /51.d0,41.d0,46.d0,41.d0,34.d0,29.d0,24.d0,29.d0,30.d0,3        !/*pt         52*/
     & 3.d0,31.d0,29.d0,26.d0/                                                  !/*pt         53*/
      data sc0n /5*20.d0,21.d0,22.d0,23.d0,26.d0,26.d0,28.d0,27.d0,25.d0        !/*pt         54*/
     & /                                                                        !/*pt         55*/
!                                                                               !/*pt         56*/
!        above data is for (plus, minus, or neutral) kaons on                   !/*pt         57*/
!        protons and neutrons respectively, in 100 mev/c steps                  !/*pt         58*/
!            program changed thusly 12 mar 83 [mrc]                             !/*pt         59*/
!                                                                               !/*pt         60*/
      if (cp.lt.1.2d+03) go to 10                                               !/*pt         61*/
      write (io,50) cp                                                          !/*pt         62*/
      cp=1199.d0                                                                !/*pt         63*/
   10 abp=dp0                                                                   !/*pt         64*/
      abn=dp0                                                                   !/*pt         65*/
! the following is based on fact that above data are at equally                 !/*pt         66*/
! spaced energy intervals of 100 mev/c                                          !/*pt         67*/
      rie=1.d-2*cp                                                              !/*pt         68*/
      ie=rie                                                                    !/*pt         69*/
      ie=ie+1                                                                   !/*pt         70*/
      de=rie-real(ie-1)                                                         !/*pt         71*/
      ae=dp1-de                                                                 !/*pt         72*/
!                                                                               !/*pt         73*/
      if (typ) 40,30,20                                                         !/*pt         74*/
   20 scp=ae*scmp(ie)+de*scmp(ie+1)                                             !/*pt         75*/
      scn=ae*scmn(ie)+de*scmn(ie+1)                                             !/*pt         76*/
      return                                                                    !/*pt         77*/
!                                                                               !/*pt         78*/
   30 scp=ae*sc0p(ie)+de*sc0p(ie+1)                                             !/*pt         79*/
      scn=ae*sc0n(ie)+de*sc0n(ie+1)                                             !/*pt         80*/
      return                                                                    !/*pt         81*/
!                                                                               !/*pt         82*/
   40 scp=ae*scpp(ie)+de*scpp(ie+1)                                             !/*pt         83*/
      scn=ae*scpn(ie)+de*scpn(ie+1)                                             !/*pt         84*/
      return                                                                    !/*pt         85*/
!                                                                               !/*pt         86*/
   50 format ('0momenta=',f10.4,' too large (>1199.) for ptotkl')               !/*pt         87*/
      end                                                                       !/*pt         88*/

!     $Id: ptotal.F,v 1.4 2002/03/27 18:15:17 gwm Exp $                       !/*pt          1*/
      subroutine ptotal (e,ind9,ind10,ind11,scatt,absor)                        !/*pt          2*/
#include <lcs/double.h>                                                         /*!pt          3*/
!     ==================================================================        !/*pt          4*/
!     ptotal, given the kinetic energy in the c.m.routine will return           !/*pt          5*/
!     the total i-i or i-j pi-nucleon cross section for that energy             !/*pt          6*/
!     and the absorption cross section as well as the pi0-nucleon               !/*pt          7*/
!     cross section.  ind9=1 means an i-j collision,ind10=1 means an            !/*pt          8*/
!     i-i collision, and ind11=1 means an i-0 collisision                       !/*pt          9*/
!                                                                               !/*pt         10*/
! PTOTAL called by AMFP  TWOPIN  CASCAS  ONEPIN  PIN1PI  PSEL                   !/*pt         11*/
!     ==================================================================        !/*pt         12*/
!                                                                               !/*pt         13*/
#include <lcs/inout.h>                                                          /*!pt         14*/
      dimension scaii(67), scaij(67), scao(67)                                  !/*pt         15*/
      data scaii /dp0,2.d0,4.d0,6.d0,12.d0,19.d0,29.d0,42.d0,58.d0,77.d0        !/*pt         16*/
     & ,99.d0,129.d0,159.d0,179.d0,191.d0,195.d0,185.d0,162.d0,143.d0,12        !/*pt         17*/
     & 4.d0,109.d0,95.d0,83.d0,73.d0,64.d0,56.d0,50.d0,44.d0,40.d0,36.d0        !/*pt         18*/
     & ,33.d0,31.d0,29.d0,27.d0,25.d0,24.d0,22.d0,21.d0,20.d0,19.d0,18          !/*pt         19*/
     & .d0,17.d0,17.d0,16.d0,16.d0,16.d0,16.d0,16.d0,17.d0,17.d0,18.d0,1        !/*pt         20*/
     & 8.d0,19.d0,20.d0,21.d0,22.d0,23.d0,24.d0,24.d0,25.d0,25.d0,26.d0,        !/*pt         21*/
     & 26.d0,26.d0,27.d0,27.d0,27.d0/                                           !/*pt         22*/
      data scaij /dp0,2.d0,4.d0,6.d0,8.d0,11.d0,14.d0,18.d0,22.d0,29.d0,        !/*pt         23*/
     & 37.d0,47.d0,55.d0,62.d0,66.d0,67.d0,65.d0,59.d0,52.d0,44.d0,39.d0        !/*pt         24*/
     & ,36.d0,33.d0,31.d0,29.d0,28.d0,27.d0,27.d0,27.d0,27.d0,27.d0,27          !/*pt         25*/
     & .d0,27.d0,28.d0,29.d0,30.d0,31.d0,33.d0,34.d0,36.d0,38.d0,42.d0,4        !/*pt         26*/
     & 5.d0,47.d0,48.d0,47.d0,45.d0,42.d0,40.d0,38.d0,38.d0,38.d0,39.d0,        !/*pt         27*/
     & 40.d0,42.d0,45.d0,49.d0,52.d0,56.d0,58.d0,59.d0,58.d0,57.d0,54.d0        !/*pt         28*/
     & ,50.d0,47.d0,45.d0/                                                      !/*pt         29*/
      data scao /dp0,2.d0,4.d0,6.d0,10.d0,15.d0,22.d0,30.d0,40.d0,53.d0,        !/*pt         30*/
     & 68.d0,88.d0,107.d0,121.d0,129.d0,131.d0,125.d0,111.d0,98.d0,84.d0        !/*pt         31*/
     & ,74.d0,66.d0,58.d0,52.d0,47.d0,42.d0,39.d0,36.d0,34.d0,32.d0,30          !/*pt         32*/
     & .d0,29.d0,28.d0,28.d0,27.d0,27.d0,27.d0,27.d0,27.d0,28.d0,28.d0,3        !/*pt         33*/
     & 0.d0,31.d0,32.d0,32.d0,32.d0,31.d0,29.d0,29.d0,28.d0,28.d0,28.d0,        !/*pt         34*/
     & 29.d0,30.d0,32.d0,34.d0,36.d0,38.d0,40.d0,42.d0,42.d0,42.d0,42.d0        !/*pt         35*/
     & ,40.d0,39.d0,37.d0,36.d0/                                                !/*pt         36*/
! above tables are based on cern/hera 69-1 report                               !/*pt         37*/
! above tables are good for c.m.k.e. =660mev or lab.pi k.e.=990mev              !/*pt         38*/
!     or lab pi momentum = 1.12 gev/c                                           !/*pt         39*/
      if (e.lt.66.d1) go to 10                                                  !/*pt         40*/
      write (io,40) e                                                           !/*pt         41*/
      e=659.99d0                                                                !/*pt         42*/
   10 absor=dp0                                                                 !/*pt         43*/
! the following is based on fact that above data are at equally                 !/*pt         44*/
! spaced energy intervals of 10 mev                                             !/*pt         45*/
      rie=0.1d0*e                                                               !/*pt         46*/
      ie=rie                                                                    !/*pt         47*/
      ie=ie+1                                                                   !/*pt         48*/
      de=rie-real(ie-1)                                                         !/*pt         49*/
      ae=dp1-de                                                                 !/*pt         50*/
      if (ind9.eq.1) go to 20                                                   !/*pt         51*/
      if (ind10.eq.1) go to 30                                                  !/*pt         52*/
! zero ind(9),ind(10),and ind(11) before returning                              !/*pt         53*/
      ind9=0                                                                    !/*pt         54*/
      ind10=0                                                                   !/*pt         55*/
      ind11=0                                                                   !/*pt         56*/
      scatt=ae*scao(ie)+de*scao(ie+1)                                           !/*pt         57*/
      return                                                                    !/*pt         58*/
   20 continue                                                                  !/*pt         59*/
! zero ind(9),ind(10),and ind(11) before returning                              !/*pt         60*/
      ind9=0                                                                    !/*pt         61*/
      ind10=0                                                                   !/*pt         62*/
      ind11=0                                                                   !/*pt         63*/
      scatt=ae*scaii(ie)+de*scaii(ie+1)                                         !/*pt         64*/
      return                                                                    !/*pt         65*/
   30 continue                                                                  !/*pt         66*/
! zero ind(9),ind(10),and ind(11) before returning                              !/*pt         67*/
      ind9=0                                                                    !/*pt         68*/
      ind10=0                                                                   !/*pt         69*/
      ind11=0                                                                   !/*pt         70*/
      scatt=ae*scaij(ie)+de*scaij(ie+1)                                         !/*pt         71*/
      return                                                                    !/*pt         72*/
!                                                                               !/*pt         73*/
   40 format ('0energy=',f10.4,' too large (>659.9) for ptotal')                !/*pt         74*/
      end                                                                       !/*pt         75*/

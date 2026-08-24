!     $Id: pmcdg.F,v 1.5 2002/03/27 18:15:16 gwm Exp $                        !/*pm          1*/
      subroutine pmcdg (mult,npm,wsp3,ws3,psel1)                                !/*pm          2*/
#include <lcs/double.h>                                                         /*!pm          3*/
!     ==================================================================        !/*pm          4*/
!     pmcdg uses the heuristic expression found in Orfandis                     !/*pm          5*/
!     and Rittenberg  nucl. phys. b59(73) 570 to generate the                   !/*pm          6*/
!     total pion multiplicity, and uses a pure combinatorical scheme            !/*pm          7*/
!     to generate the joint probabiltiy distribution of pi-, as                 !/*pm          8*/
!     done by bill gibbs [9 feb 84, mrc],                                       !/*pm          9*/
!     given the total number of pi's, rather than o and r's                     !/*pm         10*/
!     "decay along a chain", which only makes a difference                      !/*pm         11*/
!     for very-small total-multiplicities                                       !/*pm         12*/
!                                                                               !/*pm         13*/
! PMCDG  calls   SQRT  FLTRNF  ALOG  SIN                                        !/*pm         14*/
! called by GAMMA                                                               !/*pm         15*/
!     ==================================================================        !/*pm         16*/
!                                                                               !/*pm         17*/
!        this version of subr pmcd is designed to be                            !/*pm         18*/
!      more reader-friendly, by using 2-d matrices to index                     !/*pm         19*/
!      the cumulative distribution functions                                    !/*pm         20*/
!                                                                               !/*pm         21*/
#include <lcs/vegas.h>                                                          /*!pm         22*/
      dimension anpmn(11,19), anpm0(10,19)                                      !/*pm         23*/
      dimension f(0:20), tmp(20)                                                !/*pm         24*/
      equivalence (f0,f(0))                                                     !/*pm         25*/
! ..  save f, t0, tmp, anpmn, anpm0                                             !/*pm         26*/
!                                                                               !/*pm         27*/
      data null /0/                                                             !/*pm         28*/
!                                                                               !/*pm         29*/
!         determine total cm energy (s) for total mult.                         !/*pm         30*/
!                                                                               !/*pm         31*/
      iws3=abs(ws3)                                                             !/*pm         32*/
      iwsp3=abs(wsp3)                                                           !/*pm         33*/
      plab=psel1                                                                !/*pm         34*/
      e1q=plab**2+amass(iws3)**2                                                !/*pm         35*/
      e1=sqrt(e1q)                                                              !/*pm         36*/
!     e2 = amass(iwsp3)                                                         !/*pm         37*/
!     s = (e1+e2)**2 - psel1**2                                                 !/*pm         38*/
!     4m**2 = (m1+m2)**2                                                        !/*pm         39*/
      ratio=(amass(iws3)**2+amass(iwsp3)*(amass(iwsp3)+dp2*e1))/(amass          !/*pm         40*/
     & (iws3)+amass(iwsp3))**2                                                  !/*pm         41*/
      rmean=4.86d0*ratio**dpth                                                  !/*pm         42*/
      stdv=sqrt(rmean/3.5d0)                                                    !/*pm         43*/
!                                                                               !/*pm         44*/
   10 continue                                                                  !/*pm         45*/
      rand=rang()                                                               !/*pm         46*/
      r2=rang()                                                                 !/*pm         47*/
!           following generates a true gaussian deviate (mean=0, std=1)         !/*pm         48*/
      xn=sqrt(-dp2*log(rand))*sin(6.2832d0*r2)                                  !/*pm         49*/
!        add 2 in addition to 0.5 to avoid round-offs at 0                      !/*pm         50*/
      mult=rmean+stdv*xn+2.5d0                                                  !/*pm         51*/
      mult=mult-2                                                               !/*pm         52*/
      if (mult.lt.2) go to 10                                                   !/*pm         53*/
      if (mult.gt.19) go to 10                                                  !/*pm         54*/
      rand=rang()                                                               !/*pm         55*/
!                                                                               !/*pm         56*/
!              now determine the number of pi-                                  !/*pm         57*/
!                                                                               !/*pm         58*/
      if (wsp3+ws3) 40,20,40                                                    !/*pm         59*/
!                 total charge = 0                                              !/*pm         60*/
   20 npimx=mult/2                                                              !/*pm         61*/
      do 30 m=null,npimx                                                        !/*pm         62*/
      m1=m+1                                                                    !/*pm         63*/
      if (rand.le.anpm0(m1,mult)) go to 60                                      !/*pm         64*/
   30 continue                                                                  !/*pm         65*/
      go to 60                                                                  !/*pm         66*/
!                 total charge = -1                                             !/*pm         67*/
   40 npimx=(mult+1)/2                                                          !/*pm         68*/
      do 50 m=1,npimx                                                           !/*pm         69*/
      m1=m+1                                                                    !/*pm         70*/
      if (rand.le.anpmn(m1,mult)) go to 60                                      !/*pm         71*/
   50 continue                                                                  !/*pm         72*/
!                                                                               !/*pm         73*/
   60 npm=m1-1                                                                  !/*pm         74*/
      return                                                                    !/*pm         75*/
!                                                                               !/*pm         76*/
      entry pmcdgi                                                              !/*pm         77*/
!                                                                               !/*pm         78*/
!       here we initialize a set of matrices of                                 !/*pm         79*/
!      cumulative distribution-functions for the # of pi-                       !/*pm         80*/
!      using pure combinatoric (rather than orfandis scheme)                    !/*pm         81*/
!                                                                               !/*pm         82*/
      f0=dp1                                                                    !/*pm         83*/
      do 70 i=1,20                                                              !/*pm         84*/
      f(i)=real(i)*f(i-1)                                                       !/*pm         85*/
   70 continue                                                                  !/*pm         86*/
!                                                                               !/*pm         87*/
!     do (pbar+p, nbar+n) case first (qtot=0)                                   !/*pm         88*/
!                                                                               !/*pm         89*/
!       total multiplicities run up to 19                                       !/*pm         90*/
!                                                                               !/*pm         91*/
      do 100 npi=2,19                                                           !/*pm         92*/
      s=dp0                                                                     !/*pm         93*/
      l2=npi/2                                                                  !/*pm         94*/
      do 80 npim=null,l2                                                        !/*pm         95*/
      npi0=npi-2*npim                                                           !/*pm         96*/
      tmp(npim)=f(npi)/(f(npim)**2*f(npi0))                                     !/*pm         97*/
      s=s+tmp(npim)                                                             !/*pm         98*/
   80 continue                                                                  !/*pm         99*/
      t=dp0                                                                     !/*pm        100*/
      do 90 npim=null,l2                                                        !/*pm        101*/
      m1=npim+1                                                                 !/*pm        102*/
      t=t+tmp(npim)                                                             !/*pm        103*/
      anpm0(m1,npi)=t/s                                                         !/*pm        104*/
   90 continue                                                                  !/*pm        105*/
  100 continue                                                                  !/*pm        106*/
!                                                                               !/*pm        107*/
!     do (pbar+n,(nbar+p)) case next (qtot=-1(1))                               !/*pm        108*/
!                                                                               !/*pm        109*/
!       total multiplicities run up to 19                                       !/*pm        110*/
!                                                                               !/*pm        111*/
      do 130 npi=2,19                                                           !/*pm        112*/
      s=dp0                                                                     !/*pm        113*/
      l2=(npi+1)/2                                                              !/*pm        114*/
      do 110 npim=1,l2                                                          !/*pm        115*/
      npi0=npi-2*npim+1                                                         !/*pm        116*/
      tmp(npim)=f(npi)/(f(npim)*f(npim-1)*f(npi0))                              !/*pm        117*/
      s=s+tmp(npim)                                                             !/*pm        118*/
  110 continue                                                                  !/*pm        119*/
      t=dp0                                                                     !/*pm        120*/
      anpmn(1,npi)=dp0                                                          !/*pm        121*/
      do 120 npim=1,l2                                                          !/*pm        122*/
      m1=npim+1                                                                 !/*pm        123*/
      t=t+tmp(npim)                                                             !/*pm        124*/
      anpmn(m1,npi)=t/s                                                         !/*pm        125*/
  120 continue                                                                  !/*pm        126*/
  130 continue                                                                  !/*pm        127*/
      return                                                                    !/*pm        128*/
      end                                                                       !/*pm        129*/

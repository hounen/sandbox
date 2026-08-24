!     $Id: frecol.F,v 1.4 2002/03/27 18:14:44 gwm Exp $                       !/*fr          1*/
      subroutine frecol (ecl,aa,ij,iflag,nrj)                                   !/*fr          2*/
!REP.... by R E Prael 4/29/92 for use as RECOIL with FLUKA                      !/*fr          3*/
!     KIND has FLUKA indicies.                                                  !/*fr          4*/
!     IJ is FLUKA index of incident particle.                                   !/*fr          5*/
!REP                                                                            !/*fr          6*/
!     IJ      BPART             ILAHET                                          !/*fr          7*/
!                                                                               !/*fr          8*/
!      1     proton              0                                              !/*fr          9*/
!      2     aproton             16                                             !/*fr         10*/
!      3     electron            19                                             !/*fr         11*/
!      4     positron            20                                             !/*fr         12*/
!      5     neutrie             99                                             !/*fr         13*/
!      6     aneutrie            99                                             !/*fr         14*/
!      7     photon              11                                             !/*fr         15*/
!      8     neutron             1                                              !/*fr         16*/
!      9     aneutron            17                                             !/*fr         17*/
!     10     muon+               5                                              !/*fr         18*/
!     11     muon-               6                                              !/*fr         19*/
!     12     kaonlong            13                                             !/*fr         20*/
!     13     pion+               2                                              !/*fr         21*/
!     14     pion-               4                                              !/*fr         22*/
!     15     kaon+               12                                             !/*fr         23*/
!     16     kaon-               15                                             !/*fr         24*/
!     17     lambda              99                                             !/*fr         25*/
!     18     alambda             99                                             !/*fr         26*/
!     19     kaonshrt            14                                             !/*fr         27*/
!     20     sigma-              99                                             !/*fr         28*/
!     21     sigma+              99                                             !/*fr         29*/
!     22     sigmazer            99                                             !/*fr         30*/
!     23     pizero              3                                              !/*fr         31*/
!     24     kaonzero            13                                             !/*fr         32*/
!     25     akaonzer            14                                             !/*fr         33*/
!     26     reserved            0                                              !/*fr         34*/
!     27     blank               0                                              !/*fr         35*/
!     28     blank               0                                              !/*fr         36*/
!     29     blank               0                                              !/*fr         37*/
!     30     heavyion            0                                              !/*fr         38*/
#include <lcs/double.h>                                                         /*!fr         39*/
#include <lcs/lptS.h>                                                           /*!fr         40*/
#include <lcs/inout.h>                                                          /*!fr         41*/
#include <lcs/rucom.h>                                                          /*!fr         42*/
#include <lcs/cacom.h>                                                          /*!fr         43*/
#include <lcs/banka.h>                                                          /*!fr         44*/
#include <fluka89/paprop.h>                                                     /*!fr         45*/
! for nobalc=0, aa is total energy of recoil nucleus:                           !/*fr         46*/
      if (apr.le.dp0) call expirx(1,'FRECOL','apr.le.dp0')                      !/*fr         47*/
      px=dp0                                                                    !/*fr         48*/
      py=dp0                                                                    !/*fr         49*/
      pz=dp0                                                                    !/*fr         50*/
      if (nopart.eq.0) go to 20                                                 !/*fr         51*/
      do 10 i=1,nopart                                                          !/*fr         52*/
!REP                                                                            !/*fr         53*/
      ep(i)=max(ep(i),dp1m6)                                                    !/*fr         54*/
!REP                                                                            !/*fr         55*/
      kd=kind(i)                                                                !/*fr         56*/
      tm=1000.d0*am(kd)                                                         !/*fr         57*/
!REP                                                                            !/*fr         58*/
      tmp1=dp1+dp2*tm/ep(i)                                                     !/*fr         59*/
      tmp1=max(tmp1,dp0)                                                        !/*fr         60*/
      pi=ep(i)*sqrt(tmp1)                                                       !/*fr         61*/
!REP                                                                            !/*fr         62*/
      px=pi*alpha(i)+px                                                         !/*fr         63*/
      py=pi*beta(i)+py                                                          !/*fr         64*/
      pz=pi*gam(i)+pz                                                           !/*fr         65*/
   10 continue                                                                  !/*fr         66*/
   20 continue                                                                  !/*fr         67*/
      tm=1000.d0*am(ij)                                                         !/*fr         68*/
      pz=ecl*sqrt(dp1+dp2*tm/ecl)-pz                                            !/*fr         69*/
      pj=px*px+py*py+pz*pz                                                      !/*fr         70*/
      if (iflag.eq.0) then                                                      !/*fr         71*/
!        AA = MASS + EXCITATION                                                 !/*fr         72*/
        erec=sqrt(aa*aa+pj)-aa                                                  !/*fr         73*/
      else                                                                      !/*fr         74*/
!        AA = MASS + EXCITATION + RECOIL                                        !/*fr         75*/
        tmp2=aa*aa-pj                                                           !/*fr         76*/
        if (tmp2.lt.dp0) then                                                   !/*fr         77*/
          erec=0.0                                                              !/*fr         78*/
          nrj=1                                                                 !/*fr         79*/
          write (itty,*) 'FRECOL recalling FLUKA tmp2= ',tmp2,' nocas='         !/*fr         80*/
     &    ,nocas                                                                !/*fr         81*/
          write (io,*) 'FRECOL recalling FLUKA tmp2= ',tmp2,' nocas='           !/*fr         82*/
     &    ,nocas                                                                !/*fr         83*/
          go to 30                                                              !/*fr         84*/
        endif                                                                   !/*fr         85*/
        erec=pj/(aa+sqrt(tmp2))                                                 !/*fr         86*/
      endif                                                                     !/*fr         87*/
      if (pj.gt.dp0) then                                                       !/*fr         88*/
        pj=sqrt(pj)                                                             !/*fr         89*/
        coslbr(1)=-px/pj                                                        !/*fr         90*/
        coslbr(2)=-py/pj                                                        !/*fr         91*/
        coslbr(3)=pz/pj                                                         !/*fr         92*/
      else                                                                      !/*fr         93*/
        erec=0.0                                                                !/*fr         94*/
        nrj=1                                                                   !/*fr         95*/
        write (itty,*) 'FRECOL recalling FLUKA pj=',pj,' nocas=',nocas          !/*fr         96*/
        write (io,*) 'FRECOL recalling FLUKA pj=',pj,' nocas=',nocas            !/*fr         97*/
        go to 30                                                                !/*fr         98*/
      endif                                                                     !/*fr         99*/
   30 continue                                                                  !/*fr        100*/
      return                                                                    !/*fr        101*/
      end                                                                       !/*fr        102*/

!     $Id: ajtest.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                       !/*aj          1*/
      subroutine ajtest                                                         !/*aj          2*/
#include <lcs/double.h>                                                         /*!aj          3*/
!     ==================================================================        !/*aj          4*/
! AJTEST calls   STNEW                                                          !/*aj          5*/
! called by INISBL                                                              !/*aj          6*/
!                                                                               !/*aj          7*/
!     ajtest loads either from st or stp into ws.  if during                    !/*aj          8*/
!     one time interval, there was one or more collisions and there was         !/*aj          9*/
!     one or more particles above the cutoff, then ajtest loads ws from         !/*aj         10*/
!     stp.  otherwise it loads ws from st.                                      !/*aj         11*/
!     ==================================================================        !/*aj         12*/
!                                                                               !/*aj         13*/
#include <lcs/vegas.h>                                                          /*!aj         14*/
!  icsb(1) is the no. of particles to be considered in                          !/*aj         15*/
!  the next time interval                                                       !/*aj         16*/
!                                                                               !/*aj         17*/
!     if (ws(18) .lt. 0.) pause                                                 !/*aj         18*/
   10 if (icsb(1).ne.0) go to 20                                                !/*aj         19*/
      if (npout.eq.0) return                                                    !/*aj         20*/
      go to 30                                                                  !/*aj         21*/
!                                                                               !/*aj         22*/
!            icsb(2) holds the number of particles to be con-                   !/*aj         23*/
!            sidered in the current interval.  subtract the one                 !/*aj         24*/
!            just handled, and see what remains                                 !/*aj         25*/
!                                                                               !/*aj         26*/
   20 icsb(2)=icsb(2)-1                                                         !/*aj         27*/
      if (icsb(2)) 40,30,40                                                     !/*aj         28*/
!                    end of a time interval                                     !/*aj         29*/
!                    move mft candidate into mft(actual),                       !/*aj         30*/
!                    set to consider #active in what will be                    !/*aj         31*/
!                    new "current" time interval                                !/*aj         32*/
!                                                                               !/*aj         33*/
   30 icsb(2)=icsb(1)                                                           !/*aj         34*/
      ws(17)=ws(18)                                                             !/*aj         35*/
      ws(19)=ws(18)+ws(19)                                                      !/*aj         36*/
!-----set csa(14) as candidate for the next time interval [y.y. 9/12/84]        !/*aj         37*/
      ws(18)=csa(14)                                                            !/*aj         38*/
!                 stnew moves projectile-holes and non-interacted               !/*aj         39*/
!                 nucleons forward to this time-interval                        !/*aj         40*/
      call stnew                                                                !/*aj         41*/
!                                                                               !/*aj         42*/
!        now is good place to write-out phase-vectors                           !/*aj         43*/
!        for purposes of movies, etc                                            !/*aj         44*/
!                                                                               !/*aj         45*/
!                 reset to read out first particle in "st"                      !/*aj         46*/
      icsb(7)=0                                                                 !/*aj         47*/
      go to 60                                                                  !/*aj         48*/
!                                                                               !/*aj         49*/
! if ind(1)=1 then ajtest loads ws from stp.   otherwise load ws from           !/*aj         50*/
!  st.  ind(1)=1 when particle is in "collision" mode.                          !/*aj         51*/
!                                                                               !/*aj         52*/
   40 if (ind(1)-1) 50,110,50                                                   !/*aj         53*/
   50 icsb(7)=icsb(7)+1                                                         !/*aj         54*/
   60 icsb6=icsb(7)*i81+1                                                       !/*aj         55*/
      if (st(icsb6).lt.0) go to 50                                              !/*aj         56*/
      if (nrep.le.0) go to 90                                                   !/*aj         57*/
!                                                                               !/*aj         58*/
!         check for, kill off any particles that died recently                  !/*aj         59*/
!                                                                               !/*aj         60*/
      if (st(icsb6+1).gt.5.1d0) go to 90                                        !/*aj         61*/
      k=st(icsb6+i81-1)+1.d-03                                                  !/*aj         62*/
      if (k.lt.1) go to 90                                                      !/*aj         63*/
!-----------------------------------------------------------------------        !/*aj         64*/
! Only kill off "real" virgins (not 5.d+02).                                    !/*aj         65*/
      if (k.ge.500) go to 90                                                    !/*aj         66*/
!-----------------------------------------------------------------------        !/*aj         67*/
      l=st(icsb6+2)+1.d-03                                                      !/*aj         68*/
      do 70 jj=1,nrep                                                           !/*aj         69*/
      if (jrep(jj).ne.l) go to 70                                               !/*aj         70*/
      if (jj.eq.nrep) go to 80                                                  !/*aj         71*/
      jrep(jj)=jrep(nrep)                                                       !/*aj         72*/
      go to 80                                                                  !/*aj         73*/
   70 continue                                                                  !/*aj         74*/
      go to 90                                                                  !/*aj         75*/
!                                                                               !/*aj         76*/
!                    kill the particle in "st", de-activate it                  !/*aj         77*/
!                                                                               !/*aj         78*/
   80 nrep=nrep-1                                                               !/*aj         79*/
      st(icsb6)=-st(icsb6)                                                      !/*aj         80*/
      icsb(1)=icsb(1)-1                                                         !/*aj         81*/
      go to 10                                                                  !/*aj         82*/
!                                                                               !/*aj         83*/
!         set pointer-base to location of particle in "st"                      !/*aj         84*/
!                                                                               !/*aj         85*/
   90 icsb(13)=icsb6-1                                                          !/*aj         86*/
      icsb13=icsb(13)                                                           !/*aj         87*/
!                                move "st" into "ws"                            !/*aj         88*/
      do 100 idx=1,14                                                           !/*aj         89*/
      ws(idx)=st(icsb13+idx)                                                    !/*aj         90*/
  100 continue                                                                  !/*aj         91*/
      ws(25)=st(icsb13+15)                                                      !/*aj         92*/
      ws(24)=st(icsb13+16)                                                      !/*aj         93*/
      ws(16)=ws(17)                                                             !/*aj         94*/
      ws(31)=st(icsb13+i81)                                                     !/*aj         95*/
      if (ws(3).lt.dp10) go to 130                                              !/*aj         96*/
      ws(26)=st(icsb13+i81-4)                                                   !/*aj         97*/
      ws(27)=st(icsb13+i81-3)                                                   !/*aj         98*/
      ws(28)=st(icsb13+i81-2)                                                   !/*aj         99*/
      ws(29)=st(icsb13+i81-1)                                                   !/*aj        100*/
      go to 130                                                                 !/*aj        101*/
!                                                                               !/*aj        102*/
!                    set base-pointer of particle in "stp",                     !/*aj        103*/
!                    move "stp" into "ws"                                       !/*aj        104*/
!                                                                               !/*aj        105*/
  110 icsb(10)=icsb(9)*icsb(11)                                                 !/*aj        106*/
      icsb10=icsb(10)                                                           !/*aj        107*/
      ws(1)=stp(icsb10+1)                                                       !/*aj        108*/
      ws(2)=stp(icsb10+2)                                                       !/*aj        109*/
      ws(3)=stp(icsb10+3)                                                       !/*aj        110*/
      ws(4)=stp(icsb10+4)                                                       !/*aj        111*/
      ws(5)=stp(icsb10+5)                                                       !/*aj        112*/
      if (ws(5).eq.dp0) go to 150                                               !/*aj        113*/
      do 120 idx=6,12                                                           !/*aj        114*/
      ws(idx)=stp(icsb10+idx)                                                   !/*aj        115*/
  120 continue                                                                  !/*aj        116*/
      ws(16)=stp(icsb10+13)                                                     !/*aj        117*/
      ws(14)=stp(icsb10+14)                                                     !/*aj        118*/
      ws(13)=max(dp1m4,ws(16))                                                  !/*aj        119*/
      ws(25)=stp(icsb10+15)                                                     !/*aj        120*/
      ws(24)=stp(icsb10+16)                                                     !/*aj        121*/
      iws15=stp(icsb10+17)/1.d+02                                               !/*aj        122*/
      ws(15)=stp(icsb10+17)-iws15*100                                           !/*aj        123*/
      ws(31)=stp(icsb10+22)                                                     !/*aj        124*/
      icsb(15)=icsb(15)+1                                                       !/*aj        125*/
!           cancel cascade if activating too much into "st"                     !/*aj        126*/
      if (icsb(15).ge.i391) go to 160                                           !/*aj        127*/
!            with "ws" filled from "stp", fill "st" with same                   !/*aj        128*/
      icsb(13)=icsb(15)*i81                                                     !/*aj        129*/
      icsb13=icsb(13)                                                           !/*aj        130*/
      st(icsb13+17)=stp(icsb10+17)                                              !/*aj        131*/
      st(icsb13+18)=stp(icsb10+18)                                              !/*aj        132*/
      st(icsb13+19)=stp(icsb10+19)                                              !/*aj        133*/
      st(icsb13+20)=stp(icsb10+20)                                              !/*aj        134*/
      st(icsb13+21)=stp(icsb10+21)                                              !/*aj        135*/
      icsb(11)=icsb(11)+1                                                       !/*aj        136*/
      ws(16)=ws(13)                                                             !/*aj        137*/
  130 do 140 ii=1,21                                                            !/*aj        138*/
      wsp(ii)=dp0                                                               !/*aj        139*/
  140 wspr(ii)=dp0                                                              !/*aj        140*/
      wsp(31)=dp0                                                               !/*aj        141*/
      wspr(31)=dp0                                                              !/*aj        142*/
      ind(6)=0                                                                  !/*aj        143*/
      return                                                                    !/*aj        144*/
!          "stp" particle has no (0.0) energy:  normal cancellation             !/*aj        145*/
  150 kcan=1                                                                    !/*aj        146*/
      lkcan='ajtest 1'                                                          !/*aj        147*/
      return                                                                    !/*aj        148*/
!              storage-overflow cancellation                                    !/*aj        149*/
  160 kcan=2                                                                    !/*aj        150*/
      lkcan='ajtest 2'                                                          !/*aj        151*/
      return                                                                    !/*aj        152*/
      end                                                                       !/*aj        153*/

!     $Id: delta.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                        !/*de          1*/
      subroutine delta                                                          !/*de          2*/
#include <lcs/double.h>                                                         /*!de          3*/
!     ==================================================================        !/*de          4*/
!     delta is a bookkeeping routine.                                           !/*de          5*/
!                                                                               !/*de          6*/
! DELTA  calls   AMFP  SQRT  PLT                                                !/*de          7*/
! called by ISABEL                                                              !/*de          8*/
!     ==================================================================        !/*de          9*/
!                                                                               !/*de         10*/
#include <lcs/inout.h>                                                          /*!de         11*/
#include <lcs/vegas.h>                                                          /*!de         12*/
      dimension p(4), pt(4)                                                     !/*de         13*/
!                                                                               !/*de         14*/
      bet=-voc                                                                  !/*de         15*/
!                     ind(1)=1 when particle is in collision mode.              !/*de         16*/
      if (ind(1)-1) 10,30,10                                                    !/*de         17*/
!                                                                               !/*de         18*/
! in statement 1 the new x,y,z,r,r*r, and potential of the particle             !/*de         19*/
!  is stored in st.                                                             !/*de         20*/
!                                                                               !/*de         21*/
   10 icsb13=icsb(13)                                                           !/*de         22*/
      st(icsb13+10)=ws(10)                                                      !/*de         23*/
      st(icsb13+11)=ws(11)                                                      !/*de         24*/
      st(icsb13+12)=ws(12)                                                      !/*de         25*/
      st(icsb13+14)=ws(14)                                                      !/*de         26*/
      st(icsb13+15)=ws(25)                                                      !/*de         27*/
      st(icsb13+16)=ws(24)                                                      !/*de         28*/
!                                                                               !/*de         29*/
! ind(4)=1 when there will be no collision, or when rlar is entered.            !/*de         30*/
!  in both instances the energy, direction cosines, etc. have to be             !/*de         31*/
!  stored in st.                                                                !/*de         32*/
!                                                                               !/*de         33*/
      if (ind(4)-1) 60,20,60                                                    !/*de         34*/
   20 ind(4)=0                                                                  !/*de         35*/
      st(icsb13+5)=ws(5)                                                        !/*de         36*/
      st(icsb13+6)=ws(6)                                                        !/*de         37*/
      st(icsb13+7)=ws(7)                                                        !/*de         38*/
      st(icsb13+8)=ws(8)                                                        !/*de         39*/
      st(icsb13+9)=ws(9)                                                        !/*de         40*/
      st(icsb13+15)=ws(25)                                                      !/*de         41*/
      st(icsb13+16)=ws(24)                                                      !/*de         42*/
!                   ind(6)=1 when particle escapes the nucleus.                 !/*de         43*/
      if (ind(6).eq.1) st(icsb13+2)=-ws(2)                                      !/*de         44*/
      return                                                                    !/*de         45*/
!                                                                               !/*de         46*/
!     if collisions decreased mean-free-time, store new lower                   !/*de         47*/
!     value in ws18 as candidate for next interval length                       !/*de         48*/
!                                                                               !/*de         49*/
! ind(6)=1 when particle escapes the nucleus.  then as in 13, all               !/*de         50*/
!  quantities are stored into st.                                               !/*de         51*/
!                                                                               !/*de         52*/
   30 if (ind(6)-1) 50,40,50                                                    !/*de         53*/
   40 ws(1)=-ws(1)                                                              !/*de         54*/
      ws(2)=-ws(2)                                                              !/*de         55*/
      go to 100                                                                 !/*de         56*/
!                                                                               !/*de         57*/
!     particle is colliding, without escaping at this point                     !/*de         58*/
!                                                                               !/*de         59*/
   50 iws3=ws(3)                                                                !/*de         60*/
      iwsi3=iws3                                                                !/*de         61*/
   60 if (ind(30).eq.1) go to 80                                                !/*de         62*/
!                                                                               !/*de         63*/
!      fix for occasional pbar (nbar) that gets to here                         !/*de         64*/
!      mrc [31 oct 83]                                                          !/*de         65*/
!                                                                               !/*de         66*/
      if (iws3.lt.0) go to 70                                                   !/*de         67*/
!                                                                               !/*de         68*/
      if (iws3.gt.2.and.iws3.lt.10) go to 70                                    !/*de         69*/
!                        only nucleons, isobars worry about cutoffs             !/*de         70*/
      if (iws3.gt.10) iwsi3=iws3/10                                             !/*de         71*/
      if ((ws(5)-ws(4)-ef(iwsi3,istept)).gt.cutofa(2+iwsi3)) go to 70           !/*de         72*/
!                                                                               !/*de         73*/
!     for particles below cutoff, use old value of                              !/*de         74*/
!     the mean free time, and not the very small value from amfp                !/*de         75*/
!                                                                               !/*de         76*/
      ws(13)=ws(18)                                                             !/*de         77*/
      go to 80                                                                  !/*de         78*/
!            calculate new mean-free-time candidate in densest region           !/*de         79*/
   70 call amfp (ws(5),den_(ieght))                                             !/*de         80*/
!                save new mft if shorter than older ones                        !/*de         81*/
      if (ws(13).lt.ws(18)) ws(18)=ws(13)                                       !/*de         82*/
!                           check if proj-intn impossible                       !/*de         83*/
   80 if (ind(31).eq.1) go to 100                                               !/*de         84*/
!                              boost into projectile,check mft                  !/*de         85*/
      p(1)=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                        !/*de         86*/
      p(2)=ws(6)                                                                !/*de         87*/
      p(3)=ws(7)                                                                !/*de         88*/
      p(4)=ws(8)                                                                !/*de         89*/
      call plt (p,ws(5),pt,et,bet,gama)                                         !/*de         90*/
      if (iws3.gt.2.and.iws3.lt.9) go to 90                                     !/*de         91*/
      if (iws3.gt.10) iwsi3=iws3/10                                             !/*de         92*/
      if ((et-ws(4)-pef(iwsi3,istepp)).gt.cutofa(9+iwsi3)) go to 90             !/*de         93*/
      ws(13)=ws(18)                                                             !/*de         94*/
      go to 100                                                                 !/*de         95*/
!            calculate new mean-free-time candidate in densest region           !/*de         96*/
   90 call amfp (et,pdn(ieght))                                                 !/*de         97*/
      if (ws(13).lt.ws(18)) ws(18)=ws(13)                                       !/*de         98*/
  100 if (ind(1).ne.1) return                                                   !/*de         99*/
      icsb13=icsb(13)                                                           !/*de        100*/
!                if next mft is negative, kill the cascade                      !/*de        101*/
      if (ws(18).le.dp0) go to 110                                              !/*de        102*/
      st(icsb13+1)=ws(1)                                                        !/*de        103*/
      st(icsb13+2)=ws(2)                                                        !/*de        104*/
      st(icsb13+3)=ws(3)                                                        !/*de        105*/
      st(icsb13+4)=ws(4)                                                        !/*de        106*/
      st(icsb13+5)=ws(5)                                                        !/*de        107*/
      st(icsb13+6)=ws(6)                                                        !/*de        108*/
      st(icsb13+7)=ws(7)                                                        !/*de        109*/
      st(icsb13+8)=ws(8)                                                        !/*de        110*/
      st(icsb13+9)=ws(9)                                                        !/*de        111*/
      st(icsb13+10)=ws(10)                                                      !/*de        112*/
      st(icsb13+11)=ws(11)                                                      !/*de        113*/
      st(icsb13+12)=ws(12)                                                      !/*de        114*/
      st(icsb13+13)=ws(13)                                                      !/*de        115*/
      st(icsb13+14)=ws(14)                                                      !/*de        116*/
      st(icsb13+15)=ws(25)                                                      !/*de        117*/
      st(icsb13+16)=ws(24)                                                      !/*de        118*/
      st(icsb13+i81)=ws(31)                                                     !/*de        119*/
      icsb(3)=icsb(3)-1                                                         !/*de        120*/
      if (icsb(3).gt.0) return                                                  !/*de        121*/
!                         if nothing left in "stp", zero flags                  !/*de        122*/
      icsb(3)=0                                                                 !/*de        123*/
      icsb(16)=0                                                                !/*de        124*/
      icsb(11)=0                                                                !/*de        125*/
      icsb(10)=0                                                                !/*de        126*/
      ind(1)=0                                                                  !/*de        127*/
      return                                                                    !/*de        128*/
  110 kcan=1                                                                    !/*de        129*/
      lkcan='delta'                                                             !/*de        130*/
      if (mprin.gt.1) write (io,120)                                            !/*de        131*/
      return                                                                    !/*de        132*/
!                                                                               !/*de        133*/
  120 format ('0 cascade stoped in sub. delta (ws(18) .le. 0.)')                !/*de        134*/
      end                                                                       !/*de        135*/

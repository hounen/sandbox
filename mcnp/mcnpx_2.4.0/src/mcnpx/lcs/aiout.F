!     $Id: aiout.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                        !/*ai          1*/
      subroutine aiout                                                          !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     aiout is called when an isobar is escaping.                               !/*ai          5*/
!                                                                               !/*ai          6*/
! AIOUT  calls   DECAY  WSSTO  ELAS  CMB  CMC  ISEDT  AIOUTA                    !/*ai          7*/
! called by RLAR                                                                !/*ai          8*/
!     ==================================================================        !/*ai          9*/
!                                                                               !/*ai         10*/
#include <lcs/vegas.h>                                                          /*!ai         11*/
! save quantities pertinent to the isobar(in ws) into gws in case it is         !/*ai         12*/
!  not allowed to decay at the surface.                                         !/*ai         13*/
! change made 10/20/70.store the direction cosines and energy                   !/*ai         14*/
! of isobar inside as well as value of ind(25) before trying to decay           !/*ai         15*/
! outside also put energy and direction cosines isobar has outside              !/*ai         16*/
! in gws                                                                        !/*ai         17*/
!                                                                               !/*ai         18*/
      gws2=ws(24)                                                               !/*ai         19*/
      gws31=ws(31)                                                              !/*ai         20*/
      gws3=ws(3)                                                                !/*ai         21*/
      gws4=ws(4)                                                                !/*ai         22*/
      gws5=gws(5)                                                               !/*ai         23*/
      gws6=gws(6)                                                               !/*ai         24*/
      gws7=gws(7)                                                               !/*ai         25*/
      gws8=gws(8)                                                               !/*ai         26*/
      ind25=ind(25)                                                             !/*ai         27*/
      ind(25)=0                                                                 !/*ai         28*/
      gws(5)=ws(5)                                                              !/*ai         29*/
      gws(6)=ws(6)                                                              !/*ai         30*/
      gws(7)=ws(7)                                                              !/*ai         31*/
      gws(8)=ws(8)                                                              !/*ai         32*/
      wws(4)=ws(4)                                                              !/*ai         33*/
!  ind(17)=1 when isobar is escaping                                            !/*ai         34*/
      ind(17)=1                                                                 !/*ai         35*/
      if (ws(3)-25.d0) 10,20,10                                                 !/*ai         36*/
! change made 10/20/70 to recalculate ind(25) for i+ and i0                     !/*ai         37*/
   10 if (ws(3).ne.13.d0) go to 150                                             !/*ai         38*/
! ind(12)=1 for i++ and i-.                                                     !/*ai         39*/
   20 ind(12)=1                                                                 !/*ai         40*/
!     -------------------------                                                 !/*ai         41*/
   30 call decay (ans)                                                          !/*ai         42*/
!     -------------------------                                                 !/*ai         43*/
! ind(18)=1 when in subr pih the momentum in center of mass to be               !/*ai         44*/
!  divided by the two decay particles is negative.  this of course              !/*ai         45*/
!  indicate a not allowed decay.                                                !/*ai         46*/
      if (kcan.ne.0) return                                                     !/*ai         47*/
      if (ind(18)-1) 50,40,50                                                   !/*ai         48*/
! change made 10/20/70.if isobar can not decay restoregws,ws,ind(25)            !/*ai         49*/
   40 continue                                                                  !/*ai         50*/
      ind(25)=ind25                                                             !/*ai         51*/
      ws(3)=gws3                                                                !/*ai         52*/
      ws(4)=gws4                                                                !/*ai         53*/
      ws(24)=gws2                                                               !/*ai         54*/
      ws(31)=gws31                                                              !/*ai         55*/
      ws(5)=gws(5)                                                              !/*ai         56*/
      ws(6)=gws(6)                                                              !/*ai         57*/
      ws(7)=gws(7)                                                              !/*ai         58*/
      ws(8)=gws(8)                                                              !/*ai         59*/
      gws(5)=gws5                                                               !/*ai         60*/
      gws(6)=gws6                                                               !/*ai         61*/
      gws(7)=gws7                                                               !/*ai         62*/
      gws(8)=gws8                                                               !/*ai         63*/
      return                                                                    !/*ai         64*/
   50 continue                                                                  !/*ai         65*/
! change made 11/13/70 to restore gws to values that they are assumed           !/*ai         66*/
! to have                                                                       !/*ai         67*/
      gws(2)=gws2                                                               !/*ai         68*/
      gws(3)=gws3                                                               !/*ai         69*/
      gws(4)=gws4                                                               !/*ai         70*/
      if (gws(3)-13.d0) 70,60,70                                                !/*ai         71*/
!       aip(1,2),ain(1,2),keep count of no. of i++,i+,i0,i- emitted.            !/*ai         72*/
   60 aip(1)=aip(1)+dp1                                                         !/*ai         73*/
      go to 120                                                                 !/*ai         74*/
   70 if (gws(3)-14.d0) 90,80,90                                                !/*ai         75*/
   80 aip(2)=aip(2)+dp1                                                         !/*ai         76*/
      go to 120                                                                 !/*ai         77*/
   90 if (gws(3)-24.d0) 110,100,110                                             !/*ai         78*/
  100 ain(1)=ain(1)+dp1                                                         !/*ai         79*/
      go to 120                                                                 !/*ai         80*/
  110 ain(2)=ain(2)+dp1                                                         !/*ai         81*/
! when an isobar escapes, the time,x,y,z of escape,and energy before            !/*ai         82*/
!  decay are stored.                                                            !/*ai         83*/
  120 ws(15)=ws(19)-ws(16)                                                      !/*ai         84*/
      icsb13=icsb(13)                                                           !/*ai         85*/
      st(icsb13+3)=gws(3)                                                       !/*ai         86*/
      st(icsb13+4)=gws(4)                                                       !/*ai         87*/
      st(icsb13+5)=gws(5)                                                       !/*ai         88*/
      st(icsb13+16)=gws(2)                                                      !/*ai         89*/
      icsb13=16.d0+ws(2)                                                        !/*ai         90*/
      icsb13=icsb13+icsb(13)                                                    !/*ai         91*/
      ws(2)=-(ws(2)+dp5)                                                        !/*ai         92*/
      ws(1)=-ws(1)                                                              !/*ai         93*/
      st(icsb13+2)=ws(10)                                                       !/*ai         94*/
      st(icsb13+3)=ws(11)                                                       !/*ai         95*/
      st(icsb13+4)=ws(12)                                                       !/*ai         96*/
      st(icsb13+1)=ws(15)                                                       !/*ai         97*/
      st(icsb13+5)=gws(5)                                                       !/*ai         98*/
!     -------------------------                                                 !/*ai         99*/
      call wssto (icsb(13))                                                     !/*ai        100*/
!     -------------------------                                                 !/*ai        101*/
! the decay of the isobar at the surface is treated in almost exactly           !/*ai        102*/
!  way as decay within the the nucleus, except the nucleon from the             !/*ai        103*/
!  decay is not checked for pauli forbiddenness.                                !/*ai        104*/
!     -------------------------                                                 !/*ai        105*/
      call elas (ans)                                                           !/*ai        106*/
      call cmb                                                                  !/*ai        107*/
      call cmc (cm(7),cm(8),cm(9),cm(10))                                       !/*ai        108*/
!     -------------------------                                                 !/*ai        109*/
      ws(5)=cm2(3)                                                              !/*ai        110*/
      ws(6)=cm2(4)/cm2(2)                                                       !/*ai        111*/
      ws(7)=cm2(5)/cm2(2)                                                       !/*ai        112*/
      ws(8)=cm2(6)/cm2(2)                                                       !/*ai        113*/
!     -------------------------                                                 !/*ai        114*/
      call cmc (cm(11),cm(12),cm(13),cm(14))                                    !/*ai        115*/
!     -------------------------                                                 !/*ai        116*/
      wsp(5)=cm2(3)                                                             !/*ai        117*/
      wsp(6)=cm2(4)/cm2(2)                                                      !/*ai        118*/
      wsp(7)=cm2(5)/cm2(2)                                                      !/*ai        119*/
      wsp(8)=cm2(6)/cm2(2)                                                      !/*ai        120*/
      wsp(14)=ws(14)                                                            !/*ai        121*/
      do 130 i=10,12                                                            !/*ai        122*/
      wsp(i)=ws(i)                                                              !/*ai        123*/
  130 continue                                                                  !/*ai        124*/
      wsp(25)=ws(25)                                                            !/*ai        125*/
      iws15=ws(15)/1.d+02                                                       !/*ai        126*/
      ws15=ws(15)-iws15*100                                                     !/*ai        127*/
      wsp(27)=ws15                                                              !/*ai        128*/
      ws(27)=ws15                                                               !/*ai        129*/
      ws(1)=poe1                                                                !/*ai        130*/
      wsp(1)=poe1                                                               !/*ai        131*/
      j2=ws(2)/dp5+1.d-03                                                       !/*ai        132*/
      ws(2)=dp5-ws(2)+j2*dp5                                                    !/*ai        133*/
      wsp(2)=ws(2)                                                              !/*ai        134*/
      if (ind(1).eq.0) go to 140                                                !/*ai        135*/
      icsb(3)=icsb(3)-1                                                         !/*ai        136*/
      if (icsb(3).gt.0) go to 140                                               !/*ai        137*/
      icsb(16)=0                                                                !/*ai        138*/
      icsb(11)=0                                                                !/*ai        139*/
      icsb(10)=0                                                                !/*ai        140*/
      ind(1)=0                                                                  !/*ai        141*/
!       subr edit is called to count the decay particle as escapees.            !/*ai        142*/
  140 ws(31)=dp0                                                                !/*ai        143*/
      wsp(31)=gws(15)                                                           !/*ai        144*/
!     -------------------------                                                 !/*ai        145*/
      call isedt (wsp)                                                          !/*ai        146*/
!     -------------------------                                                 !/*ai        147*/
!                                                                               !/*ai        148*/
      if (kcan.ne.0) return                                                     !/*ai        149*/
!                                                                               !/*ai        150*/
!     -------------------------                                                 !/*ai        151*/
      call isedt (ws)                                                           !/*ai        152*/
!     -------------------------                                                 !/*ai        153*/
      if (kcan.ne.0) return                                                     !/*ai        154*/
!                                                                               !/*ai        155*/
!       subr aiouta is called to store the decay particles into st.             !/*ai        156*/
!     -------------------------                                                 !/*ai        157*/
      call aiouta (wsp)                                                         !/*ai        158*/
      call aiouta (ws)                                                          !/*ai        159*/
!     -------------------------                                                 !/*ai        160*/
      return                                                                    !/*ai        161*/
! change made 10/20/70 to recalculate ind(25) for i+ and i0                     !/*ai        162*/
  150 continue                                                                  !/*ai        163*/
      if (ws(3).eq.14.d0) go to 160                                             !/*ai        164*/
! have an i0                                                                    !/*ai        165*/
      if (ws(4).le.(amass(2)+amass(4))) ind(25)=1                               !/*ai        166*/
      if (ws(4).le.(amass(1)+amass(5))) ind(25)=2                               !/*ai        167*/
      go to 30                                                                  !/*ai        168*/
  160 continue                                                                  !/*ai        169*/
! have an i+                                                                    !/*ai        170*/
      if (ws(4).le.(amass(2)+amass(3))) ind(25)=1                               !/*ai        171*/
      if (ws(4).le.(amass(1)+amass(4))) ind(25)=2                               !/*ai        172*/
      go to 30                                                                  !/*ai        173*/
      end                                                                       !/*ai        174*/

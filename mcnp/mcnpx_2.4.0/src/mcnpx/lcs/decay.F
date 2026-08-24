!     $Id: decay.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                        !/*de          1*/
      subroutine decay (ans)                                                    !/*de          2*/
#include <lcs/double.h>                                                         /*!de          3*/
!     ==================================================================        !/*de          4*/
!     decay takes care of the decay of the isobar in its own center             !/*de          5*/
!     of mass.  the argument ans is the angle between the pion and              !/*de          6*/
!     nucleon after decay.                                                      !/*de          7*/
!                                                                               !/*de          8*/
! DECAY  calls   FLTRNF  CEP  SQRT  PIH  PANGLE  ANGLI1                         !/*de          9*/
! called by AIOUT  GAMMA                                                        !/*de         10*/
!     ==================================================================        !/*de         11*/
!                                                                               !/*de         12*/
#include <lcs/inout.h>                                                          /*!de         13*/
#include <lcs/vegas.h>                                                          /*!de         14*/
      dimension gws32(32)                                                       !/*de         15*/
      equivalence (gws32(1),gws(1))                                             !/*de         16*/
!                                                                               !/*de         17*/
!  ind(15)=1 for decay                                                          !/*de         18*/
      ind(15)=1                                                                 !/*de         19*/
      gws4=ws(4)                                                                !/*de         20*/
!  first calculate the fermi energy of nucleons                                 !/*de         21*/
!  ind(17)=1 for escaping isobar and therefore efermi=0                         !/*de         22*/
      efn=dp0                                                                   !/*de         23*/
      efp=dp0                                                                   !/*de         24*/
      if (ind(17).eq.1) go to 20                                                !/*de         25*/
      if (ws(31).eq.dp0) go to 10                                               !/*de         26*/
      if (istepp.eq.0) go to 20                                                 !/*de         27*/
      efp=pef(1,istepp)                                                         !/*de         28*/
      efn=pef(2,istepp)                                                         !/*de         29*/
      go to 20                                                                  !/*de         30*/
   10 if (istept.eq.dp0) go to 20                                               !/*de         31*/
      efp=ef(1,istept)                                                          !/*de         32*/
      efn=ef(2,istept)                                                          !/*de         33*/
! ind(12)=1 when the decaying particle is i++ or i-.                            !/*de         34*/
   20 if (ind(12).ne.1) go to 40                                                !/*de         35*/
!  delta is a fudging quantity used to conserve energy. when an i++ or          !/*de         36*/
!  i- decays, no fudging is necessary.  i++ feels a proton potential            !/*de         37*/
!  and i- feels a neutron potential.  hence before and after decay              !/*de         38*/
!  there is no change in potential. however for i+ and i0, there is a           !/*de         39*/
!  possibility of a change of potential because i+ and i0 feel a                !/*de         40*/
!  proton and neutron potential respectively.                                   !/*de         41*/
      delta=dp0                                                                 !/*de         42*/
      if (ws(3).eq.25.d0) go to 30                                              !/*de         43*/
!  i++ decaying into pion+ and proton.                                          !/*de         44*/
      wsp(3)=dp1                                                                !/*de         45*/
      ws(3)=dp3                                                                 !/*de         46*/
      wsp(24)=efp                                                               !/*de         47*/
      go to 110                                                                 !/*de         48*/
!  i- decaying into pion- and neutron.                                          !/*de         49*/
   30 wsp(3)=dp2                                                                !/*de         50*/
      ws(3)=dp5                                                                 !/*de         51*/
      wsp(24)=efn                                                               !/*de         52*/
      go to 110                                                                 !/*de         53*/
! gf is a random number used later to choose decay mode of i+ and i0.           !/*de         54*/
!                                                                               !/*de         55*/
   40 continue                                                                  !/*de         56*/
      gf=rang()                                                                 !/*de         57*/
!                                                                               !/*de         58*/
! ind(25)=1 when i+ decaying to n and pion+ or i0 to n and pion0 not            !/*de         59*/
!  allowed kinematically.                                                       !/*de         60*/
! ind(25)=2 when i+ decaying to p and pion0 or i0 to p and pion- not            !/*de         61*/
!  allowed kinematically.                                                       !/*de         62*/
! ind(25)=3 when i+ and i0 are not allowed kinematically to decay into          !/*de         63*/
!  any mode.                                                                    !/*de         64*/
! ind(25)=0 when all modes are allowed.                                         !/*de         65*/
! ind(25) is set in subr aitota.                                                !/*de         66*/
      if (ind(25)-1) 70,50,60                                                   !/*de         67*/
   50 if (ws(3)-14.d0) 140,100,140                                              !/*de         68*/
   60 if (ws(3)-14.d0) 130,90,130                                               !/*de         69*/
! statement 7 is for all modes allowed.                                         !/*de         70*/
   70 continue                                                                  !/*de         71*/
! change made 10/21/70.if the c.m.k.e.is less than or equal 140 mev             !/*de         72*/
! than the t=1/2 cross section should be neglible and the probability           !/*de         73*/
! for an i+ or i0 decaying into a pi+ and a neutron or a pi- and a              !/*de         74*/
! proton should be 1.d0/3.                                                      !/*de         75*/
      aisobd=dpth                                                               !/*de         76*/
      aisob1=ws(3)                                                              !/*de         77*/
      if (ws(3)-14.d0) 120,80,120                                               !/*de         78*/
! for i+, if gf is less than aisobd, then goes to pion+ and n.                  !/*de         79*/
!  if greater, goes to pion0 and p.                                             !/*de         80*/
   80 if (gf-aisobd) 90,100,100                                                 !/*de         81*/
   90 wsp(3)=dp2                                                                !/*de         82*/
      ws(3)=dp3                                                                 !/*de         83*/
      wsp(24)=efn                                                               !/*de         84*/
      delta=efn-efp-csa(11)                                                     !/*de         85*/
      if (ws(31).eq.dp0) go to 110                                              !/*de         86*/
      call cep (delta,delt,gws4,gws32)                                          !/*de         87*/
      if ((delt+ws(5)).eq.dp0) go to 100                                        !/*de         88*/
      delta=delt                                                                !/*de         89*/
      go to 110                                                                 !/*de         90*/
  100 wsp(3)=dp1                                                                !/*de         91*/
      ws(3)=dp4                                                                 !/*de         92*/
      wsp(24)=efp                                                               !/*de         93*/
      delta=dp0                                                                 !/*de         94*/
  110 i=wsp(3)                                                                  !/*de         95*/
      wsp(4)=amass(i)                                                           !/*de         96*/
      i=ws(3)                                                                   !/*de         97*/
      ws(4)=amass(i)                                                            !/*de         98*/
      ws(24)=dp0                                                                !/*de         99*/
      wsp(9)=dp0                                                                !/*de        100*/
      ws(9)=dp0                                                                 !/*de        101*/
      wsp(19)=wsp(4)*wsp(4)                                                     !/*de        102*/
      go to 150                                                                 !/*de        103*/
! for i0, if gf is less than aisobd, then goes to pion- and p.                  !/*de        104*/
! if greater, goes to pion0 and n.                                              !/*de        105*/
  120 if (gf-aisobd) 140,130,130                                                !/*de        106*/
  130 ws(3)=dp4                                                                 !/*de        107*/
      wsp(3)=dp2                                                                !/*de        108*/
      delta=dp0                                                                 !/*de        109*/
      wsp(24)=efn                                                               !/*de        110*/
      go to 110                                                                 !/*de        111*/
  140 ws(3)=dp5                                                                 !/*de        112*/
      wsp(3)=dp1                                                                !/*de        113*/
      wsp(24)=efp                                                               !/*de        114*/
      delta=efp-efn+csa(11)                                                     !/*de        115*/
      if (ws(31).eq.dp0) go to 110                                              !/*de        116*/
      call cep (delta,delt,gws4,gws32)                                          !/*de        117*/
      if ((delt+ws(5)).eq.dp0) go to 130                                        !/*de        118*/
      delta=delt                                                                !/*de        119*/
      go to 110                                                                 !/*de        120*/
! statements 8 to 22 do pseudo center of mass transformation.                   !/*de        121*/
! the decaying isobar is already in its own center of mass system.              !/*de        122*/
!  statement 8 to 22 do what subr cma does.                                     !/*de        123*/
  150 cm(2)=gws(5)                                                              !/*de        124*/
      cm(4)=sqrt(gws(5)*gws(5)-wws(4)*wws(4))                                   !/*de        125*/
      cm(3)=wws(4)                                                              !/*de        126*/
      do 160 i=1,8                                                              !/*de        127*/
      cm(i+6)=dp0                                                               !/*de        128*/
  160 continue                                                                  !/*de        129*/
!     subr pih divides the energy available for decay in c.m. between           !/*de        130*/
!     the two decay particles. for more details see flow chart for pih          !/*de        131*/
      cm(25)=dp1                                                                !/*de        132*/
      cm(5)=dp1                                                                 !/*de        133*/
      cm(1)=dp0                                                                 !/*de        134*/
      cm(6)=dp0                                                                 !/*de        135*/
      if (ws(26).eq.dp10) go to 170                                             !/*de        136*/
      cm(5)=ws(28)                                                              !/*de        137*/
! temporary test                                                                !/*de        138*/
      temp=dp1-ws(28)*ws(28)                                                    !/*de        139*/
      if (temp.lt.dp0) then                                                     !/*de        140*/
        write (io,280) temp,ws(28)                                              !/*de        141*/
        temp=dp0                                                                !/*de        142*/
      endif                                                                     !/*de        143*/
      cm(6)=sqrt(temp)                                                          !/*de        144*/
  170 continue                                                                  !/*de        145*/
      call pih (delta)                                                          !/*de        146*/
! ind(18)=1 when the momentum available for decay is negative. this is          !/*de        147*/
!  an unallowed decay.                                                          !/*de        148*/
      if (ind(18)-1) 180,250,180                                                !/*de        149*/
! if it is allowed, choose the angle between the two decay partners             !/*de        150*/
!  using subr pangle.                                                           !/*de        151*/
  180 continue                                                                  !/*de        152*/
      if (ws(26).eq.dp10) go to 260                                             !/*de        153*/
      if (ws(29).eq.dp10) go to 270                                             !/*de        154*/
! on isobar exchange force isotropic decay                                      !/*de        155*/
      if (ws(3).ne.ws(29)) go to 240                                            !/*de        156*/
      if (ws(3)-dp4) 220,230,190                                                !/*de        157*/
  190 if (wsp(3)-dp2) 200,210,200                                               !/*de        158*/
  200 ind(10)=1                                                                 !/*de        159*/
      go to 240                                                                 !/*de        160*/
  210 ind(9)=1                                                                  !/*de        161*/
      go to 240                                                                 !/*de        162*/
  220 if (wsp(3)-dp2) 210,200,210                                               !/*de        163*/
  230 ind(11)=1                                                                 !/*de        164*/
  240 e=(wws(4)-ws(4)-wsp(4))                                                   !/*de        165*/
      call pangle (e,ind(9),ind(10),ind(11),ws(3),ws(29),ans)                   !/*de        166*/
  250 return                                                                    !/*de        167*/
  260 continue                                                                  !/*de        168*/
      ans=rang()                                                                !/*de        169*/
      ans=dp2*ans-dp1                                                           !/*de        170*/
      go to 250                                                                 !/*de        171*/
  270 continue                                                                  !/*de        172*/
! inelastic isobars decay as p2(costheta)                                       !/*de        173*/
      call angli1 (ans)                                                         !/*de        174*/
      return                                                                    !/*de        175*/
!                                                                               !/*de        176*/
  280 format (' decay',1p7e13.5)                                                !/*de        177*/
      end                                                                       !/*de        178*/

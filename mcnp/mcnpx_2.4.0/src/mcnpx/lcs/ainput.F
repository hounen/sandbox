!     $Id: ainput.F,v 1.5 2002/03/27 18:14:52 gwm Exp $                       !/*ai          1*/
      subroutine ainput (mdl)                                                   !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     ainput reads in the input data cards and print out some                   !/*ai          5*/
!     characteristics of the model used.                                        !/*ai          6*/
!                                                                               !/*ai          7*/
! AINPUT called by INISBL                                                       !/*ai          8*/
!     ==================================================================        !/*ai          9*/
!                                                                               !/*ai         10*/
! model=71  read format 21.                                                     !/*ai         11*/
! model=72  read format 22.                                                     !/*ai         12*/
! model=73  read format 23.                                                     !/*ai         13*/
! model=74  read format 24.                                                     !/*ai         14*/
! model=75  read format 25.                                                     !/*ai         15*/
! model=76  read format 26.                                                     !/*ai         16*/
!                                                                               !/*ai         17*/
#include <lcs/inout.h>                                                          /*!ai         18*/
#include <lcs/vegas.h>                                                          /*!ai         19*/
      model=mdl                                                                 !/*ai         20*/
      mdl=model                                                                 !/*ai         21*/
!                                                                               !/*ai         22*/
#ifdef LCS_PRINT                                                                /*!ai         23*/
      if (ntdel.le.0) then                                                      !/*ai         24*/
        write (io,250)                                                          !/*ai         25*/
  250   format (1h0,3x,' no cascade-cascade interactions'/)                     !/*ai         26*/
      else                                                                      !/*ai         27*/
        write (io,260) ntdel,rcas                                               !/*ai         28*/
  260   format (1h0,3x,'cascade-cascade interaction allowed every',i3,          !/*ai         29*/
     &  ' time intervals'/4x,'maximal inter-particle distance ',                !/*ai         30*/
     &  ' for a cascade-cascade interaction is ',f5.2,' fm.'/)                  !/*ai         31*/
      endif                                                                     !/*ai         32*/
!                                                                               !/*ai         33*/
      if (iden.lt.2) then                                                       !/*ai         34*/
        write (io,270) model,dx(3),timef                                        !/*ai         35*/
  270   format(1h0,3x,'data step model=',i3/4x,'number of steps=8',20x,         !/*ai         36*/
     &  'standard step size=',f8.3,'  fermi'/4x,'time interval factor=',        !/*ai         37*/
     &   f6.2)                                                                  !/*ai         38*/
      else                                                                      !/*ai         39*/
        write (io,280) model,dxp(10),timef                                      !/*ai         40*/
  280   format (1h0,3x,'data step model=',i3/4x,'number of steps=16',           !/*ai         41*/
     &   20x,'standard step size=',f8.3,'  fermi'/4x,                           !/*ai         42*/
     &  'time interval factor=',f6.2)                                           !/*ai         43*/
      endif                                                                     !/*ai         44*/
#endif                                                                          /*!ai         45*/
!                                                                               !/*ai         46*/
      ind(21)=model-70                                                          !/*ai         47*/
      ind21=ind(21)                                                             !/*ai         48*/
!                                                                               !/*ai         49*/
!           vpion is zeroed when necessary [mrc 28/july/83]                     !/*ai         50*/
!                                                                               !/*ai         51*/
      if (ind21.le.3) vpion=dp0                                                 !/*ai         52*/
!                                                                               !/*ai         53*/
#ifdef LCS_PRINT                                                                /*!ai         54*/
      go to (30,40,50,60,70,80), ind21                                          !/*ai         55*/
   30 write (io,290)                                                            !/*ai         56*/
      go to 90                                                                  !/*ai         57*/
   40 write (io,300)                                                            !/*ai         58*/
      go to 90                                                                  !/*ai         59*/
   50 write (io,310)                                                            !/*ai         60*/
      go to 90                                                                  !/*ai         61*/
   60 write (io,320)                                                            !/*ai         62*/
      go to 90                                                                  !/*ai         63*/
   70 write (io,330)                                                            !/*ai         64*/
      go to 90                                                                  !/*ai         65*/
   80 write (io,340)                                                            !/*ai         66*/
   90 if (isonsw.eq.0) go to 100                                                !/*ai         67*/
      write (io,350)                                                            !/*ai         68*/
      go to 110                                                                 !/*ai         69*/
  100 write (io,360)                                                            !/*ai         70*/
  110 if (ind21.ge.4) write (io,370) vpion                                      !/*ai         71*/
      if (gamiso.eq.dp0) write (io,380)                                         !/*ai         72*/
      if (gamiso.ne.dp0) write (io,390) gamiso                                  !/*ai         73*/
#endif                                                                          /*!ai         74*/
      iknp=abs(knp)                                                             !/*ai         75*/
#ifdef LCS_PRINT                                                                /*!ai         76*/
      if (np.ne.0) go to 150                                                    !/*ai         77*/
      if (knp.lt.0) write (io,480)                                              !/*ai         78*/
      if (knp.gt.0) write (io,490)                                              !/*ai         79*/
      if (iknp-2) 120,130,140                                                   !/*ai         80*/
  120 write (io,400) gapl                                                       !/*ai         81*/
      go to 150                                                                 !/*ai         82*/
  130 write (io,410)                                                            !/*ai         83*/
      go to 150                                                                 !/*ai         84*/
  140 write (io,420)                                                            !/*ai         85*/
  150 if (np.le.0) go to 160                                                    !/*ai         86*/
      if (iknp-2) 170,180,190                                                   !/*ai         87*/
  160 write (io,430)                                                            !/*ai         88*/
      go to 200                                                                 !/*ai         89*/
  170 write (io,440) gapl                                                       !/*ai         90*/
      go to 200                                                                 !/*ai         91*/
  180 write (io,450)                                                            !/*ai         92*/
      go to 200                                                                 !/*ai         93*/
  190 write (io,460)                                                            !/*ai         94*/
  200 write (io,470)                                                            !/*ai         95*/
!                                                                               !/*ai         96*/
      if (iden-1) 210,220,230                                                   !/*ai         97*/
  210 write (io,510)                                                            !/*ai         98*/
      go to 240                                                                 !/*ai         99*/
  220 write (io,520)                                                            !/*ai        100*/
      go to 240                                                                 !/*ai        101*/
  230 write (io,530)                                                            !/*ai        102*/
!                                                                               !/*ai        103*/
  240 continue                                                                  !/*ai        104*/
!                                                                               !/*ai        105*/
      if (kaskew.ne.0) write (io,500)                                           !/*ai        106*/
#endif                                                                          /*!ai        107*/
      return                                                                    !/*ai        108*/
!                                                                               !/*ai        109*/
  290 format (' isobar with refr and refl, no cutoff for isobars')              !/*ai        110*/
  300 format (' isobar with refr and refl, with cutoff for isobars')            !/*ai        111*/
  310 format (' isobar with no refr nor refl, with cutoff for isobars')         !/*ai        112*/
  320 format (' isobar with refr and refl, with pion potential,',               !/*ai        113*/
     & ' no cutoff for isobars')                                                !/*ai        114*/
  330 format (' isobar with refr and refl, with pion potential,',               !/*ai        115*/
     & ' with cutoff for isobars')                                              !/*ai        116*/
  340 format (' isobar with no refr nor refl, with pion potential,',            !/*ai        117*/
     & ' with cutoff for isobars')                                              !/*ai        118*/
  350 format (' isobar nucleon exchange scattering not allowed')                !/*ai        119*/
  360 format (' isobar nucleon exchange allowed')                               !/*ai        120*/
  370 format (' the pion potential is constant=',f10.2,' mev')                  !/*ai        121*/
  380 format (' energy dependent resonance width')                              !/*ai        122*/
  390 format (' energy independent resonance width=',f6.1,' mev')               !/*ai        123*/
  400 format (' the distance restriction is ',f6.2,' fm')                       !/*ai        124*/
  410 format (' the distance restriction is volume times density=1')            !/*ai        125*/
  420 format (' the distance restriction is 1/cubert(density)')                 !/*ai        126*/
  430 format (' the total density is reduced after each collision')             !/*ai        127*/
  440 format (' the minimal distance between any two collisions is',f6.2        !/*ai        128*/
     & ,' fm')                                                                  !/*ai        129*/
  450 format (' the minimal distance between any two collisions',               !/*ai        130*/
     & ' is (3/4pi/density)**1/3')                                              !/*ai        131*/
  460 format (' the minimal distance between any two collisions',               !/*ai        132*/
     & ' is (1/density)**1/3')                                                  !/*ai        133*/
  470 format (' the projectile is lorentz-contracted')                          !/*ai        134*/
  480 format (' the distance restriction is isospin in-dependent')              !/*ai        135*/
  490 format (' the distance restriction is isospin -dependent')                !/*ai        136*/
  500 format (' kaon-production is skewed, renorm with unskewed data ')         !/*ai        137*/
  510 format (' iden=0, myer''s prescription for (8) densities,'/               !/*ai        138*/
     & '         thomas-fermi local density approx for momenta')                !/*ai        139*/
  520 format (' iden=1, old (isobar) prescription for (8) densities'/           !/*ai        140*/
     & '         thomas-fermi local density approx for momenta')                !/*ai        141*/
  530 format (' iden=2, folded-yukawa prescription for (16) densities,'/        !/*ai        142*/
     &'         thomas-fermi local density approx for momenta')                 !/*ai        143*/
      end                                                                       !/*ai        144*/

!     $Id: bg6c.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                         !/*bg          1*/
      subroutine bg6c (int1)                                                    !/*bg          2*/
#include <lcs/double.h>                                                         /*!bg          3*/
#include <lcs/btcom.h>                                                          /*!bg          4*/
      if (knot-7) 10,260,260                                                    !/*bg          5*/
   10 aabs=dp0                                                                  !/*bg          6*/
      if (knot-2) 20,30,30                                                      !/*bg          7*/
   20 if (int1) 70,100,70                                                       !/*bg          8*/
   30 if (knot-5) 40,50,60                                                      !/*bg          9*/
   40 if (int1) 100,70,100                                                      !/*bg         10*/
   50 it=11                                                                     !/*bg         11*/
      if (int1) 80,110,80                                                       !/*bg         12*/
   60 it=12                                                                     !/*bg         13*/
      if (int1) 110,80,110                                                      !/*bg         14*/
   70 it=2*knot-1                                                               !/*bg         15*/
   80 strkp=-dp1                                                                !/*bg         16*/
   90 i1=0                                                                      !/*bg         17*/
      go to 130                                                                 !/*bg         18*/
  100 it=2*knot                                                                 !/*bg         19*/
  110 strkp=-dp2                                                                !/*bg         20*/
  120 i1=1                                                                      !/*bg         21*/
  130 i2=clsm                                                                   !/*bg         22*/
! subroutine bb coded inline                                                    !/*bg         23*/
      i=i2                                                                      !/*bg         24*/
!     collision cut-off energy (proton)                                         !/*bg         25*/
      if (i1.eq.0) go to 140                                                    !/*bg         26*/
      i=i+3                                                                     !/*bg         27*/
!     collision cut-off energy (neutron)                                        !/*bg         28*/
! BB coded inline                                                               !/*bg         29*/
  140 if (knot-6) 160,160,150                                                   !/*bg         30*/
  150 if (knot-12) 170,170,160                                                  !/*bg         31*/
  160 clcfe=cfepn(i)                                                            !/*bg         32*/
  170 e(1)=wkrpn(i)*rcpmv+pm(1)                                                 !/*bg         33*/
!     total energy particle 1                                                   !/*bg         34*/
      p1oe1=sqrt(e(1)*e(1)-pm(1)*pm(1))                                         !/*bg         35*/
      if (in1.eq.0) go to 180                                                   !/*bg         36*/
      pxyz(1)=p1oe1*curr(7)                                                     !/*bg         37*/
      pxyz(5)=p1oe1*curr(8)                                                     !/*bg         38*/
      pxyz(9)=p1oe1*curr(9)                                                     !/*bg         39*/
      go to 190                                                                 !/*bg         40*/
  180 continue                                                                  !/*bg         41*/
      pxyz(1)=dp0                                                               !/*bg         42*/
      pxyz(9)=p1oe1                                                             !/*bg         43*/
      pxyz(5)=dp0                                                               !/*bg         44*/
  190 continue                                                                  !/*bg         45*/
      p1oe1=p1oe1/e(1)                                                          !/*bg         46*/
  200 continue                                                                  !/*bg         47*/
! ISOM coded inline                                                             !/*bg         48*/
  210 continue                                                                  !/*bg         49*/
      z=rang()                                                                  !/*bg         50*/
      z=dp2*z-dp1                                                               !/*bg         51*/
      x=rang()                                                                  !/*bg         52*/
      x=dp2*x-dp1                                                               !/*bg         53*/
      y=rang()                                                                  !/*bg         54*/
      y=dp2*y-dp1                                                               !/*bg         55*/
      dd=x*x+y*y+z*z                                                            !/*bg         56*/
      if (dd.gt.dp1) go to 210                                                  !/*bg         57*/
      m=clsm+0.05d0                                                             !/*bg         58*/
      if (strkp.le.-dp2) m=m+3                                                  !/*bg         59*/
      fermn=fmpn(m)                                                             !/*bg         60*/
! p2 redefined as square of original here and in crjab                          !/*bg         61*/
      p2=fermn*fermn*dd                                                         !/*bg         62*/
!     frmic selects largest of 3 random numbers                                 !/*bg         63*/
!     p2=momentum of particle selected from proper                              !/*bg         64*/
!     fermi distribution                                                        !/*bg         65*/
!     p2 sin theta                                                              !/*bg         66*/
      pxyz(2)=fermn*x                                                           !/*bg         67*/
!     p2 sin theta cos phi                                                      !/*bg         68*/
      pxyz(6)=fermn*y                                                           !/*bg         69*/
!     p2 sin phi                                                                !/*bg         70*/
      pxyz(10)=fermn*z                                                          !/*bg         71*/
!     p2 cos theta                                                              !/*bg         72*/
      e(2)=sqrt(p2+sqnm)                                                        !/*bg         73*/
!     sq. rt. momentum struck part. sqd. +nucleon mass sqd.                     !/*bg         74*/
      rlke=(((e(1)*e(2)-pxyz(1)*pxyz(2)-pxyz(5)*pxyz(6)-pxyz(9)*pxyz(10)        !/*bg         75*/
     & )/dncms)-pm(1))/rcpmv                                                    !/*bg         76*/
!     relative k.e.(mev)--constant=nucleon mass         /cm,                    !/*bg         77*/
!     second=mev/cm.                                                            !/*bg         78*/
      go to (220,220,220,230,220,220,250,250,250,250,250,250,220,220,220        !/*bg         79*/
     & ,220,230,220,220), knot                                                  !/*bg         80*/
  220 if (rlke-25.d+02) 240,240,200                                             !/*bg         81*/
  230 rlke=dp0                                                                  !/*bg         82*/
  240 return                                                                    !/*bg         83*/
  250 if (rlke-35.d+02) 240,240,200                                             !/*bg         84*/
  260 if (knot-12) 270,270,370                                                  !/*bg         85*/
  270 if (in1) 420,280,420                                                      !/*bg         86*/
  280 if (knot-8) 290,340,380                                                   !/*bg         87*/
  290 if (int1) 300,320,300                                                     !/*bg         88*/
  300 it=2*(knot+1)                                                             !/*bg         89*/
  310 strkp=-dp2                                                                !/*bg         90*/
      go to 90                                                                  !/*bg         91*/
  320 it=2*knot+1                                                               !/*bg         92*/
  330 strkp=-dp1                                                                !/*bg         93*/
      go to 120                                                                 !/*bg         94*/
  340 if (int1) 350,360,350                                                     !/*bg         95*/
  350 it=2*(knot+1)                                                             !/*bg         96*/
      go to 80                                                                  !/*bg         97*/
  360 it=2*knot+1                                                               !/*bg         98*/
      go to 110                                                                 !/*bg         99*/
  370 if (knot-18) 380,500,500                                                  !/*bg        100*/
  380 it=knot+10                                                                !/*bg        101*/
      if (knot-10) 390,400,410                                                  !/*bg        102*/
  390 if (int1) 80,110,80                                                       !/*bg        103*/
  400 if (int1) 310,330,310                                                     !/*bg        104*/
  410 if (knot-12) 390,400,480                                                  !/*bg        105*/
  420 if (knot-8) 430,450,380                                                   !/*bg        106*/
  430 if (int1) 440,380,440                                                     !/*bg        107*/
  440 it=knot+11                                                                !/*bg        108*/
      go to 80                                                                  !/*bg        109*/
  450 if (int1) 470,460,470                                                     !/*bg        110*/
  460 it=2*knot-1                                                               !/*bg        111*/
      go to 330                                                                 !/*bg        112*/
  470 it=knot+knot                                                              !/*bg        113*/
      go to 310                                                                 !/*bg        114*/
  480 aabs=dp0                                                                  !/*bg        115*/
      if (knot-15) 80,110,490                                                   !/*bg        116*/
  490 if (knot-18) 110,80,110                                                   !/*bg        117*/
  500 it=28                                                                     !/*bg        118*/
      go to 480                                                                 !/*bg        119*/
      end                                                                       !/*bg        120*/

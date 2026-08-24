!     $Id: geo.F,v 1.4 2002/03/27 18:15:05 gwm Exp $                          !/*ge          1*/
      subroutine geo                                                            !/*ge          2*/
#include <lcs/double.h>                                                         /*!ge          3*/
#include <lcs/btcom.h>                                                          /*!ge          4*/
      i1=0                                                                      !/*ge          5*/
      t1=out(2)*out(2)                                                          !/*ge          6*/
!     r1sq                                                                      !/*ge          7*/
      t2=out(3)*out(3)                                                          !/*ge          8*/
!     (r1+1)sq                                                                  !/*ge          9*/
      t3=out(4)*out(4)                                                          !/*ge         10*/
!     (r1+2)sq                                                                  !/*ge         11*/
      t4=dp2*t3                                                                 !/*ge         12*/
!     2(r1+2)sq                                                                 !/*ge         13*/
   10 t5=xi(1)*xi(1)+xi(2)*xi(2)+xi(3)*xi(3)                                    !/*ge         14*/
!     t5=r sq                                                                   !/*ge         15*/
      go to (20,70,130,180), med                                                !/*ge         16*/
   20 t6=t5-t1                                                                  !/*ge         17*/
      if (t6) 230,230,30                                                        !/*ge         18*/
!     med=1                                                                     !/*ge         19*/
   30 temp=t1                                                                   !/*ge         20*/
   40 if ((t6/temp)-5.d-6) 50,50,220                                            !/*ge         21*/
   50 do 60 i=1,3                                                               !/*ge         22*/
      xi(i)=xi(i)*9.99995d-1                                                    !/*ge         23*/
   60 curr(i+3)=xi(i)                                                           !/*ge         24*/
      go to 10                                                                  !/*ge         25*/
   70 t6=t5-t1                                                                  !/*ge         26*/
!     med=2                                                                     !/*ge         27*/
      if (t6) 80,80,120                                                         !/*ge         28*/
   80 temp=t1                                                                   !/*ge         29*/
   90 if (5.d-6+(t6/temp)) 220,100,100                                          !/*ge         30*/
  100 do 110 i=1,3                                                              !/*ge         31*/
      xi(i)=xi(i)*10.00005d-1                                                   !/*ge         32*/
  110 curr(i+3)=xi(i)                                                           !/*ge         33*/
      go to 10                                                                  !/*ge         34*/
  120 t6=t5-t2                                                                  !/*ge         35*/
      temp=t2                                                                   !/*ge         36*/
      if (t6) 230,230,40                                                        !/*ge         37*/
  130 t6=t5-t2                                                                  !/*ge         38*/
!     med=3                                                                     !/*ge         39*/
      if (t6) 140,140,150                                                       !/*ge         40*/
  140 temp=t2                                                                   !/*ge         41*/
      go to 90                                                                  !/*ge         42*/
  150 t6=t5-t3                                                                  !/*ge         43*/
      if (t6) 230,230,160                                                       !/*ge         44*/
  160 temp=t3                                                                   !/*ge         45*/
      go to 40                                                                  !/*ge         46*/
!*********************************************                                  !/*ge         47*/
!                dead code                                                      !/*ge         48*/
!     if (xi(2)) 220,170,220                                                    !/*ge         49*/
!     med=4                                                                     !/*ge         50*/
! 170 if (curr(5)) 220,180,220                                                  !/*ge         51*/
!*********************************************                                  !/*ge         52*/
  180 t6=t5-t3                                                                  !/*ge         53*/
      if (t6) 190,190,200                                                       !/*ge         54*/
  190 temp=t3                                                                   !/*ge         55*/
      go to 90                                                                  !/*ge         56*/
  200 t6=t5-t4                                                                  !/*ge         57*/
      if (t6) 230,230,210                                                       !/*ge         58*/
  210 temp=t4                                                                   !/*ge         59*/
      go to 40                                                                  !/*ge         60*/
  220 i1=-1                                                                     !/*ge         61*/
      go to 280                                                                 !/*ge         62*/
  230 t4=xi(1)*dcos(1)+xi(2)*dcos(2)+xi(3)*dcos(3)                              !/*ge         63*/
!     t4=-b=-rcos(theta)=sum of xi(i)*dcos(i)                                   !/*ge         64*/
      t6=t4*t4                                                                  !/*ge         65*/
!     t5=r sq.                                                                  !/*ge         66*/
      t6=t5-t6                                                                  !/*ge         67*/
!     t6=r sq.-b sq.                                                            !/*ge         68*/
      if (t3-t6) 220,240,240                                                    !/*ge         69*/
  240 t3=sqrt(t3-t6)                                                            !/*ge         70*/
!     t3=a3=sq.root of b sq.-r sq.+radius3 sq. similar                          !/*ge         71*/
!     for t2=a2 andt1=a1                                                        !/*ge         72*/
      temp=t2-t6                                                                !/*ge         73*/
      t2=sqrt(abs(temp))                                                        !/*ge         74*/
      tempo=t1-t6                                                               !/*ge         75*/
      t1=sqrt(abs(tempo))                                                       !/*ge         76*/
      do 250 i=1,6                                                              !/*ge         77*/
  250 d(i)=dp0                                                                  !/*ge         78*/
      go to (260,290,340,400), med                                              !/*ge         79*/
  260 if (temp) 220,270,270                                                     !/*ge         80*/
  270 d(4)=t1-t4                                                                !/*ge         81*/
!     b+a1                                                                      !/*ge         82*/
      d(5)=t2-t1                                                                !/*ge         83*/
!     a2-a1                                                                     !/*ge         84*/
      d(6)=t3-t2                                                                !/*ge         85*/
!     a3-a2                                                                     !/*ge         86*/
  280 return                                                                    !/*ge         87*/
  290 if (temp) 220,300,300                                                     !/*ge         88*/
  300 d(6)=t3-t2                                                                !/*ge         89*/
      if (t4) 320,310,310                                                       !/*ge         90*/
  310 d(3)=t2-t4                                                                !/*ge         91*/
!     b+a2                                                                      !/*ge         92*/
      go to 280                                                                 !/*ge         93*/
  320 if (tempo) 310,330,330                                                    !/*ge         94*/
  330 d(3)=-(t4+t1)                                                             !/*ge         95*/
!     b-a1                                                                      !/*ge         96*/
      d(4)=t1+t1                                                                !/*ge         97*/
!     2a1                                                                       !/*ge         98*/
      d(5)=t2-t1                                                                !/*ge         99*/
!     a2-a1                                                                     !/*ge        100*/
      go to 280                                                                 !/*ge        101*/
  340 if (t4) 360,350,350                                                       !/*ge        102*/
  350 d(2)=t3-t4                                                                !/*ge        103*/
!     b+a3                                                                      !/*ge        104*/
      go to 280                                                                 !/*ge        105*/
  360 if (temp) 350,370,370                                                     !/*ge        106*/
  370 d(2)=-(t4+t2)                                                             !/*ge        107*/
!     b-a2                                                                      !/*ge        108*/
      d(6)=t3-t2                                                                !/*ge        109*/
!     a3-a2                                                                     !/*ge        110*/
      if (tempo) 380,390,390                                                    !/*ge        111*/
  380 d(3)=t2+t2                                                                !/*ge        112*/
!     2a2                                                                       !/*ge        113*/
      go to 280                                                                 !/*ge        114*/
  390 d(3)=t2-t1                                                                !/*ge        115*/
!     a2-a1                                                                     !/*ge        116*/
      d(5)=d(3)                                                                 !/*ge        117*/
      d(4)=t1+t1                                                                !/*ge        118*/
!     2a1                                                                       !/*ge        119*/
      go to 280                                                                 !/*ge        120*/
  400 d(1)=-(t4+t3)                                                             !/*ge        121*/
      if (temp) 410,420,420                                                     !/*ge        122*/
  410 d(2)=t3+t3                                                                !/*ge        123*/
      go to 280                                                                 !/*ge        124*/
  420 d(2)=t3-t2                                                                !/*ge        125*/
      d(6)=d(2)                                                                 !/*ge        126*/
!     b-a3, a3-a2,region 4                                                      !/*ge        127*/
      if (tempo) 440,430,430                                                    !/*ge        128*/
  430 d(3)=t2-t1                                                                !/*ge        129*/
!     a2-a1                                                                     !/*ge        130*/
      d(5)=d(3)                                                                 !/*ge        131*/
      d(4)=t1+t1                                                                !/*ge        132*/
      go to 280                                                                 !/*ge        133*/
  440 d(3)=t2+t2                                                                !/*ge        134*/
!     2a2                                                                       !/*ge        135*/
      go to 280                                                                 !/*ge        136*/
      end                                                                       !/*ge        137*/

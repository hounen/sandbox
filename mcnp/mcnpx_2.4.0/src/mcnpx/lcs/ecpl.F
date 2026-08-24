!     $Id: ecpl.F,v 1.4 2002/03/27 18:15:01 gwm Exp $                         !/*ec          1*/
      subroutine ecpl                                                           !/*ec          2*/
#include <lcs/double.h>                                                         /*!ec          3*/
#include <lcs/btcom.h>                                                          /*!ec          4*/
      i1=0                                                                      !/*ec          5*/
      med=clsm                                                                  !/*ec          6*/
      if (pt(38)) 590,10,590                                                    !/*ec          7*/
   10 if (curr(1)-dp1) 380,540,20                                               !/*ec          8*/
   20 if (curr(1)-dp3) 450,420,30                                               !/*ec          9*/
   30 if (curr(1)-dp5) 320,40,380                                               !/*ec         10*/
   40 if (strkp+dp1) 50,60,380                                                  !/*ec         11*/
   50 if (strkp+dp2) 380,230,380                                                !/*ec         12*/
   60 if (pt(2)-dp2) 80,70,80                                                   !/*ec         13*/
   70 pt(3)=vnvp(med)                                                           !/*ec         14*/
      go to 90                                                                  !/*ec         15*/
!     pi-,curr(1)=5.0,pt6+1=0,strkp=dp1                                         !/*ec         16*/
   80 pt(3)=dp0                                                                 !/*ec         17*/
   90 pt(15)=hvp(med)                                                           !/*ec         18*/
      if (pt(26)-dp1) 380,190,100                                               !/*ec         19*/
  100 if (pt(26)-dp2) 380,110,380                                               !/*ec         20*/
  110 pt(27)=-ppan(med)                                                         !/*ec         21*/
      if (curr(1)-dp2) 580,120,120                                              !/*ec         22*/
!     ppan=-vnhp(neut.well depth-1/2proton well depth)                          !/*ec         23*/
  120 if (curr(1)-dp4) 440,380,130                                              !/*ec         24*/
  130 if (pt(2)-dp3) 380,150,140                                                !/*ec         25*/
  140 if (pt(2)-dp5) 170,180,380                                                !/*ec         26*/
  150 if (pt(14)-dp5) 380,160,380                                               !/*ec         27*/
  160 return                                                                    !/*ec         28*/
  170 if (pt(14)-dp4) 380,160,380                                               !/*ec         29*/
  180 if (pt(14)-dp3) 380,160,380                                               !/*ec         30*/
!     1/2prot.well depth                                                        !/*ec         31*/
  190 pt(27)=hvp(med)                                                           !/*ec         32*/
      if (curr(1)-dp2) 560,200,200                                              !/*ec         33*/
  200 if (curr(1)-dp4) 400,380,210                                              !/*ec         34*/
  210 if (pt(2)-dp4) 380,150,220                                                !/*ec         35*/
  220 if (pt(2)-dp5) 380,170,380                                                !/*ec         36*/
  230 pt(3)=-vnvp(med)                                                          !/*ec         37*/
  240 pt(15)=-pmac(med)                                                         !/*ec         38*/
  250 if (pt(26)-dp1) 380,270,260                                               !/*ec         39*/
  260 if (pt(26)-dp2) 380,300,380                                               !/*ec         40*/
  270 pt(27)=-pmac(med)                                                         !/*ec         41*/
      if (curr(1)-dp2) 150,150,280                                              !/*ec         42*/
  280 if (curr(1)-dp4) 130,210,290                                              !/*ec         43*/
  290 if (pt(2)-dp5) 380,150,380                                                !/*ec         44*/
  300 pt(27)=hvn(med)                                                           !/*ec         45*/
      if (curr(1)-dp2) 170,170,310                                              !/*ec         46*/
  310 if (curr(1)-dp4) 400,130,210                                              !/*ec         47*/
  320 if (strkp+dp1) 330,340,380                                                !/*ec         48*/
  330 if (strkp+dp2) 380,230,380                                                !/*ec         49*/
!     pi0                                                                       !/*ec         50*/
  340 pt(3)=dp0                                                                 !/*ec         51*/
  350 pt(15)=hvp(med)                                                           !/*ec         52*/
      if (pt(26)-dp1) 380,370,360                                               !/*ec         53*/
  360 if (pt(26)-dp2) 380,390,380                                               !/*ec         54*/
  370 pt(27)=hvp(med)                                                           !/*ec         55*/
      if (curr(1)-dp4) 170,130,380                                              !/*ec         56*/
  380 i1=1                                                                      !/*ec         57*/
      go to 160                                                                 !/*ec         58*/
  390 pt(27)=-ppan(med)                                                         !/*ec         59*/
      if (curr(1)-dp4) 180,400,380                                              !/*ec         60*/
  400 if (pt(2)-dp3) 380,170,410                                                !/*ec         61*/
  410 if (pt(2)-dp4) 380,180,380                                                !/*ec         62*/
  420 if (strkp+dp1) 430,60,380                                                 !/*ec         63*/
!     pi+                                                                       !/*ec         64*/
  430 if (strkp+dp2) 380,230,380                                                !/*ec         65*/
  440 if (pt(2)-dp3) 380,180,380                                                !/*ec         66*/
  450 if (strkp+dp1) 460,470,380                                                !/*ec         67*/
!     neutron                                                                   !/*ec         68*/
  460 if (strkp+dp2) 380,490,380                                                !/*ec         69*/
  470 pt(3)=dp0                                                                 !/*ec         70*/
      if (pt(2)-dp1) 380,240,480                                                !/*ec         71*/
  480 if (pt(2)-dp2) 380,350,380                                                !/*ec         72*/
  490 pt(15)=-pmac(med)                                                         !/*ec         73*/
      if (pt(2)-dp1) 380,510,500                                                !/*ec         74*/
  500 if (pt(2)-dp2) 380,530,380                                                !/*ec         75*/
  510 if (pt(26)-dp2) 380,520,380                                               !/*ec         76*/
  520 pt(3)=-vnvp(med)                                                          !/*ec         77*/
      pt(27)=hvn(med)                                                           !/*ec         78*/
      go to 150                                                                 !/*ec         79*/
  530 pt(3)=dp0                                                                 !/*ec         80*/
      go to 250                                                                 !/*ec         81*/
  540 if (strkp+dp1) 550,60,380                                                 !/*ec         82*/
  550 if (strkp+dp2) 380,470,380                                                !/*ec         83*/
!     proton                                                                    !/*ec         84*/
  560 if (pt(2)-dp1) 380,170,570                                                !/*ec         85*/
  570 if (pt(2)-dp2) 380,180,380                                                !/*ec         86*/
  580 if (pt(2)-dp1) 380,180,380                                                !/*ec         87*/
  590 if (curr(1)-dp1) 380,610,600                                              !/*ec         88*/
  600 if (curr(1)-dp2) 380,630,380                                              !/*ec         89*/
  610 if (strkp+dp1) 620,840,380                                                !/*ec         90*/
  620 if (strkp+dp2) 380,650,380                                                !/*ec         91*/
  630 if (strkp+dp1) 640,650,380                                                !/*ec         92*/
  640 if (strkp+dp2) 380,950,380                                                !/*ec         93*/
  650 if (pt(14)) 380,380,660                                                   !/*ec         94*/
  660 if (pt(38)) 380,380,670                                                   !/*ec         95*/
  670 if (pt(38)-dp2) 680,740,380                                               !/*ec         96*/
  680 if (pt(14)-dp2) 690,800,380                                               !/*ec         97*/
  690 pt(3)=tffn(med)                                                           !/*ec         98*/
      pt(15)=tffn(med)                                                          !/*ec         99*/
      pt(27)=tffn(med)                                                          !/*ec        100*/
      pt(39)=tffn(med)                                                          !/*ec        101*/
  700 if (pt(2)-dp3) 380,380,710                                                !/*ec        102*/
  710 if (pt(2)-dp5) 730,720,380                                                !/*ec        103*/
  720 if (pt(26)-dp4) 380,160,380                                               !/*ec        104*/
  730 if (pt(26)-dp5) 380,160,380                                               !/*ec        105*/
  740 if (pt(14)-dp2) 750,810,380                                               !/*ec        106*/
  750 pt(27)=ffptfn(med)                                                        !/*ec        107*/
      pt(3)=fvnp(med)                                                           !/*ec        108*/
  760 pt(39)=fvnp(med)                                                          !/*ec        109*/
      pt(15)=fvnp(med)                                                          !/*ec        110*/
  770 if (pt(2)-dp3) 380,730,780                                                !/*ec        111*/
  780 if (pt(2)-dp5) 720,790,380                                                !/*ec        112*/
  790 if (pt(26)-dp3) 380,160,380                                               !/*ec        113*/
  800 pt(3)=ffptfn(med)                                                         !/*ec        114*/
      pt(27)=fvnp(med)                                                          !/*ec        115*/
      go to 760                                                                 !/*ec        116*/
  810 pt(3)=tffn(med)                                                           !/*ec        117*/
      pt(27)=tffn(med)                                                          !/*ec        118*/
      pt(15)=tffp(med)                                                          !/*ec        119*/
      pt(39)=tffp(med)                                                          !/*ec        120*/
  820 if (pt(2)-dp3) 380,720,830                                                !/*ec        121*/
  830 if (pt(2)-dp4) 380,790,380                                                !/*ec        122*/
  840 if (pt(14)) 380,380,850                                                   !/*ec        123*/
  850 if (pt(38)-dp1) 380,870,860                                               !/*ec        124*/
  860 if (pt(38)-dp2) 380,920,380                                               !/*ec        125*/
  870 if (pt(14)-dp2) 880,900,380                                               !/*ec        126*/
  880 pt(3)=hvp(med)                                                            !/*ec        127*/
  890 pt(15)=hvp(med)                                                           !/*ec        128*/
      pt(27)=hvp(med)                                                           !/*ec        129*/
      pt(39)=hvp(med)                                                           !/*ec        130*/
      if (pt(14)-dp2) 770,700,380                                               !/*ec        131*/
  900 pt(27)=hvn(med)                                                           !/*ec        132*/
  910 pt(3)=-pmac(med)                                                          !/*ec        133*/
      pt(39)=hvn(med)                                                           !/*ec        134*/
      pt(15)=hvn(med)                                                           !/*ec        135*/
      if (pt(38)-dp2) 820,770,380                                               !/*ec        136*/
  920 if (pt(14)-dp2) 930,940,380                                               !/*ec        137*/
  930 pt(3)=hvn(med)                                                            !/*ec        138*/
      pt(15)=hvn(med)                                                           !/*ec        139*/
      pt(27)=-pmac(med)                                                         !/*ec        140*/
      pt(39)=hvn(med)                                                           !/*ec        141*/
      go to 820                                                                 !/*ec        142*/
  940 pt(15)=-ppan(med)                                                         !/*ec        143*/
      pt(39)=-ppan(med)                                                         !/*ec        144*/
      pt(3)=hvp(med)                                                            !/*ec        145*/
      pt(27)=hvp(med)                                                           !/*ec        146*/
      if (pt(2)-dp3) 380,790,380                                                !/*ec        147*/
  950 if (pt(14)) 380,380,960                                                   !/*ec        148*/
  960 if (pt(38)-dp1) 380,980,970                                               !/*ec        149*/
  970 if (pt(38)-dp2) 380,1010,380                                              !/*ec        150*/
  980 if (pt(14)-dp2) 990,1000,380                                              !/*ec        151*/
  990 pt(39)=-pmac(med)                                                         !/*ec        152*/
      pt(15)=-pmac(med)                                                         !/*ec        153*/
      pt(27)=-pmac(med)                                                         !/*ec        154*/
      pt(3)=-pmac(med)                                                          !/*ec        155*/
      if (pt(2)-dp5) 380,730,380                                                !/*ec        156*/
 1000 pt(3)=thpn(med)                                                           !/*ec        157*/
      go to 890                                                                 !/*ec        158*/
 1010 if (pt(14)-dp2) 1020,1030,380                                             !/*ec        159*/
 1020 pt(3)=hvp(med)                                                            !/*ec        160*/
      pt(15)=hvp(med)                                                           !/*ec        161*/
      pt(39)=hvp(med)                                                           !/*ec        162*/
      pt(27)=thpn(med)                                                          !/*ec        163*/
      go to 700                                                                 !/*ec        164*/
 1030 pt(27)=-pmac(med)                                                         !/*ec        165*/
      go to 910                                                                 !/*ec        166*/
      end                                                                       !/*ec        167*/

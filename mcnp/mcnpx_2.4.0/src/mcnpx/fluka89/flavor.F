!     $Id: flavor.F,v 1.5 2002/03/27 18:14:43 gwm Exp $                       !/*fl          1*/
      subroutine flavor (it,lt,ll,e0,iv,re,kfr1,kfr2,isu,bete,kfa1,kfa2         !/*fl          2*/
     & ,kfa3,kfa4,iopt)                                                         !/*fl          3*/
!                                                                     *         !/*fl          4*/
!---------------------------------------------------------------------*         !/*fl          5*/
!                                                                     *         !/*fl          6*/
#include <lcs/double.h>                                                         /*!fl          7*/
#include <fluka89/flprm1.h>                                                     /*!fl          8*/
#include <fluka89/flprm2.h>                                                     /*!fl          9*/
#include <lcs/inout.h>                                                          /*!fl         10*/
      dimension re(*), kfr1(*), kfr2(*), iv(*)                                  !/*fl         11*/
!                                                                               !/*fl         12*/
      if (lt.eq.1) write (io,200) it,lt,ll,e0,isu,bete,kfa1,kfa2                !/*fl         13*/
      um=0.3d0                                                                  !/*fl         14*/
      sm=0.5d0                                                                  !/*fl         15*/
      cm=2.1d0                                                                  !/*fl         16*/
      bm=5.0d0                                                                  !/*fl         17*/
      i=it                                                                      !/*fl         18*/
      j=it-1                                                                    !/*fl         19*/
      iva=1                                                                     !/*fl         20*/
      ivx=iv(i)                                                                 !/*fl         21*/
!                                                                               !/*fl         22*/
      pd=dp0                                                                    !/*fl         23*/
      kf1=0                                                                     !/*fl         24*/
      kf2=0                                                                     !/*fl         25*/
      kf3=0                                                                     !/*fl         26*/
      kf4=0                                                                     !/*fl         27*/
!                                                                               !/*fl         28*/
      if (i.gt.1) go to 10                                                      !/*fl         29*/
      kx1=kfa1                                                                  !/*fl         30*/
      kx2=0                                                                     !/*fl         31*/
      if (iopt.eq.2) kx1=kfa1                                                   !/*fl         32*/
      if (iopt.eq.2) kx2=kfa2                                                   !/*fl         33*/
      if (iopt.eq.3.and.ll.eq.1) kx1=kfa2                                       !/*fl         34*/
      if (iopt.eq.4.and.kfa1.le.6.and.ll.eq.1) kx1=kfa2                         !/*fl         35*/
      if (iopt.eq.4.and.kfa1.le.6.and.ll.eq.1) kx2=kfa3                         !/*fl         36*/
      if (iopt.eq.4.and.kfa1.gt.6.and.ll.eq.0) kx1=kfa2                         !/*fl         37*/
      if (iopt.eq.4.and.kfa1.gt.6.and.ll.eq.0) kx2=kfa3                         !/*fl         38*/
      if (iopt.eq.5.and.ll.eq.0) kx1=kfa3                                       !/*fl         39*/
      if (iopt.eq.5.and.ll.eq.0) kx2=kfa4                                       !/*fl         40*/
      if (iopt.eq.5.and.ll.eq.1) kx1=kfa1                                       !/*fl         41*/
      if (iopt.eq.5.and.ll.eq.1) kx2=kfa2                                       !/*fl         42*/
   10 continue                                                                  !/*fl         43*/
      if (i.gt.1) kx1=kfr1(j)                                                   !/*fl         44*/
      if (i.gt.1) kx2=kfr2(j)                                                   !/*fl         45*/
      bet=bete                                                                  !/*fl         46*/
      if (kx1.gt.0.and.kx2.gt.0) bet=10.d0                                      !/*fl         47*/
      z1=rang()                                                                 !/*fl         48*/
      z2=rang()                                                                 !/*fl         49*/
      if (i.eq.1) rx=e0                                                         !/*fl         50*/
      if (i.gt.1) rx=re(j)                                                      !/*fl         51*/
      if (isu.eq.2) go to 20                                                    !/*fl         52*/
      if (isu.eq.3) go to 30                                                    !/*fl         53*/
      if (isu.eq.4) go to 40                                                    !/*fl         54*/
   20 if (z1.le.1.d0) kf1=1                                                     !/*fl         55*/
      if (z1.le.pd) kf1=2                                                       !/*fl         56*/
      if (z2.le.1.d0) kf2=1                                                     !/*fl         57*/
      if (z2.le.pd) kf2=2                                                       !/*fl         58*/
      pu=0.5d0                                                                  !/*fl         59*/
      pd=0.5d0                                                                  !/*fl         60*/
      ps=0.d0                                                                   !/*fl         61*/
      pc=0.d0                                                                   !/*fl         62*/
      go to 100                                                                 !/*fl         63*/
   30 continue                                                                  !/*fl         64*/
      if (rx.le.1.019) go to 20                                                 !/*fl         65*/
      x1=rx                                                                     !/*fl         66*/
      x2=um                                                                     !/*fl         67*/
      pu=beta(x1,x2,bet)                                                        !/*fl         68*/
      x2=sm                                                                     !/*fl         69*/
      ps=beta(x1,x2,bet)                                                        !/*fl         70*/
      pu1=pu/(2*pu+ps)                                                          !/*fl         71*/
      ps=ps/(2.d0*pu+ps)                                                        !/*fl         72*/
      pu=pu1                                                                    !/*fl         73*/
      pd=pu                                                                     !/*fl         74*/
      if (a.eq.1.d0) pu=0.34d0*2.d0*pu1                                         !/*fl         75*/
      if (a.eq.1.d0) pd=0.66d0*2.d0*pu1                                         !/*fl         76*/
      if (a.eq.2.d0) pu=0.66d0*2.d0*pu1                                         !/*fl         77*/
      if (a.eq.2.d0) pd=0.34d0*2.d0*pu1                                         !/*fl         78*/
      pc=0.d0                                                                   !/*fl         79*/
      if (z1.le.1.d0) kf1=3                                                     !/*fl         80*/
      if (z1.le.1.d0-ps) kf1=2                                                  !/*fl         81*/
      if (z1.le.pu) kf1=1                                                       !/*fl         82*/
      if (z2.le.1.d0) kf2=3                                                     !/*fl         83*/
      if (z2.le.1.d0-ps) kf2=2                                                  !/*fl         84*/
      if (z2.le.pu) kf2=1                                                       !/*fl         85*/
      go to 100                                                                 !/*fl         86*/
   40 continue                                                                  !/*fl         87*/
      a=0.d0                                                                    !/*fl         88*/
      go to (50,60,70,80,80,50,60,70,80,80), ivx                                !/*fl         89*/
   50 gm=2.140d0                                                                !/*fl         90*/
      if (kx1.eq.4.or.kx1.eq.10) gm=3.684d0                                     !/*fl         91*/
      if (kx1.eq.1.or.kx1.eq.7) a=1.d0                                          !/*fl         92*/
      if (kx1.eq.2.or.kx1.eq.8) a=2.d0                                          !/*fl         93*/
      go to 90                                                                  !/*fl         94*/
   60 gm=2.140d0                                                                !/*fl         95*/
      if (kx1.eq.4.or.kx1.eq.10) gm=4.89d0                                      !/*fl         96*/
      go to 90                                                                  !/*fl         97*/
   70 gm=3.85d0                                                                 !/*fl         98*/
      if (kx1.eq.4.and.kx2.eq.4) gm=4.89d0                                      !/*fl         99*/
      if (kx1.ne.4.and.kx2.ne.4) gm=2.770d0                                     !/*fl        100*/
      if (kx1.eq.10.and.kx2.eq.10) gm=4.89d0                                    !/*fl        101*/
      if (kx1.ne.10.and.kx2.ne.10) gm=2.770d0                                   !/*fl        102*/
      go to 90                                                                  !/*fl        103*/
   80 gm=3.684d0                                                                !/*fl        104*/
      if (kx1.ne.4.and.kx2.ne.4) gm=2.140d0                                     !/*fl        105*/
      if (kx1.ne.10.and.kx2.ne.10) gm=2.140d0                                   !/*fl        106*/
      if (ivx.eq.4.or.ivx.eq.9) kaxi=kx1                                        !/*fl        107*/
      if (ivx.eq.5.or.ivx.eq.10) kaxi=kx2                                       !/*fl        108*/
      if (kaxi.eq.1.or.kaxi.eq.7) a=1.d0                                        !/*fl        109*/
      if (kaxi.eq.2.or.kaxi.eq.8) a=2.d0                                        !/*fl        110*/
      go to 90                                                                  !/*fl        111*/
   90 continue                                                                  !/*fl        112*/
      if (rx.le.gm) go to 30                                                    !/*fl        113*/
      x1=rx                                                                     !/*fl        114*/
      x2=um                                                                     !/*fl        115*/
      pu=beta(x1,x2,bet)                                                        !/*fl        116*/
      x2=sm                                                                     !/*fl        117*/
      ps=beta(x1,x2,bet)                                                        !/*fl        118*/
      x2=cm                                                                     !/*fl        119*/
      pc=beta(x1,x2,bet)                                                        !/*fl        120*/
      pu1=pu/(2*pu+ps+pc)                                                       !/*fl        121*/
      ps1=ps/(2*pu+ps+pc)                                                       !/*fl        122*/
      pc1=pc/(2*pu+ps+pc)                                                       !/*fl        123*/
      pu=pu1                                                                    !/*fl        124*/
      pd=pu                                                                     !/*fl        125*/
      if (a.eq.1.d0) pu=0.34d0*2.d0*pu1                                         !/*fl        126*/
      if (a.eq.1.d0) pd=0.66d0*2.d0*pu1                                         !/*fl        127*/
      if (a.eq.2.d0) pu=0.66d0*2.d0*pu1                                         !/*fl        128*/
      if (a.eq.2.d0) pd=0.34d0*2.d0*pu1                                         !/*fl        129*/
      ps=ps1                                                                    !/*fl        130*/
      pc=pc1                                                                    !/*fl        131*/
      if (z1.le.1.d0) kf1=4                                                     !/*fl        132*/
      if (z1.le.1.d0-pc) kf1=3                                                  !/*fl        133*/
      if (z1.le.pu+pd) kf1=2                                                    !/*fl        134*/
      if (z1.le.pu) kf1=1                                                       !/*fl        135*/
      if (z2.le.1.d0) kf2=4                                                     !/*fl        136*/
      if (z2.le.1.d0-pc) kf2=3                                                  !/*fl        137*/
      if (z2.le.pu+pd) kf2=2                                                    !/*fl        138*/
      if (z2.le.pu) kf2=1                                                       !/*fl        139*/
      go to 100                                                                 !/*fl        140*/
  100 continue                                                                  !/*fl        141*/
      ivx=iv(i)                                                                 !/*fl        142*/
      go to (110,120,130,140,150,110,120,130,140,150), ivx                      !/*fl        143*/
  110 kfr1(i)=kf1                                                               !/*fl        144*/
      kfr2(i)=0                                                                 !/*fl        145*/
      if (ll.eq.1) kfr1(i)=kf1+6                                                !/*fl        146*/
      go to 160                                                                 !/*fl        147*/
  120 kfr1(i)=kf1+6                                                             !/*fl        148*/
      kfr2(i)=kf2+6                                                             !/*fl        149*/
      if (ll.eq.1) kfr1(i)=kf1                                                  !/*fl        150*/
      if (ll.eq.1) kfr2(i)=kf2                                                  !/*fl        151*/
      go to 160                                                                 !/*fl        152*/
  130 kfr1(i)=kf1                                                               !/*fl        153*/
      kfr2(i)=0                                                                 !/*fl        154*/
      if (ll.eq.1) kfr1(i)=kf1+6                                                !/*fl        155*/
      go to 160                                                                 !/*fl        156*/
  140 kfr1(i)=kf1+6                                                             !/*fl        157*/
      kfr2(i)=kx2                                                               !/*fl        158*/
      if (ll.eq.1) kfr1(i)=kf1                                                  !/*fl        159*/
      go to 160                                                                 !/*fl        160*/
  150 kfr1(i)=kx1                                                               !/*fl        161*/
      kfr2(i)=kf2+6                                                             !/*fl        162*/
      if (ll.eq.1) kfr2(i)=kf2                                                  !/*fl        163*/
      go to 160                                                                 !/*fl        164*/
  160 continue                                                                  !/*fl        165*/
      if (lt.eq.0) go to 190                                                    !/*fl        166*/
      write (io,210) pu,pd,ps,pc,kx1,kx2                                        !/*fl        167*/
      if (i.eq.1) go to 170                                                     !/*fl        168*/
      write (io,220) iv(i),ll,kfr1(j),kfr2(j),kfr1(i),kfr2(i)                   !/*fl        169*/
      go to 180                                                                 !/*fl        170*/
  170 write (io,230) iv(i),ll,kfa1,kfa2,kfr1(i),kfr2(i)                         !/*fl        171*/
  180 continue                                                                  !/*fl        172*/
  190 continue                                                                  !/*fl        173*/
      return                                                                    !/*fl        174*/
!                                                                               !/*fl        175*/
  200 format (3i5,e12.4,i5,e12.4,2i5,                                           !/*fl        176*/
     & ' flavor it,lt,ll,e0,isu,bete,kfa1,kfa2')                                !/*fl        177*/
  210 format (1h0,' flavor pu,pd,ps,pc,kx1,kx2',4f8.4,2i5)                      !/*fl        178*/
  220 format (1h0,22hiv,ll,q1a,q2a,q1n,q2n=,6i3)                                !/*fl        179*/
  230 format (1h0,'iv(i),ll,kfa1,kfa2,kfr1(i),kfr2(i)=',6i3)                    !/*fl        180*/
      end                                                                       !/*fl        181*/

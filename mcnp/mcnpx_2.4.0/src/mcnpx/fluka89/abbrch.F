!     $Id: abbrch.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                       !/*ab          1*/
      subroutine abbrch (it,ll,la,lt,e0,pgx,pgy,pgz,kfr1,kfr2,re,kr1r           !/*ab          2*/
     & ,kr2r,kr1l,kr2l,rpx,rpy,rpz,rpxr,rpyr,rpzr,rpxl,rpyl,rpzl,rer,rel        !/*ab          3*/
     & ,iv,b1,b2,kfa1,kfa2,kfa3,kfa4,iopt,iyy)                                  !/*ab          4*/
!                                                                     *         !/*ab          5*/
!---------------------------------------------------------------------*         !/*ab          6*/
!                                                                     *         !/*ab          7*/
#include <lcs/double.h>                                                         /*!ab          8*/
#include <fluka89/flprm1.h>                                                     /*!ab          9*/
#include <fluka89/flprm2.h>                                                     /*!ab         10*/
#include <lcs/inout.h>                                                          /*!ab         11*/
      dimension re(*), kfr1(*), kfr2(*), rpx(*), rpy(*), iv(*)                  !/*ab         12*/
      i=it                                                                      !/*ab         13*/
      j=it-1                                                                    !/*ab         14*/
      iva=1                                                                     !/*ab         15*/
      rm=0.764d0                                                                !/*ab         16*/
      rmk=0.891d0                                                               !/*ab         17*/
      dm=1.863d0                                                                !/*ab         18*/
      if (la.eq.1) go to 30                                                     !/*ab         19*/
      if (i.gt.1) go to 10                                                      !/*ab         20*/
      am0=1.d0                                                                  !/*ab         21*/
      kfaa=kfa1                                                                 !/*ab         22*/
      if (iopt.eq.2) kfaa=max0(kfa2,kfa3)                                       !/*ab         23*/
      if (iopt.eq.5) kfaa=max0(kfa3,kfa4)                                       !/*ab         24*/
      if (kfaa.gt.6) kfaa=kfaa-6                                                !/*ab         25*/
      if (kfaa.eq.1.or.kfaa.eq.2) am0=rm                                        !/*ab         26*/
      if (kfaa.eq.3) am0=rmk                                                    !/*ab         27*/
      if (kfaa.eq.4) am0=dm                                                     !/*ab         28*/
      go to 20                                                                  !/*ab         29*/
   10 continue                                                                  !/*ab         30*/
      if (kfr1(j).eq.1.or.kfr1(j).eq.2) am0=rm                                  !/*ab         31*/
      if (kfr1(j).eq.7.or.kfr1(j).eq.8) am0=rm                                  !/*ab         32*/
      if (kfr1(j).eq.3.or.kfr1(j).eq.9) am0=rmk                                 !/*ab         33*/
      if (kfr1(j).eq.4.or.kfr1(j).eq.10) am0=dm                                 !/*ab         34*/
      bm0=0.d0                                                                  !/*ab         35*/
      if (kfr2(j).eq.3.or.kfr2(j).eq.9) bm0=rmk                                 !/*ab         36*/
      if (kfr2(j).eq.4.or.kfr2(j).eq.10) bm0=dm                                 !/*ab         37*/
      if (am0.lt.bm0) am0=bm0                                                   !/*ab         38*/
   20 x=rang()                                                                  !/*ab         39*/
      am=am0-1.d0/b1*log(1.d0-x)                                                !/*ab         40*/
      x=rang()                                                                  !/*ab         41*/
      esa=am0-1.d0/b2*log(1.d0-x)                                               !/*ab         42*/
      psa=sqrt(esa**2-am0**2)                                                   !/*ab         43*/
      eab=sqrt(3.d0*.5d0*psa**2+am**2)                                          !/*ab         44*/
      if (i.eq.1) rx=e0                                                         !/*ab         45*/
      if (i.gt.1) rx=re(j)                                                      !/*ab         46*/
      if (rx.gt.eab) go to 150                                                  !/*ab         47*/
      if (iyy.eq.1.and.i.eq.1.and.iopt.ne.5) go to 150                          !/*ab         48*/
   30 la=1                                                                      !/*ab         49*/
      if (i.eq.1) go to 90                                                      !/*ab         50*/
      if (ll.eq.1) go to 60                                                     !/*ab         51*/
      kr1r=kfr1(j)                                                              !/*ab         52*/
      kr2r=kfr2(j)                                                              !/*ab         53*/
      rer=re(j)                                                                 !/*ab         54*/
      rpxr=-pgx                                                                 !/*ab         55*/
      rpyr=-pgy                                                                 !/*ab         56*/
      rpzr=-pgz                                                                 !/*ab         57*/
      re(j)=e0                                                                  !/*ab         58*/
      kfr1(j)=kfa2                                                              !/*ab         59*/
      kfr2(j)=0                                                                 !/*ab         60*/
      iv(j)=iva+5                                                               !/*ab         61*/
      if (iopt.eq.5) kfr2(j)=kfa2                                               !/*ab         62*/
      if (iopt.eq.5) kfr1(j)=kfa1                                               !/*ab         63*/
      if (iopt.eq.5) iv(j)=7                                                    !/*ab         64*/
      rpx(j)=0.d0                                                               !/*ab         65*/
      rpy(j)=0.d0                                                               !/*ab         66*/
      if (iopt.ne.4.or.kfa1.gt.6) go to 40                                      !/*ab         67*/
      iv(j)=7                                                                   !/*ab         68*/
      kfr1(j)=kfa2                                                              !/*ab         69*/
      kfr2(j)=kfa3                                                              !/*ab         70*/
   40 continue                                                                  !/*ab         71*/
      pgx=0.d0                                                                  !/*ab         72*/
      pgy=0.d0                                                                  !/*ab         73*/
      pgz=0.d0                                                                  !/*ab         74*/
      if (lt.eq.0) go to 50                                                     !/*ab         75*/
      write (io,170) kr1r,kr2r,rer,rpxr,rpyr,rpzr                               !/*ab         76*/
   50 continue                                                                  !/*ab         77*/
      go to 150                                                                 !/*ab         78*/
   60 kr1l=kfr1(j)                                                              !/*ab         79*/
      kr2l=kfr2(j)                                                              !/*ab         80*/
      rel=re(j)                                                                 !/*ab         81*/
      rpxl=-pgx                                                                 !/*ab         82*/
      rpyl=-pgy                                                                 !/*ab         83*/
      rpzl=-pgz                                                                 !/*ab         84*/
      if (iopt.ne.4.or.kfa1.lt.6) go to 70                                      !/*ab         85*/
      iv(j)=2                                                                   !/*ab         86*/
      kfr1(j)=kfa2                                                              !/*ab         87*/
      kfr2(j)=kfa3                                                              !/*ab         88*/
      re(j)=e0                                                                  !/*ab         89*/
      rpx(j)=0.d0                                                               !/*ab         90*/
      rpy(j)=0.d0                                                               !/*ab         91*/
      pgx=0.d0                                                                  !/*ab         92*/
      pgy=0.d0                                                                  !/*ab         93*/
      pgz=0.d0                                                                  !/*ab         94*/
   70 continue                                                                  !/*ab         95*/
      if (lt.eq.0) go to 80                                                     !/*ab         96*/
      write (io,180) kr1l,kr2l,rel,rpxl,rpyl,rpzl                               !/*ab         97*/
   80 continue                                                                  !/*ab         98*/
      go to 150                                                                 !/*ab         99*/
   90 continue                                                                  !/*ab        100*/
      if (ll.eq.1) go to 120                                                    !/*ab        101*/
      kr1r=kfa1                                                                 !/*ab        102*/
      kr2r=0                                                                    !/*ab        103*/
      if (iopt.eq.5) kr1r=kfa3                                                  !/*ab        104*/
      if (iopt.eq.5) kr2r=kfa4                                                  !/*ab        105*/
      rer=e0                                                                    !/*ab        106*/
      rpxr=0.d0                                                                 !/*ab        107*/
      rpyr=0.d0                                                                 !/*ab        108*/
      rpzr=0.d0                                                                 !/*ab        109*/
      pgx=0.d0                                                                  !/*ab        110*/
      pgy=0.d0                                                                  !/*ab        111*/
      pgz=0.d0                                                                  !/*ab        112*/
      if (iopt.eq.2) kr1r=kfa1                                                  !/*ab        113*/
      if (iopt.eq.2) kr2r=kfa2                                                  !/*ab        114*/
      if (iopt.ne.4.or.kfa1.le.6) go to 100                                     !/*ab        115*/
      kr1r=kfa2                                                                 !/*ab        116*/
      kr2r=kfa3                                                                 !/*ab        117*/
  100 continue                                                                  !/*ab        118*/
      if (lt.eq.0) go to 110                                                    !/*ab        119*/
      write (io,170) kr1r,kr2r,rer,rpxr,rpyr,rpzr                               !/*ab        120*/
  110 continue                                                                  !/*ab        121*/
      go to 150                                                                 !/*ab        122*/
  120 continue                                                                  !/*ab        123*/
      kr1l=kfa2                                                                 !/*ab        124*/
      kr2l=0                                                                    !/*ab        125*/
      if (iopt.eq.5) kr1l=kfa1                                                  !/*ab        126*/
      if (iopt.eq.5) kr2l=kfa2                                                  !/*ab        127*/
      rel=e0                                                                    !/*ab        128*/
      rpxl=0.d0                                                                 !/*ab        129*/
      rpyl=0.d0                                                                 !/*ab        130*/
      rpzl=0.d0                                                                 !/*ab        131*/
      if (iopt.eq.2) kr1l=kfa1                                                  !/*ab        132*/
      if (iopt.eq.2) kr2l=kfa2                                                  !/*ab        133*/
      if (iopt.ne.4.or.kfa1.gt.6) go to 130                                     !/*ab        134*/
      kr1l=kfa2                                                                 !/*ab        135*/
      kr2l=kfa3                                                                 !/*ab        136*/
  130 continue                                                                  !/*ab        137*/
      if (lt.eq.0) go to 140                                                    !/*ab        138*/
      write (io,180) kr1l,kr2l,rel,rpxl,rpyl,rpzl                               !/*ab        139*/
  140 continue                                                                  !/*ab        140*/
      go to 150                                                                 !/*ab        141*/
  150 continue                                                                  !/*ab        142*/
      if (lt.eq.0) go to 160                                                    !/*ab        143*/
      write (io,190) i,ll,la,am0,am,psa,eab,rx                                  !/*ab        144*/
  160 continue                                                                  !/*ab        145*/
      return                                                                    !/*ab        146*/
!                                                                               !/*ab        147*/
  170 format (1h0,27hqr1,qr2,rer,rpxr,rpyr,rpzr=,2i3,4f8.4)                     !/*ab        148*/
  180 format (1h0,27hql1,ql2,rel,rpxl,rpyl,rpzl=,2i3,4f8.4)                     !/*ab        149*/
  190 format (1h0,26hi,ll,la,am0,am,aps,eab,rx=,3i3,5f8.4)                      !/*ab        150*/
      end                                                                       !/*ab        151*/

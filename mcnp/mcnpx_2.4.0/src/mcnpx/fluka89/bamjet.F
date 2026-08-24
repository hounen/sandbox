!     $Id: bamjet.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                       !/*ba          1*/
      subroutine bamjet (ihad,kfa1,kfa2,kfa3,kfa4,ae0,iopt)                     !/*ba          2*/
!                                                                     *         !/*ba          3*/
!---------------------------------------------------------------------*         !/*ba          4*/
!                                                                     *         !/*ba          5*/
#include <lcs/double.h>                                                         /*!ba          6*/
#include <fluka89/flprm1.h>                                                     /*!ba          7*/
#include <fluka89/flprm2.h>                                                     /*!ba          8*/
#include <lcs/inout.h>                                                          /*!ba          9*/
!                                                                               !/*ba         10*/
#include <fluka89/bamjcm.h>                                                     /*!ba         11*/
#include <fluka89/finpr1.h>                                                     /*!ba         12*/
#include <fluka89/part1.h>                                                      /*!ba         13*/
#include <fluka89/inpdt2.h>                                                     /*!ba         14*/
#include <fluka89/remain.h>                                                     /*!ba         15*/
!REP  data iv /kmxjcm*0/, kfr1 /kmxjcm*0/, kfr2 /kmxjcm*0/                      !/*ba         16*/
      data ncou /0/                                                             !/*ba         17*/
      ncou=ncou+1                                                               !/*ba         18*/
      if (lt.eq.1) write (io,150) iopt,ncou                                     !/*ba         19*/
      if (lt.eq.1) write (io,160) ihad,kfa1,kfa2,kfa3,kfa4,ae0,iopt             !/*ba         20*/
   10 continue                                                                  !/*ba         21*/
      iyy=0                                                                     !/*ba         22*/
      ihad=0                                                                    !/*ba         23*/
      it=0                                                                      !/*ba         24*/
      e0=ae0*.5d0                                                               !/*ba         25*/
      if (iopt.eq.1.or.iopt.eq.2) e0=ae0                                        !/*ba         26*/
      ll=0                                                                      !/*ba         27*/
      if (kfa1.gt.6.and.iopt.eq.1) ll=1                                         !/*ba         28*/
      if (kfa1.le.6.and.iopt.eq.2) ll=1                                         !/*ba         29*/
      if (kfa1.gt.6.and.iopt.eq.4) ll=1                                         !/*ba         30*/
      pgx=0.d0                                                                  !/*ba         31*/
      pgy=0.d0                                                                  !/*ba         32*/
      pgz=0.d0                                                                  !/*ba         33*/
      rpx0=0.d0                                                                 !/*ba         34*/
      rpy0=0.d0                                                                 !/*ba         35*/
      do 90 i=1,kmxjcm                                                          !/*ba         36*/
      kfr1(i)=0                                                                 !/*ba         37*/
      kfr2(i)=0                                                                 !/*ba         38*/
      la=0                                                                      !/*ba         39*/
      it=it+1                                                                   !/*ba         40*/
      j=it-1                                                                    !/*ba         41*/
      k=it+1                                                                    !/*ba         42*/
   20 continue                                                                  !/*ba         43*/
      call abbrch (it,ll,la,lt,e0,pgx,pgy,pgz,kfr1,kfr2,re,kr1r,kr2r            !/*ba         44*/
     & ,kr1l,kr2l,rpx,rpy,rpz,rpxr,rpyr,rpzr,rpxl,rpyl,rpzl,rer,rel,iv          !/*ba         45*/
     & ,b1,b2,kfa1,kfa2,kfa3,kfa4,iopt,iyy)                                     !/*ba         46*/
      if (la.eq.0) go to 40                                                     !/*ba         47*/
      it=it-1                                                                   !/*ba         48*/
      if (iopt.eq.3.and.ll.eq.0) go to 30                                       !/*ba         49*/
      if (iopt.eq.4.and.kfa1.le.6.and.ll.eq.0) go to 30                         !/*ba         50*/
      if (iopt.eq.4.and.kfa1.gt.6.and.ll.eq.1) go to 30                         !/*ba         51*/
      if (iopt.eq.5.and.ll.eq.0) go to 30                                       !/*ba         52*/
      go to 100                                                                 !/*ba         53*/
   30 continue                                                                  !/*ba         54*/
      iyy=1                                                                     !/*ba         55*/
      ll=1                                                                      !/*ba         56*/
      if (iopt.eq.4.and.kfa1.gt.6) ll=0                                         !/*ba         57*/
      iar=it                                                                    !/*ba         58*/
      go to 80                                                                  !/*ba         59*/
   40 continue                                                                  !/*ba         60*/
      call vertex (it,lt,ll,kfa1,e0,iv,re,kfr1,kfr2,ame,iopt)                   !/*ba         61*/
      call flavor (it,lt,ll,e0,iv,re,kfr1,kfr2,isu,bet,kfa1,kfa2,kfa3           !/*ba         62*/
     & ,kfa4,iopt)                                                              !/*ba         63*/
      call hklass (it,lt,la,ll,kfr1,kfr2,kr1r,kr2r,kr1l,kr2l,iv,imps            !/*ba         64*/
     & ,imve,ib08,ia08,ib10,ia10,as,b8,kfa1,kfa2,kfa3,kfa4,iopt)                !/*ba         65*/
      if (it.eq.1) rx=e0                                                        !/*ba         66*/
      if (it.gt.1) rx=re(j)                                                     !/*ba         67*/
      if (amf(it).gt.rx) go to 10                                               !/*ba         68*/
      if (amf(it).le.rx) go to 50                                               !/*ba         69*/
      la=1                                                                      !/*ba         70*/
      go to 20                                                                  !/*ba         71*/
   50 continue                                                                  !/*ba         72*/
      ihad=ihad+1                                                               !/*ba         73*/
      call energi (it,ll,lt,iv,re,hma,he,e0,a1)                                 !/*ba         74*/
      call impuls (he,hma,hps,hpx,hpy,hpz,lt,ll,b3)                             !/*ba         75*/
      if (it.gt.1) go to 60                                                     !/*ba         76*/
      rpx(it)=rpx0-hpx                                                          !/*ba         77*/
      rpy(it)=rpy0-hpy                                                          !/*ba         78*/
      go to 70                                                                  !/*ba         79*/
   60 rpx(it)=rpx(j)-hpx                                                        !/*ba         80*/
      rpy(it)=rpy(j)-hpy                                                        !/*ba         81*/
   70 continue                                                                  !/*ba         82*/
      if (iopt.eq.1.and.ll.eq.1) hpz=-hpz                                       !/*ba         83*/
      if (iopt.eq.2.and.ll.eq.1) hpz=-hpz                                       !/*ba         84*/
      if (iopt.eq.4.and.kfa1.gt.6) hpz=-hpz                                     !/*ba         85*/
      if (iopt.eq.5) hpz=-hpz                                                   !/*ba         86*/
      pgx=pgx+hpx                                                               !/*ba         87*/
      pgy=pgy+hpy                                                               !/*ba         88*/
      pgz=pgz+hpz                                                               !/*ba         89*/
      pxf(it)=hpx                                                               !/*ba         90*/
      pyf(it)=hpy                                                               !/*ba         91*/
      pzf(it)=hpz                                                               !/*ba         92*/
   80 continue                                                                  !/*ba         93*/
   90 continue                                                                  !/*ba         94*/
  100 continue                                                                  !/*ba         95*/
      if (iopt.eq.1.or.iopt.eq.2) go to 110                                     !/*ba         96*/
      ial=it                                                                    !/*ba         97*/
      call verein (it,la,lt,rer,rel,rpxr,rpyr,rpzr,rpxl,rpyl,rpzl,kr1r          !/*ba         98*/
     & ,kr2r,kr1l,kr2l,ihad,ll,kfr1,kfr2,imps,imve,ib08,ia08,ib10,ia10          !/*ba         99*/
     & ,b3,as,b8,iar,kfa1,kfa2,kfa3,kfa4,iopt)                                  !/*ba        100*/
      if (la.eq.3) go to 10                                                     !/*ba        101*/
      if (la.eq.2) go to 10                                                     !/*ba        102*/
  110 continue                                                                  !/*ba        103*/
      if (iopt.eq.3.or.iopt.eq.4.or.iopt.eq.5) go to 120                        !/*ba        104*/
      if (ll.eq.0) go to 120                                                    !/*ba        105*/
      rpxr=rpxl                                                                 !/*ba        106*/
      rpyr=rpyl                                                                 !/*ba        107*/
      rpzr=rpzl                                                                 !/*ba        108*/
      rer=rel                                                                   !/*ba        109*/
      kr1r=kr1l                                                                 !/*ba        110*/
      kr2r=kr2l                                                                 !/*ba        111*/
  120 continue                                                                  !/*ba        112*/
      if (le.eq.0) go to 140                                                    !/*ba        113*/
      write (io,170)                                                            !/*ba        114*/
      do 130 j=1,it                                                             !/*ba        115*/
      write (io,180) nref(j),anf(j),amf(j),ichf(j),ibarf(j),pxf(j),pyf(j        !/*ba        116*/
     & ),pzf(j),hef(j)                                                          !/*ba        117*/
  130 continue                                                                  !/*ba        118*/
  140 continue                                                                  !/*ba        119*/
      return                                                                    !/*ba        120*/
!                                                                               !/*ba        121*/
!REP                                                                            !/*ba        122*/
! 160 format (5i5,2e12.4,' bamjet,ihad,kfa1,kfa2,kfa3,kfa4,ae0,iopt')           !/*ba        123*/
!REP                                                                            !/*ba        124*/
!                                                                               !/*ba        125*/
  150 format ('  bamjet',2i10)                                                  !/*ba        126*/
  160 format (5i5,e12.4,i5,' bamjet,ihad,kfa1,kfa2,kfa3,kfa4,ae0,iopt')         !/*ba        127*/
  170 format (2x,'nf,name,mass,iq,ib,px,py,pz,e')                               !/*ba        128*/
  180 format (2x,i3,a6,f6.3,2i4,4f8.4)                                          !/*ba        129*/
      end                                                                       !/*ba        130*/
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        !/*ba        131*/

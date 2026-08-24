!     $Id: verein.F,v 1.5 2002/03/27 18:14:48 gwm Exp $                       !/*ve          1*/
      subroutine verein (it,la,lt,rer,rel,rpxr,rpyr,rpzr,rpxl,rpyl,rpzl         !/*ve          2*/
     & ,kr1r,kr2r,kr1l,kr2l,ihad,ll,kfr1,kfr2,imps,imve,ib08,ia08,ib10          !/*ve          3*/
     & ,ia10,b3,as,b8,iar,kfa1,kfa2,kfa3,kfa4,iopt)                             !/*ve          4*/
!                                                                     *         !/*ve          5*/
!---------------------------------------------------------------------*         !/*ve          6*/
!                                                                     *         !/*ve          7*/
#include <lcs/double.h>                                                         /*!ve          8*/
#include <fluka89/flprm1.h>                                                     /*!ve          9*/
#include <fluka89/flprm2.h>                                                     /*!ve         10*/
#include <lcs/inout.h>                                                          /*!ve         11*/
#include <fluka89/finpr1.h>                                                     /*!ve         12*/
#include <fluka89/part1.h>                                                      /*!ve         13*/
      dimension kfr1(*), kfr2(*), imps(6,6)                                     !/*ve         14*/
      parameter (kmxjcm=100)                                                    !/*ve         15*/
      dimension iv(kmxjcm)                                                      !/*ve         16*/
      dimension imve(6,6), ib08(6,21), ia08(6,21), ib10(6,21), ia10(6,21        !/*ve         17*/
     & )                                                                        !/*ve         18*/
      data iv /kmxjcm*0/                                                        !/*ve         19*/
      if (lt.eq.0) go to 10                                                     !/*ve         20*/
      write (io,130) it                                                         !/*ve         21*/
   10 continue                                                                  !/*ve         22*/
      if (it.eq.0) go to 100                                                    !/*ve         23*/
      if (kr2r.eq.0.or.kr1r.eq.0) go to 20                                      !/*ve         24*/
      if (kr1l.eq.0.or.kr2l.eq.0) go to 20                                      !/*ve         25*/
      reg=rer+rel                                                               !/*ve         26*/
      rpxg=rpxr+rpxl                                                            !/*ve         27*/
      rpyg=rpyr+rpyl                                                            !/*ve         28*/
      rpzg=rpzr+rpzl                                                            !/*ve         29*/
      j=it+1                                                                    !/*ve         30*/
      i=it+2                                                                    !/*ve         31*/
      it=j                                                                      !/*ve         32*/
      call hklass (it,lt,la,ll,kfr1,kfr2,kr1r,kr2r,kr1l,kr2l,iv,imps            !/*ve         33*/
     & ,imve,ib08,ia08,ib10,ia10,as,b8,kfa1,kfa2,kfa3,kfa4,iopt)                !/*ve         34*/
      ihad=ihad+2                                                               !/*ve         35*/
      if (lt.eq.1) write (io,140) ihad                                          !/*ve         36*/
      go to 30                                                                  !/*ve         37*/
   20 iak=it                                                                    !/*ve         38*/
      x=rang()                                                                  !/*ve         39*/
      if (x.le.0.5d0.and.iar.ne.0) iak=iar                                      !/*ve         40*/
      reg=rer+rel+hef(iak)                                                      !/*ve         41*/
      rpxg=rpxr+rpxl+pxf(iak)                                                   !/*ve         42*/
      rpyg=rpyr+rpyl+pyf(iak)                                                   !/*ve         43*/
      rpzg=rpzr+rpzl+pzf(iak)                                                   !/*ve         44*/
      i=it+1                                                                    !/*ve         45*/
      j=iak                                                                     !/*ve         46*/
      it=i                                                                      !/*ve         47*/
      call hklass (it,lt,la,ll,kfr1,kfr2,kr1r,kr2r,kr1l,kr2l,iv,imps            !/*ve         48*/
     & ,imve,ib08,ia08,ib10,ia10,as,b8,kfa1,kfa2,kfa3,kfa4,iopt)                !/*ve         49*/
      hp2=hp1                                                                   !/*ve         50*/
      ihad=ihad+1                                                               !/*ve         51*/
      if (lt.eq.1) write (io,140) ihad                                          !/*ve         52*/
   30 rpg=sqrt(rpxg**2+rpyg**2+rpzg**2)                                         !/*ve         53*/
      if (rpg.gt.reg) la=3                                                      !/*ve         54*/
      if (la.eq.3.and.lt.gt.0) write (io,150)                                   !/*ve         55*/
      if (la.eq.3) return                                                       !/*ve         56*/
      rmg=sqrt(reg**2-rpg**2)                                                   !/*ve         57*/
      hm1=amf(j)                                                                !/*ve         58*/
      hm2=amf(i)                                                                !/*ve         59*/
      if (rmg.lt.hm1+hm2) la=3                                                  !/*ve         60*/
      if (la.eq.3.and.lt.gt.0) write (io,150)                                   !/*ve         61*/
      if (la.eq.3) return                                                       !/*ve         62*/
      if (lt.eq.0) go to 40                                                     !/*ve         63*/
      write (io,160) reg,rmg,rpg,hm1,hm2,j,i                                    !/*ve         64*/
   40 gaa=reg/rmg                                                               !/*ve         65*/
      gabe=rpg/rmg                                                              !/*ve         66*/
      he1=(rmg**2+hm1**2-hm2**2)/(2.d0*rmg)                                     !/*ve         67*/
      he2=rmg-he1                                                               !/*ve         68*/
      hp1=sqrt(he1**2-hm1**2)                                                   !/*ve         69*/
      he=he1                                                                    !/*ve         70*/
      hma=hm1                                                                   !/*ve         71*/
      call impuls (he,hma,hps,hpx,hpy,hpz,lt,ll,b3)                             !/*ve         72*/
      hp1x=hpx                                                                  !/*ve         73*/
      hp1y=hpy                                                                  !/*ve         74*/
      hp1z=hpz                                                                  !/*ve         75*/
      hp2x=-hp1x                                                                !/*ve         76*/
      hp2y=-hp1y                                                                !/*ve         77*/
      if (iopt.eq.4.and.hm2.gt.hm1) go to 50                                    !/*ve         78*/
      hp1z=-hpz                                                                 !/*ve         79*/
      hp2z=hpz                                                                  !/*ve         80*/
   50 continue                                                                  !/*ve         81*/
      hp2z=-hp1z                                                                !/*ve         82*/
      if (rpg.eq.0.d0) go to 80                                                 !/*ve         83*/
      hex=he1*gaa+hp1z*gabe                                                     !/*ve         84*/
      hey=he2*gaa+hp2z*gabe                                                     !/*ve         85*/
      hp1z=he1*gabe+hp1z*gaa                                                    !/*ve         86*/
      hp2z=he2*gabe+hp2z*gaa                                                    !/*ve         87*/
      he1=hex                                                                   !/*ve         88*/
      he2=hey                                                                   !/*ve         89*/
      x=hp1x                                                                    !/*ve         90*/
      y=hp1y                                                                    !/*ve         91*/
      z=hp1z                                                                    !/*ve         92*/
      cote=rpzg/rpg                                                             !/*ve         93*/
      site=sqrt(1.d0-cote**2)                                                   !/*ve         94*/
      if (site.eq.0.d0) go to 60                                                !/*ve         95*/
      cov=rpxg/(rpg*site)                                                       !/*ve         96*/
      siv=rpyg/(rpg*site)                                                       !/*ve         97*/
      go to 70                                                                  !/*ve         98*/
   60 siv=1.d0                                                                  !/*ve         99*/
      cov=0.d0                                                                  !/*ve        100*/
   70 cops=-siv                                                                 !/*ve        101*/
      sips=cov                                                                  !/*ve        102*/
      call drelab (x,y,z,cote,site,cops,sips)                                   !/*ve        103*/
      hp1x=x                                                                    !/*ve        104*/
      hp1y=y                                                                    !/*ve        105*/
      hp1z=z                                                                    !/*ve        106*/
      x=hp2x                                                                    !/*ve        107*/
      y=hp2y                                                                    !/*ve        108*/
      z=hp2z                                                                    !/*ve        109*/
      call drelab (x,y,z,cote,site,cops,sips)                                   !/*ve        110*/
      hp2x=x                                                                    !/*ve        111*/
      hp2y=y                                                                    !/*ve        112*/
      hp2z=z                                                                    !/*ve        113*/
   80 continue                                                                  !/*ve        114*/
      hef(j)=he1                                                                !/*ve        115*/
      pxf(j)=hp1x                                                               !/*ve        116*/
      pyf(j)=hp1y                                                               !/*ve        117*/
      pzf(j)=hp1z                                                               !/*ve        118*/
      hef(i)=he2                                                                !/*ve        119*/
      pxf(i)=hp2x                                                               !/*ve        120*/
      pyf(i)=hp2y                                                               !/*ve        121*/
      pzf(i)=hp2z                                                               !/*ve        122*/
      if (lt.eq.0) go to 90                                                     !/*ve        123*/
      write (io,170) hef(j),pxf(j),pyf(j),pzf(j)                                !/*ve        124*/
      write (io,170) hef(i),pxf(i),pyf(i),pzf(i)                                !/*ve        125*/
   90 go to 120                                                                 !/*ve        126*/
  100 continue                                                                  !/*ve        127*/
      if (lt.eq.0) go to 110                                                    !/*ve        128*/
      write (io,180)                                                            !/*ve        129*/
  110 continue                                                                  !/*ve        130*/
      ihad=1                                                                    !/*ve        131*/
      la=2                                                                      !/*ve        132*/
  120 continue                                                                  !/*ve        133*/
      return                                                                    !/*ve        134*/
!                                                                               !/*ve        135*/
  130 format (1h0,i5,3h=it)                                                     !/*ve        136*/
  140 format (1h0,13hhadronanzahl=,i5)                                          !/*ve        137*/
  150 format (2x,'rest jet momentum is greater than rest jet energy')           !/*ve        138*/
  160 format (1h0,19heg,mg,pg,m1,m2,j,i=,5f8.4,2i3)                             !/*ve        139*/
  170 format (1h0,11he,px,py,pz=,4f8.4)                                         !/*ve        140*/
  180 format (1h0,                                                              !/*ve        141*/
     & 'cut off of the left and right jet befor the first decay step'/          !/*ve        142*/
     & 'if allowed only one resonance could be created')                        !/*ve        143*/
      end                                                                       !/*ve        144*/

!     $Id: hklass.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*hk          1*/
      subroutine hklass (it,lt,la,ll,kfr1,kfr2,kr1r,kr2r,kr1l,kr2l,iv           !/*hk          2*/
     & ,imps,imve,ib08,ia08,ib10,ia10,as,b8,kfa1,kfa2,kfa3,kfa4,iopt)           !/*hk          3*/
!                                                                     *         !/*hk          4*/
!---------------------------------------------------------------------*         !/*hk          5*/
!                                                                     *         !/*hk          6*/
#include <lcs/double.h>                                                         /*!hk          7*/
#include <fluka89/flprm1.h>                                                     /*!hk          8*/
#include <fluka89/flprm2.h>                                                     /*!hk          9*/
#include <lcs/inout.h>                                                          /*!hk         10*/
#include <fluka89/finpr1.h>                                                     /*!hk         11*/
#include <fluka89/part1.h>                                                      /*!hk         12*/
      logical lswtch                                                            !/*hk         13*/
#include <fluka89/valita.h>                                                     /*!hk         14*/
      dimension kfr1(*), kfr2(*), iv(*), imps(6,6), imve(6,6), ib08(6,21        !/*hk         15*/
     & ), ib10(6,21), ia08(6,21), ia10(6,21)                                    !/*hk         16*/
      resc=0.5d0                                                                !/*hk         17*/
      ix=0                                                                      !/*hk         18*/
      x=rang()                                                                  !/*hk         19*/
      z=rang()                                                                  !/*hk         20*/
      if (la.eq.0) go to 50                                                     !/*hk         21*/
      a=kr1r*kr2r*kr1l*kr2l                                                     !/*hk         22*/
      if (a.eq.0) go to 20                                                      !/*hk         23*/
      ix=1                                                                      !/*hk         24*/
      li=kr1r-6                                                                 !/*hk         25*/
      lj=kr1l                                                                   !/*hk         26*/
      i=it                                                                      !/*hk         27*/
      go to 120                                                                 !/*hk         28*/
   10 ix=2                                                                      !/*hk         29*/
      li=kr2r-6                                                                 !/*hk         30*/
      lj=kr2l                                                                   !/*hk         31*/
      i=it+1                                                                    !/*hk         32*/
      it=it+1                                                                   !/*hk         33*/
      go to 120                                                                 !/*hk         34*/
   20 b=kr1r*kr2r                                                               !/*hk         35*/
      c=kr1l*kr2l                                                               !/*hk         36*/
      if (b.gt.0.d0.or.c.gt.0.d0) go to 30                                      !/*hk         37*/
      lj=kr1r                                                                   !/*hk         38*/
      if (kr1r.eq.0) lj=kr2r                                                    !/*hk         39*/
      li=kr1l-6                                                                 !/*hk         40*/
      if (kr1l.eq.0) li=kr2l-6                                                  !/*hk         41*/
      i=it                                                                      !/*hk         42*/
      go to 120                                                                 !/*hk         43*/
   30 if (b.gt.0.d0) go to 40                                                   !/*hk         44*/
      li=kr1r                                                                   !/*hk         45*/
      if (kr1r.eq.0) li=kr2r                                                    !/*hk         46*/
      ka=kr1l                                                                   !/*hk         47*/
      kb=kr2l                                                                   !/*hk         48*/
      call index2 (ka,kb,ind)                                                   !/*hk         49*/
      lj=ind                                                                    !/*hk         50*/
      i=it                                                                      !/*hk         51*/
      go to 140                                                                 !/*hk         52*/
   40 li=kr1l-6                                                                 !/*hk         53*/
      if (kr1l.eq.0) li=kr2l-6                                                  !/*hk         54*/
      ka=kr1r-6                                                                 !/*hk         55*/
      kb=kr2r-6                                                                 !/*hk         56*/
      call index2 (ka,kb,ind)                                                   !/*hk         57*/
      lj=ind                                                                    !/*hk         58*/
      i=it                                                                      !/*hk         59*/
      go to 170                                                                 !/*hk         60*/
   50 continue                                                                  !/*hk         61*/
      i=it                                                                      !/*hk         62*/
      j=it-1                                                                    !/*hk         63*/
      k1i=kfr1(i)                                                               !/*hk         64*/
      k2i=kfr2(i)                                                               !/*hk         65*/
      k2j=0                                                                     !/*hk         66*/
      k1j=kfa1                                                                  !/*hk         67*/
      if (i.gt.1) go to 60                                                      !/*hk         68*/
      if (iopt.eq.3.and.ll.eq.1) k1j=kfa2                                       !/*hk         69*/
      if (iopt.eq.2) k1j=kfa1                                                   !/*hk         70*/
      if (iopt.eq.2) k2j=kfa2                                                   !/*hk         71*/
      if (iopt.eq.4.and.kfa1.le.6.and.ll.eq.1) k1j=kfa2                         !/*hk         72*/
      if (iopt.eq.4.and.kfa1.le.6.and.ll.eq.1) k2j=kfa3                         !/*hk         73*/
      if (iopt.eq.4.and.kfa1.gt.6.and.ll.eq.0) k1j=kfa2                         !/*hk         74*/
      if (iopt.eq.4.and.kfa1.gt.6.and.ll.eq.0) k2j=kfa3                         !/*hk         75*/
      if (iopt.eq.5.and.ll.eq.0) k1j=kfa3                                       !/*hk         76*/
      if (iopt.eq.5.and.ll.eq.0) k2j=kfa4                                       !/*hk         77*/
      if (iopt.eq.5.and.ll.eq.1) k1j=kfa1                                       !/*hk         78*/
      if (iopt.eq.5.and.ll.eq.1) k2j=kfa2                                       !/*hk         79*/
   60 continue                                                                  !/*hk         80*/
      if (i.gt.1) k1j=kfr1(j)                                                   !/*hk         81*/
      if (i.gt.1) k2j=kfr2(j)                                                   !/*hk         82*/
      ivy=iv(i)                                                                 !/*hk         83*/
      go to (70,100,110,80,90,70,100,110,80,90), ivy                            !/*hk         84*/
   70 li=k1i                                                                    !/*hk         85*/
      lj=k1j                                                                    !/*hk         86*/
      if (ll.eq.1) li=k1j-6                                                     !/*hk         87*/
      if (ll.eq.1) lj=k1i-6                                                     !/*hk         88*/
      go to 120                                                                 !/*hk         89*/
   80 li=k1j-6                                                                  !/*hk         90*/
      lj=k1i-6                                                                  !/*hk         91*/
      if (ll.eq.1) li=k1i                                                       !/*hk         92*/
      if (ll.eq.1) lj=k1j                                                       !/*hk         93*/
      go to 120                                                                 !/*hk         94*/
   90 li=k2j-6                                                                  !/*hk         95*/
      lj=k2i-6                                                                  !/*hk         96*/
      if (ll.eq.1) li=k2i                                                       !/*hk         97*/
      if (ll.eq.1) lj=k2j                                                       !/*hk         98*/
      go to 120                                                                 !/*hk         99*/
  100 li=k1j                                                                    !/*hk        100*/
      if (ll.eq.1) li=k1j-6                                                     !/*hk        101*/
      ka=k1i                                                                    !/*hk        102*/
      kb=k2i                                                                    !/*hk        103*/
      if (ll.eq.0) ka=k1i-6                                                     !/*hk        104*/
      if (ll.eq.0) kb=k2i-6                                                     !/*hk        105*/
      if (ll.eq.1) ka=k1i                                                       !/*hk        106*/
      if (ll.eq.1) kb=k2i                                                       !/*hk        107*/
      call index2 (ka,kb,ind)                                                   !/*hk        108*/
      lj=ind                                                                    !/*hk        109*/
      if (ll.eq.1) go to 170                                                    !/*hk        110*/
      go to 140                                                                 !/*hk        111*/
  110 li=k1i                                                                    !/*hk        112*/
      if (ll.eq.1) li=k1i-6                                                     !/*hk        113*/
      ka=k1j                                                                    !/*hk        114*/
      kb=k2j                                                                    !/*hk        115*/
      if (ll.eq.0) ka=k1j-6                                                     !/*hk        116*/
      if (ll.eq.0) kb=k2j-6                                                     !/*hk        117*/
      call index2 (ka,kb,ind)                                                   !/*hk        118*/
      lj=ind                                                                    !/*hk        119*/
      if (ll.eq.1) go to 140                                                    !/*hk        120*/
      go to 170                                                                 !/*hk        121*/
  120 continue                                                                  !/*hk        122*/
      if (x.le.as) go to 130                                                    !/*hk        123*/
      index=imve(li,lj)                                                         !/*hk        124*/
      if (index.eq.33.and.z.le.0.5d0) index=35                                  !/*hk        125*/
      if (.not.lswtch) go to 210                                                !/*hk        126*/
      x=rang()                                                                  !/*hk        127*/
      if (x.ge.resc) go to 210                                                  !/*hk        128*/
      if (index.eq.33.or.index.eq.35.or.index.eq.96) index=181                  !/*hk        129*/
      if (index.eq.32) index=182                                                !/*hk        130*/
      if (index.eq.34) index=183                                                !/*hk        131*/
      go to 210                                                                 !/*hk        132*/
  130 index=imps(li,lj)                                                         !/*hk        133*/
      if (index.eq.23) go to 200                                                !/*hk        134*/
      if (index.eq.31.and.z.le.0.33d0) index=95                                 !/*hk        135*/
      if (.not.lswtch) go to 210                                                !/*hk        136*/
      if (index.eq.31.and.z.le.0.97d0) index=95                                 !/*hk        137*/
      x=rang()                                                                  !/*hk        138*/
      if (x.ge.resc) go to 210                                                  !/*hk        139*/
      if (index.eq.23.or.index.eq.31.or.index.eq.95) index=181                  !/*hk        140*/
      if (index.eq.13) index=182                                                !/*hk        141*/
      if (index.eq.14) index=183                                                !/*hk        142*/
      go to 210                                                                 !/*hk        143*/
  140 continue                                                                  !/*hk        144*/
      b10=1.d0-b8                                                               !/*hk        145*/
      if (x.le.b8) go to 160                                                    !/*hk        146*/
  150 index=ib10(li,lj)                                                         !/*hk        147*/
      go to 210                                                                 !/*hk        148*/
  160 index=ib08(li,lj)                                                         !/*hk        149*/
      if (index.eq.22.and.z.le.0.5d0) index=17                                  !/*hk        150*/
      if (index.eq.137.and.z.le.0.5d0) index=141                                !/*hk        151*/
      if (index.eq.138.and.z.le.0.5d0) index=143                                !/*hk        152*/
      if (index.eq.139.and.z.le.0.5d0) index=144                                !/*hk        153*/
      if (index.eq.0) go to 150                                                 !/*hk        154*/
      go to 210                                                                 !/*hk        155*/
  170 continue                                                                  !/*hk        156*/
      a8=b8                                                                     !/*hk        157*/
      a10=1.d0-a8                                                               !/*hk        158*/
      if (x.le.a8) go to 190                                                    !/*hk        159*/
  180 index=ia10(li,lj)                                                         !/*hk        160*/
      go to 210                                                                 !/*hk        161*/
  190 index=ia08(li,lj)                                                         !/*hk        162*/
      if (index.eq.100.and.z.le.0.5d0) index=18                                 !/*hk        163*/
      if (index.eq.149.and.z.le.0.5d0) index=153                                !/*hk        164*/
      if (index.eq.150.and.z.le.0.5d0) index=155                                !/*hk        165*/
      if (index.eq.151.and.z.le.0.5d0) index=156                                !/*hk        166*/
      if (index.eq.0) go to 180                                                 !/*hk        167*/
      go to 210                                                                 !/*hk        168*/
  200 continue                                                                  !/*hk        169*/
      if (lswtch) then                                                          !/*hk        170*/
        if (z.le.0.5d0) index=31                                                !/*hk        171*/
        if (z.le.0.5d0*0.03d0) index=95                                         !/*hk        172*/
      else                                                                      !/*hk        173*/
        if (z.le.0.5d0) index=33                                                !/*hk        174*/
        if (z.le.0.5d0*0.67d0) index=95                                         !/*hk        175*/
      endif                                                                     !/*hk        176*/
      go to 210                                                                 !/*hk        177*/
  210 continue                                                                  !/*hk        178*/
      if (lt.eq.0) go to 220                                                    !/*hk        179*/
      write (io,260) index                                                      !/*hk        180*/
  220 continue                                                                  !/*hk        181*/
      anf(i)=aname(index)                                                       !/*hk        182*/
      amf(i)=am(index)                                                          !/*hk        183*/
      ichf(i)=ich(index)                                                        !/*hk        184*/
      ibarf(i)=ibar(index)                                                      !/*hk        185*/
      nref(i)=index                                                             !/*hk        186*/
      if (ix.eq.1) go to 10                                                     !/*hk        187*/
      if (ix.eq.2) go to 230                                                    !/*hk        188*/
      go to 230                                                                 !/*hk        189*/
  230 continue                                                                  !/*hk        190*/
      if (lt.eq.0) go to 250                                                    !/*hk        191*/
      iz=0                                                                      !/*hk        192*/
      if (ix.eq.2) i=i-1                                                        !/*hk        193*/
  240 continue                                                                  !/*hk        194*/
      write (io,270) anf(i),amf(i),ichf(i),ibarf(i),nref(i),ll                  !/*hk        195*/
      if (ix.eq.0) go to 250                                                    !/*hk        196*/
      iz=iz+1                                                                   !/*hk        197*/
      if (iz.eq.2) go to 250                                                    !/*hk        198*/
      i=i+1                                                                     !/*hk        199*/
      go to 240                                                                 !/*hk        200*/
  250 continue                                                                  !/*hk        201*/
      return                                                                    !/*hk        202*/
!                                                                               !/*hk        203*/
  260 format (1h0,i3,6h=index)                                                  !/*hk        204*/
  270 format ((1x,a7,1x,f6.3,1x,4(i3,1x),'mame,mass,q,bq,nr,ll'))               !/*hk        205*/
      end                                                                       !/*hk        206*/

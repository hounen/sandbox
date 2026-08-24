!     $Id: opt11.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                        !/*op          1*/
      subroutine opt11 (hold,nerg,nerg1)                                        !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
#include <htape3x/box.h>                                                        /*!op          8*/
      dimension hold(3,nerg1,1)                                                 !/*op          9*/
      dimension nc1(8)                                                          !/*op         10*/
      data nc1 /0,0,0,1,0,0,1,0/                                                !/*op         11*/
      assign 70 to jbr0                                                         !/*op         12*/
   10 continue                                                                  !/*op         13*/
      if (ncol.ne.4.and.ncol.ne.7) go to 200                                    !/*op         14*/
   20 continue                                                                  !/*op         15*/
      do 30 ipt=1,ntype                                                         !/*op         16*/
      if (int(tip).eq.itip(ipt)) go to 40                                       !/*op         17*/
   30 continue                                                                  !/*op         18*/
      go to 200                                                                 !/*op         19*/
   40 continue                                                                  !/*op         20*/
      ja=lelem                                                                  !/*op         21*/
      do 50 is=1,nparm                                                          !/*op         22*/
      if (ja.eq.lparm(is)) go to 60                                             !/*op         23*/
   50 continue                                                                  !/*op         24*/
      go to 200                                                                 !/*op         25*/
   60 continue                                                                  !/*op         26*/
      go to jbr0, (70,220)                                                      !/*op         27*/
   70 continue                                                                  !/*op         28*/
      if (nseg.eq.0) go to 90                                                   !/*op         29*/
      if (ifseg.lt.0) go to 800                                                 !/*op         30*/
      do 80 iseg=1,nseg                                                         !/*op         31*/
      if (cx(ifseg).le.fseg(iseg)) go to 100                                    !/*op         32*/
   80 continue                                                                  !/*op         33*/
      go to 90                                                                  !/*op         34*/
  800 continue                                                                  !/*op         35*/
      do 810 iseg=1,nseg                                                        !/*op         36*/
      if (ifseg.eq.-1) test=cx(2)**2+cx(3)**2                                   !/*op         37*/
      if (ifseg.eq.-2) test=cx(1)**2+cx(3)**2                                   !/*op         38*/
      if (ifseg.eq.-3) test=cx(1)**2+cx(2)**2                                   !/*op         39*/
      if (test.le.fseg(iseg)**2) go to 100                                      !/*op         40*/
  810 continue                                                                  !/*op         41*/
   90 continue                                                                  !/*op         42*/
      iseg=nseg1                                                                !/*op         43*/
  100 continue                                                                  !/*op         44*/
      if (.not.lbunch) l1=iseg+nseg1*(is-1+nparm*(ipt-1))                       !/*op         45*/
      if (lbunch) l1=iseg+nseg1*(ipt-1)                                         !/*op         46*/
      l=0                                                                       !/*op         47*/
      if (nfprm.eq.0) go to 160                                                 !/*op         48*/
      if (kopt.le.0) go to 120                                                  !/*op         49*/
      if (kopt.eq.4) go to 110                                                  !/*op         50*/
      fmu0=dc(kopt)                                                             !/*op         51*/
      go to 130                                                                 !/*op         52*/
  110 continue                                                                  !/*op         53*/
      fmu0=u*cv(1)+v*cv(2)+w*cv(3)                                              !/*op         54*/
      go to 130                                                                 !/*op         55*/
  120 continue                                                                  !/*op         56*/
      fmu0=fmu                                                                  !/*op         57*/
  130 continue                                                                  !/*op         58*/
      do 140 ia=1,nfprm                                                         !/*op         59*/
      if (fmu0.le.fparm(ia)) go to 150                                          !/*op         60*/
  140 continue                                                                  !/*op         61*/
      go to 200                                                                 !/*op         62*/
  150 continue                                                                  !/*op         63*/
      l=ia+nfprm1*(l1-1)                                                        !/*op         64*/
  160 continue                                                                  !/*op         65*/
      lt=nfprm1*l1                                                              !/*op         66*/
      do 170 ie=1,nerg                                                          !/*op         67*/
      if (ec.le.ergb(ie)) go to 180                                             !/*op         68*/
  170 continue                                                                  !/*op         69*/
      go to 200                                                                 !/*op         70*/
  180 continue                                                                  !/*op         71*/
!      convert from nanoseconds to milliseconds                                 !/*op         72*/
      del=fnorm*tc                                                              !/*op         73*/
      if (l.eq.0) go to 190                                                     !/*op         74*/
      hold(1,ie,l)=hold(1,ie,l)+wt                                              !/*op         75*/
      hold(2,ie,l)=hold(2,ie,l)+wt*del                                          !/*op         76*/
      hold(3,ie,l)=hold(3,ie,l)+wt*del**2                                       !/*op         77*/
      if (.not.lfe) go to 190                                                   !/*op         78*/
      hold(1,nerg1,l)=hold(1,nerg1,l)+wt                                        !/*op         79*/
      hold(2,nerg1,l)=hold(2,nerg1,l)+wt*del                                    !/*op         80*/
      hold(3,nerg1,l)=hold(3,nerg1,l)+wt*del**2                                 !/*op         81*/
  190 continue                                                                  !/*op         82*/
      hold(1,ie,lt)=hold(1,ie,lt)+wt                                            !/*op         83*/
      hold(2,ie,lt)=hold(2,ie,lt)+wt*del                                        !/*op         84*/
      hold(3,ie,lt)=hold(3,ie,lt)+wt*del**2                                     !/*op         85*/
      if (.not.lfe) go to 200                                                   !/*op         86*/
      hold(1,nerg1,lt)=hold(1,nerg1,lt)+wt                                      !/*op         87*/
      hold(2,nerg1,lt)=hold(2,nerg1,lt)+wt*del                                  !/*op         88*/
      hold(3,nerg1,lt)=hold(3,nerg1,lt)+wt*del**2                               !/*op         89*/
  200 continue                                                                  !/*op         90*/
      call srchnp (nc1)                                                         !/*op         91*/
      if (ncol.le.0) go to 210                                                  !/*op         92*/
      if (nocas.eq.nocas0) go to 20                                             !/*op         93*/
  210 continue                                                                  !/*op         94*/
      return                                                                    !/*op         95*/
      entry opt12(hold,nerg,nerg1)                                              !/*op         96*/
      assign 220 to jbr0                                                        !/*op         97*/
      go to 10                                                                  !/*op         98*/
  220 continue                                                                  !/*op         99*/
      iseg=1                                                                    !/*op        100*/
      if (ifseg.eq.1) go to 260                                                 !/*op        101*/
      if (iwind-2) 230,240,250                                                  !/*op        102*/
  230 continue                                                                  !/*op        103*/
      if (yc.lt.xyz(1).or.yc.gt.xyz(2).or.zc.lt.xyz(3).or.zc.gt.xyz(4))         !/*op        104*/
     & iseg=2                                                                   !/*op        105*/
      go to 100                                                                 !/*op        106*/
  240 continue                                                                  !/*op        107*/
      if (zc.lt.xyz(1).or.zc.gt.xyz(2).or.xc.lt.xyz(3).or.xc.gt.xyz(4))         !/*op        108*/
     & iseg=2                                                                   !/*op        109*/
      go to 100                                                                 !/*op        110*/
  250 continue                                                                  !/*op        111*/
      if (xc.lt.xyz(1).or.xc.gt.xyz(2).or.yc.lt.xyz(3).or.yc.gt.xyz(4))         !/*op        112*/
     & iseg=2                                                                   !/*op        113*/
      go to 100                                                                 !/*op        114*/
  260 continue                                                                  !/*op        115*/
      if (iwind-2) 270,280,290                                                  !/*op        116*/
  270 continue                                                                  !/*op        117*/
      if ((yc-xyz(1))**2+(zc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op        118*/
      go to 100                                                                 !/*op        119*/
  280 continue                                                                  !/*op        120*/
      if ((zc-xyz(1))**2+(xc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op        121*/
      go to 100                                                                 !/*op        122*/
  290 continue                                                                  !/*op        123*/
      if ((xc-xyz(1))**2+(yc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op        124*/
      go to 100                                                                 !/*op        125*/
      end                                                                       !/*op        126*/

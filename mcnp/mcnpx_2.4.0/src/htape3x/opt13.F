!     $Id: opt13.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                        !/*op          1*/
      subroutine opt13 (hold,nerg,nerg1,ntim,ntim1)                             !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
      dimension hold(ntim1,nerg1,1)                                             !/*op          8*/
      dimension nc13(8), nc13x(8)                                               !/*op          9*/
      data nc13x /0,0,0,1,0,0,1,0/                                              !/*op         10*/
      data nc13 /0,0,0,1,0,0,0,0/                                               !/*op         11*/
      if (ncol.eq.4) go to 10                                                   !/*op         12*/
      if (nhst.eq.11.and.ncol.eq.7) go to 10                                    !/*op         13*/
      go to 300                                                                 !/*op         14*/
   10 continue                                                                  !/*op         15*/
      do 20 ipt=1,ntype                                                         !/*op         16*/
      if (int(tip).eq.itip(ipt)) go to 30                                       !/*op         17*/
   20 continue                                                                  !/*op         18*/
      go to 300                                                                 !/*op         19*/
   30 continue                                                                  !/*op         20*/
      iphi=1                                                                    !/*op         21*/
      if (nparm1.eq.1) go to 110                                                !/*op         22*/
      go to (40,50,60,70,80,90), kopt                                           !/*op         23*/
   40 if (u.eq.dp0.and.v.eq.dp0) go to 110                                      !/*op         24*/
      phi=atan2(v,u)                                                            !/*op         25*/
      go to 100                                                                 !/*op         26*/
   50 if (u.eq.dp0.and.v.eq.dp0) go to 110                                      !/*op         27*/
      phi=atan2(-u,v)                                                           !/*op         28*/
      go to 100                                                                 !/*op         29*/
   60 if (v.eq.dp0.and.w.eq.dp0) go to 110                                      !/*op         30*/
      phi=atan2(w,v)                                                            !/*op         31*/
      go to 100                                                                 !/*op         32*/
   70 if (v.eq.dp0.and.w.eq.dp0) go to 110                                      !/*op         33*/
      phi=atan2(-v,w)                                                           !/*op         34*/
      go to 100                                                                 !/*op         35*/
   80 if (w.eq.dp0.and.u.eq.dp0) go to 110                                      !/*op         36*/
      phi=atan2(u,w)                                                            !/*op         37*/
      go to 100                                                                 !/*op         38*/
   90 if (w.eq.dp0.and.u.eq.dp0) go to 110                                      !/*op         39*/
      phi=atan2(-w,u)                                                           !/*op         40*/
  100 continue                                                                  !/*op         41*/
      if (phi.lt.0) phi=phi+twopi                                               !/*op         42*/
      iphi=real(nparm)*phi/twopi                                                !/*op         43*/
      iphi=iphi+1                                                               !/*op         44*/
  110 continue                                                                  !/*op         45*/
      ia=1                                                                      !/*op         46*/
      if (nfprm1.eq.1) go to 170                                                !/*op         47*/
      go to (120,120,130,130,140,140), kopt                                     !/*op         48*/
  120 polar=w                                                                   !/*op         49*/
      go to 150                                                                 !/*op         50*/
  130 polar=u                                                                   !/*op         51*/
      go to 150                                                                 !/*op         52*/
  140 polar=v                                                                   !/*op         53*/
  150 continue                                                                  !/*op         54*/
      do 160 ia=1,nfprm                                                         !/*op         55*/
      if (polar.le.fparm(ia)) go to 170                                         !/*op         56*/
  160 continue                                                                  !/*op         57*/
      go to 300                                                                 !/*op         58*/
  170 continue                                                                  !/*op         59*/
      do 180 ie=1,nerg                                                          !/*op         60*/
      if (ec.le.ergb(ie)) go to 190                                             !/*op         61*/
  180 continue                                                                  !/*op         62*/
      go to 300                                                                 !/*op         63*/
  190 continue                                                                  !/*op         64*/
!------ also skip if time convolution is invoked                                !/*op         65*/
      if (ntim1.le.1.or.itconv.ne.0) go to 210                                  !/*op         66*/
      do 200 it=1,ntim                                                          !/*op         67*/
      if (tc.le.timb(it)) go to 220                                             !/*op         68*/
  200 continue                                                                  !/*op         69*/
      go to 300                                                                 !/*op         70*/
  210 continue                                                                  !/*op         71*/
      it=1                                                                      !/*op         72*/
  220 if (itconv.ne.0) call tconv (tc,ntim)                                     !/*op         73*/
      del=wt                                                                    !/*op         74*/
      if (ifeng) del=del*ec                                                     !/*op         75*/
      if (itmult.gt.0) del=del*ec                                               !/*op         76*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op         77*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op         78*/
      if (nfprm1.le.1) go to 260                                                !/*op         79*/
      if (nparm1.le.1) go to 240                                                !/*op         80*/
      l=iphi+nparm1*(ia-1+nfprm1*(ipt-1))                                       !/*op         81*/
      if (itconv.ne.0) then                                                     !/*op         82*/
        do 230 it=1,ntim                                                        !/*op         83*/
        delta=del*ta(it)                                                        !/*op         84*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+delta                        !/*op         85*/
  230   hold(it,ie,l)=hold(it,ie,l)+delta                                       !/*op         86*/
      else                                                                      !/*op         87*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+del                          !/*op         88*/
        hold(it,ie,l)=hold(it,ie,l)+del                                         !/*op         89*/
      endif                                                                     !/*op         90*/
      if (lft) hold(ntim1,ie,l)=hold(ntim1,ie,l)+del                            !/*op         91*/
      if (lfte) hold(ntim1,nerg1,l)=hold(ntim1,nerg1,l)+del                     !/*op         92*/
  240 continue                                                                  !/*op         93*/
      if (nfprm1.le.1) go to 280                                                !/*op         94*/
      l1=nparm1*(ia+nfprm1*(ipt-1))                                             !/*op         95*/
      if (itconv.ne.0) then                                                     !/*op         96*/
        do 250 it=1,ntim                                                        !/*op         97*/
        delta=del*ta(it)                                                        !/*op         98*/
        if (lfe) hold(it,nerg1,l1)=hold(it,nerg1,l1)+delta                      !/*op         99*/
  250   hold(it,ie,l1)=hold(it,ie,l1)+delta                                     !/*op        100*/
      else                                                                      !/*op        101*/
        if (lfe) hold(it,nerg1,l1)=hold(it,nerg1,l1)+del                        !/*op        102*/
        hold(it,ie,l1)=hold(it,ie,l1)+del                                       !/*op        103*/
      endif                                                                     !/*op        104*/
      if (lft) hold(ntim1,ie,l1)=hold(ntim1,ie,l1)+del                          !/*op        105*/
      if (lfte) hold(ntim1,nerg1,l1)=hold(ntim1,nerg1,l1)+del                   !/*op        106*/
  260 continue                                                                  !/*op        107*/
      if (nparm1.le.1) go to 280                                                !/*op        108*/
      l2=iphi+nparm1*(nfprm1*ipt-1)                                             !/*op        109*/
      if (itconv.ne.0) then                                                     !/*op        110*/
        do 270 it=1,ntim                                                        !/*op        111*/
        delta=del*ta(it)                                                        !/*op        112*/
        if (lfe) hold(it,nerg1,l2)=hold(it,nerg1,l2)+delta                      !/*op        113*/
  270   hold(it,ie,l2)=hold(it,ie,l2)+delta                                     !/*op        114*/
      else                                                                      !/*op        115*/
        if (lfe) hold(it,nerg1,l2)=hold(it,nerg1,l2)+del                        !/*op        116*/
        hold(it,ie,l2)=hold(it,ie,l2)+del                                       !/*op        117*/
      endif                                                                     !/*op        118*/
      if (lft) hold(ntim1,ie,l2)=hold(ntim1,ie,l2)+del                          !/*op        119*/
      if (lfte) hold(ntim1,nerg1,l2)=hold(ntim1,nerg1,l2)+del                   !/*op        120*/
  280 continue                                                                  !/*op        121*/
      l3=nparm1*nfprm1*ipt                                                      !/*op        122*/
      if (itconv.ne.0) then                                                     !/*op        123*/
        do 290 it=1,ntim                                                        !/*op        124*/
        delta=del*ta(it)                                                        !/*op        125*/
        if (lfe) hold(it,nerg1,l3)=hold(it,nerg1,l3)+delta                      !/*op        126*/
  290   hold(it,ie,l3)=hold(it,ie,l3)+delta                                     !/*op        127*/
      else                                                                      !/*op        128*/
        if (lfe) hold(it,nerg1,l3)=hold(it,nerg1,l3)+del                        !/*op        129*/
        hold(it,ie,l3)=hold(it,ie,l3)+del                                       !/*op        130*/
      endif                                                                     !/*op        131*/
      if (lft) hold(ntim1,ie,l3)=hold(ntim1,ie,l3)+del                          !/*op        132*/
      if (lfte) hold(ntim1,nerg1,l3)=hold(ntim1,nerg1,l3)+del                   !/*op        133*/
  300 continue                                                                  !/*op        134*/
      if (nhst.ne.11) call srchnp (nc13)                                        !/*op        135*/
      if (nhst.eq.11) call srchnp (nc13x)                                       !/*op        136*/
      if (ncol.le.0) go to 310                                                  !/*op        137*/
      if (nocas.eq.nocas0) go to 10                                             !/*op        138*/
  310 continue                                                                  !/*op        139*/
      return                                                                    !/*op        140*/
      end                                                                       !/*op        141*/

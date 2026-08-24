!     $Id: opt9.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                         !/*op          1*/
      subroutine opt9 (hold,nerg,nerg1,ntim,ntim1)                              !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
#include <htape3x/box.h>                                                        /*!op          8*/
      dimension hold(ntim1,nerg1,1)                                             !/*op          9*/
      dimension nc1(8), nc1x(8)                                                 !/*op         10*/
      dimension nc4(8)                                                          !/*op         11*/
      data nc4 /0,1,0,1,1,1,1,0/                                                !/*op         12*/
      data nc1x /0,0,0,1,0,0,0,0/, nc1 /0,0,0,1,0,0,1,0/                        !/*op         13*/
      assign 260 to jbr0                                                        !/*op         14*/
   10 continue                                                                  !/*op         15*/
      if (ncol.ne.4.and.ncol.ne.7) go to 290                                    !/*op         16*/
   20 continue                                                                  !/*op         17*/
      do 30 ipt=1,ntype                                                         !/*op         18*/
      if (int(tip).eq.itip(ipt)) go to 40                                       !/*op         19*/
   30 continue                                                                  !/*op         20*/
      go to 290                                                                 !/*op         21*/
   40 continue                                                                  !/*op         22*/
      ja=lelem                                                                  !/*op         23*/
      do 50 is=1,nparm                                                          !/*op         24*/
      if (ja.eq.lparm(is)) go to 60                                             !/*op         25*/
   50 continue                                                                  !/*op         26*/
      go to 290                                                                 !/*op         27*/
   60 continue                                                                  !/*op         28*/
      iseg=1                                                                    !/*op         29*/
      if (ifseg.eq.1) go to 100                                                 !/*op         30*/
      if (iwind-2) 70,80,90                                                     !/*op         31*/
   70 continue                                                                  !/*op         32*/
      if (yc.lt.xyz(1).or.yc.gt.xyz(2).or.zc.lt.xyz(3).or.zc.gt.xyz(4))         !/*op         33*/
     & iseg=2                                                                   !/*op         34*/
      go to 140                                                                 !/*op         35*/
   80 continue                                                                  !/*op         36*/
      if (zc.lt.xyz(1).or.zc.gt.xyz(2).or.xc.lt.xyz(3).or.xc.gt.xyz(4))         !/*op         37*/
     & iseg=2                                                                   !/*op         38*/
      go to 140                                                                 !/*op         39*/
   90 continue                                                                  !/*op         40*/
      if (xc.lt.xyz(1).or.xc.gt.xyz(2).or.yc.lt.xyz(3).or.yc.gt.xyz(4))         !/*op         41*/
     & iseg=2                                                                   !/*op         42*/
      go to 140                                                                 !/*op         43*/
  100 continue                                                                  !/*op         44*/
      if (iwind-2) 110,120,130                                                  !/*op         45*/
  110 continue                                                                  !/*op         46*/
      if ((yc-xyz(1))**2+(zc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op         47*/
      go to 140                                                                 !/*op         48*/
  120 continue                                                                  !/*op         49*/
      if ((zc-xyz(1))**2+(xc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op         50*/
      go to 140                                                                 !/*op         51*/
  130 continue                                                                  !/*op         52*/
      if ((xc-xyz(1))**2+(yc-xyz(2))**2.gt.xyz(3)) iseg=2                       !/*op         53*/
  140 continue                                                                  !/*op         54*/
      do 150 ie=1,nerg                                                          !/*op         55*/
      if (ec.le.ergb(ie)) go to 160                                             !/*op         56*/
  150 continue                                                                  !/*op         57*/
      go to 290                                                                 !/*op         58*/
  160 continue                                                                  !/*op         59*/
!------ also skip if time convolution is invoked                                !/*op         60*/
      if (ntim1.le.1.or.itconv.ne.0) go to 180                                  !/*op         61*/
      do 170 it=1,ntim                                                          !/*op         62*/
      if (tc.le.timb(it)) go to 190                                             !/*op         63*/
  170 continue                                                                  !/*op         64*/
      go to 290                                                                 !/*op         65*/
  180 continue                                                                  !/*op         66*/
      it=1                                                                      !/*op         67*/
  190 continue                                                                  !/*op         68*/
      if (.not.lbunch) l1=iseg+nseg1*(is-1+nparm*(ipt-1))                       !/*op         69*/
      if (lbunch) l1=iseg+nseg1*(ipt-1)                                         !/*op         70*/
      l=0                                                                       !/*op         71*/
      if (nfprm.eq.0) go to 250                                                 !/*op         72*/
      if (kopt.le.0) go to 210                                                  !/*op         73*/
      if (kopt.eq.4) go to 200                                                  !/*op         74*/
      fmu0=dc(kopt)                                                             !/*op         75*/
      go to 220                                                                 !/*op         76*/
  200 continue                                                                  !/*op         77*/
      fmu0=u*cv(1)+v*cv(2)+w*cv(3)                                              !/*op         78*/
      go to 220                                                                 !/*op         79*/
  210 continue                                                                  !/*op         80*/
      fmu0=fmu                                                                  !/*op         81*/
  220 continue                                                                  !/*op         82*/
      do 230 ia=1,nfprm                                                         !/*op         83*/
      if (fmu0.le.fparm(ia)) go to 240                                          !/*op         84*/
  230 continue                                                                  !/*op         85*/
      go to 290                                                                 !/*op         86*/
  240 continue                                                                  !/*op         87*/
      l=ia+nfprm1*(l1-1)                                                        !/*op         88*/
  250 if (itconv.ne.0) call tconv (tc,ntim)                                     !/*op         89*/
      ll=nfprm1*l1                                                              !/*op         90*/
      go to jbr0, (260,310)                                                     !/*op         91*/
  260 continue                                                                  !/*op         92*/
      del=wt                                                                    !/*op         93*/
      if (ifeng) del=del*ec                                                     !/*op         94*/
      if (itmult.gt.0) del=del*ec                                               !/*op         95*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op         96*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op         97*/
      if (itconv.ne.0) then                                                     !/*op         98*/
        do 270 it=1,ntim                                                        !/*op         99*/
        delta=del*ta(it)                                                        !/*op        100*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+delta                      !/*op        101*/
  270   hold(it,ie,ll)=hold(it,ie,ll)+delta                                     !/*op        102*/
      else                                                                      !/*op        103*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+del                        !/*op        104*/
        hold(it,ie,ll)=hold(it,ie,ll)+del                                       !/*op        105*/
      endif                                                                     !/*op        106*/
      if (lft) hold(ntim1,ie,ll)=hold(ntim1,ie,ll)+del                          !/*op        107*/
      if (lfte) hold(ntim1,nerg1,ll)=hold(ntim1,nerg1,ll)+del                   !/*op        108*/
      if (l.le.0) go to 290                                                     !/*op        109*/
      if (itconv.ne.0) then                                                     !/*op        110*/
        do 280 it=1,ntim                                                        !/*op        111*/
        delta=del*ta(it)                                                        !/*op        112*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+delta                        !/*op        113*/
  280   hold(it,ie,l)=hold(it,ie,l)+delta                                       !/*op        114*/
      else                                                                      !/*op        115*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+del                          !/*op        116*/
        hold(it,ie,l)=hold(it,ie,l)+del                                         !/*op        117*/
      endif                                                                     !/*op        118*/
      if (lft) hold(ntim1,ie,l)=hold(ntim1,ie,l)+del                            !/*op        119*/
      if (lfte) hold(ntim1,nerg1,l)=hold(ntim1,nerg1,l)+del                     !/*op        120*/
  290 continue                                                                  !/*op        121*/
      call srchnp (nc1)                                                         !/*op        122*/
      if (ncol.le.0) go to 300                                                  !/*op        123*/
      if (nocas.eq.nocas0) go to 20                                             !/*op        124*/
  300 continue                                                                  !/*op        125*/
      return                                                                    !/*op        126*/
      entry opt10(hold,nerg,nerg1,ntim,ntim1)                                   !/*op        127*/
      assign 310 to jbr0                                                        !/*op        128*/
      go to 10                                                                  !/*op        129*/
  310 continue                                                                  !/*op        130*/
      temp=abs(fmu)                                                             !/*op        131*/
      if (temp.le.0.1) temp=0.05                                                !/*op        132*/
      del=wt/temp                                                               !/*op        133*/
      if (ifeng) del=del*ec                                                     !/*op        134*/
      if (itmult.gt.0) del=del*ec                                               !/*op        135*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op        136*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op        137*/
      if (itconv.ne.0) then                                                     !/*op        138*/
        do 320 it=1,ntim                                                        !/*op        139*/
        delta=del*ta(it)                                                        !/*op        140*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+delta                      !/*op        141*/
  320   hold(it,ie,ll)=hold(it,ie,ll)+delta                                     !/*op        142*/
      else                                                                      !/*op        143*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+del                        !/*op        144*/
        hold(it,ie,ll)=hold(it,ie,ll)+del                                       !/*op        145*/
      endif                                                                     !/*op        146*/
      if (lft) hold(ntim1,ie,ll)=hold(ntim1,ie,ll)+del                          !/*op        147*/
      if (lfte) hold(ntim1,nerg1,ll)=hold(ntim1,nerg1,ll)+del                   !/*op        148*/
      call srchnp (nc1)                                                         !/*op        149*/
      if (ncol.le.0) go to 330                                                  !/*op        150*/
      if (nocas.eq.nocas0) go to 20                                             !/*op        151*/
  330 continue                                                                  !/*op        152*/
      return                                                                    !/*op        153*/
      end                                                                       !/*op        154*/

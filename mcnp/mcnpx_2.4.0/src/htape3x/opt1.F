!     $Id: opt1.F,v 1.4 2002/03/27 18:14:00 gwm Exp $                         !/*op          1*/
      subroutine opt1 (hold,nerg,nerg1,ntim,ntim1)                              !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
      dimension hold(ntim1,nerg1,1)                                             !/*op          8*/
      dimension nc1(8), nc1x(8)                                                 !/*op          9*/
      dimension nc4(8)                                                          !/*op         10*/
      data nc4 /0,1,0,1,1,1,1,0/                                                !/*op         11*/
      data nc1x /0,0,0,1,0,0,0,0/, nc1 /0,0,0,1,0,0,1,0/                        !/*op         12*/
      assign 200 to jbr0                                                        !/*op         13*/
      if (ncol.eq.4) go to 10                                                   !/*op         14*/
      if (nparm.eq.0.or.ncol.ne.7) go to 240                                    !/*op         15*/
   10 continue                                                                  !/*op         16*/
      do 20 ipt=1,ntype                                                         !/*op         17*/
      if (int(tip).eq.itip(ipt)) go to 30                                       !/*op         18*/
   20 continue                                                                  !/*op         19*/
      go to 240                                                                 !/*op         20*/
   30 continue                                                                  !/*op         21*/
      if (nparm.eq.0) go to 50                                                  !/*op         22*/
      ja=lelem                                                                  !/*op         23*/
      do 40 is=1,nparm                                                          !/*op         24*/
      if (ja.eq.lparm(is)) go to 50                                             !/*op         25*/
   40 continue                                                                  !/*op         26*/
      go to 250                                                                 !/*op         27*/
   50 continue                                                                  !/*op         28*/
      iseg=nseg1                                                                !/*op         29*/
      if (nseg.eq.0) go to 70                                                   !/*op         30*/
      if (ifseg.lt.0) go to 800                                                 !/*op         31*/
      do 60 iseg=1,nseg                                                         !/*op         32*/
      if (cx(ifseg).le.fseg(iseg)) go to 70                                     !/*op         33*/
   60 continue                                                                  !/*op         34*/
      iseg=nseg1                                                                !/*op         35*/
      go to 70                                                                  !/*op         36*/
  800 continue                                                                  !/*op         37*/
      do 810 iseg=1,nseg                                                        !/*op         38*/
      if (ifseg.eq.-1) test=cx(2)**2+cx(3)**2                                   !/*op         39*/
      if (ifseg.eq.-2) test=cx(1)**2+cx(3)**2                                   !/*op         40*/
      if (ifseg.eq.-3) test=cx(1)**2+cx(2)**2                                   !/*op         41*/
      if (test.le.fseg(iseg)**2) go to 70                                       !/*op         42*/
  810 continue                                                                  !/*op         43*/
      iseg=nseg1                                                                !/*op         44*/
   70 continue                                                                  !/*op         45*/
      if (lbunch.or.nparm.eq.0) then                                            !/*op         46*/
        l1=iseg+nseg1*(ipt-1)                                                   !/*op         47*/
      else                                                                      !/*op         48*/
        l1=iseg+nseg1*(is-1+nparm*(ipt-1))                                      !/*op         49*/
      endif                                                                     !/*op         50*/
      l=0                                                                       !/*op         51*/
      if (nfprm.eq.0) go to 130                                                 !/*op         52*/
      if (kopt.le.0) go to 90                                                   !/*op         53*/
      if (kopt.eq.4) go to 80                                                   !/*op         54*/
      fmu0=dc(kopt)                                                             !/*op         55*/
      go to 100                                                                 !/*op         56*/
   80 continue                                                                  !/*op         57*/
      fmu0=u*cv(1)+v*cv(2)+w*cv(3)                                              !/*op         58*/
      go to 100                                                                 !/*op         59*/
   90 continue                                                                  !/*op         60*/
      fmu0=fmu                                                                  !/*op         61*/
  100 continue                                                                  !/*op         62*/
      do 110 ia=1,nfprm                                                         !/*op         63*/
      if (fmu0.le.fparm(ia)) go to 120                                          !/*op         64*/
  110 continue                                                                  !/*op         65*/
      go to 240                                                                 !/*op         66*/
  120 continue                                                                  !/*op         67*/
      l=ia+nfprm1*(l1-1)                                                        !/*op         68*/
  130 continue                                                                  !/*op         69*/
      ll=nfprm1*l1                                                              !/*op         70*/
      go to 140                                                                 !/*op         71*/
  140 continue                                                                  !/*op         72*/
      do 150 ie=1,nerg                                                          !/*op         73*/
      if (ec.le.ergb(ie)) go to 160                                             !/*op         74*/
  150 continue                                                                  !/*op         75*/
      go to 240                                                                 !/*op         76*/
  160 continue                                                                  !/*op         77*/
!------ also skip if time convolution is invoked                                !/*op         78*/
      if (ntim1.le.1.or.itconv.ne.0) go to 180                                  !/*op         79*/
      do 170 it=1,ntim                                                          !/*op         80*/
      if (tc.le.timb(it)) go to 190                                             !/*op         81*/
  170 continue                                                                  !/*op         82*/
      go to 240                                                                 !/*op         83*/
  180 continue                                                                  !/*op         84*/
      it=1                                                                      !/*op         85*/
  190 if (itconv.ne.0) call tconv (tc,ntim)                                     !/*op         86*/
      go to jbr0, (200,280)                                                     !/*op         87*/
  200 continue                                                                  !/*op         88*/
      del=wt                                                                    !/*op         89*/
      if (ifeng) del=del*ec                                                     !/*op         90*/
      if (itmult.gt.0) del=del*ec                                               !/*op         91*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op         92*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op         93*/
      if (l.eq.0) go to 220                                                     !/*op         94*/
      if (itconv.ne.0) then                                                     !/*op         95*/
        do 210 it=1,ntim                                                        !/*op         96*/
        delta=del*ta(it)                                                        !/*op         97*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+delta                        !/*op         98*/
  210   hold(it,ie,l)=hold(it,ie,l)+delta                                       !/*op         99*/
      else                                                                      !/*op        100*/
        if (lfe) hold(it,nerg1,l)=hold(it,nerg1,l)+del                          !/*op        101*/
        hold(it,ie,l)=hold(it,ie,l)+del                                         !/*op        102*/
      endif                                                                     !/*op        103*/
      if (lft) hold(ntim1,ie,l)=hold(ntim1,ie,l)+del                            !/*op        104*/
      if (lfte) hold(ntim1,nerg1,l)=hold(ntim1,nerg1,l)+del                     !/*op        105*/
  220 continue                                                                  !/*op        106*/
      if (itconv.ne.0) then                                                     !/*op        107*/
        do 230 it=1,ntim                                                        !/*op        108*/
        delta=del*ta(it)                                                        !/*op        109*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+delta                      !/*op        110*/
  230   hold(it,ie,ll)=hold(it,ie,ll)+delta                                     !/*op        111*/
      else                                                                      !/*op        112*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+del                        !/*op        113*/
        hold(it,ie,ll)=hold(it,ie,ll)+del                                       !/*op        114*/
      endif                                                                     !/*op        115*/
      if (lft) hold(ntim1,ie,ll)=hold(ntim1,ie,ll)+del                          !/*op        116*/
      if (lfte) hold(ntim1,nerg1,ll)=hold(ntim1,nerg1,ll)+del                   !/*op        117*/
  240 continue                                                                  !/*op        118*/
      if (nparm.gt.0) go to 250                                                 !/*op        119*/
      call srchnp (nc1x)                                                        !/*op        120*/
      go to 260                                                                 !/*op        121*/
  250 continue                                                                  !/*op        122*/
      call srchnp (nc1)                                                         !/*op        123*/
  260 continue                                                                  !/*op        124*/
      if (ncol.le.0) go to 270                                                  !/*op        125*/
      if (nocas.eq.nocas0) go to 10                                             !/*op        126*/
  270 continue                                                                  !/*op        127*/
      return                                                                    !/*op        128*/
      entry opt2(hold,nerg,nerg1,ntim,ntim1)                                    !/*op        129*/
      assign 280 to jbr0                                                        !/*op        130*/
      if (ncol.ne.4.and.ncol.ne.7) go to 250                                    !/*op        131*/
      go to 10                                                                  !/*op        132*/
  280 continue                                                                  !/*op        133*/
      temp=abs(fmu)                                                             !/*op        134*/
      if (temp.le.0.1) temp=0.05                                                !/*op        135*/
      del=wt/temp                                                               !/*op        136*/
      if (ifeng) del=del*ec                                                     !/*op        137*/
      if (itmult.gt.0) del=del*ec                                               !/*op        138*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op        139*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op        140*/
      if (itconv.ne.0) then                                                     !/*op        141*/
        do 290 it=1,ntim                                                        !/*op        142*/
        delta=del*ta(it)                                                        !/*op        143*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+delta                      !/*op        144*/
  290   hold(it,ie,ll)=hold(it,ie,ll)+delta                                     !/*op        145*/
      else                                                                      !/*op        146*/
        if (lfe) hold(it,nerg1,ll)=hold(it,nerg1,ll)+del                        !/*op        147*/
        hold(it,ie,ll)=hold(it,ie,ll)+del                                       !/*op        148*/
      endif                                                                     !/*op        149*/
      if (lft) hold(ntim1,ie,ll)=hold(ntim1,ie,ll)+del                          !/*op        150*/
      if (lfte) hold(ntim1,nerg1,ll)=hold(ntim1,nerg1,ll)+del                   !/*op        151*/
      go to 250                                                                 !/*op        152*/
      end                                                                       !/*op        153*/

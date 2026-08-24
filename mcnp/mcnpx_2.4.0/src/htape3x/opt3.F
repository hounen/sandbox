!     $Id: opt3.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                         !/*op          1*/
      subroutine opt3 (hold,nerg,nerg1,ndim2)                                   !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/kpt.h>                                                        /*!op          7*/
      dimension hold(nerg1,ndim2,1)                                             !/*op          8*/
      dimension nc3(8), ll(2)                                                   !/*op          9*/
      data nc3 /0,1,1,0,1,1,0,0/                                                !/*op         10*/
      if (ncol.lt.2.or.ncol.eq.4.or.ncol.gt.6) go to 110                        !/*op         11*/
   10 continue                                                                  !/*op         12*/
      if (ifeng.and.(name.ne.1)) go to 110                                      !/*op         13*/
      if (kopt.gt.3.and.mnuc.ne.2) go to 110                                    !/*op         14*/
      if (nparm.eq.0) go to 30                                                  !/*op         15*/
      if (jmat.eq.0) icomp=bold                                                 !/*op         16*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         17*/
      do 20 ib=1,nparm                                                          !/*op         18*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         19*/
   20 continue                                                                  !/*op         20*/
   30 continue                                                                  !/*op         21*/
      if (lbunch) go to 110                                                     !/*op         22*/
      ib=0                                                                      !/*op         23*/
   40 continue                                                                  !/*op         24*/
      if (lbunch) ib=0                                                          !/*op         25*/
      nup=nabov+nbelo                                                           !/*op         26*/
      if (nup.le.0) go to 100                                                   !/*op         27*/
      do 90 j=1,nup                                                             !/*op         28*/
      if (j.gt.nabov) then                                                      !/*op         29*/
        i=l02+j-nabov                                                           !/*op         30*/
      else                                                                      !/*op         31*/
        i=j                                                                     !/*op         32*/
      endif                                                                     !/*op         33*/
      ipt=tipa(i)+1.0                                                           !/*op         34*/
      jpt=jtipa(i)                                                              !/*op         35*/
      ie=0                                                                      !/*op         36*/
      if (.not.lfe) go to 60                                                    !/*op         37*/
      do 50 ie=1,nerg                                                           !/*op         38*/
      if (ea(i).le.ergb(ie)) go to 60                                           !/*op         39*/
   50 continue                                                                  !/*op         40*/
      ie=0                                                                      !/*op         41*/
   60 continue                                                                  !/*op         42*/
      ll(1)=ipt                                                                 !/*op         43*/
      l2=0                                                                      !/*op         44*/
      if (kopt.eq.3) go to 70                                                   !/*op         45*/
      if (jpt.eq.0) then                                                        !/*op         46*/
!  LPT+1 to LPT+5                                                               !/*op         47*/
! p_casc    n_casc   pi+_casc  pi0_casc  pi-_casc                               !/*op         48*/
!  LPT+6 to LPT+10                                                              !/*op         49*/
! pi+_decay  pi0_decay  pi-_decay    ph_decay   ph_de-ex                        !/*op         50*/
!  LPT+11 to LPT+16                                                             !/*op         51*/
! p_preq    n_preq    d_preq    t_preq    He3_pre   a_preq                      !/*op         52*/
!  LPT+17 to LPT+22                                                             !/*op         53*/
! p_evap    n_evap    d_evap    t_evap    He3_eva   a_evap                      !/*op         54*/
!  1 to LPT                                  particles from all sources         !/*op         55*/
        if (ipt.le.5) l2=ipt+lpt                                                !/*op         56*/
      elseif (jpt.eq.1) then                                                    !/*op         57*/
        if (ipt.eq.12) l2=lpt+9                                                 !/*op         58*/
        if (ipt.ge.3.and.ipt.le.5) l2=lpt+ipt+3                                 !/*op         59*/
      elseif (jpt.eq.2) then                                                    !/*op         60*/
        if (ipt.eq.12) l2=lpt+10                                                !/*op         61*/
        if (ipt.le.2) l2=ipt+lpt+10                                             !/*op         62*/
        if (ipt.ge.8.and.ipt.le.11) l2=ipt+lpt+5                                !/*op         63*/
      elseif (jpt.eq.5) then                                                    !/*op         64*/
        if (ipt.eq.12) l2=lpt+10                                                !/*op         65*/
        if (kopt.ne.5) then                                                     !/*op         66*/
          if (ipt.le.2) l2=ipt+lpt+16                                           !/*op         67*/
          if (ipt.ge.8.and.ipt.le.11) l2=ipt+lpt+11                             !/*op         68*/
        endif                                                                   !/*op         69*/
      elseif (jpt.eq.6.or.jpt.eq.7) then                                        !/*op         70*/
        if (ipt.eq.12) l2=lpt+10                                                !/*op         71*/
        if (kopt.ne.4) then                                                     !/*op         72*/
          if (ipt.le.2) l2=ipt+lpt+16                                           !/*op         73*/
          if (ipt.ge.8.and.ipt.le.11) l2=ipt+lpt+11                             !/*op         74*/
        endif                                                                   !/*op         75*/
      else                                                                      !/*op         76*/
        call errorf ('opt3.1  ')                                                !/*op         77*/
      endif                                                                     !/*op         78*/
      ll(2)=l2                                                                  !/*op         79*/
   70 continue                                                                  !/*op         80*/
      do 80 k=1,2                                                               !/*op         81*/
      l=ll(k)                                                                   !/*op         82*/
      if (l.eq.0) go to 80                                                      !/*op         83*/
      if (ie.gt.0.and.ib.gt.0) hold(ie,ib,l)=hold(ie,ib,l)+wta(i)               !/*op         84*/
      if (ib.gt.0) hold(nerg1-2,ib,l)=hold(nerg1-2,ib,l)+wta(i)                 !/*op         85*/
      if (ie.gt.0) hold(ie,nbin,l)=hold(ie,nbin,l)+wta(i)                       !/*op         86*/
      hold(nerg1-2,nbin,l)=hold(nerg1-2,nbin,l)+wta(i)                          !/*op         87*/
      if (k.eq.1) then                                                          !/*op         88*/
        hold(nerg1,nbin,l)=hold(nerg1,nbin,l)+wta(i)                            !/*op         89*/
        if (ib.gt.0) hold(nerg1,ib,l)=hold(nerg1,ib,l)+wta(i)                   !/*op         90*/
      endif                                                                     !/*op         91*/
   80 continue                                                                  !/*op         92*/
   90 continue                                                                  !/*op         93*/
      if (ncol.eq.3.or.ncol.eq.5) go to 100                                     !/*op         94*/
      ityp=tip+dp1                                                              !/*op         95*/
      hold(nerg1-1,nbin,ityp)=hold(nerg1-1,nbin,ityp)+wt                        !/*op         96*/
      hold(nerg1,nbin,ityp)=hold(nerg1,nbin,ityp)-wt                            !/*op         97*/
      if (ib.gt.0) then                                                         !/*op         98*/
        hold(nerg1-1,ib,ityp)=hold(nerg1-1,ib,ityp)+wt                          !/*op         99*/
        hold(nerg1,ib,ityp)=hold(nerg1,ib,ityp)-wt                              !/*op        100*/
      endif                                                                     !/*op        101*/
  100 continue                                                                  !/*op        102*/
  110 continue                                                                  !/*op        103*/
      call srchn (nc3)                                                          !/*op        104*/
      if (nocas.ne.nocas0.or.ncol.le.0) return                                  !/*op        105*/
      go to 10                                                                  !/*op        106*/
      end                                                                       !/*op        107*/

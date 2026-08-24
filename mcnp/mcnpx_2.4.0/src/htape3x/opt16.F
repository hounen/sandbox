!     $Id: opt16.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                        !/*op          1*/
      subroutine opt16 (hold,nerg,nerg1,ntim)                                   !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
      dimension hold(ntim,nerg1,1)                                              !/*op          8*/
      dimension nc3(8)                                                          !/*op          9*/
      dimension temp(4,2)                                                       !/*op         10*/
      data nc3 /0,1,0,0,0,1,0,0/                                                !/*op         11*/
      if (ncol.ne.2.and.ncol.ne.6) go to 100                                    !/*op         12*/
   10 continue                                                                  !/*op         13*/
      if (lelem.le.0) go to 100                                                 !/*op         14*/
      if (nparm.eq.0) go to 30                                                  !/*op         15*/
      if (jmat.eq.0) icomp=bold                                                 !/*op         16*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         17*/
      do 20 ib=1,nparm                                                          !/*op         18*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         19*/
   20 continue                                                                  !/*op         20*/
   30 continue                                                                  !/*op         21*/
      if (lbunch) go to 100                                                     !/*op         22*/
      ib=0                                                                      !/*op         23*/
   40 continue                                                                  !/*op         24*/
      if (lbunch) ib=0                                                          !/*op         25*/
      if (mnuc.le.0) go to 100                                                  !/*op         26*/
      do 60 i=1,mnuc                                                            !/*op         27*/
      do 50 j=1,4                                                               !/*op         28*/
   50 temp(j,i)=dp0                                                             !/*op         29*/
      if (aprr(i).le.dp0) go to 60                                              !/*op         30*/
      temp(1,i)=erecr(i)                                                        !/*op         31*/
      a1=aprr(i)                                                                !/*op         32*/
      z1=zprr(i)                                                                !/*op         33*/
      e1=erecr(i)                                                               !/*op         34*/
      temp(3,i)=erecr(i)*flin(a1,z1,e1)                                         !/*op         35*/
      ielstc=0                                                                  !/*op         36*/
      if (mnuc.eq.1) call recels (ielstc)                                       !/*op         37*/
      if (ielstc.eq.1) then                                                     !/*op         38*/
        temp(2,1)=temp(1,1)                                                     !/*op         39*/
        temp(4,1)=temp(3,1)                                                     !/*op         40*/
      endif                                                                     !/*op         41*/
   60 continue                                                                  !/*op         42*/
      do 90 l=1,mnuc                                                            !/*op         43*/
      do 90 it=1,4                                                              !/*op         44*/
      if (temp(it,l).le.dp0) go to 90                                           !/*op         45*/
      do 70 ie=1,nerg                                                           !/*op         46*/
      if (temp(it,l).le.ergb(ie)) go to 80                                      !/*op         47*/
   70 continue                                                                  !/*op         48*/
      ie=0                                                                      !/*op         49*/
   80 continue                                                                  !/*op         50*/
      del=wt                                                                    !/*op         51*/
      if (ifeng) del=del*temp(it,l)                                             !/*op         52*/
      if (ib.gt.0.and.ie.gt.0) hold(it,ie,ib)=hold(it,ie,ib)+del                !/*op         53*/
      if (ib.gt.0) hold(it,nerg1-1,ib)=hold(it,nerg1-1,ib)+wt                   !/*op         54*/
      if (ib.gt.0) hold(it,nerg1,ib)=hold(it,nerg1,ib)+wt*temp(it,l)            !/*op         55*/
      if (ie.gt.0) hold(it,ie,nbin)=hold(it,ie,nbin)+del                        !/*op         56*/
      hold(it,nerg1-1,nbin)=hold(it,nerg1-1,nbin)+wt                            !/*op         57*/
      hold(it,nerg1,nbin)=hold(it,nerg1,nbin)+wt*temp(it,l)                     !/*op         58*/
   90 continue                                                                  !/*op         59*/
  100 continue                                                                  !/*op         60*/
      call srchn (nc3)                                                          !/*op         61*/
      if (nocas.ne.nocas0.or.ncol.le.0) return                                  !/*op         62*/
      go to 10                                                                  !/*op         63*/
      end                                                                       !/*op         64*/

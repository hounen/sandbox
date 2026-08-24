!     $Id: opt6.F,v 1.5 2002/03/27 18:14:01 gwm Exp $                         !/*op          1*/
      subroutine opt6 (hold,ntim,ntim1)                                         !/*op          2*/
#ifdef LCS                                                                      /*!op          3*/
#include <htape3x/com32b.h>                                                     /*!op          4*/
#include <htape3x/phprm0.h>                                                     /*!op          5*/
      dimension hold(ntim1,18,1)                                                !/*op          6*/
#include <htape3x/label.h>                                                      /*!op          7*/
#include <htape3x/exm.h>                                                        /*!op          8*/
#include <htape3x/contrl.h>                                                     /*!op          9*/
#include <htape3x/bndy.h>                                                       /*!op         10*/
#include <htape3x/ctconv.h>                                                     /*!op         11*/
      dimension bin(17), ika(22)                                                !/*op         12*/
      dimension instab(lpt)                                                     !/*op         13*/
      data instab /2*0,5*1,5*0,4*1,6*0/                                         !/*op         14*/
      data bin /17*dp0/                                                         !/*op         15*/
      data ika /1,0,2,0,2,3,3,6,7,8,9,0,4,0,0,4,5,0,13,13,0,0/                  !/*op         16*/
   10 continue                                                                  !/*op         17*/
      if (ncol.lt.2.or.ncol.gt.8) go to 190                                     !/*op         18*/
      if (ifeng.and.(name.ne.1)) go to 190                                      !/*op         19*/
      if (nparm.le.0) go to 30                                                  !/*op         20*/
      if (jmat.eq.0) icomp=bold                                                 !/*op         21*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         22*/
      do 20 ib=1,nparm                                                          !/*op         23*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         24*/
   20 continue                                                                  !/*op         25*/
   30 continue                                                                  !/*op         26*/
      if (lbunch) go to 190                                                     !/*op         27*/
      ib=0                                                                      !/*op         28*/
   40 continue                                                                  !/*op         29*/
      if (lbunch) ib=0                                                          !/*op         30*/
!------ also skip if time convolution is invoked                                !/*op         31*/
      if (ntim1.le.1.or.itconv.ne.0) go to 60                                   !/*op         32*/
      do 50 it=1,ntim                                                           !/*op         33*/
      if (tc.lt.timb(it)) go to 70                                              !/*op         34*/
   50 continue                                                                  !/*op         35*/
      go to 190                                                                 !/*op         36*/
   60 continue                                                                  !/*op         37*/
      it=1                                                                      !/*op         38*/
   70 continue                                                                  !/*op         39*/
      if (itconv.ne.0) call tconv (tc,ntim)                                     !/*op         40*/
      ityp=tip+1.0                                                              !/*op         41*/
      if (ifeng) then                                                           !/*op         42*/
        if (ncol.le.1) go to 200                                                !/*op         43*/
        go to 140                                                               !/*op         44*/
      endif                                                                     !/*op         45*/
      go to (190,80,130,140,100,80,140,140), ncol                               !/*op         46*/
   80 continue                                                                  !/*op         47*/
! NCOL=6                                                                        !/*op         48*/
! NCOL=2                                                                        !/*op         49*/
      if (mnuc.eq.0) go to 110                                                  !/*op         50*/
      do 90 i=1,mnuc                                                            !/*op         51*/
      bin(10)=bin(10)+wt*erecr(i)                                               !/*op         52*/
      bin(11)=bin(11)+wt*uur(i)                                                 !/*op         53*/
      bin(13)=bin(13)+wt*ecvr(i)                                                !/*op         54*/
   90 continue                                                                  !/*op         55*/
  100 continue                                                                  !/*op         56*/
! NCOL=5                                                                        !/*op         57*/
  110 continue                                                                  !/*op         58*/
      nup=nabov+nbelo                                                           !/*op         59*/
      if (nup.le.0) go to 140                                                   !/*op         60*/
      do 120 j=1,nup                                                            !/*op         61*/
      if (n1col.ge.0.and.j.le.nabov) go to 120                                  !/*op         62*/
      if (j.gt.nabov) then                                                      !/*op         63*/
        i=l02+j-nabov                                                           !/*op         64*/
      else                                                                      !/*op         65*/
        i=j                                                                     !/*op         66*/
      endif                                                                     !/*op         67*/
      itp=tipa(i)+1.0                                                           !/*op         68*/
      ik=ika(itp)                                                               !/*op         69*/
      if (ik.gt.0) bin(ik)=bin(ik)+wta(i)*ea(i)                                 !/*op         70*/
      if (itp.eq.12) bin(12)=bin(12)+wta(i)*ea(i)                               !/*op         71*/
      if (itp.eq.19) bin(14)=bin(14)+dp2*elmass*wta(i)                          !/*op         72*/
!   needs fix for itp > 11                                                      !/*op         73*/
      if (n1col.lt.0) go to 120                                                 !/*op         74*/
      if (npidk.le.0.and.itp.eq.5) go to 120                                    !/*op         75*/
      if (ibertp.le.0) go to 120                                                !/*op         76*/
      if (itp.ne.3.and.itp.ne.5) go to 120                                      !/*op         77*/
      enu=engnu*(1.0+ea(i)/pmass(3))                                            !/*op         78*/
      emu=ea(i)+pmass(3)-enu-pmass(6)                                           !/*op         79*/
      bin(15)=bin(15)+emu*wta(i)                                                !/*op         80*/
      if (itp.eq.3) bin(16)=bin(16)+wta(i)*pmass(6)                             !/*op         81*/
      if (itp.eq.5) bin(17)=bin(17)+wta(i)*pmass(6)                             !/*op         82*/
  120 continue                                                                  !/*op         83*/
      go to 140                                                                 !/*op         84*/
  130 continue                                                                  !/*op         85*/
! NCOL=3                                                                        !/*op         86*/
      if (instab(ityp).ne.0) go to 110                                          !/*op         87*/
      ec=dp0                                                                    !/*op         88*/
  140 continue                                                                  !/*op         89*/
! NCOL=7                                                                        !/*op         90*/
! NCOL=4                                                                        !/*op         91*/
! NCOL=8                                                                        !/*op         92*/
      ik=ika(ityp)                                                              !/*op         93*/
      if (ik.gt.0) bin(ik)=bin(ik)+oldwt*(e-ec)                                 !/*op         94*/
      do 180 i=1,17                                                             !/*op         95*/
      if (itconv.ne.0) then                                                     !/*op         96*/
        do 150 it=1,ntim                                                        !/*op         97*/
        delta=bin(i)*ta(it)                                                     !/*op         98*/
        hold(it,i,nbin)=hold(it,i,nbin)+delta                                   !/*op         99*/
  150   hold(it,18,nbin)=hold(it,18,nbin)+delta                                 !/*op        100*/
      else                                                                      !/*op        101*/
        hold(it,i,nbin)=hold(it,i,nbin)+bin(i)                                  !/*op        102*/
        hold(it,18,nbin)=hold(it,18,nbin)+bin(i)                                !/*op        103*/
      endif                                                                     !/*op        104*/
      if (lft) then                                                             !/*op        105*/
        hold(ntim1,i,nbin)=hold(ntim1,i,nbin)+bin(i)                            !/*op        106*/
        hold(ntim1,18,nbin)=hold(ntim1,18,nbin)+bin(i)                          !/*op        107*/
      endif                                                                     !/*op        108*/
      if (ib.le.0) go to 170                                                    !/*op        109*/
      if (itconv.ne.0) then                                                     !/*op        110*/
        do 160 it=1,ntim                                                        !/*op        111*/
        delta=bin(i)*ta(it)                                                     !/*op        112*/
        hold(it,i,ib)=hold(it,i,ib)+delta                                       !/*op        113*/
  160   hold(it,18,ib)=hold(it,18,ib)+delta                                     !/*op        114*/
      else                                                                      !/*op        115*/
        hold(it,i,ib)=hold(it,i,ib)+bin(i)                                      !/*op        116*/
        hold(it,18,ib)=hold(it,18,ib)+bin(i)                                    !/*op        117*/
      endif                                                                     !/*op        118*/
      if (lft) then                                                             !/*op        119*/
        hold(ntim1,i,ib)=hold(ntim1,i,ib)+bin(i)                                !/*op        120*/
        hold(ntim1,18,ib)=hold(ntim1,18,ib)+bin(i)                              !/*op        121*/
      endif                                                                     !/*op        122*/
  170 continue                                                                  !/*op        123*/
      bin(i)=dp0                                                                !/*op        124*/
  180 continue                                                                  !/*op        125*/
  190 continue                                                                  !/*op        126*/
! NCOL=1                                                                        !/*op        127*/
      call rednmt                                                               !/*op        128*/
      if (ncol.le.0) go to 200                                                  !/*op        129*/
      if (nocas.eq.nocas0) go to 10                                             !/*op        130*/
  200 continue                                                                  !/*op        131*/
#endif                                                                          /*!op        132*/
      return                                                                    !/*op        133*/
      end                                                                       !/*op        134*/

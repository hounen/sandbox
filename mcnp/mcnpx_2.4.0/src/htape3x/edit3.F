!     $Id: edit3.F,v 1.4 2002/03/27 18:13:57 gwm Exp $                        !/*ed          1*/
      subroutine edit3 (tlly,tlsq,nerg1,ndim2)                                  !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/kpt.h>                                                        /*!ed          7*/
      dimension tlly(nerg1,ndim2,1), tlsq(nerg1,ndim2,1)                        !/*ed          8*/
      do 50 k=1,nbin                                                            !/*ed          9*/
      write (io,110) icase,iopt,titl1,titl2                                     !/*ed         10*/
!                                                                               !/*ed         11*/
!     KOPT:                                                                     !/*ed         12*/
!     0 all                                                                     !/*ed         13*/
!     1 p,n cascade                                                             !/*ed         14*/
!     2 evaporation,preequillibrium                                             !/*ed         15*/
!     3 totals only                                                             !/*ed         16*/
!     4 prefission evaporation                                                  !/*ed         17*/
!     5 postfission evaporation                                                 !/*ed         18*/
!                                                                               !/*ed         19*/
      if (lbunch) then                                                          !/*ed         20*/
        if (jmat.ne.0) write (io,120) (lparm(i),i=1,nparm)                      !/*ed         21*/
        if (jmat.eq.0) write (io,130) (lparm(i),i=1,nparm)                      !/*ed         22*/
      elseif (k.lt.nbin) then                                                   !/*ed         23*/
        if (jmat.ne.0) write (io,140) lparm(k)                                  !/*ed         24*/
        if (jmat.eq.0) write (io,150) lparm(k)                                  !/*ed         25*/
      else                                                                      !/*ed         26*/
        write (io,160)                                                          !/*ed         27*/
      endif                                                                     !/*ed         28*/
      if (lsnprm) write (io,170) dnparm(k)                                      !/*ed         29*/
      if (kopt.eq.3) go to 20                                                   !/*ed         30*/
      if (kopt.gt.1) go to 10                                                   !/*ed         31*/
!  LPT+1 to LPT+5                                                               !/*ed         32*/
! p_casc    n_casc   pi+_casc  pi0_casc  pi-_casc                               !/*ed         33*/
      nlo=lpt+1                                                                 !/*ed         34*/
      nhi=lpt+5                                                                 !/*ed         35*/
      call hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                          !/*ed         36*/
      if (kopt.eq.1) go to 50                                                   !/*ed         37*/
!  LPT+6 to LPT+10                                                              !/*ed         38*/
! pi+_decay  pi0_decay  pi-_decay    ph_decay   ph_de-ex                        !/*ed         39*/
      nlo=lpt+6                                                                 !/*ed         40*/
      nhi=lpt+10                                                                !/*ed         41*/
      call hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                          !/*ed         42*/
!  LPT+11 to LPT+16                                                             !/*ed         43*/
! p_preq    n_preq    d_preq    t_preq    He3_pre   a_preq                      !/*ed         44*/
      nlo=lpt+11                                                                !/*ed         45*/
      nhi=lpt+16                                                                !/*ed         46*/
      call hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                          !/*ed         47*/
   10 continue                                                                  !/*ed         48*/
      if (kopt.eq.4) write (io,180)                                             !/*ed         49*/
      if (kopt.eq.5) write (io,190)                                             !/*ed         50*/
!  LPT+17 to LPT+22                                                             !/*ed         51*/
! p_evap    n_evap    d_evap    t_evap    He3_eva   a_evap                      !/*ed         52*/
      nlo=lpt+17                                                                !/*ed         53*/
      nhi=lpt+22                                                                !/*ed         54*/
      call hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                          !/*ed         55*/
      if (kopt.ne.0) go to 50                                                   !/*ed         56*/
   20 continue                                                                  !/*ed         57*/
!  1 to LPT                                  particles from all sources         !/*ed         58*/
      write (io,200)                                                            !/*ed         59*/
      nlo=1                                                                     !/*ed         60*/
   30 continue                                                                  !/*ed         61*/
      nhi=min(nlo+5,lpt)                                                        !/*ed         62*/
      test=dp1                                                                  !/*ed         63*/
      if (test.gt.dp0) then                                                     !/*ed         64*/
        call hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                        !/*ed         65*/
      endif                                                                     !/*ed         66*/
      nlo=nlo+6                                                                 !/*ed         67*/
      if (nlo.le.lpt) go to 30                                                  !/*ed         68*/
   50 continue                                                                  !/*ed         69*/
#ifdef HTPLOT                                                                   /*!ed         70*/
      if (kplot.lt.1) go to 100                                                 !/*ed         71*/
      nerg=nerg1-1                                                              !/*ed         72*/
      if (nerg.le.1) go to 100                                                  !/*ed         73*/
      nup=2*nerg+2                                                              !/*ed         74*/
      xx(1)=0.3*ergb(1)                                                         !/*ed         75*/
      xx(2)=xx(1)                                                               !/*ed         76*/
      do 60 i=1,nerg                                                            !/*ed         77*/
      j=2*i+1                                                                   !/*ed         78*/
      xx(j)=ergb(i)                                                             !/*ed         79*/
   60 xx(j+1)=ergb(i)                                                           !/*ed         80*/
      do 90 k=1,nbin                                                            !/*ed         81*/
      if (k.lt.nbin) write (bin,'(i3)') lparm(k)                                !/*ed         82*/
      if (kopt.eq.3) go to 80                                                   !/*ed         83*/
      if (kopt.gt.1) go to 70                                                   !/*ed         84*/
!  LPT+1 to LPT+5                                                               !/*ed         85*/
! p_casc    n_casc   pi+_casc  pi0_casc  pi-_casc                               !/*ed         86*/
      nlo=lpt+1                                                                 !/*ed         87*/
      nhi=lpt+5                                                                 !/*ed         88*/
      call plt3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2,bin)                      !/*ed         89*/
      if (kopt.eq.1) go to 90                                                   !/*ed         90*/
!  LPT+6 to LPT+10                                                              !/*ed         91*/
! pi+_decay  pi0_decay  pi-_decay    ph_decay   ph_de-ex                        !/*ed         92*/
      nlo=lpt+6                                                                 !/*ed         93*/
      nhi=lpt+10                                                                !/*ed         94*/
      call plt3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2,bin)                      !/*ed         95*/
!  LPT+11 to LPT+16                                                             !/*ed         96*/
! p_preq    n_preq    d_preq    t_preq    He3_pre   a_preq                      !/*ed         97*/
      nlo=lpt+11                                                                !/*ed         98*/
      nhi=lpt+16                                                                !/*ed         99*/
      call plt3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2,bin)                      !/*ed        100*/
   70 continue                                                                  !/*ed        101*/
!  LPT+17 to LPT+22                                                             !/*ed        102*/
! p_evap    n_evap    d_evap    t_evap    He3_eva   a_evap                      !/*ed        103*/
      nlo=lpt+17                                                                !/*ed        104*/
      nhi=lpt+22                                                                !/*ed        105*/
      call plt3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2,bin)                      !/*ed        106*/
! END EVAP                                                                      !/*ed        107*/
      if (kopt.ne.0) go to 90                                                   !/*ed        108*/
   80 continue                                                                  !/*ed        109*/
!  1 to LPT                                  particles from all sources         !/*ed        110*/
      call pl3tot (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2,bin)                    !/*ed        111*/
   90 continue                                                                  !/*ed        112*/
  100 continue                                                                  !/*ed        113*/
#endif                                                                          /*!ed        114*/
      return                                                                    !/*ed        115*/
!                                                                               !/*ed        116*/
  110 format ('1',20x,'case no.',i3,5x,'option no.',i3//(10x,10a8))             !/*ed        117*/
  120 format (/10x,'particle production in materials',15i4/(41x,15i4))          !/*ed        118*/
  130 format (/10x,'particle production in cells',15i4/(37x,15i4))              !/*ed        119*/
  140 format (/35x,' particle production in material',i4)                       !/*ed        120*/
  150 format (/35x,' particle production in cell',i4)                           !/*ed        121*/
  160 format (/35x,'particle production in all cells')                          !/*ed        122*/
  170 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        123*/
  180 format (/40x,'pre-fission')                                               !/*ed        124*/
  190 format (/40x,'post-fission')                                              !/*ed        125*/
  200 format (/35x,'particles from all sources')                                !/*ed        126*/
      end                                                                       !/*ed        127*/

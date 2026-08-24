!     $Id: qou17.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qo          1*/
      subroutine qou17 (t,b,r,w,g)                                              !/*qo          2*/
! QOU17      calls...  QRDET  SQRT  FLTRNF  QENE  QNGID  QLPHA  QOLL            !/*qo          3*/
!     QOU17  called by...   ISOB                                                !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
#include <lcs/isobar.h>                                                         /*!qo          6*/
      dimension t(117), b(101), r(117), w(234), g(234)                          !/*qo          7*/
      if (i3_) 10,10,150                                                        !/*qo          8*/
   10 pt_(38)=dp0                                                               !/*qo          9*/
!     1-alpha  part.6+1                                                         !/*qo         10*/
      valu1_=rlke_-1.8d+02                                                      !/*qo         11*/
      call qrdet (1,t(1),valu1_)                                                !/*qo         12*/
      com2_=crdt_(1)                                                            !/*qo         13*/
      ftr_=dncms_*rlke_*dp2*rcpmv_+2.9877156d+27                                !/*qo         14*/
!     e**2=min**2+ncnms*rlke_*2*rcpmv_                                          !/*qo         15*/
      univr_=sqrt(ftr_)                                                         !/*qo         16*/
!     e                                                                         !/*qo         17*/
   20 continue                                                                  !/*qo         18*/
      valu2_=rang()                                                             !/*qo         19*/
      com_=valu2_*com2_                                                         !/*qo         20*/
!     r-prime                                                                   !/*qo         21*/
      call qene (b(1))                                                          !/*qo         22*/
      com1_=(com_*com_+ftr_-.501264d+26)/(dp2*univr_)                           !/*qo         23*/
!     m1r prime)**2+e**2-2(pnms_)/2e=e alpha                                    !/*qo         24*/
      a_=com1_*com1_-com_*com_                                                  !/*qo         25*/
      if (a_) 30,40,40                                                          !/*qo         26*/
   30 pacnt_=pacnt_+dp1                                                         !/*qo         27*/
      go to 20                                                                  !/*qo         28*/
   40 unive_=(univr_-com1_)*com1_*(sqrt(a_)/univr_)                             !/*qo         29*/
!     ((e beta*e alpha*p alpha)/e)=f(m,tr)                                      !/*qo         30*/
      call qrdet (1,r(1),valu1_)                                                !/*qo         31*/
!     (pi-nuc)fmax(rlke_)isobar sampling s.p.                                   !/*qo         32*/
      com1_=rang()                                                              !/*qo         33*/
      if ((unive_/crdt_(1))-com1_) 20,50,50                                     !/*qo         34*/
!     random no_. less or equal than f(m,tr)/fmax(tr)                           !/*qo         35*/
   50 call qngid                                                                !/*qo         36*/
      pm_(4)=poms_                                                              !/*qo         37*/
      pm_(3)=com_                                                               !/*qo         38*/
      pt_(2)=dp3                                                                !/*qo         39*/
      pt_(4)=poms_                                                              !/*qo         40*/
      pt_(14)=dp3                                                               !/*qo         41*/
      pt_(16)=poms_                                                             !/*qo         42*/
      pt_(26)=dp1                                                               !/*qo         43*/
      pt_(28)=dncms_                                                            !/*qo         44*/
      if (isw_(9)) 70,60,70                                                     !/*qo         45*/
   60 if (isw_(10)) 110,100,110                                                 !/*qo         46*/
   70 if (isw_(10)) 120,80,120                                                  !/*qo         47*/
   80 i3_=-1                                                                    !/*qo         48*/
   90 return                                                                    !/*qo         49*/
  100 valu1_=.4d0                                                               !/*qo         50*/
      valu2_=.66666667d0                                                        !/*qo         51*/
      valu3_=dp0                                                                !/*qo         52*/
      go to 140                                                                 !/*qo         53*/
  110 call qrdet (2,w(1),valu1_)                                                !/*qo         54*/
!     (pich-p)fract. fin.sta.with recl. pi1 pi0 l.e.                            !/*qo         55*/
      valu3_=.33333333d0                                                        !/*qo         56*/
      go to 130                                                                 !/*qo         57*/
  120 call qrdet (2,g(1),valu1_)                                                !/*qo         58*/
      valu3_=strkp_                                                             !/*qo         59*/
!     (pin-p)fract.fin.sta.with recl.pi1 pio l.e.                               !/*qo         60*/
  130 valu1_=crdt_(1)                                                           !/*qo         61*/
      valu2_=crdt_(2)                                                           !/*qo         62*/
  140 call qlpha                                                                !/*qo         63*/
  150 pt_(3)=dp0                                                                !/*qo         64*/
      pt_(15)=dp0                                                               !/*qo         65*/
      pt_(27)=dp0                                                               !/*qo         66*/
      pt_(39)=dp0                                                               !/*qo         67*/
      call qoll                                                                 !/*qo         68*/
      if (col_(15)) 80,160,80                                                   !/*qo         69*/
  160 if (pt_(38)) 170,180,170                                                  !/*qo         70*/
  170 i3_=0                                                                     !/*qo         71*/
      go to 90                                                                  !/*qo         72*/
  180 pt_(39)=dp0                                                               !/*qo         73*/
      if (ik_) 190,200,190                                                      !/*qo         74*/
  190 pt_(3)=((e_(4)-pm_(4))/rcpmv_)+pt_(3)                                     !/*qo         75*/
  200 i3_=1                                                                     !/*qo         76*/
      go to 90                                                                  !/*qo         77*/
      end                                                                       !/*qo         78*/

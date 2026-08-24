!     $Id: qou21.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                        !/*qo          1*/
      subroutine qou21 (v,w,x,y,z)                                              !/*qo          2*/
! QOU21      calls...  SQRT  QRDET  FLTRNF  QENE  QNGID  QLP19  QLP28           !/*qo          3*/
!     QOU21  called by...   ISOB                                                !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
      dimension v(161), w(101), x(161), y(130), z(176)                          !/*qo          6*/
#include <lcs/isobar.h>                                                         /*!qo          7*/
      data pecnt /dp0/                                                          !/*qo          8*/
      valu2_=rlke_*4.81633308d+24+9.0554256d+27                                 !/*qo          9*/
!     e(tr)**2=rlke_*rcpmv_*2*ncms+4*ncms**2                                    !/*qo         10*/
      valu3_=sqrt(valu2_)                                                       !/*qo         11*/
      go to (10,100,110,240), i3_                                               !/*qo         12*/
   10 isw_(12)=0                                                                !/*qo         13*/
   20 pt_(38)=dp0                                                               !/*qo         14*/
      i1_=0                                                                     !/*qo         15*/
      ans_=rlke_                                                                !/*qo         16*/
   30 valu1_=ans_-3.d+02                                                        !/*qo         17*/
      call qrdet (1,v(1),valu1_)                                                !/*qo         18*/
!     (nuc-nuc) f(tr) isobar sampling                                           !/*qo         19*/
      ftr_=crdt_(1)                                                             !/*qo         20*/
   40 continue                                                                  !/*qo         21*/
      sn_=rang()                                                                !/*qo         22*/
      com_=sn_*ftr_                                                             !/*qo         23*/
!     r prime=f(tr)*random                                                      !/*qo         24*/
      call qene (w(1))                                                          !/*qo         25*/
!     (nuc-nuc)mass of isobar s.p.    m(r prime)                                !/*qo         26*/
      if (i1_) 130,50,140                                                       !/*qo         27*/
   50 com1_=(com_*com_-sqnm_+valu2_)/(dp2*valu3_)                               !/*qo         28*/
!     e gamma                                                                   !/*qo         29*/
      a_=com1_*com1_-com_*com_                                                  !/*qo         30*/
      if (a_) 60,70,70                                                          !/*qo         31*/
   60 pgcnt_=pgcnt_+dp1                                                         !/*qo         32*/
      go to 40                                                                  !/*qo         33*/
   70 univr_=sqrt(a_)*com1_*(dp1-com1_/valu3_)                                  !/*qo         34*/
!     f(m,tr)=p gamma*e gamma*e delta/e                                         !/*qo         35*/
      call qrdet (1,x(1),valu1_)                                                !/*qo         36*/
!     (nuc-nuc)fmax(tr) isobar sampling s.p.                                    !/*qo         37*/
      com1_=rang()                                                              !/*qo         38*/
      if (com1_-(univr_/crdt_(1))) 80,80,40                                     !/*qo         39*/
   80 pm_(4)=dncms_                                                             !/*qo         40*/
      pm_(3)=com_                                                               !/*qo         41*/
      call qngid                                                                !/*qo         42*/
      pt_(4)=dncms_                                                             !/*qo         43*/
      pt_(28)=dncms_                                                            !/*qo         44*/
      call qlp19                                                                !/*qo         45*/
   90 return                                                                    !/*qo         46*/
  100 isw_(12)=2                                                                !/*qo         47*/
      go to 20                                                                  !/*qo         48*/
  110 isw_(13)=0                                                                !/*qo         49*/
  120 i1_=-1                                                                    !/*qo         50*/
      ans_=((valu3_-.708d+13)**2-9.0554256d+27)/4.81633308d+24                  !/*qo         51*/
      go to 30                                                                  !/*qo         52*/
!     tr prime     com1_=rlke_ prime                                            !/*qo         53*/
  130 com1_=((valu3_+dncms_-com_)**2-9.0554256d+27)/4.81633308d+24              !/*qo         54*/
      com2_=com_                                                                !/*qo         55*/
      ans_=com1_                                                                !/*qo         56*/
      com4=ftr_                                                                 !/*qo         57*/
      i1_=1                                                                     !/*qo         58*/
      go to 30                                                                  !/*qo         59*/
  140 com1_=(com2_*com2_-com_*com_+valu2_)/(dp2*valu3_)                         !/*qo         60*/
!     e epsilon                                                                 !/*qo         61*/
      a_=com1_*com1_-com2_*com2_                                                !/*qo         62*/
      if (a_) 150,160,160                                                       !/*qo         63*/
  150 pecnt=pecnt+dp1                                                           !/*qo         64*/
      go to 170                                                                 !/*qo         65*/
!     f(m1,m2,tr)=p epsilon*e epsilon*e zeta/e                                  !/*qo         66*/
  160 univr_=sqrt(a_)*com1_*(dp1-com1_/valu3_)                                  !/*qo         67*/
      valu1_=rlke_-9.2d+02                                                      !/*qo         68*/
      call qrdet (1,y(1),valu1_)                                                !/*qo         69*/
!     (nuc-nuc)fmax(tr) isobar sampling d.p.  fmax(m1,m2,tr)                    !/*qo         70*/
      sn_=rang()                                                                !/*qo         71*/
      if (sn_-(univr_*ftr_/(crdt_(1)*com4))) 180,180,170                        !/*qo         72*/
  170 ftr_=com4                                                                 !/*qo         73*/
      i1_=-1                                                                    !/*qo         74*/
      go to 40                                                                  !/*qo         75*/
  180 continue                                                                  !/*qo         76*/
      valu1_=rang()                                                             !/*qo         77*/
      if (valu1_-dph) 190,190,200                                               !/*qo         78*/
  190 pm_(3)=com2_                                                              !/*qo         79*/
      pm_(4)=com_                                                               !/*qo         80*/
      go to 210                                                                 !/*qo         81*/
  200 pm_(3)=com_                                                               !/*qo         82*/
      pm_(4)=com2_                                                              !/*qo         83*/
  210 call qngid                                                                !/*qo         84*/
      pt_(16)=dncms_                                                            !/*qo         85*/
      pt_(40)=dncms_                                                            !/*qo         86*/
      if (isw_(13)) 220,230,220                                                 !/*qo         87*/
  220 call qrdet (1,z(1),rlke_)                                                 !/*qo         88*/
      valu1_=crdt_(1)                                                           !/*qo         89*/
!     (n-p)fract.fin.sta.3/2 l.e.                                               !/*qo         90*/
  230 pt_(2)=dp3                                                                !/*qo         91*/
      pt_(4)=poms_                                                              !/*qo         92*/
      pt_(14)=dp1                                                               !/*qo         93*/
      pt_(26)=dp3                                                               !/*qo         94*/
      pt_(28)=poms_                                                             !/*qo         95*/
      pt_(38)=dp1                                                               !/*qo         96*/
      call qlp28                                                                !/*qo         97*/
      go to 90                                                                  !/*qo         98*/
  240 isw_(13)=dp2                                                              !/*qo         99*/
      go to 120                                                                 !/*qo        100*/
      end                                                                       !/*qo        101*/

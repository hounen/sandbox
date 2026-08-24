!     $Id: rou21.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou21 (v,w,x,y,z)                                              !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension v(161), w(101), x(161), y(130), z(176)                          !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      value2=rlke*4.81633308d+24+9.0554256d+27                                  !/*ro          6*/
!     e(tr)**2=rlke*rcpmv*2*ncms+4*ncms**2                                      !/*ro          7*/
      value3=sqrt(value2)                                                       !/*ro          8*/
      go to (10,100,110,240), i3                                                !/*ro          9*/
   10 isw(12)=0                                                                 !/*ro         10*/
   20 pt(38)=dp0                                                                !/*ro         11*/
      i1=0                                                                      !/*ro         12*/
      ans=rlke                                                                  !/*ro         13*/
   30 value1=ans-3.d+02                                                         !/*ro         14*/
      call crdet (1,v(1),value1)                                                !/*ro         15*/
!     (nuc-nuc) f(tr) isobar sampling                                           !/*ro         16*/
      ftr=crdt(1)                                                               !/*ro         17*/
   40 continue                                                                  !/*ro         18*/
      sn=rang()                                                                 !/*ro         19*/
      com=sn*ftr                                                                !/*ro         20*/
!     r prime=f(tr)*random                                                      !/*ro         21*/
      call gene (w(1))                                                          !/*ro         22*/
!     (nuc-nuc)mass of isobar s.p.    m(r prime)                                !/*ro         23*/
      if (i1) 130,50,140                                                        !/*ro         24*/
   50 com1=(com*com-sqnm+value2)/(dp2*value3)                                   !/*ro         25*/
!     e gamma                                                                   !/*ro         26*/
      a=com1*com1-com*com                                                       !/*ro         27*/
      if (a) 60,70,70                                                           !/*ro         28*/
   60 pgcnt=pgcnt+dp1                                                           !/*ro         29*/
      go to 40                                                                  !/*ro         30*/
   70 univer=sqrt(a)*com1*(dp1-com1/value3)                                     !/*ro         31*/
!     f(m,tr)=p gamma*e gamma*e delta/e                                         !/*ro         32*/
      call crdet (1,x(1),value1)                                                !/*ro         33*/
!     (nuc-nuc)fmax(tr) isobar sampling s.p.                                    !/*ro         34*/
      com1=rang()                                                               !/*ro         35*/
      if (com1-(univer/crdt(1))) 80,80,40                                       !/*ro         36*/
   80 pm(4)=dncms                                                               !/*ro         37*/
      pm(3)=com                                                                 !/*ro         38*/
      call angid                                                                !/*ro         39*/
      pt(4)=dncms                                                               !/*ro         40*/
      pt(28)=dncms                                                              !/*ro         41*/
      call alp19                                                                !/*ro         42*/
   90 return                                                                    !/*ro         43*/
  100 isw(12)=2                                                                 !/*ro         44*/
      go to 20                                                                  !/*ro         45*/
  110 isw(13)=0                                                                 !/*ro         46*/
  120 i1=-1                                                                     !/*ro         47*/
      ans=((value3-pnms)**2-9.0554256d+27)/4.81633308d+24                       !/*ro         48*/
      go to 30                                                                  !/*ro         49*/
!     tr prime     com1=rlke prime                                              !/*ro         50*/
  130 com1=((value3+dncms-com)**2-9.0554256d+27)/4.81633308d+24                 !/*ro         51*/
      com2=com                                                                  !/*ro         52*/
      ans=com1                                                                  !/*ro         53*/
      com4=ftr                                                                  !/*ro         54*/
      i1=1                                                                      !/*ro         55*/
      go to 30                                                                  !/*ro         56*/
  140 com1=(com2*com2-com*com+value2)/(dp2*value3)                              !/*ro         57*/
!     e epsilon                                                                 !/*ro         58*/
      a=com1*com1-com2*com2                                                     !/*ro         59*/
      if (a) 150,160,160                                                        !/*ro         60*/
  150 pecnt=pecnt+dp1                                                           !/*ro         61*/
      go to 170                                                                 !/*ro         62*/
!     f(m1,m2,tr)=p epsilon*e epsilon*e zeta/e                                  !/*ro         63*/
  160 univer=sqrt(a)*com1*(dp1-com1/value3)                                     !/*ro         64*/
      value1=rlke-9.2d+02                                                       !/*ro         65*/
      call crdet (1,y(1),value1)                                                !/*ro         66*/
!     (nuc-nuc)fmax(tr) isobar sampling d.p.  fmax(m1,m2,tr)                    !/*ro         67*/
      sn=rang()                                                                 !/*ro         68*/
      if (sn-(univer*ftr/(crdt(1)*com4))) 180,180,170                           !/*ro         69*/
  170 ftr=com4                                                                  !/*ro         70*/
      i1=-1                                                                     !/*ro         71*/
      go to 40                                                                  !/*ro         72*/
  180 continue                                                                  !/*ro         73*/
      value1=rang()                                                             !/*ro         74*/
      if (value1-dph) 190,190,200                                               !/*ro         75*/
  190 pm(3)=com2                                                                !/*ro         76*/
      pm(4)=com                                                                 !/*ro         77*/
      go to 210                                                                 !/*ro         78*/
  200 pm(3)=com                                                                 !/*ro         79*/
      pm(4)=com2                                                                !/*ro         80*/
  210 call angid                                                                !/*ro         81*/
      pt(16)=dncms                                                              !/*ro         82*/
      pt(40)=dncms                                                              !/*ro         83*/
      if (isw(13)) 220,230,220                                                  !/*ro         84*/
  220 call crdet (1,z(1),rlke)                                                  !/*ro         85*/
      value1=crdt(1)                                                            !/*ro         86*/
!     (n-p)fract.fin.sta.3/2 l.e.                                               !/*ro         87*/
  230 pt(2)=dp3                                                                 !/*ro         88*/
      pt(4)=poms                                                                !/*ro         89*/
      pt(14)=dp1                                                                !/*ro         90*/
      pt(26)=dp3                                                                !/*ro         91*/
      pt(28)=poms                                                               !/*ro         92*/
      pt(38)=dp1                                                                !/*ro         93*/
      call alp28                                                                !/*ro         94*/
      go to 90                                                                  !/*ro         95*/
  240 isw(13)=2                                                                 !/*ro         96*/
      go to 120                                                                 !/*ro         97*/
      end                                                                       !/*ro         98*/

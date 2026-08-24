!     $Id: rou17.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou17 (t,b,r,w,g)                                              !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(117), b(101), r(117), w(234), g(234)                          !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      if (i3) 10,10,150                                                         !/*ro          6*/
   10 pt(38)=dp0                                                                !/*ro          7*/
!     1-alpha  part.6+1                                                         !/*ro          8*/
      value1=rlke-1.8d+02                                                       !/*ro          9*/
      call crdet (1,t(1),value1)                                                !/*ro         10*/
      com2=crdt(1)                                                              !/*ro         11*/
      ftr=dncms*rlke*dp2*rcpmv+2.9877156d+27                                    !/*ro         12*/
!     e**2=min**2+ncnms*rlke*2*rcpmv                                            !/*ro         13*/
      univer=sqrt(ftr)                                                          !/*ro         14*/
!     e                                                                         !/*ro         15*/
   20 continue                                                                  !/*ro         16*/
      value2=rang()                                                             !/*ro         17*/
      com=value2*com2                                                           !/*ro         18*/
!     r-prime                                                                   !/*ro         19*/
      call gene (b(1))                                                          !/*ro         20*/
      com1=(com*com+ftr-.501264d+26)/(dp2*univer)                               !/*ro         21*/
!     m1r prime)**2+e**2-2(pnms)/2e=e alpha                                     !/*ro         22*/
      a=com1*com1-com*com                                                       !/*ro         23*/
      if (a) 30,40,40                                                           !/*ro         24*/
   30 pacnt=pacnt+dp1                                                           !/*ro         25*/
      go to 20                                                                  !/*ro         26*/
   40 unive=((univer-com1)*com1/univer)*sqrt(a)                                 !/*ro         27*/
!     ((e beta*e alpha*p alpha)/e)=f(m,tr)                                      !/*ro         28*/
      call crdet (1,r(1),value1)                                                !/*ro         29*/
!     (pi-nuc)fmax(rlke)isobar sampling s.p.                                    !/*ro         30*/
      com1=rang()                                                               !/*ro         31*/
      if ((unive/crdt(1))-com1) 20,50,50                                        !/*ro         32*/
!     random no. less or equal than f(m,tr)/fmax(tr)                            !/*ro         33*/
   50 call angid                                                                !/*ro         34*/
      pm(3)=com                                                                 !/*ro         35*/
      pm(4)=poms                                                                !/*ro         36*/
      pt(2)=dp3                                                                 !/*ro         37*/
      pt(4)=poms                                                                !/*ro         38*/
      pt(14)=dp3                                                                !/*ro         39*/
      pt(16)=poms                                                               !/*ro         40*/
      pt(26)=dp1                                                                !/*ro         41*/
      pt(28)=dncms                                                              !/*ro         42*/
      if (isw(9)) 70,60,70                                                      !/*ro         43*/
   60 if (isw(10)) 110,100,110                                                  !/*ro         44*/
   70 if (isw(10)) 120,80,120                                                   !/*ro         45*/
   80 i3=-1                                                                     !/*ro         46*/
   90 return                                                                    !/*ro         47*/
  100 value1=.4d0                                                               !/*ro         48*/
      value2=6.6666667d-1                                                       !/*ro         49*/
      value3=dp0                                                                !/*ro         50*/
      go to 140                                                                 !/*ro         51*/
  110 call crdet (2,w(1),value1)                                                !/*ro         52*/
!     (pich-p)fract. fin.sta.with recl. pi1 pi0 l.e.                            !/*ro         53*/
      value3=3.3333333d-1                                                       !/*ro         54*/
      go to 130                                                                 !/*ro         55*/
  120 call crdet (2,g(1),value1)                                                !/*ro         56*/
      value3=strkp                                                              !/*ro         57*/
!     (pin-p)fract.fin.sta.with recl.pi1 pio l.e.                               !/*ro         58*/
  130 value1=crdt(1)                                                            !/*ro         59*/
      value2=crdt(2)                                                            !/*ro         60*/
  140 call alpha                                                                !/*ro         61*/
  150 call ecpl                                                                 !/*ro         62*/
      if (i1) 160,160,80                                                        !/*ro         63*/
  160 call coll (-1)                                                            !/*ro         64*/
      if (col(15)) 80,170,80                                                    !/*ro         65*/
  170 if (pt(38)) 180,190,180                                                   !/*ro         66*/
  180 i3=0                                                                      !/*ro         67*/
      go to 90                                                                  !/*ro         68*/
  190 pt(39)=dp0                                                                !/*ro         69*/
      pt(3)=((e(4)-pm(4))/rcpmv)+pt(3)                                          !/*ro         70*/
      i3=1                                                                      !/*ro         71*/
      go to 90                                                                  !/*ro         72*/
      end                                                                       !/*ro         73*/

!     $Id: stor.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                         !/*st          1*/
      subroutine stor (p0,i,e,a,b,g,w,j,erem,ps,wmass,itype)                    !/*st          2*/
! STOR       calls...  ALOG  SQRT  FLTRNF  PROT  PI                             !/*st          3*/
!     STOR   called by...   NUCNUC  PINUC                                       !/*st          4*/
#include <lcs/double.h>                                                         /*!st          5*/
      dimension icoun(20,50,10), pslowr(20,50)                                  !/*st          6*/
      dimension delps(20), hp0(20)                                              !/*st          7*/
      data istart /0/                                                           !/*st          8*/
      if (istart.lt.1) go to 30                                                 !/*st          9*/
!     calculate jp0, index of incident momentum bin                             !/*st         10*/
   10 plet=log(1.d+03/p0)                                                       !/*st         11*/
      jp0=plet/delp0                                                            !/*st         12*/
      if (p0.gt.1.d+03) go to 20                                                !/*st         13*/
      if (jp0.lt.n2) jp0=jp0+1                                                  !/*st         14*/
      incp=i                                                                    !/*st         15*/
      if (itype.gt.2) incp=i+5                                                  !/*st         16*/
      go to (60,140,150,160), j                                                 !/*st         17*/
   20 if (j.gt.1) return                                                        !/*st         18*/
      w=dp0                                                                     !/*st         19*/
      return                                                                    !/*st         20*/
!     initialization                                                            !/*st         21*/
   30 n2=20                                                                     !/*st         22*/
!     if n2 and n5 are to be increased, be sure to change dimension s           !/*st         23*/
      n5=50                                                                     !/*st         24*/
      cn2=n2                                                                    !/*st         25*/
      cn5=n5                                                                    !/*st         26*/
      delp0=log(1.d+03/2.5d0)/cn2                                               !/*st         27*/
      cp0=exp(-delp0)                                                           !/*st         28*/
      pp0=1.d+03/cp0                                                            !/*st         29*/
      do 50 k=1,n2                                                              !/*st         30*/
!     pp0 = highest p0 of bin                                                   !/*st         31*/
      pp0=pp0*cp0                                                               !/*st         32*/
      delps(k)=pp0/cn5                                                          !/*st         33*/
      hp0(k)=pp0                                                                !/*st         34*/
      do 40 m=1,n5                                                              !/*st         35*/
      cm=m                                                                      !/*st         36*/
      pslowr(k,m)=pp0-delps(k)*cm                                               !/*st         37*/
      if (pslowr(k,m).lt.dp0) pslowr(k,m)=dp0                                   !/*st         38*/
      do 40 ink=1,10                                                            !/*st         39*/
      icoun(k,m,ink)=0                                                          !/*st         40*/
   40 continue                                                                  !/*st         41*/
   50 continue                                                                  !/*st         42*/
      istart=10                                                                 !/*st         43*/
      amp=0.94d0                                                                !/*st         44*/
      ampi=0.139d0                                                              !/*st         45*/
      tamp=amp+amp                                                              !/*st         46*/
      tampi=ampi+ampi                                                           !/*st         47*/
      amp2=amp*amp                                                              !/*st         48*/
      ampi2=ampi*ampi                                                           !/*st         49*/
      go to 10                                                                  !/*st         50*/
   60 if (i.gt.2) go to 70                                                      !/*st         51*/
      if (itype.gt.2) go to 70                                                  !/*st         52*/
      perem=sqrt(erem*(erem+tamp))                                              !/*st         53*/
      go to 80                                                                  !/*st         54*/
   70 perem=sqrt(erem*(erem+tampi))                                             !/*st         55*/
!     search primary table for momentum less than perem                         !/*st         56*/
!     compute high ps index for starting search, pslowr is descending           !/*st         57*/
   80 khi=(hp0(jp0)-perem)/delps(jp0)                                           !/*st         58*/
      if (khi.lt.n5) khi=khi+1                                                  !/*st         59*/
      do 90 k=khi,n5                                                            !/*st         60*/
      if (icoun(jp0,k,incp).gt.0) go to 100                                     !/*st         61*/
   90 continue                                                                  !/*st         62*/
      w=dp0                                                                     !/*st         63*/
      return                                                                    !/*st         64*/
  100 w=dp1                                                                     !/*st         65*/
      icoun(jp0,k,incp)=icoun(jp0,k,incp)-1                                     !/*st         66*/
      rannum=rang()                                                             !/*st         67*/
      ps=pslowr(jp0,k)+delps(jp0)*rannum                                        !/*st         68*/
      if (ps.lt.perem) go to 110                                                !/*st         69*/
      rannum=rang()                                                             !/*st         70*/
      ps=pslowr(jp0,k)+delps(jp0)*rannum                                        !/*st         71*/
      if (ps.lt.perem) go to 110                                                !/*st         72*/
      rannum=rang()                                                             !/*st         73*/
      ps=pslowr(jp0,k)+delps(jp0)*rannum                                        !/*st         74*/
      if (ps.lt.perem) go to 110                                                !/*st         75*/
      ps=pslowr(jp0,k)                                                          !/*st         76*/
      if (ps.eq.dp0) ps=perem*rannum                                            !/*st         77*/
  110 if (i.gt.2) go to 120                                                     !/*st         78*/
      e=sqrt(ps*ps+amp2)-amp                                                    !/*st         79*/
      wmass=dp0                                                                 !/*st         80*/
      if (itype.gt.2) go to 130                                                 !/*st         81*/
      call prot (p0,e,ps,a,b,g)                                                 !/*st         82*/
      return                                                                    !/*st         83*/
  120 e=sqrt(ps*ps+ampi2)-ampi                                                  !/*st         84*/
      wmass=ampi                                                                !/*st         85*/
  130 call pi (p0,i,e,ps,a,b,g)                                                 !/*st         86*/
      return                                                                    !/*st         87*/
!     score primary                                                             !/*st         88*/
  140 kps=(hp0(jp0)-ps)/delps(jp0)                                              !/*st         89*/
      if (kps.lt.n5) kps=kps+1                                                  !/*st         90*/
      icoun(jp0,kps,incp)=icoun(jp0,kps,incp)+1                                 !/*st         91*/
      return                                                                    !/*st         92*/
!     score secondary                                                           !/*st         93*/
  150 kps=(hp0(jp0)-ps)/delps(jp0)                                              !/*st         94*/
      if (kps.lt.n5) kps=kps+1                                                  !/*st         95*/
      icoun(jp0,kps,incp)=icoun(jp0,kps,incp)-1                                 !/*st         96*/
      return                                                                    !/*st         97*/
!     search secondary table for momentum, ps                                   !/*st         98*/
  160 kps=(hp0(jp0)-ps)/delps(jp0)                                              !/*st         99*/
      if (kps.lt.n5) kps=kps+1                                                  !/*st        100*/
      if (icoun(jp0,kps,incp).lt.0) go to 170                                   !/*st        101*/
      return                                                                    !/*st        102*/
  170 icoun(jp0,kps,incp)=icoun(jp0,kps,incp)+1                                 !/*st        103*/
      i=0                                                                       !/*st        104*/
      return                                                                    !/*st        105*/
      end                                                                       !/*st        106*/

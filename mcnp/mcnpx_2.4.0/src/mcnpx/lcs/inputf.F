!     $Id: inputf.F,v 1.5 2002/03/27 18:15:08 gwm Exp $                       !/*in          1*/
      subroutine inputf (nbrtp)                                                 !/*in          2*/
#include <lcs/double.h>                                                         /*!in          3*/
#include <lcs/ercom.h>                                                          /*!in          4*/
#include <lcs/orcm.h>                                                           /*!in          5*/
#include <lcs/inout.h>                                                          /*!in          6*/
      dimension cfong(50)                                                       !/*in          7*/
      data cfong /2*65.d0,65.d0,86.d0,124.d0,170.d0,186.d0,190.d0,186.d0        !/*in          8*/
     & ,170.d0,140.d0,118.d0,100.d0,90.d0,80.d0,73.d0,66.d0,58.d0,52.d0,        !/*in          9*/
     & 46.d0,42.d0,39.d0,37.d0,37.d0,38.d0,43.d0,62.d0,104.d0,180.d0,222        !/*in         10*/
     & .d0,245.d0,250.d0,236.d0,190.d0,140.d0,106.d0,83.d0,73.d0,65.d0,6        !/*in         11*/
     & 0.d0,55.d0,52.d0,49.d0,47.d0,45.d0,43.d0,42.d0,3*42.d0/                  !/*in         12*/
      data ifoz /2/, ifong /0/, igraph /1/, ifinal /0/, iscale /1/,             !/*in         13*/
     & iloren /1/, iwt /0/, enrmsa /dph/, eta /0.4d0/, y /dp0/, smult /1        !/*in         14*/
     & 5.d0/, ifa /0/, idoub /0/                                                !/*in         15*/
      read (nbrtp) ainfo                                                        !/*in         16*/
      do 10 j=1,20                                                              !/*in         17*/
      write (io,70) ainfo(j),j                                                  !/*in         18*/
   10 continue                                                                  !/*in         19*/
      itrigg=ainfo(16)                                                          !/*in         20*/
      if (itrigg.ne.0) iversn=itrigg                                            !/*in         21*/
      afnp=ainfo(1)                                                             !/*in         22*/
      zfnp=ainfo(2)                                                             !/*in         23*/
      read (nbrtp) epkin,sigke                                                  !/*in         24*/
      read (nbrtp) delexc                                                       !/*in         25*/
!ccc                                                                            !/*in         26*/
!     the following excitation energies are the computed excitations for        !/*in         27*/
!     epperson incident proton energies on u235,92                              !/*in         28*/
!     ef1 = ainfo(10)                                                           !/*in         29*/
!     ef2 = ainfo(11)                                                           !/*in         30*/
!     ef3 = ainfo(12)                                                           !/*in         31*/
!     ef4 = ainfo(13)                                                           !/*in         32*/
!     ef5 = ainfo(14)                                                           !/*in         33*/
!     ef6 = ainfo(15)                                                           !/*in         34*/
!ccc                                                                            !/*in         35*/
      bz0=bzero                                                                 !/*in         36*/
      yz0=yzero                                                                 !/*in         37*/
      if (ismal.gt.2) go to 30                                                  !/*in         38*/
      bo1=dp1/ainfo(19)                                                         !/*in         39*/
      bo2=dp1/ainfo(17)                                                         !/*in         40*/
      bc=dp1/bz0                                                                !/*in         41*/
      bfactr=(bc-bo1)/(bo2-bo1)                                                 !/*in         42*/
      do 20 j=1,80                                                              !/*in         43*/
      do 20 ke=1,6                                                              !/*in         44*/
      delex1=delexc(j,ke,1)                                                     !/*in         45*/
      deldel=delexc(j,ke,2)-delex1                                              !/*in         46*/
      delexc(j,ke,ismal)=delex1+bfactr*deldel                                   !/*in         47*/
   20 continue                                                                  !/*in         48*/
   30 continue                                                                  !/*in         49*/
      if (iversn.eq.2) fm=.125d0                                                !/*in         50*/
      if (iversn.eq.1) fm=1.25d0                                                !/*in         51*/
      write (io,80) iversn,ifa,itrigg,ifoz,ismal,fm                             !/*in         52*/
!ccc                                                                            !/*in         53*/
!     vary these initial values to determine qamax                              !/*in         54*/
!ccc                                                                            !/*in         55*/
      a2m=133.d0                                                                !/*in         56*/
!ccc                                                                            !/*in         57*/
!     iamin is the minimum a-value for a fission fragment                       !/*in         58*/
!ccc                                                                            !/*in         59*/
      izdel=7                                                                   !/*in         60*/
      iamin=70                                                                  !/*in         61*/
!ccc                                                                            !/*in         62*/
!     acalm is the total number of tries for a given fission history            !/*in         63*/
!ccc                                                                            !/*in         64*/
      acalm=4.d2                                                                !/*in         65*/
      if=0                                                                      !/*in         66*/
      ib=1                                                                      !/*in         67*/
      do 40 ih=69,165,2                                                         !/*in         68*/
      if=if+1                                                                   !/*in         69*/
      iah1=ih-58                                                                !/*in         70*/
      cstiff(iah1)=cfong(if)                                                    !/*in         71*/
      cstiff(iah1+1)=cfong(if)+dph*(cfong(if+1)-cfong(if))                      !/*in         72*/
   40 continue                                                                  !/*in         73*/
      do 50 ih=59,68                                                            !/*in         74*/
      iah1=ih-58                                                                !/*in         75*/
      cstiff(iah1)=cfong(ib)                                                    !/*in         76*/
   50 continue                                                                  !/*in         77*/
      do 60 ih=167,178                                                          !/*in         78*/
      iah1=ih-58                                                                !/*in         79*/
      cstiff(iah1)=cfong(if)                                                    !/*in         80*/
   60 continue                                                                  !/*in         81*/
      return                                                                    !/*in         82*/
!                                                                               !/*in         83*/
   70 format (1h ,'ainfo(j) =',1pe15.6,2x,'j=',i10)                             !/*in         84*/
   80 format (1h ,t23,'iversn=',i5/1x,t23,'ifa=',i5/1x,t23,'itrigg=',i5/        !/*in         85*/
     & 1x,t23,'ifoz=',i5/1x,t23,'ismal=',i5/1x,t23,'fm=',f7.3)                  !/*in         86*/
      end                                                                       !/*in         87*/

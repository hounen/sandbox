!     $Id: edit8b.F,v 1.4 2002/03/27 18:13:58 gwm Exp $                       !/*ed          1*/
      subroutine edit8b (fcas,f,s)                                              !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/big.h>                                                        /*!ed          6*/
#include <htape3x/big4.h>                                                       /*!ed          7*/
      dimension f(len2,3), s(len2,3)                                            !/*ed          8*/
      write (io,510)                                                            !/*ed          9*/
      do 320 jz=1,104                                                           !/*ed         10*/
      k1=iref(jz)                                                               !/*ed         11*/
      k2=iref(jz+1)-1                                                           !/*ed         12*/
      if (k2.ge.k1) then                                                        !/*ed         13*/
        do 310 k=k1,k2                                                          !/*ed         14*/
        if (wtfin0(k).le.dp0) go to 310                                         !/*ed         15*/
        ip1=ipt(k)                                                              !/*ed         16*/
        ip2=ipt(k+1)-1                                                          !/*ed         17*/
        if (ip2.ge.ip1) then                                                    !/*ed         18*/
          do 300 ip=ip1,ip2                                                     !/*ed         19*/
          if (wtfin(ip).le.dp0) go to 300                                       !/*ed         20*/
          ia=mod(izao(k),1000)                                                  !/*ed         21*/
          iz=izao(k)/1000                                                       !/*ed         22*/
          if (lcl(ip).gt.0.and.lcl(ip).le.2) then                               !/*ed         23*/
            ff=f(k,lcl(ip)+1)                                                   !/*ed         24*/
            ss=s(k,lcl(ip)+1)                                                   !/*ed         25*/
          else                                                                  !/*ed         26*/
            ff=wtfin(ip)/wtfin0(k)                                              !/*ed         27*/
            ss=fcas*(dp1-ff)/((fcas-dp1)*wtfin0(k))                             !/*ed         28*/
          endif                                                                 !/*ed         29*/
          write (io,520) iz,ia,lcl(ip),elo(ip),tlev(ip),ff,sqrt(ss)             !/*ed         30*/
  300     continue                                                              !/*ed         31*/
        endif                                                                   !/*ed         32*/
  310   continue                                                                !/*ed         33*/
      endif                                                                     !/*ed         34*/
  320 continue                                                                  !/*ed         35*/
      return                                                                    !/*ed         36*/
!                                                                               !/*ed         37*/
  510 format ('1',10x,'metastable state production as fraction of ',            !/*ed         38*/
     & 'isotopic production'/10x,'(average over cell or materials ',            !/*ed         39*/
     & 'in list; l = CINDER level)'//'    z    a    l    e lev',8x,             !/*ed         40*/
     & 't-half',8x,'fraction')                                                  !/*ed         41*/
  520 format (3i5,f10.5,1p2d14.5,0pf7.4)                                        !/*ed         42*/
      end                                                                       !/*ed         43*/

!     $Id: edit8a.F,v 1.4 2002/03/27 18:13:58 gwm Exp $                       !/*ed          1*/
      subroutine edit8a (fcas,f,s)                                              !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/big.h>                                                        /*!ed          4*/
#include <htape3x/big4.h>                                                       /*!ed          5*/
      dimension f(len2,3), s(len2,3)                                            !/*ed          6*/
      do 320 jz=1,104                                                           !/*ed          7*/
      k1=iref(jz)                                                               !/*ed          8*/
      k2=iref(jz+1)-1                                                           !/*ed          9*/
      if (k2.ge.k1) then                                                        !/*ed         10*/
        do 310 k=k1,k2                                                          !/*ed         11*/
        f(k,1)=dp1                                                              !/*ed         12*/
        f(k,2)=dp0                                                              !/*ed         13*/
        f(k,3)=dp0                                                              !/*ed         14*/
        s(k,1)=dp0                                                              !/*ed         15*/
        s(k,2)=dp0                                                              !/*ed         16*/
        s(k,3)=dp0                                                              !/*ed         17*/
        if (wtfin0(k).le.dp0) go to 310                                         !/*ed         18*/
        ip1=ipt(k)                                                              !/*ed         19*/
        ip2=ipt(k+1)-1                                                          !/*ed         20*/
        if (ip2.ge.ip1) then                                                    !/*ed         21*/
          do 300 ip=ip1,ip2                                                     !/*ed         22*/
          if (wtfin(ip).le.dp0) go to 300                                       !/*ed         23*/
          ff=wtfin(ip)/wtfin0(k)                                                !/*ed         24*/
          ss=fcas*(dp1-ff)/((fcas-dp1)*wtfin0(k))                               !/*ed         25*/
          if (lcl(ip).gt.0.and.lcl(ip).le.2) then                               !/*ed         26*/
            f(k,lcl(ip)+1)=ff                                                   !/*ed         27*/
            f(k,1)=f(k,1)-ff                                                    !/*ed         28*/
            s(k,lcl(ip)+1)=ss                                                   !/*ed         29*/
            s(k,1)=s(k,1)+fcas*ff/((fcas-dp1)*wtfin0(k))                        !/*ed         30*/
          endif                                                                 !/*ed         31*/
  300     continue                                                              !/*ed         32*/
        endif                                                                   !/*ed         33*/
  310   continue                                                                !/*ed         34*/
      endif                                                                     !/*ed         35*/
  320 continue                                                                  !/*ed         36*/
      return                                                                    !/*ed         37*/
!                                                                               !/*ed         38*/
      end                                                                       !/*ed         39*/

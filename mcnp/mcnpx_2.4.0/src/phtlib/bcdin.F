!     $Id: bcdin.F,v 1.4 2002/03/27 18:16:21 gwm Exp $                        !/*bc          1*/
      subroutine bcdin (k,l)                                                    !/*bc          2*/
#include <htape3x/com32b.h>                                                     /*!bc          3*/
#include <htape3x/big.h>                                                        /*!bc          4*/
#include <trx/gross.h>                                                          /*!bc          5*/
      i=1                                                                       !/*bc          6*/
      ipt(1)=1                                                                  !/*bc          7*/
      idk(1)=1                                                                  !/*bc          8*/
   10 continue                                                                  !/*bc          9*/
      read (20,110) izao(i),npt                                                 !/*bc         10*/
      if (izao(i)/1000.ge.200) go to 30                                         !/*bc         11*/
      ipt(i+1)=ipt(i)+npt                                                       !/*bc         12*/
      i=i+1                                                                     !/*bc         13*/
      if (npt.eq.0) go to 10                                                    !/*bc         14*/
      njo=ipt(i-1)                                                              !/*bc         15*/
      nup=ipt(i)-1                                                              !/*bc         16*/
      do 20 j=njo,nup                                                           !/*bc         17*/
      read (20,120) elo(j),isp(j),lcl(j),ndk,tlev(j)                            !/*bc         18*/
      if (ndk.gt.0) then                                                        !/*bc         19*/
        idk(j+1)=idk(j)+ndk                                                     !/*bc         20*/
        llo=idk(j)                                                              !/*bc         21*/
        lhi=idk(j+1)-1                                                          !/*bc         22*/
        read (20,100) (pmode(l),lfin(l),l=llo,lhi)                              !/*bc         23*/
      elseif (j.eq.njo.or.tlev(j).lt.0.001d0) then                              !/*bc         24*/
        idk(j+1)=idk(j)                                                         !/*bc         25*/
      else                                                                      !/*bc         26*/
        idk(j+1)=idk(j)+1                                                       !/*bc         27*/
        llo=idk(j)                                                              !/*bc         28*/
        lfin(llo)=0                                                             !/*bc         29*/
        if (tlev(j).ge.1.d0) then                                               !/*bc         30*/
          pmode(llo)=59.d0                                                      !/*bc         31*/
        else                                                                    !/*bc         32*/
          pmode(llo)=39.d0                                                      !/*bc         33*/
        endif                                                                   !/*bc         34*/
      endif                                                                     !/*bc         35*/
   20 continue                                                                  !/*bc         36*/
      go to 10                                                                  !/*bc         37*/
   30 continue                                                                  !/*bc         38*/
      k=i                                                                       !/*bc         39*/
      l=ipt(k)                                                                  !/*bc         40*/
   40 continue                                                                  !/*bc         41*/
      nup=izao(1)/1000                                                          !/*bc         42*/
      do 50 i=1,nup                                                             !/*bc         43*/
   50 iref(i)=1                                                                 !/*bc         44*/
      nup=k-1                                                                   !/*bc         45*/
      do 70 ii=2,nup                                                            !/*bc         46*/
      izm1=izao(ii-1)/1000                                                      !/*bc         47*/
      iz=izao(ii)/1000                                                          !/*bc         48*/
      if (izm1.eq.iz) go to 70                                                  !/*bc         49*/
      do 60 iii=izm1+1,iz                                                       !/*bc         50*/
   60 iref(iii)=ii                                                              !/*bc         51*/
   70 continue                                                                  !/*bc         52*/
      nlo=iz+1                                                                  !/*bc         53*/
      do 80 i=nlo,len0                                                          !/*bc         54*/
   80 iref(i)=k                                                                 !/*bc         55*/
      return                                                                    !/*bc         56*/
!                                                                               !/*bc         57*/
  100 format (5(f12.8,i4))                                                      !/*bc         58*/
  110 format (i8,i7)                                                            !/*bc         59*/
  120 format (1pd12.5,i5,i2,i3,1pd12.5)                                         !/*bc         60*/
      end                                                                       !/*bc         61*/

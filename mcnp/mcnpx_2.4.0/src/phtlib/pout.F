!     $Id: pout.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                         !/*po          1*/
      subroutine pout                                                           !/*po          2*/
#include <htape3x/com32b.h>                                                     /*!po          3*/
#include <trx/ioput.h>                                                          /*!po          4*/
#include <htape3x/big.h>                                                        /*!po          5*/
#include <trx/gross.h>                                                          /*!po          6*/
      dimension mode(100), p(100), ll(100), izadk(100)                          !/*po          7*/
      do 40 i=1,len0-1                                                          !/*po          8*/
      iref1=iref(i)                                                             !/*po          9*/
      iref2=iref(i+1)-1                                                         !/*po         10*/
      if (iref2.lt.iref1) go to 40                                              !/*po         11*/
      do 30 l=iref1,iref2                                                       !/*po         12*/
      iza=izao(l)                                                               !/*po         13*/
      ipt1=ipt(l)                                                               !/*po         14*/
      ipt2=ipt(l+1)-1                                                           !/*po         15*/
      nlev=ipt2-ipt1+1                                                          !/*po         16*/
      if (nlev.le.0) go to 30                                                   !/*po         17*/
      iz=iza/1000                                                               !/*po         18*/
      ia=mod(iza,1000)                                                          !/*po         19*/
      write (nout,60) iz,ia,nlev                                                !/*po         20*/
      do 20 jj=1,nlev                                                           !/*po         21*/
      j=ipt1+jj-1                                                               !/*po         22*/
      idk0=idk(j)                                                               !/*po         23*/
      ndk=idk(j+1)-idk0                                                         !/*po         24*/
      if (ndk.lt.0) then                                                        !/*po         25*/
        write (nout,80) iza,elo(j),idk(j),idk(j+1)                              !/*po         26*/
        write (nout2,80) iza,elo(j),idk(j),idk(j+1)                             !/*po         27*/
        stop                                                                    !/*po         28*/
      endif                                                                     !/*po         29*/
#ifdef LCS                                                                      /*!po         30*/
      if (jj.eq.1.or.lcl(j).gt.0.or.tlev(j).ge.1.0d-04) then                    !/*po         31*/
        write (*,50) iz,ia,lcl(j),jj,elo(j),tlev(j),ndk                         !/*po         32*/
      endif                                                                     !/*po         33*/
#endif                                                                          /*!po         34*/
      if (lcl(j).gt.0) then                                                     !/*po         35*/
        write (nout,100) jj,elo(j),isp(j),tlev(j),lcl(j)                        !/*po         36*/
      else                                                                      !/*po         37*/
        write (nout,70) jj,elo(j),isp(j),tlev(j)                                !/*po         38*/
      endif                                                                     !/*po         39*/
      if (ndk.eq.0) go to 20                                                    !/*po         40*/
      do 10 k=1,ndk                                                             !/*po         41*/
      ii=idk0-1+k                                                               !/*po         42*/
      mode(k)=int(pmode(ii))                                                    !/*po         43*/
      p(k)=mod(pmode(ii),dp1)                                                   !/*po         44*/
      if (p(k).eq.dp0.and.ndk.eq.1) p(k)=1.0                                    !/*po         45*/
      ll(k)=lfin(ii)+1                                                          !/*po         46*/
      izadk(k)=izaf(iza,mod(mode(k),20))                                        !/*po         47*/
   10 continue                                                                  !/*po         48*/
      write (nout,90) (izadk(k),ll(k),mode(k),p(k),k=1,ndk)                     !/*po         49*/
   20 continue                                                                  !/*po         50*/
   30 continue                                                                  !/*po         51*/
   40 continue                                                                  !/*po         52*/
      return                                                                    !/*po         53*/
!                                                                               !/*po         54*/
   50 format (4i4,f9.5,1pe14.5,i4)                                              !/*po         55*/
   60 format (' Z = ',i3,' A = ',i3,' N-lev = ',i3)                             !/*po         56*/
   70 format (5x,'lev =',i3,' E-lev =',f8.4,' Spin =',i3,' T-half =',           !/*po         57*/
     & 1pe12.4)                                                                 !/*po         58*/
   80 format (' pout',i10,f14.7,2i10)                                           !/*po         59*/
   90 format (10x,'Final ',i6,' Lev',i3,' DKMode',i3,' Prob',f11.6)             !/*po         60*/
  100 format (5x,'lev =',i3,' E-lev =',f8.4,' Spin =',i3,' T-half =',           !/*po         61*/
     & 1pe12.4,' C-lev =',i2)                                                   !/*po         62*/
      end                                                                       !/*po         63*/

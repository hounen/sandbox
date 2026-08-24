!     $Id: list.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                         !/*li          1*/
      subroutine list (iz,ia)                                                   !/*li          2*/
#include <htape3x/com32b.h>                                                     /*!li          3*/
#include <trx/ioput.h>                                                          /*!li          4*/
#include <htape3x/big.h>                                                        /*!li          5*/
#include <trx/gross.h>                                                          /*!li          6*/
      dimension mode(100), p(100), ll(100), izadk(100)                          !/*li          7*/
      write (nout,80)                                                           !/*li          8*/
      iza=1000*iz+ia                                                            !/*li          9*/
      iref1=iref(iz)                                                            !/*li         10*/
      iref2=iref(iz+1)-1                                                        !/*li         11*/
      do 10 i=iref1,iref2                                                       !/*li         12*/
      if (iza.eq.izao(i)) go to 20                                              !/*li         13*/
   10 continue                                                                  !/*li         14*/
      write (nout,140) iz,ia                                                    !/*li         15*/
      write (nout2,140) iz,ia                                                   !/*li         16*/
      return                                                                    !/*li         17*/
   20 continue                                                                  !/*li         18*/
      iza=izao(i)                                                               !/*li         19*/
      ipt1=ipt(i)                                                               !/*li         20*/
      ipt2=ipt(i+1)-1                                                           !/*li         21*/
      nlev=ipt2-ipt1+1                                                          !/*li         22*/
      if (nlev.le.0) go to 70                                                   !/*li         23*/
      iz=iza/1000                                                               !/*li         24*/
      ia=mod(iza,1000)                                                          !/*li         25*/
      write (nout,90) iz,ia,nlev                                                !/*li         26*/
      do 60 jj=1,nlev                                                           !/*li         27*/
      j=ipt1+jj-1                                                               !/*li         28*/
      idk0=idk(j)                                                               !/*li         29*/
      ndk=idk(j+1)-idk0                                                         !/*li         30*/
      if (lcl(j).gt.0) then                                                     !/*li         31*/
        write (nout,110) jj,elo(j),isp(j),tlev(j),lcl(j)                        !/*li         32*/
      else                                                                      !/*li         33*/
        write (nout,100) jj,elo(j),isp(j),tlev(j)                               !/*li         34*/
      endif                                                                     !/*li         35*/
      if (ndk) 30,60,40                                                         !/*li         36*/
   30 continue                                                                  !/*li         37*/
      write (nout,120) iza,elo(j),idk(j),idk(j+1)                               !/*li         38*/
      write (nout2,120) iza,elo(j),idk(j),idk(j+1)                              !/*li         39*/
      stop                                                                      !/*li         40*/
   40 continue                                                                  !/*li         41*/
      do 50 k=1,ndk                                                             !/*li         42*/
      ii=idk0-1+k                                                               !/*li         43*/
      mode(k)=int(pmode(ii))                                                    !/*li         44*/
      p(k)=mod(pmode(ii),dp1)                                                   !/*li         45*/
      if (p(k).eq.dp0.and.ndk.eq.1) p(k)=1.0                                    !/*li         46*/
      ll(k)=lfin(ii)+1                                                          !/*li         47*/
      izadk(k)=izaf(iza,mod(mode(k),20))                                        !/*li         48*/
   50 continue                                                                  !/*li         49*/
      write (nout,130) (izadk(k),ll(k),mode(k),p(k),k=1,ndk)                    !/*li         50*/
   60 continue                                                                  !/*li         51*/
   70 continue                                                                  !/*li         52*/
      return                                                                    !/*li         53*/
!                                                                               !/*li         54*/
   80 format (1h1)                                                              !/*li         55*/
   90 format (' Z = ',i3,' A = ',i3,' N-lev = ',i3)                             !/*li         56*/
  100 format (5x,'lev =',i3,' E-lev =',f8.4,' Spin =',i3,' T-half =',           !/*li         57*/
     & 1pe12.4)                                                                 !/*li         58*/
  110 format (5x,'lev =',i3,' E-lev =',f8.4,' Spin =',i3,' T-half =',           !/*li         59*/
     & 1pe12.4,' C-lev =',i2)                                                   !/*li         60*/
  120 format (' list',i10,f14.7,2i10)                                           !/*li         61*/
  130 format (10x,'Final ',i6,' Lev',i3,' DKMode',i3,' Prob',f11.6)             !/*li         62*/
  140 format (' Z = ',i3,' A = ',i3,' not found')                               !/*li         63*/
      end                                                                       !/*li         64*/

!     $Id: fill.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                         !/*fi          1*/
      subroutine fill                                                           !/*fi          2*/
#ifdef LCS                                                                      /*!fi          3*/
#include <htape3x/com32b.h>                                                     /*!fi          4*/
#include <trx/ioput.h>                                                          /*!fi          5*/
#include <htape3x/big.h>                                                        /*!fi          6*/
#include <trx/gross.h>                                                          /*!fi          7*/
#include <trx/gross3.h>                                                         /*!fi          8*/
      do 120 i=1,104                                                            !/*fi          9*/
      iref1=iref(i)                                                             !/*fi         10*/
      iref2=iref(i+1)-1                                                         !/*fi         11*/
      if (iref2.lt.iref1) go to 120                                             !/*fi         12*/
      do 110 l=iref1,iref2                                                      !/*fi         13*/
      iza=izao(l)                                                               !/*fi         14*/
      ipt1=ipt(l)                                                               !/*fi         15*/
      ipt2=ipt(l+1)-1                                                           !/*fi         16*/
      if (ipt2.lt.ipt1) go to 110                                               !/*fi         17*/
      if (elo(ipt1).eq.dp0) go to 10                                            !/*fi         18*/
      write (nout,130) iza,elo(ipt1)                                            !/*fi         19*/
      write (nout2,130) iza,elo(ipt1)                                           !/*fi         20*/
   10 continue                                                                  !/*fi         21*/
      do 100 j=ipt1,ipt2                                                        !/*fi         22*/
      idk0=idk(j)                                                               !/*fi         23*/
      ndk=idk(j+1)-idk0                                                         !/*fi         24*/
      if (ndk) 20,100,30                                                        !/*fi         25*/
   20 continue                                                                  !/*fi         26*/
      write (nout,140) iza,elo(j),idk(j),idk(j+1)                               !/*fi         27*/
      write (nout2,140) iza,elo(j),idk(j),idk(j+1)                              !/*fi         28*/
      stop                                                                      !/*fi         29*/
   30 continue                                                                  !/*fi         30*/
      nup=idk0+ndk-1                                                            !/*fi         31*/
      do 90 k=idk0,nup                                                          !/*fi         32*/
      mode=int(pmode(k))                                                        !/*fi         33*/
      etest=efin(k)                                                             !/*fi         34*/
      jmode=mod(mode,20)                                                        !/*fi         35*/
      izadk=izaf(iza,jmode)                                                     !/*fi         36*/
      ii=izadk/1000                                                             !/*fi         37*/
      ir1=iref(ii)                                                              !/*fi         38*/
      ir2=iref(ii+1)-1                                                          !/*fi         39*/
      do 40 jj=ir1,ir2                                                          !/*fi         40*/
      if (izao(jj).eq.izadk) go to 50                                           !/*fi         41*/
   40 continue                                                                  !/*fi         42*/
      write (98,150) iza,izadk,mode                                             !/*fi         43*/
      go to 90                                                                  !/*fi         44*/
   50 continue                                                                  !/*fi         45*/
      ip1=ipt(jj)                                                               !/*fi         46*/
      ip2=ipt(jj+1)-1                                                           !/*fi         47*/
      if (ip2.ge.ip1) go to 60                                                  !/*fi         48*/
      write (98,160) izao(jj),ip1,ip2                                           !/*fi         49*/
      go to 90                                                                  !/*fi         50*/
   60 continue                                                                  !/*fi         51*/
      do 70 kk=ip1,ip2                                                          !/*fi         52*/
      if (abs(etest-elo(kk)).lt.1.0e-06) go to 80                               !/*fi         53*/
   70 continue                                                                  !/*fi         54*/
      write (98,170) iza,elo(j),k,efin(k),izadk                                 !/*fi         55*/
      go to 90                                                                  !/*fi         56*/
   80 continue                                                                  !/*fi         57*/
      lfin(k)=kk-ip1                                                            !/*fi         58*/
   90 continue                                                                  !/*fi         59*/
  100 continue                                                                  !/*fi         60*/
  110 continue                                                                  !/*fi         61*/
  120 continue                                                                  !/*fi         62*/
      return                                                                    !/*fi         63*/
!                                                                               !/*fi         64*/
  130 format ('0missing g.s.',i10,1pd14.4)                                      !/*fi         65*/
  140 format (i10,f14.7,2i10)                                                   !/*fi         66*/
  150 format (' error type 1:',3i12)                                            !/*fi         67*/
  160 format ('error 2:',3i12)                                                  !/*fi         68*/
  170 format ('error 3:',i8,f11.7,i8,f11.7,i8)                                  !/*fi         69*/
#endif                                                                          /*!fi         70*/
      end                                                                       !/*fi         71*/

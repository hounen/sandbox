!     $Id: llib.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                         !/*ll          1*/
      subroutine llib (k,l)                                                     !/*ll          2*/
#ifdef LCS                                                                      /*!ll          3*/
#include <htape3x/com32b.h>                                                     /*!ll          4*/
#include <trx/ioput.h>                                                          /*!ll          5*/
#include <htape3x/work.h>                                                       /*!ll          6*/
      parameter (llev=150)                                                      !/*ll          7*/
      common /data/ elev(llev), par(llev), spin(llev), time(llev), idk0         !/*ll          8*/
     & (llev), iab(llev)                                                        !/*ll          9*/
#include <htape3x/big.h>                                                        /*!ll         10*/
#include <trx/gross.h>                                                          /*!ll         11*/
#include <trx/gross3.h>                                                         /*!ll         12*/
      iza0=0                                                                    !/*ll         13*/
      idkpt=1                                                                   !/*ll         14*/
      k=1                                                                       !/*ll         15*/
      l=0                                                                       !/*ll         16*/
      ipt(1)=1                                                                  !/*ll         17*/
   10 continue                                                                  !/*ll         18*/
      call get (iflag)                                                          !/*ll         19*/
      if (iza.le.1001) go to 10                                                 !/*ll         20*/
!     Branch for more of iza                                                    !/*ll         21*/
      if (iza.eq.iza0) go to 80                                                 !/*ll         22*/
!     Branch for initial record                                                 !/*ll         23*/
      if (iza0.eq.0) go to 70                                                   !/*ll         24*/
!     Bump data for this isotope and reinitialize                               !/*ll         25*/
      izao(k)=iab(1)                                                            !/*ll         26*/
      ipt(k+1)=ipt(k)+n                                                         !/*ll         27*/
      k=k+1                                                                     !/*ll         28*/
      do 60 i=1,n                                                               !/*ll         29*/
      l=l+1                                                                     !/*ll         30*/
      elo(l)=elev(i)                                                            !/*ll         31*/
      tlev(l)=time(i)                                                           !/*ll         32*/
      ic=int(2.0*spin(i)+1.001)                                                 !/*ll         33*/
      if (par(i)) 20,30,40                                                      !/*ll         34*/
   20 isp(l)=-ic                                                                !/*ll         35*/
      go to 50                                                                  !/*ll         36*/
   30 isp(l)=0                                                                  !/*ll         37*/
      go to 50                                                                  !/*ll         38*/
   40 isp(l)=ic                                                                 !/*ll         39*/
   50 continue                                                                  !/*ll         40*/
      idk(l)=idk0(i)                                                            !/*ll         41*/
   60 continue                                                                  !/*ll         42*/
   70 continue                                                                  !/*ll         43*/
      if (iflag.ne.0) go to 160                                                 !/*ll         44*/
!     Initialize for new iza                                                    !/*ll         45*/
      iza0=iza                                                                  !/*ll         46*/
      n=0                                                                       !/*ll         47*/
      go to 90                                                                  !/*ll         48*/
   80 continue                                                                  !/*ll         49*/
   90 continue                                                                  !/*ll         50*/
      n=n+1                                                                     !/*ll         51*/
      if (t.gt.0.001) go to 100                                                 !/*ll         52*/
      ptot=1.                                                                   !/*ll         53*/
  100 continue                                                                  !/*ll         54*/
      iab(n)=iza                                                                !/*ll         55*/
      elev(n)=el                                                                !/*ll         56*/
      par(n)=p                                                                  !/*ll         57*/
      spin(n)=s                                                                 !/*ll         58*/
      time(n)=t                                                                 !/*ll         59*/
      idk0(n)=idkpt                                                             !/*ll         60*/
      if (nd.eq.0) go to 150                                                    !/*ll         61*/
      if (nd.eq.1.and.idm(1).eq.99) go to 130                                   !/*ll         62*/
      if (nd.eq.1.and.idm(1).eq.19) go to 140                                   !/*ll         63*/
      do 120 i=1,nd                                                             !/*ll         64*/
      ii=idkpt-1+i                                                              !/*ll         65*/
      jj=idm(i)                                                                 !/*ll         66*/
      if (izaf(iza,jj).eq.jza(i)) go to 110                                     !/*ll         67*/
      if (jj.eq.19) go to 110                                                   !/*ll         68*/
      write (99,200) iza,jza(i),idm(i),el1(i)                                   !/*ll         69*/
  110 continue                                                                  !/*ll         70*/
      temp=pbl(i)                                                               !/*ll         71*/
      if (nd.eq.1) temp=dp0                                                     !/*ll         72*/
      pmode(ii)=real(jj)+temp                                                   !/*ll         73*/
      efin(ii)=el1(i)                                                           !/*ll         74*/
  120 continue                                                                  !/*ll         75*/
      go to 150                                                                 !/*ll         76*/
  130 continue                                                                  !/*ll         77*/
  140 continue                                                                  !/*ll         78*/
      nd=0                                                                      !/*ll         79*/
  150 continue                                                                  !/*ll         80*/
      idkpt=idkpt+nd                                                            !/*ll         81*/
      if (idkpt.le.len3) go to 10                                               !/*ll         82*/
      write (nout,210) iza,idkpt                                                !/*ll         83*/
      write (nout2,210) iza,idkpt                                               !/*ll         84*/
      call errorf ('llib.1  ')                                                  !/*ll         85*/
  160 continue                                                                  !/*ll         86*/
      nup=izao(1)/1000                                                          !/*ll         87*/
      do 170 i=1,nup                                                            !/*ll         88*/
  170 iref(i)=1                                                                 !/*ll         89*/
      nup=k-1                                                                   !/*ll         90*/
      do 180 ii=2,nup                                                           !/*ll         91*/
      izm1=izao(ii-1)/1000                                                      !/*ll         92*/
      iz=izao(ii)/1000                                                          !/*ll         93*/
      if (izm1.eq.iz) go to 180                                                 !/*ll         94*/
      iref(iz)=ii                                                               !/*ll         95*/
  180 continue                                                                  !/*ll         96*/
      nlo=iz+1                                                                  !/*ll         97*/
      do 190 i=nlo,len0                                                         !/*ll         98*/
  190 iref(i)=k                                                                 !/*ll         99*/
      l=l+1                                                                     !/*ll        100*/
      idk(l)=idkpt                                                              !/*ll        101*/
      izao(k)=900000                                                            !/*ll        102*/
      call fill                                                                 !/*ll        103*/
      return                                                                    !/*ll        104*/
!                                                                               !/*ll        105*/
  200 format (3i10,1pd14.4)                                                     !/*ll        106*/
  210 format (' storage exceeded',2i12)                                         !/*ll        107*/
#endif                                                                          /*!ll        108*/
      end                                                                       !/*ll        109*/

!     $Id: gthsig.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                       !/*gt          1*/
      subroutine gthsig (emax,ehin,ehipi,emn)                                   !/*gt          2*/
#include <lcs/double.h>                                                         /*!gt          3*/
#include <lcs/inout.h>                                                          /*!gt          4*/
#include <lcs/tpcrs.h>                                                          /*!gt          5*/
!* dexs is the const. energy spacing common t0 p-p,n-p,pi+p,pi-p xsects         !/*gt          6*/
      dimension emn(1)                                                          !/*gt          7*/
      dimension emx(4), isa(4)                                                  !/*gt          8*/
      dimension tmp(29849)                                                      !/*gt          9*/
      do 10 i=1,29849                                                           !/*gt         10*/
   10 tmp(i)=cs(i)                                                              !/*gt         11*/
      call shxd                                                                 !/*gt         12*/
      do 20 i=1,4                                                               !/*gt         13*/
   20 isa(i)=locx(3,i)                                                          !/*gt         14*/
!**** compute total p-p and n-p xsects (single production + double produ        !/*gt         15*/
!****  + elastic) and store p-p beginning at tmp(3794) anp n-p beginning        !/*gt         16*/
!****  tmp(3970).                                                               !/*gt         17*/
      isp=995                                                                   !/*gt         18*/
      idp=1153                                                                  !/*gt         19*/
      do 50 it=1,2                                                              !/*gt         20*/
      is=isa(it)                                                                !/*gt         21*/
      do 30 i=1,158                                                             !/*gt         22*/
      temp=tmp(is+18+i)                                                         !/*gt         23*/
   30 tmp(is+18+i)=temp+tmp(isp+i)                                              !/*gt         24*/
      isp=isp+288                                                               !/*gt         25*/
      do 40 i=1,130                                                             !/*gt         26*/
      temp=tmp(is+46+i)                                                         !/*gt         27*/
   40 tmp(is+46+i)=temp+tmp(idp+i)                                              !/*gt         28*/
   50 idp=idp+288                                                               !/*gt         29*/
!**** compute total pi+(sngl prod.+elas) and pi-(sngl prod.+exchng +elas        !/*gt         30*/
!**** and store pi+-p beginning at tmp(3668) and pi--p beginning at tmp(        !/*gt         31*/
      isp=2009                                                                  !/*gt         32*/
      do 70 it=3,4                                                              !/*gt         33*/
      is=isa(it)                                                                !/*gt         34*/
      do 60 i=1,117                                                             !/*gt         35*/
      temp=tmp(is+9+i)                                                          !/*gt         36*/
   60 tmp(is+9+i)=temp+tmp(isp+i)                                               !/*gt         37*/
   70 isp=isp+234                                                               !/*gt         38*/
      iex=3415                                                                  !/*gt         39*/
      is=isa(4)                                                                 !/*gt         40*/
      do 80 i=1,126                                                             !/*gt         41*/
      temp=tmp(is+i)                                                            !/*gt         42*/
   80 tmp(is+i)=temp+tmp(iex+i)                                                 !/*gt         43*/
      do 100 it=1,2                                                             !/*gt         44*/
      is=isa(it)                                                                !/*gt         45*/
      do 90 i=1,176                                                             !/*gt         46*/
   90 fnpsg(it,i)=tmp(is+i)                                                     !/*gt         47*/
  100 continue                                                                  !/*gt         48*/
      do 120 it=1,2                                                             !/*gt         49*/
      is=isa(it+2)                                                              !/*gt         50*/
      do 110 i=1,126                                                            !/*gt         51*/
  110 pipsg(it,i)=tmp(is+i)                                                     !/*gt         52*/
  120 continue                                                                  !/*gt         53*/
! *****                                                                         !/*gt         54*/
! **** select max. tot.xsects for x on prot. in energy range emn(x) to          !/*gt         55*/
! **** where x = prot.,neut.,pi+,pi-.                                           !/*gt         56*/
      if (emax.lt.ehin) go to 130                                               !/*gt         57*/
      emx(1)=ehin                                                               !/*gt         58*/
      emx(2)=ehin                                                               !/*gt         59*/
      emx(3)=ehipi                                                              !/*gt         60*/
      emx(4)=ehipi                                                              !/*gt         61*/
      go to 140                                                                 !/*gt         62*/
  130 continue                                                                  !/*gt         63*/
      emx(1)=emax                                                               !/*gt         64*/
      emx(2)=emax                                                               !/*gt         65*/
      emx(3)=2500.d0                                                            !/*gt         66*/
      emx(4)=2500.d0                                                            !/*gt         67*/
  140 continue                                                                  !/*gt         68*/
      do 160 itp=1,4                                                            !/*gt         69*/
      is=isa(itp)                                                               !/*gt         70*/
      it=itp+itp/4                                                              !/*gt         71*/
      if (it.eq.2) then                                                         !/*gt         72*/
         etemp=20.d0                                                            !/*gt         73*/
      else                                                                      !/*gt         74*/
         etemp=emn(it)                                                          !/*gt         75*/
      endif                                                                     !/*gt         76*/
      call sgm2 (itp,min(etemp,emx(itp)),il,el,sl)                              !/*gt         77*/
      call sgm2 (itp,emx(itp),ih,eh,sh)                                         !/*gt         78*/
      hsigmx(it)=max(sl,sh)                                                     !/*gt         79*/
      if (il.ge.ih) go to 160                                                   !/*gt         80*/
      il=il+1                                                                   !/*gt         81*/
      do 150 i=il,ih                                                            !/*gt         82*/
      sig=tmp(i+is)                                                             !/*gt         83*/
      if (sig.le.hsigmx(it)) go to 150                                          !/*gt         84*/
      hsigmx(it)=sig                                                            !/*gt         85*/
  150 continue                                                                  !/*gt         86*/
  160 continue                                                                  !/*gt         87*/
      hsigmx(4)=dp0                                                             !/*gt         88*/
#ifdef LCS_PRINT                                                                /*!gt         89*/
      write (io,170) (hsigmx(i),i=1,5)                                          !/*gt         90*/
  170 format (1h0,7hhsigmx ,5e15.5)                                             !/*gt         91*/
#endif                                                                          /*!gt         92*/
      return                                                                    !/*gt         93*/
      end                                                                       !/*gt         94*/

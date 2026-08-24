!     $Id: datahi.F,v 1.5 2002/03/27 18:14:59 gwm Exp $                       !/*da          1*/
      subroutine datahi (name,tc,wt,kk,kstor,mm,mtype,ito)                      !/*da          2*/
#include <lcs/double.h>                                                         /*!da          3*/
!     ==================================================================        !/*da          4*/
!     datahi   calls the following subroutines and functions                    !/*da          5*/
!     ==================================================================        !/*da          6*/
#include <lcs/lptS.h>                                                           /*!da          7*/
#include <lcs/concom.h>                                                         /*!da          8*/
#include <lcs/rucom.h>                                                          /*!da          9*/
#include <lcs/inout.h>                                                          /*!da         10*/
#include <lcs/banka.h>                                                          /*!da         11*/
#include <lcs/bankb.h>                                                          /*!da         12*/
      if (n1col.gt.0.and.name.gt.1) return                                      !/*da         13*/
      nabov=nabov+1                                                             !/*da         14*/
      if (nabov.gt.l02) then                                                    !/*da         15*/
        write (itty,90)                                                         !/*da         16*/
        write (io,90)                                                           !/*da         17*/
        nabov=nabov-1                                                           !/*da         18*/
        return                                                                  !/*da         19*/
      endif                                                                     !/*da         20*/
      wta(nabov)=wt                                                             !/*da         21*/
      tca(nabov)=tc                                                             !/*da         22*/
      go to (10,20,30,50,60,10), kstor                                          !/*da         23*/
!*****store intranuclear casade particles above emin                            !/*da         24*/
   10 continue                                                                  !/*da         25*/
      n=kk                                                                      !/*da         26*/
      tipa(nabov)=kind(n)                                                       !/*da         27*/
      ea(nabov)=ep(n)                                                           !/*da         28*/
      t1=costh*alpha(n)+sinth*gam(n)                                            !/*da         29*/
      ua(nabov)=cosphi*t1-sinphi*beta(n)                                        !/*da         30*/
      va(nabov)=sinphi*t1+cosphi*beta(n)                                        !/*da         31*/
      wa(nabov)=costh*gam(n)-sinth*alpha(n)                                     !/*da         32*/
!                                                                               !/*da         33*/
!     u = cosphi*costh* alpha  -sinphi* beta  +cosphi*sinth* gama               !/*da         34*/
!                                                                               !/*da         35*/
!     v = sinphi*costh* alpha  +cosphi* beta  +sinphi*sinth* gama               !/*da         36*/
!                                                                               !/*da         37*/
!     w =       -sinth* alpha  +   0. * beta         +costh* gama               !/*da         38*/
!                      rotation matrix                                          !/*da         39*/
!     if (kind(n).eq.13.or.kind(n).eq.14) wta(nabov)=dph*wta(nabov)             !/*da         40*/
      if (ihie.eq.1) wta(nabov)=wta(nabov)*wtfas(n)                             !/*da         41*/
      go to 80                                                                  !/*da         42*/
!*****store evap neuts above emin                                               !/*da         43*/
   20 tipa(nabov)=dp1                                                           !/*da         44*/
      j=1                                                                       !/*da         45*/
      k=kk                                                                      !/*da         46*/
      go to 40                                                                  !/*da         47*/
!*****store evap prots above emin                                               !/*da         48*/
   30 tipa(nabov)=dp0                                                           !/*da         49*/
      j=2                                                                       !/*da         50*/
      k=kk                                                                      !/*da         51*/
   40 continue                                                                  !/*da         52*/
      ea(nabov)=epart(k,j)                                                      !/*da         53*/
      ua(nabov)=cosevp(1,k,j)                                                   !/*da         54*/
      va(nabov)=cosevp(2,k,j)                                                   !/*da         55*/
      wa(nabov)=cosevp(3,k,j)                                                   !/*da         56*/
      go to 70                                                                  !/*da         57*/
!*****store heavy charged particles                                             !/*da         58*/
   50 continue                                                                  !/*da         59*/
      j=mtype                                                                   !/*da         60*/
      k=mm                                                                      !/*da         61*/
      tipa(nabov)=j+6                                                           !/*da         62*/
      ea(nabov)=hepart(k,j)                                                     !/*da         63*/
      ua(nabov)=coshev(1,k,j)                                                   !/*da         64*/
      va(nabov)=coshev(2,k,j)                                                   !/*da         65*/
      wa(nabov)=coshev(3,k,j)                                                   !/*da         66*/
      go to 70                                                                  !/*da         67*/
!*****store deexcitation photons                                                !/*da         68*/
   60 continue                                                                  !/*da         69*/
      tipa(nabov)=11.d0                                                         !/*da         70*/
      ea(nabov)=eph(kk)                                                         !/*da         71*/
      tca(nabov)=tca(nabov)+tph(kk)                                             !/*da         72*/
      call gtiso (ua(nabov),va(nabov),wa(nabov))                                !/*da         73*/
   70 continue                                                                  !/*da         74*/
      tipa(nabov)=tipa(nabov)+100.d0*(ito+4)                                    !/*da         75*/
   80 continue                                                                  !/*da         76*/
      if (kstor.eq.6) then                                                      !/*da         77*/
        namea(nabov)=name                                                       !/*da         78*/
      else                                                                      !/*da         79*/
        namea(nabov)=namax+nabov                                                !/*da         80*/
      endif                                                                     !/*da         81*/
      return                                                                    !/*da         82*/
!                                                                               !/*da         83*/
   90 format ('0nabov has exceeded l02 in subroutine datahi,the ',              !/*da         84*/
     & 'dimensions on ea,namea,tipa,ua,va,wa,and wta must be ',                 !/*da         85*/
     & 'increased.')                                                            !/*da         86*/
      end                                                                       !/*da         87*/

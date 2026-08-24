!     $Id: datalo.F,v 1.5 2002/03/27 18:14:59 gwm Exp $                       !/*da          1*/
      subroutine datalo (tc,wt,xc,yc,zc,kk,kstor,mm,mtype,ito,icell)            !/*da          2*/
#include <lcs/double.h>                                                         /*!da          3*/
!     ==================================================================        !/*da          4*/
!     datalo   calls the following subroutines and functions                    !/*da          5*/
!        SPAWN                                                                  !/*da          6*/
!     ==================================================================        !/*da          7*/
#include <lcs/lptS.h>                                                           /*!da          8*/
#include <lcs/rucom.h>                                                          /*!da          9*/
#include <lcs/banka.h>                                                          /*!da         10*/
#include <lcs/bankb.h>                                                          /*!da         11*/
#include <lcs/inout.h>                                                          /*!da         12*/
#include <lcs/concom.h>                                                         /*!da         13*/
      data dummy /dp0/                                                          !/*da         14*/
      nbelo=nbelo+1                                                             !/*da         15*/
      if (nbelo.gt.l02) then                                                    !/*da         16*/
        write (itty,100)                                                        !/*da         17*/
        write (io,100)                                                          !/*da         18*/
        nbelo=nbelo-1                                                           !/*da         19*/
        return                                                                  !/*da         20*/
      endif                                                                     !/*da         21*/
      wtb(nbelo)=wt                                                             !/*da         22*/
      tcb(nbelo)=tc                                                             !/*da         23*/
      go to (10,20,30,50,60), kstor                                             !/*da         24*/
!*****store intranuclear casade particles below emin                            !/*da         25*/
   10 continue                                                                  !/*da         26*/
      n=kk                                                                      !/*da         27*/
      tipb(nbelo)=kind(n)                                                       !/*da         28*/
      eb(nbelo)=ep(n)                                                           !/*da         29*/
      t1=costh*alpha(n)+sinth*gam(n)                                            !/*da         30*/
      ub(nbelo)=cosphi*t1-sinphi*beta(n)                                        !/*da         31*/
      vb(nbelo)=sinphi*t1+cosphi*beta(n)                                        !/*da         32*/
      wb(nbelo)=costh*gam(n)-sinth*alpha(n)                                     !/*da         33*/
!      see  subroutine datahi.                                                  !/*da         34*/
!     if (kind(n).eq.13.or.kind(n).eq.14) wtb(nbelo)=dph*wtb(nbelo)             !/*da         35*/
      if (ihie.eq.1) wtb(nbelo)=wtb(nbelo)*wtfas(n)                             !/*da         36*/
      itt=mod(kind(n),100)+1                                                    !/*da         37*/
      go to 80                                                                  !/*da         38*/
!*****store evap neuts below emin                                               !/*da         39*/
   20 tipb(nbelo)=dp1                                                           !/*da         40*/
      j=1                                                                       !/*da         41*/
      itt=2                                                                     !/*da         42*/
      k=kk                                                                      !/*da         43*/
      go to 40                                                                  !/*da         44*/
!*****store evap prots below emin                                               !/*da         45*/
   30 tipb(nbelo)=dp0                                                           !/*da         46*/
      j=2                                                                       !/*da         47*/
      itt=1                                                                     !/*da         48*/
      k=kk                                                                      !/*da         49*/
   40 continue                                                                  !/*da         50*/
      eb(nbelo)=epart(k,j)                                                      !/*da         51*/
      ub(nbelo)=cosevp(1,k,j)                                                   !/*da         52*/
      vb(nbelo)=cosevp(2,k,j)                                                   !/*da         53*/
      wb(nbelo)=cosevp(3,k,j)                                                   !/*da         54*/
      go to 70                                                                  !/*da         55*/
!*****store heavy charged particles                                             !/*da         56*/
   50 continue                                                                  !/*da         57*/
      j=mtype                                                                   !/*da         58*/
      k=mm                                                                      !/*da         59*/
      tipb(nbelo)=j+6                                                           !/*da         60*/
      eb(nbelo)=hepart(k,j)                                                     !/*da         61*/
      ub(nbelo)=coshev(1,k,j)                                                   !/*da         62*/
      vb(nbelo)=coshev(2,k,j)                                                   !/*da         63*/
      wb(nbelo)=coshev(3,k,j)                                                   !/*da         64*/
      itt=mtype+7                                                               !/*da         65*/
      go to 70                                                                  !/*da         66*/
!*****store deexcitation photons                                                !/*da         67*/
   60 continue                                                                  !/*da         68*/
      itt=12                                                                    !/*da         69*/
      tipb(nbelo)=11.d0                                                         !/*da         70*/
      eb(nbelo)=eph(kk)                                                         !/*da         71*/
      tcb(nbelo)=tcb(nbelo)+tph(kk)                                             !/*da         72*/
      call gtiso (ub(nbelo),vb(nbelo),wb(nbelo))                                !/*da         73*/
   70 continue                                                                  !/*da         74*/
      tipb(nbelo)=tipb(nbelo)+100.d0*(ito+4)                                    !/*da         75*/
   80 continue                                                                  !/*da         76*/
!L    ==================================================================        !/*da         77*/
!L      if (lneutp.le.0) go to 90                                               !/*da         78*/
!L      if (lneutp.ge.1.and.(itt.eq.2.or.itt.eq.12).or.lneutp.ge.2.and.         !/*da         79*/
!L     1 itt.eq.1.or.lneutp.ge.3.and.itt.eq.8) then                             !/*da         80*/
!L        ee=eb(nbelo)                                                          !/*da         81*/
!L        wtemp=wtb(nbelo)                                                      !/*da         82*/
!L        swtm0=swtm/fimx(1,icell)                                              !/*da         83*/
!L        if (wtemp.lt.swtm0) then                                              !/*da         84*/
!L          if (rang().gt.(wtemp/swtm0)) go to 90                               !/*da         85*/
!L          wtemp=swtm0                                                         !/*da         86*/
!L        endif                                                                 !/*da         87*/
!L        call wrneut (nocas,itt,ee,ub(nbelo),vb(nbelo),wb(nbelo),xc,           !/*da         88*/
!L     1   yc,zc,wtemp,tcb(nbelo))                                              !/*da         89*/
!L      endif                                                                   !/*da         90*/
!L   90 continue                                                                !/*da         91*/
!L    ==================================================================        !/*da         92*/
      return                                                                    !/*da         93*/
!                                                                               !/*da         94*/
  100 format ('0 nbelo has exceeded the dimensions of eb,tipb,ub,vbwb,',        !/*da         95*/
     & 'wtb in subroutine datalo')                                              !/*da         96*/
      end                                                                       !/*da         97*/

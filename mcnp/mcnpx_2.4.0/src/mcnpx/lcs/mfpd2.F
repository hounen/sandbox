!     $Id: mfpd2.F,v 1.5 2002/03/27 18:15:11 gwm Exp $                        !/*mf          1*/
      subroutine mfpd2 (ec,icell,tc,u,v,w,xc,yc,zc,wt)                          !/*mf          2*/
#include <lcs/double.h>                                                         /*!mf          3*/
!     ==================================================================        !/*mf          4*/
!     mfpd2    calls the following subroutines and functions                    !/*mf          5*/
!        AZIRN      EXITA      SQRT                                             !/*mf          6*/
!     ==================================================================        !/*mf          7*/
#include <lcs/lptS.h>                                                           /*!mf          8*/
#include <lcs/rucom.h>                                                          /*!mf          9*/
#include <lcs/concom.h>                                                         /*!mf         10*/
#include <lcs/inout.h>                                                          /*!mf         11*/
#include <lcs/bankb.h>                                                          /*!mf         12*/
      parameter (mxm=30)                                                        !/*mf         13*/
      dimension cxi(mxm), cyi(mxm), czi(mxm), tki(mxm), wei(mxm), kpi           !/*mf         14*/
     & (mxm)                                                                    !/*mf         15*/
      data icnt /0/                                                             !/*mf         16*/
      data nstart /0/                                                           !/*mf         17*/
      if (ec.lt.dp0) call errorf ('mfpd2.1')                                    !/*mf         18*/
      if (instab(ityp).eq.0) call errorf ('mfpd2.2')                            !/*mf         19*/
      if (wt.ne.dp0) call errorf ('mfpd2.3')                                    !/*mf         20*/
      weight=oldwt                                                              !/*mf         21*/
!M                                                                              !/*mf         22*/
      t1=dp1                                                                    !/*mf         23*/
!M                                                                              !/*mf         24*/
!L    if (ityp.le.7) then                                                       !/*mf         25*/
!L      t1=weight*fimx(2,icell)/wtmu                                            !/*mf         26*/
!L    else                                                                      !/*mf         27*/
!L      t1=weight*fimx(1,icell)/wtpi                                            !/*mf         28*/
!L    endif                                                                     !/*mf         29*/
      npb=int(t1)                                                               !/*mf         30*/
      if (rang().le.(t1-npb)) npb=npb+1                                         !/*mf         31*/
      if (npb.le.0) go to 40                                                    !/*mf         32*/
      wt1=weight/t1                                                             !/*mf         33*/
      do 20 j=1,npb                                                             !/*mf         34*/
      call fllink (ityp,ec,u,v,w,wt1,cxi,cyi,czi,tki,kpi,wei,np)                !/*mf         35*/
      do 20 i=1,np                                                              !/*mf         36*/
      it=kpi(i)                                                                 !/*mf         37*/
      if (it.le.0) go to 20                                                     !/*mf         38*/
      ntyp=it-1                                                                 !/*mf         39*/
      energi=tki(i)                                                             !/*mf         40*/
      up=cxi(i)                                                                 !/*mf         41*/
      vp=cyi(i)                                                                 !/*mf         42*/
      wp=czi(i)                                                                 !/*mf         43*/
      weight=wei(i)                                                             !/*mf         44*/
      if (it.gt.lpt) go to 10                                                   !/*mf         45*/
      if (ibertp.eq.1.and.(it.eq.6.or.it.eq.7)) go to 10                        !/*mf         46*/
      if (energi.le.emin(it)) go to 10                                          !/*mf         47*/
      if (nabov.ge.l02) go to 30                                                !/*mf         48*/
      nabov=nabov+1                                                             !/*mf         49*/
      tipa(nabov)=ntyp+100                                                      !/*mf         50*/
      tca(nabov)=tc                                                             !/*mf         51*/
      wta(nabov)=weight                                                         !/*mf         52*/
      ea(nabov)=energi                                                          !/*mf         53*/
      ua(nabov)=up                                                              !/*mf         54*/
      va(nabov)=vp                                                              !/*mf         55*/
      wa(nabov)=wp                                                              !/*mf         56*/
      namea(nabov)=namax+nabov                                                  !/*mf         57*/
      go to 20                                                                  !/*mf         58*/
   10 continue                                                                  !/*mf         59*/
      if (nbelo.ge.l02) go to 30                                                !/*mf         60*/
      nbelo=nbelo+1                                                             !/*mf         61*/
      tipb(nbelo)=ntyp+100                                                      !/*mf         62*/
      tcb(nbelo)=tc                                                             !/*mf         63*/
      wtb(nbelo)=weight                                                         !/*mf         64*/
      eb(nbelo)=energi                                                          !/*mf         65*/
      ub(nbelo)=up                                                              !/*mf         66*/
      vb(nbelo)=vp                                                              !/*mf         67*/
      wb(nbelo)=wp                                                              !/*mf         68*/
!L    ==================================================================        !/*mf         69*/
!L      if (lneutp.le.0) go to 20                                               !/*mf         70*/
!L      itt=ntyp+1                                                              !/*mf         71*/
!L      if (lneutp.ge.1.and.(itt.eq.12)) then                                   !/*mf         72*/
!L        ee=energi                                                             !/*mf         73*/
!L        wtemp=weight                                                          !/*mf         74*/
!L        if (wtemp.lt.swtm) then                                               !/*mf         75*/
!L          if (rang().gt.(wtemp/swtm)) go to 20                                !/*mf         76*/
!L          wtemp=swtm                                                          !/*mf         77*/
!L        endif                                                                 !/*mf         78*/
!L        call wrneut (nocas,itt,ee,ub(nbelo),vb(nbelo),wb(nbelo),xc,yc         !/*mf         79*/
!L     1  ,zc,wtemp,tc)                                                         !/*mf         80*/
!L      endif                                                                   !/*mf         81*/
!L    ==================================================================        !/*mf         82*/
   20 continue                                                                  !/*mf         83*/
      namax=namax+nabov                                                         !/*mf         84*/
      go to 40                                                                  !/*mf         85*/
!                                                                               !/*mf         86*/
   30 if (icnt.ge.10) go to 40                                                  !/*mf         87*/
      icnt=icnt+1                                                               !/*mf         88*/
      write (io,50) nabov,nbelo,nbnk                                            !/*mf         89*/
      write (itty,50) nabov,nbelo,nbnk                                          !/*mf         90*/
   40 continue                                                                  !/*mf         91*/
      return                                                                    !/*mf         92*/
!                                                                               !/*mf         93*/
   50 format (1x,'dimensions exceed in mfpd2. nabov=',i4,' nbelo=',i4,          !/*mf         94*/
     & ' nomax=',i6)                                                            !/*mf         95*/
      end                                                                       !/*mf         96*/

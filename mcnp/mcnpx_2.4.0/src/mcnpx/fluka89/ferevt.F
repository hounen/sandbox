!     $Id: ferevt.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                       !/*fe          1*/
      subroutine ferevt (nhad,kp,kt,pm,anuc,znuc)                               !/*fe          2*/
!                                                                     *         !/*fe          3*/
!---------------------------------------------------------------------*         !/*fe          4*/
!                                                                     *         !/*fe          5*/
#include <lcs/double.h>                                                         /*!fe          6*/
#include <fluka89/flprm1.h>                                                     /*!fe          7*/
#include <fluka89/flprm2.h>                                                     /*!fe          8*/
#include <lcs/inout.h>                                                          /*!fe          9*/
#include <fluka89/balanc.h>                                                     /*!fe         10*/
#include <fluka89/hadpar.h>                                                     /*!fe         11*/
#include <fluka89/part2.h>                                                      /*!fe         12*/
#include <fluka89/parevt.h>                                                     /*!fe         13*/
#include <fluka89/print.h>                                                      /*!fe         14*/
      an=0.4d0                                                                  !/*fe         15*/
      ferp=an*(znuc/anuc)**0.33333333333333333d0                                !/*fe         16*/
      fern=an*((anuc-znuc)/anuc)**0.33333333333333333d0                         !/*fe         17*/
      if (kt.eq.1) then                                                         !/*fe         18*/
        ferm=ferp                                                               !/*fe         19*/
      else                                                                      !/*fe         20*/
        ferm=fern                                                               !/*fe         21*/
      endif                                                                     !/*fe         22*/
      call fermic (p2)                                                          !/*fe         23*/
      p2=ferm*p2                                                                !/*fe         24*/
      call poli (polc,pols)                                                     !/*fe         25*/
      call sfecfe (sfe,cfe)                                                     !/*fe         26*/
      cxta=pols*cfe                                                             !/*fe         27*/
      cyta=pols*sfe                                                             !/*fe         28*/
      czta=polc                                                                 !/*fe         29*/
      efer=sqrt(p2*p2+aam(kt)**2)                                               !/*fe         30*/
      gam=efer/aam(kt)                                                          !/*fe         31*/
      bga=-sqrt(gam*gam-1.d0)                                                   !/*fe         32*/
      cod=1.d0                                                                  !/*fe         33*/
      sif=1.d0                                                                  !/*fe         34*/
      cof=0.d0                                                                  !/*fe         35*/
      em=sqrt(pm*pm+aam(kp)**2)                                                 !/*fe         36*/
      call altraf (gam,bga,cxta,cyta,czta,cod,cof,sif,pm,em,plabs,cxct          !/*fe         37*/
     & ,cyct,czct,elabs)                                                        !/*fe         38*/
      efrm=efrm+efer-aam(kt)                                                    !/*fe         39*/
      pxfrm=pxfrm+p2*cxta                                                       !/*fe         40*/
      pyfrm=pyfrm+p2*cyta                                                       !/*fe         41*/
      pzfrm=pzfrm+p2*czta                                                       !/*fe         42*/
      ruun=rang()                                                               !/*fe         43*/
      if (.not.ldiffr(kp).or.plabs.le.pthdff) ruun=1.d0                         !/*fe         44*/
      if (ruun.le.frdiff) then                                                  !/*fe         45*/
        call difevt (nhad,kp,kt,plabs)                                          !/*fe         46*/
      else                                                                      !/*fe         47*/
        call hadevt (nhad,kp,kt,plabs)                                          !/*fe         48*/
      endif                                                                     !/*fe         49*/
      cote=czct                                                                 !/*fe         50*/
      if (cote.ge.1.d0) cote=1.d0                                               !/*fe         51*/
      site=sqrt(1.d0-cote*cote)                                                 !/*fe         52*/
      cops=cxct/site                                                            !/*fe         53*/
      sips=cyct/site                                                            !/*fe         54*/
      do 10 i=1,nhad                                                            !/*fe         55*/
      px=pxh(i)                                                                 !/*fe         56*/
      py=pyh(i)                                                                 !/*fe         57*/
      pz=pzh(i)                                                                 !/*fe         58*/
      call rotat (px,py,pz,pxn,pyn,pzn,cote,site,cops,sips)                     !/*fe         59*/
      pxh(i)=pxn                                                                !/*fe         60*/
      pyh(i)=pyn                                                                !/*fe         61*/
      pzh(i)=pzn                                                                !/*fe         62*/
   10 continue                                                                  !/*fe         63*/
      gam=efer/aam(kt)                                                          !/*fe         64*/
      bga=sqrt(gam*gam-1.d0)                                                    !/*fe         65*/
      do 20 i=1,nhad                                                            !/*fe         66*/
      el=heph(i)                                                                !/*fe         67*/
      pl=sqrt(pxh(i)**2+pyh(i)**2+pzh(i)**2)                                    !/*fe         68*/
      cod=pzh(i)/pl                                                             !/*fe         69*/
      sid=sqrt(1.d0-cod*cod)                                                    !/*fe         70*/
      sif=pyh(i)/pl/sid                                                         !/*fe         71*/
      cof=pxh(i)/pl/sid                                                         !/*fe         72*/
      call altraf (gam,bga,cxta,cyta,czta,cod,cof,sif,pl,el,plr,cxr,cyr         !/*fe         73*/
     & ,czr,elr)                                                                !/*fe         74*/
      pxh(i)=plr*cxr                                                            !/*fe         75*/
      pyh(i)=plr*cyr                                                            !/*fe         76*/
      pzh(i)=plr*czr                                                            !/*fe         77*/
      heph(i)=sqrt(amh(i)**2+pxh(i)**2+pyh(i)**2+pzh(i)**2)                     !/*fe         78*/
   20 continue                                                                  !/*fe         79*/
      return                                                                    !/*fe         80*/
      end                                                                       !/*fe         81*/

!     $Id: ferhad.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                       !/*fe          1*/
      subroutine ferhad (anuc,znuc,it,plab,elab,cx,cy,cz,itta)                  !/*fe          2*/
!                                                                     *         !/*fe          3*/
!---------------------------------------------------------------------*         !/*fe          4*/
!                                                                     *         !/*fe          5*/
#include <lcs/double.h>                                                         /*!fe          6*/
#include <fluka89/flprm1.h>                                                     /*!fe          7*/
#include <fluka89/flprm2.h>                                                     /*!fe          8*/
#include <lcs/inout.h>                                                          /*!fe          9*/
#include <fluka89/fnlsp1.h>                                                     /*!fe         10*/
#include <fluka89/perco.h>                                                      /*!fe         11*/
      data itesv /0/                                                            !/*fe         12*/
#include <fluka89/kinpa.h>                                                      /*!fe         13*/
      dimension pxyz(10), e(2)                                                  !/*fe         14*/
#include <fluka89/ablts3.h>                                                     /*!fe         15*/
      data pxyz /10*0.d0/                                                       !/*fe         16*/
#include <fluka89/fermt.h>                                                      /*!fe         17*/
      elate=elab                                                                !/*fe         18*/
      ia=anuc                                                                   !/*fe         19*/
      ir=0                                                                      !/*fe         20*/
      iz=znuc                                                                   !/*fe         21*/
      ifert=-1                                                                  !/*fe         22*/
      az=1.d0                                                                   !/*fe         23*/
      if (ia.le.iz) ia=iz+1                                                     !/*fe         24*/
      an=0.4d0                                                                  !/*fe         25*/
      if (ia.le.1) an=0.d0                                                      !/*fe         26*/
      ferp=an*(znuc/anuc)**0.33333333333333333d0                                !/*fe         27*/
      fern=an*((anuc-znuc)/anuc)**0.33333333333333333d0                         !/*fe         28*/
      ihadc=0                                                                   !/*fe         29*/
   10 continue                                                                  !/*fe         30*/
      ihadc=ihadc+1                                                             !/*fe         31*/
      ir=1                                                                      !/*fe         32*/
      itr(1)=it                                                                 !/*fe         33*/
      cxr(1)=cx                                                                 !/*fe         34*/
      cyr(1)=cy                                                                 !/*fe         35*/
      czr(1)=cz                                                                 !/*fe         36*/
      elr(1)=elab                                                               !/*fe         37*/
      plr(1)=plab                                                               !/*fe         38*/
      if (ihadc.gt.20) return                                                   !/*fe         39*/
      nfer=0                                                                    !/*fe         40*/
      ferm=ferp                                                                 !/*fe         41*/
      if (itta.eq.8) ferm=fern                                                  !/*fe         42*/
      call frmic (p2)                                                           !/*fe         43*/
      if (anuc.le.1.d0) ferm=0.000001d0                                         !/*fe         44*/
      if (iperco.gt.0) p2=0.0000001d0                                           !/*fe         45*/
      p2=ferm*p2                                                                !/*fe         46*/
   20 continue                                                                  !/*fe         47*/
      call poli (polc,pols)                                                     !/*fe         48*/
      call cosi (sfe,cfe)                                                       !/*fe         49*/
      aaa=p2*pols                                                               !/*fe         50*/
      if (nfer.lt.10.and.ihadc.lt.10) go to 30                                  !/*fe         51*/
      iplab=plab*100.d0+10                                                      !/*fe         52*/
      ifpla=ferm*100.d0+10                                                      !/*fe         53*/
      ferml=ferm                                                                !/*fe         54*/
      if (plab.lt.ferml) ferm=ferp*iplab/ifpla                                  !/*fe         55*/
      if (plab.lt.ferml.and.itta.eq.8) ferm=fern*iplab/ifpla                    !/*fe         56*/
   30 continue                                                                  !/*fe         57*/
      pxyz(2)=aaa*cfe                                                           !/*fe         58*/
      pxyz(6)=aaa*sfe                                                           !/*fe         59*/
      pxyz(10)=p2*polc                                                          !/*fe         60*/
      rlke=p2**2/(sqrt(p2**2+am(itta)**2)+am(itta))                             !/*fe         61*/
      e(2)=rlke+am(itta)                                                        !/*fe         62*/
      amta=am(itta)                                                             !/*fe         63*/
      amit=am(it)                                                               !/*fe         64*/
      tlab=elab-amit                                                            !/*fe         65*/
      pot=anuc*0.007d0                                                          !/*fe         66*/
      ipot=ipot+1                                                               !/*fe         67*/
      e(1)=elab                                                                 !/*fe         68*/
      pxyz(1)=cx*plab                                                           !/*fe         69*/
      pxyz(5)=cy*plab                                                           !/*fe         70*/
      pxyz(9)=cz*plab                                                           !/*fe         71*/
      px=pxyz(1)+pxyz(2)                                                        !/*fe         72*/
      py=pxyz(5)+pxyz(6)                                                        !/*fe         73*/
      pz=pxyz(9)+pxyz(10)                                                       !/*fe         74*/
      call kinpar (pxyz,e,0.001d0)                                              !/*fe         75*/
      ga=e(2)/amta                                                              !/*fe         76*/
      bga=-sqrt((e(2)-amta)*(e(2)+amta))/amta                                   !/*fe         77*/
      cod=cz                                                                    !/*fe         78*/
      sid=sqrt(max(dp1-cod**2,dp1m20))                                          !/*fe         79*/
      if (sid.le.0.d0) sid=1.d-10                                               !/*fe         80*/
      sidm=1.d0/sid                                                             !/*fe         81*/
      sif=cy*sidm                                                               !/*fe         82*/
      cof=cx*sidm                                                               !/*fe         83*/
      call altraf (ga,bga,cxta,cyta,czta,cod,cof,sif,plab,elab,plabs            !/*fe         84*/
     & ,cxct,cyct,czct,elabs)                                                   !/*fe         85*/
      plabou=15.d0                                                              !/*fe         86*/
      if (elabs.gt.plabou) az=0.d0                                              !/*fe         87*/
      if (elabs.gt.plabou) elabs=plabou                                         !/*fe         88*/
      plabs=sqrt(elabs*elabs-amit*amit)                                         !/*fe         89*/
      elabt=sqrt(umo2)-amta                                                     !/*fe         90*/
      nfer=nfer+1                                                               !/*fe         91*/
      if (nfer.gt.20) go to 10                                                  !/*fe         92*/
      if (elabt.gt.elabke) go to 20                                             !/*fe         93*/
      ifert=1                                                                   !/*fe         94*/
      call hadrin (it,plabs,elabs,cxct,cyct,czct,itta)                          !/*fe         95*/
      elate=0.d0                                                                !/*fe         96*/
      do 40 iii=1,ir                                                            !/*fe         97*/
      itp=itr(iii)                                                              !/*fe         98*/
      ami=am(itp)                                                               !/*fe         99*/
      el=elr(iii)                                                               !/*fe        100*/
      pl=plr(iii)                                                               !/*fe        101*/
      cod=czr(iii)                                                              !/*fe        102*/
      sid=sqrt(max(dp1-cod**2,dp1m20))                                          !/*fe        103*/
      sidm=1.d0/sid                                                             !/*fe        104*/
      sif=cyr(iii)*sidm                                                         !/*fe        105*/
      cof=cxr(iii)*sidm                                                         !/*fe        106*/
      ga=e(2)/amta                                                              !/*fe        107*/
      bga=sqrt((e(2)-amta)*(e(2)+amta))/amta                                    !/*fe        108*/
      call altraf (ga,bga,cxta,cyta,czta,cod,cof,sif,pl,el,plr(iii),cxr         !/*fe        109*/
     & (iii),cyr(iii),czr(iii),elr(iii))                                        !/*fe        110*/
      elate=elate+elr(iii)                                                      !/*fe        111*/
      px=px-plr(iii)*cxr(iii)                                                   !/*fe        112*/
      py=py-plr(iii)*cyr(iii)                                                   !/*fe        113*/
      pz=pz-plr(iii)*czr(iii)                                                   !/*fe        114*/
   40 continue                                                                  !/*fe        115*/
      nfer=nfer+1                                                               !/*fe        116*/
      rlkex=p2**2/(sqrt(p2**2+amta**2)+amta)                                    !/*fe        117*/
      e(2)=rlkex+amta                                                           !/*fe        118*/
      if (elate-e(2).gt.a) go to 20                                             !/*fe        119*/
      if (ir.gt.1) elate=elate-am(itta)                                         !/*fe        120*/
      return                                                                    !/*fe        121*/
      end                                                                       !/*fe        122*/

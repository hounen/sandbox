!     $Id: collide.F,v 1.12 2002/03/27 18:14:58 gwm Exp $                     !/*co          1*/
      subroutine collide (name,icell,u,v,w,tc,wt,xc,yc,zc,                      !/*co          2*/
#ifdef CEM                                                                      /*!co          3*/
     & iccem,                                                                   !/*co          4*/
#endif                                                                          /*!co          5*/
     & lter)                                                                    !/*co          6*/
#include <lcs/double.h>                                                         /*!co          7*/
#include <lcs/lptS.h>                                                           /*!co          8*/
#include <lcs/inout.h>                                                          /*!co          9*/
#include <lcs/concom.h>                                                         /*!co         10*/
#include <lcs/rucom.h>                                                          /*!co         11*/
#include <lcs/banka.h>                                                          /*!co         12*/
#include <lcs/bankb.h>                                                          /*!co         13*/
#include <globals/interact_outcomes.h>                                          /*!co         14*/
!=======================================================================        !/*co         15*/
!----- nuclear interaction; ncol = 2 or 6                                       !/*co         16*/
      if (nopart.gt.0) then                                                     !/*co         17*/
        ncol=2                                                                  !/*co         18*/
      elseif (nopart.eq.0) then                                                 !/*co         19*/
        ncol=6                                                                  !/*co         20*/
      else                                                                      !/*co         21*/
        call errorf ('collide')                                                 !/*co         22*/
      endif                                                                     !/*co         23*/
      if (n1col.le.0.or.name.eq.1) go to 20                                     !/*co         24*/
      do 10 i=1,6                                                               !/*co         25*/
   10 npart(i)=0                                                                !/*co         26*/
      go to 150                                                                 !/*co         27*/
   20 continue                                                                  !/*co         28*/
!------ set up transformation to lab coordinates                                !/*co         29*/
      call transform (u,v,w)                                                    !/*co         30*/
      if (nopart.eq.0) go to 70                                                 !/*co         31*/
!=======================================================================        !/*co         32*/
!----- store cascade secondaries                                                !/*co         33*/
      kstor=1                                                                   !/*co         34*/
      do 60 i=1,nopart                                                          !/*co         35*/
      n=i                                                                       !/*co         36*/
      kk=n                                                                      !/*co         37*/
      ityp2=mod(kind(n),100)+1                                                  !/*co         38*/
      if(lter.eq.id_elastic_collisn_nonterminal.and.i.eq.nopart)go to 50        !/*co         39*/
#ifndef OLDM                                                                    /*!co         40*/
      if (noact.eq.2.and.ityp2.ne.ityp) go to 30                                !/*co         41*/
#endif                                                                          /*!co         42*/
      if (ep(n).gt.emin(ityp2)) go to 40                                        !/*co         43*/
      if (n1col.eq.-1) go to 30                                                 !/*co         44*/
!                                                                               !/*co         45*/
!     The following line is debatable in an MCNPX context.                      !/*co         46*/
!     It causes a pbar which was born below cutoff to always                    !/*co         47*/
!     interact. (An nbar below cutoff won't - but that should                   !/*co         48*/
!     be a minor question.) This is good for a low cutoff for                   !/*co         49*/
!     pbar and zero cutoff for nbar.                                            !/*co         50*/
      if (ityp2.eq.17) go to 40                                                 !/*co         51*/
!                                                                               !/*co         52*/
      if (instab(ityp2).eq.0) go to 30                                          !/*co         53*/
      if (ityp2.ne.6.and.ityp2.ne.7) go to 40                                   !/*co         54*/
      if (ibertp.le.0) go to 40                                                 !/*co         55*/
   30 continue                                                                  !/*co         56*/
      call datalo (tc,wt,xc,yc,zc,kk,kstor,mm,mtype,ito,icell)                  !/*co         57*/
      go to 60                                                                  !/*co         58*/
   40 continue                                                                  !/*co         59*/
      call datahi (name,tc,wt,kk,kstor,mm,mtype,ito)                            !/*co         60*/
      ityp1=ityp2                                                               !/*co         61*/
      go to 60                                                                  !/*co         62*/
   50 continue                                                                  !/*co         63*/
      kst=6                                                                     !/*co         64*/
      call datahi (name,tc,wt,kk,kst,mm,mtype,ito)                              !/*co         65*/
      ityp1=ityp2                                                               !/*co         66*/
   60 continue                                                                  !/*co         67*/
!=======================================================================        !/*co         68*/
!----- invoke evaporation model and PHT                                         !/*co         69*/
   70 continue                                                                  !/*co         70*/
!        (LTER == 2 means elastic collision.)                                   !/*co         71*/
      if (lter.eq.id_elastic_collisn_nonterminal) go to 140                     !/*co         72*/
#ifdef CEM                                                                      /*!co         73*/
!        Preequilibrium decay, equilibrium decay, and                           !/*co         74*/
!        PHT deexcitation already performed in CEM.                             !/*co         75*/
      if(iccem.eq.0)call erup (name,wt)                                         !/*co         76*/
#else                                                                           /*!co         77*/
      call erup (name,wt)                                                       !/*co         78*/
#endif                                                                          /*!co         79*/
      if (nph.le.0) go to 90                                                    !/*co         80*/
!----- store PHT photons                                                        !/*co         81*/
      kstor=5                                                                   !/*co         82*/
      ito=1                                                                     !/*co         83*/
      do 80 i=1,nph                                                             !/*co         84*/
      kk=i                                                                      !/*co         85*/
      if (eph(i).eq.dp0) then                                                   !/*co         86*/
        ito=ito+1                                                               !/*co         87*/
      else                                                                      !/*co         88*/
        if (eph(i).gt.emin(12)) then                                            !/*co         89*/
          call datahi (name,tc,wt,kk,kstor,mm,mtype,ito)                        !/*co         90*/
        else                                                                    !/*co         91*/
          call datalo (tc,wt,xc,yc,zc,kk,kstor,mm,mtype,ito,icell)              !/*co         92*/
        endif                                                                   !/*co         93*/
      endif                                                                     !/*co         94*/
   80 continue                                                                  !/*co         95*/
   90 continue                                                                  !/*co         96*/
!----- store evaporation secondaries                                            !/*co         97*/
!----- protons and neutrons                                                     !/*co         98*/
      do 110 ll=1,2                                                             !/*co         99*/
      nup=npart(ll)                                                             !/*co        100*/
      if (nup.le.0) go to 110                                                   !/*co        101*/
      ip=3-ll                                                                   !/*co        102*/
      kstor=ll+1                                                                !/*co        103*/
      ito=1                                                                     !/*co        104*/
      do 100 i=1,nup                                                            !/*co        105*/
      kk=i                                                                      !/*co        106*/
      if (epart(i,ll).eq.dp0) then                                              !/*co        107*/
        ito=ito+1                                                               !/*co        108*/
      else                                                                      !/*co        109*/
        if (epart(i,ll).gt.emin(ip)) then                                       !/*co        110*/
          call datahi (name,tc,wt,kk,kstor,mm,mtype,ito)                        !/*co        111*/
        else                                                                    !/*co        112*/
          call datalo (tc,wt,xc,yc,zc,kk,kstor,mm,mtype,ito,icell)              !/*co        113*/
        endif                                                                   !/*co        114*/
      endif                                                                     !/*co        115*/
  100 continue                                                                  !/*co        116*/
  110 continue                                                                  !/*co        117*/
!----- store evaporation secondaries                                            !/*co        118*/
!----- ions                                                                     !/*co        119*/
      kstor=4                                                                   !/*co        120*/
      do 130 i=3,6                                                              !/*co        121*/
      np=npart(i)                                                               !/*co        122*/
      if (i.eq.4.and.apr.ge.1000) np=np-14                                      !/*co        123*/
      if (np.le.0) go to 130                                                    !/*co        124*/
      mtype=i-2                                                                 !/*co        125*/
      ito=1                                                                     !/*co        126*/
      do 120 j=1,np                                                             !/*co        127*/
      mm=j                                                                      !/*co        128*/
      if (hepart(j,mtype).eq.dp0) then                                          !/*co        129*/
        ito=ito+1                                                               !/*co        130*/
      else                                                                      !/*co        131*/
        if (hepart(j,mtype).gt.emin(i+5)) then                                  !/*co        132*/
          call datahi (name,tc,wt,kk,kstor,mm,mtype,ito)                        !/*co        133*/
        else                                                                    !/*co        134*/
          call datalo (tc,wt,xc,yc,zc,kk,kstor,mm,mtype,ito,icell)              !/*co        135*/
        endif                                                                   !/*co        136*/
      endif                                                                     !/*co        137*/
  120 continue                                                                  !/*co        138*/
  130 continue                                                                  !/*co        139*/
  140 continue                                                                  !/*co        140*/
      namax=namax+nabov                                                         !/*co        141*/
  150 continue                                                                  !/*co        142*/
      return                                                                    !/*co        143*/
!                                                                               !/*co        144*/
      end                                                                       !/*co        145*/

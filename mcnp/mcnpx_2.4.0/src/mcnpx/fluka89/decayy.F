!     $Id: decayy.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*de          1*/
      subroutine decayy (ihad,istab)                                            !/*de          2*/
!                                                                     *         !/*de          3*/
!---------------------------------------------------------------------*         !/*de          4*/
!                                                                     *         !/*de          5*/
#include <lcs/double.h>                                                         /*!de          6*/
#include <fluka89/flprm1.h>                                                     /*!de          7*/
#include <fluka89/flprm2.h>                                                     /*!de          8*/
#include <lcs/inout.h>                                                          /*!de          9*/
#include <fluka89/finpr1.h>                                                     /*!de         10*/
#include <fluka89/metlsp.h>                                                     /*!de         11*/
#include <fluka89/part1.h>                                                      /*!de         12*/
#include <fluka89/decyc1.h>                                                     /*!de         13*/
#include <fluka89/drei1.h>                                                      /*!de         14*/
#include <fluka89/gamrd1.h>                                                     /*!de         15*/
      redu=2.d0                                                                 !/*de         16*/
      do 10 i=1,ihad                                                            !/*de         17*/
      its(i)=nref(i)                                                            !/*de         18*/
      pls(i)=sqrt(pxf(i)**2+pyf(i)**2+pzf(i)**2)                                !/*de         19*/
      if (pls(i).ne.0.d0) then                                                  !/*de         20*/
        cxs(i)=pxf(i)/pls(i)                                                    !/*de         21*/
        cys(i)=pyf(i)/pls(i)                                                    !/*de         22*/
        czs(i)=pzf(i)/pls(i)                                                    !/*de         23*/
      endif                                                                     !/*de         24*/
      els(i)=hef(i)                                                             !/*de         25*/
   10 continue                                                                  !/*de         26*/
      ist=ihad                                                                  !/*de         27*/
      ir=0                                                                      !/*de         28*/
   20 continue                                                                  !/*de         29*/
      if (istab.eq.1) go to 40                                                  !/*de         30*/
      if (istab.eq.2) go to 60                                                  !/*de         31*/
      if (istab.eq.3) go to 50                                                  !/*de         32*/
      if (istab.eq.4) go to 30                                                  !/*de         33*/
!REP                                                                            !/*de         34*/
!     istab.eq.5                                                                !/*de         35*/
      if (its(ist).ge.1.and.its(ist).le.23) go to 70                            !/*de         36*/
      go to 80                                                                  !/*de         37*/
!REP                                                                            !/*de         38*/
   30 if (its(ist).ge.1..and.its(ist).le.11) go to 70                           !/*de         39*/
      if (its(ist).ge.13.and.its(ist).le.14) go to 70                           !/*de         40*/
      if (its(ist).eq.23) go to 70                                              !/*de         41*/
      go to 80                                                                  !/*de         42*/
   40 if (its(ist).eq.135.or.its(ist).eq.136) go to 70                          !/*de         43*/
      if (its(ist).ge.1.and.its(ist).le.7) go to 70                             !/*de         44*/
      go to 80                                                                  !/*de         45*/
   50 if (its(ist).ge.1.and.its(ist).le.23) go to 70                            !/*de         46*/
      if (its(ist).ge.97.and.its(ist).le.103) go to 70                          !/*de         47*/
      if (its(ist).eq.109.and.its(ist).eq.115) go to 70                         !/*de         48*/
      if (its(ist).ge.133.and.its(ist).le.136) go to 70                         !/*de         49*/
      go to 80                                                                  !/*de         50*/
   60 if (its(ist).ge.1.and.its(ist).le.30) go to 70                            !/*de         51*/
      if (its(ist).ge.97.and.its(ist).le.103) go to 70                          !/*de         52*/
      if (its(ist).ge.115.and.its(ist).le.122) go to 70                         !/*de         53*/
      if (its(ist).ge.131.and.its(ist).le.136) go to 70                         !/*de         54*/
      if (its(ist).eq.109) go to 70                                             !/*de         55*/
      if (its(ist).ge.137.and.its(ist).le.160) go to 70                         !/*de         56*/
      go to 80                                                                  !/*de         57*/
   70 ir=ir+1                                                                   !/*de         58*/
      nref(ir)=its(ist)                                                         !/*de         59*/
      itt=its(ist)                                                              !/*de         60*/
      amf(ir)=am(itt)                                                           !/*de         61*/
      anf(ir)=aname(itt)                                                        !/*de         62*/
      ichf(ir)=ich(itt)                                                         !/*de         63*/
      ibarf(ir)=ibar(itt)                                                       !/*de         64*/
      hef(ir)=els(ist)                                                          !/*de         65*/
      pxf(ir)=cxs(ist)*pls(ist)                                                 !/*de         66*/
      pyf(ir)=cys(ist)*pls(ist)                                                 !/*de         67*/
      pzf(ir)=czs(ist)*pls(ist)                                                 !/*de         68*/
      ist=ist-1                                                                 !/*de         69*/
      if (ist.ge.1) go to 20                                                    !/*de         70*/
      go to 140                                                                 !/*de         71*/
   80 it=its(ist)                                                               !/*de         72*/
      gam=els(ist)/am(it)                                                       !/*de         73*/
      bgam=pls(ist)/am(it)                                                      !/*de         74*/
      eco=am(it)                                                                !/*de         75*/
      kz1=k1(it)                                                                !/*de         76*/
      vv=rang()-1.d-17                                                          !/*de         77*/
      iik=kz1-1                                                                 !/*de         78*/
   90 iik=iik+1                                                                 !/*de         79*/
      if (vv.gt.wt(iik)) go to 90                                               !/*de         80*/
      it1=nzk(iik,1)                                                            !/*de         81*/
      it2=nzk(iik,2)                                                            !/*de         82*/
      if (it2-1.lt.0) go to 120                                                 !/*de         83*/
      it3=nzk(iik,3)                                                            !/*de         84*/
      if (it3.eq.0) go to 100                                                   !/*de         85*/
      call threpd (eco,ecm1,ecm2,ecm3,pcm1,pcm2,pcm3,cod1,cof1,sif1,cod2        !/*de         86*/
     & ,cof2,sif2,cod3,cof3,sif3,am(it1),am(it2),am(it3))                       !/*de         87*/
      go to 110                                                                 !/*de         88*/
  100 call twopad (eco,ecm1,ecm2,pcm1,pcm2,cod1,cof1,sif1,cod2,cof2,sif2        !/*de         89*/
     & ,am(it1),am(it2))                                                        !/*de         90*/
  110 continue                                                                  !/*de         91*/
  120 continue                                                                  !/*de         92*/
      its(ist)=it1                                                              !/*de         93*/
      if (it2-1.lt.0) go to 130                                                 !/*de         94*/
      its(ist+1)=it2                                                            !/*de         95*/
      its(ist+2)=it3                                                            !/*de         96*/
      rx=cxs(ist)                                                               !/*de         97*/
      ry=cys(ist)                                                               !/*de         98*/
      rz=czs(ist)                                                               !/*de         99*/
      call trafo (gam,bgam,rx,ry,rz,cod1,cof1,sif1,pcm1,ecm1,pls(ist)           !/*de        100*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*de        101*/
      ist=ist+1                                                                 !/*de        102*/
      call trafo (gam,bgam,rx,ry,rz,cod2,cof2,sif2,pcm2,ecm2,pls(ist)           !/*de        103*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*de        104*/
      if (it3.le.0) go to 130                                                   !/*de        105*/
      ist=ist+1                                                                 !/*de        106*/
      call trafo (gam,bgam,rx,ry,rz,cod3,cof3,sif3,pcm3,ecm3,pls(ist)           !/*de        107*/
     & ,cxs(ist),cys(ist),czs(ist),els(ist))                                    !/*de        108*/
  130 continue                                                                  !/*de        109*/
      go to 20                                                                  !/*de        110*/
  140 continue                                                                  !/*de        111*/
      idapu=idmax3                                                              !/*de        112*/
      if (ir.gt.idmax3) write (itty,150) idapu                                  !/*de        113*/
      ihad=ir                                                                   !/*de        114*/
      return                                                                    !/*de        115*/
!                                                                               !/*de        116*/
  150 format (' number of stab. final part. is greater than',i5)                !/*de        117*/
      end                                                                       !/*de        118*/

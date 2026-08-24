!     $Id: emit.F,v 1.5 2002/03/27 18:15:02 gwm Exp $                         !/*em          1*/
      subroutine emit (a,z,ja,jz,kfiss,jrn,cos_,cosh_,ep_,hep_,np_,l12          !/*em          2*/
     & ,l13)                                                                    !/*em          3*/
#include <lcs/double.h>                                                         /*!em          4*/
#ifndef OLDM                                                                    /*!em          5*/
#include <lcs/phprm0.h>                                                         /*!em          6*/
#endif                                                                          /*!em          7*/
#include <lcs/lptS.h>                                                           /*!em          8*/
#include <lcs/cacom.h>                                                          /*!em          9*/
#include <lcs/ercom.h>                                                          /*!em         10*/
#include <lcs/inout.h>                                                          /*!em         11*/
      dimension ep_(l12,2), hep_(l13,4), cos_(3,l12,2), cosh_(3,l13,4),         !/*em         12*/
     & np_(6)                                                                   !/*em         13*/
      dimension c(3)                                                            !/*em         14*/
      data nwr2 /0/, ballim /0.1d0/                                             !/*em         15*/
#ifdef OLDM                                                                     /*!em         16*/
      data um /931.20793d0/                                                     !/*em         17*/
#endif                                                                          /*!em         18*/
      kfiss=0                                                                   !/*em         19*/
      k=jemiss                                                                  !/*em         20*/
      if (jrn.ne.0) then                                                        !/*em         21*/
        eps=uu-q9(k)                                                            !/*em         22*/
        go to 40                                                                !/*em         23*/
      endif                                                                     !/*em         24*/
      if (penbar) go to 40                                                      !/*em         25*/
!.......................................................................        !/*em         26*/
!///// ORNL routine for cluster energy                                          !/*em         27*/
      if (ievap.ne.0) then                                                      !/*em         28*/
        js=sos(k)+dp1                                                           !/*em         29*/
        if (js.ge.1000) then                                                    !/*em         30*/
          ratio2=(s9(k)**3-6.d0*s9(k)**2+15.d0*s9(k)-15.d0)/((dp2*s9(k)*        !/*em         31*/
     &    *2-6.d0*s9(k)+6.d0)*smalla(k))                                        !/*em         32*/
        else                                                                    !/*em         33*/
          ratio2=(pp2(js)+(pp2(js+1)-pp2(js))*(sos(k)-strun(k)))/smalla         !/*em         34*/
     &    (k)                                                                   !/*em         35*/
        endif                                                                   !/*em         36*/
        epsav=dp2*ratio2                                                        !/*em         37*/
!.......................................................................        !/*em         38*/
!///// RAL routine for cluster energy /////                                     !/*em         39*/
      else                                                                      !/*em         40*/
        call drein2 (s9(k),smalla(k),eye2)                                      !/*em         41*/
        epsav=eye2/eye1(k)                                                      !/*em         42*/
      endif                                                                     !/*em         43*/
!.......................................................................        !/*em         44*/
      if (k.eq.1) then                                                          !/*em         45*/
        mm=ja-ia6(k)                                                            !/*em         46*/
        epsav=(epsav+bet(mm))/(dp1+bet(mm)*eye0/eye1(k))                        !/*em         47*/
      endif                                                                     !/*em         48*/
!.......................................................................        !/*em         49*/
!///// RAL fission test /////                                                   !/*em         50*/
      if (ievap.eq.0.and.jz.ge.71.and.k.eq.1.and..not.fisinh.and.nofis          !/*em         51*/
     & .ne.0) then                                                              !/*em         52*/
        agoes=(uu-7.d0)/(epsav+7.d0)                                            !/*em         53*/
        if (agoes.lt.dp1.or.z.gt.88.d0) agoes=dp1                               !/*em         54*/
        probf=fprob(z,a,uu)/agoes                                               !/*em         55*/
        if (probf.gt.rang()) then                                               !/*em         56*/
          fisinh=.true.                                                         !/*em         57*/
          kfiss=1                                                               !/*em         58*/
          go to 90                                                              !/*em         59*/
        endif                                                                   !/*em         60*/
      endif                                                                     !/*em         61*/
!.......................................................................        !/*em         62*/
      eps0=dph*epsav                                                            !/*em         63*/
      smax=uu-thresh(jemiss)                                                    !/*em         64*/
      if (smax.lt.eps0) go to 20                                                !/*em         65*/
   10 continue                                                                  !/*em         66*/
      f1=-log(rang())                                                           !/*em         67*/
      f2=-log(rang())                                                           !/*em         68*/
      ss=eps0*(f1+f2)                                                           !/*em         69*/
      if (ss.ge.smax) go to 10                                                  !/*em         70*/
      go to 30                                                                  !/*em         71*/
   20 continue                                                                  !/*em         72*/
      r1=rang()                                                                 !/*em         73*/
      test=r1*exp(smax*(dp1-r1)/eps0)                                           !/*em         74*/
      r2=rang()                                                                 !/*em         75*/
      if (r2.ge.test) go to 20                                                  !/*em         76*/
      ss=smax*r1                                                                !/*em         77*/
   30 continue                                                                  !/*em         78*/
      eps=ss+thresh(jemiss)-q9(jemiss)                                          !/*em         79*/
   40 continue                                                                  !/*em         80*/
#ifdef OLDM                                                                     /*!em         81*/
      rnmass=ex2+ar*um                                                          !/*em         82*/
#else                                                                           /*!em         83*/
      rnmass=xmnuc(ar,zr)                                                       !/*em         84*/
#endif                                                                          /*!em         85*/
      unew=(uu-q9(jemiss))-eps                                                  !/*em         86*/
      if (unew.lt.dp0) then                                                     !/*em         87*/
        test1=unew+uu*1.0d-04                                                   !/*em         88*/
        if (test1.lt.dp0) write (io,100) uu,unew,ja,jz,jemiss                   !/*em         89*/
        eps=max((uu-q9(jemiss)),dp0)                                            !/*em         90*/
        unew=dp0                                                                !/*em         91*/
      endif                                                                     !/*em         92*/
      rnmass=rnmass+unew                                                        !/*em         93*/
      xkp=dph*eps*(dp2*rnmass+eps)/(rnmass+eps+zmass(jemiss))                   !/*em         94*/
      xkt=eps-xkp                                                               !/*em         95*/
#ifdef OLDM                                                                     /*!em         96*/
      parmas=exmass(jemiss)+um*fla9(jemiss)                                     !/*em         97*/
#else                                                                           /*!em         98*/
      parmas=zmass(jemiss)                                                      !/*em         99*/
#endif                                                                          /*!em        100*/
      epp=xkp+parmas                                                            !/*em        101*/
      etp=xkt+rnmass                                                            !/*em        102*/
      aaa=xkp*(xkp+dp2*parmas)                                                  !/*em        103*/
      bbb=sqrt(aaa*(gammaa-dp1)*(gammaa+dp1))                                   !/*em        104*/
      call gtiso (c(1),c(2),c(3))                                               !/*em        105*/
      coscm=coslbr(1)*c(1)+coslbr(2)*c(2)+coslbr(3)*c(3)                        !/*em        106*/
      eps=gammaa*epp+bbb*coscm-parmas                                           !/*em        107*/
      erec=gammaa*etp-bbb*coscm-rnmass                                          !/*em        108*/
      aa1=aaa**2*(dp1-coscm**2)                                                 !/*em        109*/
      dnp=sqrt(aa1+(bbb*epp+gammaa*aaa*coscm)**2)                               !/*em        110*/
      dnt=sqrt(aa1+(bbb*etp-gammaa*aaa*coscm)**2)                               !/*em        111*/
      c1=bbb*epp+aaa*(gammaa-dp1)*coscm                                         !/*em        112*/
      c2=bbb*etp-aaa*(gammaa-dp1)*coscm                                         !/*em        113*/
      do 50 i=1,3                                                               !/*em        114*/
      coslbp(i)=(c1*coslbr(i)+aaa*c(i))/dnp                                     !/*em        115*/
      coslbr(i)=(c2*coslbr(i)-aaa*c(i))/dnt                                     !/*em        116*/
   50 continue                                                                  !/*em        117*/
      uu=unew                                                                   !/*em        118*/
      np_(jemiss)=np_(jemiss)+1                                                 !/*em        119*/
      ja=ja-ia6(jemiss)                                                         !/*em        120*/
      jz=jz-iz6(jemiss)                                                         !/*em        121*/
      xrhs=uu+eps+erec+exmass(jemiss)+ex2                                       !/*em        122*/
      if (abs(xlhs-xrhs).ge.ballim) then                                        !/*em        123*/
        nwr2=nwr2+1                                                             !/*em        124*/
        if (nwr2.lt.10) write (io,110) xlhs,xrhs,ja,jz,jemiss                   !/*em        125*/
      endif                                                                     !/*em        126*/
!*****store,end of normal cycle                                                 !/*em        127*/
      smom1(jemiss)=smom1(jemiss)+eps                                           !/*em        128*/
      if (np_(jemiss).le.0) call expirx(1,'EMIT','np_(jemiss).le.0')            !/*em        129*/
      if (jemiss.gt.2) go to 70                                                 !/*em        130*/
      ep_(np_(jemiss),jemiss)=eps                                               !/*em        131*/
      do 60 i=1,3                                                               !/*em        132*/
   60 cos_(i,np_(jemiss),jemiss)=coslbp(i)                                      !/*em        133*/
      go to 90                                                                  !/*em        134*/
   70 kemiss=jemiss-2                                                           !/*em        135*/
      hep_(np_(jemiss),kemiss)=eps                                              !/*em        136*/
      do 80 i=1,3                                                               !/*em        137*/
   80 cosh_(i,np_(jemiss),kemiss)=coslbp(i)                                     !/*em        138*/
   90 continue                                                                  !/*em        139*/
!.......................................................................        !/*em        140*/
      return                                                                    !/*em        141*/
!                                                                               !/*em        142*/
  100 format (' *** uu=',g10.3,' unew=',g10.3,' ja=',i3,' jz=',i3,              !/*em        143*/
     & ' jemiss=',i2)                                                           !/*em        144*/
  110 format (' *** xlhs=',g10.3,' xrhs=',g10.3,' ja=',i3,' jz=',i3,            !/*em        145*/
     & ' jemiss=',i2)                                                           !/*em        146*/
      end                                                                       !/*em        147*/

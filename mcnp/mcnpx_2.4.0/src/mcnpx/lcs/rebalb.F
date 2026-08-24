!     $Id: rebalb.F,v 1.5 2002/03/27 18:15:19 gwm Exp $                       !/*re          1*/
      subroutine rebalb (ecl,name,a1,z1,nrj,negzpr)                             !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
#ifndef OLDM                                                                    /*!re          4*/
#include <lcs/phprm0.h>                                                         /*!re          5*/
#endif                                                                          /*!re          6*/
#include <lcs/lptS.h>                                                           /*!re          7*/
#include <lcs/inout.h>                                                          /*!re          8*/
#include <lcs/rucom.h>                                                          /*!re          9*/
#include <lcs/cacom.h>                                                          /*!re         10*/
#include <lcs/banka.h>                                                          /*!re         11*/
      dimension exm(5), parts(5)                                                !/*re         12*/
      data exm /7.58499d0,8.367439d0,140.08d0,134.96d0,139.06d0/                !/*re         13*/
#ifndef OLDM                                                                    /*!re         14*/
      data enlim /0.01d0/                                                       !/*re         15*/
      data ballim /1.0d-03/                                                     !/*re         16*/
      waz1=xmnuc(a1,z1)                                                         !/*re         17*/
#else                                                                           /*!re         18*/
      data enlim /5.d-2/                                                        !/*re         19*/
!     CHECK: pi+ mass increased by electron mass                                !/*re         20*/
!     CHECK: pi- mass decreased by electron mass                                !/*re         21*/
      data amumev /931.20793d0/                                                 !/*re         22*/
      data ncbal /0/                                                            !/*re         23*/
      waz1=energy(a1,z1)                                                        !/*re         24*/
#endif                                                                          /*!re         25*/
      if (flim0.ge.dp0) go to 20                                                !/*re         26*/
      if (ecl.gt.250.d0) go to 10                                               !/*re         27*/
      if (ecl.le.100.d0) flim=5.d-2                                             !/*re         28*/
      if (ecl.gt.100.d0) flim=dp5/ecl                                           !/*re         29*/
      go to 30                                                                  !/*re         30*/
   10 continue                                                                  !/*re         31*/
      flim=2.d-2                                                                !/*re         32*/
      go to 30                                                                  !/*re         33*/
   20 continue                                                                  !/*re         34*/
      flim=flim0                                                                !/*re         35*/
   30 continue                                                                  !/*re         36*/
      if (nopart.gt.0) go to 40                                                 !/*re         37*/
      apr=dp0                                                                   !/*re         38*/
      zpr=dp0                                                                   !/*re         39*/
      ext=dp0                                                                   !/*re         40*/
      sume=dp0                                                                  !/*re         41*/
      go to 80                                                                  !/*re         42*/
   40 continue                                                                  !/*re         43*/
      nnnn=0                                                                    !/*re         44*/
      sume=dp0                                                                  !/*re         45*/
      do 50 i=1,5                                                               !/*re         46*/
   50 parts(i)=dp0                                                              !/*re         47*/
      do 60 i=1,nopart                                                          !/*re         48*/
      lk=kind(i)+1                                                              !/*re         49*/
      parts(lk)=parts(lk)+dp1                                                   !/*re         50*/
      sume=sume+ep(i)                                                           !/*re         51*/
   60 continue                                                                  !/*re         52*/
      apr=-parts(1)-parts(2)                                                    !/*re         53*/
      zpr=-parts(1)-parts(3)+parts(5)                                           !/*re         54*/
      ext=dp0                                                                   !/*re         55*/
      do 70 i=1,5                                                               !/*re         56*/
#ifdef OLDM                                                                     /*!re         57*/
      ext=ext+parts(i)*exm(i)                                                   !/*re         58*/
#else                                                                           /*!re         59*/
      ext=ext+parts(i)*pmass(i)                                                 !/*re         60*/
#endif                                                                          /*!re         61*/
   70 continue                                                                  !/*re         62*/
      ext=-ext                                                                  !/*re         63*/
   80 continue                                                                  !/*re         64*/
      apr=apr+a1+dp1                                                            !/*re         65*/
      tip=ityp-1                                                                !/*re         66*/
      zpr=zpr+z1+dp1-tip                                                        !/*re         67*/
      if (ityp.le.2) go to 90                                                   !/*re         68*/
      apr=apr-dp1                                                               !/*re         69*/
      zpr=zpr+2                                                                 !/*re         70*/
   90 continue                                                                  !/*re         71*/
      if (zpr.lt.dp0.or.apr.lt.zpr) go to 220                                   !/*re         72*/
#ifdef OLDM                                                                     /*!re         73*/
      if (apr.eq.dp1) go to 100                                                 !/*re         74*/
      if (apr.eq.zpr.or.zpr.eq.dp0) go to 230                                   !/*re         75*/
  100 continue                                                                  !/*re         76*/
      waz2=energy(apr,zpr)                                                      !/*re         77*/
      ext=ext+exm(ityp)+waz1-waz2                                               !/*re         78*/
      ext1=ext+ecl                                                              !/*re         79*/
      waz0=waz2+apr*amumev                                                      !/*re         80*/
      ex=ext1-sume                                                              !/*re         81*/
      waz=waz0+ex                                                               !/*re         82*/
#else                                                                           /*!re         83*/
      if (apr.eq.dp0) go to 230                                                 !/*re         84*/
      if ((apr.eq.zpr.or.zpr.eq.dp0).and.apr.gt.dp3) go to 230                  !/*re         85*/
  100 continue                                                                  !/*re         86*/
      if (apr.eq.dp0) then                                                      !/*re         87*/
        waz2=dp0                                                                !/*re         88*/
      else                                                                      !/*re         89*/
        waz2=xmnuc(apr,zpr)                                                     !/*re         90*/
      endif                                                                     !/*re         91*/
      ext=ext+pmass(ityp)+waz1-waz2                                             !/*re         92*/
      ext1=ext+ecl                                                              !/*re         93*/
      ex=ext1-sume                                                              !/*re         94*/
      waz=waz2+ex                                                               !/*re         95*/
#endif                                                                          /*!re         96*/
      call recoil (ecl,waz,0)                                                   !/*re         97*/
      if (erec.lt.dp0) go to 170                                                !/*re         98*/
      ex=ex-erec                                                                !/*re         99*/
      if (apr.eq.dp1) go to 110                                                 !/*re        100*/
      if (ex) 120,190,190                                                       !/*re        101*/
  110 continue                                                                  !/*re        102*/
      nrnorm(4)=nrnorm(4)+1                                                     !/*re        103*/
  120 continue                                                                  !/*re        104*/
      if (nopart.le.0) then                                                     !/*re        105*/
        negex=negex+1                                                           !/*re        106*/
        nopart=-1                                                               !/*re        107*/
        return                                                                  !/*re        108*/
      endif                                                                     !/*re        109*/
      exhold=-abs(ex)                                                           !/*re        110*/
      factot=dp1                                                                !/*re        111*/
      if (abs(ex).le.enlim) go to 180                                           !/*re        112*/
      if (flim.eq.dp0) go to 240                                                !/*re        113*/
  130 continue                                                                  !/*re        114*/
      fac=dp1+ex/sume                                                           !/*re        115*/
      fac=min(max(dp9m1,fac),dp11m1)                                            !/*re        116*/
      factot=factot*fac                                                         !/*re        117*/
      if (nnnn.gt.0.and.abs(factot-dp1).gt.flim) go to 240                      !/*re        118*/
      do 140 i=1,nopart                                                         !/*re        119*/
  140 ep(i)=fac*ep(i)                                                           !/*re        120*/
      sume=fac*sume                                                             !/*re        121*/
      ex=ext1-sume                                                              !/*re        122*/
#ifdef OLDM                                                                     /*!re        123*/
      waz=waz0+ex                                                               !/*re        124*/
#else                                                                           /*!re        125*/
      waz=waz2+ex                                                               !/*re        126*/
#endif                                                                          /*!re        127*/
      call recoil (ecl,waz,0)                                                   !/*re        128*/
      if (erec.lt.dp0) go to 170                                                !/*re        129*/
      ex=ex-erec                                                                !/*re        130*/
      nnnn=nnnn+1                                                               !/*re        131*/
      if (abs(ex).le.enlim.and.abs(factot-dp1).le.flim) go to 180               !/*re        132*/
      if (nnnn-10) 130,160,150                                                  !/*re        133*/
  150 continue                                                                  !/*re        134*/
      call expirx(1,'REBALB','nnnn.gt.10')                                      !/*re        135*/
  160 continue                                                                  !/*re        136*/
      nrnorm(2)=nrnorm(2)+1                                                     !/*re        137*/
      go to 240                                                                 !/*re        138*/
  170 continue                                                                  !/*re        139*/
      name=igetd(2)                                                             !/*re        140*/
      write (io,260) nocas,name,apr,zpr                                         !/*re        141*/
      write (itty,260) nocas,name,apr,zpr                                       !/*re        142*/
      exhold=dp0                                                                !/*re        143*/
      go to 240                                                                 !/*re        144*/
  180 continue                                                                  !/*re        145*/
      exlow(2)=min(ex,exlow(2))                                                 !/*re        146*/
      exlow(4)=max(ex,exlow(4))                                                 !/*re        147*/
      exlow(1)=min(exhold,exlow(1))                                             !/*re        148*/
      exlow(5)=exlow(5)+ex                                                      !/*re        149*/
      erec=max(erec+ex,dp0)                                                     !/*re        150*/
      ex=dp0                                                                    !/*re        151*/
      nrnorm(1)=nrnorm(1)+1                                                     !/*re        152*/
      nrnorm(3)=nrnorm(3)+nnnn                                                  !/*re        153*/
      if (apr.ne.dp1) go to 190                                                 !/*re        154*/
      nopart=nopart+1                                                           !/*re        155*/
      ep(nopart)=erec                                                           !/*re        156*/
      kind(nopart)=dp1-zpr                                                      !/*re        157*/
      alpha(nopart)=coslbr(1)                                                   !/*re        158*/
      beta(nopart)=coslbr(2)                                                    !/*re        159*/
      gam(nopart)=coslbr(3)                                                     !/*re        160*/
      ex=dp0                                                                    !/*re        161*/
      erec=dp0                                                                  !/*re        162*/
      waz2=dp0                                                                  !/*re        163*/
      apr=dp0                                                                   !/*re        164*/
      zpr=dp0                                                                   !/*re        165*/
  190 continue                                                                  !/*re        166*/
#ifdef LCS                                                                      /*!re        167*/
      if (lelem.eq.-1) call revnt (3)                                           !/*re        168*/
      if (lelem.ne.-1) call revnt (1)                                           !/*re        169*/
#endif                                                                          /*!re        170*/
#ifdef OLDM                                                                     /*!re        171*/
      if (ncbal.gt.10) return                                                   !/*re        172*/
      casbal=ecl+exm(ityp)+waz1-ex-erec-waz2                                    !/*re        173*/
#else                                                                           /*!re        174*/
      casbal=ecl+pmass(ityp)+waz1-ex-erec-waz2                                  !/*re        175*/
#endif                                                                          /*!re        176*/
      if (nopart.le.0) go to 210                                                !/*re        177*/
      do 200 i=1,nopart                                                         !/*re        178*/
      it=kind(i)+1                                                              !/*re        179*/
#ifdef OLDM                                                                     /*!re        180*/
      casbal=casbal-ep(i)-exm(it)                                               !/*re        181*/
#else                                                                           /*!re        182*/
      casbal=casbal-ep(i)-pmass(it)                                             !/*re        183*/
#endif                                                                          /*!re        184*/
  200 continue                                                                  !/*re        185*/
  210 continue                                                                  !/*re        186*/
#ifdef OLDM                                                                     /*!re        187*/
      if (abs(casbal).gt.5.d-2) then                                            !/*re        188*/
         ncbal=ncbal+1                                                          !/*re        189*/
#else                                                                           /*!re        190*/
      if (abs(casbal).gt.ballim) then                                           !/*re        191*/
         ballim=abs(casbal)                                                     !/*re        192*/
         name=igetd(2)                                                          !/*re        193*/
#endif                                                                          /*!re        194*/
         write (io,270) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal               !/*re        195*/
         write (itty,270) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal             !/*re        196*/
      endif                                                                     !/*re        197*/
      return                                                                    !/*re        198*/
  220 negzpr=negzpr+1                                                           !/*re        199*/
      if (negzpr.gt.5) call expirx(1,'REBALB','negzpr.gt.5')                    !/*re        200*/
      negaz=negaz+1                                                             !/*re        201*/
      nrj=1                                                                     !/*re        202*/
      return                                                                    !/*re        203*/
  230 continue                                                                  !/*re        204*/
      lowaz=lowaz+1                                                             !/*re        205*/
      nrj=1                                                                     !/*re        206*/
      return                                                                    !/*re        207*/
  240 continue                                                                  !/*re        208*/
      negex=negex+1                                                             !/*re        209*/
      nrj=1                                                                     !/*re        210*/
      exlow(3)=min(exhold,exlow(3))                                             !/*re        211*/
      return                                                                    !/*re        212*/
!                                                                               !/*re        213*/
  260 format (' no valid recoil - nocas=',i6,' name=',i4,' apr=',f6.1,          !/*re        214*/
     & ' zpr=',f5.1)                                                            !/*re        215*/
  270 format (5x,' REBALB imbalance: nocas=',i6,' name',i4/' a1=',f5.0,         !/*re        216*/
     & ' z1=',f5.0,' waz1=',1e12.6/' a2=',f5.0,' z2=',f5.0,' waz2=',            !/*re        217*/
     & 1e12.6,' bal=',1e10.3)                                                   !/*re        218*/
      end                                                                       !/*re        219*/

!     $Id: rebali.F,v 1.5 2002/03/27 18:15:19 gwm Exp $                       !/*re          1*/
      subroutine rebali (ecl,name,a1,z1,nrj,negzpr)                             !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
#ifndef OLDM                                                                    /*!re          4*/
#include <lcs/phprm0.h>                                                         /*!re          5*/
#endif                                                                          /*!re          6*/
#include <lcs/lptS.h>                                                           /*!re          7*/
#include <lcs/inout.h>                                                          /*!re          8*/
#include <lcs/rucom.h>                                                          /*!re          9*/
#include <lcs/cacom.h>                                                          /*!re         10*/
#include <lcs/banka.h>                                                          /*!re         11*/
      dimension parts(lpt), wts(lpt)                                            !/*re         12*/
#ifdef OLDM                                                                     /*!re         13*/
      data wts /13*dp1,2*dph,7*dp1/                                             !/*re         14*/
      data enlim /5.d-2/                                                        !/*re         15*/
      data bindng /7.d0/                                                        !/*re         16*/
!     CHECK: pi+ mass increased by electron mass                                !/*re         17*/
!     CHECK: pi- mass decreased by electron mass                                !/*re         18*/
      data elmass, amumev /0.511d0,931.20793d0/                                 !/*re         19*/
      data ncbal /0/                                                            !/*re         20*/
#else                                                                           /*!re         21*/
      parameter (lptm=lpt-11)                                                   !/*re         22*/
      dimension fbar(lpt)                                                       !/*re         23*/
      data fbar/7*dp1,dp2,2*dp3,dp4,lptm*dp1/                                   !/*re         24*/
      data wts /lpt*dp1/                                                        !/*re         25*/
      data enlim /0.01d0/                                                       !/*re         26*/
      data ballim /1.0d-03/                                                     !/*re         27*/
      if (nopart.lt.0) call errorf('rebali.1')                                  !/*re         28*/
#endif                                                                          /*!re         29*/
      if (flim0.ge.dp0) then                                                    !/*re         30*/
         flim=flim0                                                             !/*re         31*/
      else                                                                      !/*re         32*/
#ifdef OLDM                                                                     /*!re         33*/
         flim=min(max(dp5/ecl,dp2m2),dp1)                                       !/*re         34*/
#else                                                                           /*!re         35*/
         if(ityp.ge.8.and.ityp.le.11) then                                      !/*re         36*/
            flim=0.3d0                                                          !/*re         37*/
         else                                                                   !/*re         38*/
            flim=min(max(dp5*fbar(ityp)/ecl,dp2m2),dp1)                         !/*re         39*/
         endif                                                                  !/*re         40*/
#endif                                                                          /*!re         41*/
      endif                                                                     !/*re         42*/
#ifdef OLDM                                                                     /*!re         43*/
      waz1=energy(a1,z1)                                                        !/*re         44*/
#else                                                                           /*!re         45*/
      waz1=xmnuc(a1,z1)                                                         !/*re         46*/
#endif                                                                          /*!re         47*/
      nnnn=0                                                                    !/*re         48*/
      sume=dp0                                                                  !/*re         49*/
      summ=dp0                                                                  !/*re         50*/
      zpr=z1+charge(ityp)                                                       !/*re         51*/
      do 10 i=1,lpt                                                             !/*re         52*/
   10 parts(i)=dp0                                                              !/*re         53*/
      if (nopart.eq.0) go to 30                                                 !/*re         54*/
      do 20 i=1,nopart                                                          !/*re         55*/
      lk=kind(i)+1                                                              !/*re         56*/
      zpr=zpr-charge(lk)*wts(lk)                                                !/*re         57*/
      parts(lk)=parts(lk)+wts(lk)                                               !/*re         58*/
      summ=summ+pmass(lk)*wts(lk)                                               !/*re         59*/
      sume=sume+ep(i)*wts(lk)                                                   !/*re         60*/
   20 continue                                                                  !/*re         61*/
!.......................................................................        !/*re         62*/
! energy balancing                                                              !/*re         63*/
   30 continue                                                                  !/*re         64*/
      apr=a1-parts(1)-parts(2)+parts(17)+parts(18)                              !/*re         65*/
      apr=apr-dp2*parts(8)-dp3*(parts(9)+parts(10))-dp4*parts(11)               !/*re         66*/
      if (ityp.le.2) then                                                       !/*re         67*/
         apr=apr+dp1                                                            !/*re         68*/
      elseif (ityp.eq.8) then                                                   !/*re         69*/
         apr=apr+dp2                                                            !/*re         70*/
      elseif (ityp.eq.11) then                                                  !/*re         71*/
         apr=apr+dp4                                                            !/*re         72*/
      elseif (ityp.eq.9.or.ityp.eq.10) then                                     !/*re         73*/
         apr=apr+dp3                                                            !/*re         74*/
      elseif (ityp.ge.17) then                                                  !/*re         75*/
         apr=apr-dp1                                                            !/*re         76*/
      endif                                                                     !/*re         77*/
      if (zpr.lt.dp0.or.apr.lt.zpr) then                                        !/*re         78*/
         negzpr=negzpr+1                                                        !/*re         79*/
         if (negzpr.gt.5) call expirx(1,'REBALI','negzpr.gt.5')                 !/*re         80*/
         negaz=negaz+1                                                          !/*re         81*/
         nrj=1                                                                  !/*re         82*/
         return                                                                 !/*re         83*/
      endif                                                                     !/*re         84*/
      if ((apr.eq.zpr.or.zpr.eq.dp0).and.apr.gt.dp3) then                       !/*re         85*/
         lowaz=lowaz+1                                                          !/*re         86*/
         nrj=1                                                                  !/*re         87*/
         return                                                                 !/*re         88*/
      endif                                                                     !/*re         89*/
      if (apr.eq.dp0) then                                                      !/*re         90*/
         waz2=dp0                                                               !/*re         91*/
      else                                                                      !/*re         92*/
#ifdef OLDM                                                                     /*!re         93*/
         waz2=energy(apr,zpr)                                                   !/*re         94*/
#else                                                                           /*!re         95*/
         waz2=xmnuc(apr,zpr)                                                    !/*re         96*/
#endif                                                                          /*!re         97*/
      endif                                                                     !/*re         98*/
!                                                                               !/*re         99*/
#ifdef OLDM                                                                     /*!re        100*/
      ext=waz1-waz2+(a1-apr)*amumev-(z1-zpr)*elmass+ecl+pmass(ityp)-summ        !/*re        101*/
      waz0=waz2+apr*amumev                                                      !/*re        102*/
#else                                                                           /*!re        103*/
      ext=waz1-waz2+ecl+pmass(ityp)-summ                                        !/*re        104*/
!                                                                               !/*re        105*/
      if (apr.ge.dp1.and.ext.lt.sume) then                                      !/*re        106*/
         nrj=1                                                                  !/*re        107*/
         return                                                                 !/*re        108*/
      endif                                                                     !/*re        109*/
#endif                                                                          /*!re        110*/
!                                                                               !/*re        111*/
      if (nopart.eq.0.or.flim.eq.dp0.or.apr.eq.dp1) go to 60                    !/*re        112*/
!.......................................................................        !/*re        113*/
#ifdef OLDM                                                                     /*!re        114*/
      if (nnuc.eq.0.or.apr.le.dp1) then                                         !/*re        115*/
#else                                                                           /*!re        116*/
      if (nnuc.eq.0) then                                                       !/*re        117*/
#endif                                                                          /*!re        118*/
         ex=dp0                                                                 !/*re        119*/
      elseif (nnuc.gt.dp0) then                                                 !/*re        120*/
         ex=exn(nnuc)                                                           !/*re        121*/
      else                                                                      !/*re        122*/
#ifdef OLDM                                                                     /*!re        123*/
         ex=ecl-sume                                                            !/*re        124*/
         do 40 i=3,16                                                           !/*re        125*/
         ex=ex-pmass(i)*parts(i)                                                !/*re        126*/
   40    continue                                                               !/*re        127*/
         if (ityp.ge.3.and.ityp.le.5) then                                      !/*re        128*/
            ex=ex+pmass(ityp)                                                   !/*re        129*/
         elseif (ityp.ge.13.and.ityp.le.16) then                                !/*re        130*/
            ex=ex+pmass(ityp)                                                   !/*re        131*/
         elseif (ityp.ge.17) then                                               !/*re        132*/
            ex=ex+(pmass(1)+pmass(2))*(dp1-parts(17)-parts(18))                 !/*re        133*/
         endif                                                                  !/*re        134*/
         ex=ex-bindng*(a1-apr)                                                  !/*re        135*/
#else                                                                           /*!re        136*/
         call errorf ('rebali.2')                                               !/*re        137*/
#endif                                                                          /*!re        138*/
      endif                                                                     !/*re        139*/
!                                                                               !/*re        140*/
      if (apr.eq.dp0) then                                                      !/*re        141*/
         erec=dp0                                                               !/*re        142*/
#ifndef OLDM                                                                    /*!re        143*/
         ex=dp0                                                                 !/*re        144*/
#endif                                                                          /*!re        145*/
      else                                                                      !/*re        146*/
#ifdef OLDM                                                                     /*!re        147*/
         waz=waz0+ext-sume                                                      !/*re        148*/
#else                                                                           /*!re        149*/
         waz=waz2+ext-sume                                                      !/*re        150*/
#endif                                                                          /*!re        151*/
         call recoil (ecl,waz,1)                                                !/*re        152*/
         if (erec.lt.dp0) go to 90                                              !/*re        153*/
#ifdef OLDM                                                                     /*!re        154*/
         if (nexite.ne.0) then                                                  !/*re        155*/
            ex=max(ex-erec,dp0)                                                 !/*re        156*/
         else                                                                   !/*re        157*/
            ex=max(ex,dp0)                                                      !/*re        158*/
         endif                                                                  !/*re        159*/
#else                                                                           /*!re        160*/
         ex=ext-erec-sume                                                       !/*re        161*/
#endif                                                                          /*!re        162*/
      endif                                                                     !/*re        163*/
#ifdef OLDM                                                                     /*!re        164*/
      if (apr.gt.dp0.and.ex.le.dp0) go to 60                                    !/*re        165*/
#else                                                                           /*!re        166*/
      if (apr.gt.dp0.and.ex.lt.dp0) go to 60                                    !/*re        167*/
#endif                                                                          /*!re        168*/
      fac=ext/(ex+erec+sume)                                                    !/*re        169*/
      if (abs(fac-dp1).gt.flim) then                                            !/*re        170*/
#ifdef OLDM                                                                     /*!re        171*/
         if (apr.eq.dp0) then                                                   !/*re        172*/
#else                                                                           /*!re        173*/
         if (apr.le.dp1) then                                                   !/*re        174*/
#endif                                                                          /*!re        175*/
            nrj=1                                                               !/*re        176*/
            return                                                              !/*re        177*/
         else                                                                   !/*re        178*/
            go to 60                                                            !/*re        179*/
         endif                                                                  !/*re        180*/
      endif                                                                     !/*re        181*/
!                                                                               !/*re        182*/
      ex=fac*ex                                                                 !/*re        183*/
      erec=fac*erec                                                             !/*re        184*/
      do 50 i=1,nopart                                                          !/*re        185*/
   50 ep(i)=fac*ep(i)                                                           !/*re        186*/
      go to 120                                                                 !/*re        187*/
!.......................................................................        !/*re        188*/
   60 continue                                                                  !/*re        189*/
#ifdef OLDM                                                                     /*!re        190*/
      if (apr.eq.dp1) then                                                      !/*re        191*/
         waz=waz0                                                               !/*re        192*/
         call recoil (ecl,waz,0)                                                !/*re        193*/
      else                                                                      !/*re        194*/
         waz=waz0+ext-sume                                                      !/*re        195*/
         call recoil (ecl,waz,1)                                                !/*re        196*/
      endif                                                                     !/*re        197*/
#else                                                                           /*!re        198*/
      if (apr.eq.dp1) then                                                      !/*re        199*/
         waz=waz2                                                               !/*re        200*/
         call recoil (ecl,waz,0)                                                !/*re        201*/
      else                                                                      !/*re        202*/
         waz=waz2+ext-sume                                                      !/*re        203*/
         call recoil (ecl,waz,1)                                                !/*re        204*/
      endif                                                                     !/*re        205*/
#endif                                                                          /*!re        206*/
      if (erec.lt.dp0) go to 90                                                 !/*re        207*/
      ex=ext-erec-sume                                                          !/*re        208*/
      if (apr.eq.dp1) then                                                      !/*re        209*/
         nrnorm(4)=nrnorm(4)+1                                                  !/*re        210*/
      else                                                                      !/*re        211*/
         if (ex.ge.dp0) go to 120                                               !/*re        212*/
      endif                                                                     !/*re        213*/
      if (nopart.le.0) then                                                     !/*re        214*/
         negex=negex+1                                                          !/*re        215*/
         nopart=-1                                                              !/*re        216*/
         return                                                                 !/*re        217*/
      endif                                                                     !/*re        218*/
      exhold=-abs(ex)                                                           !/*re        219*/
      factot=dp1                                                                !/*re        220*/
      if (abs(ex).le.enlim) go to 110                                           !/*re        221*/
      if (flim.eq.dp0) go to 100                                                !/*re        222*/
!.......................................................................        !/*re        223*/
! renormalization loop - iterates to zero excitation                            !/*re        224*/
   70 continue                                                                  !/*re        225*/
      fac=dp1+ex/sume                                                           !/*re        226*/
      fac=min(max(dp9m1,fac),dp11m1)                                            !/*re        227*/
      factot=factot*fac                                                         !/*re        228*/
      do 80 i=1,nopart                                                          !/*re        229*/
   80 ep(i)=fac*ep(i)                                                           !/*re        230*/
      sume=fac*sume                                                             !/*re        231*/
#ifdef OLDM                                                                     /*!re        232*/
      waz=waz0                                                                  !/*re        233*/
#else                                                                           /*!re        234*/
      waz=waz2                                                                  !/*re        235*/
#endif                                                                          /*!re        236*/
      call recoil (ecl,waz,0)                                                   !/*re        237*/
      if (erec.lt.dp0) go to 90                                                 !/*re        238*/
      ex=ext-sume-erec                                                          !/*re        239*/
      nnnn=nnnn+1                                                               !/*re        240*/
      if (abs(ex).le.enlim.and.abs(factot-dp1).le.flim) go to 110               !/*re        241*/
      if (nnnn.ge.10) then                                                      !/*re        242*/
         nrnorm(2)=nrnorm(2)+1                                                  !/*re        243*/
         go to 100                                                              !/*re        244*/
      endif                                                                     !/*re        245*/
      go to 70                                                                  !/*re        246*/
!.......................................................................        !/*re        247*/
! reject collision - low energy                                                 !/*re        248*/
   90 continue                                                                  !/*re        249*/
      name=igetd(2)                                                             !/*re        250*/
      write (io,160) nocas,name,apr,zpr                                         !/*re        251*/
      write (itty,160) nocas,name,apr,zpr                                       !/*re        252*/
  160 format (' no valid recoil - nocas=',i6,' name=',i4,' apr=',f6.1,          !/*re        253*/
     & ' zpr=',f5.1)                                                            !/*re        254*/
      exhold=dp0                                                                !/*re        255*/
  100 continue                                                                  !/*re        256*/
      negex=negex+1                                                             !/*re        257*/
      nrj=1                                                                     !/*re        258*/
      exlow(3)=min(exhold,exlow(3))                                             !/*re        259*/
      return                                                                    !/*re        260*/
!.......................................................................        !/*re        261*/
! allowed renormalization                                                       !/*re        262*/
  110 continue                                                                  !/*re        263*/
      exlow(2)=min(ex,exlow(2))                                                 !/*re        264*/
      exlow(4)=max(ex,exlow(4))                                                 !/*re        265*/
      exlow(1)=min(exhold,exlow(1))                                             !/*re        266*/
      exlow(5)=exlow(5)+ex                                                      !/*re        267*/
      erec=max(erec+ex,dp0)                                                     !/*re        268*/
      ex=dp0                                                                    !/*re        269*/
      nrnorm(1)=nrnorm(1)+1                                                     !/*re        270*/
      nrnorm(3)=nrnorm(3)+nnnn                                                  !/*re        271*/
      if (apr.eq.dp1) then                                                      !/*re        272*/
         nopart=nopart+1                                                        !/*re        273*/
         ep(nopart)=erec                                                        !/*re        274*/
         kind(nopart)=dp1-zpr                                                   !/*re        275*/
         alpha(nopart)=coslbr(1)                                                !/*re        276*/
         beta(nopart)=coslbr(2)                                                 !/*re        277*/
         gam(nopart)=coslbr(3)                                                  !/*re        278*/
         ex=dp0                                                                 !/*re        279*/
         erec=dp0                                                               !/*re        280*/
         waz2=dp0                                                               !/*re        281*/
         apr=dp0                                                                !/*re        282*/
         zpr=dp0                                                                !/*re        283*/
      endif                                                                     !/*re        284*/
  120 continue                                                                  !/*re        285*/
#ifdef LCS                                                                      /*!re        286*/
      if (lelem.eq.-1) call revnt (3)                                           !/*re        287*/
      if (lelem.ne.-1) call revnt (1)                                           !/*re        288*/
#endif                                                                          /*!re        289*/
#ifdef OLDM                                                                     /*!re        290*/
      if (ncbal.gt.10) return                                                   !/*re        291*/
      casbal=ecl+pmass(ityp)+waz1+a1*amumev-ex-erec-waz2-apr*amumev             !/*re        292*/
      casbal=casbal-(z1-zpr)*elmass                                             !/*re        293*/
#else                                                                           /*!re        294*/
      casbal=ecl+pmass(ityp)+waz1-ex-erec-waz2                                  !/*re        295*/
#endif                                                                          /*!re        296*/
      if (nopart.le.0) go to 140                                                !/*re        297*/
      do 130 i=1,nopart                                                         !/*re        298*/
      it=kind(i)+1                                                              !/*re        299*/
      casbal=casbal-(ep(i)+pmass(it))*wts(it)                                   !/*re        300*/
  130 continue                                                                  !/*re        301*/
  140 continue                                                                  !/*re        302*/
#ifdef OLDM                                                                     /*!re        303*/
      if (abs(casbal).gt.5.d-2) then                                            !/*re        304*/
         ncbal=ncbal+1                                                          !/*re        305*/
         write (io,170) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal               !/*re        306*/
         write (itty,170) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal             !/*re        307*/
      endif                                                                     !/*re        308*/
  170 format (5x,' cascad imbalance: nocas=',i6,' name',i4/' a1=',f5.0,'        !/*re        309*/
     & z1=',f5.0,' exm1=',1e12.6/' a2=',f5.0,' z2=',f5.0,' exm2=',1e12.6        !/*re        310*/
     & ,' bal=',1e10.3)                                                         !/*re        311*/
#else                                                                           /*!re        312*/
      if (abs(casbal).gt.ballim) then                                           !/*re        313*/
         ballim=abs(casbal)                                                     !/*re        314*/
         name=igetd(2)                                                          !/*re        315*/
         write (io,170) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal               !/*re        316*/
         write (itty,170) nocas,name,a1,z1,waz1,apr,zpr,waz2,casbal             !/*re        317*/
      endif                                                                     !/*re        318*/
  170 format (5x,' REBALI imbalance: nocas=',i6,' name',i4/' a1=',f5.0,'        !/*re        319*/
     & z1=',f5.0,' waz1=',1e12.6/' a2=',f5.0,' z2=',f5.0,' waz2=',1e12.6        !/*re        320*/
     & ,' bal=',1e10.3)                                                         !/*re        321*/
#endif                                                                          /*!re        322*/
!                                                                               !/*re        323*/
      return                                                                    !/*re        324*/
      end                                                                       !/*re        325*/

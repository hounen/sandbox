!     $Id: bcasc.F,v 1.5 2002/03/27 18:14:54 gwm Exp $                        !/*bc          1*/
      subroutine bcasc (ecl,med,emn,a1,z1)                                      !/*bc          2*/
#include <lcs/double.h>                                                         /*!bc          3*/
!     ==================================================================        !/*bc          4*/
!     bcasc    calls the following subroutines and functions                    !/*bc          5*/
!        BERT       ENERGY     ERROR      PDEVNT     NOBAL      SKALE           !/*bc          6*/
!                   REBAL                                                       !/*bc          7*/
!     ==================================================================        !/*bc          8*/
#ifdef OLDM                                                                     /*!bc          9*/
      parameter (pi=3.141593d0, hbar=6.581d-22, clite=2.9979d+10, cmtofm        !/*bc         10*/
     & =1.0d-13, fsc=dp1/137.04d0, hbarc=hbar*clite, rcoul=1.70d0, conce        !/*bc         11*/
     & =fsc*hbarc/(rcoul*cmtofm), conar=dph*fsc*cmtofm*rcoul/hbarc,             !/*bc         12*/
     & cmtomb=1.0d+27, conem=dp1/(cmtomb*hbar*(pi*hbarc)**2), conit=pi          !/*bc         13*/
     & /hbar, conrad=hbarc/cmtofm, sqrt2=1.414214d0, amu=931.20793d0,           !/*bc         14*/
     & fmtomb=1.d-1, tenpi=dp10*pi)                                             !/*bc         15*/
#else                                                                           /*!bc         16*/
#include <lcs/phprm0.h>                                                         /*!bc         17*/
#include <lcs/phprm1.h>                                                         /*!bc         18*/
#endif                                                                          /*!bc         19*/
#include <lcs/inout.h>                                                          /*!bc         20*/
#include <lcs/lptS.h>                                                           /*!bc         21*/
#include <lcs/rucom.h>                                                          /*!bc         22*/
#include <lcs/cacom.h>                                                          /*!bc         23*/
#include <lcs/banka.h>                                                          /*!bc         24*/
      dimension f(8), prd(5)                                                    !/*bc         25*/
      dimension iix(6)                                                          !/*bc         26*/
      data iix /1,0,7,8,9,10/, maxstg /10/                                      !/*bc         27*/
      data limit, limit1 /1000,1000/                                            !/*bc         28*/
#ifdef OLDM                                                                     /*!bc         29*/
      data amumev /931.20793d0/                                                 !/*bc         30*/
#endif                                                                          /*!bc         31*/
      tip=ityp-1                                                                !/*bc         32*/
      nrj=0                                                                     !/*bc         33*/
      nnuc=-1                                                                   !/*bc         34*/
      negzpr=0                                                                  !/*bc         35*/
      nrjpi=0                                                                   !/*bc         36*/
   10 continue                                                                  !/*bc         37*/
      f(1)=a1                                                                   !/*bc         38*/
      f(2)=z1                                                                   !/*bc         39*/
      f(3)=ecl                                                                  !/*bc         40*/
      f(5)=dp1                                                                  !/*bc         41*/
      f(6)=andit9                                                               !/*bc         42*/
      f(7)=tip                                                                  !/*bc         43*/
      if (ctofe9.lt.dp0) then                                                   !/*bc         44*/
        rad=rmass(int(a1))                                                      !/*bc         45*/
#ifdef OLDM                                                                     /*!bc         46*/
        arg0=conar*z1*rad*amu                                                   !/*bc         47*/
#else                                                                           /*!bc         48*/
        arg0=conar*z1*rad*amuo16                                                !/*bc         49*/
#endif                                                                          /*!bc         50*/
        fk=clmb3(arg0)                                                          !/*bc         51*/
! original Bertini coulomb energy                                               !/*bc         52*/
!        cmbeng=(z1/crsc(4+(int(a1)-1)*10))*1.4412d-13                          !/*bc         53*/
!                                                                               !/*bc         54*/
        cmbeng=(conce*fk*z1)/rad                                                !/*bc         55*/
        fac=dp3                                                                 !/*bc         56*/
        cmbbar=fac*cmbeng/(dp1+(fac/fk-dp1)*rang())                             !/*bc         57*/
        if (a1.le.dp3) then                                                     !/*bc         58*/
          csa5=(z1*energy(dp1,dp1)+(a1-z1)*energy(dp1,dp0)-energy(a1,z1)        !/*bc         59*/
     &    )/a1                                                                  !/*bc         60*/
        else                                                                    !/*bc         61*/
          csa5=dph*(energy(dp1,dp1)+energy(a1-dp1,z1-dp1)+energy(dp1,dp0        !/*bc         62*/
     &    )+energy(a1-dp1,z1))-energy(a1,z1)                                    !/*bc         63*/
        endif                                                                   !/*bc         64*/
        f(4)=max(dp2*csa5*rang(),cmbbar)                                        !/*bc         65*/
        f(8)=dp2*csa5*rang()                                                    !/*bc         66*/
      else                                                                      !/*bc         67*/
        f(4)=ctofe9                                                             !/*bc         68*/
        f(8)=ctofn9                                                             !/*bc         69*/
      endif                                                                     !/*bc         70*/
      nrj1=0                                                                    !/*bc         71*/
      if (ihie.ne.0) go to 40                                                   !/*bc         72*/
   20 continue                                                                  !/*bc         73*/
      if (nrj.gt.0) nrj1=nrj1+1                                                 !/*bc         74*/
      if (nrj1.gt.limit1) go to 120                                             !/*bc         75*/
      call bert (f(1),nopart,kind,ep,alpha,beta,gam)                            !/*bc         76*/
      if (nopart.eq.1.and.(ityp+kind(1)).eq.2) then                             !/*bc         77*/
        if (ityp.eq.1) then                                                     !/*bc         78*/
          diff=energy(a1,z1+dp1)+energy(dp1,dp0)-energy(dp1,dp1)                !/*bc         79*/
        else                                                                    !/*bc         80*/
          diff=energy(a1,z1-dp1)+energy(dp1,dp1)-energy(dp1,dp0)                !/*bc         81*/
        endif                                                                   !/*bc         82*/
        diff=diff-energy(a1,z1)                                                 !/*bc         83*/
        ep(1)=ep(1)-diff                                                        !/*bc         84*/
        if (kind(1).eq.0.and.ep(1).lt.f(4)) nopart=0                            !/*bc         85*/
        if (kind(1).eq.1.and.ep(1).lt.f(8)) nopart=0                            !/*bc         86*/
      endif                                                                     !/*bc         87*/
      if (nopart.ge.0) then                                                     !/*bc         88*/
        nrj=0                                                                   !/*bc         89*/
        if (nobalc.eq.0.or.nopart.eq.0) then                                    !/*bc         90*/
          call rebalb (ecl,name,a1,z1,nrj,negzpr)                               !/*bc         91*/
          if (nopart.lt.0) go to 30                                             !/*bc         92*/
        else                                                                    !/*bc         93*/
          call nobal (ecl,a1,z1,nrj,negzpr)                                     !/*bc         94*/
! nopart may be reset by NOBAL                                                  !/*bc         95*/
          if (nopart.eq.0) call rebalb (ecl,name,a1,z1,nrj,negzpr)              !/*bc         96*/
          if (nopart.lt.0) go to 30                                             !/*bc         97*/
        endif                                                                   !/*bc         98*/
        iprqa=ipreq                                                             !/*bc         99*/
        if (iprqa.eq.2) then                                                    !/*bc        100*/
          rt=25.d0/ecl                                                          !/*bc        101*/
          r1=rang()                                                             !/*bc        102*/
          iprqa=1                                                               !/*bc        103*/
          if (r1.lt.rt) iprqa=3                                                 !/*bc        104*/
        endif                                                                   !/*bc        105*/
        if (iprqa.eq.3) then                                                    !/*bc        106*/
          ipr0=3-ityp                                                           !/*bc        107*/
          nnout=a1+ia6(ipr0)-apr+1.d-02                                         !/*bc        108*/
          npiout=nopart-nnout                                                   !/*bc        109*/
          if (npiout.eq.0) then                                                 !/*bc        110*/
            nopart=0                                                            !/*bc        111*/
            call rebalb (ecl,name,a1,z1,nrj,negzpr)                             !/*bc        112*/
            if (nopart.lt.0) go to 30                                           !/*bc        113*/
          endif                                                                 !/*bc        114*/
        endif                                                                   !/*bc        115*/
        if (nrj.gt.0) go to 20                                                  !/*bc        116*/
        if (iprqa.ne.0) go to 90                                                !/*bc        117*/
        go to 80                                                                !/*bc        118*/
      endif                                                                     !/*bc        119*/
!.......................................................................        !/*bc        120*/
! no interaction - pseudocollision                                              !/*bc        121*/
   30 continue                                                                  !/*bc        122*/
      if (ityp.ne.5.or.ecl.gt.emn) then                                         !/*bc        123*/
        if (nrj.eq.1) go to 20                                                  !/*bc        124*/
        go to 80                                                                !/*bc        125*/
      endif                                                                     !/*bc        126*/
! resample pi- capture                                                          !/*bc        127*/
      nrjpi=nrjpi+1                                                             !/*bc        128*/
!                                                                               !/*bc        129*/
!     An attempt to avoid a near-infinite loop if the pion energy is            !/*bc        130*/
!     too close to zero.  However, the energy at which the collision            !/*bc        131*/
!     is calculated should not rise above EMN.                                  !/*bc        132*/
      if (ecl.lt.emn) ecl=min(ecl+dp1m2,emn)                                    !/*bc        133*/
!                                                                               !/*bc        134*/
      if (nrjpi.le.limit) go to 10                                              !/*bc        135*/
      irejpi=1                                                                  !/*bc        136*/
      write (io,130) limit,med,ecl,z1,a1                                        !/*bc        137*/
      write (itty,130) limit,med,ecl,z1,a1                                      !/*bc        138*/
      call info                                                                 !/*bc        139*/
      go to 80                                                                  !/*bc        140*/
!.......................................................................        !/*bc        141*/
   40 continue                                                                  !/*bc        142*/
! high energy interaction                                                       !/*bc        143*/
      if (nrj.gt.0) nrj1=nrj1+1                                                 !/*bc        144*/
      if (nrj1.gt.limit1) go to 120                                             !/*bc        145*/
      ehicut=ehipi                                                              !/*bc        146*/
      if (ityp.le.2) ehicut=ehin                                                !/*bc        147*/
      call skale (f,nopart,kind,ep,alpha,beta,gam,ehicut,wtfas,rmfas            !/*bc        148*/
     & ,exfas,refas)                                                            !/*bc        149*/
      if (nopart.lt.0) go to 80                                                 !/*bc        150*/
! real event                                                                    !/*bc        151*/
#ifdef LCS                                                                      /*!bc        152*/
      call revnt (8)                                                            !/*bc        153*/
#endif                                                                          /*!bc        154*/
      if (nopart.eq.0) then                                                     !/*bc        155*/
        call rebalb (ecl,name,a1,z1,nrj,negzpr)                                 !/*bc        156*/
        if (nrj.eq.1) go to 20                                                  !/*bc        157*/
        go to 80                                                                !/*bc        158*/
      endif                                                                     !/*bc        159*/
! productive event                                                              !/*bc        160*/
#ifdef LCS                                                                      /*!bc        161*/
      call revnt (1)                                                            !/*bc        162*/
#endif                                                                          /*!bc        163*/
      do 50 it=1,5                                                              !/*bc        164*/
   50 prd(it)=dp0                                                               !/*bc        165*/
      do 60 j=1,nopart                                                          !/*bc        166*/
      it=kind(j)+1                                                              !/*bc        167*/
   60 prd(it)=prd(it)+wtfas(j)                                                  !/*bc        168*/
! compute residual z                                                            !/*bc        169*/
      if (ityp.gt.2) then                                                       !/*bc        170*/
        zdum=dp3                                                                !/*bc        171*/
      else                                                                      !/*bc        172*/
        zdum=dp1                                                                !/*bc        173*/
      endif                                                                     !/*bc        174*/
      tmp=z1+zdum-tip-prd(1)-prd(3)+prd(5)                                      !/*bc        175*/
      zpr=int(tmp+dph)                                                          !/*bc        176*/
! compute residual a                                                            !/*bc        177*/
      aaa=a1-prd(1)-prd(2)                                                      !/*bc        178*/
      if (ityp.le.2) aaa=aaa+dp1                                                !/*bc        179*/
      apr=int(aaa+dph)                                                          !/*bc        180*/
      if (apr.lt.zpr.or.zpr.lt.dp0) then                                        !/*bc        181*/
        negzpr=negzpr+1                                                         !/*bc        182*/
        if (negzpr.gt.5) call expirx(1,'BCASC','negzpr.gt.5')                   !/*bc        183*/
        go to 70                                                                !/*bc        184*/
      endif                                                                     !/*bc        185*/
      if (apr.le.dp1) nrnorm(4)=nrnorm(4)+1                                     !/*bc        186*/
      if (apr.eq.dp0) then                                                      !/*bc        187*/
        ex=dp0                                                                  !/*bc        188*/
        erec=dp0                                                                !/*bc        189*/
        go to 80                                                                !/*bc        190*/
      elseif ((apr.eq.zpr.or.zpr.eq.dp0).and.apr.gt.dp3) then                   !/*bc        191*/
        lowaz=lowaz+1                                                           !/*bc        192*/
        go to 70                                                                !/*bc        193*/
      endif                                                                     !/*bc        194*/
      ex=exfas                                                                  !/*bc        195*/
      if (ex.lt.dp0) call expirx(1,'BCASC','ex.lt.dp0')                         !/*bc        196*/
#ifdef OLDM                                                                     /*!bc        197*/
      waz=amumev*apr+ex                                                         !/*bc        198*/
#else                                                                           /*!bc        199*/
      waz=amuo16*apr+ex                                                         !/*bc        200*/
#endif                                                                          /*!bc        201*/
      call hrc (ecl,waz,0,erek)                                                 !/*bc        202*/
      erec=refas                                                                !/*bc        203*/
      go to 80                                                                  !/*bc        204*/
!.......................................................................        !/*bc        205*/
! reject collision - high energy                                                !/*bc        206*/
   70 continue                                                                  !/*bc        207*/
      nrj=1                                                                     !/*bc        208*/
#ifdef LCS                                                                      /*!bc        209*/
      call revnt (-1)                                                           !/*bc        210*/
      call revnt (-8)                                                           !/*bc        211*/
#endif                                                                          /*!bc        212*/
      go to 40                                                                  !/*bc        213*/
!.......................................................................        !/*bc        214*/
   80 continue                                                                  !/*bc        215*/
      exc0=ex                                                                   !/*bc        216*/
      return                                                                    !/*bc        217*/
   90 continue                                                                  !/*bc        218*/
      if (apr.le.dp5) go to 80                                                  !/*bc        219*/
      if (ityp.gt.2.or.ityp.lt.1) go to 80                                      !/*bc        220*/
      if (ex.le.dp0) go to 80                                                   !/*bc        221*/
!                                                                               !/*bc        222*/
!     INITIALIZE EMITTING NUCLEUS - PREEQ                                       !/*bc        223*/
!                                                                               !/*bc        224*/
      exc0=ex                                                                   !/*bc        225*/
      ja=apr                                                                    !/*bc        226*/
      jz=zpr                                                                    !/*bc        227*/
#ifdef OLDM                                                                     /*!bc        228*/
      ex0=energy(apr,zpr)                                                       !/*bc        229*/
      waz=ex0+amumev*apr                                                        !/*bc        230*/
#else                                                                           /*!bc        231*/
      waz=xmnuc(apr,zpr)                                                        !/*bc        232*/
#endif                                                                          /*!bc        233*/
      gamma9=dp1+erec/(waz+ex)                                                  !/*bc        234*/
      ipr0=3-ityp                                                               !/*bc        235*/
      nnout=a1+ia6(ipr0)-apr+1.d-02                                             !/*bc        236*/
      npiout=nopart-nnout                                                       !/*bc        237*/
      nstage=max(nnout,npiout)                                                  !/*bc        238*/
      if (nstage.eq.0) then                                                     !/*bc        239*/
#ifdef OLDM                                                                     /*!bc        240*/
        rnmas0=energy(a1,z1)+amumev*a1                                          !/*bc        241*/
#else                                                                           /*!bc        242*/
        rnmas0=xmnuc(a1,z1)                                                     !/*bc        243*/
#endif                                                                          /*!bc        244*/
        temp=sqrt((rnmas0+zmass9(ipr0))**2+dp2*rnmas0*ecl)                      !/*bc        245*/
        eps0=temp-rnmas0-zmass9(ipr0)                                           !/*bc        246*/
      endif                                                                     !/*bc        247*/
      np=ia6(ipr0)+1-nnout                                                      !/*bc        248*/
      if (nopart.eq.0.and.iprqa.eq.3) then                                      !/*bc        249*/
        nh=1                                                                    !/*bc        250*/
      else                                                                      !/*bc        251*/
        if (np.le.0) then                                                       !/*bc        252*/
          nh=2-np                                                               !/*bc        253*/
          np=1                                                                  !/*bc        254*/
        else                                                                    !/*bc        255*/
          nh=2                                                                  !/*bc        256*/
          np=np+1                                                               !/*bc        257*/
        endif                                                                   !/*bc        258*/
      endif                                                                     !/*bc        259*/
  100 continue                                                                  !/*bc        260*/
!                                                                               !/*bc        261*/
!     ATTEMPT EMISSION                                                          !/*bc        262*/
!                                                                               !/*bc        263*/
      nstage=nstage+1                                                           !/*bc        264*/
      ipr=0                                                                     !/*bc        265*/
#ifndef OLDM                                                                    /*!bc        266*/
      ex0=energy(apr,zpr)                                                       !/*bc        267*/
#endif                                                                          /*!bc        268*/
      call pclct (apr,zpr,ja,jz,ex,np,nh,a1,z1,ipr0,erec)                       !/*bc        269*/
      if (ipr.eq.0) return                                                      !/*bc        270*/
!                                                                               !/*bc        271*/
!         BINNING EMITTED PARTICLE                                              !/*bc        272*/
!                                                                               !/*bc        273*/
      nopart=nopart+1                                                           !/*bc        274*/
      i=nopart                                                                  !/*bc        275*/
      ep(i)=eps9                                                                !/*bc        276*/
      alpha(i)=coslbp(1)                                                        !/*bc        277*/
      beta(i)=coslbp(2)                                                         !/*bc        278*/
      gam(i)=coslbp(3)                                                          !/*bc        279*/
      kind(i)=iix(ipr)+200                                                      !/*bc        280*/
      if (nstage.lt.maxstg) go to 100                                           !/*bc        281*/
      return                                                                    !/*bc        282*/
  120 continue                                                                  !/*bc        283*/
      write (io,140) limit1,nocas                                               !/*bc        284*/
      write (itty,140) limit1,nocas                                             !/*bc        285*/
      call info                                                                 !/*bc        286*/
      write (io,*) ' a,z  ',a1,z1                                               !/*bc        287*/
      nrj=0                                                                     !/*bc        288*/
      nopart=-1                                                                 !/*bc        289*/
      if (ihie.ne.0) go to 80                                                   !/*bc        290*/
      go to 30                                                                  !/*bc        291*/
!                                                                               !/*bc        292*/
  130 format ('BCASC: unable to absorb pi- after',i5,                           !/*bc        293*/
     & ' tries in med ',i4,' e=',e10.2,' z=',f5.0,' a=',f6.0)                   !/*bc        294*/
  140 format ('BCASC: unable to resample interaction after',i5,                 !/*bc        295*/
     & ' tries for nocas =',i10)                                                !/*bc        296*/
      end                                                                       !/*bc        297*/

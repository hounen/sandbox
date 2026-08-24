!     $Id: hcasc.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                        !/*hc          1*/
      subroutine hcasc (ecl,med,emn,a1,z1,idt,lmc)                              !/*hc          2*/
#include <lcs/double.h>                                                         /*!hc          3*/
!     ==================================================================        !/*hc          4*/
!     hcasc    calls the following subroutines and functions                    !/*hc          5*/
!     ==================================================================        !/*hc          6*/
!            tip(hetc)           csa1(isabel)                                   !/*hc          7*/
!                                                                               !/*hc          8*/
!     proton    0.                   1.0                                        !/*hc          9*/
!     neutron   1.                   2.0                                        !/*hc         10*/
!     pi+       2.                   3.0                                        !/*hc         11*/
!     pi0       3.                   4.0                                        !/*hc         12*/
!     pi-       4.                   5.0                                        !/*hc         13*/
!     mu+       5.                    -                                         !/*hc         14*/
!     mu-       6.                    -                                         !/*hc         15*/
!     d         7.                    -                                         !/*hc         16*/
!     t         8.                    -                                         !/*hc         17*/
!     he-3      9.                    -                                         !/*hc         18*/
!     alpha    10.                    -                                         !/*hc         19*/
!     photon   11.                    -                                         !/*hc         20*/
!     k+       12.                   7.0                                        !/*hc         21*/
!     k0-long  13.                 (k0,k0=8.0)                                  !/*hc         22*/
!     k0-short 14.                 (k0,k0=8.0)                                  !/*hc         23*/
!     k-       15.                   9.0                                        !/*hc         24*/
!     p-bar    16.                  -1.0                                        !/*hc         25*/
!     n-bar    17.                  -2.0                                        !/*hc         26*/
!-----------------------------------------------------------------------        !/*hc         27*/
      logical piflag                                                            !/*hc         28*/
      logical lmc                                                               !/*hc         29*/
#ifndef OLDM                                                                    /*!hc         30*/
#include <lcs/phprm0.h>                                                         /*!hc         31*/
#endif                                                                          /*!hc         32*/
#include <lcs/inout.h>                                                          /*!hc         33*/
#include <lcs/lptS.h>                                                           /*!hc         34*/
#include <lcs/rucom.h>                                                          /*!hc         35*/
#include <lcs/cacom.h>                                                          /*!hc         36*/
#include <lcs/banka.h>                                                          /*!hc         37*/
      dimension dsel(2)                                                         !/*hc         38*/
      dimension trans(18)                                                       !/*hc         39*/
      dimension iix(6)                                                          !/*hc         40*/
      data iix /1,0,7,8,9,10/, maxstg /10/                                      !/*hc         41*/
      data trans /dp1,dp2,dp3,dp4,dp5,7*dp0,7.d0,2*8.d0,9.d0,-1.d0,-2.d0        !/*hc         42*/
     & /                                                                        !/*hc         43*/
      data limit, limit1 /1000,1000/                                            !/*hc         44*/
#ifdef OLDM                                                                     /*!hc         45*/
      data amumev /931.20793d0/                                                 !/*hc         46*/
#endif                                                                          /*!hc         47*/
      data am1, am1sq /939.57d0,882791.78d0/                                    !/*hc         48*/
      data epmax, facmax, facmin, nfac, facsum, facsq /dp0,2*dp1,0,2*dp0        !/*hc         49*/
     & /                                                                        !/*hc         50*/
!                                                                               !/*hc         51*/
      nrj=0                                                                     !/*hc         52*/
      nnuc=0                                                                    !/*hc         53*/
      negzpr=0                                                                  !/*hc         54*/
      nrjpi=0                                                                   !/*hc         55*/
      piflag=.false.                                                            !/*hc         56*/
      if (ityp.eq.5.and.ecl.le.emn) piflag=.true.                               !/*hc         57*/
      nrj1=0                                                                    !/*hc         58*/
   10 continue                                                                  !/*hc         59*/
      if (nrj.gt.0) nrj1=nrj1+1                                                 !/*hc         60*/
      if (nrj1.gt.limit1) go to 80                                              !/*hc         61*/
      csa1=trans(ityp)                                                          !/*hc         62*/
      icsa1=csa1                                                                !/*hc         63*/
      if (icsa1.eq.0) idp=ityp-6                                                !/*hc         64*/
      ekin=ecl                                                                  !/*hc         65*/
      call denk (csa1,ekin,idt,idp,voverc,cutofp,cutofn)                        !/*hc         66*/
      if (ekin.le.dp0) then                                                     !/*hc         67*/
         nopart=-1                                                              !/*hc         68*/
         go to 20                                                               !/*hc         69*/
      endif                                                                     !/*hc         70*/
      gamin=sqrt(dp1-voverc**2)                                                 !/*hc         71*/
      iprqa=ipreq                                                               !/*hc         72*/
      noprts=-1                                                                 !/*hc         73*/
      call isabel (dsel(1),proj3)                                               !/*hc         74*/
      nopart=noprts                                                             !/*hc         75*/
      if (lelem.ne.-1.and.nopart.eq.1.and.(ityp.eq.1.or.ityp.eq.2).and.         !/*hc         76*/
     & (ityp+kind(1)).eq.2) then                                                !/*hc         77*/
         if (ityp.eq.1) then                                                    !/*hc         78*/
            diff=energy(a1,z1+dp1)+energy(dp1,dp0)-energy(dp1,dp1)              !/*hc         79*/
         else                                                                   !/*hc         80*/
            diff=energy(a1,z1-dp1)+energy(dp1,dp1)-energy(dp1,dp0)              !/*hc         81*/
         endif                                                                  !/*hc         82*/
         diff=diff-energy(a1,z1)                                                !/*hc         83*/
         ep(1)=ep(1)-diff                                                       !/*hc         84*/
         if (kind(1).eq.0.and.ep(1).lt.cutofp) nopart=0                         !/*hc         85*/
         if (kind(1).eq.1.and.ep(1).lt.cutofn) nopart=0                         !/*hc         86*/
      endif                                                                     !/*hc         87*/
      if (nopart.ge.0) then                                                     !/*hc         88*/
         if (piflag) then                                                       !/*hc         89*/
            if (nopart.eq.1.and.kind(1).eq.4) go to 50                          !/*hc         90*/
         endif                                                                  !/*hc         91*/
         nrj=0                                                                  !/*hc         92*/
         if (nobalc.eq.0.or.nopart.eq.0) then                                   !/*hc         93*/
            call rebali (ecl,name,a1,z1,nrj,negzpr)                             !/*hc         94*/
            if (nopart.lt.0) go to 20                                           !/*hc         95*/
         else                                                                   !/*hc         96*/
            call nobal (ecl,a1,z1,nrj,negzpr)                                   !/*hc         97*/
! nopart may be reset by NOBAL                                                  !/*hc         98*/
            if (nopart.eq.0) call rebali (ecl,name,a1,z1,nrj,negzpr)            !/*hc         99*/
            if (nopart.lt.0) go to 20                                           !/*hc        100*/
         endif                                                                  !/*hc        101*/
         if (nrj.eq.1) go to 10                                                 !/*hc        102*/
         if (iprqa.gt.0) go to 60                                               !/*hc        103*/
         go to 40                                                               !/*hc        104*/
      elseif (lmc.and.icsa1.eq.0.and.ecl/proj3.lt.dp10) then                    !/*hc        105*/
         write (itty,*)'HCASC:cmpnd nuc nocas,ityp,ecl,a1,z1 ',                 !/*hc        106*/
     &      nocas,ityp,ecl,a1,z1                                                !/*hc        107*/
         dsel(1)=dp0                                                            !/*hc        108*/
         dsel(2)=dp0                                                            !/*hc        109*/
         nopart=0                                                               !/*hc        110*/
         call rebali (ecl,name,a1,z1,nrj,negzpr)                                !/*hc        111*/
         if (nopart.ge.0) then                                                  !/*hc        112*/
            if (iprqa.gt.0) go to 60                                            !/*hc        113*/
            go to 40                                                            !/*hc        114*/
         endif                                                                  !/*hc        115*/
      endif                                                                     !/*hc        116*/
   20 continue                                                                  !/*hc        117*/
      if (piflag) go to 50                                                      !/*hc        118*/
   30 continue                                                                  !/*hc        119*/
! score pseudocollision and return                                              !/*hc        120*/
#ifdef LCS                                                                      /*!hc        121*/
      if (lelem.eq.-1) call revnt (4)                                           !/*hc        122*/
#endif                                                                          /*!hc        123*/
   40 continue                                                                  !/*hc        124*/
      exc0=ex                                                                   !/*hc        125*/
      return                                                                    !/*hc        126*/
   50 continue                                                                  !/*hc        127*/
! force pi- to interact at rest                                                 !/*hc        128*/
      nrjpi=nrjpi+1                                                             !/*hc        129*/
      if (nrjpi.le.limit) go to 10                                              !/*hc        130*/
!        difficulty in forcing interaction                                      !/*hc        131*/
      irejpi=1                                                                  !/*hc        132*/
      write (io,90) limit,med                                                   !/*hc        133*/
      write (itty,90) limit,med                                                 !/*hc        134*/
      go to 30                                                                  !/*hc        135*/
   60 continue                                                                  !/*hc        136*/
      if (apr.le.dp5) go to 40                                                  !/*hc        137*/
      if (ex.le.dp0) go to 40                                                   !/*hc        138*/
!                                                                               !/*hc        139*/
!     INITIALIZE EMITTING NUCLEUS - PREEQ                                       !/*hc        140*/
!                                                                               !/*hc        141*/
      exc0=ex                                                                   !/*hc        142*/
      ja=apr                                                                    !/*hc        143*/
      jz=zpr                                                                    !/*hc        144*/
#ifdef OLDM                                                                     /*!hc        145*/
      ex0=energy(apr,zpr)                                                       !/*hc        146*/
      waz=ex0+amumev*apr                                                        !/*hc        147*/
#else                                                                           /*!hc        148*/
      waz=xmnuc(apr,zpr)                                                        !/*hc        149*/
#endif                                                                          /*!hc        150*/
      gamma9=dp1+erec/(waz+ex)                                                  !/*hc        151*/
      nstage=1                                                                  !/*hc        152*/
      if (ityp.eq.1.or.ityp.eq.2.or.(ityp.ge.8.and.ityp.le.11)) then            !/*hc        153*/
         if (ityp.le.2) then                                                    !/*hc        154*/
            ipr0=3-ityp                                                         !/*hc        155*/
         else                                                                   !/*hc        156*/
            ipr0=ityp-5                                                         !/*hc        157*/
         endif                                                                  !/*hc        158*/
         nnout=a1+ia6(ipr0)-apr+1.d-02                                          !/*hc        159*/
         if (nopart.eq.0) then                                                  !/*hc        160*/
            nstage=0                                                            !/*hc        161*/
#ifdef OLDM                                                                     /*!hc        162*/
            rnmas0=energy(a1,z1)+amumev*a1                                      !/*hc        163*/
#else                                                                           /*!hc        164*/
            rnmas0=xmnuc(a1,z1)                                                 !/*hc        165*/
#endif                                                                          /*!hc        166*/
            temp=sqrt((rnmas0+zmass9(ipr0))**2+dp2*rnmas0*ecl)                  !/*hc        167*/
            eps0=temp-rnmas0-zmass9(ipr0)                                       !/*hc        168*/
         endif                                                                  !/*hc        169*/
         mp=ia6(ipr0)-nnout                                                     !/*hc        170*/
      elseif (ityp.eq.17.or.ityp.eq.18) then                                    !/*hc        171*/
         nnout=a1-dp1-apr+1.d-02                                                !/*hc        172*/
         mp=-1-nnout                                                            !/*hc        173*/
         ipr0=1                                                                 !/*hc        174*/
      else                                                                      !/*hc        175*/
         nnout=a1-apr+1.d-02                                                    !/*hc        176*/
         mp=-nnout                                                              !/*hc        177*/
         ipr0=1                                                                 !/*hc        178*/
      endif                                                                     !/*hc        179*/
      if (mp.le.0) then                                                         !/*hc        180*/
         mh=1-mp                                                                !/*hc        181*/
         mp=1                                                                   !/*hc        182*/
      else                                                                      !/*hc        183*/
         mh=1                                                                   !/*hc        184*/
         mp=mp+1                                                                !/*hc        185*/
      endif                                                                     !/*hc        186*/
      nh0=a1-dsel(1)-dsel(2)                                                    !/*hc        187*/
      if (nh0.lt.0) call expirx(1,'HCASC','nh0.lt.0')                           !/*hc        188*/
      ndel0=max(nh0-mh,0)                                                       !/*hc        189*/
      mp=mp+ndel0                                                               !/*hc        190*/
      mh=mh+ndel0                                                               !/*hc        191*/
      ndel1=mp/2                                                                !/*hc        192*/
      mp=mp-ndel1                                                               !/*hc        193*/
      mh=mh-ndel1                                                               !/*hc        194*/
      if (mh.eq.0) then                                                         !/*hc        195*/
         mh=1                                                                   !/*hc        196*/
         mp=mp+1                                                                !/*hc        197*/
      endif                                                                     !/*hc        198*/
   70 continue                                                                  !/*hc        199*/
!                                                                               !/*hc        200*/
!     ATTEMPT EMISSION                                                          !/*hc        201*/
!                                                                               !/*hc        202*/
      nstage=nstage+1                                                           !/*hc        203*/
      ipr=0                                                                     !/*hc        204*/
#ifndef OLDM                                                                    /*!hc        205*/
      ex0=energy(apr,zpr)                                                       !/*hc        206*/
#endif                                                                          /*!hc        207*/
      call pclct (apr,zpr,ja,jz,ex,mp,mh,a1,z1,ipr0,erec)                       !/*hc        208*/
      if (ipr.eq.0) return                                                      !/*hc        209*/
!                                                                               !/*hc        210*/
!         BINNING EMITTED PARTICLE                                              !/*hc        211*/
!                                                                               !/*hc        212*/
      nopart=nopart+1                                                           !/*hc        213*/
      i=nopart                                                                  !/*hc        214*/
      ep(i)=eps9                                                                !/*hc        215*/
      alpha(i)=coslbp(1)                                                        !/*hc        216*/
      beta(i)=coslbp(2)                                                         !/*hc        217*/
      gam(i)=coslbp(3)                                                          !/*hc        218*/
      kind(i)=iix(ipr)+200                                                      !/*hc        219*/
      if (nstage.lt.maxstg) go to 70                                            !/*hc        220*/
      return                                                                    !/*hc        221*/
   80 continue                                                                  !/*hc        222*/
      write (io,100) limit1,nocas                                               !/*hc        223*/
      write (itty,100) limit1,nocas                                             !/*hc        224*/
      call info                                                                 !/*hc        225*/
      write (io,*) ' a,z  ',a1,z1                                               !/*hc        226*/
      nrj=0                                                                     !/*hc        227*/
      nopart=-1                                                                 !/*hc        228*/
      go to 30                                                                  !/*hc        229*/
!                                                                               !/*hc        230*/
   90 format ('HCASC: unable to absorb pi- after',i4,                           !/*hc        231*/
     & ' tries in med ',i4)                                                     !/*hc        232*/
  100 format ('HCASC: unable to resample interaction after',i4,                 !/*hc        233*/
     & ' tries for nocas =',i10)                                                !/*hc        234*/
      end                                                                       !/*hc        235*/

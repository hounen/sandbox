!     $Id: fsinfo.F,v 1.6 2002/03/27 18:15:04 gwm Exp $                       !/*fs          1*/
      subroutine fsinfo                                                         !/*fs          2*/
#include <lcs/double.h>                                                         /*!fs          3*/
#include <lcs/ercom.h>                                                          /*!fs          4*/
#include <lcs/orcm.h>                                                           /*!fs          5*/
#include <lcs/inout.h>                                                          /*!fs          6*/
      data cnver /931.143d0/, pcks /2.d-02/, ekcalm /dp10/, kecalm /50/,        !/*fs          7*/
     & zcalm /8.d1/, ecalm /40.d1/                                              !/*fs          8*/
      interr=0                                                                  !/*fs          9*/
      ncall=0                                                                   !/*fs         10*/
      d0=dp0                                                                    !/*fs         11*/
!ccc                                                                            !/*fs         12*/
!     ro    = 1.5d0                                                             !/*fs         13*/
!     ro passed in common from block data                                       !/*fs         14*/
!ccc                                                                            !/*fs         15*/
      rocc=ro                                                                   !/*fs         16*/
!ccc                                                                            !/*fs         17*/
!     the mass differences are computed from actual wapstra mass tables         !/*fs         18*/
!     set the scale factors: these are quite arbitrary at present               !/*fs         19*/
!     a more complicated scaling function for a could be used                   !/*fs         20*/
!ccc                                                                            !/*fs         21*/
      scala=af/afnp                                                             !/*fs         22*/
      scalke=dp1                                                                !/*fs         23*/
      omegao=sqrt(dp4*ef*afnp/bz0)                                              !/*fs         24*/
      rfmass=energy(af,zf)                                                      !/*fs         25*/
!ccc                                                                            !/*fs         26*/
!     calculate the value of delef, for incoming ef                             !/*fs         27*/
!ccc                                                                            !/*fs         28*/
      a1m=afnp-a2m                                                              !/*fs         29*/
      call interp (a1m)                                                         !/*fs         30*/
!ccc                                                                            !/*fs         31*/
!     for neptunium, calculate qamax, the value of the probability funct        !/*fs         32*/
!     for a2, at a2 = a2m                                                       !/*fs         33*/
!ccc                                                                            !/*fs         34*/
      call qmaxx (a2m,qamax,q)                                                  !/*fs         35*/
!ccc                                                                            !/*fs         36*/
!     multiply the calculated value by qfactr to account for variations         !/*fs         37*/
!     in a2m as ef changes                                                      !/*fs         38*/
!ccc                                                                            !/*fs         39*/
      qamax=qamax*qfactr                                                        !/*fs         40*/
!ccc                                                                            !/*fs         41*/
!     the value of a2, heavy fragment, will be chosen by rejection              !/*fs         42*/
!     double interpolate in ef and a for neptunium                              !/*fs         43*/
!ccc                                                                            !/*fs         44*/
      ciamin=iamin                                                              !/*fs         45*/
      acall=dp0                                                                 !/*fs         46*/
   10 continue                                                                  !/*fs         47*/
      acall=acall+1                                                             !/*fs         48*/
      if (acall.gt.acalm) go to 110                                             !/*fs         49*/
      ekcall=dp0                                                                !/*fs         50*/
      zcall=dp0                                                                 !/*fs         51*/
      ecall=dp0                                                                 !/*fs         52*/
      kecall=0                                                                  !/*fs         53*/
      aprime=afnp/dp2+rang()*(afnp/dp2-ciamin)                                  !/*fs         54*/
      a2np=aprime                                                               !/*fs         55*/
      a2=aprime*scala                                                           !/*fs         56*/
      iint=1                                                                    !/*fs         57*/
   20 continue                                                                  !/*fs         58*/
      ia2np=a2np                                                                !/*fs         59*/
      cia2np=ia2np                                                              !/*fs         60*/
      dela=a2np-cia2np                                                          !/*fs         61*/
      ia2=ia2np                                                                 !/*fs         62*/
      delkeh=dp0                                                                !/*fs         63*/
      delech=dp0                                                                !/*fs         64*/
      k2=ke+1                                                                   !/*fs         65*/
      delkel=sigke(ia2-100,ke)*(dp1-dela)+sigke(ia2-99,ke)*dela                 !/*fs         66*/
      delecl=delexc(ia2-100,ke,ismal)*(dp1-dela)+delexc(ia2-99,ke,ismal)        !/*fs         67*/
     & *dela                                                                    !/*fs         68*/
      if (ke.eq.kemax) go to 30                                                 !/*fs         69*/
      delkeh=sigke(ia2-100,k2)*(dp1-dela)+sigke(ia2-99,k2)*dela                 !/*fs         70*/
      delech=delexc(ia2-100,k2,ismal)*(dp1-dela)+delexc(ia2-99,k2,ismal)        !/*fs         71*/
     & *dela                                                                    !/*fs         72*/
   30 continue                                                                  !/*fs         73*/
      delken=delkel*(dp1-delef)+delkeh*delef                                    !/*fs         74*/
      delecn=delecl*(dp1-delef)+delech*delef                                    !/*fs         75*/
      emaxn=delecn+ef                                                           !/*fs         76*/
!ccc                                                                            !/*fs         77*/
!     compute smasum for neptunium and assume arg is invariant                  !/*fs         78*/
!ccc                                                                            !/*fs         79*/
      call keave (a2np,a2,smasmm)                                               !/*fs         80*/
      arg=smasmm*emaxn                                                          !/*fs         81*/
      z1pnp=z1p                                                                 !/*fs         82*/
      z2pnp=z2p                                                                 !/*fs         83*/
      if (iint.eq.0) go to 40                                                   !/*fs         84*/
      call qmaxx (a2np,qa,q)                                                    !/*fs         85*/
      qaa=qa/qamax                                                              !/*fs         86*/
      if (qaa.gt.dp1) write (io,120) qfactr,qaa,qa,qamax                        !/*fs         87*/
      raa=rang()                                                                !/*fs         88*/
      if (raa.gt.qaa) go to 10                                                  !/*fs         89*/
!ccc                                                                            !/*fs         90*/
!     choose nearest integer value of a2 for true af                            !/*fs         91*/
!ccc                                                                            !/*fs         92*/
      ia2=a2+dph                                                                !/*fs         93*/
      a2=ia2                                                                    !/*fs         94*/
      a1=af-a2                                                                  !/*fs         95*/
      iah1=a1-58                                                                !/*fs         96*/
      if (iah1.lt.1) then                                                       !/*fs         97*/
        iah1=1                                                                  !/*fs         98*/
        a1=59.d0                                                                !/*fs         99*/
        a2=af-a1                                                                !/*fs        100*/
      endif                                                                     !/*fs        101*/
      iah2=a2-58                                                                !/*fs        102*/
      if (iah2.lt.1) then                                                       !/*fs        103*/
        iah2=1                                                                  !/*fs        104*/
        a2=59.d0                                                                !/*fs        105*/
        a1=af-a2                                                                !/*fs        106*/
        iah1=a1-58                                                              !/*fs        107*/
      endif                                                                     !/*fs        108*/
!ccc                                                                            !/*fs        109*/
!     assume the integer values of a2 and a1 have been chosen by process        !/*fs        110*/
!     use integer value of a2 from now on to establish corresponding a2n        !/*fs        111*/
!     for further interpolation to get ave rel ke and z2p,etc                   !/*fs        112*/
!ccc                                                                            !/*fs        113*/
      a2np=a2/scala                                                             !/*fs        114*/
      iint=0                                                                    !/*fs        115*/
      go to 20                                                                  !/*fs        116*/
   40 continue                                                                  !/*fs        117*/
      rocc=ro                                                                   !/*fs        118*/
      ekenpm=ekinr4                                                             !/*fs        119*/
!ccc                                                                            !/*fs        120*/
!     set value of ekso                                                         !/*fs        121*/
!ccc                                                                            !/*fs        122*/
      ekso=dp0                                                                  !/*fs        123*/
      coulnp=ekenpm-ekso                                                        !/*fs        124*/
!ccc                                                                            !/*fs        125*/
!     insert a theory to caculate drat here, if desired.                        !/*fs        126*/
!ccc                                                                            !/*fs        127*/
      drat=dp1                                                                  !/*fs        128*/
!ccc                                                                            !/*fs        129*/
!     set ratio of def1/def2 eq to lowest possible value                        !/*fs        130*/
!     the ratio of stiffness coefficients is assumed equal to that given        !/*fs        131*/
!     using fong coefficients, from block data                                  !/*fs        132*/
!ccc                                                                            !/*fs        133*/
      drat=cstiff(iah2)/cstiff(iah1)                                            !/*fs        134*/
      drat=drat*(a1/a2)**0.66666667d0                                           !/*fs        135*/
!ccc                                                                            !/*fs        136*/
      coulm=scalke*coulnp                                                       !/*fs        137*/
      ekeafm=coulm+ekso                                                         !/*fs        138*/
!ccc                                                                            !/*fs        139*/
!     compute interpolated value of rmcbar                                      !/*fs        140*/
!ccc                                                                            !/*fs        141*/
      ekcall=ekcall+1                                                           !/*fs        142*/
      if (ekcall.gt.ekcalm) go to 50                                            !/*fs        143*/
      interr=1                                                                  !/*fs        144*/
      ekinr4=coulm                                                              !/*fs        145*/
      z2p=zprobf(af,zf,a2)                                                      !/*fs        146*/
      z1p=zf-z2p                                                                !/*fs        147*/
      izz2=z2p+5.d-04                                                           !/*fs        148*/
      cizz2=izz2                                                                !/*fs        149*/
      izz1=z1p+5.d-04                                                           !/*fs        150*/
      cizz1=izz1                                                                !/*fs        151*/
      delz1p=z1p-cizz1                                                          !/*fs        152*/
      delz2p=z2p-cizz2                                                          !/*fs        153*/
      r1mass=energy(a1,cizz1)*(dp1-delz1p)+energy(a1,cizz1+dp1)*delz1p          !/*fs        154*/
      r2mass=energy(a2,cizz2)*(dp1-delz2p)+energy(a2,cizz2+dp1)*delz2p          !/*fs        155*/
      pairl=pairr(a1,izz1+1,a2,izz2,corr1h,corr2l)                              !/*fs        156*/
      pairh=pairr(a1,izz1,a2,izz2+1,corr1l,corr2h)                              !/*fs        157*/
      pairsm=pairl*(dp1-delz2p)+pairh*delz2p                                    !/*fs        158*/
      rmcbar=rfmass+ef-r1mass-r2mass-pairsm                                     !/*fs        159*/
!ccc                                                                            !/*fs        160*/
!     use invariance of arg to find ebar for af                                 !/*fs        161*/
!ccc                                                                            !/*fs        162*/
      c=(a2-z2p*dp2)/a2                                                         !/*fs        163*/
      smala2=a2*(dp1+yzero*c*c)/bzero                                           !/*fs        164*/
      c=(a1-z1p*dp2)/a1                                                         !/*fs        165*/
      smala1=a1*(dp1+yzero*c*c)/bzero                                           !/*fs        166*/
      smasum=smala1+smala2                                                      !/*fs        167*/
      ebar=arg/smasum                                                           !/*fs        168*/
!ccc                                                                            !/*fs        169*/
!     fix scalke here, if desired                                               !/*fs        170*/
!     scalke = z1pnp*z2pnp/(z1p*z2p)                                            !/*fs        171*/
!     find mean relative kinetic energy by scaling from neptunium values        !/*fs        172*/
!ccc                                                                            !/*fs        173*/
      delke=scalke*delken                                                       !/*fs        174*/
      coulm=scalke*coulnp                                                       !/*fs        175*/
      ekeafm=coulm+ekso                                                         !/*fs        176*/
!ccc                                                                            !/*fs        177*/
!     compute the average value of deformation energy                           !/*fs        178*/
!ccc                                                                            !/*fs        179*/
      dbar=rmcbar-ekeafm-ebar                                                   !/*fs        180*/
      if (dbar.lt.dp0) then                                                     !/*fs        181*/
        dbar=dp0                                                                !/*fs        182*/
        ebarnw=rmcbar-ekeafm                                                    !/*fs        183*/
        if (ebarnw.le.dp0) ekeafm=rmcbar-ebar                                   !/*fs        184*/
        if (ekeafm.le.dp0) go to 110                                            !/*fs        185*/
      endif                                                                     !/*fs        186*/
   50 continue                                                                  !/*fs        187*/
      if (iversn.eq.1) go to 70                                                 !/*fs        188*/
      dlowke=dbar                                                               !/*fs        189*/
!ccc                                                                            !/*fs        190*/
!     compute d0 and ro                                                         !/*fs        191*/
!ccc                                                                            !/*fs        192*/
      vs=coulm**2/(coulm-dp2*dbar)                                              !/*fs        193*/
      c12v=c12s*rocc                                                            !/*fs        194*/
      v=c12v*z1p*z2p                                                            !/*fs        195*/
      rocc=v/vs                                                                 !/*fs        196*/
!     should iterate here to get new z2p, or use correct solution of cub        !/*fs        197*/
      interr=interr+1                                                           !/*fs        198*/
      c12sn=c12v/rocc                                                           !/*fs        199*/
      c12s=c12sn                                                                !/*fs        200*/
      couls=c12s*z1p*z2p                                                        !/*fs        201*/
      ckeav=coulm/couls                                                         !/*fs        202*/
      if (dbar.ne.dp0) d0=dbar/((dp1/ckeav)-dp1)**2                             !/*fs        203*/
!ccc                                                                            !/*fs        204*/
!     choose the value of the relative kinetic energy of fission fragmen        !/*fs        205*/
!     choose relative kinetic energy for af from a truncated qaussian           !/*fs        206*/
!ccc                                                                            !/*fs        207*/
   60 continue                                                                  !/*fs        208*/
      kecall=kecall+1                                                           !/*fs        209*/
      if (kecall.gt.kecalm) go to 10                                            !/*fs        210*/
      ekin=gaurn()*delke+ekeafm                                                 !/*fs        211*/
      coulm=ekin-ekso                                                           !/*fs        212*/
      if (coulm.le.dp0) go to 60                                                !/*fs        213*/
      dlowke=d0*(vs/coulm-dp1)**2                                               !/*fs        214*/
      em=rmcbar-ekin-dlowke                                                     !/*fs        215*/
      if (em.lt.1.0d-8) go to 60                                                !/*fs        216*/
!ccc                                                                            !/*fs        217*/
!     assume ekin has been chosen by process above                              !/*fs        218*/
!     choose z1 and z2 for the two fission fragments                            !/*fs        219*/
!     find sigmaz of gaussian z2 distribution                                   !/*fs        220*/
!     should find z2p(ekin) here                                                !/*fs        221*/
!ccc                                                                            !/*fs        222*/
      arg=smasum*(rmcbar-ekin-dlowke)                                           !/*fs        223*/
   70 continue                                                                  !/*fs        224*/
      fact=sqrt(arg)                                                            !/*fs        225*/
      sigmaz=(dp1-fm/fact)*sqrt(fact/(dp2*baz*smasum))                          !/*fs        226*/
!ccc                                                                            !/*fs        227*/
!     determine minimum and maximum z2                                          !/*fs        228*/
!     assign arbitrary values to zlow and zhigh, initially                      !/*fs        229*/
!ccc                                                                            !/*fs        230*/
      zl=z2p-izdel                                                              !/*fs        231*/
      zh=z2p+izdel                                                              !/*fs        232*/
!ccc                                                                            !/*fs        233*/
!     choose z2 from a truncated gaussian distribution                          !/*fs        234*/
!ccc                                                                            !/*fs        235*/
   80 continue                                                                  !/*fs        236*/
      zcall=zcall+1                                                             !/*fs        237*/
      if (zcall.gt.zcalm) go to 10                                              !/*fs        238*/
      z=sigmaz*gaurn()+z2p                                                      !/*fs        239*/
      if (z.le.zl.or.z.ge.zh) go to 80                                          !/*fs        240*/
!ccc                                                                            !/*fs        241*/
!     assume z has been chosen by process above                                 !/*fs        242*/
!ccc                                                                            !/*fs        243*/
      z2=z                                                                      !/*fs        244*/
      z1=zf-z2                                                                  !/*fs        245*/
!ccc                                                                            !/*fs        246*/
!     select nearest integers                                                   !/*fs        247*/
!ccc                                                                            !/*fs        248*/
      izz1=z1+dph                                                               !/*fs        249*/
      izz2=z2+dph                                                               !/*fs        250*/
      z1=izz1                                                                   !/*fs        251*/
      z2=izz2                                                                   !/*fs        252*/
!ccc                                                                            !/*fs        253*/
!     compute difference between initial energy and masses of fragments         !/*fs        254*/
!     at the characteristic level;i.e.,rmc                                      !/*fs        255*/
!ccc                                                                            !/*fs        256*/
      r1mass=energy(a1,z1)                                                      !/*fs        257*/
      r2mass=energy(a2,z2)                                                      !/*fs        258*/
      pairsm=pairr(a1,izz1,a2,izz2,corr1,corr2)                                 !/*fs        259*/
      rmc=rfmass+ef-r1mass-r2mass-pairsm                                        !/*fs        260*/
      if (iversn.eq.1) ekin=c12*z1*z2/(dp1-pcks)                                !/*fs        261*/
!ccc                                                                            !/*fs        262*/
!     set minimum deformation energy and determine maximum excitation           !/*fs        263*/
!     energy of the fission fragments                                           !/*fs        264*/
!ccc                                                                            !/*fs        265*/
      def=dbar                                                                  !/*fs        266*/
      if (iversn.ne.1) then                                                     !/*fs        267*/
        cke=(ekin-ekso)/(c12s*z1*z2)                                            !/*fs        268*/
        if (cke.gt.dp0) then                                                    !/*fs        269*/
          dlowke=d0*(dp1/cke-dp1)**2                                            !/*fs        270*/
          def=dlowke                                                            !/*fs        271*/
        endif                                                                   !/*fs        272*/
      endif                                                                     !/*fs        273*/
      e1maxp=rmc-ekin-def                                                       !/*fs        274*/
!ccc                                                                            !/*fs        275*/
!     now, do check on energy conservation                                      !/*fs        276*/
!     select new z1,z2 pair                                                     !/*fs        277*/
!ccc                                                                            !/*fs        278*/
      if (e1maxp.le.1.0d-8) go to 80                                            !/*fs        279*/
      e=e1maxp                                                                  !/*fs        280*/
!ccc                                                                            !/*fs        281*/
!     select scission point excitation energies for chosen fragments            !/*fs        282*/
!ccc                                                                            !/*fs        283*/
      c=(a2-z2*dp2)/a2                                                          !/*fs        284*/
      smala2=a2*(dp1+yzero*c*c)/bzero                                           !/*fs        285*/
      c=(a1-z1*dp2)/a1                                                          !/*fs        286*/
      smala1=a1*(dp1+yzero*c*c)/bzero                                           !/*fs        287*/
   90 continue                                                                  !/*fs        288*/
      ecall=ecall+1                                                             !/*fs        289*/
      if (ecall.gt.ecalm) go to 10                                              !/*fs        290*/
      smasum=smala1+smala2                                                      !/*fs        291*/
      e1c=e*rang()                                                              !/*fs        292*/
      qearg=(dp2*sqrt(smala1*e1c)+dp2*sqrt(smala2*(e-e1c))-dp2*sqrt             !/*fs        293*/
     & (smasum*e1maxp))                                                         !/*fs        294*/
      qe=exp(qearg)                                                             !/*fs        295*/
      qe=qe*(e1c*(e-e1c)/e1maxp**2)**1.5d0                                      !/*fs        296*/
      re=rang()                                                                 !/*fs        297*/
      if (re.gt.qe) go to 90                                                    !/*fs        298*/
      e2c=e-e1c                                                                 !/*fs        299*/
!ccc                                                                            !/*fs        300*/
!     assume the scission point excitation energies have been chosen;           !/*fs        301*/
!     find excitation energies at evap time                                     !/*fs        302*/
!ccc                                                                            !/*fs        303*/
      def2=def/(dp1+drat)                                                       !/*fs        304*/
      def1=drat*def2                                                            !/*fs        305*/
!ccc                                                                            !/*fs        306*/
!     if magic no corrections have been added, they should be in corr1 a        !/*fs        307*/
!ccc                                                                            !/*fs        308*/
      e1=e1c+def1+corr1                                                         !/*fs        309*/
      e2=e2c+def2+corr2                                                         !/*fs        310*/
!ccc                                                                            !/*fs        311*/
!     find total masses, including excitation energies, at evap time            !/*fs        312*/
!ccc                                                                            !/*fs        313*/
      am1=a1*cnver+r1mass+e1                                                    !/*fs        314*/
      am2=a2*cnver+r2mass+e2                                                    !/*fs        315*/
      amcf=rfmass+ef+af*cnver                                                   !/*fs        316*/
      amc1=am1                                                                  !/*fs        317*/
      amc2=am2                                                                  !/*fs        318*/
      amdiff=rfmass+ef-r1mass-r2mass-e1-e2                                      !/*fs        319*/
!ccc                                                                            !/*fs        320*/
!     amdiff= ekin should be satisfied                                          !/*fs        321*/
!ccc                                                                            !/*fs        322*/
      ernff=ernf                                                                !/*fs        323*/
      call fisdis                                                               !/*fs        324*/
      eke1=fiske(1)                                                             !/*fs        325*/
      eke2=fiske(2)                                                             !/*fs        326*/
      afis(1)=a1                                                                !/*fs        327*/
      afis(2)=a2                                                                !/*fs        328*/
      zfis(1)=z1                                                                !/*fs        329*/
      zfis(2)=z2                                                                !/*fs        330*/
      fisu(1)=e1                                                                !/*fs        331*/
      fisu(2)=e2                                                                !/*fs        332*/
  100 continue                                                                  !/*fs        333*/
      return                                                                    !/*fs        334*/
!ccc                                                                            !/*fs        335*/
  110 continue                                                                  !/*fs        336*/
      ncall=ncall+1                                                             !/*fs        337*/
      write (io,130) acall,zcall,ecall,kecall,af,zf                             !/*fs        338*/
      go to 100                                                                 !/*fs        339*/
!                                                                               !/*fs        340*/
  120 format (1h ,'fsinfo ',2x,'qfactr =',1pe12.5,2x,'qaa =',e12.5,2x,          !/*fs        341*/
     & 'qa =',e12.5,2x,'qamax =',e12.5)                                         !/*fs        342*/
  130 format (1h ,'fission failed: acall=',f8.2,2x,'zcall=',f8.2,2x,            !/*fs        343*/
     & 'ecall=',f8.2,2x,'kecall=',i5,2x,'af=',f6.1,2x,'zf=',f6.1)               !/*fs        344*/
      end                                                                       !/*fs        345*/

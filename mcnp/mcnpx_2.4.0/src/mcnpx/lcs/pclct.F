!     $Id: pclct.F,v 1.4 2002/03/27 18:15:13 gwm Exp $                        !/*pc          1*/
      subroutine pclct (a,z,ja,jz,u,np,nh,a0,z0,ipr0,erec8)                     !/*pc          2*/
#include <lcs/double.h>                                                         /*!pc          3*/
#ifdef OLDM                                                                     /*!pc          4*/
      parameter (pi=3.141593d0, hbar=6.581d-22, clite=2.9979d+10, cmtofm        !/*pc          5*/
     & =1.0d-13, fsc=dp1/137.04d0, hbarc=hbar*clite, rcoul=1.70d0, conce        !/*pc          6*/
     & =fsc*hbarc/(rcoul*cmtofm), conar=dph*fsc*cmtofm*rcoul/hbarc,             !/*pc          7*/
     & cmtomb=1.0d+27, conem=dp1/(cmtomb*hbar*(pi*hbarc)**2), conit=pi          !/*pc          8*/
     & /hbar, conrad=hbarc/cmtofm, sqrt2=1.414214d0, amu=931.20793d0,           !/*pc          9*/
     & fmtomb=1.d-1, tenpi=dp10*pi)                                             !/*pc         10*/
#else                                                                           /*!pc         11*/
#include <lcs/phprm0.h>                                                         /*!pc         12*/
#include <lcs/phprm1.h>                                                         /*!pc         13*/
#endif                                                                          /*!pc         14*/
#include <lcs/lptS.h>                                                           /*!pc         15*/
#include <lcs/cacom.h>                                                          /*!pc         16*/
#include <lcs/cmgcom.h>                                                         /*!pc         17*/
      external pqbd                                                             !/*pc         18*/
      dimension fla(6), flz(6), rr(6), s(6)                                     !/*pc         19*/
      dimension alpha(6), beta(6), g(6), aph(6), epsmax(6), xint(6),            !/*pc         20*/
     & resmas(6), c(3), qb(6), redmas(6), rchan(6), gzero(6), gone(6),          !/*pc         21*/
     & epss(3,6), f(6), px(6), qx(6), rx(6), flamdx(6), fmux(6), fnux(6)        !/*pc         22*/
     & , fmuxpr(6), fnuxpr(6), yint(3,6), rho0(6)                               !/*pc         23*/
      data rho0 /dp0,dp0,3*.8d0,1.2d0/, rcomp /1.23d0/, mode /1/, modeb         !/*pc         24*/
     & /0/, nemax0 /0/, ptest /1.d-3/, kalb /0/                                 !/*pc         25*/
      data fla /dp1,dp1,dp2,dp3,dp3,dp4/, flz /dp0,3*dp1,2*dp2/                 !/*pc         26*/
      data alpha /dp2,dp2,dp3,dp2,dp2,dp1/                                      !/*pc         27*/
!                                                                               !/*pc         28*/
!*****************************************************************              !/*pc         29*/
!                                                                               !/*pc         30*/
!       main loop to compute the emission probabilities                         !/*pc         31*/
!       for the 6 clusters. 1=n,2=p,3=d,4=t,5=he3,6=he4                         !/*pc         32*/
!                                                                               !/*pc         33*/
!       preequilibrium model                                                    !/*pc         34*/
!                                                                               !/*pc         35*/
!*****************************************************************              !/*pc         36*/
!                                                                               !/*pc         37*/
!     ALPHA(J) = (2S+1)                                                         !/*pc         38*/
!                                                                               !/*pc         39*/
      g0=getg(u,jz,ja-jz,ilvden,isdum,g00)                                      !/*pc         40*/
      fnemax=sqrt(1.6d0*g0*u)                                                   !/*pc         41*/
      if (nemax0.gt.dp0) then                                                   !/*pc         42*/
        nemax=nemax0                                                            !/*pc         43*/
      else                                                                      !/*pc         44*/
        nemax=max(ia6(ipr0)+2,int(fnemax))                                      !/*pc         45*/
      endif                                                                     !/*pc         46*/
      if ((np+nh).gt.nemax) then                                                !/*pc         47*/
        ipr=0                                                                   !/*pc         48*/
        eps9=dp0                                                                !/*pc         49*/
        eprime=dp0                                                              !/*pc         50*/
        go to 250                                                               !/*pc         51*/
      endif                                                                     !/*pc         52*/
!                                                                               !/*pc         53*/
!     CALCULATE EXCITON-INDEPENDENT FACTORS                                     !/*pc         54*/
!                                                                               !/*pc         55*/
      do 10 j=1,6                                                               !/*pc         56*/
      zz=z-flz(j)                                                               !/*pc         57*/
      aa=a-fla(j)                                                               !/*pc         58*/
      if (aa.lt.fla(j).or.zz.lt.flz(j).or.aa.le.zz) go to 10                    !/*pc         59*/
      mm=ja-ia6(j)                                                              !/*pc         60*/
#ifdef OLDM                                                                     /*!pc         61*/
      temp1=energy(aa,zz)                                                       !/*pc         62*/
      s(j)=temp1+exmass(j)-ex0                                                  !/*pc         63*/
      resmas(j)=temp1+amu*mm                                                    !/*pc         64*/
#else                                                                           /*!pc         65*/
      s(j)=energy(aa,zz)+exmass(j)-ex0                                          !/*pc         66*/
      resmas(j)=xmnuc(aa,zz)                                                    !/*pc         67*/
#endif                                                                          /*!pc         68*/
      f(j)=resmas(j)/(zmass9(j)+resmas(j))                                      !/*pc         69*/
      redmas(j)=zmass9(j)*f(j)                                                  !/*pc         70*/
      if (itt(j).eq.0) then                                                     !/*pc         71*/
        if (j.gt.1) then                                                        !/*pc         72*/
          radtot=rmass(mm)+rho0(j)/rcoul                                        !/*pc         73*/
          arg0=conar*flz(j)*zz*radtot*redmas(j)                                 !/*pc         74*/
          epss(1,j)=conce*clmb3(arg0)*flz(j)*zz/radtot                          !/*pc         75*/
        else                                                                    !/*pc         76*/
          epss(1,j)=dp0                                                         !/*pc         77*/
        endif                                                                   !/*pc         78*/
      else                                                                      !/*pc         79*/
        call setcmg (j,aa,zz,f(j))                                              !/*pc         80*/
        epss(1,j)=ezero                                                         !/*pc         81*/
        epss(2,j)=ecoul                                                         !/*pc         82*/
        epss(3,j)=ematch                                                        !/*pc         83*/
        px(j)=p                                                                 !/*pc         84*/
        qx(j)=q                                                                 !/*pc         85*/
        rx(j)=r                                                                 !/*pc         86*/
        flamdx(j)=lambda                                                        !/*pc         87*/
        fmux(j)=mu                                                              !/*pc         88*/
        fnux(j)=nu                                                              !/*pc         89*/
        fmuxpr(j)=muprim                                                        !/*pc         90*/
        fnuxpr(j)=nuprim                                                        !/*pc         91*/
      endif                                                                     !/*pc         92*/
      if (u-s(j)-epss(1,j).le.dp0) go to 10                                     !/*pc         93*/
      nn=int(aa-zz)                                                             !/*pc         94*/
      izz=int(zz)                                                               !/*pc         95*/
      gone(j)=getg(u-s(j)-epss(1,j),izz,nn,ilvden,isdum,gzero(j))               !/*pc         96*/
!                                                                               !/*pc         97*/
!     GONE evaluated at maximum excitation, GZERO at zero                       !/*pc         98*/
!                                                                               !/*pc         99*/
      if (itt(j).eq.0) then                                                     !/*pc        100*/
        if (j.eq.1) then                                                        !/*pc        101*/
          rn0=rcomp*rmass(mm)                                                   !/*pc        102*/
          rchan(1)=sqrt2*rn0                                                    !/*pc        103*/
          beta(1)=dph*(conrad/rn0)**2/redmas(1)                                 !/*pc        104*/
        else                                                                    !/*pc        105*/
          beta(j)=-epss(1,j)                                                    !/*pc        106*/
          temp=dp2*redmas(j)*epss(1,j)                                          !/*pc        107*/
          rchan(j)=rcomp*rmass(mm)+rho0(j)+conrad/sqrt(temp)                    !/*pc        108*/
        endif                                                                   !/*pc        109*/
      endif                                                                     !/*pc        110*/
   10 continue                                                                  !/*pc        111*/
!                                                                               !/*pc        112*/
!     START EMISSION LOOP                                                       !/*pc        113*/
!                                                                               !/*pc        114*/
   20 continue                                                                  !/*pc        115*/
      ne=np+nh                                                                  !/*pc        116*/
      xp=np                                                                     !/*pc        117*/
      xh=nh                                                                     !/*pc        118*/
      xe=xp+xh                                                                  !/*pc        119*/
      rsum=dp0                                                                  !/*pc        120*/
      if (nstage.eq.1) call qsubb (qb,mode,a0,z0,ipr0,nh)                       !/*pc        121*/
      xph=max(xp,xh)                                                            !/*pc        122*/
      aph0=0.25d0*(dp4*xph**2-xp*(xp+1)-xh*(xh+1))/g00                          !/*pc        123*/
      if (u.le.aph0) then                                                       !/*pc        124*/
        eprime=dp0                                                              !/*pc        125*/
        go to 250                                                               !/*pc        126*/
      endif                                                                     !/*pc        127*/
      do 60 j=1,6                                                               !/*pc        128*/
      rr(j)=dp0                                                                 !/*pc        129*/
      zz=z-flz(j)                                                               !/*pc        130*/
      aa=a-fla(j)                                                               !/*pc        131*/
      if (np.lt.ia6(j)) go to 60                                                !/*pc        132*/
      if (aa.lt.fla(j).or.zz.lt.flz(j).or.aa.le.zz) go to 60                    !/*pc        133*/
      if (u-s(j)-epss(1,j).le.dp0) go to 60                                     !/*pc        134*/
      mm=ja-ia6(j)                                                              !/*pc        135*/
      nn=int(aa-zz)                                                             !/*pc        136*/
      izz=int(zz)                                                               !/*pc        137*/
      xpj=xp-fla(j)                                                             !/*pc        138*/
      aph(j)=0.25d0*(4*max(xpj,xh)**2-xpj*(xpj+1)-xh*(xh+1))/gzero(j)           !/*pc        139*/
      if (ilvden.eq.0) then                                                     !/*pc        140*/
        smalg1=getg(aph(j),izz,nn,ilvden,isdum,dumm)                            !/*pc        141*/
        g(j)=max(gone(j),smalg1)                                                !/*pc        142*/
      else                                                                      !/*pc        143*/
        g(j)=gone(j)                                                            !/*pc        144*/
      endif                                                                     !/*pc        145*/
      epsmax(j)=u-s(j)-aph(j)                                                   !/*pc        146*/
      x1=epsmax(j)-epss(1,j)                                                    !/*pc        147*/
      if (x1.le.dp0) go to 60                                                   !/*pc        148*/
      if (nstage.eq.1) then                                                     !/*pc        149*/
        qbx=qb(j)                                                               !/*pc        150*/
      else                                                                      !/*pc        151*/
        qbx=dp1                                                                 !/*pc        152*/
      endif                                                                     !/*pc        153*/
      nej=ne-ia6(j)                                                             !/*pc        154*/
!     fact=alpha(j)*redmas(j)*qbx*g(j)**nej                                     !/*pc        155*/
      fact=alpha(j)*(redmas(j)/redmas(1))*qbx                                   !/*pc        156*/
      if (itt(j).eq.0) then                                                     !/*pc        157*/
        xej=xpj+xh                                                              !/*pc        158*/
        xint(j)=((epsmax(j)+beta(j))/xej-x1/(xej+dp1))                          !/*pc        159*/
        xint(j)=tenpi*rchan(j)**2*xint(j)                                       !/*pc        160*/
        go to 40                                                                !/*pc        161*/
      endif                                                                     !/*pc        162*/
      yint(1,j)=f1(epsmax(j),x1,nej,px(j),qx(j),rx(j))                          !/*pc        163*/
      yint(2,j)=dp0                                                             !/*pc        164*/
      yint(3,j)=dp0                                                             !/*pc        165*/
      x2=epsmax(j)-epss(2,j)                                                    !/*pc        166*/
      if (x2.le.dp0) go to 30                                                   !/*pc        167*/
      fk2=nej*log10(x2/x1)                                                      !/*pc        168*/
      if (fk2.le.-30.d0) go to 30                                               !/*pc        169*/
      xk2=10.d0**fk2                                                            !/*pc        170*/
      yint(1,j)=yint(1,j)-f1(epsmax(j),x2,nej,px(j),qx(j),rx(j))*xk2            !/*pc        171*/
      yint(2,j)=f2(epsmax(j),x2,nej,flamdx(j),fmux(j),fnux(j))*xk2              !/*pc        172*/
      x3=epsmax(j)-epss(3,j)                                                    !/*pc        173*/
      if (x3.le.dp0) go to 30                                                   !/*pc        174*/
      fk3=nej*log10(x3/x1)                                                      !/*pc        175*/
      if (fk3.le.-30.d0) go to 30                                               !/*pc        176*/
      xk3=10.d0**fk3                                                            !/*pc        177*/
      yint(2,j)=yint(2,j)-f2(epsmax(j),x3,nej,flamdx(j),fmux(j),fnux(j))        !/*pc        178*/
     & *xk3                                                                     !/*pc        179*/
      yint(3,j)=f3(epsmax(j),x3,nej,fmuxpr(j),fnuxpr(j))*xk3                    !/*pc        180*/
   30 continue                                                                  !/*pc        181*/
      xint(j)=yint(1,j)+yint(2,j)+yint(3,j)                                     !/*pc        182*/
   40 continue                                                                  !/*pc        183*/
      iup=ia6(j)                                                                !/*pc        184*/
      t1=xp+dp1                                                                 !/*pc        185*/
      do 50 i=1,iup                                                             !/*pc        186*/
      t2=i                                                                      !/*pc        187*/
   50 fact=fact*(t1-t2)*(xe-t2)                                                 !/*pc        188*/
      rr(j)=fact*xint(j)*(x1*g(j)/(g0*(u-aph0)))**ne*(x1*g(j))**(-ia6(j)        !/*pc        189*/
     & )                                                                        !/*pc        190*/
      rsum=rsum+rr(j)                                                           !/*pc        191*/
   60 continue                                                                  !/*pc        192*/
!                                                                               !/*pc        193*/
!     COMPUTE TOTAL EMISSION RATE                                               !/*pc        194*/
!                                                                               !/*pc        195*/
      fact1=(conem/conit)*redmas(1)*(u-aph0)                                    !/*pc        196*/
      pemit=fact1*rsum                                                          !/*pc        197*/
!                                                                               !/*pc        198*/
!     COMPUTE FORWARD TRANSITION RATE                                           !/*pc        199*/
!                                                                               !/*pc        200*/
!     fac=conit*elment(a,u,ne)*g0                                               !/*pc        201*/
      fac=elment(a,u,ne)*g0                                                     !/*pc        202*/
      transp=fac*(g0*(u-(xph+1)**2/g00))**2/(xe+dp1)                            !/*pc        203*/
      if (modeb.eq.0) then                                                      !/*pc        204*/
        transm=dp0                                                              !/*pc        205*/
      else                                                                      !/*pc        206*/
!                                                                               !/*pc        207*/
!        COMPUTE BACKWARD TRANSITION RATE                                       !/*pc        208*/
!                                                                               !/*pc        209*/
        term=8.d0*g0*(u-xph**2/g00)                                             !/*pc        210*/
        term=((xp-dp1)*(xp-dp2)+(xh-dp1)*(xh-dp2))/term                         !/*pc        211*/
        transm=fac*xp*xh*((xe-dp2)-(xe-dp1)*term)                               !/*pc        212*/
      endif                                                                     !/*pc        213*/
      ptot=pemit+transp+transm                                                  !/*pc        214*/
      pemit=pemit/ptot                                                          !/*pc        215*/
   70 continue                                                                  !/*pc        216*/
      if (rang().lt.pemit) go to 80                                             !/*pc        217*/
!                                                                               !/*pc        218*/
!     INTERNAL TRANSITION SELECTED                                              !/*pc        219*/
!                                                                               !/*pc        220*/
      ptrans=transm/(transm+transp)                                             !/*pc        221*/
      r1=rang()                                                                 !/*pc        222*/
      if (modeb.ne.0.and.r1.lt.ptrans) then                                     !/*pc        223*/
        np=np-1                                                                 !/*pc        224*/
        nh=nh-1                                                                 !/*pc        225*/
        go to 20                                                                !/*pc        226*/
      else                                                                      !/*pc        227*/
        ipr=0                                                                   !/*pc        228*/
        eps9=dp0                                                                !/*pc        229*/
        if ((ne+2).gt.nemax.or.pemit.lt.ptest) then                             !/*pc        230*/
          eprime=dp0                                                            !/*pc        231*/
          go to 250                                                             !/*pc        232*/
        else                                                                    !/*pc        233*/
          np=np+1                                                               !/*pc        234*/
          nh=nh+1                                                               !/*pc        235*/
          go to 20                                                              !/*pc        236*/
        endif                                                                   !/*pc        237*/
      endif                                                                     !/*pc        238*/
   80 continue                                                                  !/*pc        239*/
!                                                                               !/*pc        240*/
!     SAMPLE FOR PARTCLE TYPE                                                   !/*pc        241*/
!                                                                               !/*pc        242*/
      rtest=rsum*rang()                                                         !/*pc        243*/
      rcum=dp0                                                                  !/*pc        244*/
      do 90 j=1,6                                                               !/*pc        245*/
      if (rr(j).eq.dp0) go to 90                                                !/*pc        246*/
      rcum=rcum+rr(j)                                                           !/*pc        247*/
      if (rtest.le.rcum) go to 100                                              !/*pc        248*/
   90 continue                                                                  !/*pc        249*/
      call expirx(1,'PCLCT','failed to sample for particle type')               !/*pc        250*/
  100 continue                                                                  !/*pc        251*/
      ipr=j                                                                     !/*pc        252*/
      ja1=ja-ia6(ipr)                                                           !/*pc        253*/
      jz1=jz-iz6(ipr)                                                           !/*pc        254*/
      if (itt(ipr).ne.0) go to 110                                              !/*pc        255*/
!                                                                               !/*pc        256*/
!     SAMPLE FOR CHANNEL ENERGY EPS (ITT=0)                                     !/*pc        257*/
!                                                                               !/*pc        258*/
      nex=ne-ia6(ipr)                                                           !/*pc        259*/
      iorder=2                                                                  !/*pc        260*/
      if (ipr.eq.1) then                                                        !/*pc        261*/
        prob=beta(1)*(nex+1)/epsmax(1)                                          !/*pc        262*/
        r3=rang()                                                               !/*pc        263*/
        if (r3.lt.prob) iorder=1                                                !/*pc        264*/
      endif                                                                     !/*pc        265*/
      y=betac(nex,iorder)                                                       !/*pc        266*/
      eps9=epsmax(ipr)-(epsmax(ipr)-epss(1,ipr))*y                              !/*pc        267*/
      eprime=u-s(ipr)-eps9                                                      !/*pc        268*/
!                                                                               !/*pc        269*/
!     REJECT ON INVERSE XSEC AND G PARAMETER                                    !/*pc        270*/
!                                                                               !/*pc        271*/
      rchan0=rcomp*rmass(ja1)+rho0(ipr)+conrad/sqrt(dp2*redmas(ipr)*eps9        !/*pc        272*/
     & )                                                                        !/*pc        273*/
      rejf1=(rchan0/rchan(ipr))**2                                              !/*pc        274*/
      if (ipr.eq.1) rejf1=rejf1*eps9/(eps9+beta(1))                             !/*pc        275*/
      if (ilvden.eq.0) then                                                     !/*pc        276*/
        gofe=getg(eprime,jz1,ja1-jz1,ilvden,isdum,dumm)                         !/*pc        277*/
        rejf2=(gofe/g(ipr))**nex                                                !/*pc        278*/
      else                                                                      !/*pc        279*/
        rejf2=dp1                                                               !/*pc        280*/
      endif                                                                     !/*pc        281*/
      if (rejf1.gt.dp1.or.rejf2.gt.1.0001) call expirx(1,'PCLCT',               !/*pc        282*/
     & 'rejf1.gt.dp1.or.rejf2.gt.1.0001')                                       !/*pc        283*/
      if (rang().gt.rejf1*rejf2) go to 70                                       !/*pc        284*/
      go to 230                                                                 !/*pc        285*/
  110 continue                                                                  !/*pc        286*/
!                                                                               !/*pc        287*/
!     SAMPLE FOR CHANNEL ENERGY EPS (ITT .NE. 0)                                !/*pc        288*/
!                                                                               !/*pc        289*/
      rtest=xint(ipr)*rang()                                                    !/*pc        290*/
      rcum=dp0                                                                  !/*pc        291*/
      do 120 j=1,3                                                              !/*pc        292*/
      rcum=rcum+yint(j,ipr)                                                     !/*pc        293*/
      if (rtest.le.rcum) go to 130                                              !/*pc        294*/
  120 continue                                                                  !/*pc        295*/
      call expirx(1,'PCLCT','failed to sample for channel energy eps')          !/*pc        296*/
  130 continue                                                                  !/*pc        297*/
      j0=j                                                                      !/*pc        298*/
      nej=ne-ia6(ipr)                                                           !/*pc        299*/
      fnej=nej                                                                  !/*pc        300*/
      frac=dp1/fnej                                                             !/*pc        301*/
      if (j0-2) 140,170,200                                                     !/*pc        302*/
  140 continue                                                                  !/*pc        303*/
      e2=epss(2,ipr)                                                            !/*pc        304*/
      pa=px(ipr)                                                                !/*pc        305*/
      qa=qx(ipr)                                                                !/*pc        306*/
      ra=rx(ipr)                                                                !/*pc        307*/
      if (pa.lt.dp0) then                                                       !/*pc        308*/
        test=dp3*pa*e2**2+dp2*qa*e2+ra                                          !/*pc        309*/
        if (test.ge.dp0) then                                                   !/*pc        310*/
          rjmax=(pa*e2**2+qa*e2+ra)*e2                                          !/*pc        311*/
        else                                                                    !/*pc        312*/
          temp=sqrt(qa**2-dp3*pa*ra)                                            !/*pc        313*/
          er=(temp-qa)/(dp3*pa)                                                 !/*pc        314*/
          rjmax=(pa*er**2+qa*er+ra)*er                                          !/*pc        315*/
        endif                                                                   !/*pc        316*/
      else                                                                      !/*pc        317*/
        rjmax=(pa*e2**2+qa*e2+ra)*e2                                            !/*pc        318*/
      endif                                                                     !/*pc        319*/
      x1=epsmax(ipr)-epss(1,ipr)                                                !/*pc        320*/
      x2=max(epsmax(ipr)-e2,dp0)                                                !/*pc        321*/
      if (x2.le.dp0) go to 150                                                  !/*pc        322*/
      test=nej*log10(x1/x2)                                                     !/*pc        323*/
      if (test.gt.30.d0) go to 150                                              !/*pc        324*/
      ff0=x2                                                                    !/*pc        325*/
      if (test.gt.-30.d0) then                                                  !/*pc        326*/
        ff1=dp10**test                                                          !/*pc        327*/
      else                                                                      !/*pc        328*/
        ff1=dp0                                                                 !/*pc        329*/
      endif                                                                     !/*pc        330*/
      ff2=dp1                                                                   !/*pc        331*/
      go to 160                                                                 !/*pc        332*/
  150 continue                                                                  !/*pc        333*/
      ff0=x1                                                                    !/*pc        334*/
      ff1=dp1                                                                   !/*pc        335*/
      ff2=dp0                                                                   !/*pc        336*/
  160 continue                                                                  !/*pc        337*/
      if (ff2.gt.dp0) then                                                      !/*pc        338*/
        x=ff0*(ff2+(ff1-ff2)*rang())**frac                                      !/*pc        339*/
      else                                                                      !/*pc        340*/
        x=ff0*rang()**frac                                                      !/*pc        341*/
      endif                                                                     !/*pc        342*/
      eps9=epsmax(ipr)-x                                                        !/*pc        343*/
      test=(pa*eps9**3+qa*eps9**2+ra*eps9)/rjmax                                !/*pc        344*/
      if (test.gt.dp1.or.test.le.dp0) call expirx(1,'PCLCT',                    !/*pc        345*/
     & 'test.gt.dp1.or.test.le.dp0')                                            !/*pc        346*/
      if (rang().gt.test) go to 160                                             !/*pc        347*/
      go to 220                                                                 !/*pc        348*/
  170 continue                                                                  !/*pc        349*/
      e3=epss(3,ipr)                                                            !/*pc        350*/
      e2=epss(2,ipr)                                                            !/*pc        351*/
      flamda=flamdx(ipr)                                                        !/*pc        352*/
      fmua=fmux(ipr)                                                            !/*pc        353*/
      fnua=fnux(ipr)                                                            !/*pc        354*/
      rjmax=flamda*e3**2+fmua*e3+fnua                                           !/*pc        355*/
      if ((dp2*flamda*e2+fmua).lt.dp0) rjmax=max(flamda*e2**2+fmua*e2           !/*pc        356*/
     & +fnua,rjmax)                                                             !/*pc        357*/
      x3=max(epsmax(ipr)-e3,dp0)                                                !/*pc        358*/
      x2=epsmax(ipr)-e2                                                         !/*pc        359*/
      if (x3.le.dp0) go to 180                                                  !/*pc        360*/
      test=nej*log10(x2/x3)                                                     !/*pc        361*/
      if (test.gt.30.d0) go to 180                                              !/*pc        362*/
      ff0=x3                                                                    !/*pc        363*/
      ff1=dp10**test                                                            !/*pc        364*/
      ff2=dp1                                                                   !/*pc        365*/
      go to 190                                                                 !/*pc        366*/
  180 continue                                                                  !/*pc        367*/
      ff0=x2                                                                    !/*pc        368*/
      ff1=dp1                                                                   !/*pc        369*/
      ff2=dp0                                                                   !/*pc        370*/
  190 continue                                                                  !/*pc        371*/
      if (ff2.gt.dp0) then                                                      !/*pc        372*/
        x=ff0*(ff2+(ff1-ff2)*rang())**frac                                      !/*pc        373*/
      else                                                                      !/*pc        374*/
        x=ff0*rang()**frac                                                      !/*pc        375*/
      endif                                                                     !/*pc        376*/
      eps9=epsmax(ipr)-x                                                        !/*pc        377*/
      test=((flamda*eps9+fmua)*eps9+fnua)/rjmax                                 !/*pc        378*/
      if (test.gt.dp1.or.test.le.dp0) call expirx(1,'PCLCT',                    !/*pc        379*/
     & 'test.gt.dp1.or.test.le.dp0')                                            !/*pc        380*/
      if (rang().gt.test) go to 190                                             !/*pc        381*/
      go to 220                                                                 !/*pc        382*/
  200 continue                                                                  !/*pc        383*/
      rjmax=fmuxpr(ipr)*epsmax(ipr)+fnuxpr(ipr)                                 !/*pc        384*/
      x3=epsmax(ipr)-epss(3,ipr)                                                !/*pc        385*/
  210 continue                                                                  !/*pc        386*/
      x=x3*(rang())**frac                                                       !/*pc        387*/
      eps9=epsmax(ipr)-x                                                        !/*pc        388*/
      test=(fmuxpr(ipr)*eps9+fnuxpr(ipr))/rjmax                                 !/*pc        389*/
      if (test.gt.dp1) call expirx(1,'PCLCT','test.gt.dp1')                     !/*pc        390*/
      if (rang().gt.test) go to 210                                             !/*pc        391*/
  220 continue                                                                  !/*pc        392*/
      eprime=u-s(ipr)-eps9                                                      !/*pc        393*/
      if (ilvden.eq.0) then                                                     !/*pc        394*/
        gofe=getg(eprime,jz1,ja1-jz1,ilvden,isdum,dumm)                         !/*pc        395*/
        rejf3=(gofe/g(ipr))**nej                                                !/*pc        396*/
        r4=rang()                                                               !/*pc        397*/
        if (r4.gt.rejf3) go to 70                                               !/*pc        398*/
      endif                                                                     !/*pc        399*/
  230 continue                                                                  !/*pc        400*/
      np0=np                                                                    !/*pc        401*/
      np=np-ia6(ipr)                                                            !/*pc        402*/
      ja=ja1                                                                    !/*pc        403*/
      jz=jz1                                                                    !/*pc        404*/
      a=ja                                                                      !/*pc        405*/
      z=jz                                                                      !/*pc        406*/
      ne=np+nh                                                                  !/*pc        407*/
!                                                                               !/*pc        408*/
!     SAMPLE ANGULAR DISTRIBUTION IN CM                                         !/*pc        409*/
!                                                                               !/*pc        410*/
      if (nstage.eq.1.or.kalb.ne.0) then                                        !/*pc        411*/
!                                                                               !/*pc        412*/
!        KALBACH PARAMETERIZATION                                               !/*pc        413*/
!                                                                               !/*pc        414*/
        aa=xxa(a0,z0,fla(ipr0),flz(ipr0),fla(ipr),flz(ipr),eps0,eps9)           !/*pc        415*/
        fmsd=max(dp1-dp2*(nh-1)/(fnemax-fla(ipr0)-dp2),dp0)                     !/*pc        416*/
        bb=(dp1-fmsd)/(dp1+fmsd)                                                !/*pc        417*/
        c(3)=dist1(aa,bb)                                                       !/*pc        418*/
        call azirn (sinph,cosph)                                                !/*pc        419*/
        temp=sqrt(dp1-c(3)**2)                                                  !/*pc        420*/
        c(1)=temp*cosph                                                         !/*pc        421*/
        c(2)=temp*sinph                                                         !/*pc        422*/
      else                                                                      !/*pc        423*/
!                                                                               !/*pc        424*/
!        ISOTROPIC DISTRIBUTION                                                 !/*pc        425*/
!                                                                               !/*pc        426*/
        call gtiso (c(1),c(2),c(3))                                             !/*pc        427*/
      endif                                                                     !/*pc        428*/
!                                                                               !/*pc        429*/
!     DEFINE RESIDUAL NUCLEUS                                                   !/*pc        430*/
!                                                                               !/*pc        431*/
#ifdef OLDM                                                                     /*!pc        432*/
      ex0=resmas(ipr)-amu*ja                                                    !/*pc        433*/
#else                                                                           /*!pc        434*/
      ex0=energy(a,z)                                                           !/*pc        435*/
#endif                                                                          /*!pc        436*/
      u=eprime                                                                  !/*pc        437*/
      rnmass=resmas(ipr)+u                                                      !/*pc        438*/
!                                                                               !/*pc        439*/
!     TRANSFORM TO LAB FRAME                                                    !/*pc        440*/
!                                                                               !/*pc        441*/
      fract=(zmass9(ipr)+dph*eps9)/(rnmass+zmass9(ipr)+eps9)                    !/*pc        442*/
      xkt=eps9*fract                                                            !/*pc        443*/
      xkp=eps9-xkt                                                              !/*pc        444*/
      epp=xkp+zmass9(ipr)                                                       !/*pc        445*/
      etp=xkt+rnmass                                                            !/*pc        446*/
      aaa=xkp*(xkp+dp2*zmass9(ipr))                                             !/*pc        447*/
      bbb=sqrt(aaa*(gamma9-dp1)*(gamma9+dp1))                                   !/*pc        448*/
      coscm=coslbr(1)*c(1)+coslbr(2)*c(2)+coslbr(3)*c(3)                        !/*pc        449*/
      eps9=gamma9*epp+bbb*coscm-zmass9(ipr)                                     !/*pc        450*/
      erec8=gamma9*etp-bbb*coscm-rnmass                                         !/*pc        451*/
      aa1=aaa**2*(dp1-coscm**2)                                                 !/*pc        452*/
      dnp=sqrt(aa1+(bbb*epp+gamma9*aaa*coscm)**2)                               !/*pc        453*/
      dnt=sqrt(aa1+(bbb*etp-gamma9*aaa*coscm)**2)                               !/*pc        454*/
      c1=bbb*epp+aaa*(gamma9-dp1)*coscm                                         !/*pc        455*/
      c2=bbb*etp-aaa*(gamma9-dp1)*coscm                                         !/*pc        456*/
      do 240 i=1,3                                                              !/*pc        457*/
      coslbp(i)=(c1*coslbr(i)+aaa*c(i))/dnp                                     !/*pc        458*/
      coslbr(i)=(c2*coslbr(i)-aaa*c(i))/dnt                                     !/*pc        459*/
  240 continue                                                                  !/*pc        460*/
      gamma9=dp1+erec8/rnmass                                                   !/*pc        461*/
  250 continue                                                                  !/*pc        462*/
      return                                                                    !/*pc        463*/
      end                                                                       !/*pc        464*/

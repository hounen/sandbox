!     $Id: levset.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                       !/*le          1*/
      subroutine levset (ipt,llev)                                              !/*le          2*/
#include <lcs/double.h>                                                         /*!le          3*/
!                                                                               !/*le          4*/
!  LEVSET calculated the branching probabilities for each nuclear               !/*le          5*/
!  level in the data library.                                                   !/*le          6*/
!                                                                               !/*le          7*/
!    REFERENCES.......  MOMENT    MASEDT                                        !/*le          8*/
!    EXTERNALS........  AMASS     CH2       CH3       CH4       EXITA           !/*le          9*/
!                       SQRT      VL        EXP       CLMB      ALOG            !/*le         10*/
!                       RANF                                                    !/*le         11*/
!                                                                               !/*le         12*/
#include <lcs/parms.h>                                                          /*!le         13*/
#include <lcs/fbrcom.h>                                                         /*!le         14*/
      dimension s(nmx), nss(nmx), inss(nmx), jlev(nmx), jpp(nmx),               !/*le         15*/
     & e0(nmx), fact(lchan), npp(nmx), ict(nmx), ef(nmx), wtl1(lchan)           !/*le         16*/
      data lo3 /1/                                                              !/*le         17*/
      data fact /dp1,dp2,6.d0/                                                  !/*le         18*/
      data hbc3 /7.683545d+06/, con1 /0.72d0/, con2 /.00516d0/,                 !/*le         19*/
     & con3 /.007168d0/, thibit /dp0/                                           !/*le         20*/
      data loff /0/                                                             !/*le         21*/
!                                                                               !/*le         22*/
! scan levels for instability                                                   !/*le         23*/
!                                                                               !/*le         24*/
      izr=jz(ipt)                                                               !/*le         25*/
      iar=ja(ipt)                                                               !/*le         26*/
      estar=e(jns(ipt)+llev)                                                    !/*le         27*/
      ti0=tiso(jns(ipt)+llev)                                                   !/*le         28*/
      pi=parity(jns(ipt)+llev)                                                  !/*le         29*/
      ajj0=aj(jns(ipt)+llev)                                                    !/*le         30*/
      emax=estar+1.d-2                                                          !/*le         31*/
      fzr=izr                                                                   !/*le         32*/
      k=0                                                                       !/*le         33*/
      fac=v(iar)/hbc3                                                           !/*le         34*/
      amr=amass(izr,iar,iok)                                                    !/*le         35*/
      if (iok.eq.0) then                                                        !/*le         36*/
        iok1=0                                                                  !/*le         37*/
        return                                                                  !/*le         38*/
      endif                                                                     !/*le         39*/
!                                                                               !/*le         40*/
!       first loop over no. of particles in breakup                             !/*le         41*/
!                                                                               !/*le         42*/
      nop=nmx                                                                   !/*le         43*/
      w2=dp1                                                                    !/*le         44*/
      if (nop.gt.iar) nop=iar                                                   !/*le         45*/
      wtot=dp0                                                                  !/*le         46*/
      kj=1                                                                      !/*le         47*/
      do 200 n=2,nop                                                            !/*le         48*/
      w2=w2*fac                                                                 !/*le         49*/
      w235=dph*w2*gam(n)                                                        !/*le         50*/
!                                                                               !/*le         51*/
!       find all channels with thes number of particles                         !/*le         52*/
!                                                                               !/*le         53*/
      if (n.le.2) then                                                          !/*le         54*/
        call ch2 (nchan)                                                        !/*le         55*/
        assign 160 to jbr                                                       !/*le         56*/
        assign 60 to jbr2                                                       !/*le         57*/
      else                                                                      !/*le         58*/
        call ch3 (nchan)                                                        !/*le         59*/
        assign 160 to jbr                                                       !/*le         60*/
        assign 40 to jbr2                                                       !/*le         61*/
      endif                                                                     !/*le         62*/
      if (nchan.le.0) go to 200                                                 !/*le         63*/
!                                                                               !/*le         64*/
!       loop over channels                                                      !/*le         65*/
!                                                                               !/*le         66*/
      do 190 i=1,nchan                                                          !/*le         67*/
!                                                                               !/*le         68*/
!       loop over no. of particles in channel                                   !/*le         69*/
!                                                                               !/*le         70*/
      q=amr                                                                     !/*le         71*/
      sum=dp0                                                                   !/*le         72*/
      prod=dp1                                                                  !/*le         73*/
      do 10 np=1,n                                                              !/*le         74*/
      iip=izap(np,i)                                                            !/*le         75*/
      jpp(np)=iip                                                               !/*le         76*/
      nss(np)=ns(iip)                                                           !/*le         77*/
      inss(np)=jns(iip)                                                         !/*le         78*/
      amm=axm(iip)                                                              !/*le         79*/
      q=q-amm                                                                   !/*le         80*/
      sum=sum+amm                                                               !/*le         81*/
      prod=prod*amm                                                             !/*le         82*/
   10 continue                                                                  !/*le         83*/
      tem4=emax+q                                                               !/*le         84*/
      if (tem4.le.dp0) go to 190                                                !/*le         85*/
      r=prod/sum                                                                !/*le         86*/
      facm=r*sqrt(r)                                                            !/*le         87*/
      ww=w235*facm                                                              !/*le         88*/
      couly=dp0                                                                 !/*le         89*/
      if (n.eq.2) then                                                          !/*le         90*/
        tem0=xminv(jpp(1))+xminv(jpp(2))                                        !/*le         91*/
        tem1=dp1/sqrt(tem0)                                                     !/*le         92*/
        iip1=jpp(1)                                                             !/*le         93*/
        iip2=jpp(2)                                                             !/*le         94*/
        couly=con3*(roh(iip1)+roh(iip2))*tem1                                   !/*le         95*/
      endif                                                                     !/*le         96*/
!                                                                               !/*le         97*/
      coulx=dp0                                                                 !/*le         98*/
      if (jpp(n-1).eq.1) go to 30                                               !/*le         99*/
      if (n.eq.2) then                                                          !/*le        100*/
        coulx=con2*z(iip1)*z(iip2)*tem1                                         !/*le        101*/
      else                                                                      !/*le        102*/
        tem=dp0                                                                 !/*le        103*/
        do 20 np=1,n                                                            !/*le        104*/
        iip=jpp(np)                                                             !/*le        105*/
        if (iip.gt.1) then                                                      !/*le        106*/
          zz=z(iip)                                                             !/*le        107*/
          iz1=izr-jz(iip)+1                                                     !/*le        108*/
          if (iz1.gt.0) then                                                    !/*le        109*/
            in1=iar-ja(iip)-iz1+2                                               !/*le        110*/
            iip1=ip(iz1,in1)                                                    !/*le        111*/
            rho2=roh(iip)+roh(iip1)                                             !/*le        112*/
            tem=tem+zz*(fzr-zz)/rho2                                            !/*le        113*/
          endif                                                                 !/*le        114*/
        endif                                                                   !/*le        115*/
   20   continue                                                                !/*le        116*/
        coulx=tem*con1                                                          !/*le        117*/
      endif                                                                     !/*le        118*/
   30 continue                                                                  !/*le        119*/
!                                                                               !/*le        120*/
!       loop over excited states for a given breakup_                           !/*le        121*/
!                                                                               !/*le        122*/
      ns1=nss(1)                                                                !/*le        123*/
      ns2=nss(2)                                                                !/*le        124*/
      lof1=inss(1)                                                              !/*le        125*/
      lof2=inss(2)                                                              !/*le        126*/
      do 180 j1=1,ns1                                                           !/*le        127*/
      if (jpp(1).eq.jpp(2).and.j1.gt.1) go to 180                               !/*le        128*/
      jlev(1)=j1                                                                !/*le        129*/
      e0(1)=e(lof1+j1)                                                          !/*le        130*/
      ajj1=aj(lof1+j1)                                                          !/*le        131*/
      ti1=tiso(lof1+j1)                                                         !/*le        132*/
      pi1=pi*parity(lof1+j1)                                                    !/*le        133*/
      s(1)=dp2*ajj1+dp1                                                         !/*le        134*/
      wtl1(1)=wtl(lof1+j1)                                                      !/*le        135*/
      lo2=1                                                                     !/*le        136*/
      ns2=1                                                                     !/*le        137*/
      if (j1.eq.1) ns2=nss(2)                                                   !/*le        138*/
      do 170 j2=lo2,ns2                                                         !/*le        139*/
      jlev(3)=1                                                                 !/*le        140*/
      e0(2)=e(lof2+j2)                                                          !/*le        141*/
      ef(2)=e0(1)+e0(2)                                                         !/*le        142*/
      if (ef(2).ge.tem4) go to 170                                              !/*le        143*/
      jlev(2)=j2                                                                !/*le        144*/
      ajj2=aj(lof2+j2)                                                          !/*le        145*/
      ti2=tiso(lof2+j2)                                                         !/*le        146*/
      pi2=pi1*parity(lof2+j2)                                                   !/*le        147*/
      s(2)=s(1)*(ajj2+ajj2+dp1)                                                 !/*le        148*/
      wtl1(2)=wtl(lof2+j2)                                                      !/*le        149*/
      j3=1                                                                      !/*le        150*/
      go to jbr2, (60,40)                                                       !/*le        151*/
   40 continue                                                                  !/*le        152*/
      if (j1.eq.1.and.j2.eq.1) j3=nss(3)                                        !/*le        153*/
      if (jpp(3).eq.jpp(2).and.j2.gt.1) go to 170                               !/*le        154*/
      lof3=inss(3)                                                              !/*le        155*/
   50 continue                                                                  !/*le        156*/
      e0(3)=e(lof3+j3)                                                          !/*le        157*/
      ef(3)=ef(2)+e0(3)                                                         !/*le        158*/
      if (ef(3).ge.tem4) go to 160                                              !/*le        159*/
      jlev(3)=j3                                                                !/*le        160*/
      ajj3=aj(lof3+j3)                                                          !/*le        161*/
      ti3=tiso(lof3+j3)                                                         !/*le        162*/
      pi3=pi2*parity(lof3+j3)                                                   !/*le        163*/
      s(3)=s(2)*(ajj3+ajj3+dp1)                                                 !/*le        164*/
      wtl1(3)=wtl(lof3+j3)                                                      !/*le        165*/
   60 continue                                                                  !/*le        166*/
      tem3=q-ef(n)                                                              !/*le        167*/
      t0=estar+tem3                                                             !/*le        168*/
      if (t0.lt.dp0) go to 150                                                  !/*le        169*/
      k=k+1                                                                     !/*le        170*/
      q01=tem3                                                                  !/*le        171*/
      gg=dp1                                                                    !/*le        172*/
      ww1=ww                                                                    !/*le        173*/
      if (n.lt.3) then                                                          !/*le        174*/
        wtl0=wtl1(1)*wtl1(2)                                                    !/*le        175*/
        tmax=ti1+ti2                                                            !/*le        176*/
        if (ti0.gt.tmax) ww1=ww1*thibit                                         !/*le        177*/
        tmin=abs(ti1-ti2)                                                       !/*le        178*/
        if (ti0.lt.tmin) ww1=ww1*thibit                                         !/*le        179*/
        lmin=0                                                                  !/*le        180*/
        ajmax=ajj1+ajj2                                                         !/*le        181*/
        if (ajj0.gt.ajmax) then                                                 !/*le        182*/
          lmin=ajj0-ajmax+1.d-3                                                 !/*le        183*/
        else                                                                    !/*le        184*/
          ajmin=abs(ajj1-ajj2)                                                  !/*le        185*/
          if (ajj0.lt.ajmin) lmin=ajmin-ajj0+1.d-3                              !/*le        186*/
        endif                                                                   !/*le        187*/
        if (lmin.gt.0) pi2=pi2*(-dp1)**lmin                                     !/*le        188*/
        if (pi2.lt.dp0) lmin=lmin+1                                             !/*le        189*/
        lmax=ajj0+ajmax                                                         !/*le        190*/
        if (lmin.gt.lmax) then                                                  !/*le        191*/
          k=k-1                                                                 !/*le        192*/
          go to 150                                                             !/*le        193*/
        endif                                                                   !/*le        194*/
        ww1=ww1*vl(t0*couly**2,lmin)                                            !/*le        195*/
        if (jpp(1).eq.jpp(2).and.e0(1).eq.e0(2)) gg=dp2                         !/*le        196*/
        go to 110                                                               !/*le        197*/
      endif                                                                     !/*le        198*/
      ajmax=ajj1+ajj2+ajj3                                                      !/*le        199*/
      lmax=ajj0+ajmax                                                           !/*le        200*/
      lmin=0                                                                    !/*le        201*/
      if (ajj0.ge.ajmax) then                                                   !/*le        202*/
        lmin=ajj0-ajmax                                                         !/*le        203*/
      else                                                                      !/*le        204*/
        if (ajj1-ajj2-ajj3.ge.dp0) then                                         !/*le        205*/
          ajmin=ajj1-ajj2-ajj3                                                  !/*le        206*/
        elseif (abs(ajj2-ajj3)-ajj1.ge.dp0) then                                !/*le        207*/
          ajmin=abs(ajj2-ajj3)-ajj1                                             !/*le        208*/
        else                                                                    !/*le        209*/
          ajmin=mod(ajmax,dp1)                                                  !/*le        210*/
        endif                                                                   !/*le        211*/
        if (ajj0.le.ajmin) lmin=ajmin-ajj0                                      !/*le        212*/
      endif                                                                     !/*le        213*/
      if (lmin.gt.0) pi3=pi3*(-dp1)**lmin                                       !/*le        214*/
      if (pi3.lt.dp0) lmin=lmin+1                                               !/*le        215*/
      if (lmin.gt.lmax) then                                                    !/*le        216*/
        k=k-1                                                                   !/*le        217*/
        go to 150                                                               !/*le        218*/
      endif                                                                     !/*le        219*/
      tmax=ti1+ti2+ti3                                                          !/*le        220*/
      if (ti0.gt.tmax) ww1=ww1*thibit                                           !/*le        221*/
      t1m=max(ti1,ti2,ti3)                                                      !/*le        222*/
      tmin=max(dp0,dp2*t1m-tmax)                                                !/*le        223*/
      if (ti0.lt.tmin) ww1=ww1*thibit                                           !/*le        224*/
      nkind=0                                                                   !/*le        225*/
      wtl0=dp1                                                                  !/*le        226*/
      do 70 iii=1,n                                                             !/*le        227*/
      ict(iii)=jpp(iii)                                                         !/*le        228*/
      wtl0=wtl0*wtl1(iii)                                                       !/*le        229*/
   70 continue                                                                  !/*le        230*/
      do 90 iii=1,n                                                             !/*le        231*/
!                                                                               !/*le        232*/
!       check that this particle has not                                        !/*le        233*/
!       already been counted                                                    !/*le        234*/
!                                                                               !/*le        235*/
      if (ict(iii).eq.0) go to 90                                               !/*le        236*/
      nkind=nkind+1                                                             !/*le        237*/
      npp(nkind)=1                                                              !/*le        238*/
      if (iii.eq.n) go to 90                                                    !/*le        239*/
      et=e0(iii)                                                                !/*le        240*/
      ica=ict(iii)                                                              !/*le        241*/
!                                                                               !/*le        242*/
!       now scan the rest of the particles for a match                          !/*le        243*/
!                                                                               !/*le        244*/
      i1=iii+1                                                                  !/*le        245*/
      do 80 ii=i1,n                                                             !/*le        246*/
      if (ict(ii).gt.ica) go to 90                                              !/*le        247*/
      if (ict(ii).eq.ica.and.e0(ii).eq.et) then                                 !/*le        248*/
        npp(nkind)=npp(nkind)+1                                                 !/*le        249*/
        ict(ii)=0                                                               !/*le        250*/
      endif                                                                     !/*le        251*/
   80 continue                                                                  !/*le        252*/
   90 continue                                                                  !/*le        253*/
!                                                                               !/*le        254*/
!       compute g factor                                                        !/*le        255*/
!                                                                               !/*le        256*/
      if (n-nkind.gt.1) then                                                    !/*le        257*/
        do 100 iii=1,nkind                                                      !/*le        258*/
        npi=npp(iii)                                                            !/*le        259*/
  100   gg=gg*fact(npi)                                                         !/*le        260*/
      elseif (n-nkind.eq.1) then                                                !/*le        261*/
        gg=dp2                                                                  !/*le        262*/
      endif                                                                     !/*le        263*/
  110 continue                                                                  !/*le        264*/
      if (ww1.le.dp0) then                                                      !/*le        265*/
        k=k-1                                                                   !/*le        266*/
        go to 150                                                               !/*le        267*/
      endif                                                                     !/*le        268*/
      w00=ww1*s(n)*wtl0/gg                                                      !/*le        269*/
      m0=n                                                                      !/*le        270*/
      if (m0.gt.2) texp=1.5d0*m0-2.5d0                                          !/*le        271*/
      if (coulx.eq.dp0) go to 130                                               !/*le        272*/
      if (m0.eq.2) go to 120                                                    !/*le        273*/
      tem2=coulx/t0                                                             !/*le        274*/
      if (tem2.gt.100.d0) tem2=100.d0                                           !/*le        275*/
      w00=w00*exp(-tem2)                                                        !/*le        276*/
      go to 130                                                                 !/*le        277*/
  120 continue                                                                  !/*le        278*/
      tem2=sqrt(t0)                                                             !/*le        279*/
      eta=coulx/tem2                                                            !/*le        280*/
      rhox=couly*tem2                                                           !/*le        281*/
      w00=w00*max(dp1m10,clmb1(rhox,eta,ml))                                    !/*le        282*/
  130 continue                                                                  !/*le        283*/
      if (m0.eq.2) then                                                         !/*le        284*/
        wtot=wtot+w00*sqrt(t0)                                                  !/*le        285*/
      else                                                                      !/*le        286*/
        wtot=wtot+w00*exp(texp*log(t0))                                         !/*le        287*/
      endif                                                                     !/*le        288*/
      jj=jlev(1)+jlev(2)+jlev(3)-3                                              !/*le        289*/
      ix=1                                                                      !/*le        290*/
      wf(loff+kj)=wtot                                                          !/*le        291*/
      q1(loff+kj)=q01+estar                                                     !/*le        292*/
      n1(loff+kj)=m0                                                            !/*le        293*/
      do 140 l=1,m0                                                             !/*le        294*/
      izap1(l,loff+kj)=jpp(l)                                                   !/*le        295*/
      if (jlev(l).gt.1) ix=l                                                    !/*le        296*/
  140 continue                                                                  !/*le        297*/
      jjix=3*jj+ix-1                                                            !/*le        298*/
      klev1(loff+kj)=jjix                                                       !/*le        299*/
      kj=kj+1                                                                   !/*le        300*/
  150 continue                                                                  !/*le        301*/
  160 j3=j3-1                                                                   !/*le        302*/
      if (j3.ge.lo3) go to 50                                                   !/*le        303*/
  170 continue                                                                  !/*le        304*/
  180 continue                                                                  !/*le        305*/
  190 continue                                                                  !/*le        306*/
  200 continue                                                                  !/*le        307*/
      if (k.eq.0.or.kj.eq.1) then                                               !/*le        308*/
!                                                                               !/*le        309*/
!       kj=1; res.nuc. does not have enough ex.en. to breakup                   !/*le        310*/
!                                                                               !/*le        311*/
        return                                                                  !/*le        312*/
      endif                                                                     !/*le        313*/
      if (k.eq.0) kj=1                                                          !/*le        314*/
      ki=kj-1                                                                   !/*le        315*/
      if ((loff+ki).gt.mch5) go to 220                                          !/*le        316*/
      il0(jns(ipt)+llev)=loff                                                   !/*le        317*/
      nklev(jns(ipt)+llev)=ki                                                   !/*le        318*/
      if (ki.le.0) go to 210                                                    !/*le        319*/
      loff=loff+ki                                                              !/*le        320*/
  210 continue                                                                  !/*le        321*/
      return                                                                    !/*le        322*/
  220 continue                                                                  !/*le        323*/
      call erprnt(1,1,0,0,0,0,0,0,'26hLEVSET.  (loff+ki).gt.mch5')              !/*le        324*/
      end                                                                       !/*le        325*/

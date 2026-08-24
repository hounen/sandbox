!     $Id: cbrset.F,v 1.5 2002/03/27 18:14:57 gwm Exp $                       !/*cb          1*/
      subroutine cbrset (izr,iar,ex,nmax)                                       !/*cb          2*/
#include <lcs/double.h>                                                         /*!cb          3*/
!                                                                               !/*cb          4*/
!  CBRSET calculates all the excitation energy-independent factors that         !/*cb          5*/
!  enter the breakup channel probabilities for an arbitrary excited             !/*cb          6*/
!  state.                                                                       !/*cb          7*/
!                                                                               !/*cb          8*/
!     REFERENCES.......  FBRK                                                   !/*cb          9*/
!     ENTRY POINTS.....  CBRAK1                                                 !/*cb         10*/
!     EXTERNALS........  AMASS     CH2       CH3       CH4       CH5            !/*cb         11*/
!                       CH6       CH7       EXITA     SQRT      CLMB            !/*cb         12*/
!                       EXP       ALOG      RANF                                !/*cb         13*/
!                                                                               !/*cb         14*/
#include <lcs/parms.h>                                                          /*!cb         15*/
#include <lcs/fbrcom.h>                                                         /*!cb         16*/
      dimension s(lchan), nss(lchan), inss(lchan), jlev(lchan),                 !/*cb         17*/
     & jpp(lchan), e0(lchan), fact(lchan), npp(lchan), ict(lchan),              !/*cb         18*/
     & ef(lchan), wtl1(lchan)                                                   !/*cb         19*/
      data index /1/                                                            !/*cb         20*/
      data fact /dp1,dp2,6.d0/                                                  !/*cb         21*/
      data rho0 /1.414d0/                                                       !/*cb         22*/
      data hbc3 /7.683545d+06/, con1 /0.72d0/, con2 /.00516d0/, con3 /.0        !/*cb         23*/
     & 07168d0/                                                                 !/*cb         24*/
      amr=amass(izr,iar,iok)                                                    !/*cb         25*/
      if (iok.eq.0) then                                                        !/*cb         26*/
        iok1=0                                                                  !/*cb         27*/
        return                                                                  !/*cb         28*/
      endif                                                                     !/*cb         29*/
      iok1=1                                                                    !/*cb         30*/
      ipt=iok                                                                   !/*cb         31*/
      esmax=1.001d0*ex                                                          !/*cb         32*/
      fzr=izr                                                                   !/*cb         33*/
      k=index-1                                                                 !/*cb         34*/
      nct=0                                                                     !/*cb         35*/
      ioff=0                                                                    !/*cb         36*/
      fac=v(iar)/hbc3                                                           !/*cb         37*/
!                                                                               !/*cb         38*/
!       first loop over no. of particles in breakup                             !/*cb         39*/
!                                                                               !/*cb         40*/
      nop=nmax                                                                  !/*cb         41*/
      w2=dp1                                                                    !/*cb         42*/
      nop=min(iar,nmax)                                                         !/*cb         43*/
      do 210 n=2,nop                                                            !/*cb         44*/
      w2=w2*fac                                                                 !/*cb         45*/
      w235=dph*w2*gam(n)                                                        !/*cb         46*/
!                                                                               !/*cb         47*/
!       find all channels with thes number of particles                         !/*cb         48*/
!                                                                               !/*cb         49*/
      if (n-3) 30,40,20                                                         !/*cb         50*/
   20 call erprnt(1,1,0,0,0,0,0,0,'15hCBRSET.  n.gt.3')                         !/*cb         51*/
   30 call ch2 (nchan)                                                          !/*cb         52*/
      assign 110 to jbr2                                                        !/*cb         53*/
      go to 50                                                                  !/*cb         54*/
   40 call ch3 (nchan)                                                          !/*cb         55*/
      assign 90 to jbr2                                                         !/*cb         56*/
   50 continue                                                                  !/*cb         57*/
      if (nchan.le.0) go to 210                                                 !/*cb         58*/
!                                                                               !/*cb         59*/
!       loop over channels                                                      !/*cb         60*/
!                                                                               !/*cb         61*/
      do 200 i=1,nchan                                                          !/*cb         62*/
!                                                                               !/*cb         63*/
!       loop over no. of particles in channel                                   !/*cb         64*/
!                                                                               !/*cb         65*/
      q=amr                                                                     !/*cb         66*/
      sum=dp0                                                                   !/*cb         67*/
      prod=dp1                                                                  !/*cb         68*/
      do 60 np=1,n                                                              !/*cb         69*/
      iip=izap(np,i)                                                            !/*cb         70*/
      jpp(np)=iip                                                               !/*cb         71*/
      nss(np)=ns(iip)                                                           !/*cb         72*/
      inss(np)=jns(iip)                                                         !/*cb         73*/
      amm=axm(iip)                                                              !/*cb         74*/
      q=q-amm                                                                   !/*cb         75*/
      sum=sum+amm                                                               !/*cb         76*/
      prod=prod*amm                                                             !/*cb         77*/
   60 continue                                                                  !/*cb         78*/
      tem4=esmax+q                                                              !/*cb         79*/
      if (tem4.le.dp0) go to 200                                                !/*cb         80*/
      r=prod/sum                                                                !/*cb         81*/
      facm=r*sqrt(r)                                                            !/*cb         82*/
      ww=w235*facm                                                              !/*cb         83*/
!                                                                               !/*cb         84*/
      coulx=dp0                                                                 !/*cb         85*/
      if (jpp(n-1).eq.1) go to 80                                               !/*cb         86*/
      if (n.eq.2) then                                                          !/*cb         87*/
        iip1=jpp(1)                                                             !/*cb         88*/
        iip2=jpp(2)                                                             !/*cb         89*/
        tem1=dp1/sqrt(xminv(iip1)+xminv(iip2))                                  !/*cb         90*/
        coulx=con2*z(iip1)*z(iip2)*tem1                                         !/*cb         91*/
        couly=con3*(roh(iip1)+roh(iip2))*tem1                                   !/*cb         92*/
      else                                                                      !/*cb         93*/
        tem=dp0                                                                 !/*cb         94*/
        do 70 np=1,n                                                            !/*cb         95*/
        iip=jpp(np)                                                             !/*cb         96*/
        if (iip.eq.1) go to 70                                                  !/*cb         97*/
        iz1=izr-jz(iip)+1                                                       !/*cb         98*/
        if (iz1.le.0) go to 70                                                  !/*cb         99*/
        in1=iar-ja(iip)-iz1+2                                                   !/*cb        100*/
        iip1=ip(iz1,in1)                                                        !/*cb        101*/
        if (iip1.le.0) then                                                     !/*cb        102*/
          aa=(iz1+in1-2)                                                        !/*cb        103*/
          rho2=roh(iip)+rho0*aa**dpth+dp1                                       !/*cb        104*/
        else                                                                    !/*cb        105*/
          rho2=roh(iip)+roh(iip1)                                               !/*cb        106*/
        endif                                                                   !/*cb        107*/
        couly=dp0                                                               !/*cb        108*/
        zz=z(iip)                                                               !/*cb        109*/
        tem=tem+zz*(fzr-zz)/rho2                                                !/*cb        110*/
   70   continue                                                                !/*cb        111*/
        coulx=tem*con1                                                          !/*cb        112*/
      endif                                                                     !/*cb        113*/
   80 continue                                                                  !/*cb        114*/
!                                                                               !/*cb        115*/
!     loop over excited states for a given breakup_                             !/*cb        116*/
!                                                                               !/*cb        117*/
      ns1=nss(1)                                                                !/*cb        118*/
      lof1=inss(1)                                                              !/*cb        119*/
      lof2=inss(2)                                                              !/*cb        120*/
      do 190 j1=1,ns1                                                           !/*cb        121*/
      if (jpp(1).eq.jpp(2).and.j1.gt.1) go to 190                               !/*cb        122*/
      jlev(1)=j1                                                                !/*cb        123*/
      e0(1)=e(lof1+j1)                                                          !/*cb        124*/
      ajj=aj(lof1+j1)                                                           !/*cb        125*/
      s(1)=dp2*ajj+dp1                                                          !/*cb        126*/
      wtl1(1)=wtl(lof1+j1)                                                      !/*cb        127*/
      lo2=1                                                                     !/*cb        128*/
      ns2=1                                                                     !/*cb        129*/
      if (j1.eq.1) ns2=nss(2)                                                   !/*cb        130*/
      do 180 j2=lo2,ns2                                                         !/*cb        131*/
      e0(2)=e(lof2+j2)                                                          !/*cb        132*/
      ef(2)=e0(1)+e0(2)                                                         !/*cb        133*/
      if (ef(2).ge.tem4) go to 180                                              !/*cb        134*/
      jlev(2)=j2                                                                !/*cb        135*/
      ajj=aj(lof2+j2)                                                           !/*cb        136*/
      s(2)=s(1)*(ajj+ajj+dp1)                                                   !/*cb        137*/
      wtl1(2)=wtl(lof2+j2)                                                      !/*cb        138*/
      go to jbr2, (110,90)                                                      !/*cb        139*/
   90 continue                                                                  !/*cb        140*/
      j3=1                                                                      !/*cb        141*/
      if (j1.eq.1.and.j2.eq.1) j3=nss(3)                                        !/*cb        142*/
      if (jpp(3).eq.jpp(2).and.j2.gt.1) go to 180                               !/*cb        143*/
      lo3=1                                                                     !/*cb        144*/
      lof3=inss(3)                                                              !/*cb        145*/
  100 continue                                                                  !/*cb        146*/
      e0(3)=e(lof3+j3)                                                          !/*cb        147*/
      ef(3)=ef(2)+e0(3)                                                         !/*cb        148*/
      if (ef(3).ge.tem4) go to 170                                              !/*cb        149*/
      jlev(3)=j3                                                                !/*cb        150*/
      ajj=aj(lof3+j3)                                                           !/*cb        151*/
      s(3)=s(2)*(ajj+ajj+dp1)                                                   !/*cb        152*/
      wtl1(3)=wtl(lof3+j3)                                                      !/*cb        153*/
  110 continue                                                                  !/*cb        154*/
      tem3=q-ef(n)                                                              !/*cb        155*/
      k=k+1                                                                     !/*cb        156*/
      if (k.gt.mch1.or.nct.gt.mch6) call erprnt(1,1,0,0,0,0,0,0,                !/*cb        157*/
     & '33hCBRSET.  k.gt.mch1.or.nct.gt.mch6')                                  !/*cb        158*/
      nct=nct+1                                                                 !/*cb        159*/
      q0(k)=tem3                                                                !/*cb        160*/
      gg=dp1                                                                    !/*cb        161*/
      if (n.eq.2) then                                                          !/*cb        162*/
        jj=jlev(1)+jlev(2)-2                                                    !/*cb        163*/
        ix=2                                                                    !/*cb        164*/
        if (jlev(1).gt.1) ix=1                                                  !/*cb        165*/
        jjix=3*jj+ix-1                                                          !/*cb        166*/
        izaps(k)=jpp(1)+100*(jpp(2)+10000*jjix)                                 !/*cb        167*/
        wtl0=wtl1(1)*wtl1(2)                                                    !/*cb        168*/
        if (jpp(1).eq.jpp(2).and.e0(1).eq.e0(2)) gg=dp2                         !/*cb        169*/
        go to 160                                                               !/*cb        170*/
      endif                                                                     !/*cb        171*/
      jj=jlev(1)+jlev(2)+jlev(3)-3                                              !/*cb        172*/
      ix=3                                                                      !/*cb        173*/
      if (jlev(1).gt.1) ix=1                                                    !/*cb        174*/
      if (jlev(2).gt.1) ix=2                                                    !/*cb        175*/
      jjix=3*jj+ix-1                                                            !/*cb        176*/
      izaps(k)=jpp(1)+100*(jpp(2)+100*(jpp(3)+100*jjix))                        !/*cb        177*/
      nkind=0                                                                   !/*cb        178*/
      wtl0=dp1                                                                  !/*cb        179*/
      do 120 iii=1,n                                                            !/*cb        180*/
      ict(iii)=jpp(iii)                                                         !/*cb        181*/
      wtl0=wtl0*wtl1(iii)                                                       !/*cb        182*/
  120 continue                                                                  !/*cb        183*/
      do 140 iii=1,n                                                            !/*cb        184*/
!                                                                               !/*cb        185*/
!     check that this particle has not already been counted                     !/*cb        186*/
!                                                                               !/*cb        187*/
      if (ict(iii).eq.0) go to 140                                              !/*cb        188*/
      nkind=nkind+1                                                             !/*cb        189*/
      npp(nkind)=1                                                              !/*cb        190*/
      if (iii.eq.n) go to 140                                                   !/*cb        191*/
      et=e0(iii)                                                                !/*cb        192*/
      ica=ict(iii)                                                              !/*cb        193*/
!                                                                               !/*cb        194*/
!     now scan the rest of the particles for a match                            !/*cb        195*/
!                                                                               !/*cb        196*/
      i1=iii+1                                                                  !/*cb        197*/
      do 130 ii=i1,n                                                            !/*cb        198*/
      if (ict(ii).gt.ica) go to 140                                             !/*cb        199*/
      if (ict(ii).eq.ica.and.e0(ii).eq.et) then                                 !/*cb        200*/
        npp(nkind)=npp(nkind)+1                                                 !/*cb        201*/
        ict(ii)=0                                                               !/*cb        202*/
      endif                                                                     !/*cb        203*/
  130 continue                                                                  !/*cb        204*/
  140 continue                                                                  !/*cb        205*/
!                                                                               !/*cb        206*/
!       compute g factor                                                        !/*cb        207*/
!                                                                               !/*cb        208*/
      if (n-nkind.gt.1) then                                                    !/*cb        209*/
        do 150 iii=1,nkind                                                      !/*cb        210*/
        npi=npp(iii)                                                            !/*cb        211*/
  150   gg=gg*fact(npi)                                                         !/*cb        212*/
      elseif (n-nkind.eq.1) then                                                !/*cb        213*/
        gg=dp2                                                                  !/*cb        214*/
      endif                                                                     !/*cb        215*/
  160 continue                                                                  !/*cb        216*/
      ioff=ioff+n                                                               !/*cb        217*/
      w0(k)=ww*s(n)/gg                                                          !/*cb        218*/
      w0(k)=w0(k)*wtl0                                                          !/*cb        219*/
      coulch(k)=coulx                                                           !/*cb        220*/
      coulck(k)=couly                                                           !/*cb        221*/
      if (n.eq.2) go to 180                                                     !/*cb        222*/
  170 j3=j3-1                                                                   !/*cb        223*/
      if (j3.ge.lo3) go to 100                                                  !/*cb        224*/
  180 continue                                                                  !/*cb        225*/
  190 continue                                                                  !/*cb        226*/
  200 continue                                                                  !/*cb        227*/
  210 continue                                                                  !/*cb        228*/
      ipdat(ipt)=index                                                          !/*cb        229*/
      npdat(ipt)=nct                                                            !/*cb        230*/
      index=index+nct                                                           !/*cb        231*/
      return                                                                    !/*cb        232*/
      end                                                                       !/*cb        233*/

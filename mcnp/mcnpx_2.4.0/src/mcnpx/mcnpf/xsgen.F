!#ident xsgen.F from deck xg                                                    !/*xg          1*/
      subroutine xsgen(nt)                                                      !/*xg          2*/
!        generate electron cross-section tables for all the materials.          !/*xg          3*/
!        nt = number of elementary electron cross-section tables                !/*xg          4*/
      use global_data                                                           !/*xgx         1*/
!                                                                               !/*xgx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*xgx         3*/
      implicit integer(kindi) (i-n)                                             !/*xgx         4*/
!                                                                               !/*xgx         5*/
      dimension ns(94),al(maxi),p(243,4)                                        !/*xg          6*/
      data ns/5*2,4*3,3*4,9*5,7*6,11*7,10*8,5*9,10*10,5*11,9*12,6*13,           !/*xg          7*/
     & 7*14,3*15/                                                               !/*xg          8*/
      data al/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,12.,15.,18.,21.,24.,27.,30.,        !/*xg          9*/
     & 35.,40.,45.,50.,60.,70.,80.,90.,100.,110.,120.,130.,140.,150.,           !/*xg         10*/
     & 160.,170.,180./                                                          !/*xg         11*/
!                                                                               !/*xg4c        1*/
!        integrated tiger series (its) method before its3.0.                    !/*xg4c        2*/
      nstp=mstp                                                                 !/*xg4c        3*/
      if(its30.eq.3)go to 14                                                    !/*xg4c        4*/
      ntop=49                                                                   !/*xg4c        5*/
      nwng=25                                                                   !/*xg4c        6*/
      nstp=8                                                                    !/*xg4c        7*/
      do 10 i=1,mtop                                                            !/*xg4c        8*/
   10 rkt(i)=rktc(i)                                                            !/*xg4c        9*/
      do 12 i=1,nwng                                                            !/*xg4c       10*/
   12 rka(i)=rkt(ntop-2*i+2)                                                    !/*xg4c       11*/
!                                                                               !/*xg         12*/
!        prepare the elementary cross-section tables.                           !/*xg         13*/
   14 do 80 j=mxe+1,mxe+nt                                                      !/*xg4c       12*/
      z=nxs(lnxs+2,j)                                                           !/*xg         15*/
!                                                                               !/*xg         16*/
!        prepare for log interpolation in energy.                               !/*xg         17*/
!        .000579=(electron_radius)**2/fine_structure_constant*1e24              !/*xg4c       13*/
!                                                                               !/*xg4c       14*/
!        pre its3.0 method:                                                     !/*xg4c       15*/
      if(nxs(lnxs+16,j).eq.3)go to 24                                           !/*xg4c       16*/
      do 16 i=1,nxs(lnxs+3,j)                                                   !/*xg4c       17*/
      exs(jxs(ljxs+2,j)+i-1)=log(exs(jxs(ljxs+2,j)+i-1)/gpt(3))                 !/*xg4c       18*/
   16 exs(jxs(ljxs+2,j)+nxs(lnxs+3,j)+i-1)=                                     !/*xg4c       19*/
     & .000579*z*(z+1.)*exs(jxs(ljxs+2,j)+nxs(lnxs+3,j)+i-1)                    !/*xg4c       20*/
      do 18 i=1,nxs(lnxs+5,j)                                                   !/*xg4c       21*/
   18 exs(jxs(ljxs+5,j)+i-1)=log(exs(jxs(ljxs+5,j)+i-1)/gpt(3))                 !/*xg4c       22*/
      do 20 i=1,nxs(lnxs+6,j)                                                   !/*xg4c       23*/
      exs(jxs(ljxs+6,j)+i-1)=log(exs(jxs(ljxs+6,j)+i-1)/gpt(3))                 !/*xg4c       24*/
   20 exs(jxs(ljxs+6,j)+nxs(lnxs+6,j)+i-1)=                                     !/*xg4c       25*/
     & log(.001*z**2*exs(jxs(ljxs+6,j)+nxs(lnxs+6,j)+i-1))                      !/*xg4c       26*/
      go to 36                                                                  !/*xg4c       27*/
!                                                                               !/*xg4c       28*/
!        its3.0 and later method:                                               !/*xg4c       29*/
!        z+1 has been replaced with z+eta, where eta accounts                   !/*xg4c       30*/
!        for electron-electron bremsstrahlung.                                  !/*xg4c       31*/
!        "cross sections for bremsstrahlung production and electron-            !/*xg4c       32*/
!        impact ionization," s. m. selzter, in monte carlo transport            !/*xg4c       33*/
!        of electrons and photons, t. m. jenkins, w. r. nelson, and             !/*xg4c       34*/
!        a. rindi, eds.                                                         !/*xg4c       35*/
!        for tracking, approximate the classical electron radius as             !/*xg4c       36*/
!        cr=planck*slite/(2*pie*fscon*gpt(3))=2.8179380d-13.                    !/*xg4c       37*/
   24 cr=2.8179380d-13                                                          !/*xg4c       38*/
      f=cr*cr/fscon*1.d24                                                       !/*xg4c       39*/
      do 26 i=1,nxs(lnxs+3,j)                                                   !/*xg4c       40*/
      exs(jxs(ljxs+2,j)+i-1)=log(exs(jxs(ljxs+2,j)+i-1)/gpt(3))                 !/*xg4c       41*/
      f1=exs(jxs(ljxs+2,j)+2*nxs(lnxs+3,j)+i-1)                                 !/*xg4c       42*/
   26 exs(jxs(ljxs+2,j)+nxs(lnxs+3,j)+i-1)=                                     !/*xg4c       43*/
     & f*z*(z+f1)*exs(jxs(ljxs+2,j)+nxs(lnxs+3,j)+i-1)                          !/*xg4c       44*/
      if(nxs(lnxs+9,j).gt.mtop)call erprnt(1,1,2,nxs(lnxs+9,j),mtop,0,          !/*xg4c       45*/
     & 0,1,'49helectron library error: nxs(9) greater than mtop.')              !/*xg4c       46*/
      if(ntop.ne.0)go to 36                                                     !/*xg4c       47*/
      ntop=nxs(lnxs+9,j)                                                        !/*xg4c       48*/
      nwng=nxs(lnxs+10,j)                                                       !/*xg4c       49*/
      if(nwng.gt.34)call erprnt(1,1,2,nxs(lnxs+10,j),34,0,0,1,                  !/*xg4c       50*/
     & '48helectron library error: nxs(10) greater than 34.')                   !/*xg4c       51*/
      if(bbrem(1).eq.0.)go to 32                                                !/*xg4c       52*/
      n7=max(1,ntop-47)                                                         !/*xg4c       53*/
      do 28 i=ntop,n7,-1                                                        !/*xg4c       54*/
   28 eba(leba+i,1)=bbrem(49-ntop+i)                                            !/*xg4c       55*/
      n8=max(1,ntop-48)                                                         !/*xg4c       56*/
      do 30 i=n8,1,-1                                                           !/*xg4c       57*/
      eba(leba+i,1)=bbrem(1)+(bbrem(2)-bbrem(1))*                               !/*xg4c       58*/
     & (exs(jxs(ljxs+9,j)+i-1)-exs(jxs(ljxs+9,j)))/                             !/*xg4c       59*/
     & (exs(jxs(ljxs+9,j)+n8)-exs(jxs(ljxs+9,j)))                               !/*xg4c       60*/
   30 if(eba(leba+i,1).le.0.)eba(leba+i,1)=max(bbrem(1),eba(leba+1,1))          !/*xg4c       61*/
!                                                                               !/*xg4c       62*/
!        load photon energy ratios from database into fixed arrays.             !/*xg4c       63*/
!        also load in interpolated bbrem values if needed.                      !/*xg4c       64*/
   32 do 34 i=1,nxs(lnxs+9,j)                                                   !/*xg4c       65*/
      rkt(i)=exs(jxs(ljxs+9,j)+i-1)                                             !/*xg4c       66*/
      if(i.le.nxs(lnxs+10,j))rka(i)=exs(jxs(ljxs+10,j)+i-1)                     !/*xg4c       67*/
      if(bbrem(1).eq.0.)go to 34                                                !/*xg4c       68*/
      bbrem(i)=eba(leba+i,1)                                                    !/*xg4c       69*/
      eba(leba+i,1)=0.                                                          !/*xg4c       70*/
   34 continue                                                                  !/*xg4c       71*/
!                                                                               !/*xg         28*/
!        set up table for low energy single scattering calculation.             !/*xg         29*/
   36 do 70 n=1,9                                                               !/*xg4c       72*/
      b=exs(jxs(ljxs+4,j)+14*(n-1)+13)                                          !/*xg         31*/
      db=24.*(b*(1.+b))**3                                                      !/*xg         32*/
      p(1,4)=(b*(1.+3.*b))/db                                                   !/*xg         33*/
      p(2,4)=3.*b*(1.+b)/db                                                     !/*xg         34*/
      p(3,4)=(1.-10.*(1.+b)**2)*p(1,4)+(15.+20.*b)*p(2,4)/3.+                   !/*xg         35*/
     & 2.5*(log((1.+b)/b)-(1./(1.+b)))                                          !/*xg         36*/
      a=2.*b*(1.+3.*b+3.*b**2)/db                                               !/*xg         37*/
      do 40 l=4,243                                                             !/*xg         38*/
   40 p(l,4)=((1.+2.*b)*(2*l-1)*p(l-1,4)-(l+2)*p(l-2,4)-a*(2*l-1))/(l-3)        !/*xg         39*/
      do 50 k=1,3                                                               !/*xg         40*/
      p(243,4-k)=1.                                                             !/*xg         41*/
      p(1,4-k)=2.*((1.+b)*p(1,5-k)-third*p(2,5-k))                              !/*xg         42*/
      do 50 l=2,242                                                             !/*xg         43*/
   50 p(l,4-k)=(1.+2.*b)*p(l,5-k)+p(1,5-k)-                                     !/*xg         44*/
     & ((l+1)*p(l+1,5-k)+l*p(l-1,5-k))/(2*l+1)                                  !/*xg         45*/
      do 70 l=1,240                                                             !/*xg         46*/
      t=2.*exs(jxs(ljxs+4,j)+14*(n-1)+6)                                        !/*xg         47*/
      do 60 m=1,4                                                               !/*xg         48*/
   60 t=t+exs(jxs(ljxs+4,j)+14*(n-1)+1+m)*p(l,m)                                !/*xg         49*/
      if(l.lt.7)t=t-2.*exs(jxs(ljxs+4,j)+14*(n-1)+6+l)/(2*l+1)                  !/*xg         50*/
   70 if(t.gt.0.)exs(jxs(ljxs+7,j)+l-1+240*(n-1))=                              !/*xg         51*/
     & log(2.*pie*t*exs(jxs(ljxs+4,j)+14*(n-1)+1))                              !/*xg         52*/
!                                                                               !/*xg         53*/
!        set up some functions of z.                                            !/*xg         54*/
      exs(jxs(ljxs+8,j))=z**third                                               !/*xg         55*/
      exs(jxs(ljxs+8,j)+1)=log(z)                                               !/*xg         56*/
      zf=(z/fscon)**2                                                           !/*xg         57*/
   80 exs(jxs(ljxs+8,j)+2)=zf*(1./(1.+zf)+.20206-                               !/*xg         58*/
     & zf*(.0369-zf*(.0083-zf*.002)))                                           !/*xg         59*/
!                                                                               !/*xg         60*/
!        set up cosines of multiple-scattering group boundaries.                !/*xg         61*/
      do 90 m=1,maxi                                                            !/*xg         62*/
   90 calph(m)=cos(al(m)*pie/180.)                                              !/*xg         63*/
!                                                                               !/*xg         64*/
!        start the knock-on cross sections.                                     !/*xgx         6*/
      do 110 i=1,nee(3)                                                         !/*xgx         7*/
      if(eee(leee+3,i).le.2.*ecf(3))go to 110                                   !/*xgx         8*/
      as=ecf(3)/eee(leee+3,i)                                                   !/*xgx         9*/
      s=eee(leee+3,i)/gpt(3)                                                    !/*xgx        10*/
      ag=(s/(s+1.))**2                                                          !/*xg         72*/
      ru=1./as-1./(1.-as)+(1.-ag)*log(as/(1.-as))                               !/*xg         73*/
      pru(lpru+i)=ru/(ru+ag*(.5-as))                                            !/*xg         74*/
      cc=.5*(ru+ag*(.5-as))*(s+1.)**2/(s**2*(s+2.))                             !/*xg         75*/
      do 100 mkc=1,nmat                                                         !/*xg         76*/
  100 pkn(lpkn+i+nee(3)*(mkc-1))=cc                                             !/*xgx        11*/
  110 continue                                                                  !/*xg         78*/
!                                                                               !/*xg         79*/
!        do all the electron materials in the problem.                          !/*xg         80*/
      do 180 mkc=1,nmat                                                         !/*xg         81*/
      do 120 i=1,mxa                                                            !/*xg         82*/
  120 if(mat(lmat+i).eq.mkc.and.(fim(lfim+3,i).ne.0..or.kpt(3).eq.0.and.        !/*xg         83*/
     & fim(lfim+2,i).ne.0))go to 130                                            !/*xg         84*/
      go to 180                                                                 !/*xg         85*/
!                                                                               !/*xg         86*/
!        calculate the average z, average atomic weight, and                    !/*xg         87*/
!        mass averages of z**(4/3) and z**2.                                    !/*xg         88*/
!        istern is a flag for the sbs density effect treatment and if           !/*xg4c       73*/
!        non-zero is the memory offset.                                         !/*xg4c       74*/
  130 az=0.                                                                     !/*xg         89*/
      aa=0.                                                                     !/*xg         90*/
      q=0.                                                                      !/*xg         91*/
      zq=0.                                                                     !/*xg         92*/
      if(nxs(lnxs+16,lme(llme+3,jmd(ljmd+mkc))).eq.3)istern=1                   !/*xg4c       75*/
      do 140 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*xg         93*/
      iz=iza(liza+j)/1000                                                       !/*xg         94*/
      az=az+fme(lfme+j)*iz                                                      !/*xg         95*/
      aa=aa+awc(lawc+j)*fme(lfme+j)                                             !/*xg         96*/
      q=q+awc(lawc+j)*fme(lfme+j)*iz**(4.*third)                                !/*xg         97*/
  140 zq=zq+awc(lawc+j)*fme(lfme+j)*iz**2                                       !/*xg         98*/
      nd=ns(int(sqrt(zq/aa)+.99))                                               !/*xg4a        1*/
!        TEMPORARY expedient: allow estep to control all substeps.              !/*xgx        12*/
      if(nsb(lnsb+mkc).le.0)nsb(lnsb+mkc)=nd                                    !/*xgx        13*/
      if(nsb(lnsb+mkc).lt.nd)call erprnt(1,2,1,mkc,0,0,0,0,                     !/*xgx        14*/
     & '45hfewer substeps than the default for material ,i5')                   !/*xgx        15*/
!                                                                               !/*xg        100*/
!        finish the knock-on cross sections.                                    !/*xg        101*/
      do 150 n=1,nee(3)                                                         !/*xgx        16*/
  150 pkn(lpkn+n+nee(3)*(mkc-1))=az*pkn(lpkn+n+nee(3)*(mkc-1))                  !/*xgx        17*/
!                                                                               !/*xg        104*/
!        calculate range, straggling, and scattering tables.                    !/*xg        105*/
#ifdef PCDOS                                                                    /*!xg4a        3*/
      if(lockl)call pttyin                                                      !/*xg4a        4*/
#endif                                                                          /*!cpp       190*/
      if(irup.ne.0)call qttyin(mkc,'23hget electron range, mat,i5')             !/*xg4a        5*/
      call ronge(az,aa,.00001*q/aa,nd)                                          !/*xg4a        6*/
#ifdef PCDOS                                                                    /*!xg4a        7*/
      if(lockl)call pttyin                                                      !/*xg4a        8*/
#endif                                                                          /*!cpp       191*/
      if(irup.ne.0)call qttyin(mkc,'28hget electron scat. xsec, mat,i5')        !/*xg4a        9*/
      call scatt(az)                                                            !/*xg        107*/
!                                                                               !/*xg        108*/
!        set up the k edge, the x-ray energy, and the auger probability         !/*xg        109*/
!        of the component with the highest z.                                   !/*xg        110*/
      jm=jmd(ljmd+mkc)                                                          !/*xg        111*/
      do 160 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*xg        112*/
  160 if(iza(liza+j).gt.iza(liza+jm))jm=j                                       !/*xg        113*/
      iz=iza(liza+jm)/1000                                                      !/*xg        114*/
      edg(ledg+mkc)=exs(jxs(ljxs+1,lme(llme+3,jm)))*.001                        !/*xg        115*/
      eek(leek+mkc)=exs(jxs(ljxs+1,lme(llme+3,jm))+1)*.001                      !/*xg        116*/
      wwk(lwwk+mkc)=1./(1.+1./(-.064+.034*iz-1.03e-6*iz**3)**4)                 !/*xg        117*/
!                                                                               !/*xg        118*/
!        calculate cross sections and bias factors for making x-rays.           !/*xg        119*/
!        allow for negative values of xnum in the future.                       !/*xg        120*/
      a=edg(ledg+mkc)/gpt(3)                                                    !/*xg        121*/
      fm=0.                                                                     !/*xg4c       76*/
      do 165 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*xg4c       77*/
  165 if(iz.eq.iza(liza+j)/1000)fm=fm+fme(lfme+j)                               !/*xg4c       78*/
      cn=min(iz,2)*fm/(a*3.)                                                    !/*xg4c       79*/
      d=0.                                                                      !/*xg        123*/
      do 170 n=1,nee(3)                                                         !/*xgx        18*/
      s=eee(leee+3,n)/gpt(3)                                                    !/*xgx        19*/
      r=eee(leee+3,n)/edg(ledg+mkc)                                             !/*xgx        20*/
      if(r.gt.1.)pxr(lpxr+n+nee(3)*(mkc-1))=                                    !/*xgx        21*/
     & cn*(.42*((s+1.)**2*log(1.19*s*(s+2.)/a)-s*(s+2.))+                       !/*xg        128*/
     & 1.5*((s+1.)**2-(.5*s**2+2.*s+1.+(2.*s+1.)*log(r))/r))/(s*(s+2.))         !/*xg        129*/
  170 if(n.gt.1.and.eee(leee+3,n).gt.ecf(3))d=d+                                !/*xgx        22*/
     & .5*(pxr(lpxr+n-1+nee(3)*(mkc-1))+pxr(lpxr+n+nee(3)*(mkc-1)))*            !/*xgx        23*/
     & (rng(lrng+3,n-1+nee_max*(mkc-1))-rng(lrng+3,n+nee_max*(mkc-1)))          !/*xgx        24*/
      xnm(lxnm+mkc)=1.                                                          !/*xg        133*/
      if(xnum.lt.0..and.d.ne.0.)xnm(lxnm+mkc)=max(one,-xnum/d)                  !/*xg        134*/
      if(xnum.ge.0.)xnm(lxnm+mkc)=xnum                                          !/*xg4c       80*/
!                                                                               !/*xg        136*/
!        calculate bremsstrahlung cross sections.                               !/*xg        137*/
#ifdef PCDOS                                                                    /*!xg4a       10*/
      if(lockl)call pttyin                                                      !/*xg4a       11*/
#endif                                                                          /*!cpp       192*/
      if(irup.ne.0)call qttyin(mkc,'28hget bremsstrahlung xsec, mat,i5')        !/*xg4a       12*/
      call brem                                                                 !/*xg        138*/
#ifdef PCDOS                                                                    /*!xg4a       13*/
      if(lockl)call pttyin                                                      !/*xg4a       14*/
#endif                                                                          /*!cpp       193*/
      if(irup.ne.0)call qttyin(mkc,'28hget bremsstrahlung scat, mat,i5')        !/*xg4a       15*/
      call brang                                                                !/*xg        139*/
!                                                                               !/*xg4c       81*/
!        print table 86.                                                        !/*xg4c       82*/
      if(ink(86).eq.0)go to 180                                                 !/*xg4c       83*/
      af=avgdn/aa                                                               !/*xg4c       84*/
      write(iuo,174)nmt(lnmt+mkc)                                               !/*xg4c       85*/
  174 format(/43h1electron secondary production for material,i5,56x,            !/*xg4c       86*/
     & 14hprint table 86//9x,6henergy,11x,14hstopping power,11x,                !/*xg4c       87*/
     & 5hbrems,3x,9hthick tgt,3x,7hk x-ray,3x,8hknock-on/4x,1hn,14x,            !/*xg4c       88*/
     & 19hcollision radiation,3x,5htotal,15x,5hbrems,16x,/10x,3hmev,            !/*xg4c       89*/
     & 6x,18hmev barn  mev barn,2x,8hmev barn,4x,4hbarn,17x,4hbarn,             !/*xg4c       90*/
     & 6x,4hbarn)                                                               !/*xg4c       91*/
      do 176 n=nee(3)-1,1,-1                                                    !/*xgx        25*/
      nk=n+nee(3)*(mkc-1)                                                       !/*xgx        26*/
  176 write(iuo,178)n,eee(leee+3,n),xse85(lx85+1,nk)/af,                        !/*xgx        27*/
     & xse85(lx85+2,nk)/af,xse85(lx85+3,nk)/af,pbr(lpbr+nk),                    !/*xg4c       95*/
     & pbt(lpbt+nk),pxr(lpxr+nk),pkn(lpkn+nk)                                   !/*xg4c       96*/
  178 format(i5,1pe12.4,2(1x,5e10.3))                                           !/*xg4c       97*/
  180 continue                                                                  !/*xg        140*/
      if(its30.eq.3)rka(1)=1.                                                   !/*xg4c       98*/
!                                                                               !/*xg        141*/
!        print bnum and enum biasing parameters.                                !/*xg        142*/
      if(numb.eq.1.and.enum.eq.1.)enum=.01                                      !/*xg4c       99*/
      if(bnum.ne.1.)write(iuo,190)bnum                                          !/*xg        143*/
  190 format(/48h the bremsstrahlung production cross section for,              !/*xg        144*/
     & 38h all materials is scaled by the factor,1pe12.4)                       !/*xg        145*/
      if(numb.eq.1)write(iuo,194)                                               !/*xg4c      100*/
  194 format(51h bremsstrahlung generated on each electron substep.)            !/*xg4c      101*/
      if(numb.eq.1.and.enum.eq..01)write(iuo,196)                               !/*xg4c      102*/
  196 format(46h secondary electron production reduced by 99%.)                 !/*xg4c      103*/
      if(xnum.ne.1.)write(iuo,200)xnum                                          !/*xg        146*/
  200 format(/39h the x ray production cross section for,                       !/*xg        147*/
     & 38h all materials is scaled by the factor,1pe12.4)                       !/*xg        148*/
      if(rnok.ne.1.)write(iuo,210)rnok                                          !/*xg        149*/
  210 format(/51h the knock-on electron production cross section for,           !/*xg        150*/
     & 38h all materials is scaled by the factor,1pe12.4)                       !/*xg        151*/
      if(enum.ne.1.)write(iuo,220)enum                                          !/*xg        152*/
  220 format(/52h the secondary electron production cross section for,          !/*xg        153*/
     & 38h all materials is scaled by the factor,1pe12.4)                       !/*xg        154*/
      return                                                                    !/*xg        155*/
      end                                                                       !/*xg        156*/

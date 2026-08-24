!#ident ronge.F from deck rn                                                    !/*rn          1*/
      subroutine ronge(az,aa,qa,nd)                                             !/*rn4a        1*/
!        calculate electron range tables and straggling tables.                 !/*rn          3*/
!        az = average atomic number   aa = average atomic weight                !/*rn          4*/
!        qa = .00001 * mass average of z**(4/3)                                 !/*rn          5*/
!        nd = default number of electron substeps per energy step               !/*rn4a        2*/
      use global_data                                                           !/*rnx         1*/
      use lambda_data                                                           !/*rnx         2*/
!                                                                               !/*rnx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*rnx         4*/
      implicit integer(kindi) (i-n)                                             !/*rnx         5*/
!                                                                               !/*rnx         6*/
      real(kindr), allocatable :: t_array(:)                                    !/*rnx         7*/
      parameter (km=5)                                                          !/*rn4c        1*/
      dimension bl(km),el(km),g(km),gb(km)                                      !/*rn4c        2*/
      character hr*12                                                           !/*rn4a        4*/
!                                                                               !/*rn4a        5*/
!        the formula for el(k) in the do 70 loop is based on equations          !/*rn4a        6*/
!        (a9) and (a10) of m. j. berger, methods in computational               !/*rn4a        7*/
!        physics, vol. 1, (academic press, new york, 1963) 135.                 !/*rn4a        8*/
!        c1 = 1.0e+24 * h**2 * c**2 * alpha**2 / (2 * pi * m * c**2),           !/*rn4a        9*/
!        where alpha = 1/fscon is the fine structure constant.                  !/*rn4a       10*/
!        c2 = ln(2*i**2), where i is the mean ionization potential              !/*rn4a       11*/
!        in units of the electron mass.                                         !/*rn4a       12*/
      c0=1.d12*planck*slite/fscon                                               !/*rn4c        3*/
      c1=c0*c0/(2*pie*gpt(3))                                                   !/*rn4a       13*/
      cl=log(two)                                                               !/*rnx         8*/
      c3=1.-cl                                                                  !/*rn4b        6*/
      c4=.125+cl                                                                !/*rn4b        7*/
      c5=euler-3.*cl                                                            !/*rn4b        8*/
      dk=km-1                                                                   !/*rn4b       11*/
!                                                                               !/*rn          9*/
!        calculate the mean-ionization-potential term.                          !/*rn         10*/
!        obtain an average density for a cell with the same material but        !/*rn4c        4*/
!        different densities.                                                   !/*rn4c        5*/
      rh=0.                                                                     !/*rn4c        6*/
      nh=0                                                                      !/*rn4c        7*/
      ic=0                                                                      !/*rn4c        8*/
      do 10 i=1,mxa                                                             !/*rn4c        9*/
      if(mat(lmat+i).ne.mkc)go to 10                                            !/*rn4c       10*/
      if(ic.eq.0)ic=i                                                           !/*rn4c       11*/
      rh=rh+rho(lrho+i)                                                         !/*rn4c       12*/
      nh=nh+1                                                                   !/*rn4c       13*/
   10 continue                                                                  !/*rn4c       14*/
      if(nh.eq.0.or.rh.eq.0.)                                                   !/*rn4c       15*/
     & call expirx(0,'ronge','zero density or non-existent material')           !/*rn4c       16*/
      rh=rh/nh                                                                  !/*rn4c       17*/
   20 sr=0.                                                                     !/*rn         13*/
      f=1.                                                                      !/*rn4b       12*/
      jj=1                                                                      !/*rn4b       13*/
      if(jemi(ljem+mkc).eq.0)jj=2                                               !/*rn4c       18*/
      if(jmd(ljmd+mkc+1)-jmd(ljmd+mkc).eq.1)go to 34                            !/*rn4c       19*/
      iz=iza(liza+jmd(ljmd+mkc))/1000                                           !/*rn4c       21*/
      do 30 j=jmd(ljmd+mkc)+1,jmd(ljmd+mkc+1)-1                                 !/*rn4c       22*/
   30 if(iz.ne.iza(liza+j)/1000)go to 32                                        !/*rn4c       23*/
      go to 34                                                                  !/*rn4c       24*/
   32 f=1.13                                                                    !/*rn4c       25*/
      jj=jj+2                                                                   !/*rn4c       26*/
   34 do 36 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*rn4c       27*/
      iz=iza(liza+j)/1000                                                       !/*rn         15*/
         p=f_ionp(iz,2,jj)                                                      !/*rnx         9*/
   36 sr=sr+log(p)*iz*fme(lfme+j)                                               !/*rn4c       28*/
      sr=exp(sr/az)                                                             !/*rn         19*/
      si=sr*1e-6                                                                !/*rn4b       25*/
      c2=log(2.*(si/gpt(3))**2)                                                 !/*rn4b       26*/
!                                                                               !/*rn         21*/
!        set up the electron scattering density correction.                     !/*rn         22*/
!        distinguish between condensed and gas state for density effect.        !/*rn4b       27*/
!        TEMPORARY LIMITATION.  the density of the first cell with              !/*rnx        10*/
!                               the current material is used.                   !/*rnx        11*/
!        az is zoa in the notation of sternhiemer, berger, seltzer.             !/*rn4c       29*/
!        cr is the classical electron radius.                                   !/*rn4c       30*/
      cr=planck*slite/(2.d0*pie*fscon*gpt(3))                                   !/*rn4c       31*/
      pl=2.d18*cr*gpt(3)*fscon*sqrt(pie*cr*rh*az)                               !/*rn4c       32*/
      su=2.d0*log(sr/pl)                                                        !/*rn4c       33*/
      if(istern.ne.0)go to 40                                                   !/*rn4c       34*/
!                                                                               !/*rn4c       35*/
!        sternheimer and peierls treatment.                                     !/*rn4c       36*/
!        cb=su+1.                                                               !/*rn4c       37*/
      cb=2.*log(sr/(37.1*sqrt(az*rho(lrho+ic))))+1.                             !/*rn4c       38*/
      if(jemi(ljem+mkc).eq.0)go to 38                                           !/*rn4c       39*/
      xb=4+min(1,int(cb/12.25))                                                 !/*rn4c       40*/
      xa=max(zero+.1*min(20,max(16,int(2.*cb)-3)),.326*cb-2.5)                  !/*rn4c       41*/
      go to 50                                                                  !/*rn4c       42*/
   38 xb=2+min(1,int(.01*sr))                                                   !/*rn4c       43*/
      xa=max(zero+.2,.326*cb-.5*xb)                                             !/*rn4c       44*/
      go to 50                                                                  !/*rn4c       45*/
!                                                                               !/*rn4c       46*/
!        sternheimer, berger, and seltzer treatment (sbs):                      !/*rn4c       47*/
!        r. m. sternheimer, s. m. seltzer, and m. j. berger, "densitys          !/*rn4c       48*/
!        effect for the ionization loss of charged particles in various         !/*rn4c       49*/
!        substances", phys rev b, 26, 6067(1982).                               !/*rn4c       50*/
   40 j1=0                                                                      !/*rn4c       51*/
      j2=0                                                                      !/*rn4c       52*/
      m8=0                                                                      !/*rn4c       53*/
      m9=0                                                                      !/*rn4c       54*/
      do 42 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*rn4c       55*/
      m8=max(m8,nxs(lnxs+3,lme(llme+3,j)))                                      !/*rn4c       56*/
      ns=nxs(lnxs+11,lme(llme+3,j))                                             !/*rn4c       57*/
      zs=exs(jxs(ljxs+11,lme(llme+3,j))+ns-1)                                   !/*rn4c       58*/
      if(jcond(ljco+mkc).eq.0.and.zs.gt.0.)j1=j1+1                              !/*rn4c       59*/
      if(jcond(ljco+mkc).gt.0.and.zs.lt.0.)j2=j2+1                              !/*rn4c       60*/
   42 m9=m9+ns                                                                  !/*rn4c       61*/
!                                                                               !/*rn4c       62*/
!        check conductor or non-conductor assignment.  non-conductor            !/*rn4c       63*/
!        if assigned by cond<0, or =0 and at least one non-conducting           !/*rn4c       64*/
!        component.  conductor if assigned cond>0 and at least one              !/*rn4c       65*/
!        conducting component, or cond=0 and no non-conducting                  !/*rn4c       66*/
!        components.                                                            !/*rn4c       67*/
      if(jcond(ljco+mkc).ne.0)go to 44                                          !/*rn4c       68*/
      if(j1.ne.0)go to 46                                                       !/*rn4c       69*/
      jcond(ljco+mkc)=1                                                         !/*rn4c       70*/
      call erprnt(1,2,1,nmt(lnmt+mkc),0,0,0,0,                                  !/*rn4c       71*/
     & '8hmaterial,i5,29h has been set to a conductor.')                        !/*rn4c       72*/
      go to 46                                                                  !/*rn4c       73*/
   44 if(j2.ne.0)go to 46                                                       !/*rn4c       74*/
      if(jcond(ljco+mkc).lt.0)jcond(ljco+mkc)=0                                 !/*rn4c       75*/
      if(jcond(ljco+mkc).eq.0)go to 46                                          !/*rn4c       76*/
      call erprnt(1,2,1,nmt(lnmt+mkc),0,0,0,0,                                  !/*rn4c       77*/
     & '8hmaterial,i5,33h has been set to a non-conductor.')                    !/*rn4c       78*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*rn4c       79*/
     & '34hno conduction electrons available.')                                 !/*rn4c       80*/
!                                                                               !/*rn4c       81*/
!        add memory for temporary arrays.                                       !/*rn4c       82*/
   46 if(istern.eq.1)istern=2*m8+3*m9                                           !/*rnx        12*/
      allocate (t_array(istern), stat=is)                                       !/*rnx        13*/
      if(is.ne.0)call expire(0,'ronge','error getting memory')                  !/*rnx        14*/
      t_array=0.                                                                !/*rnx        15*/
      l4=0                                                                      !/*rnx        16*/
      l5=l4+3*m9                                                                !/*rn4c       87*/
      call den_el1(az,su,pl,t_array(l4+1),t_array(l4+m9+1),                     !/*rnx        17*/
     & t_array(l4+2*m9+1),nt,wt,tm)                                             !/*rnx        18*/
!                                                                               !/*rn         31*/
!        calculate ranges from radiation and ionization loss rates.             !/*rn         32*/
   50 af=avgdn/aa                                                               !/*rn         33*/
      ye=0.                                                                     !/*rn         34*/
      nc=8.*log(emx(3)/ecut_min(3))/cl+1.                                       !/*rnx        19*/
      im=nxs(lnxs+16,lme(llme+3,jmd(ljmd+mkc)))                                 !/*rn4c       89*/
      do 110 nz=1,nc                                                            !/*rn         36*/
      n=nc+1-nz                                                                 !/*rn         37*/
      if(n.le.nee(3))e=eee(leee+3,n)                                            !/*rnx        20*/
      if(n.gt.nee(3))e=eee(leee+3,nee(3))*efac(3)**(n-nee(3))                   !/*rnx        21*/
      de=e*(1.-efac(3))                                                         !/*rnx        22*/
      do 70 k=1,km                                                              !/*rn4b       29*/
      s=(e-(k-1)*de/dk)/gpt(3)                                                  !/*rn4b       30*/
!                                                                               !/*rn         43*/
!        get the radiation loss by interpolation in cross-section table.        !/*rn         44*/
      bl(k)=0.                                                                  !/*rn         45*/
      do 60 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*rn         46*/
      ix=lme(llme+3,j)                                                          !/*rn         47*/
      in=1                                                                      !/*rn4c       90*/
      if(im.ne.3)bl(k)=bl(k)+qpol(log(s),exs(jxs(ljxs+2,ix)),                   !/*rn4c       91*/
     & exs(jxs(ljxs+2,ix)+nxs(lnxs+3,ix)),nxs(lnxs+3,ix))*                      !/*rn4c       92*/
     & fme(lfme+j)*gpt(3)*(s+1.)                                                !/*rn4c       93*/
      if(im.eq.3)bl(k)=bl(k)+spol(log(s),exs(jxs(ljxs+2,ix)),                   !/*rn4c       94*/
     & exs(jxs(ljxs+2,ix)+nxs(lnxs+3,ix)),t_array(l5+1),                        !/*rnx        23*/
     & t_array(l5+m8+1),nxs(lnxs+3,ix),in,id,m1,m2)*fme(lfme+j)*gpt(3)          !/*rnx        24*/
     & *(s+1.)                                                                  !/*rnx        25*/
   60 continue                                                                  !/*rn4c       97*/
!                                                                               !/*rn         50*/
!        get the ionization loss from electron scattering theory.               !/*rn         51*/
      bs=s*(s+2.)/(s+1.)**2                                                     !/*rn         52*/
      d=0.                                                                      !/*rn4c       98*/
      if(istern.eq.0)go to 66                                                   !/*rn4c       99*/
      call den_el2(s,tm,t_array(l4+1),t_array(l4+1+m9),                         !/*rnx        26*/
     & t_array(l4+1+2*m9),nt,wt,d)                                              !/*rnx        27*/
      go to 68                                                                  !/*rn4c      101*/
   66 x=.4342945*log(sqrt(s*(s+2.)))                                            !/*rn4c      102*/
      if(x.gt.xa)d=max(zero,4.606*x-cb+(cb-4.606*xa)*                           !/*rn4c      103*/
     & ((xb-min(x,xb))/(xb-xa))**3)                                             !/*rn4c      104*/
   68 if(k.eq.1)dd=d                                                            !/*rn4c      105*/
      el(k)=c1*az*(log(s**2*(s+2.))-c2+c3-bs+c4*(s/(s+1.))**2-d)/bs             !/*rn4a       17*/
      g(k)=1./(el(k)+bl(k))                                                     !/*rn4b       31*/
   70 gb(k)=g(k)*bl(k)                                                          !/*rn4b       32*/
      if(n.eq.nc)rg=.5*(e-de)*g(km)                                             !/*rn4b       33*/
      if(n.eq.nee(3)-1)rng(lrng+3,nee_max*mkc)=rg                               !/*rnx        28*/
!                                                                               !/*rn         63*/
!        average the range increment over the energy interval.                  !/*rn         64*/
      dr=simint(de,g,km)                                                        !/*rn4b       34*/
      rg=rg+dr                                                                  !/*rn         67*/
      fe=simint(de,gb,km)                                                       !/*rn4b       35*/
      ye=ye+fe                                                                  !/*rn         71*/
      if(n.ge.nee(3))go to 85                                                   !/*rnx        29*/
      rng(lrng+3,n+nee_max*(mkc-1))=rg                                          !/*rnx        30*/
      drs(ldrs+3,n+nee_max*(mkc-1))=dr/nsb(lnsb+mkc)                            !/*rnx        31*/
!                                                                               !/*rn         75*/
!        calculate energy loss and straggling coefficients.                     !/*rn         76*/
      ef=(el(1)+el(km))/(bl(1)+bl(km)+el(1)+el(km))                             !/*rn4b       37*/
      qav(lqav+3,n+nee_max*(mkc-1))=de*ef/dr                                    !/*rnx        32*/
      if(istrg(3).eq.1)go to 80                                                 !/*rnxa        1*/
      s=(e-.5*de)/gpt(3)                                                        !/*rn         80*/
      t2=(s+1.)**2                                                              !/*rn         81*/
      asp(lasp+n+nee(3)*(mkc-1))=c1*az*t2/(s*(s+2.))                            !/*rnx        34*/
      ear(lear+n+nee(3)*(mkc-1))=log((e-.5*de)/                                 !/*rnx        35*/
     & (dr*asp(lasp+n+nee(3)*(mkc-1))))+c5+c4*s**2/t2                           !/*rnx        36*/
      qcn(lqcn+n+nee(3)*(mkc-1))=sqrt(qa*de*ef)/dr                              !/*rnx        37*/
!                                                                               !/*rn4b       45*/
!        seltzer empirical modification to blunck-westphal prescription         !/*rn4b       46*/
!        for the variance of the gaussian in blunck-leisegang extension         !/*rn4b       47*/
!        of the landau straggling theory.                                       !/*rn4b       48*/
      r=dr*asp(lasp+n+nee(3)*(mkc-1))/si                                        !/*rnx        38*/
      qcn(lqcn+n+nee(3)*(mkc-1))=qcn(lqcn+n+nee(3)*(mkc-1))/(1.+3./             !/*rnx        39*/
     & sqrt(10.*r*(1.+.1*r)**3))                                                !/*rnx        40*/
!                                                                               !/*rn4b       52*/
!        set cutoff values for sampling the landau function.                    !/*rn4b       53*/
      if(ear(lear+n+nee(3)*(mkc-1)).ge.avlm(mlanc))go to 75                     !/*rnx        41*/
      flc(lflc+n+nee(3)*(mkc-1))=qpol(ear(lear+n+nee(3)*(mkc-1)),               !/*rnx        42*/
     & avlm,flam,mlanc)                                                         !/*rn4b       56*/
      go to 80                                                                  !/*rn4b       57*/
   75 flc(lflc+n+nee(3)*(mkc-1))=flam(mlanc)                                    !/*rnx        43*/
!                                                                               !/*rn4b       60*/
!        load print table 85 for xs plotting.                                   !/*rn4b       61*/
   80 nk=n+nee(3)*(mkc-1)                                                       !/*rnx        44*/
      xse85(lx85+1,nk)=el(1)*af                                                 !/*rn4b       64*/
      xse85(lx85+2,nk)=bl(1)*af                                                 !/*rn4b       65*/
      xse85(lx85+3,nk)=(el(1)+bl(1))*af                                         !/*rn4b       66*/
      if(jovr(1)+jovr(3)+jovr(5).ne.3)go to 85                                  !/*rn4c      107*/
      xse85(lx85+4,nk)=rg/af                                                    !/*rn4b       67*/
      xse85(lx85+5,nk)=ye/e                                                     !/*rn4b       68*/
      bs=e*(e+2.*gpt(3))/(e+gpt(3))**2                                          !/*rn4c      108*/
      xse85(lx85+6,nk)=bs                                                       !/*rn4b       69*/
      xse85(lx85+7,nk)=c1*af*az*dd/bs                                           !/*rn4c      109*/
      xse85(lx85+8,nk)=bl(1)/el(1)                                              !/*rn4b       71*/
      xse85(lx85+9,nk)=dr/af                                                    !/*rn4b       72*/
      xse85(lx85+10,nk)=fe                                                      !/*rn4b       73*/
!                                                                               !/*rn         86*/
!        print range table if required.                                         !/*rn         87*/
   85 if(ink(85).eq.0)go to 110                                                 !/*rn4b       74*/
      if(jemi(ljem+mkc).eq.0)hr=' (condensed)'                                  !/*rn4c      110*/
      if(jemi(ljem+mkc).ne.0)hr=' (gas)      '                                  !/*rn4c      111*/
      if(nz.eq.1)write(iuo,90)nmt(lnmt+mkc),hr,nsb(lnsb+mkc),nd,sr              !/*rn4b       75*/
   90 format(25h1range table for material,i5,a12,62x,14hprint table 85//        !/*rn4a       21*/
     & 36h electron substeps per energy step =,i4,1h,,2x,9hdefault =,i4,        !/*rn4b       76*/
     & 1h.,2x,24hmean ionization energy =,1pe12.5,4h ev./)                      !/*rn4c      112*/
      if(istern.eq.0.or.nz.ne.1)go to 94                                        !/*rn4c      113*/
      write(iuo,'(20h density effect data)')                                    !/*rn4c      114*/
      if(jcond(ljco+mkc).eq.0)write(iuo,'(14h non-conductor)')                  !/*rn4c      115*/
      if(jcond(ljco+mkc).gt.0)write(iuo,'(10h conductor)')                      !/*rn4c      116*/
      do 92 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*rn4c      117*/
      iz=iza(liza+j)/1000                                                       !/*rn4c      118*/
      ns=nxs(lnxs+11,lme(llme+3,j))                                             !/*rn4c      119*/
      write(iuo,'(6h   z =,i4)')iz                                              !/*rn4c      120*/
      write(iuo,'(4x,20hocc no, be(ev) pairs)')                                 !/*rn4c      121*/
   92 write(iuo,'(6(6x,f3.0,f11.3))')(exs(jxs(ljxs+11,lme(llme+3,j))+           !/*rn4c      122*/
     & nn-1),exs(jxs(ljxs+11,lme(llme+3,j))+ns+nn-1),nn=1,ns)                   !/*rn4c      123*/
      write(iuo,'(7x,8hplas(ev),7x,2hwt,7x,9htmin(mev))')                       !/*rn4c      124*/
      write(iuo,'(3x,3f12.5/)')pl,wt,tm*gpt(3)                                  !/*rn4c      125*/
   94 if(nz.eq.1)write(iuo,96)                                                  !/*rn4c      126*/
   96 format(9x,6henergy,11x,14hstopping power,11x,5hrange,3x,                  !/*rn4c      127*/
     & 9hradiation,3x,7hbeta**2,3x,7hdensity,3x,7hrad/col,3x,6hdrange,          !/*rn4c      128*/
     & 4x,6hdyield/4x,1hn,14x,19hcollision radiation,3x,5htotal,15x,            !/*rn4c      129*/
     & 5hyield,16x,4hcorr/10x,3hmev,6x,29hmev cm2/g mev cm2/g mev cm2/g,        !/*rn4c      130*/
     & 3x,5hg/cm2,24x,9hmev cm2/g,13x,5hg/cm2/)                                 !/*rn4c      131*/
      bs=e*(e+2.*gpt(3))/(e+gpt(3))**2                                          !/*rn         96*/
      write(iuo,100)n,e,el(1)*af,bl(1)*af,(el(1)+bl(1))*af,rg/af,ye/e,          !/*rn4b       80*/
     & bs,c1*af*az*dd/bs,bl(1)/el(1),dr/af,fe                                   !/*rn4b       81*/
  100 format(i5,1pe12.4,2(1x,5e10.3))                                           !/*rn         99*/
  110 continue                                                                  !/*rn        100*/
      if (allocated( t_array)) deallocate (t_array)                             !/*rnx        45*/
      return                                                                    !/*rn        101*/
      end                                                                       !/*rn        102*/

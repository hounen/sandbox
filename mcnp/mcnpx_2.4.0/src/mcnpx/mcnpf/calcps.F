!#ident calcps.F from deck ct                                                   !/*ct          1*/
      subroutine calcps                                                         !/*ct          2*/
!        calculate the probability density, psc, of the particle                !/*ct          3*/
!        scattering toward the detector.  also the energy in some cases.        !/*ct          4*/
      use global_data                                                           !/*ctx         1*/
!                                                                               !/*ctx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ctx         3*/
      implicit integer(kindi) (i-n)                                             !/*ctx         4*/
!                                                                               !/*ctx         5*/
      k(i,j)=ksd(lksd+i,j)                                                      !/*ct          6*/
      q(i)=sqq(lsqq+i,ic)                                                       !/*ct          7*/
!                                                                               !/*ct          8*/
      go to(         10, 20, 30,120,150,200,260,400,                            !/*ct4c2       1*/
     &      540,580,590,592, 25, 26,594,596)ipsc-2                              !/*ct4c2       2*/
      call expirx(1,'calcps','illegal value for ipsc.')                         !/*ct4c2       3*/
      return                                                                    !/*ct4c2       4*/
!                                                                               !/*ct         10*/
! >>>>>  ipsc=3 -- particle from type 0 source.  if nsr=0 and detectors         !/*ct         11*/
!        are used in the problem, you need to provide subroutine srcdx.         !/*ct         12*/
   10 call srcdx                                                                !/*ct         13*/
      return                                                                    !/*ct         14*/
!                                                                               !/*ct         15*/
! >>>>>  ipsc=4 -- neutron from collision with moving target.                   !/*ct         16*/
   20 t3=uuu*tpd(4)+vvv*tpd(5)+www*tpd(6)                                       !/*ct         17*/
      if(t3.le.0..and.tpd(1)**2.lt.abs(tpd(3)))go to 600                        !/*ct         18*/
      t1=t3**2+tpd(1)**2-abs(tpd(3))                                            !/*ct         19*/
      if(t1.lt.0.)go to 600                                                     !/*ct         20*/
      t1=sqrt(t1)                                                               !/*ct         21*/
      t2=t3+sign(t1,tpd(3))                                                     !/*ct         22*/
      erg=tpd(2)*t2**2                                                          !/*ct         23*/
      psc=t2**2/(2.*tpd(1)*t1)                                                  !/*ct         24*/
      if(ixcos.eq.0)return                                                      !/*ct         25*/
      cs=(abs(tpd(3))+(1.+awn(lawn+iex))*((uold(1)*uuu+uold(2)*vvv+             !/*ct4b        1*/
     & uold(3)*www)*t2-(uold(1)*tpd(4)+uold(2)*tpd(5)+uold(3)*tpd(6)))-         !/*ct4b        2*/
     & t2*t3)/(tpd(1)*tpd(7))                                                   !/*ct4b        3*/
      go to 60                                                                  !/*ct         28*/
!                                                                               !/*ct4a        2*/
! >>>>>  ipsc=15 -- neutron from inelastic law 67 (endf/b-vi law 7).            !/*ct4a        3*/
   25 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b        4*/
      erg=acecs6(1,0,0,0,zero,cs)                                               !/*ct4c        2*/
      if(ixcos.eq.0)return                                                      !/*ct4a        6*/
      go to 60                                                                  !/*ct4a        7*/
!                                                                               !/*ct4c        3*/
! >>>>>  ipsc=16 -- neutron from law 61 (tabulated energies / angles).          !/*ct4c        4*/
!        if isotropic and lab system, can return (psc=0.5).                     !/*ct4c        5*/
   26 if(ixcos.eq.0.and.ntyn.ge.0)return                                        !/*ct4c        6*/
!                                                                               !/*ct4c        7*/
!        cs is laboratory cosine to next event estimator.                       !/*ct4c        8*/
      cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4c        9*/
!                                                                               !/*ct4c       10*/
!        if lab system, but anisotropic, go to table lookup for psc.            !/*ct4c       11*/
!        since we're in the lab system, erg for next-event direction            !/*ct4c       12*/
!        is no different than erg in actual as-sampled direction.               !/*ct4c       13*/
      if(ntyn.ge.0)go to 60                                                     !/*ct4c       14*/
      a1=1.+awn(lawn+iex)                                                       !/*ct4c       15*/
!                                                                               !/*ct4c       16*/
!        scattering distributions are in the cm system.                         !/*ct4c       17*/
!        ergace is previously-sampled cm energy. eg0 is incident energy.        !/*ct4c       22*/
!                                                                               !/*ct4c       24*/
!        to find valid lab cosines, start with eq. in case 3 of p. 69           !/*ct4c       25*/
!        from carter and cashwell.  substitute in for q based on                !/*ct4c       26*/
!        their eq. 5.8.  then wind up with following condition,                 !/*ct4c       27*/
!        which is identical to that used by hendricks later in                  !/*ct4c       28*/
!        this routine for ipsc=14.  if t3 > cs**2, cannot scatter               !/*ct4c       29*/
!        toward next-event position.                                            !/*ct4c       30*/
      t3=1.-ergace*a1**2/eg0                                                    !/*ct4c       31*/
      if(t3.gt.cs**2)go to 600                                                  !/*ct4c       32*/
!                                                                               !/*ct4c       33*/
!        scattering is valid.  calculate lab energy (erg) via following         !/*ct4c       34*/
!        equation as in ipsc=14 code (equivalent to eq. 5.14 of carter          !/*ct4c       35*/
!        and cashwell.                                                          !/*ct4c       36*/
      t1=1./a1                                                                  !/*ct4c2       5*/
      erg=eg0*(t1*(cs+sqrt(cs**2-t3)))**2                                       !/*ct4c       38*/
!                                                                               !/*ct4c       39*/
!        return if this energy is below particle's energy cutoff.               !/*ct4c       40*/
      if(erg.lt.elc(ipt))return                                                 !/*ct4c       41*/
!                                                                               !/*ct4c       42*/
!        now, we need to calculate the d-cm cosine by d-lab cosine              !/*ct4c       43*/
!        and apply this factor to the psc.  start with eq. 5.13 of              !/*ct4c       44*/
!        carter and cashwell, which reduces to the following for                !/*ct4c       45*/
!        psc (the extra 0.5 is a starting assumption of isotropy).              !/*ct4c       46*/
!        formalism is identical to ipsc=14 and ipsc=5.                          !/*ct4c       47*/
      t2=sqrt(erg/ergace)                                                       !/*ct4c       48*/
      t4=t1*sqrt(eg0/erg)                                                       !/*ct4c       49*/
      psc=.5*t2/(1.-cs*t4)                                                      !/*ct4c       50*/
!                                                                               !/*ct4c       51*/
!        if isotropic in cm, return.                                            !/*ct4c       52*/
      if(ixcos.eq.0)return                                                      !/*ct4c       53*/
!                                                                               !/*ct4c       54*/
!        otherwise, determine cm cosine (overwrite cs with it).  start          !/*ct4c       55*/
!        with eq. 5.10 of cashwell and carter.  formalism is same as            !/*ct4c       56*/
!        for ipsc=5 and ipsc=14.                                                !/*ct4c       57*/
      cs=t2*(cs-t4)                                                             !/*ct4c       58*/
!                                                                               !/*ct4c       59*/
!        now (finally) go to table lookup to actually determine psc             !/*ct4c       60*/
!        for scattering toward the next-event estimator.                        !/*ct4c       61*/
      go to 60                                                                  !/*ct4c       62*/
!                                                                               !/*ct         29*/
! >>>>>  ipsc=5 -- neutron from collision with stationary target.               !/*ct         30*/
   30 if(mcal.ne.0)go to 100                                                    !/*ct         31*/
      if(ixcos.eq.0.and.ntyn.ge.0)return                                        !/*ct         32*/
      cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b        5*/
      aw=awn(lawn+iex)                                                          !/*ct         34*/
      if(ntyn.eq.-99)go to 40                                                   !/*ct         35*/
!                                                                               !/*ct         36*/
!        inelastic scattering case.  see carter & cashwell eqn. 5.9,5.10        !/*ct4a        8*/
      if(ntyn.ge.0)go to 60                                                     !/*ct         38*/
      t3=1.-ergace*(1.+aw)**2/eg0                                               !/*ct         39*/
      if(t3.gt.0.)go to 600                                                     !/*ct         40*/
      t1=1./(1.+aw)                                                             !/*ct         41*/
      erg=eg0*(t1*(cs+sqrt(cs**2-t3)))**2                                       !/*ct         42*/
      if(erg.lt.elc(1))return                                                   !/*ct         43*/
      t2=sqrt(erg/ergace)                                                       !/*ct         44*/
      t4=t1*sqrt(eg0/erg)                                                       !/*ct         45*/
      psc=.5*t2/(1.-cs*t4)                                                      !/*ct         46*/
      if(ixcos.eq.0)return                                                      !/*ct         47*/
      cs=t2*(cs-t4)                                                             !/*ct         48*/
      go to 60                                                                  !/*ct         49*/
!                                                                               !/*ct         50*/
!        elastic scattering case                                                !/*ct         51*/
   40 if(aw.ge.1.5)go to 50                                                     !/*ct         52*/
!        elastic scattering off hydrogen.  no backscattering.                   !/*ct         53*/
      if(cs.lt.0.)go to 600                                                     !/*ct         54*/
      erg=eg0*cs**2                                                             !/*ct         55*/
      if(erg.lt.elc(1))return                                                   !/*ct         56*/
      psc=2.*cs                                                                 !/*ct         57*/
      if(ixcos.eq.0)return                                                      !/*ct         58*/
      cs=2.*cs**2-1.                                                            !/*ct         59*/
      go to 60                                                                  !/*ct         60*/
!        other elastic scattering.                                              !/*ct         61*/
   50 t3=sqrt(cs**2-1.+aw**2)                                                   !/*ct         62*/
      erg=eg0*(cs+t3)**2/(1.+aw)**2                                             !/*ct         63*/
      if(erg.lt.elc(1))return                                                   !/*ct         64*/
      psc=.5*(cs+t3)**2/(aw*t3)                                                 !/*ct         65*/
      if(ixcos.eq.0)return                                                      !/*ct         66*/
      cs=(cs**2-1.+cs*t3)/aw                                                    !/*ct         67*/
!                                                                               !/*ct         68*/
!        calculate psc from table of equi-probable cosine intervals.            !/*ct         69*/
   60 if(ixcos.lt.0)go to 92                                                    !/*ct4c       63*/
      ic=ixcos                                                                  !/*ct4c       64*/
      ib=ixcos+32                                                               !/*ct         71*/
   70 if(ib-ic.eq.1)go to 90                                                    !/*ct         72*/
      ih=(ic+ib)/2                                                              !/*ct         73*/
      if(cs.lt.xss(ih))go to 80                                                 !/*ct         74*/
      ic=ih                                                                     !/*ct         75*/
      go to 70                                                                  !/*ct         76*/
   80 ib=ih                                                                     !/*ct         77*/
      go to 70                                                                  !/*ct         78*/
   90 psc=.0625*psc/(xss(ib)-xss(ic))                                           !/*ct         79*/
      return                                                                    !/*ct         80*/
!                                                                               !/*ct4c       65*/
!        calculate psc from tabulated cosine distribution.                      !/*ct4c       66*/
   92 ic=-ixcos                                                                 !/*ct4c       67*/
      jj=nint(xss(ic))                                                          !/*ct4c       68*/
      np=nint(xss(ic+1))                                                        !/*ct4c       69*/
      if(xss(ic+2).gt.cs.or.xss(ic+1+np).lt.cs)go to 600                        !/*ct4c       70*/
      ic=ic+3                                                                   !/*ct4c       71*/
   94 if(xss(ic).gt.cs)go to 96                                                 !/*ct4c       72*/
      ic=ic+1                                                                   !/*ct4c       73*/
      go to 94                                                                  !/*ct4c       74*/
   96 if(jj.eq.1)psc=2.*psc*xss(ic+np-1)                                        !/*ct4c       75*/
      if(jj.ne.1)psc=2.*psc*(xss(ic+np-1)+(cs-xss(ic-1))*                       !/*ct4c       76*/
     & (xss(ic+np)-xss(ic+np-1))/(xss(ic)-xss(ic-1)))                           !/*ct4c       77*/
      return                                                                    !/*ct4c       78*/
!                                                                               !/*ct         81*/
!        multigroup cross sections case.                                        !/*ct         82*/
  100 if(nxs(lnxs+3,iex).le.0)return                                            !/*ct         83*/
      cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b        6*/
      l=nint(xss(jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+16,iex)))+ixcos-2))          !/*ct4c       79*/
      if(l.eq.0)return                                                          !/*ct         86*/
      j=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+17,iex)))+l-2                         !/*ct4c       80*/
      if(cs.ge.xss(j+nxs(lnxs+3,iex)-1).or.cs.lt.xss(j))go to 600               !/*ct         88*/
  110 j=j+1                                                                     !/*ct         89*/
      if(cs.gt.xss(j))go to 110                                                 !/*ct         90*/
      psc=1./((xss(j)-xss(j-1))*(nxs(lnxs+3,iex)-1))                            !/*ct         91*/
      return                                                                    !/*ct         92*/
!                                                                               !/*ct         93*/
! >>>>>  ipsc=6 -- photon from coherent scattering.                             !/*ct         94*/
  120 cs=min(one,uold(1)*uuu+uold(2)*vvv+uold(3)*www)                           !/*ct4b        7*/
      t3=29.1445*eg0*1.956917*sqrt(1.-cs)                                       !/*ct         96*/
      if(t3.ge.vco(mcoh))go to 600                                              !/*ct4a        9*/
      do 130 i=2,mcoh                                                           !/*ct4a       10*/
  130 if(t3.lt.vco(i))go to 140                                                 !/*ct         99*/
  140 ib=jxs(ljxs+3,iex)+mcoh+i-1                                               !/*ct4a       11*/
      psc=(.2494351/(tpd(2)-tpd(1)))*(1.+cs**2)*                                !/*ct4b        9*/
     & (xss(ib)+(xss(ib-1)-xss(ib))*(t3-vco(i))/(vco(i-1)-vco(i)))**2           !/*ct4b       10*/
      return                                                                    !/*ct        103*/
!                                                                               !/*ct        104*/
! >>>>>  ipsc=7 -- photon from incoherent scattering.                           !/*ct        105*/
  150 cs=min(one,uold(1)*uuu+uold(2)*vvv+uold(3)*www)                           !/*ct4b       13*/
      t2=1.+eg0*1.956917*(1.-cs)                                                !/*ct        107*/
      t5=1./t2                                                                  !/*ct        108*/
      erg=eg0*t5                                                                !/*ct        109*/
      if(erg.lt.elc(2))return                                                   !/*ct        110*/
!                                                                               !/*ct4b       14*/
!        simple physics.  use empirical formula of hastings for integral        !/*ct4b       15*/
!        of klein-nishina formula = 1/t7 (lux & koblinger eqn. 3.11)            !/*ct4b       16*/
      if(eg0.le.emcf(2))go to 155                                               !/*ct4c       81*/
      t4=1.+.434508*eg0                                                         !/*ct4b       18*/
      t7=(((t4+12.501332)*t4-14.200407)*t4+1.699075)/                           !/*ct4b       19*/
     & ((1.651035*t4+9.340220)*t4-8.325004)                                     !/*ct4b       20*/
      if(eg0.gt.100.)t7=.2494351*nxs(lnxs+2,iex)/tpd(1)                         !/*ct4b       21*/
      psc=t7*(t5**2)*(t2+t5-1.+cs**2)                                           !/*ct4b       22*/
      return                                                                    !/*ct4b       23*/
!                                                                               !/*ct4b       24*/
!        detailed physics. use form factors.  hydrogen is special case.         !/*ct4b       25*/
  155 t3=eg0*1.956917*sqrt(1.-cs)                                               !/*ct4b       26*/
      if(nxs(lnxs+2,iex).gt.1)go to 160                                         !/*ct        112*/
      t6=1.-1./(1.+(96.9014*t3)**2)**4                                          !/*ct        113*/
      go to 190                                                                 !/*ct        114*/
  160 t3=29.1445*t3                                                             !/*ct        115*/
      t6=nxs(lnxs+2,iex)                                                        !/*ct        116*/
      if(t3.ge.vic(minc))go to 190                                              !/*ct4a       12*/
      do 170 i=2,minc                                                           !/*ct4a       13*/
  170 if(t3.lt.vic(i))go to 180                                                 !/*ct        119*/
  180 ib=jxs(ljxs+2,iex)+i-1                                                    !/*ct        120*/
      t6=xss(ib)+(xss(ib-1)-xss(ib))*(t3-vic(i))/(vic(i-1)-vic(i))              !/*ct        121*/
  190 psc=(.2494351/tpd(1))*(t5**2)*t6*(t2+t5-1.+cs**2)                         !/*ct        122*/
      return                                                                    !/*ct        123*/
!                                                                               !/*ct        124*/
! >>>>>  ipsc=8 -- photon from neutron collision.                               !/*ct        125*/
  200 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b       27*/
      if(mcal.ne.0)go to 240                                                    !/*ct        127*/
      if(ixcos.lt.0)go to 232                                                   !/*ct4c       82*/
      ic=ixcos                                                                  !/*ct        128*/
      ib=ic+32                                                                  !/*ct        129*/
  210 if(ib-ic.eq.1)go to 230                                                   !/*ct        130*/
      ih=(ic+ib)/2                                                              !/*ct        131*/
      if(cs.lt.xss(ih))go to 220                                                !/*ct        132*/
      ic=ih                                                                     !/*ct        133*/
      go to 210                                                                 !/*ct        134*/
  220 ib=ih                                                                     !/*ct        135*/
      go to 210                                                                 !/*ct        136*/
  230 psc=.03125/(xss(ib)-xss(ic))                                              !/*ct        137*/
      return                                                                    !/*ct        138*/
!                                                                               !/*ct4c       83*/
!        sample tabular rather than equiprobable bin distribution.              !/*ct4c       84*/
  232 ic=-ixcos                                                                 !/*ct4c       85*/
      np=nint(xss(ic))                                                          !/*ct4c       86*/
      jj=nint(xss(ic+1))                                                        !/*ct4c       87*/
      if(xss(ic+2).gt.cs.or.xss(ic+1+np).lt.cs)go to 600                        !/*ct4c       88*/
      ic=ic+3                                                                   !/*ct4c       89*/
  234 if(xss(ic).gt.cs)go to 236                                                !/*ct4c       90*/
      ic=ic+1                                                                   !/*ct4c       91*/
      go to 234                                                                 !/*ct4c       92*/
  236 psc=xss(ic+np-1)                                                          !/*ct4c       93*/
      if(jj.ne.1)psc=xss(ic+np-1)+(cs-xss(ic-1))*                               !/*ct4c       94*/
     & (xss(ic+np)-xss(ic+np-1))/(xss(ic)-xss(ic-1))                            !/*ct4c       95*/
      return                                                                    !/*ct4c       96*/
!                                                                               !/*ct        139*/
!        multigroup cross sections case.                                        !/*ct        140*/
  240 n=nint(xss(jxs(ljxs+15,iex)))                                             !/*ct4c       97*/
      if(n.le.0)return                                                          !/*ct        142*/
      l=nint(xss(jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+16,iex)+1))+ixcos-2))        !/*ct4c       98*/
      if(l.eq.0)return                                                          !/*ct        144*/
      j=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+17,iex)+1))+l-2                       !/*ct4c       99*/
      if(cs.ge.xss(j+n-1).or.cs.lt.xss(j))go to 600                             !/*ct        146*/
  250 j=j+1                                                                     !/*ct        147*/
      if(cs.gt.xss(j))go to 250                                                 !/*ct        148*/
      psc=1./((xss(j)-xss(j-1))*(n-1))                                          !/*ct        149*/
      return                                                                    !/*ct        150*/
!                                                                               !/*ct        151*/
! >>>>>  ipsc=9 -- neutron from s(a,b) collision.                               !/*ct        152*/
  260 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b       28*/
      if(ixcos.eq.0.and.ntyn.eq.2)go to 350                                     !/*ct        154*/
      if(ixcos.gt.0)go to 290                                                   !/*ct        155*/
      if(ixcos.eq.0)go to 600                                                   !/*ct        156*/
!                                                                               !/*ct        157*/
!        equally-probable angle bins.  ixcos < 0                                !/*ct        158*/
      ll=nxs(lnxs+3*ntyn,iet)                                                   !/*ct        159*/
      if(cs.lt.xss(-ixcos).or.cs.gt.xss(ll-ixcos))go to 600                     !/*ct        160*/
      do 270 i=1,ll                                                             !/*ct        161*/
  270 if(cs.le.xss(i-ixcos))go to 280                                           !/*ct        162*/
  280 psc=1./(ll*(xss(i-ixcos-1)-xss(i-ixcos)))                                 !/*ct        163*/
      return                                                                    !/*ct        164*/
!                                                                               !/*ct        165*/
!        equally-probable discrete angles.  ixcos > 0                           !/*ct        166*/
  290 if(ntyn.ne.1)go to 300                                                    !/*ct        167*/
      ri=rtc(krtc+1,iet)                                                        !/*ct        168*/
      ll=nxs(lnxs+3,iet)                                                        !/*ct        169*/
      ni=nxs(lnxs+4,iet)*(ll+2)                                                 !/*ct        170*/
      go to 310                                                                 !/*ct        171*/
  300 ri=rtc(krtc+4,iet)                                                        !/*ct        172*/
      ll=nxs(lnxs+6,iet)                                                        !/*ct        173*/
      ni=ll+1                                                                   !/*ct        174*/
  310 psc=0.                                                                    !/*ct        175*/
      a1=-10.                                                                   !/*ct        176*/
      a0=-10.                                                                   !/*ct        177*/
      b0=10.                                                                    !/*ct        178*/
      b1=10.                                                                    !/*ct        179*/
      ic=ixcos-1                                                                !/*ct        180*/
      ib=ixcos+ll+1                                                             !/*ct        181*/
  320 if(ib-ic.eq.1)go to 340                                                   !/*ct        182*/
      ih=(ic+ib)/2                                                              !/*ct        183*/
      if(cs.lt.(xss(ih)+ri*(xss(ih+ni)-xss(ih))))go to 330                      !/*ct        184*/
      ic=ih                                                                     !/*ct        185*/
      go to 320                                                                 !/*ct        186*/
  330 ib=ih                                                                     !/*ct        187*/
      go to 320                                                                 !/*ct        188*/
  340 if(ic.gt.ixcos)a1=xss(ic-1)+ri*(xss(ic-1+ni)-xss(ic-1))                   !/*ct        189*/
      if(ic.ge.ixcos)a0=xss(ic)+ri*(xss(ic+ni)-xss(ic))                         !/*ct        190*/
      if(ib.le.ixcos+ll)b0=xss(ib)+ri*(xss(ib+ni)-xss(ib))                      !/*ct        191*/
      if(ib.lt.ixcos+ll)b1=xss(ib+1)+ri*(xss(ib+1+ni)-xss(ib+1))                !/*ct        192*/
      da=min(.5*(b0-a0),.5*(a0-a1),1.-a0,a0+1.)                                 !/*ct        193*/
      db=min(.5*(b0-a0),.5*(b1-b0),1.-b0,b0+1.)                                 !/*ct        194*/
      if(cs.lt.a0+da)psc=.5/((ll+1)*da)                                         !/*ct        195*/
      if(cs.gt.b0-db)psc=.5/((ll+1)*db)                                         !/*ct        196*/
      return                                                                    !/*ct        197*/
!                                                                               !/*ct        198*/
!        exact treatment of coherent elastic scattering. ixcos=0, ntyn=2        !/*ct        199*/
  350 nc=nint(xss(jxs(ljxs+4,iet)))                                             !/*ct4c      100*/
      lo=jxs(ljxs+5,iet)-nc                                                     !/*ct        201*/
      ic=lo-1                                                                   !/*ct        202*/
      ib=lo+ktc(kktc+2,iet)                                                     !/*ct        203*/
      lh=ib-1                                                                   !/*ct        204*/
      a1=-10.*erg                                                               !/*ct        205*/
      a0=-10.*erg                                                               !/*ct        206*/
      b0=10.*erg                                                                !/*ct        207*/
      b1=10.*erg                                                                !/*ct        208*/
  360 if(ib-ic.eq.1)go to 380                                                   !/*ct        209*/
      ih=(ic+ib)/2                                                              !/*ct        210*/
      if(cs*erg.gt.erg-2.*xss(ih))go to 370                                     !/*ct        211*/
      ic=ih                                                                     !/*ct        212*/
      go to 360                                                                 !/*ct        213*/
  370 ib=ih                                                                     !/*ct        214*/
      go to 360                                                                 !/*ct        215*/
  380 if(ib.lt.lh)a1=erg-2.*xss(ib+1)                                           !/*ct        216*/
      if(ib.le.lh)a0=erg-2.*xss(ib)                                             !/*ct        217*/
      if(ic.ge.lo)b0=erg-2.*xss(ic)                                             !/*ct        218*/
      if(ic.gt.lo)b1=erg-2.*xss(ic-1)                                           !/*ct        219*/
      da=min(.5*(b0-a0),.5*(a0-a1),erg-a0,a0+erg,.05*erg)                       !/*ct        220*/
      db=min(.5*(b0-a0),.5*(b1-b0),erg-b0,b0+erg,.05*erg)                       !/*ct        221*/
      if(cs*erg.lt.a0+da)go to 390                                              !/*ct        222*/
      if(cs*erg.lt.b0-db)go to 600                                              !/*ct        223*/
      dx=xss(ic+nc)                                                             !/*ct        224*/
      if(ic.gt.lo)dx=dx-xss(ic+nc-1)                                            !/*ct        225*/
      psc=.5*erg*dx/(xss(lh+nc)*db)                                             !/*ct        226*/
      return                                                                    !/*ct        227*/
  390 dx=xss(ib+nc)                                                             !/*ct        228*/
      if(ib.gt.lo)dx=dx-xss(ib+nc-1)                                            !/*ct        229*/
      psc=.5*erg*dx/(xss(lh+nc)*da)                                             !/*ct        230*/
      return                                                                    !/*ct        231*/
!                                                                               !/*ct        232*/
! >>>>>  ipsc=10 -- particle from general source.                               !/*ct        233*/
  400 am=min(max(-one,uuu*sff(1,13)+vvv*sff(2,13)+www*sff(3,13)),one)*          !/*ct        234*/
     & sign(one,sff(1,15))                                                      !/*ct        235*/
      ic=abs(isic(5))                                                           !/*ct        236*/
      if(ic.ne.0)go to 410                                                      !/*ct        238*/
      if(idefv(5).ne.0.and.ivdis(5).eq.0)go to 550                              !/*ct        239*/
      psc=2.*max(zero,am)                                                       !/*ct        240*/
      return                                                                    !/*ct        241*/
  410 if(k(2,ic).eq.0)go to 420                                                 !/*ct        242*/
      if(am.lt.q(6).or.am.gt.q(7))go to 600                                     !/*ct        243*/
      if(k(2,ic).eq.7)psc=(q(2)+1.)*abs(am)**q(2)/q(9)                          !/*ct        244*/
      if(k(2,ic).eq.8.and.q(2).eq.0.)psc=1./(q(7)-q(6))                         !/*ct        245*/
      if(k(2,ic).eq.8.and.q(2).ne.0.)psc=q(2)*exp(am*q(2))/q(9)                 !/*ct        246*/
      go to 450                                                                 !/*ct        247*/
  420 if(k(5,ic).ne.0)go to 550                                                 !/*ct        248*/
      do 430 ib=1,k(4,ic)-1                                                     !/*ct        249*/
  430 if((am-spf(k(13,ic)+1,ib))*(am-spf(k(13,ic)+1,ib+1)).le.0.)               !/*ct        250*/
     & go to 440                                                                !/*ct        251*/
      go to 600                                                                 !/*ct        252*/
  440 i=ic                                                                      !/*ct        253*/
      if(ivdd(5).ne.5.and.ic.eq.ivdis(5))i=ivdis(ivdd(5))                       !/*ct        254*/
      f=(spf(k(13,i)+2,ib+1)-spf(k(13,i)+2,ib))/                                !/*ct        255*/
     & abs(spf(k(13,ic)+1,ib+1)-spf(k(13,ic)+1,ib))                             !/*ct        256*/
      if(k(19,i).eq.0)psc=f*spf(k(13,i)+3,ib+1)                                 !/*ct        257*/
      if(k(19,i).ne.0)psc=spf(k(13,i)+2,ib)+                                    !/*ct        258*/
     & f*abs(am-spf(k(13,i)+1,ib))                                              !/*ct        259*/
      if(psc.eq.0.)return                                                       !/*ct        260*/
  450 if(ivdd(3).ne.5.and.ivdd(5).ne.3)go to 500                                !/*ct        261*/
      if(ivdd(3).eq.5.and.k(6,ivdis(5)).ne.0.and.k(8,ivdis(3)).eq.0)            !/*ct        262*/
     & go to 500                                                                !/*ct        263*/
      if(ivdd(5).eq.3.and.(k(6,ivdis(5)).ne.0.or.k(9,ivdis(5)).ne.0))           !/*ct        264*/
     & go to 500                                                                !/*ct        265*/
      erg=14.                                                                   !/*ct        266*/
      kd=ivdis(3)                                                               !/*ct        267*/
      if(k(6,kd).ne.0)go to 460                                                 !/*ct        268*/
      erg=spf(k(13,kd)+1,ib)                                                    !/*ct        269*/
      if(k(5,kd).eq.0)erg=erg+(spf(k(13,kd)+1,ib+1)-erg)*                       !/*ct        270*/
     & (am-spf(k(13,ic)+1,ib))/(spf(k(13,ic)+1,ib+1)-spf(k(13,ic)+1,ib))        !/*ct        271*/
      go to 500                                                                 !/*ct        272*/
  460 j=ib                                                                      !/*ct        273*/
      if(k(8,kd).eq.0)go to 480                                                 !/*ct        274*/
      do 470 j=2,2*k(4,kd),2                                                    !/*ct4a       14*/
  470 if(spf(k(13,kd)+1,j-1).gt.am)go to 480                                    !/*ct        276*/
      call expirx(1,'calcps',                                                   !/*ct        277*/
     & 'inadequate entries on ds card with q option.')                          !/*ct        278*/
      return                                                                    !/*ct        279*/
  480 tpp(1)=spf(k(13,kd)+1,j)                                                  !/*ct4c      101*/
  490 ld=tpp(1)                                                                 !/*ct4c      102*/
      if(ld.eq.0)go to 500                                                      !/*ct        282*/
      isic(k(12,ld))=-abs(isic(k(12,ld)))                                       !/*ct4a       15*/
      call smpsrc(tpp,ld,i,f)                                                   !/*ct4c      103*/
      if(k(6,ld).ne.0)go to 490                                                 !/*ct        284*/
      erg=tpp(1)                                                                !/*ct4c      104*/
  500 if(jsu.eq.0)return                                                        !/*ct        286*/
      do 510 m=1,nlse                                                           !/*ct        287*/
      icl=lse(klse+m)                                                           !/*ct        288*/
      call chkcel(icl,2,j)                                                      !/*ct        289*/
  510 if(j.eq.0) return                                                         !/*ct        290*/
      do 520 icl=1,mxa                                                          !/*ct        291*/
      call chkcel(icl,2,j)                                                      !/*ct        292*/
  520 if(j.eq.0)go to 530                                                       !/*ct        293*/
      call expirx(1,'calcps','source point is not in any cell.')                !/*ct        294*/
      return                                                                    !/*ct        295*/
  530 nlse=nlse+1                                                               !/*ct        296*/
      lse(klse+nlse)=icl                                                        !/*ct        297*/
      return                                                                    !/*ct        298*/
!                                                                               !/*ct        299*/
! >>>>>  ipsc=11 -- monodirectional particle from general source.               !/*ct        300*/
!        move to the point opposite the detector point.                         !/*ct        301*/
  540 b=uuu*uuu9(1)+vvv*vvv9(1)+www*www9(1)                                     !/*ct        302*/
      if(b.lt.0.)go to 600                                                      !/*ct        303*/
      xxx=xxx+(uuu-uuu9(1)*b)*ddet                                              !/*ct        304*/
      yyy=yyy+(vvv-vvv9(1)*b)*ddet                                              !/*ct        305*/
      zzz=zzz+(www-www9(1)*b)*ddet                                              !/*ct        306*/
!                                                                               !/*ct        307*/
!        check that the point is still on the source disk.                      !/*ct        308*/
      r=(xxx-sff(1,6))**2+(yyy-sff(2,6))**2+(zzz-sff(3,6))**2                   !/*ct        309*/
      ic=abs(isic(10))                                                          !/*ct        310*/
      if(r.gt.q(7)**2.or.r.lt.q(6)**2)go to 600                                 !/*ct        311*/
      ddet=ddet*b                                                               !/*ct        312*/
      uuu=uuu9(1)                                                               !/*ct        313*/
      vvv=vvv9(1)                                                               !/*ct        314*/
      www=www9(1)                                                               !/*ct        315*/
      if(lev.ne.0)call uplev(uuu)                                               !/*ct4b       29*/
      psc=1./sff(1,16)                                                          !/*ct        317*/
!                                                                               !/*ct        318*/
!        check that the point is still in the cookie-cutter cell.               !/*ct        319*/
      if(sff(1,14).eq.0.)return                                                 !/*ct        320*/
      call chkcel(int(sff(1,14)),0,j)                                           !/*ct        321*/
      if(j.ne.0)go to 600                                                       !/*ct        322*/
      return                                                                    !/*ct        323*/
!                                                                               !/*ct        324*/
!        warn of no contributions from discrete dir.                            !/*ct        325*/
  550 call errprn(1,nkrp,0,zero,zero,' ',' ',                                   !/*ct4b       30*/
     & 'discrete dir source direct detector contribution missed.')              !/*ct4a       17*/
      go to 600                                                                 !/*ct        336*/
!                                                                               !/*ct        337*/
! >>>>>  ipsc=12 -- particle from surface source                                !/*ct        338*/
  580 psc=.5                                                                    !/*ctx         6*/
      if(jsu.eq.0)return                                                        !/*ctx         7*/
      cs=angl()                                                                 !/*ctx         8*/
      if((uuu9(npb)*ang(1)+vvv9(npb)*ang(2)+www9(npb)*ang(3))*cs.lt.0.)         !/*ct4a       19*/
     & go to 600                                                                !/*ct4a       20*/
      psc=(srv(1,18)+1.)*abs(cs)**srv(1,18)                                     !/*ct        342*/
      return                                                                    !/*ct        343*/
!                                                                               !/*ct        344*/
! >>>>>  ipsc=13 -- bremsstrahlung photon.                                      !/*ct        345*/
  590 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b       31*/
      psc=(1.-deb**2)/(2.*(deb*cs-1.)**2)                                       !/*ct        347*/
      return                                                                    !/*ct        348*/
!                                                                               !/*ct4a       25*/
! >>>>>  ipsc=14 -- neutron from kalbach-87 endf/b-vi coupled energy-           !/*ct4a       26*/
!        angle collision (law 44).  similar to ipsc=5 inelastic scatter.        !/*ct4a       27*/
  592 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4b       32*/
      a1=1.+awn(lawn+iex)                                                       !/*ct4a       29*/
      t3=1.-ergace*a1**2/eg0                                                    !/*ct4a       30*/
      if(t3.ge.cs**2)go to 600                                                  !/*ct4a       31*/
      t1=1./a1                                                                  !/*ct4a       32*/
!        erg from carter and cashwell eqn. 5.14                                 !/*ct4a       33*/
      erg=eg0*(t1*(cs+sqrt(cs**2-t3)))**2                                       !/*ct4a       34*/
      if(erg.lt.elc(ipt))return                                                 !/*ct4a       35*/
      t2=sqrt(erg/ergace)                                                       !/*ct4a       36*/
      t4=t1*sqrt(eg0/erg)                                                       !/*ct4a       37*/
!        cm from carter and cashwell eqn. 5.10                                  !/*ct4a       38*/
      cm=t2*(cs-t4)                                                             !/*ct4a       39*/
      t5=sinh(tpd(2))*(1.-cs*t4)                                                !/*ct4a       40*/
      if(t5.le.0.)go to 600                                                     !/*ct4a       41*/
!        kalbach-87 psc=(.5*a/sinh(a))*(cosh(a*cm)+r*sinh(a*cm))                !/*ct4a       42*/
!        d cm/d cs = t2/(1.-cs*t4)  carter and cashwell eqn 5.13                !/*ct4a       43*/
      psc=(cosh(tpd(2)*cm)+tpd(1)*sinh(tpd(2)*cm))*.5*tpd(2)*t2/t5              !/*ct4a       44*/
      return                                                                    !/*ct4a       45*/
!                                                                               !/*ct4c2       6*/
! >>>>>  ipsc=17 --  photonuclear kalbach-87 endf/b-vi coupled energy-          !/*ct4c2       7*/
!        angle collision (law 44).                                              !/*ct4c2       8*/
!        kalbach-87 psc=(.5*a/sinh(a))*(cosh(a*cs)+r*sinh(a*cs))                !/*ct4c2       9*/
!        tpd(1)=r, tpd(2)=a                                                     !/*ct4c2      10*/
  594 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4c2      11*/
      psc=.5*tpd(2)/sinh(tpd(2))                                                !/*ct4c2      12*/
     & *(cosh(tpd(2)*cs)+tpd(1)*sinh(tpd(2)*cs))                                !/*ct4c2      13*/
      return                                                                    !/*ct4c2      14*/
!                                                                               !/*ct4c2      15*/
! >>>>>  ipsc=18 --  photonuclear particle from angular distribution.           !/*ct4c2      16*/
!        currently ignore center-of-mass to lab conversion for photons.         !/*ct4c2      17*/
  596 cs=uold(1)*uuu+uold(2)*vvv+uold(3)*www                                    !/*ct4c2      18*/
      go to 60                                                                  !/*ct4c2      19*/
!                                                                               !/*ct        349*/
!        return with psc=0. if scattering toward detector is impossible.        !/*ct        350*/
  600 psc=0.                                                                    !/*ct        351*/
      return                                                                    !/*ct        352*/
      end                                                                       !/*ct        353*/

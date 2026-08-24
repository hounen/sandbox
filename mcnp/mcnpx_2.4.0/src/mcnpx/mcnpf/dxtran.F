!#ident dxtran.F from deck dx                                                   !/*dx          1*/
      subroutine dxtran                                                         !/*dx          2*/
!        create particles on the dxtran spheres.                                !/*dx          3*/
      use global_data                                                           !/*dxx         1*/
!                                                                               !/*dxx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dxx         3*/
      implicit integer(kindi) (i-n)                                             !/*dxx         4*/
!                                                                               !/*dxx         5*/
#ifdef MESHTAL                                                                  /*!dxx         6*/
      dimension zd(10),jz(5)                                                    !/*dxx         7*/
#endif                                                                          /*!dxx         8*/
!                                                                               !/*dx          5*/
!        return if kcode problem is not settled.  initialize.                   !/*dx4b        1*/
      if(kc8.gt.0)return                                                        !/*dx4c        1*/
      tw=wgt                                                                    !/*dx          6*/
      if(ipsc.eq.10)tw=tw*cbwf                                                  !/*dx4b        3*/
      te=erg                                                                    !/*dx          8*/
!                                                                               !/*dx          9*/
!        adjust the weight according to the cell probability.                   !/*dx         10*/
      if(dxcp(ldxp,ipt,icl).eq.0.)go to 110                                     !/*dx         11*/
      if(dxcp(ldxp,ipt,icl).ge.1.)go to 10                                      !/*dx         12*/
      if(rang().gt.dxcp(ldxp,ipt,icl))go to 110                                 !/*dx         13*/
      tw=tw/dxcp(ldxp,ipt,icl)                                                  !/*dx         14*/
!                                                                               !/*dx         22*/
!        pre-processing for special cases.                                      !/*dx         23*/
   10 go to(        130, 50, 80,130,130,130,130,130,                            !/*dx4c2       1*/
     &      130,130,130, 80, 80, 80,130,130)ipsc-2                              !/*dx4c2       2*/
      go to(130,130, 90, 80,100,130)ipsc-100                                    !/*dx4c2       3*/
      call expirx(1,'dxtran','illegal value for ipsc.')                         !/*dx4c2       4*/
      return                                                                    !/*dx4c2       5*/
!                                                                               !/*dx         26*/
! >>>>>  ipsc=4 -- neutron from collision with moving target.                   !/*dx         27*/
   50 tw=cmult*tw                                                               !/*dx         28*/
      if(ntyn.ge.0)go to 60                                                     !/*dx         29*/
      f=awn(lawn+iex)/ycn                                                       !/*dx         30*/
      tpd(7)=f*ssr                                                              !/*dx         31*/
      tpd(1)=tpd(7)                                                             !/*dx         32*/
      if(ntyn.ne.-99)tpd(1)=(1.+awn(lawn+iex))*sqrt(ergace/eg0)                 !/*dx         33*/
      tpd(2)=eg0/(1.+awn(lawn+iex))**2                                          !/*dx         34*/
      tpd(4)=uold(1)+f*vtr(1)                                                   !/*dx4b        4*/
      tpd(5)=uold(2)+f*vtr(2)                                                   !/*dx4b        5*/
      tpd(6)=uold(3)+f*vtr(3)                                                   !/*dx4b        6*/
      go to 70                                                                  !/*dx         38*/
   60 g=sqrt(2.*ttn/awn(lawn+iex))                                              !/*dx         39*/
      tpd(7)=g*ssr                                                              !/*dx         40*/
      tpd(1)=sqrt(2.*ergace)                                                    !/*dx         41*/
      tpd(2)=.5                                                                 !/*dx         42*/
      tpd(4)=g*vtr(1)                                                           !/*dx         43*/
      tpd(5)=g*vtr(2)                                                           !/*dx         44*/
      tpd(6)=g*vtr(3)                                                           !/*dx         45*/
   70 tpd(3)=tpd(4)**2+tpd(5)**2+tpd(6)**2                                      !/*dx         46*/
      if(tpd(1)**2.ge.tpd(3))go to 130                                          !/*dx         47*/
      tw=tw*2.                                                                  !/*dx         48*/
      if(rang().gt..5)tpd(3)=-tpd(3)                                            !/*dx         49*/
      go to 130                                                                 !/*dx         50*/
!                                                                               !/*dx         51*/
! >>>>>  ipsc=5 -- neutron from collision with stationary target and            !/*dx         52*/
!        ipsc=14 -- neutron from kalbach-87 (law 44) collision and              !/*dx4a        3*/
!        ipsc=15 -- neutron from law 67 (endf/b-vi law 7) collision and         !/*dx4a        4*/
!        ipsc=16 -- neutron from law 61 (tabulated energy / angle) and          !/*dx4c        3*/
!        ipsc=104 -- particle from isotropic multigroup collision.              !/*dx         53*/
   80 tw=cmult*tw                                                               !/*dx         54*/
      go to 130                                                                 !/*dx         55*/
!                                                                               !/*dx         56*/
! >>>>>  ipsc=103 -- photon from double fluorescence.                           !/*dx         57*/
   90 if(rang().gt..5)te=xss(jxs(ljxs+4,iex)+3*nxs(lnxs+4,iex)+1)               !/*dx         58*/
      tw=2.*tw                                                                  !/*dx         59*/
      if(te.gt.elc(2))go to 130                                                 !/*dx         60*/
      go to 110                                                                 !/*dx         61*/
!                                                                               !/*dx         62*/
! >>>>>  ipsc=105 -- photon from pair production.                               !/*dx         63*/
  100 tw=2.*tw                                                                  !/*dx         64*/
      go to 130                                                                 !/*dx         65*/
!                                                                               !/*dx         66*/
!        increment nziytc for particles that failed.                            !/*dx         67*/
  110 l=icl+mxa*(ndx(1)*(ipt-1)-1)                                              !/*dx         68*/
      do 120 ix=1,ndx(ipt)                                                      !/*dx         69*/
      if(idx.ne.ix)nziytc(1,ix,ipt)=nziytc(1,ix,ipt)+1                          !/*dx         70*/
  120 if(idx.ne.ix)dxc(kdxc+3,l+mxa*ix)=dxc(kdxc+3,l+mxa*ix)+1.                 !/*dx         71*/
      return                                                                    !/*dx         72*/
!                                                                               !/*dx         73*/
!        do all of the spheres for the current kind of particle.                !/*dx         74*/
  130 call savpar(0,2)                                                          !/*dx4c2       6*/
      nb=npb                                                                    !/*dx4c2       7*/
      do 370 ix=1,ndx(ipt)                                                      !/*dx4c2       8*/
      if(idx.eq.ix)go to 370                                                    !/*dx         76*/
      ld=icl9(npb)+mxa*(ix-1+(ipt-1)*ndx(1))                                    !/*dx         77*/
      dxc(kdxc+3,ld)=dxc(kdxc+3,ld)+1.                                          !/*dx         78*/
      wgt=tw                                                                    !/*dx         79*/
      if(dxcp(ldxp+ix,ipt,icl).eq.0.)go to 280                                  !/*dx         80*/
      if(dxcp(ldxp+ix,ipt,icl).eq.1.)go to 140                                  !/*dx         81*/
      if(rang().gt.dxcp(ldxp+ix,ipt,icl))go to 280                              !/*dx         82*/
      wgt=wgt/dxcp(ldxp+ix,ipt,icl)                                             !/*dx         83*/
  140 idx=ix                                                                    !/*dx         84*/
!                                                                               !/*dx         86*/
!        calculate the distance to the center of the sphere.                    !/*dx         87*/
      if(lev.ne.0)go to 150                                                     !/*dx         88*/
      f=dxx(ipt,1,ix)-xxx                                                       !/*dx         89*/
      g=dxx(ipt,2,ix)-yyy                                                       !/*dx         90*/
      h=dxx(ipt,3,ix)-zzz                                                       !/*dx         91*/
      go to 160                                                                 !/*dx         92*/
  150 f=dxx(ipt,1,ix)-udt(1,0)                                                  !/*dx         93*/
      g=dxx(ipt,2,ix)-udt(2,0)                                                  !/*dx         94*/
      h=dxx(ipt,3,ix)-udt(3,0)                                                  !/*dx         95*/
  160 ds=f**2+g**2+h**2                                                         !/*dx         96*/
      rd=1./sqrt(ds)                                                            !/*dx         97*/
!                                                                               !/*dx         98*/
!        sample the direction to the point on the dxtran sphere.                !/*dx         99*/
      pd=1.                                                                     !/*dx        100*/
      erg=te                                                                    !/*dx        101*/
      psc=.5                                                                    !/*dx        102*/
      if(ipsc.eq.11)go to 190                                                   !/*dx        103*/
      if(dxx(ipt,5,ix).gt.1.e-5*ds)go to 168                                    !/*dx4c        4*/
      d4=dxx(ipt,4,ix)                                                          !/*dx4c        6*/
      d5=dxx(ipt,5,ix)                                                          !/*dx4c        7*/
      if(d4.le.1.e-10*ds)d4=d5                                                  !/*dx4c        8*/
      if(rang()*(4.*d4+d5).lt.d5-d4)go to 166                                   !/*dx4c        9*/
      cf=(ds-rang()*.5*d4)/ds                                                   !/*dx4c       10*/
      pd=.1*(4.*d4+d5)/ds                                                       !/*dx4c       11*/
      go to 180                                                                 !/*dx4c       12*/
  166 cf=(ds-.5*(d5-rang()*(d5-d4)))/ds                                         !/*dx4c       13*/
      pd=.5*(4.*d4+d5)/ds                                                       !/*dx4c       14*/
      go to 180                                                                 !/*dx4c       15*/
  168 ci=sqrt(ds-dxx(ipt,4,ix))*rd                                              !/*dx4c       16*/
      co=sqrt(ds-dxx(ipt,5,ix))*rd                                              !/*dx        105*/
      pd=5.-4.*ci-co                                                            !/*dx        106*/
      if(rang()*pd.lt.ci-co)go to 170                                           !/*dx        107*/
      cf=(ci+rang()*(1.-ci))                                                    !/*dx        108*/
      pd=.2*pd                                                                  !/*dx        109*/
      go to 180                                                                 !/*dx        110*/
  170 cf=(co+rang()*(ci-co))                                                    !/*dx        111*/
  180 tpp(1)=f*rd                                                               !/*dx        112*/
      tpp(2)=g*rd                                                               !/*dx        113*/
      tpp(3)=h*rd                                                               !/*dx        114*/
      call rotas(cf,tpp,uuu,0,irt)                                              !/*dx4b        7*/
      if(lev.ne.0)call dddlev                                                   !/*dx        116*/
!                                                                               !/*dx        117*/
!        calculate the scattering probability and energy.                       !/*dx        118*/
      if(ipsc.gt.100)go to 190                                                  !/*dx        119*/
      call calcps                                                               !/*dx        120*/
      if(kdb.ne.0)go to 380                                                     !/*dx4c2       9*/
      if(psc.eq.0.)go to 290                                                    !/*dx        122*/
      if(erg.lt.elc(ipt))go to 310                                              !/*dx        123*/
      if(gpt(ipt).ne.0.)                                                        !/*dxx         9*/
     & vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                     !/*dxx        10*/
!                                                                               !/*dx        125*/
!        calculate the transmission to the new location.                        !/*dx        126*/
  190 if(lev.eq.0)q=f*uuu+g*vvv+h*www                                           !/*dx        127*/
      if(lev.ne.0)q=f*udt(4,0)+g*udt(5,0)+h*udt(6,0)                            !/*dx        128*/
      if(q.lt.0.)go to 290                                                      !/*dx        129*/
      dl=dxx(ipt,5,ix)-ds+q**2                                                  !/*dx        130*/
      if(dl.lt.0.)go to 290                                                     !/*dx        131*/
      dd=q-sqrt(dl)                                                             !/*dx        132*/
      dd=dd-dd*1e-10                                                            !/*dx        133*/
      if(dd.ge.vel*(tco(ipt)-tme))go to 300                                     !/*dx        134*/
      s=0.                                                                      !/*dx        135*/
      if(ddx(ipt,1,ix).lt.0.)s=-ddx(ipt,1,ix)/(wgt*psc*pd)                      !/*dx        136*/
      if(ddx(ipt,1,ix).gt.0..and.npstc.gt.200)                                  !/*dx        137*/
     & s=ddx(ipt,1,ix)*dxd(ldxd+ipt,24,ix)/(wgt*psc*pd)                         !/*dx        138*/
      call transm(dd,s,iz)                                                      !/*dx4b        8*/
      if(iz.ne.0)go to 295                                                      !/*dx4b        9*/
!                                                                               !/*dx        142*/
!        set up and bank the new particle.                                      !/*dx        143*/
      jsu=0                                                                     !/*dx        144*/
      wgt=wgt*psc*pd*exp(-amfp)                                                 !/*dx        145*/
      do 200 i=1,nipt                                                           !/*dxx        11*/
      elc(i)=elp(lelp+i,icl)                                                    !/*cor4-1    130*/
      fiml(i)=fim(lfim+i,icl)                                                   !/*cor4-1    131*/
      do 200 l=0,lev-1                                                          !/*cor4-1    132*/
      elc(i)=max(elc(i),elp(lelp+i,int(udt(7,l))))                              !/*cor4-1    133*/
  200 fiml(i)=fiml(i)*fim(lfim+i,int(udt(7,l)))                                 !/*cor4-1    134*/
      if(wgt*fiml(1).gt.fiml9(1,1)*dxw(ipt,2))go to 210                         !/*cor4-1    135*/
      if(wgt*fiml(1).lt.fiml9(1,1)*dxw(ipt,1)*rang())go to 320                  !/*cor4-1    136*/
      wgt=fiml9(1,1)*dxw(ipt,1)/fiml(1)                                         !/*cor4-1    137*/
  210 if(erg.lt.elc(ipt))go to 310                                              !/*cor4-1    138*/
      npa=1                                                                     !/*cor4-1    139*/
      iex=iex9(npb)                                                             !/*cor4-1    140*/
      call gain_pax (igain_all_pls_dxtran, ipt, 1, wgt, wgt*erg)                !/*dxx        12*/
      call contrib_pwb (ipt,ipwb_variance_dxtran,icl,wgt)                       !/*dxx        13*/
      call bankit(1)                                                            !/*dx4a        5*/
!                                                                               !/*dx        164*/
!        accumulate data for dxtran diagnostics.                                !/*dx        165*/
      dxd(kdxd+ipt,20,ix)=dxd(kdxd+ipt,20,ix)+wgt                               !/*dx        166*/
      j=0                                                                       !/*dx        167*/
      if(ddx(ipt,1,ix).ge.0.)go to 230                                          !/*dx        168*/
      if(wgt.ge.-ddx(ipt,1,ix)*ddx(ipt,2,ix))j=1                                !/*dx        169*/
      do 220 i=1,8                                                              !/*dx        170*/
  220 if(wgt.le.-talb(i,2)*ddx(ipt,1,ix))go to 250                              !/*dx        171*/
      go to 250                                                                 !/*dx        172*/
  230 i=9                                                                       !/*dx        173*/
      if(npstc.le.200)go to 250                                                 !/*dx        174*/
      if(wgt.ge.ddx(ipt,2,ix)*dxd(ldxd+ipt,24,ix))j=1                           !/*dx        175*/
      i=1                                                                       !/*dx4a        6*/
      if(wgt.le.ddx(ipt,1,ix)*dxd(ldxd+ipt,24,ix))go to 250                     !/*dx4a        7*/
      do 240 i=2,7                                                              !/*dx4a        8*/
  240 if(wgt.le.talb(i,1)*dxd(ldxd+ipt,24,ix))go to 250                         !/*dx        178*/
  250 dxd(kdxd+ipt,i,ix)=dxd(kdxd+ipt,i,ix)+1.                                  !/*dx        179*/
      dxd(kdxd+ipt,i+9,ix)=dxd(kdxd+ipt,i+9,ix)+wgt                             !/*dx        180*/
      if(wgt.gt.dxd(kdxd+ipt,21,ix))dxd(kdxd+ipt,23,ix)=npstc                   !/*dx        181*/
      dxd(kdxd+ipt,21,ix)=max(dxd(kdxd+ipt,21,ix),wgt)                          !/*dx        182*/
      dxc(kdxc+1,ld)=dxc(kdxc+1,ld)+1.                                          !/*dx        183*/
      dxc(kdxc+2,ld)=dxc(kdxc+2,ld)+wgt                                         !/*dx        184*/
!                                                                               !/*dx        185*/
!        print a line for especially heavy created particles.                   !/*dx        186*/
      if(j.eq.0)go to 330                                                       !/*dx        187*/
#ifdef MULTT                                                                    /*!dxxk        1*/
      call thread_lockon(jlock)                                                 !/*dxxk        2*/
#endif                                                                          /*!dxxk        3*/
      if(dxd(ldxd+ipt,22,ix).ge.100.)go to 270                                  !/*dx        189*/
      dxd(ldxd+ipt,22,ix)=dxd(ldxd+ipt,22,ix)+1.                                !/*dx        190*/
      if(dbcn(26).ne.0.)then                                                    !/*dxx        15*/
      if(mct.ge.-1)write(iuo,260)hnp(ipt)(1:1),idx,wgt,wgt9(npb),               !/*dxx        16*/
     & psc,amfp,dd,erg,ncl(lncl+icl9(npb)),npstc,ncp                            !/*dxx        17*/
      else                                                                      !/*dxx        18*/
      if(mct.ge.-1)write(iuo,260)hnp(ipt),idx,wgt,wgt9(npb),psc,amfp,           !/*dx4c       17*/
     & dd,erg,ncl(lncl+icl9(npb)),npstc,ncp                                     !/*dx4c       18*/
      endif                                                                     !/*dxx        19*/
  260 format(/4h idx,4x,6hdx wgt,4x,7hcol wgt,5x,3hpsc,7x,4hamfp,8x,            !/*dx4c       19*/
     & 2hdd,9x,3herg,5x,4hcell,8x,8hnps  ncp,13x,a11,                           !/*dxx        20*/
     & /i3,1p6e11.4,i6,i11,i5)                                                  !/*dx4c       21*/
  270 continue                                                                  !/*dx        196*/
#ifdef MULTT                                                                    /*!dxxk        4*/
      call thread_lockoff(jlock)                                                !/*dxxk        5*/
#endif                                                                          /*!dxxk        6*/
      go to 330                                                                 !/*dx        198*/
!                                                                               !/*dx        199*/
!        increment nziytc for particles that failed.                            !/*dx        200*/
  280 nziytc(1,ix,ipt)=nziytc(1,ix,ipt)+1                                       !/*dx        201*/
      go to 330                                                                 !/*dx        202*/
  290 nziytc(2,ix,ipt)=nziytc(2,ix,ipt)+1                                       !/*dx        203*/
      go to 330                                                                 !/*dx        204*/
  295 if(kdb.ne.0)go to 380                                                     !/*dx4b       10*/
      nziytc(iz,ix,ipt)=nziytc(iz,ix,ipt)+1                                     !/*dx4b       11*/
      if(iz.ne.5)go to 330                                                      !/*dx4b       12*/
      j=nziy(5,ix,ipt)+nziytc(5,ix,ipt)-1                                       !/*dx4b       13*/
      call errprn(1,j,2,zero+ix,zero+ncl(lncl+icl),'dxt','cel',                 !/*dx4c       22*/
     & 'contribution to dxtran sphere prevented by zero importance.')           !/*dx4b       15*/
      go to 330                                                                 !/*dx4b       16*/
  300 nziytc(6,ix,ipt)=nziytc(6,ix,ipt)+1                                       !/*dx        205*/
      go to 330                                                                 !/*dx        206*/
  310 nziytc(7,ix,ipt)=nziytc(7,ix,ipt)+1                                       !/*dx        207*/
      go to 330                                                                 !/*dx        208*/
  320 nziytc(8,ix,ipt)=nziytc(8,ix,ipt)+1                                       !/*dx        209*/
!                                                                               !/*dx        210*/
!        restore the particle description.                                      !/*dx        211*/
  330 call getpar(nb,2)                                                         !/*dx4c2      10*/
!                                                                               !/*dxx        22*/
#ifdef MESHTAL                                                                  /*!dxx        23*/
      if(mdxflg.ne.0) then                                                      !/*dxx        24*/
         do 363 i=1,6                                                           !/*dxx        25*/
            if(lev.eq.0) then                                                   !/*dxx        26*/
               zd(i)=gpblcm(i)                                                  !/*dxx        27*/
            else                                                                !/*dxx        28*/
               zd(i)=udt(i,0)                                                   !/*dxx        29*/
            endif                                                               !/*dxx        30*/
  363       continue                                                            !/*dxx        31*/
            zd(7)=wgt                                                           !/*dxx        32*/
            zd(8)=erg                                                           !/*dxx        33*/
            jz(1)=mat(lmat+icl)                                                 !/*dxx        34*/
            call gdtal(6,dd,zd,jz)                                              !/*dxx        35*/
      endif                                                                     !/*dxx        36*/
#endif                                                                          /*!dxx        37*/
  370 continue                                                                  !/*dx        218*/
      npb=nb-1                                                                  !/*dx4c2      11*/
      return                                                                    !/*dx        220*/
!                                                                               !/*dx        221*/
!        return with kdb=5 or 6 if the particle gets lost.                      !/*dx        222*/
  380 kdb=kdb+4                                                                 !/*dx        223*/
      npb=nb-1                                                                  !/*dx4c2      12*/
      return                                                                    !/*dx        224*/
      end                                                                       !/*dx        225*/

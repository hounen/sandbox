!#ident acegam.F from deck ag                                                   !/*ag          1*/
      subroutine acegam                                                         !/*ag4a        1*/
!        generate and bank photons from a neutron collision.                    !/*ag          3*/
      use global_data                                                           !/*agx         1*/
!                                                                               !/*agx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*agx         3*/
      implicit integer(kindi) (i-n)                                             !/*agx         4*/
!                                                                               !/*agx         5*/
      character ht*10                                                           !/*ag          6*/
      dimension en(30),yd(2)                                                    !/*ag          7*/
      data f/1.00000000001d0/                                                   !/*ag          8*/
      data en / 1.39e-10, 1.52e-7, 4.14e-7, 1.13e-6, 3.06e-6,                   !/*ag          9*/
     & 8.32e-6, 2.26e-5, 6.14e-5, 1.67e-4, 4.54e-4, 1.235e-3, 3.35e-3,          !/*ag         10*/
     & 9.12e-3, 2.48e-2, 6.76e-2, 0.184, 0.303, 0.500, 0.823, 1.353,            !/*ag         11*/
     & 1.738, 2.232, 2.865, 3.68, 6.07, 7.79, 10.0, 12.0, 13.5, 15.0/           !/*ag         12*/
!                                                                               !/*ag4b        1*/
!        return if kcode problem is not settled.  initialize.                   !/*ag4b        2*/
      if(kc8.gt.0)return                                                        !/*ag4c        1*/
      kp=mpan                                                                   !/*ag4b        4*/
      nt=0                                                                      !/*ag4b        5*/
!                                                                               !/*ag         13*/
!        with pikmt, select the nuclide and generate exactly one photon.        !/*ag         14*/
      if(npikmt.eq.0)go to 30                                                   !/*ag         16*/
      io=iex                                                                    !/*ag         17*/
      t=rang()*totgp1                                                           !/*ag         19*/
      mk=mat(lmat+icl)                                                          !/*ag         20*/
      do 10 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*ag         21*/
      t=t-tgp(ktgp+lme(llme+1,m))*fme(lfme+m)                                   !/*ag         22*/
   10 if(t.lt.0.)go to 20                                                       !/*ag         23*/
      call expirx(1,'acegam','no isotope found')                                !/*ag         24*/
      return                                                                    !/*ag         25*/
   20 iex=lme(llme+1,m)                                                         !/*ag         26*/
      gp=wgt*totgp1/totm                                                        !/*ag         27*/
      kp=ipan(lipa+icl)+m-jmd(ljmd+mk)                                          !/*ag         28*/
      ni=1                                                                      !/*ag         29*/
      lg=1                                                                      !/*ag         30*/
      nt=nxs(lnxs+15,iex)                                                       !/*ag         31*/
      go to 150                                                                 !/*ag         32*/
!                                                                               !/*ag         33*/
!        calculate the probability of production of a photon.                   !/*ag         34*/
   30 l=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*ag4b        6*/
      gp=(xss(l-1)+rtc(krtc+1,iex)*(xss(l)-xss(l-1)))/rtc(krtc+5,iex)           !/*ag4b        7*/
      if(gp.eq.0.)return                                                        !/*ag         37*/
!                                                                               !/*ag         38*/
!        find the neutron energy group in the en table.                         !/*ag         39*/
      if(jxs(ljxs+13,iex).ne.0)go to 90                                         !/*ag         40*/
      ib=31                                                                     !/*ag         41*/
      ic=1                                                                      !/*ag         42*/
   40 if(ib-ic.eq.1)go to 60                                                    !/*ag         43*/
      ih=(ic+ib)/2                                                              !/*ag         44*/
      if(erg.lt.en(ih))go to 50                                                 !/*ag         45*/
      ic=ih                                                                     !/*ag         46*/
      go to 40                                                                  !/*ag         47*/
   50 ib=ih                                                                     !/*ag         48*/
      go to 40                                                                  !/*ag         49*/
   60 lg=jxs(ljxs+12,iex)+nxs(lnxs+3,iex)+(ic-1)*20-1                           !/*ag         50*/
      do 70 it=1,20                                                             !/*ag         51*/
   70 if(xss(lg+it).gt.elc(2))go to 80                                          !/*cor4-1    148*/
      return                                                                    !/*ag         53*/
   80 gp=gp*(1.-(it-1)*.05)                                                     !/*ag         54*/
!                                                                               !/*ag4c        2*/
!        modify gamma production by ratio of unresolved total xsec to           !/*ag4c        3*/
!        smooth (average) total xsec.                                           !/*ag4c        4*/
   90 if(rtc(krtc+11,iex).lt.0.)go to 95                                        !/*ag4c        5*/
      ic=ktc(kktc+1,iex)+jxs(ljxs+1,iex)-1                                      !/*ag4c        6*/
      n=nxs(lnxs+3,iex)                                                         !/*ag4c        7*/
      tz=rtc(krtc+5,iex)/                                                       !/*ag4c        8*/
     & (xss(ic+n)+rtc(krtc+1,iex)*(xss(ic+n+1)-xss(ic+n)))                      !/*ag4c        9*/
      gp=gp*tz                                                                  !/*ag4c       10*/
!                                                                               !/*ag         55*/
!        decide how many photons to make.                                       !/*ag         56*/
   95 gp=wgt*gp                                                                 !/*ag4c       11*/
      ni=1                                                                      !/*agxc        1*/
!        produce analog gammas for pulse height tally.                          !/*agxc        2*/
      if(abs(kf8).ge.3.and.dbcn(24).eq.zero)then                                !/*agxc        3*/
         t1=wgt                                                                 !/*agxc        4*/
         if(gp.lt.t1)go to 140                                                  !/*agxc        5*/
         ni=int((gp/t1)+rang())                                                 !/*agxc        6*/
         gp=ni*t1                                                               !/*agxc        7*/
         go to 150                                                              !/*agxc        8*/
      endif                                                                     !/*agxc        9*/
!        get target weight, t1, for dxtran, importances, or windows.            !/*agxc       10*/
      if(idx.eq.0)go to 100                                                     !/*ag         58*/
      t1=max(dxw(1,3),-dxw(1,3)*wgt9(1))                                        !/*ag         59*/
      go to 130                                                                 !/*ag         60*/
  100 if(nww(2).ne.0)go to 110                                                  !/*ag         61*/
      t1=max(gwt(lgwt+icl),-gwt(lgwt+icl)*wgt9(1))*fiml9(1,1)/fiml(1)           !/*ag         62*/
      go to 130                                                                 !/*ag         63*/
  110 t1=huge                                                                   !/*ag         64*/
      do 120 i=1,nww(2)                                                         !/*ag         65*/
      t=wwval(2,icl,i,1,jx)                                                     !/*ag4c2       1*/
      if(t.le.0.)t1=min(t1,gp)                                                  !/*ag         67*/
  120 if(t.gt.0.)t1=min(t,t1)                                                   !/*ag         68*/
!        determine split number, ni, or roulette.                               !/*agxc       11*/
  130 if(gp.lt.t1)go to 140                                                     !/*agxc       12*/
      if(t1.ne.0.)ni=min(10,int(gp/(5.*t1)+1.))                                 !/*ag         71*/
      if(rtc(krtc+11,iex).ge.0.)gp=gp/tz                                        !/*ag4c       12*/
      go to 150                                                                 !/*ag         72*/
  140 if(gp.le.t1*rang())return                                                 !/*ag         73*/
      gp=t1                                                                     !/*ag         74*/
      if(rtc(krtc+11,iex).ge.0.)gp=gp/tz                                        !/*ag4c       13*/
!                                                                               !/*ag         75*/
!        generate photons and write them to the bank.                           !/*ag         76*/
  150 call savpar(0,0)                                                          !/*ag4c2       2*/
      npa=1                                                                     !/*ag         82*/
      ipt=2                                                                     !/*ag         83*/
      jan=0                                                                     !/*agxc       13*/
      gp=gp/ni                                                                  !/*ag         84*/
      es=erg                                                                    !/*ag         85*/
      vel=slite                                                                 !/*ag         86*/
      ncp=0                                                                     !/*ag         87*/
#ifdef MESHTAL                                                                  /*!agx         6*/
      mspf=2                                                                    !/*agx         7*/
#endif                                                                          /*!agx         8*/
      idx=0                                                                     !/*ag         88*/
      do 180 i=1,ndx(2)                                                         !/*ag         89*/
  180 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+(zzz-dxx(2,3,i))**2.lt.        !/*ag         90*/
     & dxx(2,5,i))idx=i                                                         !/*ag         91*/
      st=totm                                                                   !/*ag         92*/
      do 450 k=1,ni                                                             !/*ag         93*/
      wgt=gp                                                                    !/*ag         94*/
      ts=-1.                                                                    !/*ag4c       14*/
!                                                                               !/*ag         95*/
!        sample from full photon-production data, if any.                       !/*ag         96*/
      ipsc=102                                                                  !/*ag         97*/
      if(jxs(ljxs+13,iex).eq.0)go to 380                                        !/*ag         98*/
!                                                                               !/*ag         99*/
!        sample the reaction that produced the photon.                          !/*ag        100*/
      ixre=1                                                                    !/*ag        101*/
      if(nxs(lnxs+6,iex).eq.1.and.rtc(krtc+11,iex).lt.0.)go to 350              !/*ag4c       15*/
      r=0.                                                                      !/*ag        103*/
      if(nt.ne.0)go to 190                                                      !/*ag        104*/
      l=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*ag4b        9*/
      r=rang()*(xss(l-1)+rtc(krtc+1,iex)*(xss(l)-xss(l-1)))                     !/*ag4b       10*/
      s=0.                                                                      !/*ag        107*/
      tt=0.                                                                     !/*ag        108*/
  190 md=0                                                                      !/*ag        109*/
      do 290 ixre=1,nxs(lnxs+6,iex)                                             !/*ag        110*/
      ts=-1.                                                                    !/*ag4c       16*/
      if(nt.eq.0)go to 220                                                      !/*ag        111*/
      do 200 ik=1,nt                                                            !/*ag        112*/
  200 if(ixre.eq.nint(xss(jxs(ljxs+29,iex)+2*ik-2)))go to 210                   !/*agx         9*/
      go to 290                                                                 !/*ag        114*/
  210 pik(kpik+ik)=0.                                                           !/*ag        115*/
  220 l=jxs(ljxs+15,iex)+nint(xss(jxs(ljxs+14,iex)+ixre-1))+1                   !/*ag4c       18*/
      if(nint(xss(l-2)).eq.13)go to 250                                         !/*ag4c       19*/
!                                                                               !/*ag        118*/
!        mf=12 -- partial cross section = yield * neutron cross section.        !/*ag        119*/
      do 230 jy=1,2                                                             !/*ag        120*/
  230 yd(jy)=acefcn(l,xss(jxs(ljxs+1,iex)+ktc(kktc+1,iex)+jy-2)*f,ln)           !/*ag4b       11*/
      if(yd(1).eq.0..and.yd(2).eq.0.)go to 290                                  !/*ag        122*/
      ix=nint(xss(l-1))                                                         !/*ag4c       20*/
      if(ix.eq.md)go to 240                                                     !/*ag        124*/
      md=ix                                                                     !/*ag        125*/
      x1=0.                                                                     !/*ag        126*/
      x2=0.                                                                     !/*ag        127*/
      is=jxs(ljxs+21,iex)+1                                                     !/*ag        128*/
      if(ix.gt.0)is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))             !/*ag4c       21*/
      ic=min(ktc(kktc+1,iex)-nint(xss(is-1))+1,nint(xss(is)))                   !/*ag4c       22*/
      if(ic.gt.0)x1=xss(is+ic)                                                  !/*ag        131*/
      ic=min(ic+1,nint(xss(is)))                                                !/*ag4c       23*/
      if(ic.gt.0)x2=xss(is+ic)                                                  !/*ag        133*/
  240 t=x1*yd(1)+(x2*yd(2)-x1*yd(1))*rtc(krtc+1,iex)                            !/*ag4b       13*/
      if(t.eq.0.)go to 290                                                      !/*ag4c       24*/
!                                                                               !/*ag4c       25*/
!        compute change in photon production due to change in capture           !/*ag4c       26*/
!        or fission when using unresolved tables rather than smooth.            !/*ag4c       27*/
!        multiply photon weight by {partial-prob-table/partial-smooth}*         !/*ag4c       28*/
!        {total-smooth/total-prob-table} for unbiased.                          !/*ag4c       29*/
      if(rtc(krtc+11,iex).lt.0.)go to 270                                       !/*ag4c       30*/
      mt=nint(xss(jxs(ljxs+13,iex)+ixre-1))/1000                                !/*ag4c       31*/
      if(mt.eq.18.or.mt.eq.19)tf=rtc(krtc+12,iex)/                              !/*ag4c       32*/
     & (x1+(x2-x1)*rtc(krtc+1,iex))                                             !/*ag4c       33*/
      if(mt.eq.18.or.mt.eq.19)ts=tf                                             !/*ag4c       34*/
      if(mt.eq.102)tc=rtc(krtc+14,iex)/(x1+(x2-x1)*rtc(krtc+1,iex))             !/*ag4c       35*/
      if(mt.eq.102)ts=tc                                                        !/*ag4c       36*/
      go to 270                                                                 !/*ag        135*/
!                                                                               !/*ag        136*/
!        mf=13 -- partial cross section is direct from table.                   !/*ag        137*/
  250 ic=ktc(kktc+1,iex)-nint(xss(l-1))+1                                       !/*ag4c       37*/
      if(ic.lt.1)go to 290                                                      !/*ag        139*/
      if(rtc(krtc+1,iex).ne.0.)go to 260                                        !/*ag4b       15*/
      if(ic.gt.nint(xss(l)))go to 290                                           !/*ag4c       38*/
      t=xss(ic+l)                                                               !/*ag        142*/
      go to 270                                                                 !/*ag        143*/
  260 if(ic.ge.nint(xss(l)))go to 290                                           !/*ag4c       39*/
      if(rtc(krtc+11,iex).ge.0.)call expirx(0,'acegam',                         !/*agxk        1*/
     & 'mf=13 photon prod not allowed with unresolved prob tables.')            !/*ag4c       41*/
      t=xss(ic+l)+rtc(krtc+1,iex)*(xss(ic+l+1)-xss(ic+l))                       !/*ag4b       16*/
  270 if(t.eq.0.)go to 290                                                      !/*ag        146*/
      if(nt.eq.0)go to 280                                                      !/*ag        147*/
      pik(kpik+ik)=t*xss(jxs(ljxs+29,iex)+ik*2-1)                               !/*agx        10*/
      r=r+pik(kpik+ik)                                                          !/*ag        149*/
      go to 290                                                                 !/*ag        150*/
  280 if(t.gt.tt)kx=ixre                                                        !/*ag        151*/
      tt=max(t,tt)                                                              !/*ag        152*/
      s=s+t                                                                     !/*ag        153*/
      if(s.gt.r)go to 350                                                       !/*ag        154*/
  290 continue                                                                  !/*ag        155*/
      if(nt.eq.0)go to 320                                                      !/*ag        156*/
      if(r.le.0.)go to 450                                                      !/*ag        157*/
      ik=1                                                                      !/*ag        158*/
      if(nt.eq.1)go to 310                                                      !/*ag        159*/
      t=rang()*r                                                                !/*ag        160*/
      do 300 ik=1,nt                                                            !/*ag        161*/
      t=t-pik(kpik+ik)                                                          !/*ag        162*/
  300 if(t.lt.0.)go to 310                                                      !/*ag        163*/
      call expirx(1,'acegam','no photon mt selected.')                          !/*ag        164*/
      return                                                                    !/*ag        165*/
!                                                                               !/*agx        11*/
  310 ixre=nint(xss(jxs(ljxs+29,iex)+ik*2-2))                                   !/*agx        12*/
      wgt=wgt*r/(tgp(ktgp+iex)*xss(jxs(ljxs+29,iex)+ik*2-1))                    !/*agx        13*/
      if(rtc(krtc+11,iex).lt.0.)go to 350                                       !/*ag4c       43*/
      mt=nint(xss(jxs(ljxs+13,iex)+ixre-1))/1000                                !/*ag4c       44*/
      if(mt.eq.18.or.mt.eq.19)ts=tf                                             !/*ag4c       45*/
      if(mt.eq.102)ts=tc                                                        !/*ag4c       46*/
      go to 350                                                                 !/*ag        168*/
!                                                                               !/*ag        169*/
!        handle failure of the reaction cross sections to add up.               !/*ag        170*/
  320 call zaid(2,ht,ixl(lixl+1,iex))                                           !/*ag        171*/
      call errprn(1,nppm,4,erg,zero,'erg',' ',                                  !/*ag4b       17*/
     & 'no photon-production mt found in acegam. zaid = '//ht)                  !/*ag4a        4*/
      ixre=kx                                                                   !/*ag        182*/
!                                                                               !/*ag        183*/
!        sample the energy and direction of the photon.                         !/*ag        184*/
  350 erg=es                                                                    !/*ag        185*/
      if(ts.ge.0.)wgt=wgt*ts                                                    !/*ag4c       47*/
      mtp=nint(xss(jxs(ljxs+13,iex)+ixre-1))                                    !/*ag4c       48*/
      ia=jxs(ljxs+17,iex)                                                       !/*ag4c       49*/
      ka=0                                                                      !/*ag4c       50*/
      if(jxs(ljxs+16,iex).ne.0)ka=nint(xss(jxs(ljxs+16,iex)+ixre-1))            !/*ag4c       51*/
      id=jxs(ljxs+19,iex)                                                       !/*ag4c       52*/
      kd=nint(xss(jxs(ljxs+18,iex)+ixre-1))                                     !/*ag4c       53*/
      call acecas(1,1,zero,ia,ka,id,kd)                                         !/*ag4c       54*/
      if(kdb.ne.0)return                                                        !/*ag        187*/
      if(dbcn(24).eq.zero.and.abs(kf8).eq.3.and.wgt.lt.gp)then                  !/*agxc       14*/
         if(wgt.lt.rang()*gp)go to 450                                          !/*agxc       15*/
         wgt=gp                                                                 !/*agxc       16*/
      endif                                                                     !/*agxc       17*/
      if(ixcos.ne.0)ipsc=8                                                      !/*ag4c       55*/
!                                                                               !/*ag4c       56*/
!        if photon energy below cell cutoff, ignore it.                         !/*ag4c       57*/
      if(colout(1,1).lt.elc(2))go to 450                                        !/*ag4c       58*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*ag4b       18*/
      erg=colout(1,1)                                                           !/*ag        190*/
      if(nint(xss(jxs(ljxs+13,iex)+ixre-1)).lt.18000)go to 390                  !/*ag4c       59*/
      if(nint(xss(jxs(ljxs+13,iex)+ixre-1)).gt.19999)go to 390                  !/*ag4c       60*/
      if(lfcl(llfc+icl).eq.2)go to 450                                          !/*ag        193*/
      if(nsr.ne.71)go to 390                                                    !/*ag        194*/
      if(kcy.le.ikz)go to 390                                                   !/*ag4a        5*/
      do 360 j=1+mjss,mjss+nilw                                                 !/*ag        196*/
  360 if(jss(ljss+j).eq.ncl(lncl+icl))go to 370                                 !/*ag        197*/
      go to 390                                                                 !/*ag        198*/
  370 call sufwrt(j,zero)                                                       !/*ag        199*/
      go to 390                                                                 !/*ag        200*/
!                                                                               !/*ag        201*/
!        otherwise sample from equi-probable energies table.                    !/*ag        202*/
  380 if(it.eq.20)erg=xss(lg+20)                                                !/*ag        203*/
      if(it.ne.20)erg=xss(lg+it+int(rang()*(21-it)))                            !/*ag        204*/
      if(jxs(ljxs+23,iex).ne.0.and.rtc(krtc+11,iex).ge.0.)                      !/*ag4c       61*/
     & call expirx(0,'acegam',                                                  !/*agxk        2*/
     & 'old photon prod not allowed with unresolved prob tables.')              !/*ag4c       63*/
      call isos(uuu,lev)                                                        !/*ag4b       19*/
!                                                                               !/*ag        206*/
!        russian roulette photons below weight window.                          !/*ag        207*/
  390 if(nww(2).le.1.or.idx.ne.0)go to 420                                      !/*ag        208*/
      t2=erg                                                                    !/*ag4c       64*/
      if(wwp(ipt,6).ne.0.)t2=tme                                                !/*ag4c       65*/
      do 400 i=1,nww(2)                                                         !/*ag        209*/
  400 if(t2.lt.wwe(lwwe+i+mww(2)))go to 410                                     !/*ag4c       66*/
  410 t=wwval(2,icl,i,1,jx)                                                     !/*ag4c2       3*/
      if(wgt.ge.t)go to 420                                                     !/*ag        212*/
      if(wgt.lt.rang()*t)go to 450                                              !/*ag        213*/
      wgt=t                                                                     !/*ag        214*/
!                                                                               !/*ag        215*/
!        store photon-production tally information.                             !/*ag        216*/
  420 call gain_pax (igain_photon_from_neutrons, 2, 1,  wgt, wgt*erg)           !/*agx        14*/
      if(nmaz.ne.0)call rslmaz(1,1)                                             !/*ag4b       21*/
      call contrib_pwb (2,ipwb_photon_from_neutrons,icl,wgt)                    !/*agx        15*/
      pan(kpan+1,6,kp)=pan(kpan+1,6,kp)+1.                                      !/*ag4c2       4*/
      pan(kpan+1,7,kp)=pan(kpan+1,7,kp)+wgt                                     !/*ag4c2       5*/
      pan(kpan+1,8,kp)=pan(kpan+1,8,kp)+wgt*erg                                 !/*ag4c2       6*/
      pcc(kpcc+1,icl)=pcc(kpcc+1,icl)+1.                                        !/*ag        224*/
      pcc(kpcc+2,icl)=pcc(kpcc+2,icl)+wgt                                       !/*ag        225*/
      pcc(kpcc+3,icl)=pcc(kpcc+3,icl)+wgt*erg                                   !/*ag        226*/
      do 430 i=1,15                                                             !/*ag        227*/
  430 if(erg.gt.ebl(lebl+i))go to 440                                           !/*ag4b       22*/
  440 febl(kfeb+1,i)=febl(kfeb+1,i)+1.                                          !/*ag4b       23*/
      febl(kfeb+2,i)=febl(kfeb+2,i)+wgt                                         !/*ag4b       24*/
!                                                                               !/*ag        231*/
!        tally detectors, create dxtran particles, and bank the photon.         !/*ag        232*/
      if(locph(llph+icl).ne.0)call talph(icl,erg)                               !/*cor4-1    149*/
      if(kdb.ne.0)return                                                        !/*cor4-1    150*/
      if(ndet(2).ne.0)call tallyd                                               !/*ag        233*/
      if(kdb.ne.0)return                                                        !/*ag        234*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*ag        235*/
      if(kdb.ne.0)return                                                        !/*ag        236*/
#ifdef SPABI                                                                    /*!agx        16*/
      if (is_spabi.gt.0) then                                                   !/*agxa        1*/
         call sec_par_bias(2,1,erg,igain_photon_from_neutrons,                  !/*agx        18*/
     &   ipwb_photon_from_neutrons,kp,8)                                        !/*agx        19*/
         go to 450                                                              !/*agx        20*/
      endif                                                                     !/*agx        21*/
#endif                                                                          /*!agx        22*/
      call bankit(8)                                                            !/*ag4a        6*/
  450 if(kdb.ne.0)return                                                        !/*ag        238*/
!                                                                               !/*ag        239*/
      totm=st                                                                   !/*ag        240*/
      call getpar(0,0)                                                          !/*ag4c2       7*/
      if(npikmt.ne.0)iex=io                                                     !/*ag4a        7*/
      return                                                                    !/*ag        249*/
      end                                                                       !/*ag        250*/

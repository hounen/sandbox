!#ident cp_gam.F from deck xcg                                                  !/*xcg         1*/
      subroutine cp_gam                                                         !/*xcg         2*/
!        generate and bank photons from other particle collision.               !/*xcg         3*/
!        routine is very similar to acegam                                      !/*xcg         4*/
      use global_data                                                           !/*xcg         5*/
!                                                                               !/*xcg         6*/
      implicit real(kindr) (a-h,o-z)                                            !/*xcg         7*/
      implicit integer(kindi) (i-n)                                             !/*xcg         8*/
!                                                                               !/*xcg         9*/
      character ht*10                                                           !/*xcg        10*/
      dimension yd(2)                                                           !/*xcg        11*/
      data f/1.00000000001d0/                                                   !/*xcg        12*/
!                                                                               !/*xcg        13*/
!     New local variable ipt_incident                                           !/*xcg        14*/
      ipt_incident=ipt                                                          !/*xcg        15*/
! ___ QUESTION: Does this routine apply to incident antiparticles?              !/*xcgc        1*/
! ___           If so, then the next line is wrong.                             !/*xcgc        2*/
      jan = not_self_anti(ipt)                                                  !/*xcgc        3*/
!                                                                               !/*xcg        16*/
      kp=mpan                                                                   !/*xcg        17*/
!                                                                               !/*xcg        18*/
!        calculate the probability of production of a photon.                   !/*xcg        19*/
      l=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*xcg        20*/
      gp=(xss(l-1)+rtc(krtc+1,iex)*(xss(l)-xss(l-1)))/rtc(krtc+5,iex)           !/*xcg        21*/
      if(gp.eq.0.)return                                                        !/*xcg        22*/
!                                                                               !/*xcg        23*/
!     Decide how many photons to make.                                          !/*xcg        24*/
!                                                                               !/*xcg        25*/
!     Next stuff commented out -- don't know if need to keep                    !/*xcg        26*/
!     for generality or not                                                     !/*xcg        27*/
!     if(idx.eq.0)go to 100                                                     !/*xcg        28*/
!     t1=max(dxw(1,3),-dxw(1,3)*wgt9(1))                                        !/*xcg        29*/
!     go to 130                                                                 !/*xcg        30*/
! 100 if(nww(2).ne.0)go to 110                                                  !/*xcg        31*/
!     t1=max(gwt(lgwt+icl),-gwt(lgwt+icl)*wgt9(1))*fiml9(1,1)/fiml(1)           !/*xcg        32*/
!     go to 130                                                                 !/*xcg        33*/
! 110 t1=huge                                                                   !/*xcg        34*/
!     do 120 i=1,nww(2)                                                         !/*xcg        35*/
!     t=wwval(2,icl,i,1)                                                        !/*xcg        36*/
!     if(t.le.0.)t1=min(t1,gp)                                                  !/*xcg        37*/
! 120 if(t.gt.0.)t1=min(t,t1)                                                   !/*xcg        38*/
! 130 ni=1                                                                      !/*xcg        39*/
!     if(gp.lt.t1)go to 140                                                     !/*xcg        40*/
!     if(t1.ne.0.)ni=min(10,int(gp/(5.*t1)+1.))                                 !/*xcg        41*/
!     go to 150                                                                 !/*xcg        42*/
! 140 if(gp.le.t1*rang())return                                                 !/*xcg        43*/
!         produce 'ni' secondary photons.  sample ni                            !/*xcg        44*/
!         from nearest two integers to gp.  weight of each particle is          !/*xcg        45*/
!         simply the incident particle weight.                                  !/*xcg        46*/
      ni=gp+rang()                                                              !/*xcg        47*/
!                                                                               !/*xcg        48*/
!     Temporary write                                                           !/*xcg        49*/
!                                                                               !/*xcg        50*/
!     if(ni.gt.0) write(*,11) ni                                                !/*xcg        51*/
!  11 format("  producing ",i1," sec photons")                                  !/*xcg        52*/
      if(ni.eq.0) return                                                        !/*xcg        53*/
!                                                                               !/*xcg        54*/
!        generate photons and write them to the bank.                           !/*xcg        55*/
  150 call savpar(0,0)                                                          !/*xcg        56*/
      npa=1                                                                     !/*xcg        57*/
      ipt=2                                                                     !/*xcg        58*/
      jan=0                                                                     !/*xcgc        4*/
      es=erg                                                                    !/*xcg        59*/
      vel=slite                                                                 !/*xcg        60*/
      ncp=0                                                                     !/*xcg        61*/
#ifdef MESHTAL                                                                  /*!xcg        62*/
      mspf=2                                                                    !/*xcg        63*/
#endif                                                                          /*!xcg        64*/
      idx=0                                                                     !/*xcg        65*/
!     do 180 i=1,ndx(2)                                                         !/*xcg        66*/
! 180 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+(zzz-dxx(2,3,i))**2.lt.        !/*xcg        67*/
!    & dxx(2,5,i))idx=i                                                         !/*xcg        68*/
      st=totm                                                                   !/*xcg        69*/
      do 450 k=1,ni                                                             !/*xcg        70*/
!                                                                               !/*xcg        71*/
!        sample from full photon-production data.                               !/*xcg        72*/
      ipsc=102                                                                  !/*xcg        73*/
!                                                                               !/*xcg        74*/
!        sample the reaction that produced the photon.                          !/*xcg        75*/
      ixre=1                                                                    !/*xcg        76*/
      if(nxs(lnxs+6,iex).eq.1)go to 350                                         !/*xcg        77*/
      r=0.                                                                      !/*xcg        78*/
      l=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*xcg        79*/
      r=rang()*(xss(l-1)+rtc(krtc+1,iex)*(xss(l)-xss(l-1)))                     !/*xcg        80*/
      s=0.                                                                      !/*xcg        81*/
      tt=0.                                                                     !/*xcg        82*/
      md=0                                                                      !/*xcg        83*/
      do 290 ixre=1,nxs(lnxs+6,iex)                                             !/*xcg        84*/
      l=jxs(ljxs+15,iex)+xss(jxs(ljxs+14,iex)+ixre-1)+1                         !/*xcg        85*/
      if(xss(l-2).eq.13.)go to 250                                              !/*xcg        86*/
!                                                                               !/*xcg        87*/
!        mf=12 -- partial cross section = yield * neutron cross section.        !/*xcg        88*/
      do 230 jy=1,2                                                             !/*xcg        89*/
  230 yd(jy)=acefcn(l,xss(jxs(ljxs+1,iex)+ktc(kktc+1,iex)+jy-2)*f,ln)           !/*xcg        90*/
      if(yd(1).eq.0..and.yd(2).eq.0.)go to 290                                  !/*xcg        91*/
      ix=xss(l-1)                                                               !/*xcg        92*/
      if(ix.eq.md)go to 240                                                     !/*xcg        93*/
      md=ix                                                                     !/*xcg        94*/
      x1=0.                                                                     !/*xcg        95*/
      x2=0.                                                                     !/*xcg        96*/
      is=jxs(ljxs+21,iex)+1                                                     !/*xcg        97*/
      if(ix.gt.0)is=jxs(ljxs+7,iex)+xss(jxs(ljxs+6,iex)+ix-1)                   !/*xcg        98*/
      ic=min(ktc(kktc+1,iex)-xss(is-1)+1,xss(is))                               !/*xcg        99*/
      if(ic.gt.0)x1=xss(is+ic)                                                  !/*xcg       100*/
      ic=min(ic+1,int(xss(is)))                                                 !/*xcg       101*/
      if(ic.gt.0)x2=xss(is+ic)                                                  !/*xcg       102*/
  240 t=x1*yd(1)+(x2*yd(2)-x1*yd(1))*rtc(krtc+1,iex)                            !/*xcg       103*/
      go to 270                                                                 !/*xcg       104*/
!                                                                               !/*xcg       105*/
!        mf=13 -- partial cross section is direct from table.                   !/*xcg       106*/
  250 ic=ktc(kktc+1,iex)-xss(l-1)+1                                             !/*xcg       107*/
      if(ic.lt.1)go to 290                                                      !/*xcg       108*/
      if(rtc(krtc+1,iex).ne.0.)go to 260                                        !/*xcg       109*/
      if(ic.gt.xss(l))go to 290                                                 !/*xcg       110*/
      t=xss(ic+l)                                                               !/*xcg       111*/
      go to 270                                                                 !/*xcg       112*/
  260 if(ic.ge.xss(l))go to 290                                                 !/*xcg       113*/
      t=xss(ic+l)+rtc(krtc+1,iex)*(xss(ic+l+1)-xss(ic+l))                       !/*xcg       114*/
  270 if(t.eq.0.)go to 290                                                      !/*xcg       115*/
      if(t.gt.tt)kx=ixre                                                        !/*xcg       116*/
      tt=max(t,tt)                                                              !/*xcg       117*/
      s=s+t                                                                     !/*xcg       118*/
      if(s.gt.r)go to 350                                                       !/*xcg       119*/
  290 continue                                                                  !/*xcg       120*/
!                                                                               !/*xcg       121*/
!        handle failure of the reaction cross sections to add up.               !/*xcg       122*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*xcg       123*/
      call errprn(1,nppm,4,erg,zero,'erg',' ',                                  !/*xcg       124*/
     & 'no photon-production mt found in cp_gam. zaid = '//ht)                  !/*xcg       125*/
      ixre=kx                                                                   !/*xcg       126*/
!                                                                               !/*xcg       127*/
!        sample the energy and direction of the photon.                         !/*xcg       128*/
  350 erg=es                                                                    !/*xcg       129*/
      mtp=nint(xss(jxs(ljxs+13,iex)+ixre-1))                                    !/*xcg       130*/
      ia=jxs(ljxs+17,iex)                                                       !/*xcg       131*/
      if(jxs(ljxs+16,iex).ne.0)then                                             !/*xcg       132*/
        ka=nint(xss(jxs(ljxs+16,iex)+ixre-1))                                   !/*xcg       133*/
      else                                                                      !/*xcg       134*/
        ka=0                                                                    !/*xcg       135*/
      endif                                                                     !/*xcg       136*/
      id=jxs(ljxs+19,iex)                                                       !/*xcg       137*/
      kd=nint(xss(jxs(ljxs+18,iex)+ixre-1))                                     !/*xcg       138*/
      call acecas(1,ipt_incident,zero,ia,ka,id,kd)                              !/*xcg       139*/
      if(kdb.ne.0)return                                                        !/*xcg       140*/
      if(colout(1,1).eq.-huge)go to 450                                         !/*xcg       141*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*xcg       142*/
      erg=colout(1,1)                                                           !/*xcg       143*/
!                                                                               !/*xcg       144*/
!        russian roulette photons below weight window.                          !/*xcg       145*/
!     if(nww(2).le.1.or.idx.ne.0)go to 420                                      !/*xcg       146*/
!     do 400 i=1,nww(2)                                                         !/*xcg       147*/
! 400 if(erg.lt.wwe(lwwe+i+mww(2)))go to 410                                    !/*xcg       148*/
! 410 t=wwval(2,icl,i,1)                                                        !/*xcg       149*/
!     if(wgt.ge.t)go to 420                                                     !/*xcg       150*/
!     if(wgt.lt.rang()*t)go to 450                                              !/*xcg       151*/
!     wgt=t                                                                     !/*xcg       152*/
!                                                                               !/*xcg       153*/
!     Store photon-production tally information.                                !/*xcg       154*/
  420 call gain_pax (igain_photon_tabular_sampling, 2, 1, wgt, wgt*erg)         !/*xcg       155*/
      call contrib_pwb (2,ipwb_photon_tabular_sampl,icl,wgt)                    !/*xcg       156*/
!                                                                               !/*xcg       157*/
!        TEMPORARY expedient:  Return later to the question of rslmaz,          !/*xcg       158*/
!                              and pan and pcc contributions.                   !/*xcg       159*/
!     if(nmaz.ne.0)call rslmaz(1,1)                                             !/*xcg       160*/
!     pan(kpan+2,4,kp)=pan(kpan+2,4,kp)+1.                                      !/*xcg       161*/
!     pan(kpan+2,5,kp)=pan(kpan+2,5,kp)+wgt                                     !/*xcg       162*/
!     pan(kpan+2,6,kp)=pan(kpan+2,6,kp)+wgt*erg                                 !/*xcg       163*/
!     pcc(kpcc+1,icl)=pcc(kpcc+1,icl)+1.                                        !/*xcg       164*/
!     pcc(kpcc+2,icl)=pcc(kpcc+2,icl)+wgt                                       !/*xcg       165*/
!     pcc(kpcc+3,icl)=pcc(kpcc+3,icl)+wgt*erg                                   !/*xcg       166*/
!     do 430 i=1,15                                                             !/*xcg       167*/
! 430 if(erg.gt.ebl(lebl+i))go to 440                                           !/*xcg       168*/
! 440 febl(kfeb+1,i)=febl(kfeb+1,i)+1.                                          !/*xcg       169*/
!     febl(kfeb+2,i)=febl(kfeb+2,i)+wgt                                         !/*xcg       170*/
!                                                                               !/*xcg       171*/
!        tally detectors, create dxtran particles, and bank the photon.         !/*xcg       172*/
!     if(locph(llph+icl).ne.0)call talph(icl,erg)                               !/*xcg       173*/
!     if(kdb.ne.0)return                                                        !/*xcg       174*/
!     if(ndet(2).ne.0)call tallyd                                               !/*xcg       175*/
!     if(kdb.ne.0)return                                                        !/*xcg       176*/
!     if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*xcg       177*/
!     if(kdb.ne.0)return                                                        !/*xcg       178*/
      call bankit(8)                                                            !/*xcg       179*/
  450 if(kdb.ne.0)return                                                        !/*xcg       180*/
!                                                                               !/*xcg       181*/
      totm=st                                                                   !/*xcg       182*/
      call getpar(0,0)                                                          !/*xcg       183*/
      ipt=ipt_incident                                                          !/*xcg       184*/
      jan = not_self_anti(ipt)                                                  !/*xcgc        5*/
!                                                                               !/*xcg       185*/
!     Next line should not be needed after common                               !/*xcg       186*/
!     block transfers are un-commented in mcnpx                                 !/*xcg       187*/
      erg=es                                                                    !/*xcg       188*/
      return                                                                    !/*xcg       189*/
      end                                                                       !/*xcg       190*/

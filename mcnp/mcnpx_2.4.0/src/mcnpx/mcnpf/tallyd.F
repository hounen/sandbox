!#ident tallyd.F from deck td                                                   !/*td          1*/
      subroutine tallyd                                                         !/*td          2*/
!        tally contributions to detectors.                                      !/*td          3*/
      use global_data                                                           !/*tdx         1*/
!                                                                               !/*tdx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tdx         3*/
      implicit integer(kindi) (i-n)                                             !/*tdx         4*/
!                                                                               !/*tdx         5*/
#ifdef MESHTAL                                                                  /*!tdx         6*/
      dimension zd(10),jz(5)                                                    !/*tdx         7*/
#endif                                                                          /*!tdx         8*/
      dimension ik(8)                                                           !/*td          5*/
!                                                                               !/*td          6*/
!        return if kcode problem is not settled.                                !/*td          7*/
      if(kc8.gt.0)return                                                        !/*td4b        1*/
      li=lipt                                                                   !/*td          9*/
      lj=ljpt                                                                   !/*td         10*/
#ifdef RADIOG                                                                   /*!tdx         9*/
      d2=zero                                                                   !/*tdx        10*/
      ar=one                                                                    !/*tdx        11*/
      if(notrn.ne.0)amfp=zero                                                   !/*tdx        12*/
#endif                                                                          /*!tdx        13*/
!                                                                               !/*td         11*/
!        loop over all neutron or all photon detectors.                         !/*td         12*/
      ital=0                                                                    !/*td         13*/
      wf=0.                                                                     !/*td         14*/
      m1=(ipt-1)*ndet(1)                                                        !/*td4c        1*/
      nb=npb+1                                                                  !/*td4c2       1*/
      do 625 idet=m1+1,m1+ndet(ipt)                                             !/*tdx        14*/
#ifdef RADIOG                                                                   /*!tdx        15*/
      iz=0                                                                      !/*tdx        16*/
      nc=0                                                                      !/*tdx        17*/
      mg=1                                                                      !/*tdx        18*/
#endif                                                                          /*!tdx        19*/
      lg=mxa*(idet-1)+icl                                                       !/*td         18*/
      dec(kdec+3,lg)=dec(kdec+3,lg)+1.                                          !/*td         19*/
!                                                                               !/*td         25*/
!        set up for detector tally ital if not done already.                    !/*td         26*/
      tw=wgt                                                                    !/*td4c        3*/
      if(locdt(1,idet).ne.ital)go to 5                                          !/*td4c        4*/
#ifdef RADIOG                                                                   /*!tdx        20*/
      if(iptal(li+2,2,ital).eq.0)nc=2                                           !/*tdx        21*/
      if(i1.eq.0.and.idrc(idet).le.0)nc=1                                       !/*tdx        22*/
      if(nc.gt.0)go to 6                                                        !/*tdx        23*/
#else                                                                           /*!tdx        24*/
      if(iptal(li+2,2,ital).eq.0)go to 150                                      !/*td4c        5*/
      if(i1.eq.0.and.idrc(idet).le.0)go to 10                                   !/*td4c        6*/
#endif                                                                          /*!tdx        25*/
      if(i1.gt.0)tw=drc(kdrc+11,i1)                                             !/*td4c        7*/
      if(idrc(idet).gt.0)tw=drc(kdrc+11,idrc(idet))                             !/*td4c        8*/
      drc(kdrc+11,idet)=tw                                                      !/*td4c        9*/
#ifdef RADIOG                                                                   /*!tdx        26*/
      nc=2                                                                      !/*tdx        27*/
      go to 6                                                                   !/*tdx        28*/
#else                                                                           /*!tdx        29*/
      go to 150                                                                 !/*td4c       10*/
#endif                                                                          /*!tdx        30*/
!                                                                               !/*td4c       11*/
!        first detector in a new detector tally.                                !/*td4c       12*/
    5 ital=locdt(1,idet)                                                        !/*td4c       13*/
#ifdef RADIOG                                                                   /*!tdx        31*/
!        radiography tally if jptal(8,ital)=4 and                               !/*tdx        32*/
!        mg = (segment bins) * (cosine bins) > 1                                !/*tdx        33*/
      mg=1                                                                      !/*tdx        34*/
      if(jptal(lj+8,ital).eq.4)                                                 !/*tdx        35*/
     & mg=max(1,(iptal(li+4,4,ital)-1)*(iptal(li+6,4,ital)-1))                  !/*tdx        36*/
!        iptal(2,2,ital) points to pd card entries.                             !/*tdx        37*/
      if(iptal(li+2,2,ital).eq.0)go to 6                                        !/*tdx        38*/
      l=iptal(li+2,2,ital)+icl                                                  !/*tdx        39*/
      if(tds(l).ge.1.)go to 6                                                   !/*tdx        40*/
      if(tds(l).eq.0.)then                                                      !/*tdx        41*/
         mg=1                                                                   !/*tdx        42*/
         if(abs(dbcn(28)).gt.2.)rn=rang()                                       !/*tdx        43*/
      else                                                                      !/*tdx        44*/
         rn=rang()                                                              !/*tdx        45*/
         if(rn.ge.tds(l))mg=1                                                   !/*tdx        46*/
      endif                                                                     !/*tdx        47*/
!                                                                               !/*tdx        48*/
!       Make a contribution to all mg grid points at the image                  !/*tdx        49*/
!       plane for every source or scatter event.                                !/*tdx        50*/
    6 if(ipsc.ne.3.and.ipsc.ne.10.and.ipsc.ne.11.and.                           !/*tdx        51*/
     & ipsc.ne.12.and.ipsc.ne.101)go to 8                                       !/*tdx        52*/
      it=locdt(1,idet)                                                          !/*tdx        53*/
      if(jptal(lj+8,it).eq.4.and.tds(iptal(li+1,1,it)+17).ne.0.)                !/*tdx        54*/
     & go to 625                                                                !/*tdx        55*/
    8 do 620 ng=1,mg                                                            !/*tdx        56*/
      if(nc.eq.1)go to 10                                                       !/*tdx        57*/
      if(nc.eq.2)go to 150                                                      !/*tdx        58*/
      if(ng.gt.1)go to 170                                                      !/*tdx        59*/
#endif                                                                          /*!tdx        60*/
      if(iptal(li+2,2,ital).eq.0)go to 20                                       !/*td4c       14*/
      if(idrc(idet).le.0)go to 10                                               !/*td4c       15*/
      i1=0                                                                      !/*td4c       16*/
      tw=drc(kdrc+11,idrc(idet))                                                !/*td4c       17*/
      go to 20                                                                  !/*td4c       18*/
!                                                                               !/*td         29*/
!        adjust the tally weight according to the cell probability.             !/*td         30*/
   10 i1=idet                                                                   !/*td4c       19*/
      drc(kdrc+11,idet)=0.                                                      !/*td4c       20*/
      l=iptal(li+2,2,ital)+icl                                                  !/*td4c       21*/
      if(tds(l).ge.1.)go to 19                                                  !/*td4c       22*/
      if(tds(l).eq.0.)go to 550                                                 !/*td4c       23*/
#ifdef RADIOG                                                                   /*!tdx        61*/
      if(rn.ge.tds(l))go to 550                                                 !/*tdx        62*/
#else                                                                           /*!tdx        63*/
      if(rang().ge.tds(l))go to 550                                             !/*td4c       24*/
#endif                                                                          /*!tdx        64*/
      tw=wgt/tds(l)                                                             !/*td4c       25*/
   19 drc(kdrc+11,idet)=tw                                                      !/*td4c       26*/
      if(idet.gt.1.and.locdt(1,max(1,idet-1)).eq.ital)go to 150                 !/*td4c       27*/
!                                                                               !/*td         39*/
!        set the base detector number and controls for the totals nest.         !/*td         40*/
   20 i0=idet                                                                   !/*td         41*/
      ju=iptal(li+3,3,ital)                                                     !/*td         42*/
      jm=iptal(li+5,3,ital)                                                     !/*td         43*/
      je=iptal(li+7,3,ital)                                                     !/*td         44*/
      kt=iptal(li+8,3,ital)                                                     !/*td         45*/
      mm=iptal(li+5,4,ital)-1                                                   !/*td         46*/
      do 30 i=3,8                                                               !/*td         47*/
   30 ik(i)=iptal(li+i,5,ital)                                                  !/*td         48*/
!                                                                               !/*td         49*/
!        set up for all detectors, if not done already.                         !/*td         50*/
      if(wf.gt.0.)go to 150                                                     !/*td         51*/
      te=erg                                                                    !/*td         52*/
      wf=1.                                                                     !/*td         53*/
!                                                                               !/*td         54*/
!        pre-processing for special cases.                                      !/*td         55*/
      go to(        110, 40, 70,110,110,110,110, 80,                            !/*td4c2       2*/
     &      110,110,110, 70, 70, 70,110,110)ipsc-2                              !/*td4c2       3*/
      go to(110,110,90,70,100,110)ipsc-100                                      !/*td4c2       4*/
      call expirx(1,'tallyd','illegal value for ipsc.')                         !/*td4c2       5*/
      return                                                                    !/*td4c2       6*/
!                                                                               !/*td         58*/
! >>>>>  ipsc=4 -- neutron from collision with moving target.                   !/*td         59*/
   40 wf=cmult                                                                  !/*td         60*/
      if(ntyn.ge.0)go to 50                                                     !/*td         61*/
      f=awn(lawn+iex)/ycn                                                       !/*td         62*/
      tpd(7)=f*ssr                                                              !/*td         63*/
      tpd(1)=tpd(7)                                                             !/*td         64*/
      if(ntyn.ne.-99)tpd(1)=(1.+awn(lawn+iex))*sqrt(ergace/eg0)                 !/*td         65*/
      tpd(2)=eg0/(1.+awn(lawn+iex))**2                                          !/*td         66*/
      tpd(4)=uold(1)+f*vtr(1)                                                   !/*td4b        3*/
      tpd(5)=uold(2)+f*vtr(2)                                                   !/*td4b        4*/
      tpd(6)=uold(3)+f*vtr(3)                                                   !/*td4b        5*/
      go to 60                                                                  !/*td         70*/
   50 g=sqrt(2.*ttn/awn(lawn+iex))                                              !/*td         71*/
      tpd(7)=g*ssr                                                              !/*td         72*/
      tpd(1)=sqrt(2.*ergace)                                                    !/*td         73*/
      tpd(2)=.5                                                                 !/*td         74*/
      tpd(4)=g*vtr(1)                                                           !/*td         75*/
      tpd(5)=g*vtr(2)                                                           !/*td         76*/
      tpd(6)=g*vtr(3)                                                           !/*td         77*/
   60 tpd(3)=tpd(4)**2+tpd(5)**2+tpd(6)**2                                      !/*td         78*/
      if(tpd(1)**2.ge.tpd(3))go to 110                                          !/*td         79*/
      wf=wf*2.                                                                  !/*td         80*/
      if(rang().gt..5)tpd(3)=-tpd(3)                                            !/*td         81*/
      go to 110                                                                 !/*td         82*/
!                                                                               !/*td         83*/
! >>>>>  ipsc=5 -- neutron from collision with stationary target and            !/*td         84*/
!        ipsc=14 -- neutron from kalbach-87 (law 44) collision and              !/*td4a        4*/
!        ipsc=15 -- neutron from law 67 (endf/b-vi law 7) collision and         !/*td4a        5*/
!        ipsc=16 -- neutron from law 61 (tabulated energy/angle) and            !/*td4c       29*/
!        ipsc=104 -- particle from isotropic multigroup collision.              !/*td         85*/
   70 wf=cmult                                                                  !/*td         86*/
      go to 110                                                                 !/*td         87*/
!                                                                               !/*td         88*/
! >>>>>  ipsc=10 -- particle from sdef source.                                  !/*td4a        6*/
   80 wf=cbwf                                                                   !/*td4b        6*/
      go to 110                                                                 !/*td         91*/
!                                                                               !/*td         92*/
! >>>>>  ipsc=103 -- photon from double fluorescence.                           !/*td         93*/
   90 if(rang().gt..5)te=xss(jxs(ljxs+4,iex)+3*nxs(lnxs+4,iex)+1)               !/*td         94*/
      wf=2.                                                                     !/*td         95*/
      go to 110                                                                 !/*td         96*/
!                                                                               !/*td         97*/
! >>>>>  ipsc=105 -- photon from pair production.                               !/*td         98*/
  100 wf=2.                                                                     !/*td         99*/
!                                                                               !/*td        100*/
  110 call savpar(0,2)                                                          !/*td4c2       7*/
!                                                                               !/*td        108*/
  150 if(tw.eq.0.)go to 550                                                     !/*td4c       30*/
#ifdef RADIOG                                                                   /*!tdx        65*/
      ibs=1                                                                     !/*tdx        66*/
      ibc=1                                                                     !/*tdx        67*/
      ld=iptal(li+1,1,ital)                                                     !/*tdx        68*/
      go to(152,152,154,154)jptal(lj+8,ital)                                    !/*tdx        69*/
  152 ld=ld+5*(idet-i0)                                                         !/*tdx        70*/
      go to 158                                                                 !/*tdx        71*/
  154 ld=ld+19*(idet-i0)                                                        !/*tdx        72*/
  158 continue                                                                  !/*tdx        73*/
#else                                                                           /*!tdx        74*/
      ld=iptal(li+1,1,ital)+5*(idet-i0)                                         !/*td        110*/
#endif                                                                          /*!tdx        75*/
!                                                                               !/*td        111*/
!        fetch tally quantities for a coincident detector.                      !/*td        112*/
      i=idrc(idet)                                                              !/*td        114*/
      if(i.le.0)go to 170                                                       !/*td        115*/
      icl=drc(kdrc+12,i)                                                        !/*td        118*/
      if(icl.lt.0)go to 572                                                     !/*td4b        7*/
      psc=drc(kdrc+13,i)                                                        !/*td4b        8*/
      ddet=drc(kdrc+14,i)                                                       !/*td        119*/
      amfp=drc(kdrc+15,i)                                                       !/*td        120*/
      ple=drc(kdrc+16,i)                                                        !/*td        121*/
#ifdef RADIOG                                                                   /*!tdx        76*/
      d2=drc(kdrc+17,i)                                                         !/*tdx        77*/
      ar=drc(kdrc+18,i)                                                         !/*tdx        78*/
#endif                                                                          /*!tdx        79*/
      do 160 j=1,10                                                             !/*td        122*/
  160 gpblcm(j)=drc(kdrc+j,i)                                                   !/*td        123*/
#ifdef RADIOG                                                                   /*!tdx        80*/
      if(jptal(lj+8,ital).eq.3)go to 171                                        !/*tdx        81*/
#endif                                                                          /*!tdx        82*/
      go to 210                                                                 !/*td4b        9*/
!                                                                               !/*td        125*/
  170 wgt=tw*wf                                                                 !/*td        126*/
      erg=te                                                                    !/*td        127*/
#ifdef RADIOG                                                                   /*!tdx        83*/
      if(jptal(lj+8,ital).le.2)go to 172                                        !/*tdx        84*/
  171 call tallmg(ld,ng,d2,ar)                                                  !/*tdx        85*/
      if(wgt.eq.0.)go to 560                                                    !/*tdx        86*/
      if(idrc(idet).gt.0)go to 210                                              !/*tdx        87*/
#endif                                                                          /*!tdx        88*/
!                                                                               !/*td        128*/
!        calculate the distance, direction, psc, energy, and velocity.          !/*td        129*/
#ifdef RADIOG                                                                   /*!tdx        89*/
  172 if(jptal(lj+8,ital).le.2)call dddet(ld)                                   !/*tdx        90*/
      psc=half                                                                  !/*tdx        91*/
      jx=jptal(lj+8,ital)                                                       !/*tdx        92*/
      if(ipsc.lt.100.and.(ipsc.ne.11.or.jx.ne.4))call calcps                    !/*tdx        93*/
      if(ipsc.eq.11.and.jx.eq.4)psc=1.0                                         !/*tdx        94*/
#else                                                                           /*!tdx        95*/
      call dddet(ld)                                                            !/*td        130*/
      psc=.5                                                                    !/*td        131*/
      if(ipsc.lt.100)call calcps                                                !/*td        132*/
#endif                                                                          /*!tdx        96*/
      if(kdb.ne.0)go to 630                                                     !/*td4c2       8*/
      if(psc.eq.0.)go to 560                                                    !/*td        134*/
      if(mcal.le.1.eqv.erg.lt.elc(ipt))go to 570                                !/*td        135*/
      dd=ddet-1.e-10*ddet                                                       !/*td4a       15*/
      if(gpt(ipt).ne.0.)                                                        !/*tdx        97*/
     & vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                     !/*tdx        98*/
      if(iptal(li+8,1,ital).eq.0)go to 180                                      !/*td        138*/
      if(dd.lt.vel*(tds(iptal(li+8,1,ital)+iptal(li+8,4,ital)-1)-tme))          !/*td        139*/
     & go to 180                                                                !/*td        140*/
      if(lft(llft+3,ital).eq.0)go to 574                                        !/*td4b       10*/
!                                                                               !/*td        144*/
!        calculate the transmission to the detector.                            !/*td        145*/
  180 s=0.                                                                      !/*td        146*/
#ifdef RADIOG                                                                   /*!tdx        99*/
      if(jptal(lj+8,ital).eq.4)go to 190                                        !/*tdx       100*/
#endif                                                                          /*!tdx       101*/
      if(ddg(1,idet).lt.0.)s=-ddg(1,idet)/(wgt*psc)                             !/*td        147*/
      if(ddg(1,idet).gt.0..and.npstc.gt.200)                                    !/*td        148*/
     & s=ddg(1,idet)*ddn(lddn+24,idet)/(wgt*psc)                                !/*td        149*/
      if(ipsc.ne.11)s=s*2.*pie*dd**2                                            !/*td        150*/
#ifdef RADIOG                                                                   /*!tdx       102*/
  190 if(jptal(lj+8,ital).ne.4)go to 195                                        !/*tdx       103*/
      if(npstc.le.npsmg.or.npsmg.eq.0)go to 195                                 !/*tdx       104*/
      if(ipsc.ne.3.and.ipsc.ne.10.and.ipsc.ne.11.and.                           !/*tdx       105*/
     & ipsc.ne.12.and.ipsc.ne.101)go to 195                                     !/*tdx       106*/
      amfp=0.                                                                   !/*tdx       107*/
      go to 230                                                                 !/*tdx       108*/
#endif                                                                          /*!tdx       109*/
  195 call transm(dd,s,iz)                                                      !/*tdx       110*/
      if(iz.ne.0)go to 562                                                      !/*td4b       12*/
!                                                                               !/*td        154*/
!        save the tally quantities of a master detector.                        !/*td        155*/
      if(idrc(idet).ne.-1)go to 210                                             !/*td        157*/
      do 200 i=1,10                                                             !/*td        158*/
  200 drc(kdrc+i,idet)=gpblcm(i)                                                !/*td        159*/
      drc(kdrc+12,idet)=icl                                                     !/*td        160*/
      drc(kdrc+13,idet)=psc                                                     !/*td        161*/
      drc(kdrc+14,idet)=ddet                                                    !/*td        162*/
      drc(kdrc+15,idet)=amfp                                                    !/*td        163*/
      drc(kdrc+16,idet)=ple                                                     !/*td        164*/
#ifdef RADIOG                                                                   /*!tdx       111*/
      drc(kdrc+17,idet)=d2                                                      !/*tdx       112*/
      drc(kdrc+18,idet)=ar                                                      !/*tdx       113*/
#endif                                                                          /*!tdx       114*/
!                                                                               !/*td        165*/
!        calculate the score for a collision not close to the detector.         !/*td        166*/
#ifdef RADIOG                                                                   /*!tdx       115*/
  210 if(jptal(lj+8,ital).gt.2)go to 215                                        !/*tdx       116*/
      if((ddet.lt.tds(ld+4).or.amfp.lt.-tds(ld+4)))go to 220                    !/*tdx       117*/
  215 t=wgt*psc*exp(-amfp)                                                      !/*tdx       118*/
      if(ipsc.ne.11)t=t/(2.*pie*(ddet-d2)**2)                                   !/*tdx       119*/
#else                                                                           /*!tdx       120*/
  210 if((ddet.lt.tds(ld+4).or.amfp.lt.-tds(ld+4)))go to 220                    !/*td        167*/
      t=wgt*psc*exp(-amfp)                                                      !/*td        168*/
      if(ipsc.ne.11)t=t/(2.*pie*ddet**2)                                        !/*td        169*/
#endif                                                                          /*!tdx       121*/
      go to 230                                                                 !/*td        170*/
!                                                                               !/*td        171*/
!        special calculation for a collision close to the detector.             !/*td        172*/
  220 if(ple.ne.0..and.tds(ld+4).gt.0.)t=wgt*psc*                               !/*td        173*/
     & (1.-exp(-ple*tds(ld+4)))/(tds(ld+5)*ple)                                 !/*td        174*/
      if(tds(ld+4).lt.0.)t=wgt*psc*(1.-exp(tds(ld+4)))*ple**2/tds(ld+5)         !/*td        175*/
      if(ple.eq.0.)t=wgt*psc*abs(tds(ld+4))/tds(ld+5)                           !/*td        176*/
!                                                                               !/*td        177*/
!        find the energy bin and multiply by the energy multipliers.            !/*td        178*/
  230 ta=t                                                                      !/*td        179*/
#ifdef MESHTAL                                                                  /*!tdx       122*/
      if (mpdflg.ne.0) then                                                     !/*tdx       123*/
         do 235 i=1,3                                                           !/*tdx       124*/
         if (lev.eq.0) then                                                     !/*tdx       125*/
            zd(i)=gpb9cm(npb,i)                                                 !/*tdx       126*/
         else                                                                   !/*tdx       127*/
            zd(i)=udt(i,0)                                                      !/*tdx       128*/
         endif                                                                  !/*tdx       129*/
         zd(i+3)=gpblcm(i+3)                                                    !/*tdx       130*/
  235    continue                                                               !/*tdx       131*/
         zd(7)=ta                                                               !/*tdx       132*/
         zd(8)=erg                                                              !/*tdx       133*/
         jz(1)=mat(lmat+icl)                                                    !/*tdx       134*/
         call gdtal(5,dd,zd,jz)                                                 !/*tdx       135*/
      endif                                                                     !/*tdx       136*/
#endif                                                                          /*!tdx       137*/
!                                                                               !/*tdx       138*/
  238 continue                                                                  !/*tdx       139*/
#ifdef RADIOG                                                                   /*!tdx       140*/
      if(jptal(lj+8,ital).ne.3)go to 245                                        !/*tdx       141*/
      t1=tds(ld+16)                                                             !/*tdx       142*/
      if(t1.gt.0.)t1=pie*t1**2                                                  !/*tdx       143*/
      if(t1.le.0.)t1=1.                                                         !/*tdx       144*/
      ta=ta*t1/ar                                                               !/*tdx       145*/
#endif                                                                          /*!tdx       146*/
  245 ibu=1                                                                     !/*tdx       147*/
      if(lft(llft+6,ital)+lft(llft+7,ital).ne.0)ibu=jft(kjft+ital)              !/*td        181*/
      if(lft(llft+4,ital).ne.0)ibu=jbin(3,ncp+zero)                             !/*td        182*/
      if(lft(llft+5,ital).eq.0)go to 250                                        !/*td        183*/
      do 240 ibu=1,iptal(li+3,4,ital)-1                                         !/*td        184*/
  240 if(tds(iptal(li+3,1,ital)+ibu).eq.ncl(lncl+icl9(npb)))go to 270           !/*td        185*/
      ibu=0                                                                     !/*td        186*/
      go to 580                                                                 !/*td        187*/
  250 if(lft(llft+9,ital).eq.0)go to 270                                        !/*td        188*/
      l=jxs(ljxs+1,mgegbt(1))+2*jgm(1)-1                                        !/*td        189*/
      do 260 ibu=1,iptal(li+3,4,ital)-1                                         !/*td        190*/
  260 if(abs(tds(iptal(li+3,1,ital)+ibu)-xss(l+jgp)).lt..001*xss(l+jgp))        !/*td        191*/
     & go to 270                                                                !/*td        192*/
      go to 580                                                                 !/*td        193*/
  270 if(ibu.eq.0)go to 580                                                     !/*td        194*/
      ibe=1                                                                     !/*td        195*/
      ibt=1                                                                     !/*td        196*/
      jbd=1                                                                     !/*td        197*/
#ifdef RADIOG                                                                   /*!tdx       148*/
      if(jptal(lj+8,ital).le.2)go to 271                                        !/*tdx       149*/
      if(ipsc.eq.3.or.ipsc.eq.10.or.ipsc.eq.11.or.ipsc.eq.12.or.                !/*tdx       150*/
     & ipsc.eq.101)jbd=iptal(li+2,3,ital)                                       !/*tdx       151*/
      go to 275                                                                 !/*tdx       152*/
#endif                                                                          /*!tdx       153*/
  271 if(ncp.eq.0.or.ipsc.eq.12)jbd=iptal(li+2,3,ital)                          !/*tdx       154*/
  275 if(jptal(lj+4,ital).ne.0)ta=ta*erg                                        !/*tdx       155*/
      if(iptal(li+3,2,ital).ne.0)ta=dosef(ta)                                   !/*td        200*/
      if(iptal(li+7,1,ital).eq.0)go to 300                                      !/*td        201*/
      l=lft(llft+2,ital)                                                        !/*td        202*/
      if(l.eq.0)go to 290                                                       !/*td        203*/
  280 t1=2.*rang()-1.                                                           !/*td        204*/
      t2=t1**2+rang()**2                                                        !/*td        205*/
      if(t2.gt.1.)go to 280                                                     !/*td        206*/
      erg=erg+.60056120439322*(tds(l+1)+tds(l+2)*sqrt(max(zero,                 !/*td4b       13*/
     & erg+tds(l+3)*erg**2)))*sqrt(-log(t2)/t2)*t1                              !/*td4b       14*/
  290 ibe=jbin(7,erg)                                                           !/*td        209*/
      if(ibe.eq.0)go to 580                                                     !/*td        210*/
      if(iptal(li+7,2,ital).ne.0)ta=ta*tds(iptal(li+7,2,ital)+ibe)              !/*td        211*/
!                                                                               !/*td        212*/
!        find the time bin or do time convolution.                              !/*td        213*/
  300 if(iptal(li+8,1,ital).eq.0)go to 340                                      !/*td        214*/
      ll=lft(llft+3,ital)                                                       !/*td        215*/
      if(ll.ne.0)go to 310                                                      !/*td        216*/
      ibt=jbin(8,tme)                                                           !/*td        217*/
      if(ibt.eq.0)go to 580                                                     !/*td        218*/
      go to 330                                                                 !/*td        219*/
  310 tt=ta                                                                     !/*td        220*/
      ca=tme+tds(ll+1)                                                          !/*td        221*/
      cd=tme+tds(ll+2)                                                          !/*td        222*/
      ibt=1                                                                     !/*td        223*/
      b=-huge                                                                   !/*td        224*/
  320 a=b                                                                       !/*td        225*/
      if(a.ge.cd)go to 480                                                      !/*td        226*/
      b=tds(iptal(li+8,1,ital)+ibt)                                             !/*td        227*/
      if(b.le.ca)go to 470                                                      !/*td        228*/
      ta=tt*(min(b,cd)-max(a,ca))/(tds(ll+2)-tds(ll+1))                         !/*td        229*/
  330 if(iptal(li+8,2,ital).ne.0)ta=ta*tds(iptal(li+8,2,ital)+ibt)              !/*td        230*/
!                                                                               !/*td        231*/
!        do the multiplier bins.                                                !/*td        232*/
  340 kk=0                                                                      !/*td        233*/
      do 460 km=1,mm                                                            !/*td        234*/
      tc=ta                                                                     !/*td        235*/
      lm=iptal(li+5,4,ital)-km                                                  !/*td        236*/
      if(iptal(li+5,2,ital).eq.0)go to 350                                      !/*td        237*/
      kk=kk+1                                                                   !/*td        238*/
      if(iptal(li+5,1,ital).ne.0)call wtmult(tc,kk)                             !/*td        239*/
      tc=tc*tds(iptal(li+5,2,ital)+kk)                                          !/*td        240*/
!                                                                               !/*td        241*/
!        user bins loop.                                                        !/*td        242*/
  350 mu=0                                                                      !/*td        243*/
#ifdef RADIOG                                                                   /*!tdx       156*/
      ds=1.                                                                     !/*tdx       157*/
      dc=1.                                                                     !/*tdx       158*/
      m=ld                                                                      !/*tdx       159*/
      if(jptal(lj+8,ital).ne.3)go to 355                                        !/*tdx       160*/
      if(iptal(li+4,1,ital).eq.0.or.iptal(li+6,1,ital).eq.0)                    !/*tdx       161*/
     & go to 355                                                                !/*tdx       162*/
      ds=(tds(iptal(li+4,1,ital)+ibs)-tds(iptal(li+4,1,ital)+ibs-1))            !/*tdx       163*/
     & /(tds(iptal(li+4,1,ital)+iptal(li+4,4,ital)-1)-                          !/*tdx       164*/
     & tds(iptal(li+4,1,ital)))                                                 !/*tdx       165*/
      dc=(tds(iptal(li+6,1,ital)+ibc)-tds(iptal(li+6,1,ital)+ibc-1))            !/*tdx       166*/
     & /(tds(iptal(li+6,1,ital)+iptal(li+6,4,ital)-1)-                          !/*tdx       167*/
     & tds(iptal(li+6,1,ital)))                                                 !/*tdx       168*/
#endif                                                                          /*!tdx       169*/
  355 td=tc                                                                     !/*tdx       170*/
      if(iptal(li+3,1,ital).eq.0)go to 380                                      !/*td        245*/
      if(lft(llft+4,ital)+lft(llft+5,ital)+lft(llft+7,ital)                     !/*td        246*/
     & +lft(llft+9,ital).ne.0)go to 380                                         !/*td        247*/
      nx=1                                                                      !/*td        248*/
  360 mu=0                                                                      !/*td        249*/
      td=tc                                                                     !/*td        250*/
      ntx=nx                                                                    !/*td        251*/
      call tallyx(td,mu)                                                        !/*td        252*/
      if(mu.lt.0)go to 460                                                      !/*td        253*/
      if(ibu.gt.0.and.ibu.lt.iptal(li+3,4,ital))go to 370                       !/*td        254*/
      call errprn(1,ntbb(lntb+4,ital),2,zero+jptal(ljpt+1,ital),                !/*td4b       15*/
     & zero+ibu,'tal','bin','tally not scored beyond last user bin.')           !/*td4a       22*/
      go to 460                                                                 !/*td        256*/
  370 nx=nx+1                                                                   !/*td        257*/
      if(nx.le.iptal(li+1,5,ital)+1)go to 380                                   !/*td        258*/
      call expirx(1,'tallyd','tallyx is in an endless loop.')                   !/*td        259*/
      return                                                                    !/*td        260*/
!                                                                               !/*td        261*/
!        totals nest.                                                           !/*td        262*/
  380 if(td.eq.0.)go to 450                                                     !/*td        263*/
      j1=ktal+locdt(2,idet)                                                     !/*td        264*/
      lu=iptal(li+3,4,ital)-ibu                                                 !/*td        265*/
      me=iptal(li+7,4,ital)-ibe                                                 !/*td        266*/
      mt=iptal(li+8,4,ital)-ibt                                                 !/*td        267*/
      if(iptal(li+3,6,ital).ne.0)lu=1                                           !/*td4c       31*/
      if(iptal(li+5,6,ital).ne.0)lm=1                                           !/*td4c       32*/
      if(iptal(li+7,6,ital).ne.0)me=1                                           !/*td4c       33*/
      if(iptal(li+8,6,ital).ne.0)mt=1                                           !/*td4c       34*/
#ifdef RADIOG                                                                   /*!tdx       171*/
      ls=iptal(li+4,4,ital)-ibs                                                 !/*tdx       172*/
      lc=iptal(li+6,4,ital)-ibc                                                 !/*tdx       173*/
      ms=iptal(li+4,3,ital)                                                     !/*tdx       174*/
      mc=iptal(li+6,3,ital)                                                     !/*tdx       175*/
      is=ibs                                                                    !/*tdx       176*/
  382 j6=ktal+locdt(2,idet)+(is-1)*ik(5)                                        !/*tdx       177*/
      if(jptal(lj+8,ital).eq.3.and.is.eq.ms.and.ms.gt.1)                        !/*tdx       178*/
     & td=td*ds/dc                                                              !/*tdx       179*/
      ic=ibc                                                                    !/*tdx       180*/
  384 j1=j6+(ic-1)*ik(7)                                                        !/*tdx       181*/
      if(jptal(lj+8,ital).eq.3.and.ic.eq.mc.and.mc.gt.1)                        !/*tdx       182*/
     & td=td*dc                                                                 !/*tdx       183*/
#endif                                                                          /*!tdx       184*/
      iu=ibu                                                                    !/*td        268*/
  390 j2=j1+(iu-1)*ik(4)                                                        !/*td        269*/
      im=km                                                                     !/*td        270*/
  400 j3=j2+(im-1)*ik(6)                                                        !/*td        271*/
      id=1                                                                      !/*td        272*/
  410 j4=j3+(id-1)*ik(3)                                                        !/*td        273*/
      ie=ibe                                                                    !/*td        274*/
  420 j5=j4+(ie-1)*ik(8)                                                        !/*td        275*/
      it=ibt                                                                    !/*td        276*/
  430 if(j5+it.eq.ktal+iwwg)call avrclc(td)                                     !/*td4c       35*/
      if(tal(j5+it).ne.0.)go to 440                                             !/*td        278*/
      jtls=jtls+1                                                               !/*td        279*/
      if(jtls.le.ktls)tal(ktal+nmxf*mxf+jtls)=j5+it                             !/*td4a       23*/
#ifdef RADIOG                                                                   /*!tdx       185*/
  440 if(jptal(lj+8,ital).ne.4.or.npstc.le.npsmg.or.npsmg.eq.0)go to 442        !/*tdx       186*/
      if(ipsc.ne.3.and.ipsc.ne.10.and.ipsc.ne.11.and.                           !/*tdx       187*/
     & ipsc.ne.12.and.ipsc.ne.101)go to 442                                     !/*tdx       188*/
      ij=0                                                                      !/*tdx       189*/
      if(id.eq.1)ij=ik(3)                                                       !/*tdx       190*/
#if defined(MULTP) || defined(MULTT)                                            /*!tdx       191*/
      td=tal(j5-ktal+ltal+it+ij+mxf)/npsout                                     !/*tdx       192*/
#else                                                                           /*!tdx       193*/
      td=tal(j5+it+ij+mxf)/(nps-1)                                              !/*tdx       194*/
#endif                                                                          /*!tdx       195*/
  442 tal(j5+it)=tal(j5+it)+td                                                  !/*tdx       196*/
#else                                                                           /*!tdx       197*/
  440 tal(j5+it)=tal(j5+it)+td                                                  !/*td        281*/
#endif                                                                          /*!tdx       198*/
      it=it+mt                                                                  !/*td        282*/
      if(it.le.kt)go to 430                                                     !/*td        283*/
      ie=ie+me                                                                  !/*td        284*/
      if(ie.le.je)go to 420                                                     !/*td        285*/
      id=id+1                                                                   !/*td        286*/
      if(id.le.jbd)go to 410                                                    !/*td        287*/
      im=im+lm                                                                  !/*td        288*/
      if(im.le.jm)go to 400                                                     !/*td        289*/
      iu=iu+lu                                                                  !/*td        290*/
      if(iu.le.ju)go to 390                                                     !/*td        291*/
#ifdef RADIOG                                                                   /*!tdx       199*/
      ic=ic+lc                                                                  !/*tdx       200*/
      if(ic.le.mc)go to 384                                                     !/*tdx       201*/
      is=is+ls                                                                  !/*tdx       202*/
      if(is.le.ms)go to 382                                                     !/*tdx       203*/
#endif                                                                          /*!tdx       204*/
  450 if(mu.ne.0)go to 360                                                      !/*td        292*/
  460 continue                                                                  !/*td        293*/
      if(ll.eq.0)go to 480                                                      !/*td        294*/
  470 ibt=ibt+1                                                                 !/*td        295*/
      if(ibt.lt.iptal(li+8,4,ital))go to 320                                    !/*td        296*/
!                                                                               !/*td        297*/
!        accumulate data for detector diagnostics.                              !/*td        298*/
#ifdef RADIOG                                                                   /*!tdx       205*/
  480 if(jptal(lj+8,ital).gt.2)go to 580                                        !/*tdx       206*/
      ddn(kddn+20,idet)=ddn(kddn+20,idet)+t                                     !/*tdx       207*/
#else                                                                           /*!tdx       208*/
  480 ddn(kddn+20,idet)=ddn(kddn+20,idet)+t                                     !/*td        299*/
#endif                                                                          /*!tdx       209*/
      j=0                                                                       !/*td        300*/
      if(ddg(1,idet).ge.0.)go to 500                                            !/*td        301*/
      if(t.ge.-ddg(1,idet)*ddg(2,idet))j=1                                      !/*td        302*/
      do 490 i=1,8                                                              !/*td        303*/
  490 if(t.le.-talb(i,2)*ddg(1,idet))go to 520                                  !/*td        304*/
      go to 520                                                                 !/*td        305*/
  500 i=9                                                                       !/*td        306*/
      if(npstc.le.200)go to 520                                                 !/*td        307*/
      if(t.ge.ddg(2,idet)*ddn(lddn+24,idet))j=1                                 !/*td        308*/
      i=1                                                                       !/*td4a       24*/
      if(t.le.ddg(1,idet)*ddn(lddn+24,idet))go to 520                           !/*td4a       25*/
      do 510 i=2,7                                                              !/*td4a       26*/
  510 if(t.le.talb(i,1)*ddn(lddn+24,idet))go to 520                             !/*td        311*/
  520 ddn(kddn+i,idet)=ddn(kddn+i,idet)+1.                                      !/*td        312*/
      ddn(kddn+9+i,idet)=ddn(kddn+9+i,idet)+t                                   !/*td        313*/
      if(t.gt.ddn(kddn+21,idet))ddn(kddn+23,idet)=npstc                         !/*td        314*/
      ddn(kddn+21,idet)=max(ddn(kddn+21,idet),t)                                !/*td        315*/
      dec(kdec+1,lg)=dec(kdec+1,lg)+1.                                          !/*td        316*/
      dec(kdec+2,lg)=dec(kdec+2,lg)+t                                           !/*td        317*/
!                                                                               !/*td        318*/
!        print a line for especially heavy scores.                              !/*td        319*/
      if(j.eq.0)go to 580                                                       !/*td        320*/
#ifdef MULTT                                                                    /*!tdxk        1*/
      call thread_lockon(jlock)                                                 !/*tdxk        2*/
#endif                                                                          /*!tdxk        3*/
      if(ddn(lddn+22,idet).ge.100.)go to 540                                    !/*td        322*/
      ddn(lddn+22,idet)=ddn(lddn+22,idet)+1.                                    !/*td        323*/
      if(dbcn(26).ne.0.)then                                                    !/*tdx       211*/
      if(mct.ge.-1)write(iuo,530)'   '//hnp(ipt)(1:1)//'       ',               !/*tdx       212*/
     & idet,t,wgt,psc,amfp,ddet,                                                !/*tdx       213*/
     & tds(ld+4),erg,ncl(lncl+icl9(npb)),npstc,nch(ipt),rnr+rnrtc               !/*tdx       214*/
      else                                                                      !/*tdx       215*/
      if(mct.ge.-1)write(iuo,530)hnp(ipt),idet,t,wgt,psc,amfp,ddet,             !/*td4c       36*/
     & tds(ld+4),erg,ncl(lncl+icl9(npb)),npstc,nch(ipt),rnr+rnrtc               !/*td4c2       9*/
      endif                                                                     !/*tdx       216*/
  530 format(/4h det,5x,1ht,9x,3hwgt,8x,3hpsc,7x,4hamfp,7x,5hddetx,5x,          !/*td        326*/
     & 6hradius,7x,3herg,5x,4hcell,8x,3hnps,2x,3hnch,2x,a11,3hnrn/              !/*tdx       217*/
     & 1x,i2,1p7e11.4,i6,i11,i5,0pf17.0,tl1,1h )                                !/*td        328*/
  540 continue                                                                  !/*td        329*/
#ifdef MULTT                                                                    /*!tdxk        4*/
      call thread_lockoff(jlock)                                                !/*tdxk        5*/
#endif                                                                          /*!tdxk        6*/
      go to 580                                                                 !/*td        331*/
!                                                                               !/*td        332*/
!        increment ndpf for particles that failed.                              !/*td        333*/
  550 ndpf(kndp+1,idet)=ndpf(kndp+1,idet)+1                                     !/*td        334*/
      go to 625                                                                 !/*tdx       219*/
  560 ndpf(kndp+2,idet)=ndpf(kndp+2,idet)+1                                     !/*td        336*/
      if(idrc(idet).eq.-1)drc(kdrc+12,idet)=-2                                  !/*td4b       16*/
      go to 580                                                                 !/*td4b       17*/
  562 if(kdb.ne.0)go to 630                                                     !/*td4b       18*/
      ndpf(kndp+iz,idet)=ndpf(kndp+iz,idet)+1                                   !/*td4b       19*/
      if(idrc(idet).eq.-1)drc(kdrc+12,idet)=-iz                                 !/*td4b       20*/
      if(iz.ne.5)go to 580                                                      !/*td4b       21*/
      j=ndpf(kndp+5,idet)-1                                                     !/*td4c       38*/
#if defined(MULTT) || defined(MULTP)                                            /*!td4c       39*/
      j=j+ndpf(lndp+5,idet)                                                     !/*td4c       40*/
#endif                                                                          /*!cpp       271*/
      call errprn(1,j,2,                                                        !/*td4b       23*/
     & zero+jptal(ljpt+1,ital),zero+ncl(lncl+icl),'tal','cel',                  !/*td4b       24*/
     & 'score for point detector tally prevented by zero importance.')          !/*td4b       25*/
      go to 580                                                                 !/*td        337*/
  570 ndpf(kndp+6,idet)=ndpf(kndp+6,idet)+1                                     !/*td        338*/
      if(idrc(idet).eq.-1)drc(kdrc+12,idet)=-6                                  !/*td4b       26*/
      go to 580                                                                 !/*td4b       27*/
  572 if(icl.eq.-10)go to 576                                                   !/*td4b       28*/
      ndpf(kndp-icl,idet)=ndpf(kndp-icl,idet)+1                                 !/*td4b       29*/
      go to 580                                                                 !/*td4b       30*/
  574 if(idrc(idet).eq.-1)drc(kdrc+12,idet)=-10                                 !/*td4b       31*/
  576 call errprn(1,ntbb(lntb+3,ital),3,zero+jptal(ljpt+1,ital),tme,            !/*td4b       32*/
     & 'tal','tme','tally not scored beyond last time bin.')                    !/*td4b       33*/
!                                                                               !/*td        339*/
!        restore the particle description.                                      !/*td        340*/
  580 call getpar(nb,2)                                                         !/*td4c2      11*/
!                                                                               !/*td        347*/
#ifdef RADIOG                                                                   /*!tdx       220*/
  620 continue                                                                  !/*tdx       221*/
#endif                                                                          /*!tdx       222*/
  625 continue                                                                  !/*tdx       223*/
      npb=nb-1                                                                  !/*td4c2      12*/
      return                                                                    !/*td        350*/
!                                                                               !/*td        351*/
!        return with kdb=3 or 4 if the particle gets lost.                      !/*td        352*/
  630 kdb=kdb+2                                                                 !/*td        353*/
      npb=nb-1                                                                  !/*td4c2      13*/
      return                                                                    !/*td        354*/
      end                                                                       !/*td        355*/

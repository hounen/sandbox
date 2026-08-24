!#ident electr.F from deck el                                                   !/*el          1*/
      subroutine electr                                                         !/*el          2*/
!        run one electron track.                                                !/*el          3*/
      use global_data                                                           !/*elx         1*/
!                                                                               !/*elx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*elx         3*/
      implicit integer(kindi) (i-n)                                             !/*elx         4*/
!                                                                               !/*elx         5*/
#ifdef MESHTAL                                                                  /*!elx         6*/
      dimension zd(10),jz(5)                                                    !/*elx         7*/
#endif                                                                          /*!elx         8*/
!                                                                               !/*el          5*/
!        find the electron energy group.                                        !/*el          6*/
      ngp=1                                                                     !/*el          7*/
      ib=nee(3)                                                                 !/*elx         9*/
   10 if(ib-ngp.eq.1)go to 30                                                   !/*el          9*/
      ih=(ngp+ib)/2                                                             !/*el         10*/
      if(erg.ge.eee(leee+3,ih))go to 20                                         !/*elx        10*/
      ngp=ih                                                                    !/*el         12*/
      go to 10                                                                  !/*el         13*/
   20 ib=ih                                                                     !/*el         14*/
      go to 10                                                                  !/*el         15*/
!                                                                               !/*el         16*/
!        set up the material index and the energy loss rate.                    !/*el         17*/
   30 rr=0.                                                                     !/*el         18*/
!        Initializing qs is probably unnecessary.                               !/*elx        11*/
      qs=0.                                                                     !/*elx        12*/
      nq=ngp                                                                    !/*el4b        1*/
      if(dbcn(18).ne.0..and.erg.lt.0.5*(eee(leee+3,ngp)+                        !/*elx        13*/
     & eee(leee+3,ngp+1)).and.ngp.lt.nee(3)-1)nq=ngp+1                          !/*elx        14*/
      mkc=mat(lmat+icl)                                                         !/*el         19*/
      if(mkc.eq.0)go to 40                                                      !/*el         20*/
      qs=esloss(nq)                                                             !/*el4b        4*/
      ns=nsb(lnsb+mkc)                                                          !/*el         22*/
!                                                                               !/*el         23*/
!        do electron energy substeps until the track terminates.                !/*el         24*/
   40 if(wgt.gt.0.)go to 50                                                     !/*el         25*/
      call expirx(1,'electr',                                                   !/*el         26*/
     & 'the weight of the current particle is zero or less.')                   !/*el         27*/
      return                                                                    !/*el         28*/
!                                                                               !/*el         29*/
!        calculate the distances to time cutoff and energy substep.             !/*el         30*/
   50 dtc=vel*(tco(3)-tme)                                                      !/*el         31*/
      n1=ngp                                                                    !/*el4b        5*/
      pmf=huge                                                                  !/*el         32*/
#ifdef MESHTAL                                                                  /*!elx        15*/
      fd=0.0                                                                    !/*elx        16*/
#endif                                                                          /*!elx        17*/
      if(mkc.ne.0)pmf=drs(ldrs+3,nq+nee_max*(mkc-1))/rho(lrho+icl)              !/*elx        18*/
!                                                                               !/*el         34*/
!        calculate the distance to the cell boundary, dls.                      !/*el         35*/
      if(mbd(lmbd+icl).ne.0.or.jsu.ne.0)go to 60                                !/*el         36*/
      dls=huge                                                                  !/*el         37*/
      rr=rr-pmf                                                                 !/*el         38*/
      if(rr.gt.0.)go to 70                                                      !/*el         39*/
      rr=dbmin()-pmf                                                            !/*el         40*/
      if(rr.gt.0.)go to 70                                                      !/*el         41*/
   60 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*el         42*/
      call track(icl)                                                           !/*el         43*/
      if(kdb.ne.0)return                                                        !/*el         44*/
      jsu=jap                                                                   !/*el         45*/
!                                                                               !/*el         46*/
!        tally the track length in the cell.                                    !/*el         47*/
   70 d=min(pmf,dls,dtc)                                                        !/*el         48*/
      if(iwwg.gt.0)call avrwwg(d)                                               !/*el4c        3*/
      l=locct(llct+3,icl)                                                       !/*el         49*/
      if(l.ne.0)call tally(                                                     !/*elx        19*/
#ifdef MESHTAL                                                                  /*!elx        20*/
     & zero,0,                                                                  !/*elx        21*/
#endif                                                                          /*!elx        22*/
     & l,d)                                                                     !/*elx        23*/
      if(kdb.ne.0)return                                                        !/*el         51*/
      do 75 i=0,lev-1                                                           !/*el4a        1*/
      l=locct(llct+3,int(udt(7,i)))                                             !/*el4a        2*/
   75 if(l.ne.0)call tally(                                                     !/*elx        24*/
#ifdef MESHTAL                                                                  /*!elx        25*/
     & zero,0,                                                                  !/*elx        26*/
#endif                                                                          /*!elx        27*/
     & l,d)                                                                     !/*elx        28*/
!                                                                               !/*el         52*/
!        increment the summary accounts.                                        !/*el         53*/
      dt=d/vel                                                                  !/*el         54*/
      pac(kpac+3,5,icl)=pac(kpac+3,5,icl)+wgt*dt*erg                            !/*el         55*/
      pac(kpac+3,6,icl)=pac(kpac+3,6,icl)+wgt*d*erg                             !/*el         56*/
      pac(kpac+3,7,icl)=pac(kpac+3,7,icl)+d                                     !/*el         57*/
      if(pmf.ne.huge)pac(kpac+3,8,icl)=pac(kpac+3,8,icl)+wgt*d*pmf              !/*el         58*/
      pac(kpac+3,9,icl)=pac(kpac+3,9,icl)+wgt*dt                                !/*el         59*/
      pac(kpac+3,10,icl)=pac(kpac+3,10,icl)+wgt*d                               !/*el         60*/
!                                                                               !/*el         61*/
!        scatter the electron.                                                  !/*el         62*/
      uold(1)=uuu                                                               !/*el4b        7*/
      uold(2)=vvv                                                               !/*el4b        8*/
      uold(3)=www                                                               !/*el4b        9*/
      eg0=erg                                                                   !/*elx        29*/
      if(mkc.eq.0)go to 140                                                     !/*el         66*/
      am=1.                                                                     !/*el4b       10*/
      call escat(am,d,one,one,nq,1)                                             !/*el4c        4*/
#ifdef MESHTAL                                                                  /*!elx        30*/
      ej=erg                                                                    !/*elx        31*/
      erg=eg0                                                                   !/*elx        32*/
      fd=d*qs                                                                   !/*elx        33*/
      if(erg-d*qs.lt.elc(3))fd=erg                                              !/*elx        34*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*elx        35*/
         do 90 i=1,6                                                            !/*elx        36*/
         if (lev.eq.0) then                                                     !/*elx        37*/
            zd(i)=gpblcm(i)                                                     !/*elx        38*/
         else                                                                   !/*elx        39*/
            zd(i)=udt(i,0)                                                      !/*elx        40*/
         endif                                                                  !/*elx        41*/
   90    continue                                                               !/*elx        42*/
         zd(7)=wgt                                                              !/*elx        43*/
         zd(8)=erg                                                              !/*elx        44*/
         zd(9)=fd                                                               !/*elx        45*/
         jz(1)=mkc                                                              !/*elx        46*/
         jz(2)=mspf                                                             !/*elx        47*/
         jz(5)=2                                                                !/*elx        48*/
         call gdtal(2,d,zd,jz)                                                  !/*elx        49*/
      endif                                                                     !/*elx        50*/
      l=locct(llct+1,icl)                                                       !/*elx        51*/
      if(l.ne.0)call tally(fd,1,l,d)                                            !/*elx        52*/
      l=locct(llct+ipt,icl)                                                     !/*elx        53*/
      if(l.ne.0)call tally(fd,2,l,d)                                            !/*elx        54*/
      if(kdb.ne.0)return                                                        !/*elx        55*/
      do 98 i=0,lev-1                                                           !/*elx        56*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*elx        57*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*elx        58*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*elx        59*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*elx        60*/
   98    continue                                                               !/*elx        61*/
      erg=ej                                                                    !/*elx        62*/
#endif                                                                          /*!elx        63*/
!                                                                               !/*el         74*/
!        lose energy to ionization along the track.                             !/*el         75*/
      erg=erg-d*qs                                                              !/*el         77*/
      no=ngp                                                                    !/*el4a        4*/
      qo=qs                                                                     !/*el4a        5*/
      call lose_pax (loss_electron_scattering, 3, 0, zero, wgt*d*qs)            !/*elx        64*/
      if(erg.lt.elc(3))go to 170                                                !/*el         80*/
      if(erg.lt.eee(leee+3,ngp+1))go to 100                                     !/*elx        65*/
      go to 120                                                                 !/*el4b       31*/
  100 ngp=ngp+1                                                                 !/*el         84*/
      if(erg.lt.eee(leee+3,ngp+1))go to 100                                     !/*elx        66*/
!                                                                               !/*el         88*/
!        produce secondary particles and lose energy to bremsstrahlung.         !/*el         89*/
  120 npa=1                                                                     !/*el         90*/
      f=(eee(leee+3,ngp)-erg)/(eee(leee+3,ngp)-eee(leee+3,ngp+1))               !/*elx        67*/
      if(rnok.ne.0.)call knock(d,f,no,am)                                       !/*el4b       33*/
      if(erg.ge.edg(ledg+mkc).and.eek(leek+mkc).gt.min(elc(2),elc(3)))          !/*cor4-1    155*/
     & call kxray(d,f)                                                          !/*el         94*/
      call brems(d,qo,f,no,am)                                                  !/*el4b       34*/
      if(erg.lt.elc(3))go to 170                                                !/*el         96*/
      if(erg.ge.eee(leee+3,ngp+1))go to 140                                     !/*elx        68*/
  130 ngp=ngp+1                                                                 !/*el         98*/
      if(erg.lt.eee(leee+3,ngp+1))go to 130                                     !/*elx        69*/
!                                                                               !/*el        102*/
!        update the particle to the surface, substep, or termination.           !/*el        103*/
  140 call uplpos(xxx,uold,lev,d,vel,1)                                         !/*el4b       36*/
      if(d.eq.dtc)go to 160                                                     !/*el        108*/
      vel=slite*sqrt(erg*(erg+2.*gpt(3)))/(erg+gpt(3))                          !/*el        109*/
!                                                                               !/*el        110*/
!        process the end of the energy substep.                                 !/*el        111*/
      ns=ns-1                                                                   !/*el4b       37*/
      if(pmf.ge.dls)go to 150                                                   !/*el        112*/
      pac(kpac+3,3,icl)=pac(kpac+3,3,icl)+1.                                    !/*el        113*/
      pac(kpac+3,4,icl)=pac(kpac+3,4,icl)+wgt                                   !/*el        114*/
      if(nmaz.ne.0)call rslmaz(3,1)                                             !/*el4b       38*/
      nch(3)=nch(3)+1                                                           !/*el        115*/
      ncp=ncp+1                                                                 !/*el        116*/
#ifdef MESHTAL                                                                  /*!elx        70*/
      mspf=0                                                                    !/*elx        71*/
#endif                                                                          /*!elx        72*/
      jsu=0                                                                     !/*el        117*/
!                                                                               !/*el        118*/
!        play the weight-window and energy-splitting games.                     !/*el        119*/
      if(wwp(3,4).ne.0..and.ns.eq.1)call wtwndo(4)                              !/*el4c        6*/
      if(nter.ne.0)return                                                       !/*el        121*/
      if(espl(3,1).ne.0.)call ergimp                                            !/*el        122*/
      if(nter.ne.0)return                                                       !/*el        123*/
      go to 152                                                                 !/*el4b       40*/
!                                                                               !/*el4b       41*/
!        see if deflection keeps electron in same cell.                         !/*el4b       42*/
  150 call echkcl(j)                                                            !/*el4b       43*/
      if(j.ne.0)go to 158                                                       !/*el4b       44*/
!                                                                               !/*el4b       45*/
!        complete energy substep for electrons remaining in cell icl.           !/*el4b       46*/
!        dbcn(18).ne.0:  its3.0 nearest group boundary data treatment.          !/*el4b       47*/
  152 if(dbcn(18).eq.0.)go to 153                                               !/*el4b       48*/
      if(ns.ne.0)go to 40                                                       !/*el4b       50*/
      nq=ngp                                                                    !/*el4b       51*/
      if(erg.lt.0.5*(eee(leee+3,ngp)+eee(leee+3,ngp+1)).and.                    !/*elx        73*/
     & ngp.lt.nee(3)-1)nq=ngp+1                                                 !/*elx        74*/
      go to 157                                                                 !/*el4b       54*/
!        dbcn(18)=0:  bin-centered data treatment.                              !/*el4b       55*/
  153 if(n1.eq.ngp)go to 155                                                    !/*el4b       56*/
  154 nq=ngp                                                                    !/*el4b       57*/
      go to 157                                                                 !/*el4b       58*/
  155 if(ns.ne.0)go to 40                                                       !/*el4b       59*/
  157 ns=nsb(lnsb+mkc)                                                          !/*el4b       60*/
      qs=esloss(nq)                                                             !/*el4c        7*/
      go to 40                                                                  !/*el4b       62*/
!                                                                               !/*el4b       63*/
!        shove the electron thru the cell boundary.                             !/*el4b       64*/
  158 call surfac                                                               !/*el4b       65*/
      if(krflg.ne.0)call eventp(3)                                              !/*el        130*/
      if(iptr.ne.0)call ptrak(3)                                                !/*el4a        7*/
      if(kdb.ne.0)return                                                        !/*el        131*/
      if(nter.ne.0)return                                                       !/*el        132*/
      go to 30                                                                  !/*el        133*/
!                                                                               !/*el        134*/
!        terminate the electron track.                                          !/*el        135*/
  160 nter = loss_all_pls_time_cutoff                                           !/*elx        75*/
      return                                                                    !/*el        137*/
  170 nter = loss_all_pls_energy_cutoff                                         !/*elx        76*/
      if(iphot.eq.0.and.erg.gt.elc(2))call ttbr(d)                              !/*el        139*/
      if(jgp.gt.-2)return                                                       !/*cor4-1    156*/
!                                                                               !/*el        142*/
!        generate annihilation photons for positron below energy cutoff.        !/*el        143*/
      if(gpt(3).lt.elc(2))return                                                !/*cor4-1    157*/
      call savpar(0,3)                                                          !/*el4c2       1*/
      erg=gpt(3)                                                                !/*el        153*/
      ipt=2                                                                     !/*el        154*/
      jan=0                                                                     !/*elxc        1*/
      jgp=0                                                                     !/*cor4-1    158*/
      call uplpos(xxx,uold,lev,.5*d,vel,1)                                      !/*el4b       66*/
      jsu=0                                                                     !/*el        160*/
      npa=1                                                                     !/*el        161*/
      ncp=0                                                                     !/*el        162*/
#ifdef MESHTAL                                                                  /*!elx        77*/
      mspf=2                                                                    !/*elx        78*/
#endif                                                                          /*!elx        79*/
      idx=0                                                                     !/*el        163*/
      do 230 ii=1,ndx(2)                                                        !/*el        164*/
  230 if((xxx-dxx(2,1,ii))**2+(yyy-dxx(2,2,ii))**2+                             !/*el        165*/
     & (zzz-dxx(2,3,ii))**2.lt.dxx(2,5,ii))idx=ii                               !/*el        166*/
      ipsc=102                                                                  !/*el        167*/
      vel=slite                                                                 !/*el4b       67*/
      call isos(uuu,lev)                                                        !/*el4b       68*/
      call gain_pax (igain_photon_p_annihilation, 2,                            !/*elx        80*/
     &               2, two*wgt, two*wgt*erg)                                   !/*elx        81*/
      call contrib_pwb (2,ipwb_photon_p_annihilation,icl,two*wgt)               !/*elx        82*/
      do 250 i=1,2                                                              !/*el        174*/
      if(i.eq.1)go to 240                                                       !/*el        175*/
      uuu=-uuu                                                                  !/*el        176*/
      vvv=-vvv                                                                  !/*el        177*/
      www=-www                                                                  !/*el        178*/
      if(lev.ne.0)call uplev(uuu)                                               !/*el4c        8*/
  240 if(ndet(2).ne.0)call tallyd                                               !/*el        179*/
      if(kdb.ne.0)return                                                        !/*el        180*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*el        181*/
      if(kdb.ne.0)return                                                        !/*el        182*/
  250 call bankit(10)                                                           !/*el4a        8*/
      call getpar(0,3)                                                          !/*el4c2       2*/
      return                                                                    !/*el        191*/
      end                                                                       !/*el        192*/

!#ident brems.F from deck bb                                                    !/*bb          1*/
      subroutine brems(d,qo,f,no,ae)                                            !/*bb4b        1*/
!        generate a bremsstrahlung photon along path segment d.                 !/*bb          3*/
!        qo = energy loss per unit path length at ngp for eg0.                  !/*bb4a        2*/
!        f = energy interpolation factor.                                       !/*bb          5*/
!        no = value of ngp appropriate for eg0.                                 !/*bb4a        3*/
!        ae = deflection cosine of electron over path segment d.                !/*bb4b        2*/
      use global_data                                                           !/*bbx         1*/
!                                                                               !/*bbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bbx         3*/
      implicit integer(kindi) (i-n)                                             !/*bbx         4*/
!                                                                               !/*bbx         5*/
#ifdef MESHTAL                                                                  /*!bbx         6*/
      dimension zd(10),jz(5)                                                    !/*bbx         7*/
#endif                                                                          /*!bbx         8*/
!        two kinds of bremsstrahlung bias are allowed (not together):           !/*bb4c        1*/
!           bnum:  produce bnum times the analog number of photons;             !/*bb4c        2*/
!           numb:  produce 1 photon per electron substep.                       !/*bb4c        3*/
!        el is dimensioned to keep track of first, possibly real event          !/*bb4c        4*/
!        energy loss, and second and higher order events to keep track          !/*bb4c        5*/
!        of energy loss only for production purposes.                           !/*bb4c        6*/
!        ib = 0/1 / no/yes physical bremsstrahlung photons produced.            !/*bb4c        7*/
!        im = 0 for pre-its3.0 physics; = 3 for its3.0 physics.                 !/*bb4c        8*/
      dimension el(2)                                                           !/*bb4c        9*/
!                                                                               !/*bb          7*/
!        decide whether to make a photon.                                       !/*bb          8*/
      pr=d*rho(lrho+icl)*                                                       !/*bb          9*/
     & ((1.-f)*pbr(lpbr+ngp+nee(3)*(mkc-1))+                                    !/*bbx         9*/
     & f*pbr(lpbr+ngp+1+nee(3)*(mkc-1)))                                        !/*bbx        10*/
      im=nxs(lnxs+16,lme(llme+3,jmd(ljmd+mkc)))                                 !/*bb4c       10*/
      r1=rang()                                                                 !/*bb4c       11*/
      p1=exp(-pr)                                                               !/*bb4c       12*/
      ib=1                                                                      !/*bb4c       13*/
      if((im.eq.0.and.pr+r1.lt.1.).or.(im.eq.3.and.r1.lt.p1))ib=0               !/*bb4c       14*/
      if(numb+ib.eq.0)return                                                    !/*bb4c       15*/
!                                                                               !/*bb4c       16*/
!        set up to produce none, one, or more photons.                          !/*bb4c       17*/
      na=1                                                                      !/*bb4c       18*/
      nb=1                                                                      !/*bb4c       19*/
      wb=wgt                                                                    !/*bb4c       20*/
!                                                                               !/*bb4c       21*/
!        do splitting or roulette if bnum biasing is used.                      !/*bb4c       22*/
      if(bnum.eq.1.)go to 10                                                    !/*bb4c       23*/
      nb=abs(bnum)+rang()                                                       !/*bb4c       25*/
      if(nb.eq.0)na=0                                                           !/*bb4c       26*/
      nb=max(nb,1)                                                              !/*bb4c       27*/
      if(bnum.ne.0.)wb=wgt/abs(bnum)                                            !/*bb4c       28*/
   10 rb=nb                                                                     !/*bb4c       29*/
      w0=wb                                                                     !/*bb4c       30*/
      if(numb.eq.1)wb=wb-wb*p1                                                  !/*bb4c       31*/
      ps=p1                                                                     !/*bb4c       32*/
!                                                                               !/*bb4c       33*/
!        nb = number of bremsstrahlung bias photons. nb>1 if bnum>1.            !/*bb4c       34*/
!        note nb loop done outside of gpblcm(i) save because position           !/*bb4c       35*/
!        is sampled for each photon.                                            !/*bb4c       36*/
      do 240 n1=1,nb                                                            !/*bb4c       37*/
      ie=min(n1,2)                                                              !/*bb4c       38*/
      el(ie)=0.                                                                 !/*bb4c       39*/
      if(im.eq.0)go to 16                                                       !/*bb4c       40*/
      p1=ps                                                                     !/*bb4c       41*/
      ap=ps                                                                     !/*bb4c       42*/
      fa=0.                                                                     !/*bb4c       43*/
!                                                                               !/*bb         12*/
!        prepare to make a photon.                                              !/*bb         13*/
   16 call savpar(0,3)                                                          !/*bb4c2       1*/
      npa=na                                                                    !/*bb4c       47*/
      wgt=wb                                                                    !/*bb4c       48*/
!                                                                               !/*bb         24*/
!        sample the site and get the electron energy.                           !/*bb         25*/
      fd=rang()                                                                 !/*bb         26*/
      dk=fd*d                                                                   !/*bb         27*/
      call uplpos(xxx,uold,lev,dk,vel,0)                                        !/*bb4b        3*/
      es=eg0-qo*dk-el(ie)                                                       !/*bb4c       49*/
      if(es.lt.elc(3))go to 200                                                 !/*bb4c       50*/
      m=no                                                                      !/*bb4a        5*/
      if (es.lt.eee(leee+3,m+1)) then                                           !/*bbx        11*/
   40    m=m+1                                                                  !/*bbx        12*/
         if(es.lt.eee(leee+3,m+1))go to 40                                      !/*bbx        13*/
      endif                                                                     !/*bbx        14*/
!                                                                               !/*bb         37*/
!        sample an analog photon for correct electron track energy loss.        !/*bb         38*/
      rn=rang()                                                                 !/*bbx        15*/
      pk=qpol(rn,eba(leba+1,m+nee(3)*(mkc-1)),rkt,ntop)                         !/*bbx        16*/
      pa=qpol(rn,eba(leba+1,m+1+nee(3)*(mkc-1)),rkt,ntop)                       !/*bbx        17*/
      pj=pa+(es-eee(leee+3,m+1))*(pk-pa)/(eee(leee+3,m)-eee(leee+3,m+1))        !/*bbx        18*/
      erg=pj*es                                                                 !/*bb         43*/
!                                                                               !/*bb         44*/
!        always accumulate electron track energy loss.                          !/*bb         45*/
!                                                                               !/*bb4c       53*/
!        accumulate radiative energy loss for first photon or all               !/*bb4c       54*/
!        of them for bnum<0.                                                    !/*bb4c       55*/
      el(ie)=el(ie)+erg                                                         !/*bb4c       56*/
      if(n1.gt.1.and.bnum.lt.0.)el(1)=el(1)+erg                                 !/*bb4c       57*/
!                                                                               !/*bb         47*/
!        return if no photon can be followed.                                   !/*bb         48*/
#ifdef MESHTAL                                                                  /*!bbx        19*/
      if (iphot.ne.0.or.es.lt.elc(2)) then                                      !/*bbx        20*/
!        ipt = 3, erg = photon energy, es = electron energy > erg,              !/*bbx        21*/
!        eg0 = electron energy before ionization.  For consistency              !/*bbx        22*/
!        with ionization loss, lose erg at eg0.  Consider scoring               !/*bbx        23*/
!        at es instead.                                                         !/*bbx        24*/
         ej=erg                                                                 !/*bbx        25*/
         erg=eg0                                                                !/*bbx        26*/
         if (mtlflg.ne.0.or.medflg.ne.0) then                                   !/*bbx        27*/
            do 50 i=1,6                                                         !/*bbx        28*/
            if (lev.eq.0) then                                                  !/*bbx        29*/
               zd(i)=gpblcm(i)                                                  !/*bbx        30*/
            else                                                                !/*bbx        31*/
               zd(i)=udt(i,0)                                                   !/*bbx        32*/
            endif                                                               !/*bbx        33*/
   50       continue                                                            !/*bbx        34*/
            zd(7)=wgt                                                           !/*bbx        35*/
            zd(8)=erg                                                           !/*bbx        36*/
            zd(9)=ej                                                            !/*bbx        37*/
            jz(1)=mkc                                                           !/*bbx        38*/
            jz(2)=mspf                                                          !/*bbx        39*/
            jz(5)=2                                                             !/*bbx        40*/
            call gdtal(8,zero,zd,jz)                                            !/*bbx        41*/
         endif                                                                  !/*bbx        42*/
         l=locct(llct+1,icl)                                                    !/*bbx        43*/
         if(l.ne.0)call tally(ej,1,l,d)                                         !/*bbx        44*/
         l=locct(llct+ipt,icl)                                                  !/*bbx        45*/
         if(l.ne.0)call tally(ej,2,l,d)                                         !/*bbx        46*/
         if(kdb.ne.0)return                                                     !/*bbx        47*/
         do 55 i=0,lev-1                                                        !/*bbx        48*/
            l=locct(llct+1,int(udt(7,i)))                                       !/*bbx        49*/
            if(l.ne.0)call tally(ej,1,l,d)                                      !/*bbx        50*/
            l=locct(llct+ipt,int(udt(7,i)))                                     !/*bbx        51*/
            if(l.ne.0)call tally(ej,2,l,d)                                      !/*bbx        52*/
   55       continue                                                            !/*bbx        53*/
         erg=ej                                                                 !/*bbx        54*/
         go to 200                                                              !/*bbx        55*/
      endif                                                                     !/*bbx        56*/
      if(fiml(2).eq.0..or.npa.eq.0)go to 200                                    !/*bbx        57*/
#else                                                                           /*!bbx        58*/
      if(iphot.ne.0.or.fiml(2).eq.0..or.es.lt.elc(2).or.npa.eq.0)               !/*bb4c       58*/
     & go to 200                                                                !/*bb4c       59*/
#endif                                                                          /*!bbx        59*/
!                                                                               !/*bb         50*/
      if (mbi(lmbi+mkc).ne.0) then                                              !/*bbx        60*/
!        sample a biased photon energy, if bbrem is used.                       !/*bb         51*/
         rn=rang()                                                              !/*bbx        61*/
         pk=qpol(rn,ebd(lebd+1,m+nee(3)*(mkc-1)),rkt,ntop)                      !/*bbx        62*/
         pa=qpol(rn,ebd(lebd+1,m+1+nee(3)*(mkc-1)),rkt,ntop)                    !/*bbx        63*/
         pj=pa+(es-eee(leee+3,m+1))*(pk-pa)/(eee(leee+3,m)-                     !/*bbx        64*/
     &    eee(leee+3,m+1))                                                      !/*bbx        65*/
         do 60 j1=2,mtop-1                                                      !/*bbx        66*/
   60       if(pk.le.rkt(j1))go to 70                                           !/*bbx        67*/
   70    do 80 j=2,mtop-1                                                       !/*bbx        68*/
   80       if(pa.le.rkt(j))go to 90                                            !/*bbx        69*/
   90    pb=fst(lfst+m+1+nee(3)*(mkc-1))*bbrem(j)+                              !/*bbx        70*/
     &    (es-eee(leee+3,m+1))*(fst(lfst+m+nee(3)*(mkc-1))*bbrem(j1)-           !/*bbx        71*/
     &    fst(lfst+m+1+nee(3)*(mkc-1))*bbrem(j))/                               !/*bbx        72*/
     &    (eee(leee+3,m)-eee(leee+3,m+1))                                       !/*bbx        73*/
         wgt=wgt*pb                                                             !/*bbx        74*/
         erg=pj*es                                                              !/*bbx        75*/
      endif                                                                     !/*bbx        76*/
!                                                                               !/*bb         66*/
!        return if this photon cannot be followed.                              !/*bb         67*/
      if (erg.lt.elc(2)) then                                                   !/*bbx        77*/
#ifdef MESHTAL                                                                  /*!bbx        78*/
!        ipt = 3, erg = photon energy, es = electron energy > erg,              !/*bbx        79*/
!        eg0 = electron energy before ionization.  For consistency              !/*bbx        80*/
!        with ionization loss, lose erg at eg0.  Consider scoring               !/*bbx        81*/
!        at es instead.                                                         !/*bbx        82*/
         ej=erg                                                                 !/*bbx        83*/
         erg=eg0                                                                !/*bbx        84*/
         if (mtlflg.ne.0.or.medflg.ne.0) then                                   !/*bbx        85*/
            do 100 i=1,6                                                        !/*bbx        86*/
               if (lev.eq.0) then                                               !/*bbx        87*/
                  zd(i)=gpblcm(i)                                               !/*bbx        88*/
               else                                                             !/*bbx        89*/
                  zd(i)=udt(i,0)                                                !/*bbx        90*/
               endif                                                            !/*bbx        91*/
  100          continue                                                         !/*bbx        92*/
            zd(7)=wgt                                                           !/*bbx        93*/
            zd(8)=erg                                                           !/*bbx        94*/
            zd(9)=ej                                                            !/*bbx        95*/
            jz(1)=mkc                                                           !/*bbx        96*/
            jz(2)=mspf                                                          !/*bbx        97*/
            jz(5)=2                                                             !/*bbx        98*/
            call gdtal(8,zero,zd,jz)                                            !/*bbx        99*/
         endif                                                                  !/*bbx       100*/
         l=locct(llct+1,icl)                                                    !/*bbx       101*/
         if(l.ne.0)call tally(ej,1,l,d)                                         !/*bbx       102*/
         l=locct(llct+ipt,icl)                                                  !/*bbx       103*/
         if(l.ne.0)call tally(ej,2,l,d)                                         !/*bbx       104*/
         if(kdb.ne.0)return                                                     !/*bbx       105*/
         do 110 i=0,lev-1                                                       !/*bbx       106*/
            l=locct(llct+1,int(udt(7,i)))                                       !/*bbx       107*/
            if(l.ne.0)call tally(ej,1,l,d)                                      !/*bbx       108*/
            l=locct(llct+ipt,int(udt(7,i)))                                     !/*bbx       109*/
            if(l.ne.0)call tally(ej,2,l,d)                                      !/*bbx       110*/
  110       continue                                                            !/*bbx       111*/
         erg=ej                                                                 !/*bbx       112*/
#endif                                                                          /*!bbx       113*/
         if(dbcn(20).ne.0.)go to 200                                            !/*bb4c2       3*/
      call gain_pax ( igain_photon_bremsstrahlung, 2, 0, zero, wgt*erg)         !/*bbx       114*/
      call lose_pax ( loss_all_pls_energy_cutoff, 2, 0, zero, wgt*erg)          !/*bbx       115*/
         go to 200                                                              !/*bb4c2       6*/
      endif                                                                     !/*bb4c2       7*/
      ipt=2                                                                     !/*bb4c2       8*/
      jan=0                                                                     !/*bbxc        1*/
      if(jgp.le.-2)jgp=jgp+2                                                    !/*cor4-1    164*/
      call gain_pax (igain_photon_bremsstrahlung, 2,                            !/*bbx       116*/
     &               npa, npa*wgt, npa*wgt*erg)                                 !/*bbx       117*/
      call contrib_pwb (2,ipwb_photon_bremsstrahlung,icl,npa*wgt)               !/*bbx       118*/
!                                                                               !/*bb         82*/
!        sample the photon direction.                                           !/*bb         83*/
      deb=sqrt(es*(es+2.*gpt(3)))/(es+gpt(3))                                   !/*bb         84*/
      rn=rang()                                                                 !/*bb         85*/
!                                                                               !/*bb         86*/
!        use the detailed angular distribution model ...                        !/*bb         87*/
      if(ibad.ne.0)go to 160                                                    !/*bb         88*/
      k=(m-1)/nstp+1                                                            !/*bb4c       69*/
      ek=eee(leee+3,(k-1)*nstp+1)-es                                            !/*bbx       119*/
      e1=es-eee(leee+3,k*nstp+1)                                                !/*bbx       120*/
      do 140 j=2,nwng                                                           !/*bb4c       72*/
  140 if(pj.ge.rka(j))go to 150                                                 !/*bb4c       73*/
  150 rj=rka(j-1)-pj                                                            !/*bb4c       74*/
      sj=pj-rka(j)                                                              !/*bb4c       75*/
      l=rn*(mpng-1)                                                             !/*bb4c       76*/
      pl=rn*(mpng-1)-l                                                          !/*bb4c       77*/
      ql=1.-pl                                                                  !/*bb4c       78*/
      i=lech+l+1                                                                !/*bb4c       79*/
      mc=k+(nee(3)/nstp+1)*(mkc-1)                                              !/*bbx       121*/
      t1=ql*sj*e1*ech(i,j-1,mc)+pl*rj*ek*ech(i+1,j,mc+1)+                       !/*bb4c       81*/
     & pl*sj*e1*ech(i+1,j-1,mc)+ql*rj*e1*ech(i,j,mc)+                           !/*bb4c       82*/
     & ql*sj*ek*ech(i,j-1,mc+1)+ql*rj*ek*ech(i,j,mc+1)+                         !/*bb4c       83*/
     & pl*sj*ek*ech(i+1,j-1,mc+1)+pl*rj*e1*ech(i+1,j,mc)                        !/*bb4c       84*/
      if(im.eq.0)cx=4.*t1/((rka(j-1)-rka(j))*eee(leee+3,(k-1)*nstp+1))          !/*bbx       122*/
      if(im.eq.3)cx=2.*t1/((rka(j-1)-rka(j))*                                   !/*bbx       123*/
     & (eee(leee+3,(k-1)*nstp+1)-eee(leee+3,k*nstp+1)))                         !/*bbx       124*/
      am=(cx-1.+deb)/(cx*deb+1.-deb)                                            !/*bb        106*/
      go to 170                                                                 !/*bb        107*/
!                                                                               !/*bb        108*/
!        ... or the simple model.                                               !/*bb        109*/
  160 am=(2.*rn-1.-deb)/(2.*rn*deb-1.-deb)                                      !/*bb        110*/
!                                                                               !/*bb        111*/
!        sample the photon direction relative to an intermediate                !/*bb4b        4*/
!        electron direction.  change uold for tallyd, dxtran.                   !/*bb4b        5*/
  170 u=uold(1)                                                                 !/*bb4b        6*/
      v=uold(2)                                                                 !/*bb4b        7*/
      w=uold(3)                                                                 !/*bb4b        8*/
      call escat(am,dk,fd,ae,no,2)                                              !/*bb4c       88*/
!                                                                               !/*bb        123*/
!        bank the photon.                                                       !/*bb        124*/
      vel=slite                                                                 !/*bb        125*/
      ncp=0                                                                     !/*bb        126*/
#ifdef MESHTAL                                                                  /*!bbx       125*/
      mspf=2                                                                    !/*bbx       126*/
#endif                                                                          /*!bbx       127*/
      idx=0                                                                     !/*bb        127*/
      jsu=0                                                                     !/*bb        128*/
      do 190 i=1,ndx(2)                                                         !/*bb        129*/
  190 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+                               !/*bb        130*/
     & (zzz-dxx(2,3,i))**2.lt.dxx(2,5,i))idx=i                                  !/*bb        131*/
      call bankit(16)                                                           !/*bb4a        6*/
      ipsc=13                                                                   !/*bb        132*/
      if(ndet(2).ne.0)call tallyd                                               !/*bb        133*/
      if(kdb.ne.0)return                                                        !/*bb        134*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*bb        135*/
      if(kdb.ne.0)return                                                        !/*bb        136*/
      uold(1)=u                                                                 !/*bb4b       45*/
      uold(2)=v                                                                 !/*bb4b       46*/
      uold(3)=w                                                                 !/*bb4b       47*/
!                                                                               !/*bb        142*/
  200 call getpar(0,3)                                                          !/*bb4c2       9*/
!                                                                               !/*bb        150*/
!        reduce the electron energy by the amount of the radiation loss.        !/*bb        151*/
      if(im.eq.0.or.ib.eq.0)go to 240                                           !/*bb4c       90*/
      if(numb.eq.1)wb=w0                                                        !/*bb4c       91*/
      fa=fa+1.                                                                  !/*bb4c       92*/
      p1=p1*pr/fa                                                               !/*bb4c       93*/
      ap=ap+p1                                                                  !/*bb4c       94*/
      if(r1.ge.ap)go to 16                                                      !/*bb4c       95*/
  240 continue                                                                  !/*bb4c       96*/
      if(ib.eq.0)return                                                         !/*bb4c       97*/
      if(bnum.lt.0.)el(1)=el(1)/rb                                              !/*bb4c2      10*/
      erg=erg-el(1)                                                             !/*bb4c       99*/
      call lose_pax (loss_electron_bremsstrahlung, 3, 0,zero,wgt*el(1))         !/*bbx       128*/
#ifdef MESHTAL                                                                  /*!bbx       129*/
      if(erg.ge.elc(3))return                                                   !/*bbx       130*/
!        erg = electron energy and energy lost by cutoff.                       !/*bbx       131*/
!        For consistency with ionization, lose erg at eg0.                      !/*bbx       132*/
      ej=erg                                                                    !/*bbx       133*/
      erg=eg0                                                                   !/*bbx       134*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*bbx       135*/
         do 250 i=1,6                                                           !/*bbx       136*/
         if (lev.eq.0) then                                                     !/*bbx       137*/
            zd(i)=gpblcm(i)                                                     !/*bbx       138*/
         else                                                                   !/*bbx       139*/
            zd(i)=udt(i,0)                                                      !/*bbx       140*/
         endif                                                                  !/*bbx       141*/
  250    continue                                                               !/*bbx       142*/
         zd(7)=wgt                                                              !/*bbx       143*/
         zd(8)=erg                                                              !/*bbx       144*/
         zd(9)=ej                                                               !/*bbx       145*/
         jz(1)=mkc                                                              !/*bbx       146*/
         jz(2)=mspf                                                             !/*bbx       147*/
         jz(5)=2                                                                !/*bbx       148*/
         call gdtal(8,zero,zd,jz)                                               !/*bbx       149*/
      endif                                                                     !/*bbx       150*/
      l=locct(llct+1,icl)                                                       !/*bbx       151*/
      if(l.ne.0)call tally(ej,1,l,d)                                            !/*bbx       152*/
      l=locct(llct+ipt,icl)                                                     !/*bbx       153*/
      if(l.ne.0)call tally(ej,2,l,d)                                            !/*bbx       154*/
      if(kdb.ne.0)return                                                        !/*bbx       155*/
      do 255 i=0,lev-1                                                          !/*bbx       156*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*bbx       157*/
         if(l.ne.0)call tally(ej,1,l,d)                                         !/*bbx       158*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*bbx       159*/
         if(l.ne.0)call tally(ej,2,l,d)                                         !/*bbx       160*/
  255    continue                                                               !/*bbx       161*/
      erg=ej                                                                    !/*bbx       162*/
#endif                                                                          /*!bbx       163*/
      return                                                                    !/*bb        154*/
      end                                                                       !/*bb        155*/

!#ident low_prot.F from deck xlp                                                !/*xlp         1*/
      subroutine low_prot                                                       !/*xlp         2*/
!        run one proton track in the tabular energy region.                     !/*xlp         3*/
      use global_data                                                           !/*xlp         4*/
!                                                                               !/*xlp         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*xlp         6*/
      implicit integer(kindi) (i-n)                                             !/*xlp         7*/
!                                                                               !/*xlp         8*/
#ifdef MESHTAL                                                                  /*!xlp         9*/
      dimension zd(10),jz(5)                                                    !/*xlp        10*/
#endif                                                                          /*!xlp        11*/
      external rang                                                             !/*xlp        12*/
!                                                                               !/*xlp        13*/
      lp=ltran(ipt,jan)                                                         !/*xlp        14*/
      if (lp.ne.1) then                                                         !/*xlp        15*/
         call expirx(1,'low_prot','illegal particle type.')                     !/*xlp        16*/
         return                                                                 !/*xlp        17*/
      endif                                                                     !/*xlp        18*/
!                                                                               !/*xlp        19*/
      if (erg.le.elc(ipt)) then                                                 !/*xlp        20*/
         mkc = mat(lmat+icl)                                                    !/*xlp        21*/
         jsu = 0                                                                !/*xlp        22*/
         nter = loss_all_pls_energy_cutoff                                      !/*xlp        23*/
         return                                                                 !/*xlp        24*/
      endif                                                                     !/*xlp        25*/
!                                                                               !/*xlp        26*/
!        find the energy group.                                                 !/*xlp        27*/
      ngp=1                                                                     !/*xlp        28*/
      ib=nee(ipt)                                                               !/*xlp        29*/
   10 if(ib-ngp.eq.1)go to 30                                                   !/*xlp        30*/
      ih=(ngp+ib)/2                                                             !/*xlp        31*/
      if(erg.ge.eee(leee+ipt,ih))go to 20                                       !/*xlp        32*/
      ngp=ih                                                                    !/*xlp        33*/
      go to 10                                                                  !/*xlp        34*/
   20 ib=ih                                                                     !/*xlp        35*/
      go to 10                                                                  !/*xlp        36*/
!                                                                               !/*xlp        37*/
!     Begin the track, or enter a new cell.                                     !/*xlp        38*/
!     Set up the material index and preset number of substeps.                  !/*xlp        39*/
   30 rr=0.                                                                     !/*xlp        40*/
      mkc=mat(lmat+icl)                                                         !/*xlp        41*/
      if (mkc.ne.0) then                                                        !/*xlp        42*/
!        MIPT note:  nsb() can be generalized.                                  !/*xlp        43*/
         nm=nsb(lnsb+mkc)                                                       !/*xlp        44*/
      else                                                                      !/*xlp        45*/
         nm=1                                                                   !/*xlp        46*/
      endif                                                                     !/*xlp        47*/
!                                                                               !/*xlp        48*/
!     Begin an energy step (a set of substeps).                                 !/*xlp        49*/
   40 pmf=huge                                                                  !/*xlpa        1*/
      if (mkc.eq.0) go to 50                                                    !/*xlpa        2*/
      pmf=drs(ldrs+ipt,ngp+nee_max*(mkc-1))/rho(lrho+icl)                       !/*xlpa        3*/
      qs=qav(lqav+ipt,ngp+nee_max*(mkc-1))*rho(lrho+icl)                        !/*xlpa        4*/
!                                                                               !/*xlpa        5*/
!        if istrg(ipt) < 0 use MCNPX_2.2.4 straggling model.                    !/*xlpa        6*/
      if (istrg(ipt).ge.0) go to 50                                             !/*xlpa        7*/
      q1=qs                                                                     !/*xlpa        8*/
      ch=charge(ipt)                                                            !/*xlpa        9*/
      if(dbcn(28).gt.0.)ch=one                                                  !/*xlpa       10*/
      p=pmf*nsb(lnsb+mkc)                                                       !/*xlpa       11*/
      iz=0                                                                      !/*xlpa       12*/
   45 call glando(rang,p,ch,avz(lavz+mkc),                                      !/*xlpa       13*/
     & ava(lava+mkc),den(lden+icl),erg,gpt(ipt),de,ii)                          !/*xlpa       14*/
      qs=q1+de/p                                                                !/*xlpa       15*/
      if(qs.gt.0.) go to 50                                                     !/*xlpa       16*/
      iz=iz+1                                                                   !/*xlpa       17*/
      if(iz.le.1000) go to 45                                                   !/*xlpa       18*/
      call expirx(1,'low_prot','more than 1000 resamplings of glando.')         !/*xlpa       19*/
!                                                                               !/*xlp        82*/
!     Substep loop.                                                             !/*xlp        83*/
   50 do 150 ns=nm,1,-1                                                         !/*xlpa       20*/
      if(wgt.le.0.) then                                                        !/*xlp        85*/
         call expirx(1,'low_prot',                                              !/*xlp        86*/
     &  'the weight of the current particle is zero or less.')                  !/*xlp        87*/
         return                                                                 !/*xlp        88*/
      endif                                                                     !/*xlp        89*/
      nch(ipt)=nch(ipt)+1                                                       !/*xlp        90*/
      ncp=ncp+1                                                                 !/*xlp        91*/
      n1=ngp                                                                    !/*xlp        92*/
#ifdef MESHTAL                                                                  /*!xlp        93*/
      mspf=0                                                                    !/*xlp        94*/
#endif                                                                          /*!xlp        95*/
!                                                                               !/*xlp        96*/
!        calculate distances to time and energy cutoff.                         !/*xlp        97*/
      dtc=vel*(tco(ipt)-tme)                                                    !/*xlp        98*/
      if (mkc.ne.0) then                                                        !/*xlp        99*/
         dc=(erg-elc(ipt))/qs                                                   !/*xlp       100*/
      else                                                                      !/*xlp       101*/
         dc=huge                                                                !/*xlp       102*/
      endif                                                                     !/*xlp       103*/
!                                                                               !/*xlp       104*/
!        calculate the minimum distance to event thus far                       !/*xlp       105*/
      d0=min(pmf,dtc,dc)                                                        !/*xlp       106*/
!                                                                               !/*xlp       107*/
      if (mkc.ne.0) then                                                        !/*xlp       108*/
!        calculate current projected minimum energy of track                    !/*xlp       109*/
         em=erg-d0*qs                                                           !/*xlp       110*/
!                                                                               !/*xlp       111*/
!        call sig_max to get maximum total cross section (totm) over            !/*xlp       112*/
!        energy range from em to erg. call sig_tot to set rtc array.            !/*xlp       113*/
         call sig_tot(mkc)                                                      !/*xlp       114*/
         call sig_max(mkc,em,erg)                                               !/*xlp       115*/
!                                                                               !/*xlp       116*/
!        sample distance to interaction, di, based on max cross section         !/*xlp       117*/
         di=-log(rang())/(totm*rho(lrho+icl))                                   !/*xlp       118*/
      else                                                                      !/*xlp       119*/
         di=huge                                                                !/*xlp       120*/
      endif                                                                     !/*xlp       121*/
!                                                                               !/*xlp       122*/
!     Calculate the distance to the cell boundary, dls.                         !/*xlp       123*/
      d1=min(d0,di)                                                             !/*xlp       124*/
      if(mbd(lmbd+icl).ne.0.or.jsu.ne.0)go to 60                                !/*xlp       125*/
      dls=huge                                                                  !/*xlp       126*/
      rr=rr-d1                                                                  !/*xlp       127*/
      if(rr.gt.0.)go to 70                                                      !/*xlp       128*/
      rr=dbmin()-d1                                                             !/*xlp       129*/
      if(rr.gt.0.)go to 70                                                      !/*xlp       130*/
   60 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*xlp       131*/
      call track(icl)                                                           !/*xlp       132*/
      if(kdb.ne.0)return                                                        !/*xlp       133*/
      jsu=jap                                                                   !/*xlp       134*/
   70 continue                                                                  !/*xlp       135*/
!                                                                               !/*xlp       136*/
!     Tally the track length in the cell.                                       !/*xlp       137*/
      d=min(dls,d1)                                                             !/*xlp       138*/
! DEBUG BEGIN                                                                   !/*xlp       139*/
      if(d.le.0.)then                                                           !/*xlp       140*/
         write(iuo,1)nps,ipt,jan,lp,ngp,pmf,dtc,di,dc,dls,erg                   !/*xlp       141*/
    1    format(                                                                !/*xlp       142*/
     &  ' LOW_PROT DEBUG: nps,ipt,jan,lp,ngp,pmf,dtc,di,dc,dls,erg'/            !/*xlp       143*/
     &    5i9,6e14.5)                                                           !/*xlp       144*/
         stop                                                                   !/*xlp       145*/
      endif                                                                     !/*xlp       146*/
! DEBUG END                                                                     !/*xlp       147*/
      l=locct(llct+ipt,icl)                                                     !/*xlp       148*/
      if(l.ne.0)call tally(                                                     !/*xlp       149*/
#ifdef MESHTAL                                                                  /*!xlp       150*/
     & zero,0,                                                                  !/*xlp       151*/
#endif                                                                          /*!xlp       152*/
     & l,d)                                                                     !/*xlp       153*/
      if(kdb.ne.0)return                                                        !/*xlp       154*/
      do 72 i=0,lev-1                                                           !/*xlpa       21*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*xlp       156*/
   72    if(l.ne.0)call tally(                                                  !/*xlpa       22*/
#ifdef MESHTAL                                                                  /*!xlp       158*/
     &    zero,0,                                                               !/*xlp       159*/
#endif                                                                          /*!xlp       160*/
     &    l,d)                                                                  !/*xlp       161*/
!                                                                               !/*xlp       162*/
!     Increment the summary accounts.                                           !/*xlp       163*/
      dt=d/vel                                                                  !/*xlp       164*/
      pac(kpac+ipt,5,icl)=pac(kpac+ipt,5,icl)+wgt*dt*erg                        !/*xlp       165*/
      pac(kpac+ipt,6,icl)=pac(kpac+ipt,6,icl)+wgt*d*erg                         !/*xlp       166*/
      pac(kpac+ipt,7,icl)=pac(kpac+ipt,7,icl)+d                                 !/*xlp       167*/
      if(pmf.ne.huge)pac(kpac+ipt,8,icl)=pac(kpac+ipt,8,icl)+wgt*d*pmf          !/*xlp       168*/
      pac(kpac+ipt,9,icl)=pac(kpac+ipt,9,icl)+wgt*dt                            !/*xlp       169*/
      pac(kpac+ipt,10,icl)=pac(kpac+ipt,10,icl)+wgt*d                           !/*xlp       170*/
#ifdef MESHTAL                                                                  /*!xlp       171*/
      if(mkc.eq.0) then                                                         !/*xlp       172*/
         if (mtlflg.ne.0) then                                                  !/*xlp       173*/
            do 74 i=1,6                                                         !/*xlp       174*/
            if (lev.eq.0) then                                                  !/*xlp       175*/
               zd(i)=gpblcm(i)                                                  !/*xlp       176*/
            else                                                                !/*xlp       177*/
               zd(i)=udt(i,0)                                                   !/*xlp       178*/
            endif                                                               !/*xlp       179*/
   74       continue                                                            !/*xlp       180*/
            zd(7)=wgt                                                           !/*xlp       181*/
            zd(8)=erg                                                           !/*xlp       182*/
            zd(9)=zero                                                          !/*xlp       183*/
            jz(1)=0                                                             !/*xlp       184*/
            jz(2)=mspf                                                          !/*xlp       185*/
            jz(5)=0                                                             !/*xlp       186*/
            call gdtal(2,d,zd,jz)                                               !/*xlp       187*/
         endif                                                                  !/*xlp       188*/
      endif                                                                     !/*xlp       189*/
#endif                                                                          /*!xlp       190*/
!                                                                               !/*xlp       191*/
!     Scatter the particle.                                                     !/*xlp       192*/
      uold(1)=uuu                                                               !/*xlp       193*/
      uold(2)=vvv                                                               !/*xlp       194*/
      uold(3)=www                                                               !/*xlp       195*/
      eg0=erg                                                                   !/*xlp       196*/
      if(mkc.ne.0) then                                                         !/*xlp       197*/
         call mscat3(lp,d,arg(larg+mkc),erg,th)                                 !/*xlp       198*/
         call rotas(cos(th),uold,uuu,lev,irt)                                   !/*xlp       199*/
!                                                                               !/*xlpa       23*/
!        Calculate energy loss to ionization along the track, ei.               !/*xlpa       24*/
         ei=d*qs                                                                !/*xlpa       25*/
         if (istrg(ipt).eq.0) then                                              !/*xlpa       26*/
!                                                                               !/*xlpa       27*/
!           First get better estimate of CSDA over multiple groups.             !/*xlpa       28*/
            e = get_csda_energy (eg0, d)                                        !/*xlpa       29*/
!                                                                               !/*xlpa       30*/
!           Now get straggling around CSDA value.                               !/*xlpa       31*/
            iz = 0                                                              !/*xlpk        1*/
   75       call glando(rang,d,charge(ipt),avz(lavz+mkc),                       !/*xlpa       33*/
     &              ava(lava+mkc),den(lden+icl),eg0,gpt(ipt),de,ii)             !/*xlpa       34*/
!                                                                               !/*xlpa       35*/
            ei = eg0 - e + de                                                   !/*xlpa       36*/
            if (ei.le.zero) then                                                !/*xlpa       37*/
               iz = iz + 1                                                      !/*xlpk        2*/
               if(iz.gt.1000)call expirx(1,'low_prot',                          !/*xlpa       39*/
     &               'more than 1000 resamplings of glando.')                   !/*xlpa       40*/
               go to 75                                                         !/*xlpa       41*/
            endif                                                               !/*xlpa       42*/
         endif                                                                  !/*xlpa       43*/
!                                                                               !/*xlpa       44*/
#ifdef MESHTAL                                                                  /*!xlpa       45*/
!                                                                               !/*xlpa       46*/
!        Tally ionization energy deposition to mesh and type 6 tallies.         !/*xlpa       47*/
          fh=d*heatf(mkc,0)*rho(lrho+icl)                                       !/*xlp       201*/
      fi=ei                                                                     !/*xlpa       48*/
          fd=fh+fi                                                              !/*xlp       203*/
          erg=eg0                                                               !/*xlp       204*/
          l=locct(llct+1,icl)                                                   !/*xlp       205*/
          if(l.ne.0)call tally(fd,1,l,d)                                        !/*xlp       206*/
          l=locct(llct+ipt,icl)                                                 !/*xlp       207*/
          if(l.ne.0)call tally(fi,2,l,d)                                        !/*xlp       208*/
          if(kdb.ne.0)return                                                    !/*xlp       209*/
          do 76 i=0,lev-1                                                       !/*xlp       210*/
             l=locct(llct+1,int(udt(7,i)))                                      !/*xlp       211*/
             if(l.ne.0)call tally(fd,1,l,d)                                     !/*xlp       212*/
             l=locct(llct+ipt,int(udt(7,i)))                                    !/*xlp       213*/
             if(l.ne.0)call tally(fi,2,l,d)                                     !/*xlp       214*/
   76     continue                                                              !/*xlp       215*/
          if (mtlflg.ne.0.or.medflg.ne.0.and.fd.ne.0.) then                     !/*xlp       216*/
             do 78 i=1,6                                                        !/*xlp       217*/
                if (lev.eq.0) then                                              !/*xlp       218*/
                   zd(i)=gpblcm(i)                                              !/*xlp       219*/
                else                                                            !/*xlp       220*/
                   zd(i)=udt(i,0)                                               !/*xlp       221*/
                endif                                                           !/*xlp       222*/
   78        continue                                                           !/*xlp       223*/
             zd(7)=wgt                                                          !/*xlp       224*/
             zd(8)=erg                                                          !/*xlp       225*/
             zd(9)=fd                                                           !/*xlp       226*/
             jz(1)=mkc                                                          !/*xlp       227*/
             jz(2)=mspf                                                         !/*xlp       228*/
             jz(5)=2                                                            !/*xlp       229*/
             call gdtal(2,d,zd,jz)                                              !/*xlp       230*/
          endif                                                                 !/*xlp       231*/
#endif                                                                          /*!xlp       232*/
!                                                                               !/*xlp       233*/
!           lose energy to ionization along the track.                          !/*xlp       234*/
         erg=eg0-ei                                                             !/*xlpa       49*/
!                                                                               !/*xlpa       50*/
         if (erg.le.elc(ipt)) then                                              !/*xlpa       51*/
            call uplpos(xxx,uold,lev,d,vel,1)                                   !/*xlpa       52*/
            erg=elc(ipt)                                                        !/*xlpa       53*/
            call lose_pax (loss_gen_chg_multiple_scatter, ipt,                  !/*xlpa       54*/
     &                     0, zero, wgt*(eg0-erg))                              !/*xlpa       55*/
            jsu=0                                                               !/*xlpa       56*/
            nter = loss_all_pls_energy_cutoff                                   !/*xlpa       57*/
            return                                                              !/*xlpa       58*/
         else                                                                   !/*xlpa       59*/
            call lose_pax (loss_gen_chg_multiple_scatter, ipt,                  !/*xlpa       60*/
     &                     0, zero, wgt*ei)                                     !/*xlpa       61*/
         endif                                                                  !/*xlpa       62*/
!                                                                               !/*xlp       247*/
         if(erg.lt.eee(leee+ipt,ngp+1)) then                                    !/*xlp       248*/
  100       ngp=ngp+1                                                           !/*xlp       249*/
            if(erg.lt.eee(leee+ipt,ngp+1))go to 100                             !/*xlp       250*/
         endif                                                                  !/*xlp       251*/
      endif                                                                     !/*xlp       252*/
!                                                                               !/*xlp       253*/
!     Move particle to surface, substep, interaction, or termination.           !/*xlp       254*/
!     Produce secondary particles.                                              !/*xlp       255*/
!                                                                               !/*xlp       256*/
      call uplpos(xxx,uold,lev,d,vel,1)                                         !/*xlp       257*/
      if (d.eq.di) then                                                         !/*xlp       258*/
         ec=erg                                                                 !/*xlp       259*/
         wt=wgt                                                                 !/*xlp       260*/
         jsu=0                                                                  !/*xlp       261*/
!                                                                               !/*xlp       262*/
!        save maximum cross section as xx                                       !/*xlp       263*/
         xx=totm                                                                !/*xlp       264*/
!                                                                               !/*xlp       265*/
!        call sig_tot to get actual cross sections at erg                       !/*xlp       266*/
         call sig_tot(mkc)                                                      !/*xlp       267*/
!                                                                               !/*xlp       268*/
!        Decide whether collision was real or pseudo.                           !/*xlp       269*/
         if (rang()*xx.le.totm) then                                            !/*xlp       270*/
!           Real collision.  Process by cp_coll.                                !/*xlp       271*/
            call cp_coll                                                        !/*xlp       272*/
!                                                                               !/*xlp       273*/
            if (ixre.eq.0) then                                                 !/*xlp       274*/
!              IXRE == 0 if elastic scattering was sampled in cp_coll.          !/*xlp       275*/
               call lose_pax (loss_gen_chg_elastic_scatter, ipt,                !/*xlp       276*/
     &                        0, zero, wgt*(ec-erg))                            !/*xlp       277*/
               if (erg.le.elc(ipt)) then                                        !/*xlp       278*/
                  jsu=0                                                         !/*xlp       279*/
                  nter = loss_all_pls_energy_cutoff                             !/*xlp       280*/
                  return                                                        !/*xlp       281*/
               else if(erg.lt.eee(leee+ipt,ngp+1)) then                         !/*xlp       282*/
  130             ngp=ngp+1                                                     !/*xlp       283*/
                  if(erg.lt.eee(leee+ipt,ngp+1))go to 130                       !/*xlp       284*/
               endif                                                            !/*xlp       285*/
            else                                                                !/*xlp       286*/
!              ASSERT (NTER has been set .ne. 0 in cp_coll)                     !/*xlp       287*/
               erg=ec                                                           !/*xlp       288*/
               wgt=wt                                                           !/*xlp       289*/
               return                                                           !/*xlp       290*/
            endif                                                               !/*xlp       291*/
! ---                                                                           !/*xlp       292*/
! ---    else                                                                   !/*xlp       293*/
! ---       Pseudo collision.                                                   !/*xlp       294*/
! ---       Could count pseudocollisions here.                                  !/*xlp       295*/
! ---                                                                           !/*xlp       296*/
         endif                                                                  !/*xlp       297*/
      endif                                                                     !/*xlp       298*/
!                                                                               !/*xlp       299*/
!     Terminate to time cutoff.                                                 !/*xlp       300*/
      if(d.eq.dtc) then                                                         !/*xlp       301*/
         nter = loss_all_pls_time_cutoff                                        !/*xlp       302*/
         return                                                                 !/*xlp       303*/
      endif                                                                     !/*xlp       304*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                      !/*xlp       305*/
!                                                                               !/*xlp       306*/
!     Process the end of the substep.                                           !/*xlp       307*/
      if(d.lt.dls) then                                                         !/*xlp       308*/
!        particle remains in the same cell.                                     !/*xlp       309*/
         pac(kpac+ipt,3,icl)=pac(kpac+ipt,3,icl)+1.                             !/*xlp       310*/
         pac(kpac+ipt,4,icl)=pac(kpac+ipt,4,icl)+wgt                            !/*xlp       311*/
         if(nmaz.ne.0)call rslmaz(3,1)                                          !/*xlp       312*/
         jsu=0                                                                  !/*xlp       313*/
!        play the weight-window and energy-splitting games.                     !/*xlp       314*/
         if(wwp(ipt,4).ne.0..and.ns.eq.2)call wtwndo(4)                         !/*xlp       315*/
         if(nter.ne.0)return                                                    !/*xlp       316*/
         if(espl(ipt,1).ne.0.)call ergimp                                       !/*xlp       317*/
         if(nter.ne.0)return                                                    !/*xlp       318*/
      else                                                                      !/*xlp       319*/
!        see if deflection keeps particle in the same cell.                     !/*xlp       320*/
         call echkcl(j)                                                         !/*xlp       321*/
         if(j.ne.0) then                                                        !/*xlp       322*/
!           shove the particle thru the cell boundary.                          !/*xlp       323*/
            call surfac                                                         !/*xlp       324*/
            if(krflg.ne.0)call eventp(3)                                        !/*xlp       325*/
            if(iptr.ne.0)call ptrak(3)                                          !/*xlp       326*/
            if(kdb.ne.0)return                                                  !/*xlp       327*/
            if(nter.ne.0)return                                                 !/*xlp       328*/
            go to 30                                                            !/*xlp       329*/
         endif                                                                  !/*xlp       330*/
      endif                                                                     !/*xlp       331*/
!                                                                               !/*xlp       332*/
!     Complete energy substep for particle remaining in cell icl.               !/*xlp       333*/
  150 if(n1.ne.ngp)go to 40                                                     !/*xlp       334*/
      go to 40                                                                  !/*xlp       335*/
!                                                                               !/*xlp       336*/
      end                                                                       !/*xlp       337*/

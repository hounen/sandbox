!#ident neu_pl.F from deck xnp                                                  !/*xnp         1*/
      subroutine neu_pl                                                         !/*xnp         2*/
!        run one neutral particle track.                                        !/*xnp         3*/
      use global_data                                                           !/*xnp         4*/
!                                                                               !/*xnp         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*xnp         6*/
      implicit integer(kindi) (i-n)                                             !/*xnp         7*/
!                                                                               !/*xnp         8*/
#include <globals/interact_outcomes.h>                                          /*!xnp         9*/
#ifdef MESHTAL                                                                  /*!xnp        10*/
      dimension zd(10),jz(5)                                                    !/*xnp        11*/
      fd=0.                                                                     !/*xnp        12*/
#endif                                                                          /*!xnp        13*/
!     external rang                                                             !/*xnp        14*/
!                                                                               !/*xnp        15*/
      if (ntasks.gt.1) then                                                     !/*xnpk        1*/
         call expirx(1,'chg_pl',                                                !/*xnpk        2*/
     &   'threading not available above tabular region.')                       !/*xnpk        3*/
         return                                                                 !/*xnpk        4*/
      endif                                                                     !/*xnpk        5*/
      lp=ltran(ipt,jan)                                                         !/*xnp        16*/
      if (lp.eq.0) then                                                         !/*xnp        17*/
         call expirx(1,'neu_pl','illegal particle type.')                       !/*xnp        18*/
         return                                                                 !/*xnp        19*/
      endif                                                                     !/*xnp        20*/
!                                                                               !/*xnp        21*/
      if (erg.le.elc(ipt)) then                                                 !/*xnp        22*/
         mkc=mat(lmat+icl)                                                      !/*xnp        23*/
         nb=3                                                                   !/*xnp        24*/
         go to 500                                                              !/*xnp        25*/
      endif                                                                     !/*xnp        26*/
!                                                                               !/*xnp        27*/
!        find the energy group.                                                 !/*xnp        28*/
      ngp=1                                                                     !/*xnp        29*/
      ib=nee(ipt)                                                               !/*xnp        30*/
   10 if(ib-ngp.eq.1)go to 30                                                   !/*xnp        31*/
      ih=(ngp+ib)/2                                                             !/*xnp        32*/
      if(erg.ge.eee(leee+ipt,ih))go to 20                                       !/*xnp        33*/
      ngp=ih                                                                    !/*xnp        34*/
      go to 10                                                                  !/*xnp        35*/
   20 ib=ih                                                                     !/*xnp        36*/
      go to 10                                                                  !/*xnp        37*/
!                                                                               !/*xnp        38*/
!        begin track, or enter new cell.  set up material index.                !/*xnp        39*/
   30 rr=0.                                                                     !/*xnp        40*/
      mkc=mat(lmat+icl)                                                         !/*xnp        41*/
!                                                                               !/*xnp        42*/
! ...    begin an energy step.                                                  !/*xnp        43*/
   40 continue                                                                  !/*xnpa        1*/
!     ... Perform possible future energy-group related operations here.         !/*xnpa        2*/
!     ... For neutral particles, currently steps == substeps.                   !/*xnpa        3*/
!     ... Possible future substep loop: do 150 ns=nm,1,-1                       !/*xnpa        4*/
!                                                                               !/*xnp        46*/
      if (wgt.le.0.) then                                                       !/*xnp        48*/
         call expirx(1,'neu_pl',                                                !/*xnp        49*/
     &  'the weight of the current particle is zero or less.')                  !/*xnp        50*/
         return                                                                 !/*xnp        51*/
      endif                                                                     !/*xnp        52*/
      n1=ngp                                                                    !/*xnp        53*/
!                                                                               !/*xnp        54*/
!        calculate the distance to time cutoff.                                 !/*xnp        55*/
      dtc=vel*(tco(ipt)-tme)                                                    !/*xnp        56*/
!                                                                               !/*xnp        57*/
!        calculate the distance to interaction.                                 !/*xnp        58*/
      if (ipt.eq.1.and.jan.eq.1) then                                           !/*xnp        60*/
         call ace_tot(erg,lp,mkc,nel(lnel+1),mel(lmel+1),                       !/*xnp        61*/
     &    aa9(laa9+1),zz9(lzz9+1),fmf(lfmf+1),                                  !/*xnp        62*/
     &    sigg(lsgg+1,1),hsigg(lhsg+1,1),xsiso(lxso+1,1),                       !/*xnp        63*/
     &    esxmp(lesp+1),esxln(lexn+1,1),rho(lrho+icl),di)                       !/*xnp        64*/
      else                                                                      !/*xnp        65*/
         call get_tot(erg,lp,mkc,nel(lnel+1),mel(lmel+1),                       !/*xnp        66*/
#ifdef CEM                                                                      /*!xnp        67*/
     &    ipnf(lipnf+1),                                                        !/*xnp        68*/
#endif                                                                          /*!xnp        69*/
     &    aa9(laa9+1),zz9(lzz9+1),fmf(lfmf+1),                                  !/*xnp        70*/
     &    sigg(lsgg+1,1),hsigg(lhsg+1,1),xsiso(lxso+1,1),                       !/*xnp        71*/
#ifdef CEM                                                                      /*!xnp        72*/
     &    epnmin(lepnm+1),                                                      !/*xnp        73*/
#endif                                                                          /*!xnp        74*/
     &    rho(lrho+icl),di)                                                     !/*xnp        75*/
      endif                                                                     !/*xnp        76*/
      if (lcaopt(8).eq.-1) then                                                 !/*xnp        77*/
         d=zero                                                                 !/*xnp        78*/
         di=zero                                                                !/*xnp        79*/
         go to 101                                                              !/*xnp        80*/
      endif                                                                     !/*xnp        81*/
!                                                                               !/*xnp        82*/
!        calculate the distance to the cell boundary, dls.                      !/*xnp        83*/
      d1=min(dtc,di)                                                            !/*xnp        84*/
      if(mbd(lmbd+icl).ne.0.or.jsu.ne.0)go to 60                                !/*xnp        85*/
      dls=huge                                                                  !/*xnp        86*/
      rr=rr-d1                                                                  !/*xnp        87*/
      if(rr.gt.0.)go to 70                                                      !/*xnp        88*/
      rr=dbmin()-d1                                                             !/*xnp        89*/
      if(rr.gt.0.)go to 70                                                      !/*xnp        90*/
   60 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*xnp        91*/
      call track(icl)                                                           !/*xnp        92*/
      if(kdb.ne.0)return                                                        !/*xnp        93*/
      jsu=jap                                                                   !/*xnp        94*/
   70 continue                                                                  !/*xnp        95*/
!                                                                               !/*xnp        96*/
!        tally the track length in the cell.                                    !/*xnp        97*/
      d=min(dls,d1)                                                             !/*xnp        98*/
! DEBUG BEGIN                                                                   !/*xnp        99*/
      if (d.le.0.) then                                                         !/*xnp       100*/
         write(iuo,1)nps,ipt,jan,lp,ngp,dtc,di,dls,erg                          !/*xnp       101*/
    1    format(' NEU_PL DEBUG: nps,ipt,jan,lp,ngp,dtc,di,dls,erg'/             !/*xnp       102*/
     &    5i9,4e14.5)                                                           !/*xnp       103*/
         stop                                                                   !/*xnp       104*/
      endif                                                                     !/*xnp       105*/
! DEBUG END                                                                     !/*xnp       106*/
      l=locct(llct+ipt,icl)                                                     !/*xnp       107*/
      if(l.ne.0)call tally(                                                     !/*xnp       108*/
#ifdef MESHTAL                                                                  /*!xnp       109*/
     & zero,0,                                                                  !/*xnp       110*/
#endif                                                                          /*!xnp       111*/
     & l,d)                                                                     !/*xnp       112*/
      if(kdb.ne.0)return                                                        !/*xnp       113*/
      do 75 i=0,lev-1                                                           !/*xnp       114*/
      l=locct(llct+ipt,int(udt(7,i)))                                           !/*xnp       115*/
   75 if(l.ne.0)call tally(                                                     !/*xnp       116*/
#ifdef MESHTAL                                                                  /*!xnp       117*/
     & zero,0,                                                                  !/*xnp       118*/
#endif                                                                          /*!xnp       119*/
     & l,d)                                                                     !/*xnp       120*/
!                                                                               !/*xnp       121*/
!        increment the summary accounts.                                        !/*xnp       122*/
      dt=d/vel                                                                  !/*xnp       123*/
      pac(kpac+ipt,5,icl)=pac(kpac+ipt,5,icl)+wgt*dt*erg                        !/*xnp       124*/
      pac(kpac+ipt,6,icl)=pac(kpac+ipt,6,icl)+wgt*d*erg                         !/*xnp       125*/
      pac(kpac+ipt,7,icl)=pac(kpac+ipt,7,icl)+d                                 !/*xnp       126*/
      pac(kpac+ipt,9,icl)=pac(kpac+ipt,9,icl)+wgt*dt                            !/*xnp       127*/
      pac(kpac+ipt,10,icl)=pac(kpac+ipt,10,icl)+wgt*d                           !/*xnp       128*/
#ifdef MESHTAL                                                                  /*!xnp       129*/
      if (mtlflg.ne.0) then                                                     !/*xnp       130*/
         do 78 i=1,6                                                            !/*xnp       131*/
            if (lev.eq.0) then                                                  !/*xnp       132*/
               zd(i)=gpblcm(i)                                                  !/*xnp       133*/
            else                                                                !/*xnp       134*/
               zd(i)=udt(i,0)                                                   !/*xnp       135*/
            endif                                                               !/*xnp       136*/
   78    continue                                                               !/*xnp       137*/
         zd(7)=wgt                                                              !/*xnp       138*/
         zd(8)=erg                                                              !/*xnp       139*/
         zd(9)=zero                                                             !/*xnp       140*/
         jz(1)=mat(lmat+icl)                                                    !/*xnp       141*/
         jz(2)=mspf                                                             !/*xnp       142*/
         jz(5)=4                                                                !/*xnp       143*/
         call gdtal(2,d,zd,jz)                                                  !/*xnp       144*/
      endif                                                                     !/*xnp       145*/
#endif                                                                          /*!xnp       146*/
!                                                                               !/*xnp       147*/
      uold(1)=uuu                                                               !/*xnp       148*/
      uold(2)=vvv                                                               !/*xnp       149*/
      uold(3)=www                                                               !/*xnp       150*/
!                                                                               !/*xnp       151*/
!        move particle to surface, interaction, or termination.                 !/*xnp       152*/
!        produce secondary particles.                                           !/*xnp       153*/
!                                                                               !/*xnp       154*/
      tme=tme+dt                                                                !/*xnp       155*/
      xxx=xxx+uuu*d                                                             !/*xnp       156*/
      yyy=yyy+vvv*d                                                             !/*xnp       157*/
      zzz=zzz+www*d                                                             !/*xnp       158*/
      do 100 l=0,lev-1                                                          !/*xnp       159*/
      udt(1,l)=udt(1,l)+udt(4,l)*d                                              !/*xnp       160*/
      udt(2,l)=udt(2,l)+udt(5,l)*d                                              !/*xnp       161*/
  100 udt(3,l)=udt(3,l)+udt(6,l)*d                                              !/*xnp       162*/
      eg0=erg                                                                   !/*xnp       163*/
  101 continue                                                                  !/*xnp       164*/
      if (d.eq.di) then                                                         !/*xnp       165*/
         nch(ipt)=nch(ipt)+1                                                    !/*xnp       166*/
         ncp=ncp+1                                                              !/*xnp       167*/
#ifdef MESHTAL                                                                  /*!xnp       168*/
         mspf=0                                                                 !/*xnp       169*/
#endif                                                                          /*!xnp       170*/
         ec=erg                                                                 !/*xnp       171*/
         wt=wgt                                                                 !/*xnp       172*/
         jsu=0                                                                  !/*xnp       173*/
!        Convert from shakes to nsec.                                           !/*xnp       174*/
         tc=ten*tme                                                             !/*xnp       175*/
         call interact(1,lp,erg,elc(ipt),icl,mkc,0,tc,uuu,vvv,www,wgt,          !/*xnp       176*/
     &    xxx,yyy,zzz,nel(lnel+1),mel(lmel+1),aa9(laa9+1),zz9(lzz9+1),          !/*xnp       177*/
     &    iazdex(liax+1),fmf(lfmf+1),rho(lrho+1),pcap(lpcp+1),                  !/*xnp       178*/
     &    hsigg(lhsg+1,1),xsiso(lxso+1,1),esxlp(lexp+1,1),                      !/*xnp       179*/
#ifdef HISTP                                                                    /*!xnp       180*/
     &    npstc,                                                                !/*xnp       181*/
#endif                                                                          /*!xnp       182*/
#ifdef MESHTAL                                                                  /*!xnp       183*/
     &    eg_rc,eg_nt,                                                          !/*xnp       184*/
#endif                                                                          /*!xnp       185*/
     &    nr)                                                                   !/*xnp       186*/
#ifdef MESHTAL                                                                  /*!xnp       187*/
         ej=erg                                                                 !/*xnp       188*/
         wj=wgt                                                                 !/*xnp       189*/
         erg=ec                                                                 !/*xnp       190*/
         wgt=wt                                                                 !/*xnp       191*/
         fd=eg_rc+eg_nt                                                         !/*xnp       192*/
         l=locct(llct+1,icl)                                                    !/*xnp       193*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*xnp       194*/
         l=locct(llct+ipt,icl)                                                  !/*xnp       195*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*xnp       196*/
         if(kdb.ne.0)return                                                     !/*xnp       197*/
         do 105 i=0,lev-1                                                       !/*xnp       198*/
            l=locct(llct+1,int(udt(7,i)))                                       !/*xnp       199*/
            if(l.ne.0)call tally(fd,1,l,d)                                      !/*xnp       200*/
            l=locct(llct+ipt,int(udt(7,i)))                                     !/*xnp       201*/
            if(l.ne.0)call tally(fd,2,l,d)                                      !/*xnp       202*/
  105    continue                                                               !/*xnp       203*/
         if (mtlflg.ne.0.or.medflg.ne.0) then                                   !/*xnp       204*/
            do 110 i=1,6                                                        !/*xnp       205*/
               if (lev.eq.0) then                                               !/*xnp       206*/
                  zd(i)=gpblcm(i)                                               !/*xnp       207*/
               else                                                             !/*xnp       208*/
                  zd(i)=udt(i,0)                                                !/*xnp       209*/
               endif                                                            !/*xnp       210*/
  110       continue                                                            !/*xnp       211*/
            zd(7)=wgt                                                           !/*xnp       212*/
            zd(8)=erg                                                           !/*xnp       213*/
            zd(9)=eg_rc                                                         !/*xnp       214*/
            jz(1)=mkc                                                           !/*xnp       215*/
            jz(2)=mspf                                                          !/*xnp       216*/
            jz(5)=3                                                             !/*xnp       217*/
            call gdtal(8,zero,zd,jz)                                            !/*xnp       218*/
            zd(9)=eg_nt                                                         !/*xnp       219*/
            jz(5)=5                                                             !/*xnp       220*/
            call gdtal(8,zero,zd,jz)                                            !/*xnp       221*/
         endif                                                                  !/*xnp       222*/
         erg=ej                                                                 !/*xnp       223*/
         wgt=wj                                                                 !/*xnp       224*/
#endif                                                                          /*!xnp       225*/
         if (nr.eq.id_nonelastic_collisn_terminal) then                         !/*xnp       226*/
            if (ipt.eq.1) then                                                  !/*xnp       227*/
               nter = loss_neutron_nucl_interaction                             !/*xnp       228*/
            else                                                                !/*xnp       229*/
               nter = loss_gen_neu_nucl_interaction                             !/*xnp       230*/
            endif                                                               !/*xnp       231*/
            erg=ec                                                              !/*xnp       232*/
            wgt=wt                                                              !/*xnp       233*/
            return                                                              !/*xnp       234*/
         else if (nr.eq.id_elastic_collisn_nonterminal) then                    !/*xnp       235*/
            if (ipt.eq.1) then                                                  !/*xnp       236*/
               call lose_pax (loss_neutron_downscattering, 1,                   !/*xnp       237*/
     &                        0, zero, wgt*(ec-erg))                            !/*xnp       238*/
               if (wgt.lt.wt) then                                              !/*xnp       239*/
                  call lose_pax (loss_neutron_nucl_interaction, 1,              !/*xnp       240*/
     &                           0, wt-wgt, (wt-wgt)*ec)                        !/*xnp       241*/
                  call contrib_pwb (1,ipwb_neutron_nuc_interaction,icl,         !/*xnp       242*/
     &                              wgt-wt)                                     !/*xnp       243*/
!                 QUESTION:  play weight-cutoff game???                         !/*xnp       244*/
               endif                                                            !/*xnp       245*/
            else                                                                !/*xnp       246*/
               call lose_pax (loss_gen_neu_elastic_scatter, ipt,                !/*xnp       247*/
     &                        0, zero, wgt*(ec-erg))                            !/*xnp       248*/
               if (wgt.lt.wt) then                                              !/*xnp       249*/
                  call lose_pax (loss_gen_neu_nucl_interaction, ipt,            !/*xnp       250*/
     &                           0, wt-wgt, (wt-wgt)*ec)                        !/*xnp       251*/
                  call contrib_pwb (ipt,ipwb_gen_neu_nuc_interaction,           !/*xnp       252*/
     &                              icl,wgt-wt)                                 !/*xnp       253*/
!                 QUESTION:  play weight-cutoff game???                         !/*xnp       254*/
               endif                                                            !/*xnp       255*/
            endif                                                               !/*xnp       256*/
            if (erg.le.elc(ipt)) then                                           !/*xnp       257*/
               nb=3                                                             !/*xnp       258*/
               go to 500                                                        !/*xnp       259*/
            else if (ipt.eq.1.and.jan.eq.1.and.erg.le.cut_n) then               !/*xnp       260*/
!IDX                                                                            !/*xnp       261*/
               idx=0                                                            !/*xnp       262*/
               do 125 j=1,ndx(1)                                                !/*xnp       263*/
                  if((xxx-dxx(1,1,j))**2+(yyy-dxx(1,2,j))**2+                   !/*xnp       264*/
     &             (zzz-dxx(1,3,j))**2.lt.dxx(1,5,j))idx=j                      !/*xnp       265*/
  125          continue                                                         !/*xnp       266*/
!IDX                                                                            !/*xnp       267*/
               call gain_pax (igain_neutron_tabular_boundary, 1,                !/*xnp       268*/
     &                        1, wgt, wgt*erg)                                  !/*xnp       269*/
               call contrib_pwb (1,ipwb_neutron_tabular_bound,icl,wgt)          !/*xnp       270*/
               call bankit(32)                                                  !/*xnp       271*/
               nter = loss_neutron_tabular_boundary                             !/*xnp       272*/
               return                                                           !/*xnp       273*/
            else if (erg.lt.eee(leee+ipt,ngp+1)) then                           !/*xnp       274*/
  130          ngp=ngp+1                                                        !/*xnp       275*/
               if(erg.lt.eee(leee+ipt,ngp+1))go to 130                          !/*xnp       276*/
            endif                                                               !/*xnp       277*/
         else if (nr.eq.id_true_pseudo_nonterminal) then                        !/*xnp       278*/
            continue                                                            !/*xnp       279*/
!           count pseudocollisions.                                             !/*xnp       280*/
         else if (nr.eq.id_fulldecay_pseudo_terminal) then                      !/*xnp       281*/
            if (ipt.ne.1) then                                                  !/*xnp       282*/
               nter = loss_gen_neu_particle_decay                               !/*xnp       283*/
            else                                                                !/*xnp       284*/
!              QUESTION:  Revisit this specification.  In particular, is        !/*xnp       285*/
!                         this really decay (not yet done for neutrons),        !/*xnp       286*/
!                         or does it model antineutron annihilation???          !/*xnp       287*/
               nter = loss_neutron_particle_decay                               !/*xnp       288*/
            endif                                                               !/*xnp       289*/
            erg=ec                                                              !/*xnp       290*/
            wgt=wt                                                              !/*xnp       291*/
            return                                                              !/*xnp       292*/
         else if (nr.eq.id_partdecay_pseudo_nonterminal) then                   !/*xnp       293*/
            if (ipt.ne.1) then                                                  !/*xnp       294*/
               call lose_pax (loss_gen_neu_particle_decay, ipt,                 !/*xnp       295*/
     &                        0, wt-wgt, (wt-wgt)*erg)                          !/*xnp       296*/
               call contrib_pwb (ipt,ipwb_gen_neu_particle_decay,icl,           !/*xnp       297*/
     &                           wgt-wt)                                        !/*xnp       298*/
            else                                                                !/*xnp       299*/
!              QUESTION:  Revisit this specification.  In particular, is        !/*xnp       300*/
!                         this really decay (not yet done for neutrons),        !/*xnp       301*/
!                         or does it model antineutron annihilation???          !/*xnp       302*/
               call lose_pax (loss_neutron_particle_decay, 1,                   !/*xnp       303*/
     &                        0, wt-wgt, (wt-wgt)*erg)                          !/*xnp       304*/
               call contrib_pwb (1,ipwb_neutron_particle_decay,icl,             !/*xnp       305*/
     &                           wgt-wt)                                        !/*xnp       306*/
            endif                                                               !/*xnp       307*/
!           QUESTION:  Play weight-cutoff game???                               !/*xnp       308*/
         else                                                                   !/*xnp       309*/
            call expirx(1,'neu_pl',                                             !/*xnp       310*/
     &     'bad result from interact for particle at collision.')               !/*xnp       311*/
         endif                                                                  !/*xnp       312*/
      endif                                                                     !/*xnp       313*/
!                                                                               !/*xnp       314*/
!        terminate to time cutoff.                                              !/*xnp       315*/
      if (d.eq.dtc) then                                                        !/*xnp       316*/
         nter = loss_all_pls_time_cutoff                                        !/*xnp       317*/
         return                                                                 !/*xnp       318*/
      endif                                                                     !/*xnp       319*/
      if (gpt(ipt).ne.zero) then                                                !/*xnp       320*/
         vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                   !/*xnp       321*/
      else                                                                      !/*xnp       322*/
         vel=slite                                                              !/*xnp       323*/
      endif                                                                     !/*xnp       324*/
!                                                                               !/*xnp       325*/
!        process the end of the step.                                           !/*xnp       326*/
      if (d.lt.dls) then                                                        !/*xnp       327*/
!        particle remains in the same cell.                                     !/*xnp       328*/
         pac(kpac+ipt,3,icl)=pac(kpac+ipt,3,icl)+1.                             !/*xnp       329*/
         pac(kpac+ipt,4,icl)=pac(kpac+ipt,4,icl)+wgt                            !/*xnp       330*/
         if(nmaz.ne.0)call rslmaz(3,1)                                          !/*xnp       331*/
         jsu=0                                                                  !/*xnp       332*/
!        Play the weight-window and energy-splitting games.                     !/*xnp       333*/
         if(wwp(ipt,4).ne.0.)call wtwndo(4)                                     !/*xnp       334*/
         if(nter.ne.0)return                                                    !/*xnp       335*/
         if(espl(ipt,1).ne.0.)call ergimp                                       !/*xnp       336*/
         if(nter.ne.0)return                                                    !/*xnp       337*/
      else                                                                      !/*xnp       338*/
!        shove the particle thru the cell boundary.                             !/*xnp       339*/
         call surfac                                                            !/*xnp       340*/
         if(krflg.ne.0)call eventp(3)                                           !/*xnp       341*/
         if(iptr.ne.0)call ptrak(3)                                             !/*xnp       342*/
         if(kdb.ne.0)return                                                     !/*xnp       343*/
         if(nter.ne.0)return                                                    !/*xnp       344*/
         go to 30                                                               !/*xnp       345*/
      endif                                                                     !/*xnp       346*/
!                                                                               !/*xnp       347*/
! ... complete energy step for particle remaining in cell icl.                  !/*xnp       348*/
!     ... in case of future energy-group and substep logic:                     !/*xnpa        5*/
!     ... 150 if(n1.ne.ngp)go to 40                                             !/*xnpa        6*/
      go to 40                                                                  !/*xnpa        7*/
!                                                                               !/*xnp       353*/
  500 ec=erg                                                                    !/*xnp       354*/
      wt=wgt                                                                    !/*xnp       355*/
      jsu=0                                                                     !/*xnp       356*/
!     Convert from shakes to nsec.                                              !/*xnp       357*/
      tc=ten*tme                                                                !/*xnp       358*/
      call interact(nb,lp,erg,elc(ipt),icl,mkc,0,tc,uuu,vvv,www,wgt,            !/*xnp       359*/
     & xxx,yyy,zzz,nel(lnel+1),mel(lmel+1),aa9(laa9+1),zz9(lzz9+1),             !/*xnp       360*/
     & iazdex(liax+1),fmf(lfmf+1),rho(lrho+1),pcap(lpcp+1),                     !/*xnp       361*/
     & hsigg(lhsg+1,1),xsiso(lxso+1,1),esxlp(lexp+1,1),                         !/*xnp       362*/
#ifdef HISTP                                                                    /*!xnp       363*/
     & npstc,                                                                   !/*xnp       364*/
#endif                                                                          /*!xnp       365*/
#ifdef MESHTAL                                                                  /*!xnp       366*/
     & eg_rc,eg_nt,                                                             !/*xnp       367*/
#endif                                                                          /*!xnp       368*/
     & nr)                                                                      !/*xnp       369*/
      erg=ec                                                                    !/*xnp       370*/
      wgt=wt                                                                    !/*xnp       371*/
#ifdef MESHTAL                                                                  /*!xnp       372*/
      fd=erg+eg_rc+eg_nt                                                        !/*xnp       373*/
      l=locct(llct+1,icl)                                                       !/*xnp       374*/
      if(l.ne.0)call tally(fd,1,l,d)                                            !/*xnp       375*/
      l=locct(llct+ipt,icl)                                                     !/*xnp       376*/
      if(l.ne.0)call tally(fd,2,l,d)                                            !/*xnp       377*/
      if(kdb.ne.0)return                                                        !/*xnp       378*/
      do 505 i=0,lev-1                                                          !/*xnp       379*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*xnp       380*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*xnp       381*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*xnp       382*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*xnp       383*/
  505 continue                                                                  !/*xnp       384*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*xnp       385*/
         do 510 i=1,6                                                           !/*xnp       386*/
            if (lev.eq.0) then                                                  !/*xnp       387*/
               zd(i)=gpblcm(i)                                                  !/*xnp       388*/
            else                                                                !/*xnp       389*/
               zd(i)=udt(i,0)                                                   !/*xnp       390*/
            endif                                                               !/*xnp       391*/
  510    continue                                                               !/*xnp       392*/
         zd(7)=wgt                                                              !/*xnp       393*/
         zd(8)=erg                                                              !/*xnp       394*/
         zd(9)=eg_rc                                                            !/*xnp       395*/
         jz(1)=mkc                                                              !/*xnp       396*/
         jz(2)=mspf                                                             !/*xnp       397*/
         jz(5)=3                                                                !/*xnp       398*/
         call gdtal(8,zero,zd,jz)                                               !/*xnp       399*/
         zd(9)=erg+eg_nt                                                        !/*xnp       400*/
         jz(5)=5                                                                !/*xnp       401*/
         call gdtal(8,zero,zd,jz)                                               !/*xnp       402*/
      endif                                                                     !/*xnp       403*/
#endif                                                                          /*!xnp       404*/
      if (nr.eq.id_nonelastic_collisn_terminal) then                            !/*xnp       405*/
         if (ipt.eq.1) then                                                     !/*xnp       406*/
            nter = loss_neutron_nucl_interaction                                !/*xnp       407*/
         else                                                                   !/*xnp       408*/
            nter = loss_gen_neu_nucl_interaction                                !/*xnp       409*/
         endif                                                                  !/*xnp       410*/
      else if (nr.eq.id_fulldecay_pseudo_terminal) then                         !/*xnp       411*/
         if (ipt.ne.1) then                                                     !/*xnp       412*/
            nter = loss_gen_neu_particle_decay                                  !/*xnp       413*/
         else                                                                   !/*xnp       414*/
!           QUESTION:  Revisit this specification.  In particular, is           !/*xnp       415*/
!                      this really decay (not yet done for neutrons),           !/*xnp       416*/
!                      or does it model antineutron annihilation???             !/*xnp       417*/
            nter = loss_neutron_particle_decay                                  !/*xnp       418*/
         endif                                                                  !/*xnp       419*/
      else if (nr.eq.id_stopped_particle_terminal) then                         !/*xnp       420*/
         nter = loss_all_pls_energy_cutoff                                      !/*xnp       421*/
      else                                                                      !/*xnp       422*/
         call expirx(1,'neu_pl',                                                !/*xnp       423*/
     &   'bad result from interact for particle below cutoff.')                 !/*xnp       424*/
      endif                                                                     !/*xnp       425*/
      return                                                                    !/*xnp       426*/
      end                                                                       !/*xnp       427*/

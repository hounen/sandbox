!#ident sumary.F from deck eq                                                   !/*eq          1*/
      subroutine sumary                                                         !/*eq          2*/
!        print summaries of problem activity.                                   !/*eq          3*/
      use global_data                                                           !/*eqx         1*/
!                                                                               !/*eqx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*eqx         3*/
      implicit integer(kindi) (i-n)                                             !/*eqx         4*/
!                                                                               !/*eqx         5*/
!                                                                               !/*eqx         6*/
!     For each pair of pax categories:                                          !/*eqx         7*/
!                                                                               !/*eqx         8*/
!       ha(1) contains labels and tracks for the gain columns.                  !/*eqx         9*/
!       ha(2) contains labels and tracks for the loss columns.                  !/*eqx        10*/
!       ht(1) and ht(2) contain the wgt and wgt*erg for the gains.              !/*eqx        11*/
!       ht(3) and ht(4) contain the wgt and wgt*erg for the losses.             !/*eqx        12*/
!                                                                               !/*eqx        13*/
      character ha(2)*26,hg*16,ht(4)*14,hy*88,hz*10                             !/*eqx        14*/
!                                                                               !/*eqx        15*/
!                                                                               !/*eq         20*/
!        get and print the source particle normalization.                       !/*eq4b        2*/
      t=nps                                                                     !/*eq4b        3*/
      if(nsr.eq.71.and.kcz.ge.ikz)t=nsrck*(kcz-ikz)                             !/*eq4c        5*/
      if(kc8.lt.0)t=t+nsrck-wt0*nsa                                             !/*eq4c        7*/
      if(knrm.ne.0)t=pax(1,1,1)                                                 !/*eqx        20*/
      if(nsr.eq.6.and.nrrs.ge.nrss)t=np1                                        !/*eq4b        4*/
      if(nsr.eq.6.and.nrrs.lt.nrss)t=npsr                                       !/*eq4b        5*/
      fpi=1./max(one,t)                                                         !/*eq4b        9*/
      write(hy,'(20x,41hsource particle weight for summary table ,'//           !/*eqx        22*/
     & '15hnormalization =,f12.2)')t                                            !/*eqx        23*/
      if(nint(t).eq.nps)hy=' '                                                  !/*eq4c        9*/
      if(kc8.lt.0)hy(1:20)='(active cycles only)'                               !/*eqx        26*/
      write(iuo,'(17h1problem summary ,a20,14x,a68/)')hy(1:20),hy(21:88)        !/*eq4b       14*/
!                                                                               !/*eq4b       15*/
!        if the run is finished, print the reason.                              !/*eq4b       16*/
      call getidt(idtm)                                                         !/*eq4b       17*/
      if(nst.eq.0)go to 20                                                      !/*eq         27*/
      write(rfq(1)(4:7),'(i4)')ksr                                              !/*eq         28*/
      write(rfq(2)(8:17),'(i10)')nps                                            !/*eq         29*/
      write(rfq(3)(11:14),'(i4)')nerr                                           !/*eq         30*/
      write(rfq(5)(20:24),'(i5)')int(ctme)                                      !/*cor4-1    196*/
      write(rfq(7)(8:13),'(i6)')kcy                                             !/*eq4a        3*/
      write(rfq(8)(9:18),'(i10)')iptra(6)                                       !/*eq4a        4*/
      do 10 i=1,11                                                              !/*eq         33*/
      if(mod(nst/2**i,2).eq.0)go to 10                                          !/*eq         34*/
      write(iuo,'(6x,15hrun terminated ,'//rfq(i)//')')                         !/*eq4a        5*/
      if(mct.ge.0)write(iuo,'(1h+,99x,a19)')idtm                                !/*eq4a        6*/
      write(jtty,'(16h run terminated ,'//rfq(i)//')')                          !/*eq         36*/
   10 continue                                                                  !/*eq         37*/
!                                                                               !/*eq         38*/
!        print problem title and id.                                            !/*eq         39*/
   20 if(mct.ge.0)write(iuo,30)aid,probid                                       !/*eq4a        7*/
   30 format(6x,a80,5x,9hprobid = ,a19)                                         !/*eq         41*/
      if(konrun.ne.0.and.aid1(1:8).eq.'continue')write(iuo,40)aid1              !/*eq         42*/
   40 format(6x,a80)                                                            !/*eq         43*/
!                                                                               !/*eq         44*/
!        print creation and loss summary.                                       !/*eq         45*/
      nf=0                                                                      !/*eq         46*/
      t5=0.                                                                     !/*eq         47*/
      t6=0.                                                                     !/*eq         48*/
      if(smul(5).ne.0.)t5=1.+smul(2)/smul(5)                                    !/*eq4c2      44*/
      if(smul(5).ne.0.)t6=sqrt(abs(smul(3)*smul(5)**2-2.*smul(6)*smul(5)        !/*eq4c2      45*/
     & *smul(2)+smul(7)*smul(2)**2))/(smul(5)*(smul(5)+smul(2)))                !/*eq4c2      46*/
      do 200 ip=1,mipt                                                          !/*eq         51*/
      if(kpt(ip).eq.0)go to 200                                                 !/*eq         52*/
      if (ip.le.3) then                                                         !/*eqx        30*/
         iq=ip                                                                  !/*eqx        31*/
      else if (charge(ip).eq.zero) then                                         !/*eqx        32*/
         iq=4                                                                   !/*eqx        33*/
      else                                                                      !/*eqx        34*/
         iq=5                                                                   !/*eqx        35*/
      endif                                                                     !/*eqx        36*/
!                                                                               !/*eqx        37*/
      if (ip.eq.1) then                                                         !/*eqx        38*/
         ik = id_category_neutron                                               !/*eqx        39*/
      else if (ip.eq.2) then                                                    !/*eqx        40*/
         ik = id_category_photon                                                !/*eqx        41*/
      else if (ip.eq.3) then                                                    !/*eqx        42*/
         ik = id_category_electron                                              !/*eqx        43*/
      else if (charge(ip).eq.zero) then                                         !/*eqx        44*/
         ik = id_category_gen_neu                                               !/*eqx        45*/
      else                                                                      !/*eqx        46*/
         ik = id_category_gen_chg                                               !/*eqx        47*/
      endif                                                                     !/*eqx        48*/
      i=leng(hnp(ip))                                                           !/*eqx        49*/
      if(dbcn(26).ne.0.)then                                                    !/*eqx        56*/
      label_pax_gain_all_pls(igain_all_pls_nucl_interaction)=' '                !/*eqx        57*/
      label_pax_gain_all_pls(igain_all_pls_particle_decay)=' '                  !/*eqx        58*/
      label_pax_gain_specific(igain_neutron_tabular_boundary,1)=' '             !/*eqx        59*/
      label_pax_gain_specific(igain_neutron_tabular_sampling,1)=' '             !/*eqx        60*/
      label_pax_loss_specific(loss_neutron_nucl_interaction,1)=' '              !/*eqx        61*/
      label_pax_loss_specific(loss_neutron_particle_decay,1)=' '                !/*eqx        62*/
      label_pax_loss_specific(loss_neutron_tabular_boundary,1)=' '              !/*eqx        63*/
      label_pax_gain_specific(igain_photon_gamma_xgamma,2)=' '                  !/*eqx        64*/
      label_pax_gain_specific(igain_photon_tabular_sampling,2)=' '              !/*eqx        65*/
      label_pax_gain_specific(igain_electron_gamma_xelectron,3)=' '             !/*eqx        66*/
      label_pax_loss_specific(loss_electron_interact_or_decay,3)=' '            !/*eqx        67*/
      nf=nf+1                                                                   !/*eqx        68*/
      ng=0                                                                      !/*eqx        69*/
      if(nf.gt.1)ng=mod(nf,2)                                                   !/*eqx        70*/
      write(iuo,49)ng,hnp(ip)(1:i)//' creation         ',                       !/*eqx        71*/
     & hnp(ip)(1:i)//' loss             '                                       !/*eqx        72*/
   49 format(i1/1x,a20,6htracks,6x,6hweight,8x,6henergy,                        !/*eqx        73*/
     & 12x,a20,6htracks,6x,6hweight,8x,6henergy/                                !/*eqx        74*/
     & 33x,21h(per source particle),43x,21h(per source particle)/)              !/*eqx        75*/
      else                                                                      !/*eqx        76*/
      write(iuo,50)hnp(ip)(1:i)//' creation             ',                      !/*eqx        77*/
     & hnp(ip)(1:i)//' loss                 '                                   !/*eqx        78*/
   50 format(/1x,a23,6htracks,3x,6hweight,8x,6henergy,                          !/*eqx        79*/
     & 12x,a23,6htracks,3x,6hweight,8x,6henergy/                                !/*eqx        80*/
     & 33x,21h(per source particle),43x,21h(per source particle)/)              !/*eqx        81*/
      endif                                                                     !/*eqx        82*/
      do 60 i=1,6                                                               !/*eqx        83*/
   60 tpp(i)=zero                                                               !/*eqx        84*/
!                                                                               !/*eqx        85*/
      if (pax(2,igain_all_pls_source,nips).ne.zero) then                        !/*eqx        86*/
         f=one/pax(2,igain_all_pls_source,nips)                                 !/*eqx        87*/
      else                                                                      !/*eqx        88*/
         f=one                                                                  !/*eqx        89*/
      endif                                                                     !/*eqx        90*/
!                                                                               !/*eqx        91*/
      do 110 ix=1,items_pax_total                                               !/*eqx        92*/
!                                                                               !/*eqx        93*/
!     Creation and loss both blank:  skip to next category.                     !/*eqx        94*/
      if (ix.le.itm_pax_all_pls_end) then                                       !/*eqx        95*/
         if (label_pax_gain_all_pls(ix).eq.' ' .and.                            !/*eqx        96*/
     &       label_pax_loss_all_pls(ix).eq.' ') go to 110                       !/*eqx        97*/
      else                                                                      !/*eqx        98*/
         if (label_pax_gain_specific(ix,ik).eq.' ' .and.                        !/*eqx        99*/
     &       label_pax_loss_specific(ix,ik).eq.' ') go to 110                   !/*eqx       100*/
      endif                                                                     !/*eqx       101*/
!                                                                               !/*eqx       102*/
!     For now, dxtran, forced collisions, and exponential transform             !/*eqx       103*/
!     do not apply to electrons.  Note that this logic assumes that             !/*eqx       104*/
!     for each of these categories, gain and loss are on the same               !/*eqx       105*/
!     line (a sufficient though not necessary condition).                       !/*eqx       106*/
!     We think this is now the only place where a specific relationship         !/*eqx       107*/
!     between the locations of gain and loss categories is assumed.             !/*eqx       108*/
      if (ip.eq.3) then                                                         !/*eqx       109*/
         if (ix .eq. igain_all_pls_dxtran .or.                                  !/*eqx       110*/
     &       ix .eq.  loss_all_pls_dxtran .or.                                  !/*eqx       111*/
     &       ix .eq. igain_all_pls_forced_collisions .or.                       !/*eqx       112*/
     &       ix .eq.  loss_all_pls_forced_collisions .or.                       !/*eqx       113*/
     &       ix .eq. igain_all_pls_exp_transform .or.                           !/*eqx       114*/
     &       ix .eq.  loss_all_pls_exp_transform) go to 110                     !/*eqx       115*/
      endif                                                                     !/*eqx       116*/
!                                                                               !/*eqx       117*/
      do 80 i=1,6                                                               !/*eqx       118*/
   80 tpp(i)=tpp(i)+pax(i,ix,ip)                                                !/*eqx       119*/
!                                                                               !/*eqx       120*/
      ha(1)=' '                                                                 !/*eqx       121*/
      ht(1)=' '                                                                 !/*eqx       122*/
      ht(2)=' '                                                                 !/*eqx       123*/
      if (ix.le.itm_pax_all_pls_end) then                                       !/*eqx       124*/
         ha(1)(1:17)=label_pax_gain_all_pls(ix)                                 !/*eqx       125*/
      else                                                                      !/*eqx       126*/
         ha(1)(1:17)=label_pax_gain_specific(ix,ik)                             !/*eqx       127*/
      endif                                                                     !/*eqx       128*/
      if(ha(1).eq.' ')go to 90                                                  !/*eqx       129*/
      write(hg,'(f16.0)')pax(1,ix,ip)                                           !/*eqx       130*/
      call nxtsym(hg,' .',1,it,iu,1)                                            !/*eq4a       15*/
      l=max(8,iu-it)                                                            !/*eq4a       16*/
      ha(1)(26-l:26)=hg(15-l:15)                                                !/*eqx       131*/
      if (pax(2,ix,ip).eq.zero) then                                            !/*eqx       132*/
         ht(1)='    0.'                                                         !/*eqx       133*/
      else                                                                      !/*eqx       134*/
         write(ht(1),'(1pe14.4)')fpi*pax(2,ix,ip)                               !/*eqx       135*/
      endif                                                                     !/*eqx       136*/
      if (pax(3,ix,ip).eq.zero) then                                            !/*eqx       137*/
         ht(2)='    0.'                                                         !/*eqx       138*/
      else                                                                      !/*eqx       139*/
         write(ht(2),'(1pe14.4)')f*pax(3,ix,ip)                                 !/*eqx       140*/
      endif                                                                     !/*eqx       141*/
   90 continue                                                                  !/*eq4a       22*/
      ha(2)=' '                                                                 !/*eqx       142*/
      ht(3)=' '                                                                 !/*eqx       143*/
      ht(4)=' '                                                                 !/*eqx       144*/
      if (ix.le.itm_pax_all_pls_end) then                                       !/*eqx       145*/
         ha(2)(1:17)=label_pax_loss_all_pls(ix)                                 !/*eqx       146*/
      else                                                                      !/*eqx       147*/
         ha(2)(1:17)=label_pax_loss_specific(ix,ik)                             !/*eqx       148*/
      endif                                                                     !/*eqx       149*/
      if(ha(2).eq.' ')go to 95                                                  !/*eqx       150*/
      write(hg,'(f16.0)')pax(4,ix,ip)                                           !/*eqx       151*/
      call nxtsym(hg,' .',1,it,iu,1)                                            !/*eqx       152*/
      l=max(8,iu-it)                                                            !/*eqx       153*/
      ha(2)(26-l:26)=hg(15-l:15)                                                !/*eqx       154*/
      if (pax(5,ix,ip).eq.zero) then                                            !/*eqx       155*/
         ht(3)='    0.'                                                         !/*eqx       156*/
      else                                                                      !/*eqx       157*/
         write(ht(3),'(1pe14.4)')fpi*pax(5,ix,ip)                               !/*eqx       158*/
      endif                                                                     !/*eqx       159*/
      if (pax(6,ix,ip).eq.zero) then                                            !/*eqx       160*/
         ht(4)='    0.'                                                         !/*eqx       161*/
      else                                                                      !/*eqx       162*/
         write(ht(4),'(1pe14.4)')f*pax(6,ix,ip)                                 !/*eqx       163*/
      endif                                                                     !/*eqx       164*/
   95 continue                                                                  !/*eqx       165*/
!                                                                               !/*eqx       166*/
      write(iuo,100)ha(1),ht(1),ht(2),ha(2),ht(3),ht(4)                         !/*eq4a       23*/
  100 format(1x,a26,2a14,10x,a26,2a14)                                          !/*eq4a       24*/
  110 continue                                                                  !/*eq4a       25*/
!                                                                               !/*eqx       167*/
      write(iuo,130)tpp(1),fpi*tpp(2),f*tpp(3),tpp(4),fpi*tpp(5),               !/*eqx       168*/
     & f*tpp(6)                                                                 !/*eq4a       27*/
  130 format(5x,5htotal,f18.0,tl1,1p2e14.4,14x,5htotal,0pf18.0,tl1,             !/*eq4a       28*/
     & 1p2e14.4)                                                                !/*eq4a       29*/
!                                                                               !/*eq         90*/
!        print odds and ends, prompt lifetime estimates, and cutoffs.           !/*eq4b       18*/
      if(ip.gt.3.and.dbcn(26).gt.0.)go to 200                                   !/*eqx       169*/
      tr=tpp(1)*fpi                                                             !/*eqxc        1*/
      if (ip.eq.1 .or. ip.eq.2) then                                            !/*eqx       170*/
         s=zero                                                                 !/*eqx       171*/
         do 140 i=1,4                                                           !/*eqx       172*/
  140    tpp(i)=zero                                                            !/*eqx       173*/
         if (pax(5,loss_all_pls_escape,ip).gt.zero) tpp(1) =                    !/*eqx       174*/
     &         tmav(ip,1)/pax(5,loss_all_pls_escape,ip)                         !/*eqx       175*/
         if (ip.eq.1) then                                                      !/*eqx       176*/
            cp = pax(5,loss_neutron_capture,ip)                                 !/*eqx       177*/
         else                                                                   !/*eqx       178*/
            cp = pax(5,loss_photon_capture,ip) +                                !/*eqx       179*/
     &           pax(5,loss_photon_photonuclear,ip)                             !/*eqx       180*/
         endif                                                                  !/*eqx       181*/
         if (cp.gt.zero) tpp(2)=tmav(ip,2)/cp                                   !/*eqx       182*/
         if ((pax(5,loss_all_pls_escape,ip)+cp).gt.zero) tpp(3) =               !/*eqx       183*/
     &      (tmav(ip,1)+tmav(ip,2))/(pax(5,loss_all_pls_escape,ip)+cp)          !/*eqx       184*/
!                                                                               !/*eqx       185*/
         do 150 i=1,items_pax_total                                             !/*eqx       186*/
  150 s=s+pax(5,i,ip)                                                           !/*eq        104*/
         if(s.gt.zero)tpp(4)=tmav(ip,3)/s                                       !/*eqx       187*/
      endif                                                                     !/*eqx       188*/
!                                                                               !/*eqx       189*/
      if (ip.eq.1) then                                                         !/*eqx       190*/
         write(iuo,160)nbt(1),tr,tpp(1),tco(1),coll(1)*fpi,                     !/*eqx       191*/
     & tpp(2),ecf(1),coll(1),tpp(3),wc1(1),t5,t6,tpp(4),wc2(1)                  !/*eq4a       31*/
  160    format(/'   number of neutrons banked',i24,8x,                         !/*eqx       192*/
     &    'average time of (shakes)',14x,'cutoffs'/                             !/*eqx       193*/
     &    '   neutron tracks per source particle',1pe15.4,10x,'escape',         !/*eqx       194*/
     &    e22.4,10x,'tco',e13.4/                                                !/*eqx       195*/
     &    '   neutron collisions per source particle',e11.4,10x,                !/*eqx       196*/
     &    'capture',e21.4,10x,'eco',e13.4/                                      !/*eqx       197*/
     &    '   total neutron collisions',                                        !/*eqx       198*/
     &    0pf26.0,tl1,' ',9x,'capture or escape',1pe11.4,10x,'wc1',             !/*eqx       199*/
     &    e13.4/'   net multiplication',e24.4,0pf7.4,10x,                       !/*eqx       200*/
     &    'any termination',1pe13.4,10x,'wc2',e13.4)                            !/*eqx       201*/
!                                                                               !/*eqx       202*/
      else if(ip.eq.2) then                                                     !/*eqx       203*/
         write(iuo,170)nbt(2),tr,tpp(1),tco(2),coll(2)*fpi,                     !/*eqx       204*/
     & tpp(2),ecf(2),coll(2),tpp(3),wc1(2),tpp(4),wc2(2)                        !/*eq4a       40*/
  170    format(/'  number of photons banked ',i25,8x,                          !/*eqx       205*/
     &    'average time of (shakes)',14x,'cutoffs'/                             !/*eqx       206*/
     &    '   photon tracks per source particle',1pe16.4,10x,'escape',          !/*eqx       207*/
     &    e22.4,10x,'tco',e13.4/                                                !/*eqx       208*/
     &    '   photon collisions per source particle',e12.4,                     !/*eqx       209*/
     &    10x,'capture',e21.4,10x,'eco',e13.4/                                  !/*eqx       210*/
     &    '   total photon collisions',0pf27.0,tl1,' ',9x,                      !/*eqx       211*/
     &    'capture or escape',1pe11.4,10x,'wc1',e13.4/                          !/*eqx       212*/
     &    62x,'any termination',e13.4,10x,'wc2',e13.4)                          !/*eqx       213*/
!                                                                               !/*eqx       214*/
      else if(ip.eq.3) then                                                     !/*eqx       215*/
         write(iuo,180)nbt(3),tr,tco(3),coll(3)*fpi,                            !/*eqx       216*/
     & ecf(3),coll(3),wc1(3),wc2(3)                                             !/*eq4a       44*/
  180    format(/'  number of electrons banked ',i23,46x,'cutoffs'/             !/*eqx       217*/
     &    '   electron tracks per source particle',1pe14.4,48x,'tco',           !/*eqx       218*/
     &    e13.4/'   electron substeps per source particle ',e11.4,              !/*eqx       219*/
     &    48x,'eco',e13.4/'   total electron substeps ',                        !/*eqx       220*/
     &    0pf26.0,tl1,' ',47x,'wc1',1pe13.4/100x,'wc2',e13.4)                   !/*eqx       221*/
!                                                                               !/*eqx       222*/
      else if (charge(ip).eq.zero) then                                         !/*eqx       223*/
         write(iuo,190)nbt(ip),tr,tco(ip),coll(ip)*fpi,                         !/*eqx       224*/
     &    ecf(ip),coll(ip),wc1(ip),wc2(ip)                                      !/*eqx       225*/
  190    format(/'   number of particles banked',i23,46x,'cutoffs'/             !/*eqx       226*/
     &    '   particle tracks per source particle',1pe14.4,48x,'tco',           !/*eqx       227*/
     &    e13.4/'   particle collisions per source particle',e11.4,             !/*eqx       228*/
     &    47x,'eco',e13.4/'   total particle collisions',                       !/*eqx       229*/
     &    0pf25.0,tl1,' ',47x,'wc1',1pe13.4/100x,'wc2',e13.4)                   !/*eqx       230*/
!                                                                               !/*eqx       231*/
      else                                                                      !/*eqx       232*/
         write(iuo,195)nbt(ip),tr,tco(ip),coll(ip)*fpi,                         !/*eqx       233*/
     &    ecf(ip),coll(ip),wc1(ip),wc2(ip)                                      !/*eqx       234*/
  195    format(/'   number of particles banked',i23,46x,'cutoffs'/             !/*eqx       235*/
     &    '   particle tracks per source particle',1pe14.4,48x,'tco',           !/*eqx       236*/
     &    e13.4/'   particle substeps per source particle',e12.4,               !/*eqx       237*/
     &    48x,'eco',e13.4/'   total particle  substeps',                        !/*eqx       238*/
     &    0pf26.0,tl1,' ',47x,'wc1',1pe13.4/100x,'wc2',e13.4)                   !/*eqx       239*/
      endif                                                                     !/*eqx       240*/
!                                                                               !/*eqx       241*/
  200 continue                                                                  !/*eqx       242*/
!                                                                               !/*eq        133*/
!        print the run data and bank use numbers.                               !/*eq        134*/
      call secnd(tq)                                                            !/*eq4b       24*/
      if(mct.ge.0)write(iuo,210)(tq+cp3)/60.,nbhwm,cts/60.,nbov,                !/*eq4b       25*/
     & 60.*nps/max(one,cts),lfll,lfll*8,rnr,nrnh(2),nrnh(3)                     !/*eqx       243*/
!        TIMER patch, enabling  PRDMP 2J -3  for timing studies.                !/*eqx       244*/
      if(mct.lt.0.and.mct.ge.-2)                                                !/*eqx       245*/
     & write(iuo,210)0.,nbhwm,0.,nbov,0.,0,0,rnr,nrnh(2),nrnh(3)                !/*eqx       246*/
      if(mct.lt.-2)write(iuo,210)0.,nbhwm,0.,nbov,60.*nps/max(one,cts),         !/*eqx       247*/
     & 0,0,rnr,nrnh(2),nrnh(3)                                                  !/*eqx       248*/
!        End of TIMER patch.                                                    !/*eqx       249*/
  210 format(/33h computer time so far in this run,f9.2,8h minutes,12x,         !/*eq        140*/
     & 27hmaximum number ever in bank,i10/23h computer time in mcrun,           !/*eq        141*/
     & f19.2,8h minutes,12x,29hbank overflows to backup file,i8/                !/*eq        142*/
     & 28h source particles per minute,1pe22.4,12x,15hdynamic storage,          !/*eq4b       28*/
     & i10,7h words,,i11,7h bytes./25h random numbers generated,                !/*eq4b       29*/
     & 0pf26.0,tl1,1h ,11x,28hmost random numbers used was,i9,                  !/*eq4b       30*/
     & 11h in history,i9)                                                       !/*eq4b       31*/
!                                                                               !/*eq        146*/
!        print conditional comments.                                            !/*eq        147*/
      if(nrnh(1).gt.0)call erprnt(1,2,2,nstrid,nrnh(1),0,0,0,                   !/*eq4a       49*/
     & '21h random number stride,i9,9h exceeded,i9,7h times.')                  !/*eq4a       50*/
      ie=0                                                                      !/*eq        151*/
      if((float(nstrid)*2.**(-23))*(float(nps)*2.**(-23)).gt.1.)ie=1            !/*eq        152*/
      if(ie.ne.0)call erprnt(1,2,0,0,0,0,0,0,                                   !/*eq4a       51*/
     & '48hrandom number period exceeded.  decrease stride.')                   !/*eq4a       52*/
      write(iuo,240)wgts(1),wgts(2)                                             !/*eq        156*/
  240 format(/34h range of sampled source weights =,1pe11.4,3h to,e11.4)        !/*eq        157*/
      ie=0                                                                      !/*eq4a       53*/
      do 250 m=1,mxa                                                            !/*eq        158*/
      if(isef(lise+1,m).ne.0)ie=1                                               !/*eq4a       54*/
  250 if(isef(lise+1,m).ne.0)write(iuo,260)                                     !/*eq        159*/
     & 1.-isef(lise+2,m)/(isef(lise+1,m)+zero),ncl(lncl+m)                      !/*eq        160*/
  260 format(/20h source efficiency =,f7.4,9h  in cell,i6)                      !/*eq        161*/
      je=0                                                                      !/*eq4a       55*/
      do 265 i=1,msd                                                            !/*eq4a       56*/
  265 if(ksd(lksd+18,i).ne.0.and.ksd(lksd+12,i).ge.6.and.                       !/*eq4a       57*/
     & ksd(lksd+12,i).le.14)je=1                                                !/*eq4a       58*/
!        the following warning is based upon x-6:gpe:teb-91-419.                !/*eq4a       59*/
      if(ie.ne.0.and.je.ne.0)call erprnt(1,2,0,0,0,0,0,0,                       !/*eq4a       60*/
     & '55hbiased source position rejection sampling is dangerous.')            !/*eq4a       61*/
      if(netb(1).ne.0)write(iuo,270)netb(1)                                     !/*eq        162*/
  270 format(/1x,i7,38h source particles had energies > emax.)                  !/*eq4a       62*/
      if(nwse.ne.0)write(iuo,280)nwse                                           !/*eq        164*/
  280 format(/1x,i7,44h source particles had energies below cutoff.)            !/*eq        165*/
      if(nwst.ne.0)write(iuo,290)nwst                                           !/*eq        166*/
  290 format(/1x,i7,46h source particles had times later than cutoff.)          !/*eq        167*/
      n=nww(nips)                                                               !/*eq        168*/
      if(nwsg(1).ne.0)write(iuo,300)nwsg(1)                                     !/*eq        169*/
  300 format(/1x,i7,42h source particles had weight above window.)              !/*eq        170*/
      if(nwsg(1).ne.0.and.n.gt.1)write(iuo,320)(nwws(1,i),i=1,n)                !/*eq        171*/
      if(nwsg(2).ne.0)write(iuo,310)nwsg(2)                                     !/*eq        172*/
  310 format(/1x,i7,42h source particles had weight below window.)              !/*eq        173*/
      if(nwsg(2).ne.0.and.n.gt.1)write(iuo,320)(nwws(2,i),i=1,n)                !/*eq        174*/
  320 format(34h by increasing ww energy interval:,10i8,:/(2x,14i8))            !/*eq        175*/
      if(nwsb.ne.0)write(iuo,330)nwsb                                           !/*eq        176*/
  330 format(/1x,i7,42h source particles had weight below cutoff.)              !/*eq        177*/
      if(netb(2).ne.0)write(iuo,340)netb(2)                                     !/*eq        178*/
  340 format(/1x,i7,48h neutrons from inelastic collisions had energies,        !/*eq        179*/
     & 18h greater than emx.)                                                   !/*eq        180*/
      if(nerr.ne.0)write(iuo,350)nerr                                           !/*eq        181*/
  350 format(/1x,i6,20h particles got lost.)                                    !/*eq        182*/
      if(npnm.ne.0)write(iuo,360)npnm                                           !/*eq        183*/
  360 format(/32h neutron reaction mt loop failed,i4,7h times.)                 !/*eq        184*/
      if(npum.ne.0)write(iuo,365)npum                                           !/*eq4c2      54*/
  365 format(/39h photonuclear production mt loop failed,i4,7h times.)          !/*eq4c2      55*/
      if(nppm.ne.0)write(iuo,370)nppm                                           !/*eq4c2      56*/
  370 format(/49h neutron-induced photon production mt loop failed,             !/*eq4c2      57*/
     & i4,7h times.)                                                            !/*eq4c2      58*/
      if(npxm.ne.0)write(iuo,369)npxm                                           !/*eqx       250*/
  369 format(/35h particle production mt loop failed,i4,7h times.)              !/*eqx       251*/
!                                                                               !/*eqx       252*/
      do 371 n=1,mipt                                                           !/*eqx       253*/
  371 if(eacc(n,1).ne.zero)go to 372                                            !/*eqx       254*/
      go to 376                                                                 !/*eqx       255*/
  372 call erprnt(1,2,0,0,0,0,0,0,                                              !/*eqx       256*/
     & '44hsome particles above emax() were not banked.')                       !/*eqx       257*/
      write(iuo,373)                                                            !/*eqx       258*/
  373 format('1particles not banked with energies above emax():'//              !/*eqx       259*/
     & ' particle',6x,'weight',7x,'weight*energy',3x,'max. energy'/)            !/*eqx       260*/
      do 375 n=1,mipt                                                           !/*eqx       261*/
      if(eacc(n,1).eq.zero)go to 375                                            !/*eqx       262*/
      write(iuo,374)hnp(n),fpi*eacc(n,1),fpi*eacc(n,2),eacc(n,3)                !/*eqx       263*/
  374 format(1x,a11,1pe14.5,1pe14.5,1pe15.5)                                    !/*eqx       264*/
  375 continue                                                                  !/*eqx       265*/
!                                                                               !/*eqx       266*/
  376 do 377 n=1,mipt                                                           !/*eqx       267*/
  377 if(racc(n,1).ne.zero)go to 378                                            !/*eqx       268*/
      go to 383                                                                 !/*eqx       269*/
  378 call erprnt(1,2,0,0,0,0,0,0,                                              !/*eqx       270*/
     & '48hsome particles not on mode card were not banked.')                   !/*eqx       271*/
      write(iuo,379)                                                            !/*eqx       272*/
  379 format('1particles not on mode card, so not banked:'//                    !/*eqx       273*/
     & ' particle',6x,'weight',7x,'weight*energy'/)                             !/*eqx       274*/
      do 381 n=1,mipt                                                           !/*eqx       275*/
      if(racc(n,1).eq.zero)go to 381                                            !/*eqx       276*/
      write(iuo,380)hnp(n),fpi*racc(n,1),fpi*racc(n,2)                          !/*eqx       277*/
  380 format(1x,a11,1pe14.5,1pe14.5)                                            !/*eqx       278*/
  381 continue                                                                  !/*eqx       279*/
!                                                                               !/*eqx       280*/
  383 continue                                                                  !/*eqx       281*/
      if(ntasks.le.1.and.ltasks.le.1)go to 395                                  !/*eq4c       13*/
      if(mct.lt.0.or.ntasks*ltasks.eq.0.or.pptme(4).eq.0.)go to 388             !/*eq4c       14*/
      rn=100.*(pptme(2)*ntasks-pptme(1)+(pptme(4)-pptme(3)))/                   !/*eq4c       15*/
     & (pptme(4)*ntasks*ltasks)                                                 !/*eq4c       16*/
      rm=(100.-rn)*(pptme(4)-pptme(3))/pptme(4)                                 !/*eq4c       17*/
      rc=(100.-rn)*(pptme(3)-pptme(2)/ltasks)/pptme(4)                          !/*eq4c       18*/
      r1=max(zero,100.-rn-rm-rc)                                                !/*eq4c       19*/
      write(iuo,386)nint(rn),nint(r1),nint(rc),nint(rm)                         !/*eq4c       20*/
  386 format(/36h estimated system efficiency:  net =,i3,9h%  loss =,           !/*eq4c       21*/
     & i3,11h% (locks) +,i3,11h% (comm.) +,i3,9h% (misc.))                      !/*eq4c       22*/
  388 if(ltasks.le.1)write(iuo,390)(nbal(i),i=1,ntasks)                         !/*eq4c       23*/
      if(ltasks.gt.1)write(iuo,390)(nbal(i),i=1,ltasks+1)                       !/*eq4c       24*/
  390 format(/43h number of histories processed by each task/(10i10))           !/*eq        191*/
!                                                                               !/*eq        192*/
!        print source performance information.                                  !/*eq        193*/
  395 if(nsr.eq.40.and.ink(170).ne.0)call prsdft                                !/*eq4c       25*/
      if(nsr.eq.6.and.ink(170).ne.0)call prssrj                                 !/*eq        195*/
!                                                                               !/*eq        196*/
!        print a table of cells with excessive importance ratios.               !/*eq        197*/
      call crtcze(0)                                                            !/*eq        198*/
!                                                                               !/*eq        199*/
!        print cell activity, weight balance, and nuclide activity.             !/*eq        200*/
      call action                                                               !/*eq        201*/
      if(ink(130).ne.0)call tbl130                                              !/*eq4c2      59*/
      if(ink(140).ne.0.and.mxe.gt.0)call tbl140                                 !/*eq4c2      60*/
!                                                                               !/*eq        202*/
!        print table of discrete reaction rejections, if any.                   !/*eq        203*/
      k=0                                                                       !/*eq        204*/
      do 410 i=1,mxe                                                            !/*eq        205*/
      if(ndr(lndr+i).eq.0)go to 410                                             !/*eq        206*/
      if(k.eq.0)write(iuo,400)                                                  !/*eq        207*/
  400 format(1h1,6x,41hnumber of discrete reaction rejections in,               !/*eq        208*/
     & 8h acecol./)                                                             !/*eq        209*/
      k=1                                                                       !/*eq        210*/
      call zaid(2,hz,ixl(lixl+1,i))                                             !/*eq        211*/
      write(iuo,'(5x,a10,i10)')hz,ndr(lndr+i)                                   !/*eq        212*/
  410 continue                                                                  !/*eq        213*/
!                                                                               !/*eq        214*/
!        print information about neutron-induced photons.                       !/*eq        215*/
      if(kpt(1)*kpt(2).ne.0)call photp                                          !/*eq        216*/
!                                                                               !/*eq        217*/
!        print dxtran diagnostics.                                              !/*eq        218*/
      if(nps.ne.0)call dxdiag                                                   !/*eq        219*/
!                                                                               !/*eq4a       64*/
!        print information about keff results.                                  !/*eqx       284*/
      if(nsr.ne.71.or.(nst.eq.0.and.npp.ge.0))go to 415                         !/*eq4c       27*/
      call kprint                                                               !/*eqx       288*/
!                                                                               !/*eq        220*/
!        print tally data.                                                      !/*eq        221*/
  415 if(ntal.gt.0)call tallyp                                                  !/*eq4c       30*/
!                                                                               !/*eq        223*/
!        generate weight-window cards at the end of the run.                    !/*eq        224*/
      if(iwwg.gt.0.and.(nst.ne.0.or.npp.lt.0))call avrout                       !/*eq4c       31*/
      if(icw.ne.0.and.(nst.ne.0.or.npp.lt.0))call gmgww                         !/*eq        226*/
!                                                                               !/*eq        227*/
!        print rnr if required, for quick evaluation of test problems.          !/*eq        228*/
      if(dbcn(12).eq.zero.or.nst.eq.0.and.npp.ge.0.or.nps.le.0)return           !/*eqx       290*/
      write(ha(1)(1:16),'(f16.0)')dbcn(12)                                      !/*eq4a       67*/
      write(ha(2)(1:16),'(f16.0)')rnr                                           !/*eq4a       68*/
      write(jtty,420)ha(1)(1:15),ha(2)(1:15)                                    !/*eq4a       69*/
      write(iuo,420)ha(1)(1:15),ha(2)(1:15)                                     !/*eq4a       70*/
  420 format(28h random numbers:  expected =,a15,4x,8hactual =,a15)             !/*eq4a       71*/
      return                                                                    !/*eq        233*/
      end                                                                       !/*eq        234*/

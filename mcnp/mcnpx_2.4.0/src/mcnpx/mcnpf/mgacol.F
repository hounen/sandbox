!#ident mgacol.F from deck ma                                                   !/*ma          1*/
      subroutine mgacol                                                         !/*ma          2*/
!        calculate a multigroup adjoint collision.                              !/*ma          3*/
      use global_data                                                           !/*max         1*/
!                                                                               !/*max         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*max         3*/
      implicit integer(kindi) (i-n)                                             !/*max         4*/
!                                                                               !/*max         5*/
!                                                                               !/*ma          5*/
      ntyn=0                                                                    !/*max         6*/
      jsu=0                                                                     !/*max         7*/
      jg=jgp                                                                    !/*ma          6*/
      ip=ipt                                                                    !/*ma          7*/
      cmult=1.                                                                  !/*ma          8*/
!                                                                               !/*ma          9*/
!        do russian roulette on weight cutoff as required.                      !/*ma         10*/
      if(for(lfor+1,icl).ne.0)go to 10                                          !/*ma         11*/
      if(iplt.ne.1)t=fiml9(1,1)/fiml(1)                                         !/*ma         12*/
      if(iplt.eq.1)t=1./fismg                                                   !/*ma         13*/
      t=t/gamef                                                                 !/*max         8*/
      if(wgt.gt.t*wcs2tc(ip))go to 10                                           !/*ma         14*/
      t=t*wcs1tc(ip)                                                            !/*ma         15*/
      if(wgt.lt.t*rang())go to 190                                              !/*ma         16*/
!     QUESTION: Can t - wgt < 0 here???                                         !/*max         9*/
      call gain_pax (igain_all_pls_weight_cutoff, ip,                           !/*max        10*/
     &               0, t-wgt, (t-wgt)*erg)                                     !/*max        11*/
      call contrib_pwb (ip,ipwb_variance_weight_cutoff,icl,t-wgt)               !/*max        12*/
      wtfasv=wtfasv*t/wgt                                                       !/*ma         20*/
      wgt=t                                                                     !/*ma         21*/
!                                                                               !/*ma         22*/
!        sample the collision nuclide.                                          !/*ma         23*/
   10 wx=wgt                                                                    !/*ma         24*/
      mk=mat(lmat+icl)                                                          !/*ma         25*/
      ma=jmd(ljmd+mk)                                                           !/*ma         26*/
      m1=ma                                                                     !/*ma         27*/
      ll=lpmg+ipan(lipa+icl)-m1+npn*(jg-1+mgm(ip))                              !/*ma         28*/
      if(npq(lnpq+mk).eq.1)go to 30                                             !/*ma         29*/
      t=rang()*smg(lsmg+icl+mxa*(jg-1+mgm(ip)))                                 !/*ma         30*/
      do 20 ma=m1,jmd(ljmd+mk+1)-2                                              !/*ma         31*/
      t=t-pmg(ll+ma)*fme(lfme+ma)                                               !/*ma         32*/
   20 if(t.lt.0.)go to 30                                                       !/*ma         33*/
   30 iex=lme(llme+ip,ma)                                                       !/*ma         34*/
      mpan=ipan(lipa+icl)+ma-m1                                                 !/*ma4a        1*/
      pan(kpan+ip,1,mpan)=pan(kpan+ip,1,mpan)+1.                                !/*ma4a        2*/
      pan(kpan+ip,2,mpan)=pan(kpan+ip,2,mpan)+wgt                               !/*ma4a        3*/
!                                                                               !/*ma         38*/
!        special treatment for electrons.                                       !/*ma         39*/
      if(nxs(lnxs+11,iex).eq.0)go to 60                                         !/*ma         40*/
      if(deb.gt.pmf)go to 40                                                    !/*ma         41*/
      jgp=jgp-1                                                                 !/*ma         42*/
      wa=xss(jxs(ljxs+7,iex)-1+jgp)/xss(jxs(ljxs+7,iex)+jgp)                    !/*ma         43*/
      go to 150                                                                 !/*ma         44*/
   40 if(rang().gt.pfp)go to 50                                                 !/*ma         45*/
      wa=1.                                                                     !/*ma         46*/
      call rotas(zero+.9,uold,uuu,lev,irt)                                      !/*ma4b        1*/
      go to 150                                                                 !/*ma         48*/
   50 if(nxs(lnxs+11,iex).eq.2)go to 150                                        !/*ma         49*/
!                                                                               !/*ma         50*/
!        maybe sample fission if it is possible.                                !/*ma         51*/
   60 t=rang()*pmg(ll+ma)                                                       !/*ma         52*/
      if(jxs(ljxs+5,iex).eq.0)go to 90                                          !/*ma         53*/
      qf=fmg(lfmg+mpan)*xss(jxs(ljxs+5,iex)-1+jg)                               !/*ma4a        4*/
      if(t.gt.qf)go to 80                                                       !/*ma         55*/
      ipsc=104                                                                  !/*ma         56*/
!     QUESTION: What about PAX ???                                              !/*max        13*/
      call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,wx)                  !/*max        14*/
      t=t/xss(jxs(ljxs+5,iex)-1+jg)                                             !/*ma         58*/
      jgp=0                                                                     !/*ma         59*/
   70 t=t-gmg(lgmg+icl+mxa*(jgp+mgm(ip)))*xss(jxs(ljxs+3,iex)+jgp)*             !/*ma         60*/
     & xss(jxs(ljxs+4,iex)+jgp)                                                 !/*ma         61*/
      jgp=jgp+1                                                                 !/*ma         62*/
      if(t.gt.0..and.jgp.lt.jgm(1))go to 70                                     !/*ma4a        5*/
      call isos(uuu,lev)                                                        !/*ma4b        2*/
      go to 140                                                                 !/*ma         65*/
   80 t=t-qf                                                                    !/*ma         66*/
!                                                                               !/*ma         67*/
!        otherwise sample scattering.                                           !/*ma         68*/
   90 l=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)))-2                           !/*ma4c        1*/
      k=max(0,jg-nxs(lnxs+7,iex)-1)                                             !/*ma         70*/
      jgp=min(jgm(ip),jg+nxs(lnxs+6,iex))+1                                     !/*ma         71*/
  100 jgp=jgp-1                                                                 !/*ma         72*/
      if(jgp.eq.k)go to 110                                                     !/*ma         73*/
      ixcos=nint(xss(jxs(ljxs+18,iex)+jgp-1))+jg-                               !/*max        15*/
     & max(1,jgp-nxs(lnxs+6,iex))                                               !/*max        16*/
      t=t-gmg(lgmg+icl+mxa*(jgp-1+mgm(ip)))*xss(l+ixcos)                        !/*ma         75*/
      if(t.gt.0.)go to 100                                                      !/*ma         76*/
      call rotas(scat(0),uold,uuu,lev,irt)                                      !/*ma4b        3*/
      ipsc=5                                                                    !/*ma         78*/
      go to 140                                                                 !/*ma         79*/
!                                                                               !/*ma         80*/
!        turn into a neutron.                                                   !/*ma         81*/
  110 iex=lme(llme+1,ma)                                                        !/*ma         82*/
      ipsc=8                                                                    !/*ma         83*/
      ipt=1                                                                     !/*ma         84*/
      jan=1                                                                     !/*gpxc        1*/
      jgp=jgm(1)+1                                                              !/*ma         85*/
      l=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)+1))-2                         !/*ma4c        2*/
  120 jgp=jgp-1                                                                 !/*ma         87*/
      ixcos=jgm(2)*(jgp-1)+jg                                                   !/*ma         88*/
      t=t-gmg(lgmg+icl+mxa*(jgp-1+mgm(1)))*xss(l+ixcos)                         !/*ma         89*/
      if(t.gt.0..and.jgp.gt.1)go to 120                                         !/*ma4a        6*/
      call rotas(scat(1),uold,uuu,lev,irt)                                      !/*ma4b        4*/
!     QUESTION: What about PWB ???                                              !/*max        17*/
      call lose_pax (loss_photon_capture, 2, 1, wgt, wgt*erg)                   !/*max        18*/
      wa=smg(lsmg+icl+mxa*(jg-1+mgm(ip)))/                                      !/*ma         95*/
     & (totm*gmg(lgmg+icl+mxa*(jgp-1+mgm(ipt))))                                !/*ma         96*/
      wgt=wgt*wa                                                                !/*ma         97*/
      erg=xss(jxs(ljxs+1,mgegbt(ipt))+jgp-1)                                    !/*ma         98*/
      vel=gvl(lgvl+jgp+mgm(ipt))                                                !/*ma         99*/
      if(img.eq.0)go to 130                                                     !/*ma        100*/
      erg=erg+(rang()-.5)*xss(jxs(ljxs+1,mgegbt(1))+jgp-1+jgm(1))               !/*ma        101*/
      rm=xss(jxs(ljxs+1,mgegbt(1))+jgp-1+2*jgm(1))                              !/*ma        102*/
      vel=slite*sqrt(erg*(erg+2.*rm))/(erg+rm)                                  !/*ma        103*/
  130 call gain_pax (igain_all_pls_source, 1, 1, wgt, wgt*erg)                  !/*max        19*/
      call contrib_pwb (1,ipwb_external_source,icl,wgt)                         !/*max        20*/
      go to 170                                                                 !/*ma        108*/
!                                                                               !/*ma        109*/
!        increment the summary accounts.                                        !/*ma        110*/
  140 erg=xss(jxs(ljxs+1,mgegbt(ipt))+jgp-1)                                    !/*ma        111*/
      vel=gvl(lgvl+jgp+mgm(ipt))                                                !/*ma        112*/
      wa=smg(lsmg+icl+mxa*(jg-1+mgm(ip)))/                                      !/*ma        113*/
     & (totm*gmg(lgmg+icl+mxa*(jgp-1+mgm(ipt))))                                !/*ma        114*/
      if(img.eq.0)go to 150                                                     !/*ma        115*/
      erg=erg+(rang()-.5)*xss(jxs(ljxs+1,mgegbt(1))+jgp-1+jgm(1))               !/*ma        116*/
      rm=xss(jxs(ljxs+1,mgegbt(1))+jgp-1+2*jgm(1))                              !/*ma        117*/
      vel=slite*sqrt(erg*(erg+2.*rm))/(erg+rm)                                  !/*ma        118*/
  150 continue                                                                  !/*max        21*/
!                                                                               !/*max        22*/
!     We believe that this routine is visited only by neutrons (ipt==1),        !/*max        23*/
!     photons (ipt==2), or electrons masquerading as neutrons (ipt==1).         !/*max        24*/
!     Therefore we include an expirx case for unexpected particles.             !/*max        25*/
      if (ipt.eq.1) then                                                        !/*max        26*/
         if (erg.lt.eg0) then                                                   !/*max        27*/
            call lose_pax (loss_neutron_downscattering, 1,                      !/*max        28*/
     &                     0, zero, wgt*(eg0-erg))                              !/*max        29*/
         else if (erg.gt.eg0) then                                              !/*max        30*/
            call gain_pax (igain_neutron_upscattering, 1,                       !/*max        31*/
     &                     0, zero, wgt*(erg-eg0))                              !/*max        32*/
         endif                                                                  !/*max        33*/
      else if (ipt.eq.2) then                                                   !/*max        34*/
         if (erg.lt.eg0) then                                                   !/*max        35*/
            call lose_pax (loss_photon_compton_scatter, 2,                      !/*max        36*/
     &                     0, zero, wgt*(eg0-erg))                              !/*max        37*/
         else if (erg.gt.eg0) then                                              !/*max        38*/
            call gain_pax (igain_photon_p_annihilation, 2,                      !/*max        39*/
     &                     0, zero, wgt*(erg-eg0))                              !/*max        40*/
         endif                                                                  !/*max        41*/
      else                                                                      !/*max        42*/
         call expirx(1,'mgacol',                                                !/*max        43*/
     &        'unexpected particle type. contact the MCNPX developers.')        !/*max        44*/
      endif                                                                     !/*max        45*/
      wgt=wgt*wa                                                                !/*ma        122*/
      if(krflg.ne.0)call eventp(4)                                              !/*ma        123*/
      if(iptr.ne.0)call ptrak(4)                                                !/*ma4a        7*/
      t=wx-wgt                                                                  !/*ma        124*/
      pan(kpan+ipt,3,mpan)=pan(kpan+ipt,3,mpan)+t                               !/*ma4a        8*/
      if (ipt.eq.1) then                                                        !/*max        46*/
         call contrib_pwb (1,ipwb_neutron_capture,icl,-t)                       !/*max        47*/
         if(t.gt.zero) then                                                     !/*max        48*/
            call lose_pax (loss_neutron_downscattering, 1, 0, t, t*erg)         !/*max        49*/
            tmavtc(1,2)=tmavtc(1,2)+tme*t                                       !/*max        50*/
            tmavtc(1,3)=tmavtc(1,3)+tme*t                                       !/*max        51*/
         else                                                                   !/*max        52*/
!           QUESTION: Shouldn't negatives appear in the loss column???          !/*max        53*/
            call gain_pax (igain_neutron_upscattering, 1, 0, -t, -t*erg)        !/*max        54*/
         endif                                                                  !/*max        55*/
      else if (ipt.eq.2) then                                                   !/*max        56*/
         call contrib_pwb (2,ipwb_photon_capture,icl,-t)                        !/*max        57*/
         if(t.gt.zero) then                                                     !/*max        58*/
            call lose_pax (loss_photon_compton_scatter, 2, 0, t, t*erg)         !/*max        59*/
            tmavtc(2,2)=tmavtc(2,2)+tme*t                                       !/*max        60*/
            tmavtc(2,3)=tmavtc(2,3)+tme*t                                       !/*max        61*/
         else                                                                   !/*max        62*/
!           QUESTION: Shouldn't negatives appear in the loss column???          !/*max        63*/
            call gain_pax (igain_photon_p_annihilation,2,0,-t,-t*erg)           !/*max        64*/
         endif                                                                  !/*max        65*/
      else                                                                      !/*max        66*/
         call expirx(1,'mgacol',                                                !/*max        67*/
     &        'unexpected particle type. contact the MCNPX developers.')        !/*max        68*/
      endif                                                                     !/*max        69*/
  170 if(erg.gt.elc(ipt))go to 180                                              !/*ma        135*/
!                                                                               !/*ma        136*/
!        split excessive accumulated weight.                                    !/*ma        137*/
      wtfasv=wtfasv*wa                                                          !/*ma        138*/
      if(wtfasv.lt.1.5)return                                                   !/*ma        139*/
      npa=min(20,int(wtfasv+.5))                                                !/*ma        140*/
      cmult=npa                                                                 !/*ma        141*/
      wtfasv=wtfasv/npa                                                         !/*ma        142*/
      wgt=wgt/npa                                                               !/*ma        143*/
      npa=npa-1                                                                 !/*ma        144*/
      call gain_pax (igain_all_pls_weight_cutoff,ipt,npa,zero,zero)             !/*max        70*/
      pac(kpac+ipt,1,icl)=pac(kpac+ipt,1,icl)+npa                               !/*ma        146*/
      if(nmaz.ne.0.and.npa.ne.0)call rslmaz(2,npa)                              !/*ma4b        5*/
      call bankit(23)                                                           !/*ma4a        9*/
      return                                                                    !/*ma        148*/
!                                                                               !/*ma        149*/
  180 nter = loss_all_pls_energy_cutoff                                         !/*max        71*/
      return                                                                    !/*ma        151*/
  190 nter = loss_all_pls_weight_cutoff                                         !/*max        72*/
      return                                                                    !/*ma        153*/
      end                                                                       !/*ma        154*/

!#ident mgcolp.F from deck gp                                                   !/*gp          1*/
      subroutine mgcolp                                                         !/*gp4a        1*/
!        calculate a photon multigroup forward collision.                       !/*gp          3*/
      use global_data                                                           !/*gpx         1*/
!                                                                               !/*gpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gpx         3*/
      implicit integer(kindi) (i-n)                                             !/*gpx         4*/
!                                                                               !/*gpx         5*/
!                                                                               !/*gp          6*/
      ntyn=0                                                                    !/*gpx         6*/
      jsu=0                                                                     !/*gpx         7*/
      jg=jgp                                                                    !/*gp          7*/
!                                                                               !/*gp          8*/
!        sample analog capture if required.                                     !/*gp          9*/
      if(erg.gt.emcf(2))go to 10                                                !/*gp         10*/
      if(xss(jxs(ljxs+6,iex)-1+jg).lt.rang()*xss(jxs(ljxs+2,iex)-1+jg))         !/*gp         11*/
     & go to 50                                                                 !/*gp         12*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+wgt                                 !/*gp4a        2*/
      nter = loss_photon_capture                                                !/*gpx         8*/
      if(krflg.ne.0)call eventp(4)                                              !/*gp         15*/
      if(iptr.ne.0)call ptrak(4)                                                !/*gp4a        3*/
      return                                                                    !/*gp         16*/
!                                                                               !/*gp         17*/
!        otherwise reduce weight by capture probability.                        !/*gp         18*/
   10 wc=wgt*xss(jxs(ljxs+6,iex)-1+jg)/xss(jxs(ljxs+2,iex)-1+jg)                !/*gp         19*/
      call lose_pax (loss_photon_capture, 2, 0, wc, wc*eg0)                     !/*gpx         9*/
      call contrib_pwb (2,ipwb_photon_capture,icl,-wc)                          !/*gpx        10*/
      pan(kpan+2,1,mpan)=pan(kpan+2,1,mpan)+1.                                  !/*gp4a        4*/
      pan(kpan+2,2,mpan)=pan(kpan+2,2,mpan)+wgt                                 !/*gp4a        5*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+wc                                  !/*gp4a        6*/
      tmavtc(2,2)=tmavtc(2,2)+tme*wc                                            !/*cor4-1    171*/
      tmavtc(2,3)=tmavtc(2,3)+tme*wc                                            !/*cor4-1    172*/
      wgt=wgt-wc                                                                !/*gp         26*/
!                                                                               !/*gp         27*/
!        do russian roulette on weight cutoff as required.                      !/*gp         28*/
   20 if(wwp(1,4).ne.0.)go to 50                                                !/*gp4c        1*/
      if(for(lfor+2,icl).ne.0)go to 50                                          !/*gp         30*/
      if(iplt.ne.1)t=fiml9(1,1)/fiml(1)                                         !/*gp         31*/
      if(iplt.eq.1)t=1./fismg                                                   !/*gp         32*/
      t=t/gamef                                                                 !/*gpx        11*/
      if(idx.eq.0)go to 30                                                      !/*gp         33*/
      if(wgt.gt.t*dxw(2,2))go to 50                                             !/*gp         34*/
      t=t*dxw(2,1)                                                              !/*gp         35*/
      go to 40                                                                  !/*gp         36*/
   30 if(wgt.gt.t*wcs2tc(2))go to 50                                            !/*gp         37*/
      t=t*wcs1tc(2)                                                             !/*gp         38*/
   40 if(wgt.lt.t*rang())go to 90                                               !/*gp         39*/
      call gain_pax (igain_all_pls_weight_cutoff, 2,                            !/*gpx        12*/
     &               0, t-wgt, (t-wgt)*eg0)                                     !/*gpx        13*/
      call contrib_pwb (2,ipwb_variance_weight_cutoff,icl,t-wgt)                !/*gpx        14*/
      wgt=t                                                                     !/*gp         43*/
!                                                                               !/*gp         44*/
!        sample scattering.                                                     !/*gp         45*/
   50 n=rang()+xss(jxs(ljxs+19,iex)-1+jg)/                                      !/*gp         46*/
     & (xss(jxs(ljxs+2,iex)-1+jg)-xss(jxs(ljxs+6,iex)-1+jg))                    !/*gp         47*/
      if(n.le.0)go to 100                                                       !/*gp         48*/
      ipsc=5                                                                    !/*gp         49*/
      call gain_pax (igain_photon_1st_fluorescence, 2,                          !/*gpx        15*/
     &               n-1, (n-1)*wgt, zero)                                      !/*gpx        16*/
      call contrib_pwb (2,ipwb_photon_capture,icl,-wgt)                         !/*gpx        17*/
      call contrib_pwb (2,ipwb_photon_flourescence,icl,n*wgt)                   !/*gpx        18*/
      pan(kpan+1,7,mpan)=pan(kpan+1,7,mpan)+wgt*(n-1)                           !/*gp4c2       3*/
      jn=max(jg-nxs(lnxs+6,iex)-1,0)                                            !/*gp         55*/
      l=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)))+                            !/*gp4c        2*/
     & nint(xss(jxs(ljxs+18,iex)+jg-1))-2-jn                                    !/*gp4c        3*/
      npa=1                                                                     !/*gp         58*/
      do 70 i=1,n                                                               !/*gp         59*/
   55 t=rang()*xss(jxs(ljxs+19,iex)-1+jg)                                       !/*gp4a        8*/
      jgp=jn                                                                    !/*gp         61*/
   60 t=t-xss(l+jgp)                                                            !/*gp         62*/
      jgp=jgp+1                                                                 !/*gp         63*/
      if(t.gt.0.)go to 60                                                       !/*gp         64*/
      if(jgp.gt.jgm(2))go to 55                                                 !/*gp4a        9*/
      ixcos=nint(xss(jxs(ljxs+18,iex)+jg-1))+jgp-jn-1                           !/*gpx        19*/
      erg=xss(jxs(ljxs+1,iex)+jgp-1)                                            !/*gp         66*/
      call rotas(scat(0),uold,uuu,lev,irt)                                      !/*gp4b        3*/
      if(i.eq.n)go to 80                                                        !/*gp         68*/
      call gain_pax (igain_photon_1st_fluorescence, 2, 0, zero, wgt*erg)        !/*gpx        20*/
#ifdef SPABI                                                                    /*!gpx        21*/
      if (is_spabi.gt.0) then                                                   !/*gpxa        1*/
         call sec_par_bias(2,2,erg,igain_photon_1st_fluorescence,               !/*gpx        23*/
     &   ipwb_photon_flourescence,-3,22)                                        !/*gpx        24*/
      go to 70                                                                  !/*gpx        25*/
      endif                                                                     !/*gpx        26*/
#endif                                                                          /*!gpx        27*/
      call bankit(22)                                                           !/*gpx        28*/
   70 continue                                                                  !/*gpx        29*/
   80 cmult=n                                                                   !/*gp         71*/
      if(krflg.ne.0)call eventp(4)                                              !/*gp         72*/
      if(iptr.ne.0)call ptrak(4)                                                !/*gp4a       11*/
      if (erg.lt.eg0) call lose_pax (loss_photon_compton_scatter, 2,            !/*gpx        30*/
     &                               0, zero, wgt*(eg0-erg))                    !/*gpx        31*/
      if (erg.gt.eg0) call gain_pax (igain_photon_p_annihilation, 2,            !/*gpx        32*/
     &                               0, zero, wgt*(erg-eg0))                    !/*gpx        33*/
      if (erg.lt.elc(2)) nter = loss_all_pls_energy_cutoff                      !/*gpx        34*/
      return                                                                    !/*gp         76*/
!                                                                               !/*gp         77*/
   90 nter = loss_all_pls_weight_cutoff                                         !/*gpx        35*/
      return                                                                    !/*gp         79*/
  100 nter = loss_photon_capture                                                !/*gpx        36*/
      return                                                                    !/*gp         81*/
      end                                                                       !/*gp         82*/

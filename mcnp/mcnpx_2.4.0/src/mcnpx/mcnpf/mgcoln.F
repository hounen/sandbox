!#ident mgcoln.F from deck gn                                                   !/*gn          1*/
      subroutine mgcoln                                                         !/*gn4a        1*/
!        calculate a neutron multigroup forward collision.                      !/*gn          3*/
      use global_data                                                           !/*gnx         1*/
!                                                                               !/*gnx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gnx         3*/
      implicit integer(kindi) (i-n)                                             !/*gnx         4*/
!                                                                               !/*gnx         5*/
!                                                                               !/*gn          6*/
      jg=jgp                                                                    !/*gn          7*/
      je=0                                                                      !/*gn          8*/
      npa=1                                                                     !/*gn          9*/
      mtp=0                                                                     !/*gn4c        1*/
!                                                                               !/*gn         10*/
!        make photons if required and possible.                                 !/*gn         11*/
      if(kpt(2).eq.0)go to 80                                                   !/*gn         12*/
      if(jxs(ljxs+20,iex).eq.0)go to 110                                        !/*gn         13*/
      n=xss(jxs(ljxs+20,iex)-1+jgp)/xss(jxs(ljxs+2,iex)-1+jgp)+rang()           !/*gn         14*/
      if(n.le.0)go to 110                                                       !/*gn         15*/
      call savpar(0,0)                                                          !/*gn4c2       1*/
      call gain_pax (igain_photon_from_neutrons, 2, n, n*wgt, zero)             !/*gnx         6*/
      call contrib_pwb (2,ipwb_photon_from_neutrons,icl,n*wgt)                  !/*gnx         7*/
      pan(kpan+1,6,mpan)=pan(kpan+1,6,mpan)+n                                   !/*gn4c2       2*/
      pan(kpan+1,7,mpan)=pan(kpan+1,7,mpan)+wgt*n                               !/*gn4c2       3*/
      pcc(kpcc+1,icl)=pcc(kpcc+1,icl)+n                                         !/*gn         26*/
      pcc(kpcc+2,icl)=pcc(kpcc+2,icl)+wgt*n                                     !/*gn         27*/
      l=jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+13,iex)+1))+(jg-1)*jgm(2)           !/*gn4c        2*/
      ipt=2                                                                     !/*gn         29*/
      jan=0                                                                     !/*gnxc        1*/
      ncp=0                                                                     !/*gn         30*/
#ifdef MESHTAL                                                                  /*!gnx         8*/
      mspf=2                                                                    !/*gnx         9*/
#endif                                                                          /*!gnx        10*/
      idx=0                                                                     !/*gn         31*/
      do 30 i=1,ndx(2)                                                          !/*gn         32*/
   30 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+(zzz-dxx(2,3,i))**2.lt.        !/*gn         33*/
     & dxx(2,5,i))idx=i                                                         !/*gn         34*/
      vel=slite                                                                 !/*gn         35*/
      st=totm                                                                   !/*gn         36*/
      do 50 i=1,n                                                               !/*gn         37*/
      ipsc=8                                                                    !/*gn         38*/
      cmult=1.                                                                  !/*gn         39*/
   35 t=rang()*xss(jxs(ljxs+20,iex)-1+jg)                                       !/*gn4a        4*/
      jgp=0                                                                     !/*gn         41*/
   40 t=t-xss(l+jgp)                                                            !/*gn         42*/
      jgp=jgp+1                                                                 !/*gn         43*/
      if(t.gt.0.)go to 40                                                       !/*gn         44*/
      if(jgp.gt.jgm(2))go to 35                                                 !/*gn4a        5*/
      ixcos=(jg-1)*jgm(2)+jgp                                                   !/*gn         45*/
      erg=xss(jxs(ljxs+1,mgegbt(2))+jgp-1)                                      !/*gn         46*/
      call gain_pax (igain_photon_from_neutrons, 2, 0, zero, wgt*erg)           !/*gnx        11*/
      pcc(kpcc+3,icl)=pcc(kpcc+3,icl)+wgt*erg                                   !/*gn         48*/
      pan(kpan+1,8,mpan)=pan(kpan+1,8,mpan)+wgt*erg                             !/*gn4c2       4*/
      febl(kfeb+1,jgp)=febl(kfeb+1,jgp)+1.                                      !/*gn4b        1*/
      febl(kfeb+2,jgp)=febl(kfeb+2,jgp)+wgt                                     !/*gn4b        2*/
      call rotas(scat(1),uold,uuu,lev,irt)                                      !/*gn4b        3*/
      if(ndet(2).ne.0)call tallyd                                               !/*gn         53*/
      if(kdb.ne.0)return                                                        !/*gn         54*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*gn         55*/
      if(kdb.ne.0)return                                                        !/*gn         56*/
#ifdef SPABI                                                                    /*!gnx        12*/
      if (is_spabi.gt.0) then                                                   !/*gnxa        1*/
         call sec_par_bias(2,1,erg,igain_photon_from_neutrons,                  !/*gnx        14*/
     &   ipwb_photon_from_neutrons,-1,19)                                       !/*gnx        15*/
         go to 50                                                               !/*gnx        16*/
      endif                                                                     !/*gnx        17*/
#endif                                                                          /*!gnx        18*/
      call bankit(19)                                                           !/*gnx        19*/
   50 continue                                                                  !/*gnx        20*/
      totm=st                                                                   !/*gn         58*/
      call getpar(0,0)                                                          !/*gn4c2       5*/
!                                                                               !/*gn         64*/
!        special treatment for electrons.                                       !/*gn         65*/
   80 if(nxs(lnxs+11,iex).eq.0)go to 110                                        !/*gn         66*/
      n=1                                                                       !/*gn         67*/
      if(deb.gt.pmf)go to 90                                                    !/*gn         68*/
      je=1                                                                      !/*gn         69*/
      jgp=jgp+1                                                                 !/*gn         70*/
      go to 140                                                                 !/*gn         71*/
   90 if(rang().gt.pfp)go to 100                                                !/*gn         72*/
      je=1                                                                      !/*gn         73*/
      call rotas(zero+.9,uold,uuu,lev,irt)                                      !/*gn4b        4*/
      go to 140                                                                 !/*gn         75*/
  100 if(nxs(lnxs+11,iex).eq.2)go to 140                                        !/*gn         76*/
!                                                                               !/*gn         77*/
!        sample analog capture if required.                                     !/*gn         78*/
  110 pan(kpan+1,1,mpan)=pan(kpan+1,1,mpan)+1.                                  !/*gn4b        5*/
      pan(kpan+1,2,mpan)=pan(kpan+1,2,mpan)+wgt                                 !/*gn4b        6*/
      if(erg.gt.emcf(1).and.wc1(1).ne.0)go to 120                               !/*gn4b        7*/
      t=xss(jxs(ljxs+6,iex)-1+jg)+rtc(krtc+8,iex)                               !/*gn         80*/
      r=rang()*xss(jxs(ljxs+2,iex)-1+jg)                                        !/*gn         81*/
      if(t.lt.r)go to 170                                                       !/*gn         82*/
      pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+wgt                                 !/*gn4a        8*/
      if (r.ge.xss(jxs(ljxs+6,iex)-1+jg)) then                                  !/*gnx        21*/
         nter = loss_neutron_loss_to_fission                                    !/*gnx        22*/
      else                                                                      !/*gnx        23*/
         nter = loss_neutron_capture                                            !/*gnx        24*/
      endif                                                                     !/*gnx        25*/
      if(krflg.ne.0)call eventp(4)                                              !/*gn         86*/
      if(iptr.ne.0)call ptrak(4)                                                !/*gn4a        9*/
      if(nsr.ne.71)return                                                       !/*gn         87*/
      rlttc(2,1)=rlttc(2,1)+wgt*tme                                             !/*gn4b        8*/
      rlttc(2,2)=rlttc(2,2)+wgt                                                 !/*gn4b        9*/
      if (nter.eq.loss_neutron_loss_to_fission) then                            !/*gnx        26*/
         rlttc(4,1)=rlttc(4,1)+wgt*tme                                          !/*gnx        27*/
      endif                                                                     !/*gnx        28*/
      sumktc(2)=sumktc(2)+wgt*rtc(krtc+10,iex)*rtc(krtc+8,iex)/t                !/*gn         89*/
      return                                                                    !/*gn         90*/
!                                                                               !/*gn         91*/
!        otherwise reduce weight by capture probability.                        !/*gn         92*/
  120 t8=wgt/xss(jxs(ljxs+2,iex)-1+jg)                                          !/*gn         93*/
      wc=t8*(xss(jxs(ljxs+6,iex)-1+jg)+rtc(krtc+8,iex))                         !/*gn         94*/
      if(nsr.ne.71)go to 130                                                    !/*gn         95*/
      rlttc(2,1)=rlttc(2,1)+wc*tme                                              !/*gn4b       11*/
      rlttc(2,2)=rlttc(2,2)+wc                                                  !/*gn4b       12*/
      rlttc(4,1)=rlttc(4,1)+t8*rtc(krtc+8,iex)*tme                              !/*gn4b       13*/
      sumktc(2)=sumktc(2)+t8*rtc(krtc+10,iex)*rtc(krtc+8,iex)                   !/*gn4b       14*/
      pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+t8*xss(jxs(ljxs+6,iex)-1+jg)        !/*gn4b       15*/
      pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)+t8*rtc(krtc+8,iex)                  !/*gn4b       16*/
  130 wgt=wgt-wc                                                                !/*gn4b       17*/
      tmavtc(1,2)=tmavtc(1,2)+t8*xss(jxs(ljxs+6,iex)-1+jg)*tme                  !/*gn4b       19*/
      tmavtc(1,3)=tmavtc(1,3)+wc*tme                                            !/*gn4b       20*/
      call lose_pax (loss_neutron_capture, 1,                                   !/*gnx        29*/
     & 0,t8*xss(jxs(ljxs+6,iex)-1+jg),t8*xss(jxs(ljxs+6,iex)-1+jg)*eg0)         !/*gnx        30*/
      call lose_pax (loss_neutron_loss_to_fission, 1,                           !/*gnx        31*/
     & 0, t8*rtc(krtc+8,iex), t8*rtc(krtc+8,iex)*eg0)                           !/*gnx        32*/
      call contrib_pwb (1,ipwb_neutron_capture,icl,                             !/*gnx        33*/
     &                  -t8*xss(jxs(ljxs+6,iex)-1+jg))                          !/*gnx        34*/
      call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,                     !/*gnx        35*/
     &                  -t8*rtc(krtc+8,iex))                                    !/*gnx        36*/
      if(nsr.ne.71)pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+wc                     !/*gn4b       22*/
!                                                                               !/*gn        109*/
!        do russian roulette on weight cutoff as required.                      !/*gn        110*/
  140 if(wwp(1,4).ne.0.)go to 170                                               !/*gn4c        3*/
      if(for(lfor+1,icl).ne.0)go to 170                                         !/*gn        112*/
      if(iplt.ne.1)t=fiml9(1,1)/fiml(1)                                         !/*gn        113*/
      if(iplt.eq.1)t=1./fismg                                                   !/*gn        114*/
      t=t/gamef                                                                 !/*gnx        37*/
      if(idx.eq.0)go to 150                                                     !/*gn        115*/
      if(wgt.gt.t*dxw(1,2))go to 170                                            !/*gn        116*/
      t=t*dxw(1,1)                                                              !/*gn        117*/
      go to 160                                                                 !/*gn        118*/
  150 if(wgt.gt.t*wcs2tc(1))go to 170                                           !/*gn        119*/
      t=t*wcs1tc(1)                                                             !/*gn        120*/
  160 if(wgt.lt.t*rang())go to 270                                              !/*gn        121*/
      call gain_pax (igain_all_pls_weight_cutoff, 1,                            !/*gnx        38*/
     &               0, t-wgt, (t-wgt)*eg0)                                     !/*gnx        39*/
      call contrib_pwb (1,ipwb_variance_weight_cutoff,icl,t-wgt)                !/*gnx        40*/
      wgt=t                                                                     !/*gn        125*/
!                                                                               !/*gn        126*/
!        maybe sample fission if it is possible.                                !/*gn        127*/
  170 if(je.ne.0.or.nxs(lnxs+11,iex).eq.2)go to 260                             !/*gn        128*/
      if(jxs(ljxs+3,iex).eq.0.or.lfcl(llfc+icl).ne.0)go to 200                  !/*gn        129*/
      if(rang()*(xss(jxs(ljxs+2,iex)-1+jg)-xss(jxs(ljxs+6,iex)-1+jg))           !/*gn        130*/
     & .gt.xss(jxs(ljxs+3,iex)-1+jg))go to 200                                  !/*gn        131*/
      n=xss(jxs(ljxs+4,iex)-1+jg)+rang()                                        !/*gn        132*/
      if(n.le.0)go to 280                                                       !/*gn        133*/
      ipsc=104                                                                  !/*gn        134*/
      mtp=18                                                                    !/*gn4c        4*/
      call gain_pax (igain_neutron_prompt_fission,1,n, n*wgt,zero)              !/*gnx        41*/
      call lose_pax (loss_neutron_loss_to_fission,1, 1, wgt, wgt*erg)           !/*gnx        42*/
      call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,-wgt)                !/*gnx        43*/
      call contrib_pwb (1,ipwb_neutron_fission,icl,n*wgt)                       !/*gnx        44*/
      pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)+wgt*(n-1)                           !/*gn4a       17*/
      do 190 i=1,n                                                              !/*gn        143*/
      call isos(uuu,lev)                                                        !/*gn4b       25*/
  175 r=rang()                                                                  !/*gn4a       18*/
      jgp=0                                                                     !/*gn        146*/
  180 r=r-xss(jxs(ljxs+5,iex)+jgp)                                              !/*gn        147*/
      jgp=jgp+1                                                                 !/*gn        148*/
      if(r.gt.0.)go to 180                                                      !/*gn        149*/
      if(jgp.gt.jgm(1))go to 175                                                !/*gn4a       19*/
      erg=xss(jxs(ljxs+1,mgegbt(1))+jgp-1)                                      !/*gn        150*/
      vel=gvl(lgvl+jgp+mgm(1))                                                  !/*gn        151*/
      call gain_pax (igain_neutron_prompt_fission, 1, 0, zero, wgt*erg)         !/*gnx        45*/
      if(i.eq.n)go to 260                                                       !/*gn        153*/
#ifdef SPABI                                                                    /*!gnx        46*/
      if (is_spabi.gt.0) then                                                   !/*gnxa        2*/
         call sec_par_bias(1,1,erg,igain_neutron_fission,0,-2,20)               !/*gnx        48*/
         go to 190                                                              !/*gnx        49*/
      endif                                                                     !/*gnx        50*/
#endif                                                                          /*!gnx        51*/
      call bankit(20)                                                           !/*gnx        52*/
  190 continue                                                                  !/*gnx        53*/
!                                                                               !/*gn        155*/
!        otherwise sample scattering.                                           !/*gn        156*/
  200 d=xss(jxs(ljxs+2,iex)-1+jg)-xss(jxs(ljxs+6,iex)-1+jg)                     !/*gn        157*/
      if(jxs(ljxs+3,iex).ne.0)d=d-xss(jxs(ljxs+3,iex)-1+jg)                     !/*gn        158*/
      n=rang()+xss(jxs(ljxs+19,iex)-1+jg)/d                                     !/*gn        159*/
      if(n.le.0)go to 280                                                       !/*gn        160*/
      ipsc=5                                                                    !/*gn        161*/
      if(n.eq.1)go to 210                                                       !/*gn        162*/
      call gain_pax (igain_neutron_n_xn, 1, n, n*wgt, zero)                     !/*gnx        54*/
      call lose_pax (loss_neutron_loss_to_n_xn, 1, 1, wgt, wgt*erg)             !/*gnx        55*/
      call contrib_pwb (1,ipwb_neutron_n_xn,icl,n*wgt)                          !/*gnx        56*/
      pan(kpan+1,5,mpan)=pan(kpan+1,5,mpan)+wgt*(n-1)                           !/*gn4a       21*/
  210 jn=max(jg-nxs(lnxs+6,iex)-1,0)                                            !/*gn        170*/
      l=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)))+                            !/*gn4c        5*/
     & nint(xss(jxs(ljxs+18,iex)+jg-1))-2-jn                                    !/*gn4c        6*/
      do 240 i=1,n                                                              !/*gn        173*/
  215 t=rang()*xss(jxs(ljxs+19,iex)-1+jg)                                       !/*gn4a       22*/
      jgp=jn                                                                    !/*gn        175*/
  220 t=t-xss(l+jgp)                                                            !/*gn        176*/
      jgp=jgp+1                                                                 !/*gn        177*/
      if(t.gt.0.)go to 220                                                      !/*gn        178*/
      if(jgp.gt.jgm(1))go to 215                                                !/*gn4a       23*/
      ixcos=nint(xss(jxs(ljxs+18,iex)+jg-1))+jgp-jn-1                           !/*gnx        57*/
      erg=xss(jxs(ljxs+1,mgegbt(1))+jgp-1)                                      !/*gn        180*/
      vel=gvl(lgvl+jgp+mgm(1))                                                  !/*gn        181*/
      if(img.eq.0)go to 230                                                     !/*gn        182*/
      erg=erg+(rang()-.5)*xss(jxs(ljxs+1,mgegbt(1))+jgp-1+jgm(1))               !/*gn        183*/
      rm=xss(jxs(ljxs+1,mgegbt(1))+jgp-1+2*jgm(1))                              !/*gn        184*/
      vel=slite*sqrt(erg*(erg+2.*rm))/(erg+rm)                                  !/*gn        185*/
  230 call rotas(scat(0),uold,uuu,lev,irt)                                      !/*gn4b       26*/
      if(n.eq.1)go to 250                                                       !/*gn        187*/
      call gain_pax (igain_neutron_n_xn, 1, 0, zero, wgt*erg)                   !/*gnx        58*/
      if(i.eq.n)go to 260                                                       !/*gn        189*/
#ifdef SPABI                                                                    /*!gnx        59*/
      if (is_spabi.gt.0) then                                                   !/*gnxa        3*/
         call sec_par_bias(1,1,erg,igain_neutron_upscattering,0,0,21)           !/*gnx        61*/
         go to 240                                                              !/*gnx        62*/
      endif                                                                     !/*gnx        63*/
#endif                                                                          /*!gnx        64*/
      call bankit(21)                                                           !/*gnx        65*/
  240 continue                                                                  !/*gnx        66*/
  250 if (erg.lt.eg0) call lose_pax (loss_neutron_downscattering, 1,            !/*gnx        67*/
     &                               0, zero, wgt*(eg0-erg))                    !/*gnx        68*/
      if (erg.gt.eg0) call gain_pax (igain_neutron_upscattering, 1,             !/*gnx        69*/
     &                               0, zero, wgt*(erg-eg0))                    !/*gnx        70*/
  260 cmult=n                                                                   !/*gn        193*/
      smultc(1)=smultc(1)+wgt*(n-1)                                             !/*gn        194*/
      if(krflg.ne.0)call eventp(4)                                              !/*gn        195*/
      if(iptr.ne.0)call ptrak(4)                                                !/*gn4a       25*/
      if (erg.lt.elc(1)) nter = loss_all_pls_energy_cutoff                      !/*gnx        71*/
      return                                                                    !/*gn        197*/
!                                                                               !/*gn        198*/
  270 nter = loss_all_pls_weight_cutoff                                         !/*gnx        72*/
      return                                                                    !/*gn        200*/
  280 nter = loss_neutron_capture                                               !/*gnx        73*/
      return                                                                    !/*gn        202*/
      end                                                                       !/*gn        203*/

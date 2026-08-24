!#ident colidn.F from deck cn                                                   !/*cn          1*/
      subroutine colidn                                                         !/*cn          2*/
!        calculate the collision of a neutron with a nucleus.                   !/*cn          3*/
      use global_data                                                           !/*cnx         1*/
!                                                                               !/*cnx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*cnx         3*/
      implicit integer(kindi) (i-n)                                             !/*cnx         4*/
!                                                                               !/*cnx         5*/
      dimension vr(3)                                                           !/*cn          5*/
!                                                                               !/*cn          6*/
!        save the incoming direction.                                           !/*cn          7*/
      uold(1)=uuu                                                               !/*cn4b        1*/
      uold(2)=vvv                                                               !/*cn4b        2*/
      uold(3)=www                                                               !/*cn4b        3*/
      jsu=0                                                                     !/*cn         11*/
      ntyn=0                                                                    !/*cn         12*/
      if(mcal.eq.2)go to 290                                                    !/*cn         13*/
      mk=mat(lmat+icl)                                                          !/*cn         16*/
      if(mk.eq.0)call expirx(1,'colidn','collision in void.')                   !/*cn4c        1*/
      m=jmd(ljmd+mk)                                                            !/*cn         17*/
      m1=m                                                                      !/*cn         18*/
!                                                                               !/*cn4c        9*/
!        sample the nuclide, using the cumulative total cross section.          !/*cn4c       10*/
    6 if(npq(lnpq+mk).eq.1)go to 20                                             !/*cn4c       11*/
      c=rang()*totm                                                             !/*cn         20*/
    8 do 10 m=m1,jmd(ljmd+mk+1)-2                                               !/*cn4c       12*/
      c=c-rtc(krtc+5,lme(llme+1,m))*fme(lfme+m)                                 !/*cn         22*/
   10 if(c.lt.0.)go to 20                                                       !/*cn         23*/
   20 iex=lme(llme+1,m)                                                         !/*cn         24*/
      iexp=m                                                                    !/*cn4b        4*/
      mtp=2                                                                     !/*cn4b        5*/
      mpan=ipan(lipa+icl)+m-m1                                                  !/*cn4a        1*/
      if(mcal.eq.1)go to 300                                                    !/*cn         26*/
      iet=lmt(llmt+m)                                                           !/*cn         27*/
      if(erg.gt.esa(lesa+iet))iet=0                                             !/*cn         28*/
      if(rtc(krtc+11,iex).ge.0.)iet=0                                           !/*cn4c       13*/
!                                                                               !/*cn4a       19*/
!**************************  make photons  *****************************        !/*cn4a       20*/
!                                                                      *        !/*cn4a       21*/
      if(kpt(2).eq.0)go to 110                                                  !/*cn4a       22*/
      if(gwt(lgwt+icl).eq.-1.e6)go to 110                                       !/*cn         79*/
      if((jxs(ljxs+12,iex).eq.0.or.npikmt.ne.0).and.totgp1.eq.0.)               !/*cn         80*/
     & go to 110                                                                !/*cn         81*/
      if(tme.lt.tco(2))call acegam                                              !/*cn4a       23*/
      if(kdb.ne.0)return                                                        !/*cn         83*/
!                                                                               !/*cnx         8*/
!**********************  make charged particles  ***********************        !/*cnx         9*/
!                                                                      *        !/*cnx        10*/
  110 if(nxs(lnxs+7,iex).ne.0) call acecp                                       !/*cnx        11*/
!                                                                               !/*cnx        12*/
!                                                                               !/*cn         84*/
!*****************************  capture  *******************************        !/*cn         85*/
!                                                                      *        !/*cn         86*/
!        calculate fission separately in the kcode case.                        !/*cn         87*/
      if(nsr.eq.71)call colidk                                                  !/*cnx        13*/
      el=eg0                                                                    !/*cn4c       14*/
      if(kdb.ne.0)return                                                        !/*cn         89*/
      pan(kpan+1,1,mpan)=pan(kpan+1,1,mpan)+1.                                  !/*cn4a       24*/
      pan(kpan+1,2,mpan)=pan(kpan+1,2,mpan)+wgt                                 !/*cn4a       25*/
!                                                                               !/*cn         92*/
!        sample analog capture if required.                                     !/*cn         93*/
      if(wc1(1).ne.0..and.erg.gt.emcf(1))go to 120                              !/*cn         94*/
      t1=rtc(krtc+3,iex)+rtc(krtc+8,iex)                                        !/*cn         95*/
      r=rang()*rtc(krtc+5,iex)                                                  !/*cn         96*/
      if(t1.lt.r)go to 160                                                      !/*cn         97*/
      if (r.ge.rtc(krtc+3,iex)) then                                            !/*cnx        14*/
         pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)-wgt                              !/*cnxc        4*/
         nter = loss_neutron_loss_to_fission                                    !/*cnx        16*/
      else                                                                      !/*cnx        17*/
         pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+wgt                              !/*cnx        18*/
         nter = loss_neutron_capture                                            !/*cnx        19*/
      endif                                                                     !/*cnx        20*/
      if(krflg.ne.0)call eventp(4)                                              !/*cn        101*/
      if(iptr.ne.0)call ptrak(4)                                                !/*cn4a       30*/
      if(nsr.ne.71)return                                                       !/*cn        102*/
      rlttc(2,1)=rlttc(2,1)+wgt*tme                                             !/*cn4b        7*/
      rlttc(2,2)=rlttc(2,2)+wgt                                                 !/*cn4b        8*/
      if (nter.eq.loss_neutron_loss_to_fission)                                 !/*cnx        21*/
     & rlttc(4,1)=rlttc(4,1)+wgt*tme                                            !/*cnx        22*/
      sumktc(2)=sumktc(2)+wgt*rtc(krtc+10,iex)*rtc(krtc+8,iex)/t1               !/*cn        104*/
      return                                                                    !/*cn        105*/
!                                                                               !/*cn        106*/
!        otherwise simulate capture by weight reduction.                        !/*cn        107*/
  120 t8=wgt/rtc(krtc+5,iex)                                                    !/*cn        108*/
      t1=t8*(rtc(krtc+3,iex)+rtc(krtc+8,iex))                                   !/*cn        109*/
      if(nsr.ne.71)go to 130                                                    !/*cn        110*/
      rlttc(2,1)=rlttc(2,1)+t1*tme                                              !/*cn4b       10*/
      rlttc(2,2)=rlttc(2,2)+t1                                                  !/*cn4b       11*/
      rlttc(4,1)=rlttc(4,1)+t8*rtc(krtc+8,iex)*tme                              !/*cn4b       12*/
      sumktc(2)=sumktc(2)+t8*rtc(krtc+10,iex)*rtc(krtc+8,iex)                   !/*cn4a       41*/
  130 wgt=wgt-t1                                                                !/*cn        116*/
      tmavtc(1,2)=tmavtc(1,2)+t8*rtc(krtc+3,iex)*tme                            !/*cn4b       13*/
      tmavtc(1,3)=tmavtc(1,3)+tme*t1                                            !/*cn        119*/
      call lose_pax (loss_neutron_capture, 1,                                   !/*cnx        23*/
     &               0, t8*rtc(krtc+3,iex), t8*rtc(krtc+3,iex)*el)              !/*cnx        24*/
      call contrib_pwb (1,ipwb_neutron_capture,icl,-t8*rtc(krtc+3,iex))         !/*cnxc        8*/
      pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+t8*rtc(krtc+3,iex)                  !/*cnxc        9*/
      if(rtc(krtc+8,iex).gt.zero)then                                           !/*cnxc       10*/
         sw=wgt                                                                 !/*cnxc       11*/
         wgt=t8*rtc(krtc+8,iex)                                                 !/*cnxc       12*/
         pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)-wgt                              !/*cnxc       13*/
         call lose_pax (loss_neutron_loss_to_fission,1,0,wgt,wgt*el)            !/*cnxc       14*/
         call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,-wgt)             !/*cnxc       15*/
         wgt=sw                                                                 !/*cnxc       19*/
      endif                                                                     !/*cnxc       20*/
!                                                                               !/*cn        127*/
!        do russian roulette on weight cutoff as required.                      !/*cn        128*/
      if(wwp(1,4).ne.0.)go to 160                                               !/*cn4c       17*/
      if(for(lfor+1,icl).ne.0.)go to 160                                        !/*cn        130*/
      if(idx.eq.0)go to 140                                                     !/*cn        131*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*dxw(1,2))go to 160                     !/*cnx        30*/
      t1=dxw(1,1)*fiml9(1,1)/(gamef*fiml(1))                                    !/*cnx        31*/
      go to 150                                                                 !/*cn        134*/
  140 if(wgt*gamef*fiml(1).gt.fiml9(1,1)*wcs2tc(1))go to 160                    !/*cnx        32*/
      t1=wcs1tc(1)*fiml9(1,1)/(gamef*fiml(1))                                   !/*cnx        33*/
  150 if(wgt.lt.t1*rang())go to 280                                             !/*cn        137*/
      call gain_pax (igain_all_pls_weight_cutoff, 1,                            !/*cnx        34*/
     &               0, t1-wgt, (t1-wgt)*el)                                    !/*cnx        35*/
      call contrib_pwb (1,ipwb_variance_weight_cutoff,icl,t1-wgt)               !/*cnx        36*/
      wgt=t1                                                                    !/*cn        141*/
!                                                                               !/*cn        142*/
!**************************  make neutrons  ****************************        !/*cn        143*/
!                                                                      *        !/*cn        144*/
  160 if(iet.ne.0)go to 250                                                     !/*cn        145*/
!                                                                               !/*cn4b       18*/
!        sample the velocity of the target nucleus. (x-6:jsh:rep-92-163)        !/*cn4b       19*/
      ssr=0.                                                                    !/*cn4b       20*/
      rtc(krtc+2,iex)=rtc(krtc+1,iex)                                           !/*cn4b       21*/
      ktc(kktc+2,iex)=ktc(kktc+1,iex)                                           !/*cn4b       22*/
      if(awn(lawn+iex).le.1.5.or.erg.le.400.*ttn)                               !/*cn4b       23*/
     & call tgtvel(erg,rtc(krtc+2,iex),ar,vr,ktc(kktc+2,iex))                   !/*cn4b       24*/
!                                                                               !/*cn4b       25*/
!        sample elastic or inelastic scattering.                                !/*cn4b       26*/
      ipsc=5                                                                    !/*cn4a       48*/
      call acecol(0)                                                            !/*cn        146*/
      if(kdb.ne.0.or.nter.ne.0)return                                           !/*cn4a       49*/
!                                                                               !/*cn        148*/
!        bank all but one of the neutrons from the collision.                   !/*cn        149*/
      ni=cmult                                                                  !/*cn        150*/
      if(ssr.ne.0.)go to 170                                                    !/*cn4a       50*/
      if(ni.ne.1)go to 180                                                      !/*cn        154*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*cn4b       28*/
      if(rcoilf(1).ne.zero.and.nxs(lnxs+2,iex).le.2004.and.ntyn.eq.-99)         !/*cnxk        1*/
     & call low_recoil(uold)                                                    !/*cnxk        2*/
      erg=colout(1,1)                                                           !/*cn        156*/
      go to 260                                                                 !/*cn        157*/
  170 ipsc=4                                                                    !/*cn        158*/
      s=1./ssr                                                                  !/*cn        159*/
      vr(1)=s*vr(1)                                                             !/*cn        160*/
      vr(2)=s*vr(2)                                                             !/*cn        161*/
      vr(3)=s*vr(3)                                                             !/*cn        162*/
      if(ni.eq.1)go to 190                                                      !/*cn        163*/
  180 if (ntyn.ne.19) then                                                      !/*cnx        37*/
         call lose_pax (loss_neutron_loss_to_n_xn,1,1,wgt,wgt*eg0)              !/*cnx        38*/
         call contrib_pwb (1,ipwb_neutron_loss_to_n_xn,icl,-wgt)                !/*cnx        39*/
         pan(kpan+1,5,mpan)=pan(kpan+1,5,mpan)+wgt*(ni-1)                       !/*cnx        40*/
      else                                                                      !/*cnx        41*/
         call lose_pax (loss_neutron_loss_to_fission,1,1,wgt,wgt*eg0)           !/*cnx        42*/
         call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,-wgt)             !/*cnx        43*/
         pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)+wgt*(ni-1)                       !/*cnx        44*/
      endif                                                                     !/*cnx        45*/
      smultc(1)=smultc(1)+wgt*(ni-1)                                            !/*cn        170*/
      sw=wgt                                                                    !/*cn        171*/
      st=tme                                                                    !/*cn4c       19*/
  190 do 240 k=1,ni                                                             !/*cn        172*/
!                                                                               !/*cn4c       20*/
!        prepare to bank non-thermal neutrons.                                  !/*cn4c       21*/
      if(ssr.ne.0.)go to 200                                                    !/*cn4a       52*/
      call rotas(colout(2,k),uold,uuu,lev,irt)                                  !/*cn4b       30*/
      erg=colout(1,k)                                                           !/*cn        175*/
      go to 210                                                                 !/*cn        176*/
!                                                                               !/*cn4c       22*/
!        prepare to bank thermal free gas collision neutrons.                   !/*cn4c       23*/
  200 call rotas(colout(2,k),vr,uuu,0,irt)                                      !/*cn4b       31*/
      if(rcoilf(1).ne.zero.and.nxs(lnxs+2,iex).le.2004.and.ntyn.eq.-99)         !/*cnxk        3*/
     & call low_recoil(vr)                                                      !/*cnxk        4*/
      s=sqrt(colout(1,k)*ar)                                                    !/*cn        178*/
      uuu=s*uuu+vtr(1)                                                          !/*cn        179*/
      vvv=s*vvv+vtr(2)                                                          !/*cn        180*/
      www=s*www+vtr(3)                                                          !/*cn        181*/
      t=uuu**2+vvv**2+www**2                                                    !/*cn        182*/
      s=1./sqrt(t)                                                              !/*cn        183*/
      uuu=uuu*s                                                                 !/*cn        184*/
      vvv=vvv*s                                                                 !/*cn        185*/
      www=www*s                                                                 !/*cn        186*/
      if(lev.ne.0)call uplev(uuu)                                               !/*cn4b       32*/
      erg=t/ar                                                                  !/*cn        188*/
!                                                                               !/*cn4c       24*/
!        prepare to bank (n,xn), prompt fission, delayed fission.               !/*cnx        46*/
  210 if(ni.eq.1)go to 260                                                      !/*cnx        47*/
      tme=st                                                                    !/*cnx        48*/
      if (ntyn.ne.19) then                                                      !/*cnx        49*/
         nx=igain_neutron_n_xn                                                  !/*cnx        50*/
         nb=ipwb_neutron_n_xn                                                   !/*cnx        51*/
      else                                                                      !/*cnx        52*/
         nx=igain_neutron_prompt_fission                                        !/*cnx        53*/
         nb=ipwb_neutron_fission                                                !/*cnx        54*/
!           delayed fission if colout(3,k) = time delay > 0.                    !/*cnx        55*/
         if (colout(3,k).gt.0.) then                                            !/*cnx        56*/
            nx=igain_neutron_delayed_fission                                    !/*cnx        57*/
            tme=st+colout(3,k)                                                  !/*cnx        58*/
!              biased delayed fission if colout(3,1) = -bias weight.            !/*cnx        59*/
            if (colout(3,1).lt.0.) then                                         !/*cnx        60*/
               wgt=-colout(3,1)                                                 !/*cnx        61*/
               pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)+wgt-sw                     !/*cnx        62*/
               smultc(1)=smultc(1)+wgt-sw                                       !/*cnx        63*/
            endif                                                               !/*cnx        64*/
            if (tme.ge.tco(1)) then                                             !/*cnxk        5*/
               call gain_pax (nx,1,1,wgt,wgt*erg)                               !/*cnxk        6*/
               call contrib_pwb (1,nb,icl,wgt)                                  !/*cnxk        7*/
               nter = loss_all_pls_time_cutoff                                  !/*cnxk        8*/
               if(k.lt.ni)go to 230                                             !/*cnxk        9*/
               go to 270                                                        !/*cnxk       10*/
            endif                                                               !/*cnxk       11*/
         endif                                                                  !/*cnx        65*/
      endif                                                                     !/*cnx        66*/
      call gain_pax (nx,1,1,wgt,wgt*erg)                                        !/*cnx        67*/
      call contrib_pwb (1,nb,icl,wgt)                                           !/*cnx        68*/
      if(k.eq.ni)go to 270                                                      !/*cn        194*/
      if(erg.lt.elc(1))go to 220                                                !/*cn        195*/
      vel=slite*sqrt(erg*(erg+2.*gpt(1)))/(erg+gpt(1))                          !/*cn        196*/
      if(wwp(1,4).ne.0.)call wtwndo(4)                                          !/*cn4c       42*/
      if(nter.ne.0)go to 230                                                    !/*cn        198*/
      if(espl(1,1).ne.0.)call ergimp                                            !/*cn        199*/
      if(nter.ne.0)go to 230                                                    !/*cn        200*/
      npa=1                                                                     !/*cn        201*/
#ifdef SPABI                                                                    /*!cnx        69*/
      if (is_spabi.gt.0) then                                                   !/*cnxa        1*/
         call sec_par_bias(1,1,erg,nx,nb,0,7)                                   !/*cnx        71*/
         go to 240                                                              !/*cnx        72*/
      endif                                                                     !/*cnx        73*/
#endif                                                                          /*!cnx        74*/
      call bankit(7)                                                            !/*cn4a       53*/
      go to 240                                                                 !/*cn        203*/
  220 nter = loss_all_pls_energy_cutoff                                         !/*cnx        75*/
  230 call lose_pax (nter, 1, 1, wgt, wgt*erg)                                  !/*cnx        76*/
      call contrib_pwb (1,jrwb(nter,1),icl,-wgt)                                !/*cnx        77*/
      nter = 0                                                                  !/*cnx        78*/
  240 wgt=sw                                                                    !/*cn        210*/
      return                                                                    !/*cn4c       43*/
!                                                                               !/*cn        211*/
!        s(a,b) case                                                            !/*cn        212*/
  250 call sabcol                                                               !/*cn        213*/
      if(kdb.ne.0)return                                                        !/*cn        214*/
!                                                                               !/*cn        215*/
!        set up the continuing neutron.                                         !/*cn        216*/
  260 if(ntyn.eq.19)then                                                        !/*cnxk       12*/
         call lose_pax (loss_neutron_loss_to_fission,1,1,wgt,wgt*el)            !/*cnxc       22*/
         call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,-wgt)             !/*cnxc       23*/
         call gain_pax (igain_neutron_prompt_fission,1,1,wgt,wgt*erg)           !/*cnxc       24*/
         call contrib_pwb (1,ipwb_neutron_fission,icl,wgt)                      !/*cnxc       25*/
      else                                                                      !/*cnxc       26*/
         if(erg.lt.el)call lose_pax                                             !/*cnxc       27*/
     &    (loss_neutron_downscattering,1,0,zero,wgt*(el-erg))                   !/*cnxc       28*/
         if(erg.gt.el)call gain_pax                                             !/*cnxc       29*/
     &    (igain_neutron_upscattering,1,0,zero,wgt*(erg-el))                    !/*cnxc       30*/
      endif                                                                     !/*cnx        90*/
  270 if (erg.lt.elc(1).and.ntyn.ge.0.and.ssr.eq.0.)                            !/*cnx        91*/
     &    nter = loss_all_pls_energy_cutoff                                     !/*cnx        92*/
      vel=slite*sqrt(erg*(erg+2.*gpt(1)))/(erg+gpt(1))                          !/*cn        220*/
      if(krflg.ne.0)call eventp(4)                                              !/*cn        221*/
      if(iptr.ne.0)call ptrak(4)                                                !/*cn4a       55*/
      return                                                                    !/*cn        222*/
!                                                                               !/*cn        223*/
  280 nter = loss_all_pls_weight_cutoff                                         !/*cnx        93*/
      if(krflg.ne.0)call eventp(4)                                              !/*cn        225*/
      if(iptr.ne.0)call ptrak(4)                                                !/*cn4a       56*/
      return                                                                    !/*cn        226*/
!                                                                               !/*cn        227*/
!        multigroup cases.                                                      !/*cn        228*/
  290 call mgacol                                                               !/*cn        229*/
      return                                                                    !/*cn        230*/
  300 if(nsr.eq.71)call colidk                                                  !/*cn        231*/
      if(kdb.ne.0)return                                                        !/*cn        232*/
      call mgcoln                                                               !/*cn4a       57*/
      return                                                                    !/*cn        234*/
      end                                                                       !/*cn        235*/

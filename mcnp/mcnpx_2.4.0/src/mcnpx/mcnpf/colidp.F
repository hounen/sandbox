!#ident colidp.F from deck cp                                                   !/*cp          1*/
      subroutine colidp                                                         !/*cp          2*/
!        calculate a collision of a photon with an atom.                        !/*cp          3*/
      use global_data                                                           !/*cpx         1*/
!                                                                               !/*cpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*cpx         3*/
      implicit integer(kindi) (i-n)                                             !/*cpx         4*/
!                                                                               !/*cpx         5*/
!                                                                               !/*cp          5*/
!        save the incoming direction.                                           !/*cp          6*/
      uold(1)=uuu                                                               !/*cp4b        1*/
      uold(2)=vvv                                                               !/*cp4b        2*/
      uold(3)=www                                                               !/*cp4b        3*/
!                                                                               !/*cp4c2       1*/
!        photonuclear events.                                                   !/*cp4c2       2*/
      if(totpn.gt.0.)call collpn                                                !/*cp4c2       3*/
      if(nter.ne.0.or.kdb.ne.0)return                                           !/*cp4c2       4*/
!                                                                               !/*cp4c2       5*/
!        multigroup and photoatomic events.                                     !/*cp4c2       6*/
      ntyn=0                                                                    !/*cp         10*/
      jsu=0                                                                     !/*cp         11*/
      if(mcal.eq.2)go to 380                                                    !/*cp         12*/
!                                                                               !/*cp         13*/
!        sample cumulative cross section to find the collision nuclide.         !/*cp         14*/
      mk=mat(lmat+icl)                                                          !/*cp         15*/
      m=jmd(ljmd+mk)                                                            !/*cp         16*/
      m1=m                                                                      !/*cp         17*/
      if(npq(lnpq+mk).eq.1)go to 20                                             !/*cp         18*/
      t1=rang()*totm                                                            !/*cp         19*/
      c=0.                                                                      !/*cp         20*/
      do 10 m=m1,jmd(ljmd+mk+1)-2                                               !/*cp         21*/
      c=c+rtc(krtc+4,lme(llme+2,m))*fme(lfme+m)                                 !/*cp         22*/
   10 if(t1.lt.c)go to 20                                                       !/*cp         23*/
   20 iex=lme(llme+2,m)                                                         !/*cp         24*/
      iexp=m                                                                    !/*cp4b        4*/
      mtp=-2                                                                    !/*cp4c        1*/
      mpan=ipan(lipa+icl)+m-m1                                                  !/*cp4a        1*/
      if(mcal.eq.1)go to 370                                                    !/*cp         26*/
      do 22 i=1,4                                                               !/*cp4b        6*/
   22 tpd(i)=rtc(krtc+i,iex)                                                    !/*cp4b        7*/
      pan(kpan+2,1,mpan)=pan(kpan+2,1,mpan)+1.                                  !/*cp4a        2*/
      pan(kpan+2,2,mpan)=pan(kpan+2,2,mpan)+wgt                                 !/*cp4a        3*/
!                                                                               !/*cp         30*/
!        if incident energy greater than emcf, use simple physics,              !/*cp4b        8*/
!        calling emaker for electrons and bremsstrahlung for implicit           !/*cp4b        9*/
!        capture and skipping coherent scatter and fluorescence.                !/*cp4b       10*/
      if(erg.le.emcf(2))go to 80                                                !/*cp         33*/
      t2=tpd(3)-tpd(2)                                                          !/*cp4b       11*/
      t1=wgt*t2/tpd(4)                                                          !/*cp4b       12*/
      sw=wgt                                                                    !/*cp         36*/
      wgt=t1                                                                    !/*cp         37*/
      e=erg                                                                     !/*cp         38*/
      if(ides.eq.0.and.e.gt.elc(3))call emaker(1,e)                             !/*cp         39*/
      wgt=sw                                                                    !/*cp         40*/
!                                                                               !/*cp         41*/
!        sample analog capture if required.                                     !/*cp         42*/
      if(wc1(2).ne.0.)go to 30                                                  !/*cp         43*/
      if(t2.lt.rang()*tpd(4))go to 70                                           !/*cp4b       15*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+wgt                                 !/*cp4a        4*/
      nter = loss_photon_capture                                                !/*cpx         6*/
      go to 360                                                                 !/*cp         48*/
!                                                                               !/*cp         49*/
!        otherwise simulate capture by weight reduction.                        !/*cp         50*/
   30 wgt=wgt-t1                                                                !/*cp         51*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+t1                                  !/*cp4a        5*/
      tmavtc(2,2)=tmavtc(2,2)+tme*t1                                            !/*cp         54*/
      tmavtc(2,3)=tmavtc(2,3)+tme*t1                                            !/*cp         55*/
      call lose_pax (loss_photon_capture, 2, 0, t1, t1*erg)                     !/*cpx         7*/
      call contrib_pwb (2,ipwb_photon_capture,icl,-t1)                          !/*cpx         8*/
!                                                                               !/*cp         59*/
!        terminate or continue the photon according to weight cutoffs.          !/*cp         60*/
      if(for(lfor+2,icl).ne.0.)go to 70                                         !/*cp         61*/
      if(wwp(2,4).ne.0.)go to 70                                                !/*cp4c        2*/
      if(idx.eq.0)go to 40                                                      !/*cp         63*/
      if(wgt*fiml(1).gt.fiml9(1,1)*dxw(2,2))go to 70                            !/*cp         64*/
      t1=fiml9(1,1)*dxw(2,1)/fiml(1)                                            !/*cp         65*/
      go to 50                                                                  !/*cp         66*/
   40 if(wgt*fiml(1).gt.fiml9(1,1)*wcs2tc(2))go to 70                           !/*cp         67*/
      t1=fiml9(1,1)*wcs1tc(2)/fiml(1)                                           !/*cp         68*/
   50 if(wgt.ge.t1*rang())go to 60                                              !/*cp         69*/
      nter = loss_all_pls_weight_cutoff                                         !/*cpx         9*/
      go to 360                                                                 !/*cp         72*/
   60 call gain_pax (igain_all_pls_weight_cutoff, 2,                            !/*cpx        10*/
     &               0, t1-wgt, (t1-wgt)*erg)                                   !/*cpx        11*/
      call contrib_pwb (2,ipwb_variance_weight_cutoff,icl,t1-wgt)               !/*cpx        12*/
      wgt=t1                                                                    !/*cp         76*/
!                                                                               !/*cp         77*/
!        sample the type of collision.                                          !/*cp         78*/
   70 t2=(tpd(4)-t2)*rang()                                                     !/*cp4b       18*/
      if(t2.ge.tpd(2))go to 330                                                 !/*cp4b       19*/
      s=erg/gpt(3)                                                              !/*cp         81*/
      go to 90                                                                  !/*cp         82*/
!                                                                               !/*cp         83*/
!        detailed treatment as in mcp.                                          !/*cp         84*/
   80 t2=tpd(4)*rang()                                                          !/*cp4b       20*/
      if(t2.ge.tpd(2))go to 250                                                 !/*cp4b       21*/
      s=erg/gpt(3)                                                              !/*cp         87*/
      if(t2.ge.tpd(1))go to 180                                                 !/*cp4b       22*/
!                                                                               !/*cp         89*/
!**********************  incoherent scattering  ************************        !/*cp         90*/
!                                                                      *        !/*cp         91*/
!        sample scattering angle, cs, from the klein-nishina formula.           !/*cp         92*/
   90 it=0                                                                      !/*cp         93*/
  100 call klein(s,t4,cs)                                                       !/*cp         94*/
!                                                                               !/*cp         95*/
!        if energy .gt. emcf treat as mcg and bypass form factors.              !/*cp         96*/
      if(erg.gt.emcf(2))go to 170                                               !/*cp         97*/
!                                                                               !/*cp         98*/
!        calculate form factors for rejection.                                  !/*cp         99*/
      t3=s*sqrt(1.-cs)                                                          !/*cp        100*/
      if(nxs(lnxs+2,iex).gt.1)go to 110                                         !/*cp        101*/
      t3=96.9014*t3                                                             !/*cp        102*/
      t6=1.-1./(1.+t3**2)**4                                                    !/*cp        103*/
      if(it.eq.0)t8=1.-1./(1.+(fscon*s)**2)**4                                  !/*cp4b       23*/
      go to 160                                                                 !/*cp        105*/
  110 t3=29.1445*t3                                                             !/*cp        106*/
      if(t3.ge.vic(minc))go to 170                                              !/*cp4a        6*/
      do 120 i1=2,minc                                                          !/*cp4a        7*/
  120 if(t3.lt.vic(i1))go to 130                                                !/*cp        109*/
  130 ib=jxs(ljxs+2,iex)+i1-1                                                   !/*cp        110*/
      t6=xss(ib)+(xss(ib-1)-xss(ib))*(t3-vic(i1))/(vic(i1-1)-vic(i1))           !/*cp        111*/
      if(it.ne.0)go to 160                                                      !/*cp        112*/
      t3=41.2166*s                                                              !/*cp        113*/
      t8=nxs(lnxs+2,iex)                                                        !/*cp        114*/
      if(t3.ge.vic(minc))go to 160                                              !/*cp4a        8*/
      do 140 i=i1,minc                                                          !/*cp4a        9*/
  140 if(t3.lt.vic(i))go to 150                                                 !/*cp        117*/
  150 ib=jxs(ljxs+2,iex)+i-1                                                    !/*cp        118*/
      t8=xss(ib)+(xss(ib-1)-xss(ib))*(t3-vic(i))/(vic(i-1)-vic(i))              !/*cp        119*/
  160 t3=t8*rang()                                                              !/*cp        120*/
      it=1                                                                      !/*cp        121*/
      if(t3.ge.t6)go to 100                                                     !/*cp        122*/
  170 call lose_pax (loss_photon_compton_scatter, 2,                            !/*cpx        13*/
     &               0, zero, wgt*(erg-gpt(3)*t4))                              !/*cpx        14*/
      erg=gpt(3)*t4                                                             !/*cp        124*/
      cs=min(max(-one,cs),one)                                                  !/*cp        125*/
      call rotas(cs,uold,uuu,lev,irt)                                           !/*cp4b       25*/
      if(ides.eq.0.and.eg0-erg.gt.elc(3))call emaker(2,cs)                      !/*cp        127*/
      ntyn=1                                                                    !/*cp        128*/
      mtp=-1                                                                    !/*cp4c        3*/
      ipsc=7                                                                    !/*cp        129*/
      go to 360                                                                 !/*cp        131*/
!                                                                               !/*cp        132*/
!***********************  coherent scattering  *************************        !/*cp        133*/
!                                                                      *        !/*cp        134*/
  180 t5=1698.8038*s**2                                                         !/*cp        135*/
!                                                                               !/*cp        136*/
!        sample scattering angle.                                               !/*cp        137*/
      if(t5.lt.wco(mcoh))go to 190                                              !/*cp4a       10*/
      t7=xss(jxs(ljxs+3,iex)+mcoh-1)                                            !/*cp4a       11*/
      go to 220                                                                 !/*cp        140*/
  190 do 200 i=2,mcoh                                                           !/*cp4a       12*/
  200 if(t5.lt.wco(i))go to 210                                                 !/*cp        142*/
  210 t3=(t5-wco(i))/(wco(i-1)-wco(i))                                          !/*cp        143*/
      ib=jxs(ljxs+3,iex)+i-1                                                    !/*cp        144*/
      t7=xss(ib)+t3*(xss(ib-1)-xss(ib))                                         !/*cp        145*/
  220 t3=t7*rang()                                                              !/*cp        146*/
      ib=jxs(ljxs+3,iex)-1                                                      !/*cp        147*/
      do 230 i=2,mcoh                                                           !/*cp4a       13*/
  230 if(t3.lt.xss(ib+i))go to 240                                              !/*cp        149*/
  240 t3=(t3-xss(ib+i))/(xss(ib+i-1)-xss(ib+i))                                 !/*cp        150*/
      cs=1.-2.*(wco(i)+t3*(wco(i-1)-wco(i)))/t5                                 !/*cp        151*/
      t3=1.+cs**2                                                               !/*cp        152*/
      if(t3.le.2.*rang())go to 220                                              !/*cp        153*/
      ipsc=6                                                                    !/*cp        154*/
      call rotas(cs,uold,uuu,lev,irt)                                           !/*cp4b       26*/
      ntyn=2                                                                    !/*cp        156*/
      go to 360                                                                 !/*cp        158*/
!                                                                               !/*cp        159*/
!********************  capture with fluorescence  **********************        !/*cp        160*/
!                                                                      *        !/*cp        161*/
  250 if(t2.ge.tpd(3))go to 330                                                 !/*cp4b       27*/
      er=erg                                                                    !/*cp4c        4*/
      ie=iex                                                                    !/*cp4c        5*/
      ik=ipt                                                                    !/*cp4c        6*/
#ifdef MESHTAL                                                                  /*!cpx        15*/
      qx=0.                                                                     !/*cpx        16*/
      call flaug(er,ie,mk,ik,qx)                                                !/*cpx        17*/
#else                                                                           /*!cpx        18*/
      call flaug(er,ie,mk,ik)                                                   !/*cpx        19*/
#endif                                                                          /*!cpx        20*/
      go to 360                                                                 !/*cp        237*/
!                                                                               !/*cp        238*/
!*************************  pair production  ***************************        !/*cp        239*/
!                                                                      *        !/*cp        240*/
  330 ntyn=5                                                                    !/*cp        241*/
      mtp=-4                                                                    !/*cp4c        8*/
      if(ides.ne.0)go to 340                                                    !/*cp        242*/
      cs=0.                                                                     !/*cp        243*/
      call emaker(3,cs)                                                         !/*cp        244*/
      if(cs.eq.0.)go to 350                                                     !/*cp        245*/
  340 if(gpt(3).lt.elc(2))go to 350                                             !/*cp        246*/
      call lose_pax (loss_photon_pair_production, 2, 1, wgt, wgt*erg)           !/*cpx        21*/
      call contrib_pwb (2,ipwb_photon_pair_production,icl,-wgt)                 !/*cpx        22*/
      erg=gpt(3)                                                                !/*cp        251*/
      ipsc=105                                                                  !/*cp        252*/
      call isos(uuu,lev)                                                        !/*cp4b       39*/
      call gain_pax (igain_photon_p_annihilation, 2,                            !/*cpx        23*/
     &               2, two*wgt, two*wgt*erg)                                   !/*cpx        24*/
      call contrib_pwb (2,ipwb_photon_p_annihilation,icl,two*wgt)               !/*cpx        25*/
      npa=1                                                                     !/*cp        258*/
      call bankit(10)                                                           !/*cp4a       17*/
      uuu=-uuu                                                                  !/*cp        260*/
      vvv=-vvv                                                                  !/*cp        261*/
      www=-www                                                                  !/*cp        262*/
      if(lev.ne.0)call uplev(uuu)                                               !/*cp4b       40*/
      go to 360                                                                 !/*cp        264*/
  350 nter = loss_photon_pair_production                                        !/*cpx        26*/
!                                                                               !/*cp        267*/
!        print a line in the event log.                                         !/*cp        268*/
  360 if(krflg.ne.0)call eventp(4)                                              !/*cp4a       18*/
      if(iptr.ne.0)call ptrak(4)                                                !/*cp4a       19*/
      return                                                                    !/*cp        270*/
!                                                                               !/*cp        271*/
!        multigroup cases.                                                      !/*cp        272*/
  370 call mgcolp                                                               !/*cp4a       20*/
      return                                                                    !/*cp        274*/
  380 call mgacol                                                               !/*cp        275*/
      return                                                                    !/*cp        276*/
      end                                                                       !/*cp        277*/

!#ident cp_coll.F from deck xcc                                                 !/*xcc         1*/
      subroutine cp_coll                                                        !/*xcc         2*/
!     This routine calculates everything about a collision                      !/*xcc         3*/
!     involving an incident charged particle in the                             !/*xcc         4*/
!     tabular energy range                                                      !/*xcc         5*/
!     much of this routine has been modelled after (i.e., stolen from)          !/*xcc         6*/
!     subroutines colidn and acecol of mcnpx.                                   !/*xcc         7*/
      use global_data                                                           !/*xcc         8*/
!                                                                               !/*xcc         9*/
      implicit real(kindr) (a-h,o-z)                                            !/*xcc        10*/
      implicit integer(kindi) (i-n)                                             !/*xcc        11*/
!                                                                               !/*xcc        12*/
!                                                                               !/*xcc        13*/
!        save the incoming direction.                                           !/*xcc        14*/
      uold(1)=uuu                                                               !/*xcc        15*/
      uold(2)=vvv                                                               !/*xcc        16*/
      uold(3)=www                                                               !/*xcc        17*/
      jsu=0                                                                     !/*xcc        18*/
      ntyn=0                                                                    !/*xcc        19*/
!                                                                               !/*xcc        20*/
!        sample the nuclide, using the cumulative total cross section.          !/*xcc        21*/
      mk=mat(lmat+icl)                                                          !/*xcc        22*/
      m=jmd(ljmd+mk)                                                            !/*xcc        23*/
      m1=m                                                                      !/*xcc        24*/
      if(npq(lnpq+mk).eq.1)go to 20                                             !/*xcc        25*/
      c=rang()*totm                                                             !/*xcc        26*/
      do 10 m=m1,jmd(ljmd+mk+1)-2                                               !/*xcc        27*/
      c=c-rtc(krtc+5,lme(llme+ipt,m))*fme(lfme+m)                               !/*xcc        28*/
   10 if(c.lt.0.)go to 20                                                       !/*xcc        29*/
   20 iex=lme(llme+ipt,m)                                                       !/*xcc        30*/
!                                                                               !/*xcc        31*/
!  temporary write statement                                                    !/*xcc        32*/
!                                                                               !/*xcc        33*/
!     write(*,30) erg,iex                                                       !/*xcc        34*/
!  30 format("  at erg=",1pe12.5," sampled collision with iex=",i2)             !/*xcc        35*/
      iexp=m                                                                    !/*xcc        36*/
      mtp=2                                                                     !/*xcc        37*/
      mpan=ipan(lipa+icl)+m-m1                                                  !/*xcc        38*/
!                                                                               !/*xcc        39*/
!  generate photons if appropriate                                              !/*xcc        40*/
      if(kpt(2).ne.0.and.jxs(ljxs+12,iex).ne.0.and.tme.lt.tco(2))               !/*xcc        41*/
     & call cp_gam                                                              !/*xcc        42*/
!                                                                               !/*xcc        43*/
!  generate other secondary particles if appropriate                            !/*xcc        44*/
      if(nxs(lnxs+7,iex).ne.0) call acecp                                       !/*xcc        45*/
!                                                                               !/*xcc        46*/
!        for now, only allow implicit capture by weight reduction               !/*xcc        47*/
      if(rtc(krtc+3,iex).gt.0.) then                                            !/*xcc        48*/
        t1=wgt*rtc(krtc+3,iex)/rtc(krtc+5,iex)                                  !/*xcc        49*/
        wgt=wgt-t1                                                              !/*xcc        50*/
!                                                                               !/*xcc        51*/
        call lose_pax (loss_gen_chg_capture, ipt, 0, t1, t1*eg0)                !/*xcc        52*/
        call contrib_pwb (ipt,ipwb_gen_chg_capture,icl,-t1)                     !/*xcc        53*/
      endif                                                                     !/*xcc        54*/
!                                                                               !/*xcc        55*/
!        QUESTION: Play weight cutoff game here?                                !/*xcc        56*/
!                                                                               !/*xcc        57*/
!        decide on elastic vs nonelastic reaction                               !/*xcc        58*/
      if(nxs(lnxs+5,iex).eq.0) goto 60                                          !/*xcc        59*/
      ic=ktc(kktc+1,iex)+jxs(ljxs+1,iex)-1                                      !/*xcc        60*/
!                                                                               !/*xcc        61*/
!        el is the elastic cross section                                        !/*xcc        62*/
      n=nxs(lnxs+3,iex)                                                         !/*xcc        63*/
      el=xss(ic+3*n)+rtc(krtc+1,iex)*(xss(ic+3*n+1)-xss(ic+3*n))                !/*xcc        64*/
      r=rang()*rtc(krtc+5,iex)-el                                               !/*xcc        65*/
      if(r.ge.0.) goto 40                                                       !/*xcc        66*/
!                                                                               !/*xcc        67*/
!        elastic scattering sampled                                             !/*xcc        68*/
   60 ixre=0                                                                    !/*xcc        69*/
      mtp=2                                                                     !/*xcc        70*/
      ntyn=-99                                                                  !/*xcc        71*/
      cmult=1.                                                                  !/*xcc        72*/
!        sample the center-of-mass scattering cosine (c)                        !/*xcc        73*/
      c=acecos(jxs(ljxs+9,iex),nint(xss(jxs(ljxs+8,iex))))                      !/*xcc        74*/
!                                                                               !/*xcc        75*/
!        have not yet derived the kinematic formulae.                           !/*xcc        76*/
!        for initial implementation, simply use the neutron elastic             !/*xcc        77*/
!        scattering formulae, with awr being the ratio to the ipt mass.         !/*xcc        78*/
      yy=awn(lawn+iex)*gpt(1)/gpt(ipt)                                          !/*xcc        79*/
      t1=1.+yy*(yy+2.*c)                                                        !/*xcc        80*/
      colout(1,1)=erg*t1/(1.+yy)**2                                             !/*xcc        81*/
      colout(2,1)=(1.+yy*c)/sqrt(t1)                                            !/*xcc        82*/
      colout(3,1)=c                                                             !/*xcck        1*/
      ssr=zero                                                                  !/*xcck        2*/
!                                                                               !/*xcc        86*/
!        update parameters for continuing particle                              !/*xcc        87*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*xcc        88*/
      if(rcoilf(2).ne.zero.and.nxs(lnxs+2,iex).le.2004.and.ntyn.eq.-99)         !/*xcck        3*/
     & call low_recoil(uold)                                                    !/*xcck        4*/
      erg=colout(1,1)                                                           !/*xcc        89*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                      !/*xcc        90*/
      if(krflg.ne.0)call eventp(4)                                              !/*xcc        91*/
      if(iptr.ne.0)call ptrak(4)                                                !/*xcc        92*/
      return                                                                    !/*xcc        93*/
!                                                                               !/*xcc        94*/
!        inelastic scattering sampled                                           !/*xcc        95*/
   40 continue                                                                  !/*xcc        96*/
      tt=0.                                                                     !/*xcc        97*/
      kx=0                                                                      !/*xcc        98*/
      if(nxs(lnxs+5,iex).eq.1) then                                             !/*xcc        99*/
         ixre=1                                                                 !/*xcc       100*/
         go to 150                                                              !/*xcc       101*/
      endif                                                                     !/*xcc       102*/
!                                                                               !/*xcc       103*/
!        sample the inelastic reaction.                                         !/*xcc       104*/
      do 120 ixre=1,nxs(lnxs+5,iex)                                             !/*xcc       105*/
!                                                                               !/*xcc       106*/
!        calculate the cross section for reaction whose index is ixre.          !/*xcc       107*/
!        data:  ie,ne,(xs(i),i=1,ne)   xs(1) corresponds to es(ie).             !/*xcc       108*/
      is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ixre-1))                      !/*xcc       109*/
      ic=ktc(kktc+2,iex)+1-nint(xss(is-1))                                      !/*xcc       110*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)).or.ic.eq.nint(xss(is)).and.             !/*xcc       111*/
     & rtc(krtc+2,iex).ne.0.)go to 120                                          !/*xcc       112*/
      t=xss(ic+is)                                                              !/*xcc       113*/
      if(rtc(krtc+2,iex).ne.0.)t=t+rtc(krtc+2,iex)*                             !/*xcc       114*/
     & (xss(ic+is+1)-xss(ic+is))                                                !/*xcc       115*/
      if(t.eq.0.)go to 120                                                      !/*xcc       116*/
      if(t.gt.tt)kx=ixre                                                        !/*xcc       117*/
      tt=max(t,tt)                                                              !/*xcc       118*/
      r=r-t                                                                     !/*xcc       119*/
      if(r.lt.0.)go to 150                                                      !/*xcc       120*/
  120 continue                                                                  !/*xcc       121*/
!                                                                               !/*xcc       122*/
!        handle failure of the reaction cross sections to add up.               !/*xcc       123*/
      if(kx.eq.0)go to 60                                                       !/*xcc       124*/
!     call zaid(2,ht,ixl(lixl+1,iex))                                           !/*xcc       125*/
!     call errprn(1,npnm,4,erg,zero,'erg',' ',                                  !/*xcc       126*/
!    & 'no reaction mt found. collision resampled. zaid = '//ht)                !/*xcc       127*/
      ixre=kx                                                                   !/*xcc       128*/
!                                                                               !/*xcc       129*/
!        get the reaction type number.                                          !/*xcc       130*/
  150 ntyn=nint(xss(jxs(ljxs+5,iex)+ixre-1))                                    !/*xcc       131*/
      mtp=nint(xss(jxs(ljxs+3,iex)+ixre-1))                                     !/*xcc       132*/
      q=xss(jxs(ljxs+4,iex)+ixre-1)                                             !/*xcc       133*/
      ia=jxs(ljxs+9,iex)                                                        !/*xcc       134*/
      ka=nint(xss(jxs(ljxs+8,iex)+ixre))                                        !/*xcc       135*/
      id=jxs(ljxs+11,iex)                                                       !/*xcc       136*/
      kd=nint(xss(jxs(ljxs+10,iex)+ixre-1))                                     !/*xcc       137*/
      if(abs(ntyn).ge.100)go to 190                                             !/*xcc       138*/
      cmult=abs(ntyn)                                                           !/*xcc       139*/
      ns=cmult                                                                  !/*xcc       140*/
      do 170 i=1,ns                                                             !/*xcc       141*/
      call acecas(i,1,q,ia,ka,id,kd)                                            !/*xcc       142*/
  170 if(kdb.ne.0)return                                                        !/*xcc       143*/
      nter = loss_gen_chg_tabular_sampling                                      !/*xcc       144*/
      return                                                                    !/*xcc       145*/
!                                                                               !/*xcc       146*/
!        high energy reaction other than fission with energy-dependent          !/*xcc       147*/
!        multiplicity.  kalbach-87 (law 44) endf/b-vi.                          !/*xcc       148*/
!                                                                               !/*xcc       149*/
!        the location of the multiplicity table relative to dlw is              !/*xcc       150*/
!        abs(ntyn)-100.  acefcn gets non-fission reaction multiplicity.         !/*xcc       151*/
  190 l=jxs(ljxs+11,iex)+abs(ntyn)-101                                          !/*xcc       152*/
      cmult=acefcn(l,erg,ln)                                                    !/*xcc       153*/
      cmult=aint(cmult+rang())                                                  !/*xcc       154*/
      if(cmult.eq.0.) then                                                      !/*xcc       155*/
         nter = loss_gen_chg_capture                                            !/*xcc       156*/
      else                                                                      !/*xcc       157*/
         do 200 i=1,int(cmult)                                                  !/*xcc       158*/
            call acecas(i,1,q,ia,ka,id,kd)                                      !/*xcc       159*/
  200       if(kdb.ne.0)return                                                  !/*xcc       160*/
!                                                                               !/*xcc       161*/
!        Rotate, account for, and bank the secondary particles.                 !/*xcc       162*/
!        For now, bank *all* of the particles, as lahet does for                !/*xcc       163*/
!        charged-particle non-elastic reaction products.                        !/*xcc       164*/
         do 240 k=1,int(cmult)                                                  !/*xcc       165*/
            call rotas(colout(2,k),uold,uuu,lev,irt)                            !/*xcc       166*/
            erg=colout(1,k)                                                     !/*xcc       167*/
!                                                                               !/*xcc       168*/
!           Account for creation in tabular sampling entry.                     !/*xcc       169*/
            call gain_pax (igain_gen_chg_tabular_sampling, ipt,                 !/*xcc       170*/
     &                     1, wgt, wgt*erg)                                     !/*xcc       171*/
            call contrib_pwb (ipt, ipwb_gen_chg_tabular_sampl, icl, wgt)        !/*xcc       172*/
            vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                !/*xcc       173*/
            npa=1                                                               !/*xcc       174*/
            call bankit(7)                                                      !/*xcc       175*/
  240    continue                                                               !/*xcc       176*/
         nter = loss_gen_chg_tabular_sampling                                   !/*xcc       177*/
      endif                                                                     !/*xcc       178*/
!                                                                               !/*xcc       179*/
      return                                                                    !/*xcc       180*/
      end                                                                       !/*xcc       181*/

!#ident acecol.F from deck ac                                                   !/*ac          1*/
      subroutine acecol(jq)                                                     !/*ac          2*/
!        sample an elastic or inelastic neutron collision.                      !/*ac          3*/
!        jq=0 normally.                                                         !/*ac          4*/
!        jq=2 for kcode sampling of fission energy only.                        !/*ac          5*/
      use global_data                                                           !/*acx         1*/
!                                                                               !/*acx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*acx         3*/
      implicit integer(kindi) (i-n)                                             !/*acx         4*/
!                                                                               !/*acx         5*/
      character ht*10                                                           !/*ac          7*/
!                                                                               !/*ac          8*/
      nt=0                                                                      !/*ac          9*/
      colout(3,1)=0.                                                            !/*ac4c        1*/
   10 if(jq.eq.2)go to 80                                                       !/*ac         10*/
!                                                                               !/*ac         11*/
!        sample elastic vs. inelastic collision.                                !/*ac         12*/
      if(nxs(lnxs+5,iex).eq.0)go to 60                                          !/*ac         13*/
!                                                                               !/*ac4c        2*/
!        probability tables for unresolved resonances.                          !/*ac4c        3*/
      if(rtc(krtc+11,iex).lt.0.)go to 15                                        !/*ac4c        4*/
      if(jxs(ljxs+23,iex).eq.0)call expirx(0,'acecol',                          !/*acxk        1*/
     & 'flag conflict regarding unresolved prob tables.')                       !/*ac4c        6*/
      if(erg.ne.eg0)call expirx(0,'acecol',                                     !/*acxk        2*/
     & 'erg/eg0 conflict for unresolved resonances')                            !/*ac4c        8*/
      el=rtc(krtc+11,iex)                                                       !/*ac4c        9*/
      sr=rtc(krtc+4,iex)-rtc(krtc+3,iex)-rtc(krtc+8,iex)-el                     !/*ac4c       10*/
      go to 50                                                                  !/*ac4c       11*/
!                                                                               !/*ac4c       12*/
!        no probability tables (usual case).                                    !/*ac4c       13*/
   15 ic=ktc(kktc+2,iex)+jxs(ljxs+1,iex)-1                                      !/*ac4c       14*/
      n=nxs(lnxs+3,iex)                                                         !/*ac         15*/
      el=xss(ic+3*n)+rtc(krtc+2,iex)*(xss(ic+3*n+1)-xss(ic+3*n))                !/*ac         16*/
      if(erg.ne.eg0)go to 20                                                    !/*ac         17*/
      sr=rtc(krtc+4,iex)-rtc(krtc+3,iex)-rtc(krtc+8,iex)-el                     !/*ac         18*/
      go to 30                                                                  !/*ac         19*/
   20 sr=xss(ic+n)-xss(ic+2*n)+rtc(krtc+2,iex)*                                 !/*ac         20*/
     & (xss(ic+n+1)-xss(ic+n)-xss(ic+2*n+1)+xss(ic+2*n))-el                     !/*ac         21*/
      if(lfcl(llfc+icl).eq.0)go to 30                                           !/*ac         22*/
      l=jxs(ljxs+21,iex)                                                        !/*ac         23*/
      if(l.eq.0)go to 30                                                        !/*ac         24*/
      j=l+2+ktc(kktc+2,iex)-nint(xss(l))                                        !/*ac4c       15*/
      sr=sr-xss(j)-rtc(krtc+2,iex)*(xss(j+1)-xss(j))                            !/*ac         26*/
   30 if(awn(lawn+iex)*erg.gt.500.*tbt(ltbt+iex))go to 50                       !/*ac         27*/
      a2=awn(lawn+iex)*erg/tbt(ltbt+iex)                                        !/*ac         28*/
      if(a2.lt.4.)go to 40                                                      !/*ac         29*/
      el=el*a2/(a2+.5)                                                          !/*ac         30*/
      go to 50                                                                  !/*ac         31*/
   40 a=sqrt(a2)                                                                !/*ac         32*/
      b=25.*a                                                                   !/*ac         33*/
      i=b                                                                       !/*ac         34*/
      el=el*a/(thgf(i)+(b-i)*(thgf(i+1)-thgf(i)))                               !/*ac         35*/
   50 r=rang()*(sr+el)-el                                                       !/*ac         36*/
      if(r.ge.0.)go to 100                                                      !/*ac         37*/
!                                                                               !/*ac         38*/
!***************************  elastic case  ****************************        !/*ac         39*/
!                                                                      *        !/*ac         40*/
   60 ixre=0                                                                    !/*ac         41*/
      mtp=2                                                                     !/*ac4b        1*/
      ntyn=-99                                                                  !/*ac         42*/
      cmult=1.                                                                  !/*ac         43*/
!                                                                               !/*ac         44*/
!        sample the neutron output direction and calculate its energy.          !/*ac         45*/
      c=acecos(jxs(ljxs+9,iex),nint(xss(jxs(ljxs+8,iex))))                      !/*ac4c       16*/
      colout(3,1)=c                                                             !/*acxk        3*/
      if(awn(lawn+iex).lt.1.)go to 70                                           !/*ac         48*/
      t1=1.+awn(lawn+iex)*(awn(lawn+iex)+2.*c)                                  !/*ac         49*/
      colout(1,1)=erg*t1/(1.+awn(lawn+iex))**2                                  !/*ac         50*/
      colout(2,1)=(1.+awn(lawn+iex)*c)/sqrt(t1)                                 !/*ac         51*/
      return                                                                    !/*ac         52*/
!        special calculation for hydrogen.                                      !/*ac         53*/
   70 colout(1,1)=.5*erg*(1.+c)                                                 !/*ac         54*/
      colout(2,1)=sqrt(.5+.5*c)                                                 !/*ac         55*/
      return                                                                    !/*ac         56*/
!                                                                               !/*ac         57*/
!*************************  inelastic case.  ***************************        !/*ac         58*/
!                                                                      *        !/*ac         59*/
!        sample the reaction, using cumulative partial cross sections.          !/*ac         60*/
   80 if(erg.ne.eg0)go to 90                                                    !/*ac         61*/
      r=rang()*rtc(krtc+8,iex)                                                  !/*ac         62*/
      go to 100                                                                 !/*ac         63*/
   90 if(rtc(krtc+11,iex).ge.0.)call expirx(0,'acecol',                         !/*acxk        4*/
     & 'fission conflict for unresolved resonances')                            !/*ac4c       18*/
      l=jxs(ljxs+21,iex)                                                        !/*ac4c       19*/
      j=l+2+ktc(kktc+2,iex)-nint(xss(l))                                        !/*ac4c       20*/
      r=rang()*(xss(j)+rtc(krtc+2,iex)*(xss(j+1)-xss(j)))                       !/*ac         66*/
  100 tt=0.                                                                     !/*ac         67*/
      kx=0                                                                      !/*ac         68*/
      do 120 ixre=1,nxs(lnxs+5,iex)                                             !/*ac         69*/
      if(lfcl(llfc+icl).eq.0)go to 110                                          !/*ac         70*/
      if(jq.ne.2.eqv.nint(xss(jxs(ljxs+5,iex)+ixre-1)).eq.19)go to 120          !/*ac4c       21*/
!                                                                               !/*ac         72*/
!        calculate the cross section for reaction whose index is ixre.          !/*ac         73*/
!        data:  ie,ne,(xs(i),i=1,ne)   xs(1) corresponds to es(ie).             !/*ac         74*/
  110 is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ixre-1))                      !/*ac4c       22*/
      ic=ktc(kktc+2,iex)+1-nint(xss(is-1))                                      !/*ac4c       23*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)).or.ic.eq.nint(xss(is)).and.             !/*ac4c       24*/
     & rtc(krtc+2,iex).ne.0.)go to 120                                          !/*ac         78*/
      t=xss(ic+is)                                                              !/*ac         79*/
      if(rtc(krtc+2,iex).ne.0.)t=t+rtc(krtc+2,iex)*                             !/*ac         80*/
     & (xss(ic+is+1)-xss(ic+is))                                                !/*ac         81*/
      if(rtc(krtc+11,iex).lt.0.)go to 115                                       !/*ac4c       25*/
      mn=nint(xss(jxs(ljxs+3,iex)+ixre-1))                                      !/*ac4c       26*/
      if(mn.lt.18.or.mn.gt.19)go to 115                                         !/*ac4c       27*/
      if(jq.eq.2)go to 150                                                      !/*ac4c       28*/
      t=rtc(krtc+12,iex)                                                        !/*ac4c       29*/
  115 if(t.eq.0.)go to 120                                                      !/*ac4c       30*/
      if(t.gt.tt)kx=ixre                                                        !/*ac         83*/
      tt=max(t,tt)                                                              !/*ac         84*/
      r=r-t                                                                     !/*ac         85*/
      if(r.lt.0.)go to 150                                                      !/*ac         86*/
  120 continue                                                                  !/*ac         87*/
!                                                                               !/*ac         88*/
!        handle failure of the reaction cross sections to add up.               !/*ac         89*/
      if(kx.eq.0)go to 60                                                       !/*ac         90*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*ac         91*/
      call errprn(1,npnm,4,erg,zero,'erg',' ',                                  !/*ac4b        3*/
     & 'no reaction mt found. collision resampled. zaid = '//ht)                !/*ac4a        2*/
      ixre=kx                                                                   !/*ac        102*/
!                                                                               !/*ac        103*/
!        get the reaction type number and parameters for acecas.                !/*ac4c       31*/
  150 ntyn=nint(xss(jxs(ljxs+5,iex)+ixre-1))                                    !/*ac4c       32*/
      mtp=nint(xss(jxs(ljxs+3,iex)+ixre-1))                                     !/*ac4b        4*/
      q=xss(jxs(ljxs+4,iex)+ixre-1)                                             !/*ac4c       33*/
      ia=jxs(ljxs+9,iex)                                                        !/*ac4c       34*/
      ka=nint(xss(jxs(ljxs+8,iex)+ixre))                                        !/*ac4c       35*/
      id=jxs(ljxs+11,iex)                                                       !/*ac4c       36*/
      kd=nint(xss(jxs(ljxs+10,iex)+ixre-1))                                     !/*ac4c       37*/
!                                                                               !/*ac        106*/
!        check that the energy is within the band for this reaction.            !/*ac        107*/
      if(nty(lnty+iex).ne.2)go to 160                                           !/*ac        108*/
      l=jxs(ljxs+11,iex)+2+nint(xss(jxs(ljxs+10,iex)+ixre-1))                   !/*ac4c       38*/
      ie=2*nint(xss(l))+l                                                       !/*ac4c       39*/
      if(erg.le.xss(ie+2).or.erg.ge.xss(ie+1+nint(xss(ie+1))))go to 180         !/*ac4c       40*/
!                                                                               !/*ac        112*/
!        sample the energy and scattering angle of emerging neutrons.           !/*ac        113*/
  160 ns=1                                                                      !/*ac        114*/
      if(abs(ntyn).ge.100)go to 190                                             !/*ac4a        3*/
      if(ntyn.ne.19) then                                                       !/*acxc        5*/
!        (n,xn) reactions.                                                      !/*acxc        6*/
         ns=abs(ntyn)                                                           !/*acxc        7*/
         go to 163                                                              !/*acxc        8*/
      else if(nxs(lnxs+8,iex).ne.0.and.dnb.ne.zero) then                        !/*acxc        9*/
!        sample prompt and delayed fission.                                     !/*acxc       10*/
         call acedel(jq)                                                        !/*acxc       11*/
         return                                                                 !/*acxc       12*/
      else if(jq.eq.2) then                                                     !/*acxc       13*/
!        jq=2: call from colidk; get prompt fission energy only.                !/*acxc       14*/
         cmult=aint(rtc(krtc+10,iex)+rang())                                    !/*acxc       15*/
         call acecas(1,1,q,ia,ka,id,kd)                                         !/*acxc       16*/
         return                                                                 !/*acxc       17*/
      else                                                                      !/*acxc       18*/
!        sample prompt fission.                                                 !/*acxc       19*/
         fn=acenu(jxs(ljxs+2,iex))                                              !/*acxc       20*/
         ns=fn+rang()                                                           !/*acxc       30*/
      endif                                                                     !/*acxc       31*/
  163 cmult=ns                                                                  !/*acxc       32*/
      do 170 i=1,ns                                                             !/*ac        118*/
      call acecas(i,1,q,ia,ka,id,kd)                                            !/*ac4c       47*/
  170 if(kdb.ne.0)return                                                        !/*ac        120*/
      return                                                                    !/*ac        121*/
!                                                                               !/*ac        122*/
!        try again, up to 100 times, if the energy is out of the                !/*ac        123*/
!        reaction band.                                                         !/*ac        124*/
  180 if(erg.le.xss(jxs(ljxs+1,iex)).or.erg.gt.xss(jxs(ljxs+1,iex)+             !/*ac4b        5*/
     & nxs(lnxs+3,iex)-1))go to 160                                             !/*ac4b        6*/
      nt=nt+1                                                                   !/*ac4b        7*/
      ndr(kndr+iex)=ndr(kndr+iex)+1                                             !/*ac        126*/
      if(nt.le.100)go to 10                                                     !/*ac        127*/
      call expirx(1,'acecol',                                                   !/*ac        128*/
     & 'energy was not within the band for the reaction 100 times.')            !/*ac        129*/
      return                                                                    !/*ac        130*/
!                                                                               !/*ac4a        4*/
!        high energy reaction other than fission with energy-dependent          !/*ac4a        5*/
!        multiplicity.  kalbach-87 (law 44) endf/b-vi.                          !/*ac4a        6*/
!                                                                               !/*ac4a        7*/
!        the location of the multiplicity table relative to dlw is              !/*ac4a        8*/
!        abs(ntyn)-100.  acefcn gets non-fission reaction multiplicity.         !/*ac4a        9*/
  190 l=jxs(ljxs+11,iex)+abs(ntyn)-101                                          !/*ac4a       10*/
      cmult=acefcn(l,erg,ln)                                                    !/*ac4a       11*/
      if(cmult.lt.1.)go to 210                                                  !/*ac4a       12*/
      cmult=aint(cmult+rang())                                                  !/*ac4a       13*/
      do 200 i=1,int(cmult)                                                     !/*ac4a       14*/
      call acecas(i,1,q,ia,ka,id,kd)                                            !/*ac4c       48*/
  200 if(kdb.ne.0)return                                                        !/*ac4a       16*/
      return                                                                    !/*ac4a       17*/
!                                                                               !/*ac4a       18*/
!        multiplicity < 1.0 ; reduce weight by "absorption."                    !/*ac4a       19*/
!                                                                               !/*ac4a       20*/
!        analog capture case.                                                   !/*ac4a       21*/
  210 if(wc1(1).ne.0..and.erg.gt.emcf(1))go to 220                              !/*ac4a       22*/
      if(cmult+rang().le.1.)go to 280                                           !/*ac4a       23*/
      go to 270                                                                 !/*ac4a       24*/
!                                                                               !/*ac4a       25*/
!        otherwise simulate capture by weight reduction.                        !/*ac4a       26*/
  220 if(cmult.eq.0.)go to 280                                                  !/*ac4a       27*/
      w1=(1.-cmult)*wgt                                                         !/*ac4a       28*/
      wgt=wgt*cmult                                                             !/*ac4a       29*/
      if(nsr.ne.71)go to 240                                                    !/*ac4a       30*/
      rlttc(2,1)=rlttc(2,1)+w1*tme                                              !/*ac4b        8*/
      rlttc(2,2)=rlttc(2,2)+w1                                                  !/*ac4b        9*/
  240 tmavtc(1,2)=tmavtc(1,2)+tme*w1                                            !/*ac4c       49*/
      tmavtc(1,3)=tmavtc(1,3)+tme*w1                                            !/*ac4a       36*/
      call lose_pax (loss_neutron_capture, 1, 0, w1, w1*eg0)                    !/*acx         6*/
      call contrib_pwb (1,ipwb_neutron_capture,icl,-w1)                         !/*acx         7*/
      pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+w1                                  !/*ac4a       40*/
!                                                                               !/*ac4a       41*/
!        do russian roulette on weight cutoff as required.                      !/*ac4a       42*/
      if(wwp(1,4).ne.0.)go to 270                                               !/*ac4c       50*/
      if(for(lfor+1,icl).ne.0.)go to 270                                        !/*ac4a       44*/
      if(idx.eq.0)go to 250                                                     !/*ac4a       45*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*dxw(1,2))go to 270                     !/*acx         8*/
      t1=dxw(1,1)*fiml9(1,1)/(gamef*fiml(1))                                    !/*acx         9*/
      go to 260                                                                 !/*ac4a       48*/
  250 if(wgt*gamef*fiml(1).gt.fiml9(1,1)*wcs2tc(1))go to 270                    !/*acx        10*/
      t1=wcs1tc(1)*fiml9(1,1)/(gamef*fiml(1))                                   !/*acx        11*/
  260 if(wgt.lt.t1*rang())go to 300                                             !/*ac4a       51*/
      call gain_pax(igain_all_pls_weight_cutoff,1,0,t1-wgt,(t1-wgt)*eg0)        !/*acx        12*/
      call contrib_pwb (1,ipwb_variance_weight_cutoff,icl,t1-wgt)               !/*acx        13*/
      wgt=t1                                                                    !/*ac4a       55*/
!                                                                               !/*ac4a       56*/
!        sample the energy and scattering angle of emerging neutrons.           !/*ac4a       57*/
  270 cmult=1.                                                                  !/*ac4a       58*/
      call acecas(1,1,q,ia,ka,id,kd)                                            !/*ac4c       51*/
      return                                                                    !/*ac4a       60*/
!                                                                               !/*ac4a       61*/
!        kill particles of zero multiplicity or by analog capture.              !/*ac4a       62*/
  280 nter=loss_neutron_capture                                                 !/*acx        14*/
      pan(kpan+1,3,mpan)=pan(kpan+1,3,mpan)+wgt                                 !/*ac4a       64*/
      if(krflg.ne.0)call eventp(4)                                              !/*ac4a       65*/
      if(nsr.ne.71)return                                                       !/*ac4a       66*/
      rlttc(2,1)=rlttc(2,1)+wgt*tme                                             !/*ac4b       12*/
      rlttc(2,2)=rlttc(2,2)+wgt                                                 !/*ac4b       13*/
      return                                                                    !/*ac4a       70*/
!                                                                               !/*ac4a       71*/
!        kill particles below weight cutoff.                                    !/*ac4a       72*/
  300 nter=loss_all_pls_weight_cutoff                                           !/*acx        15*/
      if(krflg.ne.0)call eventp(4)                                              !/*ac4a       74*/
      return                                                                    !/*ac4a       75*/
      end                                                                       !/*ac        131*/

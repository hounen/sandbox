!#ident collpn.F from deck cpn4c2                                               !/*cpn4c2      1*/
      subroutine collpn                                                         !/*cpn4c2      2*/
!        generate and bank particles from a photonuclear collision.             !/*cpn4c2      3*/
      use global_data                                                           !/*cpnx        1*/
!                                                                               !/*cpnx        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*cpnx        3*/
      implicit integer(kindi) (i-n)                                             !/*cpnx        4*/
!                                                                               !/*cpnx        5*/
      character ht*10                                                           !/*cpn4c2      5*/
!                                                                               !/*cpn4c2      6*/
!        reduce total photon cross section by photonuclear portion              !/*cpn4c2      7*/
!        for possible continued photoatomic transport in colidp.                !/*cpn4c2      8*/
!        save totm in case it gets stepped on in tallyd or dxtran.              !/*cpn4c2      9*/
      totm=totm-totpn                                                           !/*cpn4c2     10*/
      tm=totm                                                                   !/*cpn4c2     11*/
!                                                                               !/*cpn4c2     12*/
!        for biased photonuclear production, split particle:                    !/*cpn4c2     13*/
!          weight wg goes to photonuclear absorption and                        !/*cpn4c2     14*/
!          weight wgt goes to photoatomic process.                              !/*cpn4c2     15*/
      if(ispn.le.0)go to 10                                                     !/*cpnx        6*/
      wg=wgt*totpn/(totm+totpn)                                                 !/*cpn4c2     17*/
      wgt=wgt-wg                                                                !/*cpn4c2     18*/
      tmavtc(2,2)=tmavtc(2,2)+tme*wg                                            !/*cpn4c2     19*/
      tmavtc(2,3)=tmavtc(2,3)+tme*wg                                            !/*cpn4c2     20*/
      call lose_pax (loss_photon_photonuclear, 2, 0, wg, erg*wg)                !/*cpnx        7*/
      call contrib_pwb (2,ipwb_photon_photonuc_abs,icl,-wg)                     !/*cpnx        8*/
      go to 20                                                                  !/*cpn4c2     24*/
!                                                                               !/*cpn4c2     25*/
!        for unbiased photonuclear production, sample for photonuclear          !/*cpn4c2     26*/
!        reaction.  if no, continue photoatomic transport in colidp.            !/*cpn4c2     27*/
!        if yes, entire weight wg=wgt absorbed.                                 !/*cpn4c2     28*/
   10 if(rang()*(totm+totpn).gt.totpn)return                                    !/*cpn4c2     29*/
      nter = loss_photon_photonuclear                                           !/*cpnx        9*/
      wg=wgt                                                                    !/*cpn4c2     31*/
!                                                                               !/*cpn4c2     32*/
!******************** sample photonuclear event ************************        !/*cpn4c2     33*/
!                                                                      *        !/*cpn4c2     34*/
!        save photoatomic portion of particle for further transport.            !/*cpn4c2     35*/
   20 call savpar(0,0)                                                          !/*cpn4c2     36*/
!                                                                               !/*cpn4c2     37*/
!        sample cumulative pn x-section to find the collision nuclide.          !/*cpn4c2     38*/
      mc=0                                                                      !/*cpn4c2     39*/
      im=mat(lmat+icl)                                                          !/*cpn4c2     40*/
      nn=jmd(ljmd+im)                                                           !/*cpn4c2     41*/
      iex=lmn(llmn+nn)                                                          !/*cpn4c2     42*/
      if(npq(lnpq+im).eq.1)go to 50                                             !/*cpn4c2     43*/
   30 rt=rang()*totpn                                                           !/*cpn4c2     44*/
      do 40 nn=jmd(ljmd+im),jmd(ljmd+im+1)-1                                    !/*cpn4c2     45*/
      iex=lmn(llmn+nn)                                                          !/*cpn4c2     46*/
      if(iex.ne.0)rt=rt-rtc(krtc+2,iex)*fme(lfme+nn)                            !/*cpn4c2     47*/
   40 if(rt.lt.0.)go to 50                                                      !/*cpn4c2     48*/
      mc=mc+1                                                                   !/*cpn4c2     49*/
      if(mc.lt.100)go to 30                                                     !/*cpn4c2     50*/
      call expirx(1,'collpn','photonuclear coll. nuclide not found.')           !/*cpn4c2     51*/
      return                                                                    !/*cpn4c2     52*/
!                                                                               !/*cpn4c2     53*/
!        photonuclear collision was with nnth nuclide, iex.                     !/*cpn4c2     54*/
   50 mpan=ipan(lipa+icl)+nn-jmd(ljmd+im)                                       !/*cpn4c2     55*/
      pan(kpan+3,1,mpan)=pan(kpan+3,1,mpan)+1.                                  !/*cpn4c2     56*/
      pan(kpan+3,2,mpan)=pan(kpan+3,2,mpan)+wg                                  !/*cpn4c2     57*/
      ncp=0                                                                     !/*cpn4c2     58*/
#ifdef MESHTAL                                                                  /*!cpnx       10*/
      mspf=2                                                                    !/*cpnx       11*/
#endif                                                                          /*!cpnx       12*/
      jsu=0                                                                     !/*cpn4c2     59*/
      ei=erg                                                                    !/*cpn4c2     60*/
!                                                                               !/*cpn4c2     61*/
!        sample from each species of emitted particle.                          !/*cpn4c2     62*/
      do 230 jp=1,nxs(lnxs+5,iex)                                               !/*cpn4c2     64*/
      ipt=ixs(lixs+1,jp,iex)                                                    !/*cpn4c2     65*/
      jan=not_self_anti(ipt)                                                    !/*cpnc        1*/
      if(kpt(ipt).eq.0.or.fiml(ipt).eq.0..or.fiml(1).eq.0.)go to 230            !/*cpn4c2     67*/
!                                                                               !/*cpn4c2     68*/
!        determine the total photonuclear production cross-section, tp.         !/*cpn4c2     69*/
      it=ixs(lixs+3,jp,iex)                                                     !/*cpn4c2     70*/
      if(ktc(kktc+1,iex).lt.nint(xss(it)))go to 230                             !/*cpn4c2     71*/
      if(ktc(kktc+1,iex).ge.nint(xss(it))+nint(xss(it+1))-1)go to 230           !/*cpn4c2     72*/
      ij=it+ktc(kktc+1,iex)-nint(xss(it))+2                                     !/*cpn4c2     73*/
      tp=xss(ij)+rtc(krtc+1,iex)*(xss(ij+1)-xss(ij))                            !/*cpn4c2     74*/
!                                                                               !/*cpn4c2     75*/
!        sample integer number of particles based on fractional yield.          !/*cpn4c2     76*/
      fp=tp/rtc(krtc+2,iex)                                                     !/*cpn4c2     77*/
      np=int(rang()+fp)                                                         !/*cpn4c2     78*/
      if(np.eq.0)go to 230                                                      !/*cpn4c2     79*/
      rt=fiml9(1,1)/fiml(1)                                                     !/*cpn4c2     80*/
      wz=wg                                                                     !/*cpn4c2     81*/
!                                                                               !/*cpn4c2     82*/
!        check if new particle is inside a dxtran sphere.                       !/*cpn4c2     83*/
      idx=0                                                                     !/*cpn4c2     84*/
      do 60 nd=1,ndx(ipt)                                                       !/*cpn4c2     85*/
   60 if((xxx-dxx(ipt,1,nd))**2+(yyy-dxx(ipt,2,nd))**2+                         !/*cpn4c2     86*/
     & (zzz-dxx(ipt,3,nd))**2.lt.dxx(ipt,5,nd))idx=nd                           !/*cpn4c2     87*/
!                                                                               !/*cpn4c2     88*/
!        the miminum weight is set by the dxtran weight cutoff.                 !/*cpn4c2     89*/
!        allow only analog photonuclear production with pulse height.           !/*cpnc        2*/
      if(abs(kf8).ge.3.and.dbcn(24).eq.zero)go to 110                           !/*cpnc        3*/
      if(idx.eq.0.or.wwp(ipt,5).lt.0.)go to 70                                  !/*cpn4c2     90*/
      ws=dxw(ipt,1)*rt                                                          !/*cpn4c2     91*/
      wl=dxw(ipt,2)*rt                                                          !/*cpn4c2     92*/
      go to 100                                                                 !/*cpn4c2     93*/
!                                                                               !/*cpn4c2     94*/
!        or it is set by the weight windows.                                    !/*cpn4c2     95*/
   70 if(nww(ipt).eq.0)go to 90                                                 !/*cpn4c2     96*/
!        time dependent windows.                                                !/*cpn4c2     97*/
      if(wwp(ipt,6).eq.0.)go to 76                                              !/*cpn4c2     98*/
      do 72 i=1,nww(ipt)                                                        !/*cpn4c2     99*/
   72 if(tme.lt.wwe(lwwe+i+mww(ipt)))go to 74                                   !/*cpn4c2    100*/
   74 j=min(nww(ipt),i)                                                         !/*cpn4c2    101*/
      wl=wwval(ipt,icl,j,1,ix)                                                  !/*cpn4c2    102*/
      if(wl.gt.0.)go to 80                                                      !/*cpn4c2    103*/
      if(wl.eq.0.)go to 90                                                      !/*cpn4c2    104*/
      go to 230                                                                 !/*cpn4c2    105*/
!        minimum positive energy-dependent windows.                             !/*cpn4c2    106*/
   76 wl=huge                                                                   !/*cpn4c2    107*/
      wp=-1.                                                                    !/*cpn4c2    108*/
      do 78 i=1,nww(ipt)                                                        !/*cpn4c2    109*/
      wv=wwval(ipt,icl,i,1,ix)                                                  !/*cpn4c2    110*/
      wp=max(wv,wp)                                                             !/*cpn4c2    111*/
   78 if(wv.gt.0.)wl=min(wv,wl)                                                 !/*cpn4c2    112*/
      if(wp.lt.0.)go to 230                                                     !/*cpn4c2    113*/
      if(wp.eq.0.)go to 90                                                      !/*cpn4c2    114*/
!        split particles above weight window.                                   !/*cpn4c2    115*/
   80 ws=wwp(ipt,2)*wl                                                          !/*cpn4c2    116*/
      wu=wwp(ipt,1)*wl                                                          !/*cpn4c2    117*/
      if(wz.le.wu)go to 100                                                     !/*cpn4c2    118*/
      ns=min(wwp(ipt,3),1.+wz/wu)                                               !/*cpn4c2    119*/
      np=np*ns                                                                  !/*cpn4c2    120*/
      wz=wz/ns                                                                  !/*cpn4c2    121*/
      go to 110                                                                 !/*cpn4c2    122*/
!                                                                               !/*cpn4c2    123*/
!        or else it is set by the weight cutoff.                                !/*cpn4c2    124*/
   90 ws=wcs1tc(ipt)*rt                                                         !/*cpn4c2    125*/
      wl=wcs2tc(ipt)*rt                                                         !/*cpn4c2    126*/
!                                                                               !/*cpn4c2    127*/
!        roulette particles if necessary.                                       !/*cpn4c2    128*/
  100 if(wl.le.0.)go to 110                                                     !/*cpn4c2    129*/
      if(wz.gt.wl)go to 110                                                     !/*cpn4c2    130*/
      wz=np*wz                                                                  !/*cpn4c2    131*/
      np=1                                                                      !/*cpn4c2    132*/
      if(wz.gt.wl)go to 110                                                     !/*cpn4c2    133*/
      if(wz.lt.ws*rang())go to 230                                              !/*cpn4c2    134*/
      wz=ws                                                                     !/*cpn4c2    135*/
!                                                                               !/*cpn4c2    136*/
!        sample emission parameters and bank each particle.                     !/*cpn4c2    137*/
  110 do 220 ii=1,np                                                            !/*cpn4c2    138*/
      wgt=wz                                                                    !/*cpn4c2    139*/
      ixre=1                                                                    !/*cpn4c2    140*/
      nr=ixs(lixs+2,jp,iex)                                                     !/*cpn4c2    141*/
      if(nr.eq.1)go to 180                                                      !/*cpn4c2    142*/
!                                                                               !/*cpn4c2    143*/
!        sample which reaction, ixre, produces photonuclear particle            !/*cpn4c2    144*/
!        by subtracting partial cross sections, px, from rx=tp*rang().          !/*cpn4c2    145*/
!        return here up to mp=100 times if partials do not add to tp.           !/*cpn4c2    146*/
      mp=0                                                                      !/*cpn4c2    147*/
      is=ixs(lixs+8,jp,iex)                                                     !/*cpn4c2    148*/
  120 rx=tp*rang()                                                              !/*cpn4c2    149*/
!                                                                               !/*cpn4c2    150*/
!        loop over the available reactions until xs fulfilled.                  !/*cpn4c2    151*/
!        ks=the next yield block offset locator.                                !/*cpn4c2    152*/
!        jx=mftype, the type of data to check.                                  !/*cpn4c2    153*/
      do 170 ixre=1,nr                                                          !/*cpn4c2    154*/
      ks=nint(xss(ixs(lixs+7,jp,iex)+ixre-1))                                   !/*cpn4c2    155*/
      jx=nint(xss(is+ks-1))                                                     !/*cpn4c2    156*/
!                                                                               !/*cpn4c2    157*/
!        handle mftype = 6, 12 or 16.                                           !/*cpn4c2    158*/
!          energy dependent yield xs multiplier.                                !/*cpn4c2    159*/
!          data is in format:                                                   !/*cpn4c2    160*/
!            xss(is+ks-1) = mftype - reaction type indicator                    !/*cpn4c2    161*/
!            xss(is+ks)   = mtmult - mt reaction applied to                     !/*cpn4c2    162*/
!            xss(is+ks+...) std. energy/data table entries                      !/*cpn4c2    163*/
!              nr, [nbt(i:i=1..nr), int(i:i=1..nr),]                            !/*cpn4c2    164*/
!              nv, e(i:i=1..nv), y(i:i=1..nv)                                   !/*cpn4c2    165*/
      if(jx.ne.6.and.jx.ne.12.and.jx.ne.16)go to 150                            !/*cpn4c2    166*/
!                                                                               !/*cpn4c2    167*/
!        get yield value for incident photon energy.                            !/*cpn4c2    168*/
      yd=acefcn(is+ks+1,ei,ln)                                                  !/*cpn4c2    169*/
!                                                                               !/*cpn4c2    170*/
!        look up pointer to mt reaction xs data.                                !/*cpn4c2    171*/
      do 130 im=1,nxs(lnxs+4,iex)                                               !/*cpn4c2    172*/
  130 if(nint(xss(is+ks)).eq.nint(xss(jxs(ljxs+6,iex)+im-1)))go to 140          !/*cpn4c2    173*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*cpn4c2    174*/
      call errprn(1,-1,2,zero+ixre,xss(is+ks),'ixr','mt',                       !/*cpn4c2    175*/
     & 'cound not find mt for yield multiplier. zaid = '//ht)                   !/*cpn4c2    176*/
      call expirx(1,'collpn','photonuclear yield multiplier mt wrong.')         !/*cpn4c2    177*/
      return                                                                    !/*cpn4c2    178*/
!                                                                               !/*cpn4c2    179*/
!        get partial production cross section, px=yd*xs from                    !/*cpn4c2    180*/
!        yield data, yd, and reaction cross section, xs.                        !/*cpn4c2    181*/
!        look up reaction xs using mt index im found above.                     !/*cpn4c2    182*/
  140 xs=0.                                                                     !/*cpn4c2    183*/
      it=jxs(ljxs+9,iex)+nint(xss(jxs(ljxs+8,iex)+im-1))-1                      !/*cpn4c2    184*/
      ij=it+ktc(kktc+1,iex)-nint(xss(it))+2                                     !/*cpn4c2    185*/
      if(ktc(kktc+1,iex).ge.nint(xss(it)).and.ktc(kktc+1,iex)                   !/*cpn4c2    186*/
     & .lt.nint(xss(it))+nint(xss(it+1))-1)xs=xss(ij)+                          !/*cpn4c2    187*/
     & rtc(krtc+1,iex)*(xss(ij+1)-xss(ij))                                      !/*cpn4c2    188*/
      px=yd*xs                                                                  !/*cpn4c2    189*/
      go to 160                                                                 !/*cpn4c2    190*/
!                                                                               !/*cpn4c2    191*/
!        handle mftype = 13.                                                    !/*cpn4c2    192*/
!          get partial production cross section, px, directly.                  !/*cpn4c2    193*/
!          data is in format:                                                   !/*cpn4c2    194*/
!            xss(is+ks)   = mftype - reaction type indicator                    !/*cpn4c2    195*/
!            xss(is+ks+1) = ie - index of first value                           !/*cpn4c2    196*/
!            xss(is+ks+2) = nv - number of values listed                        !/*cpn4c2    197*/
!            xss(is+ks+3..is+ks+2+nv-1) = pxs(ie..ie+nv-1)                      !/*cpn4c2    198*/
!                                                                               !/*cpn4c2    199*/
  150 if(nint(xss(is+ks)).ne.13)go to 155                                       !/*cpn4c2    200*/
      px=0.                                                                     !/*cpn4c2    201*/
      it=is+ks+1                                                                !/*cpn4c2    202*/
      ij=it+ktc(kktc+1,iex)-nint(xss(it))+2                                     !/*cpn4c2    203*/
      if(ktc(kktc+1,iex).ge.nint(xss(it)).and.ktc(kktc+1,iex)                   !/*cpn4c2    204*/
     & .lt.nint(xss(it))+nint(xss(it+1))-1)px=xss(ij)+                          !/*cpn4c2    205*/
     & rtc(krtc+1,iex)*(xss(ij+1)-xss(ij))                                      !/*cpn4c2    206*/
      go to 160                                                                 !/*cpn4c2    207*/
!                                                                               !/*cpn4c2    208*/
!        bad trouble if not mftype 6, 12, 13 or 16.                             !/*cpn4c2    209*/
  155 call zaid(2,ht,ixl(lixl+1,iex))                                           !/*cpn4c2    210*/
      call errprn(1,-1,2,zero+ixre,xss(is+ks-1),'ixr','mft',                    !/*cpn4c2    211*/
     & 'cound not find reaction type mftype. zaid = '//ht)                      !/*cpn4c2    212*/
      call expirx(1,'collpn','photonuclear reaction type not found.')           !/*cpn4c2    213*/
      return                                                                    !/*cpn4c2    214*/
!                                                                               !/*cpn4c2    215*/
!        if the partial xs has been fulfilled, use this reaction, ixre.         !/*cpn4c2    216*/
  160 rx=rx-px                                                                  !/*cpn4c2    217*/
  170 if(rx.lt.0.)go to 180                                                     !/*cpn4c2    218*/
!                                                                               !/*cpn4c2    219*/
!        resample partial xs if they do not sum to total, tp.                   !/*cpn4c2    220*/
      mp=mp+1                                                                   !/*cpn4c2    221*/
      npum=npum+1                                                               !/*cpn4c2    222*/
      if(mp.lt.100)go to 120                                                    !/*cpn4c2    223*/
      call expirx(1,'collpn','photonuclear reaction type not found.')           !/*cpn4c2    224*/
      return                                                                    !/*cpn4c2    225*/
!                                                                               !/*cpn4c2    226*/
!        ixre (sampled reaction) now found.  sample a new particle.             !/*cpn4c2    227*/
  180 erg=ei                                                                    !/*cpn4c2    228*/
      ipsc=0                                                                    !/*cpn4c2    229*/
      npa=1                                                                     !/*cpn4c2    230*/
      mtp=nint(xss(ixs(lixs+5,jp,iex)+ixre-1))                                  !/*cpn4c2    231*/
      ntyn=nint(xss(ixs(lixs+6,jp,iex)+ixre-1))                                 !/*cpn4c2    232*/
      ia=ixs(lixs+10,jp,iex)                                                    !/*cpn4c2    233*/
      ka=nint(xss(ixs(lixs+9,jp,iex)+ixre-1))                                   !/*cpn4c2    234*/
      id=ixs(lixs+12,jp,iex)                                                    !/*cpn4c2    235*/
      kd=nint(xss(ixs(lixs+11,jp,iex)+ixre-1))                                  !/*cpn4c2    236*/
      call acecas(1,2,zero,ia,ka,id,kd)                                         !/*cpn4c2    237*/
      if(kdb.ne.0)return                                                        !/*cpn4c2    238*/
      erg=colout(1,1)                                                           !/*cpn4c2    239*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                      !/*cpn4c2    240*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*cpn4c2    241*/
      if(kdb.ne.0)return                                                        !/*cpn4c2    242*/
!                                                                               !/*cpn4c2    243*/
!        check energy and weight window cutoffs.                                !/*cpn4c2    244*/
      if(erg.lt.elc(ipt))go to 220                                              !/*cpn4c2    245*/
      if(nww(ipt).le.1.or.wwp(ipt,6).ne.0.)go to 210                            !/*cpn4c2    246*/
      if(idx.ne.0.and.wwp(ipt,5).ge.0.)go to 210                                !/*cpn4c2    247*/
      do 190 i=1,nww(ipt)                                                       !/*cpn4c2    248*/
  190 if(erg.lt.wwe(lwwe+i+mww(ipt)))go to 200                                  !/*cpn4c2    249*/
  200 j=min(nww(ipt),i)                                                         !/*cpn4c2    250*/
      t=wwval(ipt,icl,j,1,ix)                                                   !/*cpn4c2    251*/
      if(wgt.ge.t)go to 210                                                     !/*cpn4c2    252*/
      t1=wwp(ipt,2)*t                                                           !/*cpn4c2    253*/
      if(wgt.lt.rang()*t1)go to 220                                             !/*cpn4c2    254*/
      wgt=t1                                                                    !/*cpn4c2    255*/
!                                                                               !/*cpn4c2    256*/
!        update summaries.                                                      !/*cpn4c2    257*/
  210 if (ipt.eq.1) then                                                        !/*cpnx       13*/
        call gain_pax (igain_neutron_photonuclear,ipt,1,wgt,wgt*erg)            !/*cpnx       14*/
        call contrib_pwb (ipt,ipwb_neutron_photonuclear,icl,wgt)                !/*cpnx       15*/
        pan(kpan+3,6,mpan)=pan(kpan+3,6,mpan)+one                               !/*cpnx       16*/
        pan(kpan+3,7,mpan)=pan(kpan+3,7,mpan)+wgt                               !/*cpnx       17*/
        pan(kpan+3,8,mpan)=pan(kpan+3,8,mpan)+wgt*erg                           !/*cpnx       18*/
      else if (ipt.eq.2) then                                                   !/*cpnx       19*/
        call gain_pax (igain_photon_photonuclear, ipt, 1, wgt, wgt*erg)         !/*cpnx       20*/
        call contrib_pwb (ipt,ipwb_photon_photonuclear,icl,wgt)                 !/*cpnx       21*/
        pan(kpan+3,3,mpan)=pan(kpan+3,3,mpan)+one                               !/*cpnx       22*/
        pan(kpan+3,4,mpan)=pan(kpan+3,4,mpan)+wgt                               !/*cpnx       23*/
        pan(kpan+3,5,mpan)=pan(kpan+3,5,mpan)+wgt*erg                           !/*cpnx       24*/
      else if (ipt.eq.3) then                                                   !/*cpnx       25*/
        call expirx(1,'collpn','unexpected electron.')                          !/*cpnx       26*/
        return                                                                  !/*cpnx       27*/
      else if (charge(ipt).eq.zero) then                                        !/*cpnx       28*/
        call gain_pax (igain_gen_neu_gamma_xgen_neu,ipt,1,wgt,wgt*erg)          !/*cpnx       29*/
        call contrib_pwb (ipt,ipwb_gen_neu_gamma_xgen_neu,icl, wgt)             !/*cpnx       30*/
      else                                                                      !/*cpnx       31*/
        call gain_pax (igain_gen_chg_gamma_xgen_chg,ipt,1,wgt,wgt*erg)          !/*cpnx       32*/
        call contrib_pwb (ipt,ipwb_gen_chg_gamma_xgen_chg,icl, wgt)             !/*cpnx       33*/
      endif                                                                     !/*cpnx       34*/
!                                                                               !/*cpn4c2    269*/
!        tally detectors, create dxtran particles, and bank particle.           !/*cpn4c2    270*/
!        ipsc=17  kalbach-chadwick photonuclear systematics.                    !/*cpn4c2    271*/
!        ipsc=18  sample independent angular distribution.                      !/*cpn4c2    272*/
!        ipsc=106 sample isotropic angular distribution.                        !/*cpn4c2    273*/
!        ipsc=999 fatal error any other reaction.                               !/*cpn4c2    274*/
      if(ipsc.eq.14)then                                                        !/*cpn4c2    275*/
         ipsc=17                                                                !/*cpn4c2    276*/
      elseif(ipsc.ne.0)then                                                     !/*cpn4c2    277*/
         ipsc=999                                                               !/*cpn4c2    278*/
      elseif(ixcos.ne.0)then                                                    !/*cpn4c2    279*/
         ipsc=18                                                                !/*cpn4c2    280*/
      elseif(ixcos.eq.0)then                                                    !/*cpn4c2    281*/
         ipsc=106                                                               !/*cpn4c2    282*/
      endif                                                                     !/*cpn4c2    283*/
      if(ndx(ipt).gt.1.or.ndx(ipt).eq.1.and.idx.eq.0)call dxtran                !/*cpn4c2    284*/
      if(kdb.ne.0)return                                                        !/*cpn4c2    285*/
      if(ndet(ipt).ne.0)call tallyd                                             !/*cpn4c2    286*/
      if(kdb.ne.0)return                                                        !/*cpn4c2    287*/
      call bankit(25)                                                           !/*cpn4c2    288*/
  220 continue                                                                  !/*cpn4c2    289*/
  230 continue                                                                  !/*cpn4c2    290*/
!                                                                               !/*cpn4c2    291*/
!        retrieve incident photon for photoatomic collision in colidp.          !/*cpn4c2    292*/
      call getpar(0,0)                                                          !/*cpn4c2    293*/
      totm=tm                                                                   !/*cpn4c2    294*/
      return                                                                    !/*cpn4c2    295*/
      end                                                                       !/*cpn4c2    296*/

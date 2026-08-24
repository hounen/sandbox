!#ident acecp.F from deck xap                                                   !/*xap         1*/
      subroutine acecp                                                          !/*xap         2*/
!         generate and bank other particles from nuclear reactions              !/*xap         3*/
!         (very similar to ACEGAM for neutron-induced photons).                 !/*xap         4*/
      use global_data                                                           !/*xap         5*/
!                                                                               !/*xap         6*/
      implicit real(kindr) (a-h,o-z)                                            !/*xap         7*/
      implicit integer(kindi) (i-n)                                             !/*xap         8*/
!                                                                               !/*xap         9*/
      character ht*10                                                           !/*xap        10*/
      dimension yd(2)                                                           !/*xap        11*/
      data f/1.00000000001d0/                                                   !/*xap        12*/
      uold(1)=uuu                                                               !/*xap        13*/
      uold(2)=vvv                                                               !/*xap        14*/
      uold(3)=www                                                               !/*xap        15*/
      es=erg                                                                    !/*xap        16*/
      in=ipt                                                                    !/*xap        17*/
!                                                                               !/*xap        18*/
!         do loop over number of other secondary particles                      !/*xap        19*/
      do 10 i=1,nxs(lnxs+7,iex)                                                 !/*xap        20*/
      ip=nint(xss(jxs(ljxs+30,iex)+i-1))                                        !/*xap        21*/
!                                                                               !/*xap        22*/
!        TEMPORARY STATEMENT to avoid trying to produce particles that          !/*xap        23*/
!        we are not transporting.  will go away after RCL writes an             !/*xap        24*/
!        appropriate expung for charged particle tables (also need to           !/*xap        25*/
!        account for heating in this new expung).                               !/*xap        26*/
      if(kpt(ip).eq.0) go to 10                                                 !/*xap        27*/
      if(fiml(ip).eq.0.)go to 10                                                !/*xap        28*/
!                                                                               !/*xap        29*/
!         determine total production cross section for this particle (t)        !/*xap        30*/
      is=ixs(lixs+1,i,iex)+1                                                    !/*xap        31*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*xap        32*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)).or.ic.eq.nint(xss(is)).and.             !/*xap        33*/
     & rtc(krtc+1,iex).ne.0.) go to 10                                          !/*xap        34*/
      t=xss(ic+is)                                                              !/*xap        35*/
      if(rtc(krtc+1,iex).ne.0.) t=t+rtc(krtc+1,iex)*                            !/*xap        36*/
     & (xss(ic+is+1)-xss(ic+is))                                                !/*xap        37*/
      if(t.eq.0.) go to 10                                                      !/*xap        38*/
!                                                                               !/*xap        39*/
!         r is the ratio of total particle production cross section             !/*xap        40*/
!         to total interaction cross section and represents the average         !/*xap        41*/
!         number of particles produced per collision.                           !/*xap        42*/
      r=t/rtc(krtc+5,iex)                                                       !/*xap        43*/
!                                                                               !/*xap        44*/
!         produce 'n' of this type of secondary particles.  sample n            !/*xap        45*/
!         from nearest two integers to r.  weight of each particle is           !/*xap        46*/
!         simply the incident weight.                                           !/*xap        47*/
      n=int(r+rang())                                                           !/*xap        48*/
!                                                                               !/*xap        49*/
!  TEMPORARY WRITE                                                              !/*xap        50*/
!     if(n.gt.0.and.in.eq.9)write(iuo,11)                                       !/*xap        51*/
!    & n,int(xss(jxs(ljxs+30,iex)+i-1))                                         !/*xap        52*/
!  11 format(' PRODUCING ',i3,' sec particles with IPT = ',i2)                  !/*xap        53*/
!                                                                               !/*xap        54*/
      if(n.eq.0) go to 10                                                       !/*xap        55*/
      call savpar(0,0)                                                          !/*xap        56*/
      npa=1                                                                     !/*xap        57*/
      ipt=ip                                                                    !/*xap        58*/
      jan = not_self_anti(ipt)                                                  !/*xapc        1*/
      ncp=0                                                                     !/*xap        59*/
#ifdef MESHTAL                                                                  /*!xap        60*/
      mspf=2                                                                    !/*xap        61*/
#endif                                                                          /*!xap        62*/
      idx=0                                                                     !/*xap        63*/
      if (ipt.eq.1) then                                                        !/*xap        64*/
         nb = ipwb_neutron_tabular_sampl                                        !/*xap        65*/
         nx = igain_neutron_tabular_sampling                                    !/*xap        66*/
      else if (ipt.gt.3 .and. charge(ipt).ne.zero) then                         !/*xap        67*/
         nb = ipwb_gen_chg_tabular_sampl                                        !/*xap        68*/
         nx = igain_gen_chg_tabular_sampling                                    !/*xap        69*/
      else                                                                      !/*xap        70*/
         call expirx(1,'acecp','unexpected particle type')                      !/*xap        71*/
         return                                                                 !/*xap        72*/
      endif                                                                     !/*xap        73*/
!                                                                               !/*xap        74*/
      do 40 j=1,n                                                               !/*xap        75*/
!                                                                               !/*xap        76*/
!         sample reaction that produced the particle                            !/*xap        77*/
      ixre=1                                                                    !/*xap        78*/
      if(nint(xss(jxs(ljxs+31,iex)+i-1)).eq.1) go to 70                         !/*xap        79*/
      a=0.                                                                      !/*xap        80*/
      tt=0.                                                                     !/*xap        81*/
      b=rang()*t                                                                !/*xap        82*/
      do 80 ixre=1,nint(xss(jxs(ljxs+31,iex)+i-1))                              !/*xap        83*/
      l=ixs(lixs+5,i,iex)+                                                      !/*xap        84*/
     & nint(xss(ixs(lixs+4,i,iex)+ixre-1))+1                                    !/*xap        85*/
      if(nint(xss(l-2)).ne.12) call expirx(1,'acecp',                           !/*xap        86*/
     & 'mftype=12 expected but not found')                                      !/*xap        87*/
!                                                                               !/*xap        88*/
!         mf=12; partial cross section = yield * reaction cross section         !/*xap        89*/
      do 90 jy=1,2                                                              !/*xap        90*/
!        This call to ACEFCN incorporates the same f "safety factor" as         !/*xap        91*/
!        is used in ACEGAM. This is probably there to avoid problems at         !/*xap        92*/
!        discontinuities in yield tables. It is unknown whether it is           !/*xap        93*/
!        strictly needed or not.                                                !/*xap        94*/
   90 yd(jy)=acefcn(l,xss(jxs(ljxs+1,iex)+ktc(kktc+1,iex)+jy-2)*f,ln)           !/*xap        95*/
      if(yd(1).eq.0..and.yd(2).eq.0.) go to 80                                  !/*xap        96*/
      ix=nint(xss(l-1))                                                         !/*xap        97*/
      is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*xap        98*/
      x1=0.                                                                     !/*xap        99*/
      x2=0.                                                                     !/*xap       100*/
!        RCL writes:                                                            !/*xap       101*/
!        For the logic of the next 4 lines of the code, I originally            !/*xap       102*/
!        copied 4 lines from ACEGAM (ag4xq.9 through ag.133).  Upon             !/*xap       103*/
!        looking at the logic contained therein, I don't agree with             !/*xap       104*/
!        the treatment if we are looking up a cross section at an               !/*xap       105*/
!        energy beyond the last tabulated value for the particular              !/*xap       106*/
!        reaction.  ACEGAM, in such cases, always uses the last value           !/*xap       107*/
!        in the table.  I think it is better to use the logic found             !/*xap       108*/
!        in ACECOL (ac.76 through ac.78).  That is what I have tried            !/*xap       109*/
!        to implement here.                                                     !/*xap       110*/
      ic=ktc(kktc+1,iex)-nint(xss(is-1))+1                                      !/*xap       111*/
      if(ic.gt.0.and.ic.le.nint(xss(is))) x1=xss(is+ic)                         !/*xap       112*/
      ic=ic+1                                                                   !/*xap       113*/
      if(ic.gt.0.and.ic.le.nint(xss(is))) x2=xss(is+ic)                         !/*xap       114*/
      s=x1*yd(1)+rtc(krtc+1,iex)*(x2*yd(2)-x1*yd(1))                            !/*xap       115*/
      if(s.eq.0.) go to 80                                                      !/*xap       116*/
      if(s.gt.tt) kx=ixre                                                       !/*xap       117*/
      tt=max(s,tt)                                                              !/*xap       118*/
      a=a+s                                                                     !/*xap       119*/
      if(a.gt.b) go to 70                                                       !/*xap       120*/
   80 continue                                                                  !/*xap       121*/
!                                                                               !/*xap       122*/
!        handle failure of reaction cross sections to sum                       !/*xap       123*/
!        to the total.  treatment is same as in acegam,                         !/*xap       124*/
!        whereby a warning is issued (1st time) and the reaction                !/*xap       125*/
!        having the largest cross section is used.                              !/*xap       126*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*xap       127*/
      call errprn(1,npxm,3,zero+ipt,erg,'ipt','erg',                            !/*xap       128*/
     & 'no particle-production mt found in acecp. zaid = '//ht)                 !/*xap       129*/
      ixre=kx                                                                   !/*xap       130*/
   70 continue                                                                  !/*xap       131*/
!                                                                               !/*xap       132*/
!         call acecas to sample secondary energy                                !/*xap       133*/
      erg=es                                                                    !/*xap       134*/
      ntyn=nint(xss(ixs(lixs+3,i,iex)+ixre-1))                                  !/*xap       135*/
      mtp=nint(xss(ixs(lixs+2,i,iex)+ixre-1))                                   !/*xap       136*/
      ia=ixs(lixs+7,i,iex)                                                      !/*xap       137*/
      ka=nint(xss(ixs(lixs+6,i,iex)+ixre-1))                                    !/*xap       138*/
      id=ixs(lixs+9,i,iex)                                                      !/*xap       139*/
      kd=nint(xss(ixs(lixs+8,i,iex)+ixre-1))                                    !/*xap       140*/
      call acecas(1,in,zero,ia,ka,id,kd)                                        !/*xap       141*/
      call rotas(colout(2,1),uold,uuu,lev,irt)                                  !/*xap       142*/
!                                                                               !/*xap       143*/
!  TEMPORARY WRITE STATEMENT                                                    !/*xap       144*/
!     if(in.eq.9) write(iuo,281) colout(1,1),colout(2,1)                        !/*xap       145*/
! 281 format(5x,"sampled energy=",1pe12.5," and cosine=",e12.5,                 !/*xap       146*/
!    & " lab")                                                                  !/*xap       147*/
!                                                                               !/*xap       148*/
      erg=colout(1,1)                                                           !/*xap       149*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                      !/*xap       150*/
!                                                                               !/*xap       151*/
!         load particle in bank.                                                !/*xap       152*/
      call gain_pax (nx, ipt, 1, wgt, wgt*erg)                                  !/*xap       153*/
      call contrib_pwb (ipt,nb,icl,wgt)                                         !/*xap       154*/
#ifdef SPABI                                                                    /*!xap       155*/
      if (is_spabi.gt.0) then                                                   !/*xapa        1*/
         call sec_par_bias(ipt,1,erg,nx,nb,0,30)                                !/*xap       157*/
         go to 40                                                               !/*xap       158*/
      endif                                                                     !/*xap       159*/
#endif                                                                          /*!xap       160*/
      call bankit(30)                                                           !/*xap       161*/
   40 continue                                                                  !/*xap       162*/
      call getpar(0,0)                                                          !/*xap       163*/
   10 continue                                                                  !/*xap       164*/
      ipt=in                                                                    !/*xap       165*/
      jan = not_self_anti(ipt)                                                  !/*xapc        2*/
      return                                                                    !/*xap       166*/
      end                                                                       !/*xap       167*/

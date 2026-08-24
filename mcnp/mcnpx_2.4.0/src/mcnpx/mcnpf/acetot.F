!#ident acetot.F from deck at                                                   !/*at          1*/
      subroutine acetot(mm,mk)                                                  !/*at          2*/
!        calculate the neutron cross sections in material mk.                   !/*at          3*/
!        mm=1 for call from wtmult, otherwise zero.                             !/*at          4*/
      use global_data                                                           !/*atx         1*/
!                                                                               !/*atx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*atx         3*/
      implicit integer(kindi) (i-n)                                             !/*atx         4*/
!                                                                               !/*atx         5*/
!                                                                               !/*at          6*/
!        get the temperature ttn of cell icl.                                   !/*at          7*/
      if(mcal.ne.0)go to 310                                                    !/*at          8*/
      if(mm.ne.0)go to 50                                                       !/*at          9*/
      l=ltmp+(icl-1)*mxt                                                        !/*at         10*/
      if(tme.ge.tth(ltth+mxt))go to 40                                          !/*at         11*/
      if(tme.le.tth(ltth+1))go to 30                                            !/*at         12*/
      do 10 n=2,mxt                                                             !/*at         13*/
   10 if(tme.lt.tth(ltth+n))go to 20                                            !/*at         14*/
   20 ttn=tmp(l+n-1)+(tmp(l+n)-tmp(l+n-1))*                                     !/*at         15*/
     & (tme-tth(ltth+n-1))/(tth(ltth+n)-tth(ltth+n-1))                          !/*at         16*/
      go to 50                                                                  !/*at         17*/
   30 ttn=tmp(l+1)                                                              !/*at         18*/
      go to 50                                                                  !/*at         19*/
   40 ttn=tmp(l+mxt)                                                            !/*at         20*/
!                                                                               !/*at         21*/
!        calculate cross sections for each nuclide in the material.             !/*at         22*/
   50 totm=0.                                                                   !/*at         23*/
      siga=0.                                                                   !/*at         24*/
      lc=0                                                                      !/*at         26*/
      do 270 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*at         27*/
      iex=lme(llme+1,m)                                                         !/*at         28*/
!                                                                               !/*at         29*/
!        find the energy bin in the cross-section table.                        !/*at         30*/
      if(rtc(krtc+6,iex).eq.erg)go to 130                                       !/*at         31*/
      n=nxs(lnxs+3,iex)                                                         !/*at         32*/
      rtc(krtc+11,iex)=-1.                                                      !/*at4c        1*/
      if(nxs(lnxs+16,iex).eq.lc)go to 100                                       !/*at         33*/
      ic=jxs(ljxs+1,iex)                                                        !/*at         34*/
      ib=ic+n-1                                                                 !/*at         35*/
      if(erg.lt.rtc(krtc+6,iex))ib=ic+ktc(kktc+1,iex)                           !/*at         36*/
      if(erg.gt.rtc(krtc+6,iex))ic=ic+ktc(kktc+1,iex)-1                         !/*at         37*/
   60 if(ib-ic.eq.1)go to 80                                                    !/*at         38*/
      ih=(ic+ib)/2                                                              !/*at         39*/
      if(erg.lt.xss(ih))go to 70                                                !/*at         40*/
      ic=ih                                                                     !/*at         41*/
      go to 60                                                                  !/*at         42*/
   70 ib=ih                                                                     !/*at         43*/
      go to 60                                                                  !/*at         44*/
!                                                                               !/*at         45*/
!        get the bin index and interpolation fraction.                          !/*at4c        2*/
   80 ktc(kktc+1,iex)=ic-jxs(ljxs+1,iex)+1                                      !/*at         48*/
      if(nty(lnty+iex).eq.2)go to 90                                            !/*at         49*/
      rtc(krtc+1,iex)=max(zero,min(one,(erg-xss(ic))/(xss(ib)-xss(ic))))        !/*at         50*/
!                                                                               !/*at4c        3*/
!        sample cross sections from unresolved range probability tables.        !/*at4c        4*/
      if(iunr.eq.0)go to 82                                                     !/*at4c        5*/
      l=jxs(ljxs+23,iex)                                                        !/*at4c        6*/
      if(l.eq.0)go to 82                                                        !/*at4c        7*/
      if(erg.le.xss(l+6).or.erg.ge.xss(l+5+nint(xss(l))))go to 82               !/*at4c        8*/
      call ures(ic)                                                             !/*at4c        9*/
      go to 120                                                                 !/*at4c       10*/
!                                                                               !/*at4c       11*/
!        get absorption and total cross sections.                               !/*at4c       12*/
   82 rtc(krtc+3,iex)=xss(ic+2*n)+rtc(krtc+1,iex)*                              !/*at4c       13*/
     & (xss(ic+2*n+1)-xss(ic+2*n))                                              !/*at         52*/
      rtc(krtc+4,iex)=xss(ic+n)+rtc(krtc+1,iex)*(xss(ib+n)-xss(ic+n))           !/*at         53*/
      go to 120                                                                 !/*at         54*/
   90 lc=nxs(lnxs+16,iex)                                                       !/*at         55*/
      id=ktc(kktc+1,iex)                                                        !/*at         56*/
      go to 110                                                                 !/*at         57*/
  100 ktc(kktc+1,iex)=id                                                        !/*at         58*/
      ic=id+jxs(ljxs+1,iex)-1                                                   !/*at         59*/
  110 rtc(krtc+3,iex)=xss(ic+2*n)                                               !/*at         60*/
      rtc(krtc+4,iex)=xss(ic+n)                                                 !/*at         61*/
  120 rtc(krtc+6,iex)=erg                                                       !/*at         62*/
      rtc(krtc+7,iex)=-1.                                                       !/*at         63*/
  130 rtc(krtc+8,iex)=0.                                                        !/*at         64*/
!                                                                               !/*at         65*/
!        adjust the cross sections for thermal effects if necessary.            !/*at         66*/
      if(rtc(krtc+11,iex).ge.0.)go to 260                                       !/*at4c       14*/
      if(lmt(llmt+m).ne.0)then                                                  !/*atx         6*/
         if(erg.lt.esa(lesa+lmt(llmt+m)))go to 160                              !/*atx         7*/
      endif                                                                     !/*atx         8*/
      if(rtc(krtc+7,iex).eq.ttn)go to 260                                       !/*at         68*/
      rtc(krtc+7,iex)=ttn                                                       !/*at         69*/
      rtc(krtc+5,iex)=rtc(krtc+4,iex)                                           !/*at         70*/
      if(tbt(ltbt+iex).ne.0.)go to 260                                          !/*at         71*/
      a2=awn(lawn+iex)*erg                                                      !/*at         72*/
      if(a2.gt.500.*ttn)go to 260                                               !/*at         73*/
      if(a2.lt.4.*ttn)go to 140                                                 !/*at         74*/
      f=.5*ttn/a2                                                               !/*at         75*/
      go to 150                                                                 !/*at         76*/
  140 a=sqrt(a2/ttn)                                                            !/*at         77*/
      b=25.*a                                                                   !/*at         78*/
      i=b                                                                       !/*at         79*/
      f=(thgf(i)+(b-i)*(thgf(i+1)-thgf(i)))/a-1.                                !/*at         80*/
  150 i=ktc(kktc+1,iex)+jxs(ljxs+1,iex)-1+3*nxs(lnxs+3,iex)                     !/*at         81*/
      rtc(krtc+5,iex)=rtc(krtc+5,iex)+f*(xss(i)+rtc(krtc+1,iex)*                !/*at         82*/
     & (xss(i+1)-xss(i)))                                                       !/*at         83*/
      go to 260                                                                 !/*at         84*/
!                                                                               !/*at         85*/
!        calculate the s(a,b) inelastic scattering cross section.               !/*at         86*/
  160 it=lmt(llmt+m)                                                            !/*at         87*/
      if(rtc(krtc+6,it).eq.erg)go to 250                                        !/*at         88*/
      n=nint(xss(jxs(ljxs+1,it)))                                               !/*at4c       15*/
      ic=jxs(ljxs+1,it)+1                                                       !/*at         90*/
      ib=jxs(ljxs+1,it)+n                                                       !/*at         91*/
      if(erg.lt.rtc(krtc+6,it))ib=ic+ktc(kktc+1,it)                             !/*at         92*/
      if(erg.gt.rtc(krtc+6,it))ic=ic+ktc(kktc+1,it)-1                           !/*at         93*/
      rtc(krtc+6,it)=erg                                                        !/*at         94*/
  170 if(ib-ic.eq.1)go to 190                                                   !/*at         95*/
      ih=(ib+ic)/2                                                              !/*at         96*/
      if(erg.lt.xss(ih))go to 180                                               !/*at         97*/
      ic=ih                                                                     !/*at         98*/
      go to 170                                                                 !/*at         99*/
  180 ib=ih                                                                     !/*at        100*/
      go to 170                                                                 !/*at        101*/
  190 ktc(kktc+1,it)=ic-jxs(ljxs+1,it)                                          !/*at        102*/
      rtc(krtc+1,it)=(erg-xss(ic))/(xss(ib)-xss(ic))                            !/*at        103*/
      rtc(krtc+8,it)=xss(ic+n)+rtc(krtc+1,it)*(xss(ic+n+1)-xss(ic+n))           !/*at        104*/
      rtc(krtc+7,it)=rtc(krtc+8,it)                                             !/*at        105*/
!                                                                               !/*at        106*/
!        calculate the s(a,b) elastic scattering cross section.                 !/*at        107*/
      if(jxs(ljxs+4,it).eq.0)go to 250                                          !/*at        108*/
      n=nint(xss(jxs(ljxs+4,it)))                                               !/*at4c       16*/
      ic=jxs(ljxs+4,it)+1                                                       !/*at        110*/
      ib=jxs(ljxs+4,it)+n                                                       !/*at        111*/
      if(nxs(lnxs+5,it).ne.4)go to 200                                          !/*at        112*/
      if(erg.le.xss(ic))go to 250                                               !/*at        113*/
      if(erg.ge.xss(ib))go to 220                                               !/*at        114*/
  200 if(ib-ic.eq.1)go to 230                                                   !/*at        115*/
      ih=(ib+ic)/2                                                              !/*at        116*/
      if(erg.lt.xss(ih))go to 210                                               !/*at        117*/
      ic=ih                                                                     !/*at        118*/
      go to 200                                                                 !/*at        119*/
  210 ib=ih                                                                     !/*at        120*/
      go to 200                                                                 !/*at        121*/
  220 ic=ib                                                                     !/*at        122*/
  230 ktc(kktc+2,it)=ic-jxs(ljxs+4,it)                                          !/*at        123*/
      if(nxs(lnxs+5,it).ne.4)go to 240                                          !/*at        124*/
      rtc(krtc+7,it)=rtc(krtc+7,it)+xss(ic+n)/erg                               !/*at        125*/
      go to 250                                                                 !/*at        126*/
  240 rtc(krtc+4,it)=(erg-xss(ic))/(xss(ib)-xss(ic))                            !/*at        127*/
      rtc(krtc+7,it)=rtc(krtc+7,it)+xss(ic+n)+rtc(krtc+4,it)*                   !/*at        128*/
     & (xss(ib+n)-xss(ic+n))                                                    !/*at        129*/
  250 rtc(krtc+5,iex)=rtc(krtc+7,it)+rtc(krtc+3,iex)                            !/*at        130*/
!                                                                               !/*at        131*/
!        accumulate the total cross section, totm, and the absorption           !/*at        132*/
!        cross section, siga.                                                   !/*at        133*/
  260 totm=totm+rtc(krtc+5,iex)*fme(lfme+m)                                     !/*at        134*/
  270 siga=siga+rtc(krtc+3,iex)*fme(lfme+m)                                     !/*at        135*/
      if(mm.ne.0)return                                                         !/*at4c       17*/
!                                                                               !/*at        136*/
!        set up for photon production biasing.                                  !/*at        137*/
      totgp1=0.                                                                 !/*at4b        2*/
      if(npikmt.eq.0)go to 290                                                  !/*at        138*/
      if(gwt(lgwt+icl).eq.-1e6)go to 290                                        !/*at        140*/
      do 280 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*at        141*/
      iex=lme(llme+1,m)                                                         !/*at        142*/
      tgp(ktgp+iex)=0.                                                          !/*at        143*/
      if(nxs(lnxs+15,iex).lt.0)go to 280                                        !/*at        144*/
      j=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*at        147*/
      qu=xss(j-1)                                                               !/*at        148*/
      if(nty(lnty+iex).eq.1)qu=qu+rtc(krtc+1,iex)*(xss(j)-xss(j-1))             !/*at        149*/
      tgp(ktgp+iex)=qu                                                          !/*at        150*/
      totgp1=totgp1+tgp(ktgp+iex)*fme(lfme+m)                                   !/*at        151*/
  280 continue                                                                  !/*at        152*/
!                                                                               !/*at        153*/
!        calculate the fission cross section and nubar.                         !/*at        154*/
  290 if(lfcl(llfc+icl).eq.0)return                                             !/*at        155*/
      do 300 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*at        156*/
      iex=lme(llme+1,m)                                                         !/*at        157*/
      l=jxs(ljxs+21,iex)                                                        !/*at        158*/
      if(l.eq.0)go to 300                                                       !/*at        159*/
      if(ktc(kktc+1,iex).lt.nint(xss(l)))go to 300                              !/*at4c       18*/
      j=l+2+ktc(kktc+1,iex)-nint(xss(l))                                        !/*at4c       19*/
      rtc(krtc+8,iex)=xss(j)+rtc(krtc+1,iex)*(xss(j+1)-xss(j))                  !/*at        162*/
      if(rtc(krtc+11,iex).ge.0.)rtc(krtc+8,iex)=rtc(krtc+12,iex)                !/*at4c       20*/
      fn=acenu(jxs(ljxs+2,iex))                                                 !/*atxc        1*/
      rtc(krtc+10,iex)=max(fn,zero)                                             !/*atxc        5*/
  300 continue                                                                  !/*at        164*/
      return                                                                    !/*at        165*/
!                                                                               !/*at        166*/
!        compute multigroup cross sections.                                     !/*at        167*/
  310 totm=0.                                                                   !/*at        168*/
      pfp=0.                                                                    !/*at        169*/
      stp=0.                                                                    !/*at        170*/
      do 330 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*at        171*/
      iex=lme(llme+1,m)                                                         !/*at        172*/
      rtc(krtc+3,iex)=xss(jxs(ljxs+6,iex)-1+jgp)                                !/*cor4-1    146*/
      rtc(krtc+5,iex)=xss(jxs(ljxs+2,iex)-1+jgp)                                !/*at        173*/
      rtc(krtc+8,iex)=0.                                                        !/*at        174*/
      if(nxs(lnxs+11,iex).eq.0)go to 320                                        !/*at        175*/
      if(jxs(ljxs+7,iex).ne.0)stp=stp+fme(lfme+m)*                              !/*at        176*/
     & xss(jxs(ljxs+7,iex)-1+jgp)                                               !/*at        177*/
      if(jxs(ljxs+8,iex).ne.0)pfp=pfp+fme(lfme+m)*                              !/*at        178*/
     & xss(jxs(ljxs+8,iex)-1+jgp)                                               !/*at        179*/
  320 if(lfcl(llfc+icl).eq.0)go to 330                                          !/*at        180*/
      if(jxs(ljxs+3,iex).eq.0)go to 330                                         !/*at        181*/
      rtc(krtc+8,iex)=xss(jxs(ljxs+3,iex)-1+jgp)                                !/*at        182*/
      rtc(krtc+10,iex)=xss(jxs(ljxs+4,iex)-1+jgp)                               !/*at        183*/
  330 totm=totm+rtc(krtc+5,iex)*fme(lfme+m)                                     !/*at        184*/
      return                                                                    !/*at        185*/
      end                                                                       !/*at        186*/

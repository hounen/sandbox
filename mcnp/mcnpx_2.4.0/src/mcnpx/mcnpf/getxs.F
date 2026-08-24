!#ident getxs.F from deck gx                                                    !/*gx          1*/
      function getxs(mt)                                                        !/*gx          2*/
!        return with cross section or other quantity identified by mt           !/*gx          3*/
!        for energy erg in cross-section table iex.                             !/*gx          4*/
      use global_data                                                           !/*gxx         1*/
!                                                                               !/*gxx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gxx         3*/
      implicit integer(kindi) (i-n)                                             !/*gxx         4*/
!                                                                               !/*gxx         5*/
!                                                                               !/*gx         14*/
      getxs=0.                                                                  !/*gx         15*/
!                                                                               !/*gxxl        1*/
      if(mcal.ne.0)go to 240                                                    !/*gxxl        2*/
      if(ipt.eq.9.or.(ipt.eq.2.and.mt.gt.0))go to 280                           !/*gxxl        3*/
!                                                                               !/*gx         16*/
!        dosimetry table.                                                       !/*gx         17*/
      if(nty(lnty+iex).ne.3)go to 30                                            !/*gx         19*/
      i=jxs(ljxs+3,iex)-1                                                       !/*gx         20*/
      do 10 ix=1,nxs(lnxs+4,iex)                                                !/*gx         21*/
   10 if(nint(xss(i+ix)).eq.mt)go to 20                                         !/*gxx         6*/
      return                                                                    !/*gx         23*/
   20 getxs=acefcn(jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))-1,           !/*gx4c        1*/
     & erg,i)                                                                   !/*gx         25*/
      return                                                                    !/*gx         26*/
!                                                                               !/*gx         27*/
!        regular table.                                                         !/*gx         28*/
   30 if(mt.gt.1000)go to 170                                                   !/*gx         29*/
      go to(60,70,160,90,90,90,90,100,110,120,130)3-mt                          !/*gx4b        1*/
!                                                                               !/*gx         31*/
! >>>>>  mt>2 -- a standard endf reaction cross section.                        !/*gx         32*/
      i=jxs(ljxs+3,iex)-1                                                       !/*gx         33*/
      do 40 ix=1,nxs(lnxs+4,iex)                                                !/*gx         34*/
   40 if(nint(xss(i+ix)).eq.mt)go to 50                                         !/*gxx         7*/
      return                                                                    !/*gx         36*/
   50 if(rtc(krtc+11,iex).lt.zero)go to 55                                      !/*gxxl        4*/
      if(mt.eq.18.or.mt.eq.19)go to 53                                          !/*gx4c        3*/
      if(mt.ne.102)go to 55                                                     !/*gx4c        4*/
      getxs=rtc(krtc+14,iex)                                                    !/*gx4c        5*/
      return                                                                    !/*gx4c        6*/
   53 continue                                                                  !/*gx4c        7*/
      getxs=rtc(krtc+12,iex)                                                    !/*gx4c        8*/
      return                                                                    !/*gx4c        9*/
   55 is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*gx4c       10*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*gx4c       11*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)))return                                  !/*gxx         8*/
      if(ic.eq.nint(xss(is)).and.rtc(krtc+1,iex).ne.0.)return                   !/*gxx         9*/
      getxs=xss(is+ic)+rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic))                !/*gx         41*/
      return                                                                    !/*gx         42*/
!                                                                               !/*gx         43*/
! >>>>>  mt=2 -- elastic scattering cross section.                              !/*gx         44*/
   60 if(rtc(krtc+11,iex).lt.0.)go to 65                                        !/*gx4c       12*/
      getxs=rtc(krtc+11,iex)                                                    !/*gx4c       13*/
      return                                                                    !/*gx4c       14*/
   65 i=jxs(ljxs+1,iex)+3*nxs(lnxs+3,iex)+ktc(kktc+1,iex)                       !/*gx4c       15*/
      getxs=xss(i-1)+rtc(krtc+1,iex)*(xss(i)-xss(i-1))+                         !/*gx         46*/
     & rtc(krtc+5,iex)-rtc(krtc+4,iex)                                          !/*gx         47*/
      return                                                                    !/*gx         48*/
!                                                                               !/*gx         49*/
! >>>>>  mt=1 -- total cross section.                                           !/*gx         50*/
   70 getxs=rtc(krtc+5,iex)                                                     !/*gx         51*/
      return                                                                    !/*gx         52*/
!                                                                               !/*gx         53*/
! >>>>>  mt=-1,-2,-3,-4 -- total, absorption, elastic, heating.                 !/*gx4b        2*/
   90 if(rtc(krtc+11,iex).lt.0.)go to 99                                        !/*gx4c       16*/
      if(mt.eq.-1)getxs=rtc(krtc+4,iex)                                         !/*gx4c       17*/
      if(mt.eq.-2)getxs=rtc(krtc+3,iex)                                         !/*gx4c       18*/
      if(mt.eq.-3)getxs=rtc(krtc+11,iex)                                        !/*gx4c       19*/
      if(mt.eq.-4)getxs=rtc(krtc+13,iex)                                        !/*gx4c       20*/
      return                                                                    !/*gx4c       21*/
   99 i=jxs(ljxs+1,iex)-mt*nxs(lnxs+3,iex)+ktc(kktc+1,iex)                      !/*gx4c       22*/
      getxs=max(zero,xss(i-1)+rtc(krtc+1,iex)*(xss(i)-xss(i-1)))                !/*gx         60*/
      return                                                                    !/*gx         61*/
!                                                                               !/*gx         62*/
! >>>>>  mt=-5 -- gamma production cross section.                               !/*gx         63*/
!        the infinite dilute photon production cross sections are used          !/*gx4c       23*/
!        in tallies within the unresolved resonance energy range.               !/*gx4c       24*/
  100 if(jxs(ljxs+12,iex).eq.0)return                                           !/*gx         64*/
      i=jxs(ljxs+12,iex)+ktc(kktc+1,iex)                                        !/*gx         65*/
      getxs=xss(i-1)+rtc(krtc+1,iex)*(xss(i)-xss(i-1))                          !/*gx         66*/
      return                                                                    !/*gx         67*/
!                                                                               !/*gx         68*/
! >>>>>  mt=-6 -- total fission cross section.                                  !/*gx         69*/
  110 if(jxs(ljxs+21,iex).eq.0)return                                           !/*gx         70*/
      if(rtc(krtc+11,iex).lt.0.)go to 115                                       !/*gx4c       25*/
      getxs=rtc(krtc+12,iex)                                                    !/*gx4c       26*/
      return                                                                    !/*gx4c       27*/
  115 is=jxs(ljxs+21,iex)+1                                                     !/*gx4c       28*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*gx4c       29*/
      if(ic.ge.1)getxs=xss(is+ic)+rtc(krtc+1,iex)*                              !/*gx         73*/
     & (xss(is+ic+1)-xss(is+ic))                                                !/*gx         74*/
      return                                                                    !/*gx         75*/
!                                                                               !/*gx         76*/
! >>>>>  mt=-7 -- fission nu.                                                   !/*gx         77*/
  120 if(jxs(ljxs+21,iex).ne.0)getxs=acenu(jxs(ljxs+2,iex))                     !/*gx4c       30*/
      return                                                                    !/*gx         79*/
!                                                                               !/*gx         80*/
! >>>>>  mt=-8 -- fission q.                                                    !/*gx         81*/
  130 do 140 i=1,22                                                             !/*gx         82*/
  140 if(nxs(lnxs+2,iex).eq.mfiss(i))go to 150                                  !/*gx4a        2*/
  150 getxs=qfiss(i)                                                            !/*gx4a        3*/
  160 return                                                                    !/*gx         85*/
!                                                                               !/*gx         86*/
! >>>>>  mt>1000 -- partial photon-production cross section.                    !/*gx         87*/
  170 if(jxs(ljxs+13,iex).eq.0)return                                           !/*gx         88*/
      do 180 ix=1,nxs(lnxs+6,iex)                                               !/*gx         89*/
  180 if(nint(xss(jxs(ljxs+13,iex)+ix-1)).eq.mt)go to 190                       !/*gxx        10*/
      return                                                                    !/*gx         91*/
  190 is=jxs(ljxs+15,iex)+nint(xss(jxs(ljxs+14,iex)+ix-1))+1                    !/*gx4c       31*/
      if(nint(xss(is-2)).eq.12)go to 210                                        !/*gxx        11*/
      ic=ktc(kktc+1,iex)-nint(xss(is-1))+1                                      !/*gx4c       32*/
      if(ic.lt.1)return                                                         !/*gx         95*/
      if(rtc(krtc+1,iex).ne.0.)go to 200                                        !/*gx         96*/
      if(ic.le.nint(xss(is)))getxs=xss(is+ic)                                   !/*gxx        12*/
      return                                                                    !/*gx         98*/
  200 if(ic.lt.nint(xss(is)))                                                   !/*gxx        13*/
     & getxs=xss(is+ic)+rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic))               !/*gx        100*/
      return                                                                    !/*gx        101*/
  210 yd=acefcn(is,erg,ln)                                                      !/*gx        102*/
      ix=nint(xss(is-1))                                                        !/*gxx        14*/
      if(ix.lt.0)go to 230                                                      !/*gx        104*/
      is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*gx4c       33*/
      ic=ktc(kktc+1,iex)-nint(xss(is-1))+1                                      !/*gx4c       34*/
      if(ic.lt.1)return                                                         !/*gx        107*/
      if(rtc(krtc+1,iex).ne.0.)go to 220                                        !/*gx        108*/
      if(ic.le.nint(xss(is)))getxs=xss(is+ic)*yd                                !/*gxx        15*/
      return                                                                    !/*gx        110*/
  220 if(ic.lt.nint(xss(is)))                                                   !/*gxx        16*/
     & getxs=(xss(is+ic)+rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic)))*yd          !/*gx        112*/
      return                                                                    !/*gx        113*/
  230 if(jxs(ljxs+21,iex).eq.0)return                                           !/*gx        114*/
      is=jxs(ljxs+21,iex)+1                                                     !/*gx        115*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*gx4c       35*/
      if(ic.ge.1)                                                               !/*gx        117*/
     & getxs=(xss(is+ic)+rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic)))*yd          !/*gx        118*/
      return                                                                    !/*gx        119*/
!                                                                               !/*gx        120*/
!        multigroup case.                                                       !/*gx        121*/
  240 if(mt.lt.0.and.mt.ge.-7)go to 270                                         !/*gx4b        4*/
      do 250 i=1,nxs(lnxs+4,iex)                                                !/*gx        124*/
  250 if(nint(xss(jxs(ljxs+9,iex)-1+i)).eq.mt)go to 260                         !/*gxx        17*/
      return                                                                    !/*gx        126*/
  260 getxs=xss(jxs(ljxs+10,iex)-1+(i-1)*nxs(lnxs+5,iex)+jgp)                   !/*gx        127*/
      return                                                                    !/*gx        128*/
  270 if(jxs(ljxs-mt+1,iex).gt.0)getxs=xss(jxs(ljxs-mt+1,iex)+jgp-1)            !/*gx4b        5*/
      return                                                                    !/*gx        130*/
!                                                                               !/*gxxl        5*/
!        proton and photonuclear case.                                          !/*gxxl        6*/
  280 call getpnx(iex,mt,n1,ne,je,jx,nf)                                        !/*gxxl        7*/
      if(ne.eq.0)return                                                         !/*gxxl        8*/
      if(erg.le.xss(je+n1))then                                                 !/*gxxl        9*/
         if(mt.lt.0)getxs=xss(jx+1)                                             !/*gxxl       10*/
      elseif(erg.ge.xss(je+ne+n1-1))then                                        !/*gxxl       11*/
         if(mt.lt.0.or.(ipt.eq.2.and.mt.lt.1000))getxs=xss(jx+ne)               !/*gxxl       12*/
      elseif(nf.ne.0)then                                                       !/*gxxl       13*/
         getxs=acefcn(nf,erg,ln)                                                !/*gxxl       14*/
      else                                                                      !/*gxxl       15*/
         if(erg.eq.rtc(krtc+6,iex))then                                         !/*gxxl       16*/
            ii=jx+ktc(kktc+1,iex)+1-n1                                          !/*gxxl       17*/
            f=rtc(krtc+1,iex)                                                   !/*gxxl       18*/
         else                                                                   !/*gxxl       19*/
            ic=je+n1                                                            !/*gxxl       20*/
            ib=je+ne+n1-1                                                       !/*gxxl       21*/
            if(erg.gt.rtc(krtc+6,iex))ic=je+ktc(kktc+1,iex)                     !/*gxxl       22*/
            if(erg.lt.rtc(krtc+6,iex))ib=je+ktc(kktc+1,iex)+1                   !/*gxxl       23*/
  290       if(ib-ic.eq.1)go to 310                                             !/*gxxl       24*/
            ih=(ic+ib)/2                                                        !/*gxxl       25*/
            if(erg.lt.xss(ih))go to 300                                         !/*gxxl       26*/
            ic=ih                                                               !/*gxxl       27*/
            go to 290                                                           !/*gxxl       28*/
  300       ib=ih                                                               !/*gxxl       29*/
            go to 290                                                           !/*gxxl       30*/
  310       ii=jx+ic-je+1-n1                                                    !/*gxxl       31*/
            f=(erg-xss(ic))/(xss(ib)-xss(ic))                                   !/*gxxl       32*/
         endif                                                                  !/*gxxl       33*/
         getxs=xss(ii)+f*(xss(ii+1)-xss(ii))                                    !/*gxxl       34*/
      endif                                                                     !/*gxxl       35*/
      return                                                                    !/*gxxl       36*/
      end                                                                       !/*gx        131*/
      subroutine getpnx(kx,mt,n1,ne,je,jx,nf)                                   !/*gxxl       37*/
!        get pointers to nuclear cross section data.                            !/*gxxl       38*/
!        input:                                                                 !/*gxxl       39*/
!           kx = iex material of nuclide.                                       !/*gxxl       40*/
!           mt = mt value provided by user.                                     !/*gxxl       41*/
!        output:                                                                !/*gxxl       42*/
!           n1 = reaction starts at energy n1 in energy table.                  !/*gxxl       43*/
!           ne = number of energy points in data table.                         !/*gxxl       44*/
!           je = pointer to word before energy table.                           !/*gxxl       45*/
!           jx = pointer to word before cross section table.                    !/*gxxl       46*/
!           nf = 0 or start of acefcn table.                                    !/*gxxl       47*/
!           ne/je/jx=0/-1/-1 if no data found.                                  !/*gxxl       48*/
!                                                                               !/*gxxl       49*/
      use global_data                                                           !/*gxxl       50*/
!                                                                               !/*gxxl       51*/
      implicit real(kindr) (a-h,o-z)                                            !/*gxxl       52*/
      implicit integer(kindi) (i-n)                                             !/*gxxl       53*/
!                                                                               !/*gxxl       54*/
      n1=1                                                                      !/*gxxl       55*/
      ne=0                                                                      !/*gxxl       56*/
      je=-1                                                                     !/*gxxl       57*/
      jx=-1                                                                     !/*gxxl       58*/
      nf=0                                                                      !/*gxxl       59*/
!                                                                               !/*gxxl       60*/
! *********************** photonuclear data ****************************        !/*gxxl       61*/
!                                                                               !/*gxxl       62*/
      if(nty(lnty+kx).ne.9)go to 20                                             !/*gxxl       63*/
      if(mt.lt.0)return                                                         !/*gxxl       64*/
      if(mt.gt.1000)go to 10                                                    !/*gxxl       65*/
!                                                                               !/*gxxl       66*/
!        mt = 1/2/3/4 = total/non-elastic/elastic/heating.                      !/*gxxl       67*/
      if(mt.le.4)then                                                           !/*gxxl       68*/
         if(jxs(ljxs+1+mt,kx).eq.0)return                                       !/*gxxl       69*/
         ne=nxs(lnxs+3,kx)                                                      !/*gxxl       70*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl       71*/
         jx=jxs(ljxs+1+mt,kx)-1                                                 !/*gxxl       72*/
!                                                                               !/*gxxl       73*/
!        mt > 4 = reaction cross sections.                                      !/*gxxl       74*/
      else                                                                      !/*gxxl       75*/
         jr=0                                                                   !/*gxxl       76*/
         do ir=1,nxs(lnxs+4,kx)                                                 !/*gxxl       77*/
            kr=nint(xss(jxs(ljxs+6,kx)+ir-1))                                   !/*gxxl       78*/
            if(kr.eq.mt)jr=ir                                                   !/*gxxl       79*/
            if(kr.lt.5)call expire(0,'getpnx','bad reaction.')                  !/*gxxl       80*/
         enddo                                                                  !/*gxxl       81*/
         if(jr.eq.0)return                                                      !/*gxxl       82*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl       83*/
         jx=jxs(ljxs+9,kx)+nint(xss(jxs(ljxs+8,kx)+jr-1))                       !/*gxxl       84*/
         n1=nint(xss(jx-1))                                                     !/*gxxl       85*/
         ne=nint(xss(jx))                                                       !/*gxxl       86*/
      endif                                                                     !/*gxxl       87*/
      return                                                                    !/*gxxl       88*/
!                                                                               !/*gxxl       89*/
!        mt > 1000:  get photonuclear yields for secondary particle mp.         !/*gxxl       90*/
   10 mu=mod(mt,1000)                                                           !/*gxxl       91*/
      mp=mt/1000                                                                !/*gxxl       92*/
      jp=0                                                                      !/*gxxl       93*/
      do ip=1,nxs(lnxs+5,kx)                                                    !/*gxxl       94*/
         if(ixs(lixs+1,ip,kx).eq.mp)jp=ip                                       !/*gxxl       95*/
      enddo                                                                     !/*gxxl       96*/
      if(jp.eq.0)return                                                         !/*gxxl       97*/
      if(mu.eq.1.or.mu.eq.4)then                                                !/*gxxl       98*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl       99*/
         jx=ixs(lixs+max(3,mu),jp,kx)+1                                         !/*gxxl      100*/
         n1=nint(xss(jx-1))                                                     !/*gxxl      101*/
         ne=nint(xss(jx))                                                       !/*gxxl      102*/
      else                                                                      !/*gxxl      103*/
         jr=0                                                                   !/*gxxl      104*/
         do ir=1,ixs(lixs+2,jp,kx)                                              !/*gxxl      105*/
            kr=nint(xss(ixs(lixs+5,jp,kx)+ir-1))                                !/*gxxl      106*/
            if(kr.eq.mu)jr=ir                                                   !/*gxxl      107*/
            if(kr.lt.5)call expire(0,'getpnx','bad reaction.')                  !/*gxxl      108*/
         enddo                                                                  !/*gxxl      109*/
         if(jr.eq.0)return                                                      !/*gxxl      110*/
         is=ixs(lixs+8,jp,kx)+nint(xss(ixs(lixs+7,jp,kx)+jr-1))                 !/*gxxl      111*/
         if(nint(xss(is)).ne.mu)call expire(0,'getpnx','bad reaction.')         !/*gxxl      112*/
         jt=nint(xss(is-1))                                                     !/*gxxl      113*/
         if(jt.eq.6.or.jt.eq.12.or.jx.eq.16)then                                !/*gxxl      114*/
!     data:  nr,nbttc(i=1,nr),int(i=1,nr),ne,e(i=1,ne),f(i=1,ne)                !/*gxxl      115*/
            nf=is+1                                                             !/*gxxl      116*/
            nr=nint(xss(nf))                                                    !/*gxxl      117*/
            je=is+2*nr+2                                                        !/*gxxl      118*/
            ne=nint(xss(je))                                                    !/*gxxl      119*/
            jx=je+ne                                                            !/*gxxl      120*/
         elseif(nint(xss(is)).eq.13)then                                        !/*gxxl      121*/
            je=jxs(ljxs+1,kx)-1                                                 !/*gxxl      122*/
            jx=is+2                                                             !/*gxxl      123*/
            ne=nint(xss(jx))                                                    !/*gxxl      124*/
            n1=nint(xss(jx-1))                                                  !/*gxxl      125*/
         else                                                                   !/*gxxl      126*/
            call expire(0,'getpnx','bad reaction.')                             !/*gxxl      127*/
         endif                                                                  !/*gxxl      128*/
      endif                                                                     !/*gxxl      129*/
      return                                                                    !/*gxxl      130*/
!                                                                               !/*gxxl      131*/
! ************************** proton data *******************************        !/*gxxl      132*/
!                                                                               !/*gxxl      133*/
   20 if(nty(lnty+kx).ne.8)return                                               !/*gxxl      134*/
!                                                                               !/*gxxl      135*/
!        mt < 5: total/non-elastic/elastic/heating/gamma production.            !/*gxxl      136*/
      if(mt.lt.5)then                                                           !/*gxxl      137*/
         if(mt.lt.-5)return                                                     !/*gxxl      138*/
         jx=jxs(ljxs+1,kx)+abs(mt)*nxs(lnxs+3,kx)-1                             !/*gxxl      139*/
         if(mt.eq.-5)then                                                       !/*gxxl      140*/
            jx=jxs(ljxs+12,kx)-1                                                !/*gxxl      141*/
            if(jx.eq.-1)return                                                  !/*gxxl      142*/
         endif                                                                  !/*gxxl      143*/
         ne=nxs(lnxs+3,kx)                                                      !/*gxxl      144*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl      145*/
         return                                                                 !/*gxxl      146*/
      endif                                                                     !/*gxxl      147*/
!                                                                               !/*gxxl      148*/
!        get interaction reaction index, ir, for reaction mu.                   !/*gxxl      149*/
      mu=mod(mt,1000)                                                           !/*gxxl      150*/
      jr=0                                                                      !/*gxxl      151*/
      if(mu.ge.5)then                                                           !/*gxxl      152*/
         do ir=1,nxs(lnxs+5,kx)                                                 !/*gxxl      153*/
            if(mu.eq.nint(xss(jxs(ljxs+3,kx)+ir-1)))jr=ir                       !/*gxxl      154*/
         enddo                                                                  !/*gxxl      155*/
         if(jr.eq.0)return                                                      !/*gxxl      156*/
      endif                                                                     !/*gxxl      157*/
!                                                                               !/*gxxl      158*/
!         mt < 1000: proton inelastic reactions.                                !/*gxxl      159*/
      if(mt.lt.1000)then                                                        !/*gxxl      160*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl      161*/
         jx=jxs(ljxs+7,kx)+nint(xss(jxs(ljxs+6,kx)+jr-1))                       !/*gxxl      162*/
         n1=nint(xss(jx-1))                                                     !/*gxxl      163*/
         ne=nint(xss(jx))                                                       !/*gxxl      164*/
         return                                                                 !/*gxxl      165*/
      endif                                                                     !/*gxxl      166*/
!                                                                               !/*gxxl      167*/
!        mt/1000 = 9:  get proton multiplicity as sampled from acefcn.          !/*gxxl      168*/
!        data:  nr,nbttc(i=1,nr),int(i=1,nr),ne,e(i=1,ne),f(i=1,ne)             !/*gxxl      169*/
      mp=mt/1000                                                                !/*gxxl      170*/
      if(mp.eq.9)then                                                           !/*gxxl      171*/
         ny=abs(nint(xss(jxs(ljxs+5,kx)+jr-1)))                                 !/*gxxl      172*/
         if(ny.lt.100)return                                                    !/*gxxl      173*/
         nf=jxs(ljxs+11,kx)+ny-101                                              !/*gxxl      174*/
         je=nf+2*nint(xss(nf))+1                                                !/*gxxl      175*/
         ne=nint(xss(je))                                                       !/*gxxl      176*/
         jx=je+ne                                                               !/*gxxl      177*/
         return                                                                 !/*gxxl      178*/
      endif                                                                     !/*gxxl      179*/
!                                                                               !/*gxxl      180*/
!        get particle index, jp, for secondary particle mp.                     !/*gxxl      181*/
      jp=0                                                                      !/*gxxl      182*/
      if(mp.gt.0)then                                                           !/*gxxl      183*/
         do ip=1,nxs(lnxs+7,kx)                                                 !/*gxxl      184*/
            if(nint(xss(jxs(ljxs+30,kx)+ip-1)).eq.mp)jp=ip                      !/*gxxl      185*/
         enddo                                                                  !/*gxxl      186*/
         if(jp.eq.0)return                                                      !/*gxxl      187*/
      endif                                                                     !/*gxxl      188*/
!                                                                               !/*gxxl      189*/
!        get proton multiplicity for secondary particle type mp.                !/*gxxl      190*/
      if(mu.lt.5)then                                                           !/*gxxl      191*/
         if(mu.eq.2.or.mu.eq.3)return                                           !/*gxxl      192*/
         je=jxs(ljxs+1,kx)-1                                                    !/*gxxl      193*/
         jx=ixs(lixs+1,jp,kx)+1                                                 !/*gxxl      194*/
         n1=nint(xss(jx-1))                                                     !/*gxxl      195*/
         ne=nint(xss(jx))                                                       !/*gxxl      196*/
         if(mu.eq.4)jx=jx+ne                                                    !/*gxxl      197*/
      else                                                                      !/*gxxl      198*/
         jr=0                                                                   !/*gxxl      199*/
         do ir=1,nint(xss(jxs(ljxs+31,kx)+jp-1))                                !/*gxxl      200*/
            kr=nint(xss(ixs(lixs+2,jp,kx)+ir-1))                                !/*gxxl      201*/
            if(kr.eq.mu)jr=ir                                                   !/*gxxl      202*/
            if(kr.lt.5)call expire(0,'getpnx','bad reaction.')                  !/*gxxl      203*/
         enddo                                                                  !/*gxxl      204*/
         if(jr.eq.0)return                                                      !/*gxxl      205*/
         is=ixs(lixs+5,jp,kx)+nint(xss(ixs(lixs+4,jp,kx)+jr-1))                 !/*gxxl      206*/
         if(nint(xss(is)).ne.mu)call expire(0,'getpnx','bad reaction.')         !/*gxxl      207*/
         if(nint(xss(is-1)).ne.12)call expire(0,'getpnx','bad mftype.')         !/*gxxl      208*/
!        data:  nr,nbttc(i=1,nr),int(i=1,nr),ne,e(i=1,ne),f(i=1,ne)             !/*gxxl      209*/
         nf=is+1                                                                !/*gxxl      210*/
         nr=nint(xss(nf))                                                       !/*gxxl      211*/
         je=is+2*nr+2                                                           !/*gxxl      212*/
         ne=nint(xss(je))                                                       !/*gxxl      213*/
         jx=je+ne                                                               !/*gxxl      214*/
      endif                                                                     !/*gxxl      215*/
      return                                                                    !/*gxxl      216*/
      end                                                                       !/*gxxl      217*/

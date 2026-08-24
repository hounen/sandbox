!#ident italpr.F from deck ip                                                   !/*ip          1*/
      subroutine italpr                                                         !/*ip          2*/
!        print information about tally ital.                                    !/*ip          3*/
      use global_data                                                           !/*ipx         1*/
      use imcn_data                                                             !/*ipx         2*/
!                                                                               !/*ipx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ipx         4*/
      implicit integer(kindi) (i-n)                                             !/*ipx         5*/
!                                                                               !/*ipx         6*/
      character hs(4)*14,hf*13,hl(4)*6,ht(18)*13,hp*11,hb(3)*6,                 !/*ip          6*/
     & hm(3)*7,hq*8,hk*10,hz*119                                                !/*ip4a        1*/
      data hs/'divrs, surface','areas, surface','volumes,  cell',               !/*ip          8*/
     & 'masses,   cell'/,hm/' ',' mev',' shakes'/                               !/*ip          9*/
      data hb/'cosine','energy',' time'/                                        !/*ip         10*/
      data hl/'loglog','linlog','loglin','linlin'/,hq/'fdusmcet'/               !/*ip         11*/
      data hk/'(): <[],u='/                                                     !/*ip4a        2*/
      dimension kj(40)                                                          !/*ipxc        1*/
      character hj(8)*51,hu(2)*8                                                !/*ipxc        2*/
      data kj/9*0,1,9*0,2,10*0,3,4,5,6,7,4*0,8/                                 !/*ipxc        3*/
      data hu/'     rem','sieverts'/                                            !/*ipxc        4*/
      data hj/'10 = ICRP-21 1971                                  ',            !/*ipxc        5*/
     & '20 = NRCP-38 1971, ANSI/ANS 6.1.1 - 1977           ',                   !/*ipxc        6*/
     & '31 = ANSI/ANS 6.1.1 - 1991 (AP anterior-posterior) ',                   !/*ipxc        7*/
     & '32 = ANSI/ANS 6.1.1 - 1991 (PA posterior-anterior) ',                   !/*ipxc        8*/
     & '33 = ANSI/ANS 6.1.1 - 1991 (LAT side exposure)     ',                   !/*ipxc        9*/
     & '34 = ANSI/ANS 6.1.1 - 1991 (ROT rotation symmetric)',                   !/*ipxc       10*/
     & '35 = ANSI/ANS 6.1.1 - 1991 (ISO isotropic)         ',                   !/*ipxc       11*/
     & '40 = ICRP-74 1996 ambient dose equivalent          '/                   !/*ipxc       12*/
!                                                                               !/*ip         12*/
!        print the order of tally printing.                                     !/*ip         13*/
      je=ital                                                                   !/*ip         14*/
      if(jfq(ljfq+1,ital).eq.0)je=0                                             !/*ip         15*/
      do 10 j=1,8                                                               !/*ip         16*/
   10 if(jfq(ljfq+j,je).lt.0)go to 20                                           !/*ip         17*/
      go to 40                                                                  !/*ip         18*/
   20 write(iuo,30)(hq((-jfq(ljfq+i,je)):(-jfq(ljfq+i,je))),i=j,8)              !/*ip         19*/
   30 format(/20h order of printing: ,8(1x,a1))                                 !/*ip         20*/
!                                                                               !/*ip         21*/
!        print the cell or surface bins.                                        !/*ip         22*/
   40 if(jptal(ljpt+2,ital).eq.5)go to 100                                      !/*ip         23*/
      l=ipnt(lipn+1,2,ital)-1                                                   !/*ip         24*/
      n1=ipnt(lipn+2,2,ital)                                                    !/*ip         25*/
      m=mod(ipnt(lipn+1,1,ital),2)                                              !/*ip         26*/
      hz=' cells   '                                                            !/*ip4a        3*/
      if(jptal(ljpt+2,ital).lt.4)hz=' surfaces'                                 !/*ip4a        4*/
      i=12                                                                      !/*ip4a        5*/
      hp=' '                                                                    !/*ip4a        6*/
      do 80 j=1,n1                                                              !/*ip4a        7*/
      l1=1                                                                      !/*ip4a        8*/
      k=nint(rtp(lrtp+l+j))                                                     !/*ip4a        9*/
      if(k.gt.1000000)go to 50                                                  !/*ip4a       10*/
      if(jptal(ljpt+2,ital).le.3)k=rtp(lrtp+l+j)                                !/*ip4c        1*/
      l1=log10(.5+float(abs(k)))+1.                                             !/*ip4a       11*/
      if(k.le.0)l1=l1+1                                                         !/*ip4a       12*/
   50 if(i+l1+1.le.119)go to 60                                                 !/*ip4a       13*/
      call begone(hz,i)                                                         !/*ip4a       14*/
      i=12                                                                      !/*ip4a       15*/
   60 if(k.lt.1000000)go to 70                                                  !/*ip4a       16*/
      hz(i:i)=hk(k-1000000:k-1000000)                                           !/*ip4a       17*/
      hp=' '                                                                    !/*ip4a       18*/
      go to 80                                                                  !/*ip4a       19*/
   70 if(hp.ne.' ')i=i+1                                                        !/*ip4a       20*/
      write(hp,'(i11)')k                                                        !/*ip4a       21*/
      hz(i:i+l1-1)=hp(12-l1:11)                                                 !/*ip4a       22*/
      if(jptal(ljpt+2,ital).gt.3)go to 80                                       !/*ip4c        2*/
      kc=namchg(2,k)                                                            !/*ip4c        3*/
      if(kc.eq.0)go to 80                                                       !/*ip4c        4*/
      if(ksm(lksm+kc).ge.0)go to 80                                             !/*ip4c        5*/
      zk=rtp(lrtp+l+j)                                                          !/*ip4c        6*/
      yk=zk-k+.001                                                              !/*ip4c        7*/
!        increment for facet of macrobody.                                      !/*ip4c        8*/
      kz=yk*10                                                                  !/*ip4c        9*/
      hp=' '                                                                    !/*ip4c       10*/
      write(hp,'(1h.,i1)')kz                                                    !/*ip4c       11*/
      hz(i+l1:i+l1+1)=hp(1:2)                                                   !/*ip4c       12*/
      i=i+2                                                                     !/*ip4c       13*/
   80 i=i+l1                                                                    !/*ip4a       23*/
      if(m.ne.0)hz(i+1:i+1)='t'                                                 !/*ip4a       24*/
      call begone(hz,i)                                                         !/*ip4a       25*/
      go to 160                                                                 !/*ip         42*/
!                                                                               !/*ip         43*/
!        print descriptions of the detectors.                                   !/*ip         44*/
  100 k=jptal(ljpt+8,ital)                                                      !/*ip         45*/
      n=iptal(lipt+1,3,ital)                                                    !/*ip         46*/
      l=iptal(lipt+1,1,ital)                                                    !/*ip         47*/
      if(k.eq.1)write(iuo,110)(j,(tds(l+5*(j-1)+i),i=1,4),j=1,n)                !/*ip         48*/
  110 format(/30h point detector specifications/                                !/*ip         49*/
     & 4x,8hdetector,8x,1hx,14x,1hy,14x,1hz,13x,2hr0/(i8,4x,1p4e15.5))          !/*ip         50*/
      if(k.eq.2)write(iuo,120)(j,(tds(l+5*(j-1)+i),i=1,2),                      !/*ip         51*/
     & char(ichar('w')+int(tds(l+5*(j-1)+3))),tds(l+5*(j-1)+4),j=1,n)           !/*ip         52*/
  120 format(/29h ring detector specifications/4x,8hdetector,                   !/*ip         53*/
     & 7x,2ha0,14x,1hr,12x,4haxis,12x,2hr0/(i8,4x,1p2e15.5,8x,a1,e21.5))        !/*ip         54*/
#ifdef RADIOG                                                                   /*!ipx         7*/
!                                                                               !/*ipx         8*/
      if (k.gt.2) then                                                          !/*ipx         9*/
         do 124 j=1,n                                                           !/*ipx        10*/
            if (k.eq.3) then                                                    !/*ipx        11*/
               write(iuo,121)(tds(l+i),i=1,3),tds(l+16),tds(l+18),              !/*ipx        12*/
     &          (tds(l+6+k),k=1,3)                                              !/*ipx        13*/
  121          format(/39h Pinhole image detector specifications:/              !/*ipx        14*/
     &          13x,1hx,13x,1hy,13x,1hz,12x,2hrp,12x,2hlf/5x,1p5e14.4/          !/*ipx        15*/
     &          13x,1hu,13x,1hv,13x,1hw/5x,3e14.4)                              !/*ipx        16*/
            else                                                                !/*ipx        17*/
  122          write(iuo,123)(tds(l+i),i=1,3),(tds(l+6+k),k=1,3)                !/*ipx        18*/
  123          format(/31h  Detector grid specifications:/                      !/*ipx        19*/
     &          13x,1hx,13x,1hy,13x,1hz/5x,1p3e14.4/                            !/*ipx        20*/
     &          13x,1hu,13x,1hv,13x,1hw/5x,3e14.4)                              !/*ipx        21*/
            endif                                                               !/*ipx        22*/
  124       l=l+19                                                              !/*ipx        23*/
      endif                                                                     !/*ipx        24*/
#endif                                                                          /*!ipx        25*/
!                                                                               !/*ip         55*/
!        print the detector contribution probabilities.                         !/*ip         56*/
      if(iptal(lipt+2,2,ital).eq.0)go to 160                                    !/*ip         57*/
      write(iuo,130)                                                            !/*ip         58*/
  130 format(/4x,46hcells whose detector probabilities are not 1.0/             !/*ip         59*/
     & 7x,11hprogr probl,8x,11hprobability)                                     !/*ip         60*/
      l=iptal(lipt+2,2,ital)                                                    !/*ip         61*/
      do 140 i=1,mxa                                                            !/*ip         62*/
  140 if(tds(l+i).ne.1.)write(iuo,150)i,ncl(lncl+i),tds(l+i)                    !/*ip         63*/
  150 format(i11,i6,f17.4)                                                      !/*ip         64*/
!                                                                               !/*ip         65*/
!        print the user bins.                                                   !/*ip         66*/
  160 if(iptal(lipt+3,1,ital).le.0)go to 200                                    !/*ip         67*/
      n=iptal(lipt+3,4,ital)                                                    !/*ip         68*/
      m=iptal(lipt+3,3,ital)                                                    !/*ip         69*/
      do 190 j=1,m,8                                                            !/*ip         70*/
      do 170 i=1,8                                                              !/*ip         71*/
      ht(i)=' '                                                                 !/*ip         72*/
      if(j+i-1.lt.n)write(ht(i),'(1pe13.5)')                                    !/*ip         73*/
     & tds(iptal(lipt+3,1,ital)+j+i-1)                                          !/*ip         74*/
  170 if(j+i-1.eq.n.and.m.eq.n)ht(i)='   total'                                 !/*ip         75*/
      if(j.eq.1)write(iuo,180)(ht(i),i=1,8)                                     !/*ip         76*/
  180 format(/10h user bins,8a13)                                               !/*ip         77*/
  190 if(j.ne.1)write(iuo,'(10x,8a13)')(ht(i),i=1,8)                            !/*ip         78*/
!                                                                               !/*ip         79*/
!        print the segment bins.                                                !/*ip         80*/
  200 if(iptal(lipt+4,1,ital).eq.0)go to 270                                    !/*ip         81*/
      k1=1+jptal(ljpt+2,ital)/2                                                 !/*ip         82*/
#ifdef RADIOG                                                                   /*!ipx        26*/
      if(jptal(ljpt+8,ital).gt.2)go to 270                                      !/*ipx        27*/
#endif                                                                          /*!ipx        28*/
      k=12                                                                      !/*ip4c       14*/
      if(ipnt(lipn+2,11,ital).ne.0)k=6                                          !/*ip         84*/
      m=iptal(lipt+4,3,ital)                                                    !/*ip         85*/
      n=iptal(lipt+4,4,ital)                                                    !/*ip         86*/
      l1=iptal(lipt+1,3,ital)                                                   !/*ip         87*/
      do 260 j=1,m,k                                                            !/*ip         88*/
      do 210 i=1,k                                                              !/*ip         89*/
      ht(i)=' '                                                                 !/*ip         90*/
      if(j+i-1.eq.n.and.m.eq.n)ht(i)='    whole'                                !/*ip4c       15*/
      if(j+i.eq.n)ht(i)='     rest'                                             !/*ip4c       16*/
      if(j+i.ge.n)go to 210                                                     !/*ip         93*/
      l=lja(llja+abs(lca(llca+iptal(lipt+4,1,ital)))+j+i-2)                     !/*ip         94*/
      write(ht(i),'(i9)')sign(nsf(lnsf+abs(l)),l)                               !/*ip4c       17*/
      if(kfq.eq.0)go to 210                                                     !/*ip4c       18*/
      hf=ht(i)                                                                  !/*ip4c       19*/
      write(ht(i)(1:9),'(a7,1h.,i1)')hf(3:9),kfq                                !/*ip4c       20*/
  210 continue                                                                  !/*ip4c       21*/
      if(k.eq.12)write(iuo,220)(ht(i),i=1,k)                                    !/*ip4c       22*/
  220 format(9h segments,12a9)                                                  !/*ip4c       23*/
      if(k.eq.12)go to 260                                                      !/*ip4c       24*/
      write(iuo,230)(ht(i),i=1,k)                                               !/*ip4c       25*/
  230 format(9h segments,25x,6(4x,a9))                                          !/*ip4c       26*/
      do 240 l=1,l1                                                             !/*ip        102*/
      call binval(l,1,hp,i)                                                     !/*ip        103*/
  240 write(iuo,250)hs(k1),hp,(tds(iptal(lipt+4,2,ital)+                        !/*ip        104*/
     & m*(l-1)+j+i-1),i=1,min(6,m-j+1))                                         !/*ip        105*/
  250 format(11h   segment ,a14,4h bin,a11,1p6e13.5)                            !/*ip        106*/
  260 continue                                                                  !/*ip        107*/
!                                                                               !/*ip        108*/
!        print the energy-dependent-multiplier bins.                            !/*ip        109*/
  270 if(iptal(lipt+5,1,ital).ne.0)go to 290                                    !/*ip        110*/
      if(iptal(lipt+5,2,ital).ne.0)write(iuo,280)                               !/*ip        111*/
     & tds(iptal(lipt+5,2,ital)+1)                                              !/*ip        112*/
  280 format(/29h all scores are multiplied by,1pe13.5)                         !/*ip        113*/
      go to 420                                                                 !/*ip        114*/
  290 write(iuo,300)                                                            !/*ip        115*/
  300 format(/16h multiplier bins/4h att,2x,8hconstant,4x,8hmaterial,           !/*ip        116*/
     & 3x,33hreactions or material-rho*x pairs)                                 !/*ip        117*/
      ln=ichar('a')-1                                                           !/*ip        118*/
      lr=2                                                                      !/*ip        119*/
      j=0                                                                       !/*ip        120*/
      n=iptal(lipt+5,4,ital)-1                                                  !/*ip        121*/
      do 370 jj=1,n                                                             !/*ip        122*/
  310 j=j+1                                                                     !/*ip        123*/
      l=tds(iptal(lipt+5,1,ital)+j)                                             !/*ip        124*/
      t=tds(iptal(lipt+5,2,ital)+j)                                             !/*ip4b        1*/
      if(l.gt.0)go to 330                                                       !/*ip4b        2*/
      if(l.ge.-2)go to 320                                                      !/*ip4b        3*/
      if(lr.eq.2)ln=ln+1                                                        !/*ip4b        4*/
      lr=2                                                                      !/*ip4b        5*/
      go to 310                                                                 !/*ip4b        6*/
  320 if(l.eq.0)write(iuo,'(1pe16.5)')t                                         !/*ip4b        7*/
      if(l.eq.-1.and.jptal(ljpt+2,ital).ne.5)                                   !/*ip4b        8*/
     & write(iuo,'(1pe16.5,4x,6htracks)')t                                      !/*ip4b        9*/
      if(l.eq.-1.and.jptal(ljpt+2,ital).eq.5)                                   !/*ip4b       10*/
     & write(iuo,'(1pe16.5,4x,10hcollisions)')t                                 !/*ip4b       11*/
      if(l.eq.-2)write(iuo,'(1pe16.5,4x,10h1/velocity)')t                       !/*ip4b       12*/
      go to 370                                                                 !/*ip4b       13*/
  330 mm=abs(tds(l))                                                            !/*ip4b       14*/
      m=mod(mm,10000000)                                                        !/*ip        134*/
      if(mm.eq.10000000+m.and.lr.eq.2)ln=ln+1                                   !/*ip        135*/
      if(mm.eq.10000000+m)lr=1                                                  !/*ip        136*/
      if(tds(l-1).ge.0.)go to 350                                               !/*ip        137*/
      write(iuo,340)t,(nmt(lnmt+int(tds(l+i))),tds(l+i+1),i=1,m,2)              !/*ip4b       15*/
  340 format(1pe16.5,i17,e12.4,4(i6,e12.4)/                                     !/*ip        140*/
     & (i33,e12.4,i6,e12.4,i6,e12.4,i6,e12.4,i6,e12.4))                         !/*ip        141*/
      go to 370                                                                 !/*ip        142*/
  350 hp=' '                                                                    !/*ip        143*/
      if(mm.gt.10000000)hp=char(ln)                                             !/*ip        144*/
      do 360 iw=1,m                                                             !/*ip        147*/
      i=mod(iw-1,14)+1                                                          !/*ip        148*/
      k=tds(l+iw)                                                               !/*ip        149*/
      ht(i)='     :'                                                            !/*ip        150*/
      if(k.ne.1000003)write(ht(i)(1:6),'(i6)')k                                 !/*ip        151*/
      if(i.lt.14.and.iw.lt.m)go to 360                                          !/*ip4b       16*/
      if(iw.le.14)write(iuo,'(2x,a1,1pe13.5,i7,5x,14a6)')hp,t,                  !/*ip4b       17*/
     & nmt(lnmt+int(tds(l-1))),(ht(k),k=1,i)                                    !/*ip4b       18*/
      if(iw.gt.14)write(iuo,'(28x,14a6)')(ht(k),k=1,i)                          !/*ip4b       19*/
  360 continue                                                                  !/*ip4b       20*/
  370 continue                                                                  !/*ip        153*/
      if(iptal(lipt+5,3,ital).eq.n+1)write(iuo,'(7x,5htotal)')                  !/*ip        154*/
      if(ln.ne.ichar('a')-1)write(iuo,380)                                      !/*ip        155*/
  380 format(/3x,11hattenuators)                                                !/*ip        156*/
      j=0                                                                       !/*ip        157*/
      do 400 jj=1,ln-ichar('a')+1                                               !/*ip        158*/
  390 j=j+1                                                                     !/*ip        159*/
      l=-tds(iptal(lipt+5,1,ital)+j)                                            !/*ip        160*/
      if(l.le.1)go to 390                                                       !/*ip        161*/
  400 write(iuo,410)char(ichar('a')-1+jj),tds(iptal(lipt+5,2,ital)+j),          !/*ip        162*/
     & (nmt(lnmt+int(tds(l+i))),tds(l+i+1),i=1,int(tds(l)),2)                   !/*ip        163*/
  410 format(2x,a1,1pe13.5,i17,e12.4,4(i6,e12.4)/                               !/*ip        164*/
     & (i33,e12.4,i6,e12.4,i6,e12.4,i6,e12.4,i6,e12.4))                         !/*ip        165*/
!                                                                               !/*ip        166*/
!        print the cosine, energy, and time bins.                               !/*ip        167*/
  420 do 470 iw=1,3                                                             !/*ip        168*/
#ifdef RADIOG                                                                   /*!ipx        29*/
      if(jptal(ljpt+8,ital).gt.2.and.iw.eq.1)go to 470                          !/*ipx        30*/
#endif                                                                          /*!ipx        31*/
      if(iptal(lipt+iw+5,1,ital).eq.0)go to 470                                 !/*ip        169*/
      k=iptal(lipt+iw+5,2,ital)                                                 !/*ip        170*/
      n=iptal(lipt+iw+5,4,ital)-1                                               !/*ip        171*/
      l=iptal(lipt+iw+5,1,ital)                                                 !/*ip        172*/
      hf=' -i'                                                                  !/*ip        173*/
      if(iw.eq.1)write(hf,'(1pe13.5)')-1.                                       !/*ip        174*/
      a=huge                                                                    !/*ip        175*/
      do 430 i=1,mipt                                                           !/*ip        176*/
  430 if(ktp(lktp+i,ital).ne.0)a=min(a,ecf(i))                                  !/*ip        177*/
      if(iw.eq.2)write(hf,'(1pe13.5)')a                                         !/*ip        178*/
      if(k.eq.0.and.n.eq.1)write(iuo,440)hb(iw),hf,tds(l+1),hm(iw)              !/*ip        179*/
      if(k.eq.0.and.n.gt.1)write(iuo,440)hb(iw),hf,(tds(l+i-1),hm(iw),          !/*ip        180*/
     & tds(l+i-1),i=2,n),tds(l+n),hm(iw)                                        !/*ip        181*/
  440 format(/1x,a6,5h bins/4x,a13,4h  to,1pe13.5,a7/                           !/*ip        182*/
     & (e17.5,4h  to,e13.5,a7))                                                 !/*ip        183*/
      if(k.ne.0.and.n.eq.1)write(iuo,450)hb(iw),hf,tds(l+1),hm(iw),             !/*ip        184*/
     & tds(k+1)                                                                 !/*ip        185*/
      if(k.ne.0.and.n.gt.1)write(iuo,450)hb(iw),hf,(tds(l+i-1),hm(iw),          !/*ip        186*/
     & tds(k+i-1),tds(l+i-1),i=2,n),tds(l+n),hm(iw),tds(k+n)                    !/*ip        187*/
  450 format(/1x,a6,5h bins,36x,10hmultiplier/4x,a13,4h  to,                    !/*ip        188*/
     & 1pe13.5,a7,e18.5/(e17.5,4h  to,e13.5,a7,e18.5))                          !/*ip        189*/
      if(iptal(lipt+iw+5,3,ital).eq.n+1)write(iuo,460)                          !/*ip        190*/
  460 format(6x,9htotal bin)                                                    !/*ip        191*/
  470 continue                                                                  !/*ip        192*/
!                                                                               !/*ip        193*/
!        print the dose function.                                               !/*ip        194*/
      if(iptal(lipt+3,2,ital).eq.0)go to 490                                    !/*ip        195*/
      ld=iptal(lipt+3,2,ital)                                                   !/*ipxc       13*/
      n=nint(tds(ld))                                                           !/*ipxc       14*/
      if(n.lt.0)then                                                            !/*ipxc       15*/
      k=nint(tds(ld+1))                                                         !/*ipxc       16*/
      iy=jptal(ljpt+1,ital)                                                     !/*ipxc       17*/
      if(k.le.0.or.k.gt.40.or.kj(k).eq.0)call erprnt(1,1,2,k,iy,0,0,0,          !/*ipxc       18*/
     & '23hbad dose function, ic =,i3,5x,7htally =,i4')                         !/*ipxc       19*/
      write(iuo,'(/24h standard dose function:,3x,a51)')hj(kj(k))               !/*ipxc       20*/
      write(iuo,'(7h units:,5x,a8,3h/hr)')hu(nint(tds(ld+2)))                   !/*ipxc       21*/
      if(tds(ld-1).eq.one)write(iuo,'(23h interpolation:  loglin)')             !/*ipxc       22*/
      if(tds(ld-1).eq.two)write(iuo,'(23h interpolation:  linlin)')             !/*ipxc       23*/
      if(tds(ld-1).eq.three)                                                    !/*ipxc       24*/
     & write(iuo,'(42h interpolation:  analytic parameterization)')             !/*ipxc       25*/
      if(tds(ld+3).gt.zero)write(iuo,'(9h factor =,1pe14.5)')tds(ld+3)          !/*ipxc       26*/
      if(nint(tds(ld+3)).eq.-1)write(iuo,'(22h factor = ICRP60(1990))')         !/*ipxc       27*/
      if(nint(tds(ld+3)).eq.-2)                                                 !/*ipxc       28*/
     & write(iuo,'(44h factor = LANSCE albatross response function)')           !/*ipxc       29*/
      else                                                                      !/*ipxc       30*/
      write(iuo,480)hl(nint(tds(ld-1))+1),                                      !/*ipxc       31*/
     & (tds(ld+i),tds(ld+n+i),i=1,n)                                            !/*ipxc       32*/
      endif                                                                     !/*ipxc       33*/
  480 format(/14h dose function,10x,22hinterpolation mode is ,a6/               !/*ip        199*/
     & 8x,6henergy,9x,6hfactor/(2x,1p2e15.5))                                   !/*ip        200*/
!                                                                               !/*ip        201*/
!        print the tally fluctuations chart bin indexes.                        !/*ip        202*/
  490 if(ipnt(lipn+1,20,ital).ne.0)                                             !/*ip        203*/
     & write(iuo,500)(jtf(ljtf+i,ital),i=1,8)                                   !/*ip        204*/
  500 format(/38h tally fluctuations chart bin indexes:,                        !/*ip        205*/
     & 32h   f   d   u   s   m   c   e   t/38x,8i4)                             !/*ip        206*/
      return                                                                    !/*ip        207*/
      end                                                                       !/*ip        208*/

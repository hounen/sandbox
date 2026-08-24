!#ident expung.F from deck ep                                                   !/*ep          1*/
      subroutine expung(hn,mg)                                                  !/*ep          2*/
!        remove data not needed by the problem from neutron table iex.          !/*ep          3*/
!        return 'total nu' or 'prompt nu' in hn for type 19 fission.            !/*ep          4*/
!        return mg=1 if gamma-production data are needed, =0 otherwise.         !/*ep          5*/
      use global_data                                                           !/*epx         1*/
!                                                                               !/*epx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*epx         3*/
      implicit integer(kindi) (i-n)                                             !/*epx         4*/
!                                                                               !/*epx         5*/
      parameter (m3=1000,m4=200)                                                !/*ep4b        1*/
      dimension jx(32),m2(m3),jf(m4),nx(8),ix(mixs,maxsec),ml(m3)               !/*epx         6*/
      character hn*(9),ht*10                                                    !/*ep          8*/
!                                                                               !/*ep          9*/
!        set lc if cross-section table iex will be used for transport.          !/*ep         10*/
!        set mg if the gamma-production data are needed for transport.          !/*ep         11*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*ep         12*/
      lc=0                                                                      !/*ep         13*/
      mg=0                                                                      !/*ep         14*/
      do 20 i=1,mxa                                                             !/*ep         15*/
      j=mat(lmat+i)                                                             !/*ep         16*/
      do 10 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                       !/*ep         17*/
      if(lme(llme+1,m).eq.iex)lc=1                                              !/*ep         18*/
      if(kpt(2).eq.0)go to 10                                                   !/*ep         19*/
      if(lme(llme+1,m).eq.iex.and.gwt(lgwt+i).ne.-1e6)mg=1                      !/*ep         20*/
   10 continue                                                                  !/*ep         21*/
   20 continue                                                                  !/*ep         22*/
!                                                                               !/*ep         23*/
!        list in jf the reaction numbers from fm cards that refer to            !/*ep         24*/
!        table iex, and set mg if gamma-production data are required.           !/*ep         25*/
!        set ih if any fm card requires heating numbers in table iex.           !/*ep         26*/
      nr=0                                                                      !/*ep         27*/
      ih=0                                                                      !/*ep         28*/
      do 80 it=1,ntal                                                           !/*ep         29*/
      iv=iptal(lipt+5,1,it)                                                     !/*ep         30*/
      if(ktp(lktp+1,it).eq.0.or.iv.eq.0)go to 80                                !/*ep         31*/
      do 70 i=1,int(tds(iv))                                                    !/*ep         32*/
      ip=abs(tds(iv+i))                                                         !/*ep         33*/
      if(ip.le.1)go to 70                                                       !/*ep         34*/
      ii=tds(ip-1)                                                              !/*ep         35*/
      if(ii.lt.0.and.lc.ne.0)go to 70                                           !/*ep         36*/
      n2=1                                                                      !/*ep         37*/
      if(ii.lt.0)n2=tds(ip)                                                     !/*ep         38*/
      n1=1                                                                      !/*ep         39*/
      if(n2.eq.1)n1=mod(int(abs(tds(ip))),10000000)                             !/*ep         40*/
      i1=1                                                                      !/*ep         41*/
      do 60 j2=1,n2,2                                                           !/*ep         42*/
      if(n2.gt.1)ii=tds(ip+j2)                                                  !/*ep         43*/
      do 50 m=jmd(ljmd+ii),jmd(ljmd+ii+1)-1                                     !/*ep         44*/
      if(lme(llme+1,m).ne.iex)go to 50                                          !/*ep         45*/
      do 40 j=1,n1                                                              !/*ep         46*/
      if(n2.eq.1)i1=tds(ip+j)                                                   !/*ep         47*/
      if(i1.eq.1000003)go to 40                                                 !/*ep         48*/
      do 30 k=1,nr                                                              !/*ep         49*/
   30 if(jf(k).eq.i1)go to 40                                                   !/*ep         50*/
      nr=nr+1                                                                   !/*ep         51*/
      if(nr.gt.m4)call erprnt(1,1,1,nr,0,0,0,0,                                 !/*ep4b        3*/
     & '44hdimension overflow in expung. increase m4 to,i5')                    !/*ep4b        4*/
      jf(nr)=i1                                                                 !/*ep         52*/
      if(i1.eq.-5.or.i1.gt.1000.and.i1.lt.1000000)mg=1                          !/*epx         7*/
      if(i1.eq.-4)ih=1                                                          !/*ep         54*/
   40 continue                                                                  !/*ep         55*/
   50 continue                                                                  !/*ep         56*/
   60 continue                                                                  !/*ep         57*/
   70 continue                                                                  !/*ep         58*/
   80 continue                                                                  !/*ep         59*/
!                                                                               !/*ep4b        5*/
!        set gamma production flag, ngmfl, for xs plotting.                     !/*ep4b        6*/
!        if mg=0 there is no gamma production for that zaid.                    !/*ep4b        7*/
!        mg will not necessarily be 0 if there is no gamma production           !/*ep4b        8*/
!        data for that particular zaid.                                         !/*ep4b        9*/
      ngmfl(lngm+iex)=mg                                                        !/*ep4b       10*/
!                                                                               !/*ep4b       11*/
!        add perturbation reaction numbers to the jf array.                     !/*ep4b       12*/
      do 84 ip=1,npert                                                          !/*ep4b       13*/
      n1=iptb(lipb+9,ip)                                                        !/*ep4b       14*/
      if(n1.eq.0)go to 84                                                       !/*ep4b       15*/
      do 83 m=nptb(lnpt+ip)+1,nptb(lnpt+ip+1)                                   !/*ep4b       16*/
      if(lme(llme+1,nint(dptb(ldpt+1,m))).ne.iex)go to 83                       !/*ep4b       17*/
      do 82 i=1,n1                                                              !/*ep4b       18*/
      i1=nint(rptb(lrpt+iptb(lipb+10,ip)+i-1))                                  !/*ep4b       19*/
      do 81 k=1,nr                                                              !/*ep4b       20*/
   81 if(jf(k).eq.i1)go to 82                                                   !/*ep4b       21*/
      nr=nr+1                                                                   !/*ep4b       22*/
      jf(nr)=i1                                                                 !/*ep4b       23*/
   82 continue                                                                  !/*ep4b       24*/
      go to 84                                                                  !/*ep4b       25*/
   83 continue                                                                  !/*ep4b       26*/
   84 continue                                                                  !/*ep4b       27*/
!                                                                               !/*ep4c        2*/
!        keep sum of inelastic mt with unresolved probability                   !/*ep4c        3*/
!        tables; if only one level involved, keep its mt.                       !/*ep4c        4*/
      l=jxs(ljxs+23,iex)                                                        !/*ep4c        5*/
      if(l.eq.0)go to 98                                                        !/*ep4c        6*/
      i1=nint(xss(l+3))                                                         !/*ep4c        7*/
      if(i1.le.0)go to 88                                                       !/*ep4c        8*/
      do 86 k=1,nr                                                              !/*ep4c        9*/
   86 if(jf(k).eq.i1)go to 88                                                   !/*ep4c       10*/
      nr=nr+1                                                                   !/*ep4c       11*/
      jf(nr)=i1                                                                 !/*ep4c       12*/
!                                                                               !/*ep4c       13*/
!        keep other absorption mt with unresolved probability tables.           !/*ep4c       14*/
   88 i1=nint(xss(l+4))                                                         !/*ep4c       15*/
      if(i1.le.0)go to 94                                                       !/*ep4c       16*/
      do 92 k=1,nr                                                              !/*ep4c       17*/
   92 if(jf(k).eq.i1)go to 94                                                   !/*ep4c       18*/
      nr=nr+1                                                                   !/*ep4c       19*/
      jf(nr)=i1                                                                 !/*ep4c       20*/
!                                                                               !/*ep4c       21*/
!        unresolved probability tables:                                         !/*ep4c       22*/
!        keep mt 102 and heating numbers (ih=1 if using factors).               !/*ep4c       23*/
   94 if(xss(l+5).ne.0.)ih=1                                                    !/*ep4c       24*/
      do 96 k=1,nr                                                              !/*ep4c       25*/
   96 if(jf(k).eq.102)go to 98                                                  !/*ep4c       26*/
      nr=nr+1                                                                   !/*ep4c       27*/
      jf(nr)=102                                                                !/*ep4c       28*/
!                                                                               !/*ep         60*/
!        set ih if any type 6 tally needs heating numbers in table iex.         !/*ep         61*/
   98 continue                                                                  !/*epx         8*/
#ifdef MESHTAL                                                                  /*!epx         9*/
      if(mtlflg.ne.0.or.medflg.ne.0)go to 123                                   !/*epx        10*/
#endif                                                                          /*!epx        11*/
      do 120 it=1,ntal                                                          !/*epx        12*/
      if(jptal(ljpt+2,it).ne.6.or.ktp(lktp+1,it).eq.0)go to 120                 !/*ep         63*/
      do 110 l=1,iptal(lipt+1,3,it)                                             !/*ep         64*/
      ip=itds(iptal(lipt+1,1,it)+l)                                             !/*ep         65*/
      do 100 k=1,itds(ip)                                                       !/*ep         66*/
      i=itds(ip+k)                                                              !/*ep4a        1*/
      if(junf.eq.0)go to 85                                                     !/*ep4a        2*/
      if(mfl(lmfl+1,i).ne.0)go to 123                                           !/*ep4a        3*/
   85 j=mat(lmat+i)                                                             !/*ep4a        4*/
      if(j.eq.0)go to 100                                                       !/*ep4a        5*/
      do 90 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                       !/*ep         68*/
   90 if(lme(llme+1,m).eq.iex)go to 123                                         !/*ep4a        6*/
  100 continue                                                                  !/*ep         70*/
  110 continue                                                                  !/*ep         71*/
  120 continue                                                                  !/*ep         72*/
      go to 125                                                                 !/*ep4a        7*/
  123 ih=1                                                                      !/*ep4a        8*/
!                                                                               !/*ep         73*/
!        set up initial values of temporary pointers and counters.              !/*ep         74*/
  125 do 130 i=2,32                                                             !/*epx        13*/
  130 jx(i)=0                                                                   !/*ep         76*/
      do 140 i=3,8                                                              !/*ep4c       31*/
  140 nx(i)=nxs(lnxs+i,iex)                                                     !/*ep         78*/
      do 145 i=1,mixs                                                           !/*epx        14*/
      do 145 j=1,nx(7)                                                          !/*epx        15*/
  145 ix(i,j)=ixs(lixs+i,j,iex)                                                 !/*epx        16*/
      lw=jxs(ljxs+1,iex)                                                        !/*ep         79*/
!                                                                               !/*ep         80*/
!        find minimum and maximum energy mesh indexes.                          !/*ep         81*/
      em=emx(1)+1.e-12*emx(1)                                                   !/*ep4a       10*/
      ec=ecf(1)-1.e-12*ecf(1)                                                   !/*ep4a       11*/
      if(nty(lnty+iex).eq.3)go to 200                                           !/*ep         84*/
      if(xss(lw).ge.xss(lw+1))call erprnt(1,2,0,0,0,0,0,0,                      !/*ep4b       28*/
     & '45h'//ht//' lowest energy grid point adjusted.')                        !/*ep4b       29*/
      if(xss(lw).ge.xss(lw+1))xss(lw)=xss(lw+1)-1.e-5*xss(lw+1)                 !/*ep4b       30*/
      j1=1                                                                      !/*ep         85*/
      j2=1                                                                      !/*ep         86*/
      j3=0                                                                      !/*ep4b       31*/
      el=-1.                                                                    !/*ep4b       32*/
      do 150 i=1,nxs(lnxs+3,iex)                                                !/*ep         87*/
      if(xss(i+lw-1).le.ec)j1=i                                                 !/*ep         88*/
      if(xss(i+lw-1).lt.em)j2=min(nxs(lnxs+3,iex),i+1)                          !/*ep4b       33*/
      if(xss(i+lw-1).le.el)j3=j3+1                                              !/*ep4b       34*/
  150 el=xss(i+lw-1)                                                            !/*ep4b       35*/
      nx(3)=j2-j1+1                                                             !/*ep         90*/
      if(nx(3).le.1)call expire(0,'expung',                                     !/*ep         91*/
     & ht//' is completely wiped out.')                                         !/*ep         92*/
      if(j3.ne.0)call erprnt(1,2,1,j3,0,0,0,0,                                  !/*ep4b       36*/
     & 'i7,44h coincident energy grid points in '//ht)                          !/*ep4b       37*/
!                                                                               !/*ep4b       38*/
!        set heating flag, nhtfl, for xs plotting.                              !/*ep4b       39*/
!        ih=1 means the heating numbers have not been expunged.                 !/*ep4b       40*/
      nhtfl(lnht+iex)=ih                                                        !/*ep4b       41*/
!                                                                               !/*ep         93*/
!        expunge energies and the four simple cross-section tables.             !/*ep         94*/
      nn=4+ih                                                                   !/*ep         95*/
      if(nx(3).eq.nxs(lnxs+3,iex).and.lc.ne.0)go to 190                         !/*ep         96*/
      if(lc.ne.0.or.ih.ne.0)go to 170                                           !/*ep         97*/
!                                                                               !/*ep         98*/
!        to protect wtmult and acetot, always keep total and absorption.        !/*ep         99*/
      nn=3                                                                      !/*ep        100*/
      do 160 i=1,nr                                                             !/*ep        101*/
  160 if(jf(i).eq.2)nn=4                                                        !/*ep        102*/
  170 do 180 n=1,nn                                                             !/*ep        103*/
      do 180 i=1,nx(3)                                                          !/*ep        104*/
  180 xss(lw+(n-1)*nx(3)+i-1)=xss(lw+(n-1)*nxs(lnxs+3,iex)+j1+i-2)              !/*ep        105*/
  190 lw=lw+nn*nx(3)                                                            !/*ep        106*/
!                                                                               !/*ep        107*/
!        set m2 array parallel to mtr with zeros for unneeded reactions.        !/*ep        108*/
      jd=j1                                                                     !/*ep        109*/
      if(nty(lnty+iex).eq.2)jd=j1-1                                             !/*ep        110*/
  200 n3=max(nxs(lnxs+4,iex),nxs(lnxs+5,iex),nxs(lnxs+6,iex))                   !/*ep4b       42*/
      if(n3.gt.m3)call erprnt(1,1,1,n3,0,0,0,0,                                 !/*ep4b       43*/
     & '44hdimension overflow in expung. increase m3 to,i5')                    !/*ep4b       44*/
      do 260 n=1,nxs(lnxs+4,iex)                                                !/*ep4b       45*/
      m2(n)=nint(xss(jxs(ljxs+3,iex)+n-1))                                      !/*ep4c       32*/
!        check energy indicies in sig for intersection with (j1,j2).            !/*ep        113*/
      if(nty(lnty+iex).eq.3)go to 210                                           !/*ep        114*/
      j=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+n-1))-1                        !/*ep4c       33*/
      if(j2.le.nint(xss(j)).or.jd.ge.nint(xss(j))+nint(xss(j+1))-1)             !/*ep4c       34*/
     & go to 250                                                                !/*ep4c       35*/
!        keep reaction if it is needed for transport.                           !/*ep        117*/
  210 if(n.le.nxs(lnxs+5,iex).and.lc.ne.0)go to 260                             !/*ep        118*/
!        keep reaction if it is needed for gamma-yield multiplier.              !/*ep        119*/
      if(mg.eq.0.or.jxs(ljxs+20,iex).eq.0)go to 230                             !/*ep        120*/
      do 220 i=1,nint(xss(jxs(ljxs+20,iex)))                                    !/*ep4c       36*/
  220 if(nint(xss(jxs(ljxs+20,iex)+i)).eq.m2(n))go to 260                       !/*ep4c       37*/
!                                                                               !/*epx        17*/
!         keep neutron reaction if needed as particle-yield multiplier          !/*epx        18*/
  230 do 221 i=1,nx(7)                                                          !/*epx        19*/
      do 223 jp=1,mipt                                                          !/*epx        20*/
  223 if(kpt(jp).ne.0.and.jp.eq.nint(xss(jxs(ljxs+30,iex)+i-1)))                !/*epx        21*/
     & go to 224                                                                !/*epx        22*/
      go to 221                                                                 !/*epx        23*/
  224 do 222 ii=1,nint(xss(ixs(lixs+10,i,iex)))                                 !/*epx        24*/
  222 if(nint(xss(ixs(lixs+10,i,iex)+ii)).eq.m2(n)) go to 260                   !/*epx        25*/
  221 continue                                                                  !/*epx        26*/
!                                                                               !/*epx        27*/
!         keep reaction if it is needed for any tally.                          !/*epx        28*/
      do 240 i=1,nr                                                             !/*epx        29*/
  240 if(m2(n).eq.jf(i).or.jf(i).eq.-6.and.                                     !/*ep        125*/
     & (m2(n).ge.18.and.m2(n).le.21.or.m2(n).eq.38))go to 260                   !/*ep        126*/
!        delete unneeded reaction.                                              !/*ep        127*/
  250 if(n.le.nxs(lnxs+5,iex))nx(5)=nx(5)-1                                     !/*ep        128*/
      nx(4)=nx(4)-1                                                             !/*ep        129*/
      m2(n)=0                                                                   !/*ep        130*/
  260 continue                                                                  !/*ep        131*/
!                                                                               !/*ep        132*/
!        check that all reactions needed for fm cards exist.                    !/*ep        133*/
      do 280 i=1,nr                                                             !/*ep        134*/
      if((jf(i).lt.3.or.jf(i).gt.1000).and.nty(lnty+iex).ne.3)go to 280         !/*ep        135*/
      do 270 l=1,nxs(lnxs+4,iex)                                                !/*ep        136*/
  270 if(m2(l).eq.jf(i))go to 280                                               !/*ep        137*/
      call erprnt(1,2,1,jf(i),0,0,0,0,                                          !/*ep        138*/
     & '16hfm/pert card rxn,i4,27h is missing from '//ht)                       !/*ep4b       46*/
  280 continue                                                                  !/*ep        140*/
!                                                                               !/*ep        141*/
!        select prompt or total type 19 fission nu data, if any.                !/*ep        142*/
      n=jxs(ljxs+3,iex)-jxs(ljxs+2,iex)                                         !/*ep        143*/
      if(jxs(ljxs+2,iex).eq.0.or.n.le.0)go to 310                               !/*ep        144*/
      jx(2)=lw                                                                  !/*ep        145*/
      lr=jxs(ljxs+2,iex)                                                        !/*ep        146*/
      if(nint(xss(lr)).gt.0)call erprnt(1,2,0,0,0,0,0,0,                        !/*ep4c       38*/
     & '50hnubar of '//ht//' may be either prompt or total.')                   !/*ep        148*/
      if(nint(xss(lr)).gt.0)go to 290                                           !/*ep4c       39*/
      n=-nint(xss(lr))                                                          !/*ep4c       40*/
      lr=lr+1                                                                   !/*ep        151*/
      hn='prompt nu'                                                            !/*ep        152*/
      if(itotnu.ne.1)go to 290                                                  !/*ep        153*/
      lr=lr+n                                                                   !/*ep        154*/
      n=jxs(ljxs+3,iex)-jxs(ljxs+2,iex)-n-1                                     !/*ep        155*/
      hn='total nu'                                                             !/*ep        156*/
  290 do 300 i=1,n                                                              !/*ep        157*/
  300 xss(lw+i-1)=xss(lr+i-1)                                                   !/*ep        158*/
      lw=lw+n                                                                   !/*ep        159*/
!                                                                               !/*ep        160*/
!        expunge the mtr, lqr, tyr, and lsig arrays.                            !/*ep        161*/
  310 if(nx(4).le.0)go to 380                                                   !/*ep        162*/
      do 330 i=3,6                                                              !/*ep        163*/
      if(nty(lnty+iex).eq.3.and.(i.eq.4.or.i.eq.5))go to 330                    !/*ep        164*/
      jx(i)=lw                                                                  !/*ep        165*/
      do 320 j=1,nxs(lnxs+4,iex)                                                !/*ep        166*/
      if(m2(j).eq.0)go to 320                                                   !/*ep        167*/
      xss(lw)=xss(jxs(ljxs+i,iex)+j-1)                                          !/*ep        168*/
      lw=lw+1                                                                   !/*ep        169*/
  320 continue                                                                  !/*ep        170*/
  330 continue                                                                  !/*ep        171*/
!                                                                               !/*ep        172*/
!        expunge the sig array and redefine the lsig array.                     !/*ep        173*/
      jx(7)=lw                                                                  !/*ep        174*/
      iw=jx(6)-1                                                                !/*ep        175*/
      do 370 j=1,nx(4)                                                          !/*ep        176*/
      ib=jxs(ljxs+7,iex)+nint(xss(iw+j))-1                                      !/*ep4c       41*/
      xss(iw+j)=lw-jx(7)+1                                                      !/*ep        178*/
      if(nty(lnty+iex).eq.3)go to 350                                           !/*ep        179*/
      if(nint(xss(jx(3)+j-1)).eq.18)jx(21)=lw                                   !/*ep4c       42*/
      jb=nint(xss(ib))+nint(xss(ib+1))-1                                        !/*ep4c       43*/
!        ic=min index to save relative to old energy table.                     !/*ep        182*/
      ic=max(nint(xss(ib)),j1)                                                  !/*ep4c       44*/
      lr=ic-nint(xss(ib))+ib                                                    !/*ep4c       45*/
      xss(lw)=ic-j1+1                                                           !/*ep        185*/
      xss(lw+1)=min(jb,j2)-ic+1                                                 !/*ep        186*/
      do 340 i=1,nint(xss(lw+1))                                                !/*ep4c       46*/
  340 xss(lw+i+1)=xss(lr+i+1)                                                   !/*ep        188*/
      lw=lw+nint(xss(lw+1))+2                                                   !/*ep4c       47*/
      go to 370                                                                 !/*ep        190*/
  350 n=2*(nint(xss(ib))+nint(xss(ib+2*nint(xss(ib))+1))+1)                     !/*ep4c       48*/
      do 360 i=1,n                                                              !/*ep        192*/
  360 xss(lw+i-1)=xss(ib+i-1)                                                   !/*ep        193*/
      lw=lw+n                                                                   !/*ep        194*/
  370 continue                                                                  !/*ep        195*/
!                                                                               !/*ep        196*/
!        expunge the land array.                                                !/*ep        197*/
  380 if(lc.eq.0)go to 480                                                      !/*ep        198*/
      jx(8)=lw                                                                  !/*ep        199*/
!        locator for elastic collision cosines                                  !/*ep        200*/
      xss(lw)=xss(jxs(ljxs+8,iex))                                              !/*ep        201*/
      lw=lw+1                                                                   !/*ep        202*/
      do 390 i=1,nxs(lnxs+5,iex)                                                !/*ep        203*/
      if(m2(i).eq.0)go to 390                                                   !/*ep        204*/
      xss(lw)=xss(jxs(ljxs+8,iex)+i)                                            !/*ep        205*/
      lw=lw+1                                                                   !/*ep        206*/
  390 continue                                                                  !/*ep        207*/
!                                                                               !/*ep        208*/
!        expunge the and array and redefine the land array.                     !/*ep        209*/
      jx(9)=lw                                                                  !/*ep        210*/
      do 450 j=1,nx(5)+1                                                        !/*ep        211*/
      i=jx(8)+j-1                                                               !/*ep        212*/
!        allow for negative values in land array.                               !/*ep4a       12*/
      if(nint(xss(i)).le.0)go to 450                                            !/*ep4c       49*/
      iw=jxs(ljxs+9,iex)+nint(xss(i))-1                                         !/*ep4c       50*/
      xss(i)=lw-jx(9)+1                                                         !/*ep        215*/
      m=nint(xss(iw))                                                           !/*ep4c       51*/
!        find minimum and maximum energy mesh indexes.                          !/*ep        217*/
      i1=1                                                                      !/*ep        218*/
      i2=1                                                                      !/*ep        219*/
      do 400 i=1,m                                                              !/*ep        220*/
      if(xss(iw+i).le.ec)i1=i                                                   !/*ep        221*/
  400 if(xss(iw+i).lt.em)i2=min(i+1,m)                                          !/*ep        222*/
!        expunge the energy and lmu arrays.                                     !/*ep        223*/
      nd=i2-i1+1                                                                !/*ep        224*/
      xss(lw)=nd                                                                !/*ep        225*/
!        ib points at the word preceeding lmu array in its new location.        !/*ep        226*/
      ib=lw+nd                                                                  !/*ep        227*/
      lw=lw+1                                                                   !/*ep        228*/
      do 410 i=1,nd                                                             !/*ep        229*/
  410 xss(lw+i-1)=xss(iw+i1+i-1)                                                !/*ep        230*/
      do 420 i=1,nd                                                             !/*ep        231*/
  420 xss(lw+nd+i-1)=xss(iw+i1+m+i-1)                                           !/*ep        232*/
      lw=lw+2*nd                                                                !/*ep        233*/
!        expunge the cosine arrays and redefine the lmu array.                  !/*ep        234*/
      do 440 i=1,nd                                                             !/*ep        235*/
      ic=nint(xss(ib+i))                                                        !/*ep4c       52*/
      if(ic.eq.0)go to 440                                                      !/*ep        237*/
!        ic>0 equiprobable bins; ic<0 tabulated function.                       !/*ep4c       53*/
      if(ic.lt.0)go to 432                                                      !/*ep4c       54*/
      xss(ib+i)=lw-jx(9)+1                                                      !/*ep        238*/
      do 430 k=1,33                                                             !/*ep        239*/
  430 xss(lw+k-1)=xss(jxs(ljxs+9,iex)+ic+k-2)                                   !/*ep        240*/
      lw=lw+33                                                                  !/*ep        241*/
      go to 440                                                                 !/*ep4c       55*/
 432  xss(ib+i)=-(lw-jx(9)+1)                                                   !/*ep4c       56*/
      lk=jxs(ljxs+9,iex)-1-ic                                                   !/*ep4c       57*/
      np=nint(xss(lk+1))                                                        !/*ep4c       58*/
      nk=np*3+2                                                                 !/*ep4c       59*/
      do 434 k=1,nk                                                             !/*ep4c       60*/
  434 xss(lw+k-1)=xss(lk+k-1)                                                   !/*ep4c       61*/
      lw=lw+nk                                                                  !/*ep4c       62*/
  440 continue                                                                  !/*ep        242*/
  450 continue                                                                  !/*ep        243*/
!                                                                               !/*ep        244*/
!        expunge ldlw and move dlw.                                             !/*ep        245*/
      if(nx(5).eq.0)go to 480                                                   !/*ep        246*/
      jx(10)=lw                                                                 !/*ep        247*/
      iw=0                                                                      !/*ep        248*/
      do 460 i=1,nxs(lnxs+5,iex)                                                !/*ep        249*/
      if(m2(i).eq.0)go to 460                                                   !/*ep        250*/
      xss(lw)=xss(jxs(ljxs+10,iex)+i-1)                                         !/*ep        251*/
      if(m2(i).eq.18.and.jxs(ljxs+2,iex).ge.jxs(ljxs+3,iex))                    !/*ep4c       63*/
     & iw=nint(xss(lw))                                                         !/*ep4c       64*/
      lw=lw+1                                                                   !/*ep        253*/
  460 continue                                                                  !/*ep        254*/
      jx(11)=lw                                                                 !/*ep        255*/
      l=jxs(ljxs+22,iex)-jxs(ljxs+11,iex)+1                                     !/*ep        256*/
!                                                                               !/*epx        30*/
!        the order of the data must be 23, 24, 12, 13, where:                   !/*epx        31*/
!           12 = photon production data                                         !/*epx        32*/
!           13 = photon production reaction types (mt numbers)                  !/*epx        33*/
!           23 = probability table data                                         !/*epx        34*/
!           24 = delayed neutron data                                           !/*epx        35*/
!        out of order data can cause overwriting if all features on.            !/*epx        36*/
      if(jxs(ljxs+13,iex).ne.0)                                                 !/*epx        37*/
     & l=min(l,jxs(ljxs+13,iex)-jxs(ljxs+11,iex))                               !/*epx        38*/
      if(jxs(ljxs+12,iex).ne.0)                                                 !/*epx        39*/
     & l=min(l,jxs(ljxs+12,iex)-jxs(ljxs+11,iex))                               !/*epx        40*/
      if(jxs(ljxs+24,iex).ne.0)                                                 !/*epx        41*/
     & l=min(l,jxs(ljxs+24,iex)-jxs(ljxs+11,iex))                               !/*epx        42*/
      if(jxs(ljxs+23,iex).ne.0)                                                 !/*epx        43*/
     & l=min(l,jxs(ljxs+23,iex)-jxs(ljxs+11,iex))                               !/*epx        44*/
      do 470 i=0,l-1                                                            !/*epx        45*/
  470 xss(lw+i)=xss(jxs(ljxs+11,iex)+i)                                         !/*epx        46*/
      if(iw.ne.0)jx(2)=lw+nint(xss(iw+1+lw))-1                                  !/*ep4c       67*/
      lw=lw+l                                                                   !/*ep        262*/
!                                                                               !/*ep4c       68*/
!        expunge unresolved probability tables.                                 !/*ep4c       69*/
!        l=jxs(ljxs+23,iex) points to location for unres prob tables;           !/*ep4c       70*/
!            xss(l)  = number of incident energies                              !/*ep4c       71*/
!            xss(l+1)= length of a table (usually 20)                           !/*ep4c       72*/
!            xss(l+2)= incident energy interpolation parameter (2 or 5)         !/*ep4c       73*/
!            xss(l+3)= flag for inelastic (-1 none, 0 by balance,>0=mt)         !/*ep4c       74*/
!            xss(l+4)= flag for "other absorption" (-1 none, etc.)              !/*ep4c       75*/
!            xss(l+5)= flag for factors, if 0, regular cross sections           !/*ep4c       76*/
!            followed by incident energies                                      !/*ep4c       77*/
!            followed by tables for each incident energy (6 columns)            !/*ep4c       78*/
!               cumulative probabilities                                        !/*ep4c       79*/
!               total cross sections                                            !/*ep4c       80*/
!               elastic cross sections                                          !/*ep4c       81*/
!               fission cross sections                                          !/*ep4c       82*/
!               capture cross sections (102)                                    !/*ep4c       83*/
!               neutron heating numbers                                         !/*ep4c       84*/
!        repeat for each incident energy.                                       !/*ep4c       85*/
  480 if(jxs(ljxs+23,iex).eq.0)go to 483                                        !/*ep4c       86*/
      if(iunr.eq.0)go to 483                                                    !/*ep4c       87*/
      if(jxs(ljxs+23,iex).lt.lw)call erprnt(1,1,0,0,0,0,0,0,                    !/*epx        47*/
     & '39hunresolved data overwritten. '//ht)                                  !/*epx        48*/
      l=jxs(ljxs+23,iex)                                                        !/*ep4c       88*/
      if(nint(xss(l+2)).ne.2.and.nint(xss(l+2)).ne.5)call erprnt(1,1,0,         !/*ep4c       89*/
     & 0,0,0,0,0,'41h error in unresolved table interpolation.')                !/*ep4c       90*/
      if(xss(l+6).lt.xunrl)xunrl=xss(l+6)                                       !/*ep4c       91*/
      if(xss(l+5+nint(xss(l))).gt.xunru)xunru=xss(l+5+nint(xss(l)))             !/*ep4c       92*/
      jx(23)=lw                                                                 !/*ep4c       93*/
      in=6+nint(xss(l))*(1+6*nint(xss(l+1)))                                    !/*ep4c       94*/
      do 482 i=0,in-1                                                           !/*ep4c       95*/
  482 xss(lw+i)=xss(l+i)                                                        !/*ep4c       96*/
      lw=lw+in                                                                  !/*ep4c       97*/
!                                                                               !/*ep4c       98*/
!        expunge delayed neutron data.                                          !/*ep4c       99*/
  483 if(itotnu.ne.1.or.dnb.eq.0.)nx(8)=0                                       !/*ep4c2       1*/
      if(nx(8).eq.0)go to 488                                                   !/*ep4c      101*/
      if(jxs(ljxs+24,iex).lt.lw)call erprnt(1,1,0,0,0,0,0,0,                    !/*epx        49*/
     & '44hdelayed neutron data overwritten. '//ht)                             !/*epx        50*/
      jx(24)=lw                                                                 !/*ep4c      102*/
      do 484 i=1,jxs(ljxs+25,iex)-jxs(ljxs+24,iex)                              !/*ep4c      103*/
  484 xss(lw+i-1)=xss(jxs(ljxs+24,iex)+i-1)                                     !/*ep4c      104*/
      lw=lw+jxs(ljxs+25,iex)-jxs(ljxs+24,iex)                                   !/*ep4c      105*/
      jx(25)=lw                                                                 !/*ep4c      106*/
      do 485 i=1,jxs(ljxs+26,iex)-jxs(ljxs+25,iex)                              !/*ep4c      107*/
  485 xss(lw+i-1)=xss(jxs(ljxs+25,iex)+i-1)                                     !/*ep4c      108*/
      lw=lw+jxs(ljxs+26,iex)-jxs(ljxs+25,iex)                                   !/*ep4c      109*/
      jx(26)=lw                                                                 !/*ep4c      110*/
      do 486 i=1,jxs(ljxs+27,iex)-jxs(ljxs+26,iex)                              !/*ep4c      111*/
  486 xss(lw+i-1)=xss(jxs(ljxs+26,iex)+i-1)                                     !/*ep4c      112*/
      lw=lw+jxs(ljxs+27,iex)-jxs(ljxs+26,iex)                                   !/*ep4c      113*/
      jx(27)=lw                                                                 !/*ep4c      114*/
      mo=jxs(ljxs+22,iex)-jxs(ljxs+27,iex)+1                                    !/*ep4c      115*/
      if(jxs(ljxs+13,iex).ne.0)mo=jxs(ljxs+13,iex)-jxs(ljxs+27,iex)             !/*ep4c      116*/
      if(jxs(ljxs+12,iex).ne.0)mo=jxs(ljxs+12,iex)-jxs(ljxs+27,iex)             !/*ep4c      117*/
      do 487 i=1,mo                                                             !/*ep4c      118*/
  487  xss(lw+i-1)=xss(jxs(ljxs+27,iex)+i-1)                                    !/*ep4c      119*/
       lw=lw+mo                                                                 !/*ep4c      120*/
!                                                                               !/*ep        263*/
!        expunge gamma production cross sections and move energy table.         !/*ep        264*/
  488 if(mg.eq.0.or.jxs(ljxs+12,iex).eq.0)go to 740                             !/*ep4c      121*/
      if(jxs(ljxs+12,iex).lt.lw)call erprnt(1,1,0,0,0,0,0,0,                    !/*epx        51*/
     & '45hgamma production data overwritten. '//ht)                            !/*epx        52*/
      jx(12)=lw                                                                 !/*ep        266*/
      do 490 i=1,nx(3)                                                          !/*ep        267*/
  490 xss(lw+i-1)=xss(jxs(ljxs+12,iex)+j1+i-2)                                  !/*ep        268*/
      lw=lw+nx(3)                                                               !/*ep        269*/
      if(jxs(ljxs+13,iex).ne.0)go to 510                                        !/*ep        270*/
      do 500 i=1,600                                                            !/*ep        271*/
  500 xss(lw+i-1)=xss(jxs(ljxs+12,iex)+nxs(lnxs+3,iex)+i-1)                     !/*ep        272*/
      lw=lw+600                                                                 !/*ep        273*/
      go to 740                                                                 !/*ep        274*/
!                                                                               !/*ep        275*/
!        expunge the partial photon-production information if it exists.        !/*ep        276*/
  510 do 540 i=1,nxs(lnxs+6,iex)                                                !/*ep        277*/
      m2(i)=nint(xss(jxs(ljxs+13,iex)+i-1))                                     !/*ep4c      122*/
      j=jxs(ljxs+15,iex)+nint(xss(jxs(ljxs+14,iex)+i-1))-1                      !/*ep4c      123*/
      if(nint(xss(j)).eq.12.or.nint(xss(j)).eq.16)go to 520                     !/*ep4c      124*/
      if(j2.gt.nint(xss(j+1)).and.jd.lt.nint(xss(j+1))+nint(xss(j+2))-1)        !/*ep4c      125*/
     & go to 540                                                                !/*ep4c      126*/
      go to 530                                                                 !/*ep        282*/
  520 j=j+2*nint(xss(j+2))+3                                                    !/*ep4c      127*/
      if(xss(j+1).lt.em.and.xss(j+nint(xss(j))).gt.ec)go to 540                 !/*ep4c      128*/
  530 nx(6)=nx(6)-1                                                             !/*ep        285*/
      m2(i)=0                                                                   !/*ep        286*/
  540 continue                                                                  !/*ep        287*/
!                                                                               !/*ep        288*/
!        check that all phot-prod reactions needed for fm cards exist.          !/*ep        289*/
      do 560 i=1,nr                                                             !/*ep        290*/
      if(jf(i).le.1000)go to 560                                                !/*ep        291*/
      do 550 l=1,nxs(lnxs+6,iex)                                                !/*ep        292*/
  550 if(m2(l).eq.jf(i))go to 560                                               !/*ep        293*/
      call erprnt(1,2,1,jf(i),0,0,0,0,                                          !/*ep        294*/
     & '16hfm/pert card rxn,i8,27h is missing from '//ht)                       !/*ep4b       48*/
  560 continue                                                                  !/*ep        296*/
!                                                                               !/*ep        297*/
!        expunge the mtrp and lsigp arrays.                                     !/*ep        298*/
      do 580 i=13,14                                                            !/*ep        299*/
      jx(i)=lw                                                                  !/*ep        300*/
      do 570 n=1,nxs(lnxs+6,iex)                                                !/*ep        301*/
      if(m2(n).eq.0)go to 570                                                   !/*ep        302*/
      xss(lw)=xss(jxs(ljxs+i,iex)+n-1)                                          !/*ep        303*/
      lw=lw+1                                                                   !/*ep        304*/
  570 continue                                                                  !/*ep        305*/
  580 continue                                                                  !/*ep        306*/
!                                                                               !/*ep        307*/
!        expunge sigp array and redefine lsigp array.                           !/*ep        308*/
      jx(15)=lw                                                                 !/*ep        309*/
      do 640 j=1,nx(6)                                                          !/*ep        310*/
      ib=jxs(ljxs+15,iex)+nint(xss(jx(14)+j-1))-1                               !/*ep4c      129*/
      xss(jx(14)+j-1)=lw-jx(15)+1                                               !/*ep        312*/
      xss(lw)=xss(ib)                                                           !/*ep        313*/
      lw=lw+1                                                                   !/*ep        314*/
      ib=ib+1                                                                   !/*ep        315*/
      if(nint(xss(ib-1)).eq.12.or.nint(xss(ib-1)).eq.16)go to 600               !/*ep4c      130*/
      jb=nint(xss(ib))+nint(xss(ib+1))-1                                        !/*ep4c      131*/
      ic=max(nint(xss(ib)),j1)                                                  !/*ep4c      132*/
      lr=ic-nint(xss(ib))+ib+2                                                  !/*ep4c      133*/
      xss(lw)=ic-j1+1                                                           !/*ep        320*/
      xss(lw+1)=min(jb,j2)-ic+1                                                 !/*ep        321*/
      do 590 i=1,nint(xss(lw+1))                                                !/*ep4c      134*/
  590 xss(lw+i+1)=xss(lr+i-1)                                                   !/*ep        323*/
      lw=lw+nint(xss(lw+1))+2                                                   !/*ep4c      135*/
      go to 640                                                                 !/*ep        325*/
!        translate neutron mt number to neutron mt index.                       !/*ep        326*/
  600 mt=nint(xss(ib))                                                          !/*ep4c      136*/
      i=-18                                                                     !/*ep        328*/
      if(mt.eq.18)go to 620                                                     !/*ep        329*/
      do 610 i=1,nx(4)                                                          !/*ep        330*/
  610 if(nint(xss(jx(3)+i-1)).eq.mt)go to 620                                   !/*ep4c      137*/
  620 xss(ib)=i                                                                 !/*ep        332*/
      n=2*nint(xss(ib+1))+2*nint(xss(ib+2+2*nint(xss(ib+1))))+3                 !/*ep4c      138*/
      do 630 i=1,n                                                              !/*ep        334*/
  630 xss(lw+i-1)=xss(ib+i-1)                                                   !/*ep        335*/
      lw=lw+n                                                                   !/*ep        336*/
  640 continue                                                                  !/*ep        337*/
!                                                                               !/*ep        338*/
!        expunge the landp array.                                               !/*ep        339*/
      jx(16)=lw                                                                 !/*ep        340*/
      do 650 i=1,nxs(lnxs+6,iex)                                                !/*ep        341*/
      if(m2(i).eq.0)go to 650                                                   !/*ep        342*/
      xss(lw)=xss(jxs(ljxs+16,iex)+i-1)                                         !/*ep        343*/
      lw=lw+1                                                                   !/*ep        344*/
  650 continue                                                                  !/*ep        345*/
!                                                                               !/*ep        346*/
!        expunge andp array and redefine landp array.                           !/*ep        347*/
      jx(17)=lw                                                                 !/*ep        348*/
      do 710 j=1,nx(6)                                                          !/*ep        349*/
      i=jx(16)+j-1                                                              !/*ep        350*/
      if(nint(xss(i)).eq.0)go to 710                                            !/*ep4c      139*/
      iw=jxs(ljxs+17,iex)+nint(xss(i))-1                                        !/*ep4c      140*/
      xss(i)=lw-jx(17)+1                                                        !/*ep        353*/
      m=nint(xss(iw))                                                           !/*ep4c      141*/
      i1=1                                                                      !/*ep        355*/
      i2=1                                                                      !/*ep        356*/
      do 660 i=1,m                                                              !/*ep        357*/
      if(xss(iw+i).le.ec)i1=i                                                   !/*ep        358*/
  660 if(xss(iw+i).lt.em)i2=min(i+1,m)                                          !/*ep        359*/
      nd=i2-i1+1                                                                !/*ep        360*/
      xss(lw)=nd                                                                !/*ep        361*/
      ib=lw+nd                                                                  !/*ep        362*/
      lw=lw+1                                                                   !/*ep        363*/
      do 670 i=1,nd                                                             !/*ep        364*/
  670 xss(lw+i-1)=xss(iw+i1+i-1)                                                !/*ep        365*/
      do 680 i=1,nd                                                             !/*ep        366*/
  680 xss(lw+nd+i-1)=xss(iw+i1+m+i-1)                                           !/*ep        367*/
      lw=lw+2*nd                                                                !/*ep        368*/
      do 700 i=1,nd                                                             !/*ep        369*/
      ic=nint(xss(ib+i))                                                        !/*ep4c      142*/
      if(ic.eq.0)go to 700                                                      !/*ep        371*/
      if(ic.lt.0)go to 692                                                      !/*ep4c      143*/
      xss(ib+i)=lw-jx(17)+1                                                     !/*ep        372*/
      do 690 k=1,33                                                             !/*ep        373*/
  690 xss(lw+k-1)=xss(jxs(ljxs+17,iex)+ic+k-2)                                  !/*ep        374*/
      lw=lw+33                                                                  !/*ep        375*/
      go to 700                                                                 !/*ep4c      144*/
  692 xss(ib+i)=-(lw-jx(17)+1)                                                  !/*ep4c      145*/
      lk=jxs(ljxs+17,iex)-1-ic                                                  !/*ep4c      146*/
      np=nint(xss(lk+1))                                                        !/*ep4c      147*/
      nk=np*3+2                                                                 !/*ep4c      148*/
      do 694 k=1,nk                                                             !/*ep4c      149*/
  694 xss(lw+k-1)=xss(lk+k-1)                                                   !/*ep4c      150*/
      lw=lw+nk                                                                  !/*ep4c      151*/
  700 continue                                                                  !/*ep        376*/
  710 continue                                                                  !/*ep        377*/
      if(jx(17).eq.lw)jx(16)=0                                                  !/*ep        378*/
!                                                                               !/*ep        379*/
!        expunge ldlwp array and move dlwp array.                               !/*ep        380*/
      jx(18)=lw                                                                 !/*ep        381*/
      do 720 n=1,nxs(lnxs+6,iex)                                                !/*ep        382*/
      if(m2(n).eq.0)go to 720                                                   !/*ep        383*/
      xss(lw)=xss(jxs(ljxs+18,iex)+n-1)                                         !/*ep        384*/
      lw=lw+1                                                                   !/*ep        385*/
  720 continue                                                                  !/*ep        386*/
      jx(19)=lw                                                                 !/*ep        387*/
      ns=jxs(ljxs+20,iex)-jxs(ljxs+19,iex)                                      !/*ep4b       49*/
      if(ns.le.0)ns=jxs(ljxs+22,iex)-jxs(ljxs+19,iex)+1                         !/*ep4b       50*/
      do 730 i=1,ns                                                             !/*ep4b       51*/
  730 xss(lw+i-1)=xss(jxs(ljxs+19,iex)+i-1)                                     !/*ep4b       52*/
      lw=lw+ns                                                                  !/*ep4b       53*/
!                                                                               !/*ep        391*/
!       process pikmt parameters.                                               !/*ep        392*/
  740 if(npikmt.eq.0)go to 800                                                  !/*ep        393*/
      nxs(lnxs+15,iex)=-1                                                       !/*ep        394*/
      read(ht,'(e9.0)')tr                                                       !/*ep        395*/
      ip=lpik+1                                                                 !/*ep        396*/
  750 if(abs(tr-pik(ip)).le.1e-06.or.aint(tr).eq.pik(ip))go to 760              !/*ep        397*/
      ip=ip+2+2*max(0,int(pik(ip+1)))                                           !/*ep        398*/
      if(ip.lt.lpik+npikmt)go to 750                                            !/*ep        399*/
      go to 800                                                                 !/*ep        400*/
  760 np=pik(ip+1)                                                              !/*ep        401*/
      nxs(lnxs+15,iex)=np                                                       !/*ep        402*/
      if(np.lt.0)go to 800                                                      !/*ep4b       54*/
      if(jxs(ljxs+13,iex).eq.0)call erprnt(1,1,0,0,0,0,0,0,                     !/*ep        404*/
     & '34hpikmt does not work for '//ht)                                       !/*ep        405*/
      if(jxs(ljxs+13,iex).eq.0.or.np.eq.0)go to 800                             !/*ep4b       55*/
!        jxs(ljxs+29,iex) is used for pikmt in MCNPX; ljxs+32 in MCNP.          !/*epx        53*/
!        jxs(ljxs+32) is used for protons in MCNPX.                             !/*epx        54*/
      jxs(ljxs+29,iex)=lw                                                       !/*epx        55*/
      do 790 i=1,np                                                             !/*ep        408*/
      do 770 j=1,nxs(lnxs+6,iex)                                                !/*ep        409*/
  770 if(pik(ip+2*i).eq.xss(jx(13)+j-1))go to 780                               !/*ep        410*/
      call erprnt(1,1,1,int(pik(ip+2*i)),0,0,0,0,                               !/*ep        411*/
     & '3hmt ,i7,30h does not exist for '//ht)                                  !/*ep        412*/
  780 xss(lw+2*i-2)=j                                                           !/*ep        413*/
  790 xss(lw+2*i-1)=pik(ip+2*i+1)                                               !/*ep        414*/
      lw=lw+2*np                                                                !/*ep        415*/
!                                                                               !/*ep        416*/
!         expunge other particle data                                           !/*epx        56*/
  800 continue                                                                  !/*epx        57*/
      if(nxs(lnxs+7,iex).eq.0) go to 801                                        !/*epx        58*/
      do 802 i=1,nxs(lnxs+7,iex)                                                !/*epx        59*/
      m2(i)=nint(xss(jxs(ljxs+30,iex)+i-1))                                     !/*epx        60*/
!                                                                               !/*epx        61*/
!         make sure that energy grid overlaps (j1,j2)                           !/*epx        62*/
      j=ixs(lixs+1,i,iex)                                                       !/*epx        63*/
      if(j2.le.nint(xss(j)).or.j1.ge.nint(xss(j))+nint(xss(j+1))-1)             !/*epx        64*/
     & go to 799                                                                !/*epx        65*/
!                                                                               !/*epx        66*/
!         is this particle required for transport                               !/*epx        67*/
      do 803 j=1,mipt                                                           !/*epx        68*/
  803 if(kpt(j).ne.0.and.j.eq.m2(i)) go to 804                                  !/*epx        69*/
!                                                                               !/*epx        70*/
!         this particle not needed for transport --> expunge data               !/*epx        71*/
  799 if(dbcn(26).eq.0.)write(iuo,805) m2(i),ht                                 !/*epx        72*/
  805 format(34h particle-production data for ipt=,i3,                          !/*epx        73*/
     & 21h being expunged from ,a10)                                            !/*epx        74*/
      m2(i)=0                                                                   !/*epx        75*/
      nx(7)=nx(7)-1                                                             !/*epx        76*/
      go to 802                                                                 !/*epx        77*/
  804 write(iuo,806) m2(i),ht                                                   !/*epx        78*/
  806 format(34h particle-production data for ipt=,i3,                          !/*epx        79*/
     & 17h being used from ,a10)                                                !/*epx        80*/
  802 continue                                                                  !/*epx        81*/
!                                                                               !/*epx        82*/
!         are we missing data for desired particles                             !/*epx        83*/
  801 do 807 i=4,mipt                                                           !/*epx        84*/
      if(kpt(i).eq.0)go to 807                                                  !/*epx        85*/
      do 808 j=1,nxs(lnxs+7,iex)                                                !/*epx        86*/
  808 if(i.eq.m2(j)) go to 807                                                  !/*epx        87*/
      write(iuo,809) i,ht                                                       !/*epx        88*/
  809 format(37h no particle-production data for ipt=,i3,                       !/*epx        89*/
     & 6h from ,a10)                                                            !/*epx        90*/
  807 continue                                                                  !/*epx        91*/
!                                                                               !/*epx        92*/
!         expunge ptype, ntro blocks                                            !/*epx        93*/
      if(nx(7).eq.0) go to 811                                                  !/*epx        94*/
      do 812 i=30,31                                                            !/*epx        95*/
      jx(i)=lw                                                                  !/*epx        96*/
      do 813 j=1,nxs(lnxs+7,iex)                                                !/*epx        97*/
      if(m2(j).eq.0) go to 813                                                  !/*epx        98*/
      xss(lw)=xss(jxs(ljxs+i,iex)+j-1)                                          !/*epx        99*/
      lw=lw+1                                                                   !/*epx       100*/
  813 continue                                                                  !/*epx       101*/
  812 continue                                                                  !/*epx       102*/
      jx(32)=0                                                                  !/*epx       103*/
!                                                                               !/*epx       104*/
!         expunge or copy actual data blocks for secondary particles            !/*epx       105*/
      ik=0                                                                      !/*epx       106*/
      do 814 i=1,nxs(lnxs+7,iex)                                                !/*epx       107*/
      if(m2(i).eq.0) go to 814                                                  !/*epx       108*/
      ik=ik+1                                                                   !/*epx       109*/
      ix(1,ik)=lw                                                               !/*epx       110*/
!                                                                               !/*epx       111*/
!         expunge hpd block                                                     !/*epx       112*/
      ib=ixs(lixs+1,i,iex)                                                      !/*epx       113*/
      jb=nint(xss(ib))+nint(xss(ib+1))-1                                        !/*epx       114*/
      ic=max(nint(xss(ib)),j1)                                                  !/*epx       115*/
      lr=ic-nint(xss(ib))+ib                                                    !/*epx       116*/
      xss(lw)=ic-j1+1                                                           !/*epx       117*/
      xss(lw+1)=min(jb,j2)-ic+1                                                 !/*epx       118*/
      do 815 j=1,nint(xss(lw+1))                                                !/*epx       119*/
      if(ih.eq.1) xss(jxs(ljxs+1,iex)+4*nx(3)+nint(xss(lw))-2+j)=               !/*epx       120*/
     & xss(jxs(ljxs+1,iex)+4*nx(3)+nint(xss(lw))-2+j)-                          !/*epx       121*/
     & xss(lr+j+1+nint(xss(ib+1)))                                              !/*epx       122*/
  815 xss(lw+j+1)=xss(lr+j+1)                                                   !/*epx       123*/
      lw=lw+2+nint(xss(lw+1))                                                   !/*epx       124*/
!                                                                               !/*epx       125*/
!         decide whether to expunge reactions                                   !/*epx       126*/
      nr=nint(xss(jx(31)+ik-1))                                                 !/*epx       127*/
      if(nr.gt.m3)call erprnt(1,1,1,nr,0,0,0,0,                                 !/*epx       128*/
     & '44hdimension overflow in expung. increase m3 to,i5')                    !/*epx       129*/
      do 816 j=1,nr                                                             !/*epx       130*/
      ml(j)=nint(xss(ixs(lixs+2,i,iex)+j-1))                                    !/*epx       131*/
      do 817 k=1,nx(4)                                                          !/*epx       132*/
  817 if(ml(j).eq.nint(xss(jx(3)+k-1))) go to 816                               !/*epx       133*/
      ml(j)=0                                                                   !/*epx       134*/
      xss(jx(31)+ik-1)=xss(jx(31)+ik-1)-1                                       !/*epx       135*/
!                                                                               !/*epx       136*/
!         translate neutron mt number to neutron mt index.                      !/*epx       137*/
  816 if(ml(j).ne.0) xss(ixs(lixs+5,i,iex)+                                     !/*epx       138*/
     & nint(xss(ixs(lixs+4,i,iex)+j-1)))=k                                      !/*epx       139*/
!                                                                               !/*epx       140*/
!         expunge mtrh, tyrh, and lsigh blocks                                  !/*epx       141*/
      ix(2,ik)=lw                                                               !/*epx       142*/
      ix(3,ik)=lw+nint(xss(jx(31)+ik-1))                                        !/*epx       143*/
      ix(4,ik)=lw+2*nint(xss(jx(31)+ik-1))                                      !/*epx       144*/
      do 818 j=2,4                                                              !/*epx       145*/
      do 818 k=1,nr                                                             !/*epx       146*/
      if(ml(k).eq.0) go to 818                                                  !/*epx       147*/
      xss(lw)=xss(ixs(lixs+j,i,iex)+k-1)                                        !/*epx       148*/
      lw=lw+1                                                                   !/*epx       149*/
  818 continue                                                                  !/*epx       150*/
!                                                                               !/*epx       151*/
!         copy the sigh block                                                   !/*epx       152*/
      ix(5,ik)=lw                                                               !/*epx       153*/
      nw=ixs(lixs+6,i,iex)-ixs(lixs+5,i,iex)                                    !/*epx       154*/
      do 819 j=1,nw                                                             !/*epx       155*/
  819 xss(lw+j-1)=xss(ixs(lixs+5,i,iex)+j-1)                                    !/*epx       156*/
      lw=lw+nw                                                                  !/*epx       157*/
!                                                                               !/*epx       158*/
!         expunge the landh block                                               !/*epx       159*/
      ix(6,ik)=lw                                                               !/*epx       160*/
      do 821 k=1,nr                                                             !/*epx       161*/
      if(ml(k).eq.0) go to 821                                                  !/*epx       162*/
      xss(lw)=xss(ixs(lixs+6,i,iex)+k-1)                                        !/*epx       163*/
      lw=lw+1                                                                   !/*epx       164*/
  821 continue                                                                  !/*epx       165*/
!                                                                               !/*epx       166*/
!         copy the andh block                                                   !/*epx       167*/
      ix(7,ik)=lw                                                               !/*epx       168*/
      nw=ixs(lixs+8,i,iex)-ixs(lixs+7,i,iex)                                    !/*epx       169*/
      do 822 j=1,nw                                                             !/*epx       170*/
  822 xss(lw+j-1)=xss(ixs(lixs+7,i,iex)+j-1)                                    !/*epx       171*/
      lw=lw+nw                                                                  !/*epx       172*/
!                                                                               !/*epx       173*/
!         expunge the ldlwh block                                               !/*epx       174*/
      ix(8,ik)=lw                                                               !/*epx       175*/
      do 823 k=1,nr                                                             !/*epx       176*/
      if(ml(k).eq.0) go to 823                                                  !/*epx       177*/
      xss(lw)=xss(ixs(lixs+8,i,iex)+k-1)                                        !/*epx       178*/
      lw=lw+1                                                                   !/*epx       179*/
  823 continue                                                                  !/*epx       180*/
!                                                                               !/*epx       181*/
!         copy the dlwh block                                                   !/*epx       182*/
      ix(9,ik)=lw                                                               !/*epx       183*/
      nw=ixs(lixs+10,i,iex)-ixs(lixs+9,i,iex)                                   !/*epx       184*/
      do 824 j=1,nw                                                             !/*epx       185*/
  824 xss(lw+j-1)=xss(ixs(lixs+9,i,iex)+j-1)                                    !/*epx       186*/
      lw=lw+nw                                                                  !/*epx       187*/
!                                                                               !/*epx       188*/
!         don't need the yh block                                               !/*epx       189*/
      ix(10,ik)=0                                                               !/*epx       190*/
  814 continue                                                                  !/*epx       191*/
!                                                                               !/*epx       192*/
!        set up the modified pointers and counters.                             !/*ep        417*/
  811 do 810 i=2,27                                                             !/*epx       193*/
  810 jxs(ljxs+i,iex)=jx(i)                                                     !/*ep4c      153*/
      jxs(ljxs+22,iex)=lw-1                                                     !/*ep4c      154*/
      do 825 i=30,32                                                            !/*epx       194*/
  825 jxs(ljxs+i,iex)=jx(i)                                                     !/*epx       195*/
      do 826 i=1,mixs                                                           !/*epx       196*/
      do 826 j=1,nx(7)                                                          !/*epx       197*/
  826 ixs(lixs+i,j,iex)=ix(i,j)                                                 !/*epx       198*/
      nxs(lnxs+1,iex)=lw-jxs(ljxs+1,iex)                                        !/*ep        421*/
      do 820 i=3,8                                                              !/*ep4c      155*/
  820 nxs(lnxs+i,iex)=nx(i)                                                     !/*ep        423*/
!                                                                               !/*ep        424*/
!        integrate the part below energy cutoff of law 4 distributions.         !/*ep        425*/
      if(jxs(ljxs+13,iex).eq.0)return                                           !/*ep        426*/
      do 880 j=1,nxs(lnxs+6,iex)                                                !/*ep        427*/
      n=jxs(ljxs+18,iex)+j                                                      !/*ep        428*/
  830 n=jxs(ljxs+19,iex)+nint(xss(n-1))                                         !/*ep4c      156*/
      if(nint(xss(n)).ne.4)go to 870                                            !/*ep4c      157*/
      iw=jxs(ljxs+19,iex)+nint(xss(n+1))-1                                      !/*ep4c      158*/
      nr=iw+2*nint(xss(iw))+1                                                   !/*ep4c      159*/
      ng=nint(xss(nr))                                                          !/*ep4c      160*/
      do 860 ic=1,ng                                                            !/*ep        434*/
      lb=jxs(ljxs+19,iex)+nint(xss(nr+ng+ic))                                   !/*ep4c      161*/
      if(xss(lb+1).ge.ecf(2))go to 860                                          !/*ep        436*/
      jj=nint(xss(lb-1))                                                        !/*ep4c      162*/
!        for now, don't integrate if discrete lines are mixed in.               !/*ep4a       16*/
      if(jj.gt.2)go to 860                                                      !/*ep4a       17*/
      np=nint(xss(lb))                                                          !/*ep4c      163*/
      xss(lb-1)=9999                                                            !/*ep        439*/
      do 840 ns=lb+1,lb+np-1                                                    !/*ep        440*/
  840 if(xss(ns+1).gt.ecf(2))go to 850                                          !/*ep        441*/
      go to 860                                                                 !/*ep        442*/
  850 wc=1.-xss(ns+2*np)-(ecf(2)-xss(ns))*xss(np+ns)                            !/*ep        443*/
      if(jj.eq.2)wc=wc-.5*(ecf(2)-xss(ns))**2*                                  !/*ep        444*/
     & (xss(ns+np+1)-xss(ns+np))/(xss(ns+1)-xss(ns))                            !/*ep        445*/
      xss(lb)=.5*wc+np                                                          !/*ep        446*/
      if(xss(lb).gt.np)xss(lb-1)=10000*jj+ns-lb                                 !/*ep        447*/
  860 continue                                                                  !/*ep        448*/
  870 if(nint(xss(n-1)).ne.0)go to 830                                          !/*ep4c      164*/
  880 continue                                                                  !/*ep        450*/
      return                                                                    !/*ep        451*/
      end                                                                       !/*ep        452*/

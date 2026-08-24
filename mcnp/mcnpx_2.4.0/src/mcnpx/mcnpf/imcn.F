!#ident imcn.F from deck im                                                     !/*im          1*/
      subroutine imcn                                                           !/*im          2*/
!        initiation for monte carlo transport.                                  !/*im          3*/
      use allocate_memory                                                       !/*imx         1*/
      use global_data                                                           !/*imx         2*/
      use imcn_data                                                             !/*imx         3*/
!                                                                               !/*imx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*imx         5*/
      implicit integer(kindi) (i-n)                                             !/*imx         6*/
!                                                                               !/*imx         7*/
#ifdef MESHTAL                                                                  /*!imx         8*/
      dimension zd(10),jz(5)                                                    !/*imx         9*/
#endif                                                                          /*!imx        10*/
      character hs*8                                                            !/*im4a        1*/
      dimension eb(16)                                                          !/*im4b        1*/
      data eb/20.,15.,10.,9.,8.,7.,6.,5.,4.,3.,2.,1.,.5,.1,.01,0./              !/*im4b        2*/
!                                                                               !/*im          6*/
!        open scratch files.                                                    !/*im          7*/
      hovr='imcn'                                                               !/*im          8*/
      open(iu1,status='scratch')                                                !/*im          9*/
      open(iu2,status='scratch')                                                !/*im         10*/
      open(iu3,status='scratch',form='unformatted')                             !/*im         11*/
!                                                                               !/*im         12*/
      if(konrun.ne.0)go to 440                                                  !/*im         13*/
!                                                                               !/*im         14*/
!        read the rest of the inp file and set up dynamically                   !/*im         15*/
!        allocated storage.                                                     !/*im         16*/
      mtasks=max(1,ntasks)                                                      !/*im         17*/
      do 10 i=1,mink*mnk                                                        !/*im         18*/
   10 ink(i)=1                                                                  !/*im         19*/
      ink(160)=1                                                                !/*im4a        6*/
      ink(175)=1                                                                !/*im4a        7*/
      nmxf=3                                                                    !/*im4a        8*/
!     dbcn(6)=1.                                                                !/*imx        11*/
!     dbcn(28)=-3.                                                              !/*imx        12*/
!                                                                               !/*imx        13*/
!        set some energy-related default values.                                !/*imx        14*/
      emx(1)=huge                                                               !/*imx        15*/
      emx(2)=huge                                                               !/*imx        16*/
      do 15 i=1,mipt                                                            !/*imx        17*/
      in_erg(i)=8                                                               !/*imx        18*/
      ecf(i)=ecut_def(i)                                                        !/*imx        19*/
   15 continue                                                                  !/*imx        20*/
!                                                                               !/*imx        21*/
#ifdef MESHTAL                                                                  /*!imx        22*/
      nmesh=0                                                                   !/*imx        23*/
#endif                                                                          /*!imx        24*/
#ifdef SPABI                                                                    /*!imx        25*/
      call zero_spb_length                                                      !/*imx        26*/
#endif                                                                          /*!imx        27*/
      call pass1                                                                !/*im         20*/
!                                                                               !/*imx        28*/
      if(mct.lt.0)rewind iuo                                                    !/*im4a        9*/
      if(dbcn(26).eq.0.)call lex(iuo)                                           !/*imx        29*/
      if(mcal.ne.0)ides=1                                                       !/*im4b        3*/
!     if(kpt(1).ne.0.and.kpt(9)+kpt(31)+kpt(32)+kpt(33)+kpt(34).ne.0)           !/*nyxk        1*/
!    & rcoilf(1)=one                                                            !/*nyxk        2*/
!     if(kpt(9).ne.0.and.kpt(1)+kpt(31)+kpt(32)+kpt(33)+kpt(34).ne.0)           !/*nyxk        3*/
!    & rcoilf(2)=one                                                            !/*nyxk        4*/
      n=0                                                                       !/*im         21*/
      do 20 i=1,mipt                                                            !/*im         22*/
      if(kpt(i).ne.0)nipt=i                                                     !/*imx        30*/
      if(kpt(mipt+1-i).ne.0)nmip=mipt+1-i                                       !/*im4b        4*/
   20 n=n+kpt(i)                                                                !/*im         23*/
      if(n.eq.0)kpt(1)=1                                                        !/*im         24*/
      if(n.eq.0)nmip=1                                                          !/*im4b        5*/
      if(nipt.eq.0)nipt=1                                                       !/*imx        31*/
      if(nipt.eq.2.and.ides.eq.0)nipt=3                                         !/*imx        32*/
      n=0                                                                       !/*imx        33*/
      if(kpt(1).ne.0)n=n+1                                                      !/*imx        34*/
      if(kpt(2).ne.0)n=n+1                                                      !/*imx        35*/
      if(kpt(3).ne.0.or.kpt(2).ne.0.and.ides.eq.0)n=n+1                         !/*imx        36*/
      if(kpt(9).ne.0)n=n+1                                                      !/*imx        37*/
      if(ispn.ne.0.and.kpt(2).eq.0)ispn=0                                       !/*im4c2       1*/
      if(ispn.ne.0)n=n+1                                                        !/*im4c2       2*/
      mxe1=mix*max(1,n)+indt                                                    !/*im4c2       3*/
      mnnm=mnnm*(kpt(1)+kpt(2))                                                 !/*im4b        6*/
      if(nsr.eq.6.or.issw.ne.0)call sfiles                                      !/*im         27*/
      ndup(3)=ndup(3)+ncparf                                                    !/*im         28*/
      if(nsr.ne.71)go to 25                                                     !/*im4a       10*/
      if(nsrc.eq.0.and.ksdef.eq.0.and.jovr(4).ne.0)call ksrctp(1)               !/*im4a       11*/
      if(nsrck.lt.100000)msrk=max(4500,2*nsrck,nsrc/3,mrl,msrk)                 !/*im4a       12*/
      if(nsrck.ge.100000)msrk=max(4500,nsrck+nsrck/2,nsrc/3,mrl,msrk)           !/*im4a       13*/
   25 mlja=mlja+2*mxit*ncomp                                                    !/*im4a       14*/
      mlaj=(12+50*junf)*mxa+50                                                  !/*im         32*/
      if(kpt(1).ne.0)mxt=max(mxt,1)                                             !/*im         33*/
      n=0                                                                       !/*im4b        7*/
      do 30 i=1,mipt                                                            !/*im4b        8*/
      if(kpt(i).eq.0)ngww(i)=0                                                  !/*im4b        9*/
   30 if(kpt(i).ne.0.and.wwp(i,5).lt.0.)n=1                                     !/*im4b       10*/
      nwwm=mxa                                                                  !/*im4b       11*/
      if(n.ne.0)call wwfile(0)                                                  !/*im4b       12*/
      if(wwg(1).ne.0..and.wwg(2).eq.0.and.iwwg.eq.2)call avrwgi(0)              !/*im4c        1*/
!                                                                               !/*imx        38*/
      if(emx(1).eq.huge.or.kpt(1).eq.0) then                                    !/*imx        39*/
         ex=0.                                                                  !/*imx        40*/
      else                                                                      !/*imx        41*/
         ex=emx(1)                                                              !/*imx        42*/
      endif                                                                     !/*imx        43*/
!                                                                               !/*imx        44*/
      if(kpt(3).ne.0.or.kpt(2).ne.0.and.ides.eq.0)ex=max(ex,emx(3))             !/*imx        45*/
      do 32 mp=4,mipt                                                           !/*imx        46*/
   32 if(kpt(mp).ne.0)ex=max(ex,emx(mp))                                        !/*imx        47*/
      if(ex.eq.0.)ex=emax_def                                                   !/*imx        48*/
!                                                                               !/*imx        49*/
      if(kpt(3).ne.0.or.kpt(2).ne.0.and.ides.eq.0) then                         !/*imx        50*/
         if(emx(3).eq.0.)emx(3)=ex                                              !/*imx        51*/
         if(ecf(3).ge.emx(3))call erprnt(1,1,0,0,0,0,0,0,                       !/*imx        52*/
     &    '22helectron ecf .ge. emx.')                                          !/*imx        53*/
      endif                                                                     !/*imx        54*/
!                                                                               !/*imx        55*/
      do 36 mp=4,mipt                                                           !/*imx        56*/
      if(kpt(mp).ne.0) then                                                     !/*imx        57*/
         if(emx(mp).eq.0.)emx(mp)=ex                                            !/*imx        58*/
         if(ecf(mp).ge.emx(mp))call erprnt(1,1,0,0,0,0,0,0,                     !/*imx        59*/
     &    '25h'//hnp(mp)//' ecf .ge. emx.')                                     !/*imx        60*/
      endif                                                                     !/*imx        61*/
   36 continue                                                                  !/*imx        62*/
!                                                                               !/*imx        63*/
!        establish energy grids.                                                !/*imx        64*/
      d=in_erg(3)                                                               !/*imx        65*/
      efac(3)=half**(one/d)                                                     !/*imx        66*/
      if(kpt(3).ne.0.or.kpt(2).ne.0.and.ides.eq.0)                              !/*im         38*/
     & nee(3)=in_erg(3)*log(emx(3)/ecf(3))/log(two)+two                         !/*imx        67*/
      do 38 mp=1,mipt                                                           !/*imx        68*/
      if(mp.eq.2.or.mp.eq.3.or.kpt(mp).eq.0)go to 38                            !/*imx        69*/
      d=in_erg(mp)                                                              !/*imx        70*/
      efac(mp)=half**(one/d)                                                    !/*imx        71*/
      if(charge(mp).eq.zero) then                                               !/*imx        72*/
         nee(mp)=2                                                              !/*imx        73*/
      else                                                                      !/*imx        74*/
         nee(mp)=in_erg(mp)*log(emx(mp)/ecf(mp))/log(two)+two                   !/*imx        75*/
      endif                                                                     !/*imx        76*/
   38 continue                                                                  !/*imx        77*/
      do 39 mp=1,mipt                                                           !/*imx        78*/
   39 nee_max=max(nee_max,nee(mp))                                              !/*imx        79*/
!                                                                               !/*imx        80*/
      call setdas                                                               !/*im         40*/
      call dynamic_allocate                                                     !/*imx        81*/
      call imcn_allocate(1)                                                     !/*imx        82*/
!                                                                               !/*im         42*/
!        initialize general common not yet done.                                !/*im         43*/
      do 40 i=1,2                                                               !/*im4c        2*/
      do 40 j=1,mxdt                                                            !/*im         45*/
   40 ddg(i,j)=huge                                                             !/*im         46*/
      do 50 i=1,mipt                                                            !/*im4c        3*/
      do 50 k=1,2                                                               !/*im         47*/
      do 50 j=1,mxdx                                                            !/*im         48*/
   50 ddx(i,k,j)=huge                                                           !/*im         49*/
      dbcn(10)=dftint                                                           !/*im         50*/
      dmp=dfdmp                                                                 !/*im         51*/
      dnb=-1.                                                                   !/*im4c        4*/
      do 60 i=2,mipt                                                            !/*im         54*/
   60 emcf(i)=emax_def                                                          !/*imx        83*/
      ikz=30                                                                    !/*im4c        5*/
      kct=-1                                                                    !/*im4c        7*/
      do 70 i=1,maxv                                                            !/*imx        86*/
   70 ivdd(i)=i                                                                 !/*im         60*/
      lost(1)=10                                                                !/*im         62*/
      lost(2)=10                                                                !/*im         63*/
      npd=1000                                                                  !/*im         64*/
      ntc=50                                                                    !/*im         65*/
      ntc1=50                                                                   !/*im         66*/
      rkk=1.                                                                    !/*im         67*/
      do 80 i=1,mipt                                                            !/*im         68*/
   80 tco(i)=.001*huge                                                          !/*im         69*/
      bnum=1.                                                                   !/*im         70*/
      xnum=1.                                                                   !/*im         71*/
      rnok=1.                                                                   !/*im         72*/
      enum=1.                                                                   !/*im         73*/
      wc1(1)=-.5                                                                !/*im         74*/
      wc2(1)=-.25                                                               !/*im         75*/
      if(kpt(2).ne.0)wc1(2)=huge                                                !/*im         76*/
      if(kpt(2).ne.0)wc2(2)=huge                                                !/*im         77*/
      wgts(1)=huge                                                              !/*im         78*/
      if(wwp(1,4).gt.0.)wc1(1)=0.                                               !/*im4c       11*/
      if(wwp(1,4).gt.0.)wc2(1)=0.                                               !/*im4c       12*/
      if(wwp(2,4).gt.0.)wc1(2)=0.                                               !/*im4c       13*/
      if(wwp(2,4).gt.0.)wc2(2)=0.                                               !/*im4c       14*/
      xunrl=20.                                                                 !/*im4c       15*/
!                                                                               !/*im         83*/
!        initialize default LCA options.                                        !/*imx        87*/
!        ielas, ipreq, iexisa, ichoic, jcoul, nexite, npidk, noact, icem        !/*imx        88*/
      lcaopt(1)=2                                                               !/*imx        89*/
      lcaopt(2)=1                                                               !/*imx        90*/
      lcaopt(3)=1                                                               !/*imx        91*/
      lcaopt(4)=23                                                              !/*imx        92*/
      lcaopt(5)=1                                                               !/*imx        93*/
      lcaopt(6)=1                                                               !/*imx        94*/
      lcaopt(7)=0                                                               !/*imx        95*/
      lcaopt(8)=1                                                               !/*imx        96*/
      lcaopt(9)=0                                                               !/*imx        97*/
!        initialize default LCB options.                                        !/*imx        98*/
!        flenb(1) ... flenb(6), ctofe9, flim0                                   !/*imx        99*/
      optlcb(1)=3490.d0                                                         !/*imx       100*/
      optlcb(2)=3490.d0                                                         !/*imx       101*/
      optlcb(3)=2490.d0                                                         !/*imx       102*/
      optlcb(4)=2490.d0                                                         !/*imx       103*/
      optlcb(5)=800.d0                                                          !/*imx       104*/
      optlcb(6)=800.d0                                                          !/*imx       105*/
      optlcb(7)=-one                                                            !/*imx       106*/
      optlcb(8)=-one                                                            !/*imx       107*/
!        initialize default LEA options.                                        !/*imx       108*/
!        ipht, icc, nobalc, nobale, ifbrk, ilvden, ievap, nofis                 !/*imx       109*/
      leaopt(1)=1                                                               !/*imx       110*/
      leaopt(2)=4                                                               !/*imx       111*/
      leaopt(3)=1                                                               !/*imx       112*/
      leaopt(4)=0                                                               !/*imx       113*/
      leaopt(5)=1                                                               !/*imx       114*/
      leaopt(6)=0                                                               !/*imx       115*/
      leaopt(7)=0                                                               !/*imx       116*/
      leaopt(8)=1                                                               !/*imx       117*/
!        initialize default LEB options.                                        !/*imx       118*/
!        yzere, bzere, yzero, bzero                                             !/*imx       119*/
      optleb(1)=1.5d0                                                           !/*imx       120*/
      optleb(2)=8.0d0                                                           !/*imx       121*/
      optleb(3)=1.5d0                                                           !/*imx       122*/
      optleb(4)=1.0d1                                                           !/*imx       123*/
!                                                                               !/*imx       124*/
      do 160 i=1,mxa                                                            !/*im         97*/
      do 150 j=1,mipt                                                           !/*im         98*/
      do 150 k=0,mxdx                                                           !/*im         99*/
  150 if(ndx(j).ne.0)dxcp(ldxp+k,j,i)=huge                                      !/*im        100*/
  160 if(kpt(1)*kpt(2).ne.0)gwt(lgwt+i)=-1.                                     !/*im        101*/
      do 170 j=1,mipt                                                           !/*im        102*/
  170 if(ngww(j).ne.0)ewwg(lewg+ngww(j)+mgww(j))=100.                           !/*im        103*/
      do 175 i=1,16                                                             !/*im4b       17*/
  175 ebl(lebl+i)=eb(i)                                                         !/*im4b       18*/
      lsc(llsc+1)=lscf                                                          !/*im        104*/
      do 180 i=0,ntal                                                           !/*im        105*/
  180 ipnt(lipn+1,1,i)=2+4+64+128                                               !/*im        106*/
      m=ichar(' ')+256*(ichar(' ')+256*ichar(' '))                              !/*im        107*/
      do 190 i=1,mxe1                                                           !/*im        108*/
      do 190 j=4,60                                                             !/*im        109*/
  190 ixc(lixc+j,i)=m                                                           !/*im        110*/
!                                                                               !/*im4a       15*/
!        initialize default cross-section libraries.                            !/*im4a       16*/
      m2=ichar(' ')+256*(ichar(' ')+256*ichar('p'))                             !/*im4a       17*/
      m3=ichar(' ')+256*(ichar(' ')+256*ichar('e'))                             !/*im4a       18*/
      m4=ichar(' ')+256*(ichar(' ')+256*ichar('u'))                             !/*im4c2       4*/
      m9=ichar(' ')+256*(ichar(' ')+256*ichar('h'))                             !/*imx       125*/
      do 195 i=1,nmat1                                                          !/*im4a       19*/
      lxd(llxd+1,i)=m                                                           !/*im4a       20*/
      lxd(llxd+2,i)=m2                                                          !/*im4a       21*/
      lxd(llxd+3,i)=m3                                                          !/*im4c2       5*/
      lxd(llxd+4,i)=m4                                                          !/*im4c2       6*/
      lxd(llxd+9,i)=m9                                                          !/*imx       126*/
  195 pnt(lpnt+i)=huge                                                          !/*im4c2       7*/
      trf(ltrf+5,0)=1.                                                          !/*im        111*/
      trf(ltrf+9,0)=1.                                                          !/*im        112*/
      trf(ltrf+13,0)=1.                                                         !/*im        113*/
      do 200 j=1,mxtr                                                           !/*im        114*/
      do 200 i=5,13                                                             !/*im        115*/
  200 trf(ltrf+i,j)=huge                                                        !/*im        116*/
      do 210 j=1,mxt                                                            !/*im        117*/
      do 210 i=1,mxa                                                            !/*im        118*/
  210 tmp(ltmp+j+(i-1)*mxt)=253e-10                                             !/*im        119*/
      endfile iu3                                                               !/*im        120*/
      rewind iu3                                                                !/*im        121*/
      do 220 i=1,mxa                                                            !/*im        122*/
  220 read(iu3)mat(lmat+i)                                                      !/*im        123*/
      i=0                                                                       !/*im        124*/
      j=0                                                                       !/*im        125*/
  230 read(iu3,end=250)m                                                        !/*im        126*/
      if(m.lt.0)go to 240                                                       !/*im        128*/
      i=i+2                                                                     !/*im        129*/
      mfm(lmfm+i-1)=m                                                           !/*im        130*/
      read(iu3,end=250)mfm(lmfm+i)                                              !/*im4a       23*/
      go to 230                                                                 !/*im        132*/
  240 j=j+1                                                                     !/*im        133*/
      nmt(lnmt+j)=-m                                                            !/*im        134*/
      go to 230                                                                 !/*im        135*/
  250 jmd(ljmd)=1                                                               !/*im        136*/
      jmd(ljmd+1)=1                                                             !/*im        137*/
#ifdef MESHTAL                                                                  /*!imx       127*/
!                                                                               !/*im        138*/
!       First, put all mesh tallies at the end.                                 !/*imx       128*/
      m=0                                                                       !/*imx       129*/
      do 260 i=1,ntal-1                                                         !/*imx       130*/
      if(ntl(i).ge.0)go to 260                                                  !/*imx       131*/
      if(i.ge.ntal-m+1)go to 268                                                !/*imx       132*/
      do 254 j=1,nmesh                                                          !/*imx       133*/
      n=m                                                                       !/*imx       134*/
      if(abs(ntl(i)).eq.nugd(j)) then                                           !/*imx       135*/
         do 252 k=ntal-n,1,-1                                                   !/*imx       136*/
         if(ntl(k).ge.0.or.ntl(k).eq.ntl(i))go to 253                           !/*imx       137*/
         do 251 l=1,nmesh                                                       !/*imx       138*/
         if(abs(ntl(k)).eq.nugd(l)) then                                        !/*imx       139*/
            jptal(ljpt+1,k)=abs(ntl(k))                                         !/*imx       140*/
            m=m+1                                                               !/*imx       141*/
            mt=mod(abs(ntl(k)),10)                                              !/*imx       142*/
            if(mt.eq.2)call erprnt(1,2,1,mt,0,0,0,0,                            !/*imx       143*/
     &   '35h fm or de/df not allowed with type,i2,12h mesh tally.')            !/*imx       144*/
            go to 252                                                           !/*imx       145*/
         endif                                                                  !/*imx       146*/
  251    continue                                                               !/*imx       147*/
         go to 253                                                              !/*imx       148*/
  252    continue                                                               !/*imx       149*/
  253    m=m+1                                                                  !/*imx       150*/
         mt=mod(abs(ntl(i)),10)                                                 !/*imx       151*/
         if(mt.eq.2)call erprnt(1,2,1,mt,0,0,0,0,                               !/*imx       152*/
     &   '35h fm or de/df not allowed with type,i2,12h mesh tally.')            !/*imx       153*/
         go to 256                                                              !/*imx       154*/
      endif                                                                     !/*imx       155*/
  254 continue                                                                  !/*imx       156*/
      go to 260                                                                 !/*imx       157*/
!                                                                               !/*imx       158*/
  256 k=ntal-m+1                                                                !/*imx       159*/
      nt=ntl(k)                                                                 !/*imx       160*/
      ntl(k)=ntl(i)                                                             !/*imx       161*/
      ntl(i)=(nt)                                                               !/*imx       162*/
      do 258 mp=1,mipt                                                          !/*imx       163*/
      nt=ktt(k,mp)                                                              !/*imx       164*/
      ktt(k,mp)=ktt(i,mp)                                                       !/*imx       165*/
  258 ktt(i,mp)=nt                                                              !/*imx       166*/
      jptal(ljpt+1,k)=abs(ntl(k))                                               !/*imx       167*/
  260 continue                                                                  !/*imx       168*/
!                                                                               !/*imx       169*/
!       Check the ntal'th tally for mesh fm.                                    !/*imx       170*/
  268 if(m.eq.0.and.ntl(ntal).lt.0) then                                        !/*imx       171*/
         m=m+1                                                                  !/*imx       172*/
         jptal(ljpt+1,ntal)=abs(ntl(ntal))                                      !/*imx       173*/
      endif                                                                     !/*imx       174*/
!                                                                               !/*imx       175*/
!       Sort through mesh tallies.                                              !/*imx       176*/
      ntal=ntal-m                                                               !/*imx       177*/
      mshfm=m                                                                   !/*imx       178*/
      l=0                                                                       !/*imx       179*/
      do 278 i=1,nmesh                                                          !/*imx       180*/
      do 276 j=1,m                                                              !/*imx       181*/
      if(abs(ntl(j+ntal)).eq.nugd(i))go to 278                                  !/*imx       182*/
  276 continue                                                                  !/*imx       183*/
      l=l+1                                                                     !/*imx       184*/
      jptal(ljpt+1,ntal+m+l)=nugd(i)                                            !/*imx       185*/
  278 continue                                                                  !/*imx       186*/
#endif                                                                          /*!imx       187*/
!                                                                               !/*imx       188*/
!        sort the tallies, neutrons first.  selection sort.                     !/*imx       189*/
      do 270 i=1,ntal-1                                                         !/*imx       190*/
      k=i                                                                       !/*imx       191*/
      do 265 j=i+1,ntal                                                         !/*imx       192*/
      l=0                                                                       !/*imx       193*/
      do 255 mp=mipt,1,-1                                                       !/*imx       194*/
      if(ktt(j,mp).gt.ktt(k,mp))go to 265                                       !/*imx       195*/
      if(ktt(j,mp).eq.ktt(k,mp))go to 255                                       !/*imx       196*/
      l=1                                                                       !/*imx       197*/
      go to 265                                                                 !/*imx       198*/
  255 continue                                                                  !/*imx       199*/
      if(abs(ntl(j)).lt.abs(ntl(k)))l=1                                         !/*imx       200*/
  265 if(l.ne.0)k=j                                                             !/*imx       201*/
      nt=ntl(k)                                                                 !/*imx       202*/
      ntl(k)=ntl(i)                                                             !/*imx       203*/
      ntl(i)=(nt)                                                               !/*imx       204*/
      do 270 mp=1,mipt                                                          !/*imx       205*/
      nt=ktt(k,mp)                                                              !/*imx       206*/
      ktt(k,mp)=ktt(i,mp)                                                       !/*imx       207*/
  270 ktt(i,mp)=nt                                                              !/*imx       208*/
      do 275 i=1,ntal                                                           !/*imx       209*/
  275 jptal(ljpt+1,i)=ntl(i)                                                    !/*imx       210*/
!                                                                               !/*im        146*/
!        reread and print the inp message block and title line.                 !/*im        147*/
      rewind iui                                                                !/*im        148*/
      do 290 ip=1,3                                                             !/*im        149*/
  280 read(iui,'(a80)')aid                                                      !/*im        150*/
      iln=iln+1                                                                 !/*im        151*/
      write(iuo,'(i5,1h-,7x,a80)')iln,aid                                       !/*im        152*/
      if(ip.eq.2.and.aid.ne.' ')go to 280                                       !/*im        153*/
      if(aid.eq.' ')hs=' '                                                      !/*imx       211*/
      if(aid.eq.' ')go to 290                                                   !/*imx       212*/
      call nxtsym(aid,' ',1,it,iu,0)                                            !/*im4a       24*/
      hs=aid(it:min(max(it,iu),it+7))                                           !/*im4a       25*/
      call nxtsym(hs,' ',1,i,j,2)                                               !/*im4a       26*/
  290 if(ip.eq.1.and.hs.ne.'message:')go to 300                                 !/*im4a       27*/
  300 if(iu.eq.0)go to 310                                                      !/*im4a       28*/
      call nxtsym(aid,' ',iu+1,it,iu,0)                                         !/*im4a       29*/
      if(iu.eq.0)go to 310                                                      !/*im4a       30*/
      if(kdata(hs).eq.2.and.kdata(aid(it:iu)).eq.2)                             !/*im4a       31*/
     & call erprnt(1,2,0,0,0,0,0,0,                                             !/*im        158*/
     & '51hthe title card looks suspiciously like a cell card.')                !/*im        159*/
!                                                                               !/*im        160*/
!        reread the rest of the inp file and set up the problem.                !/*im        161*/
  310 call rdprob                                                               !/*im        162*/
      if(dbcn(6).ne.0.)ink(128)=1                                               !/*imx       213*/
#ifdef HISTP                                                                    /*!imx       214*/
      do ic=1,mxa                                                               !/*imx       215*/
         if (nhw(lnhw+ic).ne.0) go to 312                                       !/*imx       216*/
      enddo                                                                     !/*imx       217*/
      do ic=1,mxa                                                               !/*imx       218*/
         nhw(lnhw+ic) = 1                                                       !/*imx       219*/
      enddo                                                                     !/*imx       220*/
!                                                                               !/*imx       221*/
  312 continue                                                                  !/*imx       222*/
#endif                                                                          /*!imx       223*/
!                                                                               !/*imx       224*/
      do 340 i=1,mipt                                                           !/*cor4-2     83*/
      if(kpt(i).eq.0.or.nww(i).ne.0)go to 340                                   !/*cor4-2     84*/
      do 315 m=1,mxa                                                            !/*im4a       32*/
  315 if(fim(lfim+i,m).eq.0..and.mat(lmat+m).ne.0)                              !/*im4a       33*/
     & call erprnt(1,2,1,ncl(lncl+m),0,0,0,0,                                   !/*im4a       34*/
     & '48hzero '//hnp(i)//' importance within non-void cell,i5,1h.')           !/*imx       225*/
      do 320 m=1,mxa                                                            !/*cor4-2     85*/
  320 if(fim(lfim+i,m).ne.0.)go to 330                                          !/*cor4-2     86*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*cor4-2     87*/
     & '37h'//hnp(i)//' importances are all zero.')                             !/*imx       226*/
      go to 340                                                                 !/*cor4-2     89*/
  330 if(ifip(i).eq.0)call erprnt(1,2,0,0,0,0,0,0,                              !/*cor4-2     90*/
     & '49h'//hnp(i)//' importances have been set equal to 1.')                 !/*imx       227*/
  340 if(kpt(mipt+1-i).ne.0)nips=mipt+1-i                                       !/*im        168*/
      if(srv(1,19).ne.0)nips=srv(1,19)                                          !/*im        169*/
!        enable anti-particle sources.                                          !/*imx       228*/
!        TEMPORARY RESTRICTION: do not allow positrons.                         !/*imx       229*/
      if (nips.lt.0) then                                                       !/*imx       230*/
         isanti=1                                                               !/*imx       231*/
         nips=-nips                                                             !/*imx       232*/
         if(nips.eq.3)call erprnt(1,1,0,0,0,0,0,0,                              !/*imx       233*/
     &     '36hpositron source not yet implemented.')                           !/*imx       234*/
      endif                                                                     !/*imx       235*/
      if(kpt(nips).eq.0)call erprnt(1,1,0,0,0,0,0,0,                            !/*im        170*/
     & '39h source particle type not on mode card.')                            !/*im        171*/
      if(kpt(2).eq.0)iphot=1                                                    !/*im        172*/
      do 350 i=1,mipt                                                           !/*im        173*/
  350 if(ipty(i).ne.0)go to 370                                                 !/*im        174*/
      do 360 i=1,mipt                                                           !/*im        175*/
  360 ipty(i)=1                                                                 !/*im        176*/
  370 call igeom                                                                !/*im        177*/
      call iwtwnd                                                               !/*imx       236*/
      call isourc                                                               !/*im        180*/
      call itally                                                               !/*im        181*/
#ifdef MESHTAL                                                                  /*!imx       237*/
      if(nmesh.gt.0)call gdtal(0,zero,zd,jz)                                    !/*imx       238*/
#endif                                                                          /*!imx       239*/
      if(iptr.ne.0)call ptrak(0)                                                !/*im4a       36*/
      call stuff                                                                !/*im        182*/
!                                                                               !/*imx       240*/
      if(dbcn(26).ne.0..and.cut_n.le.0.)cut_n=huge                              !/*imx       241*/
      if(cut_n.le.0.)cut_n=20.                                                  !/*imx       242*/
      if(mcal.ne.0)cut_n=max(cut_n,emx(1))                                      !/*imx       243*/
!                                                                               !/*imx       244*/
      if(mcal.eq.0) then                                                        !/*imx       245*/
!                                                                               !/*imx       246*/
!        print out particle types and energy limits.                            !/*imx       247*/
      write(iuo,371)                                                            !/*imx       248*/
  371 format(/28h1particles and energy limits,76x,14hprint table 41//           !/*imx       249*/
     & '                         minimum       table         maximum'/          !/*imx       250*/
     & '   particle type         energy        energy        energy'/)          !/*imx       251*/
      k=0                                                                       !/*imx       252*/
      do 372 ip=1,mipt                                                          !/*imx       253*/
      if(kpt(ip).eq.0)go to 372                                                 !/*imx       254*/
      em=huge                                                                   !/*imx       255*/
      do ic=1,mxa                                                               !/*imx       256*/
         em=min(em,elp(lelp+ip,ic))                                             !/*imx       257*/
      enddo                                                                     !/*imx       258*/
      em=max(ecf(ip),em)                                                        !/*imx       259*/
      et=0.                                                                     !/*imx       260*/
      if(ip.eq.1)et=cut_n                                                       !/*imx       261*/
      if(ip.eq.2)et=100000.                                                     !/*imx       262*/
      if(ip.eq.3)et=1000.                                                       !/*imx       263*/
      if(ip.eq.9)et=cut_h                                                       !/*imx       264*/
      write(iuo,'(i5,2x,a1,2x,a10,1p3e14.4)')                                   !/*imx       265*/
     & ip,hip(ip:ip),hnp(ip),em,et,emx(ip)                                      !/*imx       266*/
  372 continue                                                                  !/*imx       267*/
!                                                                               !/*imx       268*/
         if(dbcn(26).ne.0.)then                                                 !/*imx       269*/
         call setlcs(jtty,jtty,ink(43),nmat,kpt,mipt,                           !/*imx       270*/
     &    lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                  !/*imx       271*/
         else                                                                   !/*imx       272*/
         call setlcs(jtty,iuo,ink(43),nmat,kpt,mipt,                            !/*imx       273*/
     &    lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                  !/*imx       274*/
         endif                                                                  !/*imx       275*/
#ifdef LCS_PRINT                                                                /*!imx       276*/
         write(iuo,372)                                                         !/*imx       277*/
  372    format(20h1LAHET preliminaries,84x,14hprint table 42)                  !/*imx       278*/
#endif                                                                          /*!imx       279*/
         call mtinp1 (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*imx       280*/
     &    zz9(lzz9+1),fmf(lfmf+1),iza(liza+1),fme(lfme+1),jmd(ljmd+1))          !/*imx       281*/
         call matinp (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*imx       282*/
     &    zz9(lzz9+1),fmf(lfmf+1),arg(larg+1),rho(lrho+1))                      !/*imx       283*/
         call xin (nel(lnel+1),mel(lmel+1),iazdex(liax+1),fmf(lfmf+1),          !/*imx       284*/
     &    zz9(lzz9+1),aa9(laa9+1),rho(lrho+1),sigg(lsgg+1,1),                   !/*imx       285*/
     &    hsigg(lhsg+1,1),sigmx(lsgx+1,1),pcap(lpcp+1),esxln(lexn+1,1),         !/*imx       286*/
     &    esxlp(lexp+1,1),esxmp(lesp+1))                                        !/*imx       287*/
      endif                                                                     !/*imx       288*/
!                                                                               !/*imx       289*/
#ifdef HISTP                                                                    /*!imx       290*/
      if(ihistp.ne.0) then                                                      !/*imx       291*/
        if (nsr.eq.6) then                                                      !/*imx       292*/
          mx=np1                                                                !/*imx       293*/
        else                                                                    !/*imx       294*/
          mx=npp                                                                !/*imx       295*/
        endif                                                                   !/*imx       296*/
        call ins_histp(nel(lnel+1),mel(lmel+1),zz9(lzz9+1),aa9(laa9+1),         !/*imx       297*/
     &   sigg(lsgg+1,1),hsigg(lhsg+1,1),sigmx(lsgx+1,1),probid,mx)              !/*imx       298*/
      endif                                                                     !/*imx       299*/
#endif                                                                          /*!imx       300*/
      if(npert.ne.0)call inpert                                                 !/*im4b       19*/
      call imcn_allocate(0)                                                     !/*imx       301*/
      call volume_allocate(1)                                                   !/*imx       302*/
      call volume                                                               !/*im        183*/
      call celsrf                                                               !/*im        184*/
      call volume_allocate(0)                                                   !/*imx       303*/
      if(nsr.eq.40)call wtcalc                                                  !/*im        185*/
      if(iwwg.gt.0)call avrwgi(1)                                               !/*im4c       20*/
      call random                                                               !/*im        186*/
!                                                                               !/*im4a       37*/
!        set up internal history score grid for history score                   !/*im4a       38*/
!        probability density for the tfc bin. hsll (history score lower         !/*im4a       39*/
!        limit) multiplied by the starting source and the user input            !/*im4a       40*/
!        abs(dbcn(16)) defines the lower limit of the 60 decade internal        !/*im4a       41*/
!        score grid that will be accepted in the history score frequency        !/*im4a       42*/
!        arrays. there are 10 bins per score decade and nsp-2 bins per          !/*im4a       43*/
!        tally. the user could change nsp and hsll to create a different        !/*im4a       44*/
!        score grid structure. the internal history score grid bin j in         !/*im4a       45*/
!        the frequency arrays is found with j=10*log10(tal)+nhb in              !/*im4a       46*/
!        subroutine talshf. nhb (number of the history bin) is defined          !/*im4a       47*/
!        to put the smallest acceptable tally into bin 2 and the largest        !/*im4a       48*/
!        acceptable tally into bin nsp-1. the sign of score grid                !/*im4a       49*/
!        multiplier dbcn(16) is used to set hsb(nsp) to +1 or -1,               !/*im4a       50*/
!        which is used to acrue positive or negative history scores in          !/*im4a       51*/
!        subroutine talshf for the tfc bin probability density function         !/*im4a       52*/
!        analysis.                                                              !/*im4a       53*/
      wt=srv(1,17)                                                              !/*im4a       54*/
      if(wt.le.0.)wt=1.                                                         !/*im4a       55*/
      if(dbcn(16).eq.0.)dbcn(16)=1.                                             !/*im4a       56*/
      t=log10(hsll*wt*abs(dbcn(16)))                                            !/*im4a       57*/
      nl=t+sign(.01+zero,t)                                                     !/*im4a       58*/
      nhb=2-10*nl                                                               !/*im4a       59*/
      do 375 i=0,nsp-2                                                          !/*im4a       60*/
      q=nl+0.1*i                                                                !/*im4c2      11*/
  375 hsb(i+1)=10.d0**q                                                         !/*im4c2      12*/
      hsb(nsp)=sign(one,dbcn(16))                                               !/*im4a       62*/
!                                                                               !/*im        187*/
!        define offsets in das for the bank and cross-section tables.           !/*im        188*/
      ktls=.1*mxf                                                               !/*im        189*/
      mbnk=nbmx*(npblcm+15*abs(iunr)+10*nlev+3*nptb(lnpt+npert+1))              !/*imx       304*/
#if defined(MULTT) || defined(MULTP)                                            /*!imxl        1*/
      tal => chg_mem (tal,(nmxf*mxf+ktls)*(mtasks+1),is)                        !/*imxl        2*/
#else                                                                           /*!imxl        3*/
      tal => chg_mem (tal,nmxf*mxf+ktls,is)                                     !/*imxl        4*/
#endif                                                                          /*!imxl        5*/
      if(is.ne.0)call expire(0,'imcn','chg_mem error')                          !/*imx       306*/
      tal=0.                                                                    !/*imx       307*/
!                                                                               !/*im        196*/
!        warn of possible need for subroutine srcdx.                            !/*im        197*/
      do 380 i=1,ntal                                                           !/*im        198*/
  380 if(jptal(ljpt+2,i).eq.5)go to 400                                         !/*im        199*/
      do 390 i=1,mipt                                                           !/*im        200*/
  390 if(ndx(i).ne.0)go to 400                                                  !/*im        201*/
      go to 410                                                                 !/*im        202*/
  400 if(nsr.eq.0)call erprnt(1,2,0,0,0,0,0,0,                                  !/*im        203*/
     & '56hsubroutine srcdx may be needed if source is anisotropic.')           !/*im        204*/
!                                                                               !/*im        205*/
!        warn of strange time cutoffs.                                          !/*im        206*/
  410 do 420 j=2,mipt                                                           !/*im        207*/
      do 420 i=1,j-1                                                            !/*im        208*/
  420 if(kpt(i)*kpt(j).ne.0.and.tco(i).ne.tco(j))                               !/*im        209*/
     & call erprnt(1,2,0,0,0,0,0,0,                                             !/*imx       308*/
     & '47hunequal time cutoffs: '//hnp(i)//', '//hnp(j)//'.')                  !/*imx       309*/
!                                                                               !/*im        212*/
!                                                                               !/*nyxk        5*/
!        threading available only with tabular problems.                        !/*nyxk        6*/
      if(ntasks.gt.1)call erprnt(1,2,0,0,0,0,0,0,                               !/*nyxk        7*/
     & '47hthreading available only with tabular problems.')                    !/*nyxk        8*/
!        dxtran/detectors fail with delayed neutron biasing because             !/*imxc       13*/
!        cmult assumes all particles from a collision have same wgt.            !/*imxc       14*/
      if(dnb.gt.zero.and.(ndet(1).gt.0.or.ndx(1).gt.0))                         !/*imxc       15*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*imxc       16*/
     & '52hdelayed neutron biasing fails with dxtran/detectors.')               !/*imxc       17*/
!        set up the weight cutoffs.                                             !/*im        213*/
      wcs1(1)=max(wc1(1),-wc1(1)*swtm)                                          !/*im        214*/
      wcs2(1)=max(wc2(1),-wc2(1)*swtm)                                          !/*im        215*/
      if(abs(kf8).eq.3.and.ispn.gt.0)call erprnt(1,2,0,0,0,0,0,0,               !/*imxc       18*/
     & '57hphotonuclear production does variance reduction with f8.')           !/*imxc       19*/
      if(abs(kf8).eq.3.and.ispn.gt.0)kf8=sign(4,kf8)                            !/*imxc       20*/
      if(abs(kf8).eq.3.and.iunr.ne.0)call erprnt(1,2,0,0,0,0,0,0,               !/*imxc       21*/
     & '54hunresolved resonances use variance reduction with f8.')              !/*imxc       22*/
      if(abs(kf8).eq.3.and.iunr.ne.0)kf8=sign(4,kf8)                            !/*imxc       23*/
      if(abs(kf8).eq.3.and.wc1(2).ne.0.)call erprnt(1,2,0,0,0,0,0,0,            !/*imxc       24*/
     & '51hswitching to analog capture for pulse height tally.')                !/*imxc       25*/
      if(abs(kf8).eq.4)call erprnt(1,1,0,0,0,0,0,0,                             !/*imxc       30*/
     & '43hvariance reduction with pulse height tally.')                        !/*imxc       31*/
!                                                                               !/*imxc       33*/
      if(abs(kf8).eq.3)wc1(2)=0.                                                !/*imx       312*/
      do 430 i=2,mipt                                                           !/*im        217*/
      if(wc1(i).eq.huge.and.kpt(1).ne.0)wc1(i)=wc1(1)                           !/*im        218*/
      if(wc1(i).eq.huge.and.kpt(1).eq.0)wc1(i)=-.5                              !/*im        219*/
      if(wc2(i).eq.huge)wc2(i)=.5*wc1(i)                                        !/*im        220*/
      wcs1(i)=max(wc1(i),-wc1(i)*swtm)                                          !/*im        221*/
  430 wcs2(i)=max(wc2(i),-wc2(i)*swtm)                                          !/*im        222*/
      if(nde.ne.0)dbcn(2)=nde                                                   !/*im        223*/
      go to 540                                                                 !/*im        224*/
!                                                                               !/*im        225*/
!        prepare for continue run.                                              !/*im        226*/
  440 call tpefil(5)                                                            !/*im        227*/
      call imcn_allocate(1)                                                     !/*imx       313*/
!                                                                               !/*imx       314*/
      if(dbcn(26).ne.0..and.cut_n.le.0.)cut_n=huge                              !/*imx       315*/
      if(cut_n.le.0.)cut_n=20.                                                  !/*imx       316*/
      if(mcal.ne.0)cut_n=max(cut_n,emx(1))                                      !/*imx       317*/
!                                                                               !/*imx       318*/
      if(mcal.eq.0) then                                                        !/*imx       319*/
      if(dbcn(26).ne.0.)then                                                    !/*imx       320*/
         call setlcs(jtty,jtty,ink(43),nmat,kpt,mipt,                           !/*imx       321*/
     &    lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                  !/*imx       322*/
      else                                                                      !/*imx       323*/
         call setlcs(jtty,iuo,ink(43),nmat,kpt,mipt,                            !/*imx       324*/
     &    lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                  !/*imx       325*/
      endif                                                                     !/*imx       326*/
#ifdef LCS_PRINT                                                                /*!imx       327*/
         write(iuo,442)                                                         !/*imx       328*/
  442    format(20h1LAHET preliminaries,84x,14hprint table 42)                  !/*imx       329*/
#endif                                                                          /*!imx       330*/
         call mtinp1 (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*imx       331*/
     &    zz9(lzz9+1),fmf(lfmf+1),iza(liza+1),fme(lfme+1),jmd(ljmd+1))          !/*imx       332*/
         call matinp (mxa,nel(lnel+1),mel(lmel+1),aa9(laa9+1),                  !/*imx       333*/
     &    zz9(lzz9+1),fmf(lfmf+1),arg(larg+1),rho(lrho+1))                      !/*imx       334*/
         call xin (nel(lnel+1),mel(lmel+1),iazdex(liax+1),fmf(lfmf+1),          !/*imx       335*/
     &    zz9(lzz9+1),aa9(laa9+1),rho(lrho+1),sigg(lsgg+1,1),                   !/*imx       336*/
     &    hsigg(lhsg+1,1),sigmx(lsgx+1,1),pcap(lpcp+1),esxln(lexn+1,1),         !/*imx       337*/
     &    esxlp(lexp+1,1),esxmp(lesp+1))                                        !/*imx       338*/
      endif                                                                     !/*imx       339*/
!                                                                               !/*imx       340*/
#ifdef HISTP                                                                    /*!imx       341*/
      if(ihistp.ne.0)call erprnt(1,1,0,0,0,0,0,0,                               !/*imx       342*/
     & '51hcontinue run not yet consistent with histp writing.')                !/*imx       343*/
#endif                                                                          /*!imx       344*/
      do 442 i=1,ltskpt-1                                                       !/*im4c       29*/
  442 ktskpt(i)=lpntcm(i)                                                       !/*im4c       30*/
      if(mct.lt.0)rewind iuo                                                    !/*im4b       25*/
      if(dbcn(26).eq.0.)call lex(iuo)                                           !/*imx       345*/
      write(iuo,'(1x,a80)')aid                                                  !/*im        236*/
#ifdef MESHTAL                                                                  /*!imx       346*/
      if(nmesh.gt.0)call gdtal(7,zero,zd,jz)                                    !/*imx       347*/
#endif                                                                          /*!imx       348*/
      if(ntasks.gt.mtasks)call expire(0,'imcn',                                 !/*im        237*/
     & 'more tasks in this continue run than in the original run.')             !/*im        238*/
      if(nsr.eq.6.or.issw.ne.0)call sfiles                                      !/*im        239*/
      do 490 i=1,mink*mnk                                                       !/*im        240*/
  490 ink(i)=1                                                                  !/*im        241*/
  492 if(nsr.eq.71)call ksrctp(3)                                               !/*im4c       41*/
      if(iwwg.gt.0)call avrwgi(1)                                               !/*im4c       42*/
!                                                                               !/*im        243*/
!        reread and print the inp message block and title line.                 !/*im        244*/
      call nxtsym(aid1,' ',1,i,j,0)                                             !/*im4a       68*/
      if(j-i.ne.7)go to 530                                                     !/*im4a       69*/
      hs=aid1(i:j)                                                              !/*im4a       70*/
      call nxtsym(hs,' ',1,i,j,2)                                               !/*im4a       71*/
      if(hs.ne.'continue')go to 530                                             !/*im4a       72*/
      rewind iui                                                                !/*im        246*/
      write(iuo,'(1h )')                                                        !/*im        247*/
      do 510 ip=1,3                                                             !/*im        248*/
  500 read(iui,'(a80)')klin                                                     !/*im        249*/
      iln=iln+1                                                                 !/*im        250*/
      write(iuo,'(i5,1h-,7x,a80)')iln,klin                                      !/*im        251*/
      if(ip.eq.2.and.klin.ne.' ')go to 500                                      !/*im        252*/
      call nxtsym(klin,' ',1,it,iu,0)                                           !/*im4a       73*/
      hs=klin(it:min(max(it,iu),it+7))                                          !/*im4a       74*/
      call nxtsym(hs,' ',1,i,j,2)                                               !/*im4a       75*/
  510 if(ip.eq.1.and.hs.ne.'message:')go to 520                                 !/*im4a       76*/
!                                                                               !/*im        254*/
!        read the continue-run data from the inp file.                          !/*im        255*/
  520 call rdprob                                                               !/*im        256*/
      if(dbcn(1)+dbcn(8).ne.0.)call random                                      !/*im        257*/
  530 if(nde.ne.0)dbcn(2)=nde                                                   !/*im        258*/
      if(nsr.eq.6.and.nrrs.ge.nrss.and.npp.ge.0)call expire(0,'imcn',           !/*im        259*/
     & 'no more tracks on '//rssa//' file.')                                    !/*im        260*/
      if((nfer.eq.0.or.lfatl.ne.0).and.jovr(4).ne.0.and.npp.ge.0)               !/*im4a       77*/
     & call tpefil(6)                                                           !/*im4a       78*/
      call imcn_allocate(0)                                                     !/*imx       351*/
      deallocate (jtr)                                                          !/*imx       352*/
!                                                                               !/*im4a       79*/
!        clean up for both initial and continue runs.                           !/*im4a       80*/
  540 call ufiles                                                               !/*im        262*/
      close(iu1)                                                                !/*im        263*/
      close(iu2)                                                                !/*im        264*/
      close(iu3)                                                                !/*im        265*/
      if(konrun.eq.0.or.npp.lt.0.or.ntal.eq.0)return                            !/*im4a       81*/
!                                                                               !/*im4a       82*/
!        set npc(nn) to zero if tfc nps entry was not scheduled.                !/*im4a       83*/
      do 550 i=1,20                                                             !/*im4a       84*/
  550 if(npc(i).ne.0)nn=i                                                       !/*im4a       85*/
      if(nn.eq.1)go to 560                                                      !/*im4a       86*/
      n=npd                                                                     !/*im4a       87*/
      if(nn.eq.20)n=npd/2                                                       !/*im4a       88*/
      if(npc(nn).ne.npc(nn-1)+n)npc(nn)=0                                       !/*im4a       89*/
      return                                                                    !/*im4a       90*/
  560 if(npc(1).ne.npd)npc(1)=0                                                 !/*im4a       91*/
      return                                                                    !/*im        266*/
      end                                                                       !/*im        267*/

!#ident newcrd.F from deck ne                                                   !/*ne          1*/
      subroutine newcrd(ib)                                                     !/*ne          2*/
!        set up and check a new input card for processing.                      !/*ne          3*/
      use global_data                                                           !/*nex         1*/
      use imcn_data                                                             !/*nex         2*/
!                                                                               !/*nex         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*nex         4*/
      implicit integer(kindi) (i-n)                                             !/*nex         5*/
!                                                                               !/*nex         6*/
      dimension ly(10),ld(mktc)                                                 !/*ne          6*/
      character ht*8                                                            !/*ne4b        1*/
!        ly=1 for legitimate tally types.                                       !/*ne          8*/
      data ly/0,1,1,0,1,1,1,1,1,0/                                              !/*ne          9*/
!        ld=1 for tally input card types that can legally be defaulted.         !/*ne         10*/
      data ld/2*0,4*1,5*0,4*1,0,3*1,0,1,5*0/                                    !/*ne4c        1*/
!                                                                               !/*ne         12*/
!        initialize flags and scratch storage.                                  !/*ne         13*/
      nwc=0                                                                     !/*ne         14*/
      nii=0                                                                     !/*ne         15*/
      m1c=0                                                                     !/*ne         16*/
      m2c=0                                                                     !/*ne         17*/
      m3c=0                                                                     !/*ne         18*/
      m4c=0                                                                     !/*ne         19*/
      m5c=0                                                                     !/*ne4a        1*/
      m6c=0                                                                     !/*ne4a        2*/
      m7c=0                                                                     !/*ne4a        3*/
      m8c=0                                                                     !/*ne4a        4*/
      m9c=0                                                                     !/*ne4a        5*/
      m10c=0                                                                    !/*ne4a        6*/
!                                                                               !/*ne         20*/
!        set icn to the value of the number in the card type field.             !/*ne4a        7*/
!        set icx=0,1,2 for nothing , * , + in card type field.                  !/*ne4a        8*/
!        set ich and ica to the card symbol and its index.                      !/*ne         31*/
!        for the sake of easier maintenance, ica should only be used in         !/*ne         32*/
!        computed gotos and as the index of card attribute arrays.              !/*ne         33*/
      ich=' '                                                                   !/*ne         34*/
      icx=0                                                                     !/*ne4a        9*/
      icn=0                                                                     !/*ne4a       10*/
      nqw=0                                                                     !/*ne         35*/
      do 20 i=1,mipt                                                            !/*ne         36*/
   20 nqp(i)=0                                                                  !/*ne         37*/
      n=0                                                                       !/*ne         38*/
      call nxtsym(klin,' ',1,it,iu,1)                                           !/*ne4a       11*/
      do 30 irc=it,iu                                                           !/*ne4a       12*/
      if(klin(irc:irc).eq.':')go to 40                                          !/*ne4a       13*/
      if(klin(irc:irc).eq.'*')icx=1                                             !/*ne4a       14*/
      if(klin(irc:irc).eq.'+')icx=2                                             !/*ne4a       15*/
      m=index('0123456789',klin(irc:irc))                                       !/*ne4a       16*/
      if(m.ne.0)icn=10*icn+m-1                                                  !/*ne4a       17*/
      if(icn.gt.99999)call erprnt(2,1,0,0,0,0,0,1,                              !/*ne4a       18*/
     & '32htoo many numbers in first entry.')                                   !/*ne4a       19*/
      l=index('abcdefghijklmnopqrstuvwxyz',klin(irc:irc))                       !/*ne4a       20*/
      if(l.ne.0)n=n+1                                                           !/*ne4a       21*/
   30 if(l.ne.0)ich(n:n)=klin(irc:irc)                                          !/*ne4a       22*/
      go to 50                                                                  !/*ne4a       23*/
   40 irc=irc+1                                                                 !/*ne         47*/
      if(klin(irc:irc).eq.' ')go to 50                                          !/*ne         48*/
      i=index(','//hip,klin(irc:irc))-1                                         !/*ne         49*/
      if(i.gt.0)nqw=nqw+1                                                       !/*nex         7*/
      if(i.gt.0)nqp(i)=1                                                        !/*ne         51*/
      if(i.ge.0)go to 40                                                        !/*ne         52*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne         53*/
     & '33hillegal particle-type designator.')                                  !/*ne         54*/
      return                                                                    !/*ne         55*/
   50 continue                                                                  !/*nex         8*/
#ifdef MESHTAL                                                                  /*!nex         9*/
      if (mod(icn,10).eq.6.and.icx.eq.2.and.ich.eq.'f') then                    !/*nex        10*/
         if(nqw.ne.0)call erprnt(2,1,0,0,0,0,0,1,                               !/*nex        11*/
     &    '47hparticle designator not allowed with +f6 tally.')                 !/*nex        12*/
         nqw=1                                                                  !/*nex        13*/
         nqp(1)=1                                                               !/*nex        14*/
      endif                                                                     !/*nex        15*/
#endif                                                                          /*!nex        16*/
      if(ib.eq.3)go to 60                                                       !/*nex        17*/
      ica=ib                                                                    !/*ne         57*/
      if(ich.eq.' ')go to 80                                                    !/*ne         58*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne         59*/
     & '55hillegal pattern as first entry on cell or surface card.')            !/*ne4a       24*/
      return                                                                    !/*ne         61*/
   60 do 70 ica=3,nkcd                                                          !/*ne         62*/
   70 if(cnm(ica).eq.ich)go to 80                                               !/*ne         63*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne         64*/
     & '33h'//ich//' is not a legal data symbol.')                              !/*ne         65*/
      return                                                                    !/*ne         66*/
#ifdef PCDOS                                                                    /*!ne4a       25*/
   80 if(lockl)call pttyin                                                      !/*ne4a       26*/
      if(irup.ne.0)call qttyin(0,'30hreading input cards.     '//ich)           !/*ne4a       27*/
#else                                                                           /*!nex        18*/
   80 if(irup.ne.0)call qttyin(0,'30hreading input cards.     '//ich)           !/*ne4a       29*/
#endif                                                                          /*!cpp       108*/
      if(krq(2,ica).eq.0.or.nqw.ne.0)go to 90                                   !/*ne4a       30*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne         68*/
     & '36hparticle-type designator is missing.')                               !/*ne         69*/
      return                                                                    !/*ne         70*/
!                                                                               !/*ne         71*/
!        check the card type.                                                   !/*ne         72*/
   90 if(krq(1,ica).ne.0.or.konrun.eq.0)go to 100                               !/*ne         73*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne         74*/
     & '51h'//ich//' is not a legal data symbol in a continue run.')            !/*ne         75*/
      return                                                                    !/*ne         76*/
  100 if(konrun.ne.0)go to 140                                                  !/*ne         77*/
      do 110 i=1,ndup(ib)                                                       !/*ne         79*/
      if(kdup(ldup+mipt+1,i).ne.icn)go to 105                                   !/*nex        19*/
      if(kdup(ldup+mipt+2,i).ne.ica)go to 105                                   !/*nex        20*/
      do 102 mp=1,mipt                                                          !/*nex        21*/
  102 if(kdup(ldup+mp,i).ne.nqp(mp))go to 105                                   !/*nex        22*/
      go to 120                                                                 !/*nex        23*/
  105 if(kdup(ldup+mipt+2,i).eq.0)go to 130                                     !/*nex        24*/
      if(nqw.eq.0)go to 110                                                     !/*ne4a       32*/
      if(kdup(ldup+mipt+1,i).ne.icn)go to 110                                   !/*nex        25*/
      if(kdup(ldup+mipt+2,i).ne.ica)go to 110                                   !/*nex        26*/
      do 108 mp=1,mipt                                                          !/*nex        27*/
  108 if(kdup(ldup+mp,i).ne.0.and.nqp(mp).ne.0)go to 120                        !/*nex        28*/
  110 continue                                                                  !/*ne4a       37*/
  120 call nxtsym(klin,' ',1,it,iu,1)                                           !/*ne4a       38*/
      ht=klin(it:iu)                                                            !/*ne4a       39*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne4a       40*/
     & '44hdata symbol '//ht//' is used more than once.')                       !/*ne4b        2*/
      return                                                                    !/*ne         84*/
  130 kdup(ldup+mipt+1,i)=icn                                                   !/*nex        29*/
      kdup(ldup+mipt+2,i)=ica                                                   !/*nex        30*/
      do 135 mp=1,mipt                                                          !/*nex        31*/
  135 kdup(ldup+mp,i)=nqp(mp)                                                   !/*nex        32*/
!                                                                               !/*ne         86*/
!        do the initialization necessary for the particular card type.          !/*ne         87*/
  140 go to(170,180,190,150,150,150,150,150,200,210,220,225,225,160,330,        !/*ne4b        3*/
     &      230,250,260,260,260,260,260,320,330,330,330,330,330,330,330,        !/*ne         89*/
     &      330,330,330,330,330,330,330,330,330,330,330,330,330,330,330,        !/*ne         90*/
     &      330,410,440,150,150,150,490,500,150,160)ica                         !/*ne         91*/
      go to(150,150,440,150,150,150,150,150,150,150,150,150,530,550,560,        !/*ne4b        4*/
     &      150,570,580,590,150,150,150,150,150,600,150,160,610,620,640,        !/*ne4b        5*/
     &      660,550,720,150,150,150,150,740,760,800,900,330,330,330,920,        !/*nex        33*/
     &      150,150,9910,9920,9930)ica-55                                       !/*nexc        1*/
  150 return                                                                    !/*ne         95*/
  160 go to 9010                                                                !/*ne         96*/
!                                                                               !/*ne         97*/
! >>>>>  cell descriptions                                                      !/*ne         98*/
  170 mxa=mxa+1                                                                 !/*ne         99*/
      lca(llca+mxa)=nlja+1                                                      !/*ne        100*/
      ncl(lncl+mxa)=icn                                                         !/*ne        101*/
      return                                                                    !/*ne        102*/
!                                                                               !/*ne        103*/
! >>>>>  surface descriptions                                                   !/*ne        104*/
  180 mxj=mxj+1                                                                 !/*ne        105*/
      nsfm(lnsf+mxj)=icn                                                        !/*ne4c        3*/
      if(icx.ne.0)ksu(lksu+mxj)=-icx                                            !/*ne4a       43*/
      return                                                                    !/*ne        107*/
!                                                                               !/*ne        108*/
! >>>>>  specification of coordinate transformations for surfaces     tr        !/*ne        109*/
  190 mxtr=mxtr+1                                                               !/*ne        110*/
      trf(ltrf+1,mxtr)=-icn                                                     !/*ne        111*/
      return                                                                    !/*ne        112*/
!                                                                               !/*ne        113*/
! >>>>>  photon weight lower bounds                                  pwt        !/*ne        114*/
  200 if(kpt(1)*kpt(2).eq.0)call erprnt(2,1,0,0,0,0,0,1,                        !/*ne        115*/
     & '44hstorage is not provided for pwt in this mode')                       !/*cor4-2    106*/
      go to 9010                                                                !/*nexc        2*/
!                                                                               !/*ne        118*/
! >>>>>  exponential transform                                       ext        !/*ne        119*/
  210 if(nqp(3).ne.0)call erprnt(2,1,0,0,0,0,0,1,                               !/*ne        120*/
     & '53hexponential transform is not available for electrons.')              !/*ne        121*/
      go to 9010                                                                !/*ne        122*/
!                                                                               !/*ne        123*/
! >>>>>  forced collisions                                           fcl        !/*ne        124*/
  220 if(nqp(3).ne.0)call erprnt(2,1,0,0,0,0,0,1,                               !/*ne        125*/
     & '50hforced collisions are not available for electrons.')                 !/*ne        126*/
      go to 9010                                                                !/*ne        127*/
!                                                                               !/*ne4b        6*/
! >>>>>  weight-window lower bounds, energies                    wwn,wwe        !/*ne4b        7*/
  225 do 226 i=1,mipt                                                           !/*ne4b        8*/
  226 if(nqp(i).ne.0.and.nww(i).ne.0.and.wwm(1).ne.0.)go to 227                 !/*ne4b        9*/
      go to 9010                                                                !/*ne4b       10*/
  227 nww(i)=0                                                                  !/*ne4b       11*/
      call erprnt(2,1,0,0,0,0,0,1,'55hweight-window data input on '             !/*ne4b       12*/
     & //ich(1:3)//' card and in wwinp file.')                                  !/*ne4b       13*/
      go to 9010                                                                !/*ne4b       14*/
!                                                                               !/*ne        128*/
! >>>>>  dxtran cell probabilities                                   dxc        !/*ne        129*/
  230 do 240 i=1,mipt                                                           !/*ne        130*/
  240 if(nqp(i).ne.0.and.ndx(i).eq.0)call erprnt(2,2,0,0,0,0,0,1,               !/*ne        131*/
     & '53hdxc probabilities without '//hnp(i)//' dxtran spheres.')             !/*nex        35*/
      go to 9010                                                                !/*ne        134*/
!                                                                               !/*ne        135*/
! >>>>>  cells where fission is treated like capture                nonu        !/*ne        136*/
  250 itfxs=1                                                                   !/*ne        137*/
      if(ntal.ne.0)call erprnt(2,2,0,0,0,0,0,0,                                 !/*ne        138*/
     & '59htallies with nonu card do not include any fission neutrons.')        !/*ne        139*/
      if(mcal.eq.2)call erprnt(2,1,0,0,0,0,0,0,                                 !/*ne4b       15*/
     & '54hnonu fails with adjoint.  set nu=0 in multigroup data.')             !/*ne4b       16*/
      return                                                                    !/*ne        140*/
!                                                                               !/*ne        141*/
! >>>>>  source distributions                             si,sp,sb,ds,sc        !/*ne        142*/
!        m1c=distribution index.                                                !/*ne        143*/
  260 if(nsr.eq.40.and.icn.eq.0)call erprnt(2,1,0,0,0,0,0,1,                    !/*ne        144*/
     & '35hsource distribution number missing.')                                !/*ne4a       44*/
      do 270 i=1,4                                                              !/*ne        146*/
  270 if(mod(jsd(i,1+icn/31)/2**mod(icn,31),2).ne.0)go to 280                   !/*ne        147*/
      call erprnt(2,1,1,icn,0,0,0,1,'14hthere is no si,i3,6h card.')            !/*ne        148*/
      return                                                                    !/*ne        149*/
  280 do 290 m1c=1,msd                                                          !/*ne        150*/
  290 if(icn.eq.ksd(lksd+1,m1c))go to 300                                       !/*ne        151*/
      msd=m1c                                                                   !/*ne        152*/
      ksd(lksd+1,m1c)=icn                                                       !/*ne        153*/
      go to 310                                                                 !/*ne        154*/
  300 if(ich.eq.'ds'.and.ksd(lksd+2,m1c)+ksd(lksd+4,m1c).ne.0.or.               !/*ne        155*/
     & index('si sp sb ',ich(1:3)).ne.0.and.ksd(lksd+7,m1c).ne.0)               !/*ne        156*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ne        157*/
     & '42hds card with same number as si, sp, or sb.')                         !/*ne        158*/
  310 if(ich.eq.'ds')ksd(lksd+7,m1c)=1                                          !/*ne        159*/
      if(ich.eq.'sc')ksd(lksd+15,m1c)=mssc                                      !/*ne        160*/
      if(ich.eq.'sb')ksd(lksd+18,m1c)=1                                         !/*ne        161*/
      return                                                                    !/*ne        162*/
!                                                                               !/*ne        163*/
! >>>>>  source definition                                          sdef        !/*ne        164*/
!        m1c=nwc of variable name.                                              !/*ne        165*/
  320 if(nsr.eq.6)call erprnt(2,1,0,0,0,0,0,0,                                  !/*ne        166*/
     & '36hsdef and ssr cards are incompatible.')                               !/*ne        167*/
      m1c=1                                                                     !/*ne        168*/
      return                                                                    !/*ne        169*/
!                                                                               !/*ne        170*/
! >>>>>  tally cards            pd,f,fx,fy,fz,pi,tir,tic,ft,fc,e,t,c,fq,        !/*nex        36*/
!                                 fm,de,df,em,tm,cm,cf,sf,fs,sd,fu,tf,dd        !/*nex        37*/
  330 if(ich.eq.'dd'.and.(icn.eq.1.or.icn.eq.2))return                          !/*nexc        3*/
      iy=mod(icn,10)                                                            !/*ne        174*/
#ifndef RADIOG                                                                  /*!nex        43*/
      if (ich.eq.'pi'.or.ich.eq.'tir'.or.ich.eq.'tic'.or.                       !/*nex        44*/
     & ich.eq.'c'.and.iy.eq.5) then                                             !/*nex        45*/
        call erprnt(2,2,0,0,0,0,0,0,                                            !/*nex        46*/
     & '59hThis version of MCNPX does not include radiography tallies.')        !/*nex        47*/
        return                                                                  !/*nex        48*/
      endif                                                                     !/*nex        49*/
#endif                                                                          /*!nex        50*/
      ital=0                                                                    !/*ne        175*/
      if(icn.eq.0)go to 350                                                     !/*ne        176*/
#ifdef MESHTAL                                                                  /*!nex        51*/
      do 340 ital=1,ntal+mshfm                                                  !/*nex        52*/
#else                                                                           /*!nex        53*/
      do 340 ital=1,ntal                                                        !/*ne        177*/
#endif                                                                          /*!nex        54*/
  340 if(jptal(ljpt+1,ital).eq.icn)go to 350                                    !/*ne        178*/
      call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne        179*/
     & '46hthere is no f card corresponding to this card.')                     !/*ne        180*/
      return                                                                    !/*ne        181*/
  350 continue                                                                  !/*nex        55*/
      if(ich.eq.'fq')go to 390                                                  !/*nex        56*/
      jf=index('   f   fx  fy  fz  pi  tir tic ',ich(1:4))/4                    !/*nex        57*/
      if(jf.eq.0)go to 370                                                      !/*ne        184*/
      if(ipnt(lipn+2,1,ital).ne.0)call erprnt                                   !/*ne        185*/
     & (2,1,1,icn,0,0,0,1,'30hmore than one f card for tally,i4')               !/*ne        186*/
      ipnt(lipn+2,1,ital)=jf                                                    !/*ne4a       45*/
      if(icx.eq.1)ipnt(lipn+2,1,ital)=-jf                                       !/*ne4a       46*/
      if((iy.eq.8.or.iy.eq.6).and.icx.eq.2)ipnt(lipn+2,1,ital)=9                !/*nex        58*/
      if(nsr.eq.6.and.idefv(10).ne.0.and.srv(1,18).ge.0..and.                   !/*ne4c        4*/
     & mod(n,10).eq.5)call erprnt(2,2,0,0,0,0,0,0,                              !/*ne4c        5*/
     & '53hdirect det tallies wrong with surf source bcw option.')              !/*ne4c        6*/
      m=0                                                                       !/*ne        189*/
      n=0                                                                       !/*ne        190*/
      do 360 i=1,mipt                                                           !/*ne        191*/
      ktp(lktp+i,ital)=nqp(i)                                                   !/*ne        192*/
!        make f8:p and f8:e same as f8:p,e.                                     !/*nex        59*/
      if(iy.eq.8.and.(i.eq.2.or.i.eq.3).and.kpt(i).ne.0.and.nqp(i).eq.0)        !/*nex        60*/
     & ktp(lktp+i,ital)=nqp(5-i)                                                !/*nex        61*/
!        abs(kf8)=0,1,2,3,4=none,+f8,*f8,f8,f8+vrt. kf8>0 if p,e, only.         !/*nexc        4*/
      if(iy.eq.8.and.(i.eq.1.or.i.gt.3).and.nqp(i).ne.0)kf8=-abs(kf8)           !/*nexc        5*/
      n=n+ktp(lktp+i,ital)                                                      !/*nex        64*/
  360 if(kpt(i).lt.nqp(i))m=1                                                   !/*ne        195*/
      jptal(ljpt+3,ital)=n                                                      !/*nex        65*/
      if(m.ne.0.and.icx.ne.2)call erprnt(2,2,0,0,0,0,0,0,                       !/*nex        66*/
     & '51hthis tally is not consistent with the problem mode.')                !/*ne        197*/
      if(iy.ne.7)go to 364                                                      !/*nex        67*/
      do 362 i=2,mipt                                                           !/*nex        68*/
  362 if(nqp(i).ne.0)call erprnt(2,1,0,0,0,0,0,1,                               !/*nex        69*/
     & '34htally 7 not available: '//hnp(i))                                    !/*nex        70*/
  364 if(iy.ne.5)go to 368                                                      !/*nex        71*/
      do 366 i=3,mipt                                                           !/*nex        72*/
  366 if(nqp(i).ne.0)call erprnt(2,1,1,iy,0,0,0,1,                              !/*nex        73*/
     & '5htally,i2,27h not available: '//hnp(i))                                !/*nex        74*/
  368 if(ly(iy+1).eq.0)                                                         !/*nex        75*/
     & call erprnt(2,1,0,0,0,0,0,1,'28hnot a legitimate tally type.')           !/*ne        203*/
      if(n.gt.1.and.iy.ne.6.and.iy.ne.8)call erprnt(2,1,0,0,0,0,0,1,            !/*ne        204*/
     & '39hmore than one particle-type designator.')                            !/*ne        205*/
      if(iy.ne.5.and.jf.gt.1)call erprnt(2,1,0,0,0,0,0,1,                       !/*ne        206*/
     & '34h'//ich(1:2)//' is valid for type 5 tally only.')                     !/*ne        207*/
      if(kpt(1).ne.0.and.iy.eq.8)call erprnt(1,2,0,0,0,0,0,0,                   !/*cor4-1     40*/
     & '54hf8 tally unreliable since neutron transport nonanalog.')             !/*cor4-1     41*/
      if(iy.le.4.and.nqp(2).ne.0.and.(ides.ne.0.or.                             !/*ne        210*/
     & iphot.ne.0.and.kpt(3).ne.0))call erprnt(2,2,0,0,0,0,0,0,                 !/*ne        211*/
     & '51hwithout bremsstrahlung, flux estimates will be low.')                !/*ne        212*/
  370 ipnt(lipn+1,krq(3,ica),ital)=ipl+1                                        !/*ne        213*/
#ifdef MESHTAL                                                                  /*!nex        76*/
      if(ital.gt.ntal)return                                                    !/*nex        77*/
#endif                                                                          /*!nex        78*/
      if(icn.eq.0.and.ld(krq(3,ica)).eq.0)call erprnt(2,1,0,0,0,0,0,            !/*ne        214*/
     & 1,'50hnot a legal data type for tally default treatment.')               !/*ne        215*/
      if (abs(ipnt(lipn+2,1,ital)).lt.5) then                                   !/*nex        79*/
         if(ich.eq.'c'.and.iy.gt.1)call erprnt(2,2,0,0,0,0,0,0,                 !/*nex        80*/
     &    '55hcosine bins normally applicable to type 1 tallies only.')         !/*nex        81*/
      endif                                                                     !/*nex        82*/
      if((ich.eq.'cf'.or.ich.eq.'sf').and.(iy.eq.5.or.iy.eq.8))                 !/*ne        218*/
     & call erprnt(2,1,0,0,0,0,0,1,                                             !/*ne        219*/
     & '47hflagging is not possible with f5 or f8 tallies.')                    !/*ne        220*/
      if(ich.eq.'fm'.and.iy.eq.8)call erprnt(2,1,0,0,0,0,0,1,                   !/*ne        221*/
     & '50hfm card is not possible with pulse height tallies.')                 !/*ne        222*/
      if (abs(ipnt(lipn+2,1,ital)).lt.5) then                                   !/*nex        83*/
         if(ich.eq.'fs'.and.iy.eq.5)call erprnt(2,2,0,0,0,0,0,0,                !/*nex        84*/
     &    '55hsegmentation normally inapplicable to detector tallies.')         !/*nex        85*/
      endif                                                                     !/*nex        86*/
      if(ich.eq.'fs'.and.iy.eq.8)call erprnt(2,1,0,0,0,0,0,1,                   !/*ne        225*/
     & '43hsegmentation does not work with f8 tallies.')                        !/*ne        226*/
      if(ich.eq.'dd'.and.icn.ne.0.and.iy.ne.5)call erprnt                       !/*ne        227*/
     & (2,1,0,0,0,0,0,0,'34hdd is valid for type 5 tally only.')                !/*ne        228*/
      if(ich.eq.'t'.and.iy.eq.8)call erprnt(2,1,0,0,0,0,0,0,                    !/*cor4-1     42*/
     & '38htime bins do not work with f8 tallies.')                             !/*cor4-1     43*/
      if(ich.ne.'pd')return                                                     !/*ne        229*/
      do 380 i=1,mxa                                                            !/*ne        230*/
  380 rtp(lrtp+ipl+i)=1.                                                        !/*ne        231*/
      return                                                                    !/*ne        232*/
  390 do 400 i=1,8                                                              !/*ne        233*/
  400 jfq(ljfq+i,ital)=i                                                        !/*ne        234*/
      return                                                                    !/*ne        235*/
!                                                                               !/*ne        236*/
! >>>>>  dxtran parameters                                           dxt        !/*ne        237*/
  410 if(nqp(3).ne.0)call erprnt(2,1,0,0,0,0,0,1,                               !/*ne        238*/
     & '38hdxtran is not available for electrons.')                             !/*ne        239*/
      do 420 i=1,mxj                                                            !/*ne        240*/
  420 if(ksu(lksu+i).ne.0)go to 430                                             !/*ne4a       48*/
      go to 9010                                                                !/*ne        242*/
  430 call erprnt(2,2,0,0,0,0,0,0,                                              !/*ne        243*/
     & '57hdxtran-reflector combo gives wrong answers.  see chap. 2.')          !/*ne        244*/
      go to 9010                                                                !/*ne        245*/
!                                                                               !/*ne        246*/
! >>>>>  material specifications                                    m,mt        !/*ne4b       17*/
  440 do 450 i=1,mxa                                                            !/*ne        248*/
  450 if(mat(lmat+i).eq.icn)go to 480                                           !/*ne        249*/
      do 460 i=1,nmfm,2                                                         !/*ne        250*/
  460 if(mfm(lmfm+i).eq.icn)go to 470                                           !/*ne        251*/
      call erprnt(2,2,1,icn,0,0,0,1,                                            !/*ne        252*/
     & '8hmaterial,i4,28h is not used in the problem.')                         !/*ne        253*/
      return                                                                    !/*ne        254*/
  470 call erprnt(2,2,1,icn,0,0,0,0,                                            !/*ne        255*/
     & '9hmaterial ,i4,42h is used only for a perturbation or tally.')          !/*ne4b       18*/
  480 if(ich.eq.'mt')return                                                     !/*ne4b       19*/
      nmat=nmat+1                                                               !/*ne4b       20*/
      nmt(lnmt+nmat)=icn                                                        !/*ne        258*/
      return                                                                    !/*ne        259*/
!                                                                               !/*ne        260*/
! >>>>>  cross-section directory information                          xs        !/*ne        261*/
  490 if(icn.eq.0)call erprnt(2,1,0,0,0,0,0,1,                                  !/*ne        262*/
     & '21hno number on xs card.')                                              !/*ne4a       49*/
      return                                                                    !/*ne        264*/
!                                                                               !/*ne        265*/
! >>>>>  run with all voids                                         void        !/*ne        266*/
  500 ioid=1                                                                    !/*ne        267*/
      return                                                                    !/*ne        268*/
!                                                                               !/*ne        276*/
! >>>>>  print control                                             print        !/*ne        277*/
  530 if(mnk.eq.1)return                                                        !/*ne        278*/
      mnk=0                                                                     !/*ne        279*/
      do 540 i=1,mink                                                           !/*ne        280*/
  540 ink(i)=0                                                                  !/*ne        281*/
      return                                                                    !/*ne        282*/
!                                                                               !/*ne        283*/
! >>>>>  kcode, acode specifications                        kcode, acode        !/*ne4c        8*/
  550 if(konrun.eq.0)kc8=1                                                      !/*ne4c        9*/
      return                                                                    !/*ne        287*/
!                                                                               !/*ne        288*/
! >>>>>  locations of kcode source points                           ksrc        !/*ne        289*/
  560 if(ksdef.ne.0)call erprnt(2,1,0,0,0,0,0,0,                                !/*ne        290*/
     & '37hksrc and sdef cards are incompatible.')                              !/*ne        291*/
      return                                                                    !/*ne        292*/
!                                                                               !/*ne        293*/
! >>>>>  energy bins for weight-window generator                    wwge        !/*ne        294*/
  570 if(iwwg.eq.0)call erprnt(2,1,0,0,0,0,0,0,                                 !/*ne4c       10*/
     & '31h'//ich//'card without any wwg card.')                                !/*ne        296*/
      return                                                                    !/*ne        297*/
!                                                                               !/*ne        298*/
! >>>>>  surface source write information                            ssw        !/*ne        299*/
!ccc    this restriction can be dropped when machinery to reorder               !/*ne        300*/
!ccc    the scratch file is added to subroutine wrwssa.                         !/*ne        301*/
  580 if(ntasks.gt.1.and.issw.ne.0)call erprnt(2,1,0,0,0,0,0,0,                 !/*ne4c       11*/
     & '55hsurface source write and multitasking are incompatible.')            !/*ne        303*/
      return                                                                    !/*ne        304*/
!                                                                               !/*ne        305*/
! >>>>>  surface source read information                             ssr        !/*ne        306*/
!        m1c=nwc of variable name.                                              !/*ne        307*/
  590 if(nsr.eq.71)call erprnt(2,1,0,0,0,0,0,0,                                 !/*ne        308*/
     & '37hssr and kcode cards are incompatible.')                              !/*ne        309*/
      if(nsr.eq.40)call erprnt(2,1,0,0,0,0,0,0,                                 !/*ne        310*/
     & '36hssr and sdef cards are incompatible.')                               !/*ne        311*/
      m1c=1                                                                     !/*ne        312*/
      return                                                                    !/*ne        313*/
!                                                                               !/*ne        314*/
! >>>>>  photon-production bias                                    pikmt        !/*ne        315*/
  600 if(kpt(1)*kpt(2).eq.0)call erprnt(2,1,0,0,0,0,0,1,                        !/*ne        316*/
     & '43hpikmt requires neutron-photon mode problem.')                        !/*ne        317*/
      return                                                                    !/*ne        318*/
!                                                                               !/*ne        319*/
! >>>>>  runtime tally monitor                                     mplot        !/*ne        320*/
#ifdef MCPLOT                                                                   /*!ne4a       53*/
  610 open(iu4,status='scratch')                                                !/*ne        322*/
      freq=5000.                                                                !/*ne        323*/
      if(nsr.eq.71)freq=5.                                                      !/*ne4a       54*/
      if(nsr.eq.71.and.nsrck.ge.20000)freq=1.                                   !/*ne4a       55*/
      krtm=1                                                                    !/*ne        324*/
#else                                                                           /*!nex        87*/
  610 call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne        326*/
     & '24hmcplot is not available.')                                           !/*ne        327*/
#endif                                                                          /*!cpp       110*/
      return                                                                    !/*ne        328*/
!                                                                               !/*ne4a       56*/
! >>>>>  ptrac specifications                                      ptrac        !/*ne4a       57*/
  620 if(ntasks.gt.1.or.ltasks.gt.1)go to 630                                   !/*ne4c       12*/
      iptr=1                                                                    !/*ne4a       59*/
      iptra(1)=0                                                                !/*ne4a       60*/
      iptra(2)=0                                                                !/*ne4a       61*/
      return                                                                    !/*ne4a       62*/
  630 call erprnt(2,1,0,0,0,0,0,1,                                              !/*ne4a       63*/
     & '56hparticle track output incompatible with multiprocessing.')           !/*ne4a       64*/
      return                                                                    !/*ne4a       65*/
!                                                                               !/*ne4b       22*/
! >>>>>  perturbation specificatons                                 pert        !/*ne4b       23*/
  640 npert=npert+1                                                             !/*ne4b       24*/
      m=0                                                                       !/*ne4b       25*/
      do 650 i=1,mipt                                                           !/*ne4b       26*/
  650 if(kpt(i).lt.nqp(i))m=1                                                   !/*ne4b       27*/
      if(m.ne.0)call erprnt(2,2,0,0,0,0,0,0,                                    !/*ne4b       28*/
     & '58hthis perturbation is not consistent with the problem mode.')         !/*ne4b       29*/
      return                                                                    !/*ne4b       30*/
!                                                                               !/*ne4c       13*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*ne4c       14*/
!        wwma(4-6) is temporarily the number of fine mesh entries               !/*ne4c       15*/
!          per direction (iints, jints, kints)                                  !/*ne4c       16*/
  660 if(wwg(1).eq.0.)iwwg=-1                                                   !/*nex        88*/
      if(iwwg.lt.0)ics=-1                                                       !/*nex        89*/
      if(iwwg.eq.2)go to 700                                                    !/*ne4c       18*/
      do 680 i=1,3                                                              !/*ne4c       19*/
      if(imesh(i*2+5).ne.0)go to 680                                            !/*ne4c       20*/
      k=0                                                                       !/*ne4c       21*/
      do 670 j=1,nint(wwma(i+6))                                                !/*ne4c2       2*/
      k=k+3                                                                     !/*ne4c       23*/
      ir=lwga+1+k+int(i/2)*(3*nint(wwma(7))+3)                                  !/*ne4c       24*/
     & +int(i/3)*(3*nint(wwma(8))+3)                                            !/*ne4c       25*/
      wgma(ir)=1.                                                               !/*ne4c2       3*/
  670 wwma(i+3)=wwma(i+3)+1                                                     !/*ne4c       27*/
  680 continue                                                                  !/*ne4c       28*/
      do 690 i=1,3                                                              !/*ne4c       29*/
  690 if(imesh(i*2+4).ne.0.and.wwma(i+6).ne.wwma(i+3))                          !/*ne4c       30*/
     & call erprnt(1,1,0,0,0,0,0,1,'49h'//hmsh(i*2+4)//' and '                  !/*ne4c       31*/
     & //hmsh(i*2+5)//': number of entries not equal.')                         !/*ne4c       32*/
  700 do 710 i=1,nmkey                                                          !/*ne4c       33*/
  710 imesh(i)=0                                                                !/*ne4c       34*/
      return                                                                    !/*ne4c       35*/
!                                                                               !/*ne4c2       4*/
! >>>>>  photonuclear isotope override                               mpn        !/*ne4c2       5*/
!        process mpn card if it is preceded by a corresponding m card.          !/*ne4c2       6*/
!        m1c = material index on mpn and preceding m card.                      !/*ne4c2       7*/
  720 if(ispn.eq.0)call erprnt(2,2,0,0,0,0,0,1,                                 !/*ne4c2       8*/
     & '50hmpn card ignored when photonuclear physics is off.')                 !/*ne4c2       9*/
      if(ispn.eq.0)return                                                       !/*ne4c2      10*/
!        find the corresponding material card and set pointers.                 !/*ne4c2      11*/
      do 730 i=1,nmat                                                           !/*ne4c2      12*/
  730 if(icn.eq.nmt(lnmt+i))m1c=i                                               !/*ne4c2      13*/
      if(m1c.eq.0)call erprnt(2,2,0,0,0,0,0,1,                                  !/*ne4c2      14*/
     & '52hmpn card not preceded by corresponding valid m card.')               !/*ne4c2      15*/
      return                                                                    !/*ne4c2      16*/
!                                                                               !/*ne        329*/
! >>>>>  HISTP file writing                                        histp        !/*nex        90*/
  740 continue                                                                  !/*nex        91*/
#ifdef HISTP                                                                    /*!nex        92*/
      ihistp=1                                                                  !/*nex        93*/
      if(ntasks.gt.1)call erprnt(2,1,0,0,0,0,0,1,                               !/*nexk        1*/
     & '46hhistp output file incompatible with threading.')                     !/*nexk        2*/
#else                                                                           /*!nex        94*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*nex        95*/
     & '52hThis version of MCNPX does not write the histp file.')               !/*nex        96*/
#endif                                                                          /*!nex        97*/
      return                                                                    !/*nex        98*/
!                                                                               !/*nex        99*/
! >>>>>  Mesh tally data card.                                     tmesh        !/*nex       100*/
  760 continue                                                                  !/*nex       101*/
#ifdef MESHTAL                                                                  /*!nex       102*/
      call rgdtal(2)                                                            !/*nex       103*/
#else                                                                           /*!nex       104*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*nex       105*/
     & '52hThis version of MCNPX does not include mesh tallies.')               !/*nex       106*/
#endif                                                                          /*!nex       107*/
      return                                                                    !/*nex       108*/
!                                                                               !/*nex       109*/
! >>>>>  No transport card.                                        notrn        !/*nex       110*/
  800 continue                                                                  !/*nex       111*/
#ifdef RADIOG                                                                   /*!nex       112*/
      notrn=1                                                                   !/*nex       113*/
#else                                                                           /*!nex       114*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*nex       115*/
     & '59hThis version of MCNPX does not include radiography tallies.')        !/*nex       116*/
#endif                                                                          /*!nex       117*/
      return                                                                    !/*nex       118*/
!                                                                               !/*nex       119*/
! >>>>>  Tally no print card.                                      talnp        !/*nex       120*/
  900 continue                                                                  !/*nex       121*/
#ifdef RADIOG                                                                   /*!nex       122*/
      notal=0                                                                   !/*nex       123*/
#else                                                                           /*!nex       124*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*nex       125*/
     & '59hThis version of MCNPX does not include radiography tallies.')        !/*nex       126*/
#endif                                                                          /*!nex       127*/
      return                                                                    !/*nex       128*/
!                                                                               !/*nex       129*/
! >>>>> Secondary Particle Biasing                                 spabi        !/*nex       130*/
  920 continue                                                                  !/*nex       131*/
#ifdef SPABI                                                                    /*!nex       132*/
      call set_spb_offsets                                                      !/*nex       133*/
#else                                                                           /*!nex       134*/
      call erprnt(2,2,0,0,0,0,0,0,                                              !/*nex       135*/
     & '59hThis version of MCNPX does not include 2nd particle baising')        !/*nex       136*/
#endif                                                                          /*!nex       137*/
      return                                                                    !/*nex       138*/
!                                                                               !/*nexc        6*/
!        variance reduction return.                                             !/*nexc        7*/
!        abs(kf8)=0,1,2,3,4=none,+f8,*f8,f8,f8+vrt. kf8>0 if p,e, only.         !/*nexc        8*/
 9010 if(abs(kf8).eq.3)kf8=sign(4,kf8)                                          !/*nexc        9*/
      return                                                                    !/*ne        333*/
!                                                                               !/*ne        334*/
! >>>>>  first spare card type                                        za        !/*ne        335*/
 9910 continue                                                                  !/*ne        336*/
      return                                                                    !/*ne        337*/
!                                                                               !/*ne        338*/
! >>>>>  second spare card type                                       zb        !/*ne        339*/
 9920 continue                                                                  !/*ne        340*/
      return                                                                    !/*ne        341*/
!                                                                               !/*ne        342*/
! >>>>>  third spare card type                                        zc        !/*ne        343*/
 9930 continue                                                                  !/*ne        344*/
      return                                                                    !/*ne        345*/
      end                                                                       !/*ne        346*/

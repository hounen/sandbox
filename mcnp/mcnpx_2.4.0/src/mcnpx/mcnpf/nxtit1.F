!#ident nxtit1.F from deck nx                                                   !/*nx          1*/
      subroutine nxtit1                                                         !/*nx          2*/
!        process the next input item.                                           !/*nx          3*/
      use allocate_memory                                                       !/*nxx         1*/
      use global_data                                                           !/*nxx         2*/
      use imcn_data                                                             !/*nxx         3*/
!                                                                               !/*nxx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*nxx         5*/
      implicit integer(kindi) (i-n)                                             !/*nxx         6*/
!                                                                               !/*nxx         7*/
      character hb*80,hh*10                                                     !/*nx4a        1*/
!                                                                               !/*nx          6*/
      nwc=nwc+1                                                                 !/*nx          7*/
      go to( 20, 30, 10, 10, 50, 10, 10, 10, 10, 10, 10, 10, 10, 55, 10,        !/*nx4b        1*/
     &       10, 10, 60, 70, 70, 60, 80, 90,111, 10, 10, 10, 10,135, 10,        !/*nx4a        2*/
     &       10, 10, 10,140, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,        !/*nx         10*/
     &       10, 10,170, 10, 10, 10, 10, 10,180, 10)ica                         !/*nx         11*/
      go to( 10, 10, 10,250, 10, 10, 10, 10,255, 10,258, 10,259,260, 10,        !/*nx4c2       1*/
     &      265, 10,270,310,345,350,400,410,430, 10, 10, 10,435,440,520,        !/*nx4c        1*/
     &      550,260, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,        !/*nxx         8*/
     &       10,600,9910,9920,9930)ica-55                                       !/*nxxc        1*/
   10 return                                                                    !/*nx         15*/
!                                                                               !/*nx         16*/
! >>>>>  cell descriptions                                                      !/*nx         17*/
!        m1c=count of lja items.                                                !/*nx         18*/
!        m3c=flag for cell parameters.                                          !/*nx         19*/
   20 if(hitm.eq.'like'.or.likef.ne.0)call likebt(1)                            !/*nx         20*/
      if(likef.ne.0)return                                                      !/*nx         21*/
      k=index('():#=',hitm(1:1))                                                !/*nx         22*/
      if(k.eq.0.and.kitm.eq.0)m3c=1                                             !/*nx         23*/
      if(m3c.ne.0)return                                                        !/*nx         24*/
      if(k.eq.4)ncomp=ncomp+1                                                   !/*nx         25*/
      if(nwc.ne.1.and.k.eq.0..and.kitm.ne.1)                                    !/*nx         26*/
     & ncl(lncl+mxa)=ncl(lncl+mxa)+100000                                       !/*nx         27*/
      if(nwc.ne.1.and.kitm.ne.1)m1c=m1c+1                                       !/*nx         28*/
      if(nwc.eq.1)write(iu3)iitm                                                !/*nx         29*/
      return                                                                    !/*nx         30*/
!                                                                               !/*nx         31*/
! >>>>>  surface descriptions                                                   !/*nx         32*/
!        m1c=surface type index.                                                !/*nx         33*/
!        m2c=1 if surface type symbol is the second item.                       !/*nx         34*/
   30 if(kitm.ne.0)go to 45                                                     !/*nx4a        5*/
      m2c=nwc-1                                                                 !/*nx         36*/
      do 40 m1c=1,39                                                            !/*nx4c        3*/
   40 if(ksf(m1c).eq.hitm)return                                                !/*nx         38*/
      if('hex'.eq.hitm)m1c=39                                                   !/*nx4c        4*/
      if('hex'.eq.hitm)return                                                   !/*nx4c        5*/
      ics=-1                                                                    !/*nx         39*/
      return                                                                    !/*nx         40*/
   45 if(nwc.eq.1.and.iitm.lt.0)mxtr=mxtr+1                                     !/*nx4a        6*/
      return                                                                    !/*nx4a        7*/
!                                                                               !/*nx         41*/
! >>>>>  problem mode                                               mode        !/*nx         42*/
   50 i=index(hip,hitm(1:1))                                                    !/*nx         43*/
      if(i.ne.0)kpt(i)=1                                                        !/*nx         44*/
      return                                                                    !/*nx         45*/
!                                                                               !/*nx4b        4*/
! >>>>>  weight-window game parameters                               wwp        !/*nx4b        5*/
   55 do 56 i=1,mipt                                                            !/*nx4b        6*/
   56 if(nqp(i).ne.0)wwp(i,nwc)=ritm                                            !/*nx4b        7*/
      return                                                                    !/*nx4b        8*/
!                                                                               !/*nx         46*/
! >>>>>  source distributions                                      si,ds        !/*nx         47*/
!        m1c=count of <,[,],:,(, and ).                                         !/*nxxa        1*/
!        m5c=flag to indicate reading cell path in <[] format                   !/*nxxa        2*/
!        m6c=nwc of last ( that might start <[] path format                     !/*nxxa        3*/
   60 if(hitm.eq.':'.or.hitm.eq.'<'.or.hitm.eq.'['.or.hitm.eq.']'.or.           !/*nxxa        4*/
     & hitm.eq.'('.or.hitm.eq.')')m1c=m1c+1                                     !/*nxxa        5*/
      if(m5c.eq.0.and.m6c.eq.0.and.kitm.ne.0)return                             !/*nxxa        6*/
      if(nwc.le.2.and.kitm.eq.0.and.hitm.eq.'(')go to 61                        !/*nxxa        7*/
      if(m5c.ne.0.or.m6c.ne.0)go to 62                                          !/*nxxa        8*/
      if(hitm.ne.'(')return                                                     !/*nxxa        9*/
   61 m6c=nwc                                                                   !/*nxxa       10*/
      if(nwc.le.2)m5c=1                                                         !/*nxxa       11*/
      return                                                                    !/*nxxa       12*/
   62 if(m5c.ne.0)go to 63                                                      !/*nxxa       13*/
      if(nwc-m6c.eq.1)go to 63                                                  !/*nxxa       14*/
      m5c=1                                                                     !/*nxxa       15*/
      if(kitm.eq.0.and.hitm.eq.'<')go to 63                                     !/*nxxa       16*/
      m5c=0                                                                     !/*nxxa       17*/
      m6c=0                                                                     !/*nxxa       18*/
      return                                                                    !/*nxxa       19*/
!        temporarily store the values of the cell path and check if in          !/*nxxa       20*/
!        <[] format and if so increment memory needed.                          !/*nxxa       21*/
   63 call nsourc(1)                                                            !/*nxxa       22*/
      if(kitm.ne.0)return                                                       !/*nxxa       23*/
      if(hitm.ne.')')return                                                     !/*nxxa       24*/
      m5c=0                                                                     !/*nxxa       25*/
      m6c=0                                                                     !/*nxxa       26*/
      return                                                                    !/*nx         50*/
!                                                                               !/*nx         51*/
! >>>>>  source distributions                                      sp,sb        !/*nx         52*/
   70 if(nwc.ne.1.or.ritm.ge.0.)return                                          !/*nx         53*/
      i=3                                                                       !/*nx         54*/
      if(iitm.ne.-21.and.iitm.ne.-31)i=4                                        !/*nx         55*/
      if(mod(jsd(i,1+icn/31)/2**mod(icn,31),2).eq.0)                            !/*nx         56*/
     & jsd(i,1+icn/31)=jsd(i,1+icn/31)+2**mod(icn,31)                           !/*nx         57*/
      return                                                                    !/*nx         58*/
!                                                                               !/*nx         59*/
! >>>>>  source comment                                               sc        !/*nx         60*/
   80 mssc=mssc+25                                                              !/*nx         61*/
      return                                                                    !/*nx         62*/
!                                                                               !/*nx         63*/
! >>>>>  source definition                                          sdef        !/*nx         64*/
!        m1c=index of current variable.                                         !/*nx         65*/
!        m2c=count of colons and parens.                                        !/*nx         66*/
   90 do 100 i=1,maxv                                                           !/*nx         67*/
  100 if(hitm(1:3).eq.hbln(i,1))go to 110                                       !/*nx4c        6*/
      if(hitm.eq.':'.or.hitm.eq.'('.or.hitm.eq.')')m2c=m2c+1                    !/*nx         69*/
      if(hitm.eq.':'.and.ivdis(1).lt.100000)ivdis(1)=ivdis(1)+100000            !/*nx4a        8*/
      if(nitm.le.1)return                                                       !/*nx         70*/
      if(hitm(1:1).eq.'d'.and.kdata(hitm(2:nitm)).eq.2.and.                     !/*nx         71*/
     & ivdis(m1c).eq.0.and.(m2c.eq.0.or.m1c.gt.1))                              !/*nx4a        9*/
     & read(hitm(2:4),'(bn,i3)')ivdis(m1c)                                      !/*nx4a       10*/
      return                                                                    !/*nx         73*/
  110 m1c=i                                                                     !/*nx         74*/
      idefv(i)=1                                                                !/*nx         75*/
      return                                                                    !/*nx         76*/
!                                                                               !/*nx         77*/
! >>>>>  tally                                                         f        !/*nx         78*/
!        m1c=parentheses counter.                                               !/*nx4a       11*/
!        m2c=itds() storage for multiple bin format of the current bin.         !/*nx4a       12*/
!        m3c=number of bins generated by the current level.                     !/*nx4a       13*/
!        m4c=number of bins generated by the current bin - negative             !/*nx4a       14*/
!            indicates colon input format within [].                            !/*nx4a       15*/
!        m5c=previous data entry within [] - negative indicates                 !/*nx4a       16*/
!            first level data and use of universe data.                         !/*nx4a       17*/
!        m6c=number of lattice elements within [] - negative indicates          !/*nx4a       18*/
!            previous entry was a colon.                                        !/*nx4a       19*/
!        m7c=total number of bins generated by all input bins.                  !/*nx4a       20*/
!        m8c=number of first level entries in the current bin.                  !/*nx4a       21*/
!        m9c=itds() storage for talloc data.                                    !/*nx4a       22*/
!        m10c=sum of all first level entries.                                   !/*nx4a       23*/
!        macrobody tally facet specified on f1 or f2 tally.                     !/*nx4c        7*/
  111 if(mod(icn,10).ne.1.and.mod(icn,10).ne.2)go to 105                        !/*nx4c        8*/
      if(kitm.ne.1)go to 105                                                    !/*nx4c        9*/
      iitm=ritm                                                                 !/*nx4c       10*/
      kitm=2                                                                    !/*nx4c       11*/
      ritm=iitm                                                                 !/*nx4c       12*/
!        all tallies.                                                           !/*nx4c       13*/
  105 go to(112,120,121)kitm+1                                                  !/*nx4c       14*/
  112 k=index('(  )  :  <  [  ]  u  t  ',hitm(1:3))                             !/*nx4a       25*/
      if(k.eq.22)lit=lit+2*m10c+3                                               !/*nx4a       26*/
      if(k.eq.0.or.k.gt.21)return                                               !/*nx4a       27*/
      go to(113,114,115,116,117,118,119)1+(k-1)/3                               !/*nx4a       28*/
  113 m1c=m1c+1                                                                 !/*nx4a       29*/
      if(m1c.gt.1)return                                                        !/*nx4a       30*/
      m4c=1                                                                     !/*nx4a       31*/
      m5c=-1                                                                    !/*nx4a       32*/
      return                                                                    !/*nx4a       33*/
  114 m1c=m1c-1                                                                 !/*nx4a       34*/
      if(m1c.ne.0)return                                                        !/*nx4a       35*/
      lit=lit+4                                                                 !/*nx4a       36*/
      m2c=m2c+5                                                                 !/*nx4a       37*/
      m4c=m4c*max(1,m3c)                                                        !/*nx4a       38*/
      if(m5c.lt.0)m4c=1                                                         !/*nx4a       39*/
      lit=lit+m2c*(m4c-1)                                                       !/*nx4a       40*/
      m7c=m7c+m4c                                                               !/*nx4a       41*/
      m9c=m9c+m8c*(m4c+4)                                                       !/*nx4a       42*/
      m10c=m10c+m8c                                                             !/*nx4a       43*/
      m2c=0                                                                     !/*nx4a       44*/
      m3c=0                                                                     !/*nx4a       45*/
      m5c=0                                                                     !/*nx4a       46*/
      m8c=0                                                                     !/*nx4a       47*/
      return                                                                    !/*nx4a       48*/
  115 if(m4c.lt.0)m6c=-abs(m6c)                                                 !/*nx4a       49*/
      if(m4c.gt.0)m6c=-1                                                        !/*nx4a       50*/
      if(m4c.gt.0)m4c=-m4c                                                      !/*nx4a       51*/
      return                                                                    !/*nx4a       52*/
  116 lit=lit+2                                                                 !/*nx4a       53*/
      m2c=m2c+3                                                                 !/*nx4a       54*/
      m4c=m4c*max(1,m3c)                                                        !/*nx4a       55*/
      m3c=0                                                                     !/*nx4a       56*/
      m5c=0                                                                     !/*nx4a       57*/
      return                                                                    !/*nx4a       58*/
  117 m6c=1                                                                     !/*nx4a       59*/
      return                                                                    !/*nx4a       60*/
  118 m6c=abs(m6c)                                                              !/*nx4a       61*/
      if(m4c.gt.0)m6c=max(1,(m6c-1)/3)                                          !/*nx4a       62*/
      lit=lit+3+4*(m6c-1)                                                       !/*nx4a       63*/
      m2c=m2c+3                                                                 !/*nx4a       64*/
      if(m1c.eq.2)m2c=m2c+4*(m6c-1)                                             !/*nx4a       65*/
      if(m1c.lt.2)m3c=m3c+m6c-1                                                 !/*nx4a       66*/
      m4c=abs(m4c)                                                              !/*nx4a       67*/
      m5c=0                                                                     !/*nx4a       68*/
      m6c=0                                                                     !/*nx4a       69*/
      return                                                                    !/*nx4a       70*/
  119 m5c=m5c-2                                                                 !/*nx4a       71*/
  120 return                                                                    !/*nx4a       72*/
  121 if(m6c.ne.0)go to 132                                                     !/*nx4a       73*/
      n=1                                                                       !/*nx4a       74*/
      if(m5c.gt.-2)go to 131                                                    !/*nx4a       75*/
!                                                                               !/*nx4a       76*/
!        expand universe format - read fill data from iu2 if present.           !/*nx4a       77*/
      rewind (iu2)                                                              !/*nx4a       78*/
      n=mxa                                                                     !/*nx4a       79*/
  123 read(iu2,'(a80)',end=130)hb                                               !/*nx4a       80*/
      if(index(hb(1:5),'fill').eq.0)go to 123                                   !/*nx4a       81*/
      n=0                                                                       !/*nx4a       82*/
      j=0                                                                       !/*nx4a       83*/
      go to 125                                                                 !/*nx4a       84*/
  124 read(iu2,'(a80)',end=130)hb                                               !/*nx4a       85*/
      if(hb(1:5).ne.' ')go to 130                                               !/*nx4a       86*/
  125 ir=6                                                                      !/*nx4a       87*/
  126 call nxtsym(hb,' :',ir,it,iu,1)                                           !/*nx4a       88*/
      ir=iu+1                                                                   !/*nx4a       89*/
      if(iu.eq.0)go to 124                                                      !/*nx4a       90*/
      if(j.gt.0)go to 129                                                       !/*nx4a       91*/
      if(hb(it:iu).eq.':')go to 128                                             !/*nx4a       92*/
      k=0                                                                       !/*nx4a       93*/
      if(kdata(hb(it:iu)).ne.2)go to 127                                        !/*nx4a       94*/
      hh=' '                                                                    !/*nx4a       95*/
      hh(10-iu+it:10)=hb(it:iu)                                                 !/*nx4a       96*/
      read(hh,'(i10)')k                                                         !/*nx4a       97*/
  127 if(k.eq.iitm)n=n+1                                                        !/*nx4a       98*/
      go to 126                                                                 !/*nx4a       99*/
  128 if(k.eq.iitm)n=n-1                                                        !/*nx4a      100*/
      j=1                                                                       !/*nx4a      101*/
  129 j=j+1                                                                     !/*nx4a      102*/
      if(j.eq.9)j=0                                                             !/*nx4a      103*/
      go to 126                                                                 !/*nx4a      104*/
  130 m5c=m5c+2                                                                 !/*nx4a      105*/
      if(m5c.lt.0)go to 131                                                     !/*nx4a      106*/
      lit=lit+3*n                                                               !/*nx4a      107*/
      if(m1c.lt.2)m2c=m2c+3                                                     !/*nx4a      108*/
      if(m1c.eq.2)m2c=m2c+3*n                                                   !/*nx4a      109*/
  131 lit=lit+n                                                                 !/*nx4a      110*/
      if(m1c.eq.2)m2c=m2c+n                                                     !/*nx4a      111*/
      if(m1c.lt.2)m3c=m3c+n                                                     !/*nx4a      112*/
      if(m5c.lt.0.or.m1c.eq.0)m8c=m8c+n                                         !/*nx4a      113*/
      if(m1c.ne.0)return                                                        !/*nx4a      114*/
      lit=lit+4*m3c                                                             !/*nx4a      115*/
      m7c=m7c+m3c                                                               !/*nx4a      116*/
      m9c=m9c+m8c*(m3c+4)                                                       !/*nx4a      117*/
      m10c=m10c+m8c                                                             !/*nx4a      118*/
      m2c=0                                                                     !/*nx4a      119*/
      m3c=0                                                                     !/*nx4a      120*/
      m8c=0                                                                     !/*nx4a      121*/
      return                                                                    !/*nx4a      122*/
  132 if(m4c.gt.0)m6c=m6c+1                                                     !/*nx4a      123*/
      if(m6c.lt.0)m6c=abs(m6c*(iitm-m5c+1))                                     !/*nx4a      124*/
      m5c=iitm                                                                  !/*nx4a      125*/
      return                                                                    !/*nx         80*/
!                                                                               !/*nx         81*/
! >>>>>  tally comment                                                fc        !/*nx         82*/
  135 lit=lit+25                                                                !/*nx4a      126*/
      return                                                                    !/*nx         84*/
!                                                                               !/*nx         85*/
! >>>>>  tally multiplier bins                                        fm        !/*nx         86*/
!        m1c=index of next material number.                                     !/*nx         87*/
!        m2c=flag for detector attenuator.                                      !/*nx         88*/
  140 i=index('()',hitm(1:1))                                                   !/*nx         89*/
      if(i.eq.1.and.m1c.eq.nwc+1)m1c=0                                          !/*nx         90*/
      if(m2c.ne.0)go to 150                                                     !/*nx         91*/
      if(i.eq.1.and.m1c.ne.0)m1c=-m1c                                           !/*nx         92*/
      if(i.eq.1.and.m1c.eq.0)m1c=nwc+2                                          !/*nx         93*/
      if(nwc.eq.1.and.i.ne.1)m1c=2                                              !/*nx         94*/
      if(i.eq.2.and.m1c.gt.0)m1c=0                                              !/*nx         95*/
      if(i.eq.2.and.m1c.lt.0)m1c=-m1c                                           !/*nx         96*/
      if(nwc.ne.m1c.or.i.ne.0)return                                            !/*nx         97*/
      if(iitm.gt.0)go to 160                                                    !/*nx         98*/
      m1c=m1c+1                                                                 !/*nx         99*/
      m2c=1                                                                     !/*nx        100*/
      return                                                                    !/*nx        101*/
  150 if(nwc.ne.abs(m1c))return                                                 !/*nx        102*/
      if(i.eq.1)m1c=-m1c-1                                                      !/*nx        103*/
      if(i.eq.2.and.m1c.gt.0)m2c=0                                              !/*nx        104*/
      if(i.eq.2.and.m1c.gt.0)m1c=0                                              !/*nx        105*/
      if(i.eq.2.and.m1c.lt.0)m1c=-m1c+1                                         !/*nx        106*/
      if(i.ne.0)return                                                          !/*nx        107*/
      m1c=sign(abs(m1c)+2,m1c)                                                  !/*nx        108*/
  160 write(iu3)iitm                                                            !/*nx        109*/
      write(iu3)icn                                                             !/*nx        110*/
      nmfm=nmfm+2                                                               !/*nx        111*/
      return                                                                    !/*nx        112*/
!                                                                               !/*nx        113*/
! >>>>>  material specifications                                       m        !/*nx        114*/
!        count zaids and atom fractions while ignoring material options.        !/*nx4c2       3*/
!        nwc is number of zaids plus atom fractions read so far.                !/*nx4c2       4*/
!        m1c > 0 indicates already processing material option m1c.              !/*nx4c2       5*/
  170 if(mod(nwc,2).eq.0)return                                                 !/*nx4c2       6*/
      if(m1c.eq.0)go to 172                                                     !/*nx4c2       7*/
      nwc=nwc-1                                                                 !/*nx4c2       8*/
      if(hitm.ne.'=')m1c=0                                                      !/*nx4c2       9*/
      return                                                                    !/*nx4c2      10*/
  172 do 174 i=1,mopts                                                          !/*nx4c2      11*/
      if (hitm.eq.hmopt(i)) then                                                !/*nxx        10*/
        m1c=i                                                                   !/*nxx        11*/
        nwc=nwc-1                                                               !/*nxx        12*/
        return                                                                  !/*nxx        13*/
      endif                                                                     !/*nxx        14*/
  174 continue                                                                  !/*nxx        15*/
!                                                                               !/*nxx        16*/
!     postpone reporting faulty ZAID entry.                                     !/*nxx        17*/
      if(kdata(hitm(1:max(1,nitm-1))).eq.0)return                               !/*nxx        18*/
!                                                                               !/*nxx        19*/
!        The following section is used to set up LAHET parameters.              !/*nxx        20*/
!        Subroutine MTINP0 needs to be passed ZA in variable k.                 !/*nxx        21*/
      j=index(hitm,'.')                                                         !/*nxx        22*/
      if(j.eq.0)j=nitm+1                                                        !/*nxx        23*/
      k=0                                                                       !/*nxx        24*/
      do 177 i=1,j-1                                                            !/*nxx        25*/
  177    k=10*k+index('123456789',hitm(i:i))                                    !/*nxx        26*/
      mx=0                                                                      !/*nxx        27*/
      call mtinp0 (k,nwc,mx)                                                    !/*nxx        28*/
      mxmel1=max(mxmel1,mx)                                                     !/*nxx        29*/
      return                                                                    !/*nx4c2      14*/
!                                                                               !/*nx        117*/
! >>>>>  physics parameters                                         phys        !/*nx        118*/
  180 do 185 mp=1,mipt                                                          !/*nxxa       27*/
      if(mp.ne.2.and.nqp(mp).ne.0.and.nwc.eq.1)emx(mp)=ritm                     !/*nxxa       28*/
      if(mp.gt.2.and.nqp(mp).ne.0.and.charge(mp).ne.zero.and.nwc.eq.5)          !/*nxxa       29*/
     & istrg(mp)=iitm                                                           !/*nxxa       30*/
  185 continue                                                                  !/*nxxa       31*/
      if(nwc.eq.2.and.iitm.ne.0.and.(nqp(2).ne.0.or.nqp(3).ne.0))ides=1         !/*nxx        36*/
!       turn on photonuclear physics( + natural coll / - biased coll )          !/*nxx        37*/
      if(nqp(2).ne.0.and.iitm.ne.0.and.nwc.eq.4)ispn=iitm                       !/*nxx        38*/
      return                                                                    !/*nx        122*/
!                                                                               !/*nx        146*/
! >>>>>  cutoffs                                                     cut        !/*nx        147*/
  250 do 252 mp=1,mipt                                                          !/*nxx        39*/
  252 if(nqp(mp).ne.0.and.nwc.eq.2)ecf(mp)=max(ritm,ecut_min(mp))               !/*nxx        40*/
      return                                                                    !/*nx        149*/
!                                                                               !/*nx4a      145*/
! >>>>>  print and dump controls                                   prdmp        !/*nx4a      146*/
  255 if(nwc.eq.3)mct=ritm                                                      !/*nx4a      147*/
      return                                                                    !/*nx4a      148*/
!                                                                               !/*nx        150*/
! >>>>>  debugging controls                                         dbcn        !/*nx4a      149*/
!        dbcn(15) for vov in all tally bins; dbcn(16) scales score grid.        !/*nx4a      150*/
  258 if(nwc.eq.15.and.ritm.ne.0)nmxf=5                                         !/*nx4a      151*/
      dbcn(nwc)=ritm                                                            !/*nx4a      152*/
      return                                                                    !/*nx4a      153*/
!                                                                               !/*nx4b       45*/
! >>>>>  print control                                             print        !/*nx4b       46*/
  259 if(nwc.eq.1.and.iitm.gt.0)ink(128)=0                                      !/*nx4b       47*/
      if(iitm.eq.128)ink(128)=1                                                 !/*nx4b       48*/
      if(iitm.eq.-128)ink(128)=0                                                !/*nx4b       49*/
      return                                                                    !/*nx4b       50*/
!                                                                               !/*nx4a      154*/
! >>>>>  kcode, acode specifications                        kcode, acode        !/*nx4c       15*/
  260 if(nwc.eq.1)nsrck=ritm                                                    !/*nx        152*/
      if(nwc.eq.3)mrkp=max(50*iitm,mrkp)                                        !/*nx4c       16*/
      if(nwc.eq.4)mrkp=max(5*iitm,mrkp)                                         !/*nx4c       17*/
      if(nwc.eq.5)msrk=ritm                                                     !/*nx        153*/
      if((nwc.eq.7.and.ich.eq.'kcode').or.                                      !/*nx4c       18*/
     & (nwc.eq.10.and.ich.eq.'acode'))mrkp=max(iitm,mrkp)                       !/*nx4c       19*/
      return                                                                    !/*nx        154*/
!                                                                               !/*nx4c       20*/
! >>>>>  weight window generator parameters                          wwg        !/*nx4c       21*/
  265 wwg(nwc)=ritm                                                             !/*nx4c       22*/
      return                                                                    !/*nx4c       23*/
!                                                                               !/*nx        155*/
! >>>>>  surface source write information                            ssw        !/*nx        156*/
!        m1c=index of current variable                                          !/*nx        157*/
!        m2c=0 for surface, -3 for (, -1 for cell, -2 for )                     !/*nx        158*/
  270 if(m1c.ne.0)go to 280                                                     !/*nx        159*/
      if(kitm.eq.0.and.hitm.ne.'('.and.hitm.ne.')')go to 280                    !/*nx        160*/
      mjss=max(1,mjss)                                                          !/*nx        161*/
      if(m2c.lt.-1)m2c=m2c+2                                                    !/*nx        162*/
      if(kitm.eq.0)m2c=index('()',hitm(1:1))-4                                  !/*nx        163*/
      mjss=mjss+max(0,2+m2c)                                                    !/*nx        164*/
      njss=njss+max(0,1+m2c)                                                    !/*nx        165*/
      return                                                                    !/*nx        166*/
  280 if(kitm.ne.0)go to 300                                                    !/*nx        167*/
      if(hitm.eq.'=')return                                                     !/*nx        168*/
      do 290 m1c=1,maxw                                                         !/*nx        169*/
  290 if(hitm(1:3).eq.hblw(m1c))return                                          !/*nx4c       24*/
      return                                                                    !/*nx        171*/
  300 if(m1c.eq.3)nilw=nilw+1                                                   !/*nx        172*/
      return                                                                    !/*nx        173*/
!                                                                               !/*nx        174*/
! >>>>>  surface source read information                             ssr        !/*nx        175*/
!        m1c=index of current variable.                                         !/*nx        176*/
  310 if(kitm.ne.0)go to 340                                                    !/*nx        177*/
      do 320 i=1,maxv                                                           !/*nx        178*/
  320 if(hitm(1:3).eq.hbln(i,2))go to 330                                       !/*nx4c       25*/
      if(nitm.le.1)return                                                       !/*nx        180*/
      if(hitm(1:1).eq.'d'.and.kdata(hitm(2:nitm)).eq.2)                         !/*nx        181*/
     & read(hitm(2:4),'(bn,i3)')ivdis(m1c)                                      !/*nx        182*/
      return                                                                    !/*nx        183*/
  330 m1c=i                                                                     !/*nx        184*/
      idefv(i)=1                                                                !/*nx        185*/
      return                                                                    !/*nx        186*/
  340 if(m1c.eq.1)nilr(1)=nilr(1)+1                                             !/*nx4c       26*/
      if(m1c.eq.2)njsr(1)=njsr(1)+1                                             !/*nx4c       27*/
      if(m1c.eq.3)njsx(1)=njsx(1)+1                                             !/*nx4c       28*/
      return                                                                    !/*nx        190*/
!                                                                               !/*nx4b       51*/
! >>>>>  universe designators                                          u        !/*nx4b       52*/
  345 jun(ljun+nwc)=iitm                                                        !/*nx4b       53*/
      return                                                                    !/*nx4b       54*/
!                                                                               !/*nx        191*/
! >>>>>  cell transformations                                       trcl        !/*nx        192*/
!        m1c=cell index                                                         !/*nx        193*/
!        m2c=nwc of last cell seen, negative if left parens                     !/*nx        194*/
  350 go to(360,370,390)1+index('()',hitm(1:1))                                 !/*nx        195*/
  360 if(m2c.lt.0)return                                                        !/*nx        196*/
      m1c=m1c+nwc-m2c                                                           !/*nx        197*/
      m2c=nwc                                                                   !/*nx        198*/
      if(iitm.eq.0)return                                                       !/*nx        199*/
      go to 380                                                                 !/*nx        200*/
  370 m1c=m1c+nwc-m2c                                                           !/*nx        201*/
      m2c=-nwc                                                                  !/*nx        202*/
      mxtr=mxtr+1                                                               !/*nx        203*/
  380 if(m1c.gt.mxa)ics=-1                                                      !/*cor4-1     37*/
      if(m1c.gt.mxa)return                                                      !/*cor4-1     38*/
!        macrobodies: tighter checks would waste less memory.                   !/*nx4c       29*/
!          9=max # surfaces from macrobody per cell surface entry.              !/*nx4c       30*/
!          32=8*4 = maximum # surface coef from any macrobody (for rhp).        !/*nx4c       31*/
      mxj=mxj+9*ncl(lncl+m1c)/100000                                            !/*nx4c       32*/
      nsc=nsc+32*ncl(lncl+m1c)/100000                                           !/*nx4c       33*/
      return                                                                    !/*nx        206*/
  390 if(nwc.eq.-m2c+2)mxtr=mxtr-1                                              !/*nx        207*/
      m2c=nwc                                                                   !/*nx        208*/
      return                                                                    !/*nx        209*/
!                                                                               !/*nx        210*/
! >>>>>  lattice type                                                lat        !/*nx        211*/
  400 if(iitm.ne.0)nlat=nlat+1                                                  !/*nx        212*/
      return                                                                    !/*nx        213*/
!                                                                               !/*nx        214*/
! >>>>>  cell-filling universes, with transformations               fill        !/*nx        215*/
!        m1c=cell index                                                         !/*nx4b       55*/
!        m2c(positive)=nwc of last item seen, not in parens                     !/*nx4b       56*/
!        m2c(negative)=-nwc of left parens, when in parens                      !/*nx4b       57*/
!        m3c=0 when not in lattice fill                                         !/*nx4b       58*/
!        m3c(negative)=-nwc of i2 in i1:i2                                      !/*nx4b       59*/
!        m3c(positive)=n of laf(llaf+m,n)                                       !/*nx4b       60*/
!        m4c=maximum value of n                                                 !/*nx4b       61*/
  410 go to(412,420,422,424)1+index(':()',hitm(1:1))                            !/*nx4b       62*/
  412 if(m2c.lt.0)return                                                        !/*nx4b       63*/
      if(m3c.eq.0)go to 418                                                     !/*nx4b       64*/
      if(m3c.gt.0)go to 414                                                     !/*nx4b       65*/
      i=nwc+m3c                                                                 !/*nx4b       66*/
      if(mod(i,3).ne.0)laf(llaf+mlaf+2+i/3,1)=iitm                              !/*nx4b       67*/
      if(mod(i,3).eq.0)laf(llaf+mlaf+1+i/3,2)=iitm-                             !/*nx4b       68*/
     & laf(llaf+mlaf+1+i/3,1)+1                                                 !/*nx4b       69*/
      if(i.ne.6)return                                                          !/*nx4b       70*/
      m3c=2                                                                     !/*nx4b       71*/
      m2c=nwc                                                                   !/*nx4b       72*/
      m4c=2+laf(llaf+mlaf+1,2)*laf(llaf+mlaf+2,2)*                              !/*nx4b       73*/
     & laf(llaf+mlaf+3,2)                                                       !/*nx4b       74*/
      nmzu=max(nmzu,m4c)                                                        !/*nx4b       75*/
      if(mlaf/3+m4c.gt.size(laf,2))then                                         !/*nxx        41*/
      if(dbcn(6).eq.0.)then                                                     !/*nxx        42*/
        laf => chg_mem (laf,3_kindi,mlaf/3+m4c+100,is)                          !/*nxx        43*/
      else                                                                      !/*nxx        44*/
        laf => chg_mem (laf,mlaf+3*m4c+300,mlaf/3+m4c+100,is)                   !/*nxx        45*/
      endif                                                                     !/*nxx        46*/
        if(is.ne.0)call expire(0,'nxtit1','chg_mem error')                      !/*nxx        47*/
      endif                                                                     !/*nxx        48*/
      return                                                                    !/*nx4b       78*/
  414 m3c=m3c+nwc-m2c                                                           !/*nx4b       79*/
      laf(llaf+mlaf+1,m3c)=iitm                                                 !/*nx4b       80*/
      m2c=nwc                                                                   !/*nx4b       81*/
      if(m3c.lt.m4c)return                                                      !/*nx4b       82*/
      mlaf=mlaf+m4c*3                                                           !/*nx4b       83*/
      m3c=0                                                                     !/*nx4b       84*/
      return                                                                    !/*nx4b       85*/
  418 m1c=m1c+nwc-m2c                                                           !/*nx4b       86*/
      mfl(lmfl+1,m1c)=iitm                                                      !/*nx4b       87*/
      m2c=nwc                                                                   !/*nx4b       88*/
      return                                                                    !/*nx4b       89*/
  420 if(m3c.ne.0)return                                                        !/*nx4b       90*/
      m3c=-nwc-1                                                                !/*nx4b       91*/
      laf(llaf+mlaf+1,1)=mfl(lmfl+1,m1c)                                        !/*nx4b       92*/
      mfl(lmfl+1,m1c)=-llaf-mlaf                                                !/*nx4b       93*/
      return                                                                    !/*nx4b       94*/
  422 mxtr=mxtr+1                                                               !/*nx4b       95*/
      m2c=-nwc                                                                  !/*nx4b       96*/
      return                                                                    !/*nx4b       97*/
  424 m2c=nwc                                                                   !/*nx4b       98*/
      return                                                                    !/*nx        231*/
!                                                                               !/*nx        232*/
! >>>>>  multigroup options                                        mgopt        !/*nx        233*/
  430 if(nwc.eq.1.and.hitm.eq.'f')mcal=1                                        !/*nx        234*/
      if(nwc.eq.1.and.hitm.eq.'a')mcal=2                                        !/*nx        235*/
      if(nwc.eq.2)igm=abs(iitm)                                                 !/*nx        236*/
      if(nwc.eq.2.and.iitm.lt.0)img=1                                           !/*nx        237*/
      if(nwc.eq.3)iplt=iitm                                                     !/*nx        238*/
      if(nwc.eq.4)isb=iitm                                                      !/*nx        239*/
      if(nwc.eq.5)icw=iitm                                                      !/*nx        240*/
      if(nwc.eq.6)fnw=ritm                                                      !/*nx        241*/
      if(nwc.eq.7)rim=ritm                                                      !/*nx        242*/
      return                                                                    !/*nx        243*/
!                                                                               !/*nx4a      157*/
! >>>>>  runtime tally monitor                                     mplot        !/*nx4a      158*/
  435 l=index(klin,'$')                                                         !/*nx4a      159*/
      if(l.eq.0)l=81                                                            !/*nx4a      160*/
      m=index(klin,'&')                                                         !/*nx4a      161*/
      if(m.eq.0)m=81                                                            !/*nx4a      162*/
      if(m.lt.l)irc=-1                                                          !/*nx4a      163*/
      return                                                                    !/*nx4a      164*/
!                                                                               !/*nx4a      165*/
! >>>>>  ptrac specificatons                                       ptrac        !/*nx4a      166*/
!        m1c=index of current ptrac keyword.                                    !/*nx4a      167*/
!        m2c=storage needed for ptr().                                          !/*nx4a      168*/
!        m3c=number of entries following the current keyword.                   !/*nx4a      169*/
!        m4c=track storage portion of pts().                                    !/*nx4a      170*/
!        m5c=flag for particle data storage portion of pts().                   !/*nx4a      171*/
  440 go to(450,510,510)kitm+1                                                  !/*nx4a      172*/
  450 if(index(',=',hitm(1:1)).ne.0)return                                      !/*nx4a      173*/
      if(m1c.eq.0)go to 480                                                     !/*nx4a      174*/
      i=0                                                                       !/*nx4a      175*/
      if(hptr(m1c).eq.'event')i=index('src bnk sur col ter ',hitm(1:4))         !/*nx4a      176*/
      if(hptr(m1c).eq.'file')i=index('asc bin ',hitm(1:4))                      !/*nx4a      177*/
      if(hptr(m1c).eq.'type')i=index('n   p   e   ',hitm(1:4))                  !/*nx4a      178*/
      if(hptr(m1c).eq.'write')i=index('pos all ',hitm(1:4))                     !/*nx4a      179*/
      if(hptr(m1c).ne.'filter')go to 470                                        !/*nx4a      180*/
      do 460 i=1,24                                                             !/*nx4a      181*/
  460 if(hitm(1:7).eq.hpbl(i))go to 470                                         !/*nx4a      182*/
      go to 480                                                                 !/*nx4a      183*/
  470 if(i.eq.0)go to 480                                                       !/*nx4a      184*/
      if(m3c.eq.0)m2c=m2c+1                                                     !/*nx4a      185*/
      j=1                                                                       !/*nx4a      186*/
      if(hptr(m1c).eq.'filter')j=3-mod(m3c,3)                                   !/*nx4a      187*/
      m2c=m2c+j                                                                 !/*nx4a      188*/
      m3c=m3c+j                                                                 !/*nx4a      189*/
      if(hptr(m1c).eq.'write')m5c=(i+3)/4                                       !/*nx4a      190*/
      return                                                                    !/*nx4a      191*/
  480 do 490 m1c=1,nptr                                                         !/*nx4a      192*/
  490 if(hitm(1:7).eq.hptr(m1c))go to 500                                       !/*nx4a      193*/
      m1c=0                                                                     !/*nx4a      194*/
      return                                                                    !/*nx4a      195*/
  500 m3c=0                                                                     !/*nx4a      196*/
      return                                                                    !/*nx4a      197*/
  510 if(m1c.eq.0)return                                                        !/*nx4a      198*/
      if(m3c.eq.0)m2c=m2c+1                                                     !/*nx4a      199*/
      m2c=m2c+1                                                                 !/*nx4a      200*/
      m3c=m3c+1                                                                 !/*nx4a      201*/
      if(hptr(m1c).eq.'buffer'.and.iitm.gt.0)m4c=iitm                           !/*nx4a      202*/
      return                                                                    !/*nx4a      203*/
!                                                                               !/*nx4b       99*/
! >>>>>  perturbation specificatons                                 pert        !/*nx4b      100*/
!        m1c=index of current pert keyword.                                     !/*nx4b      101*/
  520 if(kitm.ne.0)go to 540                                                    !/*nx4b      102*/
      if(index(',=',hitm(1:1)).ne.0)return                                      !/*nx4b      103*/
      do 530 m1c=1,npkey                                                        !/*nx4b      104*/
  530 if(hitm(1:7).eq.hptb(m1c))return                                          !/*nx4b      105*/
      m1c=0                                                                     !/*nx4b      106*/
      return                                                                    !/*nx4b      107*/
  540 if(m1c.eq.0)return                                                        !/*nx4b      108*/
      ipert=ipert+1                                                             !/*nx4b      109*/
      if(hptb(m1c).ne.'mat')return                                              !/*nx4b      110*/
      write(iu3)iitm                                                            !/*nx4b      111*/
      write(iu3)-icn                                                            !/*nx4b      112*/
      nmfm=nmfm+2                                                               !/*nx4b      113*/
      return                                                                    !/*nx4b      114*/
!                                                                               !/*nx4c       34*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*nx4c       35*/
!        m1c=index of current mesh keyword.                                     !/*nx4c       36*/
!        wwma(4-6) is temporarily the number of fine mesh entries               !/*nx4c       37*/
!          per direction (iints, jints, kints)                                  !/*nx4c       38*/
  550 if(kitm.ne.0)go to 580                                                    !/*nx4c       39*/
      if(index(',=',hitm(1:1)).ne.0)return                                      !/*nx4c       40*/
      if(m1c.ne.0)then                                                          !/*nxx        49*/
         if(hmsh(m1c).eq.'geom'.and.imesh(m1c).eq.0)imesh(m1c)=1                !/*nxx        50*/
      endif                                                                     !/*nxx        51*/
      do 560 m1c=1,nmkey                                                        !/*nx4c       42*/
  560 if(hitm(1:7).eq.hmsh(m1c))return                                          !/*nx4c       43*/
      m1c=0                                                                     !/*nx4c       44*/
      return                                                                    !/*nx4c       45*/
  580 if(m1c.eq.0)return                                                        !/*nx4c       46*/
      imesh(m1c)=imesh(m1c)+1                                                   !/*nx4c       47*/
      if(hmsh(m1c).eq.'iints')then                                              !/*nx4c       48*/
         wwma(1)=wwma(1)+ritm                                                   !/*nx4c       49*/
         wwma(4)=wwma(4)+1                                                      !/*nx4c       50*/
      else if(hmsh(m1c).eq.'imesh')then                                         !/*nx4c       51*/
         wwma(7)=wwma(7)+1                                                      !/*nx4c       52*/
      else if(hmsh(m1c).eq.'jints')then                                         !/*nx4c       53*/
         wwma(2)=wwma(2)+ritm                                                   !/*nx4c       54*/
         wwma(5)=wwma(5)+1                                                      !/*nx4c       55*/
      else if(hmsh(m1c).eq.'jmesh')then                                         !/*nx4c       56*/
         wwma(8)=wwma(8)+1                                                      !/*nx4c       57*/
      else if(hmsh(m1c).eq.'kints')then                                         !/*nx4c       58*/
         wwma(3)=wwma(3)+ritm                                                   !/*nx4c       59*/
         wwma(6)=wwma(6)+1                                                      !/*nx4c       60*/
      else if(hmsh(m1c).eq.'kmesh')then                                         !/*nx4c       61*/
         wwma(9)=wwma(9)+1                                                      !/*nx4c       62*/
      end if                                                                    !/*nx4c       63*/
      return                                                                    !/*nx4c       64*/
!                                                                               !/*nxxc        5*/
  600 return                                                                    !/*nxxc       11*/
!                                                                               !/*nx        244*/
! >>>>>  first spare card type                                        za        !/*nx        245*/
 9910 continue                                                                  !/*nx        246*/
      return                                                                    !/*nx        247*/
!                                                                               !/*nx        248*/
! >>>>>  second spare card type                                       zb        !/*nx        249*/
 9920 continue                                                                  !/*nx        250*/
      return                                                                    !/*nx        251*/
!                                                                               !/*nx        252*/
! >>>>>  third spare card type                                        zc        !/*nx        253*/
 9930 continue                                                                  !/*nx        254*/
      return                                                                    !/*nx        255*/
!                                                                               !/*nx        256*/
      end                                                                       !/*nx        257*/
